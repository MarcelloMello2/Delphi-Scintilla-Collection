unit OutputConsole;

interface

uses
  Windows, SysUtils, Classes, ShellAPI;

const
  CR = #13;
  LF = #10;

type
  TStartEvent = procedure(Sender: TObject; const FileName,
    Params: String) of Object;
  TProgressEvent = procedure(Sender: TObject; const FileName, Params,
    Output: String) of Object;
  TFinishEvent = procedure(Sender: TObject; const FileName, Params: String;
    ConsoleOut: TStrings; ExitCode: Integer) of Object;

  TOutputConsole = class;
  
  TReaderConsole = class(TThread)
  private
    { Private declarations }
    hOutputRead: THandle;
    FOutput: String;
    FOutputConsole: TOutputConsole;
  public
    constructor Create(CommandConsole: TOutputConsole);
    procedure Execute; override;
    property OutputRead: THandle read hOutputRead write hOutputRead;
    property Output: String read FOutput;
  end;

  TOutputConsole = class(TComponent)
  private
    { Private declarations }
    FExitCode: Integer;
    FFileName: String;
    FParams: String;
    FConvertOemToAnsi: Boolean;
    FDirectory: String;
    FeCode: Integer;
    FOutput: String;
    FOutLines: TStrings;
    FOnStart: TStartEvent;
    FOnProgress: TProgressEvent;
    FOnFinish: TFinishEvent;
    hOutputRead, hInputWrite: THandle;
    FProcessInfo: TProcessInformation;
    FExecuting: Boolean;
    Reader: TReaderConsole;
    procedure DoStart;
    procedure DoProgress;
    procedure DoFinish;
    //***********
    procedure Execute;
    function Launch(hInputRead, hOutputWrite,
      hErrorWrite : THandle): Boolean;
    procedure CloseApplication(Sender: Tobject);
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
    property Executing: Boolean read FExecuting;
    property ConsoleOut: TStrings read FOutLines;
  published
    { Published declarations }
    property OnStart: TStartEvent read FOnStart write FOnStart;
    property OnProgress: TProgressEvent read FOnProgress write FOnProgress;
    property OnFinish: TFinishEvent read FOnFinish write FOnFinish;
    property FileName: String read FFileName write FFileName;
    property Params: String read FParams write FParams;
    property Directory: String read FDirectory write FDirectory;
    property ConvertOemToAnsi: Boolean read FConvertOemToAnsi write FConvertOemToAnsi;
    property ExitCode: Integer read FExitCode write FExitCode default 1;
  end;

procedure Register;

implementation


{TReaderConsole}
constructor TReaderConsole.Create(CommandConsole: TOutputConsole);
begin
  inherited Create(True);
  FOutputConsole := CommandConsole;
end;

procedure TReaderConsole.Execute;
var
  nRead: DWORD;
  bSucess: Boolean;
  Buffer: array[0..2048] of AnsiChar;
begin
  repeat
    bSucess := ReadFile(OutputRead, Buffer, SizeOf(Buffer) - 1, nRead, nil);
    if not bSucess or (nRead = 0) then
      Break;
      
    Buffer[nRead] := #0;
    if FOutputConsole.FConvertOemToAnsi then
      OemToAnsi(Buffer, Buffer);
    FOutput := FOutput + string(StrPas(PAnsiChar(@Buffer)));
    FOutputConsole.FOutput := string(StrPas(Buffer));
    FOutputConsole.FOutLines.Text := FOutput;
    Synchronize(FOutputConsole.DoProgress);
  until False;
end;

{TOutputConsole}
constructor TOutputConsole.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOutLines := TStringList.Create;
  FExitCode := 1;
end;

destructor TOutputConsole.Destroy;
begin
  FOutLines.Free;
  inherited Destroy;
end;

procedure TOutputConsole.Start;
begin
  if Executing then
    Stop;
  FOutLines.Clear;
  Execute;
end;

procedure TOutputConsole.Execute;
var
  SecAttrib: TSecurityAttributes;
  hOutputReadTmp, hInputWriteTmp: THandle;
  hInputRead, hOutputWrite, hErrorWrite: THandle;
begin
  SecAttrib.nLength := SizeOf(TSecurityAttributes);
  SecAttrib.lpSecurityDescriptor := nil;
  SecAttrib.bInheritHandle := True;

  // Create a pipe for the child process's STDOUT.
  if not CreatePipe(hOutputReadTmp, hOutputWrite, @SecAttrib, 0) then
  begin
    DoStart;
    DoFinish;
    Exit;
  end;

  if not DuplicateHandle(GetCurrentProcess(), hOutputReadTmp,
    GetCurrentProcess(), @hOutputRead,  0, False, DUPLICATE_SAME_ACCESS) then
  begin
    DoStart;
    DoFinish;
    Exit;
  end;

  if not DuplicateHandle(GetCurrentProcess(), hOutputWrite,
    GetCurrentProcess(), @hErrorWrite, 0, True, DUPLICATE_SAME_ACCESS) then
  begin
    DoStart;
    DoFinish;
    Exit;
  end;

  // Create a pipe for the child process's STDIN.
  if not CreatePipe(hInputRead, hInputWriteTmp, @SecAttrib, 0) then
  begin
    DoStart;
    DoFinish;
    Exit;
  end;

  if not DuplicateHandle(GetCurrentProcess(), hInputWriteTmp,
    GetCurrentProcess(), @hInputWrite, 0, False, DUPLICATE_SAME_ACCESS) then
  begin
    DoStart;
    DoFinish;
    Exit;
  end;

  if not CloseHandle(hOutputReadTmp) or
     not CloseHandle(hInputWriteTmp) then
  begin
    DoStart;
    DoFinish;
    Exit;
  end;

  FExecuting := Launch(hInputRead, hOutputWrite, hErrorWrite);
  DoStart;
  if not FExecuting then
  begin
    FeCode := GetLastError;
    CloseHandle(hOutputWrite);
    CloseHandle(hInputRead);
    CloseHandle(hErrorWrite);
    DoFinish;
    Exit;
  end;
  
  if not CloseHandle(hInputRead) or
     not CloseHandle(hOutputWrite) or
     not CloseHandle(hErrorWrite) then
  begin
    TerminateProcess(FProcessInfo.hProcess, 1);
    CloseHandle(FProcessInfo.hProcess);
    DoFinish;
    Exit;
  end;

  Reader := TReaderConsole.Create(Self);
  Reader.OutputRead := hOutputRead;
  Reader.OnTerminate := CloseApplication;
  Reader.FreeOnTerminate := True;
  Reader.Start;
end;

procedure TOutputConsole.CloseApplication(Sender: Tobject);
var
  eCode: Cardinal;
begin
  Reader := nil;
  GetExitCodeProcess(FProcessInfo.hProcess, eCode);
  FeCode := eCode;
  CloseHandle(hOutputRead);
  CloseHandle(hInputWrite);
  CloseHandle(FProcessInfo.hProcess);
  DoFinish;
end;

function TOutputConsole.Launch(hInputRead, hOutputWrite,
  hErrorWrite : THandle): Boolean;
var
  StartInfo: TStartupInfo;
begin
  Result := True;

  FillChar(StartInfo, SizeOf(TStartupInfo), 0);
  StartInfo.cb := SizeOf(TStartupInfo);
  StartInfo.dwFlags := STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW;
  StartInfo.hStdInput := hInputRead;
  StartInfo.hStdOutput := hOutputWrite;
  StartInfo.hStdError := hErrorWrite;

  if not CreateProcess(nil, PChar(FFileName + ' ' + FParams), nil,
    nil, True, CREATE_NEW_CONSOLE, nil, PChar(FDirectory), StartInfo,
    FProcessInfo) then
  begin
    Result := False;
    Exit;
  end;
  CloseHandle(FProcessInfo.hThread);
end;

procedure TOutputConsole.Stop;
begin
  if Executing then
    TerminateProcess(FProcessInfo.hProcess, FExitCode);
end;

procedure TOutputConsole.DoStart;
begin
  if Assigned(FOnStart) then
    FOnStart(Self, FileName, Params);
end;

procedure TOutputConsole.DoProgress;
begin
  if Assigned(FOnProgress) then
    FOnProgress(Self, FileName, Params, FOutput);
  FOutput := '';
end;

procedure TOutputConsole.DoFinish;
begin
  FExecuting := False;
  if Assigned(FOnFinish) then
    FOnFinish(Self, FFileName, FParams, FOutLines, FeCode);
end;

procedure Register;
begin
  RegisterComponents('Console', [TOutputConsole]);
end;

end.

