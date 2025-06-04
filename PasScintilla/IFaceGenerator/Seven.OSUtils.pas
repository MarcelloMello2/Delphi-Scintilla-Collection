unit Seven.OSUtils;

interface

uses
  Winapi.Windows,
  System.Types,
  System.UITypes,
  System.SysUtils,
  System.Classes,
  System.StrUtils,
  System.AnsiStrings;

Type
  TExecuteFlags = Set of (ExecInheritsHandles);

Function GetEnvironmentVariable(Const EnvVar: AnsiString): AnsiString; overload;
Function GetEnvironmentVariable(Const EnvVar: UnicodeString): UnicodeString; overload;
Function GetEnvironmentString(Index: Integer): {$IFDEF UNICODE}UnicodeString{$ELSE}AnsiString{$ENDIF};
Function GetEnvironmentVariableCount: Integer;
function ExecuteProcess(Const Path: RawByteString; Const ComLine: RawByteString; Flags: TExecuteFlags = []): Integer; overload;
function ExecuteProcess(Const Path: RawByteString; Const ComLine: Array of RawByteString; Flags: TExecuteFlags = []): Integer; overload;
function ExecuteProcess(Const Path: UnicodeString; Const ComLine: UnicodeString; Flags: TExecuteFlags = []): Integer; overload;
function ExecuteProcess(Const Path: UnicodeString; Const ComLine: Array of UnicodeString; Flags: TExecuteFlags = []): Integer; overload;

implementation

resourcestring
  SExecuteProcessFailed  = 'Failed to execute "%s", error code: %d';

Function GetEnvironmentVariable(Const EnvVar: AnsiString): AnsiString;

var
  oemenvvar, oemstr: RawByteString;
  i, hplen: LongInt;
  hp, p: PAnsiChar;
begin
  oemenvvar := uppercase(EnvVar);
  SetCodePage(oemenvvar, CP_OEMCP);
  Result := '';
  p := GetEnvironmentStringsA;
  hp := p;
  while hp^ <> #0 do
  begin
    oemstr := hp;
    { cache length, may change after uppercasing depending on code page }
    hplen := length(oemstr);
    { all environment variables are encoded in the oem code page }
    SetCodePage(oemstr, CP_OEMCP, false);
    i := System.AnsiStrings.ansipos('=', oemstr);
    if uppercase(copy(oemstr, 1, i - 1)) = oemenvvar then
    begin
      Result := copy(oemstr, i + 1, length(oemstr) - i);
      break;
    end;
    { next string entry }
    hp := hp + hplen + 1;
  end;
  FreeEnvironmentStringsA(p);
end;

Function GetEnvironmentVariable(Const EnvVar: UnicodeString): UnicodeString;

var
  s, upperenv: UnicodeString;
  i: longint;
  hp, p: pwidechar;
begin
  Result := '';
  p := GetEnvironmentStringsW;
  hp := p;
  upperenv := uppercase(EnvVar);
  while hp^ <> #0 do
  begin
    s := hp;
    i := pos('=', s);
    if uppercase(copy(s, 1, i - 1)) = upperenv then
    begin
      Result := copy(s, i + 1, length(s) - i);
      break;
    end;
    { next string entry }
    hp := hp + strlen(hp) + 1;
  end;
  FreeEnvironmentStringsW(p);
end;

Function GetEnvironmentVariableCount: Integer;

var
  hp, p: PAnsiChar;
begin
  Result := 0;
  p := GetEnvironmentStringsA();
  hp := p;
  If (hp <> Nil) then
    while hp^ <> #0 do
    begin
      Inc(Result);
      hp := hp + System.AnsiStrings.StrLen(hp) + 1;
    end;
  FreeEnvironmentStringsA(p);
end;

Function GetEnvironmentString(Index: Integer):
{$IFDEF UNICODE}UnicodeString{$ELSE}AnsiString{$ENDIF};

var
  hp, p: PAnsiChar;
{$IFDEF UNICODE}
  tmpstr: RawByteString;
{$ENDIF}
begin
  Result := '';
  p := GetEnvironmentStringsA;
  hp := p;
  If (hp <> Nil) then
  begin
    while (hp^ <> #0) and (Index > 1) do
    begin
      Dec(Index);
      hp := hp + System.AnsiStrings.StrLen(hp) + 1;
    end;
    If (hp^ <> #0) then
    begin
{$IFDEF UNICODE}
      tmpstr := hp;
      SetCodePage(tmpstr, CP_OEMCP, false);
      Result := UTF8ToString(tmpstr);
{$ELSE}
      Result := hp;
      SetCodePage(RawByteString(Result), CP_OEMCP, false);
{$ENDIF}
    end;
  end;
  FreeEnvironmentStringsA(p);
end;

function ExecuteProcess(Const Path: RawByteString; Const ComLine: RawByteString;
  Flags: TExecuteFlags = []): Integer;
begin
  Result := ExecuteProcess(UnicodeString(Path), UnicodeString(ComLine), Flags);
end;

function ExecuteProcess(Const Path: UnicodeString; Const ComLine: UnicodeString;
  Flags: TExecuteFlags = []): Integer;
// win specific  function
var
  SI: TStartupInfoW;
  PI: TProcessInformation;
  Proc: THandle;
  l: DWord;
  CommandLine: UnicodeString;
  e: EOSError;
  ExecInherits: longbool;
begin
  FillChar(SI, SizeOf(SI), 0);
  SI.cb := SizeOf(SI);
  SI.wShowWindow := 1;
  { always surround the name of the application by quotes
    so that long filenames will always be accepted. But don't
    do it if there are already double quotes, since Win32 does not
    like double quotes which are duplicated!
  }
  if pos('"', Path) = 0 then
    CommandLine := '"' + Path + '"'
  else
    CommandLine := Path;
  if ComLine <> '' then
    CommandLine := CommandLine + ' ' + ComLine + #0
  else
    CommandLine := CommandLine + #0;

  ExecInherits := ExecInheritsHandles in Flags;

  if not CreateProcessW(nil, pwidechar(CommandLine), Nil, Nil, ExecInherits,
    $20, Nil, Nil, SI, PI) then
  begin
    e := EOSError.CreateFmt(SExecuteProcessFailed, [CommandLine, GetLastError]);
    e.ErrorCode := GetLastError;
    raise e;
  end;
  Proc := PI.hProcess;
  if WaitForSingleObject(Proc, DWord($FFFFFFFF)) <> $FFFFFFFF then
  begin
    GetExitCodeProcess(Proc, l);
    CloseHandle(Proc);
    CloseHandle(PI.hThread);
    Result := l;
  end
  else
  begin
    e := EOSError.CreateFmt(SExecuteProcessFailed, [CommandLine, GetLastError]);
    e.ErrorCode := GetLastError;
    CloseHandle(Proc);
    CloseHandle(PI.hThread);
    raise e;
  end;
end;

function ExecuteProcess(Const Path: RawByteString;
  Const ComLine: Array of RawByteString; Flags: TExecuteFlags = []): Integer;

var
  CommandLine: UnicodeString;
  i: Integer;

begin
  CommandLine := '';
  for i := 0 to High(ComLine) do
    if System.AnsiStrings.ansipos(' ', ComLine[i]) <> 0 then
      CommandLine := CommandLine + ' ' + '"' + String(ComLine[i]) + '"'
    else
      CommandLine := CommandLine + ' ' + String(ComLine[i]);
  ExecuteProcess := ExecuteProcess(UnicodeString(Path), CommandLine, Flags);
end;

function ExecuteProcess(Const Path: UnicodeString;
  Const ComLine: Array of UnicodeString; Flags: TExecuteFlags = []): Integer;

var
  CommandLine: UnicodeString;
  i: Integer;

begin
  CommandLine := '';
  for i := 0 to High(ComLine) do
    if pos(' ', ComLine[i]) <> 0 then
      CommandLine := CommandLine + ' ' + '"' + ComLine[i] + '"'
    else
      CommandLine := CommandLine + ' ' + ComLine[i];
  ExecuteProcess := ExecuteProcess(Path, CommandLine, Flags);
end;

end.
