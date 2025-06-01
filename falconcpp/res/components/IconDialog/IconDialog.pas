unit IconDialog;

interface

uses
  Forms, Windows, SysUtils, Classes, Controls, Graphics, ShellAPI, IconsEx;

const
  shell32 = 'shell32.dll';
  initpath = '%SystemRoot%\system32\SHELL32.dll';

type
  TIconDialog = class(TComponent)
  private
    { Private declarations }
    FIcon: TIcon;
    FFileName: String;
    FIndex: Integer;
    FHandle: HWND;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: Boolean;
    property Icon: TIcon read FIcon;
    property Handle: HWND read FHandle write FHandle;
  published
    { Published declarations }
    property FileName: String read FFileName write FFileName;
    property IconIndex: Integer read FIndex write FIndex;
  end;

function PickIcon(const hwndOwner: HWND; var sIconFile: string;
  var iIconNum: Integer): Boolean;

implementation

function PickIconDlgA(hwndOwner: HWND; lpstrFile: PChar; nMaxFile: Word;
  var lpdwIconIndex: DWORD): Bool; stdcall external shell32 name 'PickIconDlg';

function PickIconDlgW(hwndOwner: HWND; lpstrFile: PWideChar; nMaxFile: Word;
  var lpdwIconIndex: DWORD): Bool; stdcall external shell32 name 'PickIconDlg';


function PickIcon(const hwndOwner: HWND; var sIconFile: string;
  var iIconNum: Integer): Boolean;
var
  pch: PChar;
  pwch: PWideChar;
  {$IFDEF DELPHI3UP}
  c: Cardinal;
  {$ELSE}
  c: Integer;
  {$ENDIF}
begin
  c := iIconNum;
  if Win32Platform = VER_PLATFORM_WIN32_NT then
  begin
    GetMem(pwch, MAX_PATH * sizeof(WideChar));
    try
      StringToWideChar(sIconFile, pwch, MAX_PATH);
      Result := PickIconDlgW(hwndOwner, pwch, MAX_PATH, DWORD(c));
      if Result then
      begin
        iIconNum := c;
        sIconFile := WideCharToString(pwch);
      end;
    finally
      FreeMem(pwch);
    end;
  end
  else
  begin
    GetMem(pch, MAX_PATH);
    try
      StrPCopy(pch, sIconFile);
      Result := PickIconDlgA(hwndOwner, pch, MAX_PATH , DWORD(c));
      if Result then
      begin
        iIconNum := c;
        sIconFile := pch;
      end;
    finally
      FreeMem(pch);
    end;
  end;
end;


constructor TIconDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if Assigned(AOwner) then
  begin
    if AOwner is TWinControl then
    begin
      if not(csDesigning in ComponentState) then
        FHandle := (AOwner as TWinControl).Handle
      else
        FHandle := Application.Handle;
    end
    else
    begin
      if AOwner is TApplication then
        FHandle := (AOwner as TApplication).Handle;
    end;
  end
  else
    FHandle := Application.Handle;
  FIcon := TIcon.Create;
end;

destructor TIconDialog.Destroy;
begin
  FIcon.Free;
  inherited Destroy;
end;

function TIconDialog.Execute: Boolean;
var
  S: String;
  Index: Integer;
  Ms: TMemoryStream;
  List: TStrings;
label
  ShellIcon;
begin
  S := FileName;
  Index := IconIndex;
  Result := PickIcon(Handle, S, Index);
  if Result then
  begin
    FileName := S;
    IconIndex := Index;
    Ms := TMemoryStream.Create;
    List := TStringList.Create;
    GetResourceList(FileName, List);
    if (IconIndex >= List.Count) then goto ShellIcon;
    if ExtractIconToStream(FileName, Ms, List.Strings[IconIndex]) then
    begin
      Ms.Position := 0;
      FIcon.LoadFromStream(Ms);
    end
    else
    begin
      ShellIcon:
      FIcon.Handle := ExtractIcon(hInstance, PChar(FileName), IconIndex);
    end;
    List.Free;
    Ms.Free;
  end;
end;

end.
