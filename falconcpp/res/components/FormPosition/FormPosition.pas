unit FormPosition;

interface

uses
  Windows, Messages, Forms, SysUtils, Classes, IniFiles;

type

  TFullScreenEvent = procedure(Sender:TObject; FullScreen: Boolean) of Object;
  
  TWindow = packed record
    FullScreen: Boolean;
    Left:integer;
    Top:integer;
    Height:integer;
    Width:integer;
    State:integer;
  end;

  TFormPosition = class(TComponent)
  private
    { Private declarations }
    FFileName: String;
    FForm: TForm;
    FWindow: TWindow;
    FFullScreen: TFullScreenEvent;
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    procedure SetForm(Form: TForm);
    procedure SetFullScreen; overload;
    procedure SetFullScreen(Value: Boolean); overload;
    function FullScreen: Boolean;
    procedure SetNormal;
    procedure Load;
    procedure Save;
  published
    { Published declarations }
    property FileName: String read FFileName write FFileName;
    property OnFullScreen: TFullScreenEvent read FFullScreen write FFullScreen;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Additional', [TFormPosition]);
end;

constructor TFormPosition.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if AOwner is TForm then
    FForm := TForm(AOwner)
  else
    FForm := Application.MainForm;
  FFileName := 'FormPosition.ini';
end;

procedure TFormPosition.SetForm(Form: TForm);
begin
  if Assigned(Form) then
    FForm := Form;
end;

procedure TFormPosition.SetFullScreen;
begin
  SetFullScreen(True);
end;

procedure TFormPosition.SetFullScreen(Value: Boolean);
var
  Style: Cardinal;
begin
  if Assigned(FFullScreen) then
    FFullScreen(Self, Value);
  if Value then
  begin
    if not(FForm.WindowState = wsMaximized) then
    begin
      FWindow.Left := FForm.Left;
      FWindow.Top := FForm.Top;
      FWindow.Height := FForm.Height;
      FWindow.Width := FForm.Width;
    end;
    case FForm.WindowState of
      wsNormal   : FWindow.State := 0;
      wsMinimized: FWindow.State := 1;
      wsMaximized: FWindow.State := 2;
    end;
    Style := GetWindowLong(FForm.Handle, GWL_STYLE);
    Style := Style and not (WS_CAPTION or WS_SIZEBOX) or WS_POPUP;
    SetWindowLong(FForm.Handle, GWL_STYLE, Style);
    UpdateWindow(FForm.Handle);
    FForm.Top := 0;
    FForm.Left := 0;
    FForm.Width := Screen.Width;
    FForm.Height := Screen.Height;
  end
  else
  begin
    Style := GetWindowLong(FForm.Handle, GWL_STYLE);
    Style := (Style or WS_CAPTION or WS_SIZEBOX) and not WS_POPUP;
    SetWindowLong(FForm.Handle, GWL_STYLE, Style);
    UpdateWindow(FForm.Handle);
    if not(FForm.WindowState = wsMaximized) then
    begin
      FForm.Left := FWindow.Left;
      FForm.Top := FWindow.Top;
      FForm.Height := FWindow.Height;
      FForm.Width := FWindow.Width;
    end
    else
    begin
      if FWindow.State = 2 then
      begin
        //Fix redraw Bug on back to normal mode
        SendMessage(FForm.Handle, WM_SETREDRAW, 0, 0) ;
        ShowWindow(FForm.Handle, SW_SHOWMAXIMIZED);
        SendMessage(FForm.Handle, WM_SETREDRAW, 1, 0) ;
        RedrawWindow(HWND_DESKTOP, nil, 0,
          RDW_FRAME or RDW_INVALIDATE or RDW_ALLCHILDREN or RDW_NOINTERNALPAINT);
      end;
    end;
  end;
  FWindow.FullScreen := Value;
end;

function TFormPosition.FullScreen: Boolean;
begin
  Result := FWindow.FullScreen;
end;

procedure TFormPosition.SetNormal;
begin
  SetFullScreen(False);
end;

procedure TFormPosition.Load;
var
  ini:TIniFile;
  X, Y: Integer;
begin
  if not Assigned(FForm) then Exit;
  ini := TIniFile.Create(FFileName);
  FWindow.Width := ini.ReadInteger('Bounds','Width', FForm.Width);
  FWindow.Height := ini.ReadInteger('Bounds','Height', FForm.Height);
  X := (GetSystemMetrics(SM_CXSCREEN) - FWindow.Width) div 2;
  Y := (GetSystemMetrics(SM_CYSCREEN) - FWindow.Height) div 2;
  FWindow.Left := ini.ReadInteger('Bounds','Left', X);
  FWindow.Top := ini.ReadInteger('Bounds','Top', Y);
  FWindow.State := ini.ReadInteger('Bounds','State',0);
  FWindow.FullScreen := ini.ReadBool('Bounds','FullScreen', False);
  with FForm do
  begin
    Left := FWindow.Left;
    Top := FWindow.Top;
    Width := FWindow.Width;
    Height := FWindow.Height;
    case FWindow.State of
      0:WindowState := wsNormal;
      1:WindowState := wsMinimized;
      2:WindowState := wsMaximized;
    end;
  end;
  ini.Free;
  if FWindow.FullScreen then
    SetFullScreen;
end;

procedure TFormPosition.Save;
var
  ini:TIniFile;
begin
  if not Assigned(FForm) then Exit;
  ini := TIniFile.Create(FFileName);
  with FForm do
  begin
    if not (wsMaximized = WindowState) and not FWindow.FullScreen then
    begin
      FWindow.Left := Left;
      FWindow.Top := Top;
      FWindow.Width := Width;
      FWindow.Height := Height;
    end;
    case WindowState of
      wsNormal: FWindow.State := 0;
      wsMinimized: FWindow.State := 1;
      wsMaximized: FWindow.State := 2;
    end;
    ini.WriteInteger('Bounds','Width', FWindow.Width);
    ini.WriteInteger('Bounds','Height', FWindow.Height);
    ini.WriteInteger('Bounds','Left', FWindow.Left);
    ini.WriteInteger('Bounds','Top', FWindow.Top);
    ini.WriteInteger('Bounds','State', FWindow.State);
    ini.WriteBool('Bounds','FullScreen', FWindow.FullScreen);
  end;
  ini.Free;
end;

end.

