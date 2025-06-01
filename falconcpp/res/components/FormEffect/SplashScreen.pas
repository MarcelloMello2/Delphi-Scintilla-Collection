unit SplashScreen;

interface

uses
  Windows, Forms, SysUtils, Classes, FormEffect, PNGImage, Graphics, ExtCtrls;

type
  TSplashScreen = class(TComponent)
  private
    { Private declarations }
    FResName: String;
    FPicture: TPicture;
    FWaitTime: Integer;
    FTimeIn: Integer;
    FDelayTextOut: Integer;
    FTimeOut: Integer;
    FBuffer: TBitmap;
    FBackgrd: TBitmap;
    FTransparentColor: TColor;
    FForm: TForm;
    FTimer: TTimer;
    FFont: TFont;
    FShowing: Boolean;
    FRect: TRect;
    procedure SetPicture(Value: TPicture);
    procedure SetFont(Value: TFont);
    procedure Timer(Sender: TObject);
    function GetHandle: HWND;
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Show;
    procedure Hide;
    procedure Update;
    procedure Draw(X, Y: Integer; Image: TBitmap); overload;
    procedure Draw(X, Y: Integer; Image: TBitmap; Alpha: TColor); overload;
    procedure ClearBuffer;
    procedure TextOut(X, Y: Integer; const Text: String;
      ClearLast: Boolean = True);
    property Buffer: TBitmap read FBuffer;
    property Backgrd: TBitmap read FBackgrd;
    property Handle: HWND read GetHandle;
    property Showing: Boolean read FShowing;
  published
    { Published declarations }
    property PNGResName: String read FResName write FResName;
    property Picture: TPicture read FPicture write SetPicture;
    property WaitTime: Integer read FWaitTime write FWaitTime;
    property TimeOut: Integer read FTimeOut write FTimeOut;
    property TimeIn: Integer read FTimeIn write FTimeIn;
    property DelayTextOut: Integer read FDelayTextOut write FDelayTextOut;
    property Font: TFont read FFont write SetFont;
    property TransparentColor: TColor read FTransparentColor write
      FTransparentColor;
  end;

implementation

procedure TSplashScreen.SetPicture(Value: TPicture);
begin
  FPicture.Assign(Value);
end;

procedure TSplashScreen.SetFont(Value: TFont);
begin
  FFont.Assign(Value);
end;

constructor TSplashScreen.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FWaitTime := 3000;
  FTimeIn := 800;
  FTimeOut := 800;
  FShowing := False;
  FTransparentColor := clFuchsia;
  FTimer := TTimer.Create(Self);
  FTimer.Enabled := False;
  FTimer.OnTimer := Timer;
  FTimer.Interval := FWaitTime;
  FPicture := TPicture.Create;
  FFont := TFont.Create;
  FBuffer :=  TBitmap.Create;
  FBuffer.PixelFormat := pf32bit;
  FBackgrd :=  TBitmap.Create;
  FBackgrd.PixelFormat := pf32bit;
end;

destructor TSplashScreen.Destroy;
begin
  FBackgrd.Free;
  FBuffer.Free;
  FFont.Free;
  FPicture.Free;
  inherited Destroy;
end;

procedure TSplashScreen.Show;
var
  bmp: TBitmap;
begin
  if not FShowing then
  begin
    FShowing := True;
    FForm := TForm.Create(Self);
    FForm.Name := 'SplashScreenForm';
    FForm.BorderStyle := bsNone;
    if Assigned(FPicture.Graphic) then
    begin
      FBackgrd.Height := FPicture.Graphic.Height;
      FBackgrd.Width := FPicture.Graphic.Width;
      if FPicture.Graphic is TPNGImage then
      begin
        bmp := PNGToBMP32(TPNGImage(FPicture.Graphic));
        FBackgrd.Assign(bmp);
        bmp.Free;
      end
      else
      begin
        FBackgrd.Canvas.Draw(0, 0, FPicture.Graphic);
        BitmapToAlpha(FBackgrd, FTransparentColor);
      end;
    end
    else
    begin
      bmp := GetPNGResourceInBMP(FResName);
      FBackgrd.Assign(bmp);
      bmp.Free;
    end;
    FBuffer.Assign(FBackgrd);
    FBuffer.Canvas.Font.Assign(FFont);
    FForm.ClientHeight := FBackgrd.Height;
    FForm.ClientWidth := FBackgrd.Width;
    FForm.Position := poScreenCenter;
    if FTimeIn > 0 then
      FadeInForm(FForm, FBuffer, FTimeIn)
    else
    begin
      UpDateForm(FForm, FBuffer);
      FForm.Show;
    end;
    FTimer.Interval := FWaitTime;
    FTimer.Enabled := True;
  end;
end;

procedure TSplashScreen.Hide;
begin
  if not Assigned(FForm) then
    Exit;
  repeat
    Application.ProcessMessages;
    Sleep(100);
  until not FShowing;
  if FTimeOut > 0 then
    FadeOutForm(FForm, Buffer, FTimeOut)
  else
    FForm.Close;
  FForm.Free;
  FForm := nil;
end;

function TSplashScreen.GetHandle: HWND;
begin
  Result := INVALID_HANDLE_VALUE;
  if Assigned(FForm) then
    Result := FForm.Handle;
end;

procedure TSplashScreen.Update;
begin
  if FShowing then
    UpDateForm(FForm, Buffer);
  Application.ProcessMessages;
end;

procedure TSplashScreen.Draw(X, Y: Integer; Image: TBitmap);
begin
  DrawBMP32(X, Y, Image, Buffer);
  Update;
end;

procedure TSplashScreen.Draw(X, Y: Integer; Image: TBitmap; Alpha: TColor);
begin
  DrawBMP(X, Y, Image, FBuffer, Alpha);
  Update;
end;

procedure TSplashScreen.ClearBuffer;
begin
  Buffer.Canvas.Draw(0, 0, Backgrd);
end;

procedure TSplashScreen.TextOut(X, Y: Integer; const Text: String;
  ClearLast: Boolean = True);
var
  bmp: TBitmap;
begin
  if not FShowing then
    Exit;
  if ClearLast then
    Buffer.Canvas.CopyRect(FRect, FBackgrd.Canvas, FRect);
  bmp := TBitmap.Create;
  bmp.PixelFormat := pf32bit;
  bmp.Canvas.Font.Assign(FBuffer.Canvas.Font);
  bmp.Height := bmp.Canvas.TextHeight(Text);
  bmp.Width := bmp.Canvas.TextWidth(Text);
  bmp.Canvas.Brush.Style := bsClear;
  FillBitmap(bmp);
  bmp.Canvas.TextOut(0, 0, Text);
  SetSolidText(bmp, bmp.Canvas.Font.Color, clWhite);
  FRect.Left := X;
  FRect.Top := Y;
  FRect.Right := X + bmp.Width;
  FRect.Bottom := Y + bmp.Height;
  Draw(X, Y, bmp);
  bmp.Free;
  if FDelayTextOut > 0 then
    Sleep(FDelayTextOut);
end;

procedure TSplashScreen.Timer(Sender: TObject);
begin
  FShowing := False;
  FTimer.Enabled := False;
end;

end.
 