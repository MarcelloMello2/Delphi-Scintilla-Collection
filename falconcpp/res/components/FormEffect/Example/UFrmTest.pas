unit UFrmTest;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, PNGImage, ExtCtrls, StdCtrls, SplashScreen, FormEffect, ComCtrls,
  XPMan, HSLUtils, Math;

type

  PArrayRGBQuad = ^TArrayRGBQuad;
  tagARRAYRGBQUAD = array[Word] of TRGBQuad;
  TArrayRGBQuad = tagARRAYRGBQUAD;

  TFrmTest = class(TForm)
    SplashScreen1: TSplashScreen;
    Button1: TButton;
    Image1: TImage;
    lblr: TLabel;
    lblg: TLabel;
    lblb: TLabel;
    lbla: TLabel;
    Button2: TButton;
    Button3: TButton;
    TrackBar1: TTrackBar;
    TrackBar2: TTrackBar;
    TrackBar3: TTrackBar;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    XPManifest1: TXPManifest;
    TrackBar4: TTrackBar;
    Label4: TLabel;
    Edit1: TEdit;
    TrackBar5: TTrackBar;
    TrackBar6: TTrackBar;
    TrackBar7: TTrackBar;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Button4: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure TrackBar2Change(Sender: TObject);
    procedure TrackBar3Change(Sender: TObject);
    procedure TrackBarChange;
    procedure TrackBar4Change(Sender: TObject);
    procedure TrackBar5Change(Sender: TObject);
    procedure TrackBar6Change(Sender: TObject);
    procedure TrackBar7Change(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrmTest: TFrmTest;

implementation

{$R *.dfm}

{$R Recursos.RES}

procedure TFrmTest.FormCreate(Sender: TObject);
begin
  SplashScreen1.Show;
  SplashScreen1.Font.Color := clRed;
  SplashScreen1.TextOut(50, 50, 'hehehehehe');
  SplashScreen1.Buffer.SaveToFile('d:\buffer.bmp');
  SplashScreen1.Hide;
end;

procedure TFrmTest.Button1Click(Sender: TObject);
var
  bmp: TBitmap;
begin
  bmp := TBitmap.Create;
  bmp.PixelFormat := pf32bit;
  bmp.Height := 200;
  bmp.Width := 200;
  bmp.Canvas.Font.Name := 'Arial';
  bmp.Canvas.Font.Size := 25;
  bmp.Canvas.Font.Color := clOlive;
  bmp.Canvas.Brush.Style := bsClear;
  //FillBitmap(bmp);
  bmp.Canvas.TextOut(0, 0, 'Olá mundo!');
  //SetSolidText(bmp, bmp.Canvas.Font.Color, clWhite);
  Image1.Picture.Bitmap := bmp;
  //bmp.SaveToFile('D:\Teste.bmp');
  bmp.Free;
end;

procedure TFrmTest.Image1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  Pixel: PArrayRGBQuad;
begin
  if Image1.Picture.Bitmap.Empty then Exit;
  Pixel := Image1.Picture.Bitmap.ScanLine[Y];
  lblr.Caption := Format('R: %d, $%.2X', [Pixel^[X].rgbRed, Pixel^[X].rgbRed]);
  lblg.Caption := Format('G: %d, $%.2X', [Pixel^[X].rgbGreen, Pixel^[X].rgbGreen]);
  lblb.Caption := Format('B: %d, $%.2X', [Pixel^[X].rgbBlue, Pixel^[X].rgbBlue]);
  lbla.Caption := Format('A: %d, $%.2X', [Pixel^[X].rgbReserved, Pixel^[X].rgbReserved]);
end;

procedure TFrmTest.Button2Click(Sender: TObject);
begin
  Image1.Picture.LoadFromFile('D:\teste.bmp');
end;

procedure TFrmTest.Button3Click(Sender: TObject);
begin
  ShowMessage(ColorToString(TColor(MulDiv(MulDiv(clred, clWhite,85), 1, 255))));
end;

procedure TFrmTest.TrackBar1Change(Sender: TObject);
begin
  Label1.Caption := 'R: ' + IntToStr(TrackBar1.Position);
  TrackBarChange;
end;

procedure TFrmTest.TrackBar2Change(Sender: TObject);
begin
  Label2.Caption := 'G: ' + IntToStr(TrackBar2.Position);
  TrackBarChange;
end;

procedure TFrmTest.TrackBar3Change(Sender: TObject);
begin
  Label3.Caption := 'B: ' + IntToStr(TrackBar3.Position);
  TrackBarChange;
end;

procedure TFrmTest.TrackBar4Change(Sender: TObject);
begin
  Label4.Caption := 'A: ' + IntToStr(TrackBar4.Position);
  TrackBarChange;
end;

procedure TFrmTest.TrackBarChange;
var
  bmp, bmps: TBitmap;
  h, s, v: integer;
begin
  RGBtoHSLRange(RGB(TrackBar1.Position, TrackBar2.Position,
                        TrackBar3.Position), h, s, v);
  TrackBar5.Position := h;
  TrackBar6.Position := s;
  TrackBar7.Position := v;

  bmp := TBitmap.Create;
  bmp.PixelFormat := pf32bit;
  bmp.Height := 200;
  bmp.Width := 200;
  FillBitmap(bmp);

  bmps := TBitmap.Create;
  bmps.PixelFormat := pf32bit;
  bmps.Height := 200;
  bmps.Width := 200;
  FillBitmap(bmps, RGBA(TrackBar1.Position, TrackBar2.Position,
                        TrackBar3.Position, TrackBar4.Position));
  Edit1.Text := IntToStr(RGB(TrackBar1.Position, TrackBar2.Position,
                        TrackBar3.Position));
  DrawBMP32(0,0,bmps, bmp);

  Image1.Picture.Bitmap := bmp;
  bmps.Free;
  bmp.Free;
end;


procedure TFrmTest.TrackBar5Change(Sender: TObject);
begin
  Label5.Caption := 'H: ' + IntToStr(TrackBar5.Position);
end;

procedure TFrmTest.TrackBar6Change(Sender: TObject);
begin
  Label6.Caption := 'S: ' + IntToStr(TrackBar6.Position);
end;

procedure TFrmTest.TrackBar7Change(Sender: TObject);
begin
  Label7.Caption := 'V: ' + IntToStr(TrackBar7.Position);
end;

procedure TFrmTest.Button4Click(Sender: TObject);
begin
Image1.Picture.SaveToFile('D:\Teste.bmp');
end;

end.
