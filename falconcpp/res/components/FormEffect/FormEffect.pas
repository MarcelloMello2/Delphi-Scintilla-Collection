unit FormEffect;

interface

uses
  Windows, Math, PNGImage, HSLUtils, Graphics, Forms, Classes, SysUtils,
  Controls, CommCtrl, Consts;

type
  TColor32 = Cardinal;

const
  claBlack = TColor32($FF000000);
  claMaroon = TColor32($FF000080);
  claGreen = TColor32($FF008000);
  claOlive = TColor32($FF008080);
  claOrange = TColor32($FFFF8000);
  claNavy = TColor32($FF800000);
  claPurple = TColor32($FF800080);
  claTeal = TColor32($FF808000);
  claGray = TColor32($FF808080);
  claSilver = TColor32($FFC0C0C0);
  claRed = TColor32($FF0000FF);
  claLime = TColor32($FF00FF00);
  claYellow = TColor32($FF00FFFF);
  claBlue = TColor32($FFFF0000);
  claFuchsia = TColor32($FFFF00FF);
  claAqua = TColor32($FFFFFF00);
  claWhite = TColor32($FFFFFFFF);

Function UpdateLayeredWindow(hwnd: hwnd; hdcDst: HDC; pptDst: PPoint;
  psize: psize; hdcSrc: HDC; pptSrc: PPoint; crKey: TColor;
  pblend: PBlendFunction; dwFlags: DWORD): BOOL; stdcall;
external 'user32.dll';

procedure ConvertTo32BitImageList(const ImageList: TImageList);
procedure AddImages(imglist: TImageList; resname: String); overload;
procedure AddImages(imglist1, imglist2: TImageList; resname: String); overload;
procedure AddImages(imglist: TImageList; Bitmap: TBitmap); overload;

procedure BitmapToAlpha(Bitmap: TBitmap; Color: TColor = clFuchsia);
procedure PreMultiplyBitmap(Bitmap: TBitmap);
function CreatePreMultipliedRGBQuad(Color: TColor; Alpha: Byte = $FF): TRGBQuad;
procedure SetAlphaValue(Bitmap: TBitmap; Alpha: Byte = $FF);
procedure SetSolidText(Bitmap: TBitmap; Color, Bckgrd: TColor);
procedure FillBitmap(Bitmap: TBitmap; Color: Cardinal = Cardinal(-1));
Procedure DrawBMP32(X, Y: Integer; AlphaBitmap, FinalBitmap: TBitmap);
Procedure DrawBMP(X, Y: Integer; NormalBitmap, FinalBitmap: TBitmap;
  TransparentColor: TColor = clWhite);
Procedure UpDateForm(Form: TForm; Bmp: TBitmap; Opacite: Byte = $FF);
Procedure FadeOutForm(Form: TForm; BMP32: TBitmap; Delay: Integer = 800);
Procedure FadeInForm(Form: TForm; BMP32: TBitmap; Delay: Integer = 800);
Function PNGToBMP32(PNG: TPNGImage): TBitmap;
function LoadPNG(const FileName: String; var BMP32: TBitmap): Boolean;
function GetPNGResource(resname: String): TPNGImage;
function GetPNGResourceInBMP(resname: String): TBitmap;
function RGBA(r, g, b, a: Byte): COLORREF;
function GetAValue(RGBA: DWORD): Byte;

implementation

uses Types;

function RGBA(r, g, b, a: Byte): COLORREF;
begin
  Result := (r or (g shl 8) or (b shl 16) or (a shl 24));
end;

function GetAValue(RGBA: DWORD): Byte;
begin
  Result := Byte(RGBA shr 24);
end;

procedure ConvertTo32BitImageList(const ImageList: TImageList);
// CommCtrl, Classes, Consts;
const
  Mask: array [Boolean] of Longint = (0, ILC_MASK);
var
  TemporyImageList: TImageList;
begin
  if Assigned(ImageList) then
  begin
    TemporyImageList := TImageList.Create(nil);
    try
      TemporyImageList.Assign(ImageList);
      with ImageList do
      begin
        ImageList.Handle := ImageList_Create(Width, Height,
          ILC_COLOR32 or Mask[Masked], 0, AllocBy);
        if not ImageList.HandleAllocated then
          raise EInvalidOperation.Create(SInvalidImageList);
      end;
      ImageList.AddImages(TemporyImageList);
    finally
      TemporyImageList.Free;
    end;
  end;
end;

procedure AddImages(imglist: TImageList; Bitmap: TBitmap);
begin
  imglist.AddMasked(Bitmap, clNone);
end;

procedure AddImages(imglist: TImageList; resname: String);
var
  Rs: TResourceStream;
  PNG: TPNGImage;
  Bmp: TBitmap;
begin
  Rs := TResourceStream.Create(HInstance, resname, RT_RCDATA);
  Rs.Position := 0;
  PNG := TPNGImage.Create;
  PNG.LoadFromStream(Rs);
  Bmp := PNGToBMP32(PNG);
  AddImages(imglist, Bmp);
  Bmp.Free;
  Rs.Free;
  PNG.Free;
end;

procedure AddImages(imglist1, imglist2: TImageList; resname: String);
var
  Rs: TResourceStream;
  PNG: TPNGImage;
  bmp1, bmp2: TBitmap;
begin
  Rs := TResourceStream.Create(HInstance, resname, RT_RCDATA);
  Rs.Position := 0;
  PNG := TPNGImage.Create;
  PNG.LoadFromStream(Rs);
  bmp1 := PNGToBMP32(PNG);
  bmp2 := TBitmap.Create;
  bmp2.PixelFormat := pf32bit;
  bmp2.Height := imglist2.Height;
  bmp2.Width := bmp1.Width;
  bmp2.Canvas.CopyRect(bmp2.Canvas.ClipRect, bmp1.Canvas,
    Rect(0, bmp2.Height, bmp2.Width, bmp1.Height));
  AddImages(imglist2, bmp2);
  bmp1.Height := imglist1.Height;
  AddImages(imglist1, bmp1);
  bmp1.Free;
  bmp2.Free;
  Rs.Free;
  PNG.Free;
end;

function GetPNGResource(resname: String): TPNGImage;
var
  Rs: TResourceStream;
begin
  Rs := TResourceStream.Create(HInstance, resname, RT_RCDATA);
  Rs.Position := 0;
  Result := TPNGImage.Create;
  Result.LoadFromStream(Rs);
  Rs.Free;
end;

function GetPNGResourceInBMP(resname: String): TBitmap;
var
  PNG: TPNGImage;
begin
  PNG := GetPNGResource(resname);
  Result := PNGToBMP32(PNG);
  PNG.Free;
end;

procedure BitmapToAlpha(Bitmap: TBitmap; Color: TColor = clFuchsia);
var
  X, Y: Integer;
  Pixel: PRGBQuad;
  Current: TColor;
begin
  Bitmap.PixelFormat := pf32bit;
  with Bitmap do
    for Y := 0 to Height - 1 do
    begin
      Pixel := Scanline[Y];
      for X := 0 to Width - 1 do
      begin
        Current := RGB(Pixel.rgbRed, Pixel.rgbGreen, Pixel.rgbBlue);
        if (Current = Color) then
          ZeroMemory(Pixel, Sizeof(TRGBQuad))
        else
          Pixel.rgbReserved := $FF;
        Inc(Pixel);
      end;
    end;
end;

procedure PreMultiplyBitmap(Bitmap: TBitmap);
var
  X, Y: Integer;
  Pixel: PRGBQuad;
begin
  Assert(Bitmap.PixelFormat = pf32bit);
  with Bitmap do
    for Y := Height - 1 downto 0 do
    begin
      Pixel := Scanline[Y];
      for X := Width - 1 downto 0 do
      begin
        Pixel.rgbBlue := MulDiv(Pixel.rgbBlue, Pixel.rgbReserved, 255);
        Pixel.rgbGreen := MulDiv(Pixel.rgbGreen, Pixel.rgbReserved, 255);
        Pixel.rgbRed := MulDiv(Pixel.rgbRed, Pixel.rgbReserved, 255);
        Inc(Pixel);
      end;
    end;
end;

function CreatePreMultipliedRGBQuad(Color: TColor; Alpha: Byte = $FF): TRGBQuad;
begin
  Color := ColorToRGB(Color);
  Result.rgbBlue := MulDiv(GetBValue(Color), Alpha, $FF);
  Result.rgbGreen := MulDiv(GetGValue(Color), Alpha, $FF);
  Result.rgbRed := MulDiv(GetRValue(Color), Alpha, $FF);
  Result.rgbReserved := Alpha;
end;

procedure SetAlphaValue(Bitmap: TBitmap; Alpha: Byte = $FF);
var
  X, Y: Integer;
  Pixel: PRGBQuad;
begin
  Assert(Bitmap.PixelFormat = pf32bit);
  with Bitmap do
    for Y := Pred(Height) downto 0 do
    begin
      Pixel := Scanline[Y];
      for X := Pred(Width) downto 0 do
      begin
        Pixel.rgbReserved := Alpha;
        Inc(Pixel);
      end;
    end;
end;

function GetAlphaValue(Bckgrd, Final: Cardinal): Byte;
var
  a1, a2, a3: Byte;
begin
  a1 := GetRValue(Bckgrd) - GetRValue(Final);
  a2 := GetGValue(Bckgrd) - GetGValue(Final);
  a3 := GetBValue(Bckgrd) - GetBValue(Final);
  Result := Max(a1, Max(a2, a3));
end;

procedure SetSolidText(Bitmap: TBitmap; Color, Bckgrd: TColor);
var
  X, Y: Integer;
  Pixel: PRGBQuad;
begin
  Assert(Bitmap.PixelFormat = pf32bit);
  with Bitmap do
    for Y := Pred(Height) downto 0 do
    begin
      Pixel := Scanline[Y];
      for X := Pred(Width) downto 0 do
      begin
        if Pixel.rgbReserved > 0 then
        begin
          ZeroMemory(Pixel, Sizeof(TRGBQuad));
        end
        else
        begin
          Pixel.rgbReserved := GetAlphaValue(Bckgrd,
            RGB(Pixel.rgbRed, Pixel.rgbGreen, Pixel.rgbBlue));
          Pixel.rgbBlue := GetBValue(Color);
          Pixel.rgbGreen := GetGValue(Color);
          Pixel.rgbRed := GetRValue(Color);
        end;
        Inc(Pixel);
      end;
    end;
end;

procedure FillBitmap(Bitmap: TBitmap; Color: Cardinal = $FFFFFFFF);
var
  X, Y: Integer;
  Pixel: PRGBQuad;
begin
  Bitmap.PixelFormat := pf32bit;
  with Bitmap do
    for Y := Pred(Height) downto 0 do
    begin
      Pixel := Scanline[Y];
      for X := Pred(Width) downto 0 do
      begin
        Pixel.rgbBlue := GetBValue(Color);
        Pixel.rgbGreen := GetGValue(Color);
        Pixel.rgbRed := GetRValue(Color);
        Pixel.rgbReserved := GetAValue(Color);
        Inc(Pixel);
      end;
    end;
end;

Procedure DrawBMP32(X, Y: Integer; AlphaBitmap, FinalBitmap: TBitmap);
Var
  pd, pa: pByteArray;
  i, ix, j, a, b, ad: Integer;
Begin
  If (Assigned(AlphaBitmap) = False) Then
    Exit;
  If (Assigned(FinalBitmap) = False) Then
    Exit;
  FinalBitmap.PixelFormat := pf32bit;
  For j := 0 To AlphaBitmap.Height - 1 Do
    If (j + Y <= FinalBitmap.Height - 1) And (j + Y >= 0) Then
    Begin
      pd := FinalBitmap.Scanline[j + Y];
      pa := AlphaBitmap.Scanline[j];
      For i := 0 To AlphaBitmap.Width - 1 Do
      Begin
        a := pa[i * 4 + 3];
        b := Abs($FF - a);
        ix := Max(Min(i + X, FinalBitmap.Width - 1), 0);
        ad := pd[ix * 4 + 3];
        If (a >= 240) Then
        Begin
          pd[ix * 4 + 0] := pa[i * 4 + 0];
          pd[ix * 4 + 1] := pa[i * 4 + 1];
          pd[ix * 4 + 2] := pa[i * 4 + 2];
          pd[ix * 4 + 3] := $FF;
        End
        Else If (a >= 0) Then
        Begin
          If (ad < 240) Then
            b := Round(Max(Min(a + (ad * b) / $FF, $FF), 0)); ;
          pd[ix * 4 + 0] := Round
            (Max(Min(pa[i * 4 + 0] + b * pd[ix * 4 + 0] / $FF, $FF), 0));
          pd[ix * 4 + 1] := Round
            (Max(Min(pa[i * 4 + 1] + b * pd[ix * 4 + 1] / $FF, $FF), 0));
          pd[ix * 4 + 2] := Round
            (Max(Min(pa[i * 4 + 2] + b * pd[ix * 4 + 2] / $FF, $FF), 0));
          If (ad < 240) Then
            pd[ix * 4 + 3] := b;
        End;
      End;
    End;
End;

Procedure DrawBMP(X, Y: Integer; NormalBitmap, FinalBitmap: TBitmap;
  TransparentColor: TColor = clWhite);
var
  Bmp: TBitmap;
begin
  Bmp := TBitmap.Create;
  Bmp.Height := NormalBitmap.Height;
  Bmp.Width := NormalBitmap.Width;
  Bmp.Canvas.Draw(0, 0, NormalBitmap);
  BitmapToAlpha(Bmp, TransparentColor);
  DrawBMP32(X, Y, Bmp, FinalBitmap);
  Bmp.Free;
end;

Procedure UpDateForm(Form: TForm; Bmp: TBitmap; Opacite: Byte = $FF);
Const
  WS_EX_LAYERED = $80000;
Var
  Size: psize;
  TopLeft, BmpTopLeft: TPoint;
  Blend: TBlendFunction;
Begin
  With Form Do
  Begin
    SetWindowLong(Handle, GWL_EXSTYLE,
      GetWindowLong(Handle, GWL_EXSTYLE) Or WS_EX_LAYERED);
    New(Size);
    Size.cx := Width;
    Size.cy := Height;
    TopLeft := BoundsRect.TopLeft;
    BmpTopLeft := Point(0, 0);
    With Blend Do
    Begin
      BlendOp := 0;
      BlendFlags := 0;
      SourceConstantAlpha := Opacite;
      AlphaFormat := 1;
    End;
    Bmp.PixelFormat := pf32bit;
    UpdateLayeredWindow(Handle, GetDC(0), @TopLeft, Size, Bmp.Canvas.Handle,
      @BmpTopLeft, 0, @Blend, 2);
    Dispose(Size);
  End;
End;

Function PNGToBMP32(PNG: TPNGImage): TBitmap;
Var
  AlphaBitmap: TBitmap;
  pd, pa: pByteArray;
  i, j, a: Integer;
Begin
  Result := TBitmap.Create;
  AlphaBitmap := TBitmap.Create;
  AlphaBitmap.Height := PNG.Height;
  AlphaBitmap.Width := PNG.Width;
  AlphaBitmap.PixelFormat := pf24bit;
  Result.Assign(PNG);
  Result.PixelFormat := pf32bit;
  If (PNG.Transparent) Then
  Begin
    For i := 0 To PNG.Height - 1 Do
      For j := 0 To PNG.Width - 1 Do
        If (PNG.AlphaScanline[i][j] >= 240) Then
          AlphaBitmap.Canvas.Pixels[j, i] := $FFFFFF
        Else
          AlphaBitmap.Canvas.Pixels[j, i] := HSLRangeToRGB(0, 0,
            PNG.AlphaScanline[i][j]);
  End
  Else
  Begin
    AlphaBitmap.Canvas.Brush.Style := bsSolid;
    AlphaBitmap.Canvas.Brush.Color := $FFFFFF;
    AlphaBitmap.Canvas.FillRect(AlphaBitmap.Canvas.ClipRect);
  End;
  For j := 0 To PNG.Height - 1 Do
  Begin
    pd := Result.Scanline[j];
    pa := AlphaBitmap.Scanline[j];
    For i := 0 To PNG.Width - 1 Do
    Begin
      a := pa[i * 3];
      If (a < 240) Then
      Begin
        pd[i * 4 + 0] := Round(Max(Min(pd[i * 4 + 0] * a / $FF, $FF), 0));
        pd[i * 4 + 1] := Round(Max(Min(pd[i * 4 + 1] * a / $FF, $FF), 0));
        pd[i * 4 + 2] := Round(Max(Min(pd[i * 4 + 2] * a / $FF, $FF), 0));
      End;
      pd[i * 4 + 3] := a;
    End;
  End;
  AlphaBitmap.Free;
End;

function LoadPNG(const FileName: String; var BMP32: TBitmap): Boolean;
var
  PNG: TPNGImage;
begin
  Result := False;
  PNG := TPNGImage.Create;
  try
    PNG.LoadFromFile(FileName);
  except
    PNG.Free;
    Exit;
  end;
  BMP32 := PNGToBMP32(PNG);
  Result := True;
end;

Procedure FadeInForm(Form: TForm; BMP32: TBitmap; Delay: Integer = 800);
Const
  Division = 20;
Var
  X: Integer;
  Opacite: Byte;
Begin
  Form.Height := BMP32.Height;
  Form.Width := BMP32.Width;
  Application.ProcessMessages;
  UpDateForm(Form, BMP32, 0);
  Form.Show;
  Application.ProcessMessages;
  For X := 1 To Division Do
  Begin
    Opacite := Round(Max(Min($FF / Division * X, $FF), 0));
    UpDateForm(Form, BMP32, Opacite);
    Sleep(Delay Div Division);
  End;
  UpDateForm(Form, BMP32);
End;

Procedure FadeOutForm(Form: TForm; BMP32: TBitmap; Delay: Integer = 800);
Const
  Division = 40;
Var
  X: Integer;
  Opacite: Byte;
Begin
  Form.Height := BMP32.Height;
  Form.Width := BMP32.Width;
  UpDateForm(Form, BMP32);
  For X := 1 To Division Do
  Begin
    Opacite := Abs($FF - Round(Max(Min($FF / Division * X, $FF), 0)));
    UpDateForm(Form, BMP32, Opacite);
    Sleep(Delay Div Division);
  End;
  UpDateForm(Form, BMP32, 0);
End;

end.
