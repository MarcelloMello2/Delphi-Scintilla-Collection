procedure PreMultiplyBitmap(Bitmap: TBitmap);
var
  X, Y: Integer;
  Pixel: PRGBQuad;
begin
  Assert(Bitmap.PixelFormat = pf32Bit);
  with Bitmap do
    for Y := Height - 1 downto 0 do
    begin
      Pixel := ScanLine[Y];
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

function CreateSolidBrushWithAlpha(Color: TColor; Alpha: Byte = $FF): HBRUSH;
var
  Info: TBitmapInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  Info.bmiHeader.biSize := SizeOf(Info.bmiHeader);
  Info.bmiHeader.biWidth := 1;
  Info.bmiHeader.biHeight := 1;
  Info.bmiHeader.biPlanes := 1;
  Info.bmiHeader.biBitCount := 32;
  Info.bmiHeader.biCompression := BI_RGB;
  Info.bmiColors[0] := CreatePreMultipliedRGBQuad(Color, Alpha);
  Result := CreateDIBPatternBrushPt(@Info, 0);
end;

function GetAlphaValue(Source, Current: TColor): Byte;
var
  r, g, b, ar, ag, ab, nr, ng, nb, count: Byte;
begin
  r := GetRValue(Source);
  g := GetGValue(Source);
  b := GetBValue(Source);

  ar := GetRValue(Current);
  ag := GetGValue(Current);
  ab := GetBValue(Current);

  nr := r - ar;
  ng := g - ag;
  nb := b - ab;
  count := 0;
  if(nr > 0) then Inc(count);
  if(ng > 0) then Inc(count);
  if(nb > 0) then Inc(count);
  if Count > 0 then
  begin
    Result := Byte(Round((nr + ng + nb)/count) - 1)
  else
    Result := $FF;
end;

procedure SetSolidText(Bitmap: TBitmap; Color: TColor);
var
  X, Y: Integer;
  Pixel: PRGBQuad;
begin
  Assert(Bitmap.PixelFormat = pf32Bit);
  with Bitmap do
    for Y := Pred(Height) downto 0 do
    begin
      Pixel := ScanLine[Y];
      for X := Pred(Width) downto 0 do
      begin
        if Pixel.rgbReserved = $FF then
        begin
          ZeroMemory(Pixel, SizeOf(TRGBQuad));
        end
        else
        begin
          if RGB(Pixel.rgbRed, Pixel.rgbGreen, Pixel.rgbBlue) =
            Cardinal(ColorToRGB(Color)) then
          begin
            Pixel.rgbReserved := $FF;
          end
          else
          begin
            Pixel.rgbReserved := 128;
            Pixel.rgbRed := GetRValue(Color);
            Pixel.rgbGreen := GetGValue(Color);
            Pixel.rgbBlue := GetBValue(Color);
          end;
        end;
        Inc(Pixel);
      end;
    end;
end;