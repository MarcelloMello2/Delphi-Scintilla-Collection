{$X+}
Unit HSLUtils;
Interface
Uses SysUtils, Windows, Graphics, Math;

Const
  MaxHSL: integer = 240;

Type
  TRGB = Record R, G, B: Byte; End;
  LRec = Record Lo, Hi: Word; End;
  WRec = Record Lo, Hi: Byte; End;

Procedure RGBtoHSLRange(RGB: TColor; Var H, S, L: integer);
Function HSLRangeToRGB(H, S, L: integer): TColor;
Function GetRGB(Col: Longint): TRGB;
Function SetRGB(R, G, B: Byte): Longint;


{----------------------------------------------------------------}
{                       }Implementation{                         }
{----------------------------------------------------------------}


{----------------------------------------------------------------}
Function GetRGB(Col: Longint): TRGB;
Begin
  Result.R := 0;
  Result.G := 0;
  Result.B := 0;
  Try
    Result.B := WRec(LRec(Col).Hi).Lo;
    Result.G := WRec(LRec(Col).Lo).Hi;
    Result.R := WRec(LRec(Col).Lo).Lo;
  Except
  End;
End;


{----------------------------------------------------------------}
Function SetRGB(R, G, B: Byte): Longint;
Begin
  Result := 0;
  Try
    Result := (B * $10000) + (G * $100) + R;
  Except
  End;
End;


{----------------------------------------------------------------}
Function HSLtoRGB(H, S, L: double): TColor;
Var
  M1, M2, V: double;
  R, G, B: byte;

  Function HueToColourValue(Hue: double): byte;
  Begin
    Result := 0;
    Try
      If (Hue < 0) Then
        Hue := Hue + 1
      Else If (Hue > 1) Then
        Hue := Hue - 1;
      If (6 * Hue < 1) Then
        V := M1 + (M2 - M1) * Hue * 6
      Else If (2 * Hue < 1) Then
        V := M2
      Else If (3 * Hue < 2) Then
        V := M1 + (M2 - M1) * (2 / 3 - Hue) * 6
      Else
        V := M1;
      Result := round(255 * V);
    Except
    End;
  End;

Begin
  Result := 0;
  Try
    If (S = 0) Then
    Begin
      R := byte(Round(255 * L));
      G := R;
      B := R;
    End
    Else
    Begin
      If (L <= 0.5) Then
        M2 := L * (1 + S)
      Else
        M2 := L + S - L * S;
      M1 := 2 * L - M2;
      R := HueToColourValue(H + 1 / 3);
      G := HueToColourValue(H);
      B := HueToColourValue(H - 1 / 3);
    End;
    Result := SetRGB(R, G, B);
  Except
  End;
End;


{----------------------------------------------------------------}
Procedure RGBtoHSL(Col: TColor; Var H, S, L: double);
Var
  R, G, B, D, Cmax, Cmin: double;
  rgb: TRGB;
Begin
  rgb := GetRGB(Col);
  R := rgb.R / 255;
  G := rgb.G / 255;
  B := rgb.B / 255;
  Cmax := Max(R, Max(G, B));
  Cmin := Min(R, Min(G, B));
  L := (Cmax + Cmin) / 2;
  If (Cmax = Cmin) Then
  Begin
    H := 0;
    S := 0;
  End
  Else
  Begin
    D := Cmax - Cmin;
    If (L < 0.5) Then
      S := D / (Cmax + Cmin)
    Else
      S := D / (2 - Cmax - Cmin);
    If (R = Cmax) Then
      H := (G - B) / D
    Else If (G = Cmax) Then
      H := 2 + (B - R) / D
    Else
      H := 4 + (R - G) / D;
    H := H / 6;
    If (H < 0) Then
      H := H + 1;
  End;
End;


{----------------------------------------------------------------}
Function HSLRangeToRGB(H, S, L: integer): TColor;
Begin
  Result := 0;
  Try
    Result := HSLToRGB(H / MaxHSL, S / MaxHSL, L / MaxHSL);
  Except
  End;
End;


{----------------------------------------------------------------}
Procedure RGBtoHSLRange(RGB: TColor; Var H, S, L: integer);
Var
  Hd, Sd, Ld: double;
Begin
  RGBtoHSL(RGB, Hd, Sd, Ld);
  H := round(Hd * MaxHSL);
  S := round(Sd * MaxHSL);
  L := round(Ld * MaxHSL);
End;
     

{----------------------------------------------------------------}
End.


