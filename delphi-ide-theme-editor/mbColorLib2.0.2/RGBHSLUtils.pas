unit RGBHSLUtils;

interface

uses
 Windows, Graphics, Math, Scanlines;

var //set these variables to your needs, e.g. 360, 255, 255
 MaxHue: integer = 239;
 MaxSat: integer = 240;
 MaxLum: integer = 240;

function HSLtoRGB (H, S, L: double): TColor;
function HSLRangeToRGB (H, S, L: integer): TColor;
procedure RGBtoHSLRange (RGB: TColor; var H1, S1, L1 : integer);
function GetHValue(AColor: TColor): integer;
function GetSValue(AColor: TColor): integer;
function GetLValue(AColor: TColor): integer;
procedure Clamp(var Input: integer; Min, Max: integer);
function HSLToRGBTriple(H, S, L : integer) : TRGBTriple;
function HSLToRGBQuad(H, S, L: integer): TRGBQuad;
procedure RGBTripleToHSL(RGBTriple : TRGBTriple; var h, s, l: integer);

implementation

function HSLtoRGB(H, S, L: double): TColor;
var
 M1, M2: double;

  function HueToColorValue(Hue: double): byte;
  var
   V : double;
  begin
   if Hue < 0 then
    Hue := Hue + 1
   else
    if Hue > 1 then
     Hue := Hue - 1;
   if 6 * Hue < 1 then
    V := M1 + (M2 - M1) * Hue * 6
   else
    if 2 * Hue < 1 then
     V := M2
    else
     if 3 * Hue < 2 then
      V := M1 + (M2 - M1) * (2/3 - Hue) * 6
     else
      V := M1;
   Result := round (255 * V)
  end;

var
 R, G, B: byte;
begin
 if S = 0 then
  begin
   R := round (MaxLum * L);
   G := R;
   B := R
  end
 else
  begin
   if L <= 0.5 then
    M2 := L * (1 + S)
   else
    M2 := L + S - L * S;
   M1 := 2 * L - M2;
   R := HueToColorValue (H + 1/3);
   G := HueToColorValue (H);
   B := HueToColorValue (H - 1/3)
  end;
 Result := RGB (R, G, B)
end;

function HSLRangeToRGB(H, S, L : integer): TColor;
begin
 if s > MaxSat then s := MaxSat;
 if s < 0 then s := 0;
 if l > MaxLum then l := MaxLum;
 if l < 0 then l := 0;
 Result := HSLToRGB(H / MaxHue, S / MaxSat, L / MaxLum);
end;

procedure RGBtoHSLRange(RGB: TColor; var H1, S1, L1 : integer);
var
  R, G, B, D, Cmax, Cmin, h, s, l: double;
begin
 //H := h1;
 //S := s1;
 //L := l1;
 R := GetRValue (RGB) / 255;
 G := GetGValue (RGB) / 255;
 B := GetBValue (RGB) / 255;
 Cmax := Max (R, Max (G, B));
 Cmin := Min (R, Min (G, B));
 L := (Cmax + Cmin) / 2;
 if Cmax = Cmin then
  begin
   H := 0;
   S := 0;
  end
 else
  begin
   D := Cmax - Cmin;
   //calc L
   if L < 0.5 then
    S := D / (Cmax + Cmin)
   else
    S := D / (2 - Cmax - Cmin);
   //calc H
   if R = Cmax then
    H := (G - B) / D
   else
    if G = Cmax then
     H  := 2 + (B - R) /D
    else
     H := 4 + (R - G) / D;
   H := H / 6;
   if H < 0 then
    H := H + 1;
  end;
 H1 := round (H * MaxHue);
 S1 := round (S * MaxSat);
 L1 := round (L * MaxLum);
end;

function GetHValue(AColor: TColor): integer;
var
 d, h: integer;
begin
 RGBToHSLRange(AColor, h, d, d);
 Result := h;
end;

function GetSValue(AColor: TColor): integer;
var
 d, s: integer;
begin
 RGBToHSLRange(AColor, d, s, d);
 Result := s;
end;

function GetLValue(AColor: TColor): integer;
var
 d, l: integer;
begin
 RGBToHSLRange(AColor, d, d, l);
 Result := l;
end;

procedure Clamp(var Input: integer; Min, Max: integer);
begin
 if (Input < Min) then Input := Min;
 if (Input > Max) then Input := Max;
end;

function HSLToRGBTriple(H, S, L: integer): TRGBTriple;
const
 Divisor = 255*60;
var
 hTemp, f, LS, p, q, r: integer;
begin
 Clamp(H, 0, MaxHue);
 Clamp(S, 0, MaxSat);
 Clamp(L, 0, MaxLum);
 if (S = 0) then
   Result := RGBToRGBTriple(L, L, L)
 else
  begin
   hTemp := H mod MaxHue;
   f := hTemp mod 60;
   hTemp := hTemp div 60;
   LS := L*S;
   p := L - LS div MaxLum;
   q := L - (LS*f) div Divisor;
   r := L - (LS*(60 - f)) div Divisor;
   case hTemp of
    0: Result := RGBToRGBTriple(L, r, p);
    1: Result := RGBToRGBTriple(q, L, p);
    2: Result := RGBToRGBTriple(p, L, r);
    3: Result := RGBToRGBTriple(p, q, L);
    4: Result := RGBToRGBTriple(r, p, L);
    5: Result := RGBToRGBTriple(L, p, q);
   else
    Result  := RGBToRGBTriple(0, 0, 0);
   end;
  end;
end;

function HSLToRGBQuad(H, S, L: integer): TRGBQuad;
const
 Divisor = 255*60;
var
 hTemp, f, LS, p, q, r: integer;
begin
 Clamp(H, 0, MaxHue);
 Clamp(S, 0, MaxSat);
 Clamp(L, 0, MaxLum);
 if (S = 0) then
   Result := RGBToRGBQuad(L, L, L)
 else
  begin
   hTemp := H mod MaxHue;
   f := hTemp mod 60;
   hTemp := hTemp div 60;
   LS := L*S;
   p := L - LS div MaxLum;
   q := L - (LS*f) div Divisor;
   r := L - (LS*(60 - f)) div Divisor;
   case hTemp of
    0: Result := RGBToRGBQuad(L, r, p);
    1: Result := RGBToRGBQuad(q, L, p);
    2: Result := RGBToRGBQuad(p, L, r);
    3: Result := RGBToRGBQuad(p, q, L);
    4: Result := RGBToRGBQuad(r, p, L);
    5: Result := RGBToRGBQuad(L, p, q);
   else
    Result  := RGBToRGBQuad(0, 0, 0);
   end;
  end;
end;

procedure RGBTripleToHSL(RGBTriple: TRGBTriple; var h, s, l: integer);

 function RGBMaxValue(RGB: TRGBTriple): byte;
 begin
  Result  := RGB.rgbtRed;
  if (Result < RGB.rgbtGreen) then Result := RGB.rgbtGreen;
  if (Result < RGB.rgbtBlue) then Result := RGB.rgbtBlue;
 end;

 function RGBMinValue(RGB: TRGBTriple) : byte;
 begin
  Result  := RGB.rgbtRed;
  if (Result > RGB.rgbtGreen) then Result := RGB.rgbtGreen;
  if (Result > RGB.rgbtBlue) then Result := RGB.rgbtBlue;
 end;
var
 Delta, Min: byte;
begin
 L     := RGBMaxValue(RGBTriple);
 Min   := RGBMinValue(RGBTriple);
 Delta := L-Min;
 if (L = Min) then
  begin
   H := 0;
   S := 0;
  end
 else
  begin
   S := MulDiv(Delta, 255, L);
   with RGBTriple do
    begin
     if (rgbtRed = L) then
      H := MulDiv(60, rgbtGreen-rgbtBlue, Delta)
     else
      if (rgbtGreen = L) then
       H := MulDiv(60, rgbtBlue-rgbtRed,   Delta) + 120
      else
       if (rgbtBlue = L) then
        H := MulDiv(60, rgbtRed-rgbtGreen,  Delta) + 240;
     if (H < 0) then H := H + 360;
    end;
  end;
end;

end.
