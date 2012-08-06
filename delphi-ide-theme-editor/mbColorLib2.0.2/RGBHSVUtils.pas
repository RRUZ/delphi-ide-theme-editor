unit RGBHSVUtils;

interface

uses
 Windows, SysUtils, Classes, Graphics, Math, Scanlines;

procedure Clamp(var Input: integer; Min, Max: integer);
function RGBtoRGBTriple(R, G, B: byte): TRGBTriple;
function RGBtoRGBQuad(R, G, B: byte): TRGBQuad;
function RGBTripleToColor(Triple: TRGBTriple): TColor;
procedure RGBToHSV(R,G,B: integer; var H,S,V: integer);
function HSVtoRGBTriple(H, S, V: integer): TRGBTriple;
function HSVtoRGBQuad(H, S, V: integer): TRGBQuad;
function HSVtoColor(H, S, V: integer): TColor;
function GetHValue(Color: TColor): integer;
function GetVValue(Color: TColor): integer;
function GetSValue(Color: TColor): integer;

implementation

procedure Clamp(var Input: integer; Min, Max: integer);
begin
 if Input < Min then Input := Min;
 if Input > Max then Input := Max;
end;

function RGBtoRGBTriple(R, G, B: byte): TRGBTriple;
begin
 with Result do
  begin
   rgbtRed := R;
   rgbtGreen := G;
   rgbtBlue := B;
  end
end;

function RGBtoRGBQuad(R, G, B: byte): TRGBQuad;
begin
 with Result do
  begin
   rgbRed := R;
   rgbGreen := G;
   rgbBlue := B;
   rgbReserved := 0;
  end
end;

function RGBTripleToColor(Triple: TRGBTriple): TColor;
begin
 Result := TColor(RGB(Triple.rgbtRed, Triple.rgbtGreen, Triple.rgbtBlue));
end;

procedure RGBToHSV(R, G, B: integer; var H, S, V: integer);
var
 Delta, Min, H1, S1: real;
begin
 h1 := h;
 //s1 := s;
 Min := MinIntValue([R, G, B]);
 V := MaxIntValue([R, G, B]);
 Delta := V - Min;
 if V =  0.0 then S1 := 0 else S1 := Delta / V;
 if S1  = 0.0 then
  H1 := 0
 else
  begin
   if R = V then
    H1 := 60.0 * (G - B) / Delta
   else
    if G = V then
     H1 := 120.0 + 60.0 * (B - R) / Delta
    else
     if B = V then
      H1 := 240.0 + 60.0 * (R - G) / Delta;
   if H1 < 0.0 then H1 := H1 + 360.0;
  end;
 h := round(h1);
 s := round(s1*255);
end;

function HSVtoRGBTriple(H, S, V: integer): TRGBTriple;
const
 divisor: integer = 255*60;
var
 f, hTemp, p, q, t, VS: integer;
begin
 if H > 360 then H := H - 360;
 if H < 0 then H := H + 360;
 if s = 0 then
  Result := RGBtoRGBTriple(V, V, V)
 else
  begin
   if H = 360 then hTemp := 0 else hTemp := H;
   f := hTemp mod 60;
   hTemp := hTemp div 60;
   VS := V*S;
   p := V - VS div 255;
   q := V - (VS*f) div divisor;
   t := V - (VS*(60 - f)) div divisor;
   case hTemp of
    0: Result := RGBtoRGBTriple(V, t, p);
    1: Result := RGBtoRGBTriple(q, V, p);
    2: Result := RGBtoRGBTriple(p, V, t);
    3: Result := RGBtoRGBTriple(p, q, V);
    4: Result := RGBtoRGBTriple(t, p, V);
    5: Result := RGBtoRGBTriple(V, p, q);
   else Result := RGBtoRGBTriple(0,0,0)
   end;
  end;
end;

function HSVtoRGBQuad(H, S, V: integer): TRGBQuad;
const
 divisor: integer = 255*60;
var
 f, hTemp, p, q, t, VS: integer;
begin
 if H > 360 then H := H - 360;
 if H < 0 then H := H + 360;
 if s = 0 then
  Result := RGBtoRGBQuad(V, V, V)
 else
  begin
   if H = 360 then hTemp := 0 else hTemp := H;
   f := hTemp mod 60;
   hTemp := hTemp div 60;
   VS := V*S;
   p := V - VS div 255;
   q := V - (VS*f) div divisor;
   t := V - (VS*(60 - f)) div divisor;
   case hTemp of
    0: Result := RGBtoRGBQuad(V, t, p);
    1: Result := RGBtoRGBQuad(q, V, p);
    2: Result := RGBtoRGBQuad(p, V, t);
    3: Result := RGBtoRGBQuad(p, q, V);
    4: Result := RGBtoRGBQuad(t, p, V);
    5: Result := RGBtoRGBQuad(V, p, q);
   else Result := RGBtoRGBQuad(0,0,0)
   end;
  end;
end;

function HSVtoColor(H, S, V: integer): TColor;
begin
 Result := RGBTripleToColor(HSVtoRGBTriple(H, S, V));
end;

function GetHValue(Color: TColor): integer;
var
 s, v: integer;
begin
 RGBToHSV(GetRValue(Color), GetGValue(Color), GetBValue(Color), Result, s, v);
end;

function GetSValue(Color: TColor): integer;
var
 h, v: integer;
begin
 RGBToHSV(GetRValue(Color), GetGValue(Color), GetBValue(Color), h, Result, v);
end;

function GetVValue(Color: TColor): integer;
var
 h, s: integer;
begin
 RGBToHSV(GetRValue(Color), GetGValue(Color), GetBValue(Color), h, s, Result);
end;

end.
