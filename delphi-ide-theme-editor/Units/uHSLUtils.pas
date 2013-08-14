{**************************************************************************************************}
{                                                                                                  }
{ Unit uHSLUtils                                                                                   }
{ unit uHSLUtils  for the Delphi IDE Theme Editor                                                  }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is uHSLUtils.pas.                                                              }
{                                                                                                  }
{ The Initial Developer of the Original Code is Rodrigo Ruz V.                                     }
{ Portions created by Rodrigo Ruz V. are Copyright (C) 2011-2013 Rodrigo Ruz V.                    }
{ All Rights Reserved.                                                                             }
{                                                                                                  }
{**************************************************************************************************}


unit uHSLUtils;

interface

uses
  Windows, Graphics, SysUtils, Generics.Defaults, Generics.Collections;

type
  TColorList = TList<TColor>;
  PRGB24     = ^TRGB24;

  TRGB24 = record
    B, G, R: byte;
  end;
  PRGBArray = ^TRGBArray;
  TRGBArray = array[0..0] of TRGB24;

const
  MaxHue = 180;
  MinHue = -180;
  DefHue = 0;

  MaxSat = 100;
  MinSat = 0;
  DefSat = 0;

  MaxLig = 255;
  MinLig = -255;
  DefLig = 0;


function  _HSLtoRGB(HueValue, SaturationValue, LightValue: Double): TColor;
procedure _RGBtoHSL(RGB: TColor; var HueValue, SaturationValue, LightValue: Double);
procedure _Hue(var ABitMap: TBitmap; Value: integer); overload;
procedure _Hue(var Colors: TColorList; Value: integer); overload;
procedure _Lightness(var ABitMap: TBitmap; Value: integer); overload;
procedure _Lightness(var Colors: TColorList; Value: integer); overload;
procedure _Darkness(var ABitMap: TBitmap; Value: integer); overload;
procedure _Darkness(var Colors: TColorList; Value: integer); overload;
procedure _Saturation(var ABitMap: TBitmap; Value: integer); overload;
procedure _Saturation(var Colors: TColorList; Value: integer); overload;

implementation

function RoundIntToByte(i: integer): byte;
begin
  if i > 255 then Result := 255
  else
  if i < 0   then Result := 0
  else
    Result := i;
end;


procedure GetRGB(Col: TColor; var R, G, B: byte);
var
  Color: $0..$FFFFFFFF;
begin
  Color := ColorToRGB(Col);
  R     := ($000000FF and Color);
  G     := ($0000FF00 and Color) shr 8;
  B     := ($00FF0000 and Color) shr 16;
end;

procedure _Hue(var ABitMap: TBitmap; Value: integer);
var
  r, g, b: byte;
  x, y:    integer;
  ARGB:    TColor;
  Line, Delta: integer;
  H, S, L: double;
begin
  Line  := integer(ABitMap.ScanLine[0]);
  Delta := integer(ABitMap.ScanLine[1]) - Line;
  for y := 0 to ABitMap.Height - 1 do
  begin
    for x := 0 to ABitMap.Width - 1 do
    begin
      r    := PRGBArray(Line)[x].R;
      g    := PRGBArray(Line)[x].G;
      b    := PRGBArray(Line)[x].B;
      ARGB := RGB(r, g, b);
      _RGBtoHSL(ARGB, H, S, L);
      H    := H + Value / 360;
      ARGB := _HSLtoRGB(H, S, L);
      GetRGB(ARGB, R, G, B);
      PRGBArray(Line)[x].R := r;
      PRGBArray(Line)[x].G := g;
      PRGBArray(Line)[x].B := b;
    end;
    Inc(Line, Delta);
  end;
end;

procedure _Hue(var Colors: TColorList; Value: integer);
var
  H, S, L: double;
  i: integer;
begin
  for i := 0 to Colors.Count - 1 do
  begin
    _RGBtoHSL(Colors[i], H, S, L);
    H := H + Value / 360;
    Colors[i] := _HSLtoRGB(H, S, L);
  end;
end;

procedure _Saturation(var ABitMap: TBitmap; Value: integer);
var
  Gray, r, g, b, x, y: integer;
  Line, Delta: integer;
begin
  Line  := integer(ABitMap.ScanLine[0]);
  Delta := integer(ABitMap.ScanLine[1]) - Line;
  for y := 0 to ABitMap.Height - 1 do
  begin
    for x := 0 to ABitMap.Width - 1 do
    begin
      r    := PRGBArray(Line)[x].R;
      g    := PRGBArray(Line)[x].G;
      b    := PRGBArray(Line)[x].B;
      Gray := (r + g + b) div 3;
      PRGBArray(Line)[x].R := RoundIntToByte(Gray + (((r - Gray) * Value) div 255));
      PRGBArray(Line)[x].G := RoundIntToByte(Gray + (((g - Gray) * Value) div 255));
      PRGBArray(Line)[x].B := RoundIntToByte(Gray + (((b - Gray) * Value) div 255));
    end;
    Inc(Line, Delta);
  end;
end;

procedure _Saturation(var Colors: TColorList; Value: integer);
var
  i:    integer;
  r, g, b: byte;
  Gray: integer;
begin
  for i := 0 to Colors.Count - 1 do
  begin
    GetRGB(Colors[i], r, g, b);
    Gray := (r + g + b) div 3;
    R    := RoundIntToByte(Gray + (((r - Gray) * Value) div 255));
    G    := RoundIntToByte(Gray + (((g - Gray) * Value) div 255));
    B    := RoundIntToByte(Gray + (((b - Gray) * Value) div 255));
    Colors[i] := RGB(r, g, b);
  end;
end;


procedure _Lightness(var ABitMap: TBitmap; Value: integer);
var
  r, g, b, x, y: integer;
  Line, Delta:   integer;
begin
  Line  := integer(ABitMap.ScanLine[0]);
  Delta := integer(ABitMap.ScanLine[1]) - Line;
  for y := 0 to ABitMap.Height - 1 do
  begin
    for x := 0 to ABitMap.Width - 1 do
    begin
      r := PRGBArray(Line)[x].R;
      g := PRGBArray(Line)[x].G;
      b := PRGBArray(Line)[x].B;
      PRGBArray(Line)[x].R := RoundIntToByte(r + ((255 - r) * Value) div 255);
      PRGBArray(Line)[x].G := RoundIntToByte(g + ((255 - g) * Value) div 255);
      PRGBArray(Line)[x].B := RoundIntToByte(b + ((255 - b) * Value) div 255);
    end;
    Inc(Line, Delta);
  end;
end;

procedure _Lightness(var Colors: TColorList; Value: integer);
var
  i: integer;
  r, g, b: byte;
begin
  for i := 0 to Colors.Count - 1 do
  begin
    GetRGB(Colors[i], r, g, b);
    R := RoundIntToByte(r + ((255 - r) * Value) div 255);
    G := RoundIntToByte(g + ((255 - g) * Value) div 255);
    B := RoundIntToByte(b + ((255 - b) * Value) div 255);
    Colors[i] := RGB(r, g, b);
  end;
end;


procedure _Darkness(var ABitMap: TBitmap; Value: integer);
var
  r, g, b, x, y: integer;
  Line, Delta:   integer;
begin
  Line  := integer(ABitMap.ScanLine[0]);
  Delta := integer(ABitMap.ScanLine[1]) - Line;
  for y := 0 to ABitMap.Height - 1 do
  begin
    for x := 0 to ABitMap.Width - 1 do
    begin
      r := PRGBArray(Line)[x].R;
      g := PRGBArray(Line)[x].G;
      b := PRGBArray(Line)[x].B;
      PRGBArray(Line)[x].R := RoundIntToByte(r - ((r) * Value) div 255);
      PRGBArray(Line)[x].G := RoundIntToByte(g - ((g) * Value) div 255);
      PRGBArray(Line)[x].B := RoundIntToByte(b - ((b) * Value) div 255);
    end;
    Inc(Line, Delta);
  end;
end;

procedure _Darkness(var Colors: TColorList; Value: integer);
var
  i: integer;
  r, g, b: byte;
begin
  for i := 0 to Colors.Count - 1 do
  begin
    GetRGB(Colors[i], r, g, b);
    R := RoundIntToByte(r - ((r) * Value) div 255);
    G := RoundIntToByte(g - ((g) * Value) div 255);
    B := RoundIntToByte(b - ((b) * Value) div 255);
    Colors[i] := RGB(r, g, b);
  end;
end;

function _HSLtoRGB(HueValue, SaturationValue, LightValue: double): TColor;
var
  M1, M2: double;

  function HueToColourValue(Hue: double): byte;
  var
    V: double;
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
      V := M1 + (M2 - M1) * (2 / 3 - Hue) * 6
    else
      V := M1;
    Result := round(255 * V);
  end;

var
  R, G, B: byte;
begin
  if SaturationValue = 0 then
  begin
    R := round(255 * LightValue);
    G := R;
    B := R;
  end
  else
  begin
    if LightValue <= 0.5 then
      M2 := LightValue * (1 + SaturationValue)
    else
      M2 := LightValue + SaturationValue - LightValue * SaturationValue;
    M1 := 2 * LightValue - M2;
    R := HueToColourValue(HueValue + 1 / 3);
    G := HueToColourValue(HueValue);
    B := HueToColourValue(HueValue - 1 / 3);
  end;

  Result := RGB(R, G, B);
end;

procedure _RGBtoHSL(RGB: TColor; var HueValue, SaturationValue, LightValue: double);

  function Max(a, b: double): double;
  begin
    if a > b then
      Result := a
    else
      Result := b;
  end;

  function Min(a, b: double): double;
  begin
    if a < b then
      Result := a
    else
      Result := b;
  end;

var
  R, G, B, D, Cmax, Cmin: double;
begin
  R    := GetRValue(RGB) / 255;
  G    := GetGValue(RGB) / 255;
  B    := GetBValue(RGB) / 255;
  Cmax := Max(R, Max(G, B));
  Cmin := Min(R, Min(G, B));

  LightValue := (Cmax + Cmin) / 2;

  if Cmax = Cmin then
  begin
    HueValue := 0;
    SaturationValue := 0;
  end
  else
  begin
    D := Cmax - Cmin;

    if LightValue < 0.5 then
      SaturationValue := D / (Cmax + Cmin)
    else
      SaturationValue := D / (2 - Cmax - Cmin);

    if R = Cmax then
      HueValue := (G - B) / D
    else
    if G = Cmax then
      HueValue := 2 + (B - R) / D
    else
      HueValue := 4 + (R - G) / D;

    HueValue := HueValue / 6;
    if HueValue < 0 then
      HueValue := HueValue + 1;
  end;
end;

end.
