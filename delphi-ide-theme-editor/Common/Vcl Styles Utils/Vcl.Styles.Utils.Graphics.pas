//**************************************************************************************************
//
// Unit Vcl.Styles.Utils.Graphics
// unit for the VCL Styles Utils
// http://code.google.com/p/vcl-styles-utils/
//
// The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
// you may not use this file except in compliance with the License. You may obtain a copy of the
// License at http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
// ANY KIND, either express or implied. See the License for the specific language governing rights
// and limitations under the License.
//
// The Original Code is Vcl.Styles.Utils.Graphics.pas.
//
// The Initial Developer of the Original Code is Rodrigo Ruz V.
// Portions created by Rodrigo Ruz V. are Copyright (C) 2012-2014 Rodrigo Ruz V.
// All Rights Reserved.
//
//**************************************************************************************************


unit Vcl.Styles.Utils.Graphics;

interface

uses
  System.Classes,
  System.SysUtils,
  Winapi.Windows,
  Vcl.GraphUtil,
  Vcl.Graphics;

const
  MaxHue = 180;
  MinHue = -180;
  DefHue = 0;

  MaxSat = 255;
  MinSat = 0;
  DefSat = 0;

  MaxLig = 255;
  MinLig = -255;
  DefLig = 0;


function  _HSLtoRGB(HueValue, SaturationValue, LightValue: Double): TColor;
procedure _RGBtoHSL(RGB: TColor; var HueValue, SaturationValue, LightValue: Double);


procedure _Hue(const AColor: TColor;Value: Integer; out NewColor:TColor);
procedure _Hue24(var ABitMap: TBitmap; Value: integer);
procedure _Hue32(const ABitMap: TBitmap; Value: integer);

procedure _Sepia(const AColor: TColor;Value: Integer; out NewColor:TColor);
procedure _Sepia24(const ABitMap: TBitmap;Value : Byte=32);
procedure _Sepia32(const ABitMap: TBitmap;Value : Byte=32);

procedure _BlendMultiply(const AColor: TColor;Value: Integer; out NewColor:TColor);
procedure _BlendMultiply24(const ABitMap: TBitmap;Value: Integer);
procedure _BlendMultiply32(const ABitMap: TBitmap;Value: Integer);


procedure _Lightness(const AColor: TColor;Value: Integer; out NewColor:TColor);
procedure _Lightness24(var ABitMap: TBitmap; Value: integer);
procedure _Lightness32(const ABitMap: TBitmap; Value: integer);


procedure _Darkness(const AColor: TColor;Value: Integer; out NewColor:TColor);
procedure _Darkness24(var ABitMap: TBitmap; Value: integer);
procedure _Darkness32(const ABitMap: TBitmap; Value: integer);

procedure _Saturation(const AColor: TColor;Value: Integer; out NewColor:TColor);
procedure _Saturation24(var ABitMap: TBitmap; Value: integer);
procedure _Saturation32(const ABitMap: TBitmap; Value: integer);


procedure _SetRComponent(const AColor: TColor;Value: Integer; out NewColor:TColor);
procedure _SetGComponent(const AColor: TColor;Value: Integer; out NewColor:TColor);
procedure _SetBComponent(const AColor: TColor;Value: Integer; out NewColor:TColor);

procedure _SetRGB24(const ABitMap: TBitmap; DR,DG,DB: Integer);
procedure _SetRGB32(const ABitMap: TBitmap; DR,DG,DB: Integer);

procedure _BlendBurn(const AColor: TColor;Value: Integer; out NewColor:TColor);
procedure _BlendBurn24(const ABitMap: TBitmap;Value: Integer);
procedure _BlendBurn32(const ABitMap: TBitmap;Value: Integer);


procedure _BlendAdditive(const AColor: TColor;Value: Integer; out NewColor:TColor);
procedure _BlendAdditive24(const ABitMap: TBitmap;Value: Integer);
procedure _BlendAdditive32(const ABitMap: TBitmap;Value: Integer);


procedure _BlendDodge(const AColor: TColor;Value: Integer; out NewColor:TColor);
procedure _BlendDodge24(const ABitMap: TBitmap;Value: Integer);
procedure _BlendDodge32(const ABitMap: TBitmap;Value: Integer);


procedure _BlendOverlay(const AColor: TColor;Value: Integer; out NewColor:TColor);
procedure _BlendOverlay24(const ABitMap: TBitmap;Value: Integer);
procedure _BlendOverlay32(const ABitMap: TBitmap;Value: Integer);

procedure _BlendDifference(const AColor: TColor;Value: Integer; out NewColor:TColor);
procedure _BlendDifference24(const ABitMap: TBitmap;Value: Integer);
procedure _BlendDifference32(const ABitMap: TBitmap;Value: Integer);

procedure _BlendLighten(const AColor: TColor;Value: Integer; out NewColor:TColor);
procedure _BlendLighten24(const ABitMap: TBitmap;Value: Integer);
procedure _BlendLighten32(const ABitMap: TBitmap;Value: Integer);


procedure _BlendDarken(const AColor: TColor;Value: Integer; out NewColor:TColor);
procedure _BlendDarken24(const ABitMap: TBitmap;Value: Integer);
procedure _BlendDarken32(const ABitMap: TBitmap;Value: Integer);

procedure _BlendScreen(const AColor: TColor;Value: Integer; out NewColor:TColor);
procedure _BlendScreen24(const ABitMap: TBitmap;Value: Integer);
procedure _BlendScreen32(const ABitMap: TBitmap;Value: Integer);


Type
  TColorFilter=class
  private
   FColorValue : Integer;
  public
   constructor Create(AColorValue:Integer);
   property ColorValue : Integer read FColorValue Write FColorValue;
   function ProcessColor(AColor: TColor):TColor;virtual;abstract;
  end;

  TBitmapFilter=class(TColorFilter)
  private
   //FColorValue   : Integer;
   FUseBitmap: Boolean;
   FSourceBitmap : TBitmap;
  public
   constructor Create(AColorValue:Integer);
   constructor CreateBitMap(ASourceBitmap :TBitmap);
   procedure ProcessBitmap(ABitMap: TBitmap);virtual;abstract;
  end;

  TBitmap32HueFilter=class(TBitmapFilter)
  public
   procedure ProcessBitmap(ABitMap: TBitmap);override;
   function  ProcessColor(AColor: TColor):TColor;override;
  end;

  TBitmap32SaturationFilter=class(TBitmapFilter)
  public
   procedure ProcessBitmap(ABitMap: TBitmap);override;
   function  ProcessColor(AColor: TColor):TColor;override;
  end;

  TBitmap32LightnessFilter=class(TBitmapFilter)
  public
   procedure ProcessBitmap(ABitMap: TBitmap);override;
   function  ProcessColor(AColor: TColor):TColor;override;
  end;

  TBitmap32SepiaFilter=class(TBitmapFilter)
  public
   procedure ProcessBitmap(ABitMap: TBitmap);override;
   function  ProcessColor(AColor: TColor):TColor;override;
  end;

  TBitmap32RedFilter=class(TBitmapFilter)
  public
   procedure ProcessBitmap(ABitMap: TBitmap);override;
   function  ProcessColor(AColor: TColor):TColor;override;
  end;

  TBitmap32GreenFilter=class(TBitmapFilter)
  public
   procedure ProcessBitmap(ABitMap: TBitmap);override;
   function  ProcessColor(AColor: TColor):TColor;override;
  end;

  TBitmap32BlueFilter=class(TBitmapFilter)
  public
   procedure ProcessBitmap(ABitMap: TBitmap);override;
   function  ProcessColor(AColor: TColor):TColor;override;
  end;

  TBitmap32BlendBurn=class(TBitmapFilter)
  public
   procedure ProcessBitmap(ABitMap: TBitmap);override;
   function  ProcessColor(AColor: TColor):TColor;override;
  end;

  TBitmap32BlendMultiply=class(TBitmapFilter)
  public
   procedure ProcessBitmap(ABitMap: TBitmap);override;
   function  ProcessColor(AColor: TColor):TColor;override;
  end;


  TBitmap32BlendAdditive=class(TBitmapFilter)
  public
   procedure ProcessBitmap(ABitMap: TBitmap);override;
   function  ProcessColor(AColor: TColor):TColor;override;
  end;

  TBitmap32BlendDodge=class(TBitmapFilter)
  public
   procedure ProcessBitmap(ABitMap: TBitmap);override;
   function  ProcessColor(AColor: TColor):TColor;override;
  end;

  TBitmap32BlendOverlay=class(TBitmapFilter)
  public
   procedure ProcessBitmap(ABitMap: TBitmap);override;
   function  ProcessColor(AColor: TColor):TColor;override;
  end;

  TBitmap32BlendDifference=class(TBitmapFilter)
  public
   procedure ProcessBitmap(ABitMap: TBitmap);override;
   function  ProcessColor(AColor: TColor):TColor;override;
  end;

  TBitmap32BlendLighten=class(TBitmapFilter)
  public
   procedure ProcessBitmap(ABitMap: TBitmap);override;
   function  ProcessColor(AColor: TColor):TColor;override;
  end;

  TBitmap32BlendDarken=class(TBitmapFilter)
  public
   procedure ProcessBitmap(ABitMap: TBitmap);override;
   function  ProcessColor(AColor: TColor):TColor;override;
  end;

  TBitmap32BlendScreen=class(TBitmapFilter)
  public
   procedure ProcessBitmap(ABitMap: TBitmap);override;
   function  ProcessColor(AColor: TColor):TColor;override;
  end;

  procedure GradientRoundedFillCanvas(const ACanvas: TCanvas;
  const AStartColor, AEndColor: TColor; const ARect: TRect;
  const Direction: TGradientDirection; Radius : Integer);

  procedure AlphaBlendFillCanvas(const ACanvas: TCanvas;  const AColor : TColor;const ARect: TRect; SourceConstantAlpha : Byte);

implementation

Uses
  Math;

type
  PRGBArray24 = ^TRGBArray24;
  TRGBArray24 = array[0..0] of TRGBTriple;

  PRGBArray32 = ^TRGBArray32;
  TRGBArray32 = array[0..0] of TRGBQuad;

  TFilterCallback  = procedure (const AColor: TColor;Value: Integer; out NewColor:TColor);


procedure GradientRoundedFillCanvas(const ACanvas: TCanvas;
  const AStartColor, AEndColor: TColor; const ARect: TRect;
  const Direction: TGradientDirection; Radius : Integer);
var
  LBuffer : TBitmap;
  LRect : TRect;
  LRgn : THandle;
  LPoint : TPoint;
begin
  LBuffer:=TBitmap.Create;
  try
    LBuffer.Width:=1;
    LBuffer.Height:=ARect.Height;
    LRect.Create(0, 0, 1, ARect.Height);
    GradientFillCanvas(LBuffer.Canvas, AStartColor, AEndColor, LRect, Direction);

    LRgn := CreateRoundRectRgn(ARect.Left, ARect.Top, ARect.Left +  ARect.Width,  ARect.Top + ARect.Height, Radius, Radius);
    if LRgn>0 then
    try
      GetWindowOrgEx(ACanvas.Handle, LPoint);
      OffsetRgn(LRgn, -LPoint.X, -LPoint.Y);
      SelectClipRgn(ACanvas.Handle, LRgn);
      ACanvas.StretchDraw(Rect(ARect.Left,  ARect.Top, ARect.Left + ARect.Width,  ARect.Top + ARect.Height), LBuffer);
      SelectClipRgn(ACanvas.Handle, 0);
    finally
      DeleteObject(LRgn);
    end;
  finally
   LBuffer.Free;
  end;
end;


procedure AlphaBlendFillCanvas(const ACanvas: TCanvas;  const AColor : TColor;const ARect: TRect; SourceConstantAlpha : Byte);
var
 LBuffer   : TBitmap;
 LBlendFunc: TBlendFunction;
begin
  LBuffer := TBitmap.Create;
  try
    LBuffer.Width := ARect.Width;
    LBuffer.Height := ARect.Height;
    LBuffer.Canvas.Brush.Color := AColor;
    LBuffer.Canvas.FillRect(Rect(0, 0, ARect.Width, ARect.Height));
    ZeroMemory(@LBlendFunc, SizeOf(LBlendFunc));
    LBlendFunc.BlendOp := AC_SRC_OVER;
    LBlendFunc.BlendFlags := 0;
    LBlendFunc.SourceConstantAlpha := SourceConstantAlpha;
    LBlendFunc.AlphaFormat := 0;
    AlphaBlend(ACanvas.Handle, ARect.Left, ARect.Top, LBuffer.Width, LBuffer.Height, LBuffer.Canvas.Handle, 0, 0, LBuffer.Width, LBuffer.Height, LBlendFunc);
  finally
    LBuffer.Free;
  end;
end;

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


procedure _ProcessBitmap32(const Dest: TBitmap;Value: Integer;_Process:TFilterCallback); overload;
var
  r, g, b, a   : byte;
  x, y:    integer;
  ARGB:    TColor;
  Line, Delta: integer;
begin
  Line  := integer(Dest.ScanLine[0]);
  Delta := integer(Dest.ScanLine[1]) - Line;
  for y := 0 to Dest.Height - 1 do
  begin
    for x := 0 to Dest.Width - 1 do
    begin
      {$IFOPT R+}
        {$DEFINE RANGEON}
        {$R-}
      {$ELSE}
        {$UNDEF RANGEON}
      {$ENDIF}
      r    := PRGBArray32(Line)[x].rgbRed;
      g    := PRGBArray32(Line)[x].rgbGreen;
      b    := PRGBArray32(Line)[x].rgbBlue;
      a    := PRGBArray32(Line)[x].rgbReserved;
      {$IFDEF RANGEON}
        {$R+}
        {$UNDEF RANGEON}
      {$ENDIF}
      _Process(RGB(r,g,b), Value, ARGB);
      GetRGB(ARGB, r, g, b);
      {$IFOPT R+}
        {$DEFINE RANGEON}
        {$R-}
      {$ELSE}
        {$UNDEF RANGEON}
      {$ENDIF}
      PRGBArray32(Line)[x].rgbRed := r;
      PRGBArray32(Line)[x].rgbGreen := g;
      PRGBArray32(Line)[x].rgbBlue := b;
      PRGBArray32(Line)[x].rgbReserved := a;
      {$IFDEF RANGEON}
        {$R+}
        {$UNDEF RANGEON}
      {$ENDIF}
    end;
    Inc(Line, Delta);
  end;
end;

procedure _ProcessBitmap32(const Source, Dest: TBitmap;_Process:TFilterCallback); overload;
var
  r, g, b, a   : byte;
  x, y:    integer;
  ARGB:    TColor;
  LineDest, DeltaDest: integer;
  LineSource, DeltaSource: integer;
  Value : TColor;
  SourceN : TBitmap;
begin
  SourceN:=TBitmap.Create;
  try
    SourceN.SetSize(Dest.Width, Dest.Height);
    SourceN.PixelFormat:=pf24bit;

    y := 0;
    while y < Dest.Height do
    begin
      x := 0;
      while x < Dest.Width do
      begin
        SourceN.Canvas.Draw(x, y, Source);
        x := x + Source.Width;
      end;
      y := y + Source.Height;
    end;

    LineDest  := integer(Dest.ScanLine[0]);
    DeltaDest := integer(Dest.ScanLine[1]) - LineDest;

    LineSource  := integer(SourceN.ScanLine[0]);
    DeltaSource := integer(SourceN.ScanLine[1]) - LineSource;

    for y := 0 to Dest.Height - 1 do
    begin
      for x := 0 to Dest.Width - 1 do
      begin
      {$IFOPT R+}
        {$DEFINE RANGEON}
        {$R-}
      {$ELSE}
        {$UNDEF RANGEON}
      {$ENDIF}        r    := PRGBArray32(LineDest)[x].rgbRed;
        g    := PRGBArray32(LineDest)[x].rgbGreen;
        b    := PRGBArray32(LineDest)[x].rgbBlue;
        a    := PRGBArray32(LineDest)[x].rgbReserved;
      {$IFDEF RANGEON}
        {$R+}
        {$UNDEF RANGEON}
      {$ENDIF}

        Value:=RGB(PRGBArray24(LineSource)[x].rgbtRed, PRGBArray24(LineSource)[x].rgbtGreen, PRGBArray24(LineSource)[x].rgbtBlue);


        _Process(RGB(r,g,b), Value, ARGB);
        GetRGB(ARGB, r, g, b);

      {$IFOPT R+}
        {$DEFINE RANGEON}
        {$R-}
      {$ELSE}
        {$UNDEF RANGEON}
      {$ENDIF}
        PRGBArray32(LineDest)[x].rgbRed := r;
        PRGBArray32(LineDest)[x].rgbGreen := g;
        PRGBArray32(LineDest)[x].rgbBlue := b;
        PRGBArray32(LineDest)[x].rgbReserved := a;
      {$IFDEF RANGEON}
        {$R+}
        {$UNDEF RANGEON}
      {$ENDIF}
      end;
      Inc(LineDest, DeltaDest);
      Inc(LineSource, DeltaSource);
    end;
  finally
    SourceN.Free;
  end;
end;



procedure _ProcessBitmap24(const ABitMap: TBitmap;Value: Integer;_Process:TFilterCallback); overload;
var
  r, g, b    : byte;
  x, y:    integer;
  ARGB:    TColor;
  Line, Delta: integer;
begin
  Line  := integer(ABitMap.ScanLine[0]);
  Delta := integer(ABitMap.ScanLine[1]) - Line;
  for y := 0 to ABitMap.Height - 1 do
  begin
    for x := 0 to ABitMap.Width - 1 do
    begin
      {$IFOPT R+}
        {$DEFINE RANGEON}
        {$R-}
      {$ELSE}
        {$UNDEF RANGEON}
      {$ENDIF}
      r    := PRGBArray24(Line)[x].rgbtRed;
      g    := PRGBArray24(Line)[x].rgbtGreen;
      b    := PRGBArray24(Line)[x].rgbtBlue;
      {$IFDEF RANGEON}
        {$R+}
        {$UNDEF RANGEON}
      {$ENDIF}

      _Process(RGB(r,g,b), Value, ARGB);
      GetRGB(ARGB, r, g, b);

      {$IFOPT R+}
        {$DEFINE RANGEON}
        {$R-}
      {$ELSE}
        {$UNDEF RANGEON}
      {$ENDIF}
      PRGBArray24(Line)[x].rgbtRed := r;
      PRGBArray24(Line)[x].rgbtGreen := g;
      PRGBArray24(Line)[x].rgbtBlue := b;
      {$IFDEF RANGEON}
        {$R+}
        {$UNDEF RANGEON}
      {$ENDIF}
    end;
    Inc(Line, Delta);
  end;
end;

procedure _ProcessBitmap24(const Source, Dest: TBitmap;_Process:TFilterCallback); overload;
var
  r, g, b   : byte;
  x, y:    integer;
  ARGB:    TColor;
  LineDest, DeltaDest: integer;
  LineSource, DeltaSource: integer;
  Value : TColor;
  SourceN : TBitmap;
begin
  SourceN:=TBitmap.Create;
  try
    SourceN.SetSize(Dest.Width, Dest.Height);
    SourceN.PixelFormat:=pf24bit;

    y := 0;
    while y < Dest.Height do
    begin
      x := 0;
      while x < Dest.Width do
      begin
        SourceN.Canvas.Draw(x, y, Source);
        x := x + Source.Width;
      end;
      y := y + Source.Height;
    end;

    LineDest  := integer(Dest.ScanLine[0]);
    DeltaDest := integer(Dest.ScanLine[1]) - LineDest;

    LineSource  := integer(SourceN.ScanLine[0]);
    DeltaSource := integer(SourceN.ScanLine[1]) - LineSource;

    for y := 0 to Dest.Height - 1 do
    begin
      for x := 0 to Dest.Width - 1 do
      begin
        r    := PRGBArray24(LineDest)[x].rgbtRed;
        g    := PRGBArray24(LineDest)[x].rgbtGreen;
        b    := PRGBArray24(LineDest)[x].rgbtBlue;

        Value:=RGB(PRGBArray24(LineSource)[x].rgbtRed, PRGBArray24(LineSource)[x].rgbtGreen, PRGBArray24(LineSource)[x].rgbtBlue);

        _Process(RGB(r,g,b), Value, ARGB);
        GetRGB(ARGB, r, g, b);

        PRGBArray32(LineDest)[x].rgbRed := r;
        PRGBArray32(LineDest)[x].rgbGreen := g;
        PRGBArray32(LineDest)[x].rgbBlue := b;
      end;
      Inc(LineDest, DeltaDest);
      Inc(LineSource, DeltaSource);
    end;
  finally
    SourceN.Free;
  end;
end;



procedure _Sepia(const AColor: TColor;Value: Integer; out NewColor:TColor);
var
  ARGB         : TColor;
  r, g, b      : byte;
begin
  GetRGB(AColor, r,g, b);
  ARGB:=(r+g+b) div 3;

  r:=ARGB+(Value*2);
  g:=ARGB+(Value*1);
  b:=ARGB+(Value*1);

  if r <= ((Value*2)-1) then
    r:=255;
  if g <= (Value-1) then
    g:=255;

  NewColor:= RGB(r, g, b);
end;

procedure _Sepia24(const ABitMap: TBitmap;Value : Byte);
begin
  _ProcessBitmap24(ABitMap, Value, _Sepia);
end;

procedure _Sepia32(const ABitMap: TBitmap;Value : Byte);
begin
  _ProcessBitmap32(ABitMap, Value, _Sepia);
end;

procedure _Hue(const AColor: TColor;Value: Integer; out NewColor:TColor);
var
  ARGB         : TColor;
  H, S, L      : double;
begin
  _RGBtoHSL(AColor, H, S, L);
  H    := H + Value / 360;
  ARGB := _HSLtoRGB(H, S, L);
  NewColor:= ARGB;
end;

procedure _Hue24(var ABitMap: TBitmap; Value: integer);
begin
  _ProcessBitmap24(ABitMap, Value, _Hue);
end;

procedure _Hue32(const ABitMap: TBitmap; Value: integer);
begin
  _ProcessBitmap32(ABitMap, Value, _Hue);
end;

{
if b = 0 then
  result := 0
else begin
  c := 255 - (((255-a) SHL 8) DIV b);
  if c < 0 then result := 0 else result := c;
end;
}

procedure _BlendBurn(const AColor: TColor;Value: Integer; out NewColor:TColor);
var
  ARGB         : TColor;
  r, g, b      : byte;
  br, bg, bb   : byte;
  c            : Integer;
begin
  GetRGB(AColor, r,g, b);
  ARGB := Value;
  GetRGB(ARGB, br,bg, bb);

  if br=0 then
   r:=0
  else
  begin
   c:=RoundIntToByte(255-(((255-r) SHL 8) DIV br));
   r:=c;
  end;

  if bg=0 then
   g:=0
  else
  begin
   c:=RoundIntToByte(255-(((255-g) SHL 8) DIV bg));
   g:=c;
  end;

  if bb=0 then
   b:=0
  else
  begin
   c:=RoundIntToByte(255-(((255-b) SHL 8) DIV bb));
   b:=c;
  end;

  NewColor:=RGB(r, g, b);
end;

procedure _BlendBurn24(const ABitMap: TBitmap;Value: Integer);
begin
 _ProcessBitmap24(ABitMap, Value, _BlendBurn);
end;


procedure _BlendBurn32(const ABitMap: TBitmap;Value: Integer);
begin
 _ProcessBitmap32(ABitMap, Value, _BlendBurn);
end;


{result := (a*b) SHR 8;}

procedure _BlendMultiply(const AColor: TColor;Value: Integer; out NewColor:TColor);
var
  r, g, b      : byte;
  ARGB         : TColor;
  br, bg, bb   : byte;
begin
  ARGB := Value;
  GetRGB(AColor, r,g, b);
  GetRGB(ARGB  , br,bg, bb);
  r:=(r*br) shr 8;
  g:=(g*bg) shr 8;
  b:=(b*bb) shr 8;
  NewColor:= RGB(r,g,b);
end;


procedure _BlendMultiply24(const ABitMap: TBitmap;Value: Integer);
begin
 _ProcessBitmap24(ABitMap, Value, _BlendMultiply);
end;

procedure _BlendMultiply32(const ABitMap: TBitmap;Value: Integer);
begin
 _ProcessBitmap32(ABitMap, Value, _BlendMultiply);
end;


{
c := a+b;
if c > 255 then result := 255 else result := c;
}
procedure _BlendAdditive(const AColor: TColor;Value: Integer; out NewColor:TColor);
var
  r, g, b      : byte;
  ARGB         : TColor;
  br, bg, bb   : byte;
  c            : Integer;
begin
  ARGB := Value;
  GetRGB(AColor, r,g, b);
  GetRGB(ARGB, br,bg, bb);

  c:=RoundIntToByte(r+br);
  r:=c;
  c:=RoundIntToByte(g+bg);
  g:=c;
  c:=RoundIntToByte(b+bb);
  b:=c;

  NewColor:= RGB(r,g,b);
end;

procedure _BlendAdditive24(const ABitMap: TBitmap;Value: Integer);
begin
  _ProcessBitmap24(ABitMap, Value, _BlendAdditive);
end;


procedure _BlendAdditive32(const ABitMap: TBitmap;Value: Integer);
begin
  _ProcessBitmap32(ABitMap, Value, _BlendAdditive);
end;

{
if b = 255 then
  result := 255
else begin
  c := (a SHL 8) DIV (255-b);
  if c > 255 then result := 255 else result := c;
end;
}
procedure _BlendDodge(const AColor: TColor;Value: Integer; out NewColor:TColor);
var
  r, g, b      : byte;
  ARGB         : TColor;
  br, bg, bb   : byte;
  c            : Integer;
begin
  GetRGB(AColor, r,g, b);

  ARGB := Value;
  GetRGB(ARGB, br,bg, bb);

  if br=255 then
   r:=255
  else
  begin
    c := RoundIntToByte((r SHL 8) DIV (255-br));
    r := c;
  end;

  if bg=255 then
   g:=255
  else
  begin
    c := RoundIntToByte((g SHL 8) DIV (255-bg));
    g := c;
  end;

  if bb=255 then
   b:=255
  else
  begin
    c := RoundIntToByte((b SHL 8) DIV (255-bb));
    b := c;
  end;

  NewColor:= RGB(r,g,b);
end;

procedure _BlendDodge24(const ABitMap: TBitmap;Value: Integer);
begin
 _ProcessBitmap24(ABitMap, Value, _BlendDodge);
end;


procedure _BlendDodge32(const ABitMap: TBitmap;Value: Integer);
begin
 _ProcessBitmap32(ABitMap, Value, _BlendDodge);
end;

{
if a < 128 then
  result := (a*b) SHR 7
else
  result := 255 - ((255-a) * (255-b) SHR 7);
}
procedure _BlendOverlay(const AColor: TColor;Value: Integer; out NewColor:TColor);
var
  r, g, b      : byte;
  ARGB         : TColor;
  br, bg, bb   : byte;
  c            : Integer;
begin
  GetRGB(AColor, r,g, b);
  ARGB := Value;
  GetRGB(ARGB, br,bg, bb);

  if r<128 then
   r:=RoundIntToByte((r*br) shr 7)
  else
  begin
    c := RoundIntToByte(255 - ((255-r) * (255-br) SHR 7));
    r := c;
  end;

  if g<128 then
   g:=RoundIntToByte((g*bg) shr 7)
  else
  begin
    c := RoundIntToByte(255 - ((255-g) * (255-bg) SHR 7));
    g := c;
  end;

  if b<128 then
   b:=RoundIntToByte((r*bb) shr 7)
  else
  begin
    c := RoundIntToByte(255 - ((255-b) * (255-bb) SHR 7));
    b := c;
  end;

  NewColor:= RGB(r,g,b);
end;

procedure _BlendOverlay24(const ABitMap: TBitmap;Value: Integer);
begin
  _ProcessBitmap24(ABitMap, Value, _BlendOverlay);
end;


procedure _BlendOverlay32(const ABitMap: TBitmap;Value: Integer);
begin
  _ProcessBitmap32(ABitMap, Value, _BlendOverlay);
end;

{
result := abs(a-b);
}

procedure _BlendDifference(const AColor: TColor;Value: Integer; out NewColor:TColor);
var
  r, g, b      : byte;
  ARGB         : TColor;
  br, bg, bb   : byte;
begin
  GetRGB(AColor, r,g, b);
  ARGB := Value;
  GetRGB(ARGB, br,bg, bb);
  r:=abs(r-br);
  g:=abs(g-bg);
  b:=abs(b-bb);
  NewColor:= RGB(r,g,b);
end;


procedure _BlendDifference24(const ABitMap: TBitmap;Value: Integer);
begin
  _ProcessBitmap24(ABitMap, Value, _BlendDifference);
end;

procedure _BlendDifference32(const ABitMap: TBitmap;Value: Integer);
begin
  _ProcessBitmap32(ABitMap, Value, _BlendDifference);
end;

{
if a > b then
  result := a
else
  result := b;
}
procedure _BlendLighten(const AColor: TColor;Value: Integer; out NewColor:TColor);
var
  r, g, b      : byte;
  ARGB         : TColor;
  br, bg, bb   : byte;
begin
  GetRGB(AColor, r,g, b);

  ARGB := Value;
  GetRGB(ARGB, br,bg, bb);

  r:=IfThen(r>br, r, br);
  g:=IfThen(g>bg, g, bg);
  b:=IfThen(b>bb, b, bb);

  NewColor:= RGB(r,g,b);
end;

procedure _BlendLighten24(const ABitMap: TBitmap;Value: Integer);
begin
 _ProcessBitmap24(ABitMap, Value, _BlendLighten);
end;

procedure _BlendLighten32(const ABitMap: TBitmap;Value: Integer);
begin
 _ProcessBitmap32(ABitMap, Value, _BlendLighten);
end;

{
if a < b then
  result := a
else
  result := b;
}
procedure _BlendDarken(const AColor: TColor;Value: Integer; out NewColor:TColor);
var
  r, g, b      : byte;
  ARGB         : TColor;
  br, bg, bb   : byte;
begin
  GetRGB(AColor, r,g, b);
  ARGB := Value;
  GetRGB(ARGB, br,bg, bb);
  r:=IfThen(r<br, r, br);
  g:=IfThen(g<bg, g, bg);
  b:=IfThen(b<bb, b, bb);
  NewColor:= RGB(r,g,b);
end;

procedure _BlendDarken24(const ABitMap: TBitmap;Value: Integer);
begin
 _ProcessBitmap24(ABitMap, Value, _BlendDarken);
end;

procedure _BlendDarken32(const ABitMap: TBitmap;Value: Integer);
begin
 _ProcessBitmap32(ABitMap, Value, _BlendDarken);
end;

{
result := 255 - ((255-a) * (255-b) SHR 8);
}
procedure _BlendScreen(const AColor: TColor;Value: Integer; out NewColor:TColor);
var
  r, g, b      : byte;
  ARGB         : TColor;
  br, bg, bb   : byte;
  c            : Integer;
begin
  GetRGB(AColor, r,g, b);

  ARGB := Value;
  GetRGB(ARGB, br,bg, bb);

  c := RoundIntToByte(255 - ((255-r) * (255-br) SHR 8));
  r := c;

  c := RoundIntToByte(255 - ((255-g) * (255-bg) SHR 8));
  g := c;

  c := RoundIntToByte(255 - ((255-b) * (255-bb) SHR 8));
  b := c;

  NewColor:= RGB(r,g,b);
end;

procedure _BlendScreen24(const ABitMap: TBitmap;Value: Integer);
begin
 _ProcessBitmap24(ABitMap, Value, _BlendScreen);
end;


procedure _BlendScreen32(const ABitMap: TBitmap;Value: Integer);
begin
 _ProcessBitmap32(ABitMap, Value, _BlendScreen);
end;


procedure _SetRComponent(const AColor: TColor;Value: Integer; out NewColor:TColor);
var
  r, g, b      : byte;
begin
  GetRGB(AColor, r,g, b);
  r:=RoundIntToByte(r+Value);
  NewColor:= RGB(r,g,b);
end;

procedure _SetGComponent(const AColor: TColor;Value: Integer; out NewColor:TColor);
var
  r, g, b      : byte;
begin
  GetRGB(AColor, r,g, b);
  g:=RoundIntToByte(g+Value);
  NewColor:= RGB(r,g,b);
end;

procedure _SetBComponent(const AColor: TColor;Value: Integer; out NewColor:TColor);
var
  r, g, b      : byte;
begin
  GetRGB(AColor, r,g, b);
  b:=RoundIntToByte(b+Value);
  NewColor:= RGB(r,g,b);
end;

procedure _SetRGB24(const ABitMap: TBitmap; DR,DG,DB: Integer);
var
  r, g, b : byte;
  x, y:    integer;
  Line, Delta: integer;
begin
  Line  := integer(ABitMap.ScanLine[0]);
  Delta := integer(ABitMap.ScanLine[1]) - Line;
  for y := 0 to ABitMap.Height - 1 do
  begin
    for x := 0 to ABitMap.Width - 1 do
    begin
      r    := PRGBArray24(Line)[x].rgbtRed;
      g    := PRGBArray24(Line)[x].rgbtGreen;
      b    := PRGBArray24(Line)[x].rgbtBlue;
      PRGBArray24(Line)[x].rgbtRed := RoundIntToByte(r+DR);
      PRGBArray24(Line)[x].rgbtGreen := RoundIntToByte(g+DG);
      PRGBArray24(Line)[x].rgbtBlue := RoundIntToByte(b+DB);
    end;
    Inc(Line, Delta);
  end;
end;


procedure _SetRGB32(const ABitMap: TBitmap; DR,DG,DB: Integer);
var
  r, g, b, a: byte;
  x, y:    integer;
  Line, Delta: integer;
begin
  Line  := integer(ABitMap.ScanLine[0]);
  Delta := integer(ABitMap.ScanLine[1]) - Line;
  for y := 0 to ABitMap.Height - 1 do
  begin
    for x := 0 to ABitMap.Width - 1 do
    begin
      r    := PRGBArray32(Line)[x].rgbRed;
      g    := PRGBArray32(Line)[x].rgbGreen;
      b    := PRGBArray32(Line)[x].rgbBlue;
      a    := PRGBArray32(Line)[x].rgbReserved;
      PRGBArray32(Line)[x].rgbRed := RoundIntToByte(r+DR);
      PRGBArray32(Line)[x].rgbGreen := RoundIntToByte(g+DG);
      PRGBArray32(Line)[x].rgbBlue := RoundIntToByte(b+DB);
      PRGBArray32(Line)[x].rgbReserved := a;
    end;
    Inc(Line, Delta);
  end;
end;


procedure _Saturation(const AColor: TColor;Value: Integer; out NewColor:TColor);
var
  r, g, b      : byte;
  Gray         : Integer;
begin
  GetRGB(AColor, r,g, b);
  Gray := (r + g + b) div 3;
  r := RoundIntToByte(Gray + (((r - Gray) * Value) div 255));
  g := RoundIntToByte(Gray + (((g - Gray) * Value) div 255));
  b := RoundIntToByte(Gray + (((b - Gray) * Value) div 255));
  NewColor:= RGB(r,g,b);
end;


procedure _Saturation24(var ABitMap: TBitmap; Value: integer);
begin
 _ProcessBitmap24(ABitMap, Value, _Saturation);
end;

procedure _Saturation32(const ABitMap: TBitmap; Value: integer);
begin
 _ProcessBitmap32(ABitMap, Value, _Saturation);
end;


procedure _Lightness(const AColor: TColor;Value: Integer; out NewColor:TColor);
var
  r, g, b      : byte;
begin
  GetRGB(AColor, r,g, b);
  r := RoundIntToByte(r + ((255 - r) * Value) div 255);
  g := RoundIntToByte(g + ((255 - g) * Value) div 255);
  b := RoundIntToByte(b + ((255 - b) * Value) div 255);
  NewColor:= RGB(r,g,b);
end;

procedure _Lightness24(var ABitMap: TBitmap; Value: integer);
begin
 _ProcessBitmap24(ABitMap, Value, _Lightness);
end;

procedure _Lightness32(const ABitMap: TBitmap; Value: integer);
begin
 _ProcessBitmap32(ABitMap, Value, _Lightness);
end;

procedure _Darkness(const AColor: TColor;Value: Integer; out NewColor:TColor);
var
  r, g, b      : byte;
begin
  GetRGB(AColor, r,g, b);
  r := RoundIntToByte(r - ((r) * Value) div 255);
  g := RoundIntToByte(g - ((g) * Value) div 255);
  b := RoundIntToByte(b - ((b) * Value) div 255);
  NewColor:= RGB(r,g,b);
end;


procedure _Darkness24(var ABitMap: TBitmap; Value: integer);
begin
  _ProcessBitmap24(ABitMap, Value, _Darkness);
end;

procedure _Darkness32(const ABitMap: TBitmap; Value: integer);
begin
  _ProcessBitmap32(ABitMap, Value, _Darkness);
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

{ TBitmap32Filter }


{ TBitmap32HueFilter }
procedure TBitmap32HueFilter.ProcessBitmap(ABitMap: TBitmap);
begin
  if ABitMap.PixelFormat=pf32bit then
   _ProcessBitmap32(ABitMap, ColorValue, _Hue)
  else
  if ABitMap.PixelFormat=pf24bit then
   _ProcessBitmap24(ABitMap, ColorValue, _Hue);
end;

function TBitmap32HueFilter.ProcessColor(AColor: TColor): TColor;
begin
  _Hue(AColor, ColorValue, Result);
end;

{ TBitmap32SaturationFilter }

procedure TBitmap32SaturationFilter.ProcessBitmap(ABitMap: TBitmap);
begin
  if ABitMap.PixelFormat=pf32bit then
    _ProcessBitmap32(ABitMap, ColorValue, _Saturation)
  else
  if ABitMap.PixelFormat=pf24bit then
    _ProcessBitmap24(ABitMap, ColorValue, _Saturation);
end;

function TBitmap32SaturationFilter.ProcessColor(AColor: TColor): TColor;
begin
  _Saturation(AColor, ColorValue, Result);
end;

{ TBitmap32LightnessFilter }

procedure TBitmap32LightnessFilter.ProcessBitmap(ABitMap: TBitmap);
begin
  if ABitMap.PixelFormat=pf32bit then
  begin
    if ColorValue >= 0 then
      _ProcessBitmap32(ABitMap, ColorValue, _Lightness)
    else
      _ProcessBitmap32(ABitMap, Abs(ColorValue), _Darkness);
  end
  else
  if ABitMap.PixelFormat=pf24bit then
  begin
    if ColorValue >= 0 then
      _ProcessBitmap24(ABitMap, ColorValue, _Lightness)
    else
      _ProcessBitmap24(ABitMap, Abs(ColorValue), _Darkness);
  end;
end;

function TBitmap32LightnessFilter.ProcessColor(AColor: TColor): TColor;
begin
    if ColorValue >= 0 then
      _Lightness(AColor, ColorValue, Result)
    else
      _Darkness(AColor, Abs(ColorValue), Result);
end;

{ TBitmap32SepiaFilter }

procedure TBitmap32SepiaFilter.ProcessBitmap(ABitMap: TBitmap);
begin
  if ABitMap.PixelFormat=pf32bit then
   _ProcessBitmap32(ABitMap, ColorValue, _Sepia)
  else
  if ABitMap.PixelFormat=pf24bit then
   _ProcessBitmap24(ABitMap, ColorValue, _Sepia);
end;

function TBitmap32SepiaFilter.ProcessColor(AColor: TColor): TColor;
begin
   _Sepia(AColor, ColorValue, Result);
end;

{ TColorFilter }

constructor TColorFilter.Create(AColorValue: Integer);
begin
 inherited Create;
 FColorValue:=AColorValue;
end;


{ TBitmap32BlueFilter }

procedure TBitmap32BlueFilter.ProcessBitmap(ABitMap: TBitmap);
begin
  if ABitMap.PixelFormat=pf32bit then
    _SetRGB32(ABitMap,0,0,ColorValue)
  else
  if ABitMap.PixelFormat=pf24bit then
   _SetRGB24(ABitMap,0,0,ColorValue);
end;

function TBitmap32BlueFilter.ProcessColor(AColor: TColor): TColor;
begin
  _SetBComponent(AColor, ColorValue, Result);
end;

{ TBitmap32RedFilter }

procedure TBitmap32RedFilter.ProcessBitmap(ABitMap: TBitmap);
begin
  if ABitMap.PixelFormat=pf32bit then
    _SetRGB32(ABitMap,ColorValue,0,0)
  else
  if ABitMap.PixelFormat=pf24bit then
    _SetRGB24(ABitMap,ColorValue,0,0);
end;

function TBitmap32RedFilter.ProcessColor(AColor: TColor): TColor;
begin
  _SetRComponent(AColor, ColorValue, Result);
end;

{ TBitmap32GreenFilter }

procedure TBitmap32GreenFilter.ProcessBitmap(ABitMap: TBitmap);
begin
  if ABitMap.PixelFormat=pf32bit then
    _SetRGB32(ABitMap,0,ColorValue,0)
  else
  if ABitMap.PixelFormat=pf24bit then
    _SetRGB24(ABitMap,0,ColorValue,0);
end;

function TBitmap32GreenFilter.ProcessColor(AColor: TColor): TColor;
begin
  _SetGComponent(AColor, ColorValue, Result);
end;

{ TBitmap32BlendBurn }

procedure TBitmap32BlendBurn.ProcessBitmap(ABitMap: TBitmap);
begin
  if FUseBitmap then
  begin
    if ABitMap.PixelFormat=pf32bit then
     _ProcessBitmap32(FSourceBitmap , ABitMap , _BlendBurn)
    else
    if ABitMap.PixelFormat=pf24bit then
     _ProcessBitmap24(FSourceBitmap , ABitMap , _BlendBurn)
  end
  else
  if ABitMap.PixelFormat=pf32bit then
   _ProcessBitmap32(ABitMap, ColorValue, _BlendBurn)
  else
  if ABitMap.PixelFormat=pf24bit then
   _ProcessBitmap24(ABitMap, ColorValue, _BlendBurn);
end;

function TBitmap32BlendBurn.ProcessColor(AColor: TColor): TColor;
begin
  _BlendBurn(AColor, ColorValue, Result);
end;

{ TBitmap32BlendMultiply }

procedure TBitmap32BlendMultiply.ProcessBitmap(ABitMap: TBitmap);
begin
  if FUseBitmap then
  begin
    if ABitMap.PixelFormat=pf32bit then
     _ProcessBitmap32(FSourceBitmap , ABitMap , _BlendMultiply)
    else
    if ABitMap.PixelFormat=pf24bit then
     _ProcessBitmap24(FSourceBitmap , ABitMap , _BlendMultiply)
  end
  else
  if ABitMap.PixelFormat=pf32bit then
   _ProcessBitmap32(ABitMap, ColorValue, _BlendMultiply)
  else
  if ABitMap.PixelFormat=pf24bit then
   _ProcessBitmap24(ABitMap, ColorValue, _BlendMultiply);
end;

function TBitmap32BlendMultiply.ProcessColor(AColor: TColor): TColor;
begin
  _BlendMultiply(AColor, ColorValue, Result);
end;

{ TBitmap32BlendAdditive }

procedure TBitmap32BlendAdditive.ProcessBitmap(ABitMap: TBitmap);
begin
  if FUseBitmap then
  begin
    if ABitMap.PixelFormat=pf32bit then
     _ProcessBitmap32(FSourceBitmap , ABitMap , _BlendAdditive)
    else
    if ABitMap.PixelFormat=pf24bit then
     _ProcessBitmap24(FSourceBitmap , ABitMap , _BlendAdditive)
  end
  else
  if ABitMap.PixelFormat=pf32bit then
   _ProcessBitmap32(ABitMap, ColorValue, _BlendAdditive)
  else
  if ABitMap.PixelFormat=pf24bit then
   _ProcessBitmap24(ABitMap, ColorValue, _BlendAdditive);
end;

function TBitmap32BlendAdditive.ProcessColor(AColor: TColor): TColor;
begin
  _BlendAdditive(AColor, ColorValue, Result);
end;

{ TBitmap32BlendDodge }

procedure TBitmap32BlendDodge.ProcessBitmap(ABitMap: TBitmap);
begin
  if FUseBitmap then
  begin
    if ABitMap.PixelFormat=pf32bit then
     _ProcessBitmap32(FSourceBitmap , ABitMap , _BlendDodge)
    else
    if ABitMap.PixelFormat=pf24bit then
     _ProcessBitmap24(FSourceBitmap , ABitMap , _BlendDodge)
  end
  else
  if ABitMap.PixelFormat=pf32bit then
   _ProcessBitmap32(ABitMap, ColorValue, _BlendDodge)
  else
  if ABitMap.PixelFormat=pf24bit then
   _ProcessBitmap24(ABitMap, ColorValue, _BlendDodge);
end;

function TBitmap32BlendDodge.ProcessColor(AColor: TColor): TColor;
begin
  _BlendDodge(AColor, ColorValue, Result);
end;

{ TBitmap32BlendOverlay }

procedure TBitmap32BlendOverlay.ProcessBitmap(ABitMap: TBitmap);
begin
  if FUseBitmap then
  begin
    if ABitMap.PixelFormat=pf32bit then
     _ProcessBitmap32(FSourceBitmap , ABitMap , _BlendOverlay)
    else
    if ABitMap.PixelFormat=pf24bit then
     _ProcessBitmap24(FSourceBitmap , ABitMap , _BlendOverlay)
  end
  else
  if ABitMap.PixelFormat=pf32bit then
   _ProcessBitmap32(ABitMap, ColorValue, _BlendOverlay)
  else
  if ABitMap.PixelFormat=pf24bit then
   _ProcessBitmap24(ABitMap, ColorValue, _BlendOverlay);
end;

function TBitmap32BlendOverlay.ProcessColor(AColor: TColor): TColor;
begin
  _BlendOverlay(AColor, ColorValue, Result);
end;

{ TBitmap32BlendLighten }

procedure TBitmap32BlendLighten.ProcessBitmap(ABitMap: TBitmap);
begin
  if FUseBitmap then
  begin
    if ABitMap.PixelFormat=pf32bit then
     _ProcessBitmap32(FSourceBitmap , ABitMap , _BlendLighten)
    else
    if ABitMap.PixelFormat=pf24bit then
     _ProcessBitmap24(FSourceBitmap , ABitMap , _BlendLighten)

  end
  else
  if ABitMap.PixelFormat=pf32bit then
   _ProcessBitmap32(ABitMap, ColorValue, _BlendLighten)
  else
  if ABitMap.PixelFormat=pf24bit then
   _ProcessBitmap24(ABitMap, ColorValue, _BlendLighten);
end;

function TBitmap32BlendLighten.ProcessColor(AColor: TColor): TColor;
begin
  _BlendLighten(AColor, ColorValue, Result);
end;

{ TBitmap32BlendDarken }

procedure TBitmap32BlendDarken.ProcessBitmap(ABitMap: TBitmap);
begin
  if FUseBitmap then
  begin
    if ABitMap.PixelFormat=pf32bit then
     _ProcessBitmap32(FSourceBitmap , ABitMap , _BlendDarken)
    else
    if ABitMap.PixelFormat=pf24bit then
     _ProcessBitmap24(FSourceBitmap , ABitMap , _BlendDarken)
  end
  else
  if ABitMap.PixelFormat=pf32bit then
   _ProcessBitmap32(ABitMap, ColorValue, _BlendDarken)
  else
  if ABitMap.PixelFormat=pf24bit then
   _ProcessBitmap24(ABitMap, ColorValue, _BlendDarken);
end;

function TBitmap32BlendDarken.ProcessColor(AColor: TColor): TColor;
begin
  _BlendDarken(AColor, ColorValue, Result);
end;

{ TBitmap32BlendScreen }

procedure TBitmap32BlendScreen.ProcessBitmap(ABitMap: TBitmap);
begin
  if FUseBitmap then
  begin
    if ABitMap.PixelFormat=pf32bit then
     _ProcessBitmap32(FSourceBitmap , ABitMap , _BlendScreen)
    else
    if ABitMap.PixelFormat=pf24bit then
     _ProcessBitmap24(FSourceBitmap , ABitMap , _BlendScreen)
  end
  else
  if ABitMap.PixelFormat=pf32bit then
   _ProcessBitmap32(ABitMap, ColorValue, _BlendScreen)
  else
  if ABitMap.PixelFormat=pf24bit then
   _ProcessBitmap24(ABitMap, ColorValue, _BlendScreen);
end;

function TBitmap32BlendScreen.ProcessColor(AColor: TColor): TColor;
begin
  _BlendScreen(AColor, ColorValue, Result);
end;

{ TBitmap32BlendDifference }

procedure TBitmap32BlendDifference.ProcessBitmap(ABitMap: TBitmap);
begin
  if FUseBitmap then
  begin
    if ABitMap.PixelFormat=pf32bit then
     _ProcessBitmap32(FSourceBitmap , ABitMap , _BlendDifference)
    else
    if ABitMap.PixelFormat=pf24bit then
     _ProcessBitmap24(FSourceBitmap , ABitMap , _BlendDifference)
  end
  else
  if ABitMap.PixelFormat=pf32bit then
   _ProcessBitmap32(ABitMap, ColorValue, _BlendDifference)
  else
  if ABitMap.PixelFormat=pf24bit then
   _ProcessBitmap24(ABitMap, ColorValue, _BlendDifference);
end;

function TBitmap32BlendDifference.ProcessColor(AColor: TColor): TColor;
begin
  _BlendDifference(AColor, ColorValue, Result);
end;

{ TBitmapFilter }

constructor TBitmapFilter.CreateBitMap(ASourceBitmap: TBitmap);
begin
  inherited Create (clNone);
  FSourceBitmap:=ASourceBitmap;
  FUseBitmap:=True;
end;

constructor TBitmapFilter.Create(AColorValue: Integer);
begin
  inherited Create(AColorValue);
  FUseBitmap:=False;
  FSourceBitmap:=nil;
end;

end.
