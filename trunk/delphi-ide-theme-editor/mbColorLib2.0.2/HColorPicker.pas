unit HColorPicker;

interface

uses
 Windows, Messages, SysUtils, Classes, Controls, Graphics, Forms,
 RGBHSVUtils, mbTrackBarPicker, HTMLColors, Scanlines;

type
 THColorPicker = class(TmbTrackBarPicker)
 private
  FVal, FSat, FHue: integer;
  FHBmp: TBitmap;

  function ArrowPosFromHue(h: integer): integer;
  function HueFromArrowPos(p: integer): integer;
  function GetSelectedColor: TColor;
  procedure SetSelectedColor(c: TColor);
  procedure CreateHGradient;
  procedure SetHue(h: integer);
  procedure SetSat(s: integer);
  procedure SetValue(v: integer);
 protected
  procedure CreateWnd; override;
  procedure Execute(tbaAction: integer); override;
  function GetArrowPos: integer; override;
  function GetSelectedValue: integer; override;
 public
  constructor Create(AOwner: TComponent); override;
  destructor Destroy; override;
 published
  property Hue: integer read FHue write SetHue default 0;
  property Saturation: integer read FSat write SetSat default 255;
  property Value: integer read FVal write SetValue default 255;
  property SelectedColor: TColor read GetSelectedColor write SetSelectedColor default clRed;
 end;

procedure Register;

implementation

procedure Register;
begin
 RegisterComponents('mbColor Lib', [THColorPicker]);
end;

{THColorPicker}

constructor THColorPicker.Create(AOwner: TComponent);
begin
 inherited;
 FHBmp := TBitmap.Create;
 FHBmp.PixelFormat := pf32bit;
 Width := 267;
 Height := 22;
 FSat := 255;
 FVal := 255;
 FArrowPos := ArrowPosFromHue(0);
 FChange := false;
 SetHue(0);
 HintFormat := 'Hue: %value';
 FManual := false;
 FChange := true;
end;

destructor THColorPicker.Destroy;
begin
 FHBmp.Free;
 inherited Destroy;
end;

procedure THColorPicker.CreateWnd;
begin
 inherited;
 CreateHGradient;
end;

procedure THColorPicker.CreateHGradient;
var
 i,j: integer;
 row: pRGBQuadArray;
begin
 if FHBmp = nil then
  begin
   FHBmp := TBitmap.Create;
   FHBmp.PixelFormat := pf32bit;
  end;
 if Layout = lyHorizontal then
  begin
   FHBmp.width := 360;
   FHBmp.height := 12;
   for i := 0 to 359 do
    for j := 0 to 11 do
     begin
      row := FHBmp.ScanLine[j];
      if not WebSafe then
       row[i] := RGBtoRGBQuad(HSVtoColor(i, FSat, FVal))
//       FHBmp.Canvas.Pixels[i, j] := HSVtoColor(i, FSat, FVal)
      else
       row[i] := RGBtoRGBQuad(GetWebSafe(HSVtoColor(i, FSat, FVal)));
//       FHBmp.Canvas.Pixels[i, j] := GetWebSafe(HSVtoColor(i, FSat, FVal));
     end;
  end
 else
  begin
   FHBmp.width := 12;
   FHBmp.height := 360;
   for i := 0 to 359 do
    begin
     row := FHBmp.ScanLine[i];
     for j := 0 to 11 do
      if not WebSafe then
       row[j] := RGBtoRGBQuad(HSVtoColor(i, FSat, FVal))
      else
       row[j] := RGBtoRGBQuad(GetWebSafe(HSVtoColor(i, FSat, FVal)));
    end;
  end;
end;

procedure THColorPicker.SetValue(v: integer);
begin
 if v < 0 then v := 0;
 if v > 255 then v := 255;
 if FVal <> v then
  begin
   FVal := v;
   FManual := false;
   CreateHGradient;
   Invalidate;
   if FChange then
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

procedure THColorPicker.SetHue(h: integer);
begin
 if h > 360 then h := 360;
 if h < 0 then h := 0;
 if FHue <> h then
  begin
   FHue := h;
   FArrowPos := ArrowPosFromHue(h);
   FManual := false;
   Invalidate;
   if FChange then
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

procedure THColorPicker.SetSat(s: integer);
begin
 if s > 255 then s := 255;
 if s < 0 then s := 0;
 if FSat <> s then
  begin
   FSat := s;
   FManual := false;
   CreateHGradient;
   Invalidate;
   if FChange then
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

function THColorPicker.ArrowPosFromHue(h: integer): integer;
var
 a: integer;
begin
 if Layout = lyHorizontal then
  begin
   a := Round(((Width - 12)/360)*h);
   if a > Width - FLimit then a := Width - FLimit;
  end
 else
  begin
   a := Round(((Height - 12)/360)*h);
   if a > Height - FLimit then a := Height - FLimit;
  end;
 if a < 0 then a := 0;
 Result := a;
end;

function THColorPicker.HueFromArrowPos(p: integer): integer;
var
 r: integer;
begin
 if Layout = lyHorizontal then
  r := Round(p/((Width - 12)/360))
 else
  r := Round(p/((Height - 12)/360));
 if r < 0 then r := 0;
 if r > 360 then r := 360;
 Result := r;
end;

function THColorPicker.GetSelectedColor: TColor;
begin
 if not WebSafe then
  Result := HSVtoColor(FHue, FSat, FVal)
 else
  Result := GetWebSafe(HSVtoColor(FHue, FSat, FVal));
end;

function THColorPicker.GetSelectedValue: integer;
begin
  Result := FHue;
end;

procedure THColorPicker.SetSelectedColor(c: TColor);
var
 h, s, v: integer;
begin
 if WebSafe then c := GetWebSafe(c);
 RGBToHSV(GetRValue(c), GetGValue(c), GetBValue(c), h, s, v);
 FChange := false;
 SetHue(h);
 SetSat(s);
 SetValue(v);
 FManual := false;
 FChange := true;
 if Assigned(OnChange) then OnChange(Self);
end;

function THColorPicker.GetArrowPos: integer;
begin
 Result := ArrowPosFromHue(FHue);
end;

procedure THColorPicker.Execute(tbaAction: integer);
begin
 case tbaAction of
  TBA_Resize: SetHue(FHue);
  TBA_Paint: Canvas.StretchDraw(FPickRect, FHBmp);
  TBA_MouseMove: FHue := HueFromArrowPos(FArrowPos);
  TBA_MouseDown: FHue := HueFromArrowPos(FArrowPos);
  TBA_MouseUp: FHue := HueFromArrowPos(FArrowPos);
  TBA_WheelUp: SetHue(FHue + Increment);
  TBA_WheelDown: SetHue(FHue - Increment);
  TBA_VKLeft: SetHue(FHue - Increment);
  TBA_VKCtrlLeft: SetHue(0);
  TBA_VKRight: SetHue(FHue + Increment);
  TBA_VKCtrlRight: SetHue(360);
  TBA_VKUp: SetHue(FHue - Increment);
  TBA_VKCtrlUp: SetHue(0);
  TBA_VKDown: SetHue(FHue + Increment);
  TBA_VKCtrlDown: SetHue(360);
  TBA_RedoBMP: CreateHGradient;
 end;
end;

end.
