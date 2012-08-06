unit VColorPicker;

interface

uses
 Windows, Messages, SysUtils, Classes, Controls, Forms, Graphics,
 RGBHSVUtils, mbTrackBarPicker, HTMLColors, Scanlines;

type
 TVColorPicker = class(TmbTrackBarPicker)
 private
  FHue, FSat, FVal: integer;
  FVBmp: TBitmap;

  function ArrowPosFromVal(l: integer): integer;
  function ValFromArrowPos(p: integer): integer;
  function GetSelectedColor: TColor;
  procedure SetSelectedColor(c: TColor);
  procedure CreateVGradient;
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
  property Saturation: integer read FSat write SetSat default 0;
  property Value: integer read FVal write SetValue default 255;
  property SelectedColor: TColor read GetSelectedColor write SetSelectedColor default clRed;
  property Layout default lyVertical;
 end;

procedure Register;

implementation

procedure Register;
begin
 RegisterComponents('mbColor Lib', [TVColorPicker]);
end;

{TVColorPicker}

constructor TVColorPicker.Create(AOwner: TComponent);
begin
 inherited;
 FVBmp := TBitmap.Create;
 FVBmp.PixelFormat := pf32bit;
 FVBmp.SetSize(12, 255);
 Width := 22;
 Height := 267;
 Layout := lyVertical;
 FHue := 0;
 FSat := 0;
 FArrowPos := ArrowPosFromVal(255);
 FChange := false;
 SetValue(255);
 HintFormat := 'Value: %value';
 FManual := false;
 FChange := true;
end;

destructor TVColorPicker.Destroy;
begin
 FVBmp.Free;
 inherited Destroy;
end;

procedure TVColorPicker.CreateWnd;
begin
 inherited;
 CreateVGradient;
end;

procedure TVColorPicker.CreateVGradient;
var
 i,j: integer;
 row: pRGBQuadArray;
begin
 if FVBmp = nil then
  begin
   FVBmp := TBitmap.Create;
   FVBmp.PixelFormat := pf32bit;
  end;
 if Layout = lyHorizontal then
  begin
   FVBmp.width := 255;
   FVBmp.height := 12;
   for i := 0 to 254 do
    for j := 0 to 11 do
     begin
      row := FVBmp.Scanline[j];
      if not WebSafe then
       row[i] := RGBToRGBQuad(HSVtoColor(FHue, FSat, i))
//       FVBmp.Canvas.Pixels[i, j] := HSVtoColor(FHue, FSat, i)
      else
       row[i] := RGBToRGBQuad(GetWebSafe(HSVtoColor(FHue, FSat, i)));
//       FVBmp.Canvas.Pixels[i, j] := GetWebSafe(HSVtoColor(FHue, FSat, i));
     end;
  end
 else
  begin
   FVBmp.width := 12;
   FVBmp.height := 255;
   for i := 0 to 254 do
    begin
     row := FVBmp.ScanLine[i];
     for j := 0 to 11 do
      if not WebSafe then
       row[j] := RGBToRGBQuad(HSVtoColor(FHue, FSat, 255 - i))
//       FVBmp.Canvas.Pixels[j, i] := HSVtoColor(FHue, FSat, 255 - i)
      else
       row[j] := RGBToRGBQuad(GetWebSafe(HSVtoColor(FHue, FSat, 255 - i)));
//       FVBmp.Canvas.Pixels[j, i] := GetWebSafe(HSVtoColor(FHue, FSat, 255 - i));
    end;
  end;
end;

procedure TVColorPicker.SetHue(h: integer);
begin
 if h > 360 then h := 360;
 if h < 0 then h := 0;
 if FHue <> h then
  begin
   FHue := h;
   FManual := false;
   CreateVGradient;
   Invalidate;
   if FChange then
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

procedure TVColorPicker.SetSat(s: integer);
begin
 if s > 255 then s := 255;
 if s < 0 then s := 0;
 if FSat <> s then
  begin
   FSat := s;
   FManual := false;
   CreateVGradient;
   Invalidate;
   if FChange then
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

function TVColorPicker.ArrowPosFromVal(l: integer): integer;
var
 a: integer;
begin
 if Layout = lyHorizontal then
  begin
   a := Round(((Width - 12)/255)*l);
   if a > Width - FLimit then a := Width - FLimit;
  end
 else
  begin
   l := 255 - l;
   a := Round(((Height - 12)/255)*l);
   if a > Height - FLimit then a := Height - FLimit;
  end;
 if a < 0 then a := 0;
 Result := a;
end;

function TVColorPicker.ValFromArrowPos(p: integer): integer;
var
 r: integer;
begin
 if Layout = lyHorizontal then
  r := Round(p/((Width - 12)/255))
 else
  r := Round(255 - p/((Height - 12)/255));
 if r < 0 then r := 0;
 if r > 255 then r := 255;
 Result := r;
end;

procedure TVColorPicker.SetValue(V: integer);
begin
 if v < 0 then v := 0;
 if v > 255 then v := 255;
 if FVal <> v then
  begin
   FVal := v;
   FArrowPos := ArrowPosFromVal(v);
   FManual := false;
   Invalidate;
   if FChange then
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

function TVColorPicker.GetSelectedColor: TColor;
begin
 if not WebSafe then
  Result := HSVtoColor(FHue, FSat, FVal)
 else
  Result := GetWebSafe(HSVtoColor(FHue, FSat, FVal));
end;

function TVColorPicker.GetSelectedValue: integer;
begin
  Result := FVal;
end;

procedure TVColorPicker.SetSelectedColor(c: TColor);
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

function TVColorPicker.GetArrowPos: integer;
begin
 Result := ArrowPosFromVal(FVal);
end;

procedure TVColorPicker.Execute(tbaAction: integer);
begin
 case tbaAction of
  TBA_Resize: SetValue(FVal);
  TBA_Paint: Canvas.StretchDraw(FPickRect, FVBmp);
  TBA_MouseMove: FVal := ValFromArrowPos(FArrowPos);
  TBA_MouseDown: FVal := ValFromArrowPos(FArrowPos);
  TBA_MouseUp: FVal := ValFromArrowPos(FArrowPos);
  TBA_WheelUp: SetValue(FVal + Increment);
  TBA_WheelDown: SetValue(FVal - Increment);
  TBA_VKRight: SetValue(FVal + Increment);
  TBA_VKCtrlRight: SetValue(255);
  TBA_VKLeft: SetValue(FVal - Increment);
  TBA_VKCtrlLeft: SetValue(0);
  TBA_VKUp: SetValue(FVal + Increment);
  TBA_VKCtrlUp: SetValue(255);
  TBA_VKDown: SetValue(FVal - Increment);
  TBA_VKCtrlDown: SetValue(0);
  TBA_RedoBMP: CreateVGradient;
 end;
end;

end.
