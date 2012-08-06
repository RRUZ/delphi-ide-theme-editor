unit SColorPicker;

interface

uses
 Windows, Messages, SysUtils, Classes, Controls, Graphics, Forms,
 RGBHSVUtils, mbTrackBarPicker, HTMLColors, Scanlines;

type
 TSColorPicker = class(TmbTrackBarPicker)
 private
  FVal, FHue, FSat: integer;
  FSBmp: TBitmap;

  function ArrowPosFromSat(s: integer): integer;
  function SatFromArrowPos(p: integer): integer;
  function GetSelectedColor: TColor;
  procedure SetSelectedColor(c: TColor);
  procedure CreateSGradient;
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
 RegisterComponents('mbColor Lib', [TSColorPicker]);
end;

{ TSColorPicker }

constructor TSColorPicker.Create(AOwner: TComponent);
begin
 inherited;
 FSBmp := TBitmap.Create;
 FSBmp.PixelFormat := pf32bit;
 Width := 267;
 Height := 22;
 FHue := 0;
 FVal := 255;
 FArrowPos := ArrowPosFromSat(0);
 FChange := false;
 SetSat(255);
 HintFormat := 'Saturation: %value';
 FManual := false;
 FChange := true;
end;

destructor TSColorPicker.Destroy;
begin
 FSBmp.Free;
 inherited Destroy;
end;

procedure TSColorPicker.CreateWnd;
begin
 inherited;
 CreateSGradient;
end;

procedure TSColorPicker.CreateSGradient;
var
 i,j: integer;
 row: pRGBQuadArray;
begin
 if FSBmp = nil then
  begin
   FSBmp := TBitmap.Create;
   FSBmp.PixelFormat := pf32bit;
  end;
 if Layout = lyHorizontal then
  begin
   FSBmp.width := 255;
   FSBmp.height := 12;
   for i := 0 to 254 do
    for j := 0 to 11 do
     begin
      row := FSBmp.Scanline[j];
      if not WebSafe then
       row[i] := RGBToRGBQuad(HSVtoColor(FHue, i, FVal))
//       FSBmp.Canvas.Pixels[i, j] := HSVtoColor(FHue, i, FVal)
      else
       row[i] := RGBToRGBQuad(GetWebSafe(HSVtoColor(FHue, i, FVal)));
//       FSBmp.Canvas.Pixels[i, j] := GetWebSafe(HSVtoColor(FHue, i, FVal));
     end;
  end
 else
  begin
   FSBmp.width := 12;
   FSBmp.height := 255;
   for i := 0 to 254 do
    begin
     row := FSBmp.Scanline[i];
     for j := 0 to 11 do
      if not WebSafe then
       row[j] := RGBToRGBQuad(HSVtoColor(FHue, 255-i, FVal))
//       FSBmp.Canvas.Pixels[j, i] := HSVtoColor(FHue, 255-i, FVal)
      else
       row[j] := RGBToRGBQuad(GetWebSafe(HSVtoColor(FHue, 255-i, FVal)));
//       FSBmp.Canvas.Pixels[j, i] := GetWebSafe(HSVtoColor(FHue, 255-i, FVal));
    end;
  end;
end;

procedure TSColorPicker.SetValue(v: integer);
begin
 if v < 0 then v := 0;
 if v > 255 then v := 255;
 if FVal <> v then
  begin
   FVal := v;
   FManual := false;
   CreateSGradient;
   Invalidate;
   if FChange then
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

procedure TSColorPicker.SetHue(h: integer);
begin
 if h > 360 then h := 360;
 if h < 0 then h := 0;
 if FHue <> h then
  begin
   FHue := h;
   CreateSGradient;
   FManual := false;
   Invalidate;
   if FChange then
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

procedure TSColorPicker.SetSat(s: integer);
begin
 if s > 255 then s := 255;
 if s < 0 then s := 0;
 if FSat <> s then
  begin
   FSat := s;
   FManual := false;
   FArrowPos := ArrowPosFromSat(s);
   Invalidate;
   if FChange then
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

function TSColorPicker.ArrowPosFromSat(s: integer): integer;
var
 a: integer;
begin
 if Layout = lyHorizontal then
  begin
   a := Round(((Width - 12)/255)*s);
   if a > Width - FLimit then a := Width - FLimit;
  end
 else
  begin
   s := 255 - s;
   a := Round(((Height - 12)/255)*s);
   if a > Height - FLimit then a := Height - FLimit;
  end;
 if a < 0 then a := 0;
 Result := a;
end;

function TSColorPicker.SatFromArrowPos(p: integer): integer;
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

function TSColorPicker.GetSelectedColor: TColor;
begin
 if not WebSafe then
  Result := HSVtoColor(FHue, FSat, FVal)
 else
  Result := GetWebSafe(HSVtoColor(FHue, FSat, FVal));
end;

function TSColorPicker.GetSelectedValue: integer;
begin
  Result := FSat;
end;

procedure TSColorPicker.SetSelectedColor(c: TColor);
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

function TSColorPicker.GetArrowPos: integer;
begin
 Result := ArrowPosFromSat(FSat);
end;

procedure TSColorPicker.Execute(tbaAction: integer);
begin
 case tbaAction of
  TBA_Resize: SetSat(FSat);
  TBA_Paint: Canvas.StretchDraw(FPickRect, FSBmp);
  TBA_MouseMove: FSat := SatFromArrowPos(FArrowPos);
  TBA_MouseDown: FSat := SatFromArrowPos(FArrowPos);
  TBA_MouseUp: FSat := SatFromArrowPos(FArrowPos);
  TBA_WheelUp: SetSat(FSat + Increment);
  TBA_WheelDown: SetSat(FSat - Increment);
  TBA_VKLeft: SetSat(FSat - Increment);
  TBA_VKCtrlLeft: SetSat(0);
  TBA_VKRight: SetSat(FSat + Increment);
  TBA_VKCtrlRight: SetSat(255);
  TBA_VKUp: SetSat(FSat + Increment);
  TBA_VKCtrlUp: SetSat(255);
  TBA_VKDown: SetSat(FSat - Increment);
  TBA_VKCtrlDown: SetSat(0);
  TBA_RedoBMP: CreateSGradient;
 end;
end;

end.
