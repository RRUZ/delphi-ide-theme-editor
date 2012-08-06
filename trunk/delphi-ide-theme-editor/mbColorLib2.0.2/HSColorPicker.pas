unit HSColorPicker;

interface

uses
 Windows, Messages, SysUtils, Classes, Controls, Graphics, Math, Forms,
 RGBHSLUtils, HTMLColors, SelPropUtils, mbColorPickerControl, Scanlines;

type
 THSColorPicker = class(TmbColorPickerControl)
 private
  FSelected: TColor;
  FHSLBmp: TBitmap;
  FOnChange: TNotifyEvent;
  FHue, FSaturation, FLuminance: integer;
  FLum: integer;
  FManual: boolean;
  dx, dy, mxx, myy: integer;

  procedure SetHValue(h: integer);
  procedure SetSValue(s: integer);
 protected
  function GetSelectedColor: TColor; override;
  procedure WebSafeChanged; override;
  procedure SetSelectedColor(c: TColor); override;
  procedure CNKeyDown(var Message: TWMKeyDown); message CN_KEYDOWN;
  procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
  procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  procedure DrawMarker(x, y: integer);
  procedure Paint; override;
  procedure CreateHSLGradient;
  procedure Resize; override;
  procedure CreateWnd; override;
  procedure CorrectCoords(var x, y: integer);
  function PredictColor: TColor;
 public
  constructor Create(AOwner: TComponent); override;
  destructor Destroy; override;

  function GetColorAtPoint(x, y: integer): TColor; override;
  property Lum: integer read FLum write FLum default 120;
  property Manual: boolean read FManual;
 published
  property SelectedColor default clRed;
  property HueValue: integer read FHue write SetHValue default 0;
  property SaturationValue: integer read FSaturation write SetSValue default 240;
  property MarkerStyle default msCross;

  property OnChange: TNotifyEvent read FOnChange write FOnChange;
 end;

procedure Register;

implementation

procedure Register;
begin
 RegisterComponents('mbColor Lib', [THSColorPicker]);
end;

{THSColorPicker}

constructor THSColorPicker.Create(AOwner: TComponent);
begin
 inherited;
 FHSLBmp := TBitmap.Create;
 FHSLBmp.PixelFormat := pf32bit;
 FHSLBmp.SetSize(240, 241);
 Width := 239;
 Height := 240;
 HintFormat := 'H: %h S: %hslS'#13'Hex: %hex';
 FHue := 0;
 FSaturation := 240;
 FLuminance := 120;
 FSelected := clRed;
 FLum := 120;
 FManual := false;
 dx := 0;
 dy := 0;
 mxx := 0;
 myy := 0;
 MarkerStyle := msCross;
end;

destructor THSColorPicker.Destroy;
begin
 FHSLBmp.Free;
 inherited Destroy;
end;

procedure THSColorPicker.CreateWnd;
begin
 inherited;
 CreateHSLGradient;
end;

procedure THSColorPicker.CreateHSLGradient;
var
  Hue, Sat : integer;
  row: pRGBQuadArray;
begin
 if FHSLBmp = nil then
  begin
   FHSLBmp := TBitmap.Create;
   FHSLBmp.PixelFormat := pf32bit;
   FHSLBmp.Width := 240;
   FHSLBmp.Height := 241;
  end;
 for Hue := 0 to 239 do
  for Sat := 0 to 240 do
   begin
    row := FHSLBmp.ScanLine[240 - Sat];
    if not WebSafe then
     row[Hue] := RGBToRGBQuad(HSLRangeToRGB(Hue, Sat, 120))
//     FHSLBmp.Canvas.Pixels[Hue, 240 - Sat] := HSLRangeToRGB(Hue, Sat, 120)
    else
     row[Hue] := RGBToRGBQuad(GetWebSafe(HSLRangeToRGB(Hue, Sat, 120)));
//     FHSLBmp.Canvas.Pixels[Hue, 240 - Sat] := GetWebSafe(HSLRangeToRGB(Hue, Sat, 120));
   end;
end;

procedure THSColorPicker.CorrectCoords(var x, y: integer);
begin
 if x < 0 then x := 0;
 if y < 0 then y := 0;
 if x > Width - 1 then x := Width - 1;
 if y > Height - 1 then y := Height - 1;
end;

procedure THSColorPicker.DrawMarker(x, y: integer);
var
 c: TColor;
begin
 CorrectCoords(x, y);
 RGBtoHSLRange(FSelected, FHue, FSaturation, FLuminance);
 if Assigned(FOnChange) then
  FOnChange(Self);
 dx := x;
 dy := y;
 if Focused or (csDesigning in ComponentState) then
  c := clBlack
 else
  c := clWhite;
 case MarkerStyle of
  msCircle: DrawSelCirc(x, y, Canvas);
  msSquare: DrawSelSquare(x, y, Canvas);
  msCross: DrawSelCross(x, y, Canvas, c);
  msCrossCirc: DrawSelCrossCirc(x, y, Canvas, c);
 end;
end;

function THSColorPicker.GetSelectedColor: TColor;
begin
 Result := FSelected;
end;

procedure THSColorPicker.SetSelectedColor(c: TColor);
begin
 if WebSafe then c := GetWebSafe(c);
 RGBtoHSLRange(c, FHue, FSaturation, FLuminance);
 FSelected := c;
 FManual := false;
 mxx := Round(FHue*(Width/239));
 myy := Round((240-FSaturation)*(Height/240));
 Invalidate;
end;

procedure THSColorPicker.Paint;
begin
 Canvas.StretchDraw(ClientRect, FHSLBmp);
 CorrectCoords(mxx, myy);
 DrawMarker(mxx, myy);
end;

procedure THSColorPicker.Resize;
begin
 SetSelectedColor(FSelected);
 inherited;
end;

procedure THSColorPicker.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
 R: TRect;
begin
 inherited;
 mxx := x;
 myy := y;
 if Button = mbLeft then
  begin
   R := ClientRect;
   R.TopLeft := ClientToScreen(R.TopLeft);
   R.BottomRight := ClientToScreen(R.BottomRight);
   ClipCursor(@R);
   FSelected := GetColorAtPoint(x, y);
   FManual := true;
   Invalidate;
  end;
 SetFocus;
end;

procedure THSColorPicker.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 inherited;
 ClipCursor(nil);
 mxx := x;
 myy := y;
 FSelected := GetColorAtPoint(x, y);
 FManual := true;
 Invalidate;
end;

procedure THSColorPicker.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
 inherited;
 if ssLeft in Shift then
  begin
   mxx := x;
   myy := y;
   FSelected := GetColorAtPoint(x, y);
   FManual := true;
   Invalidate;
  end;
end;

function THSColorPicker.PredictColor: TColor;
var
 FTHue, FTSat, FTLum: integer;
begin
 RGBtoHSLRange(GetColorUnderCursor, FTHue, FTSat, FTLum);
 Result := HSLRangeToRGB(FTHue, FTSat, FLum);
end;

procedure THSColorPicker.CNKeyDown(var Message: TWMKeyDown);
var
 Shift: TShiftState;
 FInherited: boolean;
begin
 FInherited := false;
 Shift := KeyDataToShiftState(Message.KeyData);
 if not (ssCtrl in Shift) then
  case Message.CharCode of
   VK_LEFT:
    begin
     mxx := dx - 1;
     myy := dy;
     FSelected := GetColorAtPoint(mxx, myy);
     FManual := true;
     Invalidate;
    end;
   VK_RIGHT:
    begin
     mxx := dx + 1;
     myy := dy;
     FSelected := GetColorAtPoint(mxx, myy);
     FManual := true;
     Invalidate;
    end;
   VK_UP:
    begin
     mxx := dx;
     myy := dy - 1;
     FSelected := GetColorAtPoint(mxx, myy);
     FManual := true;
     Invalidate;
    end;
   VK_DOWN:
    begin
     mxx := dx;
     myy := dy + 1;
     FSelected := GetColorAtPoint(mxx, myy);
     FManual := true;
     Invalidate;
    end;
  else
   begin
    FInherited := true;
    inherited;
   end;
  end
 else
  case Message.CharCode of
   VK_LEFT:
    begin
     mxx := dx - 10;
     myy := dy;
     Refresh;
     FSelected := GetColorAtPoint(mxx, myy);
     FManual := true;
     Invalidate;
    end;
   VK_RIGHT:
    begin
     mxx := dx + 10;
     myy := dy;
     Refresh;
     FSelected := GetColorAtPoint(mxx, myy);
     FManual := true;
     Invalidate;
    end;
   VK_UP:
    begin
     mxx := dx;
     myy := dy - 10;
     Refresh;
     FSelected := GetColorAtPoint(mxx, myy);
     FManual := true;
     Invalidate;
    end;
   VK_DOWN:
    begin
     mxx := dx;
     myy := dy + 10;
     Refresh;
     FSelected := GetColorAtPoint(mxx, myy);
     FManual := true;
     Invalidate;
    end;
  else
   begin
    FInherited := true;
    inherited;
   end;
  end;
 if not FInherited then
  if Assigned(OnKeyDown) then
   OnKeyDown(Self, Message.CharCode, Shift);
end;

procedure THSColorPicker.SetHValue(h: integer);
begin
 if h > 239 then h := 239;
 if h < 0 then h := 0;
 FHue := h;
 SetSelectedColor(HSLRangeToRGB(FHue, FSaturation, 120));
end;

procedure THSColorPicker.SetSValue(s: integer);
begin
 if s > 240 then s := 240;
 if s < 0 then s := 0;
 FSaturation := s;
 SetSelectedColor(HSLRangeToRGB(FHue, FSaturation, 120));
end;

function THSColorPicker.GetColorAtPoint(x, y: integer): TColor;
begin
 Result := Canvas.Pixels[x, y];
end;

procedure THSColorPicker.WebSafeChanged;
begin
 inherited;
 CreateHSLGradient;
 Invalidate;
end;

end.
