unit CIELColorPicker;

interface

uses
 Windows, Messages, SysUtils, Classes, Controls, Graphics, Math, Forms,
 HTMLColors, SelPropUtils, mbColorPickerControl, RGBCIEUtils, Scanlines;

type
 TCIELColorPicker = class(TmbColorPickerControl)
 private
  FSelected: TColor;
  FBmp: TBitmap;
  FOnChange: TNotifyEvent;
  FL, FA, FB: integer;
  FManual: boolean;
  dx, dy, mxx, myy: integer;

  procedure SetLValue(l: integer);
  procedure SetAValue(a: integer);
  procedure SetBValue(b: integer);
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
  procedure CreateLABGradient;
  procedure Resize; override;
  procedure CreateWnd; override;
  procedure CorrectCoords(var x, y: integer);
 public
  constructor Create(AOwner: TComponent); override;
  destructor Destroy; override;

  function GetColorAtPoint(x, y: integer): TColor; override;
  property Manual: boolean read FManual;
 published
  property SelectedColor default clAqua;
  property LValue: integer read FL write SetLValue default 100;
  property AValue: integer read FA write SetAValue default -128;
  property BValue: integer read FB write SetBValue default 127;
  property MarkerStyle default msCircle;

  property OnChange: TNotifyEvent read FOnChange write FOnChange;
 end;

procedure Register;

implementation

procedure Register;
begin
 RegisterComponents('mbColor Lib', [TCIELColorPicker]);
end;

{TCIELColorPicker}

constructor TCIELColorPicker.Create(AOwner: TComponent);
begin
 inherited;
 FBmp := TBitmap.Create;
 FBmp.PixelFormat := pf32bit;
 FBmp.SetSize(256, 256);
 Width := 256;
 Height := 256;
 HintFormat := 'A: %cieA B: %cieB'#13'Hex: %hex';
 FSelected := clAqua;
 FL := 100;
 FA := -128;
 FB := 127;
 FManual := false;
 dx := 0;
 dy := 0;
 mxx := 0;
 myy := 0;
 MarkerStyle := msCircle;
end;

destructor TCIELColorPicker.Destroy;
begin
 FBmp.Free;
 inherited Destroy;
end;

procedure TCIELColorPicker.CreateWnd;
begin
 inherited;
 CreateLABGradient;
end;

procedure TCIELColorPicker.CreateLABGradient;
var
  a, b: integer;
  row: pRGBQuadArray;
begin
 if FBmp = nil then
  begin
   FBmp := TBitmap.Create;
   FBmp.PixelFormat := pf32bit;
   FBmp.Width := 256;
   FBmp.Height := 256;
  end;

 for a := 0 to 255 do
  for b := 255 downto 0 do
   begin
    row := FBmp.Scanline[255 - b];
    if not WebSafe then
     row[a] := RGBToRGBQuad(LabToRGB(FL, a - 128, b - 128))
//     FBmp.Canvas.Pixels[a, 255 - b] := LabToRGB(FL, a - 128, b - 128)
    else
     row[a] := RGBToRGBQuad(GetWebSafe(LabToRGB(FL, a - 128, b - 128)));
//     FBmp.Canvas.Pixels[a, 255 - b] := GetWebSafe(LabToRGB(FL, a - 128, b - 128));
   end;
end;

procedure TCIELColorPicker.CorrectCoords(var x, y: integer);
begin
 if x < 0 then x := 0;
 if y < 0 then y := 0;
 if x > Width - 1 then x := Width - 1;
 if y > Height - 1 then y := Height - 1;
end;

procedure TCIELColorPicker.DrawMarker(x, y: integer);
var
 c: TColor;
begin
 CorrectCoords(x, y);
 FL := Round(GetCIELValue(FSelected));
 FA := Round(GetCIEAValue(FSelected));
 FB := Round(GetCIEBValue(FSelected));
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

function TCIELColorPicker.GetSelectedColor: TColor;
begin
 Result := FSelected;
end;

procedure TCIELColorPicker.SetSelectedColor(c: TColor);
begin
 if WebSafe then c := GetWebSafe(c);
 FL := Round(GetCIELValue(c));
 FA := Round(GetCIEAValue(c));
 FB := Round(GetCIEBValue(c));
 FSelected := c;
 FManual := false;
 mxx := Round((FA+128)*(Width/255));
 myy := Round((255-(FB+128))*(Height/255));
 CreateLABGradient;
 Invalidate;
end;

procedure TCIELColorPicker.Paint;
begin
 Canvas.StretchDraw(ClientRect, FBmp);
 CorrectCoords(mxx, myy);
 DrawMarker(mxx, myy);
end;

procedure TCIELColorPicker.Resize;
begin
 FManual := false;
 mxx := Round((FA+128)*(Width/255));
 myy := Round((255-(FB+128))*(Height/255));
 inherited;
end;

procedure TCIELColorPicker.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
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

procedure TCIELColorPicker.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 inherited;
 ClipCursor(nil);
 mxx := x;
 myy := y;
 FSelected := GetColorAtPoint(x, y);
 FManual := true;
 Invalidate;
end;

procedure TCIELColorPicker.MouseMove(Shift: TShiftState; X, Y: Integer);
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

procedure TCIELColorPicker.CNKeyDown(var Message: TWMKeyDown);
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

procedure TCIELColorPicker.SetLValue(l: integer);
begin
 if l > 100 then l := 100;
 if l < 0 then l := 0;
 FL := l;
 SetSelectedColor(LabToRGB(FL, FA, FB));
end;

procedure TCIELColorPicker.SetAValue(a: integer);
begin
 if a > 127 then a := 127;
 if a < -128 then a := -128;
 FA := a;
 SetSelectedColor(LabToRGB(FL, FA, FB));
end;

procedure TCIELColorPicker.SetBValue(b: integer);
begin
 if b > 127 then b := 127;
 if b < -128 then b := -128;
 FB := b;
 SetSelectedColor(LabToRGB(FL, FA, FB));
end;

function TCIELColorPicker.GetColorAtPoint(x, y: integer): TColor;
begin
 Result := Canvas.Pixels[x, y];
end;

procedure TCIELColorPicker.WebSafeChanged;
begin
 inherited;
 CreateLABGradient;
 Invalidate;
end;

end.
