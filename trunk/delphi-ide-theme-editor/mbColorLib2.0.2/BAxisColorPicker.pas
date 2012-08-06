unit BAxisColorPicker;

interface

uses
 Windows, Messages, SysUtils, Classes, Controls, Graphics, Math, Forms,
 HTMLColors, SelPropUtils, mbColorPickerControl, Scanlines;

type
 TBAxisColorPicker = class(TmbColorPickerControl)
 private
  FSelected: TColor;
  FBmp: TBitmap;
  FOnChange: TNotifyEvent;
  FR, FG, FB: integer;
  FManual: boolean;
  dx, dy, mxx, myy: integer;

  procedure SetRValue(r: integer);
  procedure SetGValue(g: integer);
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
  procedure CreateRGBGradient;
  procedure Resize; override;
  procedure CreateWnd; override;
  procedure CorrectCoords(var x, y: integer);
 public
  constructor Create(AOwner: TComponent); override;
  destructor Destroy; override;

  function GetColorAtPoint(x, y: integer): TColor; override;
  property Manual: boolean read FManual;
 published
  property SelectedColor default clBlue;
  property RValue: integer read FR write SetRValue default 0;
  property GValue: integer read FG write SetGValue default 0;
  property BValue: integer read FB write SetBValue default 255;
  property MarkerStyle default msCircle;

  property OnChange: TNotifyEvent read FOnChange write FOnChange;
 end;

procedure Register;

implementation

procedure Register;
begin
 RegisterComponents('mbColor Lib', [TBAxisColorPicker]);
end;

{TBAxisColorPicker}

constructor TBAxisColorPicker.Create(AOwner: TComponent);
begin
 inherited;
 FBmp := TBitmap.Create;
 FBmp.PixelFormat := pf32bit;
 FBmp.SetSize(256, 256);
 Width := 256;
 Height := 256;
 HintFormat := 'R: %r G: %g'#13'Hex: %hex';
 FG := 0;
 FB := 255;
 FR := 0;
 FSelected := clBlue;
 FManual := false;
 dx := 0;
 dy := 0;
 mxx := 0;
 myy := 0;
 MarkerStyle := msCircle;
end;

destructor TBAxisColorPicker.Destroy;
begin
 FBmp.Free;
 inherited Destroy;
end;

procedure TBAxisColorPicker.CreateWnd;
begin
 inherited;
 CreateRGBGradient;
end;

procedure TBAxisColorPicker.CreateRGBGradient;
var
  r, g: integer;
  row: pRGBQuadArray;
begin
 if FBmp = nil then
  begin
   FBmp := TBitmap.Create;
   FBmp.PixelFormat := pf32bit;
   FBmp.Width := 256;
   FBmp.Height := 256;
  end;

 for g := 0 to 255 do
  begin
   row := FBmp.ScanLine[255 - g];
   for r := 0 to 255 do
    if not WebSafe then
     row[r] := RGBtoRGBQuad(r, g, FB)
    else
     row[r] := RGBtoRGBQuad(GetWebSafe(RGB(r, g, FB)));
  end;
end;

procedure TBAxisColorPicker.CorrectCoords(var x, y: integer);
begin
 if x < 0 then x := 0;
 if y < 0 then y := 0;
 if x > Width - 1 then x := Width - 1;
 if y > Height - 1 then y := Height - 1;
end;

procedure TBAxisColorPicker.DrawMarker(x, y: integer);
var
 c: TColor;
begin
 CorrectCoords(x, y);
 FR := GetRValue(FSelected);
 FG := GetGValue(FSelected);
 FB := GetBValue(FSelected);
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

function TBAxisColorPicker.GetSelectedColor: TColor;
begin
 Result := FSelected;
end;

procedure TBAxisColorPicker.SetSelectedColor(c: TColor);
begin
 if WebSafe then c := GetWebSafe(c);
 FR := GetRValue(c);
 FG := GetGValue(c);
 FB := GetBValue(c);
 FSelected := c;
 FManual := false;
 mxx := Round(FR*(Width/255));
 myy := Round((255-FG)*(Height/255));
 CreateRGBGradient;
 Invalidate;
end;

procedure TBAxisColorPicker.Paint;
begin
 Canvas.StretchDraw(ClientRect, FBmp);
 CorrectCoords(mxx, myy);
 DrawMarker(mxx, myy);
end;

procedure TBAxisColorPicker.Resize;
begin
 FManual := false;
 mxx := Round(FR*(Width/255));
 myy := Round((255-FG)*(Height/255));
 inherited;
end;

procedure TBAxisColorPicker.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
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

procedure TBAxisColorPicker.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 inherited;
 ClipCursor(nil);
 mxx := x;
 myy := y;
 FSelected := GetColorAtPoint(x, y);
 FManual := true;
 Invalidate;
end;

procedure TBAxisColorPicker.MouseMove(Shift: TShiftState; X, Y: Integer);
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

procedure TBAxisColorPicker.CNKeyDown(var Message: TWMKeyDown);
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

procedure TBAxisColorPicker.SetRValue(r: integer);
begin
 if r > 255 then r := 255;
 if r < 0 then r := 0;
 FR := r;
 SetSelectedColor(RGB(FR, FG, FB));
end;

procedure TBAxisColorPicker.SetGValue(g: integer);
begin
 if g > 255 then g := 255;
 if g < 0 then g := 0;
 FG := g;
 SetSelectedColor(RGB(FR, FG, FB));
end;

procedure TBAxisColorPicker.SetBValue(b: integer);
begin
 if b > 255 then b := 255;
 if b < 0 then b := 0;
 FB := b;
 SetSelectedColor(RGB(FR, FG, FB));
end;

function TBAxisColorPicker.GetColorAtPoint(x, y: integer): TColor;
begin
 Result := Canvas.Pixels[x, y];
end;

procedure TBAxisColorPicker.WebSafeChanged;
begin
 inherited;
 CreateRGBGradient;
 Invalidate;
end;

end.
