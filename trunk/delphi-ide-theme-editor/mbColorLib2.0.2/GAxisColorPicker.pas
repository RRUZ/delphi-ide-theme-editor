unit GAxisColorPicker;

interface

uses
 Windows, Messages, SysUtils, Classes, Controls, Graphics, Math, Forms,
 HTMLColors, SelPropUtils, mbColorPickerControl, Scanlines;

type
 TGAxisColorPicker = class(TmbColorPickerControl)
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
  property SelectedColor default clLime;
  property RValue: integer read FR write SetRValue default 0;
  property GValue: integer read FG write SetGValue default 255;
  property BValue: integer read FB write SetBValue default 0;
  property MarkerStyle default msCircle;

  property OnChange: TNotifyEvent read FOnChange write FOnChange;
 end;

procedure Register;

implementation

procedure Register;
begin
 RegisterComponents('mbColor Lib', [TGAxisColorPicker]);
end;

{TGAxisColorPicker}

constructor TGAxisColorPicker.Create(AOwner: TComponent);
begin
 inherited;
 FBmp := TBitmap.Create;
 FBmp.PixelFormat := pf32bit;
 FBmp.SetSize(256, 256);
 Width := 256;
 Height := 256;
 HintFormat := 'R: %r B: %b'#13'Hex: %hex';
 FG := 255;
 FB := 0;
 FR := 0;
 FSelected := clLime;
 FManual := false;
 dx := 0;
 dy := 0;
 mxx := 0;
 myy := 0;
 MarkerStyle := msCircle;
end;

destructor TGAxisColorPicker.Destroy;
begin
 FBmp.Free;
 inherited Destroy;
end;

procedure TGAxisColorPicker.CreateWnd;
begin
 inherited;
 CreateRGBGradient;
end;

procedure TGAxisColorPicker.CreateRGBGradient;
var
  r, b : integer;
  row: pRGBQuadArray;
begin
 if FBmp = nil then
  begin
   FBmp := TBitmap.Create;
   FBmp.PixelFormat := pf32bit;
   FBmp.Width := 256;
   FBmp.Height := 256;
  end;
 for r := 255 downto 0 do
  begin
   row := FBmp.Scanline[255-r];
   for b := 0 to 255 do
    if not WebSafe then
     row[b] := RGBtoRGBQuad(r, FG, b)
    else
     row[b] := RGBtoRGBQuad(GetWebSafe(RGB(r, FG, b)));
  end;
end;

procedure TGAxisColorPicker.CorrectCoords(var x, y: integer);
begin
 if x < 0 then x := 0;
 if y < 0 then y := 0;
 if x > Width - 1 then x := Width - 1;
 if y > Height - 1 then y := Height - 1;
end;

procedure TGAxisColorPicker.DrawMarker(x, y: integer);
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

function TGAxisColorPicker.GetSelectedColor: TColor;
begin
 Result := FSelected;
end;

procedure TGAxisColorPicker.SetSelectedColor(c: TColor);
begin
 if WebSafe then c := GetWebSafe(c);
 FR := GetRValue(c);
 FG := GetGValue(c);
 FB := GetBValue(c);
 FSelected := c;
 FManual := false;
 myy := Round((255-FR)*(Height/255));
 mxx := Round(FB*(Width/255));
 CreateRGBGradient;
 Invalidate;
end;

procedure TGAxisColorPicker.Paint;
begin
 Canvas.StretchDraw(ClientRect, FBmp);
 CorrectCoords(mxx, myy);
 DrawMarker(mxx, myy);
end;

procedure TGAxisColorPicker.Resize;
begin
 FManual := false;
 myy := Round((255-FR)*(Height/255));
 mxx := Round(FB*(Width/255));
 inherited;
end;

procedure TGAxisColorPicker.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
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

procedure TGAxisColorPicker.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 inherited;
 ClipCursor(nil);
 mxx := x;
 myy := y;
 FSelected := GetColorAtPoint(x, y);
 FManual := true;
 Invalidate;
end;

procedure TGAxisColorPicker.MouseMove(Shift: TShiftState; X, Y: Integer);
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

procedure TGAxisColorPicker.CNKeyDown(var Message: TWMKeyDown);
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

procedure TGAxisColorPicker.SetRValue(r: integer);
begin
 if r > 255 then r := 255;
 if r < 0 then r := 0;
 FR := r;
 SetSelectedColor(RGB(FR, FG, FB));
end;

procedure TGAxisColorPicker.SetGValue(g: integer);
begin
 if g > 255 then g := 255;
 if g < 0 then g := 0;
 FG := g;
 SetSelectedColor(RGB(FR, FG, FB));
end;

procedure TGAxisColorPicker.SetBValue(b: integer);
begin
 if b > 255 then b := 255;
 if b < 0 then b := 0;
 FB := b;
 SetSelectedColor(RGB(FR, FG, FB));
end;

function TGAxisColorPicker.GetColorAtPoint(x, y: integer): TColor;
begin
 Result := Canvas.Pixels[x, y];
end;

procedure TGAxisColorPicker.WebSafeChanged;
begin
 inherited;
 CreateRGBGradient;
 Invalidate;
end;

end.
