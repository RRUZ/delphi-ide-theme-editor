unit HSVColorPicker;

interface

{$I mxs.inc}

uses
  Windows, Messages, SysUtils, Classes, Controls, Graphics, Math, RGBHSVUtils, Scanlines,
  Forms, {IFDEF DELPHI_7_UP Themes, $ENDIF} HTMLColors, SelPropUtils, mbColorPickerControl;

type
  THSVColorPicker = class(TmbColorPickerControl)
  private
   FHue, FSat, FValue: integer;
   FSatCircColor, FHueLineColor: TColor;
   FSelectedColor: TColor;
   FOnChange: TNotifyEvent;
   FManual: boolean;
   FShowSatCirc: boolean;
   FShowHueLine: boolean;
   FShowSelCirc: boolean;
   Fchange: boolean;
   FHSVBmp: TBitmap;
   FDoChange: boolean;

   procedure CreateHSVCircle;
   function RadHue(New: integer): integer;
   procedure SetValue(V: integer);
   procedure SetHue(h: integer);
   procedure SetSat(s: integer);
   procedure SetSatCircColor(c: TColor);
   procedure SetHueLineColor(c: TColor);
   procedure DrawSatCirc;
   procedure DrawHueLine;
   procedure DrawMarker(x, y: integer);
   procedure SelectionChanged(x, y: integer);
   procedure SetShowSatCirc(s: boolean);
   procedure SetShowSelCirc(s: boolean);
   procedure SetShowHueLine(s: boolean);
   procedure UpdateCoords;
  protected
   function GetSelectedColor: TColor; override;
   procedure SetSelectedColor(c: TColor); override;
   procedure WebSafeChanged; override;
   procedure Paint; override;
   procedure Resize; override;
   procedure CreateWnd; override;
   procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
   procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
   procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
   procedure CNKeyDown(var Message: TWMKeyDown); message CN_KEYDOWN;
  public
   constructor Create(AOwner: TComponent); override;
   destructor Destroy; override;

   function GetColorAtPoint(x, y: integer): TColor; override;
   property Manual: boolean read FManual;
  published
   property Hue: integer read FHue write SetHue default 0;
   property Saturation: integer read FSat write SetSat default 0;
   property Value: integer read FValue write SetValue default 255;
   property SaturationCircleColor: TColor read FSatCircColor write SetSatCircColor default clSilver;
   property HueLineColor: TColor read FHueLineColor write SetHueLineColor default clGray;
   property SelectedColor default clNone;
   property ShowSaturationCircle: boolean read FShowSatCirc write SetShowSatCirc default true;
   property ShowHueLine: boolean read FShowHueLine write SetShowHueLine default true;
   property ShowSelectionCircle: boolean read FShowSelCirc write SetShowSelCirc default true;
   property MarkerStyle default msCrossCirc;

   property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

procedure Register;

implementation

{$IFDEF DELPHI_XE3_UP}
uses
 System.Types;
{$ENDIF}

procedure Register;
begin
  RegisterComponents('mbColor Lib', [THSVColorPicker]);
end;

function PointInCirc(p: TPoint; size : integer): boolean;
var
 r: integer;
begin
 r := size div 2;
 Result := (SQR(p.x - r) + SQR(p.y - r) <= SQR(r));
end;

constructor THSVColorPicker.Create(AOwner: TComponent);
begin
 inherited;
 FHSVBmp := TBitmap.Create;
 FHSVBmp.PixelFormat := pf32bit;
 Width := 204;
 Height := 204;
 FValue := 255;
 FHue := 0;
 FSat := 0;
 FSatCircColor := clSilver;
 FHueLineColor := clGray;
 FSelectedColor := clNone;
 FManual := false;
 FShowSatCirc := true;
 FShowHueLine := true;
 FShowSelCirc := true;
 Fchange := true;
 FDoChange := false;
 MarkerStyle := msCrossCirc;
end;

destructor THSVColorPicker.Destroy;
begin
 FHSVBmp.Free;
 inherited;
end;

procedure THSVColorPicker.Paint;
var
 rgn: HRGN;
 R: TRect;
begin
 PaintParentBack(Canvas);
 R := ClientRect;
 R.Right := R.Left + Min(Width, Height);
 R.Bottom := R.Top + Min(Width, Height);
 rgn := CreateEllipticRgnIndirect(R);
 SelectClipRgn(Canvas.Handle, rgn);
 Canvas.Draw(0, 0, FHSVBmp);
 DeleteObject(rgn);
 DrawSatCirc;
 DrawHueLine;
 DrawMarker(mdx, mdy);
 if FDoChange then
  begin
   if Assigned(FOnChange) then FOnChange(Self);
   FDoChange := false;
  end;
end;

procedure THSVColorPicker.CreateHSVCircle;
var
 dSquared, H, S, V, i, j, Radius, RadiusSquared, x, y, size: integer;
 row: pRGBQuadArray;
 tc: TColor;
begin
 if FHSVBmp = nil then
  begin
   FHSVBmp := TBitmap.Create;
   FHSVBmp.PixelFormat := pf32bit;
  end;
 size := Min(Width, Height);
 FHSVBmp.Width := size;
 FHSVBmp.Height := size;

 Radius := size div 2;
 RadiusSquared := Radius*Radius;
 PaintParentBack(FHSVBmp.Canvas);

 V := FValue;
 for j := 0 to size-1 do
  begin
   Y := Size - 1 - j - Radius;
   row := FHSVBmp.Scanline[Size - 1 - j];
   for i := 0 to size-1 do
    begin
     X := i - Radius;
     dSquared := X*X + Y*Y;
     if dSquared <= RadiusSquared then
      begin
       if Radius <> 0 then
        S := ROUND((255*SQRT(dSquared))/Radius)
       else
        S := 0;
       H := ROUND(180*(1 + ArcTan2(X, Y)/PI));
       H := H + 90;
       if H > 360 then H := H - 360;
       if not WebSafe then
        row[i] := HSVtoRGBQuad(H,S,V)
       else
        begin
         tc := GetWebSafe(HSVtoColor(H, S, V));
         row[i] := RGBtoRGBQuad(GetRValue(tc), GetGValue(tc), GetBValue(tc));
        end;
      end
    end;
  end;
end;

procedure THSVColorPicker.Resize;
begin
 inherited;
 CreateHSVCircle;
 UpdateCoords;
end;

procedure THSVColorPicker.CreateWnd;
begin
 inherited;
 CreateHSVCircle;
 UpdateCoords;
end;

procedure THSVColorPicker.UpdateCoords;
var
 r, angle: real;
 radius: integer;
begin
 radius := Min(Width, Height) div 2;
 r := -MulDiv(radius, FSat, 255);
 angle := -FHue*PI/180 - PI;
 mdx := ROUND(COS(angle)*ROUND(r)) + radius;
 mdy := ROUND(SIN(angle)*ROUND(r)) + radius;
end;

procedure THSVColorPicker.SetHue(h: integer);
begin
 if h > 360 then h := 360;
 if h < 0 then h := 0;
 if FHue <> h then
  begin
   FHue := h;
   FManual := false;
   UpdateCoords;
   Invalidate;
   if Fchange then
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure THSVColorPicker.SetSat(s: integer);
begin
 if s > 255 then s := 255;
 if s < 0 then s := 0;
 if FSat <> s then
  begin
   FSat := s;
   FManual := false;
   UpdateCoords;
   Invalidate;
   if Fchange then
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure THSVColorPicker.SetValue(V: integer);
begin
 if V > 255 then V := 255;
 if V < 0 then V := 0;
 if FValue <> V then
  begin
   FValue := V;
   FManual := false;
   CreateHSVCircle;
   Invalidate;
   if Fchange then
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure THSVColorPicker.SetSatCircColor(c: TColor);
begin
 if FSatCircColor <> c then
  begin
   FSatCircColor := c;
   Invalidate;
  end;
end;

procedure THSVColorPicker.SetHueLineColor(c: TColor);
begin
 if FHueLineColor <> c then
  begin
   FHueLineColor := c;
   Invalidate;
  end;
end;

procedure THSVColorPicker.SetShowSatCirc(s: boolean);
begin
 if FShowSatCirc <> s then
  begin
   FShowSatCirc := s;
   Invalidate;
  end;
end;

procedure THSVColorPicker.SetShowSelCirc(s: boolean);
begin
 if FShowSelCirc <> s then
  begin
   FShowSelCirc := s;
   Invalidate;
  end;
end;

procedure THSVColorPicker.SetShowHueLine(s: boolean);
begin
 if FShowHueLine <> s then
  begin
   FShowHueLine := s;
   Invalidate;
  end;
end;

procedure THSVColorPicker.DrawSatCirc;
var
 delta: integer;
 Radius: integer;
begin
 if not FShowSatCirc then Exit;
 if FSat in [1..254] then
  begin
   Radius:= Min(Width, Height) div 2;
   Canvas.Pen.Color := FSatCircColor;
   Canvas.Brush.Style := bsClear;
   delta := MulDiv(Radius, FSat, 255);
   Canvas.Ellipse(Radius - delta, Radius - delta, Radius + delta, Radius + delta);
  end;
end;

procedure THSVColorPicker.DrawHueLine;
var
 angle: double;
 radius: integer;
begin
 if not FShowHueLine then Exit;
 Radius := Min(Width, Height) div 2;
 if (FHue >= 0) and (FHue <= 360) then
  begin
   Angle := -FHue*PI/180;
   Canvas.Pen.Color := FHueLineColor;
   Canvas.MoveTo(Radius,Radius);
   Canvas.LineTo(Radius + Round(Radius*COS(angle)), Radius + Round(Radius*SIN(angle)));
  end;
end;

procedure THSVColorPicker.DrawMarker(x, y: integer);
var
 c: TColor;
begin
 if not FShowSelCirc then Exit;
 if Focused or (csDesigning in ComponentState) then
  c := clBlack
 else
  c := clGray;
 case MarkerStyle of
  msCircle: DrawSelCirc(x, y, Canvas);
  msSquare: DrawSelSquare(x, y, Canvas);
  msCross: DrawSelCross(x, y, Canvas, c);
  msCrossCirc: DrawSelCrossCirc(x, y, Canvas, c);
 end;
end;

procedure THSVColorPicker.SelectionChanged(x, y: integer);
var
 Angle, Distance, xDelta, yDelta, Radius: integer;
begin
 if not PointInCirc(Point(x, y), Min(Width, Height)) then
  begin
   FChange := false;
   SetSelectedColor(clNone);
   FChange := true;
   Exit;
  end
 else
  FSelectedColor := clWhite;
 Radius := Min(Width, Height) div 2;
 xDelta := x - Radius;
 yDelta := y - Radius;
 Angle := ROUND(360 + 180*ArcTan2(-yDelta,xDelta)/PI);
 if Angle < 0 then Inc(Angle, 360)
 else if Angle > 360 then
  Dec(Angle, 360);
 Fchange := false;
 SetHue(Angle);
 Distance := ROUND(SQRT(SQR(xDelta) + SQR(yDelta)));
 if  Distance >= Radius then SetSat(255)
 else SetSat(MulDiv(Distance, 255, Radius));
 Fchange := true;
end;

procedure THSVColorPicker.MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
begin
 inherited;
 ClipCursor(nil);
 if csDesigning in ComponentState then Exit;
 if (Button = mbLeft) and PointInCirc(Point(x, y), Min(Width, Height)) then
  begin
   mdx := x;
   mdy := y;
   FDoChange := true;
   SelectionChanged(X, Y);
   FManual := true;
  end;
end;

procedure THSVColorPicker.MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
var
 R: TRect;
begin
 inherited;
 if csDesigning in ComponentState then Exit;
 if (Button = mbLeft) and PointInCirc(Point(x, y), Min(Width, Height)) then
  begin
   mdx := x;
   mdy := y;
   R := ClientRect;
   InflateRect(R, 1, 1);
   R.TopLeft := ClientToScreen(R.TopLeft);
   R.BottomRight := ClientToScreen(R.BottomRight);
   ClipCursor(@R);
   FDoChange := true;
   SelectionChanged(X, Y);
   FManual := true;
  end;
 SetFocus;
end;

procedure THSVColorPicker.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
 inherited;
 if csDesigning in ComponentState then Exit;
 if (ssLeft in Shift) and PointInCirc(Point(x, y), Min(Width, Height)) then
  begin
   mdx := x;
   mdy := y;
   FDoChange := true;
   SelectionChanged(X, Y);
   FManual := true;
  end;
end;

function THSVColorPicker.GetSelectedColor: TColor;
begin
 if FSelectedColor <> clNone then
  begin
   if not WebSafe then
    Result := HSVtoColor(FHue, FSat, FValue)
   else
    Result := GetWebSafe(HSVtoColor(FHue, FSat, FValue));
  end
 else
  Result := clNone;
end;

function THSVColorPicker.GetColorAtPoint(x, y: integer): TColor;
var
 Angle, Distance, xDelta, yDelta, Radius: integer;
 h, s: integer;
begin
 Radius := Min(Width, Height) div 2;
 xDelta := x - Radius;
 yDelta := y - Radius;
 Angle := ROUND(360 + 180*ArcTan2(-yDelta,xDelta)/PI);
 if Angle < 0 then Inc(Angle, 360)
 else if Angle > 360 then
  Dec(Angle, 360);
 h := Angle;
 Distance := ROUND(SQRT(SQR(xDelta) + SQR(yDelta)));
 if  Distance >= Radius then s := 255
 else s := MulDiv(Distance, 255, Radius);
 if PointInCirc(Point(mx, my), Min(Width, Height)) then
  begin
   if not WebSafe then
    Result := HSVtoColor(h, s, FValue)
   else
    Result := GetWebSafe(HSVtoColor(h, s, FValue));
  end
 else
  Result := clNone;
end;

procedure THSVColorPicker.SetSelectedColor(c: TColor);
var
 changeSave: boolean;
begin
 if WebSafe then c := GetWebSafe(c);
 changeSave := FChange;
 FManual := false;
 Fchange := false;
 SetValue(GetVValue(c));
 SetHue(GetHValue(c));
 SetSat(GetSValue(c));
 FSelectedColor := c;
 Fchange := changeSave;
 if Fchange then
  if Assigned(FOnChange) then FOnChange(Self);
 FChange := true;
end;

function THSVColorPicker.RadHue(New: integer): integer;
begin
 if New < 0 then New := New + 360;
 if New > 360 then New := New - 360;
 Result := New;
end;

procedure THSVColorPicker.CNKeyDown(var Message: TWMKeyDown);
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
      FChange := false;
      SetHue(RadHue(FHue + 1));
      FChange := true;
      FManual := true;
      if Assigned(FOnChange) then FOnChange(Self);
     end;
   VK_RIGHT:
     begin
      FChange := false;
      SetHue(RadHue(FHue - 1));
      FChange := true;
      FManual := true;
      if Assigned(FOnChange) then FOnChange(Self);
     end;
   VK_UP:
     begin
      FChange := false;
      if FSat + 1 <= 255 then
       SetSat(FSat + 1);
      FChange := true;
      FManual := true;
      if Assigned(FOnChange) then FOnChange(Self);
     end;
   VK_DOWN:
     begin
      FChange := false;
      if FSat - 1 >= 0  then
       SetSat(FSat - 1);
      FChange := true;
      FManual := true;
      if Assigned(FOnChange) then FOnChange(Self);
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
      FChange := false;
      SetHue(RadHue(FHue + 10));
      FChange := true;
      FManual := true;
      if Assigned(FOnChange) then FOnChange(Self);
     end;
   VK_RIGHT:
     begin
      FChange := false;
      SetHue(RadHue(FHue - 10));
      FChange := true;
      FManual := true;
      if Assigned(FOnChange) then FOnChange(Self);
     end;
   VK_UP:
     begin
      FChange := false;
      if FSat + 10 <= 255 then
       SetSat(FSat + 10);
      FChange := true;
      FManual := true;
      if Assigned(FOnChange) then FOnChange(Self);
     end;
   VK_DOWN:
     begin
      FChange := false;
      if FSat - 10 >= 0 then
       SetSat(FSat - 10);
      FChange := true;
      FManual := true;
      if Assigned(FOnChange) then FOnChange(Self);
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

procedure THSVColorPicker.WebSafeChanged;
begin
 inherited;
 CreateHSVCircle;
 Invalidate;
end;

end.
