unit mbTrackBarPicker;

interface

{$I mxs.inc}

uses
 Windows, Messages, SysUtils, Classes, Controls, Graphics, Forms,
 {$IFDEF DELPHI_7_UP} Themes, {$ENDIF} ExtCtrls, PalUtils;

const
 TBA_Resize = 0;
 TBA_Paint = 1;
 TBA_MouseMove = 2;
 TBA_MouseDown = 3;
 TBA_MouseUp = 4;
 TBA_WheelUp = 5;
 TBA_WheelDown = 6;
 TBA_VKUp = 7;
 TBA_VKCtrlUp = 8;
 TBA_VKDown = 9;
 TBA_VKCtrlDown = 10;
 TBA_VKLeft = 11;
 TBA_VKCtrlLeft = 12;
 TBA_VKRight = 13;
 TBA_VKCtrlRight = 14;
 TBA_RedoBMP = 15;

type
 TTrackBarLayout = (lyHorizontal, lyVertical);
 TSliderPlacement = (spBefore, spAfter, spBoth);
 TSelIndicator = (siArrows, siRect);

 TmbTrackBarPicker = class(TCustomControl)
 private
  mx, my: integer;
  FOnChange: TNotifyEvent;
  FIncrement: integer;
  FHintFormat: string;
  FLayout: TTrackBarLayout;
  FPlacement: TSliderPlacement;
  FNewArrowStyle: boolean;
  Aw, Ah: integer;
  FDoChange: boolean;
  FSelIndicator: TSelIndicator;
  FWebSafe: boolean;
  FBevelInner: TBevelCut;
  FBevelOuter: TBevelCut;
  FBevelWidth: TBevelWidth;
  FBorderStyle: TBorderStyle;

  procedure SetBevelInner(Value: TBevelCut);
  procedure SetBevelOuter(Value: TBevelCut);
  procedure SetBevelWidth(Value: TBevelWidth);
  procedure SetBorderStyle(Value: TBorderStyle);
  procedure SetWebSafe(s: boolean);
  function XToArrowPos(p: integer): integer;
  function YToArrowPos(p: integer): integer;
  procedure SetLayout(Value: TTrackBarLayout);
  procedure SetNewArrowStyle(s: boolean);
  procedure SetPlacement(Value: TSliderPlacement);
  procedure DrawMarker(p: integer);
  procedure SetSelIndicator(Value: TSelIndicator);
  procedure PaintParentBack;
  procedure CalcPickRect;
 protected
  FArrowPos: integer;
  FManual: boolean;
  FChange: boolean;
  FPickRect: TRect;
  FLimit: integer;

  procedure WheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
  procedure WheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
  procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
  procedure CMHintShow(var Message: TCMHintShow); message CM_HINTSHOW;
  procedure CNKeyDown(var Message: TWMKeyDown); message CN_KEYDOWN;
  procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
  procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  procedure CMGotFocus(var Message: TCMGotFocus); message CM_ENTER;
  procedure CMLostFocus(var Message: TCMLostFocus); message CM_EXIT;
  procedure Paint; override;
  procedure DrawFrames; dynamic;
  procedure Resize; override;
  procedure CreateWnd; override;
  procedure Execute(tbaAction: integer); dynamic;
  function GetArrowPos: integer; dynamic;
  function GetHintStr: string;
  function GetSelectedValue: integer; virtual; abstract;
 public
  constructor Create(AOwner: TComponent); override;
  property Manual: boolean read FManual;
 published
  property BevelInner: TPanelBevel read FBevelInner write SetBevelInner default bvNone;
  property BevelOuter: TPanelBevel read FBevelOuter write SetBevelOuter default bvNone;
  property BevelWidth: TBevelWidth read FBevelWidth write SetBevelWidth default 1;
  property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsNone;

  property HintFormat: string read FHintFormat write FHintFormat;
  property Increment: integer read FIncrement write FIncrement default 1;
  property Layout: TTrackBarLayout read FLayout write SetLayout default lyHorizontal;
  property ArrowPlacement: TSliderPlacement read FPlacement write SetPlacement default spAfter;
  property NewArrowStyle: boolean read FNewArrowStyle write SetNewArrowStyle default false;
  property SelectionIndicator: TSelIndicator read FSelIndicator write SetSelIndicator default siArrows;
  property WebSafe: boolean read FWebSafe write SetWebSafe default false;
  property TabStop default true;
  property ShowHint;
  property Color;
  property ParentColor default true;
  {$IFDEF DELPHI_7_UP}
  property ParentBackground default true;
  {$ENDIF}
  property ParentShowHint default true;
  property Anchors;
  property Align;
  property Visible;
  property Enabled;
  property PopupMenu;
  property TabOrder;
  property DragCursor;
  property DragMode;
  property DragKind;
  property Constraints;

  property OnChange: TNotifyEvent read FOnChange write FOnChange;
  property OnContextPopup;
  property OnMouseDown;
  property OnMouseMove;
  property OnMouseUp;
  property OnMouseWheel;
  property OnMouseWheelUp;
  property OnMouseWheelDown;
  property OnKeyDown;
  property OnKeyPress;
  property OnKeyUp;
  property OnDragDrop;
  property OnDragOver;
  property OnEndDrag;
  property OnEnter;
  property OnExit;
  property OnResize;
  property OnStartDrag;
 end;

implementation

{$IFDEF DELPHI_XE3_UP}
uses
 System.Types;
{$ENDIF}


{TmbTrackBarPicker}

constructor TmbTrackBarPicker.Create(AOwner: TComponent);
begin
 inherited;
 ControlStyle := ControlStyle - [csAcceptsControls] + [csOpaque];
 DoubleBuffered := true;
 ParentColor := true;
 {$IFDEF DELPHI_7_UP}
 ParentBackground := true;
 {$ENDIF}
 Width := 267;
 Height := 22;
 TabStop := true;
 ParentShowHint := true;
 mx := 0;
 my := 0;
 FIncrement := 1;
 FArrowPos := GetArrowPos;
 FHintFormat := '';
 OnMouseWheelUp := WheelUp;
 OnMouseWheelDown := WheelDown;
 FManual := false;
 FChange := true;
 FLayout := lyHorizontal;
 FNewArrowStyle := false;
 Aw := 6;
 Ah := 10;
 FPlacement := spAfter;
 FPickRect := Rect(Aw, 0, Width - Aw, Height - Ah);
 FDoChange := false;
 FSelIndicator := siArrows;
 FLimit := 7;
 FWebSafe := false;
 FBevelInner:= bvNone;
 FBevelOuter:= bvNone;
 FBevelWidth:= 1;
 FBorderStyle:= bsNone;
end;

procedure TmbTrackBarPicker.CreateWnd;
begin
 inherited;
 CalcPickRect;
end;

procedure TmbTrackBarPicker.CalcPickRect;
var
 f: integer;
begin
 case FSelIndicator of
  siArrows:
   if not FNewArrowStyle then
    begin
     f := 0;
     Aw := 6;
     Ah := 10;
     FLimit := 7;
    end
   else
    begin
     Aw := 8;
     Ah := 9;
     f := 2;
     FLimit := 7;
    end;
  siRect:
   begin
    f := 0;
    Aw := 4;
    Ah := 5;
    FLimit := 3;
   end
  else
   f := 0;
 end;
 case FLayout of
  lyHorizontal:
   case FSelIndicator of
    siArrows:
     case FPlacement of
      spAfter: FPickRect := Rect(Aw, 0, Width - Aw, Height - Ah - f);
      spBefore: FPickRect := Rect(Aw, Ah + f, Width - Aw, Height);
      spBoth: FPickRect := Rect(Aw, Ah + f, Width - Aw, Height - Ah - f);
     end;
    siRect: FPickRect := Rect(Aw, Ah, width - 2*Aw + 1, height - Ah);
   end;
  lyVertical:
   case FSelIndicator of
    siArrows:
     case FPlacement of
      spAfter: FPickRect := Rect(0, Aw, Width - Ah - f, Height - Aw);
      spBefore: FPickRect := Rect(Ah + f, Aw, Width, Height - Aw);
      spBoth: FPickRect := Rect(Ah + f, Aw, Width - Ah - f, Height - Aw);
     end;
    siRect: FPickRect := Rect(Ah, Aw, width - 5, height - 2*Aw + 1);
   end;
 end;
end;

procedure TmbTrackBarPicker.Paint;
begin
 CalcPickRect;
 PaintParentBack;
 FArrowPos := GetArrowPos;
 Execute(TBA_Paint);
 if FBorderStyle <> bsNone then
  DrawFrames;
 DrawMarker(FArrowPos);
 if FDoChange then
  begin
   if Assigned(FOnChange) then FOnChange(Self);
   FDoChange := false;
  end;
end;

procedure TmbTrackBarPicker.DrawFrames;
var
 flags: cardinal;
 R: TRect;
 i: integer;
begin
 flags := 0;
 if (FBorderStyle = bsNone) or (FBevelWidth = 0) then Exit;
 case FBevelInner of
  bvNone: flags := 0;
  bvRaised: flags := BDR_RAISEDINNER;
  bvLowered: flags := BDR_SUNKENINNER;
  bvSpace: flags := BDR_INNER;
 end;
 case FBevelOuter of
  bvRaised: flags := flags or BDR_RAISEDOUTER;
  bvLowered: flags := flags or BDR_SUNKENOUTER;
  bvSpace: flags := flags or BDR_OUTER;
 end;
 R := FPickRect;
 InflateRect(R, -FBevelWidth + 1, -FBevelWidth + 1);
 for i := 0 to FBevelWidth do
  begin
   DrawEdge(Canvas.Handle, R, flags, BF_RECT);
   InflateRect(R, 1, 1);
  end;
end;

procedure TmbTrackBarPicker.DrawMarker(p: integer);
var
 x, y: integer;
 R: TRect;
begin
 case FSelIndicator of
  siRect:
   begin
    case FLayout of
     lyHorizontal:
      begin
       p := p + Aw;
       R := Rect(p - 2, 2, p + 3, Height - 2);
      end;
     lyVertical:
      begin
       p := p + Aw;
       R := Rect(2, p - 2, Width - 2, p + 3);
      end;
    end;
    Canvas.Pen.Mode := pmNot;
    Canvas.Brush.Style := bsClear;
    Canvas.Rectangle(R);
    Canvas.Brush.Style := bsSolid;
    Canvas.Pen.Mode := pmCopy;
   end;
  siArrows:
   begin
    if not FNewArrowStyle then
     begin
      if Focused or (csDesigning in ComponentState)then
       begin
        Canvas.Brush.Color := clBlack;
        Canvas.Pen.Color := clBlack;
       end
      else
       begin
        Canvas.Brush.Color := clGray;
        Canvas.Pen.Color := clGray;
       end;
     end
    else
     begin
      Canvas.Brush.Color := clWindow;
      Canvas.Pen.Color := clBtnShadow;
     end;
    if FLayout = lyHorizontal then
     begin
      x := p + Aw;
      if x < Aw then x := Aw;
      if x > Width - Aw then x := Width - Aw;
      case FPlacement of
       spAfter:
        begin
         y := Height - Aw - 1;
         if not FNewArrowStyle then
          Canvas.Polygon([Point(x, y), Point(x - 4, y + 6), Point(x + 4, y + 6)])
         else
          Canvas.Polygon([Point(x, y), Point(x - 4, y + 4), Point(x - 4, y + 6),
                          Point(x - 3, y + 7), Point(x + 3, y + 7),
                          Point(x + 4, y + 6), Point(x + 4, y + 4)]);
        end;
       spBefore:
        begin
         y := Aw;
         if not FNewArrowStyle then
          Canvas.Polygon([Point(x, y), Point(x - 4, y - 6), Point(x +4, y - 6)])
         else
          Canvas.Polygon([Point(x, y), Point(x + 4, y - 4), Point(x + 4, y - 6),
                          Point(x + 3, y - 7), Point(x - 3, y - 7),
                          Point(x - 4, y - 6), Point(x - 4, y - 4)]);
        end;
       spBoth:
        begin
         y := Height - Aw - 1;
         if not FNewArrowStyle then
          Canvas.Polygon([Point(x, y), Point(x -4, y +6), Point(x +4, y + 6)])
         else
          Canvas.Polygon([Point(x, y), Point(x - 4, y + 4), Point(x - 4, y + 6),
                          Point(x - 3, y + 7), Point(x + 3, y + 7),
                          Point(x + 4, y + 6), Point(x + 4, y + 4)]);
         y := Aw;
         if not FNewArrowStyle then
          Canvas.Polygon([Point(x, y), Point(x - 4, y - 6), Point(x +4, y - 6)])
         else
          Canvas.Polygon([Point(x, y), Point(x + 4, y - 4), Point(x + 4, y - 6),
                          Point(x + 3, y - 7), Point(x - 3, y - 7),
                          Point(x - 4, y - 6), Point(x - 4, y - 4)]);
        end;
      end;
     end
    else
     begin
      if not FNewArrowStyle then
       y := p + Aw
      else
       y := p + Aw - 1;
      if y < Aw then y := Aw;
      if y > Height - Aw - 1 then y := Height - Aw - 1;
      case FPlacement of
       spAfter:
        begin
         x := width - Aw - 1;
         if not FNewArrowStyle then
          Canvas.Polygon([Point(x, y), Point(x + 6, y - 4), Point(x + 6, y + 4)])
         else
          Canvas.Polygon([Point(x, y), Point(x + 4, y - 4), Point(x + 6, y - 4),
                          Point(x + 7, y - 3), Point(x + 7, y + 3),
                          Point(x + 6, y + 4), Point(x + 4, y + 4)]);
        end;
       spBefore:
        begin
         x := Aw;
         if not FNewArrowStyle then
          Canvas.Polygon([Point(x, y), Point(x - 6, y - 4), Point(x - 6, y + 4)])
         else
          Canvas.Polygon([Point(x, y), Point(x - 4, y - 4), Point(x - 6, y - 4),
                          Point(x - 7, y + 1 - 4), Point(x - 7, y + 3),
                          Point(x - 6, y + 4), Point(x - 4, y + 4)]);
        end;
       spBoth:
        begin
         x := width - Aw - 1;
         if not FNewArrowStyle then
          Canvas.Polygon([Point(x, y), Point(x + 6, y - 4), Point(x + 6, y + 4)])
         else
          Canvas.Polygon([Point(x, y), Point(x + 4, y - 4), Point(x + 6, y - 4),
                          Point(x + 7, y - 3), Point(x + 7, y + 3),
                          Point(x + 6, y + 4), Point(x + 4, y + 4)]);
         x := Aw;
         if not FNewArrowStyle then
          Canvas.Polygon([Point(x, y), Point(x - 6, y - 4), Point(x - 6, y + 4)])
         else
          Canvas.Polygon([Point(x, y), Point(x - 4, y - 4), Point(x - 6, y - 4),
                          Point(x - 7, y + 1 - 4), Point(x - 7, y + 3),
                          Point(x - 6, y + 4), Point(x - 4, y + 4)]);
        end;
      end;
     end;
    end;
 end;
end;

procedure TmbTrackBarPicker.Resize;
begin
 inherited;
 FChange := false;
 Execute(TBA_Resize);
 FChange := true;
end;

procedure TmbTrackBarPicker.PaintParentBack;
var
 OffScreen: TBitmap;
 {$IFDEF DELPHI_7_UP}
 MemDC: HDC;
 OldBMP: HBITMAP;
 {$ENDIF}
begin
 Offscreen := TBitmap.Create;
 Offscreen.Width := Width;
 Offscreen.Height := Height;
 Offscreen.Canvas.Brush.Color := Color;
 Offscreen.Canvas.FillRect(Offscreen.Canvas.ClipRect);
 {$IFDEF DELPHI_7_UP}
 if ParentBackground then
  with {$IFDEF DELPHI_XE2_UP}StyleServices{$ELSE}ThemeServices{$ENDIF} do
   if {$IFDEF DELPHI_XE2_UP}Enabled{$ELSE}ThemesEnabled{$ENDIF} then
    begin
     MemDC := CreateCompatibleDC(0);
     OldBMP := SelectObject(MemDC, OffScreen.Handle);
     DrawParentBackground(Handle, MemDC, nil, False);
     if OldBMP <> 0 then SelectObject(MemDC, OldBMP);
     if MemDC <> 0 then DeleteDC(MemDC);
    end;
 {$ENDIF}
 Canvas.Draw(0, 0, Offscreen);
 Offscreen.Free;
end;

function TmbTrackBarPicker.XToArrowPos(p: integer): integer;
var
 pos: integer;
begin
 pos := p - Aw;
 if pos < 0 then pos := 0;
 if pos > Width - Aw - 1 then pos := Width - Aw - 1;
 Result := pos;
end;

function TmbTrackBarPicker.YToArrowPos(p: integer): integer;
var
 pos: integer;
begin
 pos := p - Aw;
 if pos < 0 then pos := 0;
 if pos > Height - Aw - 1 then pos := Height - Aw - 1;
 Result := pos;
end;

procedure TmbTrackBarPicker.MouseMove(Shift: TShiftState; X, Y: Integer);
var
 R: TRect;
begin
 if ssLeft in shift then
  begin
   R := ClientRect;
   R.TopLeft := ClientToScreen(R.TopLeft);
   R.BottomRight := ClientToScreen(R.BottomRight);
   ClipCursor(@R);
   mx := x;
   my := y;
   if FLayout = lyHorizontal then
    FArrowPos := XToArrowPos(x)
   else
    FArrowPos := YToArrowPos(y);
   Execute(TBA_MouseMove);
   FManual := true;
   FDoChange := true;
   Invalidate;
  end;
 inherited;
end;

procedure TmbTrackBarPicker.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 if Button <> mbLeft then Exit;
 mx := x;
 my := y;
 SetFocus;
 if FLayout = lyHorizontal then
  FArrowPos := XToArrowPos(x)
 else
  FArrowPos := YToArrowPos(y);
 Execute(TBA_MouseDown);
 FManual := true;
 FDoChange := true;
 Invalidate;
 inherited;
end;

procedure TmbTrackBarPicker.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 ClipCursor(nil);
 if Button <> mbLeft then Exit;
 mx := x;
 my := y;
 if FLayout = lyHorizontal then
  FArrowPos := XToArrowPos(x)
 else
  FArrowPos := YToArrowPos(y);
 Execute(TBA_MouseUp);
 FManual := true;
 FDoChange := true;
 Invalidate;
 inherited;
end;

procedure TmbTrackBarPicker.CNKeyDown(var Message: TWMKeyDown);
var
 Shift: TShiftState;
 FInherited: boolean;
begin
 FInherited := false;
 Shift := KeyDataToShiftState(Message.KeyData);
 case Message.CharCode of
  VK_UP:
   begin
    if FLayout = lyHorizontal then
     begin
      inherited;
      Exit;
     end;
    FChange := false;
    if not (ssCtrl in Shift) then
     Execute(TBA_VKUp)
    else
     Execute(TBA_VKCtrlUp);
    FManual := true;
    FChange := true;
    if Assigned(FOnChange) then FOnChange(Self);
   end;
  VK_LEFT:
   begin
    if FLayout = lyVertical then
     begin
      inherited;
      Exit;
     end;
    FChange := false;
    if not (ssCtrl in Shift) then
     Execute(TBA_VKLeft)
    else
     Execute(TBA_VKCtrlLeft);
    FManual := true;
    FChange := true;
    if Assigned(FOnChange) then FOnChange(Self);
   end;
  VK_RIGHT:
   begin
    if FLayout = lyVertical then
     begin
      inherited;
      Exit;
     end;
    FChange := false;
    if not (ssCtrl in Shift) then
     Execute(TBA_VKRight)
    else
     Execute(TBA_VKCtrlRight);
    FManual := true;
    FChange := true;
    if Assigned(FOnChange) then FOnChange(Self);
   end;
  VK_DOWN:
   begin
    if FLayout = lyHorizontal then
     begin
      inherited;
      Exit;
     end;
    FChange := false;
    if not (ssCtrl in Shift) then
     Execute(TBA_VKDown)
    else
     Execute(TBA_VKCtrlDown);
    FManual := true;
    FChange := true;
    if Assigned(FOnChange) then FOnChange(Self);
   end
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

procedure TmbTrackBarPicker.CMHintShow(var Message: TCMHintShow);
begin
 with TCMHintShow(Message) do
  if not ShowHint then
   Message.Result := 1
  else
   with HintInfo^ do
    begin
     Result := 0;
     ReshowTimeout := 1;
     HideTimeout := 5000;
     if FLayout = lyHorizontal then
      HintPos := ClientToScreen(Point(CursorPos.X - 8, Height + 2))
     else
      HintPos := ClientToScreen(Point(Width + 2, CursorPos.Y - 8));
     HintStr := GetHintStr;
    end;
 inherited;
end;

procedure TmbTrackBarPicker.CMGotFocus(var Message: TCMGotFocus);
begin
 inherited;
 Invalidate;
end;

procedure TmbTrackBarPicker.CMLostFocus(var Message: TCMLostFocus);
begin
 inherited;
 Invalidate;
end;

procedure TmbTrackBarPicker.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
 Message.Result := 1;
end;

procedure TmbTrackBarPicker.WheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
 Handled := true;
 FChange := false;
 Execute(TBA_WheelUp);
 FManual := true;
 FChange := true;
 if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TmbTrackBarPicker.WheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
 Handled := true;
 FChange := false;
 Execute(TBA_WheelDown);
 FManual := true;
 FChange := true;
 if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TmbTrackBarPicker.SetLayout(Value: TTrackBarLayout);
begin
 if FLayout <> Value then
  begin
   FLayout := Value;
   Execute(TBA_RedoBMP);
   Invalidate;
  end;
end;

procedure TmbTrackBarPicker.SetPlacement(Value: TSliderPlacement);
begin
 if FPlacement <> Value then
  begin
   FPlacement := Value;
   Invalidate;
  end;
end;

procedure TmbTrackBarPicker.SetNewArrowStyle(s: boolean);
begin
 if FNewArrowStyle <> s then
  begin
   FNewArrowStyle := s;
   Invalidate;
  end;
end;

procedure TmbTrackBarPicker.SetSelIndicator(Value: TSelIndicator);
begin
 if FSelIndicator <> Value then
  begin
   FSelIndicator := Value;
   Invalidate;
  end;
end;

procedure TmbTrackBarPicker.SetWebSafe(s: boolean);
begin
 if FWebSafe <> s then
  begin
   FWebSafe := s;
   Execute(TBA_RedoBMP);
   Invalidate;
  end;
end;

procedure TmbTrackBarPicker.Execute(tbaAction: integer);
begin
 //handled in descendants
end;

function TmbTrackBarPicker.GetArrowPos: integer;
begin
 Result := 0;
 //handled in descendants
end;

function TmbTrackBarPicker.GetHintStr: string;
begin
  Result := ReplaceFlags(FHintFormat, ['%value', '%h', '%s', '%l', '%v', '%c',
  '%m', '%y', '%k', '%r', '%g', '%b'], GetSelectedValue);
end;

procedure TmbTrackBarPicker.SetBevelInner(Value: TBevelCut);
begin
 if FBevelInner <> Value then
  begin
   FBevelInner := Value;
   Invalidate;
  end;
end;

procedure TmbTrackBarPicker.SetBevelOuter(Value: TBevelCut);
begin
 if FBevelOuter <> Value then
  begin
   FBevelOuter := Value;
   Invalidate;
  end;
end;

procedure TmbTrackBarPicker.SetBevelWidth(Value: TBevelWidth);
begin
 if FBevelWidth <> Value then
  begin
   FBevelWidth := Value;
   Invalidate;
  end;
end;

procedure TmbTrackBarPicker.SetBorderStyle(Value: TBorderStyle);
begin
 if FBorderStyle <> Value then
  begin
   FBorderStyle := Value;
   Invalidate;
  end;
end;

end.
