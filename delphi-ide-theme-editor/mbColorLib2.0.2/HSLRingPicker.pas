unit HSLRingPicker;

interface

{$I mxs.inc}

uses
 Windows, Messages, SysUtils, Classes, Controls, Graphics, Forms,
 RGBHSLUtils, HRingPicker, SLColorPicker, Menus, {$IFDEF DELPHI_7_UP}Themes,{$ENDIF}
 HTMLColors, Math;

type
 THSLRingPicker = class(TCustomControl)
 private
  FOnChange: TNotifyEvent;
  FRingPicker: THRingPicker;
  FSLPicker: TSLColorPicker;
  FSelectedColor: TColor;
  FHValue, FSValue, FLValue: integer;
  FRValue, FGValue, FBValue: integer;
  FRingHint, FSLHint: string;
  FSLMenu, FRingMenu: TPopupMenu;
  FSLCursor, FRingCursor: TCursor;
  PBack: TBitmap;

  function GetManual: boolean;
  procedure SelectColor(c: TColor);
  procedure SetH(v: integer);
  procedure SetS(v: integer);
  procedure SetL(v: integer);
  procedure SetR(v: integer);
  procedure SetG(v: integer);
  procedure SetB(v: integer);
  procedure SetRingHint(h: string);
  procedure SetSLHint(h: string);
  procedure SetSLMenu(m: TPopupMenu);
  procedure SetRingMenu(m: TPopupMenu);
  procedure SetRingCursor(c: TCursor);
  procedure SetSLCursor(c: TCursor);
  procedure PaintParentBack;
 protected
  procedure CreateWnd; override;
  procedure Paint; override;
  procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
  procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
  procedure DoMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
  procedure RingPickerChange(Sender: TObject);
  procedure SLPickerChange(Sender: TObject);
  procedure DoChange;
  procedure Resize; override;
 public
  constructor Create(AOwner: TComponent); override;
  destructor Destroy; override;

  function GetColorUnderCursor: TColor;
  function GetHexColorUnderCursor: string;
  function GetSelectedHexColor: string;
  property ColorUnderCursor: TColor read GetColorUnderCursor;
  property HValue: integer read FHValue write SetH default 0;
  property SValue: integer read FSValue write SetS default 240;
  property LValue: integer read FLValue write SetL default 120;
  property RValue: integer read FRValue write SetR default 255;
  property GValue: integer read FGValue write SetG default 0;
  property BValue: integer read FBValue write SetB default 0;
  property Manual: boolean read GetManual;
 published
  property SelectedColor: TColor read FSelectedColor write SelectColor default clRed;
  property RingPickerPopupMenu: TPopupMenu read FRingMenu write SetRingMenu;
  property SLPickerPopupMenu: TPopupMenu read FSLMenu write SetSLMenu;
  property RingPickerHintFormat: string read FRingHint write SetRingHint;
  property SLPickerHintFormat: string read FSLHint write SetSLHint;
  property RingPickerCursor: TCursor read FRingCursor write SetRingCursor default crDefault;
  property SLPickerCursor: TCursor read FSLCursor write SetSLCursor default crDefault;
  property TabStop default true;
  property ShowHint;
  property ParentShowHint;
  property Anchors;
  property Align;
  property Visible;
  property Enabled;
  property TabOrder;
  property Color;
  property ParentColor default true;
  {$IFDEF DELPHI_7_UP}
  property ParentBackground default true;
  {$ENDIF}

  property OnChange: TNotifyEvent read FOnChange write FOnChange;
  property OnMouseMove;
 end;

procedure Register;

implementation

procedure Register;
begin
 RegisterComponents('mbColor Lib', [THSLRingPicker]);
end;

{THSLRingPicker}

constructor THSLRingPicker.Create(AOwner: TComponent);
begin
 inherited;
 ControlStyle := ControlStyle - [csAcceptsControls] + [csOpaque{$IFDEF DELPHI_7_UP}, csParentBackground{$ENDIF}];
 DoubleBuffered := true;
 ParentColor := true;
 PBack := TBitmap.Create;
 PBack.PixelFormat := pf32bit;
 {$IFDEF DELPHI_7_UP}
 ParentBackground := true;
 {$ENDIF}
 Width := 245;
 Height := 245;
 TabStop := true;
 FSelectedColor := clRed;
 FRingPicker := THRingPicker.Create(Self);
 InsertControl(FRingPicker);
 FRingCursor := crDefault;
 FSLCursor := crDefault;
 with FRingPicker do
  begin
   Height := 246;
   Width := 246;
   Top := 0;
   Left := 0;
   Align := alClient;
   Visible := true;
   Saturation := 255;
   Value := 255;
   Hue := 0;
   OnChange := RingPickerChange;
   OnMouseMove := DoMouseMove;
  end;
 FSLPicker := TSLColorPicker.Create(Self);
 InsertControl(FSLPicker);
 with FSLPicker do
  begin
   Height := 120;
   Width := 120;
   Left := 63;
   Top := 63;
   Visible := true;
   OnChange := SLPickerChange;
   OnMouseMove := DoMouseMove;
  end;
 FHValue := 0;
 FSValue := 255;
 FLValue := 255;
 FRValue := 255;
 FGValue := 0;
 FBValue := 0;
 FRingHint := 'Hue: %h';
 FSLHint := 'S: %hslS L: %l'#13'Hex: %hex';
end;

destructor THSLRingPicker.Destroy;
begin
 PBack.Free;
 FRingPicker.Free;
 FSLPicker.Free;
 inherited Destroy;
end;

procedure THSLRingPicker.Resize;
begin
 inherited;
 FRingPicker.Radius := (Min(Width, Height)*30) div 245;
 FSLPicker.Left := (21*FRingPicker.Radius) div 10;
 FSLPicker.Top := (21*FRingPicker.Radius) div 10;
 FSLPicker.Width := 4*FRingPicker.Radius;
 FSLPicker.Height := 4*FRingPicker.Radius;
 PaintParentBack;
end;

procedure THSLRingPicker.RingPickerChange(Sender: TObject);
begin
 FSLPicker.Hue := FRingPicker.Hue;
 DoChange;
end;

procedure THSLRingPicker.SLPickerChange(Sender: TObject);
begin
 FSelectedColor := FSLPicker.SelectedColor;
 DoChange;
end;

procedure THSLRingPicker.DoChange;
begin
 FHValue := FRingPicker.Hue;
 FSValue := FSLPicker.Saturation;
 FLValue := FSLPicker.Luminance;
 FRValue := GetRValue(FSLPicker.SelectedColor);
 FGValue := GetGValue(FSLPicker.SelectedColor);
 FBValue := GetBValue(FSLPicker.SelectedColor);
 if Assigned(FOnChange) then
  FOnChange(Self);
end;

procedure THSLRingPicker.SelectColor(c: TColor);
begin
 FRingPicker.Hue := GetHValue(c);
 FRingPicker.Saturation := 255;
 FRingPicker.Value := 255;
 FSLPicker.SelectedColor := c;
 FSelectedColor := c;
end;

procedure THSLRingPicker.SetH(v: integer);
begin
 FHValue := v;
 FRingPicker.Hue := v;
 FSLPicker.Hue := v;
end;

procedure THSLRingPicker.SetS(v: integer);
begin
 FSValue := v;
 FSLPicker.Saturation := v;
end;

procedure THSLRingPicker.SetL(v: integer);
begin
 FLValue := v;
 FSLPicker.Luminance := v;
end;

procedure THSLRingPicker.SetR(v: integer);
begin
 FRValue := v;
 SelectColor(RGB(FRValue, FGValue, FBValue));
end;

procedure THSLRingPicker.SetG(v: integer);
begin
 FGValue := v;
 SelectColor(RGB(FRValue, FGValue, FBValue));
end;

procedure THSLRingPicker.SetB(v: integer);
begin
 FBValue := v;
 SelectColor(RGB(FRValue, FGValue, FBValue));
end;

function THSLRingPicker.GetSelectedHexColor: string;
begin
 Result := ColorToHex(FSelectedColor);
end;

procedure THSLRingPicker.SetRingHint(h: string);
begin
 FRingHint := h;
 FRingPicker.HintFormat := h;
end;

procedure THSLRingPicker.SetSLHint(h: string);
begin
 FSLHint := h;
 FSLPicker.HintFormat := h;
end;

procedure THSLRingPicker.SetRingMenu(m: TPopupMenu);
begin
 FRingMenu := m;
 FRingPicker.PopupMenu := m;
end;

procedure THSLRingPicker.SetSLMenu(m: TPopupMenu);
begin
 FSLMenu := m;
 FSLPicker.PopupMenu := m;
end;

procedure THSLRingPicker.DoMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
 if Assigned(OnMouseMove) then
  OnMouseMove(Self, Shift, x, y);
 inherited;
end;

function THSLRingPicker.GetColorUnderCursor: TColor;
begin
 Result := FSLPicker.GetColorUnderCursor;
end;

function THSLRingPicker.GetHexColorUnderCursor: string;
begin
 Result := FSLPicker.GetHexColorUnderCursor;
end;

procedure THSLRingPicker.SetRingCursor(c: TCursor);
begin
 FRingCursor := c;
 FRingPicker.Cursor := c;
end;

procedure THSLRingPicker.SetSLCursor(c: TCursor);
begin
 FSLCursor := c;
 FSLPicker.Cursor := c;
end;

procedure THSLRingPicker.WMSetFocus(var Message: TWMSetFocus);
begin
 FRingPicker.SetFocus;
 Message.Result := 1;
end;

function THSLRingPicker.GetManual:boolean;
begin
 Result := FRingPicker.Manual or FSLPicker.Manual;
end;

procedure THSLRingPicker.PaintParentBack;
var
 MemDC: HDC;
 OldBMP: HBITMAP;
begin
 if PBack = nil then
  begin
   PBack := TBitmap.Create;
   PBack.PixelFormat := pf32bit;
  end;
 PBack.Width := Width;
 PBack.Height := Height;
 PBack.Canvas.Brush.Color := Color;
 PBack.Canvas.FillRect(PBack.Canvas.ClipRect);
 {$IFDEF DELPHI_7_UP}
 if ParentBackground then
  with ThemeServices do
   if ThemesEnabled then
    begin
     MemDC := CreateCompatibleDC(0);
     OldBMP := SelectObject(MemDC, PBack.Handle);
     DrawParentBackground(Handle, MemDC, nil, False);
     if OldBMP <> 0 then SelectObject(MemDC, OldBMP);
     if MemDC <> 0 then DeleteDC(MemDC);
    end;
 {$ENDIF}
end;

procedure THSLRingPicker.Paint;
begin
 PaintParentBack;
 Canvas.Draw(0, 0, PBack);
end;

procedure THSLRingPicker.CreateWnd;
begin
 inherited;
 PaintParentBack;
end;

procedure THSLRingPicker.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
 Message.Result := 1;
end;

end.
