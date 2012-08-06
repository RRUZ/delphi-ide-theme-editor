unit HSLColorPicker;

interface

{$I mxs.inc}

uses
 Windows, Messages, SysUtils, Classes, Controls, Graphics, Forms,
 RGBHSLUtils, HSColorPicker, LColorPicker, Menus, {$IFDEF DELPHI_7_UP} Themes, {$ENDIF} HTMLColors;

type
 THSLColorPicker = class(TCustomControl)
 private
  FOnChange: TNotifyEvent;
  FHSPicker: THSColorPicker;
  FLPicker: TLColorPicker;
  FSelectedColor: TColor;
  FHValue, FSValue, FLValue: integer;
  FRValue, FGValue, FBValue: integer;
  FHSHint, FLHint: string;
  FLMenu, FHSMenu: TPopupMenu;
  FLumIncrement: integer;
  FHSCursor, FLCursor: TCursor;
  PBack: TBitmap;

  function GetManual: boolean;
  procedure SetLumIncrement(i: integer);
  procedure SelectColor(c: TColor);
  procedure SetH(v: integer);
  procedure SetS(v: integer);
  procedure SetL(v: integer);
  procedure SetR(v: integer);
  procedure SetG(v: integer);
  procedure SetB(v: integer);
  procedure SetHSHint(h: string);
  procedure SetLHint(h: string);
  procedure SetLMenu(m: TPopupMenu);
  procedure SetHSMenu(m: TPopupMenu);
  procedure SetHSCursor(c: TCursor);
  procedure SetLCursor(c: TCursor);
  procedure PaintParentBack;
  procedure SetSelectedColor(Value: TColor);
 protected
  procedure CreateWnd; override;
  procedure Resize; override;
  procedure Paint; override;
  procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
  procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
  procedure DoMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
  procedure HSPickerChange(Sender: TObject);
  procedure LPickerChange(Sender: TObject);
  procedure DoChange;
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
  property LuminanceIncrement: integer read FLumIncrement write SetLumIncrement default 1;
  property SelectedColor: TColor read FSelectedColor write SetSelectedColor default clRed;
  property HSPickerPopupMenu: TPopupMenu read FHSMenu write SetHSMenu;
  property LPickerPopupMenu: TPopupMenu read FLMenu write SetLMenu;
  property HSPickerHintFormat: string read FHSHint write SetHSHint;
  property LPickerHintFormat: string read FLHint write SetLHint;
  property HSPickerCursor: TCursor read FHSCursor write SetHSCursor default crDefault;
  property LPickerCursor: TCursor read FLCursor write SetLCursor default crDefault;
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
 RegisterComponents('mbColor Lib', [THSLColorPicker]);
end;

{THSLColorPicker}

constructor THSLColorPicker.Create(AOwner: TComponent);
begin
 inherited;
 ControlStyle := ControlStyle - [csAcceptsControls] + [csOpaque];
 DoubleBuffered := true;
 ParentColor := true;
 PBack := TBitmap.Create;
 PBack.PixelFormat := pf32bit;
 {$IFDEF DELPHI_7_UP}
 ParentBackground := true;
 {$ENDIF}
 Width := 206;
 Height := 146;
 TabStop := true;
 FSelectedColor := clRed;
 FHSPicker := THSColorPicker.Create(Self);
 InsertControl(FHSPicker);
 FLumIncrement := 1;
 FHSCursor := crDefault;
 FLCursor := crDefault;
 with FHSPicker do
  begin
   Height := 134;
   Width := 174;
   Top := 6;
   Left := 0;
   Anchors := [akLeft, akTop, akRight, akBottom];
   Visible := true;
   OnChange := HSPickerChange;
   OnMouseMove := DoMouseMove;
  end;
 FLPicker := TLColorPicker.Create(Self);
 InsertControl(FLPicker);
 with FLPicker do
  begin
   Height := 146;
   Top := 0;
   Left := 184;
   Anchors := [akRight, akTop, akBottom];
   Visible := true;
   OnChange := LPickerChange;
   OnMouseMove := DoMouseMove;
  end;
 FHValue := 0;
 FSValue := 240;
 FLValue := 120;
 FRValue := 255;
 FGValue := 0;
 FBValue := 0;
 FHSHint := 'H: %h S: %hslS'#13'Hex: %hex';
 FLHint := 'Luminance: %l';
end;

destructor THSLColorPicker.Destroy;
begin
 PBack.Free;
 FHSPicker.Free;
 FLPicker.Free;
 inherited Destroy;
end;

procedure THSLColorPicker.HSPickerChange(Sender: TObject);
begin
 FLPicker.Hue := FHSPicker.HueValue;
 FLPicker.Saturation := FHSPicker.SaturationValue;
 DoChange;
end;

procedure THSLColorPicker.LPickerChange(Sender: TObject);
begin
 FHSPicker.Lum := FLPicker.Luminance;
 FSelectedColor := FLPicker.SelectedColor;
 DoChange;
end;

procedure THSLColorPicker.DoChange;
begin
 FHValue := FLPicker.Hue;
 FSValue := FLPicker.Saturation;
 FLValue := FLPicker.Luminance;
 FRValue := GetRValue(FLPicker.SelectedColor);
 FGValue := GetGValue(FLPicker.SelectedColor);
 FBValue := GetBValue(FLPicker.SelectedColor);
 if Assigned(FOnChange) then
  FOnChange(Self);
end;

procedure THSLColorPicker.SelectColor(c: TColor);
begin
 FSelectedColor := c;
 FHSPicker.SelectedColor := c;
 FLPicker.SelectedColor := c;
end;

procedure THSLColorPicker.SetH(v: integer);
begin
 FHValue := v;
 FHSPicker.HueValue := v;
 FLPicker.Hue := v;
end;

procedure THSLColorPicker.SetS(v: integer);
begin
 FSValue := v;
 FHSPicker.SaturationValue := v;
 FLPicker.Saturation := v;
end;

procedure THSLColorPicker.SetL(v: integer);
begin
 FLValue := v;
 FLPicker.Luminance := v;
end;

procedure THSLColorPicker.SetR(v: integer);
begin
 FRValue := v;
 SetSelectedColor(RGB(FRValue, FGValue, FBValue));
end;

procedure THSLColorPicker.SetG(v: integer);
begin
 FGValue := v;
 SetSelectedColor(RGB(FRValue, FGValue, FBValue));
end;

procedure THSLColorPicker.SetB(v: integer);
begin
 FBValue := v;
 SetSelectedColor(RGB(FRValue, FGValue, FBValue));
end;

function THSLColorPicker.GetSelectedHexColor: string;
begin
 Result := ColorToHex(FSelectedColor);
end;

procedure THSLColorPicker.SetHSHint(h: string);
begin
 FHSHint := h;
 FHSPicker.HintFormat := h;
end;

procedure THSLColorPicker.SetLHint(h: string);
begin
 FLHint := h;
 FLPicker.HintFormat := h;
end;

procedure THSLColorPicker.SetLMenu(m: TPopupMenu);
begin
 FLMenu := m;
 FLPicker.PopupMenu := m;
end;

procedure THSLColorPicker.SetHSMenu(m: TPopupMenu);
begin
 FHSMenu := m;
 FHSPicker.PopupMenu := m;
end;

procedure THSLColorPicker.SetLumIncrement(i: integer);
begin
 FLumIncrement := i;
 FLPicker.Increment := i;
end;

procedure THSLColorPicker.DoMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
 if Assigned(OnMouseMove) then
  OnMouseMove(Self, Shift, x, y);
 inherited;
end;

function THSLColorPicker.GetColorUnderCursor: TColor;
begin
 Result := FHSPicker.GetColorUnderCursor;
end;

function THSLColorPicker.GetHexColorUnderCursor: string;
begin
 Result := FHSPicker.GetHexColorUnderCursor;
end;

procedure THSLColorPicker.SetHSCursor(c: TCursor);
begin
 FHSCursor := c;
 FHSPicker.Cursor := c;
end;

procedure THSLColorPicker.SetLCursor(c: TCursor);
begin
 FLCursor := c;
 FLPicker.Cursor := c;
end;

procedure THSLColorPicker.WMSetFocus(var Message: TWMSetFocus);
begin
 FHSPicker.SetFocus;
 Message.Result := 1;
end;

function THSLColorPicker.GetManual:boolean;
begin
 Result := FHSPicker.Manual or FLPicker.Manual;
end;

procedure THSLColorPicker.PaintParentBack;
{$IFDEF DELPHI_7_UP}
var
 MemDC: HDC;
 OldBMP: HBITMAP;
 {$ENDIF}
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
  with {$IFDEF DELPHI_XE2_UP}StyleServices{$ELSE}ThemeServices{$ENDIF} do
   if {$IFDEF DELPHI_XE2_UP}Enabled{$ELSE}ThemesEnabled{$ENDIF} then
    begin
     MemDC := CreateCompatibleDC(0);
     OldBMP := SelectObject(MemDC, PBack.Handle);
     DrawParentBackground(Handle, MemDC, nil, False);
     if OldBMP <> 0 then SelectObject(MemDC, OldBMP);
     if MemDC <> 0 then DeleteDC(MemDC);
    end;
 {$ENDIF}
end;

procedure THSLColorPicker.Resize;
begin
 inherited;
 PaintParentBack;
end;

procedure THSLColorPicker.CreateWnd;
begin
 inherited;
 PaintParentBack;
end;

procedure THSLColorPicker.Paint;
begin
 PaintParentBack;
 Canvas.Draw(0, 0, PBack);
end;

procedure THSLColorPicker.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
 Message.Result := 1;
end;

procedure THSLColorPicker.SetSelectedColor(Value: TColor);
begin
 if FSelectedColor <> Value then
  begin
   SelectColor(Value);
   //FLPicker.Hue := FHSPicker.HueValue;
   //FLPicker.Saturation := FHSPicker.SaturationValue;
  end;
end;

end.
