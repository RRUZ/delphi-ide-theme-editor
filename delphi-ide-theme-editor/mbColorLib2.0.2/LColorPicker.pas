unit LColorPicker;

interface

uses
 Windows, Messages, SysUtils, Classes, Controls, Graphics, Forms,
 RGBHSLUtils, mbTrackBarPicker, HTMLColors, Scanlines;

type
 TLColorPicker = class(TmbTrackBarPicker)
 private
  FHue, FSat, FLuminance: integer;
  FLBmp: TBitmap;

  function ArrowPosFromLum(l: integer): integer;
  function LumFromArrowPos(p: integer): integer;
  procedure CreateLGradient;
  procedure SetHue(h: integer);
  procedure SetSat(s: integer);
  procedure SetLuminance(l: integer);
  function GetSelectedColor: TColor;
  procedure SetSelectedColor(c: TColor);
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
  property Saturation: integer read FSat write SetSat default 240;
  property Luminance: integer read FLuminance write SetLuminance default 120;
  property SelectedColor: TColor read GetSelectedColor write SetSelectedColor default clRed;
  property Layout default lyVertical;
 end;

procedure Register;

implementation

procedure Register;
begin
 RegisterComponents('mbColor Lib', [TLColorPicker]);
end;

{TLColorPicker}

constructor TLColorPicker.Create(AOwner: TComponent);
begin
 inherited;
 FLBmp := TBitmap.Create;
 FLBmp.PixelFormat := pf32bit;
 Width := 22;
 Height := 252;
 Layout := lyVertical;
 FHue := 0;
 FSat := MaxSat;
 FArrowPos := ArrowPosFromLum(MaxLum div 2);
 Fchange := false;
 SetLuminance(MaxLum div 2);
 HintFormat := 'Luminance: %value';
 FManual := false;
 FChange := true;
end;

destructor TLColorPicker.Destroy;
begin
 FLBmp.Free;
 inherited Destroy;
end;

procedure TLColorPicker.CreateWnd;
begin
 inherited;
 CreateLGradient;
end;

procedure TLColorPicker.CreateLGradient;
var
 i,j: integer;
 row: pRGBQuadArray;
begin
 if FLBmp = nil then
  begin
   FLBmp := TBitmap.Create;
   FLBmp.PixelFormat := pf32bit;
  end;
 if Layout = lyHorizontal then
  begin
   FLBmp.width := MaxLum;
   FLBmp.height := 12;
   for i := 0 to MaxLum - 1 do
    for j := 0 to 11 do
     begin
      row := FLBmp.Scanline[j];
      if not WebSafe then
       row[i] := RGBToRGBQuad(HSLRangeToRGB(FHue, FSat, i))
//       FLBmp.Canvas.Pixels[i, j] := HSLRangeToRGB(FHue, FSat, i)
      else
       row[i] := RGBToRGBQuad(GetWebSafe(HSLRangeToRGB(FHue, FSat, i)));
//       FLBmp.Canvas.Pixels[i, j] := GetWebSafe(HSLRangeToRGB(FHue, FSat, i));
     end;
  end
 else
  begin
   FLBmp.width := 12;
   FLBmp.height := MaxLum;
   for i := 0 to MaxLum - 1 do
    begin
     row := FLBmp.Scanline[i];
     for j := 0 to 11 do
      if not WebSafe then
       row[j] := RGBToRGBQuad(HSLRangeToRGB(FHue, FSat, MaxLum - i))
//       FLBmp.Canvas.Pixels[j, i] := HSLRangeToRGB(FHue, FSat, MaxLum - i)
      else
       row[j] := RGBToRGBQuad(GetWebSafe(HSLRangeToRGB(FHue, FSat, MaxLum - i)));
//       FLBmp.Canvas.Pixels[j, i] := GetWebSafe(HSLRangeToRGB(FHue, FSat, MaxLum - i));
    end;
  end;
end;

procedure TLColorPicker.SetHue(h: integer);
begin
 if h > MaxHue then h := MaxHue;
 if h < 0 then h := 0;
 if FHue <> h then
  begin
   FHue := h;
   FManual := false;
   CreateLGradient;
   Invalidate;
   if Fchange then
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

procedure TLColorPicker.SetSat(s: integer);
begin
 if s > MaxSat then s := MaxSat;
 if s < 0 then s := 0;
 if FSat <> s then
  begin
   FSat := s;
   FManual := false;
   CreateLGradient;
   Invalidate;
   if Fchange then
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

function TLColorPicker.ArrowPosFromLum(l: integer): integer;
var
 a: integer;
begin
 if Layout = lyHorizontal then
  begin
   a := Round(((Width - 12)/MaxLum)*l);
   if a > Width - FLimit then a := Width - FLimit;
  end
 else
  begin
   l := MaxLum - l;
   a := Round(((Height - 12)/MaxLum)*l);
   if a > Height - FLimit then a := Height - FLimit;
  end;
 if a < 0 then a := 0;
 Result := a;
end;

function TLColorPicker.LumFromArrowPos(p: integer): integer;
var
 r: integer;
begin
 if Layout = lyHorizontal then
  r := Round(p/((Width - 12)/MaxLum))
 else
  r := Round(MaxLum - p/((Height - 12)/MaxLum));
 if r < 0 then r := 0;
 if r > MaxLum then r := MaxLum;
 Result := r;
end;

procedure TLColorPicker.SetLuminance(l: integer);
begin
 if l < 0 then l := 0;
 if l > MaxLum then l := MaxLum;
 if FLuminance <> l then
  begin
   FLuminance := l;
   FArrowPos := ArrowPosFromLum(l);
   FManual := false;
   Invalidate;
   if Fchange then
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

function TLColorPicker.GetSelectedColor: TColor;
begin
 if not WebSafe then
  Result := HSLRangeToRGB(FHue, FSat, FLuminance)
 else
  Result := GetWebSafe(HSLRangeToRGB(FHue, FSat, FLuminance));
end;

function TLColorPicker.GetSelectedValue: integer;
begin
  Result := FLuminance;
end;

procedure TLColorPicker.SetSelectedColor(c: TColor);
var
 h1, s1, l1: integer;
begin
 if WebSafe then c := GetWebSafe(c);
 RGBtoHSLRange(c, h1, s1, l1);
 Fchange := false;
 SetHue(h1);
 SetSat(s1);
 SetLuminance(l1);
 Fchange := true;
 FManual := false;
 if Fchange then
  if Assigned(OnChange) then OnChange(Self);
end;

function TLColorPicker.GetArrowPos: integer;
begin
 Result := ArrowPosFromLum(FLuminance);
end;

procedure TLColorPicker.Execute(tbaAction: integer);
begin
 case tbaAction of
  TBA_Resize: SetLuminance(FLuminance);
  TBA_Paint: Canvas.StretchDraw(FPickRect, FLBmp);
  TBA_MouseMove: FLuminance := LumFromArrowPos(FArrowPos);
  TBA_MouseDown: Fluminance := LumFromArrowPos(FArrowPos);
  TBA_MouseUp: Fluminance := LumFromArrowPos(FArrowPos);
  TBA_WheelUp: SetLuminance(FLuminance + Increment);
  TBA_WheelDown: SetLuminance(FLuminance - Increment);
  TBA_VKRight: SetLuminance(FLuminance + Increment);
  TBA_VKCtrlRight: SetLuminance(MaxLum);
  TBA_VKLeft: SetLuminance(FLuminance - Increment);
  TBA_VKCtrlLeft: SetLuminance(0);
  TBA_VKUp: SetLuminance(FLuminance + Increment);
  TBA_VKCtrlUp: SetLuminance(MaxLum);
  TBA_VKDown: SetLuminance(FLuminance - Increment);
  TBA_VKCtrlDown: SetLuminance(0);
  TBA_RedoBMP: CreateLGradient;
 end;
end;

end.
