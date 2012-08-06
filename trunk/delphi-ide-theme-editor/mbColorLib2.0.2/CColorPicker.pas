unit CColorPicker;

interface

uses
 Windows, Messages, SysUtils, Classes, Controls, Graphics, Forms,
 RGBCMYKUtils, mbTrackBarPicker, HTMLColors, Scanlines;

type
 TCColorPicker = class(TmbTrackBarPicker)
 private
  FCyan, FMagenta, FYellow, FBlack: integer;
  FCBmp: TBitmap;

  function ArrowPosFromCyan(c: integer): integer;
  function CyanFromArrowPos(p: integer): integer;
  function GetSelectedColor: TColor;
  procedure SetSelectedColor(c: TColor);
  procedure CreateCGradient;
  procedure SetCyan(c: integer);
  procedure SetMagenta(m: integer);
  procedure SetYellow(y: integer);
  procedure SetBlack(k: integer);
 protected
  procedure CreateWnd; override;
  procedure Execute(tbaAction: integer); override;
  function GetArrowPos: integer; override;
  function GetSelectedValue: integer; override;
 public
  constructor Create(AOwner: TComponent); override;
  destructor Destroy; override;
 published
  property Cyan: integer read FCyan write SetCyan default 255;
  property Magenta: integer read FMagenta write SetMagenta default 0;
  property Yellow: integer read FYellow write SetYellow default 0;
  property Black: integer read FBlack write SetBlack default 0;
  property SelectedColor: TColor read GetSelectedColor write SetSelectedColor default clRed;
  property Layout default lyVertical;
 end;

procedure Register;

implementation

procedure Register;
begin
 RegisterComponents('mbColor Lib', [TCColorPicker]);
end;

{TCColorPicker}

constructor TCColorPicker.Create(AOwner: TComponent);
begin
 inherited;
 FCBmp := TBitmap.Create;
 FCBmp.PixelFormat := pf32bit;
 FCBmp.SetSize(12, 255);
 Width := 22;
 Height := 267;
 Layout := lyVertical;
 FCyan := 255;
 FMagenta := 0;
 FYellow := 0;
 FBlack := 0;
 FArrowPos := ArrowPosFromCyan(255);
 FChange := false;
 SetCyan(255);
 HintFormat := 'Cyan: %value';
 FManual := false;
 FChange := true;
end;

destructor TCColorPicker.Destroy;
begin
 FCBmp.Free;
 inherited Destroy;
end;

procedure TCColorPicker.CreateWnd;
begin
 inherited;
 CreateCGradient;
end;

procedure TCColorPicker.CreateCGradient;
var
 i,j: integer;
 row: pRGBQuadArray;
begin
 if FCBmp = nil then
  begin
   FCBmp := TBitmap.Create;
   FCBmp.PixelFormat := pf32bit;
  end;
 if Layout = lyHorizontal then
  begin
   FCBmp.width := 255;
   FCBmp.height := 12;
   for i := 0 to 254 do
    for j := 0 to 11 do
     begin
      row := FCBmp.Scanline[j];
      if not WebSafe then
       row[i] := RGBToRGBQuad(CMYKtoTColor(i, FMagenta, FYellow, FBlack))
      else
       row[i] := RGBToRGBQuad(GetWebSafe(CMYKtoTColor(i, FMagenta, FYellow, FBlack)));
     end;
  end
 else
  begin
   FCBmp.width := 12;
   FCBmp.height := 255;
   for i := 0 to 254 do
    begin
     row := FCBmp.Scanline[i];
     for j := 0 to 11 do
      if not WebSafe then
       row[j] := RGBtoRGBQuad(CMYKtoTColor(255-i, FMagenta, FYellow, FBlack))
      else
       row[j] := RGBtoRGBQuad(GetWebSafe(CMYKtoTColor(255-i, FMagenta, FYellow, FBlack)));
    end;
  end;
end;

procedure TCColorPicker.SetCyan(C: integer);
begin
 if C < 0 then C := 0;
 if C > 255 then C := 255;
 if FCyan <> c then
  begin
   FCyan := c;
   FArrowPos := ArrowPosFromCyan(c);
   FManual := false;
   Invalidate;
   if FChange then
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

procedure TCColorPicker.SetMagenta(m: integer);
begin
 if m > 255 then m := 255;
 if m < 0 then m := 0;
 if FMagenta <> m then
  begin
   FMagenta := m;
   FManual := false;
   CreateCGradient;
   Invalidate;
   if FChange then
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

procedure TCColorPicker.SetYellow(y: integer);
begin
 if y > 255 then y := 255;
 if y < 0 then y := 0;
 if FYellow <> y then
  begin
   FYellow := y;
   FManual := false;
   CreateCGradient;
   Invalidate;
   if FChange then
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

procedure TCColorPicker.SetBlack(k: integer);
begin
 if k > 255 then k := 255;
 if k < 0 then k := 0;
 if FBlack <> k then
  begin
   FBlack := k;
   FManual := false;
   CreateCGradient;
   Invalidate;
   if FChange then
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

function TCColorPicker.ArrowPosFromCyan(c: integer): integer;
var
 a: integer;
begin
 if Layout = lyHorizontal then
  begin
   a := Round(((Width - 12)/255)*c);
   if a > Width - FLimit then a := Width - FLimit;
  end
 else
  begin
   c := 255 - c;
   a := Round(((Height - 12)/255)*c);
   if a > Height - FLimit then a := Height - FLimit;
  end;
 if a < 0 then a := 0;
 Result := a;
end;

function TCColorPicker.CyanFromArrowPos(p: integer): integer;
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

function TCColorPicker.GetSelectedColor: TColor;
begin
 if not WebSafe then
  Result := CMYKtoTColor(FCyan, FMagenta, FYellow, FBlack)
 else
  Result := GetWebSafe(CMYKtoTColor(FCyan, FMagenta, FYellow, FBlack));
end;

function TCColorPicker.GetSelectedValue: integer;
begin
  Result := FCyan;
end;

procedure TCColorPicker.SetSelectedColor(c: TColor);
var
 cy, m, y, k: integer;
begin
 if WebSafe then c := GetWebSafe(c);
 ColorToCMYK(c, cy, m, y, k);
 FChange := false;
 SetMagenta(m);
 SetYellow(y);
 SetBlack(k);
 SetCyan(cy);
 FManual := false;
 FChange := true;
 if Assigned(OnChange) then OnChange(Self);
end;

function TCColorPicker.GetArrowPos: integer;
begin
 Result := ArrowPosFromCyan(FCyan);
end;

procedure TCColorPicker.Execute(tbaAction: integer);
begin
 case tbaAction of
  TBA_Resize: SetCyan(FCyan);
  TBA_Paint: Canvas.StretchDraw(FPickRect, FCBmp);
  TBA_MouseMove: FCyan := CyanFromArrowPos(FArrowPos);
  TBA_MouseDown: FCyan := CyanFromArrowPos(FArrowPos);
  TBA_MouseUp: FCyan := CyanFromArrowPos(FArrowPos);
  TBA_WheelUp: SetCyan(FCyan + Increment);
  TBA_WheelDown: SetCyan(FCyan - Increment);
  TBA_VKRight: SetCyan(FCyan + Increment);
  TBA_VKCtrlRight: SetCyan(255);
  TBA_VKLeft: SetCyan(FCyan - Increment);
  TBA_VKCtrlLeft: SetCyan(0);
  TBA_VKUp: SetCyan(FCyan + Increment);
  TBA_VKCtrlUp: SetCyan(255);
  TBA_VKDown: SetCyan(FCyan - Increment);
  TBA_VKCtrlDown: SetCyan(0);
  TBA_RedoBMP: CreateCGradient;
 end;
end;

end.
