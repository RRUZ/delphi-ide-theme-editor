unit MColorPicker;

interface

uses
 Windows, Messages, SysUtils, Classes, Controls, Graphics, Forms,
 RGBCMYKUtils, mbTrackBarPicker, HTMLColors, Scanlines;

type
 TMColorPicker = class(TmbTrackBarPicker)
 private
  FCyan, FMagenta, FYellow, FBlack: integer;
  FMBmp: TBitmap;

  function ArrowPosFromMagenta(m: integer): integer;
  function MagentaFromArrowPos(p: integer): integer;
  function GetSelectedColor: TColor;
  procedure SetSelectedColor(c: TColor);
  procedure CreateMGradient;
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
  property Cyan: integer read FCyan write SetCyan default 0;
  property Magenta: integer read FMagenta write SetMagenta default 255;
  property Yellow: integer read FYellow write SetYellow default 0;
  property Black: integer read FBlack write SetBlack default 0;
  property SelectedColor: TColor read GetSelectedColor write SetSelectedColor default clRed;
  property Layout default lyVertical;
 end;

procedure Register;

implementation

procedure Register;
begin
 RegisterComponents('mbColor Lib', [TMColorPicker]);
end;

{TMColorPicker}

constructor TMColorPicker.Create(AOwner: TComponent);
begin
 inherited;
 FMBmp := TBitmap.Create;
 FMBmp.PixelFormat := pf32bit;
 FMBmp.SetSize(12, 255);
 Width := 22;
 Height := 267;
 Layout := lyVertical;
 FCyan := 0;
 FMagenta := 255;
 FYellow := 0;
 FBlack := 0;
 FArrowPos := ArrowPosFromMagenta(255);
 FChange := false;
 SetMagenta(255);
 HintFormat := 'Magenta: %value';
 FManual := false;
 FChange := true;
end;

destructor TMColorPicker.Destroy;
begin
 FMBmp.Free;
 inherited Destroy;
end;

procedure TMColorPicker.CreateWnd;
begin
 inherited;
 CreateMGradient;
end;

procedure TMColorPicker.CreateMGradient;
 var
 i,j: integer;
 row: pRGBQuadArray;
begin
 if FMBmp = nil then
  begin
   FMBmp := TBitmap.Create;
   FMBmp.PixelFormat := pf32bit;
  end;
 if Layout = lyHorizontal then
  begin
   FMBmp.width := 255;
   FMBmp.height := 12;
   for i := 0 to 254 do
    for j := 0 to 11 do
     begin
      row := FMBmp.ScanLine[j];
      if not WebSafe then
       row[i] := RGBToRGBQuad(CMYKtoTColor(FCyan, i, FYellow, FBlack))
//       FMBmp.Canvas.Pixels[i, j] := CMYKtoTColor(FCyan, i, FYellow, FBlack)
      else
       row[i] := RGBToRGBQuad(GetWebSafe(CMYKtoTColor(FCyan, i, FYellow, FBlack)));
//       FMBmp.Canvas.Pixels[i, j] := GetWebSafe(CMYKtoTColor(FCyan, i, FYellow, FBlack));
     end;
  end
 else
  begin
   FMBmp.width := 12;
   FMBmp.height := 255;
   for i := 0 to 254 do
    begin
     row := FMBmp.Scanline[i];
     for j := 0 to 11 do
      if not WebSafe then
       row[j] := RGBToRGBQuad(CMYKtoTColor(FCyan, 255-i, FYellow, FBlack))
//       FMBmp.Canvas.Pixels[j, i] := CMYKtoTColor(FCyan, 255-i, FYellow, FBlack)
      else
       row[j] := RGBToRGBQuad(GetWebSafe(CMYKtoTColor(FCyan, 255-i, FYellow, FBlack)));
//       FMBmp.Canvas.Pixels[j, i] := GetWebSafe(CMYKtoTColor(FCyan, 255-i, FYellow, FBlack));
    end;
  end;
end;

procedure TMColorPicker.SetMagenta(m: integer);
begin
 if M < 0 then M := 0;
 if M > 255 then M := 255;
 if FMagenta <> m then
  begin
   FMagenta := m;
   FArrowPos := ArrowPosFromMagenta(m);
   FManual := false;
   Invalidate;
   if FChange then
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

procedure TMColorPicker.SetCyan(c: integer);
begin
 if c > 255 then c := 255;
 if c < 0 then c := 0;
 if FCyan <> c then
  begin
   FCyan := c;
   FManual := false;
   CreateMGradient;
   Invalidate;
   if FChange then
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

procedure TMColorPicker.SetYellow(y: integer);
begin
 if y > 255 then y := 255;
 if y < 0 then y := 0;
 if FYellow <> y then
  begin
   FYellow := y;
   FManual := false;
   CreateMGradient;
   Invalidate;
   if FChange then
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

procedure TMColorPicker.SetBlack(k: integer);
begin
 if k > 255 then k := 255;
 if k < 0 then k := 0;
 if FBlack <> k then
  begin
   FBlack := k;
   FManual := false;
   CreateMGradient;
   Invalidate;
   if FChange then
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

function TMColorPicker.ArrowPosFromMagenta(m: integer): integer;
var
 a: integer;
begin
 if Layout = lyHorizontal then
  begin
   a := Round(((Width - 12)/255)*m);
   if a > Width - FLimit then a := Width - FLimit;
  end
 else
  begin
   m := 255 - m;
   a := Round(((Height - 12)/255)*m);
   if a > Height - FLimit then a := Height - FLimit;
  end;
 if a < 0 then a := 0;
 Result := a;
end;

function TMColorPicker.MagentaFromArrowPos(p: integer): integer;
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

function TMColorPicker.GetSelectedColor: TColor;
begin
 if not WebSafe then
  Result := CMYKtoTColor(FCyan, FMagenta, FYellow, FBlack)
 else
  Result := GetWebSafe(CMYKtoTColor(FCyan, FMagenta, FYellow, FBlack));
end;

function TMColorPicker.GetSelectedValue: integer;
begin
  Result := FMagenta;
end;

procedure TMColorPicker.SetSelectedColor(c: TColor);
var
 cy, m, y, k: integer;
begin
 if WebSafe then c := GetWebSafe(c);
 ColorToCMYK(c, cy, m, y, k);
 FChange := false;
 SetCyan(cy);
 SetYellow(y);
 SetBlack(k);
 SetMagenta(m);
 FManual := false;
 FChange := true;
 if Assigned(OnChange) then OnChange(Self);
end;

function TMColorPicker.GetArrowPos: integer;
begin
 Result := ArrowPosFromMagenta(FMagenta);
end;

procedure TMColorPicker.Execute(tbaAction: integer);
begin
 case tbaAction of
  TBA_Resize: SetMagenta(FMagenta);
  TBA_Paint: Canvas.StretchDraw(FPickRect, FMBmp);
  TBA_MouseMove: FMagenta := MagentaFromArrowPos(FArrowPos);
  TBA_MouseDown: FMagenta := MagentaFromArrowPos(FArrowPos);
  TBA_MouseUp: FMagenta := MagentaFromArrowPos(FArrowPos);
  TBA_WheelUp: SetMagenta(FMagenta + Increment);
  TBA_WheelDown: SetMagenta(FMagenta - Increment);
  TBA_VKRight: SetMagenta(FMagenta + Increment);
  TBA_VKCtrlRight: SetMagenta(255);
  TBA_VKLeft: SetMagenta(FMagenta - Increment);
  TBA_VKCtrlLeft: SetMagenta(0);
  TBA_VKUp: SetMagenta(FMagenta + Increment);
  TBA_VKCtrlUp: SetMagenta(255);
  TBA_VKDown: SetMagenta(FMagenta - Increment);
  TBA_VKCtrlDown: SetMagenta(0);
  TBA_RedoBMP: CreateMGradient;
 end;
end;

end.
