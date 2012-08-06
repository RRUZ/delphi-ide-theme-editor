unit YColorPicker;

interface

uses
 Windows, Messages, SysUtils, Classes, Controls, Graphics, Forms,
 RGBCMYKUtils, mbTrackBarPicker, HTMLColors, Scanlines;

type
 TYColorPicker = class(TmbTrackBarPicker)
 private
  FYellow, FMagenta, FCyan, FBlack: integer;
  FYBmp: TBitmap;

  function ArrowPosFromYellow(y: integer): integer;
  function YellowFromArrowPos(p: integer): integer;
  function GetSelectedColor: TColor;
  procedure SetSelectedColor(c: TColor);
  procedure CreateYGradient;
  procedure SetYellow(y: integer);
  procedure SetMagenta(m: integer);
  procedure SetCyan(c: integer);
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
  property Yellow: integer read FYellow write SetYellow default 255;
  property Magenta: integer read FMagenta write SetMagenta default 0;
  property Cyan: integer read FCyan write SetCyan default 0;
  property Black: integer read FBlack write SetBlack default 0;
  property SelectedColor: TColor read GetSelectedColor write SetSelectedColor default clRed;
  property Layout default lyVertical;
 end;

procedure Register;

implementation

procedure Register;
begin
 RegisterComponents('mbColor Lib', [TYColorPicker]);
end;

{TYColorPicker}

constructor TYColorPicker.Create(AOwner: TComponent);
begin
 inherited;
 FYBmp := TBitmap.Create;
 FYBmp.PixelFormat := pf32bit;
 FYBmp.SetSize(12, 255);
 Width := 22;
 Height := 267;
 Layout := lyVertical;
 FYellow := 255;
 FMagenta := 0;
 FCyan := 0;
 FBlack := 0;
 FArrowPos := ArrowPosFromYellow(255);
 FChange := false;
 SetYellow(255);
 HintFormat := 'Yellow: %value';
 FManual := false;
 FChange := true;
end;

destructor TYColorPicker.Destroy;
begin
 FYBmp.Free;
 inherited Destroy;
end;

procedure TYColorPicker.CreateWnd;
begin
 inherited;
 CreateYGradient;
end;

procedure TYColorPicker.CreateYGradient;
var
 i,j: integer;
 row: pRGBQuadArray;
begin
 if FYBmp = nil then
  begin
   FYBmp := TBitmap.Create;
   FYBmp.PixelFormat := pf32bit;
  end;
 if Layout = lyHorizontal then
  begin
   FYBmp.width := 255;
   FYBmp.height := 12;
   for i := 0 to 254 do
    for j := 0 to 11 do
     begin
      row := FYBmp.Scanline[j];
      if not WebSafe then
       row[i] := RGBToRGBQuad(CMYKtoTColor(FCyan, FMagenta, i, FBlack))
//       FYBmp.Canvas.Pixels[i, j] := CMYKtoTColor(FCyan, FMagenta, i, FBlack)
      else
       row[i] := RGBToRGBQuad(GetWebSafe(CMYKtoTColor(FCyan, FMagenta, i, FBlack)));
//       FYBmp.Canvas.Pixels[i, j] := GetWebSafe(CMYKtoTColor(FCyan, FMagenta, i, FBlack));
     end;
  end
 else
  begin
   FYBmp.width := 12;
   FYBmp.height := 255;
   for i := 0 to 254 do
    begin
     row := FYBmp.Scanline[i];
     for j := 0 to 11 do
      if not WebSafe then
       row[j] := RGBToRGBQuad(CMYKtoTColor(FCyan, FMagenta, 255-i, FBlack))
//       FYBmp.Canvas.Pixels[j, i] := CMYKtoTColor(FCyan, FMagenta, 255-i, FBlack)
      else
       row[j] := RGBToRGBQuad(GetWebSafe(CMYKtoTColor(FCyan, FMagenta, 255-i, FBlack)));
//       FYBmp.Canvas.Pixels[j, i] := GetWebSafe(CMYKtoTColor(FCyan, FMagenta, 255-i, FBlack));
    end;
  end;
end;

procedure TYColorPicker.SetYellow(y: integer);
begin
 if y < 0 then y := 0;
 if y > 255 then y := 255;
 if FYellow <> y then
  begin
   FYellow := y;
   FArrowPos := ArrowPosFromYellow(y);
   FManual := false;
   Invalidate;
   if FChange then
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

procedure TYColorPicker.SetMagenta(m: integer);
begin
 if m > 255 then m := 255;
 if m < 0 then m := 0;
 if FMagenta <> m then
  begin
   FMagenta := m;
   FManual := false;
   CreateYGradient;
   Invalidate;
   if FChange then
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

procedure TYColorPicker.SetCyan(c: integer);
begin
 if c > 255 then c := 255;
 if c < 0 then c := 0;
 if FCyan <> c then
  begin
   FCyan := c;
   FManual := false;
   CreateYGradient;
   Invalidate;
   if FChange then
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

procedure TYColorPicker.SetBlack(k: integer);
begin
 if k > 255 then k := 255;
 if k < 0 then k := 0;
 if FBlack <> k then
  begin
   FBlack := k;
   FManual := false;
   CreateYGradient;
   Invalidate;
   if FChange then
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

function TYColorPicker.ArrowPosFromYellow(y: integer): integer;
var
 a: integer;
begin
 if Layout = lyHorizontal then
  begin
   a := Round(((Width - 12)/255)*y);
   if a > Width - FLimit then a := Width - FLimit;
  end
 else
  begin
   y := 255 - y;
   a := Round(((Height - 12)/255)*y);
   if a > Height - FLimit then a := Height - FLimit;
  end;
 if a < 0 then a := 0;
 Result := a;
end;

function TYColorPicker.YellowFromArrowPos(p: integer): integer;
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

function TYColorPicker.GetSelectedColor: TColor;
begin
 if not WebSafe then
  Result := CMYKtoTColor(FCyan, FMagenta, FYellow, FBlack)
 else
  Result := GetWebSafe(CMYKtoTColor(FCyan, FMagenta, FYellow, FBlack));
end;

function TYColorPicker.GetSelectedValue: integer;
begin
  Result := FYellow;
end;

procedure TYColorPicker.SetSelectedColor(c: TColor);
var
 cy, m, y, k: integer;
begin
 if WebSafe then c := GetWebSafe(c);
 ColorToCMYK(c, cy, m, y, k);
 FChange := false;
 SetMagenta(m);
 SetCyan(cy);
 SetBlack(k);
 SetYellow(y);
 FManual := false;
 FChange := true;
 if Assigned(OnChange) then OnChange(Self);
end;

function TYColorPicker.GetArrowPos: integer;
begin
 Result := ArrowPosFromYellow(FYellow);
end;

procedure TYColorPicker.Execute(tbaAction: integer);
begin
 case tbaAction of
  TBA_Resize: SetYellow(FYellow);
  TBA_Paint: Canvas.StretchDraw(FPickRect, FYBmp);
  TBA_MouseMove: FYellow := YellowFromArrowPos(FArrowPos);
  TBA_MouseDown: FYellow := YellowFromArrowPos(FArrowPos);
  TBA_MouseUp: FYellow := YellowFromArrowPos(FArrowPos);
  TBA_WheelUp: SetYellow(FYellow + Increment);
  TBA_WheelDown: SetYellow(FYellow - Increment);
  TBA_VKRight: SetYellow(FYellow + Increment);
  TBA_VKCtrlRight: SetYellow(255);
  TBA_VKLeft: SetYellow(FYellow - Increment);
  TBA_VKCtrlLeft: SetYellow(0);
  TBA_VKUp: SetYellow(FYellow + Increment);
  TBA_VKCtrlUp: SetYellow(255);
  TBA_VKDown: SetYellow(FYellow - Increment);
  TBA_VKCtrlDown: SetYellow(0);
  TBA_RedoBMP: CreateYGradient;
 end;
end;

end.
