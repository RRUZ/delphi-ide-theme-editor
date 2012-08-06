unit KColorPicker;

interface

uses
 Windows, Messages, SysUtils, Classes, Controls, Graphics, Forms,
 RGBCMYKUtils, mbTrackBarPicker, HTMLColors, Scanlines;

type
 TKColorPicker = class(TmbTrackBarPicker)
 private
  FCyan, FMagenta, FYellow, FBlack: integer;
  FKBmp: TBitmap;

  function ArrowPosFromBlack(k: integer): integer;
  function BlackFromArrowPos(p: integer): integer;
  function GetSelectedColor: TColor;
  procedure SetSelectedColor(c: TColor);
  procedure CreateKGradient;
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
 RegisterComponents('mbColor Lib', [TKColorPicker]);
end;

{TKColorPicker}

constructor TKColorPicker.Create(AOwner: TComponent);
begin
 inherited;
 FKBmp := TBitmap.Create;
 FKBmp.PixelFormat := pf32bit;
 FKBmp.SetSize(12, 255);
 Width := 22;
 Height := 267;
 Layout := lyVertical;
 FCyan := 0;
 FMagenta := 0;
 FYellow := 0;
 FBlack := 255;
 FArrowPos := ArrowPosFromBlack(255);
 FChange := false;
 SetBlack(255);
 HintFormat := 'Black: %value';
 FManual := false;
 FChange := true;
end;

destructor TKColorPicker.Destroy;
begin
 FKBmp.Free;
 inherited Destroy;
end;

procedure TKColorPicker.CreateWnd;
begin
 inherited;
 CreateKGradient;
end;

procedure TKColorPicker.CreateKGradient;
var
 i,j: integer;
 row: pRGBQuadArray;
begin
 if FKBmp = nil then
  begin
   FKBmp := TBitmap.Create;
   FKBmp.PixelFormat := pf32bit;
  end;
 if Layout = lyHorizontal then
  begin
   FKBmp.width := 255;
   FKBmp.height := 12;
   for i := 0 to 254 do
    for j := 0 to 11 do
     begin
      row := FKBmp.ScanLine[j];
      if not WebSafe then
       row[i] := RGBToRGBQuad(CMYKtoTColor(FCyan, FMagenta, FYellow, i))
//       FKBmp.Canvas.Pixels[i, j] := CMYKtoTColor(FCyan, FMagenta, FYellow, i)
      else
       row[i] := RGBToRGBQuad(GetWebSafe(CMYKtoTColor(FCyan, FMagenta, FYellow, i)));
//       FKBmp.Canvas.Pixels[i, j] := GetWebSafe(CMYKtoTColor(FCyan, FMagenta, FYellow, i));
     end;
  end
 else
  begin
   FKBmp.width := 12;
   FKBmp.height := 255;
   for i := 0 to 254 do
    begin
     row := FKBmp.Scanline[i];
     for j := 0 to 11 do
      if not WebSafe then
       row[j] := RGBToRGBQuad(CMYKtoTColor(FCyan, FMagenta, FYellow, 255-i))
//       FKBmp.Canvas.Pixels[j, i] := CMYKtoTColor(FCyan, FMagenta, FYellow, 255-i)
      else
       row[j] := RGBToRGBQuad(GetWebSafe(CMYKtoTColor(FCyan, FMagenta, FYellow, 255-i)));
//       FKBmp.Canvas.Pixels[j, i] := GetWebSafe(CMYKtoTColor(FCyan, FMagenta, FYellow, 255-i));
    end;
  end;
end;

procedure TKColorPicker.SetBlack(k: integer);
begin
 if k < 0 then k := 0;
 if k > 255 then k := 255;
 if FBlack <> k then
  begin
   FBlack := k;
   FArrowPos := ArrowPosFromBlack(k);
   FManual := false;
   Invalidate;
   if FChange then
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

procedure TKColorPicker.SetMagenta(m: integer);
begin
 if m > 255 then m := 255;
 if m < 0 then m := 0;
 if FMagenta <> m then
  begin
   FMagenta := m;
   FManual := false;
   CreateKGradient;
   Invalidate;
   if FChange then
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

procedure TKColorPicker.SetYellow(y: integer);
begin
 if y > 255 then y := 255;
 if y < 0 then y := 0;
 if FYellow <> y then
  begin
   FYellow := y;
   FManual := false;
   CreateKGradient;
   Invalidate;
   if FChange then
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

procedure TKColorPicker.SetCyan(c: integer);
begin
 if c > 255 then c := 255;
 if c < 0 then c := 0;
 if FCyan <> c then
  begin
   FCyan := c;
   FManual := false;
   CreateKGradient;
   Invalidate;
   if FChange then
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

function TKColorPicker.ArrowPosFromBlack(k: integer): integer;
var
 a: integer;
begin
 if Layout = lyHorizontal then
  begin
   a := Round(((Width - 12)/255)*k);
   if a > Width - FLimit then a := Width - FLimit;
  end
 else
  begin
   k := 255 - k;
   a := Round(((Height - 12)/255)*k);
   if a > Height - FLimit then a := Height - FLimit;
  end;
 if a < 0 then a := 0;
 Result := a;
end;

function TKColorPicker.BlackFromArrowPos(p: integer): integer;
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

function TKColorPicker.GetSelectedColor: TColor;
begin
 if not WebSafe then
  Result := CMYKtoTColor(FCyan, FMagenta, FYellow, FBlack)
 else
  Result := GetWebSafe(CMYKtoTColor(FCyan, FMagenta, FYellow, FBlack));
end;

function TKColorPicker.GetSelectedValue: integer;
begin
  Result := FBlack;
end;

procedure TKColorPicker.SetSelectedColor(c: TColor);
var
 cy, m, y, k: integer;
begin
 if WebSafe then c := GetWebSafe(c);
 ColorToCMYK(c, cy, m, y, k);
 FChange := false;
 SetMagenta(m);
 SetYellow(y);
 SetCyan(cy);
 SetBlack(k);
 FManual := false;
 FChange := true;
 if Assigned(OnChange) then OnChange(Self);
end;

function TKColorPicker.GetArrowPos: integer;
begin
 Result := ArrowPosFromBlack(FBlack);
end;

procedure TKColorPicker.Execute(tbaAction: integer);
begin
 case tbaAction of
  TBA_Resize: SetBlack(FBlack);
  TBA_Paint: Canvas.StretchDraw(FPickRect, FKBmp);
  TBA_MouseMove: FBlack := BlackFromArrowPos(FArrowPos);
  TBA_MouseDown: FBlack := BlackFromArrowPos(FArrowPos);
  TBA_MouseUp: FBlack := BlackFromArrowPos(FArrowPos);
  TBA_WheelUp: SetBlack(FBlack + Increment);
  TBA_WheelDown: SetBlack(FBlack - Increment);
  TBA_VKRight: SetBlack(FBlack + Increment);
  TBA_VKCtrlRight: SetBlack(255);
  TBA_VKLeft: SetBlack(FBlack - Increment);
  TBA_VKCtrlLeft: SetBlack(0);
  TBA_VKUp: SetBlack(FBlack + Increment);
  TBA_VKCtrlUp: SetBlack(255);
  TBA_VKDown: SetBlack(FBlack - Increment);
  TBA_VKCtrlDown: SetBlack(0);
  TBA_RedoBMP: CreateKGradient;
 end;
end;

end.
