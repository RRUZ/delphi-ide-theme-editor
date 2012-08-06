unit GColorPicker;

interface

uses
 Windows, Messages, SysUtils, Classes, Controls, Graphics, Forms,
 mbTrackBarPicker, HTMLColors, Scanlines;

type
 TGColorPicker = class(TmbTrackBarPicker)
 private
  FRed, FGreen, FBlue: integer;
  FBmp: TBitmap;

  function ArrowPosFromGreen(g: integer): integer;
  function GreenFromArrowPos(p: integer): integer;
  function GetSelectedColor: TColor;
  procedure SetSelectedColor(c: TColor);
  procedure CreateGGradient;
  procedure SetRed(r: integer);
  procedure SetGreen(g: integer);
  procedure SetBlue(b: integer);
 protected
  procedure CreateWnd; override;
  procedure Execute(tbaAction: integer); override;
  function GetArrowPos: integer; override;
  function GetSelectedValue: integer; override;
 public
  constructor Create(AOwner: TComponent); override;
  destructor Destroy; override;
 published
  property Red: integer read FRed write SetRed default 122;
  property Green: integer read FGreen write SetGreen default 255;
  property Blue: integer read FBlue write SetBlue default 122;
  property SelectedColor: TColor read GetSelectedColor write SetSelectedColor default clRed;
  property Layout default lyVertical;
 end;

procedure Register;

implementation

procedure Register;
begin
 RegisterComponents('mbColor Lib', [TGColorPicker]);
end;

{TGColorPicker}

constructor TGColorPicker.Create(AOwner: TComponent);
begin
 inherited;
 FBmp := TBitmap.Create;
 FBmp.PixelFormat := pf32bit;
 FBmp.SetSize(12, 256);
 Width := 22;
 Height := 268;
 Layout := lyVertical;
 FRed := 122;
 FGreen := 255;
 FBlue := 122;
 FArrowPos := ArrowPosFromGreen(255);
 FChange := false;
 SetGreen(255);
 HintFormat := 'Green: %value';
 FManual := false;
 FChange := true;
end;

destructor TGColorPicker.Destroy;
begin
 FBmp.Free;
 inherited Destroy;
end;

procedure TGColorPicker.CreateWnd;
begin
 inherited;
 CreateGGradient;
end;

procedure TGColorPicker.CreateGGradient;
var
 i,j: integer;
 row: pRGBQuadArray;
begin
 if FBmp = nil then
  begin
   FBmp := TBitmap.Create;
   FBmp.PixelFormat := pf32bit;
  end;
 if Layout = lyHorizontal then
  begin
   FBmp.width := 256;
   FBmp.height := 12;
   for i := 0 to 255 do
    for j := 0 to 11 do
     begin
      row := FBmp.ScanLine[j];
      if not WebSafe then
       row[i] := RGBtoRGBQuad(FRed, i, FBlue)
//       FBmp.Canvas.Pixels[i, j] := RGB(FRed, i, FBlue)
      else
       row[i] := RGBtoRGBQuad(GetWebSafe(RGB(FRed, i, FBlue)));
//       FBmp.Canvas.Pixels[i, j] := GetWebSafe(RGB(FRed, i, FBlue));
     end;
  end
 else
  begin
   FBmp.width := 12;
   FBmp.height := 256;
   for i := 0 to 255 do
    begin
     row := FBmp.Scanline[i];
     for j := 0 to 11 do
      if not WebSafe then
       row[j] := RGBtoRGBQuad(FRed, 255-i, FBlue)
      else
       row[j] := RGBtoRGBQuad(GetWebSafe(RGB(FRed, 255-i, FBlue)));
    end;
  end;
end;

procedure TGColorPicker.SetRed(r: integer);
begin
 if r < 0 then r := 0;
 if r > 255 then r := 255;
 if FRed <> r then
  begin
   FRed := r;
   FManual := false;
   CreateGGradient;
   Invalidate;
   if FChange then
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

procedure TGColorPicker.SetGreen(g: integer);
begin
 if g > 255 then g := 255;
 if g < 0 then g := 0;
 if FGreen <> g then
  begin
   FGreen := g;
   FArrowPos := ArrowPosFromGreen(g);
   FManual := false;
   Invalidate;
   if FChange then
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

procedure TGColorPicker.SetBlue(b: integer);
begin
 if b > 255 then b := 255;
 if b < 0 then b := 0;
 if FBlue <> b then
  begin
   FBlue := b;
   FManual := false;
   CreateGGradient;
   Invalidate;
   if FChange then
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

function TGColorPicker.ArrowPosFromGreen(g: integer): integer;
var
 a: integer;
begin
 if Layout = lyHorizontal then
  begin
   a := Round(((Width - 12)/255)*g);
   if a > Width - FLimit then a := Width - FLimit;
  end
 else
  begin
   g := 255 - g;
   a := Round(((Height - 12)/255)*g);
   if a > Height - FLimit then a := Height - FLimit;
  end;
 if a < 0 then a := 0;
 Result := a;
end;

function TGColorPicker.GreenFromArrowPos(p: integer): integer;
var
 g: integer;
begin
 if Layout = lyHorizontal then
  g := Round(p/((Width - 12)/255))
 else
  g := Round(255 - p/((Height - 12)/255));
 if g < 0 then g := 0;
 if g > 255 then g := 255;
 Result := g;
end;

function TGColorPicker.GetSelectedColor: TColor;
begin
 if not WebSafe then
  Result := RGB(FRed, FGreen, FBlue)
 else
  Result := GetWebSafe(RGB(FRed, FGreen, FBlue));
end;

function TGColorPicker.GetSelectedValue: integer;
begin
  Result := FGreen;
end;

procedure TGColorPicker.SetSelectedColor(c: TColor);
begin
 if WebSafe then c := GetWebSafe(c);
 FChange := false;
 SetRed(GetRValue(c));
 SetBlue(GetBValue(c));
 SetGreen(GetGValue(c));
 FManual := false;
 FChange := true;
 if Assigned(OnChange) then OnChange(Self);
end;

function TGColorPicker.GetArrowPos: integer;
begin
 Result := ArrowPosFromGreen(FGreen);
end;

procedure TGColorPicker.Execute(tbaAction: integer);
begin
 case tbaAction of
  TBA_Resize: SetGreen(FGreen);
  TBA_Paint: Canvas.StretchDraw(FPickRect, FBmp);
  TBA_MouseMove: FGreen := GreenFromArrowPos(FArrowPos);
  TBA_MouseDown: FGreen := GreenFromArrowPos(FArrowPos);
  TBA_MouseUp: FGreen := GreenFromArrowPos(FArrowPos);
  TBA_WheelUp: SetGreen(FGreen + Increment);
  TBA_WheelDown: SetGreen(FGreen - Increment);
  TBA_VKRight: SetGreen(FGreen + Increment);
  TBA_VKCtrlRight: SetGreen(255);
  TBA_VKLeft: SetGreen(FGreen - Increment);
  TBA_VKCtrlLeft: SetGreen(0);
  TBA_VKUp: SetGreen(FGreen + Increment);
  TBA_VKCtrlUp: SetGreen(255);
  TBA_VKDown: SetGreen(FGreen - Increment);
  TBA_VKCtrlDown: SetGreen(0);
  TBA_RedoBMP: CreateGGradient;
 end;
end;

end.
