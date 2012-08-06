unit RColorPicker;

interface

uses
 Windows, Messages, SysUtils, Classes, Controls, Graphics, Forms,
 mbTrackBarPicker, HTMLColors, Scanlines;

type
 TRColorPicker = class(TmbTrackBarPicker)
 private
  FRed, FGreen, FBlue: integer;
  FBmp: TBitmap;

  function ArrowPosFromRed(r: integer): integer;
  function RedFromArrowPos(p: integer): integer;
  function GetSelectedColor: TColor;
  procedure SetSelectedColor(c: TColor);
  procedure CreateRGradient;
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
  property Red: integer read FRed write SetRed default 255;
  property Green: integer read FGreen write SetGreen default 122;
  property Blue: integer read FBlue write SetBlue default 122;
  property SelectedColor: TColor read GetSelectedColor write SetSelectedColor default clRed;
  property Layout default lyVertical;
 end;

procedure Register;

implementation

procedure Register;
begin
 RegisterComponents('mbColor Lib', [TRColorPicker]);
end;

{TRColorPicker}

constructor TRColorPicker.Create(AOwner: TComponent);
begin
 inherited;
 FBmp := TBitmap.Create;
 FBmp.PixelFormat := pf32bit;
 FBmp.SetSize(12, 256);
 Width := 22;
 Height := 268;
 Layout := lyVertical;
 FRed := 255;
 FGreen := 122;
 FBlue := 122;
 FArrowPos := ArrowPosFromRed(255);
 FChange := false;
 SetRed(255);
 HintFormat := 'Red: %value';
 FManual := false;
 FChange := true;
end;

destructor TRColorPicker.Destroy;
begin
 FBmp.Free;
 inherited Destroy;
end;

procedure TRColorPicker.CreateWnd;
begin
 inherited;
 CreateRGradient;
end;

procedure TRColorPicker.CreateRGradient;
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
      row := FBmp.Scanline[j];
      if not WebSafe then
       row[i] := RGBToRGBQuad(i, FGreen, FBlue)
//       FBmp.Canvas.Pixels[i, j] := RGB(i, FGreen, FBlue)
      else
       row[i] := RGBToRGBQuad(GetWebSafe(RGB(i, FGreen, FBlue)));
//       FBmp.Canvas.Pixels[i, j] := GetWebSafe(RGB(i, FGreen, FBlue));
     end;
  end
 else
  begin
   FBmp.width := 12;
   FBmp.height := 256;
   for i := 0 to 255 do
    begin
     row := FBmp.ScanLine[i];
     for j := 0 to 11 do
      if not WebSafe then
       row[j] := RGBtoRGBQuad(255-i, FGreen, FBlue)
//       FBmp.Canvas.Pixels[j, i] := RGB(255-i, FGreen, FBlue)
      else
       row[j] := RGBtoRGBQuad(GetWebSafe(RGB(255-i, FGreen, FBlue)));
//       FBmp.Canvas.Pixels[j, i] := GetWebSafe(RGB(255-i, FGreen, FBlue));
    end;
  end;
end;

procedure TRColorPicker.SetRed(r: integer);
begin
 if r < 0 then r := 0;
 if r > 255 then r := 255;
 if FRed <> r then
  begin
   FRed := r;
   FArrowPos := ArrowPosFromRed(r);
   FManual := false;
   Invalidate;
   if FChange then
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

procedure TRColorPicker.SetGreen(g: integer);
begin
 if g > 255 then g := 255;
 if g < 0 then g := 0;
 if FGreen <> g then
  begin
   FGreen := g;
   FManual := false;
   CreateRGradient;
   Invalidate;
   if FChange then
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

procedure TRColorPicker.SetBlue(b: integer);
begin
 if b > 255 then b := 255;
 if b < 0 then b := 0;
 if FBlue <> b then
  begin
   FBlue := b;
   FManual := false;
   CreateRGradient;
   Invalidate;
   if FChange then
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

function TRColorPicker.ArrowPosFromRed(r: integer): integer;
var
 a: integer;
begin
 if Layout = lyHorizontal then
  begin
   a := Round(((Width - 12)/255)*r);
   if a > Width - FLimit then a := Width - FLimit;
  end
 else
  begin
   r := 255 - r;
   a := Round(((Height - 12)/255)*r);
   if a > Height - FLimit then a := Height - FLimit;
  end;
 if a < 0 then a := 0;
 Result := a;
end;

function TRColorPicker.RedFromArrowPos(p: integer): integer;
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

function TRColorPicker.GetSelectedColor: TColor;
begin
 if not WebSafe then
  Result := RGB(FRed, FGreen, FBlue)
 else
  Result := GetWebSafe(RGB(FRed, FGreen, FBlue));
end;

function TRColorPicker.GetSelectedValue: integer;
begin
  Result := FRed;
end;

procedure TRColorPicker.SetSelectedColor(c: TColor);
begin
 if WebSafe then c := GetWebSafe(c);
 FChange := false;
 SetGreen(GetGValue(c));
 SetBlue(GetBValue(c));
 SetRed(GetRValue(c));
 FManual := false;
 FChange := true;
 if Assigned(OnChange) then OnChange(Self);
end;

function TRColorPicker.GetArrowPos: integer;
begin
 Result := ArrowPosFromRed(FRed);
end;

procedure TRColorPicker.Execute(tbaAction: integer);
begin
 case tbaAction of
  TBA_Resize: SetRed(FRed);
  TBA_Paint: Canvas.StretchDraw(FPickRect, FBmp);
  TBA_MouseMove: FRed := RedFromArrowPos(FArrowPos);
  TBA_MouseDown: FRed := RedFromArrowPos(FArrowPos);
  TBA_MouseUp: FRed := RedFromArrowPos(FArrowPos);
  TBA_WheelUp: SetRed(FRed + Increment);
  TBA_WheelDown: SetRed(FRed - Increment);
  TBA_VKRight: SetRed(FRed + Increment);
  TBA_VKCtrlRight: SetRed(255);
  TBA_VKLeft: SetRed(FRed - Increment);
  TBA_VKCtrlLeft: SetRed(0);
  TBA_VKUp: SetRed(FRed + Increment);
  TBA_VKCtrlUp: SetRed(255);
  TBA_VKDown: SetRed(FRed - Increment);
  TBA_VKCtrlDown: SetRed(0);
  TBA_RedoBMP: CreateRGradient;
 end;
end;

end.
