unit BColorPicker;

interface

uses
 Windows, Messages, SysUtils, Classes, Controls, Graphics, Forms, 
 mbTrackBarPicker, HTMLColors, Scanlines;

type
 TBColorPicker = class(TmbTrackBarPicker)
 private
  FRed, FGreen, FBlue: integer;
  FBmp: TBitmap;

  function ArrowPosFromBlue(b: integer): integer;
  function BlueFromArrowPos(p: integer): integer;
  function GetSelectedColor: TColor;
  procedure SetSelectedColor(c: TColor);
  procedure CreateBGradient;
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
  property Green: integer read FGreen write SetGreen default 122;
  property Blue: integer read FBlue write SetBlue default 255;
  property SelectedColor: TColor read GetSelectedColor write SetSelectedColor default clRed;
  property Layout default lyVertical;
 end;

procedure Register;

implementation

procedure Register;
begin
 RegisterComponents('mbColor Lib', [TBColorPicker]);
end;

{TBColorPicker}

constructor TBColorPicker.Create(AOwner: TComponent);
begin
 inherited;
 FBmp := TBitmap.Create;
 FBmp.PixelFormat := pf32bit;
 FBmp.SetSize(12, 256);
 Width := 22;
 Height := 268;
 Layout := lyVertical;
 FRed := 122;
 FGreen := 122;
 FBlue := 255;
 FArrowPos := ArrowPosFromBlue(255);
 FChange := false;
 SetBlue(255);
 HintFormat := 'Blue: %value';
 FManual := false;
 FChange := true;
end;

destructor TBColorPicker.Destroy;
begin
 FBmp.Free;
 inherited Destroy;
end;

procedure TBColorPicker.CreateWnd;
begin
 inherited;
 CreateBGradient;
end;

procedure TBColorPicker.CreateBGradient;
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
       row[i] := RGBtoRGBQuad(FRed, FGreen, i)
      else
       row[i] := RGBtoRGBQuad(GetWebSafe(RGB(FRed, FGreen, i)));
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
       row[j] := RGBtoRGBQuad(FRed, FGreen, 255-i)
      else
       row[j] := RGBtoRGBQuad(GetWebSafe(RGB(FRed, FGreen, 255-i)));
    end;
  end;
end;

procedure TBColorPicker.SetRed(r: integer);
begin
 if r < 0 then r := 0;
 if r > 255 then r := 255;
 if FRed <> r then
  begin
   FRed := r;
   FManual := false;
   CreateBGradient;
   Invalidate;
   if FChange then
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

procedure TBColorPicker.SetGreen(g: integer);
begin
 if g > 255 then g := 255;
 if g < 0 then g := 0;
 if FGreen <> g then
  begin
   FGreen := g;
   FManual := false;
   CreateBGradient;
   Invalidate;
   if FChange then
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

procedure TBColorPicker.SetBlue(b: integer);
begin
 if b > 255 then b := 255;
 if b < 0 then b := 0;
 if FBlue <> b then
  begin
   FBlue := b;
   FArrowPos := ArrowPosFromBlue(b);
   FManual := false;
   Invalidate;
   if FChange then
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

function TBColorPicker.ArrowPosFromBlue(b: integer): integer;
var
 a: integer;
begin
 if Layout = lyHorizontal then
  begin
   a := Round(((Width - 12)/255)*b);
   if a > Width - FLimit then a := Width - FLimit;
  end
 else
  begin
   b := 255 - b;
   a := Round(((Height - 12)/255)*b);
   if a > Height - FLimit then a := Height - FLimit;
  end;
 if a < 0 then a := 0;
 Result := a;
end;

function TBColorPicker.BlueFromArrowPos(p: integer): integer;
var
 b: integer;
begin
 if Layout = lyHorizontal then
  b := Round(p/((Width - 12)/255))
 else
  b := Round(255 - p/((Height - 12)/255));
 if b < 0 then b := 0;
 if b > 255 then b := 255;
 Result := b;
end;

function TBColorPicker.GetSelectedColor: TColor;
begin
 if not WebSafe then
  Result := RGB(FRed, FGreen, FBlue)
 else
  Result := GetWebSafe(RGB(FRed, FGreen, FBlue));
end;

function TBColorPicker.GetSelectedValue: integer;
begin
  Result := FBlue;
end;

procedure TBColorPicker.SetSelectedColor(c: TColor);
begin
 if WebSafe then c := GetWebSafe(c);
 FChange := false;
 SetRed(GetRValue(c));
 SetGreen(GetGValue(c));
 SetBlue(GetBValue(c));
 FManual := false;
 FChange := true;
 if Assigned(OnChange) then OnChange(Self);
end;

function TBColorPicker.GetArrowPos: integer;
begin
 Result := ArrowPosFromBlue(FBlue);
end;

procedure TBColorPicker.Execute(tbaAction: integer);
begin
 case tbaAction of
  TBA_Resize: SetBlue(FBlue);
  TBA_Paint: Canvas.StretchDraw(FPickRect, FBmp);
  TBA_MouseMove: FBlue := BlueFromArrowPos(FArrowPos);
  TBA_MouseDown: FBlue := BlueFromArrowPos(FArrowPos);
  TBA_MouseUp: FBlue := BlueFromArrowPos(FArrowPos);
  TBA_WheelUp: SetBlue(FBlue + Increment);
  TBA_WheelDown: SetBlue(FBlue - Increment);
  TBA_VKRight: SetBlue(FBlue + Increment);
  TBA_VKCtrlRight: SetBlue(255);
  TBA_VKLeft: SetBlue(FBlue - Increment);
  TBA_VKCtrlLeft: SetBlue(0);
  TBA_VKUp: SetBlue(FBlue + Increment);
  TBA_VKCtrlUp: SetBlue(255);
  TBA_VKDown: SetBlue(FBlue - Increment);
  TBA_VKCtrlDown: SetBlue(0);
  TBA_RedoBMP: CreateBGradient;
 end;
end;

end.
