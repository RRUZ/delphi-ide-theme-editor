unit mbColorList;

interface

{$I mxs.inc}

uses
  SysUtils, Windows, Messages, Classes, Controls, StdCtrls, Graphics,
  {$IFDEF DELPHI_7_UP} Themes, {$ENDIF} {$IFDEF DELPHI_6_UP}GraphUtil,{$ENDIF}
  HTMLColors, RGBHSLUtils, RGBHSVUtils, RGBCMYKUtils, RGBCIEUtils, Forms,
  PalUtils;

type
  {$IFNDEF DELPHI_6_UP}
  TScrollDirection = (sdLeft, sdRight, sdUp, sdDown);
  {$ENDIF}

  TmbColor = record
   name: string;
   value: TColor;
  end;

  TDrawCaptionEvent = procedure (Sender: TObject; AIndex: integer; AFont: TFont; var AText: string; Selected: boolean) of object;
  TGetHintEvent = procedure (AIndex: integer; var AHint: string; var Handled: boolean) of object;

  TmbColorList = class(TCustomListBox)
  private
   FDraw: TDrawCaptionEvent;
   mx, my: integer;
   FGetHint: TGetHintEvent;
  protected
   procedure CMHintShow(var Message: TCMHintShow); message CM_HINTSHOW;
   procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
   procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
  public
   Colors: array of TmbColor;

   constructor Create(AOwner: TComponent); override;

   procedure UpdateColors;
   procedure AddColor(Name: string; Value: TColor; refresh: boolean = true);
   procedure ClearColors;
   procedure DeleteColor(Index: integer; refresh: boolean = true);
   procedure DeleteColorByName(Name: string; All: boolean);
   procedure DeleteColorByValue(Value: TColor; All: boolean);
   procedure InsertColor(Index: integer; Name: string; Value: TColor);
   function ColorCount: integer;
  published
   property BevelKind default bkNone;
   property ParentColor default False;
   property TabStop default True;
   {$IFDEF DELPHI_7_UP}
   property AutoComplete;
   property ScrollWidth;
   {$ENDIF}
   property Align;
   property Anchors;
   property BevelEdges;
   property BevelInner;
   property BevelOuter;
   property BiDiMode;
   property BorderStyle;
   property Color;
   property Columns;
   property Constraints;
   property Ctl3D;
   property DragCursor;
   property DragKind;
   property DragMode;
   property Enabled;
   property ExtendedSelect;
   property Font;
   property ImeMode;
   property ImeName;
   property IntegralHeight default true;
   property ItemHeight default 48;
   property Items;
   property MultiSelect;
   property ParentBiDiMode;
   property ParentCtl3D;
   property ParentFont;
   property ParentShowHint;
   property PopupMenu;
   property ShowHint;
   property Sorted;
   property TabOrder;
   property TabWidth;
   property Visible;

   property OnDrawCaption: TDrawCaptionEvent read FDraw write FDraw;
   property OnGetHint: TGetHintEvent read FGetHint write FGetHint;
   property OnContextPopup;
   {$IFDEF DELPHI_7_UP}
   property OnData;
   property OnDataFind;
   property OnDataObject;
   {$ENDIF}
   property OnDblClick;
   property OnDragDrop;
   property OnDragOver;
   property OnDrawItem;
   property OnEndDock;
   property OnEndDrag;
   property OnEnter;
   property OnExit;
   property OnKeyDown;
   property OnKeyPress;
   property OnKeyUp;
   property OnMeasureItem;
   property OnMouseDown;
   property OnMouseMove;
   property OnMouseUp;
   property OnStartDock;
   property OnStartDrag;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('mbColor Lib', [TmbColorList]);
end;

//taken from GraphUtil, only for Delphi 5
{$IFNDEF DELPHI_6_UP}

procedure DrawArrow(ACanvas: TCanvas; Direction: TScrollDirection;
  Location: TPoint; Size: Integer);
const
  ArrowPts: array[TScrollDirection, 0..2] of TPoint =
    (((X:1; Y:0), (X:0; Y:1), (X:1; Y:2)),
     ((X:0; Y:0), (X:1; Y:1), (X:0; Y:2)),
     ((X:0; Y:1), (X:1; Y:0), (X:2; Y:1)),
     ((X:0; Y:0), (X:1; Y:1), (X:2; Y:0)));
var
  I: Integer;
  Pts: array[0..2] of TPoint;
  OldWidth: Integer;
  OldColor: TColor;
begin
  if ACanvas = nil then exit;
  OldColor := ACanvas.Brush.Color;
  ACanvas.Brush.Color := ACanvas.Pen.Color;
  Move(ArrowPts[Direction], Pts, SizeOf(Pts));
  for I := 0 to 2 do
    Pts[I] := Point(Pts[I].x * Size + Location.X, Pts[I].y * Size + Location.Y);
  with ACanvas do
  begin
    OldWidth := Pen.Width;
    Pen.Width := 1;
    Polygon(Pts);
    Pen.Width := OldWidth;
    Brush.Color := OldColor;
  end;
end;

{$ENDIF}

constructor TmbColorList.Create(AOwner: TComponent);
begin
 inherited;
 MaxHue := 360;
 MaxSat := 255;
 MaxLum := 255;
 style := lbOwnerDrawFixed;
 SetLength(Colors, 0);
 ItemHeight := 48;
 IntegralHeight := true;
 mx := -1;
 my := -1;
end;

procedure TmbColorList.UpdateColors;
var
 i: integer;
begin
 Items.Clear;
 for i := 0 to Length(Colors) - 1 do
  Items.Add(Colors[i].name);
end;

procedure TmbColorList.DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
 SR, TR, R: TRect;
 itemText: string;
begin
 if Length(Colors) = 0 then Exit;
 R := Rect;
 with Canvas do
  begin
   //background
   Pen.Color := clWindow;
   if odSelected in State then
    Brush.Color := clHighlight
   else
    Brush.Color := clBtnFace;
   FillRect(R);
   MoveTo(R.Left, R.Bottom - 1);
   LineTo(R.Right, R.Bottom - 1);
   //swatches
   SR := Classes.Rect(R.Left + 6, R.Top + 6, R.Left + ItemHeight - 6, R.Top + ItemHeight - 6);
   Brush.Color := Colors[Index].value;
   if odSelected in State then
    begin
     {$IFDEF DELPHI_7_UP}
     if {$IFDEF DELPHI_XE2_UP}StyleServices.Enabled{$ELSE}ThemeServices.ThemesEnabled{$ENDIF} then
      begin
       {$IFDEF DELPHI_XE2_UP}StyleServices{$ELSE}ThemeServices{$ENDIF}.DrawElement(Canvas.Handle, {$IFDEF DELPHI_XE2_UP}StyleServices{$ELSE}ThemeServices{$ENDIF}.GetElementDetails(teEditTextNormal), SR);
       InflateRect(SR, -2, -2);
       Brush.Color := Blend(Colors[Index].value, clBlack, 80);
       FillRect(SR);
       InflateRect(SR, -1, -1);
       Brush.Color := Blend(Colors[Index].value, clBlack, 90);
       FillRect(SR);
       InflateRect(SR, -1, -1);
       Brush.Color := Colors[Index].value;
       FillRect(SR);
      end
     else
     //windows 9x
      begin
     {$ENDIF}
       Pen.Color := clBackground;
       Brush.Color := clWindow;
       Rectangle(SR);
       InflateRect(SR, -1, -1);
       FillRect(SR);
       InflateRect(SR, 1, 1);
       InflateRect(SR, -2, -2);
       Brush.Color := Blend(Colors[Index].value, clBlack, 75);
       FillRect(SR);
       InflateRect(SR, -1, -1);
       Brush.Color := Blend(Colors[Index].value, clBlack, 87);
       FillRect(SR);
       InflateRect(SR, -1, -1);
       Brush.Color := Colors[Index].value;
       FillRect(SR);
      {$IFDEF DELPHI_7_UP}
      end;
      {$ENDIF}
    end
   else
    //not selected
    begin
     //windows XP
     {$IFDEF DELPHI_7_UP}
     if {$IFDEF DELPHI_XE2_UP}StyleServices.Enabled{$ELSE}ThemeServices.ThemesEnabled{$ENDIF} then
      begin
       {$IFDEF DELPHI_XE2_UP}StyleServices{$ELSE}ThemeServices{$ENDIF}.DrawElement(Canvas.Handle, {$IFDEF DELPHI_XE2_UP}StyleServices{$ELSE}ThemeServices{$ENDIF}.GetElementDetails(teEditTextNormal), SR);
       InflateRect(SR, -2, -2);
       Brush.Color := Colors[Index].value;
       FillRect(SR);
      end
     else
     //windows 9x
      begin
     {$ENDIF}
       DrawEdge(Canvas.Handle, SR, BDR_SUNKENOUTER, BF_RECT);
       InflateRect(SR, -2, -2);
       Brush.Color := Colors[Index].value;
       Pen.Color := clBlack;
       Rectangle(SR);
       InflateRect(SR, -1, -1);
       FillRect(SR);
       InflateRect(SR, 1, 1);
      {$IFDEF DELPHI_7_UP}
      end;
      {$ENDIF}
    end;
   //names
   Font.Style := [fsBold];
   if odSelected in State then
    begin
     Brush.Color := clHighlight;
     Pen.Color := clHighlightText;
     Font.Color := clHighlightText;
    end
   else
    begin
     Brush.Color := clBtnFace;
     Pen.Color := clWindowText;
     Font.Color := clWindowText;
    end;
  itemText := Items.Strings[Index];
  TR := Classes.Rect(R.Left + ItemHeight, R.Top + (ItemHeight - TextHeight(itemText)) div 2, R.Right, R.Bottom - (ItemHeight - TextHeight(itemText)) div 2);
  if Assigned(FDraw) then FDraw(Self, Index, Canvas.Font, itemText, odSelected in State);
  DrawText(Canvas.Handle, PChar(itemText), Length(itemText), TR, DT_LEFT or DT_NOCLIP or DT_END_ELLIPSIS);
 end;
end;

procedure TmbColorList.AddColor(Name: string; Value: TColor; refresh: boolean = true);
var
 l: integer;
begin
 l := Length(Colors);
 SetLength(Colors, l + 1);
 Colors[l].name := Name;
 Colors[l].value := Value;
 if refresh then
  UpdateColors;
end;

procedure TmbColorList.ClearColors;
begin
 SetLength(Colors, 0);
 UpdateColors;
end;

function TmbColorList.ColorCount: integer;
begin
 Result := Length(Colors);
end;

procedure TmbColorList.DeleteColor(Index: integer; refresh: boolean = true);
var
 i: integer;
begin
 if Length(Colors) = 0 then
  begin
   raise Exception.Create('There''s nothing to delete! The length of the array is 0.');
   Exit;
  end;

 if Index > Length(Colors) - 1 then
  begin
   raise Exception.Create(Format('List index out of bounds (%d)', [Index]));
   Exit;
  end;

 for i := Index to Length(Colors) - 2 do
  Colors[i] := Colors[i+1];
 SetLength(Colors, Length(Colors) - 1);
 if refresh then
  UpdateColors;
end;

procedure TmbColorList.DeleteColorByName(Name: string; All: boolean);
var
 i: integer;
begin
 for i := Length(Colors) - 1 downto 0 do
  if SameText(Colors[i].name, Name) then
   begin
    DeleteColor(i, false);
    if not All then
     begin
      UpdateColors;
      Exit;
     end;
   end;
 UpdateColors;
end;

procedure TmbColorList.DeleteColorByValue(Value: TColor; All: boolean);
var
 i: integer;
begin
 for i := Length(Colors) - 1 downto 0 do
  if Colors[i].Value = Value then
   begin
    DeleteColor(i, false);
    if not All then
     begin
      UpdateColors;
      Exit;
     end;
   end;
 UpdateColors;
end;

procedure TmbColorList.InsertColor(Index: integer; Name: string; Value: TColor);
var
 i: integer;
begin
 if Index > Length(Colors) - 1 then
  begin
   raise Exception.Create(Format('List index out of bounds (%d)', [Index]));
   Exit;
  end;

 SetLength(Colors, Length(Colors) + 1);
 for i := Length(Colors) - 1 downto Index do
  Colors[i] := Colors[i-1];

 Colors[Index].Name := Name;
 Colors[Index].Value := Value;

 UpdateColors;
end;

procedure TmbColorList.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
 inherited;
 mx := x;
 my := y;
end;

procedure TmbColorList.CMHintShow(var Message: TCMHintShow);
var
 Handled: boolean;
 i: integer;
begin
if PtInRect(ClientRect, Point(mx, my)) and ShowHint then
 begin
  i := ItemAtPos(Point(mx, my), true);
  if i > -1 then
   with TCMHintShow(Message) do
    if not ShowHint then
     Message.Result := 1
    else
     with HintInfo^ do
      begin
       Result := 0;
       ReshowTimeout := 2000;
       HideTimeout := 1000;
       Handled := false;
       if Assigned(FGetHint) then FGetHint(i, HintStr, Handled);
       if Handled then
        HintStr := FormatHint(HintStr, Colors[i].Value)
       else
        HintStr := Colors[i].Name;
      end;
 end;
 inherited;
end;

end.
