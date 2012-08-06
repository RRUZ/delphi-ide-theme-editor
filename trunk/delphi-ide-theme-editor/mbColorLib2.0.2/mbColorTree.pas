unit mbColorTree;

interface

{$I mxs.inc}

uses
  SysUtils, Windows, Messages, Classes, Controls, ComCtrls, Graphics,
  {$IFDEF DELPHI_7_UP} Themes, {$ENDIF} {$IFDEF DELPHI_6_UP}GraphUtil,{$ENDIF}
  ImgList, HTMLColors, RGBHSLUtils, RGBHSVUtils, RGBCMYKUtils, RGBCIEUtils,
  Forms;

type
  {$IFNDEF DELPHI_6_UP}
  TScrollDirection = (sdLeft, sdRight, sdUp, sdDown);
  {$ENDIF}

  TmbColor = record
   name: string;
   value: TColor;
  end;

  TDrawCaptionEvent = procedure (Sender: TObject; AIndex: integer; AFont: TFont; var AText: string; Selected: boolean) of object;
  TDrawLabelEvent = procedure (Sender: TObject; AIndex: integer; AFont: TFont; var AText: string) of object;
  TGetHintEvent = procedure (AIndex: integer; var AHint: string; var Handled: boolean) of object;

  TmbColorTree = class(TCustomTreeView)
  private
   dummy: TCustomImageList;
   FInfo1, FInfo2: string;
   FInfoLabel: string;
   FDraw: TDrawCaptionEvent;
   FDraw1, FDraw2, FDraw3: TDrawLabelEvent;
   mx, my: integer;
   FGetHint: TGetHintEvent;
   FOnStartDrag: TStartDragEvent;
   FOnEndDrag: TEndDragEvent;

   procedure SetInfo1(Value: string);
   procedure SetInfo2(Value: string);
   procedure SetInfoLabel(Value: string);
  protected
   function CanChange(Node: TTreeNode): Boolean; override;
   procedure CMHintShow(var Message: TCMHintShow); message CM_HINTSHOW;
   procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
   procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
   function CustomDrawItem(Node: TTreeNode; State: TCustomDrawState;
      Stage: TCustomDrawStage; var PaintImages: Boolean): Boolean; override;
   function IsCustomDrawn(Target: TCustomDrawTarget; Stage: TCustomDrawStage): Boolean; {$IFDEF DELPHI_7_UP}override;{$ENDIF}
   procedure DrawColorItem(R: TRect; Selected: boolean; Index: integer; itemText: string; Expanded: boolean); dynamic;
   procedure DrawInfoItem(R: TRect; Index: integer); dynamic;
   procedure DoArrow(c: TCanvas; dir: TScrollDirection; p: TPoint; sel: boolean);
  public
   Colors: array of TmbColor;

   constructor Create(AOwner: TComponent); override;
   destructor Destroy; override;

   procedure UpdateColors;
   procedure AddColor(Name: string; Value: TColor; refresh: boolean = true);
   procedure ClearColors;
   procedure DeleteColor(Index: integer; refresh: boolean = true);
   procedure DeleteColorByName(Name: string; All: boolean);
   procedure DeleteColorByValue(Value: TColor; All: boolean);
   procedure InsertColor(Index: integer; Name: string; Value: TColor);
   function ColorCount: integer;
  published
   property InfoLabelText: string read FInfoLabel write SetInfoLabel;
   property InfoDisplay1: string read FInfo1 write SetInfo1;
   property InfoDisplay2: string read FInfo2 write SetInfo2;
   property Align;
   property Anchors;
   property AutoExpand;
   property BevelEdges;
   property BevelInner;
   property BevelOuter;
   property BevelKind default bkNone;
   property BevelWidth;
   property BorderStyle;
   property BorderWidth;
   property ChangeDelay;
   property Ctl3D;
   property Constraints;
   property Color;
   property DragKind;
   property DragCursor;
   property DragMode;
   property Enabled;
   property Font;
   property Indent;
   {$IFDEF DELPHI_7_UP}
   property MultiSelect;
   property MultiSelectStyle;
   {$ENDIF}
   property ParentColor default False;
   property ParentCtl3D;
   property ParentFont;
   property ParentShowHint;
   property PopupMenu;
   property RightClickSelect;
   property ShowHint;
   property SortType;
   property TabOrder;
   property TabStop default True;
   property ToolTips;
   property Visible;

   property OnGetHint: TGetHintEvent read FGetHint write FGetHint;
   property OnDrawCaption: TDrawCaptionEvent read FDraw write FDraw;
   property OnDrawInfoLabel: TDrawLabelEvent read FDraw1 write FDraw1;
   property OnDrawInfoDisplay1: TDrawLabelEvent read FDraw2 write FDraw2;
   property OnDrawInfoDisplay2: TDrawLabelEvent read FDraw3 write FDraw3;
   {$IFDEF DELPHI_7_UP}
   property OnAddition;
   property OnCreateNodeClass;
   {$ENDIF}
   property OnAdvancedCustomDraw;
   property OnAdvancedCustomDrawItem;
   property OnChange;
   property OnChanging;
   property OnClick;
   property OnCollapsed;
   property OnCollapsing;
   property OnCompare;
   property OnContextPopup;
   property OnCustomDraw;
   property OnCustomDrawItem;
   property OnDblClick;
   property OnDeletion;
   property OnDragDrop;
   property OnDragOver;
   property OnEndDock;
   property OnEndDrag: TEndDragEvent read FOnEndDrag write FOnEndDrag;
   property OnEnter;
   property OnExit;
   property OnExpanding;
   property OnExpanded;
   property OnKeyDown;
   property OnKeyPress;
   property OnKeyUp;
   property OnMouseDown;
   property OnMouseMove;
   property OnMouseUp;
   property OnStartDock;
   property OnStartDrag: TStartDragEvent read FOnStartDrag write FOnStartDrag;
   property Items;
  end;

procedure Register;

implementation

uses PalUtils;

procedure Register;
begin
  RegisterComponents('mbColor Lib', [TmbColorTree]);
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

{  TmbColorTree  }

constructor TmbColorTree.Create(AOwner: TComponent);
begin
 inherited;
 ControlStyle := ControlStyle + [csDisplayDragImage];
 MaxHue := 360;
 MaxSat := 255;
 MaxLum := 255;
 ReadOnly := true;
 ShowButtons := false;
 ShowLines := false;
 ShowRoot := true;
 RowSelect := true;
 HotTrack := false;
 SetLength(Colors, 0);
 dummy := TCustomImageList.Create(Self);
 dummy.Width := 48;
 dummy.Height := 48;
 Images := dummy;
 FInfoLabel := 'Color Values:';
 FInfo1 := 'RGB: %r.%g.%b';
 FInfo2 := 'HEX: #%hex';
end;

destructor TmbColorTree.Destroy;
begin
 dummy.Free;
 inherited;
end;

procedure TmbColorTree.UpdateColors;
var
 i: integer;
 n: TTreeNode;
begin
 Items.Clear;
 for i := 0 to Length(Colors) - 1 do
  begin
   n := Items.Add(TopItem, Colors[i].name);
   Items.AddChild(n, '');
  end;
end;

function TmbColorTree.CanChange(Node: TTreeNode): Boolean;
begin
 Result := Node.HasChildren;
end;

procedure TmbColorTree.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
 r: TRect;
begin
 inherited;
 if (ssShift in Shift) or (ssCtrl in Shift) then Exit;
 if Selected <> nil then
  r := Selected.DisplayRect(false)
 else
  Exit;
 if (x > r.Right - 15) and (x < r.Right - 3) and (y > r.Top + 13) and (y < r.Top + 30) then
  if (Selected.HasChildren) and PtInRect(r, Point(x, y)) then
   begin
    if selected.Expanded then
     Selected.Collapse(false)
    else
     Selected.Expand(false);
    Invalidate;
   end;
end;

procedure TmbColorTree.MouseMove(Shift: TShiftState; X, Y: Integer);
var
 r: TRect;
begin
 inherited;
 mx := x;
 my := y;
 if GetNodeAt(x, y) <> nil then
  r := GetNodeAt(x, y).DisplayRect(false)
 else
  begin
   Cursor := crDefault;
   Exit;
  end;

 if (x > r.Right - 15) and (x < r.Right - 3) and (y > r.Top + 13) and (y < r.Top + 30) then
  begin
   if (GetNodeAt(x, y).HasChildren) and PtInRect(r, Point(x, y)) then
    Cursor := crHandPoint
   else
    Cursor := crDefault;
  end
 else
  Cursor := crDefault;
end;

function TmbColorTree.CustomDrawItem(Node: TTreeNode; State: TCustomDrawState;
  Stage: TCustomDrawStage; var PaintImages: Boolean): Boolean;
begin
 Result := true;
 if Length(Colors) = 0 then Exit;
 if Node.HasChildren then
  DrawColorItem(Node.DisplayRect(false), cdsSelected in State, node.Index, node.Text, node.Expanded)
 else
  DrawInfoItem(Node.DisplayRect(false), node.Parent.Index);
end;

procedure TmbColorTree.DoArrow(c: TCanvas; dir: TScrollDirection; p: TPoint; sel: boolean);
var
 b: TBitmap;
begin
 b := TBitmap.Create;
 try
  b.Height := 12;
  b.Width := 12;
  if Sel then
   begin
    b.Canvas.Brush.Color := clHighlight;
    b.Canvas.Pen.Color := clHighlightText;
   end
  else
   begin
    b.Canvas.Brush.Color := clBtnFace;
    b.Canvas.Pen.Color := clWindowText;
   end;
  b.Canvas.FillRect(B.Canvas.ClipRect);
  case dir of
   sdDown: DrawArrow(b.Canvas, dir, Point(2, 3), 3);
   sdRight: DrawArrow(b.Canvas, dir, Point(1, 2), 3);
  end;
  c.Draw(p.x, p.y, b);
 finally
  b.Free;
 end;
end;

procedure TmbColorTree.DrawColorItem(R: TRect; Selected: boolean; Index: integer; itemText: string; Expanded: boolean);
var
 SR, TR: TRect;
begin
  with Canvas do
   begin
    //background
    Pen.Color := clWindow;
    if Selected then
     Brush.Color := clHighlight
    else
     Brush.Color := clBtnFace;
    FillRect(R);
    MoveTo(R.Left, R.Bottom - 1);
    LineTo(R.Right, R.Bottom - 1);
    //swatches
    SR := Rect(R.Left + 6, R.Top + 6, R.Left + 42, R.Top + 42);
    Brush.Color := Colors[Index].value;
    if Selected then
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
    if Selected then
     begin
      Brush.Color := clHighlightText;
      Pen.Color := clHighlightText;
     end
    else
     begin
      Brush.Color := clWindowText;
      Pen.Color := clWindowText;
     end;
    TR := Rect(R.Left + 48, R.Top + (48 - TextHeight(itemText)) div 2, R.Right - 15, R.Bottom);
   if Assigned(FDraw) then FDraw(Self, Index, Canvas.Font, itemText, Selected);
   DrawText(Canvas.Handle, PChar(itemText), Length(itemText), TR, DT_LEFT or DT_NOCLIP or DT_END_ELLIPSIS);
    if R.Right > 60 then
     begin
      if Expanded then
       DoArrow(Canvas, sdDown, Point(R.Right - 13, R.Top + 20), selected)
      else
       DoArrow(Canvas, sdRight, Point(R.Right - 10, R.Top + 18), selected);
     end;
   end;
end;

procedure TmbColorTree.DrawInfoItem(R: TRect; Index: integer);
var
 b: TBitmap;
 BR, TR: TRect;
 i, fx: integer;
 s: string;
begin
 b := TBitmap.Create;
 try
  b.Width := R.Right - R.Left;
  b.Height := R.Bottom - R.Top;
  BR := b.Canvas.ClipRect;
  b.Canvas.Font.Assign(Font);
  with b.Canvas do
   begin
    Brush.Color := Blend(clBtnFace, clWindow, 30);
    FillRect(BR);
    BR := Rect(BR.Left + 42, BR.Top, BR.Right, BR.Bottom);
    Brush.Color := clWindow;
    FillRect(BR);
    Inc(BR.Left, 6);
    Font.Style := [];
    Font.Size := 7;

    s := FInfoLabel;
    TR := Rect(BR.Left, BR.Top + 2, BR.Right, BR.Top + 12);
    if Assigned(FDraw1) then FDraw1(Self, Index, Canvas.Font, s);
    DrawText(b.Canvas.Handle, PChar(s), Length(s), TR, DT_LEFT or DT_END_ELLIPSIS or DT_NOCLIP);

    fX := BR.Left;
    for i := 0 to (BR.Right - 2 - BR.Left) div 2 do
     begin
      Pixels[fX, BR.Top + 4 + TextHeight(s)] := clGray;
      fX := fX + 2;
     end;

    s := FormatHint(FInfo1, Colors[Index].value);
    TR := Rect(BR.Left, BR.Top + (BR.Bottom - BR.Top) div 3 + 2, BR.Right, BR.Top + 12);
    if Assigned(FDraw2) then FDraw2(Self, Index, Canvas.Font, s);
    DrawText(b.Canvas.Handle, PChar(s), Length(s), TR, DT_LEFT or DT_END_ELLIPSIS or DT_NOCLIP);

    fX := BR.Left;
    for i := 0 to (BR.Right - 2 - BR.Left) div 2 do
     begin
      Pixels[fX, BR.Top + (BR.Bottom - BR.Top) div 3 + 4 + TextHeight(s)] := clGray;
      fX := fX + 2;
     end;

    s := FormatHint(FInfo2, Colors[Index].value);
    TR := Rect(BR.Left, BR.Top + 2*((BR.Bottom - BR.Top) div 3) + 2, BR.Right, BR.Top + 12);
    if Assigned(FDraw3) then FDraw3(Self, Index, Canvas.Font, s);
    DrawText(b.Canvas.Handle, PChar(s), Length(s), TR, DT_LEFT or DT_END_ELLIPSIS or DT_NOCLIP);
   end;
  Canvas.Draw(R.Left, R.Top, b);
 finally
  b.Free;
 end;
end;

function TmbColorTree.IsCustomDrawn(Target: TCustomDrawTarget; Stage: TCustomDrawStage): Boolean;
begin
 Result := true;
end;

procedure TmbColorTree.SetInfoLabel(Value: string);
begin
 if FInfoLabel <> Value then
  begin
   FInfoLabel := Value;
   Invalidate;
  end;
end;

procedure TmbColorTree.SetInfo1(Value: string);
begin
 if FInfo1 <> Value then
  begin
   FInfo1 := Value;
   Invalidate;
  end;
end;

procedure TmbColorTree.SetInfo2(Value: string);
begin
 if FInfo2 <> Value then
  begin
   FInfo2 := Value;
   Invalidate;
  end;
end;

procedure TmbColorTree.AddColor(Name: string; Value: TColor; refresh: boolean = true);
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

procedure TmbColorTree.ClearColors;
begin
 SetLength(Colors, 0);
 UpdateColors;
end;

function TmbColorTree.ColorCount: integer;
begin
 Result := Length(Colors);
end;

procedure TmbColorTree.DeleteColor(Index: integer; refresh: boolean = true);
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

procedure TmbColorTree.DeleteColorByName(Name: string; All: boolean);
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

procedure TmbColorTree.DeleteColorByValue(Value: TColor; All: boolean);
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

procedure TmbColorTree.InsertColor(Index: integer; Name: string; Value: TColor);
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

procedure TmbColorTree.CMHintShow(var Message: TCMHintShow);
var
 Handled: boolean;
 i: integer;
 n: TTreeNode;
begin
if PtInRect(ClientRect, Point(mx, my)) and ShowHint and not Dragging then
 begin
  n := GetNodeAt(mx, my);
  if n <> nil then
   begin
    if not n.HasChildren then
     i := n.Parent.Index
    else
     i := n.Index;
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
 end;
 inherited;
end;

end.
