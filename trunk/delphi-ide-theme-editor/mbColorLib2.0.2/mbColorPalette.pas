unit mbColorPalette;

interface

{$I mxs.inc}

uses
  Windows, Messages, SysUtils, Classes, Controls, Graphics, {$IFDEF DELPHI_7_UP} Themes, {$ENDIF}
  Forms, HTMLColors, PalUtils, Dialogs;

type
  TMouseLoc = (mlNone, mlOver, mlDown);
  TTransparentStyle = (tsPhotoshop, tsPhotoshop2, tsCorel, tsMicroangelo, tsNone);
  TCellStyle = (csDefault, csCorel);
  TColorCellState = (ccsNone, ccsOver, ccsDown, ccsChecked, ccsCheckedHover);
  TMoveDirection = (mdLeft, mdRight, mdUp, mdDown);
  TPaintCellEvent = procedure (ACanvas: TCanvas; ACellRect: TRect; AColor: TColor; Index: integer; AState: TColorCellState; var AStyle: TTransparentStyle; var PaintingHandled: boolean) of object;
  TCellClickEvent = procedure (Button: TMouseButton; Shift: TShiftState; Index: integer; AColor: TColor; var DontCheck: boolean) of object;
  TGetHintTextEvent = procedure (AColor: TColor; Index: integer; var HintStr: string; var Handled: boolean) of object;
  TArrowKeyEvent = procedure (Key: Word; Shift: TShiftState) of object;

  TmbColorPalette = class(TCustomControl)
  private
   FMouseLoc: TMouseLoc;
   FMouseOver, FMouseDown, FAutoHeight: boolean;
   FColCount, FRowCount, FTop, FLeft, FIndex, FCheckedIndex, FCellSize, FTotalCells: integer;
   FTempBmp, PBack: TBitmap;
   FState: TColorCellState;
   FColors, FNames: TStrings;
   FPalette: TFileName;
   FHintFormat: string;
   FOnChange, FOnColorsChange: TNotifyEvent;
   FMinColors, FMaxColors: integer;
   FSort: TSortMode;
   FOrder: TSortOrder;
   FOld: TColor;
   FOnPaintCell: TPaintCellEvent;
   FTStyle: TTransparentStyle;
   FOnCellClick: TCellClickEvent;
   FOldIndex: integer;
   FOnGetHintText: TGetHintTextEvent;
   FCellStyle: TCellStyle;
   FOnArrowKey: TArrowKeyEvent;

   function GetMoveCellIndex(move: TMoveDirection): integer;
   function GetSelColor: TColor;
   procedure SetCellStyle(s: TCellStyle);
   procedure SetTStyle(s: TTransparentStyle);
   procedure SetCellSize(s: integer);
   procedure SetSortMode(s: TSortMode);
   procedure SetSortOrder(s: TSortOrder);
   procedure SetMinColors(m: integer);
   procedure SetMaxColors(m: integer);
   procedure SetAutoHeight(auto: boolean);
   procedure LoadPalette(FileName: TFileName);
   procedure SetStrings(s: TStrings);
   procedure SetNames(n: TStrings);
   procedure SetSelColor(k: TColor);
   procedure SortColors;
   procedure CalcAutoHeight;
   function GetTotalRowCount: integer;
  protected
   procedure Paint; override;
   procedure PaintTransparentGlyph(ACanvas: TCanvas; R: TRect);
   procedure DrawCell(clr: string);
   procedure DrawCellBack(ACanvas: TCanvas; R: TRect; AIndex: integer);
   procedure ColorsChange(Sender: TObject);
   procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
   procedure Click; override;
   procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
   procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
   procedure CNKeyDown(var Message: TWMKeyDown); message CN_KEYDOWN;
   procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
   procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
   procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
   procedure CMGotFocus(var Message: TCMGotFocus); message CM_ENTER;
   procedure CMLostFocus(var Message: TCMLostFocus); message CM_EXIT;
   procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
   procedure CMHintShow(var Message: TMessage); message CM_HINTSHOW;
   procedure Resize; override;
   procedure SelectCell(i: integer);
   procedure PaintParentBack;
   procedure CreateWnd; override;
  public
   constructor Create(AOwner: TComponent); override;
   destructor Destroy; override;
   function GetColorUnderCursor: TColor;
   function GetSelectedCellRect: TRect;
   function GetIndexUnderCursor: integer;

   property ColorUnderCursor: TColor read GetColorUnderCursor;
   property VisibleRowCount: integer read FRowCount;
   property RowCount: integer read GetTotalRowCount;
   property ColCount: integer read FColCount;
   property IndexUnderCursor: integer read GetIndexUnderCursor;
   procedure SaveColorsAsPalette(FileName: TFileName);
   procedure GeneratePalette(BaseColor: TColor);
   procedure GenerateGradientPalette(Colors: array of TColor);
  published
   property Align;
   property Anchors;
   property Enabled;
   property SortMode: TSortMode read FSort write SetSortMode default smNone;
   property SortOrder: TSortOrder read FOrder write SetSortOrder default soAscending;
   property MinColors: integer read FMinColors write SetMinColors default 0;
   property MaxColors: integer read FMaxColors write SetMaxColors default 0;
   property SelectedCell: integer read FCheckedIndex write SelectCell default -1;
   property SelectedColor: TColor read GetSelColor write SetSelColor default clNone;
   property Colors: TStrings read FColors write SetStrings;
   property Palette: TFileName read FPalette write LoadPalette;
   property HintFormat: string read FHintFormat write FHintFormat;
   property AutoHeight: boolean read FAutoHeight write SetAutoHeight default false;
   property CellSize: integer read FCellSize write SetCellSize default 18;
   property TransparentStyle: TTransparentStyle read FTStyle write SetTStyle default tsNone;
   property CellStyle: TCellStyle read FCellStyle write SetCellStyle default csDefault;
   property ColorNames: TStrings read FNames write SetNames;
   {$IFDEF DELPHI_7_UP}
   property ParentBackground default true;
   {$ENDIF}
   property TabStop default true;
   property TabOrder;
   property ShowHint default false;
   property Constraints;
   property Color;
   property ParentColor;
   property ParentShowHint default true;
   property PopupMenu;
   property Visible;

   property DragCursor;
   property DragKind;
   property DragMode;
   property OnDragDrop;
   property OnDragOver;
   property OnEndDock;
   property OnEndDrag;
   property OnStartDock;
   property OnStartDrag;
   property OnSelColorChange: TNotifyEvent read FOnChange write FOnChange;
   property OnColorsChange: TNotifyEvent read FOnColorsChange write FOnColorsChange;
   property OnPaintCell: TPaintCellEvent read FOnPaintCell write FOnPaintCell;
   property OnCellClick: TCellClickEvent read FOnCellClick write FOnCellClick;
   property OnGetHintText: TGetHintTextEvent read FOnGetHintText write FOnGetHintText;
   property OnArrowKey: TArrowKeyEvent read FOnArrowKey write FOnArrowKey;
   property OnContextPopup;
   property OnMouseMove;
   property OnMouseDown;
   property OnMouseUp;
   property OnKeyDown;
   property OnKeyUp;
   property OnKeyPress;
   property OnResize;
   property OnClick;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('mbColor Lib', [TmbColorPalette]);
end;

constructor TmbColorPalette.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 ControlStyle := ControlStyle - [csAcceptsControls] + [csOpaque];
 DoubleBuffered := true;
 PBack := TBitmap.Create;
 PBack.PixelFormat := pf32bit;
 {$IFDEF DELPHI_7_UP}
 ParentBackground := true;
 {$ENDIF}
 TabStop := true;
 ParentShowHint := true;
 ShowHint := false;
 Width := 180;
 Height := 126;
 FMouseLoc := mlNone;
 FMouseOver := false;
 FMouseDown := false;
 FColCount := 0;
 FRowCount := 0;
 FIndex := -1;
 FCheckedIndex := -1;
 FTop := 0;
 FLeft := 0;
 FCellSize := 18;
 FState := ccsNone;
 FNames := TStringList.Create;
 FColors := TStringList.Create;
 (FColors as TStringList).OnChange := ColorsChange;
 FTotalCells := 0;
 FHintFormat := 'RGB(%r, %g, %b)'#13'Hex: %hex';
 FAutoHeight := false;
 FMinColors := 0;
 FMaxColors := 0;
 FSort := smNone;
 FOrder := soAscending;
 FOld := clNone;
 FTStyle := tsNone;
 FCellStyle := csDefault;
end;

destructor TmbColorPalette.Destroy;
begin
 PBack.Free;
 FNames.Free;
 FColors.Free;
 inherited Destroy;
end;

procedure TmbColorPalette.CalcAutoHeight;
begin
 FColCount := Width div FCellSize;
 if FAutoHeight and (FColCount <> 0) then
  begin
   if FColors.Count mod FColCount > 0 then
    Height := (FColors.Count div FColCount + 1) * FCellSize
   else
    Height := (FColors.Count div FColCount) * FCellSize;
  end;
 if Height = 0 then Height := FCellSize;
 FRowCount := Height div FCellSize;
 Width := FColCount * FCellSize;
end;

function TmbColorPalette.GetTotalRowCount: integer;
begin
if FColCount <> 0 then
 Result := FTotalCells div FColCount
else
 Result := 0;
end;

procedure TmbColorPalette.CreateWnd;
begin
 inherited;
 CalcAutoHeight;
 Invalidate;
end;

procedure TmbColorPalette.PaintParentBack;
{$IFDEF DELPHI_7_UP}
var
 MemDC: HDC;
 OldBMP: HBITMAP;
{$ENDIF}
begin
 if PBack = nil then
  begin
   PBack := TBitmap.Create;
   PBack.PixelFormat := pf32bit;
  end;
 PBack.Width := Width;
 PBack.Height := Height;
 PBack.Canvas.Brush.Color := Color;
 PBack.Canvas.FillRect(PBack.Canvas.ClipRect);
 {$IFDEF DELPHI_7_UP}
 if ParentBackground then
  with {$IFDEF DELPHI_XE2_UP}StyleServices{$ELSE}ThemeServices{$ENDIF} do
   if {$IFDEF DELPHI_XE2_UP}Enabled{$ELSE}ThemesEnabled{$ENDIF} then
    begin
     MemDC := CreateCompatibleDC(0);
     OldBMP := SelectObject(MemDC, PBack.Handle);
     DrawParentBackground(Handle, MemDC, nil, False);
     if OldBMP <> 0 then SelectObject(MemDC, OldBMP);
     if MemDC <> 0 then DeleteDC(MemDC);
    end;
 {$ENDIF}
end;

procedure TmbColorPalette.Paint;
var
 i: integer;
begin
 PaintParentBack;
 //make bmp
 FTempBmp := TBitmap.Create;
 try
  FTempBmp.PixelFormat := pf32bit;
  FTempBmp.Width := Width;
  FTempBmp.Height := Height;
  FTempBmp.Canvas.Brush.Color := Color;
  {$IFDEF DELPHI_7_UP}
  if not ParentBackground then
  {$ENDIF}
   FTempBmp.Canvas.FillRect(FTempBmp.Canvas.ClipRect)
  {$IFDEF DELPHI_7_UP}
  else
   FTempBmp.Canvas.Draw(0, 0, PBack){$ENDIF};
  FTotalCells := FColors.Count - 1;
  //reset counters
  FTop := 0;
  FLeft := 0;
  //draw the cells
  for i := 0 to FColors.Count - 1 do
   begin
    if FColors.Strings[i] <> '' then
    DrawCell(FColors.Strings[i]);
    Inc(FLeft);
   end;
  //draw the result
  Canvas.Draw(0, 0, FTempBmp);
  //csDesiginng border
  if csDesigning in ComponentState then
   begin
    Canvas.Brush.Style := bsClear;
    Canvas.Pen.Style := psDot;
    Canvas.Pen.Color := clBtnShadow;
    Canvas.Rectangle(ClientRect);
    Canvas.Brush.Style := bsSolid;
    Canvas.Pen.Style := psSolid;
   end;
 finally
  FTempBmp.Free;
 end;
end;

procedure TmbColorPalette.DrawCell(clr: string);
var
 R: Trect;
 FCurrentIndex: integer;
 c: TColor;
 Handled: boolean;
begin
 // set props
 if (FLeft + 1) * FCellSize > FTempBmp.width then
  begin
   Inc(FTop);
   FLeft := 0;
  end;
 FCurrentIndex := FTop * FColCount + FLeft;
 R := Rect(FLeft * FCellSize, FTop * FCellSize, (FLeft + 1) * FCellSize, (FTop + 1) * FCellSize);
 //start drawing
 with FTempBmp.Canvas do
  begin
   Brush.Color := Color;
   //get current state
   if FCurrentIndex = FCheckedIndex then
    begin
     if FCheckedIndex = FIndex then
      begin
       if FMouseDown then
        FState := ccsDown
       else
        FState := ccsCheckedHover;
      end
     else
      FState := ccsChecked;
    end
   else
    if FIndex = FCurrentIndex then
     case FMouseLoc of
      mlNone: FState := ccsNone;
      mlOver: FState := ccsOver;
     end
    else
     FState := ccsNone;

   //paint
   DrawCellBack(FTempBmp.Canvas, R, FCurrentIndex);

   // fire the event
   Handled := false;
   if Assigned(FOnPaintCell) then
    case FCellStyle of
     csDefault: FOnPaintCell(FTempBmp.Canvas, R, mbStringToColor(clr), FCurrentIndex, FState, FTStyle, Handled);
     csCorel:
      if FColCount = 1 then
       FOnPaintCell(FTempBmp.Canvas, R, mbStringToColor(clr), FCurrentIndex, FState, FTStyle, Handled)
      else
       FOnPaintCell(FTempBmp.Canvas, Rect(R.Left, R.Top, R.Right + 1, R.Bottom), mbStringToColor(clr), FCurrentIndex, FState, FTStyle, Handled);
    end;
   if not Handled then
    begin
     // if standard colors draw the rect
     if not SameText(clr, 'clCustom') and not SameText(clr, 'clTransparent') then
      case FCellStyle of
       csDefault:
        begin
         InflateRect(R, -3, -3);
         c := mbStringToColor(clr);
         if Enabled then
          begin
           Brush.Color := c;
           Pen.Color := clBtnShadow;
          end
         else
          begin
           Brush.Color := clGray;
           Pen.Color := clGray;
          end;
         Rectangle(R);
         Exit;
        end;
       csCorel:
        begin
         if (FState <> ccsNone) then
          InflateRect(R, -2, -2)
         else
          begin
           Inc(R.Left);
           Dec(R.Bottom);
           if R.Top <= 1 then
            Inc(R.Top);
           if R.Right = Width then
            Dec(R.Right);
          end;
         c := mbStringToColor(clr);
         if Enabled then
          Brush.Color := c
         else
          Brush.Color := clGray;
         FillRect(R);
         Exit;
        end;
      end;

     //if transparent draw the glyph
     if SameText(clr, 'clTransparent') then PaintTransparentGlyph(FTempBmp.Canvas, R);
    end;
  end;
end;

procedure TmbColorPalette.DrawCellBack(ACanvas: TCanvas; R: TRect; AIndex: integer);
begin
 case FCellStyle of
  csDefault:
   begin
    {$IFDEF DELPHI_7_UP}
    if {$IFDEF DELPHI_XE2_UP}StyleServices.Enabled{$ELSE}ThemeServices.ThemesEnabled{$ENDIF} then
     begin
       with {$IFDEF DELPHI_XE2_UP}StyleServices{$ELSE}ThemeServices{$ENDIF} do
        if Enabled then
         case FState of
          ccsNone: ACanvas.CopyRect(R, PBack.Canvas, R);
          ccsOver: DrawElement(ACanvas.Handle, GetElementDetails(ttbButtonHot), R);
          ccsDown: DrawElement(ACanvas.Handle, GetElementDetails(ttbButtonPressed), R);
          ccsChecked: DrawElement(ACanvas.Handle, GetElementDetails(ttbButtonChecked), R);
          ccsCheckedHover: DrawElement(ACanvas.Handle, GetElementDetails(ttbButtonCheckedHot), R);
         end
        else
         DrawElement(ACanvas.Handle, GetElementDetails(ttbButtonDisabled), R);
     end
    else
    {$ENDIF}
     if Enabled then
      case FState of
       ccsNone: ACanvas.FillRect(R);
       ccsOver: DrawEdge(ACanvas.Handle, R, BDR_RAISEDINNER, BF_RECT);
       ccsDown, ccsChecked, ccsCheckedHover: DrawEdge(ACanvas.Handle, R, BDR_SUNKENOUTER, BF_RECT);
      end
     else
       DrawFrameControl(ACanvas.Handle, R, DFC_BUTTON, 0 or DFCS_BUTTONPUSH or DFCS_FLAT or DFCS_INACTIVE);
   end;
  csCorel:
   begin
    if Enabled then
     begin
      {$IFDEF DELPHI_7_UP}
      if {$IFDEF DELPHI_XE2_UP}StyleServices.Enabled{$ELSE}ThemeServices.ThemesEnabled{$ENDIF} then
       case FState of
        ccsNone:
         begin
          ACanvas.Brush.Color := clWhite;
          ACanvas.Pen.Color := clBlack;
          //left
          ACanvas.MoveTo(R.Left, R.Top);
          ACanvas.LineTo(R.Left, R.Bottom-1);
          //bottom
          ACanvas.MoveTo(R.Left, R.Bottom-1);
          ACanvas.LineTo(R.Right, R.Bottom-1);
          //top
          if R.Top = 0 then
           begin
            ACanvas.MoveTo(R.Left, R.Top);
            ACanvas.LineTo(R.Right, R.Top);
           end;
          //right
          if (R.Right = Width) then
           begin
            ACanvas.MoveTo(R.Right-1, R.Top);
            ACanvas.LineTo(R.Right-1, R.Bottom-1);
           end
          else
           if (AIndex = FTotalCells) then
            begin
             ACanvas.MoveTo(R.Right, R.Top);
             ACanvas.LineTo(R.Right, R.Bottom);
            end;
         end;
        ccsOver: {$IFDEF DELPHI_XE2_UP}StyleServices{$ELSE}ThemeServices{$ENDIF}.DrawElement(ACanvas.Handle, {$IFDEF DELPHI_XE2_UP}StyleServices{$ELSE}ThemeServices{$ENDIF}.GetElementDetails(ttbButtonHot), R);
        ccsDown: {$IFDEF DELPHI_XE2_UP}StyleServices{$ELSE}ThemeServices{$ENDIF}.DrawElement(ACanvas.Handle, {$IFDEF DELPHI_XE2_UP}StyleServices{$ELSE}ThemeServices{$ENDIF}.GetElementDetails(ttbButtonPressed), R);
        ccsChecked: {$IFDEF DELPHI_XE2_UP}StyleServices{$ELSE}ThemeServices{$ENDIF}.DrawElement(ACanvas.Handle, {$IFDEF DELPHI_XE2_UP}StyleServices{$ELSE}ThemeServices{$ENDIF}.GetElementDetails(ttbButtonChecked), R);
        ccsCheckedHover: {$IFDEF DELPHI_XE2_UP}StyleServices{$ELSE}ThemeServices{$ENDIF}.DrawElement(ACanvas.Handle, {$IFDEF DELPHI_XE2_UP}StyleServices{$ELSE}ThemeServices{$ENDIF}.GetElementDetails(ttbButtonCheckedHot), R);
       end
      else
      {$ENDIF}
       case FState of
        ccsNone:
         begin
          ACanvas.Brush.Color := clWhite;
          ACanvas.Pen.Color := clBlack;
          ACanvas.Brush.Color := clWhite;
          ACanvas.Pen.Color := clBlack;
          //left
          ACanvas.MoveTo(R.Left, R.Top);
          ACanvas.LineTo(R.Left, R.Bottom-1);
          //bottom
          ACanvas.MoveTo(R.Left, R.Bottom-1);
          ACanvas.LineTo(R.Right, R.Bottom-1);
          //top
          if R.Top = 0 then
           begin
            ACanvas.MoveTo(R.Left, R.Top);
            ACanvas.LineTo(R.Right, R.Top);
           end;
          //right
          if (R.Right = Width) then
           begin
            ACanvas.MoveTo(R.Right-1, R.Top);
            ACanvas.LineTo(R.Right-1, R.Bottom-1);
           end
          else
           if (AIndex = FTotalCells) then
            begin
             ACanvas.MoveTo(R.Right, R.Top);
             ACanvas.LineTo(R.Right, R.Bottom);
            end;
         end;
        ccsOver:
         begin
          OffsetRect(R, 1,1);
          DrawEdge(ACanvas.Handle, R, BDR_RAISED, BF_RECT);
         end;
        ccsDown, ccsChecked, ccsCheckedHover: DrawEdge(ACanvas.Handle, R, BDR_SUNKENOUTER, BF_RECT);
       end;
     end
    else
     {$IFDEF DELPHI_7_UP}
     if {$IFDEF DELPHI_XE2_UP}StyleServices.Enabled{$ELSE}ThemeServices.ThemesEnabled{$ENDIF} then
      {$IFDEF DELPHI_XE2_UP}StyleServices{$ELSE}ThemeServices{$ENDIF}.DrawElement(ACanvas.Handle, {$IFDEF DELPHI_XE2_UP}StyleServices{$ELSE}ThemeServices{$ENDIF}.GetElementDetails(ttbButtonDisabled), R)
     else
     {$ENDIF}
      begin
       ACanvas.Brush.Color := Color;
       ACanvas.FillRect(R);
      end;
   end;
 end;
end;

procedure TmbColorPalette.PaintTransparentGlyph(ACanvas: TCanvas; R: TRect);
begin
 InflateRect(R, -3, -3);
 if FCellStyle = csCorel then
  begin
   if FState <> ccsNone then
    InflateRect(R, -2, -2)
   else
    if FColCount > 1 then
     Inc(R.Right);
  end;
 with ACanvas do
  case FTStyle of
   tsPhotoshop:
    begin
     if Enabled then
      Pen.Color := clBtnShadow
     else
      Pen.Color := clGray;
     Brush.Color := clWhite;
     Rectangle(R);
     Brush.Color := clSilver;
     FillRect(Rect(R.Left + (R.Right - R.Left) div 2, R.Top + 1, R.Right - 1, R.Top + (R.Bottom - R.Top) div 2));
     FillRect(Rect(R.Left + 1, R.Top + (R.Bottom - R.Top) div 2, R.Left + (R.Right - R.Left) div 2, R.Bottom - 1));
    end;
   tsPhotoshop2:
    begin
     InflateRect(R, -1, -1);
     Brush.Color := clWhite;
     Rectangle(R);
     Pen.Color := clRed;
     Pen.Width := 2;
     InflateRect(R, 1, 1);
     MoveTo(R.Left, R.Top);
     LineTo(R.Right - 1, R.Bottom - 1);
     Pen.Width := 1;
     Pen.Color := clBlack;
    end;
   tsCorel:
    begin
     if FCellStyle = csCorel then
      begin
       Pen.Color := clBlack;
       InflateRect(R, 3, 3);
       Brush.Color := clWhite;
       Rectangle(R);
       //the \ line
       MoveTo(R.Left, R.Top);
       LineTo(R.Right, R.Bottom);
       //the / line
       MoveTo(R.Right-1, R.Top);
       LineTo(R.Left-1, R.Bottom);
      end
     else
      begin
       if Enabled then
        Pen.Color := clBtnShadow
       else
        Pen.Color := clGray;
       Brush.Color := clWhite;
       Rectangle(R);
       MoveTo(R.Left, R.Top);
       LineTo(R.Right, R.Bottom);
       MoveTo(R.Right - 1, R.Top);
       LineTo(R.Left - 1, R.Bottom);
      end;
    end;
   tsMicroangelo:
    begin
     InflateRect(R, -1, -1);
     Dec(R.Bottom);
     Pen.Color := clBlack;
     Brush.Color := clTeal;
     Rectangle(R);
     Pixels[R.Left + 2, R.Top + 2] := clWhite;
     Pixels[R.Left + (R.Right - R.Left) div 2, R.Bottom] := clBlack;
     MoveTo(R.Left + (R.Right - R.Left) div 2 - 2, R.Bottom + 1);
     LineTo(R.Left + (R.Right - R.Left) div 2 + 3, R.Bottom + 1);
    end;
  end;
end;

procedure TmbColorPalette.Resize;
begin
 inherited;
 CalcAutoHeight;
 Invalidate;
end;

procedure TmbColorPalette.CMMouseEnter(var Message: TMessage);
begin
 FMouseOver := true;
 FMouseLoc := mlOver;
 Invalidate;
 inherited;
end;

procedure TmbColorPalette.CMMouseLeave(var Message: TMessage);
begin
 FMouseOver := false;
 FMouseLoc := mlNone;
 FIndex := -1;
 Invalidate;
 inherited;
end;

procedure TmbColorPalette.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
 if FIndex <> (y div FCellSize)* FColCount + (x div FCellSize) then
  begin
   FIndex := (y div FCellSize)* FColCount + (x div FCellSize);
   if FIndex > FTotalCells then FIndex := -1;
   Invalidate;
  end;
 inherited;
end;

procedure TmbColorPalette.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
if Button = mbLeft then
 begin
  SetFocus;
  FMouseDown := true;
  FMouseLoc := mlDown;
  if (y div FCellSize)* FColCount + (x div FCellSize) <= FTotalCells then
   if FCheckedIndex <> (y div FCellSize)* FColCount + (x div FCellSize) then
    begin
     FOldIndex := FCheckedIndex;
     FCheckedIndex := (y div FCellSize)* FColCount + (x div FCellSize);
    end;
  Invalidate;
 end;
 inherited;
end;

procedure TmbColorPalette.Click;
begin
 inherited;
end;

procedure TmbColorPalette.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
 DontCheck: boolean;
 AColor: TColor;
begin
 FMouseDown := false;
 if FMouseOver then
  FMouseLoc := mlOver
 else
  FMouseLoc := mlNone;
 DontCheck := false;
 if (FCheckedIndex > -1) and (FCheckedIndex < FColors.Count) then
  AColor := mbStringToColor(FColors.Strings[FCheckedIndex])
 else
  AColor := clNone;
 if (Button = mbLeft) and PtInRect(ClientRect, Point(x, y)) then
  if Assigned(FOnCellClick) then
   FOnCellClick(Button, Shift, FCheckedIndex, AColor, DontCheck);
 if DontCheck then FCheckedIndex := FOldIndex;
 Invalidate;
 inherited;
 if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TmbColorPalette.CMGotFocus(var Message: TCMGotFocus);
begin
 inherited;
 Invalidate;
end;

procedure TmbColorPalette.CMLostFocus(var Message: TCMLostFocus);
begin
 inherited;
 if FMouseOver then
  FMouseLoc := mlOver
 else
  FMouseLoc := mlNone;
 Invalidate;
end;

procedure TmbColorPalette.CMEnabledChanged(var Message: TMessage);
begin
 inherited;
 Invalidate;
end;

procedure TmbColorPalette.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
 Message.Result := 1;
end;

procedure TmbColorPalette.SelectCell(i: integer);
begin
 if i < FColors.Count - 1 then
  FCheckedIndex := i
 else
  FCheckedIndex := -1;
 Invalidate;
 if Assigned(FOnChange) then FOnChange(Self);
end;

function TmbColorPalette.GetSelColor: TColor;
begin
if (FCheckedIndex > -1) and (FCheckedIndex <= FTotalCells) then
 Result := mbStringToColor(FColors.Strings[FCheckedIndex])
else
 Result := FOld;
end;

function TmbColorPalette.GetColorUnderCursor: TColor;
begin
 Result := clNone;
 if FIndex > -1 then
  if FIndex < FColors.Count then
   Result := mbStringToColor(FColors.Strings[FIndex]);
end;

function TmbColorPalette.GetIndexUnderCursor: integer;
begin
 Result := -1;
 if FIndex > -1 then
  if FIndex < FColors.Count then
   Result := FIndex;
end;

procedure TmbColorPalette.SetTStyle(s: TTransparentStyle);
begin
 if FTStyle <> s then
  begin
   FTStyle := s;
   Invalidate;
  end;
end;

procedure TmbColorPalette.SetCellStyle(s: TCellStyle);
begin
 if FCellStyle <> s then
  begin
   FCellStyle := s;
   Invalidate;
  end;
end;

procedure TmbColorPalette.SetSelColor(k: TColor);
var
 s: string;
 i: integer;
begin
 s := mbColorToString(k);
 for i:= 0 to FColors.Count - 1 do
  if SameText(s, FColors.Strings[i]) then
   begin
    FCheckedIndex := i;
    Break;
   end
  else
   FCheckedIndex := -1;
 Invalidate;
 FOld := k;
 if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TmbColorPalette.SetStrings(s: TStrings);
var
 i: integer;
begin
 FColors.Clear;
 FColors.AddStrings(s);
 if FColors.Count < FMinColors then
  for i := 0 to FMinColors - FColors.Count - 1 do
   FColors.Add('clNone');
 if (FColors.Count > FMaxColors) and (FMaxColors > 0) then
  for i := FColors.Count - 1 downto FMaxColors do
   FColors.Delete(i);
 CalcAutoHeight;
 SortColors;
 Invalidate;
end;

procedure TmbColorPalette.SetNames(n: TStrings);
var
 i: integer;
begin
 FNames.Clear;
 FNames.AddStrings(n);
 if (FNames.Count > FMaxColors) and (FMaxColors > 0) then
  for i := FNames.Count - 1 downto FMaxColors do
   FNames.Delete(i);
end;

function TmbColorPalette.GetMoveCellIndex(move: TMoveDirection): integer;
var
 FBefore: integer;
begin
 Result := -1;
 case move of
  mdLeft:
   if FCheckedIndex -1 < 0 then
    Result := FTotalCells
   else
    Result := FCheckedIndex - 1;
  mdRight:
   if FCheckedIndex + 1 > FTotalCells then
    Result := 0
   else
    Result := FCheckedIndex + 1;
  mdUp:
   if FCheckedIndex - FColCount < 0 then
    begin
     FBefore := (FTotalcells div FColCount) * FColCount;
     if FBefore + FCheckedIndex - 1 > FTotalCells then Dec(FBefore, FColCount);
     Result := FBefore + FCheckedIndex - 1;
    end
   else
    Result := FCheckedIndex - FColCount;
  mdDown:
   if FCheckedIndex + FColCount > FTotalCells then
    Result := FCheckedIndex mod FColCount + 1
   else
    Result := FCheckedIndex + FColCount;
 end;
 if Result > FColors.Count - 1 then
  Result := 0;
end;

procedure TmbColorPalette.CNKeyDown(var Message: TWMKeyDown);
var
 FInherited: boolean;
 Shift: TShiftState;
begin
 Shift := KeyDataToShiftState(Message.KeyData);
 Finherited := false;
 case Message.CharCode of
  VK_LEFT:
   begin
    FCheckedIndex := GetMoveCellIndex(mdLeft);
    if Assigned(FOnArrowKey) then FOnArrowKey(Message.CharCode, Shift);
   end;
  VK_RIGHT:
   begin
    FCheckedIndex := GetMoveCellIndex(mdRight);
    if Assigned(FOnArrowKey) then FOnArrowKey(Message.CharCode, Shift);
   end;
  VK_UP:
   begin
    FCheckedIndex := GetMoveCellIndex(mdUp);
    if Assigned(FOnArrowKey) then FOnArrowKey(Message.CharCode, Shift);
   end;
  VK_DOWN:
   begin
    FCheckedIndex := GetMoveCellIndex(mdDown);
    if Assigned(FOnArrowKey) then FOnArrowKey(Message.CharCode, Shift);
   end;
  VK_SPACE, VK_RETURN: if Assigned(FOnChange) then FOnChange(Self);
 else
  begin
   FInherited := true;
   inherited;
  end;
 end;
 if not FInherited then
  begin
   Invalidate;
   if Assigned(OnKeyDown) then OnKeyDown(Self, Message.CharCode, Shift);
   if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TmbColorPalette.CMHintShow(var Message: TMessage);
var
 clr: TColor;
 Handled: boolean;
begin
if (Colors.Count > 0) and (FIndex > -1) then
 with TCMHintShow(Message) do
  begin
   if not ShowHint then
    Message.Result := 1
   else
    begin
     with HintInfo^ do
      begin
       // show that we want a hint
       Result := 0;
       ReshowTimeout := 1;
       HideTimeout := 5000;
       clr := GetColorUnderCursor;
       //fire event
       Handled := false;
       if Assigned(FOnGetHintText) then FOnGetHintText(clr, GetIndexUnderCursor, HintStr, Handled);
       if Handled then Exit;
       //do default
       if FIndex < FNames.Count then
        HintStr := FNames.Strings[FIndex]
       else
        if SameText(FColors.Strings[GetIndexUnderCursor], 'clCustom') or SameText(FColors.Strings[GetIndexUnderCursor], 'clTransparent') then
         HintStr := StringReplace(FColors.Strings[GetIndexUnderCursor], 'cl', '', [rfReplaceAll])
        else
         HintStr := FormatHint(FHintFormat, GetColorUnderCursor);
      end;
    end;
  end;
end;

procedure TmbColorPalette.SetAutoHeight(auto: boolean);
begin
 FAutoHeight := auto;
 CalcAutoHeight;
 Invalidate;
end;

procedure TmbColorPalette.SetMinColors(m: integer);
var
 i: integer;
begin
 if (FMaxColors > 0) and (m > FMaxColors) then
  m := FMaxColors;
 FMinColors := m;
 if FColors.Count < m then
  for i := 0 to m - FColors.Count - 1 do
   FColors.Add('clNone');
 CalcAutoHeight;
 SortColors;
 Invalidate;
end;

procedure TmbColorPalette.SetMaxColors(m: integer);
var
 i: integer;
begin
 if m < 0 then m := 0;
 FMaxColors := m;
 if (m < FMinColors) and (m > 0) then
  SetMinColors(m);
 if (FColors.Count > FMaxColors) and (FMaxColors > 0) then
  for i := FColors.Count - 1 downto FMaxColors do
   FColors.Delete(i);
 CalcAutoHeight;
 SortColors;
 Invalidate;
end;

procedure TmbColorPalette.SetSortMode(s: TSortMode);
begin
 if FSort <> s then
  begin
   FSort := s;
   SortColors;
   Invalidate;
  end;
end;

procedure TmbColorPalette.SetSortOrder(s: TSortOrder);
begin
 if FOrder <> s then
  begin
   FOrder := s;
   SortColors;
   Invalidate;
  end;
end;

procedure TmbColorPalette.ColorsChange(Sender: TObject);
begin
 if Assigned(FOnColorsChange) then FOnColorsChange(Self);
 FTotalCells := FColors.Count - 1;
 CalcAutoHeight;
 Invalidate;
end;

procedure TmbColorPalette.SetCellSize(s: integer);
begin
 FCellSize := s;
 CalcAutoHeight;
 Invalidate;
end;

function TmbColorPalette.GetSelectedCellRect: TRect;
var
 row, fbottom, fleft: integer;
begin
 if FCheckedIndex > -1 then
  begin
   if FCheckedIndex mod FColCount = 0 then
    begin
     row := FCheckedIndex div FColCount;
     fleft := Width - FCellSize;
    end
   else
    begin
     row := FCheckedIndex div FColCount + 1;
     fleft := (FCheckedIndex mod FColCount - 1) * FCellSize;
    end;
   fbottom := row * FCellSize;
   Result := Rect(fleft, fbottom - FCellSize, fleft + FCellSize, fbottom);
  end
 else
  Result := Rect(0, 0, 0, 0);
end;

procedure TmbColorPalette.GeneratePalette(BaseColor: TColor);
begin
 FColors.Text := MakePalette(BaseColor, FOrder);
 CalcAutoHeight;
 SortColors;
 Invalidate;
 if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TmbColorPalette.GenerateGradientPalette(Colors: array of TColor);
begin
 FColors.Text := MakeGradientPalette(Colors);
 CalcAutoHeight;
 SortColors;
 Invalidate;
 if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TmbColorPalette.LoadPalette(FileName: TFileName);
var
 supported: boolean;
 a: AcoColors;
 i: integer;
begin
 supported := false;
 if SameText(ExtractFileExt(FileName), '.pal') then
  begin
   supported := true;
   FNames.Clear;
   FColors.Text := ReadJASCPal(FileName);
  end
 else
  if SameText(ExtractFileExt(FileName), '.aco') then
   begin
    supported := true;
    a := ReadPhotoshopAco(FileName);
    FColors.Clear;
    for i := 0 to Length(a.Colors) - 1 do
     FColors.Add(ColorToString(a.Colors[i]));
    FNames.Clear;
    if a.HasNames then
     for i := 0 to Length(a.Names) - 1 do
      FNames.Add(a.Names[i]);
   end
  else
   if SameText(ExtractFileExt(FileName), '.act') then
    begin
     supported := true;
     FNames.Clear;
     FColors.Text := ReadPhotoshopAct(FileName);
    end
   else
    Exception.Create('The file format you are trying to load is not supported in this version of the palette'#13'Please send a request to MXS along with the files of this format so'#13'loading support for this file can be added too');
 if supported then
  begin
   CalcAutoHeight;
   SortColors;
   Invalidate;
   if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TmbColorPalette.SaveColorsAsPalette(FileName: TFileName);
begin
 if SameText(ExtractFileExt(FileName), '.pal') then
  SaveJASCPal(FColors, FileName)
 else
  raise Exception.Create('The file extension specified does not identify a supported file format!'#13'Supported files formats are: .pal .aco .act');
end;

procedure TmbColorPalette.SortColors;
var
 old: TColor;
begin
 if FSort <> smNone then
  begin
   if FColors.Count = 0 then Exit;
   old := GetSelColor;
   SortPalColors(FColors, FSort, FOrder);
   SetSelColor(old);
   Invalidate;
  end;
end;

end.
