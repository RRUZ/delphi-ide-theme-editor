unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, mbColorPalette, TBXDkPanels, TB2Dock, TB2ToolWindow, TBX,
  Buttons, ExtCtrls, TBXSwitcher, ImgList, StdCtrls, TBXLists, PalUtils, 
  mbTBXPageScroller, TB2Item, TB2Toolbar, TB2ExtItems, TBXExtItems,
  mbXPScrollButton, Menus, XPMan;

const
 crColored = 5;
 crColoredL = 6;

type
  TForm1 = class(TForm)
    TBXDock1: TTBXDock;
    TBXToolWindow1: TTBXToolWindow;
    mbColorPalette1: TmbColorPalette;
    TBXAlignmentPanel1: TTBXAlignmentPanel;
    Panel1: TPanel;
    SpeedButton1: TSpeedButton;
    Panel2: TPanel;
    SpeedButton2: TSpeedButton;
    TBXSwitcher1: TTBXSwitcher;
    ScrollBox1: TScrollBox;
    ImageList1: TImageList;
    Panel3: TPanel;
    SpeedButton3: TSpeedButton;
    TBXAlignmentPanel2: TTBXAlignmentPanel;
    TBXDock2: TTBXDock;
    TBXDock3: TTBXDock;
    TBXDock4: TTBXDock;
    Shape1: TShape;
    Label2: TLabel;
    TBXToolbar1: TTBXToolbar;
    lstFonts: TTBXComboBoxItem;
    TBXPageScroller1: TPanel;
    TBXPageScroller2: TPanel;
    ScrlUp: TmbXPScrollButton;
    ScrlDown: TmbXPScrollButton;
    PalVHolder: TPanel;
    ScrlLeft: TmbXPScrollButton;
    ScrlRight: TmbXPScrollButton;
    PalHHolder: TPanel;
    ConfigMenu: TPopupMenu;
    SetOutlineColor1: TMenuItem;
    SetFillColor1: TMenuItem;
    N1: TMenuItem;
    Palette1: TMenuItem;
    N2: TMenuItem;
    Edit1: TMenuItem;
    N3: TMenuItem;
    SetAsDefault1: TMenuItem;
    ShowColorNames1: TMenuItem;
    ScrollToStart1: TMenuItem;
    ScrollToEnd1: TMenuItem;
    N4: TMenuItem;
    Customize1: TMenuItem;
    ChangeColor1: TMenuItem;
    PaletteEditor1: TMenuItem;
    FindColor1: TMenuItem;
    New1: TMenuItem;
    Open1: TMenuItem;
    Save1: TMenuItem;
    SaveAs1: TMenuItem;
    Close1: TMenuItem;
    XPManifest1: TXPManifest;
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure mbColorPalette1CellClick(Button: TMouseButton;
      Shift: TShiftState; Index: Integer; AColor: TColor;
      var DontCheck: Boolean);
    procedure mbColorPalette1PaintCell(ACanvas: TCanvas; ACellRect: TRect;
      AColor: TColor; Index: Integer; AState: TColorCellState;
      var AStyle: TTransparentStyle; var PaintingHandled: Boolean);
    procedure TBXToolWindow1Resize(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure TBXToolWindow1DockChanged(Sender: TObject);
    procedure mbColorPalette1GetHintText(AColor: TColor; Index: Integer;
      var HintStr: String; var Handled: Boolean);
    procedure mbColorPalette1ColorsChange(Sender: TObject);
    procedure mbColorPalette1Resize(Sender: TObject);
    procedure mbColorPalette1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Shape1DragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure mbColorPalette1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Shape1DragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure mbColorPalette1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure lstFontsDrawItem(Sender: TTBXCustomList;
      ACanvas: TCanvas; ARect: TRect; AIndex, AHoverIndex: Integer;
      var DrawDefault: Boolean);
    procedure lstFontsMeasureHeight(Sender: TTBXCustomList;
      ACanvas: TCanvas; var AHeight: Integer);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lstFontsAcceptText(Sender: TObject; var NewText: String;
      var Accept: Boolean);
    procedure lstFontsChange(Sender: TObject; const Text: String);
    procedure ScrlUpScroll(Sender: TObject);
    procedure ScrlDownScroll(Sender: TObject);
    procedure PalVHolderResize(Sender: TObject);
    procedure PalVHolderCanResize(Sender: TObject; var NewWidth,
      NewHeight: Integer; var Resize: Boolean);
    procedure ScrlLeftScroll(Sender: TObject);
    procedure ScrlRightScroll(Sender: TObject);
    procedure PalHHolderCanResize(Sender: TObject; var NewWidth,
      NewHeight: Integer; var Resize: Boolean);
    procedure PalHHolderResize(Sender: TObject);
    procedure SetOutlineColor1Click(Sender: TObject);
    procedure SetFillColor1Click(Sender: TObject);
  private
    procedure TransferBMP(index: integer; bTo: TBitmap);
    procedure CreateFillCursor;
    procedure CreateLineCursor;
  end;

var
  Form1: TForm1;
  Expanded: boolean = false;
  LineCl: TColor = clBlack;
  FillCl: TColor = clWhite;
  MeshCl: TColor = $00FF8000; // light blue
  DragCl: TColor = clWhite;
  Cancel: boolean = false;
  dIndex: integer;
  mdx, mdy: integer;
  //------------------for fonts---------
  allFonts, mruFonts: TStringList;
  maxMRUFonts: integer = 5;

implementation

uses Types;

{$R *.dfm}

procedure TForm1.SpeedButton1Click(Sender: TObject);
var
 p: TPoint;
begin
 GetCursorPos(p);
 ConfigMenu.Popup(p.x, p.y);
end;

procedure TForm1.SpeedButton2Click(Sender: TObject);
begin
 TBXToolWindow1.Width := 5*mbColorPalette1.CellSize + GetSystemMetrics(SM_CXVSCROLL) + 5;
end;

procedure TForm1.mbColorPalette1CellClick(Button: TMouseButton;
  Shift: TShiftState; Index: Integer; AColor: TColor;
  var DontCheck: Boolean);
begin
 if Expanded then
  begin
   if Index = 0 then
    begin
     DontCheck := true;
     SpeedButton1.Click;
     Cancel := false;
    end;
   case Index of
    1, 3: DontCheck := true;
   else
    begin
     if not Cancel then
      begin
       if ssShift in Shift then
        LineCl := AColor
       else
        if ssCtrl in Shift then
         MeshCl := AColor
        else
         FillCl := AColor;
      end
     else
      begin
       DragCl := AColor;
       DontCheck := true;
      end;
    end;
   end;
  end
 else
  case Index of
   0, 2: DontCheck := true;
  else
   begin
    if not Cancel then
      begin
       if ssShift in Shift then
        LineCl := AColor
       else
        if ssCtrl in Shift then
         MeshCl := AColor
        else
         FillCl := AColor;
      end
     else
      begin
       DragCl := AColor;
       DontCheck := true;
      end;
   end;
  end;
end;

procedure TForm1.mbColorPalette1PaintCell(ACanvas: TCanvas;
  ACellRect: TRect; AColor: TColor; Index: Integer;
  AState: TColorCellState; var AStyle: TTransparentStyle;
  var PaintingHandled: Boolean);
begin
 if Expanded then
  begin
   if Index = 0 then
    ImageList1.Draw(ACanvas, ACellRect.Left, ACellRect.Top, 0);
   case Index of
    1:
     begin
      InflateRect(ACellRect, -2, -2);
      Inc(ACellRect.Left);
      Inc(AcellRect.Top);
      with ACanvas do
       begin
        Pen.Color := LineCl;
        Pen.Width := 2;
        //what if transparent
        if LineCl <> clTransparent then
         Pen.Style := psSolid
        else
         Pen.Style := psClear;
        if FillCl <> clTransparent then
         Brush.Color := FillCl
        else
         Brush.Color := clWhite;
        //restore
        Rectangle(ACellRect);
        Pen.Width := 1;
        Pen.Style := psSolid;
       end;
      PaintingHandled := true;
     end;
    3:
     begin
      InflateRect(ACellRect, -2, -2);
      with ACanvas do
       begin
        if MeshCl <> clTransparent then
         Brush.Color := MeshCl
        else
         Brush.Color := clWhite;
        FillRect(ACellRect);
        Brush.Color := clWhite;
        Brush.Style := bsDiagCross;
        Rectangle(ACellRect);
        Brush.Style := bsSolid;
       end;
      PaintingHandled := true;
     end;
   end;
  end
 else
  case Index of
   0:
    begin
     InflateRect(ACellRect, -2, -2);
     Inc(ACellRect.Left);
     Inc(AcellRect.Top);
      with ACanvas do
       begin
        Pen.Color := LineCl;
        Pen.Width := 2;
        //what if transparent
        if LineCl <> clTransparent then
         Pen.Style := psSolid
        else
         Pen.Style := psClear;
        if FillCl <> clTransparent then
         Brush.Color := FillCl
        else
         Brush.Color := clWhite;
        //restore
        Rectangle(ACellRect);
        Pen.Width := 1;
        Pen.Style := psSolid;
       end;
      PaintingHandled := true;
    end;
   2:
    begin
     InflateRect(ACellRect, -2, -2);
      with ACanvas do
       begin
        if MeshCl <> clTransparent then
         Brush.Color := MeshCl
        else
         Brush.Color := clWhite;
        FillRect(ACellRect);
        Brush.Color := clWhite;
        Brush.Style := bsDiagCross;
        Rectangle(ACellRect);
        Brush.Style := bsSolid;
       end;
      PaintingHandled := true;
    end;
  end;
end;

procedure TForm1.TBXToolWindow1Resize(Sender: TObject);
begin
 //disallow smaller than 23x23
 if TBXToolWindow1.ClientAreaWidth < 23 then TBXToolWindow1.ClientAreaWidth := 23;
 if TBXToolWindow1.ClientAreaHeight < 23 then TBXToolWindow1.ClientAreaHeight := 23;
 //wrapping around
 if TBXToolWindow1.Floating and (TBXToolWindow1.ClientAreaWidth <> 23) and (TBXToolWindow1.ClientAreaHeight <> 23) then
  begin
   if TBXToolWindow1.ClientAreaHeight mod mbColorPalette1.CellSize > 0 then
    TBXToolWindow1.ClientAreaHeight := ((TBXToolWindow1.ClientAreaHeight div mbColorPalette1.CellSize) + 1) * mbColorPalette1.CellSize;
   if ScrollBox1.Visible and ScrollBox1.VertScrollBar.Visible then
    begin
     if (TBXToolWindow1.ClientAreaWidth - GetSystemMetrics(SM_CXVSCROLL) - 2) mod mbColorPalette1.CellSize > 0 then
      TBXToolWindow1.ClientAreaWidth := (((TBXToolWindow1.ClientAreaWidth - 2 - GetSystemMetrics(SM_CXVSCROLL))div mbColorPalette1.CellSize) + 1) * mbColorPalette1.CellSize + GetSystemMetrics(SM_CXVSCROLL) + 2;
    end
   else
    if TBXToolWindow1.ClientAreaWidth mod mbColorPalette1.CellSize > 0 then
     TBXToolWindow1.ClientAreaWidth := ((TBXToolWindow1.ClientAreaWidth div mbColorPalette1.CellSize) + 1) * mbColorPalette1.CellSize;
  end;
 //parent changing on resize
 if (mbColorPalette1.ColCount < 1) or (TBXToolWindow1.ClientAreaWidth < 23 + mbColorPalette1.CellSize) then
  begin
   if Expanded then
    mbColorPalette1.Align := alTop
   else
    mbColorPalette1.Anchors := [akLeft, akBottom, akRight];
   Expanded := false;
   SpeedButton1.Top := 0;
   Panel1.Align := alTop;
   TBXToolWindow1.ClientAreaWidth := 23;
   mbColorPalette1.Parent := PalVHolder;
   TBXAlignmentPanel2.Visible := false;
   TBXAlignmentPanel1.Visible := true;
   ScrollBox1.Visible := false;
   Panel1.Visible := true;
   Panel2.Visible := true;
   Panel3.Visible := false;
   if mbColorPalette1.Colors.Count > 1 then
    if SameText(mbColorPalette1.Colors.Strings[0], 'clCustom') and SameText(mbColorPalette1.Colors.Strings[1], 'clCustom') then
     mbColorPalette1.Colors.Delete(0);
  end
 else 
  begin
   Expanded := true;
   SpeedButton1.Top := 0;
   mbColorPalette1.Parent := ScrollBox1;
   mbColorPalette1.Align := alTop;
   TBXAlignmentPanel1.Visible := false;
   if TBXToolWindow1.ClientAreaHeight >= 25 then
    ScrollBox1.Visible := true;
   TBXAlignmentPanel2.Visible := false;
   Panel1.Visible := false;
   Panel2.Visible := false;
   Panel3.Visible := false;
   if mbColorPalette1.Colors.Count > 1 then
    if not (SameText(mbColorPalette1.Colors.Strings[0], 'clCustom') and SameText(mbColorPalette1.Colors.Strings[1], 'clCustom')) then
     mbColorPalette1.Colors.Insert(0, 'clCustom');
  end;
 if TBXToolWindow1.ClientAreaHeight < 25 then
  begin
   if Expanded then
    mbColorPalette1.Align := alLeft
   else
    mbColorPalette1.Anchors := [akTop, akRight, akBottom];
   Expanded := false;
   SpeedButton1.Top := 3;
   TBXToolWindow1.ClientAreaHeight := 23;
   Panel1.Align := alLeft;
   TBXPageScroller2.Height := mbColorPalette1.CellSize;
   mbColorPalette1.Width := mbColorPalette1.CellSize * mbColorPalette1.Colors.Count;
   mbColorPalette1.Parent := PalHHolder;
   Panel1.Visible := true;
   Panel2.Visible := false;
   Panel3.Visible := true;
   ScrollBox1.Visible := false;
   TBXAlignmentPanel2.Visible := true;
   if mbColorPalette1.Colors.Count > 1 then
    if SameText(mbColorPalette1.Colors.Strings[0], 'clCustom') and SameText(mbColorPalette1.Colors.Strings[1], 'clCustom') then
     mbColorPalette1.Colors.Delete(0);
  end;
end;

procedure TForm1.SpeedButton3Click(Sender: TObject);
begin
 TBXToolWindow1.ClientAreaHeight := mbColorPalette1.CellSize * 5;
 if ScrollBox1.VertScrollBar.Visible then
  begin
    if TBXToolWindow1.ClientAreaWidth < 23 + GetSystemMetrics(SM_CXVSCROLL) then
     TBXToolWindow1.ClientAreaWidth := 23 + GetSystemMetrics(SM_CXVSCROLL);
  end
 else
  if TBXToolWindow1.ClientAreaWidth < 23 then
   TBXToolWindow1.ClientAreaWidth := 23;
end;

procedure TForm1.TBXToolWindow1DockChanged(Sender: TObject);
begin
 //expand when the window gets undocked and restore the glyphs
 if TBXToolWindow1.CurrentDock = nil then
  begin
   TBXToolWindow1.ClientAreaHeight := 5 * mbColorPalette1.CellSize;
   if ScrollBox1.Visible and ScrollBox1.VertScrollBar.Visible then
    TBXToolWindow1.ClientAreaWidth := 14 * mbColorPalette1.CellSize + GetSystemMetrics(SM_CXVSCROLL)
   else
    TBXToolWindow1.ClientAreaWidth := 15 * mbColorPalette1.CellSize;
   TransferBMP(1, SpeedButton3.Glyph);
   TransferBMP(4, SpeedButton2.Glyph);
   Exit;
  end;
 //contract when the window gets docked and assign glyphs
 if TBXToolWindow1.Docked then
  begin
   if (TBXToolWindow1.CurrentDock.Position = dpTop) or (TBXToolWindow1.CurrentDock.Position = dpBottom) then
    TBXToolWindow1.ClientAreaHeight := 23
   else
    if (TBXToolWindow1.CurrentDock.Position = dpLeft) or (TBXToolWindow1.CurrentDock.Position = dpRight) then
     TBXToolWindow1.ClientAreaWidth := 23;
   case TBXToolWindow1.CurrentDock.Position of
    dpTop: TransferBMP(1, SpeedButton3.Glyph);
    dpBottom: TransferBMP(3, SpeedButton3.Glyph);
    dpLeft: TransferBMP(4, SpeedButton2.Glyph);
    dpRight: TransferBMP(2, SpeedButton2.Glyph);
   end;
  end;
end;

procedure TForm1.TransferBMP(index: integer; bTo: TBitmap);
var
 b: TBitmap;
begin
 b := TBitmap.Create;
 try
  ImageList1.ImageType := itMask;
  if ImageList1.GetBitmap(index, b) then
   bTo.Assign(b);
  ImageList1.ImageType := itImage; 
 finally
  b.Free;
 end;
end;

procedure TForm1.mbColorPalette1GetHintText(AColor: TColor; Index: Integer;
  var HintStr: String; var Handled: Boolean);
var
 f, l, m: string;
begin
 if FillCl <> clTransparent then
  f := 'RGB(' + IntToStr(GetRValue(FillCl)) + ', ' + IntToStr(GetGValue(FillCl)) + ', ' + IntToStr(GetBValue(FillCl)) + ')'
 else
  f := 'none';
 if LineCl <> clTransparent then
  l := 'RGB(' + IntToStr(GetRValue(LineCl)) + ', ' + IntToStr(GetGValue(LineCl)) + ', ' + IntToStr(GetBValue(LineCl)) + ')'
 else
  l := 'none';
 if MeshCl <> clTransparent then
  m := 'RGB(' + IntToStr(GetRValue(MeshCl)) + ', ' + IntToStr(GetGValue(MeshCl)) + ', ' + IntToStr(GetBValue(MeshCl)) + ')'
 else
  m := 'none';

 if Expanded then
  case Index of
   0:
    begin
     HintStr := 'Configuration';
     Handled := true;
    end;
   1:
    begin
     HintStr := 'Line: ' + l + #10#13 + 'Fill: ' + f;
     Handled := true;
    end;
   3:
    begin
     HintStr := 'Mesh: ' + m;
     Handled := true;
    end;
  end
 else
  case Index of
   0:
    begin
     HintStr := 'Line: ' + l + #10#13 + 'Fill: ' + f;
     Handled := true;
    end;
   2:
    begin
     HintStr := 'Mesh: ' + m;
     Handled := true;
    end;
  end;
end;

procedure TForm1.mbColorPalette1ColorsChange(Sender: TObject);
begin
 TBXToolWindow1.MaxClientWidth := (mbColorPalette1.Colors.Count - 1) * mbColorPalette1.CellSize + 2;
end;

procedure TForm1.mbColorPalette1Resize(Sender: TObject);
begin
if ScrollBox1.Visible and ScrollBox1.VertScrollBar.Visible then
  TBXToolWindow1.MaxClientHeight := mbColorPalette1.RowCount * mbColorPalette1.CellSize
else
 TBXToolWindow1.MaxClientHeight := (mbColorPalette1.RowCount - 1) * mbColorPalette1.CellSize;
end;

procedure TForm1.mbColorPalette1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
 if ssShift in Shift then
  LineCl := mbColorPalette1.SelectedColor
 else
  if ssCtrl in Shift then
   MeshCl := mbColorPalette1.SelectedColor
  else
    FillCl := mbColorPalette1.SelectedColor;
 if Key in [VK_UP, VK_DOWN, VK_LEFT, VK_RIGHT] then
  begin
   //for keyboard navigation with up and down keys when the palette is docked vertically
   if not Expanded and (mbColorPalette1.Parent = PalVHolder) then
    begin
     if mbColorPalette1.SelectedCell < 5 then
      mbColorPalette1.Top := 0
     else
      if mbColorPalette1.SelectedCell > mbColorPalette1.Colors.Count - 6 then
       mbColorPalette1.Top := - (mbColorPalette1.Height - PalVHolder.Height)
      else
       begin
        if -mbColorPalette1.GetSelectedCellRect.Bottom > mbColorPalette1.Top then
         ScrlUp.Click;
        if mbColorPalette1.GetSelectedCellRect.Bottom + mbColorPalette1.CellSize + mbColorPalette1.Top > PalVHolder.Height then
         ScrlDown.Click;
       end;
    end;

   //for keyboard navigation with left and right keys when the palette is docked horizontally
   if not Expanded and (mbColorPalette1.Parent = PalHHolder) then
    begin
     if mbColorPalette1.SelectedCell < 5 then
      mbColorPalette1.Left := 0
     else
      if mbColorPalette1.SelectedCell > mbColorPalette1.Colors.Count - 6 then
       mbColorPalette1.Left := - (mbColorPalette1.Width - PalHHolder.Width)
      else
       begin
        if -mbColorPalette1.GetSelectedCellRect.Right > mbColorPalette1.Left then
         ScrlLeft.Click;
        if mbColorPalette1.GetSelectedCellRect.Right + mbColorPalette1.CellSize + mbColorPalette1.Left > PalHHolder.Width then
         ScrlRight.Click;
       end;
    end;
  end
 else
  if (mbColorPalette1.Parent = PalHHolder) or (mbColorPalette1.Parent = PalVHolder) then
   case Key of
    VK_END:
     begin
      mbColorPalette1.SelectedCell := mbColorPalette1.Colors.Count - 1;
      if mbColorPalette1.Parent = PalVHolder then
       mbColorPalette1.Top := - (mbColorPalette1.Height - PalVHolder.Height)
      else
       mbColorPalette1.Left := - (mbColorPalette1.Width - (ScrlRight.Left - ScrlLeft.Left + mbColorPalette1.CellSize));
     end;
    VK_HOME:
     begin
      mbColorPalette1.SelectedCell := 1;
      if mbColorPalette1.Parent = PalVHolder then
       mbColorPalette1.Top := 0
      else
       mbColorPalette1.Left := 0;
     end;
   end;
end;

procedure TForm1.CreateFillCursor;
var
 ii : TIconInfo;
 maskBM, colorBM : TBitmap;
begin
 maskBM := TBitmap.Create;
 with maskBM do
  begin
   Height := GetSystemMetrics(SM_CYCURSOR);
   Width := GetSystemMetrics(SM_CXCURSOR);
   with Canvas do
    begin
     Brush.Color := clWhite;
     FillRect(Rect(0, 0, Width, Height));
     Brush.Color := clBlack;
     Canvas.Brush.Color := clBlack;
     Canvas.Pen.Color := clBlack;
     Canvas.Polygon([Point(1,6), Point(1,22), Point(5, 18), Point(9,26), Point(10,26),
                    Point(11,25),  Point(8,18), Point(8,17), Point(12,17), Point(2,7), Point(1,7)]);
     Canvas.Polygon([Point(Width - 8, 1), Point(Width-8,11), Point(Width-1,11), Point(Width-1,4), Point(Width-4,4),
                     Point(Width-4,1), Point(Width-1,4), Point(Width-4, 1), Point(Width-8,1)]);
     Canvas.Pixels[Width-3, 3] := clBlack;
     if DragCl <> clTransparent then
      Canvas.FillRect(Rect(Width-18, 6, Width-5, 19))
     else
      begin
       Canvas.Brush.Style := bsClear;
       Canvas.Rectangle(Width-18, 6, Width-5, 19);
       Canvas.Brush.Style := bsSolid;
      end;
    end;
  end;

 colorBM := TBitmap.Create;
 with colorBM do
  begin
   Height := GetSystemMetrics(SM_CYCURSOR);
   Width := GetSystemMetrics(SM_CXCURSOR);
   with Canvas do
    begin
     Brush.Color := clBlack;
     FillRect(Rect(0, 0, Width, Height));
     Canvas.Brush.Color := clBlack;
     Canvas.Pen.Color := clBlack;
     Canvas.Polygon([Point(1,6), Point(1,22), Point(5, 18), Point(9,26), Point(10,26),
                    Point(11,25),  Point(8,18), Point(8,17), Point(12,17), Point(2,7), Point(1,7)]);
     Canvas.Brush.Color := clWhite;
     Canvas.Polygon([Point(Width - 8, 1), Point(Width-8,11), Point(Width-1,11), Point(Width-1,4), Point(Width-4,4),
                     Point(Width-4,1), Point(Width-1,4), Point(Width-4, 1), Point(Width-8,1)]);
     Canvas.Pixels[Width-3, 3] := clWhite;
     Canvas.Brush.Color := DragCl;
     if DragCl <> clTransparent then
      Canvas.FillRect(Rect(Width-18, 6, Width-5, 19))
     else
      begin
       Canvas.Brush.Style := bsClear;
       Canvas.Rectangle(Width-18, 6, Width-5, 19);
       Canvas.Brush.Style := bsSolid;
      end;
   end;
  end;

 with ii do
  begin
   fIcon := false;
   xHotspot := 0;
   yHotspot := 5;
   hbmMask := maskBM.Handle;
   hbmColor := colorBM.Handle;
  end;
 Screen.Cursors[crColored] := CreateIconIndirect(ii);
 maskBM.Free;
 colorBM.Free;
end;

procedure TForm1.CreateLineCursor;
var
 ii : TIconInfo;
 maskBM, colorBM : TBitmap;
begin
 maskBM := TBitmap.Create;
 with maskBM do
  begin
   Height := GetSystemMetrics(SM_CYCURSOR);
   Width := GetSystemMetrics(SM_CXCURSOR);
   with Canvas do
    begin
     Brush.Color := clWhite;
     FillRect(Rect(0, 0, Width, Height));
     Brush.Color := clBlack;
     Canvas.Brush.Color := clBlack;
     Canvas.Pen.Color := clBlack;
     Canvas.Polygon([Point(1,6), Point(1,22), Point(5, 18), Point(9,26), Point(10,26),
                    Point(11,25),  Point(8,18), Point(8,17), Point(12,17), Point(2,7), Point(1,7)]);
     Canvas.Polygon([Point(Width - 8, 1), Point(Width-8,11), Point(Width-1,11), Point(Width-1,4), Point(Width-4,4),
                     Point(Width-4,1), Point(Width-1,4), Point(Width-4, 1), Point(Width-8,1)]);
     Canvas.Pixels[Width-3, 3] := clBlack;
     if DragCl <> clTransparent then
      begin
       Canvas.Brush.Style := bsClear;
       Canvas.Pen.Width := 3;
       Canvas.Rectangle(Rect(Width-17, 7, Width-6, 18));
       Canvas.Pen.Width := 1;
       Canvas.Brush.Style := bsSolid;
      end
     else
      begin
       Canvas.Pen.Color := clBlack;
       Canvas.Brush.Style := bsClear;
       Canvas.Rectangle(Rect(Width-18, 6, Width-5, 19));
       Canvas.Rectangle(Rect(Width-15, 9, Width-8, 16));
       Canvas.Brush.Style := bsSolid;
      end;
    end;
  end;

 colorBM := TBitmap.Create;
 with colorBM do
  begin
   Height := GetSystemMetrics(SM_CYCURSOR);
   Width := GetSystemMetrics(SM_CXCURSOR);
   with Canvas do
    begin
     Brush.Color := clBlack;
     FillRect(Rect(0, 0, Width, Height));
     Canvas.Brush.Color := clBlack;
     Canvas.Pen.Color := clBlack;
     Canvas.Polygon([Point(1,6), Point(1,22), Point(5, 18), Point(9,26), Point(10,26),
                    Point(11,25),  Point(8,18), Point(8,17), Point(12,17), Point(2,7), Point(1,7)]);
     Canvas.Brush.Color := clWhite;
     Canvas.Polygon([Point(Width - 8, 1), Point(Width-8,11), Point(Width-1,11), Point(Width-1,4), Point(Width-4,4),
                     Point(Width-4,1), Point(Width-1,4), Point(Width-4, 1), Point(Width-8,1)]);
     Canvas.Pixels[Width-3, 3] := clWhite;
     if DragCl <> clTransparent then
      begin
       Canvas.Pen.Color := DragCl;
       Canvas.Brush.Style := bsClear;
       Canvas.Pen.Width := 3;
       Canvas.Rectangle(Rect(Width-17, 7, Width-6, 18));
       Canvas.Pen.Width := 1;
       Canvas.Brush.Style := bsSolid;
      end
     else
      begin
       Canvas.Pen.Color := clBlack;
       Canvas.Brush.Style := bsClear;
       Canvas.Rectangle(Rect(Width-18, 6, Width-5, 19));
       Canvas.Rectangle(Rect(Width-15, 9, Width-8, 16));
       Canvas.Brush.Style := bsSolid;
      end;
   end;
  end;

 with ii do
  begin
   fIcon := false;
   xHotspot := 0;
   yHotspot := 5;
   hbmMask := maskBM.Handle;
   hbmColor := colorBM.Handle;
  end;
 Screen.Cursors[crColoredL] := CreateIconIndirect(ii);
 maskBM.Free;
 colorBM.Free;
end;

procedure TForm1.Shape1DragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
 if source = mbColorPalette1 then
  begin
   Accept := true;
   if (((x < Shape1.Pen.Width + 1) or (x > Shape1.Width - (Shape1.Pen.Width + 1))) or ((y < (Shape1.Pen.Width + 1)) or (y > Shape1.Height - (Shape1.Pen.Width + 1)))) and PtInRect(Shape1.ClientRect, Point(x, y)) then
    mbColorPalette1.DragCursor := crColoredL
   else
    mbColorPalette1.DragCursor := crColored;
  end;
end;

procedure TForm1.Shape1DragDrop(Sender, Source: TObject; X, Y: Integer);
begin
 if source = mbColorPalette1 then
  begin
   if (((x < Shape1.Pen.Width + 1) or (x > Shape1.Width - (Shape1.Pen.Width + 1))) or ((y < (Shape1.Pen.Width + 1)) or (y > Shape1.Height - (Shape1.Pen.Width + 1)))) and PtInRect(Shape1.ClientRect, Point(x, y)) then
    begin
     if Expanded then
      case dIndex of
       1: Shape1.Pen.Color := LineCl;
       2: Shape1.Pen.Color := clBtnFace;
       3: Shape1.Pen.Color := MeshCl;
      else
       Shape1.Pen.Color := DragCl;
      end
     else
      case dIndex of
       0: Shape1.Pen.Color := LineCl;
       1: Shape1.Pen.Color := clBtnFace;
       2: Shape1.Pen.Color := MeshCl;
      else
       Shape1.Pen.Color := DragCl;
      end;
    end
   else
    if Expanded then
     case dIndex of
      1: Shape1.Brush.Color := FillCl;
      2: Shape1.Brush.Color := clBtnFace;
      3: Shape1.Brush.Color := MeshCl;
     else
      Shape1.Brush.Color := DragCl;
     end
    else
     case dIndex of
      0: Shape1.Brush.Color := FillCl;
      1: Shape1.Brush.Color := clBtnFace;
      2: Shape1.Brush.Color := MeshCl;
     else
      Shape1.Brush.Color := DragCl;
     end;
   Cancel := false;
  end;
end;

procedure TForm1.mbColorPalette1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
 oldDragCl: TColor;
begin
 dIndex := mbColorPalette1.GetIndexUnderCursor;
 DragCl := mbColorPalette1.GetColorUnderCursor;
 oldDragCl := DragCl;
 if Expanded then
  case dIndex of
   1: DragCl := FillCl;
   3: DragCl := MeshCl;
  end
 else
  case dIndex of
   0: DragCl := FillCl;
   2: DragCl := MeshCl;
  end;
 CreateFillCursor;
 DragCl := oldDragCl;
 if Expanded then
  case dIndex of
   1: DragCl := LineCl;
   3: DragCl := MeshCl;
  end
 else
  case dIndex of
   0: DragCl := LineCl;
   2: DragCl := MeshCl;
  end;
 CreateLineCursor;
 Cancel := false;
 mdx := x;
 mdy := y;
end;

procedure TForm1.mbColorPalette1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
 if ssLeft in Shift then
  begin
   if not (expanded and (dIndex = 0)) and (((x >= mdx + 10) or (x <= mdx - 10)) or ((y >= mdy + 10) or (y <= mdy - 10))) then
    begin
     Cancel := true;
     mbColorPalette1.BeginDrag(true, -10);
    end;
  end
 else
  Cancel := false;
end;

procedure TForm1.lstFontsDrawItem(Sender: TTBXCustomList;
  ACanvas: TCanvas; ARect: TRect; AIndex, AHoverIndex: Integer;
  var DrawDefault: Boolean);
var
  S: string;
begin
  S := lstFonts.Strings[AIndex];
  if s = '' then
   begin
    ACanvas.Pen.Color := clWindowText;
    ACanvas.MoveTo(0, ARect.Top + (ARect.Bottom - ARect.Top) div 2);
    ACanvas.LineTo(ARect.Right, ARect.Top + (ARect.Bottom - ARect.Top) div 2);
   end
  else
   begin
    ACanvas.Font.Size := 12;
    ACanvas.Font.Name := S;
   end;
end;

procedure TForm1.lstFontsMeasureHeight(Sender: TTBXCustomList;
  ACanvas: TCanvas; var AHeight: Integer);
begin
  AHeight := AHeight * 3 div 2;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
 allFonts := TStringList.Create;
 mruFonts := TStringList.Create;
 lstFonts.Strings.Text := Screen.Fonts.Text;
 allFonts.Text := Screen.Fonts.Text;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
 allFonts.Free;
 mruFonts.Free;
end;

procedure RemDuplicates(var s: TStringList);
var
 i, j: integer;
 m: string;
begin
 for i := 0 to s.Count - 1 do
  if i < s.Count then
   begin
    m := s.Strings[i];
    for j := s.Count - 1 downto 0 do
     if ((s.Strings[j] = m) and (j <> i)) or (j > maxMRUFonts - 1) then
      s.Delete(j);
   end
  else
   Break;
end;

procedure TForm1.lstFontsAcceptText(Sender: TObject; var NewText: String;
  var Accept: Boolean);
begin
 if Text <> '' then
  mruFonts.Insert(0, lstFonts.Text)
 else
  Accept := false;
 RemDuplicates(mruFonts);
 lstFonts.Strings.Clear;
 lstFonts.Strings.AddStrings(mruFonts);
 lstFonts.Strings.Add('');
 lstFonts.Strings.AddStrings(allFonts);
end;

procedure TForm1.lstFontsChange(Sender: TObject; const Text: String);
begin
 if Text <> '' then
  mruFonts.Insert(0, lstFonts.Text);
 RemDuplicates(mruFonts);
 lstFonts.Strings.Clear;
 lstFonts.Strings.AddStrings(mruFonts);
 lstFonts.Strings.Add('');
 lstFonts.Strings.AddStrings(allFonts);
end;

procedure TForm1.ScrlUpScroll(Sender: TObject);
begin
 mbColorPalette1.Align := alNone;
 if mbColorPalette1.Top < 0 then
  begin
   mbColorPalette1.Top := mbColorPalette1.Top + mbColorPalette1.CellSize;
   ScrlDown.Enabled := true;
  end
 else
  ScrlUp.Enabled := false;
end;

procedure TForm1.ScrlDownScroll(Sender: TObject);
begin
 mbColorPalette1.Align := alNone;
 if mbColorPalette1.Top - mbColorPalette1.CellSize >= - (mbColorPalette1.Height - PalVHolder.Height) then
  begin
   mbColorPalette1.Top := mbColorPalette1.Top - mbColorPalette1.CellSize;
   ScrlUp.Enabled := true;
  end
 else
  ScrlDown.Enabled := false;
end;

procedure TForm1.PalVHolderResize(Sender: TObject);
begin
 mbColorPalette1.Align := alNone;
 if PalVHolder.Height mod mbColorPalette1.CellSize > 0 then
  PalVHolder.Height := (PalVHolder.Height div mbColorPalette1.CellSize) * mbColorPalette1.CellSize;
 if mbColorPalette1.Top > 0 then mbColorPalette1.Top := 0;
 if mbColorPalette1.Top < - (mbColorPalette1.Height - PalVHolder.Height) then mbColorPalette1.Top := - (mbColorPalette1.Height - PalVHolder.Height);
 ScrlDown.Top := PalVHolder.Top + PalVHolder.Height;
 ScrlUp.Enabled := (mbColorPalette1.Top < 0);
 ScrlDown.Enabled := (mbColorPalette1.Top > - (mbColorPalette1.Height - PalVHolder.Height));
end;

procedure TForm1.PalVHolderCanResize(Sender: TObject; var NewWidth,
  NewHeight: Integer; var Resize: Boolean);
begin
 if NewHeight > mbColorPalette1.Height then
  NewHeight := mbColorPalette1.Height;
end;

procedure TForm1.ScrlLeftScroll(Sender: TObject);
begin
 mbColorPalette1.Align := alNone;
 if mbColorPalette1.Left < 0 then
  begin
   mbColorPalette1.Left := mbColorPalette1.Left + mbColorPalette1.CellSize;
   ScrlRight.Enabled := true;
  end
 else
  ScrlLeft.Enabled := false;
end;

procedure TForm1.ScrlRightScroll(Sender: TObject);
begin
 mbColorPalette1.Align := alNone;
 if mbColorPalette1.Left - mbColorPalette1.CellSize > - (mbColorPalette1.Width - PalHHolder.Width) then
  begin
   mbColorPalette1.Left := mbColorPalette1.Left - mbColorPalette1.CellSize;
   ScrlLeft.Enabled := true;
  end
 else
  ScrlRight.Enabled := false;
end;

procedure TForm1.PalHHolderCanResize(Sender: TObject; var NewWidth,
  NewHeight: Integer; var Resize: Boolean);
begin
 if NewWidth > mbColorPalette1.Width then
  NewWidth := mbColorPalette1.Width;
end;

procedure TForm1.PalHHolderResize(Sender: TObject);
begin
 mbColorPalette1.Align := alNone;
 if PalHHolder.Width mod mbColorPalette1.CellSize > 0 then
  PalHHolder.Width := (PalHHolder.Width div mbColorPalette1.CellSize) * mbColorPalette1.CellSize;
 ScrlRight.Left := PalHHolder.Left + PalHHolder.Width;
 if mbColorPalette1.Left > 0 then mbColorPalette1.Left := 0;
 if mbColorPalette1.Left < - (mbColorPalette1.Width - PalVHolder.Width) then mbColorPalette1.Left := - (mbColorPalette1.Width - PalVHolder.Width);
 ScrlLeft.Enabled := (mbColorPalette1.Left < 0);
 ScrlRight.Enabled := (mbColorPalette1.Left > - (mbColorPalette1.Width - PalVHolder.Width));
end;

procedure TForm1.SetOutlineColor1Click(Sender: TObject);
begin
 if LineCl = clTransparent then
  Shape1.Pen.Color := clBtnFace
 else
  Shape1.Pen.Color := LineCl;
end;

procedure TForm1.SetFillColor1Click(Sender: TObject);
begin
 if FillCl = clTransparent then
  Shape1.Brush.Color := clBtnFace
 else
  Shape1.Brush.Color := FillCl;
end;

end.
