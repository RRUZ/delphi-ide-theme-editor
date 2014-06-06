//**************************************************************************************************
//
// Unit Colorizer.Hooks
// unit Colorizer.Hooks for the Delphi IDE Colorizer
//
// The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
// you may not use this file except in compliance with the License. You may obtain a copy of the
// License at http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
// ANY KIND, either express or implied. See the License for the specific language governing rights
// and limitations under the License.
//
// The Original Code is Colorizer.Hooks.pas.
//
// The Initial Developer of the Original Code is Rodrigo Ruz V.
// Portions created by Rodrigo Ruz V. are Copyright (C) 2011-2014 Rodrigo Ruz V.
// All Rights Reserved.
//
//**************************************************************************************************

unit Colorizer.Hooks;

interface
{$I ..\Common\Jedi.inc}

uses
  Controls;
const
{$IFDEF DELPHIXE}  sVclIDEModule =  'vclide150.bpl';{$ENDIF}
{$IFDEF DELPHIXE2} sVclIDEModule =  'vclide160.bpl';{$ENDIF}
{$IFDEF DELPHIXE3} sVclIDEModule =  'vclide170.bpl';{$ENDIF}
{$IFDEF DELPHIXE4} sVclIDEModule =  'vclide180.bpl';{$ENDIF}
{$IFDEF DELPHIXE5} sVclIDEModule =  'vclide190.bpl';{$ENDIF}
{$IFDEF DELPHIXE6} sVclIDEModule =  'vclide200.bpl';{$ENDIF}

{$IFDEF DELPHIXE}  sCoreIDEModule =  'coreide150.bpl';{$ENDIF}
{$IFDEF DELPHIXE2} sCoreIDEModule =  'coreide160.bpl';{$ENDIF}
{$IFDEF DELPHIXE3} sCoreIDEModule =  'coreide170.bpl';{$ENDIF}
{$IFDEF DELPHIXE4} sCoreIDEModule =  'coreide180.bpl';{$ENDIF}
{$IFDEF DELPHIXE5} sCoreIDEModule =  'coreide190.bpl';{$ENDIF}
{$IFDEF DELPHIXE6} sCoreIDEModule =  'coreide200.bpl';{$ENDIF}

 procedure InstallColorizerHooks;
 procedure RemoveColorizerHooks;
 procedure DrawNCBorder(Self : TWinControl; EraseLRCorner: Boolean);

var
  LastScrollWinControl  : TWinControl = nil;

implementation

uses
{$IFDEF DELPHIXE2_UP}
  Vcl.Styles,
  Vcl.Themes,
{$ELSE}
  Themes,
  UxTheme,
  Colorizer.uxThemeHelper,
{$ENDIF}
  Messages,
  Forms,
  IOUtils,
  ExtCtrls,
  Dialogs,
  ComCtrls,
  Windows,
  Classes,
 {$IFDEF DELPHI2009_UP}
  Generics.Collections,
 {$ENDIF}
  uDelphiVersions,
  uDelphiIDEHighlight,
  SysUtils,
  Graphics,
  ImgList,
  CommCtrl,
  JclDebug,
  PngImage,
  Colorizer.Utils,
  Colorizer.Wrappers,
  CaptionedDockTree,
  GraphUtil,
  CategoryButtons,
  ActnPopup,
  ActnMan,
  StdCtrls,
  Tabs,
  Types,
  uMisc,
  DDetours;

type
 TWinControlClass        = class(TWinControl);
 TCustomPanelClass       = class(TCustomPanel);
 TCustomStatusBarClass   = class(TCustomStatusBar);
 TDockCaptionDrawerClass = class(TDockCaptionDrawer);
{$IFDEF DELPHIXE2_UP}
 TUxThemeStyleClass      = class(TUxThemeStyle);
{$ENDIF}
 TCustomFormClass        = class(TCustomForm);
 TBrushClass             = class(TBrush);
 TCustomListViewClass    = class(TCustomListView);
 TSplitterClass          = class(TSplitter);
 TButtonControlClass     = class(TButtonControl);
 TCustomCheckBoxClass    = class(TCustomCheckBox);
 TRadioButtonClass       = class(TRadioButton);
 TCustomComboClass       = class(TCustomCombo);
 TBevelClass             = class(TBevel);
 TCustomControlClass     = class(TCustomControl);
var
  {$IF CompilerVersion<27} //XE6
  TrampolineCustomImageList_DoDraw     : procedure (Self: TObject; Index: Integer; Canvas: TCanvas; X, Y: Integer; Style: Cardinal; Enabled: Boolean) = nil;
  {$IFEND}
  Trampoline_TCanvas_FillRect          : procedure (Self: TCanvas;const Rect: TRect) = nil;
  Trampoline_TCanvas_LineTo            : procedure (Self: TCanvas; X, Y: Integer) = nil;
  Trampoline_TCanvas_Rectangle         : procedure (Self: TCanvas; X1, Y1, X2, Y2: Integer) = nil;

  TrampolineTWinControl_DefaultHandler: procedure (Self : TWinControl;var Message) = nil;

  Trampoline_TCustomStatusBar_WMPAINT  : procedure (Self: TCustomStatusBarClass; var Message: TWMPaint) = nil;
  Trampoline_TDockCaptionDrawer_DrawDockCaption      : function (Self : TDockCaptionDrawerClass;const Canvas: TCanvas; CaptionRect: TRect; State: TParentFormState): TDockCaptionHitTest =nil;
  {$IFDEF DELPHIXE6_UP}
  Trampoline_ModernDockCaptionDrawer_DrawDockCaption : function (Self : TDockCaptionDrawerClass;const Canvas: TCanvas; CaptionRect: TRect; State: TParentFormState): TDockCaptionHitTest =nil;
  {$ENDIF}
  {$IFDEF DELPHIXE2_UP}
  Trampoline_TStyleEngine_HandleMessage    : function (Self: TStyleEngine; Control: TWinControl; var Message: TMessage; DefWndProc: TWndMethod): Boolean = nil;
  Trampoline_TUxThemeStyle_DoDrawElement   : function (Self : TUxThemeStyle;DC: HDC; Details: TThemedElementDetails; const R: TRect; ClipRect: PRect = nil): Boolean = nil;
  {$ELSE}
  Trampoline_TUxTheme_DrawElement          : procedure (Self : TThemeServices;DC: HDC; Details: TThemedElementDetails; const R: TRect; ClipRect: TRect);
  Trampoline_DrawThemeBackground           : function (hTheme: UxTheme.HTHEME; hdc: HDC; iPartId, iStateId: Integer; const pRect: TRect; pClipRect: PRECT): HRESULT; stdcall = nil;
  {$ENDIF}
  Trampoline_TCustomListView_HeaderWndProc : procedure (Self:TCustomListView;var Message: TMessage) = nil;
  Trampoline_ProjectTree2PaintText         : procedure (Self : TObject; Sender: TObject{TBaseVirtualTree}; const TargetCanvas: TCanvas; Node: {PVirtualNode}Pointer; Column: Integer{TColumnIndex}; TextType: Byte {TVSTTextType})=nil;
  Trampoline_DrawText                      : function (hDC: HDC; lpString: LPCWSTR; nCount: Integer;  var lpRect: TRect; uFormat: UINT): Integer; stdcall = nil;
  Trampoline_DrawTextEx                    : function (DC: HDC; lpchText: LPCWSTR; cchText: Integer; var p4: TRect;  dwDTFormat: UINT; DTParams: PDrawTextParams): Integer; stdcall = nil;
  Trampoline_GetSysColor                   : function (nIndex: Integer): DWORD; stdcall = nil;

  Trampoline_TCategoryButtons_DrawCategory : procedure(Self :TCategoryButtons; const Category: TButtonCategory; const Canvas: TCanvas; StartingPos: Integer) = nil;
  //Trampoline_TBitmap_SetSize : procedure(Self : TBitmap;AWidth, AHeight: Integer) = nil;
  Trampoline_TCustomPanel_Paint            : procedure (Self : TCustomPanelClass) = nil;
  Trampoline_TWinControl_WMNCPaint         : procedure (Self: TWinControlClass; var Message: TWMNCPaint);
  Trampoline_TSplitter_Paint               : procedure (Self : TSplitterClass) = nil;
  Trampoline_CustomComboBox_WMPaint        : procedure (Self: TCustomComboBox;var Message: TWMPaint) = nil;
  Trampoline_TCustomCombo_WndProc          : procedure (Self: TCustomCombo;var Message: TMessage) = nil;

  Trampoline_TButtonControl_WndProc        : procedure (Self:TButtonControlClass;var Message: TMessage) = nil;

  Trampoline_DrawFrameControl              : function (DC: HDC; Rect: PRect; uType, uState: UINT): BOOL; stdcall = nil;
  Trampoline_DrawEdge                      : function (hdc: HDC; var qrc: TRect; edge: UINT; grfFlags: UINT): BOOL; stdcall = nil;

  Trampoline_DoModernPainting              : procedure (Self : TTabSet) = nil;

  //002B7ADC 10611 2EDD __fastcall Msglines::TCompilerMsgLine::Draw(Vcl::Graphics::TCanvas *, System::Types::TRect&, bool)
  Trampoline_CompilerMsgLine_Draw          : procedure (Self : TObject;Canvas : TCanvas; Rect : TRect; Flag : Boolean) = nil;
  //002B7360 10632 2F54 __fastcall Msglines::TTitleLine::Draw(Vcl::Graphics::TCanvas *, System::Types::TRect&, bool)
  Trampoline_TitleLine_Draw                : procedure (Self : TObject;Canvas : TCanvas; Rect : TRect; Flag : Boolean) = nil;
  Trampoline_TFileFindLine_Draw            : procedure (Self : TObject;Canvas : TCanvas; Rect : TRect; Flag : Boolean) = nil;
  //002B7E78 10606 2F00 __fastcall Msglines::TFileFindLine::InternalCalcDraw(Vcl::Graphics::TCanvas *, System::Types::TRect&, bool, bool)
  Trampoline_TFileFindLine_InternalCalcDraw: procedure (Self : TObject;Canvas : TCanvas; Rect : TRect; Flag, Flag2 : Boolean) = nil;

  Trampoline_HintWindow_Paint              : procedure (Self : THintWindow) = nil;
  Trampoline_Bevel_Paint                   : procedure (Self : TBevel) = nil;

  //000E6D74 4639 1D48 __fastcall Idevirtualtrees::TBaseVirtualTree::PrepareBitmaps(bool, bool)
  Trampoline_TBaseVirtualTree_PrepareBitmaps : procedure (Self : TCustomControl;NeedButtons, NeedLines: Boolean) = nil;

  //0005B7F4 1682 0BCA __fastcall Idelistbtns::TListButton::Paint()
  Trampoline_TListButton_Paint : procedure (Self : TCustomControl) = nil;

{$IFDEF DLLWIZARD}
  Trampoline_TCustomForm_WndProc :  procedure (Self : TCustomForm;var Message: TMessage) = nil;
{$ENDIF}

  FGutterBkColor : TColor = clNone;

type
  TCustomStatusBarHelper = class helper for TCustomStatusBar
  private
    function GetCanvasRW: TCanvas;
    procedure SetCanvasRW(const Value: TCanvas);
  public
    function  WMPaintAddress: Pointer;
    procedure DoUpdatePanels(UpdateRects, UpdateText: Boolean);
    property  CanvasRW : TCanvas read GetCanvasRW Write SetCanvasRW;
   end;

  TCustomFormHelper = class helper for TCustomForm
  public
    function  SetVisibleAddress: Pointer;
   end;

  TTabSetHelper = class helper for TTabSet
  public
    function  DoModernPaintingAddress: Pointer;
    function  GetMemBitmap: TBitmap;
    function  GetEdgeWidth: Integer;
    function  GetTabPositions: TList;
  end;


  TCustomListViewHelper = class helper for TCustomListView
  public
    function  HeaderWndProcAddress: Pointer;
    function  GetHeaderHandle: HWND;
   end;

  TCategoryButtonsHelper = class helper for TCategoryButtons
  public
    function  DrawCategoryAddress: Pointer;
    procedure GetCategoryBoundsHelper(const Category: TButtonCategory; const StartingPos: Integer; var CategoryBounds, ButtonBounds: TRect);
    procedure AdjustCategoryBoundsHelper(const Category: TButtonCategory; var CategoryBounds: TRect; IgnoreButtonFlow: Boolean = False);
    function  GetChevronBoundsHelper(const CategoryBounds: TRect): TRect;
    function  FSideBufferSizeHelper : Integer;
    function  FHotButtonHelper: TButtonItem;
    function  FDownButtonHelper: TButtonItem;
   end;

  TCustomComboBoxBarHelper = class helper for TCustomComboBox
  public
    function  WMPaintAddress: Pointer;
  end;

  TWinControlHelper  = class helper for TWinControl
  public
    function WMNCPaintAddress : Pointer;
  end;

var
   ListControlWrappers  : TObjectDictionary<TCustomControl, TRttiWrapper>;

type
  TTabSetClass = class(TTabSet);

{$IFDEF DELPHIXE2_UP}

{$ELSE}
  TDWordFiller = record
  {$IFDEF CPUX64}
    Filler: array[1..4] of Byte;
  {$ENDIF}
  end;
{$ENDIF}

{$IFDEF DLLWIZARD}
procedure Detour_TCustomForm_WndProc(Self : TCustomForm;var Message: TMessage);
begin
 if Assigned(TColorizerLocalSettings.Settings) and TColorizerLocalSettings.Settings.Enabled and (SameText(Self.ClassName, 'TAppBuilder')) then
 case Message.Msg of
  WM_CLOSE  :
  //WM_DESTROY,
  //WM_QUIT :
     begin
       //AddLog('Detour_TCustomForm_WndProc', Self.ClassName+' ' +WM_To_String(Message.Msg));
       //RestoreIDESettings();
       RestoreIDESettingsFast();
     end;
 end;

 Trampoline_TCustomForm_WndProc(Self, Message);
end;
{$ENDIF}

//Detour for TListButton.Paint
procedure Detour_TListButton_Paint(Self : TCustomControlClass);
var
  ArrowSize, i  : integer;
  LPoint     : TPoint;
  ListButton : TRttiListButton;
  LRect      : TRect;
  LParentForm : TCustomForm;
begin
  if (Assigned(TColorizerLocalSettings.Settings) and not TColorizerLocalSettings.Settings.Enabled) or (csDesigning in Self.ComponentState) or (not Assigned(TColorizerLocalSettings.ColorMap)) then
  begin
    Trampoline_TListButton_Paint(Self);
    exit;
  end;

  LParentForm:= GetParentForm(Self);
  if not (Assigned(LParentForm) and Assigned(TColorizerLocalSettings.HookedWindows) and (TColorizerLocalSettings.HookedWindows.IndexOf(LParentForm.ClassName)>=0)) then
  begin
    Trampoline_TListButton_Paint(Self);
    exit;
  end;

  if not ListControlWrappers.ContainsKey(Self) then
   ListControlWrappers.Add(Self, TRttiListButton.Create(Self));
  ListButton := TRttiListButton(ListControlWrappers.Items[Self]);

  //ListButton.ListBox.Color:=TColorizerLocalSettings.ColorMap.MenuColor;
  ArrowSize := 3;
  if Self.Width>16 then ArrowSize := 4;
  LPoint    := Point(Self.ClientRect.Left+4, Self.ClientRect.Top+6);

  Self.Canvas.Brush.Color:=TColorizerLocalSettings.ColorMap.MenuColor;
  Self.Canvas.Pen.Color  :=TColorizerLocalSettings.ColorMap.FrameTopLeftOuter;
  Self.Canvas.Rectangle(Self.ClientRect);

  if (ListButton.Items.Count>0) or (ListButton.PopupPanel<>nil) then
  begin
    Self.Canvas.Brush.Color:=TColorizerLocalSettings.ColorMap.FontColor;
    Self.Canvas.Pen.Color  :=TColorizerLocalSettings.ColorMap.FontColor;
    DrawArrow(Self.Canvas, TScrollDirection.sdDown, LPoint, ArrowSize);
  end
  else
  begin
    LPoint    := Point(Self.ClientRect.Left+4, Self.ClientRect.Top+6);
    for i := 0 to 2 do
    begin
     LRect := Rect(LPoint.X, LPoint.Y, LPoint.X+2, LPoint.Y+2);
     Self.Canvas.Brush.Color:=TColorizerLocalSettings.ColorMap.FontColor;
     Self.Canvas.FillRect(LRect);
     LPoint.X:=LPoint.X+3;
    end;
  end;
end;

//Detour for TBaseVirtualTree.PrepareBitmaps
procedure Detour_TBaseVirtualTree_PrepareBitmaps(Self : TCustomControl;NeedButtons, NeedLines: Boolean);
const
  LineBitsDotted: array [0..8] of Word = ($55, $AA, $55, $AA, $55, $AA, $55, $AA, $55);
var
  PatternBitmap: HBITMAP;
  Bits: Pointer;
  Size: TSize;

  procedure FillBitmap(ABitmap: TBitmap);
  begin
    if Assigned(ABitmap) then
    with ABitmap, Canvas do
    begin
      Width := Size.cx;
      Height := Size.cy;
      Brush.Color := TColorizerLocalSettings.ColorMap.MenuColor;
      FillRect(Rect(0, 0, Width, Height));
    end;
  end;

var
  LRttiBaseVirtualTree : TRttiBaseVirtualTree;
  LParentForm          : TCustomForm;
begin

  if (Assigned(TColorizerLocalSettings.Settings) and not TColorizerLocalSettings.Settings.Enabled) or (csDesigning in Self.ComponentState) or (not Assigned(TColorizerLocalSettings.ColorMap)) then
  begin
    Trampoline_TBaseVirtualTree_PrepareBitmaps(Self, NeedButtons, NeedLines);
    exit;
  end;

  LParentForm:= GetParentForm(Self);
  if not (Assigned(LParentForm) and Assigned(TColorizerLocalSettings.HookedWindows) and (TColorizerLocalSettings.HookedWindows.IndexOf(LParentForm.ClassName)>=0)) then
  begin
    Trampoline_TBaseVirtualTree_PrepareBitmaps(Self, NeedButtons, NeedLines);
    exit;
  end;

    if not ListControlWrappers.ContainsKey(Self) then
     ListControlWrappers.Add(Self, TRttiBaseVirtualTree.Create(Self));
    LRttiBaseVirtualTree := TRttiBaseVirtualTree(ListControlWrappers.Items[Self]);

    Size.cx := 9;
    Size.cy := 9;

    if NeedButtons then
    begin
       if Assigned(LRttiBaseVirtualTree.MinusBM) then
       with LRttiBaseVirtualTree.MinusBM, Canvas do
       begin
        FillBitmap(LRttiBaseVirtualTree.MinusBM);
        Pen.Color := TColorizerLocalSettings.ColorMap.FontColor;
        Rectangle(0, 0, Width, Height);
        Pen.Color := TColorizerLocalSettings.ColorMap.FontColor;
        MoveTo(2, Width div 2);
        LineTo(Width - 2, Width div 2);
       end;

      if Assigned(LRttiBaseVirtualTree.PlusBM) then
      with LRttiBaseVirtualTree.PlusBM, Canvas do
      begin
        FillBitmap(LRttiBaseVirtualTree.PlusBM);
        Pen.Color := TColorizerLocalSettings.ColorMap.FontColor;
        Rectangle(0, 0, Width, Height);
        Pen.Color := TColorizerLocalSettings.ColorMap.FontColor;
        MoveTo(2, Width div 2);
        LineTo(Width - 2, Width div 2);
        MoveTo(Width div 2, 2);
        LineTo(Width div 2, Width - 2);
      end;
    end;

    if NeedLines then
    begin
      if LRttiBaseVirtualTree.DottedBrush <> 0 then
        DeleteObject(LRttiBaseVirtualTree.DottedBrush);

      Bits := @LineBitsDotted;
      PatternBitmap := CreateBitmap(8, 8, 1, 1, Bits);
      LRttiBaseVirtualTree.DottedBrush := CreatePatternBrush(PatternBitmap);
      DeleteObject(PatternBitmap);
    end;
end;

//Detour for TWinControl.DefaultHandler
procedure Detour_TWinControl_DefaultHandler(Self : TWinControl;var Message);
var
  LParentForm : TCustomForm;
begin
  LastScrollWinControl:=Self;

//  if SameText('TPopupListBox', Self.ClassName) then   //TInspListBox
//     AddLog('CustomDefaultHandler', WM_To_String(TMessage(Message).Msg));

    if (Assigned(TColorizerLocalSettings.Settings) and not TColorizerLocalSettings.Settings.Enabled) or (csDesigning in Self.ComponentState) or (not Assigned(TColorizerLocalSettings.ColorMap)) then
    begin
     TrampolineTWinControl_DefaultHandler(Self, Message);
     exit;
    end;

    LParentForm:= GetParentForm(Self);
    if not (Assigned(LParentForm) and Assigned(TColorizerLocalSettings.HookedWindows) and (TColorizerLocalSettings.HookedWindows.IndexOf(LParentForm.ClassName)>=0)) then
    begin
     TrampolineTWinControl_DefaultHandler(Self, Message);
      exit;
    end;

    case TMessage(Message).Msg of
        //CN_CTLCOLOREDIT,
        //CN_CTLCOLORLISTBOX,
        CN_CTLCOLORMSGBOX..CN_CTLCOLORSTATIC:
        begin
          //if (GetTextColor(TMessage(Message).WParam)<>ColorToRGB(TColorizerLocalSettings.ColorMap.FontColor)) or (Self.Brush.Color<>TColorizerLocalSettings.ColorMap.MenuColor) then
          begin
            //AddLog('CustomDefaultHandler', Self.ClassName);
            SetTextColor(TMessage(Message).WParam, ColorToRGB(TColorizerLocalSettings.ColorMap.FontColor));
            Self.Brush.Color:=TColorizerLocalSettings.ColorMap.MenuColor;
            SetBkColor(TMessage(Message).WParam, ColorToRGB(Self.Brush.Color));
            TMessage(Message).Result := Self.Brush.Handle;
            Exit;
          end;
        end;
    end;

  TrampolineTWinControl_DefaultHandler(Self, Message);
end;

//Detour for TBevel.Paint
procedure Detour_TBevel_Paint(Self : TBevelClass);
var
  LParentForm : TCustomForm;

  procedure BevelRect(const R: TRect);
  begin
    with Self.Canvas do
    begin
      Pen.Color := TColorizerLocalSettings.ColorMap.FrameTopLeftOuter;
      PolyLine([Point(R.Left, R.Bottom), Point(R.Left, R.Top), Point(R.Right, R.Top)]);
      PolyLine([Point(R.Right, R.Top), Point(R.Right, R.Bottom), Point(R.Left, R.Bottom)]);
    end;
  end;

  procedure BevelLine(X1, Y1, X2, Y2: Integer);
  begin
    with Self.Canvas do
    begin
      Pen.Color := TColorizerLocalSettings.ColorMap.FrameTopLeftOuter;
      MoveTo(X1, Y1);
      LineTo(X2, Y2);
    end;
  end;

begin
  if (Assigned(TColorizerLocalSettings.Settings) and not TColorizerLocalSettings.Settings.Enabled) or (csDesigning in Self.ComponentState) or (not Assigned(TColorizerLocalSettings.ColorMap)) then
  begin
   Trampoline_Bevel_Paint(Self);
   exit;
  end;

  LParentForm:= GetParentForm(Self);
  if not (Assigned(LParentForm) and Assigned(TColorizerLocalSettings.HookedWindows) and (TColorizerLocalSettings.HookedWindows.IndexOf(LParentForm.ClassName)>=0)) then
  begin
    Trampoline_Bevel_Paint(Self);
    exit;
  end;

  with Self.Canvas do
  begin

    Pen.Width := 1;

    case Self.Shape of
      bsBox: BevelRect(Rect(0, 0, Self.Width - 1, Self.Height - 1));
      bsFrame:
        begin
          BevelRect(Rect(1, 1, Self.Width - 1, Self.Height - 1));
          BevelRect(Rect(0, 0, Self.Width - 2, Self.Height - 2));
        end;
      bsTopLine:
        begin
          BevelLine(0, 0, Self.Width, 0);
          BevelLine(0, 1, Self.Width, 1);
        end;
      bsBottomLine:
        begin
          BevelLine(0, Self.Height - 2, Self.Width, Self.Height - 2);
          BevelLine(0, Self.Height - 1, Self.Width, Self.Height - 1);
        end;
      bsLeftLine:
        begin
          BevelLine(0, 0, 0, Self.Height);
          BevelLine(1, 0, 1, Self.Height);
        end;
      bsRightLine:
        begin
          BevelLine(Self.Width - 2, 0, Self.Width - 2, Self.Height);
          BevelLine(Self.Width - 1, 0, Self.Width - 1, Self.Height);
        end;
    end;
  end;
end;

//Detour for THintWindow.Paint
procedure Detour_THintWindow_Paint(Self : THintWindow);
var
  R, ClipRect: TRect;
  LTextColor: TColor;
  //LParentForm : TCustomForm;
begin
  if (Assigned(TColorizerLocalSettings.Settings) and not TColorizerLocalSettings.Settings.Enabled) or (csDesigning in Self.ComponentState) or (not Assigned(TColorizerLocalSettings.ColorMap)) then
  begin
   Trampoline_HintWindow_Paint(Self);
   exit;
  end;

//  LParentForm:= GetParentForm(Self);
//  if not (Assigned(LParentForm) and Assigned(TColorizerLocalSettings.HookedWindows) and (TColorizerLocalSettings.HookedWindows.IndexOf(LParentForm.ClassName)>=0)) then
//  begin
//    Trampoline_HintWindow_Paint(Self);
//    exit;
//  end;

  R := Self.ClientRect;
  LTextColor := TColorizerLocalSettings.ColorMap.FontColor;
  ClipRect := R;
  InflateRect(R, 4, 4);
  Self.Canvas.Brush.Color:= TColorizerLocalSettings.ColorMap.MenuColor;
  Self.Canvas.Pen.Color:= TColorizerLocalSettings.ColorMap.FrameTopLeftOuter;
  Self.Canvas.Rectangle(R);
  R := ClipRect;
  Inc(R.Left, 2);
  Inc(R.Top, 2);
  Self.Canvas.Font.Color := LTextColor;
  DrawText(Self.Canvas.Handle, Self.Caption, -1, R, DT_LEFT or DT_NOPREFIX or  DT_WORDBREAK or Self.DrawTextBiDiModeFlagsReadingOnly);
end;

//Hook for combobox fg and bg colors
procedure Detour_TCustomCombo_WndProc(Self: TCustomCombo;var Message: TMessage);
var
  LParentForm : TCustomForm;
begin
    if (Assigned(TColorizerLocalSettings.Settings) and not TColorizerLocalSettings.Settings.Enabled) or (csDesigning in Self.ComponentState) or (not Assigned(TColorizerLocalSettings.ColorMap)) then
    begin
     Trampoline_TCustomCombo_WndProc(Self, Message);
     exit;
    end;

    LParentForm:= GetParentForm(Self);
    if not (Assigned(LParentForm) and Assigned(TColorizerLocalSettings.HookedWindows) and (TColorizerLocalSettings.HookedWindows.IndexOf(LParentForm.ClassName)>=0)) then
    begin
     Trampoline_TCustomCombo_WndProc(Self, Message);
      exit;
    end;

    case Message.Msg of
      WM_CTLCOLORMSGBOX..WM_CTLCOLORSTATIC:
        begin
          SetTextColor(Message.WParam, ColorToRGB(TColorizerLocalSettings.ColorMap.FontColor));
          Self.Brush.Color:=TColorizerLocalSettings.ColorMap.MenuColor;
          SetBkColor(Message.WParam, ColorToRGB(Self.Brush.Color));
          Message.Result := Self.Brush.Handle;
          Exit;
        end;
    end;

    Trampoline_TCustomCombo_WndProc(Self, Message);
end;


procedure Detour_TTabSet_DoModernPainting(Self : TTabSet);
type
  TTabPos = record
    Size, StartPos: Word;
    StartPosFiller: TDWordFiller;
  end;

  procedure DrawLine(Canvas: TCanvas; FromX, FromY, ToX, ToY: Integer);
  var
    T: Integer;
  begin
    if Self.TabPosition in [tpLeft, tpRight] then
    begin
      T := FromX;
      FromX := FromY;
      FromY := T;
      T := ToX;
      ToX := ToY;
      ToY := T;
    end;
    Canvas.MoveTo(FromX, FromY);
    Canvas.LineTo(ToX, ToY);
  end;

var
  LRect, LMemBitmapRect: TRect;
  sText: string;
  Tab, YStart, TabOffset, MinRect, TotalSize, TabTop, ImageIndex: Integer;
  TabPos: TTabPos;
  DrawImage: Boolean;
  TabSelected, TabNextSelected: Boolean;
  LBackgroundColor: TColor;
  LParentForm : TCustomForm;
begin
    if (Assigned(TColorizerLocalSettings.Settings) and not TColorizerLocalSettings.Settings.Enabled) or (csDesigning in Self.ComponentState) or (not Assigned(TColorizerLocalSettings.ColorMap)) or (Assigned(TColorizerLocalSettings.Settings) and TColorizerLocalSettings.Settings.UseVCLStyles) then
    begin
     Trampoline_DoModernPainting(Self);
     exit;
    end;

    LParentForm:= GetParentForm(Self);
    if not (Assigned(LParentForm) and Assigned(TColorizerLocalSettings.HookedWindows) and (TColorizerLocalSettings.HookedWindows.IndexOf(LParentForm.ClassName)>=0)) then
    begin
     Trampoline_DoModernPainting(Self);
      exit;
    end;

  if Self.TabPosition in [tpBottom, tpRight] then
  begin
    TabTop := 2;
    YStart := 1;
  end
  else if Self.TabPosition = tpTop then
  begin
    TabTop := Self.ClientHeight - Self.TabHeight - 2;
    YStart := Self.ClientHeight - 2;
  end
  else
  begin
    TabTop := Self.ClientWidth - Self.TabHeight - 2;
    YStart := Self.ClientWidth - 2;
  end;

  if Self.TabPosition in [tpTop, tpBottom] then
    TotalSize := Self.GetMemBitmap.Width
  else
    TotalSize := Self.GetMemBitmap.Height;

  //background
  with Self.GetMemBitmap.Canvas do
  begin
    LBackgroundColor := TColorizerLocalSettings.ColorMap.Color;
    Brush.Color := LBackgroundColor;
    Pen.Width := 1;
    Pen.Color := LBackgroundColor;
    LMemBitmapRect := Types.Rect(0, 0, Self.GetMemBitmap.Width, Self.GetMemBitmap.Height);
    Rectangle(LMemBitmapRect);
    DrawLine(Self.GetMemBitmap.Canvas, 0, YStart, TotalSize, YStart);
    if Self.TabPosition in [tpBottom, tpRight] then
      Inc(YStart)
    else
      Dec(YStart);

    Pen.Color := TColorizerLocalSettings.ColorMap.FrameTopLeftOuter;
    DrawLine(Self.GetMemBitmap.Canvas, 0, YStart, TotalSize, YStart);

    MinRect := TextWidth('X...');
  end;

  TabOffset := Self.GetEdgeWidth div 2;

  for Tab := 0 to Self.GetTabPositions.Count - 1 do
  begin
    TabPos := TTabPos(Self.GetTabPositions[Tab]);
    TabSelected := Tab + Self.FirstIndex = Self.TabIndex;
    TabNextSelected := (Tab + Self.FirstIndex) + 1 = Self.TabIndex;

    if TabSelected then
    begin
      Self.GetMemBitmap.Canvas.Pen.Color := TColorizerLocalSettings.ColorMap.FrameTopLeftOuter;
      DrawLine( Self.GetMemBitmap.Canvas, TabPos.StartPos - TabOffset + 1, TabTop, TabPos.StartPos - TabOffset + 1, TabTop + Self.TabHeight);

      Self.GetMemBitmap.Canvas.Pen.Color := GetHighlightColor(Self.SelectedColor);

      DrawLine(Self.GetMemBitmap.Canvas, TabPos.StartPos - TabOffset + 2, TabTop, TabPos.StartPos - TabOffset + 2, TabTop + Self.TabHeight);
      Self.GetMemBitmap.Canvas.Pen.Color := TColorizerLocalSettings.ColorMap.FrameTopLeftOuter;

      if Self.TabPosition in [tpBottom, tpRight] then
        DrawLine(Self.GetMemBitmap.Canvas,
          TabPos.StartPos + 1 - TabOffset, TabTop + Self.TabHeight - 1, TabPos.StartPos + TabPos.Size + 1 + TabOffset, TabTop + Self.TabHeight - 1)
      else
        DrawLine(Self.GetMemBitmap.Canvas, TabPos.StartPos + 1 - TabOffset + 1, TabTop, TabPos.StartPos + TabPos.Size + 1 + TabOffset, TabTop);

      DrawLine(Self.GetMemBitmap.Canvas, TabPos.StartPos + TabPos.Size + TabOffset, TabTop, TabPos.StartPos + TabPos.Size + TabOffset, TabTop + Self.TabHeight - 1);
      Self.GetMemBitmap.Canvas.Brush.Color := Self.SelectedColor;

      if Self.TabPosition in [tpTop, tpBottom] then
        LRect := Types.Rect(TabPos.StartPos + 2 - TabOffset, TabTop, TabPos.StartPos + TabPos.Size + TabOffset, TabTop + Self.TabHeight - 1)
      else
        LRect := Types.Rect(TabTop, TabPos.StartPos + 2 - TabOffset, TabTop + Self.TabHeight - 1, TabPos.StartPos + TabPos.Size + TabOffset);

      if Self.TabPosition = tpTop then
      begin
        Inc(LRect.Left);
        Inc(LRect.Top);
        Inc(LRect.Bottom);
      end
      else if Self.TabPosition = tpLeft then
      begin
        Inc(LRect.Left);
        Inc(LRect.Top);
        Inc(LRect.Right);
      end
      else if Self.TabPosition = tpRight then
        Inc(LRect.Top)
      else
        Inc(LRect.Left);
      Self.GetMemBitmap.Canvas.FillRect(LRect);
    end
    else if not TabNextSelected then
    begin
      Self.GetMemBitmap.Canvas.Pen.Color := TColorizerLocalSettings.ColorMap.FrameTopLeftOuter;
      DrawLine(Self.GetMemBitmap.Canvas, TabPos.StartPos + TabPos.Size + TabOffset, TabTop + 3, TabPos.StartPos + TabPos.Size + TabOffset, TabTop + Self.TabHeight - 1 - 2);
    end;

    if Self.TabPosition in [tpTop, tpBottom] then
      LRect := Types.Rect(TabPos.StartPos, TabTop, TabPos.StartPos + TabPos.Size, TabTop + Self.TabHeight)
    else
      LRect := Types.Rect(TabTop, TabPos.StartPos, TabTop + Self.TabHeight, TabPos.StartPos + TabPos.Size);

    with Self.GetMemBitmap.Canvas do
    begin
      Brush.Style := bsClear;
      if Self.TabPosition in [tpTop, tpBottom] then
      begin
        Inc(LRect.Top, 2);
        Inc(LRect.Left, 1);
        Inc(LRect.Right, 1);
      end
      else
      begin
        if Self.TabPosition = tpRight then
          Inc(LRect.Left, 1 + TextHeight('X'))
        else
        begin
          Inc(LRect.Left, 2);
          LRect.Top := LRect.Top + TabPos.Size;
        end;
        LRect.Right := LRect.Left + TabPos.Size + 2;
        LRect.Bottom := LRect.Top + Self.TabHeight;
      end;

      //Draw Image
      if (Self.Images <> nil) then
      begin
        ImageIndex := TTabSetClass(Self).GetImageIndex(Tab + Self.FirstIndex);
        DrawImage := (ImageIndex > -1) and (ImageIndex < Self.Images.Count);
        if Self.TabPosition in [tpTop, tpBottom] then
        begin
          if DrawImage and (LRect.Left + 2 + Self.Images.Width < LRect.Right) then
          begin
            Self.Images.Draw(Self.GetMemBitmap.Canvas, LRect.Left, LRect.Top, ImageIndex);
            Inc(LRect.Left, 2 + Self.Images.Width);
          end;
          Inc(LRect.Top, 2);
        end
        else if Self.TabPosition = tpRight then
        begin
          if DrawImage then
          begin
            Self.Images.Draw(Self.GetMemBitmap.Canvas, LRect.Left - TextHeight('X') + 2,
              LRect.Top, ImageIndex);
            Inc(LRect.Top, 2 + Self.Images.Height);
            Dec(LRect.Right, Self.Images.Height);
          end;
          Inc(LRect.Left, 2);
        end
        else
        begin
          if DrawImage then
          begin
            Self.Images.Draw(Self.GetMemBitmap.Canvas, LRect.Left, LRect.Top - Self.Images.Height, ImageIndex);
            Dec(LRect.Top, 2 + Self.Images.Height);
            Dec(LRect.Right, Self.Images.Height);
          end;
          Inc(LRect.Left, 2);
        end;
      end;

      //draw text
      sText := Self.Tabs[Tab + Self.FirstIndex];
      if (LRect.Right - LRect.Left >= MinRect) or
          (TextWidth(sText) <= (LRect.Right - LRect.Left)) then
      begin
        Self.GetMemBitmap.Canvas.Font.Color := Self.Font.Color;
        TextRect(LRect, sText, [tfEndEllipsis, tfNoClip]);
      end;
    end;
  end;
end;

//Hook DrawEdge WinApi function
function Detour_WinApi_DrawEdge(hdc: HDC; var qrc: TRect; edge: UINT; grfFlags: UINT): BOOL; stdcall;
var
 LCanvas : TCanvas;
 OrgHWND : HWND;
 LWinControl : TWinControl;
 LParentForm : TCustomForm;
begin
   //DrawEdge(DC, R, EDGE_RAISED, BF_RECT or BF_MIDDLE or Flags);
  LWinControl:=nil;
  OrgHWND :=WindowFromDC(hdc);
  if OrgHWND<>0 then
     LWinControl :=FindControl(OrgHWND);

  if LWinControl<>nil then
  begin
    LParentForm:= GetParentForm(LWinControl);
    if not (Assigned(LParentForm) and Assigned(TColorizerLocalSettings.HookedWindows) and (TColorizerLocalSettings.HookedWindows.IndexOf(LParentForm.ClassName)>=0)) then
    begin
      //AddLog('CustomDrawEdge Ignored', IntToHex(OrgHWND,8));
      Exit(Trampoline_DrawEdge(hdc, qrc, edge, grfFlags));
    end;
  end;

   case  edge of
      EDGE_SUNKEN,
      EDGE_ETCHED,
      EDGE_BUMP,
      EDGE_RAISED :
                    begin
                        LCanvas:=TCanvas.Create;
                        try
                          LCanvas.Handle:=hdc;
                           if (BF_RECT and grfFlags = BF_RECT) then
                           begin
                            LCanvas.Brush.Color := TColorizerLocalSettings.ColorMap.MenuColor;
                            LCanvas.Pen.Color   := TColorizerLocalSettings.ColorMap.FrameTopLeftOuter;
                            LCanvas.Rectangle(qrc);
                           end;
                        finally
                          LCanvas.Handle:=0;
                          LCanvas.Free;
                        end;
                        Exit(True);
                    end;

   end;


   Exit(Trampoline_DrawEdge(hdc, qrc, edge, grfFlags));
end;

//hook for unthemed TCheckbox
function Detour_WinApi_DrawFrameControl(DC: HDC; Rect: PRect; uType, uState: UINT): BOOL; stdcall;
var
 LCanvas : TCanvas;
 OrgHWND : HWND;
 LWinControl : TWinControl;
 LParentForm : TCustomForm;
begin
   if( uType=DFC_BUTTON) and (Rect<>nil) and Assigned(TColorizerLocalSettings.Settings) and (TColorizerLocalSettings.Settings.Enabled) then
   begin

      if (DFCS_BUTTONCHECK and uState = DFCS_BUTTONCHECK) then
      begin
        LWinControl:=nil;
        OrgHWND :=WindowFromDC(DC);
        if OrgHWND<>0 then
           LWinControl :=FindControl(OrgHWND);

        if LWinControl<>nil then
        begin
          LParentForm:= GetParentForm(LWinControl);
          if not (Assigned(LParentForm) and Assigned(TColorizerLocalSettings.HookedWindows) and (TColorizerLocalSettings.HookedWindows.IndexOf(LParentForm.ClassName)>=0)) then
            Exit(Trampoline_DrawFrameControl(DC, Rect, uType, uState));
        end;

        LCanvas:=TCanvas.Create;
        try
          LCanvas.Handle:=DC;
           if (DFCS_CHECKED and uState = DFCS_CHECKED) then
           begin
            LCanvas.Brush.Color:= TColorizerLocalSettings.ColorMap.MenuColor;
            LCanvas.Pen.Color  :=TColorizerLocalSettings.ColorMap.FontColor;
//             if (DFCS_HOT and uState = DFCS_HOT) then
//              LCanvas.Pen.Color  :=TColorizerLocalSettings.ColorMap.SelectedColor;
            LCanvas.Rectangle(Rect^);
            DrawCheck(LCanvas, Point(Rect^.Left+3, Rect^.Top+6), 2, False);
           end
           else
           begin
            LCanvas.Brush.Color:=TColorizerLocalSettings.ColorMap.MenuColor;
            LCanvas.Pen.Color  :=TColorizerLocalSettings.ColorMap.FontColor;
//             if (DFCS_HOT and uState = DFCS_HOT) then
//              LCanvas.Pen.Color  :=TColorizerLocalSettings.ColorMap.SelectedColor;
            LCanvas.Rectangle(Rect^);
           end;
        finally
          LCanvas.Handle:=0;
          LCanvas.Free;
        end;
        Exit(True);
      end
      else
        Exit(Trampoline_DrawFrameControl(DC, Rect, uType, uState));
   end;
   Exit(Trampoline_DrawFrameControl(DC, Rect, uType, uState));
end;

function RectVCenter(var R: TRect; Bounds: TRect): TRect;
begin
  OffsetRect(R, -R.Left, -R.Top);
  OffsetRect(R, 0, ((Bounds.Bottom - Bounds.Top) - (R.Bottom - R.Top)) div 2);
  OffsetRect(R, Bounds.Left, Bounds.Top);

  Result := R;
end;

//Hook for change color of TCustomCheckBox
procedure Detour_TButtonControlClass_WndProc(Self : TButtonControlClass;var Message: TMessage);
var
  LBrush : TBrush;
  LParentForm : TCustomForm;
begin
  if (TButtonControl(Self) is TCustomCheckBox) and Assigned(TColorizerLocalSettings.Settings) and TColorizerLocalSettings.Settings.Enabled and not (csDesigning in Self.ComponentState) then
  begin
    LParentForm:= GetParentForm(Self);
    if not (Assigned(LParentForm) and Assigned(TColorizerLocalSettings.HookedWindows) and (TColorizerLocalSettings.HookedWindows.IndexOf(LParentForm.ClassName)>=0)) then
    begin
      Trampoline_TButtonControl_WndProc(Self, Message);
      exit;
    end;

    case Message.Msg of
        CN_CTLCOLORSTATIC:
        begin
          LBrush := Self.Brush;
          LBrush.Color:=TColorizerLocalSettings.ColorMap.Color;

          SetTextColor(Message.wParam, ColorToRGB(TColorizerLocalSettings.ColorMap.FontColor));
          SetBkColor(Message.wParam, ColorToRGB(LBrush.Color));
          Message.Result := LRESULT(LBrush.Handle);
          Exit;
        end;
    else
       Trampoline_TButtonControl_WndProc(Self, Message);
    end;
  end
  else
   Trampoline_TButtonControl_WndProc(Self, Message);
end;


type
  TCustomComboBoxClass = class(TCustomComboBox);
//Hook for combobox
procedure Detour_TCustomComboBox_WMPaint(Self: TCustomComboBoxClass;var Message: TWMPaint);
var
   FListHandle : HWND;
   FEditHandle : HWND;
  function GetButtonRect: TRect;
  begin
    Result := Self.ClientRect;
    InflateRect(Result, -2, -2);
    if Self.BiDiMode <> bdRightToLeft then
      Result.Left := Result.Right - GetSystemMetrics(SM_CXVSCROLL) + 1
    else
      Result.Right := Result.Left + GetSystemMetrics(SM_CXVSCROLL) - 1;
  end;

  procedure DrawItem(Canvas: TCanvas; Index: Integer; const R: TRect; Selected: Boolean);
  var
    DIS: TDrawItemStruct;
  begin
    FillChar(DIS, SizeOf(DIS), #0);
    DIS.CtlType := ODT_COMBOBOX;
    DIS.CtlID := GetDlgCtrlID(Self.Handle);
    DIS.itemAction := ODA_DRAWENTIRE;
    DIS.hDC := Canvas.Handle;
    DIS.hwndItem := Self.Handle;
    DIS.rcItem := R;
    DIS.itemID := Index;
    DIS.itemData := SendMessage(FListHandle, LB_GETITEMDATA, 0, 0);
    if Selected then
      DIS.itemState := DIS.itemState or ODS_FOCUS or ODS_SELECTED;

    SendMessage(Self.Handle, WM_DRAWITEM, Self.Handle, LPARAM(@DIS));
  end;

  procedure PaintBorder(Canvas: TCanvas);
  var
    R, ControlRect, EditRect, ListRect: TRect;
    //DrawState: TThemedComboBox;
    //BtnDrawState: TThemedComboBox;
    //Details: TThemedElementDetails;
    Buffer: TBitmap;
  begin
    //if not StyleServices.Available then Exit;

//    if not Self.Enabled then
//      BtnDrawState := tcDropDownButtonDisabled
//    else if Self.DroppedDown then
//      BtnDrawState := tcDropDownButtonPressed
//    else if Self.FMouseOnButton then
//      BtnDrawState := tcDropDownButtonHot
//    else
//      BtnDrawState := tcDropDownButtonNormal;

//    if not Self.Enabled then
//      DrawState := tcBorderDisabled
//    else
//    if Self.Focused then
//      DrawState := tcBorderFocused
//    else if MouseInControl then
//      DrawState := tcBorderHot
//    else
//      DrawState := tcBorderNormal;

    Buffer := TBitMap.Create;
    Buffer.SetSize(Self.Width, Self.Height);
    try
      R := Rect(0, 0, Buffer.Width, Buffer.Height);
      // draw border + client in buffer
      //Details := StyleServices.GetElementDetails(DrawState);

      if (Self.Style = csSimple) and (FListHandle <> 0) then
      begin
        GetWindowRect(FListHandle, ListRect);
        GetWindowRect(Self.Handle, ControlRect);
        R.Bottom := ListRect.Top - ControlRect.Top;

        Buffer.Canvas.Pen.Color:=TColorizerLocalSettings.ColorMap.FrameTopLeftOuter;
        Buffer.Canvas.Brush.Style:=bsClear;
        Buffer.Canvas.Rectangle(R);

        R := Rect(0, Self.Height - (ControlRect.Bottom - ListRect.Bottom), Self.Width, Self.Height);
        with Buffer.Canvas do
        begin
          Brush.Style := bsSolid;
          Brush.Color := TColorizerLocalSettings.ColorMap.MenuColor;
          FillRect(R);
        end;
        R := Rect(0, 0, Buffer.Width, Buffer.Height);
        R.Bottom := ListRect.Top - ControlRect.Top;
      end
      else
      begin
        Buffer.Canvas.Brush.Style:=bsSolid;
        Buffer.Canvas.Pen.Color:=TColorizerLocalSettings.ColorMap.FrameTopLeftOuter;
        Buffer.Canvas.Brush.Color:=TColorizerLocalSettings.ColorMap.MenuColor;
        Buffer.Canvas.Rectangle(R);
      end;

      // draw button in buffer
      if Self.Style <> csSimple then
      begin
        R:=GetButtonRect;
        Buffer.Canvas.Brush.Style:=bsSolid;
        Buffer.Canvas.Pen.Color:=TColorizerLocalSettings.ColorMap.FrameTopLeftOuter;
        Buffer.Canvas.Brush.Color:=TColorizerLocalSettings.ColorMap.MenuColor;
        Buffer.Canvas.Rectangle(R);

        Buffer.Canvas.Pen.Color:=TColorizerLocalSettings.ColorMap.FontColor;
        DrawArrow(Buffer.Canvas,TScrollDirection.sdDown, Point( R.Left + ((R.Right - R.Left) Div 2)-4 , R.Top + ((R.Bottom - R.Top) Div 2) - 2) ,4);
      end;

      if (SendMessage(Self.Handle, CB_GETCURSEL, 0, 0) >= 0) and (FEditHandle = 0) then
      begin
        R := Self.ClientRect;
        InflateRect(R, -3, -3);
        R.Right := GetButtonRect.Left - 2;
        ExcludeClipRect(Canvas.Handle, R.Left, R.Top, R.Right, R.Bottom);
      end
      else
      if FEditHandle <> 0 then
      begin
        GetWindowRect(Self.Handle, R);
        GetWindowRect(FEditHandle, EditRect);
        OffsetRect(EditRect, -R.Left, -R.Top);
        with EditRect do
          ExcludeClipRect(Canvas.Handle, Left, Top, Right, Bottom);
      end;

      Canvas.Draw(0, 0, Buffer);
    finally
      Buffer.Free;
    end;
  end;

var
  LRect: TRect;
  LCanvas: TCanvas;
  lpPaint : TPaintStruct;
  SavedDC: Integer;
  DC: HDC;
  LComboBoxInfo: TComboBoxInfo;
  LParentForm  : TCustomForm;
begin
  if Assigned(TColorizerLocalSettings.Settings) and TColorizerLocalSettings.Settings.Enabled and not (csDesigning in Self.ComponentState) then
  begin
    LParentForm:= GetParentForm(Self);
    if not (Assigned(LParentForm) and Assigned(TColorizerLocalSettings.HookedWindows) and (TColorizerLocalSettings.HookedWindows.IndexOf(LParentForm.ClassName)>=0)) then
    begin
      Trampoline_CustomComboBox_WMPaint(Self, Message);
      exit;
    end;

    FillChar(LComboBoxInfo, Sizeof(LComboBoxInfo), 0);
    GetComboBoxInfo(Self.Handle, LComboBoxInfo);
    FListHandle:= LComboBoxInfo.hwndList;
    FEditHandle:= LComboBoxInfo.hwndItem;

    DC := TMessage(Message).WParam;
    LCanvas := TCanvas.Create;
    try
      if DC = 0 then
        LCanvas.Handle := BeginPaint(Self.Handle, lpPaint)
      else
        LCanvas.Handle := DC;

      SavedDC := SaveDC(LCanvas.Handle);
      try
        PaintBorder(LCanvas);
      finally
        RestoreDC(LCanvas.Handle, SavedDC);
      end;

      if (Self.Style <> csSimple) and (FEditHandle = 0) then
      begin
        LRect := Self.ClientRect;
        InflateRect(LRect, -3, -3);
        if Self.BiDiMode <> bdRightToLeft then
          LRect.Right := GetButtonRect.Left - 1
        else
          LRect.Left := GetButtonRect.Right + 1;
        SavedDC := SaveDC(LCanvas.Handle);
        try
          IntersectClipRect(LCanvas.Handle, LRect.Left, LRect.Top, LRect.Right, LRect.Bottom);
          DrawItem(LCanvas, Self.ItemIndex, LRect, Self.Focused);
        finally
          RestoreDC(LCanvas.Handle, SavedDC);
        end;
      end;

    finally
      LCanvas.Handle := 0;
      LCanvas.Free;
      if DC = 0 then
        EndPaint(Self.Handle, lpPaint);
    end;
  end
  else
     Trampoline_CustomComboBox_WMPaint(Self, Message);
end;

//Don't use this hook, instead check for another workaround
//function CustomGetStyle(Self: TPopupActionBar) : TActionBarStyle;
//begin
//  if Assigned(TColorizerLocalSettings.Settings) and TColorizerLocalSettings.Settings.Enabled and  Assigned(TColorizerLocalSettings.ActionBarStyle) then
//    Exit(TColorizerLocalSettings.ActionBarStyle)
//  else
//   Exit(Trampoline_TPopupActionBar_GetStyle(Self));
//end;


//hook for TSplitter
procedure Detour_TSplitter_Paint(Self : TSplitterClass);
var
  R: TRect;
  LParentForm : TCustomForm;
begin
  if Assigned(TColorizerLocalSettings.Settings) and TColorizerLocalSettings.Settings.Enabled and not (csDesigning in Self.ComponentState) then
  begin

    LParentForm:= GetParentForm(Self);
    if not (Assigned(LParentForm) and Assigned(TColorizerLocalSettings.HookedWindows) and (TColorizerLocalSettings.HookedWindows.IndexOf(LParentForm.ClassName)>=0)) then
    begin
      Trampoline_TSplitter_Paint(Self);
      exit;
    end;

    R := Self.ClientRect;
    Self.Canvas.Brush.Color := TColorizerLocalSettings.ColorMap.Color;
    Self.Canvas.FillRect(Self.ClientRect);

    if Assigned(Self.OnPaint) then Self.OnPaint(Self);
  end
  else
    Trampoline_TSplitter_Paint(Self);
end;


//Draw the bottom right corner when both scrollbars are active
procedure DrawNCBorder(Self : TWinControl; EraseLRCorner: Boolean);
var
  DC      : HDC;
  EmptyRect, DrawRect: TRect;
  ScrollHeight, ScrollWidth    : Integer;
  ControlStyle   : Integer;
  LCanvas : TCanvas;
begin

  if Assigned(TColorizerLocalSettings.Settings) and (TColorizerLocalSettings.Settings.Enabled) and  Assigned(TColorizerLocalSettings.ColorMap) and
     (Assigned(TColorizerLocalSettings.HookedScrollBars) and (TColorizerLocalSettings.HookedScrollBars.IndexOf(Self.ClassName)>=0)) then
  begin

      GetWindowRect(Self.Handle, DrawRect);
      OffsetRect(DrawRect, -DrawRect.Left, -DrawRect.Top);
      DC := GetWindowDC(Self.Handle);
      try
        EmptyRect := DrawRect;
        if EraseLRCorner then
        begin
          ControlStyle := GetWindowLong(Self.Handle, GWL_STYLE);
          if ((ControlStyle and WS_HSCROLL) <> 0) and ((ControlStyle and WS_VSCROLL) <> 0) then
          begin
            ScrollWidth := GetSystemMetrics(SM_CXVSCROLL);
            ScrollHeight := GetSystemMetrics(SM_CYHSCROLL);
            InflateRect(EmptyRect, -1, -1);
            with EmptyRect do
              if Self.UseRightToLeftScrollBar then
                EmptyRect := Rect(Left, Bottom - ScrollHeight, Left + ScrollWidth, Bottom)
              else
                EmptyRect := Rect(Right - ScrollWidth, Bottom - ScrollHeight, Right, Bottom);

            LCanvas:=TCanvas.Create;
            try
              LCanvas.Handle:=DC;
              LCanvas.Brush.Color:=TColorizerLocalSettings.ColorMap.Color;
              LCanvas.FillRect(EmptyRect);
            finally
              LCanvas.Handle:=0;
              LCanvas.Free;
            end;
          end;
        end;

        with DrawRect do
          ExcludeClipRect(DC, Left + 2, Top + 2, Right - 2, Bottom - 2);
        LCanvas:=TCanvas.Create;
        try
          LCanvas.Handle:=DC;
          LCanvas.Brush.Color:=TColorizerLocalSettings.ColorMap.FrameTopLeftOuter;
          LCanvas.FillRect(DrawRect);
        finally
          LCanvas.Handle:=0;
          LCanvas.Free;
        end;
      finally
        ReleaseDC(Self.Handle, DC);
      end;
  end;
end;

//hook for NCPaint of TCustomPanel
procedure Detour_TWinControl_WMNCPaint(Self: TWinControlClass; var Message: TWMNCPaint);
const
  InnerStyles: array[TBevelCut] of Integer = (0, BDR_SUNKENINNER, BDR_RAISEDINNER, 0);
  OuterStyles: array[TBevelCut] of Integer = (0, BDR_SUNKENOUTER, BDR_RAISEDOUTER, 0);
  EdgeStyles: array[TBevelKind] of Integer = (0, 0, BF_SOFT, BF_FLAT);
  Ctl3DStyles: array[Boolean] of Integer = (BF_MONO, 0);
var
  LParentForm : TCustomForm;
  LCanvas     : TCanvas;
  ControlStyle: Longint;
  DC: HDC;
  LClientRect, LWindowRect, LRect: TRect;
  LEdge: Integer;
begin
  if ((TWinControl(Self) is TCustomPanel)) and Assigned(TColorizerLocalSettings.Settings) and TColorizerLocalSettings.Settings.Enabled and not (csDesigning in Self.ComponentState) then
  begin

    LParentForm:= GetParentForm(Self);
    if not (Assigned(LParentForm) and Assigned(TColorizerLocalSettings.HookedWindows) and (TColorizerLocalSettings.HookedWindows.IndexOf(LParentForm.ClassName)>=0)) then
    begin
      //AddLog('CustomWinControl_WMNCPaint Ignored (1)', Self.ClassName);
      Trampoline_TWinControl_WMNCPaint(Self, Message);
      exit;
    end;

    if (Self.BevelKind <> bkNone) or (Self.BorderWidth > 0) then
    begin
      //AddLog('CustomWinControl_WMNCPaint', Self.ClassName);
      DC := GetWindowDC(Self.Handle);
      try
        Windows.GetClientRect(Self.Handle, LClientRect);
        GetWindowRect(Self.Handle, LWindowRect);
        MapWindowPoints(0, Self.Handle, LWindowRect, 2);
        OffsetRect(LClientRect, -LWindowRect.Left, -LWindowRect.Top);
        ExcludeClipRect(DC, LClientRect.Left, LClientRect.Top, LClientRect.Right, LClientRect.Bottom);

        LRect := LWindowRect;
        InflateRect(LClientRect, Self.BorderWidth, Self.BorderWidth);
        LWindowRect := LClientRect;
        with LWindowRect do
        begin
          ControlStyle := GetWindowLong(Self.Handle, GWL_STYLE);
          if (ControlStyle and WS_VSCROLL) <> 0 then
            if Self.UseRightToLeftScrollBar then
              Dec(Left, GetSystemMetrics(SM_CYVSCROLL))
            else
              Inc(Right, GetSystemMetrics(SM_CYVSCROLL));
          if (ControlStyle and WS_HSCROLL) <> 0 then
            Inc(Bottom, GetSystemMetrics(SM_CXHSCROLL));
        end;

        if Self.BevelKind <> bkNone then
        begin
          LEdge := 0;
          if Self.BevelInner <> bvNone then Inc(LEdge, Self.BevelWidth);
          if Self.BevelOuter <> bvNone then Inc(LEdge, Self.BevelWidth);
          with LWindowRect do
          begin
            if TBevelEdge.beLeft in Self.BevelEdges then Dec(Left, LEdge);
            if TBevelEdge.beTop in Self.BevelEdges then Dec(Top, LEdge);
            if TBevelEdge.beRight in Self.BevelEdges then Inc(Right, LEdge);
            if TBevelEdge.beBottom in Self.BevelEdges then Inc(Bottom, LEdge);
          end;

          LCanvas:=TCanvas.Create;
          try
            LCanvas.Handle:=DC;
            LCanvas.Brush.Color:=TColorizerLocalSettings.ColorMap.Color;
            LCanvas.Pen.Color  :=TColorizerLocalSettings.ColorMap.FrameTopLeftOuter;
            LCanvas.Rectangle(LWindowRect);
          finally
            LCanvas.Handle:=0;
            LCanvas.Free;
          end;
        end;
        IntersectClipRect(DC, LWindowRect.Left, LWindowRect.Top, LWindowRect.Right, LWindowRect.Bottom);
        LWindowRect := LRect;

        if Message.RGN = 1 then
          OffsetRect(LWindowRect, -LWindowRect.Left, -LWindowRect.Top)
        else
        begin
          GetRgnBox(Message.RGN, LClientRect);
          MapWindowPoints(0, Self.Handle, LClientRect, 2);
          IntersectRect(LWindowRect, LWindowRect, LClientRect);
          OffsetRect(LWindowRect, -LRect.Left, -LRect.Top);
        end;

        LCanvas:=TCanvas.Create;
        try
          LCanvas.Handle:=DC;
          LCanvas.Brush.Color:=TColorizerLocalSettings.ColorMap.Color;
          LCanvas.FillRect(LWindowRect);
        finally
          LCanvas.Handle:=0;
          LCanvas.Free;
        end;

      finally
        ReleaseDC(Self.Handle, DC);
      end;
    end;

   Self.DefaultHandler(Self);

   if ThemeControl(Self) and (csNeedsBorderPaint in Self.ControlStyle) then
   begin
      //AddLog('CustomWinControl_WMNCPaint DrawNCBorder', Self.ClassName);
      DrawNCBorder(Self, False);
   end;
  end
  else
  begin
    //AddLog('CustomWinControl_WMNCPaint Ignored (2)', Self.ClassName);
    Trampoline_TWinControl_WMNCPaint(Self, Message);
  end;
end;


//Hook for TCustomPanel, draw flat border.
procedure Detour_TCustomPanel_Paint(Self : TCustomPanelClass);
const
  Alignments: array[TAlignment] of Longint = (DT_LEFT, DT_RIGHT, DT_CENTER);
  VerticalAlignments: array[TVerticalAlignment] of Longint = (DT_TOP, DT_BOTTOM, DT_VCENTER);
var
  TopColor, BottomColor: TColor;
  LColor, LTopColor, LBottomColor: TColor;
  LBiDiModeFlags: Longint;
  LParentForm : TCustomForm;
  LRect: TRect;

  procedure AdjustColors(Bevel: TPanelBevel);
  begin
    TopColor := LTopColor;
    if Bevel = bvLowered then
      TopColor := LBottomColor;
    BottomColor := LBottomColor;
    if Bevel = bvLowered then
      BottomColor := LTopColor;
  end;

begin
  if Assigned(TColorizerLocalSettings.Settings) and TColorizerLocalSettings.Settings.Enabled and not (csDesigning in Self.ComponentState) then
  begin

    LParentForm:= GetParentForm(Self);
    if not (Assigned(LParentForm) and Assigned(TColorizerLocalSettings.HookedWindows) and (TColorizerLocalSettings.HookedWindows.IndexOf(LParentForm.ClassName)>=0)) then
    begin
      //AddLog('CustomPanelPaint Ignored', LParentForm.ClassName);
      Trampoline_TCustomPanel_Paint(Self);
      exit;
    end;

    LRect := Self.GetClientRect;

    LColor       := TColorizerLocalSettings.ColorMap.Color;
    LTopColor    := TColorizerLocalSettings.ColorMap.FrameTopLeftOuter;
    LBottomColor := TColorizerLocalSettings.ColorMap.FrameTopLeftOuter;

    if Self.BevelOuter <> bvNone then
    begin
      AdjustColors(Self.BevelOuter);
      Frame3D(Self.Canvas, LRect, TopColor, BottomColor, Self.BevelWidth);
    end;

    if (csParentBackground in Self.ControlStyle) then
      Frame3D(Self.Canvas, LRect, LColor, LColor, Self.BorderWidth)
    else
      InflateRect(LRect, -Integer(Self.BorderWidth), -Integer(Self.BorderWidth));

    if Self.BevelInner <> bvNone then
    begin
      AdjustColors(Self.BevelInner);
      Frame3D(Self.Canvas, LRect, TopColor, BottomColor, Self.BevelWidth);
    end;

    with Self.Canvas do
    begin
      if not Self.ParentBackground then
      begin
        Brush.Color := LColor;
        FillRect(LRect);
      end;

      if Self.ShowCaption and (Self.Caption <> '') then
      begin
        Brush.Style := bsClear;
        Font := Self.Font;
        LBiDiModeFlags := DT_EXPANDTABS or DT_SINGLELINE or
          VerticalAlignments[Self.VerticalAlignment] or Alignments[Self.Alignment];
        LBiDiModeFlags := Self.DrawTextBiDiModeFlags(LBiDiModeFlags);
        DrawText(Handle, Self.Caption, -1, LRect, LBiDiModeFlags);
      end;
    end;

  end
  else
   Trampoline_TCustomPanel_Paint(Self);
end;


{ TCustomStatusBarHelper }

procedure TCustomStatusBarHelper.DoUpdatePanels(UpdateRects,
  UpdateText: Boolean);
begin
  Self.UpdatePanels(UpdateRects, UpdateText);
end;

function TCustomStatusBarHelper.GetCanvasRW: TCanvas;
begin
 Result:= Self.FCanvas;
end;

procedure TCustomStatusBarHelper.SetCanvasRW(const Value: TCanvas);
begin
 Self.FCanvas:= Value;
end;

function TCustomStatusBarHelper.WMPaintAddress: Pointer;
var
  MethodAddr: procedure(var Message: TWMPaint) of object;
begin
  MethodAddr := Self.WMPaint;
  Result     := TMethod(MethodAddr).Code;
end;

{ TCustomComboBoxBarHelper }

function TCustomComboBoxBarHelper.WMPaintAddress: Pointer;
var
  MethodAddr: procedure(var Message: TWMPaint) of object;
begin
  MethodAddr := Self.WMPaint;
  Result     := TMethod(MethodAddr).Code;
end;

function TWinControlHelper.WMNCPaintAddress : Pointer;
var
  MethodAddr: procedure(var Message: TWMNCPaint) of object;
begin
  MethodAddr := Self.WMNCPaint;
  Result     := TMethod(MethodAddr).Code;
end;

{ TCustomFormHelper }

function TCustomFormHelper.SetVisibleAddress: Pointer;
var
  MethodAddr: procedure(Value: Boolean) of object;
begin
  MethodAddr := Self.SetVisible;
  Result     := TMethod(MethodAddr).Code;
end;

{ TTabSetHelper }

function TTabSetHelper.DoModernPaintingAddress: Pointer;
var
  MethodAddr: procedure of object;
begin
  MethodAddr := Self.DoModernPainting;
  Result     := TMethod(MethodAddr).Code;
end;

function TTabSetHelper.GetEdgeWidth: Integer;
begin
 Result:=Self.FEdgeWidth;
end;

function TTabSetHelper.GetMemBitmap: TBitmap;
begin
 Result:=Self.FMemBitmap;
end;

function TTabSetHelper.GetTabPositions: TList;
begin
  Result:=Self.FTabPositions;
end;

{ TCustomListViewHelper }

function TCustomListViewHelper.GetHeaderHandle: HWND;
begin
  Result:=Self.FHeaderHandle;
end;

function TCustomListViewHelper.HeaderWndProcAddress: Pointer;
var
  MethodAddr: procedure(var Message: TMessage) of object;
begin
  MethodAddr := Self.HeaderWndProc;
  Result     := TMethod(MethodAddr).Code;
end;

{ TCategoryButtonsHelper }

function TCategoryButtonsHelper.DrawCategoryAddress: Pointer;
var
  MethodAddr: procedure(const Category: TButtonCategory; const Canvas: TCanvas; StartingPos: Integer) of object;
begin
  MethodAddr := Self.DrawCategory;
  Result     := TMethod(MethodAddr).Code;
end;

procedure TCategoryButtonsHelper.GetCategoryBoundsHelper(
  const Category: TButtonCategory; const StartingPos: Integer;
  var CategoryBounds, ButtonBounds: TRect);
begin
 Self.GetCategoryBounds(Category, StartingPos, CategoryBounds, ButtonBounds);
end;

procedure TCategoryButtonsHelper.AdjustCategoryBoundsHelper(const Category: TButtonCategory; var CategoryBounds: TRect; IgnoreButtonFlow: Boolean = False);
begin
 Self.AdjustCategoryBounds(Category, CategoryBounds, IgnoreButtonFlow);
end;

function  TCategoryButtonsHelper.GetChevronBoundsHelper(const CategoryBounds: TRect): TRect;
begin
 Result := Self.GetChevronBounds(CategoryBounds);
end;

function  TCategoryButtonsHelper.FSideBufferSizeHelper : Integer;
begin
 Result:= Self.FSideBufferSize;
end;

function  TCategoryButtonsHelper.FHotButtonHelper: TButtonItem;
begin
 Result:= Self.FHotButton;
end;

function  TCategoryButtonsHelper.FDownButtonHelper: TButtonItem;
begin
 Result:= Self.FDownButton;
end;

type
 TCategoryButtonsClass = class(TCategoryButtons);

procedure Detour_TCategoryButtons_DrawCategory(Self :TCategoryButtonsClass; const Category: TButtonCategory; const Canvas: TCanvas; StartingPos: Integer);
const
  cDropDownSize = 13;

  procedure DrawDropDownButton(X, Y: Integer; Collapsed: Boolean);
  const
    ChevronDirection: array[Boolean] of TScrollDirection = (sdDown, sdRight);
    ChevronXPosAdjust: array[Boolean] of Integer = (2, 0);
    ChevronYPosAdjust: array[Boolean] of Integer = (1, 3);

    procedure DrawPlusMinus;
    var
      Width, Height: Integer;
    begin
      Width := 9;
      Height := Width;
      Inc(X, 2);
      Inc(Y, 2);

      Canvas.Pen.Color   := TColorizerLocalSettings.ColorMap.FontColor;
      Canvas.Brush.Color := TColorizerLocalSettings.ColorMap.Color;
      Canvas.Rectangle(X, Y, X + Width, Y + Height);
      Canvas.Pen.Color   := TColorizerLocalSettings.ColorMap.FontColor;

      Canvas.MoveTo(X + 2, Y + Width div 2);
      Canvas.LineTo(X + Width - 2, Y + Width div 2);

      if Collapsed then
      begin
        Canvas.MoveTo(X + Width div 2, Y + 2);
        Canvas.LineTo(X + Width div 2, Y + Width - 2);
      end;
    end;

  begin
      DrawPlusMinus;
  end;
var
  I: Integer;
  ButtonTop, ButtonLeft, ButtonRight: Integer;
  ButtonRect: TRect;
  ActualWidth: Integer;
  ButtonStart: Integer;
  ButtonBottom: Integer;
  CapWidth: Integer;
  VerticalCaption: Boolean;
  CapLeft: Integer;
  DrawState: TButtonDrawState;
  Button: TButtonItem;
  CatHeight: Integer;
  CategoryBounds, CategoryFrameBounds,
  ButtonBounds, ChevronBounds: TRect;
  GradientColor, SourceColor, TempColor: TColor;
  Caption: string;
  CaptionRect: TRect;
  CategoryRealBounds: TRect;

  LParentForm : TCustomForm;
begin
  LParentForm:=GetParentForm(Self);

  if Assigned(TColorizerLocalSettings.Settings) and (TColorizerLocalSettings.Settings.Enabled)
    and Assigned(LParentForm) and  (TColorizerLocalSettings.HookedWindows.IndexOf(LParentForm.ClassName)>=0) {SameText(Self.ClassName, 'TIDECategoryButtons') and}
    and Assigned(TColorizerLocalSettings.ColorMap) then
  begin
    Self.GetCategoryBoundsHelper(Category, StartingPos, CategoryBounds, ButtonBounds);

    if (Self.SelectedItem = Category) and (Self.SelectedButtonColor <> clNone) then
      SourceColor := TColorizerLocalSettings.ColorMap.SelectedColor//Self.SelectedButtonColor
    else if Category.Color <> clNone then
      SourceColor := TColorizerLocalSettings.ColorMap.Color//Category.Color
    else
      SourceColor := TColorizerLocalSettings.ColorMap.MenuColor;//Self.Color;

    CategoryFrameBounds := CategoryBounds;
    Self.AdjustCategoryBoundsHelper(Category, CategoryFrameBounds);
    if boCaptionOnlyBorder in Self.ButtonOptions then
      CategoryRealBounds := CategoryFrameBounds
    else
      CategoryRealBounds := CategoryBounds;

    if (Self.SelectedItem <> Category) and (boGradientFill in Self.ButtonOptions) then
    begin
      if Category.GradientColor <> clNone then
        GradientColor := TColorizerLocalSettings.ColorMap.MenuColor//Category.GradientColor
      else
        GradientColor := TColorizerLocalSettings.ColorMap.MenuColor;//Self.Color;

      GradientFillCanvas(Canvas, SourceColor, GradientColor, CategoryRealBounds, Self.GradientDirection);
    end
    else
    begin
      Canvas.Brush.Color := SourceColor;
      Canvas.FillRect(CategoryRealBounds)
    end;

    with CategoryRealBounds do
    begin
      Right := Right - 1;
      TempColor := TColorizerLocalSettings.ColorMap.MenuColor;//Self.Color;

      Canvas.Pixels[Left, Top] := TempColor;
      Canvas.Pixels[Left+1, Top] := TempColor;
      Canvas.Pixels[Left, Top+1] := TempColor;

      Canvas.Pixels[Left, Bottom] := TempColor;
      Canvas.Pixels[Left+1, Bottom] := TempColor;
      Canvas.Pixels[Left, Bottom-1] := TempColor;

      if Self.BackgroundGradientColor <> clNone then
        TempColor := Self.BackgroundGradientColor;

      Canvas.Pixels[Right, Top] := TempColor;
      Canvas.Pixels[Right-1, Top] := TempColor;
      Canvas.Pixels[Right, Top+1] := TempColor;

      Canvas.Pixels[Right, Bottom] := TempColor;
      Canvas.Pixels[Right-1, Bottom] := TempColor;
      Canvas.Pixels[Right, Bottom-1] := TempColor;

      Canvas.Pen.Color := TColorizerLocalSettings.ColorMap.FrameTopLeftOuter;

      Canvas.Polyline([Point(Left + 2, Top),
        Point(Right - 2, Top), { Top line }
        Point(Right, Top + 2), { Top right curve }
        Point(Right, Bottom - 2), { Right side line }
        Point(Right - 2, Bottom), { Bottom right curve }
        Point(Left + 2, Bottom), { Bottom line }
        Point(Left, Bottom - 2), { Bottom left curve }
        Point(Left, Top + 2), { Left side line }
        Point(Left + 2, Top)]); { Top left curve }
    end;

    if ((Category.Collapsed) and (Self.SelectedItem <> nil) and
       (Self.CurrentCategory = Category)) or (Self.SelectedItem = Category) then
    begin
      Canvas.Brush.Color := TColorizerLocalSettings.ColorMap.FrameTopLeftOuter;//GetShadowColor(SourceColor, -75);
      with CategoryFrameBounds do
        Canvas.FrameRect(Rect(Left + 1, Top + 1, Right, Bottom));
    end;

    ChevronBounds := Self.GetChevronBoundsHelper(CategoryRealBounds);

    if (Category.Items <> nil) and (Category.Items.Count > 0) then
      DrawDropDownButton(ChevronBounds.Left, ChevronBounds.Top,
        Category.Collapsed);

    VerticalCaption := Self.HasVerticalCaption(Category);

    Caption := Category.Caption;

    if (boBoldCaptions in Self.ButtonOptions) then
      Canvas.Font.Style := Canvas.Font.Style + [fsBold];

    CapWidth := Canvas.TextWidth(Caption);
    if VerticalCaption then
      CatHeight := CategoryBounds.Bottom - CategoryBounds.Top - 3 - cDropDownSize
    else
      CatHeight := CategoryBounds.Right - CategoryBounds.Left - 2 - cDropDownSize;

    CapLeft := (CatHeight - CapWidth) div 2;
    if CapLeft < 2 then
      CapLeft := 2;


    Canvas.Brush.Style := bsClear;
    Canvas.Font.Color := TColorizerLocalSettings.ColorMap.FontColor;

    if not VerticalCaption then
    begin
      CaptionRect.Left := CategoryBounds.Left + 4 + cDropDownSize;
      CaptionRect.Top := CategoryBounds.Top + 1;
    end
    else
    begin
      CaptionRect.Left := CategoryBounds.Left + 1;
      CaptionRect.Top := CategoryBounds.Bottom - CapLeft;
      Canvas.Font.Orientation := 900;
    end;

    CaptionRect.Right := CaptionRect.Left + CatHeight;
    CaptionRect.Bottom := CaptionRect.Top + Canvas.TextHeight(Caption);
    Canvas.TextRect(CaptionRect, Caption, [tfNoClip, tfEndEllipsis]);

    if (boBoldCaptions in Self.ButtonOptions) then
      Canvas.Font.Style := Canvas.Font.Style - [fsBold];

    Canvas.Brush.Style := bsSolid;
    Canvas.Font.Orientation := 0;

    if not Category.Collapsed and (Category.Items <> nil) then
    begin

      if (Self.ButtonFlow = cbfVertical) and (boFullSize in Self.ButtonOptions) then
        ActualWidth := Self.ClientWidth - Self.FSideBufferSizeHelper
      else
        ActualWidth := Self.ButtonWidth;

      ButtonStart := ButtonBounds.Left;
      ButtonTop := ButtonBounds.Top;
      ButtonLeft := ButtonStart;
      for I := 0 to Category.Items.Count - 1 do
      begin
        if (Self.ButtonFlow = cbfVertical) and (ButtonTop > Self.ClientHeight) then
          Break;

        ButtonBottom := ButtonTop + Self.ButtonHeight;
        ButtonRight := ButtonLeft + ActualWidth;
        if VerticalCaption and not (boCaptionOnlyBorder in Self.ButtonOptions) then
          Dec(ButtonRight, 3);
        if (ButtonBottom >= 0) and (ButtonRight >= 0) then
        begin
          ButtonRect := Rect(ButtonLeft, ButtonTop, ButtonRight, ButtonBottom);

          Button := Category.Items[I];
          DrawState := [];
          if Button = Self.FHotButtonHelper then
          begin
            Include(DrawState, bdsHot);
            if Button = Self.FDownButtonHelper then
              Include(DrawState, bdsDown);
          end;
          if Button = Self.SelectedItem then
            Include(DrawState, bdsSelected)
          else if (Button = Self.FocusedItem) and Self.Focused and (Self.FDownButtonHelper = nil) then
            Include(DrawState, bdsFocused);

//          if Button = FInsertTop then
//            Include(DrawState, bdsInsertTop)
//          else if Button = FInsertBottom then
//            Include(DrawState, bdsInsertBottom)
//          else if Button = FInsertRight then
//            Include(DrawState, bdsInsertRight)
//          else if Button = FInsertLeft then
//            Include(DrawState, bdsInsertLeft);

          Self.DrawButton(Button, Canvas, ButtonRect, DrawState);
        end;
        Inc(ButtonLeft, ActualWidth);

        if (ButtonLeft + ActualWidth) > ButtonBounds.Right then
        begin
          ButtonLeft := ButtonStart;
          Inc(ButtonTop, Self.ButtonHeight);
        end;
      end;
    end;

  end
  else
  Trampoline_TCategoryButtons_DrawCategory(Self, Category, Canvas, StartingPos);
end;

{$IF CompilerVersion<27} //XE6

procedure Bitmap2GrayScale(const BitMap: TBitmap);
type
  TRGBArray = array[0..32767] of TRGBTriple;
  PRGBArray = ^TRGBArray;
var
  x, y, Gray: Integer;
  Row       : PRGBArray;
begin
  BitMap.PixelFormat := pf24Bit;
  for y := 0 to BitMap.Height - 1 do
  begin
    Row := BitMap.ScanLine[y];
    for x := 0 to BitMap.Width - 1 do
    begin
      Gray             := (Row[x].rgbtRed + Row[x].rgbtGreen + Row[x].rgbtBlue) div 3;
      Row[x].rgbtRed   := Gray;
      Row[x].rgbtGreen := Gray;
      Row[x].rgbtBlue  := Gray;
    end;
  end;
end;

function GetRGBColor(Value: TColor): DWORD;
begin
  Result := ColorToRGB(Value);
  case Result of
    clNone    :  Result := CLR_NONE;
    clDefault :  Result := CLR_DEFAULT;
  end;
end;


type
  TCustomImageListClass = class(TCustomImageList);

procedure Detour_TCustomImageList_DoDraw(Self: TObject; Index: Integer; Canvas: TCanvas; X, Y: Integer; Style: Cardinal; Enabled: Boolean);
var
  MaskBitMap : TBitmap;
  GrayBitMap : TBitmap;
  LImageList : TCustomImageListClass;
begin
  if Assigned(TColorizerLocalSettings.Settings) and TColorizerLocalSettings.Settings.Enabled then
    begin
      LImageList:=TCustomImageListClass(Self);
      if not LImageList.HandleAllocated then Exit;
      if Enabled then
        ImageList_DrawEx(LImageList.Handle, Index, Canvas.Handle, X, Y, 0, 0, GetRGBColor(LImageList.BkColor), GetRGBColor(LImageList.BlendColor), Style)
      else
      begin
        GrayBitMap := TBitmap.Create;
        MaskBitMap := TBitmap.Create;
        try
          GrayBitMap.SetSize(LImageList.Width, LImageList.Height);
          MaskBitMap.SetSize(LImageList.Width, LImageList.Height);
          LImageList.GetImages(Index, GrayBitMap, MaskBitMap);
          Bitmap2GrayScale(GrayBitMap);
          BitBlt(Canvas.Handle, X, Y, LImageList.Width, LImageList.Height, MaskBitMap.Canvas.Handle, 0, 0, SRCERASE);
          BitBlt(Canvas.Handle, X, Y, LImageList.Width, LImageList.Height, GrayBitMap.Canvas.Handle, 0, 0, SRCINVERT);
        finally
          GrayBitMap.Free;
          MaskBitMap.Free;
        end;
      end;
    end
  else
    TrampolineCustomImageList_DoDraw(Self, Index, Canvas, X, Y, Style, Enabled);
end;
{$IFEND}

//Retuns the current Gutter color , using the background of the current syntax highlighter
function GetGutterBkColor : TColor;
var
  ATheme : TIDETheme;
  sColor : string;
begin
  if FGutterBkColor<>clNone then
   Result:=FGutterBkColor
  else
  begin
    if Assigned(TColorizerLocalSettings.IDEData) then
    begin
      ImportDelphiIDEThemeFromReg(ATheme, TColorizerLocalSettings.IDEData.Version, False);
      sColor:=ATheme[LineNumber].BackgroundColorNew;
      try
        Result:=StringToColor(sColor);
      except
        if Assigned(TColorizerLocalSettings.ColorMap) then
         Result:=TColorizerLocalSettings.ColorMap.Color
        else
         Result:=clBtnFace;
      end;
      FGutterBkColor:=Result;
    end
    else
    if Assigned(TColorizerLocalSettings.ColorMap) then
      Result:=TColorizerLocalSettings.ColorMap.Color
    else
      Result:=clBtnFace
  end;
end;

procedure  Detour_TCanvas_Rectangle(Self: TCanvas; X1, Y1, X2, Y2: Integer);
begin
  //Self.Brush.Color:=clRed;
  Trampoline_TCanvas_Rectangle(Self, X1, Y1, X2, Y2);
end;


//Hook for paint the border of the TClosableTabScroller control
procedure  Detour_TCanvas_LineTo(Self: TCanvas;X, Y: Integer);
var
  sCaller : string;
  LHWND : HWND;
  LWinControl : TWinControl;
  ApplyHook : Boolean;
begin
   if Assigned(TColorizerLocalSettings.Settings) and TColorizerLocalSettings.Settings.Enabled and Assigned(TColorizerLocalSettings.ColorMap) and (Self.Pen.Color=clBtnFace) then
   begin
     sCaller  := ProcByLevel(1);
     ApplyHook:= (sCaller='');
     if not ApplyHook then
     begin
       LHWND :=  WindowFromDC(Self.Handle);
       LWinControl:=nil;
       if LHWND<>0 then
         LWinControl:=FindControl(LHWND);
       ApplyHook := (Assigned(LWinControl) and SameText(LWinControl.ClassName, 'TClosableTabScroller'));
     end;

     if ApplyHook then
      Self.Pen.Color :=TColorizerLocalSettings.ColorMap.Color;
   end;

  Trampoline_TCanvas_LineTo(Self, X, Y);
end;

//Hook for paint the gutter of the TEditControl and the background of the TGradientTabSet component
procedure  Detour_TCanvas_FillRect(Self: TCanvas;const Rect: TRect);
const
 sEditorControlSignature = 'EditorControl.TCustomEditControl.EVFillGutter';
 sGradientTabsSignature  = 'GDIPlus.GradientTabs.TGradientTabSet.DrawTabsToMemoryBitmap';
var
  sCaller : string;
//  LHWND : HWND;
//  LWinControl : TWinControl;
begin
   if Assigned(TColorizerLocalSettings.Settings) and TColorizerLocalSettings.Settings.Enabled and Assigned(TColorizerLocalSettings.ColorMap) and  (Self.Brush.Color=clBtnFace) then
   begin
     sCaller := ProcByLevel(1);
     if SameText(sCaller, sEditorControlSignature) then
        Self.Brush.Color:=GetGutterBkColor
     else
      if SameText(sCaller, sGradientTabsSignature) then
        Self.Brush.Color:=TColorizerLocalSettings.ColorMap.Color;
//      else
//      begin
//         LHWND :=  WindowFromDC(Self.Handle);
//         LWinControl:=nil;
//         if LHWND<>0 then
//           LWinControl:=FindControl(LHWND);
//         AddLog('CustomFillRect', sCaller);
//
//         if LWinControl<>nil then
//           AddLog('CustomFillRect', LWinControl.ClassName);
//
//         AddLog('CustomFillRect', '--------------');
//      end;
   end;
   Trampoline_TCanvas_FillRect(Self, Rect);
end;

//Hook for paint the header of the TVirtualStringTree component
{$IFDEF DELPHIXE2_UP}
function Detour_TUxThemeStyle_DrawElement(Self : TUxThemeStyle;DC: HDC; Details: TThemedElementDetails; const R: TRect; ClipRect:PRect = nil): Boolean;
const
  sTVirtualTreeColumnsSignature = 'IDEVirtualTrees.TVirtualTreeColumns.PaintHeader';
  HP_HEADERITEMRIGHT = 3;
var
  sCaller : string;
  LCanvas : TCanvas;
  SaveIndex: Integer;

//  LWinControl : TWinControl;
//  OrgHWND : HWND;
begin

   if Assigned(TColorizerLocalSettings.Settings) and TColorizerLocalSettings.Settings.Enabled and Assigned(TColorizerLocalSettings.ColorMap) and (Details.Element = teHeader) {and (Details.Part=HP_HEADERITEMRIGHT) } then
   begin

    sCaller := ProcByLevel(2);

//        LWinControl:=nil;
//        OrgHWND :=WindowFromDC(DC);
//        if OrgHWND<>0 then
//           LWinControl :=FindControl(OrgHWND);
//
//        if LWinControl<>nil then
//        begin
//        AddLog('CustomDrawElement' , LWinControl.ClassName);
//        end
//        else
//        AddLog('CustomDrawElement Ignored' ,sCaller);

    if SameText(sCaller, sTVirtualTreeColumnsSignature) then
    begin
       SaveIndex := SaveDC(DC);
       LCanvas:=TCanvas.Create;
       try
         LCanvas.Handle:=DC;
          GradientFillCanvas(LCanvas, TColorizerLocalSettings.ColorMap.Color, TColorizerLocalSettings.ColorMap.HighlightColor, R, gdVertical);
          LCanvas.Brush.Style:=TBrushStyle.bsClear;
          LCanvas.Pen.Color:=TColorizerLocalSettings.ColorMap.FrameTopLeftInner;
          LCanvas.Rectangle(R);
       finally
          LCanvas.Handle:=0;
          LCanvas.Free;
          RestoreDC(DC, SaveIndex);
       end;

       exit(True);
    end;
   end;
   Result:=Trampoline_TUxThemeStyle_DoDrawElement(Self, DC, Details, R, ClipRect);
end;
{$ELSE}
procedure Detour_TThemeServices_DrawElement(Self : TThemeServices;DC: HDC; Details: TThemedElementDetails; const R: TRect; ClipRect: TRect);
const
  sTVirtualTreeColumnsSignature = 'IDEVirtualTrees.TVirtualTreeColumns.PaintHeader';
  HP_HEADERITEMRIGHT = 3;
var
  sCaller : string;
  LCanvas : TCanvas;
  SaveIndex: Integer;
begin
   //TFile.AppendAllText('C:\Delphi\google-code\DITE\delphi-ide-theme-editor\IDE PlugIn\CustomDrawElementXE.txt', Format('%s',['CustomDrawElement']));
   if Assigned(TColorizerLocalSettings.Settings) and TColorizerLocalSettings.Settings.Enabled and Assigned(TColorizerLocalSettings.ColorMap) and (Details.Element = teHeader) {and (Details.Part=HP_HEADERITEMRIGHT) } then
   begin
    sCaller := ProcByLevel(2);
    if SameText(sCaller, sTVirtualTreeColumnsSignature) then
    begin
       SaveIndex := SaveDC(DC);
       LCanvas:=TCanvas.Create;
       try
         LCanvas.Handle:=DC;
          GradientFillCanvas(LCanvas, TColorizerLocalSettings.ColorMap.Color, TColorizerLocalSettings.ColorMap.HighlightColor, R, gdVertical);
          LCanvas.Brush.Style:=TBrushStyle.bsClear;
          LCanvas.Pen.Color:=TColorizerLocalSettings.ColorMap.FrameTopLeftInner;
          LCanvas.Rectangle(R);
       finally
          LCanvas.Handle:=0;
          LCanvas.Free;
          RestoreDC(DC, SaveIndex);
       end;
       Exit();
    end;
   end;
   Trampoline_TUxTheme_DrawElement(Self, DC, Details, R, ClipRect);
end;

function Detour_UxTheme_DrawBackground(hTheme: UxTheme.HTHEME; hdc: HDC; iPartId, iStateId: Integer; const pRect: TRect; pClipRect: PRECT): HRESULT; stdcall;
const
  sTVirtualTreeColumnsSignature = 'IDEVirtualTrees.TVirtualTreeColumns.PaintHeader';
var
  sCaller : string;
  LCanvas : TCanvas;
  SaveIndex: Integer;
begin
  if Assigned(TColorizerLocalSettings.Settings) and TColorizerLocalSettings.Settings.Enabled and Assigned(TColorizerLocalSettings.ColorMap) and  (iPartId=HP_HEADERITEM) then
  begin
    sCaller := ProcByLevel(2);
    if SameText(sCaller, sTVirtualTreeColumnsSignature) then
    begin
       SaveIndex := SaveDC(hdc);
       LCanvas:=TCanvas.Create;
       try
         LCanvas.Handle:=hdc;
          GradientFillCanvas(LCanvas, TColorizerLocalSettings.ColorMap.Color, TColorizerLocalSettings.ColorMap.HighlightColor, pRect, gdVertical);
          LCanvas.Brush.Style:=TBrushStyle.bsClear;
          LCanvas.Pen.Color:=TColorizerLocalSettings.ColorMap.FrameTopLeftInner;
          LCanvas.Rectangle(pRect);
       finally
          LCanvas.Handle:=0;
          LCanvas.Free;
          RestoreDC(hdc, SaveIndex);
       end;
       Exit(0);
    end;
  end;
  Result:=Trampoline_DrawThemeBackground(hTheme, hdc, iPartId, iStateId, pRect, pClipRect);
end;
{$ENDIF}

{$IFDEF DELPHIXE2_UP}
//Hook, for avoid apply a VCL Style to a TWinControl in desing time
function Detour_TStyleEngine_HandleMessage(Self: TStyleEngine; Control: TWinControl; var Message: TMessage; DefWndProc: TWndMethod): Boolean;
begin
  if Assigned(TColorizerLocalSettings.Settings) and TColorizerLocalSettings.Settings.Enabled and Assigned(TColorizerLocalSettings.Settings) and TColorizerLocalSettings.Settings.UseVCLStyles then
  begin
    Result:=False;
    if not Assigned(Control) then exit;
    if csDesigning in Control.ComponentState then  exit;
  end;
  Result:=Trampoline_TStyleEngine_HandleMessage(Self, Control, Message, DefWndProc);
end;
{$ENDIF}

//Hook for paint IDE TStatusBar
procedure Detour_TStatusBar_WMPaint(Self: TCustomStatusBarClass; var Message: TWMPaint);
var
  DC: HDC;
  LBuffer: TBitmap;
  LCanvas: TCanvas;
  LpPaint: TPaintStruct;
  LStyleServices : {$IFDEF DELPHIXE2_UP}  TCustomStyleServices {$ELSE}TThemeServices{$ENDIF};
  LParentForm : TCustomForm;

      procedure DrawControlText(Canvas: TCanvas; Details: TThemedElementDetails;
        const S: string; var R: TRect; Flags: Cardinal);
      var
        TextFormat: TTextFormatFlags;
      begin
        Canvas.Font := TWinControlClass(Self).Font;
        TextFormat := TTextFormatFlags(Flags);
        Canvas.Font.Color := TColorizerLocalSettings.ColorMap.FontColor;
        LStyleServices.DrawText(Canvas.Handle, Details, S, R, TextFormat, Canvas.Font.Color);
      end;

      procedure Paint(Canvas : TCanvas);
      const
        AlignStyles: array [TAlignment] of Integer = (DT_LEFT, DT_RIGHT, DT_CENTER);
      var
        R : TRect;
        Res, Count, I: Integer;
        Idx, Flags: Cardinal;
        Details: TThemedElementDetails;
        LText: string;
        Borders: array [0..2] of Integer;
        SaveCanvas: TCanvas;
      begin

        {$IFDEF DELPHIXE2_UP}
        LStyleServices:=StyleServices;
        {$ELSE}
        LStyleServices :=ThemeServices
        {$ENDIF};

        if not {$IFDEF DELPHIXE2_UP}LStyleServices.Available{$ELSE}LStyleServices.ThemesAvailable{$ENDIF}then
          Exit;

        Details := LStyleServices.GetElementDetails(tsStatusRoot);
        Canvas.Brush.Color := TColorizerLocalSettings.ColorMap.Color;
        Canvas.FillRect(Rect(0, 0, Self.Width, Self.Height));


        if SendMessage(Self.Handle, SB_ISSIMPLE, 0, 0) > 0 then
        begin
          R := Self.ClientRect;
          FillChar(Borders, SizeOf(Borders), 0);
          SendMessage(Self.Handle, SB_GETBORDERS, 0, LParam(@Borders));
          R.Left := Borders[0] + Borders[2];
          R.Top := Borders[1];
          R.Bottom := R.Bottom - Borders[1];
          R.Right := R.Right - Borders[2];

          Canvas.Brush.Color := TColorizerLocalSettings.ColorMap.Color;
          Canvas.FillRect(R);

          {TODO : Add gripper}

          Details := LStyleServices.GetElementDetails(tsPane);
          SetLength(LText, Word(SendMessage(Self.Handle, SB_GETTEXTLENGTH, 0, 0)));
          if Length(LText) > 0 then
          begin
           SendMessage(Self.Handle, SB_GETTEXT, 0, LParam(@LText[1]));
           Flags := Self.DrawTextBiDiModeFlags(DT_LEFT);
           DrawControlText(Canvas, Details, LText, R, Flags);
          end;
        end
        else
        begin
          Count := Self.Panels.Count;
          for I := 0 to Count - 1 do
          begin
            R := Rect(0, 0, 0, 0);
            SendMessage(Self.Handle, SB_GETRECT, I, LParam(@R));
            if IsRectEmpty(R) then
              Exit;

            Canvas.Brush.Color := TColorizerLocalSettings.ColorMap.MenuColor;
            //Canvas.Pen.Color   := TColorizerLocalSettings.ColorMap.FrameTopLeftOuter;
            //Canvas.Rectangle(R);
            Canvas.FillRect(R);

            Details := LStyleServices.GetElementDetails(tsPane);
            InflateRect(R, -1, -1);
            if Self is TCustomStatusBar then
              Flags := Self.DrawTextBiDiModeFlags(AlignStyles[TCustomStatusBar(Self).Panels[I].Alignment])
            else
              Flags := Self.DrawTextBiDiModeFlags(DT_LEFT);
            Idx := I;
            SetLength(LText, Word(SendMessage(Self.Handle, SB_GETTEXTLENGTH, Idx, 0)));
            if Length(LText) > 0 then
            begin
              Res := SendMessage(Self.Handle, SB_GETTEXT, Idx, LParam(@LText[1]));
              if (Res and SBT_OWNERDRAW = 0) then
                DrawControlText(Canvas, Details, LText, R, Flags)
              else
              if (Self is TCustomStatusBar) and Assigned(TCustomStatusBar(Self).OnDrawPanel) then
              begin
                SaveCanvas  := Self.Canvas;
                Self.CanvasRW := Canvas;
                try
                  Self.OnDrawPanel(TCustomStatusBar(Self), Self.Panels[I], R);
                finally
                  Self.CanvasRW := SaveCanvas;
                end;
              end;
            end
            else if (Self is TCustomStatusBar) then
             if (TCustomStatusBar(Self).Panels[I].Style <> psOwnerDraw) then
               DrawControlText(Canvas, Details, TCustomStatusBar(Self).Panels[I].Text, R, Flags)
             else
               if Assigned(TCustomStatusBar(Self).OnDrawPanel) then
               begin
                 SaveCanvas := TCustomStatusBar(Self).Canvas;
                 TCustomStatusBar(Self).CanvasRW := Canvas;
                 try
                   TCustomStatusBar(Self).OnDrawPanel(TCustomStatusBar(Self), TCustomStatusBar(Self).Panels[I], R);
                 finally
                   TCustomStatusBar(Self).CanvasRW := SaveCanvas;
                 end;
               end;
          end;
        end;

      end;

begin
    if (Assigned(TColorizerLocalSettings.Settings) and not TColorizerLocalSettings.Settings.Enabled) or (csDesigning in Self.ComponentState) or (not Assigned(TColorizerLocalSettings.ColorMap)) or (Assigned(TColorizerLocalSettings.Settings) and TColorizerLocalSettings.Settings.UseVCLStyles) then
    begin
     Trampoline_TCustomStatusBar_WMPAINT(Self, Message);
     exit;
    end;


    LParentForm:= GetParentForm(Self);
    if not (Assigned(LParentForm) and Assigned(TColorizerLocalSettings.HookedWindows) and (TColorizerLocalSettings.HookedWindows.IndexOf(LParentForm.ClassName)>=0)) then
    begin
      Trampoline_TCustomStatusBar_WMPAINT(Self, Message);
      exit;
    end;


    Self.DoUpdatePanels(False, True);

    DC := HDC(Message.DC);
    LCanvas := TCanvas.Create;
    try
        if DC <> 0 then
          LCanvas.Handle := DC
        else
        LCanvas.Handle := BeginPaint(Self.Handle, LpPaint);
        if (DC = 0) then
        begin
          LBuffer := TBitmap.Create;
          try
            LBuffer.SetSize(Self.Width, Self.Height);
            LCanvas.Brush.Color := TColorizerLocalSettings.ColorMap.Color;
            LCanvas.FillRect(Self.ClientRect);
            Paint(LBuffer.Canvas);

            if Self is TWinControl then
              TWinControlClass(Self).PaintControls(LBuffer.Canvas.Handle, nil);
            LCanvas.Draw(0, 0, LBuffer);
          finally
            LBuffer.Free;
          end;
        end;

      if DC = 0 then
        EndPaint(Self.Handle, LpPaint);
    finally
      LCanvas.Handle := 0;
      LCanvas.Free;
    end;

end;

procedure CropPNG(Source: TPngImage; Left, Top, Width, Height: Integer; out Target: TPngImage);

  function ColorToTriple(Color: TColor): TRGBTriple;
  begin
    Color := ColorToRGB(Color);
    Result.rgbtBlue := Color shr 16 and $FF;
    Result.rgbtGreen := Color shr 8 and $FF;
    Result.rgbtRed := Color and $FF;
  end;

var
   X, Y: Integer;
   LBitmap: TBitmap;
   LRGBLine: PRGBLine;
   AlphaLineA, AlphaLineB: PngImage.PByteArray;
begin
  if (Source.Width < (Left + Width)) or (Source.Height < (Top + Height)) then
    raise Exception.Create('Invalid position/size');

  LBitmap := TBitmap.Create;
  try
    LBitmap.Width := Width;
    LBitmap.Height := Height;
    LBitmap.PixelFormat := pf24bit;

    for Y := 0 to LBitmap.Height - 1 do
    begin
      LRGBLine := LBitmap.Scanline[Y];
      for X := 0 to LBitmap.Width - 1 do
        LRGBLine^[X] := ColorToTriple(Source.Pixels[Left + X, Top + Y]);
    end;

    Target := TPngImage.Create;
    Target.Assign(LBitmap);
  finally
    LBitmap.Free;
  end;

  if Source.Header.ColorType in [COLOR_GRAYSCALEALPHA, COLOR_RGBALPHA] then begin
    Target.CreateAlpha;
    for Y := 0 to Target.Height - 1 do begin
      AlphaLineA := Source.AlphaScanline[Top + Y];
      AlphaLineB := Target.AlphaScanline[Y];
      for X := 0 to Target.Width - 1 do
        AlphaLineB^[X] := AlphaLineA^[X + Left];
    end;
  end;
end;

//Hook for the docked IDE windows.
function Detour_TDockCaptionDrawer_DrawDockCaption(Self : TDockCaptionDrawerClass;const Canvas: TCanvas; CaptionRect: TRect; State: TParentFormState): TDockCaptionHitTest;
var
  LColorStart, LColorEnd : TColor;

  procedure DrawIcon;
  var
    FormBitmap: TBitmap;
    DestBitmap: TBitmap;
    ImageSize: Integer;
    X, Y: Integer;
  begin
    if (State.Icon <> nil) and (State.Icon.HandleAllocated) then
    begin
      if Self.DockCaptionOrientation = dcoHorizontal then
      begin
        ImageSize := CaptionRect.Bottom - CaptionRect.Top - 3;
        X := CaptionRect.Left;
        Y := CaptionRect.Top + 2;
      end
      else
      begin
        ImageSize := CaptionRect.Right - CaptionRect.Left - 3;
        X := CaptionRect.Left + 1;
        Y := CaptionRect.Top;
      end;

      FormBitmap := nil;
      DestBitmap := TBitmap.Create;
      try
        FormBitmap := TBitmap.Create;
        DestBitmap.Width :=  ImageSize;
        DestBitmap.Height := ImageSize;
        DestBitmap.Canvas.Brush.Color := clFuchsia;
        DestBitmap.Canvas.FillRect(Rect(0, 0, DestBitmap.Width, DestBitmap.Height));
        FormBitmap.Width := State.Icon.Width;
        FormBitmap.Height := State.Icon.Height;
        FormBitmap.Canvas.Draw(0, 0, State.Icon);
        ScaleImage(FormBitmap, DestBitmap, DestBitmap.Width / FormBitmap.Width);

        DestBitmap.TransparentColor := DestBitmap.Canvas.Pixels[0, DestBitmap.Height - 1];
        DestBitmap.Transparent := True;

        Canvas.Draw(X, Y, DestBitmap);
      finally
        FormBitmap.Free;
        DestBitmap.Free;
      end;

      if Self.DockCaptionOrientation = dcoHorizontal then
        CaptionRect.Left := CaptionRect.Left + 6 + ImageSize
      else
        CaptionRect.Top := CaptionRect.Top + 6 + ImageSize;
    end;
  end;

  function CalcButtonSize(
    const CaptionRect: TRect): Integer;
  const
    cButtonBuffer = 8;
  begin
    if Self.DockCaptionOrientation = dcoHorizontal then
      Result := CaptionRect.Bottom - CaptionRect.Top - cButtonBuffer
    else
      Result := CaptionRect.Right - CaptionRect.Left - cButtonBuffer;
  end;

  function GetCloseRect(const CaptionRect: TRect): TRect;
  const
    cSideBuffer = 4;
  var
    CloseSize: Integer;
  begin
    CloseSize := CalcButtonSize(CaptionRect);
    if Self.DockCaptionOrientation = dcoHorizontal then
    begin
      Result.Left := CaptionRect.Right - CloseSize - cSideBuffer;
      Result.Top := CaptionRect.Top + ((CaptionRect.Bottom - CaptionRect.Top) - CloseSize) div 2;
    end
    else
    begin
      Result.Left := CaptionRect.Left + ((CaptionRect.Right - CaptionRect.Left) - CloseSize) div 2;
      Result.Top := CaptionRect.Top + 2 * cSideBuffer;
    end;
    Result.Right := Result.Left + CloseSize;
    Result.Bottom := Result.Top + CloseSize;
  end;

  function GetPinRect(const CaptionRect: TRect): TRect;
  const
    cSideBuffer = 4;
  var
    PinSize: Integer;
  begin
    PinSize := CalcButtonSize(CaptionRect);
    if Self.DockCaptionOrientation = dcoHorizontal then
    begin
      Result.Left := CaptionRect.Right - 2*PinSize - 2*cSideBuffer;
      Result.Top := CaptionRect.Top + ((CaptionRect.Bottom - CaptionRect.Top) - PinSize) div 2;
    end
    else
    begin
      Result.Left := CaptionRect.Left + ((CaptionRect.Right - CaptionRect.Left) - PinSize) div 2;
      Result.Top := CaptionRect.Top + 2*cSideBuffer + 2*PinSize;
    end;
    Result.Right := Result.Left + PinSize + 2;
    Result.Bottom := Result.Top + PinSize;
  end;

var
  ShouldDrawClose: Boolean;
  CloseRect, PinRect: TRect;
  LPngImage : TPngImage;
begin

  if (Assigned(TColorizerLocalSettings.Settings) and not TColorizerLocalSettings.Settings.Enabled) or (not TColorizerLocalSettings.Settings.DockCustom) or (not Assigned(TColorizerLocalSettings.ColorMap)) or (Assigned(TColorizerLocalSettings.Settings) and TColorizerLocalSettings.Settings.UseVCLStyles) then
  begin
    {$IFDEF DELPHIXE6_UP}
      if Assigned(Trampoline_ModernDockCaptionDrawer_DrawDockCaption) then
       Exit(Trampoline_ModernDockCaptionDrawer_DrawDockCaption(Self, Canvas, CaptionRect, State))
      else
    {$ENDIF}
    Exit(Trampoline_TDockCaptionDrawer_DrawDockCaption(Self, Canvas, CaptionRect, State));
  end;

  Canvas.Font.Color :=  TColorizerLocalSettings.ColorMap.FontColor;
  if Self.DockCaptionOrientation = dcoHorizontal then
  begin
    Canvas.Pen.Width := 1;
    Canvas.Pen.Color := TColorizerLocalSettings.ColorMap.FrameTopLeftInner;

    CaptionRect.Top := CaptionRect.Top + 1;

    if State.Focused then
    begin
      if not TColorizerLocalSettings.Settings.DockCustomColors then
      begin
        LColorStart := TColorizerLocalSettings.ColorMap.Color;
        LColorEnd   := TColorizerLocalSettings.ColorMap.HighlightColor;
      end
      else
      begin
        try LColorStart := StringToColor(TColorizerLocalSettings.Settings.DockStartGradActive); except LColorStart := TColorizerLocalSettings.ColorMap.Color; end;
        try LColorEnd   := StringToColor(TColorizerLocalSettings.Settings.DockEndGradActive);   except LColorEnd   := TColorizerLocalSettings.ColorMap.HighlightColor; end;
      end;

    end
    else
    begin
      if not TColorizerLocalSettings.Settings.DockCustomColors then
      begin
        LColorStart := TColorizerLocalSettings.ColorMap.DisabledColor;
        LColorEnd   := TColorizerLocalSettings.ColorMap.DisabledColor;//GetHighLightColor(TColorizerLocalSettings.ColorMap.DisabledColor);
      end
      else
      begin
        try LColorStart := StringToColor(TColorizerLocalSettings.Settings.DockStartGradInActive); except LColorStart := TColorizerLocalSettings.ColorMap.DisabledColor; end;
        try LColorEnd   := StringToColor(TColorizerLocalSettings.Settings.DockEndGradInActive); except LColorEnd   := TColorizerLocalSettings.ColorMap.DisabledColor; end;
      end;
    end;

    //Canvas.Brush.Color := LColor;

    if TColorizerLocalSettings.Settings.DockGradientHor then
      GradientFillCanvas(Canvas, LColorStart, LColorEnd, Rect(CaptionRect.Left + 1, CaptionRect.Top + 1, CaptionRect.Right, CaptionRect.Bottom), gdHorizontal)
    else
      GradientFillCanvas(Canvas, LColorStart, LColorEnd, Rect(CaptionRect.Left + 1, CaptionRect.Top + 1, CaptionRect.Right, CaptionRect.Bottom), gdVertical);


    Canvas.Pen.Color :=  TColorizerLocalSettings.ColorMap.FrameTopLeftInner; //GetShadowColor(Canvas.Pen.Color, -20);
    with CaptionRect do
      Canvas.Polyline([Point(Left + 2, Top),
        Point(Right - 2, Top),
        Point(Right, Top + 2),
        Point(Right, Bottom - 2),
        Point(Right - 2, Bottom),
        Point(Left + 2, Bottom),
        Point(Left, Bottom - 2),
        Point(Left, Top + 2),
        Point(Left + 3, Top)]);

    CloseRect := GetCloseRect(CaptionRect);

    if Self.DockCaptionPinButton <> dcpbNone then
    begin
      PinRect := GetPinRect(CaptionRect);

        if Self.DockCaptionPinButton = dcpbUp then
        begin
          CropPNG(TColorizerLocalSettings.DockImages, 32, 0, 16, 16, LPngImage);
          try
            Canvas.Draw(PinRect.Left, PinRect.Top, LPngImage);
          finally
            LPngImage.free;
          end;
        end
        else
        begin
            CropPNG(TColorizerLocalSettings.DockImages, 16, 0, 16, 16, LPngImage);
            try
              Canvas.Draw(PinRect.Left, PinRect.Top, LPngImage);
            finally
              LPngImage.free;
            end;
        end;


      CaptionRect.Right := PinRect.Right - 2;
    end
    else
      CaptionRect.Right := CloseRect.Right - 2;

    CaptionRect.Left := CaptionRect.Left + 6;
    DrawIcon;
    ShouldDrawClose := CloseRect.Left >= CaptionRect.Left;

  end
  else
  begin
    Canvas.MoveTo(CaptionRect.Left + 1, CaptionRect.Top + 1);
    Canvas.LineTo(CaptionRect.Right - 1, CaptionRect.Top + 1);


    if State.Focused then
    begin
      LColorStart := TColorizerLocalSettings.ColorMap.Color;
      LColorEnd   := TColorizerLocalSettings.ColorMap.HighlightColor;
    end
    else
    begin
      LColorStart := TColorizerLocalSettings.ColorMap.DisabledColor;
      LColorEnd   := GetHighLightColor(TColorizerLocalSettings.ColorMap.DisabledColor);
    end;

    //Canvas.Brush.Color := LColor;

    //Canvas.FillRect(Rect(CaptionRect.Left, CaptionRect.Top + 2, CaptionRect.Right, CaptionRect.Bottom));
    if TColorizerLocalSettings.Settings.DockGradientHor then
      GradientFillCanvas(Canvas, LColorStart, LColorEnd, Rect(CaptionRect.Left, CaptionRect.Top + 2, CaptionRect.Right, CaptionRect.Bottom), gdHorizontal)
    else
      GradientFillCanvas(Canvas, LColorStart, LColorEnd, Rect(CaptionRect.Left, CaptionRect.Top + 2, CaptionRect.Right, CaptionRect.Bottom), gdVertical);


    Canvas.Pen.Color := State.EndColor;
    Canvas.MoveTo(CaptionRect.Left + 1, CaptionRect.Bottom);
    Canvas.LineTo(CaptionRect.Right - 1, CaptionRect.Bottom);

    Canvas.Font.Orientation := 900;
    CloseRect := GetCloseRect(CaptionRect);

    if Self.DockCaptionPinButton <> dcpbNone then
    begin
      PinRect := GetPinRect(CaptionRect);
      LPngImage:=TPNGImage.Create;
      try
        if Self.DockCaptionPinButton = dcpbUp then
         LPngImage.LoadFromResourceName(HInstance, 'pin_dock_left')
        else
         LPngImage.LoadFromResourceName(HInstance, 'pin_dock');

        Canvas.Draw(PinRect.Left, PinRect.Top, LPngImage);
      finally
        LPngImage.free;
      end;
      CaptionRect.Top := PinRect.Bottom + 2;
    end
    else
      CaptionRect.Top := CloseRect.Bottom + 2;

    ShouldDrawClose   := CaptionRect.Top < CaptionRect.Bottom;
    CaptionRect.Right := CaptionRect.Left + (CaptionRect.Bottom - CaptionRect.Top - 2);
    CaptionRect.Top   := CaptionRect.Top + Canvas.TextWidth(State.Caption) + 2;

    if CaptionRect.Top > CaptionRect.Bottom then
      CaptionRect.Top := CaptionRect.Bottom;
  end;

  Canvas.Brush.Style := bsClear;
  if State.Caption <> '' then
  begin
    if State.Focused then
      Canvas.Font.Style := Canvas.Font.Style + [fsBold]
    else
      Canvas.Font.Style := Canvas.Font.Style - [fsBold];

   if ShouldDrawClose then
     CaptionRect.Right := CaptionRect.Right - (CloseRect.Right - CloseRect.Left) - 4;

    Canvas.TextRect(CaptionRect, State.Caption,
      [tfEndEllipsis, tfVerticalCenter, tfSingleLine]);
  end;

  if ShouldDrawClose then
  begin
      CropPNG(TColorizerLocalSettings.DockImages, 0, 0, 16, 16, LPngImage);
      try
        Canvas.Draw(CloseRect.Left, CloseRect.Top, LPngImage);
      finally
        LPngImage.free;
      end;
  end;

  Exit(0);
end;

//Hook for the TCustomListView component
procedure Detour_TCustomListView_WndProc(Self:TCustomListView;var Message: TMessage);
var
  LStyleServices : {$IFDEF DELPHIXE2_UP} TCustomStyleServices {$ELSE}TThemeServices{$ENDIF};

      procedure DrawControlText(Canvas: TCanvas; Details: TThemedElementDetails;
        const S: string; var R: TRect; Flags: Cardinal);
      var
        TextFormat: TTextFormatFlags;
      begin
        Canvas.Font := TWinControlClass(Self).Font;
        TextFormat := TTextFormatFlags(Flags);
        Canvas.Font.Color := TColorizerLocalSettings.ColorMap.FontColor;
        LStyleServices.DrawText(Canvas.Handle, Details, S, R, TextFormat, Canvas.Font.Color);
      end;

    procedure DrawHeaderSection(Canvas: TCanvas; R: TRect; Index: Integer;
      const Text: string; IsPressed, IsBackground: Boolean);
    var
      Item: THDItem;
      ImageList: HIMAGELIST;
      DrawState: TThemedHeader;
      IconWidth, IconHeight: Integer;
      LDetails: TThemedElementDetails;
      LBuffer : TBitmap;
    begin
      FillChar(Item, SizeOf(Item), 0);
      Item.Mask := HDI_FORMAT;
      Header_GetItem(Self.Handle, Index, Item);

      LBuffer:=TBitmap.Create;
      try
       {$IFDEF DELPHIXE2_UP}
       LBuffer.SetSize(R.Width, R.Height);
       {$ELSE}
       LBuffer.SetSize(R.Right-R.Left, R.Bottom-R.Top);
       {$ENDIF}
       LBuffer.Canvas.Pen.Color:=TColorizerLocalSettings.ColorMap.FrameTopLeftInner;
       LBuffer.Canvas.Rectangle(Rect(0, 0, R.Right, R.Bottom));
       GradientFillCanvas(LBuffer.Canvas, TColorizerLocalSettings.ColorMap.Color, TColorizerLocalSettings.ColorMap.HighlightColor, Rect(1, 1, R.Right-1, R.Bottom-1), gdVertical);
       Canvas.Draw(R.Left, R.Top, LBuffer);
      finally
       LBuffer.Free;
      end;

      ImageList := SendMessage(Self.Handle, HDM_GETIMAGELIST, 0, 0);
      Item.Mask := HDI_FORMAT or HDI_IMAGE;
      InflateRect(R, -2, -2);
      if (ImageList <> 0) and Header_GetItem(Self.Handle, Index, Item) then
      begin
        if Item.fmt and HDF_IMAGE = HDF_IMAGE then
          ImageList_Draw(ImageList, Item.iImage, Canvas.Handle, R.Left, R.Top, ILD_TRANSPARENT);
        ImageList_GetIconSize(ImageList, IconWidth, IconHeight);
        Inc(R.Left, IconWidth + 5);
      end;

      if IsBackground then
        DrawState := thHeaderItemNormal
      else
      if IsPressed then
        DrawState := thHeaderItemPressed
      else
        DrawState := thHeaderItemNormal;

      LDetails := LStyleServices.GetElementDetails(DrawState);
      DrawControlText(Canvas, LDetails, Text, R, DT_VCENTER or DT_LEFT or  DT_SINGLELINE or DT_END_ELLIPSIS);
    end;


var
  Canvas: TCanvas;
  R, HeaderR: TRect;
  PS: TPaintStruct;
  HeaderDC: HDC;
  I, ColumnIndex, RightOffset: Integer;
  SectionOrder: array of Integer;
  Item: THDItem;
  Buffer: array [0..255] of Char;
  LParentForm : TCustomForm;
begin
  {$IFDEF DELPHIXE2_UP}
  LStyleServices:=StyleServices;
  {$ELSE}
  LStyleServices:=ThemeServices
  {$ENDIF};

  if Assigned(TColorizerLocalSettings.Settings) and TColorizerLocalSettings.Settings.Enabled and  (Message.Msg=WM_PAINT) then
  if not (csDesigning in Self.ComponentState) then
  begin
    LParentForm:= GetParentForm(Self);
    if Assigned(LParentForm) and Assigned(TColorizerLocalSettings.HookedWindows) and (TColorizerLocalSettings.HookedWindows.IndexOf(LParentForm.ClassName)>=0) then
    begin
        HeaderDC := BeginPaint(Self.GetHeaderHandle, PS);
      try
        Canvas := TCanvas.Create;
        try
          Canvas.Handle := HeaderDC;
          RightOffset := 0;

          for I := 0 to Header_GetItemCount(Self.GetHeaderHandle) - 1 do
          begin
            SetLength(SectionOrder, Header_GetItemCount(Self.GetHeaderHandle));
            Header_GetOrderArray(Self.GetHeaderHandle, Header_GetItemCount(Self.GetHeaderHandle),
              Pointer(SectionOrder));
            ColumnIndex := SectionOrder[I];
            Header_GETITEMRECT(Self.GetHeaderHandle, ColumnIndex, @R);
            FillChar(Item, SizeOf(Item), 0);
            Item.Mask := HDI_TEXT;
            Item.pszText := @Buffer;
            Item.cchTextMax := Length(Buffer);
            Header_GetItem(Self.GetHeaderHandle, ColumnIndex, Item);
            DrawHeaderSection(Canvas, R, ColumnIndex, Item.pszText,
              {FPressedSection = ColumnIndex} False, False);

            if RightOffset < R.Right then
              RightOffset := R.Right;
          end;

          GetWindowRect(Self.GetHeaderHandle, HeaderR);

          {$IFDEF DELPHIXE2_UP}
          R := Rect(RightOffset, 0, HeaderR.Width + 2, HeaderR.Height);
          {$ELSE}
          R := Rect(RightOffset, 0, (HeaderR.Right-HeaderR.Left) + 2, (HeaderR.Bottom - HeaderR.Top));
          {$ENDIF};
          if not IsRectEmpty(R) then
            DrawHeaderSection(Canvas, R, -1, '', False, True);

        finally
          Canvas.Handle := 0;
          Canvas.Free;
        end;
      finally
          EndPaint(Self.GetHeaderHandle, PS)
      end;
      exit;
    end;
  end;
  Trampoline_TCustomListView_HeaderWndProc(Self, Message);
end;

//procedure CustomProjectTree2PaintText(Self : TObject; Sender: TObject{TBaseVirtualTree}; const TargetCanvas: TCanvas; Node: {PVirtualNode}Pointer; Column: Integer{TColumnIndex}; TextType: Byte {TVSTTextType});
//begin
//  //TargetCanvas.Font.Color:=clRed;
//  Trampoline_ProjectTree2PaintText(Self, Sender, TargetCanvas, Node, Column, TextType);
//end;

//Hook for allow change font color in TProjectManagerForm.TVirtualStringTree ,
//because this component is not using the colors set via RTTI
//Note  : This is a temporal workaround.
function Detour_WinApi_DrawText(hDC: HDC; lpString: LPCWSTR; nCount: Integer;  var lpRect: TRect; uFormat: UINT): Integer; stdcall;
const
 sTCustomVirtualStringTreeSignature = 'IDEVirtualTrees.TCustomVirtualStringTree.PaintNormalText';
var
  sCaller : string;
  OldColor: Integer;
  RestoreColor : Boolean;
begin
 OldColor:=0;
 RestoreColor:=False;
 if Assigned(TColorizerLocalSettings.Settings) and TColorizerLocalSettings.Settings.Enabled and Assigned(TColorizerLocalSettings.ColorMap) then
 begin
  if GetTextColor(hDC) = GetSysColor(COLOR_WINDOWTEXT) then
  begin
   sCaller := ProcByLevel(2);
    if SameText(sCaller, sTCustomVirtualStringTreeSignature) then
    begin
       RestoreColor:=True;
       OldColor:=GetTextColor(hDC);
       SetTextColor(hDC, ColorToRGB(TColorizerLocalSettings.ColorMap.FontColor));
    end;
  end;
 end;

  Result:=Trampoline_DrawText(hDC, lpString, nCount, lpRect, uFormat);
  if RestoreColor then
    SetTextColor(hDC, OldColor);
end;

function Detour_WinApi_DrawTextEx(DC: HDC; lpchText: LPCWSTR; cchText: Integer; var p4: TRect;  dwDTFormat: UINT; DTParams: PDrawTextParams): Integer; stdcall;
begin
  Result:=Trampoline_DrawTextEx(DC, lpchText, cchText, p4, dwDTFormat, DTParams);
end;

{
    002B4B34 10661 2EDB Msglines::TCompilerMsgLine::
    002B7A68 10612 2EDC __fastcall Msglines::TCompilerMsgLine::TCompilerMsgLine(Compintf::TMsgKind, int, const System::UnicodeString, int, int, const System::UnicodeString, const System::UnicodeString, bool, const System::DelphiInterface<Msglinesintf::IMessageGroup>)
    002B7ADC 10611 2EDD __fastcall Msglines::TCompilerMsgLine::Draw(Vcl::Graphics::TCanvas *, System::Types::TRect&, bool)
    002B7BBC 10610 2EDE __fastcall Msglines::TCompilerMsgLine::GetLineText()

@Msglines@TCompilerMsgLine@
@Msglines@TCompilerMsgLine@$bctr$qqr17Compintf@TMsgKindix20System@UnicodeStringiit3t3ox54System@%DelphiInterface$t26Msglinesintf@IMessageGroup%
@Msglines@TCompilerMsgLine@Draw$qqrp20Vcl@Graphics@TCanvasrx18System@Types@TRecto
@Msglines@TCompilerMsgLine@GetLineText$qqrv

}
procedure Detour_TCompilerMsgLine_Draw(Self: TObject;Canvas : TCanvas; Rect : TRect; Flag : Boolean);
var
 OldFontColor : TColor;
begin
 OldFontColor := Canvas.Font.Color;
  if Assigned(TColorizerLocalSettings.Settings) and TColorizerLocalSettings.Settings.Enabled and Assigned(TColorizerLocalSettings.ColorMap) then
   Canvas.Font.Color:=TColorizerLocalSettings.ColorMap.FontColor;
  Trampoline_CompilerMsgLine_Draw(Self, Canvas, Rect, Flag);
 Canvas.Font.Color:=OldFontColor;
end;


{
    002B7360 10632 2F54 __fastcall Msglines::TTitleLine::Draw(Vcl::Graphics::TCanvas *, System::Types::TRect&, bool)
    @Msglines@TTitleLine@Draw$qqrp20Vcl@Graphics@TCanvasrx18System@Types@TRecto
}

procedure Detour_TTitleLine_Draw(Self: TObject;Canvas : TCanvas; Rect : TRect; Flag : Boolean);
var
 OldFontColor : TColor;
begin
 OldFontColor := Canvas.Font.Color;
  if Assigned(TColorizerLocalSettings.Settings) and TColorizerLocalSettings.Settings.Enabled and Assigned(TColorizerLocalSettings.ColorMap) then
   Canvas.Font.Color:=TColorizerLocalSettings.ColorMap.FontColor;
  Trampoline_TitleLine_Draw(Self, Canvas, Rect, Flag);
 Canvas.Font.Color:=OldFontColor;
end;

{
    002B7E58 10607 2EFE __fastcall Msglines::TFileFindLine::Draw(Vcl::Graphics::TCanvas *, System::Types::TRect&, bool)
    @Msglines@TFileFindLine@Draw$qqrp20Vcl@Graphics@TCanvasrx18System@Types@TRecto
}
procedure Detour_TFileFindLine_Draw(Self: TObject;Canvas : TCanvas; Rect : TRect; Flag : Boolean);
var
 OldFontColor : TColor;
begin
 OldFontColor := Canvas.Font.Color;
  if Assigned(TColorizerLocalSettings.Settings) and TColorizerLocalSettings.Settings.Enabled and Assigned(TColorizerLocalSettings.ColorMap) then
  begin
   Canvas.Brush.Color:=TColorizerLocalSettings.ColorMap.MenuColor;
   Canvas.Font.Color :=TColorizerLocalSettings.ColorMap.FontColor;
  end;
  Trampoline_TFileFindLine_Draw(Self, Canvas, Rect, Flag);
 Canvas.Font.Color:=OldFontColor;
end;

{
    002B7E78 10606 2F00 __fastcall Msglines::TFileFindLine::InternalCalcDraw(Vcl::Graphics::TCanvas *, System::Types::TRect&, bool, bool)
    @Msglines@TFileFindLine@InternalCalcDraw$qqrp20Vcl@Graphics@TCanvasrx18System@Types@TRectoo
}
procedure Detour_TFileFindLine_InternalCalcDraw(Self: TObject;Canvas : TCanvas; Rect : TRect; Flag, Flag2 : Boolean);
var
 OldFontColor : TColor;
begin
 OldFontColor := Canvas.Font.Color;
  if Assigned(TColorizerLocalSettings.Settings) and TColorizerLocalSettings.Settings.Enabled and Assigned(TColorizerLocalSettings.ColorMap) then
  begin
   Canvas.Brush.Color:=TColorizerLocalSettings.ColorMap.MenuColor;
   Canvas.Font.Color:=TColorizerLocalSettings.ColorMap.FontColor;
  end;
  Trampoline_TFileFindLine_InternalCalcDraw(Self, Canvas, Rect, Flag, Flag2);
 Canvas.Font.Color:=OldFontColor;
end;


//Hook to fix artifacts and undocumented painting methods ex: TClosableTabScroller background
function Detour_WinApi_GetSysColor(nIndex: Integer): DWORD; stdcall;
var
  sCaller : string;
begin
   if  Assigned(TColorizerLocalSettings.Settings) and (TColorizerLocalSettings.Settings.Enabled) and Assigned(TColorizerLocalSettings.ColorMap) then
   begin
     case nIndex of

       COLOR_INACTIVECAPTION :
       begin
         if TColorizerLocalSettings.Settings.HookSystemColors then
          Exit(ColorToRGB(TColorizerLocalSettings.ColorMap.DisabledColor))
         else
          Exit(Trampoline_GetSysColor(nIndex));
       end;

       COLOR_INACTIVECAPTIONTEXT :
       begin
         if TColorizerLocalSettings.Settings.HookSystemColors then
          Exit(ColorToRGB(TColorizerLocalSettings.ColorMap.DisabledFontColor))
         else
          Exit(Trampoline_GetSysColor(nIndex));
       end;

       COLOR_HIGHLIGHT :
       begin
         if TColorizerLocalSettings.Settings.HookSystemColors then
          Exit(ColorToRGB(TColorizerLocalSettings.ColorMap.SelectedColor))
         else
          Exit(Trampoline_GetSysColor(nIndex));
       end;

       COLOR_HIGHLIGHTTEXT:
       begin
         if TColorizerLocalSettings.Settings.HookSystemColors then
          Exit(ColorToRGB(TColorizerLocalSettings.ColorMap.SelectedFontColor))
         else
          Exit(Trampoline_GetSysColor(nIndex));
       end;

       COLOR_BTNFACE :
       begin
         sCaller := ProcByLevel(2);
         if SameText(sCaller, '') then
           Exit(ColorToRGB(TColorizerLocalSettings.ColorMap.Color));
       end;
     end;
   end;

   Exit(Trampoline_GetSysColor(nIndex));
end;


const
{$IFDEF DELPHIXE}
  sCompilerMsgLineDraw        = '@Msglines@TCompilerMsgLine@Draw$qqrp16Graphics@TCanvasrx11Types@TRecto';
  sTitleLineDraw              = '@Msglines@TTitleLine@Draw$qqrp16Graphics@TCanvasrx11Types@TRecto';
  sFileFindLineDraw           = '@Msglines@TFileFindLine@Draw$qqrp16Graphics@TCanvasrx11Types@TRecto';
{$ELSE}
  sCompilerMsgLineDraw        = '@Msglines@TCompilerMsgLine@Draw$qqrp20Vcl@Graphics@TCanvasrx18System@Types@TRecto';
  sTitleLineDraw              = '@Msglines@TTitleLine@Draw$qqrp20Vcl@Graphics@TCanvasrx18System@Types@TRecto';
  sFileFindLineDraw           = '@Msglines@TFileFindLine@Draw$qqrp20Vcl@Graphics@TCanvasrx18System@Types@TRecto';
  sFileFindLineInternalCalcDraw = '@Msglines@TFileFindLine@InternalCalcDraw$qqrp20Vcl@Graphics@TCanvasrx18System@Types@TRectoo';
{$ENDIF}

  sProjectTree2PaintText      = '@Projectfrm@TProjectManagerForm@ProjectTree2PaintText$qqrp32Idevirtualtrees@TBaseVirtualTreexp20Vcl@Graphics@TCanvasp28Idevirtualtrees@TVirtualNodei28Idevirtualtrees@TVSTTextType';
{$IFDEF DELPHIXE6_UP}
  sModernThemeDrawDockCaption = '@Moderntheme@TModernDockCaptionDrawer@DrawDockCaption$qqrxp20Vcl@Graphics@TCanvasrx18System@Types@TRectrx38Vcl@Captioneddocktree@TParentFormState';
{$ENDIF}
  sBaseVirtualTreePrepareBitmaps = '@Idevirtualtrees@TBaseVirtualTree@PrepareBitmaps$qqroo';
  sListButtonPaint               = '@Idelistbtns@TListButton@Paint$qqrv';

type
  THintWindowClass = class(THintWindow);
{$IFNDEF DELPHIXE2_UP}
type
 //TThemeServicesDrawElement1 =  procedure (DC: HDC; Details: TThemedElementDetails;  const R: TRect) of object;
 TThemeServicesDrawElement2 =  procedure (DC: HDC; Details: TThemedElementDetails;  const R: TRect; ClipRect: TRect) of object;
{$ENDIF}


procedure InstallColorizerHooks;
var
  pOrgAddress, GetSysColorOrgPointer : Pointer;
{$IFDEF DELPHIXE6_UP}
  ModernThemeModule           : HMODULE;
  pModernThemeDrawDockCaption : Pointer;
{$ENDIF}
{$IFNDEF DELPHIXE2_UP}
 LThemeServicesDrawElement2   : TThemeServicesDrawElement2;
{$ENDIF}
 CoreIDEModule, VclIDEModule : HMODULE;
begin
 ListControlWrappers := TObjectDictionary<TCustomControl, TRttiWrapper>.Create([doOwnsValues]);

  CoreIDEModule := LoadLibrary(sCoreIDEModule);
  if CoreIDEModule<>0 then
  begin
   pOrgAddress := GetProcAddress(CoreIDEModule, sCompilerMsgLineDraw);
   if Assigned(pOrgAddress) then
    Trampoline_CompilerMsgLine_Draw := InterceptCreate(pOrgAddress, @Detour_TCompilerMsgLine_Draw);

   pOrgAddress := GetProcAddress(CoreIDEModule, sTitleLineDraw);
   if Assigned(pOrgAddress) then
     Trampoline_TitleLine_Draw   := InterceptCreate(pOrgAddress, @Detour_TTitleLine_Draw);

   pOrgAddress := GetProcAddress(CoreIDEModule, sFileFindLineDraw);
   if Assigned(pOrgAddress) then
     Trampoline_TFileFindLine_Draw   := InterceptCreate(pOrgAddress, @Detour_TFileFindLine_Draw);

//   pOrgAddress := GetProcAddress(CoreIDEModule, sFileFindLineInternalCalcDraw);
//   if Assigned(pOrgAddress) then
//     Trampoline_TFileFindLine_InternalCalcDraw   := InterceptCreate(pOrgAddress, @Detour_TFileFindLine_InternalCalcDraw);
  end;

  VclIDEModule := LoadLibrary(sVclIDEModule);
  if VclIDEModule<>0 then
  begin
 //  pOrgAddress := GetProcAddress(VclIDEModule, sBaseVirtualTreePrepareBitmaps);
//   if Assigned(pOrgAddress) then
 //   Trampoline_TBaseVirtualTree_PrepareBitmaps := InterceptCreate(pOrgAddress, @Detour_TBaseVirtualTree_PrepareBitmaps);

   pOrgAddress := GetProcAddress(VclIDEModule, sListButtonPaint);
   if Assigned(pOrgAddress) then
    Trampoline_TListButton_Paint := InterceptCreate(pOrgAddress, @Detour_TListButton_Paint);
  end;

  TrampolineTWinControl_DefaultHandler:=InterceptCreate(@TWinControl.DefaultHandler, @Detour_TWinControl_DefaultHandler);

  Trampoline_HintWindow_Paint := InterceptCreate(@THintWindowClass.Paint, @Detour_THintWindow_Paint);
  Trampoline_Bevel_Paint      := InterceptCreate(@TBevelClass.Paint, @Detour_TBevel_Paint);

{$IF CompilerVersion<27} //XE6
  TrampolineCustomImageList_DoDraw:=InterceptCreate(@TCustomImageListClass.DoDraw, @Detour_TCustomImageList_DoDraw);
{$IFEND}
  Trampoline_TCanvas_FillRect     :=InterceptCreate(@TCanvas.FillRect, @Detour_TCanvas_FillRect);
  Trampoline_TCanvas_LineTo       :=InterceptCreate(@TCanvas.LineTo, @Detour_TCanvas_LineTo);
  Trampoline_TCanvas_Rectangle    :=InterceptCreate(@TCanvas.Rectangle, @Detour_TCanvas_Rectangle);

  Trampoline_TCustomStatusBar_WMPAINT   := InterceptCreate(TCustomStatusBarClass(nil).WMPaintAddress,   @Detour_TStatusBar_WMPaint);
  Trampoline_CustomComboBox_WMPaint     := InterceptCreate(TCustomComboBox(nil).WMPaintAddress,   @Detour_TCustomComboBox_WMPaint);
  Trampoline_TCustomCombo_WndProc       := InterceptCreate(@TCustomComboClass.WndProc,   @Detour_TCustomCombo_WndProc);
  Trampoline_TDockCaptionDrawer_DrawDockCaption  := InterceptCreate(@TDockCaptionDrawer.DrawDockCaption,   @Detour_TDockCaptionDrawer_DrawDockCaption);

  //Trampoline_TBitmap_SetSize := InterceptCreate(@TBitmap.SetSize,   @CustomSetSize);

{$IFDEF DELPHIXE2_UP}
  Trampoline_TStyleEngine_HandleMessage     := InterceptCreate(@TStyleEngine.HandleMessage,   @Detour_TStyleEngine_HandleMessage);
  Trampoline_TUxThemeStyle_DoDrawElement    := InterceptCreate(@TUxThemeStyleClass.DoDrawElement,   @Detour_TUxThemeStyle_DrawElement);
{$ELSE}
  LThemeServicesDrawElement2                := ThemeServices.DrawElement;
  Trampoline_TUxTheme_DrawElement           := InterceptCreate(@LThemeServicesDrawElement2,   @Detour_TThemeServices_DrawElement);
  if Assigned(DrawThemeBackground) then
    Trampoline_DrawThemeBackground            := InterceptCreate(@DrawThemeBackground,   @Detour_UxTheme_DrawBackground);
{$ENDIF}
  Trampoline_TCustomListView_HeaderWndProc  := InterceptCreate(TCustomListViewClass(nil).HeaderWndProcAddress, @Detour_TCustomListView_WndProc);
  Trampoline_DrawText                       := InterceptCreate(@Windows.DrawTextW, @Detour_WinApi_DrawText);
  Trampoline_DrawTextEx                     := InterceptCreate(@Windows.DrawTextEx, @Detour_WinApi_DrawTextEx);

   GetSysColorOrgPointer     := GetProcAddress(GetModuleHandle(user32), 'GetSysColor');
   if Assigned(GetSysColorOrgPointer) then
     Trampoline_GetSysColor    :=  InterceptCreate(GetSysColorOrgPointer, @Detour_WinApi_GetSysColor);

   pOrgAddress     := GetProcAddress(GetModuleHandle(user32), 'DrawFrameControl');
   if Assigned(pOrgAddress) then
     Trampoline_DrawFrameControl :=  InterceptCreate(pOrgAddress, @Detour_WinApi_DrawFrameControl);

   pOrgAddress     := GetProcAddress(GetModuleHandle(user32), 'DrawEdge');
   if Assigned(pOrgAddress) then
     Trampoline_DrawEdge :=  InterceptCreate(pOrgAddress, @Detour_WinApi_DrawEdge);

  Trampoline_TCategoryButtons_DrawCategory := InterceptCreate(TCategoryButtons(nil).DrawCategoryAddress,   @Detour_TCategoryButtons_DrawCategory);
  Trampoline_TCustomPanel_Paint            := InterceptCreate(@TCustomPanelClass.Paint, @Detour_TCustomPanel_Paint);

  Trampoline_TWinControl_WMNCPaint      := InterceptCreate(TWinControl(nil).WMNCPaintAddress, @Detour_TWinControl_WMNCPaint);

  Trampoline_DoModernPainting           := InterceptCreate(TTabSet(nil).DoModernPaintingAddress, @Detour_TTabSet_DoModernPainting);

  Trampoline_TSplitter_Paint            := InterceptCreate(@TSplitterClass.Paint, @Detour_TSplitter_Paint);
  Trampoline_TButtonControl_WndProc     := InterceptCreate(@TButtonControlClass.WndProc, @Detour_TButtonControlClass_WndProc);

{$IFDEF DLLWIZARD}
  Trampoline_TCustomForm_WndProc        := InterceptCreate(@TCustomFormClass.WndProc, @Detour_TCustomForm_WndProc);
{$ENDIF}

{$IFDEF DELPHIXE6_UP}
  ModernThemeModule := LoadLibrary('ModernTheme200.bpl');
  if ModernThemeModule<>0 then
  begin
   pModernThemeDrawDockCaption := GetProcAddress(ModernThemeModule, sModernThemeDrawDockCaption);
   if Assigned(pModernThemeDrawDockCaption) then
     Trampoline_ModernDockCaptionDrawer_DrawDockCaption:= InterceptCreate(pModernThemeDrawDockCaption, @Detour_TDockCaptionDrawer_DrawDockCaption);
  end;
{$ENDIF}
end;

procedure RemoveColorizerHooks;
begin
  if Assigned(Trampoline_CompilerMsgLine_Draw) then
    InterceptRemove(@Trampoline_CompilerMsgLine_Draw);

  if Assigned(Trampoline_TitleLine_Draw) then
    InterceptRemove(@Trampoline_TitleLine_Draw);

  if Assigned(Trampoline_TFileFindLine_Draw) then
    InterceptRemove(@Trampoline_TFileFindLine_Draw);

  if Assigned(Trampoline_TFileFindLine_InternalCalcDraw) then
    InterceptRemove(@Trampoline_TFileFindLine_InternalCalcDraw);

  if Assigned(Trampoline_HintWindow_Paint) then
    InterceptRemove(@Trampoline_HintWindow_Paint);

  if Assigned (TrampolineTWinControl_DefaultHandler) then
    InterceptRemove(@TrampolineTWinControl_DefaultHandler);

  if Assigned(Trampoline_Bevel_Paint) then
    InterceptRemove(@Trampoline_Bevel_Paint);

  if Assigned(Trampoline_TBaseVirtualTree_PrepareBitmaps) then
    InterceptRemove(@Trampoline_TBaseVirtualTree_PrepareBitmaps);

  if Assigned(Trampoline_TListButton_Paint) then
    InterceptRemove(@Trampoline_TListButton_Paint);

{$IF CompilerVersion<27} //XE6
  if Assigned(TrampolineCustomImageList_DoDraw) then
    InterceptRemove(@TrampolineCustomImageList_DoDraw);
{$IFEND}
  if Assigned(Trampoline_TCanvas_FillRect) then
    InterceptRemove(@Trampoline_TCanvas_FillRect);

  if Assigned(Trampoline_TCanvas_LineTo) then
    InterceptRemove(@Trampoline_TCanvas_LineTo);

  if Assigned(Trampoline_TCanvas_Rectangle) then
    InterceptRemove(@Trampoline_TCanvas_Rectangle);

{$IFDEF DELPHIXE2_UP}
  if Assigned(Trampoline_TStyleEngine_HandleMessage) then
    InterceptRemove(@Trampoline_TStyleEngine_HandleMessage);
  if Assigned(Trampoline_TUxThemeStyle_DoDrawElement) then
    InterceptRemove(@Trampoline_TUxThemeStyle_DoDrawElement);
{$ELSE}
  if Assigned(Trampoline_TUxTheme_DrawElement) then
    InterceptRemove(@Trampoline_TUxTheme_DrawElement);
  if Assigned(Trampoline_DrawThemeBackground) then
    InterceptRemove(@Trampoline_DrawThemeBackground);
{$ENDIF}
  if Assigned(Trampoline_TCustomStatusBar_WMPAINT) then
    InterceptRemove(@Trampoline_TCustomStatusBar_WMPAINT);
  if Assigned(Trampoline_TDockCaptionDrawer_DrawDockCaption) then
    InterceptRemove(@Trampoline_TDockCaptionDrawer_DrawDockCaption);
  if Assigned(Trampoline_TCustomListView_HeaderWndProc) then
    InterceptRemove(@Trampoline_TCustomListView_HeaderWndProc);
  if Assigned(Trampoline_ProjectTree2PaintText) then
    InterceptRemove(@Trampoline_ProjectTree2PaintText);

  if Assigned(Trampoline_DoModernPainting) then
     InterceptRemove(@Trampoline_DoModernPainting);

  if Assigned(Trampoline_DrawText) then
    InterceptRemove(@Trampoline_DrawText);

  if Assigned(Trampoline_DrawTextEx) then
    InterceptRemove(@Trampoline_DrawTextEx);

  if Assigned(Trampoline_GetSysColor) then
    InterceptRemove(@Trampoline_GetSysColor);

  if Assigned(Trampoline_TCategoryButtons_DrawCategory) then
    InterceptRemove(@Trampoline_TCategoryButtons_DrawCategory);

  if Assigned(Trampoline_TCustomPanel_Paint) then
    InterceptRemove(@Trampoline_TCustomPanel_Paint);

  if Assigned(Trampoline_TWinControl_WMNCPaint) then
    InterceptRemove(@Trampoline_TWinControl_WMNCPaint);

  if Assigned(Trampoline_TButtonControl_WndProc) then
    InterceptRemove(@Trampoline_TButtonControl_WndProc);

  if Assigned(Trampoline_TSplitter_Paint) then
    InterceptRemove(@Trampoline_TSplitter_Paint);

  if Assigned(Trampoline_CustomComboBox_WMPaint) then
    InterceptRemove(@Trampoline_CustomComboBox_WMPaint);

  if Assigned(Trampoline_TCustomCombo_WndProc) then
    InterceptRemove(@Trampoline_TCustomCombo_WndProc);

  if Assigned(Trampoline_DrawFrameControl) then
    InterceptRemove(@Trampoline_DrawFrameControl);

  if Assigned(Trampoline_DrawEdge) then
    InterceptRemove(@Trampoline_DrawEdge);

{$IFDEF DELPHIXE6_UP}
  if Assigned(Trampoline_ModernDockCaptionDrawer_DrawDockCaption) then
    InterceptRemove(@Trampoline_ModernDockCaptionDrawer_DrawDockCaption);
{$ENDIF}

{$IFDEF DLLWIZARD}
  if Assigned(Trampoline_TCustomForm_WndProc) then
    InterceptRemove(@Trampoline_TCustomForm_WndProc);
{$ENDIF}


   ListControlWrappers.Free;
end;

end.

