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
// Portions created by Rodrigo Ruz V. are Copyright (C) 2011-2015 Rodrigo Ruz V.
// All Rights Reserved.
//
//**************************************************************************************************

unit Colorizer.Hooks;

interface
{$I ..\Common\Jedi.inc}

uses
  SyncObjs,
  Controls;

 procedure InstallColorizerHooks;
 procedure RemoveColorizerHooks;
 procedure DrawNCBorder(Self : TWinControl; EraseLRCorner: Boolean);

var
  //LastScrollWinControl  : TWinControl = nil;
  LastWinControl        : TWinControl = nil;
  DrawNamePair          : Boolean     = False;
  EnableHookTBitBtn     : Boolean     = False;
{$IFDEF DELPHIXE7_UP}
  EnableStockHook  : Boolean     = False;
  HooksLock  : TCriticalSection = nil;
{$ENDIF}

implementation

uses
{$IFDEF DELPHIXE2_UP}
  Vcl.Themes,
  Winapi.UxTheme,
  Colorizer.VCL.Styles,
{$ELSE}
  Themes,
  UxTheme,
  Colorizer.uxThemeHelper,
{$ENDIF}
  Messages,
  System.Diagnostics,
  System.TimeSpan,
  TypInfo,
  Vcl.Forms,
  Vcl.ExtCtrls,
  Vcl.Buttons,
  Vcl.Dialogs,
  Vcl.ComCtrls,
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
  StrUtils,
  JclDebug,
  PngImage,
  Colorizer.Utils,
  Colorizer.Wrappers,
  Colorizer.VirtualTrees,
  Colorizer.Hooks.Windows,
  Colorizer.Hooks.UxTheme,
  Colorizer.Hooks.IDE,
  CaptionedDockTree,
  Vcl.GraphUtil,
  Vcl.CategoryButtons,
  Vcl.ActnPopup,
  Vcl.ActnMan,
  Vcl.StdCtrls,
  Vcl.ActnMenus,
  Vcl.Tabs,
  uRttiHelper,
  Rtti,
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
 TBrushClass             = class(TBrush);
 TFontClass              = class(TFont);
 TCustomListViewClass    = class(TCustomListView);
 TSplitterClass          = class(TSplitter);
 TBitBtnClass            = class(TBitBtn);
 TCustomGroupBoxClass    = class(TCustomGroupBox);
 TButtonControlClass     = class(TButtonControl);
 TCustomCheckBoxClass    = class(TCustomCheckBox);
 TRadioButtonClass       = class(TRadioButton);
 TCustomComboClass       = class(TCustomCombo);
 TCustomComboBoxClass    = class(TCustomComboBox);
 TCustomListBoxClass     = class(TCustomListBox);
 TBevelClass             = class(TBevel);
 TCustomActionPopupMenuClass = class(TCustomActionPopupMenu);
 TCustomControlClass     = class(TCustomControl);
 TCustomControlBarClass  = class(TCustomControlBar);
 TCustomButtonClass      = class(TCustomButton);
 TCustomLabelClass       = class(TCustomLabel);
var
  TrampolineCustomImageList_DoDraw     : procedure (Self: TObject; Index: Integer; Canvas: TCanvas; X, Y: Integer; Style: Cardinal; Enabled: Boolean) = nil;
  Trampoline_TCanvas_FillRect          : procedure (Self: TCanvas;const Rect: TRect) = nil;
  Trampoline_TCanvas_LineTo            : procedure (Self: TCanvas; X, Y: Integer) = nil;
  Trampoline_TCanvas_Rectangle         : procedure (Self: TCanvas; X1, Y1, X2, Y2: Integer) = nil;
//  Trampoline_TCanvas_Polygon           : procedure (Self: TCanvas;const Points: array of TPoint) = nil;
//  Trampoline_TCanvas_Polyline          : procedure (Self: TCanvas;const Points: array of TPoint) = nil;

  Trampoline_TWinControl_DefaultHandler : procedure (Self : TWinControl;var Message) = nil;
  {$IFDEF DELPHIXE2_UP}
  //Trampoline_TStyleEngine_HandleMessage    : function (Self: TStyleEngine; Control: TWinControl; var Message: TMessage; DefWndProc: TWndMethod): Boolean = nil;
  Trampoline_TUxThemeStyle_DoDrawElement   : function (Self : TUxThemeStyle;DC: HDC; Details: TThemedElementDetails; const R: TRect; ClipRect: PRect = nil): Boolean = nil;
  {$ELSE}
  Trampoline_TUxTheme_DrawElement          : procedure (Self : TThemeServices;DC: HDC; Details: TThemedElementDetails; const R: TRect; ClipRect: TRect);
  Trampoline_DrawThemeBackground           : function (hTheme: UxTheme.HTHEME; hdc: HDC; iPartId, iStateId: Integer; const pRect: TRect; pClipRect: PRECT): HRESULT; stdcall = nil;
  {$ENDIF}

  Trampoline_TCategoryButtons_DrawCategory : procedure(Self :TCategoryButtons; const Category: TButtonCategory; const Canvas: TCanvas; StartingPos: Integer) = nil;
  Trampoline_TCategoryButtons_DrawButton   : procedure(Self :TCategoryButtons;const Button: TButtonItem; Canvas: TCanvas; Rect: TRect; State: TButtonDrawState);

  //Trampoline_TBitmap_SetSize : procedure(Self : TBitmap;AWidth, AHeight: Integer) = nil;
  Trampoline_TCustomPanel_Paint            : procedure (Self : TCustomPanelClass) = nil;
  Trampoline_TWinControl_WMNCPaint         : procedure (Self: TWinControlClass; var Message: TWMNCPaint);
  Trampoline_TSplitter_Paint               : procedure (Self : TSplitterClass) = nil;
  Trampoline_TCustomGroupBox_Paint         : procedure (Self : TCustomGroupBoxClass) = nil;

  Trampoline_TBitBtn_DrawItem              : procedure (Self: TBitBtn;const DrawItemStruct: TDrawItemStruct);

  {$IFDEF DELPHIXE7_UP}
  Trampoline_TCustomComboBox_DrawItem      :  procedure(Self: TCustomComboBox;Index: Integer; Rect: TRect; State: TOwnerDrawState);
  Trampoline_TCustomListBox_DrawItem       :  procedure(Self : TCustomListBox; Index: Integer; Rect: TRect;  State: TOwnerDrawState);
  {$ENDIF}

  Trampoline_CustomComboBox_WMPaint        : procedure (Self: TCustomComboBox;var Message: TWMPaint) = nil;
  Trampoline_TCustomCombo_WndProc          : procedure (Self: TCustomCombo;var Message: TMessage) = nil;
  Trampoline_TButtonControl_WndProc        : procedure (Self:TButtonControlClass;var Message: TMessage) = nil;
  //Trampoline_TCustomStatusBar_WMPAINT      : procedure (Self: TCustomStatusBarClass; var Message: TWMPaint) = nil;
  Trampoline_TCustomListView_HeaderWndProc : procedure (Self:TCustomListView;var Message: TMessage) = nil;

  Trampoline_DoModernPainting              : procedure (Self : TTabSet) = nil;
  Trampoline_HintWindow_Paint              : procedure (Self : THintWindow) = nil;
  Trampoline_MessageHintWindow_Paint       : procedure (Self : THintWindow) = nil;
  Trampoline_Bevel_Paint                   : procedure (Self : TBevel) = nil;
  Trampoline_TCustomControlBar_PaintControlFrame  : procedure (Self:TCustomControlBar; Canvas: TCanvas; AControl: TControl; var ARect: TRect) = nil;
  Trampoline_TCustomLabel_DoDrawText       : procedure (Self : TCustomLabel;var Rect: TRect; Flags: Longint) = nil;


  //Trampoline_TCustomActionPopupMenu_CreateParams : procedure(Self: TCustomActionPopupMenu;var Params: TCreateParams) = nil;
  {$IFDEF DELPHIXE7_UP}
  Trampoline_TBrush_SetColor   : procedure (Self: TBrush; Value: TColor);
  Trampoline_TFont_SetColor    : procedure (Self: TFont; const Value: TColor);
  {$ENDIF}


  FGutterBkColor : TColor = clNone;
type

  TBitBtnHelper = class helper for TBitBtn
    function  DrawItemAddress: Pointer;
  end;

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
    function  InsertBottomHelper : TBaseItem;
    function  InsertTopHelper : TBaseItem;
    function  InsertRightHelper : TBaseItem;
    function  InsertLeftHelper : TBaseItem;
   end;

  TCustomComboBoxBarHelper = class helper for TCustomComboBox
  public
    function  WMPaintAddress: Pointer;
  end;

  TWinControlHelper  = class helper for TWinControl
  public
    function WMNCPaintAddress : Pointer;
  end;


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



//Hook used to style the TBitBtn,
procedure Detour_TBitBtn_DrawItem(Self: TBitBtn;const DrawItemStruct: TDrawItemStruct);
var
  LParentForm : TCustomForm;
begin

  if Assigned(TColorizerLocalSettings.Settings) and TColorizerLocalSettings.Settings.Enabled and (not (csDesigning in Self.ComponentState)) and Assigned(TColorizerLocalSettings.ColorMap) then
  begin
    LParentForm:= GetParentForm(Self);
    if Assigned(LParentForm) and Assigned(TColorizerLocalSettings.HookedWindows) and (TColorizerLocalSettings.HookedWindows.IndexOf(LParentForm.ClassName)>=0) then
      EnableHookTBitBtn:=True;
  end;

  Trampoline_TBitBtn_DrawItem(Self, DrawItemStruct);
  if EnableHookTBitBtn then
  EnableHookTBitBtn:=False;
end;


procedure Detour_TCustomLabelClass_DoDrawText(Self : TCustomLabelClass;var Rect: TRect; Flags: Longint);
const
  EllipsisStr = '...';
  Ellipsis: array[TEllipsisPosition] of Longint = (0, DT_PATH_ELLIPSIS,
    DT_END_ELLIPSIS, DT_WORD_ELLIPSIS);
var
  LParentForm : TCustomForm;
  LRect: TRect;
  Height, Delim: Integer;
  LText, DText: string;

  procedure DrawStyledText(DC: HDC; const Text: UnicodeString; var TextRect: TRect; TextFlags: Cardinal);
  const
    CStates: array[Boolean] of TThemedTextLabel = (ttlTextLabelDisabled, ttlTextLabelNormal);
  var
    LFormat: TTextFormat;
    LOptions: TStyleTextOptions;
  begin
    LFormat := TTextFormatFlags(TextFlags);
    if csGlassPaint in Self.ControlState then
      Include(LFormat, tfComposited);

    LOptions.Flags := [stfTextColor, stfGlowSize];
    LOptions.TextColor := Self.Canvas.Font.Color;
    LOptions.GlowSize := Self.GlowSize;

    ColorizerStyleServices.DrawText(DC,
      ColorizerStyleServices.GetElementDetails(CStates[Self.Enabled]), Text, TextRect, LFormat, LOptions);
  end;

  procedure DrawNormalText(DC: HDC; const Text: UnicodeString; var TextRect: TRect; TextFlags: Cardinal);
  begin
    Windows.DrawTextW(DC, Text, Length(Text), TextRect, TextFlags);
  end;

begin
  if (Assigned(TColorizerLocalSettings.Settings) and not TColorizerLocalSettings.Settings.Enabled) or (csDesigning in Self.ComponentState) or (not Assigned(TColorizerLocalSettings.ColorMap)) then
  begin
    Trampoline_TCustomLabel_DoDrawText(Self, Rect, Flags);
    exit;
  end;

  LParentForm:= GetParentForm(Self);
  if not (Assigned(LParentForm) and Assigned(TColorizerLocalSettings.HookedWindows) and (TColorizerLocalSettings.HookedWindows.IndexOf(LParentForm.ClassName)>=0)) then
  begin
    Trampoline_TCustomLabel_DoDrawText(Self, Rect, Flags);
    exit;
  end;

  if Self.Enabled then
  begin
    Trampoline_TCustomLabel_DoDrawText(Self, Rect, Flags);
    exit;
  end
  else
  begin
    LText := Self.Caption;
    if (Flags and DT_CALCRECT <> 0) and
       ((LText = '') or Self.ShowAccelChar and (LText[1] = '&') and (Length(LText) = 1)) then
      LText := LText + ' ';

    if LText <> '' then
    begin
      if not Self.ShowAccelChar then Flags := Flags or DT_NOPREFIX;
      Flags := Self.DrawTextBiDiModeFlags(Flags);
      Self.Canvas.Font := Self.Font;
      if (Self.EllipsisPosition <> epNone) and not Self.AutoSize then
      begin
        DText := LText;
        Flags := Flags and not DT_EXPANDTABS;
        Flags := Flags or Ellipsis[Self.EllipsisPosition];
        if Self.WordWrap and (Self.EllipsisPosition in [epEndEllipsis, epWordEllipsis]) then
        begin
          repeat
            LRect := Rect;
            Dec(LRect.Right, Self.Canvas.TextWidth(EllipsisStr));

            if TColorizerLocalSettings.Settings.UseVCLStyles and TColorizerLocalSettings.Settings.VCLStylesControls then
             DrawStyledText(Self.Canvas.Handle, DText, LRect, Flags or DT_CALCRECT)
            else
             DrawNormalText(Self.Canvas.Handle, DText, LRect, Flags or DT_CALCRECT);

            Height := LRect.Bottom - LRect.Top;
            if (Height > Self.ClientHeight) and (Height > Self.Canvas.Font.Height) then
            begin
              Delim := LastDelimiter(' '#9, LText);
              if Delim = 0 then
                Delim := Length(LText);
              Dec(Delim);
              if ByteType(LText, Delim) = mbLeadByte then
                Dec(Delim);
              LText := Copy(LText, 1, Delim);
              DText := LText + EllipsisStr;
              if LText = '' then
                Break;
            end
             else
              Break;
          until False;
        end;
        if LText <> '' then
          LText := DText;
      end;

     if TColorizerLocalSettings.Settings.UseVCLStyles and TColorizerLocalSettings.Settings.VCLStylesControls then
       DrawStyledText(Self.Canvas.Handle, LText, Rect, Flags)
     else
     begin
       Self.Canvas.Font.Color:=TColorizerLocalSettings.ColorMap.DisabledFontColor;
       DrawNormalText(Self.Canvas.Handle, LText, Rect, Flags);
     end;
    end;
  end;
end;

procedure Detour_TCustomControlBar_PaintControlFrame(Self:TCustomControlBarClass; Canvas: TCanvas; AControl: TControl; var ARect: TRect);
const
  Offset = 3;
var
  LRect: TRect;
  Options: TBandPaintOptions;
  LParentForm : TCustomForm;
begin

  if (Assigned(TColorizerLocalSettings.Settings) and not TColorizerLocalSettings.Settings.Enabled) or (csDesigning in Self.ComponentState) or (not Assigned(TColorizerLocalSettings.ColorMap)) then
  begin
    Trampoline_TCustomControlBar_PaintControlFrame(Self, Canvas, AControl, ARect);
    exit;
  end;

  LParentForm:= GetParentForm(Self);
//  if LParentForm<>nil then
//   AddLog('Detour_TCustomControlBar_PaintControlFrame', 'LParentForm.ClassName '+ LParentForm.ClassName);
  if not (Assigned(LParentForm) and Assigned(TColorizerLocalSettings.HookedWindows) and (TColorizerLocalSettings.HookedWindows.IndexOf(LParentForm.ClassName)>=0)) then
  begin
    //AddLog('Detour_TCustomControlBar_PaintControlFrame', 'ignored LParentForm.ClassName '+ LParentForm.ClassName);
    Trampoline_TCustomControlBar_PaintControlFrame(Self, Canvas, AControl, ARect);
    exit;
  end;

  if  Self.DrawingStyle = TBandDrawingStyle.dsGradient then
    Options := [bpoGrabber, bpoGradient, bpoRoundRect]
  else
    Options := [bpoGrabber, bpoFrame];

  Self.DoBandPaint(AControl, Canvas, ARect, Options);
  with Canvas do
  begin
    if bpoFrame in Options then
      DrawEdge(Handle, ARect, BDR_RAISEDINNER, BF_RECT);

    if bpoRoundRect in Options then
    begin
      BeginPath(Handle);
        Polyline([Point(ARect.Left + Integer(Self.CornerEdge), ARect.Top),
          Point(ARect.Right - Integer(Self.CornerEdge), ARect.Top), { Top line }
          Point(ARect.Right, ARect.Top + Integer(Self.CornerEdge)), { Top right curve }
          Point(ARect.Right, ARect.Bottom - Integer(Self.CornerEdge)), { Right side line }
          Point(ARect.Right - Integer(Self.CornerEdge), ARect.Bottom), { Bottom right curve }
          Point(ARect.Left + Integer(Self.CornerEdge), ARect.Bottom), { Bottom line }
          Point(ARect.Left, ARect.Bottom - Integer(Self.CornerEdge)), { Bottom left curve }
          Point(ARect.Left, ARect.Top + Integer(Self.CornerEdge)), { Left side line }
          Point(ARect.Left + Integer(Self.CornerEdge), ARect.Top)]); { Top left curve }
      EndPath(Handle);

      SelectClipPath(Handle, RGN_COPY);
    end;

//    if bpoGradient in Options then
//      DrawGradient;
//
    if bpoGrabber in Options then
    begin
      LRect := Rect(ARect.Left + Offset, ARect.Top + 2, ARect.Left + Offset + 2, ARect.Bottom - 3);
      Pen.Color := TColorizerLocalSettings.ColorMap.FrameTopLeftOuter;
      MoveTo(LRect.Left + 1, LRect.Top);
      LineTo(LRect.Left, LRect.Top);
      LineTo(LRect.Left, LRect.Bottom);
      Pen.Color := TColorizerLocalSettings.ColorMap.FrameTopLeftInner;
      MoveTo(LRect.Right, LRect.Top);
      LineTo(LRect.Right, LRect.Bottom - 1);
      LineTo(LRect.Left, LRect.Bottom-1);
    end;
  end;
end;


type
  THintWindowClass = class(THintWindow);

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
  Self.Canvas.Brush.Color:= TColorizerLocalSettings.ColorMap.WindowColor;
  Self.Canvas.Pen.Color:= TColorizerLocalSettings.ColorMap.FrameTopLeftOuter;
  Self.Canvas.Rectangle(R);
  R := ClipRect;
  Inc(R.Left, 2);
  Inc(R.Top, 2);
  Self.Canvas.Font.Color := LTextColor;
  DrawText(Self.Canvas.Handle, Self.Caption, -1, R, DT_LEFT or DT_NOPREFIX or  DT_WORDBREAK or Self.DrawTextBiDiModeFlagsReadingOnly);
end;

//Detour for TWinControl.DefaultHandler
procedure Detour_TWinControl_DefaultHandler(Self : TWinControl;var Message);
var
  LParentForm : TCustomForm;
  ApplyHook   : Boolean;
//  LContext    : TRttiContext;
//  LType       : TRttiType;
//  LMethod     : TRttiMethod;
begin
  //LastScrollWinControl:=Self;
  LastWinControl      :=Self;

//  if SameText('TMessageHintWindow', Self.ClassName) then
//  begin
//   TRttiUtils.DumpObject(Self, 'C:\Delphi\google-code\DITE\delphi-ide-theme-editor\IDE PlugIn\Galileo\'+Self.ClassName+'.pas');
//  end;

//  if SameText('TExpandableEvalView', Self.ClassName) then
//  begin
//    //TRttiUtils.DumpObject(Self, 'C:\Delphi\google-code\DITE\delphi-ide-theme-editor\IDE PlugIn\Galileo\'+Self.ClassName+'.pas');
//
//  end;

//  if SameText('TMessageHintWindow', Self.ClassName) then
//  begin
//   if @Trampoline_MessageHintWindow_Paint=nil then
//   begin
//    LContext:=TRttiContext.Create;
//    try
//       LType:= LContext.GetType(Self);
//       LMethod:=LType.GetMethod('Paint');
//       if Assigned(LMethod) then
//         AddLog('Detour_TWinControl_DefaultHandler','LMethod Found');
//      //Trampoline_MessageHintWindow_Paint:=InterceptCreate(@THintWindowClass.Paint, @Detour_THintWindow_Paint);
//      //AddLog('Detour_TWinControl_DefaultHandler','TMessageHintWindow Hooked');
//    finally
//      LContext.Free;
//    end;
//   end;
//   //TRttiUtils.DumpObject(Self, 'C:\Delphi\google-code\DITE\delphi-ide-theme-editor\IDE PlugIn\Galileo\'+Self.ClassName+'.pas');
//
//  end;

  if SameText('TDisassemblyView', Self.ClassName) and Assigned(TColorizerLocalSettings.Settings) and TColorizerLocalSettings.Settings.Enabled then
  begin
    //AddLog2('CustomDefaultHandler', Self.ClassName+' '+WM_To_String(TMessage(Message).Msg));
    if TMessage(Message).Msg=WM_SIZE then
     Colorizer.Utils.ProcessComponent(TColorizerLocalSettings.ColorMap, TColorizerLocalSettings.ActionBarStyle, Self);
  end;

  if not TColorizerLocalSettings.Unloading then
  begin
    if  not Assigned(TColorizerLocalSettings.Settings) or (Assigned(TColorizerLocalSettings.Settings) and not TColorizerLocalSettings.Settings.Enabled) or (csDesigning in Self.ComponentState)  then
    begin
     Trampoline_TWinControl_DefaultHandler(Self, Message);
     Exit;
    end;

    ApplyHook:=False;
    if (Self is TCustomForm) and (TColorizerLocalSettings.HookedWindows.IndexOf(Self.ClassName)>=0) then
      ApplyHook:=True;

    if not ApplyHook then
    begin
      LParentForm:= GetParentForm(Self);
      if not (Assigned(LParentForm) and Assigned(TColorizerLocalSettings.HookedWindows) and (TColorizerLocalSettings.HookedWindows.IndexOf(LParentForm.ClassName)>=0)) then
      begin
       Trampoline_TWinControl_DefaultHandler(Self, Message);
       exit;
      end
      else
       ApplyHook:=True;
    end;

    if ApplyHook then
    case TMessage(Message).Msg of
        //CN_CTLCOLOREDIT,
        //CN_CTLCOLORLISTBOX,
        CN_CTLCOLORMSGBOX..CN_CTLCOLORSTATIC:
        begin
          //if (GetTextColor(TMessage(Message).WParam)<>ColorToRGB(TColorizerLocalSettings.ColorMap.FontColor)) or (Self.Brush.Color<>TColorizerLocalSettings.ColorMap.MenuColor) then
          begin
            //AddLog('CustomDefaultHandler', 'Color Applied'+Self.ClassName);
            SetTextColor(TMessage(Message).WParam, ColorToRGB(TColorizerLocalSettings.ColorMap.FontColor));
            Self.Brush.Color:=TColorizerLocalSettings.ColorMap.WindowColor;
            SetBkColor(TMessage(Message).WParam, ColorToRGB(Self.Brush.Color));
            TMessage(Message).Result := Self.Brush.Handle;
            Exit;
          end;
        end;
    end;
  end;

  Trampoline_TWinControl_DefaultHandler(Self, Message);
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
          Self.Brush.Color:=TColorizerLocalSettings.ColorMap.WindowColor;
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
  if (Assigned(TColorizerLocalSettings.Settings) and not TColorizerLocalSettings.Settings.Enabled) or (csDesigning in Self.ComponentState) or (not Assigned(TColorizerLocalSettings.ColorMap)) then
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
    LBackgroundColor := Self.BackgroundColor;//TColorizerLocalSettings.ColorMap.Color;
    Brush.Color := LBackgroundColor;
    Pen.Width := 1;
    if TColorizerLocalSettings.Settings.TabIDECustom then
      Self.GetMemBitmap.Canvas.Pen.Color:= TryStrToColor(TColorizerLocalSettings.Settings.TabIDEOutLineColor, TColorizerLocalSettings.ColorMap.FrameTopLeftOuter)
    else
    Pen.Color := LBackgroundColor;
    LMemBitmapRect := Types.Rect(0, 0, Self.GetMemBitmap.Width, Self.GetMemBitmap.Height);
    Rectangle(LMemBitmapRect);
    DrawLine(Self.GetMemBitmap.Canvas, 0, YStart, TotalSize, YStart);
    if Self.TabPosition in [tpBottom, tpRight] then
      Inc(YStart)
    else
      Dec(YStart);

    if TColorizerLocalSettings.Settings.TabIDECustom then
      Self.GetMemBitmap.Canvas.Pen.Color:= TryStrToColor(TColorizerLocalSettings.Settings.TabIDEOutLineColor, TColorizerLocalSettings.ColorMap.FrameTopLeftOuter)
    else
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
      if TColorizerLocalSettings.Settings.TabIDECustom then
        Self.GetMemBitmap.Canvas.Pen.Color:= TryStrToColor(TColorizerLocalSettings.Settings.TabIDEOutLineColor, TColorizerLocalSettings.ColorMap.FrameTopLeftOuter)
      else
      Self.GetMemBitmap.Canvas.Pen.Color := TColorizerLocalSettings.ColorMap.FrameTopLeftOuter;

      DrawLine( Self.GetMemBitmap.Canvas, TabPos.StartPos - TabOffset + 1, TabTop, TabPos.StartPos - TabOffset + 1, TabTop + Self.TabHeight);

      if TColorizerLocalSettings.Settings.TabIDECustom then
        Self.GetMemBitmap.Canvas.Pen.Color:= TryStrToColor(TColorizerLocalSettings.Settings.TabIDEOutLineColor, TColorizerLocalSettings.ColorMap.FrameTopLeftOuter)
      else
      Self.GetMemBitmap.Canvas.Pen.Color := GetHighlightColor(Self.SelectedColor);

      DrawLine(Self.GetMemBitmap.Canvas, TabPos.StartPos - TabOffset + 2, TabTop, TabPos.StartPos - TabOffset + 2, TabTop + Self.TabHeight);

      if TColorizerLocalSettings.Settings.TabIDECustom then
        Self.GetMemBitmap.Canvas.Pen.Color:= TryStrToColor(TColorizerLocalSettings.Settings.TabIDEOutLineColor, TColorizerLocalSettings.ColorMap.FrameTopLeftOuter)
      else
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
      if TColorizerLocalSettings.Settings.TabIDECustom then
        Self.GetMemBitmap.Canvas.Pen.Color:= TryStrToColor(TColorizerLocalSettings.Settings.TabIDEOutLineColor, TColorizerLocalSettings.ColorMap.FrameTopLeftOuter)
      else
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
        Self.GetMemBitmap.Canvas.Font.Color := TColorizerLocalSettings.ColorMap.FontColor;//Self.Font.Color;
        TextRect(LRect, sText, [tfEndEllipsis, tfNoClip]);
      end;
    end;
  end;
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
  LCanvas : TCanvas;
  //LBrush : TBrush;
  LParentForm : TCustomForm;
  LStyleServices : {$IFDEF DELPHIXE2_UP}  TCustomStyleServices {$ELSE}TThemeServices{$ENDIF};
  Details:  TThemedElementDetails;
  sCaption: string;
  LRect : TRect;
  DC: HDC;
  LBuffer : TBitmap;
  lpPaint : TPaintStruct;
  LFontColor : TColor;

      procedure DrawControlText(Canvas: TCanvas; Details: TThemedElementDetails;
        const S: string; var R: TRect; Flags: Cardinal);
      var
        TextFormat: TTextFormatFlags;
      begin
        Canvas.Font := TWinControlClass(Self).Font;
        TextFormat := TTextFormatFlags(Flags);
        Canvas.Font.Color := LFontColor;
        LStyleServices.DrawText(Canvas.Handle, Details, S, R, TextFormat, Canvas.Font.Color);
      end;

      procedure PaintButton;
      begin
        {$IFDEF DELPHIXE2_UP}
        if TColorizerLocalSettings.Settings.UseVCLStyles and TColorizerLocalSettings.Settings.VCLStylesControls then
         LStyleServices:= ColorizerStyleServices
        else
         LStyleServices:= StyleServices;
        {$ELSE}
        LStyleServices :=ThemeServices
        {$ENDIF};
        DC := TWMPaint(Message).DC;
        LCanvas := TCanvas.Create;
        try
            if DC <> 0 then
              LCanvas.Handle := DC
            else
              LCanvas.Handle := BeginPaint(Self.Handle, LpPaint);
            //AddLog('Detour_TButtonControlClass_WndProc', IntToHex(DC, 8));
            if (DC = 0) then
            begin
              LBuffer := TBitmap.Create;
              try
                LRect:=Self.ClientRect;
                LBuffer.SetSize(Self.Width, Self.Height);
                if not TCustomButtonClass(Self).Enabled then
                begin
                  Details := LStyleServices.GetElementDetails(tbPushButtonDisabled);
                  LBuffer.Canvas.Brush.Color := TColorizerLocalSettings.ColorMap.Color;
                  LFontColor:=TColorizerLocalSettings.ColorMap.DisabledFontColor;
                end
                else if TCustomButtonClass(Self).MouseInClient then
                begin
                  Details := LStyleServices.GetElementDetails(tbPushButtonHot);
                  LBuffer.Canvas.Brush.Color := TColorizerLocalSettings.ColorMap.BtnSelectedColor;
                  LFontColor:=TColorizerLocalSettings.ColorMap.BtnSelectedFont;
                end
                else
                begin
                  Details := LStyleServices.GetElementDetails(tbPushButtonNormal);
                  LBuffer.Canvas.Brush.Color := TColorizerLocalSettings.ColorMap.Color;
                  LFontColor:=TColorizerLocalSettings.ColorMap.FontColor;
                end;
                {$IFDEF DELPHIXE2_UP}
                if TColorizerLocalSettings.Settings.UseVCLStyles and TColorizerLocalSettings.Settings.VCLStylesControls then
                begin
                  LBuffer.Canvas.Brush.Color := TColorizerLocalSettings.ColorMap.Color;
                  LBuffer.Canvas.FillRect(LRect);
                  LStyleServices.DrawElement(LBuffer.Canvas.Handle, Details, LRect);
                  LStyleServices.GetElementColor(Details, ecTextColor, LFontColor);
                end
                else
                {$ENDIF}
                begin
                  LBuffer.Canvas.Pen.Color   := TColorizerLocalSettings.ColorMap.FrameTopLeftOuter;
                  LBuffer.Canvas.Rectangle(LRect);
                end;

                sCaption:=TCustomButtonClass(Self).Caption;
                LRect:=TCustomButtonClass(Self).ClientRect;

                DrawControlText(LBuffer.Canvas, Details, sCaption, LRect, DT_VCENTER or DT_CENTER or DT_SINGLELINE);

                if Self is TWinControl then
                  TWinControlClass(Self).PaintControls(LBuffer.Canvas.Handle, nil);
                LCanvas.Draw(0, 0, LBuffer);
              finally
                LBuffer.Free;
              end;
            end;

        finally
          LCanvas.Handle := 0;
          LCanvas.Free;
          if DC = 0 then
            EndPaint(Self.Handle, LpPaint);
        end;

        Message.Result := 0;
      end;

begin
//  if (TButtonControl(Self) is TCustomCheckBox) and Assigned(TColorizerLocalSettings.Settings) and TColorizerLocalSettings.Settings.Enabled and not (csDesigning in Self.ComponentState) then
//  begin
//    LParentForm:= GetParentForm(Self);
//    if not (Assigned(LParentForm) and Assigned(TColorizerLocalSettings.HookedWindows) and (TColorizerLocalSettings.HookedWindows.IndexOf(LParentForm.ClassName)>=0)) then
//    begin
//      Trampoline_TButtonControl_WndProc(Self, Message);
//      exit;
//    end;
//
//    case Message.Msg of
//        CN_CTLCOLORSTATIC:
//        begin
//          LBrush := Self.Brush;
//          LBrush.Color:=TColorizerLocalSettings.ColorMap.Color;
//
//          SetTextColor(Message.wParam, ColorToRGB(TColorizerLocalSettings.ColorMap.FontColor));
//          SetBkColor(Message.wParam, ColorToRGB(LBrush.Color));
//          Message.Result := LRESULT(LBrush.Handle);
//          Exit;
//        end;
//    else
//       Trampoline_TButtonControl_WndProc(Self, Message);
//    end;
//  end
//  else
//  if (TButtonControl(Self) is TRadioButton) and Assigned(TColorizerLocalSettings.Settings) and TColorizerLocalSettings.Settings.Enabled and not (csDesigning in Self.ComponentState) then
//  begin
//    LParentForm:= GetParentForm(Self);
//    if not (Assigned(LParentForm) and Assigned(TColorizerLocalSettings.HookedWindows) and (TColorizerLocalSettings.HookedWindows.IndexOf(LParentForm.ClassName)>=0)) then
//    begin
//      Trampoline_TButtonControl_WndProc(Self, Message);
//      exit;
//    end;
//    case Message.Msg of
//        CN_CTLCOLORSTATIC:
//        begin
//          LBrush := Self.Brush;
//          LBrush.Color:=TColorizerLocalSettings.ColorMap.Color;
//
//          SetTextColor(Message.wParam, ColorToRGB(TColorizerLocalSettings.ColorMap.FontColor));
//          SetBkColor(Message.wParam, ColorToRGB(LBrush.Color));
//          Message.Result := LRESULT(LBrush.Handle);
//          Exit;
//        end;
//    else
//       Trampoline_TButtonControl_WndProc(Self, Message);
//    end;
//  end
//  else
  if (TButtonControl(Self) is TButton) and Assigned(TColorizerLocalSettings.Settings) and TColorizerLocalSettings.Settings.Enabled and not (csDesigning in Self.ComponentState) then
  begin

    LParentForm:= GetParentForm(Self);
    if not (Assigned(LParentForm) and Assigned(TColorizerLocalSettings.HookedWindows) and (TColorizerLocalSettings.HookedWindows.IndexOf(LParentForm.ClassName)>=0)) then
    begin
      Trampoline_TButtonControl_WndProc(Self, Message);
      exit;
    end;

    //AddLog('Detour_TButtonControlClass_WndProc', WM_To_String(TMessage(Message).Msg));
    case TMessage(Message).Msg of
       CN_NOTIFY     :
       begin
          Trampoline_TButtonControl_WndProc(Self, Message);
          InvalidateRect(Self.Handle, nil, False);
       end;

       WM_PAINT      : PaintButton();
       WM_ERASEBKGND : Message.Result := 1

      else
        Trampoline_TButtonControl_WndProc(Self, Message);
    end;
  end
  else
   Trampoline_TButtonControl_WndProc(Self, Message);
end;


//Hook for combobox
procedure Detour_TCustomComboBox_WMPaint(Self: TCustomComboBoxClass;var Message: TWMPaint);
var
   FListHandle : HWND;
   FEditHandle : HWND;
   {$IFDEF DELPHIXE2_UP}
   LStyleServices :  TCustomStyleServices;
   {$ENDIF}


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
    {$IFDEF DELPHIXE2_UP}
    BtnDrawState: TThemedComboBox;
    Details: TThemedElementDetails;
    DrawState: TThemedComboBox;
    {$ENDIF}
    Buffer: TBitmap;
  begin
    //if not StyleServices.Available then Exit;
    {$IFDEF DELPHIXE2_UP}
    BtnDrawState := tcDropDownButtonNormal;
    {$ENDIF}

    {$IFDEF DELPHIXE2_UP}
    if TColorizerLocalSettings.Settings.UseVCLStyles and TColorizerLocalSettings.Settings.VCLStylesControls then
    begin
      if not Self.Enabled then
        BtnDrawState := tcDropDownButtonDisabled
      else if Self.DroppedDown then
        BtnDrawState := tcDropDownButtonPressed
//      else if Self.FMouseOnButton then
//        BtnDrawState := tcDropDownButtonHot
      else
        BtnDrawState := tcDropDownButtonNormal;
  //
      if not Self.Enabled then
        DrawState := tcBorderDisabled
      else
      if Self.Focused then
        DrawState := tcBorderFocused
//      else if Self.MouseInControl then
//        DrawState := tcBorderHot
      else
        DrawState := tcBorderNormal;

      Details := StyleServices.GetElementDetails(DrawState);
    end;
    {$ENDIF}

    Buffer := TBitMap.Create;
    Buffer.SetSize(Self.Width, Self.Height);
    try
      R := Rect(0, 0, Buffer.Width, Buffer.Height);
      // draw border + client in buffer

      if (Self.Style = csSimple) and (FListHandle <> 0) then
      begin
        GetWindowRect(FListHandle, ListRect);
        GetWindowRect(Self.Handle, ControlRect);
        R.Bottom := ListRect.Top - ControlRect.Top;

        {$IFDEF DELPHIXE2_UP}
//        if TColorizerLocalSettings.Settings.UseVCLStyles and TColorizerLocalSettings.Settings.VCLStylesControls then
//         LStyleServices.DrawElement(Buffer.Canvas.Handle, Details, R)
//        else
        {$ENDIF}
        begin
          Buffer.Canvas.Pen.Color:=TColorizerLocalSettings.ColorMap.FrameTopLeftOuter;
          Buffer.Canvas.Brush.Style:=bsClear;
          Buffer.Canvas.Rectangle(R);
        end;

        R := Rect(0, Self.Height - (ControlRect.Bottom - ListRect.Bottom), Self.Width, Self.Height);
        with Buffer.Canvas do
        begin
          Brush.Style := bsSolid;
          Brush.Color := TColorizerLocalSettings.ColorMap.WindowColor;
          FillRect(R);
        end;
        R := Rect(0, 0, Buffer.Width, Buffer.Height);
        R.Bottom := ListRect.Top - ControlRect.Top;
      end
      else
      begin
        {$IFDEF DELPHIXE2_UP}
//        if TColorizerLocalSettings.Settings.UseVCLStyles and TColorizerLocalSettings.Settings.VCLStylesControls then
//         LStyleServices.DrawElement(Buffer.Canvas.Handle, Details, R)
//        else
        {$ENDIF}
        begin
          Buffer.Canvas.Brush.Style:=bsSolid;
          Buffer.Canvas.Pen.Color:=TColorizerLocalSettings.ColorMap.FrameTopLeftOuter;
          Buffer.Canvas.Brush.Color:=TColorizerLocalSettings.ColorMap.WindowColor;
          Buffer.Canvas.Rectangle(R);
        end;
      end;

      // draw button in buffer
      if Self.Style <> csSimple then
      begin
        R:=GetButtonRect;
        {$IFDEF DELPHIXE2_UP}
        if TColorizerLocalSettings.Settings.UseVCLStyles and TColorizerLocalSettings.Settings.VCLStylesControls then
        begin
          Details := LStyleServices.GetElementDetails(BtnDrawState);
          LStyleServices.DrawElement(Buffer.Canvas.Handle, Details, R);
        end
        else
        {$ENDIF}
        begin
          Buffer.Canvas.Brush.Style:=bsSolid;
          Buffer.Canvas.Pen.Color:=TColorizerLocalSettings.ColorMap.FrameTopLeftOuter;
          Buffer.Canvas.Brush.Color:=TColorizerLocalSettings.ColorMap.MenuColor;
          Buffer.Canvas.Rectangle(R);

          Buffer.Canvas.Pen.Color:=TColorizerLocalSettings.ColorMap.FontColor;
          DrawArrow(Buffer.Canvas,TScrollDirection.sdDown, Point( R.Left + ((R.Right - R.Left) Div 2)-4 , R.Top + ((R.Bottom - R.Top) Div 2) - 2) ,4);
        end;
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

  {$IFDEF DELPHIXE2_UP}
  if TColorizerLocalSettings.Settings.UseVCLStyles and TColorizerLocalSettings.Settings.VCLStylesControls then
   LStyleServices:= ColorizerStyleServices
  else
   LStyleServices:= StyleServices;
  {$ENDIF}

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


//hook for TCustomGroupBox
procedure Detour_TCustomGroupBox_Paint(Self : TCustomGroupBoxClass);
var
  H: Integer;
  R: TRect;
  Flags: Longint;
  LParentForm : TCustomForm;
  {$IFDEF DELPHIXE2_UP}
//  Box: TThemedButton;
//  CaptionRect, OuterRect: TRect;
//  Size: TSize;
//  LStyle: TCustomStyleServices;
//  Details: TThemedElementDetails;
  {$ENDIF}
begin
  if Assigned(TColorizerLocalSettings.Settings) and TColorizerLocalSettings.Settings.Enabled and not (csDesigning in Self.ComponentState) then
  begin
    LParentForm:= GetParentForm(Self);
    if not (Assigned(LParentForm) and Assigned(TColorizerLocalSettings.HookedWindows) and (TColorizerLocalSettings.HookedWindows.IndexOf(LParentForm.ClassName)>=0)) then
    begin
      Trampoline_TCustomGroupBox_Paint(Self);
      exit;
    end;

    Self.Canvas.Font := Self.Font;
    {$IFDEF DELPHIXE2_UP}
//    if ThemeControl(Self) and TColorizerLocalSettings.Settings.UseVCLStyles and TColorizerLocalSettings.Settings.VCLStylesControls then
//    begin
//       LStyle:= ColorizerStyleServices;
//      if Self.Text <> '' then
//      begin
//        GetTextExtentPoint32(Self.Handle, Self.Text, Length(Self.Text), Size);
//        CaptionRect := Rect(0, 0, Size.cx, Size.cy);
//        if not Self.UseRightToLeftAlignment then
//          OffsetRect(CaptionRect, 8, 0)
//        else
//          OffsetRect(CaptionRect, Self.Width - 8 - CaptionRect.Right, 0);
//      end
//      else
//        CaptionRect := Rect(0, 0, 0, 0);
//
//      OuterRect := Self.ClientRect;
//      OuterRect.Top := (CaptionRect.Bottom - CaptionRect.Top) div 2;
//      with CaptionRect do
//        ExcludeClipRect(Self.Handle, Left, Top, Right, Bottom);
//      if Self.Enabled then
//        Box := tbGroupBoxNormal
//      else
//        Box := tbGroupBoxDisabled;
//
//      Details := LStyle.GetElementDetails(Box);
//      LStyle.DrawElement(Self.Handle, Details, OuterRect);
//
//      SelectClipRgn(Self.Handle, 0);
//      Self.Canvas.Brush.Style := bsClear;
//      if Self.Text <> '' then
//        if Self.IsRightToLeft then
//        begin
//          Flags := Self.DrawTextBiDiModeFlags(DT_SINGLELINE);
//          LStyle.DrawText(Self.Handle, Details, Self.Text, CaptionRect, Flags, 0);
//        end
//        else
//          LStyle.DrawText(Self.Handle, Details, Self.Text, CaptionRect, [tfLeft]);
//    end
//    else
  {$ENDIF}
    begin
      H := Self.Canvas.TextHeight('0');
      R := Rect(0, H div 2 - 1, Self.Width, Self.Height);
      Self.Canvas.Brush.Color := TColorizerLocalSettings.ColorMap.FrameTopLeftOuter;//clWindowFrame;


      Self.Canvas.FrameRect(R);

      if Self.Text <> '' then
      begin
        if not Self.UseRightToLeftAlignment then
          R := Rect(8, 0, 0, H)
        else
          R := Rect(R.Right - Self.Canvas.TextWidth(Self.Text) - 8, 0, 0, H);
        Flags := Self.DrawTextBiDiModeFlags(DT_SINGLELINE);
        DrawText(Self.Handle, Self.Text, Length(Self.Text), R, Flags or DT_CALCRECT);
        Self.Canvas.Brush.Color := Self.Color;
        DrawText(Self.Handle, Self.Text, Length(Self.Text), R, Flags);
      end;

    end;

    //if Assigned(Self.OnPaint) then Self.OnPaint(Self);
  end
  else
    Trampoline_TCustomGroupBox_Paint(Self);
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
            //InflateRect(EmptyRect, -2, -2);
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

          LCanvas.Brush.Style:=bsClear;
          LCanvas.Pen.Color :=TColorizerLocalSettings.ColorMap.FrameTopLeftOuter;
          LCanvas.Rectangle(DrawRect);

          //LCanvas.Brush.Color:=TColorizerLocalSettings.ColorMap.FrameTopLeftOuter;
          //LCanvas.FillRect(DrawRect);
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

{ TBitBtnHelper }

function TBitBtnHelper.DrawItemAddress: Pointer;
var
  MethodAddr: procedure(const DrawItemStruct: TDrawItemStruct) of object;
begin
  MethodAddr := Self.DrawItem;
  Result     := TMethod(MethodAddr).Code;
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

function TCategoryButtonsHelper.InsertBottomHelper: TBaseItem;
begin
 Result:= Self.FInsertBottom;
end;

function TCategoryButtonsHelper.InsertLeftHelper: TBaseItem;
begin
 Result:= Self.FInsertLeft;
end;

function TCategoryButtonsHelper.InsertRightHelper: TBaseItem;
begin
 Result:= Self.FInsertRight;
end;

function TCategoryButtonsHelper.InsertTopHelper: TBaseItem;
begin
 Result:= Self.FInsertTop;
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


procedure Detour_TCategoryButtons_DrawButton(Self :TCategoryButtonsClass;const Button: TButtonItem; Canvas: TCanvas;
  Rect: TRect; State: TButtonDrawState);
var
  TextLeft, TextTop: Integer;
  RectHeight: Integer;
  ImgTop: Integer;
  TextOffset: Integer;
  FillColor: TColor;
  EdgeColor: TColor;
  InsertIndication: TRect;
  TextRect: TRect;
  OrgRect: TRect;
  Caption: string;
  {$IFDEF DELPHIXE2_UP}
  LStyle: TCustomStyleServices;
  LDetails: TThemedElementDetails;
  LColor: TColor;
  TxtColor: TColor;
  SaveIndex: Integer;
  {$ENDIF}
  FontColor: TColor;

  LParentForm : TCustomForm;
begin
  LParentForm:=GetParentForm(Self);

  if Assigned(TColorizerLocalSettings.Settings) and (TColorizerLocalSettings.Settings.Enabled)
    and Assigned(LParentForm) and  (TColorizerLocalSettings.HookedWindows.IndexOf(LParentForm.ClassName)>=0) {SameText(Self.ClassName, 'TIDECategoryButtons') and}
    and Assigned(TColorizerLocalSettings.ColorMap) then
  begin
    if Assigned(Self.OnDrawButton) and (not (csDesigning in Self.ComponentState)) then
      Self.OnDrawButton(Self, Button, Canvas, Rect, State)
    else
    begin
      {$IFDEF DELPHIXE2_UP}
      LStyle := ColorizerStyleServices;
      {$ENDIF}
      OrgRect := Rect;
      if Assigned(Self.OnBeforeDrawButton) then
        Self.OnBeforeDrawButton(Self, Button, Canvas, Rect, State);
      InflateRect(Rect, -1, -1);

      FontColor := Self.Font.Color;

      if bdsHot in State then
      begin
        FillColor := TColorizerLocalSettings.ColorMap.HotColor;//Self.HotButtonColor;
        if bdsSelected in State then
          FillColor := TColorizerLocalSettings.ColorMap.HotColor;//GetShadowColor(FillColor, -10);
        EdgeColor := TColorizerLocalSettings.ColorMap.FrameTopLeftOuter;//GetShadowColor(FillColor);
        FontColor := TColorizerLocalSettings.ColorMap.HotFontColor;
       {$IFDEF DELPHIXE2_UP}
        if TColorizerLocalSettings.Settings.UseVCLStyles and TColorizerLocalSettings.Settings.VCLStylesControls and TColorizerLocalSettings.Settings.VCLStylesMenusColors then
        begin
          LDetails := LStyle.GetElementDetails(tcbButtonHot);
          if LStyle.GetElementColor(LDetails, ecTextColor, LColor) and (LColor <> clNone) then
            FontColor := LColor;
          if LStyle.GetElementColor(LDetails, ecBorderColor, LColor) and (LColor <> clNone) then
            EdgeColor := LColor;
        end;
       {$ENDIF}
      end
      else
      if bdsSelected in State then
      begin
        FillColor := TColorizerLocalSettings.ColorMap.Color; //Self.SelectedButtonColor;
        EdgeColor := TColorizerLocalSettings.ColorMap.FrameTopLeftOuter; //GetShadowColor(FillColor);
        FontColor := TColorizerLocalSettings.ColorMap.FontColor;
        {$IFDEF DELPHIXE2_UP}
        if TColorizerLocalSettings.Settings.UseVCLStyles and TColorizerLocalSettings.Settings.VCLStylesControls and TColorizerLocalSettings.Settings.VCLStylesMenusColors then
        begin
          LDetails := LStyle.GetElementDetails(tcbButtonSelected);
          if LStyle.GetElementColor(LDetails, ecTextColor, LColor) and (LColor <> clNone) then
            FontColor := LColor;
          if LStyle.GetElementColor(LDetails, ecBorderColor, LColor) and (LColor <> clNone) then
            EdgeColor := LColor;
        end;
        {$ENDIF}
      end
      else
      begin
        FillColor := TColorizerLocalSettings.ColorMap.Color;// Self.RegularButtonColor;
        if (bdsFocused in State) then
          EdgeColor := TColorizerLocalSettings.ColorMap.HighlightColor //GetShadowColor(Self.SelectedButtonColor)
        else
          EdgeColor := TColorizerLocalSettings.ColorMap.FrameTopLeftOuter; //GetShadowColor(FillColor);
        {$IFDEF DELPHIXE2_UP}
        if TColorizerLocalSettings.Settings.UseVCLStyles and TColorizerLocalSettings.Settings.VCLStylesControls and TColorizerLocalSettings.Settings.VCLStylesMenusColors then
        begin
          LDetails := LStyle.GetElementDetails(tcbButtonNormal);
          if LStyle.GetElementColor(LDetails, ecTextColor, LColor) and (LColor <> clNone) then
            FontColor := LColor;
          if LStyle.GetElementColor(LDetails, ecBorderColor, LColor) and (LColor <> clNone) then
            EdgeColor := LColor;
        end;
        {$ENDIF}
      end;

//      if IsStyleEnabled and TStyleManager.IsCustomStyleActive and not (seFont in StyleElements) then
//        FontColor := Font.Color;

      Canvas.Font.Color  := FontColor;
      Canvas.Brush.Color := FillColor;

      {$IFDEF DELPHIXE2_UP}
      if TColorizerLocalSettings.Settings.UseVCLStyles and TColorizerLocalSettings.Settings.VCLStylesControls and TColorizerLocalSettings.Settings.VCLStylesMenusColors then
      begin
        SaveIndex := SaveDC(Canvas.Handle);
        try
          LStyle.DrawElement(Canvas.Handle, LDetails, Rect);
        finally
          RestoreDC(Canvas.Handle, SaveIndex);
        end;
      end
      else
      {$ENDIF}
      if FillColor <> clNone then
      begin
        Canvas.FillRect(Rect);
        Canvas.Brush.Color := EdgeColor;
        Canvas.FrameRect(Rect);
      end;

      if bdsFocused in State then
      begin
        InflateRect(Rect, -1, -1);
        Canvas.FrameRect(Rect);
      end;

      Canvas.Brush.Color := FillColor;

      TextLeft := Rect.Left + 4;
      RectHeight := Rect.Bottom - Rect.Top;
      TextTop := Rect.Top + (RectHeight - Canvas.TextHeight('Wg')) div 2;

      if boFullSize in Self.ButtonOptions then
        Inc(TextLeft, 4);

      if TextTop < Rect.Top then
        TextTop := Rect.Top;
      if bdsDown in State then
      begin
        Inc(TextTop);
        Inc(TextLeft);
      end;

      TextOffset := 0;
      if Assigned(Self.OnDrawIcon) then
        Self.OnDrawIcon(Self, Button, Canvas, OrgRect, State, TextOffset)
      else if (Self.Images <> nil) and (Button.ImageIndex > -1) and
          (Button.ImageIndex < Self.Images.Count) then
      begin
        ImgTop := Rect.Top + (RectHeight - Self.Images.Height) div 2;
        if ImgTop < Rect.Top then
          ImgTop := Rect.Top;
        if bdsDown in State then
          Inc(ImgTop);
        Self.Images.Draw(Canvas, TextLeft - 1, ImgTop, Button.ImageIndex);
        TextOffset := Self.Images.Width + 1;
      end;

      if [bdsInsertLeft, bdsInsertTop, bdsInsertRight, bdsInsertBottom] * State <> [] then
      begin
        Canvas.Brush.Color := TColorizerLocalSettings.ColorMap.FrameTopLeftOuter; //GetShadowColor(EdgeColor);
        InsertIndication := Rect;
        if bdsInsertLeft in State then
        begin
          Dec(InsertIndication.Left, 2);
          InsertIndication.Right := InsertIndication.Left + 2;
        end
        else if bdsInsertTop in State then
        begin
          Dec(InsertIndication.Top);
          InsertIndication.Bottom := InsertIndication.Top + 2;
        end
        else if bdsInsertRight in State then
        begin
          Inc(InsertIndication.Right, 2);
          InsertIndication.Left := InsertIndication.Right - 2;
        end
        else if bdsInsertBottom in State then
        begin
          Inc(InsertIndication.Bottom);
          InsertIndication.Top := InsertIndication.Bottom - 2;
        end;
        Canvas.FillRect(InsertIndication);
        Canvas.Brush.Color := FillColor;
      end;

      if boShowCaptions in Self.ButtonOptions then
      begin
        if FillColor = clNone then
          Canvas.Brush.Style := bsClear;

        Inc(TextLeft, TextOffset);
        TextRect.Left := TextLeft;
        TextRect.Right := Rect.Right - 2;
        TextRect.Top := TextTop;
        TextRect.Bottom := Rect.Bottom - 2;

        if Assigned(Self.OnDrawText) then
        begin
         {$IFDEF DELPHIXE2_UP}
          if TColorizerLocalSettings.Settings.UseVCLStyles and TColorizerLocalSettings.Settings.VCLStylesControls and TColorizerLocalSettings.Settings.VCLStylesMenusColors then
            Canvas.Brush.Style := bsClear;
         {$ENDIF}
          Self.OnDrawText(Self, Button, Canvas, TextRect, State)
        end
        else
        begin
          Caption := Button.Caption;
          {$IFDEF DELPHIXE2_UP}
          if TColorizerLocalSettings.Settings.UseVCLStyles and TColorizerLocalSettings.Settings.VCLStylesControls and TColorizerLocalSettings.Settings.VCLStylesMenusColors then
          begin
            Canvas.Brush.Style := bsClear;
            if LStyle.GetElementColor(LDetails, ecTextColor, TxtColor) then
              Canvas.Font.Color := TxtColor;

            Canvas.TextRect(TextRect, Caption, [tfEndEllipsis, tfVerticalCenter]);
            Canvas.Brush.Style := bsSolid;
          end
          else
          {$ENDIF}
          begin
            Canvas.Brush.Style := bsClear;
            Canvas.TextRect(TextRect, Caption, [tfEndEllipsis, tfVerticalCenter]);
          end;
        end;
      end;

      if Assigned(Self.OnAfterDrawButton) then
        Self.OnAfterDrawButton(Self, Button, Canvas, OrgRect, State);
    end;
    Canvas.Brush.Color := Self.Color;
  end
  else
   Trampoline_TCategoryButtons_DrawButton(Self, Button, Canvas, Rect, State);
end;

procedure Detour_TCategoryButtons_DrawCategory(Self :TCategoryButtonsClass; const Category: TButtonCategory; const Canvas: TCanvas; StartingPos: Integer);
const
  cDropDownSize = 13;
{$IFDEF DELPHIXE2_UP}
var
  LStyleServices : TCustomStyleServices;
  LDetails       : TThemedElementDetails;
{$ENDIF}

  procedure DrawDropDownButton(X, Y: Integer; Collapsed, Selected: Boolean);
  const
    ChevronDirection: array[Boolean] of TScrollDirection = (sdDown, sdRight);
    {$IFDEF DELPHIXE2_UP}
    Elements: array[Boolean] of TThemedCategoryButtons = (tcbCategoryChevronOpened, tcbCategoryChevronClosed);
    {$ENDIF}
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

      {$IFDEF DELPHIXE2_UP}
      if TColorizerLocalSettings.Settings.UseVCLStyles and TColorizerLocalSettings.Settings.VCLStylesControls and TColorizerLocalSettings.Settings.VCLStylesMenusColors then
      begin
       if not Collapsed then
        LDetails := LStyleServices.GetElementDetails(tcbCategoryGlyphOpened)
       else
        LDetails := LStyleServices.GetElementDetails(tcbCategoryGlyphClosed);
        LStyleServices.DrawElement(Canvas.Handle, LDetails, Rect(X, Y, X+Width, Y+Height));
//         LBuffer:=TBitmap.Create;
//         try
//           LBuffer.SetSize(Width, Height);
//           LRect := Rect(0, 0, Width, Height);
//           LStyleServices.DrawElement(LBuffer.Canvas.Handle, LDetails, LRect);
//           BitBlt(Canvas.Handle, X, Y, Width, Height, LBuffer.Canvas.Handle, 0, 0, SRCCOPY);
//         finally
//           LBuffer.Free;
//         end;
      end
      else
      {$ENDIF}
      begin
        if Selected then
        begin
          Canvas.Pen.Color   := TColorizerLocalSettings.ColorMap.HotFontColor;
          Canvas.Brush.Color := TColorizerLocalSettings.ColorMap.HotColor;
        end
        else
        begin
          Canvas.Pen.Color   := TColorizerLocalSettings.ColorMap.FontColor;
          Canvas.Brush.Color := TColorizerLocalSettings.ColorMap.Color;
        end;

        Canvas.Rectangle(X, Y, X + Width, Y + Height);
        Canvas.MoveTo(X + 2, Y + Width div 2);
        Canvas.LineTo(X + Width - 2, Y + Width div 2);

        if Collapsed then
        begin
          Canvas.MoveTo(X + Width div 2, Y + 2);
          Canvas.LineTo(X + Width div 2, Y + Width - 2);
        end;
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
  {$IFDEF DELPHIXE2_UP}LColor, {$ENDIF}FontColor, GradientColor, SourceColor, TempColor: TColor;
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
    FontColor   := TColorizerLocalSettings.ColorMap.FontColor;

    {$IFDEF DELPHIXE2_UP}
    if TColorizerLocalSettings.Settings.UseVCLStyles and TColorizerLocalSettings.Settings.VCLStylesControls and TColorizerLocalSettings.Settings.VCLStylesMenusColors then
    begin
      LStyleServices:= ColorizerStyleServices;
      if Self.SelectedItem = Category then
        LDetails := LStyleServices.GetElementDetails(tcbCategorySelected)
      else if Category.Color <> clNone then
        LDetails := LStyleServices.GetElementDetails(tcbCategoryNormal)
      else
        LDetails := LStyleServices.GetElementDetails(tcbBackground);

      if LStyleServices.GetElementColor(LDetails, ecGradientColor1, LColor) and (LColor <> clNone) then
        SourceColor := LColor
      else
        SourceColor := clNone;
    end
    else
   {$ENDIF}
      SourceColor := clNone;

    if SourceColor = clNone then
    begin
      if (Self.SelectedItem = Category) and (Self.SelectedButtonColor <> clNone) then
      begin
        SourceColor := TColorizerLocalSettings.ColorMap.HotColor;//Self.SelectedButtonColor
        FontColor   := TColorizerLocalSettings.ColorMap.HotFontColor;
      end
      else
      if Category.Color <> clNone then
        SourceColor := TColorizerLocalSettings.ColorMap.Color//Category.Color
      else
        SourceColor := TColorizerLocalSettings.ColorMap.MenuColor;//Self.Color;
    end;

    CategoryFrameBounds := CategoryBounds;
    Self.AdjustCategoryBoundsHelper(Category, CategoryFrameBounds);
    if boCaptionOnlyBorder in Self.ButtonOptions then
      CategoryRealBounds := CategoryFrameBounds
    else
      CategoryRealBounds := CategoryBounds;

    if (Self.SelectedItem <> Category) and (boGradientFill in Self.ButtonOptions) then
    begin
      GradientColor := TColorizerLocalSettings.ColorMap.MenuColor;
      GradientFillCanvas(Canvas, SourceColor, GradientColor, CategoryRealBounds, Self.GradientDirection);
    end
    else
    begin
      {$IFDEF DELPHIXE2_UP}
      if TColorizerLocalSettings.Settings.UseVCLStyles and TColorizerLocalSettings.Settings.VCLStylesControls and TColorizerLocalSettings.Settings.VCLStylesMenusColors and
         LStyleServices.GetElementColor(LDetails, ecGradientColor2, LColor) and (LColor <> clNone) then
        GradientFillCanvas(Canvas, SourceColor, LColor, CategoryRealBounds, Self.GradientDirection)
      else
      {$ENDIF}
      begin
        Canvas.Brush.Color := SourceColor;
        Canvas.FillRect(CategoryRealBounds)
      end;
    end;

    with CategoryRealBounds do
    begin
      Right := Right - 1;

      {$IFDEF DELPHIXE2_UP}
      if TColorizerLocalSettings.Settings.UseVCLStyles and TColorizerLocalSettings.Settings.VCLStylesControls and TColorizerLocalSettings.Settings.VCLStylesMenusColors then
      begin
        LDetails := LStyleServices.GetElementDetails(tcbBackground);
        if LStyleServices.GetElementColor(LDetails, ecFillColor, LColor) and (LColor <> clNone) then
          TempColor := LColor
        else
          TempColor := Self.Color;
      end
      else
      {$ENDIF}
      TempColor := TColorizerLocalSettings.ColorMap.WindowColor;//Self.Color;

      Canvas.Pixels[Left, Top] := TempColor;
      Canvas.Pixels[Left+1, Top] := TempColor;
      Canvas.Pixels[Left, Top+1] := TempColor;

      Canvas.Pixels[Left, Bottom] := TempColor;
      Canvas.Pixels[Left+1, Bottom] := TempColor;
      Canvas.Pixels[Left, Bottom-1] := TempColor;

      if Self.BackgroundGradientColor <> clNone then
      {$IFDEF DELPHIXE2_UP}
      if TColorizerLocalSettings.Settings.UseVCLStyles and TColorizerLocalSettings.Settings.VCLStylesControls and TColorizerLocalSettings.Settings.VCLStylesMenusColors then
      begin
        LDetails := LStyleServices.GetElementDetails(tcbBackground);
        if TStyleManager.IsCustomStyleActive and LStyleServices.GetElementColor(LDetails, ecFillColor, LColor) and
           (LColor <> clNone)
        then
          TempColor := LColor
        else
        if LStyleServices.GetElementColor(LDetails, ecGradientColor1, LColor) and (LColor <> clNone) then
          TempColor := LColor
        else
          TempColor := Self.BackgroundGradientColor;
      end
      else
      {$ENDIF}
        TempColor := Self.BackgroundGradientColor;

      Canvas.Pixels[Right, Top] := TempColor;
      Canvas.Pixels[Right-1, Top] := TempColor;
      Canvas.Pixels[Right, Top+1] := TempColor;

      Canvas.Pixels[Right, Bottom] := TempColor;
      Canvas.Pixels[Right-1, Bottom] := TempColor;
      Canvas.Pixels[Right, Bottom-1] := TempColor;

      Canvas.Pen.Color := TColorizerLocalSettings.ColorMap.FrameTopLeftOuter;
      {$IFDEF DELPHIXE2_UP}
      if TColorizerLocalSettings.Settings.UseVCLStyles and TColorizerLocalSettings.Settings.VCLStylesControls and TColorizerLocalSettings.Settings.VCLStylesMenusColors then
      begin
        if Self.SelectedItem = Category then
          LDetails := LStyleServices.GetElementDetails(tcbCategorySelected)
        else
          LDetails := LStyleServices.GetElementDetails(tcbCategoryNormal);
        if LStyleServices.GetElementColor(LDetails, ecBorderColor, LColor) and (LColor <> clNone) then
          Canvas.Pen.Color := LColor;
      end;
      {$ENDIF}

      Canvas.Polyline([Point(Left + 2, Top), Point(Right - 2, Top), Point(Right, Top + 2),
        Point(Right, Bottom - 2), Point(Right - 2, Bottom), Point(Left + 2, Bottom), Point(Left, Bottom - 2),
        Point(Left, Top + 2), Point(Left + 2, Top)]);
    end;

    if ((Category.Collapsed) and (Self.SelectedItem <> nil) and
       (Self.CurrentCategory = Category)) or (Self.SelectedItem = Category) then
    begin
      {$IFDEF DELPHIXE2_UP}
      if TColorizerLocalSettings.Settings.UseVCLStyles and TColorizerLocalSettings.Settings.VCLStylesControls and TColorizerLocalSettings.Settings.VCLStylesMenusColors and
       LStyleServices.GetElementColor(LDetails, ecEdgeShadowColor, LColor) and (LColor <> clNone) then
        Canvas.Brush.Color := LColor
      else
      {$ENDIF}
      Canvas.Brush.Color := TColorizerLocalSettings.ColorMap.FrameTopLeftOuter;
      with CategoryFrameBounds do
        Canvas.FrameRect(Rect(Left + 1, Top + 1, Right, Bottom));
    end;

    ChevronBounds := Self.GetChevronBoundsHelper(CategoryRealBounds);

    if (Category.Items <> nil) and (Category.Items.Count > 0) then
      DrawDropDownButton(ChevronBounds.Left, ChevronBounds.Top, Category.Collapsed, ((Self.SelectedItem = Category) and (Self.SelectedButtonColor <> clNone)));

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
    Canvas.Font.Color  := FontColor;
    {$IFDEF DELPHIXE2_UP}
    if TColorizerLocalSettings.Settings.UseVCLStyles and TColorizerLocalSettings.Settings.VCLStylesControls and TColorizerLocalSettings.Settings.VCLStylesMenusColors then
    begin
      if Self.SelectedItem = Category then
        LDetails := LStyleServices.GetElementDetails(tcbCategorySelected)
      else
        LDetails := LStyleServices.GetElementDetails(tcbCategoryNormal);
    end;
   {$ENDIF}


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

          if Button = Self.InsertTopHelper then
            Include(DrawState, bdsInsertTop)
          else if Button = Self.InsertBottomHelper then
            Include(DrawState, bdsInsertBottom)
          else if Button = Self.InsertRightHelper then
            Include(DrawState, bdsInsertRight)
          else if Button = Self.InsertLeftHelper then
            Include(DrawState, bdsInsertLeft);

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

procedure DoDrawGrayImage(hdcDst: HDC; himl: HIMAGELIST; ImageIndex, X, Y: Integer);
var
  pimldp: TImageListDrawParams;
begin
  FillChar(pimldp, SizeOf(pimldp), #0);
  pimldp.fState := ILS_SATURATE;
  pimldp.cbSize := SizeOf(pimldp);
  pimldp.hdcDst := hdcDst;
  pimldp.himl := himl;
  pimldp.i := ImageIndex;
  pimldp.x := X;
  pimldp.y := Y;
  ImageList_DrawIndirect(@pimldp);
end;


procedure Detour_TCustomImageList_DoDraw(Self: TObject; Index: Integer; Canvas: TCanvas; X, Y: Integer; Style: Cardinal; Enabled: Boolean);
var
  LImageList : TCustomImageListClass;
{$IFDEF DELPHIXE7_UP}
  LMask : TBitmap;
  LImage : TBitmap;

    procedure GetImages(Index: Integer; Image, Mask: TBitmap);
    var
      R: TRect;
    begin
      R := Rect(0, 0, LImageList.Width, LImageList.Height);
      with Image.Canvas do
      begin
        Brush.Color := TColorizerLocalSettings.ColorMap.WindowColor;
        FillRect(R);
        ImageList_Draw(LImageList.Handle, Index, Handle, 0, 0, ILD_NORMAL);
      end;
      with Mask.Canvas do
      begin
        Brush.Color := clWhite;
        FillRect(R);
        ImageList_Draw(LImageList.Handle, Index, Handle, 0, 0, ILD_MASK);
      end;
    end;

{$ENDIF}


begin

  if Assigned(TColorizerLocalSettings.Settings) and TColorizerLocalSettings.Settings.Enabled then
    begin
      LImageList:=TCustomImageListClass(Self);
      if not LImageList.HandleAllocated then Exit;
      if Enabled  then
      begin
        {$IFDEF DELPHIXE7_UP}
        if not EnableStockHook then
          TrampolineCustomImageList_DoDraw(Self, Index, Canvas, X, Y, Style, Enabled)
        else
        begin
            LImage := TBitmap.Create;
            try
              LMask := TBitmap.Create;
              try
                LImage.SetSize(LImageList.Width, LImageList.Height);
                LMask.SetSize(LImageList.Width, LImageList.Height);
                GetImages(Index, LImage, LMask);
                BitBlt(Canvas.Handle, X, Y, LImageList.Width, LImageList.Height, LImage.Canvas.Handle, 0, 0, SRCCOPY);
              finally
                LMask.Free;
              end;
            finally
              LImage.Free;
            end;
        end;
        {$ELSE}
         TrampolineCustomImageList_DoDraw(Self, Index, Canvas, X, Y, Style, Enabled);
        {$ENDIF}
      end
      else
      begin
        if TColorizerLocalSettings.Settings.FixIDEDisabledIconsDraw  then
         DoDrawGrayImage(Canvas.Handle, LImageList.Handle, Index, X, Y)
        else
         TrampolineCustomImageList_DoDraw(Self, Index, Canvas, X, Y, Style, Enabled);
      end;
    end
  else
    TrampolineCustomImageList_DoDraw(Self, Index, Canvas, X, Y, Style, Enabled);
end;

//Retuns the current Gutter color , using the background of the current syntax highlighter
function GetGutterBkColor : TColor;
var
  ATheme : TIDETheme;
  sColor : string;
begin
  if FGutterBkColor<>clNone then
   Exit(FGutterBkColor)
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
      Exit(TColorizerLocalSettings.ColorMap.Color)
    else
      Exit(clBtnFace)
  end;
end;

procedure  Detour_TCanvas_Rectangle(Self: TCanvas; X1, Y1, X2, Y2: Integer);
begin
  Trampoline_TCanvas_Rectangle(Self, X1, Y1, X2, Y2);
end;

//procedure Detour_TCanvas_Polygon(Self: TCanvas;const Points: array of TPoint);
//begin
//  Trampoline_TCanvas_Polygon(Self, Points);
//end;
//
//procedure Detour_TCanvas_Polyline(Self: TCanvas;const Points: array of TPoint);
//begin
//  Trampoline_TCanvas_Polyline(Self, Points);
//end;

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
 sEditorControlSignature             = 'EditorControl.TCustomEditControl.EVFillGutter';
 sGradientTabsSignature              = 'GDIPlus.GradientTabs.TGradientTabSet.DrawTabsToMemoryBitmap';
 sBaseVirtualTreePaintTreeSignature  = 'IDEVirtualTrees.TBaseVirtualTree.PaintTree';

{$IFDEF DELPHIXE7_UP}
 //used to paint background of MultiView related combobox (2)
 //sTCustomComboBoxDrawItemSignature   = 'Vcl.StdCtrls.TCustomComboBox.DrawItem';
{$ENDIF}
var
  sCaller : string;
  OrgBrush : Integer; //don't use SaveDC
//  Stopwatch: TStopwatch;
//  Elapsed: TTimeSpan;
begin
  OrgBrush:=Self.Brush.Color;
  try                                                                                                                    //sGradientTabsSignature                                              //gutter
   if Assigned(TColorizerLocalSettings.Settings) and TColorizerLocalSettings.Settings.Enabled  {$IFDEF DELPHIXE8_UP} and ((OrgBrush=TColorizerLocalSettings.ModernTheme.MainToolBarTColor) or (OrgBrush=clBtnFace) ) {$ELSE} and  (OrgBrush=clBtnFace) {$ENDIF} then
   begin
     //Stopwatch := TStopwatch.StartNew;
     sCaller := ProcByLevel(1);
     //Elapsed := Stopwatch.Elapsed;
     //AddLog2(Format('ProcByLevel(1) Elapsed %n ms ',[elapsed.TotalMilliseconds]));
//      if SameText(sCaller, sGradientTabsSignature) then
//        AddLog2(Format('ProcByLevel(1) sCaller %s OrgBrush %x  MainToolBarTColor %x',[sCaller, OrgBrush, TColorizerLocalSettings.ModernTheme.MainToolBarTColor]));

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
   end
   else
   if Assigned(TColorizerLocalSettings.Settings) and TColorizerLocalSettings.Settings.Enabled and  (OrgBrush=clWindow) then
   begin
      sCaller := ProcByLevel(2);
      if SameText(sCaller, sBaseVirtualTreePaintTreeSignature) then
         Self.Brush.Color:= TColorizerLocalSettings.ColorMap.WindowColor
      {$IFDEF DELPHIXE7_UP}
//      else
//      if SameText(sCaller, sTCustomComboBoxDrawItemSignature) then
//         Self.Brush.Color:= TColorizerLocalSettings.ColorMap.WindowColor;
      {$ENDIF}
   end;
   //Self.Brush.Color:=clred;
   Trampoline_TCanvas_FillRect(Self, Rect);
  finally
   Self.Brush.Color:=OrgBrush;
  end;
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
  LColor1, LColor2  : TColor;
  {$IFDEF DELPHIXE2_UP}
  LStyleServices :  TCustomStyleServices;
  LDetails : TThemedElementDetails;
  {$ENDIF}
begin
   if EnableHookTBitBtn then
   begin
    if TColorizerLocalSettings.Settings.UseVCLStyles and TColorizerLocalSettings.Settings.VCLStylesControls then
      Exit(ColorizerStyleServices.DrawElement(DC, Details, R, ClipRect))
    else
    begin
       SaveIndex := SaveDC(DC);
       LCanvas:=TCanvas.Create;
       try
         LCanvas.Handle:=DC;
         LCanvas.Brush.Color:=TColorizerLocalSettings.ColorMap.Color;
         LCanvas.Pen.Color:=TColorizerLocalSettings.ColorMap.FrameTopLeftOuter;
         LCanvas.Rectangle(R);
       finally
          LCanvas.Handle:=0;
          LCanvas.Free;
          RestoreDC(DC, SaveIndex);
       end;
       Exit(True);
    end;
   end
   else
   if Assigned(TColorizerLocalSettings.Settings) and TColorizerLocalSettings.Settings.Enabled and Assigned(TColorizerLocalSettings.ColorMap) and (Details.Element = teHeader) {and (Details.Part=HP_HEADERITEMRIGHT) } then
   begin
    sCaller := ProcByLevel(2);
    if SameText(sCaller, sTVirtualTreeColumnsSignature) then
    begin
       SaveIndex := SaveDC(DC);
       LCanvas:=TCanvas.Create;
       try
         LCanvas.Handle:=DC;
         {$IFDEF DELPHIXE2_UP}
         if TColorizerLocalSettings.Settings.UseVCLStyles and TColorizerLocalSettings.Settings.VCLStylesControls then
         begin
           LStyleServices:= ColorizerStyleServices;
           LDetails := LStyleServices.GetElementDetails(thHeaderItemNormal);
           LStyleServices.DrawElement(LCanvas.Handle, LDetails, R, ClipRect);
         end
         else
         {$ENDIF}
         begin
           if TColorizerLocalSettings.Settings.HeaderCustom  then
           begin
             LColor1:= TryStrToColor(TColorizerLocalSettings.Settings.HeaderStartGrad, TColorizerLocalSettings.ColorMap.Color);
             LColor2:= TryStrToColor(TColorizerLocalSettings.Settings.HeaderEndGrad, TColorizerLocalSettings.ColorMap.MenuColor);
           end
           else
           begin
             LColor1:= TColorizerLocalSettings.ColorMap.Color;
             LColor2:= TColorizerLocalSettings.ColorMap.MenuColor;
           end;

           GradientFillCanvas(LCanvas, LColor1, LColor2, R, gdVertical);
           LCanvas.Brush.Style:=TBrushStyle.bsClear;

           if TColorizerLocalSettings.Settings.HeaderCustom  then
            LCanvas.Pen.Color:= TryStrToColor(TColorizerLocalSettings.Settings.HeaderBorderColor, TColorizerLocalSettings.ColorMap.FrameTopLeftOuter)
           else
            LCanvas.Pen.Color:=TColorizerLocalSettings.ColorMap.FrameTopLeftOuter;
           LCanvas.Rectangle(R);
         end;
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
  LColor1, LColor2  : TColor;
begin
   if Assigned(TColorizerLocalSettings.Settings) and TColorizerLocalSettings.Settings.Enabled and Assigned(TColorizerLocalSettings.ColorMap) and (Details.Element = teHeader) {and (Details.Part=HP_HEADERITEMRIGHT) } then
   begin
    sCaller := ProcByLevel(2);
    if SameText(sCaller, sTVirtualTreeColumnsSignature) then
    begin
       SaveIndex := SaveDC(DC);
       LCanvas:=TCanvas.Create;
       try
         LCanvas.Handle:=DC;
         if TColorizerLocalSettings.Settings.HeaderCustom  then
         begin
           LColor1:= TryStrToColor(TColorizerLocalSettings.Settings.HeaderStartGrad, TColorizerLocalSettings.ColorMap.Color);
           LColor2:= TryStrToColor(TColorizerLocalSettings.Settings.HeaderEndGrad, TColorizerLocalSettings.ColorMap.MenuColor);
         end
         else
         begin
           LColor1:= TColorizerLocalSettings.ColorMap.Color;
           LColor2:= TColorizerLocalSettings.ColorMap.MenuColor;
         end;

          GradientFillCanvas(LCanvas, LColor1, LColor2, R, gdVertical);
          LCanvas.Brush.Style:=TBrushStyle.bsClear;
          if TColorizerLocalSettings.Settings.HeaderCustom  then
            LCanvas.Pen.Color:= TryStrToColor(TColorizerLocalSettings.Settings.HeaderBorderColor, TColorizerLocalSettings.ColorMap.FrameTopLeftOuter)
          else
           LCanvas.Pen.Color:=TColorizerLocalSettings.ColorMap.FrameTopLeftOuter;
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
  LColor1, LColor2  : TColor;
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
         if TColorizerLocalSettings.Settings.HeaderCustom  then
         begin
           LColor1:= TryStrToColor(TColorizerLocalSettings.Settings.HeaderStartGrad, TColorizerLocalSettings.ColorMap.Color);
           LColor2:= TryStrToColor(TColorizerLocalSettings.Settings.HeaderEndGrad, TColorizerLocalSettings.ColorMap.MenuColor);
         end
         else
         begin
           LColor1:= TColorizerLocalSettings.ColorMap.Color;
           LColor2:= TColorizerLocalSettings.ColorMap.MenuColor;
         end;

          GradientFillCanvas(LCanvas, LColor1, LColor2, pRect, gdVertical);
          LCanvas.Brush.Style:=TBrushStyle.bsClear;
          if TColorizerLocalSettings.Settings.HeaderCustom  then
            LCanvas.Pen.Color:= TryStrToColor(TColorizerLocalSettings.Settings.HeaderBorderColor, TColorizerLocalSettings.ColorMap.FrameTopLeftOuter)
          else
           LCanvas.Pen.Color:=TColorizerLocalSettings.ColorMap.FrameTopLeftOuter;
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
//function Detour_TStyleEngine_HandleMessage(Self: TStyleEngine; Control: TWinControl; var Message: TMessage; DefWndProc: TWndMethod): Boolean;
//begin
//  if Assigned(TColorizerLocalSettings.Settings) and TColorizerLocalSettings.Settings.Enabled and Assigned(TColorizerLocalSettings.Settings) and TColorizerLocalSettings.Settings.UseVCLStyles then
//  begin
//    Result:=False;
//    if not Assigned(Control) then exit;
//    if csDesigning in Control.ComponentState then  exit;
//  end;
//  Result:=Trampoline_TStyleEngine_HandleMessage(Self, Control, Message, DefWndProc);
//end;
{$ENDIF}

//Hook for the TCustomListView component
procedure Detour_TCustomListView_WndProc(Self:TCustomListView;var Message: TMessage);
var
  //LStyleServices : {$IFDEF DELPHIXE2_UP} TCustomStyleServices {$ELSE}TThemeServices{$ENDIF};
  LDetails: TThemedElementDetails;

    procedure DrawControlText(Canvas: TCanvas; Details: TThemedElementDetails;
      const S: string; var R: TRect; Flags: Cardinal);
    var
      TextFormat: TTextFormatFlags;
      {$IFDEF DELPHIXE2_UP}
      ThemeTextColor : TColor;
      {$ENDIF}
    begin
      Canvas.Font := TWinControlClass(Self).Font;
      TextFormat  := TTextFormatFlags(Flags);
      {$IFDEF DELPHIXE2_UP}
      if TColorizerLocalSettings.Settings.UseVCLStyles and TColorizerLocalSettings.Settings.VCLStylesControls then
      begin
        ColorizerStyleServices.GetElementColor(Details, ecTextColor, ThemeTextColor);
        Canvas.Font.Color := ThemeTextColor;
      end
      else
      {$ENDIF}
      if TColorizerLocalSettings.Settings.HeaderCustom  then
        Canvas.Font.Color := TryStrToColor(TColorizerLocalSettings.Settings.HeaderFontColor, TColorizerLocalSettings.ColorMap.FontColor)
      else
        Canvas.Font.Color := TColorizerLocalSettings.ColorMap.FontColor;
      //AddLog('DrawControlText', S);
      //Canvas.Font.Color:= clRed;
      {$IFDEF DELPHIXE2_UP}
      if TColorizerLocalSettings.Settings.UseVCLStyles and TColorizerLocalSettings.Settings.VCLStylesControls then
        ColorizerStyleServices.DrawText(Canvas.Handle, Details, S, R, TextFormat, Canvas.Font.Color)
      else
        StyleServices.DrawText(Canvas.Handle, Details, S, R, TextFormat, Canvas.Font.Color);
      {$ELSE}
        ThemeServices.DrawText(Canvas.Handle, Details, S, R, TextFormat, Canvas.Font.Color);
      {$ENDIF};

      //LStyleServices.DrawText(Canvas.Handle, Details, S, R, TextFormat, Canvas.Font.Color);
    end;

    procedure DrawHeaderSection(Canvas: TCanvas; R: TRect; Index: Integer;
      const Text: string; IsPressed, IsBackground: Boolean);
    var
      Item: THDItem;
      ImageList: HIMAGELIST;
      IconWidth, IconHeight: Integer;
      LBuffer : TBitmap;
      LColor1, LColor2 : TColor;
      {$IFDEF DELPHIXE2_UP}
      DrawState: TThemedHeader;
      LStyleServices :  TCustomStyleServices;
      {$ENDIF}
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

         {$IFDEF DELPHIXE2_UP}
         if TColorizerLocalSettings.Settings.UseVCLStyles and TColorizerLocalSettings.Settings.VCLStylesControls then
         begin
          if IsBackground then
            DrawState := thHeaderItemNormal
          else
          if IsPressed then
            DrawState := thHeaderItemPressed
          else
            DrawState := thHeaderItemNormal;

           LStyleServices:= ColorizerStyleServices;
           LDetails := LStyleServices.GetElementDetails(DrawState);
           LStyleServices.DrawElement(LBuffer.Canvas.Handle, LDetails, Rect(0, 0, R.Right, R.Bottom));
         end
         else
         {$ENDIF}
         begin
           if TColorizerLocalSettings.Settings.HeaderCustom  then
            LBuffer.Canvas.Pen.Color:=TryStrToColor(TColorizerLocalSettings.Settings.HeaderBorderColor, TColorizerLocalSettings.ColorMap.FrameTopLeftOuter)
           else
            LBuffer.Canvas.Pen.Color:=TColorizerLocalSettings.ColorMap.FrameTopLeftOuter;

           LBuffer.Canvas.Rectangle(Rect(0, 0, R.Right, R.Bottom));

             if TColorizerLocalSettings.Settings.HeaderCustom  then
             begin
               LColor1:= TryStrToColor(TColorizerLocalSettings.Settings.HeaderStartGrad, TColorizerLocalSettings.ColorMap.Color);
               LColor2:= TryStrToColor(TColorizerLocalSettings.Settings.HeaderEndGrad, TColorizerLocalSettings.ColorMap.MenuColor);
             end
             else
             begin
               LColor1:= TColorizerLocalSettings.ColorMap.Color;
               LColor2:= TColorizerLocalSettings.ColorMap.MenuColor;
             end;

           GradientFillCanvas(LBuffer.Canvas, LColor1, LColor2, Rect(1, 1, R.Right-1, R.Bottom-1), gdVertical);
         end;

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
//  {$IFDEF DELPHIXE2_UP}
//  LStyleServices:=StyleServices;
//  {$ELSE}
//  LStyleServices:=ThemeServices
//  {$ENDIF};

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
            DrawHeaderSection(Canvas, R, ColumnIndex, Item.pszText, {FPressedSection = ColumnIndex} False, False);

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

{$IFDEF DELPHIXE7_UP}
procedure   Detour_TCustomComboBox_DrawItem(Self: TCustomComboBox;Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
                                                                                         //Galileo Ownerdraw
  if  (TCustomComboBoxClass(Self).Style in [csOwnerDrawFixed, csOwnerDrawVariable]) and  MatchText(Self.Name, ['cbPlatforms', 'cbDevices', 'cbStyleSelector', 'cbDeviceSelector']) and Assigned(TColorizerLocalSettings.Settings) and TColorizerLocalSettings.Settings.Enabled then
    EnableStockHook:=True;

  Trampoline_TCustomComboBox_DrawItem(Self, Index, Rect, State);
  EnableStockHook:=False;
end;


procedure  Detour_TCustomListBox_DrawItem(Self : TCustomListBox; Index: Integer; Rect: TRect;  State: TOwnerDrawState);
begin
                          //CASTALIA
  if (TCustomListBoxClass(Self).Style = lbOwnerDrawVariable) and MatchText(Self.Name, ['ResultsList']) then
    EnableStockHook:=True;

  Trampoline_TCustomListBox_DrawItem(Self, Index, Rect, State);
  EnableStockHook:=False;
end;

procedure  Detour_TBrush_SetColor(Self: TBrush; Value: TColor);
begin
  HooksLock.Enter;
  try
    if EnableStockHook and Assigned(TColorizerLocalSettings.Settings) and TColorizerLocalSettings.Settings.Enabled then
    begin
      case Value of
       clWindow     : Value := TColorizerLocalSettings.ColorMap.WindowColor;
       clBtnFace    : Value := TColorizerLocalSettings.ColorMap.Color;
      end;
    end;
    Trampoline_TBrush_SetColor(Self, Value);
  finally
    HooksLock.Leave;
  end;
end;

procedure  Detour_TFont_SetColor(const Self: TFont;const Value: TColor);
var
  LValue : TColor;
begin
//    AddLog2('Detour_TFont_SetColor 1');
    LValue:=Value;
    if EnableStockHook and Assigned(TColorizerLocalSettings.Settings) and TColorizerLocalSettings.Settings.Enabled then
    begin
//      AddLog2('Detour_TFont_SetColor 2');
      case LValue of
       clWindowText : LValue := TColorizerLocalSettings.ColorMap.FontColor;
      end;
    end;
    Trampoline_TFont_SetColor(Self, LValue);
//    AddLog2('Detour_TFont_SetColor 3');
end;
{$ENDIF}


{$IFNDEF DELPHIXE2_UP}
type
 //TThemeServicesDrawElement1 =  procedure (DC: HDC; Details: TThemedElementDetails;  const R: TRect) of object;
 TThemeServicesDrawElement2 =  procedure (DC: HDC; Details: TThemedElementDetails;  const R: TRect; ClipRect: TRect) of object;
{$ENDIF}


procedure InstallColorizerHooks;
{$IFNDEF DELPHIXE2_UP}
var
 LThemeServicesDrawElement2   : TThemeServicesDrawElement2;
{$ENDIF}
begin
  //FHOOK := SetWindowsHookEx(WH_CALLWNDPROC, @HookCallWndProc, 0, GetCurrentThreadId());
  Trampoline_TWinControl_DefaultHandler:=InterceptCreate(@TWinControl.DefaultHandler, @Detour_TWinControl_DefaultHandler);

  //Trampoline_TCustomActionPopupMenu_CreateParams :=InterceptCreate(@TCustomActionPopupMenuClass.CreateParams, @Detour_TCustomActionPopupMenu_CreateParams);

  Trampoline_HintWindow_Paint := InterceptCreate(@THintWindowClass.Paint, @Detour_THintWindow_Paint);
  Trampoline_Bevel_Paint      := InterceptCreate(@TBevelClass.Paint, @Detour_TBevel_Paint);

  TrampolineCustomImageList_DoDraw:=InterceptCreate(@TCustomImageListClass.DoDraw, @Detour_TCustomImageList_DoDraw);
  Trampoline_TCanvas_FillRect     :=InterceptCreate(@TCanvas.FillRect, @Detour_TCanvas_FillRect);
  Trampoline_TCanvas_LineTo       :=InterceptCreate(@TCanvas.LineTo, @Detour_TCanvas_LineTo);
  Trampoline_TCanvas_Rectangle    :=InterceptCreate(@TCanvas.Rectangle, @Detour_TCanvas_Rectangle);

//  Trampoline_TCanvas_Polygon      :=InterceptCreate(@TCanvas.Polygon, @Detour_TCanvas_Polygon);
//  Trampoline_TCanvas_Polyline     :=InterceptCreate(@TCanvas.Polyline, @Detour_TCanvas_Polyline);

  //Trampoline_TCustomStatusBar_WMPAINT   := InterceptCreate(TCustomStatusBarClass(nil).WMPaintAddress,   @Detour_TStatusBar_WMPaint);


  Trampoline_TBitBtn_DrawItem    := InterceptCreate(TBitBtnClass(nil).DrawItemAddress,  @Detour_TBitBtn_DrawItem);

  //hoy
  Trampoline_CustomComboBox_WMPaint     := InterceptCreate(TCustomComboBox(nil).WMPaintAddress,   @Detour_TCustomComboBox_WMPaint);
  Trampoline_TCustomCombo_WndProc       := InterceptCreate(@TCustomComboClass.WndProc,   @Detour_TCustomCombo_WndProc);

  {$IFDEF DELPHIXE7_UP}
  Trampoline_TCustomComboBox_DrawItem       := InterceptCreate(@TCustomComboBoxClass.DrawItem,   @Detour_TCustomComboBox_DrawItem);
  Trampoline_TCustomListBox_DrawItem        := InterceptCreate(@TCustomListBoxClass.DrawItem,   @Detour_TCustomListBox_DrawItem);
  {$ENDIF}
 //Trampoline_TBitmap_SetSize := InterceptCreate(@TBitmap.SetSize,   @CustomSetSize);
//************************************************
{$IFDEF DELPHIXE2_UP}
  //Trampoline_TStyleEngine_HandleMessage     := InterceptCreate(@TStyleEngine.HandleMessage,   @Detour_TStyleEngine_HandleMessage);
  Trampoline_TUxThemeStyle_DoDrawElement    := InterceptCreate(@TUxThemeStyleClass.DoDrawElement,   @Detour_TUxThemeStyle_DrawElement);
{$ELSE}
  LThemeServicesDrawElement2                := ThemeServices.DrawElement;
  Trampoline_TUxTheme_DrawElement           := InterceptCreate(@LThemeServicesDrawElement2,   @Detour_TThemeServices_DrawElement);
  if Assigned(DrawThemeBackground) then
    Trampoline_DrawThemeBackground            := InterceptCreate(@DrawThemeBackground,   @Detour_UxTheme_DrawBackground);
{$ENDIF}

  Trampoline_TCustomListView_HeaderWndProc  := InterceptCreate(TCustomListViewClass(nil).HeaderWndProcAddress, @Detour_TCustomListView_WndProc);
// **************************************************

   Trampoline_TCustomControlBar_PaintControlFrame   :=  InterceptCreate(@TCustomControlBarClass.PaintControlFrame, @Detour_TCustomControlBar_PaintControlFrame);

// *******************************************
  Trampoline_TCategoryButtons_DrawCategory := InterceptCreate(TCategoryButtons(nil).DrawCategoryAddress,   @Detour_TCategoryButtons_DrawCategory);
  Trampoline_TCategoryButtons_DrawButton   := InterceptCreate(@TCategoryButtonsClass.DrawButton,   @Detour_TCategoryButtons_DrawButton);

  Trampoline_TCustomPanel_Paint            := InterceptCreate(@TCustomPanelClass.Paint, @Detour_TCustomPanel_Paint);

  Trampoline_TWinControl_WMNCPaint      := InterceptCreate(TWinControl(nil).WMNCPaintAddress, @Detour_TWinControl_WMNCPaint);

  Trampoline_DoModernPainting           := InterceptCreate(TTabSet(nil).DoModernPaintingAddress, @Detour_TTabSet_DoModernPainting);

  Trampoline_TSplitter_Paint            := InterceptCreate(@TSplitterClass.Paint, @Detour_TSplitter_Paint);
  Trampoline_TCustomGroupBox_Paint      := InterceptCreate(@TCustomGroupBoxClass.Paint, @Detour_TCustomGroupBox_Paint);

  //***********************
  Trampoline_TButtonControl_WndProc     := InterceptCreate(@TButtonControlClass.WndProc, @Detour_TButtonControlClass_WndProc);
// *******************************************
  Trampoline_TCustomLabel_DoDrawText   := InterceptCreate(@TCustomLabelClass.DoDrawText, @Detour_TCustomLabelClass_DoDrawText);

  {$IFDEF DELPHIXE7_UP}
  Trampoline_TBrush_SetColor           := InterceptCreate(@TBrushClass.SetColor, @Detour_TBrush_SetColor);
  //Trampoline_TFont_SetColor            := InterceptCreate(@TFontClass.SetColor, @Detour_TFont_SetColor);
  {$ENDIF}

end;

procedure RemoveColorizerHooks;
begin
   InterceptRemove(@Trampoline_HintWindow_Paint);
   InterceptRemove(@Trampoline_MessageHintWindow_Paint);
   InterceptRemove(@Trampoline_TWinControl_DefaultHandler);
   InterceptRemove(@Trampoline_Bevel_Paint);

{$IF CompilerVersion<37} //XE6
   InterceptRemove(@TrampolineCustomImageList_DoDraw);
{$IFEND}
   InterceptRemove(@Trampoline_TCanvas_FillRect);
   InterceptRemove(@Trampoline_TCanvas_LineTo);
   InterceptRemove(@Trampoline_TCanvas_Rectangle);

//  if Assigned(Trampoline_TCustomActionPopupMenu_CreateParams) then
//    InterceptRemove(@Trampoline_TCustomActionPopupMenu_CreateParams);

   InterceptRemove(@Trampoline_TCustomControlBar_PaintControlFrame);

   InterceptRemove(@Trampoline_TBitBtn_DrawItem);

//  if Assigned(Trampoline_TCanvas_Polyline) then
//    InterceptRemove(@Trampoline_TCanvas_Polyline);

{$IFDEF DELPHIXE2_UP}
//  if Assigned(Trampoline_TStyleEngine_HandleMessage) then
//    InterceptRemove(@Trampoline_TStyleEngine_HandleMessage);
   InterceptRemove(@Trampoline_TUxThemeStyle_DoDrawElement);
{$ELSE}
   InterceptRemove(@Trampoline_TUxTheme_DrawElement);
   InterceptRemove(@Trampoline_DrawThemeBackground);
{$ENDIF}

   //InterceptRemove(@Trampoline_TCustomStatusBar_WMPAINT);
   InterceptRemove(@Trampoline_TCustomListView_HeaderWndProc);
   InterceptRemove(@Trampoline_DoModernPainting);
   InterceptRemove(@Trampoline_TCategoryButtons_DrawCategory);
   InterceptRemove(@Trampoline_TCategoryButtons_DrawButton);
   InterceptRemove(@Trampoline_TCustomPanel_Paint);
   InterceptRemove(@Trampoline_TWinControl_WMNCPaint);
   InterceptRemove(@Trampoline_TButtonControl_WndProc);
   InterceptRemove(@Trampoline_TSplitter_Paint);
   InterceptRemove(@Trampoline_TCustomGroupBox_Paint);

   InterceptRemove(@Trampoline_CustomComboBox_WMPaint);
{$IFDEF DELPHIXE7_UP}
   InterceptRemove(@Trampoline_TCustomComboBox_DrawItem);
   InterceptRemove(@Trampoline_TCustomListBox_DrawItem);
{$ENDIF}

   InterceptRemove(@Trampoline_TCustomCombo_WndProc);


   InterceptRemove(@Trampoline_TCustomLabel_DoDrawText);
{$IFDEF DELPHIXE7_UP}
   InterceptRemove(@Trampoline_TBrush_SetColor);
   InterceptRemove(@Trampoline_TFont_SetColor);
{$ENDIF}
end;

{$IFDEF DELPHIXE7_UP}
initialization
  HooksLock  := TCriticalSection.Create;

finalization
  HooksLock.Free;
  HooksLock := nil;
{$ENDIF}


end.

