//**************************************************************************************************
//
// Unit Colorizer.Hooks.UxTheme
// unit Colorizer.Hooks.UxTheme for the Delphi IDE Colorizer
//
// The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
// you may not use this file except in compliance with the License. You may obtain a copy of the
// License at http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
// ANY KIND, either express or implied. See the License for the specific language governing rights
// and limitations under the License.
//
// The Original Code is Colorizer.Hooks.UxTheme.pas.
//
// The Initial Developer of the Original Code is Rodrigo Ruz V.
// Portions created by Rodrigo Ruz V. are Copyright (C) 2011-2015 Rodrigo Ruz V.
// All Rights Reserved.
//
//**************************************************************************************************

unit Colorizer.Hooks.UxTheme;


interface
{$I ..\Common\Jedi.inc}

procedure InstallHooksUXTheme;
procedure RemoveHooksUXTheme;

implementation
uses
  Colorizer.Utils,
  Generics.Collections,
  Windows,
  Graphics,
  Classes,
  UxTheme,
{$IFDEF DELPHIXE2_UP}
  System.Types,
  Vcl.Styles,
  Vcl.Themes,
  Colorizer.Vcl.Styles,
  //Vcl.Styles.Utils.Graphics,
{$ELSE}
  Types,
  Themes,
{$ENDIF}
  Controls,
  uMisc,
  JclDebug,
  SyncObjs,
  Messages,
  IOUtils,
  StrUtils,
  SysUtils,
  GraphUtil,
  Forms,
  StdCtrls,
  Colorizer.Hooks,
  Colorizer.Hooks.IDE,
  Dialogs,
  DDetours;

const
  VSCLASS_ITEMSVIEW_LISTVIEW             = 'ItemsView::ListView';
  VSCLASS_ITEMSVIEW_HEADER               = 'ItemsView::Header';
  VSCLASS_EXPLORER_LISTVIEW              = 'Explorer::ListView';


  type
    THThemesClasses = class
    private
     class var FClasses : TDictionary<HTHEME, String>;
     class var FWindows : TDictionary<HTHEME, HWND>;
    public
     class property Classes : TDictionary<HTHEME, String> read FClasses write FClasses;
     class property Windows : TDictionary<HTHEME, HWND> read FWindows write FWindows;
    end;

  var
  ColorizerLock    : TCriticalSection = nil;

  DrawThemeBackgroundOrgPointer : Pointer = nil;
  OpenThemeDataOrgPointer       : Pointer = nil;
  //CloseThemeDataOrgPointer      : Pointer = nil;

  TrampolineOpenThemeData             : function(hwnd: hwnd; pszClassList: LPCWSTR) : HTHEME; stdcall = nil;
  //TrampolineCloseThemeData            : function(hTheme: HTHEME): HRESULT; stdcall = nil;
  TrampolineDrawThemeBackground       : function(HTHEME: HTHEME; hdc: hdc; iPartId, iStateId: Integer; const pRect: TRect; pClipRect: pRect) : HRESULT; stdcall = nil;
  Trampoline_DrawThemeText              : function(hTheme: HTHEME; hdc: HDC; iPartId, iStateId: Integer;  pszText: LPCWSTR; iCharCount: Integer; dwTextFlags, dwTextFlags2: DWORD; const pRect: TRect): HRESULT; stdcall = nil;

  TrampolineTWinControl_WMNCPaint     : procedure (Self : TWinControl;var Message: TWMNCPaint) = nil;
  TrampolineBaseVirtualTreeOriginalWMNCPaint : procedure (Self : TCustomControl;DC: HDC) = nil;
  //Scroll Bar Functions http://msdn.microsoft.com/en-us/library/windows/desktop/ff486021%28v=vs.85%29.aspx
  {

    EnableScrollBar
    GetScrollBarInfo
    GetScrollInfo
    GetScrollPos
    GetScrollRange
    ScrollDC
    ScrollWindow
    ScrollWindowEx
   * SetScrollInfo
   * SetScrollPos
    SetScrollRange
    ShowScrollBar
  }
//  TrampolineSetScrollPos              : function (hWnd: HWND; nBar, nPos: Integer; bRedraw: BOOL): Integer; stdcall = nil;
//  TrampolineSetScrollInfo             : function (hWnd: HWND; BarFlag: Integer; const ScrollInfo: TScrollInfo; Redraw: BOOL): Integer; stdcall = nil;

function Detour_UxTheme_DrawThemeText(hTheme: HTHEME; hdc: HDC; iPartId, iStateId: Integer;  pszText: LPCWSTR; iCharCount: Integer; dwTextFlags, dwTextFlags2: DWORD; const pRect: TRect): HRESULT; stdcall;
begin
 //AddLog('Detour_UxTheme_DrawThemeText', string(pszText));
 Exit(Trampoline_DrawThemeText(hTheme, hdc, iPartId, iStateId, pszText, iCharCount, dwTextFlags, dwTextFlags2, pRect));
end;

//function Detour_WinApi_SetScrollPos(hWnd: HWND; nBar, nPos: Integer; bRedraw: BOOL): Integer; stdcall;
//begin
//  ColorizerLock.Enter;
//  try
//    LastScrollWinControl:=FindControl(hWnd);
//  finally
//    ColorizerLock.Leave;
//  end;
//  Exit(TrampolineSetScrollPos(hWnd, nBar, nPos, bRedraw));
//end;
//
//function Detour_WinApi_SetScrollInfo(hWnd: HWND; BarFlag: Integer; const ScrollInfo: TScrollInfo; Redraw: BOOL): Integer; stdcall;
//begin
//  ColorizerLock.Enter;
//  try
//    LastScrollWinControl:=FindControl(hWnd);
//  finally
//    ColorizerLock.Leave;
//  end;
//  Exit(TrampolineSetScrollInfo(hWnd, BarFlag, ScrollInfo, Redraw));
//end;

type
  TWinControlClass = class(TWinControl);

procedure Detour_TWinControl_WMNCPaint(Self : TWinControlClass;var Message: TWMNCPaint);
begin
  TrampolineTWinControl_WMNCPaint(Self, Message);
  if csDesigning in Self.ComponentState then  exit;
  if Assigned(TColorizerLocalSettings.Settings) and (TColorizerLocalSettings.Settings.Enabled) then
    DrawNCBorder(Self, True);
end;

procedure  Detour_TBaseVirtualTree_OriginalWMNCPaint(Self : TCustomControl;DC: HDC);
begin
   TrampolineBaseVirtualTreeOriginalWMNCPaint(Self, DC);
   if csDesigning in Self.ComponentState then  exit;
  //Draw the bottom right corner when both scrollbars are active in the TBaseVirtualTree
   if Assigned(TColorizerLocalSettings.Settings) and (TColorizerLocalSettings.Settings.Enabled) then
     DrawNCBorder(TWinControlClass(Self), True);
end;

function Detour_UxTheme_OpenThemeData(hwnd: hwnd; pszClassList: LPCWSTR) : HTHEME; stdcall;
begin
  ColorizerLock.Enter;
  try
    Result := TrampolineOpenThemeData(hwnd, pszClassList);
    //AddLog2('Detour_UxTheme_OpenThemeData', 'htheme '+IntToStr(Result) +' pszClassList '+string(pszClassList));

    if THThemesClasses.Classes.ContainsKey(Result) then
      THThemesClasses.Classes.Remove(Result);
    THThemesClasses.Classes.Add(Result, pszClassList);

    if THThemesClasses.Windows.ContainsKey(Result) then
      THThemesClasses.Windows.Remove(Result);
    THThemesClasses.Windows.Add(Result, hwnd);
  finally
    ColorizerLock.Leave;
  end;
end;


{$IF CompilerVersion >= 23}

{$ELSE}
function RectCenter(var R: TRect; const Bounds: TRect): TRect;
begin
  OffsetRect(R, -R.Left, -R.Top);
  OffsetRect(R, (RectWidth(Bounds) - RectWidth(R)) div 2, (RectHeight(Bounds) - RectHeight(R)) div 2);
  OffsetRect(R, Bounds.Left, Bounds.Top);
  Result := R;
end;
{$IFEND}

function Detour_UxTheme_DrawThemeBackground(THEME: HTHEME; dc: HDC;  iPartId, iStateId: Integer; const pRect: TRect; pClipRect: pRect) : HRESULT; stdcall;
const
  sTVirtualTreeColumnsSignature = 'IDEVirtualTrees.TVirtualTreeColumns.PaintHeader';
var
  sCaller{, sCaller2} : string;
  LCanvas : TCanvas;
  VCLClassName : string;
  ApplyHook  : Boolean;
  LParentForm : TCustomForm;
  LHWND : HWND;
  LFoundControl : TWinControl;
  LBuffer   : TBitmap;
  LRect     : TRect;
  LSize     : TSize;
  SavedIndex : integer;
  {$IFDEF DELPHIXE2_UP}
  LStyleServices: TCustomStyleServices;
  LDetails: TThemedElementDetails;
  {$ENDIF}
  LThemeClass : string;
//  LColor : TColor;

{$IFDEF DELPHIXE2_UP}
  function DrawScrollBarVCLStyles : HRESULT;
  var
    LScrollDetails: TThemedScrollBar;
  begin
    //AddLog2('DrawScrollBarVCLStyles');
    LStyleServices := ColorizerStyleServices;
    LScrollDetails := tsScrollBarRoot;
    LDetails.Element := TThemedElement.teScrollBar;
    LDetails.Part := iPartId;
    LDetails.State := iStateId;
    LDetails := LStyleServices.GetElementDetails(TThemedScrollBar.tsThumbBtnHorzNormal);

    case iPartId  of
      SBP_ARROWBTN :
      begin
        case iStateId of
          ABS_UPNORMAL      : LScrollDetails := tsArrowBtnUpNormal;
          ABS_UPHOT         : LScrollDetails := tsArrowBtnUpHot;
          ABS_UPPRESSED     : LScrollDetails := tsArrowBtnUpPressed;
          ABS_UPDISABLED    : LScrollDetails := tsArrowBtnUpDisabled;
          ABS_DOWNNORMAL    : LScrollDetails := tsArrowBtnDownNormal;
          ABS_DOWNHOT       : LScrollDetails := tsArrowBtnDownHot;
          ABS_DOWNPRESSED   : LScrollDetails := tsArrowBtnDownPressed;
          ABS_DOWNDISABLED  : LScrollDetails := tsArrowBtnDownDisabled;
          ABS_LEFTNORMAL    : LScrollDetails := tsArrowBtnLeftNormal;
          ABS_LEFTHOT       : LScrollDetails := tsArrowBtnLeftHot;
          ABS_LEFTPRESSED   : LScrollDetails := tsArrowBtnLeftPressed;
          ABS_LEFTDISABLED  : LScrollDetails := tsArrowBtnLeftDisabled;
          ABS_RIGHTNORMAL   : LScrollDetails := tsArrowBtnRightNormal;
          ABS_RIGHTHOT      : LScrollDetails := tsArrowBtnRightHot;
          ABS_RIGHTPRESSED  : LScrollDetails := tsArrowBtnRightPressed;
          ABS_RIGHTDISABLED : LScrollDetails := tsArrowBtnRightDisabled;
          ABS_UPHOVER       : LScrollDetails := tsArrowBtnUpNormal;//tsArrowBtnUpHover;
          ABS_DOWNHOVER     : LScrollDetails := tsArrowBtnDownNormal;//tsArrowBtnDownHover;
          ABS_LEFTHOVER     : LScrollDetails := tsArrowBtnLeftNormal;//tsArrowBtnLeftHover;
          ABS_RIGHTHOVER    : LScrollDetails := tsArrowBtnRightNormal;//tsArrowBtnRightHover;
        end;
      end;

      SBP_THUMBBTNHORZ:
      begin
        case iStateId of
         SCRBS_NORMAL   : LScrollDetails := tsThumbBtnHorzNormal;
         SCRBS_HOT      : LScrollDetails := tsThumbBtnHorzHot;
         SCRBS_PRESSED  : LScrollDetails := tsThumbBtnHorzPressed;
         SCRBS_DISABLED : LScrollDetails := tsThumbBtnHorzDisabled;
         SCRBS_HOVER    : LScrollDetails := tsThumbBtnHorzNormal;
        end;
      end;

      SBP_THUMBBTNVERT:
      begin
        case iStateId of
         SCRBS_NORMAL   : LScrollDetails := tsThumbBtnVertNormal;
         SCRBS_HOT      : LScrollDetails := tsThumbBtnVertHot;
         SCRBS_PRESSED  : LScrollDetails := tsThumbBtnVertPressed;
         SCRBS_DISABLED : LScrollDetails := tsThumbBtnVertDisabled;
         SCRBS_HOVER    : LScrollDetails := tsThumbBtnVertNormal;
        end;
      end;

      SBP_LOWERTRACKHORZ:
      begin
        case iStateId of
         SCRBS_NORMAL   : LScrollDetails := tsLowerTrackHorzNormal;
         SCRBS_HOT      : LScrollDetails := tsLowerTrackHorzHot;
         SCRBS_PRESSED  : LScrollDetails := tsLowerTrackHorzPressed;
         SCRBS_DISABLED : LScrollDetails := tsLowerTrackHorzDisabled;
         SCRBS_HOVER    : LScrollDetails := tsLowerTrackHorzNormal;//tsLowerTrackHorzHover; //no support for hover
        end;
      end;

      SBP_UPPERTRACKHORZ :
      begin
       case iStateId of
         SCRBS_NORMAL   : LScrollDetails := tsUpperTrackHorzNormal;
         SCRBS_HOT      : LScrollDetails := tsUpperTrackHorzHot;
         SCRBS_PRESSED  : LScrollDetails := tsUpperTrackHorzPressed;
         SCRBS_DISABLED : LScrollDetails := tsUpperTrackHorzDisabled;
         SCRBS_HOVER    : LScrollDetails := tsUpperTrackHorzNormal;//tsUpperTrackHorzHover; //no support for hover
       end;
      end;

      SBP_LOWERTRACKVERT:
      begin
       case iStateId of
         SCRBS_NORMAL   : LScrollDetails := tsLowerTrackVertNormal;
         SCRBS_HOT      : LScrollDetails := tsLowerTrackVertHot;
         SCRBS_PRESSED  : LScrollDetails := tsLowerTrackVertPressed;
         SCRBS_DISABLED : LScrollDetails := tsLowerTrackVertDisabled;
         SCRBS_HOVER    : LScrollDetails := tsLowerTrackVertNormal;//tsLowerTrackVertHover; //no support for hover
       end;
      end;

      SBP_UPPERTRACKVERT:
      begin
       case iStateId of
         SCRBS_NORMAL   : LScrollDetails := tsUpperTrackVertNormal;
         SCRBS_HOT      : LScrollDetails := tsUpperTrackVertHot;
         SCRBS_PRESSED  : LScrollDetails := tsUpperTrackVertPressed;
         SCRBS_DISABLED : LScrollDetails := tsUpperTrackVertDisabled;
         SCRBS_HOVER    : LScrollDetails := tsUpperTrackVertNormal;//tsUpperTrackVertHover; //no support for hover
       end;
      end;

      SBP_SIZEBOX :
      begin
       case iStateId of
        SZB_RIGHTALIGN            : LScrollDetails := tsSizeBoxRightAlign;
        SZB_LEFTALIGN             : LScrollDetails := tsSizeBoxLeftAlign;
        SZB_TOPRIGHTALIGN         : LScrollDetails := tsSizeBoxTopRightAlign;
        SZB_TOPLEFTALIGN          : LScrollDetails := tsSizeBoxTopLeftAlign;
        SZB_HALFBOTTOMRIGHTALIGN  : LScrollDetails := tsSizeBoxHalfBottomRightAlign;
        SZB_HALFBOTTOMLEFTALIGN   : LScrollDetails := tsSizeBoxHalfBottomLeftAlign;
        SZB_HALFTOPRIGHTALIGN     : LScrollDetails := tsSizeBoxHalfTopRightAlign;
        SZB_HALFTOPLEFTALIGN      : LScrollDetails := tsSizeBoxHalfTopLeftAlign;
       end;
      end;

      SBP_GRIPPERHORZ :
      begin
       case iStateId of
         SCRBS_NORMAL   : LScrollDetails := tsGripperHorzNormal;
         SCRBS_HOT      : LScrollDetails := tsGripperHorzHot;
         SCRBS_PRESSED  : LScrollDetails := tsGripperHorzPressed;
         SCRBS_DISABLED : LScrollDetails := tsGripperHorzDisabled;
         SCRBS_HOVER    : LScrollDetails := tsGripperHorzHover;//tsGripperHorzHover; //no support for hover
       end;
      end;

      SBP_GRIPPERVERT :
      begin
       case iStateId of
         SCRBS_NORMAL   : LScrollDetails := tsGripperVertNormal;
         SCRBS_HOT      : LScrollDetails := tsGripperVertHot;
         SCRBS_PRESSED  : LScrollDetails := tsGripperVertPressed;
         SCRBS_DISABLED : LScrollDetails := tsGripperVertDisabled;
         SCRBS_HOVER    : LScrollDetails := tsGripperVertNormal;//tsGripperVertHover; //no support for hover
       end;
      end;

    end;
    LDetails := LStyleServices.GetElementDetails(LScrollDetails);

    //LStyle.DrawParentBackground(Self.Handle, dc, LDetails, False);
    if (iPartId=SBP_THUMBBTNHORZ) then
      LStyleServices.DrawElement(dc, LStyleServices.GetElementDetails(tsUpperTrackHorzNormal), pRect, pClipRect)
    else
    if (iPartId=SBP_THUMBBTNVERT) then
      LStyleServices.DrawElement(dc, LStyleServices.GetElementDetails(tsUpperTrackVertNormal), pRect, pClipRect);

    LStyleServices.DrawElement(dc, LDetails, pRect, pClipRect);
    Exit(0);
  end;
{$ENDIF}
  function DrawScrollBarFlat : HRESULT; stdcall;
  begin
    Result:=0;
      case iPartId  of
        SBP_ARROWBTN :
        begin
          case iStateId of
            ABS_UPNORMAL,
            ABS_UPHOT,
            ABS_UPPRESSED,
            ABS_UPDISABLED,
            ABS_UPHOVER       : begin
                                   LCanvas:=TCanvas.Create;
                                   try
                                     LCanvas.Handle:=dc;
                                     if (iStateId=SCRBS_HOT) or (iStateId=SCRBS_PRESSED) then
                                      LCanvas.Brush.Color:=TColorizerLocalSettings.ColorMap.HighlightColor
                                     else
                                      LCanvas.Brush.Color:=TColorizerLocalSettings.ColorMap.Color;
                                      LCanvas.Pen.Color:=TColorizerLocalSettings.ColorMap.FrameTopLeftInner;
                                      LCanvas.Rectangle(pRect);
                                      LCanvas.Brush.Color:=TColorizerLocalSettings.ColorMap.FontColor;
                                      LCanvas.Pen.Color  :=TColorizerLocalSettings.ColorMap.FontColor;
                                      DrawArrow(LCanvas, TScrollDirection.sdUp, Point(pRect.Left+4, pRect.Top+6), 4);
                                   finally
                                     LCanvas.Handle:=0;
                                     LCanvas.Free;
                                   end;
                                   Exit(0);
                                end;

            ABS_DOWNNORMAL,
            ABS_DOWNHOT,
            ABS_DOWNPRESSED,
            ABS_DOWNDISABLED,
            ABS_DOWNHOVER     : begin
                                   LCanvas:=TCanvas.Create;
                                   try
                                     LCanvas.Handle:=dc;
                                     if (iStateId=SCRBS_HOT) or (iStateId=SCRBS_PRESSED) then
                                      LCanvas.Brush.Color:=TColorizerLocalSettings.ColorMap.HighlightColor
                                     else
                                      LCanvas.Brush.Color:=TColorizerLocalSettings.ColorMap.Color;
                                     LCanvas.Pen.Color:=TColorizerLocalSettings.ColorMap.FrameTopLeftInner;
                                     LCanvas.Rectangle(pRect);

                                     LCanvas.Brush.Color:=TColorizerLocalSettings.ColorMap.FontColor;
                                     LCanvas.Pen.Color  :=TColorizerLocalSettings.ColorMap.FontColor;
                                     DrawArrow(LCanvas, TScrollDirection.sdDown, Point(pRect.Left+4, pRect.Top+6), 4);
                                   finally
                                     LCanvas.Handle:=0;
                                     LCanvas.Free;
                                   end;
                                   Exit(0);
                                end;

            ABS_LEFTNORMAL,
            ABS_LEFTHOT,
            ABS_LEFTPRESSED,
            ABS_LEFTDISABLED,
            ABS_LEFTHOVER     : begin
                                   LCanvas:=TCanvas.Create;
                                   try
                                     LCanvas.Handle:=dc;
                                     if (iStateId=SCRBS_HOT) or (iStateId=SCRBS_PRESSED) then
                                      LCanvas.Brush.Color:=TColorizerLocalSettings.ColorMap.HighlightColor
                                     else
                                      LCanvas.Brush.Color:=TColorizerLocalSettings.ColorMap.Color;
                                     LCanvas.Pen.Color:=TColorizerLocalSettings.ColorMap.FrameTopLeftInner;
                                     LCanvas.Rectangle(pRect);

                                     LCanvas.Brush.Color:=TColorizerLocalSettings.ColorMap.FontColor;
                                     LCanvas.Pen.Color  :=TColorizerLocalSettings.ColorMap.FontColor;
                                     DrawArrow(LCanvas, TScrollDirection.sdLeft, Point(pRect.Left+6, pRect.Top+4), 4);
                                   finally
                                     LCanvas.Handle:=0;
                                     LCanvas.Free;
                                   end;
                                   Exit(0);
                                end;

            ABS_RIGHTNORMAL,
            ABS_RIGHTHOT,
            ABS_RIGHTPRESSED,
            ABS_RIGHTDISABLED,
            ABS_RIGHTHOVER    : begin
                                   LCanvas:=TCanvas.Create;
                                   try
                                     LCanvas.Handle:=dc;
                                     if (iStateId=SCRBS_HOT) or (iStateId=SCRBS_PRESSED) then
                                      LCanvas.Brush.Color:=TColorizerLocalSettings.ColorMap.HighlightColor
                                     else
                                      LCanvas.Brush.Color:=TColorizerLocalSettings.ColorMap.Color;
                                     LCanvas.Pen.Color:=TColorizerLocalSettings.ColorMap.FrameTopLeftInner;
                                     LCanvas.Rectangle(pRect);

                                     LCanvas.Brush.Color:=TColorizerLocalSettings.ColorMap.FontColor;
                                     LCanvas.Pen.Color  :=TColorizerLocalSettings.ColorMap.FontColor;
                                     DrawArrow(LCanvas, TScrollDirection.sdRight, Point(pRect.Left+6, pRect.Top+4), 4);
                                   finally
                                     LCanvas.Handle:=0;
                                     LCanvas.Free;
                                   end;
                                   Exit(0);
                                end;
          end;
        end;

        SBP_THUMBBTNHORZ:
        begin
         LCanvas:=TCanvas.Create;
         try
           LCanvas.Handle:=dc;
           if (iStateId=SCRBS_HOT) or (iStateId=SCRBS_PRESSED) then
            LCanvas.Brush.Color:=TColorizerLocalSettings.ColorMap.HighlightColor
           else
            LCanvas.Brush.Color:=TColorizerLocalSettings.ColorMap.Color;

           LCanvas.Pen.Color:=TColorizerLocalSettings.ColorMap.FrameTopLeftInner;
           LCanvas.Rectangle(pRect);
         finally
           LCanvas.Handle:=0;
           LCanvas.Free;
         end;
         exit(0);
        end;

        SBP_THUMBBTNVERT:
        begin
         LCanvas:=TCanvas.Create;
         try
           LCanvas.Handle:=dc;
           if (iStateId=SCRBS_HOT) or (iStateId=SCRBS_PRESSED) then
            LCanvas.Brush.Color:=TColorizerLocalSettings.ColorMap.HighlightColor
           else
            LCanvas.Brush.Color:=TColorizerLocalSettings.ColorMap.Color;
           LCanvas.Pen.Color:=TColorizerLocalSettings.ColorMap.FrameTopLeftInner;
           LCanvas.Rectangle(pRect);
         finally
           LCanvas.Handle:=0;
           LCanvas.Free;
         end;
         exit(0);
        end;

        SBP_LOWERTRACKHORZ:
        begin
         LCanvas:=TCanvas.Create;
         try
           LCanvas.Handle:=dc;
           LCanvas.Brush.Color:=TColorizerLocalSettings.ColorMap.MenuColor;
           LCanvas.FillRect(pRect);
           LCanvas.Brush.Style := bsClear;
           LCanvas.Pen.Color   := TColorizerLocalSettings.ColorMap.FrameTopLeftInner;
           LCanvas.Rectangle(Rect(pRect.Left, pRect.Top, pRect.Right, pRect.Top+1));
           LCanvas.Rectangle(Rect(pRect.Left, pRect.Bottom-1, pRect.Right, pRect.Bottom));
         finally
           LCanvas.Handle:=0;
           LCanvas.Free;
         end;
         exit(0);
        end;

        SBP_UPPERTRACKHORZ :
        begin
         LCanvas:=TCanvas.Create;
         try
           LCanvas.Handle:=dc;
           LCanvas.Brush.Color:=TColorizerLocalSettings.ColorMap.MenuColor;
           LCanvas.FillRect(pRect);
           LCanvas.Pen.Color   := TColorizerLocalSettings.ColorMap.FrameTopLeftInner;
           LCanvas.Rectangle(Rect(pRect.Left, pRect.Top, pRect.Right, pRect.Top+1));
           LCanvas.Rectangle(Rect(pRect.Left, pRect.Bottom-1, pRect.Right, pRect.Bottom));
         finally
           LCanvas.Handle:=0;
           LCanvas.Free;
         end;
         exit(0);

        end;

        SBP_LOWERTRACKVERT:
        begin
         LCanvas:=TCanvas.Create;
         try
           LCanvas.Handle:=dc;
           LCanvas.Brush.Color:=TColorizerLocalSettings.ColorMap.MenuColor;
           LCanvas.FillRect(pRect);
           LCanvas.Pen.Color  :=TColorizerLocalSettings.ColorMap.FrameTopLeftInner;
           LCanvas.Rectangle(Rect(pRect.Left, pRect.Top, pRect.Left+1, pRect.Bottom));
           LCanvas.Rectangle(Rect(pRect.Right-1, pRect.Top, pRect.Right, pRect.Bottom));
         finally
           LCanvas.Handle:=0;
           LCanvas.Free;
         end;
         exit(0);
        end;

        SBP_UPPERTRACKVERT:
        begin
         LCanvas:=TCanvas.Create;
         try
           LCanvas.Handle:=dc;
           LCanvas.Brush.Color:=TColorizerLocalSettings.ColorMap.MenuColor;
           LCanvas.FillRect(pRect);
           LCanvas.Pen.Color  :=TColorizerLocalSettings.ColorMap.FrameTopLeftInner;
           LCanvas.Rectangle(Rect(pRect.Left, pRect.Top, pRect.Left+1, pRect.Bottom));
           LCanvas.Rectangle(Rect(pRect.Right-1, pRect.Top, pRect.Right, pRect.Bottom));
         finally
           LCanvas.Handle:=0;
           LCanvas.Free;
         end;
         exit(0);
        end;

        SBP_SIZEBOX,
        SBP_GRIPPERHORZ,
        SBP_GRIPPERVERT : exit(0);
      end;
  end;

begin
  if not (THThemesClasses.Classes.ContainsKey(THEME) and Assigned(TColorizerLocalSettings.ColorMap) and Assigned(TColorizerLocalSettings.Settings)  and TColorizerLocalSettings.Settings.Enabled) then
  begin
   Exit(TrampolineDrawThemeBackground(THEME, dc, iPartId, iStateId, pRect, pClipRect));
   //AddLog2(PChar(Format('DrawThemeBackground  class %s hTheme %d iPartId %d iStateId %d', ['Ignored', Theme, iPartId, iStateId])));
  end;

   LThemeClass:=THThemesClasses.Classes.Items[THEME];

   //AddLog2(PChar(Format('DrawThemeBackground  class %s hTheme %d iPartId %d iStateId %d', [LThemeClass, Theme, iPartId, iStateId])));


//   if  (SameText(LThemeClass, VSCLASS_LISTVIEW) or SameText(LThemeClass, VSCLASS_ITEMSVIEW_LISTVIEW) or SameText(LThemeClass, VSCLASS_EXPLORER_LISTVIEW)) then
//   begin
//        case iPartId of
//          LVP_LISTITEM       :
//                              begin
//                                  case iStateId of
//                                      LIS_HOT,
//                                      LISS_HOTSELECTED,
//                                      LIS_SELECTEDNOTFOCUS,
//                                      LIS_SELECTED          :
//                                                              begin
//                                                                //AddLog2('AlphaBlendFillCanvas');
//                                                                LColor :=ColorizerStyleServices.GetSystemColor(clHighlight);
//                                                                LCanvas:=TCanvas.Create;
//                                                                SavedIndex := SaveDC(dc);
//                                                                try
//                                                                  LCanvas.Handle:=dc;
//                                                                  if iStateId=LISS_HOTSELECTED then
//                                                                    AlphaBlendFillCanvas(LCanvas, LColor, pRect, 96)
//                                                                  else
//                                                                    AlphaBlendFillCanvas(LCanvas, LColor, pRect, 50);
//                                                                  LCanvas.Pen.Color:=LColor;
//                                                                  LCanvas.Brush.Style:=bsClear;
//                                                                  LRect:=pRect;
//                                                                  LCanvas.Rectangle(LRect.Left, LRect.Top, LRect.Left +  LRect.Width,  LRect.Top + LRect.Height);
//                                                                finally
//                                                                  LCanvas.Handle:=0;
//                                                                  LCanvas.Free;
//                                                                  RestoreDC(dc, SavedIndex);
//                                                                end;
//                                                                Result:=S_OK;
//                                                              end;
//                                  else
//                                       Exit(TrampolineDrawThemeBackground(THEME, dc, iPartId, iStateId, pRect, pClipRect));
//                                  end;
//                              end;
//
//        else
//            Exit(TrampolineDrawThemeBackground(THEME, dc, iPartId, iStateId, pRect, pClipRect));
//        end;
//  end
//  else
  if SameText(LThemeClass, VSCLASS_TOOLTIP) then
  begin
      case iPartId  of
       TTP_STANDARD :
                     begin
                       SavedIndex:=SaveDC(dc);
                       LCanvas:=TCanvas.Create;
                       try
                          LCanvas.Handle:=dc;
                          LCanvas.Brush.Color:=TColorizerLocalSettings.ColorMap.WindowColor;
                          LCanvas.Pen.Color:=TColorizerLocalSettings.ColorMap.FrameTopLeftInner;
                          LCanvas.Rectangle(pRect);
                       finally
                         LCanvas.Handle:=0;
                         LCanvas.Free;
                         if SavedIndex<>0 then
                           RestoreDC(dc, SavedIndex);
                       end;
                       Exit(0);
                     end;

      end;
   Exit(TrampolineDrawThemeBackground(THEME, dc, iPartId, iStateId, pRect, pClipRect));
  end
  else
  if SameText(LThemeClass, VSCLASS_SCROLLBAR) then
  begin

    ApplyHook:=True; //improve overall perfomance . drawback : all the scrollbars are styled.
    {
    ApplyHook:=False;
    sCaller :='';
    sCaller2:='';
    VCLClassName:='';
    LFoundControl:=nil;

    try
      if Assigned(LastScrollWinControl) then
        VCLClassName:=LastScrollWinControl.ClassName;
    except
      VCLClassName := '';
    end;
    //LastScrollWinControl:=nil;

     LHWND:=0;
    if THThemesClasses.Windows.ContainsKey(THEME) then
     LHWND:=THThemesClasses.Windows.Items[THEME];

    if LHWND=0 then
     LHWND:=WindowFromDC(dc);

    if LHWND<>0 then
     begin
      LFoundControl := FindControl(LHWND);
      if LFoundControl<>nil then
        VCLClassName:= LFoundControl.ClassName;
     end;

    if LFoundControl<>nil then
    begin
       try
         ApplyHook:= not (csDesigning in LFoundControl.ComponentState) and (TColorizerLocalSettings.HookedScrollBars.IndexOf(VCLClassName)>=0) or  (TColorizerLocalSettings.HookedWindows.IndexOf(VCLClassName)>=0);
         LParentForm:=GetParentForm(LFoundControl);
         if (LParentForm<>nil) and ApplyHook then
           ApplyHook:= Assigned(TColorizerLocalSettings.HookedWindows) and (TColorizerLocalSettings.HookedWindows.IndexOf(LParentForm.ClassName)>=0);
       except
        ApplyHook:=False
       end;
    end;

    if not ApplyHook then
    begin
      sCaller := ProcByLevel(1);
      if sCaller<>'' then
        sCaller2 := ProcByLevel(2);

      ApplyHook:= (sCaller='') or  (TColorizerLocalSettings.HookedScrollBars.IndexOf(VCLClassName)>=0) or  (TColorizerLocalSettings.HookedWindows.IndexOf(VCLClassName)>=0);
    end;

    //look for hooked controls in the caller level 1
    if not ApplyHook and (sCaller<>'') then
       for s in SplitString(sCaller,'.') do
       begin
         ApplyHook:= SameText(s, 'IDEVirtualTrees') or StartsText('T', s) and ( (TColorizerLocalSettings.HookedWindows.IndexOf(s)>=0) or (TColorizerLocalSettings.HookedScrollBars.IndexOf(s)>=0) );
         if ApplyHook then break;
       end;

    //look for hooked controls in the caller level 2
    if not ApplyHook and (sCaller2<>'') then
       for s in SplitString(sCaller2,'.') do
       begin
         ApplyHook:= SameText(s, 'IDEVirtualTrees') or StartsText('T', s) and ( (TColorizerLocalSettings.HookedWindows.IndexOf(s)>=0) or (TColorizerLocalSettings.HookedScrollBars.IndexOf(s)>=0) );
         if ApplyHook then break;
       end;
                }
//    AddLog('ScrollBar','LHWND '+IntToHex(LHWND, 8));
//    if ApplyHook and (LHWND<>0) then
//    begin
//      WClassName := GetWindowClassName(LHWND);
//      ApplyHook:= not ((WClassName<>'') and (TColorizerLocalSettings.WinAPIClasses.IndexOf(WClassName)>=0));
//      AddLog('ScrollBar','WClassName '+WClassName);
//    end;

    if ApplyHook then
    begin
      {$IFDEF DELPHIXE2_UP}
      if TColorizerLocalSettings.Settings.UseVCLStyles and TColorizerLocalSettings.Settings.VCLStylesScrollBars then
        Exit(DrawScrollBarVCLStyles())
      else
      {$ENDIF}
        Exit(DrawScrollBarFlat());
    end
    else
    begin
//      if THThemesClasses.ScrollBars.ContainsKey(THEME) then
//      begin
//         for i :=1 to 5 do
//         begin
//           sCaller := ProcByLevel(i);
//           AddLog('Scrollbar Ignored', Format(' %d %s %s',[i, VCLClassName, sCaller]));
//         end;
//           AddLog('Scrollbar Ignored', Format('%s',['------------------------------------------------']));
//      end;

      Exit(TrampolineDrawThemeBackground(THEME, dc, iPartId, iStateId, pRect, pClipRect));
    end;
  end
  else
  if SameText(LThemeClass, VSCLASS_TREEVIEW) then
  begin
           //deshabilitar en treeeview
    if (iPartId = TVP_GLYPH) and (iStateId=GLPS_OPENED) and ((pRect.Right-pRect.Left)=9) then
    begin
      sCaller  := ProcByLevel(4);
      //AddLog2('GLPS_OPENED  sCaller '+sCaller);

                   //VirtualTreeView           //Fix CustomPropListBox, because LFoundControl is nil sometimes (ex : scroll)
      ApplyHook:=  (sCaller = '') or (SameText('PropBox.TCustomPropListBox.DrawPropItem', sCaller));
      if not ApplyHook then
      begin
        //if SameText('PropBox.TCustomPropListBox.DrawPropItem', sCaller) then
        begin
            LHWND:=0;
            if THThemesClasses.Windows.ContainsKey(THEME) then
             LHWND:=THThemesClasses.Windows.Items[THEME];

            if LHWND=0 then
              LHWND:=WindowFromDC(dc);

            if LHWND<>0 then
             begin
              LFoundControl := FindControl(LHWND);
              if LFoundControl<>nil then
              begin
                 LParentForm:=GetParentForm(LFoundControl);
                 if LParentForm<>nil then
                   ApplyHook:= Assigned(TColorizerLocalSettings.HookedWindows) and (TColorizerLocalSettings.HookedWindows.IndexOf(LParentForm.ClassName)>=0);
              end;
             end;
        end;
      end;

      if ApplyHook then
      begin
         {$IFDEF DELPHIXE2_UP}
         if (sCaller<>'') and TColorizerLocalSettings.Settings.UseVCLStyles and TColorizerLocalSettings.Settings.VCLStylesControls then
         begin
           LStyleServices:= ColorizerStyleServices;
           LDetails := LStyleServices.GetElementDetails(tcbCategoryGlyphOpened);
           LBuffer:=TBitmap.Create;
           try
             //LStyleServices.GetElementSize(LBuffer.Canvas.Handle, LDetails, esActual, LSize);
             //AddLog('tcbCategoryGlyphOpened', 'LSize.Width '+IntToStr(LSize.Width)+' LSize.Height '+IntToStr(LSize.Height));
             LSize.cx:=10;
             LSize.cy:=10;
             //LRect := Rect(0, 0, pRect.Width, pRect.Height);
             LRect := Rect(0, 0, LSize.Width, LSize.Height);

             LBuffer.Canvas.Brush.Color:=LStyleServices.GetSystemColor(clWindow);
             LBuffer.Canvas.FillRect(LRect);

             LBuffer.SetSize(LRect.Width, LRect.Height);
             LStyleServices.DrawElement(LBuffer.Canvas.Handle, LDetails, LRect);
             BitBlt(dc, pRect.Left, pRect.Top, LRect.Width, LRect.Height, LBuffer.Canvas.Handle, 0, 0, SRCCOPY);
           finally
             LBuffer.Free;
           end;
         end
         else
         {$ENDIF}
         begin
           LBuffer:=TBitmap.Create;
           try
             LBuffer.SetSize((pRect.Right-pRect.Left), (pRect.Bottom-pRect.Top));
             LRect := Rect(0, 0, (pRect.Right-pRect.Left), (pRect.Bottom-pRect.Top));
             LBuffer.Canvas.Brush.Color:=TColorizerLocalSettings.ColorMap.WindowColor;
             LBuffer.Canvas.Pen.Color  :=TColorizerLocalSettings.ColorMap.FontColor;
             LBuffer.Canvas.Rectangle(LRect);
             LBuffer.Canvas.MoveTo(2, (LRect.Bottom-LRect.Top) div 2);
             LBuffer.Canvas.LineTo((LRect.Right-LRect.Left) - 2, (LRect.Bottom-LRect.Top) div 2);

             BitBlt(dc, pRect.Left, pRect.Top, (pRect.Right-pRect.Left), (pRect.Bottom-pRect.Top), LBuffer.Canvas.Handle, 0, 0, SRCCOPY);
           finally
             LBuffer.Free;
           end;
         end;
         Exit(0);
      end;
    end
    else
    if (iPartId = TVP_GLYPH) and (iStateId=GLPS_CLOSED) and ((pRect.Right-pRect.Left)=9) then
    begin
      sCaller  := ProcByLevel(4);
                   //VirtualTreeView           //Fix CustomPropListBox, because LFoundControl is nil sometimes (ex : scroll)
      ApplyHook:= (sCaller = '') or (SameText('PropBox.TCustomPropListBox.DrawPropItem', sCaller));
      if not ApplyHook then
      begin
        //if SameText('PropBox.TCustomPropListBox.DrawPropItem', sCaller) then
        begin
             LHWND:=0;
            if THThemesClasses.Windows.ContainsKey(THEME) then
             LHWND:=THThemesClasses.Windows.Items[THEME];

            if LHWND=0 then
              LHWND:=WindowFromDC(dc);

            if LHWND<>0 then
             begin
              LFoundControl := FindControl(LHWND);
              if LFoundControl<>nil then
              begin
                 LParentForm:=GetParentForm(LFoundControl);
                 if LParentForm<>nil then
                   ApplyHook:= Assigned(TColorizerLocalSettings.HookedWindows) and (TColorizerLocalSettings.HookedWindows.IndexOf(LParentForm.ClassName)>=0);
              end;
             end;
        end;
      end;

      if ApplyHook then
      begin
         {$IFDEF DELPHIXE2_UP}
         if (sCaller<>'') and  TColorizerLocalSettings.Settings.UseVCLStyles and TColorizerLocalSettings.Settings.VCLStylesControls then
         begin
           LStyleServices:= ColorizerStyleServices;
           LDetails := LStyleServices.GetElementDetails(tcbCategoryGlyphClosed);
           LBuffer:=TBitmap.Create;
           try
             //LStyleServices.GetElementSize(LBuffer.Canvas.Handle, LDetails, esActual, LSize);
             //AddLog('tcbCategoryGlyphClosed', 'LSize.Width '+IntToStr(LSize.Width)+' LSize.Height '+IntToStr(LSize.Height));
             LSize.cx:=10;
             LSize.cy:=10;
             //LRect := Rect(0, 0, pRect.Width, pRect.Height);
             LRect := Rect(0, 0, LSize.Width, LSize.Height);

             LBuffer.Canvas.Brush.Color:=LStyleServices.GetSystemColor(clWindow);
             LBuffer.Canvas.FillRect(LRect);

             LBuffer.SetSize(LRect.Width, LRect.Height);
             LStyleServices.DrawElement(LBuffer.Canvas.Handle, LDetails, LRect);
             BitBlt(dc, pRect.Left, pRect.Top, LRect.Width, LRect.Height, LBuffer.Canvas.Handle, 0, 0, SRCCOPY);
           finally
             LBuffer.Free;
           end;
         end
         else
         {$ENDIF}
         begin
           LBuffer:=TBitmap.Create;
           try
             LBuffer.SetSize((pRect.Right-pRect.Left), (pRect.Bottom-pRect.Top));
             LRect := Rect(0, 0, (pRect.Right-pRect.Left), (pRect.Bottom-pRect.Top));
             LBuffer.Canvas.Brush.Color:=TColorizerLocalSettings.ColorMap.WindowColor;
             LBuffer.Canvas.Pen.Color  :=TColorizerLocalSettings.ColorMap.FontColor;
             LBuffer.Canvas.Rectangle(LRect);
             LBuffer.Canvas.MoveTo(2, (LRect.Bottom-LRect.Top) div 2);
             LBuffer.Canvas.LineTo((LRect.Right-LRect.Left) - 2, (LRect.Bottom-LRect.Top) div 2);
             LBuffer.Canvas.MoveTo((LRect.Right-LRect.Left) div 2, 2);
             LBuffer.Canvas.LineTo((LRect.Right-LRect.Left) div 2, (LRect.Bottom-LRect.Top) - 2);

             BitBlt(dc, pRect.Left, pRect.Top, (pRect.Right-pRect.Left), (pRect.Bottom-pRect.Top), LBuffer.Canvas.Handle, 0, 0, SRCCOPY);
           finally
             LBuffer.Free;
           end;
         end;
         Exit(0);
      end;
    end;
//      sCaller  := ProcByLevel(4);
//      AddLog('THThemesClasses.TreeView','Ignored '+sCaller);
  end
  else
  if SameText(LThemeClass, VSCLASS_BUTTON) then
  begin
    if (iPartId = BP_CHECKBOX) then
    begin
      LSize.cx:= 13;
      LSize.cy:= 13;
      LHWND:=0;
      if THThemesClasses.Windows.ContainsKey(THEME) then
       LHWND:=THThemesClasses.Windows.Items[THEME];
      if LHWND=0 then
        LHWND:=WindowFromDC(dc);
       //if LHWND<>0 then
       begin
        LFoundControl := FindControl(LHWND);
        if LFoundControl=nil then
        begin
          try
            if Assigned(LastWinControl) then
            begin
              VCLClassName:=LastWinControl.ClassName;
              LFoundControl:=LastWinControl;
            end;
          except
            VCLClassName  := '';
            LFoundControl := nil;
          end;
        end
        else
          sCaller := ProcByLevel(4);

        //VCLEditors.DrawCheckbox
        //procedure DrawCheckbox(ACanvas: TCanvas; ARect : TRect;  ASelected, AEnabled, AAllEqual, AValue: Boolean);
                                  //Fix CustomPropListBox, because LFoundControl is nil sometimes (ex : scroll)
        if (LFoundControl<>nil) or SameText('VCLEditors.DrawCheckbox', sCaller) or SameText('IDEVirtualTrees.TBaseVirtualTree.PaintCheckImage', sCaller) then
        begin
           ApplyHook:=SameText('VCLEditors.DrawCheckbox', sCaller);

           if not ApplyHook then
           begin
             try
               ApplyHook:= not (csDesigning in LFoundControl.ComponentState);
               LParentForm:=GetParentForm(LFoundControl);
               if (LParentForm<>nil) and ApplyHook then
                 ApplyHook:= Assigned(TColorizerLocalSettings.HookedWindows) and (TColorizerLocalSettings.HookedWindows.IndexOf(LParentForm.ClassName)>=0);
             except
              ApplyHook:=False
             end;
           end;

           if ApplyHook then
           begin
             {$IFDEF DELPHIXE2_UP}
             if TColorizerLocalSettings.Settings.UseVCLStyles and TColorizerLocalSettings.Settings.VCLStylesControls then
             begin
               LStyleServices:= ColorizerStyleServices;
                {
                    tbCheckBoxUncheckedNormal, tbCheckBoxUncheckedHot, tbCheckBoxUncheckedPressed, tbCheckBoxUncheckedDisabled,
                    tbCheckBoxCheckedNormal, tbCheckBoxCheckedHot, tbCheckBoxCheckedPressed, tbCheckBoxCheckedDisabled,
                    tbCheckBoxMixedNormal, tbCheckBoxMixedHot, tbCheckBoxMixedPressed, tbCheckBoxMixedDisabled,
                    tbCheckBoxImplicitNormal, tbCheckBoxImplicitHot, tbCheckBoxImplicitPressed, tbCheckBoxImplicitDisabled, // Windows Vista or later
                    tbCheckBoxExcludedNormal, tbCheckBoxExcludedHot, tbCheckBoxExcludedPressed, tbCheckBoxExcludedDisabled, // Windows Vista or later

                }
               case iStateId of
                  CBS_UNCHECKEDNORMAL  :  LDetails := LStyleServices.GetElementDetails(tbCheckBoxUncheckedNormal);
                  CBS_UNCHECKEDHOT     :  LDetails := LStyleServices.GetElementDetails(tbCheckBoxUncheckedHot);
                  CBS_UNCHECKEDPRESSED :  LDetails := LStyleServices.GetElementDetails(tbCheckBoxUncheckedPressed);
                  CBS_UNCHECKEDDISABLED:  LDetails := LStyleServices.GetElementDetails(tbCheckBoxUncheckedDisabled);
                  CBS_CHECKEDNORMAL    :  LDetails := LStyleServices.GetElementDetails(tbCheckBoxCheckedNormal);
                  CBS_CHECKEDHOT       :  LDetails := LStyleServices.GetElementDetails(tbCheckBoxCheckedHot);
                  CBS_CHECKEDPRESSED   :  LDetails := LStyleServices.GetElementDetails(tbCheckBoxCheckedPressed);
                  CBS_CHECKEDDISABLED  :  LDetails := LStyleServices.GetElementDetails(tbCheckBoxCheckedDisabled);
               end;

               LBuffer:=TBitmap.Create;
               try
                 LBuffer.SetSize(LSize.cx, LSize.cy);
                 LRect := Rect(0, 0, LSize.cx, LSize.cy);
                 LBuffer.Canvas.Brush.Color := TColorizerLocalSettings.ColorMap.Color;
                 LBuffer.Canvas.FillRect(LRect);
                 LStyleServices.DrawElement(LBuffer.Canvas.Handle, LDetails, LRect);
                 RectCenter(LRect, pRect);
                 BitBlt(dc, LRect.Left, LRect.Top, LSize.cx, LSize.cy, LBuffer.Canvas.Handle, 0, 0, SRCCOPY);
               finally
                 LBuffer.Free;
               end;
               Exit(0);
             end
             else
             {$ENDIF}
             begin
               case iStateId of
                  CBS_UNCHECKEDNORMAL,
                  CBS_UNCHECKEDHOT,
                  CBS_UNCHECKEDPRESSED,
                  CBS_UNCHECKEDDISABLED : begin
                                             LBuffer:=TBitmap.Create;
                                             try
                                               LBuffer.SetSize(LSize.cx, LSize.cy);
                                               LRect := Rect(0, 0, LSize.cx, LSize.cy);
                                               if iStateId= CBS_UNCHECKEDHOT then
                                                 LBuffer.Canvas.Brush.Color:=TColorizerLocalSettings.ColorMap.SelectedColor
                                               else
                                                 LBuffer.Canvas.Brush.Color:=TColorizerLocalSettings.ColorMap.WindowColor;

                                               LBuffer.Canvas.Pen.Color  :=TColorizerLocalSettings.ColorMap.FontColor;
                                               LBuffer.Canvas.Rectangle(LRect);
                                               RectCenter(LRect, pRect);
                                               BitBlt(dc, LRect.Left, LRect.Top, LSize.cx, LSize.cy, LBuffer.Canvas.Handle, 0, 0, SRCCOPY);
                                             finally
                                               LBuffer.Free;
                                             end;
                                             Exit(0);
                                          end;
                  CBS_CHECKEDNORMAL,
                  CBS_CHECKEDHOT,
                  CBS_CHECKEDPRESSED,
                  CBS_CHECKEDDISABLED   : begin
                                             LBuffer:=TBitmap.Create;
                                             try
                                               LBuffer.SetSize(LSize.cx, LSize.cy);
                                               LRect := Rect(0, 0, LSize.cx, LSize.cy);
                                               if iStateId= CBS_CHECKEDHOT then
                                                 LBuffer.Canvas.Brush.Color:=TColorizerLocalSettings.ColorMap.SelectedColor
                                               else
                                                 LBuffer.Canvas.Brush.Color:=TColorizerLocalSettings.ColorMap.WindowColor;
                                               LBuffer.Canvas.Pen.Color  :=TColorizerLocalSettings.ColorMap.FontColor;
                                               LBuffer.Canvas.Rectangle(LRect);
                                               DrawCheck(LBuffer.Canvas, Point(LRect.Left+3, LRect.Top+6), 2, False);
                                               RectCenter(LRect, pRect);
                                               BitBlt(dc, LRect.Left, LRect.Top, LSize.cx, LSize.cy, LBuffer.Canvas.Handle, 0, 0, SRCCOPY);
                                             finally
                                               LBuffer.Free;
                                             end;
                                             Exit(0);
                                          end;
               end;
             end;
           end;
        end;
       end;
    end
    else
    if (iPartId = BP_RADIOBUTTON) then
    begin
      LSize.cx:= 13;
      LSize.cy:= 13;
      LHWND:=0;
      if THThemesClasses.Windows.ContainsKey(THEME) then
       LHWND:=THThemesClasses.Windows.Items[THEME];
      if LHWND=0 then
        LHWND:=WindowFromDC(dc);

        LFoundControl := FindControl(LHWND);
        if LFoundControl=nil then
        begin
          try
            if Assigned(LastWinControl) then
            begin
              VCLClassName:=LastWinControl.ClassName;
              LFoundControl:=LastWinControl;
            end;
          except
            VCLClassName  := '';
            LFoundControl := nil;
          end;
        end
        else
          sCaller := ProcByLevel(4);

        if (LFoundControl<>nil) {or SameText('VCLEditors.DrawCheckbox', sCaller) or SameText('IDEVirtualTrees.TBaseVirtualTree.PaintCheckImage', sCaller)} then
        begin
           ApplyHook:=false;//SameText('VCLEditors.DrawCheckbox', sCaller);

           if not ApplyHook then
           begin
             try
               ApplyHook:= not (csDesigning in LFoundControl.ComponentState);
               LParentForm:=GetParentForm(LFoundControl);
               if (LParentForm<>nil) and ApplyHook then
                 ApplyHook:= Assigned(TColorizerLocalSettings.HookedWindows) and (TColorizerLocalSettings.HookedWindows.IndexOf(LParentForm.ClassName)>=0);
             except
              ApplyHook:=False
             end;
           end;

           if ApplyHook then
           begin
             {$IFDEF DELPHIXE2_UP}
             if TColorizerLocalSettings.Settings.UseVCLStyles and TColorizerLocalSettings.Settings.VCLStylesControls then
             begin
               LStyleServices:= ColorizerStyleServices;

               case iStateId of
                  RBS_UNCHECKEDNORMAL  :  LDetails := LStyleServices.GetElementDetails(tbRadioButtonUncheckedNormal);
                  RBS_UNCHECKEDHOT     :  LDetails := LStyleServices.GetElementDetails(tbRadioButtonUncheckedHot);
                  RBS_UNCHECKEDPRESSED :  LDetails := LStyleServices.GetElementDetails(tbRadioButtonUncheckedPressed);
                  RBS_UNCHECKEDDISABLED:  LDetails := LStyleServices.GetElementDetails(tbRadioButtonUncheckedDisabled);
                  RBS_CHECKEDNORMAL    :  LDetails := LStyleServices.GetElementDetails(tbRadioButtonCheckedNormal);
                  RBS_CHECKEDHOT       :  LDetails := LStyleServices.GetElementDetails(tbRadioButtonCheckedHot);
                  RBS_CHECKEDPRESSED   :  LDetails := LStyleServices.GetElementDetails(tbRadioButtonCheckedPressed);
                  RBS_CHECKEDDISABLED  :  LDetails := LStyleServices.GetElementDetails(tbRadioButtonCheckedDisabled);
               end;

               LBuffer:=TBitmap.Create;
               try
                 LBuffer.SetSize(LSize.cx, LSize.cy);
                 LRect := Rect(0, 0, LSize.cx, LSize.cy);
                 LBuffer.Canvas.Brush.Color := TColorizerLocalSettings.ColorMap.Color;
                 LBuffer.Canvas.FillRect(LRect);
                 LStyleServices.DrawElement(LBuffer.Canvas.Handle, LDetails, LRect);
                 RectCenter(LRect, pRect);
                 BitBlt(dc, LRect.Left, LRect.Top, LSize.cx, LSize.cy, LBuffer.Canvas.Handle, 0, 0, SRCCOPY);
               finally
                 LBuffer.Free;
               end;
               Exit(0);
             end
             else
             {$ENDIF}
             begin

               case iStateId of

                  RBS_UNCHECKEDNORMAL,
                  RBS_UNCHECKEDHOT,
                  RBS_UNCHECKEDPRESSED,
                  RBS_UNCHECKEDDISABLED,

                  RBS_CHECKEDNORMAL,
                  RBS_CHECKEDHOT,
                  RBS_CHECKEDPRESSED,
                  RBS_CHECKEDDISABLED   : begin
                                             LBuffer:=TBitmap.Create;
                                             try
                                               LBuffer.SetSize(LSize.cx, LSize.cy);
                                               LRect := Rect(0, 0, LSize.cx, LSize.cy);


                                               LBuffer.Canvas.Brush.Color:=TColorizerLocalSettings.ColorMap.WindowColor;
                                               LBuffer.Canvas.FillRect(LRect);

                                               if (iStateId= RBS_CHECKEDHOT) or (iStateId= RBS_UNCHECKEDHOT) then
                                                 LBuffer.Canvas.Brush.Color:=TColorizerLocalSettings.ColorMap.SelectedColor
                                               else
                                                 LBuffer.Canvas.Brush.Color:=TColorizerLocalSettings.ColorMap.WindowColor;
                                               LBuffer.Canvas.Pen.Color  :=TColorizerLocalSettings.ColorMap.FontColor;
                                               LBuffer.Canvas.Ellipse(0, 0, LSize.cx, LSize.cy);

                                                case iStateId of
                                                    RBS_CHECKEDNORMAL,
                                                    RBS_CHECKEDHOT,
                                                    RBS_CHECKEDPRESSED,
                                                    RBS_CHECKEDDISABLED   : begin
                                                                              LBuffer.Canvas.Brush.Color:= LBuffer.Canvas.Pen.Color;
                                                                              LBuffer.Canvas.Ellipse(3, 3, 3 + LSize.cx div 2 ,3 + LSize.cy div 2);
                                                                            end;

                                                end;
                                               RectCenter(LRect, pRect);
                                               BitBlt(dc, LRect.Left, LRect.Top, LSize.cx, LSize.cy, LBuffer.Canvas.Handle, 0, 0, SRCCOPY);
                                             finally
                                               LBuffer.Free;
                                             end;
                                             Exit(0);
                                          end;
               end;
             end;
           end;
        end;

    end;
  end
  ;

  Exit(TrampolineDrawThemeBackground(THEME, dc, iPartId, iStateId, pRect, pClipRect));
end;

type
  TWinControlHelper = class helper for TWinControl
  public
    function GetWMNCPaintAddr : Pointer;
  end;


{ TWinControlHelper }

function TWinControlHelper.GetWMNCPaintAddr: Pointer;
var
  MethodAddr: procedure(var Message: TWMNCPaint) of object;
begin
  MethodAddr := Self.WMNCPaint;
  Result     := TMethod(MethodAddr).Code;
end;

procedure InstallHooksUXTheme;
const
  themelib = 'uxtheme.dll';
  sBaseVirtualTreeOriginalWMNCPaint = '@Idevirtualtrees@TBaseVirtualTree@OriginalWMNCPaint$qqrp5HDC__';
begin
  THThemesClasses.Classes := TDictionary<HTHEME, String>.Create();
  THThemesClasses.Windows := TDictionary<HTHEME, HWND>.Create();
  if {$IFDEF DELPHIXE2_UP}StyleServices.Available {and StyleServices.Enabled}  {$ELSE} ThemeServices.ThemesAvailable {$ENDIF} then
  begin
   if StyleServices.Enabled then
   begin
    THThemesClasses.Classes.Add({$IFDEF DELPHIXE2_UP}StyleServices{$ELSE}ThemeServices{$ENDIF}.Theme[teScrollBar], VSCLASS_SCROLLBAR);
    THThemesClasses.Classes.Add({$IFDEF DELPHIXE2_UP}StyleServices{$ELSE}ThemeServices{$ENDIF}.Theme[teTreeview], VSCLASS_TREEVIEW);
    THThemesClasses.Classes.Add({$IFDEF DELPHIXE2_UP}StyleServices{$ELSE}ThemeServices{$ENDIF}.Theme[tebutton], VSCLASS_BUTTON);
    THThemesClasses.Classes.Add({$IFDEF DELPHIXE2_UP}StyleServices{$ELSE}ThemeServices{$ENDIF}.Theme[teToolTip], VSCLASS_TOOLTIP);
    THThemesClasses.Classes.Add({$IFDEF DELPHIXE2_UP}StyleServices{$ELSE}ThemeServices{$ENDIF}.Theme[teListView], VSCLASS_LISTVIEW);
   end;

    TrampolineTWinControl_WMNCPaint     :=InterceptCreate(TWinControl(nil).GetWMNCPaintAddr, @Detour_TWinControl_WMNCPaint);
    if Assigned(DrawThemeText) then
      Trampoline_DrawThemeText    := InterceptCreate(@DrawThemeText,   @Detour_UxTheme_DrawThemeText);
    TrampolineOpenThemeData       := InterceptCreate(themelib, 'OpenThemeData', @Detour_UxTheme_OpenThemeData);
    TrampolineDrawThemeBackground := InterceptCreate(themelib, 'DrawThemeBackground', @Detour_UxTheme_DrawThemeBackground);
    TrampolineBaseVirtualTreeOriginalWMNCPaint := InterceptCreate(sVclIDEModule, sBaseVirtualTreeOriginalWMNCPaint, @Detour_TBaseVirtualTree_OriginalWMNCPaint);
//    TrampolineSetScrollPos  := InterceptCreate(user32,  'SetScrollPos', @Detour_WinApi_SetScrollPos);
//    TrampolineSetScrollInfo := InterceptCreate(user32, 'SetScrollInfo', @Detour_WinApi_SetScrollInfo);
  end;

end;

procedure RemoveHooksUXTheme;
begin
  InterceptRemove(@TrampolineTWinControl_WMNCPaint);
  InterceptRemove(@TrampolineOpenThemeData);
  InterceptRemove(@TrampolineBaseVirtualTreeOriginalWMNCPaint);
  InterceptRemove(@TrampolineDrawThemeBackground);
  InterceptRemove(@Trampoline_DrawThemeText);
//  InterceptRemove(@TrampolineSetScrollPos);
//  InterceptRemove(@TrampolineSetScrollInfo);

  THThemesClasses.Classes.Free;
  THThemesClasses.Windows.Free;
end;


initialization
  ColorizerLock    := TCriticalSection.Create;
finalization
  ColorizerLock.Free;
  ColorizerLock := nil;


end.
