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
// Portions created by Rodrigo Ruz V. are Copyright (C) 2011-2014 Rodrigo Ruz V.
// All Rights Reserved.
//
//**************************************************************************************************

unit Colorizer.Hooks.UxTheme;


interface
{$I ..\Common\Jedi.inc}


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
{$ELSE}
  Types,
  Themes,
{$ENDIF}
  Controls,
  uMisc,
  JclDebug,
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

  type
    THThemesClasses = class
    public
     class var ScrollBars: TDictionary<HTHEME, String>;
     class var TreeView  : TDictionary<HTHEME, String>;
     class var Button    : TDictionary<HTHEME, String>;
     class var ToolTip   : TDictionary<HTHEME, String>;
    end;

  var
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
  TrampolineSetScrollPos              : function (hWnd: HWND; nBar, nPos: Integer; bRedraw: BOOL): Integer; stdcall = nil;
  TrampolineSetScrollInfo             : function (hWnd: HWND; BarFlag: Integer; const ScrollInfo: TScrollInfo; Redraw: BOOL): Integer; stdcall = nil;

function Detour_UxTheme_DrawThemeText(hTheme: HTHEME; hdc: HDC; iPartId, iStateId: Integer;  pszText: LPCWSTR; iCharCount: Integer; dwTextFlags, dwTextFlags2: DWORD; const pRect: TRect): HRESULT; stdcall;
begin
 //AddLog('Detour_UxTheme_DrawThemeText', string(pszText));
 Exit(Trampoline_DrawThemeText(hTheme, hdc, iPartId, iStateId, pszText, iCharCount, dwTextFlags, dwTextFlags2, pRect));
end;

function Detour_WinApi_SetScrollPos(hWnd: HWND; nBar, nPos: Integer; bRedraw: BOOL): Integer; stdcall;
begin
  LastScrollWinControl:=FindControl(hWnd);
  Exit(TrampolineSetScrollPos(hWnd, nBar, nPos, bRedraw));
end;

function Detour_WinApi_SetScrollInfo(hWnd: HWND; BarFlag: Integer; const ScrollInfo: TScrollInfo; Redraw: BOOL): Integer; stdcall;
begin
  LastScrollWinControl:=FindControl(hWnd);
  Exit(TrampolineSetScrollInfo(hWnd, BarFlag, ScrollInfo, Redraw));
end;

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
  Result := TrampolineOpenThemeData(hwnd, pszClassList);
  //AddLog('Detour_UxTheme_OpenThemeData', 'pszClassList '+string(pszClassList));

  if SameText(pszClassList, VSCLASS_SCROLLBAR) then
  begin
    if not THThemesClasses.ScrollBars.ContainsKey(Result) then
      THThemesClasses.ScrollBars.Add(Result, pszClassList);
  end
  else
  if SameText(pszClassList, VSCLASS_TREEVIEW) then
  begin
    if not THThemesClasses.TreeView.ContainsKey(Result) then
      THThemesClasses.TreeView.Add(Result, pszClassList);
  end
  else
  if SameText(pszClassList, VSCLASS_BUTTON) then
  begin
    if not THThemesClasses.Button.ContainsKey(Result) then
      THThemesClasses.Button.Add(Result, pszClassList);
  end
  else
  if SameText(pszClassList, VSCLASS_TOOLTIP) then
  begin
    if not THThemesClasses.ToolTip.ContainsKey(Result) then
      THThemesClasses.ToolTip.Add(Result, pszClassList);
  end
  ;
end;


//function CustomCloseThemeData(hTheme: HTHEME): HRESULT; stdcall;
//begin
//    if ScrollBarList.ContainsKey(hTheme) then
//      ScrollBarList.Remove(hTheme);
//   Result := TrampolineCloseThemeData(hTheme);
//end;

{$IFDEF DELPHIXE2_UP}
function CustomStyleDrawThemeBackground(THEME: HTHEME; dc: HDC;  iPartId, iStateId: Integer; const pRect: TRect; pClipRect: pRect) : HRESULT; stdcall;
var
  LDetails: TThemedElementDetails;
  LStyle: TCustomStyleServices;
  LScrollDetails: TThemedScrollBar;
begin
  LStyle := TStyleManager.Style['Jet'];
  if THThemesClasses.ScrollBars.ContainsKey(THEME) then
  begin
    LScrollDetails := tsScrollBarRoot;
    LDetails.Element := TThemedElement.teScrollBar;
    LDetails.Part := iPartId;
    LDetails.State := iStateId;
    LDetails := LStyle.GetElementDetails(TThemedScrollBar.tsThumbBtnHorzNormal);

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
    LDetails := LStyle.GetElementDetails(LScrollDetails);
    LStyle.DrawElement(dc, LDetails, pRect, pClipRect);
    Exit(0);
  end
  else
  begin
    Exit(TrampolineDrawThemeBackground(THEME, dc, iPartId, iStateId, pRect, pClipRect));
  end;
end;
{$ENDIF}

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
  s, sCaller, sCaller2 : string;
  LCanvas : TCanvas;
  {WClassName, }VCLClassName : string;
  ApplyHook  : Boolean;
  LParentForm : TCustomForm;
  LHWND : HWND;
  LFoundControl : TWinControl;
  LBuffer   : TBitmap;
  LRect     : TRect;
  LSize     : TSize;
  SavedIndex : integer;

  function DrawScrollBarVCLStyles : HRESULT;
  var
    LDetails: TThemedElementDetails;
    LStyle: TCustomStyleServices;
    LScrollDetails: TThemedScrollBar;
//    B: TBitmap;
//    R : TRect;
  begin
    Result:=0;
    LStyle := ColorizerStyleServices;
    if THThemesClasses.ScrollBars.ContainsKey(THEME) then
    begin
      LScrollDetails := tsScrollBarRoot;
      LDetails.Element := TThemedElement.teScrollBar;
      LDetails.Part := iPartId;
      LDetails.State := iStateId;
      LDetails := LStyle.GetElementDetails(TThemedScrollBar.tsThumbBtnHorzNormal);

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
      LDetails := LStyle.GetElementDetails(LScrollDetails);

//      B := TBitmap.Create;
//      try
//        B.Width := pRect.Width;
//        B.Height := pRect.Height;
//        MoveWindowOrg(B.Canvas.Handle, -pRect.Left, -pRect.Top);
//        R:=Rect(0 , 0, B.Width, B.Height);
//        LStyle.DrawElement(b.Canvas.Handle, LDetails, R, nil);
//        //BitBlt(DC, Left, Top, B.Width, B.Height, B.Canvas.Handle, 0, 0, SRCCOPY);
//        BitBlt(dc, pRect.Left, pRect.Top, B.Width, B.Height, B.Canvas.Handle, 0, 0, SRCCOPY);
//      finally
//        B.Free;
//      end;
      LStyle.DrawElement(dc, LDetails, pRect, pClipRect);
      Exit(0);
    end;
  end;

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
  if not ( (THThemesClasses.ScrollBars.ContainsKey(THEME) or THThemesClasses.TreeView.ContainsKey(THEME)
     or THThemesClasses.Button.ContainsKey(THEME) or THThemesClasses.ToolTip.ContainsKey(THEME) ) and Assigned(TColorizerLocalSettings.ColorMap) and Assigned(TColorizerLocalSettings.Settings)  and TColorizerLocalSettings.Settings.Enabled) then
   Exit(TrampolineDrawThemeBackground(THEME, dc, iPartId, iStateId, pRect, pClipRect));

  if THThemesClasses.ToolTip.ContainsKey(THEME) then
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
  if THThemesClasses.ScrollBars.ContainsKey(THEME) then
  begin
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

//    AddLog('ScrollBar','LHWND '+IntToHex(LHWND, 8));
//    if ApplyHook and (LHWND<>0) then
//    begin
//      WClassName := GetWindowClassName(LHWND);
//      ApplyHook:= not ((WClassName<>'') and (TColorizerLocalSettings.WinAPIClasses.IndexOf(WClassName)>=0));
//      AddLog('ScrollBar','WClassName '+WClassName);
//    end;

    if ApplyHook then
    begin
      if TColorizerLocalSettings.Settings.UseVCLStyles and TColorizerLocalSettings.Settings.VCLStylesScrollBars then
        Exit(DrawScrollBarVCLStyles())
      else
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
  if THThemesClasses.TreeView.ContainsKey(THEME) then
  begin

    if (iPartId = TVP_GLYPH) and (iStateId=GLPS_OPENED) and ((pRect.Right-pRect.Left)=9) then
    begin
      sCaller  := ProcByLevel(4);
                   //VirtualTreeView           //Fix CustomPropListBox, because LFoundControl is nil sometimes (ex : scroll)
      ApplyHook:= (sCaller = '') or (SameText('PropBox.TCustomPropListBox.DrawPropItem', sCaller));
      if not ApplyHook then
      begin
        //if SameText('PropBox.TCustomPropListBox.DrawPropItem', sCaller) then
        begin
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
         Exit(0);
      end;
    end;
//      sCaller  := ProcByLevel(4);
//      AddLog('THThemesClasses.TreeView','Ignored '+sCaller);
  end
  else
  if THThemesClasses.Button.ContainsKey(THEME) then
  begin
    if (iPartId = BP_CHECKBOX) then
    begin
      LSize.cx:= 13;
      LSize.cy:= 13;
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


//      sCaller  := ProcByLevel(4);
//      AddLog('THThemesClasses.Button','VCLClassName '+VCLClassName+' - Ignored '+sCaller);
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


function GetBplMethodAddress(Method: Pointer): Pointer;
type
  PJmpCode = ^TJmpCode;
  TJmpCode = packed record
    Code: Word;
    Addr: ^Pointer;
  end;
const
  csJmpCode = $E9;
  csJmp32Code = $25FF;
begin
  if PJmpCode(Method)^.Code = csJmp32Code then
    Result := PJmpCode(Method)^.Addr^
  else
    Result := Method;
end;

const
  sBaseVirtualTreeOriginalWMNCPaint = '@Idevirtualtrees@TBaseVirtualTree@OriginalWMNCPaint$qqrp5HDC__';

var
  pHook, psBaseVirtualTreeOriginalWMNCPaint : Pointer;
  VclIDEModule : HMODULE;

initialization

if {$IFDEF DELPHIXE2_UP}StyleServices.Available {$ELSE} ThemeServices.ThemesAvailable {$ENDIF} then
begin
  THThemesClasses.ScrollBars := TDictionary<HTHEME, String>.Create();
  THThemesClasses.ScrollBars.Add( {$IFDEF DELPHIXE2_UP}StyleServices{$ELSE}ThemeServices{$ENDIF}.Theme[teScrollBar], VSCLASS_SCROLLBAR);

  THThemesClasses.TreeView := TDictionary<HTHEME, String>.Create();
  THThemesClasses.TreeView.Add({$IFDEF DELPHIXE2_UP}StyleServices{$ELSE}ThemeServices{$ENDIF}.Theme[teTreeview], VSCLASS_TREEVIEW);

  THThemesClasses.Button := TDictionary<HTHEME, String>.Create();
  THThemesClasses.Button.Add({$IFDEF DELPHIXE2_UP}StyleServices{$ELSE}ThemeServices{$ENDIF}.Theme[tebutton], VSCLASS_BUTTON);

  THThemesClasses.ToolTip := TDictionary<HTHEME, String>.Create();
  THThemesClasses.ToolTip.Add({$IFDEF DELPHIXE2_UP}StyleServices{$ELSE}ThemeServices{$ENDIF}.Theme[teToolTip], VSCLASS_TOOLTIP);

  TrampolineTWinControl_WMNCPaint     :=InterceptCreate(TWinControl(nil).GetWMNCPaintAddr, @Detour_TWinControl_WMNCPaint);

  if Assigned(DrawThemeText) then
    Trampoline_DrawThemeText            := InterceptCreate(@DrawThemeText,   @Detour_UxTheme_DrawThemeText);

  OpenThemeDataOrgPointer  := GetProcAddress(GetModuleHandle('UxTheme.dll'),  'OpenThemeData');
  if Assigned (OpenThemeDataOrgPointer) then
   TrampolineOpenThemeData := InterceptCreate(OpenThemeDataOrgPointer, @Detour_UxTheme_OpenThemeData);

  DrawThemeBackgroundOrgPointer  := GetProcAddress(GetModuleHandle('UxTheme.dll'), 'DrawThemeBackground');
  if Assigned (DrawThemeBackgroundOrgPointer) then
   TrampolineDrawThemeBackground := InterceptCreate(DrawThemeBackgroundOrgPointer, @Detour_UxTheme_DrawThemeBackground);

  VclIDEModule := LoadLibrary(sVclIDEModule);
  if VclIDEModule<>0 then
  begin
   psBaseVirtualTreeOriginalWMNCPaint := GetBplMethodAddress(GetProcAddress(VclIDEModule, sBaseVirtualTreeOriginalWMNCPaint));
   if Assigned(psBaseVirtualTreeOriginalWMNCPaint) then
    TrampolineBaseVirtualTreeOriginalWMNCPaint := InterceptCreate(psBaseVirtualTreeOriginalWMNCPaint, @Detour_TBaseVirtualTree_OriginalWMNCPaint);
  end;

  pHook  := GetProcAddress(GetModuleHandle(user32), 'SetScrollPos');
  if Assigned (pHook) then
   TrampolineSetScrollPos := InterceptCreate(pHook, @Detour_WinApi_SetScrollPos);

  pHook  := GetProcAddress(GetModuleHandle(user32), 'SetScrollInfo');
  if Assigned (pHook) then
   TrampolineSetScrollInfo := InterceptCreate(pHook, @Detour_WinApi_SetScrollInfo);
end;

finalization

if Assigned (TrampolineTWinControl_WMNCPaint) then
  InterceptRemove(@TrampolineTWinControl_WMNCPaint);

if Assigned (TrampolineOpenThemeData) then
  InterceptRemove(@TrampolineOpenThemeData);

if Assigned (TrampolineBaseVirtualTreeOriginalWMNCPaint) then
  InterceptRemove(@TrampolineBaseVirtualTreeOriginalWMNCPaint);

if Assigned (TrampolineDrawThemeBackground) then
  InterceptRemove(@TrampolineDrawThemeBackground);

if Assigned(Trampoline_DrawThemeText) then
  InterceptRemove(@Trampoline_DrawThemeText);

if Assigned (TrampolineSetScrollPos) then
  InterceptRemove(@TrampolineSetScrollPos);

if Assigned (TrampolineSetScrollInfo) then
  InterceptRemove(@TrampolineSetScrollInfo);

if {$IFDEF DELPHIXE2_UP}StyleServices.Available {$ELSE} ThemeServices.ThemesAvailable {$ENDIF} then
  THThemesClasses.ScrollBars.Free;
  THThemesClasses.TreeView.Free;
  THThemesClasses.Button.Free;
  THThemesClasses.ToolTip.Free;
end.
