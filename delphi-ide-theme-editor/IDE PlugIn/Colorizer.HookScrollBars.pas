//**************************************************************************************************
//
// Unit Colorizer.HookScrollBars
// unit Colorizer.HookScrollBars for the Delphi IDE Colorizer
//
// The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
// you may not use this file except in compliance with the License. You may obtain a copy of the
// License at http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
// ANY KIND, either express or implied. See the License for the specific language governing rights
// and limitations under the License.
//
// The Original Code is Colorizer.HookScrollBars.pas.
//
// The Initial Developer of the Original Code is Rodrigo Ruz V.
// Portions created by Rodrigo Ruz V. are Copyright (C) 2011-2014 Rodrigo Ruz V.
// All Rights Reserved.
//
//**************************************************************************************************

unit Colorizer.HookScrollBars;

{$I ..\Common\Jedi.inc}

interface

implementation
uses
  Colorizer.Utils,
  Generics.Collections,
  Graphics,
  Classes,
  Windows,
  UxTheme,
{$IFDEF DELPHIXE2_UP}
  Vcl.Styles,
  Vcl.Themes,
{$ELSE}
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
  Dialogs,
  DDetours;

  var
  ScrollBarList: TDictionary<HTHEME, String>;
  DrawThemeBackgroundOrgPointer : Pointer = nil;
  OpenThemeDataOrgPointer       : Pointer = nil;
  //CloseThemeDataOrgPointer      : Pointer = nil;
  LastWinControl  : TWinControl = nil;

  TrampolineOpenThemeData             : function(hwnd: hwnd; pszClassList: LPCWSTR) : HTHEME; stdcall = nil;
  //TrampolineCloseThemeData            : function(hTheme: HTHEME): HRESULT; stdcall = nil;
  TrampolineDrawThemeBackground       : function(HTHEME: HTHEME; hdc: hdc; iPartId, iStateId: Integer; const pRect: TRect; pClipRect: pRect) : HRESULT; stdcall = nil;
  TrampolineTWinControl_DefaultHandler: procedure (Self : TWinControl;var Message) = nil;
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


function CustomSetScrollPos(hWnd: HWND; nBar, nPos: Integer; bRedraw: BOOL): Integer; stdcall;
begin
  LastWinControl:=FindControl(hWnd);
  Exit(TrampolineSetScrollPos(hWnd, nBar, nPos, bRedraw));
end;

function CustomSetScrollInfo(hWnd: HWND; BarFlag: Integer; const ScrollInfo: TScrollInfo; Redraw: BOOL): Integer; stdcall;
begin
  LastWinControl:=FindControl(hWnd);
  Exit(TrampolineSetScrollInfo(hWnd, BarFlag, ScrollInfo, Redraw));
end;

procedure CustomDefaultHandler(Self : TWinControl;var Message);
begin
  LastWinControl:=Self;
  TrampolineTWinControl_DefaultHandler(Self, Message);
end;

type
  TWinControlClass = class(TWinControl);

procedure CustomWMNCPaint(Self : TWinControlClass;var Message: TWMNCPaint);
begin
  TrampolineTWinControl_WMNCPaint(Self, Message);
  if csDesigning in Self.ComponentState then  exit;
  if Assigned(TColorizerLocalSettings.Settings) and (TColorizerLocalSettings.Settings.Enabled) then
    DrawNCBorder(Self, True);
end;

procedure  CustomBaseVirtualTreeOriginalWMNCPaint(Self : TCustomControl;DC: HDC);
begin
 TrampolineBaseVirtualTreeOriginalWMNCPaint(Self, DC);
 if csDesigning in Self.ComponentState then  exit;
//Draw the bottom right corner when both scrollbars are active in the TBaseVirtualTree
 if Assigned(TColorizerLocalSettings.Settings) and (TColorizerLocalSettings.Settings.Enabled) then
   DrawNCBorder(TWinControlClass(Self), True);
end;

function CustomOpenThemeData(hwnd: hwnd; pszClassList: LPCWSTR) : HTHEME; stdcall;
begin
  Result := TrampolineOpenThemeData(hwnd, pszClassList);
  if SameText(pszClassList, VSCLASS_SCROLLBAR) then
  begin
    if not ScrollBarList.ContainsKey(Result) then
      ScrollBarList.Add(Result, pszClassList);
  end;
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
  if ScrollBarList.ContainsKey(THEME) then
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

function CustomDrawThemeBackground(THEME: HTHEME; dc: HDC;  iPartId, iStateId: Integer; const pRect: TRect; pClipRect: pRect) : HRESULT; stdcall;

  procedure DrawLine(Canvas: TCanvas; FromX, FromY, ToX, ToY: Integer);
  begin
    Canvas.MoveTo(FromX, FromY);
    Canvas.LineTo(ToX, ToY);
  end;


var
  s, sCaller, sCaller2 : string;
  LCanvas : TCanvas;
  VCLClassName : string;
  ApplyHook  : Boolean;
//  i : integer;
  LHWND : HWND;
  LFoundControl : TWinControl;
begin
  if not (ScrollBarList.ContainsKey(THEME) and Assigned(TColorizerLocalSettings.ColorMap) and Assigned(TColorizerLocalSettings.Settings)  and TColorizerLocalSettings.Settings.Enabled) then
   Exit(TrampolineDrawThemeBackground(THEME, dc, iPartId, iStateId, pRect, pClipRect));

  ApplyHook:=False;
  sCaller :='';
  sCaller2:='';
  VCLClassName:='';
  LFoundControl:=nil;

  try
    if Assigned(LastWinControl) then
      VCLClassName:=LastWinControl.ClassName;
  except
    VCLClassName := '';
  end;

  LHWND:=WindowFromDC(dc);
  if LHWND<>0 then
   begin
    LFoundControl := FindControl(LHWND);
    if LFoundControl<>nil then
      VCLClassName:= LFoundControl.ClassName;
   end;

  if LFoundControl<>nil then
   try ApplyHook:= not (csDesigning in LFoundControl.ComponentState) and (TColorizerLocalSettings.HookedScrollBars.IndexOf(VCLClassName)>=0) or  (TColorizerLocalSettings.HookedWindows.IndexOf(VCLClassName)>=0);  except ApplyHook:=False end;

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

  if ApplyHook then
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
                                    LCanvas.Brush.Color:=TColorizerLocalSettings.ColorMap.MenuColor
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
                                    LCanvas.Brush.Color:=TColorizerLocalSettings.ColorMap.MenuColor
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
                                    LCanvas.Brush.Color:=TColorizerLocalSettings.ColorMap.MenuColor
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
                                    LCanvas.Brush.Color:=TColorizerLocalSettings.ColorMap.MenuColor
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
          LCanvas.Brush.Color:=TColorizerLocalSettings.ColorMap.MenuColor
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
          LCanvas.Brush.Color:=TColorizerLocalSettings.ColorMap.MenuColor
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
  end
  else
  begin
//    if ScrollBarList.ContainsKey(THEME) then
//    begin
//       for i :=1 to 5 do
//       begin
//         sCaller := ProcByLevel(i);
//         AddLog('Scrollbar Ignored', Format(' %d %s %s',[i, VCLClassName, sCaller]));
//       end;
//         AddLog('Scrollbar Ignored', Format('%s',['------------------------------------------------']));
//    end;

    Exit(TrampolineDrawThemeBackground(THEME, dc, iPartId, iStateId, pRect, pClipRect));
  end;
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

{$IFDEF DELPHIXE}  sVclIDEModule =  'vclide150.bpl';{$ENDIF}
{$IFDEF DELPHIXE2} sVclIDEModule =  'vclide160.bpl';{$ENDIF}
{$IFDEF DELPHIXE3} sVclIDEModule =  'vclide170.bpl';{$ENDIF}
{$IFDEF DELPHIXE4} sVclIDEModule =  'vclide180.bpl';{$ENDIF}
{$IFDEF DELPHIXE5} sVclIDEModule =  'vclide190.bpl';{$ENDIF}
{$IFDEF DELPHIXE6} sVclIDEModule =  'vclide200.bpl';{$ENDIF}
var
  pHook, psBaseVirtualTreeOriginalWMNCPaint : Pointer;
  VclIDEModule : HMODULE;

initialization

if {$IFDEF DELPHIXE2_UP}StyleServices.Available {$ELSE} ThemeServices.ThemesAvailable {$ENDIF} then
begin
  ScrollBarList := TDictionary<HTHEME, String>.Create();
  ScrollBarList.Add( {$IFDEF DELPHIXE2_UP}StyleServices{$ELSE}ThemeServices{$ENDIF}.Theme[teScrollBar], VSCLASS_SCROLLBAR);

  TrampolineTWinControl_DefaultHandler:=InterceptCreate(@TWinControl.DefaultHandler, @CustomDefaultHandler);
  TrampolineTWinControl_WMNCPaint     :=InterceptCreate(TWinControl(nil).GetWMNCPaintAddr, @CustomWMNCPaint);

  OpenThemeDataOrgPointer  := GetProcAddress(GetModuleHandle('UxTheme.dll'),  'OpenThemeData');
  if Assigned (OpenThemeDataOrgPointer) then
   TrampolineOpenThemeData := InterceptCreate(OpenThemeDataOrgPointer, @CustomOpenThemeData);

  DrawThemeBackgroundOrgPointer  := GetProcAddress(GetModuleHandle('UxTheme.dll'), 'DrawThemeBackground');
  if Assigned (DrawThemeBackgroundOrgPointer) then
   TrampolineDrawThemeBackground := InterceptCreate(DrawThemeBackgroundOrgPointer, @CustomDrawThemeBackground);

  VclIDEModule := LoadLibrary(sVclIDEModule);
  if VclIDEModule<>0 then
  begin
   psBaseVirtualTreeOriginalWMNCPaint := GetBplMethodAddress(GetProcAddress(VclIDEModule, sBaseVirtualTreeOriginalWMNCPaint));
   if Assigned(psBaseVirtualTreeOriginalWMNCPaint) then
    TrampolineBaseVirtualTreeOriginalWMNCPaint := InterceptCreate(psBaseVirtualTreeOriginalWMNCPaint, @CustomBaseVirtualTreeOriginalWMNCPaint);
  end;

  pHook  := GetProcAddress(GetModuleHandle(user32), 'SetScrollPos');
  if Assigned (pHook) then
   TrampolineSetScrollPos := InterceptCreate(pHook, @CustomSetScrollPos);

  pHook  := GetProcAddress(GetModuleHandle(user32), 'SetScrollInfo');
  if Assigned (pHook) then
   TrampolineSetScrollInfo := InterceptCreate(pHook, @CustomSetScrollInfo);
end;

finalization
if Assigned (TrampolineTWinControl_DefaultHandler) then
  InterceptRemove(@TrampolineTWinControl_DefaultHandler);

if Assigned (TrampolineTWinControl_WMNCPaint) then
  InterceptRemove(@TrampolineTWinControl_WMNCPaint);

if Assigned (TrampolineOpenThemeData) then
  InterceptRemove(@TrampolineOpenThemeData);

if Assigned (TrampolineBaseVirtualTreeOriginalWMNCPaint) then
  InterceptRemove(@TrampolineBaseVirtualTreeOriginalWMNCPaint);

if Assigned (TrampolineDrawThemeBackground) then
  InterceptRemove(@TrampolineDrawThemeBackground);

if Assigned (TrampolineSetScrollPos) then
  InterceptRemove(@TrampolineSetScrollPos);

if Assigned (TrampolineSetScrollInfo) then
  InterceptRemove(@TrampolineSetScrollInfo);

if {$IFDEF DELPHIXE2_UP}StyleServices.Available {$ELSE} ThemeServices.ThemesAvailable {$ENDIF} then
  ScrollBarList.Free;
end.
