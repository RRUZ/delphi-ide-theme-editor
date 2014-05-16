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

procedure CustomDefaultHandler(Self : TWinControl;var Message);
begin
  LastWinControl:=Self;
  //AddLog('CustomDefaultHandler', Format('%s',[WM_To_String(TMessage(Message).Msg)]));
  TrampolineTWinControl_DefaultHandler(Self, Message);
end;

type
  TWinControlClass = class(TWinControl);
procedure CustomWMNCPaint(Self : TWinControlClass;var Message: TWMNCPaint);
var
  EmptyRect, DrawRect: TRect;
  DC: HDC;
  H, W: Integer;
  AStyle, ExStyle: Integer;
  LCanvas : TCanvas;
begin
  TrampolineTWinControl_WMNCPaint(Self, Message);

    AddLog('CustomWMNCPaint (1)', Self.ClassName);
  if Assigned(TColorizerLocalSettings.ColorMap) and
     (Assigned(TColorizerLocalSettings.HookedWindows) and (TColorizerLocalSettings.HookedWindows.IndexOf(Self.ClassName)>=0)) or
     (Assigned(TColorizerLocalSettings.HookedScrollBars) and (TColorizerLocalSettings.HookedScrollBars.IndexOf(Self.ClassName)>=0)) then
  begin
    AddLog('CustomWMNCPaint', Self.ClassName);
    ExStyle := GetWindowLong(Self.Handle, GWL_EXSTYLE);

    if (ExStyle and WS_EX_CLIENTEDGE) <> 0 then
    AddLog('CustomWMNCPaint', 'WS_EX_CLIENTEDGE');

    begin
      GetWindowRect(Self.Handle, DrawRect);
      OffsetRect(DrawRect, -DrawRect.Left, -DrawRect.Top);
      DC := GetWindowDC(Self.Handle);
      try
        EmptyRect := DrawRect;
        //if EraseLRCorner then
        //begin
          AStyle := GetWindowLong(Self.Handle, GWL_STYLE);
          if ((AStyle and WS_HSCROLL) <> 0) and ((AStyle and WS_VSCROLL) <> 0) then
          begin
            AddLog('CustomWMNCPaint EmptyRect', Self.ClassName);
            W := GetSystemMetrics(SM_CXVSCROLL);
            H := GetSystemMetrics(SM_CYHSCROLL);
            //InflateRect(EmptyRect, -2, -2);
            with EmptyRect do
              if Self.UseRightToLeftScrollBar then
                EmptyRect := Rect(Left, Bottom - H, Left + W, Bottom)
              else
                EmptyRect := Rect(Right - W, Bottom - H, Right, Bottom);
            //FillRect(DC, EmptyRect, GetSysColorBrush(COLOR_WINDOW));
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
        //end;
        with DrawRect do
          ExcludeClipRect(DC, Left + 2, Top + 2, Right - 2, Bottom - 2);
        AddLog('CustomWMNCPaint DrawRect', Self.ClassName);
        //Draw border
//        LCanvas:=TCanvas.Create;
//        try
//          LCanvas.Handle:=DC;
//          LCanvas.Brush.Color:=TColorizerLocalSettings.ColorMap.Color;
//          LCanvas.FillRect(DrawRect);
//        finally
//          LCanvas.Handle:=0;
//          LCanvas.Free;
//        end;

      finally
        ReleaseDC(Self.Handle, DC);
      end;
    end;


  end;

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
var
  s, sCaller, sCaller2 : string;
  LCanvas : TCanvas;
  VCLClassName : string;
  ApplyHook  : Boolean;
  i : integer;
begin
  if not (ScrollBarList.ContainsKey(THEME) and Assigned(TColorizerLocalSettings.ColorMap) and Assigned(TColorizerLocalSettings.Settings)  and TColorizerLocalSettings.Settings.Enabled) then
   Exit(TrampolineDrawThemeBackground(THEME, dc, iPartId, iStateId, pRect, pClipRect));

  try
    if Assigned(LastWinControl) then
      VCLClassName:=LastWinControl.ClassName;
  except
    VCLClassName := '';
  end;

  sCaller := ProcByLevel(1);
  if sCaller<>'' then
    sCaller2 := ProcByLevel(2);

  ApplyHook:= (sCaller='') or  (TColorizerLocalSettings.HookedScrollBars.IndexOf(VCLClassName)>=0) or  (TColorizerLocalSettings.HookedWindows.IndexOf(VCLClassName)>=0);
   //or  AnsiContainsText(TColorizerLocalSettings.HookedWindowsText, sCaller)  or AnsiContainsText(TColorizerLocalSettings.HookedScrollBarsText, sCaller)
   //or  ((sCaller2<>'') and AnsiContainsText(TColorizerLocalSettings.HookedWindowsText, sCaller2))  or ((sCaller2<>'') and AnsiContainsText(TColorizerLocalSettings.HookedScrollBarsText, sCaller2));

  //look for hooked controls in the caller level 1
  if not ApplyHook and (sCaller<>'') then
     for s in SplitString(sCaller,'.') do
     begin
       ApplyHook:= StartsText('T', s) and ( (TColorizerLocalSettings.HookedWindows.IndexOf(s)>=0) or (TColorizerLocalSettings.HookedScrollBars.IndexOf(s)>=0) );
       if ApplyHook then break;
     end;

  //look for hooked controls in the caller level 2
  if not ApplyHook and (sCaller2<>'') then
     for s in SplitString(sCaller2,'.') do
     begin
       ApplyHook:= StartsText('T', s) and ( (TColorizerLocalSettings.HookedWindows.IndexOf(s)>=0) or (TColorizerLocalSettings.HookedScrollBars.IndexOf(s)>=0) );
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
    if ScrollBarList.ContainsKey(THEME) then
    begin
       for i :=1 to 5 do
       begin
         sCaller := ProcByLevel(i);
         AddLog('Scrollbar Ignored', Format(' %d %s %s',[i, VCLClassName, sCaller]));
       end;
         AddLog('Scrollbar Ignored', Format('%s',['------------------------------------------------']));
    end;

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
initialization

if {$IFDEF DELPHIXE2_UP}StyleServices.Available {$ELSE} ThemeServices.ThemesAvailable {$ENDIF} then
begin
  ScrollBarList := TDictionary<HTHEME, String>.Create();

  TrampolineTWinControl_DefaultHandler:=InterceptCreate(@TWinControl.DefaultHandler, @CustomDefaultHandler);
  TrampolineTWinControl_WMNCPaint     :=InterceptCreate(TWinControl(nil).GetWMNCPaintAddr, @CustomWMNCPaint);

  OpenThemeDataOrgPointer  := GetProcAddress(GetModuleHandle('UxTheme.dll'),  'OpenThemeData');
  if Assigned (OpenThemeDataOrgPointer) then
   TrampolineOpenThemeData := InterceptCreate(OpenThemeDataOrgPointer, @CustomOpenThemeData);

//  CloseThemeDataOrgPointer  := GetProcAddress(GetModuleHandle('UxTheme.dll'),  'CloseThemeData');
//  if Assigned (CloseThemeDataOrgPointer) then
//   TrampolineCloseThemeData := InterceptCreate(CloseThemeDataOrgPointer, @CustomCloseThemeData);

  DrawThemeBackgroundOrgPointer  := GetProcAddress(GetModuleHandle('UxTheme.dll'), 'DrawThemeBackground');
  if Assigned (DrawThemeBackgroundOrgPointer) then
   TrampolineDrawThemeBackground := InterceptCreate(DrawThemeBackgroundOrgPointer, @CustomDrawThemeBackground);


end;

finalization
if Assigned (TrampolineTWinControl_DefaultHandler) then
  InterceptRemove(@TrampolineTWinControl_DefaultHandler);

if Assigned (TrampolineTWinControl_WMNCPaint) then
  InterceptRemove(@TrampolineTWinControl_WMNCPaint);

if Assigned (TrampolineOpenThemeData) then
  InterceptRemove(@TrampolineOpenThemeData);

//if Assigned (TrampolineCloseThemeData) then
//  InterceptRemove(@TrampolineCloseThemeData);
//

if Assigned (TrampolineDrawThemeBackground) then
  InterceptRemove(@TrampolineDrawThemeBackground);

if {$IFDEF DELPHIXE2_UP}StyleServices.Available {$ELSE} ThemeServices.ThemesAvailable {$ENDIF} then
  ScrollBarList.Free;

end.
