//**************************************************************************************************
//
// Unit Colorizer.Hooks.Windows
// unit Colorizer.Hooks.Windows for the Delphi IDE Colorizer
//
// The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
// you may not use this file except in compliance with the License. You may obtain a copy of the
// License at http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
// ANY KIND, either express or implied. See the License for the specific language governing rights
// and limitations under the License.
//
// The Original Code is Colorizer.Hooks.Windows.pas.
//
// The Initial Developer of the Original Code is Rodrigo Ruz V.
// Portions created by Rodrigo Ruz V. are Copyright (C) 2011-2014 Rodrigo Ruz V.
// All Rights Reserved.
//
//**************************************************************************************************
unit Colorizer.Hooks.Windows;

interface
{$I ..\Common\Jedi.inc}

procedure InstallHooksWinAPI();
procedure RemoveHooksWinAPI();

implementation

uses
 Colorizer.Utils,
 uMIsc,
 DDetours,
 JclDebug,
 Windows,
 Graphics,
 Controls,
 SysUtils,
 Classes,
 Types,
 GraphUtil,
{$IFDEF DELPHIXE2_UP}
 Colorizer.Vcl.Styles,
 VCl.Themes,
{$ENDIF}
 Colorizer.Hooks,
 Colorizer.Hooks.IDE,
 Forms;


var
  Trampoline_DrawText                      : function (hDC: HDC; lpString: LPCWSTR; nCount: Integer;  var lpRect: TRect; uFormat: UINT): Integer; stdcall = nil;
  Trampoline_DrawTextEx                    : function (DC: HDC; lpchText: LPCWSTR; cchText: Integer; var p4: TRect;  dwDTFormat: UINT; DTParams: PDrawTextParams): Integer; stdcall = nil;
  Trampoline_ExtTextOutW                   : function (DC: HDC; X, Y: Integer; Options: Longint; Rect: PRect; Str: LPCWSTR; Count: Longint; Dx: PInteger): BOOL; stdcall = nil;
  Trampoline_GetSysColor                   : function (nIndex: Integer): DWORD; stdcall = nil;
  Trampoline_DrawFrameControl              : function (DC: HDC; Rect: PRect; uType, uState: UINT): BOOL; stdcall = nil;
  Trampoline_DrawEdge                      : function (hdc: HDC; var qrc: TRect; edge: UINT; grfFlags: UINT): BOOL; stdcall = nil;

//Hook DrawEdge WinApi function
function Detour_WinApi_DrawEdge(hdc: HDC; var qrc: TRect; edge: UINT; grfFlags: UINT): BOOL; stdcall;
var
 LCanvas : TCanvas;
 OrgHWND : HWND;
 LWinControl : TWinControl;
 LParentForm : TCustomForm;
 SavedIndex  : Integer;
 WClassName  : string;
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
  end
  else
  if OrgHWND<>0 then
  begin
   WClassName := GetWindowClassName(OrgHWND);
   if (WClassName<>'') and Assigned(TColorizerLocalSettings.WinAPIClasses) and (TColorizerLocalSettings.WinAPIClasses.IndexOf(WClassName)>=0) then
     Exit(Trampoline_DrawEdge(hdc, qrc, edge, grfFlags));
  end;

   case  edge of
      BDR_RAISEDINNER,
      EDGE_SUNKEN,
      EDGE_ETCHED,
      EDGE_BUMP,
      EDGE_RAISED :
        begin
          SavedIndex:=SaveDC(hdc);
          try
            LCanvas:=TCanvas.Create;
            try
              LCanvas.Handle:=hdc;
               if (BF_RECT and grfFlags = BF_RECT) then
               begin
                LCanvas.Brush.Color := TColorizerLocalSettings.ColorMap.WindowColor;
                LCanvas.Pen.Color   := TColorizerLocalSettings.ColorMap.FrameTopLeftOuter;
                LCanvas.Rectangle(qrc);
               end;
            finally
              LCanvas.Handle:=0;
              LCanvas.Free;
            end;
          finally
            if SavedIndex<>0 then
              RestoreDC(hdc, SavedIndex);
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
{$IFDEF DELPHIXE2_UP}
 LDetails: TThemedElementDetails;
 LStyleServices: TCustomStyleServices;
{$ENDIF}
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
         {$IFDEF DELPHIXE2_UP}
         if TColorizerLocalSettings.Settings.UseVCLStyles and TColorizerLocalSettings.Settings.VCLStylesControls then
         begin
           LStyleServices:= ColorizerStyleServices;
           if (DFCS_CHECKED and uState = DFCS_CHECKED) then
            LDetails := LStyleServices.GetElementDetails(tbCheckBoxCheckedNormal)
           else
            LDetails := LStyleServices.GetElementDetails(tbCheckBoxUncheckedNormal);

          LCanvas.Brush.Color := TColorizerLocalSettings.ColorMap.Color;
          LCanvas.FillRect(Rect^);
          LStyleServices.DrawElement(LCanvas.Handle, LDetails, Rect^);
         end
         else
         {$ENDIF}
         begin
           if (DFCS_CHECKED and uState = DFCS_CHECKED) then
           begin
            LCanvas.Brush.Color:= TColorizerLocalSettings.ColorMap.WindowColor;
            LCanvas.Pen.Color  :=TColorizerLocalSettings.ColorMap.FontColor;
//             if (DFCS_HOT and uState = DFCS_HOT) then
//              LCanvas.Pen.Color  :=TColorizerLocalSettings.ColorMap.SelectedColor;
            LCanvas.Rectangle(Rect^);
            DrawCheck(LCanvas, Point(Rect^.Left+3, Rect^.Top+6), 2, False);
           end
           else
           begin
            LCanvas.Brush.Color:=TColorizerLocalSettings.ColorMap.WindowColor;
            LCanvas.Pen.Color  :=TColorizerLocalSettings.ColorMap.FontColor;
//             if (DFCS_HOT and uState = DFCS_HOT) then
//              LCanvas.Pen.Color  :=TColorizerLocalSettings.ColorMap.SelectedColor;
            LCanvas.Rectangle(Rect^);
           end;
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

//because this component is not using the colors set via RTTI
//Note  : This is a temporal workaround.
function Detour_WinApi_DrawText(hDC: HDC; lpString: LPCWSTR; nCount: Integer;  var lpRect: TRect; uFormat: UINT): Integer; stdcall;
const
 sCustomVirtualStringTreeSignature  = 'IDEVirtualTrees.TCustomVirtualStringTree.PaintNormalText';
 sVirtualTreeHintWindow             = 'IDEVirtualTrees.TVirtualTreeHintWindow.AnimationCallback';
var
  sCaller : string;
  OrgColor, LFontColor: Cardinal;
  RestoreColor : Boolean;
begin
 OrgColor:=0;
 RestoreColor:=False;
 sCaller:='';

 if (uFormat=2084) and Assigned(TColorizerLocalSettings.Settings) and TColorizerLocalSettings.Settings.Enabled and Assigned(TColorizerLocalSettings.ColorMap) then
 begin
   OrgColor:=GetTextColor(hDC);
   LFontColor :=ColorToRGB(TColorizerLocalSettings.ColorMap.FontColor);

   if (OrgColor=0) and (LFontColor<>OrgColor) then
   begin
    sCaller := ProcByLevel(2);
    if SameText(sCaller, sVirtualTreeHintWindow) then
    begin
//       AddLog('Detour_WinApi_DrawTextEx', sCaller);
//       AddLog('Detour_WinApi_DrawText', 'uFormat ' + IntToStr(uFormat));
//       if LastWinControl<>nil then
//         AddLog('Detour_WinApi_DrawText', 'ClassName ' + LastWinControl.ClassName);
       RestoreColor:=True;
       SetTextColor(hDC, LFontColor);
    end;
   end;

  if not RestoreColor and (LFontColor<>OrgColor) and (OrgColor = GetSysColor(COLOR_WINDOWTEXT)) then
  begin
   if sCaller<>'' then
     sCaller := ProcByLevel(2);
    if SameText(sCaller, sCustomVirtualStringTreeSignature) then
    begin
//       AddLog('Detour_WinApi_DrawTextEx', sCaller);
//       AddLog('Detour_WinApi_DrawText', 'uFormat ' + IntToStr(uFormat));
//       if LastWinControl<>nil then
//         AddLog('Detour_WinApi_DrawText', 'ClassName ' + LastWinControl.ClassName);
       RestoreColor:=True;
       SetTextColor(hDC, LFontColor);
    end;
  end;
 end;

  //SetTextColor(hDC, ColorToRGB(clRed));
  Result:=Trampoline_DrawText(hDC, lpString, nCount, lpRect, uFormat);
  if RestoreColor then
    SetTextColor(hDC, OrgColor);
end;

//Hook for allow change font color in TVirtualTreeHintWindow.InternalPaint (font color in hint window) ,
//Note  : This is a temporal workaround.
function Detour_WinApi_DrawTextEx(DC: HDC; lpchText: LPCWSTR; cchText: Integer; var p4: TRect;  dwDTFormat: UINT; DTParams: PDrawTextParams): Integer; stdcall;
 //sVirtualTreeHintWindowInternalPaint = 'IDEVirtualTrees.TVirtualTreeHintWindow.InternalPaint';
{$IFDEF DELPHIXE6_UP}
const
 sDrawActiveTab                      =  'GDIPlus.GradientDrawer.TGradientTabDrawer.DrawActiveTab';
 sDrawInactiveTab                    =  'GDIPlus.GradientDrawer.TGradientTabDrawer.DrawInactiveTab';
{$ENDIF}
{$IFDEF DELPHIXE6_UP}
var
  sCaller : string;
  OrgColor : Cardinal;
  RestoreColor : Boolean;
{$ENDIF}
begin
{$IFDEF DELPHIXE6_UP}
 OrgColor:=0;
 RestoreColor:=False;
 if Assigned(TColorizerLocalSettings.Settings) and TColorizerLocalSettings.Settings.Enabled and Assigned(TColorizerLocalSettings.ColorMap) then
 begin
  OrgColor:= GetTextColor(DC);
   if (TColor(OrgColor) = clWhite) or (TColor(OrgColor) = clBlack)  then
   begin
     sCaller := ProcByLevel(4);
     if SameText(sCaller, sDrawActiveTab) then
     begin
       if not TColorizerLocalSettings.Settings.TabIDECustom then
        SetTextColor(DC, ColorToRGB(TColorizerLocalSettings.ColorMap.FontColor))
       else
        SetTextColor(DC, ColorToRGB(TryStrToColor(TColorizerLocalSettings.Settings.TabIDEActiveFontColor, TColorizerLocalSettings.ColorMap.FontColor)));
     end
     else
     if SameText(sCaller, sDrawInactiveTab) then
     begin
       if not TColorizerLocalSettings.Settings.TabIDECustom then
        SetTextColor(DC, ColorToRGB(TColorizerLocalSettings.ColorMap.FontColor))
       else
        SetTextColor(DC, ColorToRGB(TryStrToColor(TColorizerLocalSettings.Settings.TabIDEActiveFontColor, TColorizerLocalSettings.ColorMap.FontColor)));
     end;
   end;
 end;
{$ENDIF}
 //SetTextColor(DC, ColorToRGB(clRed));
 Result:=Trampoline_DrawTextEx(DC, lpchText, cchText, p4, dwDTFormat, DTParams);
{$IFDEF DELPHIXE6_UP}
 if RestoreColor then
   SetTextColor(DC, OrgColor);
{$ENDIF}
end;

//Hook for allow change font color in IDE Insight Window  and TPopupListBox (TInspListBox)
function Detour_WinApi_ExtTextOutW(DC: HDC; X, Y: Integer; Options: Longint; Rect: PRect; Str: LPCWSTR; Count: Longint; Dx: PInteger): BOOL; stdcall;
const
{$IFDEF DELPHIXE5_UP}
 sDrawTreeDrawNode  ='IDEInsight.TIDEInsightForm.DrawTreeDrawNode';
 sPaintItemNode     ='IDEInsight.TIDEInsightForm.PaintItemNode';
 sPaintCategoryNode ='IDEInsight.TIDEInsightForm.PaintCategoryNode';
{$ELSE}
 sDrawTreeDrawNode  ='PopupSrchFrm.TPopupSearchForm.DrawTreeDrawNode';
 sPaintItemNode     ='PopupSrchFrm.TPopupSearchForm.PaintItemNode';
 sPaintCategoryNode ='PopupSrchFrm.TPopupSearchForm.PaintCategoryNode';
{$ENDIF}
var
{$IFDEF DELPHIXE5_UP}
 sCaller  : string;
 sClassName : string;
{$ENDIF}
 LBgColor, OrgColor : Cardinal;
 RestoreColor{, RestoreBg} : Boolean;
begin
 OrgColor     :=0;
 //LBgColor     :=0;
 //RestoreBg    :=False;
 RestoreColor :=False;
 {$IFDEF DELPHIXE5_UP}
 if LastWinControl<>nil then
  try sClassName := LastWinControl.ClassName except sClassName:=''; end
 else
   sClassName:='';
 {$ENDIF}

 if Assigned(TColorizerLocalSettings.Settings) and TColorizerLocalSettings.Settings.Enabled then
 begin
  if DrawNamePair then {SameStr(sClassName, 'TPopupListBox')}
  begin
    //PropInsp.TPropertyInspector.InstanceListDrawItem
    //PropInsp.TPropertyInspector.DrawNamePair
    //sCaller:=ProcByLevel(3);
   // RestoreBg :=True;
    LBgColor:=GetBkColor(DC);
    if (TColor(LBgColor) = clWhite) or (TColor(LBgColor) = TColorizerLocalSettings.ColorMap.WindowColor) then
    begin
     if TColor(LBgColor) = clWhite then
      SetBkColor(DC, ColorToRGB(TColorizerLocalSettings.ColorMap.WindowColor));
     SetTextColor(DC, ColorToRGB(TColorizerLocalSettings.ColorMap.FontColor));
    end
    else
    begin
     SetBkColor(DC, ColorToRGB(TColorizerLocalSettings.ColorMap.SelectedColor));
     SetTextColor(DC, ColorToRGB(TColorizerLocalSettings.ColorMap.SelectedFontColor));
    end;
    DrawNamePair:=False;
  end
  else
  begin
  {$IFDEF DELPHIXE5_UP}
    if  SameStr(sClassName, 'TButtonedEdit') or SameStr(sClassName, 'TIDEInsightForm') or SameStr(sClassName, 'TVirtualDrawTree') then
//    {$ELSE}
//    if  SameStr(sClassName, 'TFilterEdit') or SameStr(sClassName, 'TIDEInsightForm') or SameStr(sClassName, 'TVirtualDrawTree') then
    begin
      OrgColor:=GetTextColor(DC);
       if  OrgColor=0  then
       begin
         sCaller:=ProcByLevel(3);
         if SameStr(sCaller, sDrawTreeDrawNode) or SameStr(sCaller, sPaintCategoryNode) or SameStr(sCaller, sPaintItemNode) then
         begin
           RestoreColor:=True;
           SetTextColor(DC, ColorToRGB(TColorizerLocalSettings.ColorMap.FontColor));
         end;
       end;
    end;
  {$ELSE}
    if DrawItemIDEInsight then
    begin
      if DrawItemIDEInsightSel then
        SetTextColor(DC, ColorToRGB(TColorizerLocalSettings.ColorMap.SelectedFontColor))
      else
        SetTextColor(DC, ColorToRGB(TColorizerLocalSettings.ColorMap.FontColor));

      DrawItemIDEInsight:=False;
      DrawItemIDEInsightSel:=False;
    end;
  {$ENDIF}
  end;
 end;

 Result:=Trampoline_ExtTextOutW(DC, x, y, Options, Rect, Str, Count, Dx);
 if RestoreColor then
   SetTextColor(DC, OrgColor);
// if RestoreBg then
//   SetBkColor(DC, LBgColor);
end;

//Hook to fix artifacts and undocumented painting methods ex: TClosableTabScroller background
function Detour_WinApi_GetSysColor(nIndex: Integer): DWORD; stdcall;
const
  SystemColor = $FF000000;
var
  sCaller : string;
begin
   if  Assigned(TColorizerLocalSettings.Settings) and (TColorizerLocalSettings.Settings.Enabled) and Assigned(TColorizerLocalSettings.ColorMap) then
   begin
     case nIndex of
       COLOR_INFOTEXT:
       begin
         if TColorizerLocalSettings.Settings.HookSystemColors and (TColor(SystemColor or Cardinal(nIndex))<>TColorizerLocalSettings.ColorMap.FontColor) then
          Exit(ColorToRGB(TColorizerLocalSettings.ColorMap.FontColor))
         else
          Exit(Trampoline_GetSysColor(nIndex));
       end;

       COLOR_INFOBK:
       begin
         if TColorizerLocalSettings.Settings.HookSystemColors and (TColor(SystemColor or  Cardinal(nIndex))<>TColorizerLocalSettings.ColorMap.WindowColor) then
          Exit(ColorToRGB(TColorizerLocalSettings.ColorMap.WindowColor))
         else
          Exit(Trampoline_GetSysColor(nIndex));
       end;

       COLOR_INACTIVECAPTION :
       begin
         if TColorizerLocalSettings.Settings.HookSystemColors and (TColor(SystemColor or  Cardinal(nIndex))<>TColorizerLocalSettings.ColorMap.DisabledColor) then
          Exit(ColorToRGB(TColorizerLocalSettings.ColorMap.DisabledColor))
         else
          Exit(Trampoline_GetSysColor(nIndex));
       end;

       COLOR_INACTIVECAPTIONTEXT :
       begin
         if TColorizerLocalSettings.Settings.HookSystemColors and (TColor(SystemColor or  Cardinal(nIndex))<>TColorizerLocalSettings.ColorMap.DisabledFontColor) then
          Exit(ColorToRGB(TColorizerLocalSettings.ColorMap.DisabledFontColor))
         else
          Exit(Trampoline_GetSysColor(nIndex));
       end;

       COLOR_HIGHLIGHT :
       begin
         if TColorizerLocalSettings.Settings.HookSystemColors and (TColor(SystemColor or  Cardinal(nIndex))<>TColorizerLocalSettings.ColorMap.SelectedColor) then
          Exit(ColorToRGB(TColorizerLocalSettings.ColorMap.SelectedColor))
         else
          Exit(Trampoline_GetSysColor(nIndex));
       end;

       COLOR_HIGHLIGHTTEXT:
       begin
         if TColorizerLocalSettings.Settings.HookSystemColors and (TColor(SystemColor or  Cardinal(nIndex))<>TColorizerLocalSettings.ColorMap.SelectedFontColor) then
          Exit(ColorToRGB(TColorizerLocalSettings.ColorMap.SelectedFontColor))
         else
          Exit(Trampoline_GetSysColor(nIndex));
       end;

       COLOR_BTNFACE :
       if (TColor(SystemColor or  Cardinal(nIndex))<>TColorizerLocalSettings.ColorMap.Color) then
       begin
         sCaller := ProcByLevel(2);
         if SameText(sCaller, '') then
           Exit(ColorToRGB(TColorizerLocalSettings.ColorMap.Color));
       end;

     end;
   end;

   Exit(Trampoline_GetSysColor(nIndex));
end;


procedure InstallHooksWinAPI();
var
  pOrgAddress : Pointer;
begin
 Trampoline_DrawText                       := InterceptCreate(@Windows.DrawTextW, @Detour_WinApi_DrawText);
 Trampoline_DrawTextEx                     := InterceptCreate(@Windows.DrawTextEx, @Detour_WinApi_DrawTextEx);
 Trampoline_ExtTextOutW                    := InterceptCreate(@Windows.ExtTextOutW, @Detour_WinApi_ExtTextOutW);

 pOrgAddress     := GetProcAddress(GetModuleHandle(user32), 'GetSysColor');
 if Assigned(pOrgAddress) then
   Trampoline_GetSysColor    :=  InterceptCreate(pOrgAddress, @Detour_WinApi_GetSysColor);

 pOrgAddress     := GetProcAddress(GetModuleHandle(user32), 'DrawFrameControl');
 if Assigned(pOrgAddress) then
   Trampoline_DrawFrameControl :=  InterceptCreate(pOrgAddress, @Detour_WinApi_DrawFrameControl);

 pOrgAddress     := GetProcAddress(GetModuleHandle(user32), 'DrawEdge');
 if Assigned(pOrgAddress) then
   Trampoline_DrawEdge :=  InterceptCreate(pOrgAddress, @Detour_WinApi_DrawEdge);
end;

procedure RemoveHooksWinAPI();
begin
  if Assigned(Trampoline_DrawText) then
    InterceptRemove(@Trampoline_DrawText);

  if Assigned(Trampoline_DrawTextEx) then
    InterceptRemove(@Trampoline_DrawTextEx);

  if Assigned(Trampoline_ExtTextOutW) then
    InterceptRemove(@Trampoline_ExtTextOutW);

  if Assigned(Trampoline_GetSysColor) then
    InterceptRemove(@Trampoline_GetSysColor);

  if Assigned(Trampoline_DrawFrameControl) then
    InterceptRemove(@Trampoline_DrawFrameControl);

  if Assigned(Trampoline_DrawEdge) then
    InterceptRemove(@Trampoline_DrawEdge);
end;

end.

