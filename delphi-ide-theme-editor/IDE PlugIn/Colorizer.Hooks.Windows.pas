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
// Portions created by Rodrigo Ruz V. are Copyright (C) 2011-2015 Rodrigo Ruz V.
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
 Windows,
 Graphics,
 Controls,
 StdCtrls,
 //StrUtils,
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
 System.SyncObjs,
 System.Generics.Collections,
 Forms;

type
 TCustomListBoxClass     = class(TCustomListBox);
 TListStyleBrush = TObjectDictionary<Integer, HBRUSH>;

var
  Trampoline_DrawText                      : function (hDC: HDC; lpString: LPCWSTR; nCount: Integer;  var lpRect: TRect; uFormat: UINT): Integer; stdcall = nil;
  {$IFDEF DELPHIXE6_UP}
  Trampoline_DrawTextEx                    : function (DC: HDC; lpchText: LPCWSTR; cchText: Integer; var p4: TRect;  dwDTFormat: UINT; DTParams: PDrawTextParams): Integer; stdcall = nil;
  {$eNDIF}
  Trampoline_ExtTextOutW                   : function (DC: HDC; X, Y: Integer; Options: Longint; Rect: PRect; Str: LPCWSTR; Count: Longint; Dx: PInteger): BOOL; stdcall = nil;
  TrampolineGetSysColorBrush               : function(nIndex: Integer): HBRUSH; stdcall;
  Trampoline_GetSysColor                   : function (nIndex: Integer): DWORD; stdcall = nil;
  Trampoline_DrawFrameControl              : function (DC: HDC; Rect: PRect; uType, uState: UINT): BOOL; stdcall = nil;
  Trampoline_DrawEdge                      : function (hdc: HDC; var qrc: TRect; edge: UINT; grfFlags: UINT): BOOL; stdcall = nil;

var
  VCLStylesBrush: TObjectDictionary<string, TListStyleBrush>;
  VCLStylesLock: TCriticalSection = nil;

//Hook DrawEdge WinApi function
function Detour_WinApi_DrawEdge(hdc: HDC; var qrc: TRect; edge: UINT; grfFlags: UINT): BOOL; stdcall;
var
 LCanvas : TCanvas;
 OrgHWND : HWND;
 LWinControl : TWinControl;
 LParentForm : TCustomForm;
 SavedIndex  : Integer;
 WClassName  : string;
 Applyhook   : Boolean;
begin
   //DrawEdge(DC, R, EDGE_RAISED, BF_RECT or BF_MIDDLE or Flags);
  Applyhook:=True;
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
   begin
     //AddLog('CustomDrawEdge WinAPIClasses', WClassName);
     Exit(Trampoline_DrawEdge(hdc, qrc, edge, grfFlags));
   end;
  end;

  if (LWinControl<>nil) then
  begin
    //avoid paint the TCustomListBox when is ownerdraw
    if  (LWinControl is TCustomListBox) then
     Applyhook := TCustomListBoxClass(LWinControl).Style = lbStandard;
  end;

   if Applyhook then
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

//hook for unthemed TCheckbox , TRadioButton  ***no longer used ...
function Detour_WinApi_DrawFrameControl(DC: HDC; Rect: PRect; uType, uState: UINT): BOOL; stdcall;
var
 LCanvas : TCanvas;
 LBuffer : TBitmap;
 LRect : TRect;
 LSize   : TSize;
 OrgHWND : HWND;
 LWinControl : TWinControl;
 LParentForm : TCustomForm;
{$IFDEF DELPHIXE2_UP}
 LDetails: TThemedElementDetails;
 LStyleServices: TCustomStyleServices;
{$ENDIF}
begin
   //AddLog2(Format('Detour_WinApi_DrawFrameControl uType %d State %d', [uType, uState]));

   if( uType=DFC_SCROLL) and (Rect<>nil) and Assigned(TColorizerLocalSettings.Settings) and (TColorizerLocalSettings.Settings.Enabled) then
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


           if (DFCS_SCROLLUP and uState = DFCS_SCROLLUP) then
            LDetails := LStyleServices.GetElementDetails(tsArrowBtnUpNormal)
           else
           if (DFCS_SCROLLDOWN and uState = DFCS_SCROLLDOWN) then
            LDetails := LStyleServices.GetElementDetails(tsArrowBtnDownNormal)
            else
           if (DFCS_SCROLLLEFT and uState = DFCS_SCROLLLEFT) then
            LDetails := LStyleServices.GetElementDetails(tsArrowBtnLeftNormal)
           else
           if (DFCS_SCROLLRIGHT and uState = DFCS_SCROLLRIGHT) then
            LDetails := LStyleServices.GetElementDetails(tsArrowBtnRightNormal)
           else
           if (DFCS_SCROLLSIZEGRIP and uState = DFCS_SCROLLSIZEGRIP) then
            LDetails := LStyleServices.GetElementDetails(tsSizeBoxLeftAlign)
           else
           if (DFCS_SCROLLSIZEGRIPRIGHT and uState = DFCS_SCROLLSIZEGRIPRIGHT) then
            LDetails := LStyleServices.GetElementDetails(tsSizeBoxRightAlign)
           else
            Exit(Trampoline_DrawFrameControl(DC, Rect, uType, uState));

          LCanvas.Brush.Color := TColorizerLocalSettings.ColorMap.Color;
          LCanvas.FillRect(Rect^);
          LStyleServices.DrawElement(LCanvas.Handle, LDetails, Rect^);
         end
         else
         {$ENDIF}
         begin
           LSize.cx:= 13;
           LSize.cy:= 13;

           LBuffer:=TBitmap.Create;
           try
             LBuffer.SetSize(LSize.cx, LSize.cy);
             LRect := Types.Rect(0, 0, LSize.cx, LSize.cy);
             LBuffer.Canvas.Brush.Color:=TColorizerLocalSettings.ColorMap.WindowColor;
             LBuffer.Canvas.FillRect(LRect);
             LBuffer.Canvas.Pen.Color  :=TColorizerLocalSettings.ColorMap.FontColor;
             LBuffer.Canvas.Ellipse(0, 0, LSize.cx, LSize.cy);

             if (DFCS_CHECKED and uState = DFCS_CHECKED) then
             begin
               LBuffer.Canvas.Brush.Color:= LBuffer.Canvas.Pen.Color;
               LBuffer.Canvas.Ellipse(3, 3, 3 + LSize.cx div 2 ,3 + LSize.cy div 2);
             end;

             RectCenter(LRect, Rect^);
             BitBlt(dc, LRect.Left, LRect.Top, LSize.cx, LSize.cy, LBuffer.Canvas.Handle, 0, 0, SRCCOPY);
           finally
             LBuffer.Free;
           end;
         end;

        finally
          LCanvas.Handle:=0;
          LCanvas.Free;
        end;
        Exit(True);

   end
   else
   if( uType=DFC_BUTTON) and (Rect<>nil) and Assigned(TColorizerLocalSettings.Settings) and (TColorizerLocalSettings.Settings.Enabled) then
   begin
      //AddLog2(Format('DFC_BUTTON State %d', [uState]));

      if (DFCS_BUTTONRADIO and uState = DFCS_BUTTONRADIO) then
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
            LDetails := LStyleServices.GetElementDetails(tbRadioButtonCheckedNormal)
           else
            LDetails := LStyleServices.GetElementDetails(tbRadioButtonUncheckedNormal);

          LCanvas.Brush.Color := TColorizerLocalSettings.ColorMap.Color;
          LCanvas.FillRect(Rect^);
          LStyleServices.DrawElement(LCanvas.Handle, LDetails, Rect^);
         end
         else
         {$ENDIF}
         begin
           LSize.cx:= 13;
           LSize.cy:= 13;

           LBuffer:=TBitmap.Create;
           try
             LBuffer.SetSize(LSize.cx, LSize.cy);
             LRect := Types.Rect(0, 0, LSize.cx, LSize.cy);
             LBuffer.Canvas.Brush.Color:=TColorizerLocalSettings.ColorMap.WindowColor;
             LBuffer.Canvas.FillRect(LRect);
             LBuffer.Canvas.Pen.Color  :=TColorizerLocalSettings.ColorMap.FontColor;
             LBuffer.Canvas.Ellipse(0, 0, LSize.cx, LSize.cy);

             if (DFCS_CHECKED and uState = DFCS_CHECKED) then
             begin
               LBuffer.Canvas.Brush.Color:= LBuffer.Canvas.Pen.Color;
               LBuffer.Canvas.Ellipse(3, 3, 3 + LSize.cx div 2 ,3 + LSize.cy div 2);
             end;

             RectCenter(LRect, Rect^);
             BitBlt(dc, LRect.Left, LRect.Top, LSize.cx, LSize.cy, LBuffer.Canvas.Handle, 0, 0, SRCCOPY);
           finally
             LBuffer.Free;
           end;
         end;

        finally
          LCanvas.Handle:=0;
          LCanvas.Free;
        end;
        Exit(True);
      end
      else
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

//Hook for allow change font color in TProjectManagerForm.TCustomVirtualStringTree.PaintNormalText (font color in project manager window) ,
//because this component is not using the colors set via RTTI
function Detour_WinApi_DrawText(hDC: HDC; lpString: LPCWSTR; nCount: Integer;  var lpRect: TRect; uFormat: UINT): Integer; stdcall;
var
  OrgColor, LFontColor: Cardinal;
  RestoreColor : Boolean;
begin
 OrgColor:=0;
 RestoreColor:=False;

  if (uFormat=2084) and HookVTPaintNormalText then
  begin
   LFontColor :=ColorToRGB(TColorizerLocalSettings.ColorMap.FontColor);
   RestoreColor:=True;
   OrgColor:=SetTextColor(hDC, LFontColor);
  end;

  //SetTextColor(hDC, ColorToRGB(clRed));
  Result:=Trampoline_DrawText(hDC, lpString, nCount, lpRect, uFormat);
  if RestoreColor then
    SetTextColor(hDC, OrgColor);
end;


{$IFDEF DELPHIXE6_UP}
function Detour_WinApi_DrawTextEx(DC: HDC; lpchText: LPCWSTR; cchText: Integer; var p4: TRect;  dwDTFormat: UINT; DTParams: PDrawTextParams): Integer; stdcall;
begin
 if (dwDTFormat AND DT_CALCRECT = 0) and (HookDrawActiveTab or HookDrawInActiveTab) then
 begin
     if HookDrawActiveTab then
     begin
       if not TColorizerLocalSettings.Settings.TabIDECustom then
        SetTextColor(DC, ColorToRGB(TColorizerLocalSettings.ColorMap.FontColor))
       else
        SetTextColor(DC, ColorToRGB(TryStrToColor(TColorizerLocalSettings.Settings.TabIDEActiveFontColor, TColorizerLocalSettings.ColorMap.FontColor)));
     end
     else
     if HookDrawInActiveTab then
     begin
       if not TColorizerLocalSettings.Settings.TabIDECustom then
        SetTextColor(DC, ColorToRGB(TColorizerLocalSettings.ColorMap.FontColor))
       else
        SetTextColor(DC, ColorToRGB(TryStrToColor(TColorizerLocalSettings.Settings.TabIDEActiveFontColor, TColorizerLocalSettings.ColorMap.FontColor)));
     end;
 end;
 Result:=Trampoline_DrawTextEx(DC, lpchText, cchText, p4, dwDTFormat, DTParams);
end;
{$ENDIF}


//Hook for allow change font color in IDE Insight Window and TPopupListBox (TInspListBox)
function Detour_WinApi_ExtTextOutW(DC: HDC; X, Y: Integer; Options: Longint; Rect: PRect; Str: LPCWSTR; Count: Longint; Dx: PInteger): BOOL; stdcall;
var
 LBgColor, OrgColor : Cardinal;
 RestoreColor{, RestoreBg} : Boolean;
begin
 OrgColor     :=0;
 RestoreColor :=False;

 if Assigned(TColorizerLocalSettings.Settings) and TColorizerLocalSettings.Settings.Enabled then
 begin
  if DrawNamePair then
  begin
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
    if DrawItemPopupSearchForm then
    begin
      if DrawItemSelPopupSearchForm then
        SetTextColor(DC, ColorToRGB(TColorizerLocalSettings.ColorMap.SelectedFontColor))
      else
      begin
        if TColor(GetTextColor(DC))=$006D6D6D then
         SetTextColor(DC, ColorToRGB(TColorizerLocalSettings.ColorMap.DisabledFontColor))
        else
         SetTextColor(DC, ColorToRGB(TColorizerLocalSettings.ColorMap.FontColor));
      end;
    end
    else
    if DrawItemIDEInsight then
    begin
      if DrawItemSelIDEInsight then
        SetTextColor(DC, ColorToRGB(TColorizerLocalSettings.ColorMap.SelectedFontColor))
      else
      begin
        if TColor(GetTextColor(DC))=$006D6D6D then
         SetTextColor(DC, ColorToRGB(TColorizerLocalSettings.ColorMap.DisabledFontColor))
        else
         SetTextColor(DC, ColorToRGB(TColorizerLocalSettings.ColorMap.FontColor));
      end;
    end;
  {$ELSE}
    if DrawItemPopupSearchForm then
    begin
      if DrawItemSelPopupSearchForm then
        SetTextColor(DC, ColorToRGB(TColorizerLocalSettings.ColorMap.SelectedFontColor))
      else
      begin
        if TColor(GetTextColor(DC))=$006D6D6D then
         SetTextColor(DC, ColorToRGB(TColorizerLocalSettings.ColorMap.DisabledFontColor))
        else
         SetTextColor(DC, ColorToRGB(TColorizerLocalSettings.ColorMap.FontColor));
      end;
    end;
  {$ENDIF}
  end;
 end;

 Result:=Trampoline_ExtTextOutW(DC, x, y, Options, Rect, Str, Count, Dx);
 if RestoreColor then
   SetTextColor(DC, OrgColor);
end;

function InterceptGetSysColorBrush(nIndex: Integer): HBRUSH; stdcall;
var
  LCurrentStyleBrush: TListStyleBrush;
  LBrush: HBRUSH;
  LColor: TColor;
begin
  VCLStylesLock.Enter;
  try
    if not (Assigned(TColorizerLocalSettings.Settings) and (TColorizerLocalSettings.Settings.Enabled) and Assigned(TColorizerLocalSettings.ColorMap)) then
      Exit(TrampolineGetSysColorBrush(nIndex))
    else
    begin
      if VCLStylesBrush.ContainsKey(StyleServices.Name) then
        LCurrentStyleBrush := VCLStylesBrush.Items[StyleServices.Name]
      else
      begin
        VCLStylesBrush.Add(StyleServices.Name, TListStyleBrush.Create());
        LCurrentStyleBrush := VCLStylesBrush.Items[StyleServices.Name];
      end;
      if Assigned(LCurrentStyleBrush) then
      begin
        if LCurrentStyleBrush.ContainsKey(nIndex) then
          Exit(LCurrentStyleBrush[nIndex])
        else
        begin
          LColor := ColorizerStyleServices.GetSystemColor(TColor(nIndex or Integer($FF000000)));
          LBrush := CreateSolidBrush(LColor);
          LCurrentStyleBrush.Add(nIndex, LBrush);
          Exit(LBrush);
        end;
      end;
      Exit(TrampolineGetSysColorBrush(nIndex));
    end;
  finally
    VCLStylesLock.Leave;
  end;
end;

//Hook to fix artifacts and undocumented painting methods ex: TClosableTabScroller background
function Detour_WinApi_GetSysColor(nIndex: Integer): DWORD; stdcall;
const
  SystemColor = $FF000000;
//var
//  sCaller : string;
begin
   if  Assigned(TColorizerLocalSettings.Settings) and (TColorizerLocalSettings.Settings.Enabled) and Assigned(TColorizerLocalSettings.ColorMap) then
   begin

//
//    if nIndex = COLOR_HOTLIGHT then
//      Result := DWORD(ColorizerStyleServices.GetSystemColor(clHighlight))
//    else
//      Result := DWORD(ColorizerStyleServices.GetSystemColor(TColor(nIndex or Integer($FF000000))));
//
//
//   exit;

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

//       COLOR_BTNFACE :   //don't need this for now
//       if (TColor(SystemColor or Cardinal(nIndex))<>TColorizerLocalSettings.ColorMap.Color) then
//       begin
//         sCaller := ProcByLevel(2);
//         if SameText(sCaller, '') then
//           Exit(ColorToRGB(TColorizerLocalSettings.ColorMap.Color));
//       end;

     end;
   end;

   Exit(Trampoline_GetSysColor(nIndex));
end;


procedure InstallHooksWinAPI();
begin
 Trampoline_DrawText                       := InterceptCreate(@Windows.DrawTextW, @Detour_WinApi_DrawText);
{$IFDEF DELPHIXE6_UP}
 Trampoline_DrawTextEx                     := InterceptCreate(@Windows.DrawTextEx, @Detour_WinApi_DrawTextEx);
{$ENDIF}
 Trampoline_ExtTextOutW                    := InterceptCreate(@Windows.ExtTextOutW, @Detour_WinApi_ExtTextOutW);  //OK
 Trampoline_GetSysColor      :=  InterceptCreate(user32, 'GetSysColor', @Detour_WinApi_GetSysColor);
 //TrampolineGetSysColorBrush  := InterceptCreate(user32, 'GetSysColorBrush', @InterceptGetSysColorBrush);
  Trampoline_DrawEdge :=  InterceptCreate(user32, 'DrawEdge', @Detour_WinApi_DrawEdge);
 //Trampoline_DrawFrameControl :=  InterceptCreate(user32, 'DrawFrameControl', @Detour_WinApi_DrawFrameControl);
end;

procedure RemoveHooksWinAPI();
begin
  InterceptRemove(@Trampoline_DrawText);
{$IFDEF DELPHIXE6_UP}
  InterceptRemove(@Trampoline_DrawTextEx);
{$ENDIF}
  InterceptRemove(@Trampoline_ExtTextOutW);
  InterceptRemove(@Trampoline_GetSysColor);
  InterceptRemove(@TrampolineGetSysColorBrush);
  InterceptRemove(@Trampoline_DrawEdge);
  InterceptRemove(@Trampoline_DrawFrameControl);
end;


initialization
  VCLStylesLock  := TCriticalSection.Create;
  VCLStylesBrush := TObjectDictionary<string, TListStyleBrush>.Create([doOwnsValues]);
finalization
VCLStylesBrush.Free;
VCLStylesLock.Free;
VCLStylesLock := nil;


end.

