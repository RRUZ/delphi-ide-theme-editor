//**************************************************************************************************
//
// Unit Vcl.Styles.Hooks
// unit for the VCL Styles Utils
// http://code.google.com/p/vcl-styles-utils/
//
// The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
// you may not use this file except in compliance with the License. You may obtain a copy of the
// License at http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
// ANY KIND, either express or implied. See the License for the specific language governing rights
// and limitations under the License.
//
// The Original Code is Vcl.Styles.Hooks.pas.
//
// The Initial Developer of the Original Code is Rodrigo Ruz V.
// Portions created by Rodrigo Ruz V. are Copyright (C) 2013-2014 Rodrigo Ruz V.
// All Rights Reserved.
//
//**************************************************************************************************
unit Vcl.Styles.Hooks;

interface

implementation

uses
  DDetours,
  System.SysUtils,
  System.Types,
  System.Classes,
  Winapi.UxTheme,
  WinApi.Windows,
  Vcl.Styles,
  Vcl.Graphics,
  Vcl.GraphUtil,
  Vcl.Themes;

var
  ThemeLibrary: THandle;
  TrampolineOpenThemeData         : function(hwnd: HWND; pszClassList: LPCWSTR): HTHEME; stdcall =  nil;
  TrampolineCloseThemeDaa         : function(hTheme: HTHEME): HRESULT; stdcall =  nil;
  TrampolineDrawThemeBackground   : function(hTheme: HTHEME; hdc: HDC; iPartId, iStateId: Integer;  const pRect: TRect; pClipRect: PRECT): HRESULT; stdcall =  nil;
  TrampolineDrawThemeBackgroundEx : function(hTheme: HTHEME; hdc: HDC; iPartId: Integer; iStateId: Integer; const pRect: TRect; pOptions: PDTBGOPTS): HResult; stdcall =  nil;
  TrampolineGetThemeColor         : function(hTheme: HTHEME; iPartId, iStateId, iPropId: Integer; var pColor: COLORREF): HRESULT; stdcall = nil;
  TrampolineGetSysColor           : function (nIndex: Integer): DWORD; stdcall =  nil;
  TrampolineGetThemeSysColor      :  function(hTheme: HTHEME; iColorId: Integer): COLORREF; stdcall =  nil;

  type
    THThemesClasses = class
    public
     class var Button   : TList;
     class var TreeView : TList;
    end;

function Detour_UxTheme_OpenThemeData(hwnd: HWND; pszClassList: LPCWSTR): HTHEME; stdcall;
begin
  Result:=TrampolineOpenThemeData(hwnd, pszClassList);
  if SameText(pszClassList, VSCLASS_BUTTON) then
  begin
    if THThemesClasses.Button.IndexOf(Pointer(Result))=-1 then
      THThemesClasses.Button.Add(Pointer(Result));
  end
  else
  if SameText(pszClassList, VSCLASS_TREEVIEW) then
  begin
    if THThemesClasses.TreeView.IndexOf(Pointer(Result))=-1 then
      THThemesClasses.TreeView.Add(Pointer(Result));
  end;

  //OutputDebugString(PChar('Detour_UxTheme_OpenThemeData '+pszClassList+' hTheme '+IntToStr(Result)));
end;


function Detour_UxTheme_DrawThemeBackgroundEx(hTheme: HTHEME; hdc: HDC; iPartId, iStateId: Integer;  const pRect: TRect; pOptions: PDTBGOPTS): HRESULT; stdcall;
var
  LBuffer   : TBitmap;
  SaveIndex : integer;
  LDetails  : TThemedElementDetails;
  LRect     : TRect;
  LSize     : TSize;
  LColor, LStartColor, LEndColor  : TColor;
  LCanvas   : TCanvas;
begin
  //OutputDebugString(PChar(Format('Detour_UxTheme_DrawThemeBackgroundEx hTheme %d iPartId %d iStateId %d', [hTheme, iPartId, iStateId])));

  if StyleServices.IsSystemStyle then
    Exit(TrampolineDrawThemeBackgroundEx(hTheme, hdc, iPartId, iStateId, pRect, pOptions));

   if THThemesClasses.Button.IndexOf(Pointer(hTheme))>=0 then
   begin
        case iPartId of
            BP_RADIOBUTTON  :
            begin
              case iStateId of
                  RBS_UNCHECKEDNORMAL   : LDetails:=StyleServices.GetElementDetails(tbRadioButtonUncheckedNormal);
                  RBS_UNCHECKEDHOT      : LDetails:=StyleServices.GetElementDetails(tbRadioButtonUncheckedHot);
                  RBS_UNCHECKEDPRESSED  : LDetails:=StyleServices.GetElementDetails(tbRadioButtonUncheckedPressed);
                  RBS_UNCHECKEDDISABLED : LDetails:=StyleServices.GetElementDetails(tbRadioButtonUncheckedDisabled);
                  RBS_CHECKEDNORMAL     : LDetails:=StyleServices.GetElementDetails(tbRadioButtonCheckedNormal);
                  RBS_CHECKEDHOT        : LDetails:=StyleServices.GetElementDetails(tbRadioButtonCheckedHot);
                  RBS_CHECKEDPRESSED    : LDetails:=StyleServices.GetElementDetails(tbRadioButtonCheckedPressed);
                  RBS_CHECKEDDISABLED   : LDetails:=StyleServices.GetElementDetails(tbRadioButtonCheckedDisabled);
              end;

              SaveIndex := SaveDC(hdc); //avoid canvas issue caused by the StyleServices.DrawElement method
              try
                 StyleServices.DrawElement(hdc, LDetails, pRect, nil);
              finally
                RestoreDC(hdc, SaveIndex);
              end;

              Result:=S_OK;
            end;

            BP_CHECKBOX  :
            begin
              case iStateId of
                  CBS_UNCHECKEDNORMAL     : LDetails:=StyleServices.GetElementDetails(tbCheckBoxUncheckedNormal);
                  CBS_UNCHECKEDHOT        : LDetails:=StyleServices.GetElementDetails(tbCheckBoxUncheckedHot);
                  CBS_UNCHECKEDPRESSED    : LDetails:=StyleServices.GetElementDetails(tbCheckBoxUncheckedPressed);
                  CBS_UNCHECKEDDISABLED   : LDetails:=StyleServices.GetElementDetails(tbCheckBoxUncheckedDisabled);
                  CBS_CHECKEDNORMAL       : LDetails:=StyleServices.GetElementDetails(tbCheckBoxCheckedNormal);
                  CBS_CHECKEDHOT          : LDetails:=StyleServices.GetElementDetails(tbCheckBoxCheckedHot);
                  CBS_CHECKEDPRESSED      : LDetails:=StyleServices.GetElementDetails(tbCheckBoxCheckedPressed);
                  CBS_CHECKEDDISABLED     : LDetails:=StyleServices.GetElementDetails(tbCheckBoxCheckedDisabled);
                  CBS_MIXEDNORMAL         : LDetails:=StyleServices.GetElementDetails(tbCheckBoxMixedNormal);
                  CBS_MIXEDHOT            : LDetails:=StyleServices.GetElementDetails(tbCheckBoxMixedHot);
                  CBS_MIXEDPRESSED        : LDetails:=StyleServices.GetElementDetails(tbCheckBoxMixedPressed);
                  CBS_MIXEDDISABLED       : LDetails:=StyleServices.GetElementDetails(tbCheckBoxMixedDisabled);
                  { For Windows >= Vista }
                  CBS_IMPLICITNORMAL      : LDetails:=StyleServices.GetElementDetails(tbCheckBoxImplicitNormal);
                  CBS_IMPLICITHOT         : LDetails:=StyleServices.GetElementDetails(tbCheckBoxImplicitHot);
                  CBS_IMPLICITPRESSED     : LDetails:=StyleServices.GetElementDetails(tbCheckBoxImplicitPressed);
                  CBS_IMPLICITDISABLED    : LDetails:=StyleServices.GetElementDetails(tbCheckBoxImplicitDisabled);
                  CBS_EXCLUDEDNORMAL      : LDetails:=StyleServices.GetElementDetails(tbCheckBoxExcludedNormal);
                  CBS_EXCLUDEDHOT         : LDetails:=StyleServices.GetElementDetails(tbCheckBoxExcludedHot);
                  CBS_EXCLUDEDPRESSED     : LDetails:=StyleServices.GetElementDetails(tbCheckBoxExcludedPressed);
                  CBS_EXCLUDEDDISABLED    : LDetails:=StyleServices.GetElementDetails(tbCheckBoxExcludedDisabled);
              end;

              SaveIndex := SaveDC(hdc); //avoid canvas issue caused by the StyleServices.DrawElement method
              try
                StyleServices.DrawElement(hdc, LDetails, pRect, nil);
              finally
                RestoreDC(hdc, SaveIndex);
              end;
              Result:=S_OK;
            end
        else
          Result:= TrampolineDrawThemeBackgroundEx(hTheme, hdc, iPartId, iStateId, pRect, pOptions);
        end;
   end
  else
  if THThemesClasses.TreeView.IndexOf(Pointer(hTheme))>=0 then
  begin
    if ((iPartId = TVP_GLYPH) and ((iStateId=GLPS_OPENED) or (iStateId=GLPS_CLOSED))) or
       ((iPartId = TVP_HOTGLYPH) and ((iStateId=HGLPS_OPENED) or (iStateId=HGLPS_CLOSED))) then
    begin
      if (iStateId=GLPS_OPENED) or (iStateId=HGLPS_OPENED) then
       LDetails := StyleServices.GetElementDetails(tcbCategoryGlyphOpened)
      else
       LDetails := StyleServices.GetElementDetails(tcbCategoryGlyphClosed);

       LBuffer:=TBitmap.Create;
       try
         LSize.cx:=10;
         LSize.cy:=10;
         LRect := Rect(0, 0, LSize.Width, LSize.Height);
         LBuffer.SetSize(LRect.Width, LRect.Height);
         StyleServices.DrawElement(LBuffer.Canvas.Handle, LDetails, LRect);
         BitBlt(hdc, pRect.Left, pRect.Top, LRect.Width, LRect.Height, LBuffer.Canvas.Handle, 0, 0, SRCCOPY);
       finally
         LBuffer.Free;
       end;
       Exit(S_OK);
    end
    else
    if (iPartId = TVP_TREEITEM) and (iStateId=TREIS_SELECTED) then
    begin
       LDetails := StyleServices.GetElementDetails(tgGradientCellSelected);
       LStartColor:=GetSysColor(COLOR_HIGHLIGHT);
       LEndColor  :=GetSysColor(COLOR_HIGHLIGHT);

       if StyleServices.GetElementColor(LDetails, ecGradientColor1, LColor) and (LColor <> clNone) then
         LStartColor := LColor;
       if StyleServices.GetElementColor(LDetails, ecGradientColor2, LColor) and (LColor <> clNone) then
         LEndColor := LColor;
//        if StyleServices.GetElementColor(LDetails, ecTextColor, LColor) and (LColor <> clNone) then
//          Canvas.Font.Color := LColor;
      LCanvas:=TCanvas.Create;
      SaveIndex := SaveDC(hdc);
      try
        LCanvas.Handle:=hdc;
        GradientFillCanvas(LCanvas, LStartColor, LEndColor, pRect, gdVertical);
      finally
        LCanvas.Handle:=0;
        LCanvas.Free;
        RestoreDC(hdc, SaveIndex);
      end;

       Exit(S_OK);
    end
    else
    Result:= TrampolineDrawThemeBackgroundEx(hTheme, hdc, iPartId, iStateId, pRect, pOptions);
  end
  else
    Result:= TrampolineDrawThemeBackgroundEx(hTheme, hdc, iPartId, iStateId, pRect, pOptions);
end;


function Detour_UxTheme_DrawThemeBackground(hTheme: HTHEME; hdc: HDC; iPartId, iStateId: Integer;  const pRect: TRect; pClipRect: PRECT): HRESULT; stdcall;
var
  LBuffer   : TBitmap;
  SaveIndex : integer;
  LDetails  : TThemedElementDetails;
  LRect     : TRect;
  LSize     : TSize;
  LColor, LStartColor, LEndColor  : TColor;
  LCanvas   : TCanvas;
begin
  //OutputDebugString(PChar(Format('Detour_UxTheme_DrawThemeBackground hTheme %d iPartId %d iStateId %d', [hTheme, iPartId, iStateId])));

  if StyleServices.IsSystemStyle then
    Exit(TrampolineDrawThemeBackground(hTheme, hdc, iPartId, iStateId, pRect, pClipRect));

   if THThemesClasses.Button.IndexOf(Pointer(hTheme))>=0 then
   begin
        case iPartId of
            BP_RADIOBUTTON  :
            begin
              case iStateId of
                  RBS_UNCHECKEDNORMAL   : LDetails:=StyleServices.GetElementDetails(tbRadioButtonUncheckedNormal);
                  RBS_UNCHECKEDHOT      : LDetails:=StyleServices.GetElementDetails(tbRadioButtonUncheckedHot);
                  RBS_UNCHECKEDPRESSED  : LDetails:=StyleServices.GetElementDetails(tbRadioButtonUncheckedPressed);
                  RBS_UNCHECKEDDISABLED : LDetails:=StyleServices.GetElementDetails(tbRadioButtonUncheckedDisabled);
                  RBS_CHECKEDNORMAL     : LDetails:=StyleServices.GetElementDetails(tbRadioButtonCheckedNormal);
                  RBS_CHECKEDHOT        : LDetails:=StyleServices.GetElementDetails(tbRadioButtonCheckedHot);
                  RBS_CHECKEDPRESSED    : LDetails:=StyleServices.GetElementDetails(tbRadioButtonCheckedPressed);
                  RBS_CHECKEDDISABLED   : LDetails:=StyleServices.GetElementDetails(tbRadioButtonCheckedDisabled);
              end;

              SaveIndex := SaveDC(hdc); //avoid canvas issue caused by the StyleServices.DrawElement method
              try
                 StyleServices.DrawElement(hdc, LDetails, pRect, pClipRect^);
              finally
                RestoreDC(hdc, SaveIndex);
              end;

              Result:=S_OK;
            end;

            BP_CHECKBOX  :
            begin
              case iStateId of
                  CBS_UNCHECKEDNORMAL     : LDetails:=StyleServices.GetElementDetails(tbCheckBoxUncheckedNormal);
                  CBS_UNCHECKEDHOT        : LDetails:=StyleServices.GetElementDetails(tbCheckBoxUncheckedHot);
                  CBS_UNCHECKEDPRESSED    : LDetails:=StyleServices.GetElementDetails(tbCheckBoxUncheckedPressed);
                  CBS_UNCHECKEDDISABLED   : LDetails:=StyleServices.GetElementDetails(tbCheckBoxUncheckedDisabled);
                  CBS_CHECKEDNORMAL       : LDetails:=StyleServices.GetElementDetails(tbCheckBoxCheckedNormal);
                  CBS_CHECKEDHOT          : LDetails:=StyleServices.GetElementDetails(tbCheckBoxCheckedHot);
                  CBS_CHECKEDPRESSED      : LDetails:=StyleServices.GetElementDetails(tbCheckBoxCheckedPressed);
                  CBS_CHECKEDDISABLED     : LDetails:=StyleServices.GetElementDetails(tbCheckBoxCheckedDisabled);
                  CBS_MIXEDNORMAL         : LDetails:=StyleServices.GetElementDetails(tbCheckBoxMixedNormal);
                  CBS_MIXEDHOT            : LDetails:=StyleServices.GetElementDetails(tbCheckBoxMixedHot);
                  CBS_MIXEDPRESSED        : LDetails:=StyleServices.GetElementDetails(tbCheckBoxMixedPressed);
                  CBS_MIXEDDISABLED       : LDetails:=StyleServices.GetElementDetails(tbCheckBoxMixedDisabled);
                  { For Windows >= Vista }
                  CBS_IMPLICITNORMAL      : LDetails:=StyleServices.GetElementDetails(tbCheckBoxImplicitNormal);
                  CBS_IMPLICITHOT         : LDetails:=StyleServices.GetElementDetails(tbCheckBoxImplicitHot);
                  CBS_IMPLICITPRESSED     : LDetails:=StyleServices.GetElementDetails(tbCheckBoxImplicitPressed);
                  CBS_IMPLICITDISABLED    : LDetails:=StyleServices.GetElementDetails(tbCheckBoxImplicitDisabled);
                  CBS_EXCLUDEDNORMAL      : LDetails:=StyleServices.GetElementDetails(tbCheckBoxExcludedNormal);
                  CBS_EXCLUDEDHOT         : LDetails:=StyleServices.GetElementDetails(tbCheckBoxExcludedHot);
                  CBS_EXCLUDEDPRESSED     : LDetails:=StyleServices.GetElementDetails(tbCheckBoxExcludedPressed);
                  CBS_EXCLUDEDDISABLED    : LDetails:=StyleServices.GetElementDetails(tbCheckBoxExcludedDisabled);
              end;

              SaveIndex := SaveDC(hdc); //avoid canvas issue caused by the StyleServices.DrawElement method
              try
                StyleServices.DrawElement(hdc, LDetails, pRect, pClipRect^);
              finally
                RestoreDC(hdc, SaveIndex);
              end;
              Result:=S_OK;
            end
        else
          Result:= TrampolineDrawThemeBackground(hTheme, hdc, iPartId, iStateId, pRect, pClipRect);
        end;
   end
  else
  if THThemesClasses.TreeView.IndexOf(Pointer(hTheme))>=0 then
  begin
    if ((iPartId = TVP_GLYPH) and ((iStateId=GLPS_OPENED) or (iStateId=GLPS_CLOSED))) or
       ((iPartId = TVP_HOTGLYPH) and ((iStateId=HGLPS_OPENED) or (iStateId=HGLPS_CLOSED))) then
    begin
      if (iStateId=GLPS_OPENED) or (iStateId=HGLPS_OPENED) then
       LDetails := StyleServices.GetElementDetails(tcbCategoryGlyphOpened)
      else
       LDetails := StyleServices.GetElementDetails(tcbCategoryGlyphClosed);

       LBuffer:=TBitmap.Create;
       try
         LSize.cx:=10;
         LSize.cy:=10;
         LRect := Rect(0, 0, LSize.Width, LSize.Height);
         LBuffer.SetSize(LRect.Width, LRect.Height);
         StyleServices.DrawElement(LBuffer.Canvas.Handle, LDetails, LRect);
         BitBlt(hdc, pRect.Left, pRect.Top, LRect.Width, LRect.Height, LBuffer.Canvas.Handle, 0, 0, SRCCOPY);
       finally
         LBuffer.Free;
       end;
       Exit(S_OK);
    end
    else
    if (iPartId = TVP_TREEITEM) and (iStateId=TREIS_SELECTED) then
    begin
       LDetails := StyleServices.GetElementDetails(tgGradientCellSelected);
       LStartColor:=GetSysColor(COLOR_HIGHLIGHT);
       LEndColor  :=GetSysColor(COLOR_HIGHLIGHT);

       if StyleServices.GetElementColor(LDetails, ecGradientColor1, LColor) and (LColor <> clNone) then
         LStartColor := LColor;
       if StyleServices.GetElementColor(LDetails, ecGradientColor2, LColor) and (LColor <> clNone) then
         LEndColor := LColor;
//        if StyleServices.GetElementColor(LDetails, ecTextColor, LColor) and (LColor <> clNone) then
//          Canvas.Font.Color := LColor;
      LCanvas:=TCanvas.Create;
      SaveIndex := SaveDC(hdc);
      try
        LCanvas.Handle:=hdc;
        GradientFillCanvas(LCanvas, LStartColor, LEndColor, pRect, gdVertical);
      finally
        LCanvas.Handle:=0;
        LCanvas.Free;
        RestoreDC(hdc, SaveIndex);
      end;

       Exit(S_OK);
    end
    else
    begin
      //OutputDebugString(PChar(Format('TreeView iPartId %d iStateId %d', [iPartId, iStateId])));
      Result:= TrampolineDrawThemeBackground(hTheme, hdc, iPartId, iStateId, pRect, pClipRect);
    end;
  end
  else
    Result:= TrampolineDrawThemeBackground(hTheme, hdc, iPartId, iStateId, pRect, pClipRect);
end;


function Detour_GetSysColor(nIndex: Integer): DWORD; stdcall;
begin
  if StyleServices.IsSystemStyle  then
   Result:= TrampolineGetSysColor(nIndex)
  else
   Result:= DWORD(StyleServices.GetSystemColor(TColor(nIndex or Integer($FF000000))));
end;

function Detour_GetThemeSysColor(hTheme: HTHEME; iColorId: Integer): COLORREF; stdcall;
begin
  if StyleServices.IsSystemStyle then
   Result:= TrampolineGetThemeSysColor(hTheme, iColorId)
  else
   Result:= StyleServices.GetSystemColor(iColorId or Integer($FF000000));
end;

function Detour_GetThemeColor(hTheme: HTHEME; iPartId, iStateId, iPropId: Integer; var pColor: COLORREF): HRESULT;
begin
  //OutputDebugString(PChar(Format('Detour_GetThemeColor hTheme %d iPartId %d iStateId %d', [hTheme, iPartId, iStateId])));
  Exit(TrampolineGetThemeColor(hTheme, iPartId, iStateId, iPropId, pColor));
end;

var
 pOrgPointer : Pointer;

initialization
 THThemesClasses.Button  :=TList.Create;
 THThemesClasses.TreeView:=TList.Create;

 if StyleServices.Available then
 begin
   ThemeLibrary := GetModuleHandle('uxtheme.dll');

   pOrgPointer     := GetProcAddress(GetModuleHandle('user32.dll'), 'GetSysColor');
   if Assigned(pOrgPointer) then
   @TrampolineGetSysColor    :=  InterceptCreate(pOrgPointer, @Detour_GetSysColor);

   pOrgPointer   := GetProcAddress(ThemeLibrary, 'OpenThemeData');
   if Assigned(pOrgPointer) then
   @TrampolineOpenThemeData  := InterceptCreate(pOrgPointer, @Detour_UxTheme_OpenThemeData);

//   CloseThemeDataOrgPointer  := GetProcAddress(ThemeLibrary, 'CloseThemeData');
//   @TrampolineCloseThemeData := InterceptCreate(CloseThemeDataOrgPointer, @InterceptCloseThemeData);

   pOrgPointer := GetProcAddress(ThemeLibrary, 'DrawThemeBackground');
   if Assigned(pOrgPointer) then
   @TrampolineDrawThemeBackground := InterceptCreate(pOrgPointer, @Detour_UxTheme_DrawThemeBackground);

   pOrgPointer := GetProcAddress(ThemeLibrary, 'DrawThemeBackgroundEx');
   if Assigned(pOrgPointer) then
   @TrampolineDrawThemeBackgroundEx := InterceptCreate(pOrgPointer, @Detour_UxTheme_DrawThemeBackgroundEx);

   pOrgPointer  := GetProcAddress(ThemeLibrary, 'GetThemeSysColor');
   if Assigned(pOrgPointer) then
   @TrampolineGetThemeSysColor := InterceptCreate(pOrgPointer, @Detour_GetThemeSysColor);

//   pOrgPointer     := GetProcAddress(ThemeLibrary, 'GetThemeColor');
//   @TrampolineGetThemeColor    := InterceptCreate(GetThemeColorOrgPointer, @Detour_GetThemeColor);
 end;

finalization
 if Assigned(TrampolineGetSysColor) then
  InterceptRemove(@TrampolineGetSysColor);

 if Assigned(TrampolineGetThemeSysColor) then
  InterceptRemove(@TrampolineGetThemeSysColor);

 if Assigned(TrampolineOpenThemeData) then
  InterceptRemove(@TrampolineOpenThemeData);

 if Assigned(TrampolineGetThemeColor) then
  InterceptRemove(@TrampolineGetThemeColor);

 if Assigned(TrampolineDrawThemeBackground) then
  InterceptRemove(@TrampolineDrawThemeBackground);

 if Assigned(TrampolineDrawThemeBackgroundEx) then
  InterceptRemove(@TrampolineDrawThemeBackgroundEx);

  THThemesClasses.Button.Free;
  THThemesClasses.TreeView.Free;
end.
