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

{$IFDEF CPUX64}
  Sorry, this unit only can be used  in 32 bits mode
{$ENDIF}

implementation

uses
  {.$IFDEF DEBUG}
  //System.IOUtils,
  {.$ENDIF}
  System.SysUtils,
  System.Types,
  System.Classes,
  KOLDetours,
  Winapi.UxTheme,
  WinApi.Windows,
  Vcl.Styles,
  Vcl.Graphics,
  Vcl.Themes;

var
  ThemeLibrary: THandle;
  TrampolineOpenThemeData: function(hwnd: HWND; pszClassList: LPCWSTR): HTHEME; stdcall;
  TrampolineCloseThemeData: function(hTheme: HTHEME): HRESULT; stdcall;
  TrampolineDrawThemeBackground: function(hTheme: HTHEME; hdc: HDC; iPartId, iStateId: Integer;  const pRect: TRect; pClipRect: PRECT): HRESULT; stdcall;


  TrampolineGetSysColor  : function (nIndex: Integer): DWORD; stdcall;
  //TrampolineGetThemeSysColor :  function(hTheme: HTHEME; iColorId: Integer): COLORREF; stdcall;

  GetSysColorOrgPointer  : Pointer = nil;
  OpenThemeDataOrgPointer: Pointer = nil;
  CloseThemeDataOrgPointer: Pointer = nil;
  DrawThemeBackgroundOrgPointer: Pointer = nil;

  //GetThemeSysColorOrgPointer : Pointer = nil;
  HThemeClassesList : TStrings = nil; //use a  TStrings to avoid the use of generics

function InterceptOpenThemeData(hwnd: HWND; pszClassList: LPCWSTR): HTHEME; stdcall;
var
  i : integer;
begin
   Result:=TrampolineOpenThemeData(hwnd, pszClassList);
   i:= HThemeClassesList.IndexOfName(IntToStr(Result));
   if i=-1 then
    HThemeClassesList.Add(Format('%d=%s',[Integer(Result), pszClassList]));
end;

function InterceptCloseThemeData(hTheme: HTHEME): HRESULT; stdcall;
var
  i : integer;
begin
   i:= HThemeClassesList.IndexOfName(IntToStr(hTheme));
   if i>=0 then
     HThemeClassesList.Delete(i);
   Result:= TrampolineCloseThemeData(hTheme);
end;


function InterceptDrawThemeBackground(hTheme: HTHEME; hdc: HDC; iPartId, iStateId: Integer;  const pRect: TRect; pClipRect: PRECT): HRESULT; stdcall;
var
  SaveIndex, i : integer;
  pszClassList : string;
  LDetails: TThemedElementDetails;
begin
  if StyleServices.IsSystemStyle then
    Exit(TrampolineDrawThemeBackground(hTheme, hdc, iPartId, iStateId, pRect, pClipRect));

   i:= HThemeClassesList.IndexOfName(IntToStr(hTheme));
   if i>=0 then
   begin
      pszClassList:=HThemeClassesList.Values[IntToStr(hTheme)];
      if SameText(pszClassList,'Button') then
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
//      else
//      if SameText(pszClassList,'TreeView') then
//      begin
//        case iPartId of
//            TVP_TREEITEM  :
//            begin
//              case iStateId of
//                  TREIS_NORMAL            : LDetails:=StyleServices.GetElementDetails(ttItemNormal);
//                  TREIS_HOT               : LDetails:=StyleServices.GetElementDetails(ttItemHot);
//                  TREIS_SELECTED          : LDetails:=StyleServices.GetElementDetails(ttItemSelected);
//                  TREIS_DISABLED          : LDetails:=StyleServices.GetElementDetails(ttItemDisabled);
//                  TREIS_SELECTEDNOTFOCUS  : LDetails:=StyleServices.GetElementDetails(ttItemSelectedNotFocus);
//                  { For Windows >= Vista }
//                  TREIS_HOTSELECTED       : LDetails:=StyleServices.GetElementDetails(ttItemHotSelected);
//              end;
//
//              SaveIndex := SaveDC(hdc); //avoid canvas issue caused by the StyleServices.DrawElement method
//              try
//                StyleServices.DrawElement(hdc, LDetails, pRect, pClipRect^);
//              finally
//                RestoreDC(hdc, SaveIndex);
//              end;
//              Result:=S_OK;
//            end;
//        else
//          Result:= TrampolineDrawThemeBackground(hTheme, hdc, iPartId, iStateId, pRect, pClipRect);
//        end;
//      end
     else
       Result:= TrampolineDrawThemeBackground(hTheme, hdc, iPartId, iStateId, pRect, pClipRect);
   end
   else
   Result:= TrampolineDrawThemeBackground(hTheme, hdc, iPartId, iStateId, pRect, pClipRect);
end;


function InterceptGetSysColor(nIndex: Integer): DWORD; stdcall;
begin
  if StyleServices.IsSystemStyle  then
   Result:= TrampolineGetSysColor(nIndex)
  else
   Result:= DWORD(StyleServices.GetSystemColor(TColor(nIndex or Integer($FF000000))));
end;



//function InterceptGetThemeSysColor(hTheme: HTHEME; iColorId: Integer): COLORREF; stdcall;
//begin
//  if StyleServices.IsSystemStyle then
//   Result:= TrampolineGetThemeSysColor(hTheme, iColorId)
//  else
//   Result:= StyleServices.GetSystemColor(iColorId or Integer($FF000000));
//end;

initialization

 if StyleServices.Available then
 begin
   HThemeClassesList:=TStringList.Create;
   ThemeLibrary := GetModuleHandle('uxtheme.dll');

   GetSysColorOrgPointer     := GetProcAddress(GetModuleHandle('user32.dll'), 'GetSysColor');
   @TrampolineGetSysColor  :=  InterceptCreate(GetSysColorOrgPointer, @InterceptGetSysColor);

   OpenThemeDataOrgPointer   := GetProcAddress(ThemeLibrary, 'OpenThemeData');
   @TrampolineOpenThemeData  := InterceptCreate(OpenThemeDataOrgPointer, @InterceptOpenThemeData);

   CloseThemeDataOrgPointer  := GetProcAddress(ThemeLibrary, 'CloseThemeData');
   @TrampolineCloseThemeData := InterceptCreate(CloseThemeDataOrgPointer, @InterceptCloseThemeData);

   DrawThemeBackgroundOrgPointer := GetProcAddress(ThemeLibrary, 'DrawThemeBackground');
   @TrampolineDrawThemeBackground := InterceptCreate(DrawThemeBackgroundOrgPointer, @InterceptDrawThemeBackground);

//   GetThemeSysColorOrgPointer  := GetProcAddress(ThemeLibrary, 'GetThemeSysColor');
//   @TrampolineGetThemeSysColor := InterceptCreate(GetThemeSysColorOrgPointer, @InterceptGetThemeSysColor);
 end;

finalization

 if GetSysColorOrgPointer<>nil then
  InterceptRemove(@TrampolineGetSysColor, @InterceptGetSysColor);

// if GetThemeSysColorOrgPointer<>nil then
//  InterceptRemove(@TrampolineGetThemeSysColor, @InterceptGetThemeSysColor);

 if OpenThemeDataOrgPointer<>nil then
  InterceptRemove(@TrampolineOpenThemeData, @InterceptOpenThemeData);

 if CloseThemeDataOrgPointer<>nil then
  InterceptRemove(@TrampolineCloseThemeData, @InterceptCloseThemeData);

 if DrawThemeBackgroundOrgPointer<>nil then
  InterceptRemove(@TrampolineDrawThemeBackground, @InterceptDrawThemeBackground);

 if HThemeClassesList<>nil then
   HThemeClassesList.Free;
end.
