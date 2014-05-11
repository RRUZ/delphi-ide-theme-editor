//**************************************************************************************************
//
// Unit Colorizer.HookForms
// unit Colorizer.HookForms  for the Delphi IDE Colorizer
//
// The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
// you may not use this file except in compliance with the License. You may obtain a copy of the
// License at http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
// ANY KIND, either express or implied. See the License for the specific language governing rights
// and limitations under the License.
//
// The Original Code is Colorizer.HookForms.pas.
//
// The Initial Developer of the Original Code is Rodrigo Ruz V.
// Portions created by Rodrigo Ruz V. are Copyright (C) 2011-2014 Rodrigo Ruz V.
// All Rights Reserved.
//
//**************************************************************************************************
unit Colorizer.HookForms;

interface

  Procedure InstallFormsHook();
  Procedure RemoveFormsHook();

implementation

uses
 {$IF CompilerVersion >= 23}
 Vcl.Styles.Ext,
 {$IFEND}
 Classes,
 Forms,
 Windows,
 SysUtils,
 Dialogs,
 IOUtils,
 Controls,
 ColorXPStyleActnCtrls,
 Colorizer.Utils;

var
 hhk: HHOOK = 0;

function CBT_FUNC(nCode: Integer; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
const
  ClassNameBufferSize = 1024;
var
 LHWND  : HWND;
 i      : Integer;
 RetVal : Integer;
 ClassNameBuffer: Array[0..ClassNameBufferSize-1] of Char;
 LWinControl : TWinControl;
begin
   if Assigned(TColorizerLocalSettings.Settings) and TColorizerLocalSettings.Settings.Enabled and Assigned(TColorizerLocalSettings.ColorMap) and Assigned(TColorizerLocalSettings.HookedWindows) then
   case nCode of

     HCBT_SETFOCUS:
     begin
       LHWND := HWND(wParam);
       if (Screen<>nil) and (LHWND>0) then
       begin
          RetVal := GetClassName(wParam, ClassNameBuffer, SizeOf(ClassNameBuffer));
          if RetVal>0 then
          begin
            Assert(RetVal < ClassNameBufferSize, 'Class name larger than fixed buffer size');
            //TFile.AppendAllText('C:\Delphi\google-code\DITE\delphi-ide-theme-editor\IDE PlugIn\HCBT_SETFOCUS.txt', Format('%s %s',[ClassNameBuffer, SLineBreak]));
            if (TColorizerLocalSettings.HookedWindows.IndexOf(ClassNameBuffer)>=0) then
            begin
              LWinControl:=FindControl(LHWND);   //use FondControl because some formas are not registered in Screen.Forms
              if LWinControl<>nil then
                Colorizer.Utils.ProcessComponent(TColorizerLocalSettings.ColorMap, ColorXPStyle, LWinControl);
            end;
          end;
       end;
     end;

     HCBT_ACTIVATE:
     begin
       LHWND := HWND(wParam);

       if (Screen<>nil) and (LHWND>0) then
       begin
          RetVal := GetClassName(wParam, ClassNameBuffer, SizeOf(ClassNameBuffer));
          if RetVal>0 then
          begin
             Assert(RetVal < ClassNameBufferSize, 'Class name larger than fixed buffer size');
             //TFile.AppendAllText('C:\Delphi\google-code\DITE\delphi-ide-theme-editor\IDE PlugIn\HCBT_ACTIVATE.txt', Format('%s %s',[ClassNameBuffer, SLineBreak]));
            if (TColorizerLocalSettings.HookedWindows.IndexOf(ClassNameBuffer)>=0) then
            for i := 0 to Screen.FormCount-1 do
             if (Screen.Forms[i].Handle=LHWND) and not (csDesigning in Screen.Forms[i].ComponentState) then
               begin
                 Colorizer.Utils.ProcessComponent(TColorizerLocalSettings.ColorMap, ColorXPStyle, Screen.Forms[i]);
                 Break;
               end;
          end;
       end;
     end;
   end;
  Result := CallNextHookEx(hhk, nCode, wParam, lParam);
end;

Procedure InstallFormsHook();
begin
  if (hhk = 0) then
   hhk := SetWindowsHookEx(WH_CBT, @CBT_FUNC, hInstance, 0);
end;

Procedure RemoveFormsHook();
begin
  if (hhk <> 0) then
  begin
    UnhookWindowsHookEx(hhk);
    hhk:=0;
  end;
end;

end.
