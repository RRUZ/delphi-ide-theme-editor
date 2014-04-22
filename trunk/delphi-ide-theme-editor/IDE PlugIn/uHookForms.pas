//**************************************************************************************************
//
// Unit uHookForms
// unit uHookForms  for the Delphi IDE Colorizer
//
// The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
// you may not use this file except in compliance with the License. You may obtain a copy of the
// License at http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
// ANY KIND, either express or implied. See the License for the specific language governing rights
// and limitations under the License.
//
// The Original Code is uHookForms.pas.
//
// The Initial Developer of the Original Code is Rodrigo Ruz V.
// Portions created by Rodrigo Ruz V. are Copyright (C) 2011-2014 Rodrigo Ruz V.
// All Rights Reserved.
//
//**************************************************************************************************
unit uHookForms;

interface

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
 ColorXPStyleActnCtrls,
 uColorizerUtils;

var
 hhk: HHOOK;

function CBT_FUNC(nCode: Integer; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
const
  ClassNameBufferSize = 1024;
var
 hTemp  : HWND;
 i      : Integer;
 RetVal : Integer;
 ClassNameBuffer: Array[0..ClassNameBufferSize-1] of Char;
begin
   if Assigned(uColorizerUtils.GlobalColorMap) and Assigned(HookedWindows) then
   case nCode of
     HCBT_ACTIVATE:
     begin
       hTemp := HWND(wParam);

       if (Screen<>nil) and (hTemp>0) then
       begin
          RetVal := GetClassName(wParam, ClassNameBuffer, SizeOf(ClassNameBuffer));
          if RetVal>0 then
          begin
            {.$WARN SYMBOL_PLATFORM OFF}
            //Win32Check(RetVal <> 0);
            {.$WARN SYMBOL_PLATFORM ON}
             Assert(RetVal < ClassNameBufferSize, 'Class name larger than fixed buffer size');
            //if HookedWindows.IndexOf(ClassNameBuffer)<>-1 then//(StrIComp(@ClassNameBuffer, 'TDefaultEnvironmentDialog') <> 0) then //StrLIComp(ClassNameBuffer, 'TDefaultEnvironmentDialog', ClassNameBufferSize) <>0 then
            for i := 0 to Screen.FormCount-1 do
             if Screen.Forms[i].Handle=hTemp then
               if (HookedWindows.IndexOf(ClassNameBuffer)<>-1) and not (csDesigning in Screen.Forms[i].ComponentState) then
               begin
                  //ShowMessage('Hooked');
                  uColorizerUtils.ProcessComponent(uColorizerUtils.GlobalColorMap, ColorXPStyle,Screen.Forms[i]);
                  //Screen.Forms[i].Color:=Main.AColorMap.Color;
                  Break;
               end
               {$IF CompilerVersion >= 23}
               else
               if (csDesigning in Screen.Forms[i].ComponentState) then
               begin
                 //ShowMessage('ApplyEmptyVCLStyleHook '+Screen.Forms[i].ClassName);
                 ApplyEmptyVCLStyleHook(Screen.Forms[i].ClassType);
               end;
              {$IFEND}
          end;
       end;
     end;
   end;
  Result := CallNextHookEx(hhk, nCode, wParam, lParam);
end;

Procedure InitHook();
begin
  hhk := SetWindowsHookEx(WH_CBT, @CBT_FUNC, hInstance, 0);
end;

Procedure KillHook();
begin
  if (hhk <> 0) then
    UnhookWindowsHookEx(hhk);
end;

initialization
  InitHook();

finalization
  KillHook();

end.
