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
{$I ..\Common\Jedi.inc}

  Procedure InstallFormsHook();
  Procedure RemoveFormsHook();

implementation

uses
 Classes,
 Forms,
 Windows,
 SysUtils,
 Controls,
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
 LParentForm : TCustomForm;
begin
   if Assigned(TColorizerLocalSettings.Settings) and TColorizerLocalSettings.Settings.Enabled and Assigned(TColorizerLocalSettings.ColorMap)
   and Assigned(TColorizerLocalSettings.HookedWindows) and Assigned(TColorizerLocalSettings.HookedScrollBars) then
   case nCode of

//     HCBT_SYSCOMMAND :
//     begin
//          RetVal := GetClassName(wParam, ClassNameBuffer, SizeOf(ClassNameBuffer));
//          if RetVal>0 then
//           AddLog('HCBT_SYSCOMMAND', ClassNameBuffer+' SC_COMMAND '+IntToHex(wParam, 8));
//     end;

     HCBT_SETFOCUS:
     begin
       LHWND := HWND(wParam);
       if (Screen<>nil) and (LHWND>0) then
       begin
          RetVal := GetClassName(wParam, ClassNameBuffer, SizeOf(ClassNameBuffer));
          if RetVal>0 then
          begin
            Assert(RetVal < ClassNameBufferSize, 'Class name larger than fixed buffer size');
            //AddLog('Before HCBT_SETFOCUS '+ClassNameBuffer);
            if (TColorizerLocalSettings.HookedWindows.IndexOf(ClassNameBuffer)>=0) or  (TColorizerLocalSettings.HookedScrollBars.IndexOf(ClassNameBuffer)>=0) then
            begin
              LWinControl:=FindControl(LHWND);   //use FindControl because some forms are not registered in the Screen.Forms list

              LParentForm:=nil;
              if (LWinControl<>nil) then
                LParentForm:=GetParentForm(LWinControl);

              if (LWinControl<>nil) and ((LParentForm<>nil) and (TColorizerLocalSettings.HookedWindows.IndexOf(LParentForm.ClassName)>=0)) then
              begin
                Colorizer.Utils.ProcessComponent(TColorizerLocalSettings.ColorMap, TColorizerLocalSettings.ActionBarStyle, LWinControl);
                //AddLog('HCBT_SETFOCUS '+ClassNameBuffer);
              end;
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
             //AddLog('Before HCBT_ACTIVATE '+ClassNameBuffer);
            if (TColorizerLocalSettings.HookedWindows.IndexOf(ClassNameBuffer)>=0) then
            for i := 0 to Screen.FormCount-1 do
             if (Screen.Forms[i].Handle=LHWND) and not (csDesigning in Screen.Forms[i].ComponentState) then
               begin
                 Colorizer.Utils.ProcessComponent(TColorizerLocalSettings.ColorMap, TColorizerLocalSettings.ActionBarStyle, Screen.Forms[i]);
                 //AddLog('HCBT_ACTIVATE '+ClassNameBuffer);
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
   hhk := SetWindowsHookEx(WH_CBT, @CBT_FUNC, hInstance, GetCurrentThreadId());
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
