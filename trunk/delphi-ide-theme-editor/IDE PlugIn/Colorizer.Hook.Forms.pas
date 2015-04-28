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
// Portions created by Rodrigo Ruz V. are Copyright (C) 2011-2015 Rodrigo Ruz V.
// All Rights Reserved.
//
//**************************************************************************************************
unit Colorizer.Hook.Forms;

interface
{$I ..\Common\Jedi.inc}

  Procedure InstallFormsHook();
  Procedure RemoveFormsHook();
{$IFDEF DELPHIXE2_UP}
  Procedure RefreshColorizerVCLStyle(Invalidate : Boolean = False);
{$ENDIF}

implementation

uses
{$IFDEF DELPHIXE2_UP}
  Vcl.Styles,
  Vcl.Themes,
  Winapi.UxTheme,
{$ELSE}
  Themes,
  UxTheme,
  Colorizer.uxThemeHelper,
{$ENDIF}
  Classes,
  Forms,
  uMisc,
  DDetours,
  Windows,
  SysUtils,
  StrUtils,
  Controls,
  Vcl.ComCtrls,
  StdCtrls,
  Messages,
{$IFDEF DELPHIXE2_UP}
  Generics.Collections,
  Colorizer.Vcl.Styles,
{$ENDIF}
  Colorizer.Utils;

type
 TCustomFormClass        = class(TCustomForm);
{$IFDEF DELPHIXE2_UP}
 TWinControlClass        = class(TWinControl);
{$ENDIF}

var
 hhk: HHOOK = 0;
{$IFDEF DELPHIXE2_UP}
 //Trampoline_TWinControl_WndProc : procedure (Self : TWinControl;var Message: TMessage) = nil;
 //Trampoline_TWinControl_Destroy : procedure (Self : TWinControl);
{$ENDIF}
 Trampoline_TCustomForm_WndProc : procedure (Self : TCustomForm;var Message: TMessage) = nil;
 Trampoline_TCustomForm_DoCreate: procedure(Self : TCustomForm) = nil;

 Trampoline_TWincontrol_WndProc : procedure (Self : TWinControlClass;var Message: TMessage) = nil;

{$IFDEF DELPHIXE2_UP}
 HookedControls : TObjectDictionary<TWinControl, TColorizerStyleHook>;

Procedure RefreshColorizerVCLStyle(Invalidate : Boolean = False);
var
  i : Integer;
  LForm    : TCustomForm;
//  LControl : TWinControl;
begin
  for i := 0 to Screen.FormCount - 1 do
  if Invalidate then
  begin
    if Screen.Forms[i].HandleAllocated then
      if IsWindowVisible(Screen.Forms[I].Handle) then
        Screen.Forms[i].Invalidate;
  end
  else
  begin
    LForm:=Screen.Forms[i];
//    if LForm.HandleAllocated then
//      if IsWindowVisible(LForm.Handle) and Assigned(TColorizerLocalSettings.HookedWindows) and (TColorizerLocalSettings.HookedWindows.IndexOf(LForm.ClassName)>=0) then
//      begin
//        LControl := LForm.ActiveControl;
//        PostMessage(LForm.Handle, WM_SETREDRAW, WPARAM(LongBool(False)), 0);
//        PostMessage(LForm.Handle, CM_RECREATEWND, 0, 0);
//        if LControl <> nil then
//          LForm.ActiveControl:= LControl;
//
//        //SetWindowPos(LForm.Handle, 0, 0, 0, 0, 0, SWP_FRAMECHANGED or SWP_NOMOVE or SWP_NOZORDER or SWP_NOSIZE or SWP_NOACTIVATE);
//      end
//      else
//      if Assigned(TColorizerLocalSettings.HookedWindows) and (TColorizerLocalSettings.HookedWindows.IndexOf(Screen.Forms[i].ClassName)>=0) then
//      begin
//        LControl := LForm.ActiveControl;
//        SendMessage(LForm.Handle, WM_SETREDRAW, WPARAM(LongBool(False)), 0);
//        SendMessage(LForm.Handle, CM_RECREATEWND, 0, 0);
//        if LControl <> nil then
//          LForm.ActiveControl:= LControl;
//
//        //SetWindowPos(LForm.Handle, 0, 0, 0, 0, 0, SWP_FRAMECHANGED or SWP_NOMOVE or SWP_NOZORDER or SWP_NOSIZE or SWP_NOACTIVATE);
//      end;

//    if (LForm.HandleAllocated) and (LForm.Parent=nil) then
//      if IsWindowVisible(LForm.Handle) and Assigned(TColorizerLocalSettings.HookedWindows) and (TColorizerLocalSettings.HookedWindows.IndexOf(LForm.ClassName)>=0)  then
//        PostMessage(LForm.Handle, CM_CUSTOMSTYLECHANGED, 0, 0)
//      else
//      if Assigned(TColorizerLocalSettings.HookedWindows) and (TColorizerLocalSettings.HookedWindows.IndexOf(Screen.Forms[i].ClassName)>=0) then
//        SendMessage(LForm.Handle, CM_CUSTOMSTYLECHANGED, 0, 0);

    if (LForm.HandleAllocated) and (LForm.Parent=nil) and Assigned(TColorizerLocalSettings.HookedWindows) and (TColorizerLocalSettings.HookedWindows.IndexOf(LForm.ClassName)>=0) then
        SetWindowPos(LForm.Handle, 0, 0, 0, 0, 0, SWP_FRAMECHANGED or SWP_NOMOVE or SWP_NOZORDER or SWP_NOSIZE or SWP_NOACTIVATE);
  end;

  if Invalidate then
    Application.ProcessMessages;
end;

function HandleColorizerStyleMessage(Self : TWinControl;var Message: TMessage; WindowProc : TWndMethod): Boolean;
var
  LHook: TColorizerStyleHook;
begin
  Result:=false;
  LHook:=nil;

  if HookedControls.ContainsKey(Self) then
    LHook:=HookedControls[Self]
  else
  begin
    if Self is TCustomForm then
    begin
      HookedControls.Add(Self, TColorizerFormStyleHook.Create(Self));
      LHook:=HookedControls[Self];
    end
    else
    if Self is TCustomCheckBox then // try with TCustomCheckbox
    begin
      HookedControls.Add(Self, TColorizerCheckBoxStyleHook.Create(Self));
      LHook:=HookedControls[Self];
    end
    else
    if Self is TRadioButton then
    begin
      HookedControls.Add(Self, TColorizerRadioButtonStyleHook.Create(Self));
      LHook:=HookedControls[Self];
    end
    else
    if Self is TCustomStatusBar then
    begin
      HookedControls.Add(Self, TColorizerStatusBarStyleHook.Create(Self));
      LHook:=HookedControls[Self];
    end
    else
    if Self is TEdit then
    begin
      HookedControls.Add(Self, TColorizerEditStyleHook.Create(Self));
      LHook:=HookedControls[Self];
    end
    else
    if Self is TListBox then
    begin
      HookedControls.Add(Self, TColorizerListBoxStyleHook.Create(Self));
      LHook:=HookedControls[Self];
    end
    else
    if Self is TTrackBar then
    begin
      HookedControls.Add(Self, TColorizerTrackBarStyleHook.Create(Self));
      LHook:=HookedControls[Self];
    end
    else
    if Self is TTreeView then
    begin
      HookedControls.Add(Self, TColorizerTreeViewStyleHook.Create(Self));
      LHook:=HookedControls[Self];
    end
//    else
//    if (Self is TComboBox) or (Self is TCustomComboBox) or (MatchText(Self.ClassName, ['TDesktopComboBox', 'THistoryPropComboBox'])) then  //
//    begin
//      HookedControls.Add(Self, TColorizerComboBoxStyleHook.Create(Self));
//      LHook:=HookedControls[Self];
//    end
    ;

  end;

  if LHook<>nil then
   Result := LHook.HandleMessage(Message);
  //AddLog('HandleColorizerStyleMessage', Self.ClassName+' '+WM_To_String(Message.Msg));
end;

//procedure Detour_TWinControl_WndProc(Self : TWinControl;var Message: TMessage);
//var
// LWindowProc : TWndMethod;
//begin
// LWindowProc := TWinControlClass(Self).WindowProc;
//  if Assigned(TColorizerLocalSettings.Settings) and (TColorizerLocalSettings.Settings.Enabled) and TColorizerLocalSettings.Settings.UseVCLStyles and TColorizerLocalSettings.Settings.VCLStylesForms and (TColorizerLocalSettings.Settings.VCLStyleName<>'') and
//     Assigned(TColorizerLocalSettings.HookedWindows) and (TColorizerLocalSettings.HookedWindows.IndexOf(Self.ClassName)>=0) and
//     (Self is TCustomForm) and (Self.Parent=nil) and (Self.HostDockSite=nil) and
//     //not (csDesigning in Self.ComponentState) and
//     (TWinControlClass(Self).WindowHandle <> 0) {HandleAllocated} and
//     not (csDestroying in TWinControlClass(Self).ComponentState) and
//     not (csDestroyingHandle in TWinControlClass(Self).ControlState) and
//     not (csOverrideStylePaint in TWinControlClass(Self).ControlStyle) and
//     HandleColorizerStyleMessage(Self, Message, LWindowProc) then
//    Exit;
//
//  Trampoline_TWinControl_WndProc(Self, Message);
//end;

//procedure Detour_TWinControl_Destroy(Self : TWinControl);
//begin
// if HookedControls.ContainsKey(Self) then
//   HookedControls.Remove(Self);
// Trampoline_TWinControl_Destroy(Self);
//end;
{$ENDIF}

procedure Detour_TWinControl_WndProc(Self : TWinControlClass;var Message: TMessage);
var
  LWindowProc : TWndMethod;
  LParentForm : TCustomForm;
begin
  if not (TWinControl(Self) is TCustomForm) then
  begin
    LParentForm:= GetParentForm(Self);
    LWindowProc := Self.WindowProc;
//    if TWinControl(Self) is TCustomCheckBox then
//       AddLog2('Detour_TWinControl_WndProc '+Self.ClassName);

    if Assigned(LParentForm) and Assigned(TColorizerLocalSettings.HookedWindows) and (TColorizerLocalSettings.HookedWindows.IndexOf(LParentForm.ClassName)>=0)
    and Assigned(TColorizerLocalSettings.Settings) and (TColorizerLocalSettings.Settings.Enabled) then
      if (Self.WindowHandle <> 0) {HandleAllocated} and
        //((sfHandleMessages) in TStyleManager.Flags) and
         not (csDesigning in Self.ComponentState) and
         not (csDestroying in Self.ComponentState) and
         not (csDestroyingHandle in Self.ControlState) and
         not (csOverrideStylePaint in Self.ControlStyle)
         //and (Self.StyleElements <> [])
         and HandleColorizerStyleMessage(Self, Message, LWindowProc)
         then
          Exit;
  end;

  Trampoline_TWincontrol_WndProc(Self, Message);
end;

procedure Detour_TCustomForm_WndProc(Self : TCustomForm;var Message: TMessage);
{$IFDEF DELPHIXE2_UP}
var
 LWindowProc : TWndMethod;
 sClassName  : string;
{$ENDIF}
begin
{$IFDEF DELPHIXE2_UP}
 sClassName:=Self.ClassName;
// if (TWinControlClass(Self).WindowHandle <> 0) then
//  sClassName := GetWindowClassName(TWinControlClass(Self).WindowHandle);
 LWindowProc := Self.WindowProc;
  if not TColorizerLocalSettings.Unloading and
     (sClassName<>'') and Assigned(TColorizerLocalSettings.Settings) and (TColorizerLocalSettings.Settings.Enabled)
     and TColorizerLocalSettings.Settings.UseVCLStyles and TColorizerLocalSettings.Settings.VCLStylesForms
     and (TColorizerLocalSettings.Settings.VCLStyleName<>'') and (Self.Visible)
     and Assigned(TColorizerLocalSettings.HookedWindows) and (TColorizerLocalSettings.HookedWindows.IndexOf(sClassName)>=0) and
     (Self.Parent=nil) and (Self.HostDockSite=nil) and
     not (csDesigning in Self.ComponentState) and
     not (csDestroying in Self.ComponentState) and
     not (csDestroyingHandle in Self.ControlState) and
     not (csOverrideStylePaint in Self.ControlStyle) and
     HandleColorizerStyleMessage(Self, Message, LWindowProc) then
    Exit;
{$ENDIF}

  //if Assigned(TColorizerLocalSettings.Settings) and TColorizerLocalSettings.Settings.Enabled and (SameText(Self.ClassName, 'TDisassemblyView')) then
    //AddLog('Detour_TCustomForm_WndProc', Self.ClassName+' '+WM_To_String(Message.Msg));
{$IFDEF DLLWIZARD}
 case Message.Msg of
  WM_PAINT  : if Assigned(TColorizerLocalSettings.Settings) and TColorizerLocalSettings.Settings.Enabled and (SameText(Self.ClassName, 'TAppBuilder')) then
              begin
                 RefreshIDETheme();
              end;

  WM_CLOSE  : if Assigned(TColorizerLocalSettings.Settings) and TColorizerLocalSettings.Settings.Enabled and (SameText(Self.ClassName, 'TAppBuilder')) then
              begin
                RestoreIDESettingsFast();
              end;
 end;
{$ENDIF}
 Trampoline_TCustomForm_WndProc(Self, Message);
end;

procedure Detour_TCustomForm_DoCreate(Self : TCustomForm);
begin
  Trampoline_TCustomForm_DoCreate(Self);

  if not TColorizerLocalSettings.Unloading and Assigned(TColorizerLocalSettings.Settings) and TColorizerLocalSettings.Settings.Enabled and Assigned(TColorizerLocalSettings.HookedWindows) and (TColorizerLocalSettings.HookedWindows.IndexOf(Self.ClassName)>=0) then
    ProcessComponent(TColorizerLocalSettings.ColorMap, TColorizerLocalSettings.ActionBarStyle, Self);
end;


function CBT_FUNC(nCode: Integer; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var
 LHWND  : HWND;
 i      : Integer;
 sClassName : string;
 LWinControl : TWinControl;
 LParentForm : TCustomForm;
begin
   if not TColorizerLocalSettings.Unloading and Assigned(TColorizerLocalSettings.Settings) and TColorizerLocalSettings.Settings.Enabled and Assigned(TColorizerLocalSettings.ColorMap)
   and Assigned(TColorizerLocalSettings.HookedWindows) and Assigned(TColorizerLocalSettings.HookedScrollBars) then
   case nCode of

     HCBT_SETFOCUS:
     begin
       LHWND := HWND(wParam);
       if (Screen<>nil) and (LHWND>0) then
       begin
          sClassName:= GetWindowClassName(wParam);
          if sClassName<>'' then
          begin
            //AddLog('Before HCBT_SETFOCUS '+ClassNameBuffer);
            if (TColorizerLocalSettings.HookedWindows.IndexOf(sClassName)>=0) or  (TColorizerLocalSettings.HookedScrollBars.IndexOf(sClassName)>=0) then
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
 {
     HCBT_MOVESIZE:
     begin
       LHWND := HWND(wParam);
       if (Screen<>nil) and (LHWND>0) then
       begin
          RetVal := GetClassName(wParam, ClassNameBuffer, SizeOf(ClassNameBuffer));
          if RetVal>0 then
          begin
             AddLog2('Before HCBT_MOVESIZE '+ClassNameBuffer);
          end;
       end;
     end;
}
     HCBT_ACTIVATE:
     begin
       LHWND := HWND(wParam);

       if (Screen<>nil) and (LHWND>0) then
       begin
          sClassName:= GetWindowClassName(wParam);
          if sClassName<>'' then
          begin
             //AddLog2('Before HCBT_ACTIVATE '+sClassName);
            if (TColorizerLocalSettings.HookedWindows.IndexOf(sClassName)>=0) then
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
{$IFDEF DELPHIXE2_UP}
  HookedControls := TObjectDictionary<TWinControl, TColorizerStyleHook>.Create([doOwnsValues]);
{$ENDIF}
  if (hhk = 0) then
   hhk := SetWindowsHookEx(WH_CBT, @CBT_FUNC, hInstance, GetCurrentThreadId());

{$IFDEF DELPHIXE2_UP}
  //Trampoline_TWinControl_WndProc    := InterceptCreate(@TWinControlClass.WndProc, @Detour_TWinControl_WndProc);
  //Trampoline_TWinControl_Destroy    := InterceptCreate(@TWinControlClass.Destroy, @Detour_TWinControl_Destroy);
{$ENDIF}
  Trampoline_TCustomForm_DoCreate   := InterceptCreate(@TCustomFormClass.DoCreate, @Detour_TCustomForm_DoCreate);
  Trampoline_TCustomForm_WndProc    := InterceptCreate(@TCustomFormClass.WndProc, @Detour_TCustomForm_WndProc);

  Trampoline_TWincontrol_WndProc    := InterceptCreate(@TWinControlClass.WndProc, @Detour_TWinControl_WndProc);
end;

Procedure RemoveFormsHook();
begin
  if (hhk <> 0) then
  begin
    UnhookWindowsHookEx(hhk);
    hhk:=0;
  end;

  InterceptRemove(@Trampoline_TCustomForm_DoCreate);
  InterceptRemove(@Trampoline_TCustomForm_WndProc);
  InterceptRemove(@Trampoline_TWincontrol_WndProc);

{$IFDEF DELPHIXE2_UP}
  SetColorizerVCLStyle('');
  RefreshColorizerVCLStyle;
{$ENDIF}


{$IFDEF DELPHIXE2_UP}
  FreeAndNil(HookedControls);
{$ENDIF}
end;

end.
