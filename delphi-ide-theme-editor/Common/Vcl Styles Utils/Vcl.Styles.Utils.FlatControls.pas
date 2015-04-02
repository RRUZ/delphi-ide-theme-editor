// ***************************************************************************************************
//
// Unit Vcl.Styles.Utils.FlatControls
// unit for the VCL Styles Utils
// http://code.google.com/p/vcl-styles-utils/
//
// The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License")
// you may not use this file except in compliance with the License. You may obtain a copy of the
// License at http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
// ANY KIND, either express or implied. See the License for the specific language governing rights
// and limitations under the License.
//
//
// Portions created by Mahdi Safsafi [SMP3]   e-mail SMP@LIVE.FR
// Portions created by Rodrigo Ruz V. are Copyright (C) 2013-2015 Rodrigo Ruz V.
// All Rights Reserved.
//
// **************************************************************************************************
unit Vcl.Styles.Utils.FlatControls;

interface

uses
  Windows,
  Messages,
  Classes,
  Controls,
  Graphics,
  {$IF CompilerVersion >= 23}
  Vcl.Styles,
  {$IFEND}
  Themes,
  Types,
  SysUtils,
  Vcl.Styles.Utils.FlatStyleHook,
  Generics.Collections;

type
  PChildControlInfo = ^TChildControlInfo;

  TChildControlInfo = record
    Parent: HWND;
    ParentStyle: NativeInt;
    StyleHookClass: TSysStyleHookClass;
  end;

  PControlInfo = ^TControlInfo;

  TControlInfo = record
    Handle: HWND;
    Parent: HWND;
    Style: NativeInt;
    ParentStyle: NativeInt;
    ExStyle: NativeInt;
    ParentExStyle: NativeInt;
    ClassName: PChar;
    ParentClassName: PChar;
  end;

type
  TSysHookAction = (cAdded, cRemoved);
  TBeforeHookingControl = function(Info: PControlInfo): Boolean;
  TSysHookNotification = procedure(Action: TSysHookAction; Info: PControlInfo);

  TColorizerStyleManager = class(TComponent)
  private
  class var
    FEnabled: Boolean;
    FHook: HHook;
    FBeforeHookingControlProc: TBeforeHookingControl;
    FSysHookNotificationProc: TSysHookNotification;
    FRegSysStylesList: TObjectDictionary<String, TSysStyleHookClass>;
    FSysStyleHookList: TObjectDictionary<HWND, TSysStyleHook>;
    FChildRegSysStylesList: TObjectDictionary<HWND, TChildControlInfo>;
    FHookVclControls: Boolean;
    FUseStyleColorsChildControls: Boolean;
  protected
    /// <summary>
    /// Install the Hook
    /// </summary>
    class procedure InstallHook;
    /// <summary>
    /// Remove the Hook
    /// </summary>
    class procedure RemoveHook;
    /// <summary>
    /// Hook Callback
    /// </summary>
    class function HookCBProc(nCode: Integer; wParam: wParam; lParam: lParam): LRESULT; stdcall; static;
  public
    /// <summary>
    /// Register a Sys Style Hook for an specified class.
    /// </summary>
    class procedure RegisterSysStyleHook(const SysControlClass: String; SysStyleHookClass: TSysStyleHookClass);
    /// <summary>
    /// UnRegister a Sys Style Hook for an specified class.
    /// </summary>
    class procedure UnRegisterSysStyleHook(const SysControlClass: String; SysStyleHookClass: TSysStyleHookClass);
    class constructor Create;
    class destructor Destroy;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    /// <summary>
    /// Event to preventvor allow hook a control.
    /// </summary>
    class Property OnBeforeHookingControl: TBeforeHookingControl read FBeforeHookingControlProc write FBeforeHookingControlProc;
    /// <summary>
    /// Notify when a hook foir control is added or removed
    /// </summary>
    class Property OnHookNotification: TSysHookNotification read FSysHookNotificationProc write FSysHookNotificationProc;
    /// <summary>
    /// Enable or disable the style of the controls
    /// </summary>
    class property Enabled: Boolean read FEnabled write FEnabled;
    /// <summary>
    /// Allow set the current VCL Style font and background color in  child
    /// controls.
    /// </summary>
    class property UseStyleColorsChildControls: Boolean read FUseStyleColorsChildControls write FUseStyleColorsChildControls;
    /// <summary>
    /// Allow disable or enable the hook of VCL Controls
    /// </summary>
    class property HookVclControls: Boolean read FHookVclControls write FHookVclControls;
    /// <summary>
    /// Collection of Styled (Hooked) Controls
    /// </summary>
    class property SysStyleHookList: TObjectDictionary<HWND, TSysStyleHook> read FSysStyleHookList;
    /// <summary>
    /// Collection of Styled Child Controls
    /// </summary>
    class property ChildRegSysStylesList: TObjectDictionary<HWND, TChildControlInfo> read FChildRegSysStylesList;
  end;

function GetWindowClassName(Window: HWND): String;
function RectVCenter(var R: TRect; const Bounds: TRect): TRect;
procedure MoveWindowOrg(DC: HDC; const DX, DY: Integer);

implementation

uses
 Colorizer.Utils;

function GetWindowClassName(Window: HWND): String;
var
  sClassName: PChar;
begin
  GetMem(sClassName, 256);
  try
    GetClassName(Window, sClassName, 256);
    Result := String(sClassName);
  finally
    FreeMem(sClassName, 256);
  end;
end;

function RectVCenter(var R: TRect; const Bounds: TRect): TRect;
begin
  OffsetRect(R, -R.Left, -R.Top);
  {$IF CompilerVersion >= 23}
  OffsetRect(R, 0, (Bounds.Height - R.Height) div 2);
  {$ELSE}
  OffsetRect(R, 0, ((Bounds.Bottom-Bounds.Top) - (R.Bottom- R.Top)) div 2);
  {$IFEND}
  OffsetRect(R, Bounds.Left, Bounds.Top);
  Result := R;
end;

procedure MoveWindowOrg(DC: HDC; const DX, DY: Integer);
var
  P: TPoint;
begin
  GetWindowOrgEx(DC, P);
  SetWindowOrgEx(DC, P.X - DX, P.Y - DY, nil);
end;

function FindWinFromRoot(Root: HWND; ClassName: PChar): HWND;
var
  Next, Child: HWND;
  S: String;
begin
  Result := 0;
  Next := GetWindow(Root, GW_CHILD or GW_HWNDFIRST);
  while (Next > 0) do
  begin
    S := GetWindowClassName(Next);
    if S = String(ClassName) then
      Exit(Next);
    Next := GetWindow(Next, GW_HWNDNEXT);
    Child := GetWindow(Next, GW_CHILD or GW_HWNDFIRST);
    if Child > 0 then
      Result := FindWinFromRoot(Next, ClassName);
    if Result > 0 then
      Exit;
  end;
end;

{ -------------------------------------------------------------------------------------- }
{ TSysStyleManager }

function BeforeHookingControl(Info: PControlInfo): Boolean;
var
  LInfo: TControlInfo;
  Root, C: HWND;
begin
  {
    Return true to allow control hooking !
    Return false to prevent control hooking !
  }
  { NB: The ClassName is always in lowercase . }
  LInfo := Info^;
  Result := True;
  Root := GetAncestor(LInfo.Parent, GA_ROOT);
  if FindWinFromRoot(Root, 'DirectUIHWND') > 0 then
  begin
    Result := False;
    Exit;
  end;

  if SameText(LInfo.ClassName, 'ToolBarWindow32') then
  begin
    if Root > 0 then
    begin
      C := FindWinFromRoot(Root, 'ReBarWindow32');
      Result := not(C > 0);
    end;
  end;
end;

procedure HookNotification(Action: TSysHookAction; Info: PControlInfo);
begin

end;

class constructor TColorizerStyleManager.Create;
begin
  FBeforeHookingControlProc := @BeforeHookingControl;
  FSysHookNotificationProc := @HookNotification;
  FUseStyleColorsChildControls := True;
  FEnabled := True;
  FHookVclControls := False;
  FSysStyleHookList := TObjectDictionary<HWND, TSysStyleHook>.Create([doOwnsValues]);
  FRegSysStylesList := TObjectDictionary<String, TSysStyleHookClass>.Create;
  FChildRegSysStylesList := TObjectDictionary<HWND, TChildControlInfo>.Create;
  InstallHook;
end;

class destructor TColorizerStyleManager.Destroy;
begin
  RemoveHook;
  FRegSysStylesList.Free;
  FSysStyleHookList.Free; // remove the childs too because doOwnsValues
  FChildRegSysStylesList.Free;
  inherited;
end;

constructor TColorizerStyleManager.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TColorizerStyleManager.Destroy;
begin
  inherited;
end;

class function TColorizerStyleManager.HookCBProc(nCode: Integer; wParam: wParam; lParam: lParam): LRESULT;
var
  CBTSturct: TCBTCreateWnd;
  sClassName, Tmp: string;
  Parent: HWND;
  Style, ParentStyle, ExStyle, ParentExStyle: NativeInt;
  Info: TControlInfo;

  procedure AddChildControl(Handle: HWND);
  var
    Info: TChildControlInfo;
  begin
    { The child control will be hooked inside it's parent control. }
    ZeroMemory(@Info, sizeof(TChildControlInfo));
    Info.Parent := Parent;
    Info.ParentStyle := ParentStyle;
    Info.StyleHookClass := FRegSysStylesList[sClassName];
    if FChildRegSysStylesList.ContainsKey(Handle) then
      FChildRegSysStylesList.Remove(Handle);
    FChildRegSysStylesList.Add(Handle, Info);
    if Assigned(FSysHookNotificationProc) then
      FSysHookNotificationProc(cAdded, @Info);
  end;

  procedure AddControl(Handle: HWND);
  begin
    { Hook the control directly ! }
    if FSysStyleHookList.ContainsKey(Handle) then
      FSysStyleHookList.Remove(Handle);
    FSysStyleHookList.Add(Handle, FRegSysStylesList[sClassName].Create(Handle));
    SendMessage(Handle, CM_CONTROLHOOKEDDIRECTLY, 0, 0);
    if Assigned(FSysHookNotificationProc) then
      FSysHookNotificationProc(cAdded, @Info);
  end;

begin
  Result := CallNextHookEx(FHook, nCode, wParam, lParam);
  if not FEnabled then
    Exit;
  if (nCode = HCBT_CREATEWND) {$IF CompilerVersion >= 23} and (StyleServices.IsSystemStyle)  {$IFEND} and  Assigned(TColorizerLocalSettings.Settings) and (TColorizerLocalSettings.Settings.Enabled) then
  begin

    CBTSturct := PCBTCreateWnd(lParam)^;
    sClassName := GetWindowClassName(wParam);
    sClassName := LowerCase(sClassName);
    Parent := CBTSturct.lpcs.hwndParent;
    Style := CBTSturct.lpcs.Style;
    ExStyle := CBTSturct.lpcs.dwExStyle;
    ParentExStyle := 0;
    ParentStyle := 0;

    if Parent > 0 then
    begin
      ParentStyle := GetWindowLongPtr(Parent, GWL_STYLE);
      ParentExStyle := GetWindowLongPtr(Parent, GWL_EXSTYLE);
    end;

    if FRegSysStylesList.ContainsKey(sClassName) then
    begin
      Info.Handle := wParam;
      Info.Parent := Parent;
      Info.Style := Style;
      Info.ParentStyle := ParentStyle;
      Info.ExStyle := ExStyle;
      Info.ParentExStyle := ParentExStyle;
      Tmp := sClassName;
      Info.ClassName := PChar(Tmp);
      Tmp := LowerCase(GetWindowClassName(Parent));
      Info.ParentClassName := PChar(Tmp);

//      if IsVCLControl(wParam) then
//        AddLog('TFlatStyleManager', 'IsVCLControl');

      if not HookVclControls then
        if IsVCLControl(wParam) then
          Exit;

      if Assigned(FBeforeHookingControlProc) then
        if not FBeforeHookingControlProc(@Info) then
          Exit;

      if (Style and DS_CONTROL = DS_CONTROL) then
      begin
        { TabSheet ! }
        AddControl(wParam);
        PostMessage(wParam, CM_INITCHILDS, 0, 0);
      end
      else if (Style and WS_POPUP = WS_POPUP) then
      begin
        { Parent Control ! }
        AddControl(wParam);
      end
      else if (Style and WS_CHILD = WS_CHILD) then
      begin
        { Child Control ! }
        if FSysStyleHookList.ContainsKey(Parent) then
        begin
          { Parent is already hooked . }
          if IsVCLControl(Parent) then
            { Parent is a VCL control . }
            AddControl(wParam)
          else
            AddChildControl(wParam)
        end
        else
          { Parent not registered (not hooked). }
          AddControl(wParam);
      end
      else
        { Not (WS_CHILD or WS_POPUP) !! }
        AddControl(wParam);
    end;
  end;

  if nCode = HCBT_DESTROYWND then
  begin
    // OutputDebugString(PChar('HCBT_DESTROYWND Handle '+IntToHex(wParam, 8)));
    if FSysStyleHookList.ContainsKey(wParam) then
    begin
      ZeroMemory(@Info, sizeof(TControlInfo));
      Info.Handle := wParam;
      if Assigned(FSysHookNotificationProc) then
        OnHookNotification(cRemoved, @Info);
      // FSysStyleHookList.Remove(wParam); -> removed in WM_DESTROY
    end;
  end;
end;

class procedure TColorizerStyleManager.InstallHook;
begin
  FHook := SetWindowsHookEx(WH_CBT, @HookCBProc, 0, GetCurrentThreadId());
end;

class procedure TColorizerStyleManager.RegisterSysStyleHook(const SysControlClass: String; SysStyleHookClass: TSysStyleHookClass);
begin
  if FRegSysStylesList.ContainsKey(LowerCase(SysControlClass)) then
    FRegSysStylesList.Remove(LowerCase(SysControlClass));
  FRegSysStylesList.Add(LowerCase(SysControlClass), SysStyleHookClass);
end;

class procedure TColorizerStyleManager.RemoveHook;
begin
  if FHook <> 0 then
    UnhookWindowsHookEx(FHook);
end;

class procedure TColorizerStyleManager.UnRegisterSysStyleHook(const SysControlClass: String; SysStyleHookClass: TSysStyleHookClass);
begin
  if FRegSysStylesList.ContainsKey(LowerCase(SysControlClass)) then
    FRegSysStylesList.Remove(LowerCase(SysControlClass));
end;

end.
