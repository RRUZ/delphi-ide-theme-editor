// **************************************************************************************************
//
// Unit Vcl.Styles.Utils.SystemMenu
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
// The Initial Developer of the Original Code is Rodrigo Ruz V.
// Portions created by Rodrigo Ruz V. are Copyright (C) 2014 Rodrigo Ruz V.
// All Rights Reserved.
//
// **************************************************************************************************
unit Vcl.Styles.Utils.SystemMenu;

interface

uses
  System.Rtti,
  System.Classes,
  System.Generics.Collections,
  WinApi.Windows,
  WinApi.Messages,
  Vcl.Themes,
  Vcl.Styles,
  Vcl.Forms;


type
  TMethodInfo=class;

  TProcCallback = reference to procedure(Info : TMethodInfo);
  TMethodInfo=class
   Value1 : TValue;
   Value2 : TValue;
   Method : TProcCallback;
  end;
  TVclStylesSystemMenu=class(TComponent)
  strict private
    FVCLStylesMenu : HMenu;
    FOrgWndProc: TWndMethod;
    FForm : TForm;
    FMethodsDict : TObjectDictionary<NativeUInt, TMethodInfo>;
    procedure CreateMenus;
    procedure DeleteMenus;
    procedure CreateMenuStyles;
    procedure WndProc(var Message: TMessage);
  public
    constructor Create(AOwner: TForm); reintroduce;
    destructor Destroy; override;
  end;


implementation

uses
  Vcl.Controls,
  System.SysUtils;

const
 VCLStylesMenu=WM_USER + 666;

function InsertMenuHelper(hMenu: HMENU; uPosition: UINT; uIDNewItem: UINT_PTR; lpNewItem, IconName: LPCWSTR) : BOOL;
var
  LMenuItem : TMenuItemInfo;
begin
  ZeroMemory(@LMenuItem, SizeOf(TMenuItemInfo));
  LMenuItem.cbSize := SizeOf(TMenuItemInfo);
  LMenuItem.fMask  := MIIM_FTYPE or MIIM_ID or MIIM_BITMAP or MIIM_STRING;
  LMenuItem.fType  := MFT_STRING;
  LMenuItem.wID    := uIDNewItem;
  LMenuItem.dwTypeData := lpNewItem;
  Result:=InsertMenuItem(hMenu, uPosition, True, LMenuItem);
end;

procedure AddMenuSeparatorHelper(hMenu : HMENU; var MenuIndex : Integer);
var
  LMenuInfo    : TMenuItemInfo;
  Buffer       : array [0..79] of char;
begin
  ZeroMemory(@LMenuInfo, SizeOf(TMenuItemInfo));
  LMenuInfo.cbSize := sizeof(LMenuInfo);
  LMenuInfo.fMask  := MIIM_TYPE;
  LMenuInfo.dwTypeData := Buffer;
  LMenuInfo.cch := SizeOf(Buffer);
  if GetMenuItemInfo(hMenu, MenuIndex-1, True, LMenuInfo) then
  begin
    if (LMenuInfo.fType and MFT_SEPARATOR) = MFT_SEPARATOR then
    else
    begin
      InsertMenu(hMenu, MenuIndex, MF_BYPOSITION or MF_SEPARATOR, 0, nil);
      inc(MenuIndex);
    end;
  end;
end;

{ TVclStylesSystemMenu }

constructor TVclStylesSystemMenu.Create(AOwner: TForm);
begin
  inherited Create(AOwner);
  FForm:=AOwner;
  FMethodsDict:=TObjectDictionary<NativeUInt, TMethodInfo>.Create([doOwnsValues]);
  FOrgWndProc := FForm.WindowProc;
  FForm.WindowProc := WndProc;
  CreateMenus;
end;


destructor TVclStylesSystemMenu.Destroy;
begin
  DeleteMenus;
  FForm.WindowProc := FOrgWndProc;
  FMethodsDict.Free;
  inherited;
end;

procedure TVclStylesSystemMenu.CreateMenus;
begin
  CreateMenuStyles;
end;

procedure TVclStylesSystemMenu.DeleteMenus;
begin
   if IsMenu(FVCLStylesMenu) then
   while GetMenuItemCount(FVCLStylesMenu)>0 do
     DeleteMenu(FVCLStylesMenu, 0, MF_BYPOSITION);

   FMethodsDict.Clear;
end;

procedure TVclStylesSystemMenu.CreateMenuStyles;
var
 LSysMenu : HMenu;
 LMenuItem: TMenuItemInfo;
 s : string;
 uIDNewItem, LSubMenuIndex : Integer;
 LMethodInfo : TMethodInfo;
begin
  LSysMenu := GetSystemMenu(FForm.Handle, False);

  LSubMenuIndex:=GetMenuItemCount(LSysMenu);
  AddMenuSeparatorHelper(LSysMenu,  LSubMenuIndex);

  FVCLStylesMenu   := CreatePopupMenu();
  s:='VCL Styles';

  uIDNewItem := VCLStylesMenu;
  ZeroMemory(@LMenuItem, SizeOf(TMenuItemInfo));
  LMenuItem.cbSize := SizeOf(TMenuItemInfo);
  LMenuItem.fMask  := MIIM_SUBMENU or MIIM_FTYPE or  MIIM_ID or MIIM_BITMAP or MIIM_STRING;
  LMenuItem.fType  := MFT_STRING;
  LMenuItem.wID    := VCLStylesMenu;
  LMenuItem.hSubMenu := FVCLStylesMenu;
  LMenuItem.dwTypeData := PWideChar(s);
  LMenuItem.cch := Length(s);

  InsertMenuItem(LSysMenu, GetMenuItemCount(LSysMenu), True, LMenuItem);
  inc(uIDNewItem);
  LSubMenuIndex:=0;
  for s in TStyleManager.StyleNames do
  begin
    InsertMenuHelper(FVCLStylesMenu, LSubMenuIndex, uIDNewItem,  PChar(s), nil);
    if SameText(TStyleManager.ActiveStyle.Name, s) then
      CheckMenuItem(FVCLStylesMenu, LSubMenuIndex, MF_BYPOSITION or MF_CHECKED);
    inc(LSubMenuIndex);
    inc(uIDNewItem);
    LMethodInfo:=TMethodInfo.Create;
    LMethodInfo.Value1:=s;
    LMethodInfo.Method:=procedure(Info : TMethodInfo)
                        begin
                          TStyleManager.SetStyle(Info.Value1.AsString);
                        end;
    FMethodsDict.Add(uIDNewItem-1, LMethodInfo);
  end;
end;

procedure TVclStylesSystemMenu.WndProc(var Message: TMessage);
var
  LVerb : NativeUInt;
begin
  case Message.Msg of
    CM_RECREATEWND: begin
                      DeleteMenus;
                      FOrgWndProc(Message);
                      CreateMenus;
                    end;

    WM_SYSCOMMAND : begin
                     if FMethodsDict.ContainsKey(TWMSysCommand(Message).CmdType) then
                     begin
                      LVerb:=TWMSysCommand(Message).CmdType;
                      FMethodsDict.Items[LVerb].Method(FMethodsDict.Items[LVerb]);
                     end
                     else
                      FOrgWndProc(Message);
                    end
  else
    FOrgWndProc(Message);
  end;
end;

end.
