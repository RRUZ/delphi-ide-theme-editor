//**************************************************************************************************
//
// Unit ColorXPStyleActnCtrls
// unit ColorXPStyleActnCtrls  for the Delphi IDE Colorizer
//
// The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
// you may not use this file except in compliance with the License. You may obtain a copy of the
// License at http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
// ANY KIND, either express or implied. See the License for the specific language governing rights
// and limitations under the License.
//
// The Original Code is ColorXPStyleActnCtrls.pas.
//
// The Initial Developer of the Original Code is Rodrigo Ruz V.
// Portions created by Rodrigo Ruz V. are Copyright (C) 2011-2014 Rodrigo Ruz V.
// All Rights Reserved.
//
//**************************************************************************************************

unit ColorXPStyleActnCtrls;

interface

uses
  ActnMan,
  ActnMenus,
  Graphics,
  ActnColorMaps,
  ActnCtrls;

type
  TColorXPColorMap = class(TXPColorMap)
  public
    procedure UpdateColors; override;
  end;


{ TColorXPStyleActionBars }

  TColorXPStyleActionBars = class(TActionBarStyleEx)
  public
    function GetColorMapClass(ActionBar: TCustomActionBar): TCustomColorMapClass; override;
    function GetControlClass(ActionBar: TCustomActionBar;
      AnItem: TActionClientItem): TCustomActionControlClass; override;
    function GetPopupClass(ActionBar: TCustomActionBar): TCustomPopupClass; override;
    function GetAddRemoveItemClass(ActionBar: TCustomActionBar): TCustomAddRemoveItemClass; override;
    function GetStyleName: string; override;
    function GetScrollBtnClass: TCustomToolScrollBtnClass; override;
  end;

var
  ColorXPStyle: TColorXPStyleActionBars;

implementation

uses
  Colorizer.Utils,
  ListActns,
  XPActnCtrls,
  {$IF CompilerVersion>=23} //XE2
  PlatformDefaultStyleActnCtrls,
  {$IFEND}
  Themes;

{ TColorXPStyleActionBars }

function TColorXPStyleActionBars.GetAddRemoveItemClass(
  ActionBar: TCustomActionBar): TCustomAddRemoveItemClass;
begin
  {$IF CompilerVersion>=23} //XE2
  if TStyleManager.IsCustomStyleActive then
    Result := PlatformDefaultStyle.GetAddRemoveItemClass(ActionBar)
  else
  {$IFEND}
    Result := TXPStyleAddRemoveItem;
end;

function TColorXPStyleActionBars.GetColorMapClass(
  ActionBar: TCustomActionBar): TCustomColorMapClass;
begin
  {$IF CompilerVersion>=23} //XE2
  if TStyleManager.IsCustomStyleActive then
    Result := PlatformDefaultStyle.GetColorMapClass(ActionBar)
  else
  {$IFEND}
    Result := {TXPColorMap{TTwilightColorMap}TColorXPColorMap; //use own
end;

function TColorXPStyleActionBars.GetControlClass(ActionBar: TCustomActionBar;
  AnItem: TActionClientItem): TCustomActionControlClass;
begin
  {$IF CompilerVersion>=23} //XE2
  if TStyleManager.IsCustomStyleActive then
    Result := PlatformDefaultStyle.GetControlClass(ActionBar, AnItem)
  else
  {$IFEND}
  begin
    if ActionBar is TCustomActionToolBar then
    begin
      if AnItem.HasItems then
        Result := TXPStyleDropDownBtn
      else
        if (AnItem.Action is TStaticListAction) or
           (AnItem.Action is TVirtualListAction) then
          Result := TCustomComboControl
        else
          Result := TXPStyleButton;
    end
    else if ActionBar is TCustomActionMainMenuBar then
      Result := TXPStyleMenuButton
    else if ActionBar is TCustomizeActionToolBar then
    begin
      with TCustomizeActionToolbar(ActionBar) do
        if not Assigned(RootMenu) or
           (AnItem.ParentItem <> TCustomizeActionToolBar(RootMenu).AdditionalItem) then
          Result := TXPStyleMenuItem
        else
          Result := TXPStyleAddRemoveItem;
    end
    else if ActionBar is TCustomActionPopupMenu then
      Result := TXPStyleMenuItem
    else
      Result := TXPStyleButton;
  end;
end;

function TColorXPStyleActionBars.GetPopupClass(
  ActionBar: TCustomActionBar): TCustomPopupClass;
begin
  {$IF CompilerVersion>=23} //XE2
  if TStyleManager.IsCustomStyleActive then
    Result := PlatformDefaultStyle.GetPopupClass(ActionBar)
  else
  {$IFEND}
  if ActionBar is TCustomActionToolBar then
    Result := TXPStyleCustomizePopup
  else
    Result := TXPStylePopupMenu;
end;

function TColorXPStyleActionBars.GetScrollBtnClass: TCustomToolScrollBtnClass;
begin
  {$IF CompilerVersion>=23} //XE2
  if TStyleManager.IsCustomStyleActive then
    Result := PlatformDefaultStyle.GetScrollBtnClass
  else
  {$IFEND}
    Result := TXPStyleToolScrollBtn;
end;

function TColorXPStyleActionBars.GetStyleName: string;
begin
  Result := 'Color XP Style';
end;

{ TColorXPColorMap }

procedure TColorXPColorMap.UpdateColors;
begin
  inherited;
  LoadSettings(Self, TColorizerLocalSettings.Settings);
end;

initialization
  ColorXPStyle := TColorXPStyleActionBars.Create;
  RegisterActnBarStyle(ColorXPStyle);
  TColorizerLocalSettings.ActionBarStyle:=ColorXPStyle;
finalization
  UnregisterActnBarStyle(ColorXPStyle);
  ColorXPStyle.Free;
end.

