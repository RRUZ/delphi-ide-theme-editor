//**************************************************************************************************
//
// Unit Colorizer.XPStyleActnCtrls
// unit Colorizer.XPStyleActnCtrls  for the Delphi IDE Colorizer
//
// The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
// you may not use this file except in compliance with the License. You may obtain a copy of the
// License at http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
// ANY KIND, either express or implied. See the License for the specific language governing rights
// and limitations under the License.
//
// The Original Code is Colorizer.XPStyleActnCtrls.pas.
//
// The Initial Developer of the Original Code is Rodrigo Ruz V.
// Portions created by Rodrigo Ruz V. are Copyright (C) 2011-2014 Rodrigo Ruz V.
// All Rights Reserved.
//
//**************************************************************************************************

unit Colorizer.XPStyleActnCtrls;

interface
{$I ..\Common\Jedi.inc}

uses
  ActnMan,
  ActnMenus,
  Classes,
  Graphics,
  ActnColorMaps,
  ActnCtrls;

type
  TColorizerColorMap = class(TXPColorMap)
  private
    FWindowColor: TColor;
  public
    procedure Assign(Source: TPersistent); override;
    procedure UpdateColors; override;
  published
    property WindowColor: TColor read FWindowColor write FWindowColor default clNone;
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
  Colorizer.StoreColorMap,
  ListActns,
  XPActnCtrls,
  {$IFDEF DELPHIXE2_UP}
  PlatformDefaultStyleActnCtrls,
  Colorizer.Vcl.Styles,
  {$ENDIF}
  Themes;

{ TColorXPStyleActionBars }

function TColorXPStyleActionBars.GetAddRemoveItemClass(
  ActionBar: TCustomActionBar): TCustomAddRemoveItemClass;
begin
  {$IFDEF DELPHIXE2_UP}
  if TStyleManager.IsCustomStyleActive then
    Result := PlatformDefaultStyle.GetAddRemoveItemClass(ActionBar)
  else
  {$ENDIF}
    Result := TXPStyleAddRemoveItem;
end;

function TColorXPStyleActionBars.GetColorMapClass(
  ActionBar: TCustomActionBar): TCustomColorMapClass;
begin
  {$IFDEF DELPHIXE2_UP}
  if TStyleManager.IsCustomStyleActive then
    Result := PlatformDefaultStyle.GetColorMapClass(ActionBar)
  else
  {$ENDIF}
    Result := {TXPColorMap{TTwilightColorMap}TColorizerColorMap; //use own
end;

function TColorXPStyleActionBars.GetControlClass(ActionBar: TCustomActionBar;
  AnItem: TActionClientItem): TCustomActionControlClass;
begin
  {$IFDEF DELPHIXE2_UP}
  if TStyleManager.IsCustomStyleActive then
    Result := PlatformDefaultStyle.GetControlClass(ActionBar, AnItem)
  else
  {$ENDIF}
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
  {$IFDEF DELPHIXE2_UP}
  if TStyleManager.IsCustomStyleActive then
    Result := PlatformDefaultStyle.GetPopupClass(ActionBar)
  else
  {$ENDIF}
  if ActionBar is TCustomActionToolBar then
    Result := TXPStyleCustomizePopup
  else
    Result := TXPStylePopupMenu;
end;

function TColorXPStyleActionBars.GetScrollBtnClass: TCustomToolScrollBtnClass;
begin
  {$IFDEF DELPHIXE2_UP}
  if TStyleManager.IsCustomStyleActive then
    Result := PlatformDefaultStyle.GetScrollBtnClass
  else
  {$ENDIF}
    Result := TXPStyleToolScrollBtn;
end;

function TColorXPStyleActionBars.GetStyleName: string;
begin
  Result := 'Color XP Style';
end;

{ TColorXPColorMap }

procedure TColorizerColorMap.Assign(Source: TPersistent);
begin
  if Source is TCustomActionBarColorMap then
  begin
    BtnSelectedColor := TCustomActionBarColorMap(Source).BtnSelectedColor;
    BtnFrameColor := TCustomActionBarColorMap(Source).BtnFrameColor;
    BtnSelectedFont := TCustomActionBarColorMap(Source).BtnSelectedFont;
    Color := TCustomActionBarColorMap(Source).Color;
    DesignFocus := TCustomActionBarColorMap(Source).DesignFocus;
    DisabledColor := TCustomActionBarColorMap(Source).DisabledColor;
    DisabledFontColor := TCustomActionBarColorMap(Source).DisabledFontColor;
    DisabledFontShadow := TCustomActionBarColorMap(Source).DisabledFontShadow;
    FontColor := TCustomActionBarColorMap(Source).FontColor;
    FrameTopLeftInner := TCustomActionBarColorMap(Source).FrameTopLeftInner;
    FrameTopLeftOuter := TCustomActionBarColorMap(Source).FrameTopLeftOuter;
    FrameBottomRightInner := TCustomActionBarColorMap(Source).FrameBottomRightInner;
    FrameBottomRightOuter := TCustomActionBarColorMap(Source).FrameBottomRightOuter;
    HighlightColor := TCustomActionBarColorMap(Source).HighlightColor;
    HotColor := TCustomActionBarColorMap(Source).HotColor;
    HotFontColor := TCustomActionBarColorMap(Source).HotFontColor;
    MenuColor := TCustomActionBarColorMap(Source).MenuColor;
    SelectedColor := TCustomActionBarColorMap(Source).SelectedColor;
    SelectedFontColor := TCustomActionBarColorMap(Source).SelectedFontColor;
    ShadowColor := TCustomActionBarColorMap(Source).ShadowColor;
    UnusedColor := TCustomActionBarColorMap(Source).UnusedColor;
    WindowColor := clWindow;
  end;
end;

procedure TColorizerColorMap.UpdateColors;
begin
  inherited;
  WindowColor:=clNone;
  if Assigned(TColorizerLocalSettings.Settings) and TColorizerLocalSettings.Settings.Enabled then
   LoadColorMapFromXmlFile(Self, TColorizerLocalSettings.Settings.ThemeFileName);

 {$IFDEF DELPHIXE2_UP}
  if Assigned(TColorizerLocalSettings.Settings) and TColorizerLocalSettings.Settings.Enabled and TColorizerLocalSettings.Settings.UseVCLStyles and TColorizerLocalSettings.Settings.VCLStylesMenusColors  then
   AssignColorsFromVCLStyle(Self, ColorizerStyleServices);
{$ENDIF}
end;

initialization
  ColorXPStyle := TColorXPStyleActionBars.Create;
  RegisterActnBarStyle(ColorXPStyle);
  TColorizerLocalSettings.ActionBarStyle:=ColorXPStyle;
finalization
  UnregisterActnBarStyle(ColorXPStyle);
  ColorXPStyle.Free;
end.

