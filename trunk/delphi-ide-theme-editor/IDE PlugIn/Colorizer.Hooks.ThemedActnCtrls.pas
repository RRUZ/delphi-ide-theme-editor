//**************************************************************************************************
//
// Unit Colorizer.Hooks.ThemedActnCtrl
// unit Colorizer.Hooks.ThemedActnCtrl for the Delphi IDE Colorizer
//
// The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
// you may not use this file except in compliance with the License. You may obtain a copy of the
// License at http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
// ANY KIND, either express or implied. See the License for the specific language governing rights
// and limitations under the License.
//
// The Original Code is Colorizer.Hooks.ThemedActnCtrl.pas.
//
// The Initial Developer of the Original Code is Rodrigo Ruz V.
// Portions created by Rodrigo Ruz V. are Copyright (C) 2011-2015 Rodrigo Ruz V.
// All Rights Reserved.
//
//**************************************************************************************************


unit Colorizer.Hooks.ThemedActnCtrls;

interface
{$I ..\Common\Jedi.inc}

 procedure InstallThemedActnCtrlsHooks;
 procedure RemoveThemedActnCtrlsHooks;


implementation

uses
  Windows,
  Messages,
  Forms,
  Menus,
  System.Classes,
  SysUtils,
  Vcl.Graphics,
  Vcl.Styles,
  Vcl.Themes,
  Winapi.UxTheme,
  Colorizer.VCL.Styles,
  Colorizer.Utils,
  Vcl.ActnMenus,
  Vcl.ActnPopup,
  Vcl.ActnMan,
  Vcl.ThemedActnCtrls,
  Vcl.XPActnCtrls,
  Vcl.Controls,
  DDetours;

type
  TThemedMenuItemClass        = class(TThemedMenuItem);
  TThemedMenuButtonClass      = class(TThemedMenuButton);
  TThemedPopupMenuClass       = class(TThemedPopupMenu);
  TCustomActionPopupMenuClass = class(TCustomActionPopupMenu);
  TXPStylePopupMenuClass      = class(TXPStylePopupMenu);
  TWinControlClass            = class(TWinControl);
var
  Trampoline_TThemedMenuItem_DrawBackground  : procedure (Self: TThemedMenuItemClass;var PaintRect: TRect) = nil;
  Trampoline_TThemedMenuItem_DrawSeparator   : procedure (Self: TThemedMenuItemClass;const Offset: Integer) = nil;
  Trampoline_TThemedMenuItem_DrawSubMenuGlyph: procedure (Self: TThemedMenuItemClass) = nil;
  Trampoline_TThemedMenuItem_DrawText        : procedure (Self: TThemedMenuItemClass;var Rect: TRect; var Flags: Cardinal; Text: string) = nil;
  Trampoline_TThemedMenuItem_DoDrawText      : procedure (Self: TThemedMenuItemClass;DC: HDC; const Text: string; var Rect: TRect; Flags: Longint) = nil;
  Trampoline_TThemedMenuItem_DoDrawMenuCheck : procedure (Self: TThemedMenuItemClass) = nil;

  Trampoline_TThemedMenuButton_DoDrawText    : procedure (Self: TThemedMenuButtonClass;const Text: string; var Rect: TRect; Flags: Longint) = nil;
  Trampoline_TThemedMenuButton_DrawBackground: procedure (Self: TThemedMenuButtonClass;var PaintRect: TRect) = nil;

  Trampoline_TThemedPopupMenu_NCPaint        : procedure (Self: TThemedPopupMenuClass; DC: HDC) = nil;
  Trampoline_TCustomActionPopupMenu_DrawBackground  : procedure (Self : TCustomActionPopupMenuClass) = nil;
  Trampoline_TCustomActionPopupMenu_CreateParams    : procedure (Self : TCustomActionPopupMenuClass; var Params: TCreateParams) = nil;
  Trampoline_TCustomActionPopupMenu_CMVisibleChanged: procedure (Self : TCustomActionPopupMenuClass;var Message: TMessage) = nil;
  Trampoline_TWinControl_CreateWnd                  : procedure (Self : TWinControl) = nil;

  //used for TCustomActionPopupMenuEx
  Trampoline_TXPStylePopupMenu_NCPaint       : procedure(Self : TXPStylePopupMenuClass;DC: HDC) = nil;

type
  TThemedMenuItemHelper =  class helper for TThemedMenuItem
  private
    function GetPaintRectHelper: TRect;
    procedure SetPaintRectHelper(const Value: TRect);
    function GetGutterRectHelper: TRect;
    procedure SetGutterRectHelper(const Value: TRect);
    function GetSeparatorHeightHelper: Integer;
    procedure SetSeparatorHeightHelper(const Value: Integer);
    function GetSubMenuGlyphRectHelper: TRect;
    procedure SetSubMenuGlyphRectHelper(const Value: TRect);
    function GetCheckRectHelper: TRect;
    procedure SetCheckRectHelper(const Value: TRect);
  public
    property PaintRectHelper : TRect read GetPaintRectHelper write SetPaintRectHelper;
    property GutterRectHelper : TRect read GetGutterRectHelper write SetGutterRectHelper;
    property SubMenuGlyphRectHelper : TRect read GetSubMenuGlyphRectHelper write SetSubMenuGlyphRectHelper;
    property SeparatorHeightHelper : Integer read GetSeparatorHeightHelper write SetSeparatorHeightHelper;
    property CheckRectHelper : TRect read GetCheckRectHelper write SetCheckRectHelper;
    procedure DoDrawTextHelper(DC: HDC; const Text: string; var Rect: TRect; Flags: Longint);
    function  DoDrawTextAddress: Pointer;
    function  DoDrawMenuCheckAddress : Pointer;
  end;

  TThemedMenuButtonHelper =  class helper for TThemedMenuButton
  public
    function  DoDrawTextAddress: Pointer;
  end;


function DoDrawText(DC: HDC; Details: TThemedElementDetails;
  const S: string; var R: TRect; Flags: TTextFormat; Options: TStyleTextOptions): Boolean;
var
  LFlags: Cardinal;
  LColorRef: TColorRef;
begin
    LFlags := TTextFormatFlags(Flags);
    LColorRef := SetTextColor(DC, Vcl.Graphics.ColorToRGB(Options.TextColor));
    try
      Windows.DrawText(DC, PChar(S), Length(S), R, LFlags);
    finally
      SetTextColor(DC, LColorRef);
    end;
    Result := True;
end;

function InternalDrawText(DC: HDC; Details: TThemedElementDetails; const S: string; var R: TRect; Flags: TTextFormat; Color: TColor = clNone): Boolean;
var
  LOptions: TStyleTextOptions;
begin
  if Color <> clNone then
  begin
    LOptions.Flags := [stfTextColor];
    LOptions.TextColor := Color;
  end
  else
    LOptions.Flags := [];
  Result := DoDrawText(DC, Details, S, R, Flags, LOptions);
end;

procedure Detour_TThemedMenuItem_DrawBackground(Self: TThemedMenuItemClass;var PaintRect: TRect);
begin
  if TColorizerLocalSettings.Settings.Enabled and TColorizerLocalSettings.Settings.UseVCLStyles and TColorizerLocalSettings.Settings.VCLStylesMenusColors then
  begin
    Self.PaintRectHelper := PaintRect;
//    if ColorizerStyleServices.IsSystemStyle then
//      ColorizerStyleServices.DrawElement(Self.Canvas.Handle, StyleServices.GetElementDetails(tmPopupBackground), Self.PaintRectHelper);
//    ColorizerStyleServices.DrawElement(Self.Canvas.Handle, StyleServices.GetElementDetails(tmPopupGutter), Self.GutterRectHelper);
//    ColorizerStyleServices.DrawElement(Self.Canvas.Handle, ColorizerStyleServices.GetElementDetails(tmPopupBackground), Self.PaintRectHelper);
    ColorizerStyleServices.DrawElement(Self.Canvas.Handle, ColorizerStyleServices.GetElementDetails(tmPopupGutter), Self.GutterRectHelper);
  end
  else
    Trampoline_TThemedMenuItem_DrawBackground(Self, PaintRect);
end;

procedure  Detour_TThemedMenuItem_DrawSeparator(Self: TThemedMenuItemClass;const Offset: Integer);
var
  LRect: TRect;
begin
  if TColorizerLocalSettings.Settings.Enabled and TColorizerLocalSettings.Settings.UseVCLStyles and TColorizerLocalSettings.Settings.VCLStylesMenusColors then
  begin
    LRect := Rect(Self.GutterRectHelper.Right + 1, 0, Self.Width, Self.SeparatorHeightHelper);
    ColorizerStyleServices.DrawElement(Self.Canvas.Handle, ColorizerStyleServices.GetElementDetails(tmPopupSeparator), LRect);
  end
  else
    Trampoline_TThemedMenuItem_DrawSeparator(Self, Offset);
end;

procedure  Detour_TThemedMenuItem_DrawSubMenuGlyph(Self: TThemedMenuItemClass);
const
  SubMenuStates: array[Boolean] of TThemedMenu = (tmPopupSubMenuDisabled, tmPopupSubMenuNormal);
var
  LRect: TRect;
begin
  if TColorizerLocalSettings.Settings.Enabled and TColorizerLocalSettings.Settings.UseVCLStyles and TColorizerLocalSettings.Settings.VCLStylesMenusColors then
  begin
    LRect := Self.SubMenuGlyphRectHelper;
    OffsetRect(LRect, Self.Width, 1);
    ColorizerStyleServices.DrawElement(Self.Canvas.Handle, ColorizerStyleServices.GetElementDetails(SubMenuStates[Self.Enabled]), LRect);
  end
  else
    Trampoline_TThemedMenuItem_DrawSubMenuGlyph(Self);
end;


procedure  Detour_TThemedMenuItem_DrawText(Self: TThemedMenuItemClass;var Rect: TRect; var Flags: Cardinal; Text: string);
var
  LRect: TRect;
begin
  if TColorizerLocalSettings.Settings.Enabled and TColorizerLocalSettings.Settings.UseVCLStyles and TColorizerLocalSettings.Settings.VCLStylesMenusColors then
  begin
    if Self.Selected and Self.Enabled then
      ColorizerStyleServices.DrawElement(Self.Canvas.Handle, ColorizerStyleServices.GetElementDetails(tmPopupItemHot), Self.PaintRectHelper)
    else if Self.Selected then
      ColorizerStyleServices.DrawElement(Self.Canvas.Handle, ColorizerStyleServices.GetElementDetails(tmPopupItemDisabledHot), Self.PaintRectHelper);

    if (Self.Parent is TCustomActionBar) and (not Self.ActionBar.PersistentHotkeys) then
      Text := Self.FNoPrefix;

    Self.Canvas.Font := Screen.MenuFont;

    if Self.ActionClient.Default then
      Self.Canvas.Font.Style := Self.Canvas.Font.Style + [fsBold];
    LRect := Self.PaintRectHelper;
    Self.DoDrawTextHelper(Self.Canvas.Handle, Text, LRect, Flags or DT_CALCRECT or DT_NOCLIP);

    OffsetRect(LRect, Rect.Left,
      ((Self.PaintRectHelper.Bottom - Self.PaintRectHelper.Top) - (LRect.Bottom - LRect.Top)) div 2);

    Self.DoDrawTextHelper(Self.Canvas.Handle, Text, LRect, Flags);

    if Self.ShowShortCut and ((Self.ActionClient <> nil) and not Self.ActionClient.HasItems) then
    begin
      Flags := Self.DrawTextBiDiModeFlags(DT_RIGHT);
      LRect := TRect.Create(Self.ShortCutBounds.Left, LRect.Top, Self.ShortCutBounds.Right, LRect.Bottom);
      LRect.Offset(Self.Width, 0);
      Self.DoDrawTextHelper(Self.Canvas.Handle, Self.ActionClient.ShortCutText, LRect, Flags);
    end;
  end
  else
    Trampoline_TThemedMenuItem_DrawText(Self, Rect, Flags,Text);
end;

procedure Detour_TThemedMenuItem_DoDrawText(Self: TThemedMenuItemClass;DC: HDC; const Text: string; var Rect: TRect; Flags: Longint);
const
  MenuStates: array[Boolean] of TThemedMenu = (tmPopupItemDisabled, tmPopupItemNormal);
var
  LCaption: string;
  LFormats: TTextFormat;
  LColor, LBackColor: TColor;
  LDetails: TThemedElementDetails;
begin
  if TColorizerLocalSettings.Settings.Enabled and TColorizerLocalSettings.Settings.UseVCLStyles and TColorizerLocalSettings.Settings.VCLStylesMenusColors then
  begin
    LFormats := TTextFormatFlags(Flags);
    if Self.Selected and Self.Enabled then
      LDetails := ColorizerStyleServices.GetElementDetails(tmPopupItemHot)
    else
      LDetails := ColorizerStyleServices.GetElementDetails(MenuStates[Self.Enabled or Self.ActionBar.DesignMode]);

    if not ColorizerStyleServices.GetElementColor(LDetails, ecTextColor, LColor) or (LColor = clNone) then
      LColor := Self.ActionBar.ColorMap.FontColor;

    LCaption := Text;
    if (tfCalcRect in LFormats) and ( (LCaption = '') or (LCaption[1] = cHotkeyPrefix) and (LCaption[2] = #0) ) then
      LCaption := LCaption + ' ';

    //ColorizerStyleServices.DrawText(DC, LDetails, LCaption, Rect, LFormats, LColor);


    //if ColorizerStyleServices.GetElementColor(LDetails, ecGlowColor, LBackColor)  then

    if Self.Selected and Self.Enabled then
    begin
      LBackColor:= ColorizerStyleServices.GetSystemColor(clHighlight);
      Self.Canvas.Brush.Color := LBackColor;
    end
    else
    if not Self.Enabled then
    begin
      LBackColor:=  TColorizerLocalSettings.ColorMap.MenuColor;//ColorizerStyleServices.GetSystemColor(clBtnFace);
      Self.Canvas.Brush.Color := LBackColor;
    end;

    InternalDrawText (Self.Canvas.Handle, LDetails, LCaption, Rect, LFormats, LColor);
  end
  else
    Trampoline_TThemedMenuItem_DoDrawText(Self, DC, Text, Rect, Flags);
end;

procedure  Detour_TThemedMenuItem_DoDrawMenuCheck(Self: TThemedMenuItemClass);
const
  CheckMarkBkgStates: array[Boolean] of TThemedMenu = (tmPopupCheckBackgroundDisabled, tmPopupCheckBackgroundNormal);
  CheckMarkStates: array[Boolean] of TThemedMenu = (tmPopupCheckDisabled, tmPopupCheckNormal);
begin
  if TColorizerLocalSettings.Settings.Enabled and TColorizerLocalSettings.Settings.UseVCLStyles and TColorizerLocalSettings.Settings.VCLStylesMenusColors then
  begin
    if Self.IsChecked then
    begin
      ColorizerStyleServices.DrawElement(Self.Canvas.Handle, ColorizerStyleServices.GetElementDetails(CheckMarkBkgStates[Self.Enabled]), Self.CheckRectHelper);
      if not Self.HasGlyph then
        ColorizerStyleServices.DrawElement(Self.Canvas.Handle, ColorizerStyleServices.GetElementDetails(CheckMarkStates[Self.Enabled]), Self.CheckRectHelper);
    end;
  end
  else
    Trampoline_TThemedMenuItem_DoDrawMenuCheck(Self);
end;


procedure Detour_TThemedPopupMenu_NCPaint(Self: TThemedPopupMenuClass; DC: HDC);
var
  RC, RW: TRect;
  OldHandle: THandle;
begin
  if TColorizerLocalSettings.Settings.Enabled and TColorizerLocalSettings.Settings.UseVCLStyles and TColorizerLocalSettings.Settings.VCLStylesMenusColors then
  begin
    Windows.GetClientRect(Self.Handle, RC);
    GetWindowRect(Self.Handle, RW);
    MapWindowPoints(0, Self.Handle, RW, 2);
    OffsetRect(RC, -RW.Left, -RW.Top);
    ExcludeClipRect(DC, RC.Left, RC.Top, RC.Right, RC.Bottom);

    OffsetRect(RW, -RW.Left, -RW.Top);
    OldHandle := Self.Canvas.Handle;
    try
      Self.Canvas.Handle := DC;
      ColorizerStyleServices.DrawElement(DC, ColorizerStyleServices.GetElementDetails(tmPopupBorders), Rect(0, 0, RW.Right, RW.Bottom));
    finally
      Self.Canvas.Handle := OldHandle;
    end;
  end
  else
   Trampoline_TThemedPopupMenu_NCPaint(Self, DC);
end;

procedure Detour_TCustomActionPopupMenu_CMVisibleChanged(Self : TCustomActionPopupMenuClass;var Message: TMessage);
var
  AStyle : NativeInt;
begin
  if TColorizerLocalSettings.Settings.Enabled and TColorizerLocalSettings.Settings.MenuTransparent and Self.Visible then
  begin
     AStyle := GetWindowLong(Self.Handle, GWL_EXSTYLE);
      if (AStyle and WS_EX_LAYERED) = 0 then
        SetWindowLong(Self.Handle, GWL_EXSTYLE, AStyle or WS_EX_LAYERED);
     SetLayeredWindowAttributes(Self.Handle, 0, TColorizerLocalSettings.Settings.MenuTransLevel, LWA_ALPHA);
  end;
  Trampoline_TCustomActionPopupMenu_CMVisibleChanged(Self, Message);
end;

procedure Detour_TCustomActionPopupMenu_DrawBackground(Self : TCustomActionPopupMenuClass);
begin
  if TColorizerLocalSettings.Settings.Enabled and TColorizerLocalSettings.Settings.UseVCLStyles and TColorizerLocalSettings.Settings.VCLStylesMenusColors then
  begin
    ColorizerStyleServices.DrawElement(Self.Canvas.Handle, ColorizerStyleServices.GetElementDetails(tmPopupBackground), Rect(0, 0, Self.Width, Self.Height))
  end
  else
   Trampoline_TCustomActionPopupMenu_DrawBackground(Self);
end;




procedure Detour_TThemedMenuButton_DoDrawText(Self: TThemedMenuButtonClass;const Text: string; var Rect: TRect; Flags: Longint);
const
  MenuStates: array[Boolean] of TThemedMenu = (tmMenuBarItemNormal, tmMenuBarItemHot);
var
  LCaption: string;
  LFormats: TTextFormat;
  LColor: TColor;
  LDetails: TThemedElementDetails;
begin
  if TColorizerLocalSettings.Settings.Enabled and TColorizerLocalSettings.Settings.UseVCLStyles and TColorizerLocalSettings.Settings.VCLStylesMenusColors then
  begin
    LFormats := TTextFormatFlags(Flags);
    if Self.Enabled then
      LDetails := ColorizerStyleServices.GetElementDetails(MenuStates[Self.Selected or Self.MouseInControl or Self.ActionBar.DesignMode])
    else
      LDetails := ColorizerStyleServices.GetElementDetails(tmMenuBarItemDisabled);

    Self.Canvas.Brush.Style := bsClear;
    if Self.Selected then
      Self.Canvas.Font.Color := clHighlightText
    else
      Self.Canvas.Font.Color := clMenuText;

    if not ColorizerStyleServices.GetElementColor(LDetails, ecTextColor, LColor) or (LColor = clNone) then
      LColor := Self.ActionBar.ColorMap.FontColor;

    LCaption := Text;
    if (tfCalcRect in LFormats) and ( (LCaption = '') or (LCaption[1] = cHotkeyPrefix) and (LCaption[2] = #0) ) then
      LCaption := LCaption + ' ';

    if Self.Enabled then
      LDetails := ColorizerStyleServices.GetElementDetails(MenuStates[Self.Selected or Self.MouseInControl]);
    //ColorizerStyleServices.DrawText(Self.Canvas.Handle, LDetails, LCaption, Rect, LFormats, LColor);
    InternalDrawText(Self.Canvas.Handle, LDetails, LCaption, Rect, LFormats, LColor);

  end
  else
   Trampoline_TThemedMenuButton_DoDrawText(Self, Text, Rect, Flags);
end;

procedure Detour_TThemedMenuButton_DrawBackground(Self: TThemedMenuButtonClass;var PaintRect: TRect);
const
  MenuStates: array[Boolean, Boolean] of TThemedMenu =
    ((tmMenuBarItemNormal, tmMenuBarItemPushed), (tmMenuBarItemHot, tmMenuBarItemPushed));
begin
  if TColorizerLocalSettings.Settings.Enabled and TColorizerLocalSettings.Settings.UseVCLStyles and TColorizerLocalSettings.Settings.VCLStylesMenusColors then
  begin
    Self.Canvas.Brush.Color := Self.ActionBar.ColorMap.Color;
    ColorizerStyleServices.DrawElement(Self.Canvas.Handle, ColorizerStyleServices.GetElementDetails(MenuStates[Self.MouseInControl, Self.Selected]), PaintRect);
  end
  else
   Trampoline_TThemedMenuButton_DrawBackground(Self, PaintRect);
end;

procedure Detour_TXPStylePopupMenu_NCPaint(Self : TXPStylePopupMenuClass;DC: HDC);
var
  RC, RW: TRect;
  OldHandle: THandle;
begin
  if TColorizerLocalSettings.Settings.Enabled and TColorizerLocalSettings.Settings.UseVCLStyles and TColorizerLocalSettings.Settings.VCLStylesMenusColors then
  begin
    Windows.GetClientRect(Self.Handle, RC);
    GetWindowRect(Self.Handle, RW);
    MapWindowPoints(0, Self.Handle, RW, 2);
    OffsetRect(RC, -RW.Left, -RW.Top);
    ExcludeClipRect(DC, RC.Left, RC.Top, RC.Right, RC.Bottom);

    OffsetRect(RW, -RW.Left, -RW.Top);
    OldHandle := Self.Canvas.Handle;
    try
      Self.Canvas.Handle := DC;
      ColorizerStyleServices.DrawElement(DC, ColorizerStyleServices.GetElementDetails(tmPopupBorders), Rect(0, 0, RW.Right, RW.Bottom));
    finally
      Self.Canvas.Handle := OldHandle;
    end;
  end
  else
   Trampoline_TXPStylePopupMenu_NCPaint(Self, DC);
end;


{ TThemedMenuItemHelper }

function TThemedMenuItemHelper.DoDrawMenuCheckAddress: Pointer;
var
  MethodAddr: procedure of object;
begin
  MethodAddr := Self.DoDrawMenuCheck;
  Result     := TMethod(MethodAddr).Code;
end;


function TThemedMenuItemHelper.DoDrawTextAddress: Pointer;
var
  MethodAddr: procedure(DC: HDC; const Text: string; var Rect: TRect; Flags: Longint) of object;
begin
  MethodAddr := Self.DoDrawText;
  Result     := TMethod(MethodAddr).Code;
end;


procedure TThemedMenuItemHelper.DoDrawTextHelper(DC: HDC; const Text: string;
  var Rect: TRect; Flags: Integer);
begin
 Self.DoDrawText(DC, Text, Rect, Flags);
end;

function TThemedMenuItemHelper.GetCheckRectHelper: TRect;
begin
 Result:= Self.FCheckRect;
end;

function TThemedMenuItemHelper.GetGutterRectHelper: TRect;
begin
 Result:=Self.FGutterRect;
end;

function TThemedMenuItemHelper.GetPaintRectHelper: TRect;
begin
 Result:=Self.FPaintRect;
end;

function TThemedMenuItemHelper.GetSeparatorHeightHelper: Integer;
begin
 Result:= Self.FSeparatorHeight;
end;

function TThemedMenuItemHelper.GetSubMenuGlyphRectHelper: TRect;
begin
 Result:=Self.FSubMenuGlyphRect;
end;

procedure TThemedMenuItemHelper.SetCheckRectHelper(const Value: TRect);
begin
 Self.FCheckRect:= Value;
end;

procedure TThemedMenuItemHelper.SetGutterRectHelper(const Value: TRect);
begin
 Self.FGutterRect:=Value;
end;

procedure TThemedMenuItemHelper.SetPaintRectHelper(const Value: TRect);
begin
 Self.FPaintRect:=Value;
end;

procedure TThemedMenuItemHelper.SetSeparatorHeightHelper(const Value: Integer);
begin
 Self.FSeparatorHeight:=Value;
end;

procedure TThemedMenuItemHelper.SetSubMenuGlyphRectHelper(const Value: TRect);
begin
 Self.FSubMenuGlyphRect:=Value;
end;

{ TThemedMenuButtonHelper }

function TThemedMenuButtonHelper.DoDrawTextAddress: Pointer;
var
  MethodAddr: procedure(const Text: string; var Rect: TRect; Flags: Integer) of object;
begin
  MethodAddr := Self.DoDrawText;
  Result     := TMethod(MethodAddr).Code;
end;

procedure InstallThemedActnCtrlsHooks;
begin
  Trampoline_TThemedMenuItem_DrawBackground   := InterceptCreate(@TThemedMenuItemClass.DrawBackground, @Detour_TThemedMenuItem_DrawBackground);
  Trampoline_TThemedMenuItem_DrawSeparator    := InterceptCreate(@TThemedMenuItemClass.DrawSeparator, @Detour_TThemedMenuItem_DrawSeparator);
  Trampoline_TThemedMenuItem_DrawSubMenuGlyph := InterceptCreate(@TThemedMenuItemClass.DrawSubMenuGlyph, @Detour_TThemedMenuItem_DrawSubMenuGlyph);
  Trampoline_TThemedMenuItem_DrawText         := InterceptCreate(@TThemedMenuItemClass.DrawText, @Detour_TThemedMenuItem_DrawText);
  Trampoline_TThemedMenuItem_DoDrawText       := InterceptCreate(TThemedMenuItem(nil).DoDrawTextAddress, @Detour_TThemedMenuItem_DoDrawText);
  Trampoline_TThemedMenuItem_DoDrawMenuCheck  := InterceptCreate(TThemedMenuItem(nil).DoDrawMenuCheckAddress, @Detour_TThemedMenuItem_DoDrawMenuCheck);
  Trampoline_TThemedPopupMenu_NCPaint         := InterceptCreate(@TThemedPopupMenuClass.NCPaint, @Detour_TThemedPopupMenu_NCPaint);

  Trampoline_TCustomActionPopupMenu_DrawBackground  := InterceptCreate(@TCustomActionPopupMenuClass.DrawBackground, @Detour_TCustomActionPopupMenu_DrawBackground);
  Trampoline_TCustomActionPopupMenu_CMVisibleChanged  := InterceptCreate(@TCustomActionPopupMenuClass.CMVisibleChanged, @Detour_TCustomActionPopupMenu_CMVisibleChanged);

  Trampoline_TThemedMenuButton_DoDrawText     := InterceptCreate(TThemedMenuButton(nil).DoDrawTextAddress, @Detour_TThemedMenuButton_DoDrawText);
  Trampoline_TThemedMenuButton_DrawBackground := InterceptCreate(@TThemedMenuButtonClass.DrawBackground, @Detour_TThemedMenuButton_DrawBackground);
  Trampoline_TXPStylePopupMenu_NCPaint        := InterceptCreate(@TXPStylePopupMenuClass.NCPaint, @Detour_TXPStylePopupMenu_NCPaint);
end;

procedure RemoveThemedActnCtrlsHooks;
begin
  InterceptRemove(@Trampoline_TThemedMenuItem_DrawBackground);
  InterceptRemove(@Trampoline_TThemedMenuItem_DrawSeparator);
  InterceptRemove(@Trampoline_TThemedMenuItem_DrawSubMenuGlyph);
  InterceptRemove(@Trampoline_TThemedMenuItem_DrawText);
  InterceptRemove(@Trampoline_TThemedMenuItem_DoDrawText);
  InterceptRemove(@Trampoline_TThemedMenuItem_DoDrawMenuCheck);
  InterceptRemove(@Trampoline_TThemedPopupMenu_NCPaint);
  InterceptRemove(@Trampoline_TCustomActionPopupMenu_DrawBackground);
  InterceptRemove(@Trampoline_TCustomActionPopupMenu_CMVisibleChanged);
  InterceptRemove(@Trampoline_TThemedMenuButton_DoDrawText);
  InterceptRemove(@Trampoline_TThemedMenuButton_DrawBackground);
  InterceptRemove(@Trampoline_TXPStylePopupMenu_NCPaint);
end;

end.
