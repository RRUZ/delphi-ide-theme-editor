//**************************************************************************************************
//
// Unit Colorizer.Utils
// unit Colorizer.Utils for the Delphi IDE Colorizer
//
// The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
// you may not use this file except in compliance with the License. You may obtain a copy of the
// License at http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
// ANY KIND, either express or implied. See the License for the specific language governing rights
// and limitations under the License.
//
// The Original Code is Colorizer.Utils.pas.
//
// The Initial Developer of the Original Code is Rodrigo Ruz V.
// Portions created by Rodrigo Ruz V. are Copyright (C) 2011-2014 Rodrigo Ruz V.
// All Rights Reserved.
//
//**************************************************************************************************

unit Colorizer.Utils;

interface

{$I ..\Common\Jedi.inc}

uses
 {$IFDEF DELPHIXE2_UP}
 VCL.Themes,
 VCL.Styles,
 {$ENDIF}
 Classes,
 {$IFDEF DELPHI2009_UP}
 Generics.Collections,
 {$ENDIF}
 ActnMan,
 uDelphiVersions,
 ActnColorMaps,
 Windows,
 PngImage,
 Graphics,
 Colorizer.Settings,
 ColorXPStyleActnCtrls;

{.$DEFINE ENABLELOG}

procedure AddLog(const msg : string);

procedure RefreshIDETheme(AColorMap:TCustomActionBarColorMap;AStyle: TActionBarStyle;Restore : Boolean = False;Invalidate : Boolean = False); overload;
procedure RefreshIDETheme(Invalidate : Boolean = False); overload;
procedure RestoreIDESettings();

procedure LoadSettings(AColorMap:TCustomActionBarColorMap;ActionBarStyle : TActionBarStyle;Settings : TSettings);
procedure ProcessComponent(AColorMap:TCustomActionBarColorMap;AStyle: TActionBarStyle;AComponent: TComponent;Restore : Boolean = False; Invalidate : Boolean = False);
procedure GenerateColorMap(AColorMap:TCustomActionBarColorMap;Color, FontColor:TColor);{$IF CompilerVersion >= 23}overload;{$IFEND}
{$IFDEF DELPHIXE2_UP}
procedure GenerateColorMap(AColorMap:TCustomActionBarColorMap;Style:TCustomStyleServices);overload;
procedure RegisterVClStylesFiles;
{$ENDIF}


 type
   TColorizerLocalSettings = class
   public
      {$IFDEF DELPHI2009_UP}
      class var ActnStyleList : TList<TActionManager>;
      {$ENDIF}
      class var ColorMap       : TCustomActionBarColorMap;
      class var ActionBarStyle : TActionBarStyle;
      class var HookedWindows  : TStringList;
      class var VCLStylesPath  : string;
      class var Settings       : TSettings;
      class var ImagesGutterChanged : Boolean;
      class var IDEData        : TDelphiVersionData;
      class var DockImages     : TPngImage;
    end;

implementation

{.$DEFINE DEBUG_MODE}

uses
 {$IFDEF DELPHIXE_UP}
 PlatformDefaultStyleActnCtrls,
 {$ELSE}
 XPStyleActnCtrls,
 {$ENDIF}
 {$IFDEF DELPHI2010_UP}
 IOUtils,
 Rtti,
 {$ENDIF}
 Types,
 Forms,
 SysUtils,
 Controls,
 GraphUtil,
 Colorizer.StoreColorMap,
 Colorizer.Wrappers,
 Dialogs,
 uMisc,
 uRttiHelper;


{$IFDEF DELPHIXE2_UP}
procedure RegisterVClStylesFiles;
var
 sPath, FileName : string;
begin
  sPath:=TColorizerLocalSettings.VCLStylesPath;
  if SysUtils.DirectoryExists(sPath) then
  for FileName in TDirectory.GetFiles(sPath, '*.vsf') do
   if TStyleManager.IsValidStyle(FileName) then
    begin
       try
         TStyleManager.LoadFromFile(FileName);
       except
         on EDuplicateStyleException do
       end;
    end;
end;
{$ENDIF}

procedure AddLog(const msg : string);
begin
{$IFDEF ENABLELOG}
   TFile.AppendAllText('C:\Delphi\google-code\DITE\delphi-ide-theme-editor\IDE PlugIn\log.txt',Format('%s %s %s',[FormatDateTime('hh:nn:ss.zzz', Now),  msg, sLineBreak]));
{$ENDIF}
end;

{$IFDEF DEBUG_PROFILER}
//DumpType('GDIPlus.GradientDrawer.TGradientTabDrawer');
procedure DumpType(const QualifiedName:string);
var
  l2 : TStrings;
begin
  l2 := TStringList.Create;
  try
    l2.Text:=DumpTypeDefinition(TRttiContext.Create.FindType(QualifiedName).Handle);
    l2.SaveToFile(ExtractFilePath(GetBplLocation())+'Galileo\'+QualifiedName+'.pas');
  finally
   l2.Free;
  end;
end;

procedure DumpAllTypes;
var
  l2 : TStrings;
  t  : TRttiType;
begin
  l2 := TStringList.Create;
  try
    for t in TRttiContext.Create.GetTypes do
    if t.IsInstance then
     l2.Add(t.AsInstance.DeclaringUnitName +' '+t.Name);
   l2.SaveToFile(ExtractFilePath(GetBplLocation())+'Galileo\Types.txt');
  finally
   l2.Free;
  end;
end;

procedure DumpObject(AObject: TObject);
var
 LDumpInfo : TStrings;
begin
   LDumpInfo := TStringList.Create;
  try
   LDumpInfo.Text:=DumpTypeDefinition(AObject.ClassInfo);
   LDumpInfo.SaveToFile(ExtractFilePath(GetModuleLocation())+'Galileo\'+AObject.ClassName+'.pas');
  finally
   LDumpInfo.Free;
  end;
end;
{$ENDIF}



procedure RefreshIDETheme(Invalidate : Boolean = False);
begin
   RefreshIDETheme(TColorizerLocalSettings.ColorMap, TColorizerLocalSettings.ActionBarStyle, False, Invalidate);
end;

procedure RefreshIDETheme(AColorMap:TCustomActionBarColorMap;AStyle: TActionBarStyle;Restore : Boolean = False; Invalidate : Boolean = False);
var
  Index     : Integer;
begin
 {
  if GlobalSettings.EnableDWMColorization and DwmIsEnabled then
   SetCompositionColor(AColorMap.Color);
 }
  for Index := 0 to Screen.FormCount-1 do
  if TColorizerLocalSettings.HookedWindows.IndexOf(Screen.Forms[Index].ClassName)<>-1 then
  begin
   if not (csDesigning in Screen.Forms[Index].ComponentState) then
     ProcessComponent(AColorMap, AStyle, Screen.Forms[Index], Restore, Invalidate);
  end
//  {$IFDEF DELPHIXE2_UP}
//  else
//  if (TColorizerLocalSettings.Settings<>nil) and (TColorizerLocalSettings.Settings.UseVCLStyles) and (csDesigning in Screen.Forms[index].ComponentState) then
//    ApplyEmptyVCLStyleHook(Screen.Forms[index].ClassType);
//  {$ENDIF}
end;


procedure LoadSettings(AColorMap:TCustomActionBarColorMap;ActionBarStyle : TActionBarStyle;Settings : TSettings);
Var
 ThemeFileName : string;
begin
  if Settings=nil then exit;
  ReadSettings(Settings, ExtractFilePath(GetModuleLocation()));
  ThemeFileName:=IncludeTrailingPathDelimiter(ExtractFilePath(GetModuleLocation()))+'Themes\'+Settings.ThemeName+'.idetheme';
  if FileExists(ThemeFileName) then
   LoadColorMapFromXmlFile(AColorMap, ThemeFileName);

//  if ActionBarStyles.IndexOf(Settings.StyleBarName)>=0 then
//    ActionBarStyle:= TActionBarStyle(ActionBarStyles.Objects[ActionBarStyles.IndexOf(Settings.StyleBarName)]);
end;


procedure GenerateColorMap(AColorMap:TCustomActionBarColorMap;Color, FontColor:TColor);
begin
  AColorMap.Color                 :=Color;
  AColorMap.ShadowColor           :=GetShadowColor(Color);
  AColorMap.FontColor             :=FontColor;
  AColorMap.MenuColor             :=GetHighLightColor(Color);
  AColorMap.HighlightColor        :=GetHighLightColor(AColorMap.MenuColor);
  AColorMap.BtnSelectedColor      :=GetHighLightColor(AColorMap.MenuColor);
  AColorMap.BtnSelectedFont       :=AColorMap.FontColor;

  AColorMap.SelectedColor         :=GetHighLightColor(Color, 50);
  AColorMap.SelectedFontColor     :=AColorMap.FontColor;

  AColorMap.BtnFrameColor         :=GetShadowColor(Color);
  AColorMap.FrameTopLeftInner     :=GetShadowColor(Color);
  AColorMap.FrameTopLeftOuter     :=AColorMap.FrameTopLeftInner;
  AColorMap.FrameBottomRightInner :=AColorMap.FrameTopLeftInner;
  AColorMap.FrameBottomRightOuter :=AColorMap.FrameTopLeftInner;
end;

{$IFDEF DELPHIXE2_UP}
procedure GenerateColorMap(AColorMap:TCustomActionBarColorMap;Style:TCustomStyleServices);
begin
  AColorMap.Color                 :=Style.GetStyleColor(scPanel);
  AColorMap.ShadowColor           :=StyleServices.GetSystemColor(clBtnShadow);
  AColorMap.FontColor             :=Style.GetStyleFontColor(sfButtonTextNormal);

  AColorMap.MenuColor             :=Style.GetStyleColor(scWindow);
  AColorMap.HighlightColor        :=StyleServices.GetSystemColor(clHighlight);
  AColorMap.BtnSelectedColor      :=Style.GetStyleColor(scButtonHot);

  AColorMap.BtnSelectedFont       :=StyleServices.GetSystemColor(clHighlightText);

  AColorMap.SelectedColor         :=StyleServices.GetSystemColor(clHighlight);
  AColorMap.SelectedFontColor     :=StyleServices.GetSystemColor(clHighlightText);

  AColorMap.BtnFrameColor         :=StyleServices.GetSystemColor(clBtnShadow);

  AColorMap.FrameTopLeftInner     :=StyleServices.GetSystemColor(clBtnShadow);
  AColorMap.FrameTopLeftOuter     :=AColorMap.FrameTopLeftInner;
  AColorMap.FrameBottomRightInner :=AColorMap.FrameTopLeftInner;
  AColorMap.FrameBottomRightOuter :=AColorMap.FrameTopLeftInner;
end;
{$ENDIF}

procedure ProcessComponent(AColorMap:TCustomActionBarColorMap;AStyle: TActionBarStyle;AComponent: TComponent;Restore : Boolean = False; Invalidate: Boolean = False);
var
  Index          : Integer;
  LActionManager : TActionManager;
  LForm          : TForm;
begin
    if not Assigned(AComponent) or not Assigned(AColorMap) then  exit;

    if AComponent is TForm then
    begin
      LForm:=TForm(AComponent);
      LForm.Color := AColorMap.Color;
      LForm.Font.Color:=AColorMap.FontColor;
      if Invalidate then
        LForm.Invalidate;
    end
    else
    if AComponent is TActionManager then
    begin
      LActionManager:=TActionManager(AComponent);
      {$IFDEF DELPHI2009_UP}
//      if not ActnStyleList.ContainsKey(LActionManager) then
//          ActnStyleList.Add(LActionManager, LActionManager.Style);
      if TColorizerLocalSettings.ActnStyleList.IndexOf(LActionManager)=-1 then
          TColorizerLocalSettings.ActnStyleList.Add(LActionManager);
      {$ENDIF}
      LActionManager.Style := AStyle;
    end
    else
    if AComponent is TFrame then
    with TFrame(AComponent) do
    begin
      Color := AColorMap.Color;
      Font.Color:=AColorMap.FontColor;
    end;

    RunWrapper(AComponent, AColorMap, Invalidate);

    //process components
    for Index := 0 to AComponent.ComponentCount - 1 do
     ProcessComponent(AColorMap, ColorXPStyle, AComponent.Components[Index], Restore);

    //process dock clients
    if AComponent is TWinControl then
     for Index := 0 to TWinControl(AComponent).DockClientCount - 1 do
     if TWinControl(AComponent).DockClients[Index].Visible and (TColorizerLocalSettings.HookedWindows.IndexOf(TWinControl(AComponent).DockClients[Index].ClassName)>=0) then
     begin
       //AddLog('DockClients '+TWinControl(AComponent).DockClients[Index].ClassName);
       ProcessComponent(AColorMap, ColorXPStyle, TWinControl(AComponent).DockClients[Index]);
       if Invalidate and  (TWinControl(AComponent).DockClients[Index] is TForm) then
        TWinControl(AComponent).DockClients[Index].Invalidate();
     end;
end;

procedure RestoreActnManagerStyles;
{$IFNDEF DLLWIZARD}
var
  LActionManager : TActionManager;
{$ENDIF}
begin
{$IFNDEF DLLWIZARD}
 {$IFDEF DELPHI2009_UP}
  try
    if (TColorizerLocalSettings.ActnStyleList.Count>0)  and Assigned(ActionBarStyles) then
    begin
      for LActionManager in TColorizerLocalSettings.ActnStyleList{.Keys} do
      begin
         //LActionManager.Style:= ActnStyleList.Items[LActionManager];//ActionBarStyles.Style[ActionBarStyles.IndexOf(DefaultActnBarStyle)];
        if ActionBarStyles.IndexOf(DefaultActnBarStyle)>=0 then
        begin
         if Assigned(LActionManager.Style) and Assigned(ActionBarStyles.Style[ActionBarStyles.IndexOf(DefaultActnBarStyle)]) then
         begin
           //AddLog('ActionBarStyles '+ActionBarStyles.Style[ActionBarStyles.IndexOf(DefaultActnBarStyle)].GetStyleName);
           LActionManager.Style:= ActionBarStyles.Style[ActionBarStyles.IndexOf(DefaultActnBarStyle)];
         end;
        end;
      end;
    end;
  except on e: exception do //sometimes the references to the objects contained in ActionBarStyles are lost when the IDE is closed.
    AddLog(Format(' LActionManager.Style exception RestoreActnManagerStyles Message %s Trace %s ',[e.Message, e.StackTrace]));
  end;
 {$ELSE DELPHI2009_UP}
   //TODO
 {$ENDIF}

{$ENDIF}
end;

procedure RestoreIDESettings();
var
 NativeColorMap : TCustomActionBarColorMap;
begin
{$IFDEF DELPHIXE2_UP}
  if TColorizerLocalSettings.Settings.UseVCLStyles then
    if not TStyleManager.ActiveStyle.IsSystemStyle  then
     TStyleManager.SetStyle('Windows');
{$ENDIF}

{$IFDEF DELPHIXE_UP}
  NativeColorMap:=TThemedColorMap.Create(nil);
{$ELSE}
  NativeColorMap:=TStandardColorMap.Create(nil);
{$ENDIF}

  try
  {$IFDEF DELPHIXE_UP}
    RefreshIDETheme(NativeColorMap, PlatformDefaultStyle, True);
  {$ELSE}
    RefreshIDETheme(NativeColorMap, XPStyle, True);
  {$ENDIF}
  finally
    NativeColorMap.Free;
  end;
  RestoreActnManagerStyles();
end;

initialization

{$IFDEF ENABLELOG}
 ShowMessage('Log enabled');
{$ENDIF}


end.
