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
 Colorizer.Vcl.Styles,
 {$ENDIF}
 Classes,
 {$IFDEF DELPHI2009_UP}
 Generics.Collections,
 {$ENDIF}
 ActnMan,
 ComCtrls,
 uDelphiVersions,
 ActnColorMaps,
 Windows,
 PngImage,
 Graphics,
 Colorizer.Settings,
 Colorizer.XPStyleActnCtrls;

{.$DEFINE ENABLELOG}

procedure AddLog(const Message : string); overload;
procedure AddLog(const Category, Message : string); overload;

procedure RefreshIDETheme(AColorMap:TColorizerColorMap;AStyle: TActionBarStyle;Restore : Boolean = False;Invalidate : Boolean = False); overload;
procedure RefreshIDETheme(Invalidate : Boolean = False); overload;
procedure RestoreIDESettings();
procedure RestoreIDESettingsFast();

procedure LoadSettings(AColorMap:TColorizerColorMap; Settings : TSettings);
procedure ProcessComponent(AColorMap:TColorizerColorMap;AStyle: TActionBarStyle;AComponent: TComponent;Restore : Boolean = False; Invalidate : Boolean = False);
procedure GenerateColorMap(AColorMap:TColorizerColorMap;Color, FontColor:TColor);{$IF CompilerVersion >= 23}overload;{$IFEND}
{$IFDEF DELPHIXE2_UP}
procedure AssignColorsFromVCLStyle(AColorMap:TColorizerColorMap;Style:TCustomStyleServices);
procedure RegisterVClStylesFiles;
{$ENDIF}


 type
   TColorizerLocalSettings = class
   public
      {$IFDEF DELPHI2009_UP}
      class var ActnStyleList : TList<TActionManager>;
      {$ENDIF}
      class var ColorMap       : TColorizerColorMap;
      class var ActionBarStyle : TActionBarStyle;
      class var HookedWindows     : TStringList;
      class var HookedScrollBars  : TStringList;
      class var WinAPIClasses     : TStringList;
      class var HookedWindowsText    : string;
      class var HookedScrollBarsText  : string;
      class var VCLStylesPath  : string;
      class var Settings       : TSettings;
      class var ImagesGutterChanged : Boolean;
      class var IDEData        : TDelphiVersionData;
      class var DockImages     : TPngImage;
      class var Unloading : Boolean;
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
 Rtti,
 {$ENDIF}
 Types,
 IOUtils,
{$IFDEF ENABLELOG}
{$ENDIF}
 Forms,
 SysUtils,
 Controls,
 GraphUtil,
 Colorizer.StoreColorMap,
 Colorizer.Wrappers,
 Dialogs,
 uMisc,
 uRttiHelper;

{$IFDEF ENABLELOG}
var
  LogFile : TStrings = nil;
{$ENDIF}

//var
//  LFieldsComponents : TObjectDictionary<string,TStringList>;

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


procedure AddLog(const Message : string);
begin
  AddLog('', Message);
end;


procedure RefreshIDETheme(Invalidate : Boolean = False);
begin
   RefreshIDETheme(TColorizerLocalSettings.ColorMap, TColorizerLocalSettings.ActionBarStyle, False, Invalidate);
end;

procedure RestoreIDESettingsFast();
var
  i, j  : Integer;
  LComponent : TComponent;
  NativeColorMap : TColorizerColorMap;
{$IFDEF DELPHIXE_UP}
  LThemedColorMap : TThemedColorMap;
{$ELSE}
  LThemedColorMap: TStandardColorMap;
{$ENDIF}
begin
  NativeColorMap:=TColorizerColorMap.Create(nil);
  try
{$IFDEF DELPHIXE_UP}
  LThemedColorMap:=TThemedColorMap.Create(nil);
{$ELSE}
  LThemedColorMap:=TStandardColorMap.Create(nil);
{$ENDIF}
  NativeColorMap.Assign(LThemedColorMap);
  NativeColorMap.WindowColor:=clWindow;
  for i := 0 to Screen.FormCount-1 do
   if SameText(Screen.Forms[i].ClassName, 'TAppBuilder') then
     for j := 0 to Screen.Forms[i].ComponentCount-1 do
      begin
       LComponent:= Screen.Forms[i].Components[j];
       if LComponent is TToolBar then
         RunWrapper(LComponent, NativeColorMap, False, True);
      end;

  finally
    NativeColorMap.Free;
  end;
end;

procedure RefreshIDETheme(AColorMap:TColorizerColorMap;AStyle: TActionBarStyle;Restore : Boolean = False; Invalidate : Boolean = False);
var
  Index     : Integer;
begin
 {
  if GlobalSettings.EnableDWMColorization and DwmIsEnabled then
   SetCompositionColor(AColorMap.Color);
 }
  for Index := 0 to Screen.FormCount-1 do
  if TColorizerLocalSettings.HookedWindows.IndexOf(Screen.Forms[Index].ClassName)>=0 then
   if not (csDesigning in Screen.Forms[Index].ComponentState) then
   begin
     //AddLog('RefreshIDETheme', 'Restore = '+BoolToStr(Restore, True));
     //AddLog('RefreshIDETheme', Screen.Forms[Index].ClassName);
     ProcessComponent(AColorMap, AStyle, Screen.Forms[Index], Restore, Invalidate);
   end;
//  {$IFDEF DELPHIXE2_UP}
//  else
//  if (TColorizerLocalSettings.Settings<>nil) and (TColorizerLocalSettings.Settings.UseVCLStyles) and (csDesigning in Screen.Forms[index].ComponentState) then
//    ApplyEmptyVCLStyleHook(Screen.Forms[index].ClassType);
//  {$ENDIF}
end;


procedure LoadSettings(AColorMap:TColorizerColorMap; Settings : TSettings);
begin
  if Settings=nil then exit;
  ReadSettings(Settings, ExtractFilePath(GetModuleLocation()));

  if FileExists(Settings.ThemeFileName) then
   LoadColorMapFromXmlFile(AColorMap, Settings.ThemeFileName);

 {$IFDEF DELPHIXE2_UP}
  if Settings.UseVCLStyles and Settings.VCLStylesMenusColors then
    AssignColorsFromVCLStyle(AColorMap, ColorizerStyleServices);
{$ENDIF}

//  if ActionBarStyles.IndexOf(Settings.StyleBarName)>=0 then
//    ActionBarStyle:= TActionBarStyle(ActionBarStyles.Objects[ActionBarStyles.IndexOf(Settings.StyleBarName)]);
end;


procedure GenerateColorMap(AColorMap:TColorizerColorMap;Color, FontColor:TColor);
begin
  AColorMap.Color                 :=Color;
  AColorMap.ShadowColor           :=GetShadowColor(Color);
  AColorMap.FontColor             :=FontColor;
  AColorMap.MenuColor             :=GetHighLightColor(Color);
  AColorMap.WindowColor           :=AColorMap.MenuColor;
  AColorMap.HighlightColor        :=GetHighLightColor(AColorMap.MenuColor);
  AColorMap.HotColor              :=GetHighLightColor(AColorMap.MenuColor);
  AColorMap.BtnSelectedColor      :=AColorMap.HotColor;
  AColorMap.BtnSelectedFont       :=AColorMap.FontColor;

  AColorMap.SelectedColor         :=GetHighLightColor(Color, 50);
  AColorMap.SelectedFontColor     :=AColorMap.FontColor;
  AColorMap.HotFontColor          :=AColorMap.FontColor;

  AColorMap.BtnFrameColor         :=GetShadowColor(Color);
  AColorMap.FrameTopLeftInner     :=GetShadowColor(Color);
  AColorMap.FrameTopLeftOuter     :=AColorMap.FrameTopLeftInner;
  AColorMap.FrameBottomRightInner :=AColorMap.FrameTopLeftInner;
  AColorMap.FrameBottomRightOuter :=AColorMap.FrameTopLeftInner;
end;

{$IFDEF DELPHIXE2_UP}
procedure AssignColorsFromVCLStyle(AColorMap:TColorizerColorMap;Style:TCustomStyleServices);
var
  LDetails        : TThemedElementDetails;
  ThemeTextColor  : TColor;
begin
  AColorMap.Color                 :=Style.GetStyleColor(scPanel);
  AColorMap.ShadowColor           :=Style.GetStyleColor(scBorder);

  LDetails := Style.GetElementDetails(tmMenuBarItemNormal);
  Style.GetElementColor(LDetails, ecTextColor, ThemeTextColor);

  AColorMap.FontColor             :=ThemeTextColor;//Style.GetStyleFontColor(sfButtonTextNormal);

  AColorMap.MenuColor             :=Style.GetStyleColor(scPanel);
  AColorMap.WindowColor           :=Style.GetStyleColor(scWindow);

  AColorMap.HighlightColor        :=Style.GetStyleColor(scButtonHot);
  AColorMap.HotColor              :=Style.GetStyleColor(scButtonHot);
  AColorMap.BtnSelectedColor      :=Style.GetStyleColor(scButtonHot);
  AColorMap.SelectedColor         :=Style.GetStyleColor(scButtonHot);

  AColorMap.BtnSelectedFont       :=Style.GetStyleFontColor(sfButtonTextHot);
  AColorMap.SelectedFontColor     :=Style.GetStyleFontColor(sfButtonTextHot);

  AColorMap.BtnFrameColor         :=StyleServices.GetSystemColor(clBtnShadow);

  AColorMap.FrameTopLeftInner     :=Style.GetStyleColor(scBorder);
  AColorMap.FrameTopLeftOuter     :=AColorMap.FrameTopLeftInner;
  AColorMap.FrameBottomRightInner :=AColorMap.FrameTopLeftInner;
  AColorMap.FrameBottomRightOuter :=AColorMap.FrameTopLeftInner;
end;
{$ENDIF}

procedure ProcessComponent(AColorMap:TColorizerColorMap;AStyle: TActionBarStyle;AComponent: TComponent;Restore : Boolean = False; Invalidate: Boolean = False);
var
  Index          : Integer;
  LActionManager : TActionManager;
//  LStrings       : TStringList;
  LForm          : TForm;
//  s              : string;
//  ctx            : TRttiContext;
//  LField         : TRttiField;
//  found          : Boolean;
begin
    if not Assigned(AComponent) or not Assigned(AColorMap) then  exit;

//    if SameText(AComponent.ClassName, 'TMessageHintWindow') then
//     TRttiUtils.DumpObject(AComponent, 'C:\Delphi\google-code\DITE\delphi-ide-theme-editor\IDE PlugIn\Galileo\'+AComponent.ClassName+'.pas');

    if AComponent is TForm then
    begin
      LForm:=TForm(AComponent);
      LForm.Color := AColorMap.Color;
      LForm.Font.Color:=AColorMap.FontColor;
      //HideSeparators(LForm, 'TProjectManagerForm', 'ToolBar');

//      if SameText('TIDEInsightForm', AComponent.ClassName) then
//       TRttiUtils.DumpObject(AComponent, 'C:\Delphi\google-code\DITE\delphi-ide-theme-editor\IDE PlugIn\Galileo\'+AComponent.ClassName+'_XE4.pas');



      //process field TComponent no registered in the components list
//      ctx:=TRttiContext.Create;
//      try
//        if LFieldsComponents.ContainsKey(LForm.ClassName) then
//        begin
//          for s in LFieldsComponents.Items[LForm.ClassName] do
//          begin
//            LField:=ctx.GetType(LForm.ClassInfo).GetField(s);
//            if (LField.GetValue(LForm).AsObject<>nil) then
//             RunWrapper(TComponent(LField.GetValue(LForm).AsObject), AColorMap, Invalidate);
//          end;
//        end
//        else
//        begin
//
//          LStrings:= TStringList.Create;
//          LFieldsComponents.Add(LForm.ClassName, LStrings);
//          for LField in ctx.GetType(LForm.ClassInfo).GetFields() do
//             if LField.FieldType.IsInstance and (LField.GetValue(LForm).AsObject<>nil) and (LField.GetValue(LForm).AsObject is TComponent) and (TRegisteredWrappers.Wrappers.ContainsKey(LField.GetValue(LForm).AsObject.ClassName)) then
//             begin
//               found:=false;
//               for Index := 0 to LForm.ComponentCount - 1 do
//                 if SameText(LForm.Components[Index].Name, LField.Name) then
//                 begin
//                   found:=True;
//                   break;
//                 end;
//
//               if not found then
//                 LStrings.Add(LField.Name);
//             end;
//
//          for s in LStrings do
//            RunWrapper(TComponent(TRttiUtils.GetRttiFieldValue(LForm, s).AsObject), AColorMap, Invalidate);
//
////           if LStrings.Count>0 then
////            ShowMessage(LStrings.Text);
//        end;
//      finally
//        ctx.Free;
//      end;


      {
      if SameText(LForm.ClassName, 'TToolForm') then
        TRttiUtils.DumpObject(TRttiUtils.GetRttiFieldValue(LForm, 'FCategoriesPopup').AsObject,'C:\Delphi\google-code\DITE\delphi-ide-theme-editor\IDE PlugIn\Galileo\TCategoriesPopup.pas');
      }

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

    //if SameText('TXTabControl', AComponent.ClassName) then
     //TRttiUtils.DumpObject(AComponent, 'C:\Delphi\google-code\DITE\delphi-ide-theme-editor\IDE PlugIn\Galileo\'+AComponent.ClassName+'.pas');


    RunWrapper(AComponent, AColorMap, Invalidate, Restore);

    //process components
    for Index := 0 to AComponent.ComponentCount - 1 do
     ProcessComponent(AColorMap, AStyle, AComponent.Components[Index], Restore);

    //process dock clients
    if AComponent is TWinControl then
     for Index := 0 to TWinControl(AComponent).DockClientCount - 1 do
     if TWinControl(AComponent).DockClients[Index].Visible and (TColorizerLocalSettings.HookedWindows.IndexOf(TWinControl(AComponent).DockClients[Index].ClassName)>=0) then
     begin
       //AddLog('DockClients '+TWinControl(AComponent).DockClients[Index].ClassName);
       ProcessComponent(AColorMap, AStyle, TWinControl(AComponent).DockClients[Index]);
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
 NativeColorMap : TColorizerColorMap;
{$IFDEF DELPHIXE_UP}
 LThemedColorMap : TThemedColorMap;
{$ELSE}
 LThemedColorMap: TStandardColorMap;
{$ENDIF}
begin
{$IFDEF DELPHIXE2_UP}
  if TColorizerLocalSettings.Settings.UseVCLStyles then
    if not TStyleManager.ActiveStyle.IsSystemStyle  then
     TStyleManager.SetStyle('Windows');
{$ENDIF}
  NativeColorMap := TColorizerColorMap.Create(nil);
  try

{$IFDEF DELPHIXE_UP}
  LThemedColorMap  := TThemedColorMap.Create(nil);
  try
    NativeColorMap.Assign(LThemedColorMap);
  finally
   LThemedColorMap.Free;
  end;
{$ELSE}
  LThemedColorMap  := TStandardColorMap.Create(nil);
  try
    NativeColorMap.Assign(LThemedColorMap);
  finally
   LThemedColorMap.Free;
  end;
{$ENDIF}

    NativeColorMap.WindowColor:=clWindow;
  {$IFDEF DELPHIXE_UP}
    RefreshIDETheme(NativeColorMap, PlatformDefaultStyle, True, False);
  {$ELSE}
    RefreshIDETheme(NativeColorMap, XPStyle, True, False);
  {$ENDIF}
  finally
    NativeColorMap.Free;
  end;
  //RestoreActnManagerStyles();
end;

{$IFDEF ENABLELOG}
var
  sLogFileName : string;
{$ENDIF}


procedure AddLog(const Category, Message : string);
begin
{$IFDEF ENABLELOG}
TFile.AppendAllText(sLogFileName, Format('%s %s : %s %s',[FormatDateTime('hh:nn:ss.zzz', Now), Category, Message, sLineBreak]));
//   if not Assigned(LogFile) then exit;
//
//   if Category<>'' then
//    LogFile.Add(Format('%s : %s', [Category, Message]))
//   else
//    LogFile.Add(Format('%s', [Message]));
//
// if (LogFile.Count mod 10) = 0 then
// begin
//  TFile.AppendAllText(sLogFileName, LogFile.Text);
//  LogFile.Clear;
// end;
{$ENDIF}
end;

initialization

{$IFDEF ENABLELOG}
 sLogFileName:=ExtractFilePath(GetModuleLocation())+'log.txt';
 LogFile:=TStringList.Create;
 ShowMessage('Log enabled');
{$ENDIF}
 //LFieldsComponents := TObjectDictionary<string,TStringList>.Create([doOwnsValues]);
finalization
{$IFDEF ENABLELOG}
  //LogFile.SaveToFile(sLogFileName);
  LogFile.Free;
{$ENDIF}
 //LFieldsComponents.Free;
end.
