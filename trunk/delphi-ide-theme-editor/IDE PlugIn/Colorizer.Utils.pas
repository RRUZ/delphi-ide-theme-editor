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
// Portions created by Rodrigo Ruz V. are Copyright (C) 2011-2015 Rodrigo Ruz V.
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
 StdCtrls,
 uDelphiVersions,
 uDelphiIDEHighlight,
 ActnColorMaps,
 StrUtils,
 Windows,
 PngImage,
 Graphics,
 Colorizer.Settings,
 Colorizer.XPStyleActnCtrls;

{.$DEFINE ENABLELOG}

procedure AddLog2(const Message : string); overload;
procedure AddLog2(const Category, Message : string); overload;

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
        class var FActnStyleList : TList<TActionManager>;
        class var FColorMap       : TColorizerColorMap;
        class var FActionBarStyle : TActionBarStyle;
        class var FHookedWindows     : TStringList;
        class var FHookedScrollBars  : TStringList;
        class var FWinAPIClasses     : TStringList;
        class var FHookedWindowsText    : string;
        class var FHookedScrollBarsText  : string;
        class var FVCLStylesPath  : string;
        class var FSettings       : TSettings;
        class var FImagesGutterChanged : Boolean;
        class var FIDEData        : TDelphiVersionData;
        class var FDockImages     : TPngImage;
        class var FUnloading : Boolean;
        class var FModernTheme: TModernTheme;
     public
        class property ActnStyleList : TList<TActionManager> read  FActnStyleList write FActnStyleList;
        class property ColorMap       : TColorizerColorMap read  FColorMap write FColorMap;
        class property ActionBarStyle : TActionBarStyle read  FActionBarStyle write FActionBarStyle;
        class property HookedWindows     : TStringList read  FHookedWindows write FHookedWindows;
        class property HookedScrollBars  : TStringList read  FHookedScrollBars write FHookedScrollBars;
        class property WinAPIClasses     : TStringList read  FWinAPIClasses write FWinAPIClasses;
        class property HookedWindowsText    : string read  FHookedWindowsText write FHookedWindowsText;
        class property HookedScrollBarsText  : string read  FHookedScrollBarsText write FHookedScrollBarsText;
        class property VCLStylesPath  : string read  FVCLStylesPath write FVCLStylesPath;
        class property Settings       : TSettings read  FSettings write FSettings;
        class property ImagesGutterChanged : Boolean read  FImagesGutterChanged write FImagesGutterChanged;
        class property IDEData        : TDelphiVersionData read  FIDEData write FIDEData;
        class property ModernTheme : TModernTheme  read FModernTheme write FModernTheme;

        class property DockImages     : TPngImage read  FDockImages write FDockImages;
        class property Unloading : Boolean read  FUnloading write FUnloading;
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
 TypInfo,
 {$ENDIF}
 Types,
 IOUtils,
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
{$ENDIF}

//var
//  LFieldsComponents : TObjectDictionary<string,TStringList>;

{$IFDEF DELPHIXE2_UP}
procedure RegisterVClStylesFiles;
var
 sPath, FileName : string;
 LFiles  : TStringDynArray;
begin
  sPath:=TColorizerLocalSettings.VCLStylesPath;
  if SysUtils.DirectoryExists(sPath)  then
    LFiles := TDirectory.GetFiles(sPath, '*.vsf');

  //Appmethod doesn't include VCL Styles files, so we need read the styles from the installation folder of the plugin
  if not SysUtils.DirectoryExists(sPath) or (Length(LFiles)=0)  then
  begin
    sPath:=ExtractFilePath(GetModuleLocation())+'Styles\';
    if SysUtils.DirectoryExists(sPath)  then
      LFiles := TDirectory.GetFiles(sPath, '*.vsf');
  end;

    if Length(LFiles)>0 then
    for FileName in LFiles do
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


procedure AddLog2(const Message : string);
begin
  AddLog2('', Message);
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
// p : Pointer;
begin
    if not Assigned(AComponent) or not Assigned(AColorMap) then  exit;

    //AddLog2('ProcessComponent '+AComponent.Name);

    //cbDeviceSelector
//    if SameText(AComponent.Name, 'cbDeviceSelector') and TComboBox(AComponent).im then
//    begin
//      //TRttiUtils.DumpObject(AComponent, 'C:\Dephi\google-code\delphi-ide-theme-editor\IDE PlugIn\Galileo\cbDeviceSelector.pas');
//
//    end;


//    if SameText(AComponent.ClassName, 'TCastaliaNavToolbar') then
//     TRttiUtils.DumpObject(AComponent, 'C:\Dephi\google-code\delphi-ide-theme-editor\IDE PlugIn\Galileo\'+AComponent.ClassName+'.pas')
//    else
//    if SameText(AComponent.ClassName, 'TCastaliaNavToolbarDropdown') then
//     TRttiUtils.DumpObject(AComponent, 'C:\Dephi\google-code\delphi-ide-theme-editor\IDE PlugIn\Galileo\'+AComponent.ClassName+'.pas');


//    if SameText(AComponent.ClassName, 'TListBox') and SameText(AComponent.Name, 'ResultsList') then
//    begin
//       AddLog2(Format('Name %s',[AComponent.Name]));
//       s:= GetPropValue(TListBox(AComponent), 'Style');
//       AddLog2(Format('%s Value %s',['Style', s]));
//
////       p:=  @(TComboBox(AComponent).OnDrawItem);
//       //csOwnerDrawVariable combobox
//    end;
//


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
     ProcessComponent(AColorMap, AStyle, AComponent.Components[Index], Restore, Invalidate);

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
var
  LActionManager : TActionManager;
begin
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
    AddLog2('', Format(' LActionManager.Style exception RestoreActnManagerStyles Message %s Trace %s ',[e.Message, e.StackTrace]));
  end;
end;

procedure RestoreActnManagerStylesBackup;
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
    AddLog2('', Format(' LActionManager.Style exception RestoreActnManagerStyles Message %s Trace %s ',[e.Message, e.StackTrace]));
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


procedure AddLog2(const Category, Message : string);
begin
{$IFDEF ENABLELOG}
TFile.AppendAllText(sLogFileName, Format('%s %s : %s %s',[FormatDateTime('hh:nn:ss.zzz', Now), Category, Message, sLineBreak]));
{$ENDIF}
end;


//procedure DumpTypes;
//var
//  LCtx : TRttiContext;
//  LType : TRttiType;
//begin
//  LCtx:=TRttiContext.Create;
//  try
//    for LType in LCtx.GetTypes do
//
//
//
//     if StartsText('Gdiplus', LType.QualifiedName) then
//     //if SameText('DebugMgrOpts.TLogColors', LType.QualifiedName) then
//     begin
//       AddLog2(LType.QualifiedName);
//       TRttiUtils.DumpRttiType(LType, 'C:\Dephi\google-code\delphi-ide-theme-editor\IDE PlugIn\Galileo\'+LType.QualifiedName+'.pas');
//     end;
//
//  finally
//    LCtx.Free;
//  end;
//end;
//
//
//procedure DumpUnits;
//var
//  LCtx : TRttiContext;
//  LType : TRttiType;
//  LUnits : TStrings;
//  LUnitName : string;
//  p : integer;
//begin
//  LUnits:=TStringList.Create;
//  try
//    LCtx:=TRttiContext.Create;
//    try
//      for LType in LCtx.GetTypes do
//      begin
//       LUnitName :=  LType.QualifiedName;
//       p:= LastDelimiter('.', LUnitName);
//       if (p>0) and (Pos('<', LUnitName)=0) then
//       begin
//         LUnitName := Copy(LType.QualifiedName, 1, p-1);
//         if (LUnitName<>'') and (not StartsText('Data', LUnitName)) and (not StartsText('Bind', LUnitName)) and (not StartsText('Agent', LUnitName))
//         and (not StartsText('id', LUnitName)) and (not StartsText('FIREDAC.', LUnitName)) and (not StartsText('Datasnap.', LUnitName))
//         and (not StartsText('FMX', LUnitName)) and (not StartsText('System.', LUnitName)) and (not StartsText('WinApi.', LUnitName))
//         and (not StartsText('Castalia', LUnitName)) and (not StartsText('Vcl.', LUnitName)) and (LUnits.IndexOf(LUnitName)<0) then
//         begin
//           AddLog2('Dumping '+LUnitName);
//           LUnits.Add(LUnitName);
//           TRttiUtils.DumpUnit(LUnitName, 'C:\Dephi\google-code\delphi-ide-theme-editor\IDE PlugIn\Galileo\units\'+LUnitName+'.pas');
//         end;
//       end;
//      end;
//    finally
//      LCtx.Free;
//    end;
//  finally
//    LUnits.Free;
//  end;
//end;


initialization
{$IFDEF ENABLELOG}
 sLogFileName:=ExtractFilePath(GetModuleLocation())+'log.txt';
 ShowMessage('Log enabled');
{$ENDIF}

//DumpTypes;
//DumpUnits;
finalization

end.
