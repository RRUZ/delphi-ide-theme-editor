//**************************************************************************************************
//
// Unit Main
// unit Main  for the Delphi IDE Colorizer
//
// The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
// you may not use this file except in compliance with the License. You may obtain a copy of the
// License at http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
// ANY KIND, either express or implied. See the License for the specific language governing rights
// and limitations under the License.
//
// The Original Code is Main.pas.
//
// The Initial Developer of the Original Code is Rodrigo Ruz V.
// Portions created by Rodrigo Ruz V. are Copyright (C) 2011-2015 Rodrigo Ruz V.
// All Rights Reserved.
//
//**************************************************************************************************

//TODO
 {TODO -oRRUZ -cIssue :Fix refresh issue on Object Inspector Window when the theme is changed}
 {TODO -oRRUZ -cIssue :Fix font color in Castalia (XE8) combobox}
 {TODO -oRRUZ -cPerformance : Improve overall performance}
 {TODO -oRRUZ -cPerformance : improve performance in load of settings DIC form}
 {TODO -oRRUZ -cHooks : add StyleHook for TTreeview}
 {TODO -oRRUZ -cHooks : add StyleHook for TListview}
 {DONE -oRRUZ -cHooks : add support for TBitBtn}
 {TODO -oRRUZ -cFeature : Add option to override event log colors}
 {TODO -oRRUZ -cFeature : Transparent system menus}
 {TODO -oRRUZ -cFeature : Fast vcl styles/themes switch}
 {TODO -oRRUZ -cFeature : Add option to change Font}
 {TODO -oRRUZ -cFeature : Add option to change DisAssember colors}
 {TODO -oRRUZ -cFeature : Add CNIDEWizards true integration}



// DONE
{
  * fixed - Draw TEdit
  * fixed - Draw TLabel disabled
  * fixed - Draw TRadioButton in wrong Vert. position
  * Done - improve performance in load of project options form
  * Done - Add support for TCheckbox, TGroupBox, TRadiobutton (including fonts
  * fixed - bug in call stack -> disassmbler window is not styled
  * fixed - Scroll fails on XE in preview of source highligter - fixed with scrollbar all.
  * frame in buttons of TIDECategoryButtons   depending oh theme colors
  * Fix background for vcl styles controls (button, checkbox)
  * VCL Styles - when the IDE desktop is changed a white border is present in some forms (workaround resize the form)
  * VCL Styles - some issues in scrollbars with some vcl styles (ex : Tablet Dark)
  * VCL Styles - some issues in overlaped floating windows
  * Add support for TPopupListBox
  * Fix fonts colors in ide tabs XE6
  * add custom option for header virtual trees and listview
  * TDockToolBar (toolbars) remove border
  * TClosableTabScroller border  -->  gdi outline
  * TIDEGradientTabSet custom colors
  * TIDEGradientTabSet border  -->  gdi outline
  * Enable/Disable
  * bug in bg color of controls in Font dialog
  * Improve support for TInspListBox
  * Fix hint font color in TBetterHintWindowVirtualDrawTree  / TMessageHintWindow
  * Add support for TListButton
  * Add support for TCnProcDropDownBox
  * Improve support for TCnProcListComboBox (border , drop button)
  * TMessageViewForm -> TBetterHintWindowVirtualDrawTree fix font color
  * toolbar disabled buttons
  * toolbutton
  *      popup arrow color
  * toolbar multiline
  * hook tbitmap for closable buttons background because is a tbitmap Windowfromdc
  * hook highlight color
  * hook comboxbox
  * hook DrawFrameControl
  * XE change active page of source editor  redraw TIDECategoryButtons
  * DumpAllTypes and related to rtti helper
  * rtti helper must be class
  * popup menu tool bars (ex :recent files) -> create hook using colormap
  * TClosableTabScroller background done
  * TProjectManagerForm -> TVirtualStringTree font color
  * Event log background color done  - Via Delphi IDE
  *                    TDisassemblerView   done
  *  TStackViewFrame - TDumpVie0w    done
  *  TRegisterView    done
  *  TFlagsView       done
  *  TFPUWindow       done
  * Refactoring - find references (Background, toolbar, font)  done
  * TRefactoringTree   (Background, toolbar, font) done
  * Threads (Tlistview) columns headers  done
  * Docked forms title (active/inactive). done
  * TStatusBar separators   done
  * TTabSet background - done
  * gutter code editor   - done:
  * detect parent object from class - done via JCL ProcByLevel
  * popup menu code editor  -done :)
  * TIDEGradientTabSet background done
  * remove access violations on manual unload of package - done
  * check for color key in OTA ;) done :(
  * options-enviroment variables crash    -> TDefaultEnvironmentDialog  GExperts????    done:)
  * tidegradeint buttons , not paint correctly  done:)
  * border of panel in options window is not painted corectly done
}

unit Main;

interface
{$I ..\Common\Jedi.inc}

uses
 ToolsAPI;

Const
  sLogoBitmap             = 'Logo';
  sLogoIcon16             = 'Logo16';
  sAboutBitnap            = 'About';
  sMenuItemCaption        = 'Delphi IDE Colorizer';
  sMenuItemName           = 'DelphiIDEClrItem';
  sActionItemIdeColorizer = 'DelphiIDEClrAction';
  sColorizerPluginDescription=
  'Delphi IDE Colorizer'+sLineBreak+
  'https://code.google.com/p/delphi-ide-theme-editor'+sLineBreak+
  'Version %s'+sLineBreak+
  'Copyright: 2011-2015 Rodrigo Ruz V.'+sLineBreak+
  'http://theroadtodelphi.wordpress.com/'+sLineBreak+
  'All rights reserved.'+sLineBreak+
  ''+sLineBreak+
  'This is a freeware, you can use it freely without any fee.';


{$IFDEF DLLWIZARD}
function InitIDEColorizer(const BorlandIDEServices: IBorlandIDEServices; RegisterProc: TWizardRegisterProc; var Terminate: TWizardTerminateProc): Boolean; stdcall;
exports   InitIDEColorizer name WizardEntryPoint;
{$ELSE}
procedure Register;
{$ENDIF}


implementation

{$R DelphiIDEColorizer.res}
{$R Gutter.res}
{$R DICimages.res}

uses
 {$IF CompilerVersion >= 23}
 Vcl.Styles,
 Vcl.Themes,
 Vcl.PlatformDefaultStyleActnCtrls,
 {$IFEND}
 {$IFDEF DELPHI2009_UP}
 Generics.Collections,
 {$ENDIF}

 Classes,
 ActnMan,
 ActnList,
 Controls,
 Windows,
 Graphics,
 UxTheme,
 Colorizer.Utils,
 ActnColorMaps,
 SysUtils,
 Forms,
 Dialogs,
 Menus,
 ComObj,
 ExtCtrls,
 IOUtils,
 PngImage,
 uDelphiVersions,
 {$IFDEF DELPHIXE8_UP}
 uDelphiIDEHighlight,
 {$ENDIF}
 Colorizer.Hooks,
 Colorizer.Hook.Forms,
 Colorizer.Hooks.GDIPOBJ,
 Colorizer.Hooks.Windows,
 Colorizer.Hooks.UxTheme,
 Colorizer.SettingsForm,
 Colorizer.Hooks.IDE,
 Colorizer.Settings,
 Colorizer.OptionsDlg,
 Colorizer.XPStyleActnCtrls,
 {$IFDEF DELPHIXE2_UP}
 Colorizer.Hooks.ThemedActnCtrls,
 Colorizer.Vcl.Styles,
 {$ENDIF}
 Vcl.Styles.Utils.FlatMenus,
 uMisc;


type

  TIDEWizard = class(TInterfacedObject, IOTAWizard, IOTANotifier)
  private
    FDICConfMenuItem: TMenuItem;
    FTimerRefresher: TTimer;
    procedure AddMenuItems;
    procedure RemoveMenuItems;
    procedure InitColorizer;
    procedure FinalizeColorizer;
    procedure OnRefreher(Sender : TObject);
    procedure DICConfClick(Sender: TObject);
    {$IFDEF USE_DUMP_TIMER}
    procedure OnDumper(Sender : TObject);
    {$ENDIF}
  public
    constructor Create;
    destructor Destroy; override;
    function GetIDString: string;
    procedure Execute;
    function GetName: string;
    function GetState: TWizardState;
    procedure AfterSave;
    procedure BeforeSave;
    procedure Destroyed;
    procedure Modified;
  end;


const
  InvalidIndex = -1;

var
  SplashBmp      : Graphics.TBitmap;
  AboutBmp       : Graphics.TBitmap;
  FPlugInInfo    : Integer = InvalidIndex;
  IDEWizard      : TIDEWizard;
{$IFDEF DLLWIZARD}
var
  FWizardIndex: Integer = InvalidIndex;
{$ENDIF}

function QuerySvcs(const Instance: IUnknown; const Intf: TGUID; out Inst): Boolean;
begin
  Result := (Instance <> nil) and Supports(Instance, Intf, Inst);
end;

procedure UnRegisterPlugIn;
var
  LAboutBoxServices : IOTAAboutBoxServices;
begin
  if QuerySvcs(BorlandIDEServices, IOTAAboutBoxServices, LAboutBoxServices) and (FPlugInInfo<>InvalidIndex) then
     LAboutBoxServices.RemovePluginInfo(FPlugInInfo);

  FPlugInInfo:=InvalidIndex;
end;

procedure InstallAllHooks;
begin
  AddLog2('InstallAllHooks', 'InstallHooksWinAPI');
  InstallHooksWinAPI();
  AddLog2('InstallAllHooks', 'InstallHooksUXTheme');
  InstallHooksUXTheme();
  AddLog2('InstallAllHooks', 'InstallFormsHook');
  InstallFormsHook();
  AddLog2('InstallAllHooks', 'InstallHooksIDE');
  InstallHooksIDE();
  AddLog2('InstallAllHooks', 'InstallHooksGDI');
  InstallHooksGDI();
  AddLog2('InstallAllHooks', 'InstallColorizerHooks');
  InstallColorizerHooks();
  {$IFDEF DELPHIXE2_UP}
  AddLog2('InstallAllHooks', 'InstallThemedActnCtrlsHooks');
  InstallThemedActnCtrlsHooks();
  {$ENDIF}
  AddLog2('InstallAllHooks', 'RegisterFlatMenusHooks');
  RegisterFlatMenusHooks();
  AddLog2('InstallAllHooks', 'Done');
end;

procedure RemoveAllHooks;
begin
  //don't change unload order
  AddLog2('RemoveAllHooks', 'Init');
  RemoveColorizerHooks();
  AddLog2('RemoveAllHooks', 'RemoveColorizerHooks');
  RemoveHooksWinAPI();
  AddLog2('RemoveAllHooks', 'RemoveHooksWinAPI');
  RemoveHooksUXTheme();
  AddLog2('RemoveAllHooks', 'RemoveHooksUXTheme');
  RemoveFormsHook();
  AddLog2('RemoveAllHooks', 'RemoveFormsHook');
  RemoveHooksIDE();
  AddLog2('RemoveAllHooks', 'RemoveHooksIDE');
  RemoveHooksGDI();
  AddLog2('RemoveAllHooks', 'RemoveHooksGDI');
  {$IFDEF DELPHIXE2_UP}
  RemoveThemedActnCtrlsHooks();
  AddLog2('RemoveAllHooks', 'RemoveThemedActnCtrlsHooks');
  {$ENDIF}
  UnregisterFlatMenusHooks();
  AddLog2('RemoveAllHooks', 'Done');
end;


procedure FinalizeIDEColorizer;
{$IFDEF DLLWIZARD}
var
  WizardServices: IOTAWizardServices;
{$ENDIF}
begin
  AddLog2('FinalizeIDEColorizer', 'Init');
  IDEWizard.FTimerRefresher.Enabled:=False;
  IDEWizard.FTimerRefresher.Free;

{$IFNDEF DLLWIZARD}
  RestoreIDESettings();
{$ENDIF}

  AddLog2('FinalizeIDEColorizer', 'RemoveAllHooks');
  RemoveAllHooks();

  UnRegisterPlugIn;
  IDEWizard.RemoveMenuItems;
  AddLog2('FinalizeIDEColorizer', 'RemoveMenuItems');
  FreeAndNil(SplashBmp);
  FreeAndNil(AboutBmp);

  FreeAndNil(TColorizerLocalSettings.FActnStyleList);
  FreeAndNil(TColorizerLocalSettings.FSettings);
  TColorizerLocalSettings.IDEData.Free;
 {$IFDEF DELPHIXE8_UP}
  TColorizerLocalSettings.ModernTheme.Free;
{$ENDIF}

  TColorizerLocalSettings.DockImages.Free;
  FreeAndNil(TColorizerLocalSettings.FHookedWindows);
  FreeAndNil(TColorizerLocalSettings.FHookedScrollBars);
  FreeAndNil(TColorizerLocalSettings.FWinAPIClasses);

  IDEWizard.FinalizeColorizer();
  AddLog2('FinalizeIDEColorizer', 'FinalizeColorizer');

{$IFDEF DLLWIZARD}
  if FWizardIndex <> InvalidIndex then
  begin
    Assert(Assigned(BorlandIDEServices));
    WizardServices := BorlandIDEServices as IOTAWizardServices;
    Assert(Assigned(WizardServices));
    WizardServices.RemoveWizard(FWizardIndex);
    FWizardIndex := InvalidIndex;
    AddLog2('FinalizeIDEColorizer', 'WizardServices.RemoveWizard');
  end;
{$ENDIF}
end;

{$IFDEF DLLWIZARD}
function InitIDEColorizer(const BorlandIDEServices: IBorlandIDEServices;
  RegisterProc: TWizardRegisterProc;
  var Terminate: TWizardTerminateProc): Boolean; stdcall;
var
  WizardServices: IOTAWizardServices;
begin
  Result := BorlandIDEServices <> nil;
  if Result then
  begin
    Assert(ToolsAPI.BorlandIDEServices = BorlandIDEServices);
    Terminate := FinalizeIDEColorizer;
    WizardServices := BorlandIDEServices as IOTAWizardServices;
    Assert(Assigned(WizardServices));
    IDEWizard := TIDEWizard.Create;
    FWizardIndex := WizardServices.AddWizard(IDEWizard as IOTAWizard);
    Result := (FWizardIndex >= 0);
  end;
end;

{$ELSE}
procedure Register;
begin
   IDEWizard:=TIDEWizard.Create;
   RegisterPackageWizard(IDEWizard as IOTAWizard);
end;
{$ENDIF}

procedure RegisterPlugIn;
const
  SColorizerPluginCaption    ='Delphi IDE Colorizer';
var
  LAboutBoxServices: IOTAAboutBoxServices;
  sVersion         : string;
begin
  SplashBmp:=Graphics.TBitmap.Create;
  SplashBmp.Handle := LoadBitmap(hInstance, sLogoBitmap);

  AboutBmp:=Graphics.TBitmap.Create;
  AboutBmp.Handle := LoadBitmap(hInstance, sAboutBitnap);
  sVersion:=uMisc.GetFileVersion(GetModuleLocation);

  if Assigned(SplashScreenServices) then
    SplashScreenServices.AddPluginBitmap(SColorizerPluginCaption+' '+sVersion, SplashBmp.Handle, False, 'Freeware', 'Beta');

  if QuerySvcs(BorlandIDEServices, IOTAAboutBoxServices, LAboutBoxServices) then
   FPlugInInfo:=LAboutBoxServices.AddPluginInfo(SColorizerPluginCaption, Format(SColorizerPluginDescription, [sVersion]), AboutBmp.Handle, False, 'Freeware', 'Beta');
end;


procedure TIDEWizard.InitColorizer;
var
  LINTAServices : INTAServices;
 {$IFDEF DELPHIXE2_UP}
  found : Boolean;
  s : string;
 {$ENDIF}
begin
  try
    if BorlandIDEServices <> nil then
    begin
        LINTAServices := (BorlandIDEServices as INTAServices);
        if LINTAServices <> nil then
        begin
          RegisterColorizerAddinOptions;
          {$IFDEF DELPHIXE2_UP}
          RegisterVClStylesFiles();
          if (TColorizerLocalSettings.Settings.UseVCLStyles) and (TColorizerLocalSettings.Settings.VCLStyleName<>'') then
          begin
            //RegisterVClStylesFiles();
            found:=false;
            for s in TStyleManager.StyleNames do
             if not SameText(s, 'Windows') and SameText(s, TColorizerLocalSettings.Settings.VCLStyleName) then
             begin
               found:=True;
               break;
             end;

            if found then
            begin
              //TStyleManager.SetStyle(TColorizerLocalSettings.Settings.VCLStyleName);
              SetColorizerVCLStyle(TColorizerLocalSettings.Settings.VCLStyleName);
              //refresh the colormap colors
              LoadSettings(TColorizerLocalSettings.ColorMap, TColorizerLocalSettings.Settings);
              //GenerateColorMap(TColorizerLocalSettings.ColorMap,TStyleManager.ActiveStyle);
            end
            else
            begin
              MessageDlg(Format('The VCL Style %s was not found. The VCL Styles will be disabled',[TColorizerLocalSettings.Settings.VCLStyleName]), mtInformation, [mbOK], 0);
              TColorizerLocalSettings.Settings.UseVCLStyles:=False;
              WriteSettings(TColorizerLocalSettings.Settings, ExtractFilePath(GetModuleLocation()));
            end;
          end;

          if TColorizerLocalSettings.Settings.UseVCLStyles and TColorizerLocalSettings.Settings.VCLStylesMenusColors then
           TColorizerLocalSettings.ActionBarStyle:=PlatformDefaultStyle;
          {$ENDIF}

          if Assigned(TColorizerLocalSettings.Settings) and TColorizerLocalSettings.Settings.Enabled  then
            RefreshIDETheme();
        end;
    end;
  except
    on E: exception do
    begin
      ShowMessage(Format('%s : Error on InitColorizer %s %s Trace %s', [sMenuItemCaption, E.message, sLineBreak, E.StackTrace]));
    end;
  end;
end;

procedure TIDEWizard.FinalizeColorizer;
begin
  FreeAndNil(TColorizerLocalSettings.FColorMap);
  UnRegisterColorizerAddinOptions;
end;

{ TIDEWizard }
constructor TIDEWizard.Create;
var
  ImagesPath, s : string;
begin
  inherited;
  try
    {$WARN SYMBOL_PLATFORM OFF}
    ReportMemoryLeaksOnShutdown:=DebugHook<>0;
    {$WARN SYMBOL_PLATFORM ON}
    //SourceEditorNotifiers := TList.Create;
    TColorizerLocalSettings.Unloading:=False;
    TColorizerLocalSettings.IDEData:= TDelphiVersionData.Create;
    FillCurrentDelphiVersion(TColorizerLocalSettings.IDEData);
 {$IFDEF DELPHIXE8_UP}
    TColorizerLocalSettings.ModernTheme:=TModernTheme.Create(TColorizerLocalSettings.IDEData.Version);
    TColorizerLocalSettings.ModernTheme.LoadData();
 {$ENDIF}
    TColorizerLocalSettings.VCLStylesPath:=GetVCLStylesFolder(TColorizerLocalSettings.IDEData.Version);
    TColorizerLocalSettings.ActnStyleList:= TList<TActionManager>.Create;
    TColorizerLocalSettings.ColorMap:=nil;
    TColorizerLocalSettings.Settings:=TSettings.Create;
    TColorizerLocalSettings.ImagesGutterChanged:=False;
    TColorizerLocalSettings.DockImages:= TPngImage.Create;

    TColorizerLocalSettings.HookedWindows:=TStringList.Create;
    TColorizerLocalSettings.HookedWindows.LoadFromFile(IncludeTrailingPathDelimiter(ExtractFilePath(GetModuleLocation))+'HookedWindows.dat');
    TColorizerLocalSettings.HookedWindowsText:=TColorizerLocalSettings.HookedWindows.Text;

    TColorizerLocalSettings.HookedScrollBars:=TStringList.Create;
    TColorizerLocalSettings.HookedScrollBars.LoadFromFile(IncludeTrailingPathDelimiter(ExtractFilePath(GetModuleLocation))+'HookedScrollBars.dat');
    TColorizerLocalSettings.HookedScrollBarsText:=TColorizerLocalSettings.HookedScrollBars.Text;

    TColorizerLocalSettings.WinAPIClasses:=TStringList.Create;
    TColorizerLocalSettings.WinAPIClasses.LoadFromFile(IncludeTrailingPathDelimiter(ExtractFilePath(GetModuleLocation))+'WinAPIClasses.dat');

    TColorizerLocalSettings.ColorMap:=TColorizerColorMap.Create(nil);
    LoadSettings(TColorizerLocalSettings.ColorMap, TColorizerLocalSettings.Settings);
    ImagesPath:=ExtractFilePath(GetModuleLocation)+'images\dock_images';
    s:=IncludeTrailingPathDelimiter(ImagesPath)+TColorizerLocalSettings.Settings.DockImages+'.png';
    if FileExists(s) then
      TColorizerLocalSettings.DockImages.LoadFromFile(s);

    RegisterPlugIn;
    AddMenuItems;

    InstallAllHooks();

    InitColorizer();
    FTimerRefresher:=TTimer.Create(nil);
    FTimerRefresher.OnTimer :=OnRefreher;
    FTimerRefresher.Interval:=1500;
    FTimerRefresher.Enabled:=True;

    if TColorizerLocalSettings.Settings.CheckUpdates then
       CheckForUpdates(True);
  except
    on E: exception do
    begin
      ShowMessage(Format('%s : Error on TIDEWizard.Create %s %s Trace %s', [sMenuItemCaption, E.message, sLineBreak, E.StackTrace]));
    end;
  end;

end;

destructor TIDEWizard.Destroy;
begin
  TColorizerLocalSettings.Unloading:=True;
  {$IFNDEF DLLWIZARD}
  FinalizeIDEColorizer();
  {$ENDIF}
  inherited;
end;

procedure TIDEWizard.AddMenuItems;
var
  Index: Integer;
  IDEMenuItem, ToolsMenuItem: TMenuItem;
  NTAServices: INTAServices;
  LIcon : TIcon;
begin
  inherited;
   if BorlandIDEServices <> nil then
   begin
     NTAServices := (BorlandIDEServices as INTAServices);

      IDEMenuItem := NTAServices.MainMenu.Items;
      if not Assigned(IDEMenuItem) then
        raise Exception.Create('Was not possible found the IDE Menu Item');

      ToolsMenuItem := nil;
      for Index := 0 to IDEMenuItem.Count - 1 do
        if CompareText(IDEMenuItem.Items[Index].Name, 'ToolsMenu') = 0 then
          ToolsMenuItem := IDEMenuItem.Items[Index];
      if not Assigned(ToolsMenuItem) then
        raise Exception.Create('Was not possible found the IDE Tools Menu Item');

      FDICConfMenuItem := TMenuItem.Create(nil);
      FDICConfMenuItem.Name := sMenuItemName;
      FDICConfMenuItem.Caption := sMenuItemCaption;
      FDICConfMenuItem.OnClick := DICConfClick;

      LIcon := TIcon.Create;
      try
       LIcon.Handle := LoadIcon(hInstance, sLogoIcon16);
        FDICConfMenuItem.ImageIndex := NTAServices.ImageList.AddIcon(LIcon);
      finally
        LIcon.Free;
      end;

      ToolsMenuItem.Insert(0, FDICConfMenuItem);
   end;
end;

procedure TIDEWizard.DICConfClick(Sender: TObject);
var
  ColorizerForm : TFormIDEColorizerSettings;
begin
  try
    ColorizerForm := TFormIDEColorizerSettings.Create(nil);
    ColorizerForm.Name := 'DelphiIDEColorizer_SettingsForm';
    ColorizerForm.LabelSetting.Caption:='Delphi IDE Colorizer for '+TColorizerLocalSettings.IDEData.Name;
    ColorizerForm.Init;
    ColorizerForm.PanelMain.BorderWidth:=5;
    ColorizerForm.ShowModal();
  except
    on E: exception do
    begin
      ShowMessage(Format('%s : Error on dialog display %s', [sMenuItemCaption, E.message]));
    end;
  end;
end;

procedure TIDEWizard.AfterSave;
begin
end;

procedure TIDEWizard.BeforeSave;
begin
end;



procedure TIDEWizard.Destroyed;
begin
end;

procedure TIDEWizard.Execute;
begin
end;

function TIDEWizard.GetIDString: string;
begin
  Result := 'Delphi.IDEColorizer';
end;

function TIDEWizard.GetName: string;
begin
  Result := 'Delphi IDE Colorizer';
end;

function TIDEWizard.GetState: TWizardState;
begin
  Result := [wsEnabled];
end;

procedure TIDEWizard.Modified;
begin
end;

procedure TIDEWizard.OnRefreher(Sender: TObject);
begin
 if Assigned(TColorizerLocalSettings.ColorMap) and Assigned(TColorizerLocalSettings.Settings) and TColorizerLocalSettings.Settings.Enabled then
 begin
  RefreshIDETheme();
  FTimerRefresher.Enabled:=False;
 end;
end;


procedure TIDEWizard.RemoveMenuItems;
begin
  FreeAndNil(FDICConfMenuItem);
end;

end.


