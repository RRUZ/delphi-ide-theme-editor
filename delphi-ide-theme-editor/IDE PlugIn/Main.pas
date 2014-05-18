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
// Portions created by Rodrigo Ruz V. are Copyright (C) 2011-2014 Rodrigo Ruz V.
// All Rights Reserved.
//
//**************************************************************************************************

//TODO

{
  * TIDEGradientTabSet border lines   -->  hook Pen.Color , Canvas.Polyline?  hook tbitmap ?
  * Enable/Disable
  * toolbar disabled buttons
  * TMessageViewForm -> TBetterHintWindowVirtualDrawTree fix font color

     toolbutton
        popup arrow color
  improve themes -> pro

  hook tbitmap for closable buttons background because is a tbitmap   Windowfromdc
  scroll fails on XE in preview of source highligter

  hook comboxbox
  TVirtualMethodInterceptor for hooks
  TVirtualMethodInterceptorExt - > DDetours
}

// DONE
{
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

//options
{

  * Fix icons gray
  * hook main menu
  * Hook ide code editor
  * hook all ide windows

  * Looad feel select (standard, XP)
  * choose the colors automatic way
  * activate glass colorization vista and windows 7?

    background color for windows (tlistview and treeview)
    skin by contorls (by xml) ->>then use ClassName ;)
     * treeview
     * TListBox
     * TlistView
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
  'Copyright: 2011-2014 Rodrigo Ruz V.'+sLineBreak+
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
 Colorizer.Hooks,
 Colorizer.HookForms,
 Colorizer.SettingsForm,
 Colorizer.Settings,
 Colorizer.OptionsDlg,
 ColorXPStyleActnCtrls,
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
{$IFDEF DLLWIZARD}
  IDEWizard      : TIDEWizard;
var
  FWizardIndex: Integer = InvalidIndex;

procedure FinalizeIDEColorizer;
var
  WizardServices: IOTAWizardServices;
begin
  if FWizardIndex <> InvalidIndex then
  begin
    Assert(Assigned(BorlandIDEServices));
    WizardServices := BorlandIDEServices as IOTAWizardServices;
    Assert(Assigned(WizardServices));
    WizardServices.RemoveWizard(FWizardIndex);
    FWizardIndex := InvalidIndex;
  end;
end;

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
   RegisterPackageWizard(TIDEWizard.Create as IOTAWizard);
end;
{$ENDIF}


function QuerySvcs(const Instance: IUnknown; const Intf: TGUID; out Inst): Boolean;
begin
  Result := (Instance <> nil) and Supports(Instance, Intf, Inst);
end;

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
    SplashScreenServices.AddPluginBitmap(SColorizerPluginCaption+' '+sVersion, SplashBmp.Handle);

  if QuerySvcs(BorlandIDEServices, IOTAAboutBoxServices, LAboutBoxServices) then
   FPlugInInfo:=LAboutBoxServices.AddPluginInfo(SColorizerPluginCaption, Format(SColorizerPluginDescription, [sVersion]), AboutBmp.Handle, False, 'Freeware');
end;

procedure UnRegisterPlugIn;
var
  LAboutBoxServices : IOTAAboutBoxServices;
begin
  if QuerySvcs(BorlandIDEServices, IOTAAboutBoxServices, LAboutBoxServices) and (FPlugInInfo<>InvalidIndex) then
     LAboutBoxServices.RemovePluginInfo(FPlugInInfo);

  FPlugInInfo:=InvalidIndex;
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
          //RegisterVClStylesFiles();
          if (TColorizerLocalSettings.Settings.UseVCLStyles) and (TColorizerLocalSettings.Settings.VCLStyleName<>'') then
          begin
            RegisterVClStylesFiles();
            found:=false;
            for s in TStyleManager.StyleNames do
             if not SameText(s, 'Windows') and SameText(s, TColorizerLocalSettings.Settings.VCLStyleName) then
             begin
               found:=True;
               break;
             end;

            if found then
            begin
              TStyleManager.SetStyle(TColorizerLocalSettings.Settings.VCLStyleName);
              GenerateColorMap(TColorizerLocalSettings.ColorMap,TStyleManager.ActiveStyle);
            end
            else
              MessageDlg(Format('The VCL Style %s was not found',[TColorizerLocalSettings.Settings.VCLStyleName]), mtInformation, [mbOK], 0);
          end;
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
  FreeAndNil(TColorizerLocalSettings.ColorMap);
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
    TColorizerLocalSettings.IDEData:= TDelphiVersionData.Create;
    FillCurrentDelphiVersion(TColorizerLocalSettings.IDEData);
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

    TColorizerLocalSettings.ColorMap:=TColorXPColorMap.Create(nil);
    LoadSettings(TColorizerLocalSettings.ColorMap, TColorizerLocalSettings.Settings);
    ImagesPath:=ExtractFilePath(GetModuleLocation)+'images\dock_images';
    s:=IncludeTrailingPathDelimiter(ImagesPath)+TColorizerLocalSettings.Settings.DockImages+'.png';
    if FileExists(s) then
      TColorizerLocalSettings.DockImages.LoadFromFile(s);

    RegisterPlugIn;
    AddMenuItems;

    InstallFormsHook();
    InstallColorizerHooks();

    InitColorizer();
    FTimerRefresher:=TTimer.Create(nil);
    FTimerRefresher.OnTimer :=OnRefreher;
    FTimerRefresher.Interval:=1500;
    FTimerRefresher.Enabled:=True;
  except
    on E: exception do
    begin
      ShowMessage(Format('%s : Error on TIDEWizard.Create %s %s Trace %s', [sMenuItemCaption, E.message, sLineBreak, E.StackTrace]));
    end;
  end;

end;

destructor TIDEWizard.Destroy;
begin
  AddLog('TIDEWizard.Destroy 0');
  FTimerRefresher.Enabled:=False;
  FTimerRefresher.Free;

  RestoreIDESettings();

  AddLog('TIDEWizard.Destroy 1');
  RemoveFormsHook();
  RemoveColorizerHooks();
  UnRegisterPlugIn;
  RemoveMenuItems;
  AddLog('TIDEWizard.Destroy 2');
  FreeAndNil(SplashBmp);
  FreeAndNil(AboutBmp);

  AddLog('TIDEWizard.Destroy 3');
  FreeAndNil(TColorizerLocalSettings.ActnStyleList);
  FreeAndNil(TColorizerLocalSettings.Settings);
  TColorizerLocalSettings.IDEData.Free;
  TColorizerLocalSettings.DockImages.Free;
  FreeAndNil(TColorizerLocalSettings.HookedWindows);
  FreeAndNil(TColorizerLocalSettings.HookedScrollBars);

  FinalizeColorizer();

  AddLog('TIDEWizard.Destroy 4');
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


//function GetActiveFormEditor: IOTAFormEditor;
//var
//  Module: IOTAModule;
//  Editor: IOTAEditor;
//  i: Integer;
//begin
//  Result := nil;
//  Module := (BorlandIDEServices as IOTAModuleServices).CurrentModule;
//  if Module<>nil then
//  begin
//    for i := 0 to Module.GetModuleFileCount - 1 do
//    begin
//      Editor := Module.GetModuleFileEditor(i);
//      if Supports(Editor, IOTAFormEditor, Result) then
//        Break;
//    end;
//  end;
//end;


end.


