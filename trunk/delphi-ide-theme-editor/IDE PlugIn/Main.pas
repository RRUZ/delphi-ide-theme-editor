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
//
//
//**************************************************************************************************

//TODO

{
  * popup menu tool bars (ex :recent files) -> create hook using colormap
  * panel separation (space)  - hook, panel, statusbar, and so on



  * restore support for Delphi 2007



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

  flat (ctrl3d) global or by control ?

    background color for windows (tlistview and treeview)
    skin by contorls (by xml) ->>then use ClassName ;)
     * treeview
     * TListBox
     * TlistView
}


unit Main;

interface

{.$DEFINE DLLWIZARD}

uses
 ToolsAPI;

Const
  sLogoBitmap             = 'Logo';
  sLogoIcon16             = 'Logo16';
  sAboutBitnap            = 'About';
  sMenuItemIdeColorizer   = 'Delphi IDE Colorizer';

{$IFDEF DLLWIZARD}
function InitIDEColorizer(const BorlandIDEServices: IBorlandIDEServices; RegisterProc: TWizardRegisterProc; var Terminate: TWizardTerminateProc): Boolean; stdcall;
exports   InitIDEColorizer name WizardEntryPoint;
{$ELSE}
procedure Register;
{$ENDIF}


implementation

{$R DelphiIDEColorizer.res}
{$R Gutter.res}

{.$DEFINE USE_DUMP_TIMER}

uses
 {$IF CompilerVersion >= 23}
 Vcl.Styles,
 Vcl.Themes,
 {$IFEND}
 Classes,
 ActnMan,
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
 uDelphiVersions,
 Colorizer.SettingsForm,
 Colorizer.Settings,
 Colorizer.OptionsDlg,
 ColorXPStyleActnCtrls,
 uMisc;


type
  TIDEWizard = class(TInterfacedObject, IOTAWizard, IOTANotifier)
  private
    ExplorerItem: TMenuItem;
    //ExplorerSeparator: TMenuItem;
    {$IFDEF USE_DUMP_TIMER}
    FDumperTimer   : TTimer;
    {$ENDIF}
    FTimerRefresher: TTimer;
    procedure AddMenuItems;
    procedure RemoveMenuItems;
    procedure InitColorizer;
    procedure FinalizeColorizer;
    procedure OnRefreher(Sender : TObject);
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
    procedure ExplorerItemClick(Sender: TObject);
  end;

var
  SplashBmp      : Graphics.TBitmap;
  AboutBmp       : Graphics.TBitmap;
{$IFDEF DLLWIZARD}
  IDEWizard      : TIDEWizard;
const
  InvalidIndex = -1;
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
  SColorizerPluginDescription=
  'Delphi IDE Colorizer'+sLineBreak+
  ''+sLineBreak+
  'Version %s'+sLineBreak+
  'Copyright: 2011-2014 Rodrigo Ruz V.'+sLineBreak+
  'All rights reserved.'+sLineBreak+
  ''+sLineBreak+
  'This is a freeware, you can use it freely without any fee.'+sLineBreak+
  ''+sLineBreak+
  'http://theroadtodelphi.wordpress.com/'+sLineBreak;
var
  AboutSvcs: IOTAAboutBoxServices;
begin
  SplashBmp:=Graphics.TBitmap.Create;
  SplashBmp.Handle := LoadBitmap(hInstance, sLogoBitmap);

  AboutBmp:=Graphics.TBitmap.Create;
  AboutBmp.Handle := LoadBitmap(hInstance, sAboutBitnap);

  if Assigned(SplashScreenServices) then
    SplashScreenServices.AddPluginBitmap(SColorizerPluginCaption, SplashBmp.Handle);

    if QuerySvcs(BorlandIDEServices, IOTAAboutBoxServices, AboutSvcs) then
     AboutSvcs.AddPluginInfo(SColorizerPluginCaption, Format(SColorizerPluginDescription, [uMisc.GetFileVersion(GeModuleLocation)]), AboutBmp.Handle, False, 'Freeware');
end;

procedure TIDEWizard.InitColorizer;
var
  LServices : INTAServices;
{$IF CompilerVersion >= 23}
  found : Boolean;
  s : string;
{$IFEND}
begin
  if BorlandIDEServices <> nil then
  begin
      LServices := (BorlandIDEServices as INTAServices);
      if LServices <> nil then
      begin
       RegisterColorizerAddinOptions;
       {$IFDEF DEBUG_MODE}
       lcomp:= TStringList.Create;
       {$ENDIF}
        try
          //DelphiTheme := TXPManifest.Create(nil);
          //AColorMap:=TTwilightColorMap.Create(nil);  usar para ver color de fuentes
          //AColorMap:=TStandardColorMap.Create(nil);
          TColorizerLocalSettings.ColorMap:=TColorXPColorMap.Create(nil);
          //AColorMap:=TColorXPColorMap.Create(Application);
          //TColorizerLocalSettings.GlobalColorMap:=AColorMap;

        //  TColorizerLocalSettings.ColorMap.FontColor:=clBlack;

          //GenerateColorMap(AColorMap, clWebKhaki);
          //AColorMap:=TThemedColorMap.Create(nil);
          //AColorMap.Color:=clWebLemonChiffon;
          //AColorMap.Color:=clDkGray;
          //AColorMap.Color:=clWebDarkSeaGreen;
          //AColorMap.Color:=clWebSteelBlue;
          //ShowMessage(GetBplLocation());
          //ShowMessage( ActionBarStyles.Text);

          //TColorizerLocalSettings.ActionBarStyle:=ColorXPStyle;
          LoadSettings(TColorizerLocalSettings.ColorMap, TColorizerLocalSettings.ActionBarStyle, TColorizerLocalSettings.Settings);
          //TColorizerLocalSettings.GlobalSettings:=Settings;
          {$IF CompilerVersion >= 23}
          if (TColorizerLocalSettings.Settings.UseVCLStyles) and (TColorizerLocalSettings.Settings.VCLStyleName<>'') then
          begin
            //StyleFile:=IncludeTrailingPathDelimiter(TColorizerLocalSettings.VCLStylesPath)+TColorizerLocalSettings.Settings.VCLStyleName;
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
          {$IFEND}
          RefreshIDETheme();
        finally
          {$IFDEF DEBUG_MODE}
           //lcomp.SaveToFile('C:\Users\Public\Documents\RAD Studio\Projects\2010\pkgDelphiWithTheme\Components.txt');
           lcomp.Free;
          {$ENDIF}
        end;
      end;
  end;
end;

procedure TIDEWizard.FinalizeColorizer;
begin
  FreeAndNil(TColorizerLocalSettings.ColorMap);
  UnRegisterColorizerAddinOptions
end;

{ TIDEWizard }

constructor TIDEWizard.Create;
begin
  inherited;
  {$WARN SYMBOL_PLATFORM OFF}
  //ReportMemoryLeaksOnShutdown:=DebugHook<>0;
  {$WARN SYMBOL_PLATFORM ON}
  //AColorMap:=nil;
  TColorizerLocalSettings.Settings:=TSettings.Create;
  //ColorizerForm := nil;
  RegisterPlugIn;
  AddMenuItems;
  InitColorizer();

  {$IFDEF USE_DUMP_TIMER}
  FDumperTimer:=TTimer.Create(nil);
  FDumperTimer.OnTimer :=OnDumper;
  FDumperTimer.Interval:=1000;
  FDumperTimer.Enabled:=True;
  {$ENDIF}

  FTimerRefresher:=TTimer.Create(nil);
  FTimerRefresher.OnTimer :=OnRefreher;
  FTimerRefresher.Interval:=1500;
  FTimerRefresher.Enabled:=True;
end;



procedure TIDEWizard.AddMenuItems;
var
  MainMenu: TMainMenu;
  ToolsMenu: TMenuItem;
  I, InsertPosition: Integer;
  LIcon : TIcon;
begin
  inherited;
  if BorlandIDEServices <> nil then
  begin
    MainMenu  := (BorlandIDEServices as INTAServices).MainMenu;
    ToolsMenu := MainMenu.Items[8];

    for I := 0 to MainMenu.Items.Count - 1 do
      if CompareText(MainMenu.Items[I].Name, 'ToolsMenu') = 0 then
      begin
        ToolsMenu := MainMenu.Items[I];
        Break;
      end;

    InsertPosition:=ToolsMenu.Count - 1;
    //ExplorerSeparator := Menus.NewItem('-', 0, False, False, nil, 0, 'IdeClorSeparator');
    //ToolsMenu.InsertComponent(ExplorerSeparator);
    //ToolsMenu.Insert(InsertPosition, ExplorerSeparator);
    ExplorerItem := Menus.NewItem(sMenuItemIdeColorizer, Menus.ShortCut(Word('D'), [ssCtrl]), False, True, ExplorerItemClick, 0, 'IdeClorItem');

    LIcon:=TIcon.Create;
    try
     LIcon.Handle := LoadIcon(hInstance, sLogoIcon16);
     ExplorerItem.ImageIndex:=MainMenu.Images.AddIcon(LIcon);
    finally
      LIcon.Free;
    end;

    ToolsMenu.InsertComponent(ExplorerItem);
    ToolsMenu.Insert(InsertPosition+1, ExplorerItem);
  end;
end;

procedure TIDEWizard.AfterSave;
begin
end;

procedure TIDEWizard.BeforeSave;
begin
end;


destructor TIDEWizard.Destroy;
begin
  RemoveMenuItems;
  SplashBmp.Free;
  AboutBmp.Free;

  FinalizeColorizer();
  {$IFDEF USE_DUMP_TIMER}
  FDumperTimer.Enabled:=False;
  FDumperTimer.Free;
  {$ENDIF}
  FTimerRefresher.Enabled:=False;
  FTimerRefresher.Free;
  //ColorizerForm.Free;
  inherited;
end;

procedure TIDEWizard.Destroyed;
begin
end;

procedure TIDEWizard.Execute;
begin
end;

procedure TIDEWizard.ExplorerItemClick(Sender: TObject);
var
  ColorizerForm : TFormIDEColorizerSettings;
begin
  ColorizerForm := TFormIDEColorizerSettings.Create(nil);
  ColorizerForm.Name := 'DelphiIDEColorizer_SettingsForm';
  ColorizerForm.LabelSetting.Caption:='Delphi IDE Colorizer for '+TColorizerLocalSettings.IDEData.Name;
  ColorizerForm.Init;
  ColorizerForm.PanelMain.BorderWidth:=5;
  ColorizerForm.ShowModal();
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

{$IFDEF USE_DUMP_TIMER}
procedure TIDEWizard.OnDumper(Sender: TObject);
Var
  FileName : String;
begin
  FileName:=ExtractFilePath(GetBplLocation())+'Galileo\Dump_'+Screen.ActiveForm.Name+'.dfm';
  if not FileExists(FileName) then
  SaveComponentToFile(Screen.ActiveForm, FileName);
end;
{$ENDIF}

procedure TIDEWizard.RemoveMenuItems;
begin
  ExplorerItem.Free;
//  ExplorerSeparator.Free;
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


