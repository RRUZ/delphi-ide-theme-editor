{**************************************************************************************************}
{                                                                                                  }
{ Unit Main                                                                                        }
{ unit Main  for the Delphi IDE Colorizer                                                          }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is Main.pas.                                                                   }
{                                                                                                  }
{ The Initial Developer of the Original Code is Rodrigo Ruz V.                                     }
{ Portions created by Rodrigo Ruz V. are Copyright (C) 2011 Rodrigo Ruz V.                         }
{ All Rights Reserved.                                                                             }
{                                                                                                  }
{                                                                                                  }
{                                                                                                  }
{**************************************************************************************************}

//TODO

{

  * check for color key in OTA ;) done :(


  * options-enviroment variables crash    -> TDefaultEnvironmentDialog  GExperts????
  * tidegradeint buttons , not paint correctly
  * border of panel in  options window is not painted corectly
  * popup action bar, set colormap
  * gutter
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

Const
  sLogoBitmap             = 'Logo';
  sLogoIcon16             = 'Logo16';
  sAboutBitnap            = 'About';
  sMenuItemIdeColorizer   = 'Delphi IDE Colorizer';

procedure Register;

implementation

{$R DelphiIDEColorizer.res}

uses
 {$IF CompilerVersion >= 23}
 Vcl.Styles,
 Vcl.Themes,
 {$IFEND}
 Classes,
 ActnMan,
 Controls,
 Windows,
 ToolsAPI,
 Graphics,
 UxTheme,
 uColorizerUtils,
 ActnColorMaps,
 SysUtils,
 Forms,
 Dialogs,
 XPMan,
 Menus,
 ComObj,
 uClrSettings;


type
  TIDEWizard = class(TInterfacedObject, IOTAWizard, IOTANotifier)
  private
    //ColorizerForm: TFrmIDEColorizerSettings;
    Settings : TSettings;
    AColorMap:TXPColorMap;
    //ExplorerItem: TMenuItem;
    //ExplorerSeparator: TMenuItem;
    procedure AddMenuItems;
    procedure RemoveMenuItems;
    procedure InitColorizer;
    procedure FinalizeColorizer;
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
  SplashBmp     : Graphics.TBitmap;
  AboutBmp      : Graphics.TBitmap;
{$IFDEF Use_Notifiers}
  NotifierIndex : Integer;

type
  TToolbarIDENotifier = class(TNotifierObject, IOTANotifier, INTACustomizeToolbarNotifier)
  protected
    procedure ShowToolbar(Toolbar: TWinControl; Show: Boolean);
    procedure CreateButton(AOwner: TComponent; var Button: TControl;
      Action: TBasicAction);
    procedure FilterAction(Action: TBasicAction; ViewingAllCommands: Boolean;
      var DisplayName: string; var Display: Boolean; var Handled: Boolean);
    procedure FilterCategory(var Category: string; var Display: Boolean;
      var Handled: Boolean);
    procedure ResetToolbar(var Toolbar: TWinControl);
    procedure ToolbarModified(Toolbar: TWinControl);
  end;

type
  TToolbarIDEStreamNotifier = class(TNotifierObject, IOTANotifier, INTAToolbarStreamNotifier)
  public
    procedure AfterSave;overload;
    procedure BeforeSave;overload;
    procedure AfterSave(Toolbar: TWinControl);overload;
    procedure BeforeSave(Toolbar: TWinControl);overload;
    procedure ToolbarLoaded(Toolbar: TWinControl);
  end;
{$ENDIF}


procedure Register;
{$IFDEF Use_Notifiers}
var
  Services: INTAServices;
{$ENDIF}
begin
{$IFDEF Use_Notifiers}
  Services := BorlandIDEServices as INTAServices;
  Assert(Assigned(Services), 'INTAServices not available');
  NotifierIndex := Services.RegisterToolbarNotifier(TToolbarIDENotifier.Create);
{$ENDIF}
  RegisterPackageWizard(TIDEWizard.Create);
end;

{$IFDEF Use_Notifiers}
procedure RemoveNotifier;
var
  Services: INTAServices;
begin
  if NotifierIndex <> -1 then
  begin
    Services := BorlandIDEServices as INTAServices;
    Assert(Assigned(Services), 'IOTAServices not available');
    Services.UnregisterToolbarNotifier(NotifierIndex);
  end;
end;
{$ENDIF}

function GetFileVersion(const FileName: string): string;
var
  FSO  : OleVariant;
begin
  FSO    := CreateOleObject('Scripting.FileSystemObject');
  Result := FSO.GetFileVersion(FileName);
end;


function QuerySvcs(const Instance: IUnknown; const Intf: TGUID; out Inst): Boolean;
begin
  Result := (Instance <> nil) and Supports(Instance, Intf, Inst);
end;

procedure RegisterPlugIn;
const
  SColorizerPluginCaption    ='Delphi IDE Colorizer';
  SColorizerPluginDescription=
  'Delphi IDE Colorizer'+#13#10+
  ''+#13#10+
  'Version %s'+#13#10+
  'Copyright: 2011 Rodrigo Ruz V.'+#13#10+
  'All rights reserved.'+#13#10+
  ''+#13#10+
  'This is a freeware, you can use it freely without any fee.'+#13#10+
  ''+#13#10+
  'http://theroadtodelphi.wordpress.com/'+#13#10;
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
    AboutSvcs.AddPluginInfo(SColorizerPluginCaption, Format(SColorizerPluginDescription,[GetFileVersion(GetBplLocation)]), AboutBmp.Handle, False, 'Freeware');
end;
      {
function GetActiveFormEditor: IOTAFormEditor;
var
  Module: IOTAModule;
  Editor: IOTAEditor;
  i: Integer;
begin
  Result := nil;
  Module := (BorlandIDEServices as IOTAModuleServices).CurrentModule;
  if Module<>nil then
  begin
    for i := 0 to Module.GetModuleFileCount - 1 do
    begin
      Editor := Module.GetModuleFileEditor(i);
      if Supports(Editor, IOTAFormEditor, Result) then
        Break;
    end;
  end;
end;
            }
procedure TIDEWizard.InitColorizer;
var
  LServices : INTAServices;
{$IF CompilerVersion >= 23}
  StyleFile : string;
{$IFEND}
begin
  if BorlandIDEServices <> nil then
  begin
      LServices := (BorlandIDEServices as INTAServices);
      if LServices <> nil then
      begin
       {$IFDEF DEBUG_MODE}
       lcomp:= TStringList.Create;
       {$ENDIF}
        try
          DelphiTheme := TXPManifest.Create(nil);
          //AColorMap:=TTwilightColorMap.Create(nil);  usar para ver color de fuentes
          //AColorMap:=TStandardColorMap.Create(nil);
          AColorMap:=TXPColorMap.Create(nil);
          GlobalColorMap:=AColorMap;
          AColorMap.FontColor:=clBlack;
          //GenerateColorMap(AColorMap,clWebKhaki);
          //AColorMap:=TThemedColorMap.Create(nil);
          //AColorMap.Color:=clWebLemonChiffon;
          //AColorMap.Color:=clDkGray;
          //AColorMap.Color           :=clWebDarkSeaGreen;
          //AColorMap.Color           :=clWebSteelBlue;
          LoadSettings(AColorMap, Settings);
          GlobalSettings:=Settings;
          {$IF CompilerVersion >= 23}
          if Settings.UseVCLStyles then
          begin
            StyleFile:=IncludeTrailingPathDelimiter(Settings.VCLStylesPath)+Settings.VCLStyleName;
            //MessageDlg(StyleFile, mtInformation, [mbOK], 0);
            if FileExists(StyleFile) then
            begin
              TStyleManager.SetStyle(TStyleManager.LoadFromFile(StyleFile));
              GenerateColorMap(AColorMap,TStyleManager.ActiveStyle);
            end
            else
              MessageDlg(Format('The VCL Style file %s was not found',[StyleFile]), mtInformation, [mbOK], 0);
          end;
          {$IFEND}
          RefreshIDETheme(AColorMap);
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
  if Assigned(DelphiTheme) then
    DelphiTheme.Free;
  if Assigned(AColorMap) then
    AColorMap.Free;
end;

{
procedure GetComponentNames(lst: TStrings);
var
  i       : Integer;
  Packges : IOTAPackageServices;
begin
  Packges:= IOTAPackageServices(BorlandIDEServices);
  for i := 0 to Packges.PackageCount-1 do
   lst.Add(Packges.Package[i].FileName);
end;
}

{ TIDEWizard }

constructor TIDEWizard.Create;
begin
  inherited;
  {$WARN SYMBOL_PLATFORM OFF}
  ReportMemoryLeaksOnShutdown:=DebugHook<>0;
  {$WARN SYMBOL_PLATFORM ON}
  AColorMap:=nil;
  Settings:=TSettings.Create;
  //ColorizerForm := nil;
  RegisterPlugIn;
  //ShowMessage(GetBplLocation);
  AddMenuItems;
  InitColorizer();
end;


procedure TIDEWizard.AddMenuItems;
{
var
  MainMenu: TMainMenu;
  ToolsMenu: TMenuItem;
  I, InsertPosition: Integer;
  Image : TIcon;
}
begin
  inherited;
  {
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
    ExplorerSeparator := Menus.NewItem('-', 0, False, False, nil, 0, 'IdeClorSeparator');
    ToolsMenu.InsertComponent(ExplorerSeparator);
    ToolsMenu.Insert(InsertPosition, ExplorerSeparator);

    ExplorerItem := Menus.NewItem(sMenuItemIdeColorizer, 0, False, True, ExplorerItemClick, 0, 'IdeClorItem');


    Image:=TIcon.Create;
    try
     Image.Handle := LoadIcon(hInstance, sLogoIcon16);
     ExplorerItem.ImageIndex:=MainMenu.Images.AddIcon(Image);
    finally
      Image.Free;
    end;

    ToolsMenu.InsertComponent(ExplorerItem);
    ToolsMenu.Insert(InsertPosition+1, ExplorerItem);
  end;
  }
end;

procedure TIDEWizard.AfterSave;
begin
end;

procedure TIDEWizard.BeforeSave;
begin
end;


destructor TIDEWizard.Destroy;
begin
  Settings.Free;

  RemoveMenuItems;

  if Assigned(SplashBmp) then
    SplashBmp.Free;

  if Assigned(AboutBmp) then
    AboutBmp.Free;

  FinalizeColorizer();

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
begin
  //ColorizerForm := TFrmIDEColorizerSettings.Create(nil);
  //ColorizerForm.Name := 'DelphiIDEColorizer_SettingsForm';
  //ColorizerForm.ShowModal();
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

procedure TIDEWizard.RemoveMenuItems;
begin
{
  ExplorerItem.Free;
  ExplorerSeparator.Free;
}
end;

{$IFDEF Use_Notifiers}

procedure TToolbarIDENotifier.CreateButton(AOwner: TComponent;
  var Button: TControl; Action: TBasicAction);
begin

end;

procedure TToolbarIDENotifier.FilterAction(Action: TBasicAction;
  ViewingAllCommands: Boolean; var DisplayName: string; var Display,
  Handled: Boolean);
begin

end;

procedure TToolbarIDENotifier.FilterCategory(var Category: string; var Display,
  Handled: Boolean);
begin

end;

procedure TToolbarIDENotifier.ResetToolbar(var Toolbar: TWinControl);
begin
 if Assigned(Toolbar) then
   ShowMessage(Format('ResetToolbar Class %s Name %',[Toolbar.ClassName, Toolbar.Name]));
end;

procedure TToolbarIDENotifier.ShowToolbar(Toolbar: TWinControl; Show: Boolean);
begin
 if Assigned(Toolbar) then
   ShowMessage(Format('ShowToolbar Class %s Name %',[Toolbar.ClassName, Toolbar.Name]));
end;


procedure TToolbarIDENotifier.ToolbarModified(Toolbar: TWinControl);
begin
 if Assigned(Toolbar) then
   ShowMessage(Format('ToolbarModified Class %s Name %',[Toolbar.ClassName, Toolbar.Name]));
end;


procedure TToolbarIDEStreamNotifier.AfterSave(Toolbar: TWinControl);
begin

end;

procedure TToolbarIDEStreamNotifier.AfterSave;
begin

end;

procedure TToolbarIDEStreamNotifier.BeforeSave(Toolbar: TWinControl);
begin

end;

procedure TToolbarIDEStreamNotifier.BeforeSave;
begin

end;

procedure TToolbarIDEStreamNotifier.ToolbarLoaded(Toolbar: TWinControl);
begin
 if Assigned(Toolbar) then
   ShowMessage(Format('ToolbarLoaded Class %s Name %',[Toolbar.ClassName, Toolbar.Name]));

end;

{$ENDIF}

initialization


finalization
{$IFDEF Use_Notifiers}
  RemoveNotifier;
{$ENDIF}

end.


