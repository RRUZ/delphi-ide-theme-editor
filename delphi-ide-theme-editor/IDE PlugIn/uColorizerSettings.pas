{**************************************************************************************************}
{                                                                                                  }
{ Unit uColorizerSettings                                                                          }
{ unit uColorizerSettings  for the Delphi IDE Colorizer                                            }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is uColorizerSettings.pas.                                                     }
{                                                                                                  }
{ The Initial Developer of the Original Code is Rodrigo Ruz V.                                     }
{ Portions created by Rodrigo Ruz V. are Copyright (C) 2011-2012 Rodrigo Ruz V.                    }
{ All Rights Reserved.                                                                             }
{                                                                                                  }
{**************************************************************************************************}
unit uColorizerSettings;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, pngimage, ExtCtrls, StdCtrls, Grids, ComCtrls, ImgList,
  ActnMan, ActnColorMaps, uClrSettings, uDelphiVersions;

type
  TFrmIDEColorizerSettings = class(TForm)
    ImageIDELogo: TImage;
    CheckBoxEnabled: TCheckBox;
    cbThemeName: TComboBox;
    Label1: TLabel;
    Button3: TButton;
    CheckBoxMainMenu: TCheckBox;
    CheckBoxComponentsTabs: TCheckBox;
    CheckBoxCodeEditor: TCheckBox;
    Label3: TLabel;
    CheckBox7: TCheckBox;
    CheckBoxIDEDockWindows: TCheckBox;
    ListViewTypes: TListView;
    PageControlSettings: TPageControl;
    TabSheetMain: TTabSheet;
    TabSheet2: TTabSheet;
    PanelMain: TPanel;
    Label4: TLabel;
    ListViewProps: TListView;
    Label5: TLabel;
    ImageList1: TImageList;
    ColorMap: TXPColorMap;
    CbClrElement: TColorBox;
    BtnSelForColor: TButton;
    Label7: TLabel;
    Label6: TLabel;
    cbColorElements: TComboBox;
    CheckBoxAutoColor: TCheckBox;
    LabelSetting: TLabel;
    BtnInstall: TButton;
    BtnUnInstall: TButton;
    TabSheetVCLStyles: TTabSheet;
    CheckBoxUseVClStyles: TCheckBox;
    EditVCLStylesPath: TEdit;
    Label8: TLabel;
    BtnSelDir: TButton;
    CbStyles: TComboBox;
    Label9: TLabel;
    Panel1: TPanel;
    BtnCancel: TButton;
    BtnApply: TButton;
    TabSheet1: TTabSheet;
    CheckBoxFixIDEDrawIcon: TCheckBox;
    Image1: TImage;
    CheckBoxActivateDWM: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure ListViewTypesChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure FormDestroy(Sender: TObject);
    procedure BtnSelForColorClick(Sender: TObject);
    procedure BtnCancelClick(Sender: TObject);
    procedure cbColorElementsChange(Sender: TObject);
    procedure CbClrElementChange(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure cbThemeNameChange(Sender: TObject);
    procedure BtnApplyClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure BtnInstallClick(Sender: TObject);
    procedure BtnUnInstallClick(Sender: TObject);
    procedure BtnSelDirClick(Sender: TObject);
  private
    { Private declarations }
    FSettings: TSettings;
    FIDEData: TDelphiVersionData;
    procedure LoadColorElements;
    procedure LoadThemes;
    //procedure LoadTypes;
    //procedure LoadProperties(lType: TRttiType);
    procedure LoadSettings;
    function  GetIDEData: TDelphiVersionData;
    function  GetIDEThemesFolder : String;
    function  GetSettingsFolder : String;
    procedure LoadVClStylesList(const Path : string);
    procedure GenerateIDEThemes(const Path : string);
  public
    property IDEData   : TDelphiVersionData read GetIDEData write FIDEData;
    procedure Init;
  end;

const
  {$DEFINE DELPHI_OLDER_VERSIONS_SUPPORT}

  DelphiIDEThemePaths: array[TDelphiVersions] of string = (
  {$IFDEF DELPHI_OLDER_VERSIONS_SUPPORT}
    '',
    '',
  {$ENDIF}
    '',
    '',
    '',
    '',
    '',
    '',
    '',
    'BPL\XE',
    'BPL\XE2',
    '');

  DelphiIDEExpertsNames: array[TDelphiVersions] of string = (
  {$IFDEF DELPHI_OLDER_VERSIONS_SUPPORT}
    '',
    '',
  {$ENDIF}
    '',
    '',
    '',
    '',
    '',
    '',
    '',
    'DelphiIDEColorizerXE',
    'DelphiIDEColorizerXE2',
    'DelphiIDEColorizerXE3'
    );

implementation

Uses
 {$WARN UNIT_PLATFORM OFF}
 Vcl.FileCtrl,
 {$WARN UNIT_PLATFORM ON}
 System.Types,
 uMisc,
 IOUtils,
 System.UITypes,
 uStoreColorMap,
 GraphUtil,
 uIDEExpertUtils,
 uColorSelector,
 TypInfo;

{$R *.dfm}

procedure GenerateColorMap(AColorMap:TCustomActionBarColorMap;Color:TColor);
begin
{
+    property ShadowColor default cl3DDkShadow;
+    property Color default clBtnFace;
    property DisabledColor default clGray;
    property DisabledFontColor default clGrayText;
    property DisabledFontShadow default clBtnHighlight;
    property FontColor default clWindowText;
+    property HighlightColor;
    property HotColor default clDefault;
    property HotFontColor default clDefault;
    property MenuColor default clWindow;
+    property FrameTopLeftInner default clWhite;
+    property FrameTopLeftOuter default cXPFrameOuter;
+    property FrameBottomRightInner default clWhite;
+    property FrameBottomRightOuter default cXPFrameOuter;
+    property BtnFrameColor default cXPBtnFrameColor;
+    property BtnSelectedColor default clWhite;
+    property BtnSelectedFont default clWindowText;
+    property SelectedColor default cXPSelectedColor;
+    property SelectedFontColor default clBlack;
    property UnusedColor;
}
  AColorMap.Color                 :=Color;
  AColorMap.ShadowColor           :=GetShadowColor(Color);

  AColorMap.MenuColor             :=GetHighLightColor(Color);
  AColorMap.HighlightColor        :=GetHighLightColor(AColorMap.MenuColor);
  AColorMap.BtnSelectedColor      :=Color;
  AColorMap.BtnSelectedFont       :=AColorMap.FontColor;

  AColorMap.SelectedColor         :=GetHighLightColor(Color,50);
  AColorMap.SelectedFontColor     :=AColorMap.FontColor;

  AColorMap.BtnFrameColor         :=GetShadowColor(Color);
  AColorMap.FrameTopLeftInner     :=GetShadowColor(Color);
  AColorMap.FrameTopLeftOuter     :=AColorMap.FrameTopLeftInner;
  AColorMap.FrameBottomRightInner :=AColorMap.FrameTopLeftInner;
  AColorMap.FrameBottomRightOuter :=AColorMap.FrameTopLeftInner;
end;


procedure TFrmIDEColorizerSettings.BtnSelDirClick(Sender: TObject);
var
  Directory: string;
begin
  Directory:='';
  if SysUtils.DirectoryExists(EditVCLStylesPath.Text) then
    Directory := EditVCLStylesPath.Text;

  if SelectDirectory('Select directory',Directory,Directory,[sdNewFolder, sdNewUI, sdShowEdit, sdValidateDir, sdShowShares], nil) then
    EditVCLStylesPath.Text := Directory;
end;


procedure TFrmIDEColorizerSettings.BtnSelForColorClick(Sender: TObject);
Var
 AColor : TColor;
begin
 AColor:=DialogSelectColor(CbClrElement.Selected);
 if AColor<>clNone then
 begin
  CbClrElement.Selected:=AColor;
  CbClrElementChange(nil);
 end;
end;

procedure TFrmIDEColorizerSettings.BtnUnInstallClick(Sender: TObject);
begin
  if IsAppRunning(IDEData.Path) then
    MsgBox(Format('Before to continue you must close all running instances of the %s IDE', [IDEData.Name]))
  else
  if MessageDlg(Format('Do you want Uninstall the plugin (expert) in the %s IDE?', [IDEData.Name]), mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    if UnInstallExpert(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)))+DelphiIDEThemePaths[IDEData.Version]+'\'+DelphiIDEExpertsNames[IDEData.Version]+'.bpl', IDEData.Version) then
    begin
     MsgBox('PlugIn Uninstalled');
     BtnInstall.Enabled:=True;
     BtnUnInstall.Enabled:=False;
    end;
  end;
end;

procedure TFrmIDEColorizerSettings.BtnApplyClick(Sender: TObject);
begin
  if MessageDlg(Format('Do you want Apply the changes?', []), mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    FSettings.ThemeName := cbThemeName.Text;
    FSettings.Enabled   := CheckBoxEnabled.Checked;
    FSettings.EnableDWMColorization   := CheckBoxActivateDWM.Checked;
    FSettings.FixIDEDisabledIconsDraw   := CheckBoxFixIDEDrawIcon.Checked;
    FSettings.AutogenerateColors   := CheckBoxAutoColor.Checked;

    FSettings.UseVCLStyles :=CheckBoxUseVClStyles.Checked;
    FSettings.VCLStylesPath:=EditVCLStylesPath.Text;
    FSettings.VCLStyleName :=CbStyles.Text;
    WriteSettings(FSettings, GetSettingsFolder);
    Close();
  end;
end;

procedure TFrmIDEColorizerSettings.BtnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TFrmIDEColorizerSettings.BtnInstallClick(Sender: TObject);
begin
  if IsAppRunning(IDEData.Path) then
    MsgBox(Format('Before to continue you must close all running instances of the %s IDE',
      [IDEData.Name]))
  else
  if MessageDlg(Format('Do you want install the plugin (expert) in the %s IDE?', [IDEData.Name]), mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    if InstallExpert(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)))+DelphiIDEThemePaths[IDEData.Version]+'\'+DelphiIDEExpertsNames[IDEData.Version]+'.bpl','Delphi IDE Colorizer', IDEData.Version) then
    begin
     MsgBox('PlugIn Installed');
     BtnInstall.Enabled:=False;
     BtnUnInstall.Enabled:=True;
    end;
  end;
end;

procedure TFrmIDEColorizerSettings.Button3Click(Sender: TObject);
Var
  FileName : string;
begin
   if Trim(cbThemeName.Text)='' then
   begin
     MsgBox('The theme name is empty');
     exit;
   end;

   FileName:=IncludeTrailingPathDelimiter(GetIDEThemesFolder)+Trim(cbThemeName.Text)+'.idetheme';
   SaveColorMapToXmlFile(ColorMap,FileName);
   if FileExists(FileName) then
   begin
    MsgBox(Format('The theme %s was saved',[cbThemeName.Text]));
    LoadThemes;
   end;
end;


procedure TFrmIDEColorizerSettings.CbClrElementChange(Sender: TObject);
Var
 PropName : string;
 AColor   : TColor;
begin
 PropName:=cbColorElements.Text;
 AColor:=CbClrElement.Selected;
 SetOrdProp(ColorMap, PropName, AColor);
 if CheckBoxAutoColor.Checked then
   GenerateColorMap(ColorMap,ColorMap.Color);
end;

procedure TFrmIDEColorizerSettings.cbColorElementsChange(Sender: TObject);
Var
 PropName : string;
 AColor   : TColor;
begin
 PropName:=cbColorElements.Text;
 AColor:= GetOrdProp(ColorMap,PropName);
 CbClrElement.Selected:=AColor;
end;

procedure TFrmIDEColorizerSettings.cbThemeNameChange(Sender: TObject);
Var
  FileName : string;
  Bmp      : TBitmap;
begin
  FileName:=IncludeTrailingPathDelimiter(GetIDEThemesFolder)+cbThemeName.Text+'.idetheme';
  if FileExists(FileName)  then
  begin
    LoadColorMapFromXmlFile(ColorMap,FileName);
    cbColorElementsChange(nil);

    Bmp:=TBitmap.Create;
    try

     CreateArrayBitmap(275,25,[
      ColorMap.ShadowColor,
      ColorMap.Color,
      ColorMap.DisabledColor,
      ColorMap.DisabledFontColor,
      ColorMap.DisabledFontShadow,
      ColorMap.FontColor,
      ColorMap.HighlightColor,
      ColorMap.HotColor,
      ColorMap.HotFontColor,
      ColorMap.MenuColor,
      ColorMap.FrameTopLeftInner,
      ColorMap.FrameTopLeftOuter,
      ColorMap.FrameBottomRightInner,
      ColorMap.FrameBottomRightOuter,
      ColorMap.BtnFrameColor,
      ColorMap.BtnSelectedColor,
      ColorMap.BtnSelectedFont,
      ColorMap.SelectedColor,
      ColorMap.SelectedFontColor,
      ColorMap.UnusedColor
      ], Bmp);
     Image1.Picture.Assign(Bmp);
    finally
      Bmp.Free;
    end;


  end;
end;

procedure TFrmIDEColorizerSettings.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action:=caFree;
end;

procedure TFrmIDEColorizerSettings.FormCreate(Sender: TObject);
begin
  FSettings:=TSettings.Create;
  //CheckBoxActivateDWM.Enabled:=DwmIsEnabled;
end;

procedure TFrmIDEColorizerSettings.FormDestroy(Sender: TObject);
begin
  FSettings.Free;
end;

procedure TFrmIDEColorizerSettings.FormShow(Sender: TObject);
begin
  LabelSetting.Caption:=Format('Settings for %s',[IDEData.Name]);
  BtnInstall.Enabled  :=not ExpertInstalled(DelphiIDEExpertsNames[IDEData.Version]+'.bpl',IDEData.Version);
  BtnUnInstall.Enabled:=not BtnInstall.Enabled;
end;

procedure TFrmIDEColorizerSettings.GenerateIDEThemes(const Path: string);
Var
  i        : integer;
  FileName : string;
begin
   for i:=0 to WebNamedColorsCount-1 do
   begin
     GenerateColorMap(ColorMap,WebNamedColors[i].Value);
     FileName:=StringReplace(WebNamedColors[i].Name,'clWeb','',[rfReplaceAll]);
     FileName:=IncludeTrailingPathDelimiter(Path)+FileName+'.idetheme';
     SaveColorMapToXmlFile(ColorMap,FileName);
   end;
end;

function TFrmIDEColorizerSettings.GetIDEData: TDelphiVersionData;
begin
  Result := FIDEData;
end;

function TFrmIDEColorizerSettings.GetIDEThemesFolder: String;
begin
  Result:=IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)))+DelphiIDEThemePaths[IDEData.Version]+'\Themes';
end;

function TFrmIDEColorizerSettings.GetSettingsFolder: String;
begin
  Result:=IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)))+DelphiIDEThemePaths[IDEData.Version];
end;

procedure TFrmIDEColorizerSettings.Init;
begin
  TabSheetVCLStyles.TabVisible:=IDEData.Version=TDelphiVersions.DelphiXE2;
  LoadColorElements;
  LoadThemes;
  LoadSettings;
end;

procedure TFrmIDEColorizerSettings.ListViewTypesChange(Sender: TObject;
  Item: TListItem; Change: TItemChange);
begin
  //if ListViewTypes.Selected<>nil then
  //LoadProperties(TRttiType(ListViewTypes.Selected.Data));
end;

procedure TFrmIDEColorizerSettings.LoadColorElements;
var
  Count, Index: Integer;
  Properties  : TPropList;
begin
  Count := GetPropList(TypeInfo(TXPColorMap), tkAny, @Properties);
  cbColorElements.Items.BeginUpdate;
  try
    for Index := 0 to Pred(Count) do
     if SameText(string(Properties[Index]^.PropType^.Name),'TColor') then
      cbColorElements.Items.Add(string(Properties[Index]^.Name));
  finally
    cbColorElements.Items.EndUpdate;
  end;
  cbColorElements.ItemIndex:=cbColorElements.Items.IndexOf('Color');
end;
            {
procedure TFrmIDEColorizerSettings.LoadProperties(lType: TRttiType);
var
  p    : TRttiProperty;
  Item : TListItem;
  i    : Integer;
  Add  : Boolean;
begin
  ListViewProps.Items.BeginUpdate;
  try
    ListViewProps.Items.Clear;
    for p in lType.GetProperties do
    if p.IsWritable and ( (CompareText(p.PropertyType.Name,'TColor')=0) or (CompareText(p.PropertyType.Name,'TFont')=0) ) then// (Pos('Color',p.Name)>0) then
    begin
       Add:=True;
       for i := 0 to ListViewProps.Items.Count-1 do
       if CompareText(p.Name,ListViewProps.Items.Item[i].Caption)<>0 then
       begin
         Add:=False;
         Break;
       end;

       if Add then
       begin
        Item:=ListViewProps.Items.Add;
        Item.Caption:=p.Name;
       end;
    end;
  finally
   ListViewProps.Items.EndUpdate;
  end;
end;
         }
procedure TFrmIDEColorizerSettings.LoadSettings;
begin
  ReadSettings(FSettings, GetSettingsFolder);
  CheckBoxEnabled.Checked:=FSettings.Enabled;
  CheckBoxActivateDWM.Checked:=FSettings.EnableDWMColorization;
  CheckBoxFixIDEDrawIcon.Checked:=FSettings.FixIDEDisabledIconsDraw;
  CheckBoxAutoColor.Checked:=FSettings.AutogenerateColors;
  cbThemeName.Text:=FSettings.ThemeName;
  cbThemeNameChange(nil);

  CheckBoxUseVClStyles.Checked:=FSettings.UseVCLStyles;
  LoadVClStylesList(FSettings.VCLStylesPath);
  EditVCLStylesPath.Text:=FSettings.VCLStylesPath;
  CbStyles.ItemIndex:=CbStyles.Items.IndexOf(FSettings.VCLStyleName);
end;

procedure TFrmIDEColorizerSettings.LoadThemes;
var
 sValue, FileName : string;
 Files : TStringDynArray;
begin
  cbThemeName.Items.Clear;
  Files:=TDirectory.GetFiles(GetIDEThemesFolder,'*.idetheme');
  if Length(Files)=0 then
  begin
    GenerateIDEThemes(GetIDEThemesFolder);
    Files:=TDirectory.GetFiles(GetIDEThemesFolder,'*.idetheme');
  end;


  for sValue in Files do
  begin
    FileName:=ChangeFileExt(ExtractFileName(sValue),'');
    cbThemeName.Items.Add(FileName);
  end;
end;

procedure TFrmIDEColorizerSettings.LoadVClStylesList(const Path: string);
var
 sValue, FileName : string;
begin
  CbStyles.Items.Clear;
  if SysUtils.DirectoryExists(Path) then
  for sValue in TDirectory.GetFiles(ExcludeTrailingPathDelimiter(Path),'*.vsf') do
  begin
    FileName:=ExtractFileName(sValue);
    CbStyles.Items.Add(FileName);
  end;
end;

end.
