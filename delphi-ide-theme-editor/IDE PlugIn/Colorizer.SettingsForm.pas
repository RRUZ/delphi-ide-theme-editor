//**************************************************************************************************
//
// Unit Colorizer.SettingsForm
// unit Colorizer.SettingsForm  for the Delphi IDE Colorizer
//
// The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
// you may not use this file except in compliance with the License. You may obtain a copy of the
// License at http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
// ANY KIND, either express or implied. See the License for the specific language governing rights
// and limitations under the License.
//
// The Original Code is Colorizer.SettingsForm.pas.
//
// The Initial Developer of the Original Code is Rodrigo Ruz V.
// Portions created by Rodrigo Ruz V. are Copyright (C) 2011-2014 Rodrigo Ruz V.
// All Rights Reserved.
//
//**************************************************************************************************
unit Colorizer.SettingsForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, pngimage, ExtCtrls, StdCtrls, Grids, ComCtrls, ImgList,
  ActnMan, ActnColorMaps, Colorizer.Settings, uDelphiVersions{$IF CompilerVersion >= 23}, Vcl.Styles.Ext{$IFEND};

type
  TFormIDEColorizerSettings = class(TForm)
    CheckBoxEnabled: TCheckBox;
    cbThemeName: TComboBox;
    Label1: TLabel;
    Button3: TButton;
    ListViewTypes: TListView;
    PageControlSettings: TPageControl;
    TabSheetMain: TTabSheet;
    TabSheet2: TTabSheet;
    PanelMain: TPanel;
    Label4: TLabel;
    ListViewProps: TListView;
    Label5: TLabel;
    ImageList1: TImageList;
    XPColorMap: TXPColorMap;
    CbClrElement: TColorBox;
    BtnSelForColor: TButton;
    Label7: TLabel;
    Label6: TLabel;
    cbColorElements: TComboBox;
    CheckBoxAutoColor: TCheckBox;
    LabelSetting: TLabel;
    TabSheetVCLStyles: TTabSheet;
    CheckBoxUseVClStyles: TCheckBox;
    CbStyles: TComboBox;
    Label9: TLabel;
    Panel1: TPanel;
    BtnApply: TButton;
    CheckBoxFixIDEDrawIcon: TCheckBox;
    Image1: TImage;
    CheckBoxActivateDWM: TCheckBox;
    ColorDialog1: TColorDialog;
    Bevel1: TBevel;
    Label2: TLabel;
    CheckBoxGutterIcons: TCheckBox;
    PanelPreview: TPanel;
    Label18: TLabel;
    Label23: TLabel;
    ColorMapCombo: TComboBox;
    StyleCombo: TComboBox;
    TwilightColorMap: TTwilightColorMap;
    StandardColorMap: TStandardColorMap;
    Image2: TImage;
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
    procedure FormActivate(Sender: TObject);
    procedure CheckBoxGutterIconsClick(Sender: TObject);
    procedure CheckBoxEnabledClick(Sender: TObject);
    procedure CbStylesChange(Sender: TObject);
    procedure CheckBoxUseVClStylesClick(Sender: TObject);
  private
    { Private declarations }
    FPreview:TVclStylesPreview;
    FSettings: TSettings;
    FShowWarning : Boolean;
    procedure LoadColorElements;
    procedure LoadThemes;
    //procedure LoadProperties(lType: TRttiType);
    procedure LoadSettings;
    function  GetIDEThemesFolder : String;
    function  GetSettingsFolder : String;
    procedure LoadVClStylesList;
    procedure GenerateIDEThemes(const Path : string);
    procedure DrawSeletedVCLStyle;
  public
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
    '',
    '',
    '',
    '',
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
    'DelphiIDEColorizerXE3',
    'DelphiIDEColorizerXE4',
    'DelphiIDEColorizerXE5',
    '',
    '');

implementation

Uses
 {$IF CompilerVersion >= 23}
 VCL.Themes,
 VCL.Styles,
 {$IFEND}
 {$WARN UNIT_PLATFORM OFF}
 Vcl.FileCtrl,
 {$WARN UNIT_PLATFORM ON}
 System.Types,
 uMisc,
 IOUtils,
 System.UITypes,
 Colorizer.StoreColorMap,
 GraphUtil,
 uIDEExpertUtils,
 Colorizer.Utils,
 TypInfo;

{$R *.dfm}

{
TODO
  Enable / disable
}


//procedure TFormIDEColorizerSettings.BtnSelDirClick(Sender: TObject);
//var
//  Directory: string;
//begin
//  Directory:='';
//  if SysUtils.DirectoryExists(EditVCLStylesPath.Text) then
//    Directory := EditVCLStylesPath.Text;
//
//  if SelectDirectory('Select directory',Directory,Directory,[sdNewFolder, sdNewUI, sdShowEdit, sdValidateDir, sdShowShares], nil) then
//    EditVCLStylesPath.Text := Directory;
//end;


procedure TFormIDEColorizerSettings.BtnSelForColorClick(Sender: TObject);
Var
 LColor : TColor;
begin
 if ColorDialog1.Execute(Handle) then
 begin
   LColor:=ColorDialog1.Color;
   if LColor<>clNone then
   begin
    CbClrElement.Selected:=LColor;
    CbClrElementChange(nil);
   end;
 end;
end;

//procedure TFormIDEColorizerSettings.BtnUnInstallClick(Sender: TObject);
//begin
//  if IsAppRunning(IDEData.Path) then
//    MsgBox(Format('Before to continue you must close all running instances of the %s IDE', [IDEData.Name]))
//  else
//  if MessageDlg(Format('Do you want Uninstall the plugin (expert) in the %s IDE?', [IDEData.Name]), mtConfirmation, [mbYes, mbNo], 0) = mrYes then
//  begin
//    if UnInstallExpert(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)))+DelphiIDEThemePaths[IDEData.Version]+'\'+DelphiIDEExpertsNames[IDEData.Version]+'.bpl', IDEData.Version) then
//    begin
//     MsgBox('PlugIn Uninstalled');
//     BtnInstall.Enabled:=True;
//     BtnUnInstall.Enabled:=False;
//    end;
//  end;
//end;

procedure TFormIDEColorizerSettings.BtnApplyClick(Sender: TObject);
var
   sMessage, StyleFile : string;
begin
  if FShowWarning then
    sMessage:= Format('Do you want apply the changes?'+sLineBreak+
    'Note : some changes will only take effect the next time the IDE is started', [])
  else
    sMessage:= Format('Do you want apply the changes?', []);

  if MessageDlg(sMessage,  mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    FSettings.ThemeName := cbThemeName.Text;
    FSettings.Enabled   := CheckBoxEnabled.Checked;
    FSettings.EnableDWMColorization   := CheckBoxActivateDWM.Checked;
    FSettings.FixIDEDisabledIconsDraw   := CheckBoxFixIDEDrawIcon.Checked;
    FSettings.AutogenerateColors   := CheckBoxAutoColor.Checked;

    FSettings.UseVCLStyles :=CheckBoxUseVClStyles.Checked;
    FSettings.ChangeIconsGutter :=CheckBoxGutterIcons.Checked;
//    FSettings.ColorMapName      :=ColorMapCombo.Text;
//    FSettings.StyleBarName      :=StyleCombo.Text;
    FSettings.VCLStyleName :=CbStyles.Text;
    WriteSettings(FSettings, GetSettingsFolder);

    Colorizer.Utils.LoadSettings(TColorizerLocalSettings.ColorMap, TColorizerLocalSettings.ActionBarStyle, TColorizerLocalSettings.Settings);

    {$IF CompilerVersion >= 23}
    if TColorizerLocalSettings.Settings.UseVCLStyles then
    begin
      StyleFile:=IncludeTrailingPathDelimiter(TColorizerLocalSettings.VCLStylesPath)+TColorizerLocalSettings.Settings.VCLStyleName;
      if FileExists(StyleFile) then
      begin
        TStyleManager.SetStyle(TStyleManager.LoadFromFile(StyleFile));
        GenerateColorMap(TColorizerLocalSettings.ColorMap,TStyleManager.ActiveStyle);
      end
      else
        MessageDlg(Format('The VCL Style file %s was not found',[StyleFile]), mtInformation, [mbOK], 0);
    end;
    {$IFEND}

    RefreshIDETheme();
  end;
end;

procedure TFormIDEColorizerSettings.BtnCancelClick(Sender: TObject);
begin
end;

//procedure TFormIDEColorizerSettings.BtnInstallClick(Sender: TObject);
//begin
//  if IsAppRunning(IDEData.Path) then
//    MsgBox(Format('Before to continue you must close all running instances of the %s IDE',
//      [IDEData.Name]))
//  else
//  if MessageDlg(Format('Do you want install the plugin (expert) in the %s IDE?', [IDEData.Name]), mtConfirmation, [mbYes, mbNo], 0) = mrYes then
//  begin
//    if InstallExpert(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)))+DelphiIDEThemePaths[IDEData.Version]+'\'+DelphiIDEExpertsNames[IDEData.Version]+'.bpl','Delphi IDE Colorizer', IDEData.Version) then
//    begin
//     MsgBox('PlugIn Installed');
//     BtnInstall.Enabled:=False;
//     BtnUnInstall.Enabled:=True;
//    end;
//  end;
//end;

procedure TFormIDEColorizerSettings.Button3Click(Sender: TObject);
Var
  FileName : string;
begin
   if Trim(cbThemeName.Text)='' then
   begin
     MsgBox('The theme name is empty');
     exit;
   end;

//   FileName:=IncludeTrailingPathDelimiter(GetIDEThemesFolder)+'Twilight.idetheme';
//   SaveColorMapToXmlFile(TwilightColorMap, FileName);
//
//   FileName:=IncludeTrailingPathDelimiter(GetIDEThemesFolder)+'XPColorMap.idetheme';
//   SaveColorMapToXmlFile(XPColorMap, FileName);
//
//   FileName:=IncludeTrailingPathDelimiter(GetIDEThemesFolder)+'StandardColorMap.idetheme';
//   SaveColorMapToXmlFile(StandardColorMap, FileName);


   FileName:=IncludeTrailingPathDelimiter(GetIDEThemesFolder)+Trim(cbThemeName.Text)+'.idetheme';
   SaveColorMapToXmlFile(XPColorMap,FileName);
   if FileExists(FileName) then
   begin
    MsgBox(Format('The theme %s was saved',[cbThemeName.Text]));
    LoadThemes;
   end;
end;


procedure TFormIDEColorizerSettings.CbClrElementChange(Sender: TObject);
Var
 PropName : string;
 AColor   : TColor;
begin
 PropName:=cbColorElements.Text;
 AColor:=CbClrElement.Selected;
 SetOrdProp(XPColorMap, PropName, AColor);
 if CheckBoxAutoColor.Checked then
   GenerateColorMap(XPColorMap, XPColorMap.Color);
end;

procedure TFormIDEColorizerSettings.cbColorElementsChange(Sender: TObject);
Var
 PropName : string;
 AColor   : TColor;
begin
 PropName:=cbColorElements.Text;
 AColor:= GetOrdProp(XPColorMap,PropName);
 CbClrElement.Selected:=AColor;
end;

procedure TFormIDEColorizerSettings.CbStylesChange(Sender: TObject);
begin
   DrawSeletedVCLStyle;
end;

procedure TFormIDEColorizerSettings.cbThemeNameChange(Sender: TObject);
Var
  FileName : string;
  Bmp      : TBitmap;
begin
  FileName:=IncludeTrailingPathDelimiter(GetIDEThemesFolder)+cbThemeName.Text+'.idetheme';
  if FileExists(FileName)  then
  begin
    LoadColorMapFromXmlFile(XPColorMap,FileName);
    cbColorElementsChange(nil);

    Bmp:=TBitmap.Create;
    try

     CreateArrayBitmap(275,25,[
      XPColorMap.ShadowColor,
      XPColorMap.Color,
      XPColorMap.DisabledColor,
      XPColorMap.DisabledFontColor,
      XPColorMap.DisabledFontShadow,
      XPColorMap.FontColor,
      XPColorMap.HighlightColor,
      XPColorMap.HotColor,
      XPColorMap.HotFontColor,
      XPColorMap.MenuColor,
      XPColorMap.FrameTopLeftInner,
      XPColorMap.FrameTopLeftOuter,
      XPColorMap.FrameBottomRightInner,
      XPColorMap.FrameBottomRightOuter,
      XPColorMap.BtnFrameColor,
      XPColorMap.BtnSelectedColor,
      XPColorMap.BtnSelectedFont,
      XPColorMap.SelectedColor,
      XPColorMap.SelectedFontColor,
      XPColorMap.UnusedColor
      ], Bmp);
     Image1.Picture.Assign(Bmp);
    finally
      Bmp.Free;
    end;


  end;
end;

procedure TFormIDEColorizerSettings.CheckBoxEnabledClick(Sender: TObject);
begin
 FShowWarning :=True;
end;

procedure TFormIDEColorizerSettings.CheckBoxGutterIconsClick(Sender: TObject);
begin
 FShowWarning :=True;
end;

procedure TFormIDEColorizerSettings.CheckBoxUseVClStylesClick(Sender: TObject);
begin
 LoadVClStylesList;
end;

procedure TFormIDEColorizerSettings.FormActivate(Sender: TObject);
begin
  //TabSheetVCLStyles.TabVisible:=TColorizerLocalSettings.IDEData.Version=TDelphiVersions.DelphiXE2;
end;

procedure TFormIDEColorizerSettings.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action:=caFree;
end;

procedure TFormIDEColorizerSettings.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  ColorMapCombo.Items.AddObject('(Default)', nil);
  ColorMapCombo.ItemIndex := 0;
  for I := 0 to ComponentCount - 1 do
    if Components[I] is TCustomActionBarColorMap then
      ColorMapCombo.Items.AddObject(Components[I].Name, Components[I]);

  StyleCombo.Items.Assign(ActionBarStyles);
  FPreview:=TVclStylesPreview.Create(Self);
  FPreview.Parent:=PanelPreview;
  FPreview.BoundsRect := PanelPreview.ClientRect;
  FShowWarning:=False;
  FSettings:=TSettings.Create;
  //CheckBoxActivateDWM.Enabled:=DwmIsEnabled;
  {$IF CompilerVersion >= 23}
  TabSheetVCLStyles.TabVisible:=True;
  {$ELSE}
  TabSheetVCLStyles.TabVisible:=False;
  {$IFEND}
end;

type
  TVclStylesPreviewClass = class(TVclStylesPreview);

procedure TFormIDEColorizerSettings.DrawSeletedVCLStyle;
var
  StyleName : string;
  LStyle    : TCustomStyleServices;
begin
   StyleName:=CbStyles.Text;
   if (StyleName<>'') and (not SameText(StyleName, 'Windows')) then
   begin
     TStyleManager.StyleNames;//call DiscoverStyleResources
     LStyle:=TStyleManager.Style[StyleName];
     FPreview.Caption:=StyleName;
     FPreview.Style:=LStyle;
     TVclStylesPreviewClass(FPreview).Paint;
   end;
end;

              //  porque no creo un clon del IDE?
             //   obtener instancia form maoimn de bds (guardar en settins), util  pata iterar sobre menus y actionmanager
                                    

procedure TFormIDEColorizerSettings.FormDestroy(Sender: TObject);
begin
  FPreview.Free;
  FSettings.Free;
end;

procedure TFormIDEColorizerSettings.FormShow(Sender: TObject);
begin
  //LabelSetting.Caption:=Format('Settings for %s',[IDEData.Name]);
  //BtnInstall.Enabled  :=not ExpertInstalled(DelphiIDEExpertsNames[IDEData.Version]+'.bpl',IDEData.Version);
  //BtnUnInstall.Enabled:=not BtnInstall.Enabled;
end;

procedure TFormIDEColorizerSettings.GenerateIDEThemes(const Path: string);
Var
  i        : integer;
  FileName : string;
begin
   for i:=0 to WebNamedColorsCount-1 do
   begin
     GenerateColorMap(XPColorMap,WebNamedColors[i].Value);
     FileName:=StringReplace(WebNamedColors[i].Name,'clWeb','',[rfReplaceAll]);
     FileName:=IncludeTrailingPathDelimiter(Path)+FileName+'.idetheme';
     SaveColorMapToXmlFile(XPColorMap, FileName);
   end;
end;

function TFormIDEColorizerSettings.GetIDEThemesFolder: String;
begin
  Result:=IncludeTrailingPathDelimiter(GetSettingsFolder)+'Themes';
end;

function TFormIDEColorizerSettings.GetSettingsFolder: String;
begin
  Result:=ExtractFilePath(GeModuleLocation());
end;

procedure TFormIDEColorizerSettings.Init;
begin
  LoadColorElements;
  LoadThemes;
  LoadSettings;
end;

procedure TFormIDEColorizerSettings.ListViewTypesChange(Sender: TObject;
  Item: TListItem; Change: TItemChange);
begin
  //if ListViewTypes.Selected<>nil then
  //LoadProperties(TRttiType(ListViewTypes.Selected.Data));
end;

procedure TFormIDEColorizerSettings.LoadColorElements;
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
procedure TFormIDEColorizerSettings.LoadSettings;
begin
  ReadSettings(FSettings, GetSettingsFolder);
  CheckBoxEnabled.Checked:=FSettings.Enabled;
  CheckBoxActivateDWM.Checked:=FSettings.EnableDWMColorization;
  CheckBoxFixIDEDrawIcon.Checked:=FSettings.FixIDEDisabledIconsDraw;
  CheckBoxAutoColor.Checked:=FSettings.AutogenerateColors;
  CheckBoxGutterIcons.Checked:=FSettings.ChangeIconsGutter;
//  StyleCombo.ItemIndex:=StyleCombo.Items.IndexOf(FSettings.StyleBarName);
//  ColorMapCombo.ItemIndex:=ColorMapCombo.Items.IndexOf(FSettings.ColorMapName);
  cbThemeName.Text:=FSettings.ThemeName;
  cbThemeNameChange(nil);

  CheckBoxUseVClStyles.Checked:=FSettings.UseVCLStyles;
  LoadVClStylesList;
  //EditVCLStylesPath.Text:=FSettings.VCLStylesPath;
  CbStyles.ItemIndex:=CbStyles.Items.IndexOf(FSettings.VCLStyleName);
  DrawSeletedVCLStyle;
end;

procedure TFormIDEColorizerSettings.LoadThemes;
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


procedure TFormIDEColorizerSettings.LoadVClStylesList;
var
 s : string;
begin
 if CheckBoxUseVClStyles.Checked then
 begin
  CbStyles.Items.Clear;
  RegisterVClStylesFiles();
   for s in TStyleManager.StyleNames do
    CbStyles.Items.Add(s);

   CbStyles.ItemIndex:=CbStyles.Items.IndexOf(FSettings.VCLStyleName);
 end;
end;

end.
