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
// Portions created by Rodrigo Ruz V. are Copyright (C) 2011-2015 Rodrigo Ruz V.
// All Rights Reserved.
//
//**************************************************************************************************
unit Colorizer.SettingsForm;

interface
{$I ..\Common\Jedi.inc}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, pngimage, ExtCtrls, StdCtrls, Grids, ComCtrls, ImgList,
  ActnMan, ActnColorMaps, Colorizer.Settings, uDelphiVersions{$IF CompilerVersion >= 23}, Vcl.Styles.Ext{$IFEND}
  ,Colorizer.XPStyleActnCtrls;

type
  TColorListBox = class(ExtCtrls.TColorListBox)
  private
    FItemIndex: Integer;
    FOnChange: TNotifyEvent;
    procedure CNCommand(var AMessage: TWMCommand); message CN_COMMAND;
  protected
    procedure Change; virtual;
    procedure SetItemIndex(const Value: Integer); override;
  published
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TFormIDEColorizerSettings = class(TForm)
    CheckBoxEnabled: TCheckBox;
    cbThemeName: TComboBox;
    Label1: TLabel;
    ButtonSaveTheme: TButton;
    PageControlSettings: TPageControl;
    TabSheetMain: TTabSheet;
    PanelMain: TPanel;
    ImageList1: TImageList;
    CbClrElement: TColorBox;
    BtnSelForColor: TButton;
    Label7: TLabel;
    Label6: TLabel;
    CheckBoxAutoColor: TCheckBox;
    LabelSetting: TLabel;
    TabSheetVCLStyles: TTabSheet;
    CheckBoxUseVClStyles: TCheckBox;
    CbStyles: TComboBox;
    Label9: TLabel;
    Panel1: TPanel;
    BtnApply: TButton;
    CheckBoxFixIDEDrawIcon: TCheckBox;
    ImagePalette: TImage;
    Bevel1: TBevel;
    Label2: TLabel;
    CheckBoxGutterIcons: TCheckBox;
    PanelPreview: TPanel;
    ImageStyle: TImage;
    Button1: TButton;
    ColorBoxBase: TColorBox;
    Label3: TLabel;
    TabSheetHookedForms: TTabSheet;
    ListBoxFormsHooked: TListBox;
    EditFormClass: TEdit;
    Label8: TLabel;
    ButtonAddFormClass: TButton;
    ButtonRemoveFormClass: TButton;
    EditThemeName: TEdit;
    Label10: TLabel;
    ImageListDock: TImageList;
    ColorListBox1: TColorListBox;
    TabSheetDockOptions: TTabSheet;
    Label11: TLabel;
    ListBoxDockImages: TListBox;
    RbtnDockGradientHorz: TRadioButton;
    RbtnDockGradientVert: TRadioButton;
    CheckBoxCustomDockBars: TCheckBox;
    CheckBoxUseCustomColorsDock: TCheckBox;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    ColorBoxDockStartGradientActive: TColorBox;
    ColorBoxDockEndGradientActive: TColorBox;
    ColorBoxDockStartGradientInActive: TColorBox;
    ColorBoxDockEndGradientInActive: TColorBox;
    Button2: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    TabSheetAbout: TTabSheet;
    MemoThirdParty: TMemo;
    ButtonReportIssues: TButton;
    ButtonProjectPage: TButton;
    ButtonCheckUpdates: TButton;
    ButtonDeleteTheme: TButton;
    CheckBoxHookSystemColors: TCheckBox;
    TabSheetToolBars: TTabSheet;
    RbtnToolBarGradientVert: TRadioButton;
    RbtnToolBarGradientHorz: TRadioButton;
    Label4: TLabel;
    CheckBoxUseCustomColorsToolbar: TCheckBox;
    Label5: TLabel;
    ColorBoxToolBarStartGrad: TColorBox;
    Button3: TButton;
    Button7: TButton;
    ColorBoxToolBarStartEnd: TColorBox;
    Label17: TLabel;
    CheckBoxUpdates: TCheckBox;
    Label18: TLabel;
    ColorBoxDockFontActive: TColorBox;
    Button8: TButton;
    Label19: TLabel;
    ColorBoxDockFontInActive: TColorBox;
    Button9: TButton;
    Label20: TLabel;
    Label21: TLabel;
    ColorBoxDockBorderInActive: TColorBox;
    ColorBoxDockBorderActive: TColorBox;
    Button10: TButton;
    Button11: TButton;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    RbtnDockBorderRounded: TRadioButton;
    RbtnDockBorderRectangle: TRadioButton;
    TabSheetIDETabs: TTabSheet;
    Button12: TButton;
    Button13: TButton;
    Button14: TButton;
    Button15: TButton;
    ColorBoxIDETabEndGradientInActive: TColorBox;
    ColorBoxIDETabStartGradientInActive: TColorBox;
    ColorBoxIDETabEndGradientActive: TColorBox;
    ColorBoxIDETabStartGradientActive: TColorBox;
    Label12: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    CheckBoxIDETabsCustom: TCheckBox;
    CheckBoxIDETabsOutLine: TCheckBox;
    ColorBoxIDETabOutLineColor: TColorBox;
    ColorBoxIDETabFontActive: TColorBox;
    Button16: TButton;
    Button17: TButton;
    Label25: TLabel;
    Label26: TLabel;
    TabSheetHeaders: TTabSheet;
    CheckBoxCustomHeader: TCheckBox;
    Label27: TLabel;
    Label28: TLabel;
    Label31: TLabel;
    Label32: TLabel;
    Button20: TButton;
    Button21: TButton;
    ColorBoxHeaderEndGradient: TColorBox;
    ColorBoxHeaderStartGradient: TColorBox;
    ColorBoxHeaderBorderColor: TColorBox;
    ColorBoxHeaderFontColor: TColorBox;
    Button22: TButton;
    Button23: TButton;
    CheckBoxVCLStylesForms: TCheckBox;
    CheckBoxVCLStylesMenusColors: TCheckBox;
    CheckBoxVCLStylesScrollBars: TCheckBox;
    CheckBoxVCLStylesControls: TCheckBox;
    Bevel2: TBevel;
    LabelVersion: TLabel;
    LabelVersion2: TLabel;
    LabelVersion3: TLabel;
    LinkLabel1: TLinkLabel;
    ImageTwitter: TImage;
    ImageGooglePlus: TImage;
    Label29: TLabel;
    ImageLogo: TImage;
    ImageVCLStyles: TImage;
    PageControl2: TPageControl;
    TabSheetTheme: TTabSheet;
    Panel3: TPanel;
    ImagePin: TImage;
    CheckBoxTransparentMenus: TCheckBox;
    Level: TLabel;
    EditMenuTransValue: TEdit;
    UpDownMenu: TUpDown;
    TabSheetMenu: TTabSheet;
    TabSheetContribute: TTabSheet;
    ImageContribute: TImage;
    Label30: TLabel;
    TabSheetFonts: TTabSheet;
    cbVirtualStringTreeFont: TComboBox;
    Label33: TLabel;
    cbVirtualStringTreeFontSize: TComboBox;
    CheckboxVirtualStringTreeFontDefault: TCheckBox;
    cbVirtualStringTree: TComboBox;
    Label34: TLabel;
    Label35: TLabel;
    Label36: TLabel;
    BtnApplyVirtualTreeFont: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ListViewTypesChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure FormDestroy(Sender: TObject);
    procedure BtnSelForColorClick(Sender: TObject);
    procedure CbClrElementChange(Sender: TObject);
    procedure ButtonSaveThemeClick(Sender: TObject);
    procedure cbThemeNameChange(Sender: TObject);
    procedure BtnApplyClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure CbStylesChange(Sender: TObject);
    procedure CheckBoxUseVClStylesClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure ColorBoxBaseChange(Sender: TObject);
    procedure ColorBoxBaseGetColors(Sender: TCustomColorBox; Items: TStrings);
    procedure ButtonAddFormClassClick(Sender: TObject);
    procedure ButtonRemoveFormClassClick(Sender: TObject);
    procedure ColorListBox1GetColors(Sender: TCustomColorListBox;
      Items: TStrings);
    procedure ListBoxDockImagesMeasureItem(Control: TWinControl; Index: Integer;
      var Height: Integer);
    procedure ListBoxDockImagesDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure Button2Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure ButtonReportIssuesClick(Sender: TObject);
    procedure ButtonProjectPageClick(Sender: TObject);
    procedure ButtonDeleteThemeClick(Sender: TObject);
    procedure ButtonCheckUpdatesClick(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure Button10Click(Sender: TObject);
    procedure Button11Click(Sender: TObject);
    procedure Button15Click(Sender: TObject);
    procedure Button14Click(Sender: TObject);
    procedure Button13Click(Sender: TObject);
    procedure Button12Click(Sender: TObject);
    procedure Button16Click(Sender: TObject);
    procedure Button17Click(Sender: TObject);
    procedure Button21Click(Sender: TObject);
    procedure Button20Click(Sender: TObject);
    procedure Button22Click(Sender: TObject);
    procedure Button23Click(Sender: TObject);
    procedure ImageGooglePlusClick(Sender: TObject);
    procedure ImageTwitterClick(Sender: TObject);
    procedure ImageContributeClick(Sender: TObject);
    procedure cbVirtualStringTreeFontDrawItem(Control: TWinControl;
      Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure BtnApplyVirtualTreeFontClick(Sender: TObject);
    procedure cbVirtualStringTreeChange(Sender: TObject);
  private
    { Private declarations }
{$IFDEF DELPHIXE2_UP}
    FPreview:TVclStylesPreview;
{$ENDIF}
    FSettings: TSettings;
    FColorListItems: TStrings;
    procedure FillColorsItems;
    procedure ColorListChange(Sender: TObject);
    procedure LoadThemes;
    //procedure LoadProperties(lType: TRttiType);
    procedure LoadSettings;
    function  GetIDEThemesFolder : String;
    function  GetSettingsFolder : String;
    procedure GenerateIDEThemes(const Path : string);
{$IFDEF DELPHIXE2_UP}
    procedure DrawSeletedVCLStyle;
    procedure LoadVClStylesList;
{$ENDIF}
    procedure DrawPalette;
    procedure LoadDockIcons;
    procedure LoadColorsTheme;
  public
    procedure Init;
  end;


implementation

Uses
 {$IF CompilerVersion >= 23}
 VCL.Themes,
 VCL.Styles,
 System.UITypes,
 Colorizer.Vcl.Styles,
 Vcl.PlatformDefaultStyleActnCtrls,
 {$IFEND}
 {$WARN UNIT_PLATFORM OFF}
 FileCtrl,
 {$WARN UNIT_PLATFORM ON}
 Types,
 Main,
 uMisc,
 ShellApi,
 IOUtils,
 Colorizer.StoreColorMap,
 GraphUtil,
 CommCtrl,
 Colorizer.Utils,
 Colorizer.Hook.Forms,
 uColorSelector,
 TypInfo;

{$R *.dfm}
var
  ColorizerColorMap : TColorizerColorMap;



function CalculateTextColor(const BackgroundColor: TColor): TColor;
begin
  if (GetRValue(BackgroundColor) + GetGValue(BackgroundColor) + GetBValue(BackgroundColor)) > 384 then
    result := clBlack
  else
    result := clWhite;
end;

procedure TFormIDEColorizerSettings.BtnApplyVirtualTreeFontClick(
  Sender: TObject);
begin
 if (cbVirtualStringTree.Text<>'') and (FSettings.VirtualStringTreeFontSettingsDict.ContainsKey(cbVirtualStringTree.Text)) then
 begin
   FSettings.VirtualStringTreeFontSettingsDict.Items[cbVirtualStringTree.Text].FontName:=cbVirtualStringTreeFont.Text;
   FSettings.VirtualStringTreeFontSettingsDict.Items[cbVirtualStringTree.Text].Size    :=StrToInt(cbVirtualStringTreeFontSize.Text);
 end;
end;

procedure TFormIDEColorizerSettings.BtnSelForColorClick(Sender: TObject);
Var
 LColor : TColor;
begin
 LColor := DialogSelectColor(CbClrElement.Selected);
 if LColor<>clNone then
 begin
    CbClrElement.Selected:=LColor;
    CbClrElementChange(nil);
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
  {$IFDEF DELPHIXE2_UP}OrgVclStyleName, {$ENDIF}s, ImagesPath, sMessage : string;
  {$IFDEF DELPHIXE2_UP}OrgVclStyleForms, OrgVclStyle, {$ENDIF}FShowWarning : Boolean;
begin
{$IFDEF DELPHIXE2_UP}
  OrgVclStyleName :=FSettings.VCLStyleName;
  OrgVclStyle     :=FSettings.UseVCLStyles;
  OrgVclStyleForms:=FSettings.VCLStylesForms;

  FShowWarning:=(not CheckBoxUseVClStyles.Checked and FSettings.UseVCLStyles)
  or (not CheckBoxEnabled.Checked and FSettings.UseVCLStyles and FSettings.Enabled)
  or (CheckBoxUseVClStyles.Checked and FSettings.VCLStylesForms and not CheckBoxVCLStylesForms.Checked);

  sMessage:= Format('Disabling the VCL Styles (or the expert) while the RAD Studio IDE is running may cause which some of the windows will'+sLineBreak+
  'loose the aero and glass effect on the non client area. You will need restart the IDE in order to get the native aero look and feel back on the IDE'+sLineBreak+sLineBreak+
  'Do you want to continue ?', []);
  if FShowWarning and (MessageDlg(sMessage,  mtWarning, [mbYes, mbNo], 0) <> mrYes) then
    exit;
{$ELSE}
  FShowWarning:=False;
{$ENDIF}


  FShowWarning:=(not FShowWarning) and (CheckBoxEnabled.Checked <> FSettings.Enabled) or (CheckBoxGutterIcons.Checked <> FSettings.ChangeIconsGutter) or (CheckBoxHookSystemColors.Checked <> FSettings.HookSystemColors);

  if FShowWarning then
    sMessage:= Format('Do you want apply the changes?'+sLineBreak+
    'Note : some changes will only take effect the next time the IDE is started', [])
  else
    sMessage:= Format('Do you want apply the changes?', []);

  if MessageDlg(sMessage,  mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin

//    if Parent=nil then
//      Close();
    //AddLog('Close', 'Close');

    FSettings.ThemeName := cbThemeName.Text;
    FSettings.Enabled   := CheckBoxEnabled.Checked;
    //FSettings.EnableDWMColorization   := CheckBoxActivateDWM.Checked;
    FSettings.FixIDEDisabledIconsDraw   := CheckBoxFixIDEDrawIcon.Checked;
    FSettings.AutogenerateColors   := CheckBoxAutoColor.Checked;
    FSettings.CheckUpdates  := CheckBoxUpdates.Checked;

    FSettings.VCLStyleName  := CbStyles.Text;
    FSettings.UseVCLStyles  := CheckBoxUseVClStyles.Checked;
    FSettings.VCLStylesForms:= CheckBoxVCLStylesForms.Checked;
    FSettings.VCLStylesMenusColors:= CheckBoxVCLStylesMenusColors.Checked;
    FSettings.VCLStylesScrollBars := CheckBoxVCLStylesScrollBars.Checked;
    FSettings.VCLStylesControls   := CheckBoxVCLStylesControls.Checked;

    FSettings.ChangeIconsGutter :=CheckBoxGutterIcons.Checked;
//    FSettings.ColorMapName      :=ColorMapCombo.Text;
//    FSettings.StyleBarName      :=StyleCombo.Text;
    FSettings.DockImages   := ListBoxDockImages.Items[ListBoxDockImages.ItemIndex];

    FSettings.DockGradientHor:= RbtnDockGradientHorz.Checked;
    FSettings.DockCustom       := CheckBoxCustomDockBars.Checked;
    FSettings.DockCustomColors := CheckBoxUseCustomColorsDock.Checked;
    FSettings.DockStartGradActive   := ColorToString(ColorBoxDockStartGradientActive.Selected);
    FSettings.DockEndGradActive     := ColorToString(ColorBoxDockEndGradientActive.Selected);
    FSettings.DockStartGradInActive := ColorToString(ColorBoxDockStartGradientInActive.Selected);
    FSettings.DockEndGradInActive   := ColorToString(ColorBoxDockEndGradientInActive.Selected);
    FSettings.DockActiveFontColor   := ColorToString(ColorBoxDockFontActive.Selected);
    FSettings.DockInActiveFontColor := ColorToString(ColorBoxDockFontInActive.Selected);
    FSettings.DockActiveBorderColor   := ColorToString(ColorBoxDockBorderActive.Selected);
    FSettings.DockInActiveBorderColor := ColorToString(ColorBoxDockBorderInActive.Selected);
    FSettings.DockBorderRounded       := RbtnDockBorderRounded.Checked;

    FSettings.ToolbarGradientHor    := RbtnToolBarGradientHorz.Checked;
    FSettings.ToolbarCustomColors   := CheckBoxUseCustomColorsToolbar.Checked;
    FSettings.ToolbarStartGrad      := ColorToString(ColorBoxToolBarStartGrad.Selected);
    FSettings.ToolbarEndGrad        := ColorToString(ColorBoxToolBarStartEnd.Selected);

    FSettings.HookSystemColors      := CheckBoxHookSystemColors.Checked;

    FSettings.TabIDECustom            := CheckBoxIDETabsCustom.Checked;
    FSettings.TabIDEOutLine           := CheckBoxIDETabsOutLine.Checked;
    FSettings.TabIDEStartGradActive   := ColorToString(ColorBoxIDETabStartGradientActive.Selected);
    FSettings.TabIDEEndGradActive     := ColorToString(ColorBoxIDETabEndGradientActive.Selected);
    FSettings.TabIDEStartGradInActive := ColorToString(ColorBoxIDETabStartGradientInActive.Selected);
    FSettings.TabIDEEndGradInActive   := ColorToString(ColorBoxIDETabEndGradientInActive.Selected);
    FSettings.TabIDEActiveFontColor   := ColorToString(ColorBoxIDETabFontActive.Selected);
    FSettings.TabIDEOutLineColor      := ColorToString(ColorBoxIDETabOutLineColor.Selected);

    FSettings.HeaderCustom          := CheckBoxCustomHeader.Checked;
    FSettings.HeaderStartGrad       := ColorToString(ColorBoxHeaderStartGradient.Selected);
    FSettings.HeaderEndGrad         := ColorToString(ColorBoxHeaderEndGradient.Selected);
    FSettings.HeaderFontColor       := ColorToString(ColorBoxHeaderFontColor.Selected);
    FSettings.HeaderBorderColor     := ColorToString(ColorBoxHeaderBorderColor.Selected);

    FSettings.MenuTransparent       := CheckBoxTransparentMenus.Checked;
    FSettings.MenuTransLevel        := UpDownMenu.Position;

//    FSettings.VirtualStringTreeFont:= cbVirtualStringTreeFont.Text;
//    if cbVirtualStringTreeFontSize.Text<>'' then
//     FSettings.VirtualStringTreeFontSize:=  StrToInt(cbVirtualStringTreeFontSize.Text);
    FSettings.VirtualStringTreeFontDefault:= CheckboxVirtualStringTreeFontDefault.Checked;

    WriteSettings(FSettings, GetSettingsFolder);

    ImagesPath:=ExtractFilePath(GetModuleLocation)+'images\dock_images';
    s:=IncludeTrailingPathDelimiter(ImagesPath)+FSettings.DockImages+'.png';
    if FileExists(s) then
      TColorizerLocalSettings.DockImages.LoadFromFile(s);


    //ListBoxFormsHooked.Items.SaveToFile(IncludeTrailingPathDelimiter(ExtractFilePath(GetModuleLocation))+'HookedWindows.dat');

    {$IFDEF DELPHIXE2_UP}
    if CheckBoxUseVClStyles.Checked and CheckBoxVCLStylesMenusColors.Checked then
     TColorizerLocalSettings.ActionBarStyle := PlatformDefaultStyle
    else
     TColorizerLocalSettings.ActionBarStyle := ColorXPStyle;
    {$ENDIF}

    Colorizer.Utils.LoadSettings(TColorizerLocalSettings.ColorMap, TColorizerLocalSettings.Settings);
    {$IFDEF DELPHIXE2_UP}
    if TColorizerLocalSettings.Settings.UseVCLStyles then
    begin

     if (TColorizerLocalSettings.Settings.VCLStylesForms<>OrgVclStyleForms) or (TColorizerLocalSettings.Settings.UseVCLStyles<>OrgVclStyle) or (TColorizerLocalSettings.Settings.VCLStyleName<>OrgVclStyleName) then
     begin
       SetColorizerVCLStyle(TColorizerLocalSettings.Settings.VCLStyleName);
       //reload colors again
       Colorizer.Utils.LoadSettings(TColorizerLocalSettings.ColorMap, TColorizerLocalSettings.Settings);
       RefreshColorizerVCLStyle();
     end;
    end
    else
    begin
     if (TColorizerLocalSettings.Settings.UseVCLStyles<>OrgVclStyle) then
     begin
       SetColorizerVCLStyle(TColorizerLocalSettings.Settings.VCLStyleName);
       //reload colors again
       Colorizer.Utils.LoadSettings(TColorizerLocalSettings.ColorMap, TColorizerLocalSettings.Settings);
       RefreshColorizerVCLStyle();
     end;
    end;
    {$ENDIF}

    if FSettings.Enabled then
      RefreshIDETheme(True)
    else
    begin
    {$IFDEF DELPHIXE2_UP}
//     for i := 0 to Screen.FormCount - 1 do
//      if Screen.Forms[i].HandleAllocated then
//        if IsWindowVisible(Screen.Forms[I].Handle) then
//          PostMessage(Screen.Forms[i].Handle, CM_CUSTOMSTYLECHANGED, 0, 0)
//        else
//          SendMessage(Screen.Forms[i].Handle, CM_CUSTOMSTYLECHANGED, 0, 0);
    {$ENDIF}

      RestoreIDESettings();
    end;
  end;
end;

procedure TFormIDEColorizerSettings.Button10Click(Sender: TObject);
Var
 LColor : TColor;
begin
 LColor := DialogSelectColor(ColorBoxDockBorderActive.Selected);
 if LColor<>clNone then
    ColorBoxDockBorderActive.Selected:=LColor;
end;


procedure TFormIDEColorizerSettings.Button11Click(Sender: TObject);
Var
 LColor : TColor;
begin
 LColor := DialogSelectColor(ColorBoxDockBorderInActive.Selected);
 if LColor<>clNone then
    ColorBoxDockBorderInActive.Selected:=LColor;
end;

procedure TFormIDEColorizerSettings.Button12Click(Sender: TObject);
Var
 LColor : TColor;
begin
 LColor := DialogSelectColor(ColorBoxIDETabEndGradientInActive.Selected);
 if LColor<>clNone then
    ColorBoxIDETabEndGradientInActive.Selected:=LColor;
end;



procedure TFormIDEColorizerSettings.Button13Click(Sender: TObject);
Var
 LColor : TColor;
begin
 LColor := DialogSelectColor(ColorBoxIDETabStartGradientInActive.Selected);
 if LColor<>clNone then
    ColorBoxIDETabStartGradientInActive.Selected:=LColor;
end;


procedure TFormIDEColorizerSettings.Button14Click(Sender: TObject);
Var
 LColor : TColor;
begin
 LColor := DialogSelectColor(ColorBoxIDETabEndGradientActive.Selected);
 if LColor<>clNone then
    ColorBoxIDETabEndGradientActive.Selected:=LColor;
end;


procedure TFormIDEColorizerSettings.Button15Click(Sender: TObject);
Var
 LColor : TColor;
begin
 LColor := DialogSelectColor(ColorBoxIDETabStartGradientActive.Selected);
 if LColor<>clNone then
    ColorBoxIDETabStartGradientActive.Selected:=LColor;
end;

procedure TFormIDEColorizerSettings.Button16Click(Sender: TObject);
Var
 LColor : TColor;
begin
 LColor := DialogSelectColor(ColorBoxIDETabFontActive.Selected);
 if LColor<>clNone then
    ColorBoxIDETabFontActive.Selected:=LColor;
end;

procedure TFormIDEColorizerSettings.Button17Click(Sender: TObject);
Var
 LColor : TColor;
begin
 LColor := DialogSelectColor(ColorBoxIDETabOutLineColor.Selected);
 if LColor<>clNone then
    ColorBoxIDETabOutLineColor.Selected:=LColor;
end;

procedure TFormIDEColorizerSettings.Button1Click(Sender: TObject);
Var
 LColor : TColor;
begin
 LColor := DialogSelectColor(ColorBoxBase.Selected);
 if LColor<>clNone then
 begin
    ColorBoxBase.Selected:=LColor;
    ColorBoxBaseChange(nil);
 end;
end;

procedure TFormIDEColorizerSettings.Button20Click(Sender: TObject);
Var
 LColor : TColor;
begin
 LColor := DialogSelectColor(ColorBoxHeaderEndGradient.Selected);
 if LColor<>clNone then
    ColorBoxHeaderEndGradient.Selected:=LColor;
end;

procedure TFormIDEColorizerSettings.Button21Click(Sender: TObject);
Var
 LColor : TColor;
begin
 LColor := DialogSelectColor(ColorBoxHeaderStartGradient.Selected);
 if LColor<>clNone then
    ColorBoxHeaderStartGradient.Selected:=LColor;
end;

procedure TFormIDEColorizerSettings.Button22Click(Sender: TObject);
Var
 LColor : TColor;
begin
 LColor := DialogSelectColor(ColorBoxHeaderFontColor.Selected);
 if LColor<>clNone then
    ColorBoxHeaderFontColor.Selected:=LColor;
end;

procedure TFormIDEColorizerSettings.Button23Click(Sender: TObject);
Var
 LColor : TColor;
begin
 LColor := DialogSelectColor(ColorBoxHeaderBorderColor.Selected);
 if LColor<>clNone then
    ColorBoxHeaderBorderColor.Selected:=LColor;
end;

procedure TFormIDEColorizerSettings.Button2Click(Sender: TObject);
Var
 LColor : TColor;
begin
 LColor := DialogSelectColor(ColorBoxDockStartGradientActive.Selected);
 if LColor<>clNone then
    ColorBoxDockStartGradientActive.Selected:=LColor;
end;

procedure TFormIDEColorizerSettings.Button3Click(Sender: TObject);
Var
 LColor : TColor;
begin
 LColor := DialogSelectColor(ColorBoxToolBarStartGrad.Selected);
 if LColor<>clNone then
    ColorBoxToolBarStartGrad.Selected:=LColor;
end;

procedure TFormIDEColorizerSettings.ButtonSaveThemeClick(Sender: TObject);
Var
  ThemeName, FileName : string;
begin
   ThemeName:= Trim(EditThemeName.Text);
   if ThemeName='' then
   begin
     MsgBox('The theme name is empty');
     exit;
   end;

   FileName:=IncludeTrailingPathDelimiter(GetIDEThemesFolder)+ThemeName+'.idetheme';
   if FileExists(FileName) then
      if Application.MessageBox(PChar(Format('The theme %s already exists, Do you want overwritte the theme ?',[ThemeName])), 'Comfirmation',
        MB_YESNO + MB_ICONQUESTION) = idNo then
        exit;


   SaveColorMapToXmlFile(ColorizerColorMap, FileName);
   if FileExists(FileName) then
   begin
    MsgBox(Format('The theme %s was saved',[ThemeName]));
    LoadThemes;
    cbThemeName.ItemIndex:=cbThemeName.Items.IndexOf(ThemeName);
    DrawPalette();
   end;
end;


procedure TFormIDEColorizerSettings.Button4Click(Sender: TObject);
Var
 LColor : TColor;
begin
 LColor := DialogSelectColor(ColorBoxDockEndGradientActive.Selected);
 if LColor<>clNone then
    ColorBoxDockEndGradientActive.Selected:=LColor;
end;


procedure TFormIDEColorizerSettings.Button5Click(Sender: TObject);
Var
 LColor : TColor;
begin
 LColor := DialogSelectColor(ColorBoxDockStartGradientInActive.Selected);
 if LColor<>clNone then
    ColorBoxDockStartGradientInActive.Selected:=LColor;
end;

procedure TFormIDEColorizerSettings.Button6Click(Sender: TObject);
Var
 LColor : TColor;
begin
 LColor := DialogSelectColor(ColorBoxDockEndGradientInActive.Selected);
 if LColor<>clNone then
    ColorBoxDockEndGradientInActive.Selected:=LColor;
end;


procedure TFormIDEColorizerSettings.Button7Click(Sender: TObject);
Var
 LColor : TColor;
begin
 LColor := DialogSelectColor(ColorBoxToolBarStartEnd.Selected);
 if LColor<>clNone then
    ColorBoxToolBarStartEnd.Selected:=LColor;
end;

procedure TFormIDEColorizerSettings.Button8Click(Sender: TObject);
Var
 LColor : TColor;
begin
 LColor := DialogSelectColor(ColorBoxDockFontActive.Selected);
 if LColor<>clNone then
    ColorBoxDockFontActive.Selected:=LColor;
end;

procedure TFormIDEColorizerSettings.Button9Click(Sender: TObject);
Var
 LColor : TColor;
begin
 LColor := DialogSelectColor(ColorBoxDockFontInActive.Selected);
 if LColor<>clNone then
    ColorBoxDockFontInActive.Selected:=LColor;
end;

procedure TFormIDEColorizerSettings.ButtonReportIssuesClick(Sender: TObject);
begin
  ShellExecute(0, 'open', 'https://code.google.com/p/delphi-ide-theme-editor/issues/list', '', '', SW_SHOWNORMAL);
end;

procedure TFormIDEColorizerSettings.ButtonProjectPageClick(Sender: TObject);
begin
  ShellExecute(0, 'open', 'https://code.google.com/p/delphi-ide-theme-editor/', '', '', SW_SHOWNORMAL);
end;

procedure TFormIDEColorizerSettings.ButtonAddFormClassClick(Sender: TObject);
var
 sFormClass : string;
begin
 sFormClass:=Trim(EditFormClass.Text);
 if (sFormClass<>'') then
   if ListBoxFormsHooked.Items.IndexOf(sFormClass)>=0 then
     ShowMessage(Format('The %s form is already included in the list', [sFormClass]))
   else
     ListBoxFormsHooked.Items.Add(sFormClass);
end;

procedure TFormIDEColorizerSettings.ButtonCheckUpdatesClick(Sender: TObject);
begin
 CheckForUpdates(False);
end;

procedure TFormIDEColorizerSettings.ButtonDeleteThemeClick(Sender: TObject);
var
 sThemeName, sFileName : string;
begin
  sThemeName:=cbThemeName.Text;
  sFileName:=IncludeTrailingPathDelimiter(GetIDEThemesFolder)+sThemeName+'.idetheme';
  if FileExists(sFileName) and (MessageDlg(Format('Do you want delete the %s theme?', [sThemeName]),  mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
  begin
    DeleteFile(sFileName);
    LoadThemes;
    if cbThemeName.Items.Count>0 then
     cbThemeName.ItemIndex:=0;
  end;
end;

procedure TFormIDEColorizerSettings.ButtonRemoveFormClassClick(Sender: TObject);
begin
 if ListBoxFormsHooked.ItemIndex<>-1 then
   ListBoxFormsHooked.Items.Delete(ListBoxFormsHooked.ItemIndex);
end;

procedure TFormIDEColorizerSettings.CbClrElementChange(Sender: TObject);
Var
 PropName : string;
 AColor   : TColor;
 OldIndex : Integer;
begin
 if ColorListBox1.ItemIndex>=0 then
 begin
   PropName:=ColorListBox1.Items[ColorListBox1.ItemIndex];
   AColor:=CbClrElement.Selected;
   SetOrdProp(ColorizerColorMap, PropName, AColor);
    OldIndex:=ColorListBox1.ItemIndex;
    ColorListBox1.PopulateList;
    if OldIndex>=0 then
      ColorListBox1.ItemIndex:=OldIndex;
 end;
end;

procedure TFormIDEColorizerSettings.ColorListChange(Sender: TObject);
Var
 PropName : string;
 AColor   : TColor;
begin
 if ColorListBox1.ItemIndex>=0 then
 begin
   PropName:= ColorListBox1.Items[ColorListBox1.ItemIndex];
   AColor  := GetOrdProp(ColorizerColorMap, PropName);
   CbClrElement.Selected:=AColor;
 end;
end;


procedure TFormIDEColorizerSettings.CbStylesChange(Sender: TObject);
begin
{.$IFDEF DELPHIXE2_UP}
 DrawSeletedVCLStyle;
{.$ENDIF}
end;

procedure TFormIDEColorizerSettings.cbThemeNameChange(Sender: TObject);
Var
  FileName : string;
begin
  FileName:=IncludeTrailingPathDelimiter(GetIDEThemesFolder)+cbThemeName.Text+'.idetheme';
  if FileExists(FileName)  then
  begin
    LoadColorMapFromXmlFile(ColorizerColorMap, FileName);
    LoadColorsTheme();
    DrawPalette();
    EditThemeName.Text:=cbThemeName.Text;
    ColorBoxBase.Selected:=ColorizerColorMap.Color;
  end;
end;

procedure TFormIDEColorizerSettings.cbVirtualStringTreeChange(Sender: TObject);
begin
 if FSettings.VirtualStringTreeFontSettingsDict.ContainsKey(cbVirtualStringTree.Text) then
 begin
  if FSettings.VirtualStringTreeFontSettingsDict.Items[cbVirtualStringTree.Text].FontName='' then
  begin
    cbVirtualStringTreeFont.ItemIndex:=cbVirtualStringTreeFont.Items.IndexOf(FSettings.VirtualStringTreeFontSettingsDict.Items[cbVirtualStringTree.Text].DefaultFontName);
    cbVirtualStringTreeFontSize.ItemIndex:= cbVirtualStringTreeFontSize.Items.IndexOf(IntToStr(FSettings.VirtualStringTreeFontSettingsDict.Items[cbVirtualStringTree.Text].DefaultSize));
  end
  else
  begin
    cbVirtualStringTreeFont.ItemIndex:=cbVirtualStringTreeFont.Items.IndexOf(FSettings.VirtualStringTreeFontSettingsDict.Items[cbVirtualStringTree.Text].FontName);
    cbVirtualStringTreeFontSize.ItemIndex:= cbVirtualStringTreeFontSize.Items.IndexOf(IntToStr(FSettings.VirtualStringTreeFontSettingsDict.Items[cbVirtualStringTree.Text].Size));
  end;
 end;
end;

procedure TFormIDEColorizerSettings.cbVirtualStringTreeFontDrawItem(
  Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  with (Control as TComboBox).Canvas do
  begin
    Font.Name := Screen.Fonts.Strings[Index];
    FillRect(Rect) ;
    TextOut(Rect.Left, Rect.Top, PChar(Screen.Fonts.Strings[Index]))
  end;
end;

procedure TFormIDEColorizerSettings.CheckBoxUseVClStylesClick(Sender: TObject);
begin
{.$IFDEF DELPHIXE2_UP}
  LoadVClStylesList;
{.$ENDIF}
end;

procedure TFormIDEColorizerSettings.ColorBoxBaseChange(Sender: TObject);
begin
 if CheckBoxAutoColor.Checked then
 begin
   GenerateColorMap(ColorizerColorMap, ColorBoxBase.Selected, CalculateTextColor(ColorBoxBase.Selected));
   LoadColorsTheme();
   DrawPalette();
 end;
end;

procedure TFormIDEColorizerSettings.ColorBoxBaseGetColors(
  Sender: TCustomColorBox; Items: TStrings);
begin
  FillColorsItems;
  Items.AddStrings(FColorListItems);
end;

procedure TFormIDEColorizerSettings.ColorListBox1GetColors(
  Sender: TCustomColorListBox; Items: TStrings);
var
  Count, Index: Integer;
  Properties  : TPropList;
  PropName : string;
begin
  Count := GetPropList(TypeInfo(TColorizerColorMap), tkAny, @Properties);
    for Index := 0 to Pred(Count) do
     if SameText(string(Properties[Index]^.PropType^.Name),'TColor') then
     begin
      PropName:=string(Properties[Index]^.Name);
      if Items.IndexOf(PropName)>=0 then
        Items.Objects[Items.IndexOf(PropName)]:=TObject(Integer(GetPropValue(ColorizerColorMap, PropName)))
      else
       Items.AddObject(PropName, TObject(Integer(GetPropValue(ColorizerColorMap, PropName))));
     end;
end;

procedure TFormIDEColorizerSettings.FillColorsItems;
Var
 Item : TIdentMapEntry;
begin
  if not Assigned(FColorListItems) then
   FColorListItems:=TStringList.Create;

  if FColorListItems.Count=0 then
  begin
    FColorListItems.BeginUpdate;
    try
    for Item in WebNamedColors do
     FColorListItems.AddObject(StringReplace(Item.Name, 'clWeb', '' , [rfReplaceAll]), TObject(Item.Value));
    finally
      FColorListItems.EndUpdate;
    end;
  end;
end;

procedure TFormIDEColorizerSettings.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action:=caFree;
end;

procedure TFormIDEColorizerSettings.FormCreate(Sender: TObject);
var
  s, sVersion : string;
begin
  LoadDockIcons;
  sVersion:=uMisc.GetFileVersion(GetModuleLocation);

  LabelVersion.Caption :='Delphi IDE Colorizer '+sVersion;
  LabelVersion2.Caption:='Copyright: 2011-2015 Rodrigo Ruz V. All rights reserved.';
  LabelVersion3.Caption:='blog http://theroadtodelphi.wordpress.com/';
  LinkLabel1.Caption   :='<a href="https://code.google.com/p/delphi-ide-theme-editor">site https://code.google.com/p/delphi-ide-theme-editor</a>';

  MemoThirdParty.Lines.Add('VCL Styles Utils');
  MemoThirdParty.Lines.Add('https://code.google.com/p/vcl-styles-utils/');
  MemoThirdParty.Lines.Add('Delphi Detours Library');
  MemoThirdParty.Lines.Add('https://code.google.com/p/delphi-detours-library/');
  MemoThirdParty.Lines.Add('JCL Debug');
  MemoThirdParty.Lines.Add('http://sourceforge.net/projects/jcl/');
  MemoThirdParty.Lines.Add('');
  MemoThirdParty.Lines.Add('This product includes software developed by the OpenSSL Project for use in the OpenSSL Toolkit');
  MemoThirdParty.Lines.Add('http://www.openssl.org/');
//  ColorMapCombo.Items.AddObject('(Default)', nil);
//  ColorMapCombo.ItemIndex := 0;
//  for I := 0 to ComponentCount - 1 do
//    if Components[I] is TCustomActionBarColorMap then
//      ColorMapCombo.Items.AddObject(Components[I].Name, Components[I]);

  ColorListBox1.OnChange:=ColorListChange;

  //StyleCombo.Items.Assign(ActionBarStyles);
  {$IFDEF DELPHIXE2_UP}
  FPreview:=TVclStylesPreview.Create(Self);
  FPreview.Parent:=PanelPreview;
  FPreview.BoundsRect := PanelPreview.ClientRect;
  {$ENDIF}
  FSettings:=TSettings.Create;

  FSettings.VirtualStringTreeFontSettingsDict.Free;
  FSettings.VirtualStringTreeFontSettingsDict := TFontSettingsDict.Create(TColorizerLocalSettings.Settings.VirtualStringTreeFontSettingsDict);

  //CheckBoxActivateDWM.Enabled:=DwmIsEnabled;
  {$IFDEF DELPHIXE2_UP}
  TabSheetVCLStyles.TabVisible  := True;//{$IFDEF DLLWIZARD}False{$ELSE}True{$ENDIF};
  TabSheetMenu.TabVisible := True;
  {$ELSE}
  TabSheetVCLStyles.TabVisible  := False;
  TabSheetMenu.TabVisible := False;
  {$ENDIF}
  TabSheetHookedForms.TabVisible:=False;
  ListBoxFormsHooked.Items.LoadFromFile(IncludeTrailingPathDelimiter(ExtractFilePath(GetModuleLocation))+'HookedWindows.dat');

  for s in TColorizerLocalSettings.Settings.VirtualStringTreeFontSettingsDict.Keys do
    cbVirtualStringTree.Items.Add(s);

  cbVirtualStringTreeFont.Items := Screen.Fonts;
  cbVirtualStringTree.ItemIndex:=0;
  cbVirtualStringTreeChange(nil);
end;


procedure TFormIDEColorizerSettings.DrawPalette;
var
  LBitMap : TBitmap;
begin
    LBitMap:=TBitmap.Create;
    try
     CreateArrayBitmap(ImagePalette.ClientWidth, ImagePalette.ClientHeight,[
      ColorizerColorMap.ShadowColor,
      ColorizerColorMap.Color,
      ColorizerColorMap.DisabledColor,
      ColorizerColorMap.DisabledFontColor,
      ColorizerColorMap.DisabledFontShadow,
      ColorizerColorMap.FontColor,
      ColorizerColorMap.HighlightColor,
      ColorizerColorMap.HotColor,
      ColorizerColorMap.HotFontColor,
      ColorizerColorMap.MenuColor,
      ColorizerColorMap.FrameTopLeftInner,
      ColorizerColorMap.FrameTopLeftOuter,
      ColorizerColorMap.FrameBottomRightInner,
      ColorizerColorMap.FrameBottomRightOuter,
      ColorizerColorMap.BtnFrameColor,
      ColorizerColorMap.BtnSelectedColor,
      ColorizerColorMap.BtnSelectedFont,
      ColorizerColorMap.SelectedColor,
      ColorizerColorMap.SelectedFontColor,
      ColorizerColorMap.UnusedColor
      ], LBitMap);
     ImagePalette.Picture.Assign(LBitMap);
    finally
      LBitMap.Free;
    end;
end;

{$IFDEF DELPHIXE2_UP}
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
{$ENDIF}

procedure TFormIDEColorizerSettings.FormDestroy(Sender: TObject);
begin
{$IFDEF DELPHIXE2_UP}
  FPreview.Free;
{$ENDIF}
  FSettings.Free;
  if Assigned(FColorListItems) then
    FColorListItems.Free;
end;

procedure TFormIDEColorizerSettings.GenerateIDEThemes(const Path: string);
//Var
//  i        : integer;
//  FileName : string;
begin
//
//   FileName:=IncludeTrailingPathDelimiter(GetIDEThemesFolder)+'Twilight.idetheme';
//   SaveColorMapToXmlFile(TwilightColorMap, FileName);
//
//   FileName:=IncludeTrailingPathDelimiter(GetIDEThemesFolder)+'XPColorMap.idetheme';
//   SaveColorMapToXmlFile(XPColorMap1, FileName);
//
//   FileName:=IncludeTrailingPathDelimiter(GetIDEThemesFolder)+'StandardColorMap.idetheme';
//   SaveColorMapToXmlFile(StandardColorMap, FileName);
//
//
//   for i:=0 to WebNamedColorsCount-1 do
//   begin
//     GenerateColorMap(XPColorMap, WebNamedColors[i].Value, CalculateTextColor(WebNamedColors[i].Value));
//     FileName:=StringReplace(WebNamedColors[i].Name,'clWeb','',[rfReplaceAll]);
//     FileName:=IncludeTrailingPathDelimiter(Path)+FileName+'.idetheme';
//     SaveColorMapToXmlFile(XPColorMap, FileName);
//
////     GenerateColorMap(XPColorMap, GetHighLightColor(WebNamedColors[i].Value));
////     FileName:=StringReplace(WebNamedColors[i].Name, 'clWeb', '',[rfReplaceAll]);
////     FileName:=IncludeTrailingPathDelimiter(Path)+FileName+'Light.idetheme';
////     SaveColorMapToXmlFile(XPColorMap, FileName);
////
////
////     GenerateColorMap(XPColorMap, GetShadowColor(WebNamedColors[i].Value));
////     FileName:=StringReplace(WebNamedColors[i].Name, 'clWeb', '',[rfReplaceAll]);
////     FileName:=IncludeTrailingPathDelimiter(Path)+FileName+'Dark.idetheme';
////     SaveColorMapToXmlFile(XPColorMap, FileName);
//   end;
end;

function TFormIDEColorizerSettings.GetIDEThemesFolder: String;
begin
  Result:=IncludeTrailingPathDelimiter(GetSettingsFolder)+'Themes';
end;

function TFormIDEColorizerSettings.GetSettingsFolder: String;
begin
  Result:=ExtractFilePath(GetModuleLocation());
end;

procedure TFormIDEColorizerSettings.ImageTwitterClick(Sender: TObject);
begin
  ShellExecute(0, 'open', 'https://twitter.com/RRUZ', '', '', SW_SHOWNORMAL);
end;

procedure TFormIDEColorizerSettings.ImageGooglePlusClick(Sender: TObject);
begin
  ShellExecute(0, 'open', 'https://plus.google.com/112937016948869859802', '', '', SW_SHOWNORMAL);
end;

procedure TFormIDEColorizerSettings.ImageContributeClick(Sender: TObject);
begin
  ShellExecute(0, 'open', 'https://code.google.com/p/delphi-ide-theme-editor/', '', '', SW_SHOWNORMAL);
end;

procedure TFormIDEColorizerSettings.Init;
begin
  //LoadColorElements;
  LoadThemes;
  LoadSettings;
end;

procedure TFormIDEColorizerSettings.ListBoxDockImagesDrawItem(
  Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  CenterText : integer;
begin
  ListBoxDockImages.Canvas.FillRect(Rect);
  ImageListDock.Draw(ListBoxDockImages.Canvas, Rect.Left + 4, Rect.Top + 3, Index);
  CenterText := ( Rect.Bottom - Rect.Top - ListBoxDockImages.Canvas.TextHeight(Text)) div 2 ;
  ListBoxDockImages.Canvas.TextOut (Rect.left + ImageListDock.Width + 8 , Rect.Top + CenterText,
  ListBoxDockImages.Items.Strings[index]);
end;


procedure TFormIDEColorizerSettings.ListBoxDockImagesMeasureItem(
  Control: TWinControl; Index: Integer; var Height: Integer);
begin
  Height := ImageListDock.Height + 6;
end;

procedure TFormIDEColorizerSettings.ListViewTypesChange(Sender: TObject;
  Item: TListItem; Change: TItemChange);
begin
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
procedure TFormIDEColorizerSettings.LoadColorsTheme;
var
  OldIndex : Integer;
begin
  OldIndex:=ColorListBox1.ItemIndex;
  ColorListBox1.PopulateList;
  if OldIndex>=0 then
    ColorListBox1.ItemIndex:=OldIndex;
end;

procedure TFormIDEColorizerSettings.LoadDockIcons;
var
  s, ImagesPath : string;
  LBitMap   : TBitmap;
  LPngImage : TPngImage;
begin
  ImagesPath:=ExtractFilePath(GetModuleLocation)+'images\dock_images';

  for s in TDirectory.GetFiles(ImagesPath, '*.png') do
  begin
    LBitMap:=TBitmap.Create;
    try
      LPngImage:=TPngImage.Create;
      try
        LPngImage.LoadFromFile(s);
        LPngImage.AssignTo(LBitMap);
        LBitMap.AlphaFormat:=afIgnored;
        ImageList_Add(ImageListDock.Handle, LBitMap.Handle, 0);
      finally
        LPngImage.Free;
      end;
    finally
      LBitMap.Free;
    end;

    ListBoxDockImages.Items.Add(ChangeFileExt(ExtractFileName(s),''));
  end;

end;

procedure TFormIDEColorizerSettings.LoadSettings;
begin
  ReadSettings(FSettings, GetSettingsFolder);

  CheckBoxUpdates.Checked  := FSettings.CheckUpdates;
  CheckBoxHookSystemColors.Checked  := FSettings.HookSystemColors;
  CheckBoxCustomDockBars.Checked := FSettings.DockCustom;
  RbtnDockGradientHorz.Checked := FSettings.DockGradientHor;
  RbtnDockGradientVert.Checked := not FSettings.DockGradientHor;
  RbtnDockBorderRounded.Checked   := FSettings.DockBorderRounded;
  RbtnDockBorderRectangle.Checked := not FSettings.DockBorderRounded;

  CheckBoxUseCustomColorsDock.Checked := FSettings.DockCustomColors;
  ColorBoxDockStartGradientActive.Selected   := TryStrToColor(FSettings.DockStartGradActive, clBtnFace);
  ColorBoxDockEndGradientActive.Selected     := TryStrToColor(FSettings.DockEndGradActive, clBtnFace);
  ColorBoxDockStartGradientInActive.Selected := TryStrToColor(FSettings.DockStartGradInActive, clBtnFace);
  ColorBoxDockEndGradientInActive.Selected   := TryStrToColor(FSettings.DockEndGradInActive, clBtnFace);

  ColorBoxDockFontActive.Selected   := TryStrToColor(FSettings.DockActiveFontColor, clBlack);
  ColorBoxDockFontInActive.Selected   := TryStrToColor(FSettings.DockInActiveFontColor, clBlack);
  ColorBoxDockBorderActive.Selected     := TryStrToColor(FSettings.DockActiveBorderColor, clBlack);
  ColorBoxDockBorderInActive.Selected   := TryStrToColor(FSettings.DockInActiveBorderColor, clBlack);

  CheckBoxIDETabsCustom.Checked  := FSettings.TabIDECustom;
  CheckBoxIDETabsOutLine.Checked := FSettings.TabIDEOutLine;
  ColorBoxIDETabStartGradientActive.Selected   := TryStrToColor(FSettings.TabIDEStartGradActive, clBtnFace);
  ColorBoxIDETabEndGradientActive.Selected     := TryStrToColor(FSettings.TabIDEEndGradActive, clBtnFace);
  ColorBoxIDETabStartGradientInActive.Selected := TryStrToColor(FSettings.TabIDEStartGradInActive, clBtnFace);
  ColorBoxIDETabEndGradientInActive.Selected   := TryStrToColor(FSettings.TabIDEEndGradInActive, clBtnFace);
  ColorBoxIDETabFontActive.Selected     := TryStrToColor(FSettings.TabIDEActiveFontColor, clBlack);
  ColorBoxIDETabOutLineColor.Selected   := TryStrToColor(FSettings.TabIDEOutLineColor, clBlack);

  CheckBoxCustomHeader.Checked := FSettings.HeaderCustom;
  ColorBoxHeaderStartGradient.Selected   := TryStrToColor(FSettings.HeaderStartGrad, clBtnFace);
  ColorBoxHeaderEndGradient.Selected     := TryStrToColor(FSettings.HeaderEndGrad, clBtnFace);
  ColorBoxHeaderBorderColor.Selected     := TryStrToColor(FSettings.HeaderBorderColor, clBlack);
  ColorBoxHeaderFontColor.Selected       := TryStrToColor(FSettings.HeaderFontColor, clBlack);

  RbtnToolBarGradientVert.Checked := FSettings.ToolbarGradientHor;
  RbtnToolBarGradientHorz.Checked := not FSettings.ToolbarGradientHor;
  CheckBoxUseCustomColorsToolbar.Checked := FSettings.ToolbarCustomColors;
  ColorBoxToolBarStartGrad.Selected   := TryStrToColor(FSettings.ToolbarStartGrad, clBtnFace);
  ColorBoxToolBarStartEnd.Selected    := TryStrToColor(FSettings.ToolbarEndGrad, clBtnFace);

  CheckBoxTransparentMenus.Checked    := FSettings.MenuTransparent;
  UpDownMenu.Position                 := FSettings.MenuTransLevel;

  CheckBoxEnabled.Checked:=FSettings.Enabled;
  //CheckBoxActivateDWM.Checked:=FSettings.EnableDWMColorization;
  CheckBoxFixIDEDrawIcon.Checked:=FSettings.FixIDEDisabledIconsDraw;
  CheckBoxAutoColor.Checked:=FSettings.AutogenerateColors;
  CheckBoxGutterIcons.Checked:=FSettings.ChangeIconsGutter;
//  StyleCombo.ItemIndex:=StyleCombo.Items.IndexOf(FSettings.StyleBarName);
//  ColorMapCombo.ItemIndex:=ColorMapCombo.Items.IndexOf(FSettings.ColorMapName);
  EditThemeName.Text:=FSettings.ThemeName;
  cbThemeName.ItemIndex:=cbThemeName.Items.IndexOf(EditThemeName.Text);
  cbThemeNameChange(nil);

  CheckBoxUseVClStyles.Checked   := FSettings.UseVCLStyles;
  CheckBoxVCLStylesForms.Checked := FSettings.VCLStylesForms;
  CheckBoxVCLStylesMenusColors.Checked := FSettings.VCLStylesMenusColors;
  CheckBoxVCLStylesScrollBars.Checked  := FSettings.VCLStylesScrollBars;
  CheckBoxVCLStylesControls.Checked    := FSettings.VCLStylesControls;
{$IFDEF DELPHIXE2_UP}
  LoadVClStylesList;
{$ENDIF}
  //EditVCLStylesPath.Text:=FSettings.VCLStylesPath;
  CbStyles.ItemIndex:=CbStyles.Items.IndexOf(FSettings.VCLStyleName);
  ListBoxDockImages.ItemIndex:=ListBoxDockImages.Items.IndexOf(FSettings.DockImages);
{$IFDEF DELPHIXE2_UP}
  DrawSeletedVCLStyle;
{$ENDIF}

 //cbVirtualStringTreeFont.ItemIndex:= cbVirtualStringTreeFont.Items.IndexOf(FSettings.VirtualStringTreeFont);
 //cbVirtualStringTreeFontSize.ItemIndex := cbVirtualStringTreeFontSize.Items.IndexOf(IntToStr(FSettings.VirtualStringTreeFontSize));
 CheckboxVirtualStringTreeFontDefault.Checked:=  FSettings.VirtualStringTreeFontDefault;
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

{$IFDEF DELPHIXE2_UP}
procedure TFormIDEColorizerSettings.LoadVClStylesList;
var
 s : string;
begin
 if CheckBoxUseVClStyles.Checked then
 begin
  CbStyles.Items.Clear;
  RegisterVClStylesFiles();
   for s in TStyleManager.StyleNames do
    if not SameText(s, 'Windows') then
    CbStyles.Items.Add(s);

   CbStyles.ItemIndex:=CbStyles.Items.IndexOf(FSettings.VCLStyleName);
 end;
end;
{$ENDIF}
{ TColorListBox }

procedure TColorListBox.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TColorListBox.CNCommand(var AMessage: TWMCommand);
begin
  inherited;
  if (AMessage.NotifyCode = LBN_SELCHANGE) and (FItemIndex <> ItemIndex) then
  begin
    FItemIndex := ItemIndex;
    Change;
  end;
end;

procedure TColorListBox.SetItemIndex(const Value: Integer);
begin
  inherited;
  if FItemIndex <> ItemIndex then
  begin
    FItemIndex := ItemIndex;
    Change;
  end;
end;

initialization
  ColorizerColorMap:=TColorizerColorMap.Create(nil);
finalization
  ColorizerColorMap.Free;
end.
