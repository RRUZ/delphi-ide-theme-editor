// **************************************************************************************************
//
// Unit DITE.Main
// Main Form  for the Delphi IDE Theme Editor
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
// Portions created by Rodrigo Ruz V. are Copyright (C) 2011-2020 Rodrigo Ruz V.
// All Rights Reserved.
//
// **************************************************************************************************

unit DITE.Main;

interface

// T0DO

// save theme as... done
// delete theme   done
// clone theme     done
// hide line number ( delphi 7) - done
// grow list themes - done
// change xml format, add all versions internal support, theme GUID. theme version, theme name, author -> working  on it
// notepad++ themes
// exception handler extended - done
// eclipse themes   done
// hue/saturation - done
// *****************************************************
// Importer mapper XML . create xml with relations between delphi themes and external IDE themes
// *****************************************************
// download themes online
// config file - done
// translate
// update online   (check new version) - done

// import from http://studiostyl.es/schemes done
// import from notepad++
// http://www.eclipsecolorthemes.org/   done

// http://www.colorotate.org/

// import themes from IntelliJ IDE
// http://confluence.jetbrains.net/display/RUBYDEV/Third-party+add-ons
// http://devnet.jetbrains.net/docs/DOC-1154
// http://tedwise.com/2009/02/26/dark-pastels-theme-for-intellij-idea/
// http://yiwenandsoftware.wordpress.com/2008/05/15/textmate-golbalt-color-theme-for-intellij/
// http://blog.gokhanozcan.com/2008/10/06/intellij-idea-dark-color-scheme/
// http://stackoverflow.com/questions/4414593/where-can-i-download-intellij-idea-10-color-schemes

// import themes from    NetBeans IDE
// http://www.niccolofavari.com/dark-low-contrast-color-scheme-for-netbeans-ide
// http://blog.mixu.net/2010/05/03/syntax-highlighting-color-schemes-for-netbeans/

// import themes from Komodo
// http://www.kolormodo.com

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, DITE.ColorPanel,
  Dialogs, ImgList, StdCtrls, ComCtrls, ExtCtrls, SynEditHighlighter, DITE.SupportedIDEs, DITE.ColorSelector,
  SynHighlighterPas, SynEdit, SynMemo, DITE.DelphiVersions, DITE.DelphiIDEHighlight, DITE.LazarusVersions, Vcl.ActnPopup, DITE.AppMethodVersions,
  pngimage, DITE.Settings, ExtDlgs, Menus, SynEditExport, SynExportHTML, Generics.Defaults, Generics.Collections, Vcl.ActnList,
  Vcl.PlatformDefaultStyleActnCtrls, System.Actions, Vcl.Styles.Fixes, Vcl.Styles.NC,
  Vcl.ActnMan, System.ImageList, SynEditCodeFolding, Vcl.ControlList,
  Vcl.AppEvnts;

{ .$DEFINE ENABLE_THEME_EXPORT }

{$I units\Common.inc}

type
  TCompPngImages = class
  private
    FNormal: TPngImage;
    FBN: TPngImage;
  public
    property Normal: TPngImage read FNormal write FNormal;
    property BN: TPngImage read FBN write FBN;
    destructor Destroy; override;
  end;

  TDITETheme = class
  private
    FFileName: string;
    FName: string;
    FAuthor: string;
    FBitmap: Vcl.Graphics.TBitmap;
    FTheme: TIDETheme;
    FModified: TDateTime;
  public
    constructor Create(const AFileName: string);
    destructor Destroy; override;
    property Name: string read FName write FName;
    property FileName: string read FFileName write FFileName;
    property Author: string read FAuthor write FAuthor;
    property Modified: TDateTime read FModified write FModified;
    property Bitmap: Vcl.Graphics.TBitmap read FBitmap write FBitmap;
    property Theme: TIDETheme read FTheme write FTheme;
  end;

  // TPopupMenu=class(Vcl.ActnPopup.TPopupActionBar);

  TFrmMain = class(TForm)
    ImageListDelphiVersion: TImageList;
    lbIDEs: TLabel;
    CbElement: TComboBox;
    LabelElement: TLabel;
    GroupBoxTextAttr: TGroupBox;
    CheckBold: TCheckBox;
    CheckItalic: TCheckBox;
    CheckUnderline: TCheckBox;
    GroupBoxUseDefaults: TGroupBox;
    CheckForeground: TCheckBox;
    CheckBackground: TCheckBox;
    CblForeground: TColorBox;
    CblBackground: TColorBox;
    SynPasSyn1: TSynPasSyn;
    lbFont: TLabel;
    CbIDEFonts: TComboBox;
    EditFontSize: TEdit;
    UpDownFontSize: TUpDown;
    BtnApplyFont: TButton;
    Label7: TLabel;
    OpenDialogImport: TOpenDialog;
    ProgressBar1: TProgressBar;
    SynEditCode: TSynEdit;
    ImageListlGutterGlyphs: TImageList;
    BtnSelForColor: TButton;
    ImageList1: TImageList;
    BtnSelBackColor: TButton;
    PopupMenuThemes: TPopupMenu;
    CloneTheme1: TMenuItem;
    DeleteTheme1: TMenuItem;
    ApplyTheme1: TMenuItem;
    SaveChanges1: TMenuItem;
    SaveAs1: TMenuItem;
    LabelMsg: TLabel;
    SynExporterHTML1: TSynExporterHTML;
    OpenDialogExport: TOpenDialog;
    ComboBoxExIDEs: TComboBoxEx;
    PopupMenu1: TPopupMenu;
    ActionList1: TActionList;
    ActionApplyTheme: TAction;
    ActionCloneTheme: TAction;
    ActionDeleteTheme: TAction;
    ActionSaveChanges: TAction;
    ActionSaveAs: TAction;
    PopupActionBar1: TPopupActionBar;
    PanelColors: TPanel;
    RadioButtonFore: TRadioButton;
    RadioButtonBack: TRadioButton;
    ImageListNCArea: TImageList;
    N1: TMenuItem;
    ImportVSTheme1: TMenuItem;
    ImportEclipseTheme1: TMenuItem;
    N2: TMenuItem;
    Contribute1: TMenuItem;
    ActionManager1: TActionManager;
    ActionExoLazarusClrSch: TAction;
    ActionImportThemeReg: TAction;
    N3: TMenuItem;
    ExportThemetoLazarusColorScheme1: TMenuItem;
    ImportcurrentIDEThemefromregistry1: TMenuItem;
    ActionSetDefaultTheme: TAction;
    SetdefaultthemevaluesforselectedIDE1: TMenuItem;
    BtnAdditionalSettings: TButton;
    Panel1: TPanel;
    LinkLabel1: TLinkLabel;
    Image1: TImage;
    PanelThemeName: TPanel;
    BtnApply: TButton;
    BtnSave: TButton;
    EditThemeName: TEdit;
    LabelThemeName: TLabel;
    BtnApplySmall: TButton;
    clThemes: TControlList;
    lbTheme: TLabel;
    lbThemetype: TLabel;
    Image3: TImage;
    ApplicationEvents1: TApplicationEvents;
    chbDark: TCheckBox;
    chbLight: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure LvIDEVersionsChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure CbElementChange(Sender: TObject);
    procedure CbIDEFontsChange(Sender: TObject);
    procedure CblForegroundChange(Sender: TObject);
    procedure BtnApplyFontClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SynEditCodeClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ImageConfClick(Sender: TObject);
    procedure ImageHueClick(Sender: TObject);
    procedure SynEditCodeSpecialLineColors(Sender: TObject; Line: integer; var Special: boolean; var FG, BG: TColor);
    procedure SynEditCodeGutterClick(Sender: TObject; Button: TMouseButton; X, Y, Line: integer; Mark: TSynEditMark);
    procedure BtnSelForColorClick(Sender: TObject);
    procedure ImageBugClick(Sender: TObject);
    procedure BtnSelBackColorClick(Sender: TObject);
    procedure ImageUpdateClick(Sender: TObject);
    procedure ComboBoxExIDEsChange(Sender: TObject);
    procedure ActionDeleteThemeExecute(Sender: TObject);
    procedure ActionCloneThemeExecute(Sender: TObject);
    procedure ActionSaveAsExecute(Sender: TObject);
    procedure ActionApplyThemeExecute(Sender: TObject);
    procedure ActionSaveChangesExecute(Sender: TObject);
    procedure ImageBugMouseEnter(Sender: TObject);
    procedure ImageBugMouseLeave(Sender: TObject);
    procedure RadioButtonForeClick(Sender: TObject);
    procedure ImportVSTheme1Click(Sender: TObject);
    procedure ImportEclipseTheme1Click(Sender: TObject);
    procedure Contribute1Click(Sender: TObject);
    procedure ActionExoLazarusClrSchUpdate(Sender: TObject);
    procedure ActionExoLazarusClrSchExecute(Sender: TObject);
    procedure ActionImportThemeRegUpdate(Sender: TObject);
    procedure ActionImportThemeRegExecute(Sender: TObject);
    procedure ActionSetDefaultThemeUpdate(Sender: TObject);
    procedure ActionSetDefaultThemeExecute(Sender: TObject);
    procedure BtnAdditionalSettingsClick(Sender: TObject);
    procedure LinkLabel1LinkClick(Sender: TObject; const Link: string;
      LinkType: TSysLinkType);
    procedure clThemesBeforeDrawItem(AIndex: Integer; ACanvas: TCanvas;
      ARect: TRect; AState: TOwnerDrawState);
    procedure clThemesItemClick(Sender: TObject);
    procedure ApplicationEvents1Idle(Sender: TObject; var Done: Boolean);
    procedure chbLightClick(Sender: TObject);
    procedure ActionApplyThemeUpdate(Sender: TObject);
  private
    FCompactMode: Boolean;
    FChanging, FLoading: Boolean;
    FThemeChangued, FThemesLoaded: boolean;
    FSettings: TSettings;
    FCurrentTheme: TIDETheme;
    FMapHighlightElementsTSynAttr: TStrings;
    IDEsList: TList<TDelphiVersionData>;
    FIDEData: TDelphiVersionData;
    FrmColorPanel: TColorPanel;
    NCControls: TNCControls;
    Icons : TObjectDictionary<string, TIcon>;
    ActionImages: TObjectDictionary<string, TCompPngImages>;

    FDITEThemes: TObjectList<TDITETheme>;
    procedure SwitchModeClick(Sender: TObject);
    procedure SetCompactMode(Value : Boolean);
    procedure LoadThemes;
    procedure LoadFixedWidthFonts;
    procedure LoadValuesElements;
    procedure RefreshPasSynEdit;
    procedure SetSynAttr(Element: TIDEHighlightElements; SynAttr: TSynHighlighterAttributes; ADelphiVersionData: TDelphiVersionData);
    procedure CreateThemeFile;
    procedure ApplyCurrentTheme;
    function GetThemeIndex(const AThemeName: string): integer;
    function GetElementIndex(Element: TIDEHighlightElements): integer;
    procedure OnSelForegroundColorChange(Sender: TObject);
    procedure OnSelBackGroundColorChange(Sender: TObject);
    procedure CMStyleChanged(var Message: TMessage); message CM_STYLECHANGED;
    procedure ImportTheme(ImportType: TIDEImportThemes);
    procedure ExportToLazarusTheme();
    procedure ImportDelphiThemeRegistry();
    procedure SetDefaultDelphiTheme();
    procedure LoadNCControls;
    procedure LoadIcons;
    function GetDelphiVersionData(Index: integer): TDelphiVersionData;
    function GetIDEData: TDelphiVersionData;
    property IDEData: TDelphiVersionData read GetIDEData write FIDEData;
    property CompactMode : Boolean read FCompactMode write SetCompactMode;
  public
    property Settings: TSettings read FSettings;
  end;

var
  FrmMain: TFrmMain;

implementation

uses
{$WARN UNIT_PLATFORM OFF}
  Vcl.FileCtrl,
{$WARN UNIT_PLATFORM ON}
  System.Diagnostics,
  System.UITypes,
  System.Types,
  Winapi.ShellApi,
  System.IOUtils,
  System.StrUtils,
  Vcl.GraphUtil,
  Winapi.CommCtrl,
  Vcl.Styles,
  Vcl.Styles.Utils.Graphics,
  Vcl.Styles.FontAwesome,
  Vcl.Themes,
  Vcl.ListActns,
  DITE.VclStylesFix,
  DITE.HueSat,
  DITE.EclipseThemes,
  DITE.VSThemes,
  DITE.Misc,
  DITE.LazarusIDEHighlight,
  DITE.SMSIDEHighlight,
  DITE.StackTrace,
  DITE.SMSVersions,
  DITE.LoadThemesImages,
  DITE.HelpInsight,
  DITE.StdActionsPopMenu,
  DITE.AdditionalSettings;

const
  cInvalidBreakLine = 9;
  cExecutionPointLine = 10;
  cEnabledBreakLine = 12;
  cDisabledBreakLine = 13;
  cErrorLineLine = 14;

{$R *.dfm}

procedure TFrmMain.ActionApplyThemeExecute(Sender: TObject);
begin
  try
    if (ComboBoxExIDEs.ItemIndex >= 0) and (clThemes.ItemIndex <> -1) then
      if IsAppRunning(IDEData.Path) then
        MsgBox(Format('Before to continue you must close all running instances of the %s IDE', [IDEData.Name]))
      else if MessageDlg(Format('Do you want apply the theme "%s" to the %s IDE?',
        [FDITEThemes[clThemes.ItemIndex].Name, IDEData.Name]), mtConfirmation, [mbYes, mbNo], 0) = mrYes then
        ApplyCurrentTheme;
  except
    on E: Exception do
      MsgBox(Format('Error setting theme - Message : %s : Trace %s', [E.Message, E.StackTrace]));
  end;
end;

procedure TFrmMain.ActionApplyThemeUpdate(Sender: TObject);
begin
   TAction(Sender).Enabled := not FLoading;
end;

procedure TFrmMain.ActionCloneThemeExecute(Sender: TObject);
var
  LIndex: Integer;
  LFileName, NFileName: string;
begin
  try
    if clThemes.ItemIndex <> -1 then
    begin
      LIndex := clThemes.ItemIndex;
      LFileName := FDITEThemes[LIndex].FileName;
      NFileName := ChangeFileExt(ExtractFileName(LFileName), ''); // remove .xml
      NFileName := ChangeFileExt(ExtractFileName(NFileName), ''); // remove .theme
      NFileName := ExtractFilePath(LFileName) + NFileName + '-Clone.theme.xml';
      DeleteFile(NFileName);
      TFile.Copy(LFileName, NFileName);
      LoadThemes;
      if LIndex >= 0 then
        clThemes.ItemIndex := LIndex;
    end;
  except
    on E: Exception do
      MsgBox(Format('Error deleting theme message : %s : trace %s', [E.Message, E.StackTrace]));
  end;
end;

procedure TFrmMain.ActionDeleteThemeExecute(Sender: TObject);
var
  LIndex: integer;
begin
  try
    LIndex := clThemes.ItemIndex;
    if LIndex <> -1 then
      if MessageDlg(Format('Do you want delete the theme "%s"?',
        [FDITEThemes[LIndex].Name]), mtConfirmation, [mbYes, mbNo], 0) = mrYes
      then
      begin
        DeleteFile(FDITEThemes[LIndex].FileName);
        FDITEThemes.Delete(LIndex);
        //LoadThemes;
        if (LIndex >= 0) and (LIndex <= FDITEThemes.Count - 1) then
        begin
          clThemes.ItemIndex := LIndex;
          clThemesItemClick(nil);
        end;
      end;
  except
    on E: Exception do
      MsgBox(Format('Error deleting theme message : %s : trace %s', [E.Message, E.StackTrace]));
  end;
end;

procedure TFrmMain.ActionExoLazarusClrSchExecute(Sender: TObject);
begin
  ExportToLazarusTheme();
end;

procedure TFrmMain.ActionExoLazarusClrSchUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (IDEData.IDEType = TSupportedIDEs.LazarusIDE);
end;

procedure TFrmMain.ActionImportThemeRegExecute(Sender: TObject);
begin
  ImportDelphiThemeRegistry();
end;

procedure TFrmMain.ActionImportThemeRegUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (IDEData.IDEType = TSupportedIDEs.DelphiIDE)
  {$IFDEF DELPHI_OLDER_VERSIONS_SUPPORT}
   and not DelphiIsOldVersion(IDEData)
  {$ENDIF}
   ;
end;

procedure TFrmMain.ActionSaveAsExecute(Sender: TObject);
var
  LIndex: integer;
  Value: string;
begin
  // detect name in list show msg overwrite
  Value := EditThemeName.Text;
  try
    if InputQuery('Save As..', 'Enter the new name of the theme', Value) then
    begin
      EditThemeName.Text := Value;
      CreateThemeFile;
      LoadThemes;
      LIndex := GetThemeIndex(EditThemeName.Text);
      if LIndex >= 0 then
        clThemes.ItemIndex := LIndex;
    end;
  except
    on E: Exception do
      MsgBox(Format('Error Saving theme  Message : %s : Trace %s', [E.Message, E.StackTrace]));
  end;
end;

procedure TFrmMain.ActionSaveChangesExecute(Sender: TObject);
var
  LIndex: integer;
begin
  // detect name in list show msg overwrite
  try
    CreateThemeFile;
    LoadThemes;
    LIndex := GetThemeIndex(EditThemeName.Text);
    if LIndex >= 0 then
      clThemes.ItemIndex := LIndex;
  except
    on E: Exception do
      MsgBox(Format('Error Saving theme  Message : %s : Trace %s', [E.Message, E.StackTrace]));
  end;
end;

procedure TFrmMain.ActionSetDefaultThemeExecute(Sender: TObject);
begin
  SetDefaultDelphiTheme();
end;

procedure TFrmMain.ActionSetDefaultThemeUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (IDEData.IDEType = TSupportedIDEs.DelphiIDE);
  TAction(Sender).Visible := (DelphiVersionNumbers[IDEData.Version] < DelphiVersionNumbers[TDelphiVersions.Delphi10Sydney])
end;

procedure TFrmMain.ApplicationEvents1Idle(Sender: TObject; var Done: Boolean);
begin
 if not FThemesLoaded then
 begin
   LoadThemes;
   ComboBoxExIDEs.ItemIndex := 0;
   ComboBoxExIDEsChange(ComboBoxExIDEs);
   FThemesLoaded := True;
 end;
end;

procedure TFrmMain.ApplyCurrentTheme;
begin
  if ComboBoxExIDEs.ItemIndex >= 0 then
  begin
    if IDEData.IDEType = TSupportedIDEs.DelphiIDE then
    begin
      if ApplyDelphiIDETheme(IDEData, FCurrentTheme, FDITEThemes[clThemes.ItemIndex].Name) then
        MsgBox('The theme was successfully applied')
      else
        MsgBox('Error setting theme');

      if Settings.ApplyThemeHelpInsight and
        (DelphiVersionNumbers[IDEData.Version] < DelphiVersionNumbers[TDelphiVersions.Delphi10Sydney]) then
        ApplyThemeHelpInsight(FCurrentTheme, IDEData);
    end
    else if IDEData.IDEType = TSupportedIDEs.LazarusIDE then
      if ApplyLazarusIDETheme(FCurrentTheme, EditThemeName.Text) then
        MsgBox('The theme was successfully applied')
      else
        MsgBox('Error setting theme')
    else if IDEData.IDEType = TSupportedIDEs.SMSIDE then
      if ApplySMSIDETheme(FCurrentTheme, EditThemeName.Text) then
        MsgBox('The theme was successfully applied')
      else
        MsgBox('Error setting theme');
  end;
end;

procedure TFrmMain.BtnAdditionalSettingsClick(Sender: TObject);
var
  LDialog: TFrmAdditionalSettings;
begin
  LDialog := TFrmAdditionalSettings.Create(Self);
  try
    LDialog.IDEData := IDEData;
    LDialog.ShowModal;
  finally
    LDialog.Free;
  end;
end;

procedure TFrmMain.BtnApplyFontClick(Sender: TObject);
begin
  try
    if (ComboBoxExIDEs.ItemIndex >= 0) then
      if IsAppRunning(IDEData.Path) then
        MsgBox(Format('Before to continue you must close all running instances of the %s IDE', [IDEData.Name]))
      else if MessageDlg(Format('Do you want apply the "%s" font to the %s IDE?', [CbIDEFonts.Text, IDEData.Name]), mtConfirmation,
        [mbYes, mbNo], 0) = mrYes then
      begin
        if IDEData.IDEType = TSupportedIDEs.DelphiIDE then
        begin
          if SetDelphiIDEFont(IDEData, CbIDEFonts.Text, UpDownFontSize.Position) then
            MsgBox('The font was successfully applied')
          else
            MsgBox('Error setting font');
        end
        else if IDEData.IDEType = TSupportedIDEs.LazarusIDE then
        begin
          if SetLazarusIDEFont(CbIDEFonts.Text, UpDownFontSize.Position) then
            MsgBox('The font was successfully applied')
          else
            MsgBox('Error setting font');
        end
        else if IDEData.IDEType = TSupportedIDEs.SMSIDE then
        begin
          if SetSMSIDEFont(CbIDEFonts.Text, UpDownFontSize.Position) then
            MsgBox('The font was successfully applied')
          else
            MsgBox('Error setting font');
        end;

      end;
  except
    on E: Exception do
      MsgBox(Format('Error setting font - Message : %s : Trace %s', [E.Message, E.StackTrace]));
  end;
end;

procedure TFrmMain.ExportToLazarusTheme();
var
  i: integer;
  OutPutFolder: String;
  GoNext: boolean;
  LStopWatch: TStopwatch;
  Directory: String;
begin
  try
    Directory := '';
    OutPutFolder := GetCommonSettingsFolder + 'Themes Lazarus';
    if SysUtils.DirectoryExists(OutPutFolder) then
      Directory := OutPutFolder;

    if SelectDirectory('Select directory', Directory, Directory, [sdNewFolder, sdNewUI, sdShowEdit, sdValidateDir, sdShowShares], nil) then
      OutPutFolder := Directory
    else
      Exit;

    OpenDialogExport.InitialDir := GetPrivateSettingsFolder;
    if OpenDialogExport.Execute(Handle) then
    begin

      if OpenDialogExport.Files.Count = 1 then
        GoNext := MessageDlg(Format('Do you want import the "%s" file?', [ExtractFileName(OpenDialogExport.FileName)]), mtConfirmation,
          [mbYes, mbNo], 0) = mrYes
      else
        GoNext := MessageDlg(Format('Do you want import the %d files selected?', [OpenDialogExport.Files.Count]), mtConfirmation,
          [mbYes, mbNo], 0) = mrYes;

      if GoNext and (OpenDialogExport.Files.Count > 1) then
      begin

        LStopWatch := TStopwatch.Create;
        LStopWatch.Start;

        ProgressBar1.Visible := True;
        try
          ProgressBar1.Position := 0;
          ProgressBar1.Max := OpenDialogExport.Files.Count;
          LabelMsg.Visible := True;
          for i := 0 to OpenDialogExport.Files.Count - 1 do
          begin
            LabelMsg.Caption := Format('Exporting %s theme', [ExtractFileName(OpenDialogExport.Files[i])]);
            DelphiIDEThemeToLazarusTheme(OpenDialogExport.Files[i], OutPutFolder);
            ProgressBar1.Position := i;
          end;
        finally
          ProgressBar1.Visible := False;
          LabelMsg.Visible := False;
        end;

        LStopWatch.Stop;
        MsgBox(Format('%d Themes exported in %n seconds', [OpenDialogExport.Files.Count, LStopWatch.Elapsed.TotalSeconds]));
      end
      else if GoNext and (OpenDialogExport.Files.Count = 1) then
      begin
        if DelphiIDEThemeToLazarusTheme(OpenDialogExport.FileName, OutPutFolder) then
          MsgBox(Format('"%s" Theme exported', [ExtractFileName(OpenDialogExport.FileName)]));
      end;
    end;

  except
    on E: Exception do
      MsgBox(Format('Error exporting themes - Message : %s : Trace %s', [E.Message, E.StackTrace]));
  end;
end;

procedure TFrmMain.SetDefaultDelphiTheme;
begin
  try
    if (ComboBoxExIDEs.ItemIndex >= 0) and not IsAppRunning(IDEData.Path)  then
      if MessageDlg(Format('Do you want apply the default theme to the "%s" IDE?',
        [IDEData.Name]), mtConfirmation, [mbYes, mbNo], 0) = mrYes
      then
      begin
        if SetDelphiIDEDefaultTheme(IDEData) then
          MsgBox('Default theme was applied')
        else
          MsgBox('Error setting default theme');

        if (IDEData.IDEType = TSupportedIDEs.DelphiIDE) and
          (DelphiVersionNumbers[IDEData.Version] < DelphiVersionNumbers[TDelphiVersions.Delphi10Sydney]) then
          SetHelpInsightDefault(IDEData);

        ComboBoxExIDEsChange(nil);
      end;
  except
    on E: Exception do
      MsgBox(Format('Error setting default theme - Message : %s : Trace %s', [E.Message, E.StackTrace]));
  end;
end;

procedure PrintControl(AControl: TWinControl; ARect: TRect; AOut: TBitmap);
var
  DC: HDC;
begin
  DC := GetWindowDC(AControl.Handle);
  AOut.Width := ARect.Width;
  AOut.Height := ARect.Height;
  with AOut do
    BitBlt(Canvas.Handle, 0, 0, Width, Height, DC, ARect.Left, ARect.Top, SrcCopy);
  ReleaseDC(AControl.Handle, DC);
end;

procedure TFrmMain.BtnSelForColorClick(Sender: TObject);
var
  Frm: TDialogColorSelector;
  OldColor: TColor;
begin
  Frm := TDialogColorSelector.Create(Self);
  try
    OldColor := CblForeground.Selected;
    Frm.OnChange := OnSelForegroundColorChange;
    Frm.SelectedColor := CblForeground.Selected;
    if Frm.Execute then
    begin
      CblForeground.Selected := Frm.SelectedColor;
      CblForegroundChange(CblForeground);
    end
    else if CblForeground.Selected <> OldColor then
    begin
      CblForeground.Selected := OldColor;
      CblForegroundChange(CblForeground);
    end;
  finally
    Frm.Free;
  end;
end;

procedure TFrmMain.BtnSelBackColorClick(Sender: TObject);
var
  Frm: TDialogColorSelector;
  OldColor: TColor;
begin
  Frm := TDialogColorSelector.Create(Self);
  try
    OldColor := CblBackground.Selected;
    Frm.SelectedColor := CblBackground.Selected;
    Frm.OnChange := OnSelBackGroundColorChange;
    if Frm.Execute then
    begin
      CblBackground.Selected := Frm.SelectedColor;
      CblForegroundChange(CblBackground);
    end
    else if CblBackground.Selected <> OldColor then
    begin
      CblBackground.Selected := OldColor;
      CblForegroundChange(CblBackground);
    end;
  finally
    Frm.Free;
  end;
end;

procedure TFrmMain.OnSelForegroundColorChange(Sender: TObject);
begin
  if Sender is TDialogColorSelector then
    CblForeground.Selected := TDialogColorSelector(Sender).SelectedColor
  else if Sender is TColorPanel then
    CblForeground.Selected := TColorPanel(Sender).SelectedColor;

  CblForegroundChange(nil);
end;

procedure TFrmMain.OnSelBackGroundColorChange(Sender: TObject);
begin
  if Sender is TDialogColorSelector then
    CblBackground.Selected := TDialogColorSelector(Sender).SelectedColor
  else if Sender is TColorPanel then
    CblBackground.Selected := TColorPanel(Sender).SelectedColor;

  CblForegroundChange(nil);
end;

procedure TFrmMain.ImportDelphiThemeRegistry();
var
  ThemeName: string;
  ImpTheme: TIDETheme;
  i: integer;
begin
  try
    if ComboBoxExIDEs.ItemIndex >= 0 then
      if MessageDlg(Format('Do you want import the current theme from  the "%s" IDE?', [IDEData.Name]), mtConfirmation, [mbYes, mbNo], 0) = mrYes
      then
      begin
        if not ExistDelphiIDEThemeToImport(IDEData) then
        begin
          MsgBox(Format('The "%s" IDE has not themes stored in the windows registry?', [IDEData.Name]));
          Exit;
        end;

        ThemeName := InputBox('Import Delphi IDE Theme', 'Enter the name for the theme to import', '');
        if ThemeName <> '' then
        begin
          ImportDelphiIDEThemeFromReg(ImpTheme, IDEData);
          if IsValidDelphiIDETheme(ImpTheme) then
          begin
            FCurrentTheme := ImpTheme;
            // SaveDelphiIDEThemeToXmlFile(DelphiVersion, FCurrentTheme, FSettings.ThemePath, ThemeName);
            SaveDelphiIDEThemeToXmlFile(FCurrentTheme, FSettings.ThemePath, ThemeName);
            EditThemeName.Text := ThemeName;
            MsgBox('Theme imported');
            LoadThemes;
            for i := 0 to FDITEThemes.Count - 1 do
              if SameText(FDITEThemes[i].Name, EditThemeName.Text) then
              begin
                clThemes.ItemIndex := i;
                Break;
              end;
          end
          else
            MsgBox('The imported theme has invalid values, the theme will be discarded');

        end;
      end;
  except
    on E: Exception do
      MsgBox(Format('Error importing theme from registry - Message : %s : Trace %s', [E.Message, E.StackTrace]));
  end;
end;

procedure TFrmMain.ImportEclipseTheme1Click(Sender: TObject);
begin
  ImportTheme(TIDEImportThemes.EclipseTheme);
end;

procedure TFrmMain.ImportTheme(ImportType: TIDEImportThemes);
var
  ThemeName: string;
  LIndex: integer;
  GoNext: boolean;
  LStopWatch: TStopwatch;
begin
  // ImportType := TIDEImportThemes(
  // CbIDEThemeImport.Items.Objects[CbIDEThemeImport.ItemIndex]);
  OpenDialogImport.Filter := IDEImportThemesDialogFilter[ImportType];
  OpenDialogImport.InitialDir := ExtractFilePath(ParamStr(0));
  try
    if (ComboBoxExIDEs.ItemIndex >= 0) and OpenDialogImport.Execute(Handle) then
    begin
      if OpenDialogImport.Files.Count = 1 then
        GoNext := MessageDlg(Format('Do you want import the "%s" file?', [ExtractFileName(OpenDialogImport.FileName)]), mtConfirmation,
          [mbYes, mbNo], 0) = mrYes
      else
        GoNext := MessageDlg(Format('Do you want import the %d files selected?', [OpenDialogImport.Files.Count]), mtConfirmation,
          [mbYes, mbNo], 0) = mrYes;

      if GoNext and (OpenDialogImport.Files.Count > 1) then
      begin
        LStopWatch := TStopwatch.Create;
        LStopWatch.Start;
        ProgressBar1.Visible := True;
        try
          ProgressBar1.Position := 0;
          ProgressBar1.Max := OpenDialogImport.Files.Count;
          LabelMsg.Visible := True;
          for LIndex := 0 to OpenDialogImport.Files.Count - 1 do
          begin
            LabelMsg.Caption := Format('Importing %s theme', [ExtractFileName(OpenDialogImport.Files[LIndex])]);
            case ImportType of
              VisualStudioThemes: ImportVisualStudioTheme(IDEData, OpenDialogImport.Files[LIndex], FSettings.ThemePath, ThemeName);
              EclipseTheme: ImportEclipseTheme(IDEData, OpenDialogImport.Files[LIndex], FSettings.ThemePath, ThemeName);
            end;
            ProgressBar1.Position := LIndex;
          end;
        finally
          ProgressBar1.Visible := False;
          LabelMsg.Visible := False;
        end;

        LStopWatch.Stop;
        MsgBox(Format('%d Themes imported in %n seconds', [OpenDialogImport.Files.Count, LStopWatch.Elapsed.TotalSeconds]));
        LoadThemes;
        clThemes.ItemIndex := 0;
      end
      else if GoNext and (OpenDialogImport.Files.Count = 1) then
      begin

        case ImportType of
          VisualStudioThemes:
            GoNext := ImportVisualStudioTheme(IDEData, OpenDialogImport.FileName, FSettings.ThemePath, ThemeName);
          EclipseTheme:
            GoNext := ImportEclipseTheme(IDEData, OpenDialogImport.FileName, FSettings.ThemePath, ThemeName);
        end;

        if GoNext then
        begin
          EditThemeName.Text := ThemeName;
          MsgBox(Format('"%s" Theme imported', [ThemeName]));
          LoadThemes;
          LIndex := GetThemeIndex(EditThemeName.Text);
          if LIndex >= 0 then
            clThemes.ItemIndex := LIndex;
        end;
      end;
    end;
  except
    on E: Exception do
      MsgBox(Format('Error to import theme(s) - Message : %s : Trace %s', [E.Message, E.StackTrace]));
  end;
end;

procedure TFrmMain.ImportVSTheme1Click(Sender: TObject);
begin
  ImportTheme(TIDEImportThemes.VisualStudioThemes);
end;

procedure TFrmMain.CbElementChange(Sender: TObject);
begin
  LoadValuesElements;
end;

procedure TFrmMain.CbIDEFontsChange(Sender: TObject);
begin
  if not FLoading and (FDITEThemes.Count > 0) then
  begin
    SynEditCode.Font.Name := CbIDEFonts.Text;
    SynEditCode.Font.Size := StrToInt(EditFontSize.Text);
    BtnApplyFont.Enabled := True;
    RefreshPasSynEdit;
  end;
end;

procedure TFrmMain.LoadNCControls;
Var
  LNCButton: TNCButton;
begin
  NCControls := TNCControls.Create(Self);
  NCControls.ShowSystemMenu := False;
  NCControls.Images := ImageListNCArea;

  LNCButton := NCControls.Controls.AddEx<TNCButton>;
  LNCButton.Style := nsSplitButton;
  LNCButton.ImageStyle := isGrayHot;
  LNCButton.ImageIndex := 0;
  LNCButton.BoundsRect := Rect(5, 5, 75, 25);
  LNCButton.Caption := 'Menu';
  LNCButton.DropDownMenu := PopupMenuThemes;
  LNCButton.FontColor := StyleServices(nil).GetSystemColor(clWindowText);
  LNCButton.HotFontColor := LNCButton.FontColor;
  // LNCButton.OnClick := ButtonNCClick;

  LNCButton := NCControls.Controls.AddEx<TNCButton>;
  LNCButton.Style := nsTranparent;
  LNCButton.ImageStyle := isGrayHot;
  LNCButton.ImageIndex := 3;
  LNCButton.BoundsRect := Rect(78, 5, 98, 25);
  LNCButton.Name := 'NCHue';
  LNCButton.ShowHint := True;
  LNCButton.Hint := 'Change Hue/Saturation';
  LNCButton.Caption := '';
  LNCButton.OnClick := ImageHueClick;

  LNCButton := NCControls.Controls.AddEx<TNCButton>;
  LNCButton.Style := nsTranparent;
  LNCButton.ImageStyle := isGrayHot;
  LNCButton.ImageIndex := 2;
  LNCButton.BoundsRect := Rect(101, 5, 121, 25);
  LNCButton.Name := 'NCConf';
  LNCButton.ShowHint := True;
  LNCButton.Hint := 'Settings';
  LNCButton.Caption := '';
  LNCButton.OnClick := ImageConfClick;

  LNCButton := NCControls.Controls.AddEx<TNCButton>;
  LNCButton.Style := nsTranparent;
  LNCButton.ImageStyle := isGrayHot;
  LNCButton.ImageIndex := 1;
  LNCButton.BoundsRect := Rect(124, 5, 144, 25);
  LNCButton.Name := 'NCBug';
  LNCButton.ShowHint := True;
  LNCButton.Hint := 'Report Bugs';
  LNCButton.Caption := '';
  LNCButton.OnClick := ImageBugClick;

  LNCButton := NCControls.Controls.AddEx<TNCButton>;
  LNCButton.Style := nsTranparent;
  LNCButton.ImageStyle := isGrayHot;
  LNCButton.ImageIndex := 4;
  LNCButton.BoundsRect := Rect(147, 5, 167, 25);
  LNCButton.Name := 'NCUpdate';
  LNCButton.ShowHint := True;
  LNCButton.Hint := 'Check for updates';
  LNCButton.Caption := '';
  LNCButton.OnClick := ImageUpdateClick;

  LNCButton := NCControls.Controls.AddEx<TNCButton>;
  LNCButton.Style := nsTranparent;
  LNCButton.ImageStyle := isGrayHot;
  LNCButton.ImageIndex := 5;
  LNCButton.BoundsRect := Rect(170, 5, 190, 25);
  LNCButton.Name := 'NCUpdate';
  LNCButton.ShowHint := True;
  LNCButton.Hint := 'Switch Compact/Full';
  LNCButton.Caption := '';
  LNCButton.OnClick := SwitchModeClick;
end;

procedure TFrmMain.LoadIcons;
var
  LIndex : Integer;
begin
  Icons := TObjectDictionary<string, TIcon>.Create([doOwnsValues]);

  Icons.Add('Apply', TIcon.Create);
  Icons['Apply'].Handle := FontAwesome.GetIcon(fa_check, 16, 16,
    StyleServices.GetSystemColor(clMenuText), StyleServices.GetSystemColor(clMenu), 0, TImageAlignment.iaCenter);
  LIndex := ImageList1.AddIcon(Icons['Apply']);
  ActionApplyTheme.ImageIndex := LIndex;

  Icons.Add('delete', TIcon.Create);
  Icons['delete'].Handle := FontAwesome.GetIcon(fa_remove, 16, 16,
    StyleServices.GetSystemColor(clMenuText), StyleServices.GetSystemColor(clMenu), 0, TImageAlignment.iaCenter);
  LIndex := ImageList1.AddIcon(Icons['delete']);
  ActionDeleteTheme.ImageIndex := LIndex;

  Icons.Add('export', TIcon.Create);
  Icons['export'].Handle := FontAwesome.GetIcon(fa_chevron_right, 16, 16,
    StyleServices.GetSystemColor(clMenuText), StyleServices.GetSystemColor(clMenu), 0, TImageAlignment.iaCenter);
  LIndex := ImageList1.AddIcon(Icons['export']);
  ActionExoLazarusClrSch.ImageIndex := LIndex;

  Icons.Add('import', TIcon.Create);
  Icons['import'].Handle := FontAwesome.GetIcon(fa_chevron_left, 16, 16,
    StyleServices.GetSystemColor(clMenuText), StyleServices.GetSystemColor(clMenu), 0, TImageAlignment.iaCenter);
  LIndex := ImageList1.AddIcon(Icons['import']);
  ActionImportThemeReg.ImageIndex := LIndex;

  Icons.Add('github', TIcon.Create);
  Icons['github'].Handle := FontAwesome.GetIcon(fa_github, 24, 24,
    StyleServices.GetSystemColor(clHighlight), StyleServices.GetSystemColor(clBtnFace), 0, TImageAlignment.iaCenter);
end;

type
  TCustomComboClass = class(TCustomCombo);

procedure ComboBox_SetDroppedWidth(const ACustomCombo: TCustomCombo);
const
  PADDING = 65; //padding  + image
var
  LNewWidth, i, LWidth: integer;
begin
  LNewWidth := 0;
  for i := 0 to ACustomCombo.Items.Count -1 do
  begin
    LWidth := ACustomCombo.Canvas.TextWidth(ACustomCombo.Items[i]);
    Inc(LWidth, PADDING);
    if (LWidth > LNewWidth) then
      LNewWidth := LWidth;
  end;

  if (LNewWidth > ACustomCombo.Width) then
  begin
    if TCustomComboClass(ACustomCombo).DropDownCount < ACustomCombo.Items.Count then
      LNewWidth := LNewWidth + GetSystemMetrics(SM_CXVSCROLL);
    SendMessage(ACustomCombo.Handle, CB_SETDROPPEDWIDTH, LNewWidth, 0);
  end;
end;


//
//function ListItemsCompare(List: TListControlItems; Index1, Index2: Integer): Integer;
//begin
//  Result := CompareStr(List.Items[Index1].Caption, List.Items[Index2].Caption);
//end;

procedure TFrmMain.FormCreate(Sender: TObject);
Var
  LIDEData: TDelphiVersionData;
  Index: integer;
begin
  FCompactMode := False;
  CompactMode := True;

  Screen.MenuFont.Name := 'Calibri';
  Screen.MenuFont.Size := 9;

  FChanging := False;
  FSettings := TSettings.Create;
  ReadSettings(FSettings);
  LoadVCLStyle(FSettings.VCLStyle);

  FDITEThemes := TObjectList<TDITETheme>.Create(True);

  //ImageList1
  LoadNCControls();

  IDEsList := TList<TDelphiVersionData>.Create;
  FillListDelphiVersions(IDEsList);

  if IsLazarusInstalled then
    FillListLazarusVersions(IDEsList);

  if IsSMSInstalled then
    FillListSMSVersions(IDEsList);

  // FillListAppMethodVersions(IDEsList);

  for Index := 0 to IDEsList.Count - 1 do
  begin
    LIDEData := IDEsList[Index];
    ImageList_AddIcon(ImageListDelphiVersion.Handle, LIDEData.Icon.Handle);
    ComboBoxExIDEs.ItemsEx.AddItem(LIDEData.Name, ImageListDelphiVersion.Count - 1, ImageListDelphiVersion.Count - 1,
      ImageListDelphiVersion.Count - 1, 0, IDEsList[Index]);
  end;

  if ComboBoxExIDEs.Items.Count = 0 then
  begin
    MsgBox('You don''t have a Object Pascal IDE installed');
    Halt(0);
  end;
  ComboBox_SetDroppedWidth(ComboBoxExIDEs);
  ComboBoxExIDEs.ItemIndex := 0;

  //ComboBoxExIDEs.ItemsEx.CustomSort(ListItemsCompare);

  ActionImages := TObjectDictionary<string, TCompPngImages>.Create([doOwnsValues]);

  FillPopupActionBar(PopupActionBar1);
  AssignStdActionsPopUpMenu(Self, PopupActionBar1);

  LoadIcons();
  Image1.Picture.Icon.Assign(Icons['github']);

  { .$WARN SYMBOL_PLATFORM OFF }
  // BtnIDEColorizer.Visible:=DebugHook<>0;
  { .$WARN SYMBOL_PLATFORM ON }
  // BtnIDEColorizer.Visible:=FSettings.ActivateColorizer;

  FMapHighlightElementsTSynAttr := TStringList.Create;
  with SynPasSyn1 do
  begin
    FMapHighlightElementsTSynAttr.AddObject(IDEHighlightElementsNames[TIDEHighlightElements.Assembler], AsmAttri);
    FMapHighlightElementsTSynAttr.AddObject(IDEHighlightElementsNames[TIDEHighlightElements.Character], CharAttri);
    FMapHighlightElementsTSynAttr.AddObject(IDEHighlightElementsNames[TIDEHighlightElements.Comment], CommentAttri);
    FMapHighlightElementsTSynAttr.AddObject(IDEHighlightElementsNames[TIDEHighlightElements.Preprocessor], DirectiveAttri);
    FMapHighlightElementsTSynAttr.AddObject(IDEHighlightElementsNames[TIDEHighlightElements.Float], FloatAttri);
    FMapHighlightElementsTSynAttr.AddObject(IDEHighlightElementsNames[TIDEHighlightElements.Hex], HexAttri);
    FMapHighlightElementsTSynAttr.AddObject(IDEHighlightElementsNames[TIDEHighlightElements.Identifier], IdentifierAttri);
    FMapHighlightElementsTSynAttr.AddObject(IDEHighlightElementsNames[TIDEHighlightElements.ReservedWord], KeyAttri);
    FMapHighlightElementsTSynAttr.AddObject(IDEHighlightElementsNames[TIDEHighlightElements.Number], NumberAttri);
    FMapHighlightElementsTSynAttr.AddObject(IDEHighlightElementsNames[TIDEHighlightElements.Whitespace], SpaceAttri);
    FMapHighlightElementsTSynAttr.AddObject(IDEHighlightElementsNames[TIDEHighlightElements.string], StringAttri);
    FMapHighlightElementsTSynAttr.AddObject(IDEHighlightElementsNames[TIDEHighlightElements.Symbol], SymbolAttri);
    FMapHighlightElementsTSynAttr.AddObject(IDEHighlightElementsNames[TIDEHighlightElements.LineNumber], NumberAttri);
  end;
  FThemeChangued := False;

  // FillListIDEThemesImport(CbIDEThemeImport.Items);
  // CbIDEThemeImport.ItemIndex := 0;

  Self.Caption := Caption + Format(' %s', [DITE.Misc.GetFileVersion(ParamStr(0))]);

  LoadFixedWidthFonts;

//  LoadThemes;
//  ComboBoxExIDEs.ItemIndex := 0;
//  ComboBoxExIDEsChange(ComboBoxExIDEs);
//
//  if FDITEThemes.Count > 0 then
//    clThemes.ItemIndex := 0;
end;

procedure TFrmMain.FormDestroy(Sender: TObject);
var
  Index: integer;
begin
  FreeAndNil(FDITEThemes);
  Icons.Free;

  FSettings.Free;
  FMapHighlightElementsTSynAttr.Free;

  for Index := 0 to IDEsList.Count - 1 do
    IDEsList[Index].Free;

  IDEsList.Free;

  ActionImages.Free;
end;

procedure TFrmMain.FormShow(Sender: TObject);
begin
  BtnApplyFont.Enabled := False;
  //ComboBoxExIDEsChange(ComboBoxExIDEs);
end;

function TFrmMain.GetDelphiVersionData(Index: integer): TDelphiVersionData;
begin
  Result := TDelphiVersionData(ComboBoxExIDEs.ItemsEx[Index].Data);
end;

function TFrmMain.GetElementIndex(Element: TIDEHighlightElements): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to CbElement.Items.Count - 1 do
    if TIDEHighlightElements(CbElement.Items.Objects[i]) = Element then
      Exit(i);
end;

function TFrmMain.GetIDEData: TDelphiVersionData;
begin
  FIDEData := GetDelphiVersionData(ComboBoxExIDEs.ItemIndex);
  Result := FIDEData;
end;

function TFrmMain.GetThemeIndex(const AThemeName: string): integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to FDITEThemes.Count - 1 do
    if SameText(FDITEThemes[i].Name, AThemeName) then
      Exit(i);
end;

procedure TFrmMain.ImageBugClick(Sender: TObject);
begin
  ShellExecute(Handle, 'open', 'https://github.com/RRUZ/delphi-ide-theme-editor/issues', nil, nil, SW_SHOWNORMAL);
end;

procedure TFrmMain.ImageBugMouseEnter(Sender: TObject);
begin
  TImage(Sender).Picture.Assign(ActionImages.Items[TImage(Sender).Name].Normal);
end;

procedure TFrmMain.ImageBugMouseLeave(Sender: TObject);
begin
  TImage(Sender).Picture.Assign(ActionImages.Items[TImage(Sender).Name].BN);
end;

procedure TFrmMain.ImageConfClick(Sender: TObject);
var
  LDialog: TFrmSettings;
begin
  if FLoading then exit;
  LDialog := TFrmSettings.Create(nil);
  try
    LDialog.Settings := FSettings;
    LDialog.LoadSettings;
    LDialog.ShowModal();
  finally
    LDialog.Free;
  end;
end;

procedure TFrmMain.ImageHueClick(Sender: TObject);
var
  LDialog: TFrmHueSat;
  DelphiVersion: TDelphiVersions;
  LIndex: integer;
  FBackUpTheme: TIDETheme;
begin
  if not FLoading and (ComboBoxExIDEs.ItemIndex >= 0) and (clThemes.ItemIndex <> -1) then
  begin
    LDialog := TFrmHueSat.Create(nil);
    try
      FBackUpTheme := FCurrentTheme;
      DelphiVersion := IDEData.Version;
      LDialog.SynEditor := SynEditCode;
      LDialog.Theme := FCurrentTheme;
      LDialog.DelphiVersion := DelphiVersion;
      LDialog.ThemeName := FDITEThemes[clThemes.ItemIndex].Name;
      LDialog.Settings := FSettings;
      LDialog.init;
      LDialog.ShowModal();

      if LDialog.Reloadthemes then
      begin
        LoadThemes;
        LIndex := GetThemeIndex(EditThemeName.Text);
        if LIndex >= 0 then
          clThemes.ItemIndex := LIndex;
      end
      else
      begin
        FCurrentTheme := FBackUpTheme;
        RefreshPasSynEdit;
      end;
    finally
      LDialog.Free;
    end;
  end;
end;

procedure TFrmMain.ImageUpdateClick(Sender: TObject);
begin
  CheckForUpdates(False);
end;

procedure TFrmMain.LinkLabel1LinkClick(Sender: TObject; const Link: string;
  LinkType: TSysLinkType);
begin
  ShellExecute(0, 'Open', PChar(Link), '', nil, SW_SHOWNORMAL);
end;

procedure TFrmMain.LoadFixedWidthFonts;
var
  sDC: integer;
  LogFont: TLogFont;
begin
  CbIDEFonts.Items.Clear;
  sDC := GetDC(0);
  try
    ZeroMemory(@LogFont, sizeof(LogFont));
    LogFont.lfCharset := DEFAULT_CHARSET;
    EnumFontFamiliesEx(sDC, LogFont, @EnumFixedFontsProc, Windows.LPARAM(CbIDEFonts.Items), 0);
  finally
    ReleaseDC(0, sDC);
  end;
end;

procedure TFrmMain.SwitchModeClick(Sender: TObject);
begin
  CompactMode := not FCompactMode;
end;

procedure TFrmMain.SetCompactMode(Value : Boolean);
begin
  if FLoading then exit;


  FCompactMode := Value;

  if FCompactMode then
  begin

    if (FrmColorPanel <> nil) then
    begin
       FrmColorPanel.Free();
       FrmColorPanel := nil;
       //FrmColorPanel.Visible := false;
       //Application.ProcessMessages();
    end;

    Self.Width := Self.Width - PanelColors.Width;
    SynEditCode.Top := 63;
    Self.Height:= 490;

    //AnimateWindow(PanelColors.Handle, 200, AW_HIDE or AW_SLIDE OR AW_VER_POSITIVE);
    PanelColors.Visible := False;
    GroupBoxTextAttr.Visible := false;
    GroupBoxUseDefaults.Visible := false;
    LabelElement.Visible := false;
    CbElement.Visible := false;
    BtnAdditionalSettings.Visible:=false;
    RadioButtonFore.Visible := false;
    CblForeground.Visible := false;
    RadioButtonBack.Visible := false;
    CblBackground.Visible := false;
    BtnSelForColor.Visible := false;
    BtnSelBackColor.Visible := false;

    PanelThemeName.Visible:=false;
    clThemes.Height := 380;
    BtnApplySmall.Visible := True;
    BtnApplySmall.Top := clThemes.Top + clThemes.Height + 5;
  end
  else
  begin
    BtnApplySmall.Visible := False;
    Self.Width := Self.Width + PanelColors.Width;
    Self.Height := 620;
    SynEditCode.Top := 192;
    clThemes.Height := 400;
    Application.ProcessMessages();
    PanelColors.Visible := True;
    GroupBoxTextAttr.Visible := True;
    GroupBoxUseDefaults.Visible := True;
    LabelElement.Visible := True;
    CbElement.Visible := True;
    BtnAdditionalSettings.Visible := True;
    BtnSave.Visible := True;

    RadioButtonFore.Visible := True;
    CblForeground.Visible := True;
    RadioButtonBack.Visible := True;
    CblBackground.Visible := True;

    BtnSelForColor.Visible := True;
    BtnSelBackColor.Visible := True;
    PanelThemeName.Visible := True;

    Application.ProcessMessages();
    if (FrmColorPanel = nil) then
    begin
      FrmColorPanel := TColorPanel.Create(PanelColors);
      FrmColorPanel.Parent := PanelColors;
      FrmColorPanel.BorderStyle := bsNone;
      FrmColorPanel.Align := alClient;
      FrmColorPanel.OnChange := OnSelForegroundColorChange;
      FrmColorPanel.Show;
    end;
  end;
end;

procedure TFrmMain.LoadThemes;
var
  LTheme: TDITETheme;
  i: Integer;
  LFiles: TArray<string>;
  IsLightTheme: Boolean;

  procedure UpdateControls(AEnabled: boolean);
  begin
    chbDark.Enabled := AEnabled;
    chbLight.Enabled := AEnabled;
    BtnApply.Enabled := AEnabled;
    ComboBoxExIDEs.Enabled := AEnabled;
    CbIDEFonts.Enabled := AEnabled;
    EditFontSize.Enabled := AEnabled;
    lbIDEs.Enabled := AEnabled;
    lbFont.Enabled := AEnabled;
  end;

begin
  if FLoading or not TDirectory.Exists(FSettings.ThemePath) then
    Exit;

  FLoading := True;
  try
    FDITEThemes.Clear;
    clThemes.ItemCount := 0;

    LFiles := TDirectory.GetFiles(FSettings.ThemePath, '*.theme.xml');
    UpdateControls(False);
    ProgressBar1.Visible := True;
    try
      ProgressBar1.Position := 0;
      ProgressBar1.Max := Length(LFiles);
      //LabelMsg.Visible := True;
      for i := 0 to Length(LFiles) - 1 do
      begin
        //LabelMsg.Caption := Format('Exporting %s theme', [ExtractFileName(OpenDialogExport.Files[i])]);
        //DelphiIDEThemeToLazarusTheme(OpenDialogExport.Files[i], OutPutFolder);
        LTheme := TDITETheme.Create(LFiles[i]);
        IsLightTheme := ColorIsBright(StringToColor(LTheme.Theme[ReservedWord].BackgroundColorNew));

        if (chbDark.Checked and not IsLightTheme) or (chbLight.Checked and IsLightTheme) then
          FDITEThemes.Add(LTheme)
        else
          FreeAndNil(LTheme);

        ProgressBar1.Position := i;

        if i mod 10 = 0 then
        begin
          clThemes.ItemCount := FDITEThemes.Count;
          if clThemes.ItemIndex = -1 then
            clThemes.ItemIndex := 0;
          Application.ProcessMessages;
        end;
      end;
    finally
      ProgressBar1.Visible := False;
      UpdateControls(True);
      //LabelMsg.Visible := False;
    end;

    //for FileName in LFiles  do

    FThemeChangued := False;
    clThemes.ItemCount := FDITEThemes.Count;
    //TLoadThemesImages.Create(FSettings.ThemePath, ImageListThemes, LvThemes);
  finally
    FLoading := False;
  end;
end;

procedure TFrmMain.LoadValuesElements;
var
  Element: TIDEHighlightElements;
begin
  if ComboBoxExIDEs.ItemIndex >= 0 then
  begin
    Element := TIDEHighlightElements(CbElement.Items.Objects[CbElement.ItemIndex]);
    CblForeground.Selected := StringToColor(FCurrentTheme[Element].ForegroundColorNew);
    CblBackground.Selected := StringToColor(FCurrentTheme[Element].BackgroundColorNew);

    FChanging := True;
    try

      if FrmColorPanel <> nil then
      begin
        if RadioButtonFore.Checked then
        begin
          FrmColorPanel.SelectedColor := CblForeground.Selected;
          FrmColorPanel.OnChange := OnSelForegroundColorChange;
        end
        else
        begin
          FrmColorPanel.SelectedColor := CblBackground.Selected;
          FrmColorPanel.OnChange := OnSelBackGroundColorChange;
        end;
      end;

      CheckBold.Checked := FCurrentTheme[Element].Bold;
      CheckItalic.Checked := FCurrentTheme[Element].Italic;
      CheckUnderline.Checked := FCurrentTheme[Element].Underline;

      CheckForeground.Checked := FCurrentTheme[Element].DefaultForeground;
      CheckBackground.Checked := FCurrentTheme[Element].DefaultBackground;
    finally
      FChanging := False;
    end;
  end;
end;

procedure TFrmMain.LvIDEVersionsChange(Sender: TObject; Item: TListItem; Change: TItemChange);
var
  DelphiVersion: TDelphiVersions;
begin
  if ComboBoxExIDEs.ItemIndex >= 0 then
  begin
    if IDEData.IDEType = TSupportedIDEs.DelphiIDE then
      DelphiVersion := IDEData.Version
    else
      // if IDEType=TSupportedIDEs.LazarusIDE then
      DelphiVersion := TDelphiVersions.DelphiXE; // if is lazarus use the Delphi XE elemnents

    FillListAvailableElements(IDEData, CbElement.Items);

    SynEditCode.Gutter.Visible := DelphiVersionNumbers[DelphiVersion] >= IDEHighlightElementsMinVersion[TIDEHighlightElements.LineNumber];

    SynEditCode.Gutter.ShowLineNumbers := DelphiVersionNumbers[DelphiVersion] >= IDEHighlightElementsMinVersion
      [TIDEHighlightElements.LineNumber];

    if IDEData.IDEType = TSupportedIDEs.DelphiIDE then
      UpDownFontSize.Position := GetDelphiIDEFontSize(IDEData)
    else if IDEData.IDEType = TSupportedIDEs.LazarusIDE then
      UpDownFontSize.Position := GetLazarusIDEFontSize;

    if CbIDEFonts.Items.Count > 0 then
      if IDEData.IDEType = TSupportedIDEs.DelphiIDE then
        CbIDEFonts.ItemIndex := CbIDEFonts.Items.IndexOf(GetDelphiIDEFontName(IDEData))
      else if IDEData.IDEType = TSupportedIDEs.LazarusIDE then
        CbIDEFonts.ItemIndex := CbIDEFonts.Items.IndexOf(GetLazarusIDEFontName);

    CbIDEFontsChange(nil);
    BtnApplyFont.Enabled := False;

    // BtnImportRegTheme.Visible:=not DelphiIsOldVersion(DelphiVersion) and (IDEData.IDEType=TSupportedIDEs.DelphiIDE);
    // BtnExportToLazarusTheme.Visible:=(IDEData.IDEType=TSupportedIDEs.LazarusIDE);

    if (clThemes.ItemIndex <> -1) and (CbElement.Items.Count > 0) then
    begin
      CbElement.ItemIndex := 0;
      LoadValuesElements;
    end;
  end;
end;

procedure TFrmMain.CblForegroundChange(Sender: TObject);
var
  Element: TIDEHighlightElements;
begin
  if (ComboBoxExIDEs.ItemIndex >= 0) and (not FChanging) then
  begin
    Element := TIDEHighlightElements(CbElement.Items.Objects[CbElement.ItemIndex]);
    FCurrentTheme[Element].ForegroundColorNew := ColorToString(CblForeground.Selected);
    FCurrentTheme[Element].BackgroundColorNew := ColorToString(CblBackground.Selected);

    FCurrentTheme[Element].Bold := CheckBold.Checked;
    FCurrentTheme[Element].Italic := CheckItalic.Checked;
    FCurrentTheme[Element].Underline := CheckUnderline.Checked;

    FCurrentTheme[Element].DefaultForeground := CheckForeground.Checked;
    FCurrentTheme[Element].DefaultBackground := CheckBackground.Checked;

    RefreshPasSynEdit;

    if Sender <> nil then
    begin
      if RadioButtonFore.Checked then
      begin
        FrmColorPanel.SelectedColor := CblForeground.Selected;
        FrmColorPanel.OnChange := OnSelForegroundColorChange;
      end
      else
      begin
        FrmColorPanel.SelectedColor := CblBackground.Selected;
        FrmColorPanel.OnChange := OnSelBackGroundColorChange;
      end;
    end;
  end;
end;

procedure TFrmMain.chbLightClick(Sender: TObject);
begin
  LoadThemes;
end;

procedure TFrmMain.clThemesBeforeDrawItem(AIndex: Integer; ACanvas: TCanvas;
  ARect: TRect; AState: TOwnerDrawState);
const
  cThemeTypeStr: Array[Boolean] of string = ('Dark', 'Light');
begin
  if (AIndex < 0) or (AIndex > FDITEThemes.Count - 1) then exit;

  lbTheme.Caption := FDITEThemes[AIndex].Name;
  lbThemetype.Caption := Format('%s Theme',
    [cThemeTypeStr[ColorIsBright(StringToColor(FDITEThemes[AIndex].Theme[ReservedWord].BackgroundColorNew))]]);


  if not (odSelected in AState) then
    lbThemetype.Font.Color := ColorBlendRGB(StyleServices.GetSystemColor(clWindow),
    StyleServices.GetSystemColor(clWindowText), 0.6)
  else
    lbThemetype.Font.Color := ColorBlendRGB(StyleServices.GetSystemColor(clHighlight),
    StyleServices.GetSystemColor(clHighlightText), 0.8);


  //lbAuthor.Caption := FDITEThemes[AIndex].Author;
  //lbmodified.Caption := DateToStr(FDITEThemes[AIndex].Modified);

  if FDITEThemes[AIndex].Bitmap = nil then
  begin
    var LBitmap := TBitmap.Create;
    CreateThemeBmp(32, 32,
      StringToColor(FDITEThemes[AIndex].Theme[ReservedWord].BackgroundColorNew),
      StringToColor(FDITEThemes[AIndex].Theme[ReservedWord].ForegroundColorNew),
      StringToColor(FDITEThemes[AIndex].Theme[Identifier].ForegroundColorNew), LBitmap);
    FDITEThemes[AIndex].Bitmap := LBitmap;
  end;

  ACanvas.Draw(ARect.Left + 8, ARect.Top + 8, FDITEThemes[AIndex].Bitmap);
end;

procedure TFrmMain.clThemesItemClick(Sender: TObject);
begin
  try
    if (clThemes.ItemIndex <> -1) and (ComboBoxExIDEs.ItemIndex >= 0) then
    begin
      EditThemeName.Text := FDITEThemes[clThemes.ItemIndex].Name;
      if IsValidDelphiIDETheme(FDITEThemes[clThemes.ItemIndex].Theme) then
      begin
        FCurrentTheme := FDITEThemes[clThemes.ItemIndex].Theme;
        RefreshPasSynEdit;
        if CbElement.Items.Count > 0 then
        begin
          CbElement.ItemIndex := 0;
          LoadValuesElements;
          // PaintGutterGlyphs;
          // SynEditCode.InvalidateGutter;
        end;
      end
      else
        MsgBox(Format('The Theme %s has invalid values', [FDITEThemes[clThemes.ItemIndex].Name]));
    end;
  except
    on E: Exception do
      MsgBox(Format('Error loading values of current theme - Message : %s : Trace %s', [E.Message, E.StackTrace]));
  end;
end;

procedure TFrmMain.CMStyleChanged(var Message: TMessage);
begin
  CbElement.Style := csDropDownList;
  CbElement.OnDrawItem := nil;

  CbIDEFonts.Style := csDropDownList;
  CbIDEFonts.OnDrawItem := nil;
end;

procedure TFrmMain.ComboBoxExIDEsChange(Sender: TObject);
var
  DelphiVersion: TDelphiVersions;
  CurrentThemeName: string;
  i: integer;
begin
  if not FLoading and (ComboBoxExIDEs.ItemIndex >= 0) then
  begin
    CurrentThemeName := '';
    if IDEData.IDEType = TSupportedIDEs.DelphiIDE then
      DelphiVersion := IDEData.Version
    else
      // if IDEType=TSupportedIDEs.LazarusIDE then
      DelphiVersion := TDelphiVersions.DelphiXE; // if is lazarus, SMS or Appmethod use the Delphi XE elements

    // BtnIDEColorizer.Enabled:=FSettings.ActivateColorizer and (IDEData.IDEType=TSupportedIDEs.DelphiIDE) and  (IDEData.Version in [TDelphiVersions.DelphiXE, TDelphiVersions.DelphiXE2]);

    BtnAdditionalSettings.Visible :=  not CompactMode and (DelphiVersion >= TDelphiVersions.DelphiXE8);

    FillListAvailableElements(IDEData, CbElement.Items);

    SynEditCode.Gutter.Visible := DelphiVersionNumbers[DelphiVersion] >= IDEHighlightElementsMinVersion[TIDEHighlightElements.LineNumber];

    SynEditCode.Gutter.ShowLineNumbers := DelphiVersionNumbers[DelphiVersion] >= IDEHighlightElementsMinVersion
      [TIDEHighlightElements.LineNumber];

    if IDEData.IDEType = TSupportedIDEs.DelphiIDE then
    begin
      UpDownFontSize.Position := GetDelphiIDEFontSize(IDEData);
      CurrentThemeName := GetDelphiIDEThemeName(IDEData);
      if CurrentThemeName = '' then
        CurrentThemeName := 'default';
    end
    else if IDEData.IDEType = TSupportedIDEs.LazarusIDE then
    begin
      UpDownFontSize.Position := GetLazarusIDEFontSize;
      CurrentThemeName := GetLazarusIDEThemeName;
    end
    else if IDEData.IDEType = TSupportedIDEs.SMSIDE then
    begin
      UpDownFontSize.Position := GetSMSIDEFontSize;
      CurrentThemeName := GetSMSIDEThemeName;
    end;

    if CbIDEFonts.Items.Count > 0 then
      if IDEData.IDEType = TSupportedIDEs.DelphiIDE then
        CbIDEFonts.ItemIndex := CbIDEFonts.Items.IndexOf(GetDelphiIDEFontName(IDEData))
      else if IDEData.IDEType = TSupportedIDEs.LazarusIDE then
        CbIDEFonts.ItemIndex := CbIDEFonts.Items.IndexOf(GetLazarusIDEFontName)
      else if IDEData.IDEType = TSupportedIDEs.SMSIDE then
        CbIDEFonts.ItemIndex := CbIDEFonts.Items.IndexOf(GetSMSIDEFontName);

    CbIDEFontsChange(nil);
    BtnApplyFont.Enabled := False;

    // BtnImportRegTheme.Visible:=not DelphiIsOldVersion(DelphiVersion) and (IDEData.IDEType=TSupportedIDEs.DelphiIDE);
    // BtnExportToLazarusTheme.Visible:=(IDEData.IDEType=TSupportedIDEs.LazarusIDE);

    if CurrentThemeName <> '' then
      for i := 0 to FDITEThemes.Count - 1 do
        if SameText(CurrentThemeName, FDITEThemes[i].Name) then
        begin
          clThemes.ItemIndex := i;
          Break;
        end;

    if (clThemes.ItemIndex <> -1) and (CbElement.Items.Count > 0) then
    begin
      CbElement.ItemIndex := 0;
      LoadValuesElements;
    end;
  end;
end;

procedure TFrmMain.Contribute1Click(Sender: TObject);
begin
  ShellExecute(Handle, 'open', PChar('https://github.com/RRUZ/delphi-ide-theme-editor'), nil, nil, SW_SHOW);
end;

procedure TFrmMain.CreateThemeFile;
var
  FileName: string;
begin
  if (ComboBoxExIDEs.ItemIndex >= 0) then
  begin
    if EditThemeName.Text = '' then
      MsgBox('You must enter a name for the current theme')
    else
    begin
      FileName := SaveDelphiIDEThemeToXmlFile(FCurrentTheme, FSettings.ThemePath, EditThemeName.Text);
      MsgBox(Format('The theme was saved to the file %s', [FileName]));
    end;
  end;
end;

procedure TFrmMain.RadioButtonForeClick(Sender: TObject);
begin
  CblForeground.Enabled := RadioButtonFore.Checked;
  BtnSelForColor.Enabled := RadioButtonFore.Checked;

  CblBackground.Enabled := RadioButtonBack.Checked;
  BtnSelBackColor.Enabled := RadioButtonBack.Checked;

  if RadioButtonFore.Checked  and (FrmColorPanel <> nil) then
  begin
    FrmColorPanel.OnChange := OnSelForegroundColorChange;
    FrmColorPanel.SelectedColor := CblForeground.Selected;
  end
  else
  if (FrmColorPanel <> nil) then
  begin
    FrmColorPanel.OnChange := OnSelBackGroundColorChange;
    FrmColorPanel.SelectedColor := CblBackground.Selected;
  end;
end;

procedure TFrmMain.RefreshPasSynEdit;
var
  Element: TIDEHighlightElements;
begin
  if (ComboBoxExIDEs.ItemIndex >= 0) and (clThemes.ItemIndex <> -1) then
  begin
    Element := TIDEHighlightElements.RightMargin;
    SynEditCode.RightEdgeColor := GetDelphiVersionMappedColor(StringToColor(FCurrentTheme[Element].ForegroundColorNew), IDEData);

    Element := TIDEHighlightElements.MarkedBlock;
    SynEditCode.SelectedColor.Foreground := GetDelphiVersionMappedColor(StringToColor(FCurrentTheme[Element].ForegroundColorNew),
      IDEData);
    SynEditCode.SelectedColor.Background := GetDelphiVersionMappedColor(StringToColor(FCurrentTheme[Element].BackgroundColorNew),
      IDEData);

    Element := TIDEHighlightElements.LineNumber;
    SynEditCode.Gutter.Color := StringToColor(FCurrentTheme[Element].BackgroundColorNew);
    SynEditCode.Gutter.Font.Color := GetDelphiVersionMappedColor(StringToColor(FCurrentTheme[Element].ForegroundColorNew), IDEData);

    Element := TIDEHighlightElements.LineHighlight;
    SynEditCode.ActiveLineColor := GetDelphiVersionMappedColor(StringToColor(FCurrentTheme[Element].BackgroundColorNew), IDEData);

    Element := TIDEHighlightElements.PlainText;
    SynEditCode.Gutter.BorderColor := GetHighLightColor(StringToColor(FCurrentTheme[Element].BackgroundColorNew));

    with SynPasSyn1 do
    begin
      SetSynAttr(TIDEHighlightElements.Assembler, AsmAttri, IDEData);
      SetSynAttr(TIDEHighlightElements.Character, CharAttri, IDEData);
      SetSynAttr(TIDEHighlightElements.Comment, CommentAttri, IDEData);
      SetSynAttr(TIDEHighlightElements.Preprocessor, DirectiveAttri, IDEData);
      SetSynAttr(TIDEHighlightElements.Float, FloatAttri, IDEData);
      SetSynAttr(TIDEHighlightElements.Hex, HexAttri, IDEData);
      SetSynAttr(TIDEHighlightElements.Identifier, IdentifierAttri, IDEData);
      SetSynAttr(TIDEHighlightElements.ReservedWord, KeyAttri, IDEData);
      SetSynAttr(TIDEHighlightElements.Number, NumberAttri, IDEData);
      SetSynAttr(TIDEHighlightElements.Whitespace, SpaceAttri, IDEData);
      SetSynAttr(TIDEHighlightElements.string, StringAttri, IDEData);
      SetSynAttr(TIDEHighlightElements.Symbol, SymbolAttri, IDEData);
    end;

    {
      SynEditCodeSpecialLineColors(nil,InvalidBreakLine, Special, FG, BG);
      SynEditCodeSpecialLineColors(nil,ExecutionPointLine, Special, FG, BG);
      SynEditCodeSpecialLineColors(nil,EnabledBreakLine, Special, FG, BG);
      SynEditCodeSpecialLineColors(nil,DisabledBreakLine, Special, FG, BG);
      SynEditCodeSpecialLineColors(nil,ErrorLineLine, Special, FG, BG);
      SynEditCode.InvalidateLine(InvalidBreakLine);
    }

    SynEditCode.Repaint;
  end;
end;

procedure TFrmMain.SetSynAttr(Element: TIDEHighlightElements; SynAttr: TSynHighlighterAttributes; ADelphiVersionData: TDelphiVersionData);
begin
  SynAttr.Background := GetDelphiVersionMappedColor(StringToColor(FCurrentTheme[Element].BackgroundColorNew), ADelphiVersionData);
  SynAttr.Foreground := GetDelphiVersionMappedColor(StringToColor(FCurrentTheme[Element].ForegroundColorNew), ADelphiVersionData);
  SynAttr.Style := [];
  if FCurrentTheme[Element].Bold then
    SynAttr.Style := SynAttr.Style + [fsBold];
  if FCurrentTheme[Element].Italic then
    SynAttr.Style := SynAttr.Style + [fsItalic];
  if FCurrentTheme[Element].Underline then
    SynAttr.Style := SynAttr.Style + [fsUnderline];
end;

procedure TFrmMain.SynEditCodeClick(Sender: TObject);
var
  ptCaret: TBufferCoord;
  Token: String;
  Attri: TSynHighlighterAttributes;
  i: integer;
  ElementStr: string;
  Done: boolean;

  function GetIndexHighlightElements(AElement: TIDEHighlightElements): integer;
  var
    j: integer;
  begin
    Result := -1;
    for j := 0 to CbElement.Items.Count - 1 do
      if TIDEHighlightElements(CbElement.Items.Objects[j]) = AElement then
      begin
        Result := j;
        Break;
      end;
  end;

  function SetCbElement(AElement: TIDEHighlightElements): boolean;
  var
    Index: integer;
  begin
    Result := False;
    index := GetIndexHighlightElements(AElement);
    if index >= 0 then
    begin
      CbElement.ItemIndex := index;
      CbElementChange(nil);
      Result := True;
    end;
  end;

begin
  SynEditCode.GetPositionOfMouse(ptCaret);
  Done := False;
  case ptCaret.Line of
    cInvalidBreakLine:
      Done := SetCbElement(TIDEHighlightElements.InvalidBreak);
    cExecutionPointLine:
      Done := SetCbElement(TIDEHighlightElements.ExecutionPoint);
    cEnabledBreakLine:
      Done := SetCbElement(TIDEHighlightElements.EnabledBreak);
    cDisabledBreakLine:
      Done := SetCbElement(TIDEHighlightElements.DisabledBreak);
    cErrorLineLine:
      Done := SetCbElement(TIDEHighlightElements.ErrorLine);
  end;

  if not Done then
    if SynEditCode.GetHighlighterAttriAtRowCol(ptCaret, Token, Attri) then
    begin
      for i := 0 to FMapHighlightElementsTSynAttr.Count - 1 do
        if TSynHighlighterAttributes(FMapHighlightElementsTSynAttr.Objects[i]).Name = Attri.Name then
        begin
          ElementStr := FMapHighlightElementsTSynAttr[i];
          if CbElement.Items.IndexOf(ElementStr) >= 0 then
          begin
            CbElement.ItemIndex := CbElement.Items.IndexOf(ElementStr);
            CbElementChange(nil);
          end;
          Break;
        end;
    end;
end;

procedure TFrmMain.SynEditCodeGutterClick(Sender: TObject; Button: TMouseButton; X, Y, Line: integer; Mark: TSynEditMark);
var
  Index: integer;
begin
  index := GetElementIndex(TIDEHighlightElements.LineNumber);
  if index >= 0 then
  begin
    CbElement.ItemIndex := index;
    CbElementChange(nil);
  end;
end;

procedure TFrmMain.SynEditCodeSpecialLineColors(Sender: TObject; Line: integer; var Special: boolean; var FG, BG: TColor);

  procedure SetColorSpecialLine(AElement: TIDEHighlightElements);
  begin
    FG := StringToColor(FCurrentTheme[AElement].ForegroundColorNew);
    BG := StringToColor(FCurrentTheme[AElement].BackgroundColorNew);
    Special := True;
  end;

begin
  if clThemes.ItemIndex <> -1 then
    case Line of
      cInvalidBreakLine:
        SetColorSpecialLine(InvalidBreak);
      cExecutionPointLine:
        SetColorSpecialLine(ExecutionPoint);
      cEnabledBreakLine:
        SetColorSpecialLine(EnabledBreak);
      cDisabledBreakLine:
        SetColorSpecialLine(DisabledBreak);
      cErrorLineLine:
        SetColorSpecialLine(ErrorLine);
    end;
end;

{ TCompPngImages }

destructor TCompPngImages.Destroy;
begin
  if Assigned(FNormal) then
    FNormal.Free;

  if Assigned(FBN) then
    FBN.Free;

  Inherited;
end;


{ TDITETheme }

constructor TDITETheme.Create(const AFileName: string);
begin
  inherited Create;
  FFileName := AFileName;
  FName := Copy(ExtractFileName(AFileName), 1, Pos('.theme', ExtractFileName(AFileName)) - 1);
  LoadThemeFromXMLFile(FTheme, AFileName, FAuthor, FModified);
end;

destructor TDITETheme.Destroy;
begin
  FreeAndNil(FBitmap);
  inherited;
end;

end.
