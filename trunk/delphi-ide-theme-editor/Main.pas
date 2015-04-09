//**************************************************************************************************
//
// Unit Main
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
// Portions created by Rodrigo Ruz V. are Copyright (C) 2011-2014 Rodrigo Ruz V.
// All Rights Reserved.
//
//**************************************************************************************************

unit Main;

interface
//T0DO

 //save theme as... done
 //delete theme   done
 //clone theme     done
 //hide line number ( delphi 7) - done
 //grow list themes - done
 //change xml format, add all versions internal support, theme GUID. theme version, theme name, author -> working  on it
 //notepad++ themes
 //exception handler extended - done
 //eclipse themes   done
 //hue/saturation - done
 //*****************************************************
 //Importer mapper XML . create xml with relations between delphi themes and external IDE themes
 //*****************************************************
 //download themes online
 //config file - done
 //translate
 //update online   (check new version) - done

 //import from http://studiostyl.es/schemes done
 //import from notepad++
 //http://www.eclipsecolorthemes.org/   done

 //http://www.colorotate.org/

 //import themes from IntelliJ IDE
 //   http://confluence.jetbrains.net/display/RUBYDEV/Third-party+add-ons
 //   http://devnet.jetbrains.net/docs/DOC-1154
 //   http://tedwise.com/2009/02/26/dark-pastels-theme-for-intellij-idea/
 //   http://yiwenandsoftware.wordpress.com/2008/05/15/textmate-golbalt-color-theme-for-intellij/
 //   http://blog.gokhanozcan.com/2008/10/06/intellij-idea-dark-color-scheme/
 //   http://stackoverflow.com/questions/4414593/where-can-i-download-intellij-idea-10-color-schemes

 //import themes from    NetBeans IDE
 //   http://www.niccolofavari.com/dark-low-contrast-color-scheme-for-netbeans-ide
 //   http://blog.mixu.net/2010/05/03/syntax-highlighting-color-schemes-for-netbeans/

 //import themes from Komodo
 //   http://www.kolormodo.com


uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, uColorPanel,
  Dialogs, ImgList, StdCtrls, ComCtrls, ExtCtrls, SynEditHighlighter,uSupportedIDEs,  uColorSelector,
  SynHighlighterPas, SynEdit, SynMemo, uDelphiVersions, uDelphiIDEHighlight, uLazarusVersions, Vcl.ActnPopup,  uAppMethodVersions,
  pngimage, uSettings, ExtDlgs, Menus, SynEditExport, SynExportHTML, Generics.Defaults, Generics.Collections, Vcl.ActnList,
  Vcl.PlatformDefaultStyleActnCtrls, System.Actions, Vcl.Styles.Fixes, Vcl.Styles.NC,
  Vcl.ActnMan;

{.$DEFINE ENABLE_THEME_EXPORT}

type
  TCompPngImages=class
  private
    FNormal: TPngImage;
    FBN: TPngImage;
  public
   property Normal : TPngImage read FNormal write FNormal;
   property BN     : TPngImage read FBN write FBN;
   destructor Destroy; override;
  end;


  TPopupMenu=class(Vcl.ActnPopup.TPopupActionBar);


  TFrmMain = class(TForm)
    ImageListDelphiVersion: TImageList;
    Label1:      TLabel;
    CbElement:   TComboBox;
    Label2:      TLabel;
    GroupBox1:   TGroupBox;
    CheckBold:   TCheckBox;
    CheckItalic: TCheckBox;
    CheckUnderline: TCheckBox;
    GroupBox2:   TGroupBox;
    CheckForeground: TCheckBox;
    CheckBackground: TCheckBox;
    CblForeground: TColorBox;
    CblBackground: TColorBox;
    SynPasSyn1:  TSynPasSyn;
    Label5:      TLabel;
    CbIDEFonts:  TComboBox;
    EditFontSize: TEdit;
    UpDownFontSize: TUpDown;
    BtnApply:    TButton;
    Label6:      TLabel;
    EditThemeName: TEdit;
    BtnSave:     TButton;
    BtnApplyFont: TButton;
    Label7:      TLabel;
    LvThemes:    TListView;
    OpenDialogImport: TOpenDialog;
    Label8:      TLabel;
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
    ImageListThemes: TImageList;
    PopupMenu1: TPopupMenu;
    ActionList1: TActionList;
    ActionApplyTheme: TAction;
    ActionCloneTheme: TAction;
    ActionDeleteTheme: TAction;
    ActionSaveChanges: TAction;
    ActionSaveAs: TAction;
    PopupActionBar1: TPopupActionBar;
    Button1: TButton;
    PanelColors: TPanel;
    RadioButtonFore: TRadioButton;
    RadioButtonBack: TRadioButton;
    ImageList2: TImageList;
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
    procedure FormCreate(Sender: TObject);
    procedure LvIDEVersionsChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure CbElementChange(Sender: TObject);
    procedure CbIDEFontsChange(Sender: TObject);
    procedure CblForegroundChange(Sender: TObject);
    procedure BtnApplyFontClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LvThemesChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure SynEditCodeClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ImageConfClick(Sender: TObject);
    procedure ImageHueClick(Sender: TObject);
    procedure SynEditCodeSpecialLineColors(Sender: TObject; Line: integer;
      var Special: boolean; var FG, BG: TColor);
    procedure SynEditCodeGutterClick(Sender: TObject; Button: TMouseButton;
      X, Y, Line: integer; Mark: TSynEditMark);
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
    procedure Button1Click(Sender: TObject);
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
  private
    FChanging     : boolean;
    FThemeChangued: boolean;
    FSettings:      TSettings;
    FCurrentTheme:  TIDETheme;
    FMapHighlightElementsTSynAttr: TStrings;
    IDEsList:TList<TDelphiVersionData>;
    FIDEData: TDelphiVersionData;
    FrmColorPanel      : TColorPanel;
    NCControls: TNCControls;

    ActionImages : TObjectDictionary<string, TCompPngImages>;
    procedure LoadActionImages;
    procedure LoadThemes;
    procedure LoadFixedWidthFonts;
    procedure LoadValuesElements;
    procedure RefreshPasSynEdit;
    procedure SetSynAttr(Element: TIDEHighlightElements;
      SynAttr: TSynHighlighterAttributes;DelphiVersion : TDelphiVersions);
    procedure CreateThemeFile;
    procedure ApplyCurentTheme;
    function GetThemeIndex(const AThemeName: string): integer;
    function GetElementIndex(Element: TIDEHighlightElements): integer;
    procedure OnSelForegroundColorChange(Sender: TObject);
    procedure OnSelBackGroundColorChange(Sender: TObject);
    procedure CMStyleChanged(var Message: TMessage); message CM_STYLECHANGED;
    procedure GenerateThumbnail;
    procedure ImportTheme(ImportType: TIDEImportThemes);
    procedure ExportToLazarusTheme();
    procedure ImportDelphiThemeRegistry();
    procedure SetDefaultDelphiTheme();
    {$IFDEF ENABLE_THEME_EXPORT}
    procedure ExportThemeHtml;
    {$ENDIF}
    function GetDelphiVersionData(Index:Integer) : TDelphiVersionData;
    function GetIDEData: TDelphiVersionData;
    property IDEData   : TDelphiVersionData read GetIDEData write FIDEData;
  public
    property Settings : TSettings read FSettings;
  end;


var
  FrmMain : TFrmMain;

implementation

uses
  {$WARN UNIT_PLATFORM OFF}
  Vcl.FileCtrl,
  {$WARN UNIT_PLATFORM ON}
  Diagnostics,
  System.UITypes,
  System.Types,
  ShellApi,
  IOUtils,
  StrUtils,
  GraphUtil,
  CommCtrl,
  Vcl.Styles,
  Vcl.Themes,
  Vcl.Styles.OwnerDrawFix,
  uVclStylesFix,
  uHueSat,
  EclipseThemes,
  VSThemes,
  uMisc,
  uLazarusIDEHighlight,
  uSMSIDEHighlight,
  uStackTrace,
  uSMSVersions,
  uLoadThemesImages,
  uHelpInsight,
  uStdActionsPopMenu, uAdditionalSettings;

const
  InvalidBreakLine   = 9;
  ExecutionPointLine = 10;
  EnabledBreakLine   = 12;
  DisabledBreakLine  = 13;
  ErrorLineLine      = 14;

{$R *.dfm}
{.$R Manbdmin.RES}

procedure TFrmMain.ActionApplyThemeExecute(Sender: TObject);
begin
  try
    if (ComboBoxExIDEs.ItemIndex>=0) and (LvThemes.Selected <> nil) then
      if IsAppRunning(IDEData.Path) then
        MsgBox(Format('Before to continue you must close all running instances of the %s IDE',
          [IDEData.Name]))
      else
      if MessageDlg(
        Format('Do you want apply the theme "%s" to the %s IDE?',
        [LvThemes.Selected.Caption, IDEData.Name]), mtConfirmation, [mbYes, mbNo], 0) = mrYes then
        ApplyCurentTheme;
  except
    on E: Exception do
      MsgBox(Format('Error setting theme - Message : %s : Trace %s',
        [E.Message, E.StackTrace]));
  end;
end;


procedure TFrmMain.ActionCloneThemeExecute(Sender: TObject);
var
  index    : integer;
  FileName : string;
  NFileName: string;
begin
  try
    if LvThemes.Selected <> nil then
    begin
      Index    :=LvThemes.Selected.Index;
      FileName :=LvThemes.Selected.SubItems[0];
      NFileName:=ChangeFileExt(ExtractFileName(FileName),'');//remove .xml
      NFileName:=ChangeFileExt(ExtractFileName(NFileName),'');//remove .theme
      NFileName:=ExtractFilePath(FileName)+NFileName+'-Clone.theme.xml';
      DeleteFile(NFileName);
      TFile.Copy(FileName,NFileName);
      LoadThemes;
      if index >= 0 then
      begin
        LvThemes.Selected := LvThemes.Items.Item[index];
        LvThemes.Selected.MakeVisible(True);
      end;
    end;
  except
    on E: Exception do
      MsgBox(Format('Error deleting theme message : %s : trace %s',[E.Message, E.StackTrace]));
  end;
end;

procedure TFrmMain.ActionDeleteThemeExecute(Sender: TObject);
var
  index: integer;
begin
  try
    if LvThemes.Selected <> nil then
      if MessageDlg(
        Format('Do you want delete the theme "%s"?',[LvThemes.Selected.Caption]), mtConfirmation, [mbYes, mbNo], 0) = mrYes  then
    begin
      index := LvThemes.Selected.Index;
      DeleteFile(LvThemes.Selected.SubItems[0]);
      LoadThemes;
      if index >= 0 then
      begin
        LvThemes.Selected := LvThemes.Items.Item[index];
        LvThemes.Selected.MakeVisible(True);
      end;
    end;
  except
    on E: Exception do
      MsgBox(Format('Error deleting theme message : %s : trace %s',[E.Message, E.StackTrace]));
  end;
end;

procedure TFrmMain.ActionExoLazarusClrSchExecute(Sender: TObject);
begin
 ExportToLazarusTheme();
end;

procedure TFrmMain.ActionExoLazarusClrSchUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled:=(IDEData.IDEType=TSupportedIDEs.LazarusIDE);
end;

procedure TFrmMain.ActionImportThemeRegExecute(Sender: TObject);
begin
  ImportDelphiThemeRegistry();
end;

procedure TFrmMain.ActionImportThemeRegUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled:=(IDEData.IDEType=TSupportedIDEs.DelphiIDE) and not DelphiIsOldVersion(IDEData.Version);
end;

procedure TFrmMain.ActionSaveAsExecute(Sender: TObject);
var
  index: integer;
  Value: string;
begin
  //detect name in list show msg overwrite
  Value:=EditThemeName.Text;
  try

   if InputQuery('Save As..','Enter the new name of the theme',Value) then
   begin
    EditThemeName.Text:=Value;
    CreateThemeFile;
    LoadThemes;
    index := GetThemeIndex(EditThemeName.Text);
    if index >= 0 then
    begin
      LvThemes.Selected := LvThemes.Items.Item[index];
      LvThemes.Selected.MakeVisible(True);
    end;
   end;
  except
    on E: Exception do
      MsgBox(Format('Error Saving theme  Message : %s : Trace %s',
        [E.Message, E.StackTrace]));
  end;
end;


procedure TFrmMain.ActionSaveChangesExecute(Sender: TObject);
var
  index: integer;
begin
  //detect name in list show msg overwrite
  try
    CreateThemeFile;
    LoadThemes;
    index := GetThemeIndex(EditThemeName.Text);
    if index >= 0 then
    begin
      LvThemes.Selected := LvThemes.Items.Item[index];
      LvThemes.Selected.MakeVisible(True);
    end;
  except
    on E: Exception do
      MsgBox(Format('Error Saving theme  Message : %s : Trace %s',
        [E.Message, E.StackTrace]));
  end;
end;

procedure TFrmMain.ActionSetDefaultThemeExecute(Sender: TObject);
begin
  SetDefaultDelphiTheme();
end;

procedure TFrmMain.ActionSetDefaultThemeUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled:=(IDEData.IDEType=TSupportedIDEs.DelphiIDE);
end;

procedure TFrmMain.ApplyCurentTheme;
var
  DelphiVersion: TDelphiVersions;
begin
  if ComboBoxExIDEs.ItemIndex >=0  then
  begin

    if  IDEData.IDEType=TSupportedIDEs.DelphiIDE then
     DelphiVersion := IDEData.Version
    else
    //if IDEType=TSupportedIDEs.LazarusIDE then
     DelphiVersion := TDelphiVersions.DelphiXE; //if is lazarus or SMS use the Delphi XE elements


    if IDEData.IDEType=TSupportedIDEs.DelphiIDE then
    begin
      if ApplyDelphiIDETheme(DelphiVersion, FCurrentTheme, LvThemes.Selected.Caption) then
        MsgBox('The theme was successfully applied')
      else
        MsgBox('Error setting theme');

      if Settings.ApplyThemeHelpInsight then
         ApplyThemeHelpInsight(FCurrentTheme, IDEData);
    end
    else
    if IDEData.IDEType=TSupportedIDEs.LazarusIDE then
      if ApplyLazarusIDETheme(FCurrentTheme, EditThemeName.Text) then
        MsgBox('The theme was successfully applied')
      else
        MsgBox('Error setting theme')
    else
    if IDEData.IDEType=TSupportedIDEs.SMSIDE then
      if ApplySMSIDETheme(FCurrentTheme, EditThemeName.Text) then
        MsgBox('The theme was successfully applied')
      else
        MsgBox('Error setting theme');
   end;
end;


procedure TFrmMain.BtnAdditionalSettingsClick(Sender: TObject);
var
  LFrm : TFrmAdditionalSettings;
begin
  LFrm := TFrmAdditionalSettings.Create(Self);
  try
    LFrm.IDEData:=IDEData;
    LFrm.ShowModal;
  finally
    LFrm.Free;
  end;
end;

procedure TFrmMain.BtnApplyFontClick(Sender: TObject);
var
  DelphiVersion : TDelphiVersions;
begin
  try
    if (ComboBoxExIDEs.ItemIndex>=0) then
      if IsAppRunning(IDEData.Path) then
        MsgBox(Format('Before to continue you must close all running instances of the %s IDE', [IDEData.Name]))
      else
      if MessageDlg(
        Format('Do you want apply the "%s" font to the %s IDE?',
        [CbIDEFonts.Text, IDEData.Name]), mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      begin

        if IDEData.IDEType=TSupportedIDEs.DelphiIDE then
         DelphiVersion := IDEData.Version
        else
        //if IDEType=TSupportedIDEs.LazarusIDE then
         DelphiVersion := TDelphiVersions.DelphiXE; //if is lazarus or SMS use the Delphi XE elements


        if IDEData.IDEType=TSupportedIDEs.DelphiIDE then
        begin
          if SetDelphiIDEFont(DelphiVersion, CbIDEFonts.Text, UpDownFontSize.Position) then
            MsgBox('The font was successfully applied')
          else
            MsgBox('Error setting font');
        end
        else
        if IDEData.IDEType=TSupportedIDEs.LazarusIDE then
        begin
          if SetLazarusIDEFont(CbIDEFonts.Text, UpDownFontSize.Position) then
            MsgBox('The font was successfully applied')
          else
            MsgBox('Error setting font');
        end
        else
        if IDEData.IDEType=TSupportedIDEs.SMSIDE then
        begin
          if SetSMSIDEFont(CbIDEFonts.Text, UpDownFontSize.Position) then
            MsgBox('The font was successfully applied')
          else
            MsgBox('Error setting font');
        end;


      end;
  except
    on E: Exception do
      MsgBox(Format('Error setting font - Message : %s : Trace %s',
        [E.Message, E.StackTrace]));
  end;
end;

procedure TFrmMain.ExportToLazarusTheme();
var
  i: integer;
  OutPutFolder : String;
  GoNext : Boolean;
  s  : TStopwatch;
  Directory : String;
begin
  try
    Directory:='';
    OutPutFolder:=GetSettingsFolder+'Themes Lazarus';
    if SysUtils.DirectoryExists(OutPutFolder) then
     Directory := OutPutFolder;

    if SelectDirectory('Select directory',Directory,Directory,[sdNewFolder, sdNewUI, sdShowEdit, sdValidateDir, sdShowShares], nil) then
      OutPutFolder := Directory
    else
      Exit;

    OpenDialogExport.InitialDir := GetSettingsFolder;
    if OpenDialogExport.Execute(Handle) then
    begin

      if OpenDialogExport.Files.Count = 1 then
        GoNext := MessageDlg(
          Format('Do you want import the "%s" file?', [ExtractFileName(
          OpenDialogExport.FileName)]), mtConfirmation, [mbYes, mbNo], 0) = mrYes
      else
        GoNext := MessageDlg(
          Format('Do you want import the %d files selected?',
          [OpenDialogExport.Files.Count]), mtConfirmation, [mbYes, mbNo], 0) = mrYes;

      if GoNext and (OpenDialogExport.Files.Count > 1) then
      begin

        s:=TStopwatch.Create;
        s.Start;

        ProgressBar1.Visible := True;
        try
          ProgressBar1.Position := 0;
          ProgressBar1.Max      := OpenDialogExport.Files.Count;
          LabelMsg.Visible:=True;
          for i := 0 to OpenDialogExport.Files.Count - 1 do
          begin
            LabelMsg.Caption:=Format('Exporting %s theme',[ExtractFileName(OpenDialogExport.Files[i])]);
            DelphiIDEThemeToLazarusTheme(OpenDialogExport.Files[i],OutPutFolder);
            ProgressBar1.Position := i;
          end;
        finally
          ProgressBar1.Visible := False;
          LabelMsg.Visible:=False;
        end;

        s.Stop;
        MsgBox(Format('%d Themes exported in %n seconds', [OpenDialogExport.Files.Count,s.Elapsed.TotalSeconds]));
      end
      else
      if GoNext and (OpenDialogExport.Files.Count = 1) then
      begin
        if  DelphiIDEThemeToLazarusTheme(OpenDialogExport.FileName,OutPutFolder) then
         MsgBox(Format('"%s" Theme exported', [ExtractFileName(OpenDialogExport.FileName)]));
      end;
    end;

  except
    on E: Exception do
      MsgBox(Format('Error exporting themes - Message : %s : Trace %s',
        [E.Message, E.StackTrace]));
  end;
end;


procedure TFrmMain.SetDefaultDelphiTheme;
begin
  try
    if ComboBoxExIDEs.ItemIndex>=0 then
      if not IsAppRunning(IDEData.Path) then
        if MessageDlg(
          Format('Do you want apply the default theme to the "%s" IDE?',
          [IDEData.Name]), mtConfirmation, [mbYes, mbNo], 0) = mrYes then
        begin
          if SetDelphiIDEDefaultTheme(IDEData.Version) then
            MsgBox('Default theme was applied')
          else
            MsgBox('Error setting theme');

          if IDEData.IDEType=TSupportedIDEs.DelphiIDE then
             SetHelpInsightDefault(IDEData);

          ComboBoxExIDEsChange(nil);
        end;
  except
    on E: Exception do
      MsgBox(Format('Error setting default theme - Message : %s : Trace %s',
        [E.Message, E.StackTrace]));
  end;
end;

procedure TFrmMain.Button1Click(Sender: TObject);
var
  i : Integer;
begin
  ProgressBar1.Visible := True;
  try
    ProgressBar1.Position := 0;
    ProgressBar1.Max      := LvThemes.Items.Count;
    LabelMsg.Visible:=True;
    for i := 0 to LvThemes.Items.Count - 1 do
    begin
      LvThemes.Selected:=LvThemes.Items[i];
      LabelMsg.Caption:=Format('processing %s theme',[LvThemes.Items[i].Caption]);
      GenerateThumbnail;
      ProgressBar1.Position := i;
    end;
  finally
    ProgressBar1.Visible := False;
    LabelMsg.Visible:=False;
  end;
end;

procedure PrintControl(AControl :TWinControl;ARect:TRect;AOut:TBitmap);
var
  DC: HDC;
begin
  DC :=GetWindowDC(AControl.Handle);
  AOut.Width  :=ARect.Width;
  AOut.Height :=ARect.Height;
  with AOut do
  BitBlt(
    Canvas.Handle, 0, 0, Width, Height, DC, ARect.Left, ARect.Top, SrcCopy
  );
  ReleaseDC(AControl.Handle, DC);
end;

procedure TFrmMain.GenerateThumbnail;
var
  LBitmap : TBitmap;
  LPNG    : TPngImage;
begin
  LBitmap:=TBitmap.Create;
  try
    PrintControl(SynEditCode, Rect(0,0,450,150), LBitmap);
    LPNG:=TPngImage.Create;
    try
      //ResizeBitmap(LBitmap, 225,75, clBlack);
      //SmoothResize(LBitmap, 225,75);
      LPNG.Assign(LBitmap);
      LPNG.SaveToFile('C:\Users\Dexter\Desktop\RAD Studio Projects\XE2\delphi-ide-theme-editor\Thumbnails\'+EditThemeName.Text+'.png');
    finally
      LPNG.Free;
    end;
  finally
    LBitmap.Free;
  end;
end;


procedure TFrmMain.BtnSelForColorClick(Sender: TObject);
var
   Frm      : TDialogColorSelector;
   OldColor : TColor;
begin
   Frm := TDialogColorSelector.Create(Self);
   try
     OldColor:=CblForeground.Selected;
     Frm.OnChange:=OnSelForegroundColorChange;
     Frm.SelectedColor:=CblForeground.Selected;
     if Frm.Execute then
     begin
      CblForeground.Selected:=Frm.SelectedColor;
      CblForegroundChange(CblForeground);
     end
     else
     if CblForeground.Selected<>OldColor then
     begin
      CblForeground.Selected:=OldColor;
      CblForegroundChange(CblForeground);
     end;
   finally
     Frm.Free;
   end;
end;


procedure TFrmMain.BtnSelBackColorClick(Sender: TObject);
var
   Frm      : TDialogColorSelector;
   OldColor : TColor;
begin
   Frm := TDialogColorSelector.Create(Self);
   try
     OldColor:=CblBackground.Selected;
     Frm.SelectedColor:=CblBackground.Selected;
     Frm.OnChange:=OnSelBackGroundColorChange;
     if Frm.Execute then
     begin
      CblBackground.Selected:=Frm.SelectedColor;
      CblForegroundChange(CblBackground);
     end
     else
     if CblBackground.Selected<>OldColor then
     begin
      CblBackground.Selected:=OldColor;
      CblForegroundChange(CblBackground);
     end;
   finally
     Frm.Free;
   end;
end;

procedure TFrmMain.OnSelForegroundColorChange(Sender: TObject);
begin
  if Sender is TDialogColorSelector then
    CblForeground.Selected:=TDialogColorSelector(Sender).SelectedColor
  else
  if Sender is TColorPanel then
    CblForeground.Selected:=TColorPanel(Sender).SelectedColor;

  CblForegroundChange(nil);
end;

procedure TFrmMain.OnSelBackGroundColorChange(Sender: TObject);
begin
  if Sender is TDialogColorSelector then
    CblBackground.Selected:=TDialogColorSelector(Sender).SelectedColor
  else
  if Sender is TColorPanel then
    CblBackground.Selected:=TColorPanel(Sender).SelectedColor;

  CblForegroundChange(nil);
end;

procedure TFrmMain.ImportDelphiThemeRegistry();
var
  ThemeName: string;
  DelphiVersion: TDelphiVersions;
  ImpTheme     : TIDETheme;
  i: integer;
begin
  try
    if ComboBoxExIDEs.ItemIndex >= 0 then
      if MessageDlg(
        Format('Do you want import the current theme from  the "%s" IDE?',
        [IDEData.Name]), mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      begin
        DelphiVersion := IDEData.Version;
        if not ExistDelphiIDEThemeToImport(DelphiVersion) then
        begin
         MsgBox(
            Format('The "%s" IDE has not themes stored in the windows registry?',
            [IDEData.Name]));
          exit;
        end;

        ThemeName := InputBox('Import Delphi IDE Theme',
          'Enter the name for the theme to import', '');
        if ThemeName <> '' then
        begin
          ImportDelphiIDEThemeFromReg(ImpTheme, DelphiVersion);
          if IsValidDelphiIDETheme(ImpTheme) then
          begin
            FCurrentTheme:=ImpTheme;
            //SaveDelphiIDEThemeToXmlFile(DelphiVersion, FCurrentTheme, FSettings.ThemePath, ThemeName);
            SaveDelphiIDEThemeToXmlFile(FCurrentTheme, FSettings.ThemePath, ThemeName);
            EditThemeName.Text := ThemeName;
            MsgBox('Theme imported');
            LoadThemes;
            for i := 0 to LvThemes.Items.Count - 1 do
              if CompareText(LvThemes.Items.Item[i].Caption, EditThemeName.Text) = 0 then
              begin
                LvThemes.Selected := LvThemes.Items.Item[i];
                Break;
              end;
          end
          else
          MsgBox('The imported theme has invalid values, the theme will be discarded');

        end;
      end;
  except
    on E: Exception do
      MsgBox(Format('Error importing theme from registry - Message : %s : Trace %s',
        [E.Message, E.StackTrace]));
  end;

end;

procedure TFrmMain.ImportEclipseTheme1Click(Sender: TObject);
begin
 ImportTheme(TIDEImportThemes.EclipseTheme);
end;

procedure TFrmMain.ImportTheme(ImportType: TIDEImportThemes);
var
  ThemeName: string;
  i:      integer;
  DelphiVersion: TDelphiVersions;
  GoNext: boolean;
  s : TStopwatch;
begin
//  ImportType := TIDEImportThemes(
//    CbIDEThemeImport.Items.Objects[CbIDEThemeImport.ItemIndex]);
  OpenDialogImport.Filter := IDEImportThemesDialogFilter[ImportType];
  OpenDialogImport.InitialDir := ExtractFilePath(ParamStr(0));
  try
    if (ComboBoxExIDEs.ItemIndex >= 0) and OpenDialogImport.Execute(Handle) then
    begin
      DelphiVersion := DelphiXE;//TDelphiVersions(integer(LvDelphiVersions.Selected.Data));

      if OpenDialogImport.Files.Count = 1 then
        GoNext := MessageDlg(
          Format('Do you want import the "%s" file?', [ExtractFileName(
          OpenDialogImport.FileName)]), mtConfirmation, [mbYes, mbNo], 0) = mrYes
      else
        GoNext := MessageDlg(
          Format('Do you want import the %d files selected?',
          [OpenDialogImport.Files.Count]), mtConfirmation, [mbYes, mbNo], 0) = mrYes;

      if GoNext and (OpenDialogImport.Files.Count > 1) then
      begin

        s:=TStopwatch.Create;
        s.Start;

        ProgressBar1.Visible := True;
        try
          ProgressBar1.Position := 0;
          ProgressBar1.Max      := OpenDialogImport.Files.Count;
          LabelMsg.Visible:=True;
          for i := 0 to OpenDialogImport.Files.Count - 1 do
          begin
            LabelMsg.Caption:=Format('Importing %s theme',[ExtractFileName(OpenDialogImport.Files[i])]);
            case ImportType of
              VisualStudioThemes: ImportVisualStudioTheme(
                  DelphiVersion, OpenDialogImport.Files[i], FSettings.ThemePath, ThemeName);
              EclipseTheme: ImportEclipseTheme(
                  DelphiVersion, OpenDialogImport.Files[i], FSettings.ThemePath, ThemeName);
            end;

            ProgressBar1.Position := i;
          end;
        finally
          ProgressBar1.Visible := False;
          LabelMsg.Visible:=False;
        end;

        s.Stop;
        MsgBox(Format('%d Themes imported in %n seconds', [OpenDialogImport.Files.Count,s.Elapsed.TotalSeconds]));
        LoadThemes;
        LvThemes.Selected := LvThemes.Items.Item[0];
      end
      else
      if GoNext and (OpenDialogImport.Files.Count = 1) then
      begin

        case ImportType of
          VisualStudioThemes: GoNext :=
              ImportVisualStudioTheme(DelphiVersion, OpenDialogImport.FileName,
              FSettings.ThemePath, ThemeName);
          EclipseTheme: GoNext :=
              ImportEclipseTheme(DelphiVersion, OpenDialogImport.FileName,
              FSettings.ThemePath, ThemeName);
        end;

        if GoNext then
        begin
          EditThemeName.Text := ThemeName;
          MsgBox(Format('"%s" Theme imported', [ThemeName]));
          LoadThemes;
          i := GetThemeIndex(EditThemeName.Text);
          if i >= 0 then
            LvThemes.Selected := LvThemes.Items.Item[i];
        end;
      end;
    end;
  except
    on E: Exception do
      MsgBox(Format('Error to import theme(s) - Message : %s : Trace %s',
        [E.Message, E.StackTrace]));
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
  SynEditCode.Font.Name := CbIDEFonts.Text;
  SynEditCode.Font.Size := StrToInt(EditFontSize.Text);
      {
  SynEditCode.Gutter.Font.Name := CbIDEFonts.Text;
  SynEditCode.Gutter.Font.Size := StrToInt(EditFontSize.Text);
      }
  BtnApplyFont.Enabled  := True;
  RefreshPasSynEdit;
end;


procedure TFrmMain.FormCreate(Sender: TObject);
Var
  IDEData  : TDelphiVersionData;
  Index    : Integer;
begin
 NCControls:=TNCControls.Create(Self);
 NCControls.ShowSystemMenu:=False;
 NCControls.List.Add(TNCButton.Create(NCControls));
 NCControls.List[0].Style := nsSplitButton;
 NCControls.List[0].ImageStyle := isGrayHot;
 NCControls.List[0].Images := ImageList2;
 NCControls.List[0].ImageIndex := 0;
 NCControls.List[0].BoundsRect := Rect(5,5,75,25);
 NCControls.List[0].Caption := 'Menu';
 NCControls.List[0].DropDownMenu:= PopupMenuThemes;
 //NCControls.List[0].OnClick := ButtonNCClick;

 NCControls.List.Add(TNCButton.Create(NCControls));
 NCControls.List[1].Style := nsTranparent;
 NCControls.List[1].ImageStyle := isGrayHot;
 NCControls.List[1].Images := ImageList2;
 NCControls.List[1].ImageIndex := 3;
 NCControls.List[1].BoundsRect := Rect(78,5,98,25);
 NCControls.List[1].Name       := 'NCHue';
 NCControls.List[1].ShowHint   := True;
 NCControls.List[1].Hint       := 'Change Hue/Saturation';
 NCControls.List[1].Caption := '';
 NCControls.List[1].OnClick := ImageHueClick;

 NCControls.List.Add(TNCButton.Create(NCControls));
 NCControls.List[2].Style := nsTranparent;
 NCControls.List[2].ImageStyle := isGrayHot;
 NCControls.List[2].Images := ImageList2;
 NCControls.List[2].ImageIndex := 2;
 NCControls.List[2].BoundsRect := Rect(101,5,121,25);
 NCControls.List[2].Name       := 'NCConf';
 NCControls.List[2].ShowHint   := True;
 NCControls.List[2].Hint       := 'Settings';
 NCControls.List[2].Caption := '';
 NCControls.List[2].OnClick := ImageConfClick;

 NCControls.List.Add(TNCButton.Create(NCControls));
 NCControls.List[3].Style := nsTranparent;
 NCControls.List[3].ImageStyle := isGrayHot;
 NCControls.List[3].Images := ImageList2;
 NCControls.List[3].ImageIndex := 1;
 NCControls.List[3].BoundsRect := Rect(124,5,144,25);
 NCControls.List[3].Name       := 'NCBug';
 NCControls.List[3].ShowHint   := True;
 NCControls.List[3].Hint       := 'Report Bugs';
 NCControls.List[3].Caption := '';
 NCControls.List[3].OnClick := ImageBugClick;

 NCControls.List.Add(TNCButton.Create(NCControls));
 NCControls.List[4].Style := nsTranparent;
 NCControls.List[4].ImageStyle := isGrayHot;
 NCControls.List[4].Images := ImageList2;
 NCControls.List[4].ImageIndex := 4;
 NCControls.List[4].BoundsRect := Rect(147,5,167,25);
 NCControls.List[4].Name       := 'NCUpdate';
 NCControls.List[4].ShowHint   := True;
 NCControls.List[4].Hint       := 'Check for updates';
 NCControls.List[4].Caption    := '';
 NCControls.List[4].OnClick := ImageUpdateClick;


  IDEsList:=TList<TDelphiVersionData>.Create;
  FillListDelphiVersions(IDEsList);

  if IsLazarusInstalled then
   FillListLazarusVersions(IDEsList);

  if IsSMSInstalled then
   FillListSMSVersions(IDEsList);

  //FillListAppMethodVersions(IDEsList);

  for Index:=0 to IDEsList.Count-1 do
  begin
    IDEData:=IDEsList[Index];
    ImageList_AddIcon(ImageListDelphiVersion.Handle, IDEData.Icon.Handle);
    ComboBoxExIDEs.ItemsEx.AddItem(IDEData.Name,ImageListDelphiVersion.Count-1,ImageListDelphiVersion.Count-1,ImageListDelphiVersion.Count-1,0, IDEsList[Index]);
  end;


  ActionImages:=TObjectDictionary<string,TCompPngImages>.Create([doOwnsValues]);
  LoadActionImages;

  FillPopupActionBar(PopupActionBar1);
  AssignStdActionsPopUpMenu(Self, PopupActionBar1);

  FChanging := False;
  FSettings := TSettings.Create;
  ReadSettings(FSettings);
  LoadVCLStyle(FSettings.VCLStyle);
  {.$WARN SYMBOL_PLATFORM OFF}
  //BtnIDEColorizer.Visible:=DebugHook<>0;
  {.$WARN SYMBOL_PLATFORM ON}
  //BtnIDEColorizer.Visible:=FSettings.ActivateColorizer;

  FMapHighlightElementsTSynAttr := TStringList.Create;
  with SynPasSyn1 do
  begin
    FMapHighlightElementsTSynAttr.AddObject(
      IDEHighlightElementsNames[TIDEHighlightElements.Assembler], AsmAttri);
    FMapHighlightElementsTSynAttr.AddObject(
      IDEHighlightElementsNames[TIDEHighlightElements.Character], CharAttri);
    FMapHighlightElementsTSynAttr.AddObject(
      IDEHighlightElementsNames[TIDEHighlightElements.Comment], CommentAttri);
    FMapHighlightElementsTSynAttr.AddObject(
      IDEHighlightElementsNames[TIDEHighlightElements.Preprocessor], DirectiveAttri);
    FMapHighlightElementsTSynAttr.AddObject(
      IDEHighlightElementsNames[TIDEHighlightElements.Float], FloatAttri);
    FMapHighlightElementsTSynAttr.AddObject(
      IDEHighlightElementsNames[TIDEHighlightElements.Hex], HexAttri);
    FMapHighlightElementsTSynAttr.AddObject(
      IDEHighlightElementsNames[TIDEHighlightElements.Identifier], IdentifierAttri);
    FMapHighlightElementsTSynAttr.AddObject(
      IDEHighlightElementsNames[TIDEHighlightElements.ReservedWord], KeyAttri);
    FMapHighlightElementsTSynAttr.AddObject(
      IDEHighlightElementsNames[TIDEHighlightElements.Number], NumberAttri);
    FMapHighlightElementsTSynAttr.AddObject(
      IDEHighlightElementsNames[TIDEHighlightElements.Whitespace], SpaceAttri);
    FMapHighlightElementsTSynAttr.AddObject(
      IDEHighlightElementsNames[TIDEHighlightElements.string], StringAttri);
    FMapHighlightElementsTSynAttr.AddObject(
      IDEHighlightElementsNames[TIDEHighlightElements.Symbol], SymbolAttri);
    FMapHighlightElementsTSynAttr.AddObject(
      IDEHighlightElementsNames[TIDEHighlightElements.LineNumber], NumberAttri);
  end;
  FThemeChangued := False;

//  FillListIDEThemesImport(CbIDEThemeImport.Items);
//  CbIDEThemeImport.ItemIndex := 0;

  Self.Caption := Caption + Format(' %s', [uMisc.GetFileVersion(ParamStr(0))]);

  LoadFixedWidthFonts;
  LoadThemes;

  if ComboBoxExIDEs.Items.Count > 0 then
  begin
    ComboBoxExIDEs.ItemIndex := 0;
    ComboBoxExIDEsChange(ComboBoxExIDEs);
  end
  else
  begin
    MsgBox('You don''t have a Object Pascal IDE installed');
    Halt(0);
  end;

  if LvThemes.Items.Count > 0 then
    LvThemes.Selected := LvThemes.Items.Item[0];

  FrmColorPanel:=TColorPanel.Create(PanelColors);
  FrmColorPanel.Parent:=PanelColors;
  FrmColorPanel.BorderStyle := bsNone;
  FrmColorPanel.Align := alClient;
  FrmColorPanel.OnChange:=OnSelForegroundColorChange;
  FrmColorPanel.Show;
end;


procedure TFrmMain.FormDestroy(Sender: TObject);
var
 Index :  Integer;
begin
  FSettings.Free;
  FMapHighlightElementsTSynAttr.Free;

  for Index := 0 to IDEsList.Count-1 do
  begin
    //IDEsList[Index].Icon.Free;
    IDEsList[Index].Free;
  end;

  IDEsList.Free;

  ActionImages.Free;
end;

procedure TFrmMain.FormShow(Sender: TObject);
begin
  BtnApplyFont.Enabled := False;
  ComboBoxExIDEsChange(ComboBoxExIDEs);
end;


function TFrmMain.GetDelphiVersionData(Index: Integer): TDelphiVersionData;
begin
   Result:=TDelphiVersionData(ComboBoxExIDEs.ItemsEx[Index].Data);
end;

function TFrmMain.GetElementIndex(Element: TIDEHighlightElements): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to CbElement.Items.Count - 1 do
    if TIDEHighlightElements(CbElement.Items.Objects[i]) = Element then
    begin
      Result := i;
      Break;
    end;
end;

function TFrmMain.GetIDEData: TDelphiVersionData;
begin
  FIDEData:=GetDelphiVersionData(ComboBoxExIDEs.ItemIndex);
  Result  := FIDEData;
end;

function TFrmMain.GetThemeIndex(const AThemeName: string): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to LvThemes.Items.Count - 1 do
    if CompareText(LvThemes.Items.Item[i].Caption, AThemeName) = 0 then
    begin
      Result := i;
      Break;
    end;

end;

procedure TFrmMain.ImageBugClick(Sender: TObject);
begin
  ShellExecute(Handle, 'open', 'http://code.google.com/p/delphi-ide-theme-editor/issues/list',nil,nil, SW_SHOWNORMAL) ;
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
  Frm: TFrmSettings;
begin
  Frm := TFrmSettings.Create(nil);
  try
    Frm.Settings := FSettings;
    Frm.LoadSettings;
    Frm.ShowModal();
  finally
    Frm.Free;
  end;
end;


procedure TFrmMain.ImageHueClick(Sender: TObject);
var
  Frm:   TFrmHueSat;
  DelphiVersion: TDelphiVersions;
  index: integer;
  FBackUpTheme: TIDETheme;
begin
  if (ComboBoxExIDEs.ItemIndex >=0) and (LvThemes.Selected <> nil) then
  begin

    Frm := TFrmHueSat.Create(nil);
    try
      FBackUpTheme  := FCurrentTheme;
      DelphiVersion := IDEData.Version;
      Frm.SynEditor := SynEditCode;
      Frm.Theme     := FCurrentTheme;
      Frm.DelphiVersion := DelphiVersion;
      Frm.ThemeName := LvThemes.Selected.Caption;
      Frm.Settings  := FSettings;
      Frm.init;
      Frm.ShowModal();

      if Frm.Reloadthemes then
      begin
        LoadThemes;
        index := GetThemeIndex(EditThemeName.Text);
        if index >= 0 then
          LvThemes.Selected := LvThemes.Items.Item[index];
      end
      else
      begin
        FCurrentTheme := FBackUpTheme;
        RefreshPasSynEdit;
      end;

    finally
      Frm.Free;
    end;
  end;
end;

procedure TFrmMain.ImageUpdateClick(Sender: TObject);
begin
 CheckForUpdates(False);
end;

procedure TFrmMain.LoadActionImages;
{
 procedure AddImage(Image:TImage;Const Normal, BN:string);
  Var
   Png : TPngImage;
 begin
  ActionImages.Add(Image.Name,TCompPngImages.Create);
  ActionImages.Items[Image.Name].Normal:=TPngImage.Create;
  Png:=ActionImages.Items[Image.Name].Normal;
  Png.LoadFromFile(Normal);

  ActionImages.Items[Image.Name].BN:=TPngImage.Create;
  Png:=ActionImages.Items[Image.Name].BN;
  Png.LoadFromFile(BN);
  Image.Picture.Assign(ActionImages.Items[Image.Name].BN);
 end;
}

begin
{
  AddImage(ImageBug,ExtractFilePath(ParamStr(0))+'images\Bug.png',ExtractFilePath(ParamStr(0))+'images\BugBN.png');
  AddImage(ImageUpdate,ExtractFilePath(ParamStr(0))+'images\Update.png',ExtractFilePath(ParamStr(0))+'images\UpdateBN.png');
  AddImage(ImageHue,ExtractFilePath(ParamStr(0))+'images\Hue.png',ExtractFilePath(ParamStr(0))+'images\HueBN.png');
  AddImage(ImageConf,ExtractFilePath(ParamStr(0))+'images\Conf.png',ExtractFilePath(ParamStr(0))+'images\ConfBN.png');
}
end;

procedure TFrmMain.LoadFixedWidthFonts;
var
  sDC:     integer;
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

procedure TFrmMain.LoadThemes;
var
  Item    : TListItem;
  FileName: string;
begin
  if not TDirectory.Exists(FSettings.ThemePath) then
    exit;

  LvThemes.SmallImages:=nil;
  ImageListThemes.Clear;
  LvThemes.Items.BeginUpdate;
  try
    LvThemes.Items.Clear;
    for FileName in TDirectory.GetFiles(FSettings.ThemePath, '*.theme.xml') do
    begin
      Item := LvThemes.Items.Add;
      Item.Caption := Copy(ExtractFileName(FileName), 1, Pos('.theme', ExtractFileName(FileName)) - 1);
      Item.SubItems.Add(FileName);
    end;
    FThemeChangued := False;
  finally
    LvThemes.Items.EndUpdate;
  end;
  LvThemes.SmallImages:=ImageListThemes;

  TLoadThemesImages.Create(FSettings.ThemePath, ImageListThemes, LvThemes);
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

    FChanging:=True;
    try

      if FrmColorPanel<>nil then
      begin
        if RadioButtonFore.Checked then
        begin
         FrmColorPanel.SelectedColor:=CblForeground.Selected;
         FrmColorPanel.OnChange:=OnSelForegroundColorChange;
        end
        else
        begin
         FrmColorPanel.SelectedColor:=CblBackground.Selected;
         FrmColorPanel.OnChange:=OnSelBackGroundColorChange;
        end;
      end;

      CheckBold.Checked      := FCurrentTheme[Element].Bold;
      CheckItalic.Checked    := FCurrentTheme[Element].Italic;
      CheckUnderline.Checked := FCurrentTheme[Element].Underline;

      CheckForeground.Checked := FCurrentTheme[Element].DefaultForeground;
      CheckBackground.Checked := FCurrentTheme[Element].DefaultBackground;
    finally
      FChanging:=False;
    end;
  end;
end;

procedure TFrmMain.LvIDEVersionsChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
var
  DelphiVersion: TDelphiVersions;
begin
  if ComboBoxExIDEs.ItemIndex >= 0 then
  begin
    if IDEData.IDEType=TSupportedIDEs.DelphiIDE then
     DelphiVersion := IDEData.Version
    else
    //if IDEType=TSupportedIDEs.LazarusIDE then
     DelphiVersion := TDelphiVersions.DelphiXE; //if is lazarus use the Delphi XE elemnents

    FillListAvailableElements(DelphiVersion, CbElement.Items);

    SynEditCode.Gutter.Visible :=
      DelphiVersionNumbers[DelphiVersion] >= IDEHighlightElementsMinVersion[
      TIDEHighlightElements.LineNumber];

    SynEditCode.Gutter.ShowLineNumbers :=
      DelphiVersionNumbers[DelphiVersion] >= IDEHighlightElementsMinVersion[
      TIDEHighlightElements.LineNumber];

    if IDEData.IDEType=TSupportedIDEs.DelphiIDE then
      UpDownFontSize.Position := GetDelphiIDEFontSize(DelphiVersion)
    else
    if IDEData.IDEType=TSupportedIDEs.LazarusIDE then
      UpDownFontSize.Position := GetLazarusIDEFontSize;

    if CbIDEFonts.Items.Count > 0 then
      if IDEData.IDEType=TSupportedIDEs.DelphiIDE then
        CbIDEFonts.ItemIndex := CbIDEFonts.Items.IndexOf(GetDelphiIDEFontName(DelphiVersion))
      else
      if IDEData.IDEType=TSupportedIDEs.LazarusIDE then
        CbIDEFonts.ItemIndex := CbIDEFonts.Items.IndexOf(GetLazarusIDEFontName);

    CbIDEFontsChange(nil);
    BtnApplyFont.Enabled := False;

    //BtnImportRegTheme.Visible:=not DelphiIsOldVersion(DelphiVersion) and (IDEData.IDEType=TSupportedIDEs.DelphiIDE);
    //BtnExportToLazarusTheme.Visible:=(IDEData.IDEType=TSupportedIDEs.LazarusIDE);

    if (LvThemes.Selected <> nil) and (CbElement.Items.Count > 0) then
    begin
      CbElement.ItemIndex := 0;
      LoadValuesElements;
    end;
  end;
end;


procedure TFrmMain.LvThemesChange(Sender: TObject; Item: TListItem; Change: TItemChange);
Var
  ImpTheme : TIDETheme;
begin
  try
    if (LvThemes.Selected <> nil) and (ComboBoxExIDEs.ItemIndex >= 0) then
    begin
      EditThemeName.Text := LvThemes.Selected.Caption;
      LoadThemeFromXMLFile(ImpTheme, LvThemes.Selected.SubItems[0]);

      if IsValidDelphiIDETheme(ImpTheme) then
      begin
        FCurrentTheme:=ImpTheme;
        RefreshPasSynEdit;
        if CbElement.Items.Count > 0 then
        begin
          CbElement.ItemIndex := 0;
          LoadValuesElements;
          //PaintGutterGlyphs;
          //SynEditCode.InvalidateGutter;
        end;
      end
      else
      MsgBox(Format('The Theme %s has invalid values',[LvThemes.Selected.Caption]));

    end;
  except
    on E: Exception do
      MsgBox(Format('Error loading values of current theme - Message : %s : Trace %s',
        [E.Message, E.StackTrace]));
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

    FCurrentTheme[Element].Bold      := CheckBold.Checked;
    FCurrentTheme[Element].Italic    := CheckItalic.Checked;
    FCurrentTheme[Element].Underline := CheckUnderline.Checked;

    FCurrentTheme[Element].DefaultForeground := CheckForeground.Checked;
    FCurrentTheme[Element].DefaultBackground := CheckBackground.Checked;

    RefreshPasSynEdit;

    if Sender<>nil then
    begin
      if RadioButtonFore.Checked then
      begin
       FrmColorPanel.SelectedColor:=CblForeground.Selected;
       FrmColorPanel.OnChange:=OnSelForegroundColorChange;
      end
      else
      begin
       FrmColorPanel.SelectedColor:=CblBackground.Selected;
       FrmColorPanel.OnChange:=OnSelBackGroundColorChange;
      end;
    end;
  end;
end;


procedure TFrmMain.CMStyleChanged(var Message: TMessage);
begin
  CbElement.Style:=csDropDownList;
  CbElement.OnDrawItem:=nil;

  CbIDEFonts.Style:=csDropDownList;
  CbIDEFonts.OnDrawItem:=nil;

//  CbIDEThemeImport.Style:=csDropDownList;
//  CbIDEThemeImport.OnDrawItem:=nil;

  LvThemes.OwnerDraw  :=false;
  LvThemes.OnDrawItem :=nil;
  LvThemes.OnMouseDown:=nil;

  if not TStyleManager.ActiveStyle.IsSystemStyle then
  begin
    CbElement.Style:=csOwnerDrawFixed;
    CbElement.OnDrawItem:=VclStylesOwnerDrawFix.ComboBoxDrawItem;

    CbIDEFonts.Style:=csOwnerDrawFixed;
    CbIDEFonts.OnDrawItem:=VclStylesOwnerDrawFix.ComboBoxDrawItem;

//    CbIDEThemeImport.Style:=csOwnerDrawFixed;
//    CbIDEThemeImport.OnDrawItem:=VclStylesOwnerDrawFix.ComboBoxDrawItem;

    LvThemes.OwnerDraw  :=True;
    LvThemes.OnDrawItem :=VclStylesOwnerDrawFix.ListViewDrawItem;
    LvThemes.OnMouseDown:=VclStylesOwnerDrawFix.ListViewMouseDown;
  end;


  {
  ApplyVclStylesOwnerDrawFix(Self, false);
  if not TStyleManager.ActiveStyle.IsSystemStyle then
  ApplyVclStylesOwnerDrawFix(Self, true);
  }
end;

procedure TFrmMain.ComboBoxExIDEsChange(Sender: TObject);
var
  DelphiVersion: TDelphiVersions;
  CurrentThemeName : string;
  i: integer;
begin
  if ComboBoxExIDEs.ItemIndex >= 0 then
  begin
    CurrentThemeName:='';
    if IDEData.IDEType=TSupportedIDEs.DelphiIDE then
     DelphiVersion := IDEData.Version
    else
    //if IDEType=TSupportedIDEs.LazarusIDE then
     DelphiVersion := TDelphiVersions.DelphiXE; //if is lazarus, SMS or Appmethod use the Delphi XE elements

    //BtnIDEColorizer.Enabled:=FSettings.ActivateColorizer and (IDEData.IDEType=TSupportedIDEs.DelphiIDE) and  (IDEData.Version in [TDelphiVersions.DelphiXE, TDelphiVersions.DelphiXE2]);

    BtnAdditionalSettings.Visible := DelphiVersion>=TDelphiVersions.DelphiXE8;


    FillListAvailableElements(DelphiVersion, CbElement.Items);

    SynEditCode.Gutter.Visible :=
      DelphiVersionNumbers[DelphiVersion] >= IDEHighlightElementsMinVersion[
      TIDEHighlightElements.LineNumber];

    SynEditCode.Gutter.ShowLineNumbers :=
      DelphiVersionNumbers[DelphiVersion] >= IDEHighlightElementsMinVersion[
      TIDEHighlightElements.LineNumber];

    if IDEData.IDEType=TSupportedIDEs.DelphiIDE then
    begin
      UpDownFontSize.Position := GetDelphiIDEFontSize(DelphiVersion);
      CurrentThemeName:= GetDelphiIDEThemeName(DelphiVersion);
      if CurrentThemeName='' then CurrentThemeName:='default';
    end
    else
    if IDEData.IDEType=TSupportedIDEs.LazarusIDE then
    begin
      UpDownFontSize.Position := GetLazarusIDEFontSize;
      CurrentThemeName:=GetLazarusIDEThemeName;
    end
    else
    if IDEData.IDEType=TSupportedIDEs.SMSIDE then
    begin
      UpDownFontSize.Position := GetSMSIDEFontSize;
      CurrentThemeName:=GetSMSIDEThemeName;
    end;


    if CbIDEFonts.Items.Count > 0 then
      if IDEData.IDEType=TSupportedIDEs.DelphiIDE then
        CbIDEFonts.ItemIndex := CbIDEFonts.Items.IndexOf(GetDelphiIDEFontName(DelphiVersion))
      else
      if IDEData.IDEType=TSupportedIDEs.LazarusIDE then
        CbIDEFonts.ItemIndex := CbIDEFonts.Items.IndexOf(GetLazarusIDEFontName)
      else
      if IDEData.IDEType=TSupportedIDEs.SMSIDE then
        CbIDEFonts.ItemIndex := CbIDEFonts.Items.IndexOf(GetSMSIDEFontName);


    CbIDEFontsChange(nil);
    BtnApplyFont.Enabled := False;

    //BtnImportRegTheme.Visible:=not DelphiIsOldVersion(DelphiVersion) and (IDEData.IDEType=TSupportedIDEs.DelphiIDE);
    //BtnExportToLazarusTheme.Visible:=(IDEData.IDEType=TSupportedIDEs.LazarusIDE);

    if CurrentThemeName<>'' then
      for i:=0 to LvThemes.Items.Count-1 do
        if SameText(CurrentThemeName, LvThemes.Items[i].Caption) then
         begin
           LvThemes.Selected:=LvThemes.Items[i];
           LvThemes.Selected.MakeVisible(false);
           Break;
         end;

    if (LvThemes.Selected <> nil) and (CbElement.Items.Count > 0) then
    begin
      CbElement.ItemIndex := 0;
      LoadValuesElements;
    end;
  end;
end;


procedure TFrmMain.Contribute1Click(Sender: TObject);
begin
 ShellExecute(Handle, 'open', PChar('http://theroadtodelphi.wordpress.com/contributions/'), nil, nil, SW_SHOW);
end;

procedure TFrmMain.CreateThemeFile;
var
  //DelphiVersion: TDelphiVersions;
  FileName:      string;
begin
  if (ComboBoxExIDEs.ItemIndex>=0) then
  begin
    //DelphiVersion := TDelphiVersions(integer(LvIDEVersions.Selected.Data));
    if EditThemeName.Text = '' then
      MsgBox('You must enter a name for the current theme')
    else
    begin
      //FileName := SaveDelphiIDEThemeToXmlFile(DelphiVersion, FCurrentTheme, FSettings.ThemePath, EditThemeName.Text);
      FileName := SaveDelphiIDEThemeToXmlFile(FCurrentTheme, FSettings.ThemePath, EditThemeName.Text);
      MsgBox(Format('The theme was saved to the file %s', [FileName]));
    end;
  end;
end;

{$IFDEF ENABLE_THEME_EXPORT}
procedure TFrmMain.ExportThemeHtml;
begin
  SynExporterHTML1.Color    :=StringToColor(FCurrentTheme[PlainText].BackgroundColorNew);
  SynExporterHTML1.Font.Name:=CbIDEFonts.Text;
  SynExporterHTML1.Font.Size:=UpDownFontSize.Position;
  SynExporterHTML1.ExportAsText:=True;
  SynExporterHTML1.ExportAll(SynEditCode.Lines);
  SynExporterHTML1.SaveToFile('C:\Users\Dexter\Desktop\demo.html');
end;
{$ENDIF}

procedure TFrmMain.RadioButtonForeClick(Sender: TObject);
begin
  CblForeground.Enabled:=RadioButtonFore.Checked;
  BtnSelForColor.Enabled:=RadioButtonFore.Checked;

  CblBackground.Enabled:=RadioButtonBack.Checked;
  BtnSelBackColor.Enabled:=RadioButtonBack.Checked;

  if RadioButtonFore.Checked then
  begin
   FrmColorPanel.OnChange:=OnSelForegroundColorChange;
   FrmColorPanel.SelectedColor:=CblForeground.Selected;
  end
  else
  begin
   FrmColorPanel.OnChange:=OnSelBackGroundColorChange;
   FrmColorPanel.SelectedColor:=CblBackground.Selected;
  end;
end;

procedure TFrmMain.RefreshPasSynEdit;
var
  Element   : TIDEHighlightElements;
  DelphiVer : TDelphiVersions;
  //FG, BG    : TColor;
begin
  if (ComboBoxExIDEs.ItemIndex>=0) and (LvThemes.Selected <> nil) then
  begin
    //Patch colors for Old
    if IDEData.IDEType=TSupportedIDEs.DelphiIDE then
      DelphiVer := IDEData.Version
    else
    //if IDEType=TSupportedIDEs.LazarusIDE then
      DelphiVer := TDelphiVersions.DelphiXE; //if is lazarus use the Delphi XE elemnents

    Element := TIDEHighlightElements.RightMargin;
    SynEditCode.RightEdgeColor :=
      GetDelphiVersionMappedColor(StringToColor(FCurrentTheme[Element].ForegroundColorNew),DelphiVer);

    Element := TIDEHighlightElements.MarkedBlock;
    SynEditCode.SelectedColor.Foreground :=
      GetDelphiVersionMappedColor(StringToColor(FCurrentTheme[Element].ForegroundColorNew),DelphiVer);
    SynEditCode.SelectedColor.Background :=
      GetDelphiVersionMappedColor(StringToColor(FCurrentTheme[Element].BackgroundColorNew),DelphiVer);

    Element := TIDEHighlightElements.LineNumber;
    SynEditCode.Gutter.Color := StringToColor(FCurrentTheme[Element].BackgroundColorNew);
    SynEditCode.Gutter.Font.Color :=
      GetDelphiVersionMappedColor(StringToColor(FCurrentTheme[Element].ForegroundColorNew),DelphiVer);

    Element := TIDEHighlightElements.LineHighlight;
    SynEditCode.ActiveLineColor :=
      GetDelphiVersionMappedColor(StringToColor(FCurrentTheme[Element].BackgroundColorNew),DelphiVer);

    Element := TIDEHighlightElements.PlainText;
    SynEditCode.Gutter.BorderColor := GetHighLightColor(StringToColor(FCurrentTheme[Element].BackgroundColorNew));

    with SynPasSyn1 do
    begin
      SetSynAttr(TIDEHighlightElements.Assembler, AsmAttri,DelphiVer);
      SetSynAttr(TIDEHighlightElements.Character, CharAttri,DelphiVer);
      SetSynAttr(TIDEHighlightElements.Comment, CommentAttri,DelphiVer);
      SetSynAttr(TIDEHighlightElements.Preprocessor, DirectiveAttri,DelphiVer);
      SetSynAttr(TIDEHighlightElements.Float, FloatAttri,DelphiVer);
      SetSynAttr(TIDEHighlightElements.Hex, HexAttri,DelphiVer);
      SetSynAttr(TIDEHighlightElements.Identifier, IdentifierAttri,DelphiVer);
      SetSynAttr(TIDEHighlightElements.ReservedWord, KeyAttri,DelphiVer);
      SetSynAttr(TIDEHighlightElements.Number, NumberAttri,DelphiVer);
      SetSynAttr(TIDEHighlightElements.Whitespace, SpaceAttri,DelphiVer);
      SetSynAttr(TIDEHighlightElements.String, StringAttri,DelphiVer);
      SetSynAttr(TIDEHighlightElements.Symbol, SymbolAttri,DelphiVer);
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

procedure TFrmMain.SetSynAttr(Element: TIDEHighlightElements;
  SynAttr: TSynHighlighterAttributes;DelphiVersion : TDelphiVersions);
begin
  SynAttr.Background := GetDelphiVersionMappedColor(StringToColor(FCurrentTheme[Element].BackgroundColorNew),DelphiVersion);
  SynAttr.Foreground := GetDelphiVersionMappedColor(StringToColor(FCurrentTheme[Element].ForegroundColorNew),DelphiVersion);
  SynAttr.Style      := [];
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
  i:    integer;
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
    index: integer;
  begin
    Result := False;
    index  := GetIndexHighlightElements(AElement);
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
    InvalidBreakLine: Done   := SetCbElement(TIDEHighlightElements.InvalidBreak);
    ExecutionPointLine: Done := SetCbElement(TIDEHighlightElements.ExecutionPoint);
    EnabledBreakLine: Done   := SetCbElement(TIDEHighlightElements.EnabledBreak);
    DisabledBreakLine: Done  := SetCbElement(TIDEHighlightElements.DisabledBreak);
    ErrorLineLine: Done      := SetCbElement(TIDEHighlightElements.ErrorLine);
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

procedure TFrmMain.SynEditCodeGutterClick(Sender: TObject; Button: TMouseButton;
  X, Y, Line: integer; Mark: TSynEditMark);
var
  index: integer;
begin
  index := GetElementIndex(TIDEHighlightElements.LineNumber);
  if index >= 0 then
  begin
    CbElement.ItemIndex := index;
    CbElementChange(nil);
  end;
end;

procedure TFrmMain.SynEditCodeSpecialLineColors(Sender: TObject;
  Line: integer; var Special: boolean; var FG, BG: TColor);

  procedure SetColorSpecialLine(AElement: TIDEHighlightElements);
  begin
    FG      := StringToColor(FCurrentTheme[AElement].ForegroundColorNew);
    BG      := StringToColor(FCurrentTheme[AElement].BackgroundColorNew);
    Special := True;
  end;

begin
  if LvThemes.Selected<>nil then
  case Line of
    InvalidBreakLine  : SetColorSpecialLine(InvalidBreak);
    ExecutionPointLine: SetColorSpecialLine(ExecutionPoint);
    EnabledBreakLine  : SetColorSpecialLine(EnabledBreak);
    DisabledBreakLine : SetColorSpecialLine(DisabledBreak);
    ErrorLineLine     : SetColorSpecialLine(ErrorLine);
  end;
end;

{ TCompPngImages }

destructor TCompPngImages.destroy;
begin
 if Assigned(FNormal) then
  FNormal.Free;

 if Assigned(FBN) then
  FBN.Free;

 Inherited;
end;

//initialization
//   TStyleManager.Engine.RegisterStyleHook(TComboBoxEx, TComboBoxExStyleHookFix);



end.
