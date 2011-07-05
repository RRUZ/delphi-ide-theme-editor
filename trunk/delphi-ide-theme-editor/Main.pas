{**************************************************************************************************}
{                                                                                                  }
{ Unit Main                                                                                        }
{ Main Form  for the Delphi IDE Theme Editor                                                       }
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
{**************************************************************************************************}

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
 //update online   (check new version)

 //import from http://studiostyl.es/schemes
 //import fom notepad++
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
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ImgList, StdCtrls, ComCtrls, ExtCtrls, SynEditHighlighter,uSupportedIDEs,
  SynHighlighterPas, SynEdit, SynMemo, uDelphiVersions, uDelphiIDEHighlight, uLazarusVersions,
  pngimage, uSettings, ExtDlgs, Menus, SynEditExport, SynExportHTML, JvBaseDlg,
  JvBrowseFolder;

{.$DEFINE ENABLE_THEME_EXPORT}

type
  //THackSynPasSyn= class(TSynPasSyn);
  TFrmMain = class(TForm)
    ImageListDelphiVersion: TImageList;
    Label1:      TLabel;
    LvIDEVersions: TListView;
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
    Label3:      TLabel;
    CblBackground: TColorBox;
    Label4:      TLabel;
    SynPasSyn1:  TSynPasSyn;
    Label5:      TLabel;
    CbIDEFonts:  TComboBox;
    EditFontSize: TEdit;
    UpDownFontSize: TUpDown;
    BtnApply:    TButton;
    Label6:      TLabel;
    EditThemeName: TEdit;
    BtnSave:     TButton;
    ImageLogo:   TImage;
    BtnApplyFont: TButton;
    Label7:      TLabel;
    LvThemes:    TListView;
    BtnImport:   TButton;
    OpenDialogImport: TOpenDialog;
    BtnImportRegTheme: TButton;
    BtnSetDefault: TButton;
    Label8:      TLabel;
    ProgressBar1: TProgressBar;
    LabelVersion: TLabel;
    SynEditCode: TSynEdit;
    ImageConf:   TImage;
    ImageHue:    TImage;
    CbIDEThemeImport: TComboBox;
    ImageListlGutterGlyphs: TImageList;
    BtnSelForColor: TButton;
    ImageList1: TImageList;
    BtnSelBackColor: TButton;
    ImageBug: TImage;
    PopupMenuThemes: TPopupMenu;
    CloneTheme1: TMenuItem;
    DeleteTheme1: TMenuItem;
    ApplyTheme1: TMenuItem;
    SaveChanges1: TMenuItem;
    SaveAs1: TMenuItem;
    LabelMsg: TLabel;
    Button1: TButton;
    SynExporterHTML1: TSynExporterHTML;
    BtnExportToLazarusTheme: TButton;
    JvBrowseForFolderDialog1: TJvBrowseForFolderDialog;
    OpenDialogExport: TOpenDialog;
    procedure FormCreate(Sender: TObject);
    procedure LvIDEVersionsChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure CbElementChange(Sender: TObject);
    procedure CbIDEFontsChange(Sender: TObject);
    procedure CblForegroundChange(Sender: TObject);
    procedure BtnSaveClick(Sender: TObject);
    procedure BtnApplyClick(Sender: TObject);
    procedure BtnApplyFontClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LvThemesChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure BtnImportClick(Sender: TObject);
    procedure BtnImportRegThemeClick(Sender: TObject);
    procedure BtnSetDefaultClick(Sender: TObject);
    procedure SynEditCodeClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ImageConfClick(Sender: TObject);
    procedure ImageHueClick(Sender: TObject);
    procedure WMNCHitTest(var Msg: TWMNCHitTest); message WM_NCHitTest;
    procedure SynEditCodeSpecialLineColors(Sender: TObject; Line: integer;
      var Special: boolean; var FG, BG: TColor);
    procedure SynEditCodeGutterClick(Sender: TObject; Button: TMouseButton;
      X, Y, Line: integer; Mark: TSynEditMark);
    procedure BtnSelForColorClick(Sender: TObject);
    procedure ImageBugClick(Sender: TObject);
    procedure BtnSelBackColorClick(Sender: TObject);
    procedure DeleteTheme1Click(Sender: TObject);
    procedure CloneTheme1Click(Sender: TObject);
    procedure SaveAs1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure BtnExportToLazarusThemeClick(Sender: TObject);
  private
    FChanging     : boolean;
    FThemeChangued: boolean;
    FSettings:      TSettings;
    FCurrentTheme:  TIDETheme;
    FMapHighlightElementsTSynAttr: TStrings;
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
    {$IFDEF ENABLE_THEME_EXPORT}
    procedure ExportThemeHtml;
    {$ENDIF}
    function GetIDEType(Item:TListItem):TSupportedIDEs;

  public
    { Public declarations }
  end;

var
  FrmMain: TFrmMain;

implementation

uses
  Diagnostics,
  ShellApi,
  IOUtils,
  StrUtils,
  uHueSat,
  uColorSelector,
  EclipseThemes,
  GraphUtil,
  VSThemes, uMisc, uLazarusIDEHighlight;

const
  InvalidBreakLine   = 9;
  ExecutionPointLine = 10;
  EnabledBreakLine   = 12;
  DisabledBreakLine  = 13;
  ErrorLineLine      = 14;

{$R *.dfm}
{$R ManAdmin.RES}

procedure MsgBox(const Msg: string);
begin
  Application.MessageBox(PChar(Msg), 'Information', MB_OK + MB_ICONINFORMATION);
end;

function EnumFontsProc(var LogFont: TLogFont; var TextMetric: TTextMetric;
  FontType: integer; Data: Pointer): integer; stdcall;
begin
  //  if ((FontType and TrueType_FontType) <> 0) and  ((LogFont.lfPitchAndFamily and VARIABLE_PITCH) = 0) then
  if ((LogFont.lfPitchAndFamily and FIXED_PITCH) <> 0) then
    if not StartsText('@', LogFont.lfFaceName) and
      (FrmMain.CbIDEFonts.Items.IndexOf(LogFont.lfFaceName) < 0) then
      FrmMain.CbIDEFonts.Items.Add(LogFont.lfFaceName);

  Result := 1;
end;

procedure TFrmMain.ApplyCurentTheme;
var
  DelphiVersion: TDelphiVersions;
  IDEType      : TSupportedIDEs;
begin
  if LvIDEVersions.Selected <> nil then
  begin
    IDEType:=GetIDEType(LvIDEVersions.Selected);
    if IDEType=TSupportedIDEs.DelphiIDE then
     DelphiVersion := TDelphiVersions(integer(LvIDEVersions.Selected.Data))
    else
    //if IDEType=TSupportedIDEs.LazarusIDE then
     DelphiVersion := TDelphiVersions.DelphiXE; //if is lazarus use the Delphi XE elements


    if IDEType=TSupportedIDEs.DelphiIDE then
      if ApplyDelphiIDETheme(DelphiVersion, FCurrentTheme) then
        MsgBox('The theme was successfully applied')
      else
        MsgBox('Error setting theme')
    else
    if IDEType=TSupportedIDEs.LazarusIDE then
      if ApplyLazarusIDETheme(FCurrentTheme,EditThemeName.Text) then
        MsgBox('The theme was successfully applied')
      else
        MsgBox('Error setting theme');
  end;
end;

procedure TFrmMain.BtnApplyClick(Sender: TObject);
begin
  try
    if (LvIDEVersions.Selected <> nil) and (LvThemes.Selected <> nil) then
      if IsAppRunning(LvIDEVersions.Selected.SubItems[0]) then
        MsgBox(Format('Before to continue you must close all running instances of the %s IDE',
          [LvIDEVersions.Selected.Caption]))
      else
      if Application.MessageBox(
        PChar(Format('Do you want apply the theme "%s" to the %s IDE?',
        [LvThemes.Selected.Caption, LvIDEVersions.Selected.Caption])), 'Confirmation',
        MB_YESNO + MB_ICONQUESTION) = idYes then
        ApplyCurentTheme;
  except
    on E: Exception do
      MsgBox(Format('Error setting theme - Message : %s : Trace %s',
        [E.Message, E.StackTrace]));
  end;
end;

procedure TFrmMain.BtnApplyFontClick(Sender: TObject);
var
  DelphiVersion: TDelphiVersions;
  IDEType      : TSupportedIDEs;
begin
  try
    if (LvIDEVersions.Selected <> nil) then
      if IsAppRunning(LvIDEVersions.Selected.SubItems[0]) then
        MsgBox(Format('Before to continue you must close all running instances of the %s IDE',
          [LvIDEVersions.Selected.Caption]))
      else
      if Application.MessageBox(
        PChar(Format('Do you want apply the "%s" font to the %s IDE?',
        [CbIDEFonts.Text, LvIDEVersions.Selected.Caption])), 'Confirmation',
        MB_YESNO + MB_ICONQUESTION) = idYes then
      begin
        IDEType:=GetIDEType(LvIDEVersions.Selected);
        if IDEType=TSupportedIDEs.DelphiIDE then
         DelphiVersion := TDelphiVersions(integer(LvIDEVersions.Selected.Data))
        else
        //if IDEType=TSupportedIDEs.LazarusIDE then
         DelphiVersion := TDelphiVersions.DelphiXE; //if is lazarus use the Delphi XE elements


        if IDEType=TSupportedIDEs.DelphiIDE then
        begin
          if SetDelphiIDEFont(DelphiVersion, CbIDEFonts.Text, UpDownFontSize.Position) then
            MsgBox('The font was successfully applied')
          else
            MsgBox('Error setting font');
        end
        else
        if IDEType=TSupportedIDEs.LazarusIDE then
        begin
          if SetLazarusIDEFont(CbIDEFonts.Text, UpDownFontSize.Position) then
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

procedure TFrmMain.BtnExportToLazarusThemeClick(Sender: TObject);
var
  i: integer;
  OutPutFolder : String;
  GoNext : Boolean;
  s  : TStopwatch;
begin
  try

    OutPutFolder:=IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)))+'Themes Lazarus';
    if DirectoryExists(OutPutFolder) then
      JvBrowseForFolderDialog1.Directory := OutPutFolder;

    if JvBrowseForFolderDialog1.Execute then
      OutPutFolder := JvBrowseForFolderDialog1.Directory
    else
      Exit;


    OpenDialogExport.InitialDir := ExtractFilePath(ParamStr(0));
    if OpenDialogExport.Execute(Handle) then
    begin

      if OpenDialogExport.Files.Count = 1 then
        GoNext := Application.MessageBox(
          PChar(Format('Do you want import the "%s" file?', [ExtractFileName(
          OpenDialogExport.FileName)])), 'Confirmation', MB_YESNO + MB_ICONQUESTION) = idYes
      else
        GoNext := Application.MessageBox(
          PChar(Format('Do you want import the %d files selected?',
          [OpenDialogExport.Files.Count])),
          'Confirmation', MB_YESNO + MB_ICONQUESTION) = idYes;

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
      MsgBox(Format('Error importing theme from registry - Message : %s : Trace %s',
        [E.Message, E.StackTrace]));
  end;
end;

procedure TFrmMain.BtnSaveClick(Sender: TObject);
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

procedure TFrmMain.BtnSetDefaultClick(Sender: TObject);
var
  DelphiVersion: TDelphiVersions;
begin
  try
    if LvIDEVersions.Selected <> nil then
      if not IsAppRunning(LvIDEVersions.Selected.SubItems[0]) then
        if Application.MessageBox(
          PChar(Format('Do you want apply the default theme to the "%s" IDE?',
          [LvIDEVersions.Selected.Caption])), 'Confirmation', MB_YESNO +
          MB_ICONQUESTION) = idYes then
        begin
          DelphiVersion := TDelphiVersions(integer(LvIDEVersions.Selected.Data));
          if SetDelphiIDEDefaultTheme(DelphiVersion) then
            MsgBox('Default theme wae applied')
          else
            MsgBox('Error setting theme');
        end;
  except
    on E: Exception do
      MsgBox(Format('Error setting default theme - Message : %s : Trace %s',
        [E.Message, E.StackTrace]));
  end;
end;


procedure TFrmMain.Button1Click(Sender: TObject);
begin
 ShellExecute(Handle, 'open', PChar('http://theroadtodelphi.wordpress.com/contributions/'), nil, nil, SW_SHOW);
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
  CblForeground.Selected:=TDialogColorSelector(Sender).SelectedColor;
  CblForegroundChange(CblForeground);
end;

procedure TFrmMain.OnSelBackGroundColorChange(Sender: TObject);
begin
  CblBackground.Selected:=TDialogColorSelector(Sender).SelectedColor;
  CblForegroundChange(CblBackground);
end;

procedure TFrmMain.BtnImportRegThemeClick(Sender: TObject);
var
  ThemeName: string;
  DelphiVersion: TDelphiVersions;
  i: integer;
begin
  try
    if LvIDEVersions.Selected <> nil then
      if Application.MessageBox(
        PChar(Format('Do you want import the current theme from  the "%s" IDE?',
        [LvIDEVersions.Selected.Caption])), 'Confirmation', MB_YESNO +
        MB_ICONQUESTION) = idYes then
      begin
        DelphiVersion := TDelphiVersions(integer(LvIDEVersions.Selected.Data));
        if not ExistDelphiIDEThemeToImport(DelphiVersion) then
        begin
          Application.MessageBox(
            PChar(Format('The "%s" IDE has not themes stored in the windows registry?',
            [LvIDEVersions.Selected.Caption])), 'Information', MB_OK + MB_ICONINFORMATION);
          exit;
        end;


        ThemeName := InputBox('Import Delphi IDE Theme',
          'Enter the name for the theme to import', '');
        if ThemeName <> '' then
        begin

          ImportDelphiIDEThemeFromReg(FCurrentTheme, DelphiVersion);
          SaveDelphiIDEThemeToXmlFile(DelphiVersion, FCurrentTheme, FSettings.ThemePath, ThemeName);
          EditThemeName.Text := ThemeName;
          MsgBox('Theme imported');
          LoadThemes;

          for i := 0 to LvThemes.Items.Count - 1 do
            if CompareText(LvThemes.Items.Item[i].Caption, EditThemeName.Text) = 0 then
            begin
              LvThemes.Selected := LvThemes.Items.Item[i];
              Break;
            end;
        end;
      end;
  except
    on E: Exception do
      MsgBox(Format('Error importing theme from registry - Message : %s : Trace %s',
        [E.Message, E.StackTrace]));
  end;

end;

procedure TFrmMain.BtnImportClick(Sender: TObject);
var
  ThemeName: string;
  i:      integer;
  DelphiVersion: TDelphiVersions;
  ImportType: TIDEImportThemes;
  GoNext: boolean;
  s : TStopwatch;
begin
  ImportType := TIDEImportThemes(
    CbIDEThemeImport.Items.Objects[CbIDEThemeImport.ItemIndex]);
  OpenDialogImport.Filter := IDEImportThemesDialogFilter[ImportType];
  OpenDialogImport.InitialDir := ExtractFilePath(ParamStr(0));
  try
    if (LvIDEVersions.Selected <> nil) and OpenDialogImport.Execute(Handle) then
    begin
      DelphiVersion := DelphiXE;//TDelphiVersions(integer(LvDelphiVersions.Selected.Data));

      if OpenDialogImport.Files.Count = 1 then
        GoNext := Application.MessageBox(
          PChar(Format('Do you want import the "%s" file?', [ExtractFileName(
          OpenDialogImport.FileName)])), 'Confirmation', MB_YESNO + MB_ICONQUESTION) = idYes
      else
        GoNext := Application.MessageBox(
          PChar(Format('Do you want import the %d files selected?',
          [OpenDialogImport.Files.Count])),
          'Confirmation', MB_YESNO + MB_ICONQUESTION) = idYes;

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
begin
  FChanging := False;
  FSettings := TSettings.Create;
  ReadSettings(FSettings);

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

  FillListIDEThemesImport(CbIDEThemeImport.Items);
  CbIDEThemeImport.ItemIndex := 0;


  LabelVersion.Caption := Format('Version %s', [uMisc.GetFileVersion(ParamStr(0))]);
  FillListViewDelphiVersions(LvIDEVersions);
  if IsLazarusInstalled then
   FillListViewLazarusVersions(LvIDEVersions);


  LoadFixedWidthFonts;
  LoadThemes;


  if LvIDEVersions.Items.Count > 0 then
    LvIDEVersions.Selected := LvIDEVersions.Items.Item[0]
  else
  begin
    MsgBox('You don''t have a Object Pascal IDE installed');
    Halt(0);
  end;

  if LvThemes.Items.Count > 0 then
    LvThemes.Selected := LvThemes.Items.Item[0];
end;


procedure TFrmMain.FormDestroy(Sender: TObject);
begin
  FSettings.Free;
  FMapHighlightElementsTSynAttr.Free;
end;

procedure TFrmMain.FormShow(Sender: TObject);
begin
  BtnApplyFont.Enabled := False;
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

function TFrmMain.GetIDEType(Item: TListItem): TSupportedIDEs;
begin
  // item.SubItems.Add(IntToStr(Ord(TSupportedIDEs.LazarusIDE)));
  Result:= TSupportedIDEs(StrtoInt(item.SubItems[1]));
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
  if (LvIDEVersions.Selected <> nil) and (LvThemes.Selected <> nil) then
  begin

    Frm := TFrmHueSat.Create(nil);
    try
      FBackUpTheme  := FCurrentTheme;
      DelphiVersion := TDelphiVersions(integer(LvIDEVersions.Selected.Data));
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

//\HKCU\Software\CodeGear\ETM\12.0\Color
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
    EnumFontFamiliesEx(sDC, LogFont, @EnumFontsProc, 0, 0);
  finally
    ReleaseDC(0, sDC);
  end;
end;

procedure TFrmMain.LoadThemes;
var
  Item:     TListItem;
  FileName: string;
begin
  if not TDirectory.Exists(FSettings.ThemePath) then
    exit;

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

end;

procedure TFrmMain.LoadValuesElements;
var
  Element: TIDEHighlightElements;
begin
  if LvIDEVersions.Selected <> nil then
  begin
    Element := TIDEHighlightElements(CbElement.Items.Objects[CbElement.ItemIndex]);
    CblForeground.Selected := StringToColor(FCurrentTheme[Element].ForegroundColorNew);
    CblBackground.Selected := StringToColor(FCurrentTheme[Element].BackgroundColorNew);
    FChanging:=True;
    try
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
  IDEType      : TSupportedIDEs;
begin
  if LvIDEVersions.Selected <> nil then
  begin
    IDEType:=GetIDEType(LvIDEVersions.Selected);
    if IDEType=TSupportedIDEs.DelphiIDE then
     DelphiVersion := TDelphiVersions(integer(LvIDEVersions.Selected.Data))
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

    if IDEType=TSupportedIDEs.DelphiIDE then
      UpDownFontSize.Position := GetDelphiIDEFontSize(DelphiVersion)
    else
    if IDEType=TSupportedIDEs.LazarusIDE then
      UpDownFontSize.Position := GetLazarusIDEFontSize;


    if CbIDEFonts.Items.Count > 0 then
      if IDEType=TSupportedIDEs.DelphiIDE then
        CbIDEFonts.ItemIndex := CbIDEFonts.Items.IndexOf(GetDelphiIDEFontName(DelphiVersion))
      else
      if IDEType=TSupportedIDEs.LazarusIDE then
        CbIDEFonts.ItemIndex := CbIDEFonts.Items.IndexOf(GetLazarusIDEFontName);

    CbIDEFontsChange(nil);
    BtnApplyFont.Enabled := False;

    BtnImportRegTheme.Visible:=not DelphiIsOldVersion(DelphiVersion) and (IDEType=TSupportedIDEs.DelphiIDE);
    BtnExportToLazarusTheme.Visible:=IDEType=TSupportedIDEs.LazarusIDE;

    if (LvThemes.Selected <> nil) and (CbElement.Items.Count > 0) then
    begin
      CbElement.ItemIndex := 0;
      LoadValuesElements;
    end;
  end;
end;


procedure TFrmMain.LvThemesChange(Sender: TObject; Item: TListItem; Change: TItemChange);
begin
  try
    if (LvThemes.Selected <> nil) and (LvIDEVersions.Selected <> nil) then
    begin
      EditThemeName.Text := LvThemes.Selected.Caption;
      LoadThemeFromXMLFile(FCurrentTheme, LvThemes.Selected.SubItems[0]);
      RefreshPasSynEdit;
      if CbElement.Items.Count > 0 then
      begin
        CbElement.ItemIndex := 0;
        LoadValuesElements;
        //PaintGutterGlyphs;
        //SynEditCode.InvalidateGutter;
      end;
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
  if (LvIDEVersions.Selected <> nil) and (not FChanging) then
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
  end;
end;


procedure TFrmMain.CloneTheme1Click(Sender: TObject);
var
  index    : integer;
  FileName : string;
  NFileName: string;
begin
  try
    if LvThemes.Selected <> nil then
    begin
      index    := LvThemes.Selected.Index;
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


procedure TFrmMain.CreateThemeFile;
var
  DelphiVersion: TDelphiVersions;
  FileName:      string;
begin
  if LvIDEVersions.Selected <> nil then
  begin
    DelphiVersion := TDelphiVersions(integer(LvIDEVersions.Selected.Data));
    if EditThemeName.Text = '' then
      MsgBox('You must enter a name for the current theme')
    else
    begin
      //FileName:=SaveCurrentThemeToRegFile(DelphiVersion,ExtractFilePath(ParamStr(0)),EditThemeName.Text);
      FileName := SaveDelphiIDEThemeToXmlFile(DelphiVersion, FCurrentTheme,
        FSettings.ThemePath, EditThemeName.Text);
      MsgBox(Format('The theme was saved to the file %s', [FileName]));
    end;
  end;
end;
               
procedure TFrmMain.DeleteTheme1Click(Sender: TObject);
var
  index: integer;
begin
  try
    if LvThemes.Selected <> nil then
      if Application.MessageBox(
        PChar(Format('Do you want delete the theme "%s"?',[LvThemes.Selected.Caption])), 'Confirmation',
        MB_YESNO + MB_ICONQUESTION) = idYes then
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

procedure TFrmMain.RefreshPasSynEdit;
var
  Element   : TIDEHighlightElements;
  DelphiVer : TDelphiVersions;
  IDEType   : TSupportedIDEs;
  //FG, BG    : TColor;
begin

  if (LvIDEVersions.Selected <> nil) and (LvThemes.Selected <> nil) then
  begin
    //Patch colors for Old
    IDEType:=GetIDEType(LvIDEVersions.Selected);
    if IDEType=TSupportedIDEs.DelphiIDE then
      DelphiVer := TDelphiVersions(integer(LvIDEVersions.Selected.Data))
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

procedure TFrmMain.SaveAs1Click(Sender: TObject);
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
  case Line of
    InvalidBreakLine  : SetColorSpecialLine(InvalidBreak);
    ExecutionPointLine: SetColorSpecialLine(ExecutionPoint);
    EnabledBreakLine  : SetColorSpecialLine(EnabledBreak);
    DisabledBreakLine : SetColorSpecialLine(DisabledBreak);
    ErrorLineLine     : SetColorSpecialLine(ErrorLine);
  end;
end;

procedure TFrmMain.WMNCHitTest(var Msg: TWMNCHitTest);
var
  APoint: TPoint;
begin
  inherited;
  APoint.X := Msg.XPos;
  APoint.Y := Msg.YPos;
  APoint   := ScreenToClient(APoint);
  if (Msg.Result = htClient) and ((APoint.Y <= GlassFrame.Top) or (APoint.Y >= ClientHeight - GlassFrame.Bottom)) and
    (not PtInRect(ImageHue.BoundsRect, APoint)) and  (not PtInRect(ImageConf.BoundsRect, APoint)) and  (not PtInRect(ImageBug.BoundsRect, APoint)) then
    Msg.Result := htCaption;
end;

end.
