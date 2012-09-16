{**************************************************************************************************}
{                                                                                                  }
{ Unit uSettings                                                                                   }
{ unit uSettings  for the Delphi IDE Theme Editor                                                  }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is uSettings.pas.                                                              }
{                                                                                                  }
{ The Initial Developer of the Original Code is Rodrigo Ruz V.                                     }
{ Portions created by Rodrigo Ruz V. are Copyright (C) 2011 Rodrigo Ruz V.                         }
{ All Rights Reserved.                                                                             }
{                                                                                                  }
{**************************************************************************************************}

unit uSettings;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TSettings = class
  private
    FThemePath: string;
    FVCLStyle: string;
    FActivateColorizer: Boolean;
    FCheckForUpdates: Boolean;
    FApplyThemeHelpInsight: Boolean;
  public
    property ThemePath: string Read FThemePath Write FThemePath;
    property VCLStyle: string Read FVCLStyle Write FVCLStyle;
    property ActivateColorizer: Boolean Read FActivateColorizer write FActivateColorizer;
    property CheckForUpdates : Boolean Read  FCheckForUpdates write FCheckForUpdates;
    property ApplyThemeHelpInsight: Boolean Read FApplyThemeHelpInsight write FApplyThemeHelpInsight;
  end;


  TFrmSettings = class(TForm)
    BtnSave:   TButton;
    Label1:    TLabel;
    EditThemesFolder: TEdit;
    BtnSelFolderThemes: TButton;
    BtnCancel: TButton;
    Bevel1:    TBevel;
    Label9: TLabel;
    ComboBoxVCLStyle: TComboBox;
    ImageVCLStyle: TImage;
    CheckBoxUpdates: TCheckBox;
    CheckBoxHelpInsight: TCheckBox;
    procedure BtnSelFolderThemesClick(Sender: TObject);
    procedure BtnSaveClick(Sender: TObject);
    procedure BtnCancelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ComboBoxVCLStyleChange(Sender: TObject);
  private
    FSettings: TSettings;
    procedure  LoadStyles;
    procedure DrawSeletedVCLStyle;
  public
    property Settings: TSettings Read FSettings Write FSettings;
    procedure LoadSettings;
  end;

procedure ReadSettings(var Settings: TSettings);
procedure WriteSettings(const Settings: TSettings);
procedure LoadVCLStyle(Const StyleName:String);

implementation

uses
  {$WARN UNIT_PLATFORM OFF}
  Vcl.FileCtrl,
  {$WARN UNIT_PLATFORM ON}
  Vcl.Styles,
  Vcl.Themes,
  Vcl.Styles.Ext,
  IOUtils,
  IniFiles;


{$R *.dfm}

procedure RegisterVCLStyle(const StyleFileName: string);
begin
   if TStyleManager.IsValidStyle(StyleFileName) then
     TStyleManager.LoadFromFile(StyleFileName)
   else
     ShowMessage('The Vcl Style file is not valid');
end;

procedure LoadVCLStyle(Const StyleName:String);
begin
  if StyleName<>'' then
   TStyleManager.SetStyle(StyleName)
  else
   TStyleManager.SetStyle(TStyleManager.SystemStyle.Name);
       {
  if CompareText(StyleName,'Windows')=0 then
   TStyleManager.SetStyle(TStyleManager.SystemStyle.Name)
  else
   RegisterAndSetVCLStyle( IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)))+'Styles\'+StyleName+'.vsf');
       }
end;


procedure ReadSettings(var Settings: TSettings);
var
  iniFile: TIniFile;
begin
  iniFile := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'Settings.ini');
  try
    Settings.ActivateColorizer:= iniFile.ReadBool('Global', 'ActivateColorizer',  False);
    Settings.VCLStyle  := iniFile.ReadString('Global', 'VCLStyle',  'Windows');
    Settings.ThemePath := iniFile.ReadString('Global', 'ThemePath',  ExtractFilePath(ParamStr(0)) + 'Themes');
    Settings.CheckForUpdates :=iniFile.ReadBool('Global', 'CheckForUpdates',  True);
    Settings.ApplyThemeHelpInsight :=iniFile.ReadBool('Global', 'ApplyThemeHelpInsight',  True);
    if not TDirectory.Exists(Settings.ThemePath) then
    begin
      Settings.ThemePath := ExtractFilePath(ParamStr(0)) + 'Themes';
      SysUtils.ForceDirectories(Settings.ThemePath);
    end;
  finally
    iniFile.Free;
  end;
end;

procedure WriteSettings(const Settings: TSettings);
var
  iniFile: TIniFile;
begin
  iniFile := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'Settings.ini');
  try
    iniFile.WriteString('Global', 'ThemePath', Settings.ThemePath);
    iniFile.WriteString('Global', 'VCLStyle', Settings.VCLStyle);
    iniFile.WriteBool('Global', 'CheckForUpdates', Settings.CheckForUpdates);
    iniFile.WriteBool('Global', 'ApplyThemeHelpInsight', Settings.ApplyThemeHelpInsight);
  finally
    iniFile.Free;
  end;
end;


procedure TFrmSettings.BtnCancelClick(Sender: TObject);
begin
  Close();
end;

procedure TFrmSettings.BtnSaveClick(Sender: TObject);
begin
  if MessageDlg('Do you want save the changes ?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    FSettings.ThemePath := EditThemesFolder.Text;
    FSettings.VCLStyle  := ComboBoxVCLStyle.Text;
    FSettings.CheckForUpdates :=CheckBoxUpdates.Checked;
    FSettings.ApplyThemeHelpInsight :=CheckBoxHelpInsight.Checked;
    WriteSettings(FSettings);
    LoadVCLStyle(ComboBoxVCLStyle.Text);
    Close();
  end;
end;

procedure TFrmSettings.BtnSelFolderThemesClick(Sender: TObject);
var
  Directory: string;
begin
  Directory:='';
  if SysUtils.DirectoryExists(EditThemesFolder.Text) then
    Directory := EditThemesFolder.Text;

  if SelectDirectory('Select directory',Directory,Directory,[sdNewFolder, sdNewUI, sdShowEdit, sdValidateDir, sdShowShares], nil) then
    EditThemesFolder.Text := Directory;
end;


procedure TFrmSettings.ComboBoxVCLStyleChange(Sender: TObject);
begin
 //LoadVCLStyle(ComboBoxVCLStyle.Text);
 DrawSeletedVCLStyle;
end;

procedure TFrmSettings.DrawSeletedVCLStyle;
var
  StyleName : string;
  LBitmap   : TBitmap;
  LStyle    : TCustomStyleExt;
  SourceInfo: TSourceInfo;
begin
   ImageVCLStyle.Picture:=nil;

   StyleName:=ComboBoxVCLStyle.Text;
   if (StyleName<>'') and (CompareText('Windows',StyleName)<>0) then
   begin
    LBitmap:=TBitmap.Create;
    try
       LBitmap.PixelFormat:=pf32bit;
       LBitmap.Width :=ImageVCLStyle.ClientRect.Width;
       LBitmap.Height:=ImageVCLStyle.ClientRect.Height;
       SourceInfo:=TStyleManager.StyleSourceInfo[StyleName];
       LStyle:=TCustomStyleExt.Create(TStream(SourceInfo.Data));
       try
         DrawSampleWindow(LStyle, LBitmap.Canvas, ImageVCLStyle.ClientRect, StyleName);
         ImageVCLStyle.Picture.Assign(LBitmap);
       finally
         LStyle.Free;
       end;
    finally
      LBitmap.Free;
    end;
   end;
end;

procedure TFrmSettings.FormCreate(Sender: TObject);
begin
  LoadStyles;
end;

procedure TFrmSettings.LoadSettings;
begin
  ReadSettings(FSettings);
  EditThemesFolder.Text := FSettings.ThemePath;
  ComboBoxVCLStyle.ItemIndex:=ComboBoxVCLStyle.Items.IndexOf(FSettings.VCLStyle);
  CheckBoxUpdates.Checked:=FSettings.CheckForUpdates;
  CheckBoxHelpInsight.Checked:=FSettings.ApplyThemeHelpInsight;
  DrawSeletedVCLStyle;
end;

procedure TFrmSettings.LoadStyles;
var
  Style   : string;
begin
  try
    ComboBoxVCLStyle.Items.BeginUpdate;
    ComboBoxVCLStyle.Items.Clear;
    for Style in TStyleManager.StyleNames do
      ComboBoxVCLStyle.Items.Add(Style);
  finally
    ComboBoxVCLStyle.Items.EndUpdate;
  end;
end;

procedure RegisterVCLStyles;
var
  Style   : string;
begin
  for Style in TDirectory.GetFiles(ExtractFilePath(ParamStr(0))+'\Styles', '*.vsf') do
    RegisterVCLStyle(Style);
end;


initialization
 RegisterVCLStyles;


end.
