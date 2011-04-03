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
  Dialogs, StdCtrls, JvBaseDlg, JvBrowseFolder, ExtCtrls;

type
  TSettings = class
  private
    FThemePath: string;
  published
    property ThemePath: string Read FThemePath Write FThemePath;
  end;


  TFrmSettings = class(TForm)
    BtnSave:   TButton;
    Label1:    TLabel;
    EditThemesFolder: TEdit;
    BtnSelFolderThemes: TButton;
    BtnCancel: TButton;
    JvBrowseForFolderDialog1: TJvBrowseForFolderDialog;
    Bevel1:    TBevel;
    procedure BtnSelFolderThemesClick(Sender: TObject);
    procedure BtnSaveClick(Sender: TObject);
    procedure BtnCancelClick(Sender: TObject);
  private
    FSettings: TSettings;
  public
    property Settings: TSettings Read FSettings Write FSettings;
    procedure LoadSettings;
  end;

procedure ReadSettings(var Settings: TSettings);
procedure WriteSettings(const Settings: TSettings);

implementation

uses
  IOUtils,
  IniFiles;


{$R *.dfm}

procedure ReadSettings(var Settings: TSettings);
var
  iniFile: TIniFile;
begin
  iniFile := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'Settings.ini');
  try
    Settings.ThemePath := iniFile.ReadString('Global', 'ThemePath',
      ExtractFilePath(ParamStr(0)) + 'Themes');
    if not TDirectory.Exists(Settings.ThemePath) then
    begin
      Settings.ThemePath := ExtractFilePath(ParamStr(0)) + 'Themes';
      ForceDirectories(Settings.ThemePath);
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
  if Application.MessageBox(PChar(Format('Do you want save the changes ?%s', [''])), 'Confirmation', MB_YESNO + MB_ICONQUESTION) = idYes then
  begin
    FSettings.ThemePath := EditThemesFolder.Text;
    WriteSettings(FSettings);
    Close();
  end;
end;

procedure TFrmSettings.BtnSelFolderThemesClick(Sender: TObject);
begin
  if DirectoryExists(EditThemesFolder.Text) then
    JvBrowseForFolderDialog1.Directory := EditThemesFolder.Text;

  if JvBrowseForFolderDialog1.Execute then
    EditThemesFolder.Text := JvBrowseForFolderDialog1.Directory;
end;

procedure TFrmSettings.LoadSettings;
begin
  ReadSettings(FSettings);
  EditThemesFolder.Text := FSettings.ThemePath;
end;

end.
