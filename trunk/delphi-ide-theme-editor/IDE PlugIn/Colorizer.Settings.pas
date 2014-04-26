//**************************************************************************************************
//
// Unit Colorizer.Settings
// unit Colorizer.Settings  for the Delphi IDE Colorizer
//
// The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
// you may not use this file except in compliance with the License. You may obtain a copy of the
// License at http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
// ANY KIND, either express or implied. See the License for the specific language governing rights
// and limitations under the License.
//
// The Original Code is Colorizer.Settings.pas.
//
// The Initial Developer of the Original Code is Rodrigo Ruz V.
// Portions created by Rodrigo Ruz V. are Copyright (C) 2011-2014 Rodrigo Ruz V.
// All Rights Reserved.
//
//**************************************************************************************************aa

unit Colorizer.Settings;

interface

type
  TSettings=class
  private
    FEnableDWMColorization: boolean;
    FEnabled: boolean;
    FThemeName: string;
    FFixIDEDisabledIconsDraw: boolean;
    FAutogenerateColors: boolean;
    FVCLStyleName: string;
    FUseVCLStyles: boolean;
    FVCLStylesPath: string;
    FChangeIconsGutter: boolean;
  public
    property EnableDWMColorization : boolean read FEnableDWMColorization write FEnableDWMColorization;
    property Enabled : boolean read FEnabled write FEnabled;
    property ThemeName : string read FThemeName write FThemeName;
    property FixIDEDisabledIconsDraw : boolean read FFixIDEDisabledIconsDraw write FFixIDEDisabledIconsDraw;
    property AutogenerateColors : boolean read FAutogenerateColors write FAutogenerateColors;

    property UseVCLStyles  : boolean read FUseVCLStyles write FUseVCLStyles;
    property VCLStyleName  : string read FVCLStyleName write FVCLStyleName;
    property VCLStylesPath : string read FVCLStylesPath write FVCLStylesPath;
    property ChangeIconsGutter  : boolean read FChangeIconsGutter write FChangeIconsGutter;
  end;

  procedure ReadSettings(Settings: TSettings;Const Path:String);
  procedure WriteSettings(Settings: TSettings;Const Path:String);

implementation


uses
  SysUtils,
  IniFiles;


procedure ReadSettings(Settings: TSettings;Const Path:String);
var
  iniFile: TIniFile;
begin
  //C:\Users\Public\Documents\RAD Studio\Projects\XE2\delphi-ide-theme-editor\IDE PlugIn\
  iniFile := TIniFile.Create(IncludeTrailingPathDelimiter(Path) + 'Settings.ini');
  try
    Settings.EnableDWMColorization   := iniFile.ReadBool('Global', 'EnableDWMColorization', True);
    Settings.Enabled                 := iniFile.ReadBool('Global', 'Enabled', True);
    Settings.FixIDEDisabledIconsDraw := iniFile.ReadBool('Global', 'FixIDEDisabledIconsDraw', True);
    Settings.AutogenerateColors      := iniFile.ReadBool('Global', 'AutogenerateColors', True);
    Settings.ThemeName               := iniFile.ReadString('Global', 'ThemeName', '');
    Settings.VCLStyleName            := iniFile.ReadString('Global', 'VCLStyleName', 'Carbon.vsf');
    Settings.UseVCLStyles            := iniFile.ReadBool('Global', 'UseVCLStyles', False);
    Settings.ChangeIconsGutter       := iniFile.ReadBool('Global', 'ChangeIconsGutter', True);
    Settings.VCLStylesPath           := iniFile.ReadString('Global', 'VCLStylesPath', '');
  finally
    iniFile.Free;
  end;
end;

procedure WriteSettings(Settings: TSettings;Const Path:String);
var
  iniFile: TIniFile;
begin
  iniFile := TIniFile.Create(IncludeTrailingPathDelimiter(Path) + 'Settings.ini');
  try
    iniFile.WriteBool('Global', 'EnableDWMColorization', Settings.EnableDWMColorization);
    iniFile.WriteBool('Global', 'Enabled', Settings.Enabled);
    iniFile.WriteBool('Global', 'FixIDEDisabledIconsDraw', Settings.FixIDEDisabledIconsDraw);
    iniFile.WriteBool('Global', 'AutogenerateColors', Settings.AutogenerateColors);
    iniFile.WriteString('Global', 'ThemeName', Settings.ThemeName);
    iniFile.WriteString('Global', 'VCLStyleName', Settings.VCLStyleName);
    iniFile.WriteBool('Global', 'UseVCLStyles', Settings.UseVCLStyles);
    iniFile.WriteBool('Global', 'ChangeIconsGutter', Settings.ChangeIconsGutter);
    iniFile.WriteString('Global', 'VCLStylesPath', Settings.VCLStylesPath);
  finally
    iniFile.Free;
  end;
end;

end.

