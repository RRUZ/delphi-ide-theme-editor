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
    FChangeIconsGutter: boolean;
    FDockImages: string;
    FDockGradientHor: boolean;
    FDockCustom: boolean;
    FDockCustomColors: boolean;
    FDockStartGradActive: string;
    FDockStartGradInActive: string;
    FDockEndGradActive: string;
    FDockEndGradInActive: string;
    FHookSystemColors: boolean;
//    FStyleBarName: string;
//    FColorMapName: string;
  public
    property EnableDWMColorization : boolean read FEnableDWMColorization write FEnableDWMColorization;
    property Enabled : boolean read FEnabled write FEnabled;
    property ThemeName : string read FThemeName write FThemeName;
    property FixIDEDisabledIconsDraw : boolean read FFixIDEDisabledIconsDraw write FFixIDEDisabledIconsDraw;
    property AutogenerateColors : boolean read FAutogenerateColors write FAutogenerateColors;

    property UseVCLStyles  : boolean read FUseVCLStyles write FUseVCLStyles;
    property VCLStyleName  : string read FVCLStyleName write FVCLStyleName;

    property HookSystemColors : boolean read FHookSystemColors write FHookSystemColors;

    property DockImages    : string read FDockImages write FDockImages;
    property DockGradientHor    : boolean read FDockGradientHor write FDockGradientHor;
    property DockCustom         : boolean read FDockCustom write FDockCustom;
    property DockCustomColors   : boolean read FDockCustomColors write FDockCustomColors;
    property DockStartGradActive  : string read FDockStartGradActive write FDockStartGradActive;
    property DockEndGradActive    : string read FDockEndGradActive write FDockEndGradActive;
    property DockStartGradInActive  : string read FDockStartGradInActive write FDockStartGradInActive;
    property DockEndGradInActive  : string read FDockEndGradInActive write FDockEndGradInActive;

    property ChangeIconsGutter  : boolean read FChangeIconsGutter write FChangeIconsGutter;
//    property ColorMapName  : string read FColorMapName write FColorMapName;
//    property StyleBarName  : string read FStyleBarName write FStyleBarName;
  end;

  procedure ReadSettings(Settings: TSettings;Const Path:String);
  procedure WriteSettings(Settings: TSettings;Const Path:String);

implementation


uses
  SysUtils,
  IniFiles;


procedure ReadSettings(Settings: TSettings;Const Path:String);
var
  LIniFile: TIniFile;
begin
  LIniFile := TIniFile.Create(IncludeTrailingPathDelimiter(Path) + 'Settings.ini');
  try
    Settings.EnableDWMColorization   := LIniFile.ReadBool('Global', 'EnableDWMColorization', True);
    Settings.Enabled                 := LIniFile.ReadBool('Global', 'Enabled', True);
    Settings.FixIDEDisabledIconsDraw := LIniFile.ReadBool('Global', 'FixIDEDisabledIconsDraw', True);
    Settings.AutogenerateColors      := LIniFile.ReadBool('Global', 'AutogenerateColors', True);
    Settings.ThemeName               := LIniFile.ReadString('Global', 'ThemeName', '');
    Settings.VCLStyleName            := LIniFile.ReadString('Global', 'VCLStyleName', 'Carbon.vsf');
    Settings.UseVCLStyles            := LIniFile.ReadBool('Global', 'UseVCLStyles', False);
    Settings.ChangeIconsGutter       := LIniFile.ReadBool('Global', 'ChangeIconsGutter', True);
    Settings.HookSystemColors        := LIniFile.ReadBool('Global', 'HookSystemColors', True);
    Settings.DockImages              := LIniFile.ReadString('Global', 'DockImages', 'red');
    Settings.DockGradientHor         := LIniFile.ReadBool('Global', 'DockGradientHor', True);
    Settings.DockCustom              := LIniFile.ReadBool('Global', 'DockCustom', True);
    Settings.DockCustomColors        := LIniFile.ReadBool('Global', 'DockCustomColors', False);
    Settings.DockStartGradActive     := LIniFile.ReadString('Global', 'DockStartGradActive', 'clBlack');
    Settings.DockEndGradActive       := LIniFile.ReadString('Global', 'DockEndGradActive', 'clBlack');
    Settings.DockStartGradInActive   := LIniFile.ReadString('Global', 'DockStartGradInActive', 'clBlack');
    Settings.DockEndGradInActive     := LIniFile.ReadString('Global', 'DockEndGradInActive', 'clBlack');
    //    Settings.ColorMapName            := iniFile.ReadString('Global', 'ColorMapName', 'TXPColorMap');
//    Settings.StyleBarName            := iniFile.ReadString('Global', 'StyleBarName', 'XP Style');
  finally
    LIniFile.Free;
  end;
end;

procedure WriteSettings(Settings: TSettings;Const Path:String);
var
  LIniFile: TIniFile;
begin
  LIniFile := TIniFile.Create(IncludeTrailingPathDelimiter(Path) + 'Settings.ini');
  try
    LIniFile.WriteBool('Global', 'EnableDWMColorization', Settings.EnableDWMColorization);
    LIniFile.WriteBool('Global', 'Enabled', Settings.Enabled);
    LIniFile.WriteBool('Global', 'FixIDEDisabledIconsDraw', Settings.FixIDEDisabledIconsDraw);
    LIniFile.WriteBool('Global', 'AutogenerateColors', Settings.AutogenerateColors);
    LIniFile.WriteString('Global', 'ThemeName', Settings.ThemeName);
    LIniFile.WriteString('Global', 'VCLStyleName', Settings.VCLStyleName);
    LIniFile.WriteBool('Global', 'UseVCLStyles', Settings.UseVCLStyles);
    LIniFile.WriteBool('Global', 'ChangeIconsGutter', Settings.ChangeIconsGutter);
    LIniFile.WriteBool('Global', 'HookSystemColors', Settings.HookSystemColors);
    LIniFile.WriteString('Global', 'DockImages', Settings.DockImages);
    LIniFile.WriteBool('Global', 'DockGradientHor', Settings.DockGradientHor);
    LIniFile.WriteBool('Global', 'DockCustom', Settings.DockCustom);
    LIniFile.WriteBool('Global', 'DockCustomColors', Settings.DockCustomColors);
    LIniFile.WriteString('Global', 'DockStartGradActive', Settings.DockStartGradActive);
    LIniFile.WriteString('Global', 'DockEndGradActive', Settings.DockEndGradActive);
    LIniFile.WriteString('Global', 'DockStartGradInActive', Settings.DockStartGradInActive);
    LIniFile.WriteString('Global', 'DockEndGradInActive', Settings.DockEndGradInActive);
//    iniFile.WriteString('Global', 'ColorMapName', Settings.ColorMapName);
//    iniFile.WriteString('Global', 'StyleBarName', Settings.StyleBarName);
  finally
    LIniFile.Free;
  end;
end;

end.

