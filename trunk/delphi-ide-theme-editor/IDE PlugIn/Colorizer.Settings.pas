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
{$I ..\Common\Jedi.inc}

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
    FToolbarEndGrad: string;
    FToolbarCustomColors: boolean;
    FToolbarStartGrad: string;
    FToolbarGradientHor: boolean;
    FCheckUpdates: boolean;
    FDockActiveFontColor: string;
    FDockInActiveFontColor: string;
    FDockBorderRounded: boolean;
    FDockActiveBorderColor: string;
    FDockInActiveBorderColor: string;
    FTabIDEEndGradInActive: string;
    FTabIDEStartGradActive: string;
    FTabIDEStartGradInActive: string;
    FTabIDEEndGradActive: string;
    FTabIDEOutLine: boolean;
    FTabIDECustom: boolean;
    FTabIDEActiveFontColor: string;
    FTabIDEOutLineColor: string;
    FHeaderEndGrad: string;
    FHeaderFontColor: string;
    FHeaderBorderColor: string;
    FHeaderCustom: boolean;
    FHeaderStartGrad: string;
    FVCLStylesForms: boolean;
    function GetThemeFileName: string;
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
    property VCLStylesForms  : boolean read FVCLStylesForms write FVCLStylesForms;

    property HookSystemColors : boolean read FHookSystemColors write FHookSystemColors;

    property DockImages    : string read FDockImages write FDockImages;
    property DockGradientHor    : boolean read FDockGradientHor write FDockGradientHor;
    property DockCustom         : boolean read FDockCustom write FDockCustom;
    property DockCustomColors   : boolean read FDockCustomColors write FDockCustomColors;
    property DockStartGradActive  : string read FDockStartGradActive write FDockStartGradActive;
    property DockEndGradActive    : string read FDockEndGradActive write FDockEndGradActive;
    property DockStartGradInActive  : string read FDockStartGradInActive write FDockStartGradInActive;
    property DockEndGradInActive  : string read FDockEndGradInActive write FDockEndGradInActive;
    property DockActiveFontColor  : string read FDockActiveFontColor write FDockActiveFontColor;
    property DockInActiveFontColor  : string read FDockInActiveFontColor write FDockInActiveFontColor;

    property DockActiveBorderColor  : string read FDockActiveBorderColor write FDockActiveBorderColor;
    property DockInActiveBorderColor  : string read FDockInActiveBorderColor write FDockInActiveBorderColor;
    property DockBorderRounded    : boolean read FDockBorderRounded write FDockBorderRounded;


    property ChangeIconsGutter  : boolean read FChangeIconsGutter write FChangeIconsGutter;
//    property ColorMapName  : string read FColorMapName write FColorMapName;
//    property StyleBarName  : string read FStyleBarName write FStyleBarName;
    property ToolbarGradientHor    : boolean read FToolbarGradientHor write FToolbarGradientHor;
    property ToolbarCustomColors   : boolean read FToolbarCustomColors write FToolbarCustomColors;
    property ToolbarStartGrad  : string read FToolbarStartGrad write FToolbarStartGrad;
    property ToolbarEndGrad    : string read FToolbarEndGrad write FToolbarEndGrad;

    property TabIDEStartGradActive   : string  read FTabIDEStartGradActive write FTabIDEStartGradActive;
    property TabIDEEndGradActive     : string  read FTabIDEEndGradActive write FTabIDEEndGradActive;
    property TabIDEStartGradInActive : string  read FTabIDEStartGradInActive write FTabIDEStartGradInActive;
    property TabIDEEndGradInActive   : string  read FTabIDEEndGradInActive write FTabIDEEndGradInActive;
    property TabIDEActiveFontColor   : string read FTabIDEActiveFontColor write FTabIDEActiveFontColor;
    property TabIDEOutLineColor      : string read FTabIDEOutLineColor write FTabIDEOutLineColor;
    property TabIDEOutLine           : boolean read FTabIDEOutLine write FTabIDEOutLine;
    property TabIDECustom            : boolean read FTabIDECustom write FTabIDECustom;

    property HeaderStartGrad   : string  read FHeaderStartGrad write FHeaderStartGrad;
    property HeaderEndGrad     : string  read FHeaderEndGrad write FHeaderEndGrad;
    property HeaderFontColor   : string  read FHeaderFontColor write FHeaderFontColor;
    property HeaderBorderColor : string  read FHeaderBorderColor write FHeaderBorderColor;
    property HeaderCustom      : boolean read FHeaderCustom write FHeaderCustom;

    property CheckUpdates    : boolean read FCheckUpdates write FCheckUpdates;

    property ThemeFileName   : string read GetThemeFileName;
  end;

  procedure ReadSettings(Settings: TSettings;Const Path:String);
  procedure WriteSettings(Settings: TSettings;Const Path:String);

implementation


uses
  uMisc,
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
    Settings.VCLStylesForms          := LIniFile.ReadBool('Global', 'VCLStylesForms', True);


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
    Settings.DockActiveFontColor     := LIniFile.ReadString('Global', 'DockActiveFontColor', 'clWhite');
    Settings.DockInActiveFontColor   := LIniFile.ReadString('Global', 'DockInActiveFontColor', 'clWhite');

    Settings.DockActiveBorderColor     := LIniFile.ReadString('Global', 'DockActiveBorderColor', 'clBlack');
    Settings.DockInActiveBorderColor   := LIniFile.ReadString('Global', 'DockInActiveBorderColor', 'clBlack');
    Settings.DockBorderRounded         := LIniFile.ReadBool('Global', 'DockBorderRounded', True);

    //    Settings.ColorMapName            := iniFile.ReadString('Global', 'ColorMapName', 'TXPColorMap');
//    Settings.StyleBarName            := iniFile.ReadString('Global', 'StyleBarName', 'XP Style');
    Settings.ToolbarGradientHor   := LIniFile.ReadBool('Global', 'ToolbarGradientHor', True);
    Settings.ToolbarCustomColors  := LIniFile.ReadBool('Global', 'ToolbarCustomColors', False);
    Settings.ToolbarStartGrad     := LIniFile.ReadString('Global', 'ToolbarStartGrad', 'clGray');
    Settings.ToolbarEndGrad       := LIniFile.ReadString('Global', 'ToolbarEndGrad', 'clSilver');


    Settings.TabIDECustom            := LIniFile.ReadBool('Global', 'TabIDECustom', False);
    Settings.TabIDEOutLine           := LIniFile.ReadBool('Global', 'TabIDEOutLine', True);
    Settings.TabIDEStartGradActive   := LIniFile.ReadString('Global', 'TabIDEStartGradActive', 'clBlack');
    Settings.TabIDEEndGradActive     := LIniFile.ReadString('Global', 'TabIDEEndGradActive', 'clBlack');
    Settings.TabIDEStartGradInActive := LIniFile.ReadString('Global', 'TabIDEStartGradInActive', 'clBlack');
    Settings.TabIDEEndGradInActive   := LIniFile.ReadString('Global', 'TabIDEEndGradInActive', 'clBlack');
    Settings.TabIDEActiveFontColor   := LIniFile.ReadString('Global', 'TabIDEActiveFontColor', 'clWhite');
    Settings.TabIDEOutLineColor      := LIniFile.ReadString('Global', 'TabIDEOutLineColor', 'clBlack');

    Settings.HeaderCustom            := LIniFile.ReadBool('Global', 'HeaderCustom', False);
    Settings.HeaderStartGrad         := LIniFile.ReadString('Global', 'HeaderStartGrad', 'clSilver');
    Settings.HeaderEndGrad           := LIniFile.ReadString('Global', 'HeaderEndGrad', 'clSilver');
    Settings.HeaderFontColor         := LIniFile.ReadString('Global', 'HeaderFontColor', 'clBlack');
    Settings.HeaderBorderColor       := LIniFile.ReadString('Global', 'HeaderBorderColor', 'clBlack');

    Settings.CheckUpdates         := LIniFile.ReadBool('Global', 'CheckUpdates', True);
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
    LIniFile.WriteBool('Global', 'VCLStylesForms', Settings.VCLStylesForms);

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
    LIniFile.WriteString('Global', 'DockActiveFontColor', Settings.DockActiveFontColor);
    LIniFile.WriteString('Global', 'DockInActiveFontColor', Settings.DockInActiveFontColor);
    LIniFile.WriteString('Global', 'DockActiveBorderColor', Settings.DockActiveBorderColor);
    LIniFile.WriteString('Global', 'DockInActiveBorderColor', Settings.DockInActiveBorderColor);
    LIniFile.WriteBool('Global', 'DockBorderRounded', Settings.DockBorderRounded);

//    iniFile.WriteString('Global', 'ColorMapName', Settings.ColorMapName);
//    iniFile.WriteString('Global', 'StyleBarName', Settings.StyleBarName);
    LIniFile.WriteBool('Global', 'ToolbarGradientHor', Settings.ToolbarGradientHor);
    LIniFile.WriteBool('Global', 'ToolbarCustomColors', Settings.ToolbarCustomColors);
    LIniFile.WriteString('Global', 'ToolbarStartGrad', Settings.ToolbarStartGrad);
    LIniFile.WriteString('Global', 'ToolbarEndGrad', Settings.ToolbarEndGrad);

    LIniFile.WriteBool('Global', 'TabIDECustom', Settings.TabIDECustom);
    LIniFile.WriteBool('Global', 'TabIDEOutLine', Settings.TabIDEOutLine);
    LIniFile.WriteString('Global', 'TabIDEStartGradActive', Settings.TabIDEStartGradActive);
    LIniFile.WriteString('Global', 'TabIDEEndGradActive', Settings.TabIDEEndGradActive);
    LIniFile.WriteString('Global', 'TabIDEStartGradInActive', Settings.TabIDEStartGradInActive);
    LIniFile.WriteString('Global', 'TabIDEEndGradInActive', Settings.TabIDEEndGradInActive);
    LIniFile.WriteString('Global', 'TabIDEActiveFontColor', Settings.TabIDEActiveFontColor);
    LIniFile.WriteString('Global', 'TabIDEOutLineColor', Settings.TabIDEOutLineColor);

    LIniFile.WriteBool('Global', 'HeaderCustom', Settings.HeaderCustom);
    LIniFile.WriteString('Global', 'HeaderStartGrad', Settings.HeaderStartGrad);
    LIniFile.WriteString('Global', 'HeaderEndGrad', Settings.HeaderEndGrad);
    LIniFile.WriteString('Global', 'HeaderFontColor', Settings.HeaderFontColor);
    LIniFile.WriteString('Global', 'HeaderBorderColor', Settings.HeaderBorderColor);

    LIniFile.WriteBool('Global', 'CheckUpdates', Settings.CheckUpdates);
  finally
    LIniFile.Free;
  end;
end;

{ TSettings }

function TSettings.GetThemeFileName: string;
begin
 Result:=IncludeTrailingPathDelimiter(ExtractFilePath(GetModuleLocation()))+'Themes\'+FThemeName+'.idetheme';
end;

end.

