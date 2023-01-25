// **************************************************************************************************
//
// Unit DITE.DelphiVersions
// unit for the Delphi IDE Theme Editor
//
// The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
// you may not use this file except in compliance with the License. You may obtain a copy of the
// License at http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
// ANY KIND, either express or implied. See the License for the specific language governing rights
// and limitations under the License.
//
// The Original Code is uDelphiVersions.pas.
//
// The Initial Developer of the Original Code is Rodrigo Ruz V.
// Portions created by Rodrigo Ruz V. are Copyright (C) 2011-2023 Rodrigo Ruz V.
// All Rights Reserved.
//
// **************************************************************************************************

unit DITE.DelphiVersions;

interface

uses
  Generics.Defaults,
  Generics.Collections,
  DITE.SupportedIDEs,
  Vcl.Graphics,
  SysUtils,
  Classes,
  ComCtrls;

{$I Common.inc}

type
  TDelphiVersions = (
    {$IFDEF DELPHI_OLDER_VERSIONS_SUPPORT}
    Delphi5, Delphi6,
    {$ENDIF}
    Delphi7, Delphi8, Delphi2005, Delphi2006, Delphi2007, Delphi2009, Delphi2010,
    DelphiXE, DelphiXE2, DelphiXE3, DelphiXE4, DelphiXE5, Appmethod, DelphiXE6,
    DelphiXE7, DelphiXE8, Delphi10Seattle, Delphi10Berlin, Delphi10Tokyo, Delphi10Rio,
    Delphi10Sydney, Delphi11Alexandria);

  TDelphiVersionData = Class
  private
    FVersion: TDelphiVersions;
    FName: string;
    FPath: string;
    FIcon: TIcon;
    FIDEType: TSupportedIDEs;
    FRegKey: string;
  public
    property Version: TDelphiVersions read FVersion write FVersion;
    property Path: string read FPath write FPath;
    property Name: string read FName write FName;
    property Icon: TIcon read FIcon write FIcon;
    property RegKey: string read FRegKey;
    property IDEType: TSupportedIDEs read FIDEType write FIDEType;
    constructor Create;
    destructor Destroy; override;
  end;

const
{$IFDEF DELPHI_OLDER_VERSIONS_SUPPORT}
  DelphiOldVersions = 2;
  DelphiOldVersionNumbers: array [0 .. DelphiOldVersions - 1] of TDelphiVersions = (Delphi5, Delphi6);

  DelphiOldColorsCount = 16;

  { BGR
    Color0=$000000
    Color1=$000080
    Color2=$008000
    Color3=$008080
    Color4=$800000
    Color5=$800080
    Color6=$808000
    Color7=$C0C0C0
    Color8=$808080
    Color9=$0000FF
    Color10=$00FF00
    Color11=$00FFFF
    Color12=$FF0000
    Color13=$FF00FF
    Color14=$FFFF00
    Color15=$FFFFFF
  }
  DelphiOldColorsList: array [0 .. DelphiOldColorsCount - 1] of TColor =
  ($000000, $000080, $008000, $008080, $800000,
   $800080, $808000, $C0C0C0, $808080, $0000FF,
   $00FF00, $00FFFF, $FF0000, $FF00FF, $FFFF00, $FFFFFF);
{$ENDIF}
  DelphiVersionsNames: array [TDelphiVersions] of string = (
{$IFDEF DELPHI_OLDER_VERSIONS_SUPPORT}
    'Delphi 5', 'Delphi 6',
{$ENDIF}
    'Delphi 7', 'Delphi 8', 'BDS 2005', 'BDS 2006', 'RAD Studio 2007',
    'RAD Studio 2009', 'RAD Studio 2010', 'RAD Studio XE', 'RAD Studio XE2',
    'RAD Studio XE3', 'RAD Studio XE4', 'RAD Studio XE5', 'Appmethod 1.13',
    'RAD Studio XE6/Appmethod 1.14', 'RAD Studio XE7/Appmethod 1.15',
    'RAD Studio XE8', 'RAD Studio 10 Seattle', 'RAD Studio 10.1 Berlin',
    'RAD Studio 10.2 Tokyo', 'RAD Studio 10.3 Rio', 'RAD Studio 10.4 Sydney',
    'RAD Studio 11.0 Alexandria');

  DelphiVersionNumbers: array [TDelphiVersions] of double = (
{$IFDEF DELPHI_OLDER_VERSIONS_SUPPORT}
    13, // 'Delphi 5',
    14, // 'Delphi 6',
{$ENDIF}
    15, // 'Delphi 7',
    16, // 'Delphi 8',
    17, // 'BDS 2005',
    18, // 'BDS 2006',
    18.5, // 'RAD Studio 2007',
    20, // 'RAD Studio 2009',
    21, // 'RAD Studio 2010',
    22, // 'RAD Studio XE'
    23, // 'RAD Studio XE2'
    24, // 'RAD Studio XE3'
    25, // 'RAD Studio XE4'
    26, // 'RAD Studio XE5'
    27, // 'Appmethod 1.13'
    27, // 'RAD Studio XE6'
    28, // 'RAD Studio XE7'
    29, // 'RAD Studio XE8'
    30, // 'RAD Studio 10 Seattle'
    31, // 'RAD Studio 10.1 Berlin'
    32, // 'RAD Studio 10.2 Tokyo'
    33, // 'RAD Studio 10.3 Rio'
    34, // 'RAD Studio 10.4 Sydney'
    35  // 'RAD Studio 11.0 Alexandria'
    );

  DelphiVCLStylesPaths: array [TDelphiVersions] of string = (
{$IFDEF DELPHI_OLDER_VERSIONS_SUPPORT}
    '', '',
{$ENDIF}
    '', '', '', '', '', '', '', '',
    'RAD Studio\9.0\Styles',
    'RAD Studio\10.0\Styles',
    'RAD Studio\11.0\Styles',
    'RAD Studio\12.0\Styles',
    '',
    'Embarcadero\Studio\14.0\Styles',
    'Embarcadero\Studio\15.0\Styles',
    'Embarcadero\Studio\16.0\Styles',
    'Embarcadero\Studio\17.0\Styles',
    'Embarcadero\Studio\18.0\Styles',
    'Embarcadero\Studio\19.0\Styles',
    'Embarcadero\Studio\20.0\Styles',
    'Embarcadero\Studio\21.0\Styles',
    'Embarcadero\Studio\22.0\Styles'
    );

procedure FillCurrentDelphiVersion(Data: TDelphiVersionData);
procedure FillListDelphiVersions(AList: TList<TDelphiVersionData>);
{$IFDEF DELPHI_OLDER_VERSIONS_SUPPORT}
function DelphiIsOldVersion(ADelphiVersionData: TDelphiVersionData): Boolean;
function GetIndexClosestColor(AColor: TColor): Integer;
{$ENDIF}
function GetDelphiVersionMappedColor(AColor: TColor; ADelphiVersionData: TDelphiVersionData): TColor;
function GetVCLStylesFolder(DelphiVersion: TDelphiVersions): string;

{

  [HKEY_CURRENT_USER\Software\Embarcadero\BDS\8.0\Editor\Highlight\Attribute Names]
  "Bold"="False"
  "Italic"="False"
  "Underline"="False"
  "Default Foreground"="False"
  "Default Background"="False"
  "Foreground Color New"="$00DE4841"
  "Background Color New"="$00272727"
}

implementation

uses
  DITE.Misc,
  PsAPI,
  Controls,
  ImgList,
  CommCtrl,
  ShellAPI,
  ShlObj,
  Windows,
  DITE.Registry,
  Registry;

type
  TDelphiCmpnyName = (OldBorland, Borland, CodeGear, Embarcadero);

const
  DelphiRegPaths: array [TDelphiVersions] of string = (
{$IFDEF DELPHI_OLDER_VERSIONS_SUPPORT}
    '\Software\Borland\Delphi\5.0',
    '\Software\Borland\Delphi\6.0',
{$ENDIF}
    '\Software\Borland\Delphi\7.0',
    '\Software\Borland\BDS\2.0',
    '\Software\Borland\BDS\3.0',
    '\Software\Borland\BDS\4.0',
    '\Software\Borland\BDS\5.0',
    '\Software\CodeGear\BDS\6.0',
    '\Software\CodeGear\BDS\7.0',
    '\Software\Embarcadero\BDS\8.0',
    '\Software\Embarcadero\BDS\9.0',
    '\Software\Embarcadero\BDS\10.0',
    '\Software\Embarcadero\BDS\11.0',
    '\Software\Embarcadero\BDS\12.0',
    '\Software\Embarcadero\BDS\13.0',
    '\Software\Embarcadero\BDS\14.0',
    '\Software\Embarcadero\BDS\15.0',
    '\Software\Embarcadero\BDS\16.0',
    '\Software\Embarcadero\BDS\17.0',
    '\Software\Embarcadero\BDS\18.0',
    '\Software\Embarcadero\BDS\19.0',
    '\Software\Embarcadero\BDS\20.0',
    '\Software\Embarcadero\BDS\21.0',
    '\Software\Embarcadero\BDS\22.0'
    );

  DelphiCustomRegPaths: array [TDelphiVersions] of string = (
{$IFDEF DELPHI_OLDER_VERSIONS_SUPPORT}
    '\Software\Borland\%s\5.0', // Delphi
    '\Software\Borland\%s\6.0', // Delphi
{$ENDIF}
    '\Software\Borland\%s\7.0', // Delphi
    '\Software\Borland\%s\2.0', // BDS
    '\Software\Borland\%s\3.0', // BDS
    '\Software\Borland\%s\4.0', // BDS
    '\Software\Borland\%s\5.0', // BDS
    '\Software\CodeGear\%s\6.0', // BDS
    '\Software\CodeGear\%s\7.0', // BDS
    '\Software\Embarcadero\%s\8.0', // BDS
    '\Software\Embarcadero\%s\9.0', // BDS
    '\Software\Embarcadero\%s\10.0', // BDS
    '\Software\Embarcadero\%s\11.0', // BDS
    '\Software\Embarcadero\%s\12.0', // BDS
    '\Software\Embarcadero\%s\13.0', // BDS
    '\Software\Embarcadero\%s\14.0', // BDS
    '\Software\Embarcadero\%s\15.0', // BDS
    '\Software\Embarcadero\%s\16.0', // BDS
    '\Software\Embarcadero\%s\17.0', // BDS
    '\Software\Embarcadero\%s\18.0', // BDS
    '\Software\Embarcadero\%s\19.0', // BDS
    '\Software\Embarcadero\%s\20.0', // BDS
    '\Software\Embarcadero\%s\21.0', // BDS
    '\Software\Embarcadero\%s\22.0'  // BDS
    );

  DelphiRegPathNumbers: array [TDelphiVersions] of Integer = (
{$IFDEF DELPHI_OLDER_VERSIONS_SUPPORT}
    5, // 'Delphi 5',
    6, // 'Delphi 6',
{$ENDIF}
    7, // 'Delphi 7',
    2, // 'Delphi 8',
    3, // 'BDS 2005',
    4, // 'BDS 2006',
    5, // 'RAD Studio 2007',

    6, // 'RAD Studio 2009',
    7, // 'RAD Studio 2010',
    8, // 'RAD Studio XE'
    9, // 'RAD Studio XE2'
    10, // 'RAD Studio XE3'
    11, // 'RAD Studio XE4'
    12, // 'RAD Studio XE5'
    13, // 'Appmethod 1.13'
    14, // 'RAD Studio XE6'
    15, // 'RAD Studio XE7'
    16, // 'RAD Studio XE8'
    17, // 'RAD Studio 10 Seattle'
    18, // 'RAD Studio 10.1 Berlin'
    19, // 'RAD Studio 10.2 Tokyo'
    20, // 'RAD Studio 10.3 Rio'
    21, // 'RAD Studio 10.4 Sydney'
    22  // 'RAD Studio 11.0 Alexandria'
    );

  DelphiCmpnyNames: array [TDelphiVersions] of TDelphiCmpnyName = (
{$IFDEF DELPHI_OLDER_VERSIONS_SUPPORT}
    OldBorland, // 'Delphi 5',
    OldBorland, // 'Delphi 6',
{$ENDIF}
    OldBorland, // 'Delphi 7',
    Borland, // 'Delphi 8',
    Borland, // 'BDS 2005',
    Borland, // 'BDS 2006',
    Borland, // 'RAD Studio 2007', (stored under Borland)
    CodeGear, // 'RAD Studio 2009',
    CodeGear, // 'RAD Studio 2010',
    Embarcadero, // 'RAD Studio XE'
    Embarcadero, // 'RAD Studio XE2'
    Embarcadero, // 'RAD Studio XE3'
    Embarcadero, // 'RAD Studio XE4'
    Embarcadero, // 'RAD Studio XE5'
    Embarcadero, // 'Appmethod 1.13'
    Embarcadero, // 'RAD Studio XE6'
    Embarcadero, // 'RAD Studio XE7'
    Embarcadero, // 'RAD Studio XE8'
    Embarcadero, // 'RAD Studio 10 Seattle
    Embarcadero, // 'RAD Studio 10.1 Berlin
    Embarcadero, // 'RAD Studio 10.2 Tokyo
    Embarcadero, // 'RAD Studio 10.3 Rio
    Embarcadero, // 'RAD Studio 10.4 Sydney
    Embarcadero  // 'RAD Studio 11 Alexandria
    );

{$IFDEF DELPHI_OLDER_VERSIONS_SUPPORT}

function DelphiIsOldVersion(ADelphiVersionData: TDelphiVersionData): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to (DelphiOldVersions - 1) do
    if ADelphiVersionData.Version = DelphiOldVersionNumbers[i] then
      Exit(True);
end;

function GetIndexClosestColor(AColor: TColor): Integer;
var
  SqrDist, SmallSqrDist: double;
  i, R1, G1, B1, R2, G2, B2: Integer;
begin
  Result := 0;
  SmallSqrDist := Sqrt(SQR(255) * 3);
  R1 := GetRValue(AColor);
  G1 := GetGValue(AColor);
  B1 := GetBValue(AColor);

  for i := 0 to DelphiOldColorsCount - 1 do
  begin
    R2 := GetRValue(DelphiOldColorsList[i]);
    G2 := GetGValue(DelphiOldColorsList[i]);
    B2 := GetBValue(DelphiOldColorsList[i]);
    SqrDist := Sqrt(SQR(R1 - R2) + SQR(G1 - G2) + SQR(B1 - B2));
    if SqrDist < SmallSqrDist then
    begin
      Result := i;
      SmallSqrDist := SqrDist;
    end
  end
end;

{$ENDIF}

function GetDelphiVersionMappedColor(AColor: TColor; ADelphiVersionData: TDelphiVersionData): TColor;
begin
  Result := AColor;
{$IFDEF DELPHI_OLDER_VERSIONS_SUPPORT}
  if DelphiIsOldVersion(ADelphiVersionData) then
    Result := DelphiOldColorsList[GetIndexClosestColor(AColor)];
{$ENDIF}
end;

// C:\Users\Public\Documents\Embarcadero\Studio\16.0\Styles
function GetVCLStylesFolder(DelphiVersion: TDelphiVersions): string;
var
  List: TObjectList<TDelphiVersionData>;
  LData: TDelphiVersionData;
begin
  Result := '';
  List := TObjectList<TDelphiVersionData>.Create;
  try
    FillListDelphiVersions(List);
    for LData in List do
      if LData.Version = DelphiVersion then
      begin
        Result := IncludeTrailingPathDelimiter(GetSpecialFolder(CSIDL_COMMON_DOCUMENTS)) + DelphiVCLStylesPaths
          [DelphiVersion];
        if not DirectoryExists(Result) then
          Result := '';
        break;
      end;
  finally
    List.free;
  end;
end;

procedure FillCurrentDelphiVersion(Data: TDelphiVersionData);
var
  List: TObjectList<TDelphiVersionData>;
  LData: TDelphiVersionData;
  s: string;
begin
  s := ParamStr(0);
  List := TObjectList<TDelphiVersionData>.Create;
  try
    FillListDelphiVersions(List);
    for LData in List do
      if SameText(LData.Path, s) then
      begin
        Data.FVersion := LData.Version;
        Data.Path := LData.Path;
        Data.Name := LData.Name;
        if (Data.Icon = nil) then
          Data.Icon := TIcon.Create;
        Data.Icon.Assign(LData.Icon);
        Data.IDEType := LData.IDEType;
        Data.FRegKey := LData.FRegKey;
        break;
      end;
  finally
    List.free;
  end;
end;

procedure ScanRootKey(const AKey: string; AList: TStrings; AMin, AMax: Integer);
var
  s: string;
  RootKey: HKEY;
  LList: TStrings;

  procedure GetItems();
  var
    LRegistry: TRegistry;
  begin
    LRegistry := TRegistry.Create;
    try
      LRegistry.RootKey := RootKey;
      if LRegistry.OpenKeyReadOnly(AKey) then
        LRegistry.GetKeyNames(LList);
    finally
      LRegistry.free;
    end;
  end;

  function IsValidKey(const ASubKey: string): Boolean;
  var
    LVersion: Integer;
    FullKey, FileName: string;
  begin
    Result := False;
    for LVersion := AMin to AMax do
    begin
      FullKey := Format('%s\%s\%d.0', [AKey, ASubKey, LVersion]);
      if RegKeyExists(FullKey, RootKey) then
      begin
        Result := RegReadStr(FullKey, 'App', FileName, RootKey) and FileExists(FileName);
        if Result then
          break;
      end;
    end;
  end;

begin
  RootKey := HKEY_CURRENT_USER;
  LList := TStringList.Create;
  try
    AList.Clear;
    GetItems();
    if LList.Count > 0 then
      for s in LList do
        if { (s <> 'BDS') and } IsValidKey(s) then
          AList.Add(s);
  finally
    LList.free;
  end;
end;

procedure FillListDelphiVersions(AList: TList<TDelphiVersionData>);
type
  TBDSKeysItem = record
    MinValue, MaxValue: Integer;
    Company: TDelphiCmpnyName;
    Key: string;
  end;
const
  MaxBDSKeysItem = 4;

Var
  VersionData: TDelphiVersionData;
  DelphiComp: TDelphiVersions;
  LKey, FileName: string;
  Found: Boolean;
  RootKey: HKEY;
  BDSKeys: TStrings;
  i, j: Integer;
  BDSKeysItems: Array [0 .. MaxBDSKeysItem - 1] of TBDSKeysItem;
begin
  BDSKeys := TStringList.Create;
  try
{$IFDEF DELPHI_OLDER_VERSIONS_SUPPORT}
    BDSKeysItems[0].MinValue := 5;
{$ELSE}
    BDSKeysItems[0].MinValue := 7;
{$ENDIF}
    BDSKeysItems[0].MaxValue := 7;
    BDSKeysItems[0].Company := OldBorland;
    BDSKeysItems[0].Key := '\Software\Borland';

    BDSKeysItems[1].MinValue := 2;
    BDSKeysItems[1].MaxValue := 5;
    BDSKeysItems[1].Company := Borland;
    BDSKeysItems[1].Key := '\Software\Borland';

    BDSKeysItems[2].MinValue := 6;
    BDSKeysItems[2].MaxValue := 7;
    BDSKeysItems[2].Company := CodeGear;
    BDSKeysItems[2].Key := '\Software\CodeGear';

    BDSKeysItems[3].MinValue := DelphiRegPathNumbers[DelphiXE];
    BDSKeysItems[3].MaxValue := DelphiRegPathNumbers[Delphi11Alexandria];
    BDSKeysItems[3].Company := Embarcadero;
    BDSKeysItems[3].Key := '\Software\Embarcadero';

    for j := 0 to MaxBDSKeysItem - 1 do
    begin
      BDSKeys.Clear;
      ScanRootKey(BDSKeysItems[j].Key, BDSKeys, BDSKeysItems[j].MinValue, BDSKeysItems[j].MaxValue);

      for i := 0 to BDSKeys.Count - 1 do
      begin
        for DelphiComp := Low(TDelphiVersions) to High(TDelphiVersions) do
          if (DelphiCmpnyNames[DelphiComp] = BDSKeysItems[j].Company) and
            (DelphiRegPathNumbers[DelphiComp] >= BDSKeysItems[j].MinValue) and
            (DelphiRegPathNumbers[DelphiComp] <= BDSKeysItems[j].MaxValue) then
          begin
            RootKey := HKEY_CURRENT_USER;
            // LKey    := DelphiRegPaths[DelphiComp];
            LKey := Format(DelphiCustomRegPaths[DelphiComp], [BDSKeys[i]]);
            Found := RegKeyExists(LKey, RootKey);

            FileName := '';

            if Found then
              Found := RegReadStr(LKey, 'App', FileName, RootKey) and FileExists(FileName);

            if (DelphiComp >= DelphiXE6) and not Found then
            begin
              FileName := StringReplace(FileName, 'bds.exe', 'appmethod.exe', [rfReplaceAll]);
              Found := FileExists(FileName);
            end;

            if not Found then
            begin
              RootKey := HKEY_LOCAL_MACHINE;
              Found := RegKeyExists(LKey, RootKey);
              if Found then
                Found := RegReadStr(LKey, 'App', FileName, RootKey) and FileExists(FileName);
            end;

            if Found then
            begin
              VersionData := TDelphiVersionData.Create;
              VersionData.FPath := FileName;
              VersionData.FRegKey := LKey;
              VersionData.FVersion := DelphiComp;
              VersionData.FName := DelphiVersionsNames[DelphiComp];
              if not SameText(BDSKeys[i], 'BDS') then
                VersionData.FName := DelphiVersionsNames[DelphiComp] + ' (' + BDSKeys[i] + ')';

              VersionData.FIDEType := TSupportedIDEs.DelphiIDE;
              VersionData.Icon := TIcon.Create;
              ExtractIconFile(VersionData.FIcon, FileName, SHGFI_SMALLICON);
              AList.Add(VersionData);
            end;
          end;
      end;
    end;
  finally
    BDSKeys.free;
  end;
end;

{ TDelphiVersionData }

constructor TDelphiVersionData.Create;
begin
  inherited;
  FIcon := nil;
end;

destructor TDelphiVersionData.Destroy;
begin
  FreeAndNil(FIcon);
  inherited;
end;

end.
