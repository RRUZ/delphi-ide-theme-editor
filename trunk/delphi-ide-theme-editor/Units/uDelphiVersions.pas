//**************************************************************************************************
//
// Unit uDelphiVersions
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
// Portions created by Rodrigo Ruz V. are Copyright (C) 2011-2015 Rodrigo Ruz V.
// All Rights Reserved.
//
//**************************************************************************************************

unit uDelphiVersions;

interface

uses
  Generics.Defaults,
  Generics.Collections,
  uSupportedIDEs,
  Graphics,
  SysUtils,
  Classes,
  ComCtrls;

{$DEFINE DELPHI_OLDER_VERSIONS_SUPPORT}

type
  TDelphiVersions =
    (
  {$IFDEF DELPHI_OLDER_VERSIONS_SUPPORT}
    Delphi5,
    Delphi6,
  {$ENDIF}
    Delphi7,
    Delphi8,
    Delphi2005,
    Delphi2006,
    Delphi2007,
    Delphi2009,
    Delphi2010,
    DelphiXE,
    DelphiXE2,
    DelphiXE3,
    DelphiXE4,
    DelphiXE5,
    Appmethod,
    DelphiXE6,
    DelphiXE7,
    DelphiXE8
);

  TDelphiVersionData = Class
  private
    FVersion: TDelphiVersions;
    FName: string;
    FPath: string;
    FIcon: TIcon;
    FIDEType: TSupportedIDEs;
  public
    property Version : TDelphiVersions read FVersion;
    property Path    : string read FPath write FPath;
    property Name    : string read FName write FName;
    property Icon    : TIcon read FIcon write FIcon;
    property IDEType : TSupportedIDEs read FIDEType write FIDEType;
    constructor Create;
    destructor Destroy; override;
  end;


const
  {$IFDEF DELPHI_OLDER_VERSIONS_SUPPORT}
  DelphiOldVersions = 2;
  DelphiOldVersionNumbers: array[0..DelphiOldVersions-1] of TDelphiVersions =(Delphi5,Delphi6);

  DelphiOldColorsCount =16;


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
  DelphiOldColorsList: array[0..DelphiOldColorsCount-1] of TColor =
  (
    $000000,$000080,$008000,$008080,
    $800000,$800080,$808000,$C0C0C0,
    $808080,$0000FF,$00FF00,$00FFFF,
    $FF0000,$FF00FF,$FFFF00,$FFFFFF
  );
  {$ENDIF}

  DelphiVersionsNames: array[TDelphiVersions] of string = (
  {$IFDEF DELPHI_OLDER_VERSIONS_SUPPORT}
    'Delphi 5',
    'Delphi 6',
  {$ENDIF}
    'Delphi 7',
    'Delphi 8',
    'BDS 2005',
    'BDS 2006',
    'RAD Studio 2007',
    'RAD Studio 2009',
    'RAD Studio 2010',
    'RAD Studio XE',
    'RAD Studio XE2',
    'RAD Studio XE3',
    'RAD Studio XE4',
    'RAD Studio XE5',
    'Appmethod 1.13',
    'RAD Studio XE6/Appmethod 1.14',
    'RAD Studio XE7/Appmethod 1.15',
    'RAD Studio XE8'
    );

  DelphiVersionNumbers: array[TDelphiVersions] of double =
    (
  {$IFDEF DELPHI_OLDER_VERSIONS_SUPPORT}
    13,      // 'Delphi 5',
    14,      // 'Delphi 6',
  {$ENDIF}
    15,      // 'Delphi 7',
    16,      // 'Delphi 8',
    17,      // 'BDS 2005',
    18,      // 'BDS 2006',
    18.5,    // 'RAD Studio 2007',
    20,      // 'RAD Studio 2009',
    21,      // 'RAD Studio 2010',
    22,      // 'RAD Studio XE'
    23,      // 'RAD Studio XE2'
    24,      // 'RAD Studio XE3'
    25,      // 'RAD Studio XE4'
    26,      // 'RAD Studio XE5'
    27,      // 'Appmethod 1.13'
    27,      // 'RAD Studio XE6'
    28,      // 'RAD Studio XE7'
    29       // 'RAD Studio XE8'
    );



  DelphiRegPaths: array[TDelphiVersions] of string = (
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
    '\Software\Embarcadero\BDS\16.0'
    );

  DelphiVCLStylesPaths: array[TDelphiVersions] of string = (
  {$IFDEF DELPHI_OLDER_VERSIONS_SUPPORT}
    '',
    '',
  {$ENDIF}
    '',
    '',
    '',
    '',
    '',
    '',
    '',
    '',
    'RAD Studio\9.0\Styles',
    'RAD Studio\10.0\Styles',
    'RAD Studio\11.0\Styles',
    'RAD Studio\12.0\Styles',
    '',
    'Embarcadero\Studio\14.0\Styles',
    'Embarcadero\Studio\15.0\Styles',
    'Embarcadero\Studio\16.0\Styles'
    );


procedure FillCurrentDelphiVersion(Data: TDelphiVersionData);
procedure FillListDelphiVersions(AList:TList<TDelphiVersionData>);
{$IFDEF DELPHI_OLDER_VERSIONS_SUPPORT}
function DelphiIsOldVersion(DelphiVersion:TDelphiVersions) : Boolean;
function GetIndexClosestColor(AColor:TColor) : Integer;
{$ENDIF}

function GetDelphiVersionMappedColor(AColor:TColor;DelphiVersion:TDelphiVersions) : TColor;
function GetVCLStylesFolder(DelphiVersion:TDelphiVersions) : string;

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
  uMisc,
  PsAPI,
  Controls,
  ImgList,
  CommCtrl,
  ShellAPI,
  ShlObj,
  Windows,
  uRegistry,
  Registry;


{$IFDEF DELPHI_OLDER_VERSIONS_SUPPORT}
function DelphiIsOldVersion(DelphiVersion:TDelphiVersions) : Boolean;
var
 i  : integer;
begin
 Result:=False;
  for i:=0  to DelphiOldVersions-1 do
    if DelphiVersion=DelphiOldVersionNumbers[i] then
    begin
       Result:=True;
       exit;
    end;
end;

function GetIndexClosestColor(AColor:TColor) : Integer;
var
  SqrDist,SmallSqrDist  : Double;
  i,R1,G1,B1,R2,G2,B2   : Integer;
begin
  Result:=0;
  SmallSqrDist := Sqrt(SQR(255)*3);
  R1 := GetRValue(AColor);
  G1 := GetGValue(AColor);
  B1 := GetBValue(AColor);

    for i := 0 to DelphiOldColorsCount-1 do
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


function GetDelphiVersionMappedColor(AColor:TColor;DelphiVersion:TDelphiVersions) : TColor;
begin
 Result:=AColor;
{$IFDEF DELPHI_OLDER_VERSIONS_SUPPORT}
  if DelphiIsOldVersion(DelphiVersion) then
  Result:= DelphiOldColorsList[GetIndexClosestColor(AColor)];
{$ENDIF}
end;


//C:\Users\Public\Documents\Embarcadero\Studio\16.0\Styles
function  GetVCLStylesFolder(DelphiVersion:TDelphiVersions) : string;
var
  List :TObjectList<TDelphiVersionData>;
  LData : TDelphiVersionData;
begin
  result:='';
  List:= TObjectList<TDelphiVersionData>.Create;
  try
    FillListDelphiVersions(List);
    for LData in List do
    if LData.Version=DelphiVersion then
    begin
      Result:= IncludeTrailingPathDelimiter(GetSpecialFolder(CSIDL_COMMON_DOCUMENTS))+DelphiVCLStylesPaths[DelphiVersion];
      if not DirectoryExists(Result) then
       Result:='';
      break;
    end;
  finally
    List.free;
  end;
end;

procedure FillCurrentDelphiVersion(Data: TDelphiVersionData);
var
  List :TObjectList<TDelphiVersionData>;
  LData : TDelphiVersionData;
  s : string;
begin
  s:=ParamStr(0);
  List:= TObjectList<TDelphiVersionData>.Create;
  try
    FillListDelphiVersions(List);
    for LData in List do
    if SameText(LData.Path, s) then
    begin
      Data.FVersion :=LData.Version;
      Data.Path    :=LData.Path;
      Data.Name    :=LData.Name;
      if Data.Icon=nil then Data.Icon:=TIcon.Create;
      Data.Icon.Assign(LData.Icon);
      Data.IDEType :=LData.IDEType;
      break;
    end;
  finally
    List.free;
  end;
end;

procedure FillListDelphiVersions(AList:TList<TDelphiVersionData>);
Var
  VersionData : TDelphiVersionData;
  DelphiComp  : TDelphiVersions;
  FileName    : string;
  Found       : boolean;
begin
  for DelphiComp := Low(TDelphiVersions) to High(TDelphiVersions) do
  begin
    Found := RegKeyExists(DelphiRegPaths[DelphiComp], HKEY_CURRENT_USER);
    if Found then
      Found := RegReadStr(DelphiRegPaths[DelphiComp], 'App', FileName, HKEY_CURRENT_USER) and FileExists(FileName);

    if  (DelphiComp>=DelphiXE6) and not Found then
    begin
      FileName:=StringReplace(FileName, 'bds.exe', 'appmethod.exe', [rfReplaceAll]);
      Found:=FileExists(FileName);
    end;


    if not Found then
    begin
      Found := RegKeyExists(DelphiRegPaths[DelphiComp], HKEY_LOCAL_MACHINE);
      if Found then
        Found := RegReadStr(DelphiRegPaths[DelphiComp], 'App', FileName, HKEY_LOCAL_MACHINE) and FileExists(FileName);
    end;

    if Found then
    begin
      VersionData:=TDelphiVersionData.Create;
      VersionData.FPath:=Filename;
      VersionData.FVersion:=DelphiComp;
      VersionData.FName   :=DelphiVersionsNames[DelphiComp];
      VersionData.FIDEType:=TSupportedIDEs.DelphiIDE;
      VersionData.Icon     :=TIcon.Create;
      ExtractIconFile(VersionData.FIcon, Filename, SHGFI_SMALLICON);
      AList.Add(VersionData);
    end;
  end;

end;



{ TDelphiVersionData }

constructor TDelphiVersionData.Create;
begin
  inherited;
  FIcon:=nil;
end;

destructor TDelphiVersionData.Destroy;
begin
  FreeAndNil(FIcon);
  inherited;
end;

end.

