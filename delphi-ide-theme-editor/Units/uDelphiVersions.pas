{**************************************************************************************************}
{                                                                                                  }
{ Unit uDelphiVersions                                                                             }
{ unit retrieves the delphi ide installed versions  for the Delphi IDE Theme Editor                }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is uDelphiVersions.pas.                                                        }
{                                                                                                  }
{ The Initial Developer of the Original Code is Rodrigo Ruz V.                                     }
{ Portions created by Rodrigo Ruz V. are Copyright (C) 2011 Rodrigo Ruz V.                         }
{ All Rights Reserved.                                                                             }
{                                                                                                  }
{**************************************************************************************************}

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
    DelphiXE3
);

  TDelphiVersionData=Class
  private
    FVersion: TDelphiVersions;
    FName: string;
    FPath: string;
    FIcon: TIcon;
    FIDEType: TSupportedIDEs;
    //FSupportsColorizer: Boolean;
  public
    property Version : TDelphiVersions read FVersion;
    property Path    : string read FPath write FPath;
    property Name    : string read FName write FName;
    property Icon    : TIcon read FIcon write FIcon;
    property IDEType : TSupportedIDEs read FIDEType write FIDEType;
    //property SupportsColorizer : Boolean read FSupportsColorizer write FSupportsColorizer;
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
  )
     {
  (
    $000000,$800000,$008000,$808000,
    $000080,$800080,$008080,$C0C0C0,
    $808080,$FF0000,$00FF00,$FFFF00,
    $0000FF,$FF00FF,$00FFFF,$FFFFFF
  )
   }
  ;
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
    'RAD Studio XE3'
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
    24       // 'RAD Studio XE2'
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
    '\Software\Embarcadero\BDS\10.0'
    );


procedure FillListDelphiVersions(AList:TList<TDelphiVersionData>);
{$IFDEF DELPHI_OLDER_VERSIONS_SUPPORT}
function DelphiIsOldVersion(DelphiVersion:TDelphiVersions) : Boolean;
function GetIndexClosestColor(AColor:TColor) : Integer;
{$ENDIF}

function GetDelphiVersionMappedColor(AColor:TColor;DelphiVersion:TDelphiVersions) : TColor;

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
      VersionData.Icon    :=TIcon.Create;
      ExtractIconFile(VersionData.FIcon, Filename, SHGFI_SMALLICON);
      AList.Add(VersionData);
    end;
  end;

end;



end.
