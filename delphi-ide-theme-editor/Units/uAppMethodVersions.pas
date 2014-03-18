//**************************************************************************************************
//
// Unit uAppMethodVersions
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
// The Original Code is uAppMethodVersions.pas
//
// The Initial Developer of the Original Code is Rodrigo Ruz V.
// Portions created by Rodrigo Ruz V. are Copyright (C) 2011-2014 Rodrigo Ruz V.
// All Rights Reserved.
//
//**************************************************************************************************

unit uAppMethodVersions;

interface

uses
  Generics.Defaults,
  Generics.Collections,
  uSupportedIDEs,
  uDelphiVersions,
  Graphics,
  SysUtils,
  Classes,
  ComCtrls;


type
  TAppMethodVersions =
    (
    AppMethod113
    );

const
  AppMethodVersionsNames: array[TAppMethodVersions] of string = (
    'Appmethod 1.13'
    );

  AppMethodRegPaths: array[TAppMethodVersions] of string = (
    '\Software\Embarcadero\BDS\13.0'
    );


//procedure FillListAppMethodVersions(AList:TList<TDelphiVersionData>);



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



procedure FillListAppMethodVersions(AList:TList<TDelphiVersionData>);
Var
  VersionData : TDelphiVersionData;
  LAppmethodComp  : TAppMethodVersions;
  FileName    : string;
  Found       : boolean;
begin
  for LAppmethodComp := Low(TAppMethodVersions) to High(TAppMethodVersions) do
  begin
    Found := RegKeyExists(AppMethodRegPaths[LAppmethodComp], HKEY_CURRENT_USER);
    if Found then
      Found := RegReadStr(AppMethodRegPaths[LAppmethodComp], 'App', FileName, HKEY_CURRENT_USER) and FileExists(FileName);

    if not Found then
    begin
      Found := RegKeyExists(AppMethodRegPaths[LAppmethodComp], HKEY_LOCAL_MACHINE);
      if Found then
        Found := RegReadStr(AppMethodRegPaths[LAppmethodComp], 'App', FileName, HKEY_LOCAL_MACHINE) and FileExists(FileName);
    end;

    if Found then
    begin
      VersionData:=TDelphiVersionData.Create;
      VersionData.Path:=Filename;
      //VersionData.Version:=LAppmethodComp;
      VersionData.Name   :=AppMethodVersionsNames[LAppmethodComp];
      VersionData.IDEType:=TSupportedIDEs.AppMethodIDE;
      VersionData.Icon    :=TIcon.Create;
      ExtractIconFile(VersionData.Icon, Filename, SHGFI_SMALLICON);
      AList.Add(VersionData);
    end;
  end;

end;



end.
