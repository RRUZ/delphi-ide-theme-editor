//**************************************************************************************************
//
// Unit uSMSVersions
// this unit retrieves the Smart Mobile Studio installed versions  for the Delphi IDE Theme Editor
//
// The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
// you may not use this file except in compliance with the License. You may obtain a copy of the
// License at http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
// ANY KIND, either express or implied. See the License for the specific language governing rights
// and limitations under the License.
//
// The Original Code is uSMSVersions.pas.
//
// The Initial Developer of the Original Code is Rodrigo Ruz V.
// Portions created by Rodrigo Ruz V. are Copyright (C) 2011-2014 Rodrigo Ruz V.
// All Rights Reserved.
//
//**************************************************************************************************

unit uSMSVersions;

interface

uses
  ComObj,
  ComCtrls,
  ActiveX,
  Classes,
  Windows,
  Variants,
  uDelphiVersions,
  Generics.Collections;


function GetSMSLocalFolder: string;
function GetSMSEditorOptionsFileName: string;
function GetSMSIDEFolder: string;
function GetSMSIDEFileName: string;
function GetSMSCompilerFileName: string;
function IsSMSInstalled: Boolean;
procedure FillListSMSVersions(AList:TList<TDelphiVersionData>);

implementation

uses
  uMisc,
  SysUtils,
  ShellAPI,
  ShlObj,
  Vcl.Dialogs,
  Vcl.Graphics,
  uSupportedIDEs;

const
  sSMSConfigFile = 'preferences.ini';

function GetSMSLocalFolder: string;
begin
  Result := IncludeTrailingPathDelimiter(GetSpecialFolderLocation(CSIDL_COMMON_APPDATA))+'Optimale Systemer AS\Smart Mobile Studio';
  if not DirectoryExists(Result) then
    Result := '';
end;

function GetSMSEditorOptionsFileName: string;
begin
  Result:=IncludeTrailingPathDelimiter(GetSMSLocalFolder)+sSMSConfigFile;
end;

function GetSMSIDEFolder: string;
begin
  Result := IncludeTrailingPathDelimiter(GetSpecialFolderLocation(CSIDL_PROGRAM_FILES))+'Smart Mobile Studio';
end;

function GetSMSIDEFileName: string;
begin
  Result := IncludeTrailingPathDelimiter(GetSMSIDEFolder)+'SmartMS.exe';
end;

function GetSMSCompilerFileName: string;
begin
  Result := IncludeTrailingPathDelimiter(GetSMSIDEFolder)+'SmartMS.exe';
end;

function IsSMSInstalled: Boolean;
begin
  Result := FileExists(GetSMSIDEFileName);
end;

procedure FillListSMSVersions(AList:TList<TDelphiVersionData>);
var
  VersionData : TDelphiVersionData;
  Found : Boolean;
  FileName: string;
begin
  Found:=IsSMSInstalled;
  if Found then
  begin
    FileName:=GetSMSIDEFileName;
    VersionData:=TDelphiVersionData.Create;
    VersionData.Path:=Filename;
    VersionData.Name   :=Format('Smart Mobile Studio %s',[uMisc.GetFileVersion(FileName)]);
    VersionData.IDEType:=TSupportedIDEs.SMSIDE;
    VersionData.Icon    :=TIcon.Create;
    //VersionData.Icon.LoadFromFile(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)))+'images\sms.ico');
    ExtractIconFile(VersionData.Icon, Filename, SHGFI_SMALLICON);
    AList.Add(VersionData);
  end;
end;


end.
