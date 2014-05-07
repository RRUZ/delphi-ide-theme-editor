//**************************************************************************************************
//
// Unit uIDEExpertUtils
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
// The Original Code is uIDEExpertUtils.pas.
//
// The Initial Developer of the Original Code is Rodrigo Ruz V.
// Portions created by Rodrigo Ruz V. are Copyright (C) 2011-2014 Rodrigo Ruz V.
// All Rights Reserved.
//
//**************************************************************************************************

unit uIDEExpertUtils;

interface

Uses
  uDelphiVersions;

function ExpertInstalled(const Name : String; DelphiVersion : TDelphiVersions):boolean;
function InstallExpert(const Path,Description : String; DelphiVersion : TDelphiVersions):boolean;
function UnInstallExpert(const Path : String; DelphiVersion : TDelphiVersions):boolean;

implementation

Uses
  SysUtils,
  Classes,
  Windows,
  Registry;

Const
  //IDEPackgesKey='Known IDE Packages';
  IDEPackgesKey='Known Packages';

function ExpertInstalled(const Name : String; DelphiVersion : TDelphiVersions):boolean;
Var
  Reg       : TRegistry;
  KeyNames  : TStringList;
  Key       : string;
begin
  Result:=False;
  Reg:=TRegistry.Create;
  KeyNames:=TStringList.Create;
  try
    Reg.RootKey:=HKEY_CURRENT_USER;
    if Reg.OpenKey(DelphiRegPaths[DelphiVersion]+'\'+IDEPackgesKey, False) then
    begin
       Reg.GetValueNames(KeyNames);
       for Key in KeyNames do
        if CompareText(ExtractFileName(Key),Name)=0 then
        begin
         Result:=True;
         break;
        end;
    end;
  finally
    Reg.Free;
    KeyNames.Free;
  end;
end;

function InstallExpert(const Path, Description : String; DelphiVersion : TDelphiVersions):boolean;
Var
  Reg       : TRegistry;
begin
  Result:=False;
  Reg:=TRegistry.Create;
  try
    Reg.RootKey:=HKEY_CURRENT_USER;
    if Reg.OpenKey(DelphiRegPaths[DelphiVersion]+'\'+IDEPackgesKey, False) then
    begin
       Reg.WriteString(Path,Description);
       Result:=True;
    end;
  finally
    Reg.Free;
  end;
end;


function UnInstallExpert(const Path : String; DelphiVersion : TDelphiVersions):boolean;
Var
  Reg       : TRegistry;
begin
  Result:=False;
  Reg:=TRegistry.Create;
  try
    Reg.RootKey:=HKEY_CURRENT_USER;
    if Reg.OpenKey(DelphiRegPaths[DelphiVersion]+'\'+IDEPackgesKey, False) then
       Result:=Reg.DeleteValue(Path);
  finally
    Reg.Free;
  end;
end;

end.
