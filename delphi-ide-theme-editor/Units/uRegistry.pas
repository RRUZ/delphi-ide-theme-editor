//**************************************************************************************************
//
// Unit uRegistry
// unit with windows registry helper functions  for the Delphi IDE Theme Editor
//
// The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
// you may not use this file except in compliance with the License. You may obtain a copy of the
// License at http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
// ANY KIND, either express or implied. See the License for the specific language governing rights
// and limitations under the License.
//
// The Original Code is uRegistry.pas.
//
// The Initial Developer of the Original Code is Rodrigo Ruz V.
// Portions created by Rodrigo Ruz V. are Copyright (C) 2011-2015 Rodrigo Ruz V.
// All Rights Reserved.
//
//**************************************************************************************************

unit uRegistry;

interface

uses
  Windows,
  Registry;

function RegReadStr(const RegPath, RegValue: string; var Str: string; const RootKey: HKEY): boolean;
function RegReadInt(const RegPath, RegValue: string; var IntValue: integer; const RootKey: HKEY): boolean;
function RegWriteStr(const RegPath, RegValue: string; const Str: string; const RootKey: HKEY): boolean;
function RegWriteInt(const RegPath, RegValue: string; IntValue: integer; const RootKey: HKEY): boolean;
function RegKeyExists(const RegPath: string; const RootKey: HKEY): boolean;
function RegLoadKey(const RegPath, FileName: string; const RootKey: HKEY): boolean;
function RegSaveKey(const RegPath, FileName: string; const RootKey: HKEY): boolean;

implementation

function RegLoadKey(const RegPath, FileName: string; const RootKey: HKEY): boolean;
var
  Reg: TRegistry;
begin
  try
    Reg := TRegistry.Create;
    try
      Reg.RootKey := RootKey;
      Result      := Reg.LoadKey(RegPath, FileName);
      Reg.CloseKey;
    finally
      Reg.Free;
    end;
  except
    Result := False;
  end;
end;

function RegSaveKey(const RegPath, FileName: string; const RootKey: HKEY): boolean;
var
  Reg: TRegistry;
begin
  try
    Reg := TRegistry.Create;
    try
      Reg.RootKey := RootKey;
      Result      := Reg.SaveKey(RegPath, FileName);
      Reg.CloseKey;
    finally
      Reg.Free;
    end;
  except
    Result := False;
  end;
end;

function RegWriteStr(const RegPath, RegValue: string; const Str: string; const RootKey: HKEY): boolean;
var
  Reg: TRegistry;
begin
  try
    Reg := TRegistry.Create;
    try
      Reg.RootKey := RootKey;
      Result      := Reg.OpenKey(RegPath, True);
      if Result then
        Reg.WriteString(RegValue, Str);
    finally
      Reg.Free;
    end;
  except
    Result := False;
  end;
end;

function RegReadStr(const RegPath, RegValue: string; var Str: string;
  const RootKey: HKEY): boolean;
var
  Reg: TRegistry;
begin
  try
    Reg := TRegistry.Create;
    try
      Reg.RootKey := RootKey;
      Result      := Reg.OpenKey(RegPath, True);
      if Result then
        Str := Reg.ReadString(RegValue);
    finally
      Reg.Free;
    end;
  except
    Result := False;
  end;
end;

function RegWriteInt(const RegPath, RegValue: string; IntValue: integer;
  const RootKey: HKEY): boolean;
var
  Reg: TRegistry;
begin
  try
    Reg := TRegistry.Create;
    try
      Reg.RootKey := RootKey;
      Result      := Reg.OpenKey(RegPath, True);
      if Result then
        Reg.WriteInteger(RegValue, IntValue);
    finally
      Reg.Free;
    end;
  except
    Result := False;
  end;
end;

function RegReadInt(const RegPath, RegValue: string; var IntValue: integer;
  const RootKey: HKEY): boolean;
var
  Reg: TRegistry;
begin
  try
    Reg := TRegistry.Create;
    try
      Reg.RootKey := RootKey;
      Result      := Reg.OpenKey(RegPath, True);
      if Result then
        IntValue := Reg.ReadInteger(RegValue);
    finally
      Reg.Free;
    end;
  except
    Result := False;
  end;
end;

function RegKeyExists(const RegPath: string; const RootKey: HKEY): boolean;
var
  Reg: TRegistry;
begin
  try
    Reg := TRegistry.Create;
    try
      Reg.RootKey := RootKey;
      Result      := Reg.KeyExists(RegPath);
    finally
      Reg.Free;
    end;
  except
    Result := False;
  end;
end;

end.
