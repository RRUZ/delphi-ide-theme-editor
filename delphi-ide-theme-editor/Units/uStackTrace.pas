//**************************************************************************************************
//
// Unit uStackTrace
// unit uStackTrace  for the Delphi IDE Theme Editor
//
// The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
// you may not use this file except in compliance with the License. You may obtain a copy of the
// License at http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
// ANY KIND, either express or implied. See the License for the specific language governing rights
// and limitations under the License.
//
// The Original Code is uStackTrace.pas.
//
// The Initial Developer of the Original Code is Rodrigo Ruz V.
// Portions created by Rodrigo Ruz V. are Copyright (C) 2011-2014 Rodrigo Ruz V.
// All Rights Reserved.
//
//**************************************************************************************************
unit uStackTrace;

interface

{.DEFINE ENABLE_STACKTRACE}

{$IFDEF ENABLE_STACKTRACE}
uses
  SysUtils, Classes, JclDebug;
{$ENDIF}

implementation

{$IFDEF ENABLE_STACKTRACE}
function GetExceptionStackInfoProc(P: PExceptionRecord):Pointer;
begin
  Result := TJclStackInfoList.Create(False, 0, nil);
end;

function GetStackInfoStringProc(Info: Pointer): string;
var
  StackInfoList : TJclStackInfoList;
  List  : TStringList;
begin
  if Info = nil then Exit;
  List  := TStringList.Create;
  try
   StackInfoList := TJclStackInfoList(Info);
   StackInfoList.AddToStrings(List);
   Result :=  List.Text;
  finally
    List.Free;
  end;
end;

procedure CleanUpStackInfoProc(Info: Pointer);
begin
  FreeAndNil(TJclStackInfoList(Info));
end;

initialization
  if JclStartExceptionTracking then
  begin
    Exception.GetExceptionStackInfoProc := GetExceptionStackInfoProc;
    Exception.GetStackInfoStringProc    := GetStackInfoStringProc;
    Exception.CleanUpStackInfoProc      := CleanUpStackInfoProc;
  end;

finalization
  if JclExceptionTrackingActive then
  begin
    Exception.GetExceptionStackInfoProc := nil;
    Exception.GetStackInfoStringProc    := nil;
    Exception.CleanUpStackInfoProc      := nil;
    JclStopExceptionTracking;
  end;
{$ENDIF}
end.
