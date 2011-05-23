unit uStackTrace;

interface

uses
  SysUtils, Classes, JclDebug;

implementation

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
end.
