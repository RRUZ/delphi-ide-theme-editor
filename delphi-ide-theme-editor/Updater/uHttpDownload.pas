//**************************************************************************************************
//
// Unit uHttpDownload
// Check for updates of the Delphi IDE Theme Editor
//
// The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
// you may not use this file except in compliance with the License. You may obtain a copy of the
// License at http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
// ANY KIND, either express or implied. See the License for the specific language governing rights
// and limitations under the License.
//
// The Original Code is uHttpDownload.pas.
//
// The Initial Developer of the Original Code is Rodrigo Ruz V.
// Portions created by Rodrigo Ruz V. are Copyright (C) 2011-2014 Rodrigo Ruz V.
// All Rights Reserved.
//
//**************************************************************************************************

unit uHttpDownload;

interface

uses
  SysUtils,
  IdHttp,
  IdComponent,
  IdSSLOpenSSL,
  Classes,
  Diagnostics;

const
  sUserAgent = 'Mozilla/5.001 (windows; U; NT4.0; en-US; rv:1.0) Gecko/25250101';

type
  TUpdateProgress = procedure(Offset, lIndex, lDownloaded: Integer; lRate, lPorc : Double) of object;
  TLogCallback    = procedure(const Msg : string) of object;

  THttpDownloadSegmentThr = class(TThread)
  private
    FURL: string;
    FOffset: Integer;
    FOverallDownload : Integer;
    FSessionDownload : Integer;
    FRangeFrom: Integer;
    FRangeTo: Integer;
    FFileName: string;
    FStopwatch: TStopwatch;
    FUpdateProgress: TUpdateProgress;
    FIndexItem : Integer;
    FSegmentSize : Integer;
    FException: Exception;
    FHttp: TIdHTTP;
    FileStream: TFileStream;
    FLog : TLogCallback;
    FMsg: string;
    SSLHandler : TIdSSLIOHandlerSocketOpenSSL;
    procedure OnWork(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);
    procedure UpdateProgress;
    procedure DoHandleException;
    procedure UpdateLog;
  protected
    procedure Execute; override;
    procedure HandleException; virtual;
  public
    constructor Create(const URL, FileName: string; RangeFrom, RangeTo: Integer; IndexItem : Integer; UpdateProgress: TUpdateProgress; Log: TLogCallback);
    destructor Destroy; override;
  end;


implementation

uses
  Windows,
  IdHTTPHeaderInfo;

{ THttpGetRange }

constructor THttpDownloadSegmentThr.Create(const URL, FileName: string; RangeFrom, RangeTo: Integer; IndexItem : Integer; UpdateProgress: TUpdateProgress; Log: TLogCallback);
begin
  inherited Create(False);
  FURL := URL;
  FRangeFrom := RangeFrom;
  FRangeTo := RangeTo;
  FSegmentSize := RangeTo - RangeFrom +1;
  FIndexItem:= IndexItem;
  FFileName := FileName;
  FOverallDownload := 0;
  FSessionDownload :=0;
  FLog:=Log;
  FStopwatch := TStopwatch.Create;
  FUpdateProgress := UpdateProgress;
  FHttp := TIdHTTP.Create(nil);
  SSLHandler := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
  FHttp.IOHandler:= SSLHandler;
  FHttp.Request.UserAgent:=sUserAgent;
  FHttp.HandleRedirects:=True;
//  if FileExists(FFileName) then
//  begin
//   FileStream := TFileStream.Create(FFileName, fmOpenReadWrite);
//   FOverallDownload:=FileStream.Size;
//   Inc(FRangeFrom, FOverallDownload);
//   FileStream.Position:=FileStream.Size;
//  end
//  else
   FileStream := TFileStream.Create(FFileName, fmCreate);
end;

destructor THttpDownloadSegmentThr.Destroy;
begin
  FHttp.Free;
  SSLHandler.Free;
  FileStream.Free;

  if Terminated then
  begin
    FileStream := TFileStream.Create(FFileName, fmOpenReadWrite);
    try
      if true then //FOverallDownload<FileStream.Size then
      begin
       FMsg:=Format('Truncating File segment %d',[FIndexItem+1]);
       Synchronize(UpdateLog);
       FileStream.Seek(FOverallDownload, soFromBeginning);
       SetEndOfFile(FileStream.Handle);
      end;
    finally
      FileStream.Free;
    end;
  end;

  inherited;
end;


procedure THttpDownloadSegmentThr.DoHandleException;
begin
   if Assigned(FLog) then
    FLog(Format('Exception in Segment %d : %s',[FIndexItem+1,FException.Message]));
end;

procedure THttpDownloadSegmentThr.Execute;
var
  Range: TIdEntityRange;
begin
  FException := nil;
  try
    FHttp.OnWork := OnWork;
    Range := FHttp.Request.Ranges.Add;
    Range.StartPos := FRangeFrom;
    Range.EndPos := FRangeTo;
    FStopwatch.Reset;
    FStopwatch.Start;
    FMsg:=Format('Starting segment %d',[FIndexItem+1]);
    Synchronize(UpdateLog);
    FHttp.Get(FURL, FileStream);
  except
    HandleException;
  end;
end;

procedure THttpDownloadSegmentThr.HandleException;
begin
  FException := Exception(ExceptObject);
  try
    if not (FException is EAbort) and not Terminated then
      Synchronize(DoHandleException);
  finally
    FException := nil;
  end;
end;

procedure THttpDownloadSegmentThr.OnWork(ASender: TObject; AWorkMode: TWorkMode;
  AWorkCount: Int64);
begin
    FOffset  := AWorkCount - FSessionDownload;
    FSessionDownload := AWorkCount;
    inc(FOverallDownload, FOffset);
    Synchronize(UpdateProgress);


  if Terminated then
  begin
    FMsg:=Format('Cancelling segment %d',[FIndexItem+1]);
    Synchronize(UpdateLog);
    //FHttp.Disconnect;
    Abort;
  end;
    //Abort;
end;

procedure THttpDownloadSegmentThr.UpdateLog;
begin
  if Assigned(FLog) then
   FLog(FMsg);
end;

procedure THttpDownloadSegmentThr.UpdateProgress;
var
  Rate,Porc : Double;
begin
  Rate:=0;
  {
  if FStopwatch.Elapsed.TotalSeconds > 0 then
    Rate := FCurrent / 1024.0 / FStopwatch.Elapsed.TotalSeconds;

  Porc:=FCurrent * 100.0 / (FRangeTo - FRangeFrom);

  if Assigned(FUpdateProgress) then
    FUpdateProgress(FOffset,FIndexItem, FCurrent, Rate, Porc);

  }
  if FStopwatch.Elapsed.TotalSeconds > 0 then
    Rate := FSessionDownload / 1024.0 / FStopwatch.Elapsed.TotalSeconds;

  Porc:=FOverallDownload * 100.0 / FSegmentSize;

  if Assigned(FUpdateProgress) then
    FUpdateProgress(FOffset,FIndexItem, FOverallDownload, Rate, Porc);
end;

end.
