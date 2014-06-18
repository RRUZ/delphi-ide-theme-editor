//**************************************************************************************************
//
// Unit uMain
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
// The Original Code is uMain.pas.
//
// The Initial Developer of the Original Code is Rodrigo Ruz V.
// Portions created by Rodrigo Ruz V. are Copyright (C) 2011-2014 Rodrigo Ruz V.
// All Rights Reserved.
//
//**************************************************************************************************

unit uMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, IdComponent, Diagnostics, uHttpDownload,
  Vcl.Imaging.pngimage, Vcl.ExtCtrls, Vcl.AppEvnts;

const
  WM_ALL_SEGMENTS_DONE = WM_USER + 666;

type
  TWaitDownloadThr = class(TThread)
  private
    HandlesList: Array [0 .. MAXIMUM_WAIT_OBJECTS - 1] of THandle;
    ThreadsList: Array [0 .. MAXIMUM_WAIT_OBJECTS - 1] of THttpDownloadSegmentThr;
    RangeFrom: Integer;
    RangeTo: Integer;
    CurrentIndex: Integer;
    FSegments : Integer;
    FSegmentSize : Integer;
    FFileName : string;
    procedure SetInitialValues;
  protected
    procedure Execute; override;
  public
    constructor Create(Segments, SegmentSize:Integer; Const FileName : String);
  end;

  TFrmMain = class(TForm)
    MemoLog: TMemo;
    EditURI: TEdit;
    Label1: TLabel;
    EditOutPut: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    BtnStart: TButton;
    LvSegments: TListView;
    PbGeneral: TProgressBar;
    LabelMsg: TLabel;
    Label4: TLabel;
    EditSegments: TEdit;
    BtnPause: TButton;
    ImageUpdate: TImage;
    LabelLog: TLabel;
    Button1: TButton;
    Bevel1: TBevel;
    BtnCheckUpdates: TButton;
    BtnInstall: TButton;
    Timer1: TTimer;
    procedure BtnStartClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure EditSegmentsExit(Sender: TObject);
    procedure BtnPauseClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure BtnCheckUpdatesClick(Sender: TObject);
    procedure BtnInstallClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    FApplicationName: string;
    FRemoteVersionFile : string;
    FXmlVersionInfo: string;
    FLocalVersion: string;
    FRemoteVersion: string;
    FUrlInstaller: string;
    FInstallerFileName: string;
    FTempInstallerFileName: string;
    FMaxsize: Integer;
    FStopwatch: TStopwatch;
    FSegmentSize: Integer;
    FSegments: Integer;
    WaitDownloadThr : TWaitDownloadThr;
    FCheckExternal: boolean;
    FErrorUpdate : boolean;
    FSilent : Boolean;
    procedure AddLog(const Msg: string);
    procedure UpdateProgress(Offset, lIndex, lDownloaded: Integer;
      lRate, lPorc: Double);
    procedure GetRemoteFileInfo;
    procedure InitThreads(Resume:Boolean);
    procedure MergeSegments;
    function  GetUpdateAvailable: Boolean;
    property  SegmentSize: Integer read FSegmentSize write FSegmentSize;
    property  Segments: Integer read FSegments write FSegments;
    procedure ReceiveMessage(var Msg: TMessage);  message WM_ALL_SEGMENTS_DONE;
    procedure ReadInfoUpdater;
    procedure ReadLocalInfo;
    procedure ReadRemoteInfo;
    procedure Download;
    procedure ExecuteInstaller;
  public
    property  CheckExternal   : boolean read FCheckExternal write FCheckExternal;
    property  XmlVersionInfo : string read FXmlVersionInfo write FXmlVersionInfo;
    property  RemoteVersion : string read FRemoteVersion write FRemoteVersion;
    property  LocalVersion  : string read FLocalVersion write FLocalVersion;
    property  UrlInstaller : string read FUrlInstaller write FUrlInstaller;
    property  InstallerFileName : string read FInstallerFileName write FInstallerFileName;
    property  TempInstallerFileName : string read FTempInstallerFileName write FTempInstallerFileName;
    property  UpdateAvailable : Boolean read GetUpdateAvailable;
    procedure ExecuteUpdater;
  end;

var
  FrmMain: TFrmMain;

implementation

uses
  IOUtils,
  TimeSpan,
  IdURI,
  IdSSLOpenSSL,
  ComObj,
  IdHTTP,
  ShellAPI,
  System.UITypes,
  uMisc;

{$R *.dfm}

const
  sXPathVersionNumber      = '/versioninfo/@versionapp';
  sXPathUrlInstaller       = '/versioninfo/@url';
  sXPathInstallerFileName  = '/versioninfo/@installerfilename';
  sXPathApplicationName    = '/downloadinfo/@ApplicationName';
  sXPathRemoteURL          = '/downloadinfo/@url';

function LHttpGet(const Url : string) : string;
var
  Http: TIdHTTP;
  buffer : TStringStream;
  LHandler: TIdSSLIOHandlerSocketOpenSSL;
begin
  Http := TIdHTTP.Create(nil);
  try
    buffer:=TStringStream.Create;
    try
      LHandler := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
      try
        Http.IOHandler:=LHandler;
        Http.Request.UserAgent:=sUserAgent;
        Http.HandleRedirects:=True;
        Http.Get(Url, buffer);
        Result:=buffer.DataString;
      finally
        LHandler.Free;
      end;
    finally
      buffer.Free;
    end;
  finally
    Http.Free;
  end;
end;
  
procedure TFrmMain.AddLog(const Msg: string);
begin
  MemoLog.Lines.Add(Msg);
  LabelLog.Caption:=Msg;
  LabelLog.Update;
end;

procedure TFrmMain.BtnCheckUpdatesClick(Sender: TObject);
begin
 ExecuteUpdater();
end;

procedure TFrmMain.BtnInstallClick(Sender: TObject);
begin
  ExecuteInstaller;
end;

procedure TFrmMain.BtnPauseClick(Sender: TObject);
begin
   if Assigned(WaitDownloadThr) and not WaitDownloadThr.Terminated then
   begin
    WaitDownloadThr.Terminate;
    BtnStart.Enabled:=True;
    BtnPause.Enabled:=False;
   end;
end;

procedure TFrmMain.BtnStartClick(Sender: TObject);
begin
  try
    if PbGeneral.Position=0 then
    GetRemoteFileInfo;
    InitThreads(PbGeneral.Position=0);
  except
    on E: Exception do
      if FSilent then Halt(0)
      else
      AddLog(E.Message);
  end;
end;

procedure TFrmMain.Button1Click(Sender: TObject);
begin
 if Height<640 then
   Height:=640
 else
   Height:=190;
end;

procedure TFrmMain.Download;
begin
  try
    PbGeneral.Style:=pbstNormal;
    AddLog('Getting Application information');
    FTempInstallerFileName:=IncludeTrailingPathDelimiter(GetTempDirectory)+InstallerFileName;
    DeleteFile(TempInstallerFileName);

    if PbGeneral.Position=0 then
      GetRemoteFileInfo;
    InitThreads(PbGeneral.Position=0);
  except on E : Exception do
    if FSilent then Halt(0)
    else
    AddLog(Format('Error checking updates %s',[E.Message]));
  end;
end;

procedure TFrmMain.EditSegmentsExit(Sender: TObject);
begin
  if (Trim(TEdit(Sender).Text) = '') or (Trim(TEdit(Sender).Text) = '0') then
    TEdit(Sender).Text := '1';
  FSegments := StrToInt(TEdit(Sender).Text);

  if FSegments > MAXIMUM_WAIT_OBJECTS then
  begin
    TEdit(Sender).Text := IntToStr(MAXIMUM_WAIT_OBJECTS);
    FSegments := MAXIMUM_WAIT_OBJECTS;
  end;
end;

procedure TFrmMain.ExecuteInstaller;
begin
  AddLog('Running installer');
  ShellExecute(Handle, 'Open', PChar(TempInstallerFileName), nil, nil, SW_SHOWNORMAL) ;
  Halt(0);
end;

procedure TFrmMain.ExecuteUpdater;
begin
  try
   if not Visible and not FSilent then
   begin
     Application.ShowMainForm:=True;
     Show;
   end;

    ReadInfoUpdater();
    ReadLocalInfo();
    AddLog(Format('Current Version %s',[LocalVersion]));

    PbGeneral.Style:=pbstMarquee;
    BtnCheckUpdates.Enabled:=False;
    try
      if not UpdateAvailable then
      begin
       if not FErrorUpdate and not FSilent then
        MessageDlg(Format('%s is up to date',[FApplicationName]), mtInformation, [mbOK], 0);
       Close;
      end
      else
      begin
       if not Visible then
         Show;

         if MessageDlg(Format('A new version (%s) of the %s was found. Do you want download and install?',[FRemoteVersion, FApplicationName]),  mtConfirmation, [mbYes,
           mbNo], 0) = mrYes then
           Download
         else
           Halt(0);
        //if CheckExternal then
        // ExecuteInstaller;
      end;
    finally
      //PbGeneral.Style:=pbstNormal;
      //BtnCheckUpdates.Enabled:=True;
    end;
  except on E : Exception do
    if FSilent then Halt(0)
    else
    AddLog(Format('Error checking updates %s',[E.Message]));
  end;
end;
procedure TFrmMain.FormActivate(Sender: TObject);
begin
//  if not CheckExternal then
//   ExecuteUpdater;
end;

procedure TFrmMain.FormCreate(Sender: TObject);
begin
  FSilent   := (ParamCount>1) and (SameText(ParamStr(2),'-Silent'));
  FRemoteVersion:='';
  FErrorUpdate  :=False;
  FCheckExternal:=False;
  PbGeneral.Position := 0;
  EditOutPut.Text := GetTempDirectory;
  FSegments := StrToInt(EditSegments.Text);
  FStopwatch := TStopwatch.Create;
end;

procedure TFrmMain.GetRemoteFileInfo;
var
  Http: TIdHTTP;
  LHandler: TIdSSLIOHandlerSocketOpenSSL;
begin
  Http := TIdHTTP.Create(nil);
  try
    LHandler := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
    try
      AddLog('Getting file info');
      Http.IOHandler:=LHandler;
      Http.Request.UserAgent:=sUserAgent;
      Http.HandleRedirects:=True;
      Http.Head(FUrlInstaller);
      AddLog(Http.Response.RawHeaders.Text);
      FMaxsize := Http.Response.ContentLength;
      FSegmentSize := FMaxsize div FSegments;
      AddLog(Format('Remote file size %s', [FormatFloat('#,', FMaxsize)]));
    finally
      LHandler.Free;
    end;
  finally
    Http.Free;
  end;
  PbGeneral.Max := FMaxsize;
end;

function TFrmMain.GetUpdateAvailable: Boolean;
begin
 Result:=False;
 //if DebugHook<>0 then exit;
 try
   if RemoteVersion='' then
     ReadRemoteInfo;

   if DebugHook<>0 then
     Result:=True
   else
     Result:=(FRemoteVersion>FLocalVersion);
 except on E : Exception do
   begin
    FErrorUpdate:=True;
    if FSilent then Halt(0)
    else
    MessageDlg(Format('Error checking updates %s',[E.Message]), mtWarning, [mbOK], 0);
   end;
 end;
end;


procedure TFrmMain.InitThreads(Resume:Boolean);
var
  Index: Integer;
  Item: TListItem;
begin
  BtnStart.Enabled := False;
  //BtnPause.Enabled := True;

  if Resume then
  begin
    AddLog(Format('Preparing segments, using %d segments of %d bytes',
      [Segments, SegmentSize]));

    LvSegments.Items.BeginUpdate;
    try
      LvSegments.Items.Clear;
      for Index := 0 to Segments - 1 do
      begin
        Item := LvSegments.Items.Add;
        Item.Caption := IntToStr(Index + 1);
        Item.SubItems.Add('');
        Item.SubItems.Add('');
        Item.SubItems.Add('');
        Item.SubItems.Add('');
        Item.SubItems.Add('');
      end;
    finally
      LvSegments.Items.EndUpdate;
    end;

    AddLog(Format('Init Download %s', [FormatDateTime('hh:nn:ss.zzz', Now)]));
  end
  else
  AddLog(Format('Resuming Download %s', [FormatDateTime('hh:nn:ss.zzz', Now)]));

  FStopwatch.Reset;
  FStopwatch.Start;
  WaitDownloadThr:=TWaitDownloadThr.Create(Segments, SegmentSize,IncludeTrailingPathDelimiter(EditOutPut.Text)+FInstallerFileName);
end;

procedure TFrmMain.MergeSegments;
var
  Index: Integer;
  FileName: string;
  InStream: TFileStream;
  OutStream: TFileStream;
begin
  AddLog('Merging segments');
  OutStream := TFileStream.Create(IncludeTrailingPathDelimiter(EditOutPut.Text)
    + FInstallerFileName, fmCreate or fmShareExclusive);
  try
    for Index := 0 to Segments - 1 do
    begin
      AddLog(Format('Merging segment %d', [Index + 1]));

      FileName := Format('%s%s.%s%d',[IncludeTrailingPathDelimiter(EditOutPut.Text), FInstallerFileName,'part', Index]);

      InStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
      try
        OutStream.CopyFrom(InStream, 0);
      finally
        InStream.Free;
      end;
      DeleteFile(FileName);
    end;
  finally
    OutStream.Free;
  end;
  AddLog('Merge done.');
end;

procedure TFrmMain.ReadInfoUpdater;
var
  LFile : string;
  Node   : OleVariant;
  XmlDoc : OleVariant;
begin
  AddLog('Reading info updater');
  LFile:=ExtractFilePath(ParamStr(0))+'DownloadInfo.xml';
  if not FileExists(LFile) then
   begin
     MessageDlg(Format('File %s not found', [LFile]), mtWarning, [mbOK], 0);
     Halt(0);
   end;

  XmlDoc       := CreateOleObject('Msxml2.DOMDocument.6.0');
  XmlDoc.Async := False;
  try
    XmlDoc.Load(LFile);
    XmlDoc.SetProperty('SelectionLanguage','XPath');
    if (XmlDoc.parseError.errorCode <> 0) then
     raise Exception.CreateFmt('Error in Xml Data Code %s Reason %s',[XmlDoc.parseError.errorCode, XmlDoc.parseError.reason]);

     Node:=XmlDoc.selectSingleNode(sXPathApplicationName);
     if not VarIsClear(Node) then FApplicationName:=Node.Text;

     Node:=XmlDoc.selectSingleNode(sXPathRemoteURL);
     if not VarIsClear(Node) then FRemoteVersionFile:=Node.Text;
  finally
   XmlDoc    :=Unassigned;
  end;
end;

procedure TFrmMain.ReadLocalInfo;
var
  LBinaryFile : string;
begin
   LBinaryFile:=ParamStr(1);
   if not FileExists(LBinaryFile) then
    begin
     MessageDlg(Format('File %s not found', [LBinaryFile]), mtWarning, [mbOK], 0);
      Halt(0);
    end;

   AddLog(Format('Reading version info from %s', [LBinaryFile]));
   FLocalVersion:=GetFileVersion(LBinaryFile);
end;

procedure TFrmMain.ReadRemoteInfo;
var
  XmlDoc : OleVariant;
  Node   : OleVariant;
begin
  XmlDoc       := CreateOleObject('Msxml2.DOMDocument.6.0');
  XmlDoc.Async := False;
  try
    AddLog('Getting remote version info');
    FXmlVersionInfo:=LHttpGet(FRemoteVersionFile);
    XmlDoc.LoadXml(XmlVersionInfo);
    XmlDoc.SetProperty('SelectionLanguage','XPath');
    if (XmlDoc.parseError.errorCode <> 0) then
     raise Exception.CreateFmt('Error in Xml Data Code %s Reason %s',[XmlDoc.parseError.errorCode, XmlDoc.parseError.reason]);

     Node:=XmlDoc.selectSingleNode(sXPathVersionNumber);
     if not VarIsClear(Node) then FRemoteVersion:=Node.Text;
     AddLog(Format('Version found %s',[FRemoteVersion]));

     Node:=XmlDoc.selectSingleNode(sXPathUrlInstaller);
     if not VarIsClear(Node) then FUrlInstaller:=Node.Text;
     EditURI.Text:=FUrlInstaller;
     
     Node:=XmlDoc.selectSingleNode(sXPathInstallerFileName);
     if not VarIsClear(Node) then FInstallerFileName:=Node.Text;
  finally
   XmlDoc    :=Unassigned;
  end;
end;

procedure TFrmMain.ReceiveMessage(var Msg: TMessage);
begin
  AddLog(Format('Download Done %s', [FormatDateTime('hh:nn:ss.zzz', Now)]));
  MergeSegments;
  //BtnStart.Enabled := True;
  //BtnPause.Enabled := False;
  //PbGeneral.Position := 0;
   BtnInstall.Visible:=FileExists(FTempInstallerFileName);
   BtnCheckUpdates.Visible:=not BtnInstall.Visible;
   if BtnInstall.Visible and not CheckExternal then ExecuteInstaller;  
end;

procedure TFrmMain.Timer1Timer(Sender: TObject);
begin
  TTimer(Sender).Enabled:=False;
  ExecuteUpdater;
end;

procedure TFrmMain.UpdateProgress(Offset, lIndex, lDownloaded: Integer; lRate, lPorc: Double);
var
  Rate: Double;
  FCurrent: Integer;
  porc: Double;
  MaxMilliSeconds: Integer;
  Elapsed: TTimeSpan;
  Remaining: TTimeSpan;
  ListItem: TListItem;
begin
  // update segment info in listview
  //if FStopwatch.Elapsed.TotalSeconds > 0 then
  //  Rate := lDownloaded / 1024.0 / FStopwatch.Elapsed.TotalSeconds;

  ListItem := LvSegments.Items.Item[lIndex];
  ListItem.SubItems[0] := FormatFloat('#,', lDownloaded);
  ListItem.SubItems[3] := FormatFloat('0.00 %', lPorc);
  ListItem.SubItems[4] := FormatFloat('0.00 KB/s', lRate);

  // Update overall progress
  PbGeneral.Position := PbGeneral.Position + Offset;
  FCurrent := PbGeneral.Position;

  Elapsed := FStopwatch.Elapsed;

  Rate := 0;
  if Elapsed.TotalSeconds > 0 then
    Rate := FCurrent / 1024.0 / Elapsed.TotalSeconds;

  porc := 100.0 * FCurrent / FMaxsize;
  MaxMilliSeconds := Round(Elapsed.TotalMilliseconds * 100 / porc);
  Remaining := Remaining.FromMilliseconds(MaxMilliSeconds);
  Remaining := Remaining.Subtract(Elapsed);

  LabelMsg.Caption :=
    Format('Speed %s (%n %%) %sElapsed Time %.2d:%.2d:%.2d.%.3d %sTime remaining %.2d:%.2d:%.2d.%.3d',
    [FormatFloat('0.00 KB/s', Rate), porc, slineBreak, Elapsed.Hours,
    Elapsed.Minutes, Elapsed.Seconds, Elapsed.Milliseconds, slineBreak,
    Remaining.Hours, Remaining.Minutes, Remaining.Seconds,
    Remaining.Milliseconds]);
end;


{TWaitDownloadThr}
constructor TWaitDownloadThr.Create(Segments, SegmentSize:Integer;Const FileName : String);
begin
  Inherited Create(false);
  FSegments:=Segments;
  FSegmentSize:=SegmentSize;
  FFileName:=FileName;
  FreeOnTerminate := True;
end;


procedure TWaitDownloadThr.SetInitialValues;
var
  Item: TListItem;
begin
  Item := FrmMain.LvSegments.Items[CurrentIndex];
  Item.SubItems[0] := '0';
  Item.SubItems[1] := FormatFloat('#,', RangeFrom);
  Item.SubItems[2] := FormatFloat('#,', RangeTo);
  Item.SubItems[4] := '0.00 %';
end;



procedure TWaitDownloadThr.Execute;
var
  FileName    : string;
  Index       : Integer;
  WaitResult  : Cardinal;
begin
  RangeTo := -1;

  for Index := 0 to FSegments - 1 do
  begin
    FileName := Format('%s.%s%d',[FFileName,'part', Index]);

    RangeFrom := RangeTo + 1;
    RangeTo := FSegmentSize * (Index + 1) - 1;

    if Index = FSegments - 1 then
      Inc(RangeTo, FrmMain.FMaxsize mod FSegments);

    CurrentIndex := Index;

    if (FrmMain.PbGeneral.Position=0) then
    DeleteFile(FileName);

    if not FileExists(FileName) or (FrmMain.PbGeneral.Position=0) then
    Synchronize(SetInitialValues);

    ThreadsList[Index] := THttpDownloadSegmentThr.Create(FrmMain.EditURI.Text,
      FileName, RangeFrom, RangeTo, Index, FrmMain.UpdateProgress, FrmMain.AddLog);
    HandlesList[Index] := ThreadsList[Index].Handle;
  end;

  {
  WaitForMultipleObjects(FSegments, @HandlesList[0], True, INFINITE);
  }

{
  while True do
  begin
    WaitResult:=WaitForMultipleObjects(FSegments, @HandlesList[0], True, 1000);
    if not Terminated and (WaitResult<> WAIT_TIMEOUT) then
      Break
    else
  end;
}

  while not Terminated do
  begin
    WaitResult:=WaitForMultipleObjects(FSegments, @HandlesList[0], True, 500);
    //FrmMain.AddLog(IntToStr(WaitResult));
    Synchronize(procedure begin FrmMain.LabelLog.Caption:='Downloading'; end);
    if (WaitResult<> WAIT_TIMEOUT) then
      Break;
  end;

    if Terminated then
    begin
      for Index := 0 to FSegments - 1 do
        if (HandlesList[Index] <> 0) and Assigned(ThreadsList[Index]) then
         ThreadsList[Index].Terminate;
    end;

  for Index := 0 to FSegments - 1 do
    if (HandlesList[Index] <> 0) and Assigned(ThreadsList[Index]) then
      ThreadsList[Index].Free;

  if not Terminated then
     PostMessage(FrmMain.Handle, WM_ALL_SEGMENTS_DONE, 0, 0);
end;

end.
