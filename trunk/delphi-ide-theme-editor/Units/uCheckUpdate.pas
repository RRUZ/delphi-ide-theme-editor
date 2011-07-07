{**************************************************************************************************}
{                                                                                                  }
{ Unit uCheckUpdate                                                                                }
{ Check for updates of the Delphi IDE Theme Editor                                                 }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is uCheckUpdate.pas.                                                           }
{                                                                                                  }
{ The Initial Developer of the Original Code is Rodrigo Ruz V.                                     }
{ Portions created by Rodrigo Ruz V. are Copyright (C) 2011 Rodrigo Ruz V.                         }
{ All Rights Reserved.                                                                             }
{                                                                                                  }
{**************************************************************************************************}

unit uCheckUpdate;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, pngimage, ExtCtrls, Diagnostics;

type
  TFrmCheckUpdate = class(TForm)
    LabelMsg: TLabel;
    ProgressBar1: TProgressBar;
    ImageUpdate: TImage;
    BtnCheckUpdates: TButton;
    LabelVersion: TLabel;
    BtnInstall: TButton;
    procedure FormCreate(Sender: TObject);
    procedure BtnCheckUpdatesClick(Sender: TObject);
    procedure BtnInstallClick(Sender: TObject);
  private
    FXmlVersionInfo: string;
    FLocalVersion: string;
    FRemoteVersion: string;
    FUrlInstaller: string;
    FInstallerFileName: string;
    FTempInstallerFileName: string;
    FStopwatch : TStopwatch;
    procedure ReadRemoteInfo;
    procedure ReadLocalInfo;
    property XmlVersionInfo : string read FXmlVersionInfo write FXmlVersionInfo;
    property RemoteVersion : string read FRemoteVersion write FRemoteVersion;
    property LocalVersion  : string read FLocalVersion write FLocalVersion;
    property UrlInstaller : string read FUrlInstaller write FUrlInstaller;
    property InstallerFileName : string read FInstallerFileName write FInstallerFileName;
    property TempInstallerFileName : string read FTempInstallerFileName write FTempInstallerFileName;
    procedure SetMsg(const Msg:string);
    procedure Download;
    procedure ExecuteInstaller;
    procedure DownloadCallBack(BytesRead:Integer);
  public
    function UpdateAvailable : Boolean;
  end;

implementation


uses
  uMisc,
  ComObj,
  ShellAPI,
  uWinInet;

const
  sRemoteVersionFile       = 'http://dl.dropbox.com/u/12733424/Blog/Delphi%20IDE%20Theme%20Editor/Version.xml';
  sApplicationName         = 'Delphi IDE Theme Editor';
  sXPathVersionNumber      = '/versioninfo/@versionapp';
  sXPathUrlInstaller       = '/versioninfo/@url';
  sXPathInstallerFileName  = '/versioninfo/@installerfilename';

{$R *.dfm}

{ TFrmCheckUpdate }
procedure TFrmCheckUpdate.BtnCheckUpdatesClick(Sender: TObject);
begin
  try
    ProgressBar1.Style:=pbstMarquee;
    BtnCheckUpdates.Enabled:=False;
    try
      ReadRemoteInfo;
      if not UpdateAvailable then
      begin
       MsgBox(Format('%s is up to date',[sApplicationName]));
       Close;
      end
      else
      if MessageDlg(Format('Exist a new version available %s , Do you want download the new version?',[RemoteVersion]),
      mtConfirmation, [mbYes, mbNo], 0) = mrYes then
        Download;
    finally
      ProgressBar1.Style:=pbstNormal;
      BtnCheckUpdates.Enabled:=True;
    end;
  except on E : Exception do
    SetMsg(Format('Error checking updates %s',[E.Message]));
  end;
end;

procedure TFrmCheckUpdate.BtnInstallClick(Sender: TObject);
begin
  ExecuteInstaller;
end;

procedure TFrmCheckUpdate.Download;
var
  FileStream : TFileStream;
begin
  try
   ProgressBar1.Style:=pbstNormal;
   SetMsg('Getting Application information');
   ProgressBar1.Max:= GetRemoteFileSize(UrlInstaller);
   FTempInstallerFileName:=IncludeTrailingPathDelimiter(GetTempDirectory)+InstallerFileName;
   DeleteFile(TempInstallerFileName);
   FileStream:=TFileStream.Create(TempInstallerFileName,fmCreate);
   try
     FStopwatch.Reset;
     FStopwatch.Start;
     WinInet_HttpGet(UrlInstaller,FileStream,DownloadCallBack);
     SetMsg('Application downloaded');
   finally
     FileStream.Free;
   end;
     BtnInstall.Visible:=FileExists(TempInstallerFileName);
     BtnCheckUpdates.Visible:=not BtnInstall.Visible;
     if BtnInstall.Visible then ExecuteInstaller;
  except on E : Exception do
    SetMsg(Format('Error checking updates %s',[E.Message]));
  end;
end;

procedure TFrmCheckUpdate.DownloadCallBack(BytesRead: Integer);
var
  Pos  :  Integer;
  Max  :  Integer;
  Rate :  Integer;
  sRate:  string;
begin
   if ProgressBar1.Style=pbstNormal then
   begin
     Pos:=ProgressBar1.Position+BytesRead;
     Max:=ProgressBar1.Max;
     Rate:=0;
     ProgressBar1.Position:=Pos;
     if FStopwatch.Elapsed.Seconds>0 then
     Rate:= Round(Max/1024/FStopwatch.Elapsed.Seconds);
     sRate:= Format('%d Kbytes x second',[Rate]);
     SetMsg(Format('Downloaded %s of %s bytes %n%% %sTransfer Rate %s',[FormatFloat('#,',Pos),FormatFloat('#,',Max),Pos*100/Max,#13#10,sRate]));
   end;
end;

procedure TFrmCheckUpdate.ExecuteInstaller;
begin
  if MessageDlg(Format('Do you want install the new version (the %s will be closed) ?',[sApplicationName]),
      mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    ShellExecute(Handle, 'Open', PChar(TempInstallerFileName), nil, nil, SW_SHOWNORMAL) ;
    Application.Terminate;
  end;

end;

procedure TFrmCheckUpdate.FormCreate(Sender: TObject);
begin
   FStopwatch:=TStopwatch.Create;
   ReadLocalInfo;
   LabelVersion.Caption:=Format('Current Version %s',[LocalVersion]);
   SetMsg('');
end;

procedure TFrmCheckUpdate.ReadLocalInfo;
begin
   FLocalVersion:=GetFileVersion(ParamStr(0));
end;

procedure TFrmCheckUpdate.ReadRemoteInfo;
var
  XmlDoc : OleVariant;
  Node   : OleVariant;
begin
  XmlDoc       := CreateOleObject('Msxml2.DOMDocument.6.0');
  XmlDoc.Async := False;
  try
    SetMsg('Getting version info');
    FXmlVersionInfo:=WinInet_HttpGet(sRemoteVersionFile,DownloadCallBack);
    XmlDoc.LoadXml(XmlVersionInfo);
    XmlDoc.SetProperty('SelectionLanguage','XPath');
    if (XmlDoc.parseError.errorCode <> 0) then
     raise Exception.CreateFmt('Error in Xml Data Code %s Reason %s',[XmlDoc.parseError.errorCode, XmlDoc.parseError.reason]);

     Node:=XmlDoc.selectSingleNode(sXPathVersionNumber);
     if not VarIsClear(Node) then FRemoteVersion:=Node.Text;

     Node:=XmlDoc.selectSingleNode(sXPathUrlInstaller);
     if not VarIsClear(Node) then FUrlInstaller:=Node.Text;

     Node:=XmlDoc.selectSingleNode(sXPathInstallerFileName);
     if not VarIsClear(Node) then FInstallerFileName:=Node.Text;
  finally
   XmlDoc    :=Unassigned;
  end;
end;

procedure TFrmCheckUpdate.SetMsg(const Msg: string);
begin
  LabelMsg.Caption:=Msg;
  LabelMsg.Update;
end;

function TFrmCheckUpdate.UpdateAvailable: Boolean;
begin
   if DebugHook<>0 then
     Result:=True
   else
     Result:=FRemoteVersion>FLocalVersion;
end;

end.
