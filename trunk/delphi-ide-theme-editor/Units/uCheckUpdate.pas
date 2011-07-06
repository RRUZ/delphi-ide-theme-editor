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

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, pngimage, ExtCtrls;

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
  ShellAPI,
  uMisc,
  ComObj,
  WinInet;

Type
   TProcCallBack= procedure(BytesRead:Integer) of object;

const
  sRemoteVersionFile  = 'http://dl.dropbox.com/u/12733424/Blog/Delphi%20IDE%20Theme%20Editor/Version.xml';
  sXPathVersionNumber = '/versioninfo/@versionapp';
  sXPathUrlInstaller  = '/versioninfo/@url';
  sXPathInstallerFileName  = '/versioninfo/@installerfilename';
  sApplicationName         = 'Delphi IDE Theme Editor';

{$R *.dfm}


procedure ParseURL(const lpszUrl: string; var Host, Resource: string);
var
  UrlComponents: TURLComponents;
  lpszScheme: array[0..INTERNET_MAX_SCHEME_LENGTH - 1] of Char;
  lpszHostName: array[0..INTERNET_MAX_HOST_NAME_LENGTH - 1] of Char;
  lpszUserName: array[0..INTERNET_MAX_USER_NAME_LENGTH - 1] of Char;
  lpszPassword: array[0..INTERNET_MAX_PASSWORD_LENGTH - 1] of Char;
  lpszUrlPath: array[0..INTERNET_MAX_PATH_LENGTH - 1] of Char;
  lpszExtraInfo: array[0..1024 - 1] of Char;
begin
  ZeroMemory(@lpszScheme, SizeOf(lpszScheme));
  ZeroMemory(@lpszHostName, SizeOf(lpszHostName));
  ZeroMemory(@lpszUserName, SizeOf(lpszUserName));
  ZeroMemory(@lpszPassword, SizeOf(lpszPassword));
  ZeroMemory(@lpszUrlPath, SizeOf(lpszUrlPath));
  ZeroMemory(@lpszExtraInfo, SizeOf(lpszExtraInfo));
  ZeroMemory(@UrlComponents, SizeOf(TURLComponents));

  UrlComponents.dwStructSize := SizeOf(TURLComponents);
  UrlComponents.lpszScheme := lpszScheme;
  UrlComponents.dwSchemeLength := High(lpszScheme) + 1;
  UrlComponents.lpszHostName := lpszHostName;
  UrlComponents.dwHostNameLength := High(lpszHostName) + 1;
  UrlComponents.lpszUserName := lpszUserName;
  UrlComponents.dwUserNameLength := High(lpszUserName) + 1;
  UrlComponents.lpszPassword := lpszPassword;
  UrlComponents.dwPasswordLength := High(lpszPassword) + 1;
  UrlComponents.lpszUrlPath := lpszUrlPath;
  UrlComponents.dwUrlPathLength := High(lpszUrlPath) + 1;
  UrlComponents.lpszExtraInfo := lpszExtraInfo;
  UrlComponents.dwExtraInfoLength := High(lpszExtraInfo) + 1;

  InternetCrackUrl(PChar(lpszUrl), Length(lpszUrl), ICU_DECODE or ICU_ESCAPE, UrlComponents);

  Host := lpszHostName;
  Resource := lpszUrlPath;
end;

function GetFileSize(const Url : string): Integer;
var
  hInet: HINTERNET;
  hHTTP: HINTERNET;
  hReq: HINTERNET;
  //lpvBuffer: PChar;
  lpdwBufferLength: DWORD;
  lpdwReserved    : DWORD;
  Server: string;
  Resource: string;
begin
  ParseURL(Url,Server,Resource);

  hInet := InternetOpen(PChar('Mozilla/5.001 (windows; U; NT4.0; en-US; rv:1.0) Gecko/25250101'), INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);
  try
    hHTTP := InternetConnect(hInet, PChar(Server), INTERNET_DEFAULT_HTTP_PORT, nil, nil, INTERNET_SERVICE_HTTP, 0, 0);
    try
      hReq := HttpOpenRequest(hHTTP, PChar('HEAD'), PChar(Resource), nil, nil, nil, 0, 0);
      try
        //lpdwBufferLength:=64*1020;
        lpdwBufferLength:=SizeOf(Result);
        lpdwReserved    :=0;
        if not HttpSendRequest(hReq, nil, 0, nil, 0) then
          raise Exception.Create('HttpOpenRequest failed. ' + SysErrorMessage(GetLastError));
        //GetMem(lpvBuffer,lpdwBufferLength);
        try
         if not HttpQueryInfo(hReq, HTTP_QUERY_CONTENT_LENGTH or HTTP_QUERY_FLAG_NUMBER, @Result, lpdwBufferLength, lpdwReserved) then
          Result:=0;
         {
         if HttpQueryInfo(hReq, HTTP_QUERY_RAW_HEADERS_CRLF, lpvBuffer, lpdwBufferLength, lpdwReserved) then
         begin
           ShowMessage(lpvBuffer);
         end;
         }
        finally
         //FreeMem(lpvBuffer);
        end;
      finally
        InternetCloseHandle(hReq);
      end;
    finally
      InternetCloseHandle(hHTTP);
    end;
  finally
    InternetCloseHandle(hInet);
  end;
end;


procedure WinInet_HttpGet(const Url: string;Stream:TStream;CallBack:TProcCallBack);overload;
const
  BuffSize = 1024*64;
var
  hInter   : HINTERNET;
  UrlHandle: HINTERNET;
  BytesRead: DWORD;
  Buffer   : Pointer;
begin
  hInter := InternetOpen('', INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);
  if Assigned(hInter) then
    try
      Stream.Seek(0,0);
      GetMem(Buffer,BuffSize);
      try
          UrlHandle := InternetOpenUrl(hInter, PChar(Url), nil, 0, INTERNET_FLAG_RELOAD, 0);
          if Assigned(UrlHandle) then
            try
              repeat
                InternetReadFile(UrlHandle, Buffer, BuffSize, BytesRead);
                if BytesRead>0 then
                begin
                 Stream.WriteBuffer(Buffer^,BytesRead);
                 if @CallBack<>nil then
                  CallBack(BytesRead);
                end;
              until BytesRead = 0;
            finally
             InternetCloseHandle(UrlHandle);
            end;
      finally
        FreeMem(Buffer);
      end;
    finally
     InternetCloseHandle(hInter);
    end;
end;

function WinInet_HttpGet(const Url: string;CallBack:TProcCallBack): string;overload;
Var
  StringStream : TStringStream;
begin
  Result:='';
    StringStream:=TStringStream.Create('',TEncoding.UTF8);
    try
        WinInet_HttpGet(Url,StringStream,CallBack);
        if StringStream.Size>0 then
        begin
          StringStream.Seek(0,0);
          Result:=StringStream.ReadString(StringStream.Size);
        end;
    finally
      StringStream.Free;
    end;
end;

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
   ProgressBar1.Max:= GetFileSize(UrlInstaller);
   FTempInstallerFileName:=IncludeTrailingPathDelimiter(GetTempDirectory)+InstallerFileName;
   DeleteFile(TempInstallerFileName);
   FileStream:=TFileStream.Create(TempInstallerFileName,fmCreate);
   try
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
begin
   if ProgressBar1.Style=pbstNormal then
   begin
     ProgressBar1.Position:=ProgressBar1.Position+BytesRead;
     SetMsg(Format('Downloaded %s of %s bytes ',[FormatFloat('#,',ProgressBar1.Position),FormatFloat('#,',ProgressBar1.Max)]));
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
   Result:=FRemoteVersion>FLocalVersion;
end;

end.
