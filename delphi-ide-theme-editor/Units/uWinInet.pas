//**************************************************************************************************
// Unit uWinInet
//
// The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
// you may not use this file except in compliance with the License. You may obtain a copy of the
// License at http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
// ANY KIND, either express or implied. See the License for the specific language governing rights
// and limitations under the License.
//
// The Original Code is uWinInet.pas.
//
// The Initial Developer of the Original Code is Rodrigo Ruz V.
// Portions created by Rodrigo Ruz V. are Copyright (C) 2011-2014 Rodrigo Ruz V.
// All Rights Reserved.
//
//**************************************************************************************************

unit uWinInet;

interface

uses
  Classes;

type
   TuWinInetProcCallBack= procedure(BytesRead:Integer) of object;


function  GetRemoteFileSize(const Url : string): Integer;
procedure WinInet_HttpGet(const Url: string; Stream:TStream; CallBack:TuWinInetProcCallBack);overload;
function  WinInet_HttpGet(const Url: string; CallBack:TuWinInetProcCallBack): string;overload;
function  GetWinInetError(ErrorCode:Cardinal): string;


implementation

uses
 SysUtils,
 Windows,
 WinInet;

const
  sUserAgent = 'Mozilla/5.001 (windows; U; NT4.0; en-US; rv:1.0) Gecko/25250101';

function GetWinInetError(ErrorCode:Cardinal): string;
const
   winetdll = 'wininet.dll';
var
  Len: Integer;
  Buffer: PChar;
begin
  Len := FormatMessage(
  FORMAT_MESSAGE_FROM_HMODULE or FORMAT_MESSAGE_FROM_SYSTEM or
  FORMAT_MESSAGE_ALLOCATE_BUFFER or FORMAT_MESSAGE_IGNORE_INSERTS or  FORMAT_MESSAGE_ARGUMENT_ARRAY,
  Pointer(GetModuleHandle(winetdll)), ErrorCode, 0, @Buffer, SizeOf(Buffer), nil);
  try
    while (Len > 0) and {$IFDEF UNICODE}(CharInSet(Buffer[Len - 1], [#0..#32, '.'])) {$ELSE}(Buffer[Len - 1] in [#0..#32, '.']) {$ENDIF} do Dec(Len);
    SetString(Result, Buffer, Len);
  finally
    LocalFree(HLOCAL(Buffer));
  end;
end;


procedure ParseURL(const lpszUrl: string; var Host, Resource: string);
var
  lpszScheme      : array[0..INTERNET_MAX_SCHEME_LENGTH - 1] of Char;
  lpszHostName    : array[0..INTERNET_MAX_HOST_NAME_LENGTH - 1] of Char;
  lpszUserName    : array[0..INTERNET_MAX_USER_NAME_LENGTH - 1] of Char;
  lpszPassword    : array[0..INTERNET_MAX_PASSWORD_LENGTH - 1] of Char;
  lpszUrlPath     : array[0..INTERNET_MAX_PATH_LENGTH - 1] of Char;
  lpszExtraInfo   : array[0..1024 - 1] of Char;
  lpUrlComponents : TURLComponents;
begin
  ZeroMemory(@lpszScheme, SizeOf(lpszScheme));
  ZeroMemory(@lpszHostName, SizeOf(lpszHostName));
  ZeroMemory(@lpszUserName, SizeOf(lpszUserName));
  ZeroMemory(@lpszPassword, SizeOf(lpszPassword));
  ZeroMemory(@lpszUrlPath, SizeOf(lpszUrlPath));
  ZeroMemory(@lpszExtraInfo, SizeOf(lpszExtraInfo));
  ZeroMemory(@lpUrlComponents, SizeOf(TURLComponents));

  lpUrlComponents.dwStructSize      := SizeOf(TURLComponents);
  lpUrlComponents.lpszScheme        := lpszScheme;
  lpUrlComponents.dwSchemeLength    := SizeOf(lpszScheme);
  lpUrlComponents.lpszHostName      := lpszHostName;
  lpUrlComponents.dwHostNameLength  := SizeOf(lpszHostName);
  lpUrlComponents.lpszUserName      := lpszUserName;
  lpUrlComponents.dwUserNameLength  := SizeOf(lpszUserName);
  lpUrlComponents.lpszPassword      := lpszPassword;
  lpUrlComponents.dwPasswordLength  := SizeOf(lpszPassword);
  lpUrlComponents.lpszUrlPath       := lpszUrlPath;
  lpUrlComponents.dwUrlPathLength   := SizeOf(lpszUrlPath);
  lpUrlComponents.lpszExtraInfo     := lpszExtraInfo;
  lpUrlComponents.dwExtraInfoLength := SizeOf(lpszExtraInfo);

  InternetCrackUrl(PChar(lpszUrl), Length(lpszUrl), ICU_DECODE or ICU_ESCAPE, lpUrlComponents);

  Host := lpszHostName;
  Resource := lpszUrlPath;
end;

function GetRemoteFileSize(const Url : string): Integer;
var
  hInet    : HINTERNET;
  hConnect : HINTERNET;
  hRequest : HINTERNET;
  lpdwBufferLength: DWORD;
  lpdwReserved    : DWORD;
  ServerName: string;
  Resource: string;
  ErrorCode : Cardinal;
begin
  ParseURL(Url,ServerName,Resource);
  Result:=0;

  hInet := InternetOpen(PChar(sUserAgent), INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);
  if hInet=nil then
  begin
    ErrorCode:=GetLastError;
    raise Exception.Create(Format('InternetOpen Error %d Description %s',[ErrorCode,GetWinInetError(ErrorCode)]));
  end;

  try
    hConnect := InternetConnect(hInet, PChar(ServerName), INTERNET_DEFAULT_HTTP_PORT, nil, nil, INTERNET_SERVICE_HTTP, 0, 0);
    if hConnect=nil then
    begin
      ErrorCode:=GetLastError;
      raise Exception.Create(Format('InternetConnect Error %d Description %s',[ErrorCode,GetWinInetError(ErrorCode)]));
    end;

    try
      hRequest := HttpOpenRequest(hConnect, PChar('HEAD'), PChar(Resource), nil, nil, nil, 0, 0);
        if hRequest<>nil then
        begin
          try
            lpdwBufferLength:=SizeOf(Result);
            lpdwReserved    :=0;
            if not HttpSendRequest(hRequest, nil, 0, nil, 0) then
            begin
              ErrorCode:=GetLastError;
              raise Exception.Create(Format('HttpOpenRequest Error %d Description %s',[ErrorCode,GetWinInetError(ErrorCode)]));
            end;

             if not HttpQueryInfo(hRequest, HTTP_QUERY_CONTENT_LENGTH or HTTP_QUERY_FLAG_NUMBER, @Result, lpdwBufferLength, lpdwReserved) then
             begin
              Result:=0;
              ErrorCode:=GetLastError;
              raise Exception.Create(Format('HttpQueryInfo Error %d Description %s',[ErrorCode,GetWinInetError(ErrorCode)]));
             end;
          finally
            InternetCloseHandle(hRequest);
          end;
        end
        else
        begin
          ErrorCode:=GetLastError;
          raise Exception.Create(Format('HttpOpenRequest Error %d Description %s',[ErrorCode,GetWinInetError(ErrorCode)]));
        end;
    finally
      InternetCloseHandle(hConnect);
    end;
  finally
    InternetCloseHandle(hInet);
  end;
end;


procedure WinInet_HttpGet(const Url: string; Stream:TStream; CallBack:TuWinInetProcCallBack);overload;
const
  BuffSize = 1024*64;
var
  hInter   : HINTERNET;
  hFile    : HINTERNET;
  BytesRead: DWORD;
  Buffer   : Pointer;
  ErrorCode: Cardinal;
begin
  hInter := InternetOpen('', INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);
  if hInter=nil then
  begin
    ErrorCode:=GetLastError;
    raise Exception.Create(Format('InternetOpen Error %d Description %s',[ErrorCode,GetWinInetError(ErrorCode)]));
  end;

    try
      Stream.Seek(0,0);
      GetMem(Buffer,BuffSize);
      try
          hFile := InternetOpenUrl(hInter, PChar(Url), nil, 0, INTERNET_FLAG_RELOAD, 0);
          if hFile=nil then
          begin
            ErrorCode:=GetLastError;
            raise Exception.Create(Format('InternetOpenUrl Error %d Description %s',[ErrorCode,GetWinInetError(ErrorCode)]));
          end;

            try
              repeat
                InternetReadFile(hFile, Buffer, BuffSize, BytesRead);
                if BytesRead>0 then
                begin
                 Stream.WriteBuffer(Buffer^,BytesRead);
                 if @CallBack<>nil then
                  CallBack(BytesRead);
                end;
              until BytesRead = 0;
            finally
             InternetCloseHandle(hFile);
            end;
      finally
        FreeMem(Buffer);
      end;
    finally
     InternetCloseHandle(hInter);
    end;
end;

function WinInet_HttpGet(const Url: string; CallBack:TuWinInetProcCallBack): string;overload;
Var
  StringStream : TStringStream;
begin
  Result:='';
    StringStream:=TStringStream.Create('',TEncoding.UTF8);
    try
        WinInet_HttpGet(Url, StringStream, CallBack);
        if StringStream.Size>0 then
        begin
          StringStream.Seek(0,0);
          Result:=StringStream.ReadString(StringStream.Size);
        end;
    finally
      StringStream.Free;
    end;
end;


end.
