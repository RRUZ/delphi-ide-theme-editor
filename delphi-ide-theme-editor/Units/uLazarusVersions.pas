{**************************************************************************************************}
{                                                                                                  }
{ Unit uLazarusVersions                                                                            }
{ unit retrieves the lazarus ide installed versions  for the Delphi IDE Theme Editor               }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is uLazarusVersions.pas.                                                       }
{                                                                                                  }
{ The Initial Developer of the Original Code is Rodrigo Ruz V.                                     }
{ Portions created by Rodrigo Ruz V. are Copyright (C) 2011-2013 Rodrigo Ruz V.                    }
{ All Rights Reserved.                                                                             }
{                                                                                                  }
{**************************************************************************************************}

unit uLazarusVersions;

interface

uses
  ComObj,
  ComCtrls,
  ActiveX,
  Classes,
  Windows,
  Variants,
  uDelphiVersions,
  Generics.Collections,
  SysUtils;

function GetLazarusLocalFolder: string;
function GetLazarusEditorOptionsFileName: string;
function GetLazarusIDEFolder: string;
function GetLazarusIDEFileName: string;
function GetLazarusCompilerFileName: string;
function IsLazarusInstalled: Boolean;
procedure FillListViewLazarusVersions(ListView: TListView);
procedure FillListLazarusVersions(AList:TList<TDelphiVersionData>);



implementation

uses
  uMisc,
  Graphics,
  ShellAPI,
  uSupportedIDEs;

const
  sLazarusConfigFile = 'environmentoptions.xml';
  sLazarusIDEName    = 'lazarus.exe';
  sEditorOptionsFile ='editoroptions.xml';

function GetLazarusEditorOptionsFileName : string;
begin
  Result:=IncludeTrailingPathDelimiter(GetLazarusLocalFolder)+sEditorOptionsFile;
end;

procedure FillListViewLazarusVersions(ListView: TListView);
var
  FileName: string;
  Item: TListItem;
  Found : Boolean;
begin
  Found:=IsLazarusInstalled;
  if Found then
  begin
    FileName:=GetLazarusIDEFileName;
    ExtractIconFileToImageList(ListView.SmallImages, Filename);
    Item := ListView.Items.Add;
    Item.ImageIndex := ListView.SmallImages.Count - 1;
    Item.Caption := Format('Lazarus %s',[uMisc.GetFileVersion(FileName)]);
    item.SubItems.Add(FileName);
    item.SubItems.Add(IntToStr(Ord(TSupportedIDEs.LazarusIDE)));
    Item.Data := nil;
  end;
end;

procedure FillListLazarusVersions(AList:TList<TDelphiVersionData>);
var
  VersionData : TDelphiVersionData;
  Found : Boolean;
  FileName: string;
begin
  Found:=IsLazarusInstalled;
  if Found then
  begin
    FileName:=GetLazarusIDEFileName;
    {
    ExtractIconFileToImageList(ListView.SmallImages, Filename);
    Item := ListView.Items.Add;
    Item.ImageIndex := ListView.SmallImages.Count - 1;
    Item.Caption := Format('Lazarus %s',[uMisc.GetFileVersion(FileName)]);
    item.SubItems.Add(FileName);
    item.SubItems.Add(IntToStr(Ord(TSupportedIDEs.LazarusIDE)));
    Item.Data := nil;
     }
      VersionData:=TDelphiVersionData.Create;
      VersionData.Path:=Filename;
      //VersionData.Version:=;
      VersionData.Name   :=Format('Lazarus %s',[uMisc.GetFileVersion(FileName)]);
      VersionData.IDEType:=TSupportedIDEs.LazarusIDE;
      VersionData.Icon    :=TIcon.Create;
      ExtractIconFile(VersionData.Icon, Filename, SHGFI_SMALLICON);
      AList.Add(VersionData);
  end;

end;


function GetLazarusLocalFolder: string;
begin
  Result := Format('%slazarus', [IncludeTrailingPathDelimiter(GetLocalAppDataFolder)]);
  if not DirectoryExists(Result) then
    Result := '';
end;


function GetConfigLazarusValue(const AValue: string): string;
var
  LocalFolder: TFileName;
  FileName: TFileName;
  XmlDoc: olevariant;
  Node: olevariant;
begin
  Result      := '';
  LocalFolder := GetLazarusLocalFolder;
  if LocalFolder <> '' then
  begin
    FileName := Format('%s%s', [IncludeTrailingPathDelimiter(LocalFolder),  sLazarusConfigFile]);
    if FileExists(FileName) then
    begin
      XmlDoc := CreateOleObject('Msxml2.DOMDocument.6.0');
      try
        XmlDoc.Async := False;
        XmlDoc.Load(FileName);
        XmlDoc.SetProperty('SelectionLanguage', 'XPath');

        if (XmlDoc.parseError.errorCode <> 0) then
          raise Exception.CreateFmt('Error in Xml Data %s', [XmlDoc.parseError]);

        Node := XmlDoc.selectSingleNode(AValue);
        if not VarIsClear(Node) then
          Result := Node.Text;
      finally
        XmlDoc := Unassigned;
      end;
    end;
  end;
end;


function GetLazarusIDEFolder: string;
begin
  Result := GetConfigLazarusValue('//CONFIG/EnvironmentOptions/LazarusDirectory/@Value');
end;

function GetLazarusIDEFileName: string;
begin
  Result := Format('%s%s', [IncludeTrailingPathDelimiter(GetLazarusIDEFolder),
    sLazarusIDEName]);
end;

function GetLazarusCompilerFileName: string;
begin
  Result := GetConfigLazarusValue('//CONFIG/EnvironmentOptions/CompilerFilename/@Value');
end;

function IsLazarusInstalled: boolean;
begin
  Result := FileExists(GetLazarusIDEFileName);
end;


end.
