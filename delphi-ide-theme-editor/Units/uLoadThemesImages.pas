//**************************************************************************************************
//
// Unit uLoadThemesImages
// unit for uLoadThemesImages the Delphi IDE Theme Editor
//
// The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
// you may not use this file except in compliance with the License. You may obtain a copy of the
// License at http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
// ANY KIND, either express or implied. See the License for the specific language governing rights
// and limitations under the License.
//
// The Original Code is uLoadThemesImages.pas.
//
// The Initial Developer of the Original Code is Rodrigo Ruz V.
// Portions created by Rodrigo Ruz V. are Copyright (C) 2011-2014 Rodrigo Ruz V.
// All Rights Reserved.
//
//**************************************************************************************************

unit uLoadThemesImages;

interface

uses
  Vcl.Controls,
  Vcl.ComCtrls,
  Classes;

type
  TLoadThemesImages = class(TThread)
  private
    FPath :String;
    FImageList : TImageList;
    FListview  : TListView;
  protected
    procedure Execute; override;
  public
    constructor Create(const Path : string;ImageList:TImageList;ListView: TListView);
  end;


implementation

uses
  ActiveX,
  SysUtils,
  IOUtils,
  Graphics,
  uMisc,
  uDelphiIDEHighlight;

constructor TLoadThemesImages.Create(const Path : string;ImageList:TImageList;ListView: TListView);
begin
   inherited Create(False);
   FPath:=Path;
   FImageList:=ImageList;
   FListview:=ListView;
   FreeOnTerminate:=True;
end;

procedure TLoadThemesImages.Execute;
var
  Item    : TListItem;
  ImageName: string;
  FileName: string;
  ImpTheme: TIDETheme;
  Bmp     : TBitmap;
  i       : Integer;
begin
  inherited;
  if not TDirectory.Exists(FPath) then
    exit;

  FListview.SmallImages:=nil;
  FImageList.Clear;
  SysUtils.ForceDirectories(IncludeTrailingPathDelimiter(FPath)+'Images');
  CoInitialize(nil);
  try
    for i:=0 to FListview.Items.Count-1 do
    begin
      Item:=FListview.Items.Item[i];
      ImageName:=IncludeTrailingPathDelimiter(FPath)+'Images\'+ Item.Caption + '.bmp';

        Bmp:=TBitmap.Create;
        try

         if FileExists(ImageName) then
          Bmp.LoadFromFile(ImageName)
         else
         begin
           FileName :=IncludeTrailingPathDelimiter(FPath)+ Item.Caption + '.theme.xml';
           LoadThemeFromXMLFile(ImpTheme, FileName);
           //CreateBitmapSolidColor(16,16,[StringToColor(ImpTheme[ReservedWord].BackgroundColorNew),StringToColor(ImpTheme[ReservedWord].ForegroundColorNew)], Bmp);
           CreateArrayBitmap(16,16,[StringToColor(ImpTheme[ReservedWord].ForegroundColorNew),StringToColor(ImpTheme[ReservedWord].BackgroundColorNew)], Bmp);
         end;

         Synchronize(
           procedure
           begin
             FImageList.Add(Bmp, nil);
             Item.ImageIndex:=FImageList.Count-1;
           end
         );
          Bmp.SaveToFile(ImageName);
        finally
           Bmp.Free;
        end;
    end;

  finally
    CoUninitialize;
    FListview.SmallImages:=FImageList;
  end;
end;

end.
