// **************************************************************************************************
//
// Unit DITE.LoadThemesImages
// unit for the Delphi IDE Theme Editor
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
// Portions created by Rodrigo Ruz V. are Copyright (C) 2011-2019 Rodrigo Ruz V.
// All Rights Reserved.
//
// **************************************************************************************************

unit DITE.LoadThemesImages;

interface

uses
  Vcl.Controls,
  Vcl.ComCtrls,
  Classes;

type
  TLoadThemesImages = class(TThread)
  private
    FPath: String;
    FImageList: TImageList;
    FListview: TListView;
  protected
    procedure Execute; override;
  public
    constructor Create(const Path: string; ImageList: TImageList; ListView: TListView);
  end;

implementation

uses
  ActiveX,
  SysUtils,
  IOUtils,
  Graphics,
  DITE.Misc,
  System.Types,
  DITE.DelphiIDEHighlight;

constructor TLoadThemesImages.Create(const Path: string; ImageList: TImageList; ListView: TListView);
begin
  inherited Create(False);
  FPath := Path;
  FImageList := ImageList;
  FListview := ListView;
  FreeOnTerminate := True;
end;

procedure TLoadThemesImages.Execute;
var
  Item: TListItem;
  ImageName: string;
  FileName: string;
  ImpTheme: TIDETheme;
  LBitmap: TBitmap;
  i: Integer;
  CreateThumbnail: Boolean;

  procedure CreateThemeBmp(Width, Height: Word; Background, Foreground1, Foreground2: TColor; var Bitmap: TBitmap);
  Var
    LRect: TRect;
  begin
    Bitmap.PixelFormat := pf24bit;
    Bitmap.Width := Width;
    Bitmap.Height := Height;
    Bitmap.Canvas.Brush.Color := Background;
    LRect := Rect(0, 0, Width, Height);
    Bitmap.Canvas.FillRect(LRect);

    // InflateRect(LRect, -1, -1);
    Bitmap.Canvas.Brush.Style := bsClear;
    Bitmap.Canvas.Pen.Color := clBlack;
    Bitmap.Canvas.Rectangle(LRect);

    Bitmap.Canvas.Pen.Color := Foreground1;
    Bitmap.Canvas.MoveTo(2, 3);
    Bitmap.Canvas.LineTo(6, 3);

    Bitmap.Canvas.Pen.Color := Foreground2;
    Bitmap.Canvas.MoveTo(8, 3);
    Bitmap.Canvas.LineTo(Width - 2, 3);

    Bitmap.Canvas.Pen.Color := Foreground1;
    Bitmap.Canvas.MoveTo(2, 6);
    Bitmap.Canvas.LineTo(5, 6);

    Bitmap.Canvas.Pen.Color := Foreground2;
    Bitmap.Canvas.MoveTo(2, 9);
    Bitmap.Canvas.LineTo(Width - 2, 9);

    Bitmap.Canvas.Pen.Color := Foreground2;
    Bitmap.Canvas.MoveTo(2, 12);
    Bitmap.Canvas.LineTo(Width - 2, 12);
  end;

begin
  inherited;
  if not TDirectory.Exists(FPath) then
    exit;

  FListview.SmallImages := nil;
  FImageList.Clear;
  SysUtils.ForceDirectories(IncludeTrailingPathDelimiter(FPath) + 'Images');
  CoInitialize(nil);
  try
    for i := 0 to FListview.Items.Count - 1 do
    begin
      Item := FListview.Items.Item[i];
      ImageName := IncludeTrailingPathDelimiter(FPath) + 'Images\' + Item.Caption + '.bmp';

      LBitmap := TBitmap.Create;
      try
        CreateThumbnail := True;
        if FileExists(ImageName) then
        begin
          LBitmap.LoadFromFile(ImageName);
          if (LBitmap.Width = FImageList.Width) and (LBitmap.Height = FImageList.Height) then
            CreateThumbnail := False;
        end;

        if CreateThumbnail then
        begin
          FileName := IncludeTrailingPathDelimiter(FPath) + Item.Caption + '.theme.xml';
          LoadThemeFromXMLFile(ImpTheme, FileName);
          CreateThemeBmp(16, 16, StringToColor(ImpTheme[ReservedWord].BackgroundColorNew),
            StringToColor(ImpTheme[ReservedWord].ForegroundColorNew), StringToColor(ImpTheme[Identifier].ForegroundColorNew), LBitmap)
        end;

        Synchronize(
          procedure
          begin
            FImageList.Add(LBitmap, nil);
            Item.ImageIndex := FImageList.Count - 1;
          end);
        if CreateThumbnail then
          LBitmap.SaveToFile(ImageName);
      finally
        LBitmap.Free;
      end;
    end;

  finally
    CoUninitialize;
    FListview.SmallImages := FImageList;
  end;
end;

end.
