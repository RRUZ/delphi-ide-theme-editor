// **************************************************************************************************
//
// Unit DITE.HelpInsight
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
// The Original Code is uHelpInsight.pas.
//
// The Initial Developer of the Original Code is Rodrigo Ruz V.
// Portions created by Rodrigo Ruz V. are Copyright (C) 2011-2023 Rodrigo Ruz V.
// All Rights Reserved.
//
// **************************************************************************************************
unit DITE.HelpInsight;

interface

uses
  DITE.DelphiIDEHighlight,
  DITE.DelphiVersions;

{$I Common.inc}

const
  HelpInsightPaths: array [TDelphiVersions] of string = (
{$IFDEF DELPHI_OLDER_VERSIONS_SUPPORT}
    '', '',
{$ENDIF}
    '',
    '\ObjRepos',
    '\ObjRepos',
    '\ObjRepos',
    '\ObjRepos',
    '\ObjRepos',
    '\ObjRepos\en', // 2010
    '\ObjRepos\en', // XE
    '\ObjRepos\en', // XE2
    '\ObjRepos\en', // XE3
    '\ObjRepos\en', // XE4
    '\ObjRepos\en', // XE5
    '\ObjRepos\en', // Appmethod
    '\ObjRepos\en', // XE6
    '\ObjRepos\en', // XE7
    '\ObjRepos\en', // XE8
    '\ObjRepos\en', // 10 Seattle
    '\ObjRepos\en', // 10.1 Berlin
    '\ObjRepos\en', // 10.2 Tokyo
    '\ObjRepos\en', // 10.3 Rio
    '\ObjRepos\en', // 10.4 Sydney
    '\ObjRepos\en', // 11.0 Alexandria
    '\ObjRepos\en'  // 12.0 Athens
    );

procedure ApplyThemeHelpInsight(const ATheme: TIDETheme; IDEData: TDelphiVersionData);
function SetHelpInsightDefault(IDEData: TDelphiVersionData): Boolean;

implementation

uses
  DITE.Misc,
  Winapi.Windows,
  Vcl.Graphics,
  Vcl.GraphUtil,
  Vcl.Imaging.GIFImg,
  Vcl.Dialogs,
  System.UITypes,
  System.SysUtils,
  System.Classes,
  System.IOUtils;

Const
  HelpInsightFont = '[%FONT%]';
  HelpInsightColor = '[%FONT_COLOR%]';
  HelpInsightCaptionColor = '[%FONT_CAPTION_COLOR%]';
  HelpInsightBack = '[%BACKGROUND_COLOR%]';

function SetHelpInsightDefault(IDEData: TDelphiVersionData): Boolean;
Var
  CssFile, GifFile: string;
begin
  Result := True;
  if HelpInsightPaths[IDEData.Version] <> '' then
  begin
    // restore HelpInsight.css
    CssFile := ExtractFileDir(ExtractFileDir(IDEData.Path)) + HelpInsightPaths[IDEData.Version] + '\HelpInsight.css';
    if (not IsUACEnabled) and (CurrentUserIsAdmin) then
      Result := CopyFile(PChar(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) +
        'HelpInsight\HelpInsight.css'), PChar(CssFile), false)
    else
    begin
      RunAsAdmin('cmd.exe', Format('/c copy /Y "%s" "%s"', [IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) +
        'HelpInsight\HelpInsight.css', CssFile]));
      Result := True;
    end;

    if Result then
    begin
      // restore HelpInsightGradient.gif
      GifFile := ExtractFileDir(ExtractFileDir(IDEData.Path)) + HelpInsightPaths[IDEData.Version] +
        '\HelpInsightGradient.gif';
      if (not IsUACEnabled) and (CurrentUserIsAdmin) then
        Result := CopyFile(PChar(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) +
          'HelpInsight\HelpInsightGradient.gif'), PChar(GifFile), false)
      else
      begin
        RunAsAdmin('cmd.exe', Format('/c copy /Y "%s" "%s"', [IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)))
          + 'HelpInsight\HelpInsightGradient.gif', GifFile]));
        Result := True;
      end;
    end;
  end;
end;

procedure ApplyThemeHelpInsight(const ATheme: TIDETheme; IDEData: TDelphiVersionData);
Var
  Css, TempCssFile, CssFile, TempGifFile, GifFile: string;
  Color: TColor;
  LBmp: TBitmap;
  LGif: TGIFImage;
  RunElevated: Boolean;
begin
  CssFile := ExtractFileDir(ExtractFileDir(IDEData.Path)) + HelpInsightPaths[IDEData.Version] + '\HelpInsight.css';
  if FileExists(CssFile) then
  begin
    // set HelpInsight .css
    Css := TFile.ReadAllText(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) +
      'HelpInsight\HelpInsight_Template.css');
    Css := StringReplace(Css, HelpInsightFont, 'Consolas', [rfReplaceAll]);
    Color := StringToColor(ATheme[TIDEHighlightElements.PlainText].ForegroundColorNew);
    Css := StringReplace(Css, HelpInsightColor, ColorToWebColorStr(Color), [rfReplaceAll]);
    Color := StringToColor(ATheme[TIDEHighlightElements.PlainText].BackgroundColorNew);
    Css := StringReplace(Css, HelpInsightBack, ColorToWebColorStr(Color), [rfReplaceAll]);

    // Color:=GetHighLightColor(StringToColor(ATheme[TIDEHighlightElements.LineHighlight].BackgroundColorNew));
    Color := StringToColor(ATheme[TIDEHighlightElements.MarkedBlock].ForegroundColorNew);
    Css := StringReplace(Css, HelpInsightCaptionColor, ColorToWebColorStr(Color), [rfReplaceAll]);

    RunElevated := IsUACEnabled and not CurrentUserIsAdmin;
    if not RunElevated then
      TFile.WriteAllText(CssFile, Css)
    else
    begin
      TempCssFile := IncludeTrailingPathDelimiter(GetTempDirectory) + 'HelpInsight.css';
      TFile.WriteAllText(TempCssFile, Css);
      RunAsAdmin('cmd.exe', Format('/c copy /Y "%s" "%s"', [TempCssFile, CssFile]));
    end;

    // create  HelpInsightGradient.gif
    // Color:=StringToColor(ATheme[TIDEHighlightElements.LineHighlight].BackgroundColorNew);

    Color := StringToColor(ATheme[TIDEHighlightElements.MarkedBlock].BackgroundColorNew);
    LBmp := TBitmap.Create;
    try
      LBmp.PixelFormat := pf24bit;
      LBmp.Width := 1;
      LBmp.Height := 24;
      LBmp.Canvas.Brush.Color := Color;
      LBmp.Canvas.FillRect(Rect(0, 0, LBmp.Width, LBmp.Height));
      LGif := TGIFImage.Create;
      try
        TempGifFile := IncludeTrailingPathDelimiter(GetTempDirectory) + 'HelpInsightGradient.gif';
        GifFile := ExtractFileDir(ExtractFileDir(IDEData.Path)) + HelpInsightPaths[IDEData.Version] +
          '\HelpInsightGradient.gif';

        LGif.Assign(LBmp);
        if not RunElevated then
          LGif.SaveToFile(GifFile)
        else
        begin

          LGif.SaveToFile(TempGifFile);
          // RunAsAdmin('cmd.exe', Format('/c copy /Y "%s" "%s" & pause',[TempGifFile, GifFile]));
          RunAsAdmin('cmd.exe', Format('/c copy /Y "%s" "%s"', [TempGifFile, GifFile]));
        end;
      finally
        LGif.Free;
      end;
    finally
      LBmp.Free;
    end;
  end;
end;

end.
