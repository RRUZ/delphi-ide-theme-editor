// **************************************************************************************************
//
// Unit DITE.VSThemes
// Import visual studio themes
//
// The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
// you may not use this file except in compliance with the License. You may obtain a copy of the
// License at http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
// ANY KIND, either express or implied. See the License for the specific language governing rights
// and limitations under the License.
//
// The Original Code is VSThemes.pas.
//
// The Initial Developer of the Original Code is Rodrigo Ruz V.
// Portions created by Rodrigo Ruz V. are Copyright (C) 2011-2023 Rodrigo Ruz V.
// All Rights Reserved.
//
// **************************************************************************************************
unit DITE.VSThemes;

interface

uses
  SysUtils,
  ComObj,
  Variants,
  DITE.DelphiIDEHighlight,
  DITE.DelphiVersions;

function ImportVisualStudioTheme(ADelphiVersionData: TDelphiVersionData; const FileName, Path: TFileName; var ThemeName: string): boolean;

implementation

const
  sEmpty = 'Empty';

function ImportVisualStudioTheme(ADelphiVersionData: TDelphiVersionData; const FileName, Path: TFileName; var ThemeName: string): boolean;
var
  XmlDocVSTheme, Nodes, Categories: OleVariant;
  LNodes, LCategories: Integer;
  xPathElement, FG, BG, BoldFont,
  BGPatch, FGPatch: string;
  NewTheme: TIDETheme;

  function GetDataVSTheme(const ElementName: String): boolean;
  var
    i: Integer;
    ElementVariant: OleVariant;
  begin
    Result := False;
    if ElementName = sEmpty then
      Exit;

    for i := 1 to LNodes do
    begin
      xPathElement := '//UserSettings/Category/Category/FontsAndColors/Categories/Category/Items[1]/Item[%d]/';
      xPathElement := Format(xPathElement, [i]);
      ElementVariant := XmlDocVSTheme.selectSingleNode(Format('%s%s', [xPathElement, '@Name']));

      if not VarIsNull(ElementVariant) and not VarIsClear(ElementVariant) then
        if CompareText(ElementVariant.text, ElementName) = 0 then
        begin
          // OutputDebugString(PChar(VarToStr(ElementVariant.text)));
          FG := XmlDocVSTheme.selectSingleNode(Format('%s%s', [xPathElement, '@Foreground'])).text;
          BG := XmlDocVSTheme.selectSingleNode(Format('%s%s', [xPathElement, '@Background'])).text;
          BoldFont := XmlDocVSTheme.selectSingleNode(Format('%s%s', [xPathElement, '@BoldFont'])).text;
          Result := True;
          Break;
        end;
    end;
  end;

  Procedure SetIDEHighlightElement(Element: TIDEHighlightElements; const VsElement: String);
  var
    UseBGPatch, UseFGatch: boolean;
  begin

    if GetDataVSTheme(VsElement) then
    begin
      UseBGPatch := (BG = '0x02000000');
      UseFGatch := (FG = '0x02000000');

      NewTheme[Element].Bold := BoldFont = 'Yes';
      NewTheme[Element].Italic := False;
      NewTheme[Element].Underline := False;
      NewTheme[Element].DefaultForeground := False;
      NewTheme[Element].DefaultBackground := False;
      // NewTheme[Element].ForegroundColorNew:=StringReplace(Foreground,'0x','$',[rfReplaceAll]);

      if UseBGPatch then
        NewTheme[Element].BackgroundColorNew := BGPatch
      else
        NewTheme[Element].BackgroundColorNew := StringReplace(BG, '0x', '$', [rfReplaceAll]);

      if UseFGatch then
        NewTheme[Element].ForegroundColorNew := FGPatch
      else
        NewTheme[Element].ForegroundColorNew := StringReplace(FG, '0x', '$', [rfReplaceAll]);

    end
    else
    begin
      {
        NewTheme[Element].Bold:=False;
        NewTheme[Element].Italic:=False;
        NewTheme[Element].Underline:=False;
        NewTheme[Element].DefaultForeground:=False;
        NewTheme[Element].DefaultBackground:=False;
        NewTheme[Element].ForegroundColorNew:='clDefault';
        NewTheme[Element].BackgroundColorNew:=BackgroundPatch;
      }
    end;
  end;

begin
  Result := False;
  NewTheme := GetDelphiIDEDefaultTheme(ADelphiVersionData);
  XmlDocVSTheme := CreateOleObject(Msxml2_DOMDocument);
  XmlDocVSTheme.Async := False;
  try
    XmlDocVSTheme.Load(FileName);
    XmlDocVSTheme.SetProperty('SelectionLanguage', 'XPath');
    if (XmlDocVSTheme.parseError.errorCode <> 0) then
      raise Exception.CreateFmt('Error in Visual Studio Xml Data %s', [XmlDocVSTheme.parseError]);

    Categories := XmlDocVSTheme.selectNodes('//UserSettings/Category/Category/FontsAndColors/Categories/Category');
    LCategories := Categories.Length;
    if LCategories > 1 then
      raise Exception.CreateFmt('Visual Studio theme with multiples Categories %s', ['is not supported']);

    // UserSettings/Category/Category/FontsAndColors/Categories/Category/Items
    Nodes := XmlDocVSTheme.selectNodes('//UserSettings/Category/Category/FontsAndColors/Categories/Category/Items[1]/Item');
    LNodes := Nodes.Length;

    GetDataVSTheme('Plain Text');
    BGPatch := StringReplace(BG, '0x', '$', [rfReplaceAll]);
    FGPatch := StringReplace(FG, '0x', '$', [rfReplaceAll]);

    SetIDEHighlightElement(TIDEHighlightElements.AdditionalSearchMatchHighlight, 'Selected Text');
    NewTheme[TIDEHighlightElements.AdditionalSearchMatchHighlight].DefaultForeground := True;

    SetIDEHighlightElement(TIDEHighlightElements.Assembler, 'Script Keyword'); // sEmpty);
    SetIDEHighlightElement(TIDEHighlightElements.AttributeNames, 'HTML Attribute');
    SetIDEHighlightElement(TIDEHighlightElements.AttributeValues, 'HTML Attribute Value');
    SetIDEHighlightElement(TIDEHighlightElements.BraceHighlight, 'Brace Matching (Rectangle)');
    SetIDEHighlightElement(TIDEHighlightElements.Character, 'String');
    // SetIDEHighlightElement(TIDEHighlightElements.CodeFoldingTree,'Collapsible Text');
    SetIDEHighlightElement(TIDEHighlightElements.CodeFoldingTree, 'Plain Text');
    SetIDEHighlightElement(TIDEHighlightElements.Comment, 'Comment');
    SetIDEHighlightElement(TIDEHighlightElements.DiffAddition, sEmpty);
    SetIDEHighlightElement(TIDEHighlightElements.DiffDeletion, sEmpty);
    SetIDEHighlightElement(TIDEHighlightElements.DiffMove, sEmpty);
    SetIDEHighlightElement(TIDEHighlightElements.DisabledBreak, sEmpty);
    SetIDEHighlightElement(TIDEHighlightElements.EnabledBreak, sEmpty);
    SetIDEHighlightElement(TIDEHighlightElements.ErrorLine, sEmpty);
    SetIDEHighlightElement(TIDEHighlightElements.ExecutionPoint, sEmpty);
    SetIDEHighlightElement(TIDEHighlightElements.Float, 'Number');
    SetIDEHighlightElement(TIDEHighlightElements.FoldedCode, 'Brace Matching (Rectangle)');
    SetIDEHighlightElement(TIDEHighlightElements.Hex, 'Number');
    SetIDEHighlightElement(TIDEHighlightElements.HotLink, 'urlformat');
    SetIDEHighlightElement(TIDEHighlightElements.Identifier, 'Plain Text'); // 'Identifier');
    SetIDEHighlightElement(TIDEHighlightElements.IllegalChar, 'Syntax Error');
    SetIDEHighlightElement(TIDEHighlightElements.InvalidBreak, sEmpty);

    SetIDEHighlightElement(TIDEHighlightElements.LineHighlight, 'Selected Text');
    NewTheme[TIDEHighlightElements.LineHighlight].DefaultForeground := True;

    // SetIDEHighlightElement(TIDEHighlightElements.LineNumber,'Line Numbers');
    SetIDEHighlightElement(TIDEHighlightElements.LineNumber, 'Plain Text');

    SetIDEHighlightElement(TIDEHighlightElements.MarkedBlock, 'Selected Text');
    NewTheme[TIDEHighlightElements.MarkedBlock].DefaultForeground := True;

    SetIDEHighlightElement(TIDEHighlightElements.ModifiedLine, sEmpty);
    SetIDEHighlightElement(TIDEHighlightElements.Number, 'Number');
    SetIDEHighlightElement(TIDEHighlightElements.Octal, 'Number');
    SetIDEHighlightElement(TIDEHighlightElements.PlainText, 'Plain Text');
    SetIDEHighlightElement(TIDEHighlightElements.Preprocessor, 'Preprocessor Keyword');
    SetIDEHighlightElement(TIDEHighlightElements.ReservedWord, 'Keyword');
    SetIDEHighlightElement(TIDEHighlightElements.RightMargin, 'Indicator Margin');
    SetIDEHighlightElement(TIDEHighlightElements.Scripts, sEmpty);
    SetIDEHighlightElement(TIDEHighlightElements.SearchMatch, 'Selected Text');
    SetIDEHighlightElement(TIDEHighlightElements.String, 'String');
    SetIDEHighlightElement(TIDEHighlightElements.Symbol, 'Plain Text'); // sEmpty);
    SetIDEHighlightElement(TIDEHighlightElements.SyncEditBackground, sEmpty);
    SetIDEHighlightElement(TIDEHighlightElements.SyncEditHighlight, sEmpty);
    SetIDEHighlightElement(TIDEHighlightElements.Tags, sEmpty);
    SetIDEHighlightElement(TIDEHighlightElements.Whitespace, 'Plain Text');
    // Doc.SaveToFile(Result);
    ThemeName := Copy(ExtractFileName(FileName), 1, Pos('.vssettings', ExtractFileName(FileName)) - 1);
    // SaveDelphiIDEThemeToXmlFile(TDelphiVersions.DelphiXE,NewTheme,Path,ThemeName);
    SaveDelphiIDEThemeToXmlFile(NewTheme, Path, ThemeName);
    Result := True;
  finally
    XmlDocVSTheme := Unassigned;
  end;
end;

end.
