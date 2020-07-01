// **************************************************************************************************
//
// Unit DITE.DelphiIDEHighlight
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
// The Original Code is uDelphiIDEHighlight.pas.
//
// The Initial Developer of the Original Code is Rodrigo Ruz V.
// Portions created by Rodrigo Ruz V. are Copyright (C) 2011-2020 Rodrigo Ruz V.
// All Rights Reserved.
//
// **************************************************************************************************

unit DITE.DelphiIDEHighlight;

interface

uses
  SysUtils,
  Classes,
  ComCtrls,
  UITypes,
  DITE.DelphiVersions;

{$I Common.inc}

type
  TIDEHighlightElements = (AdditionalSearchMatchHighlight, Assembler, AttributeNames, AttributeValues, BraceHighlight, Character,
    CodeFoldingTree, Comment, DiffAddition, DiffDeletion, DiffMove, DisabledBreak, EnabledBreak, ErrorLine, ExecutionPoint, Float,
    FoldedCode, Hex, HotLink, Identifier, IllegalChar, InvalidBreak, LineHighlight, LineNumber, MarkedBlock, ModifiedLine, Number, Octal,
    PlainText, Preprocessor, ReservedWord, RightMargin, Scripts, SearchMatch, &String, Symbol, SyncEditBackground, SyncEditHighlight, Tags,
    Whitespace);

  TIDEHighlightElementsAttributes = (Bold, Italic, Underline, DefaultForeground, DefaultBackground, ForegroundColorNew, BackgroundColorNew);

  TIDEImportThemes = (VisualStudioThemes, EclipseTheme);

  TItemIDEHighlightElementsAttributes = record
    Bold: Boolean;
    Italic: Boolean;
    Underline: Boolean;
    DefaultForeground: Boolean;
    DefaultBackground: Boolean;
    ForegroundColorNew: String;
    BackgroundColorNew: String;
  end;

  TIDETheme = Array [TIDEHighlightElements] of TItemIDEHighlightElementsAttributes;

const
  IDEHighlightElementsMinVersion: array [TIDEHighlightElements] of Double = (20, // AdditionalSearchMatchHighlight,
    13, // Assembler,
    13, // AttributeNames,
    13, // AttributeValues,
    17, // BraceHighlight,
    13, // Character,
    17, // CodeFoldingTree,
    13, // Comment,
    17, // DiffAddition,
    17, // DiffDeletion,
    17, // DiffMove,
    13, // DisabledBreak,
    13, // EnabledBreak,
    13, // ErrorLine,
    13, // ExecutionPoint,
    13, // Float,
    17, // FoldedCode,
    13, // Hex,
    13, // HotLink,
    13, // Identifier,
    13, // IllegalChar,
    13, // InvalidBreak,
    17, // LineHighlight,
    17, // LineNumber,
    13, // MarkedBlock,
    18, // ModifiedLine,
    13, // Number,
    13, // Octal,
    13, // PlainText,
    13, // Preprocessor,
    13, // ReservedWord,
    13, // RightMargin,
    13, // Scripts,
    13, // SearchMatch,
    13, // &String,
    13, // Symbol,
    17, // SyncEditBackground,
    17, // SyncEditHighlight,
    13, // Tags,
    13 // Whitespace
    );

  IDEHighlightElementsNames: array [TIDEHighlightElements] of string = ('Additional search match highlight', 'Assembler', 'Attribute Names',
    'Attribute Values', 'Brace Highlight', 'Character', 'Code folding tree', 'Comment', 'Diff addition', 'Diff deletion', 'Diff move',
    'Disabled break', 'Enabled break', 'Error line', 'Execution point', 'Float', 'Folded code', 'Hex', 'Hot Link', 'Identifier',
    'Illegal Char', 'Invalid break', 'Line Highlight', 'Line Number', 'Marked block', 'Modified line', 'Number', 'Octal', 'Plain text',
    'Preprocessor', 'Reserved word', 'Right margin', 'Scripts', 'Search match', 'String', 'Symbol', 'Sync edit background',
    'Sync edit highlight', 'Tags', 'Whitespace');

  IDEHighlightElementsAttributesNames: array [TIDEHighlightElementsAttributes] of string = ('Bold', 'Italic', 'Underline',
    'Default Foreground', 'Default Background', 'Foreground Color New', 'Background Color New');

  IDEImportThemesNames: array [TIDEImportThemes] of string = (
    'Import Visual Studio Themes (vssettings)',
    'Import Eclipse Themes (Eclipse Color Theme Plugin)');

  IDEImportThemesExt: array [TIDEImportThemes] of string = ('*.vssettings', '*.xml');

  IDEImportThemesDialogFilter: array [TIDEImportThemes] of string = ('Visual Studio Configuration File|*.vssettings',
    'Eclipse Theme (Eclipse Color Theme Plugin)|*.xml');

  Msxml2_DOMDocument = 'Msxml2.DOMDocument.6.0';

type
  TModernTheme = class
  private
    FFontSize: Integer;
    FFontName: string;
    FMainToolBarColor: string;
    FDelphiVersionData: TDelphiVersionData;
    FMainToolBarTColor: TColor;
    function GetHasDefaultValues: Boolean;
    procedure LoadDefaults;
    procedure SetMainToolBarColor(const Value: string);
    function GetDelphiVersion: TDelphiVersions;
  public
    property FontName: string read FFontName write FFontName;
    property FontSize: Integer read FFontSize write FFontSize;
    property MainToolBarColor: string read FMainToolBarColor write SetMainToolBarColor;
    property MainToolBarTColor: TColor read FMainToolBarTColor;
    property Version: TDelphiVersions read GetDelphiVersion;
    property HasDefaultValues: Boolean read GetHasDefaultValues;
    procedure LoadData;
    procedure RestoreData;
    procedure WriteData;
    constructor Create(ADelphiVersionData: TDelphiVersionData);
    destructor Destroy; override;
  end;

  function GetForegroundColor(ADelphiVersionData: TDelphiVersionData; Element: TIDEHighlightElements): string;
  function GetBackgroundColor(ADelphiVersionData: TDelphiVersionData; Element: TIDEHighlightElements): string;
  function GetBoldValue(ADelphiVersionData: TDelphiVersionData; Element: TIDEHighlightElements): Boolean;
  function GetItalicValue(ADelphiVersionData: TDelphiVersionData; Element: TIDEHighlightElements): Boolean;
  function GetUnderLineValue(ADelphiVersionData: TDelphiVersionData; Element: TIDEHighlightElements): Boolean;
  function GetDefaultForegroundValue(ADelphiVersionData: TDelphiVersionData; Element: TIDEHighlightElements): Boolean;
  function GetDefaultBackgroundValue(ADelphiVersionData: TDelphiVersionData; Element: TIDEHighlightElements): Boolean;

  procedure FillListAvailableElements(ADelphiVersionData: TDelphiVersionData; List: TStrings);
  procedure FillListIDEThemesImport(List: TStrings);

  procedure ImportDelphiIDEThemeFromReg(var ATheme: TIDETheme; ADelphiVersionData: TDelphiVersionData; LoadDefaults: Boolean = True);
  procedure ImportDelphiIDEThemeFromRegExt(var ATheme: TIDETheme; ADelphiVersionData: TDelphiVersionData); // internal use only
  function ImportDelphiIDEThemeToRegistry(DelphiVersion: TDelphiVersionData; const IDETheme: TIDETheme; const ThemeName: string): Boolean;

  function GetDelphiIDEFontName(ADelphiVersionData: TDelphiVersionData): string;
  function GetDelphiIDEFontSize(ADelphiVersionData: TDelphiVersionData): Integer;
  function GetDelphiIDEThemeName(ADelphiVersionData: TDelphiVersionData): string;
  function SetDelphiIDEFont(ADelphiVersionData: TDelphiVersionData; const FontName: String; FontSize: Integer): Boolean;
  function SaveDelphiIDEThemeToRegFile(ADelphiVersionData : TDelphiVersionData; const ATheme: TIDETheme; Path, Name: string): TFileName;
  function SaveDelphiIDEThemeToXmlFile(const ATheme: TIDETheme; const Path, Name: string): TFileName;

  function LoadThemeFromXMLFile(var ATheme: TIDETheme; const FileName: TFileName): Boolean;
  function SetDelphiIDEDefaultTheme(ADelphiVersionData: TDelphiVersionData): Boolean;
  function ApplyDelphiIDETheme(DelphiVersion: TDelphiVersionData; const ATheme: TIDETheme; const ThemeName: string): Boolean;
  function GetDelphiIDEDefaultTheme(ADelphiVersionData: TDelphiVersionData): TIDETheme;

  function ExistDelphiIDEThemeToImport(ADelphiVersionData: TDelphiVersionData): Boolean;
  function IsValidDelphiIDETheme(ATheme: TIDETheme): Boolean;

implementation

uses
  Forms,
  Graphics,
  ShellAPI,
  IOUtils,
  ComObj,
  Variants,
  TypInfo,
  XMLDoc,
  XMLIntf,
  Windows,
  Registry,
  DITE.Misc,
  DITE.Registry;

{$R DefaultThemes.RES}

function ExistDelphiIDEThemeToImport(ADelphiVersionData: TDelphiVersionData): Boolean;
begin
  Result := RegKeyExists(ADelphiVersionData.RegKey + '\Editor\Highlight\Assembler', HKEY_CURRENT_USER);
end;

procedure FillListIDEThemesImport(List: TStrings);
var
  IDETheme: TIDEImportThemes;
begin
  List.BeginUpdate;
  try
    List.Clear;
    for IDETheme in [Low(TIDEImportThemes) .. High(TIDEImportThemes)] do
      List.AddObject(IDEImportThemesNames[IDETheme], TObject(Ord(IDETheme)));
  finally
    List.EndUpdate;
  end;
end;

function ApplyDelphiIDETheme(DelphiVersion: TDelphiVersionData; const ATheme: TIDETheme; const ThemeName: string): Boolean;
begin
  Result := ImportDelphiIDEThemeToRegistry(DelphiVersion, ATheme, ThemeName);
  if Result then
  begin
    Result := RegWriteStr(Format('%s\Editor\DITE', [DelphiVersion.RegKey]), 'ThemeName', ThemeName, HKEY_CURRENT_USER);

    if DelphiVersionNumbers[DelphiVersion.Version] >= DelphiVersionNumbers[TDelphiVersions.Delphi10Sydney] then
    begin
      Result := RegWriteStr(Format('%s\Theme', [DelphiVersion.RegKey]), 'DarkSpeedSetting', ThemeName, HKEY_CURRENT_USER);
      if Result then
        Result := RegWriteStr(Format('%s\Theme', [DelphiVersion.RegKey]), 'LightSpeedSetting', ThemeName, HKEY_CURRENT_USER);
    end;
  end;
end;

function SetDelphiIDEDefaultTheme(ADelphiVersionData: TDelphiVersionData): Boolean;
var
  AStream: TResourceStream;
  FStream: TFileStream;
  FileName: TFileName;
  ResName: String;
begin
  {
    Delphi5 RCDATA 5.reg
    Delphi6 RCDATA 6.reg
    Delphi7 RCDATA 7.reg
    Delphi2005 RCDATA 2005.reg
    Delphi2006 RCDATA 2006.reg
    Delphi2007 RCDATA 2007.reg
    Delphi2009 RCDATA 2009.reg
    Delphi2010 RCDATA 2010.reg
    DelphiXE  RCDATA XE.reg
    DelphiXE2 RCDATA XE2.reg
    DelphiXE3 RCDATA XE3.reg
  }

  // Result:=False;
  ResName := GetEnumName(TypeInfo(TDelphiVersions), Integer(ADelphiVersionData.Version));
  FileName := IncludeTrailingPathDelimiter(GetTempDirectory) + 'Dummy.reg';
  if TFile.Exists(FileName) then
    TFile.Delete(FileName);
  AStream := TResourceStream.Create(hInstance, ResName, RT_RCDATA);
  try
    FStream := TFileStream.Create(FileName, fmCreate);
    try
      FStream.CopyFrom(AStream, 0);
    finally
      FStream.Free;
    end;

    Result := FileExists(FileName) and RunAndWait(0, 'regedit.exe', '/S "' + FileName + '"', IsUACEnabled or not CurrentUserIsAdmin);
    if Result then
      RegWriteStr(Format('%s\Editor\DITE', [ADelphiVersionData.RegKey]), 'ThemeName', 'default', HKEY_CURRENT_USER);
  finally
    // TFile.Delete(FileName);
    AStream.Free;
  end;
end;

function GetDelphiIDEDefaultTheme(ADelphiVersionData: TDelphiVersionData): TIDETheme;
var
  AStream: TResourceStream;
  FStream: TFileStream;
  FileName: TFileName;
  ResName: String;
  RegFile: TStrings;
begin
  ResName := GetEnumName(TypeInfo(TDelphiVersions), Integer(ADelphiVersionData.Version));
  FileName := IncludeTrailingPathDelimiter(GetTempDirectory) + 'Dummy.reg';
  if TFile.Exists(FileName) then
    TFile.Delete(FileName);
  AStream := TResourceStream.Create(hInstance, ResName, RT_RCDATA);
  try
    RegFile := TStringList.Create;
    try
      FStream := TFileStream.Create(FileName, fmCreate);
      try
        FStream.CopyFrom(AStream, 0);
      finally
        FStream.Free;
      end;

      RegFile.LoadFromFile(FileName, TEncoding.Unicode);
      RegFile.Text := StringReplace(RegFile.Text, '\Editor\Highlight', '\Editor\DelphiTheme\Highlight', [rfReplaceAll]);
      RegFile.SaveToFile(FileName, TEncoding.Unicode);
      RunAndWait(0, 'regedit.exe', '/S "' + FileName + '"', IsUACEnabled or not CurrentUserIsAdmin);
      ImportDelphiIDEThemeFromRegExt(Result, ADelphiVersionData);
    finally
      if TFile.Exists(FileName) then
        TFile.Delete(FileName);
      RegFile.Free;
    end;
  finally
    AStream.Free;
  end;
end;

function IsValidDelphiIDETheme(ATheme: TIDETheme): Boolean;
var
  Element: TIDEHighlightElements;
begin
  // Result:=True;
  for Element in [Low(TIDEHighlightElements) .. High(TIDEHighlightElements)] do
  begin
    Result := (Trim(ATheme[Element].ForegroundColorNew) <> '') and (Trim(ATheme[Element].BackgroundColorNew) <> '');
    if not Result then
      Break;
  end;
end;

function LoadThemeFromXMLFile(var ATheme: TIDETheme; const FileName: TFileName): Boolean;
var
  XmlDocIDETheme: OleVariant;
  xPathElement: string;
  Element: TIDEHighlightElements;
  ElementName: string;
begin
  Result := False;
  XmlDocIDETheme := CreateOleObject(Msxml2_DOMDocument);
  XmlDocIDETheme.Async := False;
  try
    // XmlDocIDETheme.LoadXML(TFile.ReadAllText(FileName));
    XmlDocIDETheme.Load(FileName);
    XmlDocIDETheme.SetProperty('SelectionLanguage', 'XPath');
    if (XmlDocIDETheme.parseError.errorCode <> 0) then
      raise Exception.CreateFmt('Error in Delphi theme Xml Data %s', [XmlDocIDETheme.parseError]);

    for Element in [Low(TIDEHighlightElements) .. High(TIDEHighlightElements)] do
    begin
      ElementName := GetEnumName(TypeInfo(TIDEHighlightElements), Integer(Element));
      // /DelphiIDETheme/AdditionalSearchMatchHighlight/Bold
      xPathElement := Format('//DelphiIDETheme/%s/', [ElementName]);
      ATheme[Element].Bold := CompareText(XmlDocIDETheme.selectSingleNode(Format('%s%s', [xPathElement, 'Bold'])).Text, 'True') = 0;
      ATheme[Element].Italic := CompareText(XmlDocIDETheme.selectSingleNode(Format('%s%s', [xPathElement, 'Italic'])).Text, 'True') = 0;
      ATheme[Element].Underline := CompareText(XmlDocIDETheme.selectSingleNode(Format('%s%s', [xPathElement, 'Underline'])).Text,
        'True') = 0;
      ATheme[Element].DefaultForeground := CompareText(XmlDocIDETheme.selectSingleNode(Format('%s%s', [xPathElement, 'DefaultForeground']))
        .Text, 'True') = 0;
      ATheme[Element].DefaultBackground := CompareText(XmlDocIDETheme.selectSingleNode(Format('%s%s', [xPathElement, 'DefaultBackground']))
        .Text, 'True') = 0;

      ATheme[Element].ForegroundColorNew := XmlDocIDETheme.selectSingleNode(Format('%s%s', [xPathElement, 'ForegroundColorNew'])).Text;
      ATheme[Element].BackgroundColorNew := XmlDocIDETheme.selectSingleNode(Format('%s%s', [xPathElement, 'BackgroundColorNew'])).Text;
    end;

    Result := True;
  finally
    XmlDocIDETheme := Unassigned;
  end;
end;

function SaveDelphiIDEThemeToXmlFile(const ATheme: TIDETheme; const Path, Name: string): TFileName;
var
  Element: TIDEHighlightElements;
  Doc: TXMLDocument;
  RootNode, ChildNode, oNode: IXMLNode;
begin
  Result := '';
  Doc := TXMLDocument.Create(nil);
  Doc.Active := True;
  Doc.Version := '1.0';
  Doc.Encoding := 'utf-8';
  Doc.Options := [doNodeAutoIndent];
  RootNode := Doc.AddChild('DelphiIDETheme');

  // <colorTheme id="1" name="Oblivion" modified="2011-02-11 14:35:49" author="Roger Dudler" website="http://www.rogerdudler.com/?p=362">
  RootNode.Attributes['modified'] := FormatDateTime('YYYY-MM-DD HH:NN:SS', Now);
  RootNode.Attributes['author'] := 'Delphi IDE Theme Editor';
  RootNode.Attributes['versionapp'] := GetFileVersion(ParamStr(0));

  for Element in [Low(TIDEHighlightElements) .. High(TIDEHighlightElements)] do
  begin
    ChildNode := RootNode.AddChild(GetEnumName(TypeInfo(TIDEHighlightElements), Integer(Element)));
    oNode := ChildNode.AddChild('Bold');
    oNode.Text := BoolToStr(ATheme[Element].Bold, True);
    oNode := ChildNode.AddChild('Italic');
    oNode.Text := BoolToStr(ATheme[Element].Italic, True);
    oNode := ChildNode.AddChild('Underline');
    oNode.Text := BoolToStr(ATheme[Element].Underline, True);
    oNode := ChildNode.AddChild('DefaultForeground');
    oNode.Text := BoolToStr(ATheme[Element].DefaultForeground, True);
    oNode := ChildNode.AddChild('DefaultBackground');
    oNode.Text := BoolToStr(ATheme[Element].DefaultBackground, True);
    oNode := ChildNode.AddChild('ForegroundColorNew');
    oNode.Text := ATheme[Element].ForegroundColorNew;
    oNode := ChildNode.AddChild('BackgroundColorNew');
    oNode.Text := ATheme[Element].BackgroundColorNew;
    {
      ChildNode := RootNode.AddChild(GetEnumName(TypeInfo(TIDEHighlightElements),integer(Element)));
      ChildNode.Attributes['Bold'] := BoolToStr(ATheme[Element].Bold,True);
      ChildNode.Attributes['Italic'] := BoolToStr(ATheme[Element].Italic,True);
      ChildNode.Attributes['Underline'] := BoolToStr(ATheme[Element].Underline,True);
      ChildNode.Attributes['DefaultForeground'] := BoolToStr(ATheme[Element].DefaultForeground,True);
      ChildNode.Attributes['DefaultBackground'] := BoolToStr(ATheme[Element].DefaultBackground,True);
    }
  end;
  ForceDirectories(Path);
  Result := Format('%s%s.theme.xml', [IncludeTrailingPathDelimiter(Path), Name]);
  Doc.SaveToFile(Result);

end;

procedure FillListAvailableElements(ADelphiVersionData: TDelphiVersionData; List: TStrings);
var
  Element: TIDEHighlightElements;
begin
  List.BeginUpdate;
  try
    List.Clear;
    for Element in [Low(TIDEHighlightElements) .. High(TIDEHighlightElements)] do
      if DelphiVersionNumbers[ADelphiVersionData.Version] >= IDEHighlightElementsMinVersion[Element] then
        List.AddObject(IDEHighlightElementsNames[Element], TObject(Ord(Element)));
  finally
    List.EndUpdate;
  end;
end;

function GetAtributeValue(ADelphiVersionData: TDelphiVersionData; Element: TIDEHighlightElements; const Atribute: String): string;
begin
  if not RegReadStr(Format('%s\Editor\Highlight\%s', [ADelphiVersionData.RegKey, IDEHighlightElementsNames[Element]]), Atribute, Result,
    HKEY_CURRENT_USER) then
    Result := '';
end;

function GetAtributeValueExt(ADelphiVersionData: TDelphiVersionData; Element: TIDEHighlightElements; const Atribute: String): string;
begin
  if not RegReadStr(Format('%s\Editor\DelphiTheme\Highlight\%s', [ADelphiVersionData.RegKey, IDEHighlightElementsNames[Element]]),
    Atribute, Result, HKEY_CURRENT_USER) then
    Result := '';
end;

function GetForegroundColor(ADelphiVersionData: TDelphiVersionData; Element: TIDEHighlightElements): string;
begin
  Result := GetAtributeValue(ADelphiVersionData, Element, IDEHighlightElementsAttributesNames
    [TIDEHighlightElementsAttributes.ForegroundColorNew]);
end;

function GetBackgroundColor(ADelphiVersionData: TDelphiVersionData; Element: TIDEHighlightElements): string;
begin
  Result := GetAtributeValue(ADelphiVersionData, Element, IDEHighlightElementsAttributesNames
    [TIDEHighlightElementsAttributes.BackgroundColorNew]);
end;

function GetBoldValue(ADelphiVersionData: TDelphiVersionData; Element: TIDEHighlightElements): Boolean;
begin
  Result := CompareText(GetAtributeValue(ADelphiVersionData, Element, IDEHighlightElementsAttributesNames[TIDEHighlightElementsAttributes.Bold]),
    'True') = 0;
end;

function GetItalicValue(ADelphiVersionData: TDelphiVersionData; Element: TIDEHighlightElements): Boolean;
begin
  Result := CompareText(GetAtributeValue(ADelphiVersionData, Element, IDEHighlightElementsAttributesNames[TIDEHighlightElementsAttributes.Italic]
    ), 'True') = 0;
end;

function GetUnderLineValue(ADelphiVersionData: TDelphiVersionData; Element: TIDEHighlightElements): Boolean;
begin
  Result := CompareText(GetAtributeValue(ADelphiVersionData, Element,
    IDEHighlightElementsAttributesNames[TIDEHighlightElementsAttributes.Underline]), 'True') = 0;
end;

function GetDefaultForegroundValue(ADelphiVersionData: TDelphiVersionData; Element: TIDEHighlightElements): Boolean;
begin
  Result := CompareText(GetAtributeValue(ADelphiVersionData, Element,
    IDEHighlightElementsAttributesNames[TIDEHighlightElementsAttributes.DefaultForeground]), 'True') = 0;
end;

function GetDefaultBackgroundValue(ADelphiVersionData: TDelphiVersionData; Element: TIDEHighlightElements): Boolean;
begin
  Result := CompareText(GetAtributeValue(ADelphiVersionData, Element,
    IDEHighlightElementsAttributesNames[TIDEHighlightElementsAttributes.DefaultBackground]), 'True') = 0;
end;

function GetForegroundColorExt(ADelphiVersionData: TDelphiVersionData; Element: TIDEHighlightElements): string;
begin
  Result := GetAtributeValueExt(ADelphiVersionData, Element,
    IDEHighlightElementsAttributesNames[TIDEHighlightElementsAttributes.ForegroundColorNew]);
end;

function GetBackgroundColorExt(ADelphiVersionData: TDelphiVersionData; Element: TIDEHighlightElements): string;
begin
  Result := GetAtributeValueExt(ADelphiVersionData, Element,
    IDEHighlightElementsAttributesNames[TIDEHighlightElementsAttributes.BackgroundColorNew]);
end;

function GetBoldValueExt(ADelphiVersionData: TDelphiVersionData; Element: TIDEHighlightElements): Boolean;
begin
  Result := CompareText(GetAtributeValueExt(ADelphiVersionData, Element,
    IDEHighlightElementsAttributesNames[TIDEHighlightElementsAttributes.Bold]), 'True') = 0;
end;

function GetItalicValueExt(ADelphiVersionData: TDelphiVersionData; Element: TIDEHighlightElements): Boolean;
begin
  Result := CompareText(GetAtributeValueExt(ADelphiVersionData, Element,
    IDEHighlightElementsAttributesNames[TIDEHighlightElementsAttributes.Italic]), 'True') = 0;
end;

function GetUnderLineValueExt(ADelphiVersionData: TDelphiVersionData; Element: TIDEHighlightElements): Boolean;
begin
  Result := CompareText(GetAtributeValueExt(ADelphiVersionData, Element,
    IDEHighlightElementsAttributesNames[TIDEHighlightElementsAttributes.Underline]), 'True') = 0;
end;

function GetDefaultForegroundValueExt(ADelphiVersionData: TDelphiVersionData; Element: TIDEHighlightElements): Boolean;
begin
  Result := CompareText(GetAtributeValueExt(ADelphiVersionData, Element,
    IDEHighlightElementsAttributesNames[TIDEHighlightElementsAttributes.DefaultForeground]), 'True') = 0;
end;

function GetDefaultBackgroundValueExt(ADelphiVersionData: TDelphiVersionData; Element: TIDEHighlightElements): Boolean;
begin
  Result := CompareText(GetAtributeValueExt(ADelphiVersionData, Element,
    IDEHighlightElementsAttributesNames[TIDEHighlightElementsAttributes.DefaultBackground]), 'True') = 0;
end;

procedure ImportDelphiIDEThemeFromReg(var ATheme: TIDETheme; ADelphiVersionData: TDelphiVersionData; LoadDefaults: Boolean = True);
var
  Element: TIDEHighlightElements;
  DefaultTheme: TIDETheme;
begin
  if LoadDefaults then
    LoadThemeFromXMLFile(DefaultTheme, ExtractFilePath(ParamStr(0)) + 'default\Default.theme.xml');

  for Element in [Low(TIDEHighlightElements) .. High(TIDEHighlightElements)] do
  begin
    if DelphiVersionNumbers[ADelphiVersionData.Version] >= IDEHighlightElementsMinVersion[Element] then
    begin
      ATheme[Element].Bold := GetBoldValue(ADelphiVersionData, Element);
      ATheme[Element].Italic := GetItalicValue(ADelphiVersionData, Element);
      ATheme[Element].Underline := GetUnderLineValue(ADelphiVersionData, Element);
      ATheme[Element].DefaultForeground := GetDefaultForegroundValue(ADelphiVersionData, Element);
      ATheme[Element].DefaultBackground := GetDefaultBackgroundValue(ADelphiVersionData, Element);
      ATheme[Element].ForegroundColorNew := GetForegroundColor(ADelphiVersionData, Element);
      ATheme[Element].BackgroundColorNew := GetBackgroundColor(ADelphiVersionData, Element);
    end
    else
      // if the values are not present in the current theme  then
      // load the default values from default theme template (based on XE)
      if LoadDefaults then
      begin
        ATheme[Element].Bold := DefaultTheme[Element].Bold;
        ATheme[Element].Italic := DefaultTheme[Element].Italic;
        ATheme[Element].Underline := DefaultTheme[Element].Underline;
        ATheme[Element].DefaultForeground := DefaultTheme[Element].DefaultForeground;
        ATheme[Element].DefaultBackground := DefaultTheme[Element].DefaultBackground;
        ATheme[Element].ForegroundColorNew := DefaultTheme[Element].ForegroundColorNew;
        ATheme[Element].BackgroundColorNew := DefaultTheme[Element].BackgroundColorNew;
      end;
  end;
end;

procedure ImportDelphiIDEThemeFromRegExt(var ATheme: TIDETheme; ADelphiVersionData: TDelphiVersionData);
var
  Element: TIDEHighlightElements;
begin
  for Element in [Low(TIDEHighlightElements) .. High(TIDEHighlightElements)] do
  begin
    if DelphiVersionNumbers[ADelphiVersionData.Version] >= IDEHighlightElementsMinVersion[Element] then
    begin
      ATheme[Element].Bold := GetBoldValueExt(ADelphiVersionData, Element);
      ATheme[Element].Italic := GetItalicValueExt(ADelphiVersionData, Element);
      ATheme[Element].Underline := GetUnderLineValueExt(ADelphiVersionData, Element);
      ATheme[Element].DefaultForeground := GetDefaultForegroundValueExt(ADelphiVersionData, Element);
      ATheme[Element].DefaultBackground := GetDefaultBackgroundValueExt(ADelphiVersionData, Element);
      ATheme[Element].ForegroundColorNew := GetForegroundColorExt(ADelphiVersionData, Element);
      ATheme[Element].BackgroundColorNew := GetBackgroundColorExt(ADelphiVersionData, Element);
    end
    else
    begin
      ATheme[Element].Bold := False;
      ATheme[Element].Italic := False;
      ATheme[Element].Underline := False;
      ATheme[Element].DefaultForeground := False;
      ATheme[Element].DefaultBackground := False;
      ATheme[Element].ForegroundColorNew := 'clDefault';
      ATheme[Element].BackgroundColorNew := 'clDefault';
    end;
  end;
end;

function SaveDelphiIDEThemeToRegFile(ADelphiVersionData : TDelphiVersionData; const ATheme: TIDETheme; Path, Name: string): TFileName;
var
  Element: TIDEHighlightElements;
  RegFile: TStringList;
  {$IFDEF DELPHI_OLDER_VERSIONS_SUPPORT}
  Indexb: Integer;
  Indexf: Integer;
  {$ENDIF}
begin
  Result := '';
  RegFile := TStringList.Create;
  try
    {
      Windows Registry Editor Version 5.00
      [HKEY_CURRENT_USER\Software\Borland\Delphi\7.0\Editor\Highlight]

      [HKEY_CURRENT_USER\Software\Borland\Delphi\7.0\Editor\Highlight\Assembler]
      "Bold"="False"
      "Italic"="False"
      "Underline"="False"
      "Default Foreground"="False"
      "Default Background"="False"
      "Foreground Color New"="$001E1EE8"
      "Background Color New"="$00272727"
    }

    RegFile.Add('Windows Registry Editor Version 5.00');
    RegFile.Add('');
    RegFile.Add(Format('[HKEY_CURRENT_USER%s\Editor\Highlight]', [ADelphiVersionData.RegKey]));
    RegFile.Add('');

    for Element in [Low(TIDEHighlightElements) .. High(TIDEHighlightElements)] do
      if DelphiVersionNumbers[ADelphiVersionData.Version] >= IDEHighlightElementsMinVersion[Element] then
      begin
        RegFile.Add(Format('[HKEY_CURRENT_USER%s\Editor\Highlight\%s]', [ADelphiVersionData.RegKey, IDEHighlightElementsNames[Element]]));
        RegFile.Add(Format('"Default Foreground"="%s"', [BoolToStr(ATheme[Element].DefaultForeground, True)]));
        RegFile.Add(Format('"Default Background"="%s"', [BoolToStr(ATheme[Element].DefaultBackground, True)]));

        {$IFDEF DELPHI_OLDER_VERSIONS_SUPPORT}
        if DelphiIsOldVersion(ADelphiVersionData) then
        begin
          {
            [HKEY_CURRENT_USER\Software\Borland\Delphi\5.0\Editor\Highlight\Search match]
            "Foreground Color"=dword:0000000c
            "Background Color"=dword:00000007
            "Default Foreground"="False"
            "Default Background"="False"
          }

          Indexf := GetIndexClosestColor(StringToColor(ATheme[Element].ForegroundColorNew));
          Indexb := GetIndexClosestColor(StringToColor(ATheme[Element].BackgroundColorNew));

          RegFile.Add(Format('"Foreground Color"=dword:%x', [Indexf]));
          RegFile.Add(Format('"Background Color"=dword:%x', [Indexb]));
        end
        else
        begin
        {$ENDIF}
          RegFile.Add(Format('"Bold"="%s"', [BoolToStr(ATheme[Element].Bold, True)]));
          RegFile.Add(Format('"Italic"="%s"', [BoolToStr(ATheme[Element].Italic, True)]));
          RegFile.Add(Format('"Underline"="%s"', [BoolToStr(ATheme[Element].Underline, True)]));
          RegFile.Add(Format('"Foreground Color New"="%s"', [ATheme[Element].ForegroundColorNew]));
          RegFile.Add(Format('"Background Color New"="%s"', [ATheme[Element].BackgroundColorNew]));
        {$IFDEF DELPHI_OLDER_VERSIONS_SUPPORT}
        end;
        {$ENDIF}
        RegFile.Add('');
      end;

    Result := Format('%s%s_%s.reg', [IncludeTrailingPathDelimiter(Path), Name, DelphiVersionsNames[ADelphiVersionData.Version]]);
    RegFile.SaveToFile(Result, TEncoding.Unicode);
  finally
    RegFile.Free;
  end;
end;

function ImportDelphiIDEThemeToRegistry(DelphiVersion: TDelphiVersionData; const IDETheme: TIDETheme; const ThemeName: string): Boolean;
var
  Element: TIDEHighlightElements;
  {$IFDEF DELPHI_OLDER_VERSIONS_SUPPORT}
  Indexb, Indexf: Integer;
  {$ENDIF}
  Reg: TRegistry;
  sKey: String;
begin
  Result := False;
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    {
      Windows Registry Editor Version 5.00
      [HKEY_CURRENT_USER\Software\Borland\Delphi\7.0\Editor\Highlight]

      [HKEY_CURRENT_USER\Software\Borland\Delphi\7.0\Editor\Highlight\Assembler]
      "Bold"="False"
      "Italic"="False"
      "Underline"="False"
      "Default Foreground"="False"
      "Default Background"="False"
      "Foreground Color New"="$001E1EE8"
      "Background Color New"="$00272727"
    }
    {
      RegFile.Add('Windows Registry Editor Version 5.00');
      RegFile.Add('');
      RegFile.Add(Format('[HKEY_CURRENT_USER%s\Editor\Highlight]',[DelphiRegPaths[DelphiVersion]]));
      RegFile.Add('');
    }

    if DelphiVersionNumbers[DelphiVersion.Version] >= DelphiVersionNumbers[TDelphiVersions.Delphi10Sydney] then
      sKey := '%s\Editor\Highlight\Custom Themes\' + ThemeName + '\%s'
    else
      sKey := '%s\Editor\Highlight\%s';

    for Element in [Low(TIDEHighlightElements) .. High(TIDEHighlightElements)] do
      if DelphiVersionNumbers[DelphiVersion.Version] >= IDEHighlightElementsMinVersion[Element] then
      begin
        if Reg.OpenKey(Format(sKey, [DelphiVersion.RegKey, IDEHighlightElementsNames[Element]]), True) then
        begin
          Reg.WriteString('Default Foreground', BoolToStr(IDETheme[Element].DefaultForeground, True));
          Reg.WriteString('Default Background', BoolToStr(IDETheme[Element].DefaultBackground, True));


          // RegFile.Add(Format('[HKEY_CURRENT_USER%s\Editor\Highlight\%s]',[DelphiRegPaths[DelphiVersion],IDEHighlightElementsNames[Element]]));
          // RegFile.Add(Format('"Default Foreground"="%s"',[BoolToStr(ATheme[Element].DefaultForeground,True)]));
          // RegFile.Add(Format('"Default Background"="%s"',[BoolToStr(ATheme[Element].DefaultBackground,True)]));

          {$IFDEF DELPHI_OLDER_VERSIONS_SUPPORT}
          if DelphiIsOldVersion(ADelphiVersionData) then
          begin
            {
              [HKEY_CURRENT_USER\Software\Borland\Delphi\5.0\Editor\Highlight\Search match]
              "Foreground Color"=dword:0000000c
              "Background Color"=dword:00000007
              "Default Foreground"="False"
              "Default Background"="False"
            }

            Indexf := GetIndexClosestColor(StringToColor(ATheme[Element].ForegroundColorNew));
            Indexb := GetIndexClosestColor(StringToColor(ATheme[Element].BackgroundColorNew));
            {
              RegFile.Add(Format('"Foreground Color"=dword:%x',[Indexf]));
              RegFile.Add(Format('"Background Color"=dword:%x',[Indexb]));
            }
            Reg.WriteInteger('Foreground Color', Indexf);
            Reg.WriteInteger('Background Color', Indexb);
          end
          else
          begin
          {$ENDIF}
            Reg.WriteString('Bold', BoolToStr(IDETheme[Element].Bold, True));
            Reg.WriteString('Italic', BoolToStr(IDETheme[Element].Italic, True));
            Reg.WriteString('Underline', BoolToStr(IDETheme[Element].Underline, True));
            Reg.WriteString('Foreground Color New', IDETheme[Element].ForegroundColorNew);
            Reg.WriteString('Background Color New', IDETheme[Element].BackgroundColorNew);
            {
              RegFile.Add(Format('"Bold"="%s"',[BoolToStr(ATheme[Element].Bold,True)]));
              RegFile.Add(Format('"Italic"="%s"',[BoolToStr(ATheme[Element].Italic,True)]));
              RegFile.Add(Format('"Underline"="%s"',[BoolToStr(ATheme[Element].Underline,True)]));
              RegFile.Add(Format('"Foreground Color New"="%s"',[ATheme[Element].ForegroundColorNew]));
              RegFile.Add(Format('"Background Color New"="%s"',[ATheme[Element].BackgroundColorNew]));
            }
          {$IFDEF DELPHI_OLDER_VERSIONS_SUPPORT}
          end;
          {$ENDIF}
          Result := True;
        end;

        // RegFile.Add('');
      end;

    // Result:=Format('%s%s_%s.reg',[IncludeTrailingPathDelimiter(Path),Name,DelphiVersionsNames[DelphiVersion]]);
    // RegFile.SaveToFile(Result,TEncoding.Unicode);
  finally
    Reg.Free;
  end;
end;

function GetDelphiIDEFontName(ADelphiVersionData: TDelphiVersionData): string;
begin
  if not RegReadStr(Format('%s\Editor\Options', [ADelphiVersionData.RegKey]), 'Editor Font', Result, HKEY_CURRENT_USER) then
    Result := '';
end;

function GetDelphiIDEFontSize(ADelphiVersionData: TDelphiVersionData): Integer;
begin
  if not RegReadInt(Format('%s\Editor\Options', [ADelphiVersionData.RegKey]), 'Font Size', Result, HKEY_CURRENT_USER) then
    Result := 0;
end;

function GetDelphiIDEThemeName(ADelphiVersionData: TDelphiVersionData): string;
begin
  if not RegReadStr(Format('%s\Editor\DITE', [ADelphiVersionData.RegKey]), 'ThemeName', Result, HKEY_CURRENT_USER) then
    Result := '';
end;

function SetDelphiIDEFont(ADelphiVersionData: TDelphiVersionData; const FontName: String; FontSize: Integer): Boolean;
begin
  Result := RegWriteStr(Format('%s\Editor\Options', [ADelphiVersionData.RegKey]), 'Editor Font', FontName, HKEY_CURRENT_USER) and
    RegWriteInt(Format('%s\Editor\Options', [ADelphiVersionData.RegKey]), 'Font Size', FontSize, HKEY_CURRENT_USER);
end;

{ TModernTheme }
// http://docwiki.embarcadero.com/RADStudio/XE8/en/System_Registry_Keys_for_IDE_Visual_Settings

constructor TModernTheme.Create(ADelphiVersionData: TDelphiVersionData);
begin
  inherited Create;
  FDelphiVersionData := ADelphiVersionData;
  LoadDefaults();
end;

destructor TModernTheme.Destroy;
begin

  inherited;
end;

function TModernTheme.GetDelphiVersion: TDelphiVersions;
begin
 Result := FDelphiVersionData.Version;
end;

function TModernTheme.GetHasDefaultValues: Boolean;
var
  sKey: string;
begin
  sKey := FDelphiVersionData.RegKey + '\ModernTheme';
  Result := not RegKeyExists(sKey, HKEY_CURRENT_USER);
end;

procedure TModernTheme.LoadData;
var
  s, sKey: string;
begin
  sKey := FDelphiVersionData.RegKey + '\ModernTheme';
  if RegKeyExists(sKey, HKEY_CURRENT_USER) then
  begin
    if not RegReadStr(sKey, 'FontName', FFontName, HKEY_CURRENT_USER) then
      FFontName := 'Segoe UI';

    if RegReadStr(sKey, 'MainToolBarColor', s, HKEY_CURRENT_USER) then
      MainToolBarColor := s
    else
      MainToolBarColor := 'clGradientActiveCaption';

    if not RegReadInt(sKey, 'FontSize', FFontSize, HKEY_CURRENT_USER) then
      FFontSize := 10;
  end;
end;

procedure TModernTheme.LoadDefaults;
begin
  FFontSize := $0000000A; // 10;
  FFontName := 'Segoe UI';
  MainToolBarColor := 'clGradientActiveCaption';
end;

procedure TModernTheme.RestoreData;
var
  FileName, sKey: string;
  RegFile: TStrings;
begin
  sKey := FDelphiVersionData.RegKey + '\ModernTheme';
  if RegKeyExists(sKey, HKEY_CURRENT_USER) then
    if IsUACEnabled or not CurrentUserIsAdmin then
    begin
      FileName := IncludeTrailingPathDelimiter(GetTempDirectory) + 'Dummy.reg';
      if TFile.Exists(FileName) then
        TFile.Delete(FileName);
      RegFile := TStringList.Create;
      try
        RegFile.Add('Windows Registry Editor Version 5.00');
        RegFile.Add('');
        RegFile.Add(Format('[-HKEY_CURRENT_USER%s]', [sKey]));
        RegFile.Add('');
        RegFile.SaveToFile(FileName);
      finally
        RegFile.Free;
      end;
      RunAndWait(0, 'regedit.exe', '/S "' + FileName + '"', True);
    end
    else
      DeleteRegKey(sKey, HKEY_CURRENT_USER);
end;

procedure TModernTheme.SetMainToolBarColor(const Value: string);
begin
  FMainToolBarColor := Value;
  FMainToolBarTColor := StringToColor(Value);
end;

// Windows Registry Editor Version 5.00
// [HKEY_CURRENT_USER\Software\Embarcadero\BDS\16.0\ModernTheme]
// "FontName"="Segoe UI"
// "FontSize"=dword:0000000a
// "MainToolBarColor"="clGradientActiveCaption"
procedure TModernTheme.WriteData;
var
  FileName, sKey: string;
  RegFile: TStrings;
begin
  sKey := FDelphiVersionData.RegKey + '\ModernTheme';
  if IsUACEnabled or not CurrentUserIsAdmin then
  begin
    FileName := IncludeTrailingPathDelimiter(GetTempDirectory) + 'Dummy.reg';
    if TFile.Exists(FileName) then
      TFile.Delete(FileName);
    RegFile := TStringList.Create;
    try
      RegFile.Add('Windows Registry Editor Version 5.00');
      RegFile.Add('');
      RegFile.Add(Format('[HKEY_CURRENT_USER%s]', [sKey]));
      RegFile.Add(Format('"FontName"="%s"', [FFontName]));
      RegFile.Add(Format('"FontSize"=dword:%x', [FFontSize]));
      RegFile.Add(Format('"MainToolBarColor"="%s"', [FMainToolBarColor]));
      RegFile.Add('');
      RegFile.SaveToFile(FileName);
    finally
      RegFile.Free;
    end;
    RunAndWait(0, 'regedit.exe', '/S "' + FileName + '"', True);
  end
  else
  begin
    RegWriteStr(sKey, 'FontName', FFontName, HKEY_CURRENT_USER);
    RegWriteStr(sKey, 'MainToolBarColor', FMainToolBarColor, HKEY_CURRENT_USER);
    RegWriteInt(sKey, 'FontSize', FFontSize, HKEY_CURRENT_USER);
  end;
end;

end.
