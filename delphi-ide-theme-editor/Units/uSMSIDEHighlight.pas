//**************************************************************************************************
//
// Unit uSMSIDEHighlight
// unit uSMSIDEHighlight  for the Delphi IDE Theme Editor
//
// The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
// you may not use this file except in compliance with the License. You may obtain a copy of the
// License at http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
// ANY KIND, either express or implied. See the License for the specific language governing rights
// and limitations under the License.
//
// The Original Code is uSMSIDEHighlight.pas.
//
// The Initial Developer of the Original Code is Rodrigo Ruz V.
// Portions created by Rodrigo Ruz V. are Copyright (C) 2011-2014 Rodrigo Ruz V.
// All Rights Reserved.
//
//**************************************************************************************************
unit uSMSIDEHighlight;

interface

uses
  uDelphiIDEHighlight;

function  GetSMSIDEFontSize : Integer;
function  GetSMSIDEFontName : string;
function  GetSMSIDEThemeName : string;
function  SetSMSIDEFont(const FontName:String;FontSize:Integer):Boolean;
function  ApplySMSIDETheme(const ATheme:TIDETheme;const ThemeName:string) : Boolean;

function  DelphiIDEThemeToSMSTheme(const ATheme:TIDETheme;const ThemeName : string; Var OutputFileName:string) : Boolean;
//function  DelphiIDEThemeToSMSTheme(const DelphiIdeTheme, OutputFolder:string) : Boolean;overload;

implementation

uses
  uMisc,
  ComObj,
  Windows,
  inifiles,
  System.IOUtils,
  System.SysUtils,
  System.Variants,
  uSMSVersions;

const
  sSMSThemeTemplate   ='Default.shi';

function  GetSMSIDEFontSize : Integer;
var
  LIniFileName : TIniFile;
begin
  LIniFileName:=TIniFile.Create(GetSMSEditorOptionsFileName);
  try
    Result:= LIniFileName.ReadInteger('editor','use_font_size', 10);
  finally
    LIniFileName.Free;
  end;
end;

function  GetSMSIDEFontName : string;
var
  LIniFileName : TIniFile;
begin
  LIniFileName:=TIniFile.Create(GetSMSEditorOptionsFileName);
  try
    Result:= LIniFileName.ReadString('editor','use_fontname', 'Courier New');
  finally
    LIniFileName.Free;
  end;
end;

function  GetSMSIDEThemeName : string;
var
  LIniFileName : TIniFile;
begin
  LIniFileName:=TIniFile.Create(GetSMSEditorOptionsFileName);
  try
    Result:= LIniFileName.ReadString('editor','current_theme', 'default');
  finally
    LIniFileName.Free;
  end;
end;

function  SetSMSIDEFont(const FontName:String;FontSize:Integer):Boolean;
var
  LIniFileName : TIniFile;
begin
  LIniFileName:=TIniFile.Create(GetSMSEditorOptionsFileName);
  try
    LIniFileName.WriteString('editor','use_fontname', FontName);
    LIniFileName.WriteInteger('editor','use_font_size', FontSize);
    Result:=True;
  finally
    LIniFileName.Free;
  end;
end;

function  ApplySMSIDETheme(const ATheme:TIDETheme;const ThemeName:string ) : Boolean;
var
  OutPutFileName, NewFileName : String;
  LIniFileName : TIniFile;

 procedure SetElementsValues(const Name : string; Element :TItemIDEHighlightElementsAttributes);
 var
  Style : Integer;
 begin
    if Element.ForegroundColorNew<>'' then
      LIniFileName.WriteString('editor',Name+'.foreground', Element.ForegroundColorNew);

    if Element.BackgroundColorNew<>'' then
      LIniFileName.WriteString('editor',Name+'.background', Element.BackgroundColorNew);

    Style:=0;
    if Element.Bold then
      Inc(Style, 1);

    if Element.Italic then
      Inc(Style, 2);

    if Element.Italic then
      Inc(Style, 3);

    LIniFileName.WriteString('editor',Name+'.style', IntToStr(Style));
 end;

begin
  Result:=DelphiIDEThemeToSMSTheme(ATheme, ThemeName, OutPutFileName);
  NewFileName:=IncludeTrailingPathDelimiter(GetSMSIDEFolder)+'Source Highlighter Colors\'+ExtractFileName(OutPutFileName);

  if Result then
  begin
    if (not IsUACEnabled) and (CurrentUserIsAdmin) then
    begin
     Result:= CopyFile(PChar(OutPutFileName), PChar(NewFileName), false);
    end
    else
    begin
     RunAsAdmin('cmd.exe', Format('/c copy /Y "%s" "%s"',[OutPutFileName, NewFileName]));
     Result:=True;
    end;
  end;


  if Result then
  begin
    LIniFileName:=TIniFile.Create(GetSMSEditorOptionsFileName);
    try
      SetElementsValues('pascal.assembler',  ATheme[Assembler]);
      SetElementsValues('pascal.comment',  ATheme[Comment]);
      SetElementsValues('pascal.directive',  ATheme[Preprocessor]);
      SetElementsValues('pascal.identifier',  ATheme[Identifier]);
      SetElementsValues('pascal.keyword',  ATheme[ReservedWord]);
      SetElementsValues('pascal.symbols',  ATheme[Symbol]);
      SetElementsValues('pascal.string',  ATheme[&String]);
      SetElementsValues('pascal.space',  ATheme[Whitespace]);
      SetElementsValues('pascal.number',  ATheme[Number]);
      SetElementsValues('pascal.float',  ATheme[Float]);
      SetElementsValues('pascal.hex',  ATheme[Hex]);
      SetElementsValues('pascal.char',  ATheme[Character]);

      SetElementsValues('javascript.comment',  ATheme[CodeFoldingTree]);
      SetElementsValues('javascript.identifier',  ATheme[Identifier]);
      SetElementsValues('javascript.reserved',  ATheme[ReservedWord]);
      SetElementsValues('javascript.symbols',  ATheme[Symbol]);
      SetElementsValues('javascript.string',  ATheme[&String]);
      SetElementsValues('javascript.space',  ATheme[Whitespace]);
      SetElementsValues('javascript.number',  ATheme[Number]);
      SetElementsValues('javascript.event',  ATheme[ReservedWord]);
      SetElementsValues('javascript.keyword',  ATheme[ReservedWord]);

      SetElementsValues('html.and',  ATheme[Symbol]);
      SetElementsValues('html.comment',  ATheme[Comment]);
      SetElementsValues('html.identifier',  ATheme[Identifier]);
      SetElementsValues('html.keyword',  ATheme[ReservedWord]);
      SetElementsValues('html.symbols',  ATheme[Symbol]);
      SetElementsValues('html.space',  ATheme[Whitespace]);
      SetElementsValues('html.text',  ATheme[PlainText]);
      SetElementsValues('html.value',  ATheme[Number]);
      SetElementsValues('html.undef',  ATheme[PlainText]);

      SetElementsValues('css.comment',  ATheme[Comment]);
      SetElementsValues('css.property',  ATheme[ReservedWord]);
      SetElementsValues('css.keyword',  ATheme[ReservedWord]);
      SetElementsValues('css.symbols',  ATheme[Symbol]);
      SetElementsValues('css.string',  ATheme[&String]);
      SetElementsValues('css.space',  ATheme[Whitespace]);
      SetElementsValues('css.number',  ATheme[Number]);
      SetElementsValues('css.color',  ATheme[Number]);
      SetElementsValues('css.text',  ATheme[PlainText]);
      SetElementsValues('css.value',  ATheme[Number]);
      SetElementsValues('css.undef',  ATheme[PlainText]);


      LIniFileName.WriteString('editor','current_theme', ThemeName);
    finally
      LIniFileName.Free;
    end;
  end;

end;


function  DelphiIDEThemeToSMSTheme(const ATheme:TIDETheme;const ThemeName : string; Var OutputFileName:string) : Boolean;
const
 sGlobalElementXPath               = '/Highlighter';
 sLangObjectPascalXPath            = '/Highlighter/ObjectPascal/%s';
 sLangJavaScriptXPath              = '/Highlighter/JavaScript/%s';
 sLangHTMLXPath                    = '/Highlighter/HTML/%s';
 sLangCSSXPath                     = '/Highlighter/CascadingStyleSheet/%s';
var
  XmlStr: string;
  XmlDoc: olevariant;
  //Node:   olevariant;

  procedure SetElement(const XPath,Name : string; Element :TItemIDEHighlightElementsAttributes);
  var
   ANode  : OleVariant;
   Style  : Integer;
  begin

    if Element.BackgroundColorNew<>'' then
    begin
      ANode := XmlDoc.selectSingleNode(Format(XPath,[Name]));
      if (Name='') or not VarIsClear(ANode) then
        ANode.SetAttribute('Background', StrToInt(Element.BackgroundColorNew).ToString());
    end;

    if Element.ForegroundColorNew<>'' then
    begin
      ANode := XmlDoc.selectSingleNode(Format(XPath,[Name]));
      if (Name='') or  not VarIsClear(ANode) then
        ANode.SetAttribute('Foreground', StrToInt(Element.ForegroundColorNew).ToString());
    end;

    Style:=0;
    if Element.Bold then
      Inc(Style, 1);

    if Element.Italic then
      Inc(Style, 2);

    if Element.Italic then
      Inc(Style, 3);

    ANode := XmlDoc.selectSingleNode(sGlobalElementXPath);
    if not VarIsClear(ANode) then
      ANode.SetAttribute('Style', Style.ToString());
  end;

  procedure SetGlobalElement(const Name:string; Element :TItemIDEHighlightElementsAttributes);
  var
   ANode  : OleVariant;
  begin
     // SetElement(sGlobalElementXPath, Name, Element);
    if Element.BackgroundColorNew<>'' then
    begin
      ANode := XmlDoc.selectSingleNode(sGlobalElementXPath);
      if not VarIsClear(ANode) then
        ANode.SetAttribute('Background', StrToInt(Element.BackgroundColorNew).ToString());
    end;

    if Element.ForegroundColorNew<>'' then
    begin
      ANode := XmlDoc.selectSingleNode(sGlobalElementXPath);
      if not VarIsClear(ANode) then
        ANode.SetAttribute('Foreground',StrToInt(Element.ForegroundColorNew).ToString());
    end;

    ANode := XmlDoc.selectSingleNode(sGlobalElementXPath);
    if not VarIsClear(ANode) then
      ANode.SetAttribute('Style','0');
  end;

  procedure SetLangObjectPascalElement(const Name:string; Element :TItemIDEHighlightElementsAttributes);
  begin
      SetElement(sLangObjectPascalXPath, Name, Element);
  end;

  procedure SetLangHTMLElement(const Name:string; Element :TItemIDEHighlightElementsAttributes);
  begin
      SetElement(sLangHTMLXPath, Name, Element);
  end;

  procedure SetLangJavascriptElement(const Name:string; Element :TItemIDEHighlightElementsAttributes);
  begin
      SetElement(sLangJavascriptXPath, Name, Element);
  end;

  procedure SetLangCSSElement(const Name:string; Element :TItemIDEHighlightElementsAttributes);
  begin
      SetElement(sLangCSSXPath, Name, Element);
  end;

begin
  Result:=False;
  OutputFileName:=MakeValidTagName(ThemeName)+'.shi';
  XmlStr:= TFile.ReadAllText(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)))+'default\'+sSMSThemeTemplate);

  XmlDoc := CreateOleObject('Msxml2.DOMDocument.6.0');
  try
    XmlDoc.Async := False;
    XmlDoc.LoadXml(XmlStr);
    XmlDoc.SetProperty('SelectionLanguage', 'XPath');

    if (XmlDoc.parseError.errorCode <> 0) then
      raise Exception.CreateFmt('Error in Xml Data %s', [XmlDoc.parseError]);

    SetGlobalElement('', ATheme[PlainText]);


    SetLangObjectPascalElement('Assembler', ATheme[Assembler]);
    SetLangObjectPascalElement('Character', ATheme[Character]);
    SetLangObjectPascalElement('Comment', ATheme[Comment]);
    SetLangObjectPascalElement('Float', ATheme[Float]);
    SetLangObjectPascalElement('Hexadecimal', ATheme[Hex]);
    SetLangObjectPascalElement('Identifier', ATheme[Identifier]);
    SetLangObjectPascalElement('Number', ATheme[Number]);
    SetLangObjectPascalElement('Preprocessor', ATheme[Preprocessor]);
    SetLangObjectPascalElement('ReservedWord', ATheme[ReservedWord]);
    SetLangObjectPascalElement('Space', ATheme[Whitespace]);
    SetLangObjectPascalElement('String', ATheme[&String]);
    SetLangObjectPascalElement('Symbol', ATheme[Symbol]);

    SetLangJavascriptElement('Comment', ATheme[Comment]);
    SetLangJavascriptElement('Event', ATheme[ReservedWord]);    //??
    SetLangJavascriptElement('Identifier', ATheme[Identifier]);
    SetLangJavascriptElement('NonreservedKeyword', ATheme[PlainText]);   //??
    SetLangJavascriptElement('Number', ATheme[Number]);
    SetLangJavascriptElement('ReservedWord', ATheme[ReservedWord]);
    SetLangJavascriptElement('Space', ATheme[Whitespace]);
    SetLangJavascriptElement('String', ATheme[&String]);
    SetLangJavascriptElement('Symbol', ATheme[Symbol]);

    SetLangHTMLElement('Comment', ATheme[Comment]);
    SetLangHTMLElement('EscapeAmpersand', ATheme[Symbol]);   //??
    SetLangHTMLElement('Identifier', ATheme[Identifier]);
    SetLangHTMLElement('ReservedWord', ATheme[ReservedWord]);
    SetLangHTMLElement('Space', ATheme[Whitespace]);
    SetLangHTMLElement('Symbol', ATheme[Symbol]);
    SetLangHTMLElement('Text', ATheme[PlainText]);
    SetLangHTMLElement('UnknownWord', ATheme[PlainText]);//??
    SetLangHTMLElement('Value', ATheme[Number]);//??


    SetLangCSSElement('ColorValue', ATheme[Number]);   //??
    SetLangCSSElement('Comment', ATheme[Comment]);
    SetLangCSSElement('Number', ATheme[Number]);
    SetLangCSSElement('Property', ATheme[ReservedWord]);  //??
    SetLangCSSElement('ReservedWord', ATheme[ReservedWord]);
    SetLangCSSElement('Space', ATheme[Whitespace]);
    SetLangCSSElement('String', ATheme[&String]);
    SetLangCSSElement('Text', ATheme[PlainText]);//??
    SetLangCSSElement('UndefinedProperty', ATheme[PlainText]);//??
    SetLangCSSElement('Value', ATheme[Number]);//??

    OutputFileName:=IncludeTrailingPathDelimiter(GetTempDirectory)+OutputFileName;
    XmlDoc.Save(OutputFileName);
    Result:=True;
  finally
    XmlDoc := Unassigned;
  end;
end;




end.
