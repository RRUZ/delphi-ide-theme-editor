{**************************************************************************************************}
{                                                                                                  }
{ Unit uLazarusIDEHighlight                                                                        }
{ unit uLazarusIDEHighlight  for the Delphi IDE Theme Editor                                       }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is uLazarusIDEHighlight.pas.                                                   }
{                                                                                                  }
{ The Initial Developer of the Original Code is Rodrigo Ruz V.                                     }
{ Portions created by Rodrigo Ruz V. are Copyright (C) 2011 Rodrigo Ruz V.                         }
{ All Rights Reserved.                                                                             }
{                                                                                                  }
{**************************************************************************************************}
unit uLazarusIDEHighlight;


interface

uses
  uDelphiIDEHighlight;

const
 LazarusOffsetFont=6;

function  GetLazarusIDEFontSize : Integer;
function  GetLazarusIDEFontName : string;
function  GetLazarusIDEThemeName : string;
function  SetLazarusIDEFont(const FontName:String;FontSize:Integer):Boolean;
function  ApplyLazarusIDETheme(const ATheme:TIDETheme;const ThemeName:string) : Boolean;

function  DelphiIDEThemeToLazarusTheme(const ATheme:TIDETheme;const ThemeName, OutputFolder:string) : Boolean; overload;
function  DelphiIDEThemeToLazarusTheme(const DelphiIdeTheme, OutputFolder:string) : Boolean;overload;


implementation

uses
  Variants,
  SysUtils,
  IOUtils,
  StrUtils,
  ComObj,
  uLazarusVersions;

const
  sLazarusThemeTemplate   ='ColorDummy.xml';
  sDefaultLazarusFont     ='Courier New';
  sDefaultLazarusFontSize =10;
  sXMLLazarusEditorOptions=
                            '<?xml version="1.0"?> '+
                            '<CONFIG> '+
                            '  <EditorOptions Version="6"> '+
                            '    <KeyMapping> '+
                            '      <default> '+
                            '        <Version Value="6"/> '+
                            '      </default> '+
                            '    </KeyMapping> '+
                            '    <Color Version="6"> '+
                            '      <LangObjectPascal Version="6"> '+
                            '        <ColorScheme Value="Default"/> '+
                            '      </LangObjectPascal> '+
                            '    </Color> '+
                            '    <Display EditorFont="Courier New" EditorFontHeight="15" DisableAntialiasing="False"/> '+
                            '  </EditorOptions> '+
                            '</CONFIG>';


function GetEditorOptionsXMLValue(const XPath:string)  : string;
var
  XmlDoc: OleVariant;
  Node:   OleVariant;
begin
  Result := '';
  XmlDoc := CreateOleObject('Msxml2.DOMDocument.6.0');
  try
    XmlDoc.Async := False;
    if XmlDoc.Load(GetLazarusEditorOptionsFileName) then
    begin
      XmlDoc.SetProperty('SelectionLanguage', 'XPath');

      if (XmlDoc.parseError.errorCode <> 0) then
        raise Exception.CreateFmt('Error in Xml Data %s', [XmlDoc.parseError]);

      Node := XmlDoc.selectSingleNode(XPath);
      if not VarIsClear(Node) then
        Result:= Node.Text;
    end;
  finally
    XmlDoc := Unassigned;
  end;
end;

function SetEditorOptionsXMLValue(const XPath,Value:OleVariant) : boolean;
var
  XmlDoc: olevariant;
  Node:   olevariant;
begin
  Result:=False;
  XmlDoc := CreateOleObject('Msxml2.DOMDocument.6.0');
  try
    XmlDoc.Async := False;
    if not XmlDoc.Load(GetLazarusEditorOptionsFileName) then
     XmlDoc.LoadXML(sXMLLazarusEditorOptions);

    XmlDoc.SetProperty('SelectionLanguage', 'XPath');

    if (XmlDoc.parseError.errorCode <> 0) then
      raise Exception.CreateFmt('Error in Xml Data %s', [XmlDoc.parseError]);

    Node := XmlDoc.selectSingleNode(XPath);
    if not VarIsClear(Node) then
      Node.Text:=Value;

    XmlDoc.Save(GetLazarusEditorOptionsFileName);
    Result:=True;
  finally
    XmlDoc := Unassigned;
  end;
end;


function  GetLazarusIDEFontSize : Integer;
begin
  if TryStrToInt(GetEditorOptionsXMLValue('/CONFIG/EditorOptions/Display/@EditorFontHeight'),Result) then
    Result:=Result-LazarusOffsetFont
  else
    Result:=sDefaultLazarusFontSize;
end;

function  GetLazarusIDEFontName : string;
begin
  Result:=GetEditorOptionsXMLValue('/CONFIG/EditorOptions/Display/@EditorFont');
  if Result='' then
    Result:=sDefaultLazarusFont;
end;



function  SetLazarusIDEFont(const FontName:String;FontSize:Integer):Boolean;
begin
  Result:=SetEditorOptionsXMLValue('/CONFIG/EditorOptions/Display/@EditorFont',FontName);
  if Result then
    Result:=SetEditorOptionsXMLValue('/CONFIG/EditorOptions/Display/@EditorFontHeight',FontSize+LazarusOffsetFont);
end;

function MakeValidTagName(const s: string): string;
var
  c: Char;
  i: Integer;
begin
  SetLength(Result, Length(s));
  i:=0;
  for c in s do
  begin
   Inc(i);
    if CharInSet(c,['A'..'Z', 'a'..'z', '0'..'9']) then
      Result[i] := c
    else
      Result[i] := '_';
  end;
end;


function  DelphiIDEThemeToLazarusTheme(const ATheme:TIDETheme;const ThemeName,OutputFolder:string) : Boolean;
const
 sGlobalElementXPath               = '/CONFIG/Lazarus/ColorSchemes/Globals/%s/%s';
 sLangObjectPascalXPath            = '/CONFIG/Lazarus/ColorSchemes/LangObjectPascal/%s/%s';
 sLangLazarus_Form_definitionXPath = '/CONFIG/Lazarus/ColorSchemes/LangLazarus_Form_definition/%s/%s';
 sLangXML_documentXPath            = '/CONFIG/Lazarus/ColorSchemes/LangXML_document/%s/%s';
 sLangHTML_documentXPath           = '/CONFIG/Lazarus/ColorSchemes/LangHTML_document/%s/%s';
 sLangC__XPath                     = '/CONFIG/Lazarus/ColorSchemes/LangC__/%s/%s';
 sLangPerlXPath                    = '/CONFIG/Lazarus/ColorSchemes/LangPerl/%s/%s';
 sLangJavaXPath                    = '/CONFIG/Lazarus/ColorSchemes/LangJava/%s/%s';
 sLangUNIX_Shell_ScriptXPath       = '/CONFIG/Lazarus/ColorSchemes/LangUNIX_Shell_Script/%s/%s';
 sLangPythonXPath                  = '/CONFIG/Lazarus/ColorSchemes/LangPython/%s/%s';
 sLangPHPXPath                     = '/CONFIG/Lazarus/ColorSchemes/LangPHP/%s/%s';
 sLangSQLXPath                     = '/CONFIG/Lazarus/ColorSchemes/LangSQL/%s/%s';
 sLangJavascriptXPath              = '/CONFIG/Lazarus/ColorSchemes/LangJavascript/%s/%s';
 sLangDiff_FileXPath               = '/CONFIG/Lazarus/ColorSchemes/LangDiff_File/%s/%s';
 sLangMS_DOS_batch_languageXPath   = '/CONFIG/Lazarus/ColorSchemes/LangMS_DOS_batch_language/%s/%s';
 sLangINI_fileXPath                = '/CONFIG/Lazarus/ColorSchemes/LangINI_file/%s/%s';


var
  XmlStr: string;
  XmlDoc: olevariant;
  Node:   olevariant;
  SchemeName      : string;
  SchemeFileName  : string;

  procedure SetElement(const XPath,Name : string; Element :TItemIDEHighlightElementsAttributes);
  var
   ANode  : OleVariant;
  begin

    if Element.BackgroundColorNew<>'' then
    begin
      ANode := XmlDoc.selectSingleNode(Format(XPath,[SchemeName,Name]));
      if not VarIsClear(ANode) then
        ANode.SetAttribute('Background',Element.BackgroundColorNew);
    end;

    if Element.ForegroundColorNew<>'' then
    begin
      ANode := XmlDoc.selectSingleNode(Format(XPath,[SchemeName,Name]));
      if not VarIsClear(ANode) then
        ANode.SetAttribute('Foreground',Element.ForegroundColorNew);
    end;
      {
    if Element.ForegroundColorNew<>'' then
    begin
      ANode := XmlDoc.selectSingleNode(Format(XPath,[SchemeName,Name]));
      if not VarIsClear(ANode) then
        ANode.SetAttribute('FrameColor',Element.ForegroundColorNew);
    end;
       }
    if Element.Bold then
    begin
      ANode := XmlDoc.selectSingleNode(Format(XPath,[SchemeName,Name]));
      if not VarIsClear(ANode) then
        ANode.SetAttribute('Style','fsBold');
    end
    else
    if Element.Italic then
    begin
      ANode := XmlDoc.selectSingleNode(Format(XPath,[SchemeName,Name]));
      if not VarIsClear(ANode) then
        ANode.SetAttribute('Style','fsItalic');
    end;
  end;

  procedure SetGlobalElement(const Name:string; Element :TItemIDEHighlightElementsAttributes);
  begin
      SetElement(sGlobalElementXPath, Name, Element);
  end;

  procedure SetLangObjectPascalElement(const Name:string; Element :TItemIDEHighlightElementsAttributes);
  begin
      SetElement(sLangObjectPascalXPath, Name, Element);
  end;

  procedure SetLangLazarus_Form_definitionElement(const Name:string; Element :TItemIDEHighlightElementsAttributes);
  begin
      SetElement(sLangLazarus_Form_definitionXPath, Name, Element);
  end;

  procedure SetLangXML_documentElement(const Name:string; Element :TItemIDEHighlightElementsAttributes);
  begin
      SetElement(sLangXML_documentXPath, Name, Element);
  end;

  procedure SetLangHTML_documentElement(const Name:string; Element :TItemIDEHighlightElementsAttributes);
  begin
      SetElement(sLangHTML_documentXPath, Name, Element);
  end;

  procedure SetLangPerlElement(const Name:string; Element :TItemIDEHighlightElementsAttributes);
  begin
      SetElement(sLangPerlXPath, Name, Element);
  end;

  procedure SetLangC__Element(const Name:string; Element :TItemIDEHighlightElementsAttributes);
  begin
      SetElement(sLangC__XPath, Name, Element);
  end;

  procedure SetLangJavaElement(const Name:string; Element :TItemIDEHighlightElementsAttributes);
  begin
      SetElement(sLangJavaXPath, Name, Element);
  end;

  procedure SetLangUNIX_Shell_ScriptElement(const Name:string; Element :TItemIDEHighlightElementsAttributes);
  begin
      SetElement(sLangUNIX_Shell_ScriptXPath, Name, Element);
  end;

  procedure SetLangPythonElement(const Name:string; Element :TItemIDEHighlightElementsAttributes);
  begin
      SetElement(sLangPythonXPath, Name, Element);
  end;

  procedure SetLangPHPElement(const Name:string; Element :TItemIDEHighlightElementsAttributes);
  begin
      SetElement(sLangPHPXPath, Name, Element);
  end;

  procedure SetLangSQLElement(const Name:string; Element :TItemIDEHighlightElementsAttributes);
  begin
      SetElement(sLangSQLXPath, Name, Element);
  end;

  procedure SetLangJavascriptElement(const Name:string; Element :TItemIDEHighlightElementsAttributes);
  begin
      SetElement(sLangJavascriptXPath, Name, Element);
  end;

  procedure SetLangDiff_FileElement(const Name:string; Element :TItemIDEHighlightElementsAttributes);
  begin
      SetElement(sLangDiff_FileXPath, Name, Element);
  end;

  procedure SetLangMS_DOS_batch_languageElement(const Name:string; Element :TItemIDEHighlightElementsAttributes);
  begin
      SetElement(sLangMS_DOS_batch_languageXPath, Name, Element);
  end;

  procedure SetLangINI_fileElement(const Name:string; Element :TItemIDEHighlightElementsAttributes);
  begin
      SetElement(sLangINI_fileXPath, Name, Element);
  end;

begin
  Result:=False;
  SchemeName:='Scheme'+MakeValidTagName(ThemeName);
  SchemeFileName:='Color'+MakeValidTagName(ThemeName)+'.xml';
  XmlStr:= TFile.ReadAllText(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)))+'default\'+sLazarusThemeTemplate);
  XmlStr:=StringReplace(XmlStr,'SchemeDummy',SchemeName,[rfReplaceAll]);

  XmlDoc := CreateOleObject('Msxml2.DOMDocument.6.0');
  try
    XmlDoc.Async := False;
    XmlDoc.LoadXml(XmlStr);
    XmlDoc.SetProperty('SelectionLanguage', 'XPath');

    if (XmlDoc.parseError.errorCode <> 0) then
      raise Exception.CreateFmt('Error in Xml Data %s', [XmlDoc.parseError]);

    Node := XmlDoc.selectSingleNode('/CONFIG/Lazarus/ColorSchemes/Names/Item1/@Value');
    if not VarIsClear(Node) then
      Node.Text:=MakeValidTagName(ThemeName);


    SetGlobalElement('ahaDefault', ATheme[PlainText]);
    SetGlobalElement('ahaTextBlock', ATheme[MarkedBlock]);
    SetGlobalElement('ahaExecutionPoint', ATheme[ExecutionPoint]);
    SetGlobalElement('ahaEnabledBreakpoint', ATheme[EnabledBreak]);
    SetGlobalElement('ahaDisabledBreakpoint', ATheme[DisabledBreak]);
    SetGlobalElement('ahaInvalidBreakpoint', ATheme[InvalidBreak]);
    SetGlobalElement('ahaUnknownBreakpoint', ATheme[InvalidBreak]);
    SetGlobalElement('ahaErrorLine', ATheme[ErrorLine]);

    SetGlobalElement('ahaIncrementalSearch', ATheme[SearchMatch]);
    SetGlobalElement('ahaHighlightAll', ATheme[MarkedBlock]);

    //SetGlobalElement('ahaBracketMatch', ATheme[].BackgroundColorNew, ATheme[].ForegroundColorNew,'','');
    SetGlobalElement('ahaMouseLink', ATheme[HotLink]);
    SetGlobalElement('ahaModifiedLine', ATheme[ModifiedLine]);
    SetGlobalElement('ahaCodeFoldingTree', ATheme[CodeFoldingTree]);
    //SetGlobalElement('ahaHighlightWord', ATheme[].BackgroundColorNew, ATheme[].ForegroundColorNew,'','');
    SetGlobalElement('ahaFoldedCode', ATheme[FoldedCode]);
    //SetGlobalElement('ahaWordGroup', ATheme[].BackgroundColorNew, ATheme[].ForegroundColorNew);
    //SetGlobalElement('ahaTemplateEditCur', ATheme[].BackgroundColorNew, ATheme[].ForegroundColorNew);
    //SetGlobalElement('ahaTemplateEditSync', ATheme[].BackgroundColorNew, ATheme[].ForegroundColorNew);
    //SetGlobalElement('ahaTemplateEditOther', ATheme[].BackgroundColorNew, ATheme[].ForegroundColorNew);
    //SetGlobalElement('ahaSyncroEditCur', ATheme[].BackgroundColorNew, ATheme[].ForegroundColorNew);
    //SetGlobalElement('ahaSyncroEditSync', ATheme[].BackgroundColorNew, ATheme[].ForegroundColorNew);
    //SetGlobalElement('ahaSyncroEditOther', ATheme[].BackgroundColorNew, ATheme[].ForegroundColorNew);
    //SetGlobalElement('ahaSyncroEditArea', ATheme[].BackgroundColorNew, ATheme[].ForegroundColorNew);
    SetGlobalElement('ahaGutterSeparator', ATheme[CodeFoldingTree]);
    SetGlobalElement('ahaGutter', ATheme[LineNumber]);
    SetGlobalElement('ahaRightMargin', ATheme[RightMargin]);
    SetGlobalElement('ahaLineNumber', ATheme[LineNumber]);

    SetLangObjectPascalElement('Assembler', ATheme[Assembler]);
    SetLangObjectPascalElement('Comment', ATheme[Comment]);
    SetLangObjectPascalElement('Directive', ATheme[Preprocessor]);
    SetLangObjectPascalElement('Number', ATheme[Number]);
    SetLangObjectPascalElement('Reserved_word', ATheme[ReservedWord]); //+ style???
    SetLangObjectPascalElement('String', ATheme[&String]);
    SetLangObjectPascalElement('Symbol', ATheme[Symbol]);


    SetLangLazarus_Form_definitionElement('Comment', ATheme[Comment]);
    SetLangLazarus_Form_definitionElement('Key', ATheme[ReservedWord]);
    SetLangLazarus_Form_definitionElement('Number', ATheme[Number]);
    SetLangLazarus_Form_definitionElement('String', ATheme[&String]);
    SetLangLazarus_Form_definitionElement('Symbol', ATheme[Symbol]);

    SetLangXML_documentElement('Attribute_Name', ATheme[AttributeNames]);
    SetLangXML_documentElement('Attribute_Value', ATheme[AttributeValues]);
    SetLangXML_documentElement('CDATA_Section', ATheme[PlainText]);
    SetLangXML_documentElement('Comment', ATheme[Comment]);
    SetLangXML_documentElement('DOCTYPE_Section', ATheme[Preprocessor]);
    SetLangXML_documentElement('Element_Name', ATheme[Character]);
    SetLangXML_documentElement('Entity_Reference', ATheme[Symbol]);
    SetLangXML_documentElement('Namespace_Attribute_Name', ATheme[AttributeNames]);
    SetLangXML_documentElement('Namespace_Attribute_Value', ATheme[AttributeValues]);
    SetLangXML_documentElement('Processing_Instruction', ATheme[ReservedWord]);
    SetLangXML_documentElement('Symbol', ATheme[Symbol]);
    SetLangXML_documentElement('Text', ATheme[&String]);

    SetLangXML_documentElement('Asp', ATheme[PlainText]);
    SetLangXML_documentElement('Comment', ATheme[Comment]);
    SetLangXML_documentElement('Escape_ampersand', ATheme[Symbol]);
    SetLangXML_documentElement('Identifier', ATheme[Identifier]);
    SetLangXML_documentElement('Reserved_word', ATheme[ReservedWord]);
    SetLangXML_documentElement('Symbol', ATheme[Symbol]);
    SetLangXML_documentElement('Unknown_word', ATheme[Preprocessor]);
    SetLangXML_documentElement('Value', ATheme[Character]);

    SetLangC__Element('Assembler', ATheme[Assembler]);
    SetLangC__Element('Comment', ATheme[Comment]);
    SetLangC__Element('Number', ATheme[Number]);
    SetLangC__Element('Preprocessor',  ATheme[Preprocessor]);
    SetLangC__Element('Reserved_word', ATheme[ReservedWord]); //+ style???
    SetLangC__Element('Space', ATheme[Whitespace]);
    SetLangC__Element('String', ATheme[&String]);
    SetLangC__Element('Symbol', ATheme[Symbol]);

    SetLangPerlElement('Comment', ATheme[Comment]);
    SetLangPerlElement('Number', ATheme[Number]);
    SetLangPerlElement('Pragma',  ATheme[Preprocessor]);
    SetLangPerlElement('Reserved_word', ATheme[ReservedWord]); //+ style???
    SetLangPerlElement('Space', ATheme[Whitespace]);
    SetLangPerlElement('String', ATheme[&String]);
    SetLangPerlElement('Symbol', ATheme[Symbol]);
    SetLangPerlElement('Variable', ATheme[Scripts]);

    SetLangJavaElement('Comment', ATheme[Comment]);
    SetLangJavaElement('Documentation', ATheme[Identifier]);
    SetLangJavaElement('Number', ATheme[Number]);
    SetLangJavaElement('Reserved_word', ATheme[ReservedWord]); //+ style???
    SetLangJavaElement('Space', ATheme[Whitespace]);
    SetLangJavaElement('String', ATheme[&String]);
    SetLangJavaElement('Symbol', ATheme[Symbol]);

    SetLangUNIX_Shell_ScriptElement('Comment', ATheme[Comment]);
    SetLangUNIX_Shell_ScriptElement('Number', ATheme[Number]);
    SetLangUNIX_Shell_ScriptElement('Reserved_word', ATheme[ReservedWord]); //+ style???
    SetLangUNIX_Shell_ScriptElement('String', ATheme[&String]);
    SetLangUNIX_Shell_ScriptElement('Symbol', ATheme[Symbol]);
    SetLangUNIX_Shell_ScriptElement('Variable', ATheme[Scripts]);

    SetLangPythonElement('Comment', ATheme[Comment]);
    SetLangPythonElement('Documentation', ATheme[Identifier]);
    SetLangPythonElement('Float', ATheme[Float]);
    SetLangPythonElement('Hexadecimal', ATheme[Hex]);
    SetLangPythonElement('Non_reserved_keyword', ATheme[PlainText]);
    SetLangPythonElement('Number', ATheme[Number]);
    SetLangPythonElement('Octal', ATheme[Octal]);
    SetLangPythonElement('Reserved_word', ATheme[ReservedWord]); //+ style???
    SetLangPythonElement('String', ATheme[&String]);
    SetLangPythonElement('Symbol', ATheme[Symbol]);
    SetLangPythonElement('SyntaxError', ATheme[IllegalChar]);
    SetLangPythonElement('System_functions_and_variables', ATheme[Identifier]);

    SetLangPHPElement('Comment', ATheme[Comment]);
    SetLangPHPElement('Number', ATheme[Number]);
    SetLangPHPElement('Reserved_word', ATheme[ReservedWord]); //+ style???
    SetLangPHPElement('String', ATheme[&String]);
    SetLangPHPElement('Symbol', ATheme[Symbol]);

    SetLangSQLElement('Comment', ATheme[Comment]);
    SetLangSQLElement('Data_type', ATheme[Identifier]);
    SetLangSQLElement('Default_packages', ATheme[Character]);
    SetLangSQLElement('Exception', ATheme[IllegalChar]);
    SetLangSQLElement('Function', ATheme[ReservedWord]);
    SetLangSQLElement('Number', ATheme[Number]);
    SetLangSQLElement('Reserved_word', ATheme[ReservedWord]);
    SetLangSQLElement('Reserved_word__PL_SQL_', ATheme[ReservedWord]);
    SetLangSQLElement('SQL_Plus_command', ATheme[Scripts]);
    SetLangSQLElement('String', ATheme[&String]);
    SetLangSQLElement('Symbol', ATheme[Symbol]);

    SetLangJavascriptElement('Comment', ATheme[Comment]);
    SetLangJavascriptElement('Number', ATheme[Number]);
    SetLangJavascriptElement('Reserved_word', ATheme[ReservedWord]); //+ style???
    SetLangJavascriptElement('String', ATheme[&String]);
    SetLangJavascriptElement('Symbol', ATheme[Symbol]);

    SetLangDiff_FileElement('Diff_Added_line', ATheme[DiffAddition]);
    SetLangDiff_FileElement('Diff_Changed_Line', ATheme[DiffMove]);
    SetLangDiff_FileElement('Diff_Chunk_Line_Counts', ATheme[Number]);
    SetLangDiff_FileElement('Diff_Chunk_Marker', ATheme[Symbol]);
    SetLangDiff_FileElement('Diff_Chunk_New_Line_Count', ATheme[Preprocessor]);
    SetLangDiff_FileElement('Diff_Chunk_Original_Line_Count', ATheme[Character]);
    SetLangDiff_FileElement('Diff_New_File', ATheme[Symbol]);
    SetLangDiff_FileElement('Diff_Original_File', ATheme[AttributeNames]);
    SetLangDiff_FileElement('Diff_Removed_Line', ATheme[DiffDeletion]);
    SetLangDiff_FileElement('Unknown_word', ATheme[InvalidBreak]);

    SetLangMS_DOS_batch_languageElement('Key', ATheme[ReservedWord]); //+ style???
    SetLangMS_DOS_batch_languageElement('Number', ATheme[Number]);
    SetLangMS_DOS_batch_languageElement('Comment', ATheme[Comment]);
    SetLangMS_DOS_batch_languageElement('Variable', ATheme[Identifier]);

    SetLangINI_fileElement('Comment', ATheme[Comment]);
    SetLangINI_fileElement('Section', ATheme[ReservedWord]); //+ style???

    ForceDirectories(OutputFolder);
    SchemeFileName:=IncludeTrailingPathDelimiter(OutputFolder)+SchemeFileName;
    XmlDoc.Save(SchemeFileName);
    Result:=True;
  finally
    XmlDoc := Unassigned;
  end;
end;

function  ApplyLazarusIDETheme(const ATheme:TIDETheme;const ThemeName:string) : Boolean;
var
  OutPutFolder : String;
begin
  OutPutFolder:=IncludeTrailingPathDelimiter(GetLazarusLocalFolder)+'userschemes';
  ForceDirectories(OutPutFolder);

  Result:=DelphiIDEThemeToLazarusTheme(ATheme, ThemeName, OutPutFolder);
  if Result then
   Result:=SetEditorOptionsXMLValue('/CONFIG/EditorOptions/Color/LangObjectPascal/ColorScheme/@Value',MakeValidTagName(ThemeName));
end;


function  GetLazarusIDEThemeName : string;
begin
  Result:=GetEditorOptionsXMLValue('/CONFIG/EditorOptions/Color/LangObjectPascal/ColorScheme/@Value');
end;

function  DelphiIDEThemeToLazarusTheme(const DelphiIdeTheme, OutputFolder:string) : Boolean;overload;
var
 ATheme    : TIDETheme;
 ThemeName : string;
begin
  ThemeName:=MakeValidTagName(ChangeFileExt(ExtractFileName(DelphiIdeTheme),''));
  Result:=LoadThemeFromXMLFile(ATheme,DelphiIdeTheme);
  if Result then
   Result:=DelphiIDEThemeToLazarusTheme(ATheme, ThemeName, OutputFolder);
  if Result then
   Result:=SetEditorOptionsXMLValue('/CONFIG/EditorOptions/Color/LangObjectPascal/ColorScheme/@Value',MakeValidTagName(ThemeName));
end;


end.
