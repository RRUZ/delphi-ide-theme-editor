{**************************************************************************************************}
{                                                                                                  }
{ Unit EclipseThemes                                                                               }
{ Import Eclipse IDE themes                                                                        }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is EclipseThemes.pas.                                                          }
{                                                                                                  }
{ The Initial Developer of the Original Code is Rodrigo Ruz V.                                     }
{ Portions created by Rodrigo Ruz V. are Copyright (C) 2011-2013 Rodrigo Ruz V.                    }
{ All Rights Reserved.                                                                             }
{                                                                                                  }
{**************************************************************************************************}

unit EclipseThemes;

interface

uses
  SysUtils,
  ComObj,
  Variants,
  Graphics,
  uDelphiIDEHighlight,
  uDelphiVersions;


function  ImportEclipseTheme(DelphiVersion:TDelphiVersions;const FileName,Path:TFileName;var ThemeName:string):boolean;


implementation

function  ImportEclipseTheme(DelphiVersion:TDelphiVersions;const FileName,Path:TFileName;var ThemeName:string):boolean;
var
  XmlDocEclipseTheme         : OleVariant;
  NewTheme                   : TIDETheme;
  EclipseColor               : string;
  EclipseBackGround          : string;
  EclipseForeGround          : string;

  function WebColorToTColor(const S: string): string;
  var I: Integer;
  begin
    try
      I :=StrToInt(S);
    except
      I :=0;
    end;
    Result :=ColorToString((I AND $0000FF00) OR ((I AND $00FF0000) SHR 16) OR ((I AND $000000FF) SHL 16));
  end;

  function GetDataEclipseTheme(const ElementName:String) :Boolean;
  var
    ElementVariant : OleVariant;
  begin
   Result:= False;
   ElementVariant:=XmlDocEclipseTheme.selectSingleNode(Format('%s%s/%s',['//colorTheme/',ElementName,'@color']));
   if (not VarIsClear(ElementVariant)) and (not VarIsNull(ElementVariant)) then
   begin
    EclipseColor:=ElementVariant.text;
    Result:=True;
   end;
  end;

  Procedure SetIDEHighlightElement(Element:TIDEHighlightElements;const EclipseElement:String);
  begin
      if GetDataEclipseTheme(EclipseElement) then
      begin
          NewTheme[Element].Bold:=False;
          NewTheme[Element].Italic:=False;
          NewTheme[Element].Underline:=False;
          NewTheme[Element].DefaultForeground:=False;
          NewTheme[Element].DefaultBackground:=False;
          NewTheme[Element].ForegroundColorNew:=WebColorToTColor(StringReplace(EclipseColor,'#','$',[rfReplaceAll]));
          NewTheme[Element].BackgroundColorNew:=EclipseBackGround;
      end
      else
          NewTheme[Element].BackgroundColorNew:=EclipseBackGround;
  end;

begin
  Result:=False;
  NewTheme:=GetDelphiIDEDefaultTheme(DelphiVersion);
  XmlDocEclipseTheme       := CreateOleObject(Msxml2_DOMDocument);
  XmlDocEclipseTheme.Async := False;
  try
    XmlDocEclipseTheme.Load(FileName);
    XmlDocEclipseTheme.SetProperty('SelectionLanguage','XPath');
    if (XmlDocEclipseTheme.parseError.errorCode <> 0) then
     raise Exception.CreateFmt('Error in Eclipse Theme Xml Data %s',[XmlDocEclipseTheme.parseError]);

    GetDataEclipseTheme('background');
    EclipseBackGround:=WebColorToTColor(StringReplace(EclipseColor,'#','$',[rfReplaceAll]));

    GetDataEclipseTheme('foreground');
    EclipseForeGround:=WebColorToTColor(StringReplace(EclipseColor,'#','$',[rfReplaceAll]));

    SetIDEHighlightElement(TIDEHighlightElements.AdditionalSearchMatchHighlight,'Empty');
    NewTheme[TIDEHighlightElements.AdditionalSearchMatchHighlight].DefaultForeground:=True;
    if GetDataEclipseTheme('selectionBackground') then
    NewTheme[TIDEHighlightElements.AdditionalSearchMatchHighlight].BackgroundColorNew:=WebColorToTColor(StringReplace(EclipseColor,'#','$',[rfReplaceAll]));

    SetIDEHighlightElement(TIDEHighlightElements.Assembler,'deprecatedMember');
    SetIDEHighlightElement(TIDEHighlightElements.AttributeNames,'annotatione');
    SetIDEHighlightElement(TIDEHighlightElements.AttributeValues,'annotation');

    SetIDEHighlightElement(TIDEHighlightElements.BraceHighlight,'bracket');
    SetIDEHighlightElement(TIDEHighlightElements.Character,'string');
    SetIDEHighlightElement(TIDEHighlightElements.CodeFoldingTree,'bracket');
    SetIDEHighlightElement(TIDEHighlightElements.Comment,'multiLineComment');
    SetIDEHighlightElement(TIDEHighlightElements.DiffAddition,'Empty');
    SetIDEHighlightElement(TIDEHighlightElements.DiffDeletion,'Empty');
    SetIDEHighlightElement(TIDEHighlightElements.DiffMove,'Empty');
    //SetIDEHighlightElement(TIDEHighlightElements.DisabledBreak,'Empty');
    //SetIDEHighlightElement(TIDEHighlightElements.EnabledBreak,'Empty');
    //SetIDEHighlightElement(TIDEHighlightElements.ErrorLine,'Empty');
    //SetIDEHighlightElement(TIDEHighlightElements.ExecutionPoint,'Empty');
    SetIDEHighlightElement(TIDEHighlightElements.Float,'number');
    SetIDEHighlightElement(TIDEHighlightElements.FoldedCode,'Empty');
    SetIDEHighlightElement(TIDEHighlightElements.Hex,'number');
    SetIDEHighlightElement(TIDEHighlightElements.HotLink,'javadocLink');
    SetIDEHighlightElement(TIDEHighlightElements.Identifier,'localVariableDeclaration');
    SetIDEHighlightElement(TIDEHighlightElements.IllegalChar,'deprecatedMember');
    //SetIDEHighlightElement(TIDEHighlightElements.InvalidBreak,'Empty');


    if GetDataEclipseTheme('selectionBackground') then
    NewTheme[TIDEHighlightElements.LineHighlight].BackgroundColorNew:=WebColorToTColor(StringReplace(EclipseColor,'#','$',[rfReplaceAll]));
    NewTheme[TIDEHighlightElements.LineHighlight].DefaultForeground:=True;


    SetIDEHighlightElement(TIDEHighlightElements.LineNumber,'lineNumber');

    if GetDataEclipseTheme('selectionBackground') then
    NewTheme[TIDEHighlightElements.MarkedBlock].BackgroundColorNew:=WebColorToTColor(StringReplace(EclipseColor,'#','$',[rfReplaceAll]));
    if GetDataEclipseTheme('selectionForeground') then
    NewTheme[TIDEHighlightElements.MarkedBlock].ForegroundColorNew:=WebColorToTColor(StringReplace(EclipseColor,'#','$',[rfReplaceAll]));


    SetIDEHighlightElement(TIDEHighlightElements.ModifiedLine,'Empty');
    SetIDEHighlightElement(TIDEHighlightElements.Number,'number');
    SetIDEHighlightElement(TIDEHighlightElements.Octal,'number');
    SetIDEHighlightElement(TIDEHighlightElements.PlainText,'foreground');

    SetIDEHighlightElement(TIDEHighlightElements.Preprocessor,'javadocTag');
    SetIDEHighlightElement(TIDEHighlightElements.ReservedWord,'keyword');
    SetIDEHighlightElement(TIDEHighlightElements.RightMargin,'foreground');
    SetIDEHighlightElement(TIDEHighlightElements.Scripts,'Empty');

    SetIDEHighlightElement(TIDEHighlightElements.SearchMatch,'searchResultIndication');

    SetIDEHighlightElement(TIDEHighlightElements.String,'string');
    SetIDEHighlightElement(TIDEHighlightElements.Symbol,'operator');
    SetIDEHighlightElement(TIDEHighlightElements.SyncEditBackground,'Empty');
    SetIDEHighlightElement(TIDEHighlightElements.SyncEditHighlight,'Empty');
    SetIDEHighlightElement(TIDEHighlightElements.Tags,'Empty');
    SetIDEHighlightElement(TIDEHighlightElements.Whitespace,'foreground');

    ThemeName:=Copy(ExtractFileName(FileName),1,Pos('.xml',ExtractFileName(FileName))-1);
    //SaveDelphiIDEThemeToXmlFile(TDelphiVersions.DelphiXE,NewTheme,Path,ThemeName);
    SaveDelphiIDEThemeToXmlFile(NewTheme,Path,ThemeName);
    Result:=True;
  finally
   XmlDocEclipseTheme    :=Unassigned;
  end;
end;


end.
