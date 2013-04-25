{**************************************************************************************************}
{                                                                                                  }
{ Unit VSThemes                                                                                    }
{ Import visual studio themes                                                                      }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is VSThemes.pas.                                                               }
{                                                                                                  }
{ The Initial Developer of the Original Code is Rodrigo Ruz V.                                     }
{ Portions created by Rodrigo Ruz V. are Copyright (C) 2011 Rodrigo Ruz V.                         }
{ All Rights Reserved.                                                                             }
{                                                                                                  }
{**************************************************************************************************}
unit VSThemes;

interface

uses
  SysUtils,
  ComObj,
  Variants,
  uDelphiIDEHighlight,
  uDelphiVersions;

function  ImportVisualStudioTheme(DelphiVersion:TDelphiVersions;const FileName,Path:TFileName;var ThemeName:string):boolean;


implementation

const
 sEmpty='Empty';

function  ImportVisualStudioTheme(DelphiVersion:TDelphiVersions;const FileName,Path:TFileName;var ThemeName:string):boolean;
var
  XmlDocVSTheme              : OleVariant;
  Nodes                      : OleVariant;
  lNodes                     : Integer;

  Categories                 : OleVariant;
  lCategories                : Integer;

  xPathElement               : string;
  Foreground                 : string;
  Background                 : string;
  BoldFont                   : string;

  BackgroundPatch            : string;
  ForegroundPatch            : string;

  NewTheme                   : TIDETheme;

  function GetDataVSTheme(const ElementName:String) :Boolean;
  var
    i              : Integer;
    ElementVariant : OleVariant;
  begin
   Result:= False;
   if ElementName=sEmpty then Exit;

      for i := 1 to lNodes do
      begin
        xPathElement:='//UserSettings/Category/Category/FontsAndColors/Categories/Category/Items[1]/Item[%d]/';
        xPathElement:=Format(xPathElement,[i]);
        ElementVariant:=XmlDocVSTheme.selectSingleNode(Format('%s%s',[xPathElement,'@Name']));

        if not VarIsNull(ElementVariant) and not VarIsClear(ElementVariant) then
        if CompareText(ElementVariant.text,ElementName)=0 then
        begin
            //OutputDebugString(PChar(VarToStr(ElementVariant.text)));
            Foreground:=XmlDocVSTheme.selectSingleNode(Format('%s%s',[xPathElement,'@Foreground'])).text;
            Background:=XmlDocVSTheme.selectSingleNode(Format('%s%s',[xPathElement,'@Background'])).text;
            BoldFont  :=XmlDocVSTheme.selectSingleNode(Format('%s%s',[xPathElement,'@BoldFont'])).text;
            Result:=True;
            Break;
        end;
      end;

{
        //UserSettings/Category/Category/FontsAndColors/Categories/Category/Items[1]/Item[@Name="Comment"]
        xPathElement:='UserSettings/Category/Category/FontsAndColors/Categories/Category/Items[1]/Item[@Name="%s"]';
        xPathElement:=Format(xPathElement,[ElementName]);
        ElementVariant:=XmlDocVSTheme.selectSingleNode(xPathElement);
        if not VarIsNull(ElementVariant) and not VarIsClear(ElementVariant) then
        begin
          for i := 0 to ElementVariant.Attributes.length - 1 do
           if CompareText(ElementVariant.Attributes.Item(i).nodeName,'Foreground')=0 then
             Foreground:=ElementVariant.Attributes.Item(i).nodeValue
           else
           if CompareText(ElementVariant.Attributes.Item(i).nodeName,'Background')=0 then
             Background:=ElementVariant.Attributes.Item(i).nodeValue
           else
           if CompareText(ElementVariant.Attributes.Item(i).nodeName,'BoldFont')=0 then
             BoldFont:=ElementVariant.Attributes.Item(i).nodeValue;
          Result:=True;
        end;
}

{
        xPathElement:='UserSettings/Category/Category/FontsAndColors/Categories/Category/Items[1]/Item[@Name="%s"]';
        xPathElement:=Format(xPathElement,[ElementName]);
        ElementVariant:=XmlDocVSTheme.selectSingleNode(xPathElement);
        if not VarIsNull(ElementVariant) and not VarIsClear(ElementVariant) then
        begin
          Foreground:=ElementVariant.getAttribute('Foreground');
          Background:=ElementVariant.getAttribute('Background');
          BoldFont  :=ElementVariant.getAttribute('BoldFont');
          Result:=True;
        end;
}
  end;

  Procedure SetIDEHighlightElement(Element:TIDEHighlightElements;const VsElement:String);
  var
    UseBackgroundPatch:Boolean;
    UseForegroundPatch:Boolean;
  begin

      if GetDataVSTheme(VsElement) then
      begin
          UseBackgroundPatch:=(Background='0x02000000');
          UseForegroundPatch:=(Foreground='0x02000000');

          NewTheme[Element].Bold:=BoldFont='Yes';
          NewTheme[Element].Italic:=False;
          NewTheme[Element].Underline:=False;
          NewTheme[Element].DefaultForeground:=False;
          NewTheme[Element].DefaultBackground:=False;
          //NewTheme[Element].ForegroundColorNew:=StringReplace(Foreground,'0x','$',[rfReplaceAll]);

          if UseBackgroundPatch then
            NewTheme[Element].BackgroundColorNew:=BackgroundPatch
          else
            NewTheme[Element].BackgroundColorNew:=StringReplace(Background,'0x','$',[rfReplaceAll]);

          if UseForegroundPatch then
            NewTheme[Element].ForegroundColorNew:=ForegroundPatch
          else
            NewTheme[Element].ForegroundColorNew:=StringReplace(Foreground,'0x','$',[rfReplaceAll]);

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
  Result:=False;
  NewTheme:=GetDelphiIDEDefaultTheme(DelphiVersion);
  XmlDocVSTheme       := CreateOleObject(Msxml2_DOMDocument);
  XmlDocVSTheme.Async := False;
  try
    XmlDocVSTheme.Load(FileName);
    XmlDocVSTheme.SetProperty('SelectionLanguage','XPath');
    if (XmlDocVSTheme.parseError.errorCode <> 0) then
     raise Exception.CreateFmt('Error in Visual Studio Xml Data %s',[XmlDocVSTheme.parseError]);


    Categories := XmlDocVSTheme.selectNodes('//UserSettings/Category/Category/FontsAndColors/Categories/Category');
    lCategories:= Categories.Length;
    if lCategories>1 then
     raise Exception.CreateFmt('Visual Studio theme with multiples Categories %s',['is not supported']);

    //UserSettings/Category/Category/FontsAndColors/Categories/Category/Items
    Nodes := XmlDocVSTheme.selectNodes('//UserSettings/Category/Category/FontsAndColors/Categories/Category/Items[1]/Item');
    lNodes:= Nodes.Length;

    GetDataVSTheme('Plain Text');
    BackgroundPatch:=StringReplace(Background,'0x','$',[rfReplaceAll]);
    ForegroundPatch:=StringReplace(Foreground,'0x','$',[rfReplaceAll]);

    SetIDEHighlightElement(TIDEHighlightElements.AdditionalSearchMatchHighlight,'Selected Text');
    NewTheme[TIDEHighlightElements.AdditionalSearchMatchHighlight].DefaultForeground:=True;

    SetIDEHighlightElement(TIDEHighlightElements.Assembler,'Script Keyword');//sEmpty);
    SetIDEHighlightElement(TIDEHighlightElements.AttributeNames,'HTML Attribute');
    SetIDEHighlightElement(TIDEHighlightElements.AttributeValues,'HTML Attribute Value');
    SetIDEHighlightElement(TIDEHighlightElements.BraceHighlight,'Brace Matching (Rectangle)');
    SetIDEHighlightElement(TIDEHighlightElements.Character,'String');
    //SetIDEHighlightElement(TIDEHighlightElements.CodeFoldingTree,'Collapsible Text');
    SetIDEHighlightElement(TIDEHighlightElements.CodeFoldingTree,'Plain Text');
    SetIDEHighlightElement(TIDEHighlightElements.Comment,'Comment');
    SetIDEHighlightElement(TIDEHighlightElements.DiffAddition,sEmpty);
    SetIDEHighlightElement(TIDEHighlightElements.DiffDeletion,sEmpty);
    SetIDEHighlightElement(TIDEHighlightElements.DiffMove,sEmpty);
    SetIDEHighlightElement(TIDEHighlightElements.DisabledBreak,sEmpty);
    SetIDEHighlightElement(TIDEHighlightElements.EnabledBreak,sEmpty);
    SetIDEHighlightElement(TIDEHighlightElements.ErrorLine,sEmpty);
    SetIDEHighlightElement(TIDEHighlightElements.ExecutionPoint,sEmpty);
    SetIDEHighlightElement(TIDEHighlightElements.Float,'Number');
    SetIDEHighlightElement(TIDEHighlightElements.FoldedCode,'Brace Matching (Rectangle)');
    SetIDEHighlightElement(TIDEHighlightElements.Hex,'Number');
    SetIDEHighlightElement(TIDEHighlightElements.HotLink,'urlformat');
    SetIDEHighlightElement(TIDEHighlightElements.Identifier,'Plain Text');//'Identifier');
    SetIDEHighlightElement(TIDEHighlightElements.IllegalChar,'Syntax Error');
    SetIDEHighlightElement(TIDEHighlightElements.InvalidBreak,sEmpty);

    SetIDEHighlightElement(TIDEHighlightElements.LineHighlight,'Selected Text');
    NewTheme[TIDEHighlightElements.LineHighlight].DefaultForeground:=True;

    //SetIDEHighlightElement(TIDEHighlightElements.LineNumber,'Line Numbers');
    SetIDEHighlightElement(TIDEHighlightElements.LineNumber,'Plain Text');


    SetIDEHighlightElement(TIDEHighlightElements.MarkedBlock,'Selected Text');
    NewTheme[TIDEHighlightElements.MarkedBlock].DefaultForeground:=True;

    SetIDEHighlightElement(TIDEHighlightElements.ModifiedLine,sEmpty);
    SetIDEHighlightElement(TIDEHighlightElements.Number,'Number');
    SetIDEHighlightElement(TIDEHighlightElements.Octal,'Number');
    SetIDEHighlightElement(TIDEHighlightElements.PlainText,'Plain Text');
    SetIDEHighlightElement(TIDEHighlightElements.Preprocessor,'Preprocessor Keyword');
    SetIDEHighlightElement(TIDEHighlightElements.ReservedWord,'Keyword');
    SetIDEHighlightElement(TIDEHighlightElements.RightMargin,'Indicator Margin');
    SetIDEHighlightElement(TIDEHighlightElements.Scripts,sEmpty);
    SetIDEHighlightElement(TIDEHighlightElements.SearchMatch,'Selected Text');
    SetIDEHighlightElement(TIDEHighlightElements.String,'String');
    SetIDEHighlightElement(TIDEHighlightElements.Symbol,'Plain Text');//sEmpty);
    SetIDEHighlightElement(TIDEHighlightElements.SyncEditBackground,sEmpty);
    SetIDEHighlightElement(TIDEHighlightElements.SyncEditHighlight,sEmpty);
    SetIDEHighlightElement(TIDEHighlightElements.Tags,sEmpty);
    SetIDEHighlightElement(TIDEHighlightElements.Whitespace,'Plain Text');
    //Doc.SaveToFile(Result);
    ThemeName:=Copy(ExtractFileName(FileName),1,Pos('.vssettings',ExtractFileName(FileName))-1);
    //SaveDelphiIDEThemeToXmlFile(TDelphiVersions.DelphiXE,NewTheme,Path,ThemeName);
    SaveDelphiIDEThemeToXmlFile(NewTheme,Path,ThemeName);
    Result:=True;
  finally
   XmlDocVSTheme    :=Unassigned;
  end;
end;


end.
