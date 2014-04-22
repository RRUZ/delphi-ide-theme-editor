//**************************************************************************************************
//
// Unit uStoreColorMap
// unit uStoreColorMap  for the Delphi IDE Colorizer
//
// The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
// you may not use this file except in compliance with the License. You may obtain a copy of the
// License at http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
// ANY KIND, either express or implied. See the License for the specific language governing rights
// and limitations under the License.
//
// The Original Code is uStoreColorMap.pas.
//
// The Initial Developer of the Original Code is Rodrigo Ruz V.
// Portions created by Rodrigo Ruz V. are Copyright (C) 2011-2014 Rodrigo Ruz V.
// All Rights Reserved.
//
//**************************************************************************************************

unit uStoreColorMap;

interface

uses
  ActnColorMaps;

procedure  LoadColorMapFromXmlFile(AColorMap : TXPColorMap;const FileName:string);
procedure  SaveColorMapToXmlFile(AColorMap : TXPColorMap;const FileName:string);


implementation

uses
  XMLDoc,
  ComObj,
  XMLIntf,
  TypInfo,
  Graphics,
  Dialogs,
  SysUtils,
  Variants;

const
  Msxml2_DOMDocument='Msxml2.DOMDocument.6.0';

procedure  LoadColorMapFromXmlFile(AColorMap : TXPColorMap;const FileName:string);
var
  PropName  : string;
  XmlDocIDETheme : OleVariant;
  Node :  OleVariant;
  xPathElement   : string;
  Count, Index: Integer;
  Properties  : TPropList;
begin
  XmlDocIDETheme       := CreateOleObject(Msxml2_DOMDocument);
  XmlDocIDETheme.Async := False;
  try
    XmlDocIDETheme.Load(FileName);
    XmlDocIDETheme.SetProperty('SelectionLanguage','XPath');
    if (XmlDocIDETheme.parseError.errorCode <> 0) then
     raise Exception.CreateFmt('Error in Delphi IDE Colorizer theme Xml Data %s',[XmlDocIDETheme.parseError]);

    Count := GetPropList(TypeInfo(TXPColorMap), tkAny, @Properties);
      for Index := 0 to Pred(Count) do
       if SameText(String(Properties[Index]^.PropType^.Name),'TColor') then
        begin
          PropName:=String(Properties[Index]^.Name);
          //AColor :=GetOrdProp(AColorMap,PropName);
          xPathElement:=Format('/DelphiColorizerTheme/ColorMap/%s',[PropName]);
          Node:=XmlDocIDETheme.selectSingleNode(xPathElement);
          if not VarIsClear(Node) then
           SetOrdProp(AColorMap,PropName,StringToColor(Node.text));
        end;
  finally
   XmlDocIDETheme    :=Unassigned;
  end;
end;


procedure  SaveColorMapToXmlFile(AColorMap : TXPColorMap;const FileName:string);
var
  PropName  : string;
  AColor    : TColor;
  Doc       : TXMLDocument;
  RootNode, ChildNode, oNode : IXMLNode;
  Count, Index: Integer;
  Properties  : TPropList;
begin
  Doc   :=TXMLDocument.Create(nil);
  try
    Doc.Active := True;
    Doc.Version:='1.0';
    Doc.Encoding:='utf-8';
    Doc.Options := [doNodeAutoIndent];
    RootNode    := Doc.AddChild('DelphiColorizerTheme');
    RootNode.Attributes['modified'] := FormatDateTime('YYYY-MM-DD HH:NN:SS',Now);
    RootNode.Attributes['author']   := 'Delphi IDE Theme Colorizer';
    RootNode.Attributes['versionapp']  := '1.0.0.0';//GetFileVersion(ParamStr(0));
    ChildNode := RootNode.AddChild('ColorMap');
    Count := GetPropList(TypeInfo(TXPColorMap), tkAny, @Properties);
      for Index := 0 to Pred(Count) do
       if SameText(String(Properties[Index]^.PropType^.Name),'TColor') then
        begin
          PropName:=String(Properties[Index]^.Name);
          AColor :=GetOrdProp(AColorMap,PropName);
          oNode  := ChildNode.AddChild(PropName);
          oNode.Text:=ColorToString(AColor);
        end;
    Doc.SaveToFile(FileName);
  finally
   Doc:=nil;
  end;
end;

end.
