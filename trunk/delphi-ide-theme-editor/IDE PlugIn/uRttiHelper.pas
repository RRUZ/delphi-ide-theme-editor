{**************************************************************************************************}
{                                                                                                  }
{ Unit uRttiHelper                                                                                 }
{ unit uRttiHelper  for the Delphi IDE Colorizer                                                   }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is uRttiHelper.pas.                                                            }
{                                                                                                  }
{ The Initial Developer of the Original Code is Rodrigo Ruz V.                                     }
{ Portions created by Rodrigo Ruz V. are Copyright (C) 2011 Rodrigo Ruz V.                         }
{ All Rights Reserved.                                                                             }
{                                                                                                  }
{**************************************************************************************************}

unit uRttiHelper;

interface

uses
{$IF CompilerVersion > 20}
  Rtti,
  Generics.Collections,
{$IFEND}
  Variants,
  TypInfo,
  Classes,
  SysUtils;


{$IF CompilerVersion > 20}
function   DumpTypeDefinition(ATypeInfo: Pointer;OnlyDeclarated:Boolean=False) : string;
procedure  SetRttiPropertyValue(const Obj:  TObject;const PropName:String; AValue:TValue);
function   GetRttiPropertyValue(const Obj:  TObject;const PropName:String): TValue;
{$ELSE}
procedure  SetRttiPropertyValue(const Obj:  TObject;const PropName:String;  Value:Variant);
{$IFEND}



implementation

{$IF CompilerVersion > 20}
var
  ctx: TRttiContext;
{$IFEND}


{$IF CompilerVersion > 20}
function  DumpTypeDefinition(ATypeInfo: Pointer;OnlyDeclarated:Boolean=False) : string;

  //add and format a field
  procedure AddField(List:TStrings;lField : TRttiField);
  begin
     if Assigned(lField.FieldType) then
      List.Add((Format('   %-20s:%s;',[lField.Name,lField.FieldType.Name])))
     else
      List.Add((Format('   %-20s:%s;',[lField.Name,'Unknow'])));
  end;

  //add and format a method
  procedure AddMethod(List:TStrings;lMethod : TRttiMethod);
  begin
     List.Add((Format('   %s;',[lMethod.ToString])));
  end;

  //add and format a Property
  procedure AddProperty(List:TStrings;lProperty : TRttiProperty);
  begin
     List.Add((Format('   %s;',[lProperty.ToString])));
  end;

const
 sType          = 'type';
 sIndent        = '  ';
 ArrVisibility  : Array[TMemberVisibility] of string = ('private','protected','public','published');//Helper array for Visibility
var
  ctx       : TRttiContext;
  lType     : TRttiType;
  lMethod   : TRttiMethod;
  lProperty : TRttiProperty;
  lField    : TRttiField;
  Definition: TObjectDictionary<string, TStringList>;
  i         : TMemberVisibility;
begin

   Result:='No Rtti Information';
   ctx       := TRttiContext.Create;
   Definition:= TObjectDictionary<string, TStringList>.Create([doOwnsValues]);
   try

     if not Assigned(ATypeInfo) then exit;
     lType:=ctx.GetType(ATypeInfo);
     if not Assigned(lType) then exit;

     Definition.Add(sType,TStringList.Create);
     Definition.Items[sType].Add('type');

     //Initialize the buffers to hold the data
     for i:=Low(TMemberVisibility) to High(TMemberVisibility) do
     begin
      Definition.Add(ArrVisibility[i]  ,TStringList.Create);
      Definition.Items[ArrVisibility[i]].Add(sIndent+ArrVisibility[i]);
     end;

     case lType.TypeKind of
       tkUnknown    : ;
       tkInteger    : ;
       tkChar       : ;
       tkEnumeration: ;
       tkFloat      : ;
       tkString     : ;
       tkSet        : ;
       tkClass      :
                     begin
                       //get the main definition
                       //Definition.Items[sType].Add('//Unit Name '+lType.QualifiedName);

                       if Assigned(lType.BaseType) then
                        Definition.Items[sType].Add(Format('%s%s=class(%s)',[sIndent,lType.Name,lType.BaseType.Name]))
                       else
                        Definition.Items[sType].Add(Format('%s%s=class',[sIndent,lType.Name]));
                     end;
       tkMethod     : ;
       tkWChar      : ;
       tkLString    : ;
       tkWString    : ;
       tkVariant    : ;
       tkArray      : ;
       tkRecord     : begin
                       //get the main definition
                        Definition.Items[sType].Add(Format('%s%s=record',[sIndent,lType.Name]));
                      end;

       tkInterface  :
                     begin
                       //get the main definition
                       if Assigned(lType.BaseType) then
                        Definition.Items[sType].Add(Format('%s%s=Interface(%s)',[sIndent,lType.Name,lType.BaseType.Name]))
                       else
                        Definition.Items[sType].Add(Format('%s%s=Interface',[sIndent,lType.Name]));

                     end;
       tkInt64      : ;
       tkDynArray   : ;
       tkUString    : ;
       tkClassRef   : ;
       tkPointer    : ;
       tkProcedure  : ;
     end;

       //add the fields
       if OnlyDeclarated then
         for lField in lType.GetDeclaredFields do
           AddField(Definition.Items[ArrVisibility[lField.Visibility]],lField)
       else
         for lField in lType.GetFields do
           AddField(Definition.Items[ArrVisibility[lField.Visibility]],lField);

       //add the methods
       if OnlyDeclarated then
         for lMethod in lType.GetDeclaredMethods do
           AddMethod(Definition.Items[ArrVisibility[lMethod.Visibility]],lMethod)
       else
         for lMethod in lType.GetMethods do
           AddMethod(Definition.Items[ArrVisibility[lMethod.Visibility]],lMethod);

       //add the Properties
       if OnlyDeclarated then
         for lProperty in lType.GetDeclaredProperties do
           AddProperty(Definition.Items[ArrVisibility[lProperty.Visibility]],lProperty)
       else
         for lProperty in lType.GetProperties do
           AddProperty(Definition.Items[ArrVisibility[lProperty.Visibility]],lProperty);

     for i:=Low(TMemberVisibility) to High(TMemberVisibility) do
      if Definition.Items[ArrVisibility[i]].Count>1 then
       Definition.Items[sType].AddStrings(Definition.Items[ArrVisibility[i]]);

     Definition.Items[sType].Add(sIndent+'end;');
     Result:=Definition.Items[sType].Text;
   finally
    Definition.free;
    ctx.free;
   end;
end;
{$IFEND}

{$IF CompilerVersion > 20}
procedure  SetRttiPropertyValue(const Obj:  TObject;const PropName:String; AValue:TValue);
var
  RttiProperty : TRttiProperty;
  Instance     : Pointer;
  Props        : TStringList;
  i            : integer;
begin
  Props:=TStringList.Create;
  try
    Props.Delimiter:='.';
    Props.DelimitedText:=PropName;
    Instance:=Obj;

    if Props.Count>0 then
     RttiProperty := ctx.GetType(Obj.ClassInfo).GetProperty(Props[0]);

    for i:=1 to Props.Count-1 do
     begin
        if Assigned(RttiProperty) and (RttiProperty.PropertyType.TypeKind=tkClass) then
         Instance          := RttiProperty.GetValue(Instance).AsObject
        else
        raise Exception.Create(Format('The property %s is not a class',[Props[i]]));

        RttiProperty := ctx.GetType(RttiProperty.PropertyType.Handle).GetProperty(Props[i]);
     end;

    if Assigned(RttiProperty) then
      RttiProperty.SetValue(Instance, AValue);
  finally
    Props.Free;
  end;
end;

function  GetRttiPropertyValue(const Obj:  TObject;const PropName:String): TValue;
var
  RttiProperty     : TRttiProperty;
  Instance         : Pointer;
  Props            : TStringList;
  i                : integer;
begin
  Props:=TStringList.Create;
  try
    Props.Delimiter:='.';
    Props.DelimitedText:=PropName;
    Instance:=Obj;

    if Props.Count>0 then
     RttiProperty := ctx.GetType(Obj.ClassInfo).GetProperty(Props[0]);

    for i:=1 to Props.Count-1 do
     begin
        if Assigned(RttiProperty) and (RttiProperty.PropertyType.TypeKind=tkClass) then
         Instance          := RttiProperty.GetValue(Instance).AsObject
        else
        raise Exception.Create(Format('The property %s is not a class',[Props[i]]));
          RttiProperty := ctx.GetType(RttiProperty.PropertyType.Handle).GetProperty(Props[i]);
     end;

    if Assigned(RttiProperty) then
      Result:= RttiProperty.GetValue(Instance);
  finally
    Props.Free;
  end;
end;

{$ELSE}
procedure  SetRttiPropertyValue(const Obj:  TObject;const PropName:String;  Value:Variant);
var
  RttiProperty     : PPropInfo;
  Obj              : TObject;
  MainProp         : String;
  ChildProp        : String;
  vType            : Integer;
  //PropInfos        : PPropList;
  //Count,i          : Integer;
  //Ok               : Boolean;


  Procedure SetValue(Instance : TObject);
  begin
    vType := VarType(Value) and VarTypeMask;
    if Assigned(RttiProperty) then
    case vType of
      varDispatch,
      varError,
      varVariant,
      varUnknown,
      varEmpty,
      varNull,
      varAny,
      varTypeMask  : ;

      varSmallInt,
      varInteger,
      varBoolean,
      varByte,
      varWord,
      varLongWord,
      varInt64     : SetOrdProp(Instance, RttiProperty, Value);

      varSingle,
      varDouble,
      varCurrency,
      varDate      : SetFloatProp(Instance, RttiProperty, Value);

      varOleStr,
      varStrArg,
      varString    : SetStrProp(Instance, RttiProperty, Value);
    end;

  end;

begin
  if Pos('.',PropName)=0 then
  begin
    RttiProperty := GetPropInfo(AComponent.ClassInfo, PropName);
    SetValue(AComponent);
  end
  else
  begin
    MainProp     := Copy(PropName,1,Pos('.',PropName)-1);
    ChildProp    := Copy(PropName,Pos('.',PropName)+1);
    Obj:=AComponent;
    RttiProperty := GetPropInfo(Obj.ClassInfo, MainProp);
    if Assigned(RttiProperty) and (RttiProperty.PropType^.Kind in [tkClass]) then
    begin
       Obj:=TObject(GetOrdProp(Obj, RttiProperty));
       if Assigned(Obj) then
       begin
         RttiProperty := GetPropInfo(Obj, ChildProp);
         SetValue(Obj);
       end;
    end;
  end;
end;
{$IFEND}


initialization

{$IF CompilerVersion > 20}
  ctx:=TRttiContext.Create;
{$IFEND}

finalization
{$IF CompilerVersion > 20}
  ctx.Free;
{$IFEND}

end.
