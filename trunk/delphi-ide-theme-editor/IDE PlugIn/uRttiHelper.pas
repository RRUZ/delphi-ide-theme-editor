//**************************************************************************************************
//
// Unit uRttiHelper
// unit uRttiHelper  for the Delphi IDE Colorizer
//
// The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
// you may not use this file except in compliance with the License. You may obtain a copy of the
// License at http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
// ANY KIND, either express or implied. See the License for the specific language governing rights
// and limitations under the License.
//
// The Original Code is uRttiHelper.pas.
//
// The Initial Developer of the Original Code is Rodrigo Ruz V.
// Portions created by Rodrigo Ruz V. are Copyright (C) 2011-2015 Rodrigo Ruz V.
// All Rights Reserved.
//
//**************************************************************************************************

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

 type
  TRttiUtils = class
   private
     class var FLocalContext: TRttiContext;
     class constructor Create;
     class destructor Destroy;
   public
      {$IF CompilerVersion > 20}
      class function DumpTypeDefinition(ATypeInfo: Pointer;OnlyDeclarated:Boolean=True) : string;
      class procedure DumpTypeToFile(const QualifiedName, FileName:string);
      class procedure DumpAllTypes(const FileName:string);
      class procedure DumpObject(AObject: TObject; const FileName: string);
      class procedure DumpRttiType(AType: TRttiType; const FileName: string);
      class procedure  SetRttiPropertyValue(const Obj:  TObject;const PropName:String; AValue:TValue);
      class function   GetRttiPropertyValue(const Obj:  TObject;const PropName:String): TValue;
      class procedure  SetRttiMemberValue(const Obj:  TObject;const MemberName:String; AValue:TValue; IsProp : Boolean);
      class function   GetRttiMemberValue(const Obj:  TObject;const MemberName:String; IsProp : Boolean) : TValue;
      class function   GetRttiFieldValue(const Obj:  TObject;const FieldName:String): TValue;
      class procedure  SetRttiFieldValue(const Obj:  TObject;const FieldName:String; AValue:TValue);
      class procedure  ExecMethodRtti(const Obj:  TObject;const Method:String);
      {$ELSE}
      class procedure  SetRttiPropertyValue(const Obj:  TObject;const PropName:String;  Value:Variant);
      {$IFEND}
  end;



implementation

{$IF CompilerVersion > 20}

class constructor TRttiUtils.Create;
begin
  FLocalContext := TRttiContext.Create;
end;


class destructor TRttiUtils.Destroy;
begin
  FLocalContext.Free;
end;

class procedure TRttiUtils.DumpTypeToFile(const QualifiedName, FileName:string);
var
  LTypeDump : TStrings;
begin
  LTypeDump := TStringList.Create;
  try
    LTypeDump.Text:=DumpTypeDefinition(TRttiContext.Create.FindType(QualifiedName).Handle);
    LTypeDump.SaveToFile(FileName);
  finally
   LTypeDump.Free;
  end;
end;

class procedure TRttiUtils.DumpAllTypes(const FileName:string);
var
  LTypes : TStrings;
  LRttiType  : TRttiType;
begin
  LTypes := TStringList.Create;
  try
    for LRttiType in TRttiContext.Create.GetTypes do
    if LRttiType.IsInstance then
     LTypes.Add(LRttiType.AsInstance.DeclaringUnitName +' '+LRttiType.Name);
   LTypes.SaveToFile(FileName);
  finally
   LTypes.Free;
  end;
end;

class procedure TRttiUtils.DumpObject(AObject: TObject; const FileName: string);
var
 LDumpInfo : TStrings;
begin
   LDumpInfo := TStringList.Create;
  try
   LDumpInfo.Text:=DumpTypeDefinition(AObject.ClassInfo);
   LDumpInfo.SaveToFile(FileName);
  finally
   LDumpInfo.Free;
  end;
end;

class procedure  TRttiUtils.DumpRttiType(AType: TRttiType; const FileName: string);
var
 LDumpInfo : TStrings;
begin
   LDumpInfo := TStringList.Create;
  try
   LDumpInfo.Text:=TRttiUtils.DumpTypeDefinition(AType.Handle);
   LDumpInfo.SaveToFile(FileName);
  finally
   LDumpInfo.Free;
  end;
end;

class function TRttiUtils.DumpTypeDefinition(ATypeInfo: Pointer;OnlyDeclarated:Boolean=True) : string;

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
  lType     : TRttiType;
  lMethod   : TRttiMethod;
  lProperty : TRttiProperty;
  lField    : TRttiField;
  Definition: TObjectDictionary<string, TStringList>;
  i         : TMemberVisibility;
  k : integer;
begin

   Result:='No Rtti Information';
   Definition:= TObjectDictionary<string, TStringList>.Create([doOwnsValues]);
   try

     if not Assigned(ATypeInfo) then exit;
     lType:=FLocalContext.GetType(ATypeInfo);
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
       tkEnumeration:
                     begin
                         Definition.Items[sType].Add(Format('%s%s=(',[sIndent,lType.Name]));

                          for k := lType.AsOrdinal.MinValue to lType.AsOrdinal.MaxValue do
                           if k<lType.AsOrdinal.MaxValue then
                            Definition.Items[sType].Add(Format('%s%s,',[sIndent,GetEnumName(lType.Handle, k)]))
                           else
                            Definition.Items[sType].Add(Format('%s%s',[sIndent,GetEnumName(lType.Handle, k)]));


                         Definition.Items[sType].Add(Format('%s)',[sIndent]));
                     end;

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
       tkArray      : begin



                      end;


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

     if lType.TypeKind<>tkEnumeration   then
        Definition.Items[sType].Add(sIndent+'end;');

     Result:=Definition.Items[sType].Text;
   finally
    Definition.free;
   end;
end;
{$IFEND}

{$IF CompilerVersion > 20}

class procedure TRttiUtils.SetRttiMemberValue(const Obj:  TObject;const MemberName:String; AValue:TValue; IsProp : Boolean);
var
  LProperty, RootProp    : TRttiProperty;
  LField       : TRttiField;
  LInstance    : Pointer;
  MemberList   : TStringList;
  i            : Integer;
begin
  LProperty:=nil;
  LField   :=nil;
  MemberList:=TStringList.Create;
  try
    MemberList.Delimiter:='.';
    MemberList.DelimitedText:=MemberName;
    LInstance:=Obj;

    //search the first member in the properties list
    if MemberList.Count>0 then
     LProperty := FLocalContext.GetType(Obj.ClassInfo).GetProperty(MemberList[0]);

    //search the first member in the field list
    if not Assigned(LProperty) then
     LField := FLocalContext.GetType(Obj.ClassInfo).GetField(MemberList[0]);

    for i:=1 to MemberList.Count-1 do
     begin
        //Get the instance to the property
        if Assigned(LProperty) and (LProperty.PropertyType.TypeKind=tkClass) then
         LInstance          := LProperty.GetValue(LInstance).AsObject
        else
        //Get the instance to the field
        if Assigned(LField) and (LField.FieldType.TypeKind=tkClass) then
         LInstance          := LField.GetValue(LInstance).AsObject
        else
        raise Exception.Create(Format('The member %s is not a class',[MemberList[i]]));

        if Assigned(LProperty) then
        begin
          RootProp  := LProperty;
          //search the current member in the properties list
          LProperty := FLocalContext.GetType(LProperty.PropertyType.Handle).GetProperty(MemberList[i]);
           //search the current member in the field list
           if not Assigned(LProperty)  then
             LField := FLocalContext.GetType(RootProp.PropertyType.Handle).GetField(MemberList[i]);
        end
        else
        if Assigned(LField) then
        begin
          //search the current member in the properties list
          LProperty  := FLocalContext.GetType(LField.FieldType.Handle).GetProperty(MemberList[i]);
           //search the current member in the field list
           if not Assigned(LProperty)  then
              LField := FLocalContext.GetType(LField.FieldType.Handle).GetField(MemberList[i]);
        end;
     end;

    if IsProp and Assigned(LProperty) and Assigned(LInstance) then
      LProperty.SetValue(LInstance, AValue)
    else
    if (not IsProp) and Assigned(LField) and Assigned(LInstance) then
      LField.SetValue(LInstance, AValue);

  finally
    MemberList.Free;
  end;
end;

class function TRttiUtils.GetRttiMemberValue(const Obj:  TObject;const MemberName:String; IsProp : Boolean) : TValue;
var
  LProperty, RootProp    : TRttiProperty;
  LField       : TRttiField;
  LInstance    : Pointer;
  MemberList      : TStringList;
  i            : Integer;
begin
  LProperty:=nil;
  LField   :=nil;
  MemberList:=TStringList.Create;
  try
    MemberList.Delimiter:='.';
    MemberList.DelimitedText:=MemberName;
    LInstance:=Obj;

    //search the first member in the properties list
    if MemberList.Count>0 then
     LProperty := FLocalContext.GetType(Obj.ClassInfo).GetProperty(MemberList[0]);

    //search the first member in the field list
    if not Assigned(LProperty) then
     LField := FLocalContext.GetType(Obj.ClassInfo).GetField(MemberList[0]);

    for i:=1 to MemberList.Count-1 do
     begin
        //Get the instance to the property
        if Assigned(LProperty) and (LProperty.PropertyType.TypeKind=tkClass) then
         LInstance          := LProperty.GetValue(LInstance).AsObject
        else
        //Get the instance to the field
        if Assigned(LField) and (LField.FieldType.TypeKind=tkClass) then
         LInstance          := LField.GetValue(LInstance).AsObject
        else
        raise Exception.Create(Format('The member %s is not a class',[MemberList[i]]));

        if Assigned(LProperty) then
        begin
          RootProp  := LProperty;
          //search the current member in the properties list
          LProperty := FLocalContext.GetType(LProperty.PropertyType.Handle).GetProperty(MemberList[i]);
           //search the current member in the field list
           if not Assigned(LProperty)  then
             LField := FLocalContext.GetType(RootProp.PropertyType.Handle).GetField(MemberList[i]);
        end
        else
        if Assigned(LField) then
        begin
          //search the current member in the properties list
          LProperty  := FLocalContext.GetType(LField.FieldType.Handle).GetProperty(MemberList[i]);
           //search the current member in the field list
           if not Assigned(LProperty)  then
              LField := FLocalContext.GetType(LField.FieldType.Handle).GetField(MemberList[i]);
        end;
     end;

    if IsProp and Assigned(LProperty) and Assigned(LInstance) then
      Result:= LProperty.GetValue(LInstance)
    else
    if (not IsProp) and Assigned(LField) and Assigned(LInstance) then
      Result:= LField.GetValue(LInstance);
  finally
    MemberList.Free;
  end;
end;

class procedure TRttiUtils.SetRttiPropertyValue(const Obj:  TObject;const PropName:String; AValue:TValue);
begin
  SetRttiMemberValue(Obj, PropName, AValue, True);
end;

class procedure TRttiUtils.SetRttiFieldValue(const Obj:  TObject;const FieldName:String; AValue:TValue);
begin
  SetRttiMemberValue(Obj, FieldName, AValue, False);
end;

class function TRttiUtils.GetRttiPropertyValue(const Obj:  TObject;const PropName:String): TValue;
begin
  Result:=GetRttiMemberValue(Obj, PropName, True);
end;

class function TRttiUtils.GetRttiFieldValue(const Obj:  TObject;const FieldName:String): TValue;
begin
  Result:=GetRttiMemberValue(Obj, FieldName, False);
end;

class procedure TRttiUtils.ExecMethodRtti(const Obj:  TObject;const Method:String);
var
  m : TRttiMethod;
begin
  m:=FLocalContext.GetType(Obj.ClassInfo).GetMethod(Method);
  if m<>nil then
    m.Invoke(Obj, []);
end;

{$ELSE}
class constructor TRttiUtils.Create;
begin
  inherited;
end;

class destructor TRttiUtils.Destroy;
begin
  inherited;
end;

class procedure TRttiUtils.SetRttiPropertyValue(const Obj:  TObject;const PropName:String;  Value:Variant);
var
  RttiProperty     : PPropInfo;
  LObject          : TObject;
  MainProp         : String;
  ChildProp        : String;
  vType            : Integer;

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
    RttiProperty := GetPropInfo(Obj.ClassInfo, PropName);
    SetValue(Obj);
  end
  else
  begin
    MainProp     := Copy(PropName,1,Pos('.',PropName)-1);
    ChildProp    := Copy(PropName,Pos('.',PropName)+1);
    LObject      := Obj;
    RttiProperty := GetPropInfo(LObject.ClassInfo, MainProp);
    if Assigned(RttiProperty) and (RttiProperty.PropType^.Kind in [tkClass]) then
    begin
       LObject:=TObject(GetOrdProp(LObject, RttiProperty));
       if Assigned(LObject) then
       begin
         RttiProperty := GetPropInfo(LObject, ChildProp);
         SetValue(LObject);
       end;
    end;
  end;
end;
{$IFEND}


end.
