{**************************************************************************************************}
{                                                                                                  }
{ Unit uVCLStyleUtils                                                                              }
{ unit uVCLStyleUtils  for the Delphi IDE Colorizer                                                }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is uVCLStyleUtils.pas.                                                         }
{                                                                                                  }
{ The Initial Developer of the Original Code is Rodrigo Ruz V.                                     }
{ Portions created by Rodrigo Ruz V. are Copyright (C) 2011 Rodrigo Ruz V.                         }
{ All Rights Reserved.                                                                             }
{                                                                                                  }
{**************************************************************************************************}
unit uVCLStyleUtils;

interface

{$IF CompilerVersion >= 23}
Uses
  Rtti,
  Vcl.Themes,
  Vcl.Styles,
  Classes;

Procedure ApplyEmptyVCLStyleHook(ControlClass :TClass);
Procedure RemoveEmptyVCLStyleHook(ControlClass :TClass);
function  IsStyleHookRegistered(ControlClass: TClass; StyleHookClass: TStyleHookClass) : Boolean;
{$IFEND}

implementation


{$IF CompilerVersion >= 23}
uses
 Generics.Collections;

type
  TStyleHookList = TList<TStyleHookClass>;
  TStyleHookDictionary = TDictionary<TClass, TStyleHookList>;
  TCustomStyleEngineHack = Class Helper for TCustomStyleEngine
  public
    class function GetRegisteredStyleHooks : TStyleHookDictionary;
  END;


class function TCustomStyleEngineHack.GetRegisteredStyleHooks: TStyleHookDictionary;
begin
  Result:= Self.FRegisteredStyleHooks;
end;

function  IsStyleHookRegistered(ControlClass: TClass; StyleHookClass: TStyleHookClass) : Boolean;
var
  List    : TStyleHookList;
begin
 Result:=False;
    if TCustomStyleEngine.GetRegisteredStyleHooks.ContainsKey(ControlClass) then
    begin
      List := TCustomStyleEngine.GetRegisteredStyleHooks[ControlClass];
      Result:=List.IndexOf(StyleHookClass) <> -1;
    end;
end;

Procedure RestoreVCLStyleHook(ControlClass :TClass);
{
var
  List    : TStyleHookList;
}
begin
{
    if TCustomStyleEngine.GetRegisteredStyleHooks.ContainsKey(AClass) then
    begin
      List := TCustomStyleEngine.GetRegisteredStyleHooks[AClass];
      if List.IndexOf(TStyleHook) <> -1 then exit;
    end;
    TStyleManager.Engine.RegisterStyleHook(AClass, TStyleHook);
}
end;

Procedure ApplyEmptyVCLStyleHook(ControlClass :TClass);
begin
   if not IsStyleHookRegistered(ControlClass, TStyleHook) then
    TStyleManager.Engine.RegisterStyleHook(ControlClass, TStyleHook);
end;

Procedure RemoveEmptyVCLStyleHook(ControlClass :TClass);
begin
   if IsStyleHookRegistered(ControlClass, TStyleHook) then
    TStyleManager.Engine.UnRegisterStyleHook(ControlClass, TStyleHook);
end;


{$IFEND}
end.
