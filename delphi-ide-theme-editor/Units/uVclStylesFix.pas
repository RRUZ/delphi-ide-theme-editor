//**************************************************************************************************
//
// Unit uVclStylesFix
// unit uVclStylesFix for the Delphi IDE Theme Editor
//
// The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
// you may not use this file except in compliance with the License. You may obtain a copy of the
// License at http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
// ANY KIND, either express or implied. See the License for the specific language governing rights
// and limitations under the License.
//
// The Original Code is uVclStylesFix.pas.
//
// The Initial Developer of the Original Code is Rodrigo Ruz V.
// Portions created by Rodrigo Ruz V. are Copyright (C) 2011-2014 Rodrigo Ruz V.
// All Rights Reserved.
//
//**************************************************************************************************

unit uVclStylesFix;

interface

uses
 Vcl.Graphics,
 Vcl.Forms,
 Vcl.Themes,
 Vcl.Styles;

type
  TMyClass = class(TFormStyleHook)
  protected
   procedure PaintBackground(Canvas: TCanvas); override;
  end;



implementation

uses
  Vcl.Styles.Ext,
  Vcl.StdCtrls,
  SynEdit,
  Types,
  SysUtils,
  Classes;


{ TMyClass }

procedure TMyClass.PaintBackground(Canvas: TCanvas);
var
  Details: TThemedElementDetails;
  R: TRect;
begin
  if StyleServices.Available then
  begin
    Details.Element := teWindow;
    Details.Part := 0;
    R := Rect(0, 0, Control.ClientWidth, Control.ClientHeight);
    //if (GetWindowLong(Form.Handle,GWL_EXSTYLE) AND WS_EX_TRANSPARENT) = WS_EX_TRANSPARENT  then
     if Form.Brush.Style = bsClear then
      Exit;
      StyleServices.DrawElement(Canvas.Handle, Details, R);
  end;
end;

initialization
   if not IsStyleHookRegistered(TCustomSynEdit, TScrollingStyleHook) then
     TStyleManager.Engine.RegisterStyleHook(TCustomSynEdit, TScrollingStyleHook);
  TStyleManager.Engine.RegisterStyleHook(TCustomForm, TMyClass);
  TStyleManager.Engine.RegisterStyleHook(TForm, TMyClass);
end.
