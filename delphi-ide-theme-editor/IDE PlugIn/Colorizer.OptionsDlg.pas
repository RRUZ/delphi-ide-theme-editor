//**************************************************************************************************
//
// Unit Colorizer.OptionsDlg
// unit Colorizer.OptionsDlg  for the Delphi IDE Colorizer
//
// The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
// you may not use this file except in compliance with the License. You may obtain a copy of the
// License at http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
// ANY KIND, either express or implied. See the License for the specific language governing rights
// and limitations under the License.
//
// The Original Code is Colorizer.OptionsDlg.pas.
//
// The Initial Developer of the Original Code is Rodrigo Ruz V.
// Portions created by Rodrigo Ruz V. are Copyright (C) 2011-2014 Rodrigo Ruz V.
// All Rights Reserved.
//
//**************************************************************************************************
unit Colorizer.OptionsDlg;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, ToolsAPI, Vcl.StdCtrls;

type
  TFrameColorizer = class(TFrame)
  private
    { Private declarations }
   public
     constructor Create(AOwner: TComponent) ; override;
   end;

  TIDEColorizerOTAExpertOptions = class(TInterfacedObject, INTAAddinOptions)
  private
    FCaption: string;
    FTitle: string;
  public
    constructor Create(const ACaption, ATitle: string);
    { INTAAddinOptions }
    function GetArea: string;
    function GetCaption: string;
    function GetFrameClass: TCustomFrameClass;
    procedure FrameCreated(AFrame: TCustomFrame);
    procedure DialogClosed(Accepted: Boolean);
    function ValidateContents: Boolean;
    function GetHelpContext: Integer;
    function IncludeInIDEInsight: Boolean;
  end;

    procedure RegisterColorizerAddinOptions;
    procedure UnRegisterColorizerAddinOptions;

implementation

uses
  uMisc,
  ShellApi,
  uDelphiVersions,
  Colorizer.SettingsForm;

type
  TFormClass=class (TForm);

{$R *.dfm}

constructor TFrameColorizer.Create(AOwner: TComponent);
var
  LForm : TFormIDEColorizerSettings;
begin
  inherited  Create(AOwner);
  LForm := TFormIDEColorizerSettings.Create(AOwner);
  LForm.Name   := 'DelphiIDEColorizer_SettingsForm';
  LForm.IDEData:= TDelphiVersionData.Create; //leak
  FillCurrentDelphiVersion(LForm.IDEData);
  LForm.LabelSetting.Caption:='Delphi IDE Colorizer for '+LForm.IDEData.Name;
  LForm.Parent := Self;
  LForm.Align := alClient;
  LForm.BorderIcons := [];
  LForm.BorderStyle := bsNone;
  TFormClass(LForm).ParentBackground := True;
  LForm.Init;
  LForm.Show;
end;


{ TIDEColorizerOTAExpertOptions }

constructor TIDEColorizerOTAExpertOptions.Create(const ACaption,
  ATitle: string);
begin
  inherited Create;
  FCaption := ACaption;
  FTitle := ATitle;
end;

procedure TIDEColorizerOTAExpertOptions.DialogClosed(Accepted: Boolean);
begin

end;

procedure TIDEColorizerOTAExpertOptions.FrameCreated(AFrame: TCustomFrame);
begin
 // TFrameColorizer(AFrame).Label1.Caption := FTitle;
end;

function TIDEColorizerOTAExpertOptions.GetArea: string;
begin
  Result := '';
end;

function TIDEColorizerOTAExpertOptions.GetCaption: string;
begin
  Result := FCaption;
end;

function TIDEColorizerOTAExpertOptions.GetFrameClass: TCustomFrameClass;
begin
  Result := TFrameColorizer;
end;

function TIDEColorizerOTAExpertOptions.GetHelpContext: Integer;
begin
  Result := 0;
end;

function TIDEColorizerOTAExpertOptions.IncludeInIDEInsight: Boolean;
begin
  Result := True;
end;

function TIDEColorizerOTAExpertOptions.ValidateContents: Boolean;
begin
  Result := True;
end;

var
  IDEColorizerOTAExpertOptions: INTAAddinOptions = nil;

procedure RegisterColorizerAddinOptions;
begin
  if not Assigned(IDEColorizerOTAExpertOptions) then
  begin
    IDEColorizerOTAExpertOptions := TIDEColorizerOTAExpertOptions.Create('Delphi IDE Colorizer', '-');
    (BorlandIDEServices as INTAEnvironmentOptionsServices).RegisterAddInOptions(IDEColorizerOTAExpertOptions);
  end;
end;

procedure UnRegisterColorizerAddinOptions;
begin
  if Assigned(IDEColorizerOTAExpertOptions) then
  begin
    (BorlandIDEServices as INTAEnvironmentOptionsServices).UnregisterAddInOptions(IDEColorizerOTAExpertOptions);
    IDEColorizerOTAExpertOptions := nil;
  end;
end;

end.
