// **************************************************************************************************
//
// Unit DITE.AdditionalSettings
// unit DITE.AdditionalSettings for the Delphi IDE Theme Editor
//
// The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
// you may not use this file except in compliance with the License. You may obtain a copy of the
// License at http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
// ANY KIND, either express or implied. See the License for the specific language governing rights
// and limitations under the License.
//
// The Original Code is uAdditionalSettings.pas.
//
// The Initial Developer of the Original Code is Rodrigo Ruz V.
// Portions created by Rodrigo Ruz V. are Copyright (C) 2011-2023 Rodrigo Ruz V.
// All Rights Reserved.
//
// **************************************************************************************************
unit DITE.AdditionalSettings;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls, Vcl.ImgList,
  Vcl.ExtCtrls, DITE.DelphiVersions, System.ImageList;

type
  TFrmAdditionalSettings = class(TForm)
    Label1: TLabel;
    Label5: TLabel;
    CbIDEFonts: TComboBox;
    EditFontSize: TEdit;
    UpDownFontSize: TUpDown;
    CblForeground: TColorBox;
    BtnSelForColor: TButton;
    ImageList1: TImageList;
    BtnApply: TButton;
    BtnRestore: TButton;
    BtnCancel: TButton;
    procedure FormCreate(Sender: TObject);
    procedure BtnSelForColorClick(Sender: TObject);
    procedure BtnCancelClick(Sender: TObject);
    procedure BtnRestoreClick(Sender: TObject);
    procedure BtnApplyClick(Sender: TObject);
    procedure CblForegroundGetColors(Sender: TCustomColorBox; Items: TStrings);
  private
    FIDEData: TDelphiVersionData;
    { Private declarations }
    procedure LoadFonts;
    procedure RestoreModernThemeData;
    procedure SetModernThemeData;
    procedure LoadModernThemeData;
    procedure SetIDEData(const Value: TDelphiVersionData);
  public
    { Public declarations }
    property IDEData: TDelphiVersionData read FIDEData write SetIDEData;
  end;

implementation

uses
  System.UITypes,
  //Vcl.Styles.Hooks,
  Vcl.GraphUtil,
  DITE.DelphiIDEHighlight,
  DITE.ColorSelector,
  DITE.Misc;

{$R *.dfm}
{ TFrmAdditionalSettings }

procedure TFrmAdditionalSettings.BtnApplyClick(Sender: TObject);
begin
  if MessageDlg(Format('Do you want apply the current settings to the %s IDE?', [IDEData.Name]), mtConfirmation,
    [mbYes, mbNo], 0) = mrYes then
  begin
    SetModernThemeData();
    Close();
  end;
end;

procedure TFrmAdditionalSettings.BtnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TFrmAdditionalSettings.BtnRestoreClick(Sender: TObject);
begin
  if MessageDlg(Format('Do you want restore the settings of the %s IDE?', [IDEData.Name]), mtConfirmation,
    [mbYes, mbNo], 0) = mrYes then
  begin
    RestoreModernThemeData();
    Close();
  end;
end;

procedure TFrmAdditionalSettings.BtnSelForColorClick(Sender: TObject);
var
  Frm: TDialogColorSelector;
  OldColor: TColor;
begin
  Frm := TDialogColorSelector.Create(Self);
  try
    OldColor := CblForeground.Selected;
    // Frm.OnChange:=OnSelForegroundColorChange;
    Frm.SelectedColor := CblForeground.Selected;
    if Frm.Execute then
    begin
      CblForeground.Selected := Frm.SelectedColor;
      // CblForegroundChange(CblForeground);
    end
    else if CblForeground.Selected <> OldColor then
    begin
      CblForeground.Selected := OldColor;
      // CblForegroundChange(CblForeground);
    end;
  finally
    Frm.Free;
  end;
end;

const
  Colors: array [0 .. 51] of TIdentMapEntry = ((
    Value: TColors.Black; Name: 'clBlack'), (Value: TColors.Maroon;Name: 'clMaroon'),
    (Value: TColors.Green; Name: 'clGreen'), (Value: TColors.Olive; Name: 'clOlive'),
    (Value: TColors.Navy; Name: 'clNavy'), (Value: TColors.Purple; Name: 'clPurple'),
    (Value: TColors.Teal; Name: 'clTeal'), (Value: TColors.Gray; Name: 'clGray'),
    (Value: TColors.Silver; Name: 'clSilver'), (Value: TColors.Red; Name: 'clRed'),
    (Value: TColors.Lime; Name: 'clLime'), (Value: TColors.Yellow; Name: 'clYellow'),
    (Value: TColors.Blue; Name: 'clBlue'), (Value: TColors.Fuchsia; Name: 'clFuchsia'),
    (Value: TColors.Aqua; Name: 'clAqua'), (Value: TColors.White; Name: 'clWhite'),

    (Value: TColors.MoneyGreen; Name: 'clMoneyGreen'),
    // Use LegacySkyBlue to maintain consistency in VCL colors
    (Value: TColors.LegacySkyBlue; Name: 'clSkyBlue'), (Value: TColors.Cream; Name: 'clCream'),
    (Value: TColors.MedGray; Name: 'clMedGray'), (Value: TColors.SysActiveBorder; Name: 'clActiveBorder'),
    (Value: TColors.SysActiveCaption; Name: 'clActiveCaption'), (Value: TColors.SysAppWorkSpace; Name: 'clAppWorkSpace'),
    (Value: TColors.SysBackground; Name: 'clBackground'), (Value: TColors.SysBtnFace; Name: 'clBtnFace'),
    (Value: TColors.SysBtnHighlight; Name: 'clBtnHighlight'), (Value: TColors.SysBtnShadow; Name: 'clBtnShadow'),
    (Value: TColors.SysBtnText; Name: 'clBtnText'), (Value: TColors.SysCaptionText; Name: 'clCaptionText'),
    (Value: TColors.SysDefault; Name: 'clDefault'), (Value: TColors.SysGradientActiveCaption; Name: 'clGradientActiveCaption'),
    (Value: TColors.SysGradientInactiveCaption; Name: 'clGradientInactiveCaption'), (Value: TColors.SysGrayText; Name: 'clGrayText'),
    (Value: TColors.SysHighlight; Name: 'clHighlight'), (Value: TColors.SysHighlightText; Name: 'clHighlightText'),
    (Value: TColors.SysHotLight; Name: 'clHotLight'), (Value: TColors.SysInactiveBorder; Name: 'clInactiveBorder'),
    (Value: TColors.SysInactiveCaption; Name: 'clInactiveCaption'), (Value: TColors.SysInactiveCaptionText; Name: 'clInactiveCaptionText'),
    (Value: TColors.SysInfoBk; Name: 'clInfoBk'), (Value: TColors.SysInfoText; Name: 'clInfoText'),
    (Value: TColors.SysMenu; Name: 'clMenu'), (Value: TColors.SysMenuBar; Name: 'clMenuBar'),
    (Value: TColors.SysMenuHighlight; Name: 'clMenuHighlight'), (Value: TColors.SysMenuText; Name: 'clMenuText'),
    (Value: TColors.SysNone; Name: 'clNone'), (Value: TColors.SysScrollBar; Name: 'clScrollBar'),
    (Value: TColors.Sys3DDkShadow; Name: 'cl3DDkShadow'), (Value: TColors.Sys3DLight; Name: 'cl3DLight'),
    (Value: TColors.SysWindow; Name: 'clWindow'), (Value: TColors.SysWindowFrame; Name: 'clWindowFrame'),
    (Value: TColors.SysWindowText; Name: 'clWindowText')
    );

function _ColorToRGB(Color: TColor): Longint;
begin
//  if Color < 0 then
//    Result := Trampoline_user32_GetSysColor(Color and $000000FF)
//  else
    Result := Color;
end;

procedure TFrmAdditionalSettings.CblForegroundGetColors(Sender: TCustomColorBox; Items: TStrings);
var
  Item: TIdentMapEntry;
begin
  for Item in Colors do
    Items.AddObject(StringReplace(Item.Name, 'cl', '', [rfReplaceAll]), TObject(_ColorToRGB(Item.Value)));

  for Item in WebNamedColors do
    Items.AddObject(StringReplace(Item.Name, 'clWeb', '', [rfReplaceAll]), TObject(Item.Value));
end;

procedure TFrmAdditionalSettings.FormCreate(Sender: TObject);
begin
  LoadFonts;
end;

procedure TFrmAdditionalSettings.LoadFonts;
var
  sDC: integer;
  LogFont: TLogFont;
begin
  CbIDEFonts.Items.Clear;
  sDC := GetDC(0);
  try
    ZeroMemory(@LogFont, sizeof(LogFont));
    LogFont.lfCharset := DEFAULT_CHARSET;
    EnumFontFamiliesEx(sDC, LogFont, @EnumFontsProc, LPARAM(CbIDEFonts.Items), 0);
  finally
    ReleaseDC(0, sDC);
  end;
end;

procedure TFrmAdditionalSettings.LoadModernThemeData;
var
  LModernTheme: TModernTheme;
begin
  LModernTheme := TModernTheme.Create(FIDEData);
  try
    LModernTheme.LoadData;
    CbIDEFonts.ItemIndex := CbIDEFonts.Items.IndexOf(LModernTheme.FontName);
    UpDownFontSize.Position := LModernTheme.FontSize;
    CblForeground.Selected := _ColorToRGB(StringToColor(LModernTheme.MainToolBarColor));
  finally
    LModernTheme.Free;
  end;
end;

procedure TFrmAdditionalSettings.RestoreModernThemeData;
var
  LModernTheme: TModernTheme;
begin
  LModernTheme := TModernTheme.Create(FIDEData);
  try
    LModernTheme.RestoreData;
  finally
    LModernTheme.Free;
  end;
end;

procedure TFrmAdditionalSettings.SetIDEData(const Value: TDelphiVersionData);
begin
  FIDEData := Value;
  LoadModernThemeData();
end;

procedure TFrmAdditionalSettings.SetModernThemeData;
var
  LModernTheme: TModernTheme;
begin
  LModernTheme := TModernTheme.Create(FIDEData);
  try
    LModernTheme.FontName := CbIDEFonts.Text;
    LModernTheme.FontSize := UpDownFontSize.Position;
    LModernTheme.MainToolBarColor := ColorToString(CblForeground.Selected);
    LModernTheme.WriteData;
  finally
    LModernTheme.Free;
  end;
end;

end.
