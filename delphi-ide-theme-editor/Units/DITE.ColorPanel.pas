﻿// **************************************************************************************************
//
// Unit DITE.ColorPanel
// Color Panel for the Delphi IDE Theme Editor
//
// The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
// you may not use this file except in compliance with the License. You may obtain a copy of the
// License at http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
// ANY KIND, either express or implied. See the License for the specific language governing rights
// and limitations under the License.
//
// The Original Code is uColorPanel.pas.
//
// The Initial Developer of the Original Code is Rodrigo Ruz V.
// Portions created by Rodrigo Ruz V. are Copyright (C) 2011-2023 Rodrigo Ruz V.
// All Rights Reserved.
//
// Based in the components of
// Marko Binić Color Lib  v2.0.2  http://mxs.bergsoft.net/index.php?p=3
//
// **************************************************************************************************

unit DITE.ColorPanel;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, mbDeskPickerButton, mbColorPickerControl,
  HRingPicker, HexaColorPicker, mbColorPreview, ImgList, HSVColorPicker,
  ComCtrls, mbColorTree, mbColorPalette, Tabs, mbTrackBarPicker, LColorPicker,
  HSColorPicker, HSLColorPicker, VColorPicker, pngimage, System.ImageList;

type
  TColorPanel = class(TForm)
    mbDeskPickerButton1: TmbDeskPickerButton;
    mbColorPreview1: TmbColorPreview;
    ImageList1: TImageList;
    Red: TEdit;
    Label1: TLabel;
    Green: TEdit;
    Label2: TLabel;
    Blue: TEdit;
    Label3: TLabel;
    mbColorPreview2: TmbColorPreview;
    PageControl1: TPageControl;
    TabSheetHexa: TTabSheet;
    HexaColorPicker1: THexaColorPicker;
    TabSheetHSV: TTabSheet;
    HSVColorPicker1: THSVColorPicker;
    VColorPicker1: TVColorPicker;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Hue: TEdit;
    Lum: TEdit;
    Sat: TEdit;
    Label7: TLabel;
    Hex: TEdit;
    PanelValues: TPanel;
    PageControl2: TPageControl;
    procedure mbDeskPickerButton1SelColorChange(Sender: TObject);
    procedure HSVColorPicker1Change(Sender: TObject);
    procedure VColorPicker1Change(Sender: TObject);
    procedure HexaColorPicker1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure RedExit(Sender: TObject);
    procedure HueExit(Sender: TObject);
    procedure HexKeyPress(Sender: TObject; var Key: Char);
    procedure HexExit(Sender: TObject);
  private
    FInitializating: Boolean;
    FSelectedColor: TColor;
    FStatus: Boolean;
    FRefreshHSVColorPicker: Boolean;
    FOnChange: TNotifyEvent;
    procedure RefreshColors(Acolor: TColor);
    procedure SetSelectedColor(const Value: TColor);
  public
    property SelectedColor: TColor read FSelectedColor write SetSelectedColor;
  published
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

uses
  GraphUtil;

{$R *.dfm}

procedure TColorPanel.FormCreate(Sender: TObject);
begin
  FInitializating := True;
  FStatus := False;
end;

procedure TColorPanel.FormShow(Sender: TObject);
begin
  FInitializating := False;
end;

function HexToTColor(const sColor: string): TColor;
begin
  Result := RGB(StrToInt('$' + Copy(sColor, 1, 2)), StrToInt('$' + Copy(sColor, 3, 2)),
    StrToInt('$' + Copy(sColor, 5, 2)));
end;

procedure TColorPanel.HexExit(Sender: TObject);
Var
  s: string;
  Value: Integer;
begin
  if Length(TEdit(Sender).Text) < 6 then
    TEdit(Sender).Text := TEdit(Sender).Text + StringOfChar('0', 6 - Length(TEdit(Sender).Text));

  s := '$' + TEdit(Sender).Text;
  if not TryStrToInt(s, Value) then
    TEdit(Sender).Text := 'FFFFFF';

  RefreshColors(HexToTColor(TEdit(Sender).Text));
end;

procedure TColorPanel.HexKeyPress(Sender: TObject; var Key: Char);
begin
  if not(CharInSet(Key, ['0' .. '9', 'a' .. 'f', 'A' .. 'F', #8, #3, #22])) then
    Key := #0;
end;

procedure TColorPanel.HexaColorPicker1Change(Sender: TObject);
begin
  if not FInitializating then
  begin
    FRefreshHSVColorPicker := False;
    HSVColorPicker1.SelectedColor := HexaColorPicker1.SelectedColor;
    FRefreshHSVColorPicker := True;
    RefreshColors(HexaColorPicker1.SelectedColor);
  end;
end;

procedure TColorPanel.HSVColorPicker1Change(Sender: TObject);
begin
  if not FInitializating then
    if FRefreshHSVColorPicker then
    begin
      VColorPicker1.SelectedColor := HSVColorPicker1.SelectedColor;
      HexaColorPicker1.SelectedColor := HSVColorPicker1.SelectedColor;
      // HSLColorPicker1.SelectedColor:=HSVColorPicker1.SelectedColor;
      // mbColorPalette1.SelectedColor:=HSVColorPicker1.SelectedColor;
      RefreshColors(VColorPicker1.SelectedColor);
    end;
end;

procedure TColorPanel.VColorPicker1Change(Sender: TObject);
begin
  if not FInitializating then
  begin
    FRefreshHSVColorPicker := False;
    try
      HSVColorPicker1.SelectedColor := VColorPicker1.SelectedColor;
      RefreshColors(HSVColorPicker1.SelectedColor);
    finally
      FRefreshHSVColorPicker := True;
    end;
  end;
end;

procedure TColorPanel.mbDeskPickerButton1SelColorChange(Sender: TObject);
begin
  if not FInitializating then
    RefreshColors(mbDeskPickerButton1.SelectedColor);
end;

procedure TColorPanel.RedExit(Sender: TObject);
const
  RGBMAX = 255; // R,G, and B vary over 0-RGBMAX
var
  Value: Integer;
  r, g, b: Byte;
  color: TColor;
begin
  if TEdit(Sender).Text = '' then
    TEdit(Sender).Text := '0';

  if TryStrToInt(TEdit(Sender).Text, Value) and (Value > RGBMAX) then
    TEdit(Sender).Text := IntToStr(RGBMAX);

  r := StrToInt(Red.Text);
  g := StrToInt(Green.Text);
  b := StrToInt(Blue.Text);
  color := RGB(r, g, b);

  RefreshColors(color);
end;

procedure TColorPanel.HueExit(Sender: TObject);
const
  HLSMAX = 240; // H,L, and S vary over 0-HLSMAX
var
  Value: Integer;
  LHue, Luminance, Saturation: Word;
begin
  if TEdit(Sender).Text = '' then
    TEdit(Sender).Text := '0';

  if TryStrToInt(TEdit(Sender).Text, Value) and (Value > HLSMAX) then
    TEdit(Sender).Text := IntToStr(HLSMAX);

  LHue := StrToInt(Self.Hue.Text);
  Luminance := StrToInt(Lum.Text);
  Saturation := StrToInt(Sat.Text);

  RefreshColors(ColorHLSToRGB(LHue, Luminance, Saturation));
end;

procedure TColorPanel.RefreshColors(Acolor: TColor);
var
  LHue, Luminance, Saturation: Word;
begin
  mbColorPreview1.color := Acolor;
  mbColorPreview2.color := Acolor;
  Red.Text := IntToStr(GetRValue(Acolor));
  Green.Text := IntToStr(GetGValue(Acolor));
  Blue.Text := IntToStr(GetBValue(Acolor));

  ColorRGBToHLS(ColorToRGB(Acolor), LHue, Luminance, Saturation);

  Self.Hue.Text := IntToStr(LHue);
  Self.Lum.Text := IntToStr(Luminance);
  Self.Sat.Text := IntToStr(Saturation);

  Hex.Text := Format('%.2x%.2x%.2x', [GetRValue(Acolor), GetGValue(Acolor), GetBValue(Acolor)]);

  FSelectedColor := Acolor;

  if { Assigned(CheckBoxLive) and CheckBoxLive.Checked and } (@FOnChange <> nil) then
    OnChange(Self);
end;

procedure TColorPanel.SetSelectedColor(const Value: TColor);
begin
  FSelectedColor := Value;
  if HexaColorPicker1 <> nil then
    HexaColorPicker1.SelectedColor := Value;
  // RefreshColors(Value);
end;

end.
