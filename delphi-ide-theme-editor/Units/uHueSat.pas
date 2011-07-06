{**************************************************************************************************}
{                                                                                                  }
{ Unit uHueSat                                                                                     }
{ unit uHueSat  for the Delphi IDE Theme Editor                                                    }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is uHueSat.pas.                                                                }
{                                                                                                  }
{ The Initial Developer of the Original Code is Rodrigo Ruz V.                                     }
{ Portions created by Rodrigo Ruz V. are Copyright (C) 2011 Rodrigo Ruz V.                         }
{ All Rights Reserved.                                                                             }
{                                                                                                  }
{**************************************************************************************************}


unit uHueSat;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Mask, JvExMask, JvSpin, ComCtrls, uDelphiVersions,
  SynEdit,
  uSettings,
  uHSLUtils,
  uDelphiIDEHighlight,
  SynEditHighlighter,
  SynHighlighterPas,
  Generics.Defaults,
  Generics.Collections;

type
  TFrmHueSat = class(TForm)
    BtnApply:    TButton;
    Bevel1:      TBevel;
    TrackBarHue: TTrackBar;
    JvSpinEditLight: TJvSpinEdit;
    JvSpinEditSat: TJvSpinEdit;
    JvSpinEditHue: TJvSpinEdit;
    ButtonLightness: TButton;
    TrackBarLightness: TTrackBar;
    ButtonSaturation: TButton;
    TrackBarSaturation: TTrackBar;
    ButtonHue:   TButton;
    Bevel3:      TBevel;
    Label3:      TLabel;
    Bevel2:      TBevel;
    Label2:      TLabel;
    Bevel4:      TBevel;
    Label1:      TLabel;
    BtnSaveAs:   TButton;
    procedure JvSpinEditHueChange(Sender: TObject);
    procedure JvSpinEditLightChange(Sender: TObject);
    procedure JvSpinEditSatChange(Sender: TObject);
    procedure ButtonHueClick(Sender: TObject);
    procedure ButtonSaturationClick(Sender: TObject);
    procedure ButtonLightnessClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TrackBarHueChange(Sender: TObject);
    procedure TrackBarLightnessChange(Sender: TObject);
    procedure TrackBarSaturationChange(Sender: TObject);
    procedure BtnApplyClick(Sender: TObject);
    procedure BtnSaveAsClick(Sender: TObject);
  private
    FSynEditor: TSynEdit;
    FColorList: TColorList;
    FHueColorList: TColorList;
    FThemeName: string;
    FSettings: TSettings;
    FDelphiVersion: TDelphiVersions;
    FTheme: TIDETheme;
    FReloadThemes: boolean;
    procedure Saturation(Value: integer);
    procedure Lightness(Value: integer);
    procedure Hue(Value: integer);
    procedure SetTheme(const Value: TIDETheme);
  public
    property SynEditor: TSynEdit Read FSynEditor Write FSynEditor;
    property DelphiVersion: TDelphiVersions Read FDelphiVersion Write FDelphiVersion;
    property ThemeName: string Read FThemeName Write FThemeName;
    property Theme: TIDETheme Read FTheme Write SetTheme;
    property Settings: TSettings Read FSettings Write FSettings;
    property Reloadthemes: boolean Read FReloadThemes;
    procedure init;
  end;



implementation

{$R *.dfm}


procedure ApplyHueSaturationToIDETheme(var ATheme: TIDETheme;
  Hue, Saturation, Lightness: integer);
var
  Element:   TIDEHighlightElements;
  ColorList: TColorList;
begin

  ColorList := TColorList.Create;
  try

     {
     InvalidBreakLine   : Done:=SetCbElement(TIDEHighlightElements.InvalidBreak);
     ExecutionPointLine : Done:=SetCbElement(TIDEHighlightElements.ExecutionPoint);
     EnabledBreakLine   : Done:=SetCbElement(TIDEHighlightElements.EnabledBreak);
     DisabledBreakLine  : Done:=SetCbElement(TIDEHighlightElements.DisabledBreak);
     ErrorLineLine      : Done:=SetCbElement(TIDEHighlightElements.ErrorLine);
     }

    for Element in [Low(TIDEHighlightElements)..High(TIDEHighlightElements)] do
      if not (Element in [InvalidBreak, ExecutionPoint, EnabledBreak,
        DisabledBreak, ErrorLine]) then
      begin
        ColorList.Clear;
        ColorList.Add(StringToColor(ATheme[Element].ForegroundColorNew));
        ColorList.Add(StringToColor(ATheme[Element].BackgroundColorNew));

        if Hue >= 0 then
          _Hue(ColorList, Hue)
        else
        if Hue < 0 then
          _Hue(ColorList, 360 - Abs(Hue));


        if Saturation <> 0 then
          _Saturation(ColorList, (255 - ((Saturation * 255) div MaxSat)));

        if Lightness <> 0 then
        begin
          if Lightness >= 0 then
            _Lightness(ColorList, Lightness)
          else
            _Darkness(ColorList, Abs(Lightness));
        end;

        ATheme[Element].ForegroundColorNew := ColorToString(ColorList[0]);
        ATheme[Element].BackgroundColorNew := ColorToString(ColorList[1]);
      end;
  finally
    ColorList.Free;
  end;
end;



procedure GetColorsSynEdit(SynEditor: TSynEdit; ColorList: TColorList);

  procedure AddColor(AColor: TColor);
  begin
    // if ColorList.IndexOf(AColor)<0 then
    ColorList.Add(AColor);
  end;

  procedure AddColorSynAttr(SynAttr: TSynHighlighterAttributes);
  begin
    AddColor(SynAttr.Foreground);
    AddColor(SynAttr.Background);
  end;

begin
  ColorList.Clear;

  AddColor(SynEditor.ActiveLineColor);
  AddColor(SynEditor.RightEdgeColor);
  AddColor(SynEditor.SelectedColor.Foreground);
  AddColor(SynEditor.SelectedColor.Background);
  AddColor(SynEditor.Gutter.Color);
  AddColor(SynEditor.Gutter.Font.Color);
  AddColorSynAttr(TSynPasSyn(SynEditor.Highlighter).AsmAttri);
  AddColorSynAttr(TSynPasSyn(SynEditor.Highlighter).CharAttri);
  AddColorSynAttr(TSynPasSyn(SynEditor.Highlighter).CommentAttri);
  AddColorSynAttr(TSynPasSyn(SynEditor.Highlighter).DirectiveAttri);
  AddColorSynAttr(TSynPasSyn(SynEditor.Highlighter).FloatAttri);
  AddColorSynAttr(TSynPasSyn(SynEditor.Highlighter).HexAttri);
  AddColorSynAttr(TSynPasSyn(SynEditor.Highlighter).IdentifierAttri);
  AddColorSynAttr(TSynPasSyn(SynEditor.Highlighter).KeyAttri);
  AddColorSynAttr(TSynPasSyn(SynEditor.Highlighter).NumberAttri);
  AddColorSynAttr(TSynPasSyn(SynEditor.Highlighter).SpaceAttri);
  AddColorSynAttr(TSynPasSyn(SynEditor.Highlighter).StringAttri);
  AddColorSynAttr(TSynPasSyn(SynEditor.Highlighter).SymbolAttri);
end;


procedure SetColorsSynEdit(SynEditor: TSynEdit; OldColorList, NewColorList: TColorList);
var
  index: integer;

  function GetColor(OldColor: TColor): TColor;
  begin
    //Result:=NewColorList[OldColorList.IndexOf(OldColor)];
    Result := NewColorList[Index];
    Inc(index);
  end;

  procedure SetColorSynAttr(SynAttr: TSynHighlighterAttributes);
  begin
    SynAttr.Foreground := GetColor(SynAttr.Foreground);
    SynAttr.Background := GetColor(SynAttr.Background);
  end;

begin
  index := 0;
  SynEditor.ActiveLineColor := GetColor(SynEditor.ActiveLineColor);
  SynEditor.RightEdgeColor := GetColor(SynEditor.RightEdgeColor);
  SynEditor.SelectedColor.Foreground := GetColor(SynEditor.SelectedColor.Foreground);
  SynEditor.SelectedColor.Background := GetColor(SynEditor.SelectedColor.Background);
  SynEditor.Gutter.Color := GetColor(SynEditor.Gutter.Color);
  SynEditor.Gutter.Font.Color := GetColor(SynEditor.Gutter.Font.Color);

  SetColorSynAttr(TSynPasSyn(SynEditor.Highlighter).AsmAttri);
  SetColorSynAttr(TSynPasSyn(SynEditor.Highlighter).CharAttri);
  SetColorSynAttr(TSynPasSyn(SynEditor.Highlighter).CommentAttri);
  SetColorSynAttr(TSynPasSyn(SynEditor.Highlighter).DirectiveAttri);
  SetColorSynAttr(TSynPasSyn(SynEditor.Highlighter).FloatAttri);
  SetColorSynAttr(TSynPasSyn(SynEditor.Highlighter).HexAttri);
  SetColorSynAttr(TSynPasSyn(SynEditor.Highlighter).IdentifierAttri);
  SetColorSynAttr(TSynPasSyn(SynEditor.Highlighter).KeyAttri);
  SetColorSynAttr(TSynPasSyn(SynEditor.Highlighter).NumberAttri);
  SetColorSynAttr(TSynPasSyn(SynEditor.Highlighter).SpaceAttri);
  SetColorSynAttr(TSynPasSyn(SynEditor.Highlighter).StringAttri);
  SetColorSynAttr(TSynPasSyn(SynEditor.Highlighter).SymbolAttri);
end;

procedure TFrmHueSat.BtnApplyClick(Sender: TObject);
begin
  try
    if Application.MessageBox(
      PChar(Format(
      'Do you want overwrite the current theme "%s" with the changes made to hue/saturation?',
      [FThemeName])), 'Confirmation', MB_YESNO + MB_ICONQUESTION) = idYes then
    begin
      ApplyHueSaturationToIDETheme(FTheme, Trunc(JvSpinEditHue.Value),
        Trunc(JvSpinEditSat.Value), Trunc(JvSpinEditLight.Value));
      //SaveDelphiIDEThemeToXmlFile(DelphiVersion, FTheme, FSettings.ThemePath, FThemeName);
      SaveDelphiIDEThemeToXmlFile(FTheme, FSettings.ThemePath, FThemeName);
      ShowMessage(Format('Changes saved to the theme "%s"', [FThemeName]));
    end;
  except
    on E: Exception do
      ShowMessage(Format('Error saving theme - Message : %s : Trace %s',
        [E.Message, E.StackTrace]));
  end;
end;

procedure TFrmHueSat.BtnSaveAsClick(Sender: TObject);
var
  NewThemeName: string;
  NewTheme:     TIDETheme;
begin
  try
    NewThemeName := InputBox('Create New Delphi IDE Theme',
      'Enter the name for the new theme', '');
    if NewThemeName <> '' then
    begin
      NewTheme := FTheme;
      ApplyHueSaturationToIDETheme(NewTheme, Trunc(JvSpinEditHue.Value),
        Trunc(JvSpinEditSat.Value), Trunc(JvSpinEditLight.Value));
      //SaveDelphiIDEThemeToXmlFile(DelphiVersion, NewTheme, FSettings.ThemePath, NewThemeName);
      SaveDelphiIDEThemeToXmlFile(NewTheme, FSettings.ThemePath, NewThemeName);
      ShowMessage(Format('The theme "%s" was created', [NewThemeName]));
      FReloadThemes := True;
    end;
  except
    on E: Exception do
      ShowMessage(Format('Error creating new theme - Message : %s : Trace %s',
        [E.Message, E.StackTrace]));
  end;
end;

procedure TFrmHueSat.ButtonHueClick(Sender: TObject);
begin
  JvSpinEditHue.Value  := DefHue;
  TrackBarHue.Position := DefHue;
end;

procedure TFrmHueSat.ButtonLightnessClick(Sender: TObject);
begin
  JvSpinEditLight.Value      := DefLig;
  TrackBarLightness.Position := DefLig;
end;

procedure TFrmHueSat.ButtonSaturationClick(Sender: TObject);
begin
  JvSpinEditSat.Value := DefSat;
  TrackBarSaturation.Position := DefSat;
end;

procedure TFrmHueSat.FormCreate(Sender: TObject);
begin
  FReloadThemes := False;
  FColorList    := TColorList.Create;
  FHueColorList := TColorList.Create;
end;

procedure TFrmHueSat.FormDestroy(Sender: TObject);
begin
  FColorList.Free;
  FHueColorList.Free;
end;

procedure TFrmHueSat.Hue(Value: integer);
var
  Colors: TColorList;
begin
  Colors := TColorList.Create;
  try
    Colors.AddRange(FColorList);
    if Value >= 0 then
      _Hue(Colors, Value)
    else
    if Value < 0 then
      _Hue(Colors, 360 - Abs(Value));

    SetColorsSynEdit(SynEditor, FHueColorList, Colors);
    FHueColorList.Clear;
    FHueColorList.AddRange(Colors);
  finally
    Colors.Free;
  end;
end;

procedure TFrmHueSat.Lightness(Value: integer);
var
  Colors: TColorList;
  BackUp: TColorList;
begin
  Colors := TColorList.Create;
  BackUp := TColorList.Create;
  try
    Colors.AddRange(FHueColorList);

    if Value >= 0 then
      _Lightness(Colors, Value)
    else
      _Darkness(Colors, Abs(Value));

    GetColorsSynEdit(SynEditor, BackUp);
    SetColorsSynEdit(SynEditor, BackUp, Colors);
  finally
    Colors.Free;
    BackUp.Free;
  end;
end;

procedure TFrmHueSat.Saturation(Value: integer);
var
  Colors: TColorList;
  BackUp: TColorList;
begin
  Colors := TColorList.Create;
  BackUp := TColorList.Create;
  try
    Colors.AddRange(FHueColorList);
    _Saturation(Colors, (255 - ((Value * 255) div MaxSat)));
    GetColorsSynEdit(SynEditor, BackUp);
    SetColorsSynEdit(SynEditor, BackUp, Colors);
  finally
    Colors.Free;
    BackUp.Free;
  end;
end;

procedure TFrmHueSat.SetTheme(const Value: TIDETheme);
begin
  FTheme := Value;
end;

procedure TFrmHueSat.init;
begin
  GetColorsSynEdit(FSynEditor, FColorList);
  FHueColorList.AddRange(FColorList);
end;

procedure TFrmHueSat.JvSpinEditHueChange(Sender: TObject);
begin
  TrackBarHue.Position := Trunc(TJvSpinEdit(Sender).Value);
end;

procedure TFrmHueSat.JvSpinEditLightChange(Sender: TObject);
begin
  TrackBarLightness.Position := Trunc(TJvSpinEdit(Sender).Value);
end;

procedure TFrmHueSat.JvSpinEditSatChange(Sender: TObject);
begin
  TrackBarSaturation.Position := Trunc(TJvSpinEdit(Sender).Value);
end;


procedure TFrmHueSat.TrackBarHueChange(Sender: TObject);
begin
  JvSpinEditHue.Value := TrackBarHue.Position;
  Hue(Trunc(JvSpinEditHue.Value));
  if JvSpinEditSat.Value <> 0 then
    Saturation(Trunc(JvSpinEditSat.Value));
  if JvSpinEditLight.Value <> 0 then
    Lightness(Trunc(JvSpinEditLight.Value));
end;

procedure TFrmHueSat.TrackBarLightnessChange(Sender: TObject);
begin
  JvSpinEditLight.Value := TrackBarLightness.Position;
  Lightness(Trunc(JvSpinEditLight.Value));
end;

procedure TFrmHueSat.TrackBarSaturationChange(Sender: TObject);
begin
  JvSpinEditSat.Value := TrackBarSaturation.Position;
  Saturation(Trunc(JvSpinEditSat.Value));
end;

end.
