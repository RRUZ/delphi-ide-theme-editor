object FrmSettings: TFrmSettings
  Left = 525
  Top = 305
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Settings'
  ClientHeight = 407
  ClientWidth = 393
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  GlassFrame.Left = 10
  GlassFrame.Top = 10
  GlassFrame.Right = 10
  GlassFrame.Bottom = 10
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 1
    Width = 70
    Height = 13
    Caption = 'Themes Folder'
  end
  object Bevel1: TBevel
    Left = 8
    Top = 359
    Width = 497
    Height = 9
    Shape = bsTopLine
  end
  object Label9: TLabel
    Left = 8
    Top = 94
    Width = 45
    Height = 13
    Caption = 'VCL Style'
  end
  object BtnSave: TButton
    Left = 8
    Top = 368
    Width = 75
    Height = 25
    Caption = 'Apply'
    TabOrder = 4
    OnClick = BtnSaveClick
  end
  object EditThemesFolder: TEdit
    Left = 8
    Top = 20
    Width = 202
    Height = 21
    TabOrder = 1
  end
  object BtnSelFolderThemes: TButton
    Left = 216
    Top = 18
    Width = 26
    Height = 25
    Caption = '...'
    TabOrder = 0
    OnClick = BtnSelFolderThemesClick
  end
  object BtnCancel: TButton
    Left = 89
    Top = 368
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 5
    OnClick = BtnCancelClick
  end
  object ComboBoxVCLStyle: TComboBox
    Left = 8
    Top = 113
    Width = 234
    Height = 21
    Style = csDropDownList
    TabOrder = 3
    OnChange = ComboBoxVCLStyleChange
  end
  object CheckBoxUpdates: TCheckBox
    Left = 8
    Top = 47
    Width = 161
    Height = 17
    Caption = 'Check for updates in start up.'
    TabOrder = 2
  end
  object CheckBoxHelpInsight: TCheckBox
    Left = 8
    Top = 71
    Width = 265
    Height = 17
    Caption = 'Apply theme to Help Insight (Elevation is required)'
    TabOrder = 6
  end
  object PanelPreview: TPanel
    Left = 8
    Top = 140
    Width = 369
    Height = 213
    BevelOuter = bvNone
    TabOrder = 7
  end
end
