object FrmSettings: TFrmSettings
  Left = 525
  Top = 305
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Settings'
  ClientHeight = 289
  ClientWidth = 512
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
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 70
    Height = 13
    Caption = 'Themes Folder'
  end
  object Bevel1: TBevel
    Left = 8
    Top = 228
    Width = 497
    Height = 9
    Shape = bsTopLine
  end
  object Label9: TLabel
    Left = 8
    Top = 57
    Width = 45
    Height = 13
    Caption = 'VCL Style'
  end
  object ImageVCLStyle: TImage
    Left = 8
    Top = 103
    Width = 202
    Height = 119
  end
  object BtnSave: TButton
    Left = 8
    Top = 243
    Width = 75
    Height = 25
    Caption = 'Apply'
    TabOrder = 0
    OnClick = BtnSaveClick
  end
  object EditThemesFolder: TEdit
    Left = 8
    Top = 27
    Width = 465
    Height = 21
    TabOrder = 1
  end
  object BtnSelFolderThemes: TButton
    Left = 479
    Top = 25
    Width = 26
    Height = 25
    Caption = '...'
    TabOrder = 2
    OnClick = BtnSelFolderThemesClick
  end
  object BtnCancel: TButton
    Left = 89
    Top = 243
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 3
    OnClick = BtnCancelClick
  end
  object ComboBoxVCLStyle: TComboBox
    Left = 8
    Top = 76
    Width = 202
    Height = 21
    Style = csDropDownList
    TabOrder = 4
    OnChange = ComboBoxVCLStyleChange
  end
end
