object FrmSettings: TFrmSettings
  Left = 525
  Top = 305
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Settings'
  ClientHeight = 188
  ClientWidth = 508
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
    Top = 140
    Width = 497
    Height = 9
    Shape = bsTopLine
  end
  object BtnSave: TButton
    Left = 8
    Top = 155
    Width = 75
    Height = 25
    Caption = 'Save'
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
    Top = 155
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 3
    OnClick = BtnCancelClick
  end
  object JvBrowseForFolderDialog1: TJvBrowseForFolderDialog
    Left = 472
    Top = 56
  end
end
