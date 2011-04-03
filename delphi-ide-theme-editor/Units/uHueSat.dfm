object FrmHueSat: TFrmHueSat
  Left = 809
  Top = 234
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Hue/Saturation'
  ClientHeight = 278
  ClientWidth = 357
  Color = clBtnFace
  TransparentColorValue = clFuchsia
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  GlassFrame.Bottom = 80
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 16
    Top = 194
    Width = 329
    Height = 9
    Shape = bsTopLine
  end
  object Bevel3: TBevel
    Left = 67
    Top = 136
    Width = 278
    Height = 5
    Shape = bsTopLine
  end
  object Label3: TLabel
    Left = 16
    Top = 128
    Width = 45
    Height = 13
    Caption = 'Lightness'
  end
  object Bevel2: TBevel
    Left = 72
    Top = 80
    Width = 273
    Height = 5
    Shape = bsTopLine
  end
  object Label2: TLabel
    Left = 16
    Top = 72
    Width = 50
    Height = 13
    Caption = 'Saturation'
  end
  object Bevel4: TBevel
    Left = 41
    Top = 24
    Width = 304
    Height = 5
    Shape = bsTopLine
  end
  object Label1: TLabel
    Left = 16
    Top = 16
    Width = 19
    Height = 13
    Caption = 'Hue'
  end
  object BtnApply: TButton
    Left = 67
    Top = 209
    Width = 225
    Height = 25
    Caption = 'Apply changes to the current theme'
    TabOrder = 9
    OnClick = BtnApplyClick
  end
  object TrackBarHue: TTrackBar
    Left = 8
    Top = 35
    Width = 249
    Height = 30
    DoubleBuffered = True
    Max = 180
    Min = -180
    ParentDoubleBuffered = False
    TabOrder = 0
    TickStyle = tsManual
    OnChange = TrackBarHueChange
  end
  object JvSpinEditLight: TJvSpinEdit
    Left = 263
    Top = 147
    Width = 50
    Height = 25
    MaxValue = 255.000000000000000000
    MinValue = -255.000000000000000000
    TabOrder = 7
    OnChange = JvSpinEditLightChange
  end
  object JvSpinEditSat: TJvSpinEdit
    Left = 263
    Top = 92
    Width = 50
    Height = 25
    MaxValue = 100.000000000000000000
    TabOrder = 4
    OnChange = JvSpinEditSatChange
  end
  object JvSpinEditHue: TJvSpinEdit
    Left = 263
    Top = 35
    Width = 50
    Height = 25
    MaxValue = 180.000000000000000000
    MinValue = -180.000000000000000000
    TabOrder = 1
    OnChange = JvSpinEditHueChange
  end
  object ButtonLightness: TButton
    Left = 319
    Top = 147
    Width = 26
    Height = 25
    Caption = '<'
    TabOrder = 8
    OnClick = ButtonLightnessClick
  end
  object TrackBarLightness: TTrackBar
    Left = 8
    Top = 147
    Width = 249
    Height = 30
    DoubleBuffered = False
    Max = 255
    Min = -255
    ParentDoubleBuffered = False
    TabOrder = 6
    TickStyle = tsManual
    OnChange = TrackBarLightnessChange
  end
  object ButtonSaturation: TButton
    Left = 320
    Top = 92
    Width = 26
    Height = 25
    Caption = '<'
    TabOrder = 5
    OnClick = ButtonSaturationClick
  end
  object TrackBarSaturation: TTrackBar
    Left = 8
    Top = 92
    Width = 249
    Height = 30
    DoubleBuffered = True
    Max = 100
    ParentDoubleBuffered = False
    TabOrder = 3
    TickStyle = tsManual
    OnChange = TrackBarSaturationChange
  end
  object ButtonHue: TButton
    Left = 319
    Top = 35
    Width = 26
    Height = 25
    Caption = '<'
    TabOrder = 2
    OnClick = ButtonHueClick
  end
  object BtnSaveAs: TButton
    Left = 67
    Top = 240
    Width = 225
    Height = 25
    Caption = 'Save changes to a New theme'
    TabOrder = 10
    OnClick = BtnSaveAsClick
  end
end
