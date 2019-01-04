object FrmHueSat: TFrmHueSat
  Left = 809
  Top = 234
  ActiveControl = EditHue
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
  Font.Height = -13
  Font.Name = 'Calibri'
  Font.Style = []
  GlassFrame.Bottom = 80
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 15
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
    Width = 51
    Height = 15
    Caption = 'Lightness'
  end
  object Bevel2: TBevel
    Left = 16
    Top = 87
    Width = 329
    Height = 5
    Shape = bsTopLine
  end
  object Label2: TLabel
    Left = 16
    Top = 72
    Width = 57
    Height = 15
    Caption = 'Saturation'
  end
  object Bevel4: TBevel
    Left = 16
    Top = 30
    Width = 329
    Height = 5
    Shape = bsTopLine
  end
  object Label1: TLabel
    Left = 16
    Top = 16
    Width = 21
    Height = 15
    Caption = 'Hue'
  end
  object BtnApply: TButton
    Left = 67
    Top = 209
    Width = 225
    Height = 25
    Caption = 'Apply changes to the current theme'
    TabOrder = 12
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
  object ButtonLightness: TButton
    Left = 319
    Top = 147
    Width = 22
    Height = 22
    ImageAlignment = iaCenter
    ImageIndex = 0
    Images = ImageList1
    TabOrder = 11
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
    TabOrder = 8
    TickStyle = tsManual
    OnChange = TrackBarLightnessChange
  end
  object ButtonSaturation: TButton
    Left = 319
    Top = 91
    Width = 22
    Height = 22
    ImageAlignment = iaCenter
    ImageIndex = 0
    Images = ImageList1
    TabOrder = 7
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
    TabOrder = 6
    TickStyle = tsManual
    OnChange = TrackBarSaturationChange
  end
  object ButtonHue: TButton
    Left = 319
    Top = 35
    Width = 22
    Height = 22
    BiDiMode = bdRightToLeftNoAlign
    ImageAlignment = iaCenter
    ImageIndex = 0
    Images = ImageList1
    ParentBiDiMode = False
    TabOrder = 1
    OnClick = ButtonHueClick
  end
  object BtnSaveAs: TButton
    Left = 67
    Top = 240
    Width = 225
    Height = 25
    Caption = 'Save changes to a New theme'
    TabOrder = 13
    OnClick = BtnSaveAsClick
  end
  object UpDownHue: TUpDown
    Left = 295
    Top = 37
    Width = 16
    Height = 23
    Associate = EditHue
    Min = -180
    Max = 180
    TabOrder = 3
    OnChanging = UpDownHueChanging
  end
  object EditHue: TEdit
    Left = 263
    Top = 37
    Width = 32
    Height = 23
    NumbersOnly = True
    TabOrder = 2
    Text = '0'
    OnExit = EditHueExit
  end
  object UpDownSat: TUpDown
    Left = 295
    Top = 91
    Width = 16
    Height = 23
    Associate = EditSat
    TabOrder = 5
    OnChanging = UpDownSatChanging
  end
  object EditSat: TEdit
    Left = 263
    Top = 91
    Width = 32
    Height = 23
    NumbersOnly = True
    TabOrder = 4
    Text = '0'
    OnExit = EditSatExit
  end
  object UpDownLight: TUpDown
    Left = 295
    Top = 147
    Width = 16
    Height = 23
    Associate = EditLight
    Min = -255
    Max = 255
    TabOrder = 10
    OnChanging = UpDownLightChanging
  end
  object EditLight: TEdit
    Left = 263
    Top = 147
    Width = 32
    Height = 23
    NumbersOnly = True
    TabOrder = 9
    Text = '0'
    OnExit = EditLightExit
  end
  object ImageList1: TImageList
    ColorDepth = cd32Bit
    Left = 304
    Top = 216
  end
end
