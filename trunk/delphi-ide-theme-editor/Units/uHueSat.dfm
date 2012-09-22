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
    Left = 16
    Top = 87
    Width = 329
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
    Left = 16
    Top = 30
    Width = 329
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
    Width = 26
    Height = 25
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
    Left = 320
    Top = 92
    Width = 26
    Height = 25
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
    Width = 26
    Height = 25
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
    Height = 21
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
    Height = 21
    NumbersOnly = True
    TabOrder = 2
    Text = '0'
    OnExit = EditHueExit
  end
  object UpDownSat: TUpDown
    Left = 295
    Top = 91
    Width = 16
    Height = 21
    Associate = EditSat
    TabOrder = 5
    OnChanging = UpDownSatChanging
  end
  object EditSat: TEdit
    Left = 263
    Top = 91
    Width = 32
    Height = 21
    NumbersOnly = True
    TabOrder = 4
    Text = '0'
    OnExit = EditSatExit
  end
  object UpDownLight: TUpDown
    Left = 295
    Top = 147
    Width = 16
    Height = 21
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
    Height = 21
    NumbersOnly = True
    TabOrder = 9
    Text = '0'
    OnExit = EditLightExit
  end
  object ImageList1: TImageList
    ColorDepth = cd32Bit
    Left = 304
    Top = 216
    Bitmap = {
      494C010101000800140010001000FFFFFFFF2110FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000001000000001002000000000000010
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000104
      0106336D38B80000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000458F
      4CD9499B52F70E21103800000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000004A9553CF6FBD
      79FF78C082FF59A962FF45984DFF3F9047FF398740FF337E39FF2D7633FF286E
      2DFF236727FF1F6122FF1B5C1EFF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000004C9555C479C885FF9BD5
      A4FF97D3A0FF93D09CFF8FCE97FF8ACB92FF86C98DFF81C588FF7CC283FF78C0
      7EFF74BD7AFF70BC76FF1F6122FF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000509D5AC97DCC89FFA1D8
      AAFF9DD6A6FF99D4A2FF95D29EFF92CF99FF8DCC94FF88CA8FFF84C78AFF80C4
      86FF7BC181FF76BF7CFF236727FF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000053A35DD17ECD
      8AFF7AC986FF5BB766FF56B060FF51A85AFF4BA054FF45984DFF3F9047FF3987
      40FF337E39FF2D7633FF286E2DFF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000000057AB
      62DB60BC6BF70000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000105
      03064B9254BB0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000100000000100010000000000800000000000000000000000
      000000000000000000000000FFFFFF00FFFF000000000000FFFF000000000000
      FFFF000000000000FFFF000000000000E7FF000000000000E3FF000000000000
      C00100000000000080010000000000008001000000000000C001000000000000
      E7FF000000000000E7FF000000000000FFFF000000000000FFFF000000000000
      FFFF000000000000FFFF00000000000000000000000000000000000000000000
      000000000000}
  end
end
