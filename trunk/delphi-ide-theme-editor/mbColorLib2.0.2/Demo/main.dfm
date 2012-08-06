object Form1: TForm1
  Left = 631
  Top = 311
  Caption = 'mbColor Lib v2.0.1 Demo'
  ClientHeight = 348
  ClientWidth = 584
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  ShowHint = True
  OnCreate = FormCreate
  DesignSize = (
    584
    348)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 455
    Top = 8
    Width = 66
    Height = 13
    Anchors = [akTop, akRight]
    Caption = 'SelectedColor'
    ExplicitLeft = 412
  end
  object Label2: TLabel
    Left = 453
    Top = 112
    Width = 86
    Height = 13
    Anchors = [akTop, akRight]
    Caption = 'ColorUnderCursor'
    ExplicitLeft = 410
  end
  object Label5: TLabel
    Left = 453
    Top = 238
    Width = 92
    Height = 65
    Anchors = [akTop, akRight]
    Caption = 
      'Aditional controls:'#13#13'- Arrow keys'#13'- Ctrl + Arrow keys'#13'- Mouse wh' +
      'eel'
    ExplicitLeft = 410
  end
  object PageControl1: TPageControl
    Left = 6
    Top = 6
    Width = 440
    Height = 335
    ActivePage = TabSheet1
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    ExplicitWidth = 397
    ExplicitHeight = 325
    object TabSheet1: TTabSheet
      Caption = 'HSLColorPicker'
      ExplicitWidth = 706
      DesignSize = (
        432
        307)
      object HSLColorPicker1: THSLColorPicker
        Left = 8
        Top = 8
        Width = 418
        Height = 293
        SelectedColor = 502023
        HSPickerHintFormat = 'H: %h S: %s'#13'Hex: %hex'
        LPickerHintFormat = 'Luminance: %l'
        Anchors = [akLeft, akTop, akRight, akBottom]
        TabOrder = 0
        OnChange = HSLColorPicker1Change
        OnMouseMove = HSLColorPicker1MouseMove
        ExplicitWidth = 692
        DesignSize = (
          418
          293)
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'HexaColorPicker'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        432
        307)
      object Label4: TLabel
        Left = 82
        Top = 288
        Width = 37
        Height = 13
        Anchors = [akLeft, akBottom]
        Caption = 'Marker:'
        ExplicitTop = 278
      end
      object HexaColorPicker1: THexaColorPicker
        Left = 48
        Top = 4
        Width = 326
        Height = 277
        Anchors = [akLeft, akTop, akRight, akBottom]
        HintFormat = 'RGB(%r, %g, %b)'#13'Hex: %h'
        IntensityText = 'Intensity'
        TabOrder = 0
        Constraints.MinHeight = 85
        Constraints.MinWidth = 93
        OnChange = HexaColorPicker1Change
        OnMouseMove = HexaColorPicker1MouseMove
        ExplicitWidth = 339
      end
      object CheckBox1: TCheckBox
        Left = 4
        Top = 284
        Width = 75
        Height = 17
        Anchors = [akLeft, akBottom]
        Caption = 'SliderVisible'
        Checked = True
        State = cbChecked
        TabOrder = 1
        OnClick = CheckBox1Click
      end
      object ComboBox1: TComboBox
        Left = 124
        Top = 284
        Width = 71
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akBottom]
        ItemIndex = 0
        TabOrder = 2
        Text = 'smArrow'
        OnChange = ComboBox1Change
        Items.Strings = (
          'smArrow'
          'smRect')
        ExplicitTop = 274
      end
      object CheckBox2: TCheckBox
        Left = 200
        Top = 286
        Width = 97
        Height = 17
        Anchors = [akLeft, akBottom]
        Caption = 'NewArrowStyle'
        TabOrder = 3
        OnClick = CheckBox2Click
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'mbColorPalette'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        432
        307)
      object Label3: TLabel
        Left = 6
        Top = 282
        Width = 24
        Height = 13
        Anchors = [akLeft, akBottom]
        Caption = 'Sort:'
        ExplicitTop = 272
      end
      object Label6: TLabel
        Left = 214
        Top = 282
        Width = 28
        Height = 13
        Anchors = [akLeft, akBottom]
        Caption = 'Style:'
        ExplicitTop = 272
      end
      object Label7: TLabel
        Left = 320
        Top = 282
        Width = 23
        Height = 13
        Anchors = [akLeft, akBottom]
        Caption = 'Size:'
        ExplicitTop = 272
      end
      object Button1: TButton
        Left = 6
        Top = 242
        Width = 107
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = 'Generate blue pal'
        TabOrder = 0
        OnClick = Button1Click
      end
      object Button2: TButton
        Left = 120
        Top = 242
        Width = 135
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = 'Generate gradient pal'
        TabOrder = 1
        OnClick = Button2Click
      end
      object Button4: TButton
        Left = 262
        Top = 242
        Width = 121
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = 'Load palette from file'
        TabOrder = 2
        OnClick = Button4Click
      end
      object ScrollBox1: TScrollBox
        Left = 7
        Top = 9
        Width = 422
        Height = 227
        Anchors = [akLeft, akTop, akRight, akBottom]
        BevelInner = bvNone
        BevelOuter = bvNone
        BorderStyle = bsNone
        ParentBackground = True
        TabOrder = 3
        ExplicitWidth = 696
        object mbColorPalette1: TmbColorPalette
          Left = 0
          Top = 0
          Width = 414
          Height = 216
          Align = alTop
          Colors.Strings = (
            'clBlack'
            '$00330000'
            '$00660000'
            '$00990000'
            '$00CC0000'
            'clBlue'
            '$00FF3300'
            '$00CC3300'
            '$00993300'
            '$00663300'
            '$00333300'
            '$00003300'
            '$00000033'
            '$00330033'
            '$00660033'
            '$00990033'
            '$00CC0033'
            '$00FF0033'
            '$00FF3333'
            '$00CC3333'
            '$00993333'
            '$00663333'
            '$00333333'
            '$00003333'
            '$00000066'
            '$00330066'
            '$00660066'
            '$00990066'
            '$00CC0066'
            '$00FF0066'
            '$00FF3366'
            '$00CC3366'
            '$00993366'
            '$00663366'
            '$00333366'
            '$00003366'
            '$00000099'
            '$00330099'
            '$00660099'
            '$00990099'
            '$00CC0099'
            '$00FF0099'
            '$00FF3399'
            '$00CC3399'
            '$00993399'
            '$00663399'
            '$00333399'
            '$00003399'
            '$000000CC'
            '$003300CC'
            '$006600CC'
            '$009900CC'
            '$00CC00CC'
            '$00FF00CC'
            '$00FF33CC'
            '$00CC33CC'
            '$009933CC'
            '$006633CC'
            '$003333CC'
            '$000033CC'
            'clRed'
            '$003300FF'
            '$006600FF'
            '$009900FF'
            '$00CC00FF'
            'clFuchsia'
            '$00FF33FF'
            '$00CC33FF'
            '$009933FF'
            '$006633FF'
            '$003333FF'
            '$000033FF'
            '$000066FF'
            '$003366FF'
            '$006666FF'
            '$009966FF'
            '$00CC66FF'
            '$00FF66FF'
            '$00FF99FF'
            '$00CC99FF'
            '$009999FF'
            '$006699FF'
            '$003399FF'
            '$000099FF'
            '$000066CC'
            '$003366CC'
            '$006666CC'
            '$009966CC'
            '$00CC66CC'
            '$00FF66CC'
            '$00FF99CC'
            '$00CC99CC'
            '$009999CC'
            '$006699CC'
            '$003399CC'
            '$000099CC'
            '$00006699'
            '$00336699'
            '$00666699'
            '$00996699'
            '$00CC6699'
            '$00FF6699'
            '$00FF9999'
            '$00CC9999'
            '$00999999'
            '$00669999'
            '$00339999'
            '$00009999'
            '$00006666'
            '$00336666'
            '$00666666'
            '$00996666'
            '$00CC6666'
            '$00FF6666'
            '$00FF9966'
            '$00CC9966'
            '$00999966'
            '$00669966'
            '$00339966'
            '$00009966'
            '$00006633'
            '$00336633'
            '$00666633'
            '$00996633'
            '$00CC6633'
            '$00FF6633'
            '$00FF9933'
            '$00CC9933'
            '$00999933'
            '$00669933'
            '$00339933'
            '$00009933'
            '$00006600'
            '$00336600'
            '$00666600'
            '$00996600'
            '$00CC6600'
            '$00FF6600'
            '$00FF9900'
            '$00CC9900'
            '$00999900'
            '$00669900'
            '$00339900'
            '$00009900'
            '$0000CC00'
            '$0033CC00'
            '$0066CC00'
            '$0099CC00'
            '$00CCCC00'
            '$00FFCC00'
            'clAqua'
            '$00CCFF00'
            '$0099FF00'
            '$0066FF00'
            '$0033FF00'
            'clLime'
            '$0000CC33'
            '$0033CC33'
            '$0066CC33'
            '$0099CC33'
            '$00CCCC33'
            '$00FFCC33'
            '$00FFFF33'
            '$00CCFF33'
            '$0099FF33'
            '$0066FF33'
            '$0033FF33'
            '$0000FF33'
            '$0000CC66'
            '$0033CC66'
            '$0066CC66'
            '$0099CC66'
            '$00CCCC66'
            '$00FFCC66'
            '$00FFFF66'
            '$00CCFF66'
            '$0099FF66'
            '$0066FF66'
            '$0033FF66'
            '$0000FF66'
            '$0000CC99'
            '$0033CC99'
            '$0066CC99'
            '$0099CC99'
            '$00CCCC99'
            '$00FFCC99'
            '$00FFFF99'
            '$00CCFF99'
            '$0099FF99'
            '$0066FF99'
            '$0033FF99'
            '$0000FF99'
            '$0000CCCC'
            '$0033CCCC'
            '$0066CCCC'
            '$0099CCCC'
            '$00CCCCCC'
            '$00FFCCCC'
            '$00FFFFCC'
            '$00CCFFCC'
            '$0099FFCC'
            '$0066FFCC'
            '$0033FFCC'
            '$0000FFCC'
            '$0000CCFF'
            '$0033CCFF'
            '$0066CCFF'
            '$0099CCFF'
            '$00CCCCFF'
            '$00FFCCFF'
            'clWhite'
            '$00CCFFFF'
            '$0099FFFF'
            '$0066FFFF'
            '$0033FFFF'
            'clYellow'
            'clBlack'
            'clBlack'
            'clBlack'
            'clBlack'
            'clBlack'
            'clBlack'
            'clBlack'
            'clBlack'
            'clBlack'
            'clBlack'
            'clBlack'
            'clBlack'
            '$00000099'
            '$00009999'
            '$00009900'
            '$00999900'
            '$00990000'
            '$00990099'
            'clBlack'
            'clBlack'
            'clBlack'
            'clBlack'
            'clBlack'
            'clBlack'
            'clRed'
            'clYellow'
            'clLime'
            'clAqua'
            'clBlue'
            'clFuchsia'
            'clWhite'
            '$00CCCCCC'
            '$00999999'
            '$00666666'
            '$00333333'
            'clBlack'
            'clBlack'
            'clBlack'
            'clBlack'
            'clBlack')
          HintFormat = 'RGB(%r, %g, %b)'#13'Hex: %h'
          AutoHeight = True
          TabOrder = 0
          OnSelColorChange = mbColorPalette1SelColorChange
          OnMouseMove = mbColorPalette1MouseMove
        end
      end
      object ComboBox2: TComboBox
        Left = 34
        Top = 276
        Width = 87
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akBottom]
        ItemIndex = 0
        TabOrder = 4
        Text = 'soAscending'
        OnChange = ComboBox2Change
        Items.Strings = (
          'soAscending'
          'soDescending')
        ExplicitTop = 266
      end
      object ComboBox3: TComboBox
        Left = 124
        Top = 276
        Width = 87
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akBottom]
        ItemIndex = 7
        TabOrder = 5
        Text = 'smNone'
        OnChange = ComboBox3Change
        Items.Strings = (
          'smRed'
          'smGreen'
          'smBlue'
          'smHue'
          'smSaturation'
          'smLuminance'
          'smValue'
          'smNone'
          'smCyan'
          'smMagenta'
          'smYellow'
          'smBlacK'
          'smCIEx'
          'smCIEy'
          'smCIEz'
          'smCIEl'
          'smCIEa'
          'smCIEb')
        ExplicitTop = 266
      end
      object ComboBox4: TComboBox
        Left = 244
        Top = 276
        Width = 71
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akBottom]
        ItemIndex = 0
        TabOrder = 6
        Text = 'csDefault'
        OnChange = ComboBox4Change
        Items.Strings = (
          'csDefault'
          'csCorel')
        ExplicitTop = 266
      end
      object UpDown1: TUpDown
        Left = 348
        Top = 276
        Width = 31
        Height = 21
        Anchors = [akLeft, akBottom]
        Position = 18
        TabOrder = 7
        Thousands = False
        Wrap = True
        OnChanging = UpDown1Changing
      end
    end
    object TabSheet4: TTabSheet
      Caption = 'HSLRingPicker'
      ImageIndex = 3
      ExplicitWidth = 445
      DesignSize = (
        432
        307)
      object HSLRingPicker1: THSLRingPicker
        Left = 50
        Top = 6
        Width = 334
        Height = 295
        RingPickerHintFormat = 'Hue: %h'
        SLPickerHintFormat = 'S: %s L: %l'#13'Hex: %hex'
        Anchors = [akLeft, akTop, akRight, akBottom]
        TabOrder = 0
        OnChange = HSLRingPicker1Change
        OnMouseMove = HSLRingPicker1MouseMove
        ExplicitWidth = 347
      end
    end
    object TabSheet5: TTabSheet
      Caption = 'HSVColorPicker'
      ImageIndex = 4
      ExplicitWidth = 445
      DesignSize = (
        432
        307)
      object HSVColorPicker1: THSVColorPicker
        Left = 24
        Top = 6
        Width = 340
        Height = 295
        HintFormat = 'H: %h S: %s V: %v'#13'Hex: %hex'
        Anchors = [akLeft, akTop, akRight, akBottom]
        TabOrder = 0
        OnMouseMove = HSVColorPicker1MouseMove
        OnChange = HSVColorPicker1Change
        ExplicitWidth = 353
      end
      object VColorPicker2: TVColorPicker
        Left = 375
        Top = 2
        Width = 22
        Height = 303
        HintFormat = 'Value: %v'
        NewArrowStyle = True
        Anchors = [akTop, akRight, akBottom]
        TabOrder = 1
        OnChange = VColorPicker2Change
        SelectedColor = clWhite
        ExplicitLeft = 388
      end
    end
    object TabSheet6: TTabSheet
      Caption = 'SLHColorPicker'
      ImageIndex = 5
      ExplicitWidth = 445
      DesignSize = (
        432
        307)
      object SLHColorPicker1: TSLHColorPicker
        Left = 6
        Top = 6
        Width = 422
        Height = 297
        HPickerHintFormat = 'Hue: %h'
        SLPickerHintFormat = 'S: %s L: %l'#13'Hex: %hex'
        Anchors = [akLeft, akTop, akRight, akBottom]
        TabOrder = 0
        OnChange = SLHColorPicker1Change
        OnMouseMove = SLHColorPicker1MouseMove
        ExplicitWidth = 435
        DesignSize = (
          422
          297)
      end
    end
    object TabSheet11: TTabSheet
      Caption = 'Lists && Trees'
      ImageIndex = 10
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object mbColorList1: TmbColorList
        Left = 192
        Top = 12
        Width = 183
        Height = 244
        TabOrder = 0
      end
      object mbColorTree1: TmbColorTree
        Left = 8
        Top = 10
        Width = 171
        Height = 247
        InfoLabelText = 'Color Values:'
        InfoDisplay1 = 'RGB: %r.%g.%b'
        InfoDisplay2 = 'HEX: #%hex'
        Indent = 51
        TabOrder = 1
      end
      object Button5: TButton
        Left = 120
        Top = 264
        Width = 137
        Height = 25
        Caption = 'Add colors from palette'
        TabOrder = 2
        OnClick = Button5Click
      end
    end
    object TabSheet7: TTabSheet
      Caption = 'More'
      ImageIndex = 6
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Label9: TLabel
        Left = 118
        Top = 8
        Width = 103
        Height = 13
        Caption = 'HintFormat variables:'
      end
      object mbDeskPickerButton1: TmbDeskPickerButton
        Left = 8
        Top = 8
        Width = 93
        Height = 25
        Caption = 'Pick from screen'
        DoubleBuffered = True
        ParentDoubleBuffered = False
        TabOrder = 0
        OnSelColorChange = mbDeskPickerButton1SelColorChange
        ScreenHintFormat = 'RGB(%r, %g, %b)'#13'Hex: %h'
      end
      object Button3: TButton
        Left = 8
        Top = 40
        Width = 93
        Height = 25
        Caption = 'OfficeColorDialog'
        TabOrder = 1
        OnClick = Button3Click
      end
      object LColorPicker1: TLColorPicker
        Left = 36
        Top = 148
        Width = 329
        Height = 25
        HintFormat = 'Luminance: %l'
        Layout = lyHorizontal
        SelectionIndicator = siRect
        TabOrder = 2
        Saturation = 238
        Luminance = 60
        SelectedColor = 263284
      end
      object VColorPicker1: TVColorPicker
        Left = 34
        Top = 116
        Width = 335
        Height = 21
        HintFormat = 'Value: %v'
        Layout = lyHorizontal
        ArrowPlacement = spBefore
        NewArrowStyle = True
        SelectionIndicator = siRect
        TabOrder = 3
        Hue = 240
        Saturation = 255
        Value = 40
        SelectedColor = 2621440
      end
      object HColorPicker1: THColorPicker
        Left = 36
        Top = 178
        Width = 335
        Height = 61
        HintFormat = 'Hue: %h'
        Increment = 5
        ArrowPlacement = spBoth
        SelectionIndicator = siRect
        TabOrder = 4
        Saturation = 120
        SelectedColor = 8882175
      end
      object SColorPicker1: TSColorPicker
        Left = 8
        Top = 70
        Width = 19
        Height = 214
        HintFormat = 'Saturation: %s'
        Layout = lyVertical
        ArrowPlacement = spBefore
        NewArrowStyle = True
        SelectionIndicator = siRect
        TabOrder = 5
        Hue = 60
        Saturation = 80
        SelectedColor = 11534335
      end
      object Memo1: TMemo
        Left = 118
        Top = 24
        Width = 247
        Height = 75
        Lines.Strings = (
          'The following variables will be replaced in the '
          'hint at runtime:'
          ''
          '%hex = HTML HEX color value'
          ''
          '%cieL = CIE L*a*b* Luminance value'
          '%cieA = CIE L*a*b* A-Chrominance value'
          '%cieB = CIE L*a*b* B-Chrominance value'
          ''
          '%cieX = CIE XYZ X value'
          '%cieY = CIE XYZ Y value'
          '%cieZ = CIE XYZ Z value'
          ''
          '%cieC = CIE LCH Chrominance value'
          '%cieH = CIE LCH Hue value'
          ''
          '%hslH = HSL Hue value'
          '%hslS = HSL Saturation value'
          '%hslL = HSL Luminance value'
          ''
          '%hsvH = HSV Hue value'
          '%hsvS = HSV Saturation value'
          '%hsvV = HSV Value value'
          ''
          '%r = RGB Red value'
          '%g = RGB Green value'
          '%b = RGB Blue value'
          ''
          '%c = CMYK Cyan value'
          '%m = CMYK Magenta value'
          '%y = CMYK Yellow value'
          '%k = CMYK blacK value'
          ''
          '%h = HSL Hue value'
          '%l = HSL Luminance value'
          '%v = HSV Value value')
        ScrollBars = ssVertical
        TabOrder = 6
      end
    end
    object TabSheet8: TTabSheet
      Caption = 'Other'
      ImageIndex = 7
      ExplicitWidth = 445
      object HSColorPicker1: THSColorPicker
        Left = 6
        Top = 6
        Width = 211
        Height = 155
        SelectedColor = 518633
        HintFormat = 'H: %h S: %s'#13'Hex: %hex'
        TabOrder = 0
        OnMouseMove = HSColorPicker1MouseMove
        HueValue = 60
        MarkerStyle = msSquare
        OnChange = HSColorPicker1Change
      end
      object SLColorPicker1: TSLColorPicker
        Left = 222
        Top = 144
        Width = 161
        Height = 147
        HintFormat = 'H: %h S: %s L: %l'#13'Hex: %hex'
        TabOrder = 1
        OnMouseMove = SLColorPicker1MouseMove
        MarkerStyle = msCross
        OnChange = SLColorPicker1Change
      end
      object HRingPicker1: THRingPicker
        Left = 4
        Top = 164
        Width = 133
        Height = 130
        HintFormat = 'Hue: %h'
        TabOrder = 2
        OnMouseMove = HRingPicker1MouseMove
        OnChange = HRingPicker1Change
      end
    end
    object TabSheet9: TTabSheet
      Caption = 'Even more'
      ImageIndex = 8
      ExplicitWidth = 445
      object Label8: TLabel
        Left = 6
        Top = 4
        Width = 128
        Height = 13
        Caption = 'New: border styles added.'
      end
      object CColorPicker1: TCColorPicker
        Left = 4
        Top = 18
        Width = 22
        Height = 267
        HintFormat = 'Cyan: %c'
        TabOrder = 0
        SelectedColor = clAqua
      end
      object MColorPicker1: TMColorPicker
        Left = 34
        Top = 18
        Width = 22
        Height = 267
        HintFormat = 'Magenta: %m'
        ArrowPlacement = spBefore
        TabOrder = 1
        SelectedColor = clFuchsia
      end
      object YColorPicker1: TYColorPicker
        Left = 68
        Top = 18
        Width = 31
        Height = 267
        HintFormat = 'Yellow: %y'
        ArrowPlacement = spBoth
        TabOrder = 2
        SelectedColor = clYellow
      end
      object KColorPicker1: TKColorPicker
        Left = 120
        Top = 18
        Width = 22
        Height = 267
        HintFormat = 'Black: %k'
        NewArrowStyle = True
        TabOrder = 3
        Cyan = 0
        Black = 255
        SelectedColor = clBlack
      end
      object RColorPicker1: TRColorPicker
        Left = 150
        Top = 18
        Width = 22
        Height = 268
        HintFormat = 'Red: %r'
        ArrowPlacement = spBefore
        NewArrowStyle = True
        TabOrder = 4
        SelectedColor = 8026879
      end
      object GColorPicker1: TGColorPicker
        Left = 182
        Top = 18
        Width = 34
        Height = 268
        HintFormat = 'Green: %g'
        ArrowPlacement = spBoth
        NewArrowStyle = True
        TabOrder = 5
        SelectedColor = 8060794
      end
      object BColorPicker1: TBColorPicker
        Left = 224
        Top = 18
        Width = 22
        Height = 268
        HintFormat = 'Blue: %b'
        SelectionIndicator = siRect
        TabOrder = 6
        SelectedColor = 16743034
      end
      object KColorPicker2: TKColorPicker
        Left = 274
        Top = 22
        Width = 69
        Height = 71
        BevelInner = bvRaised
        BevelOuter = bvRaised
        BorderStyle = bsSingle
        HintFormat = 'Black: %k'
        ArrowPlacement = spBoth
        NewArrowStyle = True
        TabOrder = 7
        Cyan = 0
        Black = 255
        SelectedColor = clBlack
      end
      object MColorPicker2: TMColorPicker
        Left = 272
        Top = 96
        Width = 91
        Height = 55
        BevelInner = bvLowered
        BevelOuter = bvRaised
        BorderStyle = bsSingle
        HintFormat = 'Magenta: %m'
        Layout = lyHorizontal
        ArrowPlacement = spBoth
        NewArrowStyle = True
        TabOrder = 8
        SelectedColor = clFuchsia
      end
      object CColorPicker2: TCColorPicker
        Left = 274
        Top = 152
        Width = 61
        Height = 67
        BevelInner = bvRaised
        BevelOuter = bvLowered
        BorderStyle = bsSingle
        HintFormat = 'Cyan: %c'
        ArrowPlacement = spBoth
        NewArrowStyle = True
        TabOrder = 9
        SelectedColor = clAqua
      end
      object YColorPicker2: TYColorPicker
        Left = 272
        Top = 228
        Width = 81
        Height = 57
        BevelInner = bvLowered
        BevelOuter = bvLowered
        BorderStyle = bsSingle
        HintFormat = 'Yellow: %y'
        ArrowPlacement = spBoth
        NewArrowStyle = True
        TabOrder = 10
        SelectedColor = clYellow
      end
    end
    object TabSheet10: TTabSheet
      Caption = 'Yet even more'
      ImageIndex = 9
      ExplicitWidth = 445
      object RAxisColorPicker1: TRAxisColorPicker
        Left = 10
        Top = 8
        Width = 100
        Height = 100
        HintFormat = 'G: %g B: %b'#13'Hex: %hex'
        TabOrder = 0
      end
      object GAxisColorPicker1: TGAxisColorPicker
        Left = 130
        Top = 10
        Width = 100
        Height = 100
        HintFormat = 'R: %r B: %b'#13'Hex: %hex'
        TabOrder = 1
        MarkerStyle = msCross
      end
      object BAxisColorPicker1: TBAxisColorPicker
        Left = 252
        Top = 10
        Width = 100
        Height = 100
        HintFormat = 'R: %r G: %g'#13'Hex: %hex'
        TabOrder = 2
        MarkerStyle = msCrossCirc
      end
      object CIELColorPicker1: TCIELColorPicker
        Left = 8
        Top = 130
        Width = 100
        Height = 100
        SelectedColor = 16119089
        HintFormat = 'A: %cieA B: %cieB'#13'Hex: %hex'
        TabOrder = 3
        LValue = 88
        AValue = -47
        BValue = -32
      end
      object CIEAColorPicker1: TCIEAColorPicker
        Left = 128
        Top = 130
        Width = 100
        Height = 100
        SelectedColor = 16515327
        HintFormat = 'L: %cieL B: %cieB'#13'Hex: %hex'
        TabOrder = 4
        LValue = 60
        AValue = 96
        BValue = -78
        MarkerStyle = msSquare
      end
      object CIEBColorPicker1: TCIEBColorPicker
        Left = 250
        Top = 130
        Width = 100
        Height = 100
        SelectedColor = 130823
        HintFormat = 'L: %cieL A: %cieA'#13'Hex: %hex'
        TabOrder = 5
        LValue = 88
        AValue = -88
        BValue = 74
      end
    end
  end
  object sc: TmbColorPreview
    Left = 453
    Top = 24
    Width = 108
    Height = 62
    Color = clNone
    Anchors = [akTop, akRight]
    ExplicitLeft = 727
  end
  object uc: TmbColorPreview
    Left = 453
    Top = 130
    Width = 108
    Height = 62
    Color = clNone
    Anchors = [akTop, akRight]
    ExplicitLeft = 727
  end
  object tb1: TTrackBar
    Left = 452
    Top = 92
    Width = 108
    Height = 20
    Hint = 'Opacity'
    Anchors = [akTop, akRight]
    Max = 100
    Position = 100
    TabOrder = 3
    ThumbLength = 15
    TickStyle = tsNone
    OnChange = tb1Change
    ExplicitLeft = 726
  end
  object tb2: TTrackBar
    Left = 453
    Top = 196
    Width = 108
    Height = 20
    Anchors = [akTop, akRight]
    Max = 100
    Position = 100
    TabOrder = 4
    ThumbLength = 15
    TickStyle = tsNone
    OnChange = tb2Change
    ExplicitLeft = 727
  end
  object CheckBox3: TCheckBox
    Left = 453
    Top = 308
    Width = 97
    Height = 17
    Anchors = [akTop, akRight]
    Caption = 'WebSafe'
    TabOrder = 5
    OnClick = CheckBox3Click
    ExplicitLeft = 727
  end
  object CheckBox4: TCheckBox
    Left = 453
    Top = 218
    Width = 97
    Height = 17
    Anchors = [akTop, akRight]
    Caption = 'SwatchStyle'
    TabOrder = 6
    OnClick = CheckBox4Click
    ExplicitLeft = 727
  end
  object mbOfficeColorDialog1: TmbOfficeColorDialog
    UseHints = True
    Left = 472
    Top = 302
  end
  object OpenDialog1: TOpenDialog
    Filter = 'JASC PAL (*.pal)|*.pal|Photoshop (*.act; *.aco)|*.act;*.aco'
    Left = 440
    Top = 304
  end
end
