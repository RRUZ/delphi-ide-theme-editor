object Form1: TForm1
  Left = 192
  Top = 109
  Width = 495
  Height = 531
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    487
    502)
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 380
    Top = 16
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'AddColors'
    TabOrder = 0
    OnClick = Button1Click
  end
  object t: TmbColorTree
    Left = 120
    Top = 116
    Width = 225
    Height = 315
    Hint = 'color tree'
    InfoLabelText = 'Color Values:'
    InfoDisplay1 = 'RGB: %r.%g.%b'
    InfoDisplay2 = 'HEX: #%hex'
    Indent = 51
    TabOrder = 1
    OnMouseMove = tMouseMove
  end
  object XPManifest1: TXPManifest
    Left = 260
    Top = 50
  end
end
