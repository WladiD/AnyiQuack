object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 523
  ClientWidth = 169
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 24
    Top = 16
    Width = 113
    Height = 25
    Caption = 'SliceChain(0)'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 24
    Top = 64
    Width = 113
    Height = 25
    Caption = 'SliceChain(0, 1)'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 24
    Top = 112
    Width = 113
    Height = 25
    Caption = 'SliceChain(-1)'
    TabOrder = 2
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 24
    Top = 160
    Width = 113
    Height = 25
    Caption = 'SliceChain(-2)'
    TabOrder = 3
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 24
    Top = 208
    Width = 113
    Height = 25
    Caption = 'SliceChain(1, 1)'
    TabOrder = 4
    OnClick = Button5Click
  end
  object Button6: TButton
    Left = 24
    Top = 256
    Width = 113
    Height = 25
    Caption = 'SliceChain(-2, 1)'
    TabOrder = 5
    OnClick = Button6Click
  end
  object Button7: TButton
    Left = 24
    Top = 304
    Width = 113
    Height = 25
    Caption = 'SliceChain(3)'
    TabOrder = 6
    OnClick = Button7Click
  end
  object Button8: TButton
    Left = 24
    Top = 352
    Width = 113
    Height = 25
    Caption = 'SliceChain(1)'
    TabOrder = 7
    OnClick = Button8Click
  end
  object CheckBox1: TCheckBox
    Left = 8
    Top = 498
    Width = 97
    Height = 17
    Caption = 'DebugMessage'
    TabOrder = 8
  end
  object Button9: TButton
    Left = 24
    Top = 400
    Width = 113
    Height = 25
    Caption = 'SliceChain(100)'
    TabOrder = 9
    OnClick = Button9Click
  end
  object Button10: TButton
    Left = 24
    Top = 448
    Width = 113
    Height = 25
    Caption = 'SliceChain(5, 3)'
    TabOrder = 10
    OnClick = Button10Click
  end
end
