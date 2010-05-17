object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Click on the form...'
  ClientHeight = 337
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClick = FormClick
  PixelsPerInch = 96
  TextHeight = 13
  object ColorListBox1: TColorListBox
    Left = 474
    Top = 0
    Width = 161
    Height = 337
    Align = alRight
    DefaultColorColor = clGray
    Selected = clBtnFace
    TabOrder = 0
    OnClick = ColorListBox1Click
    ExplicitLeft = 0
  end
end
