object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Pulsate input fields'
  ClientHeight = 337
  ClientWidth = 384
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 104
    Height = 13
    Caption = 'Please type numbers:'
  end
  object Label2: TLabel
    Left = 176
    Top = 48
    Width = 201
    Height = 233
    AutoSize = False
    Caption = 
      'Use [Tab] key to see the smooth animations. Left any input field' +
      's empty or type invalid numbers and press [ENTER]. Finally fill ' +
      'all fields with valid numbers and press [ENTER] again to see a s' +
      'haked form.'
    WordWrap = True
  end
  object Edit1: TEdit
    Left = 24
    Top = 48
    Width = 121
    Height = 21
    TabOrder = 0
    Text = '255'
  end
  object Edit2: TEdit
    Left = 24
    Top = 83
    Width = 121
    Height = 21
    TabOrder = 1
  end
  object Edit3: TEdit
    Left = 24
    Top = 118
    Width = 121
    Height = 21
    TabOrder = 2
  end
  object Edit4: TEdit
    Left = 24
    Top = 153
    Width = 121
    Height = 21
    TabOrder = 3
  end
  object Edit5: TEdit
    Left = 24
    Top = 188
    Width = 121
    Height = 21
    TabOrder = 4
  end
  object Edit6: TEdit
    Left = 24
    Top = 223
    Width = 121
    Height = 21
    TabOrder = 5
  end
  object Edit7: TEdit
    Left = 24
    Top = 258
    Width = 121
    Height = 21
    TabOrder = 6
  end
  object CheckButton: TButton
    Left = 24
    Top = 304
    Width = 121
    Height = 25
    Caption = 'Check for numbers'
    Default = True
    TabOrder = 7
    OnClick = CheckButtonClick
  end
end
