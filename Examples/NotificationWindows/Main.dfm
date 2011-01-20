object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'NotificationWindows test'
  ClientHeight = 178
  ClientWidth = 340
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object AddButton: TButton
    Left = 8
    Top = 54
    Width = 153
    Height = 49
    Caption = 'Add notify window'
    Default = True
    TabOrder = 0
    OnClick = AddButtonClick
  end
  object AutoCloseCheckBox: TCheckBox
    Left = 8
    Top = 8
    Width = 153
    Height = 17
    Caption = 'Auto close after 5 sec.'
    Checked = True
    State = cbChecked
    TabOrder = 1
  end
  object AutoCreateCheckBox: TCheckBox
    Left = 8
    Top = 31
    Width = 153
    Height = 17
    Caption = 'Auto create each second'
    TabOrder = 2
    OnClick = AutoCreateCheckBoxClick
  end
  object CloseButton: TButton
    Left = 8
    Top = 109
    Width = 153
    Height = 25
    Cancel = True
    Caption = 'Close all'
    TabOrder = 3
    OnClick = CloseButtonClick
  end
  object Timer1: TTimer
    Enabled = False
    OnTimer = Timer1Timer
    Left = 200
    Top = 64
  end
end
