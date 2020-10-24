object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'NotificationWindows test'
  ClientHeight = 530
  ClientWidth = 683
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object AddButton: TButton
    Left = 8
    Top = 206
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
  object CloseAllButton: TButton
    Left = 86
    Top = 261
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Close all'
    TabOrder = 3
    OnClick = CloseAllButtonClick
  end
  object CloseLastButton: TButton
    Left = 8
    Top = 261
    Width = 75
    Height = 25
    Caption = 'Close last'
    TabOrder = 4
    OnClick = CloseLastButtonClick
  end
  object RadioGroup1: TRadioGroup
    Left = 8
    Top = 72
    Width = 153
    Height = 105
    Caption = 'Positioning'
    ItemIndex = 0
    Items.Strings = (
      'Main Screen'
      'Application')
    TabOrder = 5
  end
  object Timer1: TTimer
    Enabled = False
    OnTimer = Timer1Timer
    Left = 136
    Top = 24
  end
end
