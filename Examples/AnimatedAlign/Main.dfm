object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'Animated Align with TAQ (AccessQuery)'
  ClientHeight = 337
  ClientWidth = 635
  Color = clAppWorkSpace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnResize = FormResize
  DesignSize = (
    635
    337)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 164
    Top = 1
    Width = 101
    Height = 13
    Caption = 'Disturbed Animations'
  end
  object Label2: TLabel
    Left = 8
    Top = 1
    Width = 47
    Height = 13
    Caption = 'Panel size'
  end
  object AddPanelButton: TButton
    Left = 552
    Top = 304
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Add Panel'
    TabOrder = 0
    OnClick = AddPanelButtonClick
  end
  object PanelSizeTrackBar: TTrackBar
    Left = 8
    Top = 20
    Width = 150
    Height = 45
    Max = 200
    Min = 20
    Frequency = 20
    Position = 100
    TabOrder = 1
    OnChange = PanelSizeTrackBarChange
  end
  object RemovePanelButton: TButton
    Left = 464
    Top = 304
    Width = 82
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Remove Panel'
    Enabled = False
    TabOrder = 2
    OnClick = RemovePanelButtonClick
  end
  object DisturbedComboBox: TComboBox
    Left = 164
    Top = 20
    Width = 145
    Height = 21
    Style = csDropDownList
    ItemIndex = 0
    TabOrder = 3
    Text = 'Cancel'
    Items.Strings = (
      'Cancel'
      'Finish')
  end
end
