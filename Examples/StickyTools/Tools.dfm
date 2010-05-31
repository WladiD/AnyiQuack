object ToolsForm: TToolsForm
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 'Tools'
  ClientHeight = 276
  ClientWidth = 74
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PopupMode = pmExplicit
  Position = poDesigned
  OnClose = FormClose
  OnShow = FormShow
  DesignSize = (
    74
    276)
  PixelsPerInch = 96
  TextHeight = 13
  object StickyCheckBox: TCheckBox
    Left = 32
    Top = 259
    Width = 42
    Height = 17
    Alignment = taLeftJustify
    Anchors = [akRight, akBottom]
    Caption = 'Sticky'
    Checked = True
    State = cbChecked
    TabOrder = 0
  end
  object SubToolsButton: TButton
    Left = -1
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Sub tools'
    TabOrder = 1
    OnClick = SubToolsButtonClick
  end
end
