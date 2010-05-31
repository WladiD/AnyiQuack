object MainForm: TMainForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Sticky tools'
  ClientHeight = 452
  ClientWidth = 645
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  DesignSize = (
    645
    452)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 248
    Top = 215
    Width = 149
    Height = 23
    Alignment = taCenter
    Caption = 'Move this window'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    Layout = tlCenter
  end
  object AnimateCheckBox: TCheckBox
    Left = 8
    Top = 427
    Width = 97
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Animate'
    Checked = True
    State = cbChecked
    TabOrder = 0
    OnClick = AnimateCheckBoxClick
  end
end
