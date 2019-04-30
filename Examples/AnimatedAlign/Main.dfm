object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'Animated Align with AnyiQuack'
  ClientHeight = 337
  ClientWidth = 624
  Color = clAppWorkSpace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnResize = FormResize
  DesignSize = (
    624
    337)
  PixelsPerInch = 96
  TextHeight = 13
  object Label3: TLabel
    Left = 373
    Top = 256
    Width = 240
    Height = 39
    Anchors = [akRight, akBottom]
    Caption = 
      'Add a couple of panels and play with the size of form or panels.' +
      ' Also try the different handling for disturbed animations.'
    Color = clInfoBk
    ParentColor = False
    Transparent = True
    WordWrap = True
    ExplicitLeft = 384
  end
  object TopPanel: TPanel
    Tag = 1
    Left = 0
    Top = 0
    Width = 624
    Height = 44
    Align = alTop
    BevelEdges = [beBottom]
    BevelKind = bkSoft
    BevelOuter = bvNone
    ParentBackground = False
    TabOrder = 0
    ExplicitWidth = 635
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
    object Label5: TLabel
      Left = 324
      Top = 1
      Width = 125
      Height = 13
      Caption = 'Animation duration (msec)'
    end
    object Label6: TLabel
      Left = 474
      Top = 1
      Width = 55
      Height = 13
      Caption = 'Hover color'
    end
    object DisturbedComboBox: TComboBox
      Left = 164
      Top = 17
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 0
      Text = 'Cancel'
      Items.Strings = (
        'Cancel'
        'Finish')
    end
    object PanelSizeTrackBar: TTrackBar
      Left = 0
      Top = 17
      Width = 150
      Height = 21
      Max = 200
      Min = 55
      PageSize = 10
      Frequency = 20
      Position = 100
      PositionToolTip = ptBottom
      TabOrder = 1
      OnChange = PanelSizeTrackBarChange
    end
    object AnimationDurationTrackBar: TTrackBar
      Left = 318
      Top = 17
      Width = 150
      Height = 21
      Max = 2000
      Min = 100
      Position = 500
      PositionToolTip = ptBottom
      TabOrder = 2
    end
    object HoverColorBox: TColorBox
      Left = 474
      Top = 17
      Width = 145
      Height = 22
      TabOrder = 3
    end
    object HoverShakeCheckBox: TCheckBox
      Left = 625
      Top = 19
      Width = 97
      Height = 17
      Caption = 'Shake on hover'
      Checked = True
      State = cbChecked
      TabOrder = 4
    end
  end
  object BottomPanel: TPanel
    Tag = 1
    Left = 0
    Top = 304
    Width = 624
    Height = 33
    Align = alBottom
    BevelEdges = [beTop]
    BevelKind = bkSoft
    BevelOuter = bvNone
    ParentBackground = False
    TabOrder = 1
    ExplicitWidth = 635
    object AddPanelButton: TButton
      AlignWithMargins = True
      Left = 546
      Top = 3
      Width = 75
      Height = 25
      Align = alRight
      Caption = '&Add Panel'
      TabOrder = 0
      OnClick = AddPanelButtonClick
      ExplicitLeft = 557
    end
    object RemovePanelButton: TButton
      AlignWithMargins = True
      Left = 458
      Top = 3
      Width = 82
      Height = 25
      Align = alRight
      Caption = '&Remove Panel'
      Enabled = False
      TabOrder = 1
      OnClick = RemovePanelButtonClick
      ExplicitLeft = 469
    end
  end
end
