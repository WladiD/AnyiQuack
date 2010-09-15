object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'Animated Align with AnyiQuack'
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
  object Label3: TLabel
    Left = 324
    Top = 268
    Width = 311
    Height = 33
    Anchors = [akRight, akBottom]
    AutoSize = False
    Caption = 
      'Add a couple of panels and play with the size of form or panels.' +
      ' Also try the different handling for disturbed animations.'
    Color = clInfoBk
    ParentColor = False
    Transparent = True
    WordWrap = True
  end
  object TopPanel: TPanel
    Tag = 1
    Left = 0
    Top = 0
    Width = 635
    Height = 44
    Align = alTop
    BevelEdges = [beBottom]
    BevelKind = bkSoft
    BevelOuter = bvNone
    ParentBackground = False
    TabOrder = 0
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
  end
  object BottomPanel: TPanel
    Tag = 1
    Left = 0
    Top = 304
    Width = 635
    Height = 33
    Align = alBottom
    BevelEdges = [beTop]
    BevelKind = bkSoft
    BevelOuter = bvNone
    ParentBackground = False
    TabOrder = 1
    object Label4: TLabel
      AlignWithMargins = True
      Left = 5
      Top = 0
      Width = 158
      Height = 31
      Margins.Left = 5
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alLeft
      AutoSize = False
      Caption = 'Note: Flickering is a issue of VCL.'
      Layout = tlCenter
      ExplicitLeft = 8
      ExplicitTop = 14
      ExplicitHeight = 13
    end
    object AddPanelButton: TButton
      AlignWithMargins = True
      Left = 557
      Top = 3
      Width = 75
      Height = 25
      Align = alRight
      Caption = 'Add Panel'
      TabOrder = 0
      OnClick = AddPanelButtonClick
    end
    object RemovePanelButton: TButton
      AlignWithMargins = True
      Left = 469
      Top = 3
      Width = 82
      Height = 25
      Align = alRight
      Caption = 'Remove Panel'
      Enabled = False
      TabOrder = 1
      OnClick = RemovePanelButtonClick
    end
  end
end
