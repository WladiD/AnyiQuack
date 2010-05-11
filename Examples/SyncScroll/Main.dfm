object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 337
  ClientWidth = 553
  Color = clAppWorkSpace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    553
    337)
  PixelsPerInch = 96
  TextHeight = 13
  object ListBox1: TListBox
    Left = 3
    Top = 57
    Width = 121
    Height = 241
    Anchors = [akLeft, akTop, akBottom]
    ItemHeight = 13
    TabOrder = 0
  end
  object ListBox2: TListBox
    Left = 130
    Top = 57
    Width = 121
    Height = 241
    Anchors = [akLeft, akTop, akBottom]
    ItemHeight = 13
    TabOrder = 1
  end
  object ListBox3: TListBox
    Left = 257
    Top = 57
    Width = 121
    Height = 241
    Anchors = [akLeft, akTop, akBottom]
    ItemHeight = 13
    TabOrder = 2
  end
  object ListBox4: TListBox
    Left = 384
    Top = 57
    Width = 121
    Height = 241
    Anchors = [akLeft, akTop, akBottom]
    ItemHeight = 13
    TabOrder = 3
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 553
    Height = 49
    Align = alTop
    ParentBackground = False
    TabOrder = 4
    DesignSize = (
      553
      49)
    object Label1: TLabel
      Left = 10
      Top = 3
      Width = 57
      Height = 13
      Caption = 'Items count'
    end
    object Label3: TLabel
      Left = 287
      Top = 5
      Width = 79
      Height = 13
      Anchors = [akLeft, akBottom]
      Caption = 'Animation-Style:'
    end
    object Label4: TLabel
      Left = 321
      Top = 27
      Width = 45
      Height = 13
      Caption = 'Duration:'
    end
    object ItemsCountTrackBar: TTrackBar
      Left = 4
      Top = 16
      Width = 150
      Height = 45
      Max = 1000
      Min = 100
      Frequency = 100
      Position = 100
      PositionToolTip = ptBottom
      TabOrder = 0
      OnChange = ItemsCountTrackBarChange
    end
    object AnimatedScrollCheckBox: TCheckBox
      Left = 160
      Top = 3
      Width = 121
      Height = 17
      Anchors = [akLeft, akBottom]
      Caption = 'Scroll with Animation'
      Checked = True
      State = cbChecked
      TabOrder = 1
    end
    object AnimationStyleComboBox: TComboBox
      Left = 372
      Top = 0
      Width = 133
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akBottom]
      ItemIndex = 0
      TabOrder = 2
      Text = 'etLinear'
      Items.Strings = (
        'etLinear'
        'etQuadratic'
        'etMassiveQuadratic'
        'etSinus')
    end
    object AnimationDurationComboBox: TComboBox
      Left = 372
      Top = 24
      Width = 133
      Height = 21
      Style = csDropDownList
      ItemIndex = 2
      TabOrder = 3
      Text = '300 msec'
      Items.Strings = (
        '100 msec'
        '200 msec'
        '300 msec'
        '400 msec'
        '500 msec'
        '600 msec'
        '700 msec'
        '800 msec'
        '900 msec'
        '1000 msec')
    end
  end
  object Panel2: TPanel
    Left = 512
    Top = 49
    Width = 41
    Height = 255
    Align = alRight
    ParentBackground = False
    TabOrder = 5
    object SyncScrollBar: TScrollBar
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 33
      Height = 247
      Align = alClient
      Kind = sbVertical
      PageSize = 20
      TabOrder = 0
      OnChange = SyncScrollBarChange
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 304
    Width = 553
    Height = 33
    Align = alBottom
    ParentBackground = False
    TabOrder = 6
    object Label2: TLabel
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 545
      Height = 25
      Align = alClient
      AutoSize = False
      Caption = 
        'Items will be added after a small delay of 500 ms on change of t' +
        'he track bar. The big scroll bar scrolls all TListBox-Objects on' +
        ' this form with a fast animation.'
      WordWrap = True
      ExplicitTop = 24
      ExplicitWidth = 562
      ExplicitHeight = 11
    end
  end
end
