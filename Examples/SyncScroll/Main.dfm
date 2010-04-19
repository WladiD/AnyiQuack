object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 337
  ClientWidth = 570
  Color = clAppWorkSpace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    570
    337)
  PixelsPerInch = 96
  TextHeight = 13
  object ListBox1: TListBox
    Left = 60
    Top = 55
    Width = 121
    Height = 258
    Anchors = [akLeft, akTop, akBottom]
    DoubleBuffered = False
    ItemHeight = 13
    ParentDoubleBuffered = False
    TabOrder = 0
  end
  object ListBox2: TListBox
    Left = 187
    Top = 55
    Width = 121
    Height = 258
    Anchors = [akLeft, akTop, akBottom]
    ItemHeight = 13
    TabOrder = 1
  end
  object ListBox3: TListBox
    Left = 314
    Top = 55
    Width = 121
    Height = 258
    Anchors = [akLeft, akTop, akBottom]
    ItemHeight = 13
    TabOrder = 2
  end
  object ListBox4: TListBox
    Left = 441
    Top = 55
    Width = 121
    Height = 258
    Anchors = [akLeft, akTop, akBottom]
    ItemHeight = 13
    TabOrder = 3
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 570
    Height = 49
    Align = alTop
    ParentBackground = False
    TabOrder = 4
    DesignSize = (
      570
      49)
    object Label1: TLabel
      Left = 10
      Top = 3
      Width = 57
      Height = 13
      Caption = 'Items count'
    end
    object Label2: TLabel
      Left = 160
      Top = 3
      Width = 399
      Height = 46
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Caption = 
        'Items will be added after a small delay of 500 ms on change of t' +
        'he track bar. The big scroll bar scrolls all TListBox-Objects on' +
        ' this form with a fast animation.'
      WordWrap = True
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
  end
  object Panel2: TPanel
    Left = 0
    Top = 49
    Width = 41
    Height = 288
    Align = alLeft
    ParentBackground = False
    TabOrder = 5
    object SyncScrollBar: TScrollBar
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 33
      Height = 280
      Align = alClient
      Kind = sbVertical
      PageSize = 20
      TabOrder = 0
      OnChange = SyncScrollBarChange
    end
  end
  object AnimatedScrollCheckBox: TCheckBox
    Left = 60
    Top = 319
    Width = 121
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Scroll with Animation'
    Checked = True
    State = cbChecked
    TabOrder = 6
  end
end
