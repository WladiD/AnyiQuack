object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'Graph for integrated Ease-Functions in AccessQuery'
  ClientHeight = 337
  ClientWidth = 635
  Color = clBtnFace
  Constraints.MinHeight = 300
  Constraints.MinWidth = 300
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnResize = GraphRecreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 150
    Height = 321
    Align = alLeft
    BevelEdges = [beRight]
    BevelKind = bkSoft
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitHeight = 337
    object Label1: TLabel
      Left = 3
      Top = 3
      Width = 47
      Height = 13
      Caption = 'EaseType'
    end
    object EaseDirectionRadioGroup: TRadioGroup
      AlignWithMargins = True
      Left = 3
      Top = 187
      Width = 142
      Height = 77
      Align = alBottom
      Caption = 'EaseDirection'
      ItemIndex = 0
      Items.Strings = (
        'edIn'
        'edOut*'
        'edInOut*')
      TabOrder = 0
      OnClick = GraphRecreate
      ExplicitTop = 116
      ExplicitWidth = 105
    end
    object EaseTypeListBox: TListBox
      AlignWithMargins = True
      Left = 0
      Top = 18
      Width = 148
      Height = 166
      Margins.Left = 0
      Margins.Top = 18
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alClient
      ItemHeight = 13
      Items.Strings = (
        'etLinear'
        'etQuadratic'
        'etMassiveQuadratic'
        'etSinus'
        'etElastic'
        'etLowWave'
        'etMiddleWave'
        'etHighWave')
      TabOrder = 1
      OnClick = GraphRecreate
      ExplicitLeft = 3
      ExplicitTop = 22
      ExplicitWidth = 105
      ExplicitHeight = 88
    end
    object AnimateOnChangeCheckBox: TCheckBox
      AlignWithMargins = True
      Left = 3
      Top = 270
      Width = 142
      Height = 17
      Align = alBottom
      Caption = 'Animate on change'
      Checked = True
      State = cbChecked
      TabOrder = 2
      ExplicitTop = 199
      ExplicitWidth = 110
    end
    object AnimateButton: TButton
      AlignWithMargins = True
      Left = 3
      Top = 293
      Width = 142
      Height = 25
      Align = alBottom
      Caption = 'Animate'
      Enabled = False
      TabOrder = 3
      OnClick = AnimateButtonClick
      ExplicitTop = 222
      ExplicitWidth = 104
    end
  end
  object EaseGraphImage: TImage32
    Left = 150
    Top = 0
    Width = 485
    Height = 321
    Align = alClient
    Bitmap.ResamplerClassName = 'TNearestResampler'
    BitmapAlign = baTopLeft
    ParentShowHint = False
    RepaintMode = rmOptimizer
    Scale = 1.000000000000000000
    ScaleMode = smNormal
    ShowHint = True
    TabOrder = 1
    OnMouseMove = EaseGraphImageMouseMove
    ExplicitLeft = 113
    ExplicitWidth = 522
    ExplicitHeight = 337
  end
  object Panel2: TPanel
    Left = 0
    Top = 321
    Width = 635
    Height = 16
    Align = alBottom
    BevelEdges = [beTop]
    BevelKind = bkSoft
    BevelOuter = bvNone
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    object Label2: TLabel
      Left = 0
      Top = 0
      Width = 635
      Height = 14
      Align = alClient
      AutoSize = False
      Caption = 
        '*Not supported by all Ease-Functions | Green=Value is inside the' +
        ' borders, Red=Value is outside the borders, Blue=End value is ar' +
        'rived'
      WordWrap = True
      ExplicitWidth = 562
      ExplicitHeight = 11
    end
  end
end
