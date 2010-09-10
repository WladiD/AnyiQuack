object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'Easing-Suite for AnyiQuack'
  ClientHeight = 412
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
    Height = 396
    Align = alLeft
    BevelEdges = [beRight]
    BevelKind = bkSoft
    BevelOuter = bvNone
    TabOrder = 0
    object Label1: TLabel
      Left = 5
      Top = 2
      Width = 104
      Height = 13
      Caption = 'EaseType:TEaseType'
    end
    object EaseModifierRadioGroup: TRadioGroup
      Left = 0
      Top = 179
      Width = 148
      Height = 163
      Align = alBottom
      Caption = 'EaseModifier:TEaseModifier'
      ItemIndex = 0
      Items.Strings = (
        'emIn'
        'emOut'
        'emInOut'
        'emInInverted*'
        'emOutInverted*'
        'emOutIn'
        'emInOutMirrored *'
        'emOutInMirrored *')
      TabOrder = 0
      OnClick = GraphRecreate
    end
    object EaseTypeListBox: TListBox
      AlignWithMargins = True
      Left = 0
      Top = 18
      Width = 148
      Height = 161
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
    end
    object AnimateOnChangeCheckBox: TCheckBox
      AlignWithMargins = True
      Left = 3
      Top = 345
      Width = 142
      Height = 17
      Align = alBottom
      Caption = 'Animate on change'
      Checked = True
      State = cbChecked
      TabOrder = 2
    end
    object AnimateButton: TButton
      AlignWithMargins = True
      Left = 3
      Top = 368
      Width = 142
      Height = 25
      Align = alBottom
      Caption = 'Animate'
      Enabled = False
      TabOrder = 3
      OnClick = AnimateButtonClick
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 396
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
    TabOrder = 1
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
  object PageControl1: TPageControl
    Left = 150
    Top = 0
    Width = 485
    Height = 396
    ActivePage = GraphTabSheet
    Align = alClient
    TabOrder = 2
    object GraphTabSheet: TTabSheet
      Caption = 'Graph'
      object EaseGraphImage: TImage32
        Left = 0
        Top = 0
        Width = 477
        Height = 368
        Align = alClient
        Bitmap.ResamplerClassName = 'TNearestResampler'
        BitmapAlign = baTopLeft
        ParentShowHint = False
        RepaintMode = rmOptimizer
        Scale = 1.000000000000000000
        ScaleMode = smNormal
        ShowHint = True
        TabOrder = 0
        OnMouseMove = EaseGraphImageMouseMove
      end
    end
  end
end
