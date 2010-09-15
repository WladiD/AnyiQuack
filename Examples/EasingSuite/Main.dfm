object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'Easing-Suite for AnyiQuack'
  ClientHeight = 474
  ClientWidth = 700
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
  OnResize = UpdateTabSheet
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 150
    Height = 474
    Align = alLeft
    BevelEdges = [beRight]
    BevelKind = bkSoft
    BevelOuter = bvNone
    TabOrder = 0
    object AnimateOnChangeCheckBox: TCheckBox
      AlignWithMargins = True
      Left = 3
      Top = 423
      Width = 142
      Height = 17
      Align = alBottom
      Caption = 'Animate on change'
      Checked = True
      State = cbChecked
      TabOrder = 0
    end
    object AnimateButton: TButton
      AlignWithMargins = True
      Left = 3
      Top = 446
      Width = 142
      Height = 25
      Align = alBottom
      Caption = 'Animate'
      Enabled = False
      TabOrder = 1
      OnClick = AnimateButtonClick
    end
    object GroupBox1: TGroupBox
      Left = 0
      Top = 0
      Width = 148
      Height = 183
      Align = alClient
      Caption = 'EaseType:TEaseType'
      TabOrder = 2
      object EaseTypeListBox: TListBox
        Left = 2
        Top = 15
        Width = 144
        Height = 166
        Margins.Left = 0
        Margins.Top = 18
        Margins.Right = 0
        Margins.Bottom = 0
        Align = alClient
        ItemHeight = 13
        Items.Strings = (
          'etLinear'
          'etQuad'
          'etCubic'
          'etQuart'
          'etQuint'
          'etSext'
          'etSinus'
          'etElastic'
          'etBack'
          'etLowWave'
          'etMiddleWave'
          'etHighWave')
        TabOrder = 0
        OnClick = UpdateTabSheet
      end
    end
    object GroupBox2: TGroupBox
      Left = 0
      Top = 183
      Width = 148
      Height = 196
      Align = alBottom
      Caption = 'EaseModifier:TEaseModifier'
      TabOrder = 3
      object EaseModifierListBox: TListBox
        Left = 2
        Top = 15
        Width = 144
        Height = 179
        Align = alClient
        ItemHeight = 13
        Items.Strings = (
          'emIn'
          'emInInverted'
          'emInSnake'
          'emOut'
          'emOutInverted'
          'emOutSnake'
          'emInOut'
          'emInOutMirrored'
          'emInOutCombined'
          'emOutIn'
          'emOutInMirrored'
          'emOutInCombined')
        TabOrder = 0
        OnClick = UpdateTabSheet
      end
    end
    object DurationPanel: TPanel
      Left = 0
      Top = 379
      Width = 148
      Height = 41
      Align = alBottom
      Caption = 'Animation duration (1500 ms)'
      TabOrder = 4
      VerticalAlignment = taAlignTop
      object DurationTrackBar: TTrackBar
        AlignWithMargins = True
        Left = 1
        Top = 16
        Width = 146
        Height = 24
        Margins.Left = 0
        Margins.Top = 15
        Margins.Right = 0
        Margins.Bottom = 0
        Align = alClient
        Max = 10000
        Min = 100
        PageSize = 100
        Frequency = 1000
        Position = 1500
        PositionToolTip = ptBottom
        ShowSelRange = False
        TabOrder = 0
        OnChange = DurationTrackBarChange
      end
    end
  end
  object VisPageControl: TPageControl
    Left = 150
    Top = 0
    Width = 550
    Height = 474
    ActivePage = EaseRealTabSheet
    Align = alClient
    TabOrder = 1
    OnChange = UpdateTabSheet
    object GraphTabSheet: TTabSheet
      Caption = 'Graph'
      object EaseGraphImage: TImage32
        Left = 0
        Top = 0
        Width = 542
        Height = 430
        Align = alClient
        Bitmap.ResamplerClassName = 'TNearestResampler'
        BitmapAlign = baTopLeft
        ParentShowHint = False
        Scale = 1.000000000000000000
        ScaleMode = smNormal
        ShowHint = False
        TabOrder = 0
        OnMouseMove = EaseGraphImageMouseMove
      end
      object Panel2: TPanel
        Left = 0
        Top = 430
        Width = 542
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
          Width = 542
          Height = 14
          Align = alClient
          AutoSize = False
          Caption = 
            'Green=Value is inside the borders, Red=Value is outside the bord' +
            'ers, Blue=End value is arrived'
          WordWrap = True
          ExplicitWidth = 562
          ExplicitHeight = 11
        end
      end
    end
    object EaseRealTabSheet: TTabSheet
      Caption = 'EaseReal-Visualizer'
      ImageIndex = 1
      object EaseRealImage: TImage32
        Left = 0
        Top = 0
        Width = 542
        Height = 422
        Align = alClient
        Bitmap.ResamplerClassName = 'TNearestResampler'
        BitmapAlign = baTopLeft
        Scale = 1.000000000000000000
        ScaleMode = smNormal
        TabOrder = 0
        OnPaintStage = EaseRealImagePaintStage
      end
      object Panel3: TPanel
        Left = 0
        Top = 422
        Width = 542
        Height = 24
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 1
        object XAxisCheckBox: TCheckBox
          Left = 2
          Top = 3
          Width = 97
          Height = 17
          Caption = 'Animate X-Axis'
          Checked = True
          State = cbChecked
          TabOrder = 0
          OnClick = UpdateTabSheet
        end
        object YAxisCheckBox: TCheckBox
          Left = 105
          Top = 3
          Width = 97
          Height = 17
          Caption = 'Animate Y-Axis'
          Checked = True
          State = cbChecked
          TabOrder = 1
          OnClick = UpdateTabSheet
        end
      end
    end
  end
end
