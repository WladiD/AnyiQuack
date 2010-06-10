object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'Graph for the integrated Ease-Functions'
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
    Width = 113
    Height = 337
    Align = alLeft
    BevelEdges = [beRight]
    BevelKind = bkSoft
    BevelOuter = bvNone
    TabOrder = 0
    object Label1: TLabel
      Left = 3
      Top = 3
      Width = 47
      Height = 13
      Caption = 'EaseType'
    end
    object EaseDirectionRadioGroup: TRadioGroup
      Left = 3
      Top = 116
      Width = 105
      Height = 77
      Caption = 'EaseDirection'
      ItemIndex = 0
      Items.Strings = (
        'edIn'
        'edOut'
        'edInOut')
      TabOrder = 0
      OnClick = GraphRecreate
    end
    object EaseTypeListBox: TListBox
      Left = 3
      Top = 22
      Width = 105
      Height = 88
      Columns = 1
      ItemHeight = 13
      Items.Strings = (
        'etLinear'
        'etQuadratic'
        'etMassiveQuadratic'
        'etSinus'
        'etElastic')
      TabOrder = 1
      OnClick = GraphRecreate
    end
    object AnimateOnChangeCheckBox: TCheckBox
      Left = 3
      Top = 199
      Width = 110
      Height = 17
      Caption = 'Animate on change'
      Checked = True
      State = cbChecked
      TabOrder = 2
    end
    object AnimateButton: TButton
      Left = 3
      Top = 222
      Width = 104
      Height = 25
      Caption = 'Animate'
      Enabled = False
      TabOrder = 3
      OnClick = AnimateButtonClick
    end
  end
  object EaseGraphImage: TImage32
    Left = 113
    Top = 0
    Width = 522
    Height = 337
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
  end
end
