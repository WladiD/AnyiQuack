object SandboxForm: TSandboxForm
  Left = 0
  Top = 0
  Caption = 'Sandbox'
  ClientHeight = 354
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 0
    Top = 268
    Width = 635
    Height = 3
    Cursor = crVSplit
    Align = alBottom
    ExplicitTop = 0
    ExplicitWidth = 248
  end
  object ConsoleMemo: TMemo
    Left = 0
    Top = 271
    Width = 635
    Height = 50
    Align = alBottom
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 321
    Width = 635
    Height = 33
    Align = alBottom
    BevelEdges = [beTop]
    BevelKind = bkSoft
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      635
      31)
    object CompileButton: TButton
      Left = 496
      Top = 4
      Width = 131
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Compile && Apply'
      TabOrder = 0
      OnClick = CompileButtonClick
    end
  end
  object CodeEdit: TSynEdit
    Left = 0
    Top = 0
    Width = 635
    Height = 268
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    TabOrder = 2
    Gutter.DigitCount = 5
    Gutter.Font.Charset = DEFAULT_CHARSET
    Gutter.Font.Color = clWindowText
    Gutter.Font.Height = -11
    Gutter.Font.Name = 'Courier New'
    Gutter.Font.Style = []
    Gutter.LeftOffset = 0
    Gutter.ShowLineNumbers = True
    Highlighter = SynPasSyn1
    Options = [eoAutoIndent, eoDragDropEditing, eoEnhanceEndKey, eoGroupUndo, eoScrollPastEol, eoShowScrollHint, eoSmartTabDelete, eoSmartTabs, eoTabIndent, eoTabsToSpaces, eoTrimTrailingSpaces]
    WantTabs = True
  end
  object SynPasSyn1: TSynPasSyn
    CommentAttri.Foreground = clGrayText
    KeyAttri.Foreground = clNavy
    NumberAttri.Foreground = clBlue
    StringAttri.Foreground = clBlue
    Left = 416
    Top = 112
  end
end
