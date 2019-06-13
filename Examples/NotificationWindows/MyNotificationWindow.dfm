inherited MyNotificationWindow: TMyNotificationWindow
  Caption = 'My custom derived notification window'
  ClientHeight = 119
  GlassFrame.Top = 0
  ExplicitHeight = 143
  PixelsPerInch = 96
  TextHeight = 13
  object MainLabel: TLabel
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 344
    Height = 81
    Margins.Bottom = 35
    Align = alClient
    Alignment = taCenter
    Caption = 'Here comes the important message...'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ShowAccelChar = False
    Layout = tlCenter
    ExplicitLeft = 0
    ExplicitTop = 0
    ExplicitWidth = 216
    ExplicitHeight = 16
  end
  object MainActionButton: TButton
    Left = 216
    Top = 86
    Width = 126
    Height = 25
    Caption = 'Action!'
    TabOrder = 0
    OnClick = MainActionButtonClick
  end
  object ProgressBar: TProgressBar
    Left = 8
    Top = 90
    Width = 202
    Height = 17
    TabOrder = 1
    Visible = False
  end
end
