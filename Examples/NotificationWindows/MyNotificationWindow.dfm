inherited MyNotificationWindow: TMyNotificationWindow
  Caption = 'My custom derived notification window'
  GlassFrame.Top = 0
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 0
    Top = 0
    Width = 350
    Height = 66
    Align = alClient
    Alignment = taCenter
    Caption = 'Add some text here to notify your users.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    GlowSize = 2
    ParentFont = False
    ShowAccelChar = False
    Layout = tlCenter
    ExplicitWidth = 234
    ExplicitHeight = 20
  end
end
