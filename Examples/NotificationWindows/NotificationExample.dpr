program NotificationExample;

uses
  Forms,
  Main in 'Main.pas' {MainForm},
  AnyiQuack in '..\..\AnyiQuack.pas',
  AQPControlAnimations in '..\..\AQPControlAnimations.pas',
  NotificationWindows in '..\..\NotificationWindowsFramework\NotificationWindows.pas' {NotificationWindow},
  MyNotificationWindow in 'MyNotificationWindow.pas' {NotificationWindow1};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
