program NotificationExample;

uses
  Forms,
  AnyiQuack in '..\..\AnyiQuack.pas',
  AQPControlAnimations in '..\..\AQPControlAnimations.pas',
  Notifications.Base.Win in '..\..\NotificationFramework\Notifications.Base.Win.pas' {NotificationWindow},
  Notifications.Manager in '..\..\NotificationFramework\Notifications.Manager.pas',
  Main in 'Main.pas' {MainForm},
  MyNotificationWindow in 'MyNotificationWindow.pas' {MyNotificationWindow};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
