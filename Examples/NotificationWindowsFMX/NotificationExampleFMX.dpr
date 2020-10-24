program NotificationExampleFMX;

uses
  System.StartUpCopy,
  FMX.Forms,
  Main in 'Main.pas' {FormMain},
  AQPControlAnimations in '..\..\AQPControlAnimations.pas',
  AnyiQuack in '..\..\AnyiQuack.pas',
  Notifications.Manager in '..\..\NotificationFramework\Notifications.Manager.pas',
  Notifications.FMX.Base in '..\..\NotificationFramework\Notifications.FMX.Base.pas' {NotificationWindowFMX},
  MyNotificationWindow in 'MyNotificationWindow.pas' {MyNotificationWindowFMX};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
