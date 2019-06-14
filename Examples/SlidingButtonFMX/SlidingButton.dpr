program SlidingButton;

uses
  System.StartUpCopy,
  FMX.Forms,
  Main in 'Main.pas' {MainForm},
  AnyiQuack in '..\..\AnyiQuack.pas',
  AQPSystemTypesAnimations in '..\..\AQPSystemTypesAnimations.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
