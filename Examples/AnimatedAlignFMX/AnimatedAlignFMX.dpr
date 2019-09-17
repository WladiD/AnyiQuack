program AnimatedAlignFMX;

uses
  System.StartUpCopy,
  FMX.Forms,
  Main in 'Main.pas' {MainForm},
  AnyiQuack in '..\..\AnyiQuack.pas',
  AQPControlAnimations in '..\..\AQPControlAnimations.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
