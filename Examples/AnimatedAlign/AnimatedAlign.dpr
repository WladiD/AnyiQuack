program AnimatedAlign;

uses
  Forms,
  Main in 'Main.pas' {MainForm},
  AccessQuery in '..\..\AccessQuery.pas',
  AQPControlAnimations in '..\..\AQPControlAnimations.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
