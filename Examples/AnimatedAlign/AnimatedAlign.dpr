program AnimatedAlign;

uses
  Forms,
  Main in 'Main.pas' {MainForm},
  AccessQuery in '..\..\AccessQuery.pas',
  AQP.Control.Animations in '..\..\AQP.Control.Animations.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
