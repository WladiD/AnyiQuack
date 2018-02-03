program SlidingForm;

uses
  Forms,
  Main in 'Main.pas' {Form1},
  AnyiQuack in '..\..\AnyiQuack.pas',
  AQPSystemTypesAnimations in '..\..\AQPSystemTypesAnimations.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
