program SlidingButtonsFMX;

uses
  System.StartUpCopy,
  FMX.Forms,
  Main in 'Main.pas' {Form2},
  AQPSystemTypesAnimations in '..\..\AQPSystemTypesAnimations.pas',
  AnyiQuack in '..\..\AnyiQuack.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
