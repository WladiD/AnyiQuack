program MethodTests;

uses
  Forms,
  SliceChain in 'SliceChain.pas' {Form1},
  AccessQuery in '..\..\AccessQuery.pas',
  AQPControlAnimations in '..\..\AQPControlAnimations.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
