program MethodTests;

uses
  Forms,
  SliceChain in 'SliceChain.pas' {Form1},
  AQPControlAnimations in '..\..\AQPControlAnimations.pas',
  AnyiQuack in '..\..\AnyiQuack.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
