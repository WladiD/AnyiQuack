program PulsateEdits;

uses
  Forms,
  Main in 'Main.pas' {Form1},
  AccessQuery in '..\..\AccessQuery.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
