program EasingSuite;

{$R *.dres}

uses
  Forms,
  Main in 'Main.pas' {MainForm},
  AnyiQuack in '..\..\AnyiQuack.pas',
  Sandbox in 'Sandbox.pas' {SandboxForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
