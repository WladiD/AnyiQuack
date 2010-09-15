program StickyTools;

uses
  Forms,
  Main in 'Main.pas' {MainForm},
  Tools in 'Tools.pas' {ToolsForm},
  AQPControlAnimations in '..\..\AQPControlAnimations.pas',
  AQPMessages in '..\..\AQPMessages.pas',
  AQPStickyTools in 'AQPStickyTools.pas',
  AnyiQuack in '..\..\AnyiQuack.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
