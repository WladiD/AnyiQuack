program StickyTools;

uses
  Forms,
  Main in 'Main.pas' {MainForm},
  Tools in 'Tools.pas' {ToolsForm},
  AccessQuery in '..\..\AccessQuery.pas',
  AQPControlAnimations in '..\..\AQPControlAnimations.pas',
  AQPMessages in '..\..\AQPMessages.pas',
  AQPStickyTools in 'AQPStickyTools.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
