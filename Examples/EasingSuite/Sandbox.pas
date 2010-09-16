unit Sandbox;

interface

uses
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs, StdCtrls, ExtCtrls,
	// ScriptEngine 2
	uSE2Compiler, // für den Compiler
	uSE2UnitCacheMngr, // für den Unit-Cache-Manager
	uSE2Errors, // für TSE2ErrorType
	uSE2Reader, // wird zum Lesen der Daten benutzt
	uSE2PEData, // die ByteCode-Daten
	uSE2RunTime, // für die RunTime
	uSE2IncConsole,
	uSE2OpCode, SynEditHighlighter, SynHighlighterPas, SynEdit;

type
	TSandboxForm = class(TForm)
		ConsoleMemo: TMemo;
		Splitter1: TSplitter;
		Panel1: TPanel;
		CompileButton: TButton;
    CodeEdit: TSynEdit;
    SynPasSyn1: TSynPasSyn;
    procedure CompileButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
	private
		procedure CompilerError(Sender: TObject; ErrorType: TSE2ErrorType;
			ErrorUnit, ErrorText: string; ErrorPos, ErrorLine: integer;
			UserData: TObject);
		procedure CompilerNeedUnit(Sender: TObject; const Name: string;
			const Readers: TList);

		function DoCompile:TSE2PE;
	public
		{ Public-Deklarationen }
	end;

var
	SandboxForm: TSandboxForm;

implementation

uses
	Main;

const
	SandboxScriptFile:String = 'Sandbox.script.pas';

{$R *.dfm}


{ TSandboxForm }

procedure TSandboxForm.CompileButtonClick(Sender: TObject);
var
	AppCode:TSE2PE;
begin
	AppCode:=DoCompile;
	if Assigned(AppCode) then
		MainForm.ApplyAppCode(AppCode);
end;

procedure TSandboxForm.CompilerError(Sender: TObject; ErrorType: TSE2ErrorType; ErrorUnit,
  ErrorText: string; ErrorPos, ErrorLine: integer; UserData: TObject);
var
	s:String;
begin
  case ErrorType of
  petHint : s := 'Hint';
  petWarning : s := 'Warning';
  petError : s := 'Error';
  end;

  ConsoleMemo.Lines.Add(s + Format(': [%s] [Line %d]: %s', [ErrorUnit, ErrorLine, ErrorText] ));
end;

procedure TSandboxForm.CompilerNeedUnit(Sender: TObject; const Name: string; const Readers: TList);
begin
//
end;

function TSandboxForm.DoCompile:TSE2PE;
var Compiler : TSE2Compiler;
begin
	ConsoleMemo.Clear;
	Compiler := TSE2Compiler.Create;
	try
		Compiler.UnitCache       := MainForm.UnitCache;
		Compiler.OnCompilerError := CompilerError;
		Compiler.OnGetFile       := CompilerNeedUnit;
		Result:=Compiler.Compile(CodeEdit.Text);
	finally
		Compiler.Free;
	end;
end;

procedure TSandboxForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
	Action:=caHide;
end;

procedure TSandboxForm.FormCreate(Sender: TObject);
var
	ScriptStream:TResourceStream;
begin
	if FileExists(SandboxScriptFile) then
		CodeEdit.Lines.LoadFromFile(SandboxScriptFile)
	else
	begin
		ScriptStream:=TResourceStream.Create(HInstance, 'Script', RT_RCDATA);
		try
			CodeEdit.Lines.LoadFromStream(ScriptStream);
		finally
			ScriptStream.Free;
		end;
	end;
end;

procedure TSandboxForm.FormDestroy(Sender: TObject);
begin
	CodeEdit.Lines.SaveToFile(SandboxScriptFile);
end;

end.
