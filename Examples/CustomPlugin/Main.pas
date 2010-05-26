unit Main;

interface

uses
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs, StdCtrls, AccessQuery, AQPCustomPlugin;

type
	TForm1 = class(TForm)
		Button1: TButton;
		Button2: TButton;
		Button3: TButton;
		Button4: TButton;
		Button5: TButton;
		Button6: TButton;
		Button7: TButton;
		Button8: TButton;
		Button9: TButton;
		Button10: TButton;
		HideButton: TButton;
		ShowButton: TButton;
		procedure ShowButtonClick(Sender: TObject);
		procedure HideButtonClick(Sender: TObject);
	private
		function TargetButtons:TAQ;
	public
		{ Public-Deklarationen }
	end;

var
	Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.HideButtonClick(Sender: TObject);
begin
	TargetButtons
		.Plugin<TAQPCustomPlugin>
		.Hide;
end;

procedure TForm1.ShowButtonClick(Sender: TObject);
begin
	TargetButtons
		.Plugin<TAQPCustomPlugin>
		.Show;
end;

function TForm1.TargetButtons:TAQ;
begin
	Result:=Take(Form1)
		.ChildrenChain
		.FilterChain(TButton)
		.ExcludeChain(OA([HideButton, ShowButton]));
end;

end.
