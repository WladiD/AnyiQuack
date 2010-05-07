unit Main;

interface

uses
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs, AccessQuery;

type
	TForm1 = class(TForm)
		procedure FormClick(Sender:TObject);
	private
		{Private-Deklarationen}
	public
		{Public-Deklarationen}
	end;

var
	Form1:TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormClick(Sender:TObject);
var
	NewLeft, NewWidth:Integer;
begin
	NewWidth:=Screen.WorkAreaWidth div 2;
	if Left <> 0 then
		NewLeft:=0
	else
		NewLeft:=Screen.WorkAreaWidth - NewWidth;

	TAQ.Take(Sender).FinishAnimations.BoundsAnimation(NewLeft, 0, NewWidth, Screen.WorkAreaHeight,
		500, TAQ.Ease(etMassiveQuadratic));
end;

end.
