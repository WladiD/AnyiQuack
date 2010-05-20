unit Main;

interface

uses
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs, StdCtrls, ExtCtrls, AccessQuery, AQP.Control.Animations;

type
	TForm1 = class(TForm)
		ColorListBox1:TColorListBox;
		procedure FormClick(Sender:TObject);
		procedure ColorListBox1Click(Sender:TObject);
	private
		{Private-Deklarationen}
	public
		{Public-Deklarationen}
	end;

var
	Form1:TForm1;

implementation

{$R *.dfm}

procedure TForm1.ColorListBox1Click(Sender:TObject);
begin
	Take(Form1)
		.Plugin<TAQPControlAnimations>
		.BackgroundColorAnimation(ColorListBox1.Selected, 500, 0, TAQ.Ease(etSinus));
end;

procedure TForm1.FormClick(Sender:TObject);
var
	NewLeft, NewWidth:Integer;
	NewColor:TColor;
begin
	NewWidth:=Screen.WorkAreaWidth div 2;
	if Left <> 0 then
	begin
		NewLeft:=0;
		NewColor:=clBlack;
	end
	else
	begin
		NewLeft:=Screen.WorkAreaWidth - NewWidth;
		NewColor:=clWhite;
	end;

	with Take(Sender)
		.FinishAnimations
		.Plugin<TAQPControlAnimations> do
	begin
		BoundsAnimation(NewLeft, 0, NewWidth, Screen.WorkAreaHeight,
			500, 0, TAQ.Ease(etMassiveQuadratic));
		BackgroundColorAnimation(NewColor, 1000, 0, TAQ.Ease(etMassiveQuadratic),
			procedure(Sender:TObject)
			begin
				ColorListBox1.Selected:=NewColor;
			end);
	end;
end;

end.
