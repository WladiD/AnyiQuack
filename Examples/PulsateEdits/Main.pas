unit Main;

interface

uses
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs, AccessQuery, AQP.Control.Animations, StdCtrls;

type
	TForm1 = class(TForm)
		Edit1: TEdit;
		Edit2: TEdit;
		Edit3: TEdit;
		Edit4: TEdit;
		Edit5: TEdit;
		Edit6: TEdit;
		Edit7: TEdit;
		CheckButton: TButton;
		Label1: TLabel;
		Label2: TLabel;
		procedure FormCreate(Sender: TObject);
		procedure CheckButtonClick(Sender: TObject);
	private
		{Private-Deklarationen}
	public
		{Public-Deklarationen}
	end;



var
	Form1:TForm1;

implementation

{$R *.dfm}

procedure TForm1.CheckButtonClick(Sender: TObject);
var
	Incorrect:TAQ;
begin
	Incorrect:=Take(Self)
		.ChildrenChain(TRUE)
		.FilterChain(
			function(AQ:TAQ; O:TObject):Boolean
			var
				Number:Integer;
			begin
				Result:=(O is TEdit) and not TryStrToInt(TEdit(O).Text, Number);
			end);

	if Incorrect.Count = 0 then
	begin
		Take(Self)
			.FinishAnimations
			.Plugin<TAQPControlAnimations>
			.ShakeAnimation(4, 20, 2, 20, 500);
		Exit;
	end;

	Incorrect
		.FinishAnimations
		.Plugin<TAQPControlAnimations>
		.ShakeAnimation(3, 10, 0, 0, 400)
		.Each(
			function(AQ:TAQ; O:TObject):Boolean
			begin
				TEdit(O).SetFocus;
				Result:=FALSE; // Es soll nur das 1. nicht ausgefüllte Feld fokussiert werden
			end);
end;

procedure TForm1.FormCreate(Sender: TObject);
var
	Pulsate:TAnonymNotifyEvent;
begin
	Pulsate:=procedure(Sender:TObject)
	var
		SenderAQ:TAQ;
	begin
		SenderAQ:=TAQ.Take(Sender);
		if (TEdit(Sender).Focused) and
			{**
			 * Diese Bedingung stellt sicher, dass keine weiteren Animationen für das Objekt laufen
			 *}
			(SenderAQ.AnimationActorsChain.Die.Count = 0) then
			SenderAQ
				.Plugin<TAQPControlAnimations>
				.ShakeAnimation(0, 0, 1, 5, 1000, 0, Pulsate);
		SenderAQ.Die;
	end;

	TAQ.Take(Self)
		.ChildrenChain(TRUE)
		.FilterChain(TEdit)
		.EachInterval(333,
			function(AQ:TAQ; O:TObject):Boolean
			begin
				Pulsate(O);
				Result:=TRUE;
			end);
end;

end.
