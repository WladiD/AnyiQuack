unit Main;

interface

uses
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs, AccessQuery, StdCtrls;

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
		procedure EditEnter(Sender: TObject);
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
	Incorrect:=TAQ.Take(Self)
		.Children(TRUE, TRUE)
		.Filter(
			function(AQ:TAQ; O:TObject):Boolean
			var
				Number:Integer;
			begin
				Result:=(O is TEdit) and not TryStrToInt(TEdit(O).Text, Number);
			end);

	if Incorrect.Count = 0 then
	begin
		TAQ.Take(Self).FinishAnimations.ShakeAnimation(4, 20, 2, 20, 500);
		Exit;
	end;

	Incorrect
		.FinishAnimations
		.ShakeAnimation(2, 10, 0, 0, 400);
end;

procedure TForm1.EditEnter(Sender: TObject);
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
			(SenderAQ.AnimationActors(FALSE).Die.Count = 0) then
			SenderAQ.ShakeAnimation(0, 0, 1, 5, 1000, Pulsate);
		SenderAQ.Die;
	end;
	Pulsate(Sender);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
	TAQ.Take(Self).Children(TRUE, TRUE).Filter(TEdit).Each(
		function(AQ:TAQ; O:TObject):Boolean
		begin
			Result:=TRUE;
			TEdit(O).OnEnter:=EditEnter;
		end);
end;

end.
