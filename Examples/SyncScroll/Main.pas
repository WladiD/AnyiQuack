unit Main;

interface

uses
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs, StdCtrls, ExtCtrls, ComCtrls, Math, AccessQuery;

type
	TForm1 = class(TForm)
		ListBox1:TListBox;
		ListBox2:TListBox;
		ListBox3:TListBox;
		ListBox4:TListBox;
		Panel1:TPanel;
		Panel2:TPanel;
		SyncScrollBar: TScrollBar;
		ItemsCountTrackBar: TTrackBar;
		Label1:TLabel;
		Label2:TLabel;
    AnimatedScrollCheckBox: TCheckBox;
		procedure ItemsCountTrackBarChange(Sender:TObject);
		procedure FormCreate(Sender:TObject);
		procedure SyncScrollBarChange(Sender:TObject);
	private
		function ListBoxesAQ:TAQ;
	public
		{Public-Deklarationen}
	end;

var
	Form1:TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
	ItemsCountTrackBarChange(ItemsCountTrackBar);
end;

procedure TForm1.ItemsCountTrackBarChange(Sender: TObject);
var
	AQ:TAQ;
begin
	AQ:=TAQ.Take(Sender);
	AQ.TimerActors.CancelTimers;
	AQ
		.EachDelay(500,
			function(AQ:TAQ; O:TObject):Boolean
			begin
				Result:=TRUE;
				SyncScrollBar.SetParams(0, 0, TTrackBar(Sender).Position - 1);
				ListBoxesAQ
					.Each(
						function(AQ:TAQ; O:TObject):Boolean
						begin
							Result:=TRUE;
							with TListBox(O) do
							begin
								Clear;
								Items.BeginUpdate
							end;
						end)
					.EachRepeat(TTrackBar(O).Position,
						function(AQ:TAQ; O:TObject):Boolean
						begin
							Result:=TRUE;
							with TListBox(O) do
								Items.Add(Format('Item #%d', [Items.Count + 1]));
						end)
					.Each(
						function(AQ:TAQ; O:TObject):Boolean
						begin
							Result:=TRUE;
							TListBox(O).Items.EndUpdate;
						end);
			end);
end;

function TForm1.ListBoxesAQ: TAQ;
begin
	Result:=TAQ.Take(Form1)
		.Children(TRUE, TRUE)
		.Filter(TListBox);
end;

procedure TForm1.SyncScrollBarChange(Sender: TObject);
var
	FirstItemIndex, ScrollItemIndex:Integer;
	AQ:TAQ;
	ScrollEach:TEachFunction;
begin
	FirstItemIndex:=ListBox1.TopIndex;
	ScrollItemIndex:=SyncScrollBar.Position;
	Form1.Caption:=IntToStr(ScrollItemIndex);

	ScrollEach:=function(AQ:TAQ; O:TObject):Boolean
	begin
		Result:=TRUE;
		if not (O is TListBox) then
			Exit;
		{**
		 * Animiert
		 *}
		if Assigned(AQ.CurrentInterval) then
			TListBox(O).TopIndex:=Round(TAQ.Ease(etQuadratic)(FirstItemIndex, ScrollItemIndex,
				AQ.CurrentInterval.Progress))
		{**
		 * Sofort
		 *}
		else
			TListBox(O).TopIndex:=ScrollItemIndex;
	end;

	AQ:=ListBoxesAQ;
	if AnimatedScrollCheckBox.Checked then
	begin
		AQ.TimerActors.CancelTimers;
		AQ.EachTimer(125, ScrollEach);
	end
	else
		AQ.Each(ScrollEach);

{	if AQ.TimerActors(FALSE).Contains(ListBox1) then
		ShowMessage('ListBox1 ist enthalten');
}
end;

end.
