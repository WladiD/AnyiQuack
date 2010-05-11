unit Main;

interface

uses
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs, StdCtrls, ExtCtrls, AccessQuery, ComCtrls, Math;

type
	TMainForm = class(TForm)
		AddPanelButton:TButton;
		PanelSizeTrackBar:TTrackBar;
		RemovePanelButton:TButton;
		Label1:TLabel;
		DisturbedComboBox:TComboBox;
		Label2:TLabel;
		Label3:TLabel;
		Label4:TLabel;
		TopPanel:TPanel;
		BottomPanel:TPanel;
		Label5:TLabel;
		AnimationDurationTrackBar:TTrackBar;
		procedure AddPanelButtonClick(Sender:TObject);
		procedure FormResize(Sender:TObject);
		procedure PanelSizeTrackBarChange(Sender:TObject);
		procedure RemovePanelButtonClick(Sender:TObject);
	private
		FPanelCounter:Integer;
	public
		procedure UpdateAlign;

		function GetPanelsAQ:TAQ;
	end;

var
	MainForm:TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.AddPanelButtonClick(Sender:TObject);
begin
	Inc(FPanelCounter);
	with TPanel.Create(Self) do
	begin
		Parent:=Self;
		SetBounds(-100, -100, 100, 100);
		ParentBackground:=FALSE;
		Color:=clBtnFace;
		Caption:=Format('Panel #%d', [FPanelCounter]);
		BringToFront;
	end;
	TopPanel.BringToFront;
	BottomPanel.BringToFront;
	UpdateAlign;
end;

procedure TMainForm.FormResize(Sender:TObject);
begin
	UpdateAlign;
end;

function TMainForm.GetPanelsAQ:TAQ;
begin
	Result:=TAQ.Take(MainForm)
		.Children
		.Filter(
			function(AQ:TAQ; O:TObject):Boolean
			begin
				Result:=(O is TPanel) and (TControl(O).Tag = 0);
			end);
end;

procedure TMainForm.PanelSizeTrackBarChange(Sender:TObject);
begin
	UpdateAlign;
end;

procedure TMainForm.RemovePanelButtonClick(Sender:TObject);
begin
	GetPanelsAQ
		.Last
		.Each(
			function(AQ:TAQ; O:TObject):Boolean
			begin
				Result:=TRUE;
				Dec(FPanelCounter);
				TControl(O).Tag:=1; // Dadurch wird es für GetPanelsAQ nicht greifbar
				AQ.CancelAnimations.BoundsAnimation(TControl(O).Left, Height, -1, -1,
					AnimationDurationTrackBar.Position, TAQ.Ease(etQuadratic),
					procedure(Sender:TObject)
					begin
						Sender.Free;
					end);
			end);
	UpdateAlign;
end;

procedure TMainForm.UpdateAlign;
var
	PanelsAQ:TAQ;
	AHeight, AWidth:Integer;
	PQSize, PIndex:Integer;
	PColumns, PRows, LeftOffset, TopOffset:Word;
begin
	PanelsAQ:=GetPanelsAQ;

	RemovePanelButton.Enabled:=PanelsAQ.Count > 0;

	AWidth:=ClientWidth;
	AHeight:=ClientHeight - TopPanel.Height - BottomPanel.Height;
	PQSize:=PanelSizeTrackBar.Position;
	DivMod(AWidth, PQSize, PColumns, LeftOffset);
	DivMod(AHeight, PQSize, PRows, TopOffset);
	PColumns:=Max(PColumns, 1);
	LeftOffset:=(AWidth - (Min(PColumns, PanelsAQ.Count) * PQSize)) div 2;
	TopOffset:=((AHeight - (Min(Ceil(PanelsAQ.Count / PColumns), PRows) * PQSize)) div 2) +
		TopPanel.Height;
	PIndex:=0;

	PanelsAQ
		.CancelDelays
		.EachDelay(50,
			function(AQ:TAQ; O:TObject):Boolean
			var
				TargetLeft, TargetTop:Integer;
				XTile, YTile, Dummy:Word;
			begin
				Result:=TRUE;
				{**
				 * Anstehende Animationen beenden oder abbrechen
				 *}
				if PIndex = 0 then
				begin
					if DisturbedComboBox.ItemIndex = 0 then
						AQ.CancelAnimations
					else
						AQ.FinishAnimations;
				end;

				YTile:=Floor(PIndex/PColumns);
				DivMod(((PIndex - (YTile * PColumns)) + PColumns), PColumns, Dummy, XTile);

				TargetLeft:=(XTile * PQSize) + LeftOffset;
				TargetTop:=(YTile * PQSize) + TopOffset;

				TAQ.Take(O).BoundsAnimation(TargetLeft, TargetTop, PQSize, PQSize,
					AnimationDurationTrackBar.Position, TAQ.Ease(etSinus));
				Inc(PIndex);
			end)
		.Die;
end;

end.
