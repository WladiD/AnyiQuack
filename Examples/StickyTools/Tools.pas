unit Tools;

interface

uses
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs, StdCtrls, AnyiQuack, AQPControlAnimations, AQPMessages;

type
	TToolsForm = class(TForm)
		StickyCheckBox:TCheckBox;
		SubToolsButton:TButton;
		procedure SubToolsButtonClick(Sender:TObject);
		procedure FormClose(Sender:TObject; var Action:TCloseAction);
		procedure FormShow(Sender:TObject);
	private
		FRelOwnerPos:TPoint;

		procedure UpdateRelOwnerPos;

		procedure WindowExitSizeMove(var Msg:TMessage); message WM_EXITSIZEMOVE;
	public
		function TargetPos:TPoint;
	end;

implementation

{$R *.dfm}

uses
	Main, AQPStickyTools;

{** TToolsForm **}

procedure TToolsForm.FormClose(Sender:TObject; var Action:TCloseAction);
begin
	Action:=caFree;
	TForm(Owner).SetFocus;
end;

procedure TToolsForm.FormShow(Sender: TObject);
begin
	UpdateRelOwnerPos;
end;

procedure TToolsForm.SubToolsButtonClick(Sender:TObject);
var
	PaddingLeft, PaddingTop:Integer;
	SubTools:TToolsForm;
begin
	PaddingLeft:=GetSystemMetrics(SM_CXSIZEFRAME)
		+ GetSystemMetrics(SM_CXBORDER)
		+ GetSystemMetrics(SM_CXPADDEDBORDER)
		+ 1;
	PaddingTop:=GetSystemMetrics(SM_CYSIZEFRAME)
		+ GetSystemMetrics(SM_CYBORDER)
		+ GetSystemMetrics(SM_CXPADDEDBORDER)
		+ 1;

	SubTools:=TToolsForm.Create(Self);
	with SubTools do
	begin
		PopupParent:=Self;
		Top:=Self.Top + Self.Height + PaddingTop;
		Left:=Self.Left +
			((Take(Self).ChildrenChain.FilterChain(TToolsForm).Die.Count - 1) * (80 + PaddingLeft));
		Height:=80;
		Width:=80;
		Show;
	end;

	Take(SubTools)
		.Plugin<TAQPStickyTools>;
end;

function TToolsForm.TargetPos:TPoint;
begin
	if not StickyCheckBox.Checked then
		Exit(BoundsRect.TopLeft);
	Result:=TForm(Owner).BoundsRect.TopLeft;
	Result.X:=Result.X + FRelOwnerPos.X;
	Result.Y:=Result.Y + FRelOwnerPos.Y;
end;

procedure TToolsForm.UpdateRelOwnerPos;
var
	SelfPos, OwnerPos:TPoint;
begin
	OwnerPos:=TForm(Owner).BoundsRect.TopLeft;
	SelfPos:=BoundsRect.TopLeft;
	FRelOwnerPos.X:=SelfPos.X - OwnerPos.X;
	FRelOwnerPos.Y:=SelfPos.Y - OwnerPos.Y;
end;

procedure TToolsForm.WindowExitSizeMove(var Msg: TMessage);
begin
	inherited;
	UpdateRelOwnerPos;
end;


end.
