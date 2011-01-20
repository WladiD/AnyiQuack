unit Main;

interface

uses
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs, StdCtrls, NotificationWindows, MyNotificationWindow, ExtCtrls;

type
	TMainForm = class(TForm)
		AddButton:TButton;
		AutoCloseCheckBox:TCheckBox;
		AutoCreateCheckBox:TCheckBox;
		Timer1:TTimer;
		CloseButton:TButton;
		procedure FormCreate(Sender:TObject);
		procedure FormDestroy(Sender:TObject);
		procedure AddButtonClick(Sender:TObject);
		procedure Timer1Timer(Sender:TObject);
		procedure AutoCreateCheckBoxClick(Sender:TObject);
		procedure CloseButtonClick(Sender:TObject);
	private
		FStack:TNotificationStack;
	end;

var
	MainForm:TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.AddButtonClick(Sender:TObject);
var
	MNW:TMyNotificationWindow;
begin
	MNW:=TMyNotificationWindow.Create(nil);
	if AutoCloseCheckBox.Checked then
		MNW.CloseTimeout:=5000;
	FStack.Add(MNW);
end;

procedure TMainForm.CloseButtonClick(Sender:TObject);
begin
	FStack.CloseAll;
end;

procedure TMainForm.AutoCreateCheckBoxClick(Sender:TObject);
begin
	Timer1.Enabled:=AutoCreateCheckBox.Checked;
end;

procedure TMainForm.FormCreate(Sender:TObject);
begin
	FStack:=TNotificationStack.Create;
end;

procedure TMainForm.FormDestroy(Sender:TObject);
begin
	FStack.Free;
end;

procedure TMainForm.Timer1Timer(Sender:TObject);
begin
	AddButton.Click;
end;

end.
