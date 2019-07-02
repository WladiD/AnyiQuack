unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, NotificationWindows, MyNotificationWindow, ExtCtrls;

type
  TMainForm = class(TForm)
    AddButton: TButton;
    AutoCloseCheckBox: TCheckBox;
    AutoCreateCheckBox: TCheckBox;
    Timer1: TTimer;
    CloseAllButton: TButton;
    CloseLastButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure AddButtonClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure AutoCreateCheckBoxClick(Sender: TObject);
    procedure CloseAllButtonClick(Sender: TObject);
    procedure CloseLastButtonClick(Sender: TObject);
  private
    FStack: TNotificationStack;
    FLastNotificationWindow: TNotificationWindow;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.AddButtonClick(Sender: TObject);
var
  MNW: TMyNotificationWindow;
begin
  MNW := TMyNotificationWindow.Create(nil);
  if AutoCloseCheckBox.Checked then
    MNW.CloseTimeout := 5000;
  FLastNotificationWindow := MNW;
  FStack.Add(MNW);
end;

procedure TMainForm.CloseAllButtonClick(Sender: TObject);
begin
  FStack.CloseAll;
end;

procedure TMainForm.CloseLastButtonClick(Sender: TObject);
var
  Target: TNotificationWindow;
begin
  Target := FLastNotificationWindow;
  if (FStack.List.Count > 0) and not FStack.List.Contains(Target) then
    Target := FStack.List.Last;

  FStack.Close(Target);
end;

procedure TMainForm.AutoCreateCheckBoxClick(Sender: TObject);
begin
  Timer1.Enabled := AutoCreateCheckBox.Checked;
end;

procedure TMainForm.FormCreate(Sender:TObject);
begin
  FStack := TNotificationStack.Create;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FStack.Free;
end;

procedure TMainForm.Timer1Timer(Sender: TObject);
begin
  AddButton.Click;
end;

end.
