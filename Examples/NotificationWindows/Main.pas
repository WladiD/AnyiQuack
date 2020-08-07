unit Main;

interface

uses
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,

  Notifications.Base.Win,
  MyNotificationWindow,
  Notifications.Manager;

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
    FManager: TNotificationManager;
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
  FManager.Add(MNW);
end;

procedure TMainForm.CloseAllButtonClick(Sender: TObject);
begin
  FManager.CloseAll;
end;

procedure TMainForm.CloseLastButtonClick(Sender: TObject);
var
  Target: TNotificationWindow;
begin
  Target := FLastNotificationWindow;
  if (FManager.List.Count > 0) and not FManager.List.Contains(Target) then
    Target := FManager.List.Last;

  FManager.Close(Target);
end;

procedure TMainForm.AutoCreateCheckBoxClick(Sender: TObject);
begin
  Timer1.Enabled := AutoCreateCheckBox.Checked;
end;

procedure TMainForm.FormCreate(Sender:TObject);
begin
  FManager := TNotificationManager.Create;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FManager.Free;
end;

procedure TMainForm.Timer1Timer(Sender: TObject);
begin
  AddButton.Click;
end;

end.
