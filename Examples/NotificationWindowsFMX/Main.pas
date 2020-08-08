unit Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts, Notifications.Manager;

type
  TFormMain = class(TForm)
    Label1: TLabel;
    cbAutoCloseAfter5: TCheckBox;
    cbAutoCreateEverySec: TCheckBox;
    btnAdd: TButton;
    Layout1: TLayout;
    btnCloseLast: TButton;
    btnCloseAll: TButton;
    Timer1: TTimer;
    procedure btnAddClick(Sender: TObject);
    procedure btnCloseAllClick(Sender: TObject);
    procedure btnCloseLastClick(Sender: TObject);
    procedure cbAutoCreateEverySecClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    fManager: TNotificationManager;
    fLastNotificationWindow: TNotificationWindow;
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

uses
  MyNotificationWindow;

{$R *.fmx}

procedure TFormMain.btnAddClick(Sender: TObject);
var
  notif: TMyNotificationWindowFMX;
begin
  notif:=TMyNotificationWindowFMX.Create(nil);
  if cbAutoCloseAfter5.IsChecked then
    notif.CloseTimeout := 5000;
  fLastNotificationWindow := notif;
  fManager.Add(notif);
end;

procedure TFormMain.btnCloseAllClick(Sender: TObject);
begin
  fManager.CloseAll;
end;

procedure TFormMain.btnCloseLastClick(Sender: TObject);
var
  Target: TNotificationWindow;
begin
  Target := fLastNotificationWindow;
  if (fManager.List.Count > 0) and not fManager.List.Contains(Target) then
    Target := fManager.List.Last;

  fManager.Close(Target);
end;

procedure TFormMain.cbAutoCreateEverySecClick(Sender: TObject);
begin
  Timer1.Enabled := cbAutoCreateEverySec.IsChecked;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  fManager.Free;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  fManager:=TNotificationManager.Create;
end;

procedure TFormMain.Timer1Timer(Sender: TObject);
begin
  btnAdd.OnClick(nil);
end;

end.
