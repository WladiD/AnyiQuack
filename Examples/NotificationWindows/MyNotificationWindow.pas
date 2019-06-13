unit MyNotificationWindow;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ComCtrls,

  NotificationWindows,
  AnyiQuack;

type
  TMyNotificationWindow = class(TNotificationWindow)
    MainLabel: TLabel;
    MainActionButton: TButton;
    ProgressBar: TProgressBar;
    procedure MainActionButtonClick(Sender: TObject);

  protected
    function AutoClosePossible: Boolean; override;
  end;

implementation

{$R *.dfm}

function TMyNotificationWindow.AutoClosePossible: Boolean;
begin
  if ProgressBar.Visible and (ProgressBar.Position < ProgressBar.Max) then
    Result := False
  else
    Result := inherited AutoClosePossible;
end;

procedure TMyNotificationWindow.MainActionButtonClick(Sender: TObject);
begin
  MainActionButton.Enabled := False;
  ProgressBar.Visible := True;

  Take(ProgressBar)
    .EachInterval(60,
      function(AQ: TAQ; O: TObject): Boolean
      var
        PB: TProgressBar absolute O;
      begin
        if PB.Position < PB.Max then
          PB.Position := PB.Position + 1
        else
        begin
          AQ.CancelIntervals;
          MainLabel.Caption := 'Done!';

          Take(Self)
            .EachDelay(1000,
              function(AQ: TAQ; O: TObject): Boolean
              begin
                Close;
                Result := True;
              end);
        end;
        Result := True;
      end);

end;

end.
