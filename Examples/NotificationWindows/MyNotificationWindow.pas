unit MyNotificationWindow;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Notifications.Base.VCL, Vcl.ComCtrls, Vcl.StdCtrls;

type
  TMyNotificationWindowVCL = class(TNotificationWindowVCL)
    ProgressBar: TProgressBar;
    MainActionButton: TButton;
    MainLabel: TLabel;
    procedure MainActionButtonClick(Sender: TObject);
  private
    { Private declarations }
  protected
    function AutoClosePossible: Boolean; override;
  public
    { Public declarations }
  end;

implementation

uses
  AnyiQuack;

{$R *.dfm}

function TMyNotificationWindowVCL.AutoClosePossible: Boolean;
begin
  if ProgressBar.Visible and (ProgressBar.Position < ProgressBar.Max) then
    Result := False
  else
    Result := inherited AutoClosePossible;
end;

procedure TMyNotificationWindowVCL.MainActionButtonClick(Sender: TObject);
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
