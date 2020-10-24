unit MyNotificationWindow;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  Notifications.FMX.Base, FMX.Controls.Presentation;

type
  TMyNotificationWindowFMX = class(TNotificationWindowFMX)
    lbTitle: TLabel;
    btnAction: TButton;
    pbrAction: TProgressBar;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure btnActionClick(Sender: TObject);
  private

  protected
    procedure OnAutoCloseCountDown(const ElapsedTimeMs: Real); override;
    function AutoClosePossible: Boolean; override;
  public
    { Public declarations }
  end;

implementation

uses
  AnyiQuack;

const
  TIME_ELAPSED_STR = 'Time to Close: %5f';

{$R *.fmx}

procedure TMyNotificationWindowFMX.FormCreate(Sender: TObject);
begin
  inherited;
  pbrAction.Visible:=false;
end;

procedure TMyNotificationWindowFMX.OnAutoCloseCountDown(
  const ElapsedTimeMs: Real);
begin
  Label1.Text:=format(TIME_ELAPSED_STR, [ElapsedTimeMs]);
end;

{ TMyNotificationWindowFMX }

function TMyNotificationWindowFMX.AutoClosePossible: Boolean;
begin
  if pbrAction.Visible and (pbrAction.Value < pbrAction.Max) then
    Result := False
  else
    Result := inherited AutoClosePossible;
end;

procedure TMyNotificationWindowFMX.btnActionClick(Sender: TObject);
begin
  inherited;
  btnAction.Enabled := False;
  pbrAction.Visible := True;

  Take(pbrAction)
    .EachInterval(60,
      function(AQ: TAQ; O: TObject): Boolean
      var
        PB: TProgressBar absolute O;
      begin
        if PB.Value < PB.Max then
          PB.Value := PB.Value + 1
        else
        begin
          AQ.CancelIntervals;
          lbTitle.Text:= 'Done!';

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
