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
    procedure btnActionClick(Sender: TObject);
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

{$R *.fmx}

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
