unit Notifications.FMX.Base;

interface

uses
  System.SysUtils, System.Types, System.Classes,
  System.Variants, FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics,
  FMX.Dialogs, System.UITypes;

type
  TNotificationWindowFMX = class;
  TCloseProcedure= procedure (const NotificationWindows: TNotificationWindowFMX) of object;
  TNotificationWindowFMX = class(TForm)
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    const
    CloseDelayID = 779;

    var
    FCloseTimeout: Integer;
    FClosed: Boolean;
    FCloseProc:TCloseProcedure;

    procedure UpdateCloseTimeout;

    procedure SetCloseTimeout(CloseTimeout: Integer);
  protected
    function AutoClosePossible: Boolean; virtual;
  public
      procedure Close; reintroduce;
   {**
     * Auto close feature
     *
     * Assign a value > 0 to enable the feature or 0 to disable it.
     * The timeout is in milliseconds. The notification window is getting closed
     * automatically, after the defined timeout is expired and until the method
     * AutoClosePossible returns True.
     *}
    property CloseTimeout: Integer read FCloseTimeout write SetCloseTimeout;

    {**
     * Close procedure
     *
     * This is used to trigger the Close event in the Notification Manager
     * It is set by the Notification Manager
     * DO NOT SET IT IN THIS CLASS
     *}
    property CloseProc: TCloseProcedure read FCloseProc write FCloseProc;

    property Closed: Boolean read FClosed write FClosed;
  end;

implementation

uses
  AnyiQuack;

{$R *.fmx}

procedure TNotificationWindowFMX.FormCreate(Sender: TObject);
begin
  FCloseTimeout:=0;
  FClosed:=false;
  FCloseProc:=nil;
end;

{ TNotificationWindow }

function TNotificationWindowFMX.AutoClosePossible: Boolean;
begin
  Result := (Screen.ActiveForm <> Self) and not PtInRect(ClientRect, Screen.MousePos);
end;

procedure TNotificationWindowFMX.Close;
begin
  FClosed := True;
  if assigned(FCloseProc) then
    FCloseProc(Self)
  else
    raise Exception.Create('Close Procedure in not set in Manager');
end;

procedure TNotificationWindowFMX.FormClose(Sender: TObject; var Action:
    TCloseAction);
begin
  Action:=TCloseAction.caNone;
  Close;
end;

procedure TNotificationWindowFMX.SetCloseTimeout(CloseTimeout: Integer);
begin
  if CloseTimeout = FCloseTimeout then
    Exit;
  FCloseTimeout := CloseTimeout;
  UpdateCloseTimeout;
end;

procedure TNotificationWindowFMX.UpdateCloseTimeout;
begin
  Take(Self)
    .CancelDelays(CloseDelayID)
    .IfThen(CloseTimeout > 0)
      {**
       * Wait "long", if the auto close feature is possible
       *}
      .IfThen(AutoClosePossible)
        .EachDelay(CloseTimeout,
          function(AQ: TAQ; O: TObject): Boolean
          begin
            if AutoClosePossible then
              Close
            else
              UpdateCloseTimeout;
            Result := True;
          end, CloseDelayID)
      {**
       * Wait "short" (polling), if the auto close feature isn't possible
       *}
      .IfElse
        .EachDelay(100,
          function(AQ: TAQ; O: TObject): Boolean
          begin
            UpdateCloseTimeout;
            Result := True;
          end)
      .IfEnd
    .IfEnd;
end;

end.
