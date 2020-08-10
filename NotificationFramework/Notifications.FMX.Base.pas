unit Notifications.FMX.Base;

interface

uses
  System.SysUtils, System.Types, System.Classes,
  System.Variants, FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics,
  FMX.Dialogs, System.UITypes;

type
  TNotificationWindowFMX = class;
  TCloseProcedure= procedure (const NotificationWindows: TNotificationWindowFMX) of object;
  TOnClose = procedure (const ID: TGUID) of object;
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
    FID: TGUID;
    FOnClose: TOnClose;
    procedure UpdateCloseTimeout;

    procedure SetCloseTimeout(CloseTimeout: Integer);
  protected
    procedure OnAutoCloseCountDown(const ElapsedTimeMs: Real); virtual;
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
     * DO NOT USE IT - USE OnClose Event
     *}
    property CloseProc: TCloseProcedure read FCloseProc write FCloseProc;

    property Closed: Boolean read FClosed write FClosed;
    property ID: TGUID read FID;

   published
    property OnClose: TOnClose read FOnClose write FOnClose;
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
  fOnClose:=nil;
  if CreateGUID(FID) <> 0 then
    fID:=StringToGUID('{00099900-0000-0000-Z999-000000000099}');
end;

procedure TNotificationWindowFMX.OnAutoCloseCountDown(
  const ElapsedTimeMs: Real);
begin
// DO NOT DELETE - OVERRIDE IN DESCENDENTS
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
  begin
    FCloseProc(Self);
    if assigned(FOnClose) then
      FOnClose(fID);
  end
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
//    .Each(function(AQ:TAQ; O: TObject): boolean
//                                begin
//
//          OnAutoCloseCountdown(AQ.CurrentInterval.Progress);
//
//                                end)
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
