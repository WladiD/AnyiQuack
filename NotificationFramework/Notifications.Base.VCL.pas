unit Notifications.Base.VCL;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Contnrs,
  System.Variants,
  System.Classes,
  System.Math,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Generics.Collections,

  AnyiQuack,
  AQPControlAnimations;

type
  TNotificationWindowVCL = class;
  TCloseProcedure= procedure (const NotificationWindows: TNotificationWindowVCL) of object;
  TOnClose = procedure (const ID: TGUID) of object;
  TNotificationWindowVCL = class(TForm)
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
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
    function AutoClosePossible: Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;

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
  published
    property OnClose: TOnClose read FOnClose write FOnClose;
  end;

implementation

{$R *.dfm}

{** TNotificationWindow **}

constructor TNotificationWindowVCL.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  // Win 10 bugfix: TWinControl descendants are sometimes not rendered, but
  // the switch AlphaBlend off and on again solve the issue.
  AlphaBlend := False;
  AlphaBlend := True;

  FCloseTimeout:=0;
  FClosed:=false;
  FCloseProc:=nil;
  FOnClose:=nil;
  if CreateGUID(FID) <> 0 then
    FID:=StringToGUID('{00099900-0000-0000-Z999-000000000099}');
end;

function TNotificationWindowVCL.AutoClosePossible: Boolean;
begin
  Result := (Screen.ActiveForm <> Self) and not PtInRect(BoundsRect, Mouse.CursorPos);
end;

procedure TNotificationWindowVCL.Close;
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

procedure TNotificationWindowVCL.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caNone;
  Close;
end;

procedure TNotificationWindowVCL.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    Close;
end;

procedure TNotificationWindowVCL.SetCloseTimeout(CloseTimeout: Integer);
begin
  if CloseTimeout = FCloseTimeout then
    Exit;
  FCloseTimeout := CloseTimeout;
  UpdateCloseTimeout;
end;

procedure TNotificationWindowVCL.UpdateCloseTimeout;
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
