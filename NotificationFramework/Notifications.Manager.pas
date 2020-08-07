unit Notifications.Manager;

interface

uses
  System.Generics.Collections
  {$IFDEF FMX}
  , FMX.Forms
  {$ELSE}
  , Vcl.Forms
  , Vcl.Controls
  , Winapi.Windows
  , Winapi.Messages
  , Notifications.Base.VCL
  {$ENDIF}
  ;

type
  TNotificationManager = class
  private
    const
    PositionAnimationID = 123;
    AlphaAnimationID = 456;

    type
    TNotificationList = TObjectList<TNotificationWindow>;

    var
    FList: TNotificationList;
    FInPositionAnimationDuration: Integer;
    FInAlphaAnimationDuration: Integer;
    FOutPositionAnimationDuration: Integer;
    FOutAlphaAnimationDuration: Integer;

    procedure UpdatePositions;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Add(const NotificationWindow: TNotificationWindow);
    procedure Close(const NotificationWindow: TNotificationWindow);
    procedure CloseAll(const Animate: Boolean = True);

    property List: TNotificationList read FList;

    property InPositionAnimationDuration: Integer read FInPositionAnimationDuration
      write FInPositionAnimationDuration;
    property InAlphaAnimationDuration: Integer read FInAlphaAnimationDuration
      write FInAlphaAnimationDuration;
    property OutPositionAnimationDuration: Integer read FOutPositionAnimationDuration
      write FOutPositionAnimationDuration;
    property OutAlphaAnimationDuration: Integer read FOutAlphaAnimationDuration
      write FOutAlphaAnimationDuration;
  end;

implementation

uses
  AQPControlAnimations, AnyiQuack, System.Math;

{ TNotificationManager }

type
  TInnerWindow = class (TForm)

  end;

procedure TNotificationManager.Add(const NotificationWindow: TNotificationWindow);
begin
  NotificationWindow.CloseProc:=Close;
  List.Add(NotificationWindow);

  NotificationWindow.Left := Screen.WorkAreaRect.Right - NotificationWindow.Width;
  NotificationWindow.Top := Screen.PrimaryMonitor.BoundsRect.Bottom;
  {$IFDEF FMX}
  {$ELSE}
  ShowWindow(TInnerWindow(NotificationWindow).WindowHandle, SW_SHOWNOACTIVATE);
  NotificationWindow.AlphaBlend := True;
  {$ENDIF}
  NotificationWindow.Visible := True;
  UpdatePositions;
end;

procedure TNotificationManager.Close(const NotificationWindow:
    TNotificationWindow);
var
  NextFocusedWindowIndex: Integer;
  AniPlugin: TAQPControlAnimations;
begin
  if not List.Contains(NotificationWindow) then
    Exit;

  AniPlugin := Take(NotificationWindow)
    .CancelAnimations
    .Plugin<TAQPControlAnimations>;
  AniPlugin.BoundsAnimation(Screen.WorkAreaRect.Right, NotificationWindow.Top, -1, -1,
    OutPositionAnimationDuration, 0,
    TAQ.Ease(etCubic, emInInverted));
  AniPlugin.AlphaBlendAnimation(0, OutAlphaAnimationDuration, 0, TAQ.Ease(etSinus),
    {**
     * Handler for the OnComplete event
     *}
    procedure(Sender: TObject)
    begin
      NotificationWindow.Release;
    end);

  NextFocusedWindowIndex := List.Remove(NotificationWindow);

  if (Screen.ActiveForm = NotificationWindow) and (List.Count > 0) then
  begin
    Dec(NextFocusedWindowIndex);
    if NextFocusedWindowIndex < 0 then
      NextFocusedWindowIndex:=0;
    List[NextFocusedWindowIndex].SetFocus;
  end;

  UpdatePositions;
end;

procedure TNotificationManager.CloseAll(const Animate: Boolean = True);
var
  cc: Integer;
begin
  for cc := List.Count - 1 downto 0 do
    if Animate then
      List[cc].Close
    else
    begin
      List[cc].Release;
      List.Delete(cc);
    end;
end;

constructor TNotificationManager.Create;
begin
  inherited;
  FList := TNotificationList.Create(False);

  FInPositionAnimationDuration := 1000;
  FInAlphaAnimationDuration := 800;
  FOutPositionAnimationDuration := 500;
  FOutAlphaAnimationDuration := 300;
end;

destructor TNotificationManager.Destroy;
begin
  FList.OwnsObjects := True;
  FList.Free;
  inherited;
end;

procedure TNotificationManager.UpdatePositions;
var
  Stack: TAQ;
  WindowIndex, TopPosition: Integer;
begin
  Stack := TAQ.Managed;
  for WindowIndex := List.Count - 1 downto 0 do
    if not List[WindowIndex].Closed then
      Stack.Add(List[WindowIndex]);
  if Stack.Count = 0 then
  begin
    Stack.Die;
    Exit;
  end;

  TopPosition := Screen.WorkAreaRect.Bottom;
  WindowIndex := 0;

  Stack
    .CancelAnimations(PositionAnimationID)
    .Each(
      function(AQ: TAQ; O: TObject):Boolean
      var
        TargetNotf: TNotificationWindow absolute O;
        AniPlugin: TAQPControlAnimations;
      begin
        Dec(TopPosition, TargetNotf.Height);

        AniPlugin := Take(O).Plugin<TAQPControlAnimations>;
        AniPlugin.BoundsAnimation(
          Screen.WorkAreaRect.Right - TargetNotf.Width,
          TopPosition, -1, -1,
          IfThen(WindowIndex = 0, InPositionAnimationDuration div 2, InPositionAnimationDuration),
          PositionAnimationID, TAQ.Ease(etBack, emInInverted));
        AniPlugin.AlphaBlendAnimation(MAXBYTE, InAlphaAnimationDuration,
          AlphaAnimationID, TAQ.Ease(etSinus));

        Inc(WindowIndex);
        Result := True;
      end);
end;

end.
