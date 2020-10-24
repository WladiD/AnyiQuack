unit Notifications.Manager;

interface

uses
  System.Generics.Collections
  {$IFDEF FMX}
  , FMX.Forms
  , FMX.Platform
  , Notifications.FMX.Base
  {$ELSE}
  , Vcl.Forms
  , Vcl.Controls
  , Winapi.Windows
  , Winapi.Messages
  , Notifications.Base.VCL
  {$ENDIF}
  ;

type
  TNotificationParent = (npMainScreen, npApplication);
  {$IFDEF FMX}
  TNotificationWindow = TNotificationWindowFMX;
  {$ELSE}
  TNotificationWindow = TNotificationWindowVCL;
  {$ENDIF}
  TNotificationManager = class
  private
    const
    PositionAnimationID = 123;
    AlphaAnimationID = 456;

    type
    TNotificationList = TObjectList<TNotificationWindow>;

    var
    FActivateNotification: boolean;
    FList: TNotificationList;
    FInPositionAnimationDuration: Integer;
    FInAlphaAnimationDuration: Integer;
    FOutPositionAnimationDuration: Integer;
    FOutAlphaAnimationDuration: Integer;
    FParent: TNotificationParent;

    procedure UpdatePositions;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Add(const NotificationWindow: TNotificationWindow);
    procedure Close(const NotificationWindow: TNotificationWindow);
    procedure CloseAll(const Animate: Boolean = True);

    property ActivateNotification: boolean read FActivateNotification write
        FActivateNotification default false;
    property List: TNotificationList read FList;

    property InPositionAnimationDuration: Integer read FInPositionAnimationDuration
      write FInPositionAnimationDuration;
    property InAlphaAnimationDuration: Integer read FInAlphaAnimationDuration
      write FInAlphaAnimationDuration;
    property OutPositionAnimationDuration: Integer read FOutPositionAnimationDuration
      write FOutPositionAnimationDuration;
    property OutAlphaAnimationDuration: Integer read FOutAlphaAnimationDuration
      write FOutAlphaAnimationDuration;
    property Parent: TNotificationParent read FParent write FParent default
        npMainScreen;
  end;

implementation

uses
  AQPControlAnimations, AnyiQuack, System.Math, System.Types, System.Classes;

{ TNotificationManager }

type
  TInnerWindow = class (TForm)

  end;

procedure TNotificationManager.Add(const NotificationWindow: TNotificationWindow);
begin
  NotificationWindow.Visible:=false;
  NotificationWindow.CloseProc:=Close;
  List.Add(NotificationWindow);

  {$IFDEF FMX}
  case fParent of
    npMainScreen: begin
                    NotificationWindow.Left := Screen.Displays[0].BoundsRect.Right - NotificationWindow.Width;
                    NotificationWindow.Top := Screen.Displays[0].BoundsRect.Bottom;
                  end;
    npApplication: begin
                     NotificationWindow.Parent:=Application.MainForm;
                     NotificationWindow.Left := round(
                        Application.MainForm.ClientToScreen(TPointF.Create(
                             Application.MainForm.Width - NotificationWindow.Width, 0)).X);
                     NotificationWindow.Top := round(
                        Application.MainForm.ClientToScreen(TPointF.Create(0,
                                        Application.MainForm.Height)).Y);

                   end;
  end;
  {$ELSE}
  case fParent of
    npMainScreen: begin
                    NotificationWindow.Left := Screen.WorkAreaRect.Right - NotificationWindow.Width;
                    NotificationWindow.Top := Screen.PrimaryMonitor.BoundsRect.Bottom;
                    ShowWindow(TInnerWindow(NotificationWindow).WindowHandle, SW_SHOWNOACTIVATE)
                  end;
    npApplication: begin
                     NotificationWindow.Left := Application.MainForm.Width
                                                        - NotificationWindow.Width;
                     NotificationWindow.Parent:=Application.MainForm;
                   end;
  end;
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
{$IFDEF FMX}
  winService: IFMXWindowService;
{$ENDIF}
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
      {$IFDEF FMX}
      if TPlatformServices.Current.
                    SupportsPlatformService(IFMXWindowService, winService) then
        winService.ReleaseWindow(NotificationWindow);
      {$ELSE}
      NotificationWindow.Release;
      {$ENDIF}
    end);

  NextFocusedWindowIndex := List.Remove(NotificationWindow);

  if (Screen.ActiveForm = NotificationWindow) and (List.Count > 0) then
  begin
    Dec(NextFocusedWindowIndex);
    if NextFocusedWindowIndex < 0 then
      NextFocusedWindowIndex:=0;
    List[NextFocusedWindowIndex].
    {$IFDEF FMX}
      Active:=true;
    {$ELSE}
      SetFocus;
    {$ENDIF}
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
      TThread.ForceQueue(nil,
        procedure
        begin
          List[cc].DisposeOf;
        end);
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

  FActivateNotification:=false;
  FParent:=npApplication;
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
  RightPosition: integer;
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

  case fParent of
    npMainScreen: TopPosition := Screen.WorkAreaRect.Bottom;
    npApplication: TopPosition := {$IFDEF FMX}
                                  round(
                        Application.MainForm.ClientToScreen(TPointF.Create(0,
                                        Application.MainForm.Height - 40)).Y);
                                  {$ELSE}
                                  Application.MainForm.Height - 40;
                                  {$ENDIF}
  end;

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
        case fParent of
          npMainScreen: RightPosition:={$IFDEF FMX}
                                        Screen.Displays[0].WorkareaRect.Right;
                                       {$ELSE}
                                        Screen.WorkAreaRect.Right;
                                       {$ENDIF}
          npApplication: RightPosition:= {$IFDEF FMX}
                                         round(
                        Application.MainForm.ClientToScreen(TPointF.Create(
                             Application.MainForm.Width - 20, 0)).X);
                                         {$ELSE}
                                         Application.MainForm.Width - 20;
                                         {$ENDIF}
        end;
        RightPosition:=RightPosition - TargetNotf.Width;

        AniPlugin.BoundsAnimation(RightPosition,
          TopPosition - 10, -1, -1,
          IfThen(WindowIndex = 0, InPositionAnimationDuration div 2, InPositionAnimationDuration),
          PositionAnimationID, TAQ.Ease(etBack, emInInverted));
        AniPlugin.AlphaBlendAnimation(high(Byte), InAlphaAnimationDuration,
          AlphaAnimationID, TAQ.Ease(etSinus));

        Inc(WindowIndex);

        if FActivateNotification then
        {$IFDEF FMX}
          TargetNotf.Active:=true
        {$ELSE}
          TargetNotf.SetFocus
        {$ENDIF}
        else
        {$IFDEF FMX}
          Application.MainForm.Active:=true;
        {$ELSE}
          Application.MainForm.SetFocus;
        {$ENDIF}
        Result := True;
      end);
end;

end.
