unit NotificationWindows;

interface

uses
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs, Generics.Collections, Math, AnyiQuack, AQPControlAnimations;

type
	TNotificationStack = class;

	TNotificationWindow = class(TForm)
		procedure FormClose(Sender:TObject; var Action:TCloseAction);
		procedure FormKeyDown(Sender:TObject; var Key:Word; Shift:TShiftState);
	private
		const
		CloseDelayID = 779;

		var
		FCloseTimeout:Integer;
		FStack:TNotificationStack;
		FClosed:Boolean;

		procedure UpdateCloseTimeout;

		procedure SetCloseTimeout(CloseTimeout:Integer);
	protected
		function AutoClosePossible:Boolean; virtual;
	public
		procedure Close; reintroduce;

		property Stack:TNotificationStack read FStack;
		{**
		 * Auto close feature
		 *
		 * Assign a value > 0 to enable the feature or 0 to disable it.
		 * The timeout is in milliseconds. The notification window is getting closed
		 * automatically, after the defined timeout is expired and until the method
		 * AutoClosePossible returns TRUE.
		 *}
		property CloseTimeout:Integer read FCloseTimeout write SetCloseTimeout;
	end;

	TNotificationStack = class
	private
		const
		PositionAnimationID = 123;
		AlphaAnimationID = 456;

		type
		TNotificationList = TObjectList<TNotificationWindow>;

		var
		FList:TNotificationList;
		FInPositionAnimationDuration:Integer;
		FInAlphaAnimationDuration:Integer;
		FOutPositionAnimationDuration:Integer;
		FOutAlphaAnimationDuration:Integer;

		procedure Close(NotificationWindow:TNotificationWindow);
		procedure UpdatePositions;

		property List:TNotificationList read FList;
	public
		constructor Create;
		destructor Destroy; override;

		procedure Add(NotificationWindow:TNotificationWindow);
		procedure CloseAll(Animate:Boolean = TRUE);

		property InPositionAnimationDuration:Integer read FInPositionAnimationDuration
			write FInPositionAnimationDuration;
		property InAlphaAnimationDuration:Integer read FInAlphaAnimationDuration
			write FInAlphaAnimationDuration;
		property OutPositionAnimationDuration:Integer read FOutPositionAnimationDuration
			write FOutPositionAnimationDuration;
		property OutAlphaAnimationDuration:Integer read FOutAlphaAnimationDuration
			write FOutAlphaAnimationDuration;
	end;

implementation

{$R *.dfm}

{** TNotificationWindow **}

function TNotificationWindow.AutoClosePossible:Boolean;
begin
	Result:=(Screen.ActiveForm <> Self) and not PtInRect(BoundsRect, Mouse.CursorPos);
end;

procedure TNotificationWindow.Close;
begin
	FClosed:=TRUE;
	Stack.Close(Self);
end;

procedure TNotificationWindow.FormClose(Sender:TObject; var Action:TCloseAction);
begin
	Action:=caNone;
	Close;
end;

procedure TNotificationWindow.FormKeyDown(Sender:TObject; var Key:Word; Shift:TShiftState);
begin
	if Key = VK_ESCAPE then
		Close;
end;

procedure TNotificationWindow.SetCloseTimeout(CloseTimeout: Integer);
begin
	if CloseTimeout = FCloseTimeout then
		Exit;
	FCloseTimeout:=CloseTimeout;
	UpdateCloseTimeout;
end;

procedure TNotificationWindow.UpdateCloseTimeout;
begin
	Take(Self)
		.CancelDelays(CloseDelayID)
		.IfThen(CloseTimeout > 0)
			{**
			 * Wait "long", if the auto close feature is possible
			 *}
			.IfThen(AutoClosePossible)
				.EachDelay(CloseTimeout,
					function(AQ:TAQ; O:TObject):Boolean
					begin
						if AutoClosePossible then
							Close
						else
							UpdateCloseTimeout;
						Result:=TRUE;
					end, CloseDelayID)
			{**
			 * Wait "short" (polling), if the auto close feature isn't possible
			 *}
			.IfElse
				.EachDelay(100,
					function(AQ:TAQ; O:TObject):Boolean
					begin
						UpdateCloseTimeout;
						Result:=TRUE;
					end)
			.IfEnd
		.IfEnd;
end;

{** TNotificationStack **}

constructor TNotificationStack.Create;
begin
	FList:=TNotificationList.Create(FALSE);

	FInPositionAnimationDuration:=1000;
	FInAlphaAnimationDuration:=800;
	FOutPositionAnimationDuration:=500;
	FOutAlphaAnimationDuration:=300;
end;

destructor TNotificationStack.Destroy;
begin
	CloseAll(FALSE);
	FList.Free;
	inherited Destroy;
end;

procedure TNotificationStack.UpdatePositions;
var
	Stack:TAQ;
	WindowIndex, TopPosition:Integer;
begin
	Stack:=TAQ.Managed;
	for WindowIndex:=List.Count - 1 downto 0 do
		if not List[WindowIndex].FClosed then
			Stack.Add(List[WindowIndex]);
	if Stack.Count = 0 then
	begin
		Stack.Die;
		Exit;
	end;

	TopPosition:=Screen.WorkAreaRect.Bottom;
	WindowIndex:=0;

	Stack
		.CancelAnimations(PositionAnimationID)
		.Each(
			function(AQ:TAQ; O:TObject):Boolean
			var
				TargetNotf:TNotificationWindow;
			begin
				TargetNotf:=TNotificationWindow(O);
				Dec(TopPosition, TargetNotf.Height);

				with Take(O).Plugin<TAQPControlAnimations> do
				begin
					BoundsAnimation(
						Screen.WorkAreaRect.Right - TargetNotf.Width,
						TopPosition, -1, -1,
						IfThen(WindowIndex = 0, InPositionAnimationDuration div 2,
							InPositionAnimationDuration),
						PositionAnimationID, TAQ.Ease(etBack, emInInverted));
					AlphaBlendAnimation(MAXBYTE, InAlphaAnimationDuration,
						AlphaAnimationID, TAQ.Ease(etSinus));
				end;

				Inc(WindowIndex);
				Result:=TRUE;
			end);
end;

procedure TNotificationStack.Add(NotificationWindow:TNotificationWindow);
begin
	NotificationWindow.FStack:=Self;
	List.Add(NotificationWindow);

	NotificationWindow.Left:=Screen.WorkAreaRect.Right - NotificationWindow.Width;
	NotificationWindow.Top:=Screen.PrimaryMonitor.BoundsRect.Bottom;
	ShowWindow(NotificationWindow.WindowHandle, SW_SHOWNOACTIVATE);
	NotificationWindow.Visible:=TRUE;
	NotificationWindow.AlphaBlend:=TRUE;
	UpdatePositions;
end;

procedure TNotificationStack.Close(NotificationWindow:TNotificationWindow);
var
	NextFocusedWindowIndex:Integer;
begin
	with Take(NotificationWindow)
		.CancelAnimations
		.Plugin<TAQPControlAnimations> do
	begin
		BoundsAnimation(Screen.WorkAreaRect.Right, NotificationWindow.Top, -1, -1,
			OutPositionAnimationDuration, 0,
			TAQ.Ease(etCubic, emInInverted),
			{**
			 * Handler for the OnComplete event
			 *}
			procedure(Sender:TObject)
			begin
				NotificationWindow.Release;
			end);

		AlphaBlendAnimation(0, OutAlphaAnimationDuration, 0, TAQ.Ease(etSinus));
	end;

	NextFocusedWindowIndex:=List.Remove(NotificationWindow);

	if (Screen.ActiveForm = NotificationWindow) and (List.Count > 0) then
	begin
		Dec(NextFocusedWindowIndex);
		if NextFocusedWindowIndex < 0 then
			NextFocusedWindowIndex:=0;
		List[NextFocusedWindowIndex].SetFocus;
	end;

	UpdatePositions;
end;

procedure TNotificationStack.CloseAll(Animate:Boolean);
var
	cc:Integer;
begin
	for cc:=List.Count - 1 downto 0 do
		if Animate then
			List[cc].Close
		else
		begin
			List[cc].Release;
			List.Delete(cc);
		end;
end;

end.
