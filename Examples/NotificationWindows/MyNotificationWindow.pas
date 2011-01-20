unit MyNotificationWindow;

interface

uses
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs, StdCtrls, NotificationWindows, AnyiQuack;

type
	TMyNotificationWindow = class(TNotificationWindow)
		Label1:TLabel;
	private
		FCloseTimeout:Integer;

		procedure SetCloseTimeout(CloseTimeout:Integer);
	public

		property CloseTimeout:Integer read FCloseTimeout write SetCloseTimeout;
	end;

implementation

{$R *.dfm}

procedure TMyNotificationWindow.SetCloseTimeout(CloseTimeout:Integer);
begin
	if CloseTimeout = FCloseTimeout then
		Exit;
	FCloseTimeout:=CloseTimeout;
	Take(Self)
		.CancelDelays(778)
		.IfThen(CloseTimeout > 0)
		.EachDelay(CloseTimeout,
			function(AQ:TAQ; O:TObject):Boolean
			begin
				Close;
				Result:=TRUE;
			end, 778);
end;

end.
