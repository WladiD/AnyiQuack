unit MyNotificationWindow;

interface

uses
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs, StdCtrls, NotificationWindows, AnyiQuack;

type
	TMyNotificationWindow = class(TNotificationWindow)
		Label1:TLabel;
	end;

implementation

{$R *.dfm}


end.
