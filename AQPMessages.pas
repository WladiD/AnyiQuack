{**
 * The contents of this file are subject to the Mozilla Public License
 * Version 1.1 (the "License"); you may not use this file except in
 * compliance with the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
 * License for the specific language governing rights and limitations
 * under the License.
 *
 * The Original Code is AQPMessages.pas.
 *
 * The Initial Developer of the Original Code is Waldemar Derr.
 * Portions created by Waldemar Derr are Copyright (C) 2010 Waldemar Derr.
 * All Rights Reserved.
 *
 * @author Waldemar Derr <mail@wladid.de>
 * @see Project-Home (german) http://id.kihiwi.de/WladiD/His/Habe_ich/KiHiWi.187/
 * @version $Id$
 *}

unit AQPMessages;

interface

uses
	SysUtils, Classes, Controls, Windows, Messages, Contnrs, AnyiQuack;

type
	TAQPMessages = class(TAQPlugin)
	protected
		class var
		FListeners:TAQ;
		FInDispatchWindowProc:Boolean;

		var
		FListenForMsg:Cardinal;
		FListenID:Integer;
		FEachMsgFunction:TEachMiscFunction<TMessage>;

		class constructor Create;
		class destructor Destroy;

		class procedure DispatchWindowProc(Control:TControl; Message:TMessage);
	public
		destructor Destroy; override;

		class function ListenersExistsFor(Control:TControl; Msg:Cardinal = 0;
			ListenID:Integer = 0):Boolean;

		function EachMessage(Msg:Cardinal; EachMsgFunction:TEachMiscFunction<TMessage>;
			ListenID:Integer = 0):TAQ; overload;
		function EachMessage(Msgs:array of Cardinal; EachMsgFunction:TEachMiscFunction<TMessage>;
			ListenID:Integer = 0):TAQ; overload;
		function CancelMessages(Msg:Cardinal; ListenID:Integer = 0):TAQ; overload;
		function CancelMessages(Msgs:array of Cardinal; ListenID:Integer = 0):TAQ; overload;
	end;

implementation

type
	TWndMethodRec = record
		Code:Pointer;
		Obj:TObject;
	end;
	PWndProcRec = ^TWndProcRec;
	TWndProcRec = record
		OrgWndProc:TWndMethod;
		Control:TControl;
	end;

	{**
	 * TWndProcList is originally written by Andreas Hausladen
	 * <http://www.delphipraxis.net/8933-post2.html>
	 *
	 * Modified for special purposes by Waldemar Derr
	 *}
	TWndProcList = class(TList)
	private
		function GetIndex(Control:TControl):Integer;
	protected
		procedure TransferWndProc(var Message:TMessage); virtual;
	public
		procedure HookControl(Control:TControl);
		procedure UnhookControl(Control:TControl);

		procedure Clear; override;
	end;

var
	WndProcList:TWndProcList;


{** TAQPMessages **}

class constructor TAQPMessages.Create;
begin
	FListeners:=TAQ.Unmanaged;
	WndProcList:=TWndProcList.Create;
end;

class destructor TAQPMessages.Destroy;
begin
	FreeAndNil(FListeners);
	WndProcList.Free;
end;

destructor TAQPMessages.Destroy;
begin
	if not Assigned(FListeners) then
	begin
		inherited;
		Exit;
	end;

	FListeners.Remove(Self);

	if (WorkAQ.Count = 1) and not ListenersExistsFor(TControl(WorkAQ[0])) then
		WndProcList.UnhookControl(TControl(WorkAQ[0]));

	FListenForMsg:=0;

	inherited;
end;

class procedure TAQPMessages.DispatchWindowProc(Control:TControl; Message:TMessage);
begin
	if not (Assigned(FListeners) and (FListeners.Count > 0)) then
		Exit;
	FInDispatchWindowProc:=TRUE;
	try
		FListeners
			.Each(
				function(AQ:TAQ; O:TObject):Boolean
				var
					MsgPlugin:TAQPMessages;
				begin
					MsgPlugin:=TAQPMessages(O);
					if
						(MsgPlugin.FListenForMsg = Message.Msg) and
						(MsgPlugin.WorkAQ.IndexOf(Control) >= 0) then
					begin
						MsgPlugin.Each(
							function(AQ:TAQ; O:TObject):Boolean
							begin
								MsgPlugin.FEachMsgFunction(AQ, O, Message);
								Result:=TRUE;
							end);
					end;
					Result:=TRUE;
				end);
	finally
		FInDispatchWindowProc:=FALSE;
	end;
end;

function TAQPMessages.CancelMessages(Msg:Cardinal; ListenID:Integer):TAQ;
var
	CancelPlugs:TAQ;
	CancelEach:TEachFunction;
begin
	CancelPlugs:=TAQ.Managed;

	FListeners
		.Each(
			function(AQ:TAQ; O:TObject):Boolean
			var
				CheckMsgPlugin:TAQPMessages;
			begin
				CheckMsgPlugin:=TAQPMessages(O);
				if
					(CheckMsgPlugin.FListenForMsg = Msg) and
					MatchID(ListenID, CheckMsgPlugin.FListenID) and
					(CheckMsgPlugin.WorkAQ.IfContainsAny(Self.WorkAQ).Die.Count > 0) then
					CancelPlugs.Add(CheckMsgPlugin);
				Result:=TRUE;
			end);

	if CancelPlugs.Count > 0 then
	begin
		CancelEach:=function(AQ:TAQ; O:TObject):Boolean
		begin
			TAQPMessages(O).Immortally:=FALSE;
			GarbageCollector.Remove(TAQPMessages(O));
			Result:=TRUE;
		end;
		{**
		 * If this method was called from a listener closure, so me must execute the real cancel
		 * at the end of the current message loop and exactly this does EachDelay(0, ...)
		 *}
		if FInDispatchWindowProc then
			CancelPlugs.EachDelay(0, CancelEach)
		{**
		 * Otherwise it's sure to cancel immediately
		 *}
		else
			CancelPlugs.Each(CancelEach);
	end
	else
		CancelPlugs.Die;

	Result:=WorkAQ;
end;

function TAQPMessages.CancelMessages(Msgs:array of Cardinal; ListenID:Integer):TAQ;
var
	cc:Integer;
begin
	for cc:=0 to Length(Msgs) - 1 do
		CancelMessages(Msgs[cc], ListenID);
	Result:=WorkAQ;
end;

function TAQPMessages.EachMessage(Msg:Cardinal; EachMsgFunction:TEachMiscFunction<TMessage>;
	ListenID:Integer):TAQ;
begin
	Result:=Each(
		function(AQ:TAQ; O:TObject):Boolean
		var
			MsgPlugin:TAQPMessages;
		begin
			Result:=TRUE;
			if not (O is TControl) then
				Exit;
			{**
			 * Jedes TControl muss in einer eigenen TAQ-Instanz residieren, da es sonst keinen
			 * Zusammenhang zwischen der Message und dem zugehörigen TControl gibt.
			 *}
			MsgPlugin:=Take(O).Plugin<TAQPMessages>;

			MsgPlugin.FEachMsgFunction:=EachMsgFunction;
			MsgPlugin.FListenForMsg:=Msg;
			MsgPlugin.FListenID:=ListenID;

			{**
			 * Die TAQ-Instanz und somit auch die Instanz dieses Plugins unsterblich machen, bis...
			 * - Das verbundene Objekt freigegeben wird
			 * - CancelMessages entsprechend aufgerufen wird
			 *}
			MsgPlugin.Immortally:=TRUE;
			{**
			 * Add the plugin to the unmanaged class wide TAQ instance
			 *}
			FListeners.Add(MsgPlugin);
			{**
			 * Install the hook
			 *}
			WndProcList.HookControl(TControl(O));
		end);
end;

function TAQPMessages.EachMessage(Msgs:array of Cardinal;
	EachMsgFunction:TEachMiscFunction<TMessage>; ListenID:Integer):TAQ;
var
	cc:Integer;
begin
	for cc:=0 to Length(Msgs) - 1 do
		EachMessage(Msgs[cc], EachMsgFunction, ListenID);
	Result:=WorkAQ;
end;

{**
 * Sagt aus, ob TAQPMessages-Instanzen existieren, die an dem übergebenen Control horchen
 *}
class function TAQPMessages.ListenersExistsFor(Control:TControl; Msg:Cardinal;
	ListenID:Integer):Boolean;
var
	ListenersExists:Boolean;
begin
	ListenersExists:=FALSE;
	FListeners.Each(
		function(AQ:TAQ; O:TObject):Boolean
		begin
			with TAQPMessages(O) do
				ListenersExists:=
					(
						((Msg = 0) and (FListenForMsg > 0)) or
						((Msg > 0) and (FListenForMsg = Msg))
					) and
					WorkAQ.Contains(Control) and
					MatchID(ListenID, FListenID);
			Result:=not ListenersExists;
		end);
	Result:=ListenersExists;
end;

{** TWndProcList **}

procedure TWndProcList.Clear;
var
	Index:Integer;
	P:PWndProcRec;
begin
	for Index:=0 to Count - 1 do
	begin
		P:=PWndProcRec(Items[Index]);
		P^.Control.WindowProc:=P^.OrgWndProc;
		Dispose(P);
	end;
	inherited Clear;
end;

function TWndProcList.GetIndex(Control:TControl):Integer;
begin
	for Result:=0 to Count - 1 do
		if PWndProcRec(Items[Result])^.Control = Control then
			Exit;
	Result:=-1;
end;

procedure TWndProcList.TransferWndProc(var Message:TMessage);
var
	Index:Integer;
	P:PWndProcRec;
	OrgWndProc:TWndMethod;
begin
	// ATTENTION: Self points to the hooked Control!
	Index:=WndProcList.GetIndex(TControl(Self));
	if Index = -1 then
		Exit;

	P:=PWndProcRec(WndProcList.Items[Index]);
	OrgWndProc:=P^.OrgWndProc;

	if (Message.Msg = WM_DESTROY) or (csDestroying in P^.Control.ComponentState) then
	begin
		WndProcList.UnhookControl(P^.Control);
		OrgWndProc(Message);
	end
	else
	begin
		OrgWndProc(Message);
		if not (csDestroying in P^.Control.ComponentState) then
			TAQPMessages.DispatchWindowProc(P^.Control, Message);
	end;
end;

procedure TWndProcList.HookControl(Control:TControl);
var
	P:PWndProcRec;
	Proc:TWndMethod;
begin
	if GetIndex(Control) >= 0 then
		Exit;

	New(P);
	P^.Control:=Control;
	P^.OrgWndProc:=Control.WindowProc;
	Add(P);

	Proc:=TransferWndProc;
	TWndMethodRec(Proc).Obj:=Control;
	Control.WindowProc:=Proc;
end;

procedure TWndProcList.UnhookControl(Control:TControl);
var
	Index:Integer;
	P:PWndProcRec;
begin
	Index:=GetIndex(Control);
	if Index < 0 then
		Exit;

	P:=PWndProcRec(Items[Index]);
	Control.WindowProc:=P^.OrgWndProc;
	Dispose(P);
	Delete(Index);
end;

end.
