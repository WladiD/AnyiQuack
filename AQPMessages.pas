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
	Controls, Windows, Messages, Contnrs, AnyiQuack;

type
	TAQPMessages = class(TAQPlugin)
	protected
		class var
		FListeners:TAQ;
		var
		FOriginalWindowProc:TWndMethod;
		FListenForMsg:Cardinal;
		FListenID:Integer;
		FEachMsgFunction:TEachMiscFunction<TMessage>;

		class constructor Create;
		class destructor Destroy;

		class function GetNextListenerFor(Control:TControl):TAQPMessages;

		procedure HookWindowProc(var Message:TMessage);
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

{** TAQPMessages **}

class constructor TAQPMessages.Create;
begin
	FListeners:=TAQ.Unmanaged;
end;

destructor TAQPMessages.Destroy;
var
	NextListener:TAQPMessages;
begin
	FListenForMsg:=0;
	FListeners.Remove(Self);

	if WorkAQ.Count = 1 then
		NextListener:=GetNextListenerFor(TControl(WorkAQ[0]))
	else
		NextListener:=nil;

	{**
	 * Die Original-Window-Prozedur an den nächsten noch aktiven Listener zuweisen
	 *}
	if Assigned(NextListener) then
	begin
		with NextListener do
		begin
			TControl(Self.WorkAQ.Items[0]).WindowProc:=HookWindowProc;
			if Assigned(Self.FOriginalWindowProc) then
				FOriginalWindowProc:=Self.FOriginalWindowProc;
		end;
	end
	{**
	 * Falls es keine weiteren Listener gibt, die Original-Window-Prozedur dem Control zuweisen
	 *}
	else if Assigned(FOriginalWindowProc) then
		Each(
			function(AQ:TAQ; O:TObject):Boolean
			begin
				TControl(O).WindowProc:=FOriginalWindowProc;
				Result:=TRUE;
			end);

	inherited;
end;

class destructor TAQPMessages.Destroy;
begin
	FListeners.Free;
end;

function TAQPMessages.CancelMessages(Msg:Cardinal; ListenID:Integer):TAQ;
begin
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
				begin
					{**
					 * Hierdurch stirbt die verbundene TAQ-Instanz, da keine Ticks gemacht wurden
					 *}
					CheckMsgPlugin.Immortally:=FALSE;
					GarbageCollector.Remove(CheckMsgPlugin);
				end;
				Result:=TRUE;
			end);
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
			 * Nur die Original-Window-Prozedur wird festgehalten
			 *}
			if not ListenersExistsFor(TControl(O)) then
				MsgPlugin.FOriginalWindowProc:=TControl(O).WindowProc;

			TControl(O).WindowProc:=MsgPlugin.HookWindowProc;
			{**
			 * Die TAQ-Instanz und somit auch die Instanz dieses Plugins unsterblich machen, bis...
			 * - Das verbundene Objekt freigegeben wird
			 * - CancelMessages entsprechend aufgerufen wird
			 *}
			MsgPlugin.Immortally:=TRUE;
			{**
			 * Das Plugin in einer klassenweiten TAQ-Instanz festhalten
			 *}
			FListeners.Add(MsgPlugin);
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

class function TAQPMessages.GetNextListenerFor(Control:TControl):TAQPMessages;
var
	NextListener:TAQPMessages;
begin
	NextListener:=nil;
	FListeners.Each(
		function(AQ:TAQ; O:TObject):Boolean
		begin
			with TAQPMessages(O) do
				if(FListenForMsg > 0) and WorkAQ.Contains(Control) then
					NextListener:=TAQPMessages(O);
			Result:=not Assigned(NextListener);
		end);
	Result:=NextListener;
end;

procedure TAQPMessages.HookWindowProc(var Message:TMessage);
var
	MessageForward:TMessage;
	cc:Integer;
	MsgPlugin:TAQPMessages;
begin
	MessageForward:=Message;
	{**
	 * In diesem Fall musste doch eine Schleife her, da var-Parameter (Message) nicht in anonymen
	 * Methoden gegriffen werden können.
	 *}
	cc:=0;
	while cc < FListeners.Count do
	begin
		MsgPlugin:=TAQPMessages(FListeners[cc]);

		if (WorkAQ.Count = 1) and MsgPlugin.WorkAQ.Contains(WorkAQ[0]) then
		begin
			if Assigned(MsgPlugin.FOriginalWindowProc) then
				MsgPlugin.FOriginalWindowProc(Message);

			if MsgPlugin.FListenForMsg = Message.Msg then
				MsgPlugin.Each(
					function(AQ:TAQ; O:TObject):Boolean
					begin
						MsgPlugin.FEachMsgFunction(AQ, O, MessageForward);
						Result:=TRUE;
					end);
		end;
		Inc(cc);
	end;
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

end.
