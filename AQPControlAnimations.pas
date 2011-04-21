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
 * The Original Code is AQPControlAnimations.pas.
 *
 * The Initial Developer of the Original Code is Waldemar Derr.
 * Portions created by Waldemar Derr are Copyright (C) 2010 Waldemar Derr.
 * All Rights Reserved.
 *
 * @author Waldemar Derr <mail@wladid.de>
 * @see Project-Home (german) http://id.kihiwi.de/WladiD/His/Habe_ich/KiHiWi.187/
 * @version $Id$
 *}

unit AQPControlAnimations;

interface

uses
	Windows, Controls, Forms, Math, Graphics, AnyiQuack;

type
	TAQPControlAnimations = class(TAQPlugin)
	protected
		function Swing(Times, Diff:Integer; Progress:Real):Integer;
		procedure CustomColorAnimation(FromColor, ToColor:TColor; Duration:Integer; ID:Integer;
			const ColorAssignFunction:TEachMiscFunction<TColor>; EaseFunction:TEaseFunction = nil;
			const OnComplete:TAnonymNotifyEvent = nil);
	public
		function AlphaBlendAnimation(ToAlphaBlendValue:Byte; Duration:Integer; ID:Integer = 0;
			const EaseFunction:TEaseFunction = nil; const OnComplete:TAnonymNotifyEvent = nil):TAQ;
		function BoundsAnimation(NewLeft, NewTop, NewWidth, NewHeight:Integer; Duration:Integer;
			ID:Integer = 0; const EaseFunction:TEaseFunction = nil;
			const OnComplete:TAnonymNotifyEvent = nil):TAQ;
		function ShakeAnimation(XTimes, XDiff, YTimes, YDiff, Duration:Integer; ID:Integer = 0;
			const OnComplete:TAnonymNotifyEvent = nil):TAQ;
		function BackgroundColorAnimation(ToColor:TColor; Duration:Integer; ID:Integer = 0;
			const EaseFunction:TEaseFunction = nil; const OnComplete:TAnonymNotifyEvent = nil):TAQ;
		function FontColorAnimation(ToColor:TColor; Duration:Integer; ID:Integer = 0;
			const EaseFunction:TEaseFunction = nil; const OnComplete:TAnonymNotifyEvent = nil):TAQ;
	end;

implementation

type
	TControlRobin = class helper for TControl
	protected
		function GetBackgroundColor:TColor;
		procedure SetBackgroundColor(NewColor:TColor);
		function GetFontColor:TColor;
		procedure SetFontColor(NewColor:TColor);
	public
		property BackgroundColor:TColor read GetBackgroundColor write SetBackgroundColor;
		property FontColor:TColor read GetFontColor write SetFontColor;
	end;

	TFormRobin = class helper for TCustomForm
	protected
		function GetAlphaBlendActivated:Boolean;
		function GetAlphaBlendValue:Byte;
		procedure SetAlphaBlendValue(NewAlphaBlendValue:Byte);
	public
		property AlphaBlendActivated:Boolean read GetAlphaBlendActivated;
		property AlphaBlendValue:Byte read GetAlphaBlendValue write SetAlphaBlendValue;
	end;


{** TAQPControlAnimations **}

{**
 * Animates the AlphaBlendValue on all contained TCustomForm descendants
 *
 * TCustomForm.AlphaBlend must be set to TRUE previously.
 *}
function TAQPControlAnimations.AlphaBlendAnimation(ToAlphaBlendValue:Byte; Duration, ID:Integer;
	const EaseFunction:TEaseFunction; const OnComplete:TAnonymNotifyEvent):TAQ;
begin
	Result:=Each(
		function(AQ:TAQ; O:TObject):Boolean
		var
			StartAlphaBlendValue:Byte;
		begin
			Result:=TRUE;

			if not (
				(O is TCustomForm) and
				TCustomForm(O).AlphaBlendActivated and
				(TCustomForm(O).AlphaBlendValue <> ToAlphaBlendValue)) then
				Exit;

			StartAlphaBlendValue:=TCustomForm(O).AlphaBlendValue;

			Take(O)
				.EachAnimation(Duration,
					function(AQ:TAQ; O:TObject):Boolean
					begin
						TCustomForm(O).AlphaBlendValue:=Byte(TAQ.EaseInteger(
							StartAlphaBlendValue, ToAlphaBlendValue, AQ.CurrentInterval.Progress,
							EaseFunction));

						if Assigned(OnComplete) and (AQ.CurrentInterval.Progress = 1) then
							OnComplete(O);

						Result:=TRUE;
					end);
		end);
end;

function TAQPControlAnimations.BackgroundColorAnimation(ToColor:TColor; Duration, ID:Integer;
	const EaseFunction:TEaseFunction; const OnComplete:TAnonymNotifyEvent):TAQ;
begin
	Result:=Each(
		function(AQ:TAQ; O:TObject):Boolean
		begin
			Result:=TRUE; // Komplett durchlaufen
			if O is TControl then
				Take(O)
					.Plugin<TAQPControlAnimations>
					.CustomColorAnimation(TControl(O).BackgroundColor, ToColor, Duration, ID,
						function(AQ:TAQ; O:TObject; Color:TColor):Boolean
						begin
							TControl(O).BackgroundColor:=Color;
							Result:=TRUE;
						end,
						EaseFunction, OnComplete);
		end);
end;

function TAQPControlAnimations.BoundsAnimation(
	NewLeft, NewTop, NewWidth, NewHeight, Duration, ID:Integer;
	const EaseFunction:TEaseFunction; const OnComplete:TAnonymNotifyEvent):TAQ;
var
	WholeEach:TEachFunction;
begin
	WholeEach:=function(AQ:TAQ; O:TObject):Boolean
	var
		EachF:TEachFunction;
		PrevLeft, PrevTop, PrevWidth, PrevHeight:Integer;
	begin
		Result:=TRUE;
		if not (O is TControl) then
			Exit;

		with TControl(O) do
		begin
			PrevLeft:=Left;
			PrevTop:=Top;
			PrevWidth:=Width;
			PrevHeight:=Height;
		end;

		EachF:=function(AQ:TAQ; O:TObject):Boolean
		var
			Progress:Real;
			AniLeft, AniTop, AniWidth, AniHeight:Integer;
		begin
			Result:=TRUE;
			Progress:=AQ.CurrentInterval.Progress;

			AniLeft:=TAQ.EaseInteger(PrevLeft, NewLeft, Progress, EaseFunction);
			AniTop:=TAQ.EaseInteger(PrevTop, NewTop, Progress, EaseFunction);
			if NewWidth >= 0 then
				AniWidth:=TAQ.EaseInteger(PrevWidth, NewWidth, Progress, EaseFunction)
			else
				AniWidth:=TControl(O).Width;
			if NewHeight >= 0 then
				AniHeight:=TAQ.EaseInteger(PrevHeight, NewHeight, Progress, EaseFunction)
			else
				AniHeight:=TControl(O).Height;

			TControl(O).SetBounds(AniLeft, AniTop, AniWidth, AniHeight);

			if Progress = 1 then
			begin
				{$IFDEF OutputDebugAnimation}
				OutputDebugString(PWideChar('BoundsAnimation beendet für $' +
					IntToHex(Integer(O), SizeOf(Integer) * 2)));
				{$ENDIF}

				if Assigned(OnComplete) then
					OnComplete(O);
			end;
		end;

		Take(O).EachAnimation(Duration, EachF, nil, ID);
	end;

	Result:=Each(WholeEach);
end;

procedure TAQPControlAnimations.CustomColorAnimation(FromColor, ToColor:TColor;
	Duration, ID:Integer;
	const ColorAssignFunction:TEachMiscFunction<TColor>;
	EaseFunction:TEaseFunction; const OnComplete:TAnonymNotifyEvent);
begin
	if FromColor = ToColor then
		Exit;

	if not Assigned(EaseFunction) then
		EaseFunction:=TAQ.Ease(etLinear);
	WorkAQ.EachAnimation(Duration,
		function(AQ:TAQ; O:TObject):Boolean
		var
			Progress:Real;
		begin
			Progress:=AQ.CurrentInterval.Progress;
			Result:=ColorAssignFunction(AQ, O,
				TAQ.EaseColor(FromColor, ToColor, Progress, EaseFunction));

			if (Progress = 1) and Assigned(OnComplete) then
				OnComplete(O);
		end,
		nil, ID);
end;

function TAQPControlAnimations.FontColorAnimation(ToColor:TColor; Duration, ID:Integer;
	const EaseFunction:TEaseFunction; const OnComplete:TAnonymNotifyEvent):TAQ;
begin
	Result:=Each(
		function(AQ:TAQ; O:TObject):Boolean
		begin
			Result:=TRUE; // Komplett durchlaufen
			if O is TControl then
				Take(O)
					.Plugin<TAQPControlAnimations>
					.CustomColorAnimation(TControl(O).FontColor, ToColor, Duration, ID,
						function(AQ:TAQ; O:TObject; Color:TColor):Boolean
						begin
							TControl(O).FontColor:=Color;
							Result:=TRUE;
						end,
						EaseFunction, OnComplete);
		end);
end;

function TAQPControlAnimations.ShakeAnimation(XTimes, XDiff, YTimes, YDiff, Duration, ID:Integer;
	const OnComplete:TAnonymNotifyEvent):TAQ;
var
	WholeEach:TEachFunction;
begin
	WholeEach:=function(AQ:TAQ; O:TObject):Boolean
	var
		EachF:TEachFunction;
		PrevLeft, PrevTop:Integer;
	begin
		Result:=TRUE;
		if not (O is TControl) then
			Exit;

		with TControl(O) do
		begin
			PrevLeft:=Left;
			PrevTop:=Top;
		end;

		EachF:=function(AQ:TAQ; O:TObject):Boolean
		var
			Progress:Real;
			AniLeft, AniTop:Integer;


		begin
			Result:=TRUE;
			Progress:=AQ.CurrentInterval.Progress;
			AniLeft:=PrevLeft;
			AniTop:=PrevTop;

			if Progress < 1 then
			begin
				if XDiff > 0 then
					AniLeft:=AniLeft + Swing(XTimes, XDiff, Progress);
				if YDiff > 0 then
					AniTop:=PrevTop + Swing(YTimes, YDiff, Progress);
			end;

			with TControl(O) do
				SetBounds(AniLeft, AniTop, Width, Height);

			if Progress = 1 then
			begin
				{$IFDEF OutputDebugAnimation}
				OutputDebugString(PWideChar('ShakeAnimation beendet für $' + IntToHex(Integer(O),
					SizeOf(Integer) * 2)));
				{$ENDIF}

				if Assigned(OnComplete) then
					OnComplete(O);
			end;
		end;

		Take(O).EachAnimation(Duration, EachF, nil, ID);
	end;

	Result:=Each(WholeEach);
end;

function TAQPControlAnimations.Swing(Times, Diff:Integer; Progress:Real):Integer;
begin
	Result:=Ceil(Diff * Sin(Progress * Times * Pi * 2));
end;

{** TControlRobin **}

function TControlRobin.GetBackgroundColor:TColor;
begin
	Result:=Color;
end;

function TControlRobin.GetFontColor: TColor;
begin
	Result:=Font.Color;
end;

procedure TControlRobin.SetBackgroundColor(NewColor:TColor);
begin
	Color:=NewColor;
end;

procedure TControlRobin.SetFontColor(NewColor:TColor);
begin
	Font.Color:=NewColor;
end;

{** TFormRobin **}

function TFormRobin.GetAlphaBlendActivated:Boolean;
begin
	Result:=AlphaBlend;
end;

function TFormRobin.GetAlphaBlendValue:Byte;
begin
	Result:=inherited AlphaBlendValue;
end;

procedure TFormRobin.SetAlphaBlendValue(NewAlphaBlendValue:Byte);
begin
	inherited AlphaBlendValue:=NewAlphaBlendValue;
end;

end.
