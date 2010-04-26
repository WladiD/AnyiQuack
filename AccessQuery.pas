{**
 * "The contents of this file are subject to the Mozilla Public License
 * Version 1.1 (the "License"); you may not use this file except in
 * compliance with the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
 * License for the specific language governing rights and limitations
 * under the License.
 *
 * The Original Code is AccessQuery.pas.
 *
 * The Initial Developer of the Original Code is Waldemar Derr.
 * Portions created by Waldemar Derr are Copyright (C) 2010 Waldemar Derr.
 * All Rights Reserved.
 *
 * @author Waldemar Derr <mail@wladid.de>
 * @see Project-Home (german) http://id.kihiwi.de/WladiD/His/Habe_ich/KiHiWi.187/
 * @version $Id$
 *}

unit AccessQuery;

interface

uses
	SysUtils, Classes, Controls, ExtCtrls, Contnrs, Windows, Math;

{$IFDEF DEBUG}
	{$INCLUDE Debug.inc}
{$ENDIF}
{**
 * Sicherstellen, dass die boolesche Kurzauswertung verwendet wird
 *}
{$BOOLEVAL OFF}

type
	EAQ = class(Exception);
	TAQ = class;
	TInterval = class;

	TObjectArray = array of TObject;
	TEachFunction = reference to function(AQ:TAQ; O:TObject):Boolean;
	TAnonymNotifyEvent = reference to procedure(Sender:TObject);
	TEaseFunction = function(StartValue, EndValue, Progress:Real):Real;
	TEaseType = (etLinear, etQuadratic, etMassiveQuadratic);

	{**
	 * AQ ist das Kürzel für AccessQuery und bildet die Kernklasse der AccessQuery-Unit
	 *
	 * TAQ greift ein oder beliebig viele Objekte und bietet Methoden, diese mittels anonymer
	 * Funktionen zu verarbeiten.
	 *
	 * Die Verarbeitung kann...
	 *
	 * - sofort (TAQ.Each),
	 * - wiederholt (TAQ.EachRepeat),
	 * - zeitversetzt (TAQ.EachDelay),
	 * - zeitbegrenzt (TAQ.EachTimer)
	 * - oder in Zeit-Intervallen (TAQ.EachInterval)
	 *
	 * durchgeführt werden.
	 *
	 * Chain-Pattern
	 * -------------
	 * TAQ wurde mit Hinblick auf das äußerst produktive Chain-Pattern entwickelt. Das Chain-Pattern
	 * ist nichts weiteres, als die Verwendung von mehreren Objekt-Methoden in einer Anweisung.
	 * Alle Methoden, die ein TAQ-Objekt zurückliefern sind zu diesem Entwurfsmuster kompatibel.
	 *
	 * Beispiel:
	 * TAQ.Take(Form1).Children(FALSE, TRUE).Filter(TButton).Each(EachFunction).Die;
	 *
	 * Beispiel-Erläuterung:
	 * - TAQ.Take(Form1) ist eine Klassenmethode und liefert uns eine neue gemanagete TAQ-Instanz,
	 *   welche die Form1 aufnimmt.
	 * - Children(FALSE, TRUE) ermittels rekursiv alle Kindsobjekte von Form1 und liefert wieder
	 *   eine neue TAQ-Instanz. Wenn der 1. Parameter TRUE wäre, würde es die aktuelle Instanz
	 *   beibehalten.
	 * - Filter(TButton) läuft nun alle von Children ermittelten Objekte durch und nimmt nur die
	 *   Objekte auf, die vom Typ/Klasse TButton sind und packt sie wieder in ein neues TAQ-Objekt.
	 * - Each(EachFunction) hier kann man nun bequem mittels einer anonymen Funktion vom Typ
	 *   TEachFunction, alle Buttons verarbeiten.
	 * - Die Die-Methode ist absolut freiwillig und bedeutet lediglich, dass wir die letzte
	 *   TAQ-Instanz nicht brauchen. Auf diese Weise kann der GarbageCollector sie schneller
	 *   freigeben oder wiederverwenden.
	 *
	 * Gemanagete TAQ-Instanzen
	 * ------------------------
	 * TAQ beherbergt einen klassenweit aktiven GarbageCollector, der instanzierte TAQ-Objekte
	 * verwaltet und sich (unter anderem) um dessen Freigabe kümmert. Jede TAQ-Instanz hat eine
	 * begrenzte Lebensdauer, die von der Konstante MaxLifeTime bestimmt wird, wird es nun für die
	 * definierte Zeitspanne inaktiv, wird es freigegeben oder wiederverwendet.
	 *
	 * Ungemanagete TAQ-Instanzen
	 * --------------------------
	 * Eine TAQ-Instanz gilt als nicht verwaltet, wenn sie über den TAQ-Konstruktor oder mittels
	 * der Klassenmethode TAQ.Unmanaged erzeugt wird. Diese werden nicht automatisch freigegeben.
	 *
	 * Globaler TTimer
	 * ---------------
	 * Beliebig viele TAQ-Objekte können von Timer-Funktionalität gebrauch nehmen, doch es wird nur
	 * ein klassenweit gültiges TTimer-Objekt verwendet. Dieses läuft in einer festen Auflösung, die
	 * in der Konstante IntervalResolution definiert ist.
	 *}
	TAQ = class(TObjectList)
	private
	class var
		FGarbageCollector:TAQ;
		FIntervalTimer:TTimer;
		FActiveIntervalAQs:TAQ;
	protected
		type
			TCancelType = (ctInterval, ctTimer, ctTimerFinish, ctAnimation, ctAnimationFinish);
		var
		FLifeTick:Cardinal;
		FIntervals:TObjectList;
		FCurrentInterval:TInterval;
		FAnimating:Boolean;
		FRecurse:Boolean;

		class function GarbageCollector:TAQ;
		class procedure GlobalIntervalTimerEvent(Sender:TObject);


		function HasTimer:Boolean;
		function HasInterval:Boolean;

		procedure LocalIntervalTimerEvent(Sender:TObject);

		function GetIntervals:TObjectList;
		procedure ClearIntervals;
		procedure AddInterval(Interval:TInterval);
		function ProcessInterval(Interval:TInterval):Boolean;
		procedure RemoveInterval(Interval:TInterval);

		procedure Animate(Duration:Integer; Each:TEachFunction; LastEach:TEachFunction = nil);

		function CustomFiller(Filler:TEachFunction; Append, Recurse:Boolean):TAQ;
		procedure CustomCancel(Local:Boolean; CancelType:TCancelType);

		function IsAlive:Boolean;
		procedure HeartBeat;

		property Recurse:Boolean read FRecurse;
	public
		constructor Create; reintroduce;
		destructor Destroy; override;

		class function Managed:TAQ;
		class function Unmanaged:TAQ;

		class function Take(Objects:TObjectArray):TAQ; overload;
		class function Take(Objects:TObjectList):TAQ; overload;
		class function Take(AObject:TObject):TAQ; overload;

		class function Ease(EaseType:TEaseType):TEaseFunction; overload;
		class function Ease(EaseFunction:TEaseFunction = nil):TEaseFunction; overload;

		procedure Clean;
		function Die:TAQ;

		function Append(Objects:TObjectArray):TAQ; overload;
		function Append(Objects:TObjectList):TAQ; overload;
		function Append(AObject:TObject):TAQ; overload;

		function AppendAQ(AQ:TAQ):TAQ;

		function Children(Append:Boolean = FALSE; Recurse:Boolean = FALSE;
			ChildrenFiller:TEachFunction = nil):TAQ;
		function Parents(Append:Boolean = FALSE; Recurse:Boolean = FALSE;
			ParentsFiller:TEachFunction = nil):TAQ;

		function Multiplex:TAQ;
		function Demultiplex:TAQ;

		function AnimationActors(IncludeOrphans:Boolean = TRUE):TAQ;
		function IntervalActors(IncludeOrphans:Boolean = TRUE):TAQ;
		function TimerActors(IncludeOrphans:Boolean = TRUE):TAQ;

		function Each(EachFunction:TEachFunction):TAQ;
		function EachInterval(Interval:Integer; Each:TEachFunction):TAQ;
		function EachTimer(Duration:Integer; Each:TEachFunction; LastEach:TEachFunction = nil):TAQ;
		function EachDelay(Delay:Integer; Each:TEachFunction):TAQ;
		function EachRepeat(Times:Integer; EachFunction:TEachFunction):TAQ;

		function FinishAnimations(Local:Boolean = FALSE):TAQ;
		function CancelAnimations(Local:Boolean = FALSE):TAQ;
		function FinishTimers(Local:Boolean = FALSE):TAQ;
		function CancelTimers(Local:Boolean = FALSE):TAQ;
		function CancelDelays(Local:Boolean = FALSE):TAQ;
		function CancelIntervals(Local:Boolean = FALSE):TAQ;

		function Filter(ByClass:TClass):TAQ; overload;
		function Filter(FilterEach:TEachFunction):TAQ; overload;
		function First:TAQ;
		function Last:TAQ;

		function BoundsAnimation(NewLeft, NewTop, NewWidth, NewHeight:Integer; Duration:Integer;
			EaseFunction:TEaseFunction = nil; OnComplete:TAnonymNotifyEvent = nil):TAQ;
		function ShakeAnimation(XTimes, XDiff, YTimes, YDiff, Duration:Integer;
			OnComplete:TAnonymNotifyEvent = nil):TAQ;

		function Contains(AObject:TObject):Boolean;

		property CurrentInterval:TInterval read FCurrentInterval;
		property Animating:Boolean read FAnimating;
	end;

	TInterval = class
	private
		FFirstTick,
		FNextTick,
		FLastTick:Cardinal;
		FInterval:Integer;
		FNextEach,
		FLastEach:TEachFunction;
	protected
		procedure UpdateNextTick;
	public
		constructor Infinite(Interval:Integer; Each:TEachFunction);
		constructor Finite(Duration:Integer; Each, LastEach:TEachFunction);

		function Each:TEachFunction;
		function IsCanceled:Boolean;
		function IsFinished:Boolean;
		function IsFinite:Boolean;
		function Progress:Real;

		procedure Finish;
		procedure Cancel;
	end;

implementation

const
	MaxLifeTime = 10000;
	IntervalResolution = 40;
	GarbageCleanInterval = 5000;

function LinearEase(StartValue, EndValue, Progress:Real):Real;
var
	Delta:Real;
begin
	Delta:=EndValue - StartValue;
	Result:=StartValue + (Delta * Progress);
end;

function QuadraticEase(StartValue, EndValue, Progress:Real):Real;
var
	Delta:Real;
begin
	Delta:=EndValue - StartValue;
	Result:=StartValue + (Delta * Sqr(Progress));
end;

function MassiveQuadraticEase(StartValue, EndValue, Progress:Real):Real;
var
	Delta:Real;
begin
	Delta:=EndValue - StartValue;
	Result:=StartValue + (Delta * Progress * Sqr(Progress));
end;

{** TAQ **}

function TAQ.Append(Objects:TObjectArray):TAQ;
var
	cc:Integer;
begin
	Result:=Self;
	for cc:=0 to Length(Objects) - 1 do
		Add(Objects[cc]);
end;

function TAQ.Append(Objects:TObjectList):TAQ;
var
	cc:Integer;
begin
	Result:=Self;
	{**
	 * Overflows vermeiden
	 *}
	if Objects = Self then
		Exit;
	for cc:=0 to Objects.Count - 1 do
		Add(Objects[cc]);
end;

function TAQ.Append(AObject:TObject):TAQ;
begin
	Result:=Self;
	Add(AObject);
end;

function TAQ.AppendAQ(AQ:TAQ):TAQ;
begin
	Result:=Self;
	Add(AQ);
end;

procedure TAQ.AddInterval(Interval:TInterval);
begin
	if (GetIntervals.Count = 0) and (FActiveIntervalAQs.IndexOf(Self) = -1) then
		FActiveIntervalAQs.Add(Self);
	GetIntervals.Add(Interval);
	{$IFDEF OutputDebugActiveIntervals}
		OutputDebugString(PWideChar('TAQ-Instanzen mit Intervallen: ' +
			IntToStr(FActiveIntervalAQs.Count)));
	{$ENDIF}
end;

procedure TAQ.Animate(Duration:Integer; Each, LastEach:TEachFunction);
var
	CustomLastEach:TEachFunction;
begin
	CustomLastEach:=function(AQ:TAQ; O:TObject):Boolean
	begin
		Result:=TRUE;
		AQ.FAnimating:=FALSE;
		if Assigned(LastEach) then
			LastEach(AQ, O)
		else
			Each(AQ, O);
	end;
	EachTimer(Duration, Each, CustomLastEach).FAnimating:=TRUE;
end;


function TAQ.AnimationActors(IncludeOrphans:Boolean):TAQ;
var
	Actors:TAQ;
begin
	Actors:=Managed;

	Each(
		{**
		 * @param SAQ Synonym für SourceAccessQuery und ist Self von TimerActors
		 * @param SO Synonym für SourceObject und beinhaltet das Objekt für das die passenden
		 *        TAQ-Instanzen gesucht werden
		 *}
		function(SAQ:TAQ; SO:TObject):Boolean
		var
			SOFound:Boolean;
		begin
			Result:=TRUE; // Each soll stets komplett durchlaufen
			SOFound:=FALSE;
			GarbageCollector.Each(
				{**
				 * @param AQ Enthält den GarbageCollector
				 * @param O Enthält eine TAQ-Instanz, die darauf untersucht wird, ob sie SO enthält
				 *        einen aktiven Timer hat und gerade animiert wird
				 *}
				function(AQ:TAQ; O:TObject):Boolean
				begin
					Result:=TRUE; // Each soll stets komplett durchlaufen
					with TAQ(O) do
						if Contains(SO) and Animating then
						begin
							Actors.Add(O);
							SOFound:=TRUE;
						end;
				end);
			if IncludeOrphans and not SOFound then
				Actors.Add(SO);
		end);

	Result:=Actors;
end;

function TAQ.BoundsAnimation(NewLeft, NewTop, NewWidth, NewHeight:Integer; Duration:Integer;
	EaseFunction:TEaseFunction; OnComplete:TAnonymNotifyEvent):TAQ;
var
	WholeEach:TEachFunction;
begin
	with Self.Filter(TControl) do
	begin
		Result:=Multiplex;
		Die; // Die gefilterte TAQ sterben lassen
	end;

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

			AniLeft:=Ceil(Ease(EaseFunction)(PrevLeft, NewLeft, Progress));
			AniTop:=Ceil(Ease(EaseFunction)(PrevTop, NewTop, Progress));
			if NewWidth >= 0 then
				AniWidth:=Ceil(Ease(EaseFunction)(PrevWidth, NewWidth, Progress))
			else
				AniWidth:=TControl(O).Width;
			if NewHeight >= 0 then
				AniHeight:=Ceil(Ease(EaseFunction)(PrevHeight, NewHeight, Progress))
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

		AQ.Animate(Duration, EachF);
	end;

	Result.Each(WholeEach);
end;


function TAQ.CancelAnimations(Local:Boolean):TAQ;
begin
	Result:=Self;
	CustomCancel(Local, ctAnimation);
end;

function TAQ.CancelDelays(Local:Boolean):TAQ;
begin
	Result:=CancelTimers(Local);
end;

function TAQ.CancelIntervals(Local:Boolean):TAQ;
begin
	Result:=Self;
	CustomCancel(Local, ctInterval);
end;

function TAQ.CancelTimers(Local:Boolean):TAQ;
begin
	Result:=Self;
	CustomCancel(Local, ctTimer);
end;

function TAQ.Children(Append, Recurse:Boolean; ChildrenFiller:TEachFunction):TAQ;
begin
	if not Assigned(ChildrenFiller) then
		ChildrenFiller:=function(AQ:TAQ; O:TObject):Boolean
		var
			cc:Integer;
		begin
			Result:=TRUE;
			if not (O is TComponent) then
				Exit;
			with TComponent(O) do
				for cc:=0 to ComponentCount - 1 do
					AQ.Add(Components[cc]);
		end;

	Result:=CustomFiller(ChildrenFiller, Append, Recurse);
end;

procedure TAQ.Clean;
begin
	Clear;
	FAnimating:=FALSE;
	FCurrentInterval:=nil;
	if Assigned(FIntervals) then
	begin
		ClearIntervals;
		FreeAndNil(FIntervals);
	end;
end;

procedure TAQ.ClearIntervals;
var
	cc:Integer;
begin
	for cc:=FIntervals.Count - 1 downto 0 do
		RemoveInterval(TInterval(FIntervals[cc]));
end;

function TAQ.Contains(AObject:TObject):Boolean;
var
	Found:Boolean;
begin
	Found:=FALSE;
	Each(
		function(AQ:TAQ; O:TObject):Boolean
		begin
			Found:=Found or (O = AObject) or (AQ.IndexOf(AObject) >= 0);
			Result:=not Found;
		end);
	Result:=Found;
end;

constructor TAQ.Create;
begin
	inherited Create(FALSE);
	FRecurse:=TRUE;
end;


procedure TAQ.CustomCancel(Local:Boolean; CancelType:TCancelType);
type
	{**
	 * Die Funktion muss TRUE liefern, wenn ein Interval abgebrochen werden soll
	 *}
	TIntervalCancelFunction = reference to function(I:TInterval):Boolean;
var
	CancelF:TIntervalCancelFunction;
	Finish, CheckForAnimation:Boolean;
	Perform:TEachFunction;
begin
	CancelF:=nil;

	if CancelType = ctInterval then
		CancelF:=function(I:TInterval):Boolean
		begin
			Result:=not I.IsFinite;
		end
	else if CancelType in [ctTimer, ctTimerFinish, ctAnimation, ctAnimationFinish] then
		CancelF:=function(I:TInterval):Boolean
		begin
			Result:=I.IsFinite;
		end
	else
		raise EAQ.Create('No CancelF-Function defined.');

	CheckForAnimation:=CancelType in [ctAnimation, ctAnimationFinish];
	Finish:=CancelType in [ctTimerFinish, ctAnimationFinish];

	Perform:=function(AQ:TAQ; O:TObject):Boolean
	var
		cc:Integer;
		TempAQ:TAQ;
		CI:TInterval; // Abkürzung für CurrentInterval
	begin
		Result:=TRUE; // Each soll komplett durchlaufen
		if not (O is TAQ) then
			Exit;
		TempAQ:=TAQ(O);
		if not Assigned(TempAQ.FIntervals) then
			Exit;

		for cc:=TempAQ.FIntervals.Count - 1 downto 0 do
		begin
			CI:=TInterval(TempAQ.FIntervals[cc]);

			if CancelF(CI) and
				(not CheckForAnimation or (CheckForAnimation and TempAQ.Animating)) then
			begin
				if Finish then
					CI.Finish
				else
					CI.Cancel;

				TempAQ.ProcessInterval(CI);
			end;
		end;
	end;

	if Local then
	begin
		{**
		 * Ggf. unterordnete TAQ-Instanzen
		 *}
		Each(Perform);
		{**
		 * Die aktuelle Instanz selbst kommt per Each nicht an
		 *}
		Perform(Self, Self);
	end
	{**
	 * Globale Suche
	 *
	 * Es sollen TAQ-Instanzen gesucht werden, die mind. ein Objekt von dieser TAQ-Instanz
	 * beherbergen.
	 *}
	else
	begin
		if not Assigned(FActiveIntervalAQs) then
			Exit;
		{**
		 * Der GarbageCollector muss kurz raus, da er hierfür nicht relevant ist und wird im
		 * Anschluss wieder hinzugefügt
		 *}
		FActiveIntervalAQs.Remove(GarbageCollector);
		{**
		 * Abbrüche und Beendigungen können sich nur an aktive TAQ-Instanzen richten
		 *}
		FActiveIntervalAQs
			.Each(
				function(GC:TAQ; Target:TObject):Boolean
				var
					TargetAQ:TAQ;
				begin
					Result:=TRUE; // Each soll komplett durchlaufen
					TargetAQ:=TAQ(Target);
					Each(
						{**
						 * @param AQ Enthält Self aus dem selben Kontext, wie die CustomCancel-Methode
						 * @param O Enthält ein Object aus aktuellem Self
						 *}
						function(AQ:TAQ; O:TObject):Boolean
						begin
							Result:=TRUE;
							if O is TAQ then
								Exit;
							if TargetAQ.IndexOf(O) >= 0 then
							begin
								TargetAQ.Each(Perform);
								Perform(TargetAQ, TargetAQ);
								Result:=FALSE; // Es wird nur ein Objekt-Vorkommen benötigt
							end;
						end);
				end)
			.Add(GarbageCollector);
	end;
end;

function TAQ.CustomFiller(Filler:TEachFunction; Append, Recurse:Boolean):TAQ;
var
	TargetAQ:TAQ;

	procedure Each(SourceAQ:TAQ);
	var
		TempAQ:TAQ;
	begin
		TempAQ:=Unmanaged;
		try
			SourceAQ.Each(
				function(AQ:TAQ; O:TObject):Boolean
				begin
					Result:=Filler(TempAQ, O);
				end);

			if TempAQ.Count = 0 then
				Exit;

			TargetAQ.Append(TempAQ);

			if Recurse then
				Each(TempAQ);
		finally
			TempAQ.Free;
		end;
	end;
begin
	if Append then
		TargetAQ:=Self
	else
		TargetAQ:=Managed;
	Each(Self);
	Result:=TargetAQ;
end;

function TAQ.Demultiplex:TAQ;
var
	SimpleAQ:TAQ;
begin
	SimpleAQ:=Managed;
	Result:=SimpleAQ;
	Each(
		function(AQ:TAQ; O:TObject):Boolean
		begin
			Result:=TRUE;
			if O is TAQ then
				Exit;
			SimpleAQ.Add(O);
		end);
end;

destructor TAQ.Destroy;
begin
	if Assigned(FIntervals) then
	begin
		ClearIntervals;
		FIntervals.Free;
	end;
	inherited Destroy;
end;

function TAQ.Die:TAQ;
begin
	Result:=Self;
	if Assigned(FIntervals) and (FIntervals.Count > 0) then
		Exit;
	FLifeTick:=0;
end;

function TAQ.Each(EachFunction:TEachFunction):TAQ;
var
	cc:Integer;
	O:TObject;
begin
	Result:=Self;
	cc:=Count;
	while cc > 0 do
	begin
		O:=Items[Count - cc];
		if Recurse and (O is TAQ) then
			TAQ(O).Each(EachFunction);
		if not EachFunction(Self, O) then
			Break
		else if cc > Count then
			cc:=Count;
		Dec(cc);
	end;
end;

function TAQ.EachDelay(Delay:Integer; Each:TEachFunction):TAQ;
begin
	if Delay >= MaxLifeTime then
		raise EAQ.CreateFmt('Delay (%d) muss kleiner als MaxLifeTime (%d) sein.',
			[Delay, MaxLifeTime]);
	Result:=EachTimer(Delay, nil, Each);
end;

function TAQ.EachInterval(Interval:Integer; Each:TEachFunction):TAQ;
begin
	if Interval >= MaxLifeTime then
		raise EAQ.CreateFmt('Interval (%d) muss kleiner als MaxLifeTime (%d) sein.',
			[Interval, MaxLifeTime]);
	Result:=Self;
	AddInterval(TInterval.Infinite(Interval, Each));
end;

function TAQ.EachRepeat(Times:Integer; EachFunction:TEachFunction):TAQ;
var
	cc:Integer;
begin
	Result:=Self;
	for cc:=1 to Times do
		Each(EachFunction);
end;

function TAQ.EachTimer(Duration:Integer; Each, LastEach:TEachFunction):TAQ;
begin
	if Duration >= MaxLifeTime then
		raise EAQ.CreateFmt('Dauer des Timers (%d) muss kleiner als MaxLifeTime (%d) sein.',
			[Duration, MaxLifeTime]);
	Result:=Self;
	AddInterval(TInterval.Finite(Duration, Each, LastEach));
end;

class function TAQ.Ease(EaseFunction:TEaseFunction):TEaseFunction;
begin
	if Assigned(EaseFunction) then
		Result:=EaseFunction
	else
		Result:=LinearEase;
end;

class function TAQ.Ease(EaseType:TEaseType):TEaseFunction;
begin
	case EaseType of
		etQuadratic:
			Result:=QuadraticEase;
		etMassiveQuadratic:
			Result:=MassiveQuadraticEase;
	else
		Result:=LinearEase;
	end;
end;

function TAQ.Filter(ByClass:TClass):TAQ;
var
	NewAQ:TAQ;
begin
	NewAQ:=Managed;

	Each(
		function(AQ:TAQ; O:TObject):Boolean
		begin
			Result:=TRUE;
			if O is ByClass then
				NewAQ.Add(O);
		end);

	Result:=NewAQ;
end;

function TAQ.Filter(FilterEach:TEachFunction):TAQ;
var
	NewAQ:TAQ;
begin
	NewAQ:=Managed;
	Each(
		function(OAQ:TAQ; OO:TObject):Boolean
		begin
			Result:=TRUE;
			if FilterEach(OAQ, OO) then
				NewAQ.Add(OO);
		end);
	Result:=NewAQ;
end;

function TAQ.FinishAnimations(Local:Boolean):TAQ;
begin
	Result:=Self;
	CustomCancel(Local, ctAnimationFinish);
end;

function TAQ.FinishTimers(Local:Boolean):TAQ;
begin
	Result:=Self;
	CustomCancel(Local, ctTimerFinish);
end;

function TAQ.First:TAQ;
begin
	Result:=Managed;
	if Count > 0 then
		Result.Append(Items[0]);
end;

class function TAQ.GarbageCollector:TAQ;
begin
	if Assigned(FGarbageCollector) then
		Exit(FGarbageCollector);

	{**
	 * Ab hier fängt die Instanzierung der gesamten Klasse an
	 *}

	FIntervalTimer:=TTimer.Create(nil);
	with FIntervalTimer do
	begin
		Enabled:=TRUE;
		Interval:=IntervalResolution;
		OnTimer:=GlobalIntervalTimerEvent;
	end;

	FActiveIntervalAQs:=TAQ.Create;
	FActiveIntervalAQs.FRecurse:=FALSE;

	FGarbageCollector:=TAQ.Create;
	FGarbageCollector.OwnsObjects:=TRUE;
	FGarbageCollector.FRecurse:=FALSE;
	FGarbageCollector.EachInterval(GarbageCleanInterval,
		{**
		 * In GarbageCollector befindet sich der FGarbageCollector selbst und
		 * in O eine TAQ-Instanz die auf ihre Lebenszeichen untersucht werden muss.
		 *}
		function(GarbageCollector:TAQ; O:TObject):Boolean
		var
			CheckAQ:TAQ;
		begin
			CheckAQ:=TAQ(O);
			if not CheckAQ.IsAlive then
			begin
				GarbageCollector.Remove(CheckAQ);

				{$IFDEF OutputDebugGCFree}
				OutputDebugString(PWideChar(Format('TAQ freigegeben. Verbleibend im GarbageCollector: %d.',
					[GarbageCollector.Count])));
				{$ENDIF}
			end;
			Result:=TRUE;
		end);

	Result:=FGarbageCollector;
end;

function TAQ.GetIntervals:TObjectList;
begin
	if not Assigned(FIntervals) then
		FIntervals:=TObjectList.Create(TRUE);
	Result:=FIntervals;
end;

class procedure TAQ.GlobalIntervalTimerEvent(Sender:TObject);
begin
	FActiveIntervalAQs.Each(
		{**
		 * @param AQ Enthält FActiveIntervalAQs
		 * @param O Enthält ein TAQ-Objekt, welches mind. ein Interval besitzt
		 *}
		function(AQ:TAQ; O:TObject):Boolean
		begin
			TAQ(O).LocalIntervalTimerEvent(Sender);
			Result:=TRUE; // Die Each soll komplett durchlaufen
		end);
end;

function TAQ.HasInterval:Boolean;
var
	AnyIntervals:Boolean;
begin
	Result:=FALSE;
	if not (Assigned(FIntervals) and (FIntervals.Count > 0)) then
		Exit;
	AnyIntervals:=FALSE;
	TAQ.Take(FIntervals)
		.Each(
			function(AQ:TAQ; O:TObject):Boolean
			begin
				AnyIntervals:=AnyIntervals or (not TInterval(O).IsFinite);
				{**
				 * Die Each soll nur laufen, bis das erste Interval gefunden wird
				 *}
				Result:=not AnyIntervals;
			end)
		.Die;
	Result:=AnyIntervals;
end;

function TAQ.HasTimer:Boolean;
var
	AnyTimers:Boolean;
begin
	Result:=FALSE;
	if not (Assigned(FIntervals) and (FIntervals.Count > 0)) then
		Exit;
	AnyTimers:=FALSE;
	TAQ.Take(FIntervals)
		.Each(
			function(AQ:TAQ; O:TObject):Boolean
			begin
				with TInterval(O) do
					AnyTimers:=IsFinite and not IsFinished;
				{**
				 * Die Each soll nur laufen, bis der erste Timer gefunden wird
				 *}
				Result:=not AnyTimers;
			end)
		.Die;
	Result:=AnyTimers;
end;

procedure TAQ.HeartBeat;
	{**
	 * Leitet den Herzschlag an ggf. enthaltene TAQ-Instanzen rekursiv weiter
	 *}
	procedure HeartBeatEcho(AQ:TAQ);
	var
		cc:Integer;
	begin
		for cc:=0 to AQ.Count - 1 do
			if AQ[cc] is TAQ then
			begin
				TAQ(AQ[cc]).HeartBeat;
				HeartBeatEcho(TAQ(AQ[cc]));
			end;
	end;
begin
	FLifeTick:=GetTickCount;
	{**
	 * Der Herzschlag muss an enthaltene TAQ-Instanzen weitergereicht werden, wenn diese Instanz
	 * Rekursiv (TAQ.Recurse) ist. Standardmäßig sind alle TAQ-Instanzen rekursiv.
	 * Ausnahmen: TAQ.FGarbageCollector
	 *}
//	if Recurse then
//		HeartBeatEcho(Self);
end;

function TAQ.IntervalActors(IncludeOrphans:Boolean):TAQ;
var
	Actors:TAQ;
begin
	Actors:=Managed;

	Each(
		{**
		 * @param SAQ Synonym für SourceAccessQuery und ist Self von TimerActors
		 * @param SO Synonym für SourceObject und beinhaltet das Objekt für das die passenden
		 *        TAQ-Instanzen gesucht werden
		 *}
		function(SAQ:TAQ; SO:TObject):Boolean
		var
			SOFound:Boolean;
		begin
			Result:=TRUE; // Each soll stets komplett durchlaufen
			SOFound:=FALSE;
			GarbageCollector.Each(
				{**
				 * @param AQ Enthält den GarbageCollector
				 * @param O Enthält eine TAQ-Instanz, die darauf untersucht wird, ob sie SO enthält
				 *        und einen aktiven Timer hat
				 *}
				function(AQ:TAQ; O:TObject):Boolean
				begin
					Result:=TRUE; // Each soll stets komplett durchlaufen
					with TAQ(O) do
						if Contains(SO) and HasInterval then
						begin
							Actors.Add(O);
							SOFound:=TRUE;
						end;
				end);
			if IncludeOrphans and not SOFound then
				Actors.Add(SO);
		end);

	Result:=Actors;
end;

function TAQ.IsAlive:Boolean;
begin
	Result:=(FLifeTick + MaxLifeTime) > GetTickCount;
end;

function TAQ.Last:TAQ;
begin
	Result:=Managed;
	if Count > 0 then
		Result.Append(Items[Count - 1]);
end;

procedure TAQ.LocalIntervalTimerEvent(Sender:TObject);
var
	cc:Integer;
begin
	if not (Assigned(FIntervals) and (FIntervals.Count > 0)) then
		Exit;

	for cc:=FIntervals.Count - 1 downto 0 do
		ProcessInterval(TInterval(FIntervals[cc]));

	HeartBeat;
end;

class function TAQ.Managed:TAQ;
var
	ManagedAQ:TAQ;
begin
	ManagedAQ:=nil;

	{**
	 * Im GarbageCollector nach einer abgelaufenen Instanz suchen
	 *}
	GarbageCollector.Each(
		function(AQ:TAQ; O:TObject):Boolean
		var
			CheckAQ:TAQ;
		begin
			CheckAQ:=TAQ(O);
			if not CheckAQ.IsAlive then
			begin
				ManagedAQ:=CheckAQ;
				ManagedAQ.Clean;
				ManagedAQ.HeartBeat;
				{$IFDEF OutputDebugGCRecycle}
				OutputDebugString(PWideChar(Format('TAQ %p am Index #%d wiederverwendet.',
					[@O, AQ.IndexOf(O)])));
				{$ENDIF}
			end;
			{**
			 * Die Each soll solange laufen, bis eine abgelaufene Instanz gefunden wurde
			 *}
			Result:=not Assigned(ManagedAQ);
		end);

	if Assigned(ManagedAQ) then
		Exit(ManagedAQ);

	Result:=TAQ.Create;
	Result.HeartBeat;
	{**
	 * Das ist das ganze Geheimnis des TAQ-Objekt-Managing ;)
	 *}
	GarbageCollector.Add(Result);
	{$IFDEF OutputDebugGCCreate}
	OutputDebugString(PWideChar(Format('Neuer TAQ %p am Index #%d.',
		[@Result, GarbageCollector.IndexOf(Result)])));
	{$ENDIF}
end;

function TAQ.Multiplex:TAQ;
var
	MultiAQ:TAQ;
begin
	MultiAQ:=Managed;
	Result:=MultiAQ;
	Each(
		function(AQ:TAQ; O:TObject):Boolean
		begin
			Result:=TRUE;
			if O is TAQ then
				Exit;
			MultiAQ.AppendAQ(TAQ.Take(O));
		end);
end;

function TAQ.Parents(Append, Recurse:Boolean; ParentsFiller:TEachFunction):TAQ;
begin
	if not Assigned(ParentsFiller) then
		ParentsFiller:=function(AQ:TAQ; O:TObject):Boolean
		begin
			Result:=TRUE;
			if not ((O is TComponent) and (TComponent(O).HasParent)) then
				Exit;
			AQ.Add(TComponent(O).GetParentComponent);
		end;
	Result:=CustomFiller(ParentsFiller, Append, Recurse);
end;

function TAQ.ProcessInterval(Interval:TInterval):Boolean;
var
	EachFunction:TEachFunction;
begin
	Result:=TRUE;
	FCurrentInterval:=Interval;

	EachFunction:=(CurrentInterval.Each);
	if Assigned(EachFunction) then
		Each(EachFunction);

	if CurrentInterval.IsFinished then
	begin
		RemoveInterval(CurrentInterval);
		Result:=FALSE;
		FCurrentInterval:=nil;
	end;
end;

procedure TAQ.RemoveInterval(Interval:TInterval);
begin
	FIntervals.Remove(Interval);
	if FIntervals.Count = 0 then
		FActiveIntervalAQs.Remove(Self);
	{$IFDEF OutputDebugActiveIntervals}
		OutputDebugString(PWideChar('TAQ-Instanzen mit Intervallen: ' +
			IntToStr(FActiveIntervalAQs.Count)));
	{$ENDIF}
end;

function TAQ.ShakeAnimation(XTimes, XDiff, YTimes, YDiff, Duration:Integer;
	OnComplete:TAnonymNotifyEvent):TAQ;
var
	WholeEach:TEachFunction;
begin
	with Self.Filter(TControl) do
	begin
		Result:=Multiplex;
		Die; // Die gefilterte TAQ sterben lassen
	end;

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

			function Swing(Times, Diff:Integer; Progress:Real):Integer;
			begin
				Result:=Ceil(Diff * Sin(Progress * Times * Pi * 2));
			end;
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

		AQ.Animate(Duration, EachF);
	end;

	Result.Each(WholeEach);
end;

class function TAQ.Take(Objects:TObjectArray):TAQ;
begin
	Result:=Managed.Append(Objects);
end;

class function TAQ.Take(AObject:TObject):TAQ;
begin
	Result:=Managed.Append(AObject);
end;

function TAQ.TimerActors(IncludeOrphans:Boolean):TAQ;
var
	Actors:TAQ;
begin
	Actors:=Managed;
	Result:=Actors;

	Each(
		{**
		 * @param SAQ Synonym für SourceAccessQuery und ist Self von TimerActors
		 * @param SO Synonym für SourceObject und beinhaltet das Objekt für das die passenden
		 *        TAQ-Instanzen gesucht werden
		 *}
		function(SAQ:TAQ; SO:TObject):Boolean
		var
			SOFound:Boolean;
		begin
			Result:=TRUE; // Each soll stets komplett durchlaufen
			SOFound:=FALSE;
			GarbageCollector.Each(
				{**
				 * @param AQ Enthält den GarbageCollector
				 * @param O Enthält eine TAQ-Instanz, die darauf untersucht wird, ob sie SO enthält
				 *        und einen aktiven Timer hat
				 *}
				function(AQ:TAQ; O:TObject):Boolean
				begin
					Result:=TRUE; // Each soll stets komplett durchlaufen
					with TAQ(O) do
						if Contains(SO) and HasTimer then
						begin
							Actors.Add(O);
							SOFound:=TRUE;
						end;
				end);
			if IncludeOrphans and not SOFound then
				Actors.Add(SO);
		end);
end;

class function TAQ.Unmanaged:TAQ;
begin
	Result:=TAQ.Create;
end;

class function TAQ.Take(Objects:TObjectList):TAQ;
begin
	Result:=Managed.Append(Objects);
end;

{** TInterval **}

procedure TInterval.Cancel;
begin
	FInterval:=0;
end;

function TInterval.Each:TEachFunction;
var
	CurrentTick:Cardinal;
begin
	Result:=nil;
	if IsCanceled then
		Exit;
	CurrentTick:=GetTickCount;
	{**
	 * Unendlicher Interval
	 *}
	if (FLastTick = 0) and (CurrentTick >= FNextTick) then
	begin
		Result:=FNextEach;
		UpdateNextTick;
	end
	{**
	 * Endlicher Interval
	 *}
	else if (FLastTick > 0) then
	begin
		if CurrentTick >= FLastTick then
			if Assigned(FLastEach) then
				Result:=FLastEach
			else
				Result:=FNextEach
		else if CurrentTick >= FNextTick then
		begin
			Result:=FNextEach;
			UpdateNextTick;
		end;
	end;
end;

procedure TInterval.Finish;
begin
	if IsFinite and not IsFinished then
		FLastTick:=GetTickCount;
end;

constructor TInterval.Finite(Duration:Integer; Each, LastEach:TEachFunction);
begin
	FNextEach:=Each;
	FLastEach:=LastEach;

	FFirstTick:=GetTickCount;
	FInterval:=Max(IntervalResolution, Ceil(Duration / IntervalResolution));

	FLastTick:=FFirstTick + Cardinal(Duration);
	UpdateNextTick;
end;

constructor TInterval.Infinite(Interval:Integer; Each:TEachFunction);
begin
	FFirstTick:=GetTickCount;
	FNextEach:=Each;
	FLastEach:=nil;
	FInterval:=Max(IntervalResolution, Interval);
	FLastTick:=0;
	UpdateNextTick;
end;

function TInterval.IsCanceled:Boolean;
begin
	Result:=FInterval = 0;
end;

function TInterval.IsFinished:Boolean;
begin
	Result:=((FLastTick > 0) and (GetTickCount >= FLastTick)) or IsCanceled;
end;

function TInterval.IsFinite:Boolean;
begin
	Result:=FLastTick > 0;
end;

function TInterval.Progress:Real;
begin
	Result:=Min(1, (GetTickCount - FFirstTick) / Max(1, (FLastTick - FFirstTick)));
end;

procedure TInterval.UpdateNextTick;
begin
	FNextTick:=GetTickCount + Cardinal(FInterval);
end;

initialization

finalization

{**
 * Freigabe von allem, was in der Klassenmethode TAQ.GarbageCollector instanziert wurde
 *}
if Assigned(TAQ.FGarbageCollector) then
begin
	{**
	 * Alle offenen TAQ-Instanzen freigeben
	 *}
	TAQ.FGarbageCollector.Free;
	{**
	 * Dieser Timer wird zusammen mit FGarbageCollector erstellt, muss auch dementsprechend zusammen
	 * freigegeben werden.
	 *}
	TAQ.FIntervalTimer.Free;
	{**
	 * Diese unverwaltete TAQ-Instanz wird ebenfalls mit dem FGarbageCollector erstellt und muss
	 * hier manuell freigegeben werden.
	 *}
	TAQ.FActiveIntervalAQs.Free;
end;

end.
