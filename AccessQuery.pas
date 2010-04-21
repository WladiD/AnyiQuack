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
	{**
	 * Dynamisches Array für Objekte
	 *}
	TObjectArray = array of TObject;
	{**
	 * Typ für anonyme Each-Funktionen
	 *
	 * @param AQ Die TAQ-Instanz, die die Each-Funktion aufruft
	 * @param O Das Objekt, welches durch die Each-Funktion bearbeitet werden soll
	 * @return Boolean Sobald die Funktion FALSE liefert, wird die Ausführung der Each-Methode
	 *         angehalten
	 *}
	TEachFunction = reference to function(AQ:TAQ; O:TObject):Boolean;
	{**
	 * Typ für anonyme Benachrichtigungs-Events, der aber auch mit TNotifyEvent kompatibel ist
	 *}
	TAnonymNotifyEvent = reference to procedure(Sender:TObject);
	{**
	 * Typ für Beschleunigungsfunktionen
	 *
	 * @param StartValue Der Wert, ab dem gestartet wird
	 * @param EndValue Der Endwert, der erreicht werden sollte, wenn Progress = 1 ist
	 * @param Progress Fortschritt der Beschleunigung. Wert zwischen 0..1
	 * @return Real
	 *}
	TEaseFunction = function(StartValue, EndValue, Progress:Real):Real;
	{**
	 * Vordefinierte Arten von Beschleunigungsfunktionen
	 *}
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
	 * verwaltet und sich (unter anderem) um dessen freigabe kümmert. Jede TAQ-Instanz hat eine
	 * begrenzte Lebensdauer, die von der Konstante MaxLifeTime bestimmt wird, wird es nun für die
	 * definierte Zeitspanne inaktiv, wird es freigegeben oder wiederverwendet.
	 *
	 * Ungemanagete TAQ-Instanzen
	 * --------------------------
	 * Eine TAQ-Instanz gilt als nicht verwaltet, wenn sie über den TAQ-Konstruktor oder mittels
	 * der Klassenmethode TAQ.Unmanaged erzeugt wird. Diese werden nicht automatisch freigegeben.
	 * Auch können solche Instanzen nicht von den Methoden gebrauch nehmen, die in irgendeiner Art
	 * auf der Basis von Timern realisiert werden, wie z.B. TAQ.EachDelay, TAQ.EachTimer,
	 * TAQ.XAnimation etc...
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
		{**
		 * Nimmt alle gemanagete TAQ-Instanzen auf
		 *
		 * Hierrüber wird, unter anderem, die Freigabe von nicht mehr verwendeten TAQ-Objekten
		 * abgewickelt.
		 *
		 * Wird von der Klassenmethode GarbageCollector instanziert
		 *
		 * @see TAQ.GarbageCollector
		 * @see TAQ.Managed
		 *}
		FGarbageCollector:TAQ;
		{**
		 * Der globale Timer
		 *
		 * Wird von der Klassenmethode GarbageCollector instanziert
		 *
		 * @see TAQ.GarbageCollector
		 *}
		FIntervalTimer:TTimer;
		{**
		 * Nimmt alle gemanagete TAQ-Instanzen auf, die mind. ein Interval verwenden
		 *
		 * Wird von der Klassenmethode GarbageCollector instanziert.
		 *
		 * @see TAQ.GarbageCollector
		 * @see TAQ.UpdateActiveIntervalAQs
		 *}
		FActiveIntervalAQs:TAQ;
	protected
		type
			TCancelType = (ctInterval, ctTimer, ctTimerFinish, ctAnimation, ctAnimationFinish);
		var
		{**
		 * Der letzte Tick, der als Lebenszeichen ausgewertet wird
		 *
		 * @see MaxLifeTime
		 * @see TAQ.HeartBeat
		 *}
		FLifeTick:Cardinal;
		{**
		 * Nimmt mehrere TInterval-Objekte auf
		 *
		 * Wird von GetIntervals instanziert.
		 *
		 * @see TInterval
		 * @see TAQ.GetIntervals
		 *}
		FIntervals:TObjectList;
		{**
		 * @see TAQ.CurrentInterval
		 *}
		FCurrentInterval:TInterval;
		{**
		 * @see TAQ.Animating
		 *}
		FAnimating:Boolean;
		{**
		 * @see TAQ.Recurse
		 *}
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

		{**
		 * Bestimmt, ob TAQ.Each ggf. andere enthaltene TAQ-Instanzen rekursiv mit der übergebenen
		 * Each-Funktion durchlaufen soll.
		 *
		 * Standard ist TRUE. Beim FGarbageCollector und FActiveIntervalAQs aber FALSE.
		 *}
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

		{**
		 * Diese Eigenschaft ist für jene TEachFunction-Funktionen gedacht, die aus dem Kontext der
		 * EachInterval-, EachDelay- und EachTimer-Methoden aufgerufen werden.
		 *
		 * Hierrüber kann man den Fortschritt TInterval.Progress abrufen oder es mittels
		 * TInterval.Cancel abbrechen.
		 *
		 * @see TInterval.Progress
		 * @see TInterval.Cancel
		 *}
		property CurrentInterval:TInterval read FCurrentInterval;
		{**
		 * Sagt aus, ob die aktuelle Instanz eine Animation durchführt. Sie wird nur von
		 * internen XAnimation-Methoden gesetzt. Manuelle Animationen mittels EachTimer können
		 * das hier nicht setzen.
		 *}
		property Animating:Boolean read FAnimating;
	end;

	TInterval = class
	private
		{**
		 * Der Tick, als der Interval begann
		 *}
		FFirstTick,
		{**
		 * Der Tick für die nächste Ausführung
		 *}
		FNextTick,
		{**
		 * Der Tick für die letzte Ausführung
		 *
		 * Ist 0, bei unendlichem Interval
		 *}
		FLastTick:Cardinal;
		{**
		 * Die Länge eines Intervals in msec
		 *
		 * Ist 0 bei abgebrochenem Interval
		 *
		 * @see TInterval.Cancel
		 * @see TInterval.IsCanceled
		 *}
		FInterval:Integer;
		{**
		 * Die Each-Funktion, die nicht unbedingt ausgeführt wird
		 *}
		FNextEach,
		{**
		 * Die letzte Each-Funktion
		 *
		 * Ist nil, bei unendlichem Interval
		 *}
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
	{**
	 * Die maximale Lebensdauer einer TAQ-Instanz bei Nichtbeschäftigung (msec)
	 *}
	MaxLifeTime = 10000;
	{**
	 * Der Interval basiert auf 40 msec (25fps = 1 sec)
	 *}
	IntervalResolution = 40;
	{**
	 * Interval für den Bereinigungsvorgang des GarbageCollector in Millisekunden
	 *}
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

{**
 * Fügt mehrere Objekte mittels eines Objekt-Arrays hinzu
 *
 * Fungiert als Shortcut, für die Hinzufügung mehrerer Objekte ohne ein TObjectList-Objekt.
 *
 * Beispiel:
 *
 * TAQ.Take(Form1).Append(TObjectArray.Create(Button1, Button2));
 *}
function TAQ.Append(Objects:TObjectArray):TAQ;
var
	cc:Integer;
begin
	Result:=Self;
	for cc:=0 to Length(Objects) - 1 do
		Add(Objects[cc]);
end;

{**
 * Fügt mehrere Objekte aus einer TObjectList-Instanz hinzu
 *}
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

{**
 * Fügt ein einzelnes Objekt hinzu
 *
 * Es ist eine Chain-Kompatible Add-Methode.
 *}
function TAQ.Append(AObject:TObject):TAQ;
begin
	Result:=Self;
	Add(AObject);
end;

{**
 * Fügt eine TAQ-Instanz als solche hinzu
 *
 * Während Append(TObjectList) mit einem TAQ-Objekt als Parameter dessen Objekte hinzufügen würde,
 * fügt diese Methode das TAQ-Objekt selbst hinzu
 *}
function TAQ.AppendAQ(AQ:TAQ):TAQ;
begin
	Result:=Self;
	Add(AQ);
end;

{**
 * Fügt ein Interval hinzu
 *}
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

{**
 * Verwaltungsoverhead für Animationen
 *}
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


{**
 * Liefert ein neues TAQ-Objekt mit TAQ-Instanzen, die gerade eine Animation, für die in dieser
 * Instanz enthaltene Objekte, durchführen.
 *
 * Da die Each-Methode rekursiv mit den enthaltenen TAQ-Objekten arbeitet, laufen weitere Aktionen
 * transparent ab.
 *
 * @param IncludeOrphans Optional. Standard ist TRUE. Bestimmt, ob die neue Instanz auch die Objekte
 *        in sich vereint, für die keine passende TAQ-Instanz gefunden wurde.
 *}
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

{**
 * Führt eine Positionierungsanimation mit den enthaltenen TControl-Objekten durch
 *
 * @param NewLeft Neue absolute Links-Position.
 * @param NewTop Neue absolute Oben-Position.
 * @param NewWidth Neue absolute Breite. Soll die Breite nicht verändert werden, ist -1 anzugeben.
 * @param NewHeight Neue absolute Höhe. Soll die Höhe nicht verändert werden, ist -1 anzugeben.
 * @param Duration Die Dauer der Animation in Millisekunden.
 * @param EaseFunction Optional. Eine Beschleunigungsfunktion. Siehe TAQ.Ease, um vordefinierte
 *        Beschleunigungsfunktion zu verwenden. Wird keine (nil) angegeben, so kommt eine lineare
 *        Beschleunigungsfunktion zum Einsatz.
 * @param OnComplete Stanadrd ist nil. Event-Handler, der ausgelöst wird, wenn die Animation
 *        beendet ist. Das Ereignis wird übrigens für jedes Objekt, das animiert wird, ausgelöst.
 *}
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


{**
 * Bricht die laufenden Animationen ab
 *
 * Im Gegensatz zu TAQ.FinishAnimations werden die Animationen auf der aktuellen Position
 * angehalten.
 *
 * @see TAQ.FinishAnimations
 *
 * @param Local Optional. Standard ist FALSE. Bestimmt, ob alle (FALSE) gemanageten TAQ-Instanzen
 *        auf die hier enthaltenen Objekte untersucht werden sollen. Wird TRUE übergeben, so bezieht
 *        sich die Operation nur auf die aktuelle Instanz.
 *}
function TAQ.CancelAnimations(Local:Boolean):TAQ;
begin
	Result:=Self;
	CustomCancel(Local, ctAnimation);
end;

{**
 * Bricht alle, mittels EachDelay erstellte Verzögerungen ab
 *
 * Zur Zeit ist es lediglich ein Wrapper zu TAQ.CancelTimers
 *
 * @param Local Optional. Standard ist FALSE. Bestimmt, ob alle (FALSE) gemanageten TAQ-Instanzen
 *        auf die hier enthaltenen Objekte untersucht werden sollen. Wird TRUE übergeben, so bezieht
 *        sich die Operation nur auf die aktuelle Instanz.
 *}
function TAQ.CancelDelays(Local:Boolean):TAQ;
begin
	Result:=CancelTimers(Local);
end;

{**
 * Bricht alle, mittels EachInterval erstellte, Intervale ab
 *
 * @param Local Optional. Standard ist FALSE. Bestimmt, ob alle (FALSE) gemanageten TAQ-Instanzen
 *        auf die hier enthaltenen Objekte untersucht werden sollen. Wird TRUE übergeben, so bezieht
 *        sich die Operation nur auf die aktuelle Instanz.
 *}
function TAQ.CancelIntervals(Local:Boolean):TAQ;
begin
	Result:=Self;
	CustomCancel(Local, ctInterval);
end;

{**
 * Bricht aktive Timer ab
 *
 * Die Timer können dadurch !nicht! die letzte Each-Funktion ausführen. Siehe TAQ.FinishTimers.
 *
 * @see TAQ.FinishTimers
 *
 * @param Local Optional. Standard ist FALSE. Bestimmt, ob alle (FALSE) gemanageten TAQ-Instanzen
 *        auf die hier enthaltenen Objekte untersucht werden sollen. Wird TRUE übergeben, so bezieht
 *        sich die Operation nur auf die aktuelle Instanz.
 *}
function TAQ.CancelTimers(Local:Boolean):TAQ;
begin
	Result:=Self;
	CustomCancel(Local, ctTimer);
end;

{**
 * Ermittelt die Kindsobjekte für die enthaltenen Objekte
 *
 * @param Append Optional. Standard ist FALSE. Bestimmt, ob die Kindsobjekte der aktuellen
 *        TAQ-Instanz hinzugefügt werden sollen (TRUE) oder ob eine neue Instanz erstellt
 *        werden soll (FALSE), die zurückgeliefert wird.
 * @param Recurse Optional. Standard ist FALSE. Bestimmt, ob die ermittelten Kindsobjekte wiederrum
 *        auf Kindsobjekte untersucht werden sollen.
 * @param ChildrenFiller Optional. Standard ist nil. Wie der Name schon vermuten lässt, hat die
 *        Funktion die Aufgabe, dem übergebenen TAQ-Objekt die Kindsobjekte hinzuzufügen. Wird der
 *        Parameter nicht angegeben (nil), so wird eine interne Funktion verwendet, die nur
 *        von TComponent abgeleitete Objekte akzeptiert und deren Components-Eigenschaft
 *        berücksichtigt.
 *}
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

{**
 * Macht eine saubere Instanz
 *
 * Als wäre sie neu erstellt worden.
 * Die Lebenszeit wird verlängert.
 *}
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

{**
 * Entfernt alle vorhandenen Intervalle
 *}
procedure TAQ.ClearIntervals;
var
	cc:Integer;
begin
	for cc:=FIntervals.Count - 1 downto 0 do
		RemoveInterval(TInterval(FIntervals[cc]));
end;

{**
 * Sagt aus, ob das übergebene Objekt enthalten ist
 *
 * Diese Funktion arbeitet per TAQ.Each und berücksichtigt daher die Eigenschaft TAQ.Recurse,
 * das heisst: Wenn diese Instanz Recurse = TRUE ist, so wird rekursiv in allen ggf. hier
 * enthaltenen TAQ-Instanzen gesucht
 *
 *}
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

{**
 * Erstellt eine neue TAQ-Instanz mit allen Nicht-TAQ-Objekten
 *
 * Diverse Methoden liefern verschachtelte TAQ-Objekte, die wiederrum TAQ-Objekte enthalten können.
 * Viele Methoden können mit verschachtelten TAQ-Objekten problemlos und vorallem für den Anwender
 * transparent arbeiten, doch kann die Performance stark einbrechen, wenn damit intensive Aktionen
 * durchgeführt werden. Genau für solche Fälle existiert diese Methode, die wieder aus einem
 * komplexen ein einfaches TAQ-Objekt erstellt, mit dem Operationen deutlich performanter sind.
 *
 * @see TAQ.Multiplex
 *}
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

{**
 * Wichtig:
 * Gemeanagete TAQ-Instanzen sollten !nie! außerhalb freigegeben werden, das ist Aufgabe des
 * GarbageCollectors!
 *}
destructor TAQ.Destroy;
begin
	if Assigned(FIntervals) then
	begin
		ClearIntervals;
		FIntervals.Free;
	end;
	inherited Destroy;
end;

{**
 * Kennzeichnet die Instanz als tot
 *
 * TAQ.IsAlive würde beim nächsten Aufruf FALSE liefern. Wenn hingegen irgendwelche weitere
 * Aktionen mit den Each-Methoden erfolgen, wird die Lebenszeit wieder erhöht.
 *
 * Um Verwirrungen vorzubeugen:
 * - Die Die-Methode hat nur bei gemanageten Instanzen einen Effekt
 * - Das Objekt wird nicht sofort freigegeben, sondern erst beim nächsten Durchlauf des
 *   GarbageCollectors
 * - Die Verwendung dieser Methode ist absolut freiwillig, hat aber den positiven Effekt, dass
 *   die Instanz unter bestimmten Umständen wiederverwendet wird, wenn eine neue Instanz von
 *   TAQ.Managed angefordert wird und sie noch nicht freigegeben wurde.
 * - Auch die TAQ-Instanzen, die diese Methode nicht aufrufen, "sterben", sobald sie inaktiv
 *   werden.
 * - Das Objekt wird nicht als "tot" gekennzeichnet, wenn irgendwelche
 *   Intervale/Timer/Animationen/Verzögerungen aktiv sind.
 * - Das Objekt kann in jedem Fall im selben Chain weiterverwendet werden.
 *
 * Diese Methode wurde vor allem im Hinblick auf folgendes Szenario implementiert:
 *
 * TAQ.Take(AnyObjects).Each(EachFunction);   // 1. Instanz wird erstellt und lebt 10 Sekunden (Standard)
 * TAQ.Take(OtherObjects).Each(EachFunction); // 2. Instanz wird erstellt und lebt 10 Sekunden
 *
 * In diesem Fall wird TAQ nur dazu benutzt, um bestimmte Objekte schnell durchzulaufen. Nun ist es
 * so, dass die erstellten TAQ-Instanzen mind. MaxLifeTime Millisekunden leben, bis sie freigegeben
 * oder wiederverwendet werden können.
 *
 * Das selbse Beispiel mit der Die-Methode:
 *
 * TAQ.Take(AnyObjects).Each(EachFunction).Die;   // 1. Instanz wird erstellt
 * TAQ.Take(OtherObjects).Each(EachFunction).Die; // 1. Instanz wird wiederverwendet und wird
 *                                                // baldmöglichst freigegeben oder wiederverwendet
 *}
function TAQ.Die:TAQ;
begin
	Result:=Self;
	if Assigned(FIntervals) and (FIntervals.Count > 0) then
		Exit;
	FLifeTick:=0;
end;

{**
 * Führt die übergebene Funktion für jedes Objekt aus
 *
 * Das ist die Kernfunktion der gesamten Klasse, auch wenn sie nicht danach aussieht.
 *}
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

{**
 * Startet eine Verzögerung
 *
 * Identisch mit TAQ.EachInterval, mit dem Unterschied, dass die Each-Funktion nur einmal
 * durchgeführt wird. Intern entspricht es einem Interval und kann daher mittels TAQ.CancelInterval
 * abgebrochen und mittels TAQ.IntervalActors gefunden werden.
 *
 * @see TAQ.EachInterval
 * @see TAQ.CancelInterval
 *
 * @param Delay Anzahl der Millisekunden, nach denen die Each-Funktion durchgeführt wird
 * @param Each Die Each-Funktion, die verzögert ausgeführt werden soll
 *
 * @throws EAQ Wenn Delay >= MaxLifeTime ist
 *}
function TAQ.EachDelay(Delay:Integer; Each:TEachFunction):TAQ;
begin
	if Delay >= MaxLifeTime then
		raise EAQ.CreateFmt('Delay (%d) muss kleiner als MaxLifeTime (%d) sein.',
			[Delay, MaxLifeTime]);
	Result:=EachTimer(Delay, nil, Each);
end;

{**
 * Startet einen unbegrenzten (unendlichen) Timer/Interval
 *
 * Das Interval kann nur manuell mit der Methode TAQ.CancelIntervals beendet werden.
 *
 * @see TAQ.CancelIntervals
 *
 * @param Interval Anzahl der Millisekunden, nach denen die Each-Funktion gestartet wird
 * @param Each Die Each-Funktion, die solange ausgeführt wird, bis das Interval manuell beendet wird
 *
 * @throws EAQ Wenn Interval >= MaxLifeTime ist
 *}
function TAQ.EachInterval(Interval:Integer; Each:TEachFunction):TAQ;
begin
	if Interval >= MaxLifeTime then
		raise EAQ.CreateFmt('Interval (%d) muss kleiner als MaxLifeTime (%d) sein.',
			[Interval, MaxLifeTime]);
	Result:=Self;
	AddInterval(TInterval.Infinite(Interval, Each));
end;

{**
 * Wiederholt die übergebene
 *}
function TAQ.EachRepeat(Times:Integer; EachFunction:TEachFunction): TAQ;
var
	cc:Integer;
begin
	Result:=Self;
	for cc:=1 to Times do
		Each(EachFunction);
end;

{**
 * Startet einen begrenzten Timer, der nach einer definierten Zeit abläuft
 *
 * Ist in den Each-Funktionen (Parameter Each und LastEach) ein Fortschritt vonnöten,
 * wie es z.B. bei Animationen der Fall ist, so kann dieser in TAQ.CurrentInterval.Progress
 * abgerufen werden.
 *
 * @see TAQ.CurrentInterval
 * @see TInterval.Progress
 *
 * Soll der Timer außerhalb beendet werden, so ist die Methode FinishTimers auf die entsprechende
 * TAQ-Instanz anzuwenden. Der Timer kann aber auch abgebrochen werden, in diesem Fall wird aber
 * die entsprechende LastEach nicht aufgerufen, mittels der Methode CancelTimers.
 *
 * @see TAQ.FinishTimers
 * @see TAQ.CancelTimers
 *
 * Eine weitere Möglichkeit der vorzeitigen Beendigung des Timers besteht innerhalb der
 * Each-Funktionen und zwar über TAQ.CurrentInterval.Finish
 *
 * @see TAQ.CurrentInterval
 * @see TInterval.Finish
 *
 * @param Duration Dauer in Millisekunden
 * @param Each Each-Funktion, die in unbestimmten Abständen aufgerufen wird.
 *        Wird LastEach angegeben, so ist es durchaus möglich, dass diese Funktion niemals
 *        aufgerufen wird, wenn der Timer z.B. zu kurz gesetzt ist oder das System ausgelastet ist.
 *        Wird LastEach jedoch nicht definiert (nil), so ist garantiert, dass sie mindestens einmal
 *        (nämlich zum Abschluss) aufgerufen wird.
 * @param LastEach Optional. Standard ist nil. Wird hier eine Each-Funktion angegeben, so ist
 *        sichergestellt (außer der Timer wird mittels TAQ.CancelTimers abgebrochen), dass sie nach
 *        Ablauf des Timers aufgerufen wird. Ist keine Funktion (nil) angegeben, wird der
 *        Each-Parameter stellvertretend verwendet.
 *
 * @throws EAQ Wenn Duration >= MaxLifeTime ist
 *}
function TAQ.EachTimer(Duration:Integer; Each, LastEach:TEachFunction):TAQ;
begin
	if Duration >= MaxLifeTime then
		raise EAQ.CreateFmt('Dauer des Timers (%d) muss kleiner als MaxLifeTime (%d) sein.',
			[Duration, MaxLifeTime]);
	Result:=Self;
	AddInterval(TInterval.Finite(Duration, Each, LastEach));
end;

{**
 * Stellt sicher, dass eine Beschleunigungsfunktion geliefert wird
 *}
class function TAQ.Ease(EaseFunction:TEaseFunction):TEaseFunction;
begin
	if Assigned(EaseFunction) then
		Result:=EaseFunction
	else
		Result:=LinearEase;
end;

{**
 * Liefert eine Beschleunigungsfunktion anhand eines TEaseType
 *}
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

{**
 * Erstellt eine neue TAQ-Instanz, die alle hier verfügbaren Objekte enthält, wenn sie von der
 * übergebenen Klasse abgeleitet sind
 *}
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

{**
 * Übernimmt alle Objekte aus der aktuellen in eine neue TAQ-Instanz, die von der übergebenen
 * Each-Funktion mit TRUE bestätigt werden.
 *}
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

{**
 * Beendet laufende Animationen
 *
 * Im Gegensatz zu TAQ.CancelAnimations wird die Animation sofort im Endzustand beendet.
 *
 * @see TAQ.CancelAnimations
 *
 * @param Local Optional. Standard ist FALSE. Bestimmt, ob alle (FALSE) gemanageten TAQ-Instanzen
 *        auf die hier enthaltenen Objekte untersucht werden sollen. Wird TRUE übergeben, so bezieht
 *        sich die Operation nur auf die aktuelle Instanz.
 *}
function TAQ.FinishAnimations(Local:Boolean):TAQ;
begin
	Result:=Self;
	CustomCancel(Local, ctAnimationFinish);
end;

{**
 * Beendet aktive Timer
 *
 * Auf diese Weise haben Timer die Möglichkeit, ihre letzte Each-Funktion auszuführen.
 *
 * @see TAQ.CancelTimers
 *
 * @param Local Optional. Standard ist FALSE. Bestimmt, ob alle (FALSE) gemanageten TAQ-Instanzen
 *        auf die hier enthaltenen Objekte untersucht werden sollen. Wird TRUE übergeben, so bezieht
 *        sich die Operation nur auf die aktuelle Instanz.
 *}
function TAQ.FinishTimers(Local:Boolean):TAQ;
begin
	Result:=Self;
	CustomCancel(Local, ctTimerFinish);
end;

{**
 * Erstellt ein neues TAQ-Objekt, welches das erste Objekt aus der aktuellen Instanz enthält
 *
 * Hat die aktuelle Instanz keine Objekte, wird ein leeres Objekt geliefert.
 *}
function TAQ.First:TAQ;
begin
	Result:=Managed;
	if Count > 0 then
		Result.Append(Items[0]);
end;

{**
 * Liefert eine Instanz des GarbageCollector, der unter anderem die Aufgabe hat, nicht verwendete
 * TAQ-Instanzen freizugeben.
 *
 * Es wird nach dem Singleton-Pattern instanziert.
 *
 * Der GarbageCollector gibt die von ihm verwaltete TAQ-Instanzen frei, wenn das letzte
 * Lebenszeichen (TAQ.FLifeTick) länger als MaxLifeTime her ist.
 *}
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

{**
 * On-Demand-Instanzierung von FIntervals
 *}
function TAQ.GetIntervals:TObjectList;
begin
	if not Assigned(FIntervals) then
		FIntervals:=TObjectList.Create(TRUE);
	Result:=FIntervals;
end;

{**
 * Der globale OnTimer-Event-Handler von FIntervalTimer
 *
 * Um Speicher zu sparen, wird nur ein TTimer verwendet, der mit einer festen Auflösung läuft.
 * Um nun das Event an die richtigen TAQ-Instanzen zu leiten, wird FActiveIntervalAQs verwendet.
 *}
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

{**
 * Ermittelt, ob mind. ein Interval definiert ist
 *
 * Intervalle werden stets mittels TAQ.EachInterval gestartet.
 *
 * @see TAQ.EachInterval
 *}
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

{**
 * Ermittelt, ob mind. ein Timer definiert ist
 *
 * Timer werden stets mittels TAQ.EachTimer gestartet.
 *
 * @see TAQ.EachTimer
 *}
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

{**
 * Macht einen "Herzschlag" und verlängert damit die Lebenszeit des Objekts
 *
 * @see TAQ.IsAlive
 *}
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

{**
 * Liefert ein neues TAQ-Objekt mit TAQ-Instanzen, die gerade ein Interval, für die in dieser
 * Instanz enthaltene Objekte, haben.
 *
 * Da die Each-Methode rekursiv mit den enthaltenen TAQ-Objekten arbeitet, laufen weitere Aktionen
 * transparent ab.
 *
 * @param IncludeOrphans Optional. Standard ist TRUE. Bestimmt, ob die neue Instanz auch die Objekte
 *        in sich vereint, für die keine passende TAQ-Instanz gefunden wurde.
 *}
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

{**
 * Sagt aus, ob die Lebenszeit der Instanz abgelaufen ist
 *
 * @see TAQ.HeartBeat
 *}
function TAQ.IsAlive:Boolean;
begin
	Result:=(FLifeTick + MaxLifeTime) > GetTickCount;
end;

{**
 * Liefert ein neues TAQ-Objekt, welches das letzte Objekt aus der aktuellen Instanz enthält
 *
 * Hat die aktuelle Instanz keine Objekte, wird ein leeres Objekt geliefert.
 *}
function TAQ.Last:TAQ;
begin
	Result:=Managed;
	if Count > 0 then
		Result.Append(Items[Count - 1]);
end;

{**
 * Lokaler OnTimer-Event-Handler
 *
 * Das ursprüngliche Ereignis kommt von der Klassenmethode GlobalIntervalTimerEvent
 *
 * @see TAQ.GlobalIntervalTimerEvent
 *}
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

{**
 * Liefert eine neue gemanagete TAQ-Instanz, die automatisch freigegeben wird, wenn sie nicht
 * mehr verwendet wird.
 *
 * Auch kann nur eine gemanagete TAQ-Instanz von Intervallen profitieren.
 *}
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

{**
 * Erstellt eine neue komplexe TAQ-Instanz, die jedes enthaltene Nicht-TAQ-Objekt in eine separate
 * TAQ-Instanz verpackt und der neuen Instanz unterordnet.
 *
 * Wenn untergeordnete TAQ-Instanzen enthalten sind, so werden diese verworfen, aber ihre Objekte
 * übernommen.
 *
 * Dieses Verfahren wird z.B. bei den internen Animationsmethoden angewendet.
 *
 * Die verschachtelten TAQ-Objekte können mittels TAQ.Demultiplex wieder in ein einfaches TAQ-Objekt
 * umgewandelt werden.
 *
 * @see TAQ.Demultiplex
 *}
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

{**
 * Ermittelt die Elternobjekte für die enthaltenen Objekte
 *
 * @param Append Optional. Standard ist FALSE. Bestimmt, ob die Elternobjekte der aktuellen
 *        TAQ-Instanz hinzugefügt werden sollen (TRUE) oder ob eine neue Instanz erstellt
 *        werden soll (FALSE), die zurückgeliefert wird.
 * @param Recurse Optional. Standard ist FALSE. Bestimmt, ob die ermittelten Elternobjekte wiederrum
 *        auf Elternobjekte untersucht werden sollen.
 * @param ParentsFiller Optional. Standard ist nil. Wie der Name schon vermuten lässt, hat die
 *        Funktion die Aufgabe, dem übergebenen TAQ-Objekt die Elternobjekte hinzuzufügen. Wird der
 *        Parameter nicht angegeben (nil), so wird eine interne Funktion verwendet, die nur
 *        von TComponent abgeleitete Objekte akzeptiert.
 *        Die interne Funktion verwendet die Eigenschaft TComponent.HasParent und die Methode
 *        TComponent.GetParentComponent für die Objektfindung.
 *}
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

{**
 * Führt die TInterval.Each-Funktion aus und sagt aus, ob das Interval noch aktiv ist
 *
 * Wenn diese Methode FALSE liefert, so heisst es, dass das Interval auch aus FIntervals entfernt
 * wurde, da es beendet ist.
 *}
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

{**
 * Entfernt ein zuvor mittels TAQ.AddInterval hinzugefügtes Interval
 *
 * @see TAQ.AddInterval
 *}
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

{**
 * Führt eine Schüttel-Animation mit allen enthaltenen TControl-Objekten durch
 *
 * @param XTimes Bestimmt, wie oft das Objekt rechts und links geschüttelt werden soll
 *        Wird 0 angegeben, so wird es nicht in der X-Achse geschüttelt.
 * @param XDiff Bestimmt, wie weit nach rechts bzw. nach links geschüttelt werden soll
 * @param YTimes Bestimmt, wie oft das Objekt runter und hoch geschüttelt werden soll
 *        Wird 0 angegeben, so wird es nicht in der Y-Achse geschüttelt.
 * @param YDiff Bestimmt, wie weit nach unten bzw. nach oben geschüttelt werden soll
 * @param Duration Die Dauer der Animation in Millisekunden
 * @param OnComplete Event-Handler, der aufgerufen wird, wenn die Animation beendet ist.
 *        Das Ereignis wird für jedes animierte Objekt, welches im Sender übergeben wird,
 *        ausgelöst.
 *}
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

{**
 * Erstellt eine neue gemanagete TAQ-Instanz, mit Objekten aus dem Array
 *}
class function TAQ.Take(Objects:TObjectArray):TAQ;
begin
	Result:=Managed.Append(Objects);
end;

{**
 * Erstellt eine neue gemanagete TAQ-Instanz, mit einem einzelnen übergebenen Objekt
 *}
class function TAQ.Take(AObject:TObject):TAQ;
begin
	Result:=Managed.Append(AObject);
end;

{**
 * Liefert ein neues TAQ-Objekt mit TAQ-Instanzen, die gerade einen Timer, für die in dieser
 * Instanz enthaltenen Objekte, haben.
 *
 * Da die Each-Methode rekursiv mit den enthaltenen TAQ-Objekten arbeitet, laufen weitere Aktionen
 * transparent ab.
 *
 * @param IncludeOrphans Optional. Standard ist TRUE. Bestimmt, ob die neue Instanz auch die Objekte
 *        in sich vereint, für die keine passende TAQ-Instanz gefunden wurde.
 *}
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

{**
 * Liefert eine neue !nicht! gemanagete TAQ-Instanz
 *
 * Sie muss manuell freigegeben werden.
 *
 * Ein sinnvolles Einsatzszenario wäre:
 *
 * TAQ.Unmanaged.Append(ObjectList).Each(function...).Free;
 *
 * Einschränkung: Die EachTimer- und EachInterval-Methoden können mit ungemanageten TAQ-Instanzen
 * nicht verwendet werden, da die OnTimer-Event-Verteilung über den GarbageCollector in der Methode
 * TAQ.GlobalIntervalTimerEvent abgewickelt wird.
 *
 * @see TAQ.GlobalIntervalTimerEvent
 *}
class function TAQ.Unmanaged:TAQ;
begin
	Result:=TAQ.Create;
end;

{**
 * Erstellt eine neue gemanagete TAQ-Instanz, mit Objekten aus einer TObjectList
 *
 * Anmerkung: TAQ ist von TObjectList abgeleitet und kann somit auch hier übergeben werden
 *}
class function TAQ.Take(Objects:TObjectList):TAQ;
begin
	Result:=Managed.Append(Objects);
end;

{** TInterval **}

{**
 * Bricht das Interval ab
 *
 * Auswirkungen:
 * - TInterval.Each wird keine Funktion (nil) zurückliefern.
 * - TInterval.IsFinished liefert TRUE
 *
 * @see TInterval.Finish
 *}
procedure TInterval.Cancel;
begin
	FInterval:=0;
end;

{**
 * Liefert die entsprechenden Each-Funktion, wenn der Zeitpunkt erreicht ist oder nil
 *}
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

{**
 * Manuelle Beendigung eines endlichen Intervals
 *
 * Der nächste Aufruf von TInterval.Each liefert die letzte Each-Funktion.
 *
 * @see TInterval.Cancel
 *}
procedure TInterval.Finish;
begin
	if IsFinite and not IsFinished then
		FLastTick:=GetTickCount;
end;

{**
 * Erstellt einen endlichen Interval
 *
 * @param Duration Die Dauer des gesamten Intervals in Millisekunden.
 *        Wert muss >= IntervalResolution sein.
 * @param Each Each-Funktion, die in Teilintervalen ausgeführt wird. Es ist durchaus möglich, dass
 *        diese nie ausgeführt wird, es sei denn der LastEach-Parameter wird nicht angegeben.
 * @param LastEach Each-Funktion, die in jedem Fall beim Ablauf des Gesamtintervals ausgeführt wird.
 *        Wird hier nil angegeben, wird stellvertretend die im Each-Parameter angegebene Funktion
 *        verwendet.
 *}
constructor TInterval.Finite(Duration:Integer; Each, LastEach:TEachFunction);
begin
	FNextEach:=Each;
	FLastEach:=LastEach;

	FFirstTick:=GetTickCount;
	FInterval:=Max(IntervalResolution, Ceil(Duration / IntervalResolution));

	FLastTick:=FFirstTick + Cardinal(Duration);
	UpdateNextTick;
end;

{**
 * Erstellt einen unbegrenzten Interval
 *
 * @param Interval Angabe in Millisekunden
 *        Wert muss >= IntervalResolution sein.
 * @param Each Each-Funktion, die immer nach dem gesetzten Interval ausgeführt wird
 *}
constructor TInterval.Infinite(Interval:Integer; Each:TEachFunction);
begin
	FFirstTick:=GetTickCount;
	FNextEach:=Each;
	FLastEach:=nil;
	FInterval:=Max(IntervalResolution, Interval);
	FLastTick:=0;
	UpdateNextTick;
end;

{**
 * Sagt aus, ob das Interval zuvor mittels TInterval.Cancel abgebrochen wurde
 *
 * @see TInterval.Cancel
 *}
function TInterval.IsCanceled:Boolean;
begin
	Result:=FInterval = 0;
end;

{**
 * Sagt aus, ob das Interval beendet ist
 *
 * Beendet ist es, wenn FLastTick eines endlichen Intervals abgelaufen ist oder das Interval
 * abgebrochen wurde.
 *}
function TInterval.IsFinished:Boolean;
begin
	Result:=((FLastTick > 0) and (GetTickCount >= FLastTick)) or IsCanceled;
end;

{**
 * Sagt aus, ob das Interval endlich ist
 *
 * In TAQ wird TRUE als Timer behandlet und FALSE als Interval, doch in diesem Kontext ist beides
 * ein Interval ;)
 *}
function TInterval.IsFinite:Boolean;
begin
	Result:=FLastTick > 0;
end;

{**
 * Liefert den Fortschritt des Intervals als Fließkommazahl im Bereich von 0..1
 *}
function TInterval.Progress:Real;
begin
	Result:=Min(1, (GetTickCount - FFirstTick) / Max(1, (FLastTick - FFirstTick)));
end;

{**
 * Aktualisiert den nächsten gültigen Tick
 *}
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
