unit AccessQuery;

interface

uses
	SysUtils, Classes, Controls, ExtCtrls, Contnrs, Generics.Collections, Windows, Math;

type
	TAQ = class;

	TObjectArray = array of TObject;
	{**
	 * Typ für anonyme Each-Funktionen
	 *
	 * @param AQ Die TAQ-Instanz, die die Each-Funktion aufruft
	 * @param O Das Objekt, welches durch die Each-Funktion bearbeitet werden soll
	 * @return Boolean Sobald die Funktion FALSE liefert, wird die Ausführung der Each-Prozedur
	 *         angehalten
	 *}
	TEachFunction = reference to function(AQ:TAQ; O:TObject):Boolean;
	{**
	 * Typ für nicht lineare Beschleunigungsfunktionen
	 *
	 * @param StartValue Der Wert, ab dem gestartet wird
	 * @param EndValue Der Endwert, der erreicht werden sollte, wenn Progress = 1 ist
	 * @param Progress Fortschritt der Beschleunigung. Wert zwischen 0..1
	 * @return Real
	 *}
	TEaseFunction = function(StartValue, EndValue, Progress:Real):Real;

	TEaseType = (etLinear, etQuadratic, etMassiveQuadratic);

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
		 *}
		FInterval:Integer;
		{**
		 * Die Each-Funktion, die nicht unbedingt ausgeführt wird ()
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
		function IsFinished:Boolean;
		function Progress:Real;
	end;

	TAQ = class(Contnrs.TObjectList)
	private
	class var
		FGarbageCollector:TAQ;
	var
		FLifeTick:Cardinal;
		FIntervals:TObjectList<TInterval>;
		FIntervalTimer:TTimer;
		FCurrentInterval:TInterval;
		FAnimating:Boolean;
	protected
		class function GarbageCollector:TAQ;
		class function Managed:TAQ;

		procedure CheckIntervalTimer;
		procedure IntervalTimerEvent(Sender:TObject);
		procedure AnimateObject(O:TObject; Duration:Integer; Each:TEachFunction;
			LastEach:TEachFunction = nil);
	public
		constructor Create; reintroduce;
		destructor Destroy; override;

		class function EaseFunction(EaseType:TEaseType):TEaseFunction;

		function Each(EachFunction:TEachFunction):TAQ;
		function EachInterval(Interval:Integer; Each:TEachFunction):TAQ;
		function EachTimer(Duration:Integer; Each:TEachFunction; LastEach:TEachFunction = nil):TAQ;

		function BoundsAnimation(NewLeft, NewTop, NewWidth, NewHeight:Integer; Duration:Integer;
			EaseType:TEaseType = etLinear; OnComplete:TNotifyEvent = nil):TAQ;

		class function Take(Objects:TObjectArray):TAQ; overload;
		class function Take(AQ:TAQ):TAQ; overload;
		class function Take(AObject:TObject):TAQ; overload;

		class function Animator(SO:TObject):TAQ;

		{**
		 * Diese Eigenschaft ist für jene TEachFunction-Funktionen gedacht, die aus dem Kontext der
		 * EachInterval- und EachTimer-Methoden aufgerufen werden.
		 *}
		property CurrentInterval:TInterval read FCurrentInterval;
		{**
		 * Sagt aus, ob die aktuelle Instanz eine Animation durchführt
		 *}
		property Animating:Boolean read FAnimating;
	end;

	function LinearEase(StartValue, EndValue, Progress:Real):Real; forward;
	function QuadraticEase(StartValue, EndValue, Progress:Real):Real; forward;
	function MassiveQuadraticEase(StartValue, EndValue, Progress:Real):Real; forward;

implementation

const
	{**
	 * Die maximale Lebensdauer einer TAQ-Instanz bei Nichtbeschäftigung
	 *}
	MaxLifeTime = 10000;
	{**
	 * Der Interval basiert auf 40 msec (25fps = 1 sec)
	 *}
	IntervalResolution = 40;


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
 * Startet die Animation für ein einzelnes Objekt
 *
 * Es ist lediglich eine Switch-Methode, die entscheidet ob das übergebene Objekt in der aktuellen
 * TAQ-Instanz oder in einer neuen Instanz ausgeführt werden soll.
 *
 * Hintergrund: Alle Animationsmethoden von TAQ sollen eine beliebige Anzahl von Objekten für die
 * Animierung unterstützen. Da die Animationen über anonyme Funktionen abgewickelt werden und diese
 * in Delphi nicht explizit neuerstellt werden können, hat es sich als unmöglich herausgestellt
 * mehrere Aufrufe von Each-Funktionen mit verschachtelten Kontexten durchzuführen (der nächste
 * Aufruf überschreibt den Status des Vorhergehenden). Diese Einschränkung gilt jedoch nicht, wenn
 * für jede Animation eine separate TAQ-Instanz genommen wird und genau das soll diese Methode für
 * uns machen. Durch die gemanagte Instanzierung brauche ich mir keine Gedanken über zuviele
 * Instanzen machen...
 *}
procedure TAQ.AnimateObject(O:TObject; Duration:Integer; Each, LastEach:TEachFunction);
begin
	{**
	 * Wenn es nur ein Objekt ist, welches sich in der aktuellen TAQ-Instanz befindet, dann kann
	 * man die aktuelle TAQ auch nehmen...
	 *}
	if (Count = 1) and (Items[0] = O) then
		EachTimer(Duration, Each, LastEach)
	{**
	 * ...andernfalls muss ein neues her.
	 *}
	else
		TAQ.Take(O).EachTimer(Duration, Each, LastEach);
end;

{**
 * Liefert die TAQ-Instanz, die mit der Animierung des übergebenen Objekts beschäftigt ist
 *
 * Achtung: Wird das Objekt nicht animiert, wird nil geliefert!
 *}
class function TAQ.Animator(SO:TObject):TAQ;
var
	AnimatorAQ:TAQ;
begin
	AnimatorAQ=nil;
	{**
	 * Der GarbageCollector enthält ja bekanntlich alle TAQ-Instanzen
	 *}
	GarbageCollector.Each(
		function(AQ:TAQ; O:TObject):Boolean
		begin
			{**
			 * Um spätere Verwirrungen vorzubeugen: In AQ befindet sich der GarbageCollector und
			 * in O eine reguläre TAQ-Instanz
			 *}
			Result:=TAQ(O).IndexOf(SO) = -1;
			{**
			 * Folgende if bedeutet, ich habe die entsprechende Instanz gefunden und die ausführende
			 * Each wird damit beendet.
			 *}
			if not Result and TAQ(O).Animating then
				AnimatorAQ:=TAQ(O);
		end);
	Result:=AnimatorAQ;
end;

{**
 * Führt eine Positionierungsanimation mit den enthaltenen TControl-Objekten durch
 *
 * @param NewLeft Neue absolute Links-Position.
 * @param NewTop Neue absolute Oben-Position.
 * @param NewWidth Neue absolute Breite. Soll die Breite nicht verändert werden, ist -1 anzugeben.
 * @param NewHeight Neue absolute Höhe. Soll die Höhe nicht verändert werden, ist -1 anzugeben.
 * @param Duration Die Dauer der Animation in Millisekunden.
 * @param EaseType Standard ist etLinear. Die Art der Beschleunigung.
 * @param OnComplete Stanadrd ist nil. Event-Handler, der ausgelöst wird, wenn die Animation
 *        beendet ist. Das Ereignis wird übrigens für jedes Objekt, das animiert wird, ausgelöst.
 *}
function TAQ.BoundsAnimation(NewLeft, NewTop, NewWidth, NewHeight:Integer; Duration:Integer;
	EaseType:TEaseType; OnComplete:TNotifyEvent):TAQ;
var
	WholeEach:TEachFunction;
begin
	Result:=Self;

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

			AniLeft:=Ceil(EaseFunction(EaseType)(PrevLeft, NewLeft, Progress));
			AniTop:=Ceil(EaseFunction(EaseType)(PrevTop, NewTop, Progress));
			if NewWidth >= 0 then
				AniWidth:=Ceil(EaseFunction(EaseType)(PrevWidth, NewWidth, Progress))
			else
				AniWidth:=TControl(O).Width;
			if NewHeight >= 0 then
				AniHeight:=Ceil(EaseFunction(EaseType)(PrevHeight, NewHeight, Progress))
			else
				AniHeight:=TControl(O).Height;

			if Progress = 1 then
			begin
				{$IFDEF DEBUG}
				OutputDebugString(PWideChar('Animation beendet für ' + IntToHex(Integer(O),
					SizeOf(Integer) * 2)));
				{$ENDIF}
				if Assigned(OnComplete) then
					OnComplete(O);
			end;

			TControl(O).SetBounds(AniLeft, AniTop, AniWidth, AniHeight);
		end;

		AnimateObject(O, Duration, EachF);
	end;

	Result.Each(WholeEach);
end;

{**
 * Überprüft, ob FIntervalTimer erstellt werden muss, oder ob er freigegeben werden kann
 *}
procedure TAQ.CheckIntervalTimer;
begin
	if (FIntervals.Count > 0) and not Assigned(FIntervalTimer) then
	begin
		FIntervalTimer:=TTimer.Create(nil);
		with FIntervalTimer do
		begin
			Enabled:=TRUE;
			Interval:=IntervalResolution;
			OnTimer:=IntervalTimerEvent;
		end;
	end
	else if (FIntervals.Count = 0) and Assigned(FIntervalTimer) then
		FreeAndNil(FIntervalTimer);
end;

constructor TAQ.Create;
begin
	inherited Create(FALSE);
	FIntervals:=TObjectList<TInterval>.Create(TRUE);
	FLifeTick:=GetTickCount;
end;

class function TAQ.Take(Objects:TObjectArray):TAQ;
var
	cc:Integer;
begin
	Result:=Managed;
	for cc:=0 to Length(Objects) - 1 do
		Result.Add(Objects[cc]);
end;

destructor TAQ.Destroy;
begin
	if Assigned(FIntervalTimer) then
		FreeAndNil(FIntervalTimer);
	FIntervals.Free;
	inherited;
end;

{**
 * Führt die übergebene Funktion für jedes Objekt aus
 *
 * Das ist die Kernfunktion der gesamten Klasse.
 *}
function TAQ.Each(EachFunction:TEachFunction):TAQ;
var
	cc:Integer;
begin
	Result:=Self;
	for cc:=Count - 1 downto 0 do
	begin
		if not EachFunction(Self, Items[cc]) or (cc >= Count) then
			Break;
	end;
	FLifeTick:=GetTickCount;
end;

{**
 * Startet einen unbegrenzten (unendlichen) Timer
 *}
function TAQ.EachInterval(Interval:Integer; Each:TEachFunction):TAQ;
begin
	Result:=Self;
	FIntervals.Add(TInterval.Infinite(Interval, Each));
	CheckIntervalTimer;
end;

{**
 * Startet einen begrenzten Timer, der nach einer definierten Zeit beendet ist
 *}
function TAQ.EachTimer(Duration:Integer; Each, LastEach:TEachFunction):TAQ;
begin
	Result:=Self;
	FIntervals.Add(TInterval.Finite(Duration, Each, LastEach));
	CheckIntervalTimer;
end;

class function TAQ.EaseFunction(EaseType:TEaseType):TEaseFunction;
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
 * Liefert eine Instanz des GarbageCollector, der die Aufgabe hat, nicht verwendete TAQ-Instanzen
 * freizugeben.
 *
 * Es wird nach dem Singleton-Pattern instanziert. Wenn das letzte Lebenszeichen (TAQ.FLifeTick)
 * länger als MaxLifeTime her ist, wird es freigegeben.
 *}
class function TAQ.GarbageCollector:TAQ;
begin
	if not Assigned(FGarbageCollector) then
	begin
		FGarbageCollector:=TAQ.Create;
		FGarbageCollector.OwnsObjects:=TRUE;
		FGarbageCollector.EachInterval(1000,
			{**
			 * In GarbageCollector befindet sich der FGarbageCollector selbst und
			 * in O eine TAQ-Instanz die auf ihre Lebenszeichen untersucht werden muss.
			 *}
			function(GarbageCollector:TAQ; O:TObject):Boolean
			var
				CheckAQ:TAQ;
			begin
				CheckAQ:=TAQ(O);
				if GetTickCount > (CheckAQ.FLifeTick + MaxLifeTime) then
				begin
					GarbageCollector.Remove(CheckAQ);
					{$IFDEF DEBUG}
					OutputDebugString(PWideChar(Format('Verbleibende TAQ-Instanzen im GarbageCollector: %d', [GarbageCollector.Count])));
					{$ENDIF}
				end;
				Result:=TRUE;
			end);
	end;
	Result:=FGarbageCollector;
end;

procedure TAQ.IntervalTimerEvent(Sender:TObject);
var
	EachFunction:TEachFunction;
	AnyRemoved:Boolean;
	TempInterval:TInterval;
begin
	if not Assigned(FIntervalTimer) or not Assigned(FIntervals) then
		Exit;

	AnyRemoved:=FALSE;

	for TempInterval in FIntervals do
	begin
		FCurrentInterval:=TempInterval;
		EachFunction:=(CurrentInterval.Each);
		if Assigned(EachFunction) then
		begin
			Each(EachFunction);
			if CurrentInterval.IsFinished then
			begin
				FIntervals.Remove(FCurrentInterval);
				FCurrentInterval:=nil;
				AnyRemoved:=TRUE;
			end;
		end;
	end;
	if AnyRemoved then
		CheckIntervalTimer;
end;



{**
 * Liefert eine neue gemanagete TAQ-Instanz, die automatisch freigegeben wird, wenn sie nicht
 * mehr verwendet wird.
 *}
class function TAQ.Managed:TAQ;
begin
	Result:=TAQ.Create;
	GarbageCollector.Add(Result);
end;

class function TAQ.Take(AObject:TObject):TAQ;
begin
	Result:=Managed;
	Result.Add(AObject);
end;

class function TAQ.Take(AQ:TAQ):TAQ;
begin
	Result:=Managed;
	Result.Assign(AQ);
end;

{** TInterval **}

function TInterval.Each:TEachFunction;
var
	CurrentTick:Cardinal;
begin
	CurrentTick:=GetTickCount;
	Result:=nil;
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
 * Erstellt einen endlichen Interval
 *
 * @param Duration Die Dauer des gesamten Intervals in Millisekunden
 * @param Each Each-Funktion, die in Teilintervalen ausgeführt wird. Es ist durchaus möglich, dass
 *        diese nie ausgeführt wird, es sei denn der LastEach-Parameter wird nicht angegeben.
 * @param LastEach Each-Funktion, die in jedem Fall beim Ablauf des Gesamtinterval ausgeführt wird.
 *        Wird hier nil angegeben, wird stellvertretend die im Each-Parameter angegebene Funktion
 *        verwendet.
 *}
constructor TInterval.Finite(Duration:Integer; Each, LastEach:TEachFunction);
begin
	FNextEach:=Each;
	FLastEach:=LastEach;

	FFirstTick:=GetTickCount;
	FInterval:=Max(1, Ceil(Duration / IntervalResolution));

	FLastTick:=FFirstTick + Cardinal(Duration);
	UpdateNextTick;
end;

{**
 * Erstellt einen unbegrenzten Interval
 *
 * @param Interval Angabe in Millisekunden
 * @param Each Each-Funktion, die immer nach dem gesetzten Interval ausgeführt wird
 *}
constructor TInterval.Infinite(Interval:Integer; Each:TEachFunction);
begin
	FFirstTick:=GetTickCount;
	FNextEach:=Each;
	FLastEach:=nil;
	FInterval:=Interval;
	FLastTick:=0;
	UpdateNextTick;
end;

{**
 * Sagt aus, ob das Interval beendet ist
 *
 * Folglich kann diese Methode TRUE nur bei endlichen Intervalen (TInterval.Finite) liefern.
 *}
function TInterval.IsFinished:Boolean;
begin
	Result:=(FLastTick > 0) and (GetTickCount >= FLastTick);
end;

{**
 * Liefert den Fortschritt des Intervals als Fließkommazahl im Bereich von 0..1
 *}
function TInterval.Progress:Real;
begin
	Result:=Min(1, (GetTickCount - FFirstTick) / (FLastTick - FFirstTick));
end;

{**
 * Aktualisiert den nächsten gültigen Tick
 *}
procedure TInterval.UpdateNextTick;
begin
	FNextTick:=GetTickCount + Cardinal(FInterval);
end;

end.
