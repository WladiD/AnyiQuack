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
 * The Original Code is AnyiQuack.pas
 *
 * The Initial Developer of the Original Code is Waldemar Derr.
 * Portions created by Waldemar Derr are Copyright (C) Waldemar Derr.
 * All Rights Reserved.
 *
 * @author Waldemar Derr <furevest@gmail.com>
 *}

unit AnyiQuack;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Types,
  System.Classes,
  System.Contnrs,
  System.Character,
  System.Math,
  System.Diagnostics,
  System.SyncObjs,
  System.UITypes,
  Generics.Collections;

{$INCLUDE Compile.inc}

const
  Version = '1.0.4';

type
  EAQ = class(Exception);
  TAQBase = class;
  TAQ = class;
  TAQPlugin = class;
  TInterval = class;
  TTimerThread = class;

  TEaseType = (etLinear, etQuad, etCubic, etQuart, etQuint, etSext, etSinus, etElastic, etBack,
    etLowWave, etMiddleWave, etHighWave, etBounce, etCircle, etSwing10, etSwing50, etSwing100,
    etSwing200);
  TEaseModifier = (
    emIn, emInInverted, emInSnake, emInSnakeInverted,
    emOut, emOutInverted, emOutSnake, emOutSnakeInverted,
    emInOut, emInOutMirrored, emInOutCombined,
    emOutIn, emOutInMirrored, emOutInCombined);
  TActorRole = (arTimer, arInterval, arDelay, arAnimation);
  TActorRoles = set of TActorRole;

  TObjectArray = TArray<TObject>;
  TEaseArray = array of TEaseType;

  TEachFunction = reference to function(AQ: TAQ; O: TObject): Boolean;
  TEachMiscFunction<T> = reference to function(AQ: TAQ; O: TObject; Misc: T): Boolean;
  TAnonymNotifyEvent = reference to procedure(Sender: TObject);
  TEaseFunction = reference to function(Progress: Real): Real;

  TAQBase = class(TObjectList)
  protected
    function Each(EachFunction: TEachFunction): TAQ; virtual; abstract;
  public
    constructor Create; reintroduce; virtual;
  end;

  TAQPlugin = class(TAQBase)
  protected
    FWorkAQ: TAQ;

    function GarbageCollector: TAQ;
    function Each(EachFunction: TEachFunction): TAQ; override;

    procedure Autorun; virtual;

    procedure SetImmortally(Value: Boolean);
    function GetImmortally: Boolean;

    property Immortally: Boolean read GetImmortally write SetImmortally;
  public
    property WorkAQ: TAQ read FWorkAQ;
  end;

  TAQ = class sealed (TAQBase)
  // Private section for local types and constants
  private
    type
    // Enumeration for all public TAQ methods, which returns a TAQ instance
    TAQMethod = (
      // Each related
      aqmEach,
      aqmEachAnimation,
      aqmEachDelay,
      aqmEachInterval,
      aqmEachRepeat,
      aqmEachTimer,

      // Append related
      aqmAppend,
      aqmAppendAQ,
      aqmChildrenAppend,
      aqmParentsAppend,

      // Finish related
      aqmFinishAnimations,
      aqmFinishTimers,

      // Cancel related
      aqmCancelAnimations,
      aqmCancelDelays,
      aqmCancelIntervals,
      aqmCancelTimers,

      // Chain related
      aqmNewChain,
      aqmExcludeChain,
      aqmFilterChain,
      aqmChildrenChain,
      aqmIntervalActorsChain,
      aqmAnimationActorsChain,
      aqmDelayActorsChain,
      aqmDemultiplexChain,
      aqmTimerActorsChain,
      aqmMultiplexChain,
      aqmParentsChain,
      aqmSliceChain,
      aqmEndChain,

      // Conditional chain related
      aqmIfThen,
      aqmIfAll,
      aqmIfAny,
      aqmIfContainsAll,
      aqmIfContainsAny,
      aqmIfContains,
      aqmIfElse,
      aqmIfEnd,

      // Misc
      aqmDebugMessage,
      aqmDie);

    TAQMethods = set of TAQMethod;

    const
    AQEachMethods: TAQMethods = [aqmEach, aqmEachAnimation, aqmEachDelay, aqmEachInterval,
      aqmEachRepeat, aqmEachTimer];

    AQAppendMethods: TAQMethods = [aqmAppend, aqmAppendAQ, aqmChildrenAppend, aqmParentsAppend];

    AQFinishMethods: TAQMethods = [aqmFinishAnimations, aqmFinishTimers];

    AQCancelMethods: TAQMethods = [aqmCancelAnimations, aqmCancelDelays, aqmCancelIntervals,
      aqmCancelTimers];

    AQChainMethods: TAQMethods = [aqmNewChain, aqmExcludeChain, aqmFilterChain, aqmChildrenChain,
      aqmIntervalActorsChain, aqmAnimationActorsChain, aqmDelayActorsChain,
      aqmDemultiplexChain, aqmTimerActorsChain, aqmMultiplexChain, aqmParentsChain,
      aqmSliceChain, aqmEndChain];

    AQConditionMethods: TAQMethods = [aqmIfThen, aqmIfAll, aqmIfAny, aqmIfContainsAll,
      aqmIfContainsAny, aqmIfContains, aqmIfElse, aqmIfEnd];

  // Private class related stuff
  private
    class var
    FGC: TAQ;
{$IFDEF UseThreadTimer}
    FTimerThread: TTimerThread;
{$ELSE}
    FTimerHandler: Cardinal;
{$ENDIF}
    FActiveIntervalAQs: TAQ;
    FStopWatch: TStopWatch;
    FTick: Int64;
    FComponentsNotifier: TComponentList;
    FIDGenerator: Integer;

    class procedure Initialize;
    class procedure Finalize;

    class function GarbageCollector: TAQ;
    class procedure GlobalIntervalTimerEvent;
    class procedure ComponentsNotification(AComponent: TComponent; Operation: TOperation);
    class function EaseIntegrated(EaseType: TEaseType): TEaseFunction;

    class property Tick: Int64 read FTick;

  // Private instance related stuff
  private
    FLifeTick: Int64;
    FIntervals: TObjectList;
    FCurrentInterval: TInterval;
    FChainedTo: TAQ;
    FConditionCount: Byte;
    FBools: Byte;

    function HasActors(ActorRole: TActorRole; ID: Integer = 0): Boolean;

    procedure LocalIntervalTimerEvent;

    function GetIntervals: TObjectList;
    procedure ClearIntervals;
    procedure AddInterval(Interval: TInterval);
    procedure ProcessInterval(Interval: TInterval);
    procedure RemoveInterval(Interval: TInterval);

    function CustomFiller(Filler: TEachFunction; Append, Recurse: Boolean): TAQ;
    procedure CustomCancel(ActorRole: TActorRole; ID: Integer; Finish: Boolean);
    function CustomActors(ActorRole: TActorRole; ID: Integer; IncludeOrphans: Boolean): TAQ;

    function IfContainsEach(ByClass: TClass): TEachFunction; overload;
    function IfContainsEach(Objects: TObjectArray): TEachFunction; overload;
    function IfContainsEach(Objects: TObjectList): TEachFunction; overload;
    function IfContainsEach(AQ: TAQ): TEachFunction; overload;

    function ChildrenFiller(AQ: TAQ; O: TObject): Boolean;
    function ParentsFiller(AQ: TAQ; O: TObject): Boolean;

    function IsAlive: Boolean;
    procedure HeartBeat;

    function SupervisorLock(out AQ: TAQ; Method: TAQMethod): Boolean;

    procedure SetRecurse(Value: Boolean);
    function GetRecurse: Boolean;
    procedure SetConditionLock(Value: Boolean);
    function GetConditionLock: Boolean;
    procedure SetImmortally(Value: Boolean);
    function GetImmortally: Boolean;

    property Recurse: Boolean read GetRecurse write SetRecurse;
    property ConditionLock: Boolean read GetConditionLock write SetConditionLock;
    property Immortally: Boolean read GetImmortally write SetImmortally;

  // Because TAQ is sealed, no new methods are introduced as protected, but some must be overriden
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;

  // Public class related stuff
  public
    class function Managed: TAQ;
    class function Unmanaged: TAQ;

    class function Take(AObject: TObject): TAQ; overload;
    class function Take(const Objects: TObjectArray): TAQ; overload;
    class function Take(Objects: TObjectList): TAQ; overload;
    class function Take<T: class>(Objects: TObjectList<T>): TAQ; overload;

    class function HasActiveActors(CheckActors: TActorRoles; AObject: TObject; ID: Integer = 0): Boolean;

    class function GetUniqueID: Integer;

    class function Ease(EaseType: TEaseType;
      EaseModifier: TEaseModifier = emIn): TEaseFunction; overload;
    class function Ease(const EaseTypes: array of TEaseType;
      EaseModifier: TEaseModifier = emIn): TEaseFunction; overload;
    class function Ease(EaseFunction: TEaseFunction = nil;
      EaseModifier: TEaseModifier = emIn): TEaseFunction; overload;

    class function EaseReal(StartValue, EndValue, Progress: Real; EaseType: TEaseType;
      EaseModifier: TEaseModifier = emIn): Real; overload;
    class function EaseReal(StartValue, EndValue, Progress: Real;
      EaseFunction: TEaseFunction): Real; overload;

    class function EaseInteger(StartValue, EndValue: Integer; Progress: Real; EaseType: TEaseType;
      EaseModifier: TEaseModifier = emIn): Integer; overload;
    class function EaseInteger(StartValue, EndValue: Integer; Progress: Real;
      EaseFunction: TEaseFunction): Integer; overload;

    class function EaseColor(StartColor, EndColor: TColor; Progress: Real; EaseType: TEaseType;
      EaseModifier: TEaseModifier = emIn): TColor; overload;
    class function EaseColor(StartColor, EndColor: TColor; Progress: Real;
      EaseFunction: TEaseFunction): TColor; overload;

    class function EasePoint(StartPoint, EndPoint: TPoint; Progress: Real; EaseType: TEaseType;
      EaseModifier: TEaseModifier = emIn): TPoint; overload;
    class function EasePoint(StartPoint, EndPoint: TPoint; Progress: Real;
      EaseFunction: TEaseFunction): TPoint; overload;

    class function EaseRect(StartRect, EndRect: TRect; Progress: Real; EaseType: TEaseType;
      EaseModifier: TEaseModifier = emIn): TRect; overload;
    class function EaseRect(StartRect, EndRect: TRect; Progress: Real;
      EaseFunction: TEaseFunction): TRect; overload;

    class function EaseString(StartString, EndString: String; Progress: Real; EaseType: TEaseType;
      EaseModifier: TEaseModifier = emIn): String; overload;
    class function EaseString(StartString, EndString: String; Progress: Real;
      EaseFunction: TEaseFunction): String; overload;

  // Public instance related stuff
  public
    constructor Create; override;
    destructor Destroy; override;

    function Each(EachFunction: TEachFunction): TAQ; override;
    function EachInterval(Interval: Integer; Each: TEachFunction; ID: Integer = 0): TAQ;
    function EachTimer(Duration: Integer; Each: TEachFunction; LastEach: TEachFunction = nil;
      ID: Integer = 0): TAQ;
    function EachAnimation(Duration: Integer; Each: TEachFunction; LastEach: TEachFunction = nil;
      ID: Integer = 0): TAQ;
    function EachDelay(Delay: Integer; Each: TEachFunction; ID: Integer = 0): TAQ;
    function EachRepeat(Times: Integer; EachFunction: TEachFunction): TAQ;

    function NewChain: TAQ;
    function EndChain: TAQ;

    function Die: TAQ;

    function Append(AObject: TObject): TAQ; overload;
    function Append(Objects: TObjectArray): TAQ; overload;
    function Append(Objects: TObjectList): TAQ; overload;
    function AppendAQ(AQ: TAQ): TAQ;

    function ChildrenAppend(Recurse: Boolean = False; ChildrenFiller: TEachFunction = nil): TAQ;
    function ChildrenChain(Recurse: Boolean = False; ChildrenFiller: TEachFunction = nil): TAQ;
    function ParentsAppend(Recurse: Boolean = False; ParentsFiller: TEachFunction = nil): TAQ;
    function ParentsChain(Recurse: Boolean = False; ParentsFiller: TEachFunction = nil): TAQ;

    function MultiplexChain: TAQ;
    function DemultiplexChain: TAQ;

    function AnimationActorsChain(ID: Integer = 0; IncludeOrphans: Boolean = False): TAQ;
    function IntervalActorsChain(ID: Integer = 0; IncludeOrphans: Boolean = False): TAQ;
    function TimerActorsChain(ID: Integer = 0; IncludeOrphans: Boolean = False): TAQ;
    function DelayActorsChain(ID: Integer = 0; IncludeOrphans: Boolean = False): TAQ;

    function FinishAnimations(ID: Integer = 0): TAQ;
    function CancelAnimations(ID: Integer = 0): TAQ;
    function FinishTimers(ID: Integer = 0): TAQ;
    function CancelTimers(ID: Integer = 0): TAQ;
    function CancelDelays(ID: Integer = 0): TAQ;
    function CancelIntervals(ID: Integer = 0): TAQ;

    function FilterChain(ByClass: TClass): TAQ; overload;
    function FilterChain(FilterEach: TEachFunction): TAQ; overload;

    function ExcludeChain(ByClass: TClass): TAQ; overload;
    function ExcludeChain(AObject: TObject): TAQ; overload;
    function ExcludeChain(Objects: TObjectArray): TAQ; overload;
    function ExcludeChain(Objects: TObjectList): TAQ; overload;
    function ExcludeChain(AQ: TAQ): TAQ; overload;
    function ExcludeChain(ExcludeEach: TEachFunction): TAQ; overload;

    function IfThen(Condition: Boolean): TAQ;
    function IfElse: TAQ;
    function IfEnd: TAQ;

    function IfAll(EachFunction: TEachFunction): TAQ;
    function IfAny(EachFunction: TEachFunction): TAQ;

    function IfContains(AObject: TObject): TAQ;

    function IfContainsAny(ByClass: TClass): TAQ; overload;
    function IfContainsAny(Objects: TObjectArray): TAQ; overload;
    function IfContainsAny(Objects: TObjectList): TAQ; overload;
    function IfContainsAny(AQ: TAQ): TAQ; overload;

    function IfContainsAll(ByClass: TClass): TAQ; overload;
    function IfContainsAll(Objects: TObjectArray): TAQ; overload;
    function IfContainsAll(Objects: TObjectList): TAQ; overload;
    function IfContainsAll(AQ: TAQ): TAQ; overload;

    function SliceChain(StartIndex: Integer; Count: Integer = 0): TAQ;

    function DebugMessage(HeadMessage: String = ''; Caption: String = ''): TAQ;

    function Plugin<T: TAQPlugin,CONSTRUCTOR>: T;

    function Contains(AObject: TObject): Boolean;
    procedure Clean;

    property CurrentInterval: TInterval read FCurrentInterval;
  end;

  TInterval = class
  private
    FFirstTick,
    FNextTick,
    FLastTick: Int64;
    FInterval: Integer;
    FNextEach,
    FLastEach: TEachFunction;
    FActorRole: TActorRole;
    FID: Integer;
  protected
    procedure UpdateNextTick;
  public
    constructor Infinite(Interval: Integer; Each: TEachFunction; ActorRole: TActorRole;
      ID: Integer);
    constructor Finite(Duration: Integer; Each, LastEach: TEachFunction; ActorRole: TActorRole;
      ID: Integer);
    destructor Destroy; override;

    function Each: TEachFunction;
    function IsCanceled: Boolean;
    function IsFinished: Boolean;
    function IsFinite: Boolean;
    function Progress: Real;

    procedure Finish;
    procedure Cancel;

    property ActorRole: TActorRole read FActorRole;
    property ID: Integer read FID;
  end;

  TTimerThread = class(TThread)
  private
    FInterval: Integer;
    FTimerProc: TThreadProcedure;
    FMainSignal: TEvent;
    FEnabled: Boolean;
    FWindowHandle: HWND;

    procedure WndProc(var Msg: TMessage);
    procedure SetInterval(NewInterval: Integer);
  protected
    procedure Execute; override;
  public
    constructor Create(Interval: Integer; TimerProc: TThreadProcedure);
    destructor Destroy; override;

    procedure Enable;
    procedure Disable;

    {**
     * DON'T USE TERMINATE FOR THIS THREAD: USE DESTRUCTOR!
     *}
    //procedure Terminate;

    property Enabled: Boolean read FEnabled;
    property Interval: Integer read FInterval write SetInterval;
  end;

  // Shortcuts to appropriate `TAQ.Take` methods
  function Take(AObject: TObject): TAQ; overload;
  function Take(Objects: TObjectArray): TAQ; overload;
  function Take(Objects: TObjectList): TAQ; overload;
  function Take(Enumerator: TEnumerable<TObject>): TAQ; overload;

  function OA(Objects: array of TObject): TObjectArray;

  function MatchID(CompareID, CurrentID: Integer): Boolean;

const
  MaxLifeTime = 10000;

implementation

const
{$IFDEF UseThreadTimer}
  IntervalResolution = 15;
{$ELSE}
  IntervalResolution = 20;
{$ENDIF}
  GarbageCleanInterval = 5000;
  GarbageCleanTime = IntervalResolution div 2;
  SpareAQsCount = 1000;

const
  RecurseBitMask       = $01;
  ConditionLockBitMask = $02;
  ImmortallyBitMask    = $04;

var
  Initialized, Finalized: Boolean;

type
  TComponentsNotifier = class(TComponentList)
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  end;

{$IF RTLVersion < 22}
  {**
   * Cheap implementation of TSpinWait shipped since Delphi XE
   *
   * Just only to be backward compatible with Delphi 2010.
   *}
  TSpinWait = record
  public
    class function SpinUntil(const ACondition: TFunc<Boolean>; Timeout: LongWord): Boolean; static;
  end;
{$IFEND}

function LinearEase(Progress: Real): Real;
begin
  Result := Progress;
end;

function QuadEase(Progress: Real): Real;
begin
  Result := Progress * Progress;
end;

function CubicEase(Progress: Real): Real;
begin
  Result := Power(Progress, 3);
end;

function QuartEase(Progress: Real): Real;
begin
  Result := Power(Progress, 4);
end;

function QuintEase(Progress: Real): Real;
begin
  Result := Power(Progress, 5);
end;

function SextEase(Progress: Real): Real;
begin
  Result := Power(Progress, 6);
end;

function SinusEase(Progress: Real): Real;
begin
  Result := Sin(Progress * (Pi / 2));
end;

function ElasticEase(Progress: Real): Real;
begin
  Result := (Sin(Progress * Pi * (0.2 + 2.5 * Progress * Progress * Progress)) *
    Power(1 - Progress, 2.2) + Progress) * (1 + (1.2 * (1 - Progress)));
end;

function LowWaveEase(Progress: Real): Real;
begin
  Result := Progress + (Sin(Progress * 3 * Pi) * 0.1);
end;

function MiddleWaveEase(Progress: Real): Real;
begin
  Result := Progress + (Sin(Progress * 3 * Pi) * 0.2);
end;

function HighWaveEase(Progress: Real): Real;
begin
  Result := Progress + (Sin(Progress * 3 * Pi) * 0.4);
end;

function BackEase(Progress: Real): Real;
begin
  Result := Progress * Progress * ((2.70158 * Progress) - 1.70158);
end;

function BounceEase(Progress: Real): Real;
const
  Base: Real = 7.5625;
begin
  if Progress < (1 / 2.75) then
    Result := Base * Progress * Progress
  else if Progress < (2 / 2.75) then
  begin
    Progress := Progress - (1.5 / 2.75);
    Result := (Base * Progress) * Progress + 0.75;
  end
  else if Progress < (2.5 / 2.75) then
  begin
    Progress := Progress - (2.25 / 2.75);
    Result := (Base * Progress) * Progress + 0.9375;
  end
  else
  begin
    Progress := Progress - (2.625 / 2.75);
    Result := (Base * Progress) * Progress + 0.984375;
  end;
end;

function CircleEase(Progress: Real): Real;
begin
  Result := 1 - Sqrt(1 - Progress * Progress);
end;

function SwingCustom(Progress, Swings: Real): Real;
begin
  Result := Progress + (Sin(Progress * Swings * Pi) * (1 / Swings));
end;

function Swing10Ease(Progress: Real): Real;
begin
  Result := SwingCustom(Progress, 10);
end;

function Swing50Ease(Progress: Real): Real;
begin
  Result := SwingCustom(Progress, 50);
end;

function Swing100Ease(Progress: Real): Real;
begin
  Result := SwingCustom(Progress, 100);
end;

function Swing200Ease(Progress: Real): Real;
begin
  Result := SwingCustom(Progress, 200);
end;

function Take(AObject: TObject): TAQ;
begin
  Result := TAQ.Take(AObject);
end;

function Take(Objects: TObjectArray): TAQ;
begin
  Result := TAQ.Take(Objects);
end;

function Take(Objects: TObjectList): TAQ;
begin
  Result := TAQ.Take(Objects);
end;

function Take(Enumerator: TEnumerable<TObject>): TAQ;
begin
  Result := TAQ.Take(Enumerator);
end;

function OA(Objects: array of TObject): TObjectArray;
var
  cc: Integer;
begin
  SetLength(Result, Length(Objects));
  for cc := 0 to Length(Objects) - 1 do
    Result[cc] := Objects[cc];
end;

// ID comparer
//
// Following rules will be applied
// - when CompareID =  0, CurrentID will not be compared and the result is always True
// - when CompareID >  0, CurrentID must be equal
// - when CompareID = -1, CurrentID must be 0
// - when CompareID = -2, CurrentID must be greater than 0
function MatchID(CompareID, CurrentID: Integer): Boolean;
begin
  Result := (CompareID = 0) or
    ((CompareID > 0) and (CompareID = CurrentID)) or
    ((CompareID = -1) and (CurrentID = 0)) or
    ((CompareID = -2) and (CurrentID > 0));
end;

procedure SetBit(var Container: Byte; BitMask: Byte; Value: Boolean);
begin
  if Value then
    Container := Container or BitMask
  else
    Container := Container and not BitMask;
end;

function GetBit(Container: Byte; BitMask: Byte): Boolean;
begin
  Result := (Container and BitMask) <> 0;
end;

{ TAQBase }

constructor TAQBase.Create;
begin
  inherited Create(False);
end;

{ TAQ }

// Appends all in the TObjectArray contained objects to the current TAQ instance
function TAQ.Append(Objects: TObjectArray): TAQ;
var
  cc: Integer;
begin
  if SupervisorLock(Result, aqmAppend) then
    Exit;
  for cc := 0 to Length(Objects) - 1 do
    Add(Objects[cc]);
end;

// Appends all in Objects contained objects, but not the TObjectList by itself,
// to the current TAQ instance
function TAQ.Append(Objects: TObjectList): TAQ;
var
  cc: Integer;
begin
  if SupervisorLock(Result, aqmAppend) then
    Exit;
  // Avoid overflows
  if Objects = Self then
    Exit;
  for cc := 0 to Objects.Count - 1 do
    Add(Objects[cc]);
end;

// Appends AObject to the current TAQ instance
function TAQ.Append(AObject: TObject): TAQ;
begin
  if SupervisorLock(Result, aqmAppend) then
    Exit;
  Add(AObject);
end;

// Appends an other TAQ instance to the current one
//
// This method is handy for recursive tasks. For example you have already some TAQ instances with
// some objects and want to perform an action on all of them:
// ```delphi
// Take(MyInstanceA).AppendAQ(MyInstanceB).AppendAQ(MyInstanceC).Each({...Your action...});
// ```
//
// In contrast to `TAQ.Append` it appends just the passed TAQ instance to the current instance
// instead of the contained object.
function TAQ.AppendAQ(AQ: TAQ): TAQ;
begin
  if SupervisorLock(Result, aqmAppendAQ) then
    Exit;
  Add(AQ);
end;

procedure TAQ.AddInterval(Interval: TInterval);
begin
  // The interval will not be taken and will be freed, when the current TAQ instance
  // do not contains any objects.
  //
  // One exception is for the garbage collector.
  if (Count = 0) and (Self <> FGC) then
  begin
    Interval.Free;
    Exit;
  end;

  if (GetIntervals.Count = 0) and (FActiveIntervalAQs.IndexOf(Self) = -1) then
    FActiveIntervalAQs.Add(Self);
  GetIntervals.Add(Interval);
  {$IFDEF OutputDebugActiveIntervals}
    OutputDebugString(PWideChar('TAQ-Instanzen mit Intervallen: ' +
      IntToStr(FActiveIntervalAQs.Count)));
  {$ENDIF}
end;

function TAQ.AnimationActorsChain(ID: Integer; IncludeOrphans: Boolean): TAQ;
begin
  if SupervisorLock(Result, aqmAnimationActorsChain) then
    Exit;
  Result := CustomActors(arAnimation, ID, IncludeOrphans);
end;

function TAQ.CancelAnimations(ID: Integer): TAQ;
begin
  if SupervisorLock(Result, aqmCancelAnimations) then
    Exit;
  CustomCancel(arAnimation, ID, False);
end;

function TAQ.CancelDelays(ID: Integer): TAQ;
begin
  if SupervisorLock(Result, aqmCancelDelays) then
    Exit;
  CustomCancel(arDelay, ID, False);
end;

function TAQ.CancelIntervals(ID: Integer): TAQ;
begin
  if SupervisorLock(Result, aqmCancelIntervals) then
    Exit;
  CustomCancel(arInterval, ID, False);
end;

function TAQ.CancelTimers(ID: Integer): TAQ;
begin
  if SupervisorLock(Result, aqmCancelTimers) then
    Exit;
  CustomCancel(arTimer, ID, False);
end;

function TAQ.NewChain: TAQ;
begin
  if SupervisorLock(Result, aqmNewChain) then
    Exit;
  Result := Managed;
  Result.FChainedTo := Self;
end;

procedure TAQ.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if (Action = lnAdded) and (TObject(Ptr) is TComponent) and
    (FComponentsNotifier.IndexOf(TComponent(Ptr)) < 0) then
    FComponentsNotifier.Add(TComponent(Ptr));

  inherited Notify(Ptr, Action);

  if (Action in [lnExtracted, lnDeleted]) and (Count = 0) then
  begin
    Clean;
    Die;
  end;
end;

function TAQ.ChildrenAppend(Recurse: Boolean; ChildrenFiller: TEachFunction): TAQ;
begin
  if SupervisorLock(Result, aqmChildrenAppend) then
    Exit;
  if not Assigned(ChildrenFiller) then
    ChildrenFiller := Self.ChildrenFiller;
  Result := CustomFiller(ChildrenFiller, True, Recurse);
end;

function TAQ.ChildrenChain(Recurse: Boolean; ChildrenFiller: TEachFunction): TAQ;
begin
  if SupervisorLock(Result, aqmChildrenChain) then
    Exit;
  if not Assigned(ChildrenFiller) then
    ChildrenFiller := Self.ChildrenFiller;
  Result := CustomFiller(ChildrenFiller, False, Recurse);
end;

function TAQ.ChildrenFiller(AQ: TAQ; O: TObject): Boolean;
var
  cc: Integer;
  OC: TComponent absolute O;
begin
  Result := True;
  if O is TComponent then
    for cc := 0 to OC.ComponentCount - 1 do
      AQ.Add(OC.Components[cc]);
end;

procedure TAQ.Clean;
begin
  if not Finalized then
  begin
    // Global effects
    GarbageCollector.Each(
      function(GC: TAQ; O: TObject): Boolean
      begin
        // When any plugin is connected with this instance, so it must be freed
        if (O is TAQPlugin) and (TAQPlugin(O).WorkAQ = Self) then
          GC.Remove(O)
        // If this instance is chained with an other, so it must be "unchained"
        else if (O is TAQ) and (TAQ(O).FChainedTo = Self) then
          TAQ(O).FChainedTo := nil;
        Result := True; // Full scan
      end);
  end;

  Clear;
  FConditionCount := 0;
  FBools := 0;
  Recurse := True;
  FCurrentInterval := nil;
  FChainedTo := nil;

  if Assigned(FIntervals) then
  begin
    ClearIntervals;
    FreeAndNil(FIntervals);
  end;
end;

procedure TAQ.ClearIntervals;
var
  cc: Integer;
begin
  for cc := FIntervals.Count - 1 downto 0 do
    RemoveInterval(TInterval(FIntervals[cc]));
end;

class procedure TAQ.ComponentsNotification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and not Finalized then
    // The removed component must be removed from all living TAQ instances
    GarbageCollector.Each(
      function(GC: TAQ; O: TObject): Boolean
      begin
        if TAQ(O).IsAlive then
          TAQ(O).Remove(AComponent);
        Result := True;
      end);
end;

function TAQ.Contains(AObject: TObject): Boolean;
var
  Found: Boolean;
begin
  Found := False;
  Each(
    function(AQ: TAQ; O: TObject): Boolean
    begin
      Found := Found or (O = AObject);
      Result := not Found;
    end);
  Result := Found;
end;

constructor TAQ.Create;
begin
  inherited Create;
  FConditionCount := 0;
  FBools := 0;
  Recurse := True;
end;

function TAQ.CustomActors(ActorRole: TActorRole; ID: Integer; IncludeOrphans: Boolean): TAQ;
var
  Actors: TAQ;
begin
  Actors := NewChain;
  if not Assigned(FActiveIntervalAQs) then
    Exit(Actors);
  Each(
    {**
     * @param SAQ Synonym für SourceAQ und ist Self von CustomActors
     * @param SO Synonym für SourceObject und beinhaltet das Objekt für das die passenden
     *        TAQ-Instanzen gesucht werden
     *}
    function(SAQ: TAQ; SO: TObject): Boolean
    var
      SOFound: Boolean;
    begin
      Result := True; // Each soll stets komplett durchlaufen
      SOFound := False;
      FActiveIntervalAQs.Each(
        {**
         * @param AQ Enthält FActiveIntervalAQs
         * @param O Enthält eine TAQ-Instanz, die darauf untersucht wird, ob sie SO enthält
         *        einen aktiven Timer hat und gerade animiert wird
         *}
        function(AQ: TAQ; O: TObject): Boolean
        var
          TargetAQ: TAQ;
        begin
          Result := True; // Each soll stets komplett durchlaufen
          {**
           * Der Garbage-Collector darf hier nicht berücksichtigt werden
           *}
          if O = FGC then
            Exit;
          TargetAQ := TAQ(O);
          if TargetAQ.HasActors(ActorRole, ID) and TargetAQ.Contains(SO) then
          begin
            Actors.Add(O);
            SOFound := True;
          end;
        end);
      if IncludeOrphans and not SOFound then
        Actors.Add(SO);
    end);
  Result := Actors;
end;

procedure TAQ.CustomCancel(ActorRole: TActorRole; ID: Integer; Finish: Boolean);
var
  Perform: TEachFunction;
begin
  Perform := function(AQ: TAQ; O: TObject): Boolean
  var
    cc: Integer;
    TempAQ: TAQ absolute O;
    CI: TInterval; // Shortcut for CurrentInterval
  begin
    Result := True; // Performed Each should go through completely
    if not (O is TAQ) then
      Exit;

    if not Assigned(TempAQ.FIntervals) then
      Exit;

    for cc := TempAQ.FIntervals.Count - 1 downto 0 do
    begin
      CI := TInterval(TempAQ.FIntervals[cc]);

      if (CI.ActorRole = ActorRole) and ((ID = 0) or (ID = CI.ID)) then
      begin
        if Finish then
          CI.Finish
        else
          CI.Cancel;
        TempAQ.ProcessInterval(CI);
      end;
    end;
  end;

  if not Assigned(FActiveIntervalAQs) then
    Exit;

  // Cancelation could only subjected to active TAQ instances
  FActiveIntervalAQs
    .Each(
      function(GC: TAQ; Target: TObject): Boolean
      var
        TargetAQ: TAQ absolute Target;
      begin
        Result := True; // Full Each scan
        if Target = FGC then
          Exit;

        Each(
          {**
           * @param AQ Enthält Self aus dem selben Kontext, wie die CustomCancel-Methode
           * @param O Enthält ein Object aus Self
           *}
          function(AQ: TAQ; O: TObject): Boolean
          begin
            Result := True;
            if O is TAQ then
              Exit;
            if TargetAQ.IndexOf(O) >= 0 then
            begin
              TargetAQ.Each(Perform);
              Perform(nil, TargetAQ);
              Result := False; // Only one occurence required, so break the Each
            end;
          end);
      end);

  // Very important reset, because this closure is used nested in previous
  // FActiveIntervalAQs.Each call and otherwise it's not released -> produces memory leaks.
  Perform := nil;
end;

function TAQ.CustomFiller(Filler: TEachFunction; Append, Recurse: Boolean): TAQ;
var
  TargetAQ: TAQ;

  procedure Each(SourceAQ: TAQ);
  var
    TempAQ: TAQ;
  begin
    TempAQ := Unmanaged;
    try
      SourceAQ.Each(
        function(AQ: TAQ; O: TObject): Boolean
        begin
          Result := Filler(TempAQ, O);
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
    TargetAQ := Self
  else
    TargetAQ := NewChain;
  Each(Self);
  Result := TargetAQ;
end;

function TAQ.DebugMessage(HeadMessage: String = ''; Caption: String = ''): TAQ;
{$IFDEF DEBUG}
var
  ChainPath: TStringList;
  cc: Integer;
  IntervalsCount: Integer;
  WholeMessage: String;
{$ENDIF}
begin
  {$IFNDEF DEBUG}
  Exit(Self);
  {$ELSE}
  if SupervisorLock(Result, aqmDebugMessage) then
    Exit;
  ChainPath := TStringList.Create;
  repeat
    IntervalsCount := 0;
    if Assigned(Result.FIntervals) then
      IntervalsCount := Result.FIntervals.Count;
    ChainPath.Add(Format('(Objects: %d, Intervals: %d, ConditionCount: %d)',
      [Result.Count, IntervalsCount, Result.FConditionCount]));
    Result := Result.FChainedTo;
  until not Assigned(Result);

  for cc := 0 to ChainPath.Count - 1 do
    ChainPath[cc] := Format('Chain #%d - %s' , [ChainPath.Count - cc, ChainPath[cc]]);

  if Caption <> '' then
    Caption := Caption + ' - ';
  Caption := Caption + 'Chain-Debug';

  WholeMessage := ChainPath.Text;
  if HeadMessage <> '' then
    WholeMessage := HeadMessage + #10#13 + '-------------------------------' + #10#13 +
      WholeMessage;
  MessageBox(0, PWideChar(WholeMessage), PWideChar(Caption), MB_OK or MB_ICONINFORMATION);
//	OutputDebugString(PWideChar(WholeMessage)); // Wer keine Boxen mag, kann die Console für die Ausgabe nutzen
  Result := Self; // Wichtig, da der richtige Result in der oberen Schleife überschrieben wird
  {$ENDIF}
end;

function TAQ.DelayActorsChain(ID: Integer; IncludeOrphans: Boolean): TAQ;
begin
  if SupervisorLock(Result, aqmDelayActorsChain) then
    Exit;
  Result := CustomActors(arDelay, ID, IncludeOrphans);
end;

destructor TAQ.Destroy;
begin
  Clean;
  inherited Destroy;
end;

// Try to let die the current TAQ instance
//
// It is impossible to die if there are any active intervals, such as animations, delays etc.
// The main reason for this method is to mark an *intermediate* TAQ instance as died,
// so it can be faster reused from the garbage collector. Under special conditions this
// can you save a lot of memory and unnecessary garbage clean ups.
function TAQ.Die: TAQ;
begin
  if SupervisorLock(Result, aqmDie) then
    Exit;
  if Assigned(FIntervals) and (FIntervals.Count > 0) then
    Exit;
  FLifeTick := 0;
end;

// Performs the passed method/closure on each in TAQ contained object
//
// Each is the core method of AnyiQuack. Although its implementation looks simple, it is very
// powerful.
// The passed EachFunction gets as first paramater the processing TAQ instance (Self) and
// in the second parameter the subject object. You are able to break the Each from inside of
// EachFunction by returning False. Otherwise you have to return True for further/full processing.
// If the current TAQ instance contains other TAQ instances (appended by `TAQ.AppendAQ`) and
// `TAQ.Recurse` is True (Default) EachFunction is also performed on them.
function TAQ.Each(EachFunction: TEachFunction): TAQ;
var
  cc: Integer;
  O: TObject;
  LocalRecurse: Boolean;
begin
  if SupervisorLock(Result, aqmEach) then
    Exit;
  cc := Count;
  LocalRecurse := Recurse;
  while cc > 0 do
  begin
    O := Items[Count - cc];
    if LocalRecurse and (O is TAQ) then
      TAQ(O).Each(EachFunction);
    if not EachFunction(Self, O) then
      Break
    else if cc > Count then
      cc := Count + 1;
    Dec(cc);
  end;
end;

function TAQ.EachAnimation(Duration: Integer; Each, LastEach: TEachFunction; ID: Integer): TAQ;
begin
  if Duration >= MaxLifeTime then
    raise EAQ.CreateFmt('Duration of the animation (%d) should be lower than MaxLifeTime (%d)',
      [Duration, MaxLifeTime])
  else if SupervisorLock(Result, aqmEachAnimation) then
    Exit;
  AddInterval(TInterval.Finite(Duration, Each, LastEach, arAnimation, ID));
end;

function TAQ.EachDelay(Delay: Integer; Each: TEachFunction; ID: Integer): TAQ;
begin
  if Delay >= MaxLifeTime then
    raise EAQ.CreateFmt('Delay (%d) must be lower than MaxLifeTime (%d)',
      [Delay, MaxLifeTime])
  else if SupervisorLock(Result, aqmEachDelay) then
     Exit;
  AddInterval(TInterval.Finite(Delay, nil, Each, arDelay, ID));
end;

// Performs the passed method/closure on each in TAQ contained object each interval repeatedly
//
// The Each is called each interval until you cancel it with `TAQ.CancelIntervals`.
// Unit of Interval is millisecond. If you consider to perform several EachInterval's on the same
// objects, you should define an ID, so you'll be able to cancel only the specific intervaled Each.
function TAQ.EachInterval(Interval: Integer; Each: TEachFunction; ID: Integer): TAQ;
begin
  if Interval >= MaxLifeTime then
    raise EAQ.CreateFmt('Interval (%d) must be lower than MaxLifeTime (%d)',
      [Interval, MaxLifeTime])
  else if SupervisorLock(Result, aqmEachInterval) then
    Exit;
  AddInterval(TInterval.Infinite(Interval, Each, arInterval, ID));
end;

// Performs the passed method/closure on each in TAQ contained object x times
function TAQ.EachRepeat(Times: Integer; EachFunction: TEachFunction): TAQ;
var
  cc: Integer;
begin
  if SupervisorLock(Result, aqmEachRepeat) then
    Exit;
  for cc := 1 to Times do
    Each(EachFunction);
end;

// Performs the passed methods/closures on each in TAQ contained object as long as the timer
// runs (parameter Each) and once when the timer expires (parameter LastEach)
//
// From the performing Each function you can access to the associated interval with
// `AQ.CurrentInterval` and use further runtime specific informations
// e.g. `AQ.CurrentInterval.Progress`
function TAQ.EachTimer(Duration: Integer; Each, LastEach: TEachFunction; ID: Integer): TAQ;
begin
  if Duration >= MaxLifeTime then
    raise EAQ.CreateFmt('Timers duration (%d) must be lower than MaxLifeTime (%d)',
      [Duration, MaxLifeTime])
  else if SupervisorLock(Result, aqmEachTimer) then
    Exit;
  AddInterval(TInterval.Finite(Duration, Each, LastEach, arTimer, ID));
end;

function TAQ.IfEnd: TAQ;
begin
  if SupervisorLock(Result, aqmIfEnd) then
    Exit;
  if FConditionCount > 0 then
    Dec(FConditionCount);
  if FConditionCount = 0 then
  begin
    ConditionLock := False;
    Result := EndChain;
  end;
end;

function TAQ.ExcludeChain(Objects: TObjectArray): TAQ;
begin
  if SupervisorLock(Result, aqmExcludeChain) then
    Exit;
  Result := ExcludeChain(
    function(AQ: TAQ; O: TObject): Boolean
    var
      cc: Integer;
    begin
      for cc := 0 to Length(Objects) - 1 do
        if Objects[cc] = O then
          Exit(True);
      Result := False;
    end);
end;

function TAQ.ExcludeChain(AObject: TObject): TAQ;
begin
  if SupervisorLock(Result, aqmExcludeChain) then
    Exit;
  Result := ExcludeChain(
    function(AQ: TAQ; O: TObject): Boolean
    begin
      Result := O = AObject;
    end);
end;

function TAQ.ExcludeChain(ByClass: TClass): TAQ;
begin
  if SupervisorLock(Result, aqmExcludeChain) then
    Exit;
  Result := ExcludeChain(
    function(AQ: TAQ; O: TObject): Boolean
    begin
      Result := O is ByClass;
    end);
end;

function TAQ.ExcludeChain(ExcludeEach: TEachFunction): TAQ;
var
  NewAQ: TAQ;
begin
  if SupervisorLock(Result, aqmExcludeChain) then
    Exit;
  NewAQ := NewChain;
  Each(
    function(AQ: TAQ; O: TObject): Boolean
    begin
      if not ExcludeEach(AQ, O) then
        NewAQ.Add(O);
      Result := True;
    end);
  Result := NewAQ;
end;

function TAQ.ExcludeChain(AQ: TAQ): TAQ;
begin
  if SupervisorLock(Result, aqmExcludeChain) then
    Exit;
  Result := ExcludeChain(
    function(OAQ: TAQ; O: TObject): Boolean
    begin
      Result := AQ.Contains(O);
    end);
end;

function TAQ.ExcludeChain(Objects: TObjectList): TAQ;
begin
  if SupervisorLock(Result, aqmExcludeChain) then
    Exit;
  Result := ExcludeChain(
    function(AQ: TAQ; O: TObject): Boolean
    begin
      Result := Objects.IndexOf(O) >= 0;
    end);
end;

class function TAQ.GetUniqueID: Integer;
begin
  Result := AtomicIncrement(FIDGenerator);
end;

class function TAQ.EaseIntegrated(EaseType: TEaseType): TEaseFunction;
begin
  case EaseType of
    etQuad:
      Result := QuadEase;
    etCubic:
      Result := CubicEase;
    etQuart:
      Result := QuartEase;
    etQuint:
      Result := QuintEase;
    etSext:
      Result := SextEase;
    etSinus:
      Result := SinusEase;
    etElastic:
      Result := ElasticEase;
    etLowWave:
      Result := LowWaveEase;
    etMiddleWave:
      Result := MiddleWaveEase;
    etHighWave:
      Result := HighWaveEase;
    etBack:
      Result := BackEase;
    etBounce:
      Result := BounceEase;
    etCircle:
      Result := CircleEase;
    etSwing10:
      Result := Swing10Ease;
    etSwing50:
      Result := Swing50Ease;
    etSwing100:
      Result := Swing100Ease;
    etSwing200:
      Result := Swing200Ease;
  else
    Result := LinearEase;
  end;
end;

class function TAQ.Ease(EaseType: TEaseType; EaseModifier: TEaseModifier = emIn): TEaseFunction;
begin
  Result := Ease(EaseIntegrated(EaseType), EaseModifier);
end;

class function TAQ.Ease(const EaseTypes: array of TEaseType; EaseModifier: TEaseModifier): TEaseFunction;
var
  LocalEaseTypes: TEaseArray;
  cc: Integer;
begin
  if Length(EaseTypes) = 1 then
    Result := Ease(EaseIntegrated(EaseTypes[0]), EaseModifier)
  else
  begin
    SetLength(LocalEaseTypes, Length(EaseTypes));
    for cc := 0 to Length(EaseTypes) - 1 do
      LocalEaseTypes[cc] := EaseTypes[cc];

    Result := Ease(
      function(Progress: Real): Real
      var
        cc: Integer;
        Scale: Real;
      begin
        Result := 0;
        Scale := 1 / Length(LocalEaseTypes);
        for cc := 0 to Length(LocalEaseTypes) - 1 do
          Result := Result + (Scale * EaseIntegrated(LocalEaseTypes[cc])(Progress));
      end, EaseModifier);
  end;
end;

class function TAQ.Ease(EaseFunction: TEaseFunction; EaseModifier: TEaseModifier): TEaseFunction;
begin
  if not Assigned(EaseFunction) then
    EaseFunction := LinearEase;

  case EaseModifier of
    emIn:
      Result := EaseFunction;
    emOut:
      Result := function(Progress: Real): Real
      begin
        Result := EaseFunction(1 - Progress);
      end;
    emInOut:
      Result := function(Progress: Real): Real
      begin
        if Progress <= 0.5 then
          Progress := Progress / 0.5
        else
          Progress := 1 - ((Progress - 0.5) / 0.5);
        Result := EaseFunction(Progress);
      end;
    emInInverted:
      Result := function(Progress: Real): Real
      begin
        Result := 1 - EaseFunction(1 - Progress);
      end;
    emOutInverted:
      Result := function(Progress: Real): Real
      begin
        Result := 1 - EaseFunction(Progress);
      end;
    emOutIn:
      Result := function(Progress: Real): Real
      begin
        if Progress <= 0.5 then
          Progress := Progress / 0.5
        else
          Progress := 1 - ((Progress - 0.5) / 0.5);
        Result := EaseFunction(1 - Progress);
      end;
    emInOutMirrored:
      Result := function(Progress: Real): Real
      begin
        if Progress <= 0.5 then
          Progress := Progress / 0.5
        else
          Progress := 1 - ((Progress - 0.5) / 0.5);
        Result := 1 - EaseFunction(1 - Progress);
      end;
    emOutInMirrored:
      Result := function(Progress: Real): Real
      begin
        if Progress <= 0.5 then
          Progress := Progress / 0.5
        else
          Progress := 1 - ((Progress - 0.5) / 0.5);
        Result := 1 - EaseFunction(Progress);
      end;
    emInOutCombined:
      Result := function(Progress: Real): Real
      begin
        if Progress <= 0.5 then
          Result := EaseFunction(Progress / 0.5)
        else
          Result := 1 - EaseFunction((Progress - 0.5) / 0.5);
      end;
    emOutInCombined:
      Result := function(Progress: Real): Real
      begin
        if Progress <= 0.5 then
          Result := EaseFunction(1 - (Progress / 0.5))
        else
          Result := 1 - EaseFunction(1 - ((Progress - 0.5) / 0.5));
      end;
    emInSnake:
      Result := function(Progress: Real): Real
      begin
        if Progress <= 0.5 then
          Result := EaseFunction(Progress / 0.5)
        else
          Result := EaseFunction(1) + (1 - EaseFunction(1 - ((Progress - 0.5) / 0.5)));
        Result := Result / 2;
      end;
    emOutSnake:
      Result := function(Progress: Real): Real
      begin
        if Progress <= 0.5 then
          Result := 1 + (1 - EaseFunction(Progress / 0.5))
        else
          Result := EaseFunction(1 - ((Progress - 0.5) / 0.5));
        Result := Result / 2;
      end;
    emInSnakeInverted:
      Result := function(Progress: Real): Real
      begin
        if Progress <= 0.5 then
          Result := 1 - EaseFunction(1 - (Progress / 0.5))
        else
          Result := EaseFunction(1) + EaseFunction((Progress - 0.5) / 0.5);
        Result := Result / 2;
      end;
    emOutSnakeInverted:
      Result := function(Progress: Real): Real
      begin
        if Progress <= 0.5 then
          Result := 1 + EaseFunction(1 - Progress / 0.5)
        else
          Result := 1 - EaseFunction((Progress - 0.5) / 0.5);
        Result := Result / 2;
      end;
  end;
end;

class function TAQ.EaseColor(StartColor, EndColor: TColor; Progress: Real; EaseType: TEaseType;
  EaseModifier: TEaseModifier): TColor;
begin
  Result := EaseColor(StartColor, EndColor, Progress, Ease(EaseType, EaseModifier));
end;

class function TAQ.EaseColor(StartColor, EndColor: TColor; Progress: Real;
  EaseFunction: TEaseFunction): TColor;
var
  StartCR, EndCR: TColorRec;
begin
  if StartColor = EndColor then
    Exit(StartColor);

  StartCR.Color := TColorRec.ColorToRGB(StartColor);
  EndCR.Color := TColorRec.ColorToRGB(EndColor);
  Progress := EaseFunction(Progress);

  Result := RGB(
    Min(255, Max(0, EaseInteger(StartCR.R, EndCR.R, Progress, nil))),
    Min(255, Max(0, EaseInteger(StartCR.G, EndCR.G, Progress, nil))),
    Min(255, Max(0, EaseInteger(StartCR.B, EndCR.B, Progress, nil))));
end;

class function TAQ.EaseInteger(StartValue, EndValue: Integer; Progress: Real;
  EaseFunction: TEaseFunction): Integer;
begin
  if Assigned(EaseFunction) then
    Progress := EaseFunction(Progress);
  Result := Round(StartValue + ((EndValue - StartValue) * Progress));
end;

class function TAQ.EasePoint(StartPoint, EndPoint: TPoint; Progress: Real;
  EaseFunction: TEaseFunction): TPoint;
begin
  if Assigned(EaseFunction) then
    Progress := EaseFunction(Progress);
  Result := Point(
    EaseInteger(StartPoint.X, EndPoint.X, Progress, nil),
    EaseInteger(StartPoint.Y, EndPoint.Y, Progress, nil));
end;

class function TAQ.EasePoint(StartPoint, EndPoint: TPoint; Progress: Real; EaseType: TEaseType;
  EaseModifier: TEaseModifier): TPoint;
begin
  Result := EasePoint(StartPoint, EndPoint, Progress, Ease(EaseType, EaseModifier));
end;

class function TAQ.EaseInteger(StartValue, EndValue: Integer; Progress: Real; EaseType: TEaseType;
  EaseModifier: TEaseModifier): Integer;
begin
  Result := EaseInteger(StartValue, EndValue, Progress, Ease(EaseType, EaseModifier));
end;

class function TAQ.EaseReal(StartValue, EndValue, Progress: Real; EaseFunction: TEaseFunction): Real;
begin
  if Assigned(EaseFunction) then
    Progress := EaseFunction(Progress);
  Result := StartValue + ((EndValue - StartValue) * Progress);
end;

class function TAQ.EaseRect(StartRect, EndRect: TRect; Progress: Real;
  EaseFunction: TEaseFunction): TRect;
begin
  if Assigned(EaseFunction) then
    Progress := EaseFunction(Progress);
  Result := Rect(
    EasePoint(StartRect.TopLeft, EndRect.TopLeft, Progress, nil),
    EasePoint(StartRect.BottomRight, EndRect.BottomRight, Progress, nil));
end;

class function TAQ.EaseString(StartString, EndString: String; Progress: Real; EaseType: TEaseType;
  EaseModifier: TEaseModifier): String;
begin
  Result := EaseString(StartString, EndString, Progress, Ease(EaseType, EaseModifier));
end;

class function TAQ.EaseString(StartString, EndString: String; Progress: Real;
  EaseFunction: TEaseFunction): String;
var
  StartStringLength, EndStringLength, EasedStringLength: Integer;
  StartChar, EndChar, EasedChar: Char;
  cc: Integer;
begin
  if Assigned(EaseFunction) then
    Progress := EaseFunction(Progress);
  StartStringLength := Length(StartString);
  EndStringLength := Length(EndString);
  EasedStringLength := EaseInteger(StartStringLength, EndStringLength, Progress, nil);
  Result := '';
  for cc := 1 to EasedStringLength do
  begin
    if cc <= StartStringLength then
      StartChar := Copy(StartString, cc, 1)[1]
    else
      StartChar := ' ';
    if cc <= EndStringLength then
      EndChar := Copy(EndString, cc, 1)[1]
    else
      EndChar := ' ';
    if StartChar <> EndChar then
    begin
      EasedChar := WideChar(EaseInteger(Integer(StartChar), Integer(EndChar), Progress, nil));
      if EasedChar.IsControl then
        EasedChar := '-';
      Result := Result + EasedChar;
    end
    else
      Result := Result + StartChar;
  end;
end;

class function TAQ.EaseRect(StartRect, EndRect: TRect; Progress: Real; EaseType: TEaseType;
  EaseModifier: TEaseModifier): TRect;
begin
  Result := EaseRect(StartRect, EndRect, Progress, Ease(EaseType, EaseModifier));
end;

class function TAQ.EaseReal(StartValue, EndValue, Progress: Real; EaseType: TEaseType;
  EaseModifier: TEaseModifier): Real;
begin
  Result := EaseReal(StartValue, EndValue, Progress, Ease(EaseType, EaseModifier));
end;

function TAQ.FilterChain(ByClass: TClass): TAQ;
begin
  if SupervisorLock(Result, aqmFilterChain) then
    Exit;
  Result := FilterChain(
    function(AQ: TAQ; O: TObject): Boolean
    begin
      Result := O is ByClass;
    end);
end;

function TAQ.FilterChain(FilterEach: TEachFunction): TAQ;
var
  NewAQ: TAQ;
begin
  if SupervisorLock(Result, aqmFilterChain) then
    Exit;
  NewAQ := NewChain;
  Each(
    function(OAQ: TAQ; OO: TObject): Boolean
    begin
      Result := True;
      if FilterEach(OAQ, OO) then
        NewAQ.Add(OO);
    end);
  Result := NewAQ;
end;

// Finishes all or specific animations
//
// This affects all currently running animations and not only for the contained objects, if you
// pass 0 (default) for the ID parameter.
// But you can finish specific animations, if you previously has defined an ID for the desired
// animation and now pass it for the ID parameter.
function TAQ.FinishAnimations(ID: Integer): TAQ;
begin
  if SupervisorLock(Result, aqmFinishAnimations) then
    Exit;
  CustomCancel(arAnimation, ID, True);
end;

// Finishes all or specific timers
//
// This affects all currently running timers and not only for the contained objects, if you
// pass 0 (default) for the ID parameter.
// But you can finish specific timers, if you previously has defined an ID for the desired
// timer and now pass it for the ID parameter.
function TAQ.FinishTimers(ID: Integer): TAQ;
begin
  if SupervisorLock(Result, aqmFinishTimers) then
    Exit;
  CustomCancel(arTimer, ID, True);
end;

function TAQ.GetConditionLock: Boolean;
begin
  Result := GetBit(FBools, ConditionLockBitMask);
end;

function TAQ.GetImmortally: Boolean;
begin
  Result := GetBit(FBools, ImmortallyBitMask);
end;

function TAQ.GetIntervals: TObjectList;
begin
  if not Assigned(FIntervals) then
    FIntervals := TObjectList.Create(True);
  Result := FIntervals;
end;

function TAQ.GetRecurse: Boolean;
begin
  Result := GetBit(FBools, RecurseBitMask);
end;

function TAQ.HasActors(ActorRole: TActorRole; ID: Integer): Boolean;
var
  AnyActors: Boolean;
begin
  Result := False;
  if not (Assigned(FIntervals) and (FIntervals.Count > 0)) then
    Exit;
  AnyActors := False;
  TAQ.Take(FIntervals)
    .Each(
      function(AQ: TAQ; O: TObject): Boolean
      begin
        AnyActors := (TInterval(O).ActorRole = ActorRole) and
          ((ID = 0) or (TInterval(O).ID = ID)) and not TInterval(O).IsFinished;
        // Let run the Each until we have found our first Actor
        Result := not AnyActors;
      end)
    .Die;
  Result := AnyActors;
end;

procedure TAQ.HeartBeat;

  procedure HeartBeatEcho(AQ: TAQ);
  var
    cc: Integer;
  begin
    for cc := 0 to AQ.Count - 1 do
      if Assigned(AQ[cc]) and (AQ[cc] is TAQ) then
        TAQ(AQ[cc]).HeartBeat;
  end;

begin
  FLifeTick := TAQ.Tick;
  // The heartbeat must be routed to the contained TAQ instances if the `TAQ.Recurse` property
  // is True. By default are TAQ instances recurse.
  // Except: `TAQ.FGarbageCollector`
  if Recurse then
    HeartBeatEcho(Self);
end;

function TAQ.IfAll(EachFunction: TEachFunction): TAQ;
var
  Condition: Boolean;
begin
  if SupervisorLock(Result, aqmIfAll) then
    Exit;
  Condition := Count > 0;
  Each(
    function(AQ: TAQ; O: TObject): Boolean
    begin
      Condition := Condition and EachFunction(AQ, O);
      Result := Condition;
    end);
  Result := IfThen(Condition);
end;

function TAQ.IfAny(EachFunction: TEachFunction): TAQ;
var
  Condition: Boolean;
begin
  if SupervisorLock(Result, aqmIfAny) then
    Exit;
  Condition := False;
  Each(
    function(AQ: TAQ; O: TObject): Boolean
    begin
      Condition := Condition or EachFunction(AQ, O);
      Result := not Condition;
    end);
  Result := IfThen(Condition);
end;

function TAQ.IfContainsAll(Objects: TObjectList): TAQ;
begin
  if SupervisorLock(Result, aqmIfContainsAll) then
    Exit;
  Result := IfAll(IfContainsEach(Objects));
end;

function TAQ.IfContainsAll(AQ: TAQ): TAQ;
begin
  if SupervisorLock(Result, aqmIfContainsAll) then
    Exit;
  Result := IfAll(IfContainsEach(AQ));
end;

function TAQ.IfContainsAll(Objects: TObjectArray): TAQ;
begin
  if SupervisorLock(Result, aqmIfContainsAll) then
    Exit;
  Result := IfAll(IfContainsEach(Objects));
end;

function TAQ.IfContainsAll(ByClass: TClass): TAQ;
begin
  if SupervisorLock(Result, aqmIfContainsAll) then
    Exit;
  Result := IfAll(IfContainsEach(ByClass));
end;

function TAQ.IfContainsAny(Objects: TObjectList): TAQ;
begin
  if SupervisorLock(Result, aqmIfContainsAny) then
    Exit;
  Result := IfAny(IfContainsEach(Objects));
end;

function TAQ.IfContainsAny(AQ: TAQ): TAQ;
begin
  if SupervisorLock(Result, aqmIfContainsAny) then
    Exit;
  Result := IfAny(IfContainsEach(AQ));
end;

function TAQ.IfContainsAny(Objects: TObjectArray): TAQ;
begin
  if SupervisorLock(Result, aqmIfContainsAny) then
    Exit;
  Result := IfAny(IfContainsEach(Objects));
end;

function TAQ.IfContainsAny(ByClass: TClass): TAQ;
begin
  if SupervisorLock(Result, aqmIfContainsAny) then
    Exit;
  Result := IfAny(IfContainsEach(ByClass));
end;

function TAQ.IfContains(AObject: TObject): TAQ;
begin
  if SupervisorLock(Result, aqmIfContains) then
    Exit;
  Result := IfThen(Contains(AObject));
end;

function TAQ.IfContainsEach(ByClass: TClass): TEachFunction;
begin
  Result := function(AQ: TAQ; O: TObject): Boolean
  begin
    Result := O is ByClass;
  end;
end;

function TAQ.IfContainsEach(Objects: TObjectArray): TEachFunction;
begin
  Result := function(AQ: TAQ; O: TObject): Boolean
  var
    cc: Integer;
  begin
    Result := False;
    for cc := 0 to Length(Objects) - 1 do
      if O = Objects[cc] then
        Exit(True);
  end;
end;

function TAQ.IfContainsEach(AQ: TAQ): TEachFunction;
begin
  Result := function(SourceAQ: TAQ; O: TObject): Boolean
  begin
    Result := AQ.Contains(O);
  end;
end;

function TAQ.IfElse: TAQ;
var
  PrevChain: TAQ;
begin
  if SupervisorLock(Result, aqmIfElse) then
    Exit;
  if FConditionCount <= 1 then
  begin
    PrevChain := IfEnd;
    Result := PrevChain.IfThen((PrevChain.Count > 0) and (Count = 0));
  end;
end;

function TAQ.IfContainsEach(Objects: TObjectList): TEachFunction;
begin
  Result := function(AQ: TAQ; O: TObject): Boolean
  begin
    Result := Objects.IndexOf(O) >= 0;
  end;
end;

function TAQ.IfThen(Condition: Boolean): TAQ;
begin
  if SupervisorLock(Result, aqmIfThen) then
    Exit;

  if not ConditionLock then
  begin
    Result := NewChain;
    Result.ConditionLock := not Condition;
  end;

  if not Result.ConditionLock then
    Result.Append(Self);
  Inc(Result.FConditionCount);
end;

class procedure TAQ.Initialize;
begin
  if Initialized then
    Exit;
  Initialized := True;
  {**
   * Die Initialisierung der gesamten Klasse
   **********************************************************************************************}

  FStopWatch := TStopwatch.StartNew;

  FTick := FStopWatch.ElapsedMilliseconds;
{$IFDEF UseThreadTimer}
  FTimerThread := TTimerThread.Create(IntervalResolution, TAQ.GlobalIntervalTimerEvent);
  FTimerThread.Enable;
{$ELSE}
  FTimerHandler := SetTimer(0, 0, IntervalResolution, @TAQ.GlobalIntervalTimerEvent);
{$ENDIF}

  FActiveIntervalAQs := TAQ.Create;
  FActiveIntervalAQs.Recurse := False;

  FGC := TAQ.Create;
  FGC.OwnsObjects := True;
  FGC.Recurse := False;

  FComponentsNotifier := TComponentsNotifier.Create;
  FComponentsNotifier.OwnsObjects := False;
end;

// Free all class related stuff which is initialized in `TAQ.Initialize`
class procedure TAQ.Finalize;
begin
  if Finalized or not Initialized then
    Exit;

  Finalized := True;

  // This timer is created together with FGarbageCollector, so accordingly it must be freed with it.
{$IFDEF UseThreadTimer}
  FreeAndNil(FTimerThread);
{$ELSE}
  if (FTimerHandler > 0) and KillTimer(0, FTimerHandler) then
    FTimerHandler := 0;
{$ENDIF}

  FreeAndNil(FComponentsNotifier);
  FreeAndNil(FGC); // Release the garbage collector with all the managed instances
  FreeAndNil(FActiveIntervalAQs);
end;

class function TAQ.GarbageCollector: TAQ;
begin
  if Initialized then
    Exit(FGC);

  Initialize;

  FGC.EachInterval(GarbageCleanInterval,
    {**
     * In GarbageCollector befindet sich der FGarbageCollector selbst und
     * in O eine TAQ-Instanz die auf ihre Lebenszeichen untersucht werden muss.
     *}
    function(GC: TAQ; O: TObject): Boolean
    var
      CleanEndTick: Int64;
      AQsForDestroy: Integer;
    begin
      {**
       * Soll nur einmal ausgeführt werden, da die eigentliche Bereinigung in den
       * untergeordeneten Eachs abläuft
       *}
      // False means here, that this closure is only called once per Interval, because the
      // actually clean up is performed in the subsequent Each calls.
      Result := False;

      // Determine the count of dead TAQ instances
      AQsForDestroy := 0;
      GC.Each(
        function(AQ: TAQ; O: TObject): Boolean
        begin
          if (O is TAQ) and not TAQ(O).IsAlive then
            Inc(AQsForDestroy);
          Result := True;
        end);

      // Clean up is only performed when SpareAQsCount is exceeded
      if AQsForDestroy < SpareAQsCount then
      begin
        {$IFDEF OutputDebugGCFree}
        OutputDebugString(PWideChar(Format('Clean up skipped, beacause the spare limit (%d) isn''t exsceeded (%d)',
          [SpareAQsCount, AQsForDestroy])));
        {$ENDIF}
        Exit;
      end;

      CleanEndTick := FStopWatch.ElapsedMilliseconds + GarbageCleanTime;

      GC.Each(
        function(GC: TAQ; O: TObject): Boolean
        begin
          if (O is TAQ) and not TAQ(O).IsAlive then
          begin
            GC.Remove(O);
            Dec(AQsForDestroy);
            {$IFDEF OutputDebugGCFree}
            OutputDebugString(PWideChar(Format('TAQ released. Left instances in GarbageCollector: %d.',
              [GarbageCollector.Count])));
            {$ENDIF}
          end;
          // Conditional cleanup runtime
          // It runs as long there are more destroyable instances as SpareAQsCount
          // or until GarbageCleanTime expired
          Result := (AQsForDestroy > SpareAQsCount) or
            (CleanEndTick >= FStopWatch.ElapsedMilliseconds);
        end);
        {$IFDEF OutputDebugGCFree}
        if CleanEndTick < FStopWatch.ElapsedMilliseconds then
          OutputDebugString('Cleanup process aborted prematurely, because the timeout expired.');
        {$ENDIF}
    end);

  Result := FGC;
end;

class procedure TAQ.GlobalIntervalTimerEvent;
begin
  if Finalized then
    Exit;
  FTick := FStopWatch.ElapsedMilliseconds;
  FActiveIntervalAQs.Each(
    {**
     * @param AQ Contains FActiveIntervalAQs
     * @param O Contains a TAQ instance, which at least contains one Interval
     *}
    function(AQ: TAQ; O: TObject): Boolean
    begin
      TAQ(O).LocalIntervalTimerEvent;
      Result := True; // full Each scan
    end);
end;

function TAQ.IntervalActorsChain(ID: Integer; IncludeOrphans: Boolean): TAQ;
begin
  if SupervisorLock(Result, aqmIntervalActorsChain) then
    Exit;
  Result := CustomActors(arInterval, ID, IncludeOrphans);
end;

function TAQ.IsAlive: Boolean;
begin
  Result := ((FLifeTick + MaxLifeTime) >= TAQ.Tick) or Immortally;
end;

procedure TAQ.LocalIntervalTimerEvent;
var
  cc: Integer;
begin
  if not (Assigned(FIntervals) and (FIntervals.Count > 0)) then
    Exit;

  for cc := FIntervals.Count - 1 downto 0 do
    ProcessInterval(TInterval(FIntervals[cc]));

  HeartBeat;
end;

// Returns a managed TAQ instance
//
// Managed means, that you don't need to worry about memory leaks ;-)
// There is a simple (but maybe powerful) garbage collector implementation. By requesting for a
// managed instance it looks primary in the garabage for an died TAQ instance and in case returning
// it.
class function TAQ.Managed: TAQ;
var
  ManagedAQ: TAQ;
begin
  ManagedAQ := nil;

  // Looks in in the GarbageCollector for an died TAQ instance
  GarbageCollector.Each(
    function(AQ: TAQ; O: TObject): Boolean
    begin
      if (O is TAQ) and not TAQ(O).IsAlive then
      begin
        ManagedAQ := TAQ(O);
        ManagedAQ.Clean;
        ManagedAQ.HeartBeat;
{$IFDEF OutputDebugGCRecycle}
        OutputDebugString(PWideChar(Format('TAQ %p at index #%d of GC recycled.',
          [@O, AQ.IndexOf(O)])));
{$ENDIF}
      end;
      // Let the Each run, until we find an died TAQ instance
      Result := not Assigned(ManagedAQ);
    end);

  if Assigned(ManagedAQ) then
    Exit(ManagedAQ);

  Result := TAQ.Create;
  Result.HeartBeat;
  // This is the whole magic behind the managed TAQ instance
  GarbageCollector.Add(Result);
{$IFDEF OutputDebugGCCreate}
  OutputDebugString(PWideChar(Format('New TAQ %p in GC at index #%d.',
    [@Result, GarbageCollector.IndexOf(Result)])));
{$ENDIF}
end;

// Creates a new chained TAQ instance, where each currently contained object resist in
// it's own TAQ instance
function TAQ.MultiplexChain: TAQ;
var
  MultiAQ: TAQ;
begin
  if SupervisorLock(Result, aqmMultiplexChain) then
    Exit;
  MultiAQ := NewChain;
  Result := MultiAQ;
  Each(
    function(AQ: TAQ; O: TObject): Boolean
    begin
      Result := True;
      if not (O is TAQ) then
        MultiAQ.AppendAQ(TAQ.Take(O));
    end);
end;

// Creates a new chained TAQ instance, where all (possibly recursive) contained objects are
// flattened to the returned one
//
// For better understanding: This method makes the opposite of TAQ.MultiplexChain.
function TAQ.DemultiplexChain: TAQ;
var
  SimpleAQ: TAQ;
begin
  if SupervisorLock(Result, aqmDemultiplexChain) then
    Exit;
  SimpleAQ := NewChain;
  Result := SimpleAQ;
  Each(
    function(AQ: TAQ; O: TObject): Boolean
    begin
      Result := True;
      if not(O is TAQ) then
        SimpleAQ.Add(O);
    end);
end;

function TAQ.ParentsAppend(Recurse: Boolean; ParentsFiller: TEachFunction): TAQ;
begin
  if SupervisorLock(Result, aqmParentsAppend) then
    Exit;
  if not Assigned(ParentsFiller) then
    ParentsFiller := Self.ParentsFiller;
  Result := CustomFiller(ParentsFiller, True, Recurse);
end;

function TAQ.ParentsChain(Recurse: Boolean; ParentsFiller: TEachFunction): TAQ;
begin
  if SupervisorLock(Result, aqmParentsChain) then
    Exit;
  if not Assigned(ParentsFiller) then
    ParentsFiller := Self.ParentsFiller;
  Result := CustomFiller(ParentsFiller, False, Recurse);
end;

function TAQ.ParentsFiller(AQ: TAQ; O: TObject): Boolean;
begin
  Result := True;
  if not ((O is TComponent) and (TComponent(O).HasParent)) then
    Exit;
  AQ.Add(TComponent(O).GetParentComponent);
end;

function TAQ.Plugin<T>: T;
begin
  Result := T.Create;
  TAQPlugin(Result).FWorkAQ := Self;
  GarbageCollector.Add(Result);
  TAQPlugin(Result).Autorun;
end;

function TAQ.EndChain: TAQ;
begin
  if SupervisorLock(Result, aqmEndChain) then
    Exit;
  if Assigned(FChainedTo) and GarbageCollector.Contains(FChainedTo) then
    Exit(FChainedTo);
  Result := Managed;
end;

procedure TAQ.ProcessInterval(Interval: TInterval);
var
  EachFunction: TEachFunction;
begin
  FCurrentInterval := Interval;

  EachFunction := (CurrentInterval.Each);
  if Assigned(EachFunction) then
    Each(EachFunction);

  if Assigned(CurrentInterval) and CurrentInterval.IsFinished then
  begin
    RemoveInterval(CurrentInterval);
    FCurrentInterval := nil;
  end;
end;

procedure TAQ.RemoveInterval(Interval: TInterval);
begin
  FIntervals.Remove(Interval);
  if FIntervals.Count = 0 then
    FActiveIntervalAQs.Remove(Self);
  {$IFDEF OutputDebugActiveIntervals}
    OutputDebugString(PWideChar('TAQ instances with intervals: ' +
      IntToStr(FActiveIntervalAQs.Count)));
  {$ENDIF}
end;

procedure TAQ.SetConditionLock(Value: Boolean);
begin
  if Value = ConditionLock then
    Exit;
  SetBit(FBools, ConditionLockBitMask, Value);
end;

procedure TAQ.SetImmortally(Value: Boolean);
begin
  if Value = Immortally then
    Exit;
  SetBit(FBools, ImmortallyBitMask, Value);
end;

procedure TAQ.SetRecurse(Value: Boolean);
begin
  SetBit(FBools, RecurseBitMask, Value);
end;

function TAQ.SliceChain(StartIndex, Count: Integer): TAQ;
var
  EndIndex, TotalCount, cc: Integer;
begin
  if SupervisorLock(Result, aqmSliceChain) then
    Exit;
  Result := NewChain;
  TotalCount := Self.Count;
  if TotalCount = 0 then
    Exit;
  if StartIndex < 0 then
    StartIndex := TotalCount + StartIndex;
  if Count = 0 then
    EndIndex := TotalCount
  else
    EndIndex := StartIndex + Count;

  StartIndex := Max(0, StartIndex);
  EndIndex := Min(EndIndex, Self.Count);

  for cc := StartIndex to EndIndex - 1 do
    Result.Add(Items[cc]);
end;

function TAQ.SupervisorLock(out AQ: TAQ; Method: TAQMethod): Boolean;
var
  RelatedMethod: Boolean;
begin
  Result := False;
  AQ := Self;
  if ConditionLock then
  begin
    RelatedMethod := Method in AQConditionMethods;  // Check, whether it's a condition related method
    Result := (FConditionCount > 0) and not RelatedMethod;
  end;
end;

// Private function, used by the overloaded TAQ.Take methods
//
// @param AQ Is will be only assigned, if the result is True
function PrimaryRetakeCheck(CheckForAQ: TObject; First: TObject; ObjectsCount: Integer;
  out AQ: TAQ): Boolean; inline;
var
  CheckAQ: TAQ;
begin
  if not (CheckForAQ is TAQ) then
    Exit(False);
  CheckAQ := TAQ(CheckForAQ);
  Result := (CheckAQ.Count = ObjectsCount) and (CheckAQ.FConditionCount = 0) and
     (CheckAQ[0] = First) and not CheckAQ.ConditionLock and CheckAQ.Recurse
    {and (AQ.FChainedTo = nil)};
  if Result then
    AQ := CheckAQ;
end;

procedure RetakeDebugMessage(RetakenAQ: TAQ);
begin
  OutputDebugString(PWideChar(Format('TAQ %p at index #%d of GC retaken.',
    [@RetakenAQ, TAQ.GarbageCollector.IndexOf(RetakenAQ)])));
end;

class function TAQ.Take(const Objects: TObjectArray): TAQ;
{$IFDEF RetakeFromGC}
var
  AQMatch: TAQ;
  ObjectsCount: Integer;
begin
  AQMatch := nil;
  ObjectsCount := Length(Objects);

  if ObjectsCount > 0 then
    GarbageCollector.Each(
      function(GC: TAQ; O: TObject): Boolean
      var
        cc: Integer;
        Match: Boolean;
        AQ: TAQ;
      begin
        Match := PrimaryRetakeCheck(O, Objects[0], ObjectsCount, AQ);
        if Match then
        begin
          for cc := 1 to ObjectsCount - 1 do // Note: Begin at index 1, because IsAQMatch has already tested on 0
            if AQ[cc] <> Objects[cc] then
            begin
              Match := False;
              Break;
            end;
          if Match then
            AQMatch := AQ;
        end;
        Result := not Match; // Break the Each, if the first TAQ instance matched
      end);

  if Assigned(AQMatch) then
  begin
    AQMatch.HeartBeat;
    Result := AQMatch;
{$IFDEF OutputDebugGCRetake}
    RetakeDebugMessage(Result);
{$ENDIF}
  end
  else
    Result := Managed.Append(Objects);
{$ELSE}
begin
  Result := Managed.Append(Objects);
{$ENDIF}
end;

class function TAQ.Take(AObject: TObject): TAQ;
{$IFDEF RetakeFromGC}
var
  AQMatch: TAQ;
begin
  AQMatch := nil;
  GarbageCollector.Each(
    function(GC: TAQ; O: TObject): Boolean
    begin
      Result := not PrimaryRetakeCheck(O, AObject, 1, AQMatch);
    end);

  if Assigned(AQMatch) then
  begin
    AQMatch.HeartBeat;
    Result := AQMatch;
{$IFDEF OutputDebugGCRetake}
    RetakeDebugMessage(Result);
{$ENDIF}
  end
  else
    Result := Managed.Append(AObject);
{$ELSE}
begin
  Result := Managed.Append(AObject);
{$ENDIF}
end;

class function TAQ.Take(Objects: TObjectList): TAQ;
{$IFDEF RetakeFromGC}
var
  AQMatch: TAQ;
  ObjectsCount: Integer;
begin
  AQMatch := nil;
  ObjectsCount := Objects.Count;

  if ObjectsCount > 0 then
    GarbageCollector.Each(
      function(GC: TAQ; O: TObject): Boolean
      var
        cc: Integer;
        Match: Boolean;
        AQ: TAQ;
      begin
        Match := PrimaryRetakeCheck(O, Objects[0], ObjectsCount, AQ);
        if Match then
        begin
          for cc := 1 to ObjectsCount - 1 do // Note: Begin at index 1, because IsAQMatch has already tested on 0
            if AQ[cc] <> Objects[cc] then
            begin
              Match := False;
              Break;
            end;
          if Match then
            AQMatch := AQ;
        end;

        Result := not Match;  // Break the Each, if the first TAQ instance matched
      end);

  if Assigned(AQMatch) then
  begin
    AQMatch.HeartBeat;
    Result := AQMatch;
{$IFDEF OutputDebugGCRetake}
    RetakeDebugMessage(Result);
{$ENDIF}
  end
  else
    Result := Managed.Append(Objects);
{$ELSE}
begin
  Result := Managed.Append(Objects);
{$ENDIF}
end;

class function TAQ.Take<T>(Objects: TObjectList<T>): TAQ;
var
  cc: Integer;
{$IFDEF RetakeFromGC}
  AQMatch: TAQ;
  ObjectsCount: Integer;
begin
  AQMatch := nil;
  ObjectsCount := Objects.Count;

  if ObjectsCount > 0 then
    GarbageCollector.Each(
      function(GC: TAQ; O: TObject): Boolean
      var
        cc: Integer;
        Match: Boolean;
        AQ: TAQ;
      begin
        Match := PrimaryRetakeCheck(O, Objects[0], ObjectsCount, AQ);
        if Match then
        begin
          for cc := 1 to ObjectsCount - 1 do // Note: Begin at index 1, because IsAQMatch has already tested on 0
            if AQ[cc] <> TObject(Objects[cc]) then
            begin
              Match := False;
              Break;
            end;
          if Match then
            AQMatch := AQ;
        end;

        Result := not Match;  // Break the Each, if the first TAQ instance matched
      end);

  if Assigned(AQMatch) then
  begin
    AQMatch.HeartBeat;
    Result := AQMatch;
//{$IFDEF OutputDebugGCRetake}
//		RetakeDebugMessage(Result);
//{$ENDIF}
    Exit;
  end
  else
    Result := Managed;
{$ELSE}
begin
  Result := Managed;
{$ENDIF}
  for cc := 0 to Objects.Count - 1 do
    Result.Add(Objects[cc]);
end;

// Determines, whether there are active actors (running animation, delay...) for AObject in general
// or optional for the actor with the specified ID
class function TAQ.HasActiveActors(CheckActors: TActorRoles; AObject: TObject; ID: Integer): Boolean;
var
  Found: Boolean;
begin
  Found := False;
  FActiveIntervalAQs.Each(
    function(AQ: TAQ; O: TObject): Boolean
    var
      OAQ: TAQ absolute O;
      CheckActor: TActorRole;
    begin
      for CheckActor in CheckActors do
      begin
        Found := OAQ.Contains(AObject) and OAQ.HasActors(CheckActor, ID);
        if Found then
          Break;
      end;

      Result := not Found;
    end);
  Result := Found;
end;

function TAQ.TimerActorsChain(ID: Integer; IncludeOrphans: Boolean): TAQ;
begin
  if SupervisorLock(Result, aqmTimerActorsChain) then
    Exit;
  Result := CustomActors(arTimer, ID, IncludeOrphans);
end;

class function TAQ.Unmanaged: TAQ;
begin
  Result := TAQ.Create;
end;

{** TAQPlugin **}

procedure TAQPlugin.Autorun;
begin
  // Can be implemented in custom plugins
end;

function TAQPlugin.Each(EachFunction: TEachFunction): TAQ;
begin
  Result := WorkAQ.Each(EachFunction);
end;

function TAQPlugin.GarbageCollector: TAQ;
begin
  Result := TAQ.GarbageCollector;
end;

function TAQPlugin.GetImmortally: Boolean;
begin
  Result := WorkAQ.Immortally;
end;

procedure TAQPlugin.SetImmortally(Value: Boolean);
begin
  WorkAQ.Immortally := Value;
end;

{** TInterval **}

constructor TInterval.Finite(Duration: Integer; Each, LastEach: TEachFunction; ActorRole: TActorRole;
  ID: Integer);
begin
  FID := ID;
  FActorRole := ActorRole;
  FNextEach := Each;
  FLastEach := LastEach;

  FFirstTick := TAQ.Tick;
  FInterval := IntervalResolution;

  FLastTick := FFirstTick + Cardinal(Duration);
  UpdateNextTick;
end;

constructor TInterval.Infinite(Interval: Integer; Each: TEachFunction; ActorRole: TActorRole;
  ID: Integer);
begin
  FID := ID;
  FActorRole := ActorRole;
  FFirstTick := TAQ.Tick;
  FNextEach := Each;
  FLastEach := nil;
  FInterval := Max(IntervalResolution, Interval);
  FLastTick := 0;
  UpdateNextTick;
end;

destructor TInterval.Destroy;
begin
  FNextEach := nil;
  FLastEach := nil;
  inherited;
end;

procedure TInterval.Cancel;
begin
  FInterval := 0;
end;

function TInterval.Each: TEachFunction;
begin
  Result := nil;
  if IsCanceled then
    Exit;

  // Infinite interval
  if (FLastTick = 0) and (TAQ.Tick >= FNextTick) then
  begin
    Result := FNextEach;
    UpdateNextTick;
  end
  // Finite interval
  else if (FLastTick > 0) then
  begin
    if TAQ.Tick >= FLastTick then
    begin
      if Assigned(FLastEach) then
        Result := FLastEach
      else
        Result := FNextEach;
      FLastEach := nil;
      FNextEach := nil;
    end
    else if TAQ.Tick >= FNextTick then
    begin
      Result := FNextEach;
      UpdateNextTick;
    end;
  end;
end;

procedure TInterval.Finish;
begin
  if IsFinite and not IsFinished then
    FLastTick := TAQ.Tick;
end;

function TInterval.IsCanceled: Boolean;
begin
  Result := FInterval = 0;
end;

function TInterval.IsFinished: Boolean;
begin
  Result := ((FLastTick > 0) and (TAQ.Tick >= FLastTick) and not Assigned(FLastEach) and
    not Assigned(FNextEach)) or IsCanceled;
end;

function TInterval.IsFinite: Boolean;
begin
  Result := FLastTick > 0;
end;

function TInterval.Progress: Real;
begin
  if (FLastTick = FFirstTick) or (TAQ.Tick >= FLastTick) then
    Exit(1);
  Result := (Min(TAQ.Tick, FLastTick) - FFirstTick) / (FLastTick - FFirstTick);
end;

procedure TInterval.UpdateNextTick;
begin
  FNextTick := TAQ.Tick + Cardinal(FInterval);
end;

{ TComponentsNotifier }

procedure TComponentsNotifier.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if Action in [lnExtracted, lnDeleted] then
    TAQ.ComponentsNotification(TComponent(Ptr), opRemove);
  inherited Notify(Ptr, Action);
end;

{ TSpinWait }

{$IF RTLVersion < 22}
class function TSpinWait.SpinUntil(const ACondition: TFunc<Boolean>; Timeout: LongWord): Boolean;
var
  Timer: TStopwatch;
begin
  Timer := TStopwatch.StartNew;
  while not ACondition() do
  begin
    if (Timeout = 0) or ((Timeout <> INFINITE) and (Timeout <= Timer.ElapsedMilliseconds)) then
      Exit(False);
    Sleep(1);
  end;
  Result := True;
end;
{$IFEND}

{ TTimerThread }

constructor TTimerThread.Create(Interval: Integer; TimerProc: TThreadProcedure);
begin
  FInterval := Interval;
  FTimerProc := TimerProc;
  FMainSignal := TEvent.Create(nil, False, False, '');
  FWindowHandle := AllocateHWnd(WndProc);

  inherited Create(False);
end;

destructor TTimerThread.Destroy;
begin
  Terminate;
  FMainSignal.SetEvent;
  while not TSpinWait.SpinUntil(
    function: Boolean
    begin
      Result := Finished;
    end, 20) do
    CheckSynchronize;
  FMainSignal.Free;

  if FWindowHandle <> 0 then
  begin
    DeallocateHWnd(FWindowHandle);
    FWindowHandle := 0;
  end;

  inherited Destroy;
end;

procedure TTimerThread.Disable;
begin
  if Enabled then
  begin
    FEnabled := False;
    FMainSignal.SetEvent;
  end;
end;

procedure TTimerThread.Enable;
begin
  if not Enabled then
  begin
    FEnabled := True;
    FMainSignal.SetEvent;
  end;
end;

procedure TTimerThread.SetInterval(NewInterval: Integer);
begin
  if (NewInterval = Interval) or (NewInterval <= 0) then
    Exit;
{$IF RTLVersion < 22}
  InterlockedExchange(FInterval, NewInterval);
{$ELSE}
  TInterlocked.Exchange(FInterval, NewInterval);
{$IFEND}
  FMainSignal.SetEvent;
end;

procedure TTimerThread.WndProc(var Msg: TMessage);
begin
  with Msg do
    if Msg = WM_TIMER then
      try
        FTimerProc;
      except
        System.Classes.ApplicationHandleException(Self);
      end
    else
      Result := DefWindowProc(FWindowHandle, Msg, wParam, lParam);
end;

procedure TTimerThread.Execute;
var
  LocalInterval: Integer;

  procedure RaiseTimer;
  var
{$IF CompilerVersion >= 23.0}
    MessageResult: NativeUInt; // Since XE2
{$ELSE}
    MessageResult: Cardinal;
{$IFEND}
  begin
    SendMessageTimeout(FWindowHandle, WM_TIMER, 0, 0, SMTO_ABORTIFHUNG, LocalInterval,
      @MessageResult);
    {**
     * Old solution
     *}
    // Synchronize(FTimerProc);
  end;
begin
  while not Terminated do
  begin
    FMainSignal.WaitFor(INFINITE);
    {**
     * This is the whole timer
     *}
    if Enabled then
    begin
      LocalInterval := Interval;
      while not TSpinWait.SpinUntil(
        function: Boolean
        begin
          Result := Terminated or not Enabled or (LocalInterval <> Interval);
        end, LocalInterval) do
        RaiseTimer;
    end;
  end;
end;

initialization

finalization
TAQ.Finalize;

end.