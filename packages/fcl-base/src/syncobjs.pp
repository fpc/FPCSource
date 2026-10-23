{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1998 by Florian Klaempfl
    member of the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}
{$modeswitch advancedrecords}
{$modeswitch functionreferences}
{$modeswitch anonymousfunctions}
{$h+}

{$IF DEFINED(WINCE) or DEFINED(AIX)}
{$DEFINE NO_SEMAPHORE_SUPPORT}
{$ENDIF}

{$IF DEFINED(WINCE)}
{$DEFINE NO_MUTEX_SUPPORT}
{$ENDIF}

{ SYNCOBJS_PORTABLE_CONDVAR makes the condition variables use the RTL event based waiter queue on all platforms }
{$IFNDEF SYNCOBJS_PORTABLE_CONDVAR}
{$IF DEFINED(MSWINDOWS) and not DEFINED(WINCE)}
{$DEFINE NATIVE_CONDVAR_CS}
{$ENDIF}
{$IFDEF UNIX}
{$DEFINE NATIVE_CONDVAR_MUTEX}
{$ENDIF}
{$ENDIF}


{$IFNDEF FPC_DOTTEDUNITS}
unit SyncObjs;
{$ENDIF FPC_DOTTEDUNITS}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  {$IFNDEF VER3_2}
  system.timespan,
  {$ENDIF}
  {$IFDEF UNIX}
  UnixApi.Types,
  {$ENDIF}
  {$IFDEF CPUWASM32}
  Wasm.Semaphore,
  {$ENDIF}
  System.DateUtils,
  System.Classes,
  System.SysUtils;

{$ELSE FPC_DOTTEDUNITS}
  {$IFNDEF VER3_2}
  system.timespan,
  {$ENDIF}
  {$IFDEF UNIX}
  unixtype,
  {$ENDIF}
  {$IFDEF CPUWASM32}
  WasmSem,
  {$ENDIF}
  DateUtils,
  Classes, // TThread
  sysutils;
{$ENDIF FPC_DOTTEDUNITS}

type
  PSecurityAttributes = Pointer;
  TEventHandle = Pointer;

{$IFNDEF NO_SEMAPHORE_SUPPORT}
{$IFDEF UNIX}
  TPosixSemaphore = sem_t;
  PPosixSemaphore = ^TPosixSemaphore;
{$ENDIF}
{$ENDIF}

const
  INFINITE = Cardinal(-1);

type
   ESyncObjectException = Class(Exception);
   ELockException = Class(ESyncObjectException);
   ELockRecursionException = Class(ESyncObjectException);

   TWaitResult = (wrSignaled, wrTimeout, wrAbandoned, wrError);

   TSynchroObject = class(TObject)
      procedure Acquire;virtual;
      procedure Release;virtual;
      // Waits until the object is signaled or aTimeout milliseconds have passed; returns wrError by default
      function WaitFor(aTimeout: Cardinal = INFINITE): TWaitResult; overload; virtual;
      {$IFNDEF VER3_2}
      // Waits until the object is signaled or aTimeout has passed
      function WaitFor(const aTimeout: TTimeSpan): TWaitResult; overload;
      {$ENDIF}
   end;

   { TLockGuard }
   generic TLockGuard<T:TSynchroObject> = record
     obj: T;
     class operator Initialize(var hdl: TLockGuard);
     class operator Finalize(var hdl: TLockGuard);
     procedure Init(AObj: T);
   end;

   TCriticalSection = class(TSynchroObject)
   private
      CriticalSection : TRTLCriticalSection;
   public
      procedure Acquire;override;
      procedure Release;override;
      // Enters the critical section, waiting at most aTimeout milliseconds; the caller releases it on wrSignaled
      function WaitFor(aTimeout: Cardinal = INFINITE): TWaitResult; overload; override;
      procedure Enter;
      function  TryEnter:boolean;
      procedure Leave;
      constructor Create;
      destructor Destroy;override;
   end;
   TCriticalSectionGuard = specialize TLockGuard<TCriticalSection>;

   THandleObject= class;
   THandleObjectArray = array of THandleObject;

   { THandleObject }

   THandleObject = class abstract  (TSynchroObject)
   protected
      FHandle : TEventHandle;
      FLastError : Integer;
      {$IFDEF MSWINDOWS}
       // Windows specific, use COWait* functions for com compatibility.
        FUseCOMWait: Boolean;
      {$ENDIF MSWINDOWS}
   public
      constructor Create(UseComWait : Boolean=false);
      destructor Destroy; override;
      function WaitFor(Timeout : Cardinal=INFINITE) : TWaitResult;overload; override;
      {$IFNDEF VER3_2}
      {$IFDEF MSWINDOWS}
        class function WaitForMultiple(const HandleObjs: THandleObjectArray; Timeout: Cardinal; AAll: Boolean; out SignaledObj: THandleObject; UseCOMWait: Boolean = False; Len: Integer = 0): TWaitResult;
      {$ENDIF MSWINDOWS}
      {$ENDIF VER3_2}
      property Handle : TEventHandle read FHandle;
      property LastError : Integer read FLastError;
   end;

   TEventObject = class(THandleObject)
   private
      FManualReset: Boolean;
   public
      constructor Create(EventAttributes : PSecurityAttributes;
        AManualReset,InitialState : Boolean;const Name : string;
        UseComWait:boolean=false); overload;
      procedure ResetEvent;
      procedure SetEvent;

      Property ManualReset : Boolean read FManualReset;
   end;

   TEvent = TEventObject;

   TSimpleEvent = class(TEventObject)
      constructor Create;
   end;

   { TCountdownEvent }

   TCountdownEvent = class(TSynchroObject)
   private
     FEvent: TEvent;
     FInitialCount: Integer;
     FCurrentCount: Integer;
     function GetIsSet: Boolean;
   public
     // Creates the event with a count of aCount; the event is set when aCount is 0
     constructor Create(aCount: Integer);
     destructor Destroy; override;
     // Decrements the count by aCount and sets the event at 0; returns True when the count reached 0
     function Signal(aCount: Integer = 1): Boolean;
     // Increments the count by aCount; raises when the event is already set
     procedure AddCount(aCount: Integer = 1);
     // Increments the count by aCount; returns False when the event is already set
     function TryAddCount(aCount: Integer = 1): Boolean;
     // Restores the count to InitialCount
     procedure Reset; overload;
     // Sets InitialCount and the count to aCount
     procedure Reset(aCount: Integer); overload;
     // Waits until the count reaches 0 or aTimeout milliseconds have passed
     function WaitFor(aTimeout: Cardinal = INFINITE): TWaitResult; overload; override;
     // Remaining number of signals before the event is set
     property CurrentCount: Integer read FCurrentCount;
     // Count the event starts from after Create or Reset
     property InitialCount: Integer read FInitialCount;
     // True when the count is 0
     property IsSet: Boolean read GetIsSet;
   end;

{$IFDEF CPU16}
{$DEFINE NOPOINTER}
{$ENDIF}

  TBitOffset = 0 .. 31;

  TInterlocked = class sealed
    class function Add(var Target: Longint; aIncrement: Longint): Longint; overload; static; inline;
    class function Exchange(var Target: Longint; Value: Longint): Longint; overload; static; inline;
    class function CompareExchange(var Target: Longint; Value: Longint; Comparand: Longint): Longint; overload; static; inline;
    class function CompareExchange(var Target: Longint; Value: Longint; Comparand: Longint; out Succeeded: Boolean): Longint; overload; static;
    class function Decrement(var Target: Longint): Longint; overload; static; inline;
    class function Increment(var Target: Longint): Longint; overload; static; inline;

    class function BitTestAndSet(var Target: Longint; BitOffset: TBitOffset): Boolean; static;
    class function BitTestAndClear(var Target: Longint; BitOffset: TBitOffset): Boolean; static;

{$ifdef FPC_HAS_TYPE_SINGLE}
    class function Exchange(var Target: Single; Value: Single): Single; overload; static; inline;
    class function CompareExchange(var Target: Single; Value: Single; Comparand: Single): Single; overload; static; inline;
{$endif}
{$ifdef cpu64}
    class function Add(var Target: Int64; aIncrement: Int64): Int64; overload; static; inline;
    class function Exchange(var Target: Int64; Value: Int64): Int64; overload; static; inline;
    class function CompareExchange(var Target: Int64; Value: Int64; Comparand: Int64): Int64; overload; static; inline;
    class function Decrement(var Target: Int64): Int64; overload; static; inline;
    class function Increment(var Target: Int64): Int64; overload; static; inline;
    class function Read(var Target: Int64): Int64; static; inline;
{$ifdef FPC_HAS_TYPE_DOUBLE}
    class function CompareExchange(var Target: Double; Value: Double; Comparand: Double): Double; overload; static; inline;
    class function Exchange(var Target: Double; Value: Double): Double; overload; static; inline;
{$endif}
{$endif cpu64}

{$IFNDEF NOPOINTER}
    class function Exchange(var Target: Pointer; Value: Pointer): Pointer; overload; static; inline;
    class function Exchange(var Target: TObject; Value: TObject): TObject; overload; static; inline;
    class function CompareExchange(var Target: Pointer; Value: Pointer; Comparand: Pointer): Pointer; overload; static; inline;
    class function CompareExchange(var Target: TObject; Value: TObject; Comparand: TObject): TObject; overload; static; inline;
    generic class function Exchange<T: class>(var Target: T; Value: T): T; overload; static; inline;
    generic class function CompareExchange<T: class>(var Target: T; Value: T; Comparand: T): T; overload; static; inline;
{$ENDIF NOPOINTER}
  end;

{$IFNDEF NO_SEMAPHORE_SUPPORT}
  TSemaphore = class(THandleObject)
  {$IFDEF UNIX}
     Fsem: TPosixSemaphore;
  {$ENDIF}
  {$IFDEF CPUWASM32}
     FSem : TWasmSemaphore;
  {$ENDIF}
   public
     constructor Create(aUseCOMWait: boolean = false); overload;
     constructor Create(aAttributes: PSecurityAttributes; aInitial, aMaximum: Integer;const aName: string;aUseCOMWait: boolean = false); overload;
     constructor Create(aAccess: Cardinal; aInherit: boolean; const aName: string; aUseCOMWait: boolean = False); overload;
     destructor Destroy; override;
     procedure Acquire; override;
     procedure Release; overload; override;
     function Release(aCount: Integer): Integer; reintroduce; overload;
     function WaitFor(aTimeout: Cardinal = INFINITE): TWaitResult; override;
   end;
   TSemaphoreGuard = specialize TLockGuard<TSemaphore>;
{$ENDIF}

{$IFNDEF NO_MUTEX_SUPPORT}
  TMutex = class(THandleObject)
  private
{$IFDEF UNIX}
    FMutex: pthread_mutex_t;
{$ENDIF POSIX}
{$IFDEF CPUWASM32}
    FMutex: TRTLCriticalSection;
{$ENDIF CPUWASM32}
  public
    constructor Create(aUseCOMWait: Boolean = False); overload;
    constructor Create(aAttributes: PSecurityAttributes; aInitialOwner: Boolean; const aName: string; aUseCOMWait: Boolean = False); overload;
    constructor Create(aAccess: Cardinal; aInherit: Boolean; const aName: string; aUseCOMWait: Boolean = False); overload;
    destructor Destroy; override;
    function WaitFor(aTimeout: Cardinal): TWaitResult; override;
    procedure Acquire; override;
    procedure Release; override;
  end;
  TMutexGuard = specialize TLockGuard<TMutex>;

  { TConditionVariableMutex }

  TConditionVariableMutex = class(TSynchroObject)
  private
{$IFDEF NATIVE_CONDVAR_MUTEX}
    FCondition: pthread_cond_t;
{$ELSE}
    FQueue: Pointer;
{$ENDIF}
  public
    constructor Create;
    destructor Destroy; override;
    // Raises ESyncObjectException: a condition variable is waited on with WaitFor and an external mutex
    procedure Acquire; override;
    // Wakes one thread waiting in WaitFor
    procedure Release; override;
    // Wakes all threads waiting in WaitFor
    procedure ReleaseAll;
    // Releases aExternalMutex, waits until woken or aTimeout milliseconds have passed and acquires aExternalMutex again.
    // aExternalMutex must be held exactly once by the calling thread. The wait can end without a Release,
    // so the caller checks its condition again.
    function WaitFor(aExternalMutex: TMutex; aTimeout: Cardinal = INFINITE): TWaitResult; overload;
  end;
{$ENDIF}

  { TConditionVariableCS }

  TConditionVariableCS = class(TSynchroObject)
  private
{$IFDEF NATIVE_CONDVAR_CS}
    FCondition: Pointer;
{$ENDIF}
    FQueue: Pointer;
  public
    constructor Create;
    destructor Destroy; override;
    // Raises ESyncObjectException: a condition variable is waited on with WaitFor and an external critical section
    procedure Acquire; override;
    // Wakes one thread waiting in WaitFor
    procedure Release; override;
    // Wakes all threads waiting in WaitFor
    procedure ReleaseAll;
    // Leaves aCriticalSection, waits until woken or aTimeout milliseconds have passed and enters aCriticalSection again.
    // aCriticalSection must be entered exactly once by the calling thread. The wait can end without a Release,
    // so the caller checks its condition again.
    function WaitFor(aCriticalSection: TCriticalSection; aTimeout: Cardinal = INFINITE): TWaitResult; overload;
    // Same as WaitFor with a TCriticalSection, for a TRTLCriticalSection
    function WaitFor(var aCriticalSection: TRTLCriticalSection; aTimeout: Cardinal = INFINITE): TWaitResult; overload;
  end;

  { TLightweightEvent }

  TLightweightEvent = class(TSynchroObject)
  private
    FState: LongInt;
    FBlockedCount: LongInt;
    FSpinCount: Integer;
    FLock: TRTLCriticalSection;
    FCondition: TConditionVariableCS;
    function GetIsSet: Boolean;
  public
    // Creates a reset event
    constructor Create; overload;
    // Creates an event that is set when aInitialState is True
    constructor Create(aInitialState: Boolean); overload;
    // Creates an event that spins aSpinCount times before WaitFor blocks
    constructor Create(aInitialState: Boolean; aSpinCount: Integer); overload;
    destructor Destroy; override;
    // Waits until the event is set
    procedure Acquire; override;
    // Clears the event
    procedure ResetEvent;
    // Sets the event and wakes all waiting threads
    procedure SetEvent;
    // Waits until the event is set or aTimeout milliseconds have passed
    function WaitFor(aTimeout: Cardinal = INFINITE): TWaitResult; overload; override;
    // Number of threads blocked in WaitFor
    property BlockedCount: LongInt read FBlockedCount;
    // True when the event is set
    property IsSet: Boolean read GetIsSet;
    // Number of spin cycles WaitFor makes before it blocks
    property SpinCount: Integer read FSpinCount;
  end;

  { TLightweightSemaphore }

  TLightweightSemaphore = class(TSynchroObject)
  private
    FCount: LongInt;
    FMaxCount: LongInt;
    FBlockedCount: LongInt;
    FSpinCount: Integer;
    FLock: TRTLCriticalSection;
    FCondition: TConditionVariableCS;
    function TryTake: Boolean;
  public
    // Creates a semaphore with aInitialCount free slots out of at most aMaxCount
    constructor Create(aInitialCount: Integer; aMaxCount: Integer = MaxInt);
    destructor Destroy; override;
    // Waits until a slot is free and takes it
    procedure Acquire; override;
    // Returns one slot
    procedure Release; overload; override;
    // Returns aReleaseCount slots and returns the count before the release
    function Release(aReleaseCount: Integer): Integer; reintroduce; overload;
    // Takes a slot, waiting at most aTimeout milliseconds for one to become free
    function WaitFor(aTimeout: Cardinal = INFINITE): TWaitResult; overload; override;
    // Number of free slots
    property CurrentCount: LongInt read FCount;
  end;

  // Delphi compatible (hopefully) TSpinwait
  // Uses exponential backoff for first 10 SpinCycles, uses sleep(1) every 20 and sleep(1) every 5 cycles afterwards.
  TSpinFunction = reference to function : Boolean;

  TSpinWait = record
  private
    FCount : Integer;
    // Cache CPU count at program start, assuming it doesn't change
    class var FCPUCount : LongWord;

    class constructor Create;
    function GetNextSpinCycleWillYield: Boolean;
  public
    procedure Reset;
    procedure SpinCycle;

    class procedure SpinUntil(const aCondition: TSpinFunction); overload; static;
    class function SpinUntil(const aCondition: TSpinFunction; aTimeout: Cardinal): Boolean; overload; static;
    class function SpinUntil(const aCondition: TSpinFunction; const aTimeout: TTimeSpan): Boolean; overload; static;
    property Count: Integer read FCount;
    property NextSpinCycleWillYield: Boolean read GetNextSpinCycleWillYield;
  end;
  
  // Fast lock, to be used only when lock is held for short times: uses the spinner.
Type
  { TSpinLock }

  TSpinLock = record
  private
    FLock: LongInt;
    FOwningThread: TThreadID;
    FRecursionCount: Integer;
    FThreadTracking: Boolean;
    function GetIsLocked: Boolean; inline;
    function GetIsLockedByCurrentThread: Boolean;
    function GetIsThreadTrackingEnabled: Boolean; inline;
    function GetIsOwnedByCurrentThread: Boolean; inline;
  public
    constructor Create(EnableThreadTracking: Boolean);
    procedure Enter; inline;
    procedure Exit(PublishNow: Boolean = True); inline;
    function TryEnter: Boolean; overload; inline;
    function TryEnter(Timeout: Cardinal): Boolean; overload; inline;
    function TryEnter(const Timeout: TTimeSpan): Boolean; overload; inline;

    property IsLocked: Boolean read GetIsLocked;
    property IsOwnedByCurrentThread: Boolean read GetIsOwnedByCurrentThread;
    property IsLockedByCurrentThread: Boolean read GetIsLockedByCurrentThread;
    property IsThreadTrackingEnabled: Boolean read GetIsThreadTrackingEnabled;
  end;


  // Guardian pattern. Use to see if an object was freed or not by adding a guardian instance to it.

  IGuardian = interface
    function GetIsDismantled: Boolean;
    procedure Dismantle;
    property IsDismantled: Boolean read GetIsDismantled;
  end;

  TGuardian = class(TInterfacedObject, IGuardian)
  private
    FIsDismantled: Boolean;
    function GetIsDismantled: Boolean;
  public
    procedure Dismantle;
    property IsDismantled: Boolean read GetIsDismantled;
  end;

implementation

{$ifdef MSWindows}
{$IFDEF FPC_DOTTEDUNITS}
uses WinApi.Windows;
{$ELSE}
uses Windows;
{$ENDIF}
{$endif}

{$ifdef UNIX}
{$IFDEF FPC_DOTTEDUNITS}
uses UnixApi.Unix, UnixApi.Base, UnixApi.Pthreads;
{$ELSE}
uses unix, baseunix, pthreads;
{$ENDIF}
{$endif}

Resourcestring
  SErrEventCreateFailed   = 'Failed to create OS basic event with name "%s"';
  SErrEventZeroNotAllowed = 'Handle count of zero is not allowed.';
  SErrEventMaxObjects     = 'The maximal amount of objects is %d.';
  SErrEventTooManyHandles = 'Length of object handles smaller than Len.';
  SErrNamesNotSupported   = 'Named semaphores are not supported on this platform';
  SErrNoSemaphoreSupport  = 'Semaphores are not supported on this platform';
  SErrInvalidReleaseCount = '%d is not a valid release count, count must be >0';
  SErrFailedToSignalSemaphore = 'Failed to signal semaphore at count %d';
  SErrMutexNotSupported   = 'Mutexes are not supported on this platform';
  SErrCountdownNegative   = 'Countdown event count %d must not be negative';
  SErrCountdownInvalid    = 'Countdown event count change %d must be positive';
  SErrCountdownSignal     = 'Cannot signal countdown event %d times, current count is %d';
  SErrCountdownIsSet      = 'Cannot add to a countdown event that is already set';
  SErrCountdownOverflow   = 'Adding %d to countdown event count %d overflows';
  SErrCondVarAcquire      = 'Cannot call Acquire on a condition variable, call WaitFor with an external lock';
  SErrSpinCountNegative   = 'Spin count %d must not be negative';
  SErrSemaphoreCounts     = 'Initial count %d and maximum count %d are not valid for a semaphore';
  SErrSemaphoreOverflow   = 'Releasing %d slots exceeds the maximum semaphore count %d';

{ ---------------------------------------------------------------------
    Real syncobjs implementation
  ---------------------------------------------------------------------}

{$IFDEF OS2}
type
  TBasicEventState = record
                      FHandle: THandle;
                      FLastError: longint;
                     end;
  PLocalEventRec = ^TBasicEventState;
{$ENDIF OS2}

procedure TSynchroObject.Acquire;
begin
end;

procedure TSynchroObject.Release;
begin
end;

function TSynchroObject.WaitFor(aTimeout: Cardinal): TWaitResult;

begin
  Result:=wrError;
end;


{$IFNDEF VER3_2}
function TSynchroObject.WaitFor(const aTimeout: TTimeSpan): TWaitResult;

begin
  Result:=WaitFor(Cardinal(Round(aTimeout.TotalMilliseconds)));
end;
{$ENDIF}


function TCriticalSection.WaitFor(aTimeout: Cardinal): TWaitResult;

var
  lStart: QWord;
  lWait: TSpinWait;

begin
  if aTimeout=INFINITE then
    begin
    Acquire;
    Exit(wrSignaled);
    end;
  lStart:=GetTickCount64;
  lWait.Reset;
  while not TryEnter do
    begin
    if (GetTickCount64-lStart)>=aTimeout then
      Exit(wrTimeout);
    lWait.SpinCycle;
    end;
  Result:=wrSignaled;
end;


procedure TCriticalSection.Enter;
begin
  Acquire;
end;

procedure TCriticalSection.Leave;
begin
  Release;
end;

function  TCriticalSection.TryEnter:boolean;
begin
  result:=System.TryEnterCriticalSection(CriticalSection)<>0;
end;

procedure TCriticalSection.Acquire;

begin
  EnterCriticalSection(CriticalSection);
end;

procedure TCriticalSection.Release;

begin
  LeaveCriticalSection(CriticalSection);
end;

constructor TCriticalSection.Create;

begin
  Inherited Create;
  InitCriticalSection(CriticalSection);
end;

destructor TCriticalSection.Destroy;

begin
  DoneCriticalSection(CriticalSection);
  Inherited;
end;

{ THandleObject }

constructor THandleObject.Create(UseComWait : Boolean=false);
// compatibility shortcut constructor, Com waiting not implemented yet
begin
  FHandle := BasicEventCreate(nil, True,False,'');
  if (FHandle=Nil) then
    Raise ESyncObjectException.CreateFmt(SErrEventCreateFailed,['']);
end;

function THandleObject.WaitFor(Timeout : Cardinal) : TWaitResult;

begin
  Result := TWaitResult(basiceventWaitFor(Timeout, Handle));
  if Result = wrError then
{$IFDEF OS2}
    FLastError := PLocalEventRec (Handle)^.FLastError;
{$ELSE OS2}
  {$if declared(getlastoserror)}
    FLastError := GetLastOSError;
  {$else}
    FLastError:=-1;
  {$endif}
{$ENDIF OS2}
end;

{$IFNDEF VER3_2}
{$IFDEF MSWINDOWS}
class function THandleObject.WaitForMultiple(const HandleObjs: THandleObjectArray; Timeout: Cardinal; AAll: Boolean; out SignaledObj: THandleObject; UseCOMWait: Boolean = False; Len: Integer = 0): TWaitResult;
const COWAIT_DEFAULT = 0;
      COWAIT_WAITALL = 1;
      RPC_S_CALLPENDING = HRESULT($80010115);
var
  HandleIndex: SizeInt;
  ret, CoWaitFlags, SignaledIndex: DWord;
  WOHandles: TWOHandleArray;
begin
  if Len = 0 then
    Len := Length(HandleObjs);

  if Len = 0 then
    raise ESyncObjectException.Create(SErrEventZeroNotAllowed);

  if Len > Length(HandleObjs) then
    raise ESyncObjectException.Create(SErrEventTooManyHandles);

  if Len > MAXIMUM_WAIT_OBJECTS then
    raise ESyncObjectException.CreateFmt(SErrEventMaxObjects, [MAXIMUM_WAIT_OBJECTS]);

  for HandleIndex := 0 to Len - 1 do
    WOHandles[HandleIndex] := {$IFDEF FPC_DOTTEDUNITS}WinApi.{$ENDIF}Windows.HANDLE(HandleObjs[HandleIndex].Handle);

  // what about UseCOMWait?
  if UseCOMWait Then
    begin
      SetLastError(ERROR_SUCCESS); // workaround for mutexes, see docs on CoWaitForMultipleHandles.
      CoWaitFlags := COWAIT_DEFAULT;
      if AAll then
        CoWaitFlags := CoWaitFlags or COWAIT_WAITALL;
      case CoWaitForMultipleHandles(CoWaitFlags, Timeout, Len, @WOHandles, SignaledIndex) of
        S_OK:
          begin
            if not AAll then
              SignaledObj := HandleObjs[SignaledIndex];
            Exit(wrSignaled);
          end;
        RPC_S_CALLPENDING:
          Exit(wrTimeout);
        else
          Exit(wrError);
      end;
    end;

  ret := WaitForMultipleObjects(Len, @WOHandles, AAll, Timeout);

  if (ret >= WAIT_OBJECT_0) and (ret < (WAIT_OBJECT_0 + Len)) then
    begin
      if not AAll then
        SignaledObj := HandleObjs[ret];
      Exit(wrSignaled);
    end;

  if (ret >= WAIT_ABANDONED_0) and (ret < (WAIT_ABANDONED_0 + Len)) then
    begin
      if not AAll then
        SignaledObj := HandleObjs[ret];
      Exit(wrAbandoned);
    end;

  case ret of
    WAIT_TIMEOUT:
      Result := wrTimeout;
    else
      Result := wrError;
  end;
end;
{$endif MSWINDOWS}
{$ENDIF VER_3_2}

destructor THandleObject.Destroy;
begin
  if (Handle<>Nil) then
    BasicEventDestroy(Handle);
end;

constructor TEventObject.Create(EventAttributes : PSecurityAttributes;
  AManualReset,InitialState : Boolean;const Name : string;UseComWait:boolean=false);

begin
  {$IFDEF MSWINDOWS}
    FUseCOMWait:=UseComWait;
  {$endif}
  FHandle := BasicEventCreate(EventAttributes, AManualReset, InitialState, Name);
  if (FHandle=Nil) then
    Raise ESyncObjectException.CreateFmt(SErrEventCreateFailed,[Name]);
  FManualReset:=AManualReset;
end;

procedure TEventObject.ResetEvent;

begin
  BasicEventResetEvent(Handle);
end;

procedure TEventObject.SetEvent;

begin
  BasicEventSetEvent(Handle);
end;

constructor TSimpleEvent.Create;

begin
  inherited Create(nil, True, False, '');
end;

{ ---------------------------------------------------------------------
  TCountdownEvent
  ---------------------------------------------------------------------}

constructor TCountdownEvent.Create(aCount: Integer);

begin
  inherited Create;
  if aCount<0 then
    raise ESyncObjectException.CreateFmt(SErrCountdownNegative, [aCount]);
  FInitialCount:=aCount;
  FCurrentCount:=aCount;
  FEvent:=TEvent.Create(nil, True, aCount=0, '');
end;


destructor TCountdownEvent.Destroy;

begin
  FreeAndNil(FEvent);
  inherited Destroy;
end;


function TCountdownEvent.GetIsSet: Boolean;

begin
  Result:=FCurrentCount=0;
end;


function TCountdownEvent.Signal(aCount: Integer): Boolean;

var
  lCurrent: Integer;

begin
  if aCount<1 then
    raise ESyncObjectException.CreateFmt(SErrCountdownInvalid, [aCount]);
  repeat
    lCurrent:=FCurrentCount;
    if aCount>lCurrent then
      raise ESyncObjectException.CreateFmt(SErrCountdownSignal, [aCount, lCurrent]);
  until InterlockedCompareExchange(FCurrentCount, lCurrent-aCount, lCurrent)=lCurrent;
  Result:=lCurrent=aCount;
  if Result then
    FEvent.SetEvent;
end;


procedure TCountdownEvent.AddCount(aCount: Integer);

begin
  if not TryAddCount(aCount) then
    raise ESyncObjectException.Create(SErrCountdownIsSet);
end;


function TCountdownEvent.TryAddCount(aCount: Integer): Boolean;

var
  lCurrent: Integer;

begin
  if aCount<1 then
    raise ESyncObjectException.CreateFmt(SErrCountdownInvalid, [aCount]);
  repeat
    lCurrent:=FCurrentCount;
    if lCurrent=0 then
      Exit(False);
    if lCurrent>High(Integer)-aCount then
      raise ESyncObjectException.CreateFmt(SErrCountdownOverflow, [aCount, lCurrent]);
  until InterlockedCompareExchange(FCurrentCount, lCurrent+aCount, lCurrent)=lCurrent;
  Result:=True;
end;


procedure TCountdownEvent.Reset;

begin
  Reset(FInitialCount);
end;


procedure TCountdownEvent.Reset(aCount: Integer);

begin
  if aCount<0 then
    raise ESyncObjectException.CreateFmt(SErrCountdownNegative, [aCount]);
  FInitialCount:=aCount;
  InterlockedExchange(FCurrentCount, aCount);
  if aCount=0 then
    FEvent.SetEvent
  else
    FEvent.ResetEvent;
end;


function TCountdownEvent.WaitFor(aTimeout: Cardinal): TWaitResult;

begin
  Result:=FEvent.WaitFor(aTimeout);
end;


{ ---------------------------------------------------------------------
  TInterlocked
  ---------------------------------------------------------------------}


class function TInterlocked.Add(var Target: Longint; aIncrement: Longint): Longint; overload; static; inline;
var
  PreviousValue: Longint;
begin
  PreviousValue := InterLockedExchangeAdd(Target, aIncrement); // returns previous value
  Result := PreviousValue + aIncrement;
end;


class function TInterlocked.CompareExchange(var Target: Longint; Value: Longint; Comparand: Longint): Longint; overload; static; inline;
begin
  Result := InterlockedCompareExchange(Target, Value, Comparand);
end;

class function TInterlocked.CompareExchange(var Target: Longint; Value: Longint; Comparand: Longint; out Succeeded: Boolean): Longint; overload; static;
begin
  Result := InterlockedCompareExchange(Target, Value, Comparand);
  Succeeded := (Result = Comparand);
end;



class function TInterlocked.Decrement(var Target: Longint): Longint; overload; static; inline;
begin
  Result := InterLockedDecrement(Target); // returns new value
end;


class function TInterlocked.Exchange(var Target: Longint; Value: Longint): Longint; overload; static; inline;
begin
  Result := InterLockedExchange(Target, Value);
end;




class function TInterlocked.Increment(var Target: Longint): Longint; overload; static; inline;
begin
  Result := InterLockedIncrement(Target); // returns new value
end;

class function TInterlocked.BitTestAndSet(var Target: Longint; BitOffset: TBitOffset): Boolean;
var
  Fetch, NewValue: Longint;
begin
  repeat
    Fetch := Target;
    Result := Boolean(Fetch shr BitOffset and 1);
    NewValue := Fetch or Longint(1) shl BitOffset;
  until InterlockedCompareExchange(Target, NewValue, Fetch) = Fetch;
end;

class function TInterlocked.BitTestAndClear(var Target: Longint; BitOffset: TBitOffset): Boolean;
var
  Fetch, NewValue: Longint;
begin
  repeat
    Fetch := Target;
    Result := Boolean(Fetch shr BitOffset and 1);
    NewValue := Fetch and not (Longint(1) shl BitOffset);
  until InterlockedCompareExchange(Target, NewValue, Fetch) = Fetch;
end;

{ ---------------------------------------------------------------------
  32-bit single versions
  ---------------------------------------------------------------------}

{$ifdef FPC_HAS_TYPE_SINGLE}
class function TInterlocked.Exchange(var Target: Single; Value: Single): Single; overload; static; inline;
begin
  Result := TSingleRec(TInterlocked.Exchange(Longint(Target), Longint(Value))).Value;
end;

class function TInterlocked.CompareExchange(var Target: Single; Value: Single; Comparand: Single): Single; overload; static; inline;
begin
  Result := TSingleRec(TInterlocked.CompareExchange(Longint(Target), Longint(Value), Longint(Comparand))).Value;
end;
{$endif}


{ ---------------------------------------------------------------------
  64-bit versions
  ---------------------------------------------------------------------}

{$ifdef cpu64}
class function TInterlocked.Read(var Target: Int64): Int64; static; inline;
begin
  Result := InterlockedCompareExchange64(Target, 0, 0);
end;

class function TInterlocked.Increment(var Target: Int64): Int64; overload; static; inline;
begin
  Result := InterLockedIncrement64(Target); // returns new value
end;

class function TInterlocked.Exchange(var Target: Int64; Value: Int64): Int64; overload; static; inline;
begin
  Result := System.InterLockedExchange64(Target, Value);
end;

class function TInterlocked.Decrement(var Target: Int64): Int64; overload; static; inline;
begin
  Result := InterLockedDecrement64(Target); // returns new value
end;class function TInterlocked.Add(var Target: Int64; aIncrement: Int64): Int64; overload; static; inline;
var
  PreviousValue: Int64;
begin
  PreviousValue := System.InterLockedExchangeAdd64(Target, aIncrement); // returns previous value
  Result := PreviousValue + aIncrement;
end;

class function TInterlocked.CompareExchange(var Target: Int64; Value: Int64; Comparand: Int64): Int64; overload; static; inline;
begin
  Result := System.InterlockedCompareExchange64(Target, Value, Comparand);
end;


{ ---------------------------------------------------------------------
  64-bit double versions
  ---------------------------------------------------------------------}

{$ifdef FPC_HAS_TYPE_DOUBLE}
class function TInterlocked.CompareExchange(var Target: Double; Value: Double; Comparand: Double): Double; overload; static; inline;
begin
  Result := TDoubleRec(TInterlocked.CompareExchange(Int64(Target), Int64(Value), Int64(Comparand))).Value;
end;
{$endif}

{$ifdef FPC_HAS_TYPE_DOUBLE}
class function TInterlocked.Exchange(var Target: Double; Value: Double): Double; overload; static; inline;
begin
  Result := TDoubleRec(TInterlocked.Exchange(Int64(Target), Int64(Value))).Value;
end;
{$endif}

{$endif cpu64}

{ ---------------------------------------------------------------------
  Pointer versions
  ---------------------------------------------------------------------}

{$IFNDEF NOPOINTER}
class function TInterlocked.CompareExchange(var Target: Pointer; Value: Pointer; Comparand: Pointer): Pointer; overload; static; inline;
begin
  Result := InterlockedCompareExchange(Target, Value, Comparand);
end;

class function TInterlocked.CompareExchange(var Target: TObject; Value: TObject; Comparand: TObject): TObject; overload; static; inline;
begin
  Result := TObject(InterlockedCompareExchange(Pointer(Target), Pointer(Value), Pointer(Comparand)));
end;

class function TInterlocked.Exchange(var Target: Pointer; Value: Pointer): Pointer; overload; static; inline;
begin
  Result := InterLockedExchange(Target, Value);
end;

class function TInterlocked.Exchange(var Target: TObject; Value: TObject): TObject; overload; static; inline;
begin
  Result := TObject(InterLockedExchange(Pointer(Target), Pointer(Value)));
end;

generic class function TInterlocked.CompareExchange<T>(var Target: T; Value: T; Comparand: T): T; overload; static; inline;
begin
  Result := T(InterlockedCompareExchange(Pointer(Target), Pointer(Value), Pointer(Comparand)));
end;

generic class function TInterlocked.Exchange<T>(var Target: T; Value: T): T; overload; static; inline;
begin
  Result := T(InterLockedExchange(Pointer(Target), Pointer(Value)));
end;

{$ENDIF NOPOINTER}

{$IFDEF UNIX}
{$IF NOT DECLARED(sem_timedwait)}
{$DEFINE USE_SEM_TRYWAIT}
{$DEFINE WAITLOOP}
{$ENDIF}

{$IF NOT DECLARED(pthread_mutex_timedlock)}
{$DEFINE USE_pthread_mutex_trylock}
{$DEFINE WAITLOOP}
{$ENDIF}

{$IFDEF WAITLOOP}
Const
  WaitloopMsecsInterval = 10;

function MsecsBetween(tnow,tthen : Timeval) : Int64;


begin
  Result:=(tnow.tv_sec-tthen.tv_sec)*1000;
  Result:=Result+((tnow.tv_usec-tthen.tv_usec) div 1000);
end;
{$ENDIF WAITLOOP}

procedure MSecsFromNow (tNow : Timeval; aTimeout : Integer; out tfuture: TTimespec);

var
  td,tm : integer;

begin
  td:=aTimeout div 1000;
  tm:=aTimeout mod 1000;
  tfuture.tv_sec:=tnow.tv_sec+td;
  tfuture.tv_nsec:=tnow.tv_usec*1000+(tm*1000*1000);
  if tfuture.tv_nsec>=1000000000 then
    begin
    inc(tfuture.tv_sec);
    dec(tfuture.tv_nsec,1000000000);
    end;
end;
{$ENDIF UNIX}

{ ---------------------------------------------------------------------
  TSemaphore
  ---------------------------------------------------------------------}
{$IFNDEF NO_SEMAPHORE_SUPPORT}
constructor TSemaphore.Create(aUseCOMWait: boolean = false);

begin
  Create(Nil,1,1,'',aUseComWait);
end;

constructor TSemaphore.Create(aAttributes: PSecurityAttributes; aInitial, aMaximum: Integer; const aName: string; aUseCOMWait: boolean = false);

{$IFDEF WINDOWS}
var
  PN : PChar;
{$ENDIF}
begin
{$IFDEF WINDOWS}
  inherited Create(aUseCOMWait);
  PN:=Pchar(Pointer(aName));
{$IF SIZEOF(CHAR)=1}
  FHandle:=TEventHandle(CreateSemaphoreA(aAttributes,aInitial,aMaximum,PN));
{$ELSE}
  FHandle:=TEventHandle(CreateSemaphoreW(aAttributes,aInitial,aMaximum,PN));
{$ENDIF}
  if (FHandle=TEventHandle(0)) then
    RaiseLastOSError;
{$ELSE WINDOWS}
{$IFDEF UNIX}
  if (aName<>'') then
    raise ESyncObjectException.Create(SErrNamesNotSupported);
  if sem_init(@FSem,0,aInitial)<>0 then
    RaiseLastOSError;
{$ELSE UNIX}
{$IFDEF CPUWASM32}
  semaphore_init(FSEM,aInitial,aMaximum);
{$ELSE CPUWASM32}
  Raise ESyncObjectException.Create(SErrNoSemaphoreSupport);
{$ENDIF CPUWASM32}
{$ENDIF UNIX}
{$ENDIF WINDOWS}
end;

constructor TSemaphore.Create(aAccess: Cardinal; aInherit: Boolean; const aName: string; aUseCOMWait: Boolean = False);

{$IFDEF WINDOWS}
var
  PN : PChar;
{$ENDIF}

begin
{$IFNDEF WINDOWS}
  Create(Nil,1,1,aName,aUseCOMWait);
{$ELSE WINDOWS}
  inherited Create(aUseCOMWait);
  PN:=PChar(Pointer(aName));
{$IF SIZEOF(CHAR)=1}
  FHandle:=TEventHandle(OpenSemaphoreA(aAccess,aInherit,PN));
{$ELSE}
  FHandle:=TEventHandle(OpenSemaphoreW(aAccess,aInherit,PN));
{$ENDIF}
  if (FHandle=TEventHandle(0)) then
    RaiseLastOSError;
{$ENDIF WINDOWS}
end;


destructor TSemaphore.Destroy;

begin
{$IFDEF UNIX}
  sem_destroy(@FSem);
{$ENDIF}
{$IFDEF CPUWASM32}
  // If someone is waiting, (s)he'll be notified...
  semaphore_signal(FSEM);
{$ENDIF}
  inherited Destroy;
end;



function TSemaphore.WaitFor(aTimeout: Cardinal = INFINITE): TWaitResult;

{$IFDEF CPUWASM32}
var
  Res : Boolean;
{$ENDIF}

{$IFDEF UNIX}
var
  errno : integer;
  {$IFDEF USE_SEM_TRYWAIT}
  tnew : timeval;
  {$ENDIF}
  tnow : timeval;
  Tmp: ttimespec;
{$ENDIF}
begin
{$IFDEF UNIX}
  Result:=wrError;
  if (aTimeout=0) then
    begin
    if sem_trywait(@FSem) = 0 then
      Result:=wrSignaled
    else if (fpGetErrno=ESysEAGAIN) then
      Result:=wrTimeout
    end
  else if (aTimeout<>INFINITE) then
    begin
    fpgettimeofday(@tnow,Nil);
    {$IFNDEF USE_SEM_TRYWAIT} // not in Darwin
    MsecsFromNow(tnow,aTimeOut,tmp);
    errno:=sem_timedwait(@FSem,@tmp);
    {$ELSE USE_SEM_TRYWAIT}
    Repeat
      ErrNo:=sem_trywait(@FSem);
      if ErrNo=ESysEAGAIN then
        begin
        Sleep(10);
        fpgettimeofday(@tnew,Nil);
        if MSecsBetween(tnew,tnow)>aTimeOut then
          errNo:=ESysETIMEDOUT;
        end
      else if ErrNo=ESysEINTR then
        ErrNo:=ESysEAGAIN;
    until (ErrNo<>ESysEAGAIN);
    {$ENDIF USE_SEM_TRYWAIT}
    if (errno=0) then
      Result:=wrSignaled
    else if errno=ESysETIMEDOUT then
      Result:=wrTimeout;
    end
  else
    begin
    if (sem_wait(@FSem)=0) then
      Result:=wrSignaled
    end;
{$ELSE UNIX}
{$IFDEF CPUWASM32}
  if aTimeout=INFINITE then
    Res:=semaphore_wait_infinite(FSEM)
  else
    Res:=semaphore_wait(FSEM,aTimeOut);
  if Res then
    Result:=wrSignaled
  else
    Result:=wrTimeout;
{$ELSE}
  Result:=Inherited WaitFor(aTimeOut);
{$ENDIF}
{$ENDIF UNIX}

end;


procedure TSemaphore.Acquire;

begin
  if WaitFor(INFINITE)=wrError then
    RaiseLastOSError;
end;

procedure TSemaphore.Release;

begin
  Release(1);
end;


function TSemaphore.Release(aCount: Integer): Integer;

begin
  if (aCount<1) then
    raise ESyncObjectException.CreateFmt(SErrInvalidReleaseCount, [aCount]);
{$IFDEF WINDOWS}
  if not ReleaseSemaphore(PtrUint(FHandle),aCount,@Result) then
    RaiseLastOSError;
{$ENDIF}
{$IFDEF UNIX}
  Result:=0;
  While aCount>0 do
    begin
    if (sem_post(@FSem)<>0) then
      RaiseLastOSError;
    Inc(Result);
    Dec(aCount);
    end;
{$ENDIF}
{$IFDEF CPUWASM32}
  Result:=0;
  While aCount>0 do
    begin
    if not semaphore_signal(FSem) then
          raise ESyncObjectException.CreateFmt(SErrFailedToSignalSemaphore, [aCount]);
    Inc(Result);
    Dec(aCount);
    end;
{$ENDIF}
end;

{$ENDIF NO_SEMAPHORE_SUPPORT}

{ ---------------------------------------------------------------------
  TMutex
  ---------------------------------------------------------------------}

{$IFNDEF NO_MUTEX_SUPPORT}

constructor TMutex.Create(aUseCOMWait: Boolean = False);

begin
  Create(Nil,False,'',aUseComWait);
end;

constructor TMutex.Create(aAttributes: PSecurityAttributes; aInitialOwner: Boolean; const aName: string; aUseCOMWait: Boolean = False);

{$IFDEF UNIX}
var
  Mattr: pthread_mutexattr_t;
{$ENDIF}
{$IFDEF WINDOWS}
var
  PN : PChar;
{$ENDIF}

begin
{$IFDEF UNIX}
  inherited Create;
  if (aName<>'') then
    raise ESyncObjectException.Create(SErrNamesNotSupported);
  CheckOSError(pthread_mutexattr_init(@Mattr));
  try
    CheckOSError(pthread_mutexattr_settype(@Mattr,Ord(PTHREAD_MUTEX_RECURSIVE)));
    CheckOSError(pthread_mutex_init(@FMutex,@Mattr));
  finally
    pthread_mutexattr_destroy(@Mattr); // don't raise second error, it would hide the first
  end;
  if aInitialOwner then
    Acquire;
{$ELSE}
{$IFDEF WINDOWS}
  inherited Create(aUseCOMWait);
  PN:=PChar(Pointer(aName));
  {$IF SIZEOF(CHAR)=1}
  FHandle:=TEventHandle(CreateMutexA(aAttributes,aInitialOwner,PN));
  {$ELSE}
  FHandle:=TEventHandle(CreateMutexW(aAttributes,aInitialOwner,PN));
  {$ENDIF}
  if (FHandle=TEventHandle(0)) then
    RaiseLastOSError;
{$ELSE}
{$IFDEF CPUWASM}
  inherited Create;
  if (aName <> '') then
    raise ESyncObjectException.Create(SErrNamesNotSupported);
  // InitCriticalSection creates a recursive mutex (mkRecursive),
  // matching the UNIX behavior (PTHREAD_MUTEX_RECURSIVE)
  InitCriticalSection(FMutex);
  if aInitialOwner then
    Acquire;
{$ELSE}
  raise ESyncObjectException.Create(SErrMutexNotSupported);
{$ENDIF CPUWASM}
{$ENDIF WINDOWS}
{$ENDIF UNIX}
end;

constructor TMutex.Create(aAccess: Cardinal; aInherit: Boolean; const aName: string; aUseCOMWait: Boolean = False);

{$IFDEF WINDOWS}
var
  PN : PChar;
{$ENDIF}

begin
{$IFDEF UNIX}
  Create(nil,false,aName,aUseCOMWait);
{$ELSE}
{$IFDEF WINDOWS}
  inherited Create(aUseCOMWait);
  PN:=PChar(Pointer(aName));
{$IF SIZEOF(CHAR)=1}
  FHandle:=TEventHandle(OpenMutexA(aAccess,aInherit,PN));
{$ELSE}
  FHandle:=TEventHandle(OpenMutexW(aAccess,aInherit,PN));
{$ENDIF}
  if FHandle=TEventHandle(0) then
    RaiseLastOSError;
{$ELSE}
{$IFDEF CPUWASM}
  Create(nil, false, aName, aUseCOMWait);
{$ELSE}
  raise ESyncObjectException.Create(SErrMutexNotSupported);
{$ENDIF CPUWASM}
{$ENDIF WINDOWS}
{$ENDIF UNIX}
end;

destructor TMutex.Destroy;

begin
{$IFDEF UNIX}
   pthread_mutex_destroy(@FMutex);
{$ENDIF}
{$IFDEF CPUWASM}
  DoneCriticalSection(FMutex);
{$ENDIF}
  Inherited;
end;


function TMutex.WaitFor(aTimeout: Cardinal): TWaitResult;

{$IFDEF UNIX}
var
  td,tm,Errno: Integer;
  {$IFDEF USE_pthread_mutex_trylock}
  tnew : timeval;
  {$ENDIF}
  tnow: ttimeval;
  Tmp: timespec;
{$ENDIF}
{$IFDEF CPUWASM32}
var
  EndTime : Int64;
{$ENDIF}
begin
{$IFDEF CPUWASM32}
    // TRTLCriticalSection only supports blocking acquire (no timeout).
    // For INFINITE, use EnterCriticalSection directly.
    // For timeout=0 (try-lock), use TryEnterCriticalSection.
    // Other timeouts: spin with TryEnterCriticalSection.
    if aTimeout = INFINITE then
    begin
      EnterCriticalSection(FMutex);
      Result := wrSignaled;
    end
    else if aTimeout = 0 then
    begin
      if TryEnterCriticalSection(FMutex) <> 0 then
        Result := wrSignaled
      else
        Result := wrTimeout;
    end
    else
    begin
      // Spin-wait with timeout (no native timed lock in WASM RTL)
      Result := wrTimeout;
      // LockMutexTimeout from wasmmutex.inc supports timed waits,
      // but it's not exported. Use a simple spin loop:
      EndTime := GetTickCount64 + aTimeout;
      repeat
        if TryEnterCriticalSection(FMutex) <> 0 then
        begin
          Result := wrSignaled;
          Break;
        end;
        ThreadSwitch; // yield to other web workers
      until GetTickCount64 >= EndTime;
    end;
{$ELSE}
{$IFNDEF UNIX}
  Result:=Inherited WaitFor(aTimeOut);
{$ELSE}
  Result:=wrError;
  if (aTimeout=0) then
    begin
    ErrNo:=pthread_mutex_trylock(@FMutex);
    if ErrNo=0 then
      Result:=wrSignaled
    else if (Errno=ESysEAGAIN) then
      Result:=wrTimeout
    end
  else if (aTimeout<>INFINITE) then
    begin
    // Todo: maybe write small helper function, reuse it here and in semaphore
    fpgettimeofday(@tnow,Nil);
    {$IFNDEF USE_pthread_mutex_trylock }
    MsecsFromNow(tnow,aTimeOut,tmp);
    ErrNo:=pthread_mutex_timedlock(@FMutex,@tmp);
    {$ELSE}
    Repeat
      ErrNo:=pthread_mutex_trylock(@FMutex);
      if ErrNo=ESysEAGAIN then
        begin
        Sleep(10);
        fpgettimeofday(@tnew,Nil);
        if MSecsBetween(tnew,tnow)>aTimeOut then
          errNo:=ESysETIMEDOUT;
        end
      else if ErrNo=ESysEINTR then
        ErrNo:=ESysEAGAIN;
    until (ErrNo<>ESysEAGAIN);
    {$ENDIF}
    if ErrNo=0 then
      Result:=wrSignaled
    else if (Errno=ESysEBUSY) or (errNo=ESysETIMEDOUT) then
      Result:=wrTimeout
    end
  else
    begin
    if (pthread_mutex_lock(@FMutex)=0) then
      Result:=wrSignaled
    end;
{$ENDIF}
{$ENDIF}
end;

procedure TMutex.Acquire;

begin
  if WaitFor(INFINITE)=wrError then
    RaiseLastOSError;
end;

procedure TMutex.Release;

begin
{$IFDEF WINDOWS}
  if not ReleaseMutex(PtrUInt(FHandle)) then
    RaiseLastOSError;
{$ENDIF WINDOWS}
{$IFDEF UNIX}
  CheckOSError(pthread_mutex_unlock(@FMutex));
{$ENDIF UNIX}
{$IFDEF CPUWASM32}
  LeaveCriticalSection(FMutex);
{$ENDIF CPUWASM32}
end;
{$ENDIF NO_MUTEX_SUPPORT}

{ ---------------------------------------------------------------------
  Condition variable waiter queue
  ---------------------------------------------------------------------}

type
  PCondVarWaiter = ^TCondVarWaiter;
  TCondVarWaiter = record
    Event: PRTLEvent;
    Next: PCondVarWaiter;
  end;

  PCondVarQueue = ^TCondVarQueue;
  TCondVarQueue = record
    Lock: TRTLCriticalSection;
    Head, Tail: PCondVarWaiter;
  end;


// Allocates an empty waiter queue
function CondVarQueueCreate: PCondVarQueue;

begin
  New(Result);
  InitCriticalSection(Result^.Lock);
  Result^.Head:=nil;
  Result^.Tail:=nil;
end;


// Frees aQueue
procedure CondVarQueueFree(aQueue: PCondVarQueue);

begin
  DoneCriticalSection(aQueue^.Lock);
  Dispose(aQueue);
end;


// Gives aWaiter a new event and appends it to aQueue
procedure CondVarQueueAdd(aQueue: PCondVarQueue; var aWaiter: TCondVarWaiter);

begin
  aWaiter.Event:=RTLEventCreate;
  aWaiter.Next:=nil;
  EnterCriticalSection(aQueue^.Lock);
  if aQueue^.Tail=nil then
    aQueue^.Head:=@aWaiter
  else
    aQueue^.Tail^.Next:=@aWaiter;
  aQueue^.Tail:=@aWaiter;
  LeaveCriticalSection(aQueue^.Lock);
end;


// Removes aWaiter from aQueue, whose lock is held; False when it is no longer queued
function CondVarQueueRemove(aQueue: PCondVarQueue; var aWaiter: TCondVarWaiter): Boolean;

var
  lPrev, lCur: PCondVarWaiter;

begin
  lPrev:=nil;
  lCur:=aQueue^.Head;
  while (lCur<>nil) and (lCur<>@aWaiter) do
    begin
    lPrev:=lCur;
    lCur:=lCur^.Next;
    end;
  Result:=lCur<>nil;
  if not Result then
    exit;
  if lPrev=nil then
    aQueue^.Head:=lCur^.Next
  else
    lPrev^.Next:=lCur^.Next;
  if aQueue^.Tail=lCur then
    aQueue^.Tail:=lPrev;
end;


// Waits until aWaiter is woken or aTimeout milliseconds have passed, and frees its event
function CondVarQueueWait(aQueue: PCondVarQueue; var aWaiter: TCondVarWaiter; aTimeout: Cardinal): TWaitResult;

var
  lTimeout: LongInt;

begin
  if aTimeout=INFINITE then
    begin
    RTLEventWaitFor(aWaiter.Event);
    Result:=wrSignaled;
    end
  else
    begin
    if aTimeout>Cardinal(High(LongInt)) then
      lTimeout:=High(LongInt)
    else
      lTimeout:=aTimeout;
    if RTLEventWaitFor(aWaiter.Event,lTimeout) then
      Result:=wrSignaled
    else
      Result:=wrTimeout;
    end;
  // Waits until a concurrent CondVarQueueWake has finished setting the event
  EnterCriticalSection(aQueue^.Lock);
  if (Result=wrTimeout) and not CondVarQueueRemove(aQueue,aWaiter) then
    Result:=wrSignaled;
  LeaveCriticalSection(aQueue^.Lock);
  RTLEventDestroy(aWaiter.Event);
end;


// Wakes the first waiter of aQueue, or all of them when aAll is True
procedure CondVarQueueWake(aQueue: PCondVarQueue; aAll: Boolean);

var
  lWaiter: PCondVarWaiter;

begin
  EnterCriticalSection(aQueue^.Lock);
  repeat
    lWaiter:=aQueue^.Head;
    if lWaiter=nil then
      break;
    aQueue^.Head:=lWaiter^.Next;
    if aQueue^.Head=nil then
      aQueue^.Tail:=nil;
    RTLEventSetEvent(lWaiter^.Event);
  until not aAll;
  LeaveCriticalSection(aQueue^.Lock);
end;


{$IFDEF NATIVE_CONDVAR_CS}
type
  TCondVarProc = procedure(aCondition: Pointer); stdcall;
  TSleepCondVarCS = function(aCondition, aCriticalSection: Pointer; aMilliseconds: DWord): BOOL; stdcall;

var
  gInitializeConditionVariable: TCondVarProc;
  gWakeConditionVariable: TCondVarProc;
  gWakeAllConditionVariable: TCondVarProc;
  gSleepConditionVariableCS: TSleepCondVarCS;


// Loads the condition variable functions of kernel32, available since Windows Vista
procedure LoadNativeCondVar;

var
  lKernel: HMODULE;

begin
  lKernel:=GetModuleHandle('kernel32.dll');
  if lKernel=0 then
    exit;
  Pointer(gInitializeConditionVariable):=GetProcAddress(lKernel,'InitializeConditionVariable');
  Pointer(gWakeConditionVariable):=GetProcAddress(lKernel,'WakeConditionVariable');
  Pointer(gWakeAllConditionVariable):=GetProcAddress(lKernel,'WakeAllConditionVariable');
  Pointer(gSleepConditionVariableCS):=GetProcAddress(lKernel,'SleepConditionVariableCS');
  if not (Assigned(gInitializeConditionVariable) and Assigned(gWakeConditionVariable) and
          Assigned(gWakeAllConditionVariable) and Assigned(gSleepConditionVariableCS)) then
    gSleepConditionVariableCS:=nil;
end;
{$ENDIF NATIVE_CONDVAR_CS}


{ ---------------------------------------------------------------------
  TConditionVariableMutex
  ---------------------------------------------------------------------}

{$IFNDEF NO_MUTEX_SUPPORT}

constructor TConditionVariableMutex.Create;

begin
  inherited Create;
{$IFDEF NATIVE_CONDVAR_MUTEX}
  CheckOSError(pthread_cond_init(@FCondition,nil));
{$ELSE}
  FQueue:=CondVarQueueCreate;
{$ENDIF}
end;


destructor TConditionVariableMutex.Destroy;

begin
{$IFDEF NATIVE_CONDVAR_MUTEX}
  pthread_cond_destroy(@FCondition);
{$ELSE}
  if Assigned(FQueue) then
    CondVarQueueFree(FQueue);
{$ENDIF}
  inherited Destroy;
end;


procedure TConditionVariableMutex.Acquire;

begin
  raise ESyncObjectException.Create(SErrCondVarAcquire);
end;


procedure TConditionVariableMutex.Release;

begin
{$IFDEF NATIVE_CONDVAR_MUTEX}
  pthread_cond_signal(@FCondition);
{$ELSE}
  CondVarQueueWake(FQueue,False);
{$ENDIF}
end;


procedure TConditionVariableMutex.ReleaseAll;

begin
{$IFDEF NATIVE_CONDVAR_MUTEX}
  pthread_cond_broadcast(@FCondition);
{$ELSE}
  CondVarQueueWake(FQueue,True);
{$ENDIF}
end;


function TConditionVariableMutex.WaitFor(aExternalMutex: TMutex; aTimeout: Cardinal): TWaitResult;

{$IFDEF NATIVE_CONDVAR_MUTEX}
var
  lErr: cint;
  lNow: TTimeVal;
  lUntil: TTimeSpec;
{$ELSE}
var
  lWaiter: TCondVarWaiter;
{$ENDIF}

begin
{$IFDEF NATIVE_CONDVAR_MUTEX}
  if aTimeout=INFINITE then
    lErr:=pthread_cond_wait(@FCondition,@aExternalMutex.FMutex)
  else
    begin
    fpgettimeofday(@lNow,nil);
    if aTimeout>Cardinal(High(Integer)) then
      aTimeout:=High(Integer);
    MSecsFromNow(lNow,aTimeout,lUntil);
    lErr:=pthread_cond_timedwait(@FCondition,@aExternalMutex.FMutex,@lUntil);
    end;
  if lErr=0 then
    Result:=wrSignaled
  else if lErr=ESysETIMEDOUT then
    Result:=wrTimeout
  else
    Result:=wrError;
{$ELSE}
  CondVarQueueAdd(FQueue,lWaiter);
  aExternalMutex.Release;
  Result:=CondVarQueueWait(FQueue,lWaiter,aTimeout);
  aExternalMutex.Acquire;
{$ENDIF}
end;

{$ENDIF NO_MUTEX_SUPPORT}


{ ---------------------------------------------------------------------
  TConditionVariableCS
  ---------------------------------------------------------------------}

constructor TConditionVariableCS.Create;

begin
  inherited Create;
{$IFDEF NATIVE_CONDVAR_CS}
  if Assigned(gSleepConditionVariableCS) then
    begin
    gInitializeConditionVariable(@FCondition);
    exit;
    end;
{$ENDIF}
  FQueue:=CondVarQueueCreate;
end;


destructor TConditionVariableCS.Destroy;

begin
  if Assigned(FQueue) then
    CondVarQueueFree(FQueue);
  inherited Destroy;
end;


procedure TConditionVariableCS.Acquire;

begin
  raise ESyncObjectException.Create(SErrCondVarAcquire);
end;


procedure TConditionVariableCS.Release;

begin
{$IFDEF NATIVE_CONDVAR_CS}
  if not Assigned(FQueue) then
    begin
    gWakeConditionVariable(@FCondition);
    exit;
    end;
{$ENDIF}
  CondVarQueueWake(FQueue,False);
end;


procedure TConditionVariableCS.ReleaseAll;

begin
{$IFDEF NATIVE_CONDVAR_CS}
  if not Assigned(FQueue) then
    begin
    gWakeAllConditionVariable(@FCondition);
    exit;
    end;
{$ENDIF}
  CondVarQueueWake(FQueue,True);
end;


function TConditionVariableCS.WaitFor(aCriticalSection: TCriticalSection; aTimeout: Cardinal): TWaitResult;

begin
  Result:=WaitFor(aCriticalSection.CriticalSection,aTimeout);
end;


function TConditionVariableCS.WaitFor(var aCriticalSection: TRTLCriticalSection; aTimeout: Cardinal): TWaitResult;

var
  lWaiter: TCondVarWaiter;

begin
{$IFDEF NATIVE_CONDVAR_CS}
  if not Assigned(FQueue) then
    begin
    if gSleepConditionVariableCS(@FCondition,@aCriticalSection,aTimeout) then
      Result:=wrSignaled
    else if GetLastError=ERROR_TIMEOUT then
      Result:=wrTimeout
    else
      Result:=wrError;
    exit;
    end;
{$ENDIF}
  CondVarQueueAdd(FQueue,lWaiter);
  LeaveCriticalSection(aCriticalSection);
  Result:=CondVarQueueWait(FQueue,lWaiter,aTimeout);
  EnterCriticalSection(aCriticalSection);
end;


{ ---------------------------------------------------------------------
  TLightweightEvent
  ---------------------------------------------------------------------}

// Returns the spin count of the lightweight objects: 1 on a single CPU, 10 otherwise
function DefaultLightweightSpinCount: Integer;

begin
  if TThread.ProcessorCount<=1 then
    Result:=1
  else
    Result:=10;
end;


// Returns the milliseconds left of aTimeout since aStart, 0 when it has passed; INFINITE stays INFINITE
function RemainingTimeout(aStart: QWord; aTimeout: Cardinal): Cardinal;

var
  lElapsed: QWord;

begin
  if aTimeout=INFINITE then
    Exit(INFINITE);
  lElapsed:=GetTickCount64-aStart;
  if lElapsed>=aTimeout then
    Result:=0
  else
    Result:=aTimeout-lElapsed;
end;


constructor TLightweightEvent.Create;

begin
  Create(False);
end;


constructor TLightweightEvent.Create(aInitialState: Boolean);

begin
  Create(aInitialState,DefaultLightweightSpinCount);
end;


constructor TLightweightEvent.Create(aInitialState: Boolean; aSpinCount: Integer);

begin
  inherited Create;
  if aSpinCount<0 then
    raise ESyncObjectException.CreateFmt(SErrSpinCountNegative,[aSpinCount]);
  FSpinCount:=aSpinCount;
  FState:=Ord(aInitialState);
  InitCriticalSection(FLock);
  FCondition:=TConditionVariableCS.Create;
end;


destructor TLightweightEvent.Destroy;

begin
  if Assigned(FCondition) then
    begin
    FCondition.Free;
    DoneCriticalSection(FLock);
    end;
  inherited Destroy;
end;


function TLightweightEvent.GetIsSet: Boolean;

begin
  Result:=InterlockedCompareExchange(FState,0,0)<>0;
end;


procedure TLightweightEvent.Acquire;

begin
  WaitFor(INFINITE);
end;


procedure TLightweightEvent.ResetEvent;

begin
  InterlockedExchange(FState,0);
end;


procedure TLightweightEvent.SetEvent;

begin
  InterlockedExchange(FState,1);
  if InterlockedCompareExchange(FBlockedCount,0,0)>0 then
    begin
    EnterCriticalSection(FLock);
    FCondition.ReleaseAll;
    LeaveCriticalSection(FLock);
    end;
end;


function TLightweightEvent.WaitFor(aTimeout: Cardinal): TWaitResult;

var
  lStart: QWord;
  lSpin: TSpinWait;
  lRemaining: Cardinal;
  i: Integer;

begin
  if GetIsSet then
    Exit(wrSignaled);
  if aTimeout=0 then
    Exit(wrTimeout);
  lStart:=GetTickCount64;
  lSpin.Reset;
  for i:=1 to FSpinCount do
    begin
    lSpin.SpinCycle;
    if GetIsSet then
      Exit(wrSignaled);
    end;
  Result:=wrSignaled;
  EnterCriticalSection(FLock);
  try
    InterlockedIncrement(FBlockedCount);
    while not GetIsSet do
      begin
      lRemaining:=RemainingTimeout(lStart,aTimeout);
      if lRemaining=0 then
        begin
        Result:=wrTimeout;
        Break;
        end;
      FCondition.WaitFor(FLock,lRemaining);
      end;
    InterlockedDecrement(FBlockedCount);
  finally
    LeaveCriticalSection(FLock);
  end;
end;


{ ---------------------------------------------------------------------
  TLightweightSemaphore
  ---------------------------------------------------------------------}

constructor TLightweightSemaphore.Create(aInitialCount: Integer; aMaxCount: Integer);

begin
  inherited Create;
  if (aMaxCount<1) or (aInitialCount<0) or (aInitialCount>aMaxCount) then
    raise ESyncObjectException.CreateFmt(SErrSemaphoreCounts,[aInitialCount,aMaxCount]);
  FCount:=aInitialCount;
  FMaxCount:=aMaxCount;
  FSpinCount:=DefaultLightweightSpinCount;
  InitCriticalSection(FLock);
  FCondition:=TConditionVariableCS.Create;
end;


destructor TLightweightSemaphore.Destroy;

begin
  if Assigned(FCondition) then
    begin
    FCondition.Free;
    DoneCriticalSection(FLock);
    end;
  inherited Destroy;
end;


function TLightweightSemaphore.TryTake: Boolean;

var
  lCount: LongInt;

begin
  repeat
    lCount:=InterlockedCompareExchange(FCount,0,0);
    if lCount<=0 then
      Exit(False);
  until InterlockedCompareExchange(FCount,lCount-1,lCount)=lCount;
  Result:=True;
end;


procedure TLightweightSemaphore.Acquire;

begin
  WaitFor(INFINITE);
end;


procedure TLightweightSemaphore.Release;

begin
  Release(1);
end;


function TLightweightSemaphore.Release(aReleaseCount: Integer): Integer;

var
  lBlocked: LongInt;
  i: Integer;

begin
  if aReleaseCount<1 then
    raise ESyncObjectException.CreateFmt(SErrInvalidReleaseCount,[aReleaseCount]);
  repeat
    Result:=InterlockedCompareExchange(FCount,0,0);
    if Result>FMaxCount-aReleaseCount then
      raise ESyncObjectException.CreateFmt(SErrSemaphoreOverflow,[aReleaseCount,FMaxCount]);
  until InterlockedCompareExchange(FCount,Result+aReleaseCount,Result)=Result;
  lBlocked:=InterlockedCompareExchange(FBlockedCount,0,0);
  if lBlocked>0 then
    begin
    EnterCriticalSection(FLock);
    if aReleaseCount>=lBlocked then
      FCondition.ReleaseAll
    else
      for i:=1 to aReleaseCount do
        FCondition.Release;
    LeaveCriticalSection(FLock);
    end;
end;


function TLightweightSemaphore.WaitFor(aTimeout: Cardinal): TWaitResult;

var
  lStart: QWord;
  lSpin: TSpinWait;
  lRemaining: Cardinal;
  i: Integer;

begin
  if TryTake then
    Exit(wrSignaled);
  if aTimeout=0 then
    Exit(wrTimeout);
  lStart:=GetTickCount64;
  lSpin.Reset;
  for i:=1 to FSpinCount do
    begin
    lSpin.SpinCycle;
    if TryTake then
      Exit(wrSignaled);
    end;
  Result:=wrSignaled;
  EnterCriticalSection(FLock);
  try
    InterlockedIncrement(FBlockedCount);
    while not TryTake do
      begin
      lRemaining:=RemainingTimeout(lStart,aTimeout);
      if lRemaining=0 then
        begin
        Result:=wrTimeout;
        Break;
        end;
      FCondition.WaitFor(FLock,lRemaining);
      end;
    InterlockedDecrement(FBlockedCount);
  finally
    LeaveCriticalSection(FLock);
  end;
end;


{ TLockGuard }

class operator TLockGuard.Initialize(var hdl: TLockGuard);
begin
  hdl.obj := nil;
end;

class operator TLockGuard.Finalize(var hdl: TLockGuard);
begin
  if (hdl.obj=nil) then
    exit;
  hdl.obj.Release();
end;

procedure TLockGuard.Init(AObj:T);
begin
  self.obj := AObj;
  self.obj.Acquire();
end;

{ ---------------------------------------------------------------------
  TSpinWait
  ---------------------------------------------------------------------}

class constructor TSpinWait.Create;
begin
  // Get the number of cores on the system once
  FCPUCount := GetCPUCount; // Available in the System unit
end;

procedure TSpinWait.Reset;
begin
  FCount := 0;
end;

procedure TSpinWait.SpinCycle;
var
  i: Integer;
  SpinCount: Integer;
begin
  // Increment cycle count
  Inc(FCount);
  if (FCPUCount > 1) and (FCount <= 10) then
    begin
    // Exponential backoff for first 10 cycles
    // Base 2 exponentially increasing spin count starting at 4
    SpinCount := 2 shl FCount;
    TThread.SpinWait(SpinCount);
    // Do not fallback to Single-CPU yet
    exit;
    end;
  // Single-CPU behaviour, or large spin cycle counts:
  // Sleep 1ms every 20 cycles
  if (FCount mod 20) = 0 then
    Sleep(1)
  // Sleep 0ms every 5 cycles
  else if (FCount mod 5) = 0 then
    Sleep(0)
  // All other cycles simply yield
  else
    TThread.Yield; // Yield execution to other threads
end;


function TSpinWait.GetNextSpinCycleWillYield: Boolean;
begin
  // If CPUCount > 1, the first 10 cycles are pure spin (no yield/sleep), so it will not yield.
  if (FCPUCount > 1) and (FCount < 10) then
    Exit(False);
  // Single-CPU logic check (for FCPUCount = 1, or FCPUCount > 1 and FCount >= 10)
  // The *next* count will be FCount + 1.
  Result := (((FCount + 1) mod 20) <> 0) // Not a Sleep(1) cycle
            and (((FCount + 1) mod 5) <> 0);  // Not a Sleep(0) cycle
end;


class procedure TSpinWait.SpinUntil(const aCondition: TSpinFunction); overload; static;
var
  lWait : TSpinWait;
begin
  // Spin indefinitely until aCondition returns True
  lWait.Reset;
  while not aCondition() do
    lWait.SpinCycle;
end;


class function TSpinWait.SpinUntil(const aCondition: TSpinFunction; aTimeout: Cardinal): Boolean; overload;
static;
begin
  Result:=SpinUntil(aCondition, TTimeSpan.FromMilliseconds(aTimeout));
end;


class function TSpinWait.SpinUntil(const aCondition: TSpinFunction; const aTimeout: TTimeSpan): Boolean; overload; static;
var
  lStartTime: TDateTime;
  lWaitTime,lElapsedTime: Int64;
  lWait: TSpinWait;
begin
  Result:=False;
  if aCondition() then
    Exit(True);
  lWaitTime:=Round(aTimeout.TotalMilliseconds);
  lWait.Reset;
  lStartTime:=Now;
  repeat
    lWait.SpinCycle;
    if aCondition() then
      Exit(True);
    lElapsedTime:=MilliSecondsBetween(Now,lStartTime);
  until (lElapsedTime >= lWaitTime);
end;

{ ---------------------------------------------------------------------
  TSpinLock
  ---------------------------------------------------------------------}


function TSpinLock.GetIsLocked: Boolean;

begin
  Result:=FLock <> 0;
end;


function TSpinLock.GetIsThreadTrackingEnabled: Boolean; 

begin
  Result:=FThreadTracking;
end;


function TSpinLock.GetIsOwnedByCurrentThread: Boolean;

begin
  Result:=(FOwningThread = GetCurrentThreadId);
end;


function TSpinLock.GetIsLockedByCurrentThread: Boolean;

begin
  Result:=GetIsThreadTrackingEnabled and GetIsLocked and GetIsOwnedByCurrentThread
end;


constructor TSpinLock.Create(EnableThreadTracking: Boolean);
 
begin
  FLock:=0;
  FOwningThread:=TThreadID(0);
  FRecursionCount:=0;
  FThreadTracking:=EnableThreadTracking;
end;


procedure TSpinLock.Enter;

var
  Spinner: TSpinWait;
  CT: TThreadID;
  
begin
  if FThreadTracking then
    begin
    CT:=GetCurrentThreadId;
    if (FOwningThread=CT) then
      begin
      Inc(FRecursionCount);
      System.Exit;
      end;
    end;

  Spinner.Reset;
  while TInterlocked.CompareExchange(FLock,1,0)<>0 do
    Spinner.SpinCycle;

  if FThreadTracking then
    begin
    FOwningThread:=GetCurrentThreadId;
    FRecursionCount:=1;
    end;
end;


procedure TSpinLock.Exit(PublishNow: Boolean);

var
  CT: TThreadID;

begin
  if FThreadTracking then
    begin
    CT:=GetCurrentThreadId;
    if (FOwningThread<>CT) then
      raise ELockException.Create('Thread does not own the lock');
    
    Dec(FRecursionCount);
    if (FRecursionCount>0) then 
      System.Exit;
    FOwningThread:=TThreadID(0);
    end;
  
  if PublishNow then
    TInterlocked.Exchange(FLock, 0)
  else
    begin
    // Not 100% sure about this one ?
    ReadWriteBarrier;
    FLock:=0;
    end;
end;


function TSpinLock.TryEnter: Boolean;

begin
  if FThreadTracking and IsOwnedByCurrentThread then
    begin
    Inc(FRecursionCount);
    System.Exit(True);
    end;

  Result:=TInterlocked.CompareExchange(FLock,1,0)=0;
  
  if Result and FThreadTracking then
  begin
    FOwningThread:=GetCurrentThreadId;
    FRecursionCount:=1;
  end;
end;

function TSpinLock.TryEnter(const Timeout: TTimeSpan): Boolean;
var
  LSpinner: TSpinWait;
  LStart: QWord;
  LTotalMs: QWord;
begin
  if TryEnter then 
    System.Exit(True);

  LTotalMs:=Round(Timeout.TotalMilliseconds);
  LStart:=GetTickCount64;
  LSpinner.Reset;

  while (GetTickCount64-LStart)<LTotalMs do
    begin
    LSpinner.SpinCycle;
    if TryEnter then 
      System.Exit(True);
    end;
  Result:=False;
end;


function TSpinLock.TryEnter(Timeout: Cardinal): Boolean;

begin
  Result:=TryEnter(TTimeSpan.FromMilliseconds(Timeout));
end;

{ ---------------------------------------------------------------------
  TGuardian
  ---------------------------------------------------------------------}


procedure TGuardian.Dismantle;
begin
  FIsDismantled := True;
end;

function TGuardian.GetIsDismantled: Boolean;
begin
  Result := FIsDismantled;
end;


{$IFDEF NATIVE_CONDVAR_CS}
initialization
  LoadNativeCondVar;
{$ENDIF}
end.
