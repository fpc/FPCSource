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
      function WaitFor(Timeout : Cardinal=INFINITE) : TWaitResult;overload; virtual;
      {$IFNDEF VER3_2}
      function WaitFor(const Timeout : TTimespan) : TWaitResult;overload;
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
{$ENDIF}

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

  { TCountdownEvent }

  TCountdownEvent = class(TSynchroObject)
  private
    FInitialCount: Integer;
    FCurrentCount: Integer;
    FEvent: TSimpleEvent;
    FLock: TSpinLock;
    function GetCurrentCount: Integer; inline;
    function GetIsSet: Boolean; inline;
  public
    constructor Create(aInitialCount: Integer = 1);
    destructor Destroy; override;
    function Signal(aSignalCount: Integer = 1): Boolean;
    procedure AddCount(aSignalCount: Integer = 1);
    function TryAddCount(aSignalCount: Integer = 1): Boolean;
    procedure Reset; overload;
    procedure Reset(aCount: Integer); overload;
    function WaitFor(aTimeout: Cardinal = INFINITE): TWaitResult; overload;
    {$IFNDEF VER3_2}
    function WaitFor(const aTimeout: TTimeSpan): TWaitResult; overload;
    {$ENDIF}
    property CurrentCount: Integer read GetCurrentCount;
    property InitialCount: Integer read FInitialCount;
    property IsSet: Boolean read GetIsSet;
  end;

  { TBarrier }

  TBarrier = class;
  TBarrierPhaseAction = reference to procedure(aBarrier: TBarrier);
  EBarrierPostPhaseException = class(ESyncObjectException);

  TBarrier = class(TSynchroObject)
  private
    FParticipantCount: Integer;
    FRemainingParticipants: Integer;
    FCurrentPhaseNumber: Int64;
    FPostPhaseAction: TBarrierPhaseAction;
    FLock: TSpinLock;
    FPhaseEvents: array[0..1] of TSimpleEvent;
    FPhaseHasException: array[0..1] of Boolean;
    FPhaseExceptions: array[0..1] of string;
    function GetCurrentPhaseNumber: Int64; inline;
    function GetParticipantCount: Integer; inline;
    function GetParticipantsRemaining: Integer; inline;
    procedure FinishPhase(aMyPhase: Int64);
  public
    constructor Create(aParticipantCount: Integer; const aPostPhaseAction: TBarrierPhaseAction = nil);
    destructor Destroy; override;
    function AddParticipant: Int64;
    function AddParticipants(aParticipantCount: Integer): Int64;
    procedure RemoveParticipant;
    procedure RemoveParticipants(aParticipantCount: Integer);
    function SignalAndWait(aTimeout: Cardinal = INFINITE): Boolean; overload;
    {$IFNDEF VER3_2}
    function SignalAndWait(const aTimeout: TTimeSpan): Boolean; overload;
    {$ENDIF}
    property CurrentPhaseNumber: Int64 read GetCurrentPhaseNumber;
    property ParticipantCount: Integer read GetParticipantCount;
    property ParticipantsRemaining: Integer read GetParticipantsRemaining;
  end;

  { TSlimReaderWriterLock }

  TSlimReaderWriterLock = class(TSynchroObject)
  private const
    WRITER_HELD = LongInt($40000000);
    READER_MASK = LongInt($000FFFFF);
  private
    FState: LongInt;
    FWaitingWriters: LongInt;
    FWaitingReaders: LongInt;
    FLock: TSpinLock;
    FReadEvent: TSimpleEvent;
    FWriteEvent: TSimpleEvent;
    function TryAcquireReadFast: Boolean; inline;
    function TryAcquireWriteFast: Boolean; inline;
    function GetReaderCount: Integer; inline;
    function GetIsWriteLocked: Boolean; inline;
  public
    constructor Create;
    destructor Destroy; override;
    procedure BeginRead;
    function TryBeginRead(aTimeout: Cardinal = 0): Boolean; overload;
    {$IFNDEF VER3_2}
    function TryBeginRead(const aTimeout: TTimeSpan): Boolean; overload;
    {$ENDIF}
    procedure EndRead;
    procedure BeginWrite;
    function TryBeginWrite(aTimeout: Cardinal = 0): Boolean; overload;
    {$IFNDEF VER3_2}
    function TryBeginWrite(const aTimeout: TTimeSpan): Boolean; overload;
    {$ENDIF}
    procedure EndWrite;
    property ReaderCount: Integer read GetReaderCount;
    property IsWriteLocked: Boolean read GetIsWriteLocked;
  end;

  { TSlimReadLockGuard }

  TSlimReadLockGuard = record
    Lock: TSlimReaderWriterLock;
    class operator Initialize(var aGuard: TSlimReadLockGuard);
    class operator Finalize(var aGuard: TSlimReadLockGuard);
    procedure Init(aLock: TSlimReaderWriterLock);
    procedure Release;
  end;

  { TSlimWriteLockGuard }

  TSlimWriteLockGuard = record
    Lock: TSlimReaderWriterLock;
    class operator Initialize(var aGuard: TSlimWriteLockGuard);
    class operator Finalize(var aGuard: TSlimWriteLockGuard);
    procedure Init(aLock: TSlimReaderWriterLock);
    procedure Release;
  end;

  { TChannel }

  EChannelClosedException = class(ESyncObjectException);

  TCustomChannel = class(TSynchroObject)
  protected
    FHead: Integer;
    FTail: Integer;
    FCount: Integer;
    FCapacity: Integer;
    FBounded: Boolean;
    FClosed: Boolean;
    FWaitingReceivers: Integer;
    FWaitingSenders: Integer;
    FLock: TSpinLock;
    FNotEmptyEvent: TSimpleEvent;
    FNotFullEvent: TSimpleEvent;
    procedure NotifyItemSent;
    procedure NotifyItemReceived;
    class procedure RaiseChannelClosed; static;
    function GetCount: Integer; inline;
    function GetIsClosed: Boolean;
  public
    constructor Create(aCapacity: Integer = 0);
    destructor Destroy; override;
    procedure Close;
    property Count: Integer read GetCount;
    property Capacity: Integer read FCapacity;
    property IsClosed: Boolean read GetIsClosed;
  end;

  generic TChannel<T> = class(TCustomChannel)
  public type
    TChannelEnumerator = record
    private
      FChannel: TChannel;
      FCurrent: T;
    public
      function MoveNext: Boolean; inline;
      property Current: T read FCurrent;
    end;
  private
    FBuffer: array of T;
    procedure GrowBuffer;
  public
    constructor Create(aCapacity: Integer = 0);
    procedure Send(const aItem: T);
    function TrySend(const aItem: T; aTimeout: Cardinal = 0): Boolean; overload;
    {$IFNDEF VER3_2}
    function TrySend(const aItem: T; const aTimeout: TTimeSpan): Boolean; overload;
    {$ENDIF}
    function Receive(out aItem: T): Boolean; overload;
    function Receive: T; overload;
    function TryReceive(out aItem: T; aTimeout: Cardinal = 0): Boolean; overload;
    {$IFNDEF VER3_2}
    function TryReceive(out aItem: T; const aTimeout: TTimeSpan): Boolean; overload;
    {$ENDIF}
    function GetEnumerator: TChannelEnumerator; inline;
  end;

implementation

{$ifdef MSWindows}
{$IFDEF FPC_DOTTEDUNITS}
uses WinApi.Windows;
{$ELSE}
uses Windows;
{$ENDIF}
{$endif}

// Compile with -dFPC_VALGRIND to annotate TSpinLock for Helgrind/DRD.
{$ifdef UNIX}
{$IFDEF FPC_DOTTEDUNITS}
uses UnixApi.Unix, UnixApi.Base, UnixApi.Pthreads;
{$ELSE}
uses unix, baseunix, pthreads{$IFDEF FPC_VALGRIND}, fpvalgrind{$ENDIF};
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
  SErrInvalidCount        = '%d is not a valid count';
  SErrCountdownAlreadyZero = 'CountdownEvent count is already zero or would become negative';
  SErrBarrierZeroParticipants = 'Barrier has no participants';
  SErrBarrierTooManyRemovals = 'Cannot remove more participants than are currently registered';
  SErrLockNotOwned        = 'Thread does not own the lock';
  SErrChannelClosed       = 'Channel is closed';

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
function THandleObject.WaitFor(const Timeout: TTimespan): TWaitResult;
begin
  result:=waitfor(round(timeout.TotalMilliseconds));
end;
{$ENDIF}

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
  SpinUntil(aCondition, TTimeSpan.FromMilliseconds(aTimeout));
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
  until (lElapsedTime >= lWaitTime) or lWait.NextSpinCycleWillYield;
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
  {$IFDEF FPC_VALGRIND}
  VgHappensAfter(@FLock);
  {$ENDIF}

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
  {$IFDEF FPC_VALGRIND}
  VgHappensBefore(@FLock);
  {$ENDIF}
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
  {$IFDEF FPC_VALGRIND}
  if Result then
    VgHappensAfter(@FLock);
  {$ENDIF}
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

{ ---------------------------------------------------------------------
  TCountdownEvent
  ---------------------------------------------------------------------}

constructor TCountdownEvent.Create(aInitialCount: Integer);
begin
  inherited Create;
  if aInitialCount < 0 then
    raise ESyncObjectException.CreateFmt(SErrInvalidCount, [aInitialCount]);
  FInitialCount := aInitialCount;
  FCurrentCount := aInitialCount;
  FLock := TSpinLock.Create(False);
  FEvent := TSimpleEvent.Create;
  {$IFDEF FPC_VALGRIND}
  VgBenignRace(@FCurrentCount, SizeOf(FCurrentCount));
  {$ENDIF}
  if aInitialCount = 0 then
    FEvent.SetEvent;
end;

destructor TCountdownEvent.Destroy;
begin
  FreeAndNil(FEvent);
  inherited Destroy;
end;

function TCountdownEvent.GetCurrentCount: Integer;
begin
  Result := InterlockedCompareExchange(FCurrentCount, 0, 0);
end;

function TCountdownEvent.GetIsSet: Boolean;
begin
  Result := GetCurrentCount = 0;
  if Result then
  begin
    ReadBarrier;
    {$IFDEF FPC_VALGRIND}
    VgHappensAfter(@FCurrentCount);
    {$ENDIF}
  end;
end;

function TCountdownEvent.Signal(aSignalCount: Integer): Boolean;
begin
  if aSignalCount <= 0 then
    raise ESyncObjectException.CreateFmt(SErrInvalidCount, [aSignalCount]);
  FLock.Enter;
  try
    if FCurrentCount < aSignalCount then
      raise ESyncObjectException.Create(SErrCountdownAlreadyZero);
    InterlockedExchangeAdd(FCurrentCount, -aSignalCount);
    Result := (FCurrentCount = 0);
    if Result then
    begin
      {$IFDEF FPC_VALGRIND}
      VgHappensBefore(@FCurrentCount);
      {$ENDIF}
      FEvent.SetEvent;
    end;
  finally
    FLock.Exit;
  end;
end;

function TCountdownEvent.TryAddCount(aSignalCount: Integer): Boolean;
begin
  if aSignalCount <= 0 then
    raise ESyncObjectException.CreateFmt(SErrInvalidCount, [aSignalCount]);
  FLock.Enter;
  try
    if FCurrentCount = 0 then
      Exit(False);
    InterlockedExchangeAdd(FCurrentCount, aSignalCount);
    Result := True;
  finally
    FLock.Exit;
  end;
end;

procedure TCountdownEvent.AddCount(aSignalCount: Integer);
begin
  if not TryAddCount(aSignalCount) then
    raise ESyncObjectException.Create(SErrCountdownAlreadyZero);
end;

procedure TCountdownEvent.Reset;
begin
  Reset(FInitialCount);
end;

procedure TCountdownEvent.Reset(aCount: Integer);
begin
  if aCount < 0 then
    raise ESyncObjectException.CreateFmt(SErrInvalidCount, [aCount]);
  FLock.Enter;
  try
    FInitialCount := aCount;
    InterlockedExchange(FCurrentCount, aCount);
    if aCount = 0 then
    begin
      {$IFDEF FPC_VALGRIND}
      VgHappensBefore(@FCurrentCount);
      {$ENDIF}
      FEvent.SetEvent;
    end
    else
      FEvent.ResetEvent;
  finally
    FLock.Exit;
  end;
end;

function TCountdownEvent.WaitFor(aTimeout: Cardinal): TWaitResult;
begin
  if GetIsSet then
    Exit(wrSignaled);
  if aTimeout = 0 then
    Exit(wrTimeout);
  Result := FEvent.WaitFor(aTimeout);
  if Result = wrSignaled then
  begin
    ReadBarrier;
    {$IFDEF FPC_VALGRIND}
    VgHappensAfter(@FCurrentCount);
    {$ENDIF}
  end;
end;

{$IFNDEF VER3_2}
function TCountdownEvent.WaitFor(const aTimeout: TTimeSpan): TWaitResult;
begin
  Result := WaitFor(Cardinal(Round(aTimeout.TotalMilliseconds)));
end;
{$ENDIF}

{ ---------------------------------------------------------------------
  TBarrier
  ---------------------------------------------------------------------}

constructor TBarrier.Create(aParticipantCount: Integer; const aPostPhaseAction: TBarrierPhaseAction);
begin
  inherited Create;
  if aParticipantCount < 0 then
    raise ESyncObjectException.CreateFmt(SErrInvalidCount, [aParticipantCount]);
  FParticipantCount := aParticipantCount;
  FRemainingParticipants := aParticipantCount;
  FCurrentPhaseNumber := 0;
  FPostPhaseAction := aPostPhaseAction;
  FLock := TSpinLock.Create(False);
  FPhaseEvents[0] := TSimpleEvent.Create;
  FPhaseEvents[1] := TSimpleEvent.Create;
  {$IFDEF FPC_VALGRIND}
  VgBenignRace(@FParticipantCount, SizeOf(FParticipantCount));
  VgBenignRace(@FRemainingParticipants, SizeOf(FRemainingParticipants));
  VgBenignRace(@FCurrentPhaseNumber, SizeOf(FCurrentPhaseNumber));
  {$ENDIF}
end;

destructor TBarrier.Destroy;
begin
  FreeAndNil(FPhaseEvents[0]);
  FreeAndNil(FPhaseEvents[1]);
  inherited Destroy;
end;

function TBarrier.GetCurrentPhaseNumber: Int64;
begin
  {$IFDEF CPU64}
  Result := InterlockedCompareExchange64(FCurrentPhaseNumber, 0, 0);
  {$ELSE}
  FLock.Enter;
  try
    Result := FCurrentPhaseNumber;
  finally
    FLock.Exit;
  end;
  {$ENDIF}
end;

function TBarrier.GetParticipantCount: Integer;
begin
  Result := InterlockedCompareExchange(FParticipantCount, 0, 0);
end;

function TBarrier.GetParticipantsRemaining: Integer;
begin
  Result := InterlockedCompareExchange(FRemainingParticipants, 0, 0);
end;

procedure TBarrier.FinishPhase(aMyPhase: Int64);
var
  Parity, NextParity: Integer;
  HasExc: Boolean;
  ExcMsg: string;
begin
  // Called with FLock held when FRemainingParticipants reaches 0.
  Parity := aMyPhase and 1;
  FPhaseHasException[Parity] := False;
  FPhaseExceptions[Parity] := '';
  FLock.Exit;
  try
    if Assigned(FPostPhaseAction) then
      try
        FPostPhaseAction(Self);
      except
        on E: Exception do
        begin
          FPhaseHasException[Parity] := True;
          FPhaseExceptions[Parity] := E.Message;
        end;
      end;
  finally
    FLock.Enter;
    NextParity := (aMyPhase + 1) and 1;
    FPhaseEvents[NextParity].ResetEvent;
    FRemainingParticipants := FParticipantCount;
    {$IFDEF CPU64}
    InterlockedIncrement64(FCurrentPhaseNumber);
    {$ELSE}
    Inc(FCurrentPhaseNumber);
    {$ENDIF}
    {$IFDEF FPC_VALGRIND}
    VgHappensBefore(@FPhaseEvents[Parity]);
    {$ENDIF}
    FPhaseEvents[Parity].SetEvent;
    HasExc := FPhaseHasException[Parity];
    ExcMsg := FPhaseExceptions[Parity];
    FLock.Exit;
  end;
  if HasExc then
    raise EBarrierPostPhaseException.Create(ExcMsg);
end;

function TBarrier.AddParticipant: Int64;
begin
  Result := AddParticipants(1);
end;

function TBarrier.AddParticipants(aParticipantCount: Integer): Int64;
begin
  if aParticipantCount <= 0 then
    raise ESyncObjectException.CreateFmt(SErrInvalidCount, [aParticipantCount]);
  FLock.Enter;
  try
    Inc(FParticipantCount, aParticipantCount);
    Inc(FRemainingParticipants, aParticipantCount);
    Result := FCurrentPhaseNumber;
  finally
    FLock.Exit;
  end;
end;

procedure TBarrier.RemoveParticipant;
begin
  RemoveParticipants(1);
end;

procedure TBarrier.RemoveParticipants(aParticipantCount: Integer);
var
  MyPhase: Int64;
begin
  if aParticipantCount <= 0 then
    raise ESyncObjectException.CreateFmt(SErrInvalidCount, [aParticipantCount]);
  FLock.Enter;
  if (FParticipantCount < aParticipantCount) or (FRemainingParticipants < aParticipantCount) then
  begin
    FLock.Exit;
    raise ESyncObjectException.Create(SErrBarrierTooManyRemovals);
  end;
  Dec(FParticipantCount, aParticipantCount);
  Dec(FRemainingParticipants, aParticipantCount);
  if (FParticipantCount > 0) and (FRemainingParticipants = 0) then
  begin
    MyPhase := FCurrentPhaseNumber;
    FinishPhase(MyPhase); // Releases FLock
  end
  else
    FLock.Exit;
end;

function TBarrier.SignalAndWait(aTimeout: Cardinal): Boolean;
var
  MyPhase: Int64;
  Parity: Integer;
  StartTick, Elapsed, Rem: QWord;
  HasExc: Boolean;
  ExcMsg: string;
begin
  FLock.Enter;
  if FParticipantCount = 0 then
  begin
    FLock.Exit;
    raise ESyncObjectException.Create(SErrBarrierZeroParticipants);
  end;
  MyPhase := FCurrentPhaseNumber;
  Parity := MyPhase and 1;
  {$IFDEF FPC_VALGRIND}
  VgHappensBefore(@FPhaseEvents[Parity]);
  {$ENDIF}
  Dec(FRemainingParticipants);
  if FRemainingParticipants = 0 then
  begin
    FinishPhase(MyPhase); // Releases FLock
    Exit(True);
  end;
  FLock.Exit;

  StartTick := GetTickCount64;
  while GetCurrentPhaseNumber = MyPhase do
  begin
    if aTimeout = 0 then
      Rem := 0
    else if aTimeout = INFINITE then
      Rem := INFINITE
    else
    begin
      Elapsed := GetTickCount64 - StartTick;
      if Elapsed >= aTimeout then
        Rem := 0
      else
        Rem := aTimeout - Elapsed;
    end;

    if (Rem = 0) or (FPhaseEvents[Parity].WaitFor(Rem) <> wrSignaled) then
    begin
      FLock.Enter;
      if FCurrentPhaseNumber = MyPhase then
      begin
        Inc(FRemainingParticipants);
        FLock.Exit;
        Exit(False);
      end;
      FLock.Exit;
      Break;
    end;
  end;

  {$IFDEF FPC_VALGRIND}
  VgHappensAfter(@FPhaseEvents[Parity]);
  {$ENDIF}
  FLock.Enter;
  HasExc := FPhaseHasException[Parity];
  ExcMsg := FPhaseExceptions[Parity];
  FLock.Exit;
  if HasExc then
    raise EBarrierPostPhaseException.Create(ExcMsg);
  Result := True;
end;

{$IFNDEF VER3_2}
function TBarrier.SignalAndWait(const aTimeout: TTimeSpan): Boolean;
begin
  Result := SignalAndWait(Cardinal(Round(aTimeout.TotalMilliseconds)));
end;
{$ENDIF}

{ ---------------------------------------------------------------------
  TSlimReaderWriterLock
  ---------------------------------------------------------------------}

constructor TSlimReaderWriterLock.Create;
begin
  inherited Create;
  FState := 0;
  FWaitingWriters := 0;
  FWaitingReaders := 0;
  FLock := TSpinLock.Create(False);
  FReadEvent := TSimpleEvent.Create;
  FWriteEvent := TSimpleEvent.Create;
  {$IFDEF FPC_VALGRIND}
  VgBenignRace(@FState, SizeOf(FState));
  VgBenignRace(@FWaitingWriters, SizeOf(FWaitingWriters));
  VgBenignRace(@FWaitingReaders, SizeOf(FWaitingReaders));
  {$ENDIF}
end;

destructor TSlimReaderWriterLock.Destroy;
begin
  FreeAndNil(FReadEvent);
  FreeAndNil(FWriteEvent);
  inherited Destroy;
end;

function TSlimReaderWriterLock.GetReaderCount: Integer;
begin
  Result := InterlockedCompareExchange(FState, 0, 0) and READER_MASK;
end;

function TSlimReaderWriterLock.GetIsWriteLocked: Boolean;
begin
  Result := (InterlockedCompareExchange(FState, 0, 0) and WRITER_HELD) <> 0;
end;

function TSlimReaderWriterLock.TryAcquireReadFast: Boolean;
var
  S: LongInt;
begin
  if FWaitingWriters > 0 then
    Exit(False);
  repeat
    S := FState;
    if (S and WRITER_HELD) <> 0 then
      Exit(False);
    if FWaitingWriters > 0 then
      Exit(False);
  until InterlockedCompareExchange(FState, S + 1, S) = S;
  {$IFDEF FPC_VALGRIND}
  VgHappensAfter(@FState);
  {$ENDIF}
  Result := True;
end;

function TSlimReaderWriterLock.TryAcquireWriteFast: Boolean;
begin
  Result := InterlockedCompareExchange(FState, WRITER_HELD, 0) = 0;
  {$IFDEF FPC_VALGRIND}
  if Result then
    VgHappensAfter(@FState);
  {$ENDIF}
end;

procedure TSlimReaderWriterLock.BeginRead;
begin
  if not TryAcquireReadFast then
    TryBeginRead(INFINITE);
end;

function TSlimReaderWriterLock.TryBeginRead(aTimeout: Cardinal): Boolean;
var
  Spinner: TSpinWait;
  StartTick, Elapsed, Rem: QWord;
  I: Integer;
begin
  if TryAcquireReadFast then
    Exit(True);

  Spinner.Reset;
  for I := 1 to 10 do
  begin
    if FWaitingWriters > 0 then
      Break;
    Spinner.SpinCycle;
    if TryAcquireReadFast then
      Exit(True);
  end;

  if aTimeout = 0 then
    Exit(False);

  if aTimeout <> INFINITE then
    StartTick := GetTickCount64
  else
    StartTick := 0;

  while True do
  begin
    FLock.Enter;
    if TryAcquireReadFast then
    begin
      if (FWaitingReaders > 0) and (FWaitingWriters = 0) then
        FReadEvent.SetEvent;
      FLock.Exit;
      Exit(True);
    end;
    FReadEvent.ResetEvent;
    if TryAcquireReadFast then
    begin
      if (FWaitingReaders > 0) and (FWaitingWriters = 0) then
        FReadEvent.SetEvent;
      FLock.Exit;
      Exit(True);
    end;
    InterlockedIncrement(FWaitingReaders);
    FLock.Exit;

    if aTimeout = INFINITE then
      Rem := INFINITE
    else
    begin
      Elapsed := GetTickCount64 - StartTick;
      if Elapsed >= aTimeout then
        Rem := 0
      else
        Rem := aTimeout - Elapsed;
    end;

    if Rem > 0 then
      FReadEvent.WaitFor(Rem);

    FLock.Enter;
    InterlockedDecrement(FWaitingReaders);
    Result := TryAcquireReadFast;
    if Result and (FWaitingReaders > 0) and (FWaitingWriters = 0) then
      FReadEvent.SetEvent;
    FLock.Exit;

    if Result then
      Exit(True);
    if (aTimeout <> INFINITE) and ((GetTickCount64 - StartTick) >= aTimeout) then
      Exit(False);
  end;
end;

{$IFNDEF VER3_2}
function TSlimReaderWriterLock.TryBeginRead(const aTimeout: TTimeSpan): Boolean;
begin
  Result := TryBeginRead(Cardinal(Round(aTimeout.TotalMilliseconds)));
end;
{$ENDIF}

procedure TSlimReaderWriterLock.EndRead;
var
  NewState: LongInt;
begin
  {$IFDEF FPC_VALGRIND}
  VgHappensBefore(@FState);
  {$ENDIF}
  NewState := InterlockedDecrement(FState);
  if NewState < 0 then
    raise ELockException.Create(SErrLockNotOwned);
  if (NewState = 0) and (FWaitingWriters > 0) then
  begin
    FLock.Enter;
    try
      if FWaitingWriters > 0 then
        FWriteEvent.SetEvent;
    finally
      FLock.Exit;
    end;
  end;
end;

procedure TSlimReaderWriterLock.BeginWrite;
begin
  if not TryAcquireWriteFast then
    TryBeginWrite(INFINITE);
end;

function TSlimReaderWriterLock.TryBeginWrite(aTimeout: Cardinal): Boolean;
var
  Spinner: TSpinWait;
  StartTick, Elapsed, Rem: QWord;
  I: Integer;
begin
  if TryAcquireWriteFast then
    Exit(True);

  if aTimeout = 0 then
    Exit(False);

  InterlockedIncrement(FWaitingWriters);
  try
    Spinner.Reset;
    for I := 1 to 10 do
    begin
      if TryAcquireWriteFast then
        Exit(True);
      Spinner.SpinCycle;
    end;

    if aTimeout <> INFINITE then
      StartTick := GetTickCount64
    else
      StartTick := 0;

    FLock.Enter;
    try
      while True do
      begin
        if TryAcquireWriteFast then
          Exit(True);
        FWriteEvent.ResetEvent;
        if TryAcquireWriteFast then
          Exit(True);

        if aTimeout = INFINITE then
          Rem := INFINITE
        else
        begin
          Elapsed := GetTickCount64 - StartTick;
          if Elapsed >= aTimeout then
            Exit(False);
          Rem := aTimeout - Elapsed;
        end;

        FLock.Exit;
        FWriteEvent.WaitFor(Rem);
        FLock.Enter;

        if TryAcquireWriteFast then
          Exit(True);
        if (aTimeout <> INFINITE) and ((GetTickCount64 - StartTick) >= aTimeout) then
          Exit(False);
      end;
    finally
      FLock.Exit;
    end;
  finally
    if InterlockedDecrement(FWaitingWriters) = 0 then
      if FWaitingReaders > 0 then
      begin
        FLock.Enter;
        try
          if (FWaitingReaders > 0) and (FWaitingWriters = 0) and ((FState and WRITER_HELD) = 0) then
            FReadEvent.SetEvent;
        finally
          FLock.Exit;
        end;
      end;
  end;
end;

{$IFNDEF VER3_2}
function TSlimReaderWriterLock.TryBeginWrite(const aTimeout: TTimeSpan): Boolean;
begin
  Result := TryBeginWrite(Cardinal(Round(aTimeout.TotalMilliseconds)));
end;
{$ENDIF}

procedure TSlimReaderWriterLock.EndWrite;
begin
  {$IFDEF FPC_VALGRIND}
  VgHappensBefore(@FState);
  {$ENDIF}
  if InterlockedCompareExchange(FState, 0, WRITER_HELD) <> WRITER_HELD then
    raise ELockException.Create(SErrLockNotOwned);
  if (FWaitingWriters > 0) or (FWaitingReaders > 0) then
  begin
    FLock.Enter;
    try
      if FWaitingWriters > 0 then
        FWriteEvent.SetEvent
      else if FWaitingReaders > 0 then
        FReadEvent.SetEvent;
    finally
      FLock.Exit;
    end;
  end;
end;

{ TSlimReadLockGuard }

class operator TSlimReadLockGuard.Initialize(var aGuard: TSlimReadLockGuard);
begin
  aGuard.Lock := nil;
end;

class operator TSlimReadLockGuard.Finalize(var aGuard: TSlimReadLockGuard);
begin
  aGuard.Release;
end;

procedure TSlimReadLockGuard.Init(aLock: TSlimReaderWriterLock);
begin
  Release;
  Lock := aLock;
  Lock.BeginRead;
end;

procedure TSlimReadLockGuard.Release;
begin
  if Lock <> nil then
  begin
    Lock.EndRead;
    Lock := nil;
  end;
end;

{ TSlimWriteLockGuard }

class operator TSlimWriteLockGuard.Initialize(var aGuard: TSlimWriteLockGuard);
begin
  aGuard.Lock := nil;
end;

class operator TSlimWriteLockGuard.Finalize(var aGuard: TSlimWriteLockGuard);
begin
  aGuard.Release;
end;

procedure TSlimWriteLockGuard.Init(aLock: TSlimReaderWriterLock);
begin
  Release;
  Lock := aLock;
  Lock.BeginWrite;
end;

procedure TSlimWriteLockGuard.Release;
begin
  if Lock <> nil then
  begin
    Lock.EndWrite;
    Lock := nil;
  end;
end;

{ ---------------------------------------------------------------------
  TCustomChannel & TChannel<T>
  ---------------------------------------------------------------------}

constructor TCustomChannel.Create(aCapacity: Integer);
begin
  inherited Create;
  if aCapacity < 0 then
    raise ESyncObjectException.CreateFmt(SErrInvalidCount, [aCapacity]);
  FCapacity := aCapacity;
  FBounded := aCapacity > 0;
  FHead := 0;
  FTail := 0;
  FCount := 0;
  FClosed := False;
  FWaitingReceivers := 0;
  FWaitingSenders := 0;
  FLock := TSpinLock.Create(False);
  FNotEmptyEvent := TSimpleEvent.Create;
  FNotFullEvent := TSimpleEvent.Create;
  FNotFullEvent.SetEvent;
  {$IFDEF FPC_VALGRIND}
  VgBenignRace(@FCount, SizeOf(FCount));
  VgBenignRace(@FClosed, SizeOf(FClosed));
  {$ENDIF}
end;

destructor TCustomChannel.Destroy;
begin
  Close;
  FreeAndNil(FNotEmptyEvent);
  FreeAndNil(FNotFullEvent);
  inherited Destroy;
end;

procedure TCustomChannel.NotifyItemSent;
begin
  {$IFDEF FPC_VALGRIND}
  VgHappensBefore(@FNotEmptyEvent);
  {$ENDIF}
  if FWaitingReceivers > 0 then
    FNotEmptyEvent.SetEvent;
end;

procedure TCustomChannel.NotifyItemReceived;
begin
  {$IFDEF FPC_VALGRIND}
  VgHappensAfter(@FNotEmptyEvent);
  {$ENDIF}
  if FWaitingSenders > 0 then
    FNotFullEvent.SetEvent;
end;

class procedure TCustomChannel.RaiseChannelClosed;
begin
  raise EChannelClosedException.Create(SErrChannelClosed);
end;

function TCustomChannel.GetCount: Integer;
begin
  Result := InterlockedCompareExchange(FCount, 0, 0);
end;

function TCustomChannel.GetIsClosed: Boolean;
begin
  FLock.Enter;
  try
    Result := FClosed;
  finally
    FLock.Exit;
  end;
end;

procedure TCustomChannel.Close;
begin
  FLock.Enter;
  try
    if FClosed then
      Exit;
    FClosed := True;
    {$IFDEF FPC_VALGRIND}
    VgHappensBefore(@FNotEmptyEvent);
    {$ENDIF}
    FNotEmptyEvent.SetEvent;
    FNotFullEvent.SetEvent;
  finally
    FLock.Exit;
  end;
end;

function TChannel.TChannelEnumerator.MoveNext: Boolean;
begin
  Result := FChannel.Receive(FCurrent);
end;

constructor TChannel.Create(aCapacity: Integer);
begin
  inherited Create(aCapacity);
  if FBounded then
    SetLength(FBuffer, aCapacity)
  else
    SetLength(FBuffer, 16);
end;

procedure TChannel.GrowBuffer;
var
  NewCap, I, Idx: Integer;
  NewBuf: array of T;
begin
  NewCap := Length(FBuffer) * 2;
  if NewCap < 16 then
    NewCap := 16;
  SetLength(NewBuf, NewCap);
  Idx := FHead;
  for I := 0 to FCount - 1 do
  begin
    NewBuf[I] := FBuffer[Idx];
    Idx := (Idx + 1) mod Length(FBuffer);
  end;
  FBuffer := NewBuf;
  FHead := 0;
  FTail := FCount;
end;

function TChannel.TrySend(const aItem: T; aTimeout: Cardinal): Boolean;
var
  StartTick, Elapsed, Rem: QWord;
  HasStartTick: Boolean;
begin
  HasStartTick := False;
  StartTick := 0;
  while True do
  begin
    FLock.Enter;
    if FClosed then
    begin
      FLock.Exit;
      Exit(False);
    end;
    if (not FBounded) or (FCount < FCapacity) then
    begin
      if (not FBounded) and (FCount = Length(FBuffer)) then
        GrowBuffer;
      FBuffer[FTail] := aItem;
      FTail := (FTail + 1) mod Length(FBuffer);
      Inc(FCount);
      NotifyItemSent;
      FLock.Exit;
      Exit(True);
    end;
    if aTimeout = 0 then
    begin
      FLock.Exit;
      Exit(False);
    end;
    Inc(FWaitingSenders);
    FNotFullEvent.ResetEvent;
    FLock.Exit;

    if aTimeout = INFINITE then
      Rem := INFINITE
    else
    begin
      if not HasStartTick then
      begin
        StartTick := GetTickCount64;
        HasStartTick := True;
      end;
      Elapsed := GetTickCount64 - StartTick;
      if Elapsed >= aTimeout then
      begin
        FLock.Enter;
        Dec(FWaitingSenders);
        FLock.Exit;
        Exit(False);
      end;
      Rem := aTimeout - Elapsed;
    end;

    FNotFullEvent.WaitFor(Rem);

    FLock.Enter;
    Dec(FWaitingSenders);
    if (FWaitingSenders > 0) and ((not FBounded) or (FCount < FCapacity) or FClosed) then
      FNotFullEvent.SetEvent;
    FLock.Exit;
  end;
end;

{$IFNDEF VER3_2}
function TChannel.TrySend(const aItem: T; const aTimeout: TTimeSpan): Boolean;
begin
  Result := TrySend(aItem, Cardinal(Round(aTimeout.TotalMilliseconds)));
end;
{$ENDIF}

procedure TChannel.Send(const aItem: T);
begin
  if not TrySend(aItem, INFINITE) then
    RaiseChannelClosed;
end;

function TChannel.TryReceive(out aItem: T; aTimeout: Cardinal): Boolean;
var
  StartTick, Elapsed, Rem: QWord;
  HasStartTick: Boolean;
begin
  HasStartTick := False;
  StartTick := 0;
  while True do
  begin
    FLock.Enter;
    if FCount > 0 then
    begin
      aItem := FBuffer[FHead];
      FBuffer[FHead] := Default(T);
      FHead := (FHead + 1) mod Length(FBuffer);
      Dec(FCount);
      NotifyItemReceived;
      FLock.Exit;
      Exit(True);
    end;
    if FClosed then
    begin
      NotifyItemReceived;
      FLock.Exit;
      aItem := Default(T);
      Exit(False);
    end;
    if aTimeout = 0 then
    begin
      FLock.Exit;
      aItem := Default(T);
      Exit(False);
    end;
    Inc(FWaitingReceivers);
    FNotEmptyEvent.ResetEvent;
    FLock.Exit;

    if aTimeout = INFINITE then
      Rem := INFINITE
    else
    begin
      if not HasStartTick then
      begin
        StartTick := GetTickCount64;
        HasStartTick := True;
      end;
      Elapsed := GetTickCount64 - StartTick;
      if Elapsed >= aTimeout then
      begin
        FLock.Enter;
        Dec(FWaitingReceivers);
        FLock.Exit;
        aItem := Default(T);
        Exit(False);
      end;
      Rem := aTimeout - Elapsed;
    end;

    FNotEmptyEvent.WaitFor(Rem);

    FLock.Enter;
    Dec(FWaitingReceivers);
    if (FWaitingReceivers > 0) and ((FCount > 0) or FClosed) then
      FNotEmptyEvent.SetEvent;
    FLock.Exit;
  end;
end;

{$IFNDEF VER3_2}
function TChannel.TryReceive(out aItem: T; const aTimeout: TTimeSpan): Boolean;
begin
  Result := TryReceive(aItem, Cardinal(Round(aTimeout.TotalMilliseconds)));
end;
{$ENDIF}

function TChannel.Receive(out aItem: T): Boolean;
begin
  Result := TryReceive(aItem, INFINITE);
end;

function TChannel.Receive: T;
begin
  if not Receive(Result) then
    RaiseChannelClosed;
end;

function TChannel.GetEnumerator: TChannelEnumerator;
begin
  Result.FChannel := Self;
  Result.FCurrent := Default(T);
end;

end.
