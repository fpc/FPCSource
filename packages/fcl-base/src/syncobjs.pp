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
{$h+}

{$IF DEFINED(WINCE) or DEFINED(AIX)}
{$DEFINE NO_SEMAPHORE_SUPPORT}
{$ENDIF}

{$IF DEFINED(WINCE)} 
{$DEFINE NO_MUTEX_SUPPORT}
{$ENDIF}


{$IFNDEF FPC_DOTTEDUNITS}
unit syncobjs;
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
  System.SysUtils;
  
{$ELSE FPC_DOTTEDUNITS}
  {$IFNDEF VER3_2}
  system.timespan,
  {$ENDIF}
  {$IFDEF UNIX}
  unixtype,
  {$ENDIF}
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
{$ifndef VER3_0}
    generic class function Exchange<T: class>(var Target: T; Value: T): T; overload; static; inline;
    generic class function CompareExchange<T: class>(var Target: T; Value: T; Comparand: T): T; overload; static; inline;
{$endif VER3_0}
{$ENDIF NOPOINTER}
  end;

{$IFNDEF NO_SEMAPHORE_SUPPORT}
  TSemaphore = class(THandleObject)
  {$IFDEF UNIX}
     Fsem: TPosixSemaphore;
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
{$ENDIF}

{$IFNDEF NO_MUTEX_SUPPORT}
  TMutex = class(THandleObject)
  private
{$IFDEF UNIX}
    FMutex: pthread_mutex_t;
{$ENDIF POSIX}
  public
    constructor Create(aUseCOMWait: Boolean = False); overload;
    constructor Create(aAttributes: PSecurityAttributes; aInitialOwner: Boolean; const aName: string; aUseCOMWait: Boolean = False); overload;
    constructor Create(aAccess: Cardinal; aInherit: Boolean; const aName: string; aUseCOMWait: Boolean = False); overload;
    destructor Destroy; override;
    function WaitFor(aTimeout: Cardinal): TWaitResult; override;
    procedure Acquire; override;
    procedure Release; override;
  end;
{$ENDIF}



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
  SErrMutexNotSupported   = 'Mutexes are not supported on this platform';
  
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
end;

{ THandleObject }

constructor THandleObject.Create(UseComWait : Boolean=false);
// cmompatibility shortcut constructor, Com waiting not implemented yet
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

{$ifndef VER3_0}
generic class function TInterlocked.CompareExchange<T>(var Target: T; Value: T; Comparand: T): T; overload; static; inline;
begin
  Result := T(InterlockedCompareExchange(Pointer(Target), Pointer(Value), Pointer(Comparand)));
end;

generic class function TInterlocked.Exchange<T>(var Target: T; Value: T): T; overload; static; inline;
begin
  Result := T(InterLockedExchange(Pointer(Target), Pointer(Value)));
end;
{$endif VER3_0}

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
{$ELSE}
  Raise ESyncObjectException.Create(SErrNoSemaphoreSupport);
{$ENDIF}    
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
  inherited Destroy;
end;



function TSemaphore.WaitFor(aTimeout: Cardinal = INFINITE): TWaitResult; 


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
  Result:=Inherited WaitFor(aTimeOut);
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
  raise ESyncObjectException.Create(SErrMutexNotSupported);
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
  raise ESyncObjectException.Create(SErrMutexNotSupported);
{$ENDIF WINDOWS}
{$ENDIF UNIX}
end;

destructor TMutex.Destroy; 

begin
{$IFDEF UNIX}
   pthread_mutex_destroy(@FMutex);
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
begin
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
end;
{$ENDIF NO_MUTEX_SUPPORT}

end.
