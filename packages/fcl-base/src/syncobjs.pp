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
unit syncobjs;

interface

uses
  sysutils,system.timespan;

type
  PSecurityAttributes = Pointer;
  TEventHandle = Pointer;

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
      function WaitFor(Timeout : Cardinal=INFINITE) : TWaitResult;overload;
      function WaitFor(const Timeout : TTimespan) : TWaitResult;overload;
      {$IFDEF MSWINDOWS}
        class function WaitForMultiple(const HandleObjs: THandleObjectArray; Timeout: Cardinal; AAll: Boolean; out SignaledObj: THandleObject; UseCOMWait: Boolean = False; Len: Integer = 0): TWaitResult;
      {$ENDIF MSWINDOWS}
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

implementation

{$ifdef MSWindows}
uses Windows;

function CoWaitForMultipleHandles(dwFlags, dwTimeout: DWORD; cHandles: ULONG; pHandles: PWOHandleArray; out lpdwindex: DWORD): HRESULT; stdcall; external 'ole32.dll' name 'CoWaitForMultipleHandles';
{$endif}


Resourcestring
  SErrEventCreateFailed   = 'Failed to create OS basic event with name "%s"';
  SErrEventZeroNotAllowed = 'Handle count of zero is not allowed.';
  SErrEventMaxObjects     = 'The maximal amount of objects is %d.';
  SErrEventTooManyHandles = 'Length of object handles smaller than Len.';

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

function THandleObject.WaitFor(const Timeout: TTimespan): TWaitResult;
begin
  result:=waitfor(round(timeout.TotalMilliseconds));
end;

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
    WOHandles[HandleIndex] := Windows.HANDLE(HandleObjs[HandleIndex].Handle);

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
{$endif}

destructor THandleObject.Destroy;
begin
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

end.
