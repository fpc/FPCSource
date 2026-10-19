{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by the Free Pascal development team

    High-Performance User-Space Fibers and Coroutines (Phase 4, Step 1)
    Non-blocking fiber suspension, resumption, and async integration

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.
}
{$mode objfpc}{$H+}
{$modeswitch advancedrecords}
{$modeswitch functionreferences}
{$modeswitch anonymousfunctions}

unit async.fiber;

interface

uses
  {$IFDEF UNIX}cthreads, baseunix, unixtype,{$ENDIF}
  SysUtils, Classes, SyncObjs,
  System.Threading,
  eventloop.types, eventloop.core, eventloop.sockets,
  async.types;

type
  TFiber = class
  private
    FStackBase: Pointer;
    FStackSize: Cardinal;
    FSavedRsp: Pointer;
    FCallerRsp: Pointer;
    FState: TFiberState;
    FProc: TFiberProc;
    FMethod: TFiberMethod;
    FException: Exception;
    FEventLoop: IEventLoop;
    FUserData: Pointer;
    FSavedExceptAddr: Pointer;
    FCallerExceptAddr: Pointer;
    FSavedExceptObj: Pointer;
    FCallerExceptObj: Pointer;

    procedure InitStack;
    procedure Run;
    procedure OnSleepTimer(ATimerId: Int64);
    procedure OnContinuation(ATask: ITask);
  public
    constructor Create(AProc: TFiberProc; AStackSize: Cardinal = 128 * 1024; ALoop: IEventLoop = nil); overload;
    constructor Create(AMethod: TFiberMethod; AStackSize: Cardinal = 128 * 1024; ALoop: IEventLoop = nil); overload;
    destructor Destroy; override;

    { Execute / resume fiber execution }
    procedure Resume;

    { Yield current fiber execution back to caller / scheduler }
    class procedure Yield; static;

    { Current executing fiber in the current thread }
    class function Current: TFiber; static;

    { Non-blocking await of IFuture<T> without OS thread blocking }
    generic class function Await<T>(const AFuture: specialize IFuture<T>): T; overload; static;
    class procedure Await(const ATask: ITask); overload; static;

    { Non-blocking sleep: yields fiber to event loop until timer expiry }
    class procedure Sleep(AMilliseconds: Cardinal; ALoop: IEventLoop = nil); static;

    { Non-blocking socket read/write inside fibers }
    class function AwaitRead(ASocket: IAsyncSocket; ABuffer: Pointer; ACount: NativeInt): NativeInt; static;
    class function AwaitWrite(ASocket: IAsyncSocket; ABuffer: Pointer; ACount: NativeInt): NativeInt; static;

    property State: TFiberState read FState;
    property EventLoop: IEventLoop read FEventLoop write FEventLoop;
    property FatalException: Exception read FException;
    property UserData: Pointer read FUserData write FUserData;
  end;

implementation

threadvar
  GCurrentFiber: TFiber;

type
  TThreadVarRelocateFunc = function(offset: DWord): Pointer;

var
  FPC_THREADVAR_RELOCATE: TThreadVarRelocateFunc; external name 'FPC_THREADVAR_RELOCATE';
  U_EXCEPTADDRSTACK: array[0..15] of Byte; external name 'U_$SYSTEM_$$_EXCEPTADDRSTACK';
  U_EXCEPTOBJECTSTACK: array[0..15] of Byte; external name 'U_$SYSTEM_$$_EXCEPTOBJECTSTACK';

function GetExceptAddrStackPtr: PPointer;
begin
  if Assigned(FPC_THREADVAR_RELOCATE) then
    Result := PPointer(FPC_THREADVAR_RELOCATE(PDWord(@U_EXCEPTADDRSTACK)^))
  else
    Result := PPointer(@U_EXCEPTADDRSTACK[8]);
end;

function GetExceptObjectStackPtr: PPointer;
begin
  if Assigned(FPC_THREADVAR_RELOCATE) then
    Result := PPointer(FPC_THREADVAR_RELOCATE(PDWord(@U_EXCEPTOBJECTSTACK)^))
  else
    Result := PPointer(@U_EXCEPTOBJECTSTACK[8]);
end;

{$IFDEF CPUX86_64}
{
  Fast Assembly Context Switch for System V AMD64 ABI:
  RDI: var CurrentRsp: Pointer
  RSI: TargetRsp: Pointer
}
procedure FiberSwitchContext(var CurrentRsp: Pointer; TargetRsp: Pointer); assembler; nostackframe;
asm
  pushq %rbx
  pushq %rbp
  pushq %r12
  pushq %r13
  pushq %r14
  pushq %r15
  movq  %rsp, (%rdi)
  movq  %rsi, %rsp
  popq  %r15
  popq  %r14
  popq  %r13
  popq  %r12
  popq  %rbp
  popq  %rbx
  ret
end;
{$ELSE}
{$FATAL TFiber context switch currently requires x86_64 CPU architecture}
{$ENDIF}

procedure FiberEntryTrampoline;
var
  LFiber: TFiber;
begin
  LFiber := GCurrentFiber;
  if LFiber <> nil then
    LFiber.Run;
end;

{ TFiber }

procedure TFiber.InitStack;
var
  LSp: PPointer;
begin
  {$IFDEF UNIX}
  { Allocate private anonymous memory for fiber stack }
  FStackBase := Fpmmap(nil, FStackSize, PROT_READ or PROT_WRITE, MAP_PRIVATE or MAP_ANONYMOUS, -1, 0);
  if FStackBase = MAP_FAILED then
    raise EFiberException.CreateFmt('Failed to allocate fiber stack (%d bytes)', [FStackSize]);
  {$ELSE}
  GetMem(FStackBase, FStackSize);
  {$ENDIF}

  { Setup stack growing downwards, 16-byte aligned }
  LSp := PPointer(PByte(FStackBase) + FStackSize);

  { 16-byte ABI alignment word: ret pops trampoline, leaving rsp at 8 mod 16 }
  Dec(LSp);
  LSp^ := nil;

  { Push trampoline return address }
  Dec(LSp);
  LSp^ := @FiberEntryTrampoline;

  { Push 6 callee-saved registers (r15, r14, r13, r12, rbp, rbx) }
  Dec(LSp); LSp^ := nil; { r15 }
  Dec(LSp); LSp^ := nil; { r14 }
  Dec(LSp); LSp^ := nil; { r13 }
  Dec(LSp); LSp^ := nil; { r12 }
  Dec(LSp); LSp^ := nil; { rbp }
  Dec(LSp); LSp^ := nil; { rbx }

  FSavedRsp := LSp;
end;

constructor TFiber.Create(AProc: TFiberProc; AStackSize: Cardinal; ALoop: IEventLoop);
begin
  inherited Create;
  FProc := AProc;
  FMethod := nil;
  FStackSize := (AStackSize + 4095) and not 4095; { Round to 4KB page }
  if FStackSize < 16384 then
    FStackSize := 16384;
  FState := fsCreated;
  FEventLoop := ALoop;
  FException := nil;
  FUserData := nil;
  InitStack;
end;

constructor TFiber.Create(AMethod: TFiberMethod; AStackSize: Cardinal; ALoop: IEventLoop);
begin
  inherited Create;
  FProc := nil;
  FMethod := AMethod;
  FStackSize := (AStackSize + 4095) and not 4095;
  if FStackSize < 16384 then
    FStackSize := 16384;
  FState := fsCreated;
  FEventLoop := ALoop;
  FException := nil;
  FUserData := nil;
  InitStack;
end;

destructor TFiber.Destroy;
begin
  if FState = fsRunning then
    raise EInvalidFiberOperation.Create('Cannot destroy an actively running fiber');

  if FStackBase <> nil then
  begin
    {$IFDEF UNIX}
    Fpmunmap(FStackBase, FStackSize);
    {$ELSE}
    FreeMem(FStackBase);
    {$ENDIF}
    FStackBase := nil;
  end;

  FException.Free;
  inherited Destroy;
end;

procedure TFiber.Run;
begin
  try
    if Assigned(FProc) then
      FProc
    else if Assigned(FMethod) then
      FMethod;
    FState := fsCompleted;
  except
    on E: Exception do
    begin
      FState := fsFaulted;
      FException := Exception(AcquireExceptionObject);
    end;
  end;

  { Execution finished; switch back to caller context }
  Yield;
end;

procedure TFiber.Resume;
var
  LPrevFiber: TFiber;
  LAddrPtr, LObjPtr: PPointer;
begin
  if FState in [fsCompleted, fsFaulted] then
    raise EInvalidFiberOperation.Create('Cannot resume an already completed or faulted fiber');

  LAddrPtr := GetExceptAddrStackPtr;
  LObjPtr := GetExceptObjectStackPtr;

  LPrevFiber := GCurrentFiber;
  GCurrentFiber := Self;
  FState := fsRunning;

  { Save caller exception stack and install fiber's exception stack }
  FCallerExceptAddr := LAddrPtr^;
  FCallerExceptObj := LObjPtr^;
  LAddrPtr^ := FSavedExceptAddr;
  LObjPtr^ := FSavedExceptObj;

  FiberSwitchContext(FCallerRsp, FSavedRsp);

  { Context has returned to caller; Yield already saved fiber and restored caller context }
  GCurrentFiber := LPrevFiber;

  if (FState = fsFaulted) and (FException <> nil) then
  begin
    raise Exception.Create(FException.Message);
  end;
end;

class procedure TFiber.Yield;
var
  LCur: TFiber;
  LAddrPtr, LObjPtr: PPointer;
begin
  LCur := GCurrentFiber;
  if LCur = nil then
    raise EInvalidFiberOperation.Create('Cannot yield: no fiber is currently executing');

  if LCur.FState = fsRunning then
    LCur.FState := fsSuspended;

  LAddrPtr := GetExceptAddrStackPtr;
  LObjPtr := GetExceptObjectStackPtr;

  { Save fiber exception stack and restore caller's exception stack }
  LCur.FSavedExceptAddr := LAddrPtr^;
  LCur.FSavedExceptObj := LObjPtr^;
  LAddrPtr^ := LCur.FCallerExceptAddr;
  LObjPtr^ := LCur.FCallerExceptObj;

  FiberSwitchContext(LCur.FSavedRsp, LCur.FCallerRsp);

  { When we return here, Resume has already saved caller context and restored fiber context }
end;

class function TFiber.Current: TFiber;
begin
  Result := GCurrentFiber;
end;

procedure TFiber.OnSleepTimer(ATimerId: Int64);
begin
  Resume;
end;

procedure TFiber.OnContinuation(ATask: ITask);
begin
  if FEventLoop <> nil then
    FEventLoop.Post(@Resume)
  else
    TTask.Run(@Resume);
end;

generic class function TFiber.Await<T>(const AFuture: specialize IFuture<T>): T;
var
  LFiber: TFiber;
begin
  if AFuture = nil then
    raise EArgumentNilException.Create('AFuture cannot be nil in TFiber.Await');

  if AFuture.Status = TTaskStatus.Completed then
    Exit(AFuture.Value);

  if AFuture.Status in [TTaskStatus.Canceled, TTaskStatus.Exception] then
    Exit(AFuture.Value); { Raises exception }

  LFiber := Current;
  if LFiber = nil then
    raise EFiberException.Create('TFiber.Await must be called from inside an active fiber');

  LFiber.FState := fsSuspended;
  AFuture.ContinueWith(@LFiber.OnContinuation);

  Yield;

  Result := AFuture.Value;
end;

class procedure TFiber.Await(const ATask: ITask);
var
  LFiber: TFiber;
begin
  if ATask = nil then
    raise EArgumentNilException.Create('ATask cannot be nil in TFiber.Await');

  if ATask.Status = TTaskStatus.Completed then
    Exit;

  if ATask.Status in [TTaskStatus.Canceled, TTaskStatus.Exception] then
  begin
    ATask.Wait;
    Exit;
  end;

  LFiber := Current;
  if LFiber = nil then
    raise EFiberException.Create('TFiber.Await must be called from inside an active fiber');

  LFiber.FState := fsSuspended;
  ATask.ContinueWith(@LFiber.OnContinuation);

  Yield;

  if ATask.Status in [TTaskStatus.Canceled, TTaskStatus.Exception] then
    ATask.Wait;
end;

class procedure TFiber.Sleep(AMilliseconds: Cardinal; ALoop: IEventLoop);
var
  LFiber: TFiber;
  LLoop: IEventLoop;
begin
  LFiber := Current;
  if LFiber = nil then
  begin
    SysUtils.Sleep(AMilliseconds);
    Exit;
  end;

  LLoop := ALoop;
  if LLoop = nil then
    LLoop := LFiber.FEventLoop;

  LFiber.FState := fsSuspended;
  if LLoop <> nil then
  begin
    LLoop.SetTimer(AMilliseconds, @LFiber.OnSleepTimer, False);
    Yield;
  end
  else
  begin
    TTask.Run(
      procedure
      begin
        SysUtils.Sleep(AMilliseconds);
        LFiber.Resume;
      end);
    Yield;
  end;
end;

class function TFiber.AwaitRead(ASocket: IAsyncSocket; ABuffer: Pointer; ACount: NativeInt): NativeInt;
var
  LFiber: TFiber;
  LBytesRead: NativeInt;
  LStatus: Integer;
begin
  LFiber := Current;
  if LFiber = nil then
    raise EFiberException.Create('TFiber.AwaitRead must be called from inside a fiber');

  LFiber.FState := fsSuspended;
  LBytesRead := 0;
  LStatus := 0;

  ASocket.AsyncRead(ABuffer, ACount,
    procedure(ABytes: NativeInt; AStat: Integer)
    begin
      LBytesRead := ABytes;
      LStatus := AStat;
      if LFiber.FEventLoop <> nil then
        LFiber.FEventLoop.Post(@LFiber.Resume)
      else
        TTask.Run(@LFiber.Resume);
    end);

  Yield;

  if LStatus <> 0 then
    raise ESocketException.CreateFmt('Async read failed with error %d', [LStatus]);
  Result := LBytesRead;
end;

class function TFiber.AwaitWrite(ASocket: IAsyncSocket; ABuffer: Pointer; ACount: NativeInt): NativeInt;
var
  LFiber: TFiber;
  LBytesWritten: NativeInt;
  LStatus: Integer;
begin
  LFiber := Current;
  if LFiber = nil then
    raise EFiberException.Create('TFiber.AwaitWrite must be called from inside a fiber');

  LFiber.FState := fsSuspended;
  LBytesWritten := 0;
  LStatus := 0;

  ASocket.AsyncWrite(ABuffer, ACount,
    procedure(ABytes: NativeInt; AStat: Integer)
    begin
      LBytesWritten := ABytes;
      LStatus := AStat;
      if LFiber.FEventLoop <> nil then
        LFiber.FEventLoop.Post(@LFiber.Resume)
      else
        TTask.Run(@LFiber.Resume);
    end);

  Yield;

  if LStatus <> 0 then
    raise ESocketException.CreateFmt('Async write failed with error %d', [LStatus]);
  Result := LBytesWritten;
end;

end.
