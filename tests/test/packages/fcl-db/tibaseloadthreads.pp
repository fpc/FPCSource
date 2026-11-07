{ %target=linux,freebsd,darwin,win32,win64 }
{ %timeout=30 }

program tibaseloadthreads;

{$mode objfpc}{$H+}

uses
  {$ifdef unix}cthreads,{$endif}
  Classes, SysUtils, SyncObjs, DynLibs, IBase60Dyn;

const
  WaitTime = 5000;
  WorkerCount = 16;
  FixtureLibrary = 'ibase-loader-fixture';
  MissingLibrary = 'ibase-loader-missing';
  FixtureHandle = TLibHandle(1);

type
  TOperation = (opAcquire, opRelease, opMixed);

  TWorker = class(TThread)
  public
    Started, Completed: TEvent;
    StartGate: TEvent;
    Operation: TOperation;
    LibraryName: AnsiString;
    Repetitions, Count: Integer;
    CheckGate, EarlyReturn, ExpectError, SawExpectedError: Boolean;
    Error: String;
    constructor Create(AOperation: TOperation; const AName: AnsiString);
    destructor Destroy; override;
    procedure Execute; override;
    procedure Acquire;
  end;

var
  OriginalManager, TestManager: TDynLibsManager;
  GateEntered, GateContinue: TEvent;
  PauseLookup, PauseUnload, FailLoads, FailUnload: LongInt;
  Loads, Unloads, Shutdowns, BadShutdown: LongInt;

procedure Check(Condition: Boolean; const Message: String);
begin
  if not Condition then
    raise Exception.Create(Message);
end;

function FixtureLoadA(const Name: RawByteString): TLibHandle;
begin
  if (Name = MissingLibrary) or (FailLoads <> 0) then
    Exit(NilHandle);

  InterlockedIncrement(Loads);
  Result := FixtureHandle;
end;

function FixtureLoadU(const Name: UnicodeString): TLibHandle;
begin
  Result := FixtureLoadA(UTF8Encode(Name));
end;

procedure DummyProcedure;
begin
end;

function FixtureShutdown(Timeout: UINT; Reason: Integer): Int;
  {$ifdef windows}stdcall{$else}cdecl{$endif};
begin
  InterlockedIncrement(Shutdowns);

  if (Timeout <> 300) or (Reason <> 1) then
    InterlockedIncrement(BadShutdown);

  Result := 0;
end;

procedure PauseAtGate;
begin
  GateEntered.SetEvent;
  Check(GateContinue.WaitFor(WaitTime) = wrSignaled, 'fixture gate timed out');
end;

function FixtureLookup(Lib: TLibHandle; const Name: AnsiString): Pointer;
begin
  Check(Lib = FixtureHandle, 'symbol lookup with invalid handle');

  if (Name = 'isc_attach_database') and
     (InterlockedCompareExchange(PauseLookup, 0, 1) = 1) then
    PauseAtGate;

  if Name = 'fb_shutdown' then
    Result := @FixtureShutdown
  else
    { These addresses are checked for publication, never called. }
    Result := @DummyProcedure;
end;

function FixtureUnload(Lib: TLibHandle): Boolean;
begin
  Check(Lib = FixtureHandle, 'unload with invalid handle');

  if InterlockedCompareExchange(PauseUnload, 0, 1) = 1 then
    PauseAtGate;

  if InterlockedCompareExchange(FailUnload, 0, 1) = 1 then
    Exit(False);

  InterlockedIncrement(Unloads);
  Result := True;
end;

constructor TWorker.Create(AOperation: TOperation; const AName: AnsiString);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  Operation := AOperation;
  LibraryName := AName;
  Repetitions := 1;
  Started := TEvent.Create(nil, True, False, '');
  Completed := TEvent.Create(nil, True, False, '');
end;

destructor TWorker.Destroy;
begin
  Started.Free;
  Completed.Free;
  inherited Destroy;
end;

procedure TWorker.Acquire;
begin
  if LibraryName = '' then
    Count := InitialiseIBase60
  else
    Count := InitialiseIBase60(LibraryName);

  if CheckGate and (GateContinue.WaitFor(0) <> wrSignaled) then
    EarlyReturn := True;

  Check(Assigned(isc_attach_database) and Assigned(isc_database_info),
    'acquisition returned before API publication');
end;

procedure TWorker.Execute;
var
  I: Integer;
begin
  try
    try
      if Assigned(StartGate) then
        Check(StartGate.WaitFor(WaitTime) = wrSignaled, 'worker start timed out');

      Started.SetEvent;

      for I := 1 to Repetitions do
        case Operation of
          opAcquire: Acquire;
          opRelease: ReleaseIBase60;
          opMixed:
            begin
              Acquire;
              ReleaseIBase60;
            end;
        end;

      if ExpectError then
        Error := 'expected EInOutError was not raised';
    except
      on E: EInOutError do
        if ExpectError then
          SawExpectedError := True
        else
          Error := E.ClassName + ': ' + E.Message;
      on E: Exception do
        Error := E.ClassName + ': ' + E.Message;
    end;
  finally
    Completed.SetEvent;
  end;
end;

procedure Join(Worker: TWorker);
begin
  { Halt on timeout: destroying a live thread could wait forever. }
  if Worker.Completed.WaitFor(WaitTime) <> wrSignaled then
  begin
    WriteLn(StdErr, 'FAILED: worker did not finish');
    Halt(2);
  end;

  Worker.WaitFor;
end;

procedure CheckWorker(Worker: TWorker);
begin
  Check(Worker.Error = '', Worker.Error);
end;

procedure ResetFixture;
begin
  Check(IBaseLibraryHandle = NilHandle, 'previous scenario retained a handle');
  GateEntered.ResetEvent;
  GateContinue.ResetEvent;
  Loads := 0;
  Unloads := 0;
  Shutdowns := 0;
  BadShutdown := 0;
  FailLoads := 0;
  FailUnload := 0;
  PauseLookup := 0;
  PauseUnload := 0;
end;

procedure CheckBlockedPair(FirstNamed, Releasing: Boolean);
var
  First, Second: TWorker;
  Name: AnsiString;
  Premature: Boolean;
begin
  ResetFixture;

  if Releasing then
  begin
    InitialiseIBase60(FixtureLibrary);
    PauseUnload := 1;
    First := TWorker.Create(opRelease, '');
  end else
  begin
    PauseLookup := 1;

    if FirstNamed then
      Name := FixtureLibrary
    else
      Name := '';

    First := TWorker.Create(opAcquire, Name);
  end;

  First.Start;
  try
    Check(GateEntered.WaitFor(WaitTime) = wrSignaled, 'fixture was not entered');

    if FirstNamed or Releasing then
      Name := ''
    else
      Name := IBaseLoadedLibrary;

    Second := TWorker.Create(opAcquire, Name);
    try
      Second.CheckGate := True;
      Second.Start;
      Check(Second.Started.WaitFor(WaitTime) = wrSignaled, 'second worker did not start');
      Premature := Second.Completed.WaitFor(200) = wrSignaled;
      GateContinue.SetEvent;
      Join(First);
      Join(Second);
      CheckWorker(First);
      CheckWorker(Second);
      Check(not Premature and not Second.EarlyReturn,
        'acquisition crossed an unfinished loader transition');

      if Releasing then
      begin
        Check((Second.Count = 1) and (Loads = 2) and (Unloads = 1),
          'acquisition did not reload after final unload');
        ReleaseIBase60;
      end else
      begin
        Check((Second.Count = 2) and (Loads = 1), 'publication lost a reference');
        ReleaseIBase60;
        Check(IBaseLibraryHandle <> NilHandle, 'live reference lost its handle');
        ReleaseIBase60;
      end;

      Check(IBaseLibraryHandle = NilHandle, 'last release retained a handle');
      Check(Shutdowns = 0, 'remote client was shut down');
    finally
      GateContinue.SetEvent;
      Join(Second);
      Second.Free;
    end;
  finally
    GateContinue.SetEvent;
    Join(First);
    First.Free;
  end;
end;

procedure CheckConcurrent(Operation: TOperation);
const
  Repeats = 1000;
var
  Workers: array[0..WorkerCount - 1] of TWorker;
  Start: TEvent;
  I: Integer;
  Name: AnsiString;
begin
  ResetFixture;
  InitialiseIBase60(FixtureLibrary);

  if Operation = opRelease then
    for I := 1 to WorkerCount * Repeats - 1 do
      InitialiseIBase60;

  Start := TEvent.Create(nil, True, False, '');
  try
    for I := 0 to High(Workers) do
    begin
      if Odd(I) then
        Name := FixtureLibrary
      else
        Name := '';

      Workers[I] := TWorker.Create(Operation, Name);
      Workers[I].Repetitions := Repeats;
      Workers[I].StartGate := Start;
      Workers[I].Start;
    end;

    Start.SetEvent;

    for I := 0 to High(Workers) do
      Join(Workers[I]);

    for I := 0 to High(Workers) do
      CheckWorker(Workers[I]);

    if Operation = opAcquire then
    begin
      Check(InitialiseIBase60 = WorkerCount * Repeats + 2, 'concurrent acquisitions lost references');

      for I := 1 to WorkerCount * Repeats + 2 do
        ReleaseIBase60;
    end;

    if Operation = opMixed then
    begin
      Check(InitialiseIBase60 = 2, 'mixed operations changed the anchor reference');
      ReleaseIBase60;
      Check(IBaseLibraryHandle <> NilHandle, 'mixed operations lost the anchor handle');
      ReleaseIBase60;
    end;

    Check((IBaseLibraryHandle = NilHandle) and (Loads = 1) and (Unloads = 1),
      'concurrent lifecycle has unbalanced load/unload');
  finally
    for I := 0 to High(Workers) do
      Workers[I].Free;

    Start.Free;
  end;
end;

procedure CheckExceptionRecovery(Conflict, DefaultFailure: Boolean);
var
  Failing, Successor: TWorker;
begin
  ResetFixture;

  if Conflict then
    InitialiseIBase60(FixtureLibrary);

  if DefaultFailure then
  begin
    FailLoads := 1;
    Failing := TWorker.Create(opAcquire, '');
  end else
    Failing := TWorker.Create(opAcquire, MissingLibrary);

  try
    Failing.ExpectError := True;
    Failing.Start;
    Join(Failing);
    FailLoads := 0;
    CheckWorker(Failing);
    Check(Failing.SawExpectedError, 'missing expected exception');

    Successor := TWorker.Create(opAcquire, '');
    try
      Successor.Start;
      Join(Successor);
      CheckWorker(Successor);

      if Conflict then
      begin
        Check(Successor.Count = 2, 'conflicting name changed the reference count');
        ReleaseIBase60;
        Check(IBaseLibraryHandle <> NilHandle, 'conflict lost the anchor');
      end else
        Check(Successor.Count = 1, 'failed load retained a reference');

      ReleaseIBase60;
      Check(IBaseLibraryHandle = NilHandle, 'exception recovery retained a handle');
    finally
      Successor.Free;
    end;
  finally
    Failing.Free;
  end;
end;

procedure CheckShutdownAndUnloadFailure;
begin
  ResetFixture;
  InitialiseIBase60(FixtureLibrary);
  FailUnload := 1;
  ReleaseIBase60;
  Check(IBaseLibraryHandle = FixtureHandle, 'failed unload lost the handle');
  Check(InitialiseIBase60 = 2, 'failed unload changed the reference count');
  ReleaseIBase60;
  ReleaseIBase60;
  Check((IBaseLibraryHandle = NilHandle) and (Unloads = 1) and (Shutdowns = 0),
    'remote unload behaviour changed');

  ResetFixture;
  UseEmbeddedFirebird := True;
  try
    InitialiseIBase60;
    ReleaseIBase60;
    Check((IBaseLibraryHandle = NilHandle) and (Shutdowns = 1) and (BadShutdown = 0),
      'embedded shutdown behaviour changed');
  finally
    UseEmbeddedFirebird := False;
  end;
end;

procedure Run;
begin
  GetDynLibsManager(OriginalManager);
  TestManager := OriginalManager;
  TestManager.LoadLibraryA := @FixtureLoadA;
  TestManager.LoadLibraryU := @FixtureLoadU;
  TestManager.GetProcAddress := @FixtureLookup;
  TestManager.UnloadLibrary := @FixtureUnload;
  GateEntered := TEvent.Create(nil, True, False, '');
  GateContinue := TEvent.Create(nil, True, False, '');
  SetDynLibsManager(TestManager);
  try
    if ParamCount = 0 then
    begin
      CheckBlockedPair(False, False);
      CheckBlockedPair(True, False);
      CheckBlockedPair(False, True);
      CheckConcurrent(opAcquire);
      CheckConcurrent(opRelease);
      CheckConcurrent(opMixed);
      CheckExceptionRecovery(False, False);
      CheckExceptionRecovery(False, True);
      CheckExceptionRecovery(True, False);
      CheckShutdownAndUnloadFailure;
    end else
      case ParamStr(1) of
        'publish-default': CheckBlockedPair(False, False);
        'publish-named': CheckBlockedPair(True, False);
        'unload': CheckBlockedPair(False, True);
        else raise Exception.Create('unknown scenario');
      end;
  finally
    SetDynLibsManager(OriginalManager);
    GateContinue.Free;
    GateEntered.Free;
  end;
end;

begin
  try
    Run;
  except
    on E: Exception do
    begin
      WriteLn(StdErr, 'FAILED: ', E.Message);
      Halt(1);
    end;
  end;
end.
