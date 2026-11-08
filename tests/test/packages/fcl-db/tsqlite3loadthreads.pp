{ %target=linux,freebsd,darwin,win32,win64 }
{ %timeout=30 }
{
    Concurrent loading and unloading of the SQLite client library by SQLite3Dyn,
    with a fake library installed through the RTL dynamic-library manager.
}
program tsqlite3loadthreads;

{$mode objfpc}{$H+}

uses
  {$ifdef unix}cthreads,{$endif}
  Classes, SysUtils, DynLibs, SQLite3Dyn;

const
  WaitTime = 5000;
  PauseTime = 300;
  FakeLibrary = 'sqlite3-loader-fake';
  MissingLibrary = 'sqlite3-loader-missing';
  FakeHandle = TLibHandle(1);
  WorkerCount = 8;
  Rounds = 2000;

type
  TLoaderOp = (opAcquire, opRelease, opCycle);

  { TLoaderThread }

  TLoaderThread = class(TThread)
  private
    FOp: TLoaderOp;
  public
    Done: Boolean;
    Error: String;
    SawPointers: Boolean;
    constructor Create(aOp: TLoaderOp);
    procedure Execute; override;
  end;

var
  OriginalManager, FakeManager: TDynLibsManager;
  GateEntered, GateRelease: PRTLEvent;
  PauseLookup, PauseUnload: LongInt;
  Loads, Unloads: LongInt;
  Failures: Integer = 0;


// Report a failed check
procedure Check(aCondition: Boolean; const aMsg: String);

begin
  if aCondition then
    Writeln('  PASS  ',aMsg)
  else
    begin
    Writeln('  FAIL  ',aMsg);
    Inc(Failures);
    end;
end;


// Fake LoadLibrary: a fixed handle, or NilHandle for MissingLibrary
function FakeLoadA(const aName: RawByteString): TLibHandle;

begin
  if aName=MissingLibrary then
    Exit(NilHandle);
  InterlockedIncrement(Loads);
  Result:=FakeHandle;
end;


// Fake LoadLibrary for unicode names
function FakeLoadU(const aName: UnicodeString): TLibHandle;

begin
  Result:=FakeLoadA(UTF8Encode(aName));
end;


// Target of every resolved symbol; never called
procedure DummyProc;

begin
end;


// Signal that the fake library reached its gate and wait until the test opens it
procedure PauseAtGate;

begin
  RTLEventSetEvent(GateEntered);
  RTLEventWaitFor(GateRelease,WaitTime);
end;


// Fake GetProcAddress, pausing at the first symbol when PauseLookup is set
function FakeLookup(aLib: TLibHandle; const aName: AnsiString): Pointer;

begin
  if (aName='sqlite3_libversion') and (InterlockedCompareExchange(PauseLookup,0,1)=1) then
    PauseAtGate;
  Result:=@DummyProc;
end;


// Fake UnloadLibrary, pausing when PauseUnload is set
function FakeUnload(aLib: TLibHandle): Boolean;

begin
  if InterlockedCompareExchange(PauseUnload,0,1)=1 then
    PauseAtGate;
  InterlockedIncrement(Unloads);
  Result:=True;
end;


constructor TLoaderThread.Create(aOp: TLoaderOp);

begin
  FOp:=aOp;
  inherited Create(False);
end;


procedure TLoaderThread.Execute;

var
  I: Integer;

begin
  try
    case FOp of
      opAcquire:
        begin
        InitializeSqlite(FakeLibrary);
        SawPointers:=Assigned(sqlite3_libversion) and Assigned(sqlite3_uri_parameter);
        end;
      opRelease:
        ReleaseSqlite;
      opCycle:
        for I:=1 to Rounds do
          begin
          InitializeSqlite(FakeLibrary);
          if not (Assigned(sqlite3_libversion) and Assigned(sqlite3_uri_parameter)) then
            raise Exception.Create('API not published after InitializeSqlite');
          ReleaseSqlite;
          end;
    end;
  except
    on E: Exception do
      Error:=E.ClassName+': '+E.Message;
  end;
  Done:=True;
end;


// Wait until aThread has finished
procedure Join(aThread: TLoaderThread);

begin
  aThread.WaitFor;
end;


// A second InitializeSqlite waits until the first has resolved all symbols
procedure TestPublication;

var
  lFirst, lSecond: TLoaderThread;

begin
  Writeln('Acquire while the first acquire resolves symbols');
  PauseLookup:=1;
  lFirst:=TLoaderThread.Create(opAcquire);
  RTLEventWaitFor(GateEntered,WaitTime);
  lSecond:=TLoaderThread.Create(opAcquire);
  Sleep(PauseTime);
  Check(not lSecond.Done,'Second acquire does not return before the library is loaded');
  RTLEventSetEvent(GateRelease);
  Join(lFirst);
  Join(lSecond);
  Check((lFirst.Error='') and (lSecond.Error=''),'Both acquires succeed '+lFirst.Error+lSecond.Error);
  Check(lSecond.SawPointers,'Second acquire sees the resolved API');
  Check(Loads=1,Format('Library loaded once (%d)',[Loads]));
  lFirst.Free;
  lSecond.Free;
  ReleaseSqlite;
  ReleaseSqlite;
  Check((SQLiteLibraryHandle=NilHandle) and (Unloads=1),Format('Unloaded after two releases (%d)',[Unloads]));
end;


// An acquire during the final release waits for the unload and then loads again
procedure TestAcquireDuringUnload;

var
  lRelease, lAcquire: TLoaderThread;

begin
  Writeln('Acquire while the last release unloads');
  Loads:=0;
  Unloads:=0;
  InitializeSqlite(FakeLibrary);
  PauseUnload:=1;
  lRelease:=TLoaderThread.Create(opRelease);
  RTLEventWaitFor(GateEntered,WaitTime);
  lAcquire:=TLoaderThread.Create(opAcquire);
  Sleep(PauseTime);
  Check(not lAcquire.Done,'Acquire does not return while the library is being unloaded');
  RTLEventSetEvent(GateRelease);
  Join(lRelease);
  Join(lAcquire);
  Check((lRelease.Error='') and (lAcquire.Error=''),'Release and acquire succeed '+lRelease.Error+lAcquire.Error);
  Check(SQLiteLibraryHandle<>NilHandle,'Library handle valid after the acquire');
  Check(SQLiteLoadedLibrary=FakeLibrary,'Loaded library name kept after the acquire');
  Check((Loads=2) and (Unloads=1),Format('Loaded twice and unloaded once (%d, %d)',[Loads,Unloads]));
  lRelease.Free;
  lAcquire.Free;
  ReleaseSqlite;
  Check(SQLiteLibraryHandle=NilHandle,'Unloaded after the last release');
end;


// Many threads loading and releasing keep the count balanced
procedure TestConcurrentCycles;

var
  lThreads: array[0..WorkerCount-1] of TLoaderThread;
  I: Integer;
  lErrors: String;

begin
  Writeln('Concurrent acquire and release cycles');
  Loads:=0;
  Unloads:=0;
  for I:=0 to WorkerCount-1 do
    lThreads[I]:=TLoaderThread.Create(opCycle);
  lErrors:='';
  for I:=0 to WorkerCount-1 do
    begin
    Join(lThreads[I]);
    lErrors:=lErrors+lThreads[I].Error;
    lThreads[I].Free;
    end;
  Check(lErrors='','Every cycle sees the resolved API '+lErrors);
  Check(SQLiteLibraryHandle=NilHandle,'Library unloaded after all cycles');
  Check(Loads=Unloads,Format('Every load has an unload (%d, %d)',[Loads,Unloads]));
  Check(TryInitializeSqlite(FakeLibrary)=1,'Reference count is zero after all cycles');
  ReleaseSqlite;
end;


// A failed load leaves the loader usable
procedure TestFailedLoad;

begin
  Writeln('Failed load');
  Check(TryInitializeSqlite(MissingLibrary)=-1,'TryInitializeSqlite returns -1 for a missing library');
  Check(SQLiteLibraryHandle=NilHandle,'No handle after a failed load');
  Check(TryInitializeSqlite(FakeLibrary)=1,'Next load counts from one');
  Check(TryInitializeSqlite(FakeLibrary)=2,'Second load counts two');
  ReleaseSqlite;
  ReleaseSqlite;
  Check(SQLiteLibraryHandle=NilHandle,'Unloaded after matching releases');
end;


begin
  GetDynLibsManager(OriginalManager);
  FakeManager:=OriginalManager;
  FakeManager.LoadLibraryU:=@FakeLoadU;
  FakeManager.LoadLibraryA:=@FakeLoadA;
  FakeManager.GetProcAddress:=@FakeLookup;
  FakeManager.UnloadLibrary:=@FakeUnload;
  SetDynLibsManager(FakeManager);
  GateEntered:=RTLEventCreate;
  GateRelease:=RTLEventCreate;
  try
    TestPublication;
    TestAcquireDuringUnload;
    TestConcurrentCycles;
    TestFailedLoad;
  finally
    SetDynLibsManager(OriginalManager);
    RTLEventDestroy(GateEntered);
    RTLEventDestroy(GateRelease);
  end;
  if Failures>0 then
    begin
    Writeln(Failures,' check(s) failed');
    Halt(1);
    end;
  Writeln('All checks passed');
end.
