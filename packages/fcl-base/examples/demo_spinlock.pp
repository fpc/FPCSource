program demo_spinlock;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, syncobjs, dateutils;

const
  THREAD_COUNT = 10;
  INCREMENTS_PER_THREAD = 1000000; // We're supposed to go fast, so lots of increments :-)

{
  Results on my i9 machine, 24 cores: 
  Linux: spinlock is ~4 times faster than criticalsection
  Windows: spinlock is ~10 times faster than criticalsection
}

var
  MySpinLock: TSpinLock;
  CS: TCriticalSection;
  Counter: Integer;
  
type
  TSpinLockThread = class(TThread)
    procedure Execute; override;
  end;

  TSpinLockRelaxedThread = class(TThread)
    procedure Execute; override;
  end;

  TCriticalSectionThread = class(TThread)
    procedure Execute; override;
  end;

procedure TSpinLockThread.Execute;
var
  I: Integer;
begin
  for I:=1 to INCREMENTS_PER_THREAD do
    begin
    MySpinLock.Enter;
    try
      Inc(Counter);
    finally
      MySpinLock.Exit;
    end;
    end;
end;

procedure TSpinLockRelaxedThread.Execute;
var
  I: Integer;
begin
  for I:=1 to INCREMENTS_PER_THREAD do
    begin
    MySpinLock.Enter;
    try
      Inc(Counter);
    finally
      MySpinLock.Exit(False);
    end;
    end;
end;

procedure TCriticalSectionThread.Execute;
var
  I: Integer;
begin
  for I:=1 to INCREMENTS_PER_THREAD do
    begin
    CS.Enter;
    try
      Inc(Counter);
    finally
      CS.Leave;
    end;
    end;
end;

function RunSpinLockBenchmark: Int64;
var
  Threads: array of TSpinLockThread;
  I: Integer;
  StartTime, EndTime: TDateTime;
begin
  WriteLn('--- Testing TSpinLock Performance (PublishNow=True) ---');
  MySpinLock:=TSpinLock.Create(False);
  Counter:=0;
  SetLength(Threads, THREAD_COUNT);

  WriteLn('Starting ', THREAD_COUNT, ' threads, ', INCREMENTS_PER_THREAD, ' ops each.');
  StartTime:=Now;

  for I:=0 to THREAD_COUNT - 1 do
    begin
    Threads[I]:=TSpinLockThread.Create(True);
    Threads[I].FreeOnTerminate:=False;
    Threads[I].Start;
    end;

  for I:=0 to THREAD_COUNT - 1 do
    begin
    Threads[I].WaitFor;
    Threads[I].Free;
    end;

  EndTime:=Now;
  Result:=MilliSecondsBetween(EndTime, StartTime);
  WriteLn('TSpinLock (Strict) Time: ', Result, ' ms');
  WriteLn('Counter: ', Counter);
  WriteLn;
end;

function RunSpinLockRelaxedBenchmark: Int64;
var
  Threads: array of TSpinLockRelaxedThread;
  I: Integer;
  StartTime, EndTime: TDateTime;
begin
  WriteLn('--- Testing TSpinLock Performance (PublishNow=False) ---');
  MySpinLock:=TSpinLock.Create(False);
  Counter:=0;
  SetLength(Threads, THREAD_COUNT);

  WriteLn('Starting ', THREAD_COUNT, ' threads, ', INCREMENTS_PER_THREAD, ' ops each.');
  StartTime:=Now;

  for I:=0 to THREAD_COUNT - 1 do
    begin
    Threads[I]:=TSpinLockRelaxedThread.Create(True);
    Threads[I].FreeOnTerminate:=False;
    Threads[I].Start;
    end;

  for I:=0 to THREAD_COUNT - 1 do
    begin
    Threads[I].WaitFor;
    Threads[I].Free;
    end;

  EndTime:=Now;
  Result:=MilliSecondsBetween(EndTime, StartTime);
  WriteLn('TSpinLock (Relaxed) Time: ', Result, ' ms');
  WriteLn('Counter: ', Counter);
  WriteLn;
end;

function RunCSBenchmark: Int64;
var
  Threads: array of TCriticalSectionThread;
  I: Integer;
  StartTime, EndTime: TDateTime;
begin
  WriteLn('--- Testing TCriticalSection Performance ---');
  CS:=TCriticalSection.Create;
  Counter:=0;
  SetLength(Threads, THREAD_COUNT);

  WriteLn('Starting ', THREAD_COUNT, ' threads, ', INCREMENTS_PER_THREAD, ' ops each.');
  StartTime:=Now;

  for I:=0 to THREAD_COUNT - 1 do
    begin
    Threads[I]:=TCriticalSectionThread.Create(True);
    Threads[I].FreeOnTerminate:=False;
    Threads[I].Start;
    end;

  for I:=0 to THREAD_COUNT - 1 do
    begin
    Threads[I].WaitFor;
    Threads[I].Free;
    end;

  EndTime:=Now;
  CS.Free;
  Result:=MilliSecondsBetween(EndTime, StartTime);
  WriteLn('TCriticalSection Time: ', Result, ' ms');
  WriteLn('Counter: ', Counter);
  WriteLn;
end;

procedure TestRecursion;
begin
  WriteLn('--- Testing Recursion (Thread Tracking) ---');
  MySpinLock:=TSpinLock.Create(True);
  WriteLn('Entering first time...');
  MySpinLock.Enter;
  try
    WriteLn('Entering second time (recursive)...');
    MySpinLock.Enter;
    try
      WriteLn('Inside recursive lock.');
    finally
      MySpinLock.Exit;
    end;
    WriteLn('Exited once.');
  finally
    MySpinLock.Exit;
  end;
  WriteLn('Exited twice. Success.');
  WriteLn;
end;

procedure TestTryEnter;
begin
  WriteLn('--- Testing TryEnter ---');
  MySpinLock:=TSpinLock.Create(False);
  if MySpinLock.TryEnter then
    begin
    WriteLn('Acquired lock with TryEnter.');
    if not MySpinLock.TryEnter then
      WriteLn('Correctly failed to acquire already held lock with TryEnter.')
    else
      WriteLn('FAILURE: Acquired already held lock without tracking!');
    MySpinLock.Exit;
    end
  else
    WriteLn('FAILURE: Could not acquire free lock with TryEnter.');
  WriteLn;
end;

var
  SpinTime, SpinRelaxedTime, CSTime: Int64;
begin
  try
    TestRecursion;
    TestTryEnter;
    
    SpinTime:=RunSpinLockBenchmark;
    SpinRelaxedTime:=RunSpinLockRelaxedBenchmark;
    CSTime:=RunCSBenchmark;

    WriteLn('--- Results ---');
    WriteLn('TSpinLock (Strict):  ', SpinTime, ' ms');
    WriteLn('TSpinLock (Relaxed): ', SpinRelaxedTime, ' ms');
    WriteLn('TCriticalSection:    ', CSTime, ' ms');
    
    if SpinRelaxedTime < SpinTime then
      WriteLn('Relaxed SpinLock was faster by ', SpinTime - SpinRelaxedTime, ' ms')
    else
      WriteLn('Relaxed SpinLock was slower/equal (Difference: ', SpinTime - SpinRelaxedTime, ' ms)');

  except
    on E: Exception do
      WriteLn('Test failed with exception: ', E.ClassName, ': ', E.Message);
  end;
end.
