{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by the Free Pascal development team

    Unit tests for fcl-async (Fibers, Coroutines, Generators, State Machines)

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.
}
{$mode objfpc}{$H+}
{$modeswitch advancedrecords}
{$modeswitch functionreferences}
{$modeswitch anonymousfunctions}

unit utasync;

interface

uses
  SysUtils, Classes, fpcunit, testregistry,
  System.Threading,
  eventloop.types, eventloop.core, eventloop.sockets, fcleventloop,
  async.types, async.fiber, async.generator, async.statemachine, fclasync;

type
  { Test Suite for User-Space Fibers }
  TTestFibers = class(TTestCase)
  private
    FOrderLog: string;
    FExecuted: Boolean;
    procedure SimpleFiberProc;
    procedure YieldingFiberProc;
    procedure FaultingFiberProc;
  published
    procedure TestFiberRunAndComplete;
    procedure TestFiberYieldAndResume;
    procedure TestFiberExceptionPropagation;
    procedure TestFiberAwaitFuture;
    procedure TestFiberAwaitTask;
    procedure TestFiberSleep;
  end;

  { Test Suite for Generators (Yield Iterators) }
  TTestGenerators = class(TTestCase)
  published
    procedure TestGeneratorSequence;
    procedure TestGeneratorForInLoop;
  end;

  { Test Suite for State Machine Builders & TaskCompletionSource }
  TTestStateMachines = class(TTestCase)
  published
    procedure TestTaskCompletionSourceSetResult;
    procedure TestTaskCompletionSourceSetException;
    procedure TestTaskCompletionSourceSetCanceled;
    procedure TestAsyncStateMachineExecution;
  end;

implementation

{ TTestFibers }

procedure TTestFibers.SimpleFiberProc;
begin
  FOrderLog := FOrderLog + 'B';
  FExecuted := True;
end;

procedure TTestFibers.YieldingFiberProc;
begin
  FOrderLog := FOrderLog + '1';
  TFiber.Yield;
  FOrderLog := FOrderLog + '2';
  TFiber.Yield;
  FOrderLog := FOrderLog + '3';
end;

procedure TTestFibers.FaultingFiberProc;
begin
  raise Exception.Create('Fiber boom');
end;

procedure TTestFibers.TestFiberRunAndComplete;
var
  LFiber: TFiber;
begin
  FOrderLog := 'A';
  FExecuted := False;
    LFiber := TFiber.Create(@Self.SimpleFiberProc);
    try
      AssertEquals('Initial state fsCreated', ord(fsCreated), ord(LFiber.State));
      LFiber.Resume;
      FOrderLog := FOrderLog + 'C';
      AssertTrue('Fiber executed', FExecuted);
      AssertEquals('Execution order match', 'ABC', FOrderLog);
      AssertEquals('Final state fsCompleted', ord(fsCompleted), ord(LFiber.State));
    finally
      LFiber.Free;
    end;
  end;

  procedure TTestFibers.TestFiberYieldAndResume;
  var
    LFiber: TFiber;
  begin
    FOrderLog := 'Start:';
    LFiber := TFiber.Create(@Self.YieldingFiberProc);
    try
      LFiber.Resume;
      AssertEquals('State after 1st yield', ord(fsSuspended), ord(LFiber.State));
      FOrderLog := FOrderLog + 'A:';

      LFiber.Resume;
      AssertEquals('State after 2nd yield', ord(fsSuspended), ord(LFiber.State));
      FOrderLog := FOrderLog + 'B:';

      LFiber.Resume;
      AssertEquals('State after finish', ord(fsCompleted), ord(LFiber.State));
      FOrderLog := FOrderLog + 'End';

      AssertEquals('Yield/Resume interleaving', 'Start:1A:2B:3End', FOrderLog);
    finally
      LFiber.Free;
    end;
  end;

  procedure TTestFibers.TestFiberExceptionPropagation;
  var
    LFiber: TFiber;
    LCaught: Boolean;
  begin
    LFiber := TFiber.Create(@Self.FaultingFiberProc);
  try
    LCaught := False;
    try
      LFiber.Resume;
    except
      on E: Exception do
      begin
        LCaught := True;
        AssertEquals('Exception message preserved', 'Fiber boom', E.Message);
      end;
    end;
    AssertTrue('Exception was caught', LCaught);
    AssertEquals('State fsFaulted', ord(fsFaulted), ord(LFiber.State));
  finally
    LFiber.Free;
  end;
end;

type
  TFiberFutureRunner = class
  public
    Future: specialize IFuture<Integer>;
    ResultVal: Integer;
    Executed: Boolean;
    procedure Run;
  end;

procedure TFiberFutureRunner.Run;
begin
  ResultVal := TFiber.specialize Await<Integer>(Future);
  Executed := True;
end;

procedure TTestFibers.TestFiberAwaitFuture;
var
  LRunner: TFiberFutureRunner;
  LFiber: TFiber;
  I: Integer;
begin
  LRunner := TFiberFutureRunner.Create;
  try
    LRunner.Executed := False;
    LRunner.ResultVal := 0;

    { Launch asynchronous computation on thread pool }
    LRunner.Future := TTask.specialize Future<Integer>(
      function: Integer
      begin
        Sleep(50);
        Result := 42;
      end);

    LFiber := TFiber.Create(@LRunner.Run);
    try
      LFiber.Resume;

      { Fiber should have yielded because future is not yet completed }
      if not LRunner.Executed then
        AssertEquals('Fiber suspended on await', ord(fsSuspended), ord(LFiber.State));

      { Wait for future completion and fiber resumption }
      for I := 1 to 50 do
      begin
        if LRunner.Executed then
          Break;
        Sleep(10);
      end;

      AssertTrue('Fiber resumed and completed after Future finished', LRunner.Executed);
      AssertEquals('Awaited value from future match', 42, LRunner.ResultVal);
    finally
      LFiber.Free;
    end;
  finally
    LRunner.Free;
  end;
end;

type
  TFiberTaskRunner = class
  public
    TaskObj: ITask;
    Executed: Boolean;
    procedure Run;
  end;

procedure TFiberTaskRunner.Run;
begin
  TFiber.Await(TaskObj);
  Executed := True;
end;

procedure TTestFibers.TestFiberAwaitTask;
var
  LRunner: TFiberTaskRunner;
  LFiber: TFiber;
  I: Integer;
begin
  LRunner := TFiberTaskRunner.Create;
  try
    LRunner.Executed := False;
    LRunner.TaskObj := TTask.Run(
      procedure
      begin
        Sleep(40);
      end);

    LFiber := TFiber.Create(@LRunner.Run);
    try
      LFiber.Resume;

      for I := 1 to 50 do
      begin
        if LRunner.Executed then
          Break;
        Sleep(10);
      end;

      AssertTrue('Fiber awaited ITask successfully', LRunner.Executed);
    finally
      LFiber.Free;
    end;
  finally
    LRunner.Free;
  end;
end;

type
  TFiberSleepRunner = class
  public
    Loop: IEventLoop;
    Done: Boolean;
    procedure Run;
  end;

procedure TFiberSleepRunner.Run;
begin
  TFiber.Sleep(30, Loop);
  Done := True;
end;

procedure TTestFibers.TestFiberSleep;
var
  LRunner: TFiberSleepRunner;
  LFiber: TFiber;
  I: Integer;
begin
  LRunner := TFiberSleepRunner.Create;
  try
    LRunner.Loop := TEventLoop.Create(bkAuto);
    LRunner.Done := False;

    LFiber := TFiber.Create(@LRunner.Run, 128 * 1024, LRunner.Loop);
    try
      LFiber.Resume;
      AssertFalse('Not done immediately', LRunner.Done);

      { Pump event loop until sleep timer triggers }
      for I := 1 to 20 do
      begin
        if LRunner.Done then
          Break;
        LRunner.Loop.RunOnce(10);
      end;

      AssertTrue('Fiber resumed by event loop timer', LRunner.Done);
    finally
      LFiber.Free;
    end;
  finally
    LRunner.Free;
  end;
end;

{ TTestGenerators }

procedure ProducerCountToFive(const Yield: specialize TGenerator<Integer>.TYieldProc);
var
  I: Integer;
begin
  for I := 1 to 5 do
    Yield(I * 10);
end;

procedure TTestGenerators.TestGeneratorSequence;
var
  LGen: specialize TGenerator<Integer>;
begin
  LGen := specialize TGenerator<Integer>.Create(@ProducerCountToFive);
  try
    AssertTrue('1st MoveNext', LGen.MoveNext);
    AssertEquals('1st value', 10, LGen.Current);

    AssertTrue('2nd MoveNext', LGen.MoveNext);
    AssertEquals('2nd value', 20, LGen.Current);

    AssertTrue('3rd MoveNext', LGen.MoveNext);
    AssertEquals('3rd value', 30, LGen.Current);

    AssertTrue('4th MoveNext', LGen.MoveNext);
    AssertEquals('4th value', 40, LGen.Current);

    AssertTrue('5th MoveNext', LGen.MoveNext);
    AssertEquals('5th value', 50, LGen.Current);

    AssertFalse('6th MoveNext is false (done)', LGen.MoveNext);
    AssertTrue('Generator Done', LGen.Done);
  finally
    LGen.Free;
  end;
end;

procedure TTestGenerators.TestGeneratorForInLoop;
var
  LGen: specialize TGenerator<Integer>;
  LSum: Integer;
  LCount: Integer;
  Val: Integer;
begin
  LGen := specialize TGenerator<Integer>.Create(@ProducerCountToFive);
  try
    LSum := 0;
    LCount := 0;
    for Val in LGen do
    begin
      Inc(LCount);
      Inc(LSum, Val);
    end;

    AssertEquals('Count of yielded items', 5, LCount);
    AssertEquals('Sum of yielded items (10+20+30+40+50)', 150, LSum);
  finally
    LGen.Free;
  end;
end;

{ TTestStateMachines }

procedure TTestStateMachines.TestTaskCompletionSourceSetResult;
var
  LTcs: specialize TTaskCompletionSource<string>;
  LFut: specialize IFuture<string>;
begin
  LTcs := specialize TTaskCompletionSource<string>.Create;
  try
    LFut := LTcs.Task;
    AssertFalse('Not completed initially', LFut.Status = TTaskStatus.Completed);

    { Set result from another thread }
    TTask.Run(
      procedure
      begin
        Sleep(20);
        LTcs.SetResult('HelloCompletionSource');
      end);

    AssertEquals('Future result match', 'HelloCompletionSource', LFut.Value);
    AssertEquals('Status Completed', ord(TTaskStatus.Completed), ord(LFut.Status));
  finally
    LTcs.Free;
  end;
end;

procedure TTestStateMachines.TestTaskCompletionSourceSetException;
var
  LTcs: specialize TTaskCompletionSource<Integer>;
  LFut: specialize IFuture<Integer>;
  LCaught: Boolean;
begin
  LTcs := specialize TTaskCompletionSource<Integer>.Create;
  try
    LFut := LTcs.Task;
    TTask.Run(
      procedure
      begin
        Sleep(10);
        LTcs.SetException(Exception.Create('ExplicitFailure'));
      end);

    LCaught := False;
    try
      LFut.Value;
    except
      on E: EAggregateException do
      begin
        LCaught := True;
        AssertEquals('Exception message', 'ExplicitFailure', E.InnerExceptions[0].Message);
      end;
      on E: Exception do
      begin
        LCaught := True;
        AssertEquals('Exception message', 'ExplicitFailure', E.Message);
      end;
    end;
    AssertTrue('Exception raised upon reading Value', LCaught);
  finally
    LTcs.Free;
  end;
end;

procedure TTestStateMachines.TestTaskCompletionSourceSetCanceled;
var
  LTcs: specialize TTaskCompletionSource<Integer>;
  LFut: specialize IFuture<Integer>;
  LCaught: Boolean;
begin
  LTcs := specialize TTaskCompletionSource<Integer>.Create;
  try
    LFut := LTcs.Task;
    TTask.Run(
      procedure
      begin
        Sleep(10);
        LTcs.SetCanceled;
      end);

    LCaught := False;
    try
      LFut.Value;
    except
      on E: EOperationCancelled do
        LCaught := True;
    end;
    AssertTrue('EOperationCancelled raised', LCaught);
  finally
    LTcs.Free;
  end;
end;

type
  { Simulated compiler-generated async state machine }
  TSampleAsyncStateMachine = class(TInterfacedObject, IAsyncStateMachine)
  private
    FState: Integer;
    FBuilder: specialize TAsyncTaskMethodBuilder<Integer>;
    FAwaiter: specialize IFuture<Integer>;
    FAccum: Integer;
  public
    constructor Create;
    function GetTask: specialize IFuture<Integer>;
    procedure MoveNext;
    procedure SetStateMachine(const AStateMachine: IAsyncStateMachine);
    property Task: specialize IFuture<Integer> read GetTask;
  end;

function TSampleAsyncStateMachine.GetTask: specialize IFuture<Integer>;
begin
  Result := FBuilder.Task;
end;

constructor TSampleAsyncStateMachine.Create;
begin
  inherited Create;
  FState := 0;
  FBuilder := specialize TAsyncTaskMethodBuilder<Integer>.Create;
  FAccum := 10;
end;

procedure TSampleAsyncStateMachine.SetStateMachine(const AStateMachine: IAsyncStateMachine);
begin
  { no-op for refcounted interface }
end;

procedure TSampleAsyncStateMachine.MoveNext;
var
  LRes: Integer;
begin
  try
    case FState of
      0:
        begin
          { State 0: launch async computation }
          FState := 1;
          FAwaiter := TTask.specialize Future<Integer>(
            function: Integer
            begin
              Sleep(30);
              Result := 50;
            end);

          if not (FAwaiter.Status = TTaskStatus.Completed) then
          begin
            FBuilder.AwaitOnCompleted(FAwaiter, Self);
            Exit;
          end;
          { Fall through if already completed }
        end;

      1:
        begin
          { State 1: resume after await }
          LRes := FAwaiter.Value;
          Inc(FAccum, LRes);
          FBuilder.SetResult(FAccum);
          FState := -1;
        end;
    end;
  except
    on E: Exception do
      FBuilder.SetException(Exception(AcquireExceptionObject));
  end;
end;

procedure TTestStateMachines.TestAsyncStateMachineExecution;
var
  LMachine: TSampleAsyncStateMachine;
  LStateMachineIntf: IAsyncStateMachine;
  LFut: specialize IFuture<Integer>;
begin
  LMachine := TSampleAsyncStateMachine.Create;
  LStateMachineIntf := LMachine;
  LFut := LMachine.Task;

  LMachine.FBuilder.Start(LStateMachineIntf);

  { Value should be 10 + 50 = 60 }
  AssertEquals('Async State Machine result', 60, LFut.Value);
end;

initialization
  RegisterTest(TTestFibers);
  RegisterTest(TTestGenerators);
  RegisterTest(TTestStateMachines);
end.
