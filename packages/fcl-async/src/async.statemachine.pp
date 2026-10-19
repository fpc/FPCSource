{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by the Free Pascal development team

    Async State Machine Builders & TaskCompletionSource (Phase 4, Step 1 & 2)
    Provides runtime foundation for stackless async/await state machines

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.
}
{$mode objfpc}{$H+}
{$modeswitch advancedrecords}
{$modeswitch functionreferences}
{$modeswitch anonymousfunctions}

unit async.statemachine;

interface

uses
  SysUtils, Classes, SyncObjs,
  System.Threading,
  async.types;

type
  { Manual Task Completion Source for typed results }
  generic TTaskCompletionSource<TResult> = class
  private
    FTask: specialize IFuture<TResult>;
    FResult: TResult;
    FException: Exception;
    FCompleted: Boolean;
    FCanceled: Boolean;
    FLock: TCriticalSection;
    FCond: PRTLEvent;
    function FutureComputation: TResult;
  public
    constructor Create;
    destructor Destroy; override;

    function SetResult(const AResult: TResult): Boolean;
    function SetException(const AException: Exception): Boolean;
    function SetCanceled: Boolean;

    property Task: specialize IFuture<TResult> read FTask;
  end;

  { State machine method builder for typed futures }
  generic TAsyncTaskMethodBuilder<TResult> = record
  private type
    TSource = specialize TTaskCompletionSource<TResult>;
  private
    FSource: TSource;
  public
    class function Create: specialize TAsyncTaskMethodBuilder<TResult>; static;
    function GetTask: specialize IFuture<TResult>;
    procedure Start(const AStateMachine: IAsyncStateMachine);
    procedure AwaitOnCompleted(const ATask: ITask; const AStateMachine: IAsyncStateMachine);
    procedure SetResult(const AResult: TResult);
    procedure SetException(const AException: Exception);

    property Task: specialize IFuture<TResult> read GetTask;
  end;

  { State machine method builder for void ITask }
  TAsyncTaskBuilder = record
  private
    FSource: specialize TTaskCompletionSource<Boolean>;
  public
    class function Create: TAsyncTaskBuilder; static;
    function GetTask: ITask;
    procedure Start(const AStateMachine: IAsyncStateMachine);
    procedure AwaitOnCompleted(const ATask: ITask; const AStateMachine: IAsyncStateMachine);
    procedure SetResult;
    procedure SetException(const AException: Exception);

    property Task: ITask read GetTask;
  end;

implementation

{ TTaskCompletionSource<TResult> }

function TTaskCompletionSource.FutureComputation: TResult;
begin
  FLock.Acquire;
  try
    while not FCompleted do
    begin
      FLock.Release;
      RTLEventWaitFor(FCond);
      FLock.Acquire;
    end;

    if FCanceled then
      raise EOperationCancelled.Create('Task was cancelled');
    if FException <> nil then
      raise Exception.Create(FException.Message);
    Result := FResult;
  finally
    FLock.Release;
  end;
end;

constructor TTaskCompletionSource.Create;
begin
  inherited Create;
  FCompleted := False;
  FCanceled := False;
  FException := nil;
  FLock := TCriticalSection.Create;
  FCond := RTLEventCreate;
  FTask := TTask.specialize Future<TResult>(@FutureComputation);
  FTask.Start;
end;

destructor TTaskCompletionSource.Destroy;
begin
  RTLEventDestroy(FCond);
  FreeAndNil(FLock);
  FException.Free;
  inherited Destroy;
end;

function TTaskCompletionSource.SetResult(const AResult: TResult): Boolean;
begin
  FLock.Acquire;
  try
    if FCompleted then
      Exit(False);
    FResult := AResult;
    FCompleted := True;
    RTLEventSetEvent(FCond);
    Result := True;
  finally
    FLock.Release;
  end;
end;

function TTaskCompletionSource.SetException(const AException: Exception): Boolean;
begin
  FLock.Acquire;
  try
    if FCompleted then
      Exit(False);
    FException := AException;
    FCompleted := True;
    RTLEventSetEvent(FCond);
    Result := True;
  finally
    FLock.Release;
  end;
end;

function TTaskCompletionSource.SetCanceled: Boolean;
begin
  FLock.Acquire;
  try
    if FCompleted then
      Exit(False);
    FCanceled := True;
    FCompleted := True;
    RTLEventSetEvent(FCond);
    Result := True;
  finally
    FLock.Release;
  end;
end;

{ TAsyncTaskMethodBuilder<TResult> }

class function TAsyncTaskMethodBuilder.Create: specialize TAsyncTaskMethodBuilder<TResult>;
begin
  Result.FSource := TSource.Create;
end;

function TAsyncTaskMethodBuilder.GetTask: specialize IFuture<TResult>;
begin
  if FSource = nil then
    FSource := TSource.Create;
  Result := FSource.Task;
end;

procedure TAsyncTaskMethodBuilder.Start(const AStateMachine: IAsyncStateMachine);
begin
  if FSource = nil then
    FSource := TSource.Create;
  AStateMachine.MoveNext;
end;

procedure TAsyncTaskMethodBuilder.AwaitOnCompleted(const ATask: ITask; const AStateMachine: IAsyncStateMachine);
var
  LState: IAsyncStateMachine;
begin
  LState := AStateMachine;
  ATask.ContinueWith(
    procedure(ACompletedTask: ITask)
    begin
      LState.MoveNext;
    end);
end;

procedure TAsyncTaskMethodBuilder.SetResult(const AResult: TResult);
begin
  if FSource <> nil then
    FSource.SetResult(AResult);
end;

procedure TAsyncTaskMethodBuilder.SetException(const AException: Exception);
begin
  if FSource <> nil then
    FSource.SetException(AException);
end;

{ TAsyncTaskBuilder }

class function TAsyncTaskBuilder.Create: TAsyncTaskBuilder;
begin
  Result.FSource := specialize TTaskCompletionSource<Boolean>.Create;
end;

function TAsyncTaskBuilder.GetTask: ITask;
begin
  if FSource = nil then
    FSource := specialize TTaskCompletionSource<Boolean>.Create;
  Result := FSource.Task;
end;

procedure TAsyncTaskBuilder.Start(const AStateMachine: IAsyncStateMachine);
begin
  if FSource = nil then
    FSource := specialize TTaskCompletionSource<Boolean>.Create;
  AStateMachine.MoveNext;
end;

procedure TAsyncTaskBuilder.AwaitOnCompleted(const ATask: ITask; const AStateMachine: IAsyncStateMachine);
var
  LState: IAsyncStateMachine;
begin
  LState := AStateMachine;
  ATask.ContinueWith(
    procedure(ACompletedTask: ITask)
    begin
      LState.MoveNext;
    end);
end;

procedure TAsyncTaskBuilder.SetResult;
begin
  if FSource <> nil then
    FSource.SetResult(True);
end;

procedure TAsyncTaskBuilder.SetException(const AException: Exception);
begin
  if FSource <> nil then
    FSource.SetException(AException);
end;

end.
