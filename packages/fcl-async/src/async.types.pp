{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by the Free Pascal development team

    Async/Coroutine Types and Interfaces (Phase 4, Step 1)

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.
}
{$mode objfpc}{$H+}
{$modeswitch advancedrecords}
{$modeswitch functionreferences}

unit async.types;

interface

uses
  SysUtils, Classes;

type
  TProc = reference to procedure;

  { Fiber and Coroutine execution states }
  TFiberState = (
    fsCreated,    { Fiber initialized, not yet started }
    fsRunning,    { Fiber is currently executing on a thread }
    fsSuspended,  { Fiber yielded / awaiting async operation }
    fsCompleted,  { Fiber finished execution successfully }
    fsFaulted     { Fiber terminated with unhandled exception }
  );

  TFiberProc = procedure;
  TFiberMethod = procedure of object;

  { Exception classes }
  EFiberException = class(Exception);
  EAsyncException = class(Exception);
  EInvalidFiberOperation = class(EFiberException);

  { Async State Machine interface (compatible with compiler-generated state machines) }
  IAsyncStateMachine = interface
    ['{8A95F63A-5BC2-4E12-B8E5-A13768D32E74}']
    procedure MoveNext;
    procedure SetStateMachine(const AStateMachine: IAsyncStateMachine);
  end;

  { Generic awaiter interface }
  generic IAwaiter<TResult> = interface
    ['{7C184E25-0E9A-40C2-9B7C-4148B928503C}']
    function IsCompleted: Boolean;
    function GetResult: TResult;
    procedure OnCompleted(const AContinuation: TProc);
  end;

implementation

end.
