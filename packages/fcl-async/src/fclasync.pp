{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by the Free Pascal development team

    fcl-async: Coroutines, Fibers, and Async State Machines (Phase 4)
    Public umbrella unit

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.
}
{$mode objfpc}{$H+}

unit fclasync;

interface

uses
  async.types,
  async.fiber,
  async.generator,
  async.statemachine;

type
  { Re-export non-generic types }
  TFiberState = async.types.TFiberState;
  EFiberException = async.types.EFiberException;
  EAsyncException = async.types.EAsyncException;
  IAsyncStateMachine = async.types.IAsyncStateMachine;

  TFiber = async.fiber.TFiber;
  TAsyncTaskBuilder = async.statemachine.TAsyncTaskBuilder;

implementation

end.
