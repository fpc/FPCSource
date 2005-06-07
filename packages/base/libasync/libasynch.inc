{

    libasync: Asynchronous event management
    Copyright (C) 2001-2002 by
      Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org

    Common interface declaration

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

type
  TAsyncHandleStruct = packed record
    UserData: Pointer;
    Data: TAsyncData;
  end;
  TAsyncHandle = ^TAsyncHandleStruct;

  TAsyncTimer = Pointer;

  TAsyncCallback = procedure(UserData: Pointer); cdecl;

  TAsyncResult = (
    asyncOK,
    asyncInvalidHandle,
    asyncInvalidFileHandle,
    asyncHandlerAlreadySet);


// Construction and destruction

procedure asyncInit(
  Handle: TAsyncHandle); cdecl;

procedure asyncFree(
  Handle: TAsyncHandle); cdecl;


// Running and stopping the event loop

procedure asyncRun(
  Handle: TAsyncHandle); cdecl;

procedure asyncBreak(
  Handle: TAsyncHandle); cdecl;


// Status information

function asyncIsRunning(
  Handle: TAsyncHandle
  ): Boolean; cdecl;

function asyncGetTicks: Int64; cdecl;


// Timer management

function asyncAddTimer(
  Handle: TAsyncHandle;
  MSec: LongInt;
  Periodic: Boolean;            // False = One-shot timer, True = Periodic timer
  Callback: TAsyncCallback;
  UserData: Pointer             // User data for callback
  ): TAsyncTimer; cdecl;

procedure asyncRemoveTimer(
  Handle: TAsyncHandle;
  Timer: TASyncTimer); cdecl;


// I/O callback management

function asyncSetIOCallback(
  Handle: TAsyncHandle;
  IOHandle: LongInt;
  Callback: TAsyncCallback;
  UserData: Pointer): TAsyncResult; cdecl;

procedure asyncClearIOCallback(
  Handle: TAsyncHandle;
  IOHandle: LongInt); cdecl;

function asyncSetDataAvailableCallback(
  Handle: TAsyncHandle;
  IOHandle: LongInt;
  Callback: TAsyncCallback;
  UserData: Pointer): TAsyncResult; cdecl;

procedure asyncClearDataAvailableCallback(
  Handle: TAsyncHandle;
  IOHandle: LongInt); cdecl;

function asyncSetCanWriteCallback(
  Handle: TAsyncHandle;
  IOHandle: LongInt;
  Callback: TAsyncCallback;
  UserData: Pointer): TAsyncResult; cdecl;

procedure asyncClearCanWriteCallback(
  Handle: TAsyncHandle;
  IOHandle: LongInt); cdecl;


