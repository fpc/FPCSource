{
    $Id$

    libasync: Asynchronous event management
    Copyright (C) 2001 by
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
  Periodic: Boolean;		// False = One-shot timer, True = Periodic timer
  Callback: TAsyncCallback;
  UserData: Pointer		// User data for callback
  ): TAsyncTimer; cdecl;

procedure asyncRemoveTimer(
  Handle: TAsyncHandle;
  Timer: TASyncTimer); cdecl;


// I/O callback management

procedure asyncSetIOCallback(
  Handle: TAsyncHandle;
  IOHandle: LongInt;
  Callback: TAsyncCallback;
  UserData: Pointer); cdecl;

procedure asyncClearIOCallback(
  Handle: TAsyncHandle;
  IOHandle: LongInt); cdecl;

procedure asyncSetDataAvailableCallback(
  Handle: TAsyncHandle;
  IOHandle: LongInt;
  Callback: TAsyncCallback;
  UserData: Pointer); cdecl;

procedure asyncClearDataAvailableCallback(
  Handle: TAsyncHandle;
  IOHandle: LongInt); cdecl;

procedure asyncSetCanWriteCallback(
  Handle: TAsyncHandle;
  IOHandle: LongInt;
  Callback: TAsyncCallback;
  UserData: Pointer); cdecl;

procedure asyncClearCanWriteCallback(
  Handle: TAsyncHandle;
  IOHandle: LongInt); cdecl;


{
  $Log$
  Revision 1.1  2002-01-29 17:54:53  peter
    * splitted to base and extra

  Revision 1.2  2001/12/11 17:45:28  marco
   * was only commited to fixes.

  Revision 1.1.2.2  2001/11/16 12:51:41  sg
  * Now different handlers for available data and space in write buffer can
    be set
  * LOTS of bugfixes in the implementation
  * fpAsync now has a write buffer class (a read buffer class for reading
    line by line will be included in the next release)

  Revision 1.1.2.1  2001/09/08 15:43:24  sg
  * First public version

}
