{$MACRO ON}

(******************************************************************************
 *
 * Copyright (c) 1997-2000 Palm, Inc. or its subsidiaries.
 * All rights reserved.
 *
 * File: ExgLib.h
 *
 * Release: Palm OS SDK 4.0 (63220)
 *
 * Description:
 *    Include file the Exchange Library interface. The Exchange Library is a
 *    generic interface to any number of librarys. Any Exchange Library
 *    MUST have entrypoint traps in exactly the order listed here.
 *    The System Exchange manager functions call these functions when
 *    applications make calls to the Exchange manager. Applications will
 *    usually not make direct calls to this API.
 *
 * History:
 *    5/23/97 Created by Gavin Peacock
 *
 *****************************************************************************)

unit exglib;

interface

uses  palmos, libtraps, exgmgr;

// special exchange mgr event key
const
  exgIntDataChr = $01ff;

//-----------------------------------------------------------------------------
//  Obx library call ID's. Each library call gets the trap number:
//   exgTrapXXXX which serves as an index into the library's dispatch table.
//   The constant sysLibTrapCustom is the first available trap number after
//   the system predefined library traps Open,Close,Sleep & Wake.
//
// WARNING!!! This order of these traps MUST match the order of the dispatch
//  table in and Exchange library!!!
//-----------------------------------------------------------------------------

type
  ExgLibTrapNumberEnum = Enum;

const
  exgLibTrapHandleEvent = sysLibTrapCustom;
  exgLibTrapConnect = Succ(exgLibTrapHandleEvent);
  exgLibTrapAccept = Succ(exgLibTrapConnect);
  exgLibTrapDisconnect = Succ(exgLibTrapAccept);
  exgLibTrapPut = Succ(exgLibTrapDisconnect);
  exgLibTrapGet = Succ(exgLibTrapPut);
  exgLibTrapSend = Succ(exgLibTrapGet);
  exgLibTrapReceive = Succ(exgLibTrapSend);
  exgLibTrapControl = Succ(exgLibTrapReceive);
  exgLibTrapRequest = Succ(exgLibTrapControl);
  exgLibTrapReserved1 = Succ(exgLibTrapRequest);
  exgLibTrapReserved2 = Succ(exgLibTrapReserved1);
  exgLibTrapReserved3 = Succ(exgLibTrapReserved2);
  exgLibTrapReserved4 = Succ(exgLibTrapReserved3);
  exgLibTrapReserved5 = Succ(exgLibTrapReserved4);
  exgLibTrapReserved6 = Succ(exgLibTrapReserved5);
  exgLibTrapReserved7 = Succ(exgLibTrapReserved6);
  exgLibTrapReserved8 = Succ(exgLibTrapReserved7);
  exgLibTrapReserved9 = Succ(exgLibTrapReserved8);
  exgLibTrapReserved10 = Succ(exgLibTrapReserved9);
  exgLibTrapLast = Succ(exgLibTrapReserved10);

(************************************************************
 * Net Library procedures.
 *************************************************************)

//--------------------------------------------------
// Library initialization, shutdown, sleep and wake
//--------------------------------------------------
// Open the library - enable server for receiving data.
function ExgLibOpen(libRefnum: UInt16): Err; syscall sysLibTrapOpen;

function ExgLibClose(libRefnum: UInt16): Err; syscall sysLibTrapClose;

function ExgLibSleep(libRefnum: UInt16): Err; syscall sysLibTrapSleep;

function ExgLibWake(libRefnum: UInt16): Err; syscall sysLibTrapWake;

//  MemHandle events that this library needs. This will be called by
//  sysHandle event when certain low level events are triggered.
function ExgLibHandleEvent(libRefnum: UInt16; eventP: Pointer): Boolean; syscall exgLibTrapHandleEvent;

//  Establish a new connection
function ExgLibConnect(libRefNum: UInt16; exgSocketP: ExgSocketPtr): Err; syscall exgLibTrapConnect;

// Accept a connection request from remote end
function ExgLibAccept(libRefnum: UInt16; exgSocketP: ExgSocketPtr): Err; syscall exgLibTrapAccept;

// Disconnect
function ExgLibDisconnect(libRefnum: UInt16; exgSocketP: ExgSocketPtr; error: Err): Err; syscall exgLibTrapDisconnect;

// Initiate a Put command. This passes the name and other information about
// an object to be sent
function ExgLibPut(libRefnum: UInt16; exgSocketP: ExgSocketPtr): Err; syscall exgLibTrapPut;

// Initiate a Get command. This requests an object from the remote end.
function ExgLibGet(libRefNum: UInt16; exgSocketP: ExgSocketPtr): Err; syscall exgLibTrapGet;

// Send data to remote end - called after a Put command
function ExgLibSend(libRefNum: UInt16; exgSocketP: ExgSocketPtr; const bufP: Pointer; const bufLen: UInt32; var errP: Err): UInt32; syscall exgLibTrapSend;

// Receive data from remote end -- called after Accept
function ExgLibReceive(libRefNum: UInt16; exgSocketP: ExgSocketPtr; bufP: Pointer; const bufSize: UInt32; var errP: Err): UInt32; syscall exgLibTrapReceive;

// Send various option commands to the Exg library
function ExgLibControl(libRefNum: UInt16; op: UInt16; valueP: Pointer; var valueLenP: UInt16): Err; syscall exgLibTrapControl;

// Tell the Exg library to check for incoming data
function ExgLibRequest(libRefNum: UInt16; socketP: ExgSocketPtr): Err; syscall exgLibTrapRequest;

implementation

end.
