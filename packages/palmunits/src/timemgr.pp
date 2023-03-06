{$MACRO ON}
(******************************************************************************
 *
 * Copyright (c) 1995-2000 Palm, Inc. or its subsidiaries.
 * All rights reserved.
 *
 * File: TimeMgr.h
 *
 * Release: Palm OS SDK 4.0 (63220)
 *
 * Description:
 *    Time manager functions
 *
 * History:
 *    1/19/95  roger - Created by Roger Flores
 *
 *****************************************************************************)

{$IFNDEF FPC_DOTTEDUNITS}
unit timemgr;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses PalmApi.Palmos, PalmApi.Coretraps, PalmApi.Errorbase;
{$ELSE FPC_DOTTEDUNITS}
uses palmos, coretraps, errorbase;
{$ENDIF FPC_DOTTEDUNITS}

(************************************************************
 * Time Manager result codes
 * (timErrorClass is defined in SystemMgr.h)
 *************************************************************)

const
  timErrMemory = timErrorClass or 1;

(************************************************************
 * Function Prototypes
 *************************************************************)

//-------------------------------------------------------------------
// Initialization
//-------------------------------------------------------------------

function TimInit: Err; syscall sysTrapTimInit;

//-------------------------------------------------------------------
// API
//-------------------------------------------------------------------

// seconds since 1/1/1904
function TimGetSeconds: UInt32; syscall sysTrapTimGetSeconds;

// seconds since 1/1/1904
procedure TimSetSeconds(seconds: UInt32); syscall sysTrapTimSetSeconds;

// ticks since power on
function TimGetTicks: UInt32; syscall sysTrapTimGetTicks;

implementation

end.
