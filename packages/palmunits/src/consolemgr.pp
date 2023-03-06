{$MACRO ON}

(******************************************************************************
 *
 * Copyright (c) 1994-2000 Palm, Inc. or its subsidiaries.
 * All rights reserved.
 *
 * File: ConsoleMgr.h
 *
 * Release: Palm OS SDK 4.0 (63220)
 *
 * Description:
 *    This module implements simple text in and text out to a console
 *  application on the other end of the serial port. It talks through
 *  the Serial Link Manager and sends and receives packets of type slkPktTypeConsole.
 *
 * History:
 *    10/25/94  RM - Created by Ron Marianetti
 *
 *****************************************************************************)

{$IFNDEF FPC_DOTTEDUNITS}
unit consolemgr;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses PalmApi.Palmos, PalmApi.Coretraps;
{$ELSE FPC_DOTTEDUNITS}
uses palmos, coretraps;
{$ENDIF FPC_DOTTEDUNITS}

(********************************************************************
 * Console Manager Routines
 ********************************************************************)

function ConPutS(const message: PAnsiChar): Err; syscall sysTrapConPutS;

function ConGetS(message: PAnsiChar; timeout: Int32): Err; syscall sysTrapConGetS;

implementation

end.
