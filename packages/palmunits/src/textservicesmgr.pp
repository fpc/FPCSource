{$MACRO ON}
(******************************************************************************
 *
 * Copyright (c) 1998-2000 Palm, Inc. or its subsidiaries.
 * All rights reserved.
 *
 * File: TextServicesMgr.h
 *
 * Release: Palm OS SDK 4.0 (63220)
 *
 * Description:
 *    Header file for Text Services Manager. This provides the caller with
 *    an API for interacting with various text services, including front-end
 *    processors (FEPs), which are sometimes known as input methods.
 *
 * History:
 *    03/05/98 kwk   Created by Ken Krugler.
 *    02/03/99 kwk   Changed name to TextServicesMgr.h, was TextServices.h.
 *    10/20/99 kwk   Moved private stuff into TextServicesPrv.h
 *    04/19/00 kwk   Use portable typedef for TsmSelector and TsmFepModeType.
 *                   Fixed up descriptions for TsmGet/SetFepMode. Added new
 *                   selectors for TsmInit, TsmDrawMode, TsmFepHandleEvent,
 *                   TsmFepTerminate, and TsmFepCommit.
 *    07/06/00 kwk   Set type of unused status ptr param to be void*, and
 *                   moved TsmFepStatusType into the private header file.
 *    08/21/00 kwk   Moved tsmFtrCreator here from TextServicesPrv.h.
 *                   Added tsmFtrNumFlags, tsmFtrFlagsHasFep.
 *    11/15/00 kwk   Added tsmGet/SetSystemFep, tsmGet/SetCurrentFep selectors.
 *
 *****************************************************************************)

unit textservicesmgr;

interface

uses palmos, coretraps, systemresources;

(***********************************************************************
 * Public constants
 ***********************************************************************)

// Feature Creators and numbers, for use with the FtrGet() call.
const
  tsmFtrCreator = sysFileCTextServices;

// Selector used with call to FtrGet(tsmFtrCreator, xxx) to get the
// Text Services Manager flags.
  tsmFtrNumFlags = 0;

// Flags returned by FtrGet(tsmFtrCreator, tsmFtrNumFlags) call.
  tsmFtrFlagsHasFep = $1; // Bit set if FEP is installed.

// Selectors for routines found in the Text Services manager. The order
// of these selectors MUST match the jump table in TextServicesMgr.c.
type
  TsmSelector = UInt16;

const
  tsmGetFepMode_    = 0;
  tsmSetFepMode_    = 1;
  tsmHandleEvent    = 2;
  tsmInit           = 3; // new in 4.0
  tsmDrawMode       = 4; // new in 4.0
  tsmGetSystemFep   = 5; // new in 4.0
  tsmSetSystemFep   = 6; // new in 4.0
  tsmGetCurrentFep  = 7; // new in 4.0
  tsmSetCurrentFep  = 8; // new in 4.0

  tsmMaxSelector    = tsmSetCurrentFep;

// Input mode - used with TsmGet/SetFepMode.
type
  TsmFepModeType = UInt16;

const
  tsmFepModeDefault = TsmFepModeType(0);
  tsmFepModeOff     = TsmFepModeType(1);
  tsmFepModeCustom  = TsmFepModeType(128);

(***********************************************************************
 * Public types
 ***********************************************************************)

(***********************************************************************
 * Public routines
 ***********************************************************************)

// Return the current mode for the active FEP. The <nullParam> parameter
// is unused and must be set to NULL.
function TsmGetFepMode(nullParam: Pointer): TsmFepModeType;

// Set the mode for the active FEP to be <inNewMode>. The previous mode
// is returned. The <nullParam> parameter is unused and must be set
// to NULL.
function TsmSetFepMode(nullParam: Pointer; inNewMode: TsmFepModeType): TsmFepModeType;

implementation

function __TsmGetFepMode(nullParam: Pointer): TsmFepModeType; syscall sysTrapTsmDispatch;
function __TsmSetFepMode(nullParam: Pointer; inNewMode: TsmFepModeType): TsmFepModeType; syscall sysTrapTsmDispatch;

function TsmGetFepMode(nullParam: Pointer): TsmFepModeType;
begin
 asm
  move.l #$tsmGetFepMode_, D2;
 end;
 TsmGetFepMode := __TsmGetFepMode(nullParam);
end;

function TsmSetFepMode(nullParam: Pointer; inNewMode: TsmFepModeType): TsmFepModeType;
begin
 asm
  move.l #$tsmSetFepMode_, D2;
 end;
 TsmSetFepMode := __TsmSetFepMode(nullParam, inNewMode);
end;

end.
