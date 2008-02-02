{$MACRO ON}

(******************************************************************************
 *
 * Copyright (c) 1994-2000 Palm, Inc. or its subsidiaries.
 * All rights reserved.
 *
 * File: SelTime.h
 *
 * Release: Palm OS SDK 4.0 (63220)
 *
 * Description:
 *   This file defines select time structures and routines.
 *
 * History:
 *    December 6, 1994  Created by Roger Flores
 *           Nick Twyman 8/4/98. Added SelectOneTime trap
 *
 *****************************************************************************)

unit seltime;

interface

uses palmos, coretraps, datetime;

//-------------------------------------------------------------------
// structures
//-------------------------------------------------------------------

type
  HMSTime = record
    hours: UInt8;
    minutes: UInt8;
    seconds: UInt8;
    reserved: UInt8;
  end;

// This is slated to be deleted in the next version.
function SelectTimeV33(var startTimeP, EndTimeP: TimeType; untimed: Boolean;
                       const titleP: PChar; startOfDay: Int16): Boolean; syscall sysTrapSelectTimeV33;

function SelectTime(var startTimeP, EndTimeP: TimeType; untimed: Boolean; const titleP: PChar;
                    startOfDay, endOfDay, startOfDisplay: Int16): Boolean; syscall sysTrapSelectTime;

function SelectOneTime(var hour, minute: Int16; const titleP: PChar): Boolean; syscall sysTrapSelectOneTime;

implementation

end.
