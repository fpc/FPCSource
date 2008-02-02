{$MACRO ON}

(******************************************************************************
 *
 * Copyright (c) 1995-2000 Palm, Inc. or its subsidiaries.
 * All rights reserved.
 *
 * File: GraffitiShift.h
 *
 * Release: Palm OS SDK 4.0 (63220)
 *
 * Description:
 *        This file defines Griffiti shift state indicator routines.
 *
 * History:
 *    Aug 24, 1995   Created by Art Lamb
 *      mm/dd/yy   initials - brief revision comment
 *
 *****************************************************************************)

unit graffitishift;

interface

uses palmos, coretraps;

// Graffiti lock flags
const
  glfCapsLock = $01;
  glfNumLock  = $02;

type
  GsiShiftState = Enum;

const
  gsiShiftNone = 0;                             // no indicator
  gsiNumLock = Succ(gsiShiftNone);              // numeric lock
  gsiCapsLock = Succ(gsiNumLock);               // capital lock
  gsiShiftPunctuation = Succ(gsiCapsLock);      // punctuation shift
  gsiShiftExtended = Succ(gsiShiftPunctuation); // extented punctuation shift
  gsiShiftUpper = Succ(gsiShiftExtended);       // alpha upper case shift
  gsiShiftLower = Succ(gsiShiftUpper);          // alpha lower case

procedure GsiInitialize; syscall sysTrapGsiInitialize;

procedure GsiSetLocation(const x, y: Int16); syscall sysTrapGsiSetLocation;

procedure GsiEnable(const enableIt: Boolean); syscall sysTrapGsiEnable;

function GsiEnabled: Boolean; syscall sysTrapGsiEnabled;

procedure GsiSetShiftState(const lockFlags, tempShift: UInt16); syscall sysTrapGsiSetShiftState;

implementation

end.
