{$MACRO ON}

(******************************************************************************
 *
 * Copyright (c) 1994-2000 Palm, Inc. or its subsidiaries.
 * All rights reserved.
 *
 * File: KeyMgr.h
 *
 * Release: Palm OS SDK 4.0 (63220)
 *
 * Description:
 *    Include file for Key manager
 *
 * History:
 *    9/13/95 Created by Ron Marianetti
 *    2/04/98  srj-  added contrast key defines
 *    8/23/98  SCL-  Cross-merged 3.1 and 3.2
 *
 *****************************************************************************)

unit keymgr;

interface

uses palmos, coretraps;

(********************************************************************
 * Definition of bit field returned from KeyCurrentState
 ********************************************************************)

const
  keyBitPower          = $0001;     // Power key
  keyBitPageUp         = $0002;     // Page-up
  keyBitPageDown       = $0004;     // Page-down
  keyBitHard1          = $0008;     // App #1
  keyBitHard2          = $0010;     // App #2
  keyBitHard3          = $0020;     // App #3
  keyBitHard4          = $0040;     // App #4
  keyBitCradle         = $0080;     // Button on cradle
  keyBitAntenna        = $0100;     // Antenna "key" <chg 3-31-98 RM>
  keyBitContrast       = $0200;     // Contrast key

  keyBitsAll           = $FFFFFFFF; // all keys

  slowestKeyDelayRate  = $ff;
  slowestKeyPeriodRate = $ff;

(********************************************************************
 * Key manager Routines
 ********************************************************************)

// Set/Get the auto-key repeat rate
function KeyRates(set_: Boolean; var initDelayP, periodP, doubleTapDelayP: UInt16;
                  var queueAheadP: Boolean): Err; syscall sysTrapKeyRates;

// Get the current state of the hardware keys
// This is now updated every tick, even when more than 1 key is held down.
function KeyCurrentState: UInt32; syscall sysTrapKeyCurrentState;

// Set the state of the hardware key mask which controls if the key
// generates a keyDownEvent
function KeySetMask(keyMask: UInt32): UInt32; syscall sysTrapKeySetMask;

implementation

end.
