{$MACRO ON}

(******************************************************************************
 *
 * Copyright (c) 1994-2000 Palm, Inc. or its subsidiaries.
 * All rights reserved.
 *
 * File: Keyboard.h
 *
 * Release: Palm OS SDK 4.0 (63220)
 *
 * Description:
 *   This file defines the keyboard's  structures
 *   and routines.
 *
 * History:
 *    March 29, 1995 Created by Roger Flores
 *
 *****************************************************************************)

unit keyboard;

interface

uses palmos, coretraps, rect, chars, window, control;

const
  kbdReturnKey = linefeedChr;
  kbdTabKey = tabChr;
  kbdBackspaceKey = backspaceChr;
  kbdShiftKey = 2;
  kbdCapsKey = 1;
  kbdNoKey = $ff;

type
  KeyboardType = Enum;

const
 kbdAlpha = 0;
 kbdNumbersAndPunc = 1;
 kbdAccent = 2;
 kbdDefault = $ff; // based on graffiti mode (usually alphaKeyboard)

type
  KeyboardStatus = record
  end;
  KeyboardStatusPtr = ^KeyboardStatus;

// Shift state flags
const
  KeyboardShiftFlag = $0001;
  KeyboardCapslockFlag = $0002;

(************************************************************
 * Keyboard procedures
 *************************************************************)

// At some point the Graffiti code will need access to the
// shift and caps lock info.  Either export the structures
// or provide calls to the info.

procedure SysKeyboardDialogV10; syscall sysTrapSysKeyboardDialogV10;

procedure SysKeyboardDialog(kbd: KeyboardType); syscall sysTrapSysKeyboardDialog;

function KeyboardStatusNew(keyboardID: UInt16): KeyboardStatusPtr; syscall sysTrapKeyboardStatusNew;

procedure KeyboardStatusFree(ks: KeyboardStatusPtr); syscall sysTrapKeyboardStatusFree;

procedure KbdSetLayout(ks: KeyboardStatusPtr; layout: UInt16); syscall sysTrapKbdSetLayout;

function KbdGetLayout(const ks: KeyboardStatusPtr): UInt16; syscall sysTrapKbdGetLayout;

procedure KbdSetPosition(ks: KeyboardStatusPtr; const p: PointPtr); syscall sysTrapKbdSetPosition;

procedure KbdGetPosition(const ks: KeyboardStatusPtr; p: PointPtr); syscall sysTrapKbdGetPosition;

procedure KbdSetShiftState(ks: KeyboardStatusPtr; shiftState: UInt16); syscall sysTrapKbdSetShiftState;

function KbdGetShiftState(const ks: KeyboardStatusPtr): UInt16; syscall sysTrapKbdGetShiftState;

procedure KbdDraw(ks: KeyboardStatusPtr; keyTopsOnly, ignoreModifiers: Boolean); syscall sysTrapKbdDraw;

procedure KbdErase(ks: KeyboardStatusPtr); syscall sysTrapKbdErase;

function KbdHandleEvent(ks: KeyboardStatusPtr; pEvent: EventPtr): Boolean; syscall sysTrapKbdHandleEvent;

implementation

end.
