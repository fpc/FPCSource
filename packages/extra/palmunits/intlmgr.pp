{$MACRO ON}

(******************************************************************************
 *
 * Copyright (c) 1998-2000 Palm, Inc. or its subsidiaries.
 * All rights reserved.
 *
 * File: IntlMgr.h
 *
 * Release: Palm OS SDK 4.0 (63220)
 *
 * Description:
 *   This file defines public Int'l Mgr structures and routines.
 *
 * History:
 * 03/21/98 kwk   Created by Ken Krugler.
 * 10/14/98 kwk   Added intlIntlGetRoutineAddress selector and
 *             IntlGetRoutineAddress routine declaration.
 * 08/05/99 kwk   Added intlIntlHandleEvent selector and the
 *             IntlHandleEvent routine declaration.
 * 09/22/99 kwk   Added intlTxtParamString selector.
 * 10/20/99 kwk   Moved private stuff to IntlPrv.h
 * 03/01/00 kwk   Added intlTxtConvertEncoding selector.
 * 05/10/00 kwk   Added intlIntlSetRoutineAddress selector & routine declaration.
 *             Also intlErrInvalidSelector.
 * 05/18/00 kwk   Added intlMgrStrict feature flag.
 * 05/26/00 kwk   Added intlTxtGetWordWrapOffset selector.
 * 07/13/00 kwk   Added intlTxtNameToEncoding selector.
 * 07/27/00 kwk   Added intlTxtConvertEncodingV35 selector.
 * 11/29/00 kwk   Added intlIntlStrictChecks selector.
 *
 *****************************************************************************)

unit intlmgr;

interface

uses palmos, coretraps, errorbase;

(***********************************************************************
 * Public constants
 ***********************************************************************)

const
// Bits set for the Intl Mgr feature.
 intlMgrExists          = $00000001;     // IntlMgr/TextMgr calls can be made.
 intlMgrStrict          = $00000002;     // Trigger extra errors on debug ROM.

// Intl manager error codes.
 intlErrInvalidSelector = intlErrorClass or 1;

// Selectors for routines found in the international manager. The order
// of these selectors MUST match the jump table in IntlDispatch.c.
const
  intlIntlInit               = 0;
  intlTxtByteAttr            = 1;
  intlTxtCharAttr            = 2;
  intlTxtCharXAttr           = 3;
  intlTxtCharSize            = 4;
  intlTxtGetPreviousChar     = 5;
  intlTxtGetNextChar         = 6;
  intlTxtGetChar             = 7;
  intlTxtSetNextChar         = 8;
  intlTxtCharBounds          = 9;
  intlTxtPrepFindString      = 10;
  intlTxtFindString          = 11;
  intlTxtReplaceStr          = 12;
  intlTxtWordBounds          = 13;
  intlTxtCharEncoding        = 14;
  intlTxtStrEncoding         = 15;
  intlTxtEncodingName        = 16;
  intlTxtMaxEncoding         = 17;
  intlTxtTransliterate       = 18;
  intlTxtCharIsValid         = 19;
  intlTxtCompare             = 20;
  intlTxtCaselessCompare     = 21;
  intlTxtCharWidth           = 22;
  intlTxtGetTruncationOffset = 23;
  intlIntlGetRoutineAddress  = 24;

// New for Palm OS 3.5
  intlIntlHandleEvent        = 25;
  intlTxtParamString         = 26;

// Patched for Palm OS 3.5.2
  intlTxtConvertEncodingV35  = 27;

// New for Palm OS 4.0
  intlTxtConvertEncoding     = 28;
  intlIntlSetRoutineAddress  = 29;
  intlTxtGetWordWrapOffset   = 30;
  intlTxtNameToEncoding      = 31;
  intlIntlStrictChecks       = 32;

  intlMaxSelector            = intlIntlStrictChecks;

type
  IntlSelector = UInt16;

(***********************************************************************
 * Public routines
 ***********************************************************************)

// Return back the address of the routine indicated by <inSelector>. If
// <inSelector> isn't a valid routine selector, return back NULL.
function IntlGetRoutineAddress(inSelector: IntlSelector): Pointer;

// Set the address of the international mgr routine indicated by <iSelector>
// to be <iProcPtr>. If <iSelector> isn't valid, return an error.
function IntlSetRoutineAddress(iSelector: IntlSelector; iProcPtr: Pointer): Err;

implementation

function __IntlGetRoutineAddress(inSelector: IntlSelector): Pointer; syscall sysTrapIntlDispatch;
function __IntlSetRoutineAddress(iSelector: IntlSelector; iProcPtr: Pointer): Err; syscall sysTrapIntlDispatch;

function IntlGetRoutineAddress(inSelector: IntlSelector): Pointer;
begin
 asm
  move.l #$intlIntlGetRoutineAddress, D2;
 end;
 IntlGetRoutineAddress := __IntlGetRoutineAddress(inSelector);
end;

function IntlSetRoutineAddress(iSelector: IntlSelector; iProcPtr: Pointer): Err;
begin
 asm
  move.l #$intlIntlSetRoutineAddress, D2;
 end;
 IntlSetRoutineAddress := __IntlSetRoutineAddress(iSelector, iProcPtr);
end;

end.
