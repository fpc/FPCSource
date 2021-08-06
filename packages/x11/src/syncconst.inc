(*

Copyright 1991, 1993, 1994, 1998  The Open Group

Permission to use, copy, modify, distribute, and sell this software and its
documentation for any purpose is hereby granted without fee, provided that
the above copyright notice appear in all copies and that both that
copyright notice and this permission notice appear in supporting
documentation.

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
OPEN GROUP BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN
AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

Except as contained in this notice, the name of The Open Group shall not be
used in advertising or otherwise to promote the sale, use or other dealings
in this Software without prior written authorization from The Open Group.

*)

(***********************************************************
Copyright 1991,1993 by Digital Equipment Corporation, Maynard, Massachusetts,
and Olivetti Research Limited, Cambridge, England.

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its
documentation for any purpose and without fee is hereby granted,
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in
supporting documentation, and that the names of Digital or Olivetti
not be used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.

DIGITAL AND OLIVETTI DISCLAIM ALL WARRANTIES WITH REGARD TO THIS
SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND
FITNESS, IN NO EVENT SHALL THEY BE LIABLE FOR ANY SPECIAL, INDIRECT OR
CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF
USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
PERFORMANCE OF THIS SOFTWARE.

******************************************************************)

const
  SYNC_NAME = 'SYNC';

  SYNC_MAJOR_VERSION = 3;
  SYNC_MINOR_VERSION = 1;


  XSyncCounterNotify   = 0;
  XSyncAlarmNotify     = 1;
  XSyncAlarmNotifyMask = 1 shl XSyncAlarmNotify;

  XSyncNumberEvents    = 2;

  XSyncBadCounter      = 0;
  XSyncBadAlarm        = 1;
  XSyncBadFence        = 2;
  XSyncNumberErrors    = XSyncBadFence + 1;

(*
 * Flags for Alarm Attributes
 *)
  XSyncCACounter   = 1 shl 0;
  XSyncCAValueType = 1 shl 1;
  XSyncCAValue     = 1 shl 2;
  XSyncCATestType  = 1 shl 3;
  XSyncCADelta     = 1 shl 4;
  XSyncCAEvents    = 1 shl 5;

type
(*
 * Constants for the value_type argument of various requests
 *)
  PXSyncValueType = ^TXSyncValueType;
  TXSyncValueType = (
    XSyncAbsolute,
    XSyncRelative);

(*
 * Alarm Test types
 *)
  PXSyncTestType = ^TXSyncTestType;
  TXSyncTestType = (
    XSyncPositiveTransition,
    XSyncNegativeTransition,
    XSyncPositiveComparison,
    XSyncNegativeComparison);

(*
 * Alarm state constants
 *)
  PXSyncAlarmState = ^TXSyncAlarmState;
  TXSyncAlarmState = (
    XSyncAlarmActive,
    XSyncAlarmInactive,
    XSyncAlarmDestroyed);

  PXSyncCounter = ^TXSyncCounter;
  TXSyncCounter = TXID;

  PXSyncAlarm = ^TXSyncAlarm;
  TXSyncAlarm = TXID;

  PXSyncFence = ^TXSyncFence;
  TXSyncFence = TXID;

  PXSyncValue = ^TXSyncValue;
  TXSyncValue = record
    hi: cint;
    lo: cuint;
  end;
