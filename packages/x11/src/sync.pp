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

unit sync;

{$PACKRECORDS c}

interface

uses
  ctypes, x, xlib;

const
  libXext = 'Xext';

{$I syncconst.inc}

type
  PXSyncSystemCounter = ^TXSyncSystemCounter;
  TXSyncSystemCounter = record
    name: PAnsiChar;              { null-terminated name of system counter }
    counter: TXSyncCounter;   { counter id of this system counter }
    resolution: TXSyncValue;  { resolution of this system counter }
  end;

  PXSyncTrigger = ^TXSyncTrigger;
  TXSyncTrigger = record
    counter: TXSyncCounter;       { counter to trigger on }
    value_type: TXSyncValueType;  { absolute/relative }
    wait_value: TXSyncValue;      { value to compare counter to }
    test_type: TXSyncTestType;    { pos/neg comparison/transtion }
  end;

  PXSyncWaitCondition = ^TXSyncWaitCondition;
  TXSyncWaitCondition = record
    trigger: TXSyncTrigger;        { trigger for await }
    event_threshold: TXSyncValue;  { send event if past threshold }
  end;

  PXSyncAlarmAttributes = ^TXSyncAlarmAttributes;
  TXSyncAlarmAttributes = record
    trigger: TXSyncTrigger;
    delta: TXSyncValue;
    events: TBool;
    state: TXSyncAlarmState;
  end;

(*
 *  Events
 *)

  PXSyncCounterNotifyEvent = ^TXSyncCounterNotifyEvent;
  TXSyncCounterNotifyEvent = record
    _type: cint;                 { event base + XSyncCounterNotify }
    serial: culong;              { # of last request processed by server }
    send_event: TBool;           { true if this came from a SendEvent request }
    display: PDisplay;           { Display the event was read from }
    counter: TXSyncCounter;      { counter involved in await }
    wait_value: TXSyncValue;     { value being waited for }
    counter_value: TXSyncValue;  { counter value when this event was sent }
    time: TTime;                 { milliseconds }
    count: cint;                 { how many more events to come }
    destroyed: TBool;            { True if counter was destroyed }
  end;

  PXSyncAlarmNotifyEvent = ^TXSyncAlarmNotifyEvent;
  TXSyncAlarmNotifyEvent = record
    _type: cint;                 { event base + XSyncAlarmNotify }
    serial: culong;              { # of last request processed by server }
    send_event: TBool;           { true if this came from a SendEvent request }
    display: PDisplay;           { Display the event was read from }
    alarm: TXSyncAlarm;          { alarm that triggered }
    counter_value: TXSyncValue;  { value that triggered the alarm }
    alarm_value: TXSyncValue;    { test  value of trigger in alarm }
    time: TTime;                 { milliseconds }
    state: TXSyncAlarmState;     { new state of alarm }
  end;

(*
 *  Errors
 *)

  PXSyncAlarmError = ^TXSyncAlarmError;
  TXSyncAlarmError = record
    _type: cint;
    display: PDisplay;     { Display the event was read from }
    alarm: TXSyncAlarm;    { resource id }
    serial: culong;        { serial number of failed request }
    error_code: cuchar;    { error base + XSyncBadAlarm }
    request_code: cuchar;  { Major op-code of failed request }
    minor_code: cuchar;    { Minor op-code of failed request }
  end;

  PXSyncCounterError = ^TXSyncCounterError;
  TXSyncCounterError = record
    _type: cint;
    display: PDisplay;       { Display the event was read from }
    counter: TXSyncCounter;  { resource id }
    serial: culong;          { serial number of failed request }
    error_code: cuchar;      { error base + XSyncBadCounter }
    request_code: cuchar;    { Major op-code of failed request }
    minor_code: cuchar;      { Minor op-code of failed request }
  end;


procedure XSyncIntToValue(
    pv: PXSyncValue;
    i: cint
); cdecl; external libXext;

procedure XSyncIntsToValue(
    pv: PXSyncValue;
    l: cuint;
    h: cint
); cdecl; external libXext;

function XSyncValueGreaterThan(
    a: TXSyncValue;
    b: TXSyncValue
): TBoolResult; cdecl; external libXext;

function XSyncValueLessThan(
    a: TXSyncValue;
    b: TXSyncValue
): TBoolResult; cdecl; external libXext;

function XSyncValueGreaterOrEqual(
    a: TXSyncValue;
    b: TXSyncValue
): TBoolResult; cdecl; external libXext;

function XSyncValueLessOrEqual(
    a: TXSyncValue;
    b: TXSyncValue
): TBoolResult; cdecl; external libXext;

function XSyncValueEqual(
    a: TXSyncValue;
    b: TXSyncValue
): TBoolResult; cdecl; external libXext;

function XSyncValueIsNegative(
    v: TXSyncValue
): TBoolResult; cdecl; external libXext;

function XSyncValueIsZero(
    a: TXSyncValue
): TBoolResult; cdecl; external libXext;

function XSyncValueIsPositive(
    v: TXSyncValue
): TBoolResult; cdecl; external libXext;

function XSyncValueLow32(
    v: TXSyncValue
): cuint; cdecl; external libXext;

function XSyncValueHigh32(
    v: TXSyncValue
): cint; cdecl; external libXext;

procedure XSyncValueAdd(
    presult: PXSyncValue;
    a: TXSyncValue;
    b: TXSyncValue;
    poverflow: Pcint
); cdecl; external libXext;

procedure XSyncValueSubtract(
    presult: PXSyncValue;
    a: TXSyncValue;
    b: TXSyncValue;
    poverflow: Pcint
); cdecl; external libXext;

procedure XSyncMaxValue(
    pv: PXSyncValue
); cdecl; external libXext;

procedure XSyncMinValue(
    pv: PXSyncValue
); cdecl; external libXext;

function XSyncQueryExtension(
    dpy: PDisplay;
    event_base_return,
    error_base_return: Pcint
): TStatus; cdecl; external libXext;

function XSyncInitialize(
    dpy: PDisplay;
    major_version_return,
    minor_version_return: Pcint
): TStatus; cdecl; external libXext;

function XSyncListSystemCounters(
    dpy: PDisplay;
    n_counters_return: Pcint
): PXSyncSystemCounter; cdecl; external libXext;

procedure XSyncFreeSystemCounterList(
    list: PXSyncSystemCounter
); cdecl; external libXext;

function XSyncCreateCounter(
    dpy: PDisplay;
    initial_value: TXSyncValue
): TXSyncCounter; cdecl; external libXext;

function XSyncSetCounter(
    dpy: PDisplay;
    counter: TXSyncCounter;
    value: TXSyncValue
): TStatus; cdecl; external libXext;

function XSyncChangeCounter(
    dpy: PDisplay;
    counter: TXSyncCounter;
    value: TXSyncValue
): TStatus; cdecl; external libXext;

function XSyncDestroyCounter(
    dpy: PDisplay;
    counter: TXSyncCounter
): TStatus; cdecl; external libXext;

function XSyncQueryCounter(
    dpy: PDisplay;
    counter: TXSyncCounter;
    value_return: PXSyncValue
): TStatus; cdecl; external libXext;

function XSyncAwait(
    dpy: PDisplay;
    wait_list: PXSyncWaitCondition;
    n_conditions: cint
): TStatus; cdecl; external libXext;

function XSyncCreateAlarm(
    dpy: PDisplay;
    values_mask: culong;
    values: PXSyncAlarmAttributes
): TXSyncAlarm; cdecl; external libXext;

function XSyncDestroyAlarm(
    dpy: PDisplay;
    alarm: TXSyncAlarm
): TStatus; cdecl; external libXext;

function XSyncQueryAlarm(
    dpy: PDisplay;
    alarm: TXSyncAlarm;
    values_return: PXSyncAlarmAttributes
): TStatus; cdecl; external libXext;

function XSyncChangeAlarm(
    dpy: PDisplay;
    alarm: TXSyncAlarm;
    values_mask: culong;
    values: PXSyncAlarmAttributes
): TStatus; cdecl; external libXext;

function XSyncSetPriority(
    dpy: PDisplay;
    client_resource_id: TXID;
    priority: cint
): TStatus; cdecl; external libXext;

function XSyncGetPriority(
    dpy: PDisplay;
    client_resource_id: TXID;
    return_priority: Pcint
): TStatus; cdecl; external libXext;

function XSyncCreateFence(
    dpy: PDisplay;
    d: TDrawable;
    initially_triggered: TBool
): TXSyncFence; cdecl; external libXext;

function XSyncTriggerFence(
    dpy: PDisplay;
    fence: TXSyncFence
): TBoolResult; cdecl; external libXext;

function XSyncResetFence(
    dpy: PDisplay;
    fence: TXSyncFence
): TBoolResult; cdecl; external libXext;

function XSyncDestroyFence(
    dpy: PDisplay;
    fence: TXSyncFence
): TBoolResult; cdecl; external libXext;

function XSyncQueryFence(
    dpy: PDisplay;
    fence: TXSyncFence;
    triggered: PBool
): TBoolResult; cdecl; external libXext;

function XSyncAwaitFence(
    dpy: PDisplay;
    fence_list: PXSyncFence;
    n_fences: cint
): TBoolResult; cdecl; external libXext;

implementation
end.
