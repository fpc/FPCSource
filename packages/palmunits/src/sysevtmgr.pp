{$MACRO ON}

{$define Rsc := }
(******************************************************************************
 *
 * Copyright (c) 1994-2000 Palm, Inc. or its subsidiaries.
 * All rights reserved.
 *
 * File: SysEvtMgr.h
 *
 * Release: Palm OS SDK 4.0 (63220)
 *
 * Description:
 *    Header for the System Event Manager
 *
 * History:
 *    03/22/95 RM    Created by Ron Marianetti
 *    07/23/98 kwk   Changed UInt16 param in EvtEnqueueKey to WChar.
 *
 *****************************************************************************)

unit sysevtmgr;

interface

uses palmos, coretraps, rect, errorbase, sysevent;

(************************************************************
 * System Event Manager Errors
 *************************************************************)

const
  evtErrParamErr   = evtErrorClass or 1;
  evtErrQueueFull  = evtErrorClass or 2;
  evtErrQueueEmpty = evtErrorClass or 3;

(************************************************************
 * Commands for EvtSetAutoOffTimer()
 *************************************************************)
type
  EvtSetAutoOffCmd = Enum;

const
  SetAtLeast = 0;    // turn off in at least xxx seconds
  SetExactly = Succ(SetAtLeast);    // turn off in xxx seconds
  SetAtMost  = Succ(SetExactly);     // turn off in at most xxx seconds
  SetDefault = Succ(SetAtMost);    // change default auto-off timeout to xxx seconds
  ResetTimer = Succ(SetDefault);    // reset the timer to the default auto-off timeout

(************************************************************
 * Pen button info structure. This structure is used
 *  to hold the bounds of each button on the silk screen and
 *  the ascii code and modifiers byte that each will generate
 *  when tapped by the user.
 *************************************************************)

type
  PenBtnInfoType = record
    boundsR: RectangleType;        // bounding rectangle of button
    asciiCode: WChar;              // ascii code for key event
    keyCode: UInt16;               // virtual key code for key event
    modifiers: UInt16;             // modifiers for key event
  end;
  PenBtnInfoPtr = ^PenBtnInfoType;

type
  PenBtnListType = record
    numButtons: UInt16;                      // Count of number of buttons
    buttons: array [0..0] of PenBtnInfoType; // Placeholder for one or more buttons
  end;

(************************************************************
 * Silkscreen area info structure. An array of these structures
 * is returned by the EvtGetSilkscreenAreaList function.
 *************************************************************)

// Different types of rectangles on the display. For new vendor areas,
// the type should be set to the vendor's creator code, as assigned
// by 3Com's Partner Engineering group.

const
  silkscreenRectScreen    = Rsc('scrn');
  silkscreenRectGraffiti  = Rsc('graf');

// Values for SilkscreenAreaType.index if areaType = silkscreenRectGraffiti
  alphaGraffitiSilkscreenArea   = 0;
  numericGraffitiSilkscreenArea = 1;

// One silkscreen area. The areaType field tells us which type of
// area it is, while the index field has different meanings depending
// on the area type.

type
  SilkscreenAreaType = record
    bounds: RectangleType;
    areaType: UInt32; // four byte creator code.
    index: UInt16;
  end;
  SilkscreenAreaPtr = ^SilkscreenAreaType;

(************************************************************
 * System Event Manager procedures
 *************************************************************)

//-----------------------------------------------------------------
// High Level Calls
//------------------------------------------------------------------

function EvtSysInit: Err; syscall sysTrapEvtSysInit;

// Return next "System" event. This routine will send strokes to Graffiti as necessary
//  and return a key event. Otherwise, it will return a simple pen down or pen
//  up event, or put the processor to sleep for a max time of 'timeout' if
// no events are available.
procedure EvtGetSysEvent(var eventP: SysEventType; timeout: Int32); syscall sysTrapEvtGetSysEvent;

// Return true if there is a low level system event (pen or key) available
function EvtSysEventAvail(ignorePenUps: Boolean): Boolean; syscall sysTrapEvtSysEventAvail;

// Translate a stroke in the silk screen area to a key event
function EvtProcessSoftKeyStroke(var startPtP, endPtP: PointType): Err; syscall sysTrapEvtProcessSoftKeyStroke;

//-----------------------------------------------------------------
// Pen Queue Utilties
//------------------------------------------------------------------

// Replace current pen queue with another of the given size
function EvtSetPenQueuePtr(penQueueP: MemPtr; size: UInt32): Err; syscall sysTrapEvtSetPenQueuePtr;

// Return size of current pen queue in bytes
function EvtPenQueueSize: UInt32; syscall sysTrapEvtPenQueueSize;

// Flush the pen queue
function EvtFlushPenQueue: Err; syscall sysTrapEvtFlushPenQueue;

// Append a point to the pen queue. Passing -1 for x and y means
//  pen-up (terminate the current stroke). Called by digitizer interrupt routine
function EvtEnqueuePenPoint(var ptP: PointType): Err; syscall sysTrapEvtEnqueuePenPoint;

// Return the stroke info for the next stroke in the pen queue. This MUST
//  be the first call when removing a stroke from the queue
function EvtDequeuePenStrokeInfo(var startPtP, endPtP: PointType): Err; syscall sysTrapEvtDequeuePenStrokeInfo;

// Dequeue the next point from the pen queue. Returns non-0 if no
//  more points. The point returned will be (-1,-1) at the end
//  of the stroke.
function EvtDequeuePenPoint(var retP: PointType): Err; syscall sysTrapEvtDequeuePenPoint;

// Flush the entire stroke from the pen queue and dispose it
function EvtFlushNextPenStroke: Err; syscall sysTrapEvtFlushNextPenStroke;

//-----------------------------------------------------------------
// Key Queue Utilties
//------------------------------------------------------------------

// Replace current key queue with another of the given size. This routine will
//  intialize the given key queue before installing it
function EvtSetKeyQueuePtr(keyQueueP: MemPtr; size: UInt32): Err; syscall sysTrapEvtSetKeyQueuePtr;

// Return size of current key queue in bytes
function EvtKeyQueueSize: UInt32; syscall sysTrapEvtKeyQueueSize;

// Flush the key queue
function EvtFlushKeyQueue: Err; syscall sysTrapEvtFlushKeyQueue;

// Append a key to the key queue.
function EvtEnqueueKey(ascii: WChar; keycode, modifiers: UInt16): Err; syscall sysTrapEvtEnqueueKey;

// Return true of key queue empty.
function EvtKeyQueueEmpty: Boolean; syscall sysTrapEvtKeyQueueEmpty;

// Pop off the next key event from the key queue and fill in the given
//  event record structure. Returns non-zero if there aren't any keys in the
//  key queue. If peek is non-zero, key will be left in key queue.
function EvtDequeueKeyEvent(var eventP: SysEventType; peek: UInt16): Err; syscall sysTrapEvtDequeueKeyEvent;

//-----------------------------------------------------------------
// Silkscreen information calls
//------------------------------------------------------------------

// Return pointer to the pen based button list
function EvtGetPenBtnList(var numButtons: UInt16): PenBtnInfoPtr; syscall sysTrapEvtGetPenBtnList;

// Return pointer to the silkscreen area list
function EvtGetSilkscreenAreaList(var numAreas: UInt16): SilkscreenAreaPtr; syscall sysTrapEvtGetSilkscreenAreaList;

//-----------------------------------------------------------------
// General Utilities
//------------------------------------------------------------------
// Force the system to wake-up. This will result in a null event being
//  sent to the current app.
function EvtWakeup: Err; syscall sysTrapEvtWakeup;

// Force the system to wake-up. This will NOT result in a null event being
//  sent to the current app.
function EvtWakeupWithoutNilEvent: Err; syscall sysTrapEvtWakeupWithoutNilEvent;

// Reset the auto-off timer. This is called by the SerialLink Manager in order
//  so we don't auto-off while receiving data over the serial port.
function EvtResetAutoOffTimer: Err; syscall sysTrapEvtResetAutoOffTimer;

function EvtSetAutoOffTimer(cmd: EvtSetAutoOffCmd; timeout: UInt16): Err; syscall sysTrapEvtSetAutoOffTimer;

// Set Graffiti enabled or disabled.
procedure EvtEnableGraffiti(enable: Boolean); syscall sysTrapEvtEnableGraffiti;

// Force a NullEvent at or before tick
function EvtSetNullEventTick(tick: UInt32): Boolean; syscall sysTrapEvtSetNullEventTick;

(************************************************************
 * Assembly Function Prototypes
 *************************************************************)

//#define  _EvtEnqueuePenPoint ASM_SYS_TRAP(sysTrapEvtEnqueuePenPoint)

implementation

end.
