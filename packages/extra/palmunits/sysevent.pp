(******************************************************************************
 *
 * Copyright (c) 1994-2000 Palm, Inc. or its subsidiaries.
 * All rights reserved.
 *
 * File: SysEvent.h
 *
 * Release: Palm OS SDK 4.0 (63220)
 *
 * Description:
 *        This file defines event structures and routines.
 *
 * History:
 *    September 26, 1994   Created by Art Lamb
 *       05/05/98 art   Add Text Services event.
 *       07/23/98 kwk   Changed UInt16 field in keyDown event to WChar.
 *       08/20/98 kwk   Split tsmEvent into tsmConfirmEvent & tsmFepButtonEvent.
 *       09/07/98 kwk   Added EvtPeekEvent routine declaration.
 *       10/13/98 kwk   Removed EvtPeekEvent until API can be finalized.
 *       03/11/99 grant Fixed types of pointers in SysEventType data fields.
 *       05/31/99 kwk   Added tsmFepModeEvent event.
 *       07/14/99 jesse Moved UI structures & constants to Event.h
 *                      defined ranges for future UI & system events.
 *       07/30/99 kwk   Moved TSM events here from Event.h
 *       09/12/99 gap   Add new multi-tap implementation
 *       09/14/99 gap   Removed EvtGetTrapState.
 *
 *****************************************************************************)
unit sysevent;

interface

uses palmos, coretraps, rect, window;

type
  SysEventsEnum = WordEnum;

const
  sysEventNilEvent = 0;
  sysEventPenDownEvent = Succ(sysEventNilEvent);
  sysEventPenUpEvent = Succ(sysEventPenDownEvent);
  sysEventPenMoveEvent = Succ(sysEventPenUpEvent);
  sysEventKeyDownEvent = Succ(sysEventPenMoveEvent);
  sysEventWinEnterEvent = Succ(sysEventKeyDownEvent);
  sysEventWinExitEvent = Succ(sysEventWinEnterEvent);
  sysEventAppStopEvent = 22;
  sysEventTsmConfirmEvent = 35;
  sysEventTsmFepButtonEvent = Succ(sysEventTsmConfirmEvent);
  sysEventTsmFepModeEvent = Succ(sysEventTsmFepButtonEvent);
  sysEventFrmTitleChangedEvent = Succ(sysEventTsmFepModeEvent);

  // add future UI level events in this numeric space
  // to save room for new system level events
  sysEventNextUIEvent = $0800;

  // <chg 2-25-98 RM> Equates added for library events
  sysEventFirstINetLibEvent = $1000;
  sysEventFirstWebLibEvent = $1100;

  // <chg 10/9/98 SCL> Changed firstUserEvent from 32767 (0x7FFF) to 0x6000
  // Enums are signed ints, so 32767 technically only allowed for ONE event.
  sysEventFirstUserEvent = $6000;
  sysEventLastUserEvent  = $7FFF;

// keyDownEvent modifers
const
  shiftKeyMask      = $0001;
  capsLockMask      = $0002;
  numLockMask       = $0004;
  commandKeyMask    = $0008;
  optionKeyMask     = $0010;
  controlKeyMask    = $0020;
  autoRepeatKeyMask = $0040; // True if generated due to auto-repeat
  doubleTapKeyMask  = $0080; // True if this is a double-tap event
  poweredOnKeyMask  = $0100; // True if this is a double-tap event
  appEvtHookKeyMask = $0200; // True if this is an app hook key
  libEvtHookKeyMask = $0400; // True if this is a library hook key

// define mask for all "virtual" keys
  virtualKeyMask    = appEvtHookKeyMask or libEvtHookKeyMask or commandKeyMask;

// Event timeouts
  evtWaitForever    = -1;
  evtNoWait         = 0;

type
  _GenericEventType = record
    datum: array [0..7] of UInt16;
  end;

  _PenUpEventType = record
    start: PointType;            // display coord. of stroke start
    end_: PointType;             // display coord. of stroke start
  end;

  _KeyDownEventType = record
    chr: WChar;               // ascii code
    keyCode: UInt16;          // virtual key code
    modifiers: UInt16;
  end;

  _WinEnterEventType = record
    enterWindow: WinHandle;
    exitWindow: WinHandle;
  end;

  _WinExitEventType = record
    enterWindow: WinHandle;
    exitWindow: WinHandle;
  end;

  _TSMConfirmType = record
    yomiText: PChar;
    formID: UInt16;
  end;

  _TSMFepButtonType = record
    buttonID: UInt16;
  end;

  _TSMFepModeEventType = record
    mode: UInt16;     // DOLATER kwk - use real type for mode?
  end;

// The event record.
  SysEventType = record
    eType: SysEventsEnum;
    penDown: Boolean;
    tapCount: UInt8;
    screenX: Coord;
    screenY: Coord;
    case Integer of
      1: (generic: _GenericEventType);
      2: (penUp: _PenUpEventType);
      3: (keyDown: _KeyDownEventType);
      4: (winEnter: _WinEnterEventType);
      5: (winExit: _WinExitEventType);
      6: (tsmConfirm: _TSMConfirmType);
      7: (tsmFepButton: _TSMFepButtonType);
      8: (tsmFepMode: _TSMFepModeEventType);
  end;

// Events are stored in the event queue with some extra fields:
  SysEventStoreType = record
    event: SysEventType;
    id: UInt32; // used to support EvtAddUniqueEvent
  end;

procedure PenGetPoint(var pScreenX, pScreenY: Int16; var pPenDown: Boolean); syscall sysTrapEvtGetPen;

implementation

end.
