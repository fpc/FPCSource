(******************************************************************************
 *
 * Copyright (c) 1994-2000 Palm, Inc. or its subsidiaries.
 * All rights reserved.
 *
 * File: Event.h
 *
 * Release: Palm OS SDK 4.0 (63220)
 *
 * Description:
 *        This file defines UI event structures and routines.
 *
 * History:
 *    September 26, 1994   Created by Art Lamb
 *    07/14/99 jesse Separated from Event.h
 *    09/12/99 gap   Add for new multi-tap implementation
 *    09/14/99 gap   Removed EvtGetTrapState.
 *    10/28/99 kwk   Added EvtKeydownIsVirtual macro.
 *
 *****************************************************************************)
unit event_;

interface

uses palmos, coretraps, sysevent, control, day, field, list, scrollbar, table;

type
  eventsEnum = WordEnum;

const
  nilEvent = 0;                          // system level
  penDownEvent = Succ(nilEvent);         // system level
  penUpEvent = Succ(penDownEvent);       // system level
  penMoveEvent = Succ(penUpEvent);       // system level
  keyDownEvent = Succ(penMoveEvent);     // system level
  winEnterEvent = Succ(keyDownEvent);    // system level
  winExitEvent = Succ(winEnterEvent);    // system level
  ctlEnterEvent = Succ(winExitEvent);
  ctlExitEvent = Succ(ctlEnterEvent);
  ctlSelectEvent = Succ(ctlExitEvent);
  ctlRepeatEvent = Succ(ctlSelectEvent);
  lstEnterEvent = Succ(ctlRepeatEvent);
  lstSelectEvent = Succ(lstEnterEvent);
  lstExitEvent = Succ(lstSelectEvent);
  popSelectEvent = Succ(lstExitEvent);
  fldEnterEvent = Succ(popSelectEvent);
  fldHeightChangedEvent = Succ(fldEnterEvent);
  fldChangedEvent = Succ(fldHeightChangedEvent);
  tblEnterEvent = Succ(fldChangedEvent);
  tblSelectEvent = Succ(tblEnterEvent);
  daySelectEvent = Succ(tblSelectEvent);
  menuEvent = Succ(daySelectEvent);
  appStopEvent = 22;                     // system level
  frmLoadEvent = Succ(appStopEvent);
  frmOpenEvent = Succ(frmLoadEvent);
  frmGotoEvent = Succ(frmOpenEvent);
  frmUpdateEvent = Succ(frmGotoEvent);
  frmSaveEvent = Succ(frmUpdateEvent);
  frmCloseEvent = Succ(frmSaveEvent);
  frmTitleEnterEvent = Succ(frmCloseEvent);
  frmTitleSelectEvent = Succ(frmTitleEnterEvent);
  tblExitEvent = Succ(frmTitleSelectEvent);
  sclEnterEvent = Succ(tblExitEvent);
  sclExitEvent = Succ(sclEnterEvent);
  sclRepeatEvent = Succ(sclExitEvent);
  tsmConfirmEvent = 35;                      // system level
  tsmFepButtonEvent = Succ(tsmConfirmEvent); // system level
  tsmFepModeEvent = Succ(tsmFepButtonEvent); // system level

//DOLATER - peter: remove this:  frmTitleChangedEvent,      // system level
  attnIndicatorEnterEvent = Succ(tsmFepModeEvent);          // for attention manager's indicator
  attnIndicatorSelectEvent = Succ(attnIndicatorEnterEvent); // for attention manager's indicator

  // add future UI level events in this numeric space
  // to save room for new system level events
  menuCmdBarOpenEvent = $0800;
  menuOpenEvent = Succ(menuCmdBarOpenEvent);
  menuCloseEvent = Succ(menuOpenEvent);
  frmGadgetEnterEvent = Succ(menuCloseEvent);
  frmGadgetMiscEvent = Succ(frmGadgetEnterEvent);

  // <chg 2-25-98 RM> Equates added for library events
  firstINetLibEvent = $1000;
  firstWebLibEvent = $1100;

  // <chg 10/9/98 SCL> Changed firstUserEvent from 32767 (0x7FFF) to 0x6000
  // Enums are signed ints, so 32767 technically only allowed for ONE event.
  firstUserEvent = $6000;
  lastUserEvent  = $7FFF;

type
  ctlEnter = record
    controlID: UInt16;
    pControl: ControlPtr;
  end;

  ctlExit = record
    controlID: UInt16;
    pControl: ControlPtr;
  end;

  ctlSelect = record
    controlID: UInt16;
    pControl: ControlPtr;
    on: Boolean;
    reserved1: UInt8;
    value: UInt16; // used for slider controls only
  end;

  ctlRepeat = record
    controlID: UInt16;
    pControl: ControlPtr;
    time: UInt32;
    value: UInt16; // used for slider controls only
  end;

  fldEnter = record
    fieldID: UInt16;
    pField: FieldPtr;
  end;

  fldHeightChanged = record
    fieldID: UInt16;
    pField: FieldPtr;
    newHeight: Int16;
    currentPos: UInt16;
  end;

  fldChanged = record
    fieldID: UInt16;
    pField: ^FieldType;
  end;

  fldExit = record
    fieldID: UInt16;
    pField: ^FieldType;
  end;

  lstEnter = record
    listID: UInt16;
    pList: ^ListType;
    selection: Int16;
  end;

  lstExit = record
    listID: UInt16;
    pList: ListPtr;
  end;

  lstSelect = record
    listID: UInt16;
    pList: ^ListType;
    selection: Int16;
  end;

  tblEnter = record
    tableID: UInt16;
    pTable: TablePtr;
    row: Int16;
    column: Int16;
  end;

  tblExit = record
    tableID: UInt16;
    pTable: TablePtr;
    row: Int16;
    column: Int16;
  end;

  tblSelect = record
    tableID: UInt16;
    pTable: TablePtr;
    row: Int16;
    column: Int16;
  end;

  frmLoad = record
    formID: UInt16;
  end;

  frmOpen = record
    formID: UInt16;
  end;

  frmGoto = record
    formID: UInt16;
    recordNum: UInt16;     // index of record that contain a match
    matchPos: UInt16;      // postion in record of the match.
    matchLen: UInt16;      // length of match.
    matchFieldNum: UInt16; // field number string was found int
    matchCustom: UInt32;   // application specific info
  end;

  frmClose = record
    formID: UInt16;
  end;

  frmUpdate = record
    formID: UInt16;
    updateCode: UInt16;    // Application specific
  end;

  frmTitleEnter = record
    formID: UInt16;
  end;

  frmTitleSelect = record
    formID: UInt16;
  end;

  attnIndicatorEnter = record
    formID: UInt16;
  end;

  attnIndicatorSelect = record
    formID: UInt16;
  end;

  daySelect = record
    pSelector: ^DaySelectorType;
    selection: Int16;
    useThisDate: Boolean;
    reserved1: UInt8;
  end;

  menu = record
    itemID: UInt16;
  end;

  popSelect = record
    controlID: UInt16;
    controlP: ^ControlType;
    listID: UInt16;
    listP: ^ListType;
    selection: Int16;
    priorSelection: Int16;
  end;

  sclEnter = record
    scrollBarID: UInt16;
    pScrollBar: ^ScrollBarType;
  end;

  sclExit = record
    scrollBarID: UInt16;
    pScrollBar: ^ScrollBarType;
    value: Int16;
    newValue: Int16;
  end;

  sclRepeat = record
    scrollBarID: UInt16;
    pScrollBar: ^ScrollBarType;
    value: Int16;
    newValue: Int16;
    time: Int32;
  end;

  menuCmdBarOpen = record
   preventFieldButtons: Boolean; // set to stop the field from automatically adding cut/copy/paste
   reserved: UInt8;              // alignment padding
  end;

  menuOpen = record
   menuRscID: UInt16;
   //struct MenuBarType *pMenu;
   cause: Int16;
  end;

  gadgetEnter = record
    gadgetID: UInt16;         // must be same as gadgetMisc
    gadgetP: Pointer{^FormGadgetType}; // must be same as gadgetMisc
  end;

  gadgetMisc = record
    gadgetID: UInt16;         // must be same as gadgetEnter
    gadgetP: Pointer{^FormGadgetType}; // must be same as gadgetEnter
    selector: UInt16;
    dataP: Pointer;
  end;

// The event record.
type
  EventType = record
   eType: eventsEnum;
   penDown: Boolean;
   tapCount: UInt8;
   screenX: Int16;
   screenY: Int16;
   case Integer of
     1: (generic: _GenericEventType);
     2: (penUp: _PenUpEventType);
     3: (keyDown: _KeyDownEventType);
     4: (winEnter: _WinEnterEventType);
     5: (winExit: _WinExitEventType);
     6: (tsmConfirm: _TSMConfirmType);
     7: (tsmFepButton: _TSMFepButtonType);
     8: (tsmFepMode: _TSMFepModeEventType);
     9: (ctlEnter: ctlEnter);
     10: (ctlSelect: ctlSelect);
     11: (ctlRepeat: ctlRepeat);
     12: (ctlExit: ctlExit);
     13: (fldEnter: fldEnter);
     14: (fldHeightChanged: fldHeightChanged);
     15: (fldChanged: fldChanged);
     16: (fldExit: fldExit);
     17: (lstEnter: lstEnter);
     18: (lstExit: lstExit);
     19: (lstSelect: lstSelect);
     20: (tblEnter: tblEnter);
     21: (tblExit: tblExit);
     22: (tblSelect: tblSelect);
     23: (frmLoad: frmLoad);
     24: (frmOpen: frmOpen);
     25: (frmGoto: frmGoto);
     26: (frmClose: frmClose);
     27: (frmUpdate: frmUpdate);
     28: (frmTitleEnter: frmTitleEnter);
     29: (frmTitleSelect: frmTitleSelect);
     30: (attnIndicatorEnter: attnIndicatorEnter);
     31: (attnIndicatorSelect: attnIndicatorSelect);
     32: (daySelect: daySelect);
     33: (menu: menu);
     34: (popSelect: popSelect);
     35: (sclEnter: sclEnter);
     36: (sclExit: sclExit);
     37: (sclRepeat: sclRepeat);
     38: (menuCmdBarOpen: menuCmdBarOpen);
     39: (menuOpen: menuOpen);
     40: (gadgetEnter: gadgetEnter);
     41: (gadgetMisc: gadgetMisc);
  end;

  TEvent = EventType;
  EventPtr = ^EventType;

// Evaluate to true if <eventP> is a pointer to a virtual character key-
// down event. We assume that the caller has already determined the event
// is a keydown. WARNING!!! This macro is only safe to use on Palm OS 3.5
// or later. With earlier versions of the OS, use TxtGlueCharIsVirtual()
// in PalmOSGlue.lib

function EvtKeydownIsVirtual(eventP: EventPtr): Boolean;

//---------------------------------------------------------------------
// Event Functions
//---------------------------------------------------------------------

procedure EvtAddEventToQueue(const event: EventPtr); syscall sysTrapEvtAddEventToQueue;

procedure EvtAddUniqueEventToQueue(const eventP: EventPtr; id: UInt32; inPlace: Boolean); syscall sysTrapEvtAddUniqueEventToQueue;

procedure EvtCopyEvent(const source: EventPtr; dest: EventPtr); syscall sysTrapEvtCopyEvent;

procedure EvtGetEvent(var event: EventType; timeout: Int32); syscall sysTrapEvtGetEvent;

function EvtEventAvail: Boolean; syscall sysTrapEvtEventAvail;

procedure EvtGetPen(var pScreenX, pScreenY: Int16; var pPenDown: Boolean); syscall sysTrapEvtGetPen;

implementation

function EvtKeydownIsVirtual(eventP: EventPtr): Boolean;
begin
  EvtKeydownIsVirtual := (eventP^.keyDown.modifiers and virtualKeyMask) <> 0
end;

end.
