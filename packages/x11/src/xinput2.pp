(*
 * Copyright © 2009 Red Hat, Inc.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice (including the next
 * paragraph) shall be included in all copies or substantial portions of the
 * Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 *
 *)

unit xinput2;

interface

{$PACKRECORDS C}

{ Definitions used by the library and client }

uses
  ctypes, X, Xlib, XI2, Xge, Xfixes;


const
  libXi = 'Xi';

type
(*******************************************************************
 *
 *)
  PXIAddMasterInfo = ^TXIAddMasterInfo;
  TXIAddMasterInfo = record
    _type: cint;
    name: PChar;
    send_core: TBool;
    enable: TBool;
  end;

  PXIRemoveMasterInfo = ^TXIRemoveMasterInfo;
  TXIRemoveMasterInfo = record
    _type: cint;
    deviceid: cint;
    return_mode: cint; { AttachToMaster, Floating }
    return_pointer: cint;
    return_keyboard: cint;
  end;

  PXIAttachSlaveInfo = ^TXIAttachSlaveInfo;
  TXIAttachSlaveInfo = record
    _type: cint;
    deviceid: cint;
    new_master: cint;
  end;

  PXIDetachSlaveInfo = ^TXIDetachSlaveInfo;
  TXIDetachSlaveInfo = record
    _type: cint;
    deviceid: cint;
  end;

  PXIAnyHierarchyChangeInfo = ^TXIAnyHierarchyChangeInfo;
  TXIAnyHierarchyChangeInfo = record
    case Integer of
      0: (_type: cint); { must be first element }
      1: (add: TXIAddMasterInfo);
      2: (remove: TXIRemoveMasterInfo);
      3: (attach: TXIAttachSlaveInfo);
      4: (detach: TXIDetachSlaveInfo);
  end;

  PXIModifierState = ^TXIModifierState;
  TXIModifierState = record
    base: cint;
    latched: cint;
    locked: cint;
    effective: cint;
  end;

  PXIGroupState = ^TXIGroupState;
  TXIGroupState = TXIModifierState;

  PXIButtonState = ^TXIButtonState;
  TXIButtonState = record
    mask_len: cint;
    mask: Pcuchar;
  end;

  PXIValuatorState = ^TXIValuatorState;
  TXIValuatorState = record
    mask_len: cint;
    mask: Pcuchar;
    values: Pcdouble;
  end;


  PXIEventMask = ^TXIEventMask;
  TXIEventMask = record
    deviceid: cint;
    mask_len: cint;
    mask: Pcuchar;
  end;

  PPXIAnyClassInfo = ^PXIAnyClassInfo;
  PXIAnyClassInfo = ^TXIAnyClassInfo;
  TXIAnyClassInfo = record
    _type: cint;
    sourceid: cint;
  end;

  PXIButtonClassInfo = ^TXIButtonClassInfo;
  TXIButtonClassInfo = record
    _type: cint;
    sourceid: cint;
    num_buttons: cint;
    labels: PAtom;
    state: TXIButtonState;
  end;

  PXIKeyClassInfo = ^TXIKeyClassInfo;
  TXIKeyClassInfo = record
    _type: cint;
    sourceid: cint;
    num_keycodes: cint;
    keycodes: Pcint;
  end;

  PXIValuatorClassInfo = ^TXIValuatorClassInfo;
  TXIValuatorClassInfo = record
    _type: cint;
    sourceid: cint;
    number: cint;
    _label: TAtom;
    min: cdouble;
    max: cdouble;
    value: cdouble;
    resolution: cint;
    mode: cint;
  end;

{ new in XI 2.1 }
  PXIScrollClassInfo = ^TXIScrollClassInfo;
  TXIScrollClassInfo = record
    _type: cint;
    sourceid: cint;
    number: cint;
    scroll_type: cint;
    increment: cdouble;
    flags: cint;
  end;

  PXITouchClassInfo = ^TXITouchClassInfo;
  TXITouchClassInfo = record
    _type: cint;
    sourceid: cint;
    mode: cint;
    num_touches: cint;
  end;

  PXIDeviceInfo = ^TXIDeviceInfo;
  TXIDeviceInfo = record
    deviceid: cint;
    name: PChar;
    use: cint;
    attachment: cint;
    enabled: TBool;
    num_classes: cint;
    classes: PPXIAnyClassInfo;
  end;

  PXIGrabModifiers = ^TXIGrabModifiers;
  TXIGrabModifiers = record
    modifiers: cint;
    status: cint;
  end;

  PBarrierEventID = ^TBarrierEventID;
  TBarrierEventID = cuint;

  PXIBarrierReleasePointerInfo = ^TXIBarrierReleasePointerInfo;
  TXIBarrierReleasePointerInfo = record
    deviceid: cint;
    barrier: TPointerBarrier;
    eventid: TBarrierEventID;
  end;

(**
 * Generic XI2 event. All XI2 events have the same header.
 *)
  PXIEvent = ^TXIEvent;
  TXIEvent = record
    _type: cint;                { GenericEvent }
    serial: culong;             { # of last request processed by server }
    send_event: TBool;          { true if this came from a SendEvent request }
    display: PDisplay;          { Display the event was read from }
    extension: cint;            { XI extension offset }
    evtype: cint;
    time: TTime;
 end;


  PXIHierarchyInfo = ^TXIHierarchyInfo;
  TXIHierarchyInfo = record
    deviceid: cint;
    attachment: cint;
    use: cint;
    enabled: TBool;
    flags: cint;
  end;

(*
 * Notifies the client that the device hierarchy has been changed. The client
 * is expected to re-query the server for the device hierarchy.
 *)
  PXIHierarchyEvent = ^TXIHierarchyEvent;
  TXIHierarchyEvent = record
    _type: cint;                { GenericEvent }
    serial: culong;             { # of last request processed by server }
    send_event: TBool;          { true if this came from a SendEvent request }
    display: PDisplay;          { Display the event was read from }
    extension: cint;            { XI extension offset }
    evtype: cint;               { XI_HierarchyChanged }
    time: TTime;
    flags: cint;
    num_info: cint;
    info: PXIHierarchyInfo;
  end;

(*
 * Notifies the client that the classes have been changed. This happens when
 * the slave device that sends through the master changes.
 *)
  PXIDeviceChangedEvent = ^TXIDeviceChangedEvent;
  TXIDeviceChangedEvent = record
    _type: cint;                { GenericEvent }
    serial: culong;             { # of last request processed by server }
    send_event: TBool;          { true if this came from a SendEvent request }
    display: PDisplay;          { Display the event was read from }
    extension: cint;            { XI extension offset }
    evtype: cint;               { XI_DeviceChanged }
    time: TTime;
    deviceid: cint;             { id of the device that changed }
    sourceid: cint;             { Source for the new classes. }
    reason: cint;               { Reason for the change }
    num_classes: cint;
    classes: PPXIAnyClassInfo;  { same as in XIDeviceInfo }
  end;

  PXIDeviceEvent = ^TXIDeviceEvent;
  TXIDeviceEvent = record
    _type: cint;                { GenericEvent }
    serial: culong;             { # of last request processed by server }
    send_event: TBool;          { true if this came from a SendEvent request }
    display: PDisplay;          { Display the event was read from }
    extension: cint;            { XI extension offset }
    evtype: cint;
    time: TTime;
    deviceid: cint;
    sourceid: cint;
    detail: cint;
    root: TWindow;
    event: TWindow;
    child: TWindow;
    root_x: cdouble;
    root_y: cdouble;
    event_x: cdouble;
    event_y: cdouble;
    flags: cint;
    buttons: TXIButtonState;
    valuators: TXIValuatorState;
    mods: TXIModifierState;
    group: TXIGroupState;
  end;

  PXIRawEvent = ^TXIRawEvent;
  TXIRawEvent = record
    _type: cint;                { GenericEvent }
    serial: culong;             { # of last request processed by server }
    send_event: TBool;          { true if this came from a SendEvent request }
    display: PDisplay;          { Display the event was read from }
    extension: cint;            { XI extension offset }
    evtype: cint;               { XI_RawKeyPress, XI_RawKeyRelease, etc. }
    time: TTime;
    deviceid: cint;
    sourceid: cint;             { Bug: Always 0. https://bugs.freedesktop.org//show_bug.cgi?id=34240 }
    detail: cint;
    flags: cint;
    valuators: TXIValuatorState;
    raw_values: Pcdouble;
  end;

  PXIEnterEvent = ^TXIEnterEvent;
  TXIEnterEvent = record
    _type: cint;                { GenericEvent }
    serial: culong;             { # of last request processed by server }
    send_event: TBool;          { true if this came from a SendEvent request }
    display: PDisplay;          { Display the event was read from }
    extension: cint;            { XI extension offset }
    evtype: cint;
    time: TTime;
    deviceid: cint;
    sourceid: cint;
    detail: cint;
    root: TWindow;
    event: TWindow;
    child: TWindow;
    root_x: cdouble;
    root_y: cdouble;
    event_x: cdouble;
    event_y: cdouble;
    mode: cint;
    focus: TBool;
    same_screen: TBool;
    buttons: TXIButtonState;
    mods: TXIModifierState;
    group: TXIGroupState;
  end;

  PXILeaveEvent = ^TXILeaveEvent;
  TXILeaveEvent = TXIEnterEvent;
  PXIFocusInEvent = ^TXIFocusInEvent;
  TXIFocusInEvent = TXIEnterEvent;
  PXIFocusOutEvent = ^TXIFocusOutEvent;
  TXIFocusOutEvent = TXIEnterEvent;

  PXIPropertyEvent = ^TXIPropertyEvent;
  TXIPropertyEvent = record
    _type: cint;                { GenericEvent }
    serial: culong;             { # of last request processed by server }
    send_event: TBool;          { true if this came from a SendEvent request }
    display: PDisplay;          { Display the event was read from }
    extension: cint;            { XI extension offset }
    evtype: cint;               { XI_PropertyEvent }
    time: TTime;
    deviceid: cint;             { id of the device that changed }
    _property: TAtom;
    what: cint;
  end;

  PXITouchOwnershipEvent = ^TXITouchOwnershipEvent;
  TXITouchOwnershipEvent = record
    _type: cint;                { GenericEvent }
    serial: culong;             { # of last request processed by server }
    send_event: TBool;          { true if this came from a SendEvent request }
    display: PDisplay;          { Display the event was read from }
    extension: cint;            { XI extension offset }
    evtype: cint;
    time: TTime;
    deviceid: cint;
    sourceid: cint;
    touchid: cuint;
    root: TWindow;
    event: TWindow;
    child: TWindow;
    flags: cint;
  end;

  PXIBarrierEvent = ^TXIBarrierEvent;
  TXIBarrierEvent = record
    _type: cint;                { GenericEvent }
    serial: culong;             { # of last request processed by server }
    send_event: TBool;          { true if this came from a SendEvent request }
    display: PDisplay;          { Display the event was read from }
    extension: cint;            { XI extension offset }
    evtype: cint;
    time: TTime;
    deviceid: cint;
    sourceid: cint;
    event: TWindow;
    root: TWindow;
    root_x: cdouble;
    root_y: cdouble;
    dx: cdouble;
    dy: cdouble;
    dtime: cint;
    flags: cint;
    barrier: TPointerBarrier;
    eventid: TBarrierEventID;
  end;

//_XFUNCPROTOBEGIN

function XIQueryPointer(
    display: PDisplay;
    deviceid: cint;
    win: TWindow;
    root: PWindow;
    child: PWindow;
    root_x: Pcdouble;
    root_y: Pcdouble;
    win_x: Pcdouble;
    win_y: Pcdouble;
    buttons: PXIButtonState;
    mods: PXIModifierState;
    group: PXIGroupState
): TBoolResult; cdecl; external libXi;

function XIWarpPointer(
    display: PDisplay;
    deviceid: cint;
    src_win: TWindow;
    dst_win: TWindow;
    src_x: cdouble;
    src_y: cdouble;
    src_width: cuint;
    src_height: cuint;
    dst_x: cdouble;
    dst_y: cdouble
): TBoolResult; cdecl; external libXi;

function XIDefineCursor(
    display: PDisplay;
    deviceid: cint;
    win: TWindow;
    cursor: TCursor
): TStatus; cdecl; external libXi;

function XIUndefineCursor(
    display: PDisplay;
    deviceid: cint;
    win: TWindow
): TStatus; cdecl; external libXi;

function XIChangeHierarchy(
    display: PDisplay;
    changes: PXIAnyHierarchyChangeInfo;
    num_changes: cint
): TStatus; cdecl; external libXi;

function XISetClientPointer(
    dpy: PDisplay;
    win: TWindow;
    deviceid: cint
): TStatus; cdecl; external libXi;

function XIGetClientPointer(
    dpy: PDisplay;
    win: TWindow;
    deviceid: Pcint
): TBoolResult; cdecl; external libXi;

function XISelectEvents(
    dpy: PDisplay;
    win: TWindow;
    masks: PXIEventMask;
    num_masks: cint
): cint; cdecl; external libXi;

function XIGetSelectedEvents(
    dpy: PDisplay;
    win: TWindow;
    num_masks_return: Pcint
): PXIEventMask; cdecl; external libXi;

function XIQueryVersion(
    dpy: PDisplay;
    major_version_inout: Pcint;
    minor_version_inout: Pcint
): TStatus; cdecl; external libXi;

function XIQueryDevice(
    dpy: PDisplay;
    deviceid: cint;
    ndevices_return: Pcint
): PXIDeviceInfo; cdecl; external libXi;

function XISetFocus(
    dpy: PDisplay;
    deviceid: cint;
    focus: TWindow;
    time: TTime
): TStatus; cdecl; external libXi;

function XIGetFocus(
    dpy: PDisplay;
    deviceid: cint;
    focus_return: PWindow): TStatus; cdecl; external libXi;

function XIGrabDevice(
    dpy: PDisplay;
    deviceid: cint;
    grab_window: TWindow;
    time: TTime;
    cursor: TCursor;
    grab_mode: cint;
    paired_device_mode: cint;
    owner_events: TBool;
    mask: PXIEventMask
): TStatus; cdecl; external libXi;

{ boolean overload for the TBool parameter }
function XIGrabDevice(
    dpy: PDisplay;
    deviceid: cint;
    grab_window: TWindow;
    time: TTime;
    cursor: TCursor;
    grab_mode: cint;
    paired_device_mode: cint;
    owner_events: Boolean;
    mask: PXIEventMask
): TStatus; inline;

function XIUngrabDevice(
    dpy: PDisplay;
    deviceid: cint;
    time: TTime
): TStatus; cdecl; external libXi;

function XIAllowEvents(
    display: PDisplay;
    deviceid: cint;
    event_mode: cint;
    time: TTime
): TStatus; cdecl; external libXi;

function XIAllowTouchEvents(
    display: PDisplay;
    deviceid: cint;
    touchid: cuint;
    grab_window: TWindow;
    event_mode: cint
): TStatus; cdecl; external libXi;

function XIGrabButton(
    display: PDisplay;
    deviceid: cint;
    button: cint;
    grab_window: TWindow;
    cursor: TCursor;
    grab_mode: cint;
    paired_device_mode: cint;
    owner_events: cint;
    mask: PXIEventMask;
    num_modifiers: cint;
    modifiers_inout: PXIGrabModifiers
): cint; cdecl; external libXi;

function XIGrabKeycode(
    display: PDisplay;
    deviceid: cint;
    keycode: cint;
    grab_window: TWindow;
    grab_mode: cint;
    paired_device_mode: cint;
    owner_events: cint;
    mask: PXIEventMask;
    num_modifiers: cint;
    modifiers_inout: PXIGrabModifiers
): cint; cdecl; external libXi;

function XIGrabEnter(
    display: PDisplay;
    deviceid: cint;
    grab_window: TWindow;
    cursor: TCursor;
    grab_mode: cint;
    paired_device_mode: cint;
    owner_events: cint;
    mask: PXIEventMask;
    num_modifiers: cint;
    modifiers_inout: PXIGrabModifiers
): cint; cdecl; external libXi;

function XIGrabFocusIn(
    display: PDisplay;
    deviceid: cint;
    grab_window: TWindow;
    grab_mode: cint;
    paired_device_mode: cint;
    owner_events: cint;
    mask: PXIEventMask;
    num_modifiers: cint;
    modifiers_inout: PXIGrabModifiers
): cint; cdecl; external libXi;

function XIGrabTouchBegin(
    display: PDisplay;
    deviceid: cint;
    grab_window: TWindow;
    owner_events: cint;
    mask: PXIEventMask;
    num_modifiers: cint;
    modifiers_inout: PXIGrabModifiers
): cint; cdecl; external libXi;

function XIUngrabButton(
    display: PDisplay;
    deviceid: cint;
    button: cint;
    grab_window: TWindow;
    num_modifiers: cint;
    modifiers: PXIGrabModifiers
): TStatus; cdecl; external libXi;

function XIUngrabKeycode(
    display: PDisplay;
    deviceid: cint;
    keycode: cint;
    grab_window: TWindow;
    num_modifiers: cint;
    modifiers: PXIGrabModifiers
): TStatus; cdecl; external libXi;

function XIUngrabEnter(
    display: PDisplay;
    deviceid: cint;
    grab_window: TWindow;
    num_modifiers: cint;
    modifiers: PXIGrabModifiers
): TStatus; cdecl; external libXi;

function XIUngrabFocusIn(
    display: PDisplay;
    deviceid: cint;
    grab_window: TWindow;
    num_modifiers: cint;
    modifiers: PXIGrabModifiers
): TStatus; cdecl; external libXi;

function XIUngrabTouchBegin(
    display: PDisplay;
    deviceid: cint;
    grab_window: TWindow;
    num_modifiers: cint;
    modifiers: PXIGrabModifiers
): TStatus; cdecl; external libXi;

function XIListProperties(
    display: PDisplay;
    deviceid: cint;
    num_props_return: Pcint
): PAtom; cdecl; external libXi;

procedure XIChangeProperty(
    display: PDisplay;
    deviceid: cint;
    _property: TAtom;
    _type: TAtom;
    format: cint;
    mode: cint;
    data: Pcuchar;
    num_items: cint
); cdecl; external libXi;

procedure XIDeleteProperty(
    display: PDisplay;
    deviceid: cint;
    _property: TAtom
); cdecl; external libXi;

function XIGetProperty(
    display: PDisplay;
    deviceid: cint;
    _property: TAtom;
    offset: clong;
    length: clong;
    delete_property: TBool;
    _type: TAtom;
    type_return: PAtom;
    format_return: Pcint;
    num_items_return: Pculong;
    bytes_after_return: Pculong;
    data: PPcuchar
): TStatus; cdecl; external libXi;

{ boolean overload for the TBool parameter }
function XIGetProperty(
    display: PDisplay;
    deviceid: cint;
    _property: TAtom;
    offset: clong;
    length: clong;
    delete_property: Boolean;
    _type: TAtom;
    type_return: PAtom;
    format_return: Pcint;
    num_items_return: Pculong;
    bytes_after_return: Pculong;
    data: PPcuchar
): TStatus; inline;

procedure XIBarrierReleasePointers(
    display: PDisplay;
    barriers: PXIBarrierReleasePointerInfo;
    num_barriers: cint
); cdecl; external libXi;

procedure XIBarrierReleasePointer(
    display: PDisplay;
    deviceid: cint;
    barrier: TPointerBarrier;
    eventid: TBarrierEventID
); cdecl; external libXi;

procedure XIFreeDeviceInfo(info: PXIDeviceInfo); cdecl; external libXi;

//_XFUNCPROTOEND

implementation

function XIGrabDevice(dpy: PDisplay; deviceid: cint; grab_window: TWindow;
  time: TTime; cursor: TCursor; grab_mode: cint; paired_device_mode: cint;
  owner_events: Boolean; mask: PXIEventMask): TStatus; inline;
begin
  XIGrabDevice := XIGrabDevice(dpy, deviceid, grab_window, time, cursor, grab_mode, paired_device_mode, Ord(owner_events), mask);
end;

function XIGetProperty(display: PDisplay; deviceid: cint; _property: TAtom;
  offset: clong; length: clong; delete_property: Boolean; _type: TAtom;
  type_return: PAtom; format_return: Pcint; num_items_return: Pculong;
  bytes_after_return: Pculong; data: PPcuchar): TStatus;
begin
  XIGetProperty := XIGetProperty(display, deviceid, _property, offset, length, Ord(delete_property), _type, type_return, format_return,
    num_items_return, bytes_after_return, data);
end;

end.
