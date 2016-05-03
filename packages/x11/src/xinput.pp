(************************************************************

Copyright 1989, 1998  The Open Group

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

Copyright 1989 by Hewlett-Packard Company, Palo Alto, California.

			All Rights Reserved

Permission to use, copy, modify, and distribute this software and its
documentation for any purpose and without fee is hereby granted,
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in
supporting documentation, and that the name of Hewlett-Packard not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.

HEWLETT-PACKARD DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
HEWLETT-PACKARD BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

********************************************************)

{ Definitions used by the library and client }

unit xinput;

interface

{$PACKRECORDS C}
{$MODE objfpc}

uses
  ctypes, x, xlib, xi;

const
  libXi = 'Xi';

  _deviceKeyPress         = 0;
  _deviceKeyRelease       = 1;

  _deviceButtonPress      = 0;
  _deviceButtonRelease    = 1;

  _deviceMotionNotify     = 0;

  _deviceFocusIn          = 0;
  _deviceFocusOut         = 1;

  _proximityIn            = 0;
  _proximityOut           = 1;

  _deviceStateNotify      = 0;
  _deviceMappingNotify    = 1;
  _changeDeviceNotify     = 2;
(* Space of 3 between is necessary! Reserved for DeviceKeyStateNotify,
   DeviceButtonStateNotify, DevicePresenceNotify (essentially unused). This
   code has to be in sync with FixExtensionEvents() in xserver/Xi/extinit.c *)
  _propertyNotify         = 6;

{ We need the declaration for DevicePresence. }
function _XiGetDevicePresenceNotifyEvent(dpy: PDisplay): cint; cdecl; external libXi;
procedure _xibaddevice(dpy: PDisplay; error: Pcint); cdecl; external libXi;
procedure _xibadclass(dpy: PDisplay; error: Pcint); cdecl; external libXi;
procedure _xibadevent(dpy: PDisplay; error: Pcint); cdecl; external libXi;
procedure _xibadmode(dpy: PDisplay; error: Pcint); cdecl; external libXi;
procedure _xidevicebusy(dpy: PDisplay; error: Pcint); cdecl; external libXi;

procedure DevicePresence(dpy: PDisplay; out event_type: cint; event_class: TXEventClass); inline;

{ Errors }
procedure BadDevice(dpy: PDisplay; out error: cint); inline;

procedure BadClass(dpy: PDisplay; out error: cint); inline;

procedure BadEvent(dpy: PDisplay; out error: cint); inline;

procedure BadMode(dpy: PDisplay; out error: cint); inline;

procedure DeviceBusy(dpy: PDisplay; out error: cint); inline;

type

(***************************************************************
 *
 * DeviceKey events.  These events are sent by input devices that
 * support input class Keys.
 * The location of the X pointer is reported in the coordinate
 * fields of the x,y and x_root,y_root fields.
 *
 *)

  PXDeviceKeyEvent = ^TXDeviceKeyEvent;
  TXDeviceKeyEvent = record
    _type: cint;                 { of event }
    serial: culong;              { # of last request processed }
    send_event: TBool;           { true if from SendEvent request }
    display: PDisplay;           { Display the event was read from }
    window: TWindow;             { "event" window reported relative to }
    deviceid: TXID;
    root: TWindow;               { root window event occured on }
    subwindow: TWindow;          { child window }
    time: TTime;                 { milliseconds }
    x, y: cint;                  { x, y coordinates in event window }
    x_root: cint;                { coordinates relative to root }
    y_root: cint;                { coordinates relative to root }
    state: cuint;                { key or button mask }
    keycode: cuint;              { detail }
    same_screen: TBool;          { same screen flag }
    device_state: cuint;         { device key or button mask }
    axes_count: cuchar;
    first_axis: cuchar;
    axis_data: array [0..5] of cint;
  end;

  PXDeviceKeyPressedEvent = ^TXDeviceKeyPressedEvent;
  TXDeviceKeyPressedEvent = TXDeviceKeyEvent;
  PXDeviceKeyReleasedEvent = ^TXDeviceKeyReleasedEvent;
  TXDeviceKeyReleasedEvent = TXDeviceKeyEvent;

(*******************************************************************
 *
 * DeviceButton events.  These events are sent by extension devices
 * that support input class Buttons.
 *
 *)

  PXDeviceButtonEvent = ^TXDeviceButtonEvent;
  TXDeviceButtonEvent = record
    _type: cint;                { of event }
    serial: culong;             { # of last request processed by server }
    send_event: TBool;          { true if from a SendEvent request }
    display: PDisplay;          { Display the event was read from }
    window: TWindow;            { "event" window reported relative to }
    deviceid: TXID;
    root: TWindow;              { root window that the event occured on }
    subwindow: TWindow;         { child window }
    time: TTime;                { milliseconds }
    x, y: cint;                 { x, y coordinates in event window }
    x_root: cint;               { coordinates relative to root }
    y_root: cint;               { coordinates relative to root }
    state: cuint;               { key or button mask }
    button: cuint;              { detail }
    same_screen: TBool;         { same screen flag }
    device_state: cuint;        { device key or button mask }
    axes_count: cuchar;
    first_axis: cuchar;
    axis_data: array [0..5] of cint;
  end;

  PXDeviceButtonPressedEvent = ^TXDeviceButtonPressedEvent;
  TXDeviceButtonPressedEvent = TXDeviceButtonEvent;
  PXDeviceButtonReleasedEvent = ^TXDeviceButtonReleasedEvent;
  TXDeviceButtonReleasedEvent = TXDeviceButtonEvent;

(*******************************************************************
 *
 * DeviceMotionNotify event.  These events are sent by extension devices
 * that support input class Valuators.
 *
 *)

  PXDeviceMotionEvent = ^TXDeviceMotionEvent;
  TXDeviceMotionEvent = record
    _type: cint;               { of event }
    serial: culong;            { # of last request processed by server }
    send_event: TBool;         { true if from a SendEvent request }
    display: PDisplay;         { Display the event was read from }
    window: TWindow;           { "event" window reported relative to }
    deviceid: TXID;
    root: TWindow;             { root window that the event occured on }
    subwindow: TWindow;        { child window }
    time: TTime;               { milliseconds }
    x, y: cint;                { x, y coordinates in event window }
    x_root: cint;              { coordinates relative to root }
    y_root: cint;              { coordinates relative to root }
    state: cuint;              { key or button mask }
    is_hint: char;             { detail }
    same_screen: TBool;        { same screen flag }
    device_state: cuint;       { device key or button mask }
    axes_count: cuchar;
    first_axis: cuchar;
    axis_data: array [0..5] of cint;
  end;

(*******************************************************************
 *
 * DeviceFocusChange events.  These events are sent when the focus
 * of an extension device that can be focused is changed.
 *
 *)

  PXDeviceFocusChangeEvent = ^TXDeviceFocusChangeEvent;
  TXDeviceFocusChangeEvent = record
    _type: cint;              { of event }
    serial: culong;           { # of last request processed by server }
    send_event: TBool;        { true if from a SendEvent request }
    display: PDisplay;        { Display the event was read from }
    window: TWindow;          { "event" window reported relative to }
    deviceid: TXID;
    mode: cint;               { NotifyNormal, NotifyGrab, NotifyUngrab }
    detail: cint;
        (*
         * NotifyAncestor, NotifyVirtual, NotifyInferior,
         * NotifyNonLinear,NotifyNonLinearVirtual, NotifyPointer,
         * NotifyPointerRoot, NotifyDetailNone
         *)
    time: TTime;
  end;

  PXDeviceFocusInEvent = ^TXDeviceFocusInEvent;
  TXDeviceFocusInEvent = TXDeviceFocusChangeEvent;
  PXDeviceFocusOutEvent = ^TXDeviceFocusOutEvent;
  TXDeviceFocusOutEvent = TXDeviceFocusChangeEvent;

(*******************************************************************
 *
 * ProximityNotify events.  These events are sent by those absolute
 * positioning devices that are capable of generating proximity information.
 *
 *)

  PXProximityNotifyEvent = ^TXProximityNotifyEvent;
  TXProximityNotifyEvent = record
    _type: cint;               { ProximityIn or ProximityOut }
    serial: culong;            { # of last request processed by server }
    send_event: TBool;         { true if this came from a SendEvent request }
    display: PDisplay;         { Display the event was read from }
    window: TWindow;
    deviceid: TXID;
    root: TWindow;
    subwindow: TWindow;
    time: TTime;
    x, y: cint;
    x_root, y_root: cint;
    state: cuint;
    same_screen: TBool;
    device_state: cuint;       { device key or button mask }
    axes_count: cuchar;
    first_axis: cuchar;
    axis_data: array [0..5] of cint;
  end;
  PXProximityInEvent = ^TXProximityInEvent;
  TXProximityInEvent = TXProximityNotifyEvent;
  PXProximityOutEvent = ^TXProximityOutEvent;
  TXProximityOutEvent = TXProximityNotifyEvent;

(*******************************************************************
 *
 * DeviceStateNotify events are generated on EnterWindow and FocusIn 
 * for those clients who have selected DeviceState.
 *
 *)

  PXInputClass = ^TXInputClass;
  TXInputClass = record
    c_class: cuchar;
    length: cuchar;
  end;

  PXDeviceStateNotifyEvent = ^TXDeviceStateNotifyEvent;
  TXDeviceStateNotifyEvent = record
     _type: cint;
     serial: culong;             { # of last request processed by server }
     send_event: TBool;          { true if this came from a SendEvent request }
     display: PDisplay;          { Display the event was read from }
     window: TWindow;
     deviceid: TXID;
     time: TTime;
     num_classes: cint;
     data: array [0..63] of cchar;
  end;

  PXValuatorStatus = ^TXValuatorStatus;
  TXValuatorStatus = record
    c_class: cuchar;
    length: cuchar;
    num_valuators: cuchar;
    mode: cuchar;
    valuators: array [0..5] of cint;
  end;

  PXKeyStatus = ^TXKeyStatus;
  TXKeyStatus = record
    c_class: cuchar;
    length: cuchar;
    num_keys: cshort;
    keys: array [0..31] of cchar;
  end;

  PXButtonStatus = ^TXButtonStatus;
  TXButtonStatus = record
    c_class: cuchar;
    length: cuchar;
    num_buttons: cshort	;
    buttons: array [0..31] of cchar;
  end;

(*******************************************************************
 *
 * DeviceMappingNotify event.  This event is sent when the key mapping,
 * modifier mapping, or button mapping of an extension device is changed.
 *
 *)

  PXDeviceMappingEvent = ^TXDeviceMappingEvent;
  TXDeviceMappingEvent = record
    _type: cint;
    serial: culong;             { # of last request processed by server }
    send_event: TBool;          { true if this came from a SendEvent request }
    display: PDisplay;          { Display the event was read from }
    window: TWindow;            { unused }
    deviceid: TXID;
    time: TTime;
    request: cint;              { one of MappingModifier, MappingKeyboard,
                                   MappingPointer }
    first_keycode: cint;        { first keycode }
    count: cint;                { defines range of change w. first_keycode }
  end;

(*******************************************************************
 *
 * ChangeDeviceNotify event.  This event is sent when an 
 * XChangeKeyboard or XChangePointer request is made.
 *
 *)

  PXChangeDeviceNotifyEvent = ^TXChangeDeviceNotifyEvent;
  TXChangeDeviceNotifyEvent = record
    _type: cint;
    serial: culong;             { # of last request processed by server }
    send_event: TBool;          { true if this came from a SendEvent request }
    display: PDisplay;          { Display the event was read from }
    window: TWindow;            { unused }
    deviceid: TXID;
    time: TTime;
    request: cint;              { NewPointer or NewKeyboard }
  end;

(*******************************************************************
 *
 * DevicePresenceNotify event.  This event is sent when the list of
 * input devices changes, in which case devchange will be false, and
 * no information about the change will be contained in the event;
 * the client should use XListInputDevices() to learn what has changed.
 *
 * If devchange is true, an attribute that the server believes is
 * important has changed on a device, and the client should use
 * XGetDeviceControl to examine the device.  If control is non-zero,
 * then that control has changed meaningfully.
 *)

  PXDevicePresenceNotifyEvent = ^TXDevicePresenceNotifyEvent;
  TXDevicePresenceNotifyEvent = record
    _type: cint;
    serial: culong;             { # of last request processed by server }
    send_event: TBool;          { true if this came from a SendEvent request }
    display: PDisplay;          { Display the event was read from }
    window: TWindow;            { unused }
    time: TTime;
    devchange: TBool;
    deviceid: TXID;
    control: TXID;
  end;

(*
 * Notifies the client that a property on a device has changed value. The
 * client is expected to query the server for updated value of the property.
 *)
  PXDevicePropertyNotifyEvent = ^TXDevicePropertyNotifyEvent;
  TXDevicePropertyNotifyEvent = record
    _type: cint;
    serial: culong;             { # of last request processed by server }
    send_event: TBool;          { true if this came from a SendEvent request }
    display: PDisplay;          { Display the event was read from }
    window: TWindow;            { unused }
    time: TTime;
    deviceid: TXID;             { id of the device that changed }
    atom: TAtom;                { the property that changed }
    state: cint;                { PropertyNewValue or PropertyDeleted }
  end;


(*******************************************************************
 *
 * Control structures for input devices that support input class
 * Feedback.  These are used by the XGetFeedbackControl and 
 * XChangeFeedbackControl functions.
 *
 *)

  PXFeedbackState = ^TXFeedbackState;
  TXFeedbackState = record
    c_class: TXID;
    length: cint;
    id: TXID;
  end;

  PXKbdFeedbackState = ^TXKbdFeedbackState;
  TXKbdFeedbackState = record
    c_class: TXID;
    length: cint;
    id: TXID;
    click: cint;
    percent: cint;
    pitch: cint;
    duration: cint;
    led_mask: cint;
    global_auto_repeat: cint;
    auto_repeats: array [0..31] of cchar;
  end;

  PXPtrFeedbackState = ^TXPtrFeedbackState;
  TXPtrFeedbackState = record
    c_class: TXID;
    length: cint;
    id: TXID;
    accelNum: cint;
    accelDenom: cint;
    threshold: cint;
  end;

  PXIntegerFeedbackState = ^TXIntegerFeedbackState;
  TXIntegerFeedbackState = record
    c_class: TXID;
    length: cint;
    id: TXID;
    resolution: cint;
    minVal: cint;
    maxVal: cint;
  end;

  PXStringFeedbackState = ^TXStringFeedbackState;
  TXStringFeedbackState = record
    c_class: TXID;
    length: cint;
    id: TXID;
    max_symbols: cint;
    num_syms_supported: cint;
    syms_supported: PKeySym;
  end;

  PXBellFeedbackState = ^TXBellFeedbackState;
  TXBellFeedbackState = record
    c_class: TXID;
    length: cint;
    id: TXID;
    percent: cint;
    pitch: cint;
    duration: cint;
  end;

  PXLedFeedbackState = ^TXLedFeedbackState;
  TXLedFeedbackState = record
    c_class: TXID;
    length: cint;
    id: TXID;
    led_values: cint;
    led_mask: cint;
  end;

  PXFeedbackControl = ^TXFeedbackControl;
  TXFeedbackControl = record
    c_class: TXID;
    length: cint;
    id: TXID;
  end;

  PXPtrFeedbackControl = ^TXPtrFeedbackControl;
  TXPtrFeedbackControl = record
    c_class: TXID;
    length: cint;
    id: TXID;
    accelNum: cint;
    accelDenom: cint;
    threshold: cint;
  end;

  PXKbdFeedbackControl = ^TXKbdFeedbackControl;
  TXKbdFeedbackControl = record
    c_class: TXID;
    length: cint;
    id: TXID;
    click: cint;
    percent: cint;
    pitch: cint;
    duration: cint;
    led_mask: cint;
    led_value: cint;
    key: cint;
    auto_repeat_mode: cint;
  end;

  PXStringFeedbackControl = ^TXStringFeedbackControl;
  TXStringFeedbackControl = record
    c_class: TXID;
    length: cint;
    id: TXID;
    num_keysyms: cint;
    syms_to_display: PKeySym;
  end;

  PXIntegerFeedbackControl = ^TXIntegerFeedbackControl;
  TXIntegerFeedbackControl = record
    c_class: TXID;
    length: cint;
    id: TXID;
    int_to_display: cint;
  end;

  PXBellFeedbackControl = ^TXBellFeedbackControl;
  TXBellFeedbackControl = record
    c_class: TXID;
    length: cint;
    id: TXID;
    percent: cint;
    pitch: cint;
    duration: cint;
  end;

  PXLedFeedbackControl = ^TXLedFeedbackControl;
  TXLedFeedbackControl = record
    c_class: TXID;
    length: cint;
    id: TXID;
    led_mask: cint;
    led_values: cint;
  end;

(*******************************************************************
 *
 * Device control structures.
 *
 *)

  PXDeviceControl = ^TXDeviceControl;
  TXDeviceControl = record
    control: TXID;
    length: cint;
  end;

  PXDeviceResolutionControl = ^TXDeviceResolutionControl;
  TXDeviceResolutionControl = record
    control: TXID;
    length: cint;
    first_valuator: cint;
    num_valuators: cint;
    resolutions: Pcint;
  end;

  PXDeviceResolutionState = ^TXDeviceResolutionState;
  TXDeviceResolutionState = record
    control: TXID;
    length: cint;
    num_valuators: cint;
    resolutions: Pcint;
    min_resolutions: Pcint;
    max_resolutions: Pcint;
  end;

  PXDeviceAbsCalibControl = ^TXDeviceAbsCalibControl;
  TXDeviceAbsCalibControl = record
    control: TXID;
    length: cint;
    min_x: cint;
    max_x: cint;
    min_y: cint;
    max_y: cint;
    flip_x: cint;
    flip_y: cint;
    rotation: cint;
    button_threshold: cint;
  end;
  PXDeviceAbsCalibState = ^TXDeviceAbsCalibState;
  TXDeviceAbsCalibState = TXDeviceAbsCalibControl;

  PXDeviceAbsAreaControl = ^TXDeviceAbsAreaControl;
  TXDeviceAbsAreaControl = record
    control: TXID;
    length: cint;
    offset_x: cint;
    offset_y: cint;
    width: cint;
    height: cint;
    screen: cint;
    following: TXID;
  end;
  PXDeviceAbsAreaState = ^TXDeviceAbsAreaState;
  TXDeviceAbsAreaState = TXDeviceAbsAreaControl;

  PXDeviceCoreControl = ^TXDeviceCoreControl;
  TXDeviceCoreControl = record
    control: TXID;
    length: cint;
    status: cint;
  end;

  PXDeviceCoreState = ^TXDeviceCoreState;
  TXDeviceCoreState = record
    control: TXID;
    length: cint;
    status: cint;
    iscore: cint;
  end;

  PXDeviceEnableControl = ^TXDeviceEnableControl;
  TXDeviceEnableControl = record
    control: TXID;
    length: cint;
    enable: cint;
  end;
  PXDeviceEnableState = ^TXDeviceEnableState;
  TXDeviceEnableState = TXDeviceEnableControl;

(*******************************************************************
 *
 * An array of XDeviceList structures is returned by the 
 * XListInputDevices function.  Each entry contains information
 * about one input device.  Among that information is an array of 
 * pointers to structures that describe the characteristics of 
 * the input device.
 *
 *)

  PXAnyClassInfo = ^TXAnyClassInfo;
  TXAnyClassPtr = PXAnyClassInfo;
  TXAnyClassInfo = record
    c_classs: TXID;
    length: cint;
  end;

  PXDeviceInfo = ^TXDeviceInfo;
  TXDeviceInfoPtr = PXDeviceInfo;
  TXDeviceInfo = record
    id: TXID;
    _type: TAtom;
    name: PChar;
    num_classes: cint;
    use: cint;
    inputclassinfo: TXAnyClassPtr;
  end;

  PXKeyInfo = ^TXKeyInfo;
  TXKeyInfoPtr = PXKeyInfo;
  TXKeyInfo = record
    c_class: TXID;
    length: cint;
    min_keycode: cushort;
    max_keycode: cushort;
    num_keys: cushort;
  end;

  PXButtonInfo = ^TXButtonInfo;
  TXButtonInfoPtr = PXButtonInfo;
  TXButtonInfo = record
    c_class: TXID;
    length: cint;
    num_buttons: cshort;
  end;

  PXAxisInfo = ^TXAxisInfo;
  TXAxisInfoPtr = PXAxisInfo;
  TXAxisInfo = record
    resolution: cint;
    min_value: cint;
    max_value: cint;
  end;

  PXValuatorInfo = ^TXValuatorInfo;
  TXValuatorInfoPtr = PXValuatorInfo;
  TXValuatorInfo = record
    c_class: TXID;
    length: cint;
    num_axes: cuchar;
    mode: cuchar;
    motion_buffer: culong;
    axes: TXAxisInfoPtr;
  end;

(*******************************************************************
 *
 * An XDevice structure is returned by the XOpenDevice function.  
 * It contains an array of pointers to XInputClassInfo structures.
 * Each contains information about a class of input supported by the
 * device, including a pointer to an array of data for each type of event
 * the device reports.
 *
 *)


  PXInputClassInfo = ^TXInputClassInfo;
  TXInputClassInfo = record
    input_class: cuchar;
    event_type_base: cuchar;
  end;

  PXDevice = ^TXDevice;
  TXDevice = record
    device_id: TXID;
    num_classes: cint;
    classes: PXInputClassInfo;
  end;


(*******************************************************************
 *
 * The following structure is used to return information for the 
 * XGetSelectedExtensionEvents function.
 *
 *)

  PXEventList = ^TXEventList;
  TXEventList = record
    event_type: TXEventClass;
    device: TXID;
  end;

(*******************************************************************
 *
 * The following structure is used to return motion history data from 
 * an input device that supports the input class Valuators.
 * This information is returned by the XGetDeviceMotionEvents function.
 *
 *)

  PXDeviceTimeCoord = ^TXDeviceTimeCoord;
  TXDeviceTimeCoord = record
    time: TTime;
    data: Pcint;
  end;


(*******************************************************************
 *
 * Device state structure.
 * This is returned by the XQueryDeviceState request.
 *
 *)

  PXDeviceState = ^TXDeviceState;
  TXDeviceState = record
    device_id: TXID;
    num_classes: cint;
    data: PXInputClass;
  end;

(*******************************************************************
 *
 * Note that the mode field is a bitfield that reports the Proximity
 * status of the device as well as the mode.  The mode field should
 * be OR'd with the mask DeviceMode and compared with the values
 * Absolute and Relative to determine the mode, and should be OR'd
 * with the mask ProximityState and compared with the values InProximity
 * and OutOfProximity to determine the proximity state.
 *
 *)

  PXValuatorState = ^TXValuatorState;
  TXValuatorState = record
    c_class: cuchar;
    length: cuchar;
    num_valuators: cuchar;
    mode: cuchar;
    valuators: Pcint;
  end;

  PXKeyState = ^TXKeyState;
  TXKeyState = record
    c_class: cuchar;
    length: cuchar;
    num_keys: cshort;
    keys: array [0..31] of cchar;
  end;

  PXButtonState = ^TXButtonState;
  TXButtonState = record
    c_class: cuchar;
    length: cuchar;
    num_buttons: cshort;
    buttons: array [0..31] of cchar;
  end;

procedure FindTypeAndClass(d: PXDevice; out event_type: cint; out event_class: TXEventClass; classid: cuchar; offset: cint); inline;
procedure DeviceKeyPress(d: PXDevice; out event_type: cint; out event_class: TXEventClass); inline;
procedure DeviceKeyRelease(d: PXDevice; out event_type: cint; out event_class: TXEventClass); inline;
procedure DeviceButtonPress(d: PXDevice; out event_type: cint; out event_class: TXEventClass); inline;
procedure DeviceButtonRelease(d: PXDevice; out event_type: cint; out event_class: TXEventClass); inline;
procedure DeviceMotionNotify(d: PXDevice; out event_type: cint; out event_class: TXEventClass); inline;
procedure DeviceFocusIn(d: PXDevice; out event_type: cint; out event_class: TXEventClass); inline;
procedure DeviceFocusOut(d: PXDevice; out event_type: cint; out event_class: TXEventClass); inline;
procedure ProximityIn(d: PXDevice; out event_type: cint; out event_class: TXEventClass); inline;
procedure ProximityOut(d: PXDevice; out event_type: cint; out event_class: TXEventClass); inline;
procedure DeviceStateNotify(d: PXDevice; out event_type: cint; out event_class: TXEventClass); inline;
procedure DeviceMappingNotify(d: PXDevice; out event_type: cint; out event_class: TXEventClass); inline;
procedure ChangeDeviceNotify(d: PXDevice; out event_type: cint; out event_class: TXEventClass); inline;
procedure DevicePropertyNotify(d: PXDevice; out event_type: cint; out event_class: TXEventClass); inline;
procedure DevicePointerMotionHint(d: PXDevice; event_type: cint; out event_class: TXEventClass); inline;
procedure DeviceButton1Motion(d: PXDevice; event_type: cint; out event_class: TXEventClass); inline;
procedure DeviceButton2Motion(d: PXDevice; event_type: cint; out event_class: TXEventClass); inline;
procedure DeviceButton3Motion(d: PXDevice; event_type: cint; out event_class: TXEventClass); inline;
procedure DeviceButton4Motion(d: PXDevice; event_type: cint; out event_class: TXEventClass); inline;
procedure DeviceButton5Motion(d: PXDevice; event_type: cint; out event_class: TXEventClass); inline;
procedure DeviceButtonMotion(d: PXDevice; event_type: cint; out event_class: TXEventClass); inline;
procedure DeviceOwnerGrabButton(d: PXDevice; event_type: cint; out event_class: TXEventClass); inline;
procedure DeviceButtonPressGrab(d: PXDevice; event_type: cint; out event_class: TXEventClass); inline;
procedure NoExtensionEvent(d: PXDevice; event_type: cint; out event_class: TXEventClass); inline;




(*******************************************************************
 *
 * Function definitions.
 *
 *)

//_XFUNCPROTOBEGIN

function XChangeKeyboardDevice(
    display: PDisplay;
    device: PXDevice
): cint; cdecl; external libXi;

function XChangePointerDevice(
    display: PDisplay;
    device: PXDevice;
    xaxis: cint;
    yaxis: cint
): cint; cdecl; external libXi;

function XGrabDevice(
    display: PDisplay;
    device: PXDevice;
    grab_window: TWindow;
    ownerEvents: TBool;
    event_count: cint;
    event_list: PXEventClass;
    this_device_mode: cint;
    other_devices_mode: cint;
    time: TTime
): cint; cdecl; external libXi;

{ boolean overload for the TBool parameter }
function XGrabDevice(
    display: PDisplay;
    device: PXDevice;
    grab_window: TWindow;
    ownerEvents: Boolean;
    event_count: cint;
    event_list: PXEventClass;
    this_device_mode: cint;
    other_devices_mode: cint;
    time: TTime
): cint; inline;

function XUngrabDevice(
    display: PDisplay;
    device: PXDevice;
    time: TTime
): cint; cdecl; external libXi;

function XGrabDeviceKey(
    display: PDisplay;
    device: PXDevice;
    key: cuint;
    modifiers: cuint;
    modifier_device: PXDevice;
    grab_window: TWindow;
    owner_events: TBool;
    event_count: cuint;
    event_list: PXEventClass;
    this_device_mode: cint;
    other_devices_mode: cint
): cint; cdecl; external libXi;

{ boolean overload for the TBool parameter }
function XGrabDeviceKey(
    display: PDisplay;
    device: PXDevice;
    key: cuint;
    modifiers: cuint;
    modifier_device: PXDevice;
    grab_window: TWindow;
    owner_events: Boolean;
    event_count: cuint;
    event_list: PXEventClass;
    this_device_mode: cint;
    other_devices_mode: cint
): cint; inline;

function XUngrabDeviceKey(
    display: PDisplay;
    device: PXDevice;
    key: cuint;
    modifiers: cuint;
    modifier_dev: PXDevice;
    grab_window: TWindow
): cint; cdecl; external libXi;

function XGrabDeviceButton(
    display: PDisplay;
    device: PXDevice;
    button: cuint;
    modifiers: cuint;
    modifier_device: PXDevice;
    grab_window: TWindow;
    owner_events: TBool;
    event_count: cuint;
    event_list: PXEventClass;
    this_device_mode: cint;
    other_devices_mode: cint
): cint; cdecl; external libXi;

{ boolean overload for the TBool parameter }
function XGrabDeviceButton(
    display: PDisplay;
    device: PXDevice;
    button: cuint;
    modifiers: cuint;
    modifier_device: PXDevice;
    grab_window: TWindow;
    owner_events: Boolean;
    event_count: cuint;
    event_list: PXEventClass;
    this_device_mode: cint;
    other_devices_mode: cint
): cint; inline;

function XUngrabDeviceButton(
    display: PDisplay;
    device: PXDevice;
    button: cuint;
    modifiers: cuint;
    modifier_dev: PXDevice;
    grab_window: TWindow
): cint; cdecl; external libXi;

function XAllowDeviceEvents(
    display: PDisplay;
    device: PXDevice;
    event_mode: cint;
    time: TTime
): cint; cdecl; external libXi;

function XGetDeviceFocus(
    display: PDisplay;
    device: PXDevice;
    focus: PWindow;
    revert_to: Pcint;
    time: PTime
): cint; cdecl; external libXi;

function XSetDeviceFocus(
    display: PDisplay;
    device: PXDevice;
    focus: TWindow;
    revert_to: cint;
    time: TTime
): cint; cdecl; external libXi;

function XGetFeedbackControl(
    display: PDisplay;
    device: PXDevice;
    num_feedbacks: Pcint
): PXFeedbackState; cdecl; external libXi;

procedure XFreeFeedbackList(
    list: PXFeedbackState
); cdecl; external libXi;

function XChangeFeedbackControl(
    display: PDisplay;
    device: PXDevice;
    mask: culong;
    f: PXFeedbackControl
): cint; cdecl; external libXi;

function XDeviceBell(
    display: PDisplay;
    device: PXDevice;
    feedbackclass: TXID;
    feedbackid: TXID;
    percent: cint
): cint; cdecl; external libXi;

function XGetDeviceKeyMapping(
    display: PDisplay;
    device: PXDevice;
{$ifdef NeedWidePrototypes}
    first: cuint;
{$else}
    first: TKeyCode;
{$endif}
    keycount: cint;
    syms_per_code: Pcint
): PKeySym; cdecl; external libXi;

function XChangeDeviceKeyMapping(
    display: PDisplay;
    device: PXDevice;
    first: cint;
    syms_per_code: cint;
    keysyms: PKeySym;
    count: cint
): cint; cdecl; external libXi;

function XGetDeviceModifierMapping(
    display: PDisplay;
    device: PXDevice
): PXModifierKeymap; cdecl; external libXi;

function XSetDeviceModifierMapping(
    display: PDisplay;
    device: PXDevice;
    modmap: PXModifierKeymap
): cint; cdecl; external libXi;

function XSetDeviceButtonMapping(
    display: PDisplay;
    device: PXDevice;
    map: Pcuchar;
    nmap: cint
): cint; cdecl; external libXi;

function XGetDeviceButtonMapping(
    display: PDisplay;
    device: PXDevice;
    map: Pcuchar;
    nmap: cuint
): cint; cdecl; external libXi;

function XQueryDeviceState(
    display: PDisplay;
    device: PXDevice
): PXDeviceState; cdecl; external libXi;

procedure XFreeDeviceState(
    list: PXDeviceState
); cdecl; external libXi;

function XGetExtensionVersion(
    display: PDisplay;
    name: {_Xconst} Pchar
): PXExtensionVersion; cdecl; external libXi;

function XListInputDevices(
    display: PDisplay;
    ndevices: Pcint
): PXDeviceInfo; cdecl; external libXi;

procedure XFreeDeviceList(
    list: PXDeviceInfo
); cdecl; external libXi;

function XOpenDevice(
    display: PDisplay;
    id: TXID
): PXDevice; cdecl; external libXi;

function XCloseDevice(
    display: PDisplay;
    device: PXDevice
): cint; cdecl; external libXi;

function XSetDeviceMode(
    display: PDisplay;
    device: PXDevice;
    mode: cint
): cint; cdecl; external libXi;

function XSetDeviceValuators(
    display: PDisplay;
    device: PXDevice;
    valuators: Pcint;
    first_valuator: cint;
    num_valuators: cint
): cint; cdecl; external libXi;

function XGetDeviceControl(
    display: PDisplay;
    device: PXDevice;
    control: cint
): PXDeviceControl; cdecl; external libXi;

function XChangeDeviceControl(
    display: PDisplay;
    device: PXDevice;
    control: cint;
    d: PXDeviceControl
): cint; cdecl; external libXi;

function XSelectExtensionEvent(
    display: PDisplay;
    w: TWindow;
    event_list: PXEventClass;
    count: cint
): cint; cdecl; external libXi;

function XGetSelectedExtensionEvents(
    display: PDisplay;
    w: TWindow;
    this_client_count: Pcint;
    this_client_list: PPXEventClass;
    all_clients_count: Pcint;
    all_clients_list: PPXEventClass
): cint; cdecl; external libXi;

function XChangeDeviceDontPropagateList(
    display: PDisplay;
    window: TWindow;
    count: cint;
    events: PXEventClass;
    mode: cint
): cint; cdecl; external libXi;

function XGetDeviceDontPropagateList(
    display: PDisplay;
    window: TWindow;
    count: Pcint
): PXEventClass; cdecl; external libXi;

function XSendExtensionEvent(
    display: PDisplay;
    device: PXDevice;
    dest: TWindow;
    prop: TBool;
    count: cint;
    list: PXEventClass;
    event: PXEvent
): TStatus; cdecl; external libXi;

{ boolean overload for the TBool parameter }
function XSendExtensionEvent(
    display: PDisplay;
    device: PXDevice;
    dest: TWindow;
    prop: Boolean;
    count: cint;
    list: PXEventClass;
    event: PXEvent
): TStatus; inline;

function XGetDeviceMotionEvents(
    display: PDisplay;
    device: PXDevice;
    start: TTime;
    stop: TTime;
    nEvents: Pcint;
    mode: Pcint;
    axis_count: Pcint
): PXDeviceTimeCoord; cdecl; external libXi;

procedure XFreeDeviceMotionEvents(
    events: PXDeviceTimeCoord
); cdecl; external libXi;

procedure XFreeDeviceControl(
    control: PXDeviceControl
); cdecl; external libXi;

function XListDeviceProperties(
    dpy: PDisplay;
    dev: PXDevice;
    nprops_return: Pcint
): PAtom; cdecl; external libXi;

procedure XChangeDeviceProperty(
    dpy: PDisplay;
    dev: PXDevice;
    _property: TAtom;
    _type: TAtom;
    format: cint;
    mode: cint;
    data: {_Xconst} Pcuchar;
    nelements: cint
); cdecl; external libXi;

procedure XDeleteDeviceProperty(
    dpy: PDisplay;
    dev: PXDevice;
    _property: TAtom
); cdecl; external libXi;

function XGetDeviceProperty(
     dpy: PDisplay;
     dev: PXDevice;
     _property: TAtom;
     offset: clong;
     length: clong;
     delete: TBool;
     req_type: TAtom;
     actual_type: PAtom;
     actual_format: Pcint;
     nitems: Pculong;
     bytes_after: Pculong;
     prop: PPcuchar
): TStatus; cdecl; external libXi;

{ boolean overload for the TBool parameter }
function XGetDeviceProperty(
     dpy: PDisplay;
     dev: PXDevice;
     _property: TAtom;
     offset: clong;
     length: clong;
     delete: Boolean;
     req_type: TAtom;
     actual_type: PAtom;
     actual_format: Pcint;
     nitems: Pculong;
     bytes_after: Pculong;
     prop: PPcuchar
): TStatus; inline;

//_XFUNCPROTOEND

implementation

procedure DevicePresence(dpy: PDisplay; out event_type: cint; event_class: TXEventClass); inline;
begin
  event_type := _XiGetDevicePresenceNotifyEvent(dpy);
  event_class := $10000 or _devicePresence;
end;

procedure BadDevice(dpy: PDisplay; out error: cint); inline;
begin
  _xibaddevice(dpy, @error);
end;

procedure BadClass(dpy: PDisplay; out error: cint); inline;
begin
  _xibadclass(dpy, @error);
end;

procedure BadEvent(dpy: PDisplay; out error: cint); inline;
begin
  _xibadevent(dpy, @error);
end;

procedure BadMode(dpy: PDisplay; out error: cint); inline;
begin
  _xibadmode(dpy, @error);
end;

procedure DeviceBusy(dpy: PDisplay; out error: cint); inline;
begin
  _xidevicebusy(dpy, @error);
end;

procedure FindTypeAndClass(d: PXDevice; out event_type: cint; out event_class: TXEventClass; classid: cuchar; offset: cint); inline;
var
  _i : cint = 0;
  _ip: PXInputClassInfo;
begin
  event_type := 0;
  event_class := 0;

  _ip := d^.classes;
  for _i := 0 to d^.num_classes - 1 do
  begin
    if _ip^.input_class = classid then
    begin
      event_type := _ip^.event_type_base + offset;
      event_class := (d^.device_id shl 8) or event_type;
    end;
    Inc(_ip);
  end;
end;

procedure DeviceKeyPress(d: PXDevice; out event_type: cint; out event_class: TXEventClass); inline;
begin
  FindTypeAndClass(d, event_type, event_class, KeyClass, _deviceKeyPress);
end;

procedure DeviceKeyRelease(d: PXDevice; out event_type: cint; out event_class: TXEventClass); inline;
begin
  FindTypeAndClass(d, event_type, event_class, KeyClass, _deviceKeyRelease);
end;

procedure DeviceButtonPress(d: PXDevice; out event_type: cint; out event_class: TXEventClass); inline;
begin
  FindTypeAndClass(d, event_type, event_class, ButtonClass, _deviceButtonPress);
end;

procedure DeviceButtonRelease(d: PXDevice; out event_type: cint; out event_class: TXEventClass); inline;
begin
  FindTypeAndClass(d, event_type, event_class, ButtonClass, _deviceButtonRelease);
end;

procedure DeviceMotionNotify(d: PXDevice; out event_type: cint; out event_class: TXEventClass); inline;
begin
  FindTypeAndClass(d, event_type, event_class, ValuatorClass, _deviceMotionNotify);
end;

procedure DeviceFocusIn(d: PXDevice; out event_type: cint; out event_class: TXEventClass); inline;
begin
  FindTypeAndClass(d, event_type, event_class, FocusClass, _deviceFocusIn);
end;

procedure DeviceFocusOut(d: PXDevice; out event_type: cint; out event_class: TXEventClass); inline;
begin
  FindTypeAndClass(d, event_type, event_class, FocusClass, _deviceFocusOut);
end;

procedure ProximityIn(d: PXDevice; out event_type: cint; out event_class: TXEventClass); inline;
begin
  FindTypeAndClass(d, event_type, event_class, ProximityClass, _proximityIn);
end;

procedure ProximityOut(d: PXDevice; out event_type: cint; out event_class: TXEventClass); inline;
begin
  FindTypeAndClass(d, event_type, event_class, ProximityClass, _proximityOut);
end;

procedure DeviceStateNotify(d: PXDevice; out event_type: cint; out event_class: TXEventClass); inline;
begin
  FindTypeAndClass(d, event_type, event_class, OtherClass, _deviceStateNotify);
end;

procedure DeviceMappingNotify(d: PXDevice; out event_type: cint; out event_class: TXEventClass); inline;
begin
  FindTypeAndClass(d, event_type, event_class, OtherClass, _deviceMappingNotify);
end;

procedure ChangeDeviceNotify(d: PXDevice; out event_type: cint; out event_class: TXEventClass); inline;
begin
  FindTypeAndClass(d, event_type, event_class, OtherClass, _changeDeviceNotify);
end;

procedure DevicePropertyNotify(d: PXDevice; out event_type: cint; out event_class: TXEventClass); inline;
begin
  FindTypeAndClass(d, event_type, event_class, OtherClass, _propertyNotify);
end;

procedure DevicePointerMotionHint(d: PXDevice; event_type: cint; out event_class: TXEventClass); inline;
begin
  event_class := (d^.device_id shl 8) or _devicePointerMotionHint;
end;

procedure DeviceButton1Motion(d: PXDevice; event_type: cint; out event_class: TXEventClass); inline;
begin
  event_class := (d^.device_id shl 8) or _deviceButton1Motion;
end;

procedure DeviceButton2Motion(d: PXDevice; event_type: cint; out event_class: TXEventClass); inline;
begin
  event_class := (d^.device_id shl 8) or _deviceButton2Motion;
end;

procedure DeviceButton3Motion(d: PXDevice; event_type: cint; out event_class: TXEventClass); inline;
begin
  event_class := (d^.device_id shl 8) or _deviceButton3Motion;
end;

procedure DeviceButton4Motion(d: PXDevice; event_type: cint; out event_class: TXEventClass); inline;
begin
  event_class := (d^.device_id shl 8) or _deviceButton4Motion;
end;

procedure DeviceButton5Motion(d: PXDevice; event_type: cint; out event_class: TXEventClass); inline;
begin
  event_class := (d^.device_id shl 8) or _deviceButton5Motion;
end;

procedure DeviceButtonMotion(d: PXDevice; event_type: cint; out event_class: TXEventClass); inline;
begin
  event_class := (d^.device_id shl 8) or _deviceButtonMotion;
end;

procedure DeviceOwnerGrabButton(d: PXDevice; event_type: cint; out event_class: TXEventClass); inline;
begin
  event_class := (d^.device_id shl 8) or _deviceOwnerGrabButton;
end;

procedure DeviceButtonPressGrab(d: PXDevice; event_type: cint; out event_class: TXEventClass); inline;
begin
  event_class := (d^.device_id shl 8) or _deviceButtonGrab;
end;

procedure NoExtensionEvent(d: PXDevice; event_type: cint; out event_class: TXEventClass); inline;
begin
  event_class := (d^.device_id shl 8) or _noExtensionEvent;
end;

function XGrabDevice(display: PDisplay; device: PXDevice; grab_window: TWindow;
  ownerEvents: Boolean; event_count: cint; event_list: PXEventClass;
  this_device_mode: cint; other_devices_mode: cint; time: TTime): cint; inline;
begin
  XGrabDevice := XGrabDevice(display, device, grab_window, Ord(ownerEvents), event_count, event_list, this_device_mode,
    other_devices_mode, time);
end;

function XGrabDeviceKey(display: PDisplay; device: PXDevice; key: cuint;
  modifiers: cuint; modifier_device: PXDevice; grab_window: TWindow;
  owner_events: Boolean; event_count: cuint; event_list: PXEventClass;
  this_device_mode: cint; other_devices_mode: cint): cint; inline;
begin
  XGrabDeviceKey := XGrabDeviceKey(display, device, key, modifiers, modifier_device, grab_window,
    Ord(owner_events), event_count, event_list, this_device_mode, other_devices_mode);
end;

function XGrabDeviceButton(display: PDisplay; device: PXDevice; button: cuint;
  modifiers: cuint; modifier_device: PXDevice; grab_window: TWindow;
  owner_events: Boolean; event_count: cuint; event_list: PXEventClass;
  this_device_mode: cint; other_devices_mode: cint): cint; inline;
begin
  XGrabDeviceButton := XGrabDeviceButton(display, device, button, modifiers, modifier_device, grab_window, Ord(owner_events),
    event_count, event_list, this_device_mode, other_devices_mode);
end;

function XSendExtensionEvent(display: PDisplay; device: PXDevice;
  dest: TWindow; prop: Boolean; count: cint; list: PXEventClass; event: PXEvent
  ): TStatus; inline;
begin
  XSendExtensionEvent := XSendExtensionEvent(display, device, dest, Ord(prop), count, list, event);
end;

function XGetDeviceProperty(dpy: PDisplay; dev: PXDevice; _property: TAtom;
  offset: clong; length: clong; delete: Boolean; req_type: TAtom;
  actual_type: PAtom; actual_format: Pcint; nitems: Pculong;
  bytes_after: Pculong; prop: PPcuchar): TStatus; inline;
begin
  XGetDeviceProperty := XGetDeviceProperty(dpy, dev, _property, offset, length, Ord(delete), req_type, actual_type,
    actual_format, nitems, bytes_after, prop);
end;

end.

