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
unit xi2;

{$PACKRECORDS C}

interface

uses
  ctypes;

const
  XInput_2_0                              = 7;
(* DO NOT ADD TO THIS LIST. These are libXi-specific defines.
   See commit libXi-1.4.2-21-ge8531dd *)

  XI_2_Major                              = 2;
  XI_2_Minor                              = 3;

{ Property event flags }
  XIPropertyDeleted                       = 0;
  XIPropertyCreated                       = 1;
  XIPropertyModified                      = 2;

{ Property modes }
  XIPropModeReplace                       = 0;
  XIPropModePrepend                       = 1;
  XIPropModeAppend                        = 2;

{ Special property type used for XIGetProperty }
  XIAnyPropertyType                       = 0;

{ Enter/Leave and Focus In/Out modes }
  XINotifyNormal                          = 0;
  XINotifyGrab                            = 1;
  XINotifyUngrab                          = 2;
  XINotifyWhileGrabbed                    = 3;
  XINotifyPassiveGrab                     = 4;
  XINotifyPassiveUngrab                   = 5;

{ Enter/Leave and focus In/out detail }
  XINotifyAncestor                        = 0;
  XINotifyVirtual                         = 1;
  XINotifyInferior                        = 2;
  XINotifyNonlinear                       = 3;
  XINotifyNonlinearVirtual                = 4;
  XINotifyPointer                         = 5;
  XINotifyPointerRoot                     = 6;
  XINotifyDetailNone                      = 7;

{ Grab modes }
  XIGrabModeSync                          = 0;
  XIGrabModeAsync                         = 1;
  XIGrabModeTouch                         = 2;

{ Grab reply status codes }
  XIGrabSuccess                           = 0;
  XIAlreadyGrabbed                        = 1;
  XIGrabInvalidTime                       = 2;
  XIGrabNotViewable                       = 3;
  XIGrabFrozen                            = 4;

{ Grab owner events values }
  XIOwnerEvents                           = True;
  XINoOwnerEvents                         = False;

{ Passive grab types }
  XIGrabtypeButton                        = 0;
  XIGrabtypeKeycode                       = 1;
  XIGrabtypeEnter                         = 2;
  XIGrabtypeFocusIn                       = 3;
  XIGrabtypeTouchBegin                    = 4;

{ Passive grab modifier }
  XIAnyModifier                           = LongWord(1) shl 31;
  XIAnyButton                             = 0;
  XIAnyKeycode                            = 0;

{ XIAllowEvents event-modes }
  XIAsyncDevice                           = 0;
  XISyncDevice                            = 1;
  XIReplayDevice                          = 2;
  XIAsyncPairedDevice                     = 3;
  XIAsyncPair                             = 4;
  XISyncPair                              = 5;
  XIAcceptTouch                           = 6;
  XIRejectTouch                           = 7;

{ DeviceChangedEvent change reasons }
  XISlaveSwitch                           = 1;
  XIDeviceChange                          = 2;

{ Hierarchy flags }
  XIMasterAdded                           = 1 shl 0;
  XIMasterRemoved                         = 1 shl 1;
  XISlaveAdded                            = 1 shl 2;
  XISlaveRemoved                          = 1 shl 3;
  XISlaveAttached                         = 1 shl 4;
  XISlaveDetached                         = 1 shl 5;
  XIDeviceEnabled                         = 1 shl 6;
  XIDeviceDisabled                        = 1 shl 7;

{ ChangeHierarchy constants }
  XIAddMaster                             = 1;
  XIRemoveMaster                          = 2;
  XIAttachSlave                           = 3;
  XIDetachSlave                           = 4;

  XIAttachToMaster                        = 1;
  XIFloating                              = 2;

{ Valuator modes }
  XIModeRelative                          = 0;
  XIModeAbsolute                          = 1;

{ Device types }
  XIMasterPointer                         = 1;
  XIMasterKeyboard                        = 2;
  XISlavePointer                          = 3;
  XISlaveKeyboard                         = 4;
  XIFloatingSlave                         = 5;

(* Device classes: classes that are not identical to Xi 1.x classes must be
 * numbered starting from 8. *)
  XIKeyClass                              = 0;
  XIButtonClass                           = 1;
  XIValuatorClass                         = 2;
  XIScrollClass                           = 3;
  XITouchClass                            = 8;

{ Scroll class types }
  XIScrollTypeVertical                    = 1;
  XIScrollTypeHorizontal                  = 2;

{ Scroll class flags }
  XIScrollFlagNoEmulation                 = 1 shl 0;
  XIScrollFlagPreferred                   = 1 shl 1;

{ Device event flags (common) }
{ Device event flags (key events only) }
  XIKeyRepeat                             = 1 shl 16;
{ Device event flags (pointer events only) }
  XIPointerEmulated                       = 1 shl 16;
{ Device event flags (touch events only) }
  XITouchPendingEnd                       = 1 shl 16;
  XITouchEmulatingPointer                 = 1 shl 17;

{ Barrier event flags }
  XIBarrierPointerReleased                = 1 shl 0;
  XIBarrierDeviceIsGrabbed                = 1 shl 1;


{ Touch modes }
  XIDirectTouch                           = 1;
  XIDependentTouch                        = 2;

{ XI2 event mask macros }
procedure XISetMask(ptr: Pcuchar; event: cint); inline;
procedure XIClearMask(ptr: Pcuchar; event: cint); inline;
function XIMaskIsSet(ptr: Pcuchar; event: cint): Boolean; inline;
function XIMaskLen(event: cint): cint; inline;

const
{ Fake device ID's for event selection }
  XIAllDevices                            = 0;
  XIAllMasterDevices                      = 1;

{ Event types }
  XI_DeviceChanged                 = 1;
  XI_KeyPress                      = 2;
  XI_KeyRelease                    = 3;
  XI_ButtonPress                   = 4;
  XI_ButtonRelease                 = 5;
  XI_Motion                        = 6;
  XI_Enter                         = 7;
  XI_Leave                         = 8;
  XI_FocusIn                       = 9;
  XI_FocusOut                      = 10;
  XI_HierarchyChanged              = 11;
  XI_PropertyEvent                 = 12;
  XI_RawKeyPress                   = 13;
  XI_RawKeyRelease                 = 14;
  XI_RawButtonPress                = 15;
  XI_RawButtonRelease              = 16;
  XI_RawMotion                     = 17;
  XI_TouchBegin                    = 18; { XI 2.2 }
  XI_TouchUpdate                   = 19;
  XI_TouchEnd                      = 20;
  XI_TouchOwnership                = 21;
  XI_RawTouchBegin                 = 22;
  XI_RawTouchUpdate                = 23;
  XI_RawTouchEnd                   = 24;
  XI_BarrierHit                    = 25; { XI 2.3 }
  XI_BarrierLeave                  = 26;
  XI_LASTEVENT                     = XI_BarrierLeave;
(* NOTE: XI2LASTEVENT in xserver/include/inputstr.h must be the same value
 * as XI_LASTEVENT if the server is supposed to handle masks etc. for this
 * type of event. *)

(* Event masks.
 * Note: the protocol spec defines a mask to be of (1 << type). Clients are
 * free to create masks by bitshifting instead of using these defines.
 *)
  XI_DeviceChangedMask             = 1 shl XI_DeviceChanged;
  XI_KeyPressMask                  = 1 shl XI_KeyPress;
  XI_KeyReleaseMask                = 1 shl XI_KeyRelease;
  XI_ButtonPressMask               = 1 shl XI_ButtonPress;
  XI_ButtonReleaseMask             = 1 shl XI_ButtonRelease;
  XI_MotionMask                    = 1 shl XI_Motion;
  XI_EnterMask                     = 1 shl XI_Enter;
  XI_LeaveMask                     = 1 shl XI_Leave;
  XI_FocusInMask                   = 1 shl XI_FocusIn;
  XI_FocusOutMask                  = 1 shl XI_FocusOut;
  XI_HierarchyChangedMask          = 1 shl XI_HierarchyChanged;
  XI_PropertyEventMask             = 1 shl XI_PropertyEvent;
  XI_RawKeyPressMask               = 1 shl XI_RawKeyPress;
  XI_RawKeyReleaseMask             = 1 shl XI_RawKeyRelease;
  XI_RawButtonPressMask            = 1 shl XI_RawButtonPress;
  XI_RawButtonReleaseMask          = 1 shl XI_RawButtonRelease;
  XI_RawMotionMask                 = 1 shl XI_RawMotion;
  XI_TouchBeginMask                = 1 shl XI_TouchBegin;
  XI_TouchEndMask                  = 1 shl XI_TouchEnd;
  XI_TouchOwnershipChangedMask     = 1 shl XI_TouchOwnership;
  XI_TouchUpdateMask               = 1 shl XI_TouchUpdate;
  XI_RawTouchBeginMask             = 1 shl XI_RawTouchBegin;
  XI_RawTouchEndMask               = 1 shl XI_RawTouchEnd;
  XI_RawTouchUpdateMask            = 1 shl XI_RawTouchUpdate;
  XI_BarrierHitMask                = 1 shl XI_BarrierHit;
  XI_BarrierLeaveMask              = 1 shl XI_BarrierLeave;

implementation

{ XI2 event mask macros }
procedure XISetMask(ptr: Pcuchar; event: cint); inline;
begin
  ptr[event shr 3] := ptr[event shr 3] or (1 shl (event and 7));
end;

procedure XIClearMask(ptr: Pcuchar; event: cint); inline;
begin
  ptr[event shr 3] := ptr[event shr 3] and not (1 shl (event and 7))
end;

function XIMaskIsSet(ptr: Pcuchar; event: cint): Boolean; inline;
begin
  XIMaskIsSet := (ptr[event shr 3] and (1 shl (event and 7))) <> 0;
end;

function XIMaskLen(event: cint): cint; inline;
begin
  XIMaskLen := (event shr 3) + 1;
end;

end.

