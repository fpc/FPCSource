{ $Xorg: XKBlib.h,v 1.6 2000/08/17 19:45:03 cpqbld Exp $
************************************************************
Copyright (c) 1993 by Silicon Graphics Computer Systems, Inc.

Permission to use, copy, modify, and distribute this
software and its documentation for any purpose and without
fee is hereby granted, provided that the above copyright
notice appear in all copies and that both that copyright
notice and this permission notice appear in supporting
documentation, and that the name of Silicon Graphics not be
used in advertising or publicity pertaining to distribution
of the software without specific prior written permission.
Silicon Graphics makes no representation about the suitability
of this software for any purpose. It is provided "as is"
without any express or implied warranty.

SILICON GRAPHICS DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS
SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
AND FITNESS FOR A PARTICULAR PURPOSE. IN NO EVENT SHALL SILICON
GRAPHICS BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL
DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE,
DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE
OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION  WITH
THE USE OR PERFORMANCE OF THIS SOFTWARE.

********************************************************/
 $XFree86: xc/lib/X11/XKBlib.h,v 3.3 2001/08/01 00:44:38 tsi Exp $

 Pascal Convertion was made by Ido Kannner - kanerido@actcom.net.il

Thanks:
         I want to thanks to oliebol for putting up with all of the problems that was found
         while translating this code. ;)

         I want to thanks #fpc channel in freenode irc, for helping me, and to put up with my
         wierd questions ;)

         Thanks for mmc in #xlib on freenode irc And so for the channel itself for the helping me to
         understanding some of the problems I had converting this headers and pointing me to resources
         that helped translating this headers.

 Ido

History:
        2004/10/15        - Fixed a bug of accessing second based records by removing "paced record" and
                            chnaged it to "reocrd" only.
        2004/10/10        - Added to TXkbGetAtomNameFunc and TXkbInternAtomFunc the cdecl call.
        2004/10/06 - 09   - Convertion from the c header of XKBlib.h

}
{$PACKRECORDS C}
{$MODE OBJFPC} {$MACRO ON} {$DEFINE MACRO}

unit xkblib;

interface
uses X, Xlib, XKB;

type
        PXkbAnyEvent = ^TXkbAnyEvent;
        TXkbAnyEvent = record
                        _type      : Smallint;   // XkbAnyEvent
                        serial     : Longword;   // # of last req processed by server
                        send_event : Boolean;    // is this from a SendEvent request?
                        display    : PDisplay;   // Display the event was read from
                        time       : TTime;      // milliseconds;
                        xkb_type   : Smallint;   // XKB event minor code
                        device     : Word;       // device ID
                       end;

type
        PXkbNewKeyboardNotifyEvent = ^TXkbNewKeyboardNotifyEvent;
        TXkbNewKeyboardNotifyEvent = record
                                      _type            : Smallint;   // XkbAnyEvent
                                      serial           : Longword;   // of last req processed by server
                                      send_event       : Boolean;    // is this from a SendEvent request?
                                      display          : PDisplay;   // Display the event was read from
                                      time             : TTime;      // milliseconds
                                      xkb_type         : Smallint;   // XkbNewKeyboardNotify
                                      device           : Smallint;   // device ID
                                      old_device       : Smallint;   // device ID of previous keyboard
                                      min_key_code     : Smallint;   // minimum key code
                                      max_key_code     : Smallint;   // maximum key code
                                      old_min_key_code : Smallint;   // min key code of previous kbd
                                      old_max_key_code : Smallint;   // max key code of previous kbd
                                      changed          : Word;       // changed aspects of the keyboard
                                      req_major        : Shortint;   // major and minor opcode of req
                                      req_minor        : Shortint;   // that caused change, if applicable
                                     end;

type
        PXkbMapNotifyEvent = ^TXkbMapNotifyEvent;
        TXkbMapNotifyEvent = record
                              _type              : Smallint;  // XkbAnyEvent
                              serial             : Longword;  // of last req processed by server
                              send_event         : Boolean;   // is this from a SendEvent request
                              display            : PDisplay;  // Display the event was read from
                              time               : TTime;     // milliseconds
                              xkb_type           : Smallint;  // XkbMapNotify
                              device             : Smallint;  // device ID
                              changed            : Word;      // fields which have been changed
                              flags              : Word;      // reserved
                              first_type         : Smallint;  // first changed key type
                              num_types          : Smallint;  // number of changed key types
                              min_key_code       : TKeyCode;
                              max_key_code       : TKeyCode;
                              first_key_sym      : TKeyCode;
                              first_key_act      : TKeyCode;
                              first_key_behavior : TKeyCode;
                              first_key_explicit : TKeyCode;
                              first_modmap_key   : TKeyCode;
                              first_vmodmap_key  : TKeyCode;
                              num_key_syms       : Smallint;
                              num_key_acts       : Smallint;
                              num_key_behaviors  : Smallint;
                              num_key_explicit   : Smallint;
                              num_modmap_keys    : Smallint;
                              num_vmodmap_keys   : Smallint;
                              vmods              : Word;      // mask of changed virtual mods
                             end;

type
        PXkbStateNotifyEvent = ^TXkbStateNotifyEvent;
        TXkbStateNotifyEvent = record
                                _type              : Smallint;   // XkbAnyEvent
                                serial             : Longword;   // # of last req processed by server
                                send_event         : Boolean;    // is this from a SendEvent request?
                                display            : PDisplay;   // Display the event was read from
                                time               : TTime;      // milliseconds
                                xkb_type           : Smallint;   // XkbStateNotify
                                device             : Smallint;   // device ID
                                changed            : Word;       // mask of changed state components
                                group              : Smallint;   // keyboard group
                                base_group         : Smallint;   // base keyboard group
                                latched_group      : Smallint;   // latched keyboard group
                                locked_group       : Smallint;   // locked keyboard group
                                mods               : Word;       // modifier state
                                base_mods          : Word;       // base modifier state
                                latched_mods       : Word;       // latched modifiers
                                locked_mods        : Word;       // locked modifiers
                                compat_state       : Smallint;   // compatibility state
                                grab_mods          : Byte;       // mods used for grabs
                                compat_grab_mods   : Byte;       // grab mods for non-XKB clients
                                lookup_mods        : Byte;       // mods sent to clients
                                compat_lookup_mods : Byte;       // mods sent to non-XKB clients
                                ptr_buttons        : Smallint;   // pointer button state
                                keycode            : TKeyCode;   // keycode that caused the change
                                event_type         : Shortint;   // KeyPress or KeyRelease
                                req_major          : Shortint;   // Major opcode of request
                                req_minor          : Shortint;   // Minor opcode of request
                               end;
type
        PXkbControlsNotifyEvent = ^TXkbControlsNotifyEvent;
        TXkbControlsNotifyEvent = record
                                   _type                : Smallint;  // XkbAnyEvent
                                   serial               : Longword;  // of last req processed by server
                                   send_event           : Boolean;   // is this from a SendEvent request?
                                   display              : PDisplay;  // Display the event was read from
                                   time                 : TTime;     // milliseconds
                                   xkb_type             : Smallint;  // XkbControlsNotify
                                   device               : Smallint;  // device ID
                                   changed_ctrls        : Word;      // controls with changed sub-values
                                   enabled_ctrls        : Word;      // controls currently enabled
                                   enabled_ctrl_changes : Word;      // controls just {en,dis}abled
                                   num_groups           : Smallint;  // total groups on keyboard
                                   keycode              : TKeyCode;  // key that caused change or 0
                                   event_type           : Byte;      // type of event that caused change
                                   req_major            : Byte;      // if keycode==0, major and minor
                                   req_minor            : Byte;      // opcode of req that caused change
                                  end;

type
        PXkbIndicatorNotifyEvent = ^TXkbIndicatorNotifyEvent;
        TXkbIndicatorNotifyEvent = record
                                    _type      : Smallint;  // XkbAnyEvent
                                    serial     : Longword;  // of last req processed by server
                                    send_event : Boolean;   // is this from a SendEvent request?
                                    display    : PDisplay;  // Display the event was read from
                                    time       : TTime;     // milliseconds
                                    xkb_type   : Smallint;  // XkbIndicatorNotify
                                    device     : Smallint;  // device
                                    changed    : Word;      // indicators with new state or map
                                    state      : Word;      // current state of all indicators
                                   end;

type
        PXkbNamesNotifyEvent = ^TXkbNamesNotifyEvent;
        TXkbNamesNotifyEvent = record
                                _type              : Smallint;   // XkbAnyEvent
                                serial             : Longword;   // of last req processed by server
                                send_event         : Boolean;    // is this from a SendEvent request?
                                display            : PDisplay;   // Display the event was read from
                                time               : TTime;      // milliseconds
                                xkb_type           : Smallint;   // XkbNamesNotify
                                device             : Smallint;   // device ID
                                changed            : Longword;   // names that have changed
                                first_type         : Smallint;   // first key type with new name
                                num_types          : Smallint;   // number of key types with new names
                                first_lvl          : Smallint;   // first key type new new level names
                                num_lvls           : Smallint;   // # of key types w/new level names
                                num_aliases        : Smallint;   // total number of key aliases
                                num_radio_groups   : Smallint;   // total number of radio groups
                                changed_vmods      : Word;       // virtual modifiers with new names
                                changed_groups     : Word;       // groups with new names
                                changed_indicators : Word;       // indicators with new names
                                first_key          : Smallint;   // first key with new name
                                num_keys           : Smallint;   // number of keys with new names
                               end;

type
        PXkbCompatMapNotifyEvent = ^TXkbCompatMapNotifyEvent;
        TXkbCompatMapNotifyEvent = record
                                    _type          : Smallint;  // XkbAnyEvent
                                    serial         : Longword;  // of last req processed by server
                                    send_event     : Boolean;   // is this from a SendEvent request?
                                    display        : PDisplay;  // Display the event was read from
                                    time           : TTime;     // milliseconds
                                    xkb_type       : Smallint;  // XkbCompatMapNotify
                                    device         : Smallint;  // device ID
                                    changed_groups : Word;      // groups with new compat maps
                                    first_si       : Smallint;  // first new symbol interp
                                    num_si         : Smallint;  // number of new symbol interps
                                    num_total_si   : Smallint;  // total # of symbol interps
                                   end;

type
        PXkbBellNotifyEvent = ^TXkbBellNotifyEvent;
        TXkbBellNotifyEvent = record
                               _type      : Smallint;  // XkbAnyEvent
                               serial     : Longword;  // of last req processed by server
                               send_event : Boolean;   // is this from a SendEvent request?
                               display    : PDisplay;  // Display the event was read from
                               time       : TTime;     // milliseconds
                               xkb_type   : Smallint;  // XkbBellNotify
                               device     : Smallint;  // device ID
                               percent    : Smallint;  // requested volume as a % of maximum
                               pitch      : Smallint;  // requested pitch in Hz
                               duration   : Smallint;  // requested duration in useconds
                               bell_class : Smallint;  // (input extension) feedback class
                               bell_id    : Smallint;  // (input extension) ID of feedback
                               name       : TAtom;     // "name" of requested bell
                               window     : TWindow;   // window associated with event
                               event_only : Boolean;   // "event only" requested
                              end;

type
        PXkbActionMessageEvent = ^TXkbActionMessageEvent;
        TXkbActionMessageEvent = record
                                  _type             : Smallint;  // XkbAnyEvent
                                  serial            : Longword;  // of last req processed by server
                                  send_event        : Boolean;   // is this from a SendEvent request?
                                  display           : PDisplay;  // Display the event was read from
                                  time              : TTime;     // milliseconds
                                  xkb_type          : Smallint;  // XkbActionMessage
                                  device            : Smallint;  // device ID
                                  keycode           : TKeyCode;  // key that generated the event
                                  press             : Boolean;   // true if act caused by key press
                                  key_event_follows : Boolean;   // true if key event also generated
                                  group             : Smallint;  // effective group
                                  mods              : Word;      // effective mods
                                  message           : array [0..XkbActionMessageLength] of Char;
                                                           // message -- leave space for NUL
                                 end;

type
        PXkbAccessXNotifyEvent = ^TXkbAccessXNotifyEvent;
        TXkbAccessXNotifyEvent = record
                                  _type          : Smallint;  // XkbAnyEvent
                                  serial         : Longword;  // of last req processed by server
                                  send_event     : Boolean;   // is this from a SendEvent request?
                                  display        : PDisplay;  // Display the event was read from
                                  time           : TTime;     // milliseconds
                                  xkb_type       : Smallint;  // XkbAccessXNotify
                                  device         : Smallint;  // device ID
                                  detail         : Smallint;  // XkbAXN_*
                                  keycode        : Smallint;  // key of event
                                  sk_delay       : Smallint;  // current slow keys delay
                                  debounce_delay : Smallint;  // current debounce delay
                                 end;

type
        PXkbExtensionDeviceNotifyEvent = ^TXkbExtensionDeviceNotifyEvent;
        TXkbExtensionDeviceNotifyEvent = record
                                          _type          : Smallint;  // XkbAnyEvent
                                          serial         : Longword;  // of last req processed by server
                                          send_event     : Boolean;   // is this from a SendEvent request?
                                          display        : PDisplay;  // Display the event was read from
                                          time           : TTime;     // milliseconds
                                          xkb_type       : Smallint;  // XkbExtensionDeviceNotify
                                          device         : Smallint;  // device ID
                                          reason         : Word;      // reason for the event
                                          supported      : Word;      // mask of supported features
                                          unsupported    : Word;      // mask of unsupported features
                                                                // that some app tried to use
                                          first_btn      : Smallint;  // first button that changed
                                          num_btns       : Smallint;  // range of buttons changed
                                          leds_defined   : Word;      // indicators with names or maps
                                          led_state      : Word;      // current state of the indicators
                                          led_class      : Smallint;  // feedback class for led changes
                                          led_id         : Smallint;  // feedback id for led changes
                                         end;

type
        PXkbEvent = ^TXkbEvent;
        TXkbEvent = record
                     _type       : Smallint;
                     any         : TXkbAnyEvent;
                     new_kbd     : TXkbNewKeyboardNotifyEvent;
                     map         : TXkbMapNotifyEvent;
                     state       : TXkbStateNotifyEvent;
                     ctrls       : TXkbControlsNotifyEvent;
                     indicators  : TXkbIndicatorNotifyEvent;
                     names       : TXkbNamesNotifyEvent;
                     compat      : TXkbCompatMapNotifyEvent;
                     bell        : TXkbBellNotifyEvent;
                     message     : TXkbActionMessageEvent;
                     accessx     : TXkbAccessXNotifyEvent;
                     device      : TXkbExtensionDeviceNotifyEvent;
                     core        : TXEvent;
                    end;

type
//typedef struct        _XkbKbdDpyState XkbKbdDpyStateRec,*XkbKbdDpyStatePtr;
        PXkbKbdDpyStatePtr = ^TXkbKbdDpyStateRec;
        TXkbKbdDpyStateRec = record
                             end;

        { XkbOpenDisplay error codes }
const
        XkbOD_Success           = 0;
        XkbOD_BadLibraryVersion = 1;
        XkbOD_ConnectionRefused = 2;
        XkbOD_NonXkbServer      = 3;
        XkbOD_BadServerVersion  = 4;

        { Values for XlibFlags }
const
        XkbLC_ForceLatin1Lookup         = 1 shl  0;
        XkbLC_ConsumeLookupMods         = 1 shl  1;
        XkbLC_AlwaysConsumeShiftAndLock = 1 shl  2;
        XkbLC_IgnoreNewKeyboards        = 1 shl  3;
        XkbLC_ControlFallback           = 1 shl  4;
        XkbLC_ConsumeKeysOnComposeFail  = 1 shl 29;
        XkbLC_ComposeLED                = 1 shl 30;
        XkbLC_BeepOnComposeFail         = 1 shl 31;

        XkbLC_AllComposeControls        = $C0000000;
        XkbLC_AllControls               = $C000001F;

function XkbIgnoreExtension (ignore : Boolean) : Boolean;
        cdecl; external libX11 name 'XkbIgnoreExtension';

function XkbOpenDisplay (name : PChar; ev_rtrn, err_rtrn,
                         major_rtrn, minor_rtrn, reason : PSmallint) : PDisplay;
        cdecl; external libX11 name 'XkbOpenDisplay';

function XkbQueryExtension (dpy : PDisplay; opcodeReturn, eventBaseReturn,
                            errorBaseReturn, majorRtrn, minorRtrn: PSmallint) : Boolean;
        cdecl; external libX11 name 'XkbQueryExtension';

function XkbUseExtension (dpy : PDisplay; major_rtrn, minor_rtrn : PSmallint) : Boolean;
        cdecl; external libX11 name 'XkbUseExtension';

function XkbLibraryVersion (libMajorRtrn, libMinorRtrn : PSmallint) : Boolean;
        cdecl; external libX11 name 'XkbLibraryVersion';

function XkbSetXlibControls (dpy : PDisplay; affect, values : Word) : Word;
        cdecl; external libX11 name 'XkbSetXlibControls';

function XkbGetXlibControls (dpy : PDisplay) : Word;
        cdecl; external libX11 name 'XkbGetXlibControls';

type
        TXkbInternAtomFunc = function (dpy : PDisplay; name : PChar; only_if_exists : Boolean) : TAtom; cdecl;

type
        TXkbGetAtomNameFunc = function (dpy : PDisplay; atom : TAtom) : PChar; cdecl;

procedure XkbSetAtomFuncs (getAtom : TXkbInternAtomFunc; getName : TXkbGetAtomNameFunc);
        cdecl; external libX11 name 'XkbSetAtomFuncs';

function XkbKeycodeToKeysym (dpy : PDisplay;
                           {$IFDEF NeedWidePrototypes}
                             kc  : Word;
                           {$ELSE}
                             kc  : TKeyCode;
                           {$ENDIF}
                             group, level : Smallint) : TKeySym;
        cdecl; external libX11 name 'XkbKeycodeToKeysym';

function XkbKeysymToModifiers (dpy : PDisplay; ks : TKeySym) : Word;
        cdecl; external libX11 name 'XkbKeysymToModifiers';

function XkbLookupKeySym (dpy               : PDisplay; keycode       : TKeyCode; modifiers,
                           modifiers_return : Word;     keysym_return : PKeySym)             : Boolean;
        cdecl; external libX11 name 'XkbLookupKeySym';

function XkbLookupKeyBinding (dpy    : PDisplay; sym_rtrn : TKeySym;  mods       : Word;
                              buffer : PChar;    nbytes   : Smallint; extra_rtrn : PSmallint) : Smallint;
        cdecl; external libX11 name 'XkbLookupKeyBinding';

function XkbTranslateKeyCode (xkb              : PXkbDescPtr; keycode       : TKeyCode; modifiers,
                              modifiers_return : Word;        keysym_return : PKeySym)            : Boolean;
        cdecl; external libX11 name 'XkbTranslateKeyCode';

function XkbTranslateKeySym (dpy    : PDisplay; sym_return : TKeySym;  modifiers  : Word;
                             buffer : PChar;    nbytes     : Smallint; extra_rtrn : PSmallint) : Smallint;
        cdecl; external libX11 name 'XkbTranslateKeySym';

function XkbSetAutoRepeatRate (dpy : PDisplay; deviceSpec, delay, interval : Word) : Boolean;
        cdecl; external libX11 name 'XkbSetAutoRepeatRate';

function XkbGetAutoRepeatRate (dpy : PDisplay; deviceSpec : Word; delayRtrn, intervalRtrn : PWord) : Boolean;
        cdecl; external libX11 name 'XkbGetAutoRepeatRate';

function XkbChangeEnabledControls (dpy : PDisplay; deviceSpec, affect, values : Word) : Boolean;
        cdecl; external libX11 name 'XkbChangeEnabledControls';

function XkbDeviceBell (dpy : PDisplay; win : TWindow; deviceSpec,
                        bellClass, bellID, percent : Smallint; name : TAtom) : Boolean;
        cdecl; external libX11 name 'XkbDeviceBell';

function XkbForceDeviceBell (dpy : PDisplay; deviceSpec, bellClass, bellID, percent : Smallint) : Boolean;
        cdecl; external libX11 name 'XkbForceDeviceBell';

function XkbDeviceBellEvent (dpy : PDisplay; win : TWindow; deviceSpec,
                             bellClass, bellID, percent : Smallint; name : TAtom) : Boolean;
        cdecl; external libX11 name 'XkbDeviceBellEvent';

function XkbBell (dpy : PDisplay; win : TWindow; percent : Smallint; name : TAtom) : Boolean;
        cdecl; external libX11 name 'XkbBell';

function XkbForceBell (dpy : PDisplay; percent : Smallint) : Boolean;
        cdecl; external libX11 name 'XkbForceBell';

function XkbBellEvent (dpy : PDisplay; win : TWindow; percent : Smallint; name : TAtom) : Boolean;
        cdecl; external libX11 name 'XkbBellEvent';

function XkbSelectEvents (dpy : PDisplay; deviceID, affect, values : Word) : Boolean;
        cdecl; external libX11 name 'XkbSelectEvents';

function XkbSelectEventDetails (dpy : PDisplay; deviceID, eventType : Word;
                                affect, details : Longword) : Boolean;
        cdecl; external libX11 name 'XkbSelectEventDetails';

procedure XkbNoteMapChanges (old : PXkbMapChangesPtr; new : PXkbMapNotifyEvent; wanted : Word);
        cdecl; external libX11 name 'XkbNoteMapChanges';

procedure XkbNoteNameChanges (old : PXkbNameChangesPtr; new : PXkbNamesNotifyEvent; wanted : Word);
        cdecl; external libX11 name 'XkbNoteNameChanges';

function XkbGetIndicatorState (dpy : PDisplay; deviceSpec : Word; pStateRtrn : PWord) : TStatus;
        cdecl; external libX11 name 'XkbGetIndicatorState';

function XkbGetDeviceIndicatorState (dpy   : PDisplay; deviceSpec, ledClass,
                                     ledID : Word;     pStateRtrn : PWord)   : TStatus;
        cdecl; external libX11 name 'XkbGetDeviceIndicatorState';

function XkbGetIndicatorMap (dpy : PDisplay; which : Longword; desc : PXkbDescPtr) : TStatus;
        cdecl; external libX11 name 'XkbGetIndicatorMap';

function XkbSetIndicatorMap (dpy : PDisplay; which : Longword; desc : PXkbDescPtr) : Boolean;
        cdecl; external libX11 name 'XkbSetIndicatorMap';

procedure XkbNoteIndicatorMapChanges   (o, n : PXkbIndicatorChangesPtr; w : Word);
procedure XkbNoteIndicatorStateChanges (o, n : PXkbIndicatorChangesPtr; w : Word);

function  XkbGetIndicatorMapChanges    (d : PDisplay; x : PXkbDescPtr;
                                        c : PXkbIndicatorChangesPtr) : TStatus;

function  XkbChangeIndicatorMaps       (d : PDisplay; x : PXkbDescPtr;
                                        c : PXkbIndicatorChangesPtr) : Boolean;

function XkbGetNamedIndicator (dpy        : PDisplay; name     : TAtom; pNdxRtrn : PSmallint;
                               pStateRtrn : PBoolean; pMapRtrn : PXkbIndicatorMapPtr;
                                                                                                                         pRealRtrn  : PBoolean)                                  : Boolean;
        cdecl; external libX11 name 'XkbGetNamedIndicator';

function XkbGetNamedDeviceIndicator (dpy      : PDisplay; deviceSpec, ledClass, ledID : Word;
                                     name     : TAtom;    pNdxRtrn : PSmallint; pStateRtrn : PBoolean;
                                                                                                                                                 pMapRtrn : PXkbIndicatorMapPtr; pRealRtrn  : PBoolean) : Boolean;
        cdecl; external libX11 name 'XkbGetNamedDeviceIndicator';

function XkbSetNamedIndicator (dpy : PDisplay; name : TAtom; changeState,
                               state, createNewMap : Boolean; pMap : PXkbIndicatorMapPtr) : Boolean;
        cdecl; external libX11 name 'XkbSetNamedIndicator';

function XkbSetNamedDeviceIndicator (dpy  : PDisplay; deviceSpec, ledClass, ledID : Word;
                                                                                                                                                 name : TAtom; changeState, state, createNewMap : Boolean;
                                                                                                                                                 pMap : PXkbIndicatorMapPtr) : Boolean;
        cdecl; external libX11 name 'XkbSetNamedDeviceIndicator';

function XkbLockModifiers (dpy : PDisplay; deviceSpec, affect, values : Word) : Boolean;
        cdecl; external libX11 name 'XkbLockModifiers';

function XkbLatchModifiers (dpy : PDisplay; deviceSpec, affect, values : Word) : Boolean;
        cdecl; external libX11 name 'XkbLatchModifiers';

function XkbLockGroup (dpy : PDisplay; deviceSpec, group : Word) : Boolean;
        cdecl; external libX11 name 'XkbLockGroup';

function XkbLatchGroup (dpy : PDisplay; deviceSpec, group : Word) : Boolean;
        cdecl; external libX11 name 'XkbLatchGroup';

function XkbSetServerInternalMods (dpy : PDisplay; deviceSpec, affectReal,
                                   realValues, affectVirtual, virtualValues : Word) : Boolean;
        cdecl; external libX11 name 'XkbSetServerInternalMods';

function XkbSetIgnoreLockMods (dpy : PDisplay; deviceSpec, affectReal,
                                   realValues, affectVirtual, virtualValues : Word) : Boolean;
        cdecl; external libX11 name 'XkbSetIgnoreLockMods';

function XkbVirtualModsToReal (dpy : PDisplay; virtual_mask : Word; mask_rtrn : PWord) : Boolean;
        cdecl; external libX11 name 'XkbVirtualModsToReal';

function XkbComputeEffectiveMap (xkb : PXkbDescPtr; _type : PXkbKeyTypePtr; map_rtrn : PByte) : Boolean;
        cdecl; external libX11 name 'XkbComputeEffectiveMap';

function XkbInitCanonicalKeyTypes (xkb : PXkbDescPtr; which : Word; keypadVMod : Smallint) : TStatus;
        cdecl; external libX11 name 'XkbInitCanonicalKeyTypes';

function XkbAllocKeyboard : PXkbDescPtr;
        cdecl; external libX11 name 'XkbAllocKeyboard';

procedure XkbFreeKeyboard (xkb : PXkbDescPtr; which : Word; freeDesc : Boolean);
        cdecl; external libX11 name 'XkbFreeKeyboard';

function XkbAllocClientMap (xkb : PXkbDescPtr; which, nTypes : Word) : TStatus;
        cdecl; external libX11 name 'XkbAllocClientMap';

function XkbAllocServerMap (xkb : PXkbDescPtr; which, nActions : Word) : TStatus;
        cdecl; external libX11 name 'XkbAllocServerMap';

procedure XkbFreeClientMap (xkb : PXkbDescPtr; what : Word; freeMap : Boolean);
        cdecl; external libX11 name 'XkbFreeClientMap';

procedure XkbFreeServerMap (xkb : PXkbDescPtr; what : Word; freeMap : Boolean);
        cdecl; external libX11 name 'XkbFreeServerMap';

function XkbAddKeyType (xkb           : PXkbDescPtr; name     : TAtom; map_count : Smallint;
                        want_preserve : Boolean;     num_lvls : Smallint) : PXkbKeyTypePtr;
        cdecl; external libX11 name 'XkbAddKeyType';

function XkbAllocIndicatorMaps (xkb : PXkbDescPtr) : TStatus;
        cdecl; external libX11 name 'XkbAllocIndicatorMaps';

procedure XkbFreeIndicatorMaps (xkb : PXkbDescPtr);
        cdecl; external libX11 name 'XkbFreeIndicatorMaps';

function XkbGetMap (dpy : PDisplay; which, deviceSpec : Word) : PXkbDescPtr;
        cdecl; external libX11 name 'XkbGetMap';

function XkbGetUpdatedMap (dpy : PDisplay; which : Word; desc : PXkbDescPtr) : TStatus;
        cdecl; external libX11 name 'XkbGetUpdatedMap';

function XkbGetMapChanges (dpy : PDisplay; xkb : PXkbDescPtr; changes : PXkbMapChangesPtr) : TStatus;
        cdecl; external libX11 name 'XkbGetMapChanges';

function XkbRefreshKeyboardMapping (event : PXkbMapNotifyEvent) : TStatus;
        cdecl; external libX11 name 'XkbRefreshKeyboardMapping';

function XkbGetKeyTypes (dpy : PDisplay; first, num : Word; xkb : PXkbDescPtr) : TStatus;
        cdecl; external libX11 name 'XkbGetKeyTypes';

function XkbGetKeySyms (dpy : PDisplay; first, num : Word; xkb : PXkbDescPtr) : TStatus;
        cdecl; external libX11 name 'XkbGetKeySyms';

function XkbGetKeyActions (dpy : PDisplay; first, num : Word; xkb : PXkbDescPtr) : TStatus;
        cdecl; external libX11 name 'XkbGetKeyActions';

function XkbGetKeyBehaviors (dpy : PDisplay; firstKey, nKeys : Word; desc : PXkbDescPtr) : TStatus;
        cdecl; external libX11 name 'XkbGetKeyBehaviors';

function XkbGetVirtualMods (dpy : PDisplay; which : Word; desc : PXkbDescPtr) : TStatus;
        cdecl; external libX11 name 'XkbGetVirtualMods';

function XkbGetKeyExplicitComponents (dpy : PDisplay; firstKey, nKeys : Word; desc : PXkbDescPtr) : TStatus;
        cdecl; external libX11 name 'XkbGetKeyExplicitComponents';

function XkbGetKeyModifierMap (dpy : PDisplay; firstKey, nKeys : Word; desc : PXkbDescPtr) : TStatus;
        cdecl; external libX11 name 'XkbGetKeyModifierMap';

function XkbAllocControls (xkb : PXkbDescPtr; which : Word) : TStatus;
        cdecl; external libX11 name 'XkbAllocControls';

procedure XkbFreeControls (xkb : PXkbDescPtr; which : Word; freeMap : Boolean);
        cdecl; external libX11 name 'XkbFreeControls';

function XkbGetControls (dpy : PDisplay; which : Longword; desc : PXkbDescPtr) : TStatus;
        cdecl; external libX11 name 'XkbGetControls';

function XkbSetControls (dpy : PDisplay; which : Longword; desc : PXkbDescPtr) : Boolean;
        cdecl; external libX11 name 'XkbSetControls';

procedure XkbNoteControlsChanges (old    : PXkbControlsChangesPtr; new : PXkbControlsNotifyEvent;
                                                                                                                                  wanted : Word);
        cdecl; external libX11 name 'XkbNoteControlsChanges';

function XkbGetControlsChanges (d : PDisplay; x : PXkbDescPtr; c : PXkbControlsChangesPtr) : TStatus;
function XkbChangeControls     (d : PDisplay; x : PXkbDescPtr; c : PXkbControlsChangesPtr) : Boolean;

function XkbAllocCompatMap (xkb : PXkbDescPtr; which, nInterpret : Word) : TStatus;
        cdecl; external libX11 name 'XkbAllocCompatMap';

procedure XkbFreeCompatMap (xkib : PXkbDescPtr; which : Word; freeMap : Boolean);
        cdecl; external libX11 name 'XkbFreeCompatMap';

function XkbGetCompatMap (dpy : PDisplay;  which : Word; xkb : PXkbDescPtr) : TStatus;
        cdecl; external libX11 name 'XkbGetCompatMap';

function XkbSetCompatMap (dpy : PDisplay;    which         : Word;
                          xkb : PXkbDescPtr; updateActions : Boolean) : Boolean;
        cdecl; external libX11 name 'XkbSetCompatMap';

function XkbAddSymInterpret (xkb       : PXkbDescPtr; si      : PXkbSymInterpretPtr;
                             updateMap : Boolean;     changes : PXkbChangesPtr) : PXkbSymInterpretPtr;
        cdecl; external libX11 name 'XkbAddSymInterpret';

function XkbAllocNames (xkb : PXkbDescPtr; which : Word; nTotalRG, nTotalAliases : Smallint) : TStatus;
        cdecl; external libX11 name 'XkbAllocNames';

function XkbGetNames (dpy : PDisplay; which : Word; desc : PXkbDescPtr) : TStatus;
        cdecl; external libX11 name 'XkbGetNames';

function XkbSetNames (dpy : PDisplay; which, firstType, nTypes : Word; desc : PXkbDescPtr) : Boolean;
        cdecl; external libX11 name 'XkbSetNames';

function XkbChangeNames (dpy : PDisplay; xkb : PXkbDescPtr; changes : PXkbNameChangesPtr) : Boolean;
        cdecl; external libX11 name 'XkbChangeNames';

procedure XkbFreeNames (xkb : PXkbDescPtr; which : Word; freeMap : Boolean);
        cdecl; external libX11 name 'XkbFreeNames';

function XkbGetState(dpy : PDisplay; deviceSpec : Word; rtrnState : PXkbStatePtr) : TStatus;
        cdecl; external libX11 name 'XkbGetState';

function XkbSetMap (dpy : PDisplay; which : Word; desc : PXkbDescPtr) : Boolean;
        cdecl; external libX11 name 'XkbSetMap';

function XkbChangeMap (dpy : PDisplay; desc : PXkbDescPtr; changes : PXkbMapChangesPtr) : Boolean;
        cdecl; external libX11 name 'XkbChangeMap';

function XkbSetDetectableAutoRepeat (dpy : PDisplay; detectable : Boolean; supported : PBoolean) : Boolean;
        cdecl; external libX11 name 'XkbSetDetectableAutoRepeat';

function XkbGetDetectableAutoRepeat (dpy : PDisplay; supported : PBoolean) : Boolean;
        cdecl; external libX11 name 'XkbGetDetectableAutoRepeat';

function XkbSetAutoResetControls (dpy : PDisplay; changes : Word;
                                  auto_ctrls, auto_values : PWord) : Boolean;
        cdecl; external libX11 name 'XkbSetAutoResetControls';

function XkbGetAutoResetControls (dpy : PDisplay; auto_ctrls, auto_ctrl_values : PWord) : Boolean;
        cdecl; external libX11 name 'XkbGetAutoResetControls';

function XkbSetPerClientControls (dpy : PDisplay; change : Word; values : PWord) : Boolean;
        cdecl; external libX11 name 'XkbSetPerClientControls';

function XkbGetPerClientControls (dpy : PDisplay; ctrls : PWord) : Boolean;
        cdecl; external libX11 name 'XkbGetPerClientControls';

function XkbCopyKeyType (from, into : PXkbKeyTypePtr) : TStatus;
        cdecl; external libX11 name 'XkbCopyKeyType';

function XkbCopyKeyTypes (from, into : PXkbKeyTypePtr; num_types : Smallint) : TStatus;
        cdecl; external libX11 name 'XkbCopyKeyTypes';

function XkbResizeKeyType (xkb : PXkbDescPtr; type_ndx, map_count : Smallint;
                           want_preserve : Boolean;  new_num_lvls : Smallint) : TStatus;
        cdecl; external libX11 name 'XkbResizeKeyType';

function XkbResizeKeySyms (desc : PXkbDescPtr; forKey, symsNeeded : Smallint) : PKeySym;
        cdecl; external libX11 name 'XkbResizeKeySyms';

function XkbResizeKeyActions (desc : PXkbDescPtr; forKey, actsNeeded : Smallint) : PXkbAction;
        cdecl; external libX11 name 'XkbResizeKeyActions';

function XkbChangeTypesOfKey (xkb    : PXkbDescPtr; key, num_groups : Smallint;
                              groups : Word; newTypes : PSmallint; pChanges : PXkbMapChangesPtr) : TStatus;
        cdecl; external libX11 name 'XkbChangeTypesOfKey';

(***====================================================================***)

function XkbListComponents (dpy   : PDisplay; deviceSpec : Word;
                            ptrns : PXkbComponentNamesPtr; max_inout : PSmallint) : PXkbComponentListPtr;
        cdecl; external libX11 name 'XkbListComponents';

procedure XkbFreeComponentList (list : PXkbComponentListPtr);
        cdecl; external libX11 name 'XkbFreeComponentList';

function XkbGetKeyboard (dpy : PDisplay; which, deviceSpec : Word) : PXkbDescPtr;
        cdecl; external libX11 name 'XkbGetKeyboard';

function XkbGetKeyboardByName (dpy : PDisplay; deviceSpec : Word; names : PXkbComponentNamesPtr;
                                     want, need : Word; load : Boolean) : PXkbDescPtr;
        cdecl; external libX11 name 'XkbGetKeyboardByName';

(***====================================================================***)

function XkbKeyTypesForCoreSymbols (                                   // returns # of groups
                                    xkb           : PXkbDescPtr;       // keyboard device
                                    map_width     : Smallint;          // keyboard device
                                                                                                                                                core_syms     : PKeySym;           // always mapWidth symbols
                                                                                                                                                protected     : Word;              // explicit key types
                                                                                                                                                types_inout   : PSmallint;         // always four type indices
                                                                                                                                                xkb_syms_rtrn : PKeySym            // must have enough space
                                                                                                                                         ) : Smallint;
        cdecl; external libX11 name 'XkbKeyTypesForCoreSymbols';

function XkbApplyCompatMapToKey (                                      // False only on error
                                 xkb     : PXkbDescPtr;                // keymap to be edited
                                                                                                                                 key     : TKeyCode;                   // key to be updated
                                                                                                                                 changes : PXkbChangesPtr              // resulting changes to map
                                                                                                                                ) : Boolean;
        cdecl; external libX11 name 'XkbApplyCompatMapToKey';

function XkbUpdateMapFromCore (                                        //False only on error
                               xkb          : PXkbDescPtr;             // XKB keyboard to be edited
                                                                                                                         first_key    : TKeyCode;                // first changed key
                                                                                                                         num_keys,                               // number of changed keys
                                                                                                                         map_width    : Smallint;                // width of core keymap
                                                                                                                         core_keysyms : PKeySym;                 // symbols from core keymap
                                                                                                                         changes      : PXkbChangesPtr           // resulting changes
                                                                                                                        ) : Boolean;
        cdecl; external libX11 name 'XkbUpdateMapFromCore';

(***====================================================================***)

function XkbAddDeviceLedInfo (devi : PXkbDeviceInfoPtr; ledClass, ledId : Word) : PXkbDeviceLedInfoPtr;
        cdecl; external libX11 name 'XkbAddDeviceLedInfo';

function XkbResizeDeviceButtonActions (devi : PXkbDeviceInfoPtr; newTotal : Word) : TStatus;
        cdecl; external libX11 name 'XkbResizeDeviceButtonActions';

function XkbAllocDeviceInfo (deviceSpec, nButtons, szLeds : Word) : PXkbDeviceInfoPtr;
        cdecl; external libX11 name 'XkbAllocDeviceInfo';

procedure XkbFreeDeviceInfo (devi : PXkbDeviceInfoPtr; which : Word; freeDevI : Boolean);
        cdecl; external libX11 name 'XkbFreeDeviceInfo';

procedure XkbNoteDeviceChanges (old    : PXkbDeviceChangesPtr; new : PXkbExtensionDeviceNotifyEvent;
                                wanted : Word);
        cdecl; external libX11 name 'XkbNoteDeviceChanges';

function XkbGetDeviceInfo (dpy : PDisplay; which, deviceSpec, ledClass, ledID : Word) : PXkbDeviceInfoPtr;
        cdecl; external libX11 name 'XkbGetDeviceInfo';

function XkbGetDeviceInfoChanges (dpy : PDisplay; devi : PXkbDeviceInfoPtr;
                                  changes : PXkbDeviceChangesPtr)           : TStatus;
        cdecl; external libX11 name 'XkbGetDeviceInfoChanges';

function XkbGetDeviceButtonActions (dpy : PDisplay; devi : PXkbDeviceInfoPtr; all : Boolean;
                                    first, nBtns : Word)                                     : TStatus;
        cdecl; external libX11 name 'XkbGetDeviceButtonActions';

function XkbGetDeviceLedInfo (dpy : PDisplay; devi : PXkbDeviceInfoPtr;
                              ledClass, ledId, which : Word)            : TStatus;
        cdecl; external libX11 name 'XkbGetDeviceLedInfo';

function XkbSetDeviceInfo (dpy : PDisplay; which : Word; devi : PXkbDeviceInfoPtr) : Boolean;
        cdecl; external libX11 name 'XkbSetDeviceInfo';

function XkbChangeDeviceInfo (dpy : PDisplay; desc : PXkbDeviceInfoPtr;
                              changes : PXkbDeviceChangesPtr)            : Boolean;
        cdecl; external libX11 name 'XkbChangeDeviceInfo';

function XkbSetDeviceLedInfo (dpy : PDisplay; devi : PXkbDeviceInfoPtr;
                              ledClass, ledID, which : Word)             : Boolean;
        cdecl; external libX11 name 'XkbSetDeviceLedInfo';

function XkbSetDeviceButtonActions (dpy : PDisplay; devi : PXkbDeviceInfoPtr;
                                    first, nBtns : Word)                 : Boolean;
        cdecl; external libX11 name 'XkbSetDeviceButtonActions';

(***====================================================================***)

function XkbToControl (c : Byte) : Byte;
        cdecl; external libX11 name 'XkbToControl';

(***====================================================================***)

function XkbSetDebuggingFlags (dpy : PDisplay; mask, flags : Word; msg : PChar;
                               ctrls_mask, ctrls, rtrn_flags, rtrn_ctrls : Word) : Boolean;
        cdecl; external libX11 name 'XkbSetDebuggingFlags';

function XkbApplyVirtualModChanges (xkb : PXkbDescPtr; changed : Word; changes : PXkbChangesPtr) : Boolean;
        cdecl; external libX11 name 'XkbApplyVirtualModChanges';

implementation

procedure XkbNoteIndicatorMapChanges (o, n : PXkbIndicatorChangesPtr; w : Word);
begin
{#define XkbNoteIndicatorMapChanges(o,n,w) ((o)->map_changes|=((n)->map_changes&(w)))}
        o^.map_changes := o^.map_changes or (n^.map_changes and w);
end;

procedure XkbNoteIndicatorStateChanges (o, n : PXkbIndicatorChangesPtr; w : Word);
begin
{#define XkbNoteIndicatorStateChanges(o,n,w) ((o)->state_changes|=((n)->state_changes&(w)))}
        o^.state_changes := o^.state_changes or (n^.state_changes and w);
end;

function XkbGetIndicatorMapChanges (d : PDisplay; x : PXkbDescPtr; c : PXkbIndicatorChangesPtr) : TStatus;
begin
{#define XkbGetIndicatorMapChanges(d,x,c) (XkbGetIndicatorMap((d),(c)->map_changes,x)}
        Result := XkbGetIndicatorMap (d, c^.map_changes, x);
end;

function XkbChangeIndicatorMaps (d : PDisplay; x : PXkbDescPtr; c : PXkbIndicatorChangesPtr) : Boolean;
begin
{#define XkbChangeIndicatorMaps(d,x,c) (XkbSetIndicatorMap((d),(c)->map_changes,x))}
        Result := XkbSetIndicatorMap (d, c^.map_changes, x);
end;

function XkbGetControlsChanges (d : PDisplay; x : PXkbDescPtr; c : PXkbControlsChangesPtr) : TStatus;
begin
{#define XkbGetControlsChanges(d,x,c) XkbGetControls(d,(c)->changed_ctrls,x)}
        Result := XkbGetControls (d, c^.changed_ctrls, x);
end;

function XkbChangeControls (d : PDisplay; x : PXkbDescPtr; c : PXkbControlsChangesPtr) : Boolean;
begin
{#define XkbChangeControls(d,x,c) XkbSetControls(d,(c)->changed_ctrls,x)}
        Result := XkbSetControls (d, c^.changed_ctrls, x);
end;

end.
