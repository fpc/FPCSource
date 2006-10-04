{
 $Xorg: XKB.h,v 1.3 2000/08/18 04:05:45 coskrey Exp $
************************************************************
 $Xorg: XKBstr.h,v 1.3 2000/08/18 04:05:45 coskrey Exp $
************************************************************
 $Xorg: XKBgeom.h,v 1.3 2000/08/18 04:05:45 coskrey Exp $
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

********************************************************
 $XFree86: xc/include/extensions/XKB.h,v 1.5 2002/11/20 04:49:01 dawes Exp $
 $XFree86: xc/include/extensions/XKBgeom.h,v 3.9 2002/09/18 17:11:40 tsi Exp $

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
        2004/10/15           - Fixed a bug of accessing second based records by removing "paced record" and
                               chnaged it to "reocrd" only.
        2004/10/04 - 06      - Convertion from the c header of XKBgeom.h.
        2004/10/03           - Removed the XKBstr_UNIT compiler decleration. Afther the joined files,
                                                                                     There is no need for it anymore.
                                                                             - There is a need to define (for now) XKBgeom (compiler define) in order
                                                                               to use the code of it. At this moment, I did not yet converted it to Pascal.

        2004/09/17 - 10/04   - Convertion from the c header of XKBstr.

        2004/10/03           - Joined xkbstr.pas into xkb.pas because of the circular calls problems.
                             - Added the history of xkbstr.pas above this addition.

        2004/09/17           - Fixed a wrong convertion number of XkbPerKeyBitArraySize, insted
                               of float, it's now converted into integer (as it should have been).

        2004/09/15 - 16      - Convertion from the c header of XKB.h.
}
{$PACKRECORDS C}
{$MODE OBJFPC} {$MACRO ON} {$DEFINE MACRO}

unit XKB;
interface
uses X, Xlib;

function XkbCharToInt    (v       : Byte) : SmallInt;
procedure XkbIntTo2Chars (i : Word; var h, l : byte);

function Xkb2CharsToInt (h, l    : Byte)  : SmallInt;

        {
          Common data structures and access macros
        }
type
        PXkbStatePtr = ^TXkbStateRec;
        TXkbStateRec = record
                         group              : Byte;
                         locked_group       : Byte;
                         base_group         : Word;
                                                                         latched_group      : Word;
                                                                         mods               : Byte;
                         base_mods          : Byte;
                         latched_mods       : Byte;
                         locked_mods        : Byte;
                         compat_state       : Byte;
                         grab_mods          : Byte;
                                                                         compat_grab_mods   : Byte;
                                                                         lookup_mods        : Byte;
                         compat_lookup_mods : Byte;
                         ptr_buttons        : Word;
                        end;

function XkbModLocks          (s : PXkbStatePtr) : Byte;
function XkbStateMods         (s : PXkbStatePtr) : Word;
function XkbGroupLock         (s : PXkbStatePtr) : Byte;
function XkbStateGroup        (s : PXkbStatePtr) : Word;
function XkbStateFieldFromRec (s : PXkbStatePtr) : Cardinal;
function XkbGrabStateFromRec  (s : PXkbStatePtr) : Cardinal;

type
        PXkbModsPtr = ^TXkbModsRec;
        TXkbModsRec = record
                       mask      : Byte; // effective mods
                                                                 real_mods : Byte;
                       vmods     : Word;
                      end;

type
        PXkbKTMapEntryPtr = ^TXkbKTMapEntryRec;
        TXkbKTMapEntryRec = record
                             active : Boolean;
                             level  : Byte;
                             mods   : TXkbModsRec;
                            end;

type
        PXkbKeyTypePtr = ^TXkbKeyTypeRec;
        TXkbKeyTypeRec = record
                          mods        : TXkbModsRec;
                          num_levels  : Byte;
                          map_count   : Byte;
                          map         : PXkbKTMapEntryPtr;
                          preserve    : PXkbModsPtr;
                          name        : TAtom;
                          level_names : TAtom;
                         end;

function XkbNumGroups             (g       : Word) : Word;
function XkbOutOfRangeGroupInfo   (g       : Word) : Word;
function XkbOutOfRangeGroupAction (g       : Word) : Word;
function XkbOutOfRangeGroupNumber (g       : Word) : Word;
function XkbSetGroupInfo          (g, w, n : Word) : Word;
function XkbSetNumGroups          (g, n    : Word) : Word;

        {
          Structures and access macros used primarily by the server
        }

type
        PXkbBehavior = ^TXkbBehavior;
        TXkbBehavior = record
                        _type : Byte;
                        data  : Byte;
                       end;


type
        PXkbModAction = ^TXkbModAction;
        TXkbModAction = record
                                                                         _type     : Byte;
                         flags     : Byte;
                         mask      : Byte;
                                                                         real_mods : Byte;
                         vmods1    : Byte;
                         vmods2    : Byte;
                        end;

function XkbModActionVMods     (a : PXkbModAction) : SmallInt;
procedure XkbSetModActionVMods (a : PXkbModAction; v : Byte);

type
        PXkbGroupAction = ^TXkbGroupAction;
        TXkbGroupAction = record
                           _type     : Byte;
                           flags     : Byte;
                           group_XXX : ShortInt;
                                                                                end;

function  XkbSAGroup    (a : PXkbGroupAction) : ShortInt;
procedure XkbSASetGroup (a : PXkbGroupAction; g : ShortInt);

type
        PXkbISOAction = ^TXkbISOAction;
        TXkbISOAction = record
                         _type     : Byte;
                         flags     : Byte;
                         mask      : Byte;
                         real_mods : Byte;
                         group_XXX : ShortInt;
                                                                         affect    : Byte;
                         vmods1    : Byte;
                         vmods2    : Byte;
                        end;

type
        PXkbPtrAction = ^TXkbPtrAction;
        TXkbPtrAction = record
                         _type    : Byte;
                         flags    : Byte;
                         high_XXX : Byte;
                                                                         low_XXX  : Byte;
                                                                         high_YYY : Byte;
                         low_YYY  : Byte;
                        end;

function  XkbPtrActionX    (a : PXkbPtrAction) : Word;
function  XkbPtrActionY    (a : PXkbPtrAction) : Word;
procedure XkbSetPtrActionX (a : PXkbPtrAction; x : Byte);
procedure XkbSetPtrActionY (a : PXkbPtrAction; y : Byte);

type
        PXkbPtrBtnAction = ^TXkbPtrBtnAction;
        TXkbPtrBtnAction = record
                                                                                        _type  : Byte;
                            flags  : Byte;
                            count  : Byte;
                            button : Byte;
                           end;

type
        PXkbPtrDfltAction = ^TXkbPtrDfltAction;
        TXkbPtrDfltAction = record
                             _type    : Byte;
                             flags    : Byte;
                             affect   : Byte;
                             valueXXX : ShortInt;
                            end;

function XkbSAPtrDfltValue    (a : PXkbPtrDfltAction) : ShortInt;
procedure XkbSASetPtrDfltValue (a : PXkbPtrDfltAction; const c);

type
        PXkbSwitchScreenAction = ^TXkbSwitchScreenAction;
        TXkbSwitchScreenAction = record
                                  _type     : Byte;
                                  flags     : Byte;
                                  screenXXX : ShortInt;
                                end;

function XkbSAScreen     (a : PXkbSwitchScreenAction) : ShortInt;
procedure XkbSASetScreen (a : PXkbSwitchScreenAction; const s);

type
        PXkbCtrlsAction = ^TXkbCtrlsAction;
        TXkbCtrlsAction = record
                           _type  : Byte;
                           flags  : Byte;
                           ctrls3 : Byte;
                           ctrls2 : Byte;
                                                                                 ctrls1 : Byte;
                           ctrls0 : Byte;
                          end;

procedure XkbActionSetCtrls (a : PXkbCtrlsAction; c : Byte);
function XkbActionCtrls     (a : PXkbCtrlsAction) : Word;

type
        PXkbMessageAction = ^TXkbMessageAction;
        TXkbMessageAction = record
                             _type   : Byte;
                             flags   : Byte;
                             message : array [0..5] of char;
                            end;

type
        PXkbRedirectKeyAction = ^TXkbRedirectKeyAction;
        TXkbRedirectKeyAction = record
                                 _type       : Byte;
                                 new_key     : Byte;
                                 mods_mask   : Byte;
                                 mods        : Byte;
                                 vmods_mask0 : Byte;
                                 vmods_mask1 : Byte;
                                 vmods0      : Byte;
                                                                                                         vmods1      : Byte;
                                end;

function XkbSARedirectVMods         (a : PXkbRedirectKeyAction) : Word;
procedure XkbSARedirectSetVMods     (a : PXkbRedirectKeyAction; m : Byte);
function XkbSARedirectVModsMask     (a : PXkbRedirectKeyAction) : Word;
procedure XkbSARedirectSetVModsMask (a : PXkbRedirectKeyAction; m : Byte);

type
        PXkbDeviceBtnAction = ^TXkbDeviceBtnAction;
        TXkbDeviceBtnAction = record
                               _type  : Byte;
                               flags  : Byte;
                               count  : Byte;
                               button : Byte;
                                                                                                 device : Byte;
                              end;

type
        PXkbDeviceValuatorAction = ^TXkbDeviceValuatorAction;
        TXkbDeviceValuatorAction = record
                                    _type    : Byte;
                                    device   : Byte;
                                                                                                                        v1_what  : Byte;
                                    v1_ndx   : Byte;
                                    v1_value : Byte;
                                    v2_what  : Byte;
                                                                                                                        v2_ndx   : Byte;
                                    v2_value : Byte;
                                   end;

    {
      Macros to classify key actions
                }
const
        XkbAnyActionDataSize = 7;

type
        PXkbAnyAction = ^TXkbAnyAction;
        TXkbAnyAction = record
                         _type : byte;
                         data  : array [0..XkbAnyActionDataSize-1] of byte;
                        end;

function XkbIsModAction   (a : PXkbAnyAction) : LongBool;
function XkbIsGroupAction (a : PXkbAnyAction) : LongBool;
function XkbIsPtrAction   (a : PXkbAnyAction) : LongBool;

type
        PXkbAction = ^TXkbAction;
        TXkbAction = record
                                                                any      : TXkbAnyAction;
                      mods     : TXkbModAction;
                      group    : TXkbGroupAction;
                      iso      : TXkbISOAction;
                      ptr      : TXkbPtrAction;
                      btn      : TXkbPtrBtnAction;
                      dflt     : TXkbPtrDfltAction;
                      screen   : TXkbSwitchScreenAction;
                      ctrls    : TXkbCtrlsAction;
                      msg      : TXkbMessageAction;
                      redirect : TXkbRedirectKeyAction;
                      devbtn   : TXkbDeviceBtnAction;
                      devval   : TXkbDeviceValuatorAction;
                      _type    : Byte;
                     end;

   {
      XKB request codes, used in:
      -  xkbReqType field of all requests
      -  requestMinor field of some events
                }
const
        X_kbUseExtension      =   0;
        X_kbSelectEvents      =   1;
        X_kbBell              =   3;
        X_kbGetState          =   4;
        X_kbLatchLockState    =   5;
        X_kbGetControls       =   6;
        X_kbSetControls       =   7;
        X_kbGetMap            =   8;
        X_kbSetMap            =   9;
        X_kbGetCompatMap      =  10;
        X_kbSetCompatMap      =  11;
        X_kbGetIndicatorState =  12;
        X_kbGetIndicatorMap   =  13;
        X_kbSetIndicatorMap   =  14;
        X_kbGetNamedIndicator =  15;
        X_kbSetNamedIndicator =  16;
        X_kbGetNames          =  17;
        X_kbSetNames          =  18;
        X_kbGetGeometry       =  19;
        X_kbSetGeometry       =  20;
        X_kbPerClientFlags    =  21;
        X_kbListComponents    =  22;
        X_kbGetKbdByName      =  23;
        X_kbGetDeviceInfo     =  24;
        X_kbSetDeviceInfo     =  25;
        X_kbSetDebuggingFlags = 101;

   {
      In the X sense, XKB reports only one event.
      The type field of all XKB events is XkbEventCode
                }
const
        XkbEventCode    = 0;
        XkbNumberEvents = XkbEventCode +1;

   {
      XKB has a minor event code so it can use one X event code for
      multiple purposes.
       - reported in the xkbType field of all XKB events.
       - XkbSelectEventDetails: Indicates the event for which event details
         are being changed
                }
const
        XkbNewKeyboardNotify     =  0;
        XkbMapNotify             =  1;
        XkbStateNotify           =  2;
        XkbControlsNotify        =  3;
        XkbIndicatorStateNotify  =  4;
        XkbIndicatorMapNotify    =  5;
        XkbNamesNotify           =  6;
        XkbCompatMapNotify       =  7;
        XkbBellNotify            =  8;
        XkbActionMessage         =  9;
        XkbAccessXNotify         = 10;
        XkbExtensionDeviceNotify = 11;

    {
      Event Mask:
       - XkbSelectEvents:  Specifies event interest.
    }
const
        XkbNewKeyboardNotifyMask     = Cardinal(1) shl  0;
        XkbMapNotifyMask             = Cardinal(1) shl  1;
        XkbStateNotifyMask           = Cardinal(1) shl  2;
        XkbControlsNotifyMask        = Cardinal(1) shl  3;
        XkbIndicatorStateNotifyMask  = Cardinal(1) shl  4;
        XkbIndicatorMapNotifyMask    = Cardinal(1) shl  5;
        XkbNamesNotifyMask           = Cardinal(1) shl  6;
        XkbCompatMapNotifyMask       = Cardinal(1) shl  7;
        XkbBellNotifyMask            = Cardinal(1) shl  8;
        XkbActionMessageMask         = Cardinal(1) shl  9;
        XkbAccessXNotifyMask         = Cardinal(1) shl 10;
        XkbExtensionDeviceNotifyMask = Cardinal(1) shl 11;
        XkbAllEventsMask             = $FFF;

    {
      NewKeyboardNotify event details:
    }
const
        XkbNKN_KeycodesMask         = Cardinal(1) shl 0;
        XkbNKN_GeometryMask         = Cardinal(1) shl 1;
        XkbNKN_DeviceIDMask         = Cardinal(1) shl 2;
        XkbAllNewKeyboardEventsMask = $7;

    {
      AccessXNotify event types:
       - The 'what' field of AccessXNotify events reports the
         reason that the event was generated.
                }
const
        XkbAXN_SKPress    = 0;
        XkbAXN_SKAccept   = 1;
        XkbAXN_SKReject   = 2;
        XkbAXN_SKRelease  = 3;
        XkbAXN_BKAccept   = 4;
        XkbAXN_BKReject   = 5;
        XkbAXN_AXKWarning = 6;

   {
      AccessXNotify details:
      - Used as an event detail mask to limit the conditions under which
        AccessXNotify events are reported
                }
const
        XkbAXN_SKPressMask      = Cardinal(1) shl 0;
        XkbAXN_SKAcceptMask     = Cardinal(1) shl 1;
        XkbAXN_SKRejectMask     = Cardinal(1) shl 2;
        XkbAXN_SKReleaseMask    = Cardinal(1) shl 3;
        XkbAXN_BKAcceptMask     = Cardinal(1) shl 4;
        XkbAXN_BKRejectMask     = Cardinal(1) shl 5;
        XkbAXN_AXKWarningMask   = Cardinal(1) shl 6;
        XkbAllAccessXEventsMask = $f;
  {
      State detail mask:
       - The 'changed' field of StateNotify events reports which of
         the keyboard state components have changed.
       - Used as an event detail mask to limit the conditions under
         which StateNotify events are reported.
                }
const
        XkbModifierStateMask      = Cardinal(1) shl  0;
        XkbModifierBaseMask       = Cardinal(1) shl  1;
        XkbModifierLatchMask      = Cardinal(1) shl  2;
        XkbModifierLockMask       = Cardinal(1) shl  3;
        XkbGroupStateMask         = Cardinal(1) shl  4;
        XkbGroupBaseMask          = Cardinal(1) shl  5;
        XkbGroupLatchMask         = Cardinal(1) shl  6;
        XkbGroupLockMask          = Cardinal(1) shl  7;
        XkbCompatStateMask        = Cardinal(1) shl  8;
        XkbGrabModsMask           = Cardinal(1) shl  9;
        XkbCompatGrabModsMask     = Cardinal(1) shl 10;
        XkbLookupModsMask         = Cardinal(1) shl 11;
        XkbCompatLookupModsMask   = Cardinal(1) shl 12;
        XkbPointerButtonMask      = Cardinal(1) shl 13;
        XkbAllStateComponentsMask = $3fff;

   {
      Controls detail masks:
       The controls specified in XkbAllControlsMask:
       - The 'changed' field of ControlsNotify events reports which of
         the keyboard controls have changed.
       - The 'changeControls' field of the SetControls request specifies
         the controls for which values are to be changed.
       - Used as an event detail mask to limit the conditions under
         which ControlsNotify events are reported.

       The controls specified in the XkbAllBooleanCtrlsMask:
       - The 'enabledControls' field of ControlsNotify events reports the
         current status of the boolean controls.
       - The 'enabledControlsChanges' field of ControlsNotify events reports
         any boolean controls that have been turned on or off.
       - The 'affectEnabledControls' and 'enabledControls' fields of the
         kbSetControls request change the set of enabled controls.
       - The 'accessXTimeoutMask' and 'accessXTimeoutValues' fields of
         an XkbControlsRec specify the controls to be changed if the keyboard
         times out and the values to which they should be changed.
       - The 'autoCtrls' and 'autoCtrlsValues' fields of the PerClientFlags
         request specifies the specify the controls to be reset when the
         client exits and the values to which they should be reset.
       - The 'ctrls' field of an indicator map specifies the controls
         that drive the indicator.
       - Specifies the boolean controls affected by the SetControls and
         LockControls key actions.
                }
const
        XkbRepeatKeysMask       = Cardinal(1) shl  0;
        XkbSlowKeysMask         = Cardinal(1) shl  1;
        XkbBounceKeysMask       = Cardinal(1) shl  2;
        XkbStickyKeysMask       = Cardinal(1) shl  3;
        XkbMouseKeysMask        = Cardinal(1) shl  4;
        XkbMouseKeysAccelMask   = Cardinal(1) shl  5;
        XkbAccessXKeysMask      = Cardinal(1) shl  6;
        XkbAccessXTimeoutMask   = Cardinal(1) shl  7;
        XkbAccessXFeedbackMask  = Cardinal(1) shl  8;
        XkbAudibleBellMask      = Cardinal(1) shl  9;
        XkbOverlay1Mask         = Cardinal(1) shl 10;
        XkbOverlay2Mask         = Cardinal(1) shl 11;
        XkbIgnoreGroupLockMask  = Cardinal(1) shl 12;
        XkbGroupsWrapMask       = Cardinal(1) shl 27;
        XkbInternalModsMask     = Cardinal(1) shl 28;
        XkbIgnoreLockModsMask   = Cardinal(1) shl 29;
        XkbPerKeyRepeatMask     = Cardinal(1) shl 30;
        XkbControlsEnabledMask  = Cardinal(1) shl 31;

        XkbAccessXOptionsMask   = XkbStickyKeysMask or XkbAccessXFeedbackMask;

        XkbAllBooleanCtrlsMask  = $00001FFF;
        XkbAllControlsMask      = $F8001FFF;

    {
      Compatibility Map Compontents:
       - Specifies the components to be allocated in XkbAllocCompatMap.
                }
const
        XkbSymInterpMask   = 1 shl 0;
        XkbGroupCompatMask = 1 shl 1;
        XkbAllCompatMask   = $3;

    {
      Assorted constants and limits.
                }
const
        XkbAllIndicatorsMask   = $ffffffff;

    {
      Map components masks:
      Those in AllMapComponentsMask:
       - Specifies the individual fields to be loaded or changed for the
         GetMap and SetMap requests.
      Those in ClientInfoMask:
       - Specifies the components to be allocated by XkbAllocClientMap.
      Those in ServerInfoMask:
       - Specifies the components to be allocated by XkbAllocServerMap.
                }
const
        XkbKeyTypesMask           = 1 shl 0;
        XkbKeySymsMask            = 1 shl 1;
        XkbModifierMapMask        = 1 shl 2;
        XkbExplicitComponentsMask = 1 shl 3;
        XkbKeyActionsMask         = 1 shl 4;
        XkbKeyBehaviorsMask       = 1 shl 5;
        XkbVirtualModsMask        = 1 shl 6;
        XkbVirtualModMapMask      = 1 shl 7;

        XkbAllClientInfoMask      = XkbKeyTypesMask           or XkbKeySymsMask       or XkbModifierMapMask;
        XkbAllServerInfoMask      = XkbExplicitComponentsMask or XkbKeyActionsMask    or XkbKeyBehaviorsMask or
                                    XkbVirtualModsMask        or XkbVirtualModMapMask;
        XkbAllMapComponentsMask   = XkbAllClientInfoMask      or XkbAllServerInfoMask;

    {
      Names component mask:
       - Specifies the names to be loaded or changed for the GetNames and
         SetNames requests.
       - Specifies the names that have changed in a NamesNotify event.
       - Specifies the names components to be allocated by XkbAllocNames.
                }
const
        XkbKeycodesNameMask    = 1 shl  0;
        XkbGeometryNameMask    = 1 shl  1;
        XkbSymbolsNameMask     = 1 shl  2;
        XkbPhysSymbolsNameMask = 1 shl  3;
        XkbTypesNameMask       = 1 shl  4;
        XkbCompatNameMask      = 1 shl  5;
        XkbKeyTypeNamesMask    = 1 shl  6;
        XkbKTLevelNamesMask    = 1 shl  7;
        XkbIndicatorNamesMask  = 1 shl  8;
        XkbKeyNamesMask        = 1 shl  9;
        XkbKeyAliasesMask      = 1 shl 10;
        XkbVirtualModNamesMask = 1 shl 11;
        XkbGroupNamesMask      = 1 shl 12;
        XkbRGNamesMask         = 1 shl 13;
        XkbComponentNamesMask  = $3f;
        XkbAllNamesMask        = $3fff;

  {
      Miscellaneous event details:
      - event detail masks for assorted events that don't reall
        have any details.
                }
const
        XkbAllStateEventsMask     = XkbAllStateComponentsMask;
        XkbAllMapEventsMask       = XkbAllMapComponentsMask;
        XkbAllControlEventsMask   = XkbAllControlsMask;
        XkbAllIndicatorEventsMask = XkbAllIndicatorsMask;
        XkbAllNameEventsMask      = XkbAllNamesMask;
        XkbAllCompatMapEventsMask = XkbAllCompatMask;
        XkbAllBellEventsMask      = Cardinal(1) shl 0;
        XkbAllActionMessagesMask  = Cardinal(1) shl 0;

    {
      XKB reports one error:  BadKeyboard
      A further reason for the error is encoded into to most significant
      byte of the resourceID for the error:
         XkbErr_BadDevice - the device in question was not found
         XkbErr_BadClass  - the device was found but it doesn't belong to
                            the appropriate class.
         XkbErr_BadId     - the device was found and belongs to the right
                            class, but not feedback with a matching id was
                            found.
      The low byte of the resourceID for this error contains the device
      id, class specifier or feedback id that failed.
                }
const
        XkbKeyboard      =   0;
        XkbNumberErrors  =   1;
        XkbErr_BadDevice = $ff;
        XkbErr_BadClass  = $fe;
        XkbErr_BadId     = $fd;

    {
      Keyboard Components Mask:
      - Specifies the components that follow a GetKeyboardByNameReply
                }
const
        XkbClientMapMask     = Cardinal(1) shl 0;
        XkbServerMapMask     = Cardinal(1) shl 1;
        XkbCompatMapMask     = Cardinal(1) shl 2;
        XkbIndicatorMapMask  = Cardinal(1) shl 3;
        XkbNamesMask         = Cardinal(1) shl 4;
        XkbGeometryMask      = Cardinal(1) shl 5;
        XkbControlsMask      = Cardinal(1) shl 6;
        XkbAllComponentsMask = $7f;


    {
      AccessX Options Mask
       - The 'accessXOptions' field of an XkbControlsRec specifies the
         AccessX options that are currently in effect.
       - The 'accessXTimeoutOptionsMask' and 'accessXTimeoutOptionsValues'
         fields of an XkbControlsRec specify the Access X options to be
         changed if the keyboard times out and the values to which they
         should be changed.
                }
const
        XkbAX_SKPressFBMask    = Cardinal(1) shl  0;
        XkbAX_SKAcceptFBMask   = Cardinal(1) shl  1;
        XkbAX_FeatureFBMask    = Cardinal(1) shl  2;
        XkbAX_SlowWarnFBMask   = Cardinal(1) shl  3;
        XkbAX_IndicatorFBMask  = Cardinal(1) shl  4;
        XkbAX_StickyKeysFBMask = Cardinal(1) shl  5;
        XkbAX_TwoKeysMask      = Cardinal(1) shl  6;
        XkbAX_LatchToLockMask  = Cardinal(1) shl  7;
        XkbAX_SKReleaseFBMask  = Cardinal(1) shl  8;
        XkbAX_SKRejectFBMask   = Cardinal(1) shl  9;
        XkbAX_BKRejectFBMask   = Cardinal(1) shl 10;
        XkbAX_DumbBellFBMask   = Cardinal(1) shl 11;
        XkbAX_FBOptionsMask    = $F3F;
        XkbAX_SKOptionsMask    = $0C0;
        XkbAX_AllOptionsMask   = $FFF;

    {
      XkbUseCoreKbd is used to specify the core keyboard without having
                        to look up its X input extension identifier.
      XkbUseCorePtr is used to specify the core pointer without having
                        to look up its X input extension identifier.
      XkbDfltXIClass is used to specify "don't care" any place that the
                        XKB protocol is looking for an X Input Extension
                        device class.
      XkbDfltXIId is used to specify "don't care" any place that the
                        XKB protocol is looking for an X Input Extension
                        feedback identifier.
      XkbAllXIClasses is used to get information about all device indicators,
                        whether they're part of the indicator feedback class
                        or the keyboard feedback class.
      XkbAllXIIds is used to get information about all device indicator
                        feedbacks without having to list them.
      XkbXINone is used to indicate that no class or id has been specified.
      XkbLegalXILedClass(c)  True if 'c' specifies a legal class with LEDs
      XkbLegalXIBellClass(c) True if 'c' specifies a legal class with bells
      XkbExplicitXIDevice(d) True if 'd' explicitly specifies a device
      XkbExplicitXIClass(c)  True if 'c' explicitly specifies a device class
      XkbExplicitXIId(c)     True if 'i' explicitly specifies a device id
      XkbSingleXIClass(c)    True if 'c' specifies exactly one device class,
                             including the default.
      XkbSingleXIId(i)       True if 'i' specifies exactly one device
                              identifier, including the default.
                }
const
        XkbUseCoreKbd   = $0100;
        XkbUseCorePtr   = $0200;
        XkbDfltXIClass  = $0300;
        XkbDfltXIId     = $0400;
        XkbAllXIClasses = $0500;
        XkbAllXIIds     = $0600;
        XkbXINone       = $ff00;

function XkbLegalXILedClass  (c : Cardinal) : LongBool;
function XkbLegalXIBellClass (c : Cardinal) : LongBool;
function XkbExplicitXIDevice (c : Cardinal) : LongBool;
function XkbExplicitXIClass  (c : Cardinal) : LongBool;
function XkbExplicitXIId     (c : Cardinal) : LongBool;
function XkbSingleXIClass    (c : Cardinal) : LongBool;
function XkbSingleXIId       (c : Cardinal) : LongBool;

const
        XkbNoModifier         = $ff;
        XkbNoShiftLevel       = $ff;
        XkbNoShape            = $ff;
        XkbNoIndicator        = $ff;

        XkbNoModifierMask     = 0;
        XkbAllModifiersMask   = $ff;
        XkbAllVirtualModsMask = $ffff;

        XkbNumKbdGroups       = 4;
        XkbMaxKbdGroup        = XkbNumKbdGroups-1;

        XkbMaxMouseKeysBtn    = 4;

                {
      Group Index and Mask:
       - Indices into the kt_index array of a key type.
       - Mask specifies types to be changed for XkbChangeTypesOfKey
    }
const
        XkbGroup1Index   =   0;
        XkbGroup2Index   =   1;
        XkbGroup3Index   =   2;
        XkbGroup4Index   =   3;
        XkbAnyGroup      = 254;
        XkbAllGroups     = 255;

        XkbGroup1Mask    = 1 shl  0;
        XkbGroup2Mask    = 1 shl  1;
        XkbGroup3Mask    = 1 shl  2;
        XkbGroup4Mask    = 1 shl  3;
        XkbAnyGroupMask  = 1 shl  7;
        XkbAllGroupsMask = $f;

    {
      BuildCoreState: Given a keyboard group and a modifier state,
                      construct the value to be reported an event.
      GroupForCoreState:  Given the state reported in an event,
                      determine the keyboard group.
      IsLegalGroup:   Returns TRUE if 'g' is a valid group index.
                }
function XkbBuildCoreState    (m, g : Cardinal) : Cardinal;
function XkbGroupForCoreState (s    : Cardinal) : Cardinal;
function XkbIsLegalGroup      (g    : Cardinal) : LongBool;

    {
      GroupsWrap values:
       - The 'groupsWrap' field of an XkbControlsRec specifies the
         treatment of out of range groups.
       - Bits 6 and 7 of the group info field of a key symbol map
         specify the interpretation of out of range groups for the
         corresponding key.
                }
const
        XkbWrapIntoRange     = $00;
        XkbClampIntoRange    = $40;
        XkbRedirectIntoRange = $80;

    {
      Action flags:  Reported in the 'flags' field of most key actions.
      Interpretation depends on the type of the action; not all actions
      accept all flags.

      Option                    Used for Actions
      ------                    ----------------
      ClearLocks                SetMods, LatchMods, SetGroup, LatchGroup
      LatchToLock               SetMods, LatchMods, SetGroup, LatchGroup
      LockNoLock                LockMods, ISOLock, LockPtrBtn, LockDeviceBtn
      LockNoUnlock              LockMods, ISOLock, LockPtrBtn, LockDeviceBtn
      UseModMapMods             SetMods, LatchMods, LockMods, ISOLock
      GroupAbsolute             SetGroup, LatchGroup, LockGroup, ISOLock
      UseDfltButton             PtrBtn, LockPtrBtn
      NoAcceleration            MovePtr
      MoveAbsoluteX             MovePtr
      MoveAbsoluteY             MovePtr
      ISODfltIsGroup            ISOLock
      ISONoAffectMods           ISOLock
      ISONoAffectGroup          ISOLock
      ISONoAffectPtr            ISOLock
      ISONoAffectCtrls          ISOLock
      MessageOnPress            ActionMessage
      MessageOnRelease          ActionMessage
      MessageGenKeyEvent        ActionMessage
      AffectDfltBtn             SetPtrDflt
      DfltBtnAbsolute           SetPtrDflt
      SwitchApplication SwitchScreen
      SwitchAbsolute            SwitchScreen
                }
const
        XkbSA_ClearLocks         = Cardinal(1) shl 0;
        XkbSA_LatchToLock        = Cardinal(1) shl 1;

        XkbSA_LockNoLock         = Cardinal(1) shl 0;
        XkbSA_LockNoUnlock       = Cardinal(1) shl 1;

        XkbSA_UseModMapMods      = Cardinal(1) shl 2;

        XkbSA_GroupAbsolute      = Cardinal(1) shl 2;
        XkbSA_UseDfltButton      = 0;

        XkbSA_NoAcceleration     = Cardinal(1) shl 0;
        XkbSA_MoveAbsoluteX      = Cardinal(1) shl 1;
        XkbSA_MoveAbsoluteY      = Cardinal(1) shl 2;

        XkbSA_ISODfltIsGroup     = Cardinal(1) shl 7;
        XkbSA_ISONoAffectMods    = Cardinal(1) shl 6;
        XkbSA_ISONoAffectGroup   = Cardinal(1) shl 5;
        XkbSA_ISONoAffectPtr     = Cardinal(1) shl 4;
        XkbSA_ISONoAffectCtrls   = Cardinal(1) shl 3;
        XkbSA_ISOAffectMask      = $78;

        XkbSA_MessageOnPress     = Cardinal(1) shl 0;
        XkbSA_MessageOnRelease   = Cardinal(1) shl 1;
        XkbSA_MessageGenKeyEvent = Cardinal(1) shl 2;

        XkbSA_AffectDfltBtn      = 1;
        XkbSA_DfltBtnAbsolute    = Cardinal(1) shl 2;

        XkbSA_SwitchApplication  = Cardinal(1) shl 0;
        XkbSA_SwitchAbsolute     = Cardinal(1) shl 2;

    {
      The following values apply to the SA_DeviceValuator
      action only.  Valuator operations specify the action
      to be taken.   Values specified in the action are
      multiplied by 2^scale before they are applied.
                }
const
        XkbSA_IgnoreVal      = $00;
        XkbSA_SetValMin      = $10;
        XkbSA_SetValCenter   = $20;
        XkbSA_SetValMax      = $30;
        XkbSA_SetValRelative = $40;
        XkbSA_SetValAbsolute = $50;
        XkbSA_ValOpMask      = $70;
        XkbSA_ValScaleMask   = $07;

function XkbSA_ValOp    (a : Cardinal) : Cardinal;
function XkbSA_ValScale (a : Cardinal) : Cardinal;

    {
      Action types: specifies the type of a key action.  Reported in the
      type field of all key actions.
                }
const
        XkbSA_NoAction        = $00;
        XkbSA_SetMods         = $01;
        XkbSA_LatchMods       = $02;
        XkbSA_LockMods        = $03;
        XkbSA_SetGroup        = $04;
        XkbSA_LatchGroup      = $05;
        XkbSA_LockGroup       = $06;
        XkbSA_MovePtr         = $07;
        XkbSA_PtrBtn          = $08;
        XkbSA_LockPtrBtn      = $09;
        XkbSA_SetPtrDflt      = $0a;
        XkbSA_ISOLock         = $0b;
        XkbSA_Terminate       = $0c;
        XkbSA_SwitchScreen    = $0d;
        XkbSA_SetControls     = $0e;
        XkbSA_LockControls    = $0f;
        XkbSA_ActionMessage   = $10;
        XkbSA_RedirectKey     = $11;
        XkbSA_DeviceBtn       = $12;
        XkbSA_LockDeviceBtn   = $13;
        XkbSA_DeviceValuator  = $14;
        XkbSA_LastAction      = XkbSA_DeviceValuator;
        XkbSA_NumActions      = XkbSA_LastAction +1;

{$ifDef XF86DDXACTIONS}
        {$Define XF86}
{$ELSE}
        {$IFDEF XFree86Server}
                 {$DEFINE XF86}
        {$endif}
{$ENDIF}

{$IFDEF XF86}
        XkbSA_XFree86Private  = $86;
{$Endif}

   {
      Specifies the key actions that clear latched groups or modifiers.
                }
const
{#define        XkbSA_BreakLatch \
        ((1<<XkbSA_NoAction)|(1<<XkbSA_PtrBtn)|(1<<XkbSA_LockPtrBtn)|\
        (1<<XkbSA_Terminate)|(1<<XkbSA_SwitchScreen)|(1<<XkbSA_SetControls)|\
        (1<<XkbSA_LockControls)|(1<<XkbSA_ActionMessage)|\
        (1<<XkbSA_RedirectKey)|(1<<XkbSA_DeviceBtn)|(1<<XkbSA_LockDeviceBtn))
}
        XkbSA_BreakLatch = (1 shl XkbSA_PtrBtn)        or (1 shl XkbSA_LockPtrBtn)  or (1 shl XkbSA_Terminate)
                           or
                           (1 shl XkbSA_SwitchScreen)  or (1 shl XkbSA_SetControls) or (1 shl XkbSA_LockControls)
                                                                                 or
                                                                                 (1 shl XkbSA_ActionMessage) or (1 shl XkbSA_RedirectKey) or (1 shl XkbSA_DeviceBtn)
                                                                                 or
                                                                                 (1 shl XkbSA_LockDeviceBtn);

    {
      Key Behavior Qualifier:
         KB_Permanent indicates that the behavior describes an unalterable
         characteristic of the keyboard, not an XKB software-simulation of
         the listed behavior.
      Key Behavior Types:
         Specifies the behavior of the underlying key.
                }
const
        XkbKB_Permanent   = $80;
        XkbKB_OpMask      = $7f;

        XkbKB_Default     = $00;
        XkbKB_Lock        = $01;
        XkbKB_RadioGroup  = $02;
        XkbKB_Overlay1    = $03;
        XkbKB_Overlay2    = $04;

        XkbKB_RGAllowNone = $80;

    {
      Various macros which describe the range of legal keycodes.
                }
const
        XkbMinLegalKeyCode    =   8;
        XkbMaxLegalKeyCode    = 255;
        XkbMaxKeyCount        = XkbMaxLegalKeyCode - XkbMinLegalKeyCode +1;
        XkbPerKeyBitArraySize = (XkbMaxLegalKeyCode + 1) div 8;

function XkbIsLegalKeycode (const k : Cardinal) : LongBool;

type
        PXkbControlsPtr = ^TXkbControlsRec;
        TXkbControlsRec = record
                           mk_dflt_btn      : Byte;
                           num_groups       : Byte;
                           groups_wrap      : Byte;
                           internal         : TXkbModsRec;
                           ignore_lock      : TXkbModsRec;
                           enabled_ctrls    : Word;
                           repeat_delay     : Word;
                           repeat_interval  : Word;
                           slow_keys_delay  : Word;
                           debounce_delay   : Word;
                           mk_delay         : Word;
                           mk_interval      : Word;
                           mk_time_to_max   : Word;
                           mk_max_speed     : Word;
                           mk_curve         : SmallInt;
                           ax_options       : Word;
                           ax_timeout       : Word;
                           axt_opts_mask    : Word;
                           axt_opts_values  : Word;
                                                                                 axt_ctrls_mask   : Word;
                                                                                 axt_ctrls_values : Word;
                                                                                 per_key_repeat   : array [0..XkbPerKeyBitArraySize -1] of Byte;
                          end;

function XkbAX_AnyFeedback  (c : PXkbControlsPtr)           : Word;
function XkbAX_NeedOption   (c : PXkbControlsPtr; w : Word) : Word;
function XkbAX_NeedFeedback (c : PXkbControlsPtr; w : Word) : Boolean;

    {
      Assorted constants and limits.
                }
const
        XkbNumModifiers        =   8;
        XkbNumVirtualMods      =  16;
        XkbNumIndicators       =  32;

        XkbMaxRadioGroups      =  32;
        XkbAllRadioGroupsMask  = $ffffffff;
        XkbMaxShiftLevel       =  63;
        XkbMaxSymsPerKey       = XkbMaxShiftLevel * XkbNumKbdGroups;
        XkbRGMaxMembers        =  12;
        XkbActionMessageLength =   6;
        XkbKeyNameLength       =   4;
        XkbMaxRedirectCount    =   8;

        XkbGeomPtsPerMM        =  10;
        XkbGeomMaxColors       =  32;
        XkbGeomMaxLabelColors  =   3;
        XkbGeomMaxPriority     = 255;

type
        PXkbServerMapPtr = ^TXkbServerMapRec;
        TXkbServerMapRec = record
                            num_acts   : Word;
                                                                                        size_acts  : Word;
                            acts       : PXkbAction;
                            behaviors  : PXkbBehavior;
                            key_acts   : PWord;
                                                                                {$IF defined (__cplusplus) or defined (c_plusplus)}
                                                                                        //explicit is a C++ reserved word
                            c_explicit : PByte;
                                                                                {$ELSE}
                            explicit   : PByte;
                          {$ENDIF}
                                                                                        vmods      : array [0..XkbNumVirtualMods -1] of Byte;
                            vmodmap    : PWord;
                           end;

function XkbSMKeyActionsPtr (m : PXkbServerMapPtr; k : Word) : PXkbAction;

        {
          Structures and access macros used primarily by clients
        }

type
        PXkbSymMapPtr = ^TXkbSymMapRec;
        TXkbSymMapRec = record
                         kt_index   : array [0..XkbNumKbdGroups -1] of Byte;
                         group_info : Byte;
                         width      : Byte;
                         offset     : Byte;
                                                                        end;

type
        PXkbClientMapPtr = ^TXkbClientMapRec;
        TXkbClientMapRec = record
                            size_types  : Byte;
                            num_types   : Byte;
                            types       : PXkbKeyTypePtr;

                            size_syms   : Word;
                            num_syms    : Word;
                            syms        : PKeySym;
                            key_sym_map : PXkbSymMapPtr;

                            modmap      : PByte;
                           end;

function XkbCMKeyGroupInfo   (m : PXkbClientMapPtr; k : Word)           : Byte;
function XkbCMKeyNumGroups   (m : PXkbClientMapPtr; k : Word)           : Byte;
function XkbCMKeyGroupWidth  (m : PXkbClientMapPtr; k : Word; g : Byte) : Byte;
function XkbCMKeyGroupsWidth (m : PXkbClientMapPtr; k : Word)           : Byte;
function XkbCMKeyTypeIndex   (m : PXkbClientMapPtr; k : Word; g : Byte) : Byte;
function XkbCMKeyType        (m : PXkbClientMapPtr; k : Word; g : Byte) : PXkbKeyTypePtr;
function XkbCMKeyNumSyms     (m : PXkbClientMapPtr; k : Word)           : Word;
function XkbCMKeySymsOffset  (m : PXkbClientMapPtr; k : Word)           : Byte;
function XkbCMKeySymsPtr     (m : PXkbClientMapPtr; k : Word)           : PKeySym;

        {
          Compatibility structures and access macros
        }
type
        PXkbSymInterpretPtr = ^TXkbSymInterpretRec;
        TXkbSymInterpretRec = record
                               sym         : TKeySym;
                               flags       : Byte;
                               match       : Byte;
                               mods        : Byte;
                               virtual_mod : Byte;
                               act         : TXkbAnyAction;
                              end;

type
        PXkbCompatMapPtr = ^TXkbCompatMapRec;
        TXkbCompatMapRec = record
                            sym_interpret : PXkbSymInterpretPtr;
                            groups        : array [0..XkbNumKbdGroups -1] of TXkbModsRec;
                            num_si        : Word;
                            size_si       : Word;
                           end;

type
        PXkbIndicatorMapPtr = ^TXkbIndicatorMapRec;
        TXkbIndicatorMapRec = record
                               flags        : Byte;
                               which_groups : Byte;
                               groups       : Byte;
                               which_mods   : Byte;
                                                                                                 mods         : TXkbModsRec;
                               ctrls        : Word;
                              end;

function XkbIM_IsAuto (i : PXkbIndicatorMapPtr) : Boolean;
function XkbIM_InUse  (i : PXkbIndicatorMapPtr) : Boolean;

type
        PXkbIndicatorPtr = ^TXkbIndicatorRec;
        TXkbIndicatorRec = record
                            phys_indicators : LongWord;
                            maps            : array [0..XkbNumIndicators -1] of TXkbIndicatorMapRec;
                           end;

type
        PXkbKeyNamePtr = ^TXkbKeyNameRec;
        TXkbKeyNameRec = record
                          name : array [0..XkbKeyNameLength -1] of Char;
                         end;

type
        PXkbKeyAliasPtr = ^TXkbKeyAliasRec;
        TXkbKeyAliasRec = record
                                                                                 real  : array [0..XkbKeyNameLength -1] of Char;
                           alias : array [0..XkbKeyNameLength -1] of Char;
                          end;

        {
          Names for everything
        }
type
        PXkbNamesPtr = ^TXkbNamesRec;
        TXkbNamesRec = record
                        keycodes        : TAtom;
                        geometry        : TAtom;
                        symbols         : TAtom;
                                                                        types           : TAtom;
                        compat          : TAtom;
                        vmods           : Array [0..XkbNumVirtualMods -1] of TAtom;
                        indicators      : array [0..XkbNumIndicators  -1] of TAtom;
                                                                        groups          : array [0..XkbNumKbdGroups   -1] of TAtom;
                        keys            : PXkbKeyNamePtr;
                        key_aliases     : PXkbKeyAliasPtr;
                        radio_groups    : PAtom;
                        phys_symbols    : TAtom;
                        num_keys        : Byte;
                                                                        num_key_aliases : Byte;
                                                                        num_rg          : Word;
                       end;

    {
      Key Type index and mask for the four standard key types.
                }
const
        XkbOneLevelIndex    =   0;
        XkbTwoLevelIndex    =   1;
        XkbAlphabeticIndex  =   2;
        XkbKeypadIndex      =   3;
        XkbLastRequiredType = XkbKeypadIndex;
        XkbNumRequiredTypes = XkbLastRequiredType + 1;
        XkbMaxKeyTypes      = 255;

        XkbOneLevelMask     = 1 shl 0;
        XkbTwoLevelMask     = 1 shl 1;
        XkbAlphabeticMask   = 1 shl 2;
        XkbKeypadMask       = 1 shl 3;
        XkbAllRequiredTypes = $f;

function XkbShiftLevel     (n : Byte) : Byte;
function XkbShiftLevelMask (n : Byte) : Byte;

    {
      Extension name and version information
                }
{$IFDEF MACRO}
        {$DEFINE XkbName         := 'XKEYBOARD'}
        {$DEFINE XkbMajorVersion := 1}
        {$DEFINE XkbMinorVersion := 0}
{$ELSE}
const
        XkbName         = 'XKEYBOARD';
        XkbMajorVersion = 1;
        XkbMinorVersion = 0;
{$ENDIF}

    {
      Explicit map components:
       - Used in the 'explicit' field of an XkbServerMap.  Specifies
         the keyboard components that should _not_ be updated automatically
         in response to core protocol keyboard mapping requests.
                }
const
        XkbExplicitKeyTypesMask   = $0f;
        XkbExplicitKeyType1Mask   = 1 shl 0;
        XkbExplicitKeyType2Mask   = 1 shl 1;
        XkbExplicitKeyType3Mask   = 1 shl 2;
        XkbExplicitKeyType4Mask   = 1 shl 3;
        XkbExplicitInterpretMask  = 1 shl 4;
        XkbExplicitAutoRepeatMask = 1 shl 5;
        XkbExplicitBehaviorMask   = 1 shl 6;
        XkbExplicitVModMapMask    = 1 shl 7;
        XkbAllExplicitMask        = $ff;

    {
      Symbol interpretations flags:
       - Used in the flags field of a symbol interpretation
                }
const
        XkbSI_AutoRepeat = 1 shl 0;
        XkbSI_LockingKey = 1 shl 1;

    {
      Symbol interpretations match specification:
       - Used in the match field of a symbol interpretation to specify
         the conditions under which an interpretation is used.
                }
const
        XkbSI_LevelOneOnly = $80;
        XkbSI_OpMask       = $7f;
        XkbSI_NoneOf       = 0;
        XkbSI_AnyOfOrNone  = 1;
        XkbSI_AnyOf        = 2;
        XkbSI_AllOf        = 3;
        XkbSI_Exactly      = 4;

    {
      Indicator map flags:
       - Used in the flags field of an indicator map to indicate the
         conditions under which and indicator can be changed and the
         effects of changing the indicator.
                }
const
        XkbIM_NoExplicit  = Cardinal(1) shl 7;
        XkbIM_NoAutomatic = Cardinal(1) shl 6;
        XkbIM_LEDDrivesKB = Cardinal(1) shl 5;

    {
      Indicator map component specifications:
       - Used by the 'which_groups' and 'which_mods' fields of an indicator
         map to specify which keyboard components should be used to drive
         the indicator.
                }
const
        XkbIM_UseBase      = Cardinal(1) shl 0;
        XkbIM_UseLatched   = Cardinal(1) shl 1;
        XkbIM_UseLocked    = Cardinal(1) shl 2;
        XkbIM_UseEffective = Cardinal(1) shl 3;
        XkbIM_UseCompat    = Cardinal(1) shl 4;

        XkbIM_UseNone      = 0;
        XkbIM_UseAnyGroup  = XkbIM_UseBase     or XkbIM_UseLatched or XkbIM_UseLocked or XkbIM_UseEffective;
        XkbIM_UseAnyMods   = XkbIM_UseAnyGroup or XkbIM_UseCompat;

    {
      GetByName components:
       - Specifies desired or necessary components to GetKbdByName request.
       - Reports the components that were found in a GetKbdByNameReply
                }
const
        XkbGBN_TypesMask         = Cardinal(1) shl 0;
        XkbGBN_CompatMapMask     = Cardinal(1) shl 1;
        XkbGBN_ClientSymbolsMask = Cardinal(1) shl 2;
        XkbGBN_ServerSymbolsMask = Cardinal(1) shl 3;
        XkbGBN_SymbolsMask       = XkbGBN_ClientSymbolsMask or XkbGBN_ServerSymbolsMask;
        XkbGBN_IndicatorMapMask  = Cardinal(1) shl 4;
        XkbGBN_KeyNamesMask      = Cardinal(1) shl 5;
        XkbGBN_GeometryMask      = Cardinal(1) shl 6;
        XkbGBN_OtherNamesMask    = Cardinal(1) shl 7;
        XkbGBN_AllComponentsMask = $ff;

     {
       ListComponents flags
                        }
const
        XkbLC_Hidden           = Cardinal(1) shl  0;
        XkbLC_Default          = Cardinal(1) shl  1;
        XkbLC_Partial          = Cardinal(1) shl  2;

        XkbLC_AlphanumericKeys = Cardinal(1) shl  8;
        XkbLC_ModifierKeys     = Cardinal(1) shl  9;
        XkbLC_KeypadKeys       = Cardinal(1) shl 10;
        XkbLC_FunctionKeys     = Cardinal(1) shl 11;
        XkbLC_AlternateGroup   = Cardinal(1) shl 12;

    {
      X Input Extension Interactions
      - Specifies the possible interactions between XKB and the X input
        extension
      - Used to request (XkbGetDeviceInfo) or change (XKbSetDeviceInfo)
        XKB information about an extension device.
      - Reports the list of supported optional features in the reply to
        XkbGetDeviceInfo or in an XkbExtensionDeviceNotify event.
      XkbXI_UnsupportedFeature is reported in XkbExtensionDeviceNotify
      events to indicate an attempt to use an unsupported feature.
                }
const
        XkbXI_KeyboardsMask             = Cardinal(1) shl  0;
        XkbXI_ButtonActionsMask         = Cardinal(1) shl  1;
        XkbXI_IndicatorNamesMask        = Cardinal(1) shl  2;
        XkbXI_IndicatorMapsMask         = Cardinal(1) shl  3;
        XkbXI_IndicatorStateMask        = Cardinal(1) shl  4;
        XkbXI_UnsupportedFeatureMask    = Cardinal(1) shl 15;
        XkbXI_AllFeaturesMask           = $001f;
        XkbXI_AllDeviceFeaturesMask     = $001e;

        XkbXI_IndicatorsMask            = $001c;
        XkbAllExtensionDeviceEventsMask = $801f;

    {
      Per-Client Flags:
       - Specifies flags to be changed by the PerClientFlags request.
                }
const
        XkbPCF_DetectableAutoRepeatMask = Cardinal(1) shl 0;
        XkbPCF_GrabsUseXKBStateMask     = Cardinal(1) shl 1;
        XkbPCF_AutoResetControlsMask    = Cardinal(1) shl 2;
        XkbPCF_LookupStateWhenGrabbed   = Cardinal(1) shl 3;
        XkbPCF_SendEventUsesXKBState    = Cardinal(1) shl 4;
        XkbPCF_AllFlagsMask             = $1F;

    {
      Debugging flags and controls
                }
const
        XkbDF_DisableLocks = 1 shl 0;

type
        PXkbPropertyPtr = ^TXkbPropertyRec;
        TXkbPropertyRec = record
                           name  : PChar;
                           value : PChar;
                          end;

type
        PXkbColorPtr = ^TXkbColorRec;
        TXkbColorRec = record
                        pixel : Word;
                        spec  : PChar;
                       end;

type
        PXkbPointPtr = ^TXkbPointRec;
        TXkbPointRec = record
                        x : SmallInt;
                        y : SmallInt;
                       end;

type
        PXkbBoundsPtr = ^TXkbBoundsRec;
        TXkbBoundsRec = record
                         x1 : SmallInt;
                         y1 : SmallInt;
                         x2 : SmallInt;
                         y2 : SmallInt;
                        end;

function XkbBoundsWidth  (b : PXkbBoundsPtr) : SmallInt;
function XkbBoundsHeight (b : PXkbBoundsPtr) : SmallInt;

type
        PXkbOutlinePtr = ^TXkbOutlineRec;
        TXkbOutlineRec = record
                          num_points    : Word;
                          sz_points     : Word;
                          corner_radius : Word;
                          points        : PXkbPointPtr;
                         end;

type
        PXkbShapePtr = ^TXkbShapeRec;
        TXkbShapeRec = record
                        name         : TAtom;
                        num_outlines : Word;
                        sz_outlines  : Word;
                        outlines     : PXkbOutlinePtr;
                                                                        approx       : PXkbOutlinePtr;
                        primary      : PXkbOutlinePtr;
                                                                        bounds       : TXkbBoundsRec;
                       end;

function XkbOutlineIndex (s : PXkbShapePtr; o : PXkbOutlinePtr) : longint;

type
        PXkbShapeDoodadPtr = ^TXkbShapeDoodadRec;
        TXkbShapeDoodadRec = record
                              name      : TAtom;
                              _type     : Byte;
                              priority  : Byte;
                              top       : SmallInt;
                                                                                                left      : SmallInt;
                              angle     : SmallInt;
                              color_ndx : Word;
                                                                                                shape_ndx : Word;
                             end;

type
        PXkbTextDoodadPtr = ^TXkbTextDoodadRec;
        TXkbTextDoodadRec = record
                             name      : TAtom;
                             _type     : Byte;
                             priority  : Byte;
                             top       : SmallInt;
                             left      : SmallInt;
                             angle     : SmallInt;
                             width     : SmallInt;
                             height    : SmallInt;
                             color_ndx : Word;
                             text      : PChar;
                             font      : PChar;
                            end;

type
        PXkbIndicatorDoodadPtr = ^TXkbIndicatorDoodadRec;
        TXkbIndicatorDoodadRec = record
                                  name          : TAtom;
                                  _type         : Byte;
                                  priority      : Byte;
                                  top           : SmallInt;
                                  left          : SmallInt;
                                  angle         : SmallInt;
                                  shape_ndx     : Word;
                                  on_color_ndx  : Word;
                                  off_color_ndx : Word;
                                 end;

type
        PXkbLogoDoodadPtr = ^TXkbLogoDoodadRec;
        TXkbLogoDoodadRec = record
                                                                                         name      : TAtom;
                             _type     : Byte;
                             priority  : Byte;
                             top       : SmallInt;
                             left      : SmallInt;
                             angle     : SmallInt;
                             color_ndx : Word;
                             shape_ndx : Word;
                             logo_name : PChar;
                            end;

type
        PXkbAnyDoodadPtr = ^TXkbAnyDoodadRec;
        TXkbAnyDoodadRec = record
                            name     : TAtom;
                            _type    : Byte;
                            priority : Byte;
                            top      : SmallInt;
                            left     : SmallInt;
                            angle    : SmallInt;
                           end;

type
        PXkbDoodadPtr = ^TXkbDoodadRec;
        TXkbDoodadRec = record
                         any       : TXkbAnyDoodadRec;
                         shape     : TXkbShapeDoodadRec;
                         text      : TXkbTextDoodadRec;
                         indicator : TXkbIndicatorDoodadRec;
                         logo      : TXkbLogoDoodadRec;
                        end;

const
        XkbUnknownDoodad   = 0;
        XkbOutlineDoodad   = 1;
        XkbSolidDoodad     = 2;
        XkbTextDoodad      = 3;
        XkbIndicatorDoodad = 4;
        XkbLogoDoodad      = 5;

type
        PXkbKeyPtr = ^TXkbKeyRec;
        TXkbKeyRec = record
                      name      : TXkbKeyNameRec;
                      gap       : SmallInt;
                      shape_ndx : Byte;
                      color_ndx : Byte;
                     end;

type
        PXkbRowPtr = ^TXkbRowRec;
        TXkbRowRec = record
                      top      : SmallInt;
                      left     : SmallInt;
                                                                num_keys : Word;
                      sz_keys  : Word;
                      vertical : SmallInt;
                      Keys     : PXkbKeyPtr;
                      bounds   : TXkbBoundsRec;
                     end;

type
        PXkbOverlayPtr = ^TXkbOverlayRec; //forward for TXkbSectionRec use.

//Do not add more "type"
        PXkbSectionPtr = ^TXkbSectionRec;
        TXkbSectionRec = record
                          name         : TAtom;
                          priority     : Byte;
                          top          : SmallInt;
                          left         : SmallInt;
                          width        : Word;
                          height       : Word;
                          angle        : SmallInt;
                          num_rows     : Word;
                          num_doodads  : Word;
                          num_overlays : Word;
                          rows         : PXkbRowPtr;
                                                                                doodads      : PXkbDoodadPtr;
                          bounds       : TXkbBoundsRec;
                          overlays     : PXkbOverlayPtr;
                         end;

//Do not add more "type"
        PXkbOverlayKeyPtr = ^TXkbOverlayKeyRec;
        TXkbOverlayKeyRec = record
                             over  : TXkbKeyNameRec;
                             under : TXkbKeyNameRec;
                            end;

//Do not add more "type"
        PXkbOverlayRowPtr = ^TXkbOverlayRowRec;
        TXkbOverlayRowRec = record
                             row_under : SmallInt;
                             num_keys  : SmallInt;
                             sz_keys   : SmallInt;
                             keys      : PXkbOverlayKeyPtr;
                            end;

//Do not add more "type"
        TXkbOverlayRec = record
                          name          : TAtom;
                          section_under : PXkbSectionPtr;
                          num_rows      : Word;
                          sz_rows       : Word;
                          rows          : PXkbOverlayRowPtr;
                          bounds        : PXkbBoundsPtr;
                         end;

type
        PXkbGeometryRec = ^TXkbGeometryRec;
        PXkbGeometryPtr = PXkbGeometryRec;
        TXkbGeometryRec = record
                           name            : TAtom;
                           width_mm        : Word;
                                                                                 height_mm       : Word;
                           label_font      : PChar;
                           label_color     : PXkbColorPtr;
                           base_color      : PXkbColorPtr;
                           sz_properties   : Word;
                           sz_colors       : Word;
                           sz_shapes       : Word;
                           sz_sections     : Word;
                           sz_doodads      : Word;
                                                                                 sz_key_aliases  : Word;
                           num_properties  : Word;
                           num_colors      : Word;
                           num_shapes      : Word;
                                                                                 num_sections    : Word;
                                                                                 num_doodads     : Word;
                                                                                 num_key_aliases : Word;
                                                                                 properties      : PXkbPropertyPtr;
                                                                                 colors          : PXkbColorPtr;
                                                                                 shapes          : PXkbShapePtr;
                                                                                 sections        : PXkbSectionPtr;
                                                                                 key_aliases     : PXkbKeyAliasPtr;
                          end;

const
        XkbGeomPropertiesMask = 1 shl 0;
        XkbGeomColorsMask     = 1 shl 1;
        XkbGeomShapesMask     = 1 shl 2;
        XkbGeomSectionsMask   = 1 shl 3;
        XkbGeomDoodadsMask    = 1 shl 4;
        XkbGeomKeyAliasesMask = 1 shl 5;
        XkbGeomAllMask        = $3f;

type
        PXkbGeometrySizesPtr = ^TXkbGeometrySizesRec;
        TXkbGeometrySizesRec = record
                                which           : Word;
                                num_properties  : Word;
                                                                                                        num_colors      : Word;
                                num_shapes      : Word;
                                num_sections    : Word;
                                num_doodads     : Word;
                                num_key_aliases : Word;
                                                                                                 end;

        {
          Tie it all together into one big keyboard description
        }
type
        PXkbDescPtr = ^TXkbDescRec;
        TXkbDescRec = record
                       dpy          : PDisplay;
                       flags        : Word;
                       device_spec  : Word;
                       min_key_code : TKeyCode;
                       max_key_code : TKeyCode;
                       ctrls        : PXkbControlsPtr;
                       server       : PXkbServerMapPtr;
                       map          : PXkbClientMapPtr;
                       indicators   : PXkbIndicatorPtr;
                       names        : PXkbNamesPtr;
                       compat       : PXkbCompatMapPtr;
                       geom         : PXkbGeometryPtr;
                      end;

function XkbKeyKeyTypeIndex (d : PXkbDescPtr; k : Word; g  : Byte)           : Byte;
function XkbKeyKeyType      (d : PXkbDescPtr; k : Word; g  : Byte)           : PXkbKeyTypePtr;
function XkbKeyGroupWidth   (d : PXkbDescPtr; k : Word; g  : Byte)           : Byte;
function XkbKeyGroupsWidth  (d : PXkbDescPtr; k : Word)                      : Byte;
function XkbKeyGroupInfo    (d : PXkbDescPtr; k : Word)                      : Byte;
function XkbKeyNumGroups    (d : PXkbDescPtr; k : Word)                      : Byte;
function XkbKeyNumSyms      (d : PXkbDescPtr; k : Word)                      : Word;
function XkbKeySymsPtr      (d : PXkbDescPtr; k : Word)                      : PKeySym;
function XkbKeySym          (d : PXkbDescPtr; k : Word; n  : Word)           : TKeySym;
function XkbKeySymEntry     (d : PXkbDescPtr; k : Word; sl : Word; g : Byte) : TKeySym;
function XkbKeyAction       (d : PXkbDescPtr; k : Word; n  : Word)           : PXkbAction;
function XkbKeyActionEntry  (d : PXkbDescPtr; k : Word; sl : Word; g : Byte) : Byte;
function XkbKeyHasActions   (d : PXkbDescPtr; k : Word)                      : Boolean;
function XkbKeyNumActions   (d : PXkbDescPtr; k : Word)                      : Word;
function XkbKeyActionsPtr   (d : PXkbDescPtr; k : Word)                      : PXkbAction;
function XkbKeycodeInRange  (d : PXkbDescPtr; k : Word)                      : Boolean;
function XkbNumKeys         (d : PXkbDescPtr)                                : Byte;

        {
          The following structures can be used to track changes
          to a keyboard device
        }
type
        PXkbMapChangesPtr = ^TXkbMapChangesRec;
        TXkbMapChangesRec = record
                             changed            : Word;
                             min_key_code       : TKeyCode;
                             max_key_code       : TKeyCode;
                             first_type         : Byte;
                             num_types          : Byte;
                             first_key_sym      : TKeyCode;
                             num_key_syms       : Byte;
                             first_key_act      : TKeyCode;
                             num_key_acts       : Byte;
                             first_key_behavior : TKeyCode;
                             num_key_behaviors  : Byte;
                             first_key_explicit : TKeyCode;
                             num_key_explicit   : Byte;
                             first_modmap_key   : TKeyCode;
                             num_modmap_keys    : Byte;
                             first_vmodmap_key  : TKeyCode;
                             num_vmodmap_keys   : Byte;
                             pad                : Byte;
                             vmods              : Word;
                            end;

type
        PXkbControlsChangesPtr = ^TXkbControlsChangesRec;
        TXkbControlsChangesRec = record
                                                                                                                changed_ctrls         : Word;
                                  enabled_ctrls_changes : Word;
                                  num_groups_changed    : Boolean;
                                 end;

type
        PXkbIndicatorChangesPtr = ^TXkbIndicatorChangesRec;
        TXkbIndicatorChangesRec = record
                                   state_changes : Word;
                                                                                                                 map_changes   : Word;
                                  end;

type
        PXkbNameChangesPtr = ^TXkbNameChangesRec;
        TXkbNameChangesRec = record
                              changed            : Word;
                              first_type         : Byte;
                              num_types          : Byte;
                              first_lvl          : Byte;
                              num_lvls           : Byte;
                              num_aliases        : Byte;
                              num_rg             : Byte;
                              first_key          : Byte;
                                                                                                num_keys           : Byte;
                                                                                                changed_vmods      : Word;
                              changed_indicators : LongWord;
                              changed_groups     : Byte;
                             end;

type
        PXkbCompatChangesPtr = ^TXkbCompatChangesRec;
        TXkbCompatChangesRec = record
                                                                                                        changed_groups : Byte;
                                first_si       : Word;
                                num_si         : Word;
                               end;

type
        PXkbChangesPtr = ^TXkbChangesRec;
        TXkbChangesRec = record
                          device_spec   : Word;
                          state_changes : Word;
                                                                                map           : TXkbMapChangesRec;
                          ctrls         : TXkbControlsChangesRec;
                          indicators    : TXkbIndicatorChangesRec;
                                                                                names         : TXkbNameChangesRec;
                          compat        : TXkbCompatChangesRec;
                         end;

        {
          These data structures are used to construct a keymap from
          a set of components or to list components in the server
          database.
        }
type
        PXkbComponentNamesPtr = ^TXkbComponentNamesRec;
        TXkbComponentNamesRec = record
                                 keymap   : PShortInt;
                                 keycodes : PShortInt;
                                 types    : PShortInt;
                                 compat   : PShortInt;
                                 symbols  : PShortInt;
                                 geometry : PShortInt;
                                end;

type
        PXkbComponentNamePtr = ^TXkbComponentNameRec;
        TXkbComponentNameRec = record
                                flags : Word;
                                name  : PChar;
                               end;

type
        PXkbComponentListPtr = ^TXkbComponentListRec;
        TXkbComponentListRec = record
                                num_keymaps  : SmallInt;
                                num_keycodes : SmallInt;
                                num_types    : SmallInt;
                                num_compat   : SmallInt;
                                                                                                        num_symbols  : SmallInt;
                                num_geometry : SmallInt;
                                keymaps      : PXkbComponentNamePtr;
                                                                                                        keycodes     : PXkbComponentNamePtr;
                                types        : PXkbComponentNamePtr;
                                compat       : PXkbComponentNamePtr;
                                symbols      : PXkbComponentNamePtr;
                                geometry     : PXkbComponentNamePtr;
                               end;

        {
          The following data structures describe and track changes to a
          non-keyboard extension device
        }
type
        PXkbDeviceLedInfoPtr = ^TXkbDeviceLedInfoRec;
        TXkbDeviceLedInfoRec = record
                                led_class       : Word;
                                                                                                        led_id          : Word;
                                phys_indicators : Word;
                                maps_present    : Word;
                                names_present   : Word;
                                state           : Word;
                                names           : array [0..XkbNumIndicators -1] of TAtom;
                                                                                                        maps            : array [0..XkbNumIndicators -1] of TXkbIndicatorMapRec;
                               end;

type
        PXkbDeviceInfoPtr = ^TXkbDeviceInfoRec;
        TXkbDeviceInfoRec = record
                             name          : PChar;
                             _type         : TAtom;
                             device_spec   : Word;
                             has_own_state : Boolean;
                                                                                         supported     : Word;
                             unsupported   : Word;
                             num_btns      : Word;
                             btn_acts      : PXkbAction;
                             sz_leds       : Word;
                             num_leds      : Word;
                                                                                         dflt_kbd_fb   : Word;
                             dflt_led_fb   : Word;
                                                                                         leds          : PXkbDeviceLedInfoPtr;
                            end;

function XkbXI_DevHasBtnActs (d : PXkbDeviceInfoPtr)           : Boolean;
function XkbXI_LegalDevBtn   (d : PXkbDeviceInfoPtr; b : Word) : Boolean;
function XkbXI_DevHasLeds    (d : PXkbDeviceInfoPtr)           : Boolean;

type
        PXkbDeviceLedChangesPtr = ^TXkbDeviceLedChangesRec;
        TXkbDeviceLedChangesRec = record
                                   led_class : Word;
                                   led_id    : Word;
                                   defined   : Word; //names or maps changed
                                   next      : PXkbDeviceLedChangesPtr;
                                  end;

type
        PXkbDeviceChangesPtr = ^TXkbDeviceChangesRec;
        TXkbDeviceChangesRec = record
                                changed   : Word;
                                                                                                        first_btn : Word;
                                num_btns  : Word;
                                                                                                        leds      : TXkbDeviceLedChangesRec;
                               end;

function XkbShapeDoodadColor     (g : PXkbGeometryPtr; d : PXkbShapeDoodadPtr)  : PXkbColorPtr;
function XkbShapeDoodadShape     (g : PXkbGeometryPtr; d : PXkbShapeDoodadPtr)  : PXkbShapePtr;
procedure XkbSetShapeDoodadColor (g : PXkbGeometryPtr; d : PXkbShapeDoodadPtr; c : PXkbColorPtr);
procedure XkbSetShapeDoodadShape (g : PXkbGeometryPtr; d : PXkbShapeDoodadPtr; s : PXkbShapePtr);

function XkbTextDoodadColor     (g : PXkbGeometryPtr; d : PXkbTextDoodadPtr)    : PXkbColorPtr;
procedure XkbSetTextDoodadColor (g : PXkbGeometryPtr; d : PXkbTextDoodadPtr; c : PXkbColorPtr);

function XkbIndicatorDoodadShape        (g : PXkbGeometryPtr; d : PXkbIndicatorDoodadPtr)
                                                                                                                                                                                                                                                                                                                                : PXkbShapeDoodadPtr;
function XkbIndicatorDoodadOnColor      (g : PXkbGeometryPtr; d : PXkbIndicatorDoodadPtr) : PXkbColorPtr;
function XkbIndicatorDoodadOffColor     (g : PXkbGeometryPtr; d : PXkbIndicatorDoodadPtr) : PXkbColorPtr;
procedure XkbSetIndicatorDoodadOnColor  (g : PXkbGeometryPtr; d : PXkbIndicatorDoodadPtr;
                                         c : PXkbColorPtr);
procedure XkbSetIndicatorDoodadOffColor (g : PXkbGeometryPtr; d : PXkbIndicatorDoodadPtr;
                                         c : PXkbColorPtr);
procedure XkbSetIndicatorDoodadShape    (g : PXkbGeometryPtr; d : PXkbIndicatorDoodadPtr;
                                                                                                                                                                 s : PXkbShapeDoodadPtr);

function XkbLogoDoodadColor     (g : PXkbGeometryPtr; d : PXkbLogoDoodadPtr) : PXkbColorPtr;
function XkbLogoDoodadShape     (g : PXkbGeometryPtr; d : PXkbLogoDoodadPtr) : PXkbShapeDoodadPtr;
procedure XkbSetLogoDoodadColor (g : PXkbGeometryPtr; d : PXkbLogoDoodadPtr; c : PXkbColorPtr);
procedure XkbSetLogoDoodadShape (g : PXkbGeometryPtr; d : PXkbLogoDoodadPtr; s : PXkbShapeDoodadPtr);

function XkbKeyShape     (g : PXkbGeometryPtr; k : PXkbKeyPtr) : PXkbShapeDoodadPtr;
function XkbKeyColor     (g : PXkbGeometryPtr; k : PXkbKeyPtr) : PXkbColorPtr;
procedure XkbSetKeyShape (g : PXkbGeometryPtr; k : PXkbKeyPtr; s : PXkbShapeDoodadPtr);
procedure XkbSetKeyColor (g : PXkbGeometryPtr; k : PXkbKeyPtr; c : PXkbColorPtr);

function XkbGeomColorIndex (g : PXkbGeometryPtr; c : PXkbColorPtr) : longint;

function XkbAddGeomProperty (geom : PXkbGeometryPtr; name : PChar; value : PChar) : PXkbPropertyPtr;
        cdecl; external libX11 name 'XkbAddGeomProperty';

function XkbAddGeomKeyAlias (geom : PXkbGeometryPtr; alias : PChar; real : PChar) : PXkbKeyAliasPtr;
        cdecl; external libX11 name 'XkbAddGeomKeyAlias';

function XkbAddGeomColor (geom : PXkbGeometryPtr; spec : PChar; pixel : Word) : PXkbColorPtr;
        cdecl; external libX11 name 'XkbAddGeomColor';

function XkbAddGeomOutline (shape : PXkbShapePtr; sz_points : SmallInt) : PXkbOutlinePtr;
        cdecl; external libX11 name 'XkbAddGeomOutline';

function XkbAddGeomShape (geom : PXkbGeometryPtr; name : TAtom; sz_outlines : SmallInt) : PXkbShapePtr;
        cdecl; external libX11 name 'XkbAddGeomShape';

function XkbAddGeomKey (row : PXkbRowPtr) : PXkbKeyPtr;
        cdecl; external libX11 name 'XkbAddGeomKey';

function XkbAddGeomRow (section : PXkbSectionPtr; sz_keys : SmallInt) : PXkbRowPtr;
        cdecl; external libX11 name 'XkbAddGeomRow';

function XkbAddGeomSection (geom        : PXkbGeometryPtr; name       : TAtom;
                            sz_rows     : SmallInt;               sz_doodads : SmallInt;
                                                                                                                sz_overlays : SmallInt)                              : PXkbSectionPtr;
        cdecl; external libX11 name 'XkbAddGeomSection';

function XkbAddGeomOverlay (section : PXkbSectionPtr; name : TAtom; sz_rows : SmallInt) : PXkbOverlayPtr;
        cdecl; external libX11 name 'XkbAddGeomOverlay';

function XkbAddGeomOverlayRow (overlay : PXkbOverlayPtr; row_under : SmallInt;
                               sz_keys : SmallInt)                              : PXkbOverlayRowPtr;
        cdecl; external libX11 name 'XkbAddGeomOverlayRow';

function XkbAddGeomOverlayKey (overlay : PXkbOverlayPtr; row   : PXkbOverlayRowPtr;
                                                                                                                         over    : PChar;          under : PChar)              : PXkbOverlayKeyPtr;
        cdecl; external libX11 name 'XkbAddGeomOverlayKey';

function XkbAddGeomDoodad (geom : PXkbGeometryPtr; section : PXkbSectionPtr; name : TAtom) : PXkbDoodadPtr;
        cdecl; external libX11 name 'XkbAddGeomDoodad';

procedure XkbFreeGeomKeyAliases (geom  : PXkbGeometryPtr; first   : SmallInt;
                                 count : SmallInt;        freeAll : Boolean);
        cdecl; external libX11 name 'XkbFreeGeomKeyAliases';

procedure XkbFreeGeomColors (geom : PXkbGeometryPtr; first : SmallInt; count : SmallInt; freeAll : Boolean);
        cdecl; external libX11 name 'XkbFreeGeomColors';

procedure XkbFreeGeomDoodads (doodads : PXkbDoodadPtr; nDoodads : SmallInt; freeAll : Boolean);
        cdecl; external libX11 name 'XkbFreeGeomDoodads';

procedure XkbFreeGeomProperties (geom  : PXkbGeometryPtr; first   : SmallInt;
                                 count : SmallInt;        freeAll : Boolean);
        cdecl; external libX11 name 'XkbFreeGeomProperties';

procedure XkbFreeGeomOverlayKeys (row   : PXkbOverlayRowPtr; first   : SmallInt;
                                  count : SmallInt;          freeAll : Boolean);
        cdecl; external libX11 name 'XkbFreeGeomOverlayKeys';

procedure XkbFreeGeomOverlayRows (overlay : PXkbOverlayPtr; first   : SmallInt;
                                  count   : SmallInt;       freeAll : Boolean);
        cdecl; external libX11 name 'XkbFreeGeomOverlayRows';

procedure XkbFreeGeomOverlays (section : PXkbSectionPtr; first   : SmallInt;
                               count   : SmallInt;       freeAll : Boolean);
        cdecl; external libX11 name 'XkbFreeGeomOverlays';

procedure XkbFreeGeomKeys (row : PXkbRowPtr; first : SmallInt; count : SmallInt; freeAll : Boolean);
        cdecl; external libX11 name 'XkbFreeGeomKeys';

procedure XkbFreeGeomRows (section : PXkbSectionPtr; first   : SmallInt;
                           count   : SmallInt;       freeAll : Boolean);
        cdecl; external libX11 name 'XkbFreeGeomRows';

procedure XkbFreeGeomSections (geom  : PXkbGeometryPtr; first   : SmallInt;
                               count : SmallInt;        freeAll : Boolean);
        cdecl; external libX11 name 'XkbFreeGeomSections';

procedure XkbFreeGeomPoints (outline : PXkbOutlinePtr; first   : SmallInt;
                             count   : SmallInt;       freeAll : Boolean);
        cdecl; external libX11 name 'XkbFreeGeomPoints';

procedure XkbFreeGeomOutlines (shape : PXkbShapePtr; first   : SmallInt;
                               count : SmallInt;     freeAll : Boolean);
        cdecl; external libX11 name 'XkbFreeGeomOutlines';

procedure XkbFreeGeomShapes (geom  : PXkbGeometryPtr; first   : SmallInt;
                             count : SmallInt;        freeAll : Boolean);
        cdecl; external libX11 name 'XkbFreeGeomShapes';

procedure XkbFreeGeometry (geom : PXkbGeometryPtr; which : Word; freeMap : Boolean);
        cdecl; external libX11 name 'XkbFreeGeometry';

function XkbAllocGeomProps (geom : PXkbGeometryPtr; nProps : SmallInt) : TStatus;
        cdecl; external libX11 name 'XkbAllocGeomProps';

function XkbAllocGeomKeyAliases (geom : PXkbGeometryPtr; nAliases : SmallInt) : TStatus;
        cdecl; external libX11 name 'XkbAllocGeomKeyAliases';

function XkbAllocGeomColors (geom : PXkbGeometryPtr; nColors : SmallInt) : TStatus;
        cdecl; external libX11 name 'XkbAllocGeomColors';

function XkbAllocGeomShapes (geom : PXkbGeometryPtr; nShapes : SmallInt) : TStatus;
        cdecl; external libX11 name 'XkbAllocGeomShapes';

function XkbAllocGeomSections (geom : PXkbGeometryPtr; nSections : SmallInt) : TStatus;
        cdecl; external libX11 name 'XkbAllocGeomSections';

function XkbAllocGeomOverlays (section : PXkbSectionPtr; num_needed : SmallInt) : TStatus;
        cdecl; external libX11 name 'XkbAllocGeomOverlays';

function XkbAllocGeomOverlayRows (overlay : PXkbOverlayPtr; num_needed : SmallInt) : TStatus;
        cdecl; external libX11 name 'XkbAllocGeomOverlayRows';

function XkbAllocGeomOverlayKeys (row : PXkbOverlayRowPtr; num_needed : SmallInt) : TStatus;
        cdecl; external libX11 name 'XkbAllocGeomOverlayKeys';

function XkbAllocGeomDoodads (geom : PXkbGeometryPtr; nDoodads : SmallInt) : TStatus;
        cdecl; external libX11 name 'XkbAllocGeomDoodads';

function XkbAllocGeomSectionDoodads (section : PXkbSectionPtr; nDoodads : SmallInt) : TStatus;
        cdecl; external libX11 name 'XkbAllocGeomSectionDoodads';

function XkbAllocGeomOutlines (shape : PXkbShapePtr; nOL : SmallInt) : TStatus;
        cdecl; external libX11 name 'XkbAllocGeomOutlines';

function XkbAllocGeomRows (section : PXkbSectionPtr; nRows : SmallInt) : TStatus;
        cdecl; external libX11 name 'XkbAllocGeomRows';

function XkbAllocGeomPoints (ol : PXkbOutlinePtr; nPts : SmallInt) : TStatus;
        cdecl; external libX11 name 'XkbAllocGeomPoints';

function XkbAllocGeomKeys (row : PXkbRowPtr; nKeys : SmallInt) : TStatus;
        cdecl; external libX11 name 'XkbAllocGeomKeys';

function XkbAllocGeometry (xkb : PXkbDescPtr; sizes : PXkbGeometrySizesPtr) : TStatus;
        cdecl; external libX11 name 'XkbAllocGeometry';

function XkbSetGeometry (dpy : PDisplay; deviceSpec : Word; geom : PXkbGeometryPtr) : TStatus;
        cdecl; external libX11 name 'XkbSetGeometry';

function XkbComputeShapeTop (shape : PXkbShapePtr; bounds : PXkbBoundsPtr) : Boolean;
        cdecl; external libX11 name 'XkbComputeShapeTop';

function XkbComputeShapeBounds (shape : PXkbShapePtr) : Boolean;
        cdecl; external libX11 name 'XkbComputeShapeBounds';

function XkbComputeRowBounds (geom : PXkbGeometryPtr; section : PXkbSectionPtr; row : PXkbRowPtr) : Boolean;
        cdecl; external libX11 name 'XkbComputeRowBounds';

function XkbComputeSectionBounds (geom : PXkbGeometryPtr; section : PXkbSectionPtr) : Boolean;
        cdecl; external libX11 name 'XkbComputeSectionBounds';

function XkbFindOverlayForKey (geom : PXkbGeometryPtr; wanted : PXkbSectionPtr; under : PChar) : PChar;
        cdecl; external libX11 name 'XkbFindOverlayForKey';

function XkbGetGeometry (dpy : PDisplay; xkb : PXkbDescPtr) : TStatus;
        cdecl; external libX11 name 'XkbGetGeometry';

function XkbGetNamedGeometry (dpy : PDisplay; xkb : PXkbDescPtr; name : TAtom) : TStatus;
        cdecl; external libX11 name 'XkbGetNamedGeometry';


{$ifdef XKB_IN_SERVER}
function SrvXkbAddGeomKeyAlias (geom : PXkbGeometryPtr; alias : PChar; real : PChar) : PXkbKeyAliasPtr;
        cdecl; external libX11 name 'XkbAddGeomKeyAlias';

function SrvXkbAddGeomColor (geom : PXkbGeometryPtr; spec : PChar; pixel : Word) : PXkbColorPtr;
        cdecl; external libX11 name 'XkbAddGeomColor';

function SrvXkbAddGeomDoodad (geom : PXkbGeometryPtr; section : PXkbSectionPtr;
                              name : TAtom)                                     : PXkbDoodadPtr;
        cdecl; external libX11 name 'XkbAddGeomDoodad';

function SrvXkbAddGeomKey (geom : PXkbGeometryPtr; alias : PChar; real : PChar) : PXkbKeyAliasPtr;
        cdecl; external libX11 name 'XkbAddGeomKeyAlias';

function SrvXkbAddGeomOutline (shape : PXkbShapePtr; sz_points : SmallInt) : PXkbOutlinePtr;
        cdecl; external libX11 name 'XkbAddGeomOutline';

function SrvXkbAddGeomOverlay (overlay : PXkbOverlayPtr; row   : PXkbOverlayRowPtr;
                                                                                                                         over    : PChar;          under : PChar)              : PXkbOverlayKeyPtr;
        cdecl; external libX11 name 'XkbAddGeomOverlayKey';

function SrvXkbAddGeomOverlayRow (overlay : PXkbOverlayPtr; row_under : SmallInt;
                               sz_keys : SmallInt)                              : PXkbOverlayRowPtr
        cdecl; external libX11 name 'XkbAddGeomOverlayRow';

function SrvXkbAddGeomOverlayKey (overlay : PXkbOverlayPtr; row   : PXkbOverlayRowPtr;
                                                                                                                         over    : PChar;          under : PChar)              : PXkbOverlayKeyPtr;
        cdecl; external libX11 name 'XkbAddGeomOverlayKey';

function SrvXkbAddGeomProperty (geom : PXkbGeometryPtr; name : PChar; value : PChar) : PXkbPropertyPtr;
        cdecl; external libX11 name 'XkbAddGeomProperty';

function SrvXkbAddGeomRow (section : PXkbSectionPtr; sz_keys : SmallInt) : PXkbRowPtr;
        cdecl; external libX11 name 'XkbAddGeomRow';

function SrvXkbAddGeomSection (geom        : PXkbGeometryPtr; name       : TAtom;
                               sz_rows     : SmallInt;                    sz_doodads : SmallInt;
                                                                                                                   sz_overlays : SmallInt)                              : PXkbSectionPtr;
        cdecl; external libX11 name 'XkbAddGeomSection';

function SrvXkbAddGeomShape (geom : PXkbGeometryPtr; name : TAtom; sz_outlines : SmallInt) : PXkbShapePtr;
        cdecl; external libX11 name 'XkbAddGeomShape';

function SrvXkbAllocGeomKeyAliases (geom : PXkbGeometryPtr; nAliases : SmallInt) : TStatus;
        cdecl; external libX11 name 'XkbAllocGeomKeyAliases';

function SrvXkbAllocGeomColors (geom : PXkbGeometryPtr; nColors : SmallInt) : TStatus;
        cdecl; external libX11 name 'XkbAllocGeomColors';

function SrvXkbAllocGeomDoodads (geom : PXkbGeometryPtr; nDoodads : SmallInt) : TStatus;
        cdecl; external libX11 name 'XkbAllocGeomDoodads';

function SrvXkbAllocGeomKeys (row : PXkbRowPtr; nKeys : SmallInt) : TStatus;
        cdecl; external libX11 name 'XkbAllocGeomKeys';

function SrvXkbAllocGeomOutlines (shape : PXkbShapePtr; nOL : SmallInt) : TStatus;
        cdecl; external libX11 name 'XkbAllocGeomOutlines';

function SrvXkbAllocGeomPoints (ol : PXkbOutlinePtr; nPts : SmallInt) : TStatus;
        cdecl; external libX11 name 'XkbAllocGeomPoints';

function SrvXkbAllocGeomProps (geom : PXkbGeometryPtr; nProps : SmallInt) : TStatus;
        cdecl; external libX11 name 'XkbAllocGeomProps';

function SrvXkbAllocGeomRows (section : PXkbSectionPtr; nRows : SmallInt) : TStatus;
        cdecl; external libX11 name 'XkbAllocGeomRows';

function SrvXkbAllocGeomSectionDoodads (section : PXkbSectionPtr; nDoodads : SmallInt) : TStatus;
        cdecl; external libX11 name 'XkbAllocGeomSectionDoodads';

function SrvXkbAllocGeomSections (geom : PXkbGeometryPtr; nSections : SmallInt) : TStatus;
        cdecl; external libX11 name 'XkbAllocGeomSections';

function SrvXkbAllocGeomOverlays (section : PXkbSectionPtr; num_needed : SmallInt) : TStatus;
        cdecl; external libX11 name 'XkbAllocGeomOverlays';

function SrvXkbAllocGeomOverlayRows (overlay : PXkbOverlayPtr; num_needed : SmallInt) : TStatus;
        cdecl; external libX11 name 'XkbAllocGeomOverlayRows';

function SrvXkbAllocGeomOverlayKeys (row : PXkbOverlayRowPtr; num_needed : SmallInt) : TStatus;
        cdecl; external libX11 name 'XkbAllocGeomOverlayKeys';

function SrvXkbAllocGeomShapes (geom : PXkbGeometryPtr; nShapes : SmallInt) : TStatus;
        cdecl; external libX11 name 'XkbAllocGeomShapes';

function SrvXkbAllocGeometry (xkb : PXkbDescPtr; sizes : PXkbGeometrySizesPtr) : TStatus;
        cdecl; external libX11 name 'XkbAllocGeometry';

procedure SrvXkbFreeGeomKeyAliases (geom  : PXkbGeometryPtr; first   : SmallInt;
                                 count : SmallInt;        freeAll : Boolean);
        cdecl; external libX11 name 'XkbFreeGeomKeyAliases';

procedure SrvXkbFreeGeomColors (geom  : PXkbGeometryPtr; first   : SmallInt;
                                count : SmallInt;        freeAll : Boolean);
        cdecl; external libX11 name 'XkbFreeGeomColors';

procedure SrvXkbFreeGeomDoodads (doodads : PXkbDoodadPtr; nDoodads : SmallInt; freeAll : Boolean);
        cdecl; external libX11 name 'XkbFreeGeomDoodads';

procedure SrvXkbFreeGeomProperties (geom  : PXkbGeometryPtr; first   : SmallInt;
                                 count : SmallInt;        freeAll : Boolean);
        cdecl; external libX11 name 'XkbFreeGeomProperties';

procedure SrvXkbFreeGeomOverlayKeys (row   : PXkbOverlayRowPtr; first   : SmallInt;
                                     count : SmallInt;          freeAll : Boolean);
        cdecl; external libX11 name 'XkbFreeGeomOverlayKeys';

procedure SrvXkbFreeGeomOverlayRows (overlay : PXkbOverlayPtr; first   : SmallInt;
                                  count   : SmallInt;       freeAll : Boolean);
        cdecl; external libX11 name 'XkbFreeGeomOverlayRows';

procedure SrvXkbFreeGeomOverlays (section : PXkbSectionPtr; first   : SmallInt;
                               count   : SmallInt;       freeAll : Boolean);
        cdecl; external libX11 name 'XkbFreeGeomOverlays';

procedure SrvXkbFreeGeomKeys (row : PXkbRowPtr; first : SmallInt; count : SmallInt; freeAll : Boolean);
        cdecl; external libX11 name 'XkbFreeGeomKeys';

procedure SrvXkbFreeGeomRows (section : PXkbSectionPtr; first   : SmallInt;
                           count   : SmallInt;       freeAll : Boolean);
        cdecl; external libX11 name 'XkbFreeGeomRows';

procedure SrvXkbFreeGeomSections (geom  : PXkbGeometryPtr; first   : SmallInt;
                               count : SmallInt;        freeAll : Boolean);
        cdecl; external libX11 name 'XkbFreeGeomSections';

procedure SrvXkbFreeGeomPoints (outline : PXkbOutlinePtr; first   : SmallInt;
                             count   : SmallInt;       freeAll : Boolean);
        cdecl; external libX11 name 'XkbFreeGeomPoints';

procedure SrvXkbFreeGeomOutlines (shape : PXkbShapePtr; first   : SmallInt;
                               count : SmallInt;     freeAll : Boolean);
        cdecl; external libX11 name 'XkbFreeGeomOutlines';

procedure SrvXkbFreeGeomShapes (geom  : PXkbGeometryPtr; first   : SmallInt;
                             count : SmallInt;        freeAll : Boolean);
        cdecl; external libX11 name 'XkbFreeGeomShapes';

procedure SrvXkbFreeGeometry (geom : PXkbGeometryPtr; which : Word; freeMap : Boolean);
        cdecl; external libX11 name 'XkbFreeGeometry';
{$endif}

implementation
uses xi;

(************************************* xkb *************************************)
function XkbLegalXILedClass (c : Cardinal) : LongBool;
begin
{#define XkbLegalXILedClass(c) (((c)==KbdFeedbackClass)||((c)==LedFeedbackClass)||
                                ((c)==XkbDfltXIClass)||((c)==XkbAllXIClasses))}
        Result := (c = KbdFeedbackClass) or (c = LedFeedbackClass) or
            (c = XkbDfltXIClass)   or (c = XkbAllXIClasses);
end;

function XkbLegalXIBellClass (c : Cardinal) : LongBool;
begin
{#define XkbLegalXIBellClass(c) (((c)==KbdFeedbackClass)||((c)==BellFeedbackClass)||
                                 ((c)==XkbDfltXIClass)||((c)==XkbAllXIClasses))}
        Result := (c = KbdFeedbackClass) or (c = BellFeedbackClass) or
            (c = XkbDfltXIClass)   or (c = XkbAllXIClasses);
end;

function XkbExplicitXIDevice (c : Cardinal) : LongBool;
begin
{#define XkbExplicitXIDevice(c) (((c)&(~0xff))==0)}
        Result := (c and (not $ff)) = 0;
end;

function XkbExplicitXIClass (c : Cardinal) : LongBool;
begin
{#define XkbExplicitXIClass(c) (((c)&(~0xff))==0)}
        Result := (c and (not $ff)) = 0;
end;

function XkbExplicitXIId (c : Cardinal) : LongBool;
begin
{#define XkbExplicitXIId(c) (((c)&(~0xff))==0)}
        Result := (c and (not $ff)) = 0;
end;

function XkbSingleXIClass (c : Cardinal) : LongBool;
begin
{#define XkbSingleXIClass(c) ((((c)&(~0xff))==0)||((c)==XkbDfltXIClass))}
        Result := ((c and (not $ff)) = 0) or (c = XkbDfltXIClass);
end;

function XkbSingleXIId (c : Cardinal) : LongBool;
begin
{#define XkbSingleXIId(c) ((((c)&(~0xff))==0)||((c)==XkbDfltXIId))}
        Result := ((c and (not $ff)) = 0) or (c = XkbDfltXIId);
end;

function XkbBuildCoreState (m, g : Cardinal) : Cardinal;
begin
{#define XkbBuildCoreState(m,g) ((((g)&0x3)<<13)|((m)&0xff))}
        Result := ((g and $3) shl 13) or (m and $ff);
end;

function XkbGroupForCoreState (s : Cardinal) : Cardinal;
begin
{#define XkbGroupForCoreState(s) (((s)>>13)&0x3)}
        Result := (s shr 13) and $3;
end;

function XkbIsLegalGroup (g : Cardinal) : LongBool;
begin
{#define XkbIsLegalGroup(g) (((g)>=0)&&((g)<XkbNumKbdGroups))}
        Result := (g >= 0) and (g < XkbNumKbdGroups);
end;

function XkbSA_ValOp (a : Cardinal) : Cardinal;
begin
{#define XkbSA_ValOp(a) ((a)&XkbSA_ValOpMask)}
        Result := a and XkbSA_ValOpMask;
end;

function XkbSA_ValScale (a : Cardinal) : Cardinal;
begin
{#define XkbSA_ValScale(a) ((a)&XkbSA_ValScaleMask)}
        Result := a and XkbSA_ValScaleMask;
end;

function XkbIsModAction (a : PXkbAnyAction) : LongBool;
begin
{#define XkbIsModAction(a) (((a)->type>=Xkb_SASetMods)&&((a)->type<=XkbSA_LockMods))}
        Result := (a^._type >= XkbSA_SetMods) and (a^._type <= XkbSA_LockMods);
end;

function XkbIsGroupAction (a : PXkbAnyAction) : LongBool;
begin
{#define XkbIsGroupAction(a) (((a)->type>=XkbSA_SetGroup)&&((a)->type<=XkbSA_LockGroup))}
        Result := (a^._type >= XkbSA_SetGroup) or (a^._type <= XkbSA_LockGroup);
end;

function XkbIsPtrAction (a : PXkbAnyAction) : LongBool;
begin
{#define XkbIsPtrAction(a) (((a)->type>=XkbSA_MovePtr)&&((a)->type<=XkbSA_SetPtrDflt))}
        Result := (a^._type >= XkbSA_MovePtr) and (a^._type <= XkbSA_SetPtrDflt);
end;

function XkbIsLegalKeycode (const k : Cardinal) : LongBool;
begin
{#define        XkbIsLegalKeycode(k)    (((k)>=XkbMinLegalKeyCode)&&((k)<=XkbMaxLegalKeyCode))}
        Result := (k >= XkbMinLegalKeyCode) and (k <= XkbMaxLegalKeyCode);
end;

function XkbShiftLevel (n : Byte) : Byte;
begin
{#define XkbShiftLevel(n) ((n)-1)}
        Result := n -1;
end;

function XkbShiftLevelMask (n : Byte) : Byte;
begin
{#define XkbShiftLevelMask(n) (1<<((n)-1))}
        Result := 1 shl (n - 1);
end;

(********************************** End of xkb **********************************)

(************************************ xkbstr ************************************)
function XkbCharToInt (v : Byte) : SmallInt;
begin
{#define XkbCharToInt(v) ((v)&0x80?(int)((v)|(~0xff)):(int)((v)&0x7f))}
        if ((v and $80)<>0) then
                Result := v or (not $ff)
        else
                Result := longint (v) and $7f;
end;

procedure XkbIntTo2Chars (i : word; var h, l : byte);
begin
{#define XkbIntTo2Chars(i,h,l) (((h)=((i>>8)&0xff)),((l)=((i)&0xff)))}
        h := (i shr 8) and $ff;
        l := i and $ff;
end;

function Xkb2CharsToInt (h, l : Byte) : SmallInt;
begin
{$IFDEF WORD64}
        {$IFDEF UNSIGNEDBITFIELDS}
                {$DEFINE BIT64}
        {$ENDIF}
{$ENDIF}
{$IFDEF BIT64}
{$UNDEF BIT64}
{#define Xkb2CharsToInt(h,l) ((h)&0x80?(int)(((h)<<8)|(l)|(~0xffff)): (int)(((h)<<8)|(l)&0x7fff))}
                if ((h = $80) <> 0) then
                        Result := ((LongInt (h) shl 8) or l or (not $ffff))
                else
                        Result := ((LongInt (h) shl 8) or l and $7fff));
{$ELSE}
{#define Xkb2CharsToInt(h,l) ((short)(((h)<<8)|(l)))}
                Result := (integer (h) shl 8) or l;
{$ENDIF}
end;

function XkbModLocks (s : PXkbStatePtr) : Byte;
begin
{#define XkbModLocks(s) ((s)->locked_mods)}
        Result := s^.locked_mods;
end;

function XkbStateMods (s : PXkbStatePtr) : Word;
begin
{#define XkbStateMods(s) ((s)->base_mods|(s)->latched_mods|XkbModLocks(s))}
        Result := s^.base_mods or s^.latched_mods or XkbModLocks (s);
end;

function XkbGroupLock (s : PXkbStatePtr) : Byte;
begin
{#define XkbGroupLock(s) ((s)->locked_group)}
        Result := s^.locked_group;
end;

function XkbStateGroup (s : PXkbStatePtr) : Word;
begin
{#define XkbStateGroup(s) ((s)->base_group+(s)->latched_group+XkbGroupLock(s))}
        Result := S^.base_group + s^.latched_group + XkbGroupLock (s);
end;

function XkbStateFieldFromRec (s : PXkbStatePtr) : Cardinal;
begin
{#define XkbStateFieldFromRec(s) XkbBuildCoreState((s)->lookup_mods,(s)->group)}
        Result := XkbBuildCoreState (s^.lookup_mods, s^.group);
end;

function XkbGrabStateFromRec  (s : PXkbStatePtr) : Cardinal;
begin
{#define XkbGrabStateFromRec(s) XkbBuildCoreState((s)->grab_mods,(s)->group)}
        Result := XkbBuildCoreState (s^.grab_mods, s^.group);
end;

function XkbNumGroups (g : Word) : Word;
begin
{#define XkbNumGroups(g) ((g)&0x0f)}
        Result := g and $0f;
end;

function XkbOutOfRangeGroupInfo (g : Word) : Word;
begin
{#define XkbOutOfRangeGroupInfo(g) ((g)&0xf0)}
        Result := g and $f0;
end;

function XkbOutOfRangeGroupAction (g : Word) : Word;
begin
{#define XkbOutOfRangeGroupAction(g) ((g)&0xc0)}
        Result := g and $c0;
end;

function XkbOutOfRangeGroupNumber (g : Word) : Word;
begin
{#define XkbOutOfRangeGroupNumber(g) (((g)&0x30)>>4)}
        Result := (g and $30) shr 4;
end;

function XkbSetGroupInfo (g, w, n : Word) : Word;
begin
{#define XkbSetGroupInfo(g,w,n) (((w)&0xc0)|(((n)&3)<<4)|((g)&0x0f))}
        Result := (w and $c0) or ((n and 3) shl 4) or (g and $0f);
end;

function XkbSetNumGroups (g, n : Word) : Word;
begin
{#define XkbSetNumGroups(g,n) (((g)&0xf0)|((n)&0x0f))}
        Result := (g and $f0) or (n and $0f);
end;

function XkbModActionVMods (a : PXkbModAction) : SmallInt;
begin
{#define XkbModActionVMods(a) ((short)(((a)->vmods1<<8)|((a)->vmods2)))}
        Result := (integer (a^.vmods1) shl 8) or a^.vmods2;
end;

procedure XkbSetModActionVMods (a : PXkbModAction; v : Byte);
begin
{#define XkbSetModActionVMods(a,v) (((a)->vmods1=(((v)>>8)&0xff)),(a)->vmods2=((v)&0xff))}
        a^.vmods1 := (v shr 8) and $ff;
        a^.vmods2 := v and $ff;
end;

function XkbSAGroup (a : PXkbGroupAction) : ShortInt;
begin
{#define XkbSAGroup(a) (XkbCharToInt((a)->group_XXX))}
        Result := ShortInt(XkbCharToInt(a^.group_XXX));
end;

procedure XkbSASetGroup (a : PXkbGroupAction; g : ShortInt);
begin
{#define XkbSASetGroup(a,g) ((a)->group_XXX=(g))}
        a^.group_XXX := g;
end;

function XkbPtrActionX (a : PXkbPtrAction) : Word;
begin
{#define XkbPtrActionX(a) (Xkb2CharsToInt((a)->high_XXX,(a)->low_XXX))}
        Result := Word(Xkb2CharsToInt(a^.high_XXX,a^.low_XXX));
end;

function XkbPtrActionY (a : PXkbPtrAction) : Word;
begin
{#define XkbPtrActionY(a) (Xkb2CharsToInt((a)->high_YYY,(a)->low_YYY))}
        Result := Word(Xkb2CharsToInt(a^.high_YYY,a^.low_YYY));
end;

procedure XkbSetPtrActionX (a : PXkbPtrAction; x : Byte);
begin
{#define XkbSetPtrActionX(a,x) (XkbIntTo2Chars(x,(a)->high_XXX,(a)->low_XXX))}
        XkbIntTo2Chars(x, a^.high_XXX, a^.low_XXX);
end;

procedure XkbSetPtrActionY (a : PXkbPtrAction; y : Byte);
begin
{#define XkbSetPtrActionY(a,y) (XkbIntTo2Chars(y,(a)->high_YYY,(a)->low_YYY))}
        XkbIntTo2Chars (y, a^.high_YYY, a^.low_YYY);
end;

function XkbSAPtrDfltValue (a : PXkbPtrDfltAction) : ShortInt;
begin
{#define XkbSAPtrDfltValue(a) (XkbCharToInt((a)->valueXXX))}
        Result := ShortInt (XkbCharToInt(a^.valueXXX));
end;

procedure XkbSASetPtrDfltValue (a : PXkbPtrDfltAction; const c);
begin
{#define XkbSASetPtrDfltValue(a,c) ((a)->valueXXX= ((c)&0xff))}
        a^.valueXXX := ShortInt (c) and $ff;
end;

function XkbSAScreen (a : PXkbSwitchScreenAction) : ShortInt;
begin
{#define XkbSAScreen(a) (XkbCharToInt((a)->screenXXX))}
        Result := ShortInt (XkbCharToInt(a^.screenXXX));
end;

procedure XkbSASetScreen (a : PXkbSwitchScreenAction; const s);
begin
{#define XkbSASetScreen(a,s) ((a)->screenXXX= ((s)&0xff))}
        a^.screenXXX := ShortInt (s) and $ff;
end;

procedure XkbActionSetCtrls (a : PXkbCtrlsAction; c : Byte);
begin
{#define XkbActionSetCtrls(a,c) (((a)->ctrls3=(((c)>>24)&0xff)),((a)->ctrls2=(((c)>>16)&0xff)),
                                 ((a)->ctrls1=(((c)>>8)&0xff)),((a)->ctrls0=((c)&0xff)))        }
        a^.ctrls3 := ((c shr  24) and $ff);
        a^.ctrls2 := ((c shr  16) and $ff);
        a^.ctrls1 := ((c shr   8) and $ff);
        a^.ctrls0 :=  (c and $ff);
end;

function XkbActionCtrls (a : PXkbCtrlsAction) : Word;
begin
{#define XkbActionCtrls(a) ((((unsigned int)(a)->ctrls3)<<24)|(((unsigned int)(a)->ctrls2)<<16)|
                            (((unsigned int)(a)->ctrls1)<<8)|((unsigned int)((a)->ctrls0)))      }
        Result := (a^.ctrls3 shl 24) or
                  (a^.ctrls2 shl 16) or
                  (a^.ctrls1 shl  8) or
                   a^.ctrls0;
end;

function XkbSARedirectVMods (a : PXkbRedirectKeyAction) : Word;
begin
{#define XkbSARedirectVMods(a) ((((unsigned int)(a)->vmods1)<<8)|((unsigned int)(a)->vmods0))}
        Result := (a^.vmods1 shl 8) or
                                                a^.vmods0;
end;

procedure XkbSARedirectSetVMods (a : PXkbRedirectKeyAction; m : Byte);
begin
{#define XkbSARedirectSetVMods(a,m) (((a)->vmods_mask1=(((m)>>8)&0xff)),((a)->vmods_mask0=((m)&0xff)))}
        a^.vmods_mask1 := (m shr 8) and $ff;
        a^.vmods_mask0 :=  m or $ff;
end;

function XkbSARedirectVModsMask (a : PXkbRedirectKeyAction) : Word;
begin
{#define XkbSARedirectVModsMask(a) ((((unsigned int)(a)->vmods_mask1)<<8)|
                                     ((unsigned int)(a)->vmods_mask0))}
        Result := (a^.vmods_mask1 shl 8) or
                   a^.vmods_mask0;
end;

procedure XkbSARedirectSetVModsMask (a : PXkbRedirectKeyAction; m : Byte);
begin
{#define XkbSARedirectSetVModsMask(a,m) (((a)->vmods_mask1=(((m)>>8)&0xff)),((a)->vmods_mask0=((m)&0xff)))}
        a^.vmods_mask1 := (m shr 8) and $ff;
        a^.vmods_mask0 := (m and $ff);
end;

function XkbAX_AnyFeedback (c : PXkbControlsPtr) : Word;
begin
{#define XkbAX_AnyFeedback(c) ((c)->enabled_ctrls&XkbAccessXFeedbackMask)}
        Result := c^.enabled_ctrls and XkbAccessXFeedbackMask;
end;

function XkbAX_NeedOption (c : PXkbControlsPtr; w : Word) : Word;
begin
{#define XkbAX_NeedOption(c,w) ((c)->ax_options&(w))}
        Result := c^.ax_options and w;
end;

function XkbAX_NeedFeedback (c : PXkbControlsPtr; w : Word) : Boolean;
begin
{#define XkbAX_NeedFeedback(c,w) (XkbAX_AnyFeedback(c)&&XkbAX_NeedOption(c,w))}
        Result := (XkbAX_AnyFeedback (c) > 0) and (XkbAX_NeedOption (c, w) > 0);
end;

function XkbSMKeyActionsPtr (m : PXkbServerMapPtr; k : Word) : PXkbAction;
begin
{#define XkbSMKeyActionsPtr(m,k) (&(m)->acts[(m)->key_acts[k]])}
        Result := @m^.acts [m^.key_acts [k]];
end;

function XkbCMKeyGroupInfo (m : PXkbClientMapPtr; k : Word) : Byte;
begin
{#define XkbCMKeyGroupInfo(m,k) ((m)->key_sym_map[k].group_info)}
        Result := m^.key_sym_map [k].group_info;
end;

function XkbCMKeyNumGroups (m : PXkbClientMapPtr; k : Word) : Byte;
begin
{#define XkbCMKeyNumGroups(m,k) (XkbNumGroups((m)->key_sym_map[k].group_info))}
        Result := XkbNumGroups (m^.key_sym_map [k].group_info);
end;

function XkbCMKeyGroupWidth (m : PXkbClientMapPtr; k : Word; g : Byte) : Byte;
begin
{#define XkbCMKeyGroupWidth(m,k,g) (XkbCMKeyType(m,k,g)->num_levels)}
        Result := XkbCMKeyType(m,k,g)^.num_levels;
end;

function XkbCMKeyGroupsWidth (m : PXkbClientMapPtr; K : Word) : Byte;
begin
{#define XkbCMKeyGroupsWidth(m,k) ((m)->key_sym_map[k].width)}
        Result := m^.key_sym_map [k].width;
end;

function XkbCMKeyTypeIndex (m : PXkbClientMapPtr; k : Word; g : Byte) : Byte;
begin
{#define XkbCMKeyTypeIndex(m,k,g) ((m)->key_sym_map[k].kt_index[g&0x3])}
        Result := m^.key_sym_map [k].kt_index [g and $3];
end;

function XkbCMKeyType (m : PXkbClientMapPtr; k : Word; g : Byte) : PXkbKeyTypePtr;
begin
{#define XkbCMKeyType(m,k,g) (&(m)->types[XkbCMKeyTypeIndex(m,k,g)])}
        Result := @m^.types [XkbCMKeyTypeIndex(m,k,g)];
end;

function XkbCMKeyNumSyms (m : PXkbClientMapPtr; k : Word) : Word;
begin
{#define XkbCMKeyNumSyms(m,k) (XkbCMKeyGroupsWidth(m,k)*XkbCMKeyNumGroups(m,k))}
        Result := XkbCMKeyGroupsWidth (m,k) or XkbCMKeyNumGroups(m,k);
end;

function XkbCMKeySymsOffset (m : PXkbClientMapPtr; k : Word) : Byte;
begin
{#define XkbCMKeySymsOffset(m,k) ((m)->key_sym_map[k].offset)}
        Result := m^.key_sym_map [k].offset;
end;

function XkbCMKeySymsPtr (m : PXkbClientMapPtr; k : Word) : PKeySym;
begin
{#define XkbCMKeySymsPtr(m,k) (&(m)->syms[XkbCMKeySymsOffset(m,k)])}
        Result := @m^.syms [XkbCMKeySymsOffset(m,k)];
end;

function XkbIM_IsAuto (i : PXkbIndicatorMapPtr) : Boolean;
begin
{#define XkbIM_IsAuto(i) ((((i)->flags&XkbIM_NoAutomatic)==0)&&(((i)->which_groups&&(i)->groups)||
                           ((i)->which_mods&&(i)->mods.mask)||  ((i)->ctrls)))}
        Result := ((i^.flags and XkbIM_NoAutomatic) = 0) and
                   (((i^.which_groups > 0) and (i^.groups    > 0)) or
                    ((i^.which_mods   > 0) and (i^.mods.mask > 0)) or
                     (i^.ctrls        > 0));
end;

function XkbIM_InUse  (i : PXkbIndicatorMapPtr) : Boolean;
begin
{#define XkbIM_InUse(i) (((i)->flags)||((i)->which_groups)||((i)->which_mods)||((i)->ctrls)) }
        Result := (i^.flags > 0) or (i^.which_groups > 0) or (i^.which_mods > 0) or (i^.ctrls > 0);
end;

function XkbKeyKeyTypeIndex (d : PXkbDescPtr; k : Word; g : Byte) : Byte;
begin
{#define XkbKeyKeyTypeIndex(d,k,g)      (XkbCMKeyTypeIndex((d)->map,k,g))}
        Result := XkbCMKeyTypeIndex(d^.map, k, g);
end;

function XkbKeyKeyType (d : PXkbDescPtr; k : Word; g : Byte) : PXkbKeyTypePtr;
begin
{#define XkbKeyKeyType(d,k,g) (XkbCMKeyType((d)->map,k,g))}
        Result := XkbCMKeyType(d^.map, k, g);
end;

function XkbKeyGroupWidth (d : PXkbDescPtr; k : Word; g : Byte) : Byte;
begin
{#define XkbKeyGroupWidth(d,k,g) (XkbCMKeyGroupWidth((d)->map,k,g))}
        Result := XkbCMKeyGroupWidth(d^.map, k, g);
end;

function XkbKeyGroupsWidth (d : PXkbDescPtr; k : Word) : Byte;
begin
{#define XkbKeyGroupsWidth(d,k) (XkbCMKeyGroupsWidth((d)->map,k))}
        Result := XkbCMKeyGroupsWidth (d^.map, k);
end;

function XkbKeyGroupInfo (d : PXkbDescPtr; k : Word) : Byte;
begin
{#define XkbKeyGroupInfo(d,k) (XkbCMKeyGroupInfo((d)->map,(k)))}
        Result := XkbCMKeyGroupInfo (d^.map, k);
end;

function XkbKeyNumGroups (d : PXkbDescPtr; k : Word) : Byte;
begin
{#define XkbKeyNumGroups(d,k) (XkbCMKeyNumGroups((d)->map,(k)))}
        Result := XkbCMKeyNumGroups(d^.map, k);
end;

function XkbKeyNumSyms (d : PXkbDescPtr; k : Word) : Word;
begin
{#define XkbKeyNumSyms(d,k) (XkbCMKeyNumSyms((d)->map,(k)))}
        Result := XkbCMKeyNumSyms (d^.map, k);
end;

function XkbKeySymsPtr (d : PXkbDescPtr; k : Word) : PKeySym;
begin
{#define XkbKeySymsPtr(d,k) (XkbCMKeySymsPtr((d)->map,(k)))}
        Result := XkbCMKeySymsPtr (d^.map, k);
end;

function XkbKeySym (d : PXkbDescPtr; k : Word; n : Word) : TKeySym;
begin
{#define XkbKeySym(d,k,n) (XkbKeySymsPtr(d,k)[n])}
        Result := XkbKeySymsPtr(d, k) [n];
end;

function XkbKeySymEntry (d : PXkbDescPtr; k : Word; sl : Word; g : Byte) : TKeySym;
begin
{#define XkbKeySymEntry(d,k,sl,g) (XkbKeySym(d,k,((XkbKeyGroupsWidth(d,k)*(g))+(sl))))}
        Result := XkbKeySym (d, k, (XkbKeyGroupsWidth(d, k) * g) + sl);
end;

function XkbKeyAction (d : PXkbDescPtr; k : Word; n  : Word) : PXkbAction;
begin
{#define XkbKeyAction(d,k,n) (XkbKeyHasActions(d,k)?&XkbKeyActionsPtr(d,k)[n]:NULL)}
        if (XkbKeyHasActions(d,k)) then
                        Result := @TXkbAction(XkbKeyActionsPtr(d,k)[n]) //Buggy !!!
        else
                FillChar (Result, sizeof (TXkbAction), #0);
end;

function XkbKeyActionEntry (d : PXkbDescPtr; k : Word; sl : Word; g : Byte) : Byte;
begin
{#define XkbKeyActionEntry(d,k,sl,g) (XkbKeyHasActions(d,k) ?
                                      XkbKeyAction(d, k, ((XkbKeyGroupsWidth(d, k) * (g))+(sl))):NULL)}
        if (XkbKeyHasActions(d, k)) then
                Result := (XkbKeyGroupsWidth(d, k) * g) + sl
        else
                Result := 0;
end;

function XkbKeyHasActions (d : PXkbDescPtr; k : Word) : Boolean;
begin
{#define XkbKeyHasActions(d,k) ((d)->server->key_acts[k]!=0)}
        Result := (not (d^.server^.key_acts [k] = 0));
end;

function XkbKeyNumActions (d : PXkbDescPtr; k : Word) : Word;
begin
{#define XkbKeyNumActions(d,k) (XkbKeyHasActions(d,k)?XkbKeyNumSyms(d,k):1)}
        if (XkbKeyHasActions(d,k)) then
                Result := XkbKeyNumSyms(d,k)
        else
                Result := 1;
end;

function XkbKeyActionsPtr (d : PXkbDescPtr; k : Word) : PXkbAction;
begin
{#define XkbKeyActionsPtr(d,k) (XkbSMKeyActionsPtr((d)->server,k))}
        Result := XkbSMKeyActionsPtr(d^.server, k);
end;

function XkbKeycodeInRange (d : PXkbDescPtr; k : Word) : Boolean;
begin
{#define XkbKeycodeInRange(d,k) (((k)>=(d)->min_key_code)&& ((k)<=(d)->max_key_code))}
        Result := (k >= d^.min_key_code) and (k <= d^.max_key_code);
end;

function XkbNumKeys (d : PXkbDescPtr) : Byte;
begin
{#define XkbNumKeys(d) ((d)->max_key_code-(d)->min_key_code+1)}
        Result := d^.max_key_code - d^.min_key_code + 1;
end;

function XkbXI_DevHasBtnActs (d : PXkbDeviceInfoPtr) : Boolean;
begin
{#define XkbXI_DevHasBtnActs(d) (((d)->num_btns>0)&&((d)->btn_acts!=NULL))}
        Result := (d^.num_btns > 0) and (not (d^.btn_acts = nil));
end;

function XkbXI_LegalDevBtn (d : PXkbDeviceInfoPtr; b : Word) : Boolean;
begin
{#define XkbXI_LegalDevBtn(d,b) (XkbXI_DevHasBtnActs(d)&&((b)<(d)->num_btns))}
        Result := XkbXI_DevHasBtnActs(d) and (b < d^.num_btns);
end;

function XkbXI_DevHasLeds (d : PXkbDeviceInfoPtr) : Boolean;
begin
{#define XkbXI_DevHasLeds(d) (((d)->num_leds>0)&&((d)->leds!=NULL))}
        Result := (d^.num_leds > 0) and (not (d^.leds = nil));
end;
(******************************** end of xkbstr ********************************)
(*********************************** xkbgeom ***********************************)

function XkbBoundsWidth (b : PXkbBoundsPtr) : SmallInt;
begin
{#define XkbBoundsWidth(b) (((b)->x2)-((b)->x1))}
        Result := b^.x2 - b^.x1;
end;

function XkbBoundsHeight (b : PXkbBoundsPtr) : SmallInt;
begin
{#define XkbBoundsHeight(b) (((b)->y2)-((b)->y1))}
        Result := b^.y2 - b^.y1;
end;

function XkbOutlineIndex (s : PXkbShapePtr; o : PXkbOutlinePtr) : longint;
begin
{#define XkbOutlineIndex(s,o) ((int)((o)-&(s)->outlines[0]))}
        Result := longint(o - @s^.outlines [0]);
end;

function XkbShapeDoodadColor (g : PXkbGeometryPtr; d : PXkbShapeDoodadPtr) : PXkbColorPtr;
begin
{#define XkbShapeDoodadColor(g,d) (&(g)->colors[(d)->color_ndx])}
        Result := @(g^.colors [d^.color_ndx]);
end;

function XkbShapeDoodadShape (g : PXkbGeometryPtr; d : PXkbShapeDoodadPtr) : PXkbShapePtr;
begin
{#define XkbShapeDoodadShape(g,d) (&(g)->shapes[(d)->shape_ndx])}
        Result := @g^.shapes [d^.shape_ndx];
end;

procedure XkbSetShapeDoodadColor (g : PXkbGeometryPtr; d : PXkbShapeDoodadPtr; c : PXkbColorPtr);
begin
{#define XkbSetShapeDoodadColor(g,d,c) ((d)->color_ndx= (c)-&(g)->colors[0])}
        d^.color_ndx := c - @g^.colors [0];
end;

procedure XkbSetShapeDoodadShape (g : PXkbGeometryPtr; d : PXkbShapeDoodadPtr; s : PXkbShapePtr);
begin
{#define XkbSetShapeDoodadShape(g,d,s) ((d)->shape_ndx= (s)-&(g)->shapes[0])}
        d^.shape_ndx := PXkbShapePtr(s) - @g^.shapes [0];
end;

function XkbTextDoodadColor (g : PXkbGeometryPtr; d : PXkbTextDoodadPtr) : PXkbColorPtr;
begin
{#define XkbTextDoodadColor(g,d) (&(g)->colors[(d)->color_ndx])}
        Result := @g^.colors [d^.color_ndx];
end;

procedure XkbSetTextDoodadColor (g : PXkbGeometryPtr; d : PXkbTextDoodadPtr; c : PXkbColorPtr);
begin
{#define XkbSetTextDoodadColor(g,d,c) ((d)->color_ndx= (c)-&(g)->colors[0])}
        d^.color_ndx := c - @g^.colors [0];
end;

function XkbIndicatorDoodadShape (g : PXkbGeometryPtr; d : PXkbIndicatorDoodadPtr) : PXkbShapeDoodadPtr;
begin
{#define XkbIndicatorDoodadShape(g,d) (&(g)->shapes[(d)->shape_ndx])}
        Result := PXkbShapeDoodadPtr(@g^.shapes [d^.shape_ndx]);
end;

function XkbIndicatorDoodadOnColor (g : PXkbGeometryPtr; d : PXkbIndicatorDoodadPtr) : PXkbColorPtr;
begin
{#define XkbIndicatorDoodadOnColor(g,d) (&(g)->colors[(d)->on_color_ndx])}
        Result := @g^.colors [d^.on_color_ndx];
end;

function XkbIndicatorDoodadOffColor (g : PXkbGeometryPtr; d : PXkbIndicatorDoodadPtr) : PXkbColorPtr;
begin
{#define XkbIndicatorDoodadOffColor(g,d) (&(g)->colors[(d)->off_color_ndx])}
        Result := @g^.colors [d^.off_color_ndx];
end;

procedure XkbSetIndicatorDoodadOnColor (g : PXkbGeometryPtr; d : PXkbIndicatorDoodadPtr;
                                        c : PXkbColorPtr);
begin
{#define XkbSetIndicatorDoodadOnColor(g,d,c) ((d)->on_color_ndx= (c)-&(g)->colors[0])}
        d^.on_color_ndx := c - @g^.colors [0];
end;

procedure XkbSetIndicatorDoodadOffColor (g : PXkbGeometryPtr; d : PXkbIndicatorDoodadPtr;
                                         c : PXkbColorPtr);
begin
{#define        XkbSetIndicatorDoodadOffColor(g,d,c) ((d)->off_color_ndx= (c)-&(g)->colors[0])}
        d^.off_color_ndx := c - @g^.colors [0];
end;

procedure XkbSetIndicatorDoodadShape (g : PXkbGeometryPtr; d : PXkbIndicatorDoodadPtr;
                                      s : PXkbShapeDoodadPtr);
begin
{#define XkbSetIndicatorDoodadShape(g,d,s) ((d)->shape_ndx= (s)-&(g)->shapes[0])}
        d^.shape_ndx := PXkbShapePtr(s) - @g^.shapes [0];
end;

function XkbLogoDoodadColor (g : PXkbGeometryPtr; d : PXkbLogoDoodadPtr) : PXkbColorPtr;
begin
{#define XkbLogoDoodadColor(g,d) (&(g)->colors[(d)->color_ndx])}
        Result := @g^.colors [d^.color_ndx];
end;

function XkbLogoDoodadShape (g : PXkbGeometryPtr; d : PXkbLogoDoodadPtr) : PXkbShapeDoodadPtr;
begin
{#define XkbLogoDoodadShape(g,d) (&(g)->shapes[(d)->shape_ndx])}
        Result := PXkbShapeDoodadPtr(@g^.shapes [d^.shape_ndx]);
end;

procedure XkbSetLogoDoodadColor (g : PXkbGeometryPtr; d : PXkbLogoDoodadPtr; c : PXkbColorPtr);
begin
{#define XkbSetLogoDoodadColor(g,d,c) ((d)->color_ndx= (c)-&(g)->colors[0])}
        d^.color_ndx := c - @g^.colors [0];
end;

procedure XkbSetLogoDoodadShape (g : PXkbGeometryPtr; d : PXkbLogoDoodadPtr; s : PXkbShapeDoodadPtr);
begin
{#define XkbSetLogoDoodadShape(g,d,s) ((d)->shape_ndx= (s)-&(g)->shapes[0])}
        d^.shape_ndx := PXkbShapePtr(s) - @g^.shapes [0];
end;

function XkbKeyShape (g : PXkbGeometryPtr; k : PXkbKeyPtr) : PXkbShapeDoodadPtr;
begin
{#define XkbKeyShape(g,k) (&(g)->shapes[(k)->shape_ndx])}
        Result := PXkbShapeDoodadPtr(@g^.shapes [k^.shape_ndx]);
end;

function XkbKeyColor (g : PXkbGeometryPtr; k : PXkbKeyPtr) : PXkbColorPtr;
begin
{#define XkbKeyColor(g,k) (&(g)->colors[(k)->color_ndx])}
        Result := @g^.colors [k^.color_ndx];
end;

procedure XkbSetKeyShape (g : PXkbGeometryPtr; k : PXkbKeyPtr; s : PXkbShapeDoodadPtr);
begin
{#define XkbSetKeyShape(g,k,s) ((k)->shape_ndx= (s)-&(g)->shapes[0])}
        k^.shape_ndx := PXkbShapePtr(s) - @g^.shapes [0];
end;

procedure XkbSetKeyColor (g : PXkbGeometryPtr; k : PXkbKeyPtr; c : PXkbColorPtr);
begin
{#define XkbSetKeyColor(g,k,c) ((k)->color_ndx= (c)-&(g)->colors[0])}
        k^.color_ndx := c - @g^.colors [0];
end;

function XkbGeomColorIndex (g : PXkbGeometryPtr; c : PXkbColorPtr) : longint;
begin
{#define XkbGeomColorIndex(g,c) ((int)((c)-&(g)->colors[0]))}
        Result := longint (c - @g^.colors [0]);
end;
(******************************** end of xkbgeom *******************************)

end.
