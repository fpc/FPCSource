{
 $Xorg: XKB.h,v 1.3 2000/08/18 04:05:45 coskrey Exp $
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

 Pascal Convertion was made by Ido Kannner.

History:
        2004/09/15 - 2004/09/16 - Convertion from the c header of XKB.h.
                                                                                                          Thanks for mmc in #xlib on freenode irc for the help
                                                                                                                in understanding some of the problems I had converting
                                                                                                                this header.
}
{$PACKRECORDS C}
{$MODE OBJFPC} {$MACRO ON} {$DEFINE MACRO}
{.$DEFINE XKBstr_UNIT} {.$DEFINE XI_UNIT}
unit XKB;
interface
{$IFDEF XKBstr_UNIT}
uses XKBstr;
{$ENDIF}

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
        //XkbAllControlEventsMask = XkbAllControlsMask;

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

{$IFNDEF XI_UNIT}
        KbdFeedbackClass      = 0;
        LedFeedbackClass      = 4;
        BellFeedbackClass     = 5;
{$ENDIF}

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
      Macros to classify key actions
                }
{$IFNDEF XKBstr_UNIT}
const
        XkbAnyActionDataSize = 7;

type
        PXkbAnyAction = ^TXkbAnyAction;
        TXkbAnyAction = packed record
                         _type : byte;
                                                                         data : array [0..XkbAnyActionDataSize-1] of byte;
                        end;
{$ENDIF}

function XkbIsModAction   (a : PXkbAnyAction) : LongBool;
function XkbIsGroupAction (a : PXkbAnyAction) : LongBool;
function XkbIsPtrAction   (a : PXkbAnyAction) : LongBool;

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
        XkbPerKeyBitArraySize = (XkbMaxLegalKeyCode + 1) / 8;

function XkbIsLegalKeycode (const k : Cardinal) : LongBool;

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

implementation
{$IFDEF XI_UNIT}
uses xi;
{$ENDIF}

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

end.
