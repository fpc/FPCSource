{
     File:       HIToolbox/Events.h
 
     Contains:   Event Manager Interfaces.
 
     Version:    HIToolbox-219.4.81~2
 
     Copyright:  © 1985-2005 by Apple Computer, Inc., all rights reserved
 
     Bugs?:      For bug reports, consult the following page on
                 the World Wide Web:
 
                     http://www.freepascal.org/bugs.html
 
}
{       Pascal Translation Updated:  Peter N Lewis, <peter@stairways.com.au>, August 2005 }
{
    Modified for use with Free Pascal
    Version 210
    Please report any bugs to <gpc@microbizz.nl>
}

{$mode macpas}
{$packenum 1}
{$macro on}
{$inline on}
{$calling mwpascal}

unit Events;
interface
{$setc UNIVERSAL_INTERFACES_VERSION := $0342}
{$setc GAP_INTERFACES_VERSION := $0210}

{$ifc not defined USE_CFSTR_CONSTANT_MACROS}
    {$setc USE_CFSTR_CONSTANT_MACROS := TRUE}
{$endc}

{$ifc defined CPUPOWERPC and defined CPUI386}
	{$error Conflicting initial definitions for CPUPOWERPC and CPUI386}
{$endc}
{$ifc defined FPC_BIG_ENDIAN and defined FPC_LITTLE_ENDIAN}
	{$error Conflicting initial definitions for FPC_BIG_ENDIAN and FPC_LITTLE_ENDIAN}
{$endc}

{$ifc not defined __ppc__ and defined CPUPOWERPC}
	{$setc __ppc__ := 1}
{$elsec}
	{$setc __ppc__ := 0}
{$endc}
{$ifc not defined __i386__ and defined CPUI386}
	{$setc __i386__ := 1}
{$elsec}
	{$setc __i386__ := 0}
{$endc}

{$ifc defined __ppc__ and __ppc__ and defined __i386__ and __i386__}
	{$error Conflicting definitions for __ppc__ and __i386__}
{$endc}

{$ifc defined __ppc__ and __ppc__}
	{$setc TARGET_CPU_PPC := TRUE}
	{$setc TARGET_CPU_X86 := FALSE}
{$elifc defined __i386__ and __i386__}
	{$setc TARGET_CPU_PPC := FALSE}
	{$setc TARGET_CPU_X86 := TRUE}
{$elsec}
	{$error Neither __ppc__ nor __i386__ is defined.}
{$endc}
{$setc TARGET_CPU_PPC_64 := FALSE}

{$ifc defined FPC_BIG_ENDIAN}
	{$setc TARGET_RT_BIG_ENDIAN := TRUE}
	{$setc TARGET_RT_LITTLE_ENDIAN := FALSE}
{$elifc defined FPC_LITTLE_ENDIAN}
	{$setc TARGET_RT_BIG_ENDIAN := FALSE}
	{$setc TARGET_RT_LITTLE_ENDIAN := TRUE}
{$elsec}
	{$error Neither FPC_BIG_ENDIAN nor FPC_LITTLE_ENDIAN are defined.}
{$endc}
{$setc ACCESSOR_CALLS_ARE_FUNCTIONS := TRUE}
{$setc CALL_NOT_IN_CARBON := FALSE}
{$setc OLDROUTINENAMES := FALSE}
{$setc OPAQUE_TOOLBOX_STRUCTS := TRUE}
{$setc OPAQUE_UPP_TYPES := TRUE}
{$setc OTCARBONAPPLICATION := TRUE}
{$setc OTKERNEL := FALSE}
{$setc PM_USE_SESSION_APIS := TRUE}
{$setc TARGET_API_MAC_CARBON := TRUE}
{$setc TARGET_API_MAC_OS8 := FALSE}
{$setc TARGET_API_MAC_OSX := TRUE}
{$setc TARGET_CARBON := TRUE}
{$setc TARGET_CPU_68K := FALSE}
{$setc TARGET_CPU_MIPS := FALSE}
{$setc TARGET_CPU_SPARC := FALSE}
{$setc TARGET_OS_MAC := TRUE}
{$setc TARGET_OS_UNIX := FALSE}
{$setc TARGET_OS_WIN32 := FALSE}
{$setc TARGET_RT_MAC_68881 := FALSE}
{$setc TARGET_RT_MAC_CFM := FALSE}
{$setc TARGET_RT_MAC_MACHO := TRUE}
{$setc TYPED_FUNCTION_POINTERS := TRUE}
{$setc TYPE_BOOL := FALSE}
{$setc TYPE_EXTENDED := FALSE}
{$setc TYPE_LONGLONG := TRUE}
uses MacTypes,OSUtils,Quickdraw,Endian;


{$ALIGN MAC68K}

type
	EventKind = UInt16;
type
	EventMask = UInt16;
const
	nullEvent = 0;
	mouseDown = 1;
	mouseUp = 2;
	keyDown = 3;
	keyUp = 4;
	autoKey = 5;
	updateEvt = 6;
	diskEvt = 7;    { Not sent in Carbon. See kEventClassVolume in CarbonEvents.h}
	activateEvt = 8;
	osEvt = 15;
	kHighLevelEvent = 23;

const
	mDownMask = 1 shl mouseDown; { mouse button pressed}
	mUpMask = 1 shl mouseUp; { mouse button released}
	keyDownMask = 1 shl keyDown; { key pressed}
	keyUpMask = 1 shl keyUp; { key released}
	autoKeyMask = 1 shl autoKey; { key repeatedly held down}
	updateMask = 1 shl updateEvt; { window needs updating}
	diskMask = 1 shl diskEvt; { disk inserted}
	activMask = 1 shl activateEvt; { activate/deactivate window}
	highLevelEventMask = $0400; { high-level events (includes AppleEvents)}
	osMask = 1 shl osEvt; { operating system events (suspend, resume)}
	everyEvent = $FFFF; { all of the above}

const
	charCodeMask = $000000FF;
	keyCodeMask = $0000FF00;
	adbAddrMask = $00FF0000;
	osEvtMessageMask = $FF000000;

const
{ OS event messages.  Event (sub)code is in the high byte of the message field.}
	mouseMovedMessage = $00FA;
	suspendResumeMessage = $0001;

const
	resumeFlag = 1;     { Bit 0 of message indicates resume vs suspend}


{
    CARBON ALERT! BATTLESTATIONS!
    
    The EventModifiers bits defined here are also used in the newer Carbon Event
    key modifiers parameters. There are two main differences:
    
    1)  The Carbon key modifiers parameter is a UInt32, not a UInt16. Never try to
        extract the key modifiers parameter from a Carbon Event into an EventModifiers
        type. You will probably get your stack trashed.
    2)  The Carbon key modifiers is just that: key modifiers. That parameter will
        never contain the button state bit.
}
type
	EventModifiers = UInt16;
const
{ modifiers }
	activeFlagBit = 0;    { activate? (activateEvt and mouseDown)}
	btnStateBit = 7;    { state of button?}
	cmdKeyBit = 8;    { command key down?}
	shiftKeyBit = 9;    { shift key down?}
	alphaLockBit = 10;   { alpha lock down?}
	optionKeyBit = 11;   { option key down?}
	controlKeyBit = 12;   { control key down?}
	rightShiftKeyBit = 13;   { right shift key down? Not supported on Mac OS X.}
	rightOptionKeyBit = 14;   { right Option key down? Not supported on Mac OS X.}
	rightControlKeyBit = 15;    { right Control key down? Not supported on Mac OS X.}

const
	activeFlag = 1 shl activeFlagBit;
	btnState = 1 shl btnStateBit;
	cmdKey = 1 shl cmdKeyBit;
	shiftKey = 1 shl shiftKeyBit;
	alphaLock = 1 shl alphaLockBit;
	optionKey = 1 shl optionKeyBit;
	controlKey = 1 shl controlKeyBit;
	rightShiftKey = 1 shl rightShiftKeyBit; { Not supported on Mac OS X.}
	rightOptionKey = 1 shl rightOptionKeyBit; { Not supported on Mac OS X.}
	rightControlKey = 1 shl rightControlKeyBit; { Not supported on Mac OS X.}

{ MacRoman character codes}
const
	kNullCharCode = 0;
	kHomeCharCode = 1;
	kEnterCharCode = 3;
	kEndCharCode = 4;
	kHelpCharCode = 5;
	kBellCharCode = 7;
	kBackspaceCharCode = 8;
	kTabCharCode = 9;
	kLineFeedCharCode = 10;
	kVerticalTabCharCode = 11;
	kPageUpCharCode = 11;
	kFormFeedCharCode = 12;
	kPageDownCharCode = 12;
	kReturnCharCode = 13;
	kFunctionKeyCharCode = 16;
	kCommandCharCode = 17;   { glyph available only in system fonts}
	kCheckCharCode = 18;   { glyph available only in system fonts}
	kDiamondCharCode = 19;   { glyph available only in system fonts}
	kAppleLogoCharCode = 20;   { glyph available only in system fonts}
	kEscapeCharCode = 27;
	kClearCharCode = 27;
	kLeftArrowCharCode = 28;
	kRightArrowCharCode = 29;
	kUpArrowCharCode = 30;
	kDownArrowCharCode = 31;
	kSpaceCharCode = 32;
	kDeleteCharCode = 127;
	kBulletCharCode = 165;
	kNonBreakingSpaceCharCode = 202;

{ useful Unicode code points}
const
	kShiftUnicode = $21E7; { Unicode UPWARDS WHITE ARROW}
	kControlUnicode = $2303; { Unicode UP ARROWHEAD}
	kOptionUnicode = $2325; { Unicode OPTION KEY}
	kCommandUnicode = $2318; { Unicode PLACE OF INTEREST SIGN}
	kPencilUnicode = $270E; { Unicode LOWER RIGHT PENCIL; actually pointed left until Mac OS X 10.3}
	kPencilLeftUnicode = $F802; { Unicode LOWER LEFT PENCIL; available in Mac OS X 10.3 and later}
	kCheckUnicode = $2713; { Unicode CHECK MARK}
	kDiamondUnicode = $25C6; { Unicode BLACK DIAMOND}
	kBulletUnicode = $2022; { Unicode BULLET}
	kAppleLogoUnicode = $F8FF; { Unicode APPLE LOGO}

type
	EventRecord = record
		what: EventKind;
		message: UInt32;
		when: UInt32;
		where: Point;
		modifiers: EventModifiers;
	end;
	EventRecordPtr = ^EventRecord;
type
	FKEYProcPtr = procedure;
type
	FKEYUPP = FKEYProcPtr;
{
 *  NewFKEYUPP()
 *  
 *  Availability:
 *    Mac OS X:         not available
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   available as macro/inline
 }

{
 *  DisposeFKEYUPP()
 *  
 *  Availability:
 *    Mac OS X:         not available
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   available as macro/inline
 }

{
 *  InvokeFKEYUPP()
 *  
 *  Availability:
 *    Mac OS X:         not available
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   available as macro/inline
 }

{
 *  GetMouse()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
procedure GetMouse( var mouseLoc: Point ); external name '_GetMouse';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  Button()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function Button: Boolean; external name '_Button';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  StillDown()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function StillDown: Boolean; external name '_StillDown';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  WaitMouseUp()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function WaitMouseUp: Boolean; external name '_WaitMouseUp';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  KeyTranslate()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function KeyTranslate( transData: {const} UnivPtr; keycode: UInt16; var state: UInt32 ): UInt32; external name '_KeyTranslate';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  GetCaretTime()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function GetCaretTime: UInt32; external name '_GetCaretTime';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{ 
    QuickTime 3.0 supports GetKeys() on unix and win32
}
type
    KeyMap = packed array [0..127] of boolean;
{
 *  GetKeys()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
procedure __GetKeys( var theKeys: KeyMap ); external name '_GetKeys';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
		With GPC and FPC on PowerPC, the bytes of the KeyMap must be swapped
}

procedure GetKeys( var theKeys: KeyMap );

{ Obsolete event types & masks }
const
	networkEvt = 10;
	driverEvt = 11;
	app1Evt = 12;
	app2Evt = 13;
	app3Evt = 14;
	app4Evt = 15;
	networkMask = $0400;
	driverMask = $0800;
	app1Mask = $1000;
	app2Mask = $2000;
	app3Mask = $4000;
	app4Mask = $8000;

type
	EvQEl = record
		qLink: QElemPtr;
		qType: SInt16;
		evtQWhat: EventKind;               { this part is identical to the EventRecord as defined above }
		evtQMessage: UInt32;
		evtQWhen: UInt32;
		evtQWhere: Point;
		evtQModifiers: EventModifiers;
	end;
	EvQElPtr = ^EvQEl;
type
	GetNextEventFilterProcPtr = procedure( var theEvent: EventRecord; var result: Boolean );
type
	GetNextEventFilterUPP = GetNextEventFilterProcPtr;
{
 *  NewGetNextEventFilterUPP()
 *  
 *  Availability:
 *    Mac OS X:         not available
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   available as macro/inline
 }

{
 *  DisposeGetNextEventFilterUPP()
 *  
 *  Availability:
 *    Mac OS X:         not available
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   available as macro/inline
 }

{
 *  InvokeGetNextEventFilterUPP()
 *  
 *  Availability:
 *    Mac OS X:         not available
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   available as macro/inline
 }

type
	GNEFilterUPP = GetNextEventFilterUPP;
{
 *  GetDblTime()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function GetDblTime: UInt32; external name '_GetDblTime';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  SetEventMask()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
procedure SetEventMask( value: EventMask ); external name '_SetEventMask';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  GetEvQHdr()
 *  
 *  Availability:
 *    Mac OS X:         not available
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }


{
 *  PPostEvent()
 *  
 *  Availability:
 *    Mac OS X:         not available
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }


{
 *  GetNextEvent()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function GetNextEvent( eventMask_: EventMask; var theEvent: EventRecord ): Boolean; external name '_GetNextEvent';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  WaitNextEvent()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function WaitNextEvent( eventMask_: EventMask; var theEvent: EventRecord; sleep: UInt32; mouseRgn: RgnHandle { can be NULL } ): Boolean; external name '_WaitNextEvent';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  EventAvail()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function EventAvail( eventMask_: EventMask; var theEvent: EventRecord ): Boolean; external name '_EventAvail';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  PostEvent()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PostEvent( eventNum: EventKind; eventMsg: UInt32 ): OSErr; external name '_PostEvent';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
    For Carbon, use EventAvail, TickCount, GetGlobalMouse,
    GetKeys, or GetCurrentKeyModifiers instead of
    OSEventAvail, and use GetNextEvent or WaitNextEvent
    instead of GetOSEvent.
}

{
 *  OSEventAvail()
 *  
 *  Availability:
 *    Mac OS X:         not available
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }


{
 *  GetOSEvent()
 *  
 *  Availability:
 *    Mac OS X:         not available
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }


{
 *  FlushEvents()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
procedure FlushEvents( whichMask: EventMask; stopMask: EventMask ); external name '_FlushEvents';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  SystemClick()
 *  
 *  Availability:
 *    Mac OS X:         not available
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }


{
 *  SystemTask()
 *  
 *  Availability:
 *    Mac OS X:         not available
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }


{
 *  SystemEvent()
 *  
 *  Availability:
 *    Mac OS X:         not available
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }

(*
#if OLDROUTINENAMES
#define KeyTrans(transData, keycode, state) KeyTranslate(transData, keycode, state)
#endif  { OLDROUTINENAMES }
*)
{
    GetGlobalMouse, GetCurrentKeyModifiers, and CheckEventQueueForUserCancel
    are only available as part of the Carbon API.
}

{
 *  GetGlobalMouse()
 *  
 *  Summary:
 *    Returns the position of the mouse in global coordinates.
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Parameters:
 *    
 *    globalMouse:
 *      On exit, contains the mouse position in global coordinates.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in CarbonAccessors.o 1.0 and later
 }
procedure GetGlobalMouse( var globalMouse: Point ); external name '_GetGlobalMouse';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  GetCurrentKeyModifiers()
 *  
 *  Summary:
 *    Returns the current hardware keyboard modifier state.
 *  
 *  Discussion:
 *    In most cases, you should not use GetCurrentKeyModifiers, but
 *    should use the GetCurrentEventKeyModifiers function instead.
 *    GetCurrentEventKeyModifiers is much faster than
 *    GetCurrentKeyModifiers because it returns the locally cached
 *    modifier state; GetCurrentKeyModifiers must get the modifier
 *    state from the window server, which is slower. Using
 *    GetCurrentKeyModifiers also can prevent your application from
 *    being operated by remote posting of events, since the hardware
 *    input device is not actually changing state in that case. Most
 *    commonly, you might need to use GetCurrentKeyModifiers when your
 *    application is not the active application (as determined by the
 *    Process Manager function GetFrontProcess). In that case, the
 *    cached modifier state returned by GetCurrentEventKeyModifiers is
 *    not valid because modifier-changed events are not flowing to your
 *    application, and you must use GetCurrentKeyModifiers to determine
 *    the current hardware state.
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Result:
 *    The hardware state of the keyboard modifiers. The format of the
 *    return value is the same as the modifiers field of an EventRecord
 *    (but only includes keyboard modifiers and not the other modifier
 *    flags included in an EventRecord).
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   not available
 }
function GetCurrentKeyModifiers: UInt32; external name '_GetCurrentKeyModifiers';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  CheckEventQueueForUserCancel()
 *  
 *  Summary:
 *    Determines if there is a cancel event in the main thread's event
 *    queue.
 *  
 *  Discussion:
 *    This API supports two cancel events: Escape and Cmd-Period. The
 *    cancel event itself, as well as mouse or keyboard events in front
 *    of the cancel event in the event queue, will be removed from the
 *    queue.
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Non-Carbon CFM:   not available
 }
function CheckEventQueueForUserCancel: Boolean; external name '_CheckEventQueueForUserCancel';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  KeyScript()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
procedure KeyScript( code: SInt16 ); external name '_KeyScript';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  IsCmdChar()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function IsCmdChar( const (*var*) event: EventRecord; test: SInt16 ): Boolean; external name '_IsCmdChar';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{ 
    LowMem accessor functions previously in LowMem.h
}
{
 *  LMGetKeyThresh()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function LMGetKeyThresh: SInt16; external name '_LMGetKeyThresh';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  LMSetKeyThresh()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
procedure LMSetKeyThresh( value: SInt16 ); external name '_LMSetKeyThresh';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  LMGetKeyRepThresh()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function LMGetKeyRepThresh: SInt16; external name '_LMGetKeyRepThresh';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  LMSetKeyRepThresh()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
procedure LMSetKeyRepThresh( value: SInt16 ); external name '_LMSetKeyRepThresh';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  LMGetKbdLast()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function LMGetKbdLast: UInt8; external name '_LMGetKbdLast';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  LMSetKbdLast()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
procedure LMSetKbdLast( value: UInt8 ); external name '_LMSetKbdLast';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  LMGetKbdType()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function LMGetKbdType: UInt8; external name '_LMGetKbdType';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  LMSetKbdType()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
procedure LMSetKbdType( value: UInt8 ); external name '_LMSetKbdType';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


implementation



{$ifc TARGET_RT_BIG_ENDIAN}

procedure GetKeys( var theKeys: KeyMap );
var
	theReverseKeys: KeyMap;
	theKey: 0..127;
begin
	__GetKeys( theReverseKeys);
	for theKey:= 0 to 127 do
		theKeys[ theKey]:= theReverseKeys[ ((theKey div 8) * 8) + (7 - (theKey mod 8))]
end;

{$elsec}

procedure GetKeys( var theKeys: KeyMap );
begin
	__GetKeys( theKeys)
end;

{$endc}


end.
