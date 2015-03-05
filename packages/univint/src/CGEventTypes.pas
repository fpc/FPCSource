{ CoreGraphics - CGEventTypes.h
   Copyright (c) 2004-2008 Apple Inc.
   All rights reserved. }
{       Pascal Translation:  Peter N Lewis, <peter@stairways.com.au>, August 2005 }
{       Pascal Translation Updated:  Jonas Maebe, <jonas@freepascal.org>, October 2009 }
{       Pascal Translation Updated:  Jonas Maebe, <jonas@freepascal.org>, October 2012 }
{
    Modified for use with Free Pascal
    Version 308
    Please report any bugs to <gpc@microbizz.nl>
}

{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}
{$mode macpas}
{$packenum 1}
{$macro on}
{$inline on}
{$calling mwpascal}

unit CGEventTypes;
interface
{$setc UNIVERSAL_INTERFACES_VERSION := $0400}
{$setc GAP_INTERFACES_VERSION := $0308}

{$ifc not defined USE_CFSTR_CONSTANT_MACROS}
    {$setc USE_CFSTR_CONSTANT_MACROS := TRUE}
{$endc}

{$ifc defined CPUPOWERPC and defined CPUI386}
	{$error Conflicting initial definitions for CPUPOWERPC and CPUI386}
{$endc}
{$ifc defined FPC_BIG_ENDIAN and defined FPC_LITTLE_ENDIAN}
	{$error Conflicting initial definitions for FPC_BIG_ENDIAN and FPC_LITTLE_ENDIAN}
{$endc}

{$ifc not defined __ppc__ and defined CPUPOWERPC32}
	{$setc __ppc__ := 1}
{$elsec}
	{$setc __ppc__ := 0}
{$endc}
{$ifc not defined __ppc64__ and defined CPUPOWERPC64}
	{$setc __ppc64__ := 1}
{$elsec}
	{$setc __ppc64__ := 0}
{$endc}
{$ifc not defined __i386__ and defined CPUI386}
	{$setc __i386__ := 1}
{$elsec}
	{$setc __i386__ := 0}
{$endc}
{$ifc not defined __x86_64__ and defined CPUX86_64}
	{$setc __x86_64__ := 1}
{$elsec}
	{$setc __x86_64__ := 0}
{$endc}
{$ifc not defined __arm__ and defined CPUARM}
	{$setc __arm__ := 1}
{$elsec}
	{$setc __arm__ := 0}
{$endc}
{$ifc not defined __arm64__ and defined CPUAARCH64}
  {$setc __arm64__ := 1}
{$elsec}
  {$setc __arm64__ := 0}
{$endc}

{$ifc defined cpu64}
  {$setc __LP64__ := 1}
{$elsec}
  {$setc __LP64__ := 0}
{$endc}


{$ifc defined __ppc__ and __ppc__ and defined __i386__ and __i386__}
	{$error Conflicting definitions for __ppc__ and __i386__}
{$endc}

{$ifc defined __ppc__ and __ppc__}
	{$setc TARGET_CPU_PPC := TRUE}
	{$setc TARGET_CPU_PPC64 := FALSE}
	{$setc TARGET_CPU_X86 := FALSE}
	{$setc TARGET_CPU_X86_64 := FALSE}
	{$setc TARGET_CPU_ARM := FALSE}
	{$setc TARGET_CPU_ARM64 := FALSE}
	{$setc TARGET_OS_MAC := TRUE}
	{$setc TARGET_OS_IPHONE := FALSE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
	{$setc TARGET_OS_EMBEDDED := FALSE}
{$elifc defined __ppc64__ and __ppc64__}
	{$setc TARGET_CPU_PPC := FALSE}
	{$setc TARGET_CPU_PPC64 := TRUE}
	{$setc TARGET_CPU_X86 := FALSE}
	{$setc TARGET_CPU_X86_64 := FALSE}
	{$setc TARGET_CPU_ARM := FALSE}
	{$setc TARGET_CPU_ARM64 := FALSE}
	{$setc TARGET_OS_MAC := TRUE}
	{$setc TARGET_OS_IPHONE := FALSE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
	{$setc TARGET_OS_EMBEDDED := FALSE}
{$elifc defined __i386__ and __i386__}
	{$setc TARGET_CPU_PPC := FALSE}
	{$setc TARGET_CPU_PPC64 := FALSE}
	{$setc TARGET_CPU_X86 := TRUE}
	{$setc TARGET_CPU_X86_64 := FALSE}
	{$setc TARGET_CPU_ARM := FALSE}
	{$setc TARGET_CPU_ARM64 := FALSE}
{$ifc defined(iphonesim)}
 	{$setc TARGET_OS_MAC := FALSE}
	{$setc TARGET_OS_IPHONE := TRUE}
	{$setc TARGET_IPHONE_SIMULATOR := TRUE}
{$elsec}
	{$setc TARGET_OS_MAC := TRUE}
	{$setc TARGET_OS_IPHONE := FALSE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
{$endc}
	{$setc TARGET_OS_EMBEDDED := FALSE}
{$elifc defined __x86_64__ and __x86_64__}
	{$setc TARGET_CPU_PPC := FALSE}
	{$setc TARGET_CPU_PPC64 := FALSE}
	{$setc TARGET_CPU_X86 := FALSE}
	{$setc TARGET_CPU_X86_64 := TRUE}
	{$setc TARGET_CPU_ARM := FALSE}
	{$setc TARGET_CPU_ARM64 := FALSE}
{$ifc defined(iphonesim)}
 	{$setc TARGET_OS_MAC := FALSE}
	{$setc TARGET_OS_IPHONE := TRUE}
	{$setc TARGET_IPHONE_SIMULATOR := TRUE}
{$elsec}
	{$setc TARGET_OS_MAC := TRUE}
	{$setc TARGET_OS_IPHONE := FALSE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
{$endc}
	{$setc TARGET_OS_EMBEDDED := FALSE}
{$elifc defined __arm__ and __arm__}
	{$setc TARGET_CPU_PPC := FALSE}
	{$setc TARGET_CPU_PPC64 := FALSE}
	{$setc TARGET_CPU_X86 := FALSE}
	{$setc TARGET_CPU_X86_64 := FALSE}
	{$setc TARGET_CPU_ARM := TRUE}
	{$setc TARGET_CPU_ARM64 := FALSE}
	{ will require compiler define when/if other Apple devices with ARM cpus ship }
	{$setc TARGET_OS_MAC := FALSE}
	{$setc TARGET_OS_IPHONE := TRUE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
	{$setc TARGET_OS_EMBEDDED := TRUE}
{$elifc defined __arm64__ and __arm64__}
	{$setc TARGET_CPU_PPC := FALSE}
	{$setc TARGET_CPU_PPC64 := FALSE}
	{$setc TARGET_CPU_X86 := FALSE}
	{$setc TARGET_CPU_X86_64 := FALSE}
	{$setc TARGET_CPU_ARM := FALSE}
	{$setc TARGET_CPU_ARM64 := TRUE}
	{ will require compiler define when/if other Apple devices with ARM cpus ship }
	{$setc TARGET_OS_MAC := FALSE}
	{$setc TARGET_OS_IPHONE := TRUE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
	{$setc TARGET_OS_EMBEDDED := TRUE}
{$elsec}
	{$error __ppc__ nor __ppc64__ nor __i386__ nor __x86_64__ nor __arm__ nor __arm64__ is defined.}
{$endc}

{$ifc defined __LP64__ and __LP64__ }
  {$setc TARGET_CPU_64 := TRUE}
{$elsec}
  {$setc TARGET_CPU_64 := FALSE}
{$endc}

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
{$setc TARGET_OS_UNIX := FALSE}
{$setc TARGET_OS_WIN32 := FALSE}
{$setc TARGET_RT_MAC_68881 := FALSE}
{$setc TARGET_RT_MAC_CFM := FALSE}
{$setc TARGET_RT_MAC_MACHO := TRUE}
{$setc TYPED_FUNCTION_POINTERS := TRUE}
{$setc TYPE_BOOL := FALSE}
{$setc TYPE_EXTENDED := FALSE}
{$setc TYPE_LONGLONG := TRUE}
uses MacTypes,MacOSXPosix,CGRemoteOperation,CGBase;
{$endc} {not MACOSALLINCLUDE}

{$ALIGN POWER}


{ An opaque type that represents a low-level hardware event.

   Low-level hardware events of this type are referred to as Quartz events.
   A typical event in Mac OS X originates when the user manipulates an input
   device such as a mouse or a keyboard. The device driver associated with
   that device, through the I/O Kit, creates a low-level event, puts it in
   the window server’s event queue, and notifies the window server. The
   window server creates a Quartz event, annotates the event, and dispatches
   the event to the appropriate run-loop port of the target process. There
   the event is picked up by the Carbon Event Manager and forwarded to the
   event-handling mechanism appropriate to the application environment. You
   can use event taps to gain access to Quartz events at several different
   steps in this process.

   This opaque type is derived from `CFType' and inherits the properties
   that all Core Foundation types have in common. }

type
	CGEventRef = ^__CGEvent; { an opaque type }
	__CGEvent = record end;

{ Constants that specify buttons on a one, two, or three-button mouse. }
const
	kCGMouseButtonLeft = 0;
	kCGMouseButtonRight = 1;
	kCGMouseButtonCenter = 2;
type
	CGMouseButton = UInt32;

{ Constants that specify the unit of measurement for a scrolling event. }
const
	kCGScrollEventUnitPixel = 0;
	kCGScrollEventUnitLine = 1;
type
	CGScrollEventUnit = UInt32;

{ Constants that indicate the modifier key state at the time an event is
   created, as well as other event-related states.

   Any bits not specified are reserved for future use. }
(*
Uncomment when IOKit is translated

const { Masks for the bits in event flags }
{ Device-independent modifier key bits. }
	kCGEventFlagMaskAlphaShift = NX_ALPHASHIFTMASK;
	kCGEventFlagMaskShift = NX_SHIFTMASK;
	kCGEventFlagMaskControl = NX_CONTROLMASK;
	kCGEventFlagMaskAlternate = NX_ALTERNATEMASK;
	kCGEventFlagMaskCommand = NX_COMMANDMASK;

  { Special key identifiers. }
	kCGEventFlagMaskHelp = NX_HELPMASK;
	kCGEventFlagMaskSecondaryFn = NX_SECONDARYFNMASK;

  { Identifies key events from numeric keypad area on extended keyboards. }
	kCGEventFlagMaskNumericPad = NX_NUMERICPADMASK;

  { Indicates if mouse/pen movement events are not being coalesced }
	kCGEventFlagMaskNonCoalesced = NX_NONCOALSESCEDMASK;
*)
type
	CGEventFlags = UInt64;      { Flags for events }

{ Constants that specify the different types of input events. }

{ Event types }
type
	_CGEventType = SInt32;
(*
Uncomment when IOKit is translated

const
	kCGEventNull = NX_NULLEVENT;			{ Placeholder; the Null Event }
    { mouse events }
	kCGEventLeftMouseDown = NX_LMOUSEDOWN;		{ left mouse-down event }
	kCGEventLeftMouseUp = NX_LMOUSEUP;			{ left mouse-up event }
	kCGEventRightMouseDown = NX_RMOUSEDOWN;		{ right mouse-down event }
	kCGEventRightMouseUp = NX_RMOUSEUP;			{ right mouse-up event }
	kCGEventMouseMoved = NX_MOUSEMOVED;			{ mouse-moved event }
	kCGEventLeftMouseDragged = NX_LMOUSEDRAGGED;	{ left mouse-dragged event }
	kCGEventRightMouseDragged = NX_RMOUSEDRAGGED;	{ right mouse-dragged event }

    { keyboard events }
	kCGEventKeyDown = NX_KEYDOWN;			{ key-down event }
	kCGEventKeyUp = NX_KEYUP;				{ key-up event }
	kCGEventFlagsChanged = NX_FLAGSCHANGED;		{ flags-changed (modifier keys and status) event }

    { Specialized control devices }
	kCGEventScrollWheel = NX_SCROLLWHEELMOVED;		{ Scroll wheel input device }
	kCGEventTabletPointer = NX_TABLETPOINTER;		{ specialized tablet pointer event, in addition to tablet mouse event }
	kCGEventTabletProximity = NX_TABLETPROXIMITY;	{ specialized tablet proximity event, in addition to tablet mouse event }
	kCGEventOtherMouseDown = NX_OMOUSEDOWN;		{ Mouse button 2-31 down }
	kCGEventOtherMouseUp = NX_OMOUSEUP;			{ Mouse button 2-31 up }
	kCGEventOtherMouseDragged = NX_OMOUSEDRAGGED;	{ Drag with mouse button 2-31 down }
*)

    {
     * Out of band types, delivered for unusual conditions
     * These are delivered to the event tap callback to notify of unusual
     * conditions that disable the event tap.
     }
const
	kCGEventTapDisabledByTimeout = $FFFFFFFE;
	kCGEventTapDisabledByUserInput = $FFFFFFFF;
type
	CGEventType = UInt32;

{ Event timestamp; roughly, nanoseconds since startup. }
type
	CGEventTimestamp = UInt64;

{ Constants used as keys to access specialized fields in low-level events. }
const
{ Key to access an integer field that contains the mouse button event
     number. Matching mouse-down and mouse-up events will have the same
     event number. }
	kCGMouseEventNumber = 0;

  { Key to access an integer field that contains the mouse button click
  state. A click state of 1 represents a single click. A click state of 2
  represents a double-click. A click state of 3 represents a
  triple-click. }
	kCGMouseEventClickState = 1;

  { Key to access a double field that contains the mouse button pressure.
     The pressure value may range from 0 to 1, with 0 representing the mouse
     being up. This value is commonly set by tablet pens mimicking a
     mouse. }
	kCGMouseEventPressure = 2;

  { Key to access an integer field that contains the mouse button
     number. }
	kCGMouseEventButtonNumber = 3;

  { Key to access an integer field that contains the horizontal mouse delta
     since the last mouse movement event. }
	kCGMouseEventDeltaX = 4;

  { Key to access an integer field that contains the vertical mouse delta
     since the last mouse movement event. }
	kCGMouseEventDeltaY = 5;

  { Key to access an integer field. The value is non-zero if the event
     should be ignored by the Inkwell subsystem. }
	kCGMouseEventInstantMouser = 6;

  { Key to access an integer field that encodes the mouse event subtype as
     a `kCFNumberIntType'. }
	kCGMouseEventSubtype = 7;

  { Key to access an integer field, non-zero when this is an autorepeat of
     a key-down, and zero otherwise. }
	kCGKeyboardEventAutorepeat = 8;

  { Key to access an integer field that contains the virtual keycode of the
     key-down or key-up event. }
	kCGKeyboardEventKeycode = 9;

  { Key to access an integer field that contains the keyboard type
     identifier. }
	kCGKeyboardEventKeyboardType = 10;

  { Key to access an integer field that contains scrolling data. This field
     typically contains the change in vertical position since the last
     scrolling event from a Mighty Mouse scroller or a single-wheel mouse
     scroller. }
	kCGScrollWheelEventDeltaAxis1 = 11;

  { Key to access an integer field that contains scrolling data. This field
     typically contains the change in horizontal position since the last
     scrolling event from a Mighty Mouse scroller. }
	kCGScrollWheelEventDeltaAxis2 = 12;

  { This field is not used. }
	kCGScrollWheelEventDeltaAxis3 = 13;

  { Key to access a field that contains scrolling data. The scrolling data
     represents a line-based or pixel-based change in vertical position
     since the last scrolling event from a Mighty Mouse scroller or a
     single-wheel mouse scroller. The scrolling data uses a fixed-point
     16.16 signed integer format. If this key is passed to
     `CGEventGetDoubleValueField', the fixed-point value is converted to a
     double value. }
	kCGScrollWheelEventFixedPtDeltaAxis1 = 93;

  { Key to access a field that contains scrolling data. The scrolling data
     represents a line-based or pixel-based change in horizontal position
     since the last scrolling event from a Mighty Mouse scroller. The
     scrolling data uses a fixed-point 16.16 signed integer format. If this
     key is passed to `CGEventGetDoubleValueField', the fixed-point value is
     converted to a double value. }
	kCGScrollWheelEventFixedPtDeltaAxis2 = 94;

  { This field is not used. }
	kCGScrollWheelEventFixedPtDeltaAxis3 = 95;

  { Key to access an integer field that contains pixel-based scrolling
     data. The scrolling data represents the change in vertical position
     since the last scrolling event from a Mighty Mouse scroller or a
     single-wheel mouse scroller. }
	kCGScrollWheelEventPointDeltaAxis1 = 96;

  { Key to access an integer field that contains pixel-based scrolling
     data. The scrolling data represents the change in horizontal position
     since the last scrolling event from a Mighty Mouse scroller. }
	kCGScrollWheelEventPointDeltaAxis2 = 97;

  { This field is not used. }
	kCGScrollWheelEventPointDeltaAxis3 = 98;
    
  {  }
	kCGScrollWheelEventScrollPhase = 99;
    
  { rdar://11259169 }
	kCGScrollWheelEventScrollCount = 100;
    
  { Key to access an integer field that indicates whether the event should
     be ignored by the Inkwell subsystem. If the value is non-zero, the
     event should be ignored. }
	kCGScrollWheelEventInstantMouser = 14;

  { Key to access an integer field that contains the absolute X coordinate
     in tablet space at full tablet resolution. }
	kCGTabletEventPointX = 15;

  { Key to access an integer field that contains the absolute Y coordinate
     in tablet space at full tablet resolution. }
	kCGTabletEventPointY = 16;

  { Key to access an integer field that contains the absolute Z coordinate
     in tablet space at full tablet resolution. }
	kCGTabletEventPointZ = 17;

  { Key to access an integer field that contains the tablet button state.
     Bit 0 is the first button, and a set bit represents a closed or pressed
     button. Up to 16 buttons are supported. }
	kCGTabletEventPointButtons = 18;

  { Key to access a double field that contains the tablet pen pressure. A
     value of 0.0 represents no pressure, and 1.0 represents maximum
     pressure. }
	kCGTabletEventPointPressure = 19;

  { Key to access a double field that contains the horizontal tablet pen
     tilt. A value of 0 represents no tilt, and 1 represents maximum tilt. }
	kCGTabletEventTiltX = 20;

  { Key to access a double field that contains the vertical tablet pen
     tilt. A value of 0 represents no tilt, and 1 represents maximum tilt. }
	kCGTabletEventTiltY = 21;

  { Key to access a double field that contains the tablet pen rotation. }
	kCGTabletEventRotation = 22;

  { Key to access a double field that contains the tangential pressure on
     the device. A value of 0.0 represents no pressure, and 1.0 represents
     maximum pressure. }
	kCGTabletEventTangentialPressure = 23;

  { Key to access an integer field that contains the system-assigned unique
     device ID. }
	kCGTabletEventDeviceID = 24;

  { Key to access an integer field that contains a vendor-specified value. }
	kCGTabletEventVendor1 = 25;

  { Key to access an integer field that contains a vendor-specified value. }
	kCGTabletEventVendor2 = 26;

  { Key to access an integer field that contains a vendor-specified value. }
	kCGTabletEventVendor3 = 27;

  { Key to access an integer field that contains the vendor-defined ID,
     typically the USB vendor ID. }
	kCGTabletProximityEventVendorID = 28;

  { Key to access an integer field that contains the vendor-defined tablet
     ID, typically the USB product ID. }
	kCGTabletProximityEventTabletID = 29;

  { Key to access an integer field that contains the vendor-defined ID of
     the pointing device. }
	kCGTabletProximityEventPointerID = 30;

  { Key to access an integer field that contains the system-assigned device
     ID. }
	kCGTabletProximityEventDeviceID = 31;

  { Key to access an integer field that contains the system-assigned unique
     tablet ID. }
	kCGTabletProximityEventSystemTabletID = 32;

  { Key to access an integer field that contains the vendor-assigned
     pointer type. }
	kCGTabletProximityEventVendorPointerType = 33;

  { Key to access an integer field that contains the vendor-defined pointer
     serial number. }
	kCGTabletProximityEventVendorPointerSerialNumber = 34;

  { Key to access an integer field that contains the vendor-defined unique
     ID. }
	kCGTabletProximityEventVendorUniqueID = 35;

  { Key to access an integer field that contains the device capabilities
     mask. }
	kCGTabletProximityEventCapabilityMask = 36;

  { Key to access an integer field that contains the pointer type. }
	kCGTabletProximityEventPointerType = 37;

  { Key to access an integer field that indicates whether the pen is in
     proximity to the tablet. The value is non-zero if the pen is in
     proximity to the tablet and zero when leaving the tablet. }
	kCGTabletProximityEventEnterProximity = 38;

  { Key to access a field that contains the event target process serial
     number. The value is a 64-bit value. }
	kCGEventTargetProcessSerialNumber = 39;

  { Key to access a field that contains the event target Unix process ID. }
	kCGEventTargetUnixProcessID = 40;

  { Key to access a field that contains the event source Unix process ID. }
	kCGEventSourceUnixProcessID = 41;

  { Key to access a field that contains the event source user-supplied
     data, up to 64 bits. }
	kCGEventSourceUserData = 42;

  { Key to access a field that contains the event source Unix effective
     UID. }
	kCGEventSourceUserID = 43;

  { Key to access a field that contains the event source Unix effective
     GID. }
	kCGEventSourceGroupID = 44;

  { Key to access a field that contains the event source state ID used to
     create this event. }
	kCGEventSourceStateID = 45;
    
  { Key to access an integer field that indicates whether a scrolling event
     contains continuous, pixel-based scrolling data. The value is non-zero
     when the scrolling data is pixel-based and zero when the scrolling data
     is line-based. }
	kCGScrollWheelEventIsContinuous = 88;
  
  { Added in 10.5; made public in 10.7 }
	kCGMouseEventWindowUnderMousePointer = 91;
	kCGMouseEventWindowUnderMousePointerThatCanHandleThisEvent = 92;
type
	CGEventField = UInt32;

{ Constants used with the `kCGMouseEventSubtype' event field. }
const
	kCGEventMouseSubtypeDefault = 0;
	kCGEventMouseSubtypeTabletPoint = 1;
	kCGEventMouseSubtypeTabletProximity = 2;
type
	CGEventMouseSubtype = UInt32;

{ Constants that specify possible tapping points for events. }
const
	kCGHIDEventTap = 0;
	kCGSessionEventTap = 1;
	kCGAnnotatedSessionEventTap = 2;
type
	CGEventTapLocation = UInt32;

{ Constants that specify where a new event tap is inserted into the list of
   active event taps. }
const
	kCGHeadInsertEventTap = 0;
	kCGTailAppendEventTap = 1;
type
	CGEventTapPlacement = UInt32;

{ Constants that specify whether a new event tap is an active filter or a
   passive listener. }
const
	kCGEventTapOptionDefault = $00000000;
	kCGEventTapOptionListenOnly = $00000001;
type
	CGEventTapOptions = UInt32;

{ A mask that identifies the set of Quartz events to be observed in an
   event tap. }
type
	CGEventMask = UInt64;
{
Generate an event mask for a single type of event.
#define CGEventMaskBit(eventType)	((CGEventMask)1 << (eventType))
}

{ Generate an event mask for a single type of event.
#define kCGEventMaskForAllEvents	(~(CGEventMask)0)
}

const
	kCGEventMaskForAllEvents = UInt64($FFFFFFFFFFFFFFFF);

{ An opaque type that represents state within the client application that’s
   associated with an event tap. }
type
	CGEventTapProxy = ^__CGEventTapProxy; { an opaque type }
	__CGEventTapProxy = record end;

{ A client-supplied callback function that’s invoked whenever an associated
   event tap receives a Quartz event.

   The callback is passed a proxy for the tap, the event type, the incoming
   event, and the user-defined data specified when the event tap was
   created. The function should return the (possibly modified) passed-in
   event, a newly constructed event, or NULL if the event is to be deleted.

   The event passed to the callback is retained by the calling code, and is
   released after the callback returns and the data is passed back to the
   event system. If a different event is returned by the callback function,
   then that event will be released by the calling code along with the
   original event, after the event data has been passed back to the event
   system. }

type
	CGEventTapCallBack = function( proxy: CGEventTapProxy; typ: CGEventType; event: CGEventRef; userInfo: UnivPtr ): CGEventRef;

{ When an event tap is installed or released, a notification is posted. See
   notify(3) and notify.h for details. }

const
	kCGNotifyEventTapAdded = 'com.apple.coregraphics.eventTapAdded';
const
	kCGNotifyEventTapRemoved = 'com.apple.coregraphics.eventTapRemoved';

{ The structure used to report information about event taps. }

type
	CGEventTapInformationPtr = ^CGEventTapInformation;
	CGEventTapInformation = record
		eventTapID: UInt32;
		tapPoint: CGEventTapLocation;		{ HID, session, annotated session }
		options: CGEventTapOptions;		{ Listener, filter }
{$ifc TARGET_CPU_64}
    __alignment_dummy: UInt32;
{$endc}
		eventsOfInterest: CGEventMask;	{ Mask of events being tapped }
		tappingProcess: pid_t;		{ Process that is tapping events }
		processBeingTapped: pid_t;	{ Zero if not a per-process tap }
		enabled: CBool;		{ True if tap is enabled }
		minUsecLatency: Float32;		{ Minimum latency in microseconds }
		avgUsecLatency: Float32;		{ Average latency in microseconds }
		maxUsecLatency: Float32;		{ Maximum latency in microseconds }
	end;
	__CGEventTapInformation = CGEventTapInformation;

{ An opaque type that represents the source of a Quartz event. }
type
	CGEventSourceRef = ^__CGEventSource; { an opaque type }
	__CGEventSource = record end;

{ Constants that specify the possible source states of an event source. }
const
	kCGEventSourceStatePrivate = -1;
	kCGEventSourceStateCombinedSessionState = 0;
	kCGEventSourceStateHIDSystemState = 1;
type
	CGEventSourceStateID = UInt32;

{ A code that represents the type of keyboard used with a specified event
   source. }
type
	CGEventSourceKeyboardType = UInt32;

{ A constant specifying any input event type }
const
	kCGAnyInputEventType = $FFFFFFFF;

{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}

end.
{$endc} {not MACOSALLINCLUDE}
