{
 *  CGEvent.h
 *  CoreGraphics
 *
 *  Copyright (c) 2004 Apple Computer, Inc. All rights reserved.
 *
 }
{       Pascal Translation:  Peter N Lewis, <peter@stairways.com.au>, August 2005 }
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

unit CGEvent;
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
uses MacTypes,CFBase,CFData,CFMachPort,CGBase,CGDirectDisplay,CGEventTypes,CGGeometry,CGErrors,CGRemoteOperation,CGEventSource;
{$ALIGN POWER}


{ Return the CFTypeID for CGEventRefs. }
function CGEventGetTypeID: CFTypeID; external name '_CGEventGetTypeID'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ 
 * APIs provide the ability to create a NULL event, or to create specialized
 * events reflecting a state specified as parameters to the creation functions.
 }

{ Create a NULL event to be filled in.  'source' may be NULL. }
function CGEventCreate( source: CGEventSourceRef ): CGEventRef; external name '_CGEventCreate'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{
 * Functions to flatten and reconstruct a CGEventRef for network transport.
 * These may be useful in remote control and helpdesk applications.
 *
 * Returns a CGEventRef built from the flattened data representation, or NULL
 * if the eventData is invalid.
 }
function CGEventCreateFromData( allocator: CFAllocatorRef; eventData: CFDataRef ): CGEventRef; external name '_CGEventCreateFromData';

{
 * Returns a CFDataRef containing  the flattened data representation of the event,
 * or NULL if the eventData is invalid.
 }
function CGEventCreateData( allocator: CFAllocatorRef; event: CGEventRef ): CFDataRef; external name '_CGEventCreateData';

{
 * Create mouse events.
 *
 * The event source may be taken from another event, or may be NULL.
 * mouseType should be one of the mouse event types.
 * mouseCursorPosition should be the global coordinates the mouse is at for the event.
 * For kCGEventOtherMouseDown, kCGEventOtherMouseDragged, and
 * kCGEventOtherMouseUp  events, the mouseButton parameter should
 * indicate which button is changing state.
 * 
 * The current implemementation of the event system supports a maximum of thirty-two buttons.
 * Mouse button 0 is the primary button on the mouse. Mouse button 1 is the secondary
 * mouse button (right). Mouse button 2 is the center button, and the remaining
 * buttons would be in USB device order.
 }
function CGEventCreateMouseEvent( source: CGEventSourceRef; mouseType: CGEventType; mouseCursorPosition: CGPoint; mouseButton: CGMouseButton ): CGEventRef; external name '_CGEventCreateMouseEvent'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{
 * Create keyboard events.
 *
 * The event source may be taken from another event, or may be NULL.
 * Based on the virtual key code values entered,
 * the appropriate key down, key up, or flags changed events are generated.
 *
 * All keystrokes needed to generate a character must be entered, including
 * SHIFT, CONTROL, OPTION, and COMMAND keys.  For example, to produce a 'Z',
 * the SHIFT key must be down, the 'z' key must go down, and then the SHIFT
 * and 'z' key must be released:
 *	CGEventCreateKeyboardEvent((CGKeyCode)56, true ); // shift down
 *	CGEventCreateKeyboardEvent( (CGKeyCode)6, true ); // 'z' down
 *	CGEventCreateKeyboardEvent( (CGKeyCode)6, false ); // 'z' up
 *	CGEventCreateKeyboardEvent( (CGKeyCode)56, false ); // 'shift up
 }
function CGEventCreateKeyboardEvent( source: CGEventSourceRef; virtualKey: CGKeyCode; keyDown: CBool ): CGEventRef; external name '_CGEventCreateKeyboardEvent'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

function CGEventCreateCopy( event: CGEventRef ): CGEventRef; external name '_CGEventCreateCopy'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
{ 
 * CFRetain() and CFRelease() may be used to retain and release CGEventRefs.
 }

function CGEventGetSource( event: CGEventRef ): CGEventSourceRef; external name '_CGEventGetSource'; (* DEPRECATED_IN_MAC_OS_X_VERSION_10_4_AND_LATER *)
{
 * These functions provide access to the event source for an event.
 * Event filters may use these to generate events that are compatible
 * with an event being filtered.
 *
 * Note that CGEventCreateSourceFromEvent may return NULL if the event
 * was generated with a private CGEventSourceStateID owned by another
 * process.  Such events should be filtered based on the public state.
 }
function CGEventCreateSourceFromEvent( event: CGEventRef ): CGEventSourceRef; external name '_CGEventCreateSourceFromEvent'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
procedure CGEventSetSource( event: CGEventRef; source: CGEventSourceRef ); external name '_CGEventSetSource'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{
 * The following functions will provide high level access to selected event data.
 }
function CGEventGetType( event: CGEventRef ): CGEventType; external name '_CGEventGetType'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
procedure CGEventSetType( event: CGEventRef; typ: CGEventType ); external name '_CGEventSetType'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

function CGEventGetTimestamp( event: CGEventRef ): CGEventTimestamp; external name '_CGEventGetTimestamp'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
procedure CGEventSetTimestamp( event: CGEventRef; timestamp: CGEventTimestamp ); external name '_CGEventSetTimestamp'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

function CGEventGetLocation( event: CGEventRef ): CGPoint; external name '_CGEventGetLocation'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
procedure CGEventSetLocation( event: CGEventRef; location: CGPoint ); external name '_CGEventSetLocation'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

function CGEventGetFlags( event: CGEventRef ): CGEventFlags; external name '_CGEventGetFlags'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
procedure CGEventSetFlags( event: CGEventRef; flags: CGEventFlags ); external name '_CGEventSetFlags'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{
 * Access to UniChar data in keyboard events.
 *
 * There are no runtime errors associated with these functions.
 * Use on non-keyboard events leaves the event unchanged.
 * Note that many frameworks use a fixed length representation
 * of a CGEvent, and so are limited to a maximum string length
 * of 20 Unicode characters.  The CGEventRef itself is limited to 65535
 * characters.
 *
 * Calling CGEventKeyboardGetUnicodeString() with a NULL unicodeString
 * or zero maxStringLength will still return the actual count of
 * UniCode characters in the event data.
 }
procedure CGEventKeyboardGetUnicodeString( event: CGEventRef; maxStringLength: UniCharCount; var actualStringLength: UniCharCount; unicodeString: {variable-size-array} UniCharPtr ); external name '_CGEventKeyboardGetUnicodeString'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

procedure CGEventKeyboardSetUnicodeString( event: CGEventRef; stringLength: UniCharCount; {const} unicodeString: {variable-size-array} UniCharPtr ); external name '_CGEventKeyboardSetUnicodeString'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{
 * Additional low level functions provide access to specialized fields of the events
 }

{
 * The CGEventGetDoubleValueField and CGEventSetDoubleValueField
 * Functions renormalize certain fixed point and integer values
 * to an appropriate floating point range.  Keys this is done for
 * are:
 *
 * kCGMouseEventPressure	pressure value in the range 0-255
 * 				scaled to [0.0-1.0]
 * kCGTabletEventPointPressure	scaled pressure value; MAX=(2^16)-1, MIN=0
 * kCGTabletEventTiltX	tilt range is -((2^15)-1) to (2^15)-1 (-32767 to 32767)
 * kCGTabletEventTiltY	scaled to [0.0-1.0]
 * kCGTabletEventTangentialPressure	Same range as tilt
 * kCGTabletEventRotation	Fixed-point representation of device
 *				rotation in a 10.6 format
 }

function CGEventGetIntegerValueField( event: CGEventRef; field: CGEventField ): SInt64; external name '_CGEventGetIntegerValueField'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
function CGEventGetDoubleValueField( event: CGEventRef; field: CGEventField ): Float64; external name '_CGEventGetDoubleValueField'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{
 * Before using the Set functions, the event type must be set
 * properly by using the appropriate type creation function or by calling
 * CGEventSetType().
 *
 * If this is to be a mouse event generated by a tablet, call
 * CGEventSetIntegerValueField(event, kCGMouseEventSubtype) with a value of
 * kCGEventMouseSubtypeTabletPoint or kCGEventMouseSubtypeTabletProximity
 * before setting other parameters.
 *
 * The correct type and subtype must be set so that the internal
 * event-specific data structures may be properly filled in.
 }
procedure CGEventSetIntegerValueField( event: CGEventRef; field: CGEventField; value: SInt64 ); external name '_CGEventSetIntegerValueField'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
procedure CGEventSetDoubleValueField( event: CGEventRef; field: CGEventField; value: Float64 ); external name '_CGEventSetDoubleValueField'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ 
 * Registering an Event Tap
 * 
 * A function registers an event tap, taking a pointer to the
 * program's tap function and an arbitrary reference to be passed
 * to the tap function, and returning a CFMachPortRef the program
 * can add to the appropriate run loop by creating a surce and
 * using  CFRunLoopAddSource().
 * 
 * Taps may be placed at the point where HIDSystem events enter
 * the server, at the point where HIDSystem and remote control
 * events enter a session, at the point where events have been
 * annotated to flow to a specific application, or at the point
 * where events are delivered to the application.  Taps may be
 * inserted at a specified point at the head of pre-existing filters,
 * or appended after any pre-existing filters.
 * 
 * Taps may be passive event listeners, or active filters.
 * An active filter may pass an event through unmodified, modify
 * an event, or discard an event.  When a tap is registered, it
 * identifies the set of events to be observed with a mask, and
 * indicates if it is a passive or active event filter.  Multiple
 * event type bitmasks may be ORed together.
 *
 * Taps may only be placed at kCGHIDEventTap by a process running
 * as the root user.  NULL is returned for other users.
 *
 * Taps placed at kCGHIDEventTap, kCGSessionEventTap,
 * kCGAnnotatedSessionEventTap, or on a specific process may
 * only receive key up and down events if access for assistive
 * devices is enabled (Preferences Universal Access panel,
 * Keyboard view). If the tap is not permitted to monitor these
 * when the tap is being created, then the appropriate bits
 * in the mask are cleared.  If that results in an empty mask,
 * then NULL is returned.
 *
 * Releasing the CFMachPortRef will release the tap.
 * 
 * The CGEventTapProxy is an opaque reference to state within
 * the client application associated with the tap.  The tap
 * function may pass this reference to other functions, such as
 * the event-posting routines.
 *
 * The event tap callback runs from the CFRunLoop to which the
 * tap CFMachPort is added as a source. The thread safety is defined
 * by the CFRunLoop and it's environment.
 }

{ Return the a CFMachPortRef  for the event tap. }
function CGEventTapCreate( tap: CGEventTapLocation; place: CGEventTapPlacement; options: CGEventTapOptions; eventsOfInterest: CGEventMask; callback: CGEventTapCallBack; refcon: UnivPtr ): CFMachPortRef; external name '_CGEventTapCreate'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ 
 * Events being routed to individual applications may be tapped
 * using another function.  CGEventTapCreateForPSN() will report
 * all events being routed to the specified application.
 }

{ Return the a CFMachPortRef  for an event tap for the specified process. }
function CGEventTapCreateForPSN( processSerialNumber: { Temp type, til def moves to CoreServices } UnivPtr; place: CGEventTapPlacement; options: CGEventTapOptions; eventsOfInterest: CGEventMask; callback: CGEventTapCallBack; refcon: UnivPtr ): CFMachPortRef; external name '_CGEventTapCreateForPSN'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{
 * Enable or disable an event tap.
 *
 * The taps are normally enabled when created.
 * If a tap becomes unresponsive, or a user requests taps be disabled,
 * then an appropriate kCGEventTapDisabled... event is passed to the
 * registered CGEventTapCallBack function.
 *
 * Event taps may be re-enabled by calling this function.
 }
procedure CGEventTapEnable( myTap: CFMachPortRef; enable: CBool ); external name '_CGEventTapEnable'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
function CGEventTapIsEnabled( myTap: CFMachPortRef ): CBool; external name '_CGEventTapIsEnabled'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{
 * A function to post events from a tap is provided.  The events
 * are posted to the same points that an event returned from an
 * event tap would be posted to.  If an event tap posts new events,
 * the new events enter the system before the event returned by
 * the tap enters the system.  This order may be changed by
 * explicitly posting the filtered event along with new events
 * in the desired order, and then setting the returned event
 * *pEventOut to NULL.
 *
 * Events posted into the system will be seen by all taps placed
 * after the tap posting the event.
 }

{ Post an event from the event tap into the event stream. }
procedure CGEventTapPostEvent( proxy: CGEventTapProxy; event: CGEventRef ); external name '_CGEventTapPostEvent'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ 
 * Functions to post events into the system at various points
 * are also provided.  Each event posted by these functions
 * enters the system at a point immediately before any taps
 * instantiated for that point, and will pass through any such taps.
 *
 * This mechanism permits an external process to establish an
 * event routing policy, for example, by tapping events at the
 * kCGAnnotatedSessionEventTap and then posting the events to
 * the desired PSN.
 }

{ Post an event from the event tap into the event stream. }
procedure CGEventPost( tap: CGEventTapLocation; event: CGEventRef ); external name '_CGEventPost'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ Post an event from the event tap into the event stream for a specific application. }
procedure CGEventPostToPSN( processSerialNumber: { Temp type, til def moves to CoreServices } UnivPtr; event: CGEventRef ); external name '_CGEventPostToPSN'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{
 * Mechanism used to list event taps.
 * An array length (maxNumberOfTaps) and array of CGEventTapInformation structures
 * are passed in.
 * Up to maxNumberOfTaps elements of the array are filled in with event tap information.
 * The actual number of elements filled in is returned in eventTapCount.
 *
 * The call resets the accumulated minUsecLatency and maxUsecLatency fields.  Values
 * reported in these fields reflect the min and max values seen since the preceding call,
 * or the instantiation of the tap.
 *
 * If the CGEventTapInformation array is NULL, maxNumberOfTaps is ignored, and *eventTapCount
 * is filled in with the number of event taps that are currently installed.
 }
function CGGetEventTapList( maxNumberOfTaps: CGTableCount; tapList: {variable-size-array} CGEventTapInformationPtr; var eventTapCount: CGTableCount ): CGError; external name '_CGGetEventTapList'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


end.
