{
*  CGEventSource.h
*  CoreGraphics
*
*  Copyright (c) 2004 Apple Computer, Inc. All rights reserved.
*
}
{       Pascal Translation:  Peter N Lewis, <peter@stairways.com.au>, August 2005 }
{
    Modified for use with Free Pascal
    Version 200
    Please report any bugs to <gpc@microbizz.nl>
}

{$mode macpas}
{$packenum 1}
{$macro on}
{$inline on}
{$CALLING MWPASCAL}

unit CGEventSource;
interface
{$setc UNIVERSAL_INTERFACES_VERSION := $0342}
{$setc GAP_INTERFACES_VERSION := $0200}

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
uses MacTypes,CFBase,CFDate,CGRemoteOperation,CGEventTypes;
{$ALIGN POWER}


{ Return the CFTypeID for CGEventSourceRefs. }
function CGEventSourceGetTypeID: CFTypeID; external name '_CGEventSourceGetTypeID'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{
 * Create a new CGEventSource
 *
 * The event source contains accumulated state related to event
 * generation and event posting, allowing for customized event
 * generation and processing.
 *
 * The CGEventSourceStateID refers to a global event state table.
 * These tables contain accumulated information on modifier flag state,
 * keyboard key state, mouse button state, and related internal parameters
 * placed in effect by posting events with associated sources.
 *
 * Two pre-existing tables are defined.
 *
 * The kCGEventSourceStateCombinedSessionState table reflects the combined state
 * of all event sources posting to this user login session. Mouse button,
 * keyboard state, and modifier flag state (derived from keyboard state)
 * are logically ORed together in this state table.
 *
 * The kCGEventSourceStateHIDSystemState table reflects the combined state
 * of all hardware event sources posting from the HID system. Mouse button,
 * keyboard state, and modifier flag state (derived from keyboard state)
 * for the hardware devices are logically ORed together in this state table.
 *
 * A program, or application posting from within a login session should use
 * the kCGEventSourceStateCombinedSessionState.
 *
 * A user space device driver interpreting hardware state and generating events
 * should use the kCGEventSourceStateHIDSystemState.
 *
 * Very specialized applications such as remote control programs may want to
 * generate and track event source state independent of other processes.
 * These programs should use the kCGEventSourceStatePrivate value in creating
 * their event source. An independent state table and unique CGEventSourceStateID
 * are created to track the event source's state.  The independent sate table is owned
 * by the creating event source and released with it.
 *
 * If the CGEventSourceStateID from another CGEventSourceRef
 * is released while being used in a second CGEventSourceRef, the second source
 * will behave as if all keys and buttons on input devices are up in generating
 * new events from this source.
 *
 * Default behavior without an event source, that is, passing NULL to
 * CGEvent creation functions, is identical to using an unmodified
 * CGEventSource created with the kCGEventSourceStateCombinedSystemState
 * source state ID, if running within a login session, or using
 * kCGEventSourceStateHIDSystemState if running outside of a login session,
 * as in a daemon or user space device driver.
 *
 * Returns NULL if the specified event source is not a valid CGEventSourceStateID,
 * or is a private event source owned by another process,
 * or is not a member of the following enumeration.
 *
 * The returned object should be released with CFRelease when no longer needed.
 }
function CGEventSourceCreate( sourceState: CGEventSourceStateID ): CGEventSourceRef; external name '_CGEventSourceCreate'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{
 * Set and get the keyboard type to be used with this source
 * The value will be used with UCKeyTranslate() to drive keyboard translation
 }
function CGEventSourceGetKeyboardType( source: CGEventSourceRef ): CGEventSourceKeyboardType; external name '_CGEventSourceGetKeyboardType'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
procedure CGEventSourceSetKeyboardType( source: CGEventSourceRef; keyboardType: CGEventSourceKeyboardType ); external name '_CGEventSourceSetKeyboardType'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{
 * Return the event source state ID associated with the event source.
 * For event sources created with the kCGEventSourceStatePrivate
 * CGEventSourceStateID, this returns the actual CGEventSourceStateID
 * created for the CGEventSourceRef.
 *
 * The value returned may be passed to CGEventSourceCreate() to create a
 * second event source sharing the same state table.  This may be useful,
 * for example, in creating seperate mouse and keyboard sources which share
 * a common private state.
 *
 * If the CGEventSourceStateID from another CGEventSourceRef
 * is released while being used in a second CGEventSourceRef, the second source
 * will behave as if all keys and buttons on input devices are up in generating
 * new events from this source.
 }
function CGEventSourceGetSourceStateID( source: CGEventSourceRef ): CGEventSourceStateID; external name '_CGEventSourceGetSourceStateID'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{
 * The state of an event source may be queried for specialized event processing
 * purposes.
 }
function CGEventSourceButtonState( sourceState: CGEventSourceStateID; button: CGMouseButton ): CBool; external name '_CGEventSourceButtonState'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
function CGEventSourceKeyState( sourceState: CGEventSourceStateID; key: CGKeyCode ): CBool; external name '_CGEventSourceKeyState'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
function CGEventSourceFlagsState( sourceState: CGEventSourceStateID ): CGEventFlags; external name '_CGEventSourceFlagsState'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{
 * Time since last event for an event source.
 *
 * The kCGAnyInputEventType eventType will report the last timestamp for any
 * input event, keyboard, mouse, or tablet.  The various system and app
 * defined events do not contribute to this event type's time.
 *
 * Again, a program or application posting from within a login session should use
 * the kCGEventSourceStateCombinedSessionState.
 *
 * A user space device driver interpreting hardware state and generating events
 * should use the kCGEventSourceStateHIDSystemState.
 }

function CGEventSourceSecondsSinceLastEventType( source: CGEventSourceStateID; eventType: CGEventType ): CFTimeInterval; external name '_CGEventSourceSecondsSinceLastEventType'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{
 * Returns a count of events of different types seen since the window server started.
 *
 * Note that modifier keys produce kCGEventFlagsChanged events, not kCGEventKeyDown
 * events, and do so both on press and release.
 *
 * Please note that some keys on the keyboard are not implemented as keys,
 * but instead are USB button devices.  We account for the ones we can see as
 * kCGEventKeyDown events. Where we don't get a different event for key-up, we
 * record both a key down and a key up.
 *
 * There is no guarantee that the number of key down and key up events will match
 * when all keyboard keys are up, due to the inconsistent nature of the USB button device keys.
 *
 * Key autorepeat events are not counted.
 *
 * Synthetic events posted into the system may also produce assymetric 'down' and 'up' event counts.
 *
 * Again, a program or application posting from within a login session should use
 * the kCGEventSourceStateCombinedSessionState.
 *
 * A user space device driver interpreting hardware state and generating events
 * should use the kCGEventSourceStateHIDSystemState.
 *
 }
function CGEventSourceCounterForEventType( source: CGEventSourceStateID; evType: CGEventType ): UInt32; external name '_CGEventSourceCounterForEventType'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{
 * Each event carries a payload of 64 bits of user specified data.
 * The values may be individually set per event using the
 * CGEventSetIntegerValueField() API, or may be set for all events
 * created by this event source using this API.
 * This mechanism is more convenient for uses such as vendor hardware IDs.
 }
procedure CGEventSourceSetUserData( source: CGEventSourceRef; userData: SInt64 ); external name '_CGEventSourceSetUserData'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
function CGEventSourceGetUserData( source: CGEventSourceRef ): SInt64; external name '_CGEventSourceGetUserData'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{
 * The CGRemoteOperation APIs (CoreGraphics/CGRemoteOperation.h) use an implicit event
 * source to affect system behavior on event posting.  Here we expose the same mechanism
 * on a per event source basis.  Default values are different than the remote operation API
 * exposes, based on developer feedback.
 }

{
 * The system may optionally suppress local hardware events
 * from the keyboard and mouse during a short interval after
 * a program posts an event (see CGSetLocalEventsSuppressionInterval())
 * or while your program has a left mouse button down (mouse drag) in effect.
 *
 * Some classes of applications may want to disable events from some of the local hardware.
 * For example, an app may want to post only mouse events, and so may wish to permit local
 * keyboard hardware events to pass through while blocking local mouse events.
 * Set the event source to permit keyboard events
 * prior to creating the mouse event after which you want to get keyboard events.
 *
 * This interface lets an app specify a state (event suppression interval, or mouse drag), and
 * a mask of event categories to be passed through. The new filter state takes effect
 * with the next event your app posts that is created with this event source.
 *
 * The kCGEventSuppressionStateSuppressionInterval state allows one to set a filter that
 * permits local hardware mouse events, local keyboard events, both, or neither during the
 * specified short interval of time after your process posts an event created with this source.
 *
 * The kCGEventSuppressionStateRemoteMouseDrag state allows one to set a filter that
 * permits local hardware mouse events, local keyboard events, both, or neither during
 * the time that your event source has a left mouse button down (mouse drag) in effect.
 *
 * The default state for a CGEventSourceRef is to have all filtering off, so that local
 * hardware events are unaffected.
 *
 * When a user enters the 'Force Quit' keyboard attention sequence, Command-Option-Escape,
 * all local event supression filters in effect are disabled, and all local hardware
 * events are delivered as normal.  This allows for recovery from unfortunate programming
 * errors.
 }

procedure CGEventSourceSetLocalEventsFilterDuringSuppressionState( source: CGEventSourceRef; filter: CGEventFilterMask; state: CGEventSuppressionState ); external name '_CGEventSourceSetLocalEventsFilterDuringSuppressionState'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

function CGEventSourceGetLocalEventsFilterDuringSuppressionState( source: CGEventSourceRef; state: CGEventSuppressionState ): CGEventFilterMask; external name '_CGEventSourceGetLocalEventsFilterDuringSuppressionState'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{
 * Set the period of time in seconds that specified local hardware events (keyboard or mouse)
 * may suppressed after posting a CGEventRef created with this source, if the event
 * source is set to apply the kCGEventSuppressionStateSuppressionInterval.
 *
 * Defaults to 0.25 second.
 }
procedure CGEventSourceSetLocalEventsSuppressionInterval( source: CGEventSourceRef; seconds: CFTimeInterval ); external name '_CGEventSourceSetLocalEventsSuppressionInterval'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
function CGEventSourceGetLocalEventsSuppressionInterval( source: CGEventSourceRef ): CFTimeInterval; external name '_CGEventSourceGetLocalEventsSuppressionInterval'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


end.
