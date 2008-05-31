{
 *  AXUIElement.h
 *
 *  Copyright (c) 2002 Apple Computer, Inc. All rights reserved.
 *
 }
{	  Pascal Translation:  Peter N Lewis, <peter@stairways.com.au>, 2004 }
{     Pascal Translation Updated:  Gale R Paeper, <gpaeper@empirenet.com>, 2006 }

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

unit AXUIElement;
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
uses MacTypes,CFBase,CFArray,AXErrors,CFRunLoop,CGRemoteOperation,MacOSXPosix;
{$ALIGN MAC68K}

function AXAPIEnabled: Boolean; external name '_AXAPIEnabled';
function AXIsProcessTrusted: Boolean; external name '_AXIsProcessTrusted';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
// must be called with root privs
function AXMakeProcessTrusted( executablePath: CFStringRef ): AXError; external name '_AXMakeProcessTrusted';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

type
	AXUIElementRef    = ^SInt32; { an opaque 32-bit type }

const
	kAXCopyMultipleAttributeOptionStopOnError = $1;
type
	AXCopyMultipleAttributeOptions = UInt32;


function AXUIElementGetTypeID: CFTypeID; external name '_AXUIElementGetTypeID';

function AXUIElementCopyAttributeNames( element: AXUIElementRef; var names: CFArrayRef ): AXError; external name '_AXUIElementCopyAttributeNames';
function AXUIElementCopyAttributeValue( element: AXUIElementRef; attribute: CFStringRef; var value: CFTypeRef ): AXError; external name '_AXUIElementCopyAttributeValue';
function AXUIElementGetAttributeValueCount( element: AXUIElementRef; attribute: CFStringRef; var count: CFIndex ): AXError; external name '_AXUIElementGetAttributeValueCount';
function AXUIElementCopyAttributeValues( element: AXUIElementRef; attribute: CFStringRef; index: CFIndex; maxValues: CFIndex; var values: CFArrayRef ): AXError; external name '_AXUIElementCopyAttributeValues';
function AXUIElementIsAttributeSettable( element: AXUIElementRef; attribute: CFStringRef; var settable: Boolean ): AXError; external name '_AXUIElementIsAttributeSettable';
function AXUIElementSetAttributeValue( element: AXUIElementRef; attribute: CFStringRef; value: CFTypeRef ): AXError; external name '_AXUIElementSetAttributeValue';
function AXUIElementCopyMultipleAttributeValues( element: AXUIElementRef; attributes: CFArrayRef; options: AXCopyMultipleAttributeOptions; var values: CFArrayRef ): AXError; external name '_AXUIElementCopyMultipleAttributeValues';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

function AXUIElementCopyParameterizedAttributeNames( element: AXUIElementRef; var names: CFArrayRef ): AXError; external name '_AXUIElementCopyParameterizedAttributeNames';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)
function AXUIElementCopyParameterizedAttributeValue( element: AXUIElementRef; parameterizedAttribute: CFStringRef; parameter: CFTypeRef; var result: CFTypeRef ): AXError; external name '_AXUIElementCopyParameterizedAttributeValue';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)

function AXUIElementCopyActionNames( element: AXUIElementRef; var names: CFArrayRef ): AXError; external name '_AXUIElementCopyActionNames';
function AXUIElementCopyActionDescription( element: AXUIElementRef; action: CFStringRef; var description: CFStringRef ): AXError; external name '_AXUIElementCopyActionDescription';
function AXUIElementPerformAction( element: AXUIElementRef; action: CFStringRef ): AXError; external name '_AXUIElementPerformAction';

function AXUIElementCopyElementAtPosition( application: AXUIElementRef; x: Float32; y: Float32; var element: AXUIElementRef ): AXError; external name '_AXUIElementCopyElementAtPosition';

function AXUIElementCreateApplication( pid: pid_t ): AXUIElementRef; external name '_AXUIElementCreateApplication';
function AXUIElementCreateSystemWide: AXUIElementRef; external name '_AXUIElementCreateSystemWide';

function AXUIElementGetPid( element: AXUIElementRef; var pid: pid_t ): AXError; external name '_AXUIElementGetPid';

// pass the SystemWide element (AXUIElementCreateSystemWide) if you want to set the timeout globally for this process
// setting the timeout on another AXUIElementRef sets it only for that ref, not for other AXUIElementRef(s) that are
// equal to it.
// setting timeout to 0 makes this element use the global timeout
function AXUIElementSetMessagingTimeout( element: AXUIElementRef; timeoutInSeconds: Float32 ): AXError; external name '_AXUIElementSetMessagingTimeout';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

// see CGRemoteOperation.h for documentation of parameters
// you can only pass the root or application uielement
function AXUIElementPostKeyboardEvent( application: AXUIElementRef; keyChar: CGCharCode; virtualKey: CGKeyCode; keyDown: Boolean ): AXError; external name '_AXUIElementPostKeyboardEvent';


// Notification APIs
type
	AXObserverRef    = ^SInt32; { an opaque 32-bit type }

type
    AXObserverCallback = procedure( observer: AXObserverRef; element: AXUIElementRef; notification: CFStringRef; refcon: UnivPtr );

function AXObserverGetTypeID: CFTypeID; external name '_AXObserverGetTypeID';

function AXObserverCreate( application: pid_t; callback: AXObserverCallback; var outObserver: AXObserverRef ): AXError; external name '_AXObserverCreate';

function AXObserverAddNotification( observer: AXObserverRef; element: AXUIElementRef; notification: CFStringRef; refcon: UnivPtr ): AXError; external name '_AXObserverAddNotification';
function AXObserverRemoveNotification( observer: AXObserverRef; element: AXUIElementRef; notification: CFStringRef ): AXError; external name '_AXObserverRemoveNotification';

function AXObserverGetRunLoopSource( observer: AXObserverRef ): CFRunLoopSourceRef; external name '_AXObserverGetRunLoopSource';

end.
