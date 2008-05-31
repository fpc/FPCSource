{	CFMessagePort.h
	Copyright (c) 1998-2005, Apple, Inc. All rights reserved.
}
{	  Pascal Translation:  Peter N Lewis, <peter@stairways.com.au>, 2004 }
{	  Pascal Translation Updated:  Peter N Lewis, <peter@stairways.com.au>, September 2005 }
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

unit CFMessagePort;
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
uses MacTypes,CFBase,CFString,CFRunLoop,CFData,CFDate;
{$ALIGN POWER}


type
	CFMessagePortRef = ^SInt32; { an opaque 32-bit type }

const
	kCFMessagePortSuccess = 0;
	kCFMessagePortSendTimeout = -1;
	kCFMessagePortReceiveTimeout = -2;
	kCFMessagePortIsInvalid = -3;
	kCFMessagePortTransportError = -4;

type
	CFMessagePortContext = record
		version: CFIndex;
		info: UnivPtr;
		retain: function( info: {const} UnivPtr ): UnivPtr;
		release: procedure( info: {const} UnivPtr );
		copyDescription: function( info: {const} UnivPtr ): CFStringRef;
	end;
	CFMessagePortContextPtr = ^CFMessagePortContext;

type
	CFMessagePortCallBack = function( local: CFMessagePortRef; msgid: SInt32; data: CFDataRef; info: UnivPtr ): CFDataRef;
{ If callout wants to keep a hold of the data past the return of the callout, it must COPY the data. This includes the case where the data is given to some routine which _might_ keep a hold of it; System will release returned CFData. }
type
	CFMessagePortInvalidationCallBack = procedure( ms: CFMessagePortRef; info: UnivPtr );

function CFMessagePortGetTypeID: CFTypeID; external name '_CFMessagePortGetTypeID';

function CFMessagePortCreateLocal( allocator: CFAllocatorRef; name: CFStringRef; callout: CFMessagePortCallBack; var context: CFMessagePortContext; var shouldFreeInfo: Boolean ): CFMessagePortRef; external name '_CFMessagePortCreateLocal';
function CFMessagePortCreateRemote( allocator: CFAllocatorRef; name: CFStringRef ): CFMessagePortRef; external name '_CFMessagePortCreateRemote';

function CFMessagePortIsRemote( ms: CFMessagePortRef ): Boolean; external name '_CFMessagePortIsRemote';
function CFMessagePortGetName( ms: CFMessagePortRef ): CFStringRef; external name '_CFMessagePortGetName';
function CFMessagePortSetName( ms: CFMessagePortRef; newName: CFStringRef ): Boolean; external name '_CFMessagePortSetName';
procedure CFMessagePortGetContext( ms: CFMessagePortRef; var context: CFMessagePortContext ); external name '_CFMessagePortGetContext';
procedure CFMessagePortInvalidate( ms: CFMessagePortRef ); external name '_CFMessagePortInvalidate';
function CFMessagePortIsValid( ms: CFMessagePortRef ): Boolean; external name '_CFMessagePortIsValid';
function CFMessagePortGetInvalidationCallBack( ms: CFMessagePortRef ): CFMessagePortInvalidationCallBack; external name '_CFMessagePortGetInvalidationCallBack';
procedure CFMessagePortSetInvalidationCallBack( ms: CFMessagePortRef; callout: CFMessagePortInvalidationCallBack ); external name '_CFMessagePortSetInvalidationCallBack';

{ NULL replyMode argument means no return value expected, dont wait for it }
function CFMessagePortSendRequest( remote: CFMessagePortRef; msgid: SInt32; data: CFDataRef; sendTimeout: CFTimeInterval; rcvTimeout: CFTimeInterval; replyMode: CFStringRef; returnData: CFDataRefPtr ): SInt32; external name '_CFMessagePortSendRequest';

function CFMessagePortCreateRunLoopSource( allocator: CFAllocatorRef; local: CFMessagePortRef; order: CFIndex ): CFRunLoopSourceRef; external name '_CFMessagePortCreateRunLoopSource';


end.
