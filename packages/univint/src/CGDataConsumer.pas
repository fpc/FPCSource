{ CoreGraphics - CGDataConsumer.h
 * Copyright (c) 1999-2004 Apple Computer, Inc.
 * All rights reserved.
 }
{       Pascal Translation Updated:  Peter N Lewis, <peter@stairways.com.au>, August 2005 }
{       Pascal Translation Updated:  Gale R Paeper, <gpaeper@empirenet.com>, 2006 }
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

unit CGDataConsumer;
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
uses MacTypes,CFBase,CFData,CGBase,CFURL;
{$ALIGN POWER}


type
	CGDataConsumerRef = ^SInt32; { an opaque 32-bit type }


{ This callback is called to copy `count' bytes from `buffer' to the
 * data consumer. }

type
	CGDataConsumerPutBytesCallback = function( info: UnivPtr; buffer: {const} UnivPtr; count: size_t ): size_t;

{ This callback is called to release the `info' pointer when the data
 * provider is freed. }

type
	CGDataConsumerReleaseInfoCallback = procedure( info: UnivPtr );

{ Callbacks for accessing data.
 * `putBytes' copies `count' bytes from `buffer' to the consumer, and
 * returns the number of bytes copied.  It should return 0 if no more data
 * can be written to the consumer.
 * `releaseConsumer', if non-NULL, is called when the consumer is freed. }

type
	CGDataConsumerCallbacks = record
		putBytes: CGDataConsumerPutBytesCallback;
		releaseConsumer: CGDataConsumerReleaseInfoCallback;
	end;

{ Return the CFTypeID for CGDataConsumerRefs. }

function CGDataConsumerGetTypeID: CFTypeID; external name '_CGDataConsumerGetTypeID'; (* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)

{ Create a data consumer using `callbacks' to handle the data.  `info' is
 * passed to each of the callback functions. }

function CGDataConsumerCreate( info: UnivPtr; const (*var*) callbacks: CGDataConsumerCallbacks ): CGDataConsumerRef; external name '_CGDataConsumerCreate';

{ Create a data consumer which writes data to `url'. }

function CGDataConsumerCreateWithURL( url: CFURLRef ): CGDataConsumerRef; external name '_CGDataConsumerCreateWithURL';

{ Create a data consumer which writes to `data'. }

function CGDataConsumerCreateWithCFData( data: CFMutableDataRef ): CGDataConsumerRef; external name '_CGDataConsumerCreateWithCFData'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ Equivalent to `CFRetain(consumer)'. }

function CGDataConsumerRetain( consumer: CGDataConsumerRef ): CGDataConsumerRef; external name '_CGDataConsumerRetain';

{ Equivalent to `CFRelease(consumer)'. }

procedure CGDataConsumerRelease( consumer: CGDataConsumerRef ); external name '_CGDataConsumerRelease';


end.
