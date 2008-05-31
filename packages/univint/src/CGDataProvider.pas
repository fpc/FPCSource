{ CoreGraphics - CGDataProvider.h
 * Copyright (c) 1999-2004 Apple Computer, Inc.
 * All rights reserved.
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

unit CGDataProvider;
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
	CGDataProviderRef = ^SInt32; { an opaque 32-bit type }


{ This callback is called to copy `count' bytes from the sequential data
 * stream to `buffer'. }

type
	CGDataProviderGetBytesCallback = function( info: UnivPtr; buffer: UnivPtr; count: size_t ): size_t;

{ This callback is called to skip `count' bytes forward in the sequential
 * data stream. }

type
	CGDataProviderSkipBytesCallback = procedure( info: UnivPtr; count: size_t );

{ This callback is called to rewind to the beginning of sequential data
 * stream. }

type
	CGDataProviderRewindCallback = procedure( info: UnivPtr );

{ This callback is called to release the `info' pointer when the data
 * provider is freed. }

type
	CGDataProviderReleaseInfoCallback = procedure( info: UnivPtr );

{ Callbacks for sequentially accessing data.
 * `getBytes' is called to copy `count' bytes from the sequential data
 *   stream to `buffer'.  It should return the number of bytes copied, or 0
 *   if there's no more data.
 * `skipBytes' is called to skip ahead in the sequential data stream by
 *   `count' bytes.
 * `rewind' is called to rewind the sequential data stream to the beginning
 *   of the data.
 * `releaseProvider', if non-NULL, is called to release the `info' pointer
 *   when the provider is freed. }

type
	CGDataProviderCallbacks = record
		getBytes: CGDataProviderGetBytesCallback;
		skipBytes: CGDataProviderSkipBytesCallback;
		rewind: CGDataProviderRewindCallback;
		releaseProvider: CGDataProviderReleaseInfoCallback;
	end;

{ This callback is called to get a pointer to the entire block of data. }

type
	CGDataProviderGetBytePointerCallback = function( info: UnivPtr ): UnivPtr;

{ This callback is called to release the pointer to entire block of
 * data. }

type
	CGDataProviderReleaseBytePointerCallback = procedure( info: UnivPtr; pointr: {const} UnivPtr );

{ This callback is called to copy `count' bytes at byte offset `offset'
 * into `buffer'. }

type
	CGDataProviderGetBytesAtOffsetCallback = function( info: UnivPtr; buffer: UnivPtr; offset: size_t; count: size_t ): size_t;

{ Callbacks for directly accessing data.
 * `getBytePointer', if non-NULL, is called to return a pointer to the
 *   provider's entire block of data.
 * `releaseBytePointer', if non-NULL, is called to release a pointer to
 *   the provider's entire block of data.
 * `getBytes', if non-NULL, is called to copy `count' bytes at offset
 * `offset' from the provider's data to `buffer'.  It should return the
 *   number of bytes copied, or 0 if there's no more data.
 * `releaseProvider', if non-NULL, is called when the provider is freed.
 *
 * At least one of `getBytePointer' or `getBytes' must be non-NULL.  }

type
	CGDataProviderDirectAccessCallbacks = record
		getBytePointer: CGDataProviderGetBytePointerCallback;
		releaseBytePointer: CGDataProviderReleaseBytePointerCallback;
		getBytes: CGDataProviderGetBytesAtOffsetCallback;
		releaseProvider: CGDataProviderReleaseInfoCallback;
	end;

{ Return the CFTypeID for CGDataProviderRefs. }

function CGDataProviderGetTypeID: CFTypeID; external name '_CGDataProviderGetTypeID'; (* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)

{ Create a sequential-access data provider using `callbacks' to provide
 * the data.  `info' is passed to each of the callback functions. }

function CGDataProviderCreate( info: UnivPtr; const (*var*) callbacks: CGDataProviderCallbacks ): CGDataProviderRef; external name '_CGDataProviderCreate';

{ Create a direct-access data provider using `callbacks' to supply `size'
 * bytes of data. `info' is passed to each of the callback functions. }

function CGDataProviderCreateDirectAccess( info: UnivPtr; size: size_t; const (*var*) callbacks: CGDataProviderDirectAccessCallbacks ): CGDataProviderRef; external name '_CGDataProviderCreateDirectAccess';

{ The callback used by `CGDataProviderCreateWithData'. }

type
	CGDataProviderReleaseDataCallback = procedure( info: UnivPtr; data: {const} UnivPtr; size: size_t );

{ Create a direct-access data provider using `data', an array of `size'
 * bytes.  `releaseData' is called when the data provider is freed, and is
 * passed `info' as its first argument. }

function CGDataProviderCreateWithData( info: UnivPtr; data: {const} UnivPtr; size: size_t; releaseData: CGDataProviderReleaseDataCallback ): CGDataProviderRef; external name '_CGDataProviderCreateWithData';

{ Create a direct-access data provider which reads from `data'. }

function CGDataProviderCreateWithCFData( data: CFDataRef ): CGDataProviderRef; external name '_CGDataProviderCreateWithCFData'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ Create a data provider using `url'. }

function CGDataProviderCreateWithURL( url: CFURLRef ): CGDataProviderRef; external name '_CGDataProviderCreateWithURL';

{ Equivalent to `CFRetain(provider)'. }

function CGDataProviderRetain( provider: CGDataProviderRef ): CGDataProviderRef; external name '_CGDataProviderRetain';

{ Equivalent to `CFRelease(provider)'. }

procedure CGDataProviderRelease( provider: CGDataProviderRef ); external name '_CGDataProviderRelease';

{* DEPRECATED FUNCTIONS *}

{ Don't use this function; use CGDataProviderCreateWithURL instead. }

function CGDataProviderCreateWithFilename( filename: ConstCStringPtr ): CGDataProviderRef; external name '_CGDataProviderCreateWithFilename';


end.
