{ CoreGraphics - CGDataProvider.h
   Copyright (c) 1999-2011 Apple Inc.
   All rights reserved. }
{       Pascal Translation Updated:  Peter N Lewis, <peter@stairways.com.au>, August 2005 }
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

unit CGDataProvider;
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
	{$setc TARGET_OS_MAC := TRUE}
	{$setc TARGET_OS_IPHONE := FALSE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
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
uses MacTypes,MacOSXPosix,CFBase,CFData,CGBase,CFURL;
{$endc} {not MACOSALLINCLUDE}

{$ALIGN POWER}


type
	CGDataProviderRef = ^OpaqueCGDataProviderRef; { an opaque type }
	OpaqueCGDataProviderRef = record end;


{ This callback is called to copy `count' bytes from the sequential data
   stream to `buffer'. }

type
	CGDataProviderGetBytesCallback = function( info: UnivPtr; buffer: UnivPtr; count: size_t ): size_t;

{ This callback is called to skip `count' bytes forward in the sequential
   data stream. It should return the number of bytes that were actually
   skipped. }

type
	CGDataProviderSkipForwardCallback = function( info: UnivPtr; count: off_t ): off_t;

{ This callback is called to rewind to the beginning of sequential data
   stream. }

type
	CGDataProviderRewindCallback = procedure( info: UnivPtr );

{ This callback is called to release the `info' pointer when the data
   provider is freed. }

type
	CGDataProviderReleaseInfoCallback = procedure( info: UnivPtr );

{ Callbacks for sequentially accessing data.
   `version' is the version of this structure. It should be set to 0.
   `getBytes' is called to copy `count' bytes from the sequential data
     stream to `buffer'. It should return the number of bytes copied, or 0
     if there's no more data.
   `skipForward' is called to skip ahead in the sequential data stream by
     `count' bytes.
   `rewind' is called to rewind the sequential data stream to the beginning
     of the data.
   `releaseInfo', if non-NULL, is called to release the `info' pointer when
     the provider is freed. }

type
	CGDataProviderSequentialCallbacks = record
		version: UInt32;
{$ifc TARGET_CPU_64}
		__alignment_dummy: UInt32;
{$endc}
		getBytes: CGDataProviderGetBytesCallback;
		skipForward: CGDataProviderSkipForwardCallback;
		rewind: CGDataProviderRewindCallback;
		releaseInfo: CGDataProviderReleaseInfoCallback;
	end;

{ This callback is called to get a pointer to the entire block of data. }

type
	CGDataProviderGetBytePointerCallback = function( info: UnivPtr ): UnivPtr;

{ This callback is called to release the pointer to entire block of
   data. }

type
	CGDataProviderReleaseBytePointerCallback = procedure( info: UnivPtr; pointr: {const} UnivPtr );

{ This callback is called to copy `count' bytes at byte offset `position'
   into `buffer'. }

type
	CGDataProviderGetBytesAtPositionCallback = function( info: UnivPtr; buffer: UnivPtr; position: off_t; count: size_t ): size_t;

{ Callbacks for directly accessing data.
   `version' is the version of this structure. It should be set to 0.
   `getBytePointer', if non-NULL, is called to return a pointer to the
     provider's entire block of data.
   `releaseBytePointer', if non-NULL, is called to release a pointer to the
     provider's entire block of data.
   `getBytesAtPosition', if non-NULL, is called to copy `count' bytes at
     offset `position' from the provider's data to `buffer'. It should
     return the number of bytes copied, or 0 if there's no more data.
   `releaseInfo', if non-NULL, is called to release the `info' pointer when
     the provider is freed.

   At least one of `getBytePointer' or `getBytesAtPosition' must be
   non-NULL. }

type
	CGDataProviderDirectCallbacks = record
		version: UInt32;
{$ifc TARGET_CPU_64}
		__alignment_dummy: UInt32;
{$endc}
		getBytePointer: CGDataProviderGetBytePointerCallback;
		releaseBytePointer: CGDataProviderReleaseBytePointerCallback;
		getBytesAtPosition: CGDataProviderGetBytesAtPositionCallback;
		releaseInfo: CGDataProviderReleaseInfoCallback;
	end;

{ Return the CFTypeID for CGDataProviderRefs. }

function CGDataProviderGetTypeID: CFTypeID; external name '_CGDataProviderGetTypeID';
(* CG_AVAILABLE_STARTING(__MAC_10_2, __IPHONE_2_0) *)

{ Create a sequential-access data provider using `callbacks' to provide the
   data. `info' is passed to each of the callback functions. }

function CGDataProviderCreateSequential( info: UnivPtr; const (*var*) callbacks: CGDataProviderSequentialCallbacks ): CGDataProviderRef; external name '_CGDataProviderCreateSequential';
(* CG_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_2_0) *)

{ Create a direct-access data provider using `callbacks' to supply `size'
   bytes of data. `info' is passed to each of the callback functions. }

function CGDataProviderCreateDirect( info: UnivPtr; size: off_t; const (*var*) callbacks: CGDataProviderDirectCallbacks ): CGDataProviderRef; external name '_CGDataProviderCreateDirect';
(* CG_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_2_0) *)

{ The callback used by `CGDataProviderCreateWithData'. }

type
	CGDataProviderReleaseDataCallback = procedure( info: UnivPtr; data: {const} UnivPtr; size: size_t );

{ Create a direct-access data provider using `data', an array of `size'
   bytes. `releaseData' is called when the data provider is freed, and is
   passed `info' as its first argument. }

function CGDataProviderCreateWithData( info: UnivPtr; data: {const} UnivPtr; size: size_t; releaseData: CGDataProviderReleaseDataCallback ): CGDataProviderRef; external name '_CGDataProviderCreateWithData';
(* CG_AVAILABLE_STARTING(__MAC_10_0, __IPHONE_2_0) *)

{ Create a direct-access data provider which reads from `data'. }

function CGDataProviderCreateWithCFData( data: CFDataRef ): CGDataProviderRef; external name '_CGDataProviderCreateWithCFData';
(* CG_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_2_0) *)

{ Create a data provider reading from `url'. }

function CGDataProviderCreateWithURL( url: CFURLRef ): CGDataProviderRef; external name '_CGDataProviderCreateWithURL';
(* CG_AVAILABLE_STARTING(__MAC_10_0, __IPHONE_2_0) *)

{ Create a data provider reading from `filename'. }

function CGDataProviderCreateWithFilename( filename: ConstCStringPtr ): CGDataProviderRef; external name '_CGDataProviderCreateWithFilename';
(* CG_AVAILABLE_STARTING(__MAC_10_0, __IPHONE_2_0) *)

{ Equivalent to `CFRetain(provider)', but doesn't crash (as CFRetain does)
   if `provider' is NULL. }

function CGDataProviderRetain( provider: CGDataProviderRef ): CGDataProviderRef; external name '_CGDataProviderRetain';
(* CG_AVAILABLE_STARTING(__MAC_10_0, __IPHONE_2_0) *)

{ Equivalent to `CFRelease(provider)', but doesn't crash (as CFRelease
   does) if `provider' is NULL. }

procedure CGDataProviderRelease( provider: CGDataProviderRef ); external name '_CGDataProviderRelease';
(* CG_AVAILABLE_STARTING(__MAC_10_0, __IPHONE_2_0) *)

{ Return a copy of the data specified by provider. Returns NULL if a
   complete copy of the data can't be obtained (for example, if the
   underlying data is too large to fit in memory). }

function CGDataProviderCopyData( provider: CGDataProviderRef ): CFDataRef; external name '_CGDataProviderCopyData';
(* CG_AVAILABLE_STARTING(__MAC_10_3, __IPHONE_2_0) *)

{$ifc TARGET_OS_MAC}
{ Deprecated API. }

{ This callback is called to skip `count' bytes forward in the sequential
   data stream. }

type
	CGDataProviderSkipBytesCallback = procedure( info: UnivPtr; count: size_t );

{ Old-style callbacks for sequentially accessing data.
   `getBytes' is called to copy `count' bytes from the sequential data
     stream to `buffer'. It should return the number of bytes copied, or 0
     if there's no more data.
   `skipBytes' is called to skip ahead in the sequential data stream by
     `count' bytes.
   `rewind' is called to rewind the sequential data stream to the beginning
     of the data.
   `releaseProvider', if non-NULL, is called to release the `info' pointer
     when the provider is freed. }

type
	CGDataProviderCallbacks = record
		getBytes: CGDataProviderGetBytesCallback;
		skipBytes: CGDataProviderSkipBytesCallback;
		rewind: CGDataProviderRewindCallback;
		releaseProvider: CGDataProviderReleaseInfoCallback;
	end;

{ This callback is called to copy `count' bytes at byte offset `offset'
   into `buffer'. }

type
	CGDataProviderGetBytesAtOffsetCallback = function( info: UnivPtr; buffer: UnivPtr; offset: size_t; count: size_t ): size_t;

{ Callbacks for directly accessing data.
   `getBytePointer', if non-NULL, is called to return a pointer to the
     provider's entire block of data.
   `releaseBytePointer', if non-NULL, is called to release a pointer to the
     provider's entire block of data.
   `getBytes', if non-NULL, is called to copy `count' bytes at offset
     `offset' from the provider's data to `buffer'. It should return the
     number of bytes copied, or 0 if there's no more data.
   `releaseProvider', if non-NULL, is called when the provider is freed.
  
   At least one of `getBytePointer' or `getBytes' must be non-NULL. }

type
	CGDataProviderDirectAccessCallbacks = record
		getBytePointer: CGDataProviderGetBytePointerCallback;
		releaseBytePointer: CGDataProviderReleaseBytePointerCallback;
		getBytes: CGDataProviderGetBytesAtOffsetCallback;
		releaseProvider: CGDataProviderReleaseInfoCallback;
	end;

{ Create a sequential-access data provider using `callbacks' to provide the
   data. `info' is passed to each of the callback functions. }

function CGDataProviderCreate( info: UnivPtr; const (*var*) callbacks: CGDataProviderCallbacks ): CGDataProviderRef; external name '_CGDataProviderCreate';
(* CG_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_5, __IPHONE_NA, __IPHONE_NA) *)

{ Create a direct-access data provider using `callbacks' to supply `size'
   bytes of data. `info' is passed to each of the callback functions. }

function CGDataProviderCreateDirectAccess( info: UnivPtr; size: size_t; const (*var*) callbacks: CGDataProviderDirectAccessCallbacks ): CGDataProviderRef; external name '_CGDataProviderCreateDirectAccess';
(* CG_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_5, __IPHONE_NA, __IPHONE_NA) *)
{$endc}
{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}

end.
{$endc} {not MACOSALLINCLUDE}
