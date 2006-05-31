{	CFUUID.h
	Copyright (c) 1999-2005, Apple, Inc. All rights reserved.
}
{	  Pascal Translation Updated:  Peter N Lewis, <peter@stairways.com.au>, November 2005 }
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

unit CFUUID;
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
uses MacTypes,CFBase,CFString;
{$ALIGN POWER}


type
	CFUUIDRef = ^SInt32; { an opaque 32-bit type }
	CFUUIDRefPtr = ^CFUUIDRef;

type
	CFUUIDBytes = record
		byte0: SInt8;
		byte1: SInt8;
		byte2: SInt8;
		byte3: SInt8;
		byte4: SInt8;
		byte5: SInt8;
		byte6: SInt8;
		byte7: SInt8;
		byte8: SInt8;
		byte9: SInt8;
		byte10: SInt8;
		byte11: SInt8;
		byte12: SInt8;
		byte13: SInt8;
		byte14: SInt8;
		byte15: SInt8;
	end;
	CFUUIDBytesPtr = ^CFUUIDBytes;
{ The CFUUIDBytes struct is a 128-bit struct that contains the
raw UUID.  A CFUUIDRef can provide such a struct from the
CFUUIDGetUUIDBytes() function.  This struct is suitable for
passing to APIs that expect a raw UUID.
}
        
function CFUUIDGetTypeID: CFTypeID; external name '_CFUUIDGetTypeID';

function CFUUIDCreate( alloc: CFAllocatorRef ): CFUUIDRef; external name '_CFUUIDCreate';
    { Create and return a brand new unique identifier }

function CFUUIDCreateWithBytes( alloc: CFAllocatorRef; byte0: ByteParameter; byte1: ByteParameter; byte2: ByteParameter; byte3: ByteParameter; byte4: ByteParameter; byte5: ByteParameter; byte6: ByteParameter; byte7: ByteParameter; byte8: ByteParameter; byte9: ByteParameter; byte10: ByteParameter; byte11: ByteParameter; byte12: ByteParameter; byte13: ByteParameter; byte14: ByteParameter; byte15: ByteParameter ): CFUUIDRef; external name '_CFUUIDCreateWithBytes';
    { Create and return an identifier with the given contents.  This may return an existing instance with its ref count bumped because of uniquing. }

function CFUUIDCreateFromString( alloc: CFAllocatorRef; uuidStr: CFStringRef ): CFUUIDRef; external name '_CFUUIDCreateFromString';
    { Converts from a string representation to the UUID.  This may return an existing instance with its ref count bumped because of uniquing. }

function CFUUIDCreateString( alloc: CFAllocatorRef; uuid: CFUUIDRef ): CFStringRef; external name '_CFUUIDCreateString';
    { Converts from a UUID to its string representation. }

function CFUUIDGetConstantUUIDWithBytes( alloc: CFAllocatorRef; byte0: ByteParameter; byte1: ByteParameter; byte2: ByteParameter; byte3: ByteParameter; byte4: ByteParameter; byte5: ByteParameter; byte6: ByteParameter; byte7: ByteParameter; byte8: ByteParameter; byte9: ByteParameter; byte10: ByteParameter; byte11: ByteParameter; byte12: ByteParameter; byte13: ByteParameter; byte14: ByteParameter; byte15: ByteParameter ): CFUUIDRef; external name '_CFUUIDGetConstantUUIDWithBytes';
    { This returns an immortal CFUUIDRef that should not be released.  It can be used in headers to declare UUID constants with #define. }

function CFUUIDGetUUIDBytes( uuid: CFUUIDRef ): CFUUIDBytes; external name '_CFUUIDGetUUIDBytes';

function CFUUIDCreateFromUUIDBytes( alloc: CFAllocatorRef; bytes: CFUUIDBytes ): CFUUIDRef; external name '_CFUUIDCreateFromUUIDBytes';


end.
