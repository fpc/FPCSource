{	CFNumber.h
	Copyright (c) 1999-2005, Apple, Inc. All rights reserved.
}
{   Pascal Translation Updated:  Peter N Lewis, <peter@stairways.com.au>, September 2005 }
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

unit CFNumber;
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
uses MacTypes,CFBase;
{$ALIGN POWER}


type
	CFBooleanRef = ^SInt32; { an opaque 32-bit type }
	CFBooleanRefPtr = ^CFBooleanRef;

var kCFBooleanTrue: CFBooleanRef; external name '_kCFBooleanTrue'; (* attribute const *)
var kCFBooleanFalse: CFBooleanRef; external name '_kCFBooleanFalse'; (* attribute const *)

function CFBooleanGetTypeID: CFTypeID; external name '_CFBooleanGetTypeID';

function CFBooleanGetValue( value: CFBooleanRef ): Boolean; external name '_CFBooleanGetValue';

type
	CFNumberType = SInt32;
const
																{  Types from MacTypes.h  }
	kCFNumberSInt8Type = 1;
	kCFNumberSInt16Type = 2;
	kCFNumberSInt32Type = 3;
	kCFNumberSInt64Type = 4;
	kCFNumberFloat32Type = 5;
	kCFNumberFloat64Type = 6;							{  64-bit IEEE 754  }
																{  Basic C types  }
	kCFNumberCharType = 7;
	kCFNumberShortType = 8;
	kCFNumberIntType = 9;
	kCFNumberLongType = 10;
	kCFNumberLongLongType = 11;
	kCFNumberFloatType = 12;
	kCFNumberDoubleType = 13;							{  Other  }
	kCFNumberCFIndexType = 14;
	kCFNumberMaxType = 14;

type
	CFNumberRef = ^SInt32; { an opaque 32-bit type }
	CFNumberRefPtr = ^CFNumberRef;

var kCFNumberPositiveInfinity: CFNumberRef; external name '_kCFNumberPositiveInfinity'; (* attribute const *)
var kCFNumberNegativeInfinity: CFNumberRef; external name '_kCFNumberNegativeInfinity'; (* attribute const *)
var kCFNumberNaN: CFNumberRef; external name '_kCFNumberNaN'; (* attribute const *)

function CFNumberGetTypeID: CFTypeID; external name '_CFNumberGetTypeID';

{
	Creates a CFNumber with the given value. The type of number pointed
	to by the valuePtr is specified by type. If type is a floating point
	type and the value represents one of the infinities or NaN, the
	well-defined CFNumber for that value is returned. If either of
	valuePtr or type is an invalid value, the result is undefined.
}
function CFNumberCreate( allocator: CFAllocatorRef; theType: CFNumberType; valuePtr: {const} UnivPtr ): CFNumberRef; external name '_CFNumberCreate';

{
	Returns the storage format of the CFNumber's value.  Note that
	this is not necessarily the type provided in CFNumberCreate().
}
function CFNumberGetType( number: CFNumberRef ): CFNumberType; external name '_CFNumberGetType';

{
	Returns the size in bytes of the type of the number.
}
function CFNumberGetByteSize( number: CFNumberRef ): CFIndex; external name '_CFNumberGetByteSize';

{
	Returns true if the type of the CFNumber's value is one of
	the defined floating point types.
}
function CFNumberIsFloatType( number: CFNumberRef ): Boolean; external name '_CFNumberIsFloatType';

{
	Copies the CFNumber's value into the space pointed to by
	valuePtr, as the specified type. If conversion needs to take
	place, the conversion rules follow human expectation and not
	C's promotion and truncation rules. If the conversion is
	lossy, or the value is out of range, false is returned. Best
	attempt at conversion will still be in *valuePtr.
}
function CFNumberGetValue( number: CFNumberRef; theType: CFNumberType; valuePtr: UnivPtr ): Boolean; external name '_CFNumberGetValue';

{
	Compares the two CFNumber instances. If conversion of the
	types of the values is needed, the conversion and comparison
	follow human expectations and not C's promotion and comparison
	rules. Negative zero compares less than positive zero.
	Positive infinity compares greater than everything except
	itself, to which it compares equal. Negative infinity compares
	less than everything except itself, to which it compares equal.
	Unlike standard practice, if both numbers are NaN, then they
	compare equal; if only one of the numbers is NaN, then the NaN
	compares greater than the other number if it is negative, and
	smaller than the other number if it is positive. (Note that in
	CFEqual() with two CFNumbers, if either or both of the numbers
	is NaN, true is returned.)
}
function CFNumberCompare( number: CFNumberRef; otherNumber: CFNumberRef; context: UnivPtr ): CFComparisonResult; external name '_CFNumberCompare';


end.
