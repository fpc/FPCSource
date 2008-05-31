{	CFCalendar.h
	Copyright (c) 2004-2005, Apple, Inc. All rights reserved.
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

unit CFCalendar;
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
uses MacTypes,CFBase,CFLocale,CFDate,CFTimeZone;
{$ALIGN POWER}


{#if MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4}


type
	CFCalendarRef = ^SInt32; { an opaque 32-bit type }

function CFCalendarGetTypeID: CFTypeID; external name '_CFCalendarGetTypeID';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

function CFCalendarCopyCurrent: CFCalendarRef; external name '_CFCalendarCopyCurrent';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

function CFCalendarCreateWithIdentifier( allocator: CFAllocatorRef; identifier: CFStringRef ): CFCalendarRef; external name '_CFCalendarCreateWithIdentifier';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
	// Create a calendar.  The identifiers are the kCF*Calendar
	// constants in CFLocale.h.

function CFCalendarGetIdentifier( calendar: CFCalendarRef ): CFStringRef; external name '_CFCalendarGetIdentifier';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
	// Returns the calendar's identifier.

function CFCalendarCopyLocale( calendar: CFCalendarRef ): CFLocaleRef; external name '_CFCalendarCopyLocale';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

procedure CFCalendarSetLocale( calendar: CFCalendarRef; locale: CFLocaleRef ); external name '_CFCalendarSetLocale';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

function CFCalendarCopyTimeZone( calendar: CFCalendarRef ): CFTimeZoneRef; external name '_CFCalendarCopyTimeZone';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

procedure CFCalendarSetTimeZone( calendar: CFCalendarRef; tz: CFTimeZoneRef ); external name '_CFCalendarSetTimeZone';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

function CFCalendarGetFirstWeekday( calendar: CFCalendarRef ): CFIndex; external name '_CFCalendarGetFirstWeekday';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

procedure CFCalendarSetFirstWeekday( calendar: CFCalendarRef; wkdy: CFIndex ); external name '_CFCalendarSetFirstWeekday';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

function CFCalendarGetMinimumDaysInFirstWeek( calendar: CFCalendarRef ): CFIndex; external name '_CFCalendarGetMinimumDaysInFirstWeek';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

procedure CFCalendarSetMinimumDaysInFirstWeek( calendar: CFCalendarRef; mwd: CFIndex ); external name '_CFCalendarSetMinimumDaysInFirstWeek';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


type
	CFCalendarUnit = SInt32;
const
	kCFCalendarUnitEra = 1 shl 1;
	kCFCalendarUnitYear = 1 shl 2;
	kCFCalendarUnitMonth = 1 shl 3;
	kCFCalendarUnitDay = 1 shl 4;
	kCFCalendarUnitHour = 1 shl 5;
	kCFCalendarUnitMinute = 1 shl 6;
	kCFCalendarUnitSecond = 1 shl 7;
	kCFCalendarUnitWeek = 1 shl 8;
	kCFCalendarUnitWeekday = 1 shl 9;
	kCFCalendarUnitWeekdayOrdinal = 1 shl 10;

function CFCalendarGetMinimumRangeOfUnit( calendar: CFCalendarRef; unt: CFCalendarUnit ): CFRange; external name '_CFCalendarGetMinimumRangeOfUnit';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

function CFCalendarGetMaximumRangeOfUnit( calendar: CFCalendarRef; unt: CFCalendarUnit ): CFRange; external name '_CFCalendarGetMaximumRangeOfUnit';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

function CFCalendarGetRangeOfUnit( calendar: CFCalendarRef; smallerUnit: CFCalendarUnit; biggerUnit: CFCalendarUnit; at: CFAbsoluteTime ): CFRange; external name '_CFCalendarGetRangeOfUnit';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

function CFCalendarGetOrdinalityOfUnit( calendar: CFCalendarRef; smallerUnit: CFCalendarUnit; biggerUnit: CFCalendarUnit; at: CFAbsoluteTime ): CFIndex; external name '_CFCalendarGetOrdinalityOfUnit';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


function CFCalendarComposeAbsoluteTime( calendar: CFCalendarRef; var at: { out } CFAbsoluteTime; componentDesc: ConstCStringPtr; ... ): Boolean; external name '_CFCalendarComposeAbsoluteTime';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

function CFCalendarDecomposeAbsoluteTime( calendar: CFCalendarRef; at: CFAbsoluteTime; componentDesc: ConstCStringPtr; ... ): Boolean; external name '_CFCalendarDecomposeAbsoluteTime';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


const
	kCFCalendarComponentsWrap = 1 shl 0;  // option for adding

function CFCalendarAddComponents( calendar: CFCalendarRef; var at: { out } CFAbsoluteTime; options: CFOptionFlags; componentDesc: ConstCStringPtr; ... ): Boolean; external name '_CFCalendarAddComponents';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

function CFCalendarGetComponentDifference( calendar: CFCalendarRef; startingAT: CFAbsoluteTime; resultAT: CFAbsoluteTime; options: CFOptionFlags; componentDesc: ConstCStringPtr; ... ): Boolean; external name '_CFCalendarGetComponentDifference';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)



{#endif}


end.
