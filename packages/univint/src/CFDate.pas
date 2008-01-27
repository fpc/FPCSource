{	CFDate.h
	Copyright (c) 1998-2005, Apple, Inc. All rights reserved.
}
{   Pascal Translation Updated:  Peter N Lewis, <peter@stairways.com.au>, September 2005 }
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

unit CFDate;
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
uses MacTypes,CFBase;
{$ALIGN POWER}


type
	CFTimeInterval = Float64;
	CFAbsoluteTime = CFTimeInterval;
	CFAbsoluteTimePtr = ^CFAbsoluteTime;
{ absolute time is the time interval since the reference date }
{ the reference date (epoch) is 00:00:00 1 January 2001. }

function CFAbsoluteTimeGetCurrent: CFAbsoluteTime; external name '_CFAbsoluteTimeGetCurrent';

var kCFAbsoluteTimeIntervalSince1970: CFTimeInterval; external name '_kCFAbsoluteTimeIntervalSince1970'; (* attribute const *)
var kCFAbsoluteTimeIntervalSince1904: CFTimeInterval; external name '_kCFAbsoluteTimeIntervalSince1904'; (* attribute const *)

type
	CFDateRef = ^SInt32; { an opaque 32-bit type }
	CFDateRefPtr = ^CFDateRef;

function CFDateGetTypeID: CFTypeID; external name '_CFDateGetTypeID';

function CFDateCreate( allocator: CFAllocatorRef; at: CFAbsoluteTime ): CFDateRef; external name '_CFDateCreate';

function CFDateGetAbsoluteTime( theDate: CFDateRef ): CFAbsoluteTime; external name '_CFDateGetAbsoluteTime';

function CFDateGetTimeIntervalSinceDate( theDate: CFDateRef; otherDate: CFDateRef ): CFTimeInterval; external name '_CFDateGetTimeIntervalSinceDate';

function CFDateCompare( theDate: CFDateRef; otherDate: CFDateRef; context: UnivPtr ): CFComparisonResult; external name '_CFDateCompare';

type
	CFTimeZoneRef = ^SInt32; { an opaque 32-bit type }
	CFTimeZoneRefPtr = ^CFTimeZoneRef;

type
	CFGregorianDate = record
		year: SInt32;
		month: SInt8;
		day: SInt8;
		hour: SInt8;
		minute: SInt8;
		second: Float64;
	end;
	CFGregorianDatePtr = ^CFGregorianDate;

type
	CFGregorianUnits = record
		years: SInt32;
		months: SInt32;
		days: SInt32;
		hours: SInt32;
		minutes: SInt32;
		seconds: Float64;
	end;
	CFGregorianUnitsPtr = ^CFGregorianUnits;

type
	CFGregorianUnitFlags = SInt32;
const
	kCFGregorianUnitsYears = 1 shl 0;
	kCFGregorianUnitsMonths = 1 shl 1;
	kCFGregorianUnitsDays = 1 shl 2;
	kCFGregorianUnitsHours = 1 shl 3;
	kCFGregorianUnitsMinutes = 1 shl 4;
	kCFGregorianUnitsSeconds = 1 shl 5;
	kCFGregorianAllUnits = $00FFFFFF;

function CFGregorianDateIsValid( gdate: CFGregorianDate; unitFlags: CFOptionFlags ): Boolean; external name '_CFGregorianDateIsValid';

function CFGregorianDateGetAbsoluteTime( gdate: CFGregorianDate; tz: CFTimeZoneRef ): CFAbsoluteTime; external name '_CFGregorianDateGetAbsoluteTime';

function CFAbsoluteTimeGetGregorianDate( at: CFAbsoluteTime; tz: CFTimeZoneRef ): CFGregorianDate; external name '_CFAbsoluteTimeGetGregorianDate';

function CFAbsoluteTimeAddGregorianUnits( at: CFAbsoluteTime; tz: CFTimeZoneRef; units: CFGregorianUnits ): CFAbsoluteTime; external name '_CFAbsoluteTimeAddGregorianUnits';

function CFAbsoluteTimeGetDifferenceAsGregorianUnits( at1: CFAbsoluteTime; at2: CFAbsoluteTime; tz: CFTimeZoneRef; unitFlags: CFOptionFlags ): CFGregorianUnits; external name '_CFAbsoluteTimeGetDifferenceAsGregorianUnits';

function CFAbsoluteTimeGetDayOfWeek( at: CFAbsoluteTime; tz: CFTimeZoneRef ): SInt32; external name '_CFAbsoluteTimeGetDayOfWeek';

function CFAbsoluteTimeGetDayOfYear( at: CFAbsoluteTime; tz: CFTimeZoneRef ): SInt32; external name '_CFAbsoluteTimeGetDayOfYear';

function CFAbsoluteTimeGetWeekOfYear( at: CFAbsoluteTime; tz: CFTimeZoneRef ): SInt32; external name '_CFAbsoluteTimeGetWeekOfYear';


end.
