{
     File:       CarbonCore/DateTimeUtils.h
 
     Contains:   International Date and Time Interfaces (previously in TextUtils)
 
     Copyright:  © 1994-2011 by Apple Inc. All rights reserved.
 
     Bugs?:      For bug reports, consult the following page on
                 the World Wide Web:
 
                     http://bugs.freepascal.org
 
}
{    Pascal Translation Updated:  Jonas Maebe, <jonas@freepascal.org>, October 2009 }
{    Pascal Translation Updated:  Jonas Maebe, <jonas@freepascal.org>, September 2012 }
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

unit DateTimeUtils;
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
uses MacTypes,UTCUtils,CFDate;
{$endc} {not MACOSALLINCLUDE}


{$ifc TARGET_OS_MAC}

{$ALIGN MAC68K}

{

    Here are the current routine names and the translations to the older forms.
    Please use the newer forms in all new code and migrate the older names out of existing
    code as maintainance permits.
    
    New Name                    Old Name(s)
    
    DateString                  IUDatePString IUDateString 
    InitDateCache
    LongDateString              IULDateString
    LongTimeString              IULTimeString
    StringToDate                String2Date
    StringToTime                                
    TimeString                  IUTimeString IUTimePString
    LongDateToSeconds           LongDate2Secs
    LongSecondsToDate           LongSecs2Date
    DateToSeconds               Date2Secs
    SecondsToDate               Secs2Date


    Carbon only supports the new names.  The old names are undefined for Carbon targets.
    This is true for C, Assembly and Pascal.
    
    InterfaceLib always has exported the old names.  For C macros have been defined to allow
    the use of the new names.  For Pascal and Assembly using the new names will result
    in link errors. 
    
}

type
	ToggleResults = SInt16;
const
{ Toggle results }
	toggleUndefined = 0;
	toggleOK = 1;
	toggleBadField = 2;
	toggleBadDelta = 3;
	toggleBadChar = 4;
	toggleUnknown = 5;
	toggleBadNum = 6;
	toggleOutOfRange = 7;    {synonym for toggleErr3}
	toggleErr3 = 7;
	toggleErr4 = 8;
	toggleErr5 = 9;

const
{ Date equates }
	smallDateBit = 31;   {Restrict valid date/time to range of Time global}
	togChar12HourBit = 30;   {If toggling hour by char, accept hours 1..12 only}
	togCharZCycleBit = 29;   {Modifier for togChar12HourBit: accept hours 0..11 only}
	togDelta12HourBit = 28;   {If toggling hour up/down, restrict to 12-hour range (am/pm)}
	genCdevRangeBit = 27;   {Restrict date/time to range used by genl CDEV}
	validDateFields = -1;
	maxDateField = 10;

const
	eraMask = $0001;
	yearMask = $0002;
	monthMask = $0004;
	dayMask = $0008;
	hourMask = $0010;
	minuteMask = $0020;
	secondMask = $0040;
	dayOfWeekMask = $0080;
	dayOfYearMask = $0100;
	weekOfYearMask = $0200;
	pmMask = $0400;
	dateStdMask = $007F; {default for ValidDate flags and ToggleDate TogglePB.togFlags}

type
	LongDateField = SInt8;
const
	eraField = 0;
	yearField = 1;
	monthField = 2;
	dayField = 3;
	hourField = 4;
	minuteField = 5;
	secondField = 6;
	dayOfWeekField = 7;
	dayOfYearField = 8;
	weekOfYearField = 9;
	pmField = 10;
	res1Field = 11;
	res2Field = 12;
	res3Field = 13;

type
	DateForm = SInt8;
const
	shortDate = 0;
	longDate = 1;
	abbrevDate = 2;

const
{ StringToDate status values }
	fatalDateTime = $8000; { StringToDate and String2Time mask to a fatal error }
	longDateFound = 1;    { StringToDate mask to long date found }
	leftOverChars = 2;    { StringToDate & Time mask to warn of left over characters }
	sepNotIntlSep = 4;    { StringToDate & Time mask to warn of non-standard separators }
	fieldOrderNotIntl = 8;    { StringToDate & Time mask to warn of non-standard field order }
	extraneousStrings = 16;   { StringToDate & Time mask to warn of unparsable strings in text }
	tooManySeps = 32;   { StringToDate & Time mask to warn of too many separators }
	sepNotConsistent = 64;   { StringToDate & Time mask to warn of inconsistent separators }
	tokenErr = $8100; { StringToDate & Time mask for 'tokenizer err encountered' }
	cantReadUtilities = $8200;
	dateTimeNotFound = $8400;
	dateTimeInvalid = $8800;

type
	StringToDateStatus = SInt16;
	String2DateStatus = StringToDateStatus;
	DateCacheRecordPtr = ^DateCacheRecord;
	DateCacheRecord = packed record
		hidden: array [0..255] of SInt16;				{ only for temporary use }
	end;
type
	DateCachePtr = DateCacheRecordPtr;
	DateTimeRecPtr = ^DateTimeRec;
	DateTimeRec = record
		year: SInt16;
		month: SInt16;
		day: SInt16;
		hour: SInt16;
		minute: SInt16;
		second: SInt16;
		dayOfWeek: SInt16;
	end;

type
	LongDateTime = SInt64;
	LongDateTimePtr = ^LongDateTime;
{$ifc TARGET_RT_BIG_ENDIAN}
	LongDateCvt = record
		case SInt16 of
		0: (
			c: SInt64;
			);
		1: (
			lHigh: UInt32;
			lLow: UInt32;
		   );
	end;
{$elsec}
	LongDateCvt = record
		case SInt16 of
		0: (
			c: SInt64;
			);
		1: (
			lLow: UInt32;
			lHigh: UInt32;
		   );
	end;
{$endc}
	LongDateCvtPtr = ^LongDateCvt;

	LongDateRecPtr = ^LongDateRec;
	LongDateRec = record
		case SInt16 of
		0: (
			era: SInt16;
			year: SInt16;
			month: SInt16;
			day: SInt16;
			hour: SInt16;
			minute: SInt16;
			second: SInt16;
			dayOfWeek: SInt16;
			dayOfYear: SInt16;
			weekOfYear: SInt16;
			pm: SInt16;
			res1: SInt16;
			res2: SInt16;
			res3: SInt16;
		   );
		1: (
			list: array [0..13] of SInt16;				{Index by LongDateField!}
			);
		2: (
			eraAlt: SInt16;
			oldDate: DateTimeRec;
		   );
	end;

type
	DateDelta = SInt8;
	TogglePBPtr = ^TogglePB;
	TogglePB = record
		togFlags: SIGNEDLONG;               {caller normally sets low word to dateStdMask=$7F}
		amChars: ResType;                {from 'itl0', but uppercased}
		pmChars: ResType;                {from 'itl0', but uppercased}
		reserved: array [0..3] of SInt32;
	end;
{
    Conversion utilities between CF and Carbon time types. 
}
{
 *  UCConvertUTCDateTimeToCFAbsoluteTime()
 *  
 *  Discussion:
 *    Use UCConvertUTCDateTimeToCFAbsoluteTime to convert from a
 *    UTCDDateTime to a CFAbsoluteTime. Remember that the epoch for
 *    UTCDateTime is January 1, 1904 while the epoch for CFAbsoluteTime
 *    is January 1, 2001.
 *  
 *  Parameters:
 *    
 *    iUTCDate:
 *      A pointer to a UTCDateTime struct that represents the time you
 *      wish to convert from.
 *    
 *    oCFTime:
 *      A pointer to a CFAbsoluteTime. On successful return, this will
 *      contain the converted time from the input time type.
 *  
 *  Result:
 *    A result code indicating whether or not conversion was successful.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }
function UCConvertUTCDateTimeToCFAbsoluteTime( const (*var*) iUTCDate: UTCDateTime; var oCFTime: CFAbsoluteTime ): OSStatus; external name '_UCConvertUTCDateTimeToCFAbsoluteTime';
(* __OSX_AVAILABLE_STARTING(__MAC_10_2, __IPHONE_NA) *)


{
 *  UCConvertSecondsToCFAbsoluteTime()
 *  
 *  Discussion:
 *    Use UCConvertSecondsToCFAbsoluteTime to convert from the normal
 *    seconds representation of time to a CFAbsoluteTime. Remember that
 *    the epoch for seconds is January 1, 1904 while the epoch for
 *    CFAbsoluteTime is January 1, 2001.
 *  
 *  Parameters:
 *    
 *    iSeconds:
 *      A UInt32 value that represents the time you wish to convert
 *      from.
 *    
 *    oCFTime:
 *      A pointer to a CFAbsoluteTime. On successful return, this will
 *      contain the converted time from the input time type.
 *  
 *  Result:
 *    A result code indicating whether or not conversion was successful.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }
function UCConvertSecondsToCFAbsoluteTime( iSeconds: UInt32; var oCFTime: CFAbsoluteTime ): OSStatus; external name '_UCConvertSecondsToCFAbsoluteTime';
(* __OSX_AVAILABLE_STARTING(__MAC_10_2, __IPHONE_NA) *)


{
 *  UCConvertLongDateTimeToCFAbsoluteTime()
 *  
 *  Discussion:
 *    Use UCConvertLongDateTimeToCFAbsoluteTime to convert from a
 *    LongDateTime to a CFAbsoluteTime. Remember that the epoch for
 *    LongDateTime is January 1, 1904 while the epoch for
 *    CFAbsoluteTime is January 1, 2001.
 *  
 *  Parameters:
 *    
 *    iLongTime:
 *      A LongDateTime value that represents the time you wish to
 *      convert from.
 *    
 *    oCFTime:
 *      A pointer to a CFAbsoluteTime. On successful return, this will
 *      contain the converted time from the input time type.
 *  
 *  Result:
 *    A result code indicating whether or not conversion was successful.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }
function UCConvertLongDateTimeToCFAbsoluteTime( iLongTime: LongDateTime; var oCFTime: CFAbsoluteTime ): OSStatus; external name '_UCConvertLongDateTimeToCFAbsoluteTime';
(* __OSX_AVAILABLE_STARTING(__MAC_10_2, __IPHONE_NA) *)


{
 *  UCConvertCFAbsoluteTimeToUTCDateTime()
 *  
 *  Discussion:
 *    Use UCConvertCFAbsoluteTimeToUTCDateTime to convert from a
 *    CFAbsoluteTime to a UTCDateTime. Remember that the epoch for
 *    UTCDateTime is January 1, 1904 while the epoch for CFAbsoluteTime
 *    is January 1, 2001.
 *  
 *  Parameters:
 *    
 *    iCFTime:
 *      A CFAbsoluteTime value that represents the time you wish to
 *      convert from.
 *    
 *    oUTCDate:
 *      A pointer to a UTCDateTime. On successful return, this will
 *      contain the converted time from the CFAbsoluteTime input.
 *  
 *  Result:
 *    A result code indicating whether or not conversion was successful.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }
function UCConvertCFAbsoluteTimeToUTCDateTime( iCFTime: CFAbsoluteTime; var oUTCDate: UTCDateTime ): OSStatus; external name '_UCConvertCFAbsoluteTimeToUTCDateTime';
(* __OSX_AVAILABLE_STARTING(__MAC_10_2, __IPHONE_NA) *)


{
 *  UCConvertCFAbsoluteTimeToSeconds()
 *  
 *  Discussion:
 *    Use UCConvertCFAbsoluteTimeToSeconds to convert from a
 *    CFAbsoluteTime to a UInt32 representation of seconds. Remember
 *    that the epoch for seconds is January 1, 1904 while the epoch for
 *    CFAbsoluteTime is January 1, 2001.
 *  
 *  Parameters:
 *    
 *    iCFTime:
 *      A CFAbsoluteTime value that represents the time you wish to
 *      convert from.
 *    
 *    oSeconds:
 *      A pointer to a UInt32. On successful return, this will contain
 *      the converted time from the CFAbsoluteTime input.
 *  
 *  Result:
 *    A result code indicating whether or not conversion was successful.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }
function UCConvertCFAbsoluteTimeToSeconds( iCFTime: CFAbsoluteTime; var oSeconds: UInt32 ): OSStatus; external name '_UCConvertCFAbsoluteTimeToSeconds';
(* __OSX_AVAILABLE_STARTING(__MAC_10_2, __IPHONE_NA) *)


{
 *  UCConvertCFAbsoluteTimeToLongDateTime()
 *  
 *  Discussion:
 *    Use UCConvertCFAbsoluteTimeToLongDateTime to convert from a
 *    CFAbsoluteTime to a LongDateTime. Remember that the epoch for
 *    LongDateTime is January 1, 1904 while the epoch for
 *    CFAbsoluteTime is January 1, 2001.
 *  
 *  Parameters:
 *    
 *    iCFTime:
 *      A CFAbsoluteTime value that represents the time you wish to
 *      convert from.
 *    
 *    oLongDate:
 *      A pointer to a LongDateTime. On successful return, this will
 *      contain the converted time from the CFAbsoluteTime input.
 *  
 *  Result:
 *    A result code indicating whether or not conversion was successful.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }
function UCConvertCFAbsoluteTimeToLongDateTime( iCFTime: CFAbsoluteTime; var oLongDate: LongDateTime ): OSStatus; external name '_UCConvertCFAbsoluteTimeToLongDateTime';
(* __OSX_AVAILABLE_STARTING(__MAC_10_2, __IPHONE_NA) *)


{$ifc not TARGET_CPU_64}
{
    These routine are available in Carbon with their new name
}
{$ifc not TARGET_CPU_64}
{
 *  DateString()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    use CFDateFormatterCreateStringWithDate instead
 *  
 *  Discussion:
 *    This function is no longer recommended. Please use
 *    CFDateFormatterCreateStringWithDate instead.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.3
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   not available
 }
procedure DateString( dateTime: SInt32; longFlag: DateForm; var result: Str255; intlHandle: Handle ); external name '_DateString';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_3, __IPHONE_NA, __IPHONE_NA) *)


{
 *  TimeString()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    use CFDateFormatterCreateStringWithDate instead
 *  
 *  Discussion:
 *    This function is no longer recommended. Please use
 *    CFDateFormatterCreateStringWithDate instead.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.3
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   not available
 }
procedure TimeString( dateTime: SInt32; wantSeconds: Boolean; var result: Str255; intlHandle: Handle ); external name '_TimeString';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_3, __IPHONE_NA, __IPHONE_NA) *)


{
 *  LongDateString()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    use CFDateFormatterCreateStringWithDate instead
 *  
 *  Discussion:
 *    This function is no longer recommended. Please use
 *    CFDateFormatterCreateStringWithDate instead.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.3
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   not available
 }
procedure LongDateString( (*const*) var dateTime: LongDateTime; longFlag: DateForm; var result: Str255; intlHandle: Handle ); external name '_LongDateString';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_3, __IPHONE_NA, __IPHONE_NA) *)


{
 *  LongTimeString()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    use CFDateFormatterCreateStringWithDate instead
 *  
 *  Discussion:
 *    This function is no longer recommended. Please use
 *    CFDateFormatterCreateStringWithDate instead.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.3
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   not available
 }
procedure LongTimeString( (*const*) var dateTime: LongDateTime; wantSeconds: Boolean; var result: Str255; intlHandle: Handle ); external name '_LongTimeString';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_3, __IPHONE_NA, __IPHONE_NA) *)


{
    These routine are available in Carbon and InterfaceLib with their new name
}
{
 *  InitDateCache()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    No longer needed on MacOS X.
 *  
 *  Discussion:
 *    This function is obsolate.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.3
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function InitDateCache( theCache: DateCachePtr ): OSErr; external name '_InitDateCache';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_3, __IPHONE_NA, __IPHONE_NA) *)


{
 *  StringToDate()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    use CFDateFormatterCreateDateFromString instead
 *  
 *  Discussion:
 *    This function is no longer recommended. Please use
 *    CFDateFormatterCreateDateFromString instead.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.3
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function StringToDate( textPtr: Ptr; textLen: SInt32; theCache: DateCachePtr; var lengthUsed: SInt32; var dateTime: LongDateRec ): StringToDateStatus; external name '_StringToDate';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_3, __IPHONE_NA, __IPHONE_NA) *)


{
 *  StringToTime()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    use CFDateFormatterCreateDateFromString instead
 *  
 *  Discussion:
 *    This function is no longer recommended. Please use
 *    CFDateFormatterCreateDateFromString instead.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.3
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function StringToTime( textPtr: Ptr; textLen: SInt32; theCache: DateCachePtr; var lengthUsed: SInt32; var dateTime: LongDateRec ): StringToDateStatus; external name '_StringToTime';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_3, __IPHONE_NA, __IPHONE_NA) *)


{
 *  LongDateToSeconds()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    use CFCalendar instead.
 *  
 *  Discussion:
 *    This function is no longer recommended. Please use CFCalendar
 *    instead.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.3
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
procedure LongDateToSeconds( const (*var*) lDate: LongDateRec; var lSecs: LongDateTime ); external name '_LongDateToSeconds';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_3, __IPHONE_NA, __IPHONE_NA) *)


{
 *  LongSecondsToDate()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    use CFCalendar instead.
 *  
 *  Discussion:
 *    This function is no longer recommended. Please use CFCalendar
 *    instead.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.3
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
procedure LongSecondsToDate( (*const*) var lSecs: LongDateTime; var lDate: LongDateRec ); external name '_LongSecondsToDate';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_3, __IPHONE_NA, __IPHONE_NA) *)


{
 *  ToggleDate()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    use CFCalendar instead.
 *  
 *  Discussion:
 *    This function is no longer recommended. Please use CFCalendar
 *    instead.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.3
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function ToggleDate( var lSecs: LongDateTime; field: LongDateField; delta: DateDelta; ch: SInt16; const (*var*) params: TogglePB ): ToggleResults; external name '_ToggleDate';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_3, __IPHONE_NA, __IPHONE_NA) *)


{
 *  ValidDate()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    use CFCalendar instead.
 *  
 *  Discussion:
 *    This function is no longer recommended. Please use CFCalendar
 *    instead.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.3
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function ValidDate( const (*var*) vDate: LongDateRec; flags: SIGNEDLONG; var newSecs: LongDateTime ): SInt16; external name '_ValidDate';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_3, __IPHONE_NA, __IPHONE_NA) *)


{
 *  ReadDateTime()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    use CFAbsoluteTimeGetCurrent instead.
 *  
 *  Discussion:
 *    This function is no longer recommended. Please use
 *    CFAbsoluteTimeGetCurrent instead.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.3
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function ReadDateTime( var datetime: UNSIGNEDLONG ): OSErr; external name '_ReadDateTime';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_3, __IPHONE_NA, __IPHONE_NA) *)


{
 *  GetDateTime()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    use CFAbsoluteTimeGetCurrent instead.
 *  
 *  Discussion:
 *    This function is no longer recommended. Please use
 *    CFAbsoluteTimeGetCurrent instead.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.3
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
procedure GetDateTime( var secs: UNSIGNEDLONG ); external name '_GetDateTime';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_3, __IPHONE_NA, __IPHONE_NA) *)


{
 *  SetDateTime()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    deprecated it without replacement
 *  
 *  Discussion:
 *    This function is no longer recommended.  One has to be root on
 *    MacOSX to set. This is done through other means on OS X
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.3
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function SetDateTime( datetime: UNSIGNEDLONG ): OSErr; external name '_SetDateTime';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_3, __IPHONE_NA, __IPHONE_NA) *)


{
 *  SetTime()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    deprecated it without replacement
 *  
 *  Discussion:
 *    This function is no longer recommended.  One has to be root on
 *    MacOSX to set. This is done through other means on OS X
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.3
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
procedure SetTime( const (*var*) d: DateTimeRec ); external name '_SetTime';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_3, __IPHONE_NA, __IPHONE_NA) *)


{
 *  GetTime()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    use CFAbsoluteTimeGetCurrent instead.
 *  
 *  Discussion:
 *    This function is no longer recommended. Please use
 *    CFAbsoluteTimeGetCurrent instead.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.3
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
procedure GetTime( var d: DateTimeRec ); external name '_GetTime';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_3, __IPHONE_NA, __IPHONE_NA) *)


{$endc} {not TARGET_CPU_64}

{$ifc not TARGET_CPU_64}
{
 *  DateToSeconds()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    use CFCalendar instead.
 *  
 *  Discussion:
 *    This function is no longer recommended. Please use CFCalendar
 *    instead.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.3
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
procedure DateToSeconds( const (*var*) d: DateTimeRec; var secs: UNSIGNEDLONG ); external name '_DateToSeconds';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_3, __IPHONE_NA, __IPHONE_NA) *)


{$endc} {not TARGET_CPU_64}

{$ifc not TARGET_CPU_64}
{
 *  SecondsToDate()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    use CFCalendar instead.
 *  
 *  Discussion:
 *    This function is no longer recommended. Please use CFCalendar
 *    instead.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.3
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
procedure SecondsToDate( secs: UNSIGNEDLONG; var d: DateTimeRec ); external name '_SecondsToDate';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_3, __IPHONE_NA, __IPHONE_NA) *)


{
    These routine are available in InterfaceLib using their old name.
    Macros allow using the new names in all source code.
    
    @deprecated use CFDateFormatter instead.
}
{
 *  IUDateString()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         not available [32-bit only] but deprecated in 10.3
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }


{
 *  IUTimeString()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         not available [32-bit only] but deprecated in 10.3
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }


{
 *  IUDatePString()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         not available [32-bit only] but deprecated in 10.3
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }


{
 *  IUTimePString()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         not available [32-bit only] but deprecated in 10.3
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }


{
 *  IULDateString()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         not available [32-bit only] but deprecated in 10.3
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }


{
 *  IULTimeString()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         not available [32-bit only] but deprecated in 10.3
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }


{$endc} {not TARGET_CPU_64}

{
 *  iudatestring()
 *  
 *  Availability:
 *    Mac OS X:         not available
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }


{
 *  iudatepstring()
 *  
 *  Availability:
 *    Mac OS X:         not available
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }


{
 *  iutimestring()
 *  
 *  Availability:
 *    Mac OS X:         not available
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }


{
 *  iutimepstring()
 *  
 *  Availability:
 *    Mac OS X:         not available
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }


{
 *  iuldatestring()
 *  
 *  Availability:
 *    Mac OS X:         not available
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }


{
 *  iultimestring()
 *  
 *  Availability:
 *    Mac OS X:         not available
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }


{$endc} {not TARGET_CPU_64}


{$endc} {TARGET_OS_MAC}
{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}

end.
{$endc} {not MACOSALLINCLUDE}
