{
     File:       DateTimeUtils.p
 
     Contains:   International Date and Time Interfaces (previously in TextUtils)
 
     Version:    Technology: Mac OS 8.5
                 Release:    Universal Interfaces 3.4.2
 
     Copyright:  © 1994-2002 by Apple Computer, Inc., all rights reserved.
 
     Bugs?:      For bug reports, consult the following page on
                 the World Wide Web:
 
                     http://www.freepascal.org/bugs.html
 
}


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

unit DateTimeUtils;
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
uses MacTypes,ConditionalMacros;


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
	ToggleResults 				= SInt16;
const
																{  Toggle results  }
	toggleUndefined				= 0;
	toggleOK					= 1;
	toggleBadField				= 2;
	toggleBadDelta				= 3;
	toggleBadChar				= 4;
	toggleUnknown				= 5;
	toggleBadNum				= 6;
	toggleOutOfRange			= 7;							{ synonym for toggleErr3 }
	toggleErr3					= 7;
	toggleErr4					= 8;
	toggleErr5					= 9;

																{  Date equates  }
	smallDateBit				= 31;							{ Restrict valid date/time to range of Time global }
	togChar12HourBit			= 30;							{ If toggling hour by char, accept hours 1..12 only }
	togCharZCycleBit			= 29;							{ Modifier for togChar12HourBit: accept hours 0..11 only }
	togDelta12HourBit			= 28;							{ If toggling hour up/down, restrict to 12-hour range (am/pm) }
	genCdevRangeBit				= 27;							{ Restrict date/time to range used by genl CDEV }
	validDateFields				= -1;
	maxDateField				= 10;

	eraMask						= $0001;
	yearMask					= $0002;
	monthMask					= $0004;
	dayMask						= $0008;
	hourMask					= $0010;
	minuteMask					= $0020;
	secondMask					= $0040;
	dayOfWeekMask				= $0080;
	dayOfYearMask				= $0100;
	weekOfYearMask				= $0200;
	pmMask						= $0400;
	dateStdMask					= $007F;						{ default for ValidDate flags and ToggleDate TogglePB.togFlags }


type
	LongDateField 				= SInt8;
const
	eraField					= 0;
	yearField					= 1;
	monthField					= 2;
	dayField					= 3;
	hourField					= 4;
	minuteField					= 5;
	secondField					= 6;
	dayOfWeekField				= 7;
	dayOfYearField				= 8;
	weekOfYearField				= 9;
	pmField						= 10;
	res1Field					= 11;
	res2Field					= 12;
	res3Field					= 13;


type
	DateForm 					= SInt8;
const
	shortDate					= 0;
	longDate					= 1;
	abbrevDate					= 2;

																{  StringToDate status values  }
	fatalDateTime				= $8000;						{  StringToDate and String2Time mask to a fatal error  }
	longDateFound				= 1;							{  StringToDate mask to long date found  }
	leftOverChars				= 2;							{  StringToDate & Time mask to warn of left over characters  }
	sepNotIntlSep				= 4;							{  StringToDate & Time mask to warn of non-standard separators  }
	fieldOrderNotIntl			= 8;							{  StringToDate & Time mask to warn of non-standard field order  }
	extraneousStrings			= 16;							{  StringToDate & Time mask to warn of unparsable strings in text  }
	tooManySeps					= 32;							{  StringToDate & Time mask to warn of too many separators  }
	sepNotConsistent			= 64;							{  StringToDate & Time mask to warn of inconsistent separators  }
	tokenErr					= $8100;						{  StringToDate & Time mask for 'tokenizer err encountered'  }
	cantReadUtilities			= $8200;
	dateTimeNotFound			= $8400;
	dateTimeInvalid				= $8800;


type
	StringToDateStatus					= SInt16;
	String2DateStatus					= StringToDateStatus;
	DateCacheRecordPtr = ^DateCacheRecord;
	DateCacheRecord = packed record
		hidden:					array [0..255] of SInt16;				{  only for temporary use  }
	end;

	DateCachePtr						= ^DateCacheRecord;
	DateTimeRecPtr = ^DateTimeRec;
	DateTimeRec = record
		year:					SInt16;
		month:					SInt16;
		day:					SInt16;
		hour:					SInt16;
		minute:					SInt16;
		second:					SInt16;
		dayOfWeek:				SInt16;
	end;

	LongDateTime						= SInt64;
	LongDateTimePtr 					= ^LongDateTime;
{$ifc TARGET_RT_BIG_ENDIAN}
	LongDateCvt = record
		case SInt16 of
		0: (
			c:					SInt64;
			);
		1: (
			lHigh:				UInt32;
			lLow:				UInt32;
		   );
	end;
{$elsec}
	LongDateCvt = record
		case SInt16 of
		0: (
			c:					SInt64;
			);
		1: (
			lLow:				UInt32;
			lHigh:				UInt32;
		   );
	end;
{$endc}
	LongDateCvtPtr = ^LongDateCvt;

	LongDateRecPtr = ^LongDateRec;
	LongDateRec = record
		case SInt16 of
		0: (
			era:				SInt16;
			year:				SInt16;
			month:				SInt16;
			day:				SInt16;
			hour:				SInt16;
			minute:				SInt16;
			second:				SInt16;
			dayOfWeek:			SInt16;
			dayOfYear:			SInt16;
			weekOfYear:			SInt16;
			pm:					SInt16;
			res1:				SInt16;
			res2:				SInt16;
			res3:				SInt16;
		   );
		1: (
			list:				array [0..13] of SInt16;				{ Index by LongDateField! }
			);
		2: (
			eraAlt:				SInt16;
			oldDate:			DateTimeRec;
		   );
	end;

	DateDelta							= SInt8;
	TogglePBPtr = ^TogglePB;
	TogglePB = record
		togFlags:				SInt32;								{ caller normally sets low word to dateStdMask=$7F }
		amChars:				ResType;								{ from 'itl0', but uppercased }
		pmChars:				ResType;								{ from 'itl0', but uppercased }
		reserved:				array [0..3] of SInt32;
	end;

	{	
	    These routine are available in Carbon with their new name
		}
	{
	 *  DateString()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   not available
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
procedure DateString(dateTime: UInt32; longFlag: ByteParameter; var result: Str255; intlHandle: Handle); external name '_DateString';
{
 *  TimeString()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure TimeString(dateTime: UInt32; wantSeconds: boolean; var result: Str255; intlHandle: Handle); external name '_TimeString';
{
 *  LongDateString()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure LongDateString((*const*) var dateTime: LongDateTime; longFlag: ByteParameter; var result: Str255; intlHandle: Handle); external name '_LongDateString';
{
 *  LongTimeString()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure LongTimeString((*const*) var dateTime: LongDateTime; wantSeconds: boolean; var result: Str255; intlHandle: Handle); external name '_LongTimeString';
{
    These routine are available in Carbon and InterfaceLib with their new name
}
{
 *  InitDateCache()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function InitDateCache(theCache: DateCachePtr): OSErr; external name '_InitDateCache';
{
 *  StringToDate()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function StringToDate(textPtr: Ptr; textLen: SInt32; theCache: DateCachePtr; var lengthUsed: SInt32; var dateTime: LongDateRec): StringToDateStatus; external name '_StringToDate';
{
 *  StringToTime()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function StringToTime(textPtr: Ptr; textLen: SInt32; theCache: DateCachePtr; var lengthUsed: SInt32; var dateTime: LongDateRec): StringToDateStatus; external name '_StringToTime';
{
 *  LongDateToSeconds()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure LongDateToSeconds(const (*var*) lDate: LongDateRec; var lSecs: LongDateTime); external name '_LongDateToSeconds';
{
 *  LongSecondsToDate()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure LongSecondsToDate((*const*) var lSecs: LongDateTime; var lDate: LongDateRec); external name '_LongSecondsToDate';
{
 *  ToggleDate()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ToggleDate(var lSecs: LongDateTime; field: ByteParameter; delta: DateDelta; ch: SInt16; const (*var*) params: TogglePB): ToggleResults; external name '_ToggleDate';
{
 *  ValidDate()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ValidDate(const (*var*) vDate: LongDateRec; flags: SInt32; var newSecs: LongDateTime): SInt16; external name '_ValidDate';
{
 *  ReadDateTime()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ReadDateTime(var time: UInt32): OSErr; external name '_ReadDateTime';
{
 *  GetDateTime()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure GetDateTime(var secs: UInt32); external name '_GetDateTime';
{
 *  SetDateTime()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SetDateTime(time: UInt32): OSErr; external name '_SetDateTime';
{
 *  SetTime()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure SetTime(const (*var*) d: DateTimeRec); external name '_SetTime';
{
 *  GetTime()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure GetTime(var d: DateTimeRec); external name '_GetTime';
{
 *  DateToSeconds()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DateToSeconds(const (*var*) d: DateTimeRec; var secs: UInt32); external name '_DateToSeconds';

{
 *  SecondsToDate()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure SecondsToDate(secs: UInt32; var d: DateTimeRec); external name '_SecondsToDate';
{
    These routine are available in InterfaceLib using their old name.
    Macros allow using the new names in all source code.
}
{$ifc CALL_NOT_IN_CARBON}
{
 *  IUDateString()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure IUDateString(dateTime: SInt32; longFlag: ByteParameter; var result: Str255); external name '_IUDateString';
{
 *  IUTimeString()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure IUTimeString(dateTime: SInt32; wantSeconds: boolean; var result: Str255); external name '_IUTimeString';
{
 *  IUDatePString()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure IUDatePString(dateTime: SInt32; longFlag: ByteParameter; var result: Str255; intlHandle: Handle); external name '_IUDatePString';
{
 *  IUTimePString()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure IUTimePString(dateTime: SInt32; wantSeconds: boolean; var result: Str255; intlHandle: Handle); external name '_IUTimePString';
{
 *  IULDateString()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure IULDateString(var dateTime: LongDateTime; longFlag: ByteParameter; var result: Str255; intlHandle: Handle); external name '_IULDateString';
{
 *  IULTimeString()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure IULTimeString(var dateTime: LongDateTime; wantSeconds: boolean; var result: Str255; intlHandle: Handle); external name '_IULTimeString';
{$endc}  {CALL_NOT_IN_CARBON}

{$ifc OLDROUTINENAMES}
{$ifc CALL_NOT_IN_CARBON}
{
 *  LongDate2Secs()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LongDate2Secs(const (*var*) lDate: LongDateRec; var lSecs: LongDateTime); external name '_LongDate2Secs';
{
 *  LongSecs2Date()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LongSecs2Date(var lSecs: LongDateTime; var lDate: LongDateRec); external name '_LongSecs2Date';
{
 *  Date2Secs()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure Date2Secs(const (*var*) d: DateTimeRec; var secs: UInt32); external name '_Date2Secs';

{
 *  Secs2Date()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure Secs2Date(secs: UInt32; var d: DateTimeRec); external name '_Secs2Date';
{$endc}  {CALL_NOT_IN_CARBON}
{$endc}  {OLDROUTINENAMES}


{$ALIGN MAC68K}


end.
