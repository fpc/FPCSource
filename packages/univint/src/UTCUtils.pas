{
     File:       UTCUtils.p
 
     Contains:   Interface for UTC to Local Time conversion and 64 Bit Clock routines
 
     Version:    Technology: Mac OS 9
                 Release:    Universal Interfaces 3.4.2
 
     Copyright:  © 1999-2002 by Apple Computer, Inc., all rights reserved.
 
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

unit UTCUtils;
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
uses MacTypes,MacErrors;


{$ALIGN MAC68K}

{ Options for Set & Get DateTime Routines }

const
	kUTCDefaultOptions			= 0;

	{	 64 Bit Clock Typedefs 	}

type
	UTCDateTimePtr = ^UTCDateTime;
	UTCDateTime = record
		highSeconds:			UInt16;
		lowSeconds:				UInt32;
		fraction:				UInt16;
	end;

	UTCDateTimeHandle					= ^UTCDateTimePtr;
	LocalDateTimePtr = ^LocalDateTime;
	LocalDateTime = record
		highSeconds:			UInt16;
		lowSeconds:				UInt32;
		fraction:				UInt16;
	end;

	LocalDateTimeHandle					= ^LocalDateTimePtr;
	{	 Classic 32 bit clock conversion routines 	}
	{
	 *  ConvertLocalTimeToUTC()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in UTCUtils 1.0 and later
	 *    CarbonLib:        in CarbonLib 1.0.2 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
function ConvertLocalTimeToUTC(localSeconds: UInt32; var utcSeconds: UInt32): OSStatus; external name '_ConvertLocalTimeToUTC';

{
 *  ConvertUTCToLocalTime()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in UTCUtils 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ConvertUTCToLocalTime(utcSeconds: UInt32; var localSeconds: UInt32): OSStatus; external name '_ConvertUTCToLocalTime';

{ 64 bit clock conversion routines }
{
 *  ConvertUTCToLocalDateTime()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in UTCUtils 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ConvertUTCToLocalDateTime(const (*var*) utcDateTime_: UTCDateTime; var localDateTime_: LocalDateTime): OSStatus; external name '_ConvertUTCToLocalDateTime';

{
 *  ConvertLocalToUTCDateTime()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in UTCUtils 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ConvertLocalToUTCDateTime(const (*var*) localDateTime_: LocalDateTime; var utcDateTime_: UTCDateTime): OSStatus; external name '_ConvertLocalToUTCDateTime';

{ Getter and Setter Clock routines using 64 Bit values }
{
 *  GetUTCDateTime()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in UTCUtils 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 }
function GetUTCDateTime(var utcDateTime_: UTCDateTime; options: OptionBits): OSStatus; external name '_GetUTCDateTime';

{
 *  SetUTCDateTime()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in UTCUtils 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SetUTCDateTime(const (*var*) utcDateTime_: UTCDateTime; options: OptionBits): OSStatus; external name '_SetUTCDateTime';

{
 *  GetLocalDateTime()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in UTCUtils 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 }
function GetLocalDateTime(var localDateTime_: LocalDateTime; options: OptionBits): OSStatus; external name '_GetLocalDateTime';

{
 *  SetLocalDateTime()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in UTCUtils 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SetLocalDateTime(const (*var*) localDateTime_: LocalDateTime; options: OptionBits): OSStatus; external name '_SetLocalDateTime';

{$ALIGN MAC68K}


end.
