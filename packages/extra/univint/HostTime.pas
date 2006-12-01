{==================================================================================================
     File:       CoreAudio/HostTime.h

     Contains:   Routines for accessing the host's time base

     Version:    Technology: Mac OS X
                 Release:    Mac OS X

     Copyright:  (c) 1985-2005 by Apple Computer, Inc., all rights reserved.

==================================================================================================}

{	 Pascal Translation:  Gale R Paeper, <gpaeper@empirenet.com>, 2006 }

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

unit HostTime;
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
uses MacTypes,CoreAudioTypes;
{$ALIGN POWER}


//==================================================================================================
//#pragma mark    Theory of Operation

{!
    @header HostTime
    This collection of functions provides access to the host's time base. It also provides
    discriptive information about the time base and translations to and from nanoseconds.
}


{!
    @functiongroup  HostTime
}

{!
    @function       AudioGetCurrentHostTime
    @abstract       Gets the current host time.
    @result         A UInt64 containing the current host time.
}
function AudioGetCurrentHostTime: UInt64; external name '_AudioGetCurrentHostTime';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{!
    @function       AudioGetHostClockFrequency
    @abstract       Gets the number of ticks per second in the host time base.
    @result         A Float64 containing the number of ticks per second in the host time base.
}
function AudioGetHostClockFrequency: Float64; external name '_AudioGetHostClockFrequency';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{!
    @function       AudioGetHostClockMinimumTimeDelta
    @abstract       Gets the smallest number of ticks that two succeeding values will ever differ.
                    by.
    @result         A UInt32 containing the smallest number of ticks that two succeeding values will
                    ever differ.
}
function AudioGetHostClockMinimumTimeDelta: UInt32; external name '_AudioGetHostClockMinimumTimeDelta';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{!
    @function       AudioConvertHostTimeToNanos
    @abstract       Convert the given host time into a time in nanoseconds.
    @param          inHostTime
                        A UInt64 containing the host time to convert.
    @result         A UInt64 containining the converted host time.
}
function AudioConvertHostTimeToNanos( inHostTime: UInt64 ): UInt64; external name '_AudioConvertHostTimeToNanos';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{!
    @function       AudioConvertNanosToHostTime
    @abstract       Convert the given nanosecond time into a host time.
    @param          inNanos
                        A UInt64 containing the nanosecond time to convert.
    @result         A UInt64 containining the converted nanosecond time.
}
function AudioConvertNanosToHostTime( inNanos: UInt64 ): UInt64; external name '_AudioConvertNanosToHostTime';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


end.
