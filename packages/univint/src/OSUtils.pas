{
     File:       CarbonCore/OSUtils.h
 
     Contains:   OS Utilities Interfaces.
                 The contents of this header file are deprecated.
 
     Copyright:  © 1985-2011 by Apple Inc. All rights reserved.
}
{      Pascal Translation Updated:  Peter N Lewis, <peter@stairways.com.au>, November 2005 }
{      Pascal Translation Updated:  Jonas Maebe, <jonas@freepascal.org>, October 2009 }
{      Pascal Translation Updated:  Gorazd Krosl, <gorazd_1957@yahoo.ca>, October 2009 }
{      Pascal Translation Updated:  Jonas Maebe, <jonas@freepascal.org>, September 2012 }
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

unit OSUtils;
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
	{ will require compiler define when/if other Apple devices with ARM cpus ship }
	{$setc TARGET_OS_MAC := FALSE}
	{$setc TARGET_OS_IPHONE := TRUE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
	{$setc TARGET_OS_EMBEDDED := TRUE}
{$elsec}
	{$error __ppc__ nor __ppc64__ nor __i386__ nor __x86_64__ nor __arm__ is defined.}
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
uses MacTypes,CFBase;
{$endc} {not MACOSALLINCLUDE}


{$ifc TARGET_OS_MAC}

{$ALIGN POWER}


{  HandToHand and other memory utilties were moved to MacMemory.h }

{  Date and Time utilties were moved to DateTimeUtils.h }


{$ALIGN MAC68K}

const
{ result types for RelString Call }
	sortsBefore = -1;   {first string < second string}
	sortsEqual = 0;    {first string = second string}
	sortsAfter = 1;     {first string > second string}

const
	dummyType = 0;
	vType = 1;
	ioQType = 2;
	drvQType = 3;
	evType = 4;
	fsQType = 5;
	sIQType = 6;
	dtQType = 7;
	nmType = 8;

type
	QTypes = SignedByte;
	QElemPtr = ^QElem;
	QElem = record
		qLink: QElemPtr;
		qType: SInt16;
		qData: array [0..0] of SInt16;
	end;
type
	QHdr = record
		qFlags: {volatile} SInt16;
		qHead: {volatile} QElemPtr;
		qTail: {volatile} QElemPtr;
	end;
	QHdrPtr = ^QHdr;
{ 
    In order for MachineLocation to be endian-safe, a new member 
    has been added to the 'u' union in the structure. You are 
    encouraged to use the new member instead of the old one.
    
    If your code looked like this:
    
        MachineLocation.u.dlsDelta = isDLS? 0x80: 0x00;
    
    you should change it to this:
    
        MachineLocation.u.dls.Delta = isDLS? 0x80: 0x00;
    
    to be endian safe. The gmtDelta remains the same; the low 24-bits
    are used. Remember that order of assignment DOES matter:
    
    This will overwrite results:
    
        MachineLocation.u.dls.Delta = 0xAA;         // u = 0xAAGGGGGG; G=Garbage
        MachineLocation.u.gmtDelta = 0xBBBBBB;      // u = 0x00BBBBBB;
    
    when in fact reversing the assignment would have preserved the values:

        MachineLocation.u.gmtDelta = 0xBBBBBB;      // u = 0x00BBBBBB;  
        MachineLocation.u.dls.Delta = 0xAA;         // u = 0xAABBBBBB;
        
    NOTE:   The information regarding dlsDelta in Inside Mac is INCORRECT.
            It's always 0x80 for daylight-saving time or 0x00 for standard time.
}
type
	MachineLocationPtr = ^MachineLocation;
	MachineLocation = packed record
		latitude: Fract;
		longitude: Fract;
		case SInt16 of
{$ifc TARGET_RT_BIG_ENDIAN}
		0: (
			dlsDelta: SInt8;									{ signed byte; daylight savings delta }
			);
{$endc}
		1: (
			gmtDelta: SIGNEDLONG;           { use low 24-bits only }
			);
		2: (
{$ifc TARGET_RT_LITTLE_ENDIAN}
			pad0,pad1,pad2: SInt8;
{$endc}
			Delta: SInt8;
			);
	end;
{
 *  IsMetric()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    Use CFLocaleGetValue() and the property kCFLocaleUsesMetricSystem
 *    to determine this value.
 *  
 *  Summary:
 *    Verifies whether the current script system is using the metric
 *    system or the English system of measurement.
 *  
 *  Discussion:
 *    The IsMetric function examines the metricSys field of the
 *    numeric-format resource (resource type 'itl0') to determine if
 *    the current script is using the metric system. A value of 255 in
 *    the metricSys field indicates that the metric system
 *    (centimeters, kilometers, milligrams, degrees Celsius, and so on)
 *    is being used. A value of 0 in the metricSys field indicates that
 *    the English system of measurement (inches, miles, ounces, degrees
 *    Fahrenheit, and so on) is used.
 *    If you want to use units of measurement different from that of
 *    the current script, you need to override the value of the
 *    metricSys field in the current numeric-format resource. You can
 *    do this by using your own version of the numeric-format resource
 *    instead of the current script system’s default international
 *    resource.
 *    The IsMetric function is the same as the IUMetric function, which
 *    was previously available with the International Utilities Package.
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Result:
 *    TRUE if the metric system is being used; FALSE if the English
 *    system is being used.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework but deprecated in 10.7
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function IsMetric: Boolean; external name '_IsMetric';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_7, __IPHONE_NA, __IPHONE_NA) *)


{
 *  Delay()
 *  
 *  Summary:
 *    Delays execture for the specified amount of time.
 *  
 *  Mac OS X threading:
 *    Thread safe
 *  
 *  Parameters:
 *    
 *    numTicks:
 *      the number of ticks to delay  for
 *    
 *    finalTicks:
 *      on return, if not NULL, will contain the value of TickCount()
 *      at the end of the delay period
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
procedure Delay( numTicks: UNSIGNEDLONG; var finalTicks: UNSIGNEDLONG ); external name '_Delay';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  Enqueue()
 *  
 *  Summary:
 *    Atomically adds a queue element to the given queue
 *  
 *  Discussion:
 *    A queue ( represented by a QHdrPtr ) is a singly linked list of
 *    elements.  Enqueue inserts the given element into the queue in a
 *    multi-thread safe way.  If the element is already in the queue,
 *    or in some other queue, the data structures will be corrupted and
 *    will likely cause a crash or infinite loop.
 *  
 *  Mac OS X threading:
 *    Thread safe
 *  
 *  Parameters:
 *    
 *    qElement:
 *      a pointer to the element to be inserted
 *    
 *    qHeader:
 *      a pointer to the queue header.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
procedure Enqueue( qElement: QElemPtr; qHeader: QHdrPtr ); external name '_Enqueue';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  Dequeue()
 *  
 *  Summary:
 *    Atomically removes a queue element from the given queue
 *  
 *  Discussion:
 *    A queue ( represented by a QHdrPtr ) is a singly linked list of
 *    elements.  Dequeue removes the given element from the queue in a
 *    multi-thread safe way.  If the element is not in the queue, qErr
 *    is returned.
 *  
 *  Mac OS X threading:
 *    Thread safe
 *  
 *  Parameters:
 *    
 *    qElement:
 *      a pointer to the element to be removed
 *    
 *    qHeader:
 *      a pointer to the queue header.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function Dequeue( qElement: QElemPtr; qHeader: QHdrPtr ): OSErr; external name '_Dequeue';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{$ifc not TARGET_CPU_64}
{
 *  MakeDataExecutable()
 *  
 *  Summary:
 *    Notifies the system that the specified data is subject to
 *    execution.
 *  
 *  Discussion:
 *    On some computer architectures it is necessary to tell the
 *    processor that an area of memory should be made executable.  This
 *    function does the necessary operations ( if possible ) to make it
 *    possible to execute code in the given address range.
 *    MakeDataExecutable is not supported for 64-bit applications. Use
 *    sys_icache_invalidate(3) and/or mprotect(2) as appropriate.
 *  
 *  Mac OS X threading:
 *    Thread safe
 *  
 *  Parameters:
 *    
 *    baseAddress:
 *      the starting address to be made executable
 *    
 *    length:
 *      the length of the data pointed to by the baseAddress parameter.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only]
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.5 and later
 }
procedure MakeDataExecutable( baseAddress: UnivPtr; length: UNSIGNEDLONG ); external name '_MakeDataExecutable';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{$endc} {not TARGET_CPU_64}

{
 *  ReadLocation()
 *  
 *  Summary:
 *    Obtains information about a geographic location or time zone.
 *  
 *  Discussion:
 *    The latitude and longitude are stored as Fract values, giving
 *    accuracy to within one foot. For example, a Fract value of 1.0
 *    equals 90 degrees –1.0 equals –90 degrees and –2.0 equals –180
 *    degrees.
 *    To convert these values to a degrees format, you need to convert
 *    the Fract values first to the Fixed data type, then to the
 *    LongInt data type. Use the Mathematical and Logical Utilities
 *    functions Fract2Fix and Fix2Long to accomplish this task.
 *    The DST value is a signed byte value that specifies the offset
 *    for the hour field—whether to add one hour, subtract one hour, or
 *    make no change at all.
 *    The GMT value is in seconds east of GMT. For example, San
 *    Francisco is at –28,800 seconds (8 hours * 3,600 seconds per
 *    hour) east of GMT. The gmtDelta field is a 3-byte value contained
 *    in a long word, so you must take care to get it properly.
 *    For more information on the Fract data type and the conversion
 *    routines Long2Fix, Fix2Fract, Fract2Fix, and Fix2Long, see
 *    Mathematical and Logical Utilities.
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Parameters:
 *    
 *    loc:
 *      On return, the fields of the geographic location structure
 *      containing the geographic location and the time-zone
 *      information.
 *      You can get values for the latitude, longitude, daylight
 *      savings time (DST), or Greenwich mean time (GMT). If the
 *      geographic location record has never been set, all fields
 *      contain 0.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
procedure ReadLocation( var loc: MachineLocation ); external name '_ReadLocation';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  TickCount()
 *  
 *  Summary:
 *    Obtains the current number of ticks (a tick is approximately 1/60
 *    of a second) approximately since the system last started up.
 *  
 *  Discussion:
 *    The TickCount function returns an unsigned 32-bit integer that
 *    indicates the current number of ticks since the system last
 *    started up. You can use this value to compare the number of ticks
 *    that have elapsed since a given event or other action occurred.
 *    For example, you could compare the current value returned by
 *    TickCount with the value of the when field of an event
 *    structure.
 *    The tick count rolls over in approximately 2 years 3 months,
 *    which means you should not use this to time intervals which may
 *    exceed ( or even approach ) this interval.
 *    Do not rely on the tick count being exact; it is usually accurate
 *    to within one tick, but this level of accuracy is not guaranteed.
 *  
 *  Mac OS X threading:
 *    Thread safe
 *  
 *  Result:
 *    the tick count
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function TickCount: UInt32; external name '_TickCount';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  CSCopyUserName()
 *  
 *  Summary:
 *    Returns a reference to the CFString that represents the user name.
 *  
 *  Mac OS X threading:
 *    Thread safe
 *  
 *  Parameters:
 *    
 *    useShortName:
 *      A Boolean value that specifies whether to return the short name
 *      or full name of the user.
 *  
 *  Result:
 *    the requested name in a CFStringRef.  You should release this
 *    when you are done with it.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.5 and later
 *    Non-Carbon CFM:   not available
 }
function CSCopyUserName( useShortName: Boolean ): CFStringRef; external name '_CSCopyUserName';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  CSCopyMachineName()
 *  
 *  Summary:
 *    Returns a reference to the CFString that represents the computer
 *    name.
 *  
 *  Mac OS X threading:
 *    Thread safe
 *  
 *  Result:
 *    the name of this machine in a CFStringRef.  You should release
 *    this when you are done with it.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.5 and later
 *    Non-Carbon CFM:   not available
 }
function CSCopyMachineName: CFStringRef; external name '_CSCopyMachineName';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


const
	useFree = 0;
	useATalk = 1;
	useAsync = 2;
	useExtClk = 3;    {Externally clocked}
	useMIDI = 4;

const
	false32b = 0;    {24 bit addressing error}
	true32b = 1;     {32 bit addressing error}

type
	SysPPtr = UnivPtr;

function GetMMUMode: SInt8; inline;
procedure SwapMMUMode( var mode: SInt8 ); inline;

{$ifc not TARGET_CPU_64}
{
 *  GetSysPPtr()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    Don't use this function; it always returns NULL on Mac OS X.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function GetSysPPtr: SysPPtr; external name '_GetSysPPtr';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{
    NOTE: SysBeep() has been moved to Sound.h.  
 We could not automatically #include Sound.h in this file
 because Sound.h indirectly #include's OSUtils.h which
 would make a circular include.
 }
{$endc} {not TARGET_CPU_64}

type
	DeferredTaskProcPtr = procedure( dtParam: SIGNEDLONG );
	DeferredTaskUPP = DeferredTaskProcPtr;
{
 *  NewDeferredTaskUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function NewDeferredTaskUPP( userRoutine: DeferredTaskProcPtr ): DeferredTaskUPP; external name '_NewDeferredTaskUPP';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)

{
 *  DisposeDeferredTaskUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure DisposeDeferredTaskUPP( userUPP: DeferredTaskUPP ); external name '_DisposeDeferredTaskUPP';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)

{
 *  InvokeDeferredTaskUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure InvokeDeferredTaskUPP( dtParam: SIGNEDLONG; userUPP: DeferredTaskUPP ); external name '_InvokeDeferredTaskUPP';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)

type
	DeferredTask = record
		qLink: {volatile} QElemPtr;
		qType: SInt16;
		dtFlags: {volatile} SInt16;
		dtAddr: DeferredTaskUPP;
		dtParam: SIGNEDLONG;
		dtReserved: SIGNEDLONG;
	end;
	DeferredTaskPtr = ^DeferredTask;
{$ifc not TARGET_CPU_64}
{
 *  DTInstall()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    The Deferred Task Manager is deprecated.  Look into restructuring
 *    your code to use threads, or MPTasks, or some other threading
 *    solution.
 *  
 *  Summary:
 *    Adds the specified task record to the deferred-task queue.
 *  
 *  Mac OS X threading:
 *    Thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function DTInstall( dtTaskPtr: DeferredTaskPtr ): OSErr; external name '_DTInstall';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{
 *  DTUninstall()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    The Deferred Task Manager is deprecated.  Look into restructuring
 *    your code to use threads, or MPTasks, or some other threading
 *    solution.
 *  
 *  Summary:
 *    Adds the specified task record to the deferred-task queue.
 *  
 *  Mac OS X threading:
 *    Thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function DTUninstall( dtTaskPtr: DeferredTaskPtr ): OSErr; external name '_DTUninstall';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{
 *  SetCurrentA5()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    You no longer need to use SetCurrentA5() on Mac OS X.
 *  
 *  Mac OS X threading:
 *    Thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function SetCurrentA5: SIGNEDLONG; external name '_SetCurrentA5';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{
 *  SetA5()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    You no longer need to use SetA5() on Mac OS X.
 *  
 *  Mac OS X threading:
 *    Thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function SetA5( newA5: SIGNEDLONG ): SIGNEDLONG; external name '_SetA5';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{
 *  InitUtil()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    It is not necessary to call InitUtil on Mac OS X.  You should
 *    remove all calls to this from your code.
 *  
 *  Mac OS X threading:
 *    Thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.3
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function InitUtil: OSErr; external name '_InitUtil';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_3, __IPHONE_NA, __IPHONE_NA) *)


{
 *  WriteParam()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    This function no longer does anything on Mac OS X; you should
 *    remove all calls to it from your code.
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function WriteParam: OSErr; external name '_WriteParam';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{
 *  WriteLocation()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    WriteLocation can not be used to set the geographic information
 *    on Mac OS X.  You should remove all calls to this function from
 *    your code.
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.0
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
procedure WriteLocation( const (*var*) loc: MachineLocation ); external name '_WriteLocation';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_1, __IPHONE_NA, __IPHONE_NA) *)


{$endc} {not TARGET_CPU_64}
{$endc} {TARGET_OS_MAC}

{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}
implementation

{$ifc TARGET_OS_MAC}

{$R-}

function GetMMUMode: SInt8; inline;
begin
	GetMMUMode:= true32b
end;

procedure SwapMMUMode( var mode: SInt8 ); inline;
begin
	mode := true32b;
end;


{$endc} {TARGET_OS_MAC}

end.

{$endc} {not MACOSALLINCLUDE}
