{
     File:       CarbonCore/Multiprocessing.h
 
     Contains:   Multiprocessing interfaces
 
     Version:    CarbonCore-859.2~1
 
     Copyright:  © 1995-2008 DayStar Digital, Inc.
 
     Bugs?:      For bug reports, consult the following page on
                 the World Wide Web:
 
                     http://www.freepascal.org/bugs.html
 
}
{       Pascal Translation Updated:  Jonas Maebe, <jonas@freepascal.org>, October 2009 }
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

unit Multiprocessing;
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
{$elifc defined __ppc64__ and __ppc64__}
	{$setc TARGET_CPU_PPC := TFALSE}
	{$setc TARGET_CPU_PPC64 := TRUE}
	{$setc TARGET_CPU_X86 := FALSE}
	{$setc TARGET_CPU_X86_64 := FALSE}
	{$setc TARGET_CPU_ARM := FALSE}
	{$setc TARGET_OS_MAC := TRUE}
	{$setc TARGET_OS_IPHONE := FALSE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
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
{$elifc defined __x86_64__ and __x86_64__}
	{$setc TARGET_CPU_PPC := FALSE}
	{$setc TARGET_CPU_PPC64 := FALSE}
	{$setc TARGET_CPU_X86 := FALSE}
	{$setc TARGET_CPU_X86_64 := TRUE}
	{$setc TARGET_CPU_ARM := FALSE}
	{$setc TARGET_OS_MAC := TRUE}
	{$setc TARGET_OS_IPHONE := FALSE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
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
uses MacTypes;
{$endc} {not MACOSALLINCLUDE}


{$ifc TARGET_OS_MAC}

{
   ===========================================================================================
   *** WARNING: You must properly check the availability of MP services before calling them!
   See the section titled "Checking API Availability".
   ===========================================================================================
}


{$ALIGN POWER}


{
   ======================================= NOTICE ============================================
   As of Mac OS X v10.6, the APIs in this header file are discouraged. These APIs are slated
   for deprecation in the next major release of OS X. The new dispatch APIs (see dispatch(3))
   replace the Multiprocessing APIs and the pthread threading APIs.
   ===========================================================================================
}

{
   ===========================================================================================
   This is the header file for version 2.2 of the Mac OS multiprocessing support.  This version
   has been totally reimplemented and has significant new services.  The main goal of the
   reimplementation has been to transfer task management into the core operating system to provide
   much more reliable and more efficient operation, including on single processor machines.
   The memory management has also been massively improved, it is much faster and wastes much
   less space.  New services include POSIX style per-task storage, timers with millisecond and
   microsecond resolutions, memory allocation at a specified alignment, and system pageable
   and RAM resident memory pools.  See the MP API documentation for details.
   The old "DayStar" debugging services (whose names began with an underscore) have been
   removed from this header.  A very few are still implemented for binary compatibility, or in
   cases where they happened to be exposed inappropriately.  (E.g. _MPIsFullyInitialized must
   be called to see if the MP API is ReallyTruly™ usable.)  New code and recompiles of old
   code should avoid use of these defunct services, except for _MPIsFullyInitialized.
   ===========================================================================================
}


{
   ===========================================================================================
   The following services are from the original MP API and remain supported in version 2.0:
    MPProcessors
    MPCreateTask
    MPTerminateTask
    MPCurrentTaskID
    MPYield
    MPExit
    MPCreateQueue
    MPDeleteQueue
    MPNotifyQueue
    MPWaitOnQueue
    MPCreateSemaphore
    MPCreateBinarySemaphore     (In C only, a macro that calls MPCreateSemaphore.)
    MPDeleteSemaphore
    MPSignalSemaphore
    MPWaitOnSemaphore
    MPCreateCriticalRegion
    MPDeleteCriticalRegion
    MPEnterCriticalRegion
    MPExitCriticalRegion
    MPAllocate                  (Deprecated, use MPAllocateAligned for new builds.)
    MPFree
    MPBlockCopy
    MPLibraryIsLoaded           (In C only, a macro.)
    _MPIsFullyInitialized       (See comments about checking for MP API availability.)
   ===========================================================================================
}


{
   ===========================================================================================
   The following services are new in version 2.0:
    MPProcessorsScheduled
    MPSetTaskWeight
    MPTaskIsPreemptive
    MPAllocateTaskStorageIndex
    MPDeallocateTaskStorageIndex
    MPSetTaskStorageValue
    MPGetTaskStorageValue
    MPSetQueueReserve
    MPCreateEvent
    MPDeleteEvent
    MPSetEvent
    MPWaitForEvent
    UpTime
    DurationToAbsolute
    AbsoluteToDuration
    MPDelayUntil
    MPCreateTimer
    MPDeleteTimer
    MPSetTimerNotify
    MPArmTimer
    MPCancelTimer
    MPSetExceptionHandler
    MPThrowException
    MPDisposeTaskException
    MPExtractTaskState
    MPSetTaskState
    MPRegisterDebugger
    MPUnregisterDebugger
    MPAllocateAligned           (Preferred over MPAllocate.)
    MPGetAllocatedBlockSize
    MPBlockClear
    MPDataToCode
    MPRemoteCall                (Preferred over _MPRPC.)
   ===========================================================================================
}


{
   ===========================================================================================
   The following services are new in version 2.1:
    MPCreateNotification
    MPDeleteNotification
    MPModifyNotification
    MPCauseNotification
    MPGetNextTaskID
    MPGetNextCpuID
   ===========================================================================================
}


{
   ===========================================================================================
   The following services are "unofficial" extensions to the original API.  They are not in
   the multiprocessing API documentation, but were in previous versions of this header.  They
   remain supported in version 2.0.  They may not be supported in other environments.
    _MPRPC                      (Deprecated, use MPRemoteCall for new builds.)
    _MPAllocateSys              (Deprecated, use MPAllocateAligned for new builds.)
    _MPTaskIsToolboxSafe
    _MPLibraryVersion
    _MPLibraryIsCompatible
   ===========================================================================================
}


{
   ===========================================================================================
   The following services were in previous versions of this header for "debugging only" use.
   They are NOT implemented in version 2.0.  For old builds they can be accessed by defining
   the symbol MPIncludeDefunctServices to have a nonzero value.
    _MPInitializePrintf
    _MPPrintf
    _MPDebugStr
    _MPStatusPString
    _MPStatusCString
   ===========================================================================================
}


{
   §
   ===========================================================================================
   General Types and Constants
   ===========================
}

const
  MPCopyrightNotice = 'Copyright © 1995-2000 Apple Computer, Inc.';
const
	MPLibraryName = 'MPLibrary';
	MPLibraryCName = MPLibraryName;
	MPLibraryPName = MPLibraryName;
const
	MP_API_Version = '2.3';

const
	MPLibrary_MajorVersion = 2;    { ! When these change be sure to update the build versions}
	MPLibrary_MinorVersion = 3;    { !  used in the startup check in MPInitializeAPI!}
	MPLibrary_Release = 1;
	MPLibrary_DevelopmentRevision = 1;


type
	MPProcessID = ^SInt32; { an opaque type }
	MPProcessIDPtr = ^MPProcessID;
	MPTaskID = ^SInt32; { an opaque type }
	MPTaskIDPtr = ^MPTaskID;
	MPQueueID = ^SInt32; { an opaque type }
	MPQueueIDPtr = ^MPQueueID;
	MPSemaphoreID = ^SInt32; { an opaque type }
	MPSemaphoreIDPtr = ^MPSemaphoreID;
	MPCriticalRegionID = ^SInt32; { an opaque type }
	MPCriticalRegionIDPtr = ^MPCriticalRegionID;
	MPTimerID = ^SInt32; { an opaque type }
	MPTimerIDPtr = ^MPTimerID;
	MPEventID = ^SInt32; { an opaque type }
	MPEventIDPtr = ^MPEventID;
	MPAddressSpaceID = ^SInt32; { an opaque type }
	MPAddressSpaceIDPtr = ^MPAddressSpaceID;
	MPNotificationID = ^SInt32; { an opaque type }
	MPNotificationIDPtr = ^MPNotificationID;
	MPCoherenceID = ^SInt32; { an opaque type }
	MPCoherenceIDPtr = ^MPCoherenceID;
	MPCpuID = ^SInt32; { an opaque type }
	MPCpuIDPtr = ^MPCpuID;
	MPAreaID = ^SInt32; { an opaque type }
	MPAreaIDPtr = ^MPAreaID;
	MPConsoleID = ^SInt32; { an opaque type }
	MPConsoleIDPtr = ^MPConsoleID;
	MPOpaqueID = ^SInt32; { an opaque type }
	MPOpaqueIDPtr = ^MPOpaqueID;
const
{ Values for MPOpaqueIDClass.}
	kOpaqueAnyID = 0;
	kOpaqueProcessID = 1;
	kOpaqueTaskID = 2;
	kOpaqueTimerID = 3;
	kOpaqueQueueID = 4;
	kOpaqueSemaphoreID = 5;
	kOpaqueCriticalRegionID = 6;
	kOpaqueCpuID = 7;
	kOpaqueAddressSpaceID = 8;
	kOpaqueEventID = 9;
	kOpaqueCoherenceID = 10;
	kOpaqueAreaID = 11;
	kOpaqueNotificationID = 12;
	kOpaqueConsoleID = 13;

type
	MPOpaqueIDClass = UInt32;

const
	kMPNoID = kInvalidID; { New code should use kInvalidID everywhere.}


type
	MPTaskOptions = OptionBits;
	MPTaskOptionsPtr = ^MPTaskOptions;
	TaskStorageIndex = ItemCount;
	TaskStorageIndexPtr = ^TaskStorageIndex;
	TaskStorageValue = LogicalAddress;
	TaskStorageValuePtr = ^TaskStorageValue;
	MPSemaphoreCount = ItemCount;
	MPSemaphoreCountPtr = ^MPSemaphoreCount;
	MPTaskWeight = UInt32;
	MPTaskWeightPtr = ^MPTaskWeight;
	MPEventFlags = UInt32;
	MPEventFlagsPtr = ^MPEventFlags;
	MPExceptionKind = UInt32;
	MPExceptionKindPtr = ^MPExceptionKind;
	MPTaskStateKind = UInt32;
	MPTaskStateKindPtr = ^MPTaskStateKind;
	MPPageSizeClass = UInt32;
	MPPageSizeClassPtr = ^MPPageSizeClass;

const
	kDurationImmediate = 0;
	kDurationForever = $7FFFFFFF;
	kDurationMillisecond = 1;
	kDurationMicrosecond = -1;


{
   §
   ===========================================================================================
   Process/Processor Services
   ==========================
}


{
 *  MPProcessors()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in MPLibrary 1.0 and later
 }
function MPProcessors: ItemCount; external name '_MPProcessors';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{ The physical total.}

{
 *  MPProcessorsScheduled()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in MPLibrary 2.0 and later
 }
function MPProcessorsScheduled: ItemCount; external name '_MPProcessorsScheduled';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{ Those currently in use.}

{
   §
   ===========================================================================================
   Tasking Services
   ================
}


const
{ For MPCreateTask options}
	kMPCreateTaskSuspendedMask = 1 shl 0; {    not supported on Mac OS X}
	kMPCreateTaskTakesAllExceptionsMask = 1 shl 1;
	kMPCreateTaskNotDebuggableMask = 1 shl 2;
	kMPCreateTaskValidOptionsMask = kMPCreateTaskSuspendedMask or kMPCreateTaskTakesAllExceptionsMask or kMPCreateTaskNotDebuggableMask;


{ -------------------------------------------------------------------------------------------}


type
	TaskProc = function( parameter: UnivPtr ): OSStatus;

{
 *  MPCreateTask()
 *  
 *  Mac OS X threading:
 *    Thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in MPLibrary 1.0 and later
 }
function MPCreateTask( entryPoint: TaskProc; parameter: UnivPtr; stackSize: ByteCount; notifyQueue: MPQueueID; terminationParameter1: UnivPtr; terminationParameter2: UnivPtr; options: MPTaskOptions; task: MPTaskIDPtr ): OSStatus; external name '_MPCreateTask';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  MPTerminateTask()
 *  
 *  Mac OS X threading:
 *    Thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in MPLibrary 1.0 and later
 }
function MPTerminateTask( task: MPTaskID; terminationStatus: OSStatus ): OSStatus; external name '_MPTerminateTask';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  MPSetTaskWeight()
 *  
 *  Mac OS X threading:
 *    Thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in MPLibrary 2.0 and later
 }
function MPSetTaskWeight( task: MPTaskID; weight: MPTaskWeight ): OSStatus; external name '_MPSetTaskWeight';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  MPTaskIsPreemptive()
 *  
 *  Mac OS X threading:
 *    Thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in MPLibrary 2.0 and later
 }
function MPTaskIsPreemptive( taskID: MPTaskID ): Boolean; external name '_MPTaskIsPreemptive';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{ May be kInvalidID.}

{
 *  MPExit()
 *  
 *  Mac OS X threading:
 *    Thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in MPLibrary 1.0 and later
 }
procedure MPExit( status: OSStatus ); external name '_MPExit';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  MPYield()
 *  
 *  Mac OS X threading:
 *    Thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in MPLibrary 1.0 and later
 }
procedure MPYield; external name '_MPYield';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  MPCurrentTaskID()
 *  
 *  Mac OS X threading:
 *    Thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in MPLibrary 1.0 and later
 }
function MPCurrentTaskID: MPTaskID; external name '_MPCurrentTaskID';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  MPSetTaskType()
 *  
 *  Mac OS X threading:
 *    Thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.1 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in MPLibrary 2.3 and later
 }
function MPSetTaskType( task: MPTaskID; taskType: OSType ): OSStatus; external name '_MPSetTaskType';
(* AVAILABLE_MAC_OS_X_VERSION_10_1_AND_LATER *)


{ -------------------------------------------------------------------------------------------}


{
   ---------------------------------------------------
   ! The task storage services are new in version 2.0.
}


{
 *  MPAllocateTaskStorageIndex()
 *  
 *  Mac OS X threading:
 *    Thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in MPLibrary 2.0 and later
 }
function MPAllocateTaskStorageIndex( var taskIndex: TaskStorageIndex ): OSStatus; external name '_MPAllocateTaskStorageIndex';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  MPDeallocateTaskStorageIndex()
 *  
 *  Mac OS X threading:
 *    Thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in MPLibrary 2.0 and later
 }
function MPDeallocateTaskStorageIndex( taskIndex: TaskStorageIndex ): OSStatus; external name '_MPDeallocateTaskStorageIndex';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  MPSetTaskStorageValue()
 *  
 *  Mac OS X threading:
 *    Thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in MPLibrary 2.0 and later
 }
function MPSetTaskStorageValue( taskIndex: TaskStorageIndex; value: TaskStorageValue ): OSStatus; external name '_MPSetTaskStorageValue';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  MPGetTaskStorageValue()
 *  
 *  Mac OS X threading:
 *    Thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in MPLibrary 2.0 and later
 }
function MPGetTaskStorageValue( taskIndex: TaskStorageIndex ): TaskStorageValue; external name '_MPGetTaskStorageValue';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
   §
   ===========================================================================================
   Synchronization Services
   ========================
}


{
 *  MPCreateQueue()
 *  
 *  Mac OS X threading:
 *    Thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in MPLibrary 1.0 and later
 }
function MPCreateQueue( var queue: MPQueueID ): OSStatus; external name '_MPCreateQueue';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  MPDeleteQueue()
 *  
 *  Mac OS X threading:
 *    Thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in MPLibrary 1.0 and later
 }
function MPDeleteQueue( queue: MPQueueID ): OSStatus; external name '_MPDeleteQueue';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  MPNotifyQueue()
 *  
 *  Mac OS X threading:
 *    Thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in MPLibrary 1.0 and later
 }
function MPNotifyQueue( queue: MPQueueID; param1: UnivPtr; param2: UnivPtr; param3: UnivPtr ): OSStatus; external name '_MPNotifyQueue';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  MPWaitOnQueue()
 *  
 *  Mac OS X threading:
 *    Thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in MPLibrary 1.0 and later
 }
function MPWaitOnQueue( queue: MPQueueID; var param1: UnivPtr; var param2: UnivPtr; var param3: UnivPtr; timeout: Duration ): OSStatus; external name '_MPWaitOnQueue';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  MPSetQueueReserve()
 *  
 *  Mac OS X threading:
 *    Thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in MPLibrary 2.0 and later
 }
function MPSetQueueReserve( queue: MPQueueID; count: ItemCount ): OSStatus; external name '_MPSetQueueReserve';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{ -------------------------------------------------------------------------------------------}


{
 *  MPCreateSemaphore()
 *  
 *  Mac OS X threading:
 *    Thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in MPLibrary 1.0 and later
 }
function MPCreateSemaphore( maximumValue: MPSemaphoreCount; initialValue: MPSemaphoreCount; var semaphore: MPSemaphoreID ): OSStatus; external name '_MPCreateSemaphore';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  MPDeleteSemaphore()
 *  
 *  Mac OS X threading:
 *    Thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in MPLibrary 1.0 and later
 }
function MPDeleteSemaphore( semaphore: MPSemaphoreID ): OSStatus; external name '_MPDeleteSemaphore';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  MPSignalSemaphore()
 *  
 *  Mac OS X threading:
 *    Thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in MPLibrary 1.0 and later
 }
function MPSignalSemaphore( semaphore: MPSemaphoreID ): OSStatus; external name '_MPSignalSemaphore';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  MPWaitOnSemaphore()
 *  
 *  Mac OS X threading:
 *    Thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in MPLibrary 1.0 and later
 }
function MPWaitOnSemaphore( semaphore: MPSemaphoreID; timeout: Duration ): OSStatus; external name '_MPWaitOnSemaphore';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{ -------------------------------------------------------------------------------------------}


{
 *  MPCreateCriticalRegion()
 *  
 *  Mac OS X threading:
 *    Thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in MPLibrary 1.0 and later
 }
function MPCreateCriticalRegion( var criticalRegion: MPCriticalRegionID ): OSStatus; external name '_MPCreateCriticalRegion';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  MPDeleteCriticalRegion()
 *  
 *  Mac OS X threading:
 *    Thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in MPLibrary 1.0 and later
 }
function MPDeleteCriticalRegion( criticalRegion: MPCriticalRegionID ): OSStatus; external name '_MPDeleteCriticalRegion';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  MPEnterCriticalRegion()
 *  
 *  Mac OS X threading:
 *    Thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in MPLibrary 1.0 and later
 }
function MPEnterCriticalRegion( criticalRegion: MPCriticalRegionID; timeout: Duration ): OSStatus; external name '_MPEnterCriticalRegion';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  MPExitCriticalRegion()
 *  
 *  Mac OS X threading:
 *    Thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in MPLibrary 1.0 and later
 }
function MPExitCriticalRegion( criticalRegion: MPCriticalRegionID ): OSStatus; external name '_MPExitCriticalRegion';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{ -------------------------------------------------------------------------------------------}


{
 *  MPCreateEvent()
 *  
 *  Mac OS X threading:
 *    Thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in MPLibrary 2.0 and later
 }
function MPCreateEvent( var event: MPEventID ): OSStatus; external name '_MPCreateEvent';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  MPDeleteEvent()
 *  
 *  Mac OS X threading:
 *    Thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in MPLibrary 2.0 and later
 }
function MPDeleteEvent( event: MPEventID ): OSStatus; external name '_MPDeleteEvent';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  MPSetEvent()
 *  
 *  Mac OS X threading:
 *    Thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in MPLibrary 2.0 and later
 }
function MPSetEvent( event: MPEventID; flags: MPEventFlags ): OSStatus; external name '_MPSetEvent';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  MPWaitForEvent()
 *  
 *  Mac OS X threading:
 *    Thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in MPLibrary 2.0 and later
 }
function MPWaitForEvent( event: MPEventID; flags: MPEventFlagsPtr; timeout: Duration ): OSStatus; external name '_MPWaitForEvent';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
   §
   ===========================================================================================
   Notification Services (API)
   =====================
}


{
 *  MPCreateNotification()
 *  
 *  Mac OS X threading:
 *    Thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in MPLibrary 2.1 and later
 }
function MPCreateNotification( var notificationID: MPNotificationID ): OSStatus; external name '_MPCreateNotification';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  MPDeleteNotification()
 *  
 *  Mac OS X threading:
 *    Thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in MPLibrary 2.1 and later
 }
function MPDeleteNotification( notificationID: MPNotificationID ): OSStatus; external name '_MPDeleteNotification';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  MPModifyNotification()
 *  
 *  Mac OS X threading:
 *    Thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in MPLibrary 2.1 and later
 }
function MPModifyNotification( notificationID: MPNotificationID; anID: MPOpaqueID; notifyParam1: UnivPtr; notifyParam2: UnivPtr; notifyParam3: UnivPtr ): OSStatus; external name '_MPModifyNotification';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  MPModifyNotificationParameters()
 *  
 *  Mac OS X threading:
 *    Thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.1 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in MPLibrary 2.3 and later
 }
function MPModifyNotificationParameters( notificationID: MPNotificationID; kind: MPOpaqueIDClass; notifyParam1: UnivPtr; notifyParam2: UnivPtr; notifyParam3: UnivPtr ): OSStatus; external name '_MPModifyNotificationParameters';
(* AVAILABLE_MAC_OS_X_VERSION_10_1_AND_LATER *)


{
 *  MPCauseNotification()
 *  
 *  Mac OS X threading:
 *    Thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in MPLibrary 2.1 and later
 }
function MPCauseNotification( notificationID: MPNotificationID ): OSStatus; external name '_MPCauseNotification';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
   §
   ===========================================================================================
   Timer Services
   ==============
}


{
   --------------------------------------------
   ! The timer services are new in version 2.0.
}


const
{ For MPArmTimer options}
	kMPPreserveTimerIDMask = 1 shl 0;
	kMPTimeIsDeltaMask = 1 shl 1;
	kMPTimeIsDurationMask = 1 shl 2;


{
 *  MPDelayUntil()
 *  
 *  Mac OS X threading:
 *    Thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in MPLibrary 2.0 and later
 }
function MPDelayUntil( var expirationTime: AbsoluteTime ): OSStatus; external name '_MPDelayUntil';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  MPDelayUntilSys()
 *  
 *  Availability:
 *    Mac OS X:         not available
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in MPLibrary 2.1 and later
 }


{
 *  MPCreateTimer()
 *  
 *  Mac OS X threading:
 *    Thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in MPLibrary 2.0 and later
 }
function MPCreateTimer( var timerID: MPTimerID ): OSStatus; external name '_MPCreateTimer';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  MPDeleteTimer()
 *  
 *  Mac OS X threading:
 *    Thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in MPLibrary 2.0 and later
 }
function MPDeleteTimer( timerID: MPTimerID ): OSStatus; external name '_MPDeleteTimer';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  MPSetTimerNotify()
 *  
 *  Mac OS X threading:
 *    Thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in MPLibrary 2.0 and later
 }
function MPSetTimerNotify( timerID: MPTimerID; anID: MPOpaqueID; notifyParam1: UnivPtr; notifyParam2: UnivPtr; notifyParam3: UnivPtr ): OSStatus; external name '_MPSetTimerNotify';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  MPArmTimer()
 *  
 *  Mac OS X threading:
 *    Thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in MPLibrary 2.0 and later
 }
function MPArmTimer( timerID: MPTimerID; var expirationTime: AbsoluteTime; options: OptionBits ): OSStatus; external name '_MPArmTimer';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  MPCancelTimer()
 *  
 *  Mac OS X threading:
 *    Thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in MPLibrary 2.0 and later
 }
function MPCancelTimer( timerID: MPTimerID; timeRemaining: AbsoluteTimePtr ): OSStatus; external name '_MPCancelTimer';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
   §
   ===========================================================================================
   Memory Services
   ===============
}


const
{ Maximum allocation request size is 1GB.}
	kMPMaxAllocSize = 1073741824;

const
{ Values for the alignment parameter to MPAllocateAligned.}
	kMPAllocateDefaultAligned = 0;
	kMPAllocate8ByteAligned = 3;
	kMPAllocate16ByteAligned = 4;
	kMPAllocate32ByteAligned = 5;
	kMPAllocate1024ByteAligned = 10;
	kMPAllocate4096ByteAligned = 12;
	kMPAllocateMaxAlignment = 16;   { Somewhat arbitrary limit on expectations.}
	kMPAllocateAltiVecAligned = kMPAllocate16ByteAligned; { The P.C. name.}
	kMPAllocateVMXAligned = kMPAllocateAltiVecAligned; { The older, common name.}
	kMPAllocateVMPageAligned = 254;  { Pseudo value, converted at runtime.}
	kMPAllocateInterlockAligned = 255;   { Pseudo value, converted at runtime.}


const
{ Values for the options parameter to MPAllocateAligned.}
	kMPAllocateClearMask = $0001; { Zero the allocated block.}
	kMPAllocateGloballyMask = $0002; { Allocate from the globally visible pool.}
	kMPAllocateResidentMask = $0004; { Allocate from the RAM-resident pool.}
	kMPAllocateNoGrowthMask = $0010; { Do not attempt to grow the pool.}
	kMPAllocateNoCreateMask = $0020; { Do not attempt to create the pool if it doesn't exist yet.}


{ -------------------------------------------------------------------------------------------}


{
 *  MPAllocateAligned()
 *  
 *  Mac OS X threading:
 *    Thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in MPLibrary 2.0 and later
 }
function MPAllocateAligned( size: ByteCount; alignment: UInt8; options: OptionBits ): LogicalAddress; external name '_MPAllocateAligned';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{ ! MPAllocateAligned is new in version 2.0.}

{
 *  MPAllocate()
 *  
 *  Mac OS X threading:
 *    Thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in MPLibrary 1.0 and later
 }
function MPAllocate( size: ByteCount ): LogicalAddress; external name '_MPAllocate';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{ Use MPAllocateAligned instead.}

{
 *  MPFree()
 *  
 *  Mac OS X threading:
 *    Thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in MPLibrary 1.0 and later
 }
procedure MPFree( objct: LogicalAddress ); external name '_MPFree';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  MPGetAllocatedBlockSize()
 *  
 *  Mac OS X threading:
 *    Thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in MPLibrary 2.0 and later
 }
function MPGetAllocatedBlockSize( objct: LogicalAddress ): ByteCount; external name '_MPGetAllocatedBlockSize';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{ -------------------------------------------------------------------------------------------}


{
 *  MPBlockCopy()
 *  
 *  Mac OS X threading:
 *    Thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in MPLibrary 1.0 and later
 }
procedure MPBlockCopy( source: LogicalAddress; destination: LogicalAddress; size: ByteCount ); external name '_MPBlockCopy';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  MPBlockClear()
 *  
 *  Mac OS X threading:
 *    Thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in MPLibrary 2.0 and later
 }
procedure MPBlockClear( address: LogicalAddress; size: ByteCount ); external name '_MPBlockClear';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{$ifc not TARGET_CPU_64}
{
 *  MPDataToCode()
 *  
 *  Mac OS X threading:
 *    Thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only]
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in MPLibrary 2.0 and later
 }
procedure MPDataToCode( address: LogicalAddress; size: ByteCount ); external name '_MPDataToCode';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{ NOTE:    MPDataToCode is not supported for 64-bit applications. Use mprotect(2) instead.}


{
   §
   ===========================================================================================
   Exception/Debugging Services
   ============================
}


{
   -------------------------------------------------------------------------------------------
   *** Important Note ***
   ----------------------
   
   The functions MPExtractTaskState and MPSetTaskState infer the size of the "info" buffer
   from the "kind" parameter.  A given value for MPTaskStateKind will always refer to a
   single specific physical buffer layout.  Should new register sets be added, or the size
   or number of any registers change, new values of MPTaskStateKind will be introduced to
   refer to the new buffer layouts.
   
   The following types for the buffers are in MachineExceptions. The correspondence between
   MPTaskStateKind values and MachineExceptions types is:
   
        kMPTaskStateRegisters               -> RegisterInformation
        kMPTaskStateFPU                     -> FPUInformation
        kMPTaskStateVectors                 -> VectorInformation
        kMPTaskStateMachine                 -> MachineInformation
        kMPTaskState32BitMemoryException    -> ExceptionInfo for old-style 32-bit memory exceptions
   
    For reference, on PowerPC the MachineExceptions types contain:
   
        RegisterInformation -> The GPRs, 32 values of 64 bits each.
        FPUInformation      -> The FPRs plus FPSCR, 32 values of 64 bits each, one value of
                                32 bits.
        VectorInformation   -> The AltiVec vector registers plus VSCR and VRSave, 32 values
                                of 128 bits each, one value of 128 bits, and one 32 bit value.
        MachineInformation  -> The CTR, LR, PC, each of 64 bits.  The CR, XER, MSR, MQ,
                                exception kind, and DSISR, each of 32 bits.  The 64 bit DAR.
        ExceptionInfo       -> Only memory exceptions are specified, 4 fields of 32 bits each.
                                Note that this type only covers memory exceptions on 32-bit CPUs!
   The following types are declared here:
        kMPTaskStateTaskInfo                -> MPTaskInfo
}


{$endc} {not TARGET_CPU_64}

const
{ Values for the TaskStateKind to MPExtractTaskState and MPSetTaskState.}
	kMPTaskStateRegisters = 0;    { The task general registers.}
	kMPTaskStateFPU = 1;    { The task floating point registers}
	kMPTaskStateVectors = 2;    { The task vector registers}
	kMPTaskStateMachine = 3;    { The task machine registers}
	kMPTaskState32BitMemoryException = 4; { The task memory exception information for 32-bit CPUs.}
	kMPTaskStateTaskInfo = 5;     { Static and dynamic information about the task.}


const
{ Option bits and numbers for MPDisposeTaskException.}
	kMPTaskPropagate = 0;    { The exception is propagated.}
	kMPTaskResumeStep = 1;    { The task is resumed and single step is enabled.}
	kMPTaskResumeBranch = 2;    { The task is resumed and branch stepping is enabled.}
	kMPTaskResumeMask = $0000; { The task is resumed.}
	kMPTaskPropagateMask = 1 shl kMPTaskPropagate; { The exception is propagated.}
	kMPTaskResumeStepMask = 1 shl kMPTaskResumeStep; { The task is resumed and single step is enabled.}
	kMPTaskResumeBranchMask = 1 shl kMPTaskResumeBranch; { The task is resumed and branch stepping is enabled.}


const
{ For kMPTaskStateTaskInfo, the task's runState}
	kMPTaskBlocked = 0;    { Task is blocked (queued on resource)}
	kMPTaskReady = 1;    { Task is runnable}
	kMPTaskRunning = 2;     { Task is running}

const
{ For kMPTaskStateTaskInfo, the version of the MPTaskInfo structure requested.}
	kMPTaskInfoVersion = 3;


type
	MPTaskInfoVersion2Ptr = ^MPTaskInfoVersion2;
	MPTaskInfoVersion2 = record
		version: PBVersion;                { Version 2 of the data structure requested}

		name: OSType;                   { Task name}

		queueName: OSType;              { Task's queue owner name}
		runState: UInt16;               { Running, ready, blocked}
		lastCPU: UInt16;                { Address of CPU where task previously ran}
		weight: UInt32;                 { Processing weight: 1 - 10,000}

		processID: MPProcessID;              { Owning process ID}

		cpuTime: AbsoluteTime;                { Accumulated task time}
		schedTime: AbsoluteTime;              { Time when last scheduled}
		creationTime: AbsoluteTime;           { Time when task created}

		codePageFaults: ItemCount;         { Page faults from code execution}
		dataPageFaults: ItemCount;         { Page faults from data access}
		preemptions: ItemCount;            { Number of times task was preempted}

		cpuID: MPCpuID;                  { ID of CPU where task previously ran}
	end;
type
	MPTaskInfoPtr = ^MPTaskInfo;
	MPTaskInfo = record
		version: PBVersion;                { Version 3 of the data structure requested}

		name: OSType;                   { Task name}

		queueName: OSType;              { Task's queue owner name}
		runState: UInt16;               { Running, ready, blocked}
		lastCPU: UInt16;                { Address of CPU where task previously ran}
		weight: UInt32;                 { Processing weight: 1 - 10,000}

		processID: MPProcessID;              { Owning process ID}

		cpuTime: AbsoluteTime;                { Accumulated task time}
		schedTime: AbsoluteTime;              { Time when last scheduled}
		creationTime: AbsoluteTime;           { Time when task created}

		codePageFaults: ItemCount;         { Page faults from code execution}
		dataPageFaults: ItemCount;         { Page faults from data access}
		preemptions: ItemCount;            { Number of times task was preempted}

		cpuID: MPCpuID;                  { ID of CPU where task previously ran.}
		blockedObject: MPOpaqueID;          { ID of blocked object.}
		spaceID: MPAddressSpaceID;                { Address space ID of this task.}

		stackBase: LogicalAddress;              { Base of stack (lowest address).}
		stackLimit: LogicalAddress;             { Stack limit (highest address).}
		stackCurr: LogicalAddress;              { Current stack address.}
	end;
{
    Upon a task exception, the following message is sent to the designated queue:
      1. The MPTaskID, 
      2. The exception kind. These are enumerated in the interfaces header MachineExceptions.h 
      3. N/A
}


{ -------------------------------------------------------------------------------------------}


{
 *  MPSetExceptionHandler()
 *  
 *  Mac OS X threading:
 *    Thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in MPLibrary 2.0 and later
 }
function MPSetExceptionHandler( task: MPTaskID; exceptionQ: MPQueueID ): OSStatus; external name '_MPSetExceptionHandler';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  MPDisposeTaskException()
 *  
 *  Mac OS X threading:
 *    Thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in MPLibrary 2.0 and later
 }
function MPDisposeTaskException( task: MPTaskID; action: OptionBits ): OSStatus; external name '_MPDisposeTaskException';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  MPExtractTaskState()
 *  
 *  Mac OS X threading:
 *    Thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in MPLibrary 2.0 and later
 }
function MPExtractTaskState( task: MPTaskID; kind: MPTaskStateKind; info: UnivPtr ): OSStatus; external name '_MPExtractTaskState';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  MPSetTaskState()
 *  
 *  Mac OS X threading:
 *    Thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in MPLibrary 2.0 and later
 }
function MPSetTaskState( task: MPTaskID; kind: MPTaskStateKind; info: UnivPtr ): OSStatus; external name '_MPSetTaskState';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  MPThrowException()
 *  
 *  Mac OS X threading:
 *    Thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in MPLibrary 2.0 and later
 }
function MPThrowException( task: MPTaskID; kind: MPExceptionKind ): OSStatus; external name '_MPThrowException';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{ -------------------------------------------------------------------------------------------}


type
	MPDebuggerLevel = UInt32;
const
	kMPLowLevelDebugger = $00000000; { MacsBug-like}
	kMPMidLevelDebugger = $10000000; { Jasik-like}
	kMPHighLevelDebugger = $20000000; { Metrowerks-like}


{
 *  MPRegisterDebugger()
 *  
 *  Mac OS X threading:
 *    Thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in MPLibrary 2.0 and later
 }
function MPRegisterDebugger( queue: MPQueueID; level: MPDebuggerLevel ): OSStatus; external name '_MPRegisterDebugger';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  MPUnregisterDebugger()
 *  
 *  Mac OS X threading:
 *    Thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in MPLibrary 2.0 and later
 }
function MPUnregisterDebugger( queue: MPQueueID ): OSStatus; external name '_MPUnregisterDebugger';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
   §
   ===========================================================================================
   Remote Call Services
   ====================
}


type
	MPRemoteProcedure = function( parameter: UnivPtr ): UnivPtr;

type
	MPRemoteContext = UInt8;
const
	kMPAnyRemoteContext = 0;
	kMPOwningProcessRemoteContext = 1;
	kMPInterruptRemoteContext = 2;
	kMPAsyncInterruptRemoteContext = 3;


{
 *  MPRemoteCall()
 *  
 *  Summary:
 *    Calls a nonreentrant function and blocks the current task.
 *  
 *  Discussion:
 *    You use this function primarily to indirectly execute Mac OS
 *    system software functions. The task making the remote call is
 *    blocked until the call completes. The amount of time taken to
 *    schedule the remote procedure depends on the choice of the
 *    designated operating context. Specifying kMPAnyRemoteContext
 *    offers the lowest latency, but the called procedure may not have
 *    access to process-specific resources such as some low-memory
 *    values. Specifying kMPOwningProcessRemoteContext has higher
 *    latency because the remote procedure is deferred until the owning
 *    process becomes active. However, the remote procedure is
 *    guaranteed to execute within the owning process. Note that with
 *    the exception of functions in Multiprocessing Services, you
 *    cannot safely call any system software functions directly from a
 *    preemptive task. Even if some system software function appears to
 *    work today when called from a preemptive task, unless explicitly
 *    stated otherwise there is no guarantee that subsequent versions
 *    of the same function will continue to work in future versions of
 *    system software. In Mac OS 8 implementations of Multiprocessing
 *    Services, the only exceptions to this rule are the atomic memory
 *    operations (such as AddAtomic ) exported in the InterfaceLib
 *    shared library. Even these functions may switch to 68K mode if
 *    the operands to them are not aligned. If you need to access
 *    system software functions from a preemptive task, you must do so
 *    using the MPRemoteCall function.
 *  
 *  Mac OS X threading:
 *    Thread safe
 *  
 *  Parameters:
 *    
 *    remoteProc:
 *      A pointer of type MPRemoteProcedure that references the
 *      application-defined function you want to call. See
 *      MyRemoteProcedure for more information about the form of this
 *      function.
 *    
 *    parameter:
 *      A pointer to a parameter to pass to the application-defined
 *      function. For example, this value could point to a data
 *      structure or a memory location.
 *    
 *    context:
 *      A value of type MPRemoteContext that specifies which contexts
 *      (that is processes) are allowed to execute the function. See
 *      “Remote Call Context Option Constants” for a list of possible
 *      values.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in MPLibrary 2.0 and later
 }
function MPRemoteCall( remoteProc: MPRemoteProcedure; parameter: UnivPtr; context: MPRemoteContext ): UnivPtr; external name '_MPRemoteCall';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  MPRemoteCallCFM()
 *  
 *  Summary:
 *    Calls a nonreentrant function and blocks the current task.
 *  
 *  Discussion:
 *    You use this function primarily to indirectly execute Mac OS
 *    system software functions. The task making the remote call is
 *    blocked until the call completes. The amount of time taken to
 *    schedule the remote procedure depends on the choice of the
 *    designated operating context. Specifying kMPAnyRemoteContext
 *    offers the lowest latency, but the called procedure may not have
 *    access to process-specific resources such as some low-memory
 *    values. Specifying kMPOwningProcessRemoteContext has higher
 *    latency because the remote procedure is deferred until the owning
 *    process becomes active. However, the remote procedure is
 *    guaranteed to execute within the owning process. Note that with
 *    the exception of functions in Multiprocessing Services, you
 *    cannot safely call any system software functions directly from a
 *    preemptive task. Even if some system software function appears to
 *    work today when called from a preemptive task, unless explicitly
 *    stated otherwise there is no guarantee that subsequent versions
 *    of the same function will continue to work in future versions of
 *    system software.
 *  
 *  Mac OS X threading:
 *    Thread safe
 *  
 *  Parameters:
 *    
 *    remoteProc:
 *      A pointer of type MPRemoteProcedure to a CFM function that
 *      references the application-defined function you want to call.
 *      See MyRemoteProcedure for more information about the form of
 *      this function.
 *    
 *    parameter:
 *      A pointer to a parameter to pass to the application-defined
 *      function. For example, this value could point to a data
 *      structure or a memory location.
 *    
 *    context:
 *      A value of type MPRemoteContext that specifies which contexts
 *      (that is processes) are allowed to execute the function. See
 *      “Remote Call Context Option Constants” for a list of possible
 *      values.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in MPLibrary 2.0 and later
 }
function MPRemoteCallCFM( remoteProc: MPRemoteProcedure; parameter: UnivPtr; context: MPRemoteContext ): UnivPtr; external name '_MPRemoteCallCFM';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{
   §
   ===========================================================================================
   Checking API Availability
   =========================
}


{
   ===========================================================================================
   *** WARNING: You must properly check the availability of MP services before calling them!
   ===========================================================================================
   
   Checking for the availability of the MP API is rather ugly.  This is a historical problem,
   caused by the original implementation letting itself get prepared when it really wasn't
   usable and complicated by some important clients then depending on weak linking to "work".
   (And further complicated by CFM not supporting "deferred" imports, which is how many
   programmers think weak imports work.)
   
   The end result is that the MP API library may get prepared by CFM but be totally unusable.
   This means that if you import from the MP API library, you cannot simply check for a
   resolved import to decide if MP services are available.  Worse, if you explicitly prepare
   the MP API library you cannot assume that a noErr result from GetSharedLibrary means that
   MP services are available.
   
   • If you import from the MP API library you MUST:
   
        Use the MPLibraryIsLoaded macro (or equivalent code in languages other than C) to tell
        if the MP API services are available.  It is not sufficient to simply check that an
        imported symbol is resolved as is commonly done for other libraries.  The macro expands
        to the expression:
   
            ( ( (UInt32)_MPIsFullyInitialized != (UInt32)kUnresolvedCFragSymbolAddress ) &&
              ( _MPIsFullyInitialized () ) )
   
        This checks if the imported symbol _MPIsFullyInitialized is resolved and if resolved
        calls it.  Both parts must succeed for the MP API services to be available.
   
   • If you explicitly prepare the MP API library you MUST:
   
        Use code similar to the following example to tell if the MP API services are available.
        It is not sufficient to depend on just a noErr result from GetSharedLibrary.
   
            OSErr                       err;
            Boolean                     mpIsAvailable           = false;
            CFragConnectionID           connID                  = kInvalidID;
            MPIsFullyInitializedProc    mpIsFullyInitialized    = NULL;
   
            err = GetSharedLibrary  ( "\pMPLibrary", kCompiledCFragArch, kReferenceCFrag,
                                      &connID, NULL, NULL );
   
            if ( err == noErr ) (
                err = FindSymbol    ( connID, "\p_MPIsFullyInitialized",
                                      (Ptr *) &mpIsFullyInitialized, NULL );
            )
   
            if ( err == noErr ) (
                mpIsAvailable = (* mpIsFullyInitialized) ();
            )
   
   ===========================================================================================
}


{
 *  _MPIsFullyInitialized()
 *  
 *  Mac OS X threading:
 *    Thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in MPLibrary 1.0 and later
 }
function _MPIsFullyInitialized: Boolean; external name '__MPIsFullyInitialized';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


type
	MPIsFullyInitializedProc = function: Boolean;
const
	kMPUnresolvedCFragSymbolAddress = 0;
	MPLibraryIsLoaded = true;
{
   §
   ===========================================================================================
   Miscellaneous Services
   ======================
}


{
 *  _MPLibraryVersion()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in MPLibrary 1.0 and later
 }
procedure _MPLibraryVersion( versionCString: ConstCStringPtrPtr; major: UInt32Ptr; minor: UInt32Ptr; release: UInt32Ptr; revision: UInt32Ptr ); external name '__MPLibraryVersion';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
   §
   ===========================================================================================
   Unofficial Services
   ===================
}


{
   ===========================================================================================
   *** WARNING ***
   These services are not part of the officially documented multiprocessing API.  They may not
   be avaliable in future versions of Mac OS multiprocessing support, or in environments that
   have a different underlying OS architecture such as Mac OS on top of a microkernel, the
   Mac OS Blue Box under Mac OS X, native MP support in Mac OS X, etc.
   ===========================================================================================
}

{$ifc CALL_NOT_IN_CARBON}
{
 *  _MPAllocateSys()
 *  
 *  Availability:
 *    Mac OS X:         not available
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in MPLibrary 1.0 and later
 }


{ Use MPAllocateAligned instead.}
{
 *  _MPRPC()
 *  
 *  Availability:
 *    Mac OS X:         not available
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in MPLibrary 1.0 and later
 }


{ Use _MPRemoteCall instead.}
{
 *  _MPTaskIsToolboxSafe()
 *  
 *  Availability:
 *    Mac OS X:         not available
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in MPLibrary 1.0 and later
 }


{$endc}  {CALL_NOT_IN_CARBON}

{
 *  _MPLibraryIsCompatible()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in MPLibrary 1.0 and later
 }
function _MPLibraryIsCompatible( versionCString: ConstCStringPtr; major: UInt32; minor: UInt32; release: UInt32; revision: UInt32 ): Boolean; external name '__MPLibraryIsCompatible';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
   §
   ===========================================================================================
   Defunct Services
   ================
}

{$ifc CALL_NOT_IN_CARBON}
{$ifc undefined MPIncludeDefunctServices}
{$setc MPIncludeDefunctServices := 0}
{$endc}
{$ifc MPIncludeDefunctServices}
{
 *  _MPDebugStr()
 *  
 *  Availability:
 *    Mac OS X:         not available
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in MPLibraryObsolete 1.0 and later
 }


{
 *  _MPStatusPString()
 *  
 *  Availability:
 *    Mac OS X:         not available
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in MPLibraryObsolete 1.0 and later
 }


{
 *  _MPStatusCString()
 *  
 *  Availability:
 *    Mac OS X:         not available
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in MPLibraryObsolete 1.0 and later
 }


type
	MPPrintfHandler = procedure( taskID: MPTaskID; format: ConstCStringPtr; args: va_list );
{
 *  _MPInitializePrintf()
 *  
 *  Availability:
 *    Mac OS X:         not available
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in MPLibraryObsolete 1.0 and later
 }


{
 *  _MPPrintf()
 *  
 *  Availability:
 *    Mac OS X:         not available
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in MPLibraryObsolete 1.0 and later
 }


{$endc}  {MPIncludeDefunctServices}
{$endc}  {CALL_NOT_IN_CARBON}

{ ===========================================================================================}

{$endc} {TARGET_OS_MAC}
{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}

end.
{$endc} {not MACOSALLINCLUDE}
