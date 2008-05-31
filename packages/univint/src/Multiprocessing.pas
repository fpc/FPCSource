{
     File:       Multiprocessing.p
 
     Contains:   Multiprocessing interfaces
 
     Version:    Technology: Multiprocessing API version 2.4, integrated NanoKernel support
                 Release:    Universal Interfaces 3.4.2
 
     Copyright:  © 1995-2002 DayStar Digital, Inc.
 
     Bugs?:      For bug reports, consult the following page on
                 the World Wide Web:
 
                     http://www.freepascal.org/bugs.html
 
}


{
   ===========================================================================================
   *** WARNING: You must properly check the availability of MP services before calling them!
   See the section titled "Checking API Availability".
   ===========================================================================================
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

unit Multiprocessing;
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
uses MacTypes;


{$ALIGN POWER}


{
   ===========================================================================================
   This is the header file for version 2.4 of the Mac OS multiprocessing support.  This version
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
   be called to see if the MP API is ReallyTrulyª usable.)  New code and recompiles of old
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
   ¤
   ===========================================================================================
   General Types and Constants
   ===========================
}


const
	MPLibrary_MajorVersion		= 2;							{  ! When these change be sure to update the build versions }
	MPLibrary_MinorVersion		= 4;							{  !  used in the startup check in MPInitializeAPI! }
	MPLibrary_Release			= 1;
	MPLibrary_DevelopmentRevision = 1;


type
	MPProcessID    = ^SInt32; { an opaque 32-bit type }
	MPProcessIDPtr = ^MPProcessID;  { when a var xx:MPProcessID parameter can be nil, it is changed to xx: MPProcessIDPtr }
	MPTaskID    = ^SInt32; { an opaque 32-bit type }
	MPTaskIDPtr = ^MPTaskID;  { when a var xx:MPTaskID parameter can be nil, it is changed to xx: MPTaskIDPtr }
	MPQueueID    = ^SInt32; { an opaque 32-bit type }
	MPQueueIDPtr = ^MPQueueID;  { when a var xx:MPQueueID parameter can be nil, it is changed to xx: MPQueueIDPtr }
	MPSemaphoreID    = ^SInt32; { an opaque 32-bit type }
	MPSemaphoreIDPtr = ^MPSemaphoreID;  { when a var xx:MPSemaphoreID parameter can be nil, it is changed to xx: MPSemaphoreIDPtr }
	MPCriticalRegionID    = ^SInt32; { an opaque 32-bit type }
	MPCriticalRegionIDPtr = ^MPCriticalRegionID;  { when a var xx:MPCriticalRegionID parameter can be nil, it is changed to xx: MPCriticalRegionIDPtr }
	MPTimerID    = ^SInt32; { an opaque 32-bit type }
	MPTimerIDPtr = ^MPTimerID;  { when a var xx:MPTimerID parameter can be nil, it is changed to xx: MPTimerIDPtr }
	MPEventID    = ^SInt32; { an opaque 32-bit type }
	MPEventIDPtr = ^MPEventID;  { when a var xx:MPEventID parameter can be nil, it is changed to xx: MPEventIDPtr }
	MPAddressSpaceID    = ^SInt32; { an opaque 32-bit type }
	MPAddressSpaceIDPtr = ^MPAddressSpaceID;  { when a var xx:MPAddressSpaceID parameter can be nil, it is changed to xx: MPAddressSpaceIDPtr }
	MPNotificationID    = ^SInt32; { an opaque 32-bit type }
	MPNotificationIDPtr = ^MPNotificationID;  { when a var xx:MPNotificationID parameter can be nil, it is changed to xx: MPNotificationIDPtr }
	MPCoherenceID    = ^SInt32; { an opaque 32-bit type }
	MPCoherenceIDPtr = ^MPCoherenceID;  { when a var xx:MPCoherenceID parameter can be nil, it is changed to xx: MPCoherenceIDPtr }
	MPCpuID    = ^SInt32; { an opaque 32-bit type }
	MPCpuIDPtr = ^MPCpuID;  { when a var xx:MPCpuID parameter can be nil, it is changed to xx: MPCpuIDPtr }
	MPAreaID    = ^SInt32; { an opaque 32-bit type }
	MPAreaIDPtr = ^MPAreaID;  { when a var xx:MPAreaID parameter can be nil, it is changed to xx: MPAreaIDPtr }
	MPConsoleID    = ^SInt32; { an opaque 32-bit type }
	MPConsoleIDPtr = ^MPConsoleID;  { when a var xx:MPConsoleID parameter can be nil, it is changed to xx: MPConsoleIDPtr }
	MPOpaqueID    = ^SInt32; { an opaque 32-bit type }
	MPOpaqueIDPtr = ^MPOpaqueID;  { when a var xx:MPOpaqueID parameter can be nil, it is changed to xx: MPOpaqueIDPtr }

const
																{  Values for MPOpaqueIDClass. }
	kOpaqueAnyID				= 0;
	kOpaqueProcessID			= 1;
	kOpaqueTaskID				= 2;
	kOpaqueTimerID				= 3;
	kOpaqueQueueID				= 4;
	kOpaqueSemaphoreID			= 5;
	kOpaqueCriticalRegionID		= 6;
	kOpaqueCpuID				= 7;
	kOpaqueAddressSpaceID		= 8;
	kOpaqueEventID				= 9;
	kOpaqueCoherenceID			= 10;
	kOpaqueAreaID				= 11;
	kOpaqueNotificationID		= 12;
	kOpaqueConsoleID			= 13;


type
	MPOpaqueIDClass						= UInt32;


const
	kMPNoID						= 0;							{  New code should use kInvalidID everywhere. }


type
	MPTaskOptions						= OptionBits;
	TaskStorageIndex					= UInt32;
	TaskStorageValue					= UInt32;
	MPSemaphoreCount					= ItemCount;
	MPTaskWeight						= UInt32;
	MPEventFlags						= UInt32;
	MPEventFlagsPtr						= ^MPEventFlags; { when a VAR xx: MPEventFlags parameter can be nil, it is changed to xx: MPEventFlagsPtr }
	MPExceptionKind						= UInt32;
	MPTaskStateKind						= UInt32;
	MPPageSizeClass						= UInt32;


const
	kDurationImmediate			= 0;
	kDurationForever			= $7FFFFFFF;
	kDurationMillisecond		= 1;
	kDurationMicrosecond		= -1;


	{
	   ¤
	   ===========================================================================================
	   Process/Processor Services
	   ==========================
	}


	{
	 *  MPProcessors()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in MPLibrary 1.0 and later
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
function MPProcessors: ItemCount; external name '_MPProcessors';

{  The physical total. }

{
 *  MPProcessorsScheduled()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in MPLibrary 2.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function MPProcessorsScheduled: ItemCount; external name '_MPProcessorsScheduled';

{  Those currently in use. }

{
   ¤
   ===========================================================================================
   Tasking Services
   ================
}


const
																{  For MPCreateTask options }
	kMPCreateTaskTakesAllExceptionsMask = $00000002;
	kMPCreateTaskNotDebuggableMask = $00000004;
	kMPCreateTaskValidOptionsMask = $00000006;


	{  ------------------------------------------------------------------------------------------- }


type
{$ifc TYPED_FUNCTION_POINTERS}
	TaskProc = function(parameter: UnivPtr): OSStatus;
{$elsec}
	TaskProc = ProcPtr;
{$endc}


	{
	 *  MPCreateTask()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in MPLibrary 1.0 and later
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
function MPCreateTask(entryPoint: TaskProc; parameter: UnivPtr; stackSize: ByteCount; notifyQueue: MPQueueID; terminationParameter1: UnivPtr; terminationParameter2: UnivPtr; options: MPTaskOptions; task: MPTaskIDPtr): OSStatus; external name '_MPCreateTask';


{
 *  MPTerminateTask()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in MPLibrary 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function MPTerminateTask(task: MPTaskID; terminationStatus: OSStatus): OSStatus; external name '_MPTerminateTask';


{
 *  MPSetTaskWeight()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in MPLibrary 2.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function MPSetTaskWeight(task: MPTaskID; weight: MPTaskWeight): OSStatus; external name '_MPSetTaskWeight';


{
 *  MPTaskIsPreemptive()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in MPLibrary 2.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function MPTaskIsPreemptive(taskID: MPTaskID): boolean; external name '_MPTaskIsPreemptive';

{  May be kInvalidID. }

{
 *  MPExit()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in MPLibrary 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure MPExit(status: OSStatus); external name '_MPExit';


{
 *  MPYield()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in MPLibrary 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure MPYield; external name '_MPYield';


{
 *  MPCurrentTaskID()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in MPLibrary 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function MPCurrentTaskID: MPTaskID; external name '_MPCurrentTaskID';


{
 *  MPSetTaskType()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in MPLibrary 2.3 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function MPSetTaskType(task: MPTaskID; taskType: OSType): OSStatus; external name '_MPSetTaskType';


{  ------------------------------------------------------------------------------------------- }


{
   ---------------------------------------------------
   ! The task storage services are new in version 2.0.
}


{
 *  MPAllocateTaskStorageIndex()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in MPLibrary 2.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function MPAllocateTaskStorageIndex(var index: TaskStorageIndex): OSStatus; external name '_MPAllocateTaskStorageIndex';


{
 *  MPDeallocateTaskStorageIndex()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in MPLibrary 2.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function MPDeallocateTaskStorageIndex(index: TaskStorageIndex): OSStatus; external name '_MPDeallocateTaskStorageIndex';


{
 *  MPSetTaskStorageValue()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in MPLibrary 2.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function MPSetTaskStorageValue(index: TaskStorageIndex; value: TaskStorageValue): OSStatus; external name '_MPSetTaskStorageValue';


{
 *  MPGetTaskStorageValue()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in MPLibrary 2.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function MPGetTaskStorageValue(index: TaskStorageIndex): TaskStorageValue; external name '_MPGetTaskStorageValue';


{
   ¤
   ===========================================================================================
   Synchronization Services
   ========================
}


{
 *  MPCreateQueue()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in MPLibrary 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function MPCreateQueue(var queue: MPQueueID): OSStatus; external name '_MPCreateQueue';


{
 *  MPDeleteQueue()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in MPLibrary 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function MPDeleteQueue(queue: MPQueueID): OSStatus; external name '_MPDeleteQueue';


{
 *  MPNotifyQueue()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in MPLibrary 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function MPNotifyQueue(queue: MPQueueID; param1: UnivPtr; param2: UnivPtr; param3: UnivPtr): OSStatus; external name '_MPNotifyQueue';


{
 *  MPWaitOnQueue()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in MPLibrary 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function MPWaitOnQueue(queue: MPQueueID; param1: UnivPtr; param2: UnivPtr; param3: UnivPtr; timeout: Duration): OSStatus; external name '_MPWaitOnQueue';


{
 *  MPSetQueueReserve()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in MPLibrary 2.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function MPSetQueueReserve(queue: MPQueueID; count: ItemCount): OSStatus; external name '_MPSetQueueReserve';


{  ------------------------------------------------------------------------------------------- }


{
 *  MPCreateSemaphore()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in MPLibrary 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function MPCreateSemaphore(maximumValue: MPSemaphoreCount; initialValue: MPSemaphoreCount; var semaphore: MPSemaphoreID): OSStatus; external name '_MPCreateSemaphore';


{
 *  MPDeleteSemaphore()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in MPLibrary 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function MPDeleteSemaphore(semaphore: MPSemaphoreID): OSStatus; external name '_MPDeleteSemaphore';


{
 *  MPSignalSemaphore()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in MPLibrary 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function MPSignalSemaphore(semaphore: MPSemaphoreID): OSStatus; external name '_MPSignalSemaphore';


{
 *  MPWaitOnSemaphore()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in MPLibrary 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function MPWaitOnSemaphore(semaphore: MPSemaphoreID; timeout: Duration): OSStatus; external name '_MPWaitOnSemaphore';


{  ------------------------------------------------------------------------------------------- }


{
 *  MPCreateCriticalRegion()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in MPLibrary 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function MPCreateCriticalRegion(var criticalRegion: MPCriticalRegionID): OSStatus; external name '_MPCreateCriticalRegion';


{
 *  MPDeleteCriticalRegion()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in MPLibrary 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function MPDeleteCriticalRegion(criticalRegion: MPCriticalRegionID): OSStatus; external name '_MPDeleteCriticalRegion';


{
 *  MPEnterCriticalRegion()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in MPLibrary 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function MPEnterCriticalRegion(criticalRegion: MPCriticalRegionID; timeout: Duration): OSStatus; external name '_MPEnterCriticalRegion';


{
 *  MPExitCriticalRegion()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in MPLibrary 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function MPExitCriticalRegion(criticalRegion: MPCriticalRegionID): OSStatus; external name '_MPExitCriticalRegion';


{  ------------------------------------------------------------------------------------------- }


{
 *  MPCreateEvent()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in MPLibrary 2.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function MPCreateEvent(var event: MPEventID): OSStatus; external name '_MPCreateEvent';


{
 *  MPDeleteEvent()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in MPLibrary 2.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function MPDeleteEvent(event: MPEventID): OSStatus; external name '_MPDeleteEvent';


{
 *  MPSetEvent()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in MPLibrary 2.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function MPSetEvent(event: MPEventID; flags: MPEventFlags): OSStatus; external name '_MPSetEvent';


{
 *  MPWaitForEvent()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in MPLibrary 2.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function MPWaitForEvent(event: MPEventID; flags: MPEventFlagsPtr; timeout: Duration): OSStatus; external name '_MPWaitForEvent';

{
   ¤
   ===========================================================================================
   Notification Services (API)
   =====================
}


{
 *  MPCreateNotification()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in MPLibrary 2.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function MPCreateNotification(var notificationID: MPNotificationID): OSStatus; external name '_MPCreateNotification';


{
 *  MPDeleteNotification()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in MPLibrary 2.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function MPDeleteNotification(notificationID: MPNotificationID): OSStatus; external name '_MPDeleteNotification';


{
 *  MPModifyNotification()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in MPLibrary 2.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function MPModifyNotification(notificationID: MPNotificationID; anID: MPOpaqueID; notifyParam1: UnivPtr; notifyParam2: UnivPtr; notifyParam3: UnivPtr): OSStatus; external name '_MPModifyNotification';


{
 *  MPModifyNotificationParameters()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in MPLibrary 2.3 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function MPModifyNotificationParameters(notificationID: MPNotificationID; kind: MPOpaqueIDClass; notifyParam1: UnivPtr; notifyParam2: UnivPtr; notifyParam3: UnivPtr): OSStatus; external name '_MPModifyNotificationParameters';


{
 *  MPCauseNotification()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in MPLibrary 2.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function MPCauseNotification(notificationID: MPNotificationID): OSStatus; external name '_MPCauseNotification';


{
   ¤
   ===========================================================================================
   Timer Services
   ==============
}


{
   --------------------------------------------
   ! The timer services are new in version 2.0.
}


{
   Utilities you might want to use from DriverServices
    extern AbsoluteTime UpTime              ( void );
    extern AbsoluteTime DurationToAbsolute  ( Duration      duration );
    extern Duration     AbsoluteToDuration  ( AbsoluteTime  time );
}


const
																{  For MPArmTimer options }
	kMPPreserveTimerIDMask		= $00000001;
	kMPTimeIsDeltaMask			= $00000002;
	kMPTimeIsDurationMask		= $00000004;


	{
	 *  MPDelayUntil()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in MPLibrary 2.0 and later
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
function MPDelayUntil(var expirationTime: AbsoluteTime): OSStatus; external name '_MPDelayUntil';


{$ifc CALL_NOT_IN_CARBON}
{
 *  MPDelayUntilSys()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in MPLibrary 2.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function MPDelayUntilSys(var expirationTime: AbsoluteTime): OSStatus; external name '_MPDelayUntilSys';


{$endc}  {CALL_NOT_IN_CARBON}

{
 *  MPCreateTimer()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in MPLibrary 2.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function MPCreateTimer(var timerID: MPTimerID): OSStatus; external name '_MPCreateTimer';


{
 *  MPDeleteTimer()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in MPLibrary 2.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function MPDeleteTimer(timerID: MPTimerID): OSStatus; external name '_MPDeleteTimer';


{
 *  MPSetTimerNotify()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in MPLibrary 2.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function MPSetTimerNotify(timerID: MPTimerID; anID: MPOpaqueID; notifyParam1: UnivPtr; notifyParam2: UnivPtr; notifyParam3: UnivPtr): OSStatus; external name '_MPSetTimerNotify';


{
 *  MPArmTimer()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in MPLibrary 2.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function MPArmTimer(timerID: MPTimerID; var expirationTime: AbsoluteTime; options: OptionBits): OSStatus; external name '_MPArmTimer';


{
 *  MPCancelTimer()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in MPLibrary 2.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function MPCancelTimer(timerID: MPTimerID; timeRemaining: AbsoluteTimePtr): OSStatus; external name '_MPCancelTimer';


{
   ¤
   ===========================================================================================
   Memory Services
   ===============
}


const
																{  Maximum allocation request size is 1GB. }
	kMPMaxAllocSize				= 1073741824;

																{  Values for the alignment parameter to MPAllocateAligned. }
	kMPAllocateDefaultAligned	= 0;
	kMPAllocate8ByteAligned		= 3;
	kMPAllocate16ByteAligned	= 4;
	kMPAllocate32ByteAligned	= 5;
	kMPAllocate1024ByteAligned	= 10;
	kMPAllocate4096ByteAligned	= 12;
	kMPAllocateMaxAlignment		= 16;							{  Somewhat arbitrary limit on expectations. }
	kMPAllocateAltiVecAligned	= 4;							{  The P.C. name. }
	kMPAllocateVMXAligned		= 4;							{  The older, common name. }
	kMPAllocateVMPageAligned	= 254;							{  Pseudo value, converted at runtime. }
	kMPAllocateInterlockAligned	= 255;							{  Pseudo value, converted at runtime. }


																{  Values for the options parameter to MPAllocateAligned. }
	kMPAllocateClearMask		= $0001;						{  Zero the allocated block. }
	kMPAllocateGloballyMask		= $0002;						{  Allocate from the globally visible pool. }
	kMPAllocateResidentMask		= $0004;						{  Allocate from the RAM-resident pool. }
	kMPAllocateNoGrowthMask		= $0010;						{  Do not attempt to grow the pool. }
	kMPAllocateNoCreateMask		= $0020;						{  Do not attempt to create the pool if it doesn't exist yet. }


	{  ------------------------------------------------------------------------------------------- }


	{
	 *  MPAllocateAligned()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in MPLibrary 2.0 and later
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
function MPAllocateAligned(size: ByteCount; alignment: ByteParameter; options: OptionBits): LogicalAddress; external name '_MPAllocateAligned';

{  ! MPAllocateAligned is new in version 2.0. }

{
 *  MPAllocate()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in MPLibrary 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function MPAllocate(size: ByteCount): LogicalAddress; external name '_MPAllocate';

{  Use MPAllocateAligned instead. }

{
 *  MPFree()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in MPLibrary 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure MPFree(objct: LogicalAddress); external name '_MPFree';


{
 *  MPGetAllocatedBlockSize()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in MPLibrary 2.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function MPGetAllocatedBlockSize(objct: LogicalAddress): ByteCount; external name '_MPGetAllocatedBlockSize';


{  ------------------------------------------------------------------------------------------- }


{
 *  MPBlockCopy()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in MPLibrary 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure MPBlockCopy(source: LogicalAddress; destination: LogicalAddress; size: ByteCount); external name '_MPBlockCopy';


{
 *  MPBlockClear()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in MPLibrary 2.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure MPBlockClear(address: LogicalAddress; size: ByteCount); external name '_MPBlockClear';

{  ! MPBlockClear is new in version 2.0. }

{
 *  MPDataToCode()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in MPLibrary 2.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure MPDataToCode(address: LogicalAddress; size: ByteCount); external name '_MPDataToCode';

{  ! MPDataToCode is new in version 2.0. }
{
   ¤
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


const
																{  Values for the TaskStateKind to MPExtractTaskState and MPSetTaskState. }
	kMPTaskStateRegisters		= 0;							{  The task general registers. }
	kMPTaskStateFPU				= 1;							{  The task floating point registers }
	kMPTaskStateVectors			= 2;							{  The task vector registers }
	kMPTaskStateMachine			= 3;							{  The task machine registers }
	kMPTaskState32BitMemoryException = 4;						{  The task memory exception information for 32-bit CPUs. }
	kMPTaskStateTaskInfo		= 5;							{  Static and dynamic information about the task. }


																{  Option bits and numbers for MPDisposeTaskException. }
	kMPTaskPropagate			= 0;							{  The exception is propagated. }
	kMPTaskResumeStep			= 1;							{  The task is resumed and single step is enabled. }
	kMPTaskResumeBranch			= 2;							{  The task is resumed and branch stepping is enabled. }
	kMPTaskResumeMask			= $0000;						{  The task is resumed. }
	kMPTaskPropagateMask		= $01;							{  The exception is propagated. }
	kMPTaskResumeStepMask		= $02;							{  The task is resumed and single step is enabled. }
	kMPTaskResumeBranchMask		= $04;							{  The task is resumed and branch stepping is enabled. }


																{  For kMPTaskStateTaskInfo, the task's runState }
	kMPTaskBlocked				= 0;							{  Task is blocked (queued on resource) }
	kMPTaskReady				= 1;							{  Task is runnable }
	kMPTaskRunning				= 2;							{  Task is running }

																{  For kMPTaskStateTaskInfo, the version of the MPTaskInfo structure requested. }
	kMPTaskInfoVersion			= 3;


type
	MPTaskInfoPtr = ^MPTaskInfo;
	MPTaskInfo = record
		version:				PBVersion;								{  Version 3 of the data structure requested }
		name:					OSType;									{  Task name }
		queueName:				OSType;									{  Task's queue owner name }
		runState:				UInt16;									{  Running, ready, blocked }
		lastCPU:				UInt16;									{  Address of CPU where task previously ran }
		weight:					UInt32;									{  Processing weight: 1 - 10,000 }
		processID:				MPProcessID;							{  Owning process ID }
		cpuTime:				AbsoluteTime;							{  Accumulated task time }
		schedTime:				AbsoluteTime;							{  Time when last scheduled }
		creationTime:			AbsoluteTime;							{  Time when task created }
		codePageFaults:			ItemCount;								{  Page faults from code execution }
		dataPageFaults:			ItemCount;								{  Page faults from data access }
		preemptions:			ItemCount;								{  Number of times task was preempted }
		cpuID:					MPCpuID;								{  ID of CPU where task previously ran. }
		blockedObject:			MPOpaqueID;								{  ID of blocked object. }
		spaceID:				MPAddressSpaceID;						{  Address space ID of this task. }
		stackBase:				LogicalAddress;							{  Base of stack (lowest address). }
		stackLimit:				LogicalAddress;							{  Stack limit (highest address). }
		stackCurr:				LogicalAddress;							{  Current stack address. }
	end;

	{
	    Upon a task exception, the following message is sent to the designated queue:
	      1. The MPTaskID, 
	      2. The exception kind. These are enumerated in the interfaces header MachineExceptions.h 
	      3. N/A
	}


	{  ------------------------------------------------------------------------------------------- }


	{
	 *  MPSetExceptionHandler()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in MPLibrary 2.0 and later
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
function MPSetExceptionHandler(task: MPTaskID; exceptionQ: MPQueueID): OSStatus; external name '_MPSetExceptionHandler';


{
 *  MPDisposeTaskException()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in MPLibrary 2.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function MPDisposeTaskException(task: MPTaskID; action: OptionBits): OSStatus; external name '_MPDisposeTaskException';


{
 *  MPExtractTaskState()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in MPLibrary 2.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function MPExtractTaskState(task: MPTaskID; kind: MPTaskStateKind; info: UnivPtr): OSStatus; external name '_MPExtractTaskState';


{
 *  MPSetTaskState()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in MPLibrary 2.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function MPSetTaskState(task: MPTaskID; kind: MPTaskStateKind; info: UnivPtr): OSStatus; external name '_MPSetTaskState';


{
 *  MPThrowException()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in MPLibrary 2.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function MPThrowException(task: MPTaskID; kind: MPExceptionKind): OSStatus; external name '_MPThrowException';


{  ------------------------------------------------------------------------------------------- }


type
	MPDebuggerLevel 			= UInt32;
const
	kMPLowLevelDebugger			= $00000000;					{  MacsBug-like }
	kMPMidLevelDebugger			= $10000000;					{  Jasik-like }
	kMPHighLevelDebugger		= $20000000;					{  Metrowerks-like }


	{
	 *  MPRegisterDebugger()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in MPLibrary 2.0 and later
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
function MPRegisterDebugger(queue: MPQueueID; level: MPDebuggerLevel): OSStatus; external name '_MPRegisterDebugger';


{
 *  MPUnregisterDebugger()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in MPLibrary 2.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function MPUnregisterDebugger(queue: MPQueueID): OSStatus; external name '_MPUnregisterDebugger';


{
   ¤
   ===========================================================================================
   Remote Call Services
   ====================
}


type
{$ifc TYPED_FUNCTION_POINTERS}
	MPRemoteProcedure = function(parameter: UnivPtr): Ptr;
{$elsec}
	MPRemoteProcedure = ProcPtr;
{$endc}

	MPRemoteContext						= UInt8;

const
	kMPAnyRemoteContext			= 0;
	kMPOwningProcessRemoteContext = 1;
	kMPInterruptRemoteContext	= 2;
	kMPAsyncInterruptRemoteContext = 3;


	{
	 *  MPRemoteCall()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in MPLibrary 2.0 and later
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
function MPRemoteCall(remoteProc: MPRemoteProcedure; parameter: UnivPtr; context: ByteParameter): Ptr; external name '_MPRemoteCall';

{  ! MPRemoteCall is new in version 2.0. }
{
   ¤
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
   
   ¥ If you import from the MP API library you MUST:
   
        Use the MPLibraryIsLoaded macro (or equivalent code in languages other than C) to tell
        if the MP API services are available.  It is not sufficient to simply check that an
        imported symbol is resolved as is commonly done for other libraries.  The macro expands
        to the expression:
   
            ( ( (UInt32)_MPIsFullyInitialized != (UInt32)kUnresolvedCFragSymbolAddress ) &&
              ( _MPIsFullyInitialized () ) )
   
        This checks if the imported symbol _MPIsFullyInitialized is resolved and if resolved
        calls it.  Both parts must succeed for the MP API services to be available.
   
   ¥ If you explicitly prepare the MP API library you MUST:
   
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
 *  Availability:
 *    Non-Carbon CFM:   in MPLibrary 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function _MPIsFullyInitialized: boolean; external name '__MPIsFullyInitialized';


type
{$ifc TYPED_FUNCTION_POINTERS}
	MPIsFullyInitializedProc = function: boolean;
{$elsec}
	MPIsFullyInitializedProc = ProcPtr;
{$endc}

	{
	   ===========================================================================================
	   The MPLibraryIsLoaded service is a macro under C that expands to the logical expression:
	        ( (UInt32)MPProcessors != (UInt32)kUnresolvedCFragSymbolAddress )
	   The intention is to check if the imported symbol MPProcessors is resolved.  For other
	   languages use the equivalent expression.
	   ===========================================================================================
	}
	{
	   ¤
	   ===========================================================================================
	   Miscellaneous Services
	   ======================
	}


	{
	 *  _MPLibraryVersion()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in MPLibrary 1.0 and later
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
procedure _MPLibraryVersion(versionCString: ConstCStringPtrPtr; major: UInt32Ptr; minor: UInt32Ptr; release: UInt32Ptr; revision: UInt32Ptr); external name '__MPLibraryVersion';


{
   ¤
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
{$ifc CALL_NOT_IN_CARBON}
{
 *  _MPAllocateSys()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in MPLibrary 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function _MPAllocateSys(size: ByteCount): LogicalAddress; external name '__MPAllocateSys';

{  Use MPAllocateAligned instead. }
{
 *  _MPRPC()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in MPLibrary 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function _MPRPC(remoteProc: MPRemoteProcedure; parameter: UnivPtr): Ptr; external name '__MPRPC';

{  Use _MPRemoteCall instead. }
{
 *  _MPTaskIsToolboxSafe()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in MPLibrary 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function _MPTaskIsToolboxSafe(task: MPTaskID): boolean; external name '__MPTaskIsToolboxSafe';

{$endc}  {CALL_NOT_IN_CARBON}
{$endc}  {CALL_NOT_IN_CARBON}

{
 *  _MPLibraryIsCompatible()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in MPLibrary 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function _MPLibraryIsCompatible(versionCString: ConstCStringPtr; major: UInt32; minor: UInt32; release: UInt32; revision: UInt32): boolean; external name '__MPLibraryIsCompatible';


{
   ¤
   ===========================================================================================
   Defunct Services
   ================
}

{$ifc CALL_NOT_IN_CARBON}
{$ifc undefined MPIncludeDefunctServices}
{$setc MPIncludeDefunctServices := 0}
{$endc}
{$ifc MPIncludeDefunctServices}
{$ifc CALL_NOT_IN_CARBON}
{
 *  _MPDebugStr()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in MPLibraryObsolete 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure _MPDebugStr(const (*var*) msg: Str255); external name '__MPDebugStr';

{
 *  _MPStatusPString()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in MPLibraryObsolete 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function _MPStatusPString(status: OSStatus): StringPtr; external name '__MPStatusPString';

{
 *  _MPStatusCString()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in MPLibraryObsolete 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function _MPStatusCString(status: OSStatus): ConstCStringPtr; external name '__MPStatusCString';


{$endc}  {CALL_NOT_IN_CARBON}
{$endc}  {MPIncludeDefunctServices}
{$endc}  {CALL_NOT_IN_CARBON}

{  =========================================================================================== }


{$ALIGN MAC68K}


end.
