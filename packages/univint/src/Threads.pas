{
     File:       CarbonCore/Threads.h
 
     Contains:   Thread Manager Interfaces.
 
     Version:    CarbonCore-859.2~1
 
     Copyright:  © 1991-2008 by Apple Computer, Inc., all rights reserved.
 
     Bugs?:      For bug reports, consult the following page on
                 the World Wide Web:
 
                     http://www.freepascal.org/bugs.html
 
}
{      Pascal Translation Updated:  Peter N Lewis, <peter@stairways.com.au>, November 2005 }
{      Pascal Translation Updated:  Jonas Maebe, <jonas@freepascal.org>, October 2009 }
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

unit Threads;
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
	{$setc TARGET_CPU_PPC := FALSE}
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

{$ALIGN MAC68K}

{ Thread states}
type
	ThreadState = UInt16;
const
	kReadyThreadState = 0;
	kStoppedThreadState = 1;
	kRunningThreadState = 2;

{ Error codes have been moved to Errors.(pah)}

{ Thread environment characteristics}
type
	ThreadTaskRef = UnivPtr;
{ Thread characteristics}
type
	ThreadStyle = UInt32;
const
	kCooperativeThread = 1 shl 0;
	kPreemptiveThread = 1 shl 1;

{ Thread identifiers}
type
	ThreadID = UNSIGNEDLONG;
const
	kNoThreadID = 0;
	kCurrentThreadID = 1;
	kApplicationThreadID = 2;

{ Options when creating a thread}
type
	ThreadOptions = UInt32;
const
	kNewSuspend = 1 shl 0;
	kUsePremadeThread = 1 shl 1;
	kCreateIfNeeded = 1 shl 2;
	kFPUNotNeeded = 1 shl 3;
	kExactMatchThread = 1 shl 4;

{ Information supplied to the custom scheduler}
type
	SchedulerInfoRec = record
		InfoRecSize: UInt32;
		CurrentThreadID: ThreadID;
		SuggestedThreadID: ThreadID;
		InterruptedCoopThreadID: ThreadID;
	end;
	SchedulerInfoRecPtr = ^SchedulerInfoRec;

{
    The following ProcPtrs cannot be interchanged with UniversalProcPtrs because
    of differences between 680x0 and PowerPC runtime architectures with regard to
    the implementation of the Thread Manager.
 }
type
	voidPtr = UnivPtr;
{ Prototype for thread's entry (main) routine}
type
	ThreadEntryProcPtr = function( threadParam: UnivPtr ): voidPtr;
{ Prototype for custom thread scheduler routine}
type
	ThreadSchedulerProcPtr = function( schedulerInfo: SchedulerInfoRecPtr ): ThreadID;
{ Prototype for custom thread switcher routine}
type
	ThreadSwitchProcPtr = procedure( threadBeingSwitched: ThreadID; switchProcParam: UnivPtr );
{ Prototype for thread termination notification routine}
type
	ThreadTerminationProcPtr = procedure( threadTerminated: ThreadID; terminationProcParam: UnivPtr );
{ Prototype for debugger NewThread notification}
type
	DebuggerNewThreadProcPtr = procedure( threadCreated: ThreadID );
{ Prototype for debugger DisposeThread notification}
type
	DebuggerDisposeThreadProcPtr = procedure( threadDeleted: ThreadID );
{ Prototype for debugger schedule notification}
type
	DebuggerThreadSchedulerProcPtr = function( schedulerInfo: SchedulerInfoRecPtr ): ThreadID;
type
	ThreadEntryUPP = ThreadEntryProcPtr;
	ThreadSchedulerUPP = ThreadSchedulerProcPtr;
	ThreadSwitchUPP = ThreadSwitchProcPtr;
	ThreadTerminationUPP = ThreadTerminationProcPtr;
	DebuggerNewThreadUPP = DebuggerNewThreadProcPtr;
	DebuggerDisposeThreadUPP = DebuggerDisposeThreadProcPtr;
	DebuggerThreadSchedulerUPP = DebuggerThreadSchedulerProcPtr;
{
 *  NewThreadEntryUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function NewThreadEntryUPP( userRoutine: ThreadEntryProcPtr ): ThreadEntryUPP; external name '_NewThreadEntryUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  NewThreadSchedulerUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function NewThreadSchedulerUPP( userRoutine: ThreadSchedulerProcPtr ): ThreadSchedulerUPP; external name '_NewThreadSchedulerUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  NewThreadSwitchUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function NewThreadSwitchUPP( userRoutine: ThreadSwitchProcPtr ): ThreadSwitchUPP; external name '_NewThreadSwitchUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  NewThreadTerminationUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function NewThreadTerminationUPP( userRoutine: ThreadTerminationProcPtr ): ThreadTerminationUPP; external name '_NewThreadTerminationUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  NewDebuggerNewThreadUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function NewDebuggerNewThreadUPP( userRoutine: DebuggerNewThreadProcPtr ): DebuggerNewThreadUPP; external name '_NewDebuggerNewThreadUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  NewDebuggerDisposeThreadUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function NewDebuggerDisposeThreadUPP( userRoutine: DebuggerDisposeThreadProcPtr ): DebuggerDisposeThreadUPP; external name '_NewDebuggerDisposeThreadUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  NewDebuggerThreadSchedulerUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function NewDebuggerThreadSchedulerUPP( userRoutine: DebuggerThreadSchedulerProcPtr ): DebuggerThreadSchedulerUPP; external name '_NewDebuggerThreadSchedulerUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  DisposeThreadEntryUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure DisposeThreadEntryUPP( userUPP: ThreadEntryUPP ); external name '_DisposeThreadEntryUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  DisposeThreadSchedulerUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure DisposeThreadSchedulerUPP( userUPP: ThreadSchedulerUPP ); external name '_DisposeThreadSchedulerUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  DisposeThreadSwitchUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure DisposeThreadSwitchUPP( userUPP: ThreadSwitchUPP ); external name '_DisposeThreadSwitchUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  DisposeThreadTerminationUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure DisposeThreadTerminationUPP( userUPP: ThreadTerminationUPP ); external name '_DisposeThreadTerminationUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  DisposeDebuggerNewThreadUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure DisposeDebuggerNewThreadUPP( userUPP: DebuggerNewThreadUPP ); external name '_DisposeDebuggerNewThreadUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  DisposeDebuggerDisposeThreadUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure DisposeDebuggerDisposeThreadUPP( userUPP: DebuggerDisposeThreadUPP ); external name '_DisposeDebuggerDisposeThreadUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  DisposeDebuggerThreadSchedulerUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure DisposeDebuggerThreadSchedulerUPP( userUPP: DebuggerThreadSchedulerUPP ); external name '_DisposeDebuggerThreadSchedulerUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  InvokeThreadEntryUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function InvokeThreadEntryUPP( threadParam: UnivPtr; userUPP: ThreadEntryUPP ): voidPtr; external name '_InvokeThreadEntryUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  InvokeThreadSchedulerUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function InvokeThreadSchedulerUPP( schedulerInfo: SchedulerInfoRecPtr; userUPP: ThreadSchedulerUPP ): ThreadID; external name '_InvokeThreadSchedulerUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  InvokeThreadSwitchUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure InvokeThreadSwitchUPP( threadBeingSwitched: ThreadID; switchProcParam: UnivPtr; userUPP: ThreadSwitchUPP ); external name '_InvokeThreadSwitchUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  InvokeThreadTerminationUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure InvokeThreadTerminationUPP( threadTerminated: ThreadID; terminationProcParam: UnivPtr; userUPP: ThreadTerminationUPP ); external name '_InvokeThreadTerminationUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  InvokeDebuggerNewThreadUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure InvokeDebuggerNewThreadUPP( threadCreated: ThreadID; userUPP: DebuggerNewThreadUPP ); external name '_InvokeDebuggerNewThreadUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  InvokeDebuggerDisposeThreadUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure InvokeDebuggerDisposeThreadUPP( threadDeleted: ThreadID; userUPP: DebuggerDisposeThreadUPP ); external name '_InvokeDebuggerDisposeThreadUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  InvokeDebuggerThreadSchedulerUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function InvokeDebuggerThreadSchedulerUPP( schedulerInfo: SchedulerInfoRecPtr; userUPP: DebuggerThreadSchedulerUPP ): ThreadID; external name '_InvokeDebuggerThreadSchedulerUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
   Thread Manager function pointers (TPP):
   on classic 68k use raw function pointers (same as UPP's)
   on classic PowerPC, use raw function pointers
   on classic PowerPC with OPAQUE_UPP_TYPES=1, use UPP's
   on CFM-68K, use UPP's
   on Carbon, use UPP's
}

{ use UPP's}
type
	ThreadEntryTPP = ThreadEntryUPP;
	ThreadSchedulerTPP = ThreadSchedulerUPP;
	ThreadSwitchTPP = ThreadSwitchUPP;
	ThreadTerminationTPP = ThreadTerminationUPP;
	DebuggerNewThreadTPP = DebuggerNewThreadUPP;
	DebuggerDisposeThreadTPP = DebuggerDisposeThreadUPP;
	DebuggerThreadSchedulerTPP = DebuggerThreadSchedulerUPP;
{
 *  NewThread()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ThreadsLib 1.0 and later
 }
function NewThread( threadStyle_: ThreadStyle; threadEntry: ThreadEntryTPP; threadParam: UnivPtr; stackSize: Size; options: ThreadOptions; threadResult: UnivPtrPtr { can be NULL }; var threadMade: ThreadID ): OSErr; external name '_NewThread';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  SetThreadScheduler()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ThreadsLib 1.0 and later
 }
function SetThreadScheduler( threadScheduler: ThreadSchedulerTPP ): OSErr; external name '_SetThreadScheduler';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  SetThreadSwitcher()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ThreadsLib 1.0 and later
 }
function SetThreadSwitcher( thread: ThreadID; threadSwitcher: ThreadSwitchTPP; switchProcParam: UnivPtr; inOrOut: Boolean ): OSErr; external name '_SetThreadSwitcher';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  SetThreadTerminator()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ThreadsLib 1.0 and later
 }
function SetThreadTerminator( thread: ThreadID; threadTerminator: ThreadTerminationTPP; terminationProcParam: UnivPtr ): OSErr; external name '_SetThreadTerminator';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  SetDebuggerNotificationProcs()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ThreadsLib 1.0 and later
 }
function SetDebuggerNotificationProcs( notifyNewThread: DebuggerNewThreadTPP; notifyDisposeThread: DebuggerDisposeThreadTPP; notifyThreadScheduler: DebuggerThreadSchedulerTPP ): OSErr; external name '_SetDebuggerNotificationProcs';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  CreateThreadPool()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ThreadsLib 1.0 and later
 }
function CreateThreadPool( threadStyle_: ThreadStyle; numToCreate: SInt16; stackSize: Size ): OSErr; external name '_CreateThreadPool';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  GetDefaultThreadStackSize()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ThreadsLib 1.0 and later
 }
function GetDefaultThreadStackSize( threadStyle_: ThreadStyle; var stackSize: Size ): OSErr; external name '_GetDefaultThreadStackSize';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  ThreadCurrentStackSpace()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ThreadsLib 1.0 and later
 }
function ThreadCurrentStackSpace( thread: ThreadID; var freeStack: ByteCount ): OSErr; external name '_ThreadCurrentStackSpace';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  DisposeThread()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ThreadsLib 1.0 and later
 }
function DisposeThread( threadToDump: ThreadID; threadResult: UnivPtr; recycleThread: Boolean ): OSErr; external name '_DisposeThread';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  YieldToThread()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ThreadsLib 1.0 and later
 }
function YieldToThread( suggestedThread: ThreadID ): OSErr; external name '_YieldToThread';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  YieldToAnyThread()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ThreadsLib 1.0 and later
 }
function YieldToAnyThread: OSErr; external name '_YieldToAnyThread';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  [Mac]GetCurrentThread()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ThreadsLib 1.0 and later
 }
function GetCurrentThread( var currentThreadID: ThreadID ): OSErr; external name '_GetCurrentThread';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)
function MacGetCurrentThread( var currentThreadID: ThreadID ): OSErr; external name '_GetCurrentThread';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  GetThreadState()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ThreadsLib 1.0 and later
 }
function GetThreadState( threadToGet: ThreadID; var threadState_: ThreadState ): OSErr; external name '_GetThreadState';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  SetThreadState()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ThreadsLib 1.0 and later
 }
function SetThreadState( threadToSet: ThreadID; newState: ThreadState; suggestedThread: ThreadID ): OSErr; external name '_SetThreadState';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  SetThreadStateEndCritical()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ThreadsLib 1.0 and later
 }
function SetThreadStateEndCritical( threadToSet: ThreadID; newState: ThreadState; suggestedThread: ThreadID ): OSErr; external name '_SetThreadStateEndCritical';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  ThreadBeginCritical()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ThreadsLib 1.0 and later
 }
function ThreadBeginCritical: OSErr; external name '_ThreadBeginCritical';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  ThreadEndCritical()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ThreadsLib 1.0 and later
 }
function ThreadEndCritical: OSErr; external name '_ThreadEndCritical';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  GetThreadCurrentTaskRef()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ThreadsLib 1.0 and later
 }
function GetThreadCurrentTaskRef( var threadTRef: ThreadTaskRef ): OSErr; external name '_GetThreadCurrentTaskRef';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  GetThreadStateGivenTaskRef()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ThreadsLib 1.0 and later
 }
function GetThreadStateGivenTaskRef( threadTRef: ThreadTaskRef; threadToGet: ThreadID; var threadState_: ThreadState ): OSErr; external name '_GetThreadStateGivenTaskRef';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  SetThreadReadyGivenTaskRef()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ThreadsLib 1.0 and later
 }
function SetThreadReadyGivenTaskRef( threadTRef: ThreadTaskRef; threadToSet: ThreadID ): OSErr; external name '_SetThreadReadyGivenTaskRef';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{$ifc not TARGET_CPU_64}
{ This routine was never implemented on Mac OS X.}
{
 *  GetFreeThreadCount()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework but deprecated in 10.3
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ThreadsLib 1.0 and later
 }
function GetFreeThreadCount( threadStyle_: ThreadStyle; var freeCount: SInt16 ): OSErr; external name '_GetFreeThreadCount';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_3 *)


{ This routine was never implemented on Mac OS X.}
{
 *  GetSpecificFreeThreadCount()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework but deprecated in 10.3
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ThreadsLib 1.0 and later
 }
function GetSpecificFreeThreadCount( threadStyle_: ThreadStyle; stackSize: Size; var freeCount: SInt16 ): OSErr; external name '_GetSpecificFreeThreadCount';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_3 *)


{$endc} {not TARGET_CPU_64}

{$endc} {TARGET_OS_MAC}
{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}

end.
{$endc} {not MACOSALLINCLUDE}
