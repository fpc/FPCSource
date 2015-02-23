{
     File:       CarbonCore/MacMemory.h
 
     Contains:   Memory Manager Interfaces.
                 The contents of this header file are deprecated.
                 Use malloc, free, etc instead.
 
     Copyright:  © 1985-2011 by Apple Inc. All rights reserved.
}
{      Pascal Translation Updated:  Jonas Maebe, <jonas@freepascal.org>, October 2009 }
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

unit MacMemory;
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
uses MacTypes;
{$endc} {not MACOSALLINCLUDE}


{$ifc TARGET_OS_MAC}

{$ALIGN MAC68K}


const
	maxSize = $7FFFFFF0; {the largest block possible}

{
    If you define a macro named __MAC_OS_X_MEMORY_MANAGER_CLEAN__ with a non-zero value, then
    some Memory Manager APIs will become inlined, minimal implementations.  See the comments
    below for more information about this.
}
{$setc __MAC_OS_X_MEMORY_MANAGER_CLEAN__ := 0}

{$ifc not __MAC_OS_X_MEMORY_MANAGER_CLEAN__}
const
	defaultPhysicalEntryCount = 8;

const
{ values returned from the GetPageState function }
	kPageInMemory = 0;
	kPageOnDisk = 1;
	kNotPaged = 2;

const
{ masks for Zone->heapType field }
	k32BitHeap = 1;    { valid in all Memory Managers }
	kNewStyleHeap = 2;    { true if new Heap Manager is present }
	kNewDebugHeap = 4;     { true if new Heap Manager is running in debug mode on this heap }


{$endc} {not __MAC_OS_X_MEMORY_MANAGER_CLEAN__}

{ bits for use with HGetState/HSetState}
const
	kHandleIsResourceBit = 5;
	kHandlePurgeableBit = 6;
	kHandleLockedBit = 7;

{ masks for use with HGetState/HSetState}
const
	kHandleIsResourceMask = $20;
	kHandlePurgeableMask = $40;
	kHandleLockedMask = $80;

{$ifc not TARGET_CPU_64}
type
	GrowZoneProcPtr = function( cbNeeded: Size ): SIGNEDLONG;
	PurgeProcPtr = procedure( blockToPurge: Handle );
	UserFnProcPtr = procedure( parameter: UnivPtr );
	GrowZoneUPP = GrowZoneProcPtr;
	PurgeUPP = PurgeProcPtr;
	UserFnUPP = UserFnProcPtr;
	ZonePtr = ^Zone;
	Zone = record
		bkLim: Ptr;
		purgePtr: Ptr;
		hFstFree: Ptr;
		zcbFree: SIGNEDLONG;
		gzProc: GrowZoneUPP;
		moreMast: SInt16;
		flags: SInt16;
		cntRel: SInt16;
		maxRel: SInt16;
		cntNRel: SInt16;
		heapType: SInt8;               { previously "maxNRel", now holds flags (e.g. k32BitHeap)}
		unused: SInt8;
		cntEmpty: SInt16;
		cntHandles: SInt16;
		minCBFree: SIGNEDLONG;
		purgeProc: PurgeUPP;
		sparePtr: Ptr;
		allocPtr: Ptr;
		heapData: SInt16;
	end;
type
	THz = ^Zone;
	THzPtr = ^THz;
{$ifc not __MAC_OS_X_MEMORY_MANAGER_CLEAN__}
type
	MemoryBlockPtr = ^MemoryBlock;
	MemoryBlock = record
		address: UnivPtr;
		count: UNSIGNEDLONG;
	end;
type
	LogicalToPhysicalTablePtr = ^LogicalToPhysicalTable;
	LogicalToPhysicalTable = record
		logical: MemoryBlock;
		physical: array [0..7] of MemoryBlock;
	end;

	PageState = SInt16;
	StatusRegisterContents = SInt16;
const
	kVolumeVirtualMemoryInfoVersion1 = 1;  { first version of VolumeVirtualMemoryInfo}

type
	VolumeVirtualMemoryInfo = record
		version: PBVersion;                { Input: Version of the VolumeVirtualMemoryInfo structure}
		volumeRefNum: SInt16;           { Input: volume reference number}
		inUse: Boolean;                  { output: true if volume is currently used for file mapping}
		_fill: UInt8;
		vmOptions: UInt32;              { output: tells what volume can support (same as DriverGestaltVMOptionsResponse vmOptions bits in DriverGestalt)}
                                              { end of kVolumeVirtualMemoryInfoVersion1 structure}
	end;
	VolumeVirtualMemoryInfoPtr = ^VolumeVirtualMemoryInfo;
{$endc} {not __MAC_OS_X_MEMORY_MANAGER_CLEAN__}

{
 *  NewGrowZoneUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function NewGrowZoneUPP( userRoutine: GrowZoneProcPtr ): GrowZoneUPP; external name '_NewGrowZoneUPP';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)

{
 *  NewPurgeUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function NewPurgeUPP( userRoutine: PurgeProcPtr ): PurgeUPP; external name '_NewPurgeUPP';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)

{
 *  NewUserFnUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function NewUserFnUPP( userRoutine: UserFnProcPtr ): UserFnUPP; external name '_NewUserFnUPP';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)

{
 *  DisposeGrowZoneUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure DisposeGrowZoneUPP( userUPP: GrowZoneUPP ); external name '_DisposeGrowZoneUPP';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)

{
 *  DisposePurgeUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure DisposePurgeUPP( userUPP: PurgeUPP ); external name '_DisposePurgeUPP';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)

{
 *  DisposeUserFnUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure DisposeUserFnUPP( userUPP: UserFnUPP ); external name '_DisposeUserFnUPP';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)

{
 *  InvokeGrowZoneUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function InvokeGrowZoneUPP( cbNeeded: Size; userUPP: GrowZoneUPP ): SIGNEDLONG; external name '_InvokeGrowZoneUPP';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)

{
 *  InvokePurgeUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure InvokePurgeUPP( blockToPurge: Handle; userUPP: PurgeUPP ); external name '_InvokePurgeUPP';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)

{
 *  InvokeUserFnUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure InvokeUserFnUPP( parameter: UnivPtr; userUPP: UserFnUPP ); external name '_InvokeUserFnUPP';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)

{$endc} {not TARGET_CPU_64}

{
 *  MemError()
 *  
 *  Summary:
 *    Determines if an application’s last direct call to a Memory
 *    Manager function executed successfully.
 *  
 *  Discussion:
 *    MemError() yields the result code produced by the last Memory
 *    Manager function your application called directly, and resets
 *    MemError() to return noErr in the future. MemError() is useful
 *    during application debugging. You might also use MemError as one
 *    part of a memory-management scheme to identify instances in which
 *    the Memory Manager rejects overly large memory requests by
 *    returning the error code memFullErr.
 *    
 *    To view the result codes that MemError() can produce, see "Memory
 *    Manager Result Codes".
 *    
 *    Do not rely on MemError() as the only component of a
 *    memory-management scheme. For example, suppose you call NewHandle
 *    or NewPtr and receive the result code noErr, indicating that the
 *    Memory Manager was able to allocate sufficient memory. In this
 *    case, you have no guarantee that the allocation did not deplete
 *    your application’s memory reserves to levels so low that simple
 *    operations might cause your application to crash. Instead of
 *    relying on MemError(), check before making a memory request that
 *    there is enough memory both to fulfill the request and to support
 *    essential operations.
 *    
 *    On Mac OS X 10.3 and later, the value of MemError() is kept for
 *    each thread; prior to Mac OS X 10.3.  MemError() is global to the
 *    application.  Because of this, and other problems, the Memory
 *    Manager APIs are not thread safe before Mac OS X 10.3.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.3
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function MemError: OSErr; external name '_MemError';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  LMGetMemErr()
 *  
 *  Summary:
 *    Returns the result of the last Memory Manager function, without
 *    clearing the value like MemError() does.
 *  
 *  Discussion:
 *    LMGetMemErr yields the result code produced by the last Memory
 *    Manager function your application called directly. Unlike
 *    MemError(), this function does not reset the stored value, so
 *    subsequent calls to LMGetMemErr() will still return this value
 *    until the next Memory Manager routine is called or until
 *    MemError() is called to reset the value. LMGetMemErr is useful
 *    during application debugging. You might also use this value as
 *    one part of a memory-management scheme to identify instances in
 *    which the Memory Manager rejects overly large memory requests by
 *    returning the error code memFullErr.
 *    
 *    To view the result codes that MemError() can produce, see "Memory
 *    Manager Result Codes".
 *    
 *    Do not rely on MemError() as the only component of a
 *    memory-management scheme. For example, suppose you call NewHandle
 *    or NewPtr and receive the result code noErr, indicating that the
 *    Memory Manager was able to allocate sufficient memory. In this
 *    case, you have no guarantee that the allocation did not deplete
 *    your application’s memory reserves to levels so low that simple
 *    operations might cause your application to crash. Instead of
 *    relying on MemError(), check before making a memory request that
 *    there is enough memory both to fulfill the request and to support
 *    essential operations.
 *    
 *    On Mac OS X 10.3 and later, the value of MemError() is kept for
 *    each thread; prior to Mac OS X 10.3 there was one global value of
 *    MemError() which all threads shared. Because of this, and other
 *    problems, the Memory Manager APIs are not thread safe before Mac
 *    OS X 10.3.
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function LMGetMemErr: SInt16; external name '_LMGetMemErr';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  LMSetMemErr()
 *  
 *  Summary:
 *    Set the value which will be returned by MemError()
 *  
 *  Discussion:
 *    User code shouldn't need to call this function, which is used to
 *    set the value which the next call to MemError() will return.
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Parameters:
 *    
 *    value:
 *      the value which the next MemError() function call should return
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
procedure LMSetMemErr( value: SInt16 ); external name '_LMSetMemErr';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  NewHandle()
 *  
 *  Summary:
 *    Allocate a relocatable memory block of a specified size.
 *  
 *  Discussion:
 *    The NewHandle function attempts to allocate a new relocatable
 *    block in the current heap zone with a logical size of logicalSize
 *    bytes and then return a handle to the block. The new block is
 *    unlocked and unpurgeable. If NewHandle cannot allocate a block of
 *    the requested size, it returns NULL.  The memory block returned
 *    likely will contain garbage, and will be unlocked and
 *    non-purgeable.
 *    
 *    WARNING
 *    
 *    Do not try to manufacture your own handles without this function
 *    by simply assigning the address of a variable of type Ptr to a
 *    variable of type Handle. The resulting "fake handle" would not
 *    reference a relocatable block and could cause a system crash.
 *     If this function returns NIL, the error result can be determined
 *    by calling the function MemError().
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.3
 *  
 *  Parameters:
 *    
 *    byteCount:
 *      the size of the relocatable memory block to allocate.  If this
 *      value is < 0, NIL will be returned. If this value is 0, a
 *      handle to 0 byte block will be returned.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function NewHandle( byteCount: Size ): Handle; external name '_NewHandle';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  NewHandleClear()
 *  
 *  Summary:
 *    Allocate a relocatable memory block of a specified size.
 *  
 *  Discussion:
 *    The NewHandle function attempts to allocate a new relocatable
 *    block in the current heap zone with a logical size of logicalSize
 *    bytes and then return a handle to the block. The new block is
 *    unlocked and unpurgeable. If NewHandle cannot allocate a block of
 *    the requested size, it returns NULL.  The memory block returned
 *    will be zeroed, and will be unlocked and non-purgeable.
 *    
 *    WARNING
 *    
 *    Do not try to manufacture your own handles without this function
 *    by simply assigning the address of a variable of type Ptr to a
 *    variable of type Handle. The resulting "fake handle" would not
 *    reference a relocatable block and could cause a system crash.
 *     If this function returns NIL, the error result can be determined
 *    by calling the function MemError().
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.3
 *  
 *  Parameters:
 *    
 *    byteCount:
 *      the size of the relocatable memory block to allocate.  If this
 *      value is < 0, NIL will be returned. If this value is 0, a
 *      handle to 0 byte block will be returned.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function NewHandleClear( byteCount: Size ): Handle; external name '_NewHandleClear';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  RecoverHandle()
 *  
 *  Summary:
 *    Returns a handle to a relocatable block pointed to by a specified
 *    pointer.
 *  
 *  Discussion:
 *    The Memory Manager does not allow you to change relocatable
 *    blocks into nonrelocatable blocks, or vice-versa. However, if you
 *    no longer have access to a handle but still have access to its
 *    master pointer p, you can use the RecoverHandle function to
 *    recreate a handle to the relocatable block referenced by
 *    p.
 *    
 *    Call the function MemError() to get the result code. See "Memory
 *    Manager Result Codes".
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.3
 *  
 *  Parameters:
 *    
 *    p:
 *      the master pointer to a relocatable block.
 *  
 *  Result:
 *    A handle to a relocatable block point to by p. If p does not
 *    point to a valid block, this function returns NULL.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function RecoverHandle( p: Ptr ): Handle; external name '_RecoverHandle';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  NewPtr()
 *  
 *  Summary:
 *    Allocates a nonrelocatable block of memory of a specified size.
 *  
 *  Discussion:
 *    The NewPtr function attempts to reserve space for the new block.
 *    If it is able to reserve the requested amount of space, NewPtr
 *    allocates the nonrelocatable block.  Otherwise, NewPtr returns
 *    NULL and generates a memFullErr error. On Mac OS X, NewPtr will
 *    never fail because it is unable to allocate the pointer. Certain
 *    old versions of Mac OS X return a NULL pointer when asked to
 *    allocate a pointer of size 0.
 *    Call the function MemError to get the result code. See "Memory
 *    Manager Result Codes".
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.3
 *  
 *  Parameters:
 *    
 *    byteCount:
 *      The requested size (in bytes) of the nonrelocatable block.  If
 *      you pass a value of zero, this function returns a valid zero
 *      length pointer.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function NewPtr( byteCount: Size ): Ptr; external name '_NewPtr';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  NewPtrClear()
 *  
 *  Summary:
 *    Allocates a nonrelocatable block of memory of a specified size
 *    with all its bytes set to 0.
 *  
 *  Discussion:
 *    The NewPtr function attempts to reserve space for the new block.
 *    If it is able to reserve the requested amount of space, NewPtr
 *    allocates the nonrelocatable block.  Otherwise, NewPtr returns
 *    NULL and generates a memFullErr error. On Mac OS X, NewPtr will
 *    never fail because it is unable to allocate the pointer. Certain
 *    old versions of Mac OS X return a NULL pointer when asked to
 *    allocate a pointer of size 0.
 *    Call the function MemError to get the result code. See "Memory
 *    Manager Result Codes".
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.3
 *  
 *  Parameters:
 *    
 *    byteCount:
 *      The requested size (in bytes) of the nonrelocatable block.  If
 *      you pass a value of zero, this function returns a valid zero
 *      length pointer.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function NewPtrClear( byteCount: Size ): Ptr; external name '_NewPtrClear';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{$ifc not TARGET_CPU_64}
{
 *  MaxBlock()   *** DEPRECATED ***
 *  
 *  Summary:
 *    Return the size of the largest block you could allocate in the
 *    current heap zone after compaction.
 *  
 *  Discussion:
 *    On Mac OS X, this function always returns a large value, because
 *    virtual memory is always available to fulfill any request for
 *    memory.  This function is deprecated on Mac OS X and later.  You
 *    can assume that any reasonable memory allocation will succeed.
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function MaxBlock: SIGNEDLONG; external name '_MaxBlock';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_5, __IPHONE_NA, __IPHONE_NA) *)


{
 *  StackSpace()   *** DEPRECATED ***
 *  
 *  Summary:
 *    Returns the amount of space unused on the current thread's stack.
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function StackSpace: SIGNEDLONG; external name '_StackSpace';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_5, __IPHONE_NA, __IPHONE_NA) *)


{$endc} {not TARGET_CPU_64}

{
 *  NewEmptyHandle()
 *  
 *  Summary:
 *    Initializes a new handle without allocating any memory for it to
 *    control.
 *  
 *  Discussion:
 *    When you want to allocate memory for the empty handle, use the
 *    ReallocateHandle function.
 *    Call the function MemError to get the result code. See "Memory
 *    Manager Result Codes".
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.3
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function NewEmptyHandle: Handle; external name '_NewEmptyHandle';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  HLock()
 *  
 *  Summary:
 *    Lock a relocatable block so that it does not move in the heap
 *  
 *  Discussion:
 *    The HLock procedure locks the relocatable block to which h is a
 *    handle, preventing it from being moved within its heap zone. If
 *    the block is already locked,HLock does nothing.
 *    
 *    On Mac OS X, the behaviour of the Memory Manager and of heaps in
 *    general is different than on Mac OS 9.x and earlier. In
 *    particular, the heap on Mac OS X is never purged or compacted. 
 *    Therefore, an unlocked handle will never be relocated except as a
 *    result of a direct action by something calling SetHandleSize() or
 *    by using a function like PtrAndHand() which implicitly resizes
 *    the handle to append data to it.  Because of this, most locking
 *    and unlocking of handles is unnecessary on Mac OS X, and the use
 *    of HLock() and other functions is being deprecated.  If you
 *    define a macro named __MAC_OS_X_MEMORY_MANAGER_CLEAN__ to 1 in
 *    your sources before you include MacMemory.h, then HLock() and
 *    several other functions will become empty operations, removing
 *    the overhead of a function call.
 *    
 *    However, some applications are relying on the behavior that
 *    resizing a locked handle produces an error, or tracking the state
 *    of the locked bit for a give handle via the HGetState() function.
 *     Applications which rely on this can not use
 *    __MAC_OS_X_MEMORY_MANAGER_CLEAN__.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.3
 *  
 *  Parameters:
 *    
 *    h:
 *      the handle to lock.  If h is == NULL, then HLock() sets
 *      MemError() to noErr.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
procedure HLock( h: Handle ); external name '_HLock';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  HLockHi()
 *  
 *  Summary:
 *    Lock a relocatable handle.
 *  
 *  Discussion:
 *    The HLockHi() function locks a handle in memory.  On versions of
 *    Mac OS before Mac OS X, it would first attempt to move the handle
 *    as high in memory as feasible.  However, on Mac OS X and later,
 *    there is no advantage to having handles high in memory, and so
 *    this function never moves a handle before locking it.
 *    See the discussion about handle locking above the function
 *    HLock().
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Parameters:
 *    
 *    h:
 *      the handle to lock.  If h is == NULL, then HLockHi() sets
 *      MemError() to noErr.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
procedure HLockHi( h: Handle ); external name '_HLockHi';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  HUnlock()
 *  
 *  Summary:
 *    Unlock a relocatable block so that it does not move in the heap
 *  
 *  Discussion:
 *    The HUnlock procedure unlocks the relocatable block to which h is
 *    a handle, allowing it from being moved within its heap zone. If
 *    the block is already unlocked, HUnlock does nothing.
 *    
 *    See the discussion about handles and locking on Mac OS X above
 *    the HLock() function.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.3
 *  
 *  Parameters:
 *    
 *    h:
 *      the handle to unlock.  If h is == NULL, then HUnlock() sets
 *      MemError() to noErr.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
procedure HUnlock( h: Handle ); external name '_HUnlock';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{$ifc not TARGET_CPU_64}
{
 *  HPurge()   *** DEPRECATED ***
 *  
 *  Summary:
 *    Mark a relocatable block so that it does can be purged if a
 *    memory request cannot be fulfilled after compaction of the heap
 *  
 *  Discussion:
 *    The HPurge procedure makes the relocatable block to which h is a
 *    handle purgeable. If the block is already purgeable, HPurge does
 *    nothing.
 *    
 *    On Mac OS X, heaps are never purged.  Therefore, the use of
 *    HPurge() and its associated functios is deprecated. If you define
 *    a macro __MAC_OS_X_MEMORY_MANAGER_CLEAN__ in your sources before
 *    you include MacMemory.h, then any calls to HPurge() in your
 *    program will essentially be removed.
 *    
 *    However, some applications may set the handle as purgeable, and
 *    then later check the purgeBit for the handle via HGetState().  If
 *    your application depends on the purge bit being set for handles,
 *    you will not be able to take advantage of this macro.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.3
 *  
 *  Parameters:
 *    
 *    h:
 *      the handle to mark as purgeable.  If h is == NULL, then
 *      HPurge() just sets MemError() to noErr.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
procedure HPurge( h: Handle ); external name '_HPurge';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{
 *  HNoPurge()   *** DEPRECATED ***
 *  
 *  Summary:
 *    Mark a relocatable block so that it can not be purged.
 *  
 *  Discussion:
 *    The HNoPurge procedure makes the relocatable block to which h is
 *    a handle unpurgeable. See the discussion about purgable handles
 *    above the HPurge() function.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.3
 *  
 *  Parameters:
 *    
 *    h:
 *      the handle to mark as nonpurgeable.  If h is == NULL, then
 *      HPurge() just sets MemError() to noErr.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
procedure HNoPurge( h: Handle ); external name '_HNoPurge';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{$endc} {not TARGET_CPU_64}

{
 *  TempNewHandle()
 *  
 *  Summary:
 *    Allocate a relocatable memory block of a specified size.
 *  
 *  Discussion:
 *    The TempNewHandle function attempts to allocate a new relocatable
 *    block in the current heap zone with a logical size of logicalSize
 *    bytes and then return a handle to the block. The new block is
 *    unlocked and unpurgeable. If NewHandle cannot allocate a block of
 *    the requested size, it returns NULL.  The memory block returned
 *    likely will contain garbage.
 *    
 *    WARNING
 *    
 *    Do not try to manufacture your own handles without this function
 *    by simply assigning the address of a variable of type Ptr to a
 *    variable of type Handle. The resulting "fake handle" would not
 *    reference a relocatable block and could cause a system crash.
 *     If this function returns NIL, the error result can be determined
 *    by calling the function MemError().
 *    
 *    On Mac OS X, there is no temporary memory heap, and thus no
 *    difference between the handles returned by TempNewHandle() and
 *    those returned by NewHandle().  The only difference between the
 *    two is that TempNewHandle() also returns the error result of the
 *    call in resultCode.
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Parameters:
 *    
 *    logicalSize:
 *      the size of the relocatable memory block to allocate.  If this
 *      value is < 0, NIL will be returned. If this value is 0, a
 *      handle to 0 byte block will be returned.
 *    
 *    resultCode:
 *      On exit, this will be set to the result of the operation.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function TempNewHandle( logicalSize: Size; var resultCode: OSErr ): Handle; external name '_TempNewHandle';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{$ifc not TARGET_CPU_64}
{
 *  TempMaxMem()   *** DEPRECATED ***
 *  
 *  Summary:
 *    Return the maximum amount of temporary memory available
 *  
 *  Discussion:
 *    On Mac OS X, this function always returns a large value, because
 *    virtual memory is always available to fulfill any request for
 *    memory.  This function is deprecated on Mac OS X and later.  You
 *    can assume that any reasonable memory allocation will succeed.
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Parameters:
 *    
 *    grow:
 *      If != NULL, then this is filled in with the the value 0.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function TempMaxMem( var grow: Size ): Size; external name '_TempMaxMem';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{
 *  TempFreeMem()   *** DEPRECATED ***
 *  
 *  Summary:
 *    Return the maximum amount of free memory in the temporary heap.
 *  
 *  Discussion:
 *    On Mac OS X, there is no separate temporary memory heap.  This
 *    function always returns a large value, because virtual memory is
 *    always available to fulfill any request for memory.  This
 *    function is deprecated on Mac OS X and later.  You can assume
 *    that any reasonable memory allocation will succeed.
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function TempFreeMem: SIGNEDLONG; external name '_TempFreeMem';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{
 *  CompactMem()   *** DEPRECATED ***
 *  
 *  Summary:
 *    Compact the heap by purging and moving blocks such that at least
 *    cbNeeded bytes are available, if possible.
 *  
 *  Discussion:
 *    On Mac OS X and later, blocks are never purged and memory heaps
 *    will grow as necessary, so compaction is never necessary nor
 *    performed.
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function CompactMem( cbNeeded: Size ): Size; external name '_CompactMem';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PurgeMem()   *** DEPRECATED ***
 *  
 *  Summary:
 *    Purge blocks from the heap until cbNeeded bytes are available, if
 *    possible.
 *  
 *  Discussion:
 *    On Mac OS X and later, blocks are never purged and memory heaps
 *    will grow as necessary, so purging of a heap is never necessary
 *    nor performed.
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
procedure PurgeMem( cbNeeded: Size ); external name '_PurgeMem';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{
 *  FreeMem()   *** DEPRECATED ***
 *  
 *  Summary:
 *    Return the maximum amount of free memory in the temporary heap.
 *  
 *  Discussion:
 *    On Mac OS X, this function always returns a large value, because
 *    virtual memory is always available to fulfill any request for
 *    memory.  This function is deprecated on Mac OS X and later.  You
 *    can assume that any reasonable memory allocation will succeed.
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *
 *  Note:
 *    FreeMem has been renamed MacFreeMem, to resolve a naming conflict with
 *    FreeMem in the Turbo Pascal/Delphi/FreePascal runtime library
 }
function MacFreeMem: SInt32; external name '_FreeMem';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_5, __IPHONE_NA, __IPHONE_NA) *)


{
 *  MaxMem()   *** DEPRECATED ***
 *  
 *  Summary:
 *    Return the maximum amount of free memory available
 *  
 *  Discussion:
 *    On Mac OS X, this function always returns a large value, because
 *    virtual memory is always available to fulfill any request for
 *    memory.  This function is deprecated on Mac OS X and later.  You
 *    can assume that any reasonable memory allocation will succeed.
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Parameters:
 *    
 *    grow:
 *      If != NULL, then this is filled in with the the value 0.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function MaxMem( var grow: Size ): Size; external name '_MaxMem';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_5, __IPHONE_NA, __IPHONE_NA) *)


{
 *  SetGrowZone()   *** DEPRECATED ***
 *  
 *  Summary:
 *    Set a function which is called when a heap is grown
 *  
 *  Discussion:
 *    On Mac OS X and later, heaps never grow, and so the function set
 *    by SetGrowZone() is never called.
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Parameters:
 *    
 *    growZone:
 *      a upp for a function to call when a heap needs to be grown
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
procedure SetGrowZone( growZone: GrowZoneUPP ); external name '_SetGrowZone';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{
 *  GetGrowZone()   *** DEPRECATED ***
 *  
 *  Summary:
 *    Get the function which is called when a heap is grown
 *  
 *  Discussion:
 *    On Mac OS X and later, heaps never grow, and so this function (
 *    set by SetGrowZone() ) is never called.
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   not available
 }
function GetGrowZone: GrowZoneUPP; external name '_GetGrowZone';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{
 *  MoveHHi()   *** DEPRECATED ***
 *  
 *  Summary:
 *    Move a handle as high in memory as possible
 *  
 *  Discussion:
 *    On versions of Mac OS before Mac OS X, MoveHHi() would move the
 *    handle as high in memory as feasible. However, on Mac OS X and
 *    later, there is no advantage to having handles high in memory,
 *    and so this function never moves a handle before locking it.
 *     See the discussion about handle locking above the function
 *    HLock().
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Parameters:
 *    
 *    h:
 *      the handle to move
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
procedure MoveHHi( h: Handle ); external name '_MoveHHi';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{$endc} {not TARGET_CPU_64}

{
 *  DisposePtr()
 *  
 *  Summary:
 *    Release memory occupied by a nonrelocatable block
 *  
 *  Discussion:
 *    When you no longer need a nonrelocatable block, call the
 *    DisposePtr function to free it for other uses.
 *    Call the function MemError to get the result code. See "Memory
 *    Manager Result Codes".
 *    After a call to DisposePtr, all pointers to the released block
 *    become invalid and should not be used again. Any subsequent use
 *    of a pointer to the released block might cause a system error. 
 *    You can pass the value NULL as the pointer to dispose.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.3
 *  
 *  Parameters:
 *    
 *    p:
 *      A pointer to the nonrelocatable block you want to dispose of
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
procedure DisposePtr( p: Ptr ); external name '_DisposePtr';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  GetPtrSize()
 *  
 *  Summary:
 *    Returns the logical size of the nonrelocatable block
 *    corresponding to a pointer.
 *  
 *  Discussion:
 *    This function returns the number of bytes used for the given
 *    pointer.  Call the function MemError to get the result code. See
 *    "Memory Manager Result Codes".
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.3
 *  
 *  Parameters:
 *    
 *    p:
 *      a pointer to a nonrelocatable block.
 *  
 *  Result:
 *    The logical size, in bytes, of the nonrelocatable block pointed
 *    to by p. In case of error, the function returns 0.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function GetPtrSize( p: Ptr ): Size; external name '_GetPtrSize';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  SetPtrSize()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.3
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
procedure SetPtrSize( p: Ptr; newSize: Size ); external name '_SetPtrSize';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  DisposeHandle()
 *  
 *  Summary:
 *    Releases memory occupied by a relocatable block.
 *  
 *  Discussion:
 *    The DisposeHandle function releases the memory occupied by the
 *    relocatable block whose handle is h. It also frees the handle’s
 *    master pointer for other uses.
 *    Do not use DisposeHandle to dispose of a handle obtained from the
 *    Resource Manager (for example, by a previous call to
 *    GetResource), useReleaseResource instead. If, however, you have
 *    called DetachResource on a resource handle, you should dispose of
 *    the storage by callingDisposeHandle.
 *    Call the function MemError to get the result code. See "Memory
 *    Manager Result Codes".
 *    After a call to DisposeHandle, all handles to the released block
 *    become invalid and should not be used again. Any subsequent calls
 *    to DisposeHandleusing an invalid handle might damage the master
 *    pointer list.  You can pass the value NULL as the handle to
 *    dispose.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.3
 *  
 *  Parameters:
 *    
 *    h:
 *      A handle to a relocatable block.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
procedure DisposeHandle( h: Handle ); external name '_DisposeHandle';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  SetHandleSize()
 *  
 *  Summary:
 *    Changes the logical size of the relocatable block corresponding
 *    to the specified handle.
 *  
 *  Discussion:
 *    Change the logical size of the relocatable block corresponding to
 *    the specified handle. SetHandleSize might need to move the
 *    relocatable block to obtain enough space for the resized block.
 *    Thus, for best results you should unlock a block before resizing
 *    it.
 *    
 *    An attempt to increase the size of a locked block might fail,
 *    because of blocks above and below it that are either
 *    nonrelocatable or locked. You should be prepared for this
 *    possibility.
 *    
 *    Instead of using the SetHandleSize function to set the size of a
 *    handle to 0, you can use the EmptyHandle function.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.3
 *  
 *  Parameters:
 *    
 *    h:
 *      a handle to a relocatable block.
 *    
 *    newSize:
 *      the desired new logical size, in bytes, of the relocatable
 *      block.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
procedure SetHandleSize( h: Handle; newSize: Size ); external name '_SetHandleSize';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  GetHandleSize()
 *  
 *  Summary:
 *    Returns the logical size of the relocatable block corresponding
 *    to a handle.
 *  
 *  Discussion:
 *    Returns the logical size of the relocatable block corresponding
 *    to a handle.  Call the function MemError to get the result code.
 *    See "Memory Manager Result Codes".
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.3
 *  
 *  Parameters:
 *    
 *    h:
 *      a handle to a relocatable block.
 *  
 *  Result:
 *    The logical size, in bytes, of the relocatable block whose handle
 *    is h. In case of error, the function return 0.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function GetHandleSize( h: Handle ): Size; external name '_GetHandleSize';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  ReallocateHandle()
 *  
 *  Summary:
 *    Allocates a new relocatable block of a specified size and sets a
 *    handle’s master pointer to point to the new block.
 *  
 *  Discussion:
 *    Usually you use ReallocateHandle to reallocate space for a block
 *    that you have emptied. If the handle references an existing
 *    block, ReallocateHandle releases that block before creating a new
 *    one.
 *    
 *    To reallocate space for a resource that has been purged, you
 *    should call LoadResource, not ReallocateHandle. To resize
 *    relocatable blocks, you should call the SetHandleSize
 *    function.
 *    
 *    Call the function MemError to get the result code. See "Memory
 *    Manager Result Codes".
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.3
 *  
 *  Parameters:
 *    
 *    h:
 *      A handle to a relocatable block.
 *    
 *    byteCount:
 *      the desired new logical size (in bytes) of the relocatable
 *      block. The new block is unlocked and unpurgeable.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
procedure ReallocateHandle( h: Handle; byteCount: Size ); external name '_ReallocateHandle';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  EmptyHandle()
 *  
 *  Summary:
 *    Purges a relocatable block and sets the corresponding handle’s
 *    master pointer to NULL.
 *  
 *  Discussion:
 *    The EmptyHandle function purges the relocatable block whose
 *    handle is h and sets the handle’s master pointer to NULL. The
 *    EmptyHandle function allows you to free memory taken by a
 *    relocatable block without freeing the relocatable block’s master
 *    pointer for other uses. The block whose handle is h must be
 *    unlocked but need not be purgeable.
 *    
 *    Note that if there are multiple handles to the relocatable block,
 *    then calling the EmptyHandle function empties them all, because
 *    all of the handles share a common master pointer. When you later
 *    use ReallocateHandle to reallocate space for the block, the
 *    master pointer is updated, and all of the handles reference the
 *    new block correctly.
 *    
 *    To free the memory taken up by a relocatable block and release
 *    the block’s master pointer for other uses, use the DisposeHandle
 *    function.
 *    
 *    Call the function MemError to get the result code. See "Memory
 *    Manager Result Codes".
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.3
 *  
 *  Parameters:
 *    
 *    h:
 *      a handle to a relocatable block.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
procedure EmptyHandle( h: Handle ); external name '_EmptyHandle';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  HSetRBit()
 *  
 *  Summary:
 *    Set the "R" bit for the handle state.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.3
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
procedure HSetRBit( h: Handle ); external name '_HSetRBit';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  HClrRBit()
 *  
 *  Summary:
 *    Clear the "R" bit for the handle state.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.3
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
procedure HClrRBit( h: Handle ); external name '_HClrRBit';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  HGetState()
 *  
 *  Summary:
 *    Get the current state of the handle's locked, purgeable, and R
 *    bits
 *  
 *  Discussion:
 *    The HGetState function returns a signed byte (char) containing
 *    the flags of the master pointer for the given handle. You can
 *    save this byte, change the state of any of the flags using the
 *    functions described in this section, and then restore their
 *    original states by passing the byte to the HSetState
 *    function.
 *    
 *    You can use bit-manipulation functions on the returned signed
 *    byte to determine the value of a given attribute.
 *    Currently the following bits are used:
 *    kHandleIsResourceBit    - if set, handle is a resource
 *     kHandlePurgeableBit - if set, handle is purgeable
 *     kHandleLockedBit - if set, handle is locked
 *    On Mac OS X and later, heaps are never purged, so the purgeable
 *    bit is used but its setting is essentially ignored. Also, since
 *    heaps are never compacted, and therefore the only time a handle
 *    moves is when that handle is resized, the danger of using
 *    defererenced handles is lower and so handles likely do not need
 *    to be locked as often. Because of this, the state for a handle is
 *    less useful, and HGetState() and other functions is being
 *    deprecated.  If you define a macro named
 *    __MAC_OS_X_MEMORY_MANAGER_CLEAN__ in your sources before you
 *    include MacMemory.h, then HGetState() and several other functions
 *    will become empty operations, removing the overhead of a function
 *    call.
 *    
 *    However, some applications may depend on the state bits of a
 *    handle being correct or changing as functions like HLock(), etc.,
 *    are called.  Applications which rely on this can not use
 *    __MAC_OS_X_MEMORY_MANAGER_CLEAN__.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.3
 *  
 *  Parameters:
 *    
 *    h:
 *      the handle to get the state for
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function HGetState( h: Handle ): SInt8; external name '_HGetState';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  HSetState()
 *  
 *  Summary:
 *    Set the current state of the handle's locked, purgeable, and R
 *    bits
 *  
 *  Discussion:
 *    See the discussion about handle state and Mac OS X above the
 *    function HGetState().
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.3
 *  
 *  Parameters:
 *    
 *    h:
 *      the handle to set the state for
 *    
 *    flags:
 *      the flags to set for the handle
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
procedure HSetState( h: Handle; flags: SInt8 ); external name '_HSetState';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{****************************************************************************

    Beginning in Mac OS X Tiger, BlockMove, BlockMoveData, BlockMoveUncached,
    BlockMoveDataUncached, BlockZero, and BlockZeroUncached are inlined to a
    direct call to the posix memmove or bzero functions; this allows the 
    compiler to optimize in some cases.
    
    However, CarbonCore.framework still exports functions with these names,
    both so old code which linked them there will run and to support
    compilers which don't support inline function definitions.

    To use the exported version of BlockMove, #define NO_BLOCKMOVE_INLINE
    in your source code ( or prefix header file ) before including any headers
    which would include MacMemory.h.
    
****************************************************************************}

{$ifc not TARGET_CPU_64}
{
 *  BlockMove()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DriverServicesLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure BlockMove( srcPtr: {const} UnivPtr; destPtr: UnivPtr; byteCount: Size ); external name '_BlockMove';
{
 *  BlockMoveData()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DriverServicesLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure BlockMoveData( srcPtr: {const} UnivPtr; destPtr: UnivPtr; byteCount: Size ); external name '_BlockMoveData';
{
 *  BlockMoveUncached()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 8.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure BlockMoveUncached( srcPtr: {const} UnivPtr; destPtr: UnivPtr; byteCount: Size ); external name '_BlockMoveUncached';

{
 *  BlockMoveDataUncached()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 8.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure BlockMoveDataUncached( srcPtr: {const} UnivPtr; destPtr: UnivPtr; byteCount: Size ); external name '_BlockMoveDataUncached';

{
 *  BlockZero()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 8.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure BlockZero( destPtr: UnivPtr; byteCount: Size ); external name '_BlockZero';

{
 *  BlockZeroUncached()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 8.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure BlockZeroUncached( destPtr: UnivPtr; byteCount: Size ); external name '_BlockZeroUncached';
{$endc} {not TARGET_CPU_64}


{
 *  HandToHand()
 *  
 *  Summary:
 *    Copies all of the data from one relocatable block to a new
 *    relocatable block.
 *  
 *  Discussion:
 *    The HandToHand function attempts to copy the information in the
 *    relocatable block to which theHndl is a handle; if successful,
 *    HandToHand sets theHndlto a handle pointing to the new
 *    relocatable block.
 *    
 *    If successful in creating a new relocatable block, the HandToHand
 *    function does not duplicate the properties of the original block.
 *    The new block is unlocked, unpurgeable, and not a resource. Call
 *    HLock or HPurge to adjust the properties of the new
 *    block.
 *    
 *    To copy only part of a relocatable block into a new relocatable
 *    block, use the PtrToHand function. Before calling PtrToHand, lock
 *    and dereference the handle pointing to the relocatable block you
 *    want to copy.
 *    
 *    Because HandToHand replaces its parameter with the new handle,
 *    you should retain the original parameter value somewhere else,
 *    otherwise you will not be able to access it.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.3
 *  
 *  Parameters:
 *    
 *    theHndl:
 *      a handle to the relocatable block whose data HandToHand will
 *      copy.  On return, theHndl contains a handle to a new
 *      relocatable block whose data duplicates the original.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function HandToHand( var theHndl: Handle ): OSErr; external name '_HandToHand';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PtrToXHand()
 *  
 *  Summary:
 *    Copies data referenced by a pointer to an existing relocatable
 *    block.
 *  
 *  Discussion:
 *    The PtrToXHand function copies the specified number of bytes from
 *    the location specified by srcPtr to the handle specified by
 *    dstHndl.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.3
 *  
 *  Parameters:
 *    
 *    srcPtr:
 *      the address of the first byte to copy.
 *    
 *    dstHndl:
 *      a handle to an existing relocatable block.
 *    
 *    size:
 *      the number of bytes to copy.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PtrToXHand( srcPtr: {const} UnivPtr; dstHndl: Handle; size: SIGNEDLONG ): OSErr; external name '_PtrToXHand';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PtrToHand()
 *  
 *  Summary:
 *    Copies data referenced by a pointer to a new relocatable block.
 *  
 *  Discussion:
 *    If you dereference and lock a handle, the PtrToHand function can
 *    copy its data to a new handle. However, for copying data from one
 *    handle to another, the HandToHand function is more efficient.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.3
 *  
 *  Parameters:
 *    
 *    srcPtr:
 *      the address of the first byte to copy.
 *    
 *    dstHndl:
 *      a handle for which you have not yet allocated any memory. The
 *      PtrToHand function allocates memory for the handle and copies
 *      the specified number of bytes beginning at srcPtr into it. The
 *      dstHndl parameter is an output parameter that will hold the
 *      result. Its value on entry is ignored. If no error occurs, on
 *      exit it points to an unlocked, non-purgeable Handle of the
 *      requested size.
 *    
 *    size:
 *      the number of bytes to copy.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PtrToHand( srcPtr: {const} UnivPtr; var dstHndl: Handle; size: SIGNEDLONG ): OSErr; external name '_PtrToHand';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  HandAndHand()
 *  
 *  Summary:
 *    Use the HandAndHand function to concatenate two relocatable
 *    blocks.
 *  
 *  Discussion:
 *    The HandAndHand function concatenates the information from the
 *    relocatable block to which aHndl is a handle onto the end of the
 *    relocatable block to which bHndl is a handle. The aHndl variable
 *    remains unchanged.
 *    
 *    WARNING
 *    
 *    The HandAndHand function dereferences the handle aHndl. You must
 *    call the HLock procedure to lock the block before calling
 *    HandAndHand. Afterward, you can call the HUnlock procedure to
 *    unlock it. Alternatively, you can save the block's original state
 *    by calling the HGetState function, lock the block by calling
 *    HLock, and then restore the original settings by calling
 *    HSetState.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.3
 *  
 *  Parameters:
 *    
 *    hand1:
 *      A handle to the first relocatable block, whose contents do not
 *      change but are concatenated to the end of the second
 *      relocatable block.
 *    
 *    hand2:
 *      A handle to the second relocatable block, whose size the Memory
 *      Manager expands so that it can concatenate the information from
 *      aHndl to the end of the contents of this block.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function HandAndHand( hand1: Handle; hand2: Handle ): OSErr; external name '_HandAndHand';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PtrAndHand()
 *  
 *  Summary:
 *    Concatenates part or all of a memory block to the end of a
 *    relocatable block.
 *  
 *  Discussion:
 *    The PtrAndHand function takes the number of bytes specified by
 *    the size parameter, beginning at the location specified by ptr1,
 *    and concatenates them onto the end of the relocatable block to
 *    which hand2 is a handle. The contents of the source block remain
 *    unchanged.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.3
 *  
 *  Parameters:
 *    
 *    ptr1:
 *      a pointer to the beginning of the data that the Memory Manager
 *      is to concatenate onto the end of the relocatable block.
 *    
 *    hand2:
 *      a handle to the relocatable block, whose size the Memory
 *      Manager expands so that it can concatenate the information from
 *      ptr1 onto the end of this block.
 *    
 *    size:
 *      the number of bytes of the block referenced by ptr1 to copy.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PtrAndHand( ptr1: {const} UnivPtr; hand2: Handle; size: SIGNEDLONG ): OSErr; external name '_PtrAndHand';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{$ifc not TARGET_CPU_64}
{
 *  MoreMasters()   *** DEPRECATED ***
 *  
 *  Discussion:
 *    On Mac OS X and later, master pointers do not need to be
 *    pre-allocated.
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
procedure MoreMasters; external name '_MoreMasters';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{
 *  MoreMasterPointers()   *** DEPRECATED ***
 *  
 *  Discussion:
 *    On Mac OS X and later, master pointers do not need to be
 *    pre-allocated.
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Parameters:
 *    
 *    inCount:
 *      the number of master pointers to preallocate
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   not available
 }
procedure MoreMasterPointers( inCount: UInt32 ); external name '_MoreMasterPointers';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{  Temporary Memory routines renamed, but obsolete, in System 7.0 and later.  }
{
 *  TempHLock()   *** DEPRECATED ***
 *  
 *  Discussion:
 *    This function has been deprecated for many years; replace it with
 *    HLock()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
procedure TempHLock( h: Handle; var resultCode: OSErr ); external name '_TempHLock';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{
 *  TempHUnlock()   *** DEPRECATED ***
 *  
 *  Discussion:
 *    This function has been deprecated for many years; replace it with
 *    HUnlock()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
procedure TempHUnlock( h: Handle; var resultCode: OSErr ); external name '_TempHUnlock';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{
 *  TempDisposeHandle()   *** DEPRECATED ***
 *  
 *  Discussion:
 *    This function has been deprecated for many years; replace it with
 *    DisposeHandle()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
procedure TempDisposeHandle( h: Handle; var resultCode: OSErr ); external name '_TempDisposeHandle';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_5, __IPHONE_NA, __IPHONE_NA) *)


{
 *  TempTopMem()   *** DEPRECATED ***
 *  
 *  Discussion:
 *    Mac OS X and later does not have a seperate temporary memory
 *    heap.  This function returns NULL.
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function TempTopMem: Ptr; external name '_TempTopMem';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{
 *  HoldMemory()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    Mac OS X has never supported HoldMemory.  The functions in
 *    ./sys/mman.h may be useful for replacing usage of these
 *    functions, although Mac OS X does not allow the same level of
 *    control over whether pages are held in memory or resident as Mac
 *    OS 9.x did.
 *    If you define a macro __MAC_OS_X_MEMORY_MANAGER_CLEAN__ in your
 *    sources before you include MacMemory.h, then any calls to
 *    HoldMemory() in your program will essentially be removed.
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function HoldMemory( address: UnivPtr; count: UNSIGNEDLONG ): OSErr; external name '_HoldMemory';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{
 *  UnholdMemory()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    Mac OS X has never supported MakeMemoryResident.  See the comment
 *    above UnholdMemory for more information.
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function UnholdMemory( address: UnivPtr; count: UNSIGNEDLONG ): OSErr; external name '_UnholdMemory';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{
 *  MakeMemoryResident()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    Mac OS X has never supported MakeMemoryResident.  See the comment
 *    above UnholdMemory for more information.
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Parameters:
 *    
 *    address:
 *      the address to make resident
 *    
 *    count:
 *      the count of pages to make make resident
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Non-Carbon CFM:   in InterfaceLib 8.5 and later
 }
function MakeMemoryResident( address: UnivPtr; count: UNSIGNEDLONG ): OSErr; external name '_MakeMemoryResident';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{
 *  ReleaseMemoryData()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    Mac OS X has never supported MakeMemoryResident.  See the comment
 *    above UnholdMemory for more information.
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Parameters:
 *    
 *    address:
 *      the address to make release
 *    
 *    count:
 *      the count of pages to make make release
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Non-Carbon CFM:   in InterfaceLib 8.5 and later
 }
function ReleaseMemoryData( address: UnivPtr; count: UNSIGNEDLONG ): OSErr; external name '_ReleaseMemoryData';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{
 *  MakeMemoryNonResident()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    Mac OS X has never supported MakeMemoryResident.  See the comment
 *    above UnholdMemory for more information.
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Parameters:
 *    
 *    address:
 *      the address to make non-resident
 *    
 *    count:
 *      the count of pages to make make non-resident
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Non-Carbon CFM:   in InterfaceLib 8.5 and later
 }
function MakeMemoryNonResident( address: UnivPtr; count: UNSIGNEDLONG ): OSErr; external name '_MakeMemoryNonResident';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{
 *  FlushMemory()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    Mac OS X has never supported MakeMemoryResident.  See the comment
 *    above UnholdMemory for more information.
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Parameters:
 *    
 *    address:
 *      the address to flush
 *    
 *    count:
 *      the count of pages to make flush
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Non-Carbon CFM:   in InterfaceLib 8.5 and later
 }
function FlushMemory( address: UnivPtr; count: UNSIGNEDLONG ): OSErr; external name '_FlushMemory';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{
 *  GZSaveHnd()   *** DEPRECATED ***
 *  
 *  Summary:
 *    This function is deprecated on Mac OS X and always returns NULL.
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function GZSaveHnd: Handle; external name '_GZSaveHnd';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{
 *  TopMem()   *** DEPRECATED ***
 *  
 *  Summary:
 *    This function is deprecated on Mac OS X and always returns NULL.
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function TopMem: Ptr; external name '_TopMem';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{
 *  ReserveMem()   *** DEPRECATED ***
 *  
 *  Summary:
 *    This function is deprecated on Mac OS X.
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
procedure ReserveMem( cbNeeded: Size ); external name '_ReserveMem';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PurgeSpace()   *** DEPRECATED ***
 *  
 *  Summary:
 *    This function is deprecated on Mac OS X.
 *  
 *  Discussion:
 *    On Mac OS X and later, heaps are never purged and therefore
 *    PurgeSpace will always return a large value for both the total
 *    space available and the largest block available.  You can assume
 *    that any reasonable memory allocation will succeed.
 *    
 *    If you define a macro __MAC_OS_X_MEMORY_MANAGER_CLEAN__ in your
 *    sources before you include MacMemory.h, then any calls to
 *    PurgeSpace() in your program will essentially be removed.
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
procedure PurgeSpace( var total: SIGNEDLONG; var contig: SIGNEDLONG ); external name '_PurgeSpace';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PurgeSpaceTotal()   *** DEPRECATED ***
 *  
 *  Summary:
 *    This function is deprecated on Mac OS X.
 *  
 *  Discussion:
 *    On Mac OS X and later, heaps are never purged and therefore
 *    PurgeSpaceTotal will always return a large value. You can assume
 *    that any reasonable memory allocation will succeed.
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 8.5 and later
 }
function PurgeSpaceTotal: SIGNEDLONG; external name '_PurgeSpaceTotal';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PurgeSpaceContiguous()   *** DEPRECATED ***
 *  
 *  Summary:
 *    This function is deprecated on Mac OS X.
 *  
 *  Discussion:
 *    On Mac OS X and later, heaps are never purged and therefore
 *    PurgeSpaceContiguous will always return a large value. You can
 *    assume that any reasonable memory allocation will succeed.
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 8.5 and later
 }
function PurgeSpaceContiguous: SIGNEDLONG; external name '_PurgeSpaceContiguous';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ Carbon routines to aid in debugging. }
{
 *  CheckAllHeaps()   *** DEPRECATED ***
 *  
 *  Summary:
 *    Check all known heaps for validity.  Deprecated on Mac OS X,
 *    since there really aren't heaps.  Use IsHeapValid() if you really
 *    want this functionality.
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   not available
 }
function CheckAllHeaps: Boolean; external name '_CheckAllHeaps';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{$endc} {not TARGET_CPU_64}

{
 *  IsHeapValid()
 *  
 *  Summary:
 *    Check if the heap is valid.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.4
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   not available
 }
function IsHeapValid: Boolean; external name '_IsHeapValid';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{ It is invalid to pass a NULL or an empty Handle to IsHandleValid }
{
 *  IsHandleValid()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.3
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   not available
 }
function IsHandleValid( h: Handle ): Boolean; external name '_IsHandleValid';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{ It is invalid to pass a NULL Pointer to IsPointerValid }
{
 *  IsPointerValid()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.3
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   not available
 }
function IsPointerValid( p: Ptr ): Boolean; external name '_IsPointerValid';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)



{$ifc not TARGET_CPU_64}
{
 *  LMGetSysZone()   *** DEPRECATED ***
 *  
 *  Summary:
 *    This function is deprecated on Mac OS X.
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function LMGetSysZone: THz; external name '_LMGetSysZone';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{
 *  LMSetSysZone()   *** DEPRECATED ***
 *  
 *  Summary:
 *    This function is deprecated on Mac OS X.
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
procedure LMSetSysZone( value: THz ); external name '_LMSetSysZone';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{
 *  LMGetApplZone()   *** DEPRECATED ***
 *  
 *  Summary:
 *    This function is deprecated on Mac OS X.
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function LMGetApplZone: THz; external name '_LMGetApplZone';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{
 *  LMSetApplZone()   *** DEPRECATED ***
 *  
 *  Summary:
 *    This function is deprecated on Mac OS X.
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
procedure LMSetApplZone( value: THz ); external name '_LMSetApplZone';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{$endc} {not TARGET_CPU_64}

{$endc} {TARGET_OS_MAC}
{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}

end.
{$endc} {not MACOSALLINCLUDE}
