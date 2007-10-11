{
     File:       MacMemory.p
 
     Contains:   Memory Manager Interfaces.
 
     Version:    Technology: Mac OS 9
                 Release:    Universal Interfaces 3.4.2
 
     Copyright:  © 1985-2002 by Apple Computer, Inc., all rights reserved
 
     Bugs?:      For bug reports, consult the following page on
                 the World Wide Web:
 
                     http://www.freepascal.org/bugs.html
 
}


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

unit MacMemory;
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
uses MacTypes,MixedMode;


{$ALIGN MAC68K}


const
	maxSize						= $7FFFFFF0;					{ the largest block possible }

	defaultPhysicalEntryCount	= 8;

																{  values returned from the GetPageState function  }
	kPageInMemory				= 0;
	kPageOnDisk					= 1;
	kNotPaged					= 2;

																{  masks for Zone->heapType field  }
	k32BitHeap					= 1;							{  valid in all Memory Managers  }
	kNewStyleHeap				= 2;							{  true if new Heap Manager is present  }
	kNewDebugHeap				= 4;							{  true if new Heap Manager is running in debug mode on this heap  }


	{  bits for use with HGetState/HSetState }
	kHandleIsResourceBit		= 5;
	kHandlePurgeableBit			= 6;
	kHandleLockedBit			= 7;

	{  masks for use with HGetState/HSetState }
	kHandleIsResourceMask		= $20;
	kHandlePurgeableMask		= $40;
	kHandleLockedMask			= $80;


type
{$ifc TYPED_FUNCTION_POINTERS}
	GrowZoneProcPtr = function(cbNeeded: Size): SInt32;
{$elsec}
	GrowZoneProcPtr = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	PurgeProcPtr = procedure(blockToPurge: Handle);
{$elsec}
	PurgeProcPtr = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	UserFnProcPtr = procedure(parameter: UnivPtr);
{$elsec}
	UserFnProcPtr = Register68kProcPtr;
{$endc}

{$ifc OPAQUE_UPP_TYPES}
	GrowZoneUPP = ^SInt32; { an opaque UPP }
{$elsec}
	GrowZoneUPP = UniversalProcPtr;
{$endc}	
{$ifc OPAQUE_UPP_TYPES}
	PurgeUPP = ^SInt32; { an opaque UPP }
{$elsec}
	PurgeUPP = UniversalProcPtr;
{$endc}	
{$ifc OPAQUE_UPP_TYPES}
	UserFnUPP = ^SInt32; { an opaque UPP }
{$elsec}
	UserFnUPP = UniversalProcPtr;
{$endc}	
	ZonePtr = ^Zone;
	Zone = record
		bkLim:					Ptr;
		purgePtr:				Ptr;
		hFstFree:				Ptr;
		zcbFree:				SInt32;
		gzProc:					GrowZoneUPP;
		moreMast:				SInt16;
		flags:					SInt16;
		cntRel:					SInt16;
		maxRel:					SInt16;
		cntNRel:				SInt16;
		heapType:				SInt8;									{  previously "maxNRel", now holds flags (e.g. k32BitHeap) }
		unused:					SInt8;
		cntEmpty:				SInt16;
		cntHandles:				SInt16;
		minCBFree:				SInt32;
		purgeProc:				PurgeUPP;
		sparePtr:				Ptr;
		allocPtr:				Ptr;
		heapData:				SInt16;
	end;

	THz									= ^Zone;
	THzPtr								= ^THz;
	MemoryBlockPtr = ^MemoryBlock;
	MemoryBlock = record
		address:				Ptr;
		count:					UInt32;
	end;

	LogicalToPhysicalTablePtr = ^LogicalToPhysicalTable;
	LogicalToPhysicalTable = record
		logical:				MemoryBlock;
		physical:				array [0..7] of MemoryBlock;
	end;

	PageState							= SInt16;
	StatusRegisterContents				= SInt16;

const
	kVolumeVirtualMemoryInfoVersion1 = 1;						{  first version of VolumeVirtualMemoryInfo }


type
	VolumeVirtualMemoryInfoPtr = ^VolumeVirtualMemoryInfo;
	VolumeVirtualMemoryInfo = record
		version:				PBVersion;								{  Input: Version of the VolumeVirtualMemoryInfo structure }
		volumeRefNum:			SInt16;									{  Input: volume reference number }
		inUse:					boolean;								{  output: true if volume is currently used for file mapping }
		_fill:					SInt8;
		vmOptions:				UInt32;									{  output: tells what volume can support (same as DriverGestaltVMOptionsResponse vmOptions bits in DriverGestalt) }
																		{  end of kVolumeVirtualMemoryInfoVersion1 structure }
	end;


const
	uppGrowZoneProcInfo = $000000F0;
	uppPurgeProcInfo = $000000C0;
	uppUserFnProcInfo = $00009802;
	{
	 *  NewGrowZoneUPP()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   available as macro/inline
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
function NewGrowZoneUPP(userRoutine: GrowZoneProcPtr): GrowZoneUPP; external name '_NewGrowZoneUPP'; { old name was NewGrowZoneProc }
{
 *  NewPurgeUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewPurgeUPP(userRoutine: PurgeProcPtr): PurgeUPP; external name '_NewPurgeUPP'; { old name was NewPurgeProc }
{
 *  NewUserFnUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewUserFnUPP(userRoutine: UserFnProcPtr): UserFnUPP; external name '_NewUserFnUPP'; { old name was NewUserFnProc }
{
 *  DisposeGrowZoneUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeGrowZoneUPP(userUPP: GrowZoneUPP); external name '_DisposeGrowZoneUPP';
{
 *  DisposePurgeUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposePurgeUPP(userUPP: PurgeUPP); external name '_DisposePurgeUPP';
{
 *  DisposeUserFnUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeUserFnUPP(userUPP: UserFnUPP); external name '_DisposeUserFnUPP';
{
 *  InvokeGrowZoneUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function InvokeGrowZoneUPP(cbNeeded: Size; userRoutine: GrowZoneUPP): SInt32; external name '_InvokeGrowZoneUPP'; { old name was CallGrowZoneProc }
{
 *  InvokePurgeUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure InvokePurgeUPP(blockToPurge: Handle; userRoutine: PurgeUPP); external name '_InvokePurgeUPP'; { old name was CallPurgeProc }
{
 *  InvokeUserFnUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure InvokeUserFnUPP(parameter: UnivPtr; userRoutine: UserFnUPP); external name '_InvokeUserFnUPP'; { old name was CallUserFnProc }
{$ifc CALL_NOT_IN_CARBON}
{
 *  GetApplLimit()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function GetApplLimit: Ptr; external name '_GetApplLimit';
{
 *  SystemZone()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function SystemZone: THz; external name '_SystemZone';
{
 *  ApplicationZone()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function ApplicationZone: THz; external name '_ApplicationZone';
{$endc}  {CALL_NOT_IN_CARBON}

{
 *  GZSaveHnd()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function GZSaveHnd: Handle; external name '_GZSaveHnd';
{
 *  TopMem()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function TopMem: Ptr; external name '_TopMem';
{
 *  MemError()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function MemError: OSErr; external name '_MemError';
{$ifc CALL_NOT_IN_CARBON}
{
 *  GetZone()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function GetZone: THz; external name '_GetZone';
{$endc}  {CALL_NOT_IN_CARBON}

{
 *  NewHandle()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewHandle(byteCount: Size): Handle; external name '_NewHandle';
{$ifc CALL_NOT_IN_CARBON}
{
 *  NewHandleSys()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function NewHandleSys(byteCount: Size): Handle; external name '_NewHandleSys';
{$endc}  {CALL_NOT_IN_CARBON}

{
 *  NewHandleClear()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewHandleClear(byteCount: Size): Handle; external name '_NewHandleClear';
{$ifc CALL_NOT_IN_CARBON}
{
 *  NewHandleSysClear()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function NewHandleSysClear(byteCount: Size): Handle; external name '_NewHandleSysClear';
{
 *  HandleZone()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function HandleZone(h: Handle): THz; external name '_HandleZone';
{$endc}  {CALL_NOT_IN_CARBON}

{
 *  RecoverHandle()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function RecoverHandle(p: Ptr): Handle; external name '_RecoverHandle';
{$ifc CALL_NOT_IN_CARBON}
{
 *  RecoverHandleSys()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function RecoverHandleSys(p: Ptr): Handle; external name '_RecoverHandleSys';
{$endc}  {CALL_NOT_IN_CARBON}

{
 *  NewPtr()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewPtr(byteCount: Size): Ptr; external name '_NewPtr';
{$ifc CALL_NOT_IN_CARBON}
{
 *  NewPtrSys()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function NewPtrSys(byteCount: Size): Ptr; external name '_NewPtrSys';
{$endc}  {CALL_NOT_IN_CARBON}

{
 *  NewPtrClear()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewPtrClear(byteCount: Size): Ptr; external name '_NewPtrClear';
{$ifc CALL_NOT_IN_CARBON}
{
 *  NewPtrSysClear()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function NewPtrSysClear(byteCount: Size): Ptr; external name '_NewPtrSysClear';
{
 *  PtrZone()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PtrZone(p: Ptr): THz; external name '_PtrZone';
{$endc}  {CALL_NOT_IN_CARBON}

{
 *  MaxBlock()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function MaxBlock: SInt32; external name '_MaxBlock';
{$ifc CALL_NOT_IN_CARBON}
{
 *  MaxBlockSys()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function MaxBlockSys: SInt32; external name '_MaxBlockSys';
{$endc}  {CALL_NOT_IN_CARBON}

{
 *  StackSpace()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function StackSpace: SInt32; external name '_StackSpace';
{
 *  NewEmptyHandle()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewEmptyHandle: Handle; external name '_NewEmptyHandle';
{$ifc CALL_NOT_IN_CARBON}
{
 *  NewEmptyHandleSys()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function NewEmptyHandleSys: Handle; external name '_NewEmptyHandleSys';
{$endc}  {CALL_NOT_IN_CARBON}

{
 *  HLock()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure HLock(h: Handle); external name '_HLock';
{
 *  HUnlock()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure HUnlock(h: Handle); external name '_HUnlock';
{
 *  HPurge()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure HPurge(h: Handle); external name '_HPurge';
{
 *  HNoPurge()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure HNoPurge(h: Handle); external name '_HNoPurge';
{
 *  HLockHi()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure HLockHi(h: Handle); external name '_HLockHi';
{
 *  TempNewHandle()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function TempNewHandle(logicalSize: Size; var resultCode: OSErr): Handle; external name '_TempNewHandle';
{
 *  TempMaxMem()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function TempMaxMem(var grow: Size): Size; external name '_TempMaxMem';
{
 *  TempFreeMem()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function TempFreeMem: SInt32; external name '_TempFreeMem';
{$ifc CALL_NOT_IN_CARBON}
{
 *  InitZone()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure InitZone(pgrowZone: GrowZoneUPP; cmoreMasters: SInt16; limitPtr: UnivPtr; startPtr: UnivPtr); external name '_InitZone';

{$endc}  {CALL_NOT_IN_CARBON}

{$ifc CALL_NOT_IN_CARBON}
{
 *  SetZone()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure SetZone(hz: THz); external name '_SetZone';
{$endc}  {CALL_NOT_IN_CARBON}

{
 *  CompactMem()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function CompactMem(cbNeeded: Size): Size; external name '_CompactMem';
{$ifc CALL_NOT_IN_CARBON}
{
 *  CompactMemSys()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function CompactMemSys(cbNeeded: Size): Size; external name '_CompactMemSys';
{$endc}  {CALL_NOT_IN_CARBON}

{
 *  PurgeMem()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure PurgeMem(cbNeeded: Size); external name '_PurgeMem';
{$ifc CALL_NOT_IN_CARBON}
{
 *  PurgeMemSys()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure PurgeMemSys(cbNeeded: Size); external name '_PurgeMemSys';
{$endc}  {CALL_NOT_IN_CARBON}

{
 *  FreeMem()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later

 *  Note:
      FreeMem has been renamed MacFreeMem, to resolve a naming conflict with
      FreeMem in the Turbo Pascal/Delphi/FreePascal runtime library
 }
function MacFreeMem: SInt32; external name '_FreeMem';
{$ifc CALL_NOT_IN_CARBON}
{
 *  FreeMemSys()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function FreeMemSys: SInt32; external name '_FreeMemSys';
{$endc}  {CALL_NOT_IN_CARBON}

{
 *  ReserveMem()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure ReserveMem(cbNeeded: Size); external name '_ReserveMem';
{$ifc CALL_NOT_IN_CARBON}
{
 *  ReserveMemSys()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure ReserveMemSys(cbNeeded: Size); external name '_ReserveMemSys';
{$endc}  {CALL_NOT_IN_CARBON}

{
 *  MaxMem()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function MaxMem(var grow: Size): Size; external name '_MaxMem';
{$ifc CALL_NOT_IN_CARBON}
{
 *  MaxMemSys()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function MaxMemSys(var grow: Size): Size; external name '_MaxMemSys';
{$endc}  {CALL_NOT_IN_CARBON}

{
 *  SetGrowZone()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure SetGrowZone(growZone: GrowZoneUPP); external name '_SetGrowZone';
{
 *  GetGrowZone()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function GetGrowZone: GrowZoneUPP; external name '_GetGrowZone';

{
 *  MoveHHi()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure MoveHHi(h: Handle); external name '_MoveHHi';
{
 *  DisposePtr()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposePtr(p: Ptr); external name '_DisposePtr';
{
 *  GetPtrSize()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function GetPtrSize(p: Ptr): Size; external name '_GetPtrSize';

{
 *  SetPtrSize()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure SetPtrSize(p: Ptr; newSize: Size); external name '_SetPtrSize';
{
 *  DisposeHandle()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeHandle(h: Handle); external name '_DisposeHandle';
{
 *  SetHandleSize()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure SetHandleSize(h: Handle; newSize: Size); external name '_SetHandleSize';
{ 
    NOTE
    
    GetHandleSize and GetPtrSize are documented in Inside Mac as returning 0 
    in case of an error, but the traps actually return an error code in D0.
    The glue sets D0 to 0 if an error occurred.
}
{
 *  GetHandleSize()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function GetHandleSize(h: Handle): Size; external name '_GetHandleSize';

{$ifc CALL_NOT_IN_CARBON}
{
 *  InlineGetHandleSize()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function InlineGetHandleSize(h: Handle): Size; external name '_InlineGetHandleSize';
{$endc}  {CALL_NOT_IN_CARBON}

{
 *  ReallocateHandle()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure ReallocateHandle(h: Handle; byteCount: Size); external name '_ReallocateHandle';
{$ifc CALL_NOT_IN_CARBON}
{
 *  ReallocateHandleSys()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 8.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure ReallocateHandleSys(h: Handle; byteCount: Size); external name '_ReallocateHandleSys';
{$endc}  {CALL_NOT_IN_CARBON}

{
 *  EmptyHandle()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure EmptyHandle(h: Handle); external name '_EmptyHandle';
{
 *  HSetRBit()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure HSetRBit(h: Handle); external name '_HSetRBit';
{
 *  HClrRBit()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure HClrRBit(h: Handle); external name '_HClrRBit';
{
 *  HGetState()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function HGetState(h: Handle): SInt8; external name '_HGetState';
{
 *  HSetState()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure HSetState(h: Handle; flags: SInt8); external name '_HSetState';
{
 *  PurgeSpace()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure PurgeSpace(var total: SInt32; var contig: SInt32); external name '_PurgeSpace';

{
    PurgeSpaceTotal and PurgeSpaceContiguous are currently only implement
    on classic 68K.  The are the same as PurgeSpace() but return just
    one value (either total space purgable or contiguous space purgable).
    Begining in Mac OS 8.5 they are available in InterfaceLib.
}
{
 *  PurgeSpaceTotal()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 8.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PurgeSpaceTotal: SInt32; external name '_PurgeSpaceTotal';
{
 *  PurgeSpaceContiguous()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 8.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PurgeSpaceContiguous: SInt32; external name '_PurgeSpaceContiguous';
{$ifc CALL_NOT_IN_CARBON}
{
 *  PurgeSpaceSysTotal()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 8.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PurgeSpaceSysTotal: SInt32; external name '_PurgeSpaceSysTotal';
{
 *  PurgeSpaceSysContiguous()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 8.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PurgeSpaceSysContiguous: SInt32; external name '_PurgeSpaceSysContiguous';
{****************************************************************************

    The routines: 

        BlockMoveUncached, BlockMoveDataUncached
        BlockZero, BlockZeroUncached
    
    were first created for developers writing drivers. Originally they only
    existed in DriverServicesLib.  Later they were added to InterfaceLib 
    in PCI based PowerMacs.  MacOS 8.5 provides these routines in InterfaceLib
    on all supported machines. 
    
****************************************************************************}
{$endc}  {CALL_NOT_IN_CARBON}

{
 *  BlockMove()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DriverServicesLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure BlockMove(srcPtr: UnivPtr; destPtr: UnivPtr; byteCount: Size); external name '_BlockMove';
{
 *  BlockMoveData()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DriverServicesLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure BlockMoveData(srcPtr: UnivPtr; destPtr: UnivPtr; byteCount: Size); external name '_BlockMoveData';
{
 *  BlockMoveUncached()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 8.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure BlockMoveUncached(srcPtr: UnivPtr; destPtr: UnivPtr; byteCount: Size); external name '_BlockMoveUncached';

{
 *  BlockMoveDataUncached()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 8.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure BlockMoveDataUncached(srcPtr: UnivPtr; destPtr: UnivPtr; byteCount: Size); external name '_BlockMoveDataUncached';

{
 *  BlockZero()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 8.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure BlockZero(destPtr: UnivPtr; byteCount: Size); external name '_BlockZero';

{
 *  BlockZeroUncached()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 8.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure BlockZeroUncached(destPtr: UnivPtr; byteCount: Size); external name '_BlockZeroUncached';


{$ifc CALL_NOT_IN_CARBON}
{
 *  MaxApplZone()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure MaxApplZone; external name '_MaxApplZone';
{
 *  SetApplBase()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure SetApplBase(startPtr: UnivPtr); external name '_SetApplBase';
{$endc}  {CALL_NOT_IN_CARBON}

{
 *  MoreMasters()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure MoreMasters; external name '_MoreMasters';
{
 *  MoreMasterPointers()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure MoreMasterPointers(inCount: UInt32); external name '_MoreMasterPointers';

{$ifc CALL_NOT_IN_CARBON}
{
 *  SetApplLimit()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure SetApplLimit(zoneLimit: UnivPtr); external name '_SetApplLimit';
{
 *  InitApplZone()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure InitApplZone; external name '_InitApplZone';
{  Temporary Memory routines renamed, but obsolete, in System 7.0 and later.  }
{$endc}  {CALL_NOT_IN_CARBON}

{
 *  TempHLock()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure TempHLock(h: Handle; var resultCode: OSErr); external name '_TempHLock';
{
 *  TempHUnlock()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure TempHUnlock(h: Handle; var resultCode: OSErr); external name '_TempHUnlock';
{
 *  TempDisposeHandle()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure TempDisposeHandle(h: Handle; var resultCode: OSErr); external name '_TempDisposeHandle';
{
 *  TempTopMem()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function TempTopMem: Ptr; external name '_TempTopMem';
{
 *  HoldMemory()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function HoldMemory(address: UnivPtr; count: UInt32): OSErr; external name '_HoldMemory';
{
 *  UnholdMemory()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function UnholdMemory(address: UnivPtr; count: UInt32): OSErr; external name '_UnholdMemory';
{$ifc CALL_NOT_IN_CARBON}
{
 *  LockMemory()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LockMemory(address: UnivPtr; count: UInt32): OSErr; external name '_LockMemory';
{
 *  LockMemoryForOutput()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 8.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LockMemoryForOutput(address: UnivPtr; count: UInt32): OSErr; external name '_LockMemoryForOutput';
{
 *  LockMemoryContiguous()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LockMemoryContiguous(address: UnivPtr; count: UInt32): OSErr; external name '_LockMemoryContiguous';
{
 *  UnlockMemory()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function UnlockMemory(address: UnivPtr; count: UInt32): OSErr; external name '_UnlockMemory';
{$endc}  {CALL_NOT_IN_CARBON}

{
 *  MakeMemoryResident()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 8.5 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 }
function MakeMemoryResident(address: UnivPtr; count: UInt32): OSErr; external name '_MakeMemoryResident';
{
 *  ReleaseMemoryData()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 8.5 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ReleaseMemoryData(address: UnivPtr; count: UInt32): OSErr; external name '_ReleaseMemoryData';
{
 *  MakeMemoryNonResident()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 8.5 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 }
function MakeMemoryNonResident(address: UnivPtr; count: UInt32): OSErr; external name '_MakeMemoryNonResident';
{
 *  FlushMemory()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 8.5 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FlushMemory(address: UnivPtr; count: UInt32): OSErr; external name '_FlushMemory';
{$ifc CALL_NOT_IN_CARBON}
{
 *  GetPhysical()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function GetPhysical(var addresses: LogicalToPhysicalTable; var physicalEntryCount: UInt32): OSErr; external name '_GetPhysical';
{$endc}  {CALL_NOT_IN_CARBON}

{$ifc CALL_NOT_IN_CARBON}
{
 *  GetVolumeVirtualMemoryInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function GetVolumeVirtualMemoryInfo(volVMInfo: VolumeVirtualMemoryInfoPtr): OSErr; external name '_GetVolumeVirtualMemoryInfo';
{
 *  DeferUserFn()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function DeferUserFn(userFunction: UserFnUPP; argument: UnivPtr): OSErr; external name '_DeferUserFn';
{
 *  DebuggerGetMax()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function DebuggerGetMax: SInt32; external name '_DebuggerGetMax';
{
 *  DebuggerEnter()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure DebuggerEnter; external name '_DebuggerEnter';
{
 *  DebuggerExit()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure DebuggerExit; external name '_DebuggerExit';
{
 *  DebuggerPoll()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure DebuggerPoll; external name '_DebuggerPoll';
{
 *  GetPageState()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function GetPageState(address: UnivPtr): PageState; external name '_GetPageState';
{
 *  PageFaultFatal()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PageFaultFatal: boolean; external name '_PageFaultFatal';
{
 *  DebuggerLockMemory()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function DebuggerLockMemory(address: UnivPtr; count: UInt32): OSErr; external name '_DebuggerLockMemory';
{
 *  DebuggerUnlockMemory()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function DebuggerUnlockMemory(address: UnivPtr; count: UInt32): OSErr; external name '_DebuggerUnlockMemory';
{
 *  EnterSupervisorMode()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function EnterSupervisorMode: StatusRegisterContents; external name '_EnterSupervisorMode';
{
 *  StripAddress()
 *  
 *  Summary:
 *    A trap on classic 68K, and the identity function for pre-carbon
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function StripAddress(theAddress: UnivPtr): Ptr; external name '_StripAddress';
{
 *  Translate24To32()
 *  
 *  Summary:
 *    A trap on classic 68K, and the identity function for pre-carbon
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function Translate24To32(addr24: UnivPtr): Ptr; external name '_Translate24To32';
{$endc}  {CALL_NOT_IN_CARBON}

{
 *  HandToHand()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function HandToHand(var theHndl: Handle): OSErr; external name '_HandToHand';

{
 *  PtrToXHand()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PtrToXHand(srcPtr: UnivPtr; dstHndl: Handle; size: SInt32): OSErr; external name '_PtrToXHand';
{
 *  PtrToHand()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PtrToHand(srcPtr: UnivPtr; var dstHndl: Handle; size: SInt32): OSErr; external name '_PtrToHand';

{
 *  HandAndHand()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function HandAndHand(hand1: Handle; hand2: Handle): OSErr; external name '_HandAndHand';
{
 *  PtrAndHand()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PtrAndHand(ptr1: UnivPtr; hand2: Handle; size: SInt32): OSErr; external name '_PtrAndHand';
{ Carbon routines to aid in debugging. }
{ Checks all applicable heaps for validity }
{
 *  CheckAllHeaps()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function CheckAllHeaps: boolean; external name '_CheckAllHeaps';

{ Checks the application heap for validity }
{
 *  IsHeapValid()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function IsHeapValid: boolean; external name '_IsHeapValid';

{ It is invalid to pass a NULL or an empty Handle to IsHandleValid }
{
 *  IsHandleValid()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function IsHandleValid(h: Handle): boolean; external name '_IsHandleValid';

{ It is invalid to pass a NULL Pointer to IsPointerValid }
{
 *  IsPointerValid()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function IsPointerValid(p: Ptr): boolean; external name '_IsPointerValid';


{$ifc OLDROUTINENAMES}
{$ifc CALL_NOT_IN_CARBON}
{
 *  ApplicZone()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function ApplicZone: THz; external name '_ApplicZone';
{
 *  MFTempNewHandle()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function MFTempNewHandle(logicalSize: Size; var resultCode: OSErr): Handle; external name '_MFTempNewHandle';
{
 *  MFMaxMem()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function MFMaxMem(var grow: Size): Size; external name '_MFMaxMem';
{
 *  MFFreeMem()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function MFFreeMem: SInt32; external name '_MFFreeMem';
{
 *  MFTempHLock()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure MFTempHLock(h: Handle; var resultCode: OSErr); external name '_MFTempHLock';
{
 *  MFTempHUnlock()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure MFTempHUnlock(h: Handle; var resultCode: OSErr); external name '_MFTempHUnlock';
{
 *  MFTempDisposHandle()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure MFTempDisposHandle(h: Handle; var resultCode: OSErr); external name '_MFTempDisposHandle';
{
 *  MFTopMem()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function MFTopMem: Ptr; external name '_MFTopMem';
{
 *  ResrvMem()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure ResrvMem(cbNeeded: Size); external name '_ResrvMem';
{
 *  DisposPtr()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure DisposPtr(p: Ptr); external name '_DisposPtr';
{
 *  DisposHandle()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure DisposHandle(h: Handle); external name '_DisposHandle';
{
 *  ReallocHandle()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure ReallocHandle(h: Handle; byteCount: Size); external name '_ReallocHandle';
{$endc}  {CALL_NOT_IN_CARBON}
{$endc}  {OLDROUTINENAMES}

{$ALIGN MAC68K}


end.
