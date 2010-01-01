{
     File:       CarbonCore/MachineExceptions.h
 
     Contains:   Processor Exception Handling Interfaces.
 
     Version:    CarbonCore-859.2~1
 
     Copyright:  © 1993-2008 by Apple Computer, Inc., all rights reserved.
 
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

unit MachineExceptions;
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

{$ALIGN POWER}

{ Some basic declarations used throughout the kernel }
type
	AreaID = ^SInt32; { an opaque type }
	AreaIDPtr = ^AreaID;
{ Machine Dependent types for PowerPC: }

{ Because a number of sources do a #define CR 13 and this file contains a struct member named CR,
 * an obscure compilation error gets spit out.  Rename the field to CRRegister.   }

type
	MachineInformationPowerPCPtr = ^MachineInformationPowerPC;
	MachineInformationPowerPC = record
		CTR: UnsignedWide;
		LR: UnsignedWide;
		PC: UnsignedWide;
		CRRegister: UNSIGNEDLONG;             {    changed from CR since some folks had a #define CR  13 in their source code}
		XER: UNSIGNEDLONG;
		MSR: UNSIGNEDLONG;
		MQ: UNSIGNEDLONG;
		ExceptKind: UNSIGNEDLONG;
		DSISR: UNSIGNEDLONG;
		DAR: UnsignedWide;
		Reserved: UnsignedWide;
	end;
type
	RegisterInformationPowerPCPtr = ^RegisterInformationPowerPC;
	RegisterInformationPowerPC = record
		R0: UnsignedWide;
		R1: UnsignedWide;
		R2: UnsignedWide;
		R3: UnsignedWide;
		R4: UnsignedWide;
		R5: UnsignedWide;
		R6: UnsignedWide;
		R7: UnsignedWide;
		R8: UnsignedWide;
		R9: UnsignedWide;
		R10: UnsignedWide;
		R11: UnsignedWide;
		R12: UnsignedWide;
		R13: UnsignedWide;
		R14: UnsignedWide;
		R15: UnsignedWide;
		R16: UnsignedWide;
		R17: UnsignedWide;
		R18: UnsignedWide;
		R19: UnsignedWide;
		R20: UnsignedWide;
		R21: UnsignedWide;
		R22: UnsignedWide;
		R23: UnsignedWide;
		R24: UnsignedWide;
		R25: UnsignedWide;
		R26: UnsignedWide;
		R27: UnsignedWide;
		R28: UnsignedWide;
		R29: UnsignedWide;
		R30: UnsignedWide;
		R31: UnsignedWide;
	end;
type
	FPUInformationPowerPCPtr = ^FPUInformationPowerPC;
	FPUInformationPowerPC = record
		Registers: array [0..31] of UnsignedWide;
		FPSCR: UNSIGNEDLONG;
		Reserved: UNSIGNEDLONG;
	end;
type
	Vector128Ptr = ^Vector128;
	Vector128 = record
		case SInt16 of
		0: (
			l: array [0..3] of UInt32;
			);
		1: (
			s: array [0..7] of UInt16;
			);
		2: (
			c: packed array [0..15] of UInt8;
			);
	end;
type
	VectorInformationPowerPCPtr = ^VectorInformationPowerPC;
	VectorInformationPowerPC = record
		Registers: array [0..31] of Vector128;
		VSCR: Vector128;
		VRsave: UInt32;
	end;
{ Exception related declarations }
const
	kWriteReference = 0;
	kReadReference = 1;
	kFetchReference = 2;
	writeReference = kWriteReference; { Obsolete name}
	readReference = kReadReference; { Obsolete name}
	fetchReference = kFetchReference; { Obsolete name}


type
	MemoryReferenceKind = UNSIGNEDLONG;
	MemoryExceptionInformationPtr = ^MemoryExceptionInformation;
	MemoryExceptionInformation = record
		theArea: AreaID;                { The area related to the execption, same as MPAreaID.}
		theAddress: LogicalAddress;             { The 32-bit address of the exception.}
		theError: OSStatus;               { See enum below.}
		theReference: MemoryReferenceKind;          { read, write, instruction fetch.}
	end;
const
	kUnknownException = 0;
	kIllegalInstructionException = 1;
	kTrapException = 2;
	kAccessException = 3;
	kUnmappedMemoryException = 4;
	kExcludedMemoryException = 5;
	kReadOnlyMemoryException = 6;
	kUnresolvablePageFaultException = 7;
	kPrivilegeViolationException = 8;
	kTraceException = 9;
	kInstructionBreakpointException = 10; { Optional}
	kDataBreakpointException = 11;   { Optional}
	kIntegerException = 12;
	kFloatingPointException = 13;
	kStackOverflowException = 14;   { Optional, may be implemented as kAccessException on some systems.}
	kTaskTerminationException = 15;   { Obsolete}
	kTaskCreationException = 16;   { Obsolete}
	kDataAlignmentException = 17;    { May occur when a task is in little endian mode or created with kMPTaskTakesAllExceptions.}

{$ifc OLDROUTINENAMES}
const
	unknownException = kUnknownException; { Obsolete name}
	illegalInstructionException = kIllegalInstructionException; { Obsolete name}
	trapException = kTrapException; { Obsolete name}
	accessException = kAccessException; { Obsolete name}
	unmappedMemoryException = kUnmappedMemoryException; { Obsolete name}
	excludedMemoryException = kExcludedMemoryException; { Obsolete name}
	readOnlyMemoryException = kReadOnlyMemoryException; { Obsolete name}
	unresolvablePageFaultException = kUnresolvablePageFaultException; { Obsolete name}
	privilegeViolationException = kPrivilegeViolationException; { Obsolete name}
	traceException = kTraceException; { Obsolete name}
	instructionBreakpointException = kInstructionBreakpointException; { Obsolete name}
	dataBreakpointException = kDataBreakpointException; { Obsolete name}
	integerException = kIntegerException; { Obsolete name}
	floatingPointException = kFloatingPointException; { Obsolete name}
	stackOverflowException = kStackOverflowException; { Obsolete name}
	terminationException = kTaskTerminationException; { Obsolete name}
	kTerminationException = kTaskTerminationException; { Obsolete name}

{$endc}  {OLDROUTINENAMES}


type
	ExceptionKind = UNSIGNEDLONG;
	ExceptionInfoPtr = ^ExceptionInfo;
	ExceptionInfo = record
		case SInt16 of
		0: (
			memoryInfo: MemoryExceptionInformationPtr;
			);
	end;
type
	ExceptionInformationPowerPCPtr = ^ExceptionInformationPowerPC;
	ExceptionInformationPowerPC = record
		theKind: ExceptionKind;
		machineState: MachineInformationPowerPCPtr;
		registerImage: RegisterInformationPowerPCPtr;
		FPUImage: FPUInformationPowerPCPtr;
		info: ExceptionInfo;
		vectorImage: VectorInformationPowerPCPtr;
	end;
{$ifc TARGET_CPU_PPC or TARGET_CPU_PPC64}
type
	ExceptionInformation = ExceptionInformationPowerPC;
	MachineInformation = MachineInformationPowerPC;
	RegisterInformation = RegisterInformationPowerPC;
	FPUInformation = FPUInformationPowerPC;
	VectorInformation = VectorInformationPowerPC;
	ExceptionInformationPtr = ^ExceptionInformation;
	MachineInformationPtr = ^MachineInformation;
	RegisterInformationPtr = ^RegisterInformation;
	FPUInformationPtr = ^FPUInformation;
	VectorInformationPtr = ^VectorInformation;
{$endc}

{$ifc TARGET_CPU_X86 or TARGET_CPU_X86_64}
type
  Vector128intel = record
		case SInt16 of
{ requires vector support
		0: (
			s: single_128_bit_vector
			);
}
		1: (
			si: array [0..3] of UInt32;
			);
		2: (
			s: array [0..1] of Float64;
			);
		3: (
			c: packed array [0..15] of UInt8;
			);
	end;
{$endc}  { TARGET_CPU_X86 or TARGET_CPU_X86_64 }

{$ifc TARGET_CPU_X86}
type
	MachineInformationIntelPtr = ^MachineInformationIntel;
	MachineInformationIntel = record
		CS: UNSIGNEDLONG;
		DS: UNSIGNEDLONG;
		SS: UNSIGNEDLONG;
		ES: UNSIGNEDLONG;
		FS: UNSIGNEDLONG;
		GS: UNSIGNEDLONG;
		EFLAGS: UNSIGNEDLONG;
		EIP: UNSIGNEDLONG;
		ExceptTrap: UNSIGNEDLONG;
		ExceptErr: UNSIGNEDLONG;
		ExceptAddr: UNSIGNEDLONG;
	end;
type
	RegisterInformationIntelPtr = ^RegisterInformationIntel;
	RegisterInformationIntel = record
		EAX: UNSIGNEDLONG;
		EBX: UNSIGNEDLONG;
		ECX: UNSIGNEDLONG;
		EDX: UNSIGNEDLONG;
		ESI: UNSIGNEDLONG;
		EDI: UNSIGNEDLONG;
		EBP: UNSIGNEDLONG;
		ESP: UNSIGNEDLONG;
	end;

type
	FPRegIntel = packed array[0..9] of UInt8;
type
	FPUInformationIntel = record
		Registers: array[0..7] of FPRegIntel;
		Control: UInt16;
		Status: UInt16;
		Tag: UInt16;
		Opcode: UInt16;
		EIP: UInt32;
		DP: UInt32;
		DS: UInt32;
	end;

type
	VectorInformationIntel = record
		Registers: array[0..7] of Vector128Intel;
	end;

type
	MachineInformationPtr = ^MachineInformation;
	RegisterInformationPtr = ^RegisterInformation;
	FPUInformationPtr = ^FPUInformation;
	VectorInformationPtr = ^VectorInformation;
	MachineInformation = MachineInformationIntel;
	RegisterInformation = RegisterInformationIntel;
	FPUInformation = FPUInformationIntel;
	VectorInformation = VectorInformationIntel;
{$endc}  { TARGET_CPU_X86 }

{$ifc TARGET_CPU_X86_64}
type
	MachineInformationIntel64 = record
		CS: UNSIGNEDLONG;
		FS: UNSIGNEDLONG;
		GS: UNSIGNEDLONG;
		RFLAGS: UNSIGNEDLONG;
		RIP: UNSIGNEDLONG;
		ExceptTrap: UNSIGNEDLONG;
		ExceptErr: UNSIGNEDLONG;
		ExceptAddr: UNSIGNEDLONG;
	end;
type
	RegisterInformationIntel64 = record
		RAX: UNSIGNEDLONG;
		RBX: UNSIGNEDLONG;
		RCX: UNSIGNEDLONG;
		RDX: UNSIGNEDLONG;
		RDI: UNSIGNEDLONG;
		RSI: UNSIGNEDLONG;
		RBP: UNSIGNEDLONG;
		RSP: UNSIGNEDLONG;
		R8: UNSIGNEDLONG;
		R9: UNSIGNEDLONG;
		R10: UNSIGNEDLONG;
		R11: UNSIGNEDLONG;
		R12: UNSIGNEDLONG;
		R13: UNSIGNEDLONG;
		R14: UNSIGNEDLONG;
		R15: UNSIGNEDLONG;
	end;
type
	FPRegIntel = packed array[0..9] of UInt8;
type
	FPUInformationIntel64 = record
		Registers: array[0..7] of FPRegIntel;
		Control: UInt16;
		Status: UInt16;
		Tag: UInt16;
		Opcode: UInt16;
		IP: UInt32;
		DP: UInt32;
		DS: UInt32;
	end;
type
	VectorInformationIntel64 = record
		Registers: array[0..15] of Vector128Intel;
	end;

type
	MachineInformationPtr = ^MachineInformation;
	RegisterInformationPtr = ^RegisterInformation;
	FPUInformationPtr = ^FPUInformation;
	VectorInformationPtr = ^VectorInformation;
	MachineInformation = MachineInformationIntel64;
	RegisterInformation = RegisterInformationIntel64;
	FPUInformation = FPUInformationIntel64;
	VectorInformation = VectorInformationIntel64;
{$endc}  { TARGET_CPU_X86_64 }

{$ifc TARGET_CPU_X86 or TARGET_CPU_X86_64}
type
	ExceptionInformationPtr = ^ExceptionInformation;
	ExceptionInformation = record
		theKind: ExceptionKind;
		machineState: MachineInformationPtr;
		registerImage: RegisterInformationPtr;
		FPUImage: FPUInformationPtr;
		info: ExceptionInfo;
		vectorImage: VectorInformationPtr;
	end;
{$endc} { TARGET_CPU_X86 || TARGET_CPU_X86_64 }

{ 
    Note:   An ExceptionHandler is NOT a UniversalProcPtr, except in Carbon.
            It must be a PowerPC function pointer with NO routine descriptor, 
            except on Carbon, where it must be a UniversalProcPtr (TPP actually)
            to allow the interface to work from both CFM and Mach-O.
}
type
	ExceptionHandlerProcPtr = function( var theException: ExceptionInformation ): OSStatus;
	ExceptionHandlerUPP = ExceptionHandlerProcPtr;

{
 *  NewExceptionHandlerUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function NewExceptionHandlerUPP( userRoutine: ExceptionHandlerProcPtr ): ExceptionHandlerUPP; external name '_NewExceptionHandlerUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  DisposeExceptionHandlerUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure DisposeExceptionHandlerUPP( userUPP: ExceptionHandlerUPP ); external name '_DisposeExceptionHandlerUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  InvokeExceptionHandlerUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function InvokeExceptionHandlerUPP( var theException: ExceptionInformation; userUPP: ExceptionHandlerUPP ): OSStatus; external name '_InvokeExceptionHandlerUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
   ExceptionHandler function pointers (TPP):
   on classic PowerPC, use raw function pointers
   on classic PowerPC with OPAQUE_UPP_TYPES=1, use UPP's
   on Carbon, use UPP's
}
{ use UPP's}

type
	ExceptionHandlerTPP = ExceptionHandlerUPP;
	ExceptionHandler = ExceptionHandlerTPP;
{ Routine for installing per-process exception handlers }
{
 *  InstallExceptionHandler()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function InstallExceptionHandler( theHandler: ExceptionHandlerTPP ): ExceptionHandlerTPP; external name '_InstallExceptionHandler';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{$endc} {TARGET_OS_MAC}
{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}

end.
{$endc} {not MACOSALLINCLUDE}
