{
     File:       CarbonCore/MachineExceptions.h
 
     Contains:   Processor Exception Handling Interfaces.
 
     Version:    CarbonCore-654.0.85~1
 
     Copyright:  © 1993-2005 by Apple Computer, Inc., all rights reserved.
 
     Bugs?:      For bug reports, consult the following page on
                 the World Wide Web:
 
                     http://www.freepascal.org/bugs.html
 
}
{      Pascal Translation Updated:  Peter N Lewis, <peter@stairways.com.au>, November 2005 }
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

unit MachineExceptions;
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
uses MacTypes,MixedMode;


{$ALIGN POWER}

{ Some basic declarations used throughout the kernel }
type
	AreaID = ^SInt32; { an opaque 32-bit type }
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
		CRRegister: UInt32;             {    changed from CR since some folks had a #define CR  13 in their source code}
		XER: UInt32;
		MSR: UInt32;
		MQ: UInt32;
		ExceptKind: UInt32;
		DSISR: UInt32;
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
		FPSCR: UInt32;
		Reserved: UInt32;
	end;
type
	Vector128Ptr = ^Vector128;
	Vector128 = record
		case SInt16 of
		0: (
			l:					array [0..3] of UInt32;
			);
		1: (
			s:					array [0..7] of UInt16;
			);
		2: (
			c:					packed array [0..15] of UInt8;
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
	MemoryReferenceKind = UInt32;
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
	ExceptionKind = UInt32;
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
{$ifc TARGET_CPU_PPC OR TARGET_CPU_68K}
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

{$ifc TARGET_CPU_X86}
type
	MachineInformationIntelPtr = ^MachineInformationIntel;
	MachineInformationIntel = record
		CS: UInt16;
		DS: UInt16;
		SS: UInt16;
		ES: UInt16;
		FS: UInt16;
		GS: UInt16;
		EFLAGS: UInt32;
		EIP: UInt32;
	end;
type
	RegisterInformationIntelPtr = ^RegisterInformationIntel;
	RegisterInformationIntel = record
		EAX: UInt32;
		EBX: UInt32;
		ECX: UInt32;
		EDX: UInt32;
		ESI: UInt32;
		EDI: UInt32;
		EBP: UInt32;
		ESP: UInt32;
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
		Registers: array[0..7] of UnsignedWide;
	end;

type
	ExceptionInformationPtr = ^ExceptionInformation;
	MachineInformationPtr = ^MachineInformation;
	RegisterInformationPtr = ^RegisterInformation;
	FPUInformationPtr = ^FPUInformation;
	VectorInformationPtr = ^VectorInformation;
	MachineInformation = MachineInformationIntel;
	RegisterInformation = RegisterInformationIntel;
	FPUInformation = FPUInformationIntel;
	VectorInformation = VectorInformationIntel;
	ExceptionInformation = record
		theKind: ExceptionKind;
		machineState: MachineInformationPtr;
		registerImage: RegisterInformationPtr;
		FPUImage: FPUInformationPtr;
		info: ExceptionInfo;
		vectorImage: VectorInformationPtr;
	end;
{$endc}

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
function InvokeExceptionHandlerUPP( var theException: ExceptionInformation; userRoutine: ExceptionHandlerUPP ): OSStatus; external name '_InvokeExceptionHandlerUPP';
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




end.
