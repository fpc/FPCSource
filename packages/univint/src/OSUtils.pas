{
     File:       CarbonCore/OSUtils.h
 
     Contains:   OS Utilities Interfaces.
 
     Version:    CarbonCore-654.0.85~1
 
     Copyright:  © 1985-2005 by Apple Computer, Inc., all rights reserved
 
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

unit OSUtils;
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
uses MacTypes,CFBase,MixedMode,MacMemory,DateTimeUtils,CFString,Endian;
{$ALIGN POWER}


{  HandToHand and other memory utilties were moved to MacMemory.h }

{  Date and Time utilties were moved to DateTimeUtils.h }


{$ALIGN MAC68K}

const
	useFree = 0;
	useATalk = 1;
	useAsync = 2;
	useExtClk = 3;    {Externally clocked}
	useMIDI = 4;

const
	false32b = 0;    {24 bit addressing error}
	true32b = 1;     {32 bit addressing error}

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
type
	SysParmTypePtr = ^SysParmType;
	SysParmType = packed record
		valid: UInt8;
		aTalkA: UInt8;
		aTalkB: UInt8;
		config: UInt8;
		portA: SInt16;
		portB: SInt16;
		alarm: SInt32;
		font: SInt16;
		kbdPrint: SInt16;
		volClik: SInt16;
		misc: SInt16;
	end;
type
	SysPPtr = SysParmTypePtr;

type
	QElemPtr = ^QElem;
	QElem = record
		qLink: QElemPtr;
		qType: SInt16;
		qData: array [0..0] of SInt16;
	end;
type
	QHdrPtr = ^QHdr;
	QHdr = record
		qFlags: SInt16;
		qHead: QElemPtr;
		qTail: QElemPtr;
	end;
type
	DeferredTaskProcPtr = procedure( dtParam: SInt32 );
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
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  DisposeDeferredTaskUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure DisposeDeferredTaskUPP( userUPP: DeferredTaskUPP ); external name '_DisposeDeferredTaskUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  InvokeDeferredTaskUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure InvokeDeferredTaskUPP( dtParam: SInt32; userUPP: DeferredTaskUPP ); external name '_InvokeDeferredTaskUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

type
	DeferredTask = record
		qLink: QElemPtr;
		qType: SInt16;
		dtFlags: SInt16;
		dtAddr: DeferredTaskUPP;
		dtParam: SInt32;
		dtReserved: SInt32;
	end;
	DeferredTaskPtr = ^DeferredTask;
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
			dlsDelta: SInt8;									{  signed byte; daylight savings delta  }
			);
{$endc}
		1: (
			gmtDelta: SInt32;								{  use low 24-bits only  }
			);
		2: (
{$ifc TARGET_RT_LITTLE_ENDIAN}
			pad0,pad1,pad2: SInt8;
{$endc}
			Delta: SInt8;
			);
	end;
{
 *  IsMetric()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function IsMetric: Boolean; external name '_IsMetric';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  GetSysPPtr()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    Don't use this function; it always returns NULL on Mac OS X.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function GetSysPPtr: SysPPtr; external name '_GetSysPPtr';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4 *)


{
    NOTE: SysBeep() has been moved to Sound.h.  
          We could not automatically #include Sound.h in this file
          because Sound.h indirectly #include's OSUtils.h which
          would make a circular include.
}
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
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function DTInstall( dtTaskPtr: DeferredTaskPtr ): OSErr; external name '_DTInstall';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4 *)


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
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework but deprecated in 10.4
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function DTUninstall( dtTaskPtr: DeferredTaskPtr ): OSErr; external name '_DTUninstall';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4 *)


function GetMMUMode: SInt8; inline;
procedure SwapMMUMode( var mode: SInt8 ); inline;
{
 *  Delay()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
procedure Delay( numTicks: UInt32; var finalTicks: UInt32 ); external name '_Delay';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  WriteParam()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    This function no longer does anything on Mac OS X; you should
 *    remove all calls to it from your code.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function WriteParam: OSErr; external name '_WriteParam';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4 *)


{
 *  Enqueue()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
procedure Enqueue( qElement: QElemPtr; qHeader: QHdrPtr ); external name '_Enqueue';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  Dequeue()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function Dequeue( qElement: QElemPtr; qHeader: QHdrPtr ): OSErr; external name '_Dequeue';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  SetCurrentA5()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    You no longer need to use SetCurrentA5() on Mac OS X.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function SetCurrentA5: SInt32; external name '_SetCurrentA5';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4 *)


{
 *  SetA5()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    You no longer need to use SetA5() on Mac OS X.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function SetA5( newA5: SInt32 ): SInt32; external name '_SetA5';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4 *)


{
 *  InitUtil()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework but deprecated in 10.3
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function InitUtil: OSErr; external name '_InitUtil';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_3 *)


{
 *  MakeDataExecutable()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.5 and later
 }
procedure MakeDataExecutable( baseAddress: UnivPtr; length: UInt32 ); external name '_MakeDataExecutable';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  ReadLocation()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
procedure ReadLocation( var loc: MachineLocation ); external name '_ReadLocation';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  WriteLocation()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework but deprecated in 10.0
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
procedure WriteLocation( const (*var*) loc: MachineLocation ); external name '_WriteLocation';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED *)


{
 *  TickCount()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function TickCount: UInt32; external name '_TickCount';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  CSCopyUserName()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.5 and later
 *    Non-Carbon CFM:   not available
 }
function CSCopyUserName( useShortName: Boolean ): CFStringRef; external name '_CSCopyUserName';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  CSCopyMachineName()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.5 and later
 *    Non-Carbon CFM:   not available
 }
function CSCopyMachineName: CFStringRef; external name '_CSCopyMachineName';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
    NOTE: SysEnvirons is obsolete.  You should be using Gestalt.
}
{ Environs Equates }
const
	curSysEnvVers = 2;     {Updated to equal latest SysEnvirons version}

type
	SysEnvRecPtr = ^SysEnvRec;
	SysEnvRec = record
		environsVersion: SInt16;
		machineType: SInt16;
		systemVersion: SInt16;
		processor: SInt16;
		hasFPU: Boolean;
		hasColorQD: Boolean;
		keyBoardType: SInt16;
		atDrvrVersNum: SInt16;
		sysVRefNum: SInt16;
	end;
const
{ Machine Types }
	envMac = -1;
	envXL = -2;
	envMachUnknown = 0;
	env512KE = 1;
	envMacPlus = 2;
	envSE = 3;
	envMacII = 4;
	envMacIIx = 5;
	envMacIIcx = 6;
	envSE30 = 7;
	envPortable = 8;
	envMacIIci = 9;
	envMacIIfx = 11;

const
{ CPU types }
	envCPUUnknown = 0;
	env68000 = 1;
	env68010 = 2;
	env68020 = 3;
	env68030 = 4;
	env68040 = 5;

const
{ Keyboard types }
	envUnknownKbd = 0;
	envMacKbd = 1;
	envMacAndPad = 2;
	envMacPlusKbd = 3;
	envAExtendKbd = 4;
	envStandADBKbd = 5;
	envPrtblADBKbd = 6;
	envPrtblISOKbd = 7;
	envStdISOADBKbd = 8;
	envExtISOADBKbd = 9;

implementation


{$R-}

function GetMMUMode: SInt8; inline;
begin
	GetMMUMode:= true32b
end;

procedure SwapMMUMode( var mode: SInt8 ); inline;
begin
	mode := true32b;
end;

end.
