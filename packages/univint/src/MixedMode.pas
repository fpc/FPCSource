{
     File:       CarbonCore/MixedMode.h
 
     Contains:   Mixed Mode Manager Interfaces.
 
     Version:    CarbonCore-859.2~1
 
     Copyright:  © 1992-2008 by Apple Computer, Inc., all rights reserved.
 
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

unit MixedMode;
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



{$ALIGN MAC68K}

{
    ===========================================================================
    
    Notes on Mixed Mode and Mac OS X
    
    Mac OS X doesn't have mixed mode, nor the Mixed Mode Manager, because
    everything running is PowerPC code.  Therefore, there is little need for
    anything in this file unless the code still needs to run on Mac OS 9.x
    CarbonLib, and on Mac OS X.

    =========================================================================== 
}
{ Mixed Mode constants }
{ Current Routine Descriptor Version }
const
	kRoutineDescriptorVersion = 7;

{ MixedModeMagic Magic Cookie/Trap number }
const
	_MixedModeMagic = $AAFE;

{ MixedModeState Version for CFM68K Mixed Mode }
const
	kCurrentMixedModeStateRecord = 1;

{ Calling Conventions }
type
	CallingConventionType = UInt16;
const
	kPascalStackBased = 0;
	kCStackBased = 1;
	kRegisterBased = 2;
	kD0DispatchedPascalStackBased = 8;
	kD1DispatchedPascalStackBased = 12;
	kD0DispatchedCStackBased = 9;
	kStackDispatchedPascalStackBased = 14;
	kThinkCStackBased = 5;

{ ISA Types }
type
	ISAType = SInt8;
const
	kM68kISA = 0;
	kPowerPCISA = 1;

const
	kX86ISA = 2;

{ RTA Types }
type
	RTAType = SInt8;
const
	kOld68kRTA = 0 shl 4;
	kPowerPCRTA = 0 shl 4;
	kCFM68kRTA = 1 shl 4;

const
	kX86RTA = 2 shl 4;


const
{$ifc TARGET_OS_MAC and not TARGET_CPU_64}
  {$ifc TARGET_CPU_PPC}
    GetCurrentISA = kPowerPCISA;
    GetCurrentRTA = kPowerPCRTA;
  {$elsec}
    {$ifc TARGET_CPU_X86}
      GetCurrentISA = kX86ISA;
      GetCurrentRTA = kX86RTA;
    {$endc}
  {$endc}
    GetCurrentArchitecture = GetCurrentISA or GetCurrentRTA;
{$elsec}
  GetCurrentArchitecture = 0;
{$endc}

{ Constants for specifing 68k registers }
const
	kRegisterD0 = 0;
	kRegisterD1 = 1;
	kRegisterD2 = 2;
	kRegisterD3 = 3;
	kRegisterD4 = 8;
	kRegisterD5 = 9;
	kRegisterD6 = 10;
	kRegisterD7 = 11;
	kRegisterA0 = 4;
	kRegisterA1 = 5;
	kRegisterA2 = 6;
	kRegisterA3 = 7;
	kRegisterA4 = 12;
	kRegisterA5 = 13;
	kRegisterA6 = 14;   { A7 is the same as the PowerPC SP }
	kCCRegisterCBit = 16;
	kCCRegisterVBit = 17;
	kCCRegisterZBit = 18;
	kCCRegisterNBit = 19;
	kCCRegisterXBit = 20;

type
	registerSelectorType = UInt16;
{ SizeCodes we use everywhere }
const
	kNoByteCode = 0;
	kOneByteCode = 1;
	kTwoByteCode = 2;
	kFourByteCode = 3;

{ Mixed Mode Routine Records }
type
	ProcInfoType = UNSIGNEDLONG;
{ Routine Flag Bits }
type
	RoutineFlagsType = UInt16;
const
	kProcDescriptorIsAbsolute = $00;
	kProcDescriptorIsRelative = $01;

const
	kFragmentIsPrepared = $00;
	kFragmentNeedsPreparing = $02;

const
	kUseCurrentISA = $00;
	kUseNativeISA = $04;

const
	kPassSelector = $00;
	kDontPassSelector = $08;

const
	kRoutineIsNotDispatchedDefaultRoutine = $00;
	kRoutineIsDispatchedDefaultRoutine = $10;

const
	kProcDescriptorIsProcPtr = $00;
	kProcDescriptorIsIndex = $20;

type
	RoutineRecord = record
		procInfo: ProcInfoType;               { calling conventions }
		reserved1: SInt8;              { Must be 0 }
		ISA: ISAType;                    { Instruction Set Architecture }
		routineFlags: RoutineFlagsType;           { Flags for each routine }
		procDescriptor: ProcPtr;         { Where is the thing we’re calling? }
		reserved2: UInt32;              { Must be 0 }
		selector: UInt32;               { For dispatched routines, the selector }
	end;
	RoutineRecordPtr = ^RoutineRecord;
	RoutineRecordHandle = ^RoutineRecordPtr;
{ Mixed Mode Routine Descriptors }
{ Definitions of the Routine Descriptor Flag Bits }
type
	RDFlagsType = UInt8;
const
	kSelectorsAreNotIndexable = $00;
	kSelectorsAreIndexable = $01;

{ Routine Descriptor Structure }
type
	RoutineDescriptor = record
		goMixedModeTrap: UInt16;        { Our A-Trap }
		version: SInt8;                { Current Routine Descriptor version }
		routineDescriptorFlags: RDFlagsType; { Routine Descriptor Flags }
		reserved1: UInt32;              { Unused, must be zero }
		reserved2: UInt8;              { Unused, must be zero }
		selectorInfo: UInt8;           { If a dispatched routine, calling convention, else 0 }
		routineCount: UInt16;           { Number of routines in this RD }
		routineRecords: array [0..0] of RoutineRecord;      { The individual routines }
	end;
	RoutineDescriptorPtr = ^RoutineDescriptor;
	RoutineDescriptorHandle= ^RoutineDescriptorPtr;
{ 68K MixedModeStateRecord }
type
	MixedModeStateRecordPtr = ^MixedModeStateRecord;
	MixedModeStateRecord = record
		state1: UInt32;
		state2: UInt32;
		state3: UInt32;
		state4: UInt32;
	end;
{
 *  NewRoutineDescriptor()   *** DEPRECATED ***
 *  
 *  Discussion:
 *    This function is deprecated on Mac OS X and in CarbonLib because
 *    routine descriptors existed to allow 68k code and PowerPC code to
 *    call each other and get parameter marshalling and other OS
 *    services.  Mac OS X is entirely PowerPC native, and does not
 *    require the use of mixed mode.
 *    You should remove any calls to NewRoutineDescriptor() from your
 *    sources, and replace them with theProc parameter itself.
 *  
 *  Availability:
 *    Mac OS X:         not available but deprecated in 10.4
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }


{
 *  DisposeRoutineDescriptor()   *** DEPRECATED ***
 *  
 *  Discussion:
 *    DisposeRoutineDescriptor() is deprecated on Mac OS X. 
 *    RoutineDescriptors are no longer used.
 *    You should delete any calls to DisposeRoutineDescriptor() from
 *    your sources.
 *  
 *  Availability:
 *    Mac OS X:         not available but deprecated in 10.4
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }

function NewRoutineDescriptor( theProc: ProcPtr; theProcInfo: ProcInfoType; theISA: ISAType ): UniversalProcPtr; inline;
procedure DisposeRoutineDescriptor( theUPP: UniversalProcPtr ); inline;

{ Mixed Mode ProcInfos }
const
{ Calling Convention Offsets }
	kCallingConventionWidth = 4;
	kCallingConventionPhase = 0;
	kCallingConventionMask = $0F; { Result Offsets }
	kResultSizeWidth = 2;
	kResultSizePhase = kCallingConventionWidth;
	kResultSizeMask = $30; { Parameter offsets & widths }
	kStackParameterWidth = 2;
	kStackParameterPhase = kCallingConventionWidth + kResultSizeWidth;
	kStackParameterMask = $FFFFFFC0; { Register Result Location offsets & widths }
	kRegisterResultLocationWidth = 5;
	kRegisterResultLocationPhase = kCallingConventionWidth + kResultSizeWidth; { Register Parameter offsets & widths }
	kRegisterParameterWidth = 5;
	kRegisterParameterPhase = kCallingConventionWidth + kResultSizeWidth + kRegisterResultLocationWidth;
	kRegisterParameterMask = $7FFFF800;
	kRegisterParameterSizePhase = 0;
	kRegisterParameterSizeWidth = 2;
	kRegisterParameterWhichPhase = kRegisterParameterSizeWidth;
	kRegisterParameterWhichWidth = 3;    { Dispatched Stack Routine Selector offsets & widths }
	kDispatchedSelectorSizeWidth = 2;
	kDispatchedSelectorSizePhase = kCallingConventionWidth + kResultSizeWidth; { Dispatched Stack Routine Parameter offsets }
	kDispatchedParameterPhase = kCallingConventionWidth + kResultSizeWidth + kDispatchedSelectorSizeWidth; { Special Case offsets & widths }
	kSpecialCaseSelectorWidth = 6;
	kSpecialCaseSelectorPhase = kCallingConventionWidth;
	kSpecialCaseSelectorMask = $03F0;

const
	kSpecialCase = $000F; { (CallingConventionType) }

const
{ all of the special cases enumerated.  The selector field is 6 bits wide }
	kSpecialCaseHighHook = 0;
	kSpecialCaseCaretHook = 0;    { same as kSpecialCaseHighHook }
	kSpecialCaseEOLHook = 1;
	kSpecialCaseWidthHook = 2;
	kSpecialCaseTextWidthHook = 2;    { same as kSpecialCaseWidthHook }
	kSpecialCaseNWidthHook = 3;
	kSpecialCaseDrawHook = 4;
	kSpecialCaseHitTestHook = 5;
	kSpecialCaseTEFindWord = 6;
	kSpecialCaseProtocolHandler = 7;
	kSpecialCaseSocketListener = 8;
	kSpecialCaseTERecalc = 9;
	kSpecialCaseTEDoText = 10;
	kSpecialCaseGNEFilterProc = 11;
	kSpecialCaseMBarHook = 12;


{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
 *
 *  Macros for building ProcInfos.  Examples:
 *  
 *  
 *  uppModalFilterProcInfo = kPascalStackBased
 *       | RESULT_SIZE(SIZE_CODE(sizeof(Boolean)))
 *       | STACK_ROUTINE_PARAMETER(1, SIZE_CODE(sizeof(DialogRef)))
 *       | STACK_ROUTINE_PARAMETER(2, SIZE_CODE(sizeof(EventRecord*)))
 *       | STACK_ROUTINE_PARAMETER(3, SIZE_CODE(sizeof(short*))),
 *
 *  uppDeskHookProcInfo = kRegisterBased
 *       | REGISTER_ROUTINE_PARAMETER(1, kRegisterD0, SIZE_CODE(sizeof(Boolean)))
 *       | REGISTER_ROUTINE_PARAMETER(2, kRegisterA0, SIZE_CODE(sizeof(EventRecord*)))
 *
 *  uppGXSpoolResourceProcInfo = kCStackBased
 *       | RESULT_SIZE(SIZE_CODE(sizeof(OSErr)))
 *       | STACK_ROUTINE_PARAMETER(1, SIZE_CODE(sizeof(gxSpoolFile)))
 *       | STACK_ROUTINE_PARAMETER(2, SIZE_CODE(sizeof(Handle)))
 *       | STACK_ROUTINE_PARAMETER(3, SIZE_CODE(sizeof(ResType)))
 *       | STACK_ROUTINE_PARAMETER(4, SIZE_CODE(sizeof(long)))
 *
 *  uppTEFindWordProcInfo = SPECIAL_CASE_PROCINFO( 6 ),
 *
 }


// Lots of #defines not converted over, request if needed.

{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}
implementation


{$R-}

function NewRoutineDescriptor( theProc: ProcPtr; theProcInfo: ProcInfoType; theISA: ISAType ): UniversalProcPtr; inline;
begin
	NewRoutineDescriptor := UniversalProcPtr(theProc);
end;

procedure DisposeRoutineDescriptor( theUPP: UniversalProcPtr ); inline;
begin
end;

end.

{$endc} {not MACOSALLINCLUDE}
