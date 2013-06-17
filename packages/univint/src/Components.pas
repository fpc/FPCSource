{
     File:       CarbonCore/Components.h
 
     Contains:   Component Manager Interfaces.
                 The contents of this header file are deprecated.
                 Use Foundation or CoreFoundation bundles and plugins instead.
 
     Copyright:  © 1991-2011 by Apple Inc. All rights reserved.
}
{       Pascal Translation Updated:  Jonas Maebe, <jonas@freepascal.org>, October 2009 }
{       Pascal Translation Updated:  Jonas Maebe, <jonas@freepascal.org>, September 2012 }
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

unit Components;
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
uses MacTypes,MacErrors,Files,MixedMode,Resources;
{$endc} {not MACOSALLINCLUDE}


{$ifc TARGET_OS_MAC}

{$ALIGN MAC68K}


const
	kAppleManufacturer = FourCharCode('appl'); { Apple supplied components }
	kComponentResourceType = FourCharCode('thng'); { a components resource type }
	kComponentAliasResourceType = FourCharCode('thga'); { component alias resource type }

const
	kAnyComponentType = 0;
	kAnyComponentSubType = 0;
	kAnyComponentManufacturer = 0;
	kAnyComponentFlagsMask = 0;

const
	cmpThreadSafe = 1 shl 28; { component is thread-safe }
	cmpIsMissing = 1 shl 29;
	cmpWantsRegisterMessage = 1 shl 31;

const
	kComponentOpenSelect = -1;   { ComponentInstance for this open }
	kComponentCloseSelect = -2;   { ComponentInstance for this close }
	kComponentCanDoSelect = -3;   { selector # being queried }
	kComponentVersionSelect = -4;   { no params }
	kComponentRegisterSelect = -5;   { no params }
	kComponentTargetSelect = -6;   { ComponentInstance for top of call chain }
	kComponentUnregisterSelect = -7;   { no params }
	kComponentGetMPWorkFunctionSelect = -8; { some params }
	kComponentExecuteWiredActionSelect = -9; { QTAtomContainer actionContainer, QTAtom actionAtom, QTCustomActionTargetPtr target, QTEventRecordPtr event }
	kComponentGetPublicResourceSelect = -10; { OSType resourceType, short resourceId, Handle *resource }

{ Component Resource Extension flags }
const
	componentDoAutoVersion = 1 shl 0;
	componentWantsUnregister = 1 shl 1;
	componentAutoVersionIncludeFlags = 1 shl 2;
	componentHasMultiplePlatforms = 1 shl 3;
	componentLoadResident = 1 shl 4;


{ Set Default Component flags }
const
	defaultComponentIdentical = 0;
	defaultComponentAnyFlags = 1;
	defaultComponentAnyManufacturer = 2;
	defaultComponentAnySubType = 4;
	defaultComponentAnyFlagsAnyManufacturer = defaultComponentAnyFlags + defaultComponentAnyManufacturer;
	defaultComponentAnyFlagsAnyManufacturerAnySubType = defaultComponentAnyFlags + defaultComponentAnyManufacturer + defaultComponentAnySubType;

{ RegisterComponentResource flags }
const
	registerComponentGlobal = 1;
	registerComponentNoDuplicates = 2;
	registerComponentAfterExisting = 4;
	registerComponentAliasesOnly = 8;


type
	ComponentDescriptionPtr = ^ComponentDescription;
	ComponentDescription = record
		componentType: OSType;          { A unique 4-byte code indentifying the command set }
		componentSubType: OSType;       { Particular flavor of this instance }
		componentManufacturer: OSType;  { Vendor indentification }
		componentFlags: UInt32;         { 8 each for Component,Type,SubType,Manuf/revision }
		componentFlagsMask: UInt32;     { Mask for specifying which flags to consider in search, zero during registration }
	end;

type
	ResourceSpecPtr = ^ResourceSpec;
	ResourceSpec = record
		resType: OSType;                { 4-byte code    }
		resID: SInt16;                  {         }
	end;
type
	ComponentResource = record
		cd: ComponentDescription;                   { Registration parameters }
		component: ResourceSpec;              { resource where Component code is found }
		componentName: ResourceSpec;          { name string resource }
		componentInfo: ResourceSpec;          { info string resource }
		componentIcon: ResourceSpec;          { icon resource }
	end;
	ComponentResourcePtr = ^ComponentResource;
type
	ComponentResourceHandle = ^ComponentResourcePtr;
	ComponentPlatformInfo = record
		componentFlags: SInt32;         { flags of Component }
		component: ResourceSpec;              { resource where Component code is found }
		platformType: SInt16;           { gestaltSysArchitecture result }
	end;
type
	ComponentResourceExtensionPtr = ^ComponentResourceExtension;
	ComponentResourceExtension = record
		componentVersion: SInt32;       { version of Component }
		componentRegisterFlags: SInt32; { flags for registration }
		componentIconFamily: SInt16;    { resource id of Icon Family }
	end;
type
	ComponentPlatformInfoArrayPtr = ^ComponentPlatformInfoArray;
	ComponentPlatformInfoArray = record
		count: SInt32;
		platformArray: array [0..0] of ComponentPlatformInfo;
	end;
type
	ExtComponentResource = record
		cd: ComponentDescription;                   { registration parameters }
		component: ResourceSpec;              { resource where Component code is found }
		componentName: ResourceSpec;          { name string resource }
		componentInfo: ResourceSpec;          { info string resource }
		componentIcon: ResourceSpec;          { icon resource }
		componentVersion: SInt32;       { version of Component }
		componentRegisterFlags: SInt32; { flags for registration }
		componentIconFamily: SInt16;    { resource id of Icon Family }
		count: SInt32;                  { elements in platformArray }
    platformArray: array [0..0] of ComponentPlatformInfo;
	end;
	ExtComponentResourcePtr = ^ExtComponentResource;
type
	ExtComponentResourceHandle = ^ExtComponentResourcePtr;
	ComponentAliasResource = record
		cr: ComponentResource;                     { Registration parameters }
		aliasCD: ComponentDescription;              { component alias description }
	end;
{  Structure received by Component:        }
type
	ComponentParametersPtr = ^ComponentParameters;
	ComponentParameters = record
		flags: UInt8;                  { call modifiers: sync/async, deferred, immed, etc }
		paramSize: UInt8;              { size in bytes of actual parameters passed to this call }
		what: SInt16;                   { routine selector, negative for Component management calls }
{$ifc TARGET_CPU_64}

		padding: UInt32;
{$endc} {TARGET_CPU_64}

  params: array[0..0] of SIGNEDLONG;             { actual parameters for the indicated routine }
	end;
type
	ComponentRecord = record
		data: array [0..0] of SIGNEDLONG;
	end;
type
	Component = ^ComponentRecord;
	ComponentInstanceRecordPtr = ^ComponentInstanceRecord;
	ComponentInstanceRecord = record
		data: array [0..0] of SIGNEDLONG;
	end;
type
	ComponentInstance = ^ComponentInstanceRecord;
	RegisteredComponentRecord = record
		data: array [0..0] of SIGNEDLONG;
	end;
type
	RegisteredComponentRecordPtr = ^RegisteredComponentRecord;
type
	RegisteredComponentInstanceRecord = record
		data: array [0..0] of SIGNEDLONG;
	end;
	RegisteredComponentInstanceRecordPtr = ^RegisteredComponentInstanceRecord;
type
	ComponentResult = SInt32;
const
	platform68k = 1;    { platform type (response from gestaltComponentPlatform) }
	platformPowerPC = 2;    { (when gestaltComponentPlatform is not implemented, use }
	platformInterpreted = 3;    { gestaltSysArchitecture) }
	platformWin32 = 4;
	platformPowerPCNativeEntryPoint = 5;
	platformIA32NativeEntryPoint = 6;
	platformPowerPC64NativeEntryPoint = 7;
	platformX86_64NativeEntryPoint = 8;

const
	platformIRIXmips = 1000;
	platformSunOSsparc = 1100;
	platformSunOSintel = 1101;
	platformLinuxppc = 1200;
	platformLinuxintel = 1201;
	platformAIXppc = 1300;
	platformNeXTIntel = 1400;
	platformNeXTppc = 1401;
	platformNeXTsparc = 1402;
	platformNeXT68k = 1403;
	platformMacOSx86 = 1500;

const
	mpWorkFlagDoWork = 1 shl 0;
	mpWorkFlagDoCompletion = 1 shl 1;
	mpWorkFlagCopyWorkBlock = 1 shl 2;
	mpWorkFlagDontBlock = 1 shl 3;
	mpWorkFlagGetProcessorCount = 1 shl 4;
	mpWorkFlagGetIsRunning = 1 shl 6;

const
	cmpAliasNoFlags = 0;
	cmpAliasOnlyThisFile = 1;

type
	CSComponentsThreadMode = UInt32;
const
	kCSAcceptAllComponentsMode = 0;
	kCSAcceptThreadSafeComponentsOnlyMode = 1;

{
 *  CSSetComponentsThreadMode()
 *  
 *  Summary:
 *    Set whether or not using thread-unsafe components is allowed on
 *    the current thread.
 *  
 *  Discussion:
 *    When set to kCSAcceptThreadSafeComponentsOnlyMode, the current
 *    thread can only make thread-safe calls. Applications and other
 *    high-level code that wants to call QuickTime (and other) APIs
 *    from preemptive threads should call  SetComponentsThreadMode(
 *    kCSAcceptThreadSafeComponentsOnlyMode );  from their thread
 *    beforehand. The safeguard flag should only be left
 *    kCSAcceptAllComponentsMode for the main thread and other threads
 *    that participate in cooperative locking with it (such as the
 *    Carbon Thread Manager-style cooperative threads and application 
 *    threads that perform private locking).
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.3
 *  
 *  Parameters:
 *    
 *    mode:
 *      The thread-safety mode in current thread.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure CSSetComponentsThreadMode( mode: CSComponentsThreadMode ); external name '_CSSetComponentsThreadMode';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_3, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  CSGetComponentsThreadMode()
 *  
 *  Summary:
 *    Get the current thread's thread-safety mode.
 *  
 *  Discussion:
 *    Returns kCSAcceptThreadSafeComponentsOnlyMode if only thread-safe
 *    components are allowed in current thread and
 *    kCSAcceptAllComponentsMode if all components are accepted
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.3
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function CSGetComponentsThreadMode: CSComponentsThreadMode; external name '_CSGetComponentsThreadMode';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_3, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


type
	ComponentMPWorkFunctionHeaderRecord = record
		headerSize: UInt32;
		recordSize: UInt32;
		workFlags: UInt32;
		processorCount: UInt16;
		unused: UInt8;
		isRunning: UInt8;
	end;
	ComponentMPWorkFunctionHeaderRecordPtr = ^ComponentMPWorkFunctionHeaderRecord;
type
	ComponentMPWorkFunctionProcPtr = function( globalRefCon: UnivPtr; header: ComponentMPWorkFunctionHeaderRecordPtr ): ComponentResult;
	ComponentRoutineProcPtr = function( var cp: ComponentParameters; componentStorage: Handle ): ComponentResult;
	GetMissingComponentResourceProcPtr = function( c: Component; resType: OSType; resID: SInt16; refCon: UnivPtr; var resource: Handle ): OSErr;
	ComponentMPWorkFunctionUPP = ComponentMPWorkFunctionProcPtr;
	ComponentRoutineUPP = ComponentRoutineProcPtr;
	GetMissingComponentResourceUPP = GetMissingComponentResourceProcPtr;
{
    The parameter list for each ComponentFunction is unique. It is
    therefore up to users to create the appropriate procInfo for their
    own ComponentFunctions where necessary.
}
type
	ComponentFunctionUPP = UniversalProcPtr;
{
 *  NewComponentFunctionUPP()
 *  
 *  Discussion:
 *    For use in writing a Carbon compliant Component.  It is used to
 *    create a ComponentFunctionUPP needed to call
 *    CallComponentFunction in the Components dispatch routine.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function NewComponentFunctionUPP( userRoutine: ProcPtr; procInfo: ProcInfoType ): ComponentFunctionUPP; external name '_NewComponentFunctionUPP';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  DisposeComponentFunctionUPP()
 *  
 *  Discussion:
 *    For use in writing a Carbon compliant Component.  It is used to
 *    dispose of a ComponentFunctionUPP created by
 *    NewComponentFunctionUPP.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure DisposeComponentFunctionUPP( userUPP: ComponentFunctionUPP ); external name '_DisposeComponentFunctionUPP';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{*******************************************************
*                                                       *
*               APPLICATION LEVEL CALLS                 *
*                                                       *
*******************************************************}
{*******************************************************
* Component Database Add, Delete, and Query Routines
*******************************************************}
{
 *  RegisterComponent()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.3
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function RegisterComponent( var cd: ComponentDescription; componentEntryPoint: ComponentRoutineUPP; global: SInt16; componentName: Handle; componentInfo: Handle; componentIcon: Handle ): Component; external name '_RegisterComponent';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  RegisterComponentResource()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function RegisterComponentResource( cr: ComponentResourceHandle; global: SInt16 ): Component; external name '_RegisterComponentResource';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  UnregisterComponent()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function UnregisterComponent( aComponent: Component ): OSErr; external name '_UnregisterComponent';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  FindNextComponent()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.3
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function FindNextComponent( aComponent: Component; var looking: ComponentDescription ): Component; external name '_FindNextComponent';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  CountComponents()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.3
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function CountComponents( var looking: ComponentDescription ): SIGNEDLONG; external name '_CountComponents';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  GetComponentInfo()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.3
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetComponentInfo( aComponent: Component; var cd: ComponentDescription; componentName: Handle; componentInfo: Handle; componentIcon: Handle ): OSErr; external name '_GetComponentInfo';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  GetComponentListModSeed()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetComponentListModSeed: SInt32; external name '_GetComponentListModSeed';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  GetComponentTypeModSeed()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib via QuickTime 2.5 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetComponentTypeModSeed( componentType: OSType ): SInt32; external name '_GetComponentTypeModSeed';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{*******************************************************
* Component Instance Allocation and dispatch routines
*******************************************************}
{
 *  OpenAComponent()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.3
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib via QuickTime 2.5 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function OpenAComponent( aComponent: Component; var ci: ComponentInstance ): OSErr; external name '_OpenAComponent';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  OpenComponent()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.3
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function OpenComponent( aComponent: Component ): ComponentInstance; external name '_OpenComponent';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  CloseComponent()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.3
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function CloseComponent( aComponentInstance: ComponentInstance ): OSErr; external name '_CloseComponent';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  GetComponentInstanceError()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.4
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetComponentInstanceError( aComponentInstance: ComponentInstance ): OSErr; external name '_GetComponentInstanceError';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{*******************************************************
* Component aliases
*******************************************************}
{
 *  ResolveComponentAlias()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.3
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib via QuickTime 3.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ResolveComponentAlias( aComponent: Component ): Component; external name '_ResolveComponentAlias';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{*******************************************************
* Component public resources and public string lists
*******************************************************}
{ Note: GetComponentPublicResource returns a Handle, not a resource.  The caller must dispose it with DisposeHandle. }
{
 *  GetComponentPublicResource()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.3
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Non-Carbon CFM:   in InterfaceLib via QuickTime 4.0 and later
 }
function GetComponentPublicResource( aComponent: Component; resourceType: OSType; resourceID: SInt16; var theResource: Handle ): OSErr; external name '_GetComponentPublicResource';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  GetComponentPublicResourceList()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.3
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Non-Carbon CFM:   in InterfaceLib via QuickTime 4.0 and later
 }
function GetComponentPublicResourceList( resourceType: OSType; resourceID: SInt16; flags: SInt32; var cd: ComponentDescription; missingProc: GetMissingComponentResourceUPP; refCon: UnivPtr; atomContainerPtr: UnivPtr ): OSErr; external name '_GetComponentPublicResourceList';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  GetComponentPublicIndString()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.3
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Non-Carbon CFM:   in InterfaceLib via QuickTime 4.0 and later
 }
function GetComponentPublicIndString( aComponent: Component; var theString: Str255; strListID: SInt16; index: SInt16 ): OSErr; external name '_GetComponentPublicIndString';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{*******************************************************
*                                                       *
*                   CALLS MADE BY COMPONENTS            *
*                                                       *
*******************************************************}
{*******************************************************
* Component Management routines
*******************************************************}
{
 *  SetComponentInstanceError()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.4
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure SetComponentInstanceError( aComponentInstance: ComponentInstance; theError: OSErr ); external name '_SetComponentInstanceError';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  GetComponentRefcon()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.4
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetComponentRefcon( aComponent: Component ): SIGNEDLONG; external name '_GetComponentRefcon';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  SetComponentRefcon()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.4
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure SetComponentRefcon( aComponent: Component; theRefcon: SIGNEDLONG ); external name '_SetComponentRefcon';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  OpenComponentResFile()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function OpenComponentResFile( aComponent: Component ): ResFileRefNum; external name '_OpenComponentResFile';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  OpenAComponentResFile()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib via QuickTime 2.5 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function OpenAComponentResFile( aComponent: Component; var resRef: ResFileRefNum ): OSErr; external name '_OpenAComponentResFile';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  CloseComponentResFile()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function CloseComponentResFile( refnum: ResFileRefNum ): OSErr; external name '_CloseComponentResFile';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{ Note: GetComponentResource returns a Handle, not a resource.  The caller must dispose it with DisposeHandle. }
{
 *  GetComponentResource()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.3
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib via QuickTime 3.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetComponentResource( aComponent: Component; resType: OSType; resID: SInt16; var theResource: Handle ): OSErr; external name '_GetComponentResource';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  GetComponentIndString()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.3
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib via QuickTime 3.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetComponentIndString( aComponent: Component; var theString: Str255; strListID: SInt16; index: SInt16 ): OSErr; external name '_GetComponentIndString';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{*******************************************************
* Component Instance Management routines
*******************************************************}
{
 *  GetComponentInstanceStorage()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.4
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetComponentInstanceStorage( aComponentInstance: ComponentInstance ): Handle; external name '_GetComponentInstanceStorage';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  SetComponentInstanceStorage()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.4
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure SetComponentInstanceStorage( aComponentInstance: ComponentInstance; theStorage: Handle ); external name '_SetComponentInstanceStorage';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  CountComponentInstances()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.4
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function CountComponentInstances( aComponent: Component ): SIGNEDLONG; external name '_CountComponentInstances';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{ useful helper routines for convenient method dispatching }
{
 *  CallComponentFunction()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.3
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function CallComponentFunction( var params: ComponentParameters; func: ComponentFunctionUPP ): ComponentResult; external name '_CallComponentFunction';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  CallComponentFunctionWithStorage()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.3
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function CallComponentFunctionWithStorage( storage: Handle; var params: ComponentParameters; func: ComponentFunctionUPP ): ComponentResult; external name '_CallComponentFunctionWithStorage';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  CallComponentFunctionWithStorageProcInfo()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib via QuickTime 2.5 and later
 }
function CallComponentFunctionWithStorageProcInfo( storage: Handle; var params: ComponentParameters; func: ProcPtr; funcProcInfo: ProcInfoType ): ComponentResult; external name '_CallComponentFunctionWithStorageProcInfo';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  DelegateComponentCall()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function DelegateComponentCall( var originalParams: ComponentParameters; ci: ComponentInstance ): ComponentResult; external name '_DelegateComponentCall';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  SetDefaultComponent()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SetDefaultComponent( aComponent: Component; flags: SInt16 ): OSErr; external name '_SetDefaultComponent';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  OpenDefaultComponent()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.3
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function OpenDefaultComponent( componentType: OSType; componentSubType: OSType ): ComponentInstance; external name '_OpenDefaultComponent';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  OpenADefaultComponent()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.3
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib via QuickTime 2.5 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function OpenADefaultComponent( componentType: OSType; componentSubType: OSType; var ci: ComponentInstance ): OSErr; external name '_OpenADefaultComponent';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  CaptureComponent()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.4
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function CaptureComponent( capturedComponent: Component; capturingComponent: Component ): Component; external name '_CaptureComponent';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  UncaptureComponent()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.4
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function UncaptureComponent( aComponent: Component ): OSErr; external name '_UncaptureComponent';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  RegisterComponentResourceFile()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function RegisterComponentResourceFile( resRefNum: SInt16; global: SInt16 ): SInt32; external name '_RegisterComponentResourceFile';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{ This call is deprecated. Please use GetIconRefFromComponent() instead.}
{$ifc not TARGET_CPU_64}
{
 *  GetComponentIconSuite()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only]
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetComponentIconSuite( aComponent: Component; var iconSuite: Handle ): OSErr; external name '_GetComponentIconSuite';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{$endc} {not TARGET_CPU_64}

{
 * These calls allow you to register a file system entity.  The
 * Component Manager will "do the right thing" with the entity,
 * whether it is a standard resource fork based CFM component, CFM
 * bundle, mach-o bundle, or packaged bundle.  
 *
 * The *Entries calls allow you to specify a component description
 * which will be used to register selective components.  (Passing
 * NULL, 0 means to register all components.  
 }
{$ifc not TARGET_CPU_64}
{
 *  RegisterComponentFile()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.3
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function RegisterComponentFile( const (*var*) spec: FSSpec; global: SInt16 ): OSErr; external name '_RegisterComponentFile';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_5, __IPHONE_NA, __IPHONE_NA) *)


{
 *  RegisterComponentFileEntries()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.3
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function RegisterComponentFileEntries( const (*var*) spec: FSSpec; global: SInt16; {const} toRegister: ComponentDescriptionPtr { can be NULL }; registerCount: UInt32 ): OSErr; external name '_RegisterComponentFileEntries';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_5, __IPHONE_NA, __IPHONE_NA) *)


{$endc} {not TARGET_CPU_64}

{
 *  RegisterComponentFileRef()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.3
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function RegisterComponentFileRef( const (*var*) ref: FSRef; global: SInt16 ): OSErr; external name '_RegisterComponentFileRef';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  RegisterComponentFileRefEntries()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.3
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function RegisterComponentFileRefEntries( const (*var*) ref: FSRef; global: SInt16; {const} toRegister: ComponentDescriptionPtr { can be NULL }; registerCount: UInt32 ): OSErr; external name '_RegisterComponentFileRefEntries';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{*******************************************************
*                                                       *
*           Direct calls to the Components              *
*                                                       *
*******************************************************}
{ Old style names}

{$ifc not TARGET_CPU_64}
{
 *  ComponentFunctionImplemented()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.3
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ComponentFunctionImplemented( ci: ComponentInstance; ftnNumber: SInt16 ): ComponentResult; external name '_ComponentFunctionImplemented';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_5, __IPHONE_NA, __IPHONE_NA) *)


{
 *  GetComponentVersion()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.3
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetComponentVersion( ci: ComponentInstance ): ComponentResult; external name '_GetComponentVersion';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_5, __IPHONE_NA, __IPHONE_NA) *)


{
 *  ComponentSetTarget()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.3
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ComponentSetTarget( ci: ComponentInstance; target: ComponentInstance ): ComponentResult; external name '_ComponentSetTarget';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_5, __IPHONE_NA, __IPHONE_NA) *)


{ New style names}

{$endc} {not TARGET_CPU_64}

{
 *  CallComponentOpen()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.3
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib via QuickTime 2.5 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function CallComponentOpen( ci: ComponentInstance; self: ComponentInstance ): ComponentResult; external name '_CallComponentOpen';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  CallComponentClose()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.3
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib via QuickTime 2.5 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function CallComponentClose( ci: ComponentInstance; self: ComponentInstance ): ComponentResult; external name '_CallComponentClose';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  CallComponentCanDo()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.3
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib via QuickTime 2.5 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function CallComponentCanDo( ci: ComponentInstance; ftnNumber: SInt16 ): ComponentResult; external name '_CallComponentCanDo';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  CallComponentVersion()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.3
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib via QuickTime 2.5 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function CallComponentVersion( ci: ComponentInstance ): ComponentResult; external name '_CallComponentVersion';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  CallComponentRegister()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.3
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib via QuickTime 2.5 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function CallComponentRegister( ci: ComponentInstance ): ComponentResult; external name '_CallComponentRegister';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  CallComponentTarget()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.3
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib via QuickTime 2.5 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function CallComponentTarget( ci: ComponentInstance; target: ComponentInstance ): ComponentResult; external name '_CallComponentTarget';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  CallComponentUnregister()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.3
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib via QuickTime 2.5 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function CallComponentUnregister( ci: ComponentInstance ): ComponentResult; external name '_CallComponentUnregister';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  CallComponentGetMPWorkFunction()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.3
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib via QuickTime 2.5 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function CallComponentGetMPWorkFunction( ci: ComponentInstance; var workFunction: ComponentMPWorkFunctionUPP; var refCon: UnivPtr ): ComponentResult; external name '_CallComponentGetMPWorkFunction';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  CallComponentGetPublicResource()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.3
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in InterfaceLib via QuickTime 4.0 and later
 }
function CallComponentGetPublicResource( ci: ComponentInstance; resourceType: OSType; resourceID: SInt16; var resource: Handle ): ComponentResult; external name '_CallComponentGetPublicResource';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
    CallComponentDispatch is a CarbonLib routine that replaces CallComponent inline glue
    to call a component function.
 }
{
 *  CallComponentDispatch()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.3
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   not available
 }
function CallComponentDispatch( var cp: ComponentParameters ): ComponentResult; external name '_CallComponentDispatch';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{ UPP call backs }
{
 *  NewComponentMPWorkFunctionUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function NewComponentMPWorkFunctionUPP( userRoutine: ComponentMPWorkFunctionProcPtr ): ComponentMPWorkFunctionUPP; external name '_NewComponentMPWorkFunctionUPP';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)

{
 *  NewComponentRoutineUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function NewComponentRoutineUPP( userRoutine: ComponentRoutineProcPtr ): ComponentRoutineUPP; external name '_NewComponentRoutineUPP';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)

{
 *  NewGetMissingComponentResourceUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function NewGetMissingComponentResourceUPP( userRoutine: GetMissingComponentResourceProcPtr ): GetMissingComponentResourceUPP; external name '_NewGetMissingComponentResourceUPP';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)

{
 *  DisposeComponentMPWorkFunctionUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure DisposeComponentMPWorkFunctionUPP( userUPP: ComponentMPWorkFunctionUPP ); external name '_DisposeComponentMPWorkFunctionUPP';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)

{
 *  DisposeComponentRoutineUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure DisposeComponentRoutineUPP( userUPP: ComponentRoutineUPP ); external name '_DisposeComponentRoutineUPP';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)

{
 *  DisposeGetMissingComponentResourceUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure DisposeGetMissingComponentResourceUPP( userUPP: GetMissingComponentResourceUPP ); external name '_DisposeGetMissingComponentResourceUPP';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)

{
 *  InvokeComponentMPWorkFunctionUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function InvokeComponentMPWorkFunctionUPP( globalRefCon: UnivPtr; header: ComponentMPWorkFunctionHeaderRecordPtr; userUPP: ComponentMPWorkFunctionUPP ): ComponentResult; external name '_InvokeComponentMPWorkFunctionUPP';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)

{
 *  InvokeComponentRoutineUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function InvokeComponentRoutineUPP( var cp: ComponentParameters; componentStorage: Handle; userUPP: ComponentRoutineUPP ): ComponentResult; external name '_InvokeComponentRoutineUPP';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)

{
 *  InvokeGetMissingComponentResourceUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function InvokeGetMissingComponentResourceUPP( c: Component; resType: OSType; resID: SInt16; refCon: UnivPtr; var resource: Handle; userUPP: GetMissingComponentResourceUPP ): OSErr; external name '_InvokeGetMissingComponentResourceUPP';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)

{ ProcInfos }

{ MixedMode ProcInfo constants for component calls }
const
	uppComponentFunctionImplementedProcInfo = $000002F0;
	uppGetComponentVersionProcInfo = $000000F0;
	uppComponentSetTargetProcInfo = $000003F0;
	uppCallComponentOpenProcInfo = $000003F0;
	uppCallComponentCloseProcInfo = $000003F0;
	uppCallComponentCanDoProcInfo = $000002F0;
	uppCallComponentVersionProcInfo = $000000F0;
	uppCallComponentRegisterProcInfo = $000000F0;
	uppCallComponentTargetProcInfo = $000003F0;
	uppCallComponentUnregisterProcInfo = $000000F0;
	uppCallComponentGetMPWorkFunctionProcInfo = $00000FF0;
	uppCallComponentGetPublicResourceProcInfo = $00003BF0;

{$endc} {TARGET_OS_MAC}

{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}

end.
{$endc} {not MACOSALLINCLUDE}
