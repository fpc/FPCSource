{
     File:       Components.p
 
     Contains:   Component Manager Interfaces.
 
     Version:    Technology: QuickTime 5.0
                 Release:    Universal Interfaces 3.4.2
 
     Copyright:  © 1991-2002 by Apple Computer, Inc., all rights reserved.
 
     Bugs?:      For bug reports, consult the following page on
                 the World Wide Web:
 
                     http://www.freepascal.org/bugs.html
 
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

unit Components;
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
uses MacTypes,MacErrors,MixedMode,Files;


{$ALIGN MAC68K}


const
	kAppleManufacturer			= FourCharCode('appl');						{  Apple supplied components  }
	kComponentResourceType		= FourCharCode('thng');						{  a components resource type  }
	kComponentAliasResourceType	= FourCharCode('thga');						{  component alias resource type  }

	kAnyComponentType			= 0;
	kAnyComponentSubType		= 0;
	kAnyComponentManufacturer	= 0;
	kAnyComponentFlagsMask		= 0;

	cmpIsMissing				= $20000000;
	cmpWantsRegisterMessage		= $80000000;

	kComponentOpenSelect		= -1;							{  ComponentInstance for this open  }
	kComponentCloseSelect		= -2;							{  ComponentInstance for this close  }
	kComponentCanDoSelect		= -3;							{  selector # being queried  }
	kComponentVersionSelect		= -4;							{  no params  }
	kComponentRegisterSelect	= -5;							{  no params  }
	kComponentTargetSelect		= -6;							{  ComponentInstance for top of call chain  }
	kComponentUnregisterSelect	= -7;							{  no params  }
	kComponentGetMPWorkFunctionSelect = -8;						{  some params  }
	kComponentExecuteWiredActionSelect = -9;					{  QTAtomContainer actionContainer, QTAtom actionAtom, QTCustomActionTargetPtr target, QTEventRecordPtr event  }
	kComponentGetPublicResourceSelect = -10;					{  OSType resourceType, short resourceId, Handle *resource  }

	{	 Component Resource Extension flags 	}
	componentDoAutoVersion		= $01;
	componentWantsUnregister	= $02;
	componentAutoVersionIncludeFlags = $04;
	componentHasMultiplePlatforms = $08;
	componentLoadResident		= $10;


	{	 Set Default Component flags 	}
	defaultComponentIdentical	= 0;
	defaultComponentAnyFlags	= 1;
	defaultComponentAnyManufacturer = 2;
	defaultComponentAnySubType	= 4;
	defaultComponentAnyFlagsAnyManufacturer = 3;
	defaultComponentAnyFlagsAnyManufacturerAnySubType = 7;

	{	 RegisterComponentResource flags 	}
	registerComponentGlobal		= 1;
	registerComponentNoDuplicates = 2;
	registerComponentAfterExisting = 4;
	registerComponentAliasesOnly = 8;


type
	ComponentDescriptionPtr = ^ComponentDescription;
	ComponentDescription = record
		componentType:			OSType;									{  A unique 4-byte code indentifying the command set  }
		componentSubType:		OSType;									{  Particular flavor of this instance  }
		componentManufacturer:	OSType;									{  Vendor indentification  }
		componentFlags:			UInt32;									{  8 each for Component,Type,SubType,Manuf/revision  }
		componentFlagsMask:		UInt32;									{  Mask for specifying which flags to consider in search, zero during registration  }
	end;


	ResourceSpecPtr = ^ResourceSpec;
	ResourceSpec = record
		resType:				OSType;									{  4-byte code     }
		resID:					SInt16;								{           }
	end;

	ComponentResourcePtr = ^ComponentResource;
	ComponentResource = record
		cd:						ComponentDescription;					{  Registration parameters  }
		component:				ResourceSpec;							{  resource where Component code is found  }
		componentName:			ResourceSpec;							{  name string resource  }
		componentInfo:			ResourceSpec;							{  info string resource  }
		componentIcon:			ResourceSpec;							{  icon resource  }
	end;

	ComponentResourceHandle				= ^ComponentResourcePtr;
	ComponentPlatformInfoPtr = ^ComponentPlatformInfo;
	ComponentPlatformInfo = record
		componentFlags:			SInt32;								{  flags of Component  }
		component:				ResourceSpec;							{  resource where Component code is found  }
		platformType:			SInt16;								{  gestaltSysArchitecture result  }
	end;

	ComponentResourceExtensionPtr = ^ComponentResourceExtension;
	ComponentResourceExtension = record
		componentVersion:		SInt32;								{  version of Component  }
		componentRegisterFlags:	SInt32;								{  flags for registration  }
		componentIconFamily:	SInt16;								{  resource id of Icon Family  }
	end;

	ComponentPlatformInfoArrayPtr = ^ComponentPlatformInfoArray;
	ComponentPlatformInfoArray = record
		count:					SInt32;
		platformArray:			array [0..0] of ComponentPlatformInfo;
	end;

	ExtComponentResourcePtr = ^ExtComponentResource;
	ExtComponentResource = record
		cd:						ComponentDescription;					{  registration parameters  }
		component:				ResourceSpec;							{  resource where Component code is found  }
		componentName:			ResourceSpec;							{  name string resource  }
		componentInfo:			ResourceSpec;							{  info string resource  }
		componentIcon:			ResourceSpec;							{  icon resource  }
		componentVersion:		SInt32;								{  version of Component  }
		componentRegisterFlags:	SInt32;								{  flags for registration  }
		componentIconFamily:	SInt16;								{  resource id of Icon Family  }
		count:					SInt32;								{  elements in platformArray  }
		platformArray:			array [0..0] of ComponentPlatformInfo;
	end;

	ExtComponentResourceHandle			= ^ExtComponentResourcePtr;
	ComponentAliasResourcePtr = ^ComponentAliasResource;
	ComponentAliasResource = record
		cr:						ComponentResource;						{  Registration parameters  }
		aliasCD:				ComponentDescription;					{  component alias description  }
	end;

	{	  Structure received by Component:        	}
	ComponentParametersPtr = ^ComponentParameters;
	ComponentParameters = packed record
		flags:					UInt8;									{  call modifiers: sync/async, deferred, immed, etc  }
		paramSize:				UInt8;									{  size in bytes of actual parameters passed to this call  }
		what:					SInt16;								{  routine selector, negative for Component management calls  }
		params:					array [0..0] of SInt32;				{  actual parameters for the indicated routine  }
	end;

	ComponentRecordPtr = ^ComponentRecord;
	ComponentRecord = record
		data:					array [0..0] of SInt32;
	end;

	Component							= ^ComponentRecord;
	ComponentInstanceRecordPtr = ^ComponentInstanceRecord;
	ComponentInstanceRecord = record
		data:					array [0..0] of SInt32;
	end;

	ComponentInstance					= ^ComponentInstanceRecord;
	RegisteredComponentRecordPtr = ^RegisteredComponentRecord;
	RegisteredComponentRecord = record
		data:					array [0..0] of SInt32;
	end;

	RegisteredComponentInstanceRecordPtr = ^RegisteredComponentInstanceRecord;
	RegisteredComponentInstanceRecord = record
		data:					array [0..0] of SInt32;
	end;

	ComponentResult						= SInt32;

const
	platform68k					= 1;							{  platform type (response from gestaltComponentPlatform)  }
	platformPowerPC				= 2;							{  (when gestaltComponentPlatform is not implemented, use  }
	platformInterpreted			= 3;							{  gestaltSysArchitecture)  }
	platformWin32				= 4;
	platformPowerPCNativeEntryPoint = 5;

	mpWorkFlagDoWork			= $01;
	mpWorkFlagDoCompletion		= $02;
	mpWorkFlagCopyWorkBlock		= $04;
	mpWorkFlagDontBlock			= $08;
	mpWorkFlagGetProcessorCount	= $10;
	mpWorkFlagGetIsRunning		= $40;

	cmpAliasNoFlags				= 0;
	cmpAliasOnlyThisFile		= 1;


type
	ComponentMPWorkFunctionHeaderRecordPtr = ^ComponentMPWorkFunctionHeaderRecord;
	ComponentMPWorkFunctionHeaderRecord = record
		headerSize:				UInt32;
		recordSize:				UInt32;
		workFlags:				UInt32;
		processorCount:			UInt16;
		unused:					SInt8;
		isRunning:				SInt8;
	end;

{$ifc TYPED_FUNCTION_POINTERS}
	ComponentMPWorkFunctionProcPtr = function(globalRefCon: UnivPtr; header: ComponentMPWorkFunctionHeaderRecordPtr): ComponentResult;
{$elsec}
	ComponentMPWorkFunctionProcPtr = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	ComponentRoutineProcPtr = function(var cp: ComponentParameters; componentStorage: Handle): ComponentResult;
{$elsec}
	ComponentRoutineProcPtr = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	GetMissingComponentResourceProcPtr = function(c: Component; resType: OSType; resID: SInt16; refCon: UnivPtr; var resource: Handle): OSErr;
{$elsec}
	GetMissingComponentResourceProcPtr = ProcPtr;
{$endc}

{$ifc OPAQUE_UPP_TYPES}
	ComponentMPWorkFunctionUPP = ^SInt32; { an opaque UPP }
{$elsec}
	ComponentMPWorkFunctionUPP = UniversalProcPtr;
{$endc}	
{$ifc OPAQUE_UPP_TYPES}
	ComponentRoutineUPP = ^SInt32; { an opaque UPP }
{$elsec}
	ComponentRoutineUPP = UniversalProcPtr;
{$endc}	
{$ifc OPAQUE_UPP_TYPES}
	GetMissingComponentResourceUPP = ^SInt32; { an opaque UPP }
{$elsec}
	GetMissingComponentResourceUPP = UniversalProcPtr;
{$endc}	
	{	
	    The parameter list for each ComponentFunction is unique. It is
	    therefore up to users to create the appropriate procInfo for their
	    own ComponentFunctions where necessary.
		}
	ComponentFunctionUPP				= UniversalProcPtr;
	{
	 *  NewComponentFunctionUPP()
	 *  
	 *  Discussion:
	 *    For use in writing a Carbon compliant Component.  It is used to
	 *    create a ComponentFunctionUPP needed to call
	 *    CallComponentFunction in the Components dispatch routine.
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   available as macro/inline
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
function NewComponentFunctionUPP(userRoutine: ProcPtr; procInfo: ProcInfoType): ComponentFunctionUPP; external name '_NewComponentFunctionUPP';

{
 *  DisposeComponentFunctionUPP()
 *  
 *  Discussion:
 *    For use in writing a Carbon compliant Component.  It is used to
 *    dispose of a ComponentFunctionUPP created by
 *    NewComponentFunctionUPP.
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeComponentFunctionUPP(userUPP: ComponentFunctionUPP); external name '_DisposeComponentFunctionUPP';


{
 *  CallComponentUPP
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }


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
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function RegisterComponent(var cd: ComponentDescription; componentEntryPoint: ComponentRoutineUPP; global: SInt16; componentName: Handle; componentInfo: Handle; componentIcon: Handle): Component; external name '_RegisterComponent';
{
 *  RegisterComponentResource()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function RegisterComponentResource(cr: ComponentResourceHandle; global: SInt16): Component; external name '_RegisterComponentResource';
{
 *  UnregisterComponent()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function UnregisterComponent(aComponent: Component): OSErr; external name '_UnregisterComponent';
{
 *  FindNextComponent()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function FindNextComponent(aComponent: Component; var looking: ComponentDescription): Component; external name '_FindNextComponent';
{
 *  CountComponents()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function CountComponents(var looking: ComponentDescription): SInt32; external name '_CountComponents';
{
 *  GetComponentInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetComponentInfo(aComponent: Component; var cd: ComponentDescription; componentName: Handle; componentInfo: Handle; componentIcon: Handle): OSErr; external name '_GetComponentInfo';
{
 *  GetComponentListModSeed()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetComponentListModSeed: SInt32; external name '_GetComponentListModSeed';
{
 *  GetComponentTypeModSeed()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib via QuickTime 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetComponentTypeModSeed(componentType: OSType): SInt32; external name '_GetComponentTypeModSeed';
{*******************************************************
* Component Instance Allocation and dispatch routines
*******************************************************}
{
 *  OpenAComponent()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib via QuickTime 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function OpenAComponent(aComponent: Component; var ci: ComponentInstance): OSErr; external name '_OpenAComponent';
{
 *  OpenComponent()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function OpenComponent(aComponent: Component): ComponentInstance; external name '_OpenComponent';
{
 *  CloseComponent()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function CloseComponent(aComponentInstance: ComponentInstance): OSErr; external name '_CloseComponent';
{
 *  GetComponentInstanceError()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetComponentInstanceError(aComponentInstance: ComponentInstance): OSErr; external name '_GetComponentInstanceError';
{*******************************************************
* Component aliases
*******************************************************}
{
 *  ResolveComponentAlias()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib via QuickTime 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ResolveComponentAlias(aComponent: Component): Component; external name '_ResolveComponentAlias';
{*******************************************************
* Component public resources and public string lists
*******************************************************}
{ Note: GetComponentPublicResource returns a Handle, not a resource.  The caller must dispose it with DisposeHandle. }
{
 *  GetComponentPublicResource()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib via QuickTime 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 }
function GetComponentPublicResource(aComponent: Component; resourceType: OSType; resourceID: SInt16; var theResource: Handle): OSErr; external name '_GetComponentPublicResource';
{
 *  GetComponentPublicResourceList()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib via QuickTime 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 }
function GetComponentPublicResourceList(resourceType: OSType; resourceID: SInt16; flags: SInt32; var cd: ComponentDescription; missingProc: GetMissingComponentResourceUPP; refCon: UnivPtr; atomContainerPtr: UnivPtr): OSErr; external name '_GetComponentPublicResourceList';
{
 *  GetComponentPublicIndString()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib via QuickTime 4.0 and later
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 }
function GetComponentPublicIndString(aComponent: Component; var theString: Str255; strListID: SInt16; index: SInt16): OSErr; external name '_GetComponentPublicIndString';
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
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure SetComponentInstanceError(aComponentInstance: ComponentInstance; theError: OSErr); external name '_SetComponentInstanceError';
{
 *  GetComponentRefcon()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetComponentRefcon(aComponent: Component): SInt32; external name '_GetComponentRefcon';
{
 *  SetComponentRefcon()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure SetComponentRefcon(aComponent: Component; theRefcon: SInt32); external name '_SetComponentRefcon';
{
 *  OpenComponentResFile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function OpenComponentResFile(aComponent: Component): SInt16; external name '_OpenComponentResFile';
{
 *  OpenAComponentResFile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib via QuickTime 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function OpenAComponentResFile(aComponent: Component; var resRef: SInt16): OSErr; external name '_OpenAComponentResFile';
{
 *  CloseComponentResFile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function CloseComponentResFile(refnum: SInt16): OSErr; external name '_CloseComponentResFile';
{ Note: GetComponentResource returns a Handle, not a resource.  The caller must dispose it with DisposeHandle. }
{
 *  GetComponentResource()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib via QuickTime 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetComponentResource(aComponent: Component; resType: OSType; resID: SInt16; var theResource: Handle): OSErr; external name '_GetComponentResource';
{
 *  GetComponentIndString()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib via QuickTime 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetComponentIndString(aComponent: Component; var theString: Str255; strListID: SInt16; index: SInt16): OSErr; external name '_GetComponentIndString';
{*******************************************************
* Component Instance Management routines
*******************************************************}
{
 *  GetComponentInstanceStorage()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetComponentInstanceStorage(aComponentInstance: ComponentInstance): Handle; external name '_GetComponentInstanceStorage';
{
 *  SetComponentInstanceStorage()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure SetComponentInstanceStorage(aComponentInstance: ComponentInstance; theStorage: Handle); external name '_SetComponentInstanceStorage';
{$ifc CALL_NOT_IN_CARBON}
{
 *  GetComponentInstanceA5()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetComponentInstanceA5(aComponentInstance: ComponentInstance): SInt32; external name '_GetComponentInstanceA5';
{
 *  SetComponentInstanceA5()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure SetComponentInstanceA5(aComponentInstance: ComponentInstance; theA5: SInt32); external name '_SetComponentInstanceA5';
{$endc}  {CALL_NOT_IN_CARBON}

{
 *  CountComponentInstances()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function CountComponentInstances(aComponent: Component): SInt32; external name '_CountComponentInstances';
{ useful helper routines for convenient method dispatching }
{
 *  CallComponentFunction()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function CallComponentFunction(var params: ComponentParameters; func: ComponentFunctionUPP): SInt32; external name '_CallComponentFunction';
{
 *  CallComponentFunctionWithStorage()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function CallComponentFunctionWithStorage(storage: Handle; var params: ComponentParameters; func: ComponentFunctionUPP): SInt32; external name '_CallComponentFunctionWithStorage';

{
 *  CallComponentFunctionWithStorageProcInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib via QuickTime 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function CallComponentFunctionWithStorageProcInfo(storage: Handle; var params: ComponentParameters; func: ProcPtr; funcProcInfo: ProcInfoType): SInt32; external name '_CallComponentFunctionWithStorageProcInfo';

{
 *  DelegateComponentCall()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function DelegateComponentCall(var originalParams: ComponentParameters; ci: ComponentInstance): SInt32; external name '_DelegateComponentCall';
{
 *  SetDefaultComponent()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SetDefaultComponent(aComponent: Component; flags: SInt16): OSErr; external name '_SetDefaultComponent';
{
 *  OpenDefaultComponent()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function OpenDefaultComponent(componentType: OSType; componentSubType: OSType): ComponentInstance; external name '_OpenDefaultComponent';
{
 *  OpenADefaultComponent()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib via QuickTime 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function OpenADefaultComponent(componentType: OSType; componentSubType: OSType; var ci: ComponentInstance): OSErr; external name '_OpenADefaultComponent';
{
 *  CaptureComponent()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function CaptureComponent(capturedComponent: Component; capturingComponent: Component): Component; external name '_CaptureComponent';
{
 *  UncaptureComponent()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function UncaptureComponent(aComponent: Component): OSErr; external name '_UncaptureComponent';
{
 *  RegisterComponentResourceFile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function RegisterComponentResourceFile(resRefNum: SInt16; global: SInt16): SInt32; external name '_RegisterComponentResourceFile';
{
 *  GetComponentIconSuite()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetComponentIconSuite(aComponent: Component; var iconSuite: Handle): OSErr; external name '_GetComponentIconSuite';
{*******************************************************
*                                                       *
*           Direct calls to the Components              *
*                                                       *
*******************************************************}
{  Old style names }

{
 *  ComponentFunctionImplemented()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ComponentFunctionImplemented(ci: ComponentInstance; ftnNumber: SInt16): SInt32; external name '_ComponentFunctionImplemented';
{
 *  GetComponentVersion()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetComponentVersion(ci: ComponentInstance): SInt32; external name '_GetComponentVersion';
{
 *  ComponentSetTarget()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ComponentSetTarget(ci: ComponentInstance; target: ComponentInstance): SInt32; external name '_ComponentSetTarget';
{  New style names }

{
 *  CallComponentOpen()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib via QuickTime 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function CallComponentOpen(ci: ComponentInstance; self: ComponentInstance): ComponentResult; external name '_CallComponentOpen';
{
 *  CallComponentClose()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib via QuickTime 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function CallComponentClose(ci: ComponentInstance; self: ComponentInstance): ComponentResult; external name '_CallComponentClose';
{
 *  CallComponentCanDo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib via QuickTime 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function CallComponentCanDo(ci: ComponentInstance; ftnNumber: SInt16): ComponentResult; external name '_CallComponentCanDo';
{
 *  CallComponentVersion()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib via QuickTime 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function CallComponentVersion(ci: ComponentInstance): ComponentResult; external name '_CallComponentVersion';
{
 *  CallComponentRegister()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib via QuickTime 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function CallComponentRegister(ci: ComponentInstance): ComponentResult; external name '_CallComponentRegister';
{
 *  CallComponentTarget()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib via QuickTime 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function CallComponentTarget(ci: ComponentInstance; target: ComponentInstance): ComponentResult; external name '_CallComponentTarget';
{
 *  CallComponentUnregister()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib via QuickTime 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function CallComponentUnregister(ci: ComponentInstance): ComponentResult; external name '_CallComponentUnregister';
{
 *  CallComponentGetMPWorkFunction()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib via QuickTime 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function CallComponentGetMPWorkFunction(ci: ComponentInstance; var workFunction: ComponentMPWorkFunctionUPP; var refCon: UnivPtr): ComponentResult; external name '_CallComponentGetMPWorkFunction';
{
 *  CallComponentGetPublicResource()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib via QuickTime 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function CallComponentGetPublicResource(ci: ComponentInstance; resourceType: OSType; resourceID: SInt16; var resource: Handle): ComponentResult; external name '_CallComponentGetPublicResource';
{$ifc NOT TARGET_OS_MAC}
{ 
        CallComponent is used by ComponentGlue routines to manually call a component function.
     }
{$ifc CALL_NOT_IN_CARBON}
{
 *  CallComponent()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function CallComponent(ci: ComponentInstance; var cp: ComponentParameters): ComponentResult; external name '_CallComponent';

{$endc}  {CALL_NOT_IN_CARBON}
{$endc}

{
    CallComponentDispatch is a CarbonLib routine that replaces CallComponent inline glue
    to call a component function.
 }
{
 *  CallComponentDispatch()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function CallComponentDispatch(var cp: ComponentParameters): ComponentResult; external name '_CallComponentDispatch';


{ UPP call backs }

const
	uppComponentMPWorkFunctionProcInfo = $000003F0;
	uppComponentRoutineProcInfo = $000003F0;
	uppGetMissingComponentResourceProcInfo = $0000FBE0;
	{
	 *  NewComponentMPWorkFunctionUPP()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   available as macro/inline
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
function NewComponentMPWorkFunctionUPP(userRoutine: ComponentMPWorkFunctionProcPtr): ComponentMPWorkFunctionUPP; external name '_NewComponentMPWorkFunctionUPP'; { old name was NewComponentMPWorkFunctionProc }
{
 *  NewComponentRoutineUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewComponentRoutineUPP(userRoutine: ComponentRoutineProcPtr): ComponentRoutineUPP; external name '_NewComponentRoutineUPP'; { old name was NewComponentRoutineProc }
{
 *  NewGetMissingComponentResourceUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewGetMissingComponentResourceUPP(userRoutine: GetMissingComponentResourceProcPtr): GetMissingComponentResourceUPP; external name '_NewGetMissingComponentResourceUPP'; { old name was NewGetMissingComponentResourceProc }
{
 *  DisposeComponentMPWorkFunctionUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeComponentMPWorkFunctionUPP(userUPP: ComponentMPWorkFunctionUPP); external name '_DisposeComponentMPWorkFunctionUPP';
{
 *  DisposeComponentRoutineUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeComponentRoutineUPP(userUPP: ComponentRoutineUPP); external name '_DisposeComponentRoutineUPP';
{
 *  DisposeGetMissingComponentResourceUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeGetMissingComponentResourceUPP(userUPP: GetMissingComponentResourceUPP); external name '_DisposeGetMissingComponentResourceUPP';
{
 *  InvokeComponentMPWorkFunctionUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function InvokeComponentMPWorkFunctionUPP(globalRefCon: UnivPtr; header: ComponentMPWorkFunctionHeaderRecordPtr; userRoutine: ComponentMPWorkFunctionUPP): ComponentResult; external name '_InvokeComponentMPWorkFunctionUPP'; { old name was CallComponentMPWorkFunctionProc }
{
 *  InvokeComponentRoutineUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function InvokeComponentRoutineUPP(var cp: ComponentParameters; componentStorage: Handle; userRoutine: ComponentRoutineUPP): ComponentResult; external name '_InvokeComponentRoutineUPP'; { old name was CallComponentRoutineProc }
{
 *  InvokeGetMissingComponentResourceUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function InvokeGetMissingComponentResourceUPP(c: Component; resType: OSType; resID: SInt16; refCon: UnivPtr; var resource: Handle; userRoutine: GetMissingComponentResourceUPP): OSErr; external name '_InvokeGetMissingComponentResourceUPP'; { old name was CallGetMissingComponentResourceProc }
{ ProcInfos }


{$ALIGN MAC68K}


end.
