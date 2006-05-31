{
     File:       Resources.p
 
     Contains:   Resource Manager Interfaces.
 
     Version:    Technology: Mac OS 8.1
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

unit Resources;
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
uses MacTypes,MixedMode,Files;


{$ALIGN MAC68K}


const
	resSysHeap					= 64;							{ System or application heap? }
	resPurgeable				= 32;							{ Purgeable resource? }
	resLocked					= 16;							{ Load it in locked? }
	resProtected				= 8;							{ Protected? }
	resPreload					= 4;							{ Load in on OpenResFile? }
	resChanged					= 2;							{ Resource changed? }
	mapReadOnly					= 128;							{ Resource file read-only }
	mapCompact					= 64;							{ Compact resource file }
	mapChanged					= 32;							{ Write map out at update }

	resSysRefBit				= 7;							{ reference to system/local reference }
	resSysHeapBit				= 6;							{ In system/in application heap }
	resPurgeableBit				= 5;							{ Purgeable/not purgeable }
	resLockedBit				= 4;							{ Locked/not locked }
	resProtectedBit				= 3;							{ Protected/not protected }
	resPreloadBit				= 2;							{ Read in at OpenResource? }
	resChangedBit				= 1;							{ Existing resource changed since last update }
	mapReadOnlyBit				= 7;							{ is this file read-only? }
	mapCompactBit				= 6;							{ Is a compact necessary? }
	mapChangedBit				= 5;							{ Is it necessary to write map? }

	kResFileNotOpened			= -1;							{ ref num return as error when opening a resource file }
	kSystemResFile				= 0;							{ this is the default ref num to the system file }


type
{$ifc TYPED_FUNCTION_POINTERS}
	ResErrProcPtr = procedure(thErr: OSErr);
{$elsec}
	ResErrProcPtr = Register68kProcPtr;
{$endc}

{$ifc OPAQUE_UPP_TYPES}
	ResErrUPP = ^SInt32; { an opaque UPP }
{$elsec}
	ResErrUPP = UniversalProcPtr;
{$endc}	

const
	uppResErrProcInfo = $00001002;
	{
	 *  NewResErrUPP()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   available as macro/inline
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
function NewResErrUPP(userRoutine: ResErrProcPtr): ResErrUPP; external name '_NewResErrUPP'; { old name was NewResErrProc }
{
 *  DisposeResErrUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeResErrUPP(userUPP: ResErrUPP); external name '_DisposeResErrUPP';
{
 *  InvokeResErrUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure InvokeResErrUPP(thErr: OSErr; userRoutine: ResErrUPP); external name '_InvokeResErrUPP'; { old name was CallResErrProc }
{  QuickTime 3.0 }

type
{$ifc TYPED_FUNCTION_POINTERS}
	ResourceEndianFilterPtr = function(theResource: Handle; currentlyNativeEndian: boolean): OSErr;
{$elsec}
	ResourceEndianFilterPtr = ProcPtr;
{$endc}

{$ifc CALL_NOT_IN_CARBON}
	{
	 *  InitResources()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
	 *    CarbonLib:        not available
	 *    Mac OS X:         not available
	 	}
function InitResources: SInt16; external name '_InitResources';
{
 *  RsrcZoneInit()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure RsrcZoneInit; external name '_RsrcZoneInit';
{$endc}  {CALL_NOT_IN_CARBON}

{
 *  CloseResFile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure CloseResFile(refNum: SInt16); external name '_CloseResFile';
{
 *  ResError()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ResError: OSErr; external name '_ResError';
{
 *  CurResFile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function CurResFile: SInt16; external name '_CurResFile';
{
 *  HomeResFile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function HomeResFile(theResource: Handle): SInt16; external name '_HomeResFile';
{$ifc CALL_NOT_IN_CARBON}
{
 *  CreateResFile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure CreateResFile(const (*var*) fileName: Str255); external name '_CreateResFile';
{
 *  OpenResFile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function OpenResFile(const (*var*) fileName: Str255): SInt16; external name '_OpenResFile';
{$endc}  {CALL_NOT_IN_CARBON}

{
 *  UseResFile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure UseResFile(refNum: SInt16); external name '_UseResFile';
{
 *  CountTypes()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function CountTypes: SInt16; external name '_CountTypes';
{
 *  Count1Types()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function Count1Types: SInt16; external name '_Count1Types';
{
 *  GetIndType()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure GetIndType(var theType: ResType; index: SInt16); external name '_GetIndType';
{
 *  Get1IndType()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure Get1IndType(var theType: ResType; index: SInt16); external name '_Get1IndType';
{
 *  SetResLoad()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure SetResLoad(load: boolean); external name '_SetResLoad';
{
 *  CountResources()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function CountResources(theType: ResType): SInt16; external name '_CountResources';
{
 *  Count1Resources()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function Count1Resources(theType: ResType): SInt16; external name '_Count1Resources';
{
 *  GetIndResource()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function GetIndResource(theType: ResType; index: SInt16): Handle; external name '_GetIndResource';
{
 *  Get1IndResource()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function Get1IndResource(theType: ResType; index: SInt16): Handle; external name '_Get1IndResource';
{
 *  GetResource()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function GetResource(theType: ResType; theID: SInt16): Handle; external name '_GetResource';
{
 *  Get1Resource()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function Get1Resource(theType: ResType; theID: SInt16): Handle; external name '_Get1Resource';
{
 *  GetNamedResource()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function GetNamedResource(theType: ResType; const (*var*) name: Str255): Handle; external name '_GetNamedResource';
{
 *  Get1NamedResource()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function Get1NamedResource(theType: ResType; const (*var*) name: Str255): Handle; external name '_Get1NamedResource';
{
 *  [Mac]LoadResource()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure LoadResource(theResource: Handle); external name '_LoadResource';
{
 *  ReleaseResource()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure ReleaseResource(theResource: Handle); external name '_ReleaseResource';
{
 *  DetachResource()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DetachResource(theResource: Handle); external name '_DetachResource';
{
 *  UniqueID()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function UniqueID(theType: ResType): SInt16; external name '_UniqueID';
{
 *  Unique1ID()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function Unique1ID(theType: ResType): SInt16; external name '_Unique1ID';
{
 *  GetResAttrs()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function GetResAttrs(theResource: Handle): SInt16; external name '_GetResAttrs';
{
 *  GetResInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure GetResInfo(theResource: Handle; var theID: SInt16; var theType: ResType; var name: Str255); external name '_GetResInfo';
{
 *  SetResInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure SetResInfo(theResource: Handle; theID: SInt16; const (*var*) name: Str255); external name '_SetResInfo';
{
 *  AddResource()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure AddResource(theData: Handle; theType: ResType; theID: SInt16; const (*var*) name: Str255); external name '_AddResource';
{
 *  GetResourceSizeOnDisk()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function GetResourceSizeOnDisk(theResource: Handle): SInt32; external name '_GetResourceSizeOnDisk';
{
 *  GetMaxResourceSize()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function GetMaxResourceSize(theResource: Handle): SInt32; external name '_GetMaxResourceSize';
{$ifc CALL_NOT_IN_CARBON}
{
 *  RsrcMapEntry()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function RsrcMapEntry(theResource: Handle): SInt32; external name '_RsrcMapEntry';
{$endc}  {CALL_NOT_IN_CARBON}

{
 *  SetResAttrs()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure SetResAttrs(theResource: Handle; attrs: SInt16); external name '_SetResAttrs';
{
 *  ChangedResource()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure ChangedResource(theResource: Handle); external name '_ChangedResource';
{
 *  RemoveResource()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure RemoveResource(theResource: Handle); external name '_RemoveResource';
{
 *  UpdateResFile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure UpdateResFile(refNum: SInt16); external name '_UpdateResFile';
{
 *  WriteResource()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure WriteResource(theResource: Handle); external name '_WriteResource';
{
 *  SetResPurge()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure SetResPurge(install: boolean); external name '_SetResPurge';
{
 *  GetResFileAttrs()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function GetResFileAttrs(refNum: SInt16): SInt16; external name '_GetResFileAttrs';
{
 *  SetResFileAttrs()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure SetResFileAttrs(refNum: SInt16; attrs: SInt16); external name '_SetResFileAttrs';
{
 *  OpenRFPerm()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function OpenRFPerm(const (*var*) fileName: Str255; vRefNum: SInt16; permission: SInt8): SInt16; external name '_OpenRFPerm';
{$ifc CALL_NOT_IN_CARBON}
{
 *  RGetResource()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function RGetResource(theType: ResType; theID: SInt16): Handle; external name '_RGetResource';
{$endc}  {CALL_NOT_IN_CARBON}

{
 *  HOpenResFile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function HOpenResFile(vRefNum: SInt16; dirID: SInt32; const (*var*) fileName: Str255; permission: SInt8): SInt16; external name '_HOpenResFile';
{
 *  HCreateResFile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure HCreateResFile(vRefNum: SInt16; dirID: SInt32; const (*var*) fileName: Str255); external name '_HCreateResFile';
{
 *  FSpOpenResFile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FSpOpenResFile(const (*var*) spec: FSSpec; permission: SignedByte): SInt16; external name '_FSpOpenResFile';
{
 *  FSpCreateResFile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure FSpCreateResFile(const (*var*) spec: FSSpec; creator: OSType; fileType: OSType; scriptTag: ScriptCode); external name '_FSpCreateResFile';
{
 *  ReadPartialResource()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure ReadPartialResource(theResource: Handle; offset: SInt32; buffer: UnivPtr; count: SInt32); external name '_ReadPartialResource';
{
 *  WritePartialResource()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure WritePartialResource(theResource: Handle; offset: SInt32; buffer: UnivPtr; count: SInt32); external name '_WritePartialResource';
{
 *  SetResourceSize()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure SetResourceSize(theResource: Handle; newSize: SInt32); external name '_SetResourceSize';
{
 *  GetNextFOND()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function GetNextFOND(fondHandle: Handle): Handle; external name '_GetNextFOND';
{  QuickTime 3.0 }
{$ifc CALL_NOT_IN_CARBON}
{
 *  RegisterResourceEndianFilter()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function RegisterResourceEndianFilter(theType: ResType; theFilterProc: ResourceEndianFilterPtr): OSErr; external name '_RegisterResourceEndianFilter';

{ Use TempInsertROMMap to force the ROM resource map to be
   inserted into the chain in front of the system. Note that
   this call is only temporary - the modified resource chain
   is only used for the next call to the resource manager.
   See IM IV 19 for more information. 
}
{
 *  TempInsertROMMap()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure TempInsertROMMap(tempResLoad: boolean); external name '_TempInsertROMMap';
{
  _________________________________________________________________________________________________________
      
   ¥ RESOURCE CHAIN LOCATION - for use with the Resource Manager chain manipulation routines under Carbon.
  _________________________________________________________________________________________________________
}

{$endc}  {CALL_NOT_IN_CARBON}


type
	RsrcChainLocation					= SInt16;

const
	kRsrcChainBelowSystemMap	= 0;							{  Below the system's resource map }
	kRsrcChainBelowApplicationMap = 1;							{  Below the application's resource map }
	kRsrcChainAboveApplicationMap = 2;							{  Above the application's resource map }
	kRsrcChainAboveAllMaps		= 4;							{  Above all resource maps }

	{
	   If the file is already in the resource chain, it is removed and re-inserted at the specified location
	   If the file has been detached, it is added to the resource chain at the specified location
	   Returns resFNotFound if it's not currently open.
	}
	{
	 *  InsertResourceFile()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   not available
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
function InsertResourceFile(refNum: SInt16; where: RsrcChainLocation): OSErr; external name '_InsertResourceFile';

{
   If the file is not currently in the resource chain, this returns resNotFound
   Otherwise, the resource file is removed from the resource chain.
}
{
 *  DetachResourceFile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DetachResourceFile(refNum: SInt16): OSErr; external name '_DetachResourceFile';

{
   Returns true if the resource file is already open and known by the Resource Manager (i.e., it is
   either in the current resource chain or it's a detached resource file.)  If it's in the resource 
   chain, the inChain Boolean is set to true on exit and true is returned.  If it's an open file, but
   the file is currently detached, inChain is set to false and true is returned.  If the file is open,
   the refNum to the file is returned.
}
{
 *  FSpResourceFileAlreadyOpen()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FSpResourceFileAlreadyOpen(const (*var*) resourceFile: FSSpec; var inChain: boolean; var refNum: SInt16): boolean; external name '_FSpResourceFileAlreadyOpen';
{
   FSpOpenOrphanResFile should be used to open a resource file that is persistent across all contexts,
   because using OpenResFile normally loads a map and all preloaded resources into the application
   context.  FSpOpenOrphanResFile loads everything into the system context and detaches the file 
   from the context in which it was opened.  If the file is already in the resource chain and a new
   instance is not opened, FSpOpenOrphanResFile will return a paramErr.
   Use with care, as can and will fail if the map is very large or a lot of preload
   resources exist.
}
{
 *  FSpOpenOrphanResFile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FSpOpenOrphanResFile(const (*var*) spec: FSSpec; permission: SignedByte; var refNum: SInt16): OSErr; external name '_FSpOpenOrphanResFile';

{
   GetTopResourceFile returns the refNum of the top most resource map in the current resource chain. If
   the resource chain is empty it returns resFNotFound.
}
{
 *  GetTopResourceFile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 }
function GetTopResourceFile(var refNum: SInt16): OSErr; external name '_GetTopResourceFile';

{
   GetNextResourceFile can be used to iterate over resource files in the resource chain. By passing a
   valid refNum in curRefNum it will return in nextRefNum the refNum of the next file in 
   the chain. If curRefNum is not found in the resource chain, GetNextResourceFile returns resFNotFound.
   When the end of the chain is reached GetNextResourceFile will return noErr and nextRefNum will be NIL.
}
{
 *  GetNextResourceFile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 }
function GetNextResourceFile(curRefNum: SInt16; var nextRefNum: SInt16): OSErr; external name '_GetNextResourceFile';


{$ifc OLDROUTINENAMES}
{$ifc CALL_NOT_IN_CARBON}
{
 *  SizeResource()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function SizeResource(theResource: Handle): SInt32; external name '_SizeResource';
{
 *  MaxSizeRsrc()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function MaxSizeRsrc(theResource: Handle): SInt32; external name '_MaxSizeRsrc';
{
 *  RmveResource()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure RmveResource(theResource: Handle); external name '_RmveResource';
{$endc}  {CALL_NOT_IN_CARBON}
{$endc}  {OLDROUTINENAMES}

{
 *  FSOpenResFile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.1 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FSOpenResFile(const (*var*) ref: FSRef; permission: SInt8): SInt16; external name '_FSOpenResFile';
{
 *  FSCreateResFile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.1 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure FSCreateResFile(const (*var*) parentRef: FSRef; nameLength: UniCharCount; name: UniCharPtr; whichInfo: FSCatalogInfoBitmap; const (*var*) catalogInfo: FSCatalogInfo; newRef: FSRefPtr; newSpec: FSSpecPtr); external name '_FSCreateResFile';
{
 *  FSResourceFileAlreadyOpen()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.1 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FSResourceFileAlreadyOpen(const (*var*) resourceFileRef: FSRef; var inChain: boolean; var refNum: SInt16): boolean; external name '_FSResourceFileAlreadyOpen';
{
 *  FSCreateResourceFile()
 *  
 *  Summary:
 *    Creates a new resource file.
 *  
 *  Discussion:
 *    This function creates a new file and initializes the specified
 *    named fork as an empty resource fork.  This function allows for
 *    the creation of data fork only files which can be used for
 *    storing resources.  Passing in a null name defaults to using the
 *    data fork.
 *  
 *  Parameters:
 *    
 *    parentRef:
 *      The directory where the file is to be created
 *    
 *    nameLength:
 *      Number of Unicode characters in the file's name
 *    
 *    name:
 *      A pointer to the Unicode name
 *    
 *    whichInfo:
 *      Which catalog info fields to set
 *    
 *    catalogInfo:
 *      The values for catalog info fields to set; may be NULL
 *    
 *    forkNameLength:
 *      The length of the fork name (in Unicode characters)
 *    
 *    forkName:
 *      The name of the fork to initialize (in Unicode); may be NULL
 *    
 *    newRef:
 *      A pointer to the FSRef for the new file; may be NULL
 *    
 *    newSpec:
 *      A pointer to the FSSpec for the new directory; may be NULL
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FSCreateResourceFile(const (*var*) parentRef: FSRef; nameLength: UniCharCount; name: UniCharPtr; whichInfo: FSCatalogInfoBitmap; catalogInfo: {Const}FSCatalogInfoPtr; forkNameLength: UniCharCount; forkName: UniCharPtr; newRef: FSRefPtr; newSpec: FSSpecPtr): OSErr; external name '_FSCreateResourceFile';

{
 *  FSOpenResourceFile()
 *  
 *  Summary:
 *    Opens the specified named fork as a resource fork.
 *  
 *  Discussion:
 *    This function allows any named fork of a file to be used for
 *    storing resources.  Passing in a null forkname will result in the
 *    data fork being used.
 *  
 *  Parameters:
 *    
 *    ref:
 *      The file containing the fork to open
 *    
 *    forkNameLength:
 *      The length of the fork name (in Unicode characters)
 *    
 *    forkName:
 *      The name of the fork to open (in Unicode); may be NULL
 *    
 *    permissions:
 *      The access (read and/or write) you want
 *    
 *    refNum:
 *      On exit the reference number for accessing the open fork
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FSOpenResourceFile(const (*var*) ref: FSRef; forkNameLength: UniCharCount; forkName: UniCharPtr; permissions: SInt8; var refNum: SInt16): OSErr; external name '_FSOpenResourceFile';

{
    These typedefs were originally created for the Copland Resource Mangager
}

type
	ResFileRefNum						= SInt16;
	ResID								= SInt16;
	ResAttributes						= SInt16;
	ResFileAttributes					= SInt16;


{$ALIGN MAC68K}


end.
