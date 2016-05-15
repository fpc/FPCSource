{
     File:       CarbonCore/CodeFragments.h
 
     Contains:   Public Code Fragment Manager Interfaces.
                 The contents of this header file are deprecated.
 
     Copyright:  © 1992-2011 by Apple Inc. All rights reserved.
}
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

unit CodeFragments;
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
uses MacTypes,CFBundle,Files,Multiprocessing;
{$endc} {not MACOSALLINCLUDE}


{$ifc TARGET_OS_MAC}

{
   ===========================================================================================
   The Code Fragment Manager API
   =============================
}


{$ALIGN MAC68K}

{$ifc not TARGET_CPU_64}
{
   ¤
   ===========================================================================================
   General Types and Constants
   ===========================
}

const
	kCFragResourceType = FourCharCode('cfrg');
	kCFragResourceID = 0;
	kCFragLibraryFileType = FourCharCode('shlb');
	kCFragAllFileTypes = -1;


type
	CFragArchitecture = OSType;
const
{ Values for type CFragArchitecture.}
	kPowerPCCFragArch = FourCharCode('pwpc');
	kMotorola68KCFragArch = FourCharCode('m68k');
	kAnyCFragArch = $3F3F3F3F;


{$ifc TARGET_CPU_PPC}
const
	kCompiledCFragArch = kPowerPCCFragArch;

{$endc}  {TARGET_CPU_PPC}

{$ifc TARGET_CPU_X86 or TARGET_CPU_X86_64}
const
	kCompiledCFragArch = FourCharCode('none');

{$endc}  {TARGET_CPU_X86 or TARGET_CPU_X86_64}

type
	CFragVersionNumber = UInt32;
const
	kNullCFragVersion = 0;
	kWildcardCFragVersion = -1;


type
	CFragUsage = UInt8;
const
{ Values for type CFragUsage.}
	kImportLibraryCFrag = 0;    { Standard CFM import library.}
	kApplicationCFrag = 1;    { MacOS application.}
	kDropInAdditionCFrag = 2;    { Application or library private extension/plug-in}
	kStubLibraryCFrag = 3;    { Import library used for linking only}
	kWeakStubLibraryCFrag = 4;     { Import library used for linking only and will be automatically weak linked}


const
	kIsCompleteCFrag = 0;    { A "base" fragment, not an update.}
	kFirstCFragUpdate = 1;     { The first update, others are numbered 2, 3, ...}


const
	kCFragGoesToEOF = 0;


type
	CFragLocatorKind = UInt8;
const
{ Values for type CFragLocatorKind.}
	kMemoryCFragLocator = 0;    { Container is in memory.}
	kDataForkCFragLocator = 1;    { Container is in a file's data fork.}
	kResourceCFragLocator = 2;    { Container is in a file's resource fork.}
	kNamedFragmentCFragLocator = 4;    { ! Reserved for possible future use!}
	kCFBundleCFragLocator = 5;    { Container is in the executable of a CFBundle}
	kCFBundlePreCFragLocator = 6;     { passed to init routines in lieu of kCFBundleCFragLocator}


{
   --------------------------------------------------------------------------------------
   A 'cfrg' resource consists of a header followed by a sequence of variable length
   members.  The constant kDefaultCFragNameLen only provides for a legal ANSI declaration
   and for a reasonable display in a debugger.  The actual name field is cut to fit.
   There may be "extensions" after the name, the memberSize field includes them.  The
   general form of an extension is a 16 bit type code followed by a 16 bit size in bytes.
   Only one standard extension type is defined at present, it is used by SOM's searching
   mechanism.
}


type
	CFragUsage1UnionPtr = ^CFragUsage1Union;
	CFragUsage1Union = record
		case SInt16 of
																		{ ! Meaning differs depending on value of "usage".}
		0: (
			appStackSize: UInt32;									{ If the fragment is an application. (Not used by CFM!)}
			);
	end;
type
	CFragUsage2UnionPtr = ^CFragUsage2Union;
	CFragUsage2Union = record
		case SInt16 of
																		{ ! Meaning differs depending on value of "usage".}
		0: (
			appSubdirID: SInt16;									{ If the fragment is an application. }
			);
		1: (
			libFlags: UInt16;									{ If the fragment is an import library. }
			);
	end;
const
{ Bit masks for the CFragUsage2Union libFlags variant.}
	kCFragLibUsageMapPrivatelyMask = $0001; { Put container in app heap if necessary.}


type
	CFragWhere1UnionPtr = ^CFragWhere1Union;
	CFragWhere1Union = record
		case SInt16 of
																		{ ! Meaning differs depending on value of "where". }
		0: (
			spaceID: UInt32;									{ If the fragment is in memory.  (Actually an AddressSpaceID.)}
			);
	end;
type
	CFragWhere2UnionPtr = ^CFragWhere2Union;
	CFragWhere2Union = record
		case SInt16 of
																		{ ! Meaning differs depending on value of "where".}
		0: (
			reserved: UInt16;
			);
	end;
const
	kDefaultCFragNameLen = 16;


type
	CFragResourceMember = record
		architecture: CFragArchitecture;
		reservedA: UInt16;              { ! Must be zero!}
		reservedB: UInt8;              { ! Must be zero!}
		updateLevel: UInt8;
		currentVersion: CFragVersionNumber;
		oldDefVersion: CFragVersionNumber;
		uUsage1: CFragUsage1Union;
		uUsage2: CFragUsage2Union;
		usage: CFragUsage;
		where: CFragLocatorKind;
		offset: UInt32;
		length: UInt32;
		uWhere1: CFragWhere1Union;
		uWhere2: CFragWhere2Union;
		extensionCount: UInt16;         { The number of extensions beyond the name.}
		memberSize: UInt16;             { Size in bytes, includes all extensions.}
		name: array [0..15] of UInt8;               { ! Actually a sized PString.}
	end;
	CFragResourceMemberPtr = ^CFragResourceMember;
type
	CFragResourceExtensionHeader = record
		extensionKind: UInt16;
		extensionSize: UInt16;
	end;
	CFragResourceExtensionHeaderPtr = ^CFragResourceExtensionHeader;
type
	CFragResourceSearchExtension = record
		header: CFragResourceExtensionHeader;
		libKind: OSType;
		qualifiers: SInt8;          { ! Actually four PStrings.}
	end;


	CFragResourceSearchExtensionPtr = ^CFragResourceSearchExtension;
const
	kCFragResourceSearchExtensionKind = $30EE;


type
	CFragResource = record
		reservedA: UInt32;              { ! Must be zero!}
		reservedB: UInt32;              { ! Must be zero!}
		reservedC: UInt16;              { ! Must be zero!}
		version: UInt16;
		reservedD: UInt32;              { ! Must be zero!}
		reservedE: UInt32;              { ! Must be zero!}
		reservedF: UInt32;              { ! Must be zero!}
		reservedG: UInt32;              { ! Must be zero!}
		reservedH: UInt16;              { ! Must be zero!}
		memberCount: UInt16;
		firstMember: CFragResourceMember;
	end;
	CFragResourcePtr = ^CFragResource;
type
	CFragResourceHandle = ^CFragResourcePtr;
const
	kCurrCFragResourceVersion = 1;


type
	CFragContextID = MPProcessID;
	CFragConnectionID = ^SInt32; { an opaque type }
	CFragConnectionIDPtr = ^CFragConnectionID;  { when a var xx:CFragConnectionID parameter can be nil, it is changed to xx: CFragConnectionIDPtr }
	CFragClosureID = ^SInt32; { an opaque type }
	CFragClosureIDPtr = ^CFragClosureID;  { when a var xx:CFragClosureID parameter can be nil, it is changed to xx: CFragClosureIDPtr }
	CFragContainerID = ^SInt32; { an opaque type }
	CFragContainerIDPtr = ^CFragContainerID;  { when a var xx:CFragContainerID parameter can be nil, it is changed to xx: CFragContainerIDPtr }
	CFragLoadOptions = OptionBits;
	mainAddrPtr = ^Ptr;  { when a var mainAddr: Ptr parameter can be nil, it is changed to mainAddr: mainAddrPtr }
	symAddrPtr = ^Ptr;  { when a var symAddr: Ptr parameter can be nil, it is changed to symAddr: symAddrPtr }
const
{ Values for type CFragLoadOptions.}
	kReferenceCFrag = $0001; { Try to use existing copy, increment reference counts.}
	kFindCFrag = $0002; { Try find an existing copy, do not increment reference counts.}
	kPrivateCFragCopy = $0005; { Prepare a new private copy.  (kReferenceCFrag | 0x0004)}


const
	kUnresolvedCFragSymbolAddress = 0;


type
	CFragSymbolClass = UInt8;
	CFragSymbolClassPtr = ^CFragSymbolClass;
const
{ Values for type CFragSymbolClass.}
	kCodeCFragSymbol = 0;
	kDataCFragSymbol = 1;
	kTVectorCFragSymbol = 2;
	kTOCCFragSymbol = 3;
	kGlueCFragSymbol = 4;


{
   ¤
   ===========================================================================================
   Macros and Functions
   ====================
}


{$ifc not TARGET_CPU_64}
{
 *  GetSharedLibrary()   *** DEPRECATED ***
 *  
 *  Discussion:
 *    The connID, mainAddr, and errMessage parameters may be NULL with
 *    MacOS 8.5 and later. Passing NULL as those parameters when
 *    running Mac OS 8.1 and earlier systems will corrupt low-memory.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.4
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in CFragManager 1.0 and later
 }
function GetSharedLibrary( libName: ConstStr63Param; archType: CFragArchitecture; options: CFragLoadOptions; var connID: CFragConnectionID; var mainAddr: Ptr; var errMessage: Str255 ): OSErr; external name '_GetSharedLibrary';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_5, __IPHONE_NA, __IPHONE_NA) *)


{
 *  GetDiskFragment()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.4
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in CFragManager 1.0 and later
 }
function GetDiskFragment( const (*var*) fileSpec: FSSpec; offset: UInt32; length: UInt32; fragName: ConstStr63Param { can be NULL }; options: CFragLoadOptions; connID: CFragConnectionIDPtr { can be NULL }; mainAddr: mainAddrPtr { can be NULL }; errMessage: StringPtr { can be NULL } ): OSErr; external name '_GetDiskFragment';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_5, __IPHONE_NA, __IPHONE_NA) *)


{
 *  GetMemFragment()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.4
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in CFragManager 1.0 and later
 }
function GetMemFragment( memAddr: UnivPtr; length: UInt32; fragName: ConstStringPtr { can be NULL }; options: CFragLoadOptions; connID: CFragConnectionIDPtr { can be NULL }; mainAddr: mainAddrPtr { can be NULL }; errMessage: StringPtr { can be NULL } ): OSErr; external name '_GetMemFragment';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_5, __IPHONE_NA, __IPHONE_NA) *)


{
 *  CloseConnection()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.4
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in CFragManager 1.0 and later
 }
function CloseConnection( var connID: CFragConnectionID ): OSErr; external name '_CloseConnection';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_5, __IPHONE_NA, __IPHONE_NA) *)


{
 *  FindSymbol()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.4
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in CFragManager 1.0 and later
 }
function FindSymbol( connID: CFragConnectionID; const (*var*) symName: Str255; symAddr: symAddrPtr { can be NULL }; symClass: CFragSymbolClassPtr { can be NULL } ): OSErr; external name '_FindSymbol';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_5, __IPHONE_NA, __IPHONE_NA) *)


{
 *  CountSymbols()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.4
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in CFragManager 1.0 and later
 }
function CountSymbols( connID: CFragConnectionID; var symCount: SIGNEDLONG ): OSErr; external name '_CountSymbols';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_5, __IPHONE_NA, __IPHONE_NA) *)


{
 *  GetIndSymbol()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.4
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in CFragManager 1.0 and later
 }
function GetIndSymbol( connID: CFragConnectionID; symIndex: SIGNEDLONG; symName: StringPtr { can be NULL }; symAddr: symAddrPtr { can be NULL }; symClass: CFragSymbolClassPtr { can be NULL } ): OSErr; external name '_GetIndSymbol';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_5, __IPHONE_NA, __IPHONE_NA) *)


{
   ¤
   ===========================================================================================
   Initialization & Termination Routines
   =====================================
}


{
   -----------------------------------------------------------------------------------------
   A fragment's initialization and termination routines are called when a new incarnation of
   the fragment is created or destroyed, respectively.  Exactly when this occurs depends on
   what kinds of section sharing the fragment has and how the fragment is prepared.  Import
   libraries have at most one incarnation per process.  Fragments prepared with option
   kPrivateCFragCopy may have many incarnations per process.
   The initialization function is passed a pointer to an initialization information structure
   and returns an OSErr.  If an initialization function returns a non-zero value the entire
   closure of which it is a part fails.  The C prototype for an initialization function is:
        OSErr   CFragInitFunction   ( const CFragInitBlock *    initBlock );
   The termination procedure takes no parameters and returns nothing.  The C prototype for a
   termination procedure is:
        void    CFragTermProcedure  ( void );
   Note that since the initialization and termination routines are themselves "CFM"-style
   routines whether or not they have the "pascal" keyword is irrelevant.
}


{
   -----------------------------------------------------------------------------------------
   ! Note:
   ! The "System7" portion of these type names was introduced during the evolution towards
   ! the now defunct Copland version of Mac OS.  Copland was to be called System 8 and there
   ! were slightly different types for System 7 and System 8.  The "generic" type names were
   ! conditionally defined for the desired target system.
   ! Always use the generic types, e.g. CFragInitBlock!  The "System7" names have been kept
   ! only to avoid perturbing code that (improperly) used the target specific type.
}


{$endc} {not TARGET_CPU_64}

type
	CFragSystem7MemoryLocatorPtr = ^CFragSystem7MemoryLocator;
	CFragSystem7MemoryLocator = record
		address: LogicalAddress;
		length: UInt32;
		inPlace: Boolean;
		reservedA: UInt8;              { ! Must be zero!}
		reservedB: UInt16;              { ! Must be zero!}
	end;
type
	CFragSystem7DiskFlatLocatorPtr = ^CFragSystem7DiskFlatLocator;
	CFragSystem7DiskFlatLocator = record
		fileSpec: FSSpecPtr;
		offset: UInt32;
		length: UInt32;
	end;
{ ! This must have a file specification at the same offset as a disk flat locator!}
type
	CFragSystem7SegmentedLocatorPtr = ^CFragSystem7SegmentedLocator;
	CFragSystem7SegmentedLocator = record
		fileSpec: FSSpecPtr;
		rsrcType: OSType;
		rsrcID: SInt16;
		reservedA: UInt16;              { ! Must be zero!}
	end;
{
   The offset and length for a "Bundle" locator refers to the offset in
   the CFM executable contained by the bundle.
}
type
	CFragCFBundleLocatorPtr = ^CFragCFBundleLocator;
	CFragCFBundleLocator = record
		fragmentBundle: CFBundleRef;         { Do not call CFRelease on this bundle!}
		offset: UInt32;
		length: UInt32;
	end;
type
	CFragSystem7LocatorPtr = ^CFragSystem7Locator;
	CFragSystem7Locator = record
		where: SInt32;
		case SInt16 of
		0: (
			onDisk: CFragSystem7DiskFlatLocator;
			);
		1: (
			inMem: CFragSystem7MemoryLocator;
			);
		2: (
			inSegs: CFragSystem7SegmentedLocator;
			);
		3: (
			inBundle: CFragCFBundleLocator;
			);
	end;
type
	CFragSystem7InitBlock = record
		contextID: CFragContextID;
		closureID: CFragClosureID;
		connectionID: CFragConnectionID;
		fragLocator: CFragSystem7Locator;
		libName: StringPtr;
		reservedA: UInt32;              { ! Must be zero!}
	end;
	CFragSystem7InitBlockPtr = ^CFragSystem7InitBlock;
type
	CFragInitBlock = CFragSystem7InitBlock;
	CFragInitBlockPtr = CFragSystem7InitBlockPtr;
{ These init/term routine types are only of value to CFM itself.}
type
	CFragInitFunction = function( const (*var*) initBlock: CFragInitBlock ): OSErr;
	CFragTermProcedure = procedure;
{ For use by init routines. If you get a BundlePreLocator, convert it to a CFBundleLocator with this}
{$ifc not TARGET_CPU_64}
{
 *  ConvertBundlePreLocator()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.1 and later in CoreServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.1 and later
 *    Non-Carbon CFM:   not available
 }
function ConvertBundlePreLocator( initBlockLocator: CFragSystem7LocatorPtr ): OSErr; external name '_ConvertBundlePreLocator';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_1, __MAC_10_5, __IPHONE_NA, __IPHONE_NA) *)


{
   ¤
   ===========================================================================================
   Old Name Spellings
   ==================
}


{
   -------------------------------------------------------------------------------------------
   We've tried to reduce the risk of name collisions in the future by introducing the phrase
   "CFrag" into constant and type names.  The old names are defined below in terms of the new.
}


{$endc} {not TARGET_CPU_64}

const
	kLoadCFrag = kReferenceCFrag;


{$if OLDROUTINENAMES}
//#define IsFileLocation      CFragHasFileLocation
type
	ConnectionID = CFragConnectionID;
	LoadFlags = CFragLoadOptions;
	SymClass = CFragSymbolClass;
	InitBlock = CFragInitBlock;
	InitBlockPtr = CFragInitBlockPtr;
	MemFragment = CFragSystem7MemoryLocator;
	DiskFragment = CFragSystem7DiskFlatLocator;
	SegmentedFragment = CFragSystem7SegmentedLocator;
	FragmentLocator = CFragSystem7Locator;
	FragmentLocatorPtr = CFragSystem7LocatorPtr;
	CFragHFSMemoryLocator = CFragSystem7MemoryLocator;
	CFragHFSDiskFlatLocator = CFragSystem7DiskFlatLocator;
	CFragHFSSegmentedLocator = CFragSystem7SegmentedLocator;
	CFragHFSLocator = CFragSystem7Locator;
	CFragHFSLocatorPtr = CFragSystem7LocatorPtr;
const
	kPowerPCArch = kPowerPCCFragArch;
	kMotorola68KArch = kMotorola68KCFragArch;
	kAnyArchType = kAnyCFragArch;
	kNoLibName = 0;
	kNoConnectionID = 0;
	kLoadLib = kLoadCFrag;
	kFindLib = kFindCFrag;
	kNewCFragCopy = kPrivateCFragCopy;
	kLoadNewCopy = kPrivateCFragCopy;
	kUseInPlace = $80;
	kCodeSym = kCodeCFragSymbol;
	kDataSym = kDataCFragSymbol;
	kTVectSym = kTVectorCFragSymbol;
	kTOCSym = kTOCCFragSymbol;
	kGlueSym = kGlueCFragSymbol;
	kInMem = kMemoryCFragLocator;
	kOnDiskFlat = kDataForkCFragLocator;
	kOnDiskSegmented = kResourceCFragLocator;
	kIsLib = kImportLibraryCFrag;
	kIsApp = kApplicationCFrag;
	kIsDropIn = kDropInAdditionCFrag;
	kFullLib = kIsCompleteCFrag;
	kUpdateLib = kFirstCFragUpdate;
	kWholeFork = kCFragGoesToEOF;
	kCFMRsrcType = kCFragResourceType;
	kCFMRsrcID = kCFragResourceID;
	kSHLBFileType = kCFragLibraryFileType;
	kUnresolvedSymbolAddress = kUnresolvedCFragSymbolAddress;

const
	kPowerPC = kPowerPCCFragArch;
	kMotorola68K = kMotorola68KCFragArch;

{$endc}  {OLDROUTINENAMES}

{$endc} {not TARGET_CPU_64}

{$endc} {TARGET_OS_MAC}
{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}

end.
{$endc} {not MACOSALLINCLUDE}
