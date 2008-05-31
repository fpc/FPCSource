{
     File:       CodeFragments.p
 
     Contains:   Public Code Fragment Manager Interfaces.
 
     Version:    Technology: Forte CFM and Carbon
                 Release:    Universal Interfaces 3.4.2
 
     Copyright:  © 1992-2002 by Apple Computer, Inc., all rights reserved.
 
     Bugs?:      For bug reports, consult the following page on
                 the World Wide Web:
 
                     http://www.freepascal.org/bugs.html
 
}
{
   ¥
   ===========================================================================================
   The Code Fragment Manager API
   =============================
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

unit CodeFragments;
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
uses MacTypes,CFBundle,Files,Multiprocessing;


{$ALIGN MAC68K}

{
   ¤
   ===========================================================================================
   General Types and Constants
   ===========================
}


const
	kCFragResourceType			= FourCharCode('cfrg');
	kCFragResourceID			= 0;
	kCFragLibraryFileType		= FourCharCode('shlb');
	kCFragAllFileTypes			= $FFFFFFFF;


type
	CFragArchitecture					= OSType;

const
																{  Values for type CFragArchitecture. }
	kPowerPCCFragArch			= FourCharCode('pwpc');
	kMotorola68KCFragArch		= FourCharCode('m68k');
	kAnyCFragArch				= $3F3F3F3F;


{$ifc TARGET_CPU_PPC}
	kCompiledCFragArch			= FourCharCode('pwpc');
{$endc}

{$ifc TARGET_CPU_68K}
	kCompiledCFragArch			= FourCharCode('m68k');
{$endc}

{$ifc TARGET_CPU_X86}
	kCompiledCFragArch			= FourCharCode('none');
{$endc}


type
	CFragVersionNumber					= UInt32;

const
	kNullCFragVersion			= 0;
	kWildcardCFragVersion		= $FFFFFFFF;


type
	CFragUsage							= UInt8;

const
																{  Values for type CFragUsage. }
	kImportLibraryCFrag			= 0;							{  Standard CFM import library. }
	kApplicationCFrag			= 1;							{  MacOS application. }
	kDropInAdditionCFrag		= 2;							{  Application or library private extension/plug-in }
	kStubLibraryCFrag			= 3;							{  Import library used for linking only }
	kWeakStubLibraryCFrag		= 4;							{  Import library used for linking only and will be automatically weak linked }


	kIsCompleteCFrag			= 0;							{  A "base" fragment, not an update. }
	kFirstCFragUpdate			= 1;							{  The first update, others are numbered 2, 3, ... }


	kCFragGoesToEOF				= 0;


type
	CFragLocatorKind					= UInt8;

const
																{  Values for type CFragLocatorKind. }
	kMemoryCFragLocator			= 0;							{  Container is in memory. }
	kDataForkCFragLocator		= 1;							{  Container is in a file's data fork. }
	kResourceCFragLocator		= 2;							{  Container is in a file's resource fork. }
	kNamedFragmentCFragLocator	= 4;							{  ! Reserved for possible future use! }
	kCFBundleCFragLocator		= 5;							{  Container is in the executable of a CFBundle }
	kCFBundleIntCFragLocator	= 6;							{  passed to init routines in lieu of kCFBundleCFragLocator }


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
																		{  ! Meaning differs depending on value of "usage". }
		0: (
			appStackSize:		UInt32;									{  If the fragment is an application. (Not used by CFM!) }
			);
	end;

	CFragUsage2UnionPtr = ^CFragUsage2Union;
	CFragUsage2Union = record
		case SInt16 of
																		{  ! Meaning differs depending on value of "usage". }
		0: (
			appSubdirID:		SInt16;									{  If the fragment is an application. }
			);
		1: (
			libFlags:			UInt16;									{  If the fragment is an import library. }
			);
	end;


const
																{  Bit masks for the CFragUsage2Union libFlags variant. }
	kCFragLibUsageMapPrivatelyMask = $0001;						{  Put container in app heap if necessary. }


type
	CFragWhere1UnionPtr = ^CFragWhere1Union;
	CFragWhere1Union = record
		case SInt16 of
																		{  ! Meaning differs depending on value of "where". }
		0: (
			spaceID:			UInt32;									{  If the fragment is in memory.  (Actually an AddressSpaceID.) }
			);
	end;

	CFragWhere2UnionPtr = ^CFragWhere2Union;
	CFragWhere2Union = record
		case SInt16 of
																		{  ! Meaning differs depending on value of "where". }
		0: (
			reserved:			UInt16;
			);
	end;


const
	kDefaultCFragNameLen		= 16;


type
	CFragResourceMemberPtr = ^CFragResourceMember;
	CFragResourceMember = record
		architecture:			CFragArchitecture;
		reservedA:				UInt16;									{  ! Must be zero! }
		reservedB:				SInt8;									{  ! Must be zero! }
		updateLevel:			SInt8;
		currentVersion:			CFragVersionNumber;
		oldDefVersion:			CFragVersionNumber;
		uUsage1:				CFragUsage1Union;
		uUsage2:				CFragUsage2Union;
		usage:					SInt8;
		where:					SInt8;
		offset:					UInt32;
		length:					UInt32;
		uWhere1:				CFragWhere1Union;
		uWhere2:				CFragWhere2Union;
		extensionCount:			UInt16;									{  The number of extensions beyond the name. }
		memberSize:				UInt16;									{  Size in bytes, includes all extensions. }
		name:					packed array [0..15] of UInt8;			{  ! Actually a sized PString. }
	end;

	CFragResourceExtensionHeaderPtr = ^CFragResourceExtensionHeader;
	CFragResourceExtensionHeader = record
		extensionKind:			UInt16;
		extensionSize:			UInt16;
	end;

	CFragResourceSearchExtensionPtr = ^CFragResourceSearchExtension;
	CFragResourceSearchExtension = record
		header:					CFragResourceExtensionHeader;
		libKind:				OSType;
		qualifiers:				SInt8;									{  ! Actually four PStrings. }
	end;


const
	kCFragResourceSearchExtensionKind = $30EE;


type
	CFragResourcePtr = ^CFragResource;
	CFragResource = record
		reservedA:				UInt32;									{  ! Must be zero! }
		reservedB:				UInt32;									{  ! Must be zero! }
		reservedC:				UInt16;									{  ! Must be zero! }
		version:				UInt16;
		reservedD:				UInt32;									{  ! Must be zero! }
		reservedE:				UInt32;									{  ! Must be zero! }
		reservedF:				UInt32;									{  ! Must be zero! }
		reservedG:				UInt32;									{  ! Must be zero! }
		reservedH:				UInt16;									{  ! Must be zero! }
		memberCount:			UInt16;
		firstMember:			CFragResourceMember;
	end;

	CFragResourceHandle					= ^CFragResourcePtr;

const
	kCurrCFragResourceVersion	= 1;


type
	CFragContextID						= MPProcessID;
	CFragConnectionID    = ^SInt32; { an opaque 32-bit type }
	CFragConnectionIDPtr = ^CFragConnectionID;  { when a var xx:CFragConnectionID parameter can be nil, it is changed to xx: CFragConnectionIDPtr }
	CFragClosureID    = ^SInt32; { an opaque 32-bit type }
	CFragClosureIDPtr = ^CFragClosureID;  { when a var xx:CFragClosureID parameter can be nil, it is changed to xx: CFragClosureIDPtr }
	CFragContainerID    = ^SInt32; { an opaque 32-bit type }
	CFragContainerIDPtr = ^CFragContainerID;  { when a var xx:CFragContainerID parameter can be nil, it is changed to xx: CFragContainerIDPtr }
	CFragLoadOptions					= OptionBits;
	mainAddrPtr			= ^Ptr;  { when a var mainAddr: Ptr parameter can be nil, it is changed to mainAddr: mainAddrPtr }
	symAddrPtr			= ^Ptr;  { when a var symAddr: Ptr parameter can be nil, it is changed to symAddr: symAddrPtr }

const
																{  Values for type CFragLoadOptions. }
	kReferenceCFrag				= $0001;						{  Try to use existing copy, increment reference counts. }
	kFindCFrag					= $0002;						{  Try find an existing copy, do not increment reference counts. }
	kPrivateCFragCopy			= $0005;						{  Prepare a new private copy.  (kReferenceCFrag | 0x0004) }


	kUnresolvedCFragSymbolAddress = 0;


type
	CFragSymbolClass					= UInt8;
	CFragSymbolClassPtr					= ^CFragSymbolClass;  { when a var xx:CFragSymbolClass parameter can be nil, it is changed to xx: CFragSymbolClassPtr }

const
																{  Values for type CFragSymbolClass. }
	kCodeCFragSymbol			= 0;
	kDataCFragSymbol			= 1;
	kTVectorCFragSymbol			= 2;
	kTOCCFragSymbol				= 3;
	kGlueCFragSymbol			= 4;


	{
	   ¤
	   ===========================================================================================
	   Macros and Functions
	   ====================
	}


	{
	 *  GetSharedLibrary()
	 *  
	 *  Discussion:
	 *    The connID, mainAddr, and errMessage parameters may be NULL with
	 *    MacOS 8.5 and later. Passing NULL as those parameters when
	 *    running Mac OS 8.1 and earlier systems will corrupt low-memory.
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in CFragManager 1.0 and later
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
function GetSharedLibrary(const (*var*) libName: Str63; archType: CFragArchitecture; options: CFragLoadOptions; var connID: CFragConnectionID; var mainAddr: Ptr; var errMessage: Str255): OSErr; external name '_GetSharedLibrary';
{
 *  GetDiskFragment()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in CFragManager 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function GetDiskFragment(const (*var*) fileSpec: FSSpec; offset: UInt32; length: UInt32; fragName: ConstStringPtr; options: CFragLoadOptions; connID: CFragConnectionIDPtr; mainAddr: mainAddrPtr; errMessage: StringPtr): OSErr; external name '_GetDiskFragment';
{
 *  GetMemFragment()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in CFragManager 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function GetMemFragment(memAddr: UnivPtr; length: UInt32; fragName: ConstStringPtr; options: CFragLoadOptions; connID: CFragConnectionIDPtr; mainAddr: mainAddrPtr; errMessage: StringPtr): OSErr; external name '_GetMemFragment';
{
 *  CloseConnection()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in CFragManager 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function CloseConnection(var connID: CFragConnectionID): OSErr; external name '_CloseConnection';
{
 *  FindSymbol()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in CFragManager 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FindSymbol(connID: CFragConnectionID; const (*var*) symName: Str255; symAddr: symAddrPtr; symClass: CFragSymbolClassPtr): OSErr; external name '_FindSymbol';
{
 *  CountSymbols()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in CFragManager 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function CountSymbols(connID: CFragConnectionID; var symCount: SInt32): OSErr; external name '_CountSymbols';
{
 *  GetIndSymbol()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in CFragManager 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function GetIndSymbol(connID: CFragConnectionID; symIndex: SInt32; symName: StringPtr; symAddr: symAddrPtr; symClass: CFragSymbolClassPtr): OSErr; external name '_GetIndSymbol';
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


type
	CFragSystem7MemoryLocatorPtr = ^CFragSystem7MemoryLocator;
	CFragSystem7MemoryLocator = record
		address:				LogicalAddress;
		length:					UInt32;
		inPlace:				boolean;
		reservedA:				SInt8;									{  ! Must be zero! }
		reservedB:				UInt16;									{  ! Must be zero! }
	end;

	CFragSystem7DiskFlatLocatorPtr = ^CFragSystem7DiskFlatLocator;
	CFragSystem7DiskFlatLocator = record
		fileSpec:				FSSpecPtr;
		offset:					UInt32;
		length:					UInt32;
	end;

	{  ! This must have a file specification at the same offset as a disk flat locator! }
	CFragSystem7SegmentedLocatorPtr = ^CFragSystem7SegmentedLocator;
	CFragSystem7SegmentedLocator = record
		fileSpec:				FSSpecPtr;
		rsrcType:				OSType;
		rsrcID:					SInt16;
		reservedA:				UInt16;									{  ! Must be zero! }
	end;

	{
	   The offset and length for a "Bundle" locator refers to the offset in
	   the CFM executable contained by the bundle.
	}
	CFragCFBundleLocatorPtr = ^CFragCFBundleLocator;
	CFragCFBundleLocator = record
		fragmentBundle:			CFBundleRef;							{  Do not call CFRelease on this bundle! }
		offset:					UInt32;
		length:					UInt32;
	end;

	CFragSystem7LocatorPtr = ^CFragSystem7Locator;
	CFragSystem7Locator = record
		where:					SInt32;
		case SInt16 of
		0: (
			onDisk:				CFragSystem7DiskFlatLocator;
			);
		1: (
			inMem:				CFragSystem7MemoryLocator;
			);
		2: (
			inSegs:				CFragSystem7SegmentedLocator;
			);
		3: (
			inBundle:			CFragCFBundleLocator;
			);
	end;

	CFragSystem7InitBlockPtr = ^CFragSystem7InitBlock;
	CFragSystem7InitBlock = record
		contextID:				CFragContextID;
		closureID:				CFragClosureID;
		connectionID:			CFragConnectionID;
		fragLocator:			CFragSystem7Locator;
		libName:				StringPtr;
		reservedA:				UInt32;									{  ! Must be zero! }
	end;

	CFragInitBlock						= CFragSystem7InitBlock;
	CFragInitBlockPtr 					= ^CFragInitBlock;
	{  These init/term routine types are only of value to CFM itself. }
{$ifc TYPED_FUNCTION_POINTERS}
	CFragInitFunction = function(const (*var*) initBlock: CFragInitBlock): OSErr;
{$elsec}
	CFragInitFunction = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	CFragTermProcedure = procedure;
{$elsec}
	CFragTermProcedure = ProcPtr;
{$endc}

	{
	   For use by init routines. If you get a BundleIntLocator (used to be BundlePreLocator),
	   convert it to a CFBundleLocator with this. Only call this once per locator.
	}
	{
	 *  ConvertBundlePreLocator()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   not available
	 *    CarbonLib:        in CarbonLib 1.4 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
function ConvertBundlePreLocator(initBlockLocator: CFragSystem7LocatorPtr): OSErr; external name '_ConvertBundlePreLocator';


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


const
	kLoadCFrag					= $0001;


{$ifc OLDROUTINENAMES}

type
	ConnectionID						= CFragConnectionID;
	LoadFlags							= CFragLoadOptions;
	SymClass							= CFragSymbolClass;
	InitBlock							= CFragInitBlock;
	InitBlockPtr 						= ^InitBlock;
	MemFragment							= CFragSystem7MemoryLocator;
	MemFragmentPtr 						= ^MemFragment;
	DiskFragment						= CFragSystem7DiskFlatLocator;
	DiskFragmentPtr 					= ^DiskFragment;
	SegmentedFragment					= CFragSystem7SegmentedLocator;
	SegmentedFragmentPtr 				= ^SegmentedFragment;
	FragmentLocator						= CFragSystem7Locator;
	FragmentLocatorPtr 					= ^FragmentLocator;
	CFragHFSMemoryLocator				= CFragSystem7MemoryLocator;
	CFragHFSMemoryLocatorPtr 			= ^CFragHFSMemoryLocator;
	CFragHFSDiskFlatLocator				= CFragSystem7DiskFlatLocator;
	CFragHFSDiskFlatLocatorPtr 			= ^CFragHFSDiskFlatLocator;
	CFragHFSSegmentedLocator			= CFragSystem7SegmentedLocator;
	CFragHFSSegmentedLocatorPtr 		= ^CFragHFSSegmentedLocator;
	CFragHFSLocator						= CFragSystem7Locator;
	CFragHFSLocatorPtr 					= ^CFragHFSLocator;

const
	kPowerPCArch				= FourCharCode('pwpc');
	kMotorola68KArch			= FourCharCode('m68k');
	kAnyArchType				= $3F3F3F3F;
	kNoLibName					= 0;
	kNoConnectionID				= 0;
	kLoadLib					= $0001;
	kFindLib					= $0002;
	kNewCFragCopy				= $0005;
	kLoadNewCopy				= $0005;
	kUseInPlace					= $80;
	kCodeSym					= 0;
	kDataSym					= 1;
	kTVectSym					= 2;
	kTOCSym						= 3;
	kGlueSym					= 4;
	kInMem						= 0;
	kOnDiskFlat					= 1;
	kOnDiskSegmented			= 2;
	kIsLib						= 0;
	kIsApp						= 1;
	kIsDropIn					= 2;
	kFullLib					= 0;
	kUpdateLib					= 1;
	kWholeFork					= 0;
	kCFMRsrcType				= FourCharCode('cfrg');
	kCFMRsrcID					= 0;
	kSHLBFileType				= FourCharCode('shlb');
	kUnresolvedSymbolAddress	= 0;

	kPowerPC					= FourCharCode('pwpc');
	kMotorola68K				= FourCharCode('m68k');

{$endc}  {OLDROUTINENAMES}


{$ALIGN MAC68K}


end.
