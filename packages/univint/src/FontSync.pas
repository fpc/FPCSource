{
     File:       FontSync.p
 
     Contains:   Public interface for FontSync
 
     Version:    Technology: Mac OS 9 / Carbon
                 Release:    Universal Interfaces 3.4.2
 
     Copyright:  © 1999-2002 by Apple Computer, Inc., all rights reserved.
 
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

unit FontSync;
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
uses MacTypes,ATSTypes,Files,Fonts,SFNTTypes,MacErrors;


{$ALIGN POWER}


{ Matching Options }

type
	FNSMatchOptions 			= UInt32;
	FNSMatchOptionsPtr			= ^FNSMatchOptions; { when a VAR xx: FNSMatchOptions parameter can be nil, it is changed to xx: FNSMatchOptionsPtr }
const
	kFNSMatchNames				= $00000001;					{  font names must match  }
	kFNSMatchTechnology			= $00000002;					{  scaler technology must match  }
	kFNSMatchGlyphs				= $00000004;					{  glyph data must match  }
	kFNSMatchEncodings			= $00000008;					{  cmaps must match  }
	kFNSMatchQDMetrics			= $00000010;					{  QuickDraw Text metrics must match  }
	kFNSMatchATSUMetrics		= $00000020;					{  ATSUI metrics (incl. vertical) must match  }
	kFNSMatchKerning			= $00000040;					{  kerning data must match  }
	kFNSMatchWSLayout			= $00000080;					{  WorldScript layout tables must match  }
	kFNSMatchAATLayout			= $00000100;					{  AAT (incl. OpenType) layout tables must match  }
	kFNSMatchPrintEncoding		= $00000200;					{  PostScript font and glyph names and re-encoding vector must match  }
	kFNSMissingDataNoMatch		= $80000000;					{  treat missing data as mismatch  }
	kFNSMatchAll				= $FFFFFFFF;					{  everything must match  }
	kFNSMatchDefaults			= 0;							{  use global default match options  }

	{
	 *  FNSMatchDefaultsGet()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in FontSyncLib 1.0 and later
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
function FNSMatchDefaultsGet: FNSMatchOptions; external name '_FNSMatchDefaultsGet';


{ Version control }

type
	FNSObjectVersion 			= UInt32;
const
	kFNSVersionDontCare			= 0;
	kFNSCurSysInfoVersion		= 1;

	{  No features defined yet. }

type
	FNSFeatureFlags						= UInt32;
	{
	   The FontSync library version number is binary-coded decimal:
	   8 bits of major version, 4 minor version and 4 bits revision.
	}
	FNSSysInfoPtr = ^FNSSysInfo;
	FNSSysInfo = record
		iSysInfoVersion:		FNSObjectVersion;						{  fill this in before calling FNSSysInfoGet }
		oFeatures:				FNSFeatureFlags;
		oCurRefVersion:			FNSObjectVersion;
		oMinRefVersion:			FNSObjectVersion;
		oCurProfileVersion:		FNSObjectVersion;
		oMinProfileVersion:		FNSObjectVersion;
		oFontSyncVersion:		UInt16;
	end;

	{
	 *  FNSSysInfoGet()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in FontSyncLib 1.0 and later
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
procedure FNSSysInfoGet(var ioInfo: FNSSysInfo); external name '_FNSSysInfoGet';


{ FontSync References }

type
	FNSFontReference    = ^SInt32; { an opaque 32-bit type }
	FNSFontReferencePtr = ^FNSFontReference;  { when a var xx:FNSFontReference parameter can be nil, it is changed to xx: FNSFontReferencePtr }
	{
	 *  FNSReferenceGetVersion()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in FontSyncLib 1.0 and later
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
function FNSReferenceGetVersion(iReference: FNSFontReference; var oVersion: FNSObjectVersion): OSStatus; external name '_FNSReferenceGetVersion';

{
 *  FNSReferenceDispose()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in FontSyncLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FNSReferenceDispose(iReference: FNSFontReference): OSStatus; external name '_FNSReferenceDispose';

{
 *  FNSReferenceMatch()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in FontSyncLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FNSReferenceMatch(iReference1: FNSFontReference; iReference2: FNSFontReference; iOptions: FNSMatchOptions; oFailedMatchOptions: FNSMatchOptionsPtr): OSStatus; external name '_FNSReferenceMatch';

{
 *  FNSReferenceFlattenedSize()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in FontSyncLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FNSReferenceFlattenedSize(iReference: FNSFontReference; var oFlattenedSize: ByteCount): OSStatus; external name '_FNSReferenceFlattenedSize';

{
 *  FNSReferenceFlatten()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in FontSyncLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FNSReferenceFlatten(iReference: FNSFontReference; oFlatReference: UnivPtr; oFlattenedSize: ByteCountPtr): OSStatus; external name '_FNSReferenceFlatten';

{
 *  FNSReferenceUnflatten()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in FontSyncLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FNSReferenceUnflatten(iFlatReference: UnivPtr; iFlattenedSize: ByteCount; var oReference: FNSFontReference): OSStatus; external name '_FNSReferenceUnflatten';


{ FontSync Profiles }

const
	kFNSCreatorDefault			= 0;
	kFNSProfileFileType			= FourCharCode('fnsp');


type
	FNSFontProfile    = ^SInt32; { an opaque 32-bit type }
	FNSFontProfilePtr = ^FNSFontProfile;  { when a var xx:FNSFontProfile parameter can be nil, it is changed to xx: FNSFontProfilePtr }
	{
	 *  FNSProfileCreate()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in FontSyncLib 1.0 and later
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
function FNSProfileCreate(const (*var*) iFile: FSSpec; iCreator: FourCharCode; iEstNumRefs: ItemCount; iDesiredVersion: FNSObjectVersion; var oProfile: FNSFontProfile): OSStatus; external name '_FNSProfileCreate';

{
 *  FNSProfileOpen()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in FontSyncLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FNSProfileOpen(const (*var*) iFile: FSSpec; iOpenForWrite: boolean; var oProfile: FNSFontProfile): OSStatus; external name '_FNSProfileOpen';


{
 *  FNSProfileCreateWithFSRef()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.1 and later
 *    Mac OS X:         in version 10.1 and later
 }
function FNSProfileCreateWithFSRef(const (*var*) iParentDirectory: FSRef; iNameLength: UniCharCount; iName: ConstUniCharPtr; iCreator: FourCharCode; iEstNumRefs: ItemCount; iDesiredVersion: FNSObjectVersion; var oProfile: FNSFontProfile): OSStatus; external name '_FNSProfileCreateWithFSRef';

{
 *  FNSProfileOpenWithFSRef()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.1 and later
 *    Mac OS X:         in version 10.1 and later
 }
function FNSProfileOpenWithFSRef(const (*var*) iFile: FSRef; iOpenForWrite: boolean; var oProfile: FNSFontProfile): OSStatus; external name '_FNSProfileOpenWithFSRef';

{
 *  FNSProfileGetVersion()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in FontSyncLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FNSProfileGetVersion(iProfile: FNSFontProfile; var oVersion: FNSObjectVersion): OSStatus; external name '_FNSProfileGetVersion';

{
 *  FNSProfileCompact()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in FontSyncLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FNSProfileCompact(iProfile: FNSFontProfile): OSStatus; external name '_FNSProfileCompact';

{
 *  FNSProfileClose()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in FontSyncLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FNSProfileClose(iProfile: FNSFontProfile): OSStatus; external name '_FNSProfileClose';

{
 *  FNSProfileAddReference()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in FontSyncLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FNSProfileAddReference(iProfile: FNSFontProfile; iReference: FNSFontReference): OSStatus; external name '_FNSProfileAddReference';

{
 *  FNSProfileRemoveReference()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in FontSyncLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FNSProfileRemoveReference(iProfile: FNSFontProfile; iReference: FNSFontReference): OSStatus; external name '_FNSProfileRemoveReference';

{
 *  FNSProfileRemoveIndReference()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in FontSyncLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FNSProfileRemoveIndReference(iProfile: FNSFontProfile; iIndex: UInt32): OSStatus; external name '_FNSProfileRemoveIndReference';

{
 *  FNSProfileClear()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in FontSyncLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FNSProfileClear(iProfile: FNSFontProfile): OSStatus; external name '_FNSProfileClear';

{
 *  FNSProfileCountReferences()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in FontSyncLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FNSProfileCountReferences(iProfile: FNSFontProfile; var oCount: ItemCount): OSStatus; external name '_FNSProfileCountReferences';

{
 *  FNSProfileGetIndReference()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in FontSyncLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FNSProfileGetIndReference(iProfile: FNSFontProfile; iWhichReference: UInt32; var oReference: FNSFontReference): OSStatus; external name '_FNSProfileGetIndReference';

{
 *  FNSProfileMatchReference()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in FontSyncLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FNSProfileMatchReference(iProfile: FNSFontProfile; iReference: FNSFontReference; iMatchOptions: FNSMatchOptions; iOutputSize: ItemCount; oIndices: UInt32Ptr; oNumMatches: ItemCountPtr): OSStatus; external name '_FNSProfileMatchReference';


{ Mapping to and from Font Objects }
{
 *  FNSReferenceCreate()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in FontSyncLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FNSReferenceCreate(iFont: FMFont; iDesiredVersion: FNSObjectVersion; var oReference: FNSFontReference): OSStatus; external name '_FNSReferenceCreate';

{
 *  FNSReferenceMatchFonts()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in FontSyncLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FNSReferenceMatchFonts(iReference: FNSFontReference; iMatchOptions: FNSMatchOptions; iOutputSize: ItemCount; oFonts: FMFontPtr; oNumMatches: ItemCountPtr): OSStatus; external name '_FNSReferenceMatchFonts';


{ Mapping to and from Font Families }
{
 *  FNSReferenceCreateFromFamily()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in FontSyncLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FNSReferenceCreateFromFamily(iFamily: FMFontFamily; iStyle: FMFontStyle; iDesiredVersion: FNSObjectVersion; oReference: FNSFontReferencePtr; oActualStyle: FMFontStylePtr): OSStatus; external name '_FNSReferenceCreateFromFamily';

{
 *  FNSReferenceMatchFamilies()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in FontSyncLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FNSReferenceMatchFamilies(iReference: FNSFontReference; iMatchOptions: FNSMatchOptions; iOutputSize: ItemCount; oFonts: FMFontFamilyInstancePtr; oNumMatches: ItemCountPtr): OSStatus; external name '_FNSReferenceMatchFamilies';


{ UI Support }
{
 *  FNSReferenceGetFamilyInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in FontSyncLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FNSReferenceGetFamilyInfo(iReference: FNSFontReference; oFamilyName: StringPtr; oFamilyNameScript: ScriptCodePtr; oActualStyle: FMFontStylePtr): OSStatus; external name '_FNSReferenceGetFamilyInfo';

{
 *  FNSReferenceCountNames()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in FontSyncLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FNSReferenceCountNames(iReference: FNSFontReference; var oNameCount: ItemCount): OSStatus; external name '_FNSReferenceCountNames';

{
 *  FNSReferenceGetIndName()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in FontSyncLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FNSReferenceGetIndName(iReference: FNSFontReference; iFontNameIndex: ItemCount; iMaximumNameLength: ByteCount; oName: Ptr; oActualNameLength: ByteCountPtr; oFontNameCode: FontNameCodePtr; oFontNamePlatform: FontPlatformCodePtr; oFontNameScript: FontScriptCodePtr; oFontNameLanguage: FontLanguageCodePtr): OSStatus; external name '_FNSReferenceGetIndName';

{
 *  FNSReferenceFindName()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in FontSyncLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FNSReferenceFindName(iReference: FNSFontReference; iFontNameCode: FontNameCode; iFontNamePlatform: FontPlatformCode; iFontNameScript: FontScriptCode; iFontNameLanguage: FontLanguageCode; iMaximumNameLength: ByteCount; oName: Ptr; oActualNameLength: ByteCountPtr; oFontNameIndex: ItemCountPtr): OSStatus; external name '_FNSReferenceFindName';

{ Miscellany }
{
 *  FNSEnabled()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in FontSyncLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FNSEnabled: boolean; external name '_FNSEnabled';

{$ALIGN MAC68K}


end.
