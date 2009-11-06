{
     File:       LangAnalysis/LanguageAnalysis.h
 
     Contains:   Language Analysis Manager Interfaces
 
     Version:    LanguageAnalysis-214~9
 
     Copyright:  © 1996-2008 by Apple Inc., all rights reserved
 
     Bugs?:      For bug reports, consult the following page on
                 the World Wide Web:
 
                     http://www.freepascal.org/bugs.html
 
}
{       Pascal Translation Updated:  Jonas Maebe, <jonas@freepascal.org>, October 2009 }
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

unit LanguageAnalysis;
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
uses MacTypes,AEDataModel,Files,AERegistry,Dictionary,TextCommon,MacErrors;
{$endc} {not MACOSALLINCLUDE}


{$ifc TARGET_OS_MAC}

{$ALIGN POWER}

type
	LAEnvironmentRef = ^OpaqueLAEnvironmentRef; { an opaque type }
	OpaqueLAEnvironmentRef = record end;
	LAEnvironmentRefPtr = ^LAEnvironmentRef;  { when a var xx:LAEnvironmentRef parameter can be nil, it is changed to xx: LAEnvironmentRefPtr }
	LAContextRef = ^OpaqueLAContextRef; { an opaque type }
	OpaqueLAContextRef = record end;
	LAContextRefPtr = ^LAContextRef;  { when a var xx:LAContextRef parameter can be nil, it is changed to xx: LAContextRefPtr }
	LAPropertyKey = AEKeyword;
	LAPropertyType = DescType;
{
    Data structure for high level API
}
type
	LAMorphemeRecPtr = ^LAMorphemeRec;
	LAMorphemeRec = record
		sourceTextLength: UInt32;
		sourceTextPtr: LogicalAddress;
		morphemeTextLength: UInt32;
		morphemeTextPtr: LogicalAddress;
		partOfSpeech: UInt32;
	end;
type
	LAMorphemesArrayPtr = ^LAMorphemesArray;
	LAMorphemesArray = record
		morphemesCount: ItemCount;
		processedTextLength: UInt32;
		morphemesTextLength: UInt32;
		morphemes: array [0..1-1] of LAMorphemeRec;
	end;
const
	kLAMorphemesArrayVersion = 0;

{
    Definitions for result path/bundle structure
}
type
	LAMorphemeBundle = AERecord;
	LAMorphemeBundlePtr = ^LAMorphemeBundle;
	LAMorphemePath = AERecord;
	LAMorphemePathPtr = ^LAMorphemePath;
	LAMorpheme = AERecord;
	LAMorphemePtr = ^LAMorpheme;
	LAHomograph = AERecord;
	LAHomographPtr = ^LAHomograph;
const
	keyAELAMorphemeBundle = FourCharCode('lmfb');
	keyAELAMorphemePath = FourCharCode('lmfp');
	keyAELAMorpheme = FourCharCode('lmfn');
	keyAELAHomograph = FourCharCode('lmfh');

const
	typeLAMorphemeBundle = typeAERecord;
	typeLAMorphemePath = typeAERecord;
	typeLAMorpheme = typeAEList;
	typeLAHomograph = typeAEList;

{
    Definitions for morpheme/homograph information
}
const
	keyAEMorphemePartOfSpeechCode = FourCharCode('lamc');
	keyAEMorphemeTextRange = FourCharCode('lamt');

const
	typeAEMorphemePartOfSpeechCode = FourCharCode('lamc');
	typeAEMorphemeTextRange = FourCharCode('lamt');

type
	MorphemePartOfSpeech = UInt32;
	MorphemeTextRange = record
		sourceOffset: UInt32;
		length: UInt32;
	end;
{
    Mask for High level API convert flags 
}
const
	kLAEndOfSourceTextMask = $00000001;

{
    Constants for leading/trailing path of analysis function
}
const
	kLADefaultEdge = 0;
	kLAFreeEdge = 1;
	kLAIncompleteEdge = 2;

{
    Constants for confirm and shift function
}
const
	kLAAllMorphemes = 0;

{$ifc not TARGET_CPU_64}
{
    Library version
}
{$ifc not TARGET_CPU_64}
{
 *  LALibraryVersion()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    The Language Analysis Manager is deprecated. Use
 *    CFStringTokenizer instead.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in LanguageAnalysisLib 1.0 and later
 }
function LALibraryVersion: UInt32; external name '_LALibraryVersion';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
    High level API
}
{
 *  LATextToMorphemes()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    The Language Analysis Manager is deprecated. Use
 *    CFStringTokenizer instead.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in LanguageAnalysisLib 1.0 and later
 }
function LATextToMorphemes( context: LAContextRef; preferedEncoding: TextEncoding; textLength: ByteCount; sourceText: ConstLogicalAddress; bufferSize: ByteCount; convertFlags: OptionBits; structureVersion: UInt32; var acceptedLength: ByteCount; resultBuffer: LAMorphemesArrayPtr ): OSStatus; external name '_LATextToMorphemes';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
    Handling Context
}
{
 *  LAOpenAnalysisContext()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    The Language Analysis Manager is deprecated. Use
 *    CFStringTokenizer instead.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in LanguageAnalysisLib 1.0 and later
 }
function LAOpenAnalysisContext( environ: LAEnvironmentRef; var context: LAContextRef ): OSStatus; external name '_LAOpenAnalysisContext';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
 *  LACloseAnalysisContext()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    The Language Analysis Manager is deprecated. Use
 *    CFStringTokenizer instead.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in LanguageAnalysisLib 1.0 and later
 }
function LACloseAnalysisContext( context: LAContextRef ): OSStatus; external name '_LACloseAnalysisContext';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
    Handling Environment
}
{
 *  LAGetEnvironmentList()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    The Language Analysis Manager is deprecated. Use
 *    CFStringTokenizer instead.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in LanguageAnalysisLib 1.0 and later
 }
function LAGetEnvironmentList( maxCount: UInt32; var actualCount: UInt32; environmentList: {variable-size-array} LAEnvironmentRefPtr ): OSStatus; external name '_LAGetEnvironmentList';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
 *  LAGetEnvironmentName()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    The Language Analysis Manager is deprecated. Use
 *    CFStringTokenizer instead.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in LanguageAnalysisLib 1.0 and later
 }
function LAGetEnvironmentName( environment: LAEnvironmentRef; var environmentName: Str63 ): OSStatus; external name '_LAGetEnvironmentName';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
 *  LAGetEnvironmentRef()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    The Language Analysis Manager is deprecated. Use
 *    CFStringTokenizer instead.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in LanguageAnalysisLib 1.0 and later
 }
function LAGetEnvironmentRef( const (*var*) targetEnvironmentName: Str63; var environment: LAEnvironmentRef ): OSStatus; external name '_LAGetEnvironmentRef';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
 *  LACreateCustomEnvironment()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in LanguageAnalysisLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function LACreateCustomEnvironment(baseEnvironment: LAEnvironmentRef; const (*var*) newEnvironmentName: Str63; persistent: Boolean; var newEnvironment: LAEnvironmentRef): OSStatus; external name '_LACreateCustomEnvironment';


{
 *  LADeleteCustomEnvironment()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    The Language Analysis Manager is deprecated. Use
 *    CFStringTokenizer instead.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in LanguageAnalysisLib 1.0 and later
 }
function LADeleteCustomEnvironment( environment: LAEnvironmentRef ): OSStatus; external name '_LADeleteCustomEnvironment';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
    Handling dictionries
}
{
 *  LAOpenDictionary()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    The Language Analysis Manager is deprecated. Use
 *    CFStringTokenizer instead.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in LanguageAnalysisLib 1.0 and later
 }
function LAOpenDictionary( environ: LAEnvironmentRef; const (*var*) dictionary: FSSpec ): OSStatus; external name '_LAOpenDictionary';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
 *  LACloseDictionary()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    The Language Analysis Manager is deprecated. Use
 *    CFStringTokenizer instead.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in LanguageAnalysisLib 1.0 and later
 }
function LACloseDictionary( environ: LAEnvironmentRef; const (*var*) dictionary: FSSpec ): OSStatus; external name '_LACloseDictionary';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
 *  LAListAvailableDictionaries()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    The Language Analysis Manager is deprecated. Use
 *    CFStringTokenizer instead.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in LanguageAnalysisLib 1.0 and later
 }
function LAListAvailableDictionaries( environ: LAEnvironmentRef; maxCount: ItemCount; var actualCount: ItemCount; dictionaryList: {variable-size-array} FSSpecPtr; opened: {variable-size-array} BooleanPtr ): OSStatus; external name '_LAListAvailableDictionaries';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
 *  LAAddNewWord()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    The Language Analysis Manager is deprecated. Use
 *    CFStringTokenizer instead.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in LanguageAnalysisLib 1.0 and later
 }
function LAAddNewWord( environ: LAEnvironmentRef; const (*var*) dictionary: FSSpec; const (*var*) dataList: AEDesc ): OSStatus; external name '_LAAddNewWord';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
    Analyzing text
}
{
 *  LAMorphemeAnalysis()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    The Language Analysis Manager is deprecated. Use
 *    CFStringTokenizer instead.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in LanguageAnalysisLib 1.0 and later
 }
function LAMorphemeAnalysis( context: LAContextRef; text: ConstUniCharArrayPtr; textLength: UniCharCount; var leadingPath: LAMorphemePath; var trailingPath: LAMorphemePath; pathCount: ItemCount; var result: LAMorphemeBundle ): OSStatus; external name '_LAMorphemeAnalysis';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
 *  LAContinuousMorphemeAnalysis()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    The Language Analysis Manager is deprecated. Use
 *    CFStringTokenizer instead.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in LanguageAnalysisLib 1.0 and later
 }
function LAContinuousMorphemeAnalysis( context: LAContextRef; text: ConstUniCharArrayPtr; textLength: UniCharCount; incrementalText: Boolean; var leadingPath: LAMorphemePath; var trailingPath: LAMorphemePath; var modified: Boolean ): OSStatus; external name '_LAContinuousMorphemeAnalysis';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
 *  LAGetMorphemes()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    The Language Analysis Manager is deprecated. Use
 *    CFStringTokenizer instead.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in LanguageAnalysisLib 1.0 and later
 }
function LAGetMorphemes( context: LAContextRef; var result: LAMorphemePath ): OSStatus; external name '_LAGetMorphemes';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
 *  LAShiftMorphemes()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    The Language Analysis Manager is deprecated. Use
 *    CFStringTokenizer instead.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in LanguageAnalysisLib 1.0 and later
 }
function LAShiftMorphemes( context: LAContextRef; morphemeCount: ItemCount; var path: LAMorphemePath; var shiftedLength: UniCharCount ): OSStatus; external name '_LAShiftMorphemes';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
 *  LAResetAnalysis()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    The Language Analysis Manager is deprecated. Use
 *    CFStringTokenizer instead.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in LanguageAnalysisLib 1.0 and later
 }
function LAResetAnalysis( context: LAContextRef ): OSStatus; external name '_LAResetAnalysis';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{$endc} {not TARGET_CPU_64}

{$endc} {not TARGET_CPU_64}

{
    Check Language Analysis Manager availability
}
{$ifc TARGET_RT_MAC_CFM}
{
        LALanguageAnalysisAvailable() is a macro available only in C/C++.  
        To get the same functionality from pascal or assembly, you need
        to test if Language Analysis Manager functions are not NULL.
        For instance:
        
            IF @LALibraryVersion <> kUnresolvedCFragSymbolAddress THEN
                gLanguageAnalysisAvailable = TRUE;
            ELSE
                gLanguageAnalysisAvailable = FALSE;
            end
    
}
{$elsec}
  {$ifc TARGET_RT_MAC_MACHO}
{ Language Analysis Manager is always available on OS X }
  {$endc}
{$endc}

{
=============================================================================================
    Definitions for Japanese Analysis Module
=============================================================================================
}
{
    Names for default environments for Japanese analysis
}
const
	kLAJapaneseKanaKanjiEnvironment = 'KanaKanjiConversion';
const
	kLAJapaneseMorphemeAnalysisEnvironment = 'JapaneseMorphemeAnalysis';
const
	kLAJapaneseTTSEnvironment = 'JapaneseTextToSpeech';
{
    File cretor for dictionary of Apple Japanese access method
}
const
	kAppleJapaneseDictionarySignature = FourCharCode('jlan');

{
    Engine limitations
}
const
	kMaxInputLengthOfAppleJapaneseEngine = 200;

{
    Definitions of information in the path/bundle
}

type
	JapanesePartOfSpeech = MorphemePartOfSpeech;
	HomographWeight = UInt16;
	HomographAccent = UInt8;
{
    AE keywords and type definitions for morpheme/homograph information
}
const
	keyAEHomographDicInfo = FourCharCode('lahd');
	keyAEHomographWeight = FourCharCode('lahw');
	keyAEHomographAccent = FourCharCode('laha');

const
	typeAEHomographDicInfo = FourCharCode('lahd');
	typeAEHomographWeight = typeSInt16;
	typeAEHomographAccent = FourCharCode('laha');

{
    Structure for dictionary information of homograph
}
type
	HomographDicInfoRecPtr = ^HomographDicInfoRec;
	HomographDicInfoRec = record
		dictionaryID: DCMDictionaryID;
		uniqueID: DCMUniqueID;
	end;
{
=============================================================================================
    Definitions for Japanese part of speeches
=============================================================================================
}
{
    Masks for part of speeches
}
const
	kLASpeechRoughClassMask = $0000F000;
	kLASpeechMediumClassMask = $0000FF00;
	kLASpeechStrictClassMask = $0000FFF0;
	kLASpeechKatsuyouMask = $0000000F;


{
    Part of speeches
}
const
	kLASpeechMeishi = $00000000; { noun }
	kLASpeechFutsuuMeishi = $00000000; { general noun }
	kLASpeechJinmei = $00000100; { person name }
	kLASpeechJinmeiSei = $00000110; { family name }
	kLASpeechJinmeiMei = $00000120; { first name }
	kLASpeechChimei = $00000200; { place name }
	kLASpeechSetsubiChimei = $00000210; { place name with suffix }
	kLASpeechSoshikimei = $00000300; { organization name }
	kLASpeechKoyuuMeishi = $00000400; { proper noun }
	kLASpeechSahenMeishi = $00000500; { special noun }
	kLASpeechKeidouMeishi = $00000600; { special noun }
	kLASpeechRentaishi = $00001000;
	kLASpeechFukushi = $00002000; { adverb }
	kLASpeechSetsuzokushi = $00003000; { conjunction }
	kLASpeechKandoushi = $00004000;
	kLASpeechDoushi = $00005000; { verb }
	kLASpeechGodanDoushi = $00005000;
	kLASpeechKagyouGodan = $00005000;
	kLASpeechSagyouGodan = $00005010;
	kLASpeechTagyouGodan = $00005020;
	kLASpeechNagyouGodan = $00005030;
	kLASpeechMagyouGodan = $00005040;
	kLASpeechRagyouGodan = $00005050;
	kLASpeechWagyouGodan = $00005060;
	kLASpeechGagyouGodan = $00005070;
	kLASpeechBagyouGodan = $00005080;
	kLASpeechIchidanDoushi = $00005100;
	kLASpeechKahenDoushi = $00005200;
	kLASpeechSahenDoushi = $00005300;
	kLASpeechZahenDoushi = $00005400;
	kLASpeechKeiyoushi = $00006000; { adjective }
	kLASpeechKeiyoudoushi = $00007000;
	kLASpeechSettougo = $00008000; { prefix}
	kLASpeechSuujiSettougo = $00008100; { prefix for numbers }
	kLASpeechSetsubigo = $00009000; { suffix }
	kLASpeechJinmeiSetsubigo = $00009100; { suffix for person name }
	kLASpeechChimeiSetsubigo = $00009200; { suffix for place name }
	kLASpeechSoshikimeiSetsubigo = $00009300; { suffix for organization name }
	kLASpeechSuujiSetsubigo = $00009400; { suffix for numbers }
	kLASpeechMuhinshi = $0000A000; { no category }
	kLASpeechTankanji = $0000A000; { character }
	kLASpeechKigou = $0000A100; { symbol }
	kLASpeechKuten = $0000A110;
	kLASpeechTouten = $0000A120;
	kLASpeechSuushi = $0000A200; { numbers }
	kLASpeechDokuritsugo = $0000A300;
	kLASpeechSeiku = $0000A400;
	kLASpeechJodoushi = $0000B000; { auxiliary verb }
	kLASpeechJoshi = $0000C000; { postpositional particle }


{
    Conjugations
 }
const
	kLASpeechKatsuyouGokan = $00000001; { stem }
	kLASpeechKatsuyouMizen = $00000002;
	kLASpeechKatsuyouRenyou = $00000003;
	kLASpeechKatsuyouSyuushi = $00000004;
	kLASpeechKatsuyouRentai = $00000005;
	kLASpeechKatsuyouKatei = $00000006;
	kLASpeechKatsuyouMeirei = $00000007;


{$endc} {TARGET_OS_MAC}

{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}

end.
{$endc} {not MACOSALLINCLUDE}
