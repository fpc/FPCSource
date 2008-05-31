{
     File:       LanguageAnalysis.p
 
     Contains:   Language Analysis Manager Interfaces
 
     Version:    Technology: Mac OS 8
                 Release:    Universal Interfaces 3.4.2
 
     Copyright:  © 1996-2002 by Apple Computer, Inc., all rights reserved
 
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

unit LanguageAnalysis;
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
uses MacTypes,AEDataModel,Files,AERegistry,Dictionary,TextCommon,MacErrors;


{$ALIGN POWER}


type
	LAEnvironmentRef    = ^SInt32; { an opaque 32-bit type }
	LAEnvironmentRefPtr = ^LAEnvironmentRef;  { when a var xx:LAEnvironmentRef parameter can be nil, it is changed to xx: LAEnvironmentRefPtr }
	LAContextRef    = ^SInt32; { an opaque 32-bit type }
	LAContextRefPtr = ^LAContextRef;  { when a var xx:LAContextRef parameter can be nil, it is changed to xx: LAContextRefPtr }
	LAPropertyKey						= AEKeyword;
	LAPropertyType						= DescType;
	{	
	    Data structure for high level API
		}
	LAMorphemeRecPtr = ^LAMorphemeRec;
	LAMorphemeRec = record
		sourceTextLength:		ByteCount;
		sourceTextPtr:			LogicalAddress;
		morphemeTextLength:		ByteCount;
		morphemeTextPtr:		LogicalAddress;
		partOfSpeech:			UInt32;
	end;

	LAMorphemesArrayPtr = ^LAMorphemesArray;
	LAMorphemesArray = record
		morphemesCount:			ItemCount;
		processedTextLength:	ByteCount;
		morphemesTextLength:	ByteCount;
		morphemes:				array [0..0] of LAMorphemeRec;
	end;


const
	kLAMorphemesArrayVersion	= 0;

	{	
	    Definitions for result path/bundle structure
		}

type
	LAMorphemeBundle					= AERecord;
	LAMorphemeBundlePtr 				= ^LAMorphemeBundle;
	LAMorphemePath						= AERecord;
	LAMorphemePathPtr 					= ^LAMorphemePath;
	LAMorpheme							= AERecord;
	LAMorphemePtr 						= ^LAMorpheme;
	LAHomograph							= AERecord;
	LAHomographPtr 						= ^LAHomograph;

const
	keyAELAMorphemeBundle		= FourCharCode('lmfb');
	keyAELAMorphemePath			= FourCharCode('lmfp');
	keyAELAMorpheme				= FourCharCode('lmfn');
	keyAELAHomograph			= FourCharCode('lmfh');

	typeLAMorphemeBundle		= FourCharCode('reco');
	typeLAMorphemePath			= FourCharCode('reco');
	typeLAMorpheme				= FourCharCode('list');
	typeLAHomograph				= FourCharCode('list');

	{	
	    Definitions for morpheme/homograph information
		}
	keyAEMorphemePartOfSpeechCode = FourCharCode('lamc');
	keyAEMorphemeTextRange		= FourCharCode('lamt');

	typeAEMorphemePartOfSpeechCode = FourCharCode('lamc');
	typeAEMorphemeTextRange		= FourCharCode('lamt');


type
	MorphemePartOfSpeech				= UInt32;
	MorphemeTextRangePtr = ^MorphemeTextRange;
	MorphemeTextRange = record
		sourceOffset:			UInt32;
		length:					UInt32;
	end;

	{	
	    Mask for High level API convert flags 
		}

const
	kLAEndOfSourceTextMask		= $00000001;

	{	
	    Constants for leading/trailing path of analysis function
		}
	kLADefaultEdge				= 0;
	kLAFreeEdge					= 1;
	kLAIncompleteEdge			= 2;

	{	
	    Constants for confirm and shift function
		}
	kLAAllMorphemes				= 0;


	{	
	    Library version
		}
	{
	 *  LALibraryVersion()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in LanguageAnalysisLib 1.0 and later
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
function LALibraryVersion: UInt32; external name '_LALibraryVersion';

{
    High level API
}
{
 *  LATextToMorphemes()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in LanguageAnalysisLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function LATextToMorphemes(context: LAContextRef; preferedEncoding: TextEncoding; textLength: ByteCount; sourceText: ConstLogicalAddress; bufferSize: ByteCount; convertFlags: OptionBits; structureVersion: UInt32; var acceptedLength: ByteCount; resultBuffer: LAMorphemesArrayPtr): OSStatus; external name '_LATextToMorphemes';

{
    Handling Context
}
{
 *  LAOpenAnalysisContext()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in LanguageAnalysisLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function LAOpenAnalysisContext(environ: LAEnvironmentRef; var context: LAContextRef): OSStatus; external name '_LAOpenAnalysisContext';

{
 *  LACloseAnalysisContext()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in LanguageAnalysisLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function LACloseAnalysisContext(context: LAContextRef): OSStatus; external name '_LACloseAnalysisContext';

{
    Handling Environment
}
{
 *  LAGetEnvironmentList()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in LanguageAnalysisLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function LAGetEnvironmentList(maxCount: UInt32; var actualCount: UInt32; var environmentList: LAEnvironmentRef): OSStatus; external name '_LAGetEnvironmentList';

{
 *  LAGetEnvironmentName()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in LanguageAnalysisLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function LAGetEnvironmentName(environment: LAEnvironmentRef; var environmentName: Str63): OSStatus; external name '_LAGetEnvironmentName';

{
 *  LAGetEnvironmentRef()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in LanguageAnalysisLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function LAGetEnvironmentRef(const (*var*) targetEnvironmentName: Str63; var environment: LAEnvironmentRef): OSStatus; external name '_LAGetEnvironmentRef';

{
 *  LACreateCustomEnvironment()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in LanguageAnalysisLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function LACreateCustomEnvironment(baseEnvironment: LAEnvironmentRef; const (*var*) newEnvironmentName: Str63; persistent: boolean; var newEnvironment: LAEnvironmentRef): OSStatus; external name '_LACreateCustomEnvironment';

{
 *  LADeleteCustomEnvironment()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in LanguageAnalysisLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function LADeleteCustomEnvironment(environment: LAEnvironmentRef): OSStatus; external name '_LADeleteCustomEnvironment';

{
    Handling dictionries
}
{
 *  LAOpenDictionary()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in LanguageAnalysisLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function LAOpenDictionary(environ: LAEnvironmentRef; const (*var*) dictionary: FSSpec): OSStatus; external name '_LAOpenDictionary';

{
 *  LACloseDictionary()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in LanguageAnalysisLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function LACloseDictionary(environ: LAEnvironmentRef; const (*var*) dictionary: FSSpec): OSStatus; external name '_LACloseDictionary';

{
 *  LAListAvailableDictionaries()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in LanguageAnalysisLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function LAListAvailableDictionaries(environ: LAEnvironmentRef; maxCount: ItemCount; var actualCount: ItemCount; var dictionaryList: FSSpec; var opened: boolean): OSStatus; external name '_LAListAvailableDictionaries';

{
 *  LAAddNewWord()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in LanguageAnalysisLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function LAAddNewWord(environ: LAEnvironmentRef; const (*var*) dictionary: FSSpec; const (*var*) dataList: AEDesc): OSStatus; external name '_LAAddNewWord';

{
    Analyzing text
}
{
 *  LAMorphemeAnalysis()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in LanguageAnalysisLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function LAMorphemeAnalysis(context: LAContextRef; text: ConstUniCharArrayPtr; textLength: UniCharCount; var leadingPath: LAMorphemePath; var trailingPath: LAMorphemePath; pathCount: ItemCount; var result: LAMorphemeBundle): OSStatus; external name '_LAMorphemeAnalysis';

{
 *  LAContinuousMorphemeAnalysis()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in LanguageAnalysisLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function LAContinuousMorphemeAnalysis(context: LAContextRef; text: ConstUniCharArrayPtr; textLength: UniCharCount; incrementalText: boolean; var leadingPath: LAMorphemePath; var trailingPath: LAMorphemePath; var modified: boolean): OSStatus; external name '_LAContinuousMorphemeAnalysis';

{
 *  LAGetMorphemes()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in LanguageAnalysisLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function LAGetMorphemes(context: LAContextRef; var result: LAMorphemePath): OSStatus; external name '_LAGetMorphemes';

{
 *  LAShiftMorphemes()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in LanguageAnalysisLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function LAShiftMorphemes(context: LAContextRef; morphemeCount: ItemCount; var path: LAMorphemePath; var shiftedLength: UniCharCount): OSStatus; external name '_LAShiftMorphemes';

{
 *  LAResetAnalysis()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in LanguageAnalysisLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function LAResetAnalysis(context: LAContextRef): OSStatus; external name '_LAResetAnalysis';

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
{
    File cretor for dictionary of Apple Japanese access method
}

const
	kAppleJapaneseDictionarySignature = FourCharCode('jlan');

	{	
	    Engine limitations
		}
	kMaxInputLengthOfAppleJapaneseEngine = 200;

	{	
	    Definitions of information in the path/bundle
		}

type
	JapanesePartOfSpeech				= MorphemePartOfSpeech;
	HomographWeight						= UInt16;
	HomographAccent						= UInt8;
	{	
	    AE keywords and type definitions for morpheme/homograph information
		}

const
	keyAEHomographDicInfo		= FourCharCode('lahd');
	keyAEHomographWeight		= FourCharCode('lahw');
	keyAEHomographAccent		= FourCharCode('laha');

	typeAEHomographDicInfo		= FourCharCode('lahd');
	typeAEHomographWeight		= FourCharCode('shor');
	typeAEHomographAccent		= FourCharCode('laha');

	{	
	    Structure for dictionary information of homograph
		}

type
	HomographDicInfoRecPtr = ^HomographDicInfoRec;
	HomographDicInfoRec = record
		dictionaryID:			DCMDictionaryID;
		uniqueID:				DCMUniqueID;
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
	kLASpeechRoughClassMask		= $0000F000;
	kLASpeechMediumClassMask	= $0000FF00;
	kLASpeechStrictClassMask	= $0000FFF0;
	kLASpeechKatsuyouMask		= $0000000F;


	{	
	    Part of speeches
		}
	kLASpeechMeishi				= $00000000;					{  noun  }
	kLASpeechFutsuuMeishi		= $00000000;					{  general noun  }
	kLASpeechJinmei				= $00000100;					{  person name  }
	kLASpeechJinmeiSei			= $00000110;					{  family name  }
	kLASpeechJinmeiMei			= $00000120;					{  first name  }
	kLASpeechChimei				= $00000200;					{  place name  }
	kLASpeechSetsubiChimei		= $00000210;					{  place name with suffix  }
	kLASpeechSoshikimei			= $00000300;					{  organization name  }
	kLASpeechKoyuuMeishi		= $00000400;					{  proper noun  }
	kLASpeechSahenMeishi		= $00000500;					{  special noun  }
	kLASpeechKeidouMeishi		= $00000600;					{  special noun  }
	kLASpeechRentaishi			= $00001000;
	kLASpeechFukushi			= $00002000;					{  adverb  }
	kLASpeechSetsuzokushi		= $00003000;					{  conjunction  }
	kLASpeechKandoushi			= $00004000;
	kLASpeechDoushi				= $00005000;					{  verb  }
	kLASpeechGodanDoushi		= $00005000;
	kLASpeechKagyouGodan		= $00005000;
	kLASpeechSagyouGodan		= $00005010;
	kLASpeechTagyouGodan		= $00005020;
	kLASpeechNagyouGodan		= $00005030;
	kLASpeechMagyouGodan		= $00005040;
	kLASpeechRagyouGodan		= $00005050;
	kLASpeechWagyouGodan		= $00005060;
	kLASpeechGagyouGodan		= $00005070;
	kLASpeechBagyouGodan		= $00005080;
	kLASpeechIchidanDoushi		= $00005100;
	kLASpeechKahenDoushi		= $00005200;
	kLASpeechSahenDoushi		= $00005300;
	kLASpeechZahenDoushi		= $00005400;
	kLASpeechKeiyoushi			= $00006000;					{  adjective  }
	kLASpeechKeiyoudoushi		= $00007000;
	kLASpeechSettougo			= $00008000;					{  prefix }
	kLASpeechSuujiSettougo		= $00008100;					{  prefix for numbers  }
	kLASpeechSetsubigo			= $00009000;					{  suffix  }
	kLASpeechJinmeiSetsubigo	= $00009100;					{  suffix for person name  }
	kLASpeechChimeiSetsubigo	= $00009200;					{  suffix for place name  }
	kLASpeechSoshikimeiSetsubigo = $00009300;					{  suffix for organization name  }
	kLASpeechSuujiSetsubigo		= $00009400;					{  suffix for numbers  }
	kLASpeechMuhinshi			= $0000A000;					{  no category  }
	kLASpeechTankanji			= $0000A000;					{  character  }
	kLASpeechKigou				= $0000A100;					{  symbol  }
	kLASpeechKuten				= $0000A110;
	kLASpeechTouten				= $0000A120;
	kLASpeechSuushi				= $0000A200;					{  numbers  }
	kLASpeechDokuritsugo		= $0000A300;
	kLASpeechSeiku				= $0000A400;
	kLASpeechJodoushi			= $0000B000;					{  auxiliary verb  }
	kLASpeechJoshi				= $0000C000;					{  postpositional particle  }


	{	
	    Conjugations
	 	}
	kLASpeechKatsuyouGokan		= $00000001;					{  stem  }
	kLASpeechKatsuyouMizen		= $00000002;
	kLASpeechKatsuyouRenyou		= $00000003;
	kLASpeechKatsuyouSyuushi	= $00000004;
	kLASpeechKatsuyouRentai		= $00000005;
	kLASpeechKatsuyouKatei		= $00000006;
	kLASpeechKatsuyouMeirei		= $00000007;


{$ALIGN MAC68K}


end.
