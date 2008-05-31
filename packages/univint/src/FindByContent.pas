{
     File:       FindByContent.p
 
     Contains:   Public search interface for the Find by Content shared library
 
     Version:    Technology: 2.0
                 Release:    Universal Interfaces 3.4.2
 
     Copyright:  © 1997-2002 by Apple Computer, Inc., all rights reserved.
 
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

unit FindByContent;
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
uses MacTypes,CFBase,Files,MacErrors,CFString;

{$ALIGN MAC68K}


{
   ***************************************************************************
   Language constants used with FBCIndexItemsInLanguages: these numbers are bits
   in a 64-bit array that consists of two UInt32 words.  In the current implementation
   the low word is always 0, so values for the high word are given.  If both UInt32
   words are 0, the default value of kDefaultLanguagesHighWord is used.
   ***************************************************************************
}

const
																{  languages that use the Roman character mapping }
	englishHighWord				= $80000000;
	dutchHighWord				= $40000000;					{  also Afrikaans }
	germanHighWord				= $20000000;
	swedishHighWord				= $10000000;					{  also Norwegian }
	danishHighWord				= $08000000;
	spanishHighWord				= $04000000;					{  also Catalan }
	portugueseHighWord			= $02000000;
	italianHighWord				= $01000000;
	frenchHighWord				= $00800000;
	romanHighWord				= $00400000;					{  other languages using Roman alphabet }
																{  Languages that use other mappings }
	icelandicHighWord			= $00200000;					{  also Faroese }
	hebrewHighWord				= $00100000;					{  also Yiddish }
	arabicHighWord				= $00080000;					{  also Farsi, Urdu }
	centeuroHighWord			= $00040000;					{  Central European languages not using Cyrillic }
	croatianHighWord			= $00020000;
	turkishHighWord				= $00010000;
	romanianHighWord			= $00008000;
	greekHighWord				= $00004000;
	cyrillicHighWord			= $00002000;					{  all languages using Cyrillic }
	devanagariHighWord			= $00001000;
	gujuratiHighWord			= $00000800;
	gurmukhiHighWord			= $00000400;
	japaneseHighWord			= $00000200;
	koreanHighWord				= $00000100;
	kDefaultLanguagesHighWord	= $FF800000;					{  sum of first 9 }


	{
	   ***************************************************************************
	   Phase values
	   These values are passed to the client's callback function to indicate what
	   the FBC code is doing.
	   ***************************************************************************
	}
																{  indexing phases }
	kFBCphIndexing				= 0;
	kFBCphFlushing				= 1;
	kFBCphMerging				= 2;
	kFBCphMakingIndexAccessor	= 3;
	kFBCphCompacting			= 4;
	kFBCphIndexWaiting			= 5;							{  access phases }
	kFBCphSearching				= 6;
	kFBCphMakingAccessAccessor	= 7;
	kFBCphAccessWaiting			= 8;							{  summarization }
	kFBCphSummarizing			= 9;							{  indexing or access }
	kFBCphIdle					= 10;
	kFBCphCanceling				= 11;


	{
	   ***************************************************************************
	   Pointer types
	   These point to memory allocated by the FBC shared library, and must be deallocated
	   by calls that are defined below.
	   ***************************************************************************
	}

	{  A collection of state information for searching }

type
	FBCSearchSession    = ^SInt32; { an opaque 32-bit type }
	FBCSearchSessionPtr = ^FBCSearchSession;  { when a var xx:FBCSearchSession parameter can be nil, it is changed to xx: FBCSearchSessionPtr }
	{  a FBCWordList is a pointer to an array of pointers to c-strings }
	FBCWordListRecPtr = ^FBCWordListRec;
	FBCWordListRec = record
		words:					array [0..0] of ConstCStringPtr;		{  array of pointers to c-strings }
	end;

	FBCWordList							= ^FBCWordListRec;
	{
	   ***************************************************************************
	   Callback function type for progress reporting and cancelation during
	   searching and indexing.  The client's callback function should call
	   WaitNextEvent; a "sleep" value of 1 is suggested.  If the callback function
	   wants to cancel the current operation (indexing, search, or doc-terms
	   retrieval) it should return true.
	   ***************************************************************************
	}

{$ifc TYPED_FUNCTION_POINTERS}
	FBCCallbackProcPtr = function(phase: UInt16; percentDone: Single; data: UnivPtr): boolean;
{$elsec}
	FBCCallbackProcPtr = ProcPtr;
{$endc}

{$ifc OPAQUE_UPP_TYPES}
	FBCCallbackUPP = ^SInt32; { an opaque UPP }
{$elsec}
	FBCCallbackUPP = FBCCallbackProcPtr;
{$endc}	

const
	uppFBCCallbackProcInfo = $00000F91;
	{
	 *  NewFBCCallbackUPP()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   available as macro/inline
	 *    CarbonLib:        in CarbonLib 1.0.2 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
function NewFBCCallbackUPP(userRoutine: FBCCallbackProcPtr): FBCCallbackUPP; external name '_NewFBCCallbackUPP';
{
 *  DisposeFBCCallbackUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeFBCCallbackUPP(userUPP: FBCCallbackUPP); external name '_DisposeFBCCallbackUPP';
{
 *  InvokeFBCCallbackUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 }
function InvokeFBCCallbackUPP(phase: UInt16; percentDone: Single; data: UnivPtr; userRoutine: FBCCallbackUPP): boolean; external name '_InvokeFBCCallbackUPP';
{
   ***************************************************************************
   Set the callback function for progress reporting and cancelation during
   searching and indexing, and set the amount of heap space to reserve for
   the client's use when FBC allocates memory.
   ***************************************************************************
}
{
 *  FBCSetCallback()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in FindByContent 8.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure FBCSetCallback(fn: FBCCallbackUPP; data: UnivPtr); external name '_FBCSetCallback';

{
 *  FBCSetHeapReservation()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in FindByContent 8.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure FBCSetHeapReservation(bytes: UInt32); external name '_FBCSetHeapReservation';

{
   ***************************************************************************
   Find out whether a volume is indexed, the date & time of its last
   completed  update, and its physical size.
   ***************************************************************************
}

{
 *  FBCVolumeIsIndexed()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in FindByContent 8.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FBCVolumeIsIndexed(theVRefNum: SInt16): boolean; external name '_FBCVolumeIsIndexed';

{
 *  FBCVolumeIsRemote()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in FindByContent 8.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FBCVolumeIsRemote(theVRefNum: SInt16): boolean; external name '_FBCVolumeIsRemote';

{
 *  FBCVolumeIndexTimeStamp()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in FindByContent 8.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FBCVolumeIndexTimeStamp(theVRefNum: SInt16; var timeStamp: UInt32): OSErr; external name '_FBCVolumeIndexTimeStamp';

{
 *  FBCVolumeIndexPhysicalSize()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in FindByContent 8.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FBCVolumeIndexPhysicalSize(theVRefNum: SInt16; var size: UInt32): OSErr; external name '_FBCVolumeIndexPhysicalSize';

{
   ***************************************************************************
   Create & configure a search session
   ***************************************************************************
}

{
 *  FBCCreateSearchSession()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in FindByContent 8.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FBCCreateSearchSession(var searchSession: FBCSearchSession): OSErr; external name '_FBCCreateSearchSession';

{
 *  FBCAddAllVolumesToSession()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in FindByContent 8.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FBCAddAllVolumesToSession(theSession: FBCSearchSession; includeRemote: boolean): OSErr; external name '_FBCAddAllVolumesToSession';

{
 *  FBCSetSessionVolumes()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in FindByContent 8.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FBCSetSessionVolumes(theSession: FBCSearchSession; vRefNums: SInt16Ptr; numVolumes: UInt16): OSErr; external name '_FBCSetSessionVolumes';

{
 *  FBCAddVolumeToSession()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in FindByContent 8.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FBCAddVolumeToSession(theSession: FBCSearchSession; vRefNum: SInt16): OSErr; external name '_FBCAddVolumeToSession';

{
 *  FBCRemoveVolumeFromSession()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in FindByContent 8.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FBCRemoveVolumeFromSession(theSession: FBCSearchSession; vRefNum: SInt16): OSErr; external name '_FBCRemoveVolumeFromSession';

{
 *  FBCGetSessionVolumeCount()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in FindByContent 8.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FBCGetSessionVolumeCount(theSession: FBCSearchSession; var count: UInt16): OSErr; external name '_FBCGetSessionVolumeCount';

{
 *  FBCGetSessionVolumes()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in FindByContent 8.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FBCGetSessionVolumes(theSession: FBCSearchSession; var vRefNums: SInt16; var numVolumes: UInt16): OSErr; external name '_FBCGetSessionVolumes';

{
 *  FBCCloneSearchSession()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in FindByContent 8.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FBCCloneSearchSession(original: FBCSearchSession; var clone: FBCSearchSession): OSErr; external name '_FBCCloneSearchSession';

{
   ***************************************************************************
   Execute a search
   ***************************************************************************
}

{
 *  FBCDoQuerySearch()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in FindByContent 8.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FBCDoQuerySearch(theSession: FBCSearchSession; queryText: CStringPtr; const (*var*) targetDirs: FSSpec; numTargets: UInt32; maxHits: UInt32; maxHitWords: UInt32): OSErr; external name '_FBCDoQuerySearch';

{
 *  FBCDoCFStringSearch()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Mac OS X:         in version 10.0 and later
 }
function FBCDoCFStringSearch(theSession: FBCSearchSession; queryString: CFStringRef; const (*var*) targetDirs: FSSpec; numTargets: UInt32; maxHits: UInt32; maxHitWords: UInt32): OSErr; external name '_FBCDoCFStringSearch';

{
 *  FBCDoExampleSearch()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in FindByContent 8.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FBCDoExampleSearch(theSession: FBCSearchSession; exampleHitNums: UInt32Ptr; numExamples: UInt32; const (*var*) targetDirs: FSSpec; numTargets: UInt32; maxHits: UInt32; maxHitWords: UInt32): OSErr; external name '_FBCDoExampleSearch';

{
 *  FBCBlindExampleSearch()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in FindByContent 8.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FBCBlindExampleSearch(var examples: FSSpec; numExamples: UInt32; const (*var*) targetDirs: FSSpec; numTargets: UInt32; maxHits: UInt32; maxHitWords: UInt32; allIndexes: boolean; includeRemote: boolean; var theSession: FBCSearchSession): OSErr; external name '_FBCBlindExampleSearch';


{
   ***************************************************************************
   Get information about hits [wrapper for THitItem C++ API]
   ***************************************************************************
}

{
 *  FBCGetHitCount()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in FindByContent 8.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FBCGetHitCount(theSession: FBCSearchSession; var count: UInt32): OSErr; external name '_FBCGetHitCount';

{
 *  FBCGetHitDocument()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in FindByContent 8.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FBCGetHitDocument(theSession: FBCSearchSession; hitNumber: UInt32; var theDocument: FSSpec): OSErr; external name '_FBCGetHitDocument';

{
 *  FBCGetHitDocumentRef()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Mac OS X:         in version 10.0 and later
 }
function FBCGetHitDocumentRef(theSession: FBCSearchSession; hitNumber: UInt32; var theDocument: FSRef): OSErr; external name '_FBCGetHitDocumentRef';

{
 *  FBCGetHitScore()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in FindByContent 8.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FBCGetHitScore(theSession: FBCSearchSession; hitNumber: UInt32; var score: Single): OSErr; external name '_FBCGetHitScore';

{
 *  FBCGetMatchedWords()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in FindByContent 8.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FBCGetMatchedWords(theSession: FBCSearchSession; hitNumber: UInt32; var wordCount: UInt32; var list: FBCWordList): OSErr; external name '_FBCGetMatchedWords';

{
 *  FBCGetTopicWords()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in FindByContent 8.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FBCGetTopicWords(theSession: FBCSearchSession; hitNumber: UInt32; var wordCount: UInt32; var list: FBCWordList): OSErr; external name '_FBCGetTopicWords';


{
   ***************************************************************************
   Summarize a buffer of text
   ***************************************************************************
}

{
 *  FBCSummarize()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in FindByContent 8.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FBCSummarize(inBuf: UnivPtr; inLength: UInt32; outBuf: UnivPtr; var outLength: UInt32; var numSentences: UInt32): OSErr; external name '_FBCSummarize';

{
   ***************************************************************************
   Deallocate hit lists, word arrays, and search sessions
   ***************************************************************************
}

{
 *  FBCReleaseSessionHits()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in FindByContent 8.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FBCReleaseSessionHits(theSession: FBCSearchSession): OSErr; external name '_FBCReleaseSessionHits';

{
 *  FBCDestroyWordList()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in FindByContent 8.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FBCDestroyWordList(theList: FBCWordList; wordCount: UInt32): OSErr; external name '_FBCDestroyWordList';

{
 *  FBCDestroySearchSession()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in FindByContent 8.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FBCDestroySearchSession(theSession: FBCSearchSession): OSErr; external name '_FBCDestroySearchSession';

{
   ***************************************************************************
   Index one or more files and/or folders
   ***************************************************************************
}

{
 *  FBCIndexItems()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in FindByContent 9.0 and later
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Mac OS X:         in version 10.0 and later
 }
function FBCIndexItems(theItems: FSSpecArrayPtr; itemCount: UInt32): OSErr; external name '_FBCIndexItems';

{
 *  FBCIndexItemsInLanguages()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Mac OS X:         in version 10.0 and later
 }
function FBCIndexItemsInLanguages(theItems: FSSpecArrayPtr; itemCount: UInt32; languageHighBits: UInt32; languageLowBits: UInt32): OSErr; external name '_FBCIndexItemsInLanguages';

{
   ***************************************************************************
   (OS X only) Given a folder, find the folder that contains the index file
   of the given index
   ***************************************************************************
}

{
 *  FBCFindIndexFileFolderForFolder()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Mac OS X:         in version 10.0 and later
 }
function FBCFindIndexFileFolderForFolder(var inFolder: FSRef; var outFolder: FSRef): OSErr; external name '_FBCFindIndexFileFolderForFolder';

{
   ***************************************************************************
   (OS X only) Given a folder, delete the index file that indexes it
   ***************************************************************************
}

{
 *  FBCDeleteIndexFileForFolder()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Mac OS X:         in version 10.0 and later
 }
function FBCDeleteIndexFileForFolder(const (*var*) folder: FSRef): OSErr; external name '_FBCDeleteIndexFileForFolder';

{$ALIGN MAC68K}


end.
