{
     File:       SearchKit/SKSummary.h
 
     Contains:   SearchKit Interfaces.
 
     Version:    SearchKit-407~38
 
     Copyright:  © 2004-2008 by Apple Computer, Inc., all rights reserved
 
     Bugs?:      For bug reports, consult the following page on
                 the World Wide Web:
 
                     http://developer.apple.com/bugreporter/
 
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

unit SKSummary;
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
uses MacTypes,CFBase,CFString;
{$endc} {not MACOSALLINCLUDE}

{$ALIGN POWER}

{$ifc TARGET_OS_MAC}
{
 * Summarization API
 }


{
 *  SKSummaryRef
 *  
 *  Summary:
 *    An opaque data type representing summary information.
 *  
 *  Discussion:
 *    A summary reference contains summary information, from which
 *    summary text can be obtained.
 }
type
	SKSummaryRef = ^__SKSummary; { an opaque type }
	__SKSummary = record end;
	SKSummaryRefPtr = ^SKSummaryRef;
{
 *  SKSummaryGetTypeID()
 *  
 *  Summary:
 *    Returns the type identifier of the SKSummaryRef type.
 *  
 *  Result:
 *    Returns a CFTypeID object, or <tt>NULL</tt> on failure.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function SKSummaryGetTypeID: CFTypeID; external name '_SKSummaryGetTypeID';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{
 *  SKSummaryCreateWithString()
 *  
 *  Summary:
 *    Creates a summary reference with text string.
 *  
 *  Discussion:
 *    Creates a summary reference that pre-computes what is needed for
 *    fast summarization. This function must be balanced with a call at
 *    a later time to CFRelease.
 *  
 *  Parameters:
 *    
 *    inString:
 *      the text string.
 *  
 *  Result:
 *    Returns a summary reference, or <tt>NULL</tt> on failure.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function SKSummaryCreateWithString( inString: CFStringRef ): SKSummaryRef; external name '_SKSummaryCreateWithString';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{
 *  SKSummaryGetSentenceCount()
 *  
 *  Summary:
 *    Gets the number of sentences available.
 *  
 *  Parameters:
 *    
 *    summary:
 *      A reference to the summary object
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function SKSummaryGetSentenceCount( summary: SKSummaryRef ): CFIndex; external name '_SKSummaryGetSentenceCount';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{
 *  SKSummaryGetParagraphCount()
 *  
 *  Summary:
 *    Gets the number of paragraphs available.
 *  
 *  Parameters:
 *    
 *    summary:
 *      A reference to the summary object
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function SKSummaryGetParagraphCount( summary: SKSummaryRef ): CFIndex; external name '_SKSummaryGetParagraphCount';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{
 *  SKSummaryCopySentenceAtIndex()
 *  
 *  Summary:
 *    Gets the ith sentence from the original text.
 *  
 *  Parameters:
 *    
 *    summary:
 *      A reference to the summary object
 *    
 *    i:
 *      zero-based index
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function SKSummaryCopySentenceAtIndex( summary: SKSummaryRef; i: CFIndex ): CFStringRef; external name '_SKSummaryCopySentenceAtIndex';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{
 *  SKSummaryCopyParagraphAtIndex()
 *  
 *  Summary:
 *    Gets the ith paragraph from the original text.
 *  
 *  Parameters:
 *    
 *    summary:
 *      A reference to the summary object
 *    
 *    i:
 *      zero-based index
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function SKSummaryCopyParagraphAtIndex( summary: SKSummaryRef; i: CFIndex ): CFStringRef; external name '_SKSummaryCopyParagraphAtIndex';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{
 *  SKSummaryCopySentenceSummaryString()
 *  
 *  Summary:
 *    Gets a summary string that includes at most the requested number
 *    of sentences.
 *  
 *  Parameters:
 *    
 *    summary:
 *      A reference to the summary object
 *    
 *    numSentences:
 *      the number of sentences desired
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function SKSummaryCopySentenceSummaryString( summary: SKSummaryRef; numSentences: CFIndex ): CFStringRef; external name '_SKSummaryCopySentenceSummaryString';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{
 *  SKSummaryCopyParagraphSummaryString()
 *  
 *  Summary:
 *    Gets a summary string that includes at most the requested number
 *    of paragraphs.
 *  
 *  Parameters:
 *    
 *    summary:
 *      A reference to the summary object
 *    
 *    numParagraphs:
 *      the number of paragraphs desired
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function SKSummaryCopyParagraphSummaryString( summary: SKSummaryRef; numParagraphs: CFIndex ): CFStringRef; external name '_SKSummaryCopyParagraphSummaryString';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{
 *  SKSummaryGetSentenceSummaryInfo()
 *  
 *  Summary:
 *    Get detailed information about a sentence-based summary. Useful
 *    for constructing your own summary string. Arrays must be of size
 *    numSentences or they can be nil. Return value is the number of
 *    sentences actually returned. Sentences are returned in text order
 *    via outSentenceIndexOfSentences.
 *  
 *  Parameters:
 *    
 *    summary:
 *      A reference to the summary object
 *    
 *    numSentencesInSummary:
 *      the number of sentences desired
 *    
 *    outRankOrderOfSentences:
 *      array for returning the rank of each sentence; most important
 *      sentence is rank 1
 *    
 *    outSentenceIndexOfSentences:
 *      array for returning the index of each sentence; use
 *      SKSummaryCopySentenceAtIndex to get sentence
 *    
 *    outParagraphIndexOfSentences:
 *      array for returning the index of the paragraph in which
 *      sentence occured
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function SKSummaryGetSentenceSummaryInfo( summary: SKSummaryRef; numSentencesInSummary: CFIndex; outRankOrderOfSentences: CFIndexPtr { array }; outSentenceIndexOfSentences: CFIndexPtr { array }; outParagraphIndexOfSentences: CFIndexPtr { array } ): CFIndex; external name '_SKSummaryGetSentenceSummaryInfo';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{
 *  SKSummaryGetParagraphSummaryInfo()
 *  
 *  Summary:
 *    Get detailed information about a paragraph-based summary. Useful
 *    for constructing your own summary string. Arrays must be of size
 *    numParagraphs or they can be nil. Return value is the number of
 *    paragraphs actually returned. Paragraphs are returned in text
 *    order via outParagraphIndexOfParagraphs.
 *  
 *  Parameters:
 *    
 *    summary:
 *      A reference to the summary object
 *    
 *    numParagraphsInSummary:
 *      the number of sentences desired
 *    
 *    outRankOrderOfParagraphs:
 *      array for returning the rank of each paragraph; most important
 *      paragraph is rank 1
 *    
 *    outParagraphIndexOfParagraphs:
 *      array for returning the index of each paragraph; use
 *      SKSummaryCopyParagraphAtIndex to get paragraph
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function SKSummaryGetParagraphSummaryInfo( summary: SKSummaryRef; numParagraphsInSummary: CFIndex; outRankOrderOfParagraphs: CFIndexPtr { array }; outParagraphIndexOfParagraphs: CFIndexPtr {array} ): CFIndex; external name '_SKSummaryGetParagraphSummaryInfo';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{$endc} {TARGET_OS_MAC}

{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}

end.
{$endc} {not MACOSALLINCLUDE}
