{
     File:       SearchKit/SKSearch.h
 
     Contains:   SearchKit Interfaces.
 
     Version:    SearchKit-407~38
 
     Copyright:  © 2003-2008 by Apple Computer, Inc., all rights reserved
 
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
{$modeswitch cblocks}
{$packenum 1}
{$macro on}
{$inline on}
{$calling mwpascal}

unit SKSearch;
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
{$ifc defined iphonesim}
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
{$ifc defined iphonesim}
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
{$ifc defined ios}
	{$setc TARGET_OS_MAC := FALSE}
	{$setc TARGET_OS_IPHONE := TRUE}
	{$setc TARGET_OS_EMBEDDED := TRUE}
{$elsec}
	{$setc TARGET_OS_MAC := TRUE}
	{$setc TARGET_OS_IPHONE := FALSE}
	{$setc TARGET_OS_EMBEDDED := FALSE}
{$endc}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
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
uses MacTypes,CFBase,CFArray,CFURL,CFDictionary,CFDate,SKIndex,SKDocument;
{$endc} {not MACOSALLINCLUDE}

{$ALIGN POWER}


{$ifc TARGET_OS_MAC}
{
 * Asynchronous search
 }


{
 *  SKSearchRef
 *  
 *  Summary:
 *    An opaque data type representing an asynchronous search.
 }
type
	SKSearchRef = ^__SKSearch; { an opaque type }
	__SKSearch = record end;
{
 *  SKSearchGetTypeID()
 *  
 *  Summary:
 *    Returns the type identifier for the SKSearch type.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function SKSearchGetTypeID: CFTypeID; external name '_SKSearchGetTypeID';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{
 *  SKSearchOptions
 *  
 *  Summary:
 *    The various search options you can use with
 *    <tt>SKSearchCreate</tt>.
 }
type
	SKSearchOptions = UInt32;
const
	kSKSearchOptionDefault = 0;
	kSKSearchOptionNoRelevanceScores = 1 shl 0; { Save time by not computing relevance scores. }
	kSKSearchOptionSpaceMeansOR = 1 shl 1; { Space in a query means OR instead of AND. }
	kSKSearchOptionFindSimilar = 1 shl 2; { Find documents similar to given text string }

{
 *  SKSearchCreate()
 *  
 *  Summary:
 *    Create an asynchronous search request.
 *  
 *  Discussion:
 *    A call to this function must be balanced with a call at a later
 *    time to <tt>CFRelease</tt>.
 *  
 *  Parameters:
 *    
 *    inIndex:
 *      A reference to the index to be searched.
 *    
 *    inQuery:
 *      The query string to search for.
 *    
 *    inSearchOptions:
 *      The search options. See the <tt>SKSearchOptions</tt>
 *      enumeration for options.
 *  
 *  Result:
 *    SKSearchRef         A reference to a SKSearch opaque type.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function SKSearchCreate( inIndex: SKIndexRef; inQuery: CFStringRef; inSearchOptions: SKSearchOptions ): SKSearchRef; external name '_SKSearchCreate';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{
 *  SKSearchCancel()
 *  
 *  Summary:
 *    Cancel the search request.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure SKSearchCancel( inSearch: SKSearchRef ); external name '_SKSearchCancel';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{
 *  SKSearchFindMatches()
 *  
 *  Summary:
 *    Search for up to maximumTime seconds or until inMaximumCount (or
 *    all) items are found.
 *  
 *  Discussion:
 *    Returns TRUE if more to search, FALSE when the search is
 *    exhausted. Returns number of entries actually found in
 *    *outFoundCount. The maximumTime of 0 means return quickly, so may
 *    return TRUE, and 0 outFoundCount. The relevance score is not
 *    normalized, so it can be very large.
 *  
 *  Parameters:
 *    
 *    inSearch:
 *      A reference to the SKSearch opaque type.
 *    
 *    inMaximumCount:
 *      The maximum number of found items to return.
 *    
 *    outDocumentIDsArray:
 *      An array of found document IDs. Must be inMaximumCount in size.
 *    
 *    outScoresArray:
 *      An array of relevance scores for found items. May be
 *      <tt>NULL</tt>.
 *    
 *    maximumTime:
 *      The maximum seconds before return.
 *    
 *    outFoundCount:
 *      The number of items actually found.
 *  
 *  Result:
 *    Boolean             Returns TRUE if more to search, FALSE when
 *    the search is exhausted.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function SKSearchFindMatches( inSearch: SKSearchRef; inMaximumCount: CFIndex; outDocumentIDsArray: SKDocumentIDPtr { array }; outScoresArray: Float32Ptr { array }; maximumTime: CFTimeInterval; var outFoundCount: CFIndex ): Boolean; external name '_SKSearchFindMatches';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{
 *  SKIndexCopyInfoForDocumentIDs()
 *  
 *  Summary:
 *    Copies document names and parent ids by way of document IDs in an
 *    index.
 *  
 *  Parameters:
 *    
 *    inIndex:
 *      A reference to the index.
 *    
 *    inCount:
 *      The number of inDocumentIDsArray.
 *    
 *    inDocumentIDsArray:
 *      An array of document IDs.
 *    
 *    outNamesArray:
 *      An array of names for the specified document IDs. May be
 *      <tt>NULL</tt>.
 *    
 *    outParentIDsArray:
 *      An array of parent ids for the specified document IDs. May be
 *      <tt>NULL</tt>.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure SKIndexCopyInfoForDocumentIDs( inIndex: SKIndexRef; inCount: CFIndex; inDocumentIDsArray: SKDocumentIDPtr { array }; outNamesArray: CFStringRefPtr { array }; outParentIDsArray: SKDocumentIDPtr { array } ); external name '_SKIndexCopyInfoForDocumentIDs';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{
 *  SKIndexCopyDocumentRefsForDocumentIDs()
 *  
 *  Summary:
 *    Copies document references by way of document IDs in an index.
 *  
 *  Parameters:
 *    
 *    inIndex:
 *      A reference to the index.
 *    
 *    inCount:
 *      The number of inDocumentIDsArray.
 *    
 *    inDocumentIDsArray:
 *      An array of document IDs.
 *    
 *    outDocumentRefsArray:
 *      An array of document references for the specified document IDs.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure SKIndexCopyDocumentRefsForDocumentIDs( inIndex: SKIndexRef; inCount: CFIndex; inDocumentIDsArray: SKDocumentIDPtr { array }; outDocumentRefsArray: SKDocumentRefPtr { array } ); external name '_SKIndexCopyDocumentRefsForDocumentIDs';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{
 *  SKIndexCopyDocumentURLsForDocumentIDs()
 *  
 *  Summary:
 *    Copies document URLs by way of document IDs in an index.
 *  
 *  Parameters:
 *    
 *    inIndex:
 *      A reference to the index.
 *    
 *    inCount:
 *      The number of inDocumentIDsArray.
 *    
 *    inDocumentIDsArray:
 *      An array of document IDs.
 *    
 *    outDocumentURLsArray:
 *      An array of CFURLs for the specified document IDs.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure SKIndexCopyDocumentURLsForDocumentIDs( inIndex: SKIndexRef; inCount: CFIndex; inDocumentIDsArray: SKDocumentIDPtr { array }; outDocumentURLsArray: CFURLRefPtr { array } ); external name '_SKIndexCopyDocumentURLsForDocumentIDs';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{
 * Synchronous search
 }

{
 *  SKSearchGroupRef
 *  
 *  Summary:
 *    An opaque data type representing a search group.
 *  
 *  Discussion:
 *    A search group is a group of indexes to be searched.
 }
type
	SKSearchGroupRef = ^__SKSearchGroup; { an opaque type }
	__SKSearchGroup = record end;
{
 *  SKSearchGroupGetTypeID()   *** DEPRECATED ***
 *  
 *  Summary:
 *    Returns the type identifier for the SKSearchGroup type.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in CoreServices.framework but deprecated in 10.4
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function SKSearchGroupGetTypeID: CFTypeID; external name '_SKSearchGroupGetTypeID';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4 *)


{
 *  SKSearchResultsRef
 *  
 *  Summary:
 *    An opaque data type representing search results.
 }
type
	SKSearchResultsRef = ^__SKSearchResults; { an opaque type }
	__SKSearchResults = record end;
{
 *  SKSearchResultsGetTypeID()   *** DEPRECATED ***
 *  
 *  Summary:
 *    Returns the type identifier for the SKSearchResults object.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in CoreServices.framework but deprecated in 10.4
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function SKSearchResultsGetTypeID: CFTypeID; external name '_SKSearchResultsGetTypeID';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4 *)


{
 *  SKSearchType
 *  
 *  Discussion:
 *    The various search types you can use with
 *    <tt>SKSearchResultsCreateWithQuery</tt>. Each of these specifies
 *    a set of ranked search hits. The kSKSearchRanked and
 *    kSKSearchPrefixRanked constants can be used for all index types.
 *    The kSKSearchBooleanRanked and kSKSearchRequiredRanked constants
 *    cannot be used for Vector indexes.
 }
type
	SKSearchType = SInt32;
	SKSearchTypePtr = ^SKSearchType;
const
{
   * Basic ranked search.
   }
	kSKSearchRanked = 0;

  {
   * The query can include boolean operators including '|', '&', '!',
   * '(', and ')'.
   }
	kSKSearchBooleanRanked = 1;

  {
   * The query can specify required ('+') or excluded ('-') terms.
   }
	kSKSearchRequiredRanked = 2;

  {
   * Prefix-based search.
   }
	kSKSearchPrefixRanked = 3;


{
 *  SKSearchResultsFilterCallBack
 *  
 *  Summary:
 *    A callback function for hit testing during searching.
 *  
 *  Discussion:
 *    Return <tt>true</tt> to keep this document in the results,
 *    <tt>false</tt> to filter it out.
 }
type
	SKSearchResultsFilterCallBack = function( inIndex: SKIndexRef; inDocument: SKDocumentRef; inContext: UnivPtr ): Boolean;
{
 *  SKSearchGroupCreate()   *** DEPRECATED ***
 *  
 *  Summary:
 *    Creates a search group as an array of references to indexes.
 *  
 *  Discussion:
 *    A search group is used to search one or more indexes.
 *  
 *  Parameters:
 *    
 *    inArrayOfInIndexes:
 *      A CFArray object containing SKIndex objects.
 *  
 *  Result:
 *    SKSearchGroupRef    A reference to an SKSearchGroup opaque type.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in CoreServices.framework but deprecated in 10.4
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function SKSearchGroupCreate( inArrayOfInIndexes: CFArrayRef ): SKSearchGroupRef; external name '_SKSearchGroupCreate';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4 *)


{
 *  SKSearchGroupCopyIndexes()   *** DEPRECATED ***
 *  
 *  Summary:
 *    Gets the indexes for a search group.
 *  
 *  Result:
 *    A CFArray object containing SKIndex objects.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in CoreServices.framework but deprecated in 10.4
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function SKSearchGroupCopyIndexes( inSearchGroup: SKSearchGroupRef ): CFArrayRef; external name '_SKSearchGroupCopyIndexes';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4 *)


{
 *  SKSearchResultsCreateWithQuery()   *** DEPRECATED ***
 *  
 *  Summary:
 *    Queries the indexes in a search group. This call has been
 *    deprecated in favor of <tt>SKSearchCreate</tt>.
 *  
 *  Discussion:
 *    A call to this function must be balanced with a call at a later
 *    time to <tt>CFRelease</tt>.
 *  
 *  Parameters:
 *    
 *    inSearchGroup:
 *      A reference to the search group.
 *    
 *    inQuery:
 *      The query string to search for.
 *    
 *    inSearchType:
 *      The type of search to perform. See the <tt>SKSearchType</tt>
 *      enumeration for options.
 *    
 *    inMaxFoundDocuments:
 *      The maximum number of found items to return. Your client must
 *      specify a positive value.
 *    
 *    inContext:
 *      A client-specified context. May be <tt>NULL</tt>.
 *    
 *    inFilterCallBack:
 *      A callback function for hit testing during searching. May be
 *      <tt>NULL</tt>.
 *  
 *  Result:
 *    SKSearchResultsRef  A reference to an SKSearchResults opaque type.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in CoreServices.framework but deprecated in 10.4
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function SKSearchResultsCreateWithQuery( inSearchGroup: SKSearchGroupRef; inQuery: CFStringRef; inSearchType: SKSearchType; inMaxFoundDocuments: CFIndex; inContext: UnivPtr; inFilterCallBack: SKSearchResultsFilterCallBack ): SKSearchResultsRef; external name '_SKSearchResultsCreateWithQuery';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4 *)


{
 *  SKSearchResultsCreateWithDocuments()   *** DEPRECATED ***
 *  
 *  Summary:
 *    Finds documents similar to given example documents by searching
 *    the indexes in a search group. This call has been deprecated in
 *    favor of <tt>SKSearchCreate</tt>.
 *  
 *  Discussion:
 *    A call to SKSearchResultsCreateWithDocuments must be balanced
 *    with a call at a later time to <tt>CFRelease</tt>.
 *  
 *  Parameters:
 *    
 *    inSearchGroup:
 *      A reference to the search group.
 *    
 *    inExampleDocuments:
 *      An array of example documents. The documents must previously
 *      have been indexed.
 *    
 *    inMaxFoundDocuments:
 *      The maximum number of found items to return. Your client must
 *      specify a positive value.
 *    
 *    inContext:
 *      A client-specified context. May be <tt>NULL</tt>.
 *    
 *    inFilterCallBack:
 *      A callback function for hit testing during searching. May be
 *      <tt>NULL</tt>.
 *  
 *  Result:
 *    SKSearchResultsRef  A reference to an SKSearchResults opaque type.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in CoreServices.framework but deprecated in 10.4
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function SKSearchResultsCreateWithDocuments( inSearchGroup: SKSearchGroupRef; inExampleDocuments: CFArrayRef; inMaxFoundDocuments: CFIndex; inContext: UnivPtr; inFilterCallBack: SKSearchResultsFilterCallBack ): SKSearchResultsRef; external name '_SKSearchResultsCreateWithDocuments';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4 *)


{
 *  SKSearchResultsGetCount()   *** DEPRECATED ***
 *  
 *  Summary:
 *    Gets the total number of found items in a search.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in CoreServices.framework but deprecated in 10.4
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function SKSearchResultsGetCount( inSearchResults: SKSearchResultsRef ): CFIndex; external name '_SKSearchResultsGetCount';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4 *)


{
 *  SKSearchResultsGetInfoInRange()   *** DEPRECATED ***
 *  
 *  Summary:
 *    Fills in requested results, returns number of items that were
 *    returned.
 *  
 *  Discussion:
 *    Search results are returned in descending order of relevance
 *    score.
 *  
 *  Parameters:
 *    
 *    inSearchResults:
 *      A reference to the search results.
 *    
 *    inRange:
 *      A CFRange value pair, specified as (location, length). The
 *      location value specifies the starting item by ranking. The
 *      length value specifies the total number of items. Examples:
 *      (0,1) means the first item, which is also the highest ranking
 *      item. (1,1) means the second item, which is also the
 *      second-highest ranking item. (0,5) means to get the first 5
 *      items.
 *    
 *    outDocumentsArray:
 *      An array of found documents.
 *    
 *    outIndexesArray:
 *      An array of indexes in which the found docouments reside. May
 *      be <tt>NULL</tt> provided that the client does not care.
 *    
 *    outScoresArray:
 *      An array of correspondence scores for found items. May be
 *      <tt>NULL</tt>.
 *  
 *  Result:
 *    The number of items returned -- usually the same number as
 *    specified.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in CoreServices.framework but deprecated in 10.4
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function SKSearchResultsGetInfoInRange( inSearchResults: SKSearchResultsRef; inRange: CFRange; outDocumentsArray: SKDocumentRefPtr { array }; outIndexesArray: SKIndexRefPtr { array }; outScoresArray: Float32Ptr { array } ): CFIndex; external name '_SKSearchResultsGetInfoInRange';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4 *)


{
 *  SKSearchResultsCopyMatchingTerms()   *** DEPRECATED ***
 *  
 *  Summary:
 *    Gets the matching terms for the specified search result item
 *    index.
 *  
 *  Parameters:
 *    
 *    inSearchResults:
 *      A reference to the search results.
 *    
 *    inItem:
 *      The search result item index, starting from 1.
 *  
 *  Result:
 *    A reference to a CFArray object of term IDs.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in CoreServices.framework but deprecated in 10.4
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function SKSearchResultsCopyMatchingTerms( inSearchResults: SKSearchResultsRef; inItem: CFIndex ): CFArrayRef; external name '_SKSearchResultsCopyMatchingTerms';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4 *)



{$endc} {TARGET_OS_MAC}
{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}

end.
{$endc} {not MACOSALLINCLUDE}
