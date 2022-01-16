{
     File:       SearchKit/SKIndex.h
 
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

unit SKIndex;
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
uses MacTypes,CFBase,CFArray,CFData,CFURL,CFDictionary,SKDocument;
{$endc} {not MACOSALLINCLUDE}

{$ALIGN POWER}


{
 *  CFTypes for use with SearchKit
 }


{$ifc TARGET_OS_MAC}

{
 *  SKIndexRef
 *  
 *  Summary:
 *    An opaque data type representing an index.
 }
type
	SKIndexRef = ^__SKIndex; { an opaque type }
	__SKIndex = record end;
	SKIndexRefPtr = ^SKIndexRef;
{
 *  SKIndexGetTypeID()
 *  
 *  Summary:
 *    Returns the type identifier for the index.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function SKIndexGetTypeID: CFTypeID; external name '_SKIndexGetTypeID';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)


{
 *  SKIndexDocumentIteratorRef
 *  
 *  Summary:
 *    An opaque data type representing an index iterator.
 }
type
	SKIndexDocumentIteratorRef = ^__SKIndexDocumentIterator; { an opaque type }
	__SKIndexDocumentIterator = record end;
	SKIndexDocumentIteratorRefPtr = ^SKIndexDocumentIteratorRef;
{
 *  SKIndexDocumentIteratorGetTypeID()
 *  
 *  Summary:
 *    Returns the type identifier of the index iterator.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function SKIndexDocumentIteratorGetTypeID: CFTypeID; external name '_SKIndexDocumentIteratorGetTypeID';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)


{
 * enumeration types
 }

{
 *  SKIndexType
 *  
 *  Summary:
 *    The possible index types for SearchKit indexes.
 }
type
	SKIndexType = SInt32;
	SKIndexTypePtr = ^SKIndexType;
const
{
   * Unknown index type.
   }
	kSKIndexUnknown = 0;

  {
   * Inverted index, mapping terms to documents.
   }
	kSKIndexInverted = 1;

  {
   * Vector index, mapping documents to terms.
   }
	kSKIndexVector = 2;

  {
   * Index type with all the capabilities of an inverted and a vector
   * index.
   }
	kSKIndexInvertedVector = 3;


{
 *  SKDocumentIndexState
 *  
 *  Summary:
 *    The indexing state of a document.
 }
type
	SKDocumentIndexState = SInt32;
	SKDocumentIndexStatePtr = ^SKDocumentIndexState;
const
{
   * Document is not indexed.
   }
	kSKDocumentStateNotIndexed = 0;

  {
   * Document is indexed.
   }
	kSKDocumentStateIndexed = 1;

  {
   * Document is not in the index but will be added after the index is
   * flushed or closed.
   }
	kSKDocumentStateAddPending = 2;

  {
   * Document is in the index but will be deleted after the index is
   * flushed or closed.
   }
	kSKDocumentStateDeletePending = 3;


{
 *  SKIndexCreateWithURL()
 *  
 *  Summary:
 *    Creates a named index in a file whose location is specified with
 *    a CFURL object.
 *  
 *  Discussion:
 *    When an index is created, the client must also specify the index
 *    type. The name can be <tt>NULL</tt>. A file can contain more than
 *    one index. If the analysis properties dictionary is not
 *    specified, the default dictionary is used. The various analysis
 *    properties are described in the SKAnalysis.h header file. Use the
 *    <tt>SKIndexGetAnalysisProperties</tt> function to get the
 *    analysis properties of an index. A call to this function must be
 *    balanced with a call at a later time to <tt>CFRelease</tt>.
 *  
 *  Parameters:
 *    
 *    inURL:
 *      A reference to a CFURL object containing the location of the
 *      index.
 *    
 *    inIndexName:
 *      A reference to a CFString object containing the name of the
 *      index. May be <tt>NULL</tt>.
 *    
 *    inIndexType:
 *      The index type.
 *    
 *    inAnalysisProperties:
 *      A reference to the analysis properties dictionary. May be
 *      <tt>NULL</tt>.
 *  
 *  Result:
 *    SKIndexRef  A reference to the index.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function SKIndexCreateWithURL( inURL: CFURLRef; inIndexName: CFStringRef { can be NULL }; inIndexType: SKIndexType; inAnalysisProperties: CFDictionaryRef { can be NULL } ): SKIndexRef; external name '_SKIndexCreateWithURL';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)


{
 *  SKIndexOpenWithURL()
 *  
 *  Summary:
 *    Opens an existing, named index stored in a file whose location is
 *    specified with a CFURL object.
 *  
 *  Discussion:
 *    A call to this function must be balanced with a call at a later
 *    time to <tt>CFRelease</tt>.
 *  
 *  Parameters:
 *    
 *    inURL:
 *      A reference to a file CFURL object containing the location of
 *      the index.
 *    
 *    inIndexName:
 *      A reference to a CFString object containing the name of the
 *      index. May be <tt>NULL</tt>.
 *    
 *    inWriteAccess:
 *      A boolean value indicating whether the index is open for
 *      writing.
 *  
 *  Result:
 *    SKIndexRef  A reference to the index.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function SKIndexOpenWithURL( inURL: CFURLRef; inIndexName: CFStringRef { can be NULL }; inWriteAccess: Boolean ): SKIndexRef; external name '_SKIndexOpenWithURL';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)


{
 *  SKIndexCreateWithMutableData()
 *  
 *  Summary:
 *    Creates a named index stored in a CFData object; the name can be
 *    <tt>NULL</tt>.
 *  
 *  Parameters:
 *    
 *    inData:
 *      A reference to a CFMutableData object containing the index to
 *      create.
 *    
 *    inIndexName:
 *      A reference to a CFString object containing the name of the
 *      index. May be <tt>NULL</tt>.
 *    
 *    inIndexType:
 *      A reference to the index type.
 *    
 *    inAnalysisProperties:
 *      A reference to the analysis properties dictionary. May be
 *      <tt>NULL</tt>.
 *  
 *  Result:
 *    SKIndexRef  A reference to the index.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function SKIndexCreateWithMutableData( inData: CFMutableDataRef; inIndexName: CFStringRef { can be NULL }; inIndexType: SKIndexType; inAnalysisProperties: CFDictionaryRef { can be NULL } ): SKIndexRef; external name '_SKIndexCreateWithMutableData';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)


{
 *  SKIndexOpenWithData()
 *  
 *  Summary:
 *    Opens an existing, named index stored in a CFData object.
 *  
 *  Discussion:
 *    The index may be searched but not updated. To open the index for
 *    updating, use the <tt>SKIndexOpenWithMutableData</tt> function. A
 *    call to this function must be balanced with a call at a later
 *    time to <tt>CFRelease</tt>.
 *  
 *  Parameters:
 *    
 *    inData:
 *      A reference to a CFData object containing the index to open.
 *    
 *    inIndexName:
 *      A reference to a CFString object containing the name of the
 *      index. May be <tt>NULL</tt>.
 *  
 *  Result:
 *    SKIndexRef  A reference to the index.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function SKIndexOpenWithData( inData: CFDataRef; inIndexName: CFStringRef { can be NULL } ): SKIndexRef; external name '_SKIndexOpenWithData';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)


{
 *  SKIndexOpenWithMutableData()
 *  
 *  Summary:
 *    Opens an existing, named index stored in a CFData object.
 *  
 *  Discussion:
 *    The index may be searched or updated. To open the index for
 *    search only, use the <tt>SKIndexOpenWithData</tt> function. A
 *    call to this function must be balanced with a call at a later
 *    time to <tt>CFRelease</tt>.
 *  
 *  Parameters:
 *    
 *    inData:
 *      A reference to a CFMutableData object containing the index to
 *      open.
 *    
 *    inIndexName:
 *      A reference to a CFString object containing the name of the
 *      index. May be <tt>NULL</tt>.
 *  
 *  Result:
 *    SKIndexRef  A reference to the index.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function SKIndexOpenWithMutableData( inData: CFMutableDataRef; inIndexName: CFStringRef { can be NULL } ): SKIndexRef; external name '_SKIndexOpenWithMutableData';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)


{
 *  SKIndexFlush()
 *  
 *  Summary:
 *    Forces SearchKit to flush all caches associated with an index.
 *  
 *  Discussion:
 *    Index caches can become stale when clients add or remove
 *    documents. Before searching an index you need to call
 *    <tt>SKIndexFlush</tt>, even though the cache flushing may take
 *    some time.
 *  
 *  Result:
 *    Returns a Boolean value of <tt>true</tt> on success or
 *    <tt>false</tt> on failure.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function SKIndexFlush( inIndex: SKIndexRef ): Boolean; external name '_SKIndexFlush';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)


{
 *  SKIndexSetMaximumBytesBeforeFlush()
 *  
 *  Summary:
 *    Sets the memory size limit for an index.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure SKIndexSetMaximumBytesBeforeFlush( inIndex: SKIndexRef; inBytesForUpdate: CFIndex ); external name '_SKIndexSetMaximumBytesBeforeFlush';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)


{
 *  SKIndexGetMaximumBytesBeforeFlush()
 *  
 *  Summary:
 *    Gets the memory size limit for an index, measured in bytes.
 *  
 *  Result:
 *    Returns a CFIndex object containing the memory size limit for an
 *    index. On failure, returns 0.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function SKIndexGetMaximumBytesBeforeFlush( inIndex: SKIndexRef ): CFIndex; external name '_SKIndexGetMaximumBytesBeforeFlush';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)


{
 *  SKIndexCompact()
 *  
 *  Summary:
 *    Attempts to defragment and compact the index.
 *  
 *  Discussion:
 *    This function takes time. Call it only when the index is too
 *    fragmented. You can test for fragmentation be examining the ratio
 *    of the total document count, obtained with
 *    <tt>SKIndexGetDocumentCount</tt>, to the maximum document ID,
 *    obtained with <tt>SKIndexGetMaximumDocumentID</tt>.
 *  
 *  Result:
 *    Returns a Boolean value of <tt>true</tt> on success or
 *    <tt>false</tt> on failure.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function SKIndexCompact( inIndex: SKIndexRef ): Boolean; external name '_SKIndexCompact';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)


{
 *  SKIndexGetIndexType()
 *  
 *  Summary:
 *    Gets the type of an index.
 *  
 *  Discussion:
 *    See the <tt>SKIndexType</tt> enumeration for a list of the
 *    various index types.
 *  
 *  Result:
 *    Returns the type of the index. On failure, returns a value of
 *    kSKIndexUnknown.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function SKIndexGetIndexType( inIndex: SKIndexRef ): SKIndexType; external name '_SKIndexGetIndexType';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)


{
 *  SKIndexGetAnalysisProperties()
 *  
 *  Summary:
 *    Gets the text analysis properties of an index.
 *  
 *  Result:
 *    Returns a CFDictionary object containing the index's text
 *    analysis properties. On failure, returns <tt>NULL</tt>.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function SKIndexGetAnalysisProperties( inIndex: SKIndexRef ): CFDictionaryRef; external name '_SKIndexGetAnalysisProperties';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)


{
 *  SKIndexGetDocumentCount()
 *  
 *  Summary:
 *    Gets the total number of documents represented in an index.
 *  
 *  Discussion:
 *    Indexed documents have an indexing state of
 *    kSKDocumentStateIndexed. See the <tt>SKDocumentIndexState</tt>
 *    enumeration.
 *  
 *  Result:
 *    Returns CFIndex object containing the number of documents in the
 *    index. On failure, returns 0.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function SKIndexGetDocumentCount( inIndex: SKIndexRef ): CFIndex; external name '_SKIndexGetDocumentCount';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)


{
 *  SKIndexClose()
 *  
 *  Summary:
 *    Close the index.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure SKIndexClose( inIndex: SKIndexRef ); external name '_SKIndexClose';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{
 * Managing documents inside an index
 }
type
	SKDocumentID = CFIndex;
	SKDocumentIDPtr = ^SKDocumentID;
{
 *  SKIndexAddDocumentWithText()
 *  
 *  Summary:
 *    Adds a document and its text to an index.
 *  
 *  Parameters:
 *    
 *    inIndex:
 *      A reference to the index to which you are adding the document.
 *    
 *    inDocument:
 *      A reference to the document to add.
 *    
 *    inDocumentText:
 *      A reference to the document text. May be <tt>NULL</tt>.
 *    
 *    inCanReplace:
 *      A boolean value indicating whether a document with the same
 *      descriptor can be overwritten.
 *  
 *  Result:
 *    Returns a Boolean value of <tt>true</tt> on success or
 *    <tt>false</tt> on failure.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function SKIndexAddDocumentWithText( inIndex: SKIndexRef; inDocument: SKDocumentRef; inDocumentText: CFStringRef { can be NULL }; inCanReplace: Boolean ): Boolean; external name '_SKIndexAddDocumentWithText';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)


{
 *  SKIndexAddDocument()
 *  
 *  Summary:
 *    Adds a document to an index.
 *  
 *  Discussion:
 *    This function uses the input document and the optional MIME type
 *    hint to get the document text using plug-in-based text
 *    extractors. Call <tt>SKLoadDefaultExtractorPlugIns</tt> to load
 *    the default text extractors.
 *  
 *  Parameters:
 *    
 *    inDocument:
 *      A reference to the document to add. The document scheme must be
 *      of type "file" to use this function. If it's not, call
 *      <tt>SKIndexAddDocumentWithText</tt> instead.
 *    
 *    inIndex:
 *      A reference to the index to which you are adding the document.
 *    
 *    inDocument:
 *      A reference to the document to add.
 *    
 *    inMIMETypeHint:
 *      The MIME type hint for the document. May be <tt>NULL</tt>.
 *    
 *    inCanReplace:
 *      A boolean value indicating whether a document with the same
 *      descriptor can be overwritten.
 *  
 *  Result:
 *    Returns a Boolean value of <tt>true</tt> on success or
 *    <tt>false</tt> on failure.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function SKIndexAddDocument( inIndex: SKIndexRef; inDocument: SKDocumentRef; inMIMETypeHint: CFStringRef { can be NULL }; inCanReplace: Boolean ): Boolean; external name '_SKIndexAddDocument';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)


{
 *  SKIndexRemoveDocument()
 *  
 *  Summary:
 *    Removes a document and its children, if any, from an index.
 *  
 *  Parameters:
 *    
 *    inIndex:
 *      A reference to the index from which you want to remove the
 *      document.
 *    
 *    inDocument:
 *      A reference to the document to remove.
 *  
 *  Result:
 *    Returns a Boolean value of <tt>true</tt> on success or
 *    <tt>false</tt> on failure.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function SKIndexRemoveDocument( inIndex: SKIndexRef; inDocument: SKDocumentRef ): Boolean; external name '_SKIndexRemoveDocument';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)


{
 *  SKIndexCopyDocumentProperties()
 *  
 *  Summary:
 *    Copies the user-defined properties of a document in an index to a
 *    dictionary.
 *  
 *  Result:
 *    Returns a CFDictionary object, or <tt>NULL</tt> on failure.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function SKIndexCopyDocumentProperties( inIndex: SKIndexRef; inDocument: SKDocumentRef ): CFDictionaryRef; external name '_SKIndexCopyDocumentProperties';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)


{
 *  SKIndexSetDocumentProperties()
 *  
 *  Summary:
 *    Sets the user-defined properties of a document in an index. A
 *    document's properties are persistently stored in the index. The
 *    existing properties dictionary will be replaced with the new one.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure SKIndexSetDocumentProperties( inIndex: SKIndexRef; inDocument: SKDocumentRef; inProperties: CFDictionaryRef ); external name '_SKIndexSetDocumentProperties';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)


{
 *  SKIndexGetDocumentState()
 *  
 *  Summary:
 *    Gets the current indexing state of a document in an index.
 *  
 *  Discussion:
 *    A document can be in 1 of 4 states, as defined by the
 *    <tt>SKDocumentIndexState</tt> enumeration: not indexed, indexed,
 *    not in the index but will be added after the index is flushed or
 *    closed, and in the index but will be deleted after the index is
 *    flushed or closed.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function SKIndexGetDocumentState( inIndex: SKIndexRef; inDocument: SKDocumentRef ): SKDocumentIndexState; external name '_SKIndexGetDocumentState';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)


{
 *  SKIndexGetDocumentID()
 *  
 *  Summary:
 *    Gets the ID of a document in an index.
 *  
 *  Discussion:
 *    The document ID is a persistent way to identify the document in
 *    an index.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function SKIndexGetDocumentID( inIndex: SKIndexRef; inDocument: SKDocumentRef ): SKDocumentID; external name '_SKIndexGetDocumentID';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)


{
 *  SKIndexCopyDocumentForDocumentID()
 *  
 *  Summary:
 *    Copies a document reference by way of a document ID in an index.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function SKIndexCopyDocumentForDocumentID( inIndex: SKIndexRef; inDocumentID: SKDocumentID ): SKDocumentRef; external name '_SKIndexCopyDocumentForDocumentID';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)


{
 *  SKIndexRenameDocument()
 *  
 *  Summary:
 *    Changes the name of a document in an index.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function SKIndexRenameDocument( inIndex: SKIndexRef; inDocument: SKDocumentRef; inNewName: CFStringRef ): Boolean; external name '_SKIndexRenameDocument';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)


{
 *  SKIndexMoveDocument()
 *  
 *  Summary:
 *    Changes the parent of a document in an index.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function SKIndexMoveDocument( inIndex: SKIndexRef; inDocument: SKDocumentRef; inNewParent: SKDocumentRef ): Boolean; external name '_SKIndexMoveDocument';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)


{
 *  SKIndexDocumentIteratorCreate()
 *  
 *  Summary:
 *    Creates an iterator for an index based on a starting document
 *    reference.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function SKIndexDocumentIteratorCreate( inIndex: SKIndexRef; inParentDocument: SKDocumentRef ): SKIndexDocumentIteratorRef; external name '_SKIndexDocumentIteratorCreate';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)


{
 *  SKIndexDocumentIteratorCopyNext()
 *  
 *  Summary:
 *    Gets the next document reference from a document iterator.
 *  
 *  Discussion:
 *    This function returns <tt>NULL</tt> when there are no more
 *    documents. You must call <tt>CFRelease</tt> on all retrieved
 *    document references that are non-null.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function SKIndexDocumentIteratorCopyNext( inIterator: SKIndexDocumentIteratorRef ): SKDocumentRef; external name '_SKIndexDocumentIteratorCopyNext';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)


{
 * Documents in Index
 }
{
 *  SKIndexGetMaximumDocumentID()
 *  
 *  Summary:
 *    Gets the highest-numbered document ID in an index.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function SKIndexGetMaximumDocumentID( inIndex: SKIndexRef ): SKDocumentID; external name '_SKIndexGetMaximumDocumentID';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)


{
 *  SKIndexGetDocumentTermCount()
 *  
 *  Summary:
 *    Gets the number of terms for a given document in an index.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function SKIndexGetDocumentTermCount( inIndex: SKIndexRef; inDocumentID: SKDocumentID ): CFIndex; external name '_SKIndexGetDocumentTermCount';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)


{
 *  SKIndexCopyTermIDArrayForDocumentID()
 *  
 *  Summary:
 *    Gets the IDs for the terms of a document in an index.
 *  
 *  Result:
 *    A reference to a CFArray object containing CFNumber objects.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function SKIndexCopyTermIDArrayForDocumentID( inIndex: SKIndexRef; inDocumentID: SKDocumentID ): CFArrayRef; external name '_SKIndexCopyTermIDArrayForDocumentID';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)


{
 *  SKIndexGetDocumentTermFrequency()
 *  
 *  Summary:
 *    Gets the frequency of occurrences of a given term in a document.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function SKIndexGetDocumentTermFrequency( inIndex: SKIndexRef; inDocumentID: SKDocumentID; inTermID: CFIndex ): CFIndex; external name '_SKIndexGetDocumentTermFrequency';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)


{
 * Terms in Index
 *
 * A term is a word from the content of a document.
 }
{
 *  SKIndexGetMaximumTermID()
 *  
 *  Summary:
 *    Gets the highest-numbered term ID in an index.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function SKIndexGetMaximumTermID( inIndex: SKIndexRef ): CFIndex; external name '_SKIndexGetMaximumTermID';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)


{
 *  SKIndexGetTermDocumentCount()
 *  
 *  Summary:
 *    Gets the number of documents containing a given term in an index.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function SKIndexGetTermDocumentCount( inIndex: SKIndexRef; inTermID: CFIndex ): CFIndex; external name '_SKIndexGetTermDocumentCount';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)


{
 *  SKIndexCopyDocumentIDArrayForTermID()
 *  
 *  Summary:
 *    Gets the IDs of the documents containing a given term in an index.
 *  
 *  Result:
 *    A reference to a CFArray object containing CFNumber objects.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function SKIndexCopyDocumentIDArrayForTermID( inIndex: SKIndexRef; inTermID: CFIndex ): CFArrayRef; external name '_SKIndexCopyDocumentIDArrayForTermID';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)


{
 *  SKIndexCopyTermStringForTermID()
 *  
 *  Summary:
 *    Gets the term specified by a term ID in an index.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function SKIndexCopyTermStringForTermID( inIndex: SKIndexRef; inTermID: CFIndex ): CFStringRef; external name '_SKIndexCopyTermStringForTermID';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)


{
 *  SKIndexGetTermIDForTermString()
 *  
 *  Summary:
 *    Gets the term ID for a given term in an index.
 *  
 *  Discussion:
 *    If the term isn't found, return a value of kCFNotFound.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function SKIndexGetTermIDForTermString( inIndex: SKIndexRef; inTermString: CFStringRef ): CFIndex; external name '_SKIndexGetTermIDForTermString';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)


{
 *  SKLoadDefaultExtractorPlugIns()
 *  
 *  Summary:
 *    Loads the default text extractor plug-ins.
 *  
 *  Discussion:
 *    A text extractor knows the format of a specific kind of document.
 *    For example, various text extractors can return the text of a PDF
 *    document, strip the tags of an HTML document, and so on. Loading
 *    extractors allows the <tt>SKIndexAddDocument</tt> function to
 *    extract the text from supported documents, leaving the markup
 *    behind.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure SKLoadDefaultExtractorPlugIns; external name '_SKLoadDefaultExtractorPlugIns';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)

{$endc} {TARGET_OS_MAC}
{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}

end.
{$endc} {not MACOSALLINCLUDE}
