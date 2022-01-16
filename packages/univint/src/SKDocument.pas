{
     File:       SearchKit/SKDocument.h
 
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

unit SKDocument;
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
uses MacTypes,CFBase,CFURL;
{$endc} {not MACOSALLINCLUDE}

{$ALIGN POWER}

{$ifc TARGET_OS_MAC}

{
 *  SKDocumentRef
 *  
 *  Summary:
 *    An opaque data type representing a document.
 *  
 *  Discussion:
 *    A document reference is a generic descriptor to a document. It is
 *    built from a document scheme, a parent document, and a document
 *    name.
 }
type
	SKDocumentRef = CFTypeRef;
	SKDocumentRefPtr = ^SKDocumentRef;
{
 *  SKDocumentGetTypeID()
 *  
 *  Summary:
 *    Returns the type identifier of the SKDocument type.
 *  
 *  Result:
 *    Returns a CFTypeID object, or <tt>NULL</tt> on failure.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function SKDocumentGetTypeID: CFTypeID; external name '_SKDocumentGetTypeID';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)


{
 *  SKDocumentCreateWithURL()
 *  
 *  Summary:
 *    Creates a reference to a document with a URL.
 *  
 *  Discussion:
 *    Use SKDocumentCreateWithURL to create a reference to a file or
 *    other URL. This function must be balanced with a call at a later
 *    time to CFRelease.
 *  
 *  Parameters:
 *    
 *    inURL:
 *      Only "file:" URLs can be used with the SKIndexAddDocument
 *      function, but the URL scheme may be anything you like if you
 *      use the SKIndexAddDocumentWithText function. The scheme of the
 *      document created is set to the scheme of the URL used.
 *  
 *  Result:
 *    Returns a reference to the document, or <tt>NULL</tt> on failure.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function SKDocumentCreateWithURL( inURL: CFURLRef ): SKDocumentRef; external name '_SKDocumentCreateWithURL';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)


{
 *  SKDocumentCopyURL()
 *  
 *  Summary:
 *    Builds a CFURL object from a document reference.
 *  
 *  Result:
 *    Returns a CFURL object, or <tt>NULL</tt> on failure.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function SKDocumentCopyURL( inDocument: SKDocumentRef ): CFURLRef; external name '_SKDocumentCopyURL';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)


{
 *  SKDocumentCreate()
 *  
 *  Summary:
 *    Create a document based on a scheme, parent, and name.
 *  
 *  Discussion:
 *    The parent can be <tt>NULL</tt>, but either a scheme or a parent
 *    must be specified. This function must be balanced with a call at
 *    a later time to CFRelease
 *  
 *  Parameters:
 *    
 *    inScheme:
 *      Analogous to the scheme of a URL. Documents with a "file"
 *      scheme can be read by the <tt>SKIndexAddDocument</tt> function
 *      (see SearchKit.h). The scheme may be anything you like if you
 *      use the SKIndexAddDocumentWithText function. If the scheme is
 *      <tt>NULL</tt>, it will be set to be the same as the parent.
 *    
 *    inParent:
 *      The reference to the document or container one step up in the
 *      document hierarchy.
 *    
 *    inName:
 *      The name of this document. For a "file" scheme, it is the name
 *      of the file or the container, not its path. The path can be
 *      constructed by following parent links.
 *  
 *  Result:
 *    Returns a reference to the document, or <tt>NULL</tt> on failure.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function SKDocumentCreate( inScheme: CFStringRef; inParent: SKDocumentRef { can be NULL }; inName: CFStringRef ): SKDocumentRef; external name '_SKDocumentCreate';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)


{
 *  SKDocumentGetSchemeName()
 *  
 *  Summary:
 *    Gets the scheme name of a document.
 *  
 *  Parameters:
 *    
 *    inDocument:
 *      The document whose scheme name you want to get.
 *  
 *  Result:
 *    Returns a CFString object, or <tt>NULL</tt> on failure.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function SKDocumentGetSchemeName( inDocument: SKDocumentRef ): CFStringRef; external name '_SKDocumentGetSchemeName';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)


{
 *  SKDocumentGetName()
 *  
 *  Summary:
 *    Gets the name of a document.
 *  
 *  Parameters:
 *    
 *    inDocument:
 *      The document whose name you want to get.
 *  
 *  Result:
 *    Returns a CFString object, or <tt>NULL</tt> on failure.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function SKDocumentGetName( inDocument: SKDocumentRef ): CFStringRef; external name '_SKDocumentGetName';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)


{
 *  SKDocumentGetParent()
 *  
 *  Summary:
 *    Gets a reference to the parent document of a document.
 *  
 *  Parameters:
 *    
 *    inDocument:
 *      The document whose parent you want to get.
 *  
 *  Result:
 *    Returns a reference to the parent document, or <tt>NULL</tt> on
 *    failure.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function SKDocumentGetParent( inDocument: SKDocumentRef ): SKDocumentRef; external name '_SKDocumentGetParent';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)

{$endc} {TARGET_OS_MAC}
{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}

end.
{$endc} {not MACOSALLINCLUDE}
