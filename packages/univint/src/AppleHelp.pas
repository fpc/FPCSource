{
     File:       AppleHelp.p
 
     Contains:   Apple Help
 
     Version:    Technology: Mac OS X/CarbonLib 1.1
                 Release:    Universal Interfaces 3.4.2
 
     Copyright:  © 2000-2002 by Apple Computer, Inc., all rights reserved.
 
     Bugs?:      For bug reports, consult the following page on
                 the World Wide Web:
 
                     http://www.freepascal.org/bugs.html
 
}


{
    Modified for use with Free Pascal
    Version 200
    Please report any bugs to <gpc@microbizz.nl>
}

{$mode macpas}
{$packenum 1}
{$macro on}
{$inline on}
{$CALLING MWPASCAL}

unit AppleHelp;
interface
{$setc UNIVERSAL_INTERFACES_VERSION := $0342}
{$setc GAP_INTERFACES_VERSION := $0200}

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
uses MacTypes,CFBase,Files,CFString;


{$ALIGN MAC68K}

{ AppleHelp Error Codes }

const
	kAHInternalErr				= -10790;
	kAHInternetConfigPrefErr	= -10791;


type
	AHTOCType 					= SInt16;
const
	kAHTOCTypeUser				= 0;
	kAHTOCTypeDeveloper			= 1;

	{
	 *  AHSearch()
	 *  
	 *  Discussion:
	 *    Delivers a request to perform the specified search to the Help
	 *    Viewer application.
	 *  
	 *  Parameters:
	 *    
	 *    bookname:
	 *      Optionally, the AppleTitle of the Help book to be searched. If
	 *      NULL, all installed Help books are searched.
	 *    
	 *    query:
	 *      The query to be made. This string can, if desired, have boolean
	 *      operators or be a natural language phrase.
	 *  
	 *  Result:
	 *    An operating system result code that indicates whether the
	 *    request was successfully sent to the Help Viewer application.
	 *    Possible values: noErr, paramErr, kAHInternalErr,
	 *    kAHInternetConfigPrefErr.
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   not available
	 *    CarbonLib:        in CarbonLib 1.1 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
function AHSearch(bookname: CFStringRef; query: CFStringRef): OSStatus; external name '_AHSearch';

{
 *  AHGotoMainTOC()
 *  
 *  Discussion:
 *    Delivers a request to load the main table of contents of
 *    installed help books to the Help Viewer application.
 *  
 *  Parameters:
 *    
 *    toctype:
 *      The type of table of contents to be loaded: user or developer.
 *  
 *  Result:
 *    An operating system result code that indicates whether the
 *    request was successfully sent to the Help Viewer application.
 *    Possible values: noErr, paramErr, kAHInternalErr,
 *    kAHInternetConfigPrefErr.
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function AHGotoMainTOC(toctype: AHTOCType): OSStatus; external name '_AHGotoMainTOC';

{
 *  AHGotoPage()
 *  
 *  Discussion:
 *    Delivers a request to load a specific text/html file to the Help
 *    Viewer application.
 *  
 *  Parameters:
 *    
 *    bookname:
 *      Optionally, the AppleTitle of an installed Help book. If NULL,
 *      the path parameter must be a full file: URL to the file to be
 *      opened.
 *    
 *    path:
 *      Optionally, one of two types of paths: 1) a URL-style path to a
 *      file that is relative to the main folder of the book supplied
 *      in the bookname parameter, or 2) if bookname is NULL, a full
 *      file: URL to the file to be opened. If this parameter is NULL,
 *      then bookname must not be NULL, and is used to open the Help
 *      Viewer to the main page of Help content for the specified book.
 *    
 *    anchor:
 *      Optionally, the name of anchor tag to scroll to in the newly
 *      opened file. Can be NULL.
 *  
 *  Result:
 *    An operating system result code that indicates whether the
 *    request was successfully sent to the Help Viewer application.
 *    Possible values: noErr, paramErr, kAHInternalErr,
 *    kAHInternetConfigPrefErr.
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function AHGotoPage(bookname: CFStringRef; path: CFStringRef; anchor: CFStringRef): OSStatus; external name '_AHGotoPage';

{
 *  AHLookupAnchor()
 *  
 *  Discussion:
 *    Delivers a request to perform an anchor lookup to the Help Viewer
 *    application. Note: anchor lookups will fail unless you have
 *    indexed your help content with anchor indexing turned on in the
 *    indexing tool's preferences panel.
 *  
 *  Parameters:
 *    
 *    bookname:
 *      Optionally, the AppleTitle of the Help book to searched. If
 *      NULL, the anchor lookup is performed using all installed Help
 *      books.
 *    
 *    anchor:
 *      The name of the anchor tag to look up.
 *  
 *  Result:
 *    An operating system result code that indicates whether the
 *    request was successfully sent to the Help Viewer application.
 *    Possible values: noErr, paramErr, kAHInternalErr,
 *    kAHInternetConfigPrefErr.
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function AHLookupAnchor(bookname: CFStringRef; anchor: CFStringRef): OSStatus; external name '_AHLookupAnchor';


{
 *  AHRegisterHelpBook()
 *  
 *  Discussion:
 *    Registers a book of Help content such that the book will appear
 *    in the current user's main table of contents (Help Center) in the
 *    Help Viewer application. To be used when help books reside
 *    outside of the known help folders (i.e. help books that are kept
 *    inside of application bundles).
 *  
 *  Parameters:
 *    
 *    appBundleRef:
 *      An FSRef pointer to the bundle within which one or more Help
 *      books is stored. This is likely an FSRef to your application's
 *      main bundle.
 *  
 *  Result:
 *    An operating system result code that indicates whether all help
 *    books contained within the specified bundle were registered.
 *    Possible values: noErr, paramErr, kAHInternalErr, dirNFErr.
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function AHRegisterHelpBook(const (*var*) appBundleRef: FSRef): OSStatus; external name '_AHRegisterHelpBook';

{$ALIGN MAC68K}


end.
