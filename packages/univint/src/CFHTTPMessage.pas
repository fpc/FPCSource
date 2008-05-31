{
     File:       CFNetwork/CFHTTPMessage.h
 
     Contains:   CoreFoundation Network socket streams header
 
     Version:    CFNetwork-71.2~1
 
     Copyright:  © 2001-2003 by Apple Computer, Inc., all rights reserved
 
     Bugs?:      For bug reports, consult the following page on
                 the World Wide Web:
 
                     http://www.freepascal.org/bugs.html
 
}
{	  Pascal Translation:  Peter N Lewis, <peter@stairways.com.au>, 2004 }


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

unit CFHTTPMessage;
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
uses MacTypes,CFString,CFURL,CFBase,CFData,CFDictionary;
{$ALIGN MAC68K}

{
 *  kCFHTTPVersion1_0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.1 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
// AVAILABLE_MAC_OS_X_VERSION_10_1_AND_LATER
var kCFHTTPVersion1_0: CFStringRef; external name '_kCFHTTPVersion1_0'; (* attribute const *)
{
 *  kCFHTTPVersion1_1
 *  
 *  Availability:
 *    Mac OS X:         in version 10.1 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
// AVAILABLE_MAC_OS_X_VERSION_10_1_AND_LATER
var kCFHTTPVersion1_1: CFStringRef; external name '_kCFHTTPVersion1_1'; (* attribute const *)
{
 *  kCFHTTPAuthenticationSchemeBasic
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
// AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER
var kCFHTTPAuthenticationSchemeBasic: CFStringRef; external name '_kCFHTTPAuthenticationSchemeBasic'; (* attribute const *)
{
 *  kCFHTTPAuthenticationSchemeDigest
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
// AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER
var kCFHTTPAuthenticationSchemeDigest: CFStringRef; external name '_kCFHTTPAuthenticationSchemeDigest'; (* attribute const *)
{ Currently unsupported }
type
	CFHTTPMessageRef    = ^SInt32; { an opaque 32-bit type }
{
 *  CFHTTPMessageGetTypeID()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.1 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
// AVAILABLE_MAC_OS_X_VERSION_10_1_AND_LATER
function CFHTTPMessageGetTypeID: CFTypeID; external name '_CFHTTPMessageGetTypeID';


{
 *  CFHTTPMessageCreateRequest()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.1 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
// AVAILABLE_MAC_OS_X_VERSION_10_1_AND_LATER
function CFHTTPMessageCreateRequest( allocator: CFAllocatorRef; requestMethod: CFStringRef; url: CFURLRef; httpVersion: CFStringRef ): CFHTTPMessageRef; external name '_CFHTTPMessageCreateRequest';


{ Pass NULL to use the standard description for the given status code, as found in RFC 2616}
{
 *  CFHTTPMessageCreateResponse()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.1 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
// AVAILABLE_MAC_OS_X_VERSION_10_1_AND_LATER
function CFHTTPMessageCreateResponse( allocator: CFAllocatorRef; statusCode: SInt32; statusDescription: CFStringRef; httpVersion: CFStringRef ): CFHTTPMessageRef; external name '_CFHTTPMessageCreateResponse';


{ Creates an empty request or response, which you can then append bytes to via CFHTTPMessageAppendBytes().  The HTTP header information will be parsed out as the bytes are appended.}
{
 *  CFHTTPMessageCreateEmpty()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.1 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
// AVAILABLE_MAC_OS_X_VERSION_10_1_AND_LATER
function CFHTTPMessageCreateEmpty( allocator: CFAllocatorRef; isRequest: Boolean ): CFHTTPMessageRef; external name '_CFHTTPMessageCreateEmpty';


{
 *  CFHTTPMessageCreateCopy()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.1 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
// AVAILABLE_MAC_OS_X_VERSION_10_1_AND_LATER
function CFHTTPMessageCreateCopy( allocator: CFAllocatorRef; message: CFHTTPMessageRef ): CFHTTPMessageRef; external name '_CFHTTPMessageCreateCopy';


{ Whether the message is a response or a request}
{
 *  CFHTTPMessageIsRequest()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.1 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
// AVAILABLE_MAC_OS_X_VERSION_10_1_AND_LATER
function CFHTTPMessageIsRequest( message: CFHTTPMessageRef ): Boolean; external name '_CFHTTPMessageIsRequest';


{
 *  CFHTTPMessageCopyVersion()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.1 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
// AVAILABLE_MAC_OS_X_VERSION_10_1_AND_LATER
function CFHTTPMessageCopyVersion( message: CFHTTPMessageRef ): CFStringRef; external name '_CFHTTPMessageCopyVersion';


{
 *  CFHTTPMessageCopyBody()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.1 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
// AVAILABLE_MAC_OS_X_VERSION_10_1_AND_LATER
function CFHTTPMessageCopyBody( message: CFHTTPMessageRef ): CFDataRef; external name '_CFHTTPMessageCopyBody';


{
 *  CFHTTPMessageSetBody()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.1 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
// AVAILABLE_MAC_OS_X_VERSION_10_1_AND_LATER
procedure CFHTTPMessageSetBody( message: CFHTTPMessageRef; bodyData: CFDataRef ); external name '_CFHTTPMessageSetBody';


{
 *  CFHTTPMessageCopyHeaderFieldValue()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.1 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
// AVAILABLE_MAC_OS_X_VERSION_10_1_AND_LATER
function CFHTTPMessageCopyHeaderFieldValue( message: CFHTTPMessageRef; headerField: CFStringRef ): CFStringRef; external name '_CFHTTPMessageCopyHeaderFieldValue';


{
 *  CFHTTPMessageCopyAllHeaderFields()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.1 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
// AVAILABLE_MAC_OS_X_VERSION_10_1_AND_LATER
function CFHTTPMessageCopyAllHeaderFields( message: CFHTTPMessageRef ): CFDictionaryRef; external name '_CFHTTPMessageCopyAllHeaderFields';


{
 *  CFHTTPMessageSetHeaderFieldValue()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.1 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
// AVAILABLE_MAC_OS_X_VERSION_10_1_AND_LATER
procedure CFHTTPMessageSetHeaderFieldValue( message: CFHTTPMessageRef; headerField: CFStringRef; value: CFStringRef ); external name '_CFHTTPMessageSetHeaderFieldValue';


{ The following function appends the given bytes to the message given (parsing out any control information if appropriate).  Returns FALSE if a parsing error occurs while processing the new data.}
{
 *  CFHTTPMessageAppendBytes()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.1 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
// AVAILABLE_MAC_OS_X_VERSION_10_1_AND_LATER
function CFHTTPMessageAppendBytes( message: CFHTTPMessageRef; newBytes: UnivPtr; numBytes: CFIndex ): Boolean; external name '_CFHTTPMessageAppendBytes';


{ Whether further header data is expected by the message}
{
 *  CFHTTPMessageIsHeaderComplete()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.1 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
// AVAILABLE_MAC_OS_X_VERSION_10_1_AND_LATER
function CFHTTPMessageIsHeaderComplete( message: CFHTTPMessageRef ): Boolean; external name '_CFHTTPMessageIsHeaderComplete';


{
 *  CFHTTPMessageCopySerializedMessage()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.1 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
// AVAILABLE_MAC_OS_X_VERSION_10_1_AND_LATER
function CFHTTPMessageCopySerializedMessage( request: CFHTTPMessageRef ): CFDataRef; external name '_CFHTTPMessageCopySerializedMessage';


{*******************}
{ Request functions }
{*******************}
{
 *  CFHTTPMessageCopyRequestURL()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.1 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
// AVAILABLE_MAC_OS_X_VERSION_10_1_AND_LATER
function CFHTTPMessageCopyRequestURL( request: CFHTTPMessageRef ): CFURLRef; external name '_CFHTTPMessageCopyRequestURL';


{
 *  CFHTTPMessageCopyRequestMethod()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.1 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
// AVAILABLE_MAC_OS_X_VERSION_10_1_AND_LATER
function CFHTTPMessageCopyRequestMethod( request: CFHTTPMessageRef ): CFStringRef; external name '_CFHTTPMessageCopyRequestMethod';


{ Tries to modify request to contain the authentication information 
   requested by authenticationFailureResponse (which presumably is a 
   401 or 407 response).  Returns TRUE if successful; FALSE otherwise 
   (leaving request unmodified).  If authenticationScheme is NULL, the 
   strongest supported scheme listed in failedResponse will be used. }
{
 *  CFHTTPMessageAddAuthentication()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.1 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
// AVAILABLE_MAC_OS_X_VERSION_10_1_AND_LATER
function CFHTTPMessageAddAuthentication( request: CFHTTPMessageRef; authenticationFailureResponse: CFHTTPMessageRef; username: CFStringRef; password: CFStringRef; authenticationScheme: CFStringRef; forProxy: Boolean ): Boolean; external name '_CFHTTPMessageAddAuthentication';


{********************}
{ Response functions }
{********************}
{
 *  CFHTTPMessageGetResponseStatusCode()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.1 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
// AVAILABLE_MAC_OS_X_VERSION_10_1_AND_LATER
function CFHTTPMessageGetResponseStatusCode( response: CFHTTPMessageRef ): UInt32; external name '_CFHTTPMessageGetResponseStatusCode';


{
 *  CFHTTPMessageCopyResponseStatusLine()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.1 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
// AVAILABLE_MAC_OS_X_VERSION_10_1_AND_LATER
function CFHTTPMessageCopyResponseStatusLine( response: CFHTTPMessageRef ): CFStringRef; external name '_CFHTTPMessageCopyResponseStatusLine';

end.
