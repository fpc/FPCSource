{
     File:       CFNetwork/CFHTTPStream.h
 
     Contains:   CoreFoundation Network HTTP streams header
 
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

unit CFHTTPStream;
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
uses MacTypes,CFStream,CFBase,CFHTTPMessage;
{$ALIGN MAC68K}

{
 *  kCFStreamErrorDomainHTTP
 *  
 *  Availability:
 *    Mac OS X:         in version 10.1 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
// AVAILABLE_MAC_OS_X_VERSION_10_1_AND_LATER
var kCFStreamErrorDomainHTTP: SInt32; external name '_kCFStreamErrorDomainHTTP'; (* attribute const *)
type
	CFStreamErrorHTTP = SInt32;
const
  kCFStreamErrorHTTPParseFailure = -1;
  kCFStreamErrorHTTPRedirectionLoop = -2;
  kCFStreamErrorHTTPBadURL      = -3;

{ Value is a CFHTTPMessage with 0 bytes data. }
{
 *  kCFStreamPropertyHTTPResponseHeader
 *  
 *  Availability:
 *    Mac OS X:         in version 10.1 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
// AVAILABLE_MAC_OS_X_VERSION_10_1_AND_LATER
var kCFStreamPropertyHTTPResponseHeader: CFStringRef; external name '_kCFStreamPropertyHTTPResponseHeader'; (* attribute const *)
{ Value is the CFURL from the final request; will only differ from the URL in the original request if an autoredirection has occurred. }
{
 *  kCFStreamPropertyHTTPFinalURL
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
// AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER
var kCFStreamPropertyHTTPFinalURL: CFStringRef; external name '_kCFStreamPropertyHTTPFinalURL'; (* attribute const *)
{************************************}
{Set-able properties on HTTP streams }
{************************************}
{ HTTP proxy information is set the same way as SOCKS proxies (see CFSocketStream.h).
   Call CFReadStreamSetProperty() passing an HTTP stream and the property kCFStreamPropertyHTTPProxy.  
   The value should be a CFDictionary that includes at least one Host/Port pair from the keys below.  
   The dictionary returned by SystemConfiguration.framework can also be passed directly as the value }
{
 *  kCFStreamPropertyHTTPProxy
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
// AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER
var kCFStreamPropertyHTTPProxy: CFStringRef; external name '_kCFStreamPropertyHTTPProxy'; (* attribute const *)
{
 *  kCFStreamPropertyHTTPProxyHost
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
// AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER
var kCFStreamPropertyHTTPProxyHost: CFStringRef; external name '_kCFStreamPropertyHTTPProxyHost'; (* attribute const *)
{ matches kSCPropNetProxiesHTTPProxy; value should be a CFString}
{
 *  kCFStreamPropertyHTTPProxyPort
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
// AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER
var kCFStreamPropertyHTTPProxyPort: CFStringRef; external name '_kCFStreamPropertyHTTPProxyPort'; (* attribute const *)
{ matches kSCPropNetProxiesHTTPPort; value should be a CFNumber }
{
 *  kCFStreamPropertyHTTPSProxyHost
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
// AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER
var kCFStreamPropertyHTTPSProxyHost: CFStringRef; external name '_kCFStreamPropertyHTTPSProxyHost'; (* attribute const *)
{ matches kSCPropNetProxiesHTTPSProxy; value should be a CFString }
{
 *  kCFStreamPropertyHTTPSProxyPort
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
// AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER
var kCFStreamPropertyHTTPSProxyPort: CFStringRef; external name '_kCFStreamPropertyHTTPSProxyPort'; (* attribute const *)
{ matches kSCPropNetProxiesHTTPSPort; value should be a CFNumber }
{ Value should be a CFBoolean }
{
 *  kCFStreamPropertyHTTPShouldAutoredirect
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
// AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER
var kCFStreamPropertyHTTPShouldAutoredirect: CFStringRef; external name '_kCFStreamPropertyHTTPShouldAutoredirect'; (* attribute const *)
{ Value should be a CFBoolean.  If this property is set to true, an HTTP stream will look for an appropriate extant persistent connection to use, and if it finds none, will try to create one.  }
{
 *  kCFStreamPropertyHTTPAttemptPersistentConnection
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
// AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER
var kCFStreamPropertyHTTPAttemptPersistentConnection: CFStringRef; external name '_kCFStreamPropertyHTTPAttemptPersistentConnection'; (* attribute const *)
{ Value is a CFNumber; this property can only be retrieved, not set.  The number returned is the number of bytes from the body of the request that have been written to the underlying socket }
{
 *  kCFStreamPropertyHTTPRequestBytesWrittenCount
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
// AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER
var kCFStreamPropertyHTTPRequestBytesWrittenCount: CFStringRef; external name '_kCFStreamPropertyHTTPRequestBytesWrittenCount'; (* attribute const *)
{*******************}
{ Creation routines }
{*******************}
{ Creates a read stream for the response to the given request; when the stream is opened,
  it will begin transmitting the request.  The bytes returned are the pure body bytes; the response header has been parsed off.
  To retrieve the response header, ask for kCFStreamPropertyHTTPResponseHeader, above, any time after the first bytes arrive on 
  the stream (or when stream end is reported, if there are no data bytes).
}
{
 *  CFReadStreamCreateForHTTPRequest()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.1 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
// AVAILABLE_MAC_OS_X_VERSION_10_1_AND_LATER
function CFReadStreamCreateForHTTPRequest( alloc: CFAllocatorRef; request: CFHTTPMessageRef ): CFReadStreamRef; external name '_CFReadStreamCreateForHTTPRequest';


{ Creates a read stream for the response to the given requestHeaders plus requestBody.  Use in preference to
  CFReadStreamCreateForHTTPRequest() when the body of the request is larger than you wish to be resident in memory.  Note that 
  because streams cannot be reset, read streams created this way cannot have autoredirection enabled.  If the Content-Length 
  header is set in requestHeaders, it is assumed that the caller got the length right and that requestBody will report 
  end-of-stream after precisely Content-Length bytes have been read from it.  If the Content-Length header is not set, the 
  chunked transfer-encoding will be added to requestHeaders, and bytes read from requestBody will be transmitted chunked.
  The body of requestHeaders is ignored.
}
{
 *  CFReadStreamCreateForStreamedHTTPRequest()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
// AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER
function CFReadStreamCreateForStreamedHTTPRequest( alloc: CFAllocatorRef; requestHeaders: CFHTTPMessageRef; requestBody: CFReadStreamRef ): CFReadStreamRef; external name '_CFReadStreamCreateForStreamedHTTPRequest';


{ Deprecated - Use the properties kCFStreamPropertyHTTPShouldAutoredirect and kCFStreamPropertyHTTPProxy above instead }
{
 *  CFHTTPReadStreamSetRedirectsAutomatically()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.1 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
// AVAILABLE_MAC_OS_X_VERSION_10_1_AND_LATER
procedure CFHTTPReadStreamSetRedirectsAutomatically( httpStream: CFReadStreamRef; shouldAutoRedirect: Boolean ); external name '_CFHTTPReadStreamSetRedirectsAutomatically';


{
 *  CFHTTPReadStreamSetProxy()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.1 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
// AVAILABLE_MAC_OS_X_VERSION_10_1_AND_LATER
procedure CFHTTPReadStreamSetProxy( httpStream: CFReadStreamRef; proxyHost: CFStringRef; proxyPort: CFIndex ); external name '_CFHTTPReadStreamSetProxy';

end.
