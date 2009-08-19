{
     File:       CFNetwork/CFHTTPStream.h
 
     Contains:   CoreFoundation Network HTTP streams header
 
     Version:    CFNetwork-219~1
 
     Copyright:  © 2001-2006 by Apple Computer, Inc., all rights reserved
 
     Bugs?:      For bug reports, consult the following page on
                 the World Wide Web:
 
                     http://www.freepascal.org/bugs.html
 
}
{	  Pascal Translation:  Peter N Lewis, <peter@stairways.com.au>, 2004 }
{     Pascal Translation Updated:  Gale R Paeper, <gpaeper@empirenet.com>, 2008 }


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
 *  Discussion:
 *    Result code returned by HTTP server
 *  
 *  Availability:
 *    Mac OS X:         in version 10.1 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kCFStreamErrorDomainHTTP: SInt32; external name '_kCFStreamErrorDomainHTTP'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_1_AND_LATER *)

{
 *  CFStreamErrorHTTP
 *  
 *  Discussion:
 *    Errors from the kCFStreamErrorDomainHTTP domain.
 }
type
	CFStreamErrorHTTP = SInt32;
const
{
   * Could not parse the request/response.
   }
  kCFStreamErrorHTTPParseFailure = -1;

  {
   * A loop was detected during redirection.
   }
  kCFStreamErrorHTTPRedirectionLoop = -2;

  {
   * Could not retreive url for request/response.
   }
  kCFStreamErrorHTTPBadURL      = -3;

{
 *  kCFStreamPropertyHTTPResponseHeader
 *  
 *  Discussion:
 *    Stream property key, for copy operations. Value is a
 *    CFHTTPMessage with 0 bytes data.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.1 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kCFStreamPropertyHTTPResponseHeader: CFStringRef; external name '_kCFStreamPropertyHTTPResponseHeader'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_1_AND_LATER *)


{
 *  kCFStreamPropertyHTTPFinalURL
 *  
 *  Discussion:
 *    Stream property key, for copy operations. The response header
 *    value is the CFURL from the final request; will only differ from
 *    the URL in the original request if an autoredirection has
 *    occurred.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kCFStreamPropertyHTTPFinalURL: CFStringRef; external name '_kCFStreamPropertyHTTPFinalURL'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)

{
 *  kCFStreamPropertyHTTPFinalRequest
 *  
 *  Discussion:
 *    Stream property key, for copy operations. The value is the
 *    CFHTTPMessage transmitted by the stream, after all modifications
 *    (such as for authentication, connection policy, or redirection)
 *    have been made.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kCFStreamPropertyHTTPFinalRequest: CFStringRef; external name '_kCFStreamPropertyHTTPFinalRequest'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)

{
 *  kCFStreamPropertyHTTPProxy
 *  
 *  Discussion:
 *    Stream property key, for both set and copy operations. The value
 *    is a CFDictionary. HTTP proxy information is set the same way as
 *    SOCKS proxies (see CFSocketStream.h). Call
 *    CFReadStreamSetProperty() passing an HTTP stream and the property
 *    kCFStreamPropertyHTTPProxy. The value should include at least one
 *    Host/Port pair from the keys below. Use the dictionary returned
 *    by SystemConfiguration.framework to set the default values for
 *    the system. HTTP proxies are not applied automatically.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kCFStreamPropertyHTTPProxy: CFStringRef; external name '_kCFStreamPropertyHTTPProxy'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{
 *  kCFStreamPropertyHTTPProxyHost
 *  
 *  Discussion:
 *    Proxy dictionary key. The hostname of an HTTP proxy. The value is
 *    a CFString. The key name matches kSCPropNetProxiesHTTPProxy.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kCFStreamPropertyHTTPProxyHost: CFStringRef; external name '_kCFStreamPropertyHTTPProxyHost'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{
 *  kCFStreamPropertyHTTPProxyPort
 *  
 *  Discussion:
 *    Proxy dictionary key. Value is a CFNumber.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kCFStreamPropertyHTTPProxyPort: CFStringRef; external name '_kCFStreamPropertyHTTPProxyPort'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)
{ matches kSCPropNetProxiesHTTPPort }


{
 *  kCFStreamPropertyHTTPSProxyHost
 *  
 *  Discussion:
 *    Proxy dictionary key. Value is a CFString.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kCFStreamPropertyHTTPSProxyHost: CFStringRef; external name '_kCFStreamPropertyHTTPSProxyHost'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)
{ matches kSCPropNetProxiesHTTPSProxy }


{
 *  kCFStreamPropertyHTTPSProxyPort
 *  
 *  Discussion:
 *    Proxy dictionary key. Value is a CFNumber.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kCFStreamPropertyHTTPSProxyPort: CFStringRef; external name '_kCFStreamPropertyHTTPSProxyPort'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)
{ matches kSCPropNetProxiesHTTPSPort }


{
 *  kCFStreamPropertyHTTPShouldAutoredirect
 *  
 *  Discussion:
 *    Proxy dictionary key. Value is a CFBoolean. Redirection is not
 *    performed by default.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kCFStreamPropertyHTTPShouldAutoredirect: CFStringRef; external name '_kCFStreamPropertyHTTPShouldAutoredirect'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{
 *  kCFStreamPropertyHTTPAttemptPersistentConnection
 *  
 *  Discussion:
 *    Proxy dictionary key. Value is a CFBoolean.  If this property is
 *    set to kCFBooleanTrue, an HTTP stream will look for an
 *    appropriate extant persistent connection to use, and if it finds
 *    none, will try to create one. Persistent connections are not used
 *    by default.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kCFStreamPropertyHTTPAttemptPersistentConnection: CFStringRef; external name '_kCFStreamPropertyHTTPAttemptPersistentConnection'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{
 *  kCFStreamPropertyHTTPRequestBytesWrittenCount
 *  
 *  Discussion:
 *    Proxy dictionary key. Value is a CFNumber. This property can only
 *    be retrieved, not set. The number returned is the number of bytes
 *    from the body of the request that have been written to the
 *    underlying socket
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kCFStreamPropertyHTTPRequestBytesWrittenCount: CFStringRef; external name '_kCFStreamPropertyHTTPRequestBytesWrittenCount'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)


{*******************}
{ Creation routines }
{*******************}
{
 *  CFReadStreamCreateForHTTPRequest()
 *  
 *  Discussion:
 *    Create an HTTP read stream for the response to the given request.
 *    When the stream is opened, it will begin transmitting the
 *    request. The bytes returned are the pure body bytes; the response
 *    header has been parsed off. To retrieve the response header, ask
 *    for kCFStreamPropertyHTTPResponseHeader, above, any time after
 *    the first bytes arrive on the stream (or when stream end is
 *    reported, if there are no data bytes). When an HTTP/1.1 server
 *    returns a chunked a response, the chunks will be formed into one
 *    continuous stream.
 *  
 *  Parameters:
 *    
 *    alloc:
 *      A pointer to the CFAllocator which should be used to allocate
 *      memory for the CF read stream and its storage for values. If
 *      this reference is not a valid CFAllocator, the behavior is
 *      undefined.
 *    
 *    request:
 *      A pointer to a CFHTTPMessage created by the
 *      CFHTTPMessageCreateRequest function.
 *  
 *  Result:
 *    A pointer to the CF read stream created, or NULL if failed. It is
 *    caller's responsibilty to release the memory allocated for the
 *    read stream.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.1 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function CFReadStreamCreateForHTTPRequest( alloc: CFAllocatorRef; request: CFHTTPMessageRef ): CFReadStreamRef; external name '_CFReadStreamCreateForHTTPRequest';
(* AVAILABLE_MAC_OS_X_VERSION_10_1_AND_LATER *)


{
 *  CFReadStreamCreateForStreamedHTTPRequest()
 *  
 *  Discussion:
 *    Creates a read stream for the response to the given
 *    requestHeaders plus requestBody. Use in preference to
 *    CFReadStreamCreateForHTTPRequest() when the body of the request
 *    is larger than you wish to be resident in memory.  Note that
 *    because streams cannot be reset, read streams created this way
 *    cannot have autoredirection enabled.  If the Content-Length
 *    header is set in requestHeaders, it is assumed that the caller
 *    got the length right and that requestBody will report
 *    end-of-stream after precisely Content-Length bytes have been read
 *    from it. If the Content-Length header is not set, the chunked
 *    transfer-encoding will be added to requestHeaders, and bytes read
 *    from requestBody will be transmitted chunked. The body of
 *    requestHeaders is ignored.
 *  
 *  Parameters:
 *    
 *    alloc:
 *      A pointer to the CFAllocator which should be used to allocate
 *      memory for the CF read stream and its storage for values. If
 *      this reference is not a valid CFAllocator, the behavior is
 *      undefined.
 *    
 *    requestHeaders:
 *      A pointer to a CFHTTPMessage created by the
 *      CFHTTPMessageCreateRequest function. The body of requestHeaders
 *      is ignored.
 *    
 *    requestBody:
 *      A pointer to a CFReadStream.
 *  
 *  Result:
 *    A pointer to the CF read stream created, or NULL if failed. It is
 *    caller's responsibilty to release the memory allocated for the
 *    read stream.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function CFReadStreamCreateForStreamedHTTPRequest( alloc: CFAllocatorRef; requestHeaders: CFHTTPMessageRef; requestBody: CFReadStreamRef ): CFReadStreamRef; external name '_CFReadStreamCreateForStreamedHTTPRequest';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{
 *  CFHTTPReadStreamSetRedirectsAutomatically()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    Use the kCFStreamPropertyHTTPShouldAutoredirect property above
 *    instead.
 *  
 *  Discussion:
 *    Sets the redirection property on the http stream.
 *  
 *  Parameters:
 *    
 *    httpStream:
 *      A pointer to the CFHTTPStream to be set.
 *    
 *    shouldAutoRedirect:
 *      A boolean indicating whether to redirect or not.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.1 and later in CoreServices.framework but deprecated in 10.3
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure CFHTTPReadStreamSetRedirectsAutomatically( httpStream: CFReadStreamRef; shouldAutoRedirect: Boolean ); external name '_CFHTTPReadStreamSetRedirectsAutomatically';
(* AVAILABLE_MAC_OS_X_VERSION_10_1_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_3 *)


{
 *  CFHTTPReadStreamSetProxy()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    Use the kCFStreamPropertyHTTPProxy above instead.
 *  
 *  Discussion:
 *    Sets the redirection property on the http stream.
 *  
 *  Parameters:
 *    
 *    httpStream:
 *      A pointer to the CFHTTPStream to be set.
 *    
 *    proxyHost:
 *      The proxy hostname. A CFString value.
 *    
 *    proxyPort:
 *      The port number. A CFNumber value.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.1 and later in CoreServices.framework but deprecated in 10.3
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure CFHTTPReadStreamSetProxy( httpStream: CFReadStreamRef; proxyHost: CFStringRef; proxyPort: CFIndex ); external name '_CFHTTPReadStreamSetProxy';
(* AVAILABLE_MAC_OS_X_VERSION_10_1_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_3 *)


end.
