{
     File:       CFNetwork/CFSocketStream.h
 
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

unit CFSocketStream;
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
uses MacTypes,CFStream,CFBase,CFHost,CFNetServices;
{$ALIGN MAC68K}

{
 *  kCFStreamErrorDomainSSL
 *  
 *  Discussion:
 *    Errors located in Security/SecureTransport.h
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kCFStreamErrorDomainSSL: SInt32; external name '_kCFStreamErrorDomainSSL'; (* attribute const *)


{
 *  kCFStreamPropertySocketSecurityLevel
 *  
 *  Discussion:
 *    Stream property key, for both set and copy operations. To set a
 *    stream to be secure, call CFReadStreamSetProperty or
 *    CFWriteStreamSetPropertywith the property name set to
 *    kCFStreamPropertySocketSecurityLevel and the value being one of
 *    the following values.  Streams may set a security level after
 *    open in order to allow on-the-fly securing of a stream.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kCFStreamPropertySocketSecurityLevel: CFStringRef; external name '_kCFStreamPropertySocketSecurityLevel'; (* attribute const *)


{
 *  kCFStreamSocketSecurityLevelNone
 *  
 *  Discussion:
 *    Stream property value, for both set and copy operations.
 *    Indicates to use no security (default setting).
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kCFStreamSocketSecurityLevelNone: CFStringRef; external name '_kCFStreamSocketSecurityLevelNone'; (* attribute const *)


{
 *  kCFStreamSocketSecurityLevelSSLv2
 *  
 *  Discussion:
 *    Stream property value, for both set and copy operations.
 *    Indicates to use SSLv2 security.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kCFStreamSocketSecurityLevelSSLv2: CFStringRef; external name '_kCFStreamSocketSecurityLevelSSLv2'; (* attribute const *)


{
 *  kCFStreamSocketSecurityLevelSSLv3
 *  
 *  Discussion:
 *    Stream property value, for both set and copy operations.
 *    Indicates to use SSLv3 security.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kCFStreamSocketSecurityLevelSSLv3: CFStringRef; external name '_kCFStreamSocketSecurityLevelSSLv3'; (* attribute const *)


{
 *  kCFStreamSocketSecurityLevelTLSv1
 *  
 *  Discussion:
 *    Stream property value, for both set and copy operations.
 *    Indicates to use TLSv1 security.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kCFStreamSocketSecurityLevelTLSv1: CFStringRef; external name '_kCFStreamSocketSecurityLevelTLSv1'; (* attribute const *)


{
 *  kCFStreamSocketSecurityLevelNegotiatedSSL
 *  
 *  Discussion:
 *    Stream property value, for both set and copy operations.
 *    Indicates to use TLS or SSL with fallback to lower versions. This
 *    is what HTTPS does, for instance.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kCFStreamSocketSecurityLevelNegotiatedSSL: CFStringRef; external name '_kCFStreamSocketSecurityLevelNegotiatedSSL'; (* attribute const *)


{
 *  kCFStreamErrorDomainSOCKS
 *  
 *  Discussion:
 *    SOCKS proxy error domain.  Errors formulated using inlines below.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kCFStreamErrorDomainSOCKS: SInt32; external name '_kCFStreamErrorDomainSOCKS'; (* attribute const *)


{
CF_INLINE
SInt32 CFSocketStreamSOCKSGetErrorSubdomain(CFStreamError* error)
    return ((error->domain >> 16) & 0x0000FFFF);

CF_INLINE
SInt32 CFSocketStreamSOCKSGetError(CFStreamError* error)
    return (error->domain & 0x0000FFFF);
}

const
  kCFStreamErrorSOCKSSubDomainNone = 0; { Error code is a general SOCKS error }
  kCFStreamErrorSOCKSSubDomainVersionCode = 1; { Error code is the version of SOCKS which the server wishes to use }
  kCFStreamErrorSOCKS4SubDomainResponse = 2; { Error code is the status code returned by the server }
  kCFStreamErrorSOCKS5SubDomainUserPass = 3; { Error code is the status code that the server returned }
  kCFStreamErrorSOCKS5SubDomainMethod = 4; { Error code is the server's desired negotiation method }
  kCFStreamErrorSOCKS5SubDomainResponse = 5; { Error code is the response code that the server returned in reply to the connection request }


{ kCFStreamErrorSOCKSSubDomainNone}
const
  kCFStreamErrorSOCKS5BadResponseAddr = 1;
  kCFStreamErrorSOCKS5BadState  = 2;
  kCFStreamErrorSOCKSUnknownClientVersion = 3;

{ kCFStreamErrorSOCKS4SubDomainResponse}
const
  kCFStreamErrorSOCKS4RequestFailed = 91; { request rejected or failed }
  kCFStreamErrorSOCKS4IdentdFailed = 92; { request rejected because SOCKS server cannot connect to identd on the client }
  kCFStreamErrorSOCKS4IdConflict = 93;   { request rejected because the client program and identd report different user-ids }

{ kCFStreamErrorSOCKS5SubDomainMethod}
const
  kSOCKS5NoAcceptableMethod     = $0FF;  { other values indicate the server's desired method }


{
 *  kCFStreamPropertySOCKSProxy
 *  
 *  Discussion:
 *    Stream property key, for both set and copy operations.  To set a
 *    stream to use a SOCKS proxy, call CFReadStreamSetProperty or
 *    CFWriteStreamSetProperty with the property name set to
 *    kCFStreamPropertySOCKSProxy and the value being a dictionary with
 *    at least the following two keys: kCFStreamPropertySOCKSProxyHost
 *    and kCFStreamPropertySOCKSProxyPort.  The dictionary returned by
 *    SystemConfiguration for SOCKS proxies will work without
 *    alteration.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kCFStreamPropertySOCKSProxy: CFStringRef; external name '_kCFStreamPropertySOCKSProxy'; (* attribute const *)

{
 *  kCFStreamPropertySOCKSProxyHost
 *  
 *  Discussion:
 *    CFDictinary key for SOCKS proxy information.  The key
 *    kCFStreamPropertySOCKSProxyHost should contain a CFStringRef
 *    value representing the SOCKS proxy host.  Defined to match
 *    kSCPropNetProxiesSOCKSProxy
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kCFStreamPropertySOCKSProxyHost: CFStringRef; external name '_kCFStreamPropertySOCKSProxyHost'; (* attribute const *)

{
 *  kCFStreamPropertySOCKSProxyPort
 *  
 *  Discussion:
 *    CFDictinary key for SOCKS proxy information.  The key
 *    kCFStreamPropertySOCKSProxyPort should contain a CFNumberRef
 *    which itself is of type kCFNumberSInt32Type.  This value should
 *    represent the port on which the proxy is listening.  Defined to
 *    match kSCPropNetProxiesSOCKSPort
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kCFStreamPropertySOCKSProxyPort: CFStringRef; external name '_kCFStreamPropertySOCKSProxyPort'; (* attribute const *)

{
 *  kCFStreamPropertySOCKSVersion
 *  
 *  Discussion:
 *    CFDictinary key for SOCKS proxy information.  By default, SOCKS5
 *    will be used unless there is a kCFStreamPropertySOCKSVersion key
 *    in the dictionary.  Its value must be
 *    kCFStreamSocketSOCKSVersion4 or kCFStreamSocketSOCKSVersion5 to
 *    set SOCKS4 or SOCKS5, respectively.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kCFStreamPropertySOCKSVersion: CFStringRef; external name '_kCFStreamPropertySOCKSVersion'; (* attribute const *)

{
 *  kCFStreamSocketSOCKSVersion4
 *  
 *  Discussion:
 *    CFDictionary value for SOCKS proxy information.  Indcates that
 *    SOCKS will or is using version 4 of the SOCKS protocol.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kCFStreamSocketSOCKSVersion4: CFStringRef; external name '_kCFStreamSocketSOCKSVersion4'; (* attribute const *)

{
 *  kCFStreamSocketSOCKSVersion5
 *  
 *  Discussion:
 *    CFDictionary value for SOCKS proxy information.  Indcates that
 *    SOCKS will or is using version 5 of the SOCKS protocol.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kCFStreamSocketSOCKSVersion5: CFStringRef; external name '_kCFStreamSocketSOCKSVersion5'; (* attribute const *)

{
 *  kCFStreamPropertySOCKSUser
 *  
 *  Discussion:
 *    CFDictinary key for SOCKS proxy information.  To set a user name
 *    and/or password, if required, the dictionary must contain the
 *    key(s) kCFStreamPropertySOCKSUser and/or  
 *    kCFStreamPropertySOCKSPassword with the value being the user's
 *    name as a CFStringRef and/or the user's password as a
 *    CFStringRef, respectively.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kCFStreamPropertySOCKSUser: CFStringRef; external name '_kCFStreamPropertySOCKSUser'; (* attribute const *)

{
 *  kCFStreamPropertySOCKSPassword
 *  
 *  Discussion:
 *    CFDictinary key for SOCKS proxy information.  To set a user name
 *    and/or password, if required, the dictionary must contain the
 *    key(s) kCFStreamPropertySOCKSUser and/or  
 *    kCFStreamPropertySOCKSPassword with the value being the user's
 *    name as a CFStringRef and/or the user's password as a
 *    CFStringRef, respectively.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kCFStreamPropertySOCKSPassword: CFStringRef; external name '_kCFStreamPropertySOCKSPassword'; (* attribute const *)


{
 *  kCFStreamPropertyShouldCloseNativeSocket
 *  
 *  Discussion:
 *    Set the value to kCFBooleanTrue if the stream should close and
 *    release the underlying native socket when the stream is released.
 *     Set the value to kCFBooleanFalse to keep the native socket from
 *    closing and releasing when the stream is released. If the stream
 *    was created with a native socket, the default property setting on
 *    the stream is kCFBooleanFalse. The
 *    kCFStreamPropertyShouldCloseNativeSocket can be set through
 *    CFReadStreamSetProperty or CFWriteStreamSetProperty.  The
 *    property can be copied through CFReadStreamCopyProperty or
 *    CFWriteStreamCopyProperty.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kCFStreamPropertyShouldCloseNativeSocket: CFStringRef; external name '_kCFStreamPropertyShouldCloseNativeSocket'; (* attribute const *)


{
 *  kCFStreamPropertySocketRemoteHost
 *  
 *  Discussion:
 *    Stream property key for copy operations. Returns a CFHostRef if
 *    known, otherwise NULL.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
// AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER
var kCFStreamPropertySocketRemoteHost: CFStringRef; external name '_kCFStreamPropertySocketRemoteHost'; (* attribute const *)


{
 *  kCFStreamPropertySocketRemoteNetService
 *  
 *  Discussion:
 *    Stream property key for copy operations. Returns a
 *    CFNetServiceRef if known, otherwise NULL.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
// AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER
var kCFStreamPropertySocketRemoteNetService: CFStringRef; external name '_kCFStreamPropertySocketRemoteNetService'; (* attribute const *)


{
 *  CFStreamCreatePairWithSocketToCFHost()
 *  
 *  Discussion:
 *    Given a CFHostRef, this function will create a pair of streams
 *    suitable for connecting to the host.  If there is a failure
 *    during creation, the stream references will be set to NULL.
 *  
 *  Mac OS X threading:
 *    Thread safe
 *  
 *  Parameters:
 *    
 *    alloc:
 *      The CFAllocator which should be used to allocate memory for the
 *      streams. If this reference is not a valid CFAllocator, the
 *      behavior is undefined.
 *    
 *    host:
 *      A reference to a CFHost to which the streams are desired.  If
 *      unresolved, the host will be resolved prior to connecting.
 *    
 *    port:
 *      The port to which the connection should be established.
 *    
 *    readStream:
 *      A pointer to a CFReadStreamRef which will be set to the new
 *      read stream instance.  Can be set to NULL if not desired.
 *    
 *    writeStream:
 *      A pointer to a CFWriteStreamRef which will be set to the new
 *      write stream instance.  Can be set to NULL if not desired.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
// AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER
procedure CFStreamCreatePairWithSocketToCFHost( alloc: CFAllocatorRef; host: CFHostRef; port: UInt32; readStream: CFReadStreamRefPtr; writeStream: CFWriteStreamRefPtr ); external name '_CFStreamCreatePairWithSocketToCFHost';


{
 *  CFStreamCreatePairWithSocketToNetService()
 *  
 *  Discussion:
 *    Given a CFNetService, this function will create a pair of streams
 *    suitable for connecting to the service.  If there is a failure
 *    during creation, the stream references will be set to NULL.
 *  
 *  Mac OS X threading:
 *    Thread safe
 *  
 *  Parameters:
 *    
 *    alloc:
 *      The CFAllocator which should be used to allocate memory for the
 *      streams. If this reference is not a valid CFAllocator, the
 *      behavior is undefined.
 *    
 *    service:
 *      A reference to a CFNetService to which the streams are desired.
 *       If unresolved, the service will be resolved prior to
 *      connecting.
 *    
 *    readStream:
 *      A pointer to a CFReadStreamRef which will be set to the new
 *      read stream instance.  Can be set to NULL if not desired.
 *    
 *    writeStream:
 *      A pointer to a CFWriteStreamRef which will be set to the new
 *      write stream instance.  Can be set to NULL if not desired.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
// AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER
procedure CFStreamCreatePairWithSocketToNetService( alloc: CFAllocatorRef; service: CFNetServiceRef; readStream: CFReadStreamRefPtr; writeStream: CFWriteStreamRefPtr ); external name '_CFStreamCreatePairWithSocketToNetService';


{
 *  CFStreamSocketSecurityProtocol
 *  
 *  Discussion:
 *    These enum values and CFSocketStreamPairSetSecurityProtocol have
 *    been deprecated in favor of CFReadStreamSetProperty and
 *    CFWriteStreamSetProperty with the previously mentioned property
 *    and values.
 }
type
	CFStreamSocketSecurityProtocol = SInt32;
const
  {
   * DEPRECATED, use kCFStreamSocketSecurityLevelNone
   }
  kCFStreamSocketSecurityNone   = 0;

  {
   * DEPRECATED, use kCFStreamSocketSecurityLevelSSLv2
   }
  kCFStreamSocketSecuritySSLv2  = 1;

  {
   * DEPRECATED, use kCFStreamSocketSecurityLevelSSLv3
   }
  kCFStreamSocketSecuritySSLv3  = 2;

  {
   * DEPRECATED, use kCFStreamSocketSecurityLevelNegotiatedSSL
   }
  kCFStreamSocketSecuritySSLv23 = 3;

  {
   * DEPRECATED, use kCFStreamSocketSecurityLevelTLSv1
   }
  kCFStreamSocketSecurityTLSv1  = 4;

{
 *  CFSocketStreamPairSetSecurityProtocol()   *** DEPRECATED ***
 *  
 *  Discussion:
 *    CFSocketStreamPairSetSecurityProtocol has been deprecated in
 *    favor of CFReadStreamSetProperty and CFWriteStreamSetProperty
 *    with the previously mentioned property and values.  Sets the
 *    security level on a pair of streams.
 *  
 *  Mac OS X threading:
 *    Thread safe
 *  
 *  Parameters:
 *    
 *    socketReadStream:
 *      Read stream reference which is to have its security level
 *      changed.
 *    
 *    socketWriteStream:
 *      Write stream reference which is to have its security level
 *      changed.
 *    
 *    securityProtocol:
 *      CFStreamSocketSecurityProtocol enum indicating the security
 *      level to be set.
 *  
 *  Result:
 *    Returns TRUE if the settings were placed on the stream, FALSE
 *    otherwise.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.1 and later in CoreServices.framework but deprecated in 10.2
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
// AVAILABLE_MAC_OS_X_VERSION_10_1_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_2
function CFSocketStreamPairSetSecurityProtocol( socketReadStream: CFReadStreamRef; socketWriteStream: CFWriteStreamRef; securityProtocol: CFStreamSocketSecurityProtocol ): Boolean; external name '_CFSocketStreamPairSetSecurityProtocol';

end.
