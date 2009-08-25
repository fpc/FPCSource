{
     File:       CFNetwork/CFNetworkErrors.h
 
     Contains:   CFNetwork error header
 
     Version:    CFNetwork-219~1
 
     Copyright:  © 2006 by Apple Computer, Inc., all rights reserved
 
}

{	 Pascal Translation:  Gale R Paeper, <gpaeper@empirenet.com>, 2008 }

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

unit CFNetworkErrorss;
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
uses MacTypes, CFBase;
{$ALIGN POWER}

{GRP translation note: Double 's' unit name ending intentional to avoid GPC redeclaration error with 'CFNetworkErrors' type identifier.}

{
 *  kCFErrorDomainCFNetwork
 *  
 *  Discussion:
 *    Error domain for all errors originating in CFNetwork. Error codes
 *    may be interpreted using the list below.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kCFErrorDomainCFNetwork: CFStringRef; external name '_kCFErrorDomainCFNetwork'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)

{
 *  kCFErrorDomainWinSock
 *  
 *  Discussion:
 *    On Windows, errors originating from WinSock are represented using
 *    this domain.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kCFErrorDomainWinSock: CFStringRef; external name '_kCFErrorDomainWinSock'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)


{
 *  CFNetworkErrors
 *  
 *  Discussion:
 *    The list of all error codes returned under the error domain
 *    kCFErrorDomainCFNetwork
 }
type
	CFNetworkErrors = SInt32;
const
	kCFHostErrorHostNotFound                   = 1;
	kCFHostErrorUnknown                        = 2;    { Query the kCFGetAddrInfoFailureKey to get the value returned from getaddrinfo; lookup in netdb.h}
                                                       { SOCKS errors; in all cases you may query kCFSOCKSStatusCodeKey to recover the status code returned by the server}
	kCFSOCKSErrorUnknownClientVersion          = 100;
	kCFSOCKSErrorUnsupportedServerVersion      = 101; { Query the kCFSOCKSVersionKey to find the version requested by the server}
                                                       { SOCKS4-specific errors}
	kCFSOCKS4ErrorRequestFailed                = 110;  { request rejected or failed by the server}
	kCFSOCKS4ErrorIdentdFailed                 = 111;  { request rejected because SOCKS server cannot connect to identd on the client}
	kCFSOCKS4ErrorIdConflict                   = 112;  { request rejected because the client program and identd report different user-ids}
	kCFSOCKS4ErrorUnknownStatusCode            = 113; { SOCKS5-specific errors}
	kCFSOCKS5ErrorBadState                     = 120;
	kCFSOCKS5ErrorBadResponseAddr              = 121;
	kCFSOCKS5ErrorBadCredentials               = 122;
	kCFSOCKS5ErrorUnsupportedNegotiationMethod = 123; { query kCFSOCKSNegotiationMethodKey to find the method requested}
	kCFSOCKS5ErrorNoAcceptableMethod           = 124; { Errors originating from CFNetServices}
	kCFNetServiceErrorUnknown                  = -72000;
	kCFNetServiceErrorCollision                = -72001;
	kCFNetServiceErrorNotFound                 = -72002;
	kCFNetServiceErrorInProgress               = -72003;
	kCFNetServiceErrorBadArgument              = -72004;
	kCFNetServiceErrorCancel                   = -72005;
	kCFNetServiceErrorInvalid                  = -72006;
	kCFNetServiceErrorTimeout                  = -72007;
	kCFNetServiceErrorDNSServiceFailure        = -73000; { An error from DNS discovery; look at kCFDNSServiceFailureKey to get the error number and interpret using dns_sd.h}
                                                      { FTP errors; query the kCFFTPStatusCodeKey to get the status code returned by the server}
	kCFFTPErrorUnexpectedStatusCode            = 200; { HTTP errors}
	kCFErrorHTTPAuthenticationTypeUnsupported  = 300;
	kCFErrorHTTPBadCredentials                 = 301;
	kCFErrorHTTPConnectionLost                 = 302;
	kCFErrorHTTPParseFailure                   = 303;
	kCFErrorHTTPRedirectionLoopDetected        = 304;
	kCFErrorHTTPBadURL                         = 305;
	kCFErrorHTTPProxyConnectionFailure         = 306;
	kCFErrorHTTPBadProxyCredentials            = 307;


{ Keys used by CFNetwork to pass additional error information back to the user within CFError's userInfo dictionary }
{
 *  kCFGetAddrInfoFailureKey
 *  
 *  Discussion:
 *    When an error of kCFHostErrorUnknown is returned, this key's
 *    value is set to a CFNumber containing the raw error value
 *    returned by getaddrinfo()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kCFGetAddrInfoFailureKey: CFStringRef; external name '_kCFGetAddrInfoFailureKey'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)

{
 *  kCFSOCKSStatusCodeKey
 *  
 *  Discussion:
 *    When a SOCKS failure has occurred, this key's value is set to a
 *    CFString containing the status value returned by the SOCKS server.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kCFSOCKSStatusCodeKey: CFStringRef; external name '_kCFSOCKSStatusCodeKey'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)

{
 *  kCFSOCKSVersionKey
 *  
 *  Discussion:
 *    When an error of kCFSOCKSErrorUnsupportedServerVersion is
 *    returned, this key's value is set to a CFString containing the
 *    version number requested by the server.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kCFSOCKSVersionKey: CFStringRef; external name '_kCFSOCKSVersionKey'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)

{
 *  kCFSOCKSNegotiationMethodKey
 *  
 *  Discussion:
 *    When an error of kCFSOCKS5ErrorUnsupportedNegotiationMethod is
 *    returned, this key's value is set to a CFString containing the
 *    negotiation method requested by the server.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kCFSOCKSNegotiationMethodKey: CFStringRef; external name '_kCFSOCKSNegotiationMethodKey'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)

{
 *  kCFDNSServiceFailureKey
 *  
 *  Discussion:
 *    When an error of kCFNetServicesErrorDNSServiceFailure is
 *    returned, this key's value is set to a CFNumber containing the
 *    value returned from DNS; interret it using the values dns_sd.h
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kCFDNSServiceFailureKey: CFStringRef; external name '_kCFDNSServiceFailureKey'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)

{
 *  kCFFTPStatusCodeKey
 *  
 *  Discussion:
 *    When an error of kCFFTPErrorUnexpectedStatusCode is returned,
 *    this key's value is set to a CFString containing the status code
 *    returned by the server
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kCFFTPStatusCodeKey: CFStringRef; external name '_kCFFTPStatusCodeKey'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)

end.
