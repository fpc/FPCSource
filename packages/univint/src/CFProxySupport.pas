{
     File:       CFNetwork/CFProxySupport.h
 
     Contains:   Support for computing which proxy applies when
 
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

unit CFProxySupport;
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
uses MacTypes, CFArray, CFBase, CFDictionary, CFURL, CFError, CFRunLoop, CFStream;
{$ALIGN POWER}


{ 
    These APIs return arrays of dictionaries, where each dictionary describes a single proxy. 
    The arrays represent the order in which the proxies should be tried - try to download the URL
    using the first entry in the array, and if that fails, try using the second entry, and so on.

    The keys to the proxy dictionaries follow the function declarations; every proxy dictionary 
    will have an entry for kCFProxyTypeKey.  If the type is anything except 
    kCFProxyTypeAutoConfigurationURL, the dictionary will also have entries for the proxy's host
    and port (under kCFProxyHostNameKey and kCFProxyPortNumberKey respectively).  If the type is
    kCFProxyTypeAutoConfigurationURL, it will have an entry for kCFProxyAutoConfigurationURLKey.  
    
    The keys for username and password are optional and will only be present if the username 
    or password could be extracted from the information passed in (i.e. either the URL itself
    or the proxy dictionary supplied).  These APIs do not consult any external credential stores
    (such as the Keychain).
}


{
 *  CFNetworkCopyProxiesForURL()
 *  
 *  Discussion:
 *    Given a URL and a proxy dictionary, determines the ordered list
 *    of proxies that should be used to download the given URL.
 *  
 *  Parameters:
 *    
 *    url:
 *      The URL to be accessed
 *    
 *    proxySettings:
 *      A dictionary describing the available proxy settings; the
 *      dictionary's format should match that described in and returned
 *      by SystemConfiguration.framework
 *  
 *  Result:
 *    An array of dictionaries; each dictionary describes a single
 *    proxy.  See the comment at the top of this file for how to
 *    interpret the returned dictionaries.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function CFNetworkCopyProxiesForURL( url: CFURLRef; proxySettings: CFDictionaryRef ): CFArrayRef; external name '_CFNetworkCopyProxiesForURL';
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)


{
 *  CFProxyAutoConfigurationResultCallback
 *  
 *  Discussion:
 *    Callback function to be called when a PAC file computation
 *    (initiated by either CFNetworkExecuteProxyAutoConfigurationScript
 *    or CFNetworkExecuteProxyAutoConfigurationURL) has completed.
 *  
 *  Parameters:
 *    
 *    client:
 *      The client reference passed in to
 *      CFNetworkExecuteProxyAutoConfigurationScript or
 *      CFNetworkExecuteProxyAutoConfigurationURL
 *    
 *    proxyList:
 *      Upon success, the list of proxies returned by the
 *      autoconfiguration script.  The list has the same format as
 *      returned by CFProxyCopyProxiesForURL, above, except that no
 *      entry may be of type kCFProxyTypeAutoConfigurationURL.  Note
 *      that if the client wishes to keep this list, they must retain
 *      it when they receive this callback.
 *    
 *    error:
 *      Upon failure, an error object explaining the failure.
 }
type
	CFProxyAutoConfigurationResultCallback = procedure( client: UnivPtr; proxyList: CFArrayRef; error: CFErrorRef );
{
 *  CFNetworkCopyProxiesForAutoConfigurationScript()
 *  
 *  Discussion:
 *    Begins the process of executing proxyAutoConfigurationScript to
 *    determine the correct proxy to use to retrieve targetURL.  The
 *    caller should schedule the returned run loop source; when the
 *    results are found, the caller's callback will be called via the
 *    run loop, passing a valid proxyList and NULL error upon success,
 *    or a NULL proxyList and valid error on failure.  The caller
 *    should invalidate the returned run loop source if it wishes to
 *    terminate the request before completion.
 *  
 *  Parameters:
 *    
 *    proxyAutoConfigurationScript:
 *      A CFString containing the code of the script to be executed.
 *    
 *    targetURL:
 *      The URL that should be input in to the autoconfiguration script
 *  
 *  Result:
 *    An array of dictionaries describing the proxies returned by the
 *    script.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function CFNetworkCopyProxiesForAutoConfigurationScript( proxyAutoConfigurationScript: CFStringRef; targetURL: CFURLRef ): CFArrayRef; external name '_CFNetworkCopyProxiesForAutoConfigurationScript';
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)


{
 *  CFNetworkExecuteProxyAutoConfigurationURL()
 *  
 *  Discussion:
 *    As CFNetworkExecuteProxyAutoConfigurationScript(), above, except
 *    that CFNetworkExecuteProxyAutoConfigurationURL will additionally
 *    download the contents of proxyAutoConfigURL, convert it to a
 *    JavaScript string, and then execute that script.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function CFNetworkExecuteProxyAutoConfigurationURL( proxyAutoConfigURL: CFURLRef; targetURL: CFURLRef; cb: CFProxyAutoConfigurationResultCallback; var clientContext: CFStreamClientContext ): CFRunLoopSourceRef; external name '_CFNetworkExecuteProxyAutoConfigurationURL';
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)


{
 *  kCFProxyTypeKey
 *  
 *  Discussion:
 *    Key for the type of proxy being represented; value will be one of
 *    the kCFProxyType constants listed below.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kCFProxyTypeKey: CFStringRef; external name '_kCFProxyTypeKey'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)

{
 *  kCFProxyHostNameKey
 *  
 *  Discussion:
 *    Key for the proxy's hostname; value is a CFString.  Note that
 *    this may be an IPv4 or IPv6 dotted-IP string.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kCFProxyHostNameKey: CFStringRef; external name '_kCFProxyHostNameKey'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)

{
 *  kCFProxyPortNumberKey
 *  
 *  Discussion:
 *    Key for the proxy's port number; value is a CFNumber specifying
 *    the port on which to contact the proxy
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kCFProxyPortNumberKey: CFStringRef; external name '_kCFProxyPortNumberKey'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)

{
 *  kCFProxyAutoConfigurationURLKey
 *  
 *  Discussion:
 *    Key for the proxy's PAC file location; this key is only present
 *    if the proxy's type is kCFProxyTypeAutoConfigurationURL.  Value
 *    is a CFURL specifying the location of a proxy auto-configuration
 *    file
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kCFProxyAutoConfigurationURLKey: CFStringRef; external name '_kCFProxyAutoConfigurationURLKey'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)

{
 *  kCFProxyUsernameKey
 *  
 *  Discussion:
 *    Key for the username to be used with the proxy; value is a
 *    CFString. Note that this key will only be present if the username
 *    could be extracted from the information passed in.  No external
 *    credential stores (like the Keychain) are consulted.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kCFProxyUsernameKey: CFStringRef; external name '_kCFProxyUsernameKey'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)

{
 *  kCFProxyPasswordKey
 *  
 *  Discussion:
 *    Key for the password to be used with the proxy; value is a
 *    CFString. Note that this key will only be present if the username
 *    could be extracted from the information passed in.  No external
 *    credential stores (like the Keychain) are consulted.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kCFProxyPasswordKey: CFStringRef; external name '_kCFProxyPasswordKey'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)

{ 
    Possible values for kCFProxyTypeKey:
    kCFProxyTypeNone - no proxy should be used; contact the origin server directly
    kCFProxyTypeHTTP - the proxy is an HTTP proxy
    kCFProxyTypeHTTPS - the proxy is a tunneling proxy as used for HTTPS
    kCFProxyTypeSOCKS - the proxy is a SOCKS proxy
    kCFProxyTypeFTP - the proxy is an FTP proxy
    kCFProxyTypeAutoConfigurationURL - the proxy is specified by a proxy autoconfiguration (PAC) file
}
{
 *  kCFProxyTypeNone
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kCFProxyTypeNone: CFStringRef; external name '_kCFProxyTypeNone'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)
{
 *  kCFProxyTypeHTTP
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kCFProxyTypeHTTP: CFStringRef; external name '_kCFProxyTypeHTTP'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)
{
 *  kCFProxyTypeHTTPS
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kCFProxyTypeHTTPS: CFStringRef; external name '_kCFProxyTypeHTTPS'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)
{
 *  kCFProxyTypeSOCKS
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kCFProxyTypeSOCKS: CFStringRef; external name '_kCFProxyTypeSOCKS'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)
{
 *  kCFProxyTypeFTP
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kCFProxyTypeFTP: CFStringRef; external name '_kCFProxyTypeFTP'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)
{
 *  kCFProxyTypeAutoConfigurationURL
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kCFProxyTypeAutoConfigurationURL: CFStringRef; external name '_kCFProxyTypeAutoConfigurationURL'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)

end.
