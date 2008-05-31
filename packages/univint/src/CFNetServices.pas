{
     File:       CFNetwork/CFNetServices.h
 
     Contains:   CoreFoundation Network Net Services header
 
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

unit CFNetServices;
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
uses MacTypes,CFBase,CFStream,CFArray,CFRunLoop;
{$ALIGN MAC68K}
{
CFNetServices
Network Services is composed of two objects, a Network Service and
a Network Service Browser.  A Network Service represents a single
point service on the network.  Associated with a service is its name,
type of service, domain, port, and possibly protocol specific information
(for legacy protocols).  Services can be registered and resolved.
Registering a service advertises the service on the network, so other
computers can use the service.  Resolving a service performs a network
lookup in order to find the computer which has registered the service.
Lookup is contained by the services name, type of service, and the domain.
The port and address of the registered service will be returned.  Services
can be created or discovered.

Discovery of services takes place through the use of the Network Service
Browser.  Given a domain and a service type, the browser will search out
those services on the network.  The returned services can then be resolved
and then used.

Service type values are keywords as registered with IANA.  A list of values
may be retrieved from their web site at
<http://www.iana.org/assignments/port-numbers>.
}

{
 *  CFNetServiceRef
 *  
 *  Discussion:
 *    This is the type of a reference to a service.  It may be used for
 *    registering or for resolving.
 }
type
	CFNetServiceRef							= ^SInt32;

{
 *  CFNetServiceBrowserRef
 *  
 *  Discussion:
 *    This is the type of a reference to a service or domain browser.
 *    It may be used for discovering services or domains.
 }
type
	CFNetServiceBrowserRef							= ^SInt32;

{
 *  kCFStreamErrorDomainMach
 *  
 *  Discussion:
 *    Errors reported by mach.  See <mach/error.h>
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
// AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER
var kCFStreamErrorDomainMach: SInt32; external name '_kCFStreamErrorDomainMach'; (* attribute const *)


{
 *  kCFStreamErrorDomainNetServices
 *  
 *  Discussion:
 *    Errors listed below
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
// AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER
var kCFStreamErrorDomainNetServices: SInt32; external name '_kCFStreamErrorDomainNetServices'; (* attribute const *)


{
 *  CFNetServicesError
 *  
 *  Discussion:
 *    Errors from the kCFStreamErrorDomainNetServices domain.
 }
type CFNetServicesError = SInt32;
const
  {
   * An error of unknown type has occured.
   }
  kCFNetServicesErrorUnknown    = -72000;

  {
   * The given registration has had a name collision.  Registration
   * should be cancelled and should try again probably with a different
   * name.
   }
  kCFNetServicesErrorCollision  = -72001;

  {
   * Not used
   }
  kCFNetServicesErrorNotFound   = -72002;

  {
   * There is already a register, resolve, or browse invoke on the
   * given object.
   }
  kCFNetServicesErrorInProgress = -72003;

  {
   * Not used
   }
  kCFNetServicesErrorBadArgument = -72004;

  {
   * The register, resolve, or browse on the object has been cancelled.
   }
  kCFNetServicesErrorCancel     = -72005;

  {
   * The given CFNetServiceBrowser has already been invalidated and can
   * no longer be used for browsing.
   }
  kCFNetServicesErrorInvalid    = -72006;


{
 *  CFNetServiceBrowser flags
 *  
 *  Discussion:
 *    Result bit flags passed to CFNetServiceBrowserClientCallBack.
 }
const
	kCFNetServiceFlagMoreComing   = 1;    { Client will get another callback briefly and shouldn't do costly screen updates (or such). }
  kCFNetServiceFlagIsDomain     = 2;    { If off, the result is a service. }
  kCFNetServiceFlagIsRegistrationDomain = 4; { The result domain is the default registration domain. }
  kCFNetServiceFlagRemove       = 8;     { The result item should be removed and not added. }


{
 *  CFNetServiceClientContext
 *  
 *  Discussion:
 *    Structure containing the user-defined data and callbacks for
 *    CFNetService and CFNetServiceBrowser objects.
 }
type CFNetServiceClientContext = record

  {
   * The version number of the structure type being passed in as a
   * parameter to the CFNetService(Browser) client function. Valid
   * version number is currently 0.
   }
  version: CFIndex;

  {
   * An arbitrary pointer to client-defined data, which can be
   * associated with the service/browser and is passed to the callbacks.
   }
  info: Ptr;

  {
   * The callback used to add a retain for the service/browser on the
   * info pointer for the life of the service/browser, and may be used
   * for temporary references the service/browser needs to take. This
   * callback returns the actual info pointer to store in the
   * service/browser, almost always just the pointer passed as the
   * parameter.
   }
  retain: CFAllocatorRetainCallBack;

  {
   * The callback used to remove a retain previously added for the
   * service/browser on the info pointer.
   }
  release: CFAllocatorReleaseCallBack;

  {
   * The callback used to create a descriptive string representation of
   * the info pointer (or the data pointed to by the info pointer) for
   * debugging purposes. This is used by the CFCopyDescription()
   * function.
   }
  copyDescription: CFAllocatorCopyDescriptionCallBack;
end;
CFNetServiceClientContextPtr = ^CFNetServiceClientContext;

{
 *  CFNetServiceClientCallBack
 *  
 *  Discussion:
 *    Callback function which is called upon error or completion of
 *    resolve or register.  If resolving, the callback may be called
 *    multiple times, once for each resolved address.
 *  
 *  Parameters:
 *    
 *    theService:
 *      Service receiving the event.
 *    
 *    error:
 *      Reference to an error structure if the event is a failure.
 *    
 *    info:
 *      Client's info reference which was passed into the client
 *      context.
 }
	type
		CFNetServiceClientCallBack = procedure( service: CFNetServiceRef; var error: CFStreamError; info: Ptr );

{
 *  CFNetServiceBrowserClientCallBack
 *  
 *  Discussion:
 *    Callback function which is called upon error or upon successful
 *    discovery of services or domains.
 *  
 *  Parameters:
 *    
 *    browser:
 *      CFNetServiceBrowser receiving the event.
 *    
 *    flags:
 *      Bitwise flags indicating the event or further information about
 *      the event.
 *    
 *    domainOrService:
 *      If searching for domains, a CFStringRef indicating the domain
 *      which was found or is going away.  If searching for services,
 *      the service which was found or is going away.
 *    
 *    error:
 *      Reference to an error structure if the event is a failure.
 *    
 *    info:
 *      Client's info reference which was passed into the client
 *      context.
 }
	type
		CFNetServiceBrowserClientCallBack = procedure( browser: CFNetServiceBrowserRef; flags: CFOptionFlags; domainOrService: CFTypeRef; var error: CFStreamError; info: Ptr );


{
 *  CFNetServiceGetTypeID()
 *  
 *  Discussion:
 *    Returns the type identifier of all CFNetService instances.
 *  
 *  Mac OS X threading:
 *    Thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
// AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER
function CFNetServiceGetTypeID: CFTypeID; external name '_CFNetServiceGetTypeID';


{
 *  CFNetServiceBrowserGetTypeID()
 *  
 *  Discussion:
 *    Returns the type identifier of all CFNetServiceBrowser instances.
 *  
 *  Mac OS X threading:
 *    Thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
// AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER
function CFNetServiceBrowserGetTypeID: CFTypeID; external name '_CFNetServiceBrowserGetTypeID';


{
 *  CFNetServiceCreate()
 *  
 *  Discussion:
 *    Creates an instance of a Network Service.
 *  
 *  Mac OS X threading:
 *    Thread safe
 *  
 *  Parameters:
 *    
 *    alloc:
 *      The CFAllocator which should be used to allocate memory for the
 *      service and its storage for values. If this reference is not a
 *      valid CFAllocator, the behavior is undefined.
 *    
 *    domain:
 *      The network domain in which it is registered or will be
 *      registered. This value must be non-NULL.
 *    
 *    serviceType:
 *      The type of service being registered or resolved on the
 *      network. This value must be non-NULL.
 *    
 *    name:
 *      The name of the machine or application advertising the service.
 *       If this value is not unique, registering will eventually fail.
 *       This value must be non-NULL.  This value is usually displayed
 *      in a browser for the user.
 *    
 *    port:
 *      The port on which the service is listening.  This must be
 *      non-zero for services which are to be registered.
 *  
 *  Result:
 *    A valid CFNetService which may now be registered or resolved, or
 *    NULL if unsuccessful.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
// AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER
function CFNetServiceCreate(alloc: CFAllocatorRef; domain: CFStringRef; typ: CFStringRef; name: CFStringRef; port: UInt32): CFNetServiceRef; external name '_CFNetServiceCreate';


{
 *  CFNetServiceCreateCopy()
 *  
 *  Discussion:
 *    Creates a new CFNetService object as a copy of service argument.
 *  
 *  Mac OS X threading:
 *    Thread safe
 *  
 *  Parameters:
 *    
 *    alloc:
 *      The CFAllocator which should be used to allocate memory for the
 *      new service. If this reference is not a valid CFAllocator, the
 *      behavior is undefined.
 *    
 *    service:
 *      A CFNetServiceRef representing the original service. Must be
 *      non-NULL.  If this If this reference is not a valid
 *      CFNetServiceRef, the behavior is undefined.
 *  
 *  Result:
 *    Returns a valid CFNetServiceRef which contains a copy of all
 *    previously resolved data from the original.  NULL is returned in
 *    the case of failure.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
// AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER
function CFNetServiceCreateCopy( alloc: CFAllocatorRef; service: CFNetServiceRef ): CFNetServiceRef; external name '_CFNetServiceCreateCopy';


{
 *  CFNetServiceGetDomain()
 *  
 *  Discussion:
 *    Query a Network Service for its domain.
 *  
 *  Mac OS X threading:
 *    Thread safe
 *    The function gets the data in a thread-safe manner, but the
 *    resulting data is not safe.  Since it is returned as a matter of
 *    a get opposed to a copy, the data is not safe if the service is
 *    being altered from another thread.
 *  
 *  Parameters:
 *    
 *    theService:
 *      The Network Service to be queried.  Must be non-NULL.
 *  
 *  Result:
 *    CFStringRef which is the service's domain.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
// AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER
function CFNetServiceGetDomain(theService: CFNetServiceRef): CFStringRef; external name '_CFNetServiceGetDomain';


{
 *  CFNetServiceGetType()
 *  
 *  Discussion:
 *    Query a Network Service for its type.
 *  
 *  Mac OS X threading:
 *    Thread safe
 *    The function gets the data in a thread-safe manner, but the
 *    resulting data is not safe.  Since it is returned as a matter of
 *    a get opposed to a copy, the data is not safe if the service is
 *    being altered from another thread.
 *  
 *  Parameters:
 *    
 *    theService:
 *      The Network Service to be queried.  Must be non-NULL.
 *  
 *  Result:
 *    CFStringRef which is the service's service type.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
// AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER
function CFNetServiceGetType(theService: CFNetServiceRef): CFStringRef; external name '_CFNetServiceGetType';


{
 *  CFNetServiceGetName()
 *  
 *  Discussion:
 *    Query a Network Service for its name.
 *  
 *  Mac OS X threading:
 *    Thread safe
 *    The function gets the data in a thread-safe manner, but the
 *    resulting data is not safe.  Since it is returned as a matter of
 *    a get opposed to a copy, the data is not safe if the service is
 *    being altered from another thread.
 *  
 *  Parameters:
 *    
 *    theService:
 *      The Network Service to be queried.  Must be non-NULL.
 *  
 *  Result:
 *    CFStringRef which is the service's name.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
// AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER
function CFNetServiceGetName(theService: CFNetServiceRef): CFStringRef; external name '_CFNetServiceGetName';


{
 *  CFNetServiceGetAddressing()
 *  
 *  Discussion:
 *    Query a Network Service for its addressing.
 *  
 *  Mac OS X threading:
 *    Thread safe
 *    The function gets the data in a thread-safe manner, but the
 *    resulting data is not safe.  Since it is returned as a matter of
 *    a get opposed to a copy, the data is not safe if the service is
 *    being altered from another thread.
 *  
 *  Parameters:
 *    
 *    theService:
 *      The Network Service to be queried.  Must be non-NULL.
 *  
 *  Result:
 *    Returns NULL if the entity's addressing is not known (has not
 *    been resolved).  The array will contain a CFDataRef for each
 *    address resolved.  Each CFDataRef contains a struct sockaddr
 *    representing the address of the service.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
// AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER
function CFNetServiceGetAddressing(theService: CFNetServiceRef): CFArrayRef; external name '_CFNetServiceGetAddressing';


{
 *  CFNetServiceGetProtocolSpecificInformation()
 *  
 *  Discussion:
 *    Query a Network Service for its protocol specific information.
 *  
 *  Mac OS X threading:
 *    Thread safe
 *    The function gets the data in a thread-safe manner, but the
 *    resulting data is not safe.  Since it is returned as a matter of
 *    a get opposed to a copy, the data is not safe if the service is
 *    being altered from another thread.
 *  
 *  Parameters:
 *    
 *    theService:
 *      The Network Service to be queried.  Must be non-NULL.
 *  
 *  Result:
 *    Returns NULL if a resolve has not been performed or if
 *    CFNetServiceSetProtocolSpecificInformation has not been called. 
 *    It will return a CFStringRef containing the specific information
 *    if there is some.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
// AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER
function CFNetServiceGetProtocolSpecificInformation(theService: CFNetServiceRef): CFStringRef; external name '_CFNetServiceGetProtocolSpecificInformation';


{
 *  CFNetServiceSetProtocolSpecificInformation()
 *  
 *  Discussion:
 *    Set a Network Service's protocol specific information.
 *  
 *  Mac OS X threading:
 *    Thread safe
 *  
 *  Parameters:
 *    
 *    theService:
 *      The Network Service to be queried.  Must be non-NULL.
 *    
 *    theInfo:
 *      The protocol specific information to be added.  Pass NULL to
 *      remove the information from the service.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
// AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER
procedure CFNetServiceSetProtocolSpecificInformation(theService: CFNetServiceRef;theInfo: CFStringRef); external name '_CFNetServiceSetProtocolSpecificInformation';


{
 *  CFNetServiceRegister()
 *  
 *  Discussion:
 *    Registers the entity on the network.  This requires that the
 *    service has a domain, a type, a name, and a port.  The service is
 *    registered on the network until this function returns or is
 *    cancelled by calling CFNetServiceCancel.  In synchronous mode,
 *    this function will block until there is an error or it is
 *    cancelled from another thread.  In asynchronous mode, this
 *    function returns immediately and the underlying network
 *    registration process will start.
 *  
 *  Mac OS X threading:
 *    Thread safe
 *  
 *  Parameters:
 *    
 *    theService:
 *      The Network Service to register on the network.  Must be
 *      non-NULL.
 *    
 *    error:
 *      A reference to an error struct which will be set to the error
 *      and domain of the error should one occur.  If the value of
 *      error is not desired, set to NULL.
 *  
 *  Result:
 *    Returns FALSE if domain, type, name or port is NULL.  In
 *    synchronous mode, it will always return FALSE as a result of the
 *    error or the cancellation.  In asynchronous mode, it will return
 *    TRUE if the registration process could start.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
// AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER
function CFNetServiceRegister(theService: CFNetServiceRef;error: CFStreamErrorPtr): Boolean; external name '_CFNetServiceRegister';


{
 *  CFNetServiceResolve()
 *  
 *  Discussion:
 *    Resolves the addressing for the given service.  This requires
 *    that the service has a domain, a type, and a name.  The service
 *    is  resolved on the network until this function returns or is
 *    cancelled by calling CFNetServiceCancel. In synchronous mode,
 *    this function will block until there is an error or it is
 *    cancelled from another thread.  In asynchronous mode, this
 *    function returns immediately and the underlying network
 *    resolution process will start.
 *  
 *  Mac OS X threading:
 *    Thread safe
 *  
 *  Parameters:
 *    
 *    theService:
 *      The Network Service to resolve on the network.  Must be
 *      non-NULL.
 *    
 *    error:
 *      A reference to an error struct which will be set to the error
 *      and domain of the error should one occur.  If the value of
 *      error is not desired, set to NULL.
 *  
 *  Result:
 *    Returns FALSE if domain, type, or name is NULL.  In synchronous
 *    mode, it will return FALSE as a result of an error or a
 *    cancellation.  It will return TRUE if the resolution does
 *    succeed.  In asynchronous mode, it will return TRUE if the
 *    resolution process could start.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
// AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER
function CFNetServiceResolve(theService: CFNetServiceRef;error: CFStreamErrorPtr): Boolean; external name '_CFNetServiceResolve';


{
 *  CFNetServiceCancel()
 *  
 *  Discussion:
 *    Cancels an outstanding request for registration or resolution.
 *  
 *  Mac OS X threading:
 *    Thread safe
 *  
 *  Parameters:
 *    
 *    theService:
 *      The Network Service which is active.  Must be non-NULL.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
// AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER
procedure CFNetServiceCancel(theService: CFNetServiceRef); external name '_CFNetServiceCancel';


{
 *  CFNetServiceSetClient()
 *  
 *  Discussion:
 *    Sets up the service to be used in asynchronous mode. 
 *    Notification of registration failure or resolution completion
 *    will occur through the given callback.  Once the client is set,
 *    the service must be scheduled on a runloop. The client callback
 *    will be triggered via one of the scheduled run loops; It is the
 *    caller's responsibility to ensure that at least one of the
 *    scheduled run loops is being run.  This call must be performed
 *    before calling CFNetServiceRegister or CFNetServiceResolve,
 *    otherwise it will return FALSE.  TRUE will be returned if the
 *    client could be set.
 *  
 *  Mac OS X threading:
 *    Thread safe
 *  
 *  Parameters:
 *    
 *    theService:
 *      The service to set up for asynchronous mode.  Must be non-NULL.
 *    
 *    clientCB:
 *      Function pointer will be called upon registration failure or
 *      upon resolution completion.  In the case of resolution, this
 *      callback may be called multiple times if there is more than one
 *      address for a service.  Passing NULL will remove the client
 *      from the entity and cancel any outstanding activity.
 *    
 *    clientContext:
 *      Client contextual information to be used when calling clientCB.
 *      Passing NULL will remove the client from the entity and cancel
 *      any outstanding activity.
 *  
 *  Result:
 *    Returns FALSE if the client could not be set, TRUE otherwise.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
// AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER
function CFNetServiceSetClient(theService: CFNetServiceRef; clientCB: CFNetServiceClientCallBack; clientContext: CFNetServiceClientContextPtr): Boolean; external name '_CFNetServiceSetClient';


{
 *  CFNetServiceScheduleWithRunLoop()
 *  
 *  Discussion:
 *    Schedule the given service on the given run loop and mode.
 *  
 *  Mac OS X threading:
 *    Thread safe
 *  
 *  Parameters:
 *    
 *    theService:
 *      The service which is set up for asynchronous mode. Must be
 *      non-NULL.
 *    
 *    runLoop:
 *      A reference to a runloop on which the service should be
 *      scheduled. Must be non-NULL.
 *    
 *    runLoopMode:
 *      The mode on which to schedule the service.  Must be non-NULL.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
// AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER
procedure CFNetServiceScheduleWithRunLoop(theService: CFNetServiceRef; runLoop: CFRunLoopRef; runLoopMode: CFStringRef); external name '_CFNetServiceScheduleWithRunLoop';


{
 *  CFNetServiceUnscheduleFromRunLoop()
 *  
 *  Discussion:
 *    Unschedule the given service from the given run loop and mode.
 *  
 *  Mac OS X threading:
 *    Thread safe
 *  
 *  Parameters:
 *    
 *    theService:
 *      The service which is set up for asynchronous mode.  Must be
 *      non-NULL.
 *    
 *    runLoop:
 *      A reference to a runloop from which the service should be
 *      unscheduled.  Must be non-NULL.
 *    
 *    runLoopMode:
 *      The mode from which to unschedule the service.  Must be
 *      non-NULL.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
// AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER
procedure CFNetServiceUnscheduleFromRunLoop(theService: CFNetServiceRef; runLoop: CFRunLoopRef; runLoopMode: CFStringRef); external name '_CFNetServiceUnscheduleFromRunLoop';


{
 *  CFNetServiceBrowserCreate()
 *  
 *  Discussion:
 *    Creates an instance of a browser object.
 *  
 *  Mac OS X threading:
 *    Thread safe
 *  
 *  Parameters:
 *    
 *    alloc:
 *      The CFAllocator which should be used to allocate memory for the
 *      browser and its storage for values. If this reference is not a
 *      valid CFAllocator, the behavior is undefined.
 *    
 *    clientCB:
 *      Function pointer that will be called as domains or services are
 *      found on the network.  Must be non-NULL.
 *    
 *    clientContext:
 *      Client ontextual information to be used when calling clientCB.
 *      Must be non-NULL.
 *  
 *  Result:
 *    Returns a new instance of a browser, or NULL if the instance
 *    could not be created.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
// AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER
function CFNetServiceBrowserCreate(alloc: CFAllocatorRef; clientCB: CFNetServiceBrowserClientCallBack; var clientContext: CFNetServiceClientContext): CFNetServiceBrowserRef; external name '_CFNetServiceBrowserCreate';


{
 *  CFNetServiceBrowserInvalidate()
 *  
 *  Discussion:
 *    Invalidates the given browser object so that it may no longer be
 *    scheduled and callback never be called.  This will also stop any
 *    searches currently in progress.
 *  
 *  Parameters:
 *    
 *    browser:
 *      Network Service Browser to invalidate.  Must be non-NULL.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
// AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER
procedure CFNetServiceBrowserInvalidate(browser: CFNetServiceBrowserRef); external name '_CFNetServiceBrowserInvalidate';


{
 *  CFNetServiceBrowserSearchForDomains()
 *  
 *  Discussion:
 *    Starts a search for domains.  The browser will either try to find
 *    "Browse" domains or will search for "Registration" domains.  If
 *    there is already an outstanding search, it will return FALSE.  In
 *    syncronous mode, this call blocks until the search is stopped. 
 *    It will return FALSE if there is an error performing the search.
 *    It will return TRUE otherwise.  In asynchronous mode, this call
 *    will return TRUE or FALSE depending if the underlying network
 *    search could be started.
 *  
 *  Mac OS X threading:
 *    Thread safe
 *  
 *  Parameters:
 *    
 *    browser:
 *      Network Service Browser to perform the search.  Must be
 *      non-NULL.
 *    
 *    registrationDomains:
 *      FALSE if "Browse" domains are to be discovered. TRUE if only
 *      "Registration" domains are to be discovered.
 *    
 *    error:
 *      A reference to an error struct which will be set to the error
 *      and domain of the error should one occur.  If the value of
 *      error is not desired, set to NULL.
 *  
 *  Result:
 *    Returns FALSE if an error occurs during a synchronous search or
 *    if the search could not start.  It returns TRUE otherwise.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
// AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER
function CFNetServiceBrowserSearchForDomains(browser: CFNetServiceBrowserRef; registrationDomains: Boolean; error: CFStreamErrorPtr): Boolean; external name '_CFNetServiceBrowserSearchForDomains';


{
 *  CFNetServiceBrowserSearchForServices()
 *  
 *  Discussion:
 *    Starts a search for a service type on the given domain.  If there
 *    is already an outstanding search, it will return FALSE.  In
 *    syncronous mode, this call blocks until the search is stopped. 
 *    It will return FALSE if there is an error performing the search
 *    or if there is some other error.  It will return TRUE otherwise.
 *    In asynchronous mode, this call will return TRUE or FALSE
 *    depending if the underlying network search could be instantiated.
 *  
 *  Mac OS X threading:
 *    Thread safe
 *  
 *  Parameters:
 *    
 *    browser:
 *      Network Service Browser to perform the search.  Must be
 *      non-NULL.
 *    
 *    domain:
 *      Network domain to search in order to find the service.  Must be
 *      non-NULL.
 *    
 *    serviceType:
 *      Service type for which to search.  Must be non-NULL.
 *    
 *    error:
 *      A reference to an error struct which will be set to the error
 *      and domain of the error should one occur.  If the value of
 *      error is not desired, set to NULL.
 *  
 *  Result:
 *    Returns FALSE if an error occurs during a synchronous search or
 *    if the search could not start.  It returns TRUE otherwise.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
// AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER
function CFNetServiceBrowserSearchForServices(browser: CFNetServiceBrowserRef; domain: CFStringRef; typ: CFStringRef; error: CFStreamErrorPtr): Boolean; external name '_CFNetServiceBrowserSearchForServices';


{
 *  CFNetServiceBrowserStopSearch()
 *  
 *  Discussion:
 *    Stops an outstanding browser search.
 *  
 *  Mac OS X threading:
 *    Thread safe
 *  
 *  Parameters:
 *    
 *    browser:
 *      Network Service Browser performing the search.  Must be
 *      non-NULL.
 *    
 *    error:
 *      Error value to be returned in "error" in
 *      CFNetServiceBrowserStartServiceSearch if search is being
 *      performed in synchronous mode.  In this case, a non-zero of the
 *      error field of the struct will cause
 *      CFNetServiceBrowserStartServiceSearch to return FALSE. In
 *      asynchronous mode, the client call back will be called with
 *      this error.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
// AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER
procedure CFNetServiceBrowserStopSearch(browser: CFNetServiceBrowserRef;var error: CFStreamError); external name '_CFNetServiceBrowserStopSearch';


{
 *  CFNetServiceBrowserScheduleWithRunLoop()
 *  
 *  Discussion:
 *    Schedules the browser on a run loop and mode.  Use this to place
 *    the given browser into asynchronous mode.  The client callback
 *    will be triggered via one of the scheduled run loops; It is the
 *    caller's responsibility to ensure that at least one of the
 *    scheduled run loops is being run.
 *  
 *  Mac OS X threading:
 *    Thread safe
 *  
 *  Parameters:
 *    
 *    browser:
 *      Network Service Browser to schedule.  Must be non-NULL.
 *    
 *    runLoop:
 *      A reference to a runloop on which the browser should be
 *      scheduled.  Must be non-NULL.
 *    
 *    runLoopMode:
 *      The mode on which to schedule the browser.  Must be non-NULL.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
// AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER
procedure CFNetServiceBrowserScheduleWithRunLoop(browser: CFNetServiceBrowserRef; runLoop: CFRunLoopRef; runLoopMode: CFStringRef); external name '_CFNetServiceBrowserScheduleWithRunLoop';


{
 *  CFNetServiceBrowserUnscheduleFromRunLoop()
 *  
 *  Discussion:
 *    Unschedules the browser from a run loop and mode.
 *  
 *  Mac OS X threading:
 *    Thread safe
 *  
 *  Parameters:
 *    
 *    browser:
 *      Network Service Browser to unschedule.  Must be non-NULL.
 *    
 *    runLoop:
 *      A reference to a runloop from which the browser should be
 *      unscheduled. Must be non-NULL.
 *    
 *    runLoopMode:
 *      The mode from which to unschedule the browser.  Must be
 *      non-NULL.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
// AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER
procedure CFNetServiceBrowserUnscheduleFromRunLoop(browser: CFNetServiceBrowserRef; runLoop: CFRunLoopRef; runLoopMode: CFStringRef); external name '_CFNetServiceBrowserUnscheduleFromRunLoop';

end.
