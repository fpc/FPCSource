{
	 File:	   CFNetwork/CFNetServices.h
 
	 Contains:   CoreFoundation Network Net Services header
 
	 Copyright:  Copyright (c) 2001-2008, Apple Inc. All rights reserved.
 
	 Bugs?:	  For bug reports, consult the following page on
				 the World Wide Web:
 
					 http://bugs.freepascal.org
 
}
{       Pascal Translation:  Peter N Lewis, <peter@stairways.com.au>, 2004 }
{       Pascal Translation Updated:  Gale R Paeper, <gpaeper@empirenet.com>, 2008 }
{       Pascal Translation Updated:  Jonas Maebe, <jonas@freepascal.org>, October 2009 }
{       Pascal Translation Updated:  Jonas Maebe, <jonas@freepascal.org>, October 2012 }
{       Pascal Translation Updated: Jonas Maebe <jonas@freepascal.org>, August 2015 }
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

{$IFNDEF FPC_DOTTEDUNITS}
unit CFNetServices;
{$ENDIF FPC_DOTTEDUNITS}
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
{$IFDEF FPC_DOTTEDUNITS}
uses MacOsApi.MacTypes,MacOsApi.CFBase,MacOsApi.CFStream,MacOsApi.CFArray,MacOsApi.CFRunLoop, MacOsApi.CFData, MacOsApi.CFDate, MacOsApi.CFDictionary;
{$ELSE FPC_DOTTEDUNITS}
uses MacTypes,CFBase,CFStream,CFArray,CFRunLoop, CFData, CFDate, CFDictionary;
{$ENDIF FPC_DOTTEDUNITS}
{$endc} {not MACOSALLINCLUDE}

{$ALIGN MAC68K}


{
 *  CFNetServiceRef
 *  
 *  Discussion:
 *	This is the type of a reference to a service.  It may be used for
 *	registering or for resolving.
 }
type
	CFNetServiceRef = ^__CFNetService; { an opaque type }
	__CFNetService = record end;

{
 *  CFNetServiceMonitorRef
 *  
 *  Discussion:
 *	This is the type of a reference to a service monitor.  It may be
 *	used for watching record changes on a CFNetServiceRef.
 }
type
	CFNetServiceMonitorRef = ^__CFNetServiceMonitor; { an opaque type }
	__CFNetServiceMonitor = record end;

{
 *  CFNetServiceBrowserRef
 *  
 *  Discussion:
 *	This is the type of a reference to a service or domain browser.
 *	It may be used for discovering services or domains.
 }
type
	CFNetServiceBrowserRef = ^__CFNetServiceBrowser; { an opaque type }
	__CFNetServiceBrowser = record end;
{
 *  kCFStreamErrorDomainMach
 *  
 *  Discussion:
 *	Errors reported by mach.  See <mach/error.h>
 *  
 *  Availability:
 *	Mac OS X:		 in version 10.2 and later in CoreServices.framework
 *	CarbonLib:		not available
 *	Non-Carbon CFM:   not available
 }
var kCFStreamErrorDomainMach: SInt32; external name '_kCFStreamErrorDomainMach'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_2,__IPHONE_2_0) *)

{
 *  kCFStreamErrorDomainNetServices
 *  
 *  Discussion:
 *	Errors listed below or reported by the Service Discovery API's.
 *	See <dns_sd.h>.  The Service Discovery errors will only be
 *	returned when using the new, Mac OS X 10.4-based API's or
 *	CFNetServiceBrowser.
 *  
 *  Availability:
 *	Mac OS X:		 in version 10.2 and later in CoreServices.framework
 *	CarbonLib:		not available
 *	Non-Carbon CFM:   not available
 }
var kCFStreamErrorDomainNetServices: SInt32; external name '_kCFStreamErrorDomainNetServices'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_2,__IPHONE_2_0) *)


{
 *  CFNetServicesError
 *  
 *  Discussion:
 *	Errors from the kCFStreamErrorDomainNetServices domain.
 }
type
	CFNetServicesError = SInt32;
const
{
   * An error of unknown type has occurred.
   }
	kCFNetServicesErrorUnknown = -72000;

  {
   * The given registration has had a name collision.  Registration
   * should be cancelled and should try again probably with a different
   * name.
   }
	kCFNetServicesErrorCollision = -72001;

  {
   * Not used
   }
	kCFNetServicesErrorNotFound = -72002;

  {
   * There is already a register, resolve, browse, or monitor in
   * progress on the given object.
   }
	kCFNetServicesErrorInProgress = -72003;

  {
   * Not used
   }
	kCFNetServicesErrorBadArgument = -72004;

  {
   * The register, resolve, or browse on the object has been cancelled.
   }
	kCFNetServicesErrorCancel = -72005;

  {
   * The given CFNetServiceBrowser or CFNetServiceMonitor has already
   * been invalidated and can no longer be used.
   }
	kCFNetServicesErrorInvalid = -72006;

  {
   * The given CFNetServiceResolveWithTimeout has hit the timeout
   * before a successful resolve.
   }
	kCFNetServicesErrorTimeout = -72007;


{
 *  CFNetServiceMonitorType
 *  
 *  Discussion:
 *	Record type specifier in order to inform CFNetServiceMonitor to
 *	watch for certain record changes.
 }
type
	CFNetServiceMonitorType = SInt32;
const
{
   * Watch for TXT record changes.
   }
	kCFNetServiceMonitorTXT = 1;


{
 *  CFNetService flags
 *
 *  Discussion:
 *	  Bit flags to be used for registration of a service with CFNetServiceRegisterWithOptions.
 }
const
	kCFNetServiceFlagNoAutoRename = 1;	 { Indicate that registration should not auto-rename the service to prevent name conflicts.}


{
 *  CFNetServiceBrowser flags
 *  
 *  Discussion:
 *	Result bit flags passed to CFNetServiceBrowserClientCallBack.
 }
const
	kCFNetServiceFlagMoreComing = 1;  { Client will get another callback briefly and shouldn't do costly screen updates (or such).}
	kCFNetServiceFlagIsDomain = 2;  { If off, the result is a service.}
	kCFNetServiceFlagIsDefault = 4;  { The result domain is the default domain for the given domain browse type (registration or browse).}
	kCFNetServiceFlagIsRegistrationDomain = 4;  { Same as the previous but incorrectly named. Kept for compatibility.}
	kCFNetServiceFlagRemove = 8;   { The result item should be removed and not added.}


{
 *  CFNetServiceClientContext
 *  
 *  Discussion:
 *	Structure containing the user-defined data and callbacks for
 *	CFNetService and CFNetServiceBrowser objects.
 }
type
	CFNetServiceClientContext = record
{
   * The version number of the structure type being passed in as a
   * parameter to the CFNetService, Browser, or Monitor client
   * function.  The current version number is 0.
   }
		version: CFIndex;

  {
   * An arbitrary pointer to client-defined data, which can be
   * associated with the service/browser and is passed to the callbacks.
   }
		info: UnivPtr;

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
 *	Callback function which is called upon error or completion of
 *	resolve or register.  If resolving with the deprecated API's, the
 *	callback may be called multiple times, once for each resolved
 *	address.
 *  
 *  Parameters:
 *	
 *	theService:
 *	  Service receiving the event.
 *	
 *	error:
 *	  Reference to an error structure if the event is a failure.
 *	
 *	info:
 *	  Client's info reference which was passed into the client
 *	  context.
 }
type
	CFNetServiceClientCallBack = procedure( theService: CFNetServiceRef; var error: CFStreamError; info: UnivPtr );

{
 *  CFNetServiceMonitorClientCallBack
 *  
 *  Discussion:
 *	Callback function which is called as the monitored record changes.
 *  
 *  Parameters:
 *	
 *	theMonitor:
 *	  CFNetServiceMonitor receiving the event.
 *	
 *	theService:
 *	  Service receiving the event.
 *	
 *	typeInfo:
 *	  The information type which changed.
 *	
 *	rdata:
 *	  The contents of the record that changed.
 *	
 *	error:
 *	  Reference to an error structure if the event is a failure.
 *	
 *	info:
 *	  Client's info reference which was passed into the client
 *	  context.
 }
type
	CFNetServiceMonitorClientCallBack = procedure( theMonitor: CFNetServiceMonitorRef; theService: CFNetServiceRef; typeInfo: CFNetServiceMonitorType; rdata: CFDataRef; var error: CFStreamError; info: UnivPtr );

{
 *  CFNetServiceBrowserClientCallBack
 *  
 *  Discussion:
 *	Callback function which is called upon error or upon successful
 *	discovery of services or domains.
 *  
 *  Parameters:
 *	
 *	browser:
 *	  CFNetServiceBrowser receiving the event.
 *	
 *	flags:
 *	  Bitwise flags indicating the event or further information about
 *	  the event.
 *	
 *	domainOrService:
 *	  If searching for domains, a CFStringRef indicating the domain
 *	  which was found or is going away.  If searching for services,
 *	  the service which was found or is going away.
 *	
 *	error:
 *	  Reference to an error structure if the event is a failure.
 *	
 *	info:
 *	  Client's info reference which was passed into the client
 *	  context.
 }
type
	CFNetServiceBrowserClientCallBack = procedure( browser: CFNetServiceBrowserRef; flags: CFOptionFlags; domainOrService: CFTypeRef; var error: CFStreamError; info: UnivPtr );
{
 *  CFNetServiceGetTypeID()
 *  
 *  Discussion:
 *	Returns the type identifier of all CFNetService instances.
 *  
 *  Mac OS X threading:
 *	Thread safe
 *  
 *  Availability:
 *	Mac OS X:		 in version 10.2 and later in CoreServices.framework
 *	CarbonLib:		not available
 *	Non-Carbon CFM:   not available
 }
function CFNetServiceGetTypeID: CFTypeID; external name '_CFNetServiceGetTypeID';
(* __OSX_AVAILABLE_STARTING(__MAC_10_2,__IPHONE_2_0) *)


{
 *  CFNetServiceMonitorGetTypeID()
 *  
 *  Discussion:
 *	Returns the type identifier of all CFNetServiceMonitor instances.
 *  
 *  Mac OS X threading:
 *	Thread safe
 *  
 *  Availability:
 *	Mac OS X:		 in version 10.4 and later in CoreServices.framework
 *	CarbonLib:		not available
 *	Non-Carbon CFM:   not available
 }
function CFNetServiceMonitorGetTypeID: CFTypeID; external name '_CFNetServiceMonitorGetTypeID';
(* __OSX_AVAILABLE_STARTING(__MAC_10_4,__IPHONE_2_0) *)


{
 *  CFNetServiceBrowserGetTypeID()
 *  
 *  Discussion:
 *	Returns the type identifier of all CFNetServiceBrowser instances.
 *  
 *  Mac OS X threading:
 *	Thread safe
 *  
 *  Availability:
 *	Mac OS X:		 in version 10.2 and later in CoreServices.framework
 *	CarbonLib:		not available
 *	Non-Carbon CFM:   not available
 }
function CFNetServiceBrowserGetTypeID: CFTypeID; external name '_CFNetServiceBrowserGetTypeID';
(* __OSX_AVAILABLE_STARTING(__MAC_10_2,__IPHONE_2_0) *)


{
 *  CFNetServiceCreate()
 *  
 *  Discussion:
 *	Creates an instance of a Network Service.
 *  
 *  Mac OS X threading:
 *	Thread safe
 *  
 *  Parameters:
 *	
 *	alloc:
 *	  The CFAllocator which should be used to allocate memory for the
 *	  service and its storage for values. If this reference is not a
 *	  valid CFAllocator, the behavior is undefined.
 *	
 *	domain:
 *	  The network domain in which it is registered or will be
 *	  registered. This value must be non-NULL.
 *	
 *	serviceType:
 *	  The type of service being registered or resolved on the
 *	  network. The service type consists of the application protocol
 *	  name followed by the transport protocol name, separated by a
 *	  dot (e.g. "_ftp._tcp").  The application protocol name should
 *	  be 14 characters or less, and should only contain lower-case
 *	  letters, digits, and hyphens.  New service types should be
 *	  registered at <htp://www.dns-sd.org/ServiceTypes.html>.  This
 *	  value must be non-NULL.
 *	
 *	name:
 *	  The name of the machine or application advertising the service.
 *	   If this value is not unique, registering will eventually fail.
 *	   This value must be non-NULL.  This value is usually displayed
 *	  in a browser for the user.
 *	
 *	port:
 *	  The port on which the service is listening.  This must be
 *	  non-zero for services which are to be registered.
 *  
 *  Result:
 *	A valid CFNetService which may now be registered or resolved, or
 *	NULL if unsuccessful.
 *  
 *  Availability:
 *	Mac OS X:		 in version 10.2 and later in CoreServices.framework
 *	CarbonLib:		not available
 *	Non-Carbon CFM:   not available
 }
function CFNetServiceCreate( alloc: CFAllocatorRef; domain: CFStringRef; serviceType: CFStringRef; name: CFStringRef; port: SInt32 ): CFNetServiceRef; external name '_CFNetServiceCreate';
(* __OSX_AVAILABLE_STARTING(__MAC_10_2,__IPHONE_2_0) *)


{
 *  CFNetServiceCreateCopy()
 *  
 *  Discussion:
 *	Creates a new CFNetService object as a copy of service argument.
 *  
 *  Mac OS X threading:
 *	Thread safe
 *  
 *  Parameters:
 *	
 *	alloc:
 *	  The CFAllocator which should be used to allocate memory for the
 *	  new service. If this reference is not a valid CFAllocator, the
 *	  behavior is undefined.
 *	
 *	service:
 *	  A CFNetServiceRef representing the original service. Must be
 *	  non-NULL.  If this If this reference is not a valid
 *	  CFNetServiceRef, the behavior is undefined.
 *  
 *  Result:
 *	Returns a valid CFNetServiceRef which contains a copy of all
 *	previously resolved data from the original.  NULL is returned in
 *	the case of failure.
 *  
 *  Availability:
 *	Mac OS X:		 in version 10.3 and later in CoreServices.framework
 *	CarbonLib:		not available
 *	Non-Carbon CFM:   not available
 }
function CFNetServiceCreateCopy( alloc: CFAllocatorRef; service: CFNetServiceRef ): CFNetServiceRef; external name '_CFNetServiceCreateCopy';
(* __OSX_AVAILABLE_STARTING(__MAC_10_3,__IPHONE_2_0) *)


{
 *  CFNetServiceGetDomain()
 *  
 *  Discussion:
 *	Query a Network Service for its domain.
 *  
 *  Mac OS X threading:
 *	Thread safe
 *	The function gets the data in a thread-safe manner, but the
 *	resulting data is not safe.  Since it is returned as a matter of
 *	a get opposed to a copy, the data is not safe if the service is
 *	being altered from another thread.
 *  
 *  Parameters:
 *	
 *	theService:
 *	  The Network Service to be queried.  Must be non-NULL.
 *  
 *  Result:
 *	CFStringRef which is the service's domain.
 *  
 *  Availability:
 *	Mac OS X:		 in version 10.2 and later in CoreServices.framework
 *	CarbonLib:		not available
 *	Non-Carbon CFM:   not available
 }
function CFNetServiceGetDomain( theService: CFNetServiceRef ): CFStringRef; external name '_CFNetServiceGetDomain';
(* __OSX_AVAILABLE_STARTING(__MAC_10_2,__IPHONE_2_0) *)


{
 *  CFNetServiceGetType()
 *  
 *  Discussion:
 *	Query a Network Service for its type.
 *  
 *  Mac OS X threading:
 *	Thread safe
 *	The function gets the data in a thread-safe manner, but the
 *	resulting data is not safe.  Since it is returned as a matter of
 *	a get opposed to a copy, the data is not safe if the service is
 *	being altered from another thread.
 *  
 *  Parameters:
 *	
 *	theService:
 *	  The Network Service to be queried.  Must be non-NULL.
 *  
 *  Result:
 *	CFStringRef which is the service's service type.
 *  
 *  Availability:
 *	Mac OS X:		 in version 10.2 and later in CoreServices.framework
 *	CarbonLib:		not available
 *	Non-Carbon CFM:   not available
 }
function CFNetServiceGetType( theService: CFNetServiceRef ): CFStringRef; external name '_CFNetServiceGetType';
(* __OSX_AVAILABLE_STARTING(__MAC_10_2,__IPHONE_2_0) *)


{
 *  CFNetServiceGetName()
 *  
 *  Discussion:
 *	Query a Network Service for its name.
 *  
 *  Mac OS X threading:
 *	Thread safe
 *	The function gets the data in a thread-safe manner, but the
 *	resulting data is not safe.  Since it is returned as a matter of
 *	a get opposed to a copy, the data is not safe if the service is
 *	being altered from another thread.
 *  
 *  Parameters:
 *	
 *	theService:
 *	  The Network Service to be queried.  Must be non-NULL.
 *  
 *  Result:
 *	CFStringRef which is the service's name.
 *  
 *  Availability:
 *	Mac OS X:		 in version 10.2 and later in CoreServices.framework
 *	CarbonLib:		not available
 *	Non-Carbon CFM:   not available
 }
function CFNetServiceGetName( theService: CFNetServiceRef ): CFStringRef; external name '_CFNetServiceGetName';
(* __OSX_AVAILABLE_STARTING(__MAC_10_2,__IPHONE_2_0) *)


{
 *  CFNetServiceRegisterWithOptions()
 *  
 *  Discussion:
 *	Registers the entity on the network.  This requires that the
 *	service has a domain, a type, a name, and a port.  The service is
 *	registered on the network until this function returns or is
 *	cancelled by calling CFNetServiceCancel.  In synchronous mode,
 *	this function will block until there is an error or it is
 *	cancelled from another thread.  In asynchronous mode, this
 *	function returns immediately and the underlying network
 *	registration process will start.
 *  
 *  Mac OS X threading:
 *	Thread safe
 *  
 *  Parameters:
 *	
 *	theService:
 *	  The Network Service to register on the network.  Must be
 *	  non-NULL.
 *	
 *	options:
 *	  A set of bit options used to instruct the registration process.
 *	  Current supported option is kCFNetServiceFlagNoAutoRename.
 *	
 *	error:
 *	  A reference to an error struct which will be set to the error
 *	  and domain of the error should one occur.  If the value of
 *	  error is not desired, set to NULL.
 *  
 *  Result:
 *	Returns FALSE if domain, type, name or port is NULL.  In
 *	synchronous mode, it will always return FALSE as a result of the
 *	error or the cancellation.  In asynchronous mode, it will return
 *	TRUE if the registration process could start.
 *  
 *  Availability:
 *	Mac OS X:		 in version 10.4 and later in CoreServices.framework
 *	CarbonLib:		not available
 *	Non-Carbon CFM:   not available
 }
function CFNetServiceRegisterWithOptions( theService: CFNetServiceRef; options: CFOptionFlags; error: CFStreamErrorPtr { can be NULL } ): Boolean; external name '_CFNetServiceRegisterWithOptions';
(* __OSX_AVAILABLE_STARTING(__MAC_10_4,__IPHONE_2_0) *)


{
 *  CFNetServiceResolveWithTimeout()
 *  
 *  Discussion:
 *	Resolves the information related to this service.  It will
 *	resolve the target host, the addresses, and the first TXT record
 *	associated with the service.
 *  
 *  Mac OS X threading:
 *	Thread safe
 *  
 *  Parameters:
 *	
 *	theService:
 *	  The CFNetServiceRef which should be resolved. Must be non-NULL.
 *	  If this reference is not a valid CFNetServiceRef, the behavior
 *	  is undefined.
 *	
 *	timeout:
 *	  CFTimeInterval representing the maximum amount of time to take
 *	  to perform the resolve.  If the resolve can not be performed
 *	  within this timeout, the function or callback will recieve a
 *	  timeout error.  Values less than or equal to zero indicate an
 *	  infinite timeout.
 *	
 *	error:
 *	  A reference to a CFStreamError structure which will be filled
 *	  with any error information should an error occur.  May be set
 *	  to NULL if error information is not wanted.
 *  
 *  Result:
 *	Returns TRUE on success and FALSE on failure.  In asynchronous
 *	mode, this function will return immediately.  In synchronous
 *	mode, it will block until the resolve has completed or until the
 *	resolve is cancelled.
 *  
 *  Availability:
 *	Mac OS X:		 in version 10.4 and later in CoreServices.framework
 *	CarbonLib:		not available
 *	Non-Carbon CFM:   not available
 }
function CFNetServiceResolveWithTimeout( theService: CFNetServiceRef; timeout: CFTimeInterval; error: CFStreamErrorPtr { can be NULL } ): Boolean; external name '_CFNetServiceResolveWithTimeout';
(* __OSX_AVAILABLE_STARTING(__MAC_10_4,__IPHONE_2_0) *)


{
 *  CFNetServiceCancel()
 *  
 *  Discussion:
 *	Cancels an outstanding request for registration or resolution.
 *  
 *  Mac OS X threading:
 *	Thread safe
 *  
 *  Parameters:
 *	
 *	theService:
 *	  The Network Service which is active.  Must be non-NULL.
 *  
 *  Availability:
 *	Mac OS X:		 in version 10.2 and later in CoreServices.framework
 *	CarbonLib:		not available
 *	Non-Carbon CFM:   not available
 }
procedure CFNetServiceCancel( theService: CFNetServiceRef ); external name '_CFNetServiceCancel';
(* __OSX_AVAILABLE_STARTING(__MAC_10_2,__IPHONE_2_0) *)


{
 *  CFNetServiceGetTargetHost()
 *  
 *  Discussion:
 *	Query a Network Service for its resolve target.
 *  
 *  Mac OS X threading:
 *	Thread safe
 *	The function gets the data in a thread-safe manner, but the
 *	resulting data is not safe.  Since it is returned as a matter of
 *	a get opposed to a copy, the data is not safe if the service is
 *	being altered from another thread.
 *  
 *  Parameters:
 *	
 *	theService:
 *	  The Network Service to be queried.  Must be non-NULL.
 *  
 *  Result:
 *	Returns The target hostname of the machine providing the service,
 *	or NULL if the entity's target is not known (has not been
 *	resolved).
 *  
 *  Availability:
 *	Mac OS X:		 in version 10.4 and later in CoreServices.framework
 *	CarbonLib:		not available
 *	Non-Carbon CFM:   not available
 }
function CFNetServiceGetTargetHost( theService: CFNetServiceRef ): CFStringRef; external name '_CFNetServiceGetTargetHost';
(* __OSX_AVAILABLE_STARTING(__MAC_10_4,__IPHONE_2_0) *)


{
 *  CFNetServiceGetPortNumber()
 *  
 *  Discussion:
 *	Query a Network Service for its port number.
 *  
 *  Mac OS X threading:
 *	Thread safe
 *	The function gets the data in a thread-safe manner.
 *  
 *  Parameters:
 *	
 *	theService:
 *	  The Network Service to be queried.  Must be non-NULL.
 *  
 *  Result:
 *	Returns a SInt32 containing the port number in host byte order.
 *	Returns -1 if the entity's port is not known (has not been
 *	resolved)
 *  
 *  Availability:
 *	Mac OS X:		 in version 10.5 and later in CoreServices.framework
 *	CarbonLib:		not available
 *	Non-Carbon CFM:   not available
 }
function CFNetServiceGetPortNumber( theService: CFNetServiceRef ): SInt32; external name '_CFNetServiceGetPortNumber';
(* __OSX_AVAILABLE_STARTING(__MAC_10_5,__IPHONE_2_0) *)


{
 *  CFNetServiceGetAddressing()
 *  
 *  Discussion:
 *	Query a Network Service for its addressing.
 *  
 *  Mac OS X threading:
 *	Thread safe
 *	The function gets the data in a thread-safe manner, but the
 *	resulting data is not safe.  Since it is returned as a matter of
 *	a get opposed to a copy, the data is not safe if the service is
 *	being altered from another thread.
 *  
 *  Parameters:
 *	
 *	theService:
 *	  The Network Service to be queried.  Must be non-NULL.
 *  
 *  Result:
 *	Returns NULL if the entity's addressing is not known (has not
 *	been resolved).  The array will contain a CFDataRef for each
 *	address resolved.  Each CFDataRef contains a struct sockaddr
 *	representing the address of the service.
 *  
 *  Availability:
 *	Mac OS X:		 in version 10.2 and later in CoreServices.framework
 *	CarbonLib:		not available
 *	Non-Carbon CFM:   not available
 }
function CFNetServiceGetAddressing( theService: CFNetServiceRef ): CFArrayRef; external name '_CFNetServiceGetAddressing';
(* __OSX_AVAILABLE_STARTING(__MAC_10_2,__IPHONE_2_0) *)


{
 *  CFNetServiceGetTXTData()
 *  
 *  Discussion:
 *	Query a Network Service for its TXT record contents.
 *  
 *  Mac OS X threading:
 *	Thread safe
 *	The function gets the data in a thread-safe manner, but the
 *	resulting data is not safe.  Since it is returned as a matter of
 *	a get opposed to a copy, the data is not safe if the service is
 *	being altered from another thread.
 *  
 *  Parameters:
 *	
 *	theService:
 *	  The Network Service to be queried.  Must be non-NULL.
 *  
 *  Result:
 *	Returns NULL if the entity's TXT is not known (has not been
 *	resolved).  The result will contain the contents of the TXT
 *	record.  This is suitable to pass to
 *	CFNetServiceCreateDictionaryWithTXTData.
 *  
 *  Availability:
 *	Mac OS X:		 in version 10.4 and later in CoreServices.framework
 *	CarbonLib:		not available
 *	Non-Carbon CFM:   not available
 }
function CFNetServiceGetTXTData( theService: CFNetServiceRef ): CFDataRef; external name '_CFNetServiceGetTXTData';
(* __OSX_AVAILABLE_STARTING(__MAC_10_4,__IPHONE_2_0) *)


{
 *  CFNetServiceSetTXTData()
 *  
 *  Discussion:
 *	Sets the TXT record for the service.  If the service is currently
 *	registered on the network, the record will be broadcast.  Setting
 *	the TXT record on a resolving service is not allowed.
 *  
 *  Mac OS X threading:
 *	Thread safe
 *  
 *  Parameters:
 *	
 *	theService:
 *	  The Network Service to receive the new record.  Must be
 *	  non-NULL.
 *	
 *	txtRecord:
 *	  The contents of the TXT record.  This should not exceed a
 *	  length of 1450 bytes.
 *  
 *  Availability:
 *	Mac OS X:		 in version 10.4 and later in CoreServices.framework
 *	CarbonLib:		not available
 *	Non-Carbon CFM:   not available
 }
function CFNetServiceSetTXTData( theService: CFNetServiceRef; txtRecord: CFDataRef ): Boolean; external name '_CFNetServiceSetTXTData';
(* __OSX_AVAILABLE_STARTING(__MAC_10_4,__IPHONE_2_0) *)


{
 *  CFNetServiceCreateDictionaryWithTXTData()
 *  
 *  Discussion:
 *	Parses the given TXT record data into a set of key/value pairs as
 *	a CFDictionary where keys are CFStringRefs and values are
 *	CFDataRefs.  If the given record can not be parsed, NULL will be
 *	returned.  READ THE COMMENTS FOR
 *	CFNetServiceCreateTXTDataWithDictionary TO FULLY UNDERSTAND THE
 *	USE AND RESULTS OF THIS FUNCTION.
 *  
 *  Mac OS X threading:
 *	Thread safe
 *  
 *  Parameters:
 *	
 *	alloc:
 *	  CFAllocatorRef to be used for the creation of the result.
 *	
 *	txtRecord:
 *	  The TXT record data as returned by CFNetServiceGetInfo.
 *  
 *  Result:
 *	CFDictionaryRef containing the key/value pairs parsed from the
 *	record. It will return NULL if the record could not be parsed. 
 *	Keys in the dictionary are CFStringRef's.  Values are CFDataRef's.
 *  
 *  Availability:
 *	Mac OS X:		 in version 10.4 and later in CoreServices.framework
 *	CarbonLib:		not available
 *	Non-Carbon CFM:   not available
 }
function CFNetServiceCreateDictionaryWithTXTData( alloc: CFAllocatorRef; txtRecord: CFDataRef ): CFDictionaryRef; external name '_CFNetServiceCreateDictionaryWithTXTData';
(* __OSX_AVAILABLE_STARTING(__MAC_10_4,__IPHONE_2_0) *)


{
 *  CFNetServiceCreateTXTDataWithDictionary()
 *  
 *  Discussion:
 *	Flattens a set of key/value pairs into a CFDataRef suitable to
 *	pass into CFNetServiceSetTXTData.  This function will properly
 *	format the data for TXT record usage.  THIS IS NOT A GENERAL
 *	CFDictionaryRef FLATTENING ROUTINE.  CFDictionaryRef keys should
 *	be CFStringRef's and values should be CFDataRef's.  For
 *	convenience, values that are CFStringRef's will be converted to
 *	CFDataRef's representing the flattened UTF-8 bytes of the string.
 *	 The types of the values are not encoded in the CFDataRef's,
 *	therefore CFStringRef's will be flattened into CFDataRef's, and
 *	they will come out of CFNetServiceCreateDictionaryWithTXTData as
 *	CFDataRef's.
 *  
 *  Mac OS X threading:
 *	Thread safe
 *  
 *  Parameters:
 *	
 *	alloc:
 *	  CFAllocatorRef to be used for the creation of the result.
 *	
 *	keyValuePairs:
 *	  CFDictionaryRef containing keys and values to be placed into
 *	  the TXT record.  The keys must be CFStringRef's.  The values
 *	  should be CFDataRef's (CFStringRef's are permitted for
 *	  convenience).  Any other types will cause a failure.  The
 *	  length of a key and its value should not exceed 255.
 *  
 *  Result:
 *	CFDataRef containing the flattened form of the keys and values. 
 *	If the dictionary could not be flattend, NULL will be returned.
 *  
 *  Availability:
 *	Mac OS X:		 in version 10.4 and later in CoreServices.framework
 *	CarbonLib:		not available
 *	Non-Carbon CFM:   not available
 }
function CFNetServiceCreateTXTDataWithDictionary( alloc: CFAllocatorRef; keyValuePairs: CFDictionaryRef ): CFDataRef; external name '_CFNetServiceCreateTXTDataWithDictionary';
(* __OSX_AVAILABLE_STARTING(__MAC_10_4,__IPHONE_2_0) *)


{
 *  CFNetServiceSetClient()
 *  
 *  Discussion:
 *	Sets up the service to be used in asynchronous mode. 
 *	Notification of registration failure or resolution completion
 *	will occur through the given callback.  Once the client is set,
 *	the service must be scheduled on a runloop. The client callback
 *	will be triggered via one of the scheduled run loops; It is the
 *	caller's responsibility to ensure that at least one of the
 *	scheduled run loops is being run.  This call must be performed
 *	before calling CFNetServiceRegister or CFNetServiceResolve,
 *	otherwise it will return FALSE.  TRUE will be returned if the
 *	client could be set.
 *  
 *  Mac OS X threading:
 *	Thread safe
 *  
 *  Parameters:
 *	
 *	theService:
 *	  The service to set up for asynchronous mode.  Must be non-NULL.
 *	
 *	clientCB:
 *	  Function pointer will be called upon registration failure or
 *	  upon resolution completion.  In the case of resolution, this
 *	  callback may be called multiple times if there is more than one
 *	  address for a service.  Passing NULL will remove the client
 *	  from the entity and cancel any outstanding activity.
 *	
 *	clientContext:
 *	  Client contextual information to be used when calling clientCB.
 *	  Passing NULL will remove the client from the entity and cancel
 *	  any outstanding activity.
 *  
 *  Result:
 *	Returns FALSE if the client could not be set, TRUE otherwise.
 *  
 *  Availability:
 *	Mac OS X:		 in version 10.2 and later in CoreServices.framework
 *	CarbonLib:		not available
 *	Non-Carbon CFM:   not available
 }
function CFNetServiceSetClient( theService: CFNetServiceRef; clientCB: CFNetServiceClientCallBack { can be NULL }; clientContext: CFNetServiceClientContextPtr { can be NULL } ): Boolean; external name '_CFNetServiceSetClient';
(* __OSX_AVAILABLE_STARTING(__MAC_10_2,__IPHONE_2_0) *)


{
 *  CFNetServiceScheduleWithRunLoop()
 *  
 *  Discussion:
 *	Schedule the given service on the given run loop and mode.
 *  
 *  Mac OS X threading:
 *	Thread safe
 *  
 *  Parameters:
 *	
 *	theService:
 *	  The service which is set up for asynchronous mode. Must be
 *	  non-NULL.
 *	
 *	runLoop:
 *	  A reference to a runloop on which the service should be
 *	  scheduled. Must be non-NULL.
 *	
 *	runLoopMode:
 *	  The mode on which to schedule the service.  Must be non-NULL.
 *  
 *  Availability:
 *	Mac OS X:		 in version 10.2 and later in CoreServices.framework
 *	CarbonLib:		not available
 *	Non-Carbon CFM:   not available
 }
procedure CFNetServiceScheduleWithRunLoop( theService: CFNetServiceRef; runLoop: CFRunLoopRef; runLoopMode: CFStringRef ); external name '_CFNetServiceScheduleWithRunLoop';
(* __OSX_AVAILABLE_STARTING(__MAC_10_2,__IPHONE_2_0) *)


{
 *  CFNetServiceUnscheduleFromRunLoop()
 *  
 *  Discussion:
 *	Unschedule the given service from the given run loop and mode.
 *  
 *  Mac OS X threading:
 *	Thread safe
 *  
 *  Parameters:
 *	
 *	theService:
 *	  The service which is set up for asynchronous mode.  Must be
 *	  non-NULL.
 *	
 *	runLoop:
 *	  A reference to a runloop from which the service should be
 *	  unscheduled.  Must be non-NULL.
 *	
 *	runLoopMode:
 *	  The mode from which to unschedule the service.  Must be
 *	  non-NULL.
 *  
 *  Availability:
 *	Mac OS X:		 in version 10.2 and later in CoreServices.framework
 *	CarbonLib:		not available
 *	Non-Carbon CFM:   not available
 }
procedure CFNetServiceUnscheduleFromRunLoop( theService: CFNetServiceRef; runLoop: CFRunLoopRef; runLoopMode: CFStringRef ); external name '_CFNetServiceUnscheduleFromRunLoop';
(* __OSX_AVAILABLE_STARTING(__MAC_10_2,__IPHONE_2_0) *)


{
 *  CFNetServiceMonitorCreate()
 *  
 *  Discussion:
 *	Creates an instance of an object suitable for watching for
 *	CFNetService record changes on the network.
 *  
 *  Mac OS X threading:
 *	Thread safe
 *  
 *  Parameters:
 *	
 *	alloc:
 *	  The CFAllocator which should be used to allocate memory for the
 *	  monitor and its storage for values. If this reference is not a
 *	  valid CFAllocator, the behavior is undefined.
 *	
 *	theService:
 *	  The CFNetService to be monitored for record changes.
 *	
 *	clientCB:
 *	  Function pointer that will be called as record changes occur. 
 *	  Must be non-NULL.
 *	
 *	clientContext:
 *	  Client contextual information to be used when calling clientCB.
 *	   Must be non-NULL.
 *  
 *  Result:
 *	Returns a new instance of a CFNetServiceMonitor, or NULL if the
 *	object could not be created.
 *  
 *  Availability:
 *	Mac OS X:		 in version 10.4 and later in CoreServices.framework
 *	CarbonLib:		not available
 *	Non-Carbon CFM:   not available
 }
function CFNetServiceMonitorCreate( alloc: CFAllocatorRef; theService: CFNetServiceRef; clientCB: CFNetServiceMonitorClientCallBack; var clientContext: CFNetServiceClientContext ): CFNetServiceMonitorRef; external name '_CFNetServiceMonitorCreate';
(* __OSX_AVAILABLE_STARTING(__MAC_10_4,__IPHONE_2_0) *)


{
 *  CFNetServiceMonitorInvalidate()
 *  
 *  Discussion:
 *	Invalidates the given monitor object so that it may no longer be
 *	scheduled and callback never be called.  This will also stop any
 *	monitors currently in progress.
 *  
 *  Mac OS X threading:
 *	Thread safe
 *  
 *  Parameters:
 *	
 *	monitor:
 *	  CFNetServiceMonitor to invalidate.  Must be non-NULL.
 *  
 *  Availability:
 *	Mac OS X:		 in version 10.4 and later in CoreServices.framework
 *	CarbonLib:		not available
 *	Non-Carbon CFM:   not available
 }
procedure CFNetServiceMonitorInvalidate( monitor: CFNetServiceMonitorRef ); external name '_CFNetServiceMonitorInvalidate';
(* __OSX_AVAILABLE_STARTING(__MAC_10_4,__IPHONE_2_0) *)


{
 *  CFNetServiceMonitorStart()
 *  
 *  Discussion:
 *	Starts monitoring for record changes on a service.  It watches
 *	for changes of the given record type.  If there is already an
 *	outstanding monitor, it will return FALSE.  In synchronous mode,
 *	this call blocks until the monitor is stopped. It will return
 *	FALSE if there is an error performing the monitor or if there is
 *	some other error.  It will return TRUE otherwise.  In
 *	asynchronous mode, this call will return TRUE or FALSE depending
 *	if the underlying network query could be instantiated.
 *  
 *  Mac OS X threading:
 *	Thread safe
 *  
 *  Parameters:
 *	
 *	monitor:
 *	  CFNetServiceMonitor to perform the watch.
 *	
 *	recordType:
 *	  CFNetServiceMonitorType indicating the record type to watch.
 *	
 *	error:
 *	  A reference to an error struct which will be set to the error
 *	  and domain of the error should one occur.  If the value of
 *	  error is not desired, set to NULL.
 *  
 *  Result:
 *	Returns FALSE if an error occurs during a synchronous monitor or
 *	if the monitor could not start.  It returns TRUE otherwise.
 *  
 *  Availability:
 *	Mac OS X:		 in version 10.4 and later in CoreServices.framework
 *	CarbonLib:		not available
 *	Non-Carbon CFM:   not available
 }
function CFNetServiceMonitorStart( monitor: CFNetServiceMonitorRef; recordType: CFNetServiceMonitorType; error: CFStreamErrorPtr { can be NULL } ): Boolean; external name '_CFNetServiceMonitorStart';
(* __OSX_AVAILABLE_STARTING(__MAC_10_4,__IPHONE_2_0) *)


{
 *  CFNetServiceMonitorStop()
 *  
 *  Discussion:
 *	Stops an outstanding monitor.
 *  
 *  Mac OS X threading:
 *	Thread safe
 *  
 *  Parameters:
 *	
 *	monitor:
 *	  CFNetServiceMonitor with an active monitor.  Must be non-NULL.
 *	
 *	error:
 *	  Error value to be returned in "error" in
 *	  CFNetServiceMonitorStart if monitor is being performed in
 *	  synchronous mode.  In this case, a non-zero of the error field
 *	  of the struct will cause CFNetServiceMonitorStart to return
 *	  FALSE.  In asynchronous mode, the client call back will be
 *	  called with this error.
 *  
 *  Availability:
 *	Mac OS X:		 in version 10.4 and later in CoreServices.framework
 *	CarbonLib:		not available
 *	Non-Carbon CFM:   not available
 }
procedure CFNetServiceMonitorStop( monitor: CFNetServiceMonitorRef; error: CFStreamErrorPtr { can be NULL } ); external name '_CFNetServiceMonitorStop';
(* __OSX_AVAILABLE_STARTING(__MAC_10_4,__IPHONE_2_0) *)


{
 *  CFNetServiceMonitorScheduleWithRunLoop()
 *  
 *  Discussion:
 *	Schedules the monitor on a run loop and mode.  Use this to place
 *	the given monitor into asynchronous mode.  The client callback
 *	will be triggered via one of the scheduled run loops; It is the
 *	caller's responsibility to ensure that at least one of the
 *	scheduled run loops is being run.
 *  
 *  Mac OS X threading:
 *	Thread safe
 *  
 *  Parameters:
 *	
 *	monitor:
 *	  CFNetServiceMonitor to schedule.  Must be non-NULL.
 *	
 *	runLoop:
 *	  A reference to a runloop on which the monitor should be
 *	  scheduled.  Must be non-NULL.
 *	
 *	runLoopMode:
 *	  The mode on which to schedule the monitor.  Must be non-NULL.
 *  
 *  Availability:
 *	Mac OS X:		 in version 10.4 and later in CoreServices.framework
 *	CarbonLib:		not available
 *	Non-Carbon CFM:   not available
 }
procedure CFNetServiceMonitorScheduleWithRunLoop( monitor: CFNetServiceMonitorRef; runLoop: CFRunLoopRef; runLoopMode: CFStringRef ); external name '_CFNetServiceMonitorScheduleWithRunLoop';
(* __OSX_AVAILABLE_STARTING(__MAC_10_4,__IPHONE_2_0) *)


{
 *  CFNetServiceMonitorUnscheduleFromRunLoop()
 *  
 *  Discussion:
 *	Unschedules the browser from a run loop and mode.
 *  
 *  Mac OS X threading:
 *	Thread safe
 *  
 *  Parameters:
 *	
 *	monitor:
 *	  CFNetServiceMonitor to unschedule.  Must be non-NULL.
 *	
 *	runLoop:
 *	  A reference to a runloop from which the monitor should be
 *	  unscheduled. Must be non-NULL.
 *	
 *	runLoopMode:
 *	  The mode from which to unschedule the monitor.  Must be
 *	  non-NULL.
 *  
 *  Availability:
 *	Mac OS X:		 in version 10.4 and later in CoreServices.framework
 *	CarbonLib:		not available
 *	Non-Carbon CFM:   not available
 }
procedure CFNetServiceMonitorUnscheduleFromRunLoop( monitor: CFNetServiceMonitorRef; runLoop: CFRunLoopRef; runLoopMode: CFStringRef ); external name '_CFNetServiceMonitorUnscheduleFromRunLoop';
(* __OSX_AVAILABLE_STARTING(__MAC_10_4,__IPHONE_2_0) *)


{
 *  CFNetServiceBrowserCreate()
 *  
 *  Discussion:
 *	Creates an instance of a browser object.
 *  
 *  Mac OS X threading:
 *	Thread safe
 *  
 *  Parameters:
 *	
 *	alloc:
 *	  The CFAllocator which should be used to allocate memory for the
 *	  browser and its storage for values. If this reference is not a
 *	  valid CFAllocator, the behavior is undefined.
 *	
 *	clientCB:
 *	  Function pointer that will be called as domains or services are
 *	  found on the network.  Must be non-NULL.
 *	
 *	clientContext:
 *	  Client contextual information to be used when calling clientCB.
 *	  Must be non-NULL.
 *  
 *  Result:
 *	Returns a new instance of a browser, or NULL if the instance
 *	could not be created.
 *  
 *  Availability:
 *	Mac OS X:		 in version 10.2 and later in CoreServices.framework
 *	CarbonLib:		not available
 *	Non-Carbon CFM:   not available
 }
function CFNetServiceBrowserCreate( alloc: CFAllocatorRef; clientCB: CFNetServiceBrowserClientCallBack; var clientContext: CFNetServiceClientContext ): CFNetServiceBrowserRef; external name '_CFNetServiceBrowserCreate';
(* __OSX_AVAILABLE_STARTING(__MAC_10_2,__IPHONE_2_0) *)


{
 *  CFNetServiceBrowserInvalidate()
 *  
 *  Discussion:
 *	Invalidates the given browser object so that it may no longer be
 *	scheduled and callback never be called.  This will also stop any
 *	searches currently in progress.
 *  
 *  Parameters:
 *	
 *	browser:
 *	  Network Service Browser to invalidate.  Must be non-NULL.
 *  
 *  Availability:
 *	Mac OS X:		 in version 10.2 and later in CoreServices.framework
 *	CarbonLib:		not available
 *	Non-Carbon CFM:   not available
 }
procedure CFNetServiceBrowserInvalidate( browser: CFNetServiceBrowserRef ); external name '_CFNetServiceBrowserInvalidate';
(* __OSX_AVAILABLE_STARTING(__MAC_10_2,__IPHONE_2_0) *)


{
 *  CFNetServiceBrowserSearchForDomains()
 *  
 *  Discussion:
 *	Starts a search for domains.  The browser will either try to find
 *	"Browse" domains or will search for "Registration" domains.  If
 *	there is already an outstanding search, it will return FALSE.  In
 *	syncronous mode, this call blocks until the search is stopped. 
 *	It will return FALSE if there is an error performing the search.
 *	It will return TRUE otherwise.  In asynchronous mode, this call
 *	will return TRUE or FALSE depending if the underlying network
 *	search could be started.
 *  
 *  Mac OS X threading:
 *	Thread safe
 *  
 *  Parameters:
 *	
 *	browser:
 *	  Network Service Browser to perform the search.  Must be
 *	  non-NULL.
 *	
 *	registrationDomains:
 *	  FALSE if "Browse" domains are to be discovered. TRUE if only
 *	  "Registration" domains are to be discovered.
 *	
 *	error:
 *	  A reference to an error struct which will be set to the error
 *	  and domain of the error should one occur.  If the value of
 *	  error is not desired, set to NULL.
 *  
 *  Result:
 *	Returns FALSE if an error occurs during a synchronous search or
 *	if the search could not start.  It returns TRUE otherwise.
 *  
 *  Availability:
 *	Mac OS X:		 in version 10.2 and later in CoreServices.framework
 *	CarbonLib:		not available
 *	Non-Carbon CFM:   not available
 }
function CFNetServiceBrowserSearchForDomains( browser: CFNetServiceBrowserRef; registrationDomains: Boolean; error: CFStreamErrorPtr { can be NULL } ): Boolean; external name '_CFNetServiceBrowserSearchForDomains';
(* __OSX_AVAILABLE_STARTING(__MAC_10_2,__IPHONE_2_0) *)


{
 *  CFNetServiceBrowserSearchForServices()
 *  
 *  Discussion:
 *	Starts a search for a service type on the given domain.  If there
 *	is already an outstanding search, it will return FALSE.  In
 *	syncronous mode, this call blocks until the search is stopped. 
 *	It will return FALSE if there is an error performing the search
 *	or if there is some other error.  It will return TRUE otherwise.
 *	In asynchronous mode, this call will return TRUE or FALSE
 *	depending if the underlying network search could be instantiated.
 *  
 *  Mac OS X threading:
 *	Thread safe
 *  
 *  Parameters:
 *	
 *	browser:
 *	  Network Service Browser to perform the search.  Must be
 *	  non-NULL.
 *	
 *	domain:
 *	  Network domain to search in order to find the service.  Must be
 *	  non-NULL.
 *	
 *	serviceType:
 *	  Service type for which to search.  Must be non-NULL.
 *	
 *	error:
 *	  A reference to an error struct which will be set to the error
 *	  and domain of the error should one occur.  If the value of
 *	  error is not desired, set to NULL.
 *  
 *  Result:
 *	Returns FALSE if an error occurs during a synchronous search or
 *	if the search could not start.  It returns TRUE otherwise.
 *  
 *  Availability:
 *	Mac OS X:		 in version 10.2 and later in CoreServices.framework
 *	CarbonLib:		not available
 *	Non-Carbon CFM:   not available
 }
function CFNetServiceBrowserSearchForServices( browser: CFNetServiceBrowserRef; domain: CFStringRef; serviceType: CFStringRef; error: CFStreamErrorPtr { can be NULL } ): Boolean; external name '_CFNetServiceBrowserSearchForServices';
(* __OSX_AVAILABLE_STARTING(__MAC_10_2,__IPHONE_2_0) *)


{
 *  CFNetServiceBrowserStopSearch()
 *  
 *  Discussion:
 *	Stops an outstanding browser search.
 *  
 *  Mac OS X threading:
 *	Thread safe
 *  
 *  Parameters:
 *	
 *	browser:
 *	  Network Service Browser performing the search.  Must be
 *	  non-NULL.
 *	
 *	error:
 *	  Error value to be returned in "error" in
 *	  CFNetServiceBrowserStartServiceSearch if search is being
 *	  performed in synchronous mode.  In this case, a non-zero of the
 *	  error field of the struct will cause
 *	  CFNetServiceBrowserStartServiceSearch to return FALSE. In
 *	  asynchronous mode, the client call back will be called with
 *	  this error.
 *  
 *  Availability:
 *	Mac OS X:		 in version 10.2 and later in CoreServices.framework
 *	CarbonLib:		not available
 *	Non-Carbon CFM:   not available
 }
procedure CFNetServiceBrowserStopSearch( browser: CFNetServiceBrowserRef; error: CFStreamErrorPtr { can be NULL } ); external name '_CFNetServiceBrowserStopSearch';
(* __OSX_AVAILABLE_STARTING(__MAC_10_2,__IPHONE_2_0) *)


{
 *  CFNetServiceBrowserScheduleWithRunLoop()
 *  
 *  Discussion:
 *	Schedules the browser on a run loop and mode.  Use this to place
 *	the given browser into asynchronous mode.  The client callback
 *	will be triggered via one of the scheduled run loops; It is the
 *	caller's responsibility to ensure that at least one of the
 *	scheduled run loops is being run.
 *  
 *  Mac OS X threading:
 *	Thread safe
 *  
 *  Parameters:
 *	
 *	browser:
 *	  Network Service Browser to schedule.  Must be non-NULL.
 *	
 *	runLoop:
 *	  A reference to a runloop on which the browser should be
 *	  scheduled.  Must be non-NULL.
 *	
 *	runLoopMode:
 *	  The mode on which to schedule the browser.  Must be non-NULL.
 *  
 *  Availability:
 *	Mac OS X:		 in version 10.2 and later in CoreServices.framework
 *	CarbonLib:		not available
 *	Non-Carbon CFM:   not available
 }
procedure CFNetServiceBrowserScheduleWithRunLoop( browser: CFNetServiceBrowserRef; runLoop: CFRunLoopRef; runLoopMode: CFStringRef ); external name '_CFNetServiceBrowserScheduleWithRunLoop';
(* __OSX_AVAILABLE_STARTING(__MAC_10_2,__IPHONE_2_0) *)


{
 *  CFNetServiceBrowserUnscheduleFromRunLoop()
 *  
 *  Discussion:
 *	Unschedules the browser from a run loop and mode.
 *  
 *  Mac OS X threading:
 *	Thread safe
 *  
 *  Parameters:
 *	
 *	browser:
 *	  Network Service Browser to unschedule.  Must be non-NULL.
 *	
 *	runLoop:
 *	  A reference to a runloop from which the browser should be
 *	  unscheduled. Must be non-NULL.
 *	
 *	runLoopMode:
 *	  The mode from which to unschedule the browser.  Must be
 *	  non-NULL.
 *  
 *  Availability:
 *	Mac OS X:		 in version 10.2 and later in CoreServices.framework
 *	CarbonLib:		not available
 *	Non-Carbon CFM:   not available
 }
procedure CFNetServiceBrowserUnscheduleFromRunLoop( browser: CFNetServiceBrowserRef; runLoop: CFRunLoopRef; runLoopMode: CFStringRef ); external name '_CFNetServiceBrowserUnscheduleFromRunLoop';
(* __OSX_AVAILABLE_STARTING(__MAC_10_2,__IPHONE_2_0) *)

{$ifc TARGET_OS_MAC}
{
 *  CFNetServiceRegister()   *** DEPRECATED ***
 *  
 *  Discussion:
 *	Registers the entity on the network.  This requires that the
 *	service has a domain, a type, a name, and a port.  The service is
 *	registered on the network until this function returns or is
 *	cancelled by calling CFNetServiceCancel.  In synchronous mode,
 *	this function will block until there is an error or it is
 *	cancelled from another thread.  In asynchronous mode, this
 *	function returns immediately and the underlying network
 *	registration process will start. 
 *	
 *	As a result of new, better performing API's in Service Discovery,
 *	users should now call CFNetServiceRegisterWithOptions.  Using the
 *	new calls will allow your application to perform better on the
 *	network.
 *  
 *  Mac OS X threading:
 *	Thread safe
 *  
 *  Parameters:
 *	
 *	theService:
 *	  The Network Service to register on the network.  Must be
 *	  non-NULL.
 *	
 *	error:
 *	  A reference to an error struct which will be set to the error
 *	  and domain of the error should one occur.  If the value of
 *	  error is not desired, set to NULL.
 *  
 *  Result:
 *	Returns FALSE if domain, type, name or port is NULL.  In
 *	synchronous mode, it will always return FALSE as a result of the
 *	error or the cancellation.  In asynchronous mode, it will return
 *	TRUE if the registration process could start.
 *  
 *  Availability:
 *	Mac OS X:		 in version 10.2 and later in CoreServices.framework but deprecated in 10.4
 *	CarbonLib:		not available
 *	Non-Carbon CFM:   not available
 }
function CFNetServiceRegister( theService: CFNetServiceRef; error: CFStreamErrorPtr { can be NULL } ): Boolean; external name '_CFNetServiceRegister';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_2,__MAC_10_4,__IPHONE_NA,__IPHONE_NA) *)


{
 *  CFNetServiceResolve()   *** DEPRECATED ***
 *  
 *  Discussion:
 *	Resolves the addressing for the given service.  This requires
 *	that the service has a domain, a type, and a name.  The service
 *	is  resolved on the network until this function returns or is
 *	cancelled by calling CFNetServiceCancel. In synchronous mode,
 *	this function will block until there is an error or it is
 *	cancelled from another thread.  In asynchronous mode, this
 *	function returns immediately and the underlying network
 *	resolution process will start. 
 *	
 *	As a result of new, better performing API's in Service Discovery,
 *	users should now call CFNetServiceResolveWithTimeout.  If needing
 *	to monitor TXT record changes, users should use the new
 *	CFNetServiceMonitor object. Using the new calls will allow your
 *	application to perform better on the network.
 *  
 *  Mac OS X threading:
 *	Thread safe
 *  
 *  Parameters:
 *	
 *	theService:
 *	  The Network Service to resolve on the network.  Must be
 *	  non-NULL.
 *	
 *	error:
 *	  A reference to an error struct which will be set to the error
 *	  and domain of the error should one occur.  If the value of
 *	  error is not desired, set to NULL.
 *  
 *  Result:
 *	Returns FALSE if domain, type, or name is NULL.  In synchronous
 *	mode, it will return FALSE as a result of an error or a
 *	cancellation.  It will return TRUE if the resolution does
 *	succeed.  In asynchronous mode, it will return TRUE if the
 *	resolution process could start.
 *  
 *  Availability:
 *	Mac OS X:		 in version 10.2 and later in CoreServices.framework but deprecated in 10.4
 *	CarbonLib:		not available
 *	Non-Carbon CFM:   not available
 }
function CFNetServiceResolve( theService: CFNetServiceRef; error: CFStreamErrorPtr { can be NULL } ): Boolean; external name '_CFNetServiceResolve';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_2,__MAC_10_4,__IPHONE_NA,__IPHONE_NA) *)


{
 *  CFNetServiceGetProtocolSpecificInformation()   *** DEPRECATED ***
 *  
 *  Discussion:
 *	Query a Network Service for its protocol specific information.
 *	
 *	
 *	As a result of new, better performing API's in Service Discovery,
 *	users should now call CFNetServiceGetTXTData.  If needing to
 *	monitor TXT record changes, users should use the new
 *	CFNetServiceMonitor object. Using the new calls will allow your
 *	application to perform better on the network.
 *  
 *  Mac OS X threading:
 *	Thread safe
 *	The function gets the data in a thread-safe manner, but the
 *	resulting data is not safe.  Since it is returned as a matter of
 *	a get opposed to a copy, the data is not safe if the service is
 *	being altered from another thread.
 *  
 *  Parameters:
 *	
 *	theService:
 *	  The Network Service to be queried.  Must be non-NULL.
 *  
 *  Result:
 *	Returns NULL if a resolve has not been performed or if
 *	CFNetServiceSetProtocolSpecificInformation has not been called. 
 *	It will return a CFStringRef containing the specific information
 *	if there is some.
 *  
 *  Availability:
 *	Mac OS X:		 in version 10.2 and later in CoreServices.framework but deprecated in 10.4
 *	CarbonLib:		not available
 *	Non-Carbon CFM:   not available
 }
function CFNetServiceGetProtocolSpecificInformation( theService: CFNetServiceRef ): CFStringRef; external name '_CFNetServiceGetProtocolSpecificInformation';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_2,__MAC_10_4,__IPHONE_NA,__IPHONE_NA) *)


{
 *  CFNetServiceSetProtocolSpecificInformation()   *** DEPRECATED ***
 *  
 *  Discussion:
 *	Set a Network Service's protocol specific information. 
 *	
 *	As a result of new, better performing API's in Service Discovery,
 *	users should now call CFNetServiceSetTXTData.  Using the new
 *	calls will allow your application to perform better on the
 *	network.
 *  
 *  Mac OS X threading:
 *	Thread safe
 *  
 *  Parameters:
 *	
 *	theService:
 *	  The Network Service to be queried.  Must be non-NULL.
 *	
 *	theInfo:
 *	  The protocol specific information to be added.  Pass NULL to
 *	  remove the information from the service.
 *  
 *  Availability:
 *	Mac OS X:		 in version 10.2 and later in CoreServices.framework but deprecated in 10.4
 *	CarbonLib:		not available
 *	Non-Carbon CFM:   not available
 }
procedure CFNetServiceSetProtocolSpecificInformation( theService: CFNetServiceRef; theInfo: CFStringRef ); external name '_CFNetServiceSetProtocolSpecificInformation';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_2,__MAC_10_4,__IPHONE_NA,__IPHONE_NA) *)


{$endc} {TARGET_OS_MAC}
{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}

end.
{$endc} {not MACOSALLINCLUDE}
