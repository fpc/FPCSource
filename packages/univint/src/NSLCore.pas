{
     File:       NSLCore/NSLCore.h
 
     Contains:   Interface to API for using the NSL Manager
 
     Version:    NSLCore-145~102
 
     Copyright:  © 2000-2008 by Apple Computer, Inc., all rights reserved
 
     Bugs?:      For bug reports, consult the following page on
                 the World Wide Web:
 
                     http://www.freepascal.org/bugs.html
 
}
{      Pascal Translation Updated:  Jonas Maebe, <jonas@freepascal.org>, October 2009 }
{
    Modified for use with Free Pascal
    Version 308
    Please report any bugs to <gpc@microbizz.nl>
}

{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}
{$mode macpas}
{$packenum 1}
{$macro on}
{$inline on}
{$calling mwpascal}

unit NSLCore;
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
	{$setc TARGET_OS_MAC := TRUE}
	{$setc TARGET_OS_IPHONE := FALSE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
{$elifc defined __ppc64__ and __ppc64__}
	{$setc TARGET_CPU_PPC := TFALSE}
	{$setc TARGET_CPU_PPC64 := TRUE}
	{$setc TARGET_CPU_X86 := FALSE}
	{$setc TARGET_CPU_X86_64 := FALSE}
	{$setc TARGET_CPU_ARM := FALSE}
	{$setc TARGET_OS_MAC := TRUE}
	{$setc TARGET_OS_IPHONE := FALSE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
{$elifc defined __i386__ and __i386__}
	{$setc TARGET_CPU_PPC := FALSE}
	{$setc TARGET_CPU_PPC64 := FALSE}
	{$setc TARGET_CPU_X86 := TRUE}
	{$setc TARGET_CPU_X86_64 := FALSE}
	{$setc TARGET_CPU_ARM := FALSE}
{$ifc defined(iphonesim)}
 	{$setc TARGET_OS_MAC := FALSE}
	{$setc TARGET_OS_IPHONE := TRUE}
	{$setc TARGET_IPHONE_SIMULATOR := TRUE}
{$elsec}
	{$setc TARGET_OS_MAC := TRUE}
	{$setc TARGET_OS_IPHONE := FALSE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
{$endc}
{$elifc defined __x86_64__ and __x86_64__}
	{$setc TARGET_CPU_PPC := FALSE}
	{$setc TARGET_CPU_PPC64 := FALSE}
	{$setc TARGET_CPU_X86 := FALSE}
	{$setc TARGET_CPU_X86_64 := TRUE}
	{$setc TARGET_CPU_ARM := FALSE}
	{$setc TARGET_OS_MAC := TRUE}
	{$setc TARGET_OS_IPHONE := FALSE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
{$elifc defined __arm__ and __arm__}
	{$setc TARGET_CPU_PPC := FALSE}
	{$setc TARGET_CPU_PPC64 := FALSE}
	{$setc TARGET_CPU_X86 := FALSE}
	{$setc TARGET_CPU_X86_64 := FALSE}
	{$setc TARGET_CPU_ARM := TRUE}
	{ will require compiler define when/if other Apple devices with ARM cpus ship }
	{$setc TARGET_OS_MAC := FALSE}
	{$setc TARGET_OS_IPHONE := TRUE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
{$elsec}
	{$error __ppc__ nor __ppc64__ nor __i386__ nor __x86_64__ nor __arm__ is defined.}
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
uses MacTypes,MacErrors,CodeFragments,Files,Threads;
{$endc} {not MACOSALLINCLUDE}


{$ifc TARGET_OS_MAC}

{$ALIGN MAC68K}


const
	kNSLMinSystemVersion = $0900; { equivalent to 9.0}
	kNSLMinOTVersion = $0130; { equivalent to 1.3}

const
	kNSLDefaultListSize = 256;   { default list size for service and protocol lists}

const
	kNSLURLDelimiter = 44;   { delimits URL's within memory buffers}


const
	kNSLNoContext = 0;     { the default context for NSLError structs}

type
	NSLError = record
		theErr: OSStatus;
		theContext: UInt32;
	end;
	NSLErrorPtr = ^NSLError;
const
{ Constants to use with NSLPrepareRequest}
                                        { kNSLDuplicateSearchInProgress is not strictly an error.  The client is free to ignore}
                                        { this result, and nothing bad will happen if it does.  It is}
                                        { informational only.}
	kNSLDuplicateSearchInProgress = 100;
	kNSLUserCanceled = userCanceledErr; { User hit cancel from the NSLStandardGetURL dialog }
                                        { Invalid enumeratorRef  }
	kNSLInvalidEnumeratorRef = 0;     { this is not an error; it is the equiv to a NULL ptr}

type
	NSLSearchState = UInt16;
const
{ State codes for notifiers.}
	kNSLSearchStateBufferFull = 1;
	kNSLSearchStateOnGoing = 2;
	kNSLSearchStateComplete = 3;
	kNSLSearchStateStalled = 4;
	kNSLWaitingForContinue = 5;

type
	NSLEventCode = UInt32;
const
{ Event codes}
	kNSLServicesLookupDataEvent = 6;
	kNSLNeighborhoodLookupDataEvent = 7;
	kNSLNewDataEvent = 8;
	kNSLContinueLookupEvent = 9;


type
	NSLClientRef = UInt32;
	NSLRequestRef = UInt32;
	NSLOneBasedIndex = UInt32;
	NSLPath = CStringPtr;
	NSLServiceType = CStringPtr;
	NSLServicesList = Handle;
	NSLNeighborhood = UInt8Ptr;
{
   cstring which is a comma delimited list of protocols which can be used to
   create a NSLProtocolList internally
}

{ the async information block for client<->manager interaction}
type
	NSLClientAsyncInfo = record
		clientContextPtr: UnivPtr;       { set by the client for its own use}
		mgrContextPtr: UnivPtr;          { set by NSL mgr; ptr to request object ptr}
		resultBuffer: CStringPtr;
		bufferLen: SIGNEDLONG;
		maxBufferSize: SIGNEDLONG;
		startTime: UInt32;              { when the search starts, to use with maxSearchTime to determine time-out condition}
		intStartTime: UInt32;           { used with alertInterval}
		maxSearchTime: UInt32;          { total time for search, in ticks (0 == no time limit)}
		alertInterval: UInt32;          { call client's notifier or return, every this many ticks ( 0 == don't use this param)}
		totalItems: UInt32;             { total number of tuples currently in buffer}
		alertThreshold: UInt32;         { call client's notifier or return, every this many items found ( 0 == don't use this param)}
		searchState: NSLSearchState;
		searchResult: NSLError;
		searchDataType: NSLEventCode;         { this is a data type code which allows the client's asyncNotifier to properly}
                                              { handle the data in resultBuffer.}
	end;
	NSLClientAsyncInfoPtr = ^NSLClientAsyncInfo;

{ the async information block plugin<->manager interaction}
type
	NSLPluginAsyncInfo = record
		mgrContextPtr: UnivPtr;          { set by NSL mgr; ptr to request object ptr}
		pluginContextPtr: UnivPtr;       { set/used by individual plugins}
		pluginPtr: UnivPtr;              { ptr to the plugin object waiting for continue lookup call}
		resultBuffer: CStringPtr;           { set by plugin to point at data}
		bufferLen: SIGNEDLONG;
		maxBufferSize: SIGNEDLONG;
		maxSearchTime: UInt32;          { total time for search, in ticks (0 == no time limit)}
		reserved1: UInt32;
		reserved2: UInt32;
		reserved3: UInt32;
		clientRef: NSLClientRef;
		requestRef: NSLRequestRef;
		searchState: NSLSearchState;
		searchResult: OSStatus;
	end;
	NSLPluginAsyncInfoPtr = ^NSLPluginAsyncInfo;

{ the manager asynchronous notifier routine.}
type
	NSLMgrNotifyProcPtr = procedure( var thePluginAsyncInfo: NSLPluginAsyncInfo );

{ the client asynchronous notifier routine.}
type
	NSLClientNotifyProcPtr = procedure( var theClientAsyncInfo: NSLClientAsyncInfo );
	NSLMgrNotifyUPP = NSLMgrNotifyProcPtr;
	NSLClientNotifyUPP = NSLClientNotifyProcPtr;
{
 *  NewNSLMgrNotifyUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function NewNSLMgrNotifyUPP( userRoutine: NSLMgrNotifyProcPtr ): NSLMgrNotifyUPP; external name '_NewNSLMgrNotifyUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)

{
 *  NewNSLClientNotifyUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function NewNSLClientNotifyUPP( userRoutine: NSLClientNotifyProcPtr ): NSLClientNotifyUPP; external name '_NewNSLClientNotifyUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)

{
 *  DisposeNSLMgrNotifyUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure DisposeNSLMgrNotifyUPP( userUPP: NSLMgrNotifyUPP ); external name '_DisposeNSLMgrNotifyUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)

{
 *  DisposeNSLClientNotifyUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure DisposeNSLClientNotifyUPP( userUPP: NSLClientNotifyUPP ); external name '_DisposeNSLClientNotifyUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)

{
 *  InvokeNSLMgrNotifyUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure InvokeNSLMgrNotifyUPP( var thePluginAsyncInfo: NSLPluginAsyncInfo; userUPP: NSLMgrNotifyUPP ); external name '_InvokeNSLMgrNotifyUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)

{
 *  InvokeNSLClientNotifyUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure InvokeNSLClientNotifyUPP( var theClientAsyncInfo: NSLClientAsyncInfo; userUPP: NSLClientNotifyUPP ); external name '_InvokeNSLClientNotifyUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
   this struct is a format for dealing with our internal data representation.  Typed data will be contiguous chunk of
   memory, with the first 4 bytes being a data "descriptor".
}
type
	NSLTypedData = record
		dataType: UNSIGNEDLONG;
		lengthOfData: UNSIGNEDLONG;
{  void*                           theData; }
	end;
	NSLTypedDataPtr = ^NSLTypedData;

const
  kNSLDataType = FourCharCode('NSL_');
{
   This is just a header at the beginning of a handle that stores our list of service types.
   Each service type is a pascal string, so each service type starts after the end of the
   previous one.
}
type
	NSLServicesListHeader = record
		numServices: UNSIGNEDLONG;
		logicalLen: UNSIGNEDLONG;             { length of all usable data in handle}
{  Ptr                             firstService; }
	end;
	NSLServicesListHeaderPtr = ^NSLServicesListHeader;

{ some defs for common protocols}

const
	kSLPProtocolType = 'SLP';
const
	kDNSProtocolType = 'DNS';
const
	kLDAPProtocolType = 'LDAP';
const
	kNBPProtocolType = 'NBP';
const
	kNSLDirectoryServiceProtocolType = 'DirService';

{
   general information from a plug-in.  Includes supported protocols, data types and services,
   as well as an info/comment string describing the function of the plug-in in human-readable
   form.  The offsets point to the beginning of each list of data returned, and the protocol
   data offset is the startOfData member of the struct
}
type
	NSLPluginData = record
		reserved1: SIGNEDLONG;
		reserved2: SIGNEDLONG;
		reserved3: SIGNEDLONG;
		supportsRegistration: Boolean;
		isPurgeable: Boolean;
		totalLen: UInt16;               { length of everything, including header}
		dataTypeOffset: UInt16;
		serviceListOffset: UInt16;
		protocolListOffset: UInt16;
		commentStringOffset: UInt16;
{  char*                           startOfData; }
                                              { protocol data is first on the list}
	end;
	NSLPluginDataPtr = ^NSLPluginData;

{
  -----------------------------------------------------------------------------
    Finding out if the library is present and getting its version
  -----------------------------------------------------------------------------
}

{$ifc not TARGET_CPU_64}
{
 *  NSLLibraryVersion()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in NSLPPCLib 1.1 and later
 }
function NSLLibraryVersion: UInt32; external name '_NSLLibraryVersion';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{$endc} {not TARGET_CPU_64}


// #define NSLLibraryPresent()        true
   

{
  -----------------------------------------------------------------------------
    High level API calls: the following two calls are ALL an application needs
    to register/deregister its service.
    If you use these, you don't need to make any of the other calls to NSLAPI 
    (including NSLOpenNavigationAPI) 
  -----------------------------------------------------------------------------
}

{ <--- error code from registration }
{ ---> urlToRegister is a null terminated url that has only legal characters defined for URLs.  Use HexEncodeText to encode}
{          portions of the url that have illegal characters }
{ ---> neighborhoodToRegisterIn is an optional parameter for explicitly defining a neighborhood to register in.
            If parameter is NULL, then the plugins will determine where to register the service }
{$ifc not TARGET_CPU_64}
{
 *  NSLStandardRegisterURL()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in NSLPPCLib 1.1 and later
 }
function NSLStandardRegisterURL( urlToRegister: NSLPath; neighborhoodToRegisterIn: NSLNeighborhood { can be NULL } ): NSLError; external name '_NSLStandardRegisterURL';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{ <--- error code from registration }
{ ---> urlToRegister is a null terminated url that has only legal characters defined for URLs.  Use HexEncodeText to encode}
{          portions of the url that have illegal characters }
{ ---> neighborhoodToDeregisterIn is an optional parameter for explicitly defining a neighborhood to register in.
            If parameter is NULL, then the plugins will determine where to register the service }
{
 *  NSLStandardDeregisterURL()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in NSLPPCLib 1.1 and later
 }
function NSLStandardDeregisterURL( urlToDeregister: NSLPath; neighborhoodToDeregisterIn: NSLNeighborhood { can be NULL } ): NSLError; external name '_NSLStandardDeregisterURL';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{-----------------------------------------------------------------------------}

{
 *  NSLHexEncodeText()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in NSLPPCLib 1.1 and later
 }
function NSLHexEncodeText( rawText: ConstCStringPtr; rawTextLen: UInt16; newTextBuffer: CStringPtr; var newTextBufferLen: UInt16; var textChanged: Boolean ): OSStatus; external name '_NSLHexEncodeText';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
 *  NSLHexDecodeText()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in NSLPPCLib 1.1 and later
 }
function NSLHexDecodeText( encodedText: ConstCStringPtr; encodedTextLen: UInt16; decodedTextBuffer: CStringPtr; var decodedTextBufferLen: UInt16; var textChanged: Boolean ): OSStatus; external name '_NSLHexDecodeText';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
  -----------------------------------------------------------------------------
    Basic API Utility calls: sufficient to create, and parse data structures
  -----------------------------------------------------------------------------
}

{
 *  NSLMakeNewServicesList()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in NSLPPCLib 1.0 and later
 }
function NSLMakeNewServicesList( initialServiceList: ConstCStringPtr ): NSLServicesList; external name '_NSLMakeNewServicesList';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
 *  NSLAddServiceToServicesList()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in NSLPPCLib 1.0 and later
 }
function NSLAddServiceToServicesList( serviceList: NSLServicesList; serviceType: NSLServiceType ): NSLError; external name '_NSLAddServiceToServicesList';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
 *  NSLDisposeServicesList()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in NSLPPCLib 1.0 and later
 }
procedure NSLDisposeServicesList( theList: NSLServicesList ); external name '_NSLDisposeServicesList';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
    The name reflects the name of the Neighborhood, i.e. "apple.com." or "AppleTalk Zone One".
    The protocolList is a comma delimited list of protocols that the Neighborhood might exist in.
    If the user passes in NULL, then all protocols will be queried.  The result must be disposed
    of by the user by calling NSLFreeNeighborhood.
}
{
 *  NSLMakeNewNeighborhood()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in NSLPPCLib 1.0 and later
 }
function NSLMakeNewNeighborhood( name: ConstCStringPtr; protocolList: ConstCStringPtr { can be NULL } ): NSLNeighborhood; external name '_NSLMakeNewNeighborhood';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{ creates an exact copy of an existing neighborhood }
{
 *  NSLCopyNeighborhood()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in NSLPPCLib 1.1 and later
 }
function NSLCopyNeighborhood( neighborhood: NSLNeighborhood ): NSLNeighborhood; external name '_NSLCopyNeighborhood';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
 *  NSLFreeNeighborhood()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in NSLPPCLib 1.0 and later
 }
function NSLFreeNeighborhood( neighborhood: NSLNeighborhood ): NSLNeighborhood; external name '_NSLFreeNeighborhood';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
 *  NSLGetNameFromNeighborhood()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in NSLPPCLib 1.0 and later
 }
procedure NSLGetNameFromNeighborhood( neighborhood: NSLNeighborhood; var name: CStringPtr; var length: SIGNEDLONG ); external name '_NSLGetNameFromNeighborhood';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
   create a block of formatted data, pointed to by newDataPtr.  This will be used
   in calls (typically request-related calls) for plug-ins that handle the NSL data type.
}
{
 *  NSLMakeServicesRequestPB()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in NSLPPCLib 1.0 and later
 }
function NSLMakeServicesRequestPB( serviceList: NSLServicesList; var newDataPtr: NSLTypedDataPtr ): OSStatus; external name '_NSLMakeServicesRequestPB';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{ releases any storage created with MakeXXXPB calls, associated with TypedData.}
{
 *  NSLFreeTypedDataPtr()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in NSLPPCLib 1.0 and later
 }
function NSLFreeTypedDataPtr( nslTypeData: NSLTypedDataPtr ): NSLTypedDataPtr; external name '_NSLFreeTypedDataPtr';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
   utility function that returns whether a url was found, a pointer to the beginning
   of the url, and the length of the URL.
}
{
 *  NSLGetNextUrl()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in NSLPPCLib 1.0 and later
 }
function NSLGetNextUrl( infoPtr: NSLClientAsyncInfoPtr; var urlPtr: CStringPtr; var urlLength: SIGNEDLONG ): Boolean; external name '_NSLGetNextUrl';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
   utility function that returns whether a Neighborhood was found, a pointer to the beginning
   of the Neighborhood, and the length of the Neighborhood.
}
{
 *  NSLGetNextNeighborhood()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in NSLPPCLib 1.0 and later
 }
function NSLGetNextNeighborhood( infoPtr: NSLClientAsyncInfoPtr; var neighborhood: NSLNeighborhood; var neighborhoodLength: SIGNEDLONG ): Boolean; external name '_NSLGetNextNeighborhood';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
   NSLErrorToString:    convert a numeric error code to its string equivalent.  Caller must
                        have allocated sufficient space to store both strings.  (Max 255 chars each)
                                
                        The errorString parameter will return a textual explanation of what is wrong,
                        while the solutionString returns a possible solution to get around the problem
}

{
 *  NSLErrorToString()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in NSLPPCLib 1.0 and later
 }
function NSLErrorToString( theErr: NSLError; errorString: CStringPtr; solutionString: CStringPtr ): OSStatus; external name '_NSLErrorToString';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
  -----------------------------------------------------------------------------
    Basic API calls: sufficient to create simple requests, and receive answers
  -----------------------------------------------------------------------------
}

{
 *  NSLOpenNavigationAPI()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in NSLPPCLib 1.0 and later
 }
function NSLOpenNavigationAPI( var newRef: NSLClientRef ): OSStatus; external name '_NSLOpenNavigationAPI';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
 *  NSLCloseNavigationAPI()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in NSLPPCLib 1.0 and later
 }
procedure NSLCloseNavigationAPI( theClient: NSLClientRef ); external name '_NSLCloseNavigationAPI';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
    NSLPrepareRequest:  creates an NSLRequestRef, sets up some internal data
    notifier is an NSLClientNotifyUPP that will be called when data is available, when the lookup has
    completed, or if an error occurs.  When the notifier is called, the cookie will be the NSLRequestRef.
    If notifier is NULL, then the NSLManager will assume that the request is made synchronously.  This
    should only be used while in a separate thread, so that the client app can still process events, etc.
    
    contextPtr is a void* which is passed as the contextPtr argument when the notifier is called.  
    
    upon exit:
    1) ref will contain a pointer to a NSLRequestRef which must be passed to all other functions
    which require a NSLRequestRef.
    2) infoPtr will point to a newly created ClientAsycnInfoPtr which will be disposed by the manager when the search is completed
    NOTE: Only one search can be running at a time per clientRef.
}

{
 *  NSLPrepareRequest()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in NSLPPCLib 1.0 and later
 }
function NSLPrepareRequest( notifier: NSLClientNotifyUPP; contextPtr: UnivPtr; theClient: NSLClientRef; var ref: NSLRequestRef; bufPtr: CStringPtr; bufLen: UNSIGNEDLONG; var infoPtr: NSLClientAsyncInfoPtr ): NSLError; external name '_NSLPrepareRequest';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
   NSLStartNeighborhoodLookup: looking for neighborhoods associated with or neighboring a particular neighborhood
    Passing in NULL for neighborhood will generate a list of a default neighborhood(s)
   
}

{
 *  NSLStartNeighborhoodLookup()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in NSLPPCLib 1.0 and later
 }
function NSLStartNeighborhoodLookup( ref: NSLRequestRef; neighborhood: NSLNeighborhood; var asyncInfo: NSLClientAsyncInfo ): NSLError; external name '_NSLStartNeighborhoodLookup';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
   NSLStartServicesLookup: starts looking for entities if the specified type in the specified neighborhood
   
}

{
 *  NSLStartServicesLookup()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in NSLPPCLib 1.0 and later
 }
function NSLStartServicesLookup( ref: NSLRequestRef; neighborhood: NSLNeighborhood; requestData: NSLTypedDataPtr; var asyncInfo: NSLClientAsyncInfo ): NSLError; external name '_NSLStartServicesLookup';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{ NSLContinueLookup:  continues a paused/outstanding lookup}

{
 *  NSLContinueLookup()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in NSLPPCLib 1.0 and later
 }
function NSLContinueLookup( var asyncInfo: NSLClientAsyncInfo ): NSLError; external name '_NSLContinueLookup';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{ NSLCancelRequest: cancels an ongoing search}

{
 *  NSLCancelRequest()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in NSLPPCLib 1.0 and later
 }
function NSLCancelRequest( ref: NSLRequestRef ): NSLError; external name '_NSLCancelRequest';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
   NSLDeleteRequest: deletes info associated with this ref.  The ClientAsyncInfoPtr will no longer be valid
    This must be called when the client is no longer using this requestRef.
}

{
 *  NSLDeleteRequest()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in NSLPPCLib 1.0 and later
 }
function NSLDeleteRequest( ref: NSLRequestRef ): NSLError; external name '_NSLDeleteRequest';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
  -----------------------------------------------------------------------------
   Utility API calls: use these accessors to manipulate NSL's typed data
  -----------------------------------------------------------------------------
}

{ NSLParseServicesRequestPB provides the inverse of NSLMakeRequestPB, filling out the offsets found within newDataPtr}
{ <--- returns an OSStatus if any errors occur parsing the data }
{ <--- newDataPtr is the construct passed to the plugin }
{ ---> serviceListPtr is the address of a pointer which will be set to point at the portion of the newDataPtr that holds the serviceList to be searched }
{ ---> serviceListLen is the length of the serviceListPtr data pointed to by serviceListPtr }
{
 *  NSLParseServicesRequestPB()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in NSLPPCLib 1.0 and later
 }
function NSLParseServicesRequestPB( newDataPtr: NSLTypedDataPtr; var serviceListPtr: CStringPtr; var serviceListLen: UInt16 ): OSStatus; external name '_NSLParseServicesRequestPB';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{ NSLParseServiceRegistrationPB provides for breaking apart a registration request from a client to a plugin }
{ <--- returns an OSStatus if any errors occur parsing the data }
{ <--- newDataPtr is the construct passed to the plugin }
{ ---> neighborhoodPtr gets set to point at the portion of the newDataPtr that contains the neighborhood }
{ ---> neighborhoodLen is the length of the neighborhood pointed to by neighborhoodPtr }
{ ---> urlPtr is the address of a pointer which will be set to point at the portion of the newDataPtr that holds the url to be registered }
{ ---> urlLen is the length of the url data pointed to by urlPtr }
{
 *  NSLParseServiceRegistrationPB()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in NSLPPCLib 1.1 and later
 }
function NSLParseServiceRegistrationPB( newDataPtr: NSLTypedDataPtr; var neighborhoodPtr: NSLNeighborhood; var neighborhoodLen: UInt16; var urlPtr: CStringPtr; var urlLen: UInt16 ): OSStatus; external name '_NSLParseServiceRegistrationPB';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{ NSLGetErrorStringsFromResource is obsolete in X.  It will ignore the fileSpecPtr }
{ and errorResID parameters and return the standard error strings. }
{ NSLGetErrorStringsFromResource makes a basic assumption: }
{ errorString and solutionString both point to valid memory of at least 256 bytes! }
{ <--- returns an OSStatus if any errors occur }
{ ---> theErr is an OSStatus to be matched against a resource list of errors }
{ ---> fileSpecPtr is a FSSpecPtr to the resource containing the list of errors }
{ ---> errorResID is the resourceID of the NSLI resource of the list of errors }
{ <--> errorString is a pointer to valid memory of at least 256 bytes which will be filled out by the error portion of the error string }
{ <--> solutionString is a pointer to valid memory of at least 256 bytes which will be filled out by the solution portion of the error string }
{
 *  NSLGetErrorStringsFromResource()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in NSLPPCLib 1.1 and later
 }
function NSLGetErrorStringsFromResource( theErr: OSStatus; const (*var*) fileSpecPtr: FSSpec; errorResID: SInt16;  errorString: CStringPtr; solutionString: CStringPtr ): OSStatus; external name '_NSLGetErrorStringsFromResource';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{ <--- Returns true if given service is in the given service list }
{ ---> serviceList is a valid NSLServicesList containing information about services to be searched }
{ ---> svcToFind is an NSLServiceType of a particular service to check if it is in the serviceList }
{
 *  NSLServiceIsInServiceList()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in NSLPPCLib 1.1 and later
 }
function NSLServiceIsInServiceList( serviceList: NSLServicesList; svcToFind: NSLServiceType ): Boolean; external name '_NSLServiceIsInServiceList';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{ <--- returns an OSStatus if any errors occur parsing the data }
{ ---> svcString is the address of a pointer which will be set to point at the portion of theURL that holds the serviceType of theURL }
{ ---> svcLen is the length of the serviceType pointed to by svcString }
{
 *  NSLGetServiceFromURL()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in NSLPPCLib 1.1 and later
 }
function NSLGetServiceFromURL( theURL: CStringPtr; var svcString: CStringPtr; var svcLen: UInt16 ): OSStatus; external name '_NSLGetServiceFromURL';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{ <--- returns the length of a Neighborhood data structure }
{ ---> neighborhood is a valid NSLNeighborhood }
{
 *  NSLGetNeighborhoodLength()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in NSLPPCLib 1.1 and later
 }
function NSLGetNeighborhoodLength( neighborhood: NSLNeighborhood ): SIGNEDLONG; external name '_NSLGetNeighborhoodLength';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
  -------------------------------------------------------------------------------------
   Utility API calls: use these routines to separate plugin threads from client threads
  -------------------------------------------------------------------------------------
}

{ this routine works the same as the Thread manager's routine NewThread, except }
{ that the thread is added to the NSL manager's thread list. }
{
 *  NSLNewThread()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in NSLPPCLib 1.1 and later
 }
function NSLNewThread( threadStyle_: ThreadStyle; threadEntry: ThreadEntryProcPtr; threadParam: UnivPtr; stackSize: Size; options: ThreadOptions; var threadResult: UnivPtr; var threadMade: ThreadID ): OSErr; external name '_NSLNewThread';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{ this routine works the same as the Thread manager's routine DisposeThread, except }
{ that the thread is removed from the NSL manager's thread list. }
{
 *  NSLDisposeThread()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in NSLPPCLib 1.1 and later
 }
function NSLDisposeThread( threadToDump: ThreadID; threadResult: UnivPtr; recycleThread: Boolean ): OSErr; external name '_NSLDisposeThread';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{$endc} {not TARGET_CPU_64}

{$ifc OLDROUTINENAMES}
type
	ClientAsyncInfo = NSLClientAsyncInfo;
	PluginAsyncInfo = NSLPluginAsyncInfo;
	TypedData = NSLTypedData;
	PluginData = NSLPluginData;
	ClientAsyncInfoPtr = NSLClientAsyncInfoPtr;
	PluginAsyncInfoPtr = NSLPluginAsyncInfoPtr;
	TypedDataPtr = NSLTypedDataPtr;
	PluginDataPtr = NSLPluginDataPtr;

{$endc} {OLDROUTINENAMES}

{$endc} {TARGET_OS_MAC}
{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}

end.
{$endc} {not MACOSALLINCLUDE}
