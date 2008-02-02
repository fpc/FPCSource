{
     File:       NSLCore.p
 
     Contains:   Interface to API for using the NSL Manager
 
     Version:    Technology: Mac OS X
                 Release:    Universal Interfaces 3.4.2
 
     Copyright:  © 1985-2002 by Apple Computer, Inc., all rights reserved
 
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

unit NSLCore;
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
uses MacTypes,CodeFragments,Files,Threads;

{$ALIGN MAC68K}


const
	kNSLMinSystemVersion		= $0900;						{  equivalent to 9.0 }
	kNSLMinOTVersion			= $0130;						{  equivalent to 1.3 }

	kNSLDefaultListSize			= 256;							{  default list size for service and protocol lists }

	kNSLURLDelimiter			= 44;							{  delimits URL's within memory buffers }

	kNSLNoContext				= 0;							{  the default context for NSLError structs }


type
	NSLErrorPtr = ^NSLError;
	NSLError = record
		theErr:					OSStatus;
		theContext:				UInt32;
	end;


const
																{  Constants to use with NSLPrepareRequest }
																{  kNSLDuplicateSearchInProgress is not strictly an error.  The client is free to ignore }
																{  this result, and nothing bad will happen if it does.  It is }
																{  informational only. }
	kNSLDuplicateSearchInProgress = 100;
	kNSLUserCanceled			= -128;							{  User hit cancel from the NSLStandardGetURL dialog  }
																{  Invalid enumeratorRef   }
	kNSLInvalidEnumeratorRef	= 0;							{  this is not an error; it is the equiv to a NULL ptr }


type
	NSLSearchState 				= UInt16;
const
																{  State codes for notifiers. }
	kNSLSearchStateBufferFull	= 1;
	kNSLSearchStateOnGoing		= 2;
	kNSLSearchStateComplete		= 3;
	kNSLSearchStateStalled		= 4;
	kNSLWaitingForContinue		= 5;


type
	NSLEventCode 				= UInt32;
const
																{  Event codes }
	kNSLServicesLookupDataEvent	= 6;
	kNSLNeighborhoodLookupDataEvent = 7;
	kNSLNewDataEvent			= 8;
	kNSLContinueLookupEvent		= 9;


type
	NSLClientRef						= UInt32;
	NSLRequestRef						= UInt32;
	NSLOneBasedIndex					= UInt32;
	NSLPath								= ^char;
	NSLServiceType						= ^char;
	NSLServicesList						= Handle;
	NSLNeighborhood						= ^UInt8;
	{
	   cstring which is a comma delimited list of protocols which can be used to
	   create a NSLProtocolList internally
	}
	{  the async information block for client<->manager interaction }
	NSLClientAsyncInfoPtr = ^NSLClientAsyncInfo;
	NSLClientAsyncInfo = record
		clientContextPtr:		Ptr;									{  set by the client for its own use }
		mgrContextPtr:			Ptr;									{  set by NSL mgr; ptr to request object ptr }
		resultBuffer:			CStringPtr;
		bufferLen:				SInt32;
		maxBufferSize:			SInt32;
		startTime:				UInt32;									{  when the search starts, to use with maxSearchTime to determine time-out condition }
		intStartTime:			UInt32;									{  used with alertInterval }
		maxSearchTime:			UInt32;									{  total time for search, in ticks (0 == no time limit) }
		alertInterval:			UInt32;									{  call client's notifier or return, every this many ticks ( 0 == don't use this param) }
		totalItems:				UInt32;									{  total number of tuples currently in buffer }
		alertThreshold:			UInt32;									{  call client's notifier or return, every this many items found ( 0 == don't use this param) }
		searchState:			NSLSearchState;
		searchResult:			NSLError;
		searchDataType:			NSLEventCode;							{  this is a data type code which allows the client's asyncNotifier to properly }
																		{  handle the data in resultBuffer. }
	end;

	{  the async information block plugin<->manager interaction }
	NSLPluginAsyncInfoPtr = ^NSLPluginAsyncInfo;
	NSLPluginAsyncInfo = record
		mgrContextPtr:			Ptr;									{  set by NSL mgr; ptr to request object ptr }
		pluginContextPtr:		Ptr;									{  set/used by individual plugins }
		pluginPtr:				Ptr;									{  ptr to the plugin object waiting for continue lookup call }
		resultBuffer:			CStringPtr;								{  set by plugin to point at data }
		bufferLen:				SInt32;
		maxBufferSize:			SInt32;
		maxSearchTime:			UInt32;									{  total time for search, in ticks (0 == no time limit) }
		reserved1:				UInt32;
		reserved2:				UInt32;
		reserved3:				UInt32;
		clientRef:				NSLClientRef;
		requestRef:				NSLRequestRef;
		searchState:			NSLSearchState;
		searchResult:			OSStatus;
	end;

	{  the manager asynchronous notifier routine. }
{$ifc TYPED_FUNCTION_POINTERS}
	NSLMgrNotifyProcPtr = procedure(var thePluginAsyncInfo: NSLPluginAsyncInfo);
{$elsec}
	NSLMgrNotifyProcPtr = ProcPtr;
{$endc}

	{  the client asynchronous notifier routine. }
{$ifc TYPED_FUNCTION_POINTERS}
	NSLClientNotifyProcPtr = procedure(var theClientAsyncInfo: NSLClientAsyncInfo);
{$elsec}
	NSLClientNotifyProcPtr = ProcPtr;
{$endc}

{$ifc OPAQUE_UPP_TYPES}
	NSLMgrNotifyUPP = ^SInt32; { an opaque UPP }
{$elsec}
	NSLMgrNotifyUPP = UniversalProcPtr;
{$endc}	
{$ifc OPAQUE_UPP_TYPES}
	NSLClientNotifyUPP = ^SInt32; { an opaque UPP }
{$elsec}
	NSLClientNotifyUPP = UniversalProcPtr;
{$endc}	

const
	uppNSLMgrNotifyProcInfo = $000000C0;
	uppNSLClientNotifyProcInfo = $000000C0;
	{
	 *  NewNSLMgrNotifyUPP()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   available as macro/inline
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
function NewNSLMgrNotifyUPP(userRoutine: NSLMgrNotifyProcPtr): NSLMgrNotifyUPP; external name '_NewNSLMgrNotifyUPP'; { old name was NewNSLMgrNotifyProc }
{
 *  NewNSLClientNotifyUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewNSLClientNotifyUPP(userRoutine: NSLClientNotifyProcPtr): NSLClientNotifyUPP; external name '_NewNSLClientNotifyUPP'; { old name was NewNSLClientNotifyProc }
{
 *  DisposeNSLMgrNotifyUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeNSLMgrNotifyUPP(userUPP: NSLMgrNotifyUPP); external name '_DisposeNSLMgrNotifyUPP';
{
 *  DisposeNSLClientNotifyUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeNSLClientNotifyUPP(userUPP: NSLClientNotifyUPP); external name '_DisposeNSLClientNotifyUPP';
{
 *  InvokeNSLMgrNotifyUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure InvokeNSLMgrNotifyUPP(var thePluginAsyncInfo: NSLPluginAsyncInfo; userRoutine: NSLMgrNotifyUPP); external name '_InvokeNSLMgrNotifyUPP'; { old name was CallNSLMgrNotifyProc }
{
 *  InvokeNSLClientNotifyUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure InvokeNSLClientNotifyUPP(var theClientAsyncInfo: NSLClientAsyncInfo; userRoutine: NSLClientNotifyUPP); external name '_InvokeNSLClientNotifyUPP'; { old name was CallNSLClientNotifyProc }
{
   this struct is a format for dealing with our internal data representation.  Typed data will be contiguous chunk of
   memory, with the first 4 bytes being a data "descriptor".
}

type
	NSLTypedDataPtr = ^NSLTypedData;
	NSLTypedData = record
		dataType:				UInt32;
		lengthOfData:			UInt32;
	end;

	{
	   This is just a header at the beginning of a handle that stores our list of service types.
	   Each service type is a pascal string, so each service type starts after the end of the
	   previous one.
	}
	NSLServicesListHeaderPtr = ^NSLServicesListHeader;
	NSLServicesListHeader = record
		numServices:			UInt32;
		logicalLen:				UInt32;									{  length of all usable data in handle }
	end;

	{  some defs for common protocols }
	{
	   general information from a plug-in.  Includes supported protocols, data types and services,
	   as well as an info/comment string describing the function of the plug-in in human-readable
	   form.  The offsets point to the beginning of each list of data returned, and the protocol
	   data offset is the startOfData member of the struct
	}
	NSLPluginDataPtr = ^NSLPluginData;
	NSLPluginData = record
		reserved1:				SInt32;
		reserved2:				SInt32;
		reserved3:				SInt32;
		supportsRegistration:	boolean;
		isPurgeable:			boolean;
		totalLen:				UInt16;									{  length of everything, including header }
		dataTypeOffset:			UInt16;
		serviceListOffset:		UInt16;
		protocolListOffset:		UInt16;
		commentStringOffset:	UInt16;
																		{  protocol data is first on the list }
	end;

	{
	  -----------------------------------------------------------------------------
	    Finding out if the library is present and getting its version
	  -----------------------------------------------------------------------------
	}

	{
	 *  NSLLibraryVersion()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in NSLPPCLib 1.1 and later
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
function NSLLibraryVersion: UInt32; external name '_NSLLibraryVersion';


{
    NSLLibraryPresent() is a macro available only in C/C++.  
}
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
{
 *  NSLStandardRegisterURL()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in NSLPPCLib 1.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NSLStandardRegisterURL(urlToRegister: NSLPath; neighborhoodToRegisterIn: NSLNeighborhood): NSLError; external name '_NSLStandardRegisterURL';

{ <--- error code from registration }
{ ---> urlToRegister is a null terminated url that has only legal characters defined for URLs.  Use HexEncodeText to encode}
{          portions of the url that have illegal characters }
{ ---> neighborhoodToDeregisterIn is an optional parameter for explicitly defining a neighborhood to register in.
            If parameter is NULL, then the plugins will determine where to register the service }
{
 *  NSLStandardDeregisterURL()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in NSLPPCLib 1.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NSLStandardDeregisterURL(urlToDeregister: NSLPath; neighborhoodToDeregisterIn: NSLNeighborhood): NSLError; external name '_NSLStandardDeregisterURL';


{ ----------------------------------------------------------------------------- }

{
 *  NSLHexEncodeText()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in NSLPPCLib 1.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NSLHexEncodeText(rawText: ConstCStringPtr; rawTextLen: UInt16; newTextBuffer: CStringPtr; var newTextBufferLen: UInt16; var textChanged: boolean): OSStatus; external name '_NSLHexEncodeText';

{
 *  NSLHexDecodeText()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in NSLPPCLib 1.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NSLHexDecodeText(encodedText: ConstCStringPtr; encodedTextLen: UInt16; decodedTextBuffer: CStringPtr; var decodedTextBufferLen: UInt16; var textChanged: boolean): OSStatus; external name '_NSLHexDecodeText';

{
  -----------------------------------------------------------------------------
    Basic API Utility calls: sufficient to create, and parse data structures
  -----------------------------------------------------------------------------
}

{
 *  NSLMakeNewServicesList()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in NSLPPCLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NSLMakeNewServicesList(initialServiceList: ConstCStringPtr): NSLServicesList; external name '_NSLMakeNewServicesList';

{
 *  NSLAddServiceToServicesList()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in NSLPPCLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NSLAddServiceToServicesList(serviceList: NSLServicesList; serviceType: NSLServiceType): NSLError; external name '_NSLAddServiceToServicesList';

{
 *  NSLDisposeServicesList()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in NSLPPCLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure NSLDisposeServicesList(theList: NSLServicesList); external name '_NSLDisposeServicesList';

{
    The name reflects the name of the Neighborhood, i.e. "apple.com." or "AppleTalk Zone One".
    The protocolList is a comma delimited list of protocols that the Neighborhood might exist in.
    If the user passes in NULL, then all protocols will be queried.  The result must be disposed
    of by the user by calling NSLFreeNeighborhood.
}
{
 *  NSLMakeNewNeighborhood()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in NSLPPCLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NSLMakeNewNeighborhood(name: ConstCStringPtr; protocolList: ConstCStringPtr): NSLNeighborhood; external name '_NSLMakeNewNeighborhood';

{ creates an exact copy of an existing neighborhood }
{
 *  NSLCopyNeighborhood()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in NSLPPCLib 1.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NSLCopyNeighborhood(neighborhood: NSLNeighborhood): NSLNeighborhood; external name '_NSLCopyNeighborhood';

{
 *  NSLFreeNeighborhood()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in NSLPPCLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NSLFreeNeighborhood(neighborhood: NSLNeighborhood): NSLNeighborhood; external name '_NSLFreeNeighborhood';

{
 *  NSLGetNameFromNeighborhood()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in NSLPPCLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure NSLGetNameFromNeighborhood(neighborhood: NSLNeighborhood; var name: CStringPtr; var length: SInt32); external name '_NSLGetNameFromNeighborhood';

{
   create a block of formatted data, pointed to by newDataPtr.  This will be used
   in calls (typically request-related calls) for plug-ins that handle the NSL data type.
}
{
 *  NSLMakeServicesRequestPB()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in NSLPPCLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NSLMakeServicesRequestPB(serviceList: NSLServicesList; var newDataPtr: NSLTypedDataPtr): OSStatus; external name '_NSLMakeServicesRequestPB';

{  releases any storage created with MakeXXXPB calls, associated with TypedData. }
{
 *  NSLFreeTypedDataPtr()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in NSLPPCLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NSLFreeTypedDataPtr(nslTypeData: NSLTypedDataPtr): NSLTypedDataPtr; external name '_NSLFreeTypedDataPtr';

{
   utility function that returns whether a url was found, a pointer to the beginning
   of the url, and the length of the URL.
}
{
 *  NSLGetNextUrl()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in NSLPPCLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NSLGetNextUrl(infoPtr: NSLClientAsyncInfoPtr; var urlPtr: CStringPtr; var urlLength: SInt32): boolean; external name '_NSLGetNextUrl';

{
   utility function that returns whether a Neighborhood was found, a pointer to the beginning
   of the Neighborhood, and the length of the Neighborhood.
}
{
 *  NSLGetNextNeighborhood()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in NSLPPCLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NSLGetNextNeighborhood(infoPtr: NSLClientAsyncInfoPtr; var neighborhood: NSLNeighborhood; var neighborhoodLength: SInt32): boolean; external name '_NSLGetNextNeighborhood';


{
   NSLErrorToString:    convert a numeric error code to its string equivalent.  Caller must
                        have allocated sufficient space to store both strings.  (Max 255 chars each)
                                
                        The errorString parameter will return a textual explanation of what is wrong,
                        while the solutionString returns a possible solution to get around the problem
}

{
 *  NSLErrorToString()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in NSLPPCLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NSLErrorToString(theErr: NSLError; errorString: CStringPtr; solutionString: CStringPtr): OSStatus; external name '_NSLErrorToString';


{
  -----------------------------------------------------------------------------
    Basic API calls: sufficient to create simple requests, and receive answers
  -----------------------------------------------------------------------------
}

{
 *  NSLOpenNavigationAPI()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in NSLPPCLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NSLOpenNavigationAPI(var newRef: NSLClientRef): OSStatus; external name '_NSLOpenNavigationAPI';

{
 *  NSLCloseNavigationAPI()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in NSLPPCLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure NSLCloseNavigationAPI(theClient: NSLClientRef); external name '_NSLCloseNavigationAPI';

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
 *  NSLPrepareRequest()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in NSLPPCLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NSLPrepareRequest(notifier: NSLClientNotifyUPP; contextPtr: UnivPtr; theClient: NSLClientRef; var ref: NSLRequestRef; bufPtr: CStringPtr; bufLen: UInt32; var infoPtr: NSLClientAsyncInfoPtr): NSLError; external name '_NSLPrepareRequest';


{
   NSLStartNeighborhoodLookup: looking for neighborhoods associated with or neighboring a particular neighborhood
    Passing in NULL for neighborhood will generate a list of a default neighborhood(s)
   
}

{
 *  NSLStartNeighborhoodLookup()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in NSLPPCLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NSLStartNeighborhoodLookup(ref: NSLRequestRef; neighborhood: NSLNeighborhood; var asyncInfo: NSLClientAsyncInfo): NSLError; external name '_NSLStartNeighborhoodLookup';

{
   NSLStartServicesLookup: starts looking for entities if the specified type in the specified neighborhood
   
}

{
 *  NSLStartServicesLookup()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in NSLPPCLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NSLStartServicesLookup(ref: NSLRequestRef; neighborhood: NSLNeighborhood; requestData: NSLTypedDataPtr; var asyncInfo: NSLClientAsyncInfo): NSLError; external name '_NSLStartServicesLookup';


{  NSLContinueLookup:  continues a paused/outstanding lookup }

{
 *  NSLContinueLookup()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in NSLPPCLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NSLContinueLookup(var asyncInfo: NSLClientAsyncInfo): NSLError; external name '_NSLContinueLookup';


{  NSLCancelRequest: cancels an ongoing search }

{
 *  NSLCancelRequest()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in NSLPPCLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NSLCancelRequest(ref: NSLRequestRef): NSLError; external name '_NSLCancelRequest';

{
   NSLDeleteRequest: deletes info associated with this ref.  The ClientAsyncInfoPtr will no longer be valid
    This must be called when the client is no longer using this requestRef.
}

{
 *  NSLDeleteRequest()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in NSLPPCLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NSLDeleteRequest(ref: NSLRequestRef): NSLError; external name '_NSLDeleteRequest';


{
  -----------------------------------------------------------------------------
   Utility API calls: use these accessors to manipulate NSL's typed data
  -----------------------------------------------------------------------------
}

{  NSLParseServicesRequestPB provides the inverse of NSLMakeRequestPB, filling out the offsets found within newDataPtr }
{ <--- returns an OSStatus if any errors occur parsing the data }
{ <--- newDataPtr is the construct passed to the plugin }
{ ---> serviceListPtr is the address of a pointer which will be set to point at the portion of the newDataPtr that holds the serviceList to be searched }
{ ---> serviceListLen is the length of the serviceListPtr data pointed to by serviceListPtr }
{
 *  NSLParseServicesRequestPB()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in NSLPPCLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NSLParseServicesRequestPB(newDataPtr: NSLTypedDataPtr; var serviceListPtr: CStringPtr; var serviceListLen: UInt16): OSStatus; external name '_NSLParseServicesRequestPB';


{ NSLParseServiceRegistrationPB provides for breaking apart a registration request from a client to a plugin }
{ <--- returns an OSStatus if any errors occur parsing the data }
{ <--- newDataPtr is the construct passed to the plugin }
{ ---> neighborhoodPtr gets set to point at the portion of the newDataPtr that contains the neighborhood }
{ ---> neighborhoodLen is the length of the neighborhood pointed to by neighborhoodPtr }
{ ---> urlPtr is the address of a pointer which will be set to point at the portion of the newDataPtr that holds the url to be registered }
{ ---> urlLen is the length of the url data pointed to by urlPtr }
{
 *  NSLParseServiceRegistrationPB()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in NSLPPCLib 1.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NSLParseServiceRegistrationPB(newDataPtr: NSLTypedDataPtr; var neighborhoodPtr: NSLNeighborhood; var neighborhoodLen: UInt16; var urlPtr: CStringPtr; var urlLen: UInt16): OSStatus; external name '_NSLParseServiceRegistrationPB';

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
 *  NSLGetErrorStringsFromResource()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in NSLPPCLib 1.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NSLGetErrorStringsFromResource(theErr: OSStatus; const (*var*) fileSpecPtr: FSSpec; errorResID: SInt16; errorString: CStringPtr; solutionString: CStringPtr): OSStatus; external name '_NSLGetErrorStringsFromResource';

{ <--- Returns true if given service is in the given service list }
{ ---> serviceList is a valid NSLServicesList containing information about services to be searched }
{ ---> svcToFind is an NSLServiceType of a particular service to check if it is in the serviceList }
{
 *  NSLServiceIsInServiceList()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in NSLPPCLib 1.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NSLServiceIsInServiceList(serviceList: NSLServicesList; svcToFind: NSLServiceType): boolean; external name '_NSLServiceIsInServiceList';

{ <--- returns an OSStatus if any errors occur parsing the data }
{ ---> svcString is the address of a pointer which will be set to point at the portion of theURL that holds the serviceType of theURL }
{ ---> svcLen is the length of the serviceType pointed to by svcString }
{
 *  NSLGetServiceFromURL()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in NSLPPCLib 1.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NSLGetServiceFromURL(theURL: CStringPtr; var svcString: CStringPtr; var svcLen: UInt16): OSStatus; external name '_NSLGetServiceFromURL';

{ <--- returns the length of a Neighborhood data structure }
{ ---> neighborhood is a valid NSLNeighborhood }
{
 *  NSLGetNeighborhoodLength()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in NSLPPCLib 1.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NSLGetNeighborhoodLength(neighborhood: NSLNeighborhood): SInt32; external name '_NSLGetNeighborhoodLength';

{
  -------------------------------------------------------------------------------------
   Utility API calls: use these routines to separate plugin threads from client threads
  -------------------------------------------------------------------------------------
}

{ this routine works the same as the Thread manager's routine NewThread, except }
{ that the thread is added to the NSL manager's thread list. }
{
 *  NSLNewThread()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in NSLPPCLib 1.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NSLNewThread(threadStyle_: ThreadStyle; threadEntry: ThreadEntryProcPtr; threadParam: UnivPtr; stackSize: Size; options: ThreadOptions; var threadResult: UnivPtr; var threadMade: ThreadID): OSErr; external name '_NSLNewThread';

{ this routine works the same as the Thread manager's routine DisposeThread, except }
{ that the thread is removed from the NSL manager's thread list. }
{
 *  NSLDisposeThread()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in NSLPPCLib 1.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NSLDisposeThread(threadToDump: ThreadID; threadResult: UnivPtr; recycleThread: boolean): OSErr; external name '_NSLDisposeThread';


{$ifc OLDROUTINENAMES}

type
	ClientAsyncInfo						= NSLClientAsyncInfo;
	ClientAsyncInfoPtr 					= ^ClientAsyncInfo;
	PluginAsyncInfo						= NSLPluginAsyncInfo;
	PluginAsyncInfoPtr 					= ^PluginAsyncInfo;
	TypedData							= NSLTypedData;
	TypedDataPtr 						= ^TypedData;
	PluginData							= NSLPluginData;
	PluginDataPtr 						= ^PluginData;
{$endc}  {OLDROUTINENAMES}

{$ALIGN MAC68K}


end.
