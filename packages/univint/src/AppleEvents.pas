{
     File:       AE/AppleEvents.h
 
     Contains:   AppleEvent Package Interfaces.
 
     Version:    AppleEvents-496~1
 
     Copyright:  © 1989-2008 by Apple Computer, Inc., all rights reserved
 
     Bugs?:      For bug reports, consult the following page on
                 the World Wide Web:
 
                     http://www.freepascal.org/bugs.html
 
}
{  Pascal Translation Updated:  Jonas Maebe, <jonas@freepascal.org>, October 2009 }
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

unit AppleEvents;
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
	{$setc TARGET_CPU_PPC := FALSE}
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
uses MacTypes,MixedMode,AEDataModel,AEInteraction, CFArray, CFBase, CFRunLoop, CFStream, CFURL;
{$endc} {not MACOSALLINCLUDE}


{$ifc TARGET_OS_MAC}

{
    Note:   The functions and types for the building and parsing AppleEvent  
            messages has moved to AEDataModel.h
}


{$ALIGN MAC68K}

const
{ Keywords for Apple event parameters }
	keyDirectObject = FourCharCode('----');
	keyErrorNumber = FourCharCode('errn');
	keyErrorString = FourCharCode('errs');
	keyProcessSerialNumber = FourCharCode('psn '); { Keywords for special handlers }
	keyPreDispatch = FourCharCode('phac'); { preHandler accessor call }
	keySelectProc = FourCharCode('selh'); { more selector call }
                                        { Keyword for recording }
	keyAERecorderCount = FourCharCode('recr'); { available only in vers 1.0.1 and greater }
                                        { Keyword for version information }
	keyAEVersion = FourCharCode('vers'); { available only in vers 1.0.1 and greater }

{ Event Class }
const
	kCoreEventClass = FourCharCode('aevt');

{ Event ID's }
const
	kAEOpenApplication = FourCharCode('oapp');
	kAEOpenDocuments = FourCharCode('odoc');
	kAEPrintDocuments = FourCharCode('pdoc');
	kAEOpenContents = FourCharCode('ocon');
	kAEQuitApplication = FourCharCode('quit'); { may include a property kAEQuitReason indicating what lead to the quit being sent. }
	kAEAnswer = FourCharCode('ansr');
	kAEApplicationDied = FourCharCode('obit');
	kAEShowPreferences = FourCharCode('pref'); { sent by Mac OS X when the user chooses the Preferences item }

{ Constants for recording }
const
	kAEStartRecording = FourCharCode('reca'); { available only in vers 1.0.1 and greater }
	kAEStopRecording = FourCharCode('recc'); { available only in vers 1.0.1 and greater }
	kAENotifyStartRecording = FourCharCode('rec1'); { available only in vers 1.0.1 and greater }
	kAENotifyStopRecording = FourCharCode('rec0'); { available only in vers 1.0.1 and greater }
	kAENotifyRecording = FourCharCode('recr'); { available only in vers 1.0.1 and greater }


{
 * AEEventSource is defined as an SInt8 for compatability with pascal.
 * Important note: keyEventSourceAttr is returned by AttributePtr as a typeShortInteger.
 * Be sure to pass at least two bytes of storage to AEGetAttributePtr - the result can be
 * compared directly against the following enums.
 }
type
	AEEventSource = SInt8;
const
	kAEUnknownSource = 0;
	kAEDirectCall = 1;
	kAESameProcess = 2;
	kAELocalProcess = 3;
	kAERemoteProcess = 4;

{*************************************************************************
  These calls are used to set up and modify the event dispatch table.
*************************************************************************}
{
 *  AEInstallEventHandler()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function AEInstallEventHandler( theAEEventClass: AEEventClass; theAEEventID: AEEventID; handler: AEEventHandlerUPP; handlerRefcon: SRefCon; isSysHandler: Boolean ): OSErr; external name '_AEInstallEventHandler';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  AERemoveEventHandler()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function AERemoveEventHandler( theAEEventClass: AEEventClass; theAEEventID: AEEventID; handler: AEEventHandlerUPP; isSysHandler: Boolean ): OSErr; external name '_AERemoveEventHandler';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  AEGetEventHandler()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function AEGetEventHandler( theAEEventClass: AEEventClass; theAEEventID: AEEventID; var handler: AEEventHandlerUPP; var handlerRefcon: SRefCon; isSysHandler: Boolean ): OSErr; external name '_AEGetEventHandler';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{*************************************************************************
  These calls are used to set up and modify special hooks into the
  AppleEvent manager.
*************************************************************************}
{
 *  AEInstallSpecialHandler()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function AEInstallSpecialHandler( functionClass: AEKeyword; handler: AEEventHandlerUPP; isSysHandler: Boolean ): OSErr; external name '_AEInstallSpecialHandler';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  AERemoveSpecialHandler()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function AERemoveSpecialHandler( functionClass: AEKeyword; handler: AEEventHandlerUPP; isSysHandler: Boolean ): OSErr; external name '_AERemoveSpecialHandler';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  AEGetSpecialHandler()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function AEGetSpecialHandler( functionClass: AEKeyword; var handler: AEEventHandlerUPP; isSysHandler: Boolean ): OSErr; external name '_AEGetSpecialHandler';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{*************************************************************************
  This call was added in version 1.0.1. If called with the keyword
  keyAERecorderCount ('recr'), the number of recorders that are
  currently active is returned in 'result'
  (available only in vers 1.0.1 and greater).
*************************************************************************}
{
 *  AEManagerInfo()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function AEManagerInfo( keyWord: AEKeyword; var result: SIGNEDLONG ): OSErr; external name '_AEManagerInfo';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
  AERemoteProcessResolver:
  
  These calls subsume the functionality of using the PPCToolbox on Mac
  OS 9 to locate processes on remote computers.  (PPCToolbox is not
  part of Carbon.)  These calls are supported on Mac OS X 10.3 or
  later.
  
  The model is to create a resolver for a particular URL and schedule
  it on a CFRunLoop to retrieve the results asynchronously.  If
  synchronous behavior is desired, just call
  AERemoteProcessResolverGetProcesses to get the array; the call will
  block until the request is completed.
  
  A resolver can only be used once; once it has fetched the data or
  gotten an error it can no longer be scheduled.
  
  The data obtained from the resolver is a CFArrayRef of
  CFDictionaryRef objects.  Each dictionary contains the URL of the
  remote application and its human readable name.
}

{
 *  kAERemoteProcessURLKey
 *  
 *  Discussion:
 *    the full URL to this application, a CFURLRef.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kAERemoteProcessURLKey: CFStringRef; external name '_kAERemoteProcessURLKey'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)
{
 *  kAERemoteProcessNameKey
 *  
 *  Discussion:
 *    the visible name to this application, in the localization
 *    supplied by the server, a CFStringRef.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kAERemoteProcessNameKey: CFStringRef; external name '_kAERemoteProcessNameKey'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)
{
 *  kAERemoteProcessUserIDKey
 *  
 *  Discussion:
 *    the userid of this application, if available.  If present, a
 *    CFNumberRef.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kAERemoteProcessUserIDKey: CFStringRef; external name '_kAERemoteProcessUserIDKey'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)
{
 *  kAERemoteProcessProcessIDKey
 *  
 *  Discussion:
 *    the process id of this application, if available.  If present, a
 *    CFNumberRef.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kAERemoteProcessProcessIDKey: CFStringRef; external name '_kAERemoteProcessProcessIDKey'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)

{
 *  AERemoteProcessResolverContext
 *  
 *  Discussion:
 *    An optional context parameter for asynchronous resolution.  The
 *    context is copied and the info pointer retained.  When the
 *    callback is made, the info pointer is passed to the callback.
 }
type
	AERemoteProcessResolverContextPtr = ^AERemoteProcessResolverContext;
	AERemoteProcessResolverContext = record
{
   * set to zero (0)
   }
		version: CFIndex;

  {
   * info pointer to be passed to the callback
   }
		info: UnivPtr;

  {
   * callback made on the info pointer. This field may be NULL.
   }
		retain: CFAllocatorRetainCallBack;

  {
   * callback made on the info pointer. This field may be NULL.
   }
		release: CFAllocatorReleaseCallBack;

  {
   * callback made on the info pointer. This field may be NULL.
   }
		copyDescription: CFAllocatorCopyDescriptionCallBack;
	end;

{
 *  AERemoteProcessResolverRef
 *  
 *  Discussion:
 *    An opaque reference to an object that encapsulates the mechnanism
 *    by which a list of processes running on a remote machine are
 *    obtained.  Created by AECreateRemoteProcessResolver, and must be
 *    disposed of by AEDisposeRemoteProcessResolver. A
 *    AERemoteProcessResolverRef is not a CFType.
 }
type
	AERemoteProcessResolverRef = ^SInt32; { an opaque type }
{
 *  AECreateRemoteProcessResolver()
 *  
 *  Discussion:
 *    Create a Remote Process List Resolver object.  The allocator is
 *    used for any CoreFoundation types created or returned by this
 *    API.  The resulting object can be scheduled on a run loop, or
 *    queried synchronously.  Once the object has retreived results
 *    from the server, or got an error doing so, it will not re-fetch
 *    the data.  To retrieve a new list of processes, create a new
 *    instance of this object.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.3
 *  
 *  Parameters:
 *    
 *    allocator:
 *      a CFAllocatorRef to use when creating CFTypes
 *    
 *    url:
 *      a CFURL identifying the remote host and port.
 *  
 *  Result:
 *    a AECreateRemoteProcessResolverRef, which must be disposed of
 *    with AEDisposeRemoteProcessResolver.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function AECreateRemoteProcessResolver( allocator: CFAllocatorRef; url: CFURLRef ): AERemoteProcessResolverRef; external name '_AECreateRemoteProcessResolver';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)


{
 *  AEDisposeRemoteProcessResolver()
 *  
 *  Discussion:
 *    Disposes of a AERemoteProcessResolverRef.  If this resolver is
 *    currently scheduled on a run loop, it is unscheduled.  In this
 *    case, the asynchronous callback will not be executed.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.3
 *  
 *  Parameters:
 *    
 *    ref:
 *      The AERemoteProcessResolverRef to dispose
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure AEDisposeRemoteProcessResolver( ref: AERemoteProcessResolverRef ); external name '_AEDisposeRemoteProcessResolver';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)


{
 *  AERemoteProcessResolverGetProcesses()
 *  
 *  Discussion:
 *    Returns a CFArrayRef containing CFDictionary objects containing
 *    information about processses running on a remote machine.  If the
 *    result array is NULL, the query failed and the error out
 *    parameter will contain information about the failure.  If the
 *    resolver had not been previously scheduled for execution, this
 *    call will block until the resulting array is available or an
 *    error occurs.  If the resolver had been scheduled but had not yet
 *    completed fetching the array, this call will block until the
 *    resolver does complete.  The array returned is owned by the
 *    resolver, so callers must retain it before disposing of the
 *    resolver object itself.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.3
 *  
 *  Parameters:
 *    
 *    ref:
 *      The AERemoteProcessResolverRef to query
 *    
 *    outError:
 *      If the result is NULL, outError will contain a CFStreamError
 *      with information about the type of failure
 *  
 *  Result:
 *    a CFArray of CFDictionary objects containing information about
 *    the remote applications.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function AERemoteProcessResolverGetProcesses( ref: AERemoteProcessResolverRef; var outError: CFStreamError ): CFArrayRef; external name '_AERemoteProcessResolverGetProcesses';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)


{
 *  AERemoteProcessResolverCallback
 *  
 *  Discussion:
 *    A callback made when the asynchronous execution of a resolver
 *    completes, either due to success or failure. The data itself
 *    should be obtained with AERemoteProcessResolverGetProcesses.
 }
type
	AERemoteProcessResolverCallback = procedure( ref: AERemoteProcessResolverRef; info: UnivPtr );
{
 *  AERemoteProcessResolverScheduleWithRunLoop()
 *  
 *  Discussion:
 *    Schedules a resolver for execution on a given runloop in a given
 *    mode.   The resolver will move through various internal states as
 *    long as the specified run loop is run.  When the resolver
 *    completes, either with success or an error condition, the
 *    callback is executed.  There is no explicit unschedule of the
 *    resolver; you must dispose of it to remove it from the run loop.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.3
 *  
 *  Parameters:
 *    
 *    ref:
 *      The AERemoteProcessResolverRef to scheduile
 *    
 *    runLoop:
 *      a CFRunLoop
 *    
 *    runLoopMode:
 *      a CFString specifying the run loop mode
 *    
 *    callback:
 *      a callback to be executed when the reolver completes
 *    
 *    ctx:
 *      a AERemoteProcessResolverContext.  If this parameter is not
 *      NULL, the info field of this structure will be passed to the
 *      callback (otherwise, the callback info parameter will
 *      explicitly be NULL.)
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure AERemoteProcessResolverScheduleWithRunLoop( ref: AERemoteProcessResolverRef; runLoop: CFRunLoopRef; runLoopMode: CFStringRef; callback: AERemoteProcessResolverCallback; {const} ctx: AERemoteProcessResolverContextPtr { can be NULL } ); external name '_AERemoteProcessResolverScheduleWithRunLoop';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)


{$endc} {TARGET_OS_MAC}
{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}

end.
{$endc} {not MACOSALLINCLUDE}
