{
     File:       URLAccess.p
 
     Contains:   URL Access Interfaces.
 
     Version:    Technology: URLAccess 2.0
                 Release:    Universal Interfaces 3.4.2
 
     Copyright:  © 1994-2002 by Apple Computer, Inc., all rights reserved
 
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

unit URLAccess;
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
uses MacTypes,Files,CodeFragments,MacErrors,Events;

{$ALIGN MAC68K}

{ Data structures and types }

type
	URLReference    = ^SInt32; { an opaque 32-bit type }
	URLReferencePtr = ^URLReference;  { when a var xx:URLReference parameter can be nil, it is changed to xx: URLReferencePtr }
	URLOpenFlags 				= UInt32;
const
	kURLReplaceExistingFlag		= $01;
	kURLBinHexFileFlag			= $02;							{  Binhex before uploading if necessary }
	kURLExpandFileFlag			= $04;							{  Use StuffIt engine to expand file if necessary }
	kURLDisplayProgressFlag		= $08;
	kURLDisplayAuthFlag			= $10;							{  Display auth dialog if guest connection fails }
	kURLUploadFlag				= $20;							{  Do an upload instead of a download }
	kURLIsDirectoryHintFlag		= $40;							{  Hint: the URL is a directory }
	kURLDoNotTryAnonymousFlag	= $80;							{  Don't try to connect anonymously before getting logon info }
	kURLDirectoryListingFlag	= $0100;						{  Download the directory listing, not the whole directory }
	kURLExpandAndVerifyFlag		= $0200;						{  Expand file and then verify using signature resource }
	kURLNoAutoRedirectFlag		= $0400;						{  Do not automatically redirect to new URL }
	kURLDebinhexOnlyFlag		= $0800;						{  Do not use Stuffit Expander - just internal debinhex engine }
	kURLDoNotDeleteOnErrorFlag	= $1000;						{  Do not delete the downloaded file if an error or abort occurs. }
																{  This flag applies to downloading only and should be used if }
																{  interested in later resuming the download. }
	kURLResumeDownloadFlag		= $2000;						{  The passed in file is partially downloaded, attempt to resume }
																{  it.  Currently works for HTTP only.  If no FSSpec passed in, }
																{  this flag will be ignored. Overriden by kURLReplaceExistingFlag.  }
	kURLReservedFlag			= $80000000;					{  reserved for Apple internal use }


type
	URLState 					= UInt32;
const
	kURLNullState				= 0;
	kURLInitiatingState			= 1;
	kURLLookingUpHostState		= 2;
	kURLConnectingState			= 3;
	kURLResourceFoundState		= 4;
	kURLDownloadingState		= 5;
	kURLDataAvailableState		= $15;
	kURLTransactionCompleteState = 6;
	kURLErrorOccurredState		= 7;
	kURLAbortingState			= 8;
	kURLCompletedState			= 9;
	kURLUploadingState			= 10;


type
	URLEvent 					= UInt32;
const
	kURLInitiatedEvent			= 1;
	kURLResourceFoundEvent		= 4;
	kURLDownloadingEvent		= 5;
	kURLAbortInitiatedEvent		= 8;
	kURLCompletedEvent			= 9;
	kURLErrorOccurredEvent		= 7;
	kURLDataAvailableEvent		= $15;
	kURLTransactionCompleteEvent = 6;
	kURLUploadingEvent			= 10;
	kURLSystemEvent				= 29;
	kURLPercentEvent			= 30;
	kURLPeriodicEvent			= 31;
	kURLPropertyChangedEvent	= 32;


type
	URLEventMask						= UInt32;

const
	kURLInitiatedEventMask		= $01;
	kURLResourceFoundEventMask	= $08;
	kURLDownloadingMask			= $10;
	kURLUploadingMask			= $0200;
	kURLAbortInitiatedMask		= $80;
	kURLCompletedEventMask		= $0100;
	kURLErrorOccurredEventMask	= $40;
	kURLDataAvailableEventMask	= $00100000;
	kURLTransactionCompleteEventMask = $20;
	kURLSystemEventMask			= $10000000;
	kURLPercentEventMask		= $20000000;
	kURLPeriodicEventMask		= $40000000;
	kURLPropertyChangedEventMask = $80000000;
	kURLAllBufferEventsMask		= $00100020;
	kURLAllNonBufferEventsMask	= $E00003D1;
	kURLAllEventsMask			= $FFFFFFFF;


type
	URLCallbackInfoPtr = ^URLCallbackInfo;
	URLCallbackInfo = record
		version:				UInt32;
		urlRef:					URLReference;
		proprty:				ConstCStringPtr;
		currentSize:			UInt32;
		systemEvent:			EventRecordPtr;
	end;


	{  authentication type flags }

const
	kUserNameAndPasswordFlag	= $00000001;

	kURLURL						= 'URLString';
	kURLResourceSize			= 'URLResourceSize';
	kURLLastModifiedTime		= 'URLLastModifiedTime';
	kURLMIMEType				= 'URLMIMEType';
	kURLFileType				= 'URLFileType';
	kURLFileCreator				= 'URLFileCreator';
	kURLCharacterSet			= 'URLCharacterSet';
	kURLResourceName			= 'URLResourceName';
	kURLHost					= 'URLHost';
	kURLAuthType				= 'URLAuthType';
	kURLUserName				= 'URLUserName';
	kURLPassword				= 'URLPassword';
	kURLStatusString			= 'URLStatusString';
	kURLIsSecure				= 'URLIsSecure';
	kURLCertificate				= 'URLCertificate';
	kURLTotalItems				= 'URLTotalItems';
	kURLConnectTimeout			= 'URLConnectTimeout';
	{  http and https properties }
	kURLHTTPRequestMethod		= 'URLHTTPRequestMethod';
	kURLHTTPRequestHeader		= 'URLHTTPRequestHeader';
	kURLHTTPRequestBody			= 'URLHTTPRequestBody';
	kURLHTTPRespHeader			= 'URLHTTPRespHeader';
	kURLHTTPUserAgent			= 'URLHTTPUserAgent';
	kURLHTTPRedirectedURL		= 'URLHTTPRedirectedURL';
	kURLSSLCipherSuite			= 'URLSSLCipherSuite';


	{
	 *  URLGetURLAccessVersion()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in URLAccessLib 1.0 and later
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
function URLGetURLAccessVersion(var returnVers: UInt32): OSStatus; external name '_URLGetURLAccessVersion';


{$ifc TARGET_RT_MAC_CFM}
{
        URLAccessAvailable() is a macro/inline available only in C/C++.  
        To get the same functionality from pascal or assembly, you need
        to test if URLGetURLAccessVersion function is not NULL.  For instance:
        
            gURLAccessAvailable = FALSE;
            IF @URLAccessAvailable <> kUnresolvedCFragSymbolAddress THEN
                gURLAccessAvailable = TRUE;
            end
    
    }
{$elsec}
  {$ifc TARGET_RT_MAC_MACHO}
{ URL Access is always available on OS X }
  {$endc}
{$endc}


type
{$ifc TYPED_FUNCTION_POINTERS}
	URLNotifyProcPtr = function(userContext: UnivPtr; event: URLEvent; var callbackInfo: URLCallbackInfo): OSStatus;
{$elsec}
	URLNotifyProcPtr = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	URLSystemEventProcPtr = function(userContext: UnivPtr; var event: EventRecord): OSStatus;
{$elsec}
	URLSystemEventProcPtr = ProcPtr;
{$endc}

{$ifc OPAQUE_UPP_TYPES}
	URLNotifyUPP = ^SInt32; { an opaque UPP }
{$elsec}
	URLNotifyUPP = URLNotifyProcPtr;
{$endc}	
{$ifc OPAQUE_UPP_TYPES}
	URLSystemEventUPP = ^SInt32; { an opaque UPP }
{$elsec}
	URLSystemEventUPP = URLSystemEventProcPtr;
{$endc}	

const
	uppURLNotifyProcInfo = $00000FF0;
	uppURLSystemEventProcInfo = $000003F0;
	{
	 *  NewURLNotifyUPP()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   available as macro/inline
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
function NewURLNotifyUPP(userRoutine: URLNotifyProcPtr): URLNotifyUPP; external name '_NewURLNotifyUPP';
{
 *  NewURLSystemEventUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewURLSystemEventUPP(userRoutine: URLSystemEventProcPtr): URLSystemEventUPP; external name '_NewURLSystemEventUPP';
{
 *  DisposeURLNotifyUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeURLNotifyUPP(userUPP: URLNotifyUPP); external name '_DisposeURLNotifyUPP';
{
 *  DisposeURLSystemEventUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeURLSystemEventUPP(userUPP: URLSystemEventUPP); external name '_DisposeURLSystemEventUPP';
{
 *  InvokeURLNotifyUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function InvokeURLNotifyUPP(userContext: UnivPtr; event: URLEvent; var callbackInfo: URLCallbackInfo; userRoutine: URLNotifyUPP): OSStatus; external name '_InvokeURLNotifyUPP';
{
 *  InvokeURLSystemEventUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function InvokeURLSystemEventUPP(userContext: UnivPtr; var event: EventRecord; userRoutine: URLSystemEventUPP): OSStatus; external name '_InvokeURLSystemEventUPP';
{
 *  URLSimpleDownload()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in URLAccessLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function URLSimpleDownload(url: ConstCStringPtr; destination: FSSpecPtr; destinationHandle: Handle; openFlags: URLOpenFlags; eventProc: URLSystemEventUPP; userContext: UnivPtr): OSStatus; external name '_URLSimpleDownload';

{
 *  URLDownload()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in URLAccessLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function URLDownload(urlRef: URLReference; destination: FSSpecPtr; destinationHandle: Handle; openFlags: URLOpenFlags; eventProc: URLSystemEventUPP; userContext: UnivPtr): OSStatus; external name '_URLDownload';

{
 *  URLSimpleUpload()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in URLAccessLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function URLSimpleUpload(url: ConstCStringPtr; const (*var*) source: FSSpec; openFlags: URLOpenFlags; eventProc: URLSystemEventUPP; userContext: UnivPtr): OSStatus; external name '_URLSimpleUpload';

{
 *  URLUpload()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in URLAccessLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function URLUpload(urlRef: URLReference; const (*var*) source: FSSpec; openFlags: URLOpenFlags; eventProc: URLSystemEventUPP; userContext: UnivPtr): OSStatus; external name '_URLUpload';

{
 *  URLNewReference()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in URLAccessLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function URLNewReference(url: ConstCStringPtr; var urlRef: URLReference): OSStatus; external name '_URLNewReference';

{
 *  URLDisposeReference()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in URLAccessLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function URLDisposeReference(urlRef: URLReference): OSStatus; external name '_URLDisposeReference';

{
 *  URLOpen()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in URLAccessLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function URLOpen(urlRef: URLReference; fileSpec: FSSpecPtr; openFlags: URLOpenFlags; notifyProc: URLNotifyUPP; eventRegister: URLEventMask; userContext: UnivPtr): OSStatus; external name '_URLOpen';

{
 *  URLAbort()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in URLAccessLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function URLAbort(urlRef: URLReference): OSStatus; external name '_URLAbort';

{
 *  URLGetDataAvailable()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in URLAccessLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function URLGetDataAvailable(urlRef: URLReference; var dataSize: Size): OSStatus; external name '_URLGetDataAvailable';

{
 *  URLGetBuffer()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in URLAccessLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function URLGetBuffer(urlRef: URLReference; var buffer: UnivPtr; var bufferSize: Size): OSStatus; external name '_URLGetBuffer';

{
 *  URLReleaseBuffer()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in URLAccessLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function URLReleaseBuffer(urlRef: URLReference; buffer: UnivPtr): OSStatus; external name '_URLReleaseBuffer';

{
 *  URLGetProperty()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in URLAccessLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function URLGetProperty(urlRef: URLReference; proprty: ConstCStringPtr; propertyBuffer: UnivPtr; bufferSize: Size): OSStatus; external name '_URLGetProperty';

{
 *  URLGetPropertySize()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in URLAccessLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function URLGetPropertySize(urlRef: URLReference; proprty: ConstCStringPtr; var propertySize: Size): OSStatus; external name '_URLGetPropertySize';

{
 *  URLSetProperty()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in URLAccessLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function URLSetProperty(urlRef: URLReference; proprty: ConstCStringPtr; propertyBuffer: UnivPtr; bufferSize: Size): OSStatus; external name '_URLSetProperty';

{
 *  URLGetCurrentState()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in URLAccessLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function URLGetCurrentState(urlRef: URLReference; var state: URLState): OSStatus; external name '_URLGetCurrentState';

{
 *  URLGetError()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in URLAccessLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function URLGetError(urlRef: URLReference; var urlError: OSStatus): OSStatus; external name '_URLGetError';

{
 *  URLIdle()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in URLAccessLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function URLIdle: OSStatus; external name '_URLIdle';

{
 *  URLGetFileInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in URLAccessLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function URLGetFileInfo(fName: StringPtr; var fType: OSType; var fCreator: OSType): OSStatus; external name '_URLGetFileInfo';

{$ALIGN MAC68K}


end.
