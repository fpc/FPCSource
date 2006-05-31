{
     File:       PMCore.p
 
     Contains:   Carbon Printing Manager Interfaces.
 
     Version:    Technology: Mac OS X
                 Release:    Universal Interfaces 3.4.2
 
     Copyright:  © 1998-2002 by Apple Computer, Inc., all rights reserved
 
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

unit PMCore;
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
uses MacTypes,CFBase,CFArray,MacErrors,Files,CFString,CFURL,Quickdraw,CMApplication,PMDefinitions;


{$ALIGN MAC68K}

{$ifc undefined PM_USE_SESSION_APIS}
{$setc PM_USE_SESSION_APIS := 1}
{$endc}

{ Callbacks }

type
{$ifc TYPED_FUNCTION_POINTERS}
	PMIdleProcPtr = procedure;
{$elsec}
	PMIdleProcPtr = ProcPtr;
{$endc}

{$ifc OPAQUE_UPP_TYPES}
	PMIdleUPP = ^SInt32; { an opaque UPP }
{$elsec}
	PMIdleUPP = UniversalProcPtr;
{$endc}	

const
	uppPMIdleProcInfo = $00000000;
	{
	 *  NewPMIdleUPP()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   not available
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
function NewPMIdleUPP(userRoutine: PMIdleProcPtr): PMIdleUPP; external name '_NewPMIdleUPP';
{
 *  DisposePMIdleUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposePMIdleUPP(userUPP: PMIdleUPP); external name '_DisposePMIdleUPP';
{
 *  InvokePMIdleUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure InvokePMIdleUPP(userRoutine: PMIdleUPP); external name '_InvokePMIdleUPP';
{$ifc PM_USE_SESSION_APIS}
{ Session routines }
{ Session support }
{
 *  PMRetain()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMRetain(objct: PMObject): OSStatus; external name '_PMRetain';

{
 *  PMRelease()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMRelease(objct: PMObject): OSStatus; external name '_PMRelease';

{ Session Print loop }
{**********************}
{ A session is created with a refcount of 1. }
{**********************}
{
 *  PMCreateSession()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMCreateSession(var printSession: PMPrintSession): OSStatus; external name '_PMCreateSession';

{ Session PMPageFormat }
{**********************}
{ A pageformat is created with a refcount of 1. }
{**********************}
{
 *  PMCreatePageFormat()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMCreatePageFormat(var pageFormat: PMPageFormat): OSStatus; external name '_PMCreatePageFormat';

{
 *  PMSessionDefaultPageFormat()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMSessionDefaultPageFormat(printSession: PMPrintSession; pageFormat: PMPageFormat): OSStatus; external name '_PMSessionDefaultPageFormat';

{
 *  PMSessionValidatePageFormat()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMSessionValidatePageFormat(printSession: PMPrintSession; pageFormat: PMPageFormat; result: BooleanPtr): OSStatus; external name '_PMSessionValidatePageFormat';

{ Session PMPrintSettings }
{**********************}
{ A printSettings is created with a refcount of 1. }
{**********************}
{
 *  PMCreatePrintSettings()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMCreatePrintSettings(var printSettings: PMPrintSettings): OSStatus; external name '_PMCreatePrintSettings';

{
 *  PMSessionDefaultPrintSettings()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMSessionDefaultPrintSettings(printSession: PMPrintSession; printSettings: PMPrintSettings): OSStatus; external name '_PMSessionDefaultPrintSettings';

{
 *  PMSessionValidatePrintSettings()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMSessionValidatePrintSettings(printSession: PMPrintSession; printSettings: PMPrintSettings; result: BooleanPtr): OSStatus; external name '_PMSessionValidatePrintSettings';

{ Session Classic support }
{
 *  PMSessionGeneral()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMSessionGeneral(printSession: PMPrintSession; pData: Ptr): OSStatus; external name '_PMSessionGeneral';

{
 *  PMSessionConvertOldPrintRecord()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMSessionConvertOldPrintRecord(printSession: PMPrintSession; printRecordHandle: Handle; var printSettings: PMPrintSettings; var pageFormat: PMPageFormat): OSStatus; external name '_PMSessionConvertOldPrintRecord';

{
 *  PMSessionMakeOldPrintRecord()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMSessionMakeOldPrintRecord(printSession: PMPrintSession; printSettings: PMPrintSettings; pageFormat: PMPageFormat; var printRecordHandle: Handle): OSStatus; external name '_PMSessionMakeOldPrintRecord';

{ Session Driver Information }
{
 *  PMPrinterGetDescriptionURL()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMPrinterGetDescriptionURL(printer: PMPrinter; descriptionType: CFStringRef; var fileURL: CFURLRef): OSStatus; external name '_PMPrinterGetDescriptionURL';

{
 *  PMSessionGetCurrentPrinter()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMSessionGetCurrentPrinter(printSession: PMPrintSession; var currentPrinter: PMPrinter): OSStatus; external name '_PMSessionGetCurrentPrinter';

{
 *  PMPrinterGetLanguageInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMPrinterGetLanguageInfo(printer: PMPrinter; var info: PMLanguageInfo): OSStatus; external name '_PMPrinterGetLanguageInfo';

{
 *  PMPrinterGetDriverCreator()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMPrinterGetDriverCreator(printer: PMPrinter; var creator: OSType): OSStatus; external name '_PMPrinterGetDriverCreator';

{
 *  PMPrinterGetDriverReleaseInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMPrinterGetDriverReleaseInfo(printer: PMPrinter; var release: VersRec): OSStatus; external name '_PMPrinterGetDriverReleaseInfo';

{
 *  PMPrinterGetPrinterResolutionCount()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMPrinterGetPrinterResolutionCount(printer: PMPrinter; var count: UInt32): OSStatus; external name '_PMPrinterGetPrinterResolutionCount';

{
 *  PMPrinterGetPrinterResolution()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMPrinterGetPrinterResolution(printer: PMPrinter; tag: PMTag; var res: PMResolution): OSStatus; external name '_PMPrinterGetPrinterResolution';

{
 *  PMPrinterGetIndexedPrinterResolution()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMPrinterGetIndexedPrinterResolution(printer: PMPrinter; index: UInt32; var res: PMResolution): OSStatus; external name '_PMPrinterGetIndexedPrinterResolution';

{
 *  PMPrinterIsPostScriptCapable()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.2 and later
 }
function PMPrinterIsPostScriptCapable(printer: PMPrinter): boolean; external name '_PMPrinterIsPostScriptCapable';

{
 *  PMPrinterGetMakeAndModelName()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.2 and later
 }
function PMPrinterGetMakeAndModelName(printer: PMPrinter; var makeAndModel: CFStringRef): OSStatus; external name '_PMPrinterGetMakeAndModelName';

{ Session ColorSync & PostScript Support }
{
 *  PMSessionEnableColorSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMSessionEnableColorSync(printSession: PMPrintSession): OSStatus; external name '_PMSessionEnableColorSync';

{
 *  PMSessionDisableColorSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMSessionDisableColorSync(printSession: PMPrintSession): OSStatus; external name '_PMSessionDisableColorSync';

{
 *  PMSessionPostScriptBegin()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMSessionPostScriptBegin(printSession: PMPrintSession): OSStatus; external name '_PMSessionPostScriptBegin';

{
 *  PMSessionPostScriptEnd()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMSessionPostScriptEnd(printSession: PMPrintSession): OSStatus; external name '_PMSessionPostScriptEnd';

{
 *  PMSessionPostScriptHandle()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMSessionPostScriptHandle(printSession: PMPrintSession; psHandle: Handle): OSStatus; external name '_PMSessionPostScriptHandle';

{
 *  PMSessionPostScriptData()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMSessionPostScriptData(printSession: PMPrintSession; psPtr: Ptr; len: Size): OSStatus; external name '_PMSessionPostScriptData';

{
 *  PMSessionPostScriptFile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMSessionPostScriptFile(printSession: PMPrintSession; var psFile: FSSpec): OSStatus; external name '_PMSessionPostScriptFile';

{
 *  PMSessionSetPSInjectionData()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMSessionSetPSInjectionData(printSession: PMPrintSession; printSettings: PMPrintSettings; injectionDictArray: CFArrayRef): OSStatus; external name '_PMSessionSetPSInjectionData';

{ Session Error }
{
 *  PMSessionError()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMSessionError(printSession: PMPrintSession): OSStatus; external name '_PMSessionError';

{
 *  PMSessionSetError()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMSessionSetError(printSession: PMPrintSession; printError: OSStatus): OSStatus; external name '_PMSessionSetError';

{ Other Session routines }
{
 *  PMSessionGetDocumentFormatGeneration()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMSessionGetDocumentFormatGeneration(printSession: PMPrintSession; var docFormats: CFArrayRef): OSStatus; external name '_PMSessionGetDocumentFormatGeneration';

{
 *  PMSessionSetDocumentFormatGeneration()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMSessionSetDocumentFormatGeneration(printSession: PMPrintSession; docFormat: CFStringRef; graphicsContextTypes: CFArrayRef; options: CFTypeRef): OSStatus; external name '_PMSessionSetDocumentFormatGeneration';

{
 *  PMSessionGetDocumentFormatSupported()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMSessionGetDocumentFormatSupported(printSession: PMPrintSession; var docFormats: CFArrayRef; limit: UInt32): OSStatus; external name '_PMSessionGetDocumentFormatSupported';

{
 *  PMSessionIsDocumentFormatSupported()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMSessionIsDocumentFormatSupported(printSession: PMPrintSession; docFormat: CFStringRef; var supported: boolean): OSStatus; external name '_PMSessionIsDocumentFormatSupported';

{
 *  PMSessionGetGraphicsContext()
 *  
 *  Parameters:
 *    
 *    printSession:
 *      the session
 *    
 *    graphicsContextType:
 *      either kPMGraphicsContextQuickdraw or
 *      kPMGraphicsContextCoreGraphics
 *    
 *    graphicsContext:
 *      returns a GrafPtr or a CGContextRef
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMSessionGetGraphicsContext(printSession: PMPrintSession; graphicsContextType: CFStringRef; var graphicsContext: UnivPtr): OSStatus; external name '_PMSessionGetGraphicsContext';

{
 *  PMSessionSetIdleProc()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMSessionSetIdleProc(printSession: PMPrintSession; idleProc: PMIdleUPP): OSStatus; external name '_PMSessionSetIdleProc';

{
 *  PMSessionSetDataInSession()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMSessionSetDataInSession(printSession: PMPrintSession; key: CFStringRef; data: CFTypeRef): OSStatus; external name '_PMSessionSetDataInSession';

{
 *  PMSessionGetDataFromSession()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMSessionGetDataFromSession(printSession: PMPrintSession; key: CFStringRef; var data: CFTypeRef): OSStatus; external name '_PMSessionGetDataFromSession';

{
 *  PMSessionCreatePrinterList()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.4 and later
 *    Mac OS X:         in version 10.1 and later
 }
function PMSessionCreatePrinterList(printSession: PMPrintSession; var printerList: CFArrayRef; var currentIndex: CFIndex; var currentPrinter: PMPrinter): OSStatus; external name '_PMSessionCreatePrinterList';

{
 *  PMSessionSetCurrentPrinter()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.4 and later
 *    Mac OS X:         in version 10.1 and later
 }
function PMSessionSetCurrentPrinter(session: PMPrintSession; printerName: CFStringRef): OSStatus; external name '_PMSessionSetCurrentPrinter';

{
 *  PMSessionSetDestination()
 *  
 *  Summary:
 *    Alter a print session and print settings so that an associated
 *    print job is sent to the provided destination type in the,
 *    optional, MIME document format.
 *  
 *  Discussion:
 *    This function is most useful when an application would like to
 *    write its print output to disk without requiring user
 *    interaction. The list of MIME types that can be sent to the
 *    provided destination can be obtained from
 *    PMSessionCopyOutputFormatList and one of these passed to this
 *    function.
 *  
 *  Parameters:
 *    
 *    printSession:
 *      The session to be used for a print job. The session holds the
 *      preview setting which can override the destination type in the
 *      print settings.
 *    
 *    printSettings:
 *      The print settings to be used for a print job. The print
 *      settings specify whether a job will be directed toward a
 *      printer or to file. It also holds the requested MIME output
 *      type.
 *    
 *    destType:
 *      The destiation type for a print job associated with the
 *      provided print session and print settings. Fax is currently not
 *      supported, but kPMDestinationPrinter, kPMDestinationFile, and
 *      kPMDestinationPreview can be set.
 *    
 *    destFormat:
 *      The MIME type to be generated for the provided destination
 *      type. This parameter can be NULL in which the default format
 *      for the requested destination type is used. To obtain a list of
 *      valid formats for a given destiation type, use the function
 *      PMSessionCopyOutputFormatList.
 *    
 *    destLocation:
 *      Some destination types support a destination location. The
 *      clearest example is the kPMDestinationFile destination type
 *      which allows a caller to also supply a file URL specifying
 *      where the output file is to be created.
 *    
 *    SPECIAL_AVAILABILITY_NOTE:
 *      This routine is available in ApplicationsServices.framework in
 *      Mac OS X version 10.1 and later. On Mac OS X it is available to
 *      CFM applications through CarbonLib starting with Mac OS X
 *      version 10.2 and later.
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.5 and later
 *    Mac OS X:         in version 10.1 and later
 }
function PMSessionSetDestination(printSession: PMPrintSession; printSettings: PMPrintSettings; destType: PMDestinationType; destFormat: CFStringRef; destLocation: CFURLRef): OSStatus; external name '_PMSessionSetDestination';

{
 *  PMSessionGetDestinationType()
 *  
 *  Summary:
 *    Hand back the destination type that will be used for a print job
 *    with the specified print settings and print session.
 *  
 *  Discussion:
 *    Currently there are four destination types:
 *    kPMDestinationPrinter, kPMDestinationFile, kPMDestinationFax and
 *    kPMDestinationPreview. The first three destination types are
 *    stored in the print settings. The switch for preview is stored in
 *    the print session and, if enabled, overrides the destination in
 *    the print setting. This function is preferred over
 *    PMGetDestination as the latter does not take a print session
 *    parameter and therefore can not indicate whether preview has been
 *    selected as the destination.
 *  
 *  Parameters:
 *    
 *    printSession:
 *      The session to be used for a print job. The session holds the
 *      preview setting which can override the destination type in the
 *      print settings.
 *    
 *    printSettings:
 *      The print settings to be used for a print job. The print
 *      settings specify whether a job will be directed toward a
 *      printer or to file.
 *    
 *    destTypeP:
 *      A pointer to a caller supplied PMDestinationType variable. If
 *      this function succeeds then *'destTypeP' will be filled in with
 *      the destination type for a print job that used the specified
 *      session and print settings. If this function fails, then
 *      *'destType' will be set to kPMDestinationInvalid.
 *    
 *    SPECIAL_AVAILABILITY_NOTE:
 *      This routine is available in ApplicationsServices.framework in
 *      Mac OS X version 10.1 and later. On Mac OS X it is available to
 *      CFM applications through CarbonLib starting with Mac OS X
 *      version 10.2 and later.
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.5 and later
 *    Mac OS X:         in version 10.1 and later
 }
function PMSessionGetDestinationType(printSession: PMPrintSession; printSettings: PMPrintSettings; var destTypeP: PMDestinationType): OSStatus; external name '_PMSessionGetDestinationType';

{
 *  PMSessionCopyDestinationFormat()
 *  
 *  Summary:
 *    Hand back the destination output MIME type associated with the
 *    provided print session and print settings.
 *  
 *  Parameters:
 *    
 *    printSession:
 *      A currently open print session.
 *    
 *    printSettings:
 *      The print settings that are to be searched.
 *    
 *    destFormatP:
 *      A pointer to a caller allocated CFStringRef variable. If this
 *      routine returns noErr then *'destFormatP' will either be a copy
 *      of a CFStringRef specifying the output format for the print
 *      job, or NULL indicating that the default output format will be
 *      used. If this function return an error, then *'destFormatP'
 *      will be set to NULL.
 *    
 *    SPECIAL_AVAILABILITY_NOTE:
 *      This routine is available in ApplicationsServices.framework in
 *      Mac OS X version 10.1 and later. On Mac OS X it is available to
 *      CFM applications through CarbonLib starting with Mac OS X
 *      version 10.2 and later.
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.5 and later
 *    Mac OS X:         in version 10.1 and later
 }
function PMSessionCopyDestinationFormat(printSession: PMPrintSession; printSettings: PMPrintSettings; var destFormatP: CFStringRef): OSStatus; external name '_PMSessionCopyDestinationFormat';

{
 *  PMSessionCopyDestinationLocation()
 *  
 *  Summary:
 *    Hand back the URL destination location given a print session and
 *    print settings.
 *  
 *  Discussion:
 *    Some destination type support a destination location which
 *    further defines where the output from a pritn job should be sent.
 *    The kPMDestinationFile destiation type, for example, will use a
 *    file URL to determine where a new file should be created.
 *  
 *  Parameters:
 *    
 *    printSession:
 *      A currently open print session.
 *    
 *    printSettings:
 *      The print settings that are to be searched.
 *    
 *    destLocationP:
 *      A pointer to a caller allocated CFURLRef variable. If this
 *      routine returns noErr then *'outputFileP' will either be NULL
 *      indicating that the job is using the default destination
 *      location for the current destination type or a copy of a
 *      CFURLRef will be placed in *'destLocationP'. If this function
 *      returns an error then 'destLocationP' will be set to NULL.
 *    
 *    SPECIAL_AVAILABILITY_NOTE:
 *      This routine is available in ApplicationsServices.framework in
 *      Mac OS X version 10.1 and later. On Mac OS X it is available to
 *      CFM applications through CarbonLib starting with Mac OS X
 *      version 10.2 and later.
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.5 and later
 *    Mac OS X:         in version 10.1 and later
 }
function PMSessionCopyDestinationLocation(printSession: PMPrintSession; printSettings: PMPrintSettings; var destLocationP: CFURLRef): OSStatus; external name '_PMSessionCopyDestinationLocation';

{
 *  PMSessionCopyOutputFormatList()
 *  
 *  Summary:
 *    Hands back an an array of MIME types describing the possible
 *    output formats for the printer module associated with the current
 *    printer.
 *  
 *  Parameters:
 *    
 *    printSession:
 *      This session's current printer's printer module will be queried
 *      for its supported output MIME types.
 *    
 *    destType:
 *      A print job can have one of several possible destination types.
 *      The list of valid output formats is dependent upon the
 *      destination type. This parameter specifies destination type of
 *      interest when retrieving the output formats list.
 *    
 *    documentFormatP:
 *      A pointer to a caller's CFArrayRef variable. If this routine
 *      completes successfully, then *'documentFormatP' will be set to
 *      a CFArrayRef containing CFStringRefs. Each CFStringRef in the
 *      array is a MIME type specifying a type of output that can be
 *      generated by the printer module associated with the current
 *      printer.
 *    
 *    SPECIAL_AVAILABILITY_NOTE:
 *      This routine is available in ApplicationsServices.framework in
 *      Mac OS X version 10.1 and later. On Mac OS X it is available to
 *      CFM applications through CarbonLib starting with Mac OS X
 *      version 10.2 and later. On Mac OS 8/9 using CarbonLib, this
 *      routine returns kPMNotImplemented
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.1 and later
 }
function PMSessionCopyOutputFormatList(printSession: PMPrintSession; destType: PMDestinationType; var documentFormatP: CFArrayRef): OSStatus; external name '_PMSessionCopyOutputFormatList';


{
 *  PMSessionCreatePageFormatList()
 *  
 *  Summary:
 *    Hand back a list of page format instances. Each page format
 *    instance describes a paper size available on the specified
 *    printer.
 *  
 *  Parameters:
 *    
 *    printSession:
 *      A currently open print session.
 *    
 *    printer:
 *      The printer whose page size list should be enumerated. To get
 *      the session's current printer, see PMSessionGetCurrentPrinter().
 *    
 *    pageFormatList:
 *      If this function is successful then noErr will be returned and
 *      *'pageFormatList' will be set to a newly created CFArray. Each
 *      element in the array will be a PMPageFormat describing an
 *      available paper size for the specified printer. If this
 *      function fails then a non-zero error code will be returned and
 *      *'pageFormatList' will be set to NULL.
 *    
 *    SPECIAL_AVAILABILITY_NOTE:
 *      This routine is available in ApplicationsServices.framework in
 *      Mac OS X version 10.1 and later. On Mac OS X it is available to
 *      CFM applications through CarbonLib starting with Mac OS X
 *      version 10.2 and later. On Mac OS 8/9 using CarbonLib, this
 *      routine returns kPMNotImplemented
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.1 and later
 }
function PMSessionCreatePageFormatList(printSession: PMPrintSession; printer: PMPrinter; var pageFormatList: CFArrayRef): OSStatus; external name '_PMSessionCreatePageFormatList';

{
 * SPECIAL AVAILABILITY note: This routine is available in ApplicationsServices.framework in
 * Mac OS X version 10.0 and later. On Mac OS X it is available to CFM applications through CarbonLib
 * starting with Mac OS X version 10.2 and later.
 *
 * On Mac OS 8/9 using CarbonLib, this routine returns kPMNotImplemented
 }
{
 *  PMSessionBeginDocumentNoDialog()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMSessionBeginDocumentNoDialog(printSession: PMPrintSession; printSettings: PMPrintSettings; pageFormat: PMPageFormat): OSStatus; external name '_PMSessionBeginDocumentNoDialog';

{
 * SPECIAL AVAILABILITY note: This routine is available in ApplicationsServices.framework in
 * Mac OS X version 10.0 and later. On Mac OS X it is available to CFM applications through CarbonLib
 * starting with Mac OS X version 10.2 and later.
 *
 * On Mac OS 8/9 using CarbonLib, this routine returns kPMNotImplemented
 }
{
 *  PMSessionEndDocumentNoDialog()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMSessionEndDocumentNoDialog(printSession: PMPrintSession): OSStatus; external name '_PMSessionEndDocumentNoDialog';

{
 * SPECIAL AVAILABILITY note: This routine is available in ApplicationsServices.framework in
 * Mac OS X version 10.0 and later. On Mac OS X it is available to CFM applications through CarbonLib
 * starting with Mac OS X version 10.2 and later.
 *
 * On Mac OS 8/9 using CarbonLib, this routine returns kPMNotImplemented
 }
{
 *  PMSessionBeginPageNoDialog()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMSessionBeginPageNoDialog(printSession: PMPrintSession; pageFormat: PMPageFormat; pageFrame: PMRectPtr): OSStatus; external name '_PMSessionBeginPageNoDialog';

{
 * SPECIAL AVAILABILITY note: This routine is available in ApplicationsServices.framework in
 * Mac OS X version 10.0 and later. On Mac OS X it is available to CFM applications through CarbonLib
 * starting with Mac OS X version 10.2 and later.
 *
 * On Mac OS 8/9 using CarbonLib, this routine returns kPMNotImplemented
 }
{
 *  PMSessionEndPageNoDialog()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMSessionEndPageNoDialog(printSession: PMPrintSession): OSStatus; external name '_PMSessionEndPageNoDialog';

{$elsec}
{
 *  PMSetIdleProc()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMSetIdleProc(idleProc: PMIdleUPP): OSStatus; external name '_PMSetIdleProc';

{ Print loop }
{
 *  PMBegin()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMBegin: OSStatus; external name '_PMBegin';

{
 *  PMEnd()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMEnd: OSStatus; external name '_PMEnd';

{**********************}
{  Valid only within a PMBeginPage/PMEndPage block. You should retrieve the printing }
{  port with this call and set it before imaging a page. }
{**********************}
{
 *  PMGetGrafPtr()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMGetGrafPtr(printContext: PMPrintContext; var grafPort: GrafPtr): OSStatus; external name '_PMGetGrafPtr';

{ PMPageFormat }
{
 *  PMNewPageFormat()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMNewPageFormat(var pageFormat: PMPageFormat): OSStatus; external name '_PMNewPageFormat';

{
 *  PMDisposePageFormat()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMDisposePageFormat(pageFormat: PMPageFormat): OSStatus; external name '_PMDisposePageFormat';

{
 *  PMDefaultPageFormat()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMDefaultPageFormat(pageFormat: PMPageFormat): OSStatus; external name '_PMDefaultPageFormat';

{
 *  PMValidatePageFormat()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMValidatePageFormat(pageFormat: PMPageFormat; var result: boolean): OSStatus; external name '_PMValidatePageFormat';

{ PMPrintSettings }
{
 *  PMNewPrintSettings()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMNewPrintSettings(var printSettings: PMPrintSettings): OSStatus; external name '_PMNewPrintSettings';

{
 *  PMDisposePrintSettings()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMDisposePrintSettings(printSettings: PMPrintSettings): OSStatus; external name '_PMDisposePrintSettings';

{
 *  PMDefaultPrintSettings()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMDefaultPrintSettings(printSettings: PMPrintSettings): OSStatus; external name '_PMDefaultPrintSettings';

{
 *  PMValidatePrintSettings()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMValidatePrintSettings(printSettings: PMPrintSettings; var result: boolean): OSStatus; external name '_PMValidatePrintSettings';

{ Classic Support }
{
 *  PMGeneral()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMGeneral(pData: Ptr): OSStatus; external name '_PMGeneral';

{
 *  PMConvertOldPrintRecord()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMConvertOldPrintRecord(printRecordHandle: Handle; var printSettings: PMPrintSettings; var pageFormat: PMPageFormat): OSStatus; external name '_PMConvertOldPrintRecord';

{
 *  PMMakeOldPrintRecord()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMMakeOldPrintRecord(printSettings: PMPrintSettings; pageFormat: PMPageFormat; var printRecordHandle: Handle): OSStatus; external name '_PMMakeOldPrintRecord';

{ Driver Information }
{
 *  PMIsPostScriptDriver()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMIsPostScriptDriver(var isPostScript: boolean): OSStatus; external name '_PMIsPostScriptDriver';

{
 *  PMGetLanguageInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMGetLanguageInfo(var info: PMLanguageInfo): OSStatus; external name '_PMGetLanguageInfo';

{
 *  PMGetDriverCreator()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMGetDriverCreator(var creator: OSType): OSStatus; external name '_PMGetDriverCreator';

{
 *  PMGetDriverReleaseInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMGetDriverReleaseInfo(var release: VersRec): OSStatus; external name '_PMGetDriverReleaseInfo';

{
 *  PMGetPrinterResolutionCount()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMGetPrinterResolutionCount(var count: UInt32): OSStatus; external name '_PMGetPrinterResolutionCount';

{
 *  PMGetPrinterResolution()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMGetPrinterResolution(tag: PMTag; var res: PMResolution): OSStatus; external name '_PMGetPrinterResolution';

{
 *  PMGetIndexedPrinterResolution()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMGetIndexedPrinterResolution(index: UInt32; var res: PMResolution): OSStatus; external name '_PMGetIndexedPrinterResolution';

{**********************}
{  PMEnableColorSync and PMDisableColorSync are valid within }
{  BeginPage/EndPage block }
{**********************}
{ ColorSync & PostScript Support }
{
 *  PMEnableColorSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMEnableColorSync: OSStatus; external name '_PMEnableColorSync';

{
 *  PMDisableColorSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMDisableColorSync: OSStatus; external name '_PMDisableColorSync';

{**********************}
{  The PMPostScriptxxx calls are valid within a }
{  BeginPage/EndPage block }
{**********************}
{
 *  PMPostScriptBegin()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMPostScriptBegin: OSStatus; external name '_PMPostScriptBegin';

{
 *  PMPostScriptEnd()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMPostScriptEnd: OSStatus; external name '_PMPostScriptEnd';

{**********************}
{  These PMPostScriptxxx calls are valid within a }
{  PMPostScriptBegin/PMPostScriptEnd block }
{**********************}
{
 *  PMPostScriptHandle()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMPostScriptHandle(psHandle: Handle): OSStatus; external name '_PMPostScriptHandle';

{
 *  PMPostScriptData()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMPostScriptData(psPtr: Ptr; len: Size): OSStatus; external name '_PMPostScriptData';

{
 *  PMPostScriptFile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMPostScriptFile(var psFile: FSSpec): OSStatus; external name '_PMPostScriptFile';

{ Error }
{
 *  PMError()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMError: OSStatus; external name '_PMError';

{
 *  PMSetError()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMSetError(printError: OSStatus): OSStatus; external name '_PMSetError';

{$endc}  {PM_USE_SESSION_APIS}

{ PMPageFormat }
{
 *  PMCopyPageFormat()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMCopyPageFormat(formatSrc: PMPageFormat; formatDest: PMPageFormat): OSStatus; external name '_PMCopyPageFormat';

{**********************}
{  Flattening a page format should only be necessary if you intend to preserve }
{  the object settings along with a document. A page format will persist outside of a }
{  PMBegin/PMEnd block. This will allow you to use any accessors on the object without }
{  the need to flatten and unflatten. Keep in mind accessors make no assumption }
{  on the validity of the value you set. This can only be done thru PMValidatePageFormat }
{  in a PMBegin/PMEnd block or with PMSessionValidatePageFormat with a valid session. }
{  It is your responsibility for disposing of the handle. }
{**********************}
{
 *  PMFlattenPageFormat()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMFlattenPageFormat(pageFormat: PMPageFormat; var flatFormat: Handle): OSStatus; external name '_PMFlattenPageFormat';

{
 *  PMUnflattenPageFormat()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMUnflattenPageFormat(flatFormat: Handle; var pageFormat: PMPageFormat): OSStatus; external name '_PMUnflattenPageFormat';

{ PMPageFormat Accessors }
{**********************}
{ PMSetxxx calls only saves the value inside the printing object. They make no assumption on the }
{ validity of the value. This should be done using PMValidatePageFormat/PMSessionValidatePageFormat }
{ Any dependant settings are also updated during a validate call. }
{ For example: }
{ PMGetAdjustedPaperRect - returns a rect of a certain size }
{ PMSetScale( aPageFormat, 500.0 )  }
{ PMGetAdjustedPaperRect - returns the SAME rect as the first call  }
{}
{ PMGetAdjustedPaperRect - returns a rect of a certain size }
{ PMSetScale( aPageFormat, 500.0 ) }
{ PMValidatePageFormat or PMSessionValidatePageFormat }
{ PMGetAdjustedPaperRect - returns a rect thats scaled 500% from the first call }
{**********************}
{
 *  PMGetPageFormatExtendedData()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMGetPageFormatExtendedData(pageFormat: PMPageFormat; dataID: OSType; var size: UInt32; extendedData: UnivPtr): OSStatus; external name '_PMGetPageFormatExtendedData';

{
 *  PMSetPageFormatExtendedData()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMSetPageFormatExtendedData(pageFormat: PMPageFormat; dataID: OSType; size: UInt32; extendedData: UnivPtr): OSStatus; external name '_PMSetPageFormatExtendedData';

{**********************}
{  A value of 100.0 means 100% (no scaling). 50.0 means 50% scaling }
{**********************}
{
 *  PMGetScale()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMGetScale(pageFormat: PMPageFormat; var scale: Double): OSStatus; external name '_PMGetScale';

{
 *  PMSetScale()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMSetScale(pageFormat: PMPageFormat; scale: Double): OSStatus; external name '_PMSetScale';

{**********************}
{  This is the drawing resolution of an app. This should not be confused with }
{  the resolution of the printer. You can call PMGetPrinterResolution to see }
{  what resolutions are avaliable for the current printer. }
{**********************}
{
 *  PMGetResolution()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMGetResolution(pageFormat: PMPageFormat; var res: PMResolution): OSStatus; external name '_PMGetResolution';

{
 *  PMSetResolution()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMSetResolution(pageFormat: PMPageFormat; const (*var*) res: PMResolution): OSStatus; external name '_PMSetResolution';

{**********************}
{  This is the physical size of the paper without regard to resolution, orientation }
{  or scaling. It is returned as a 72dpi value. }
{**********************}
{
 *  PMGetPhysicalPaperSize()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMGetPhysicalPaperSize(pageFormat: PMPageFormat; var paperSize: PMRect): OSStatus; external name '_PMGetPhysicalPaperSize';

{
 *  PMSetPhysicalPaperSize()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMSetPhysicalPaperSize(pageFormat: PMPageFormat; const (*var*) paperSize: PMRect): OSStatus; external name '_PMSetPhysicalPaperSize';

{**********************}
{  This is the physical size of the page without regard to resolution, orientation }
{  or scaling. It is returned as a 72dpi value. }
{**********************}
{
 *  PMGetPhysicalPageSize()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMGetPhysicalPageSize(pageFormat: PMPageFormat; var pageSize: PMRect): OSStatus; external name '_PMGetPhysicalPageSize';

{
 *  PMGetAdjustedPaperRect()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMGetAdjustedPaperRect(pageFormat: PMPageFormat; var paperRect: PMRect): OSStatus; external name '_PMGetAdjustedPaperRect';

{
 *  PMGetAdjustedPageRect()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMGetAdjustedPageRect(pageFormat: PMPageFormat; var pageRect: PMRect): OSStatus; external name '_PMGetAdjustedPageRect';

{
 *  PMGetUnadjustedPaperRect()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMGetUnadjustedPaperRect(pageFormat: PMPageFormat; var paperRect: PMRect): OSStatus; external name '_PMGetUnadjustedPaperRect';

{
 *  PMSetUnadjustedPaperRect()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMSetUnadjustedPaperRect(pageFormat: PMPageFormat; const (*var*) paperRect: PMRect): OSStatus; external name '_PMSetUnadjustedPaperRect';

{
 *  PMGetUnadjustedPageRect()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMGetUnadjustedPageRect(pageFormat: PMPageFormat; var pageRect: PMRect): OSStatus; external name '_PMGetUnadjustedPageRect';

{
 *  PMSetAdjustedPageRect()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMSetAdjustedPageRect(pageFormat: PMPageFormat; const (*var*) pageRect: PMRect): OSStatus; external name '_PMSetAdjustedPageRect';

{
 *  PMGetOrientation()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMGetOrientation(pageFormat: PMPageFormat; var orientation: PMOrientation): OSStatus; external name '_PMGetOrientation';

{
 *  PMSetOrientation()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMSetOrientation(pageFormat: PMPageFormat; orientation: PMOrientation; lock: boolean): OSStatus; external name '_PMSetOrientation';

{ PMPrintSettings }
{
 *  PMCopyPrintSettings()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMCopyPrintSettings(settingSrc: PMPrintSettings; settingDest: PMPrintSettings): OSStatus; external name '_PMCopyPrintSettings';

{**********************}
{  Flattening a print settings should only be necessary if you intend to preserve }
{  the object settings along with a document. A print settings will persist outside of a }
{  PMBegin/PMEnd block. This allows you to use any accessors on the object without }
{  the need to flatten and unflatten. Keep in mind the accessors make no assumption }
{  on the validity of the value. This can only be done thru PMValidatePrintSettings }
{  in a PMBegin/PMEnd block or with PMSessionValidatePrintSettings with a valid session. }
{  It is your responsibility for disposing of the handle. }
{**********************}
{
 *  PMFlattenPrintSettings()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMFlattenPrintSettings(printSettings: PMPrintSettings; var flatSettings: Handle): OSStatus; external name '_PMFlattenPrintSettings';

{
 *  PMUnflattenPrintSettings()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMUnflattenPrintSettings(flatSettings: Handle; var printSettings: PMPrintSettings): OSStatus; external name '_PMUnflattenPrintSettings';

{ PMPrintSettings Accessors }
{
 *  PMGetPrintSettingsExtendedData()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMGetPrintSettingsExtendedData(printSettings: PMPrintSettings; dataID: OSType; var size: UInt32; extendedData: UnivPtr): OSStatus; external name '_PMGetPrintSettingsExtendedData';

{
 *  PMSetPrintSettingsExtendedData()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMSetPrintSettingsExtendedData(printSettings: PMPrintSettings; dataID: OSType; size: UInt32; extendedData: UnivPtr): OSStatus; external name '_PMSetPrintSettingsExtendedData';

{
 *  PMGetDestination()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMGetDestination(printSettings: PMPrintSettings; var destType: PMDestinationType; var fileURL: CFURLRef): OSStatus; external name '_PMGetDestination';

{
 *  PMGetJobName()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMGetJobName(printSettings: PMPrintSettings; name: StringPtr): OSStatus; external name '_PMGetJobName';

{
 *  PMSetJobName()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMSetJobName(printSettings: PMPrintSettings; const (*var*) name: Str255): OSStatus; external name '_PMSetJobName';

{
 *  PMGetCopies()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMGetCopies(printSettings: PMPrintSettings; var copies: UInt32): OSStatus; external name '_PMGetCopies';

{
 *  PMSetCopies()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMSetCopies(printSettings: PMPrintSettings; copies: UInt32; lock: boolean): OSStatus; external name '_PMSetCopies';

{
 *  PMGetFirstPage()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMGetFirstPage(printSettings: PMPrintSettings; var first: UInt32): OSStatus; external name '_PMGetFirstPage';

{
 *  PMSetFirstPage()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMSetFirstPage(printSettings: PMPrintSettings; first: UInt32; lock: boolean): OSStatus; external name '_PMSetFirstPage';

{
 *  PMGetLastPage()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMGetLastPage(printSettings: PMPrintSettings; var last: UInt32): OSStatus; external name '_PMGetLastPage';

{
 *  PMSetLastPage()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMSetLastPage(printSettings: PMPrintSettings; last: UInt32; lock: boolean): OSStatus; external name '_PMSetLastPage';

{**********************}
{  The default page range is from 1-32000. The page range is something that is }
{  set by the application. It is NOT the first and last page to print. It serves }
{  as limits for setting the first and last page. You may pass kPMPrintAllPages for }
{  the maxPage value to specified that all pages are available for printing. }
{**********************}
{
 *  PMGetPageRange()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMGetPageRange(printSettings: PMPrintSettings; var minPage: UInt32; var maxPage: UInt32): OSStatus; external name '_PMGetPageRange';

{**********************}
{ The first and last page are immediately clipped to the new range }
{**********************}
{
 *  PMSetPageRange()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMSetPageRange(printSettings: PMPrintSettings; minPage: UInt32; maxPage: UInt32): OSStatus; external name '_PMSetPageRange';

{
 *  PMSetProfile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMSetProfile(printSettings: PMPrintSettings; tag: PMTag; const (*var*) profile: CMProfileLocation): OSStatus; external name '_PMSetProfile';

{
 *  PMGetColorMode()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMGetColorMode(printSettings: PMPrintSettings; var colorMode: PMColorMode): OSStatus; external name '_PMGetColorMode';

{
 *  PMSetColorMode()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMSetColorMode(printSettings: PMPrintSettings; colorMode: PMColorMode): OSStatus; external name '_PMSetColorMode';

{
 *  PMGetJobNameCFString()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMGetJobNameCFString(printSettings: PMPrintSettings; var name: CFStringRef): OSStatus; external name '_PMGetJobNameCFString';

{
 *  PMSetJobNameCFString()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMSetJobNameCFString(printSettings: PMPrintSettings; name: CFStringRef): OSStatus; external name '_PMSetJobNameCFString';

{
 *  PMSetCollate()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.2 and later
 }
function PMSetCollate(printSettings: PMPrintSettings; collate: boolean): OSStatus; external name '_PMSetCollate';

{
 *  PMGetCollate()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.2 and later
 }
function PMGetCollate(printSettings: PMPrintSettings; var collate: boolean): OSStatus; external name '_PMGetCollate';

{$ALIGN MAC68K}


end.
