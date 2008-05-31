{
     File:       ATSFont.p
 
     Contains:   Public interface to the font access and data management functions of ATS.
 
     Version:    Technology: Mac OS
                 Release:    Universal Interfaces 3.4.2
 
     Copyright:  © 2000-2002 by Apple Computer, Inc., all rights reserved.
 
     Bugs?:      For bug reports, consult the following page on
                 the World Wide Web:
 
                     http://www.freepascal.org/bugs.html
 
}


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

unit ATSFont;
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
uses MacTypes,CFBase,CFRunLoop,CFPropertyList,ATSTypes,CFString,Files,TextCommon,SFNTTypes;


{$ALIGN MAC68K}


const
	kATSOptionFlagsDefault		= 0;
	kATSOptionFlagsComposeFontPostScriptName = $01;				{  ATSFontGetPostScriptName  }
	kATSOptionFlagsUseDataForkAsResourceFork = $0100;			{  ATSFontActivateFromFileSpecification  }
	kATSOptionFlagsUseResourceFork = $0200;
	kATSOptionFlagsUseDataFork	= $0300;

	kATSIterationCompleted		= -980;
	kATSInvalidFontFamilyAccess	= -981;
	kATSInvalidFontAccess		= -982;
	kATSIterationScopeModified	= -983;
	kATSInvalidFontTableAccess	= -984;
	kATSInvalidFontContainerAccess = -985;
  kATSInvalidGlyphAccess        = -986;


type
	ATSFontContext						= UInt32;

const
	kATSFontContextUnspecified	= 0;
	kATSFontContextGlobal		= 1;
  kATSFontContextLocal          = 2;

const
  kATSOptionFlagsProcessSubdirectories = $00000001 shl 6; { Used by activation/deactivation & iteration }
  kATSOptionFlagsDoNotNotify    = $00000001 shl 7; { Do not notify after global activation/deactivation }

{ Iteration Option Flags }
const
  kATSOptionFlagsIterateByPrecedenceMask = $00000001 shl 5;
  kATSOptionFlagsIterationScopeMask = $00000007 shl 12; { Mask option bits 12-14 for iteration scopes }
  kATSOptionFlagsDefaultScope   = $00000000 shl 12;
  kATSOptionFlagsUnRestrictedScope = $00000001 shl 12;
  kATSOptionFlagsRestrictedScope = $00000002 shl 12;


type
	ATSFontFormat						= UInt32;

const
	kATSFontFormatUnspecified	= 0;


type
{$ifc TYPED_FUNCTION_POINTERS}
	ATSFontFamilyApplierFunction = function(iFamily: ATSFontFamilyRef; iRefCon: UnivPtr): OSStatus;
{$elsec}
	ATSFontFamilyApplierFunction = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	ATSFontApplierFunction = function(iFont: ATSFontRef; iRefCon: UnivPtr): OSStatus;
{$elsec}
	ATSFontApplierFunction = ProcPtr;
{$endc}

	ATSFontFamilyIterator    = ^SInt32; { an opaque 32-bit type }
	ATSFontFamilyIteratorPtr = ^ATSFontFamilyIterator;  { when a var xx:ATSFontFamilyIterator parameter can be nil, it is changed to xx: ATSFontFamilyIteratorPtr }
	ATSFontIterator    = ^SInt32; { an opaque 32-bit type }
	ATSFontIteratorPtr = ^ATSFontIterator;  { when a var xx:ATSFontIterator parameter can be nil, it is changed to xx: ATSFontIteratorPtr }

const
	kATSFontFilterCurrentVersion = 0;


type
	ATSFontFilterSelector 		= SInt32;
const
	kATSFontFilterSelectorUnspecified = 0;
	kATSFontFilterSelectorGeneration = 3;
	kATSFontFilterSelectorFontFamily = 7;
	kATSFontFilterSelectorFontFamilyApplierFunction = 8;
	kATSFontFilterSelectorFontApplierFunction = 9;


type
	ATSFontFilterPtr = ^ATSFontFilter;
	ATSFontFilter = record
		version:				UInt32;
		filterSelector:			ATSFontFilterSelector;
		case SInt16 of
		0: (
			generationFilter:	ATSGeneration;
			);
		1: (
			fontFamilyFilter:	ATSFontFamilyRef;
			);
		2: (
			fontFamilyApplierFunctionFilter: ATSFontFamilyApplierFunction;
			);
		3: (
			fontApplierFunctionFilter: ATSFontApplierFunction;
			);
	end;

{ Notification related }
type
	ATSFontNotificationRef    = ^SInt32; { an opaque 32-bit type }
	ATSFontNotificationRefPtr = ^ATSFontNotificationRef;
	ATSFontNotificationInfoRef    = ^SInt32; { an opaque 32-bit type }
	ATSFontNotificationInfoRefPtr = ^ATSFontNotificationInfoRef;

{
 *  ATSFontNotifyOption
 *  
 *  Discussion:
 *    Options used with ATSFontNotificationSubscribe.  Any of the
 *    options that follow may be used together in order to alter the
 *    default behavior of ATS notifications.
 }
type
	ATSFontNotifyOption = SInt32;
const

  {
   * Default behavior of ATSFontNotificationSubscribe.
   }
  kATSFontNotifyOptionDefault   = 0;

  {
   * Normally applications will only receive ATS notifications while in
   * the foreground.   If suspended, the notification will be delivered
   * when then application comes to the foreground.  This is the
   * default.  You should set this option if you are a server or tool
   * that performs font management functions and require immediate
   * notification when something changes.
   }
  kATSFontNotifyOptionReceiveWhileSuspended = 1 shl 0;

{
 *  ATSFontNotifyAction
 *  
 *  Discussion:
 *    Used with ATSFontNotify.   The following is a list of actions you
 *    might wish the ATS server to perform and notify clients if
 *    appropriate.
 }
type
	ATSFontNotifyAction = SInt32;
const

  {
   * Used after a batch (de)activation of fonts occurs.   Typically the
   * caller has exercised multiple global (De)Activation calls with the
   * kATSOptionFlagsDoNotNotify set. Once all calls are completed, one
   * may use ATSFontNotify with this action to ask ATS to notify all
   * clients.
   }
  kATSFontNotifyActionFontsChanged = 1;

  {
   * The ATS system with the help of the Finder keeps track of changes
   * to any of the font directories in the system domains ( System,
   * Local, Network, User, & Classic). However, one may wish to
   * add/remove fonts to these locations programmatically. This action
   * is used to let ATS server to rescan these directories and post
   * notifications if necessary.
   }
  kATSFontNotifyActionDirectoriesChanged = 2;


{
 *  ATSNotificationCallback
 *  
 *  Discussion:
 *    Callback delivered for ATS notifications.
 *  
 *  Parameters:
 *    
 *    info:
 *      Parameter is placed here for future improvements.  Initially
 *      the contents of this parameter will be NULL.
 *    
 *    refCon:
 *      User data/state to be supplied to callback function
 }
type ATSNotificationCallback = procedure( info: ATSFontNotificationInfoRef; refCon: UnivPtr );

	{	 ----------------------------------------------------------------------------------------- 	}
	{	 Font container                                                                            	}
	{	 ----------------------------------------------------------------------------------------- 	}
	{
	 *  ATSGetGeneration()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   not available
	 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
function ATSGetGeneration: ATSGeneration; external name '_ATSGetGeneration';

{
 *  ATSFontActivateFromFileSpecification()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ATSFontActivateFromFileSpecification(const (*var*) iFile: FSSpec; iContext: ATSFontContext; iFormat: ATSFontFormat; iReserved: UnivPtr; iOptions: ATSOptionFlags; var oContainer: ATSFontContainerRef): OSStatus; external name '_ATSFontActivateFromFileSpecification';

{
 *  ATSFontActivateFromMemory()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ATSFontActivateFromMemory(iData: LogicalAddress; iLength: ByteCount; iContext: ATSFontContext; iFormat: ATSFontFormat; iReserved: UnivPtr; iOptions: ATSOptionFlags; var oContainer: ATSFontContainerRef): OSStatus; external name '_ATSFontActivateFromMemory';

{
 *  ATSFontDeactivate()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ATSFontDeactivate(iContainer: ATSFontContainerRef; iRefCon: UnivPtr; iOptions: ATSOptionFlags): OSStatus; external name '_ATSFontDeactivate';

{ ----------------------------------------------------------------------------------------- }
{ Font family                                                                               }
{ ----------------------------------------------------------------------------------------- }
{
 *  ATSFontFamilyApplyFunction()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ATSFontFamilyApplyFunction(iFunction: ATSFontFamilyApplierFunction; iRefCon: UnivPtr): OSStatus; external name '_ATSFontFamilyApplyFunction';

{
 *  ATSFontFamilyIteratorCreate()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ATSFontFamilyIteratorCreate(iContext: ATSFontContext; iFilter: {Const}ATSFontFilterPtr; iRefCon: UnivPtr; iOptions: ATSOptionFlags; var ioIterator: ATSFontFamilyIterator): OSStatus; external name '_ATSFontFamilyIteratorCreate';

{
 *  ATSFontFamilyIteratorRelease()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ATSFontFamilyIteratorRelease(var ioIterator: ATSFontFamilyIterator): OSStatus; external name '_ATSFontFamilyIteratorRelease';

{
 *  ATSFontFamilyIteratorReset()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ATSFontFamilyIteratorReset(iContext: ATSFontContext; iFilter: {Const}ATSFontFilterPtr; iRefCon: UnivPtr; iOptions: ATSOptionFlags; var ioIterator: ATSFontFamilyIterator): OSStatus; external name '_ATSFontFamilyIteratorReset';

{
 *  ATSFontFamilyIteratorNext()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ATSFontFamilyIteratorNext(iIterator: ATSFontFamilyIterator; var oFamily: ATSFontFamilyRef): OSStatus; external name '_ATSFontFamilyIteratorNext';

{
 *  ATSFontFamilyFindFromName()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ATSFontFamilyFindFromName(iName: CFStringRef; iOptions: ATSOptionFlags): ATSFontFamilyRef; external name '_ATSFontFamilyFindFromName';

{
 *  ATSFontFamilyGetGeneration()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ATSFontFamilyGetGeneration(iFamily: ATSFontFamilyRef): ATSGeneration; external name '_ATSFontFamilyGetGeneration';

{
 *  ATSFontFamilyGetName()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ATSFontFamilyGetName(iFamily: ATSFontFamilyRef; iOptions: ATSOptionFlags; var oName: CFStringRef): OSStatus; external name '_ATSFontFamilyGetName';

{
 *  ATSFontFamilyGetEncoding()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ATSFontFamilyGetEncoding(iFamily: ATSFontFamilyRef): TextEncoding; external name '_ATSFontFamilyGetEncoding';

{ ----------------------------------------------------------------------------------------- }
{ Font                                                                                      }
{ ----------------------------------------------------------------------------------------- }
{
 *  ATSFontApplyFunction()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ATSFontApplyFunction(iFunction: ATSFontApplierFunction; iRefCon: UnivPtr): OSStatus; external name '_ATSFontApplyFunction';

{
 *  ATSFontIteratorCreate()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ATSFontIteratorCreate(iContext: ATSFontContext; iFilter: {Const}ATSFontFilterPtr; iRefCon: UnivPtr; iOptions: ATSOptionFlags; var ioIterator: ATSFontIterator): OSStatus; external name '_ATSFontIteratorCreate';

{
 *  ATSFontIteratorRelease()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ATSFontIteratorRelease(var ioIterator: ATSFontIterator): OSStatus; external name '_ATSFontIteratorRelease';

{
 *  ATSFontIteratorReset()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ATSFontIteratorReset(iContext: ATSFontContext; iFilter: {Const}ATSFontFilterPtr; iRefCon: UnivPtr; iOptions: ATSOptionFlags; var ioIterator: ATSFontIterator): OSStatus; external name '_ATSFontIteratorReset';

{
 *  ATSFontIteratorNext()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ATSFontIteratorNext(iIterator: ATSFontIterator; var oFont: ATSFontRef): OSStatus; external name '_ATSFontIteratorNext';

{
 *  ATSFontFindFromName()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ATSFontFindFromName(iName: CFStringRef; iOptions: ATSOptionFlags): ATSFontRef; external name '_ATSFontFindFromName';

{
 *  ATSFontFindFromPostScriptName()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ATSFontFindFromPostScriptName(iName: CFStringRef; iOptions: ATSOptionFlags): ATSFontRef; external name '_ATSFontFindFromPostScriptName';

{
 *  ATSFontFindFromContainer()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ATSFontFindFromContainer(iContainer: ATSFontContainerRef; iOptions: ATSOptionFlags; iCount: ItemCount; var ioArray: ATSFontRef; var oCount: ItemCount): OSStatus; external name '_ATSFontFindFromContainer';

{
 *  ATSFontGetGeneration()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ATSFontGetGeneration(iFont: ATSFontRef): ATSGeneration; external name '_ATSFontGetGeneration';

{
 *  ATSFontGetName()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ATSFontGetName(iFont: ATSFontRef; iOptions: ATSOptionFlags; var oName: CFStringRef): OSStatus; external name '_ATSFontGetName';

{
 *  ATSFontGetPostScriptName()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ATSFontGetPostScriptName(iFont: ATSFontRef; iOptions: ATSOptionFlags; var oName: CFStringRef): OSStatus; external name '_ATSFontGetPostScriptName';

{
 *  ATSFontGetTableDirectory()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ATSFontGetTableDirectory(iFont: ATSFontRef; iBufferSize: ByteCount; ioBuffer: UnivPtr; oSize: ByteCountPtr): OSStatus; external name '_ATSFontGetTableDirectory';

{
 *  ATSFontGetTable()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ATSFontGetTable(iFont: ATSFontRef; iTag: FourCharCode; iOffset: ByteOffset; iBufferSize: ByteCount; ioBuffer: UnivPtr; oSize: ByteCountPtr): OSStatus; external name '_ATSFontGetTable';

{
 *  ATSFontGetHorizontalMetrics()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ATSFontGetHorizontalMetrics(iFont: ATSFontRef; iOptions: ATSOptionFlags; var oMetrics: ATSFontMetrics): OSStatus; external name '_ATSFontGetHorizontalMetrics';

{
 *  ATSFontGetVerticalMetrics()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ATSFontGetVerticalMetrics(iFont: ATSFontRef; iOptions: ATSOptionFlags; var oMetrics: ATSFontMetrics): OSStatus; external name '_ATSFontGetVerticalMetrics';

{ ----------------------------------------------------------------------------------------- }
{ Compatibiity                                                                              }
{ ----------------------------------------------------------------------------------------- }
{
 *  ATSFontFamilyFindFromQuickDrawName()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ATSFontFamilyFindFromQuickDrawName(const (*var*) iName: Str255): ATSFontFamilyRef; external name '_ATSFontFamilyFindFromQuickDrawName';

{
 *  ATSFontFamilyGetQuickDrawName()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ATSFontFamilyGetQuickDrawName(iFamily: ATSFontFamilyRef; var oName: Str255): OSStatus; external name '_ATSFontFamilyGetQuickDrawName';

{
 *  ATSFontGetFileSpecification()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ATSFontGetFileSpecification(iFont: ATSFontRef; var oFile: FSSpec): OSStatus; external name '_ATSFontGetFileSpecification';

{
 *  ATSFontGetFontFamilyResource()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ATSFontGetFontFamilyResource(iFont: ATSFontRef; iBufferSize: ByteCount; ioBuffer: UnivPtr; oSize: ByteCountPtr): OSStatus; external name '_ATSFontGetFontFamilyResource';

{ ----------------------------------------------------------------------------------------- }
{ Notification                                                                              }
{ ----------------------------------------------------------------------------------------- }
{
 *  ATSFontNotify()
 *  
 *  Summary:
 *    Used to alert ATS that an action which may require notification
 *    to clients has occurred.
 *  
 *  Parameters:
 *    
 *    action:
 *      Action that should be taken by the ATS Server
 *    
 *    info:
 *      Any required or optional information that may be required by
 *      the action taken. can be NULL
 *  
 *  Result:
 *    noErr Action successfully reported paramErr Invalid action passed
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in ApplicationServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.2 and later
 *    Non-Carbon CFM:   not available
 }
// AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER
function ATSFontNotify( action: ATSFontNotifyAction; info: UnivPtr ): OSStatus; external name '_ATSFontNotify';


{
 *  ATSFontNotificationSubscribe()
 *  
 *  Summary:
 *    Ask the ATS System to notify caller when certain events have
 *    occurred.  Note that your application must have a CFRunLoop in
 *    order to receive notifications. Any Appkit or Carbon event loop
 *    based application will have one by default.
 *  
 *  Parameters:
 *    
 *    callback:
 *      Function that will be called by the ATS system whenever an
 *      event of interest takes place.
 *    
 *    options:
 *      Set the wanted ATSFontNotificationOptions to modify the default
 *      behavior of ATS Notifications.
 *    
 *    iRefcon:
 *      User data/state which will be passed to the callback funtion. can be NULL
 *    
 *    oNotificationRef:
 *      You may use this reference to un-subscribe to this notification. can be NULL
 *  
 *  Result:
 *    noErr Subscribed successfully paramErr NULL callback was passed.
 *    memFullErr Could not allocate enough memory for internal data
 *    structures.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in ApplicationServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.2 and later
 *    Non-Carbon CFM:   not available
 }
// AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER
function ATSFontNotificationSubscribe( callback: ATSNotificationCallback; options: ATSFontNotifyOption; iRefcon: UnivPtr; oNotificationRef: ATSFontNotificationRefPtr ): OSStatus; external name '_ATSFontNotificationSubscribe';


{
 *  ATSFontNotificationUnsubscribe()
 *  
 *  Summary:
 *    Release subscription and stop receiving notifications for a given
 *    reference.
 *  
 *  Parameters:
 *    
 *    notificationRef:
 *      Notification reference for which you want to stop receiving
 *      notifications. Note, if more than one notification has been
 *      requested of ATS, you will still receive notifications on those
 *      requests.
 *  
 *  Result:
 *    noErr Unsubscribed successfully paramErr NULL/invalid
 *    notificationRef passed
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in ApplicationServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.2 and later
 *    Non-Carbon CFM:   not available
 }
// AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER
function ATSFontNotificationUnsubscribe( notificationRef: ATSFontNotificationRef ): OSStatus; external name '_ATSFontNotificationUnsubscribe';


{ ----------------------------------------------------------------------------------------- }
{ Font query message hooks                                                                  }
{ ----------------------------------------------------------------------------------------- }

{
 *  ATSFontQuerySourceContext
 *  
 *  Summary:
 *    A parameter block for client information to be retained by ATS
 *    and passed back to an ATSFontQueryCallback function.
 }
type
	ATSFontQuerySourceContext = record

  {
   * A 32-bit unsigned integer that indicates the version of this
   * structure. This should be set to 0.
   }
		version: UInt32;

  {
   * A pointer-sized client datum that should be passed back to an
   * ATSFontQueryCallback function.
   }
		refCon: Ptr;

  {
   * The callback used to add a retain to the refCon.
   }
		retain: CFAllocatorRetainCallBack;

  {
   * The callback used to remove a retain to the refCon.
   }
		release: CFAllocatorReleaseCallBack;
	end;
	ATSFontQuerySourceContextPtr = ^ATSFontQuerySourceContext;

{
 *  ATSFontQueryMessageID
 *  
 *  Discussion:
 *    Constants for ATS font query message types.
 }
type
	ATSFontQueryMessageID = SInt32;
const

  {
   * The message ID for a font request query. The data for a message
   * with this ID is a flattened CFDictionaryRef with keys and values
   * as decribed below. A query dictionary may have any or all of these
   * entries.
   }
  kATSQueryActivateFontMessage  = FourCharCode('atsa');


{
 *  ATSFontQueryCallback
 *  
 *  Summary:
 *    Callback for receiving font-related queries from ATS.
 *  
 *  Parameters:
 *    
 *    msgid:
 *      An ATSFontQueryMessageID that identifies the message type.
 *    
 *    data:
 *      A CFPropertyListRef that represents the query. The content is
 *      message type-specific.
 *    
 *    refCon:
 *      A pointer-sized client datum that was optionally provided to
 *      ATSCreateFontQueryRunLoopSource.
 *  
 *  Result:
 *    A CFPropertyListRef that represents the message type-specific
 *    response to the query. May be NULL.
 }
type ATSFontQueryCallback = function( msgid: ATSFontQueryMessageID; data: CFPropertyListRef; refCon: UnivPtr ): CFPropertyListRef;
{
 *  ATSCreateFontQueryRunLoopSource()
 *  
 *  Summary:
 *    Creates a CFRunLoopSourceRef that will be used to convey font
 *    queries from ATS.
 *  
 *  Parameters:
 *    
 *    queryOrder:
 *      A CFIndex that specifies the priority of this query receiver
 *      relative to others. When ATS makes a font query, it will send
 *      the query to each receiver in priority order, from highest to
 *      lowest. "Normal" priority is 0.
 *    
 *    sourceOrder:
 *      The order of the created run loop source.
 *    
 *    callout:
 *      A function pointer of type ATSFontQueryCallback that will be
 *      called to process a font query.
 *    
 *    context:
 *      An ATSFontQuerySourceContext parameter block that provides a
 *      pointer-sized client datum which will be retained by ATS and
 *      passed to the callout function. May be NULL.
 *  
 *  Result:
 *    A CFRunLoopSourceRef. To stop receiving queries, invalidate this
 *    run loop source.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
// AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER
function ATSCreateFontQueryRunLoopSource( queryOrder: CFIndex; sourceOrder: CFIndex; callout: ATSFontQueryCallback; context: ATSFontQuerySourceContextPtr ): CFRunLoopSourceRef; external name '_ATSCreateFontQueryRunLoopSource';

{ ----------------------------------------------------------------------------------------- }
{ Font request query message content                                                        }
{ ----------------------------------------------------------------------------------------- }
{ Keys in a font request query dictionary. }
{ These keys appear in the dictionary for a kATSQueryActivateFontMessage query. }

//const
{
 *  kATSQueryClientPID
 *  
 *  Discussion:
 *    The process ID of the application making the query. The
 *    corresponding value is a CFNumberRef that contains a pid_t.
 }
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kATSQueryClientPID CFSTRP('ATS client pid')}
{$endc}

{
 *  kATSQueryQDFamilyName
 *  
 *  Discussion:
 *    The Quickdraw-style family name of the font being requested, e.g.
 *    the name passed to GetFNum. The corresponding value is a
 *    CFStringRef.
 }
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kATSQueryQDFamilyName CFSTRP('font family name')}
{$endc}

{
 *  kATSQueryFontName
 *  
 *  Discussion:
 *    The name of the font being requested. The corresponding value is
 *    a CFStringRef suitable as an argument to ATSFontFindFromName().
 *    This should match a candidate font's unique or full name.
 }
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kATSQueryFontName CFSTRP('font name')}
{$endc}

{
 *  kATSQueryFontPostScriptName
 *  
 *  Discussion:
 *    The PostScript name of the font being requested. The
 *    corresponding value is a CFStringRef suitable as an argument to
 *    ATSFontFindFromPostScriptName(). This should match either the
 *    PostScript name derived from the font's FOND resource or its sfnt
 *    name table, with preference given to the FOND PostScript name.
 }
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kATSQueryFontPostScriptName CFSTRP('font PS name')}
{$endc}

{
 *  kATSQueryFontNameTableEntries
 *  
 *  Discussion:
 *    A descriptor for sfnt name table entries that the requested font
 *    must have. The corresponding value is a CFArrayRef of
 *    CFDictionaryRefs that describe name table entries. A font must
 *    have all of the specified entries to be considered a match.
 }
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kATSQueryFontNameTableEntries CFSTRP('font name table entries')}
{$endc}
{ Keys in a font raw name descriptor dictionary. }

{
 *  kATSFontNameTableCode
 *  
 *  Discussion:
 *    The font name's name code. The corresponding value is a
 *    CFNumberRef. If missing, assume kFontNoNameCode.
 }
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kATSFontNameTableCode CFSTRP('font name code')}
{$endc}

{
 *  kATSFontNameTablePlatform
 *  
 *  Discussion:
 *    The font name's platform code. The corresponding value is a
 *    CFNumberRef. If missing, assume kFontNoPlatformCode.
 }
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kATSFontNameTablePlatform CFSTRP('font platform code')}
{$endc}

{
 *  kATSFontNameTableScript
 *  
 *  Discussion:
 *    The font name's script code. The corresponding value is a
 *    CFNumberRef. If missing, assume kFontNoScriptCode.
 }
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kATSFontNameTableScript CFSTRP('font script code')}
{$endc}

{
 *  kATSFontNameTableLanguage
 *  
 *  Discussion:
 *    The font name's language code. The corresponding value is a
 *    CFNumberRef. If missing, assume kFontNoLanguageCode.
 }
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kATSFontNameTableLanguage CFSTRP('font language code')}
{$endc}

{
 *  kATSFontNameTableBytes
 *  
 *  Discussion:
 *    The raw bytes of the font name. The corresponding value is a
 *    CFDataRef that contains the raw name bytes.
 }
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kATSFontNameTableBytes CFSTRP('font name table bytes')}
{$endc}

{$ALIGN MAC68K}


end.
