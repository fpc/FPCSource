{
     File:       DrawSprocket/DrawSprocket.h
 
     Contains:   Games Sprockets: DrawSprocket interfaces
 
     Version:    DrawSprocket-2.0.85~65
 
     Copyright:  © 1999-2008 by Apple Computer, Inc., all rights reserved.
 
     Bugs?:      For bug reports, consult the following page on
                 the World Wide Web:
 
                     http://bugs.freepascal.org
 
}
{       Pascal Translation Updated:  Jonas Maebe, <jonas@freepascal.org>, October 2009 }
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

unit DrawSprocket;
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
{$ifc defined(iphonesim)}
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
	{$setc TARGET_OS_MAC := TRUE}
	{$setc TARGET_OS_IPHONE := FALSE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
	{$setc TARGET_OS_EMBEDDED := FALSE}
{$elifc defined __arm__ and __arm__}
	{$setc TARGET_CPU_PPC := FALSE}
	{$setc TARGET_CPU_PPC64 := FALSE}
	{$setc TARGET_CPU_X86 := FALSE}
	{$setc TARGET_CPU_X86_64 := FALSE}
	{$setc TARGET_CPU_ARM := TRUE}
	{$setc TARGET_CPU_ARM64 := FALSE}
	{ will require compiler define when/if other Apple devices with ARM cpus ship }
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
	{ will require compiler define when/if other Apple devices with ARM cpus ship }
	{$setc TARGET_OS_MAC := FALSE}
	{$setc TARGET_OS_IPHONE := TRUE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
	{$setc TARGET_OS_EMBEDDED := TRUE}
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
uses MacTypes,Video,Events,QuickdrawTypes,QDOffscreen,Displays,MacErrors;
{$endc} {not MACOSALLINCLUDE}


{$ifc TARGET_OS_MAC}

{$ALIGN POWER}


{******************* DEPRECATION NOTICE *********************
 *
 * The DrawSprocket API is being deprecated, and should be replaced
 * by the CGDirectDisplay API in the CoreGraphics framework in 
 * ApplicationServices.framework.
 *
 ************************************************************}

{
********************************************************************************
** constants
********************************************************************************
}
type
	DSpDepthMask = SInt32;
const
	kDSpDepthMask_1 = 1 shl 0;
	kDSpDepthMask_2 = 1 shl 1;
	kDSpDepthMask_4 = 1 shl 2;
	kDSpDepthMask_8 = 1 shl 3;
	kDSpDepthMask_16 = 1 shl 4;
	kDSpDepthMask_32 = 1 shl 5;
	kDSpDepthMask_All = -1;

type
	DSpColorNeeds = SInt32;
const
	kDSpColorNeeds_DontCare = 0;
	kDSpColorNeeds_Request = 1;
	kDSpColorNeeds_Require = 2;

type
	DSpContextState = SInt32;
const
	kDSpContextState_Active = 0;
	kDSpContextState_Paused = 1;
	kDSpContextState_Inactive = 2;

{ kDSpContextOption_QD3DAccel not yet implemented }
type
	DSpContextOption = SInt32;
const
{    kDSpContextOption_QD3DAccel       = 1<<0,}
	kDSpContextOption_PageFlip = 1 shl 1;
	kDSpContextOption_DontSyncVBL = 1 shl 2;
	kDSpContextOption_Stereoscopic = 1 shl 3;

type
	DSpAltBufferOption = SInt32;
const
	kDSpAltBufferOption_RowBytesEqualsWidth = 1 shl 0;

type
	DSpBufferKind = SInt32;
const
	kDSpBufferKind_Normal = 0;

type
	DSpBlitMode = SInt32;
const
	kDSpBlitMode_Plain = 0;
	kDSpBlitMode_SrcKey = 1 shl 0;
	kDSpBlitMode_DstKey = 1 shl 1;
	kDSpBlitMode_Interpolation = 1 shl 2;

{
********************************************************************************
** data types
********************************************************************************
}
type
	DSpAltBufferReference = ^OpaqueDSpAltBufferReference; { an opaque type }
	OpaqueDSpAltBufferReference = record end;
	DSpAltBufferReferencePtr = ^DSpAltBufferReference;  { when a var xx:DSpAltBufferReference parameter can be nil, it is changed to xx: DSpAltBufferReferencePtr }
	DSpContextReference = ^OpaqueDSpContextReference; { an opaque type }
	OpaqueDSpContextReference = record end;
	DSpContextReferencePtr = ^DSpContextReference;  { when a var xx:DSpContextReference parameter can be nil, it is changed to xx: DSpContextReferencePtr }
	DSpContextReferenceConst = ^OpaqueDSpContextReference;
	DSpContextReferenceConstPtr = ^DSpContextReferenceConst;  { when a var xx:DSpContextReferenceConst parameter can be nil, it is changed to xx: DSpContextReferenceConstPtr }
const
	kDSpEveryContext = nil;
type
	DSpEventProcPtr = function( var inEvent: EventRecord ): Boolean;
	DSpCallbackProcPtr = function( inContext: DSpContextReference; inRefCon: UnivPtr ): Boolean;
	DSpEventUPP = DSpEventProcPtr;
	DSpCallbackUPP = DSpCallbackProcPtr;
{
 *  NewDSpEventUPP()
 *  
 *  Availability:
 *    Mac OS X:         not available
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   available as macro/inline
 }

{
 *  NewDSpCallbackUPP()
 *  
 *  Availability:
 *    Mac OS X:         not available
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   available as macro/inline
 }

{
 *  DisposeDSpEventUPP()
 *  
 *  Availability:
 *    Mac OS X:         not available
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   available as macro/inline
 }

{
 *  DisposeDSpCallbackUPP()
 *  
 *  Availability:
 *    Mac OS X:         not available
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   available as macro/inline
 }

{
 *  InvokeDSpEventUPP()
 *  
 *  Availability:
 *    Mac OS X:         not available
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   available as macro/inline
 }

{
 *  InvokeDSpCallbackUPP()
 *  
 *  Availability:
 *    Mac OS X:         not available
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   available as macro/inline
 }

type
	DSpContextAttributesPtr = ^DSpContextAttributes;
	DSpContextAttributes = record
		frequency: Fixed;
		displayWidth: UInt32;
		displayHeight: UInt32;
		reserved1: UInt32;
		reserved2: UInt32;
		colorNeeds: UInt32;
		colorTable: CTabHandle;
		contextOptions: OptionBits;
		backBufferDepthMask: OptionBits;
		displayDepthMask: OptionBits;
		backBufferBestDepth: UInt32;
		displayBestDepth: UInt32;
		pageCount: UInt32;
		filler1,filler2,filler3: SInt8;
		gameMustConfirmSwitch: Boolean;
		reserved3: array [0..4-1] of UInt32;
	end;
type
	DSpAltBufferAttributesPtr = ^DSpAltBufferAttributes;
	DSpAltBufferAttributes = record
		width: UInt32;
		height: UInt32;
		options: DSpAltBufferOption;
		reserved: array [0..4-1] of UInt32;
	end;
type
	DSpBlitInfoPtr = ^DSpBlitInfo;
	DSpBlitDoneProc = procedure( info: DSpBlitInfoPtr );
	DSpBlitInfo = record
		completionFlag: Boolean;
		filler1, filler2, filler3: SInt8;
		completionProc: DSpBlitDoneProc;
		srcContext: DSpContextReference;
		srcBuffer: CGrafPtr;
		srcRect: Rect;
		srcKey: UInt32;

		dstContext: DSpContextReference;
		dstBuffer: CGrafPtr;
		dstRect: Rect;
		dstKey: UInt32;

		mode: DSpBlitMode;
		reserved: array [0..4-1] of UInt32;
	end;
{
********************************************************************************
** function prototypes
********************************************************************************
}

{
** global operations
}
{
 *  DSpStartup()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in DrawSprocket.framework but deprecated in 10.4
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in DrawSprocketLib 1.0 and later
 }
function DSpStartup: OSStatus; external name '_DSpStartup';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4 *)


{
 *  DSpShutdown()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in DrawSprocket.framework but deprecated in 10.4
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in DrawSprocketLib 1.0 and later
 }
function DSpShutdown: OSStatus; external name '_DSpShutdown';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4 *)


{
 *  DSpGetVersion()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in DrawSprocket.framework but deprecated in 10.4
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in DrawSprocketLib 1.7 and later
 }
function DSpGetVersion: NumVersion; external name '_DSpGetVersion';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4 *)


{
 *  DSpGetFirstContext()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in DrawSprocket.framework but deprecated in 10.4
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in DrawSprocketLib 1.0 and later
 }
function DSpGetFirstContext( inDisplayID: DisplayIDType; var outContext: DSpContextReference ): OSStatus; external name '_DSpGetFirstContext';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4 *)


{
 *  DSpGetNextContext()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in DrawSprocket.framework but deprecated in 10.4
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in DrawSprocketLib 1.0 and later
 }
function DSpGetNextContext( inCurrentContext: DSpContextReference; var outContext: DSpContextReference ): OSStatus; external name '_DSpGetNextContext';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4 *)


{
 *  DSpGetCurrentContext()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in DrawSprocket.framework but deprecated in 10.4
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in DrawSprocketLib 1.7 and later
 }
function DSpGetCurrentContext( inDisplayID: DisplayIDType; var outContext: DSpContextReference ): OSStatus; external name '_DSpGetCurrentContext';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4 *)


{
 *  DSpFindBestContext()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in DrawSprocket.framework but deprecated in 10.4
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in DrawSprocketLib 1.0 and later
 }
function DSpFindBestContext( inDesiredAttributes: DSpContextAttributesPtr; var outContext: DSpContextReference ): OSStatus; external name '_DSpFindBestContext';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4 *)


{
 *  DSpFindBestContextOnDisplayID()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in DrawSprocket.framework but deprecated in 10.4
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in DrawSprocketLib 1.7 and later
 }
function DSpFindBestContextOnDisplayID( inDesiredAttributes: DSpContextAttributesPtr; var outContext: DSpContextReference; inDisplayID: DisplayIDType ): OSStatus; external name '_DSpFindBestContextOnDisplayID';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4 *)


{
 *  DSpCanUserSelectContext()
 *  
 *  Availability:
 *    Mac OS X:         not available
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in DrawSprocketLib 1.0 and later
 }


{
 *  DSpUserSelectContext()
 *  
 *  Availability:
 *    Mac OS X:         not available
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in DrawSprocketLib 1.0 and later
 }


{
 *  DSpProcessEvent()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in DrawSprocket.framework but deprecated in 10.4
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in DrawSprocketLib 1.0 and later
 }
function DSpProcessEvent( var inEvent: EventRecord; var outEventWasProcessed: Boolean ): OSStatus; external name '_DSpProcessEvent';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4 *)


{
 *  DSpSetBlankingColor()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in DrawSprocket.framework but deprecated in 10.4
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in DrawSprocketLib 1.0 and later
 }
function DSpSetBlankingColor( const (*var*) inRGBColor: RGBColor ): OSStatus; external name '_DSpSetBlankingColor';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4 *)


{
 *  DSpSetDebugMode()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in DrawSprocket.framework but deprecated in 10.4
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in DrawSprocketLib 1.0 and later
 }
function DSpSetDebugMode( inDebugMode: Boolean ): OSStatus; external name '_DSpSetDebugMode';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4 *)


{
 *  DSpFindContextFromPoint()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in DrawSprocket.framework but deprecated in 10.4
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in DrawSprocketLib 1.0 and later
 }
function DSpFindContextFromPoint( inGlobalPoint: Point; var outContext: DSpContextReference ): OSStatus; external name '_DSpFindContextFromPoint';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4 *)


{
 *  DSpGetMouse()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in DrawSprocket.framework but deprecated in 10.4
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in DrawSprocketLib 1.0 and later
 }
function DSpGetMouse( var outGlobalPoint: Point ): OSStatus; external name '_DSpGetMouse';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4 *)


{
** alternate buffer operations
}
{
 *  DSpAltBuffer_New()
 *  
 *  Availability:
 *    Mac OS X:         not available
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in DrawSprocketLib 1.0 and later
 }


{
 *  DSpAltBuffer_Dispose()
 *  
 *  Availability:
 *    Mac OS X:         not available
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in DrawSprocketLib 1.0 and later
 }


{
 *  DSpAltBuffer_InvalRect()
 *  
 *  Availability:
 *    Mac OS X:         not available
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in DrawSprocketLib 1.0 and later
 }


{
 *  DSpAltBuffer_GetCGrafPtr()
 *  
 *  Availability:
 *    Mac OS X:         not available
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in DrawSprocketLib 1.0 and later
 }


{
** context operations
}
{ general }
{
 *  DSpContext_GetAttributes()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in DrawSprocket.framework but deprecated in 10.4
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in DrawSprocketLib 1.0 and later
 }
function DSpContext_GetAttributes( inContext: DSpContextReferenceConst; outAttributes: DSpContextAttributesPtr ): OSStatus; external name '_DSpContext_GetAttributes';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4 *)


{
 *  DSpContext_Reserve()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in DrawSprocket.framework but deprecated in 10.4
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in DrawSprocketLib 1.0 and later
 }
function DSpContext_Reserve( inContext: DSpContextReference; inDesiredAttributes: DSpContextAttributesPtr ): OSStatus; external name '_DSpContext_Reserve';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4 *)


{
 *  DSpContext_Queue()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in DrawSprocket.framework but deprecated in 10.4
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in DrawSprocketLib 1.7 and later
 }
function DSpContext_Queue( inParentContext: DSpContextReference; inChildContext: DSpContextReference; inDesiredAttributes: DSpContextAttributesPtr ): OSStatus; external name '_DSpContext_Queue';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4 *)


{
 *  DSpContext_Switch()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in DrawSprocket.framework but deprecated in 10.4
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in DrawSprocketLib 1.7 and later
 }
function DSpContext_Switch( inOldContext: DSpContextReference; inNewContext: DSpContextReference ): OSStatus; external name '_DSpContext_Switch';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4 *)


{
 *  DSpContext_Release()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in DrawSprocket.framework but deprecated in 10.4
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in DrawSprocketLib 1.0 and later
 }
function DSpContext_Release( inContext: DSpContextReference ): OSStatus; external name '_DSpContext_Release';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4 *)


{
 *  DSpContext_Dispose()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in DrawSprocket.framework but deprecated in 10.4
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function DSpContext_Dispose( inContext: DSpContextReference ): OSStatus; external name '_DSpContext_Dispose';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4 *)


{
 *  DSpContext_GetDisplayID()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in DrawSprocket.framework but deprecated in 10.4
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in DrawSprocketLib 1.0 and later
 }
function DSpContext_GetDisplayID( inContext: DSpContextReferenceConst; var outDisplayID: DisplayIDType ): OSStatus; external name '_DSpContext_GetDisplayID';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4 *)


{
 *  DSpContext_GlobalToLocal()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in DrawSprocket.framework but deprecated in 10.4
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in DrawSprocketLib 1.0 and later
 }
function DSpContext_GlobalToLocal( inContext: DSpContextReferenceConst; var ioPoint: Point ): OSStatus; external name '_DSpContext_GlobalToLocal';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4 *)


{
 *  DSpContext_LocalToGlobal()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in DrawSprocket.framework but deprecated in 10.4
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in DrawSprocketLib 1.0 and later
 }
function DSpContext_LocalToGlobal( inContext: DSpContextReferenceConst; var ioPoint: Point ): OSStatus; external name '_DSpContext_LocalToGlobal';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4 *)


{
 *  DSpContext_SetVBLProc()
 *  
 *  Availability:
 *    Mac OS X:         not available
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in DrawSprocketLib 1.0 and later
 }


{
 *  DSpContext_GetFlattenedSize()
 *  
 *  Availability:
 *    Mac OS X:         not available
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in DrawSprocketLib 1.0 and later
 }


{
 *  DSpContext_Flatten()
 *  
 *  Availability:
 *    Mac OS X:         not available
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in DrawSprocketLib 1.0 and later
 }


{
 *  DSpContext_Restore()
 *  
 *  Availability:
 *    Mac OS X:         not available
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in DrawSprocketLib 1.0 and later
 }


{
 *  DSpContext_GetMonitorFrequency()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in DrawSprocket.framework but deprecated in 10.4
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in DrawSprocketLib 1.0 and later
 }
function DSpContext_GetMonitorFrequency( inContext: DSpContextReferenceConst; var outFrequency: Fixed ): OSStatus; external name '_DSpContext_GetMonitorFrequency';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4 *)


{
 *  DSpContext_SetMaxFrameRate()
 *  
 *  Availability:
 *    Mac OS X:         not available
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in DrawSprocketLib 1.0 and later
 }


{
 *  DSpContext_GetMaxFrameRate()
 *  
 *  Availability:
 *    Mac OS X:         not available
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in DrawSprocketLib 1.0 and later
 }


{
 *  DSpContext_SetState()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in DrawSprocket.framework but deprecated in 10.4
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in DrawSprocketLib 1.0 and later
 }
function DSpContext_SetState( inContext: DSpContextReference; inState: DSpContextState ): OSStatus; external name '_DSpContext_SetState';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4 *)


{
 *  DSpContext_GetState()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in DrawSprocket.framework but deprecated in 10.4
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in DrawSprocketLib 1.0 and later
 }
function DSpContext_GetState( inContext: DSpContextReferenceConst; var outState: DSpContextState ): OSStatus; external name '_DSpContext_GetState';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4 *)


{
 *  DSpContext_IsBusy()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in DrawSprocket.framework but deprecated in 10.4
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in DrawSprocketLib 1.0 and later
 }
function DSpContext_IsBusy( inContext: DSpContextReferenceConst; var outBusyFlag: Boolean ): OSStatus; external name '_DSpContext_IsBusy';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4 *)


{ dirty rectangles }
{
 *  DSpContext_SetDirtyRectGridSize()
 *  
 *  Availability:
 *    Mac OS X:         not available
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in DrawSprocketLib 1.0 and later
 }


{
 *  DSpContext_GetDirtyRectGridSize()
 *  
 *  Availability:
 *    Mac OS X:         not available
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in DrawSprocketLib 1.0 and later
 }


{
 *  DSpContext_GetDirtyRectGridUnits()
 *  
 *  Availability:
 *    Mac OS X:         not available
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in DrawSprocketLib 1.0 and later
 }


{
 *  DSpContext_InvalBackBufferRect()
 *  
 *  Availability:
 *    Mac OS X:         not available
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in DrawSprocketLib 1.0 and later
 }


{ underlays }
{
 *  DSpContext_SetUnderlayAltBuffer()
 *  
 *  Availability:
 *    Mac OS X:         not available
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in DrawSprocketLib 1.0 and later
 }


{
 *  DSpContext_GetUnderlayAltBuffer()
 *  
 *  Availability:
 *    Mac OS X:         not available
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in DrawSprocketLib 1.0 and later
 }


{ gamma }
{
 *  DSpContext_FadeGammaOut()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in DrawSprocket.framework but deprecated in 10.4
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in DrawSprocketLib 1.0 and later
 }
function DSpContext_FadeGammaOut( inContext: DSpContextReference; var inZeroIntensityColor: RGBColor ): OSStatus; external name '_DSpContext_FadeGammaOut';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4 *)


{
 *  DSpContext_FadeGammaIn()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in DrawSprocket.framework but deprecated in 10.4
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in DrawSprocketLib 1.0 and later
 }
function DSpContext_FadeGammaIn( inContext: DSpContextReference; var inZeroIntensityColor: RGBColor ): OSStatus; external name '_DSpContext_FadeGammaIn';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4 *)


{
 *  DSpContext_FadeGamma()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in DrawSprocket.framework but deprecated in 10.4
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in DrawSprocketLib 1.0 and later
 }
function DSpContext_FadeGamma( inContext: DSpContextReference; inPercentOfOriginalIntensity: SInt32; var inZeroIntensityColor: RGBColor ): OSStatus; external name '_DSpContext_FadeGamma';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4 *)


{ buffering }
{
 *  DSpContext_SwapBuffers()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in DrawSprocket.framework but deprecated in 10.4
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in DrawSprocketLib 1.0 and later
 }
function DSpContext_SwapBuffers( inContext: DSpContextReference; inBusyProc: DSpCallbackUPP; inUserRefCon: UnivPtr ): OSStatus; external name '_DSpContext_SwapBuffers';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4 *)


{
 *  DSpContext_GetBackBuffer()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in DrawSprocket.framework but deprecated in 10.4
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in DrawSprocketLib 1.0 and later
 }
function DSpContext_GetBackBuffer( inContext: DSpContextReference; inBufferKind: DSpBufferKind; var outBackBuffer: CGrafPtr ): OSStatus; external name '_DSpContext_GetBackBuffer';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4 *)


{
 *  DSpContext_GetFrontBuffer()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in DrawSprocket.framework but deprecated in 10.4
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in DrawSprocketLib 1.1 and later
 }
function DSpContext_GetFrontBuffer( inContext: DSpContextReferenceConst; var outFrontBuffer: CGrafPtr ): OSStatus; external name '_DSpContext_GetFrontBuffer';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4 *)


{ clut operations }
{
 *  DSpContext_SetCLUTEntries()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in DrawSprocket.framework but deprecated in 10.4
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in DrawSprocketLib 1.0 and later
 }
function DSpContext_SetCLUTEntries( inContext: DSpContextReference; const (*var*) inEntries: ColorSpec; inStartingEntry: UInt16; inLastEntry: UInt16 ): OSStatus; external name '_DSpContext_SetCLUTEntries';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4 *)


{
 *  DSpContext_GetCLUTEntries()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in DrawSprocket.framework but deprecated in 10.4
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in DrawSprocketLib 1.0 and later
 }
function DSpContext_GetCLUTEntries( inContext: DSpContextReferenceConst; var outEntries: ColorSpec; inStartingEntry: UInt16; inLastEntry: UInt16 ): OSStatus; external name '_DSpContext_GetCLUTEntries';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4 *)


{ blit operations }
{
 *  DSpBlit_Faster()
 *  
 *  Availability:
 *    Mac OS X:         not available
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in DrawSprocketLib 1.1 and later
 }


{
 *  DSpBlit_Fastest()
 *  
 *  Availability:
 *    Mac OS X:         not available
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in DrawSprocketLib 1.1 and later
 }


{$endc} {TARGET_OS_MAC}
{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}

end.
{$endc} {not MACOSALLINCLUDE}
