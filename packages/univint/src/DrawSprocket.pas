{
     File:       DrawSprocket.p
 
     Contains:   Games Sprockets: DrawSprocket interfaces
 
     Version:    Technology: Draw Sprocket 1.7
                 Release:    Universal Interfaces 3.4.2
 
     Copyright:  © 1996-2002 by Apple Computer, Inc., all rights reserved.
 
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

unit DrawSprocket;
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
uses MacTypes,Video,Events,Quickdraw,QDOffscreen,Displays,MacErrors;

{$ALIGN POWER}

{
********************************************************************************
** constants
********************************************************************************
}

type
	DSpDepthMask 				= SInt32;
const
	kDSpDepthMask_1				= $01;
	kDSpDepthMask_2				= $02;
	kDSpDepthMask_4				= $04;
	kDSpDepthMask_8				= $08;
	kDSpDepthMask_16			= $10;
	kDSpDepthMask_32			= $20;
	kDSpDepthMask_All			= -1;


type
	DSpColorNeeds 				= SInt32;
const
	kDSpColorNeeds_DontCare		= 0;
	kDSpColorNeeds_Request		= 1;
	kDSpColorNeeds_Require		= 2;


type
	DSpContextState 			= SInt32;
const
	kDSpContextState_Active		= 0;
	kDSpContextState_Paused		= 1;
	kDSpContextState_Inactive	= 2;

	{	 kDSpContextOption_QD3DAccel not yet implemented 	}

type
	DSpContextOption 			= SInt32;
const
																{     kDSpContextOption_QD3DAccel       = 1<<0, }
	kDSpContextOption_PageFlip	= $02;
	kDSpContextOption_DontSyncVBL = $04;
	kDSpContextOption_Stereoscopic = $08;


type
	DSpAltBufferOption 			= SInt32;
const
	kDSpAltBufferOption_RowBytesEqualsWidth = $01;


type
	DSpBufferKind 				= SInt32;
const
	kDSpBufferKind_Normal		= 0;


type
	DSpBlitMode 				= SInt32;
const
	kDSpBlitMode_Plain			= 0;
	kDSpBlitMode_SrcKey			= $01;
	kDSpBlitMode_DstKey			= $02;
	kDSpBlitMode_Interpolation	= $04;

	{	
	********************************************************************************
	** data types
	********************************************************************************
		}

type
	DSpAltBufferReference    = ^SInt32; { an opaque 32-bit type }
	DSpAltBufferReferencePtr = ^DSpAltBufferReference;  { when a var xx:DSpAltBufferReference parameter can be nil, it is changed to xx: DSpAltBufferReferencePtr }
	DSpContextReference    = ^SInt32; { an opaque 32-bit type }
	DSpContextReferencePtr = ^DSpContextReference;  { when a var xx:DSpContextReference parameter can be nil, it is changed to xx: DSpContextReferencePtr }
	DSpContextReferenceConst    = ^SInt32; { an opaque 32-bit type }
	DSpContextReferenceConstPtr = ^DSpContextReferenceConst;  { when a var xx:DSpContextReferenceConst parameter can be nil, it is changed to xx: DSpContextReferenceConstPtr }
{$ifc TYPED_FUNCTION_POINTERS}
	DSpEventProcPtr = function(var inEvent: EventRecord): boolean;
{$elsec}
	DSpEventProcPtr = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	DSpCallbackProcPtr = function(inContext: DSpContextReference; inRefCon: UnivPtr): boolean;
{$elsec}
	DSpCallbackProcPtr = ProcPtr;
{$endc}

{$ifc OPAQUE_UPP_TYPES}
	DSpEventUPP = ^SInt32; { an opaque UPP }
{$elsec}
	DSpEventUPP = DSpEventProcPtr;
{$endc}	
{$ifc OPAQUE_UPP_TYPES}
	DSpCallbackUPP = ^SInt32; { an opaque UPP }
{$elsec}
	DSpCallbackUPP = DSpCallbackProcPtr;
{$endc}	

const
	uppDSpEventProcInfo = $000000D1;
	uppDSpCallbackProcInfo = $000003D1;
{$ifc CALL_NOT_IN_CARBON}
	{
	 *  NewDSpEventUPP()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   available as macro/inline
	 *    CarbonLib:        not available
	 *    Mac OS X:         not available
	 	}
function NewDSpEventUPP(userRoutine: DSpEventProcPtr): DSpEventUPP; external name '_NewDSpEventUPP';
{
 *  NewDSpCallbackUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function NewDSpCallbackUPP(userRoutine: DSpCallbackProcPtr): DSpCallbackUPP; external name '_NewDSpCallbackUPP';
{
 *  DisposeDSpEventUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure DisposeDSpEventUPP(userUPP: DSpEventUPP); external name '_DisposeDSpEventUPP';
{
 *  DisposeDSpCallbackUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure DisposeDSpCallbackUPP(userUPP: DSpCallbackUPP); external name '_DisposeDSpCallbackUPP';
{
 *  InvokeDSpEventUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function InvokeDSpEventUPP(var inEvent: EventRecord; userRoutine: DSpEventUPP): boolean; external name '_InvokeDSpEventUPP';
{
 *  InvokeDSpCallbackUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function InvokeDSpCallbackUPP(inContext: DSpContextReference; inRefCon: UnivPtr; userRoutine: DSpCallbackUPP): boolean; external name '_InvokeDSpCallbackUPP';
{$endc}  {CALL_NOT_IN_CARBON}


type
	DSpContextAttributesPtr = ^DSpContextAttributes;
	DSpContextAttributes = record
		frequency:				Fixed;
		displayWidth:			UInt32;
		displayHeight:			UInt32;
		reserved1:				UInt32;
		reserved2:				UInt32;
		colorNeeds:				UInt32;
		colorTable:				CTabHandle;
		contextOptions:			OptionBits;
		backBufferDepthMask:	OptionBits;
		displayDepthMask:		OptionBits;
		backBufferBestDepth:	UInt32;
		displayBestDepth:		UInt32;
		pageCount:				UInt32;
		filler1,filler2,filler3:		SInt8;
		gameMustConfirmSwitch:	boolean;
		reserved3:				array [0..3] of UInt32;
	end;

	DSpAltBufferAttributesPtr = ^DSpAltBufferAttributes;
	DSpAltBufferAttributes = record
		width:					UInt32;
		height:					UInt32;
		options:				DSpAltBufferOption;
		reserved:				array [0..3] of UInt32;
	end;

	DSpBlitInfoPtr = ^DSpBlitInfo;
{$ifc TYPED_FUNCTION_POINTERS}
	DSpBlitDoneProc = procedure(info: DSpBlitInfoPtr);
{$elsec}
	DSpBlitDoneProc = ProcPtr;
{$endc}

	DSpBlitInfo = record
		completionFlag:			boolean;
		filler1, filler2, filler3:	SInt8;
		completionProc:			DSpBlitDoneProc;
		srcContext:				DSpContextReference;
		srcBuffer:				CGrafPtr;
		srcRect:				Rect;
		srcKey:					UInt32;
		dstContext:				DSpContextReference;
		dstBuffer:				CGrafPtr;
		dstRect:				Rect;
		dstKey:					UInt32;
		mode:					DSpBlitMode;
		reserved:				array [0..3] of UInt32;
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
	 *  DSpStartup()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in DrawSprocketLib 1.0 and later
	 *    CarbonLib:        not available
	 *    Mac OS X:         in version 10.0 and later
	 	}
function DSpStartup: OSStatus; external name '_DSpStartup';

{
 *  DSpShutdown()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DrawSprocketLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         in version 10.0 and later
 }
function DSpShutdown: OSStatus; external name '_DSpShutdown';

{
 *  DSpGetVersion()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DrawSprocketLib 1.7 and later
 *    CarbonLib:        not available
 *    Mac OS X:         in version 10.0 and later
 }
function DSpGetVersion: NumVersion; external name '_DSpGetVersion';

{
 *  DSpGetFirstContext()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DrawSprocketLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         in version 10.0 and later
 }
function DSpGetFirstContext(inDisplayID: DisplayIDType; var outContext: DSpContextReference): OSStatus; external name '_DSpGetFirstContext';

{
 *  DSpGetNextContext()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DrawSprocketLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         in version 10.0 and later
 }
function DSpGetNextContext(inCurrentContext: DSpContextReference; var outContext: DSpContextReference): OSStatus; external name '_DSpGetNextContext';

{
 *  DSpGetCurrentContext()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DrawSprocketLib 1.7 and later
 *    CarbonLib:        not available
 *    Mac OS X:         in version 10.0 and later
 }
function DSpGetCurrentContext(inDisplayID: DisplayIDType; var outContext: DSpContextReference): OSStatus; external name '_DSpGetCurrentContext';

{
 *  DSpFindBestContext()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DrawSprocketLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         in version 10.0 and later
 }
function DSpFindBestContext(inDesiredAttributes: DSpContextAttributesPtr; var outContext: DSpContextReference): OSStatus; external name '_DSpFindBestContext';

{
 *  DSpFindBestContextOnDisplayID()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DrawSprocketLib 1.7 and later
 *    CarbonLib:        not available
 *    Mac OS X:         in version 10.0 and later
 }
function DSpFindBestContextOnDisplayID(inDesiredAttributes: DSpContextAttributesPtr; var outContext: DSpContextReference; inDisplayID: DisplayIDType): OSStatus; external name '_DSpFindBestContextOnDisplayID';

{$ifc CALL_NOT_IN_CARBON}
{
 *  DSpCanUserSelectContext()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DrawSprocketLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function DSpCanUserSelectContext(inDesiredAttributes: DSpContextAttributesPtr; var outUserCanSelectContext: boolean): OSStatus; external name '_DSpCanUserSelectContext';

{
 *  DSpUserSelectContext()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DrawSprocketLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function DSpUserSelectContext(inDesiredAttributes: DSpContextAttributesPtr; inDialogDisplayLocation: DisplayIDType; inEventProc: DSpEventUPP; var outContext: DSpContextReference): OSStatus; external name '_DSpUserSelectContext';

{$endc}  {CALL_NOT_IN_CARBON}

{
 *  DSpProcessEvent()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DrawSprocketLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         in version 10.0 and later
 }
function DSpProcessEvent(var inEvent: EventRecord; var outEventWasProcessed: boolean): OSStatus; external name '_DSpProcessEvent';

{
 *  DSpSetBlankingColor()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DrawSprocketLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         in version 10.0 and later
 }
function DSpSetBlankingColor(const (*var*) inRGBColor: RGBColor): OSStatus; external name '_DSpSetBlankingColor';

{
 *  DSpSetDebugMode()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DrawSprocketLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         in version 10.0 and later
 }
function DSpSetDebugMode(inDebugMode: boolean): OSStatus; external name '_DSpSetDebugMode';

{
 *  DSpFindContextFromPoint()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DrawSprocketLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         in version 10.0 and later
 }
function DSpFindContextFromPoint(inGlobalPoint: Point; var outContext: DSpContextReference): OSStatus; external name '_DSpFindContextFromPoint';

{
 *  DSpGetMouse()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DrawSprocketLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         in version 10.0 and later
 }
function DSpGetMouse(var outGlobalPoint: Point): OSStatus; external name '_DSpGetMouse';

{
** alternate buffer operations
}
{$ifc CALL_NOT_IN_CARBON}
{
 *  DSpAltBuffer_New()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DrawSprocketLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function DSpAltBuffer_New(inContext: DSpContextReference; inVRAMBuffer: boolean; var inAttributes: DSpAltBufferAttributes; var outAltBuffer: DSpAltBufferReference): OSStatus; external name '_DSpAltBuffer_New';

{
 *  DSpAltBuffer_Dispose()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DrawSprocketLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function DSpAltBuffer_Dispose(inAltBuffer: DSpAltBufferReference): OSStatus; external name '_DSpAltBuffer_Dispose';

{
 *  DSpAltBuffer_InvalRect()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DrawSprocketLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function DSpAltBuffer_InvalRect(inAltBuffer: DSpAltBufferReference; const (*var*) inInvalidRect: Rect): OSStatus; external name '_DSpAltBuffer_InvalRect';

{
 *  DSpAltBuffer_GetCGrafPtr()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DrawSprocketLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function DSpAltBuffer_GetCGrafPtr(inAltBuffer: DSpAltBufferReference; inBufferKind: DSpBufferKind; var outCGrafPtr: CGrafPtr; var outGDevice: GDHandle): OSStatus; external name '_DSpAltBuffer_GetCGrafPtr';

{
** context operations
}
{ general }
{$endc}  {CALL_NOT_IN_CARBON}

{
 *  DSpContext_GetAttributes()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DrawSprocketLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         in version 10.0 and later
 }
function DSpContext_GetAttributes(inContext: DSpContextReferenceConst; outAttributes: DSpContextAttributesPtr): OSStatus; external name '_DSpContext_GetAttributes';

{
 *  DSpContext_Reserve()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DrawSprocketLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         in version 10.0 and later
 }
function DSpContext_Reserve(inContext: DSpContextReference; inDesiredAttributes: DSpContextAttributesPtr): OSStatus; external name '_DSpContext_Reserve';

{
 *  DSpContext_Queue()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DrawSprocketLib 1.7 and later
 *    CarbonLib:        not available
 *    Mac OS X:         in version 10.0 and later
 }
function DSpContext_Queue(inParentContext: DSpContextReference; inChildContext: DSpContextReference; inDesiredAttributes: DSpContextAttributesPtr): OSStatus; external name '_DSpContext_Queue';

{
 *  DSpContext_Switch()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DrawSprocketLib 1.7 and later
 *    CarbonLib:        not available
 *    Mac OS X:         in version 10.0 and later
 }
function DSpContext_Switch(inOldContext: DSpContextReference; inNewContext: DSpContextReference): OSStatus; external name '_DSpContext_Switch';

{
 *  DSpContext_Release()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DrawSprocketLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         in version 10.0 and later
 }
function DSpContext_Release(inContext: DSpContextReference): OSStatus; external name '_DSpContext_Release';

{
 *  DSpContext_Dispose()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         in version 10.0 and later
 }
function DSpContext_Dispose(inContext: DSpContextReference): OSStatus; external name '_DSpContext_Dispose';

{
 *  DSpContext_GetDisplayID()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DrawSprocketLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         in version 10.0 and later
 }
function DSpContext_GetDisplayID(inContext: DSpContextReferenceConst; var outDisplayID: DisplayIDType): OSStatus; external name '_DSpContext_GetDisplayID';

{
 *  DSpContext_GlobalToLocal()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DrawSprocketLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         in version 10.0 and later
 }
function DSpContext_GlobalToLocal(inContext: DSpContextReferenceConst; var ioPoint: Point): OSStatus; external name '_DSpContext_GlobalToLocal';

{
 *  DSpContext_LocalToGlobal()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DrawSprocketLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         in version 10.0 and later
 }
function DSpContext_LocalToGlobal(inContext: DSpContextReferenceConst; var ioPoint: Point): OSStatus; external name '_DSpContext_LocalToGlobal';

{$ifc CALL_NOT_IN_CARBON}
{
 *  DSpContext_SetVBLProc()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DrawSprocketLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function DSpContext_SetVBLProc(inContext: DSpContextReference; inProcPtr: DSpCallbackUPP; inRefCon: UnivPtr): OSStatus; external name '_DSpContext_SetVBLProc';

{
 *  DSpContext_GetFlattenedSize()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DrawSprocketLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function DSpContext_GetFlattenedSize(inContext: DSpContextReference; var outFlatContextSize: UInt32): OSStatus; external name '_DSpContext_GetFlattenedSize';

{
 *  DSpContext_Flatten()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DrawSprocketLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function DSpContext_Flatten(inContext: DSpContextReference; outFlatContext: UnivPtr): OSStatus; external name '_DSpContext_Flatten';

{
 *  DSpContext_Restore()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DrawSprocketLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function DSpContext_Restore(inFlatContext: UnivPtr; var outRestoredContext: DSpContextReference): OSStatus; external name '_DSpContext_Restore';

{$endc}  {CALL_NOT_IN_CARBON}

{
 *  DSpContext_GetMonitorFrequency()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DrawSprocketLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         in version 10.0 and later
 }
function DSpContext_GetMonitorFrequency(inContext: DSpContextReferenceConst; var outFrequency: Fixed): OSStatus; external name '_DSpContext_GetMonitorFrequency';

{$ifc CALL_NOT_IN_CARBON}
{
 *  DSpContext_SetMaxFrameRate()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DrawSprocketLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function DSpContext_SetMaxFrameRate(inContext: DSpContextReference; inMaxFPS: UInt32): OSStatus; external name '_DSpContext_SetMaxFrameRate';

{
 *  DSpContext_GetMaxFrameRate()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DrawSprocketLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function DSpContext_GetMaxFrameRate(inContext: DSpContextReferenceConst; var outMaxFPS: UInt32): OSStatus; external name '_DSpContext_GetMaxFrameRate';

{$endc}  {CALL_NOT_IN_CARBON}

{
 *  DSpContext_SetState()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DrawSprocketLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         in version 10.0 and later
 }
function DSpContext_SetState(inContext: DSpContextReference; inState: DSpContextState): OSStatus; external name '_DSpContext_SetState';

{
 *  DSpContext_GetState()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DrawSprocketLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         in version 10.0 and later
 }
function DSpContext_GetState(inContext: DSpContextReferenceConst; var outState: DSpContextState): OSStatus; external name '_DSpContext_GetState';

{
 *  DSpContext_IsBusy()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DrawSprocketLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         in version 10.0 and later
 }
function DSpContext_IsBusy(inContext: DSpContextReferenceConst; var outBusyFlag: boolean): OSStatus; external name '_DSpContext_IsBusy';

{ dirty rectangles }
{$ifc CALL_NOT_IN_CARBON}
{
 *  DSpContext_SetDirtyRectGridSize()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DrawSprocketLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function DSpContext_SetDirtyRectGridSize(inContext: DSpContextReference; inCellPixelWidth: UInt32; inCellPixelHeight: UInt32): OSStatus; external name '_DSpContext_SetDirtyRectGridSize';

{
 *  DSpContext_GetDirtyRectGridSize()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DrawSprocketLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function DSpContext_GetDirtyRectGridSize(inContext: DSpContextReferenceConst; var outCellPixelWidth: UInt32; var outCellPixelHeight: UInt32): OSStatus; external name '_DSpContext_GetDirtyRectGridSize';

{
 *  DSpContext_GetDirtyRectGridUnits()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DrawSprocketLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function DSpContext_GetDirtyRectGridUnits(inContext: DSpContextReferenceConst; var outCellPixelWidth: UInt32; var outCellPixelHeight: UInt32): OSStatus; external name '_DSpContext_GetDirtyRectGridUnits';

{
 *  DSpContext_InvalBackBufferRect()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DrawSprocketLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function DSpContext_InvalBackBufferRect(inContext: DSpContextReference; const (*var*) inRect: Rect): OSStatus; external name '_DSpContext_InvalBackBufferRect';

{ underlays }
{
 *  DSpContext_SetUnderlayAltBuffer()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DrawSprocketLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function DSpContext_SetUnderlayAltBuffer(inContext: DSpContextReference; inNewUnderlay: DSpAltBufferReference): OSStatus; external name '_DSpContext_SetUnderlayAltBuffer';

{
 *  DSpContext_GetUnderlayAltBuffer()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DrawSprocketLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function DSpContext_GetUnderlayAltBuffer(inContext: DSpContextReferenceConst; var outUnderlay: DSpAltBufferReference): OSStatus; external name '_DSpContext_GetUnderlayAltBuffer';

{ gamma }
{$endc}  {CALL_NOT_IN_CARBON}

{
 *  DSpContext_FadeGammaOut()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DrawSprocketLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         in version 10.0 and later
 }
function DSpContext_FadeGammaOut(inContext: DSpContextReference; var inZeroIntensityColor: RGBColor): OSStatus; external name '_DSpContext_FadeGammaOut';

{
 *  DSpContext_FadeGammaIn()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DrawSprocketLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         in version 10.0 and later
 }
function DSpContext_FadeGammaIn(inContext: DSpContextReference; var inZeroIntensityColor: RGBColor): OSStatus; external name '_DSpContext_FadeGammaIn';

{
 *  DSpContext_FadeGamma()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DrawSprocketLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         in version 10.0 and later
 }
function DSpContext_FadeGamma(inContext: DSpContextReference; inPercentOfOriginalIntensity: SInt32; var inZeroIntensityColor: RGBColor): OSStatus; external name '_DSpContext_FadeGamma';

{ buffering }
{
 *  DSpContext_SwapBuffers()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DrawSprocketLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         in version 10.0 and later
 }
function DSpContext_SwapBuffers(inContext: DSpContextReference; inBusyProc: DSpCallbackUPP; inUserRefCon: UnivPtr): OSStatus; external name '_DSpContext_SwapBuffers';

{
 *  DSpContext_GetBackBuffer()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DrawSprocketLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         in version 10.0 and later
 }
function DSpContext_GetBackBuffer(inContext: DSpContextReference; inBufferKind: DSpBufferKind; var outBackBuffer: CGrafPtr): OSStatus; external name '_DSpContext_GetBackBuffer';

{
 *  DSpContext_GetFrontBuffer()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DrawSprocketLib 1.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         in version 10.0 and later
 }
function DSpContext_GetFrontBuffer(inContext: DSpContextReferenceConst; var outFrontBuffer: CGrafPtr): OSStatus; external name '_DSpContext_GetFrontBuffer';

{ clut operations }
{
 *  DSpContext_SetCLUTEntries()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DrawSprocketLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         in version 10.0 and later
 }
function DSpContext_SetCLUTEntries(inContext: DSpContextReference; const (*var*) inEntries: ColorSpec; inStartingEntry: UInt16; inLastEntry: UInt16): OSStatus; external name '_DSpContext_SetCLUTEntries';

{
 *  DSpContext_GetCLUTEntries()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DrawSprocketLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         in version 10.0 and later
 }
function DSpContext_GetCLUTEntries(inContext: DSpContextReferenceConst; var outEntries: ColorSpec; inStartingEntry: UInt16; inLastEntry: UInt16): OSStatus; external name '_DSpContext_GetCLUTEntries';

{ blit operations }
{$ifc CALL_NOT_IN_CARBON}
{
 *  DSpBlit_Faster()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DrawSprocketLib 1.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function DSpBlit_Faster(inBlitInfo: DSpBlitInfoPtr; inAsyncFlag: boolean): OSStatus; external name '_DSpBlit_Faster';

{
 *  DSpBlit_Fastest()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DrawSprocketLib 1.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function DSpBlit_Fastest(inBlitInfo: DSpBlitInfoPtr; inAsyncFlag: boolean): OSStatus; external name '_DSpBlit_Fastest';


{$endc}  {CALL_NOT_IN_CARBON}

{$ALIGN MAC68K}


end.
