{
     File:       QuickTimeVR.p
 
     Contains:   QuickTime VR interfaces
 
     Version:    Technology: QuickTime VR 5.0
                 Release:    Universal Interfaces 3.4.2
 
     Copyright:  © 1997-2002 by Apple Computer, Inc., all rights reserved.
 
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

unit QuickTimeVR;
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
uses MacTypes,Quickdraw,Movies;


{$ALIGN MAC68K}


type
	QTVRInstance    = ^SInt32; { an opaque 32-bit type }
	QTVRInstancePtr = ^QTVRInstance;  { when a var xx:QTVRInstance parameter can be nil, it is changed to xx: QTVRInstancePtr }


const
	kQTVRControllerSubType		= $63747970 (* 'ctyp' *);
	kQTVRQTVRType				= $71747672 (* 'qtvr' *);
	kQTVRPanoramaType			= $70616E6F (* 'pano' *);
	kQTVRObjectType				= $6F626A65 (* 'obje' *);
	kQTVROldPanoType			= $5354706E (* 'STpn' *);						{  Used in QTVR 1.0 release }
	kQTVROldObjectType			= $73746E61 (* 'stna' *);						{  Used in QTVR 1.0 release }

{$ifc TARGET_OS_MAC}
{$elsec}
{$endc}  {TARGET_OS_MAC}

	{  QTVR hot spot types }
	kQTVRHotSpotLinkType		= $6C696E6B (* 'link' *);
	kQTVRHotSpotURLType			= $75726C20 (* 'url ' *);
	kQTVRHotSpotUndefinedType	= $756E6466 (* 'undf' *);

	{  Special Values for nodeID in QTVRGoToNodeID }
	kQTVRCurrentNode			= 0;
	kQTVRPreviousNode			= $80000000;
	kQTVRDefaultNode			= $80000001;

	{  Panorama correction modes used for the kQTVRImagingCorrection imaging property }
	kQTVRNoCorrection			= 0;
	kQTVRPartialCorrection		= 1;
	kQTVRFullCorrection			= 2;

	{  Imaging Modes used by QTVRSetImagingProperty, QTVRGetImagingProperty, QTVRUpdate, QTVRBeginUpdate }

type
	QTVRImagingMode 			= UInt32;
const
	kQTVRStatic					= 1;
	kQTVRMotion					= 2;
	kQTVRCurrentMode			= 0;							{  Special Value for QTVRUpdate }
	kQTVRAllModes				= 100;							{  Special value for QTVRSetProperty }

	{  Imaging Properties used by QTVRSetImagingProperty, QTVRGetImagingProperty }
	kQTVRImagingCorrection		= 1;
	kQTVRImagingQuality			= 2;
	kQTVRImagingDirectDraw		= 3;
	kQTVRImagingCurrentMode		= 100;							{  Get Only }

	{  OR the above with kImagingDefaultValue to get/set the default value }
	kImagingDefaultValue		= $80000000;

	{  Transition Types used by QTVRSetTransitionProperty, QTVREnableTransition }
	kQTVRTransitionSwing		= 1;

	{  Transition Properties QTVRSetTransitionProperty }
	kQTVRTransitionSpeed		= 1;
	kQTVRTransitionDirection	= 2;

	{  Constraint values used to construct value returned by GetConstraintStatus }
	kQTVRUnconstrained			= 0;
	kQTVRCantPanLeft			= $00000001;
	kQTVRCantPanRight			= $00000002;
	kQTVRCantPanUp				= $00000004;
	kQTVRCantPanDown			= $00000008;
	kQTVRCantZoomIn				= $00000010;
	kQTVRCantZoomOut			= $00000020;
	kQTVRCantTranslateLeft		= $00000040;
	kQTVRCantTranslateRight		= $00000080;
	kQTVRCantTranslateUp		= $00000100;
	kQTVRCantTranslateDown		= $00000200;

	{  Object-only mouse mode values used to construct value returned by QTVRGetCurrentMouseMode }
	kQTVRPanning				= $00000001;					{  standard objects, "object only" controllers }
	kQTVRTranslating			= $00000002;					{  all objects }
	kQTVRZooming				= $00000004;					{  all objects }
	kQTVRScrolling				= $00000008;					{  standard object arrow scrollers and joystick object }
	kQTVRSelecting				= $00000010;					{  object absolute controller }

	{  Properties for use with QTVRSetInteractionProperty/GetInteractionProperty }
	kQTVRInteractionMouseClickHysteresis = 1;					{  pixels within which the mouse is considered not to have moved (UInt16) }
	kQTVRInteractionMouseClickTimeout = 2;						{  ticks after which a mouse click times out and turns into panning (UInt32) }
	kQTVRInteractionPanTiltSpeed = 3;							{  control the relative pan/tilt speed from 1 (slowest) to 10 (fastest). (UInt32) Default is 5; }
	kQTVRInteractionZoomSpeed	= 4;							{  control the relative zooming speed from 1 (slowest) to 10 (fastest). (UInt32) Default is 5; }
	kQTVRInteractionTranslateOnMouseDown = 101;					{  Holding MouseDown with this setting translates zoomed object movies (Boolean) }
	kQTVRInteractionMouseMotionScale = 102;						{  The maximum angle of rotation caused by dragging across the display window. (* float) }
	kQTVRInteractionNudgeMode	= 103;							{  A QTVRNudgeMode: rotate, translate, or the same as the current mouse mode. Requires QTVR 2.1 }

	{  OR the above with kQTVRInteractionDefaultValue to get/set the default value }
	kQTVRInteractionDefaultValue = $80000000;


	{  Geometry constants used in QTVRSetBackBufferPrefs, QTVRGetBackBufferSettings, QTVRGetBackBufferMemInfo }
	kQTVRUseMovieGeometry		= 0;
	kQTVRVerticalCylinder		= $7663796C (* 'vcyl' *);
	kQTVRHorizontalCylinder		= $6863796C (* 'hcyl' *);
	kQTVRCube					= $63756265 (* 'cube' *);

	{  Resolution constants used in QTVRSetBackBufferPrefs, QTVRGetBackBufferSettings, QTVRGetBackBufferMemInfo }
	kQTVRDefaultRes				= 0;
	kQTVRFullRes				= $00000001;
	kQTVRHalfRes				= $00000002;
	kQTVRQuarterRes				= $00000004;

	{  QTVR-specific pixelFormat constants used in QTVRSetBackBufferPrefs, QTVRGetBackBufferSettings, QTVRGetBackBufferMemInfo }
	kQTVRUseMovieDepth			= 0;

	{  Cache Size Pref constants used in QTVRSetBackBufferPrefs, QTVRGetBackBufferSettings }
	kQTVRMinimumCache			= -1;
	kQTVRSuggestedCache			= 0;
	kQTVRFullCache				= 1;

	{  Angular units used by QTVRSetAngularUnits }

type
	QTVRAngularUnits 			= UInt32;
const
	kQTVRDegrees				= 0;
	kQTVRRadians				= 1;

	{  Values for enableFlag parameter in QTVREnableHotSpot }
	kQTVRHotSpotID				= 0;
	kQTVRHotSpotType			= 1;
	kQTVRAllHotSpots			= 2;

	{  Values for viewParameter for QTVRSet/GetViewParameter }
	kQTVRPanAngle				= $0100;						{  default units; &float, &float }
	kQTVRTiltAngle				= $0101;						{  default units; &float, &float }
	kQTVRFieldOfViewAngle		= $0103;						{  default units; &float, &float }
	kQTVRViewCenter				= $0104;						{  pixels (per object movies); &QTVRFloatPoint, &QTVRFloatPoint }
	kQTVRHotSpotsVisible		= $0200;						{  Boolean, &Boolean }

	{  Values for flagsIn for QTVRSet/GetViewParameter }
	kQTVRValueIsRelative		= $00000001;					{  Is the value absolute or relative to the current value? }
	kQTVRValueIsRate			= $00000002;					{  Is the value absolute or a rate of change to be applied? }
	kQTVRValueIsUserPrefRelative = $00000004;					{  Is the value a percentage of the user rate pref? }

	{  Values for kind parameter in QTVRGet/SetConstraints, QTVRGetViewingLimits }
	kQTVRPan					= 0;
	kQTVRTilt					= 1;
	kQTVRFieldOfView			= 2;
	kQTVRViewCenterH			= 4;							{  WrapAndConstrain only }
	kQTVRViewCenterV			= 5;							{  WrapAndConstrain only }

	{  Values for setting parameter in QTVRSetAnimationSetting, QTVRGetAnimationSetting }

type
	QTVRObjectAnimationSetting 	= UInt32;
const
																{  View Frame Animation Settings }
	kQTVRPalindromeViewFrames	= 1;
	kQTVRStartFirstViewFrame	= 2;
	kQTVRDontLoopViewFrames		= 3;
	kQTVRPlayEveryViewFrame		= 4;							{  Requires QTVR 2.1 (kQTVRAPIMajorVersion02 + kQTVRAPIMinorVersion10) }
																{  View Animation Settings }
	kQTVRSyncViewToFrameRate	= 16;
	kQTVRPalindromeViews		= 17;
	kQTVRPlayStreamingViews		= 18;							{  Requires QTVR 2.1 (kQTVRAPIMajorVersion02 + kQTVRAPIMinorVersion10) }


type
	QTVRControlSetting 			= UInt32;
const
	kQTVRWrapPan				= 1;
	kQTVRWrapTilt				= 2;
	kQTVRCanZoom				= 3;
	kQTVRReverseHControl		= 4;
	kQTVRReverseVControl		= 5;
	kQTVRSwapHVControl			= 6;
	kQTVRTranslation			= 7;


type
	QTVRViewStateType 			= UInt32;
const
	kQTVRDefault				= 0;
	kQTVRCurrent				= 2;
	kQTVRMouseDown				= 3;


type
	QTVRNudgeControl 			= UInt32;
const
	kQTVRRight					= 0;
	kQTVRUpRight				= 45;
	kQTVRUp						= 90;
	kQTVRUpLeft					= 135;
	kQTVRLeft					= 180;
	kQTVRDownLeft				= 225;
	kQTVRDown					= 270;
	kQTVRDownRight				= 315;


type
	QTVRNudgeMode 				= UInt32;
const
	kQTVRNudgeRotate			= 0;
	kQTVRNudgeTranslate			= 1;
	kQTVRNudgeSameAsMouse		= 2;


	{  Flags to control elements of the QTVR control bar (set via mcActionSetFlags)  }
	mcFlagQTVRSuppressBackBtn	= $00010000;
	mcFlagQTVRSuppressZoomBtns	= $00020000;
	mcFlagQTVRSuppressHotSpotBtn = $00040000;
	mcFlagQTVRSuppressTranslateBtn = $00080000;
	mcFlagQTVRSuppressHelpText	= $00100000;
	mcFlagQTVRSuppressHotSpotNames = $00200000;
	mcFlagQTVRExplicitFlagSet	= $80000000;					{  bits 0->30 should be interpreted as "explicit on" for the corresponding suppression bits }

	{  Cursor types used in type field of QTVRCursorRecord }
	kQTVRUseDefaultCursor		= 0;
	kQTVRStdCursorType			= 1;
	kQTVRColorCursorType		= 2;

	{  Values for flags parameter in QTVRMouseOverHotSpot callback }
	kQTVRHotSpotEnter			= 0;
	kQTVRHotSpotWithin			= 1;
	kQTVRHotSpotLeave			= 2;

	{  Values for flags parameter in QTVRSetPrescreenImagingCompleteProc }
	kQTVRPreScreenEveryIdle		= $00000001;					{  Requires QTVR 2.1 (kQTVRAPIMajorVersion02 + kQTVRAPIMinorVersion10) }

	{  Values for flags field of areasOfInterest in QTVRSetBackBufferImagingProc }
	kQTVRBackBufferEveryUpdate	= $00000001;
	kQTVRBackBufferEveryIdle	= $00000002;
	kQTVRBackBufferAlwaysRefresh = $00000004;
	kQTVRBackBufferHorizontal	= $00000008;					{  Requires that backbuffer proc be long-rowBytes aware (gestaltQDHasLongRowBytes) }

	{  Values for flagsIn parameter in QTVRBackBufferImaging callback }
	kQTVRBackBufferRectVisible	= $00000001;
	kQTVRBackBufferWasRefreshed	= $00000002;

	{  Values for flagsOut parameter in QTVRBackBufferImaging callback }
	kQTVRBackBufferFlagDidDraw	= $00000001;
	kQTVRBackBufferFlagLastFlag	= $80000000;

	{  QTVRCursorRecord used in QTVRReplaceCursor }

type
	QTVRCursorRecordPtr = ^QTVRCursorRecord;
	QTVRCursorRecord = record
		theType:				UInt16;									{  field was previously named "type" }
		rsrcID:					SInt16;
		handle:					Handle_fix;
	end;

	QTVRFloatPointPtr = ^QTVRFloatPoint;
	QTVRFloatPoint = record
		x:						Single;
		y:						Single;
	end;

	{  Struct used for areasOfInterest parameter in QTVRSetBackBufferImagingProc }
	QTVRAreaOfInterestPtr = ^QTVRAreaOfInterest;
	QTVRAreaOfInterest = record
		panAngle:				Single;
		tiltAngle:				Single;
		width:					Single;
		height:					Single;
		flags:					UInt32;
	end;

	{
	  =================================================================================================
	   Callback routines 
	  -------------------------------------------------------------------------------------------------
	}

{$ifc TYPED_FUNCTION_POINTERS}
	QTVRLeavingNodeProcPtr = function(qtvr: QTVRInstance; fromNodeID: UInt32; toNodeID: UInt32; var cancel: boolean; refCon: SInt32): OSErr;
{$elsec}
	QTVRLeavingNodeProcPtr = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	QTVREnteringNodeProcPtr = function(qtvr: QTVRInstance; nodeID: UInt32; refCon: SInt32): OSErr;
{$elsec}
	QTVREnteringNodeProcPtr = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	QTVRMouseOverHotSpotProcPtr = function(qtvr: QTVRInstance; hotSpotID: UInt32; flags: UInt32; refCon: SInt32): OSErr;
{$elsec}
	QTVRMouseOverHotSpotProcPtr = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	QTVRImagingCompleteProcPtr = function(qtvr: QTVRInstance; refCon: SInt32): OSErr;
{$elsec}
	QTVRImagingCompleteProcPtr = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	QTVRBackBufferImagingProcPtr = function(qtvr: QTVRInstance; var drawRect: Rect; areaIndex: UInt16; flagsIn: UInt32; var flagsOut: UInt32; refCon: SInt32): OSErr;
{$elsec}
	QTVRBackBufferImagingProcPtr = ProcPtr;
{$endc}

{$ifc OPAQUE_UPP_TYPES}
	QTVRLeavingNodeUPP = ^SInt32; { an opaque UPP }
{$elsec}
	QTVRLeavingNodeUPP = UniversalProcPtr;
{$endc}	
{$ifc OPAQUE_UPP_TYPES}
	QTVREnteringNodeUPP = ^SInt32; { an opaque UPP }
{$elsec}
	QTVREnteringNodeUPP = UniversalProcPtr;
{$endc}	
{$ifc OPAQUE_UPP_TYPES}
	QTVRMouseOverHotSpotUPP = ^SInt32; { an opaque UPP }
{$elsec}
	QTVRMouseOverHotSpotUPP = UniversalProcPtr;
{$endc}	
{$ifc OPAQUE_UPP_TYPES}
	QTVRImagingCompleteUPP = ^SInt32; { an opaque UPP }
{$elsec}
	QTVRImagingCompleteUPP = UniversalProcPtr;
{$endc}	
{$ifc OPAQUE_UPP_TYPES}
	QTVRBackBufferImagingUPP = ^SInt32; { an opaque UPP }
{$elsec}
	QTVRBackBufferImagingUPP = UniversalProcPtr;
{$endc}	

const
	uppQTVRLeavingNodeProcInfo = $0000FFE0;
	uppQTVREnteringNodeProcInfo = $00000FE0;
	uppQTVRMouseOverHotSpotProcInfo = $00003FE0;
	uppQTVRImagingCompleteProcInfo = $000003E0;
	uppQTVRBackBufferImagingProcInfo = $0003FBE0;
	{
	 *  NewQTVRLeavingNodeUPP()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   available as macro/inline
	 *    CarbonLib:        in CarbonLib 1.1 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
function NewQTVRLeavingNodeUPP(userRoutine: QTVRLeavingNodeProcPtr): QTVRLeavingNodeUPP; external name '_NewQTVRLeavingNodeUPP'; { old name was NewQTVRLeavingNodeProc }
{
 *  NewQTVREnteringNodeUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewQTVREnteringNodeUPP(userRoutine: QTVREnteringNodeProcPtr): QTVREnteringNodeUPP; external name '_NewQTVREnteringNodeUPP'; { old name was NewQTVREnteringNodeProc }
{
 *  NewQTVRMouseOverHotSpotUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewQTVRMouseOverHotSpotUPP(userRoutine: QTVRMouseOverHotSpotProcPtr): QTVRMouseOverHotSpotUPP; external name '_NewQTVRMouseOverHotSpotUPP'; { old name was NewQTVRMouseOverHotSpotProc }
{
 *  NewQTVRImagingCompleteUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewQTVRImagingCompleteUPP(userRoutine: QTVRImagingCompleteProcPtr): QTVRImagingCompleteUPP; external name '_NewQTVRImagingCompleteUPP'; { old name was NewQTVRImagingCompleteProc }
{
 *  NewQTVRBackBufferImagingUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewQTVRBackBufferImagingUPP(userRoutine: QTVRBackBufferImagingProcPtr): QTVRBackBufferImagingUPP; external name '_NewQTVRBackBufferImagingUPP'; { old name was NewQTVRBackBufferImagingProc }
{
 *  DisposeQTVRLeavingNodeUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeQTVRLeavingNodeUPP(userUPP: QTVRLeavingNodeUPP); external name '_DisposeQTVRLeavingNodeUPP';
{
 *  DisposeQTVREnteringNodeUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeQTVREnteringNodeUPP(userUPP: QTVREnteringNodeUPP); external name '_DisposeQTVREnteringNodeUPP';
{
 *  DisposeQTVRMouseOverHotSpotUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeQTVRMouseOverHotSpotUPP(userUPP: QTVRMouseOverHotSpotUPP); external name '_DisposeQTVRMouseOverHotSpotUPP';
{
 *  DisposeQTVRImagingCompleteUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeQTVRImagingCompleteUPP(userUPP: QTVRImagingCompleteUPP); external name '_DisposeQTVRImagingCompleteUPP';
{
 *  DisposeQTVRBackBufferImagingUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeQTVRBackBufferImagingUPP(userUPP: QTVRBackBufferImagingUPP); external name '_DisposeQTVRBackBufferImagingUPP';
{
 *  InvokeQTVRLeavingNodeUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function InvokeQTVRLeavingNodeUPP(qtvr: QTVRInstance; fromNodeID: UInt32; toNodeID: UInt32; var cancel: boolean; refCon: SInt32; userRoutine: QTVRLeavingNodeUPP): OSErr; external name '_InvokeQTVRLeavingNodeUPP'; { old name was CallQTVRLeavingNodeProc }
{
 *  InvokeQTVREnteringNodeUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function InvokeQTVREnteringNodeUPP(qtvr: QTVRInstance; nodeID: UInt32; refCon: SInt32; userRoutine: QTVREnteringNodeUPP): OSErr; external name '_InvokeQTVREnteringNodeUPP'; { old name was CallQTVREnteringNodeProc }
{
 *  InvokeQTVRMouseOverHotSpotUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function InvokeQTVRMouseOverHotSpotUPP(qtvr: QTVRInstance; hotSpotID: UInt32; flags: UInt32; refCon: SInt32; userRoutine: QTVRMouseOverHotSpotUPP): OSErr; external name '_InvokeQTVRMouseOverHotSpotUPP'; { old name was CallQTVRMouseOverHotSpotProc }
{
 *  InvokeQTVRImagingCompleteUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function InvokeQTVRImagingCompleteUPP(qtvr: QTVRInstance; refCon: SInt32; userRoutine: QTVRImagingCompleteUPP): OSErr; external name '_InvokeQTVRImagingCompleteUPP'; { old name was CallQTVRImagingCompleteProc }
{
 *  InvokeQTVRBackBufferImagingUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function InvokeQTVRBackBufferImagingUPP(qtvr: QTVRInstance; var drawRect: Rect; areaIndex: UInt16; flagsIn: UInt32; var flagsOut: UInt32; refCon: SInt32; userRoutine: QTVRBackBufferImagingUPP): OSErr; external name '_InvokeQTVRBackBufferImagingUPP'; { old name was CallQTVRBackBufferImagingProc }
{
  =================================================================================================
    QTVR Intercept Struct, Callback, Routine Descriptors 
  -------------------------------------------------------------------------------------------------
}


type
	QTVRProcSelector 			= UInt32;
const
	kQTVRSetPanAngleSelector	= $2000;
	kQTVRSetTiltAngleSelector	= $2001;
	kQTVRSetFieldOfViewSelector	= $2002;
	kQTVRSetViewCenterSelector	= $2003;
	kQTVRMouseEnterSelector		= $2004;
	kQTVRMouseWithinSelector	= $2005;
	kQTVRMouseLeaveSelector		= $2006;
	kQTVRMouseDownSelector		= $2007;
	kQTVRMouseStillDownSelector	= $2008;
	kQTVRMouseUpSelector		= $2009;
	kQTVRTriggerHotSpotSelector	= $200A;
	kQTVRGetHotSpotTypeSelector	= $200B;						{  Requires QTVR 2.1 (kQTVRAPIMajorVersion02 + kQTVRAPIMinorVersion10) }
	kQTVRSetViewParameterSelector = $200C;						{  Requires QTVR 5.0 (kQTVRAPIMajorVersion05 + kQTVRAPIMinorVersion00) }
	kQTVRGetViewParameterSelector = $200D;						{  Requires QTVR 5.0 (kQTVRAPIMajorVersion05 + kQTVRAPIMinorVersion00) }


type
	QTVRInterceptRecordPtr = ^QTVRInterceptRecord;
	QTVRInterceptRecord = record
		reserved1:				SInt32;
		selector:				SInt32;
		reserved2:				SInt32;
		reserved3:				SInt32;
		paramCount:				SInt32;
		parameter:				array [0..5] of Ptr;
	end;

	QTVRInterceptPtr					= ^QTVRInterceptRecord;
	{  Prototype for Intercept Proc callback }
{$ifc TYPED_FUNCTION_POINTERS}
	QTVRInterceptProcPtr = procedure(qtvr: QTVRInstance; qtvrMsg: QTVRInterceptPtr; refCon: SInt32; var cancel: boolean);
{$elsec}
	QTVRInterceptProcPtr = ProcPtr;
{$endc}

{$ifc OPAQUE_UPP_TYPES}
	QTVRInterceptUPP = ^SInt32; { an opaque UPP }
{$elsec}
	QTVRInterceptUPP = UniversalProcPtr;
{$endc}	

const
	uppQTVRInterceptProcInfo = $00003FC0;
	{
	 *  NewQTVRInterceptUPP()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   available as macro/inline
	 *    CarbonLib:        in CarbonLib 1.1 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
function NewQTVRInterceptUPP(userRoutine: QTVRInterceptProcPtr): QTVRInterceptUPP; external name '_NewQTVRInterceptUPP'; { old name was NewQTVRInterceptProc }
{
 *  DisposeQTVRInterceptUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeQTVRInterceptUPP(userUPP: QTVRInterceptUPP); external name '_DisposeQTVRInterceptUPP';
{
 *  InvokeQTVRInterceptUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure InvokeQTVRInterceptUPP(qtvr: QTVRInstance; qtvrMsg: QTVRInterceptPtr; refCon: SInt32; var cancel: boolean; userRoutine: QTVRInterceptUPP); external name '_InvokeQTVRInterceptUPP'; { old name was CallQTVRInterceptProc }
{
  =================================================================================================
    Initialization QTVR calls 
  -------------------------------------------------------------------------------------------------
   Requires QTVR 2.1 (kQTVRAPIMajorVersion02 + kQTVRAPIMinorVersion10) and only work on Non-Macintosh platforms
}
{$ifc NOT TARGET_OS_MAC}
{$ifc CALL_NOT_IN_CARBON}
{
 *  InitializeQTVR()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 *    Windows:          in QTVR.lib 2.1 and later
 }
function InitializeQTVR: OSErr; external name '_InitializeQTVR';

{
 *  TerminateQTVR()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 *    Windows:          in QTVR.lib 2.1 and later
 }
function TerminateQTVR: OSErr; external name '_TerminateQTVR';

{$endc}  {CALL_NOT_IN_CARBON}
{$endc}

{
  =================================================================================================
    General QTVR calls 
  -------------------------------------------------------------------------------------------------
}
{
 *  QTVRGetQTVRTrack()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRGetQTVRTrack(theMovie: Movie; index: SInt32): Track; external name '_QTVRGetQTVRTrack';

{
 *  QTVRGetQTVRInstance()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRGetQTVRInstance(var qtvr: QTVRInstance; qtvrTrack: Track; mc: MovieController): OSErr; external name '_QTVRGetQTVRInstance';

{
  =================================================================================================
    Viewing Angles and Zooming 
  -------------------------------------------------------------------------------------------------
}

{  QTVRSetViewParameter requires QTVR 5.0 (kQTVRAPIMajorVersion05 + kQTVRAPIMinorVersion00) }
{
 *  QTVRSetViewParameter()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeVRLib 5.0 and later
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTVR.lib 5.0 and later
 }
function QTVRSetViewParameter(qtvr: QTVRInstance; viewParameter: UInt32; value: UnivPtr; flagsIn: UInt32): OSErr; external name '_QTVRSetViewParameter';

{  QTVRGetViewParameter requires QTVR 5.0 (kQTVRAPIMajorVersion05 + kQTVRAPIMinorVersion00) }
{
 *  QTVRGetViewParameter()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeVRLib 5.0 and later
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTVR.lib 5.0 and later
 }
function QTVRGetViewParameter(qtvr: QTVRInstance; viewParameter: UInt32; value: UnivPtr; flagsIn: UInt32; var flagsOut: UInt32): OSErr; external name '_QTVRGetViewParameter';

{
 *  QTVRSetPanAngle()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRSetPanAngle(qtvr: QTVRInstance; panAngle: Single): OSErr; external name '_QTVRSetPanAngle';

{
 *  QTVRGetPanAngle()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRGetPanAngle(qtvr: QTVRInstance): Single; external name '_QTVRGetPanAngle';

{
 *  QTVRSetTiltAngle()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRSetTiltAngle(qtvr: QTVRInstance; tiltAngle: Single): OSErr; external name '_QTVRSetTiltAngle';

{
 *  QTVRGetTiltAngle()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRGetTiltAngle(qtvr: QTVRInstance): Single; external name '_QTVRGetTiltAngle';

{
 *  QTVRSetFieldOfView()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRSetFieldOfView(qtvr: QTVRInstance; fieldOfView: Single): OSErr; external name '_QTVRSetFieldOfView';

{
 *  QTVRGetFieldOfView()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRGetFieldOfView(qtvr: QTVRInstance): Single; external name '_QTVRGetFieldOfView';

{
 *  QTVRShowDefaultView()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRShowDefaultView(qtvr: QTVRInstance): OSErr; external name '_QTVRShowDefaultView';

{  Object Specific }
{
 *  QTVRSetViewCenter()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRSetViewCenter(qtvr: QTVRInstance; const (*var*) viewCenter: QTVRFloatPoint): OSErr; external name '_QTVRSetViewCenter';

{
 *  QTVRGetViewCenter()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRGetViewCenter(qtvr: QTVRInstance; var viewCenter: QTVRFloatPoint): OSErr; external name '_QTVRGetViewCenter';

{
 *  QTVRNudge()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRNudge(qtvr: QTVRInstance; direction: QTVRNudgeControl): OSErr; external name '_QTVRNudge';

{  QTVRInteractionNudge requires QTVR 2.1 (kQTVRAPIMajorVersion02 + kQTVRAPIMinorVersion10) }
{
 *  QTVRInteractionNudge()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.1 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRInteractionNudge(qtvr: QTVRInstance; direction: QTVRNudgeControl): OSErr; external name '_QTVRInteractionNudge';

{
  =================================================================================================
    Scene and Node Location Information 
  -------------------------------------------------------------------------------------------------
}

{
 *  QTVRGetVRWorld()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRGetVRWorld(qtvr: QTVRInstance; var VRWorld: QTAtomContainer): OSErr; external name '_QTVRGetVRWorld';

{
 *  QTVRGetNodeInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRGetNodeInfo(qtvr: QTVRInstance; nodeID: UInt32; var nodeInfo: QTAtomContainer): OSErr; external name '_QTVRGetNodeInfo';

{
 *  QTVRGoToNodeID()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRGoToNodeID(qtvr: QTVRInstance; nodeID: UInt32): OSErr; external name '_QTVRGoToNodeID';

{
 *  QTVRGetCurrentNodeID()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRGetCurrentNodeID(qtvr: QTVRInstance): UInt32; external name '_QTVRGetCurrentNodeID';

{
 *  QTVRGetNodeType()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRGetNodeType(qtvr: QTVRInstance; nodeID: UInt32): SInt32; external name '_QTVRGetNodeType';

{
  =================================================================================================
    Hot Spot related calls 
  -------------------------------------------------------------------------------------------------
}

{
 *  QTVRPtToHotSpotID()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRPtToHotSpotID(qtvr: QTVRInstance; pt: Point; var hotSpotID: UInt32): OSErr; external name '_QTVRPtToHotSpotID';

{  QTVRGetHotSpotType requires QTVR 2.1 (kQTVRAPIMajorVersion02 + kQTVRAPIMinorVersion10) }
{
 *  QTVRGetHotSpotType()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.1 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRGetHotSpotType(qtvr: QTVRInstance; hotSpotID: UInt32; var hotSpotType: OSType): OSErr; external name '_QTVRGetHotSpotType';

{
 *  QTVRTriggerHotSpot()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRTriggerHotSpot(qtvr: QTVRInstance; hotSpotID: UInt32; nodeInfo: QTAtomContainer; selectedAtom: QTAtom): OSErr; external name '_QTVRTriggerHotSpot';

{
 *  QTVRSetMouseOverHotSpotProc()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRSetMouseOverHotSpotProc(qtvr: QTVRInstance; mouseOverHotSpotProc: QTVRMouseOverHotSpotUPP; refCon: SInt32; flags: UInt32): OSErr; external name '_QTVRSetMouseOverHotSpotProc';

{
 *  QTVREnableHotSpot()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVREnableHotSpot(qtvr: QTVRInstance; enableFlag: UInt32; hotSpotValue: UInt32; enable: boolean): OSErr; external name '_QTVREnableHotSpot';

{
 *  QTVRGetVisibleHotSpots()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRGetVisibleHotSpots(qtvr: QTVRInstance; hotSpots: Handle): UInt32; external name '_QTVRGetVisibleHotSpots';

{
 *  QTVRGetHotSpotRegion()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRGetHotSpotRegion(qtvr: QTVRInstance; hotSpotID: UInt32; hotSpotRegion: RgnHandle): OSErr; external name '_QTVRGetHotSpotRegion';

{
  =================================================================================================
    Event & Cursor Handling Calls 
  -------------------------------------------------------------------------------------------------
}

{
 *  QTVRSetMouseOverTracking()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRSetMouseOverTracking(qtvr: QTVRInstance; enable: boolean): OSErr; external name '_QTVRSetMouseOverTracking';

{
 *  QTVRGetMouseOverTracking()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRGetMouseOverTracking(qtvr: QTVRInstance): boolean; external name '_QTVRGetMouseOverTracking';

{
 *  QTVRSetMouseDownTracking()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRSetMouseDownTracking(qtvr: QTVRInstance; enable: boolean): OSErr; external name '_QTVRSetMouseDownTracking';

{
 *  QTVRGetMouseDownTracking()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRGetMouseDownTracking(qtvr: QTVRInstance): boolean; external name '_QTVRGetMouseDownTracking';

{
 *  QTVRMouseEnter()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRMouseEnter(qtvr: QTVRInstance; pt: Point; var hotSpotID: UInt32; w: WindowRef): OSErr; external name '_QTVRMouseEnter';

{
 *  QTVRMouseWithin()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRMouseWithin(qtvr: QTVRInstance; pt: Point; var hotSpotID: UInt32; w: WindowRef): OSErr; external name '_QTVRMouseWithin';

{
 *  QTVRMouseLeave()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRMouseLeave(qtvr: QTVRInstance; pt: Point; w: WindowRef): OSErr; external name '_QTVRMouseLeave';

{
 *  QTVRMouseDown()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRMouseDown(qtvr: QTVRInstance; pt: Point; when: UInt32; modifiers: UInt16; var hotSpotID: UInt32; w: WindowRef): OSErr; external name '_QTVRMouseDown';

{
 *  QTVRMouseStillDown()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRMouseStillDown(qtvr: QTVRInstance; pt: Point; var hotSpotID: UInt32; w: WindowRef): OSErr; external name '_QTVRMouseStillDown';

{
 *  QTVRMouseUp()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRMouseUp(qtvr: QTVRInstance; pt: Point; var hotSpotID: UInt32; w: WindowRef): OSErr; external name '_QTVRMouseUp';

{  These require QTVR 2.01 (kQTVRAPIMajorVersion02 + kQTVRAPIMinorVersion01) }
{
 *  QTVRMouseStillDownExtended()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0.1 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRMouseStillDownExtended(qtvr: QTVRInstance; pt: Point; var hotSpotID: UInt32; w: WindowRef; when: UInt32; modifiers: UInt16): OSErr; external name '_QTVRMouseStillDownExtended';

{
 *  QTVRMouseUpExtended()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0.1 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRMouseUpExtended(qtvr: QTVRInstance; pt: Point; var hotSpotID: UInt32; w: WindowRef; when: UInt32; modifiers: UInt16): OSErr; external name '_QTVRMouseUpExtended';

{
  =================================================================================================
    Intercept Routines 
  -------------------------------------------------------------------------------------------------
}

{
 *  QTVRInstallInterceptProc()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRInstallInterceptProc(qtvr: QTVRInstance; selector: QTVRProcSelector; interceptProc: QTVRInterceptUPP; refCon: SInt32; flags: UInt32): OSErr; external name '_QTVRInstallInterceptProc';

{
 *  QTVRCallInterceptedProc()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRCallInterceptedProc(qtvr: QTVRInstance; var qtvrMsg: QTVRInterceptRecord): OSErr; external name '_QTVRCallInterceptedProc';

{
  =================================================================================================
    Object Movie Specific Calls 
  -------------------------------------------------------------------------------------------------
   QTVRGetCurrentMouseMode requires QTRVR 2.1 (kQTVRAPIMajorVersion02 + kQTVRAPIMinorVersion10)
}
{
 *  QTVRGetCurrentMouseMode()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.1 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRGetCurrentMouseMode(qtvr: QTVRInstance): UInt32; external name '_QTVRGetCurrentMouseMode';

{
 *  QTVRSetFrameRate()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRSetFrameRate(qtvr: QTVRInstance; rate: Single): OSErr; external name '_QTVRSetFrameRate';

{
 *  QTVRGetFrameRate()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRGetFrameRate(qtvr: QTVRInstance): Single; external name '_QTVRGetFrameRate';

{
 *  QTVRSetViewRate()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRSetViewRate(qtvr: QTVRInstance; rate: Single): OSErr; external name '_QTVRSetViewRate';

{
 *  QTVRGetViewRate()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRGetViewRate(qtvr: QTVRInstance): Single; external name '_QTVRGetViewRate';

{
 *  QTVRSetViewCurrentTime()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRSetViewCurrentTime(qtvr: QTVRInstance; time: TimeValue): OSErr; external name '_QTVRSetViewCurrentTime';

{
 *  QTVRGetViewCurrentTime()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRGetViewCurrentTime(qtvr: QTVRInstance): TimeValue; external name '_QTVRGetViewCurrentTime';

{
 *  QTVRGetCurrentViewDuration()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRGetCurrentViewDuration(qtvr: QTVRInstance): TimeValue; external name '_QTVRGetCurrentViewDuration';

{
  =================================================================================================
   View State Calls - QTVR Object Only
  -------------------------------------------------------------------------------------------------
}

{
 *  QTVRSetViewState()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRSetViewState(qtvr: QTVRInstance; viewStateType: QTVRViewStateType; state: UInt16): OSErr; external name '_QTVRSetViewState';

{
 *  QTVRGetViewState()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRGetViewState(qtvr: QTVRInstance; viewStateType: QTVRViewStateType; var state: UInt16): OSErr; external name '_QTVRGetViewState';

{
 *  QTVRGetViewStateCount()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRGetViewStateCount(qtvr: QTVRInstance): UInt16; external name '_QTVRGetViewStateCount';

{
 *  QTVRSetAnimationSetting()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRSetAnimationSetting(qtvr: QTVRInstance; setting: QTVRObjectAnimationSetting; enable: boolean): OSErr; external name '_QTVRSetAnimationSetting';

{
 *  QTVRGetAnimationSetting()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRGetAnimationSetting(qtvr: QTVRInstance; setting: QTVRObjectAnimationSetting; var enable: boolean): OSErr; external name '_QTVRGetAnimationSetting';

{
 *  QTVRSetControlSetting()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRSetControlSetting(qtvr: QTVRInstance; setting: QTVRControlSetting; enable: boolean): OSErr; external name '_QTVRSetControlSetting';

{
 *  QTVRGetControlSetting()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRGetControlSetting(qtvr: QTVRInstance; setting: QTVRControlSetting; var enable: boolean): OSErr; external name '_QTVRGetControlSetting';

{
 *  QTVREnableFrameAnimation()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVREnableFrameAnimation(qtvr: QTVRInstance; enable: boolean): OSErr; external name '_QTVREnableFrameAnimation';

{
 *  QTVRGetFrameAnimation()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRGetFrameAnimation(qtvr: QTVRInstance): boolean; external name '_QTVRGetFrameAnimation';

{
 *  QTVREnableViewAnimation()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVREnableViewAnimation(qtvr: QTVRInstance; enable: boolean): OSErr; external name '_QTVREnableViewAnimation';

{
 *  QTVRGetViewAnimation()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRGetViewAnimation(qtvr: QTVRInstance): boolean; external name '_QTVRGetViewAnimation';


{
  =================================================================================================
    Imaging Characteristics 
  -------------------------------------------------------------------------------------------------
}

{
 *  QTVRSetVisible()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRSetVisible(qtvr: QTVRInstance; visible: boolean): OSErr; external name '_QTVRSetVisible';

{
 *  QTVRGetVisible()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRGetVisible(qtvr: QTVRInstance): boolean; external name '_QTVRGetVisible';

{
 *  QTVRSetImagingProperty()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRSetImagingProperty(qtvr: QTVRInstance; imagingMode: QTVRImagingMode; imagingProperty: UInt32; propertyValue: SInt32): OSErr; external name '_QTVRSetImagingProperty';

{
 *  QTVRGetImagingProperty()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRGetImagingProperty(qtvr: QTVRInstance; imagingMode: QTVRImagingMode; imagingProperty: UInt32; var propertyValue: SInt32): OSErr; external name '_QTVRGetImagingProperty';

{
 *  QTVRUpdate()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRUpdate(qtvr: QTVRInstance; imagingMode: QTVRImagingMode): OSErr; external name '_QTVRUpdate';

{
 *  QTVRBeginUpdateStream()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRBeginUpdateStream(qtvr: QTVRInstance; imagingMode: QTVRImagingMode): OSErr; external name '_QTVRBeginUpdateStream';

{
 *  QTVREndUpdateStream()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVREndUpdateStream(qtvr: QTVRInstance): OSErr; external name '_QTVREndUpdateStream';

{
 *  QTVRSetTransitionProperty()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRSetTransitionProperty(qtvr: QTVRInstance; transitionType: UInt32; transitionProperty: UInt32; transitionValue: SInt32): OSErr; external name '_QTVRSetTransitionProperty';

{
 *  QTVREnableTransition()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVREnableTransition(qtvr: QTVRInstance; transitionType: UInt32; enable: boolean): OSErr; external name '_QTVREnableTransition';

{
  =================================================================================================
    Basic Conversion and Math Routines 
  -------------------------------------------------------------------------------------------------
}

{
 *  QTVRSetAngularUnits()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRSetAngularUnits(qtvr: QTVRInstance; units: QTVRAngularUnits): OSErr; external name '_QTVRSetAngularUnits';

{
 *  QTVRGetAngularUnits()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRGetAngularUnits(qtvr: QTVRInstance): QTVRAngularUnits; external name '_QTVRGetAngularUnits';

{  Pano specific routines }
{
 *  QTVRPtToAngles()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRPtToAngles(qtvr: QTVRInstance; pt: Point; var panAngle: Single; var tiltAngle: Single): OSErr; external name '_QTVRPtToAngles';

{
 *  QTVRCoordToAngles()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRCoordToAngles(qtvr: QTVRInstance; var coord: QTVRFloatPoint; var panAngle: Single; var tiltAngle: Single): OSErr; external name '_QTVRCoordToAngles';

{
 *  QTVRAnglesToCoord()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRAnglesToCoord(qtvr: QTVRInstance; panAngle: Single; tiltAngle: Single; var coord: QTVRFloatPoint): OSErr; external name '_QTVRAnglesToCoord';

{  Object specific routines }
{
 *  QTVRPanToColumn()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRPanToColumn(qtvr: QTVRInstance; panAngle: Single): SInt16; external name '_QTVRPanToColumn';

{  zero based    }
{
 *  QTVRColumnToPan()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRColumnToPan(qtvr: QTVRInstance; column: SInt16): Single; external name '_QTVRColumnToPan';

{  zero based    }
{
 *  QTVRTiltToRow()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRTiltToRow(qtvr: QTVRInstance; tiltAngle: Single): SInt16; external name '_QTVRTiltToRow';

{  zero based    }
{
 *  QTVRRowToTilt()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRRowToTilt(qtvr: QTVRInstance; row: SInt16): Single; external name '_QTVRRowToTilt';

{  zero based                }
{
 *  QTVRWrapAndConstrain()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRWrapAndConstrain(qtvr: QTVRInstance; kind: SInt16; value: Single; var result: Single): OSErr; external name '_QTVRWrapAndConstrain';


{
  =================================================================================================
    Interaction Routines 
  -------------------------------------------------------------------------------------------------
}

{
 *  QTVRSetEnteringNodeProc()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRSetEnteringNodeProc(qtvr: QTVRInstance; enteringNodeProc: QTVREnteringNodeUPP; refCon: SInt32; flags: UInt32): OSErr; external name '_QTVRSetEnteringNodeProc';

{
 *  QTVRSetLeavingNodeProc()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRSetLeavingNodeProc(qtvr: QTVRInstance; leavingNodeProc: QTVRLeavingNodeUPP; refCon: SInt32; flags: UInt32): OSErr; external name '_QTVRSetLeavingNodeProc';

{
 *  QTVRSetInteractionProperty()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRSetInteractionProperty(qtvr: QTVRInstance; proprty: UInt32; value: UnivPtr): OSErr; external name '_QTVRSetInteractionProperty';

{
 *  QTVRGetInteractionProperty()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRGetInteractionProperty(qtvr: QTVRInstance; proprty: UInt32; value: UnivPtr): OSErr; external name '_QTVRGetInteractionProperty';

{
 *  QTVRReplaceCursor()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRReplaceCursor(qtvr: QTVRInstance; var cursRecord: QTVRCursorRecord): OSErr; external name '_QTVRReplaceCursor';

{
  =================================================================================================
    Viewing Limits and Constraints 
  -------------------------------------------------------------------------------------------------
}

{
 *  QTVRGetViewingLimits()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRGetViewingLimits(qtvr: QTVRInstance; kind: UInt16; var minValue: Single; var maxValue: Single): OSErr; external name '_QTVRGetViewingLimits';

{
 *  QTVRGetConstraintStatus()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRGetConstraintStatus(qtvr: QTVRInstance): UInt32; external name '_QTVRGetConstraintStatus';

{
 *  QTVRGetConstraints()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRGetConstraints(qtvr: QTVRInstance; kind: UInt16; var minValue: Single; var maxValue: Single): OSErr; external name '_QTVRGetConstraints';

{
 *  QTVRSetConstraints()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRSetConstraints(qtvr: QTVRInstance; kind: UInt16; minValue: Single; maxValue: Single): OSErr; external name '_QTVRSetConstraints';


{
  =================================================================================================
    Back Buffer Memory Management 
  -------------------------------------------------------------------------------------------------
}

{
 *  QTVRGetAvailableResolutions()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRGetAvailableResolutions(qtvr: QTVRInstance; var resolutionsMask: UInt16): OSErr; external name '_QTVRGetAvailableResolutions';

{  These require QTVR 2.1 (kQTVRAPIMajorVersion02 + kQTVRAPIMinorVersion10) }
{
 *  QTVRGetBackBufferMemInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.1 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRGetBackBufferMemInfo(qtvr: QTVRInstance; geometry: UInt32; resolution: UInt16; cachePixelFormat: UInt32; var minCacheBytes: SInt32; var suggestedCacheBytes: SInt32; var fullCacheBytes: SInt32): OSErr; external name '_QTVRGetBackBufferMemInfo';

{
 *  QTVRGetBackBufferSettings()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.1 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRGetBackBufferSettings(qtvr: QTVRInstance; var geometry: UInt32; var resolution: UInt16; var cachePixelFormat: UInt32; var cacheSize: SInt16): OSErr; external name '_QTVRGetBackBufferSettings';

{
 *  QTVRSetBackBufferPrefs()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.1 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRSetBackBufferPrefs(qtvr: QTVRInstance; geometry: UInt32; resolution: UInt16; cachePixelFormat: UInt32; cacheSize: SInt16): OSErr; external name '_QTVRSetBackBufferPrefs';

{
  =================================================================================================
    Buffer Access 
  -------------------------------------------------------------------------------------------------
}

{
 *  QTVRSetPrescreenImagingCompleteProc()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRSetPrescreenImagingCompleteProc(qtvr: QTVRInstance; imagingCompleteProc: QTVRImagingCompleteUPP; refCon: SInt32; flags: UInt32): OSErr; external name '_QTVRSetPrescreenImagingCompleteProc';

{
 *  QTVRSetBackBufferImagingProc()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRSetBackBufferImagingProc(qtvr: QTVRInstance; backBufferImagingProc: QTVRBackBufferImagingUPP; numAreas: UInt16; var areasOfInterest: QTVRAreaOfInterest; refCon: SInt32): OSErr; external name '_QTVRSetBackBufferImagingProc';

{
 *  QTVRRefreshBackBuffer()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRRefreshBackBuffer(qtvr: QTVRInstance; flags: UInt32): OSErr; external name '_QTVRRefreshBackBuffer';


{
  =================================================================================================
    Old Names
  -------------------------------------------------------------------------------------------------
}
{$ifc OLDROUTINENAMES}

type
	CursorRecord						= QTVRCursorRecord;
	CursorRecordPtr 					= ^CursorRecord;
	AreaOfInterest						= QTVRAreaOfInterest;
	AreaOfInterestPtr 					= ^AreaOfInterest;
	FloatPoint							= QTVRFloatPoint;
	FloatPointPtr 						= ^FloatPoint;
	LeavingNodeProcPtr					= QTVRLeavingNodeProcPtr;
	LeavingNodeUPP						= QTVRLeavingNodeUPP;
	EnteringNodeProcPtr					= QTVREnteringNodeProcPtr;
	EnteringNodeUPP						= QTVREnteringNodeUPP;
	MouseOverHotSpotProcPtr				= QTVRMouseOverHotSpotProcPtr;
	MouseOverHotSpotUPP					= QTVRMouseOverHotSpotUPP;
	ImagingCompleteProcPtr				= QTVRImagingCompleteProcPtr;
	ImagingCompleteUPP					= QTVRImagingCompleteUPP;
	BackBufferImagingProcPtr			= QTVRBackBufferImagingProcPtr;
	BackBufferImagingUPP				= QTVRBackBufferImagingUPP;
{$endc}  {OLDROUTINENAMES}


{$ALIGN MAC68K}


end.
