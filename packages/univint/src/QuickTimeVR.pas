{
     File:       QuickTime/QuickTimeVR.h
 
     Contains:   QuickTime VR interfaces
 
     Version:    QuickTime 7.7.1
 
     Copyright:  © 1997-2012 by Apple Inc., all rights reserved.
 
     Bugs?:      For bug reports, consult the following page on
                 the World Wide Web:
 
                     http://bugs.freepascal.org
 
}
{  Pascal Translation Updated:  Jonas Maebe, <jonas@freepascal.org>, October 2009 }
{  Pascal Translation Updated:  Jonas Maebe, <jonas@freepascal.org>, October 2012 }
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

unit QuickTimeVR;
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
uses MacTypes,MacWindows,QuickdrawTypes,Movies;
{$endc} {not MACOSALLINCLUDE}


{$ifc TARGET_OS_MAC}

{$ALIGN MAC68K}

{ QuickTime is not available to 64-bit clients }

{$ifc not TARGET_CPU_64}

type
	QTVRInstance = ^OpaqueQTVRInstance; { an opaque type }
	OpaqueQTVRInstance = record end;
	QTVRInstancePtr = ^QTVRInstance;  { when a var xx:QTVRInstance parameter can be nil, it is changed to xx: QTVRInstancePtr }

{ Released API Version numbers }
const
  kQTVRAPIMajorVersion05 = $05;
  kQTVRAPIMajorVersion02 = $02;
  kQTVRAPIMinorVersion00 = $00;
  kQTVRAPIMinorVersion01 = $01;
  kQTVRAPIMinorVersion10 = $10;
  kQTVRAPIMinorVersion20 = $20;

{ Version numbers for the API described in this header }
const
  kQTVRAPIMajorVersion = kQTVRAPIMajorVersion05;
  kQTVRAPIMinorVersion = kQTVRAPIMinorVersion00;


const
	kQTVRControllerSubType = FourCharCode('ctyp');
	kQTVRQTVRType = FourCharCode('qtvr');
	kQTVRPanoramaType = FourCharCode('pano');
	kQTVRObjectType = FourCharCode('obje');
	kQTVROldPanoType = FourCharCode('STpn'); { Used in QTVR 1.0 release}
	kQTVROldObjectType = FourCharCode('stna'); { Used in QTVR 1.0 release}

  kQTVRUnknownType = FourCharCode('????'); { Unknown node type }

{ QTVR hot spot types}
const
	kQTVRHotSpotLinkType = FourCharCode('link');
	kQTVRHotSpotURLType = FourCharCode('url ');
	kQTVRHotSpotUndefinedType = FourCharCode('undf');

{ Special Values for nodeID in QTVRGoToNodeID}
const
	kQTVRCurrentNode = 0;
	kQTVRPreviousNode = $80000000;
	kQTVRDefaultNode = $80000001;

{ Panorama correction modes used for the kQTVRImagingCorrection imaging property}
const
	kQTVRNoCorrection = 0;
	kQTVRPartialCorrection = 1;
	kQTVRFullCorrection = 2;

{ Imaging Modes used by QTVRSetImagingProperty, QTVRGetImagingProperty, QTVRUpdate, QTVRBeginUpdate}
type
	QTVRImagingMode = UInt32;
const
	kQTVRStatic = 1;
	kQTVRMotion = 2;
	kQTVRCurrentMode = 0;    { Special Value for QTVRUpdate}
	kQTVRAllModes = 100;   { Special value for QTVRSetProperty}

{ Imaging Properties used by QTVRSetImagingProperty, QTVRGetImagingProperty}
const
	kQTVRImagingCorrection = 1;
	kQTVRImagingQuality = 2;
	kQTVRImagingDirectDraw = 3;
	kQTVRImagingCurrentMode = 100;   { Get Only}

{ OR the above with kImagingDefaultValue to get/set the default value}
const
	kImagingDefaultValue = $80000000;

{ Transition Types used by QTVRSetTransitionProperty, QTVREnableTransition}
const
	kQTVRTransitionSwing = 1;

{ Transition Properties QTVRSetTransitionProperty}
const
	kQTVRTransitionSpeed = 1;
	kQTVRTransitionDirection = 2;

{ Constraint values used to construct value returned by GetConstraintStatus}
const
	kQTVRUnconstrained = 0;
	kQTVRCantPanLeft = 1 shl 0;
	kQTVRCantPanRight = 1 shl 1;
	kQTVRCantPanUp = 1 shl 2;
	kQTVRCantPanDown = 1 shl 3;
	kQTVRCantZoomIn = 1 shl 4;
	kQTVRCantZoomOut = 1 shl 5;
	kQTVRCantTranslateLeft = 1 shl 6;
	kQTVRCantTranslateRight = 1 shl 7;
	kQTVRCantTranslateUp = 1 shl 8;
	kQTVRCantTranslateDown = 1 shl 9;

{ Object-only mouse mode values used to construct value returned by QTVRGetCurrentMouseMode}
const
	kQTVRPanning = 1 shl 0; { standard objects, "object only" controllers}
	kQTVRTranslating = 1 shl 1; { all objects}
	kQTVRZooming = 1 shl 2; { all objects}
	kQTVRScrolling = 1 shl 3; { standard object arrow scrollers and joystick object}
	kQTVRSelecting = 1 shl 4; { object absolute controller}

{ Properties for use with QTVRSetInteractionProperty/GetInteractionProperty}
const
	kQTVRInteractionMouseClickHysteresis = 1; { pixels within which the mouse is considered not to have moved (UInt16)}
	kQTVRInteractionMouseClickTimeout = 2; { ticks after which a mouse click times out and turns into panning (UInt32)}
	kQTVRInteractionPanTiltSpeed = 3;    { control the relative pan/tilt speed from 1 (slowest) to 10 (fastest). (UInt32) Default is 5;}
	kQTVRInteractionZoomSpeed = 4;    { control the relative zooming speed from 1 (slowest) to 10 (fastest). (UInt32) Default is 5;}
	kQTVRInteractionTranslateOnMouseDown = 101; { Holding MouseDown with this setting translates zoomed object movies (Boolean)}
	kQTVRInteractionMouseMotionScale = 102; { The maximum angle of rotation caused by dragging across the display window. (* float)}
	kQTVRInteractionNudgeMode = 103;   { A QTVRNudgeMode: rotate, translate, or the same as the current mouse mode. Requires QTVR 2.1}

{ OR the above with kQTVRInteractionDefaultValue to get/set the default value}
const
	kQTVRInteractionDefaultValue = $80000000;


{ Geometry constants used in QTVRSetBackBufferPrefs, QTVRGetBackBufferSettings, QTVRGetBackBufferMemInfo}
const
	kQTVRUseMovieGeometry = 0;
	kQTVRVerticalCylinder = FourCharCode('vcyl');
	kQTVRHorizontalCylinder = FourCharCode('hcyl');
	kQTVRCube = FourCharCode('cube');

{ Resolution constants used in QTVRSetBackBufferPrefs, QTVRGetBackBufferSettings, QTVRGetBackBufferMemInfo}
const
	kQTVRDefaultRes = 0;
	kQTVRFullRes = 1 shl 0;
	kQTVRHalfRes = 1 shl 1;
	kQTVRQuarterRes = 1 shl 2;

{ QTVR-specific pixelFormat constants used in QTVRSetBackBufferPrefs, QTVRGetBackBufferSettings, QTVRGetBackBufferMemInfo}
const
	kQTVRUseMovieDepth = 0;

{ Cache Size Pref constants used in QTVRSetBackBufferPrefs, QTVRGetBackBufferSettings}
const
	kQTVRMinimumCache = -1;
	kQTVRSuggestedCache = 0;
	kQTVRFullCache = 1;

{ Angular units used by QTVRSetAngularUnits}
type
	QTVRAngularUnits = UInt32;
const
	kQTVRDegrees = 0;
	kQTVRRadians = 1;

{ Values for enableFlag parameter in QTVREnableHotSpot}
const
	kQTVRHotSpotID = 0;
	kQTVRHotSpotType = 1;
	kQTVRAllHotSpots = 2;

{ Values for viewParameter for QTVRSet/GetViewParameter}
const
	kQTVRPanAngle = $0100; { default units; &float, &float}
	kQTVRTiltAngle = $0101; { default units; &float, &float}
	kQTVRFieldOfViewAngle = $0103; { default units; &float, &float}
	kQTVRViewCenter = $0104; { pixels (per object movies); &QTVRFloatPoint, &QTVRFloatPoint}
	kQTVRHotSpotsVisible = $0200; { Boolean, &Boolean}

{ Values for flagsIn for QTVRSet/GetViewParameter}
const
	kQTVRValueIsRelative = 1 shl 0; { Is the value absolute or relative to the current value?}
	kQTVRValueIsRate = 1 shl 1; { Is the value absolute or a rate of change to be applied?}
	kQTVRValueIsUserPrefRelative = 1 shl 2; { Is the value a percentage of the user rate pref?}

{ Values for kind parameter in QTVRGet/SetConstraints, QTVRGetViewingLimits}
const
	kQTVRPan = 0;
	kQTVRTilt = 1;
	kQTVRFieldOfView = 2;
	kQTVRViewCenterH = 4;    { WrapAndConstrain only}
	kQTVRViewCenterV = 5;     { WrapAndConstrain only}

{ Values for setting parameter in QTVRSetAnimationSetting, QTVRGetAnimationSetting}
type
	QTVRObjectAnimationSetting = UInt32;
const
{ View Frame Animation Settings}
	kQTVRPalindromeViewFrames = 1;
	kQTVRStartFirstViewFrame = 2;
	kQTVRDontLoopViewFrames = 3;
	kQTVRPlayEveryViewFrame = 4;    { Requires QTVR 2.1 (kQTVRAPIMajorVersion02 + kQTVRAPIMinorVersion10)}
                                        { View Animation Settings}
	kQTVRSyncViewToFrameRate = 16;
	kQTVRPalindromeViews = 17;
	kQTVRPlayStreamingViews = 18;    { Requires QTVR 2.1 (kQTVRAPIMajorVersion02 + kQTVRAPIMinorVersion10)}

type
	QTVRControlSetting = UInt32;
const
	kQTVRWrapPan = 1;
	kQTVRWrapTilt = 2;
	kQTVRCanZoom = 3;
	kQTVRReverseHControl = 4;
	kQTVRReverseVControl = 5;
	kQTVRSwapHVControl = 6;
	kQTVRTranslation = 7;

type
	QTVRViewStateType = UInt32;
const
	kQTVRDefault = 0;
	kQTVRCurrent = 2;
	kQTVRMouseDown = 3;

type
	QTVRNudgeControl = UInt32;
const
	kQTVRRight = 0;
	kQTVRUpRight = 45;
	kQTVRUp = 90;
	kQTVRUpLeft = 135;
	kQTVRLeft = 180;
	kQTVRDownLeft = 225;
	kQTVRDown = 270;
	kQTVRDownRight = 315;

type
	QTVRNudgeMode = UInt32;
const
	kQTVRNudgeRotate = 0;
	kQTVRNudgeTranslate = 1;
	kQTVRNudgeSameAsMouse = 2;


{ Flags to control elements of the QTVR control bar (set via mcActionSetFlags) }
const
	mcFlagQTVRSuppressBackBtn = 1 shl 16;
	mcFlagQTVRSuppressZoomBtns = 1 shl 17;
	mcFlagQTVRSuppressHotSpotBtn = 1 shl 18;
	mcFlagQTVRSuppressTranslateBtn = 1 shl 19;
	mcFlagQTVRSuppressHelpText = 1 shl 20;
	mcFlagQTVRSuppressHotSpotNames = 1 shl 21;
	mcFlagQTVRExplicitFlagSet = 1 shl 31; { bits 0->30 should be interpreted as "explicit on" for the corresponding suppression bits}

{ Cursor types used in type field of QTVRCursorRecord}
const
	kQTVRUseDefaultCursor = 0;
	kQTVRStdCursorType = 1;
	kQTVRColorCursorType = 2;

{ Values for flags parameter in QTVRMouseOverHotSpot callback}
const
	kQTVRHotSpotEnter = 0;
	kQTVRHotSpotWithin = 1;
	kQTVRHotSpotLeave = 2;

{ Values for flags parameter in QTVRSetPrescreenImagingCompleteProc}
const
	kQTVRPreScreenEveryIdle = 1 shl 0; { Requires QTVR 2.1 (kQTVRAPIMajorVersion02 + kQTVRAPIMinorVersion10)}

{ Values for flags field of areasOfInterest in QTVRSetBackBufferImagingProc}
const
	kQTVRBackBufferEveryUpdate = 1 shl 0;
	kQTVRBackBufferEveryIdle = 1 shl 1;
	kQTVRBackBufferAlwaysRefresh = 1 shl 2;
	kQTVRBackBufferHorizontal = 1 shl 3; { Requires that backbuffer proc be long-rowBytes aware (gestaltQDHasLongRowBytes)}

{ Values for flagsIn parameter in QTVRBackBufferImaging callback}
const
	kQTVRBackBufferRectVisible = 1 shl 0;
	kQTVRBackBufferWasRefreshed = 1 shl 1;

{ Values for flagsOut parameter in QTVRBackBufferImaging callback}
const
	kQTVRBackBufferFlagDidDraw = 1 shl 0;
	kQTVRBackBufferFlagLastFlag = 1 shl 31;

{ QTVRCursorRecord used in QTVRReplaceCursor}
type
	QTVRCursorRecordPtr = ^QTVRCursorRecord;
	QTVRCursorRecord = record
		theType: UInt16;                { field was previously named "type"}
		rsrcID: SInt16;
		handle: Handle_fix;
	end;
type
	QTVRFloatPointPtr = ^QTVRFloatPoint;
	QTVRFloatPoint = record
		x: Float32;
		y: Float32;
	end;
{ Struct used for areasOfInterest parameter in QTVRSetBackBufferImagingProc}
type
	QTVRAreaOfInterestPtr = ^QTVRAreaOfInterest;
	QTVRAreaOfInterest = record
		panAngle: Float32;
		tiltAngle: Float32;
		width: Float32;
		height: Float32;
		flags: UInt32;
	end;
{
  =================================================================================================
   Callback routines 
  -------------------------------------------------------------------------------------------------
}

type
	QTVRLeavingNodeProcPtr = function( qtvr: QTVRInstance; fromNodeID: UInt32; toNodeID: UInt32; var cancel: Boolean; refCon: SInt32 ): OSErr;
	QTVREnteringNodeProcPtr = function( qtvr: QTVRInstance; nodeID: UInt32; refCon: SInt32 ): OSErr;
	QTVRMouseOverHotSpotProcPtr = function( qtvr: QTVRInstance; hotSpotID: UInt32; flags: UInt32; refCon: SInt32 ): OSErr;
	QTVRImagingCompleteProcPtr = function( qtvr: QTVRInstance; refCon: SInt32 ): OSErr;
	QTVRBackBufferImagingProcPtr = function( qtvr: QTVRInstance; var drawRect: Rect; areaIndex: UInt16; flagsIn: UInt32; var flagsOut: UInt32; refCon: SInt32 ): OSErr;
	QTVRLeavingNodeUPP = QTVRLeavingNodeProcPtr;
	QTVREnteringNodeUPP = QTVREnteringNodeProcPtr;
	QTVRMouseOverHotSpotUPP = QTVRMouseOverHotSpotProcPtr;
	QTVRImagingCompleteUPP = QTVRImagingCompleteProcPtr;
	QTVRBackBufferImagingUPP = QTVRBackBufferImagingProcPtr;
{
 *  NewQTVRLeavingNodeUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function NewQTVRLeavingNodeUPP( userRoutine: QTVRLeavingNodeProcPtr ): QTVRLeavingNodeUPP; external name '_NewQTVRLeavingNodeUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  NewQTVREnteringNodeUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function NewQTVREnteringNodeUPP( userRoutine: QTVREnteringNodeProcPtr ): QTVREnteringNodeUPP; external name '_NewQTVREnteringNodeUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  NewQTVRMouseOverHotSpotUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function NewQTVRMouseOverHotSpotUPP( userRoutine: QTVRMouseOverHotSpotProcPtr ): QTVRMouseOverHotSpotUPP; external name '_NewQTVRMouseOverHotSpotUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  NewQTVRImagingCompleteUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function NewQTVRImagingCompleteUPP( userRoutine: QTVRImagingCompleteProcPtr ): QTVRImagingCompleteUPP; external name '_NewQTVRImagingCompleteUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  NewQTVRBackBufferImagingUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function NewQTVRBackBufferImagingUPP( userRoutine: QTVRBackBufferImagingProcPtr ): QTVRBackBufferImagingUPP; external name '_NewQTVRBackBufferImagingUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  DisposeQTVRLeavingNodeUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure DisposeQTVRLeavingNodeUPP( userUPP: QTVRLeavingNodeUPP ); external name '_DisposeQTVRLeavingNodeUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  DisposeQTVREnteringNodeUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure DisposeQTVREnteringNodeUPP( userUPP: QTVREnteringNodeUPP ); external name '_DisposeQTVREnteringNodeUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  DisposeQTVRMouseOverHotSpotUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure DisposeQTVRMouseOverHotSpotUPP( userUPP: QTVRMouseOverHotSpotUPP ); external name '_DisposeQTVRMouseOverHotSpotUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  DisposeQTVRImagingCompleteUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure DisposeQTVRImagingCompleteUPP( userUPP: QTVRImagingCompleteUPP ); external name '_DisposeQTVRImagingCompleteUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  DisposeQTVRBackBufferImagingUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure DisposeQTVRBackBufferImagingUPP( userUPP: QTVRBackBufferImagingUPP ); external name '_DisposeQTVRBackBufferImagingUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  InvokeQTVRLeavingNodeUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function InvokeQTVRLeavingNodeUPP( qtvr: QTVRInstance; fromNodeID: UInt32; toNodeID: UInt32; var cancel: Boolean; refCon: SInt32; userUPP: QTVRLeavingNodeUPP ): OSErr; external name '_InvokeQTVRLeavingNodeUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  InvokeQTVREnteringNodeUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function InvokeQTVREnteringNodeUPP( qtvr: QTVRInstance; nodeID: UInt32; refCon: SInt32; userUPP: QTVREnteringNodeUPP ): OSErr; external name '_InvokeQTVREnteringNodeUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  InvokeQTVRMouseOverHotSpotUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function InvokeQTVRMouseOverHotSpotUPP( qtvr: QTVRInstance; hotSpotID: UInt32; flags: UInt32; refCon: SInt32; userUPP: QTVRMouseOverHotSpotUPP ): OSErr; external name '_InvokeQTVRMouseOverHotSpotUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  InvokeQTVRImagingCompleteUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function InvokeQTVRImagingCompleteUPP( qtvr: QTVRInstance; refCon: SInt32; userUPP: QTVRImagingCompleteUPP ): OSErr; external name '_InvokeQTVRImagingCompleteUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  InvokeQTVRBackBufferImagingUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function InvokeQTVRBackBufferImagingUPP( qtvr: QTVRInstance; var drawRect: Rect; areaIndex: UInt16; flagsIn: UInt32; var flagsOut: UInt32; refCon: SInt32; userUPP: QTVRBackBufferImagingUPP ): OSErr; external name '_InvokeQTVRBackBufferImagingUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
  =================================================================================================
    QTVR Intercept Struct, Callback, Routine Descriptors 
  -------------------------------------------------------------------------------------------------
}

type
	QTVRProcSelector = UInt32;
const
	kQTVRSetPanAngleSelector = $2000;
	kQTVRSetTiltAngleSelector = $2001;
	kQTVRSetFieldOfViewSelector = $2002;
	kQTVRSetViewCenterSelector = $2003;
	kQTVRMouseEnterSelector = $2004;
	kQTVRMouseWithinSelector = $2005;
	kQTVRMouseLeaveSelector = $2006;
	kQTVRMouseDownSelector = $2007;
	kQTVRMouseStillDownSelector = $2008;
	kQTVRMouseUpSelector = $2009;
	kQTVRTriggerHotSpotSelector = $200A;
	kQTVRGetHotSpotTypeSelector = $200B; { Requires QTVR 2.1 (kQTVRAPIMajorVersion02 + kQTVRAPIMinorVersion10)}
	kQTVRSetViewParameterSelector = $200C; { Requires QTVR 5.0 (kQTVRAPIMajorVersion05 + kQTVRAPIMinorVersion00)}
	kQTVRGetViewParameterSelector = $200D; { Requires QTVR 5.0 (kQTVRAPIMajorVersion05 + kQTVRAPIMinorVersion00)}

type
	QTVRInterceptRecordPtr = ^QTVRInterceptRecord;
	QTVRInterceptRecord = record
		reserved1: SInt32;
		selector: SInt32;

		reserved2: SInt32;
		reserved3: SInt32;

		paramCount: SInt32;
		parameter: array [0..5] of UnivPtr;
	end;
type
	QTVRInterceptPtr = QTVRInterceptRecordPtr;
{ Prototype for Intercept Proc callback}
type
	QTVRInterceptProcPtr = procedure( qtvr: QTVRInstance; qtvrMsg: QTVRInterceptPtr; refCon: SInt32; var cancel: Boolean );
	QTVRInterceptUPP = QTVRInterceptProcPtr;
{
 *  NewQTVRInterceptUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function NewQTVRInterceptUPP( userRoutine: QTVRInterceptProcPtr ): QTVRInterceptUPP; external name '_NewQTVRInterceptUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  DisposeQTVRInterceptUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure DisposeQTVRInterceptUPP( userUPP: QTVRInterceptUPP ); external name '_DisposeQTVRInterceptUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  InvokeQTVRInterceptUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure InvokeQTVRInterceptUPP( qtvr: QTVRInstance; qtvrMsg: QTVRInterceptPtr; refCon: SInt32; var cancel: Boolean; userUPP: QTVRInterceptUPP ); external name '_InvokeQTVRInterceptUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
  =================================================================================================
    Initialization QTVR calls 
  -------------------------------------------------------------------------------------------------
   Requires QTVR 2.1 (kQTVRAPIMajorVersion02 + kQTVRAPIMinorVersion10) and only work on Non-Macintosh platforms
}
{
  =================================================================================================
    General QTVR calls 
  -------------------------------------------------------------------------------------------------
}
{
 *  QTVRGetQTVRTrack()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRGetQTVRTrack( theMovie: Movie; index: SInt32 ): Track; external name '_QTVRGetQTVRTrack';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  QTVRGetQTVRInstance()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRGetQTVRInstance( var qtvr: QTVRInstance; qtvrTrack: Track; mc: MovieController ): OSErr; external name '_QTVRGetQTVRInstance';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
  =================================================================================================
    Viewing Angles and Zooming 
  -------------------------------------------------------------------------------------------------
}

{ QTVRSetViewParameter requires QTVR 5.0 (kQTVRAPIMajorVersion05 + kQTVRAPIMinorVersion00)}
{
 *  QTVRSetViewParameter()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Non-Carbon CFM:   in QuickTimeVRLib 5.0 and later
 *    Windows:          in QTVR.lib 5.0 and later
 }
function QTVRSetViewParameter( qtvr: QTVRInstance; viewParameter: UInt32; value: UnivPtr; flagsIn: UInt32 ): OSErr; external name '_QTVRSetViewParameter';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{ QTVRGetViewParameter requires QTVR 5.0 (kQTVRAPIMajorVersion05 + kQTVRAPIMinorVersion00)}
{
 *  QTVRGetViewParameter()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Non-Carbon CFM:   in QuickTimeVRLib 5.0 and later
 *    Windows:          in QTVR.lib 5.0 and later
 }
function QTVRGetViewParameter( qtvr: QTVRInstance; viewParameter: UInt32; value: UnivPtr; flagsIn: UInt32; var flagsOut: UInt32 ): OSErr; external name '_QTVRGetViewParameter';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  QTVRSetPanAngle()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRSetPanAngle( qtvr: QTVRInstance; panAngle: Float32 ): OSErr; external name '_QTVRSetPanAngle';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  QTVRGetPanAngle()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRGetPanAngle( qtvr: QTVRInstance ): Float32; external name '_QTVRGetPanAngle';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  QTVRSetTiltAngle()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRSetTiltAngle( qtvr: QTVRInstance; tiltAngle: Float32 ): OSErr; external name '_QTVRSetTiltAngle';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  QTVRGetTiltAngle()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRGetTiltAngle( qtvr: QTVRInstance ): Float32; external name '_QTVRGetTiltAngle';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  QTVRSetFieldOfView()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRSetFieldOfView( qtvr: QTVRInstance; fieldOfView: Float32 ): OSErr; external name '_QTVRSetFieldOfView';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  QTVRGetFieldOfView()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRGetFieldOfView( qtvr: QTVRInstance ): Float32; external name '_QTVRGetFieldOfView';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  QTVRShowDefaultView()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRShowDefaultView( qtvr: QTVRInstance ): OSErr; external name '_QTVRShowDefaultView';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{ Object Specific}
{
 *  QTVRSetViewCenter()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRSetViewCenter( qtvr: QTVRInstance; const (*var*) viewCenter: QTVRFloatPoint ): OSErr; external name '_QTVRSetViewCenter';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  QTVRGetViewCenter()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRGetViewCenter( qtvr: QTVRInstance; var viewCenter: QTVRFloatPoint ): OSErr; external name '_QTVRGetViewCenter';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  QTVRNudge()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRNudge( qtvr: QTVRInstance; direction: QTVRNudgeControl ): OSErr; external name '_QTVRNudge';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{ QTVRInteractionNudge requires QTVR 2.1 (kQTVRAPIMajorVersion02 + kQTVRAPIMinorVersion10)}
{
 *  QTVRInteractionNudge()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.1 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRInteractionNudge( qtvr: QTVRInstance; direction: QTVRNudgeControl ): OSErr; external name '_QTVRInteractionNudge';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
  =================================================================================================
    Scene and Node Location Information 
  -------------------------------------------------------------------------------------------------
}

{
 *  QTVRGetVRWorld()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRGetVRWorld( qtvr: QTVRInstance; var VRWorld: QTAtomContainer ): OSErr; external name '_QTVRGetVRWorld';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  QTVRGetNodeInfo()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRGetNodeInfo( qtvr: QTVRInstance; nodeID: UInt32; var nodeInfo: QTAtomContainer ): OSErr; external name '_QTVRGetNodeInfo';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  QTVRGoToNodeID()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRGoToNodeID( qtvr: QTVRInstance; nodeID: UInt32 ): OSErr; external name '_QTVRGoToNodeID';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  QTVRGetCurrentNodeID()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRGetCurrentNodeID( qtvr: QTVRInstance ): UInt32; external name '_QTVRGetCurrentNodeID';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  QTVRGetNodeType()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRGetNodeType( qtvr: QTVRInstance; nodeID: UInt32 ): OSType; external name '_QTVRGetNodeType';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
  =================================================================================================
    Hot Spot related calls 
  -------------------------------------------------------------------------------------------------
}

{
 *  QTVRPtToHotSpotID()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRPtToHotSpotID( qtvr: QTVRInstance; pt: Point; var hotSpotID: UInt32 ): OSErr; external name '_QTVRPtToHotSpotID';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{ QTVRGetHotSpotType requires QTVR 2.1 (kQTVRAPIMajorVersion02 + kQTVRAPIMinorVersion10)}
{
 *  QTVRGetHotSpotType()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.1 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRGetHotSpotType( qtvr: QTVRInstance; hotSpotID: UInt32; var hotSpotType: OSType ): OSErr; external name '_QTVRGetHotSpotType';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  QTVRTriggerHotSpot()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRTriggerHotSpot( qtvr: QTVRInstance; hotSpotID: UInt32; nodeInfo: QTAtomContainer; selectedAtom: QTAtom ): OSErr; external name '_QTVRTriggerHotSpot';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  QTVRSetMouseOverHotSpotProc()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRSetMouseOverHotSpotProc( qtvr: QTVRInstance; mouseOverHotSpotProc: QTVRMouseOverHotSpotUPP; refCon: SInt32; flags: UInt32 ): OSErr; external name '_QTVRSetMouseOverHotSpotProc';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  QTVREnableHotSpot()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVREnableHotSpot( qtvr: QTVRInstance; enableFlag: UInt32; hotSpotValue: UInt32; enable: Boolean ): OSErr; external name '_QTVREnableHotSpot';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  QTVRGetVisibleHotSpots()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRGetVisibleHotSpots( qtvr: QTVRInstance; hotSpots: Handle ): UInt32; external name '_QTVRGetVisibleHotSpots';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  QTVRGetHotSpotRegion()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRGetHotSpotRegion( qtvr: QTVRInstance; hotSpotID: UInt32; hotSpotRegion: RgnHandle ): OSErr; external name '_QTVRGetHotSpotRegion';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
  =================================================================================================
    Event & Cursor Handling Calls 
  -------------------------------------------------------------------------------------------------
}

{
 *  QTVRSetMouseOverTracking()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRSetMouseOverTracking( qtvr: QTVRInstance; enable: Boolean ): OSErr; external name '_QTVRSetMouseOverTracking';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  QTVRGetMouseOverTracking()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRGetMouseOverTracking( qtvr: QTVRInstance ): Boolean; external name '_QTVRGetMouseOverTracking';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  QTVRSetMouseDownTracking()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRSetMouseDownTracking( qtvr: QTVRInstance; enable: Boolean ): OSErr; external name '_QTVRSetMouseDownTracking';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  QTVRGetMouseDownTracking()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRGetMouseDownTracking( qtvr: QTVRInstance ): Boolean; external name '_QTVRGetMouseDownTracking';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  QTVRMouseEnter()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRMouseEnter( qtvr: QTVRInstance; pt: Point; var hotSpotID: UInt32; w: WindowRef ): OSErr; external name '_QTVRMouseEnter';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  QTVRMouseWithin()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRMouseWithin( qtvr: QTVRInstance; pt: Point; var hotSpotID: UInt32; w: WindowRef ): OSErr; external name '_QTVRMouseWithin';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  QTVRMouseLeave()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRMouseLeave( qtvr: QTVRInstance; pt: Point; w: WindowRef ): OSErr; external name '_QTVRMouseLeave';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  QTVRMouseDown()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRMouseDown( qtvr: QTVRInstance; pt: Point; when: UInt32; modifiers: UInt16; var hotSpotID: UInt32; w: WindowRef ): OSErr; external name '_QTVRMouseDown';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  QTVRMouseStillDown()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRMouseStillDown( qtvr: QTVRInstance; pt: Point; var hotSpotID: UInt32; w: WindowRef ): OSErr; external name '_QTVRMouseStillDown';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  QTVRMouseUp()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRMouseUp( qtvr: QTVRInstance; pt: Point; var hotSpotID: UInt32; w: WindowRef ): OSErr; external name '_QTVRMouseUp';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{ These require QTVR 2.01 (kQTVRAPIMajorVersion02 + kQTVRAPIMinorVersion01)}
{
 *  QTVRMouseStillDownExtended()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0.1 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRMouseStillDownExtended( qtvr: QTVRInstance; pt: Point; var hotSpotID: UInt32; w: WindowRef; when: UInt32; modifiers: UInt16 ): OSErr; external name '_QTVRMouseStillDownExtended';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  QTVRMouseUpExtended()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0.1 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRMouseUpExtended( qtvr: QTVRInstance; pt: Point; var hotSpotID: UInt32; w: WindowRef; when: UInt32; modifiers: UInt16 ): OSErr; external name '_QTVRMouseUpExtended';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
  =================================================================================================
    Intercept Routines 
  -------------------------------------------------------------------------------------------------
}

{
 *  QTVRInstallInterceptProc()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRInstallInterceptProc( qtvr: QTVRInstance; selector: QTVRProcSelector; interceptProc: QTVRInterceptUPP; refCon: SInt32; flags: UInt32 ): OSErr; external name '_QTVRInstallInterceptProc';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  QTVRCallInterceptedProc()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRCallInterceptedProc( qtvr: QTVRInstance; var qtvrMsg: QTVRInterceptRecord ): OSErr; external name '_QTVRCallInterceptedProc';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


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
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.1 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRGetCurrentMouseMode( qtvr: QTVRInstance ): UInt32; external name '_QTVRGetCurrentMouseMode';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  QTVRSetFrameRate()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRSetFrameRate( qtvr: QTVRInstance; rate: Float32 ): OSErr; external name '_QTVRSetFrameRate';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  QTVRGetFrameRate()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRGetFrameRate( qtvr: QTVRInstance ): Float32; external name '_QTVRGetFrameRate';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  QTVRSetViewRate()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRSetViewRate( qtvr: QTVRInstance; rate: Float32 ): OSErr; external name '_QTVRSetViewRate';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  QTVRGetViewRate()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRGetViewRate( qtvr: QTVRInstance ): Float32; external name '_QTVRGetViewRate';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  QTVRSetViewCurrentTime()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRSetViewCurrentTime( qtvr: QTVRInstance; time: TimeValue ): OSErr; external name '_QTVRSetViewCurrentTime';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  QTVRGetViewCurrentTime()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRGetViewCurrentTime( qtvr: QTVRInstance ): TimeValue; external name '_QTVRGetViewCurrentTime';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  QTVRGetCurrentViewDuration()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRGetCurrentViewDuration( qtvr: QTVRInstance ): TimeValue; external name '_QTVRGetCurrentViewDuration';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
  =================================================================================================
   View State Calls - QTVR Object Only
  -------------------------------------------------------------------------------------------------
}

{
 *  QTVRSetViewState()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRSetViewState( qtvr: QTVRInstance; viewStateType: QTVRViewStateType; state: UInt16 ): OSErr; external name '_QTVRSetViewState';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  QTVRGetViewState()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRGetViewState( qtvr: QTVRInstance; viewStateType: QTVRViewStateType; var state: UInt16 ): OSErr; external name '_QTVRGetViewState';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  QTVRGetViewStateCount()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRGetViewStateCount( qtvr: QTVRInstance ): UInt16; external name '_QTVRGetViewStateCount';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  QTVRSetAnimationSetting()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRSetAnimationSetting( qtvr: QTVRInstance; setting: QTVRObjectAnimationSetting; enable: Boolean ): OSErr; external name '_QTVRSetAnimationSetting';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  QTVRGetAnimationSetting()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRGetAnimationSetting( qtvr: QTVRInstance; setting: QTVRObjectAnimationSetting; var enable: Boolean ): OSErr; external name '_QTVRGetAnimationSetting';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  QTVRSetControlSetting()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRSetControlSetting( qtvr: QTVRInstance; setting: QTVRControlSetting; enable: Boolean ): OSErr; external name '_QTVRSetControlSetting';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  QTVRGetControlSetting()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRGetControlSetting( qtvr: QTVRInstance; setting: QTVRControlSetting; var enable: Boolean ): OSErr; external name '_QTVRGetControlSetting';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  QTVREnableFrameAnimation()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVREnableFrameAnimation( qtvr: QTVRInstance; enable: Boolean ): OSErr; external name '_QTVREnableFrameAnimation';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  QTVRGetFrameAnimation()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRGetFrameAnimation( qtvr: QTVRInstance ): Boolean; external name '_QTVRGetFrameAnimation';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  QTVREnableViewAnimation()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVREnableViewAnimation( qtvr: QTVRInstance; enable: Boolean ): OSErr; external name '_QTVREnableViewAnimation';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  QTVRGetViewAnimation()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRGetViewAnimation( qtvr: QTVRInstance ): Boolean; external name '_QTVRGetViewAnimation';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
  =================================================================================================
    Imaging Characteristics 
  -------------------------------------------------------------------------------------------------
}

{
 *  QTVRSetVisible()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRSetVisible( qtvr: QTVRInstance; visible: Boolean ): OSErr; external name '_QTVRSetVisible';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  QTVRGetVisible()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRGetVisible( qtvr: QTVRInstance ): Boolean; external name '_QTVRGetVisible';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  QTVRSetImagingProperty()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRSetImagingProperty( qtvr: QTVRInstance; imagingMode: QTVRImagingMode; imagingProperty: UInt32; propertyValue: SInt32 ): OSErr; external name '_QTVRSetImagingProperty';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  QTVRGetImagingProperty()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRGetImagingProperty( qtvr: QTVRInstance; imagingMode: QTVRImagingMode; imagingProperty: UInt32; var propertyValue: SInt32 ): OSErr; external name '_QTVRGetImagingProperty';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  QTVRUpdate()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRUpdate( qtvr: QTVRInstance; imagingMode: QTVRImagingMode ): OSErr; external name '_QTVRUpdate';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  QTVRBeginUpdateStream()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRBeginUpdateStream( qtvr: QTVRInstance; imagingMode: QTVRImagingMode ): OSErr; external name '_QTVRBeginUpdateStream';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  QTVREndUpdateStream()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVREndUpdateStream( qtvr: QTVRInstance ): OSErr; external name '_QTVREndUpdateStream';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  QTVRSetTransitionProperty()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRSetTransitionProperty( qtvr: QTVRInstance; transitionType: UInt32; transitionProperty: UInt32; transitionValue: SInt32 ): OSErr; external name '_QTVRSetTransitionProperty';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  QTVREnableTransition()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVREnableTransition( qtvr: QTVRInstance; transitionType: UInt32; enable: Boolean ): OSErr; external name '_QTVREnableTransition';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
  =================================================================================================
    Basic Conversion and Math Routines 
  -------------------------------------------------------------------------------------------------
}

{
 *  QTVRSetAngularUnits()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRSetAngularUnits( qtvr: QTVRInstance; units: QTVRAngularUnits ): OSErr; external name '_QTVRSetAngularUnits';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  QTVRGetAngularUnits()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRGetAngularUnits( qtvr: QTVRInstance ): QTVRAngularUnits; external name '_QTVRGetAngularUnits';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{ Pano specific routines}
{
 *  QTVRPtToAngles()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRPtToAngles( qtvr: QTVRInstance; pt: Point; var panAngle: Float32; var tiltAngle: Float32 ): OSErr; external name '_QTVRPtToAngles';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  QTVRCoordToAngles()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRCoordToAngles( qtvr: QTVRInstance; var coord: QTVRFloatPoint; var panAngle: Float32; var tiltAngle: Float32 ): OSErr; external name '_QTVRCoordToAngles';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  QTVRAnglesToCoord()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRAnglesToCoord( qtvr: QTVRInstance; panAngle: Float32; tiltAngle: Float32; var coord: QTVRFloatPoint ): OSErr; external name '_QTVRAnglesToCoord';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{ Object specific routines}
{
 *  QTVRPanToColumn()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRPanToColumn( qtvr: QTVRInstance; panAngle: Float32 ): SInt16; external name '_QTVRPanToColumn';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{ zero based   }
{
 *  QTVRColumnToPan()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRColumnToPan( qtvr: QTVRInstance; column: SInt16 ): Float32; external name '_QTVRColumnToPan';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{ zero based   }
{
 *  QTVRTiltToRow()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRTiltToRow( qtvr: QTVRInstance; tiltAngle: Float32 ): SInt16; external name '_QTVRTiltToRow';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{ zero based   }
{
 *  QTVRRowToTilt()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRRowToTilt( qtvr: QTVRInstance; row: SInt16 ): Float32; external name '_QTVRRowToTilt';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{ zero based               }
{
 *  QTVRWrapAndConstrain()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRWrapAndConstrain( qtvr: QTVRInstance; kind: SInt16; value: Float32; var result: Float32 ): OSErr; external name '_QTVRWrapAndConstrain';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
  =================================================================================================
    Interaction Routines 
  -------------------------------------------------------------------------------------------------
}

{
 *  QTVRSetEnteringNodeProc()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRSetEnteringNodeProc( qtvr: QTVRInstance; enteringNodeProc: QTVREnteringNodeUPP; refCon: SInt32; flags: UInt32 ): OSErr; external name '_QTVRSetEnteringNodeProc';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  QTVRSetLeavingNodeProc()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRSetLeavingNodeProc( qtvr: QTVRInstance; leavingNodeProc: QTVRLeavingNodeUPP; refCon: SInt32; flags: UInt32 ): OSErr; external name '_QTVRSetLeavingNodeProc';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  QTVRSetInteractionProperty()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRSetInteractionProperty( qtvr: QTVRInstance; proprty: UInt32; value: UnivPtr ): OSErr; external name '_QTVRSetInteractionProperty';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  QTVRGetInteractionProperty()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRGetInteractionProperty( qtvr: QTVRInstance; proprty: UInt32; value: UnivPtr ): OSErr; external name '_QTVRGetInteractionProperty';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  QTVRReplaceCursor()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRReplaceCursor( qtvr: QTVRInstance; var cursRecord: QTVRCursorRecord ): OSErr; external name '_QTVRReplaceCursor';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
  =================================================================================================
    Viewing Limits and Constraints 
  -------------------------------------------------------------------------------------------------
}

{
 *  QTVRGetViewingLimits()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRGetViewingLimits( qtvr: QTVRInstance; kind: UInt16; var minValue: Float32; var maxValue: Float32 ): OSErr; external name '_QTVRGetViewingLimits';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  QTVRGetConstraintStatus()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRGetConstraintStatus( qtvr: QTVRInstance ): UInt32; external name '_QTVRGetConstraintStatus';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  QTVRGetConstraints()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRGetConstraints( qtvr: QTVRInstance; kind: UInt16; var minValue: Float32; var maxValue: Float32 ): OSErr; external name '_QTVRGetConstraints';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  QTVRSetConstraints()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRSetConstraints( qtvr: QTVRInstance; kind: UInt16; minValue: Float32; maxValue: Float32 ): OSErr; external name '_QTVRSetConstraints';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
  =================================================================================================
    Back Buffer Memory Management 
  -------------------------------------------------------------------------------------------------
}

{
 *  QTVRGetAvailableResolutions()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRGetAvailableResolutions( qtvr: QTVRInstance; var resolutionsMask: UInt16 ): OSErr; external name '_QTVRGetAvailableResolutions';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{ These require QTVR 2.1 (kQTVRAPIMajorVersion02 + kQTVRAPIMinorVersion10)}
{
 *  QTVRGetBackBufferMemInfo()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.1 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRGetBackBufferMemInfo( qtvr: QTVRInstance; geometry: UInt32; resolution: UInt16; cachePixelFormat: UInt32; var minCacheBytes: SInt32; var suggestedCacheBytes: SInt32; var fullCacheBytes: SInt32 ): OSErr; external name '_QTVRGetBackBufferMemInfo';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  QTVRGetBackBufferSettings()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.1 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRGetBackBufferSettings( qtvr: QTVRInstance; var geometry: UInt32; var resolution: UInt16; var cachePixelFormat: UInt32; var cacheSize: SInt16 ): OSErr; external name '_QTVRGetBackBufferSettings';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  QTVRSetBackBufferPrefs()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.1 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRSetBackBufferPrefs( qtvr: QTVRInstance; geometry: UInt32; resolution: UInt16; cachePixelFormat: UInt32; cacheSize: SInt16 ): OSErr; external name '_QTVRSetBackBufferPrefs';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
  =================================================================================================
    Buffer Access 
  -------------------------------------------------------------------------------------------------
}

{
 *  QTVRSetPrescreenImagingCompleteProc()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRSetPrescreenImagingCompleteProc( qtvr: QTVRInstance; imagingCompleteProc: QTVRImagingCompleteUPP; refCon: SInt32; flags: UInt32 ): OSErr; external name '_QTVRSetPrescreenImagingCompleteProc';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  QTVRSetBackBufferImagingProc()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRSetBackBufferImagingProc( qtvr: QTVRInstance; backBufferImagingProc: QTVRBackBufferImagingUPP; numAreas: UInt16; areasOfInterest: {variable-size-array} QTVRAreaOfInterestPtr; refCon: SInt32 ): OSErr; external name '_QTVRSetBackBufferImagingProc';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  QTVRRefreshBackBuffer()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QuickTimeVRLib 2.0 and later
 *    Windows:          in QTVR.lib 2.1 and later
 }
function QTVRRefreshBackBuffer( qtvr: QTVRInstance; flags: UInt32 ): OSErr; external name '_QTVRRefreshBackBuffer';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{$endc} {not TARGET_CPU_64}

{$endc} {TARGET_OS_MAC}
{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}

end.
{$endc} {not MACOSALLINCLUDE}
