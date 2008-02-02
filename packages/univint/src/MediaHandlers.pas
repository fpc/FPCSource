{
     File:       MediaHandlers.p
 
     Contains:   QuickTime Interfaces.
 
     Version:    Technology: QuickTime 6.0
                 Release:    Universal Interfaces 3.4.2
 
     Copyright:  © 1990-2002 by Apple Computer, Inc., all rights reserved
 
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

unit MediaHandlers;
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
uses MacTypes,Quickdraw,Events,ImageCompression,ConditionalMacros,Components,Sound,Movies;


{$ALIGN MAC68K}


type
{$ifc TYPED_FUNCTION_POINTERS}
	PrePrerollCompleteProcPtr = procedure(mh: MediaHandler; err: OSErr; refcon: UnivPtr);
{$elsec}
	PrePrerollCompleteProcPtr = ProcPtr;
{$endc}

{$ifc OPAQUE_UPP_TYPES}
	PrePrerollCompleteUPP = ^SInt32; { an opaque UPP }
{$elsec}
	PrePrerollCompleteUPP = UniversalProcPtr;
{$endc}	

const
	handlerHasSpatial			= $01;
	handlerCanClip				= $02;
	handlerCanMatte				= $04;
	handlerCanTransferMode		= $08;
	handlerNeedsBuffer			= $10;
	handlerNoIdle				= $20;
	handlerNoScheduler			= $40;
	handlerWantsTime			= $80;
	handlerCGrafPortOnly		= $0100;
	handlerCanSend				= $0200;
	handlerCanHandleComplexMatrix = $0400;
	handlerWantsDestinationPixels = $0800;
	handlerCanSendImageData		= $1000;
	handlerCanPicSave			= $2000;

	{	 media task flags 	}
	mMustDraw					= $08;
	mAtEnd						= $10;
	mPreflightDraw				= $20;
	mSyncDrawing				= $40;
	mPrecompositeOnly			= $0200;
	mSoundOnly					= $0400;
	mDoIdleActionsBeforeDraws	= $0800;
	mDisableIdleActions			= $1000;

	{	 media task result flags 	}
	mDidDraw					= $01;
	mNeedsToDraw				= $04;
	mDrawAgain					= $08;
	mPartialDraw				= $10;
	mWantIdleActions			= $20;

	forceUpdateRedraw			= $01;
	forceUpdateNewBuffer		= $02;

	{	 media hit test flags 	}
	mHitTestBounds				= $00000001;					{     point must only be within targetRefCon's bounding box  }
	mHitTestImage				= $00000002;					{   point must be within the shape of the targetRefCon's image  }
	mHitTestInvisible			= $00000004;					{   invisible targetRefCon's may be hit tested  }
	mHitTestIsClick				= $00000008;					{   for codecs that want mouse events  }

	{	 media is opaque flags 	}
	mOpaque						= $00000001;
	mInvisible					= $00000002;

	{	 MediaSetPublicInfo/MediaGetPublicInfo selectors 	}
	kMediaQTIdleFrequencySelector = $69646671 (* 'idfq' *);


type
	GetMovieCompleteParamsPtr = ^GetMovieCompleteParams;
	GetMovieCompleteParams = record
		version:				SInt16;
		theMovie:				Movie;
		theTrack:				Track;
		theMedia:				Media;
		movieScale:				TimeScale;
		mediaScale:				TimeScale;
		movieDuration:			TimeValue;
		trackDuration:			TimeValue;
		mediaDuration:			TimeValue;
		effectiveRate:			Fixed;
		timeBase:				TimeBase_fix;
		volume:					SInt16;
		width:					Fixed;
		height:					Fixed;
		trackMovieMatrix:		MatrixRecord;
		moviePort:				CGrafPtr;
		movieGD:				GDHandle;
		trackMatte:				PixMapHandle;
		inputMap:				QTAtomContainer;
		mediaContextID:			QTMediaContextID;
	end;


const
	kMediaVideoParamBrightness	= 1;
	kMediaVideoParamContrast	= 2;
	kMediaVideoParamHue			= 3;
	kMediaVideoParamSharpness	= 4;
	kMediaVideoParamSaturation	= 5;
	kMediaVideoParamBlackLevel	= 6;
	kMediaVideoParamWhiteLevel	= 7;

	{  These are for MediaGetInfo() and MediaSetInfo(). }
	kMHInfoEncodedFrameRate		= $6F726174 (* 'orat' *);						{  Parameter is a MHInfoEncodedFrameRateRecord*. }

	{  This holds the frame rate at which the track was encoded. }

type
	MHInfoEncodedFrameRateRecordPtr = ^MHInfoEncodedFrameRateRecord;
	MHInfoEncodedFrameRateRecord = record
		encodedFrameRate:		Fixed;
	end;

	dataHandlePtr						= ^Handle;
	dataHandleHandle					= ^dataHandlePtr;

	QTCustomActionTargetRecordPtr = ^QTCustomActionTargetRecord;
	QTCustomActionTargetRecord = record
		movie:					Movie_fix;
		doMCActionCallbackProc:	DoMCActionUPP;
		callBackRefcon:			SInt32;
		track:					Track_fix;
		trackObjectRefCon:		SInt32;
		defaultTrack:			Track_fix;
		defaultObjectRefCon:	SInt32;
		reserved1:				SInt32;
		reserved2:				SInt32;
	end;

	QTCustomActionTargetPtr				= ^QTCustomActionTargetRecord;
	MediaEQSpectrumBandsRecordPtr = ^MediaEQSpectrumBandsRecord;
	MediaEQSpectrumBandsRecord = record
		count:					SInt16;
		frequency:				UnsignedFixedPtr;						{  pointer to array of frequencies }
	end;

	{
	 *  CallComponentExecuteWiredAction()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
	 *    CarbonLib:        in CarbonLib 1.0.2 and later
	 *    Mac OS X:         in version 10.0 and later
	 *    Windows:          in qtmlClient.lib 4.0 and later
	 	}
function CallComponentExecuteWiredAction(ci: ComponentInstance; actionContainer: QTAtomContainer; actionAtom: QTAtom; target: QTCustomActionTargetPtr; event: QTEventRecordPtr): ComponentResult; external name '_CallComponentExecuteWiredAction';
{ MediaCallRange2 }
{ These are unique to each type of media handler }
{ They are also included in the public interfaces }


{  Flags for MediaSetChunkManagementFlags }

const
	kEmptyPurgableChunksOverAllowance = 1;

	{
	 *  MediaSetChunkManagementFlags()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in QuickTimeLib 6.0 and later
	 *    CarbonLib:        in CarbonLib 1.6 and later
	 *    Mac OS X:         in version 10.2 and later
	 *    Windows:          in qtmlClient.lib 6.0 and later
	 	}
function MediaSetChunkManagementFlags(mh: MediaHandler; flags: UInt32; flagsMask: UInt32): ComponentResult; external name '_MediaSetChunkManagementFlags';
{
 *  MediaGetChunkManagementFlags()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 6.0 and later
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.2 and later
 *    Windows:          in qtmlClient.lib 6.0 and later
 }
function MediaGetChunkManagementFlags(mh: MediaHandler; var flags: UInt32): ComponentResult; external name '_MediaGetChunkManagementFlags';
{
 *  MediaSetPurgeableChunkMemoryAllowance()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 6.0 and later
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.2 and later
 *    Windows:          in qtmlClient.lib 6.0 and later
 }
function MediaSetPurgeableChunkMemoryAllowance(mh: MediaHandler; allowance: Size): ComponentResult; external name '_MediaSetPurgeableChunkMemoryAllowance';
{
 *  MediaGetPurgeableChunkMemoryAllowance()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 6.0 and later
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.2 and later
 *    Windows:          in qtmlClient.lib 6.0 and later
 }
function MediaGetPurgeableChunkMemoryAllowance(mh: MediaHandler; var allowance: Size): ComponentResult; external name '_MediaGetPurgeableChunkMemoryAllowance';
{
 *  MediaEmptyAllPurgeableChunks()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 6.0 and later
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.2 and later
 *    Windows:          in qtmlClient.lib 6.0 and later
 }
function MediaEmptyAllPurgeableChunks(mh: MediaHandler): ComponentResult; external name '_MediaEmptyAllPurgeableChunks';
{**** These are the calls for dealing with the Generic media handler ****}
{
 *  MediaInitialize()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MediaInitialize(mh: MediaHandler; var gmc: GetMovieCompleteParams): ComponentResult; external name '_MediaInitialize';
{
 *  MediaSetHandlerCapabilities()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MediaSetHandlerCapabilities(mh: MediaHandler; flags: SInt32; flagsMask: SInt32): ComponentResult; external name '_MediaSetHandlerCapabilities';
{
 *  MediaIdle()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MediaIdle(mh: MediaHandler; atMediaTime: TimeValue; flagsIn: SInt32; var flagsOut: SInt32; const (*var*) movieTime: TimeRecord): ComponentResult; external name '_MediaIdle';
{
 *  MediaGetMediaInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MediaGetMediaInfo(mh: MediaHandler; h: Handle): ComponentResult; external name '_MediaGetMediaInfo';
{
 *  MediaPutMediaInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MediaPutMediaInfo(mh: MediaHandler; h: Handle): ComponentResult; external name '_MediaPutMediaInfo';
{
 *  MediaSetActive()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MediaSetActive(mh: MediaHandler; enableMedia: boolean): ComponentResult; external name '_MediaSetActive';
{
 *  MediaSetRate()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MediaSetRate(mh: MediaHandler; rate: Fixed): ComponentResult; external name '_MediaSetRate';
{
 *  MediaGGetStatus()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MediaGGetStatus(mh: MediaHandler; var statusErr: ComponentResult): ComponentResult; external name '_MediaGGetStatus';
{
 *  MediaTrackEdited()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MediaTrackEdited(mh: MediaHandler): ComponentResult; external name '_MediaTrackEdited';
{
 *  MediaSetMediaTimeScale()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MediaSetMediaTimeScale(mh: MediaHandler; newTimeScale: TimeScale): ComponentResult; external name '_MediaSetMediaTimeScale';
{
 *  MediaSetMovieTimeScale()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MediaSetMovieTimeScale(mh: MediaHandler; newTimeScale: TimeScale): ComponentResult; external name '_MediaSetMovieTimeScale';
{
 *  MediaSetGWorld()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MediaSetGWorld(mh: MediaHandler; aPort: CGrafPtr; aGD: GDHandle): ComponentResult; external name '_MediaSetGWorld';
{
 *  MediaSetDimensions()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MediaSetDimensions(mh: MediaHandler; width: Fixed; height: Fixed): ComponentResult; external name '_MediaSetDimensions';
{
 *  MediaSetClip()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MediaSetClip(mh: MediaHandler; theClip: RgnHandle): ComponentResult; external name '_MediaSetClip';
{
 *  MediaSetMatrix()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MediaSetMatrix(mh: MediaHandler; var trackMovieMatrix: MatrixRecord): ComponentResult; external name '_MediaSetMatrix';
{
 *  MediaGetTrackOpaque()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MediaGetTrackOpaque(mh: MediaHandler; var trackIsOpaque: boolean): ComponentResult; external name '_MediaGetTrackOpaque';
{
 *  MediaSetGraphicsMode()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MediaSetGraphicsMode(mh: MediaHandler; mode: SInt32; const (*var*) opColor: RGBColor): ComponentResult; external name '_MediaSetGraphicsMode';
{
 *  MediaGetGraphicsMode()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MediaGetGraphicsMode(mh: MediaHandler; var mode: SInt32; var opColor: RGBColor): ComponentResult; external name '_MediaGetGraphicsMode';
{
 *  MediaGSetVolume()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MediaGSetVolume(mh: MediaHandler; volume: SInt16): ComponentResult; external name '_MediaGSetVolume';
{
 *  MediaSetSoundBalance()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MediaSetSoundBalance(mh: MediaHandler; balance: SInt16): ComponentResult; external name '_MediaSetSoundBalance';
{
 *  MediaGetSoundBalance()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MediaGetSoundBalance(mh: MediaHandler; var balance: SInt16): ComponentResult; external name '_MediaGetSoundBalance';
{
 *  MediaGetNextBoundsChange()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MediaGetNextBoundsChange(mh: MediaHandler; var when: TimeValue): ComponentResult; external name '_MediaGetNextBoundsChange';
{
 *  MediaGetSrcRgn()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MediaGetSrcRgn(mh: MediaHandler; rgn: RgnHandle; atMediaTime: TimeValue): ComponentResult; external name '_MediaGetSrcRgn';
{
 *  MediaPreroll()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MediaPreroll(mh: MediaHandler; time: TimeValue; rate: Fixed): ComponentResult; external name '_MediaPreroll';
{
 *  MediaSampleDescriptionChanged()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MediaSampleDescriptionChanged(mh: MediaHandler; index: SInt32): ComponentResult; external name '_MediaSampleDescriptionChanged';
{
 *  MediaHasCharacteristic()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MediaHasCharacteristic(mh: MediaHandler; characteristic: OSType; var hasIt: boolean): ComponentResult; external name '_MediaHasCharacteristic';
{
 *  MediaGetOffscreenBufferSize()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MediaGetOffscreenBufferSize(mh: MediaHandler; var bounds: Rect; depth: SInt16; ctab: CTabHandle): ComponentResult; external name '_MediaGetOffscreenBufferSize';
{
 *  MediaSetHints()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MediaSetHints(mh: MediaHandler; hints: SInt32): ComponentResult; external name '_MediaSetHints';
{
 *  MediaGetName()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MediaGetName(mh: MediaHandler; var name: Str255; requestedLanguage: SInt32; var actualLanguage: SInt32): ComponentResult; external name '_MediaGetName';
{
 *  MediaForceUpdate()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MediaForceUpdate(mh: MediaHandler; forceUpdateFlags: SInt32): ComponentResult; external name '_MediaForceUpdate';
{
 *  MediaGetDrawingRgn()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MediaGetDrawingRgn(mh: MediaHandler; var partialRgn: RgnHandle): ComponentResult; external name '_MediaGetDrawingRgn';
{
 *  MediaGSetActiveSegment()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MediaGSetActiveSegment(mh: MediaHandler; activeStart: TimeValue; activeDuration: TimeValue): ComponentResult; external name '_MediaGSetActiveSegment';
{
 *  MediaInvalidateRegion()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MediaInvalidateRegion(mh: MediaHandler; invalRgn: RgnHandle): ComponentResult; external name '_MediaInvalidateRegion';
{
 *  MediaGetNextStepTime()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MediaGetNextStepTime(mh: MediaHandler; flags: SInt16; mediaTimeIn: TimeValue; var mediaTimeOut: TimeValue; rate: Fixed): ComponentResult; external name '_MediaGetNextStepTime';
{
 *  MediaSetNonPrimarySourceData()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MediaSetNonPrimarySourceData(mh: MediaHandler; inputIndex: SInt32; dataDescriptionSeed: SInt32; dataDescription: Handle; data: UnivPtr; dataSize: SInt32; asyncCompletionProc: ICMCompletionProcRecordPtr; transferProc: ICMConvertDataFormatUPP; refCon: UnivPtr): ComponentResult; external name '_MediaSetNonPrimarySourceData';
{
 *  MediaChangedNonPrimarySource()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MediaChangedNonPrimarySource(mh: MediaHandler; inputIndex: SInt32): ComponentResult; external name '_MediaChangedNonPrimarySource';
{
 *  MediaTrackReferencesChanged()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MediaTrackReferencesChanged(mh: MediaHandler): ComponentResult; external name '_MediaTrackReferencesChanged';
{
 *  MediaGetSampleDataPointer()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MediaGetSampleDataPointer(mh: MediaHandler; sampleNum: SInt32; var dataPtr: Ptr; var dataSize: SInt32; var sampleDescIndex: SInt32): ComponentResult; external name '_MediaGetSampleDataPointer';
{
 *  MediaReleaseSampleDataPointer()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MediaReleaseSampleDataPointer(mh: MediaHandler; sampleNum: SInt32): ComponentResult; external name '_MediaReleaseSampleDataPointer';
{
 *  MediaTrackPropertyAtomChanged()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MediaTrackPropertyAtomChanged(mh: MediaHandler): ComponentResult; external name '_MediaTrackPropertyAtomChanged';
{
 *  MediaSetTrackInputMapReference()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MediaSetTrackInputMapReference(mh: MediaHandler; inputMap: QTAtomContainer): ComponentResult; external name '_MediaSetTrackInputMapReference';
{
 *  MediaSetVideoParam()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MediaSetVideoParam(mh: MediaHandler; whichParam: SInt32; var value: UInt16): ComponentResult; external name '_MediaSetVideoParam';
{
 *  MediaGetVideoParam()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MediaGetVideoParam(mh: MediaHandler; whichParam: SInt32; var value: UInt16): ComponentResult; external name '_MediaGetVideoParam';
{
 *  MediaCompare()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MediaCompare(mh: MediaHandler; var isOK: boolean; srcMedia: Media; srcMediaComponent: ComponentInstance): ComponentResult; external name '_MediaCompare';
{
 *  MediaGetClock()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MediaGetClock(mh: MediaHandler; var clock: ComponentInstance): ComponentResult; external name '_MediaGetClock';
{
 *  MediaSetSoundOutputComponent()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MediaSetSoundOutputComponent(mh: MediaHandler; outputComponent: Component): ComponentResult; external name '_MediaSetSoundOutputComponent';
{
 *  MediaGetSoundOutputComponent()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MediaGetSoundOutputComponent(mh: MediaHandler; var outputComponent: Component): ComponentResult; external name '_MediaGetSoundOutputComponent';
{
 *  MediaSetSoundLocalizationData()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MediaSetSoundLocalizationData(mh: MediaHandler; data: Handle): ComponentResult; external name '_MediaSetSoundLocalizationData';
{
 *  MediaGetInvalidRegion()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MediaGetInvalidRegion(mh: MediaHandler; rgn: RgnHandle): ComponentResult; external name '_MediaGetInvalidRegion';
{
 *  MediaSampleDescriptionB2N()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MediaSampleDescriptionB2N(mh: MediaHandler; sampleDescriptionH: SampleDescriptionHandle): ComponentResult; external name '_MediaSampleDescriptionB2N';
{
 *  MediaSampleDescriptionN2B()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MediaSampleDescriptionN2B(mh: MediaHandler; sampleDescriptionH: SampleDescriptionHandle): ComponentResult; external name '_MediaSampleDescriptionN2B';
{
 *  MediaQueueNonPrimarySourceData()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MediaQueueNonPrimarySourceData(mh: MediaHandler; inputIndex: SInt32; dataDescriptionSeed: SInt32; dataDescription: Handle; data: UnivPtr; dataSize: SInt32; asyncCompletionProc: ICMCompletionProcRecordPtr; const (*var*) frameTime: ICMFrameTimeRecord; transferProc: ICMConvertDataFormatUPP; refCon: UnivPtr): ComponentResult; external name '_MediaQueueNonPrimarySourceData';
{
 *  MediaFlushNonPrimarySourceData()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MediaFlushNonPrimarySourceData(mh: MediaHandler; inputIndex: SInt32): ComponentResult; external name '_MediaFlushNonPrimarySourceData';
{
 *  MediaGetURLLink()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MediaGetURLLink(mh: MediaHandler; displayWhere: Point; var urlLink: Handle): ComponentResult; external name '_MediaGetURLLink';
{
 *  MediaMakeMediaTimeTable()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MediaMakeMediaTimeTable(mh: MediaHandler; var offsets: SInt32Ptr; startTime: TimeValue; endTime: TimeValue; timeIncrement: TimeValue; firstDataRefIndex: SInt16; lastDataRefIndex: SInt16; var retDataRefSkew: SInt32): ComponentResult; external name '_MediaMakeMediaTimeTable';
{
 *  MediaHitTestForTargetRefCon()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MediaHitTestForTargetRefCon(mh: MediaHandler; flags: SInt32; loc: Point; var targetRefCon: SInt32): ComponentResult; external name '_MediaHitTestForTargetRefCon';
{
 *  MediaHitTestTargetRefCon()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MediaHitTestTargetRefCon(mh: MediaHandler; targetRefCon: SInt32; flags: SInt32; loc: Point; var wasHit: boolean): ComponentResult; external name '_MediaHitTestTargetRefCon';
{
 *  MediaGetActionsForQTEvent()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MediaGetActionsForQTEvent(mh: MediaHandler; event: QTEventRecordPtr; targetRefCon: SInt32; var container: QTAtomContainer; var atom: QTAtom): ComponentResult; external name '_MediaGetActionsForQTEvent';
{
 *  MediaDisposeTargetRefCon()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MediaDisposeTargetRefCon(mh: MediaHandler; targetRefCon: SInt32): ComponentResult; external name '_MediaDisposeTargetRefCon';
{
 *  MediaTargetRefConsEqual()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MediaTargetRefConsEqual(mh: MediaHandler; firstRefCon: SInt32; secondRefCon: SInt32; var equal: boolean): ComponentResult; external name '_MediaTargetRefConsEqual';
{
 *  MediaSetActionsCallback()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MediaSetActionsCallback(mh: MediaHandler; actionsCallbackProc: ActionsUPP; refcon: UnivPtr): ComponentResult; external name '_MediaSetActionsCallback';
{
 *  MediaPrePrerollBegin()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MediaPrePrerollBegin(mh: MediaHandler; time: TimeValue; rate: Fixed; completeProc: PrePrerollCompleteUPP; refcon: UnivPtr): ComponentResult; external name '_MediaPrePrerollBegin';
{
 *  MediaPrePrerollCancel()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MediaPrePrerollCancel(mh: MediaHandler; refcon: UnivPtr): ComponentResult; external name '_MediaPrePrerollCancel';
{
 *  MediaEnterEmptyEdit()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MediaEnterEmptyEdit(mh: MediaHandler): ComponentResult; external name '_MediaEnterEmptyEdit';
{
 *  MediaCurrentMediaQueuedData()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MediaCurrentMediaQueuedData(mh: MediaHandler; var milliSecs: SInt32): ComponentResult; external name '_MediaCurrentMediaQueuedData';
{
 *  MediaGetEffectiveVolume()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function MediaGetEffectiveVolume(mh: MediaHandler; var volume: SInt16): ComponentResult; external name '_MediaGetEffectiveVolume';
{
 *  MediaResolveTargetRefCon()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function MediaResolveTargetRefCon(mh: MediaHandler; container: QTAtomContainer; atom: QTAtom; var targetRefCon: SInt32): ComponentResult; external name '_MediaResolveTargetRefCon';
{
 *  MediaGetSoundLevelMeteringEnabled()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function MediaGetSoundLevelMeteringEnabled(mh: MediaHandler; var enabled: boolean): ComponentResult; external name '_MediaGetSoundLevelMeteringEnabled';
{
 *  MediaSetSoundLevelMeteringEnabled()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function MediaSetSoundLevelMeteringEnabled(mh: MediaHandler; enable: boolean): ComponentResult; external name '_MediaSetSoundLevelMeteringEnabled';
{
 *  MediaGetSoundLevelMeterInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function MediaGetSoundLevelMeterInfo(mh: MediaHandler; levelInfo: LevelMeterInfoPtr): ComponentResult; external name '_MediaGetSoundLevelMeterInfo';
{
 *  MediaGetEffectiveSoundBalance()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function MediaGetEffectiveSoundBalance(mh: MediaHandler; var balance: SInt16): ComponentResult; external name '_MediaGetEffectiveSoundBalance';
{
 *  MediaSetScreenLock()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function MediaSetScreenLock(mh: MediaHandler; lockIt: boolean): ComponentResult; external name '_MediaSetScreenLock';
{
 *  MediaSetDoMCActionCallback()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function MediaSetDoMCActionCallback(mh: MediaHandler; doMCActionCallbackProc: DoMCActionUPP; refcon: UnivPtr): ComponentResult; external name '_MediaSetDoMCActionCallback';
{
 *  MediaGetErrorString()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function MediaGetErrorString(mh: MediaHandler; theError: ComponentResult; var errorString: Str255): ComponentResult; external name '_MediaGetErrorString';
{
 *  MediaGetSoundEqualizerBands()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function MediaGetSoundEqualizerBands(mh: MediaHandler; spectrumInfo: MediaEQSpectrumBandsRecordPtr): ComponentResult; external name '_MediaGetSoundEqualizerBands';
{
 *  MediaSetSoundEqualizerBands()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function MediaSetSoundEqualizerBands(mh: MediaHandler; spectrumInfo: MediaEQSpectrumBandsRecordPtr): ComponentResult; external name '_MediaSetSoundEqualizerBands';
{
 *  MediaGetSoundEqualizerBandLevels()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function MediaGetSoundEqualizerBandLevels(mh: MediaHandler; var bandLevels: UInt8): ComponentResult; external name '_MediaGetSoundEqualizerBandLevels';
{
 *  MediaDoIdleActions()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function MediaDoIdleActions(mh: MediaHandler): ComponentResult; external name '_MediaDoIdleActions';
{
 *  MediaSetSoundBassAndTreble()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function MediaSetSoundBassAndTreble(mh: MediaHandler; bass: SInt16; treble: SInt16): ComponentResult; external name '_MediaSetSoundBassAndTreble';
{
 *  MediaGetSoundBassAndTreble()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function MediaGetSoundBassAndTreble(mh: MediaHandler; var bass: SInt16; var treble: SInt16): ComponentResult; external name '_MediaGetSoundBassAndTreble';
{
 *  MediaTimeBaseChanged()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function MediaTimeBaseChanged(mh: MediaHandler): ComponentResult; external name '_MediaTimeBaseChanged';
{
 *  MediaMCIsPlayerEvent()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.1 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.1 and later
 }
function MediaMCIsPlayerEvent(mh: MediaHandler; const (*var*) e: EventRecord; var handledIt: boolean): ComponentResult; external name '_MediaMCIsPlayerEvent';
{
 *  MediaGetMediaLoadState()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.1 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.1 and later
 }
function MediaGetMediaLoadState(mh: MediaHandler; var mediaLoadState: SInt32): ComponentResult; external name '_MediaGetMediaLoadState';
{
 *  MediaVideoOutputChanged()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 5.0 and later
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 5.0 and later
 }
function MediaVideoOutputChanged(mh: MediaHandler; vout: ComponentInstance): ComponentResult; external name '_MediaVideoOutputChanged';
{
 *  MediaEmptySampleCache()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 5.0 and later
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 5.0 and later
 }
function MediaEmptySampleCache(mh: MediaHandler; sampleNum: SInt32; sampleCount: SInt32): ComponentResult; external name '_MediaEmptySampleCache';
{
 *  MediaGetPublicInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 5.0 and later
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 5.0 and later
 }
function MediaGetPublicInfo(mh: MediaHandler; infoSelector: OSType; infoDataPtr: UnivPtr; var ioDataSize: Size): ComponentResult; external name '_MediaGetPublicInfo';
{
 *  MediaSetPublicInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 5.0 and later
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 5.0 and later
 }
function MediaSetPublicInfo(mh: MediaHandler; infoSelector: OSType; infoDataPtr: UnivPtr; dataSize: Size): ComponentResult; external name '_MediaSetPublicInfo';
{
 *  MediaGetUserPreferredCodecs()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 5.0 and later
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 5.0 and later
 }
function MediaGetUserPreferredCodecs(mh: MediaHandler; var userPreferredCodecs: CodecComponentHandle): ComponentResult; external name '_MediaGetUserPreferredCodecs';
{
 *  MediaSetUserPreferredCodecs()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 5.0 and later
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 5.0 and later
 }
function MediaSetUserPreferredCodecs(mh: MediaHandler; userPreferredCodecs: CodecComponentHandle): ComponentResult; external name '_MediaSetUserPreferredCodecs';
{  Keyboard Focus Support }

{
 *  MediaRefConSetProperty()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 6.0 and later
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.2 and later
 *    Windows:          in qtmlClient.lib 6.0 and later
 }
function MediaRefConSetProperty(mh: MediaHandler; refCon: SInt32; propertyType: SInt32; propertyValue: UnivPtr): ComponentResult; external name '_MediaRefConSetProperty';
{
 *  MediaRefConGetProperty()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 6.0 and later
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.2 and later
 *    Windows:          in qtmlClient.lib 6.0 and later
 }
function MediaRefConGetProperty(mh: MediaHandler; refCon: SInt32; propertyType: SInt32; propertyValue: UnivPtr): ComponentResult; external name '_MediaRefConGetProperty';
{
 *  MediaNavigateTargetRefCon()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 6.0 and later
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.2 and later
 *    Windows:          in qtmlClient.lib 6.0 and later
 }
function MediaNavigateTargetRefCon(mh: MediaHandler; navigation: SInt32; var refCon: SInt32): ComponentResult; external name '_MediaNavigateTargetRefCon';
{
 *  MediaGGetIdleManager()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 6.0 and later
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.2 and later
 *    Windows:          in qtmlClient.lib 6.0 and later
 }
function MediaGGetIdleManager(mh: MediaHandler; var pim: IdleManager): ComponentResult; external name '_MediaGGetIdleManager';
{
 *  MediaGSetIdleManager()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 6.0 and later
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.2 and later
 *    Windows:          in qtmlClient.lib 6.0 and later
 }
function MediaGSetIdleManager(mh: MediaHandler; im: IdleManager): ComponentResult; external name '_MediaGSetIdleManager';
const
	uppPrePrerollCompleteProcInfo = $00000EC0;
{$ifc CALL_NOT_IN_CARBON}
	{
	 *  NewPrePrerollCompleteUPP()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   available as macro/inline
	 *    CarbonLib:        not available
	 *    Mac OS X:         not available
	 	}
function NewPrePrerollCompleteUPP(userRoutine: PrePrerollCompleteProcPtr): PrePrerollCompleteUPP; external name '_NewPrePrerollCompleteUPP'; { old name was NewPrePrerollCompleteProc }
{
 *  DisposePrePrerollCompleteUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure DisposePrePrerollCompleteUPP(userUPP: PrePrerollCompleteUPP); external name '_DisposePrePrerollCompleteUPP';
{
 *  InvokePrePrerollCompleteUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure InvokePrePrerollCompleteUPP(mh: MediaHandler; err: OSErr; refcon: UnivPtr; userRoutine: PrePrerollCompleteUPP); external name '_InvokePrePrerollCompleteUPP'; { old name was CallPrePrerollCompleteProc }
{$endc}  {CALL_NOT_IN_CARBON}

{$ALIGN MAC68K}


end.
