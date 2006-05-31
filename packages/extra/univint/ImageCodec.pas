{
     File:       ImageCodec.p
 
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

unit ImageCodec;
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
uses MacTypes,Events,QDOffscreen,OSUtils,Dialogs,Quickdraw,Components,GXTypes,ImageCompression,Movies;


{$ALIGN MAC68K}


{  codec capabilities flags    }

const
	codecCanScale				= $00000001;
	codecCanMask				= $00000002;
	codecCanMatte				= $00000004;
	codecCanTransform			= $00000008;
	codecCanTransferMode		= $00000010;
	codecCanCopyPrev			= $00000020;
	codecCanSpool				= $00000040;
	codecCanClipVertical		= $00000080;
	codecCanClipRectangular		= $00000100;
	codecCanRemapColor			= $00000200;
	codecCanFastDither			= $00000400;
	codecCanSrcExtract			= $00000800;
	codecCanCopyPrevComp		= $00001000;
	codecCanAsync				= $00002000;
	codecCanMakeMask			= $00004000;
	codecCanShift				= $00008000;
	codecCanAsyncWhen			= $00010000;
	codecCanShieldCursor		= $00020000;
	codecCanManagePrevBuffer	= $00040000;
	codecHasVolatileBuffer		= $00080000;					{  codec requires redraw after window movement  }
	codecWantsRegionMask		= $00100000;
	codecImageBufferIsOnScreen	= $00200000;					{  old def of codec using overlay surface, = ( codecIsDirectToScreenOnly | codecUsesOverlaySurface | codecImageBufferIsOverlaySurface | codecSrcMustBeImageBuffer )  }
	codecWantsDestinationPixels	= $00400000;
	codecWantsSpecialScaling	= $00800000;
	codecHandlesInputs			= $01000000;
	codecCanDoIndirectSurface	= $02000000;					{  codec can handle indirect surface (GDI)  }
	codecIsSequenceSensitive	= $04000000;
	codecRequiresOffscreen		= $08000000;
	codecRequiresMaskBits		= $10000000;
	codecCanRemapResolution		= $20000000;
	codecIsDirectToScreenOnly	= $40000000;					{  codec can only decompress data to the screen  }
	codecCanLockSurface			= $80000000;					{  codec can lock destination surface, icm doesn't lock for you  }

	{	  codec capabilities flags2   	}
	codecUsesOverlaySurface		= $00000001;					{  codec uses overlay surface  }
	codecImageBufferIsOverlaySurface = $00000002;				{  codec image buffer is overlay surface, the bits in the buffer are on the screen  }
	codecSrcMustBeImageBuffer	= $00000004;					{  codec can only source data from an image buffer  }
	codecImageBufferIsInAGPMemory = $00000010;					{  codec image buffer is in AGP space, byte writes are OK  }
	codecImageBufferIsInPCIMemory = $00000020;					{  codec image buffer is across a PCI bus; byte writes are bad  }
	codecImageBufferMemoryFlagsValid = $00000040;				{  set by ImageCodecNewImageBufferMemory/NewImageGWorld to indicate that it set the AGP/PCI flags (supported in QuickTime 6.0 and later)  }
	codecDrawsHigherQualityScaled = $00000080;					{  codec will draw higher-quality image if it performs scaling (eg, wipe effect with border)  }


type
	CodecCapabilitiesPtr = ^CodecCapabilities;
	CodecCapabilities = record
		flags:					SInt32;
		wantedPixelSize:		SInt16;
		extendWidth:			SInt16;
		extendHeight:			SInt16;
		bandMin:				SInt16;
		bandInc:				SInt16;
		pad:					SInt16;
		time:					UInt32;
		flags2:					SInt32;								{  field new in QuickTime 4.0  }
	end;

	{	  codec condition flags   	}

const
	codecConditionFirstBand		= $00000001;
	codecConditionLastBand		= $00000002;
	codecConditionFirstFrame	= $00000004;
	codecConditionNewDepth		= $00000008;
	codecConditionNewTransform	= $00000010;
	codecConditionNewSrcRect	= $00000020;
	codecConditionNewMask		= $00000040;
	codecConditionNewMatte		= $00000080;
	codecConditionNewTransferMode = $00000100;
	codecConditionNewClut		= $00000200;
	codecConditionNewAccuracy	= $00000400;
	codecConditionNewDestination = $00000800;
	codecConditionFirstScreen	= $00001000;
	codecConditionDoCursor		= $00002000;
	codecConditionCatchUpDiff	= $00004000;
	codecConditionMaskMayBeChanged = $00008000;
	codecConditionToBuffer		= $00010000;
	codecConditionCodecChangedMask = $80000000;


	codecInfoResourceType		= $63646369 (* 'cdci' *);						{  codec info resource type  }
	codecInterfaceVersion		= 2;							{  high word returned in component GetVersion  }


type
	CDSequenceDataSourceQueueEntryPtr = ^CDSequenceDataSourceQueueEntry;
	CDSequenceDataSourceQueueEntry = record
		nextBusy:				Ptr;
		descSeed:				SInt32;
		dataDesc:				Handle;
		data:					Ptr;
		dataSize:				SInt32;
		useCount:				SInt32;
		frameTime:				TimeValue;
		frameDuration:			TimeValue;
		timeScale:				TimeValue;
	end;

	CDSequenceDataSourcePtr = ^CDSequenceDataSource;
	CDSequenceDataSource = record
		recordSize:				SInt32;
		next:					Ptr;
		seqID:					ImageSequence;
		sourceID:				ImageSequenceDataSource;
		sourceType:				OSType;
		sourceInputNumber:		SInt32;
		dataPtr:				Ptr;
		dataDescription:		Handle;
		changeSeed:				SInt32;
		transferProc:			ICMConvertDataFormatUPP;
		transferRefcon:			Ptr;
		dataSize:				SInt32;
																		{  fields available in QT 3 and later  }
		dataQueue:				QHdrPtr;								{  queue of CDSequenceDataSourceQueueEntry structures }
		originalDataPtr:		Ptr;
		originalDataSize:		SInt32;
		originalDataDescription: Handle;
		originalDataDescriptionSeed: SInt32;
	end;

	ICMFrameTimeInfoPtr = ^ICMFrameTimeInfo;
	ICMFrameTimeInfo = record
		startTime:				wide;
		scale:					SInt32;
		duration:				SInt32;
	end;

	CodecCompressParamsPtr = ^CodecCompressParams;
	CodecCompressParams = record
		sequenceID:				ImageSequence;							{  precompress,bandcompress  }
		imageDescription:		ImageDescriptionHandle;					{  precompress,bandcompress  }
		data:					Ptr;
		bufferSize:				SInt32;
		frameNumber:			SInt32;
		startLine:				SInt32;
		stopLine:				SInt32;
		conditionFlags:			SInt32;
		callerFlags:			CodecFlags;
		capabilities:			CodecCapabilitiesPtr;					{  precompress,bandcompress  }
		progressProcRecord:		ICMProgressProcRecord;
		completionProcRecord:	ICMCompletionProcRecord;
		flushProcRecord:		ICMFlushProcRecord;
		srcPixMap:				PixMap;									{  precompress,bandcompress  }
		prevPixMap:				PixMap;
		spatialQuality:			CodecQ;
		temporalQuality:		CodecQ;
		similarity:				Fixed;
		dataRateParams:			DataRateParamsPtr;
		reserved:				SInt32;
																		{  The following fields only exist for QuickTime 2.1 and greater  }
		majorSourceChangeSeed:	UInt16;
		minorSourceChangeSeed:	UInt16;
		sourceData:				CDSequenceDataSourcePtr;
																		{  The following fields only exist for QuickTime 2.5 and greater  }
		preferredPacketSizeInBytes: SInt32;
																		{  The following fields only exist for QuickTime 3.0 and greater  }
		requestedBufferWidth:	SInt32;								{  must set codecWantsSpecialScaling to indicate this field is valid }
		requestedBufferHeight:	SInt32;								{  must set codecWantsSpecialScaling to indicate this field is valid }
																		{  The following fields only exist for QuickTime 4.0 and greater  }
		wantedSourcePixelType:	OSType;
																		{  The following fields only exist for QuickTime 5.0 and greater  }
		compressedDataSize:		SInt32;								{  if nonzero, this overrides (*imageDescription)->dataSize }
		taskWeight:				UInt32;									{  preferred weight for MP tasks implementing this operation }
		taskName:				OSType;									{  preferred name (type) for MP tasks implementing this operation }
	end;

	CodecDecompressParamsPtr = ^CodecDecompressParams;
	CodecDecompressParams = record
		sequenceID:				ImageSequence;							{  predecompress,banddecompress  }
		imageDescription:		ImageDescriptionHandle;					{  predecompress,banddecompress  }
		data:					Ptr;
		bufferSize:				SInt32;
		frameNumber:			SInt32;
		startLine:				SInt32;
		stopLine:				SInt32;
		conditionFlags:			SInt32;
		callerFlags:			CodecFlags;
		capabilities:			CodecCapabilitiesPtr;					{  predecompress,banddecompress  }
		progressProcRecord:		ICMProgressProcRecord;
		completionProcRecord:	ICMCompletionProcRecord;
		dataProcRecord:			ICMDataProcRecord;
		port:					CGrafPtr;								{  predecompress,banddecompress  }
		dstPixMap:				PixMap;									{  predecompress,banddecompress  }
		maskBits:				BitMapPtr;
		mattePixMap:			PixMapPtr;
		srcRect:				Rect;									{  predecompress,banddecompress  }
		matrix:					MatrixRecordPtr;						{  predecompress,banddecompress  }
		accuracy:				CodecQ;									{  predecompress,banddecompress  }
		transferMode:			SInt16;								{  predecompress,banddecompress  }
		frameTime:				ICMFrameTimePtr;						{  banddecompress  }
		reserved:				array [0..0] of SInt32;
																		{  The following fields only exist for QuickTime 2.0 and greater  }
		matrixFlags:			SInt8;									{  high bit set if 2x resize  }
		matrixType:				SInt8;
		dstRect:				Rect;									{  only valid for simple transforms  }
																		{  The following fields only exist for QuickTime 2.1 and greater  }
		majorSourceChangeSeed:	UInt16;
		minorSourceChangeSeed:	UInt16;
		sourceData:				CDSequenceDataSourcePtr;
		maskRegion:				RgnHandle;
																		{  The following fields only exist for QuickTime 2.5 and greater  }
		wantedDestinationPixelTypes: ^OSTypePtr;						{  Handle to 0-terminated list of OSTypes  }
		screenFloodMethod:		SInt32;
		screenFloodValue:		SInt32;
		preferredOffscreenPixelSize: SInt16;
																		{  The following fields only exist for QuickTime 3.0 and greater  }
		syncFrameTime:			ICMFrameTimeInfoPtr;					{  banddecompress  }
		needUpdateOnTimeChange:	boolean;								{  banddecompress  }
		enableBlackLining:		boolean;
		needUpdateOnSourceChange: boolean;								{  band decompress  }
		pad:					boolean;
		unused:					SInt32;
		finalDestinationPort:	CGrafPtr;
		requestedBufferWidth:	SInt32;								{  must set codecWantsSpecialScaling to indicate this field is valid }
		requestedBufferHeight:	SInt32;								{  must set codecWantsSpecialScaling to indicate this field is valid }
																		{  The following fields only exist for QuickTime 4.0 and greater  }
		displayableAreaOfRequestedBuffer: Rect;							{  set in predecompress }
		requestedSingleField:	boolean;
		needUpdateOnNextIdle:	boolean;
		pad2:					array [0..1] of boolean;
		bufferGammaLevel:		Fixed;
																		{  The following fields only exist for QuickTime 5.0 and greater  }
		taskWeight:				UInt32;									{  preferred weight for MP tasks implementing this operation }
		taskName:				OSType;									{  preferred name (type) for MP tasks implementing this operation }
																		{  The following fields only exist for QuickTime 6.0 and greater  }
		bidirectionalPredictionMode: boolean;
		destinationBufferMemoryPreference: SInt8;						{  a codec's PreDecompress/Preflight call can set this to express a preference about what kind of memory its destination buffer should go into.  no guarantees. }
		codecBufferMemoryPreference: SInt8;								{  may indicate preferred kind of memory that NewImageGWorld/NewImageBufferMemory should create its buffer in, if applicable. }
		pad4:					SInt8;
		mediaContextID:			QTMediaContextID;
	end;


const
	matrixFlagScale2x			= $00000080;
	matrixFlagScale1x			= $00000040;
	matrixFlagScaleHalf			= $00000020;

	kScreenFloodMethodNone		= 0;
	kScreenFloodMethodKeyColor	= 1;
	kScreenFloodMethodAlpha		= 2;

	kFlushLastQueuedFrame		= 0;
	kFlushFirstQueuedFrame		= 1;

	kNewImageGWorldErase		= $00000001;

	{	 values for destinationBufferMemoryPreference and codecBufferMemoryPreference 	}
	kICMImageBufferNoPreference	= 0;
	kICMImageBufferPreferMainMemory = 1;
	kICMImageBufferPreferVideoMemory = 2;


type
{$ifc TYPED_FUNCTION_POINTERS}
	ImageCodecTimeTriggerProcPtr = procedure(refcon: UnivPtr);
{$elsec}
	ImageCodecTimeTriggerProcPtr = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	ImageCodecDrawBandCompleteProcPtr = procedure(refcon: UnivPtr; drawBandResult: ComponentResult; drawBandCompleteFlags: UInt32);
{$elsec}
	ImageCodecDrawBandCompleteProcPtr = ProcPtr;
{$endc}

{$ifc OPAQUE_UPP_TYPES}
	ImageCodecTimeTriggerUPP = ^SInt32; { an opaque UPP }
{$elsec}
	ImageCodecTimeTriggerUPP = UniversalProcPtr;
{$endc}	
{$ifc OPAQUE_UPP_TYPES}
	ImageCodecDrawBandCompleteUPP = ^SInt32; { an opaque UPP }
{$elsec}
	ImageCodecDrawBandCompleteUPP = UniversalProcPtr;
{$endc}	

const
	uppImageCodecTimeTriggerProcInfo = $000000C0;
	uppImageCodecDrawBandCompleteProcInfo = $00000FC0;
	{
	 *  NewImageCodecTimeTriggerUPP()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   available as macro/inline
	 *    CarbonLib:        in CarbonLib 1.0.2 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
function NewImageCodecTimeTriggerUPP(userRoutine: ImageCodecTimeTriggerProcPtr): ImageCodecTimeTriggerUPP; external name '_NewImageCodecTimeTriggerUPP'; { old name was NewImageCodecTimeTriggerProc }
{
 *  NewImageCodecDrawBandCompleteUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewImageCodecDrawBandCompleteUPP(userRoutine: ImageCodecDrawBandCompleteProcPtr): ImageCodecDrawBandCompleteUPP; external name '_NewImageCodecDrawBandCompleteUPP'; { old name was NewImageCodecDrawBandCompleteProc }
{
 *  DisposeImageCodecTimeTriggerUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeImageCodecTimeTriggerUPP(userUPP: ImageCodecTimeTriggerUPP); external name '_DisposeImageCodecTimeTriggerUPP';
{
 *  DisposeImageCodecDrawBandCompleteUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeImageCodecDrawBandCompleteUPP(userUPP: ImageCodecDrawBandCompleteUPP); external name '_DisposeImageCodecDrawBandCompleteUPP';
{
 *  InvokeImageCodecTimeTriggerUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure InvokeImageCodecTimeTriggerUPP(refcon: UnivPtr; userRoutine: ImageCodecTimeTriggerUPP); external name '_InvokeImageCodecTimeTriggerUPP'; { old name was CallImageCodecTimeTriggerProc }
{
 *  InvokeImageCodecDrawBandCompleteUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure InvokeImageCodecDrawBandCompleteUPP(refcon: UnivPtr; drawBandResult: ComponentResult; drawBandCompleteFlags: UInt32; userRoutine: ImageCodecDrawBandCompleteUPP); external name '_InvokeImageCodecDrawBandCompleteUPP'; { old name was CallImageCodecDrawBandCompleteProc }
type
	ImageSubCodecDecompressCapabilitiesPtr = ^ImageSubCodecDecompressCapabilities;
	ImageSubCodecDecompressCapabilities = record
		recordSize:				SInt32;								{  sizeof(ImageSubCodecDecompressCapabilities) }
		decompressRecordSize:	SInt32;								{  size of your codec's decompress record }
		canAsync:				boolean;								{  default true }
		pad0:					SInt8;
																		{  The following field only exists for QuickTime 4.1 and greater  }
		suggestedQueueSize:		UInt16;
																		{  The following field only exists for QuickTime 4.0 and greater  }
		canProvideTrigger:		boolean;
																		{  The following fields only exist for QuickTime 5.0 and greater  }
		subCodecFlushesScreen:	boolean;								{  only used on Mac OS X }
		subCodecCallsDrawBandComplete: boolean;
		pad2:					SInt8;
																		{  The following fields only exist for QuickTime 5.0.1 and greater  }
		isChildCodec:			boolean;								{  set by base codec before calling Initialize }
		pad3,pad4,pad5:			SInt8;
	end;


const
	kCodecFrameTypeUnknown		= 0;
	kCodecFrameTypeKey			= 1;
	kCodecFrameTypeDifference	= 2;
	kCodecFrameTypeDroppableDifference = 3;


type
	ImageSubCodecDecompressRecordPtr = ^ImageSubCodecDecompressRecord;
	ImageSubCodecDecompressRecord = record
		baseAddr:				Ptr;
		rowBytes:				SInt32;
		codecData:				Ptr;
		progressProcRecord:		ICMProgressProcRecord;
		dataProcRecord:			ICMDataProcRecord;
		userDecompressRecord:	Ptr;									{  pointer to codec-specific per-band data }
		frameType:				SInt8;
		inhibitMP:				boolean;								{  set this in BeginBand to tell the base decompressor not to call DrawBand from an MP task for this frame.  (Only has any effect for MP-capable subcodecs.  New in QuickTime 5.0.) }
		pad:					packed array [0..1] of UInt8;
		priv:					array [0..1] of SInt32;
																		{  The following fields only exist for QuickTime 5.0 and greater  }
		drawBandCompleteUPP:	ImageCodecDrawBandCompleteUPP;			{  only used if subcodec set subCodecCallsDrawBandComplete; if drawBandCompleteUPP is non-nil, codec must call it when a frame is finished, but may return from DrawBand before the frame is finished.  }
		drawBandCompleteRefCon:	Ptr;									{  Note: do not call drawBandCompleteUPP directly from a hardware interrupt; instead, use DTInstall to run a function at deferred task time, and call drawBandCompleteUPP from that.  }
	end;

	{
	  These data structures are used by code that wants to pass planar pixmap 
	   information around.
	  The structure below gives the basic idea of what is being done.
	  Normal instances of code will use a fixed number of planes (eg YUV420 uses 
	   three planes, Y, U and V). Each such code instance will define its own
	   version of the PlanarPixMapInfo struct counting the number of planes it 
	   needs along with defining constants that specify the meanings of each
	   plane.
	}
	PlanarComponentInfoPtr = ^PlanarComponentInfo;
	PlanarComponentInfo = record
		offset:					SInt32;
		rowBytes:				UInt32;
	end;

	PlanarPixMapInfoPtr = ^PlanarPixMapInfo;
	PlanarPixMapInfo = record
		componentInfo:			array [0..0] of PlanarComponentInfo;
	end;

	PlanarPixmapInfoSorensonYUV9Ptr = ^PlanarPixmapInfoSorensonYUV9;
	PlanarPixmapInfoSorensonYUV9 = record
		componentInfoY:			PlanarComponentInfo;
		componentInfoU:			PlanarComponentInfo;
		componentInfoV:			PlanarComponentInfo;
	end;

	PlanarPixmapInfoYUV420Ptr = ^PlanarPixmapInfoYUV420;
	PlanarPixmapInfoYUV420 = record
		componentInfoY:			PlanarComponentInfo;
		componentInfoCb:		PlanarComponentInfo;
		componentInfoCr:		PlanarComponentInfo;
	end;


const
	codecSuggestedBufferSentinel = $73656E74 (* 'sent' *);						{  codec public resource containing suggested data pattern to put past end of data buffer  }


	{  name of parameters or effect -- placed in root container, required  }
	kParameterTitleName			= $6E616D65 (* 'name' *);
	kParameterTitleID			= 1;

	{  codec sub-type of parameters or effect -- placed in root container, required  }
	kParameterWhatName			= $77686174 (* 'what' *);
	kParameterWhatID			= 1;

	{  effect version -- placed in root container, optional, but recommended  }
	kParameterVersionName		= $76657273 (* 'vers' *);
	kParameterVersionID			= 1;

	{  is effect repeatable -- placed in root container, optional, default is TRUE }
	kParameterRepeatableName	= $70657465 (* 'pete' *);
	kParameterRepeatableID		= 1;

	kParameterRepeatableTrue	= 1;
	kParameterRepeatableFalse	= 0;

	{  substitution codec in case effect is missing -- placed in root container, recommended  }
	kParameterAlternateCodecName = $73756273 (* 'subs' *);
	kParameterAlternateCodecID	= 1;

	{  maximum number of sources -- placed in root container, required  }
	kParameterSourceCountName	= $73726373 (* 'srcs' *);
	kParameterSourceCountID		= 1;

	{  EFFECT CLASSES }

	{
	   The effect major class defines the major grouping of the effect.
	   Major classes are defined only by Apple and are not extendable by third
	   parties.  Major classes are used for filtering of the effect list by
	   applications, but do not define what UI sub-group may or may not be
	   presented to the user.  For example, the major class may be a transition,
	   but the minor class may be a wipe.  
	}

	{
	   Effects that fail to include a
	   kEffectMajorClassType will be classified as kMiscMajorClass.
	}
	kEffectMajorClassType		= $636C7361 (* 'clsa' *);
	kEffectMajorClassID			= 1;

	kGeneratorMajorClass		= $67656E72 (* 'genr' *);						{  zero source effects }
	kFilterMajorClass			= $66696C74 (* 'filt' *);						{  one source effects }
	kTransitionMajorClass		= $7472616E (* 'tran' *);						{  multisource morph effects  }
	kCompositorMajorClass		= $636F6D70 (* 'comp' *);						{  multisource layer effects }
	kMiscMajorClass				= $6D697363 (* 'misc' *);						{  all other effects }

	{
	   The effect minor class defines the grouping of effects for the purposes
	   of UI.  Apple defines a set of minor classes and will extend it over
	   time.  Apple also provides strings within the UI for minor classes
	   that it defines.  Third party developers may either classify
	   their effects as a type defined by Apple, or may define their own
	   minor class.  Effects which define a minor class of their own
	   must also then supply a kEffectMinorClassNameType atom.
	}

	{
	   If a kEffectMinorClassNameType atom is present, but
	   the minor type is one defined by Apple, the Apple supplied
	   string will be used in the UI.
	}

	{
	   Effects that fail to supply a kEffectMinorClassType will be 
	   classified as kMiscMinorClass.
	}
	kEffectMinorClassType		= $636C7369 (* 'clsi' *);
	kEffectMinorClassID			= 1;
	kEffectMinorClassNameType	= $636C736E (* 'clsn' *);
	kEffectMinorClassNameID		= 1;

	kGeneratorMinorClass		= $67656E72 (* 'genr' *);						{  "Generators" }
	kRenderMinorClass			= $72656E64 (* 'rend' *);						{  "Render" }
	kFilterMinorClass			= $66696C74 (* 'filt' *);						{  "Filters" }
	kArtisticMinorClass			= $61727473 (* 'arts' *);						{  "Artistic }
	kBlurMinorClass				= $626C7572 (* 'blur' *);						{  "Blur" }
	kSharpenMinorClass			= $73687270 (* 'shrp' *);						{  "Sharpen" }
	kDistortMinorClass			= $64697374 (* 'dist' *);						{  "Distort" }
	kNoiseMinorClass			= $6E6F6973 (* 'nois' *);						{  "Noise" }
	kAdjustmentMinorClass		= $61647374 (* 'adst' *);						{  "Adjustments" }
	kTransitionMinorClass		= $7472616E (* 'tran' *);						{  "Transitions" }
	kWipeMinorClass				= $77697065 (* 'wipe' *);						{  "Wipes" }
	k3DMinorClass				= $707A7265 (* 'pzre' *);						{  "3D Transitions" }
	kCompositorMinorClass		= $636F6D70 (* 'comp' *);						{  "Compositors" }
	kEffectsMinorClass			= $66786678 (* 'fxfx' *);						{  "Special Effects" }
	kMiscMinorClass				= $6D697363 (* 'misc' *);						{  "Miscellaneous" }

	{
	   Effects can define a number of "preset" values which will be presented to the user
	   in a simplified UI.  Each preset is an atom within the parameter description list
	   and must have an atom ID from 1 going up sequentially.  Inside of this atom are three other
	   atoms containing:
	    1) the name of the preset as a Pascal string
	    2) a preview picture for the preset, 86 x 64 pixels in size
	    3) the ENTIRE set of parameter values needed to create a sample of this preset.
	}
	kEffectPresetType			= $70656666 (* 'peff' *);
	kPresetNameType				= $706E616D (* 'pnam' *);
	kPresetNameID				= 1;
	kPresetPreviewPictureType	= $70706374 (* 'ppct' *);
	kPresetPreviewPictureID		= 1;
	kPresetSettingsType			= $70737374 (* 'psst' *);
	kPresetSettingsID			= 1;

	kParameterDependencyName	= $64656570 (* 'deep' *);
	kParameterDependencyID		= 1;

	kParameterListDependsUponColorProfiles = $70726F66 (* 'prof' *);
	kParameterListDependsUponFonts = $666F6E74 (* 'font' *);


type
	ParameterDependancyRecordPtr = ^ParameterDependancyRecord;
	ParameterDependancyRecord = record
		dependCount:			SInt32;
		depends:				array [0..0] of OSType;
	end;

	{
	   enumeration list in container -- placed in root container, optional unless used by a
	   parameter in the list
	}

const
	kParameterEnumList			= $656E756D (* 'enum' *);


type
	EnumValuePairPtr = ^EnumValuePair;
	EnumValuePair = record
		value:					SInt32;
		name:					Str255;
	end;

	EnumListRecordPtr = ^EnumListRecord;
	EnumListRecord = record
		enumCount:				SInt32;								{  number of enumeration items to follow }
		values:					array [0..0] of EnumValuePair;			{  values and names for them, packed  }
	end;

	{  atom type of parameter }

const
	kParameterAtomTypeAndID		= $74797065 (* 'type' *);

	kNoAtom						= $6E6F6E65 (* 'none' *);						{  atom type for no data got/set }
	kAtomNoFlags				= $00000000;
	kAtomNotInterpolated		= $00000001;					{  atom can never be interpolated }
	kAtomInterpolateIsOptional	= $00000002;					{  atom can be interpolated, but it is an advanced user operation }
	kAtomMayBeIndexed			= $00000004;					{  more than one value of atom can exist with accending IDs (ie, lists of colors) }


type
	ParameterAtomTypeAndIDPtr = ^ParameterAtomTypeAndID;
	ParameterAtomTypeAndID = record
		atomType:				QTAtomType;								{  type of atom this data comes from/goes into }
		atomID:					QTAtomID;								{  ID of atom this data comes from/goes into }
		atomFlags:				SInt32;								{  options for this atom }
		atomName:				Str255;									{  name of this value type }
	end;

	{  data type of a parameter }

const
	kParameterDataType			= $64617461 (* 'data' *);

	kParameterTypeDataLong		= 2;							{  SInt16 value }
	kParameterTypeDataFixed		= 3;							{  fixed point value }
	kParameterTypeDataRGBValue	= 8;							{  RGBColor data }
	kParameterTypeDataDouble	= 11;							{  IEEE 64 bit floating point value }
	kParameterTypeDataText		= $74657874 (* 'text' *);						{  editable text item }
	kParameterTypeDataEnum		= $656E756D (* 'enum' *);						{  enumerated lookup value }
	kParameterTypeDataBitField	= $626F6F6C (* 'bool' *);						{  bit field value (something that holds boolean(s)) }
	kParameterTypeDataImage		= $696D6167 (* 'imag' *);						{  reference to an image via Picture data }


type
	ParameterDataTypePtr = ^ParameterDataType;
	ParameterDataType = record
		dataType:				OSType;									{  type of data this item is stored as }
	end;

	{
	   alternate (optional) data type -- main data type always required.  
	   Must be modified or deleted when modifying main data type.
	   Main data type must be modified when alternate is modified.
	}

const
	kParameterAlternateDataType	= $616C7431 (* 'alt1' *);
	kParameterTypeDataColorValue = $636D6C72 (* 'cmlr' *);						{  CMColor data (supported on machines with ColorSync) }
	kParameterTypeDataCubic		= $63756269 (* 'cubi' *);						{  cubic bezier(s) (no built-in support) }
	kParameterTypeDataNURB		= $6E757262 (* 'nurb' *);						{  nurb(s) (no built-in support) }


type
	ParameterAlternateDataEntryPtr = ^ParameterAlternateDataEntry;
	ParameterAlternateDataEntry = record
		dataType:				OSType;									{  type of data this item is stored as }
		alternateAtom:			QTAtomType;								{  where to store }
	end;

	ParameterAlternateDataTypePtr = ^ParameterAlternateDataType;
	ParameterAlternateDataType = record
		numEntries:				SInt32;
		entries:				array [0..0] of ParameterAlternateDataEntry;
	end;

	{  legal values for the parameter }

const
	kParameterDataRange			= $72616E67 (* 'rang' *);

	kNoMinimumLongFixed			= $7FFFFFFF;					{  ignore minimum/maxiumum values }
	kNoMaximumLongFixed			= $80000000;
	kNoScaleLongFixed			= 0;							{  don't perform any scaling of value }
	kNoPrecision				= -1;							{  allow as many digits as format }

	{  'text' }

type
	StringRangeRecordPtr = ^StringRangeRecord;
	StringRangeRecord = record
		maxChars:				SInt32;								{  maximum length of string }
		maxLines:				SInt32;								{  number of editing lines to use (1 typical, 0 to default) }
	end;

	{  'long' }
	LongRangeRecordPtr = ^LongRangeRecord;
	LongRangeRecord = record
		minValue:				SInt32;								{  no less than this }
		maxValue:				SInt32;								{  no more than this }
		scaleValue:				SInt32;								{  muliply content by this going in, divide going out }
		precisionDigits:		SInt32;								{  # digits of precision when editing via typing }
	end;

	{  'enum' }
	EnumRangeRecordPtr = ^EnumRangeRecord;
	EnumRangeRecord = record
		enumID:					SInt32;								{  'enum' list in root container to search within }
	end;

	{  'fixd' }
	FixedRangeRecordPtr = ^FixedRangeRecord;
	FixedRangeRecord = record
		minValue:				Fixed;									{  no less than this }
		maxValue:				Fixed;									{  no more than this }
		scaleValue:				Fixed;									{  muliply content by this going in, divide going out }
		precisionDigits:		SInt32;								{  # digits of precision when editing via typing }
	end;

	{  'doub' }
	{  'bool'    }
	BooleanRangeRecordPtr = ^BooleanRangeRecord;
	BooleanRangeRecord = record
		maskValue:				SInt32;								{  value to mask on/off to set/clear the boolean }
	end;

	{  'rgb ' }
	RGBRangeRecordPtr = ^RGBRangeRecord;
	RGBRangeRecord = record
		minColor:				RGBColor;
		maxColor:				RGBColor;
	end;

	{  'imag' }

const
	kParameterImageNoFlags		= 0;
	kParameterImageIsPreset		= 1;

	kStandardPresetGroup		= $70736574 (* 'pset' *);


type
	ImageRangeRecordPtr = ^ImageRangeRecord;
	ImageRangeRecord = record
		imageFlags:				SInt32;
		fileType:				OSType;									{  file type to contain the preset group (normally kStandardPresetGroup) }
		replacedAtoms:			SInt32;								{  # atoms at this level replaced by this preset group }
	end;

	{  union of all of the above }
	{  UI behavior of a parameter }

const
	kParameterDataBehavior		= $6469746C (* 'ditl' *);

																{  items edited via typing }
	kParameterItemEditText		= $65646974 (* 'edit' *);						{  edit text box }
	kParameterItemEditLong		= $6C6F6E67 (* 'long' *);						{  long number editing box }
	kParameterItemEditFixed		= $66697864 (* 'fixd' *);						{  fixed point number editing box }
	kParameterItemEditDouble	= $646F7562 (* 'doub' *);						{  double number editing box }
																{  items edited via control(s) }
	kParameterItemPopUp			= $706F7075 (* 'popu' *);						{  pop up value for enum types }
	kParameterItemRadioCluster	= $72616469 (* 'radi' *);						{  radio cluster for enum types }
	kParameterItemCheckBox		= $63686578 (* 'chex' *);						{  check box for booleans }
	kParameterItemControl		= $636E746C (* 'cntl' *);						{  item controlled via a standard control of some type }
																{  special user items }
	kParameterItemLine			= $6C696E65 (* 'line' *);						{  line }
	kParameterItemColorPicker	= $7069636B (* 'pick' *);						{  color swatch & picker }
	kParameterItemGroupDivider	= $64697669 (* 'divi' *);						{  start of a new group of items }
	kParameterItemStaticText	= $73746174 (* 'stat' *);						{  display "parameter name" as static text }
	kParameterItemDragImage		= $696D6167 (* 'imag' *);						{  allow image display, along with drag and drop }
																{  flags valid for lines and groups }
	kGraphicsNoFlags			= $00000000;					{  no options for graphics }
	kGraphicsFlagsGray			= $00000001;					{  draw lines with gray }
																{  flags valid for groups }
	kGroupNoFlags				= $00000000;					{  no options for group -- may be combined with graphics options              }
	kGroupAlignText				= $00010000;					{  edit text items in group have the same size }
	kGroupSurroundBox			= $00020000;					{  group should be surrounded with a box }
	kGroupMatrix				= $00040000;					{  side-by-side arrangement of group is okay }
	kGroupNoName				= $00080000;					{  name of group should not be displayed above box }
																{  flags valid for popup/radiocluster/checkbox/control }
	kDisableControl				= $00000001;
	kDisableWhenNotEqual		= $00000001;
	kDisableWhenEqual			= $00000011;
	kDisableWhenLessThan		= $00000021;
	kDisableWhenGreaterThan		= $00000031;					{  flags valid for controls }
	kCustomControl				= $00100000;					{  flags valid for popups }
	kPopupStoreAsString			= $00010000;


type
	ControlBehaviorsPtr = ^ControlBehaviors;
	ControlBehaviors = record
		groupID:				QTAtomID;								{  group under control of this item }
		controlValue:			SInt32;								{  control value for comparison purposes }
	end;

	ParameterDataBehaviorPtr = ^ParameterDataBehavior;
	ParameterDataBehavior = record
		behaviorType:			OSType;
		behaviorFlags:			SInt32;
		case SInt16 of
		0: (
			controls:			ControlBehaviors;
			);
	end;

	{  higher level purpose of a parameter or set of parameters }

const
	kParameterDataUsage			= $75736520 (* 'use ' *);

	kParameterUsagePixels		= $7069786C (* 'pixl' *);
	kParameterUsageRectangle	= $72656374 (* 'rect' *);
	kParameterUsagePoint		= $78792020 (* 'xy  ' *);
	kParameterUsage3DPoint		= $78797A20 (* 'xyz ' *);
	kParameterUsageDegrees		= $64656772 (* 'degr' *);
	kParameterUsageRadians		= $72616473 (* 'rads' *);
	kParameterUsagePercent		= $70636E74 (* 'pcnt' *);
	kParameterUsageSeconds		= $73656373 (* 'secs' *);
	kParameterUsageMilliseconds	= $6D736563 (* 'msec' *);
	kParameterUsageMicroseconds	= $C2B57365 (* 'µsec' *);
	kParameterUsage3by3Matrix	= $33627933 (* '3by3' *);
	kParameterUsageCircularDegrees = $64656763 (* 'degc' *);
	kParameterUsageCircularRadians = $72616463 (* 'radc' *);


type
	ParameterDataUsagePtr = ^ParameterDataUsage;
	ParameterDataUsage = record
		usageType:				OSType;									{  higher level purpose of the data or group }
	end;

	{  default value(s) for a parameter }

const
	kParameterDataDefaultItem	= $64666C74 (* 'dflt' *);

	{	 atoms that help to fill in data within the info window 	}
	kParameterInfoLongName		= $C2A96E61 (* '©nam' *);
	kParameterInfoCopyright		= $C2A96370 (* '©cpy' *);
	kParameterInfoDescription	= $C2A9696E (* '©inf' *);
	kParameterInfoWindowTitle	= $C2A9776E (* '©wnt' *);
	kParameterInfoPicture		= $C2A97069 (* '©pix' *);
	kParameterInfoManufacturer	= $C2A96D61 (* '©man' *);
	kParameterInfoIDs			= 1;

	{	 flags for ImageCodecValidateParameters 	}
	kParameterValidationNoFlags	= $00000000;
	kParameterValidationFinalValidation = $00000001;


type
	QTParameterValidationOptions		= SInt32;
	{  QTAtomTypes for atoms in image compressor settings containers }

const
	kImageCodecSettingsFieldCount = $6669656C (* 'fiel' *);						{  Number of fields (UInt8)  }
	kImageCodecSettingsFieldOrdering = $66646F6D (* 'fdom' *);					{  Ordering of fields (UInt8) }
	kImageCodecSettingsFieldOrderingF1F2 = 1;
	kImageCodecSettingsFieldOrderingF2F1 = 2;


	{  Additional Image Description Extensions }
	kColorInfoImageDescriptionExtension = $636F6C72 (* 'colr' *);				{  image description extension describing the color properties     }
	kPixelAspectRatioImageDescriptionExtension = $70617370 (* 'pasp' *);		{  image description extension describing the pixel aspect ratio }
	kCleanApertureImageDescriptionExtension = $636C6170 (* 'clap' *);			{  image description extension describing the pixel aspect ratio }


	{  Color Info Image Description Extension types }
	kVideoColorInfoImageDescriptionExtensionType = $6E636C63 (* 'nclc' *);		{  For video color descriptions (defined below)     }
	kICCProfileColorInfoImageDescriptionExtensionType = $70726F66 (* 'prof' *);	{  For ICC Profile color descriptions (not defined here) }


	{  Video Color Info Image Description Extensions }

type
	NCLCColorInfoImageDescriptionExtensionPtr = ^NCLCColorInfoImageDescriptionExtension;
	NCLCColorInfoImageDescriptionExtension = record
		colorParamType:			OSType;									{  Type of color parameter 'nclc'                }
		primaries:				UInt16;									{  CIE 1931 xy chromaticity coordinates           }
		transferFunction:		UInt16;									{  Nonlinear transfer function from RGB to ErEgEb  }
		matrix:					UInt16;									{  Matrix from ErEgEb to EyEcbEcr            }
	end;

	{  Primaries }

const
	kQTPrimaries_ITU_R709_2		= 1;							{  ITU-R BT.709-2, SMPTE 274M-1995, and SMPTE 296M-1997  }
	kQTPrimaries_Unknown		= 2;							{  Unknown  }
	kQTPrimaries_EBU_3213		= 5;							{  EBU Tech. 3213 (1981)  }
	kQTPrimaries_SMPTE_C		= 6;							{  SMPTE C Primaries from SMPTE RP 145-1993  }

	{  Transfer Function }
	kQTTransferFunction_ITU_R709_2 = 1;							{  Recommendation ITU-R BT.709-2, SMPTE 274M-1995, SMPTE 296M-1997, SMPTE 293M-1996 and SMPTE 170M-1994  }
	kQTTransferFunction_Unknown	= 2;							{  Unknown  }
	kQTTransferFunction_SMPTE_240M_1995 = 7;					{  SMPTE 240M-1995 and interim color implementation of SMPTE 274M-1995  }

	{  Matrix }
	kQTMatrix_ITU_R_709_2		= 1;							{  Recommendation ITU-R BT.709-2 (1125/60/2:1 only), SMPTE 274M-1995 and SMPTE 296M-1997  }
	kQTMatrix_Unknown			= 2;							{  Unknown  }
	kQTMatrix_ITU_R_601_4		= 6;							{  Recommendation ITU-R BT.601-4, Recommendation ITU-R BT.470-4 System B and G, SMPTE 170M-1994 and SMPTE 293M-1996  }
	kQTMatrix_SMPTE_240M_1995	= 7;							{  SMPTE 240M-1995 and interim color implementation of SMPTE 274M-1995  }


	{  Field/Frame Info Image Description (this remaps to FieldInfoImageDescriptionExtension) }

type
	FieldInfoImageDescriptionExtension2Ptr = ^FieldInfoImageDescriptionExtension2;
	FieldInfoImageDescriptionExtension2 = packed record
		fields:					UInt8;
		detail:					UInt8;
	end;


const
	kQTFieldsProgressiveScan	= 1;
	kQTFieldsInterlaced			= 2;

	kQTFieldDetailUnknown		= 0;
	kQTFieldDetailTemporalTopFirst = 1;
	kQTFieldDetailTemporalBottomFirst = 6;
	kQTFieldDetailSpatialFirstLineEarly = 9;
	kQTFieldDetailSpatialFirstLineLate = 14;


	{  Pixel Aspect Ratio Image Description Extensions }

type
	PixelAspectRatioImageDescriptionExtensionPtr = ^PixelAspectRatioImageDescriptionExtension;
	PixelAspectRatioImageDescriptionExtension = record
		hSpacing:				UInt32;									{  Horizontal Spacing  }
		vSpacing:				UInt32;									{  Vertical Spacing  }
	end;

	{  Clean Aperture Image Description Extensions }
	CleanApertureImageDescriptionExtensionPtr = ^CleanApertureImageDescriptionExtension;
	CleanApertureImageDescriptionExtension = record
		cleanApertureWidthN:	UInt32;									{  width of clean aperture, numerator, denominator  }
		cleanApertureWidthD:	UInt32;
		cleanApertureHeightN:	UInt32;									{  height of clean aperture, numerator, denominator }
		cleanApertureHeightD:	UInt32;
		horizOffN:				UInt32;									{  horizontal offset of clean aperture center minus (width-1)/2, numerator, denominator  }
		horizOffD:				UInt32;
		vertOffN:				UInt32;									{  vertical offset of clean aperture center minus (height-1)/2, numerator, denominator  }
		vertOffD:				UInt32;
	end;

{$ifc TYPED_FUNCTION_POINTERS}
	ImageCodecMPDrawBandProcPtr = function(refcon: UnivPtr; var drp: ImageSubCodecDecompressRecord): ComponentResult;
{$elsec}
	ImageCodecMPDrawBandProcPtr = ProcPtr;
{$endc}

{$ifc OPAQUE_UPP_TYPES}
	ImageCodecMPDrawBandUPP = ^SInt32; { an opaque UPP }
{$elsec}
	ImageCodecMPDrawBandUPP = UniversalProcPtr;
{$endc}	

const
	uppImageCodecMPDrawBandProcInfo = $000003F0;
	{
	 *  NewImageCodecMPDrawBandUPP()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   available as macro/inline
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
function NewImageCodecMPDrawBandUPP(userRoutine: ImageCodecMPDrawBandProcPtr): ImageCodecMPDrawBandUPP; external name '_NewImageCodecMPDrawBandUPP'; { old name was NewImageCodecMPDrawBandProc }
{
 *  DisposeImageCodecMPDrawBandUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeImageCodecMPDrawBandUPP(userUPP: ImageCodecMPDrawBandUPP); external name '_DisposeImageCodecMPDrawBandUPP';
{
 *  InvokeImageCodecMPDrawBandUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function InvokeImageCodecMPDrawBandUPP(refcon: UnivPtr; var drp: ImageSubCodecDecompressRecord; userRoutine: ImageCodecMPDrawBandUPP): ComponentResult; external name '_InvokeImageCodecMPDrawBandUPP'; { old name was CallImageCodecMPDrawBandProc }
{  codec selectors 0-127 are reserved by Apple }
{  codec selectors 128-191 are subtype specific }
{  codec selectors 192-255 are vendor specific }
{  codec selectors 256-32767 are available for general use }
{  negative selectors are reserved by the Component Manager }
{
 *  ImageCodecGetCodecInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageCodecGetCodecInfo(ci: ComponentInstance; var info: CodecInfo): ComponentResult; external name '_ImageCodecGetCodecInfo';
{
 *  ImageCodecGetCompressionTime()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageCodecGetCompressionTime(ci: ComponentInstance; src: PixMapHandle; const (*var*) srcRect: Rect; depth: SInt16; var spatialQuality: CodecQ; var temporalQuality: CodecQ; var time: UInt32): ComponentResult; external name '_ImageCodecGetCompressionTime';
{
 *  ImageCodecGetMaxCompressionSize()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageCodecGetMaxCompressionSize(ci: ComponentInstance; src: PixMapHandle; const (*var*) srcRect: Rect; depth: SInt16; quality: CodecQ; var size: SInt32): ComponentResult; external name '_ImageCodecGetMaxCompressionSize';
{
 *  ImageCodecPreCompress()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageCodecPreCompress(ci: ComponentInstance; var params: CodecCompressParams): ComponentResult; external name '_ImageCodecPreCompress';
{
 *  ImageCodecBandCompress()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageCodecBandCompress(ci: ComponentInstance; var params: CodecCompressParams): ComponentResult; external name '_ImageCodecBandCompress';
{
 *  ImageCodecPreDecompress()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageCodecPreDecompress(ci: ComponentInstance; var params: CodecDecompressParams): ComponentResult; external name '_ImageCodecPreDecompress';
{
 *  ImageCodecBandDecompress()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageCodecBandDecompress(ci: ComponentInstance; var params: CodecDecompressParams): ComponentResult; external name '_ImageCodecBandDecompress';
{
 *  ImageCodecBusy()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageCodecBusy(ci: ComponentInstance; seq: ImageSequence): ComponentResult; external name '_ImageCodecBusy';
{
 *  ImageCodecGetCompressedImageSize()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageCodecGetCompressedImageSize(ci: ComponentInstance; desc: ImageDescriptionHandle; data: Ptr; bufferSize: SInt32; dataProc: ICMDataProcRecordPtr; var dataSize: SInt32): ComponentResult; external name '_ImageCodecGetCompressedImageSize';
{
 *  ImageCodecGetSimilarity()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageCodecGetSimilarity(ci: ComponentInstance; src: PixMapHandle; const (*var*) srcRect: Rect; desc: ImageDescriptionHandle; data: Ptr; var similarity: Fixed): ComponentResult; external name '_ImageCodecGetSimilarity';
{
 *  ImageCodecTrimImage()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageCodecTrimImage(ci: ComponentInstance; Desc: ImageDescriptionHandle; inData: Ptr; inBufferSize: SInt32; dataProc: ICMDataProcRecordPtr; outData: Ptr; outBufferSize: SInt32; flushProc: ICMFlushProcRecordPtr; var trimRect: Rect; progressProc: ICMProgressProcRecordPtr): ComponentResult; external name '_ImageCodecTrimImage';
{
 *  ImageCodecRequestSettings()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageCodecRequestSettings(ci: ComponentInstance; settings: Handle; var rp: Rect; filterProc: ModalFilterUPP): ComponentResult; external name '_ImageCodecRequestSettings';
{
 *  ImageCodecGetSettings()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageCodecGetSettings(ci: ComponentInstance; settings: Handle): ComponentResult; external name '_ImageCodecGetSettings';
{
 *  ImageCodecSetSettings()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageCodecSetSettings(ci: ComponentInstance; settings: Handle): ComponentResult; external name '_ImageCodecSetSettings';
{
 *  ImageCodecFlush()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageCodecFlush(ci: ComponentInstance): ComponentResult; external name '_ImageCodecFlush';
{
 *  ImageCodecSetTimeCode()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageCodecSetTimeCode(ci: ComponentInstance; timeCodeFormat: UnivPtr; timeCodeTime: UnivPtr): ComponentResult; external name '_ImageCodecSetTimeCode';
{
 *  ImageCodecIsImageDescriptionEquivalent()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageCodecIsImageDescriptionEquivalent(ci: ComponentInstance; newDesc: ImageDescriptionHandle; var equivalent: boolean): ComponentResult; external name '_ImageCodecIsImageDescriptionEquivalent';
{
 *  ImageCodecNewMemory()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageCodecNewMemory(ci: ComponentInstance; var data: Ptr; dataSize: Size; dataUse: SInt32; memoryGoneProc: ICMMemoryDisposedUPP; refCon: UnivPtr): ComponentResult; external name '_ImageCodecNewMemory';
{
 *  ImageCodecDisposeMemory()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageCodecDisposeMemory(ci: ComponentInstance; data: Ptr): ComponentResult; external name '_ImageCodecDisposeMemory';
{
 *  ImageCodecHitTestData()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageCodecHitTestData(ci: ComponentInstance; desc: ImageDescriptionHandle; data: UnivPtr; dataSize: Size; where: Point; var hit: boolean): ComponentResult; external name '_ImageCodecHitTestData';
{
 *  ImageCodecNewImageBufferMemory()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageCodecNewImageBufferMemory(ci: ComponentInstance; var params: CodecDecompressParams; flags: SInt32; memoryGoneProc: ICMMemoryDisposedUPP; refCon: UnivPtr): ComponentResult; external name '_ImageCodecNewImageBufferMemory';
{
 *  ImageCodecExtractAndCombineFields()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageCodecExtractAndCombineFields(ci: ComponentInstance; fieldFlags: SInt32; data1: UnivPtr; dataSize1: SInt32; desc1: ImageDescriptionHandle; data2: UnivPtr; dataSize2: SInt32; desc2: ImageDescriptionHandle; outputData: UnivPtr; var outDataSize: SInt32; descOut: ImageDescriptionHandle): ComponentResult; external name '_ImageCodecExtractAndCombineFields';
{
 *  ImageCodecGetMaxCompressionSizeWithSources()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageCodecGetMaxCompressionSizeWithSources(ci: ComponentInstance; src: PixMapHandle; const (*var*) srcRect: Rect; depth: SInt16; quality: CodecQ; sourceData: CDSequenceDataSourcePtr; var size: SInt32): ComponentResult; external name '_ImageCodecGetMaxCompressionSizeWithSources';
{
 *  ImageCodecSetTimeBase()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageCodecSetTimeBase(ci: ComponentInstance; base: UnivPtr): ComponentResult; external name '_ImageCodecSetTimeBase';
{
 *  ImageCodecSourceChanged()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageCodecSourceChanged(ci: ComponentInstance; majorSourceChangeSeed: UInt32; minorSourceChangeSeed: UInt32; sourceData: CDSequenceDataSourcePtr; var flagsOut: SInt32): ComponentResult; external name '_ImageCodecSourceChanged';
{
 *  ImageCodecFlushFrame()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageCodecFlushFrame(ci: ComponentInstance; flags: UInt32): ComponentResult; external name '_ImageCodecFlushFrame';
{
 *  ImageCodecGetSettingsAsText()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageCodecGetSettingsAsText(ci: ComponentInstance; var text: Handle): ComponentResult; external name '_ImageCodecGetSettingsAsText';
{
 *  ImageCodecGetParameterListHandle()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageCodecGetParameterListHandle(ci: ComponentInstance; var parameterDescriptionHandle: Handle): ComponentResult; external name '_ImageCodecGetParameterListHandle';
{
 *  ImageCodecGetParameterList()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageCodecGetParameterList(ci: ComponentInstance; var parameterDescription: QTAtomContainer): ComponentResult; external name '_ImageCodecGetParameterList';
{
 *  ImageCodecCreateStandardParameterDialog()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageCodecCreateStandardParameterDialog(ci: ComponentInstance; parameterDescription: QTAtomContainer; parameters: QTAtomContainer; dialogOptions: QTParameterDialogOptions; existingDialog: DialogPtr; existingUserItem: SInt16; var createdDialog: QTParameterDialog): ComponentResult; external name '_ImageCodecCreateStandardParameterDialog';
{
 *  ImageCodecIsStandardParameterDialogEvent()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageCodecIsStandardParameterDialogEvent(ci: ComponentInstance; var pEvent: EventRecord; createdDialog: QTParameterDialog): ComponentResult; external name '_ImageCodecIsStandardParameterDialogEvent';
{
 *  ImageCodecDismissStandardParameterDialog()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageCodecDismissStandardParameterDialog(ci: ComponentInstance; createdDialog: QTParameterDialog): ComponentResult; external name '_ImageCodecDismissStandardParameterDialog';
{
 *  ImageCodecStandardParameterDialogDoAction()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageCodecStandardParameterDialogDoAction(ci: ComponentInstance; createdDialog: QTParameterDialog; action: SInt32; params: UnivPtr): ComponentResult; external name '_ImageCodecStandardParameterDialogDoAction';
{
 *  ImageCodecNewImageGWorld()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageCodecNewImageGWorld(ci: ComponentInstance; var params: CodecDecompressParams; var newGW: GWorldPtr; flags: SInt32): ComponentResult; external name '_ImageCodecNewImageGWorld';
{
 *  ImageCodecDisposeImageGWorld()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageCodecDisposeImageGWorld(ci: ComponentInstance; theGW: GWorldPtr): ComponentResult; external name '_ImageCodecDisposeImageGWorld';
{
 *  ImageCodecHitTestDataWithFlags()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageCodecHitTestDataWithFlags(ci: ComponentInstance; desc: ImageDescriptionHandle; data: UnivPtr; dataSize: Size; where: Point; var hit: SInt32; hitFlags: SInt32): ComponentResult; external name '_ImageCodecHitTestDataWithFlags';
{
 *  ImageCodecValidateParameters()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageCodecValidateParameters(ci: ComponentInstance; parameters: QTAtomContainer; validationFlags: QTParameterValidationOptions; errorString: StringPtr): ComponentResult; external name '_ImageCodecValidateParameters';
{
 *  ImageCodecGetBaseMPWorkFunction()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageCodecGetBaseMPWorkFunction(ci: ComponentInstance; var workFunction: ComponentMPWorkFunctionUPP; var refCon: UnivPtr; drawProc: ImageCodecMPDrawBandUPP; drawProcRefCon: UnivPtr): ComponentResult; external name '_ImageCodecGetBaseMPWorkFunction';
{
 *  ImageCodecRequestGammaLevel()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 5.0 and later
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 5.0 and later
 }
function ImageCodecRequestGammaLevel(ci: ComponentInstance; srcGammaLevel: Fixed; dstGammaLevel: Fixed; var codecCanMatch: SInt32): ComponentResult; external name '_ImageCodecRequestGammaLevel';
{
 *  ImageCodecGetSourceDataGammaLevel()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 5.0 and later
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 5.0 and later
 }
function ImageCodecGetSourceDataGammaLevel(ci: ComponentInstance; var sourceDataGammaLevel: Fixed): ComponentResult; external name '_ImageCodecGetSourceDataGammaLevel';
{
 *  ImageCodecGetDecompressLatency()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 5.0 and later
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 5.0 and later
 }
function ImageCodecGetDecompressLatency(ci: ComponentInstance; var latency: TimeRecord): ComponentResult; external name '_ImageCodecGetDecompressLatency';
{
 *  ImageCodecMergeFloatingImageOntoWindow()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 6.0 and later
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.2 and later
 *    Windows:          in qtmlClient.lib 6.0 and later
 }
function ImageCodecMergeFloatingImageOntoWindow(ci: ComponentInstance; flags: UInt32): ComponentResult; external name '_ImageCodecMergeFloatingImageOntoWindow';
{
 *  ImageCodecRemoveFloatingImage()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 6.0 and later
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.2 and later
 *    Windows:          in qtmlClient.lib 6.0 and later
 }
function ImageCodecRemoveFloatingImage(ci: ComponentInstance; flags: UInt32): ComponentResult; external name '_ImageCodecRemoveFloatingImage';
{
 *  ImageCodecGetDITLForSize()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 6.0 and later
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.2 and later
 *    Windows:          in qtmlClient.lib 6.0 and later
 }
function ImageCodecGetDITLForSize(ci: ComponentInstance; var ditl: Handle; var requestedSize: Point): ComponentResult; external name '_ImageCodecGetDITLForSize';
{
 *  ImageCodecDITLInstall()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 6.0 and later
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.2 and later
 *    Windows:          in qtmlClient.lib 6.0 and later
 }
function ImageCodecDITLInstall(ci: ComponentInstance; d: DialogRef; itemOffset: SInt16): ComponentResult; external name '_ImageCodecDITLInstall';
{
 *  ImageCodecDITLEvent()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 6.0 and later
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.2 and later
 *    Windows:          in qtmlClient.lib 6.0 and later
 }
function ImageCodecDITLEvent(ci: ComponentInstance; d: DialogRef; itemOffset: SInt16; const (*var*) theEvent: EventRecord; var itemHit: SInt16; var handled: boolean): ComponentResult; external name '_ImageCodecDITLEvent';
{
 *  ImageCodecDITLItem()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 6.0 and later
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.2 and later
 *    Windows:          in qtmlClient.lib 6.0 and later
 }
function ImageCodecDITLItem(ci: ComponentInstance; d: DialogRef; itemOffset: SInt16; itemNum: SInt16): ComponentResult; external name '_ImageCodecDITLItem';
{
 *  ImageCodecDITLRemove()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 6.0 and later
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.2 and later
 *    Windows:          in qtmlClient.lib 6.0 and later
 }
function ImageCodecDITLRemove(ci: ComponentInstance; d: DialogRef; itemOffset: SInt16): ComponentResult; external name '_ImageCodecDITLRemove';
{
 *  ImageCodecDITLValidateInput()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 6.0 and later
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.2 and later
 *    Windows:          in qtmlClient.lib 6.0 and later
 }
function ImageCodecDITLValidateInput(ci: ComponentInstance; var ok: boolean): ComponentResult; external name '_ImageCodecDITLValidateInput';
{
 *  ImageCodecPreflight()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageCodecPreflight(ci: ComponentInstance; var params: CodecDecompressParams): ComponentResult; external name '_ImageCodecPreflight';
{
 *  ImageCodecInitialize()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageCodecInitialize(ci: ComponentInstance; var cap: ImageSubCodecDecompressCapabilities): ComponentResult; external name '_ImageCodecInitialize';
{
 *  ImageCodecBeginBand()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageCodecBeginBand(ci: ComponentInstance; var params: CodecDecompressParams; var drp: ImageSubCodecDecompressRecord; flags: SInt32): ComponentResult; external name '_ImageCodecBeginBand';
{
 *  ImageCodecDrawBand()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageCodecDrawBand(ci: ComponentInstance; var drp: ImageSubCodecDecompressRecord): ComponentResult; external name '_ImageCodecDrawBand';
{
 *  ImageCodecEndBand()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageCodecEndBand(ci: ComponentInstance; var drp: ImageSubCodecDecompressRecord; result: OSErr; flags: SInt32): ComponentResult; external name '_ImageCodecEndBand';
{
 *  ImageCodecQueueStarting()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageCodecQueueStarting(ci: ComponentInstance): ComponentResult; external name '_ImageCodecQueueStarting';
{
 *  ImageCodecQueueStopping()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageCodecQueueStopping(ci: ComponentInstance): ComponentResult; external name '_ImageCodecQueueStopping';
{
 *  ImageCodecDroppingFrame()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageCodecDroppingFrame(ci: ComponentInstance; const (*var*) drp: ImageSubCodecDecompressRecord): ComponentResult; external name '_ImageCodecDroppingFrame';
{
 *  ImageCodecScheduleFrame()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function ImageCodecScheduleFrame(ci: ComponentInstance; const (*var*) drp: ImageSubCodecDecompressRecord; triggerProc: ImageCodecTimeTriggerUPP; triggerProcRefCon: UnivPtr): ComponentResult; external name '_ImageCodecScheduleFrame';
{
 *  ImageCodecCancelTrigger()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function ImageCodecCancelTrigger(ci: ComponentInstance): ComponentResult; external name '_ImageCodecCancelTrigger';
const
	kMotionJPEGTag				= $6D6A7067 (* 'mjpg' *);
	kJPEGQuantizationTablesImageDescriptionExtension = $6D6A7174 (* 'mjqt' *);
	kJPEGHuffmanTablesImageDescriptionExtension = $6D6A6874 (* 'mjht' *);
	kFieldInfoImageDescriptionExtension = $6669656C (* 'fiel' *);				{  image description extension describing the field count and field orderings }

	kFieldOrderUnknown			= 0;
	kFieldsStoredF1F2DisplayedF1F2 = 1;
	kFieldsStoredF1F2DisplayedF2F1 = 2;
	kFieldsStoredF2F1DisplayedF1F2 = 5;
	kFieldsStoredF2F1DisplayedF2F1 = 6;


type
	MotionJPEGApp1MarkerPtr = ^MotionJPEGApp1Marker;
	MotionJPEGApp1Marker = record
		unused:					SInt32;
		tag:					SInt32;
		fieldSize:				SInt32;
		paddedFieldSize:		SInt32;
		offsetToNextField:		SInt32;
		qTableOffset:			SInt32;
		huffmanTableOffset:		SInt32;
		sofOffset:				SInt32;
		sosOffset:				SInt32;
		soiOffset:				SInt32;
	end;

	FieldInfoImageDescriptionExtensionPtr = ^FieldInfoImageDescriptionExtension;
	FieldInfoImageDescriptionExtension = packed record
		fieldCount:				UInt8;
		fieldOrderings:			UInt8;
	end;


	{
	 *  QTPhotoSetSampling()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 *    Windows:          in qtmlClient.lib 3.0 and later
	 	}
function QTPhotoSetSampling(codec: ComponentInstance; yH: SInt16; yV: SInt16; cbH: SInt16; cbV: SInt16; crH: SInt16; crV: SInt16): ComponentResult; external name '_QTPhotoSetSampling';
{
 *  QTPhotoSetRestartInterval()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function QTPhotoSetRestartInterval(codec: ComponentInstance; restartInterval: UInt16): ComponentResult; external name '_QTPhotoSetRestartInterval';
{
 *  QTPhotoDefineHuffmanTable()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function QTPhotoDefineHuffmanTable(codec: ComponentInstance; componentNumber: SInt16; isDC: boolean; lengthCounts: UInt8Ptr; values: UInt8Ptr): ComponentResult; external name '_QTPhotoDefineHuffmanTable';
{
 *  QTPhotoDefineQuantizationTable()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function QTPhotoDefineQuantizationTable(codec: ComponentInstance; componentNumber: SInt16; table: UInt8Ptr): ComponentResult; external name '_QTPhotoDefineQuantizationTable';
{ source identifier -- placed in root container of description, one or more required }

const
	kEffectSourceName			= $73726320 (* 'src ' *);


	{	 source type -- placed in the input map to identify the source kind 	}
	kEffectDataSourceType		= $64747374 (* 'dtst' *);

	{	  default effect types 	}
	kEffectRawSource			= 0;							{  the source is raw image data }
	kEffectGenericType			= $67656666 (* 'geff' *);						{  generic effect for combining others }


type
	EffectSourcePtr = ^EffectSource;
	SourceDataPtr = ^SourceData;
	SourceData = record
		case SInt16 of
		0: (
			image:				CDSequenceDataSourcePtr;
			);
		1: (
			effect:				EffectSourcePtr;
			);
	end;


	EffectSource = record
		effectType:				SInt32;								{  type of effect or kEffectRawSource if raw ICM data }
		data:					Ptr;									{  track data for this effect }
		source:					SourceData;								{  source/effect pointers }
		next:					EffectSourcePtr;						{  the next source for the parent effect }
																		{  fields added for QuickTime 4.0 }
		lastTranslatedFrameTime: TimeValue;								{  start frame time of last converted frame, may be -1 }
		lastFrameDuration:		TimeValue;								{  duration of the last converted frame, may be zero }
		lastFrameTimeScale:		TimeValue;								{  time scale of this source frame, only has meaning if above fields are valid }
	end;

	EffectsFrameParamsPtr = ^EffectsFrameParams;
	EffectsFrameParams = record
		frameTime:				ICMFrameTimeRecord;						{  timing data }
		effectDuration:			SInt32;								{  the duration of a single effect frame }
		doAsync:				boolean;								{  set to true if the effect can go async }
		pad1,pad2,pad3:			SInt8;
		source:					EffectSourcePtr;						{  ptr to the source input tree }
		refCon:					Ptr;									{  storage for the effect }
	end;


	{
	 *  ImageCodecEffectSetup()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 *    Windows:          in qtmlClient.lib 3.0 and later
	 	}
function ImageCodecEffectSetup(effect: ComponentInstance; var p: CodecDecompressParams): ComponentResult; external name '_ImageCodecEffectSetup';
{
 *  ImageCodecEffectBegin()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageCodecEffectBegin(effect: ComponentInstance; var p: CodecDecompressParams; ePtr: EffectsFrameParamsPtr): ComponentResult; external name '_ImageCodecEffectBegin';
{
 *  ImageCodecEffectRenderFrame()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageCodecEffectRenderFrame(effect: ComponentInstance; p: EffectsFrameParamsPtr): ComponentResult; external name '_ImageCodecEffectRenderFrame';
{
 *  ImageCodecEffectConvertEffectSourceToFormat()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageCodecEffectConvertEffectSourceToFormat(effect: ComponentInstance; sourceToConvert: EffectSourcePtr; requestedDesc: ImageDescriptionHandle): ComponentResult; external name '_ImageCodecEffectConvertEffectSourceToFormat';
{
 *  ImageCodecEffectCancel()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageCodecEffectCancel(effect: ComponentInstance; p: EffectsFrameParamsPtr): ComponentResult; external name '_ImageCodecEffectCancel';
{
 *  ImageCodecEffectGetSpeed()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageCodecEffectGetSpeed(effect: ComponentInstance; parameters: QTAtomContainer; var pFPS: Fixed): ComponentResult; external name '_ImageCodecEffectGetSpeed';
const
	kSMPTENoFlag				= 0;
	kSMPTESmoothEdgeFlag		= $01;							{  smooth edges of the stroke }
	kSMPTEStrokeEdgeFlag		= $02;							{  stroke edge with color }


type
	SMPTEFlags							= SInt32;
	SMPTEFrameReference					= SInt32;

const
	kSlideHorizontalWipe		= 1;
	kSlideVerticalWipe			= 2;
	kTopLeftWipe				= 3;
	kTopRightWipe				= 4;
	kBottomRightWipe			= 5;
	kBottomLeftWipe				= 6;
	kFourCornerWipe				= 7;
	kFourBoxWipe				= 8;
	kBarnVerticalWipe			= 21;
	kBarnHorizontalWipe			= 22;
	kTopCenterWipe				= 23;
	kRightCenterWipe			= 24;
	kBottomCenterWipe			= 25;
	kLeftCenterWipe				= 26;
	kDiagonalLeftDownWipe		= 41;
	kDiagonalRightDownWipe		= 42;
	kTopBottomBowTieWipe		= 43;
	kLeftRightBowTieWipe		= 44;
	kDiagonalLeftOutWipe		= 45;
	kDiagonalRightOutWipe		= 46;
	kDiagonalCrossWipe			= 47;
	kDiagonalBoxWipe			= 48;
	kFilledVWipe				= 61;
	kFilledVRightWipe			= 62;
	kFilledVBottomWipe			= 63;
	kFilledVLeftWipe			= 64;
	kHollowVWipe				= 65;
	kHollowVRightWipe			= 66;
	kHollowVBottomWipe			= 67;
	kHollowVLeftWipe			= 68;
	kVerticalZigZagWipe			= 71;
	kHorizontalZigZagWipe		= 72;
	kVerticalBarnZigZagWipe		= 73;
	kHorizontalBarnZigZagWipe	= 74;

	kRectangleWipe				= 101;
	kDiamondWipe				= 102;
	kTriangleWipe				= 103;
	kTriangleRightWipe			= 104;
	kTriangleUpsideDownWipe		= 105;
	kTriangleLeftWipe			= 106;
	kSpaceShipWipe				= 107;
	kSpaceShipRightWipe			= 108;
	kSpaceShipUpsideDownWipe	= 109;
	kSpaceShipLeftWipe			= 110;
	kPentagonWipe				= 111;
	kPentagonUpsideDownWipe		= 112;
	kHexagonWipe				= 113;
	kHexagonSideWipe			= 114;
	kCircleWipe					= 119;
	kOvalWipe					= 120;
	kOvalSideWipe				= 121;
	kCatEyeWipe					= 122;
	kCatEyeSideWipe				= 123;
	kRoundRectWipe				= 124;
	kRoundRectSideWipe			= 125;
	kFourPointStarWipe			= 127;
	kFivePointStarWipe			= 128;
	kStarOfDavidWipe			= 129;
	kHeartWipe					= 130;
	kKeyholeWipe				= 131;

	kRotatingTopWipe			= 201;
	kRotatingRightWipe			= 202;
	kRotatingBottomWipe			= 203;
	kRotatingLeftWipe			= 204;
	kRotatingTopBottomWipe		= 205;
	kRotatingLeftRightWipe		= 206;
	kRotatingQuadrantWipe		= 207;
	kTopToBottom180Wipe			= 211;
	kRightToLeft180Wipe			= 212;
	kTopToBottom90Wipe			= 213;
	kRightToLeft90Wipe			= 214;
	kTop180Wipe					= 221;
	kRight180Wipe				= 222;
	kBottom180Wipe				= 223;
	kLeft180Wipe				= 224;
	kCounterRotatingTopBottomWipe = 225;
	kCounterRotatingLeftRightWipe = 226;
	kDoubleRotatingTopBottomWipe = 227;
	kDoubleRotatingLeftRightWipe = 228;
	kVOpenTopWipe				= 231;
	kVOpenRightWipe				= 232;
	kVOpenBottomWipe			= 233;
	kVOpenLeftWipe				= 234;
	kVOpenTopBottomWipe			= 235;
	kVOpenLeftRightWipe			= 236;
	kRotatingTopLeftWipe		= 241;
	kRotatingBottomLeftWipe		= 242;
	kRotatingBottomRightWipe	= 243;
	kRotatingTopRightWipe		= 244;
	kRotatingTopLeftBottomRightWipe = 245;
	kRotatingBottomLeftTopRightWipe = 246;
	kRotatingTopLeftRightWipe	= 251;
	kRotatingLeftTopBottomWipe	= 252;
	kRotatingBottomLeftRightWipe = 253;
	kRotatingRightTopBottomWipe	= 254;
	kRotatingDoubleCenterRightWipe = 261;
	kRotatingDoubleCenterTopWipe = 262;
	kRotatingDoubleCenterTopBottomWipe = 263;
	kRotatingDoubleCenterLeftRightWipe = 264;

	kHorizontalMatrixWipe		= 301;
	kVerticalMatrixWipe			= 302;
	kTopLeftDiagonalMatrixWipe	= 303;
	kTopRightDiagonalMatrixWipe	= 304;
	kBottomRightDiagonalMatrixWipe = 305;
	kBottomLeftDiagonalMatrixWipe = 306;
	kClockwiseTopLeftMatrixWipe	= 310;
	kClockwiseTopRightMatrixWipe = 311;
	kClockwiseBottomRightMatrixWipe = 312;
	kClockwiseBottomLeftMatrixWipe = 313;
	kCounterClockwiseTopLeftMatrixWipe = 314;
	kCounterClockwiseTopRightMatrixWipe = 315;
	kCounterClockwiseBottomRightMatrixWipe = 316;
	kCounterClockwiseBottomLeftMatrixWipe = 317;
	kVerticalStartTopMatrixWipe	= 320;
	kVerticalStartBottomMatrixWipe = 321;
	kVerticalStartTopOppositeMatrixWipe = 322;
	kVerticalStartBottomOppositeMatrixWipe = 323;
	kHorizontalStartLeftMatrixWipe = 324;
	kHorizontalStartRightMatrixWipe = 325;
	kHorizontalStartLeftOppositeMatrixWipe = 326;
	kHorizontalStartRightOppositeMatrixWipe = 327;
	kDoubleDiagonalTopRightMatrixWipe = 328;
	kDoubleDiagonalBottomRightMatrixWipe = 329;
	kDoubleSpiralTopMatixWipe	= 340;
	kDoubleSpiralBottomMatixWipe = 341;
	kDoubleSpiralLeftMatixWipe	= 342;
	kDoubleSpiralRightMatixWipe	= 343;
	kQuadSpiralVerticalMatixWipe = 344;
	kQuadSpiralHorizontalMatixWipe = 345;
	kVerticalWaterfallLeftMatrixWipe = 350;
	kVerticalWaterfallRightMatrixWipe = 351;
	kHorizontalWaterfallLeftMatrixWipe = 352;
	kHorizontalWaterfallRightMatrixWipe = 353;
	kRandomWipe					= 409;							{  non-SMPTE standard numbers }
	kRandomWipeGroupWipe		= 501;
	kRandomIrisGroupWipe		= 502;
	kRandomRadialGroupWipe		= 503;
	kRandomMatrixGroupWipe		= 504;


type
	SMPTEWipeType						= UInt32;
	{
	 *  ImageCodecEffectPrepareSMPTEFrame()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
	 *    CarbonLib:        in CarbonLib 1.0.2 and later
	 *    Mac OS X:         in version 10.0 and later
	 *    Windows:          in qtmlClient.lib 4.0 and later
	 	}
function ImageCodecEffectPrepareSMPTEFrame(effect: ComponentInstance; destPixMap: PixMapPtr; var returnValue: SMPTEFrameReference): ComponentResult; external name '_ImageCodecEffectPrepareSMPTEFrame';
{
 *  ImageCodecEffectDisposeSMPTEFrame()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function ImageCodecEffectDisposeSMPTEFrame(effect: ComponentInstance; frameRef: SMPTEFrameReference): ComponentResult; external name '_ImageCodecEffectDisposeSMPTEFrame';
{
 *  ImageCodecEffectRenderSMPTEFrame()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function ImageCodecEffectRenderSMPTEFrame(effect: ComponentInstance; destPixMap: PixMapPtr; frameRef: SMPTEFrameReference; effectPercentageEven: Fixed; effectPercentageOdd: Fixed; var pSourceRect: Rect; var pMatrix: MatrixRecord; effectNumber: SMPTEWipeType; xRepeat: SInt32; yRepeat: SInt32; flags: SMPTEFlags; penWidth: Fixed; strokeValue: SInt32): ComponentResult; external name '_ImageCodecEffectRenderSMPTEFrame';
{ curve atom types and data structures }

const
	kCurvePathAtom				= $70617468 (* 'path' *);
	kCurveEndAtom				= $7A65726F (* 'zero' *);
	kCurveAntialiasControlAtom	= $616E7469 (* 'anti' *);
	kCurveAntialiasOff			= 0;
	kCurveAntialiasOn			= $FFFFFFFF;
	kCurveFillTypeAtom			= $66696C6C (* 'fill' *);
	kCurvePenThicknessAtom		= $70656E74 (* 'pent' *);
	kCurveMiterLimitAtom		= $6D697472 (* 'mitr' *);
	kCurveJoinAttributesAtom	= $6A6F696E (* 'join' *);
	kCurveMinimumDepthAtom		= $6D696E64 (* 'mind' *);
	kCurveDepthAlwaysOffscreenMask = $80000000;
	kCurveTransferModeAtom		= $78666572 (* 'xfer' *);
	kCurveGradientAngleAtom		= $616E676C (* 'angl' *);
	kCurveGradientRadiusAtom	= $72616469 (* 'radi' *);
	kCurveGradientOffsetAtom	= $63656E74 (* 'cent' *);

	kCurveARGBColorAtom			= $61726762 (* 'argb' *);


type
	ARGBColorPtr = ^ARGBColor;
	ARGBColor = record
		alpha:					UInt16;
		red:					UInt16;
		green:					UInt16;
		blue:					UInt16;
	end;


const
	kCurveGradientRecordAtom	= $67726164 (* 'grad' *);


type
	GradientColorRecordPtr = ^GradientColorRecord;
	GradientColorRecord = record
		thisColor:				ARGBColor;
		endingPercentage:		Fixed;
	end;

	GradientColorPtr					= ^GradientColorRecord;

const
	kCurveGradientTypeAtom		= $67726474 (* 'grdt' *);

	{	 currently supported gradient types 	}
	kLinearGradient				= 0;
	kCircularGradient			= 1;


type
	GradientType						= SInt32;
	{
	 *  CurveGetLength()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 *    Windows:          in qtmlClient.lib 3.0 and later
	 	}
function CurveGetLength(effect: ComponentInstance; var target: gxPaths; index: SInt32; var wideLength: wide): ComponentResult; external name '_CurveGetLength';
{
 *  CurveLengthToPoint()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function CurveLengthToPoint(effect: ComponentInstance; var target: gxPaths; index: SInt32; length: Fixed; var location: FixedPoint; var tangent: FixedPoint): ComponentResult; external name '_CurveLengthToPoint';
{
 *  CurveNewPath()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function CurveNewPath(effect: ComponentInstance; var pPath: Handle): ComponentResult; external name '_CurveNewPath';
{
 *  CurveCountPointsInPath()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function CurveCountPointsInPath(effect: ComponentInstance; var aPath: gxPaths; contourIndex: UInt32; var pCount: UInt32): ComponentResult; external name '_CurveCountPointsInPath';
{
 *  CurveGetPathPoint()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function CurveGetPathPoint(effect: ComponentInstance; var aPath: gxPaths; contourIndex: UInt32; pointIndex: UInt32; var thePoint: gxPoint; var ptIsOnPath: boolean): ComponentResult; external name '_CurveGetPathPoint';
{
 *  CurveInsertPointIntoPath()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function CurveInsertPointIntoPath(effect: ComponentInstance; var aPoint: gxPoint; thePath: Handle; contourIndex: UInt32; pointIndex: UInt32; ptIsOnPath: boolean): ComponentResult; external name '_CurveInsertPointIntoPath';
{
 *  CurveSetPathPoint()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function CurveSetPathPoint(effect: ComponentInstance; var aPath: gxPaths; contourIndex: UInt32; pointIndex: UInt32; var thePoint: gxPoint; ptIsOnPath: boolean): ComponentResult; external name '_CurveSetPathPoint';
{
 *  CurveGetNearestPathPoint()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function CurveGetNearestPathPoint(effect: ComponentInstance; var aPath: gxPaths; var thePoint: FixedPoint; var contourIndex: UInt32; var pointIndex: UInt32; var theDelta: Fixed): ComponentResult; external name '_CurveGetNearestPathPoint';
{
 *  CurvePathPointToLength()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function CurvePathPointToLength(ci: ComponentInstance; var aPath: gxPaths; startDist: Fixed; endDist: Fixed; var thePoint: FixedPoint; var pLength: Fixed): ComponentResult; external name '_CurvePathPointToLength';
{
 *  CurveCreateVectorStream()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function CurveCreateVectorStream(effect: ComponentInstance; var pStream: Handle): ComponentResult; external name '_CurveCreateVectorStream';
{
 *  CurveAddAtomToVectorStream()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function CurveAddAtomToVectorStream(effect: ComponentInstance; atomType: OSType; atomSize: Size; pAtomData: UnivPtr; vectorStream: Handle): ComponentResult; external name '_CurveAddAtomToVectorStream';
{
 *  CurveAddPathAtomToVectorStream()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function CurveAddPathAtomToVectorStream(effect: ComponentInstance; pathData: Handle; vectorStream: Handle): ComponentResult; external name '_CurveAddPathAtomToVectorStream';
{
 *  CurveAddZeroAtomToVectorStream()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function CurveAddZeroAtomToVectorStream(effect: ComponentInstance; vectorStream: Handle): ComponentResult; external name '_CurveAddZeroAtomToVectorStream';
{
 *  CurveGetAtomDataFromVectorStream()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function CurveGetAtomDataFromVectorStream(effect: ComponentInstance; vectorStream: Handle; atomType: SInt32; var dataSize: SInt32; var dataPtr: Ptr): ComponentResult; external name '_CurveGetAtomDataFromVectorStream';
{ UPP call backs }
{$ALIGN MAC68K}


end.
