{
     File:       QuickTime/ImageCodec.h
 
     Contains:   QuickTime Interfaces.
 
     Version:    QuickTime 7.7.1
 
     Copyright:  © 1990-2012 by Apple Inc., all rights reserved
 
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

unit ImageCodec;
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
uses MacTypes,Components,Dialogs,Events,ImageCompression,Movies,OSUtils,QuickdrawTypes,QDOffscreen,CFDictionary;
{$endc} {not MACOSALLINCLUDE}


{$ifc TARGET_OS_MAC}

{$ALIGN MAC68K}

{ QuickTime is not available to 64-bit clients }

{$ifc not TARGET_CPU_64}

{
   The following GX types were previously in GXTypes.h, but that header
   is not available in any Mac OS X framework. 
}
type
	gxPoint = record
		x: Fixed;
		y: Fixed;
	end;
type
	gxPathPtr = ^gxPath;
	gxPath = record
		vectors: SIGNEDLONG;
		controlBits: array [0..0] of SIGNEDLONG;
		vector: array [0..0] of gxPoint;
	end;
type
	gxPathsPtr = ^gxPaths;
	gxPaths = record
		contours: SIGNEDLONG;
    contour: array [0..0] of gxPath;
	end;
{  codec capabilities flags    }
const
	codecCanScale = 1 shl 0;
	codecCanMask = 1 shl 1;
	codecCanMatte = 1 shl 2;
	codecCanTransform = 1 shl 3;
	codecCanTransferMode = 1 shl 4;
	codecCanCopyPrev = 1 shl 5;
	codecCanSpool = 1 shl 6;
	codecCanClipVertical = 1 shl 7;
	codecCanClipRectangular = 1 shl 8;
	codecCanRemapColor = 1 shl 9;
	codecCanFastDither = 1 shl 10;
	codecCanSrcExtract = 1 shl 11;
	codecCanCopyPrevComp = 1 shl 12;
	codecCanAsync = 1 shl 13;
	codecCanMakeMask = 1 shl 14;
	codecCanShift = 1 shl 15;
	codecCanAsyncWhen = 1 shl 16;
	codecCanShieldCursor = 1 shl 17;
	codecCanManagePrevBuffer = 1 shl 18;
	codecHasVolatileBuffer = 1 shl 19; { codec requires redraw after window movement }
	codecWantsRegionMask = 1 shl 20;
	codecImageBufferIsOnScreen = 1 shl 21; { old def of codec using overlay surface, = ( codecIsDirectToScreenOnly | codecUsesOverlaySurface | codecImageBufferIsOverlaySurface | codecSrcMustBeImageBuffer ) }
	codecWantsDestinationPixels = 1 shl 22;
	codecWantsSpecialScaling = 1 shl 23;
	codecHandlesInputs = 1 shl 24;
	codecCanDoIndirectSurface = 1 shl 25; { codec can handle indirect surface (GDI) }
	codecIsSequenceSensitive = 1 shl 26;
	codecRequiresOffscreen = 1 shl 27;
	codecRequiresMaskBits = 1 shl 28;
	codecCanRemapResolution = 1 shl 29;
	codecIsDirectToScreenOnly = 1 shl 30; { codec can only decompress data to the screen }
	codecCanLockSurface = 1 shl 31; { codec can lock destination surface, icm doesn't lock for you }

{  codec capabilities flags2   }
const
	codecUsesOverlaySurface = 1 shl 0; { codec uses overlay surface }
	codecImageBufferIsOverlaySurface = 1 shl 1; { codec image buffer is overlay surface, the bits in the buffer are on the screen }
	codecSrcMustBeImageBuffer = 1 shl 2; { codec can only source data from an image buffer }
	codecImageBufferIsInAGPMemory = 1 shl 4; { codec image buffer is in AGP space, byte writes are OK }
	codecImageBufferIsInPCIMemory = 1 shl 5; { codec image buffer is across a PCI bus; byte writes are bad }
	codecImageBufferMemoryFlagsValid = 1 shl 6; { set by ImageCodecNewImageBufferMemory/NewImageGWorld to indicate that it set the AGP/PCI flags (supported in QuickTime 6.0 and later) }
	codecDrawsHigherQualityScaled = 1 shl 7; { codec will draw higher-quality image if it performs scaling (eg, wipe effect with border) }
	codecSupportsOutOfOrderDisplayTimes = 1 shl 8; { codec supports frames queued in one order for display in a different order, eg, IPB content }
	codecSupportsScheduledBackwardsPlaybackWithDifferenceFrames = 1 shl 9; { codec can use additional buffers to minimise redecoding during backwards playback }

type
	CodecCapabilitiesPtr = ^CodecCapabilities;
	CodecCapabilities = record
		flags: SIGNEDLONG;
		wantedPixelSize: SInt16;
		extendWidth: SInt16;
		extendHeight: SInt16;
		bandMin: SInt16;
		bandInc: SInt16;
		pad: SInt16;
		time: UNSIGNEDLONG;
		flags2: SIGNEDLONG;                 { field new in QuickTime 4.0 }
	end;
{  codec condition flags   }
const
	codecConditionFirstBand = 1 shl 0;
	codecConditionLastBand = 1 shl 1;
	codecConditionFirstFrame = 1 shl 2;
	codecConditionNewDepth = 1 shl 3;
	codecConditionNewTransform = 1 shl 4;
	codecConditionNewSrcRect = 1 shl 5;
	codecConditionNewMask = 1 shl 6;
	codecConditionNewMatte = 1 shl 7;
	codecConditionNewTransferMode = 1 shl 8;
	codecConditionNewClut = 1 shl 9;
	codecConditionNewAccuracy = 1 shl 10;
	codecConditionNewDestination = 1 shl 11;
	codecConditionFirstScreen = 1 shl 12;
	codecConditionDoCursor = 1 shl 13;
	codecConditionCatchUpDiff = 1 shl 14;
	codecConditionMaskMayBeChanged = 1 shl 15;
	codecConditionToBuffer = 1 shl 16;
	codecConditionCodecChangedMask = 1 shl 31;


const
	codecInfoResourceType = FourCharCode('cdci'); { codec info resource type }
	codecInterfaceVersion = 2;     { high word returned in component GetVersion }

type
	CDSequenceDataSourceQueueEntry = record
		nextBusy: UnivPtr;

		descSeed: SIGNEDLONG;
		dataDesc: Handle;
		data: UnivPtr;
		dataSize: SIGNEDLONG;

		useCount: SIGNEDLONG;

		frameTime: TimeValue;
		frameDuration: TimeValue;
		timeScale: TimeValue;
	end;
	CDSequenceDataSourceQueueEntryPtr = ^CDSequenceDataSourceQueueEntry;
type
	CDSequenceDataSource = record
		recordSize: SIGNEDLONG;

		next: UnivPtr;

		seqID: ImageSequence;
		sourceID: ImageSequenceDataSource;
		sourceType: OSType;
		sourceInputNumber: SIGNEDLONG;
		dataPtr: UnivPtr;
		dataDescription: Handle;
		changeSeed: SIGNEDLONG;
		transferProc: ICMConvertDataFormatUPP;
		transferRefcon: UnivPtr;
		dataSize: SIGNEDLONG;

                                              { fields available in QT 3 and later }

		dataQueue: QHdrPtr;              { queue of CDSequenceDataSourceQueueEntry structures}

		originalDataPtr: UnivPtr;
		originalDataSize: SIGNEDLONG;
		originalDataDescription: Handle;
		originalDataDescriptionSeed: SIGNEDLONG;
	end;
	CDSequenceDataSourcePtr = ^CDSequenceDataSource;
type
	ICMFrameTimeInfo = record
		startTime: wide;
		scale: SIGNEDLONG;
		duration: SIGNEDLONG;
	end;
	ICMFrameTimeInfoPtr = ^ICMFrameTimeInfo;
type
	CodecCompressParams = record
		sequenceID: ImageSequence;             { precompress,bandcompress }
		imageDescription: ImageDescriptionHandle;   { precompress,bandcompress }
		data: Ptr;
		bufferSize: SIGNEDLONG;
		frameNumber: SIGNEDLONG;
		startLine: SIGNEDLONG;
		stopLine: SIGNEDLONG;
		conditionFlags: SIGNEDLONG;
		callerFlags: CodecFlags;
		capabilities: CodecCapabilitiesPtr;           { precompress,bandcompress }
		progressProcRecord: ICMProgressProcRecord;
		completionProcRecord: ICMCompletionProcRecord;
		flushProcRecord: ICMFlushProcRecord;

		srcPixMap: PixMap;              { precompress,bandcompress }
		prevPixMap: PixMap;
		spatialQuality: CodecQ;
		temporalQuality: CodecQ;
		similarity: Fixed;
		dataRateParams: DataRateParamsPtr;
		reserved: SIGNEDLONG;

                                              { The following fields only exist for QuickTime 2.1 and greater }
		majorSourceChangeSeed: UInt16;
		minorSourceChangeSeed: UInt16;
		sourceData: CDSequenceDataSourcePtr;

                                              { The following fields only exist for QuickTime 2.5 and greater }
		preferredPacketSizeInBytes: SIGNEDLONG;

                                              { The following fields only exist for QuickTime 3.0 and greater }
		requestedBufferWidth: SIGNEDLONG;   { must set codecWantsSpecialScaling to indicate this field is valid}
		requestedBufferHeight: SIGNEDLONG;  { must set codecWantsSpecialScaling to indicate this field is valid}

                                              { The following fields only exist for QuickTime 4.0 and greater }
		wantedSourcePixelType: OSType;

                                              { The following fields only exist for QuickTime 5.0 and greater }
		compressedDataSize: SIGNEDLONG;     { if nonzero, this overrides (*imageDescription)->dataSize}
		taskWeight: UInt32;             { preferred weight for MP tasks implementing this operation}
		taskName: OSType;               { preferred name (type) for MP tasks implementing this operation}
	end;
type
	CodecDecompressParamsPtr = ^CodecDecompressParams;
	CodecDecompressParams = record
		sequenceID: ImageSequence;             { predecompress,banddecompress }
		imageDescription: ImageDescriptionHandle;   { predecompress,banddecompress }
		data: Ptr;
		bufferSize: SIGNEDLONG;
		frameNumber: SIGNEDLONG;
		startLine: SIGNEDLONG;
		stopLine: SIGNEDLONG;
		conditionFlags: SIGNEDLONG;
		callerFlags: CodecFlags;
		capabilities: CodecCapabilitiesPtr;           { predecompress,banddecompress }
		progressProcRecord: ICMProgressProcRecord;
		completionProcRecord: ICMCompletionProcRecord;
		dataProcRecord: ICMDataProcRecord;

		port: CGrafPtr;                   { predecompress,banddecompress }
		dstPixMap: PixMap;              { predecompress,banddecompress }
		maskBits: BitMapPtr;
		mattePixMap: PixMapPtr;
		srcRect: Rect;                { predecompress,banddecompress }
		matrix: MatrixRecordPtr;                 { predecompress,banddecompress }
		accuracy: CodecQ;               { predecompress,banddecompress }
		transferMode: SInt16;           { predecompress,banddecompress }
		frameTime: ICMFrameTimePtr;              { banddecompress }
    reserved: array [0..0] of SIGNEDLONG;

                                              { The following fields only exist for QuickTime 2.0 and greater }
		matrixFlags: SInt8;            { high bit set if 2x resize }
		matrixType: SInt8;
		dstRect: Rect;                { only valid for simple transforms }

                                              { The following fields only exist for QuickTime 2.1 and greater }
		majorSourceChangeSeed: UInt16;
		minorSourceChangeSeed: UInt16;
		sourceData: CDSequenceDataSourcePtr;

		maskRegion: RgnHandle;

                                              { The following fields only exist for QuickTime 2.5 and greater }
		wantedDestinationPixelTypes: OSTypeHandle; { Handle to 0-terminated list of OSTypes }

		screenFloodMethod: SIGNEDLONG;
		screenFloodValue: SIGNEDLONG;
		preferredOffscreenPixelSize: SInt16;

                                              { The following fields only exist for QuickTime 3.0 and greater }
		syncFrameTime: ICMFrameTimeInfoPtr;         { banddecompress }
		needUpdateOnTimeChange: Boolean; { banddecompress }
		enableBlackLining: Boolean;
		needUpdateOnSourceChange: Boolean; { band decompress }
		pad: Boolean;

		unused: SIGNEDLONG;

		finalDestinationPort: CGrafPtr;

		requestedBufferWidth: SIGNEDLONG;   { must set codecWantsSpecialScaling to indicate this field is valid}
		requestedBufferHeight: SIGNEDLONG;  { must set codecWantsSpecialScaling to indicate this field is valid}

                                              { The following fields only exist for QuickTime 4.0 and greater }
		displayableAreaOfRequestedBuffer: Rect; { set in predecompress}
		requestedSingleField: Boolean;
		needUpdateOnNextIdle: Boolean;
		pad2: array [0..1] of Boolean;
		bufferGammaLevel: Fixed;

                                              { The following fields only exist for QuickTime 5.0 and greater }
		taskWeight: UInt32;             { preferred weight for MP tasks implementing this operation}
		taskName: OSType;               { preferred name (type) for MP tasks implementing this operation}

                                              { The following fields only exist for QuickTime 6.0 and greater }
		pad3: Boolean;
		destinationBufferMemoryPreference: UInt8; { a codec's PreDecompress/Preflight call can set this to express a preference about what kind of memory its destination buffer should go into.  no guarantees.}
		codecBufferMemoryPreference: UInt8; { may indicate preferred kind of memory that NewImageGWorld/NewImageBufferMemory should create its buffer in, if applicable.}
		onlyUseCodecIfItIsInUserPreferredCodecList: Boolean; { set to prevent this codec from being used unless it is in the userPreferredCodec list}

		mediaContextID: QTMediaContextID;

                                              { The following fields only exist for QuickTime 6.5 and greater }
		deinterlaceRequest: UInt8;     { set by the ICM before PreDecompress/Preflight }
		deinterlaceAnswer: UInt8;      { codec should set this in PreDecompress/Preflight if it will satisfy the deinterlaceRequest }

                                              { The following fields only exist for QuickTime 7.0 and greater }
		pad4: array[0..1] of UInt8;
		reserved2: SIGNEDLONG;
		reserved3: UInt32;
		reserved4: SIGNEDLONG;
		reserved5: UnivPtr;
		reserved6: UnivPtr;
		reserved7: UnivPtr;
		reserved8: UnivPtr;
	end;
const
	matrixFlagScale2x = 1 shl 7;
	matrixFlagScale1x = 1 shl 6;
	matrixFlagScaleHalf = 1 shl 5;

const
	kScreenFloodMethodNone = 0;
	kScreenFloodMethodKeyColor = 1;
	kScreenFloodMethodAlpha = 2;

const
	kFlushLastQueuedFrame = 0;
	kFlushFirstQueuedFrame = 1;

const
	kNewImageGWorldErase = 1 shl 0;

{ values for destinationBufferMemoryPreference and codecBufferMemoryPreference }
const
	kICMImageBufferNoPreference = 0;
	kICMImageBufferPreferMainMemory = 1;
	kICMImageBufferPreferVideoMemory = 2;

{ values for deinterlaceRequest and deinterlaceAnswer }
const
	kICMNoDeinterlacing = 0;
	kICMDeinterlaceFields = 1;

type
	ImageCodecTimeTriggerProcPtr = procedure( refcon: UnivPtr );
	ImageCodecDrawBandCompleteProcPtr = procedure( refcon: UnivPtr; drawBandResult: ComponentResult; drawBandCompleteFlags: UInt32 );
	ImageCodecTimeTriggerUPP = ImageCodecTimeTriggerProcPtr;
	ImageCodecDrawBandCompleteUPP = ImageCodecDrawBandCompleteProcPtr;
{
 *  NewImageCodecTimeTriggerUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function NewImageCodecTimeTriggerUPP( userRoutine: ImageCodecTimeTriggerProcPtr ): ImageCodecTimeTriggerUPP; external name '_NewImageCodecTimeTriggerUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  NewImageCodecDrawBandCompleteUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function NewImageCodecDrawBandCompleteUPP( userRoutine: ImageCodecDrawBandCompleteProcPtr ): ImageCodecDrawBandCompleteUPP; external name '_NewImageCodecDrawBandCompleteUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  DisposeImageCodecTimeTriggerUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure DisposeImageCodecTimeTriggerUPP( userUPP: ImageCodecTimeTriggerUPP ); external name '_DisposeImageCodecTimeTriggerUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  DisposeImageCodecDrawBandCompleteUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure DisposeImageCodecDrawBandCompleteUPP( userUPP: ImageCodecDrawBandCompleteUPP ); external name '_DisposeImageCodecDrawBandCompleteUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  InvokeImageCodecTimeTriggerUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure InvokeImageCodecTimeTriggerUPP( refcon: UnivPtr; userUPP: ImageCodecTimeTriggerUPP ); external name '_InvokeImageCodecTimeTriggerUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  InvokeImageCodecDrawBandCompleteUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure InvokeImageCodecDrawBandCompleteUPP( refcon: UnivPtr; drawBandResult: ComponentResult; drawBandCompleteFlags: UInt32; userUPP: ImageCodecDrawBandCompleteUPP ); external name '_InvokeImageCodecDrawBandCompleteUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

type
	ImageSubCodecDecompressCapabilitiesPtr = ^ImageSubCodecDecompressCapabilities;
	ImageSubCodecDecompressCapabilities = record
		recordSize: SIGNEDLONG;             { sizeof(ImageSubCodecDecompressCapabilities)}
		decompressRecordSize: SIGNEDLONG;   { size of your codec's decompress record}
		canAsync: Boolean;               { default true}
		pad0: UInt8;
                                              { The following field only exists for QuickTime 4.1 and greater }
		suggestedQueueSize: UInt16;
                                              { The following field only exists for QuickTime 4.0 and greater }
		canProvideTrigger: Boolean;

                                              { The following fields only exist for QuickTime 5.0 and greater }
		subCodecFlushesScreen: Boolean;  { only used on Mac OS X}
		subCodecCallsDrawBandComplete: Boolean;
		pad2: array[0..1] of UInt8;

                                              { The following fields only exist for QuickTime 5.0.1 and greater }
		isChildCodec: Boolean;           { set by base codec before calling Initialize}
		reserved1: UInt8;
		pad4: array[0..1] of UInt8;

                                              { The following fields only exist for QuickTime 7.0 and greater }
		subCodecIsMultiBufferAware: Boolean; { set if subcodec always draws using ImageSubCodecDecompressRecord.baseAddr/rowBytes passed to ImageCodecDrawBand, and always writes every pixel in the buffer}
		subCodecSupportsOutOfOrderDisplayTimes: Boolean;
		subCodecSupportsScheduledBackwardsPlaybackWithDifferenceFrames: Boolean;
		subCodecNeedsHelpReportingNonDisplayableFrames: Boolean;
		baseCodecShouldCallDecodeBandForAllFrames: Boolean;

		pad5: array[0..1] of UInt8;
		subCodecSupportsDrawInDecodeOrder: Boolean; { indicates that it's okay for the subcodec to get a single DrawBand call for each frame in decode order even when frames need reordering.  (This will only happen when other circumstances allow it.)}
		subCodecSupportsDecodeSmoothing: Boolean; { Frame-reordering subcodecs should set this to indicate that they can safely decode a non-droppable frame before drawing the previous non-droppable frame.  This enables smoother playback in GWorlds.}
		pad6: array[0..3] of UInt8;
	end;
const
	kCodecFrameTypeUnknown = 0;
	kCodecFrameTypeKey = 1;
	kCodecFrameTypeDifference = 2;
	kCodecFrameTypeDroppableDifference = 3;

type
	ImageSubCodecDecompressRecordPtr = ^ImageSubCodecDecompressRecord;
	ImageSubCodecDecompressRecord = record
		baseAddr: Ptr;
		rowBytes: SIGNEDLONG;
		codecData: Ptr;
		progressProcRecord: ICMProgressProcRecord;
		dataProcRecord: ICMDataProcRecord;
		userDecompressRecord: UnivPtr;   { pointer to codec-specific per-band data}
		frameType: UInt8;
		inhibitMP: Boolean;              { set this in BeginBand to tell the base decompressor not to call DrawBand from an MP task for this frame.  (Only has any effect for MP-capable subcodecs.  New in QuickTime 5.0.)}
		pad2: array[0..11] of UInt8;
		priv: array[0..1] of SIGNEDLONG;

                                              { The following fields only exist for QuickTime 5.0 and greater }
		drawBandCompleteUPP: ImageCodecDrawBandCompleteUPP; { only used if subcodec set subCodecCallsDrawBandComplete; if drawBandCompleteUPP is non-nil, codec must call it when a frame is finished, but may return from DrawBand before the frame is finished. }
		drawBandCompleteRefCon: UnivPtr; { Note: do not call drawBandCompleteUPP directly from a hardware interrupt; instead, use DTInstall to run a function at deferred task time, and call drawBandCompleteUPP from that. }

                                              { The following fields only exist for QuickTime 7.0 and greater }
		reserved1: UnivPtr;
		reserved2: SIGNEDLONG;
		reserved3: SIGNEDLONG;
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
type
	PlanarComponentInfo = record
		offset: SInt32;
		rowBytes: UInt32;
	end;
type
	PlanarPixMapInfoPtr = ^PlanarPixMapInfo;
	PlanarPixMapInfo = record
		componentInfo: array [0..0] of PlanarComponentInfo;
	end;
type
	PlanarPixmapInfoSorensonYUV9Ptr = ^PlanarPixmapInfoSorensonYUV9;
	PlanarPixmapInfoSorensonYUV9 = record
		componentInfoY: PlanarComponentInfo;
		componentInfoU: PlanarComponentInfo;
		componentInfoV: PlanarComponentInfo;
	end;
type
	PlanarPixmapInfoYUV420Ptr = ^PlanarPixmapInfoYUV420;
	PlanarPixmapInfoYUV420 = record
		componentInfoY: PlanarComponentInfo;
		componentInfoCb: PlanarComponentInfo;
		componentInfoCr: PlanarComponentInfo;
	end;
const
	codecSuggestedBufferSentinel = FourCharCode('sent'); { codec public resource containing suggested data pattern to put past end of data buffer }


{ name of parameters or effect -- placed in root container, required }
const
	kParameterTitleName = FourCharCode('name');
	kParameterTitleID = 1;

{ codec sub-type of parameters or effect -- placed in root container, required }
const
	kParameterWhatName = FourCharCode('what');
	kParameterWhatID = 1;

{ effect version -- placed in root container, optional, but recommended }
const
	kParameterVersionName = FourCharCode('vers');
	kParameterVersionID = 1;

{ is effect repeatable -- placed in root container, optional, default is TRUE}
const
	kParameterRepeatableName = FourCharCode('pete');
	kParameterRepeatableID = 1;

const
	kParameterRepeatableTrue = 1;
	kParameterRepeatableFalse = 0;

{ substitution codec in case effect is missing -- placed in root container, recommended }
const
	kParameterAlternateCodecName = FourCharCode('subs');
	kParameterAlternateCodecID = 1;

{ maximum number of sources -- placed in root container, required }
const
	kParameterSourceCountName = FourCharCode('srcs');
	kParameterSourceCountID = 1;

{ EFFECT CLASSES}

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
const
	kEffectMajorClassType = FourCharCode('clsa');
	kEffectMajorClassID = 1;

const
	kGeneratorMajorClass = FourCharCode('genr'); { zero source effects}
	kFilterMajorClass = FourCharCode('filt'); { one source effects}
	kTransitionMajorClass = FourCharCode('tran'); { multisource morph effects }
	kCompositorMajorClass = FourCharCode('comp'); { multisource layer effects}
	kMiscMajorClass = FourCharCode('misc'); { all other effects}

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
const
	kEffectMinorClassType = FourCharCode('clsi');
	kEffectMinorClassID = 1;
	kEffectMinorClassNameType = FourCharCode('clsn');
	kEffectMinorClassNameID = 1;

const
	kGeneratorMinorClass = FourCharCode('genr'); { "Generators"}
	kRenderMinorClass = FourCharCode('rend'); { "Render"}
	kFilterMinorClass = FourCharCode('filt'); { "Filters"}
	kArtisticMinorClass = FourCharCode('arts'); { "Artistic}
	kBlurMinorClass = FourCharCode('blur'); { "Blur"}
	kSharpenMinorClass = FourCharCode('shrp'); { "Sharpen"}
	kDistortMinorClass = FourCharCode('dist'); { "Distort"}
	kNoiseMinorClass = FourCharCode('nois'); { "Noise"}
	kAdjustmentMinorClass = FourCharCode('adst'); { "Adjustments"}
	kTransitionMinorClass = FourCharCode('tran'); { "Transitions"}
	kWipeMinorClass = FourCharCode('wipe'); { "Wipes"}
	k3DMinorClass = FourCharCode('pzre'); { "3D Transitions"}
	kCompositorMinorClass = FourCharCode('comp'); { "Compositors"}
	kEffectsMinorClass = FourCharCode('fxfx'); { "Special Effects"}
	kMiscMinorClass = FourCharCode('misc'); { "Miscellaneous"}

{
   Effects can define a number of "preset" values which will be presented to the user
   in a simplified UI.  Each preset is an atom within the parameter description list
   and must have an atom ID from 1 going up sequentially.  Inside of this atom are three other
   atoms containing:
    1) the name of the preset as a Pascal string
    2) a preview picture for the preset, 86 x 64 pixels in size
    3) the ENTIRE set of parameter values needed to create a sample of this preset.
}
const
	kEffectPresetType = FourCharCode('peff');
	kPresetNameType = FourCharCode('pnam');
	kPresetNameID = 1;
	kPresetPreviewPictureType = FourCharCode('ppct');
	kPresetPreviewPictureID = 1;
	kPresetSettingsType = FourCharCode('psst');
	kPresetSettingsID = 1;

const
	kParameterDependencyName = FourCharCode('deep');
	kParameterDependencyID = 1;

const
	kParameterListDependsUponColorProfiles = FourCharCode('prof');
	kParameterListDependsUponFonts = FourCharCode('font');

type
	ParameterDependancyRecordPtr = ^ParameterDependancyRecord;
	ParameterDependancyRecord = record
		dependCount: SIGNEDLONG;
		depends: array [0..0] of OSType;
	end;
{
   enumeration list in container -- placed in root container, optional unless used by a
   parameter in the list
}
const
	kParameterEnumList = FourCharCode('enum');

type
	EnumValuePairPtr = ^EnumValuePair;
	EnumValuePair = record
		value: SIGNEDLONG;
		name: Str255;
	end;
type
	EnumListRecordPtr = ^EnumListRecord;
	EnumListRecord = record
		enumCount: SIGNEDLONG;              { number of enumeration items to follow}
		values: array [0..0] of EnumValuePair;              { values and names for them, packed }
	end;
{ atom type of parameter}
const
	kParameterAtomTypeAndID = FourCharCode('type');

const
	kNoAtom = FourCharCode('none'); { atom type for no data got/set}
	kAtomNoFlags = $00000000;
	kAtomNotInterpolated = $00000001; { atom can never be interpolated}
	kAtomInterpolateIsOptional = $00000002; { atom can be interpolated, but it is an advanced user operation}
	kAtomMayBeIndexed = $00000004; { more than one value of atom can exist with accending IDs (ie, lists of colors)}

type
	ParameterAtomTypeAndIDPtr = ^ParameterAtomTypeAndID;
	ParameterAtomTypeAndID = record
		atomType: QTAtomType;               { type of atom this data comes from/goes into}
		atomID: QTAtomID;                 { ID of atom this data comes from/goes into}
		atomFlags: SIGNEDLONG;              { options for this atom}
		atomName: Str255;               { name of this value type}
	end;
{ optional specification of mapping between parameters and properties}
const
	kParameterProperty = FourCharCode('prop');

type
	ParameterProperty = record
		propertyClass: OSType;          { class to set for this property (0 for default which is specified by caller)}
		propertyID: OSType;             { id to set for this property (default is the atomType)}
	end;
{ data type of a parameter}
const
	kParameterDataType = FourCharCode('data');

const
	kParameterTypeDataLong = kTweenTypeLong; { integer value}
	kParameterTypeDataFixed = kTweenTypeFixed; { fixed point value}
	kParameterTypeDataRGBValue = kTweenTypeRGBColor; { RGBColor data}
	kParameterTypeDataDouble = kTweenTypeQTFloatDouble; { IEEE 64 bit floating point value}
	kParameterTypeDataText = FourCharCode('text'); { editable text item}
	kParameterTypeDataEnum = FourCharCode('enum'); { enumerated lookup value}
	kParameterTypeDataBitField = FourCharCode('bool'); { bit field value (something that holds boolean(s))}
	kParameterTypeDataImage = FourCharCode('imag'); { reference to an image via Picture data}

type
	ParameterDataTypePtr = ^ParameterDataType;
	ParameterDataType = record
		dataType: OSType;               { type of data this item is stored as}
	end;
{
   alternate (optional) data type -- main data type always required.  
   Must be modified or deleted when modifying main data type.
   Main data type must be modified when alternate is modified.
}
const
	kParameterAlternateDataType = FourCharCode('alt1');
	kParameterTypeDataColorValue = FourCharCode('cmlr'); { CMColor data (supported on machines with ColorSync)}
	kParameterTypeDataCubic = FourCharCode('cubi'); { cubic bezier(s) (no built-in support)}
	kParameterTypeDataNURB = FourCharCode('nurb'); { nurb(s) (no built-in support)}

type
	ParameterAlternateDataEntryPtr = ^ParameterAlternateDataEntry;
	ParameterAlternateDataEntry = record
		dataType: OSType;               { type of data this item is stored as}
		alternateAtom: QTAtomType;          { where to store}
	end;
type
	ParameterAlternateDataTypePtr = ^ParameterAlternateDataType;
	ParameterAlternateDataType = record
		numEntries: SIGNEDLONG;
		entries: array [0..0] of ParameterAlternateDataEntry;
	end;
{ legal values for the parameter}
const
	kParameterDataRange = FourCharCode('rang');

const
	kNoMinimumLongFixed = $7FFFFFFF; { ignore minimum/maxiumum values}
	kNoMaximumLongFixed = $80000000;
	kNoScaleLongFixed = 0;    { don't perform any scaling of value}
	kNoPrecision = -1;  { allow as many digits as format}

{ 'text'}
type
	StringRangeRecordPtr = ^StringRangeRecord;
	StringRangeRecord = record
		maxChars: SIGNEDLONG;               { maximum length of string}
		maxLines: SIGNEDLONG;               { number of editing lines to use (1 typical, 0 to default)}
	end;
{ 'long'}
type
	LongRangeRecordPtr = ^LongRangeRecord;
	LongRangeRecord = record
		minValue: SIGNEDLONG;               { no less than this}
		maxValue: SIGNEDLONG;               { no more than this}
		scaleValue: SIGNEDLONG;             { muliply content by this going in, divide going out}
		precisionDigits: SIGNEDLONG;        { # digits of precision when editing via typing}
	end;
{ 'enum'}
type
	EnumRangeRecordPtr = ^EnumRangeRecord;
	EnumRangeRecord = record
		enumID: SIGNEDLONG;                 { 'enum' list in root container to search within}
	end;
{ 'fixd'}
type
	FixedRangeRecordPtr = ^FixedRangeRecord;
	FixedRangeRecord = record
		minValue: Fixed;               { no less than this}
		maxValue: Fixed;               { no more than this}
		scaleValue: Fixed;             { muliply content by this going in, divide going out}
		precisionDigits: SIGNEDLONG;        { # digits of precision when editing via typing}
	end;
{ 'doub'}


{$push}
{$r-}
{$q-}
const
  kNoMinimumDouble=0.0/0.0;                   { ignore minimum/maxiumum values }
  kNoMaximumDouble=0.0/0.0;
{$pop}
const
  kNoScaleDouble = 0.0;                     { don't perform any scaling of value }
type
	DoubleRangeRecordPtr = ^DoubleRangeRecord;
	DoubleRangeRecord = record
		minValue: QTFloatDouble;           { no less than this }
		maxValue: QTFloatDouble;           { no more than this }
		scaleValue: QTFloatDouble;         { muliply content by this going in, divide going out }
		precisionDigits: SIGNEDLONG;    { # digits of precision when editing via typing }
	end;
    
{ 'bool'   }
type
	BooleanRangeRecordPtr = ^BooleanRangeRecord;
	BooleanRangeRecord = record
		maskValue: SIGNEDLONG;              { value to mask on/off to set/clear the boolean}
	end;
{ 'rgb '}
type
	RGBRangeRecordPtr = ^RGBRangeRecord;
	RGBRangeRecord = record
		minColor: RGBColor;
		maxColor: RGBColor;
	end;
{ 'imag'}
const
	kParameterImageNoFlags = 0;
	kParameterImageIsPreset = 1;

const
	kStandardPresetGroup = FourCharCode('pset');

type
	ImageRangeRecordPtr = ^ImageRangeRecord;
	ImageRangeRecord = record
		imageFlags: SIGNEDLONG;
		fileType: OSType;               { file type to contain the preset group (normally kStandardPresetGroup)}
		replacedAtoms: SIGNEDLONG;          { # atoms at this level replaced by this preset group}
	end;
{ union of all of the above}

type
	ParameterRangeRecord = record
	  case SInt16 of
	    0: (
        longRange: LongRangeRecord;
        );
      1: (
        enumRange: EnumRangeRecord;
        );
      2: (
        fixedRange: FixedRangeRecord;
        );
      3: (
        doubleRange: DoubleRangeRecord;
        );
      4: (
        stringRange: StringRangeRecord;
        );
      5: (
        booleanRange: BooleanRangeRecord;
        );
      6: (
        rgbRange: RGBRangeRecord;
        );
      7: (
        imageRange: ImageRangeRecord;
        );
	end;
  
{ UI behavior of a parameter}
const
	kParameterDataBehavior = FourCharCode('ditl');

const
{ items edited via typing}
	kParameterItemEditText = FourCharCode('edit'); { edit text box}
	kParameterItemEditLong = FourCharCode('long'); { long number editing box}
	kParameterItemEditFixed = FourCharCode('fixd'); { fixed point number editing box}
	kParameterItemEditDouble = FourCharCode('doub'); { double number editing box}
                                        { items edited via control(s)}
	kParameterItemPopUp = FourCharCode('popu'); { pop up value for enum types}
	kParameterItemRadioCluster = FourCharCode('radi'); { radio cluster for enum types}
	kParameterItemCheckBox = FourCharCode('chex'); { check box for booleans}
	kParameterItemControl = FourCharCode('cntl'); { item controlled via a standard control of some type}
                                        { special user items}
	kParameterItemLine = FourCharCode('line'); { line}
	kParameterItemColorPicker = FourCharCode('pick'); { color swatch & picker}
	kParameterItemGroupDivider = FourCharCode('divi'); { start of a new group of items}
	kParameterItemStaticText = FourCharCode('stat'); { display "parameter name" as static text}
	kParameterItemDragImage = FourCharCode('imag'); { allow image display, along with drag and drop}
                                        { flags valid for lines and groups}
	kGraphicsNoFlags = $00000000; { no options for graphics}
	kGraphicsFlagsGray = $00000001; { draw lines with gray}
                                        { flags valid for groups}
	kGroupNoFlags = $00000000; { no options for group -- may be combined with graphics options             }
	kGroupAlignText = $00010000; { edit text items in group have the same size}
	kGroupSurroundBox = $00020000; { group should be surrounded with a box}
	kGroupMatrix = $00040000; { side-by-side arrangement of group is okay}
	kGroupNoName = $00080000; { name of group should not be displayed above box}
                                        { flags valid for popup/radiocluster/checkbox/control}
	kDisableControl = $00000001;
	kDisableWhenNotEqual = $00000000 + kDisableControl;
	kDisableWhenEqual = $00000010 + kDisableControl;
	kDisableWhenLessThan = $00000020 + kDisableControl;
	kDisableWhenGreaterThan = $00000030 + kDisableControl; { flags valid for controls}
	kCustomControl = $00100000; { flags valid for popups}
	kPopupStoreAsString = $00010000;

type
	ControlBehaviors = record
		groupID: QTAtomID;                { group under control of this item}
		controlValue: SIGNEDLONG;           { control value for comparison purposes}
		customType: QTAtomType;             { custom type identifier, for kCustomControl}
		customID: QTAtomID;               { custom type ID, for kCustomControl}
	end;
type
	ParameterDataBehaviorPtr = ^ParameterDataBehavior;
	ParameterDataBehavior = record
		behaviorType: OSType;
		behaviorFlags: SIGNEDLONG;
		case SInt16 of
			0: (
				controls: ControlBehaviors;
				);
	end;
{ higher level purpose of a parameter or set of parameters}
const
	kParameterDataUsage = FourCharCode('use ');

const
	kParameterUsagePixels = FourCharCode('pixl');
	kParameterUsageRectangle = FourCharCode('rect');
	kParameterUsagePoint = FourCharCode('xy  ');
	kParameterUsage3DPoint = FourCharCode('xyz ');
	kParameterUsageDegrees = FourCharCode('degr');
	kParameterUsageRadians = FourCharCode('rads');
	kParameterUsagePercent = FourCharCode('pcnt');
	kParameterUsageSeconds = FourCharCode('secs');
	kParameterUsageMilliseconds = FourCharCode('msec');
	kParameterUsageMicroseconds = FourCharCode('µsec');
	kParameterUsage3by3Matrix = FourCharCode('3by3');
	kParameterUsageCircularDegrees = FourCharCode('degc');
	kParameterUsageCircularRadians = FourCharCode('radc');

type
	ParameterDataUsagePtr = ^ParameterDataUsage;
	ParameterDataUsage = record
		usageType: OSType;              { higher level purpose of the data or group}
	end;
{ default value(s) for a parameter}
const
	kParameterDataDefaultItem = FourCharCode('dflt');

	{	 atoms that help to fill in data within the info window 	}
	kParameterInfoLongName = FourCharCode('©nam');
	kParameterInfoCopyright = FourCharCode('©cpy');
	kParameterInfoDescription	= FourCharCode('©inf');
	kParameterInfoWindowTitle	= FourCharCode('©wnt');
	kParameterInfoPicture = FourCharCode('©pix');
	kParameterInfoManufacturer = FourCharCode('©man');
	kParameterInfoIDs = 1;

	{ flags for ImageCodecValidateParameters }
	kParameterValidationNoFlags = $00000000;
	kParameterValidationFinalValidation = $00000001;


type
	QTParameterValidationOptions = SIGNEDLONG;
{ QTAtomTypes for atoms in image compressor settings containers}
const
	kImageCodecSettingsFieldCount = FourCharCode('fiel'); { Number of fields (UInt8) }
	kImageCodecSettingsFieldOrdering = FourCharCode('fdom'); { Ordering of fields (UInt8)}
	kImageCodecSettingsFieldOrderingF1F2 = 1;
	kImageCodecSettingsFieldOrderingF2F1 = 2;


{
 *  Summary:
 *    Additional Image Description Extensions
 }
const
{
   * Image description extension describing the color properties.
   }
	kColorInfoImageDescriptionExtension = FourCharCode('colr');

  {
   * Image description extension describing the pixel aspect ratio.
   }
	kPixelAspectRatioImageDescriptionExtension = FourCharCode('pasp'); { big-endian PixelAspectRatioImageDescriptionExtension }

  {
   * Image description extension describing the clean aperture.
   }
	kCleanApertureImageDescriptionExtension = FourCharCode('clap'); { big-endian CleanApertureImageDescriptionExtension }

  {
   * Specifies the offset in bytes from the start of one pixel row to
   * the next. Only valid for chunky pixel formats. If present, this
   * image description extension overrides other conventions for
   * calculating rowBytes.
   }
	kQTRowBytesImageDescriptionExtension = FourCharCode('rowb'); { big-endian SInt32 }

{ Color Info Image Description Extension types}
const
	kVideoColorInfoImageDescriptionExtensionType = FourCharCode('nclc'); { For video color descriptions (defined below)    }
	kICCProfileColorInfoImageDescriptionExtensionType = FourCharCode('prof'); { For ICC Profile color descriptions (not defined here)}


{ Video Color Info Image Description Extensions}
type
	NCLCColorInfoImageDescriptionExtension = record
		colorParamType: OSType;         { Type of color parameter 'nclc'               }
		primaries: UInt16;              { CIE 1931 xy chromaticity coordinates          }
		transferFunction: UInt16;       { Nonlinear transfer function from RGB to ErEgEb }
		matrix: UInt16;                 { Matrix from ErEgEb to EyEcbEcr           }
	end;
{ Primaries}
const
	kQTPrimaries_ITU_R709_2 = 1;    { ITU-R BT.709-2, SMPTE 274M-1995, and SMPTE 296M-1997 }
	kQTPrimaries_Unknown = 2;    { Unknown }
	kQTPrimaries_EBU_3213 = 5;    { EBU Tech. 3213 (1981) }
	kQTPrimaries_SMPTE_C = 6;     { SMPTE C Primaries from SMPTE RP 145-1993 }

{ Transfer Function}
const
	kQTTransferFunction_ITU_R709_2 = 1;   { Recommendation ITU-R BT.709-2, SMPTE 274M-1995, SMPTE 296M-1997, SMPTE 293M-1996 and SMPTE 170M-1994 }
	kQTTransferFunction_Unknown = 2;    { Unknown }
	kQTTransferFunction_SMPTE_240M_1995 = 7; { SMPTE 240M-1995 and interim color implementation of SMPTE 274M-1995 }

{ Matrix}
const
	kQTMatrix_ITU_R_709_2 = 1;    { Recommendation ITU-R BT.709-2 (1125/60/2:1 only), SMPTE 274M-1995 and SMPTE 296M-1997 }
	kQTMatrix_Unknown = 2;    { Unknown }
	kQTMatrix_ITU_R_601_4 = 6;    { Recommendation ITU-R BT.601-4, Recommendation ITU-R BT.470-4 System B and G, SMPTE 170M-1994 and SMPTE 293M-1996 }
	kQTMatrix_SMPTE_240M_1995 = 7;     { SMPTE 240M-1995 and interim color implementation of SMPTE 274M-1995 }


{ Field/Frame Info Image Description (this remaps to FieldInfoImageDescriptionExtension)}
type
	FieldInfoImageDescriptionExtension2Ptr = ^FieldInfoImageDescriptionExtension2;
	FieldInfoImageDescriptionExtension2 = record
		fields: UInt8;
		detail: UInt8;
	end;
const
	kQTFieldsProgressiveScan = 1;
	kQTFieldsInterlaced = 2;

const
	kQTFieldDetailUnknown = 0;
	kQTFieldDetailTemporalTopFirst = 1;
	kQTFieldDetailTemporalBottomFirst = 6;
	kQTFieldDetailSpatialFirstLineEarly = 9;
	kQTFieldDetailSpatialFirstLineLate = 14;


{ Pixel Aspect Ratio Image Description Extensions}
type
	PixelAspectRatioImageDescriptionExtensionPtr = ^PixelAspectRatioImageDescriptionExtension;
	PixelAspectRatioImageDescriptionExtension = record
		hSpacing: UInt32;               { Horizontal Spacing }
		vSpacing: UInt32;               { Vertical Spacing }
	end;
{ Clean Aperture Image Description Extensions}
type
	CleanApertureImageDescriptionExtensionPtr = ^CleanApertureImageDescriptionExtension;
	CleanApertureImageDescriptionExtension = record
		cleanApertureWidthN: UInt32;    { width of clean aperture, numerator, denominator }
		cleanApertureWidthD: UInt32;
		cleanApertureHeightN: UInt32;   { height of clean aperture, numerator, denominator}
		cleanApertureHeightD: UInt32;
		horizOffN: SInt32;              { horizontal offset of clean aperture center minus (width-1)/2, numerator, denominator }
		horizOffD: UInt32;
		vertOffN: SInt32;               { vertical offset of clean aperture center minus (height-1)/2, numerator, denominator }
		vertOffD: UInt32;
	end;
type
	ImageCodecMPDrawBandProcPtr = function( refcon: UnivPtr; var drp: ImageSubCodecDecompressRecord ): ComponentResult;
	ImageCodecMPDrawBandUPP = ImageCodecMPDrawBandProcPtr;
{
 *  NewImageCodecMPDrawBandUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function NewImageCodecMPDrawBandUPP( userRoutine: ImageCodecMPDrawBandProcPtr ): ImageCodecMPDrawBandUPP; external name '_NewImageCodecMPDrawBandUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  DisposeImageCodecMPDrawBandUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure DisposeImageCodecMPDrawBandUPP( userUPP: ImageCodecMPDrawBandUPP ); external name '_DisposeImageCodecMPDrawBandUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  InvokeImageCodecMPDrawBandUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function InvokeImageCodecMPDrawBandUPP( refcon: UnivPtr; var drp: ImageSubCodecDecompressRecord; userUPP: ImageCodecMPDrawBandUPP ): ComponentResult; external name '_InvokeImageCodecMPDrawBandUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{  codec selectors 0-127 are reserved by Apple }
{  codec selectors 128-191 are subtype specific }
{  codec selectors 192-255 are vendor specific }
{  codec selectors 256-511 are available for general use }
{  codec selectors 512-1023 are reserved by Apple }
{  codec selectors 1024-32767 are available for general use }
{  negative selectors are reserved by the Component Manager }
{
 *  ImageCodecGetCodecInfo()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageCodecGetCodecInfo( ci: ComponentInstance; var info: CodecInfo ): ComponentResult; external name '_ImageCodecGetCodecInfo';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  ImageCodecGetCompressionTime()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageCodecGetCompressionTime( ci: ComponentInstance; src: PixMapHandle; const (*var*) srcRect: Rect; depth: SInt16; var spatialQuality: CodecQ; var temporalQuality: CodecQ; var time: UNSIGNEDLONG ): ComponentResult; external name '_ImageCodecGetCompressionTime';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  ImageCodecGetMaxCompressionSize()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageCodecGetMaxCompressionSize( ci: ComponentInstance; src: PixMapHandle; const (*var*) srcRect: Rect; depth: SInt16; quality: CodecQ; var size: SIGNEDLONG ): ComponentResult; external name '_ImageCodecGetMaxCompressionSize';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  ImageCodecPreCompress()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageCodecPreCompress( ci: ComponentInstance; var params: CodecCompressParams ): ComponentResult; external name '_ImageCodecPreCompress';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  ImageCodecBandCompress()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageCodecBandCompress( ci: ComponentInstance; var params: CodecCompressParams ): ComponentResult; external name '_ImageCodecBandCompress';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  ImageCodecPreDecompress()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageCodecPreDecompress( ci: ComponentInstance; var params: CodecDecompressParams ): ComponentResult; external name '_ImageCodecPreDecompress';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  ImageCodecBandDecompress()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageCodecBandDecompress( ci: ComponentInstance; var params: CodecDecompressParams ): ComponentResult; external name '_ImageCodecBandDecompress';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  ImageCodecBusy()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageCodecBusy( ci: ComponentInstance; seq: ImageSequence ): ComponentResult; external name '_ImageCodecBusy';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  ImageCodecGetCompressedImageSize()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageCodecGetCompressedImageSize( ci: ComponentInstance; desc: ImageDescriptionHandle; data: Ptr; bufferSize: SIGNEDLONG; dataProc: ICMDataProcRecordPtr; var dataSize: SIGNEDLONG ): ComponentResult; external name '_ImageCodecGetCompressedImageSize';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  ImageCodecGetSimilarity()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageCodecGetSimilarity( ci: ComponentInstance; src: PixMapHandle; const (*var*) srcRect: Rect; desc: ImageDescriptionHandle; data: Ptr; var similarity: Fixed ): ComponentResult; external name '_ImageCodecGetSimilarity';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  ImageCodecTrimImage()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageCodecTrimImage( ci: ComponentInstance; Desc: ImageDescriptionHandle; inData: Ptr; inBufferSize: SIGNEDLONG; dataProc: ICMDataProcRecordPtr; outData: Ptr; outBufferSize: SIGNEDLONG; flushProc: ICMFlushProcRecordPtr; var trimRect: Rect; progressProc: ICMProgressProcRecordPtr ): ComponentResult; external name '_ImageCodecTrimImage';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  ImageCodecRequestSettings()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageCodecRequestSettings( ci: ComponentInstance; settings: Handle; var rp: Rect; filterProc: ModalFilterUPP ): ComponentResult; external name '_ImageCodecRequestSettings';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  ImageCodecGetSettings()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageCodecGetSettings( ci: ComponentInstance; settings: Handle ): ComponentResult; external name '_ImageCodecGetSettings';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  ImageCodecSetSettings()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageCodecSetSettings( ci: ComponentInstance; settings: Handle ): ComponentResult; external name '_ImageCodecSetSettings';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  ImageCodecFlush()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageCodecFlush( ci: ComponentInstance ): ComponentResult; external name '_ImageCodecFlush';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  ImageCodecSetTimeCode()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageCodecSetTimeCode( ci: ComponentInstance; timeCodeFormat: UnivPtr; timeCodeTime: UnivPtr ): ComponentResult; external name '_ImageCodecSetTimeCode';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  ImageCodecIsImageDescriptionEquivalent()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageCodecIsImageDescriptionEquivalent( ci: ComponentInstance; newDesc: ImageDescriptionHandle; var equivalent: Boolean ): ComponentResult; external name '_ImageCodecIsImageDescriptionEquivalent';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  ImageCodecNewMemory()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageCodecNewMemory( ci: ComponentInstance; var data: Ptr; dataSize: Size; dataUse: SIGNEDLONG; memoryGoneProc: ICMMemoryDisposedUPP; refCon: UnivPtr ): ComponentResult; external name '_ImageCodecNewMemory';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  ImageCodecDisposeMemory()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageCodecDisposeMemory( ci: ComponentInstance; data: Ptr ): ComponentResult; external name '_ImageCodecDisposeMemory';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  ImageCodecHitTestData()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageCodecHitTestData( ci: ComponentInstance; desc: ImageDescriptionHandle; data: UnivPtr; dataSize: Size; where: Point; var hit: Boolean ): ComponentResult; external name '_ImageCodecHitTestData';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  ImageCodecNewImageBufferMemory()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageCodecNewImageBufferMemory( ci: ComponentInstance; var params: CodecDecompressParams; flags: SIGNEDLONG; memoryGoneProc: ICMMemoryDisposedUPP; refCon: UnivPtr ): ComponentResult; external name '_ImageCodecNewImageBufferMemory';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  ImageCodecExtractAndCombineFields()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageCodecExtractAndCombineFields( ci: ComponentInstance; fieldFlags: SIGNEDLONG; data1: UnivPtr; dataSize1: SIGNEDLONG; desc1: ImageDescriptionHandle; data2: UnivPtr; dataSize2: SIGNEDLONG; desc2: ImageDescriptionHandle; outputData: UnivPtr; var outDataSize: SIGNEDLONG; descOut: ImageDescriptionHandle ): ComponentResult; external name '_ImageCodecExtractAndCombineFields';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  ImageCodecGetMaxCompressionSizeWithSources()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageCodecGetMaxCompressionSizeWithSources( ci: ComponentInstance; src: PixMapHandle; const (*var*) srcRect: Rect; depth: SInt16; quality: CodecQ; sourceData: CDSequenceDataSourcePtr; var size: SIGNEDLONG ): ComponentResult; external name '_ImageCodecGetMaxCompressionSizeWithSources';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  ImageCodecSetTimeBase()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageCodecSetTimeBase( ci: ComponentInstance; base: UnivPtr ): ComponentResult; external name '_ImageCodecSetTimeBase';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  ImageCodecSourceChanged()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageCodecSourceChanged( ci: ComponentInstance; majorSourceChangeSeed: UInt32; minorSourceChangeSeed: UInt32; sourceData: CDSequenceDataSourcePtr; var flagsOut: SIGNEDLONG ): ComponentResult; external name '_ImageCodecSourceChanged';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  ImageCodecFlushFrame()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageCodecFlushFrame( ci: ComponentInstance; flags: UInt32 ): ComponentResult; external name '_ImageCodecFlushFrame';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  ImageCodecGetSettingsAsText()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageCodecGetSettingsAsText( ci: ComponentInstance; var text: Handle ): ComponentResult; external name '_ImageCodecGetSettingsAsText';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  ImageCodecGetParameterListHandle()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageCodecGetParameterListHandle( ci: ComponentInstance; var parameterDescriptionHandle: Handle ): ComponentResult; external name '_ImageCodecGetParameterListHandle';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  ImageCodecGetParameterList()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageCodecGetParameterList( ci: ComponentInstance; var parameterDescription: QTAtomContainer ): ComponentResult; external name '_ImageCodecGetParameterList';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  ImageCodecCreateStandardParameterDialog()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageCodecCreateStandardParameterDialog( ci: ComponentInstance; parameterDescription: QTAtomContainer; parameters: QTAtomContainer; dialogOptions: QTParameterDialogOptions; existingDialog: DialogPtr; existingUserItem: SInt16; var createdDialog: QTParameterDialog ): ComponentResult; external name '_ImageCodecCreateStandardParameterDialog';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  ImageCodecIsStandardParameterDialogEvent()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageCodecIsStandardParameterDialogEvent( ci: ComponentInstance; var pEvent: EventRecord; createdDialog: QTParameterDialog ): ComponentResult; external name '_ImageCodecIsStandardParameterDialogEvent';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  ImageCodecDismissStandardParameterDialog()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageCodecDismissStandardParameterDialog( ci: ComponentInstance; createdDialog: QTParameterDialog ): ComponentResult; external name '_ImageCodecDismissStandardParameterDialog';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  ImageCodecStandardParameterDialogDoAction()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageCodecStandardParameterDialogDoAction( ci: ComponentInstance; createdDialog: QTParameterDialog; action: SIGNEDLONG; params: UnivPtr ): ComponentResult; external name '_ImageCodecStandardParameterDialogDoAction';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  ImageCodecNewImageGWorld()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageCodecNewImageGWorld( ci: ComponentInstance; var params: CodecDecompressParams; var newGW: GWorldPtr; flags: SIGNEDLONG ): ComponentResult; external name '_ImageCodecNewImageGWorld';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  ImageCodecDisposeImageGWorld()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageCodecDisposeImageGWorld( ci: ComponentInstance; theGW: GWorldPtr ): ComponentResult; external name '_ImageCodecDisposeImageGWorld';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  ImageCodecHitTestDataWithFlags()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageCodecHitTestDataWithFlags( ci: ComponentInstance; desc: ImageDescriptionHandle; data: UnivPtr; dataSize: Size; where: Point; var hit: SIGNEDLONG; hitFlags: SIGNEDLONG ): ComponentResult; external name '_ImageCodecHitTestDataWithFlags';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  ImageCodecValidateParameters()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageCodecValidateParameters( ci: ComponentInstance; parameters: QTAtomContainer; validationFlags: QTParameterValidationOptions; errorString: StringPtr ): ComponentResult; external name '_ImageCodecValidateParameters';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  ImageCodecGetBaseMPWorkFunction()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageCodecGetBaseMPWorkFunction( ci: ComponentInstance; var workFunction: ComponentMPWorkFunctionUPP; var refCon: UnivPtr; drawProc: ImageCodecMPDrawBandUPP; drawProcRefCon: UnivPtr ): ComponentResult; external name '_ImageCodecGetBaseMPWorkFunction';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  ImageCodecLockBits()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 (or QuickTime 6.4) and later in QuickTime.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 *    Windows:          in qtmlClient.lib 6.5 and later
 }
function ImageCodecLockBits( ci: ComponentInstance; port: CGrafPtr ): ComponentResult; external name '_ImageCodecLockBits';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)


{
 *  ImageCodecUnlockBits()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 (or QuickTime 6.4) and later in QuickTime.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 *    Windows:          in qtmlClient.lib 6.5 and later
 }
function ImageCodecUnlockBits( ci: ComponentInstance; port: CGrafPtr ): ComponentResult; external name '_ImageCodecUnlockBits';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)


{
 *  ImageCodecRequestGammaLevel()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Non-Carbon CFM:   in QuickTimeLib 5.0 and later
 *    Windows:          in qtmlClient.lib 5.0 and later
 }
function ImageCodecRequestGammaLevel( ci: ComponentInstance; srcGammaLevel: Fixed; dstGammaLevel: Fixed; var codecCanMatch: SIGNEDLONG ): ComponentResult; external name '_ImageCodecRequestGammaLevel';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  ImageCodecGetSourceDataGammaLevel()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Non-Carbon CFM:   in QuickTimeLib 5.0 and later
 *    Windows:          in qtmlClient.lib 5.0 and later
 }
function ImageCodecGetSourceDataGammaLevel( ci: ComponentInstance; var sourceDataGammaLevel: Fixed ): ComponentResult; external name '_ImageCodecGetSourceDataGammaLevel';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{ (Selector 42 skipped) }
{
 *  ImageCodecGetDecompressLatency()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Non-Carbon CFM:   in QuickTimeLib 5.0 and later
 *    Windows:          in qtmlClient.lib 5.0 and later
 }
function ImageCodecGetDecompressLatency( ci: ComponentInstance; var latency: TimeRecord ): ComponentResult; external name '_ImageCodecGetDecompressLatency';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  ImageCodecMergeFloatingImageOntoWindow()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Non-Carbon CFM:   in QuickTimeLib 6.0 and later
 *    Windows:          in qtmlClient.lib 6.0 and later
 }
function ImageCodecMergeFloatingImageOntoWindow( ci: ComponentInstance; flags: UInt32 ): ComponentResult; external name '_ImageCodecMergeFloatingImageOntoWindow';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{
 *  ImageCodecRemoveFloatingImage()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Non-Carbon CFM:   in QuickTimeLib 6.0 and later
 *    Windows:          in qtmlClient.lib 6.0 and later
 }
function ImageCodecRemoveFloatingImage( ci: ComponentInstance; flags: UInt32 ): ComponentResult; external name '_ImageCodecRemoveFloatingImage';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{
 *  ImageCodecGetDITLForSize()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Non-Carbon CFM:   in QuickTimeLib 6.0 and later
 *    Windows:          in qtmlClient.lib 6.0 and later
 }
function ImageCodecGetDITLForSize( ci: ComponentInstance; var ditl: Handle; var requestedSize: Point ): ComponentResult; external name '_ImageCodecGetDITLForSize';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{
 *  ImageCodecDITLInstall()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Non-Carbon CFM:   in QuickTimeLib 6.0 and later
 *    Windows:          in qtmlClient.lib 6.0 and later
 }
function ImageCodecDITLInstall( ci: ComponentInstance; d: DialogRef; itemOffset: SInt16 ): ComponentResult; external name '_ImageCodecDITLInstall';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{
 *  ImageCodecDITLEvent()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Non-Carbon CFM:   in QuickTimeLib 6.0 and later
 *    Windows:          in qtmlClient.lib 6.0 and later
 }
function ImageCodecDITLEvent( ci: ComponentInstance; d: DialogRef; itemOffset: SInt16; const (*var*) theEvent: EventRecord; var itemHit: SInt16; var handled: Boolean ): ComponentResult; external name '_ImageCodecDITLEvent';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{
 *  ImageCodecDITLItem()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Non-Carbon CFM:   in QuickTimeLib 6.0 and later
 *    Windows:          in qtmlClient.lib 6.0 and later
 }
function ImageCodecDITLItem( ci: ComponentInstance; d: DialogRef; itemOffset: SInt16; itemNum: SInt16 ): ComponentResult; external name '_ImageCodecDITLItem';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{
 *  ImageCodecDITLRemove()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Non-Carbon CFM:   in QuickTimeLib 6.0 and later
 *    Windows:          in qtmlClient.lib 6.0 and later
 }
function ImageCodecDITLRemove( ci: ComponentInstance; d: DialogRef; itemOffset: SInt16 ): ComponentResult; external name '_ImageCodecDITLRemove';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{
 *  ImageCodecDITLValidateInput()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Non-Carbon CFM:   in QuickTimeLib 6.0 and later
 *    Windows:          in qtmlClient.lib 6.0 and later
 }
function ImageCodecDITLValidateInput( ci: ComponentInstance; var ok: Boolean ): ComponentResult; external name '_ImageCodecDITLValidateInput';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{ (Selector 52 skipped) }
{ (Selector 53 skipped) }
{
 *  ImageCodecGetPreferredChunkSizeAndAlignment()
 *  
 *  Summary:
 *    Returns the preferences of an image decompressor for the chunking
 *    of image data within a container, e.g. a movie file.
 *  
 *  Discussion:
 *    If you are writing image data to a container, you can optimize
 *    the subsequent loading of the image data for playback and other
 *    operations by chunking multiple samples of image data together.
 *    This function can be called to determine whether an image
 *    decompressor has special chunking preferences.
 *  
 *  Parameters:
 *    
 *    ci:
 *      Component instance / instance globals.
 *    
 *    desc:
 *      The image description for the image data to be chunked.
 *    
 *    frameRate:
 *      Mean frame rate in frames per second as in SCTemporalSettings;
 *      0 if not known or not applicable.
 *    
 *    dataRate:
 *      Mean data rate in bytes per second as in SCDataRateSettings; 0
 *      if not known.
 *    
 *    bytesPerChunk:
 *      Points to a variable to receive the preferred maximum size in
 *      bytes of each chunk of image data. It is not safe to pass NULL
 *      for this parameter. The codec may indicate that it has no
 *      preference regarding chunk sizing by setting the variable to 0.
 *    
 *    alignment:
 *      Points to a variable to receive the preferred boundary for
 *      chunk alignment in bytes, e.g. 512. It is not safe to pass NULL
 *      for this parameter. The codec may indicate that it has no
 *      preference regarding chunk alignment by setting the variable to
 *      0.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 (or QuickTime 7.0) and later in QuickTime.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function ImageCodecGetPreferredChunkSizeAndAlignment( ci: ComponentInstance; desc: ImageDescriptionHandle; frameRate: Fixed; dataRate: UInt32; var bytesPerChunk: SIGNEDLONG; var alignment: SIGNEDLONG ): ComponentResult; external name '_ImageCodecGetPreferredChunkSizeAndAlignment';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ B-Frame Capable Image Compressor Component API }
{
 *  ImageCodecPrepareToCompressFrames()
 *  
 *  Summary:
 *    Prepares the compressor to receive frames.
 *  
 *  Discussion:
 *    The compressor should record session and retain
 *    compressionSessionOptions for use in later calls. 
 *    The compressor may modify imageDescription at this point. 
 *     The compressor should create and return pixel buffer attributes,
 *    which the ICM will release. 
 *    (Note: this replaces ImageCodecPreCompress.)
 *  
 *  Parameters:
 *    
 *    ci:
 *      Component instance / instance globals.
 *    
 *    session:
 *      The compressor session reference. The compressor should store
 *      this in its globals; it will need it when calling the ICM back
 *      (eg, to call ICMEncodedFrameCreateMutable and
 *      ICMCompressorSessionEmitEncodedFrame). 
 *      This is not a CF type. Do not call CFRetain or CFRelease on it.
 *    
 *    compressionSessionOptions:
 *      The session options from the client. The compressor should
 *      retain this and use the settings to guide compression.
 *    
 *    imageDescription:
 *      The image description. The compressor may add image description
 *      extensions.
 *    
 *    reserved:
 *      Reserved for future use.  Ignore this parameter.
 *    
 *    compressorPixelBufferAttributesOut:
 *      The compressor should create a pixel buffer attributes
 *      dictionary and set compressorPixelBufferAttributesOut to it. 
 *      The ICM will release it.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 (or QuickTime 7.0) and later in QuickTime.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function ImageCodecPrepareToCompressFrames( ci: ComponentInstance; session: ICMCompressorSessionRef; compressionSessionOptions: ICMCompressionSessionOptionsRef; imageDescription: ImageDescriptionHandle; reserved: UnivPtr; var compressorPixelBufferAttributesOut: CFDictionaryRef ): ComponentResult; external name '_ImageCodecPrepareToCompressFrames';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{
 *  ImageCodecEncodeFrame()
 *  
 *  Summary:
 *    Presents the compressor with a frame to encode.
 *  
 *  Discussion:
 *    The compressor may encode the frame immediately or queue it for
 *    later encoding. If the compressor queues the frame for later
 *    decode, it must retain it (by calling
 *    ICMCompressorSourceFrameRetain) and release it when it is done
 *    with it (by calling ICMCompressorSourceFrameRelease). 
 *    Pixel buffers are guaranteed to conform to the pixel buffer
 *    attributes returned by ImageCodecPrepareToCompressFrames. 
 *     During multipass encoding, if the compressor requested the
 *    kICMCompressionPassMode_NoSourceFrames flag, the source frame
 *    pixel buffers may be NULL. 
 *    (Note: this replaces ImageCodecBandCompress.)
 *  
 *  Parameters:
 *    
 *    ci:
 *      Component instance / instance globals.
 *    
 *    sourceFrame:
 *      The source frame to encode.
 *    
 *    flags:
 *      Reserved; ignore.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 (or QuickTime 7.0) and later in QuickTime.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function ImageCodecEncodeFrame( ci: ComponentInstance; sourceFrame: ICMCompressorSourceFrameRef; flags: UInt32 ): ComponentResult; external name '_ImageCodecEncodeFrame';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{
 *  ImageCodecCompleteFrame()
 *  
 *  Summary:
 *    Directs the compressor to finish with a queued source frame,
 *    either emitting or dropping it.
 *  
 *  Discussion:
 *    This frame does not necessarily need to be the first or only
 *    source frame emitted or dropped during this call, but the
 *    compressor must call either ICMCompressorSessionDropFrame or
 *    ICMCompressorSessionEmitEncodedFrame with this frame before
 *    returning. 
 *    The ICM will call this function to force frames to be encoded for
 *    the following reasons: (a) the maximum frame delay count or
 *    maximum frame delay time in the compressionSessionOptions does
 *    not permit frames to be queued; (b) the client has called
 *    ICMCompressionSessionCompleteFrames.
 *  
 *  Parameters:
 *    
 *    ci:
 *      Component instance / instance globals.
 *    
 *    sourceFrame:
 *      The source frame that must be completed.
 *    
 *    flags:
 *      Reserved; ignore.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 (or QuickTime 7.0) and later in QuickTime.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function ImageCodecCompleteFrame( ci: ComponentInstance; sourceFrame: ICMCompressorSourceFrameRef; flags: UInt32 ): ComponentResult; external name '_ImageCodecCompleteFrame';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{
 *  ImageCodecBeginPass()
 *  
 *  Summary:
 *    Notifies the compressor that it should operate in multipass mode
 *    and use the given multipass storage.
 *  
 *  Parameters:
 *    
 *    ci:
 *      Component instance / instance globals.
 *    
 *    passModeFlags:
 *      Indicates how the compressor should operate in this pass. 
 *       If the kICMCompressionPassMode_WriteToMultiPassStorage flag is
 *      set, the compressor may gather information of interest and
 *      store it in multiPassStorage. 
 *      If the kICMCompressionPassMode_ReadFromMultiPassStorage flag is
 *      set, the compressor may retrieve information from
 *      multiPassStorage. 
 *      If the kICMCompressionPassMode_OutputEncodedFrames flag is set,
 *      the compressor must encode or drop every frame by calling
 *      ICMCompressorSessionDropFrame or
 *      ICMCompressorSessionEmitEncodedFrame. If that flag is not set,
 *      the compressor should not call these routines.
 *    
 *    flags:
 *      Reserved.  Ignore this parameter.
 *    
 *    multiPassStorage:
 *      The multipass storage object that the compressor should use to
 *      store and retrieve information between passes.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 (or QuickTime 7.0) and later in QuickTime.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function ImageCodecBeginPass( ci: ComponentInstance; passModeFlags: ICMCompressionPassModeFlags; flags: UInt32; multiPassStorage: ICMMultiPassStorageRef ): ComponentResult; external name '_ImageCodecBeginPass';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{
 *  ImageCodecEndPass()
 *  
 *  Summary:
 *    Notifies the compressor that a pass is over.
 *  
 *  Parameters:
 *    
 *    ci:
 *      Component instance / instance globals.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 (or QuickTime 7.0) and later in QuickTime.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function ImageCodecEndPass( ci: ComponentInstance ): ComponentResult; external name '_ImageCodecEndPass';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{
 *  ImageCodecProcessBetweenPasses()
 *  
 *  Summary:
 *    Gives the compressor an opportunity to perform processing between
 *    passes.
 *  
 *  Discussion:
 *    This function will be called repeatedly until it returns true in
 *    *interpassProcessingDoneOut. 
 *    The compressor may read and write to multiPassStorage. 
 *    The compressor should indicate which type of pass it would prefer
 *    to perform next by setting *requestedNextPassTypeOut.
 *  
 *  Parameters:
 *    
 *    ci:
 *      Component instance / instance globals.
 *    
 *    multiPassStorage:
 *      The multipass storage object that the compressor should use to
 *      store and retrieve information between passes.
 *    
 *    interpassProcessingDoneOut:
 *      Points to a Boolean. Set this to false if you want your
 *      ImageCodecProcessBetweenPasses function to be called again to
 *      perform more processing, true if not.
 *    
 *    requestedNextPassModeFlagsOut:
 *      Set *requestedNextPassModeFlagsOut to indicate the type of pass
 *      that should be performed next: 
 *      To recommend a repeated analysis pass, set it to
 *      kICMCompressionPassMode_ReadFromMultiPassStorage |
 *      kICMCompressionPassMode_WriteToMultiPassStorage. 
 *      To recommend a final encoding pass, set it to
 *      kICMCompressionPassMode_ReadFromMultiPassStorage |
 *      kICMCompressionPassMode_OutputEncodedFrames. 
 *      If source frame buffers are not necessary for the recommended
 *      pass (eg, because all the required data has been copied into
 *      multipass storage), set kICMCompressionPassMode_NoSourceFrames.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 (or QuickTime 7.0) and later in QuickTime.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function ImageCodecProcessBetweenPasses( ci: ComponentInstance; multiPassStorage: ICMMultiPassStorageRef; var interpassProcessingDoneOut: Boolean; var requestedNextPassModeFlagsOut: ICMCompressionPassModeFlags ): ComponentResult; external name '_ImageCodecProcessBetweenPasses';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ (Selector 61 skipped) }
{ (Selector 62 skipped) }
{ (Selector 63 skipped) }
{ (Selector 64 skipped) }
{ (Selector 65 skipped) }
{ (Selector 66 skipped) }
{
 *  ImageCodecPreflight()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageCodecPreflight( ci: ComponentInstance; var params: CodecDecompressParams ): ComponentResult; external name '_ImageCodecPreflight';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  ImageCodecInitialize()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageCodecInitialize( ci: ComponentInstance; var cap: ImageSubCodecDecompressCapabilities ): ComponentResult; external name '_ImageCodecInitialize';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  ImageCodecBeginBand()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageCodecBeginBand( ci: ComponentInstance; var params: CodecDecompressParams; var drp: ImageSubCodecDecompressRecord; flags: SIGNEDLONG ): ComponentResult; external name '_ImageCodecBeginBand';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  ImageCodecDrawBand()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageCodecDrawBand( ci: ComponentInstance; var drp: ImageSubCodecDecompressRecord ): ComponentResult; external name '_ImageCodecDrawBand';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  ImageCodecEndBand()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageCodecEndBand( ci: ComponentInstance; var drp: ImageSubCodecDecompressRecord; result: OSErr; flags: SIGNEDLONG ): ComponentResult; external name '_ImageCodecEndBand';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  ImageCodecQueueStarting()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageCodecQueueStarting( ci: ComponentInstance ): ComponentResult; external name '_ImageCodecQueueStarting';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  ImageCodecQueueStopping()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageCodecQueueStopping( ci: ComponentInstance ): ComponentResult; external name '_ImageCodecQueueStopping';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  ImageCodecDroppingFrame()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageCodecDroppingFrame( ci: ComponentInstance; const (*var*) drp: ImageSubCodecDecompressRecord ): ComponentResult; external name '_ImageCodecDroppingFrame';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  ImageCodecScheduleFrame()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function ImageCodecScheduleFrame( ci: ComponentInstance; const (*var*) drp: ImageSubCodecDecompressRecord; triggerProc: ImageCodecTimeTriggerUPP; triggerProcRefCon: UnivPtr ): ComponentResult; external name '_ImageCodecScheduleFrame';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  ImageCodecCancelTrigger()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function ImageCodecCancelTrigger( ci: ComponentInstance ): ComponentResult; external name '_ImageCodecCancelTrigger';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  ImageCodecDecodeBand()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 (or QuickTime 7.0) and later in QuickTime.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 *    Windows:          in qtmlClient.lib version 10.4 (or QuickTime 7.0) and later
 }
function ImageCodecDecodeBand( ci: ComponentInstance; var drp: ImageSubCodecDecompressRecord; flags: UNSIGNEDLONG ): ComponentResult; external name '_ImageCodecDecodeBand';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ selectors for component calls }
const
	kImageCodecGetCodecInfoSelect = $0000;
	kImageCodecGetCompressionTimeSelect = $0001;
	kImageCodecGetMaxCompressionSizeSelect = $0002;
	kImageCodecPreCompressSelect = $0003;
	kImageCodecBandCompressSelect = $0004;
	kImageCodecPreDecompressSelect = $0005;
	kImageCodecBandDecompressSelect = $0006;
	kImageCodecBusySelect = $0007;
	kImageCodecGetCompressedImageSizeSelect = $0008;
	kImageCodecGetSimilaritySelect = $0009;
	kImageCodecTrimImageSelect = $000A;
	kImageCodecRequestSettingsSelect = $000B;
	kImageCodecGetSettingsSelect = $000C;
	kImageCodecSetSettingsSelect = $000D;
	kImageCodecFlushSelect = $000E;
	kImageCodecSetTimeCodeSelect = $000F;
	kImageCodecIsImageDescriptionEquivalentSelect = $0010;
	kImageCodecNewMemorySelect = $0011;
	kImageCodecDisposeMemorySelect = $0012;
	kImageCodecHitTestDataSelect = $0013;
	kImageCodecNewImageBufferMemorySelect = $0014;
	kImageCodecExtractAndCombineFieldsSelect = $0015;
	kImageCodecGetMaxCompressionSizeWithSourcesSelect = $0016;
	kImageCodecSetTimeBaseSelect = $0017;
	kImageCodecSourceChangedSelect = $0018;
	kImageCodecFlushFrameSelect = $0019;
	kImageCodecGetSettingsAsTextSelect = $001A;
	kImageCodecGetParameterListHandleSelect = $001B;
	kImageCodecGetParameterListSelect = $001C;
	kImageCodecCreateStandardParameterDialogSelect = $001D;
	kImageCodecIsStandardParameterDialogEventSelect = $001E;
	kImageCodecDismissStandardParameterDialogSelect = $001F;
	kImageCodecStandardParameterDialogDoActionSelect = $0020;
	kImageCodecNewImageGWorldSelect = $0021;
	kImageCodecDisposeImageGWorldSelect = $0022;
	kImageCodecHitTestDataWithFlagsSelect = $0023;
	kImageCodecValidateParametersSelect = $0024;
	kImageCodecGetBaseMPWorkFunctionSelect = $0025;
	kImageCodecLockBitsSelect = $0026;
	kImageCodecUnlockBitsSelect = $0027;
	kImageCodecRequestGammaLevelSelect = $0028;
	kImageCodecGetSourceDataGammaLevelSelect = $0029;
	kImageCodecGetDecompressLatencySelect = $002B;
	kImageCodecMergeFloatingImageOntoWindowSelect = $002C;
	kImageCodecRemoveFloatingImageSelect = $002D;
	kImageCodecGetDITLForSizeSelect = $002E;
	kImageCodecDITLInstallSelect = $002F;
	kImageCodecDITLEventSelect = $0030;
	kImageCodecDITLItemSelect = $0031;
	kImageCodecDITLRemoveSelect = $0032;
	kImageCodecDITLValidateInputSelect = $0033;
	kImageCodecGetPreferredChunkSizeAndAlignmentSelect = $0036;
	kImageCodecPrepareToCompressFramesSelect = $0037;
	kImageCodecEncodeFrameSelect = $0038;
	kImageCodecCompleteFrameSelect = $0039;
	kImageCodecBeginPassSelect = $003A;
	kImageCodecEndPassSelect = $003B;
	kImageCodecProcessBetweenPassesSelect = $003C;
	kImageCodecPreflightSelect = $0200;
	kImageCodecInitializeSelect = $0201;
	kImageCodecBeginBandSelect = $0202;
	kImageCodecDrawBandSelect = $0203;
	kImageCodecEndBandSelect = $0204;
	kImageCodecQueueStartingSelect = $0205;
	kImageCodecQueueStoppingSelect = $0206;
	kImageCodecDroppingFrameSelect = $0207;
	kImageCodecScheduleFrameSelect = $0208;
	kImageCodecCancelTriggerSelect = $0209;
	kImageCodecDecodeBandSelect = $020F;


const
	kMotionJPEGTag = FourCharCode('mjpg');
	kJPEGQuantizationTablesImageDescriptionExtension = FourCharCode('mjqt');
	kJPEGHuffmanTablesImageDescriptionExtension = FourCharCode('mjht');
	kFieldInfoImageDescriptionExtension = FourCharCode('fiel'); { image description extension describing the field count and field orderings}

const
	kFieldOrderUnknown = 0;
	kFieldsStoredF1F2DisplayedF1F2 = 1;
	kFieldsStoredF1F2DisplayedF2F1 = 2;
	kFieldsStoredF2F1DisplayedF1F2 = 5;
	kFieldsStoredF2F1DisplayedF2F1 = 6;

type
	MotionJPEGApp1MarkerPtr = ^MotionJPEGApp1Marker;
	MotionJPEGApp1Marker = record
		unused: SIGNEDLONG;
		tag: SIGNEDLONG;
		fieldSize: SIGNEDLONG;
		paddedFieldSize: SIGNEDLONG;
		offsetToNextField: SIGNEDLONG;
		qTableOffset: SIGNEDLONG;
		huffmanTableOffset: SIGNEDLONG;
		sofOffset: SIGNEDLONG;
		sosOffset: SIGNEDLONG;
		soiOffset: SIGNEDLONG;
	end;
type
	FieldInfoImageDescriptionExtensionPtr = ^FieldInfoImageDescriptionExtension;
	FieldInfoImageDescriptionExtension = record
		fieldCount: UInt8;
		fieldOrderings: UInt8;
	end;

{
 *  QTPhotoSetSampling()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function QTPhotoSetSampling( codec: ComponentInstance; yH: SInt16; yV: SInt16; cbH: SInt16; cbV: SInt16; crH: SInt16; crV: SInt16 ): ComponentResult; external name '_QTPhotoSetSampling';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  QTPhotoSetRestartInterval()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function QTPhotoSetRestartInterval( codec: ComponentInstance; restartInterval: UInt16 ): ComponentResult; external name '_QTPhotoSetRestartInterval';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  QTPhotoDefineHuffmanTable()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function QTPhotoDefineHuffmanTable( codec: ComponentInstance; componentNumber: SInt16; isDC: Boolean; lengthCounts: UInt8Ptr; values: UInt8Ptr ): ComponentResult; external name '_QTPhotoDefineHuffmanTable';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  QTPhotoDefineQuantizationTable()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function QTPhotoDefineQuantizationTable( codec: ComponentInstance; componentNumber: SInt16; table: UInt8Ptr ): ComponentResult; external name '_QTPhotoDefineQuantizationTable';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{ selectors for component calls }
const
	kQTPhotoSetSamplingSelect = $0100;
	kQTPhotoSetRestartIntervalSelect = $0101;
	kQTPhotoDefineHuffmanTableSelect = $0102;
	kQTPhotoDefineQuantizationTableSelect = $0103;

{
 *  Summary:
 *    Properties for image compressor components
 }
const
{
   * Property class for image compressor components.
   }
	kQTPropertyClass_ImageCompressor = FourCharCode('imco');


{
 *  Summary:
 *    Enforced properties for image compressor components
 *  
 *  Discussion:
 *    Image compressors that sometimes or always restrict image
 *    dimensions, clean aperture and/or pixel aspect ratio should
 *    support these properties. 
 *    If these properties can change dynamically for a compressor (eg,
 *    in response to user interaction) then the properties should be
 *    listenable, and the compressor should call the listeners whenever
 *    the properties change. (In this case, the component's
 *    GetComponentPropertyInfo function should set the
 *    kComponentPropertyFlagWillNotifyListeners flag.) 
 *    If a compressor has a mode in which these properties are
 *    flexible, then when the component is in that mode, (a) the
 *    component's GetComponentProperty function should return
 *    kQTPropertyAskLaterErr for these properties, and (b) the
 *    component's GetComponentPropertyInfo function should set the
 *    kComponentPropertyFlagCanGetLater flag for these properties.
 }
const
{
   * The encoded width enforced for compressed frames.
   }
	kICMImageCompressorPropertyID_EnforcedEncodedWidth = FourCharCode('enwi'); { SInt32, Read/Sometimes Listen }

  {
   * The encoded height enforced for compressed frames.
   }
	kICMImageCompressorPropertyID_EnforcedEncodedHeight = FourCharCode('enhe'); { SInt32, Read/Sometimes Listen }

  {
   * The clean aperture enforced for compressed frames.
   }
	kICMImageCompressorPropertyID_EnforcedCleanAperture = FourCharCode('encl'); { CleanApertureImageDescriptionExtension, Read/Sometimes Listen }

  {
   * The pixel aspect ratio enforced for compressed frames.
   }
	kICMImageCompressorPropertyID_EnforcedPixelAspectRatio = FourCharCode('enpa'); { PixelAspectRatioImageDescriptionExtension, Read/Sometimes Listen }

  {
   * The number and order of fields enforced for compressed frames.
   }
	kICMImageCompressorPropertyID_EnforcedFieldInfo = FourCharCode('enfi'); { FieldInfoImageDescriptionExtension2, Read/Sometimes Listen }


{
 *  Summary:
 *    DV Compressor Component Properties.
 }
const
{
   * Property class for DV compressors.  (Applicable to DV25, DV50,
   * NTSC, PAL, PROPAL.)
   }
	kQTPropertyClass_DVCompressor = FourCharCode('dvco');

  {
   * If set, indicates that the compressed frames should be marked as
   * progressive-scan. By default, this flag is clear, meaning that
   * frames should be marked as interlaced.
   }
	kDVCompressorPropertyID_ProgressiveScan = FourCharCode('prog'); { Boolean, Read/Write }

  {
   * If set, indicates that the compressor should use a 16:9 picture
   * aspect ratio. If clear, the compressor will use the default 4:3
   * picture aspect ratio.
   }
	kDVCompressorPropertyID_AspectRatio16x9 = FourCharCode('16x9'); { Boolean, Read/Write }


{ source identifier -- placed in root container of description, one or more required }
const
	kEffectSourceName = FourCharCode('src ');


{ source type -- placed in the input map to identify the source kind }
const
	kEffectDataSourceType = FourCharCode('dtst');

{  default effect types }
const
	kEffectRawSource = 0;    { the source is raw image data}
	kEffectGenericType = FourCharCode('geff'); { generic effect for combining others}

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
		effectType: SIGNEDLONG;             { type of effect or kEffectRawSource if raw ICM data}
		data: Ptr;                   { track data for this effect}
		source: SourceData;                 { source/effect pointers}
		next: EffectSourcePtr;                   { the next source for the parent effect}

                                              { fields added for QuickTime 4.0}
		lastTranslatedFrameTime: TimeValue; { start frame time of last converted frame, may be -1}
		lastFrameDuration: TimeValue;      { duration of the last converted frame, may be zero}
		lastFrameTimeScale: TimeValue;     { time scale of this source frame, only has meaning if above fields are valid}
	end;

type
	ICMFrameTimeRecord_QT3Ptr = ^ICMFrameTimeRecord_QT3;
	ICMFrameTimeRecord_QT3 = record
		value: wide;                  { frame display time}
		scale: SIGNEDLONG;                  { timescale of value/duration fields}
		base: UnivPtr;                   { timebase}

		duration: SIGNEDLONG;               { duration frame is to be displayed (0 if unknown)}
		rate: Fixed;                   { rate of timebase relative to wall-time}

		recordSize: SIGNEDLONG;             { total number of bytes in ICMFrameTimeRecord}

		frameNumber: SIGNEDLONG;            { number of frame, zero if not known}

		flags: SIGNEDLONG;

		virtualStartTime: wide;       { conceptual start time}
		virtualDuration: SIGNEDLONG;        { conceptual duration}
	end;
type
	EffectsFrameParamsPtr = ^EffectsFrameParams;
	EffectsFrameParams = record
		frameTime: ICMFrameTimeRecord_QT3;          { timing data (uses non-extended ICMFrameTimeRecord)}
		effectDuration: SIGNEDLONG;         { the duration of a single effect frame}
		doAsync: Boolean;                { set to true if the effect can go async}
		pad: array[0..2] of UInt8;
		source: EffectSourcePtr;                 { ptr to the source input tree}
		refCon: UnivPtr;                 { storage for the effect}
	end;

{
 *  ImageCodecEffectSetup()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageCodecEffectSetup( effect: ComponentInstance; var p: CodecDecompressParams ): ComponentResult; external name '_ImageCodecEffectSetup';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  ImageCodecEffectBegin()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageCodecEffectBegin( effect: ComponentInstance; var p: CodecDecompressParams; ePtr: EffectsFrameParamsPtr ): ComponentResult; external name '_ImageCodecEffectBegin';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  ImageCodecEffectRenderFrame()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageCodecEffectRenderFrame( effect: ComponentInstance; p: EffectsFrameParamsPtr ): ComponentResult; external name '_ImageCodecEffectRenderFrame';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  ImageCodecEffectConvertEffectSourceToFormat()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageCodecEffectConvertEffectSourceToFormat( effect: ComponentInstance; sourceToConvert: EffectSourcePtr; requestedDesc: ImageDescriptionHandle ): ComponentResult; external name '_ImageCodecEffectConvertEffectSourceToFormat';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  ImageCodecEffectCancel()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageCodecEffectCancel( effect: ComponentInstance; p: EffectsFrameParamsPtr ): ComponentResult; external name '_ImageCodecEffectCancel';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  ImageCodecEffectGetSpeed()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageCodecEffectGetSpeed( effect: ComponentInstance; parameters: QTAtomContainer; var pFPS: Fixed ): ComponentResult; external name '_ImageCodecEffectGetSpeed';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


const
	kSMPTENoFlag = 0;
	kSMPTESmoothEdgeFlag = $01; { smooth edges of the stroke}
	kSMPTEStrokeEdgeFlag = $02;  { stroke edge with color}

type
	SMPTEFlags = SIGNEDLONG;
	SMPTEFrameReference = SIGNEDLONG;
const
	kSlideHorizontalWipe = 1;
	kSlideVerticalWipe = 2;
	kTopLeftWipe = 3;
	kTopRightWipe = 4;
	kBottomRightWipe = 5;
	kBottomLeftWipe = 6;
	kFourCornerWipe = 7;
	kFourBoxWipe = 8;
	kBarnVerticalWipe = 21;
	kBarnHorizontalWipe = 22;
	kTopCenterWipe = 23;
	kRightCenterWipe = 24;
	kBottomCenterWipe = 25;
	kLeftCenterWipe = 26;
	kDiagonalLeftDownWipe = 41;
	kDiagonalRightDownWipe = 42;
	kTopBottomBowTieWipe = 43;
	kLeftRightBowTieWipe = 44;
	kDiagonalLeftOutWipe = 45;
	kDiagonalRightOutWipe = 46;
	kDiagonalCrossWipe = 47;
	kDiagonalBoxWipe = 48;
	kFilledVWipe = 61;
	kFilledVRightWipe = 62;
	kFilledVBottomWipe = 63;
	kFilledVLeftWipe = 64;
	kHollowVWipe = 65;
	kHollowVRightWipe = 66;
	kHollowVBottomWipe = 67;
	kHollowVLeftWipe = 68;
	kVerticalZigZagWipe = 71;
	kHorizontalZigZagWipe = 72;
	kVerticalBarnZigZagWipe = 73;
	kHorizontalBarnZigZagWipe = 74;

const
	kRectangleWipe = 101;
	kDiamondWipe = 102;
	kTriangleWipe = 103;
	kTriangleRightWipe = 104;
	kTriangleUpsideDownWipe = 105;
	kTriangleLeftWipe = 106;
	kSpaceShipWipe = 107;
	kSpaceShipRightWipe = 108;
	kSpaceShipUpsideDownWipe = 109;
	kSpaceShipLeftWipe = 110;
	kPentagonWipe = 111;
	kPentagonUpsideDownWipe = 112;
	kHexagonWipe = 113;
	kHexagonSideWipe = 114;
	kCircleWipe = 119;
	kOvalWipe = 120;
	kOvalSideWipe = 121;
	kCatEyeWipe = 122;
	kCatEyeSideWipe = 123;
	kRoundRectWipe = 124;
	kRoundRectSideWipe = 125;
	kFourPointStarWipe = 127;
	kFivePointStarWipe = 128;
	kStarOfDavidWipe = 129;
	kHeartWipe = 130;
	kKeyholeWipe = 131;

const
	kRotatingTopWipe = 201;
	kRotatingRightWipe = 202;
	kRotatingBottomWipe = 203;
	kRotatingLeftWipe = 204;
	kRotatingTopBottomWipe = 205;
	kRotatingLeftRightWipe = 206;
	kRotatingQuadrantWipe = 207;
	kTopToBottom180Wipe = 211;
	kRightToLeft180Wipe = 212;
	kTopToBottom90Wipe = 213;
	kRightToLeft90Wipe = 214;
	kTop180Wipe = 221;
	kRight180Wipe = 222;
	kBottom180Wipe = 223;
	kLeft180Wipe = 224;
	kCounterRotatingTopBottomWipe = 225;
	kCounterRotatingLeftRightWipe = 226;
	kDoubleRotatingTopBottomWipe = 227;
	kDoubleRotatingLeftRightWipe = 228;
	kVOpenTopWipe = 231;
	kVOpenRightWipe = 232;
	kVOpenBottomWipe = 233;
	kVOpenLeftWipe = 234;
	kVOpenTopBottomWipe = 235;
	kVOpenLeftRightWipe = 236;
	kRotatingTopLeftWipe = 241;
	kRotatingBottomLeftWipe = 242;
	kRotatingBottomRightWipe = 243;
	kRotatingTopRightWipe = 244;
	kRotatingTopLeftBottomRightWipe = 245;
	kRotatingBottomLeftTopRightWipe = 246;
	kRotatingTopLeftRightWipe = 251;
	kRotatingLeftTopBottomWipe = 252;
	kRotatingBottomLeftRightWipe = 253;
	kRotatingRightTopBottomWipe = 254;
	kRotatingDoubleCenterRightWipe = 261;
	kRotatingDoubleCenterTopWipe = 262;
	kRotatingDoubleCenterTopBottomWipe = 263;
	kRotatingDoubleCenterLeftRightWipe = 264;

const
	kHorizontalMatrixWipe = 301;
	kVerticalMatrixWipe = 302;
	kTopLeftDiagonalMatrixWipe = 303;
	kTopRightDiagonalMatrixWipe = 304;
	kBottomRightDiagonalMatrixWipe = 305;
	kBottomLeftDiagonalMatrixWipe = 306;
	kClockwiseTopLeftMatrixWipe = 310;
	kClockwiseTopRightMatrixWipe = 311;
	kClockwiseBottomRightMatrixWipe = 312;
	kClockwiseBottomLeftMatrixWipe = 313;
	kCounterClockwiseTopLeftMatrixWipe = 314;
	kCounterClockwiseTopRightMatrixWipe = 315;
	kCounterClockwiseBottomRightMatrixWipe = 316;
	kCounterClockwiseBottomLeftMatrixWipe = 317;
	kVerticalStartTopMatrixWipe = 320;
	kVerticalStartBottomMatrixWipe = 321;
	kVerticalStartTopOppositeMatrixWipe = 322;
	kVerticalStartBottomOppositeMatrixWipe = 323;
	kHorizontalStartLeftMatrixWipe = 324;
	kHorizontalStartRightMatrixWipe = 325;
	kHorizontalStartLeftOppositeMatrixWipe = 326;
	kHorizontalStartRightOppositeMatrixWipe = 327;
	kDoubleDiagonalTopRightMatrixWipe = 328;
	kDoubleDiagonalBottomRightMatrixWipe = 329;
	kDoubleSpiralTopMatixWipe = 340;
	kDoubleSpiralBottomMatixWipe = 341;
	kDoubleSpiralLeftMatixWipe = 342;
	kDoubleSpiralRightMatixWipe = 343;
	kQuadSpiralVerticalMatixWipe = 344;
	kQuadSpiralHorizontalMatixWipe = 345;
	kVerticalWaterfallLeftMatrixWipe = 350;
	kVerticalWaterfallRightMatrixWipe = 351;
	kHorizontalWaterfallLeftMatrixWipe = 352;
	kHorizontalWaterfallRightMatrixWipe = 353;
	kRandomWipe = 409;  { non-SMPTE standard numbers}
	kRandomWipeGroupWipe = 501;
	kRandomIrisGroupWipe = 502;
	kRandomRadialGroupWipe = 503;
	kRandomMatrixGroupWipe = 504;

type
	SMPTEWipeType = UNSIGNEDLONG;
{
 *  ImageCodecEffectPrepareSMPTEFrame()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function ImageCodecEffectPrepareSMPTEFrame( effect: ComponentInstance; destPixMap: PixMapPtr; var returnValue: SMPTEFrameReference ): ComponentResult; external name '_ImageCodecEffectPrepareSMPTEFrame';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  ImageCodecEffectDisposeSMPTEFrame()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function ImageCodecEffectDisposeSMPTEFrame( effect: ComponentInstance; frameRef: SMPTEFrameReference ): ComponentResult; external name '_ImageCodecEffectDisposeSMPTEFrame';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  ImageCodecEffectRenderSMPTEFrame()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function ImageCodecEffectRenderSMPTEFrame( effect: ComponentInstance; destPixMap: PixMapPtr; frameRef: SMPTEFrameReference; effectPercentageEven: Fixed; effectPercentageOdd: Fixed; var pSourceRect: Rect; var matrixP: MatrixRecord; effectNumber: SMPTEWipeType; xRepeat: SIGNEDLONG; yRepeat: SIGNEDLONG; flags: SMPTEFlags; penWidth: Fixed; strokeValue: SIGNEDLONG ): ComponentResult; external name '_ImageCodecEffectRenderSMPTEFrame';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{ selectors for component calls }
const
	kImageCodecEffectSetupSelect = $0300;
	kImageCodecEffectBeginSelect = $0301;
	kImageCodecEffectRenderFrameSelect = $0302;
	kImageCodecEffectConvertEffectSourceToFormatSelect = $0303;
	kImageCodecEffectCancelSelect = $0304;
	kImageCodecEffectGetSpeedSelect = $0305;
	kImageCodecEffectPrepareSMPTEFrameSelect = $0100;
	kImageCodecEffectDisposeSMPTEFrameSelect = $0101;
	kImageCodecEffectRenderSMPTEFrameSelect = $0102;


{ curve atom types and data structures }
const
	kCurvePathAtom = FourCharCode('path');
	kCurveEndAtom = FourCharCode('zero');
	kCurveAntialiasControlAtom = FourCharCode('anti');
	kCurveAntialiasOff = 0;
	kCurveAntialiasOn = $FFFFFFFF;
	kCurveFillTypeAtom = FourCharCode('fill');
	kCurvePenThicknessAtom = FourCharCode('pent');
	kCurveMiterLimitAtom = FourCharCode('mitr');
	kCurveJoinAttributesAtom = FourCharCode('join');
	kCurveMinimumDepthAtom = FourCharCode('mind');
	kCurveDepthAlwaysOffscreenMask = $80000000;
	kCurveTransferModeAtom = FourCharCode('xfer');
	kCurveGradientAngleAtom = FourCharCode('angl');
	kCurveGradientRadiusAtom = FourCharCode('radi');
	kCurveGradientOffsetAtom = FourCharCode('cent');

const
	kCurveARGBColorAtom = FourCharCode('argb');

type
	ARGBColorPtr = ^ARGBColor;
	ARGBColor = record
		alpha: UInt16;
		red: UInt16;
		green: UInt16;
		blue: UInt16;
	end;
const
	kCurveGradientRecordAtom = FourCharCode('grad');

type
	GradientColorRecordPtr = ^GradientColorRecord;
	GradientColorRecord = record
		thisColor: ARGBColor;
		endingPercentage: Fixed;
	end;
type
	GradientColorPtr = GradientColorRecordPtr;
const
	kCurveGradientTypeAtom = FourCharCode('grdt');

{ currently supported gradient types }
const
	kLinearGradient = 0;
	kCircularGradient = 1;

type
	GradientType = SIGNEDLONG;
{
 *  CurveGetLength()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function CurveGetLength( effect: ComponentInstance; var target: gxPaths; index: SIGNEDLONG; var wideLength: wide ): ComponentResult; external name '_CurveGetLength';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  CurveLengthToPoint()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function CurveLengthToPoint( effect: ComponentInstance; var target: gxPaths; index: SIGNEDLONG; length: Fixed; var location: FixedPoint; var tangent: FixedPoint ): ComponentResult; external name '_CurveLengthToPoint';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  CurveNewPath()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function CurveNewPath( effect: ComponentInstance; var pPath: Handle ): ComponentResult; external name '_CurveNewPath';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  CurveCountPointsInPath()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function CurveCountPointsInPath( effect: ComponentInstance; var aPath: gxPaths; contourIndex: UNSIGNEDLONG; var pCount: UNSIGNEDLONG ): ComponentResult; external name '_CurveCountPointsInPath';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  CurveGetPathPoint()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function CurveGetPathPoint( effect: ComponentInstance; var aPath: gxPaths; contourIndex: UNSIGNEDLONG; pointIndex: UNSIGNEDLONG; var thePoint: gxPoint; var ptIsOnPath: Boolean ): ComponentResult; external name '_CurveGetPathPoint';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  CurveInsertPointIntoPath()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function CurveInsertPointIntoPath( effect: ComponentInstance; var aPoint: gxPoint; thePath: Handle; contourIndex: UNSIGNEDLONG; pointIndex: UNSIGNEDLONG; ptIsOnPath: Boolean ): ComponentResult; external name '_CurveInsertPointIntoPath';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  CurveSetPathPoint()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function CurveSetPathPoint( effect: ComponentInstance; var aPath: gxPaths; contourIndex: UNSIGNEDLONG; pointIndex: UNSIGNEDLONG; var thePoint: gxPoint; ptIsOnPath: Boolean ): ComponentResult; external name '_CurveSetPathPoint';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  CurveGetNearestPathPoint()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function CurveGetNearestPathPoint( effect: ComponentInstance; var aPath: gxPaths; var thePoint: FixedPoint; var contourIndex: UNSIGNEDLONG; var pointIndex: UNSIGNEDLONG; var theDelta: Fixed ): ComponentResult; external name '_CurveGetNearestPathPoint';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  CurvePathPointToLength()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function CurvePathPointToLength( ci: ComponentInstance; var aPath: gxPaths; startDist: Fixed; endDist: Fixed; var thePoint: FixedPoint; var pLength: Fixed ): ComponentResult; external name '_CurvePathPointToLength';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  CurveCreateVectorStream()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function CurveCreateVectorStream( effect: ComponentInstance; var pStream: Handle ): ComponentResult; external name '_CurveCreateVectorStream';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  CurveAddAtomToVectorStream()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function CurveAddAtomToVectorStream( effect: ComponentInstance; atomType: OSType; atomSize: Size; pAtomData: UnivPtr; vectorStream: Handle ): ComponentResult; external name '_CurveAddAtomToVectorStream';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  CurveAddPathAtomToVectorStream()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function CurveAddPathAtomToVectorStream( effect: ComponentInstance; pathData: Handle; vectorStream: Handle ): ComponentResult; external name '_CurveAddPathAtomToVectorStream';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  CurveAddZeroAtomToVectorStream()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function CurveAddZeroAtomToVectorStream( effect: ComponentInstance; vectorStream: Handle ): ComponentResult; external name '_CurveAddZeroAtomToVectorStream';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  CurveGetAtomDataFromVectorStream()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function CurveGetAtomDataFromVectorStream( effect: ComponentInstance; vectorStream: Handle; atomType: SIGNEDLONG; var dataSize: SIGNEDLONG; var dataPtr: Ptr ): ComponentResult; external name '_CurveGetAtomDataFromVectorStream';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{ selectors for component calls }
const
	kCurveGetLengthSelect = $0100;
	kCurveLengthToPointSelect = $0101;
	kCurveNewPathSelect = $0102;
	kCurveCountPointsInPathSelect = $0103;
	kCurveGetPathPointSelect = $0104;
	kCurveInsertPointIntoPathSelect = $0105;
	kCurveSetPathPointSelect = $0106;
	kCurveGetNearestPathPointSelect = $0107;
	kCurvePathPointToLengthSelect = $0108;
	kCurveCreateVectorStreamSelect = $0109;
	kCurveAddAtomToVectorStreamSelect = $010A;
	kCurveAddPathAtomToVectorStreamSelect = $010B;
	kCurveAddZeroAtomToVectorStreamSelect = $010C;
	kCurveGetAtomDataFromVectorStreamSelect = $010D;
{ UPP call backs }

{$endc} {not TARGET_CPU_64}

{$endc} {TARGET_OS_MAC}
{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}

end.
{$endc} {not MACOSALLINCLUDE}
