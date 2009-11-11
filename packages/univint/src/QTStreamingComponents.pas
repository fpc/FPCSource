{
     File:       QuickTime/QTStreamingComponents.h
 
     Contains:   QuickTime Interfaces.
 
     Version:    QuickTime 7.6.3
 
     Copyright:  © 1990-2008 by Apple Inc., all rights reserved
 
     Bugs?:      For bug reports, consult the following page on
                 the World Wide Web:
 
                     http://www.freepascal.org/bugs.html
 
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

unit QTStreamingComponents;
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
	{$setc TARGET_OS_MAC := TRUE}
	{$setc TARGET_OS_IPHONE := FALSE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
{$elifc defined __ppc64__ and __ppc64__}
	{$setc TARGET_CPU_PPC := FALSE}
	{$setc TARGET_CPU_PPC64 := TRUE}
	{$setc TARGET_CPU_X86 := FALSE}
	{$setc TARGET_CPU_X86_64 := FALSE}
	{$setc TARGET_CPU_ARM := FALSE}
	{$setc TARGET_OS_MAC := TRUE}
	{$setc TARGET_OS_IPHONE := FALSE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
{$elifc defined __i386__ and __i386__}
	{$setc TARGET_CPU_PPC := FALSE}
	{$setc TARGET_CPU_PPC64 := FALSE}
	{$setc TARGET_CPU_X86 := TRUE}
	{$setc TARGET_CPU_X86_64 := FALSE}
	{$setc TARGET_CPU_ARM := FALSE}
{$ifc defined(iphonesim)}
 	{$setc TARGET_OS_MAC := FALSE}
	{$setc TARGET_OS_IPHONE := TRUE}
	{$setc TARGET_IPHONE_SIMULATOR := TRUE}
{$elsec}
	{$setc TARGET_OS_MAC := TRUE}
	{$setc TARGET_OS_IPHONE := FALSE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
{$endc}
{$elifc defined __x86_64__ and __x86_64__}
	{$setc TARGET_CPU_PPC := FALSE}
	{$setc TARGET_CPU_PPC64 := FALSE}
	{$setc TARGET_CPU_X86 := FALSE}
	{$setc TARGET_CPU_X86_64 := TRUE}
	{$setc TARGET_CPU_ARM := FALSE}
	{$setc TARGET_OS_MAC := TRUE}
	{$setc TARGET_OS_IPHONE := FALSE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
{$elifc defined __arm__ and __arm__}
	{$setc TARGET_CPU_PPC := FALSE}
	{$setc TARGET_CPU_PPC64 := FALSE}
	{$setc TARGET_CPU_X86 := FALSE}
	{$setc TARGET_CPU_X86_64 := FALSE}
	{$setc TARGET_CPU_ARM := TRUE}
	{ will require compiler define when/if other Apple devices with ARM cpus ship }
	{$setc TARGET_OS_MAC := FALSE}
	{$setc TARGET_OS_IPHONE := TRUE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
{$elsec}
	{$error __ppc__ nor __ppc64__ nor __i386__ nor __x86_64__ nor __arm__ is defined.}
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
uses MacTypes,Components,Dialogs,Movies,QuickTimeStreaming;
{$endc} {not MACOSALLINCLUDE}


{$ifc TARGET_OS_MAC}

{$ALIGN MAC68K}


{ QuickTime is not available to 64-bit clients }

{$ifc not TARGET_CPU_64}

{============================================================================
        Stream Sourcer
============================================================================}
const
	kQTSSourcerType = FourCharCode('srcr');

type
	QTSSourcer = ComponentInstance;
const
	kQTSSGChannelSourcerType = FourCharCode('sgch');
	kQTSMovieTrackSourcerType = FourCharCode('trak');
	kQTSPushDataSourcerType = FourCharCode('push');

{ flags for sourcer data }
const
	kQTSSourcerDataFlag_SyncSample = $00000001;
	kQTSPushDataSourcerFlag_SampleTimeIsValid = $80000000;


const
	kQTSSourcerInitParamsVersion1 = 1;

type
	QTSSourcerInitParamsPtr = ^QTSSourcerInitParams;
	QTSSourcerInitParams = record
		version: SInt32;
		flags: SInt32;
		dataType: OSType;
		data: UnivPtr;
		dataLength: UInt32;
	end;
{
 *  QTSNewSourcer()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Non-Carbon CFM:   in QTStreamLib 5.0 and later
 }
function QTSNewSourcer( params: UnivPtr; const (*var*) inInitParams: QTSSourcerInitParams; inFlags: SInt32; var outSourcer: ComponentInstance ): OSErr; external name '_QTSNewSourcer';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{ info selectors for sourcers - get and set }
const
	kQTSInfo_Track = FourCharCode('trak'); { QTSTrackParams* }
	kQTSInfo_Loop = FourCharCode('loop'); { QTSLoopParams* }
	kQTSInfo_SourcerTiming = FourCharCode('stim'); { QTSSourcerTimingParams* }
	kQTSInfo_TargetFrameRate = FourCharCode('tfps'); { Fixed * in frames per second }
	kQTSInfo_PushData = FourCharCode('push'); { QTSPushDataParams* }
	kQTSInfo_SourcerCallbackProc = FourCharCode('scbp'); { QTSSourcerCallbackProcParams* }
	kQTSInfo_TargetDataRate = FourCharCode('tdrt'); { UInt32 * in bytes per second }
	kQTSInfo_AudioAutoGainOnOff = FourCharCode('agc '); { Boolean*  - error if unavailable}
	kQTSInfo_AudioGain = FourCharCode('gain'); { Fixed* kFixed1 is unity gain }
	kQTSInfo_CroppedInputRect = FourCharCode('crpr'); { Rect* - defined relative to kQTSInfo_FullInputRect below }
	kQTSInfo_SpatialSettings = FourCharCode('sptl'); { pointer to SCSpatialSettings struct}
	kQTSInfo_TemporalSettings = FourCharCode('tprl'); { pointer to SCTemporalSettings struct}
	kQTSInfo_DataRateSettings = FourCharCode('drat'); { pointer to SCDataRateSettings struct}
	kQTSInfo_CodecFlags = FourCharCode('cflg'); { pointer to CodecFlags}
	kQTSInfo_CodecSettings = FourCharCode('cdec'); { pointer to Handle}
	kQTSInfo_ForceKeyValue = FourCharCode('ksim'); { pointer to long}
	kQTSInfo_SoundSampleRate = FourCharCode('ssrt'); { pointer to UnsignedFixed}
	kQTSInfo_SoundSampleSize = FourCharCode('ssss'); { pointer to short}
	kQTSInfo_SoundChannelCount = FourCharCode('sscc'); { pointer to short}
	kQTSInfo_SoundCompression = FourCharCode('ssct'); { pointer to OSType}
	kQTSInfo_CompressionList = FourCharCode('ctyl'); { pointer to OSType Handle}
	kQTSInfo_VideoHue = FourCharCode('hue '); { UInt16* }
	kQTSInfo_VideoSaturation = FourCharCode('satr'); { UInt16* }
	kQTSInfo_VideoContrast = FourCharCode('trst'); { UInt16* }
	kQTSInfo_VideoBrightness = FourCharCode('brit'); { UInt16* }
	kQTSInfo_VideoSharpness = FourCharCode('shrp'); { UInt16* }
	kQTSInfo_TimeScale = FourCharCode('scal'); { UInt32* }
	kQTSInfo_SGChannelDeviceName = FourCharCode('innm'); { Handle* }
	kQTSInfo_SGChannelDeviceList = FourCharCode('srdl'); { SGDeviceList* }
	kQTSInfo_SGChannelDeviceInput = FourCharCode('sdii'); { short* }
	kQTSInfo_SGChannelSettings = FourCharCode('sesg'); { QTSSGChannelSettingsParams }
	kQTSInfo_PreviewWhileRecordingMode = FourCharCode('srpr'); { Boolean* }
	kQTSInfo_CompressionParams = FourCharCode('sccp'); { QTAtomContainer* }

{ info selectors for sourcers - get only}
const
	kQTSInfo_SGChannel = FourCharCode('sgch'); { SGChannel* }
	kQTSInfo_SGChannelInputName = FourCharCode('srnm'); { Handle* }
	kQTSInfo_FullInputRect = FourCharCode('fulr'); { Rect* }

{ loop flags }
const
	kQTSLoopFlag_Loop = $00000001;

const
	kQTSLoopParamsVersion1 = 1;

type
	QTSLoopParamsPtr = ^QTSLoopParams;
	QTSLoopParams = record
		version: SInt32;
		flags: SInt32;
		loopFlags: SInt32;
		flagsMask: SInt32;
		numLoops: SInt32;
	end;
const
	kQTSTrackParamsVersion1 = 1;

type
	QTSTrackParamsPtr = ^QTSTrackParams;
	QTSTrackParams = record
		version: SInt32;
		flags: SInt32;
		track: Track_fix;
		trackStartOffset: TimeValue64;       { to start other than at the beginning otherwise set to 0}
		duration: TimeValue64;               { to limit the duration otherwise set to 0}
		loopParams: QTSLoopParamsPtr;             { set to NULL if not using; default is no looping }
	end;
const
	kQTSSourcerTimingParamsVersion1 = 1;

type
	QTSSourcerTimingParamsPtr = ^QTSSourcerTimingParams;
	QTSSourcerTimingParams = record
		version: SInt32;
		flags: SInt32;
		timeScale_: TimeScale;
		presentationStartTime: TimeValue64;
		presentationEndTime: TimeValue64;
		presentationCurrentTime: TimeValue64;
		localStartTime: TimeValue64;
		localEndTime: TimeValue64;
		localCurrentTime: TimeValue64;
	end;
const
	kQTSPushDataParamsVersion1 = 1;

const
	kQTSPushDataFlag_SampleTimeIsValid = $00000001;
	kQTSPushDataFlag_DurationIsValid = $00000002;

type
	QTSPushDataParamsPtr = ^QTSPushDataParams;
	QTSPushDataParams = record
		version: SInt32;
		flags: SInt32;
		sampleDescription: SampleDescriptionHandle; { caller owns the handle }
		sampleDescSeed: UInt32;
		sampleTime: TimeValue64;             { also set flag if you set this }
		duration: TimeValue64;               { also set flag if you set this }
		dataLength: UInt32;
		dataPtr: UnivPtr;                { this does not have to be a real macintosh Ptr }
	end;
const
	kQTSSourcerCallbackProcParamsVersion1 = 1;


type
	QTSSourcerCallbackProcParamsPtr = ^QTSSourcerCallbackProcParams;
	QTSSourcerCallbackProcParams = record
		version: SInt32;
		flags: SInt32;
		proc: QTSNotificationUPP;
		refCon: UnivPtr;
	end;
{ track sourcer callback selectors}
const
	kQTSSourcerCallback_Done = FourCharCode('done'); { QTSSourcerDoneParams* }


{ push data sourcer callback selectors}
const
	kQTSPushDataSourcerCallback_HasCharacteristic = $050D; { QTSPushDataHasCharacteristicParams* }
	kQTSPushDataSourcerCallback_SetInfo = $0507; { QTSPushDataInfoParams* }
	kQTSPushDataSourcerCallback_GetInfo = $0508; { QTSPushDataInfoParams* }

type
	QTSPushDataHasCharacteristicParamsPtr = ^QTSPushDataHasCharacteristicParams;
	QTSPushDataHasCharacteristicParams = record
		version: SInt32;
		flags: SInt32;
		characteristic: OSType;
		returnedHasIt: Boolean;
		reserved1: SInt8;
		reserved2: SInt8;
		reserved3: SInt8;
	end;
type
	QTSPushDataInfoParamsPtr = ^QTSPushDataInfoParams;
	QTSPushDataInfoParams = record
		version: SInt32;
		flags: SInt32;
		selector: OSType;
		ioParams: UnivPtr;
	end;
const
	kQTSSourcerDoneParamsVersion1 = 1;

type
	QTSSourcerDoneParamsPtr = ^QTSSourcerDoneParams;
	QTSSourcerDoneParams = record
		version: SInt32;
		flags: SInt32;
		sourcer: ComponentInstance;
	end;
type
	QTSSGChannelSettingsParamsPtr = ^QTSSGChannelSettingsParams;
	QTSSGChannelSettingsParams = record
		settings: UserData;
		flags: SInt32;
	end;

{-----------------------------------------
    Stream Sourcer Selectors
-----------------------------------------}
const
	kQTSSourcerInitializeSelect = $0500;
	kQTSSourcerSetEnableSelect = $0503;
	kQTSSourcerGetEnableSelect = $0504;
	kQTSSourcerSetInfoSelect = $0507;
	kQTSSourcerGetInfoSelect = $0508;
	kQTSSourcerSetTimeScaleSelect = $050E;
	kQTSSourcerGetTimeScaleSelect = $050F;
	kQTSSourcerIdleSelect = $0516;

{-----------------------------------------
    Stream Sourcer Prototypes
-----------------------------------------}
{
 *  QTSSourcerInitialize()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.1 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Non-Carbon CFM:   in QTStreamLib 5.0.1 and later
 }
function QTSSourcerInitialize( inSourcer: QTSSourcer; const (*var*) inInitParams: QTSSourcerInitParams ): ComponentResult; external name '_QTSSourcerInitialize';
(* AVAILABLE_MAC_OS_X_VERSION_10_1_AND_LATER *)


{
 *  QTSSourcerIdle()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Non-Carbon CFM:   in QTStreamLib 5.0 and later
 }
function QTSSourcerIdle( inSourcer: QTSSourcer; (*const*) var inTime: TimeValue64; inFlags: SInt32; var outFlags: SInt32 ): ComponentResult; external name '_QTSSourcerIdle';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  QTSSourcerSetEnable()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Non-Carbon CFM:   in QTStreamLib 5.0 and later
 }
function QTSSourcerSetEnable( inSourcer: QTSSourcer; inEnableMode: Boolean; inFlags: SInt32 ): ComponentResult; external name '_QTSSourcerSetEnable';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  QTSSourcerGetEnable()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Non-Carbon CFM:   in QTStreamLib 5.0 and later
 }
function QTSSourcerGetEnable( inSourcer: QTSSourcer; var outEnableMode: Boolean; inFlags: SInt32 ): ComponentResult; external name '_QTSSourcerGetEnable';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  QTSSourcerSetTimeScale()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Non-Carbon CFM:   in QTStreamLib 5.0 and later
 }
function QTSSourcerSetTimeScale( inSourcer: QTSSourcer; inTimeScale: TimeScale ): ComponentResult; external name '_QTSSourcerSetTimeScale';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  QTSSourcerGetTimeScale()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Non-Carbon CFM:   in QTStreamLib 5.0 and later
 }
function QTSSourcerGetTimeScale( inSourcer: QTSSourcer; var outTimeScale: TimeScale ): ComponentResult; external name '_QTSSourcerGetTimeScale';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  QTSSourcerSetInfo()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Non-Carbon CFM:   in QTStreamLib 5.0 and later
 }
function QTSSourcerSetInfo( inSourcer: QTSSourcer; inSelector: OSType; ioParams: UnivPtr ): ComponentResult; external name '_QTSSourcerSetInfo';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  QTSSourcerGetInfo()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Non-Carbon CFM:   in QTStreamLib 5.0 and later
 }
function QTSSourcerGetInfo( inSourcer: QTSSourcer; inSelector: OSType; ioParams: UnivPtr ): ComponentResult; external name '_QTSSourcerGetInfo';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


const
	kQTSInfo_InputDeviceName = FourCharCode('innm'); { Handle* }
	kQTSInfo_InputSourceName = FourCharCode('srnm'); { Handle* }


{============================================================================
        Stream Handler
============================================================================}

{
    Server edits are only valid for the current chunk
}
type
	SHServerEditParametersPtr = ^SHServerEditParameters;
	SHServerEditParameters = record
		version: UInt32;
		editRate: Fixed;
		dataStartTime_mediaAxis: TimeValue64;
		dataEndTime_mediaAxis: TimeValue64;
	end;
const
	kSHNoChunkDispatchFlags = 0;
	kSHChunkFlagSyncSample = 1 shl 2;
	kSHChunkFlagDataLoss = 1 shl 4;
	kSHChunkFlagExtended = 1 shl 5;

type
	SHChunkRecordPtr = ^SHChunkRecord;
	SHChunkRecord = record
		version: UInt32;
		reserved1: SIGNEDLONG;
		flags: SInt32;
		dataSize: UInt32;
		dataPtr: UInt8Ptr;
		reserved2: SIGNEDLONG;
		reserved3: SIGNEDLONG;
		presentationTime: TimeValue64;

		reserved4: SIGNEDLONG;
		reserved5: SIGNEDLONG;
		serverEditParameters: {const} SHServerEditParametersPtr;
		reserved6: SIGNEDLONG;
		reserved7: SIGNEDLONG;
	end;
const
	kSHNumExtendedDataLongs = 10;

const
	kSHExtendedChunkFlag_HasSampleCount = 1 shl 0;
	kSHExtendedChunkFlag_HasFrameLengths = 1 shl 1;

type
	SHExtendedChunkRecordPtr = ^SHExtendedChunkRecord;
	SHExtendedChunkRecord = record
		chunk: SHChunkRecord;
		extendedFlags: SInt32;
		extendedData: array [0..9] of SInt32;
	end;


{============================================================================
        RTP Components
============================================================================}

type
	RTPSSRC = UInt32;
const
	kRTPInvalidSSRC = 0;


{ RTP standard content encodings for audio }
const
	kRTPPayload_PCMU = 0;    { 8kHz PCM mu-law mono }
	kRTPPayload_1016 = 1;    { 8kHz CELP (Fed Std 1016) mono }
	kRTPPayload_G721 = 2;    { 8kHz G.721 ADPCM mono }
	kRTPPayload_GSM = 3;    { 8kHz GSM mono }
	kRTPPayload_G723 = 4;    { 8kHz G.723 ADPCM mono }
	kRTPPayload_DVI_8 = 5;    { 8kHz Intel DVI ADPCM mono }
	kRTPPayload_DVI_16 = 6;    { 16kHz Intel DVI ADPCM mono }
	kRTPPayload_LPC = 7;    { 8kHz LPC }
	kRTPPayload_PCMA = 8;    { 8kHz PCM a-law mono }
	kRTPPayload_L16_44_2 = 10;   { 44.1kHz 16-bit linear stereo }
	kRTPPayload_L16_44_1 = 11;   { 44.1kHz 16-bit linear mono }
	kRTPPayload_PureVoice = 12;   { 8kHz PureVoice mono (QCELP) }
	kRTPPayload_MPEGAUDIO = 14;   { MPEG I and II audio }
	kRTPPayload_DVI_11 = 16;   { 11kHz Intel DVI ADPCM mono }
	kRTPPayload_DVI_22 = 17;    { 22kHz Intel DVI ADPCM mono }

{ RTP standard content encodings for video }
const
	kRTPPayload_CELLB = 25;   { Sun CellB }
	kRTPPayload_JPEG = 26;   { JPEG }
	kRTPPayload_CUSEEME = 27;   { Cornell CU-SeeMe }
	kRTPPayload_NV = 28;   { Xerox PARC nv }
	kRTPPayload_PICWIN = 29;   { BBN Picture Window }
	kRTPPayload_CPV = 30;   { Bolter CPV }
	kRTPPayload_H261 = 31;   { CCITT H.261 }
	kRTPPayload_MPEGVIDEO = 32;   { MPEG I and II video }
	kRTPPayload_H263 = 34;    { CCITT H.263 }

{ Other RTP standard content encodings }
const
	kRTPPayload_MPEG2T = 33;    { MPEG 2 Transport }

{ Dynamic encodings }
const
	kRTPPayload_FirstDynamic = 96;
	kRTPPayload_LastDynamic = 127;
	kRTPPayload_Unknown = $FF;


{
-----------------------------------------
    RTP Info selectors
-----------------------------------------
}
{ ----- these are get and set ----- }
const
	kRTPInfo_SSRC = FourCharCode('ssrc'); { UInt32* }
	kRTPInfo_NextSeqNum = FourCharCode('rnsn'); { UInt16* }

{-----------------------------------------
    RTP Statistics
-----------------------------------------}
const
	kRTPTotalReceivedPktsStat = FourCharCode('trcp');
	kRTPTotalLostPktsStat = FourCharCode('tlsp');
	kRTPTotalProcessedPktsStat = FourCharCode('tprp');
	kRTPTotalDroppedPktsStat = FourCharCode('tdrp');
	kRTPBadHeaderDroppedPktsStat = FourCharCode('bhdp');
	kRTPOurHeaderDroppedPktsStat = FourCharCode('ohdp');
	kRTPNotReceivingSenderDroppedPktsStat = FourCharCode('nsdp');
	kRTPNotProcessingDroppedPktsStat = FourCharCode('npdp');
	kRTPBadSeqDroppedPktsStat = FourCharCode('bsdp');
	kRTPArriveTooLatePktsStat = FourCharCode('artl');
	kRTPWaitForSeqDroppedPktsStat = FourCharCode('wsdp');
	kRTPBadStateDroppedPktsStat = FourCharCode('stdp');
	kRTPBadPayloadDroppedPktsStat = FourCharCode('bpdp');
	kRTPNoTimeScaleDroppedPktsStat = FourCharCode('ntdp');
	kRTPDupSeqNumDroppedPktsStat = FourCharCode('dsdp');
	kRTPLostPktsPercentStat = FourCharCode('lspp');
	kRTPDroppedPktsPercentStat = FourCharCode('dppp');
	kRTPTotalUnprocessedPktsPercentStat = FourCharCode('tupp');
	kRTPRTCPDataRateStat = FourCharCode('rrcd');
	kRTPPayloadIDStat = FourCharCode('rpid');
	kRTPPayloadNameStat = FourCharCode('rpnm');
	kRTPNumPktsInQueueStat = FourCharCode('rnpq');
	kRTPTotalPktsInQueueStat = FourCharCode('rtpq');
	kRTPTotalOutOfOrderPktsStat = FourCharCode('rtoo');
	kRTPRetransmissionStat = FourCharCode('rrtx');


{-----------------------------------------
    Payload Info
-----------------------------------------}
const
	kRTPPayloadSpeedTag = FourCharCode('sped'); { 0-255, 255 is fastest}
	kRTPPayloadLossRecoveryTag = FourCharCode('loss'); { 0-255, 0 can't handle any loss, 128 can handle 50% packet loss}
	kRTPPayloadConformanceTag = FourCharCode('conf'); { more than one of these can be present}

type
	RTPPayloadCharacteristicPtr = ^RTPPayloadCharacteristic;
	RTPPayloadCharacteristic = record
		tag: OSType;
		value: SIGNEDLONG;
	end;
{
    pass RTPPayloadSortRequest to QTSFindMediaPacketizer or QTSFindMediaPacketizerForTrack.
    define the characteristics to sort by. tag is key to sort on. value is positive for ascending
    sort (low value first), negative for descending sort (high value first).
}
type
	RTPPayloadSortRequest = record
		characteristicCount: SIGNEDLONG;
		characteristic: array [0..0] of RTPPayloadCharacteristic; { tag is key to sort on, value is + for ascending, - for descending}
	end;
	RTPPayloadSortRequestPtr = ^RTPPayloadSortRequest;
{ flags for RTPPayloadInfo }
const
	kRTPPayloadTypeStaticFlag = $00000001;
	kRTPPayloadTypeDynamicFlag = $00000002;

type
	RTPPayloadInfo = record
		payloadFlags: SIGNEDLONG;
		payloadID: UInt8;
		reserved1: SInt8;
		reserved2: SInt8;
		reserved3: SInt8;
    payloadName: array [0..1] of SInt8;
	end;
	RTPPayloadInfoPtr = ^RTPPayloadInfo;
type
	RTPPayloadInfoHandle = ^RTPPayloadInfoPtr;
{============================================================================
        RTP Reassembler
============================================================================}
type
	RTPReassembler = ComponentInstance;
const
	kRTPReassemblerType = FourCharCode('rtpr');

const
	kRTPBaseReassemblerType = FourCharCode('gnrc');
	kRTP261ReassemblerType = FourCharCode('h261');
	kRTP263ReassemblerType = FourCharCode('h263');
	kRTP263PlusReassemblerType = FourCharCode('263+');
	kRTPAudioReassemblerType = FourCharCode('soun');
	kRTPQTReassemblerType = FourCharCode('qtim');
	kRTPPureVoiceReassemblerType = FourCharCode('Qclp');
	kRTPJPEGReassemblerType = FourCharCode('jpeg');
	kRTPQDesign2ReassemblerType = FourCharCode('QDM2');
	kRTPSorensonReassemblerType = FourCharCode('SVQ1');
	kRTPMP3ReassemblerType = FourCharCode('mp3 ');
	kRTPMPEG4AudioReassemblerType = FourCharCode('mp4a');
	kRTPMPEG4VideoReassemblerType = FourCharCode('mp4v');

type
	RTPRssmInitParamsPtr = ^RTPRssmInitParams;
	RTPRssmInitParams = record
		ssrc: RTPSSRC;
		payloadType: UInt8;
		reserved1: UInt8;
		reserved2: UInt8;
		reserved3: UInt8;
		timeBase_: TimeBase;
		timeScale_: TimeScale;
	end;
type
	RTPDescParamsPtr = ^RTPDescParams;
	RTPDescParams = record
		container: QTAtomContainer;
		presentationParentAtom: QTAtom;
		streamParentAtom: QTAtom;
	end;
type
	RTPRssmMoreInitParamsPtr = ^RTPRssmMoreInitParams;
	RTPRssmMoreInitParams = record
		initParams: RTPRssmInitParams;
		version: SInt32;
		desc: RTPDescParams;
	end;
const
	kRTPRssmMoreInitParamsVersion1 = 1;


{ get/set info selectors}
const
	kRTPRssmInfo_MoreInitParams = FourCharCode('rrmi');


type
	RTPRssmPacketPtr = ^RTPRssmPacket;
	RTPRssmPacket = record
		next: RTPRssmPacketPtr;
		prev: RTPRssmPacketPtr;
		streamBuffer: QTSStreamBufferPtr;
		paramsFilledIn: Boolean;
		reserved: UInt8;
		sequenceNum: UInt16;
		transportHeaderLength: UInt32;  { filled in by base}
		payloadHeaderLength: UInt32;    { derived adjusts this }
		dataLength: UInt32;
		serverEditParams: SHServerEditParameters;
		timeStamp: TimeValue64;              { lower 32 bits is original rtp timestamp}
		chunkFlags: SInt32;             { these are or'd together}
		flags: SInt32;
	end;
{ flags for RTPRssmPacket struct}
const
	kRTPRssmPacketHasMarkerBitSet = $00000001;
	kRTPRssmPacketHasServerEditFlag = $00010000;

{ flags for RTPRssmSendStreamBufferRange}
const
	kRTPRssmCanRefStreamBuffer = $00000001;

{ flags for RTPRssmSendPacketList}
const
	kRTPRssmLostSomePackets = $00000001;

{ flags for RTPRssmSetFlags}
const
	kRTPRssmEveryPacketAChunkFlag = $00000001;
	kRTPRssmQueueAndUseMarkerBitFlag = $00000002;
	kRTPRssmTrackLostPacketsFlag = $00010000;
	kRTPRssmNoReorderingRequiredFlag = $00020000;


type
	RTPSendStreamBufferRangeParamsPtr = ^RTPSendStreamBufferRangeParams;
	RTPSendStreamBufferRangeParams = record
		streamBuffer: QTSStreamBufferPtr;
		presentationTime: TimeValue64;
		chunkStartPosition: UInt32;
		numDataBytes: UInt32;
		chunkFlags: SInt32;
		flags: SInt32;
		serverEditParams: {const} SHServerEditParametersPtr; { NULL if no edit}
	end;
{ characteristics}
const
	kRTPCharacteristic_RequiresOrderedPackets = FourCharCode('rrop');
	kRTPCharacteristic_TimeStampsNotMonoIncreasing = FourCharCode('tsmi');


const
	kRTPReassemblerInfoResType = FourCharCode('rsmi');

type
	RTPReassemblerInfo = record
		characteristicCount: SIGNEDLONG;
		characteristic: array [0..0] of RTPPayloadCharacteristic;

                                              { after the last characteristic, the payload name (defined by the MediaPacketizerPayloadInfo}
                                              { structure) is present. }
	end;
	RTPReassemblerInfoPtr = ^RTPReassemblerInfo;
type
	RTPReassemblerInfoHandle = ^RTPReassemblerInfoPtr;
	{	 RTPReassemblerInfoElement structs are padded to 32 bits 	}
const
	kRTPReassemblerInfoPadUpToBytes = 4;


{
 *  QTSFindReassemblerForPayloadID()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function QTSFindReassemblerForPayloadID( inPayloadID: UInt8; var inSortInfo: RTPPayloadSortRequest; var outReassemblerList: QTAtomContainer ): OSErr; external name '_QTSFindReassemblerForPayloadID';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  QTSFindReassemblerForPayloadName()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function QTSFindReassemblerForPayloadName( inPayloadName: ConstCStringPtr; var inSortInfo: RTPPayloadSortRequest; var outReassemblerList: QTAtomContainer ): OSErr; external name '_QTSFindReassemblerForPayloadName';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{-----------------------------------------
    RTP Reassembler Selectors
-----------------------------------------}
const
	kRTPRssmSetCapabilitiesSelect = $0100;
	kRTPRssmGetCapabilitiesSelect = $0101;
	kRTPRssmSetPayloadHeaderLengthSelect = $0102;
	kRTPRssmGetPayloadHeaderLengthSelect = $0103;
	kRTPRssmSetTimeScaleSelect = $0104;
	kRTPRssmGetTimeScaleSelect = $0105;
	kRTPRssmNewStreamHandlerSelect = $0106;
	kRTPRssmSetStreamHandlerSelect = $0107;
	kRTPRssmGetStreamHandlerSelect = $0108;
	kRTPRssmSendStreamHandlerChangedSelect = $0109;
	kRTPRssmSetSampleDescriptionSelect = $010A;
	kRTPRssmGetChunkAndIncrRefCountSelect = $010D;
	kRTPRssmSendChunkAndDecrRefCountSelect = $010E;
	kRTPRssmSendLostChunkSelect = $010F;
	kRTPRssmSendStreamBufferRangeSelect = $0110;
	kRTPRssmClearCachedPackets = $0111;
	kRTPRssmFillPacketListParamsSelect = $0113;
	kRTPRssmReleasePacketListSelect = $0114;
	kRTPRssmIncrChunkRefCountSelect = $0115;
	kRTPRssmDecrChunkRefCountSelect = $0116;
	kRTPRssmGetExtChunkAndIncrRefCountSelect = $0117;
	kRTPRssmInitializeSelect = $0500;
	kRTPRssmHandleNewPacketSelect = $0501;
	kRTPRssmComputeChunkSizeSelect = $0502;
	kRTPRssmAdjustPacketParamsSelect = $0503;
	kRTPRssmCopyDataToChunkSelect = $0504;
	kRTPRssmSendPacketListSelect = $0505;
	kRTPRssmGetTimeScaleFromPacketSelect = $0506;
	kRTPRssmSetInfoSelect = $0509;
	kRTPRssmGetInfoSelect = $050A;
	kRTPRssmHasCharacteristicSelect = $050B;
	kRTPRssmResetSelect = $050C;

{-----------------------------------------
    RTP Reassembler functions - base to derived
-----------------------------------------}

{
 *  RTPRssmInitialize()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPRssmInitialize( rtpr: RTPReassembler; var inInitParams: RTPRssmInitParams ): ComponentResult; external name '_RTPRssmInitialize';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  RTPRssmHandleNewPacket()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPRssmHandleNewPacket( rtpr: RTPReassembler; var inStreamBuffer: QTSStreamBuffer; inNumWraparounds: SInt32 ): ComponentResult; external name '_RTPRssmHandleNewPacket';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  RTPRssmComputeChunkSize()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPRssmComputeChunkSize( rtpr: RTPReassembler; var inPacketListHead: RTPRssmPacket; inFlags: SInt32; var outChunkDataSize: UInt32 ): ComponentResult; external name '_RTPRssmComputeChunkSize';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  RTPRssmAdjustPacketParams()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPRssmAdjustPacketParams( rtpr: RTPReassembler; var inPacket: RTPRssmPacket; inFlags: SInt32 ): ComponentResult; external name '_RTPRssmAdjustPacketParams';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  RTPRssmCopyDataToChunk()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPRssmCopyDataToChunk( rtpr: RTPReassembler; var inPacketListHead: RTPRssmPacket; inMaxChunkDataSize: UInt32; var inChunk: SHChunkRecord; inFlags: SInt32 ): ComponentResult; external name '_RTPRssmCopyDataToChunk';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  RTPRssmSendPacketList()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPRssmSendPacketList( rtpr: RTPReassembler; var inPacketListHead: RTPRssmPacket; (*const*) var inLastChunkPresentationTime: TimeValue64; inFlags: SInt32 ): ComponentResult; external name '_RTPRssmSendPacketList';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  RTPRssmGetTimeScaleFromPacket()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPRssmGetTimeScaleFromPacket( rtpr: RTPReassembler; var inStreamBuffer: QTSStreamBuffer; var outTimeScale: TimeScale ): ComponentResult; external name '_RTPRssmGetTimeScaleFromPacket';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  RTPRssmSetInfo()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPRssmSetInfo( rtpr: RTPReassembler; inSelector: OSType; ioParams: UnivPtr ): ComponentResult; external name '_RTPRssmSetInfo';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  RTPRssmGetInfo()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPRssmGetInfo( rtpr: RTPReassembler; inSelector: OSType; ioParams: UnivPtr ): ComponentResult; external name '_RTPRssmGetInfo';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  RTPRssmHasCharacteristic()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPRssmHasCharacteristic( rtpr: RTPReassembler; inCharacteristic: OSType; var outHasIt: Boolean ): ComponentResult; external name '_RTPRssmHasCharacteristic';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  RTPRssmReset()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPRssmReset( rtpr: RTPReassembler; inFlags: SInt32 ): ComponentResult; external name '_RTPRssmReset';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{-----------------------------------------
    RTP Reassembler functions - derived to base
-----------------------------------------}
{ ----- setup}
{
 *  RTPRssmSetCapabilities()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPRssmSetCapabilities( rtpr: RTPReassembler; inFlags: SInt32; inFlagsMask: SInt32 ): ComponentResult; external name '_RTPRssmSetCapabilities';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  RTPRssmGetCapabilities()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPRssmGetCapabilities( rtpr: RTPReassembler; var outFlags: SInt32 ): ComponentResult; external name '_RTPRssmGetCapabilities';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  RTPRssmSetPayloadHeaderLength()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPRssmSetPayloadHeaderLength( rtpr: RTPReassembler; inPayloadHeaderLength: UInt32 ): ComponentResult; external name '_RTPRssmSetPayloadHeaderLength';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  RTPRssmGetPayloadHeaderLength()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPRssmGetPayloadHeaderLength( rtpr: RTPReassembler; var outPayloadHeaderLength: UInt32 ): ComponentResult; external name '_RTPRssmGetPayloadHeaderLength';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  RTPRssmSetTimeScale()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPRssmSetTimeScale( rtpr: RTPReassembler; inSHTimeScale: TimeScale ): ComponentResult; external name '_RTPRssmSetTimeScale';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  RTPRssmGetTimeScale()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPRssmGetTimeScale( rtpr: RTPReassembler; var outSHTimeScale: TimeScale ): ComponentResult; external name '_RTPRssmGetTimeScale';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  RTPRssmNewStreamHandler()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPRssmNewStreamHandler( rtpr: RTPReassembler; inSHType: OSType; inSampleDescription: SampleDescriptionHandle; inSHTimeScale: TimeScale; var outHandler: ComponentInstance ): ComponentResult; external name '_RTPRssmNewStreamHandler';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  RTPRssmSetStreamHandler()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPRssmSetStreamHandler( rtpr: RTPReassembler; inStreamHandler: ComponentInstance ): ComponentResult; external name '_RTPRssmSetStreamHandler';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  RTPRssmGetStreamHandler()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPRssmGetStreamHandler( rtpr: RTPReassembler; var outStreamHandler: ComponentInstance ): ComponentResult; external name '_RTPRssmGetStreamHandler';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  RTPRssmSendStreamHandlerChanged()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPRssmSendStreamHandlerChanged( rtpr: RTPReassembler ): ComponentResult; external name '_RTPRssmSendStreamHandlerChanged';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  RTPRssmSetSampleDescription()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPRssmSetSampleDescription( rtpr: RTPReassembler; inSampleDescription: SampleDescriptionHandle ): ComponentResult; external name '_RTPRssmSetSampleDescription';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{ ----- manually sending chunks}
{
 *  RTPRssmGetChunkAndIncrRefCount()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPRssmGetChunkAndIncrRefCount( rtpr: RTPReassembler; inChunkDataSize: UInt32; (*const*) var inChunkPresentationTime: TimeValue64; var outChunk: UnivPtr ): ComponentResult; external name '_RTPRssmGetChunkAndIncrRefCount';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  RTPRssmGetExtChunkAndIncrRefCount()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Non-Carbon CFM:   in QTStreamLib 6.0 and later
 *    Windows:          in qtmlClient.lib 6.0 and later
 }
function RTPRssmGetExtChunkAndIncrRefCount( rtpr: RTPReassembler; inChunkDataSize: UInt32; (*const*) var inChunkPresentationTime: TimeValue64; inFlags: SInt32; var outChunk: UnivPtr ): ComponentResult; external name '_RTPRssmGetExtChunkAndIncrRefCount';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{
 *  RTPRssmSendChunkAndDecrRefCount()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPRssmSendChunkAndDecrRefCount( rtpr: RTPReassembler; var inChunk: SHChunkRecord; const (*var*) inServerEdit: SHServerEditParameters ): ComponentResult; external name '_RTPRssmSendChunkAndDecrRefCount';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  RTPRssmSendLostChunk()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPRssmSendLostChunk( rtpr: RTPReassembler; (*const*) var inChunkPresentationTime: TimeValue64 ): ComponentResult; external name '_RTPRssmSendLostChunk';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  RTPRssmSendStreamBufferRange()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPRssmSendStreamBufferRange( rtpr: RTPReassembler; var inParams: RTPSendStreamBufferRangeParams ): ComponentResult; external name '_RTPRssmSendStreamBufferRange';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  RTPRssmClearCachedPackets()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPRssmClearCachedPackets( rtpr: RTPReassembler; inFlags: SInt32 ): ComponentResult; external name '_RTPRssmClearCachedPackets';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  RTPRssmFillPacketListParams()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPRssmFillPacketListParams( rtpr: RTPReassembler; var inPacketListHead: RTPRssmPacket; inNumWraparounds: SInt32; inFlags: SInt32 ): ComponentResult; external name '_RTPRssmFillPacketListParams';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  RTPRssmReleasePacketList()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPRssmReleasePacketList( rtpr: RTPReassembler; var inPacketListHead: RTPRssmPacket ): ComponentResult; external name '_RTPRssmReleasePacketList';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  RTPRssmIncrChunkRefCount()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPRssmIncrChunkRefCount( rtpr: RTPReassembler; var inChunk: SHChunkRecord ): ComponentResult; external name '_RTPRssmIncrChunkRefCount';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  RTPRssmDecrChunkRefCount()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPRssmDecrChunkRefCount( rtpr: RTPReassembler; var inChunk: SHChunkRecord ): ComponentResult; external name '_RTPRssmDecrChunkRefCount';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{============================================================================
        RTP Media Packetizer
============================================================================}
const
	kRTPMediaPacketizerType = FourCharCode('rtpm');

type
	RTPMediaPacketizer = ComponentInstance;
const
	kRTPBaseMediaPacketizerType = FourCharCode('gnrc');
	kRTP261MediaPacketizerType = FourCharCode('h261');
	kRTP263PlusMediaPacketizerType = FourCharCode('263+');
	kRTPAudioMediaPacketizerType = FourCharCode('soun');
	kRTPQTMediaPacketizerType = FourCharCode('qtim');
	kRTPPureVoiceMediaPacketizerType = FourCharCode('Qclp');
	kRTPJPEGMediaPacketizerType = FourCharCode('jpeg');
	kRTPQDesign2MediaPacketizerType = FourCharCode('QDM2');
	kRTPSorensonMediaPacketizerType = FourCharCode('SVQ1');
	kRTPMP3MediaPacketizerType = FourCharCode('mp3 ');
	kRTPMPEG4AudioMediaPacketizerType = FourCharCode('mp4a');
	kRTPMPEG4VideoMediaPacketizerType = FourCharCode('mp4v');
	kRTPAMRMediaPacketizerType = FourCharCode('amr ');

type
	RTPMPSampleRef = UInt32;
	RTPMPDataReleaseProcPtr = procedure( var inData: UInt8; inRefCon: UnivPtr );
	RTPMPDataReleaseUPP = RTPMPDataReleaseProcPtr;
const
	kMediaPacketizerCanPackEditRate = 1 shl 0;
	kMediaPacketizerCanPackLayer = 1 shl 1;
	kMediaPacketizerCanPackVolume = 1 shl 2;
	kMediaPacketizerCanPackBalance = 1 shl 3;
	kMediaPacketizerCanPackGraphicsMode = 1 shl 4;
	kMediaPacketizerCanPackEmptyEdit = 1 shl 5;


type
	MediaPacketizerRequirements = record
		mediaType: OSType;              { media type supported (0 for all)}
		dataFormat: OSType;             { data format (e.g., compression) supported (0 for all)}
		capabilityFlags: UInt32;        { ability to handle non-standard track characteristics}
		canPackMatrixType: UInt8;      { can pack any matrix type up to this (identityMatrixType for identity only)}
		reserved1: UInt8;
		reserved2: UInt8;
		reserved3: UInt8;
	end;
	MediaPacketizerRequirementsPtr = ^MediaPacketizerRequirements;
type
	MediaPacketizerInfo = record
		mediaType: OSType;              { media type supported (0 for all)}
		dataFormat: OSType;             { data format (e.g., compression) supported (0 for all)}
		vendor: OSType;                 { manufacturer of this packetizer (e.g., 'appl' for Apple)}
		capabilityFlags: UInt32;        { ability to handle non-standard track characteristics}
		canPackMatrixType: UInt8;      { can pack any matrix type up to this (identityMatrixType for identity only)}
		reserved1: UInt8;
		reserved2: UInt8;
		reserved3: UInt8;
		characteristicCount: SIGNEDLONG;
		characteristic: array [0..0] of RTPPayloadCharacteristic;

                                              { after the last characteristic, the payload name (defined by the RTPPayloadInfo}
                                              { structure) is present. }
	end;
	MediaPacketizerInfoPtr = ^MediaPacketizerInfo;
type
	MediaPacketizerInfoHandle = ^MediaPacketizerInfoPtr;
{ MediaPacketizerInfo structs are padded to 32 bits }
const
	kMediaPacketizerInfoPadUpToBytes = 4;

const
	kRTPMediaPacketizerInfoRezType = FourCharCode('pcki');


{
 *  QTSFindMediaPacketizer()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function QTSFindMediaPacketizer( inPacketizerinfo: MediaPacketizerRequirementsPtr; inSampleDescription: SampleDescriptionHandle; inSortInfo: RTPPayloadSortRequestPtr; var outPacketizerList: QTAtomContainer ): OSErr; external name '_QTSFindMediaPacketizer';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  QTSFindMediaPacketizerForTrack()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function QTSFindMediaPacketizerForTrack( inTrack: Track; inSampleDescriptionIndex: SIGNEDLONG; inSortInfo: RTPPayloadSortRequestPtr; var outPacketizerList: QTAtomContainer ): OSErr; external name '_QTSFindMediaPacketizerForTrack';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  QTSFindMediaPacketizerForPayloadID()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function QTSFindMediaPacketizerForPayloadID( payloadID: SIGNEDLONG; inSortInfo: RTPPayloadSortRequestPtr; var outPacketizerList: QTAtomContainer ): OSErr; external name '_QTSFindMediaPacketizerForPayloadID';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  QTSFindMediaPacketizerForPayloadName()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function QTSFindMediaPacketizerForPayloadName( payloadName: ConstCStringPtr; inSortInfo: RTPPayloadSortRequestPtr; var outPacketizerList: QTAtomContainer ): OSErr; external name '_QTSFindMediaPacketizerForPayloadName';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{ flags for RTPMPInitialize}
const
	kRTPMPRealtimeModeFlag = $00000001;

{ flags for RTPMPSampleDataParams}
const
	kRTPMPSyncSampleFlag = $00000001;
	kRTPMPRespectDurationFlag = $00000002;

type
	RTPMPSampleDataParamsPtr = ^RTPMPSampleDataParams;
	RTPMPSampleDataParams = record
		version: UInt32;
		timeStamp: UInt32;
		duration: UInt32;               { 0 = unknown duration}
		playOffset: UInt32;
		playRate: Fixed;
		flags: SInt32;
		sampleDescSeed: UInt32;
		sampleDescription: Handle;
		sampleRef: RTPMPSampleRef;
		dataLength: UInt32;
		data: {const} UInt8Ptr;
		releaseProc: RTPMPDataReleaseUPP;
		refCon: UnivPtr;
	end;
{ out flags for idle, RTPMPSetSampleData, and RTPMPFlush}
const
	kRTPMPStillProcessingData = $00000001; { not done with data you've got}

type
	RTPMPPayloadTypeParamsPtr = ^RTPMPPayloadTypeParams;
	RTPMPPayloadTypeParams = record
		flags: UInt32;
		payloadNumber: UInt32;
		nameLength: SInt16;             { in: size of payloadName buffer (counting null terminator) -- this will be reset to needed length and paramErr returned if too small }
		payloadName: CStringPtr;            { caller must provide buffer }
	end;
{-----------------------------------------
    RTP Media Packetizer Info selectors
-----------------------------------------}
{ info selectors - get only }
const
	kRTPMPPayloadTypeInfo = FourCharCode('rtpp'); { RTPMPPayloadTypeParams* }
	kRTPMPRTPTimeScaleInfo = FourCharCode('rtpt'); { TimeScale* }
	kRTPMPRequiredSampleDescriptionInfo = FourCharCode('sdsc'); { SampleDescriptionHandle* }
	kRTPMPMinPayloadSize = FourCharCode('mins'); { UInt32* in bytes, does not include rtp header; default is 0 }
	kRTPMPMinPacketDuration = FourCharCode('mind'); { UInt3* in milliseconds; default is no min required }
	kRTPMPSuggestedRepeatPktCountInfo = FourCharCode('srpc'); { UInt32* }
	kRTPMPSuggestedRepeatPktSpacingInfo = FourCharCode('srps'); { UInt32* in milliseconds }
	kRTPMPMaxPartialSampleSizeInfo = FourCharCode('mpss'); { UInt32* in bytes }
	kRTPMPPreferredBufferDelayInfo = FourCharCode('prbd'); { UInt32* in milliseconds }
	kRTPMPPayloadNameInfo = FourCharCode('name'); { StringPtr }
	kRTPInfo_FormatString = FourCharCode('fmtp'); { char **, caller allocates ptr, callee disposes }

{-----------------------------------------
    RTP Media Packetizer Characteristics
-----------------------------------------}
{ also supports relevant ones in Movies.h and QTSToolbox.h }
const
	kRTPMPNoSampleDataRequiredCharacteristic = FourCharCode('nsdr');
	kRTPMPHasUserSettingsDialogCharacteristic = FourCharCode('sdlg');
	kRTPMPPrefersReliableTransportCharacteristic = FourCharCode('rely');
	kRTPMPRequiresOutOfBandDimensionsCharacteristic = FourCharCode('robd');
	kRTPMPReadsPartialSamplesCharacteristic = FourCharCode('rpsp');

{-----------------------------------------
    RTP Media Packetizer selectors
-----------------------------------------}
const
	kRTPMPInitializeSelect = $0500;
	kRTPMPPreflightMediaSelect = $0501;
	kRTPMPIdleSelect = $0502;
	kRTPMPSetSampleDataSelect = $0503;
	kRTPMPFlushSelect = $0504;
	kRTPMPResetSelect = $0505;
	kRTPMPSetInfoSelect = $0506;
	kRTPMPGetInfoSelect = $0507;
	kRTPMPSetTimeScaleSelect = $0508;
	kRTPMPGetTimeScaleSelect = $0509;
	kRTPMPSetTimeBaseSelect = $050A;
	kRTPMPGetTimeBaseSelect = $050B;
	kRTPMPHasCharacteristicSelect = $050C;
	kRTPMPSetPacketBuilderSelect = $050E;
	kRTPMPGetPacketBuilderSelect = $050F;
	kRTPMPSetMediaTypeSelect = $0510;
	kRTPMPGetMediaTypeSelect = $0511;
	kRTPMPSetMaxPacketSizeSelect = $0512;
	kRTPMPGetMaxPacketSizeSelect = $0513;
	kRTPMPSetMaxPacketDurationSelect = $0514;
	kRTPMPGetMaxPacketDurationSelect = $0515; { for export component and apps who want to}
                                        { access dialogs for Media-specific settings}
                                        { (such as Pure Voice interleave factor)}
	kRTPMPDoUserDialogSelect = $0516;
	kRTPMPSetSettingsFromAtomContainerAtAtomSelect = $0517;
	kRTPMPGetSettingsIntoAtomContainerAtAtomSelect = $0518;
	kRTPMPGetSettingsAsTextSelect = $0519;
	kRTPMPGetSettingsSelect = $051C;
	kRTPMPSetSettingsSelect = $051D;

{-----------------------------------------
    RTP Media Packetizer functions
-----------------------------------------}

{
 *  RTPMPInitialize()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPMPInitialize( rtpm: RTPMediaPacketizer; inFlags: SInt32 ): ComponentResult; external name '_RTPMPInitialize';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{ return noErr if you can handle this media }
{
 *  RTPMPPreflightMedia()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPMPPreflightMedia( rtpm: RTPMediaPacketizer; inMediaType: OSType; inSampleDescription: SampleDescriptionHandle ): ComponentResult; external name '_RTPMPPreflightMedia';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
   do work here if you need to - give up time periodically
   if you're doing time consuming operations
}
{
 *  RTPMPIdle()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPMPIdle( rtpm: RTPMediaPacketizer; inFlags: SInt32; var outFlags: SInt32 ): ComponentResult; external name '_RTPMPIdle';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
   caller owns the RTPMPSampleDataParams struct
   media Packetizer must copy any fields of the struct it wants to keep
   media Packetizer must call release proc when done with the data
   you can do the processing work here if it does not take up too
   much cpu time - otherwise do it in idle
}
{
 *  RTPMPSetSampleData()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPMPSetSampleData( rtpm: RTPMediaPacketizer; const (*var*) inSampleData: RTPMPSampleDataParams; var outFlags: SInt32 ): ComponentResult; external name '_RTPMPSetSampleData';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
   send everything you have buffered - you will get idles while
   you set the kRTPMPStillProcessingData flag here and in idle
}
{
 *  RTPMPFlush()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPMPFlush( rtpm: RTPMediaPacketizer; inFlags: SInt32; var outFlags: SInt32 ): ComponentResult; external name '_RTPMPFlush';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
   dispose of anything buffered and get rid of state
   do not send the buffered data (because presumably
   there is no connection for you to send on)
   state should be the same as if you were just initialized
}
{
 *  RTPMPReset()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPMPReset( rtpm: RTPMediaPacketizer; inFlags: SInt32 ): ComponentResult; external name '_RTPMPReset';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{-----------------------------------------
    RTP Media Packetizer get / set functions
-----------------------------------------}
{
 *  RTPMPSetInfo()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPMPSetInfo( rtpm: RTPMediaPacketizer; inSelector: OSType; ioParams: {const} UnivPtr ): ComponentResult; external name '_RTPMPSetInfo';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  RTPMPGetInfo()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPMPGetInfo( rtpm: RTPMediaPacketizer; inSelector: OSType; ioParams: UnivPtr ): ComponentResult; external name '_RTPMPGetInfo';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  RTPMPSetTimeScale()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPMPSetTimeScale( rtpm: RTPMediaPacketizer; inTimeScale: TimeScale ): ComponentResult; external name '_RTPMPSetTimeScale';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  RTPMPGetTimeScale()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPMPGetTimeScale( rtpm: RTPMediaPacketizer; var outTimeScale: TimeScale ): ComponentResult; external name '_RTPMPGetTimeScale';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  RTPMPSetTimeBase()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPMPSetTimeBase( rtpm: RTPMediaPacketizer; inTimeBase: TimeBase ): ComponentResult; external name '_RTPMPSetTimeBase';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  RTPMPGetTimeBase()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPMPGetTimeBase( rtpm: RTPMediaPacketizer; var outTimeBase: TimeBase ): ComponentResult; external name '_RTPMPGetTimeBase';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  RTPMPHasCharacteristic()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPMPHasCharacteristic( rtpm: RTPMediaPacketizer; inSelector: OSType; var outHasIt: Boolean ): ComponentResult; external name '_RTPMPHasCharacteristic';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  RTPMPSetPacketBuilder()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPMPSetPacketBuilder( rtpm: RTPMediaPacketizer; inPacketBuilder: ComponentInstance ): ComponentResult; external name '_RTPMPSetPacketBuilder';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  RTPMPGetPacketBuilder()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPMPGetPacketBuilder( rtpm: RTPMediaPacketizer; var outPacketBuilder: ComponentInstance ): ComponentResult; external name '_RTPMPGetPacketBuilder';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  RTPMPSetMediaType()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPMPSetMediaType( rtpm: RTPMediaPacketizer; inMediaType: OSType ): ComponentResult; external name '_RTPMPSetMediaType';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  RTPMPGetMediaType()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPMPGetMediaType( rtpm: RTPMediaPacketizer; var outMediaType: OSType ): ComponentResult; external name '_RTPMPGetMediaType';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{ size is in bytes}
{
 *  RTPMPSetMaxPacketSize()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPMPSetMaxPacketSize( rtpm: RTPMediaPacketizer; inMaxPacketSize: UInt32 ): ComponentResult; external name '_RTPMPSetMaxPacketSize';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  RTPMPGetMaxPacketSize()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPMPGetMaxPacketSize( rtpm: RTPMediaPacketizer; var outMaxPacketSize: UInt32 ): ComponentResult; external name '_RTPMPGetMaxPacketSize';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{ duration is in milliseconds}
{
 *  RTPMPSetMaxPacketDuration()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPMPSetMaxPacketDuration( rtpm: RTPMediaPacketizer; inMaxPacketDuration: UInt32 ): ComponentResult; external name '_RTPMPSetMaxPacketDuration';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  RTPMPGetMaxPacketDuration()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPMPGetMaxPacketDuration( rtpm: RTPMediaPacketizer; var outMaxPacketDuration: UInt32 ): ComponentResult; external name '_RTPMPGetMaxPacketDuration';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  RTPMPDoUserDialog()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPMPDoUserDialog( rtpm: RTPMediaPacketizer; inFilterUPP: ModalFilterUPP; var canceled: Boolean ): ComponentResult; external name '_RTPMPDoUserDialog';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  RTPMPSetSettingsFromAtomContainerAtAtom()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPMPSetSettingsFromAtomContainerAtAtom( rtpm: RTPMediaPacketizer; inContainer: QTAtomContainer; inParentAtom: QTAtom ): ComponentResult; external name '_RTPMPSetSettingsFromAtomContainerAtAtom';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  RTPMPGetSettingsIntoAtomContainerAtAtom()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPMPGetSettingsIntoAtomContainerAtAtom( rtpm: RTPMediaPacketizer; inOutContainer: QTAtomContainer; inParentAtom: QTAtom ): ComponentResult; external name '_RTPMPGetSettingsIntoAtomContainerAtAtom';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  RTPMPGetSettingsAsText()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPMPGetSettingsAsText( rtpm: RTPMediaPacketizer; var text: Handle ): ComponentResult; external name '_RTPMPGetSettingsAsText';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  RTPMPGetSettings()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Non-Carbon CFM:   in QTStreamLib 5.0 and later
 *    Windows:          in QTSClient.lib 5.0 and later
 }
function RTPMPGetSettings( rtpm: RTPMediaPacketizer; var outSettings: QTAtomContainer; inFlags: SInt32 ): ComponentResult; external name '_RTPMPGetSettings';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  RTPMPSetSettings()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Non-Carbon CFM:   in QTStreamLib 5.0 and later
 *    Windows:          in QTSClient.lib 5.0 and later
 }
function RTPMPSetSettings( rtpm: RTPMediaPacketizer; inSettings: QTAtomSpecPtr; inFlags: SInt32 ): ComponentResult; external name '_RTPMPSetSettings';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{============================================================================
        RTP Packet Builder
============================================================================}
const
	kRTPPacketBuilderType = FourCharCode('rtpb');


type
	RTPPacketBuilder = ComponentInstance;
	RTPPacketGroupRef = ^SInt32; { an opaque type }
	RTPPacketRef = ^SInt32; { an opaque type }
	RTPPacketRepeatedDataRef = ^SInt32; { an opaque type }
{ flags for RTPPBBegin/EndPacket, RTPPBBegin/EndPacketGroup}
const
	kRTPPBSetMarkerFlag = $00000001;
	kRTPPBRepeatPacketFlag = $00000002;
	kRTPPBSyncSampleFlag = $00010000;
	kRTPPBBFrameFlag = $00020000;
	kRTPPBDontSendFlag = $10000000; { when set in EndPacketGroup, will not add group}

const
	kRTPPBUnknownPacketMediaDataLength = 0;

{ flags for RTPPBGetSampleData}
const
	kRTPPBEndOfDataFlag = $00000001;


type
	RTPPBCallbackProcPtr = procedure( inSelector: OSType; ioParams: UnivPtr; inRefCon: UnivPtr );
	RTPPBCallbackUPP = RTPPBCallbackProcPtr;
{-----------------------------------------
    RTP Packet Builder selectors
-----------------------------------------}
const
	kRTPPBBeginPacketGroupSelect = $0500;
	kRTPPBEndPacketGroupSelect = $0501;
	kRTPPBBeginPacketSelect = $0502;
	kRTPPBEndPacketSelect = $0503;
	kRTPPBAddPacketLiteralDataSelect = $0504;
	kRTPPBAddPacketSampleDataSelect = $0505;
	kRTPPBAddPacketRepeatedDataSelect = $0506;
	kRTPPBReleaseRepeatedDataSelect = $0507;
	kRTPPBSetPacketSequenceNumberSelect = $0508;
	kRTPPBGetPacketSequenceNumberSelect = $0509;
	kRTPPBSetCallbackSelect = $050A;
	kRTPPBGetCallbackSelect = $050B;
	kRTPPBSetInfoSelect = $050C;
	kRTPPBGetInfoSelect = $050D;
	kRTPPBSetPacketTimeStampOffsetSelect = $050E;
	kRTPPBGetPacketTimeStampOffsetSelect = $050F;
	kRTPPBAddPacketSampleData64Select = $0510;
	kRTPPBGetSampleDataSelect = $0511;
	kRTPPBAddRepeatPacketSelect = $0512;

{-----------------------------------------
    RTP Packet Builder functions
-----------------------------------------}
{
 *  RTPPBBeginPacketGroup()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPPBBeginPacketGroup( rtpb: RTPPacketBuilder; inFlags: SInt32; inTimeStamp: UInt32; var outPacketGroup: RTPPacketGroupRef ): ComponentResult; external name '_RTPPBBeginPacketGroup';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  RTPPBEndPacketGroup()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPPBEndPacketGroup( rtpb: RTPPacketBuilder; inFlags: SInt32; inPacketGroup: RTPPacketGroupRef ): ComponentResult; external name '_RTPPBEndPacketGroup';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  RTPPBBeginPacket()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPPBBeginPacket( rtpb: RTPPacketBuilder; inFlags: SInt32; inPacketGroup: RTPPacketGroupRef; inPacketMediaDataLength: UInt32; var outPacket: RTPPacketRef ): ComponentResult; external name '_RTPPBBeginPacket';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  RTPPBEndPacket()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPPBEndPacket( rtpb: RTPPacketBuilder; inFlags: SInt32; inPacketGroup: RTPPacketGroupRef; inPacket: RTPPacketRef; inTransmissionTimeOffset: UInt32; inDuration: UInt32 ): ComponentResult; external name '_RTPPBEndPacket';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
   non-NULL RTPPacketRepeatedDataRef means this data will be repeated later
   pb must return a repeated data ref
}
{
 *  RTPPBAddPacketLiteralData()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPPBAddPacketLiteralData( rtpb: RTPPacketBuilder; inFlags: SInt32; inPacketGroup: RTPPacketGroupRef; inPacket: RTPPacketRef; var inData: UInt8; inDataLength: UInt32; var outDataRef: RTPPacketRepeatedDataRef ): ComponentResult; external name '_RTPPBAddPacketLiteralData';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
   non-NULL RTPPacketRepeatedDataRef means this data will be repeated later
   pb must return a repeated data ref
}
{
 *  RTPPBAddPacketSampleData()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPPBAddPacketSampleData( rtpb: RTPPacketBuilder; inFlags: SInt32; inPacketGroup: RTPPacketGroupRef; inPacket: RTPPacketRef; var inSampleDataParams: RTPMPSampleDataParams; inSampleOffset: UInt32; inSampleDataLength: UInt32; var outDataRef: RTPPacketRepeatedDataRef ): ComponentResult; external name '_RTPPBAddPacketSampleData';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
   non-NULL RTPPacketRepeatedDataRef means this data will be repeated later
   pb must return a repeated data ref
}
{
 *  RTPPBAddPacketSampleData64()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Non-Carbon CFM:   in QTStreamLib 5.0 and later
 *    Windows:          in QTSClient.lib 5.0 and later
 }
function RTPPBAddPacketSampleData64( rtpb: RTPPacketBuilder; inFlags: SInt32; inPacketGroup: RTPPacketGroupRef; inPacket: RTPPacketRef; var inSampleDataParams: RTPMPSampleDataParams; (*const*) var inSampleOffset: UInt64; inSampleDataLength: UInt32; var outDataRef: RTPPacketRepeatedDataRef ): ComponentResult; external name '_RTPPBAddPacketSampleData64';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
   call to add the repeated data using the ref you got from
   RTPPBAddPacketLiteralData or RTPPBAddPacketSampleData
}
{
 *  RTPPBAddPacketRepeatedData()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPPBAddPacketRepeatedData( rtpb: RTPPacketBuilder; inFlags: SInt32; inPacketGroup: RTPPacketGroupRef; inPacket: RTPPacketRef; inDataRef: RTPPacketRepeatedDataRef ): ComponentResult; external name '_RTPPBAddPacketRepeatedData';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{ call when done with repeated data}
{
 *  RTPPBReleaseRepeatedData()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPPBReleaseRepeatedData( rtpb: RTPPacketBuilder; inDataRef: RTPPacketRepeatedDataRef ): ComponentResult; external name '_RTPPBReleaseRepeatedData';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
   seq number is just relative seq number
   don't call if you don't care when seq # is used
}
{
 *  RTPPBSetPacketSequenceNumber()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPPBSetPacketSequenceNumber( rtpb: RTPPacketBuilder; inFlags: SInt32; inPacketGroup: RTPPacketGroupRef; inPacket: RTPPacketRef; inSequenceNumber: UInt32 ): ComponentResult; external name '_RTPPBSetPacketSequenceNumber';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  RTPPBGetPacketSequenceNumber()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPPBGetPacketSequenceNumber( rtpb: RTPPacketBuilder; inFlags: SInt32; inPacketGroup: RTPPacketGroupRef; inPacket: RTPPacketRef; var outSequenceNumber: UInt32 ): ComponentResult; external name '_RTPPBGetPacketSequenceNumber';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  RTPPBSetPacketTimeStampOffset()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Non-Carbon CFM:   in QTStreamLib 5.0 and later
 *    Windows:          in QTSClient.lib 5.0 and later
 }
function RTPPBSetPacketTimeStampOffset( rtpb: RTPPacketBuilder; inFlags: SInt32; inPacketGroup: RTPPacketGroupRef; inPacket: RTPPacketRef; inTimeStampOffset: SInt32 ): ComponentResult; external name '_RTPPBSetPacketTimeStampOffset';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  RTPPBGetPacketTimeStampOffset()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Non-Carbon CFM:   in QTStreamLib 5.0 and later
 *    Windows:          in QTSClient.lib 5.0 and later
 }
function RTPPBGetPacketTimeStampOffset( rtpb: RTPPacketBuilder; inFlags: SInt32; inPacketGroup: RTPPacketGroupRef; inPacket: RTPPacketRef; var outTimeStampOffset: SInt32 ): ComponentResult; external name '_RTPPBGetPacketTimeStampOffset';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  RTPPBAddRepeatPacket()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Non-Carbon CFM:   in QTStreamLib 5.0 and later
 *    Windows:          in QTSClient.lib 5.0 and later
 }
function RTPPBAddRepeatPacket( rtpb: RTPPacketBuilder; inFlags: SInt32; inPacketGroup: RTPPacketGroupRef; inPacket: RTPPacketRef; inTransmissionOffset: TimeValue; inSequenceNumber: UInt32 ): ComponentResult; external name '_RTPPBAddRepeatPacket';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
   used for communicating with the caller of the media packetizers if needed
   NOT used for communicating with the media packetizers themselves
}
{
 *  RTPPBSetCallback()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPPBSetCallback( rtpb: RTPPacketBuilder; inCallback: RTPPBCallbackUPP; inRefCon: UnivPtr ): ComponentResult; external name '_RTPPBSetCallback';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  RTPPBGetCallback()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPPBGetCallback( rtpb: RTPPacketBuilder; var outCallback: RTPPBCallbackUPP; var outRefCon: UnivPtr ): ComponentResult; external name '_RTPPBGetCallback';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  RTPPBSetInfo()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPPBSetInfo( rtpb: RTPPacketBuilder; inSelector: OSType; ioParams: UnivPtr ): ComponentResult; external name '_RTPPBSetInfo';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  RTPPBGetInfo()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPPBGetInfo( rtpb: RTPPacketBuilder; inSelector: OSType; ioParams: UnivPtr ): ComponentResult; external name '_RTPPBGetInfo';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  RTPPBGetSampleData()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Non-Carbon CFM:   in QTStreamLib 5.0 and later
 *    Windows:          in QTSClient.lib 5.0 and later
 }
function RTPPBGetSampleData( rtpb: RTPPacketBuilder; var inParams: RTPMPSampleDataParams; (*const*) var inStartOffset: UInt64; var outDataBuffer: UInt8; inBytesToRead: UInt32; var outBytesRead: UInt32; var outFlags: SInt32 ): ComponentResult; external name '_RTPPBGetSampleData';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{ UPP call backs }
{
 *  NewRTPMPDataReleaseUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function NewRTPMPDataReleaseUPP( userRoutine: RTPMPDataReleaseProcPtr ): RTPMPDataReleaseUPP; external name '_NewRTPMPDataReleaseUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  NewRTPPBCallbackUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function NewRTPPBCallbackUPP( userRoutine: RTPPBCallbackProcPtr ): RTPPBCallbackUPP; external name '_NewRTPPBCallbackUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  DisposeRTPMPDataReleaseUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure DisposeRTPMPDataReleaseUPP( userUPP: RTPMPDataReleaseUPP ); external name '_DisposeRTPMPDataReleaseUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  DisposeRTPPBCallbackUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure DisposeRTPPBCallbackUPP( userUPP: RTPPBCallbackUPP ); external name '_DisposeRTPPBCallbackUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  InvokeRTPMPDataReleaseUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure InvokeRTPMPDataReleaseUPP( var inData: UInt8; inRefCon: UnivPtr; userUPP: RTPMPDataReleaseUPP ); external name '_InvokeRTPMPDataReleaseUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  InvokeRTPPBCallbackUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure InvokeRTPPBCallbackUPP( inSelector: OSType; ioParams: UnivPtr; inRefCon: UnivPtr; userUPP: RTPPBCallbackUPP ); external name '_InvokeRTPPBCallbackUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{$endc} {not TARGET_CPU_64}

{$endc} {TARGET_OS_MAC}
{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}

end.
{$endc} {not MACOSALLINCLUDE}
