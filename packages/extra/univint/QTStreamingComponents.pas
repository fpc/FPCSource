{
     File:       QTStreamingComponents.p
 
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

unit QTStreamingComponents;
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
uses MacTypes,Dialogs,Components,Movies,QuickTimeStreaming;


{$ALIGN MAC68K}

{============================================================================
        Stream Sourcer
============================================================================}

const
	kQTSSourcerType				= $73726372 (* 'srcr' *);


type
	QTSSourcer							= ComponentInstance;

const
	kQTSSGChannelSourcerType	= $73676368 (* 'sgch' *);
	kQTSMovieTrackSourcerType	= $7472616B (* 'trak' *);
	kQTSPushDataSourcerType		= $70757368 (* 'push' *);

	{	 flags for sourcer data 	}
	kQTSSourcerDataFlag_SyncSample = $00000001;
	kQTSPushDataSourcerFlag_SampleTimeIsValid = $80000000;


	kQTSSourcerInitParamsVersion1 = 1;


type
	QTSSourcerInitParamsPtr = ^QTSSourcerInitParams;
	QTSSourcerInitParams = record
		version:				SInt32;
		flags:					SInt32;
		dataType:				OSType;
		data:					Ptr;
		dataLength:				UInt32;
	end;

	{
	 *  QTSNewSourcer()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in QTStreamLib 5.0 and later
	 *    CarbonLib:        in CarbonLib 1.3 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
function QTSNewSourcer(params: UnivPtr; const (*var*) inInitParams: QTSSourcerInitParams; inFlags: SInt32; var outSourcer: ComponentInstance): OSErr; external name '_QTSNewSourcer';

{ info selectors for sourcers - get and set }

const
	kQTSInfo_Track				= $7472616B (* 'trak' *);						{  QTSTrackParams*  }
	kQTSInfo_Loop				= $6C6F6F70 (* 'loop' *);						{  QTSLoopParams*  }
	kQTSInfo_SourcerTiming		= $7374696D (* 'stim' *);						{  QTSSourcerTimingParams*  }
	kQTSInfo_TargetFrameRate	= $74667073 (* 'tfps' *);						{  Fixed * in frames per second  }
	kQTSInfo_PushData			= $70757368 (* 'push' *);						{  QTSPushDataParams*  }
	kQTSInfo_SourcerCallbackProc = $73636270 (* 'scbp' *);						{  QTSSourcerCallbackProcParams*  }
	kQTSInfo_TargetDataRate		= $74647274 (* 'tdrt' *);						{  UInt32 * in bytes per second  }
	kQTSInfo_AudioAutoGainOnOff	= $61676320 (* 'agc ' *);						{  Boolean*  - error if unavailable }
	kQTSInfo_AudioGain			= $6761696E (* 'gain' *);						{  Fixed* kFixed1 is unity gain  }
	kQTSInfo_CroppedInputRect	= $63727072 (* 'crpr' *);						{  Rect* - defined relative to kQTSInfo_FullInputRect below  }
	kQTSInfo_SpatialSettings	= $7370746C (* 'sptl' *);						{  pointer to SCSpatialSettings struct }
	kQTSInfo_TemporalSettings	= $7470726C (* 'tprl' *);						{  pointer to SCTemporalSettings struct }
	kQTSInfo_DataRateSettings	= $64726174 (* 'drat' *);						{  pointer to SCDataRateSettings struct }
	kQTSInfo_CodecFlags			= $63666C67 (* 'cflg' *);						{  pointer to CodecFlags }
	kQTSInfo_CodecSettings		= $63646563 (* 'cdec' *);						{  pointer to Handle }
	kQTSInfo_ForceKeyValue		= $6B73696D (* 'ksim' *);						{  pointer to long }
	kQTSInfo_SoundSampleRate	= $73737274 (* 'ssrt' *);						{  pointer to UnsignedFixed }
	kQTSInfo_SoundSampleSize	= $73737373 (* 'ssss' *);						{  pointer to short }
	kQTSInfo_SoundChannelCount	= $73736363 (* 'sscc' *);						{  pointer to short }
	kQTSInfo_SoundCompression	= $73736374 (* 'ssct' *);						{  pointer to OSType }
	kQTSInfo_CompressionList	= $6374796C (* 'ctyl' *);						{  pointer to OSType Handle }
	kQTSInfo_VideoHue			= $68756520 (* 'hue ' *);						{  UInt16*  }
	kQTSInfo_VideoSaturation	= $73617472 (* 'satr' *);						{  UInt16*  }
	kQTSInfo_VideoContrast		= $74727374 (* 'trst' *);						{  UInt16*  }
	kQTSInfo_VideoBrightness	= $62726974 (* 'brit' *);						{  UInt16*  }
	kQTSInfo_VideoSharpness		= $73687270 (* 'shrp' *);						{  UInt16*  }
	kQTSInfo_TimeScale			= $7363616C (* 'scal' *);						{  UInt32*  }
	kQTSInfo_SGChannelDeviceName = $696E6E6D (* 'innm' *);						{  Handle*  }
	kQTSInfo_SGChannelDeviceList = $7372646C (* 'srdl' *);						{  SGDeviceList*  }
	kQTSInfo_SGChannelDeviceInput = $73646969 (* 'sdii' *);						{  short*  }
	kQTSInfo_SGChannelSettings	= $73657367 (* 'sesg' *);						{  QTSSGChannelSettingsParams  }
	kQTSInfo_PreviewWhileRecordingMode = $73727072 (* 'srpr' *);				{  Boolean*  }
	kQTSInfo_CompressionParams	= $73636370 (* 'sccp' *);						{  QTAtomContainer*  }

	{	 info selectors for sourcers - get only	}
	kQTSInfo_SGChannel			= $73676368 (* 'sgch' *);						{  SGChannel*  }
	kQTSInfo_SGChannelInputName	= $73726E6D (* 'srnm' *);						{  Handle*  }
	kQTSInfo_FullInputRect		= $66756C72 (* 'fulr' *);						{  Rect*  }

	{	 loop flags 	}
	kQTSLoopFlag_Loop			= $00000001;

	kQTSLoopParamsVersion1		= 1;


type
	QTSLoopParamsPtr = ^QTSLoopParams;
	QTSLoopParams = record
		version:				SInt32;
		flags:					SInt32;
		loopFlags:				SInt32;
		flagsMask:				SInt32;
		numLoops:				SInt32;
	end;


const
	kQTSTrackParamsVersion1		= 1;


type
	QTSTrackParamsPtr = ^QTSTrackParams;
	QTSTrackParams = record
		version:				SInt32;
		flags:					SInt32;
		track:					Track_fix;
		trackStartOffset:		TimeValue64;							{  to start other than at the beginning otherwise set to 0 }
		duration:				TimeValue64;							{  to limit the duration otherwise set to 0 }
		loopParams:				QTSLoopParamsPtr;						{  set to NULL if not using; default is no looping  }
	end;


const
	kQTSSourcerTimingParamsVersion1 = 1;


type
	QTSSourcerTimingParamsPtr = ^QTSSourcerTimingParams;
	QTSSourcerTimingParams = record
		version:				SInt32;
		flags:					SInt32;
		timeScale:				TimeScale_fix;
		presentationStartTime:	TimeValue64;
		presentationEndTime:	TimeValue64;
		presentationCurrentTime: TimeValue64;
		localStartTime:			TimeValue64;
		localEndTime:			TimeValue64;
		localCurrentTime:		TimeValue64;
	end;


const
	kQTSPushDataParamsVersion1	= 1;

	kQTSPushDataFlag_SampleTimeIsValid = $00000001;
	kQTSPushDataFlag_DurationIsValid = $00000002;


type
	QTSPushDataParamsPtr = ^QTSPushDataParams;
	QTSPushDataParams = record
		version:				SInt32;
		flags:					SInt32;
		sampleDescription:		SampleDescriptionHandle;				{  caller owns the handle  }
		sampleDescSeed:			UInt32;
		sampleTime:				TimeValue64;							{  also set flag if you set this  }
		duration:				TimeValue64;							{  also set flag if you set this  }
		dataLength:				UInt32;
		dataPtr:				Ptr;									{  this does not have to be a real macintosh Ptr  }
	end;


const
	kQTSSourcerCallbackProcParamsVersion1 = 1;


type
	QTSSourcerCallbackProcParamsPtr = ^QTSSourcerCallbackProcParams;
	QTSSourcerCallbackProcParams = record
		version:				SInt32;
		flags:					SInt32;
		proc:					QTSNotificationUPP;
		refCon:					Ptr;
	end;

	{  track sourcer callback selectors }

const
	kQTSSourcerCallback_Done	= $646F6E65 (* 'done' *);						{  QTSSourcerDoneParams*  }


	{  push data sourcer callback selectors }
	kQTSPushDataSourcerCallback_HasCharacteristic = $050D;		{  QTSPushDataHasCharacteristicParams*  }
	kQTSPushDataSourcerCallback_SetInfo = $0507;				{  QTSPushDataInfoParams*  }
	kQTSPushDataSourcerCallback_GetInfo = $0508;				{  QTSPushDataInfoParams*  }


type
	QTSPushDataHasCharacteristicParamsPtr = ^QTSPushDataHasCharacteristicParams;
	QTSPushDataHasCharacteristicParams = record
		version:				SInt32;
		flags:					SInt32;
		characteristic:			OSType;
		returnedHasIt:			boolean;
		reserved1:				SInt8;
		reserved2:				SInt8;
		reserved3:				SInt8;
	end;

	QTSPushDataInfoParamsPtr = ^QTSPushDataInfoParams;
	QTSPushDataInfoParams = record
		version:				SInt32;
		flags:					SInt32;
		selector:				OSType;
		ioParams:				Ptr;
	end;


const
	kQTSSourcerDoneParamsVersion1 = 1;


type
	QTSSourcerDoneParamsPtr = ^QTSSourcerDoneParams;
	QTSSourcerDoneParams = record
		version:				SInt32;
		flags:					SInt32;
		sourcer:				ComponentInstance;
	end;

	QTSSGChannelSettingsParamsPtr = ^QTSSGChannelSettingsParams;
	QTSSGChannelSettingsParams = record
		settings:				UserData;
		flags:					SInt32;
	end;


	{	-----------------------------------------
	    Stream Sourcer Selectors
	-----------------------------------------	}

const
	kQTSSourcerInitializeSelect	= $0500;
	kQTSSourcerSetEnableSelect	= $0503;
	kQTSSourcerGetEnableSelect	= $0504;
	kQTSSourcerSetInfoSelect	= $0507;
	kQTSSourcerGetInfoSelect	= $0508;
	kQTSSourcerSetTimeScaleSelect = $050E;
	kQTSSourcerGetTimeScaleSelect = $050F;
	kQTSSourcerIdleSelect		= $0516;

	{	-----------------------------------------
	    Stream Sourcer Prototypes
	-----------------------------------------	}
	{
	 *  QTSSourcerInitialize()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in QTStreamLib 5.0.1 and later
	 *    CarbonLib:        in CarbonLib 1.6 and later
	 *    Mac OS X:         in version 10.1 and later
	 	}
function QTSSourcerInitialize(inSourcer: QTSSourcer; const (*var*) inInitParams: QTSSourcerInitParams): ComponentResult; external name '_QTSSourcerInitialize';
{
 *  QTSSourcerIdle()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 5.0 and later
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 }
function QTSSourcerIdle(inSourcer: QTSSourcer; (*const*) var inTime: TimeValue64; inFlags: SInt32; var outFlags: SInt32): ComponentResult; external name '_QTSSourcerIdle';
{
 *  QTSSourcerSetEnable()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 5.0 and later
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 }
function QTSSourcerSetEnable(inSourcer: QTSSourcer; inEnableMode: boolean; inFlags: SInt32): ComponentResult; external name '_QTSSourcerSetEnable';
{
 *  QTSSourcerGetEnable()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 5.0 and later
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 }
function QTSSourcerGetEnable(inSourcer: QTSSourcer; var outEnableMode: boolean; inFlags: SInt32): ComponentResult; external name '_QTSSourcerGetEnable';
{
 *  QTSSourcerSetTimeScale()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 5.0 and later
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 }
function QTSSourcerSetTimeScale(inSourcer: QTSSourcer; inTimeScale: TimeScale): ComponentResult; external name '_QTSSourcerSetTimeScale';
{
 *  QTSSourcerGetTimeScale()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 5.0 and later
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 }
function QTSSourcerGetTimeScale(inSourcer: QTSSourcer; var outTimeScale: TimeScale): ComponentResult; external name '_QTSSourcerGetTimeScale';
{
 *  QTSSourcerSetInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 5.0 and later
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 }
function QTSSourcerSetInfo(inSourcer: QTSSourcer; inSelector: OSType; ioParams: UnivPtr): ComponentResult; external name '_QTSSourcerSetInfo';
{
 *  QTSSourcerGetInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 5.0 and later
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 }
function QTSSourcerGetInfo(inSourcer: QTSSourcer; inSelector: OSType; ioParams: UnivPtr): ComponentResult; external name '_QTSSourcerGetInfo';
const
	kQTSInfo_InputDeviceName	= $696E6E6D (* 'innm' *);						{  Handle*  }
	kQTSInfo_InputSourceName	= $73726E6D (* 'srnm' *);						{  Handle*  }


	{	============================================================================
	        Stream Handler
	============================================================================	}

	{	
	    Server edits are only valid for the current chunk
		}

type
	SHServerEditParametersPtr = ^SHServerEditParameters;
	SHServerEditParameters = record
		version:				UInt32;
		editRate:				Fixed;
		dataStartTime_mediaAxis: TimeValue64;
		dataEndTime_mediaAxis:	TimeValue64;
	end;


const
	kSHNoChunkDispatchFlags		= 0;
	kSHChunkFlagSyncSample		= $04;
	kSHChunkFlagDataLoss		= $10;
	kSHChunkFlagExtended		= $20;


type
	SHChunkRecordPtr = ^SHChunkRecord;
	SHChunkRecord = record
		version:				UInt32;
		reserved1:				SInt32;
		flags:					SInt32;
		dataSize:				UInt32;
		dataPtr:				Ptr;
		reserved2:				SInt32;
		reserved3:				SInt32;
		presentationTime:		TimeValue64;
		reserved4:				SInt32;
		reserved5:				SInt32;
		serverEditParameters:	SHServerEditParametersPtr;
		reserved6:				SInt32;
		reserved7:				SInt32;
	end;


const
	kSHNumExtendedDataLongs		= 10;

	kSHExtendedChunkFlag_HasSampleCount = $01;
	kSHExtendedChunkFlag_HasFrameLengths = $02;


type
	SHExtendedChunkRecordPtr = ^SHExtendedChunkRecord;
	SHExtendedChunkRecord = record
		chunk:					SHChunkRecord;
		extendedFlags:			SInt32;
		extendedData:			array [0..9] of SInt32;
	end;


	{	============================================================================
	        RTP Components
	============================================================================	}
	RTPSSRC								= UInt32;

const
	kRTPInvalidSSRC				= 0;


	{	 RTP standard content encodings for audio 	}
	kRTPPayload_PCMU			= 0;							{  8kHz PCM mu-law mono  }
	kRTPPayload_1016			= 1;							{  8kHz CELP (Fed Std 1016) mono  }
	kRTPPayload_G721			= 2;							{  8kHz G.721 ADPCM mono  }
	kRTPPayload_GSM				= 3;							{  8kHz GSM mono  }
	kRTPPayload_G723			= 4;							{  8kHz G.723 ADPCM mono  }
	kRTPPayload_DVI_8			= 5;							{  8kHz Intel DVI ADPCM mono  }
	kRTPPayload_DVI_16			= 6;							{  16kHz Intel DVI ADPCM mono  }
	kRTPPayload_LPC				= 7;							{  8kHz LPC  }
	kRTPPayload_PCMA			= 8;							{  8kHz PCM a-law mono  }
	kRTPPayload_L16_44_2		= 10;							{  44.1kHz 16-bit linear stereo  }
	kRTPPayload_L16_44_1		= 11;							{  44.1kHz 16-bit linear mono  }
	kRTPPayload_PureVoice		= 12;							{  8kHz PureVoice mono (QCELP)  }
	kRTPPayload_MPEGAUDIO		= 14;							{  MPEG I and II audio  }
	kRTPPayload_DVI_11			= 16;							{  11kHz Intel DVI ADPCM mono  }
	kRTPPayload_DVI_22			= 17;							{  22kHz Intel DVI ADPCM mono  }

	{	 RTP standard content encodings for video 	}
	kRTPPayload_CELLB			= 25;							{  Sun CellB  }
	kRTPPayload_JPEG			= 26;							{  JPEG  }
	kRTPPayload_CUSEEME			= 27;							{  Cornell CU-SeeMe  }
	kRTPPayload_NV				= 28;							{  Xerox PARC nv  }
	kRTPPayload_PICWIN			= 29;							{  BBN Picture Window  }
	kRTPPayload_CPV				= 30;							{  Bolter CPV  }
	kRTPPayload_H261			= 31;							{  CCITT H.261  }
	kRTPPayload_MPEGVIDEO		= 32;							{  MPEG I and II video  }
	kRTPPayload_H263			= 34;							{  CCITT H.263  }

	{	 Other RTP standard content encodings 	}
	kRTPPayload_MPEG2T			= 33;							{  MPEG 2 Transport  }

	{	 Dynamic encodings 	}
	kRTPPayload_FirstDynamic	= 96;
	kRTPPayload_LastDynamic		= 127;
	kRTPPayload_Unknown			= $FF;


	{	
	-----------------------------------------
	    RTP Info selectors
	-----------------------------------------
		}
	{	 ----- these are get and set ----- 	}
	kRTPInfo_SSRC				= $73737263 (* 'ssrc' *);						{  UInt32*  }
	kRTPInfo_NextSeqNum			= $726E736E (* 'rnsn' *);						{  UInt16*  }

	{	-----------------------------------------
	    RTP Statistics
	-----------------------------------------	}
	kRTPTotalReceivedPktsStat	= $74726370 (* 'trcp' *);
	kRTPTotalLostPktsStat		= $746C7370 (* 'tlsp' *);
	kRTPTotalProcessedPktsStat	= $74707270 (* 'tprp' *);
	kRTPTotalDroppedPktsStat	= $74647270 (* 'tdrp' *);
	kRTPBadHeaderDroppedPktsStat = $62686470 (* 'bhdp' *);
	kRTPOurHeaderDroppedPktsStat = $6F686470 (* 'ohdp' *);
	kRTPNotReceivingSenderDroppedPktsStat = $6E736470 (* 'nsdp' *);
	kRTPNotProcessingDroppedPktsStat = $6E706470 (* 'npdp' *);
	kRTPBadSeqDroppedPktsStat	= $62736470 (* 'bsdp' *);
	kRTPArriveTooLatePktsStat	= $6172746C (* 'artl' *);
	kRTPWaitForSeqDroppedPktsStat = $77736470 (* 'wsdp' *);
	kRTPBadStateDroppedPktsStat	= $73746470 (* 'stdp' *);
	kRTPBadPayloadDroppedPktsStat = $62706470 (* 'bpdp' *);
	kRTPNoTimeScaleDroppedPktsStat = $6E746470 (* 'ntdp' *);
	kRTPDupSeqNumDroppedPktsStat = $64736470 (* 'dsdp' *);
	kRTPLostPktsPercentStat		= $6C737070 (* 'lspp' *);
	kRTPDroppedPktsPercentStat	= $64707070 (* 'dppp' *);
	kRTPTotalUnprocessedPktsPercentStat = $74757070 (* 'tupp' *);
	kRTPRTCPDataRateStat		= $72726364 (* 'rrcd' *);
	kRTPPayloadIDStat			= $72706964 (* 'rpid' *);
	kRTPPayloadNameStat			= $72706E6D (* 'rpnm' *);
	kRTPNumPktsInQueueStat		= $726E7071 (* 'rnpq' *);
	kRTPTotalPktsInQueueStat	= $72747071 (* 'rtpq' *);
	kRTPTotalOutOfOrderPktsStat	= $72746F6F (* 'rtoo' *);
	kRTPRetransmissionStat		= $72727478 (* 'rrtx' *);


	{	-----------------------------------------
	    Payload Info
	-----------------------------------------	}
	kRTPPayloadSpeedTag			= $73706564 (* 'sped' *);						{  0-255, 255 is fastest }
	kRTPPayloadLossRecoveryTag	= $6C6F7373 (* 'loss' *);						{  0-255, 0 can't handle any loss, 128 can handle 50% packet loss }
	kRTPPayloadConformanceTag	= $636F6E66 (* 'conf' *);						{  more than one of these can be present }


type
	RTPPayloadCharacteristicPtr = ^RTPPayloadCharacteristic;
	RTPPayloadCharacteristic = record
		tag:					OSType;
		value:					SInt32;
	end;

	{	
	    pass RTPPayloadSortRequest to QTSFindMediaPacketizer or QTSFindMediaPacketizerForTrack.
	    define the characteristics to sort by. tag is key to sort on. value is positive for ascending
	    sort (low value first), negative for descending sort (high value first).
		}
	RTPPayloadSortRequestPtr = ^RTPPayloadSortRequest;
	RTPPayloadSortRequest = record
		characteristicCount:	SInt32;
		characteristic:			array [0..0] of RTPPayloadCharacteristic; {  tag is key to sort on, value is + for ascending, - for descending }
	end;

	{	 flags for RTPPayloadInfo 	}

const
	kRTPPayloadTypeStaticFlag	= $00000001;
	kRTPPayloadTypeDynamicFlag	= $00000002;


type
	RTPPayloadInfoPtr = ^RTPPayloadInfo;
	RTPPayloadInfo = record
		payloadFlags:			SInt32;
		payloadID:				SInt8;
		reserved1:				SInt8;
		reserved2:				SInt8;
		reserved3:				SInt8;
		payloadName:			SInt8;
	end;

	RTPPayloadInfoHandle				= ^RTPPayloadInfoPtr;
	{	============================================================================
	        RTP Reassembler
	============================================================================	}
	RTPReassembler						= ComponentInstance;

const
	kRTPReassemblerType			= $72747072 (* 'rtpr' *);

	kRTPBaseReassemblerType		= $676E7263 (* 'gnrc' *);
	kRTP261ReassemblerType		= $68323631 (* 'h261' *);
	kRTP263ReassemblerType		= $68323633 (* 'h263' *);
	kRTP263PlusReassemblerType	= $3236332B (* '263+' *);
	kRTPAudioReassemblerType	= $736F756E (* 'soun' *);
	kRTPQTReassemblerType		= $7174696D (* 'qtim' *);
	kRTPPureVoiceReassemblerType = $51636C70 (* 'Qclp' *);
	kRTPJPEGReassemblerType		= $6A706567 (* 'jpeg' *);
	kRTPQDesign2ReassemblerType	= $51444D32 (* 'QDM2' *);
	kRTPSorensonReassemblerType	= $53565131 (* 'SVQ1' *);
	kRTPMP3ReassemblerType		= $6D703320 (* 'mp3 ' *);
	kRTPMPEG4AudioReassemblerType = $6D703461 (* 'mp4a' *);
	kRTPMPEG4VideoReassemblerType = $6D703476 (* 'mp4v' *);


type
	RTPRssmInitParamsPtr = ^RTPRssmInitParams;
	RTPRssmInitParams = record
		ssrc:					RTPSSRC;
		payloadType:			SInt8;
		reserved1:				SInt8;
		reserved2:				SInt8;
		reserved3:				SInt8;
		timeBase:				TimeBase_fix;
		timeScale:				TimeScale_fix;
	end;

	RTPDescParamsPtr = ^RTPDescParams;
	RTPDescParams = record
		container:				QTAtomContainer;
		presentationParentAtom:	QTAtom;
		streamParentAtom:		QTAtom;
	end;

	RTPRssmMoreInitParamsPtr = ^RTPRssmMoreInitParams;
	RTPRssmMoreInitParams = record
		initParams:				RTPRssmInitParams;
		version:				SInt32;
		desc:					RTPDescParams;
	end;


const
	kRTPRssmMoreInitParamsVersion1 = 1;


	{  get/set info selectors }
	kRTPRssmInfo_MoreInitParams	= $72726D69 (* 'rrmi' *);


type
	RTPRssmPacketPtr = ^RTPRssmPacket;
	RTPRssmPacket = record
		next:					RTPRssmPacketPtr;
		prev:					RTPRssmPacketPtr;
		streamBuffer:			QTSStreamBufferPtr;
		paramsFilledIn:			boolean;
		reserved:				SInt8;
		sequenceNum:			UInt16;
		transportHeaderLength:	UInt32;									{  filled in by base }
		payloadHeaderLength:	UInt32;									{  derived adjusts this  }
		dataLength:				UInt32;
		serverEditParams:		SHServerEditParameters;
		timeStamp:				TimeValue64;							{  lower 32 bits is original rtp timestamp }
		chunkFlags:				SInt32;									{  these are or'd together }
		flags:					SInt32;
	end;

	{  flags for RTPRssmPacket struct }

const
	kRTPRssmPacketHasMarkerBitSet = $00000001;
	kRTPRssmPacketHasServerEditFlag = $00010000;

	{  flags for RTPRssmSendStreamBufferRange }
	kRTPRssmCanRefStreamBuffer	= $00000001;

	{  flags for RTPRssmSendPacketList }
	kRTPRssmLostSomePackets		= $00000001;

	{  flags for RTPRssmSetFlags }
	kRTPRssmEveryPacketAChunkFlag = $00000001;
	kRTPRssmQueueAndUseMarkerBitFlag = $00000002;
	kRTPRssmTrackLostPacketsFlag = $00010000;
	kRTPRssmNoReorderingRequiredFlag = $00020000;


type
	RTPSendStreamBufferRangeParamsPtr = ^RTPSendStreamBufferRangeParams;
	RTPSendStreamBufferRangeParams = record
		streamBuffer:			QTSStreamBufferPtr;
		presentationTime:		TimeValue64;
		chunkStartPosition:		UInt32;
		numDataBytes:			UInt32;
		chunkFlags:				SInt32;
		flags:					SInt32;
		serverEditParams:		SHServerEditParametersPtr;				{  NULL if no edit }
	end;

	{  characteristics }

const
	kRTPCharacteristic_RequiresOrderedPackets = $72726F70 (* 'rrop' *);
	kRTPCharacteristic_TimeStampsNotMonoIncreasing = $74736D69 (* 'tsmi' *);


	kRTPReassemblerInfoResType	= $72736D69 (* 'rsmi' *);


type
	RTPReassemblerInfoPtr = ^RTPReassemblerInfo;
	RTPReassemblerInfo = record
		characteristicCount:	SInt32;
		characteristic:			array [0..0] of RTPPayloadCharacteristic;
																		{  after the last characteristic, the payload name (defined by the MediaPacketizerPayloadInfo }
																		{  structure) is present.  }
	end;

	RTPReassemblerInfoHandle			= ^RTPReassemblerInfoPtr;
	{	 RTPReassemblerInfoElement structs are padded to 32 bits 	}

const
	kRTPReassemblerInfoPadUpToBytes = 4;


	{
	 *  QTSFindReassemblerForPayloadID()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
	 *    CarbonLib:        in CarbonLib 1.1 and later
	 *    Mac OS X:         in version 10.0 and later
	 *    Windows:          in QTSClient.lib 4.0 and later
	 	}
function QTSFindReassemblerForPayloadID(inPayloadID: ByteParameter; var inSortInfo: RTPPayloadSortRequest; var outReassemblerList: QTAtomContainer): OSErr; external name '_QTSFindReassemblerForPayloadID';

{
 *  QTSFindReassemblerForPayloadName()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function QTSFindReassemblerForPayloadName(inPayloadName: ConstCStringPtr; var inSortInfo: RTPPayloadSortRequest; var outReassemblerList: QTAtomContainer): OSErr; external name '_QTSFindReassemblerForPayloadName';

{-----------------------------------------
    RTP Reassembler Selectors
-----------------------------------------}

const
	kRTPRssmSetCapabilitiesSelect = $0100;
	kRTPRssmGetCapabilitiesSelect = $0101;
	kRTPRssmSetPayloadHeaderLengthSelect = $0102;
	kRTPRssmGetPayloadHeaderLengthSelect = $0103;
	kRTPRssmSetTimeScaleSelect	= $0104;
	kRTPRssmGetTimeScaleSelect	= $0105;
	kRTPRssmNewStreamHandlerSelect = $0106;
	kRTPRssmSetStreamHandlerSelect = $0107;
	kRTPRssmGetStreamHandlerSelect = $0108;
	kRTPRssmSendStreamHandlerChangedSelect = $0109;
	kRTPRssmSetSampleDescriptionSelect = $010A;
	kRTPRssmGetChunkAndIncrRefCountSelect = $010D;
	kRTPRssmSendChunkAndDecrRefCountSelect = $010E;
	kRTPRssmSendLostChunkSelect	= $010F;
	kRTPRssmSendStreamBufferRangeSelect = $0110;
	kRTPRssmClearCachedPackets	= $0111;
	kRTPRssmFillPacketListParamsSelect = $0113;
	kRTPRssmReleasePacketListSelect = $0114;
	kRTPRssmIncrChunkRefCountSelect = $0115;
	kRTPRssmDecrChunkRefCountSelect = $0116;
	kRTPRssmGetExtChunkAndIncrRefCountSelect = $0117;
	kRTPRssmInitializeSelect	= $0500;
	kRTPRssmHandleNewPacketSelect = $0501;
	kRTPRssmComputeChunkSizeSelect = $0502;
	kRTPRssmAdjustPacketParamsSelect = $0503;
	kRTPRssmCopyDataToChunkSelect = $0504;
	kRTPRssmSendPacketListSelect = $0505;
	kRTPRssmGetTimeScaleFromPacketSelect = $0506;
	kRTPRssmSetInfoSelect		= $0509;
	kRTPRssmGetInfoSelect		= $050A;
	kRTPRssmHasCharacteristicSelect = $050B;
	kRTPRssmResetSelect			= $050C;

	{	-----------------------------------------
	    RTP Reassembler functions - base to derived
	-----------------------------------------	}

	{
	 *  RTPRssmInitialize()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
	 *    CarbonLib:        in CarbonLib 1.1 and later
	 *    Mac OS X:         in version 10.0 and later
	 *    Windows:          in QTSClient.lib 4.0 and later
	 	}
function RTPRssmInitialize(rtpr: RTPReassembler; var inInitParams: RTPRssmInitParams): ComponentResult; external name '_RTPRssmInitialize';
{
 *  RTPRssmHandleNewPacket()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPRssmHandleNewPacket(rtpr: RTPReassembler; var inStreamBuffer: QTSStreamBuffer; inNumWraparounds: SInt32): ComponentResult; external name '_RTPRssmHandleNewPacket';
{
 *  RTPRssmComputeChunkSize()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPRssmComputeChunkSize(rtpr: RTPReassembler; var inPacketListHead: RTPRssmPacket; inFlags: SInt32; var outChunkDataSize: UInt32): ComponentResult; external name '_RTPRssmComputeChunkSize';
{
 *  RTPRssmAdjustPacketParams()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPRssmAdjustPacketParams(rtpr: RTPReassembler; var inPacket: RTPRssmPacket; inFlags: SInt32): ComponentResult; external name '_RTPRssmAdjustPacketParams';
{
 *  RTPRssmCopyDataToChunk()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPRssmCopyDataToChunk(rtpr: RTPReassembler; var inPacketListHead: RTPRssmPacket; inMaxChunkDataSize: UInt32; var inChunk: SHChunkRecord; inFlags: SInt32): ComponentResult; external name '_RTPRssmCopyDataToChunk';
{
 *  RTPRssmSendPacketList()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPRssmSendPacketList(rtpr: RTPReassembler; var inPacketListHead: RTPRssmPacket; (*const*) var inLastChunkPresentationTime: TimeValue64; inFlags: SInt32): ComponentResult; external name '_RTPRssmSendPacketList';
{
 *  RTPRssmGetTimeScaleFromPacket()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPRssmGetTimeScaleFromPacket(rtpr: RTPReassembler; var inStreamBuffer: QTSStreamBuffer; var outTimeScale: TimeScale): ComponentResult; external name '_RTPRssmGetTimeScaleFromPacket';
{
 *  RTPRssmSetInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPRssmSetInfo(rtpr: RTPReassembler; inSelector: OSType; ioParams: UnivPtr): ComponentResult; external name '_RTPRssmSetInfo';
{
 *  RTPRssmGetInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPRssmGetInfo(rtpr: RTPReassembler; inSelector: OSType; ioParams: UnivPtr): ComponentResult; external name '_RTPRssmGetInfo';
{
 *  RTPRssmHasCharacteristic()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPRssmHasCharacteristic(rtpr: RTPReassembler; inCharacteristic: OSType; var outHasIt: boolean): ComponentResult; external name '_RTPRssmHasCharacteristic';
{
 *  RTPRssmReset()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPRssmReset(rtpr: RTPReassembler; inFlags: SInt32): ComponentResult; external name '_RTPRssmReset';
{-----------------------------------------
    RTP Reassembler functions - derived to base
-----------------------------------------}
{  ----- setup }
{
 *  RTPRssmSetCapabilities()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPRssmSetCapabilities(rtpr: RTPReassembler; inFlags: SInt32; inFlagsMask: SInt32): ComponentResult; external name '_RTPRssmSetCapabilities';
{
 *  RTPRssmGetCapabilities()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPRssmGetCapabilities(rtpr: RTPReassembler; var outFlags: SInt32): ComponentResult; external name '_RTPRssmGetCapabilities';
{
 *  RTPRssmSetPayloadHeaderLength()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPRssmSetPayloadHeaderLength(rtpr: RTPReassembler; inPayloadHeaderLength: UInt32): ComponentResult; external name '_RTPRssmSetPayloadHeaderLength';
{
 *  RTPRssmGetPayloadHeaderLength()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPRssmGetPayloadHeaderLength(rtpr: RTPReassembler; var outPayloadHeaderLength: UInt32): ComponentResult; external name '_RTPRssmGetPayloadHeaderLength';
{
 *  RTPRssmSetTimeScale()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPRssmSetTimeScale(rtpr: RTPReassembler; inSHTimeScale: TimeScale): ComponentResult; external name '_RTPRssmSetTimeScale';
{
 *  RTPRssmGetTimeScale()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPRssmGetTimeScale(rtpr: RTPReassembler; var outSHTimeScale: TimeScale): ComponentResult; external name '_RTPRssmGetTimeScale';
{
 *  RTPRssmNewStreamHandler()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPRssmNewStreamHandler(rtpr: RTPReassembler; inSHType: OSType; inSampleDescription: SampleDescriptionHandle; inSHTimeScale: TimeScale; var outHandler: ComponentInstance): ComponentResult; external name '_RTPRssmNewStreamHandler';
{
 *  RTPRssmSetStreamHandler()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPRssmSetStreamHandler(rtpr: RTPReassembler; inStreamHandler: ComponentInstance): ComponentResult; external name '_RTPRssmSetStreamHandler';
{
 *  RTPRssmGetStreamHandler()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPRssmGetStreamHandler(rtpr: RTPReassembler; var outStreamHandler: ComponentInstance): ComponentResult; external name '_RTPRssmGetStreamHandler';
{
 *  RTPRssmSendStreamHandlerChanged()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPRssmSendStreamHandlerChanged(rtpr: RTPReassembler): ComponentResult; external name '_RTPRssmSendStreamHandlerChanged';
{
 *  RTPRssmSetSampleDescription()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPRssmSetSampleDescription(rtpr: RTPReassembler; inSampleDescription: SampleDescriptionHandle): ComponentResult; external name '_RTPRssmSetSampleDescription';
{  ----- manually sending chunks }
{
 *  RTPRssmGetChunkAndIncrRefCount()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPRssmGetChunkAndIncrRefCount(rtpr: RTPReassembler; inChunkDataSize: UInt32; (*const*) var inChunkPresentationTime: TimeValue64; var outChunk: UnivPtr): ComponentResult; external name '_RTPRssmGetChunkAndIncrRefCount';
{
 *  RTPRssmGetExtChunkAndIncrRefCount()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 6.0 and later
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.2 and later
 *    Windows:          in qtmlClient.lib 6.0 and later
 }
function RTPRssmGetExtChunkAndIncrRefCount(rtpr: RTPReassembler; inChunkDataSize: UInt32; (*const*) var inChunkPresentationTime: TimeValue64; inFlags: SInt32; var outChunk: UnivPtr): ComponentResult; external name '_RTPRssmGetExtChunkAndIncrRefCount';
{
 *  RTPRssmSendChunkAndDecrRefCount()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPRssmSendChunkAndDecrRefCount(rtpr: RTPReassembler; var inChunk: SHChunkRecord; const (*var*) inServerEdit: SHServerEditParameters): ComponentResult; external name '_RTPRssmSendChunkAndDecrRefCount';
{
 *  RTPRssmSendLostChunk()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPRssmSendLostChunk(rtpr: RTPReassembler; (*const*) var inChunkPresentationTime: TimeValue64): ComponentResult; external name '_RTPRssmSendLostChunk';
{
 *  RTPRssmSendStreamBufferRange()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPRssmSendStreamBufferRange(rtpr: RTPReassembler; var inParams: RTPSendStreamBufferRangeParams): ComponentResult; external name '_RTPRssmSendStreamBufferRange';
{
 *  RTPRssmClearCachedPackets()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPRssmClearCachedPackets(rtpr: RTPReassembler; inFlags: SInt32): ComponentResult; external name '_RTPRssmClearCachedPackets';
{
 *  RTPRssmFillPacketListParams()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPRssmFillPacketListParams(rtpr: RTPReassembler; var inPacketListHead: RTPRssmPacket; inNumWraparounds: SInt32; inFlags: SInt32): ComponentResult; external name '_RTPRssmFillPacketListParams';
{
 *  RTPRssmReleasePacketList()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPRssmReleasePacketList(rtpr: RTPReassembler; var inPacketListHead: RTPRssmPacket): ComponentResult; external name '_RTPRssmReleasePacketList';
{
 *  RTPRssmIncrChunkRefCount()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPRssmIncrChunkRefCount(rtpr: RTPReassembler; var inChunk: SHChunkRecord): ComponentResult; external name '_RTPRssmIncrChunkRefCount';
{
 *  RTPRssmDecrChunkRefCount()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPRssmDecrChunkRefCount(rtpr: RTPReassembler; var inChunk: SHChunkRecord): ComponentResult; external name '_RTPRssmDecrChunkRefCount';
{============================================================================
        RTP Media Packetizer
============================================================================}

const
	kRTPMediaPacketizerType		= $7274706D (* 'rtpm' *);


type
	RTPMediaPacketizer					= ComponentInstance;

const
	kRTPBaseMediaPacketizerType	= $676E7263 (* 'gnrc' *);
	kRTP261MediaPacketizerType	= $68323631 (* 'h261' *);
	kRTP263PlusMediaPacketizerType = $3236332B (* '263+' *);
	kRTPAudioMediaPacketizerType = $736F756E (* 'soun' *);
	kRTPQTMediaPacketizerType	= $7174696D (* 'qtim' *);
	kRTPPureVoiceMediaPacketizerType = $51636C70 (* 'Qclp' *);
	kRTPJPEGMediaPacketizerType	= $6A706567 (* 'jpeg' *);
	kRTPQDesign2MediaPacketizerType = $51444D32 (* 'QDM2' *);
	kRTPSorensonMediaPacketizerType = $53565131 (* 'SVQ1' *);
	kRTPMP3MediaPacketizerType	= $6D703320 (* 'mp3 ' *);
	kRTPMPEG4AudioMediaPacketizerType = $6D703461 (* 'mp4a' *);
	kRTPMPEG4VideoMediaPacketizerType = $6D703476 (* 'mp4v' *);


type
	RTPMPSampleRef						= UInt32;
{$ifc TYPED_FUNCTION_POINTERS}
	RTPMPDataReleaseProcPtr = procedure(var inData: UInt8; inRefCon: UnivPtr);
{$elsec}
	RTPMPDataReleaseProcPtr = ProcPtr;
{$endc}

{$ifc OPAQUE_UPP_TYPES}
	RTPMPDataReleaseUPP = ^SInt32; { an opaque UPP }
{$elsec}
	RTPMPDataReleaseUPP = UniversalProcPtr;
{$endc}	

const
	kMediaPacketizerCanPackEditRate = $01;
	kMediaPacketizerCanPackLayer = $02;
	kMediaPacketizerCanPackVolume = $04;
	kMediaPacketizerCanPackBalance = $08;
	kMediaPacketizerCanPackGraphicsMode = $10;
	kMediaPacketizerCanPackEmptyEdit = $20;


type
	MediaPacketizerRequirementsPtr = ^MediaPacketizerRequirements;
	MediaPacketizerRequirements = record
		mediaType:				OSType;									{  media type supported (0 for all) }
		dataFormat:				OSType;									{  data format (e.g., compression) supported (0 for all) }
		capabilityFlags:		UInt32;									{  ability to handle non-standard track characteristics }
		canPackMatrixType:		SInt8;									{  can pack any matrix type up to this (identityMatrixType for identity only) }
		reserved1:				SInt8;
		reserved2:				SInt8;
		reserved3:				SInt8;
	end;

	MediaPacketizerInfoPtr = ^MediaPacketizerInfo;
	MediaPacketizerInfo = record
		mediaType:				OSType;									{  media type supported (0 for all) }
		dataFormat:				OSType;									{  data format (e.g., compression) supported (0 for all) }
		vendor:					OSType;									{  manufacturer of this packetizer (e.g., 'appl' for Apple) }
		capabilityFlags:		UInt32;									{  ability to handle non-standard track characteristics }
		canPackMatrixType:		SInt8;									{  can pack any matrix type up to this (identityMatrixType for identity only) }
		reserved1:				SInt8;
		reserved2:				SInt8;
		reserved3:				SInt8;
		characteristicCount:	SInt32;
		characteristic:			array [0..0] of RTPPayloadCharacteristic;
																		{  after the last characteristic, the payload name (defined by the RTPPayloadInfo }
																		{  structure) is present.  }
	end;

	MediaPacketizerInfoHandle			= ^MediaPacketizerInfoPtr;
	{	 MediaPacketizerInfo structs are padded to 32 bits 	}

const
	kMediaPacketizerInfoPadUpToBytes = 4;

	{
	 *  QTSFindMediaPacketizer()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
	 *    CarbonLib:        in CarbonLib 1.1 and later
	 *    Mac OS X:         in version 10.0 and later
	 *    Windows:          in QTSClient.lib 4.0 and later
	 	}
function QTSFindMediaPacketizer(inPacketizerinfo: MediaPacketizerRequirementsPtr; inSampleDescription: SampleDescriptionHandle; inSortInfo: RTPPayloadSortRequestPtr; var outPacketizerList: QTAtomContainer): OSErr; external name '_QTSFindMediaPacketizer';

{
 *  QTSFindMediaPacketizerForTrack()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function QTSFindMediaPacketizerForTrack(inTrack: Track; inSampleDescriptionIndex: SInt32; inSortInfo: RTPPayloadSortRequestPtr; var outPacketizerList: QTAtomContainer): OSErr; external name '_QTSFindMediaPacketizerForTrack';

{
 *  QTSFindMediaPacketizerForPayloadID()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function QTSFindMediaPacketizerForPayloadID(payloadID: SInt32; inSortInfo: RTPPayloadSortRequestPtr; var outPacketizerList: QTAtomContainer): OSErr; external name '_QTSFindMediaPacketizerForPayloadID';

{
 *  QTSFindMediaPacketizerForPayloadName()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function QTSFindMediaPacketizerForPayloadName(payloadName: ConstCStringPtr; inSortInfo: RTPPayloadSortRequestPtr; var outPacketizerList: QTAtomContainer): OSErr; external name '_QTSFindMediaPacketizerForPayloadName';

{  flags for RTPMPInitialize }

const
	kRTPMPRealtimeModeFlag		= $00000001;

	{  flags for RTPMPSampleDataParams }
	kRTPMPSyncSampleFlag		= $00000001;
	kRTPMPRespectDurationFlag	= $00000002;


type
	RTPMPSampleDataParamsPtr = ^RTPMPSampleDataParams;
	RTPMPSampleDataParams = record
		version:				UInt32;
		timeStamp:				UInt32;
		duration:				UInt32;									{  0 = unknown duration }
		playOffset:				UInt32;
		playRate:				Fixed;
		flags:					SInt32;
		sampleDescSeed:			UInt32;
		sampleDescription:		Handle;
		sampleRef:				RTPMPSampleRef;
		dataLength:				UInt32;
		data:					Ptr;
		releaseProc:			RTPMPDataReleaseUPP;
		refCon:					Ptr;
	end;

	{  out flags for idle, RTPMPSetSampleData, and RTPMPFlush }

const
	kRTPMPStillProcessingData	= $00000001;					{  not done with data you've got }


type
	RTPMPPayloadTypeParamsPtr = ^RTPMPPayloadTypeParams;
	RTPMPPayloadTypeParams = record
		flags:					UInt32;
		payloadNumber:			UInt32;
		nameLength:				SInt16;								{  in: size of payloadName buffer (counting null terminator) -- this will be reset to needed length and paramErr returned if too small  }
		payloadName:			CStringPtr;								{  caller must provide buffer  }
	end;

	{	-----------------------------------------
	    RTP Media Packetizer Info selectors
	-----------------------------------------	}
	{	 info selectors - get only 	}

const
	kRTPMPPayloadTypeInfo		= $72747070 (* 'rtpp' *);						{  RTPMPPayloadTypeParams*  }
	kRTPMPRTPTimeScaleInfo		= $72747074 (* 'rtpt' *);						{  TimeScale*  }
	kRTPMPRequiredSampleDescriptionInfo = $73647363 (* 'sdsc' *);				{  SampleDescriptionHandle*  }
	kRTPMPMinPayloadSize		= $6D696E73 (* 'mins' *);						{  UInt32* in bytes, does not include rtp header; default is 0  }
	kRTPMPMinPacketDuration		= $6D696E64 (* 'mind' *);						{  UInt3* in milliseconds; default is no min required  }
	kRTPMPSuggestedRepeatPktCountInfo = $73727063 (* 'srpc' *);					{  UInt32*  }
	kRTPMPSuggestedRepeatPktSpacingInfo = $73727073 (* 'srps' *);				{  UInt32* in milliseconds  }
	kRTPMPMaxPartialSampleSizeInfo = $6D707373 (* 'mpss' *);					{  UInt32* in bytes  }
	kRTPMPPreferredBufferDelayInfo = $70726264 (* 'prbd' *);					{  UInt32* in milliseconds  }
	kRTPMPPayloadNameInfo		= $6E616D65 (* 'name' *);						{  StringPtr  }
	kRTPInfo_FormatString		= $666D7470 (* 'fmtp' *);						{  char **, caller allocates ptr, callee disposes  }

	{	-----------------------------------------
	    RTP Media Packetizer Characteristics
	-----------------------------------------	}
	{	 also supports relevant ones in Movies.h and QTSToolbox.h 	}
	kRTPMPNoSampleDataRequiredCharacteristic = $6E736472 (* 'nsdr' *);
	kRTPMPHasUserSettingsDialogCharacteristic = $73646C67 (* 'sdlg' *);
	kRTPMPPrefersReliableTransportCharacteristic = $72656C79 (* 'rely' *);
	kRTPMPRequiresOutOfBandDimensionsCharacteristic = $726F6264 (* 'robd' *);
	kRTPMPReadsPartialSamplesCharacteristic = $72707370 (* 'rpsp' *);

	{	-----------------------------------------
	    RTP Media Packetizer selectors
	-----------------------------------------	}
	kRTPMPInitializeSelect		= $0500;
	kRTPMPPreflightMediaSelect	= $0501;
	kRTPMPIdleSelect			= $0502;
	kRTPMPSetSampleDataSelect	= $0503;
	kRTPMPFlushSelect			= $0504;
	kRTPMPResetSelect			= $0505;
	kRTPMPSetInfoSelect			= $0506;
	kRTPMPGetInfoSelect			= $0507;
	kRTPMPSetTimeScaleSelect	= $0508;
	kRTPMPGetTimeScaleSelect	= $0509;
	kRTPMPSetTimeBaseSelect		= $050A;
	kRTPMPGetTimeBaseSelect		= $050B;
	kRTPMPHasCharacteristicSelect = $050C;
	kRTPMPSetPacketBuilderSelect = $050E;
	kRTPMPGetPacketBuilderSelect = $050F;
	kRTPMPSetMediaTypeSelect	= $0510;
	kRTPMPGetMediaTypeSelect	= $0511;
	kRTPMPSetMaxPacketSizeSelect = $0512;
	kRTPMPGetMaxPacketSizeSelect = $0513;
	kRTPMPSetMaxPacketDurationSelect = $0514;
	kRTPMPGetMaxPacketDurationSelect = $0515;					{  for export component and apps who want to }
																{  access dialogs for Media-specific settings }
																{  (such as Pure Voice interleave factor) }
	kRTPMPDoUserDialogSelect	= $0516;
	kRTPMPSetSettingsFromAtomContainerAtAtomSelect = $0517;
	kRTPMPGetSettingsIntoAtomContainerAtAtomSelect = $0518;
	kRTPMPGetSettingsAsTextSelect = $0519;
	kRTPMPGetSettingsSelect		= $051A;
	kRTPMPSetSettingsSelect		= $051B;

	{	-----------------------------------------
	    RTP Media Packetizer functions
	-----------------------------------------	}

	{
	 *  RTPMPInitialize()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
	 *    CarbonLib:        in CarbonLib 1.1 and later
	 *    Mac OS X:         in version 10.0 and later
	 *    Windows:          in QTSClient.lib 4.0 and later
	 	}
function RTPMPInitialize(rtpm: RTPMediaPacketizer; inFlags: SInt32): ComponentResult; external name '_RTPMPInitialize';
{ return noErr if you can handle this media }
{
 *  RTPMPPreflightMedia()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPMPPreflightMedia(rtpm: RTPMediaPacketizer; inMediaType: OSType; inSampleDescription: SampleDescriptionHandle): ComponentResult; external name '_RTPMPPreflightMedia';
{
   do work here if you need to - give up time periodically
   if you're doing time consuming operations
}
{
 *  RTPMPIdle()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPMPIdle(rtpm: RTPMediaPacketizer; inFlags: SInt32; var outFlags: SInt32): ComponentResult; external name '_RTPMPIdle';
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
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPMPSetSampleData(rtpm: RTPMediaPacketizer; const (*var*) inSampleData: RTPMPSampleDataParams; var outFlags: SInt32): ComponentResult; external name '_RTPMPSetSampleData';
{
   send everything you have buffered - you will get idles while
   you set the kRTPMPStillProcessingData flag here and in idle
}
{
 *  RTPMPFlush()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPMPFlush(rtpm: RTPMediaPacketizer; inFlags: SInt32; var outFlags: SInt32): ComponentResult; external name '_RTPMPFlush';
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
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPMPReset(rtpm: RTPMediaPacketizer; inFlags: SInt32): ComponentResult; external name '_RTPMPReset';
{-----------------------------------------
    RTP Media Packetizer get / set functions
-----------------------------------------}
{
 *  RTPMPSetInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPMPSetInfo(rtpm: RTPMediaPacketizer; inSelector: OSType; ioParams: UnivPtr): ComponentResult; external name '_RTPMPSetInfo';
{
 *  RTPMPGetInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPMPGetInfo(rtpm: RTPMediaPacketizer; inSelector: OSType; ioParams: UnivPtr): ComponentResult; external name '_RTPMPGetInfo';
{
 *  RTPMPSetTimeScale()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPMPSetTimeScale(rtpm: RTPMediaPacketizer; inTimeScale: TimeScale): ComponentResult; external name '_RTPMPSetTimeScale';
{
 *  RTPMPGetTimeScale()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPMPGetTimeScale(rtpm: RTPMediaPacketizer; var outTimeScale: TimeScale): ComponentResult; external name '_RTPMPGetTimeScale';
{
 *  RTPMPSetTimeBase()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPMPSetTimeBase(rtpm: RTPMediaPacketizer; inTimeBase: TimeBase): ComponentResult; external name '_RTPMPSetTimeBase';
{
 *  RTPMPGetTimeBase()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPMPGetTimeBase(rtpm: RTPMediaPacketizer; var outTimeBase: TimeBase): ComponentResult; external name '_RTPMPGetTimeBase';
{
 *  RTPMPHasCharacteristic()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPMPHasCharacteristic(rtpm: RTPMediaPacketizer; inSelector: OSType; var outHasIt: boolean): ComponentResult; external name '_RTPMPHasCharacteristic';
{
 *  RTPMPSetPacketBuilder()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPMPSetPacketBuilder(rtpm: RTPMediaPacketizer; inPacketBuilder: ComponentInstance): ComponentResult; external name '_RTPMPSetPacketBuilder';
{
 *  RTPMPGetPacketBuilder()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPMPGetPacketBuilder(rtpm: RTPMediaPacketizer; var outPacketBuilder: ComponentInstance): ComponentResult; external name '_RTPMPGetPacketBuilder';
{
 *  RTPMPSetMediaType()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPMPSetMediaType(rtpm: RTPMediaPacketizer; inMediaType: OSType): ComponentResult; external name '_RTPMPSetMediaType';
{
 *  RTPMPGetMediaType()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPMPGetMediaType(rtpm: RTPMediaPacketizer; var outMediaType: OSType): ComponentResult; external name '_RTPMPGetMediaType';
{  size is in bytes }
{
 *  RTPMPSetMaxPacketSize()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPMPSetMaxPacketSize(rtpm: RTPMediaPacketizer; inMaxPacketSize: UInt32): ComponentResult; external name '_RTPMPSetMaxPacketSize';
{
 *  RTPMPGetMaxPacketSize()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPMPGetMaxPacketSize(rtpm: RTPMediaPacketizer; var outMaxPacketSize: UInt32): ComponentResult; external name '_RTPMPGetMaxPacketSize';
{  duration is in milliseconds }
{
 *  RTPMPSetMaxPacketDuration()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPMPSetMaxPacketDuration(rtpm: RTPMediaPacketizer; inMaxPacketDuration: UInt32): ComponentResult; external name '_RTPMPSetMaxPacketDuration';
{
 *  RTPMPGetMaxPacketDuration()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPMPGetMaxPacketDuration(rtpm: RTPMediaPacketizer; var outMaxPacketDuration: UInt32): ComponentResult; external name '_RTPMPGetMaxPacketDuration';
{
 *  RTPMPDoUserDialog()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPMPDoUserDialog(rtpm: RTPMediaPacketizer; inFilterUPP: ModalFilterUPP; var canceled: boolean): ComponentResult; external name '_RTPMPDoUserDialog';
{
 *  RTPMPSetSettingsFromAtomContainerAtAtom()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPMPSetSettingsFromAtomContainerAtAtom(rtpm: RTPMediaPacketizer; inContainer: QTAtomContainer; inParentAtom: QTAtom): ComponentResult; external name '_RTPMPSetSettingsFromAtomContainerAtAtom';
{
 *  RTPMPGetSettingsIntoAtomContainerAtAtom()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPMPGetSettingsIntoAtomContainerAtAtom(rtpm: RTPMediaPacketizer; inOutContainer: QTAtomContainer; inParentAtom: QTAtom): ComponentResult; external name '_RTPMPGetSettingsIntoAtomContainerAtAtom';
{
 *  RTPMPGetSettingsAsText()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPMPGetSettingsAsText(rtpm: RTPMediaPacketizer; var text: Handle): ComponentResult; external name '_RTPMPGetSettingsAsText';
{
 *  RTPMPGetSettings()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 5.0 and later
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 5.0 and later
 }
function RTPMPGetSettings(rtpm: RTPMediaPacketizer; var outSettings: QTAtomContainer; inFlags: SInt32): ComponentResult; external name '_RTPMPGetSettings';
{
 *  RTPMPSetSettings()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 5.0 and later
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 5.0 and later
 }
function RTPMPSetSettings(rtpm: RTPMediaPacketizer; inSettings: QTAtomSpecPtr; inFlags: SInt32): ComponentResult; external name '_RTPMPSetSettings';
{============================================================================
        RTP Packet Builder
============================================================================}

const
	kRTPPacketBuilderType		= $72747062 (* 'rtpb' *);


type
	RTPPacketBuilder					= ComponentInstance;
	RTPPacketGroupRef    = ^SInt32; { an opaque 32-bit type }
	RTPPacketGroupRefPtr = ^RTPPacketGroupRef;  { when a var xx:RTPPacketGroupRef parameter can be nil, it is changed to xx: RTPPacketGroupRefPtr }
	RTPPacketRef    = ^SInt32; { an opaque 32-bit type }
	RTPPacketRefPtr = ^RTPPacketRef;  { when a var xx:RTPPacketRef parameter can be nil, it is changed to xx: RTPPacketRefPtr }
	RTPPacketRepeatedDataRef    = ^SInt32; { an opaque 32-bit type }
	RTPPacketRepeatedDataRefPtr = ^RTPPacketRepeatedDataRef;  { when a var xx:RTPPacketRepeatedDataRef parameter can be nil, it is changed to xx: RTPPacketRepeatedDataRefPtr }
	{  flags for RTPPBBegin/EndPacket, RTPPBBegin/EndPacketGroup }

const
	kRTPPBSetMarkerFlag			= $00000001;
	kRTPPBRepeatPacketFlag		= $00000002;
	kRTPPBSyncSampleFlag		= $00010000;
	kRTPPBBFrameFlag			= $00020000;
	kRTPPBDontSendFlag			= $10000000;					{  when set in EndPacketGroup, will not add group }

	kRTPPBUnknownPacketMediaDataLength = 0;

	{  flags for RTPPBGetSampleData }
	kRTPPBEndOfDataFlag			= $00000001;


type
{$ifc TYPED_FUNCTION_POINTERS}
	RTPPBCallbackProcPtr = procedure(inSelector: OSType; ioParams: UnivPtr; inRefCon: UnivPtr);
{$elsec}
	RTPPBCallbackProcPtr = ProcPtr;
{$endc}

{$ifc OPAQUE_UPP_TYPES}
	RTPPBCallbackUPP = ^SInt32; { an opaque UPP }
{$elsec}
	RTPPBCallbackUPP = UniversalProcPtr;
{$endc}	
	{	-----------------------------------------
	    RTP Packet Builder selectors
	-----------------------------------------	}

const
	kRTPPBBeginPacketGroupSelect = $0500;
	kRTPPBEndPacketGroupSelect	= $0501;
	kRTPPBBeginPacketSelect		= $0502;
	kRTPPBEndPacketSelect		= $0503;
	kRTPPBAddPacketLiteralDataSelect = $0504;
	kRTPPBAddPacketSampleDataSelect = $0505;
	kRTPPBAddPacketRepeatedDataSelect = $0506;
	kRTPPBReleaseRepeatedDataSelect = $0507;
	kRTPPBSetPacketSequenceNumberSelect = $0508;
	kRTPPBGetPacketSequenceNumberSelect = $0509;
	kRTPPBSetCallbackSelect		= $050A;
	kRTPPBGetCallbackSelect		= $050B;
	kRTPPBSetInfoSelect			= $050C;
	kRTPPBGetInfoSelect			= $050D;
	kRTPPBSetPacketTimeStampOffsetSelect = $050E;
	kRTPPBGetPacketTimeStampOffsetSelect = $050F;
	kRTPPBAddPacketSampleData64Select = $0510;
	kRTPPBGetSampleDataSelect	= $0511;
	kRTPPBAddRepeatPacketSelect	= $0512;

	{	-----------------------------------------
	    RTP Packet Builder functions
	-----------------------------------------	}
	{
	 *  RTPPBBeginPacketGroup()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
	 *    CarbonLib:        in CarbonLib 1.1 and later
	 *    Mac OS X:         in version 10.0 and later
	 *    Windows:          in QTSClient.lib 4.0 and later
	 	}
function RTPPBBeginPacketGroup(rtpb: RTPPacketBuilder; inFlags: SInt32; inTimeStamp: UInt32; var outPacketGroup: RTPPacketGroupRef): ComponentResult; external name '_RTPPBBeginPacketGroup';
{
 *  RTPPBEndPacketGroup()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPPBEndPacketGroup(rtpb: RTPPacketBuilder; inFlags: SInt32; inPacketGroup: RTPPacketGroupRef): ComponentResult; external name '_RTPPBEndPacketGroup';
{
 *  RTPPBBeginPacket()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPPBBeginPacket(rtpb: RTPPacketBuilder; inFlags: SInt32; inPacketGroup: RTPPacketGroupRef; inPacketMediaDataLength: UInt32; var outPacket: RTPPacketRef): ComponentResult; external name '_RTPPBBeginPacket';
{
 *  RTPPBEndPacket()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPPBEndPacket(rtpb: RTPPacketBuilder; inFlags: SInt32; inPacketGroup: RTPPacketGroupRef; inPacket: RTPPacketRef; inTransmissionTimeOffset: UInt32; inDuration: UInt32): ComponentResult; external name '_RTPPBEndPacket';
{
   non-NULL RTPPacketRepeatedDataRef means this data will be repeated later
   pb must return a repeated data ref
}
{
 *  RTPPBAddPacketLiteralData()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPPBAddPacketLiteralData(rtpb: RTPPacketBuilder; inFlags: SInt32; inPacketGroup: RTPPacketGroupRef; inPacket: RTPPacketRef; var inData: UInt8; inDataLength: UInt32; var outDataRef: RTPPacketRepeatedDataRef): ComponentResult; external name '_RTPPBAddPacketLiteralData';
{
   non-NULL RTPPacketRepeatedDataRef means this data will be repeated later
   pb must return a repeated data ref
}
{
 *  RTPPBAddPacketSampleData()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPPBAddPacketSampleData(rtpb: RTPPacketBuilder; inFlags: SInt32; inPacketGroup: RTPPacketGroupRef; inPacket: RTPPacketRef; var inSampleDataParams: RTPMPSampleDataParams; inSampleOffset: UInt32; inSampleDataLength: UInt32; var outDataRef: RTPPacketRepeatedDataRef): ComponentResult; external name '_RTPPBAddPacketSampleData';
{
   non-NULL RTPPacketRepeatedDataRef means this data will be repeated later
   pb must return a repeated data ref
}
{
 *  RTPPBAddPacketSampleData64()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 5.0 and later
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 5.0 and later
 }
function RTPPBAddPacketSampleData64(rtpb: RTPPacketBuilder; inFlags: SInt32; inPacketGroup: RTPPacketGroupRef; inPacket: RTPPacketRef; var inSampleDataParams: RTPMPSampleDataParams; (*const*) var inSampleOffset: UInt64; inSampleDataLength: UInt32; var outDataRef: RTPPacketRepeatedDataRef): ComponentResult; external name '_RTPPBAddPacketSampleData64';
{
   call to add the repeated data using the ref you got from
   RTPPBAddPacketLiteralData or RTPPBAddPacketSampleData
}
{
 *  RTPPBAddPacketRepeatedData()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPPBAddPacketRepeatedData(rtpb: RTPPacketBuilder; inFlags: SInt32; inPacketGroup: RTPPacketGroupRef; inPacket: RTPPacketRef; inDataRef: RTPPacketRepeatedDataRef): ComponentResult; external name '_RTPPBAddPacketRepeatedData';
{  call when done with repeated data }
{
 *  RTPPBReleaseRepeatedData()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPPBReleaseRepeatedData(rtpb: RTPPacketBuilder; inDataRef: RTPPacketRepeatedDataRef): ComponentResult; external name '_RTPPBReleaseRepeatedData';
{
   seq number is just relative seq number
   don't call if you don't care when seq # is used
}
{
 *  RTPPBSetPacketSequenceNumber()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPPBSetPacketSequenceNumber(rtpb: RTPPacketBuilder; inFlags: SInt32; inPacketGroup: RTPPacketGroupRef; inPacket: RTPPacketRef; inSequenceNumber: UInt32): ComponentResult; external name '_RTPPBSetPacketSequenceNumber';
{
 *  RTPPBGetPacketSequenceNumber()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPPBGetPacketSequenceNumber(rtpb: RTPPacketBuilder; inFlags: SInt32; inPacketGroup: RTPPacketGroupRef; inPacket: RTPPacketRef; var outSequenceNumber: UInt32): ComponentResult; external name '_RTPPBGetPacketSequenceNumber';
{
 *  RTPPBSetPacketTimeStampOffset()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 5.0 and later
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 5.0 and later
 }
function RTPPBSetPacketTimeStampOffset(rtpb: RTPPacketBuilder; inFlags: SInt32; inPacketGroup: RTPPacketGroupRef; inPacket: RTPPacketRef; inTimeStampOffset: SInt32): ComponentResult; external name '_RTPPBSetPacketTimeStampOffset';
{
 *  RTPPBGetPacketTimeStampOffset()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 5.0 and later
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 5.0 and later
 }
function RTPPBGetPacketTimeStampOffset(rtpb: RTPPacketBuilder; inFlags: SInt32; inPacketGroup: RTPPacketGroupRef; inPacket: RTPPacketRef; var outTimeStampOffset: SInt32): ComponentResult; external name '_RTPPBGetPacketTimeStampOffset';
{
 *  RTPPBAddRepeatPacket()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 5.0 and later
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 5.0 and later
 }
function RTPPBAddRepeatPacket(rtpb: RTPPacketBuilder; inFlags: SInt32; inPacketGroup: RTPPacketGroupRef; inPacket: RTPPacketRef; inTransmissionOffset: TimeValue; inSequenceNumber: UInt32): ComponentResult; external name '_RTPPBAddRepeatPacket';
{
   used for communicating with the caller of the media packetizers if needed
   NOT used for communicating with the media packetizers themselves
}
{
 *  RTPPBSetCallback()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPPBSetCallback(rtpb: RTPPacketBuilder; inCallback: RTPPBCallbackUPP; inRefCon: UnivPtr): ComponentResult; external name '_RTPPBSetCallback';
{
 *  RTPPBGetCallback()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPPBGetCallback(rtpb: RTPPacketBuilder; var outCallback: RTPPBCallbackUPP; var outRefCon: UnivPtr): ComponentResult; external name '_RTPPBGetCallback';
{
 *  RTPPBSetInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPPBSetInfo(rtpb: RTPPacketBuilder; inSelector: OSType; ioParams: UnivPtr): ComponentResult; external name '_RTPPBSetInfo';
{
 *  RTPPBGetInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function RTPPBGetInfo(rtpb: RTPPacketBuilder; inSelector: OSType; ioParams: UnivPtr): ComponentResult; external name '_RTPPBGetInfo';
{
 *  RTPPBGetSampleData()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 5.0 and later
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 5.0 and later
 }
function RTPPBGetSampleData(rtpb: RTPPacketBuilder; var inParams: RTPMPSampleDataParams; (*const*) var inStartOffset: UInt64; var outDataBuffer: UInt8; inBytesToRead: UInt32; var outBytesRead: UInt32; var outFlags: SInt32): ComponentResult; external name '_RTPPBGetSampleData';
{ UPP call backs }

const
	uppRTPMPDataReleaseProcInfo = $000003C0;
	uppRTPPBCallbackProcInfo = $00000FC0;
	{
	 *  NewRTPMPDataReleaseUPP()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   available as macro/inline
	 *    CarbonLib:        in CarbonLib 1.3 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
function NewRTPMPDataReleaseUPP(userRoutine: RTPMPDataReleaseProcPtr): RTPMPDataReleaseUPP; external name '_NewRTPMPDataReleaseUPP'; { old name was NewRTPMPDataReleaseProc }
{
 *  NewRTPPBCallbackUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewRTPPBCallbackUPP(userRoutine: RTPPBCallbackProcPtr): RTPPBCallbackUPP; external name '_NewRTPPBCallbackUPP'; { old name was NewRTPPBCallbackProc }
{
 *  DisposeRTPMPDataReleaseUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeRTPMPDataReleaseUPP(userUPP: RTPMPDataReleaseUPP); external name '_DisposeRTPMPDataReleaseUPP';
{
 *  DisposeRTPPBCallbackUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeRTPPBCallbackUPP(userUPP: RTPPBCallbackUPP); external name '_DisposeRTPPBCallbackUPP';
{
 *  InvokeRTPMPDataReleaseUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure InvokeRTPMPDataReleaseUPP(var inData: UInt8; inRefCon: UnivPtr; userRoutine: RTPMPDataReleaseUPP); external name '_InvokeRTPMPDataReleaseUPP'; { old name was CallRTPMPDataReleaseProc }
{
 *  InvokeRTPPBCallbackUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure InvokeRTPPBCallbackUPP(inSelector: OSType; ioParams: UnivPtr; inRefCon: UnivPtr; userRoutine: RTPPBCallbackUPP); external name '_InvokeRTPPBCallbackUPP'; { old name was CallRTPPBCallbackProc }
{$ALIGN MAC68K}


end.
