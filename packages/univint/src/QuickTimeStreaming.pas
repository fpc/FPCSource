{
     File:       QuickTimeStreaming.p
 
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

unit QuickTimeStreaming;
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
uses MacTypes,Files,Events,ImageCompression,Quickdraw,Components,MacErrors,Movies,QuickTimeComponents;


{$ALIGN MAC68K}


const
	kQTSInfiniteDuration		= $7FFFFFFF;
	kQTSUnknownDuration			= $00000000;
	kQTSNormalForwardRate		= $00010000;
	kQTSStoppedRate				= $00000000;


type
	QTSPresentationRecordPtr = ^QTSPresentationRecord;
	QTSPresentationRecord = record
		data:					array [0..0] of SInt32;
	end;

	QTSPresentation						= ^QTSPresentationRecord;
	QTSStreamRecordPtr = ^QTSStreamRecord;
	QTSStreamRecord = record
		data:					array [0..0] of SInt32;
	end;

	QTSStream							= ^QTSStreamRecord;
	QTSEditEntryPtr = ^QTSEditEntry;
	QTSEditEntry = record
		presentationDuration:	TimeValue64;
		streamStartTime:		TimeValue64;
		streamRate:				Fixed;
	end;

	QTSEditListPtr = ^QTSEditList;
	QTSEditList = record
		numEdits:				SInt32;
		edits:					array [0..0] of QTSEditEntry;
	end;

	QTSEditListHandle					= ^QTSEditListPtr;
{$ifc TYPED_FUNCTION_POINTERS}
	QTSNotificationProcPtr = function(inErr: ComponentResult; inNotificationType: OSType; inNotificationParams: UnivPtr; inRefCon: UnivPtr): ComponentResult;
{$elsec}
	QTSNotificationProcPtr = ProcPtr;
{$endc}

{$ifc OPAQUE_UPP_TYPES}
	QTSNotificationUPP = ^SInt32; { an opaque UPP }
{$elsec}
	QTSNotificationUPP = UniversalProcPtr;
{$endc}	
	{	-----------------------------------------
	    Get / Set Info
	-----------------------------------------	}

const
	kQTSGetURLLink				= $67756C6C (* 'gull' *);						{  QTSGetURLLinkRecord*  }

	{	 get and set 	}
	kQTSTargetBufferDurationInfo = $62756672 (* 'bufr' *);						{  Fixed* in seconds; expected, not actual  }
	kQTSDurationInfo			= $64757261 (* 'dura' *);						{  QTSDurationAtom*  }
	kQTSSoundLevelMeteringEnabledInfo = $6D74726E (* 'mtrn' *);					{  Boolean*  }
	kQTSSoundLevelMeterInfo		= $6C65766D (* 'levm' *);						{  LevelMeterInfoPtr  }
	kQTSSourceTrackIDInfo		= $6F746964 (* 'otid' *);						{  UInt32*  }
	kQTSSourceLayerInfo			= $6F6C7972 (* 'olyr' *);						{  UInt16*  }
	kQTSSourceLanguageInfo		= $6F6C6E67 (* 'olng' *);						{  UInt16*  }
	kQTSSourceTrackFlagsInfo	= $6F74666C (* 'otfl' *);						{  SInt32*  }
	kQTSSourceDimensionsInfo	= $6F64696D (* 'odim' *);						{  QTSDimensionParams*  }
	kQTSSourceVolumesInfo		= $6F766F6C (* 'ovol' *);						{  QTSVolumesParams*  }
	kQTSSourceMatrixInfo		= $6F6D6174 (* 'omat' *);						{  MatrixRecord*  }
	kQTSSourceClipRectInfo		= $6F636C70 (* 'oclp' *);						{  Rect*  }
	kQTSSourceGraphicsModeInfo	= $6F67726D (* 'ogrm' *);						{  QTSGraphicsModeParams*  }
	kQTSSourceScaleInfo			= $6F73636C (* 'oscl' *);						{  Point*  }
	kQTSSourceBoundingRectInfo	= $6F726374 (* 'orct' *);						{  Rect*  }
	kQTSSourceUserDataInfo		= $6F756474 (* 'oudt' *);						{  UserData  }
	kQTSSourceInputMapInfo		= $6F696D70 (* 'oimp' *);						{  QTAtomContainer  }
	kQTSInfo_DataProc			= $64617470 (* 'datp' *);						{  QTSDataProcParams*  }
	kQTSInfo_SendDataExtras		= $64657874 (* 'dext' *);						{  QTSSendDataExtrasParams*  }
	kQTSInfo_HintTrackID		= $68746964 (* 'htid' *);						{  long*  }
	kQTSInfo_URL				= $75726C20 (* 'url ' *);						{  Handle*, cstring in handle  }
	kQTSInfo_Authentication		= $61757570 (* 'auup' *);						{  QTSAuthenticationParams  }
	kQTSInfo_MediaPacketizer	= $726D706B (* 'rmpk' *);						{  ComponentInstance  }

	{	 get only 	}
	kQTSStatisticsInfo			= $73746174 (* 'stat' *);						{  QTSStatisticsParams*  }
	kQTSMinStatusDimensionsInfo	= $6D737464 (* 'mstd' *);						{  QTSDimensionParams*  }
	kQTSNormalStatusDimensionsInfo = $6E737464 (* 'nstd' *);					{  QTSDimensionParams*  }
	kQTSTotalDataRateInfo		= $64727474 (* 'drtt' *);						{  UInt32*, add to what's there  }
	kQTSTotalDataRateInInfo		= $64727469 (* 'drti' *);						{  UInt32*, add to what's there  }
	kQTSTotalDataRateOutInfo	= $6472746F (* 'drto' *);						{  UInt32*, add to what's there  }
	kQTSLostPercentInfo			= $6C706374 (* 'lpct' *);						{  QTSLostPercentParams*, add to what's there  }
	kQTSNumViewersInfo			= $6E766977 (* 'nviw' *);						{  UInt32*  }
	kQTSMediaTypeInfo			= $6D747970 (* 'mtyp' *);						{  OSType*  }
	kQTSNameInfo				= $6E616D65 (* 'name' *);						{  QTSNameParams*  }
	kQTSCanHandleSendDataType	= $63687364 (* 'chsd' *);						{  QTSCanHandleSendDataTypeParams*  }
	kQTSAnnotationsInfo			= $6D657461 (* 'meta' *);						{  QTAtomContainer  }
	kQTSRemainingBufferTimeInfo	= $62746D73 (* 'btms' *);						{  UInt32* remaining buffer time before playback, in microseconds  }
	kQTSInfo_SettingsText		= $73747478 (* 'sttx' *);						{  QTSSettingsTextParams*  }
	kQTSInfo_AverageFrameRate	= $66707320 (* 'fps ' *);						{  UnsignedFixed*  }


type
	QTSAuthenticationParamsPtr = ^QTSAuthenticationParams;
	QTSAuthenticationParams = record
		flags:					SInt32;
		userID:					ConstCStringPtr;						{  caller disposes of pointer }
		password:				ConstCStringPtr;						{  caller disposes of pointer }
	end;


const
	kQTSTargetBufferDurationTimeScale = 1000;


type
	QTSPanelFilterParamsPtr = ^QTSPanelFilterParams;
	QTSPanelFilterParams = record
		version:				SInt32;
		inStream:				QTSStream;
		inPanelType:			OSType;
		inPanelSubType:			OSType;
		details:				QTAtomSpec;
	end;

	{  return true to keep this panel }
{$ifc TYPED_FUNCTION_POINTERS}
	QTSPanelFilterProcPtr = function(var inParams: QTSPanelFilterParams; inRefCon: UnivPtr): boolean;
{$elsec}
	QTSPanelFilterProcPtr = ProcPtr;
{$endc}

{$ifc OPAQUE_UPP_TYPES}
	QTSPanelFilterUPP = ^SInt32; { an opaque UPP }
{$elsec}
	QTSPanelFilterUPP = UniversalProcPtr;
{$endc}	

const
	kQTSSettingsTextSummary		= $73657431 (* 'set1' *);
	kQTSSettingsTextDetails		= $73657464 (* 'setd' *);


type
	QTSSettingsTextParamsPtr = ^QTSSettingsTextParams;
	QTSSettingsTextParams = record
		flags:					SInt32;									{  None yet defined }
		inSettingsSelector:		OSType;									{  which kind of setting you want from enum above }
		outSettingsAsText:		Handle;									{  QTS allocates; Caller disposes }
		inPanelFilterProc:		QTSPanelFilterUPP;						{  To get a subset filter with this    }
		inPanelFilterProcRefCon: Ptr;
	end;

	QTSCanHandleSendDataTypeParamsPtr = ^QTSCanHandleSendDataTypeParams;
	QTSCanHandleSendDataTypeParams = record
		modifierTypeOrInputID:	SInt32;
		isModifierType:			boolean;
		returnedCanHandleSendDataType: boolean;							{  callee sets to true if it can handle it }
	end;

	QTSNameParamsPtr = ^QTSNameParams;
	QTSNameParams = record
		maxNameLength:			SInt32;
		requestedLanguage:		SInt32;
		returnedActualLanguage:	SInt32;
		returnedName:			Ptr;									{  pascal string; caller supplies }
	end;

	QTSLostPercentParamsPtr = ^QTSLostPercentParams;
	QTSLostPercentParams = record
		receivedPkts:			UInt32;
		lostPkts:				UInt32;
		percent:				Fixed;
	end;

	QTSDimensionParamsPtr = ^QTSDimensionParams;
	QTSDimensionParams = record
		width:					Fixed;
		height:					Fixed;
	end;

	QTSVolumesParamsPtr = ^QTSVolumesParams;
	QTSVolumesParams = record
		leftVolume:				SInt16;
		rightVolume:			SInt16;
	end;

	QTSGraphicsModeParamsPtr = ^QTSGraphicsModeParams;
	QTSGraphicsModeParams = record
		graphicsMode:			SInt16;
		opColor:				RGBColor;
	end;

	QTSGetURLLinkRecordPtr = ^QTSGetURLLinkRecord;
	QTSGetURLLinkRecord = record
		displayWhere:			Point;
		returnedURLLink:		Handle;
	end;


const
	kQTSDataProcParamsVersion1	= 1;

	kQTSDataProcType_MediaSample = $6D646961 (* 'mdia' *);
	kQTSDataProcType_HintSample	= $68696E74 (* 'hint' *);


type
	QTSDataProcParamsPtr = ^QTSDataProcParams;
	QTSDataProcParams = record
		version:				SInt32;
		flags:					SInt32;
		stream:					QTSStream;
		procType:				OSType;
		proc:					QTSNotificationUPP;
		procRefCon:				Ptr;
	end;


const
	kQTSDataProcSelector_SampleData = $73616D70 (* 'samp' *);
	kQTSDataProcSelector_UserData = $75736572 (* 'user' *);

	kQTSSampleDataCallbackParamsVersion1 = 1;


type
	QTSSampleDataCallbackParamsPtr = ^QTSSampleDataCallbackParams;
	QTSSampleDataCallbackParams = record
		version:				SInt32;
		flags:					SInt32;
		stream:					QTSStream;
		procType:				OSType;
		mediaType:				OSType;
		mediaTimeScale:			TimeScale;
		sampleDesc:				SampleDescriptionHandle;
		sampleDescSeed:			UInt32;
		sampleTime:				TimeValue64;
		duration:				TimeValue64;							{  could be 0  }
		sampleFlags:			SInt32;
		dataLength:				UInt32;
		data:					Ptr;
	end;


const
	kQTSUserDataCallbackParamsVersion1 = 1;


type
	QTSUserDataCallbackParamsPtr = ^QTSUserDataCallbackParams;
	QTSUserDataCallbackParams = record
		version:				SInt32;
		flags:					SInt32;
		stream:					QTSStream;
		procType:				OSType;
		userDataType:			OSType;
		userDataHandle:			Handle;									{  caller must make copy if it wants to keep the data around }
	end;


const
	kQTSSendDataExtrasParamsVersion1 = 1;


type
	QTSSendDataExtrasParamsPtr = ^QTSSendDataExtrasParams;
	QTSSendDataExtrasParams = record
		version:				SInt32;
		flags:					SInt32;
		procType:				OSType;
	end;

{$ifc TYPED_FUNCTION_POINTERS}
	QTSModalFilterProcPtr = function(inDialog: DialogPtr; const (*var*) inEvent: EventRecord; var ioItemHit: SInt16; inRefCon: UnivPtr): boolean;
{$elsec}
	QTSModalFilterProcPtr = ProcPtr;
{$endc}

{$ifc OPAQUE_UPP_TYPES}
	QTSModalFilterUPP = ^SInt32; { an opaque UPP }
{$elsec}
	QTSModalFilterUPP = UniversalProcPtr;
{$endc}	
	{	-----------------------------------------
	    Characteristics
	-----------------------------------------	}
	{	 characteristics in Movies.h work here too 	}

const
	kQTSSupportsPerStreamControlCharacteristic = $70736374 (* 'psct' *);


type
	QTSVideoParamsPtr = ^QTSVideoParams;
	QTSVideoParams = record
		width:					Fixed;
		height:					Fixed;
		matrix:					MatrixRecord;
		gWorld:					CGrafPtr;
		gdHandle:				GDHandle_fix;
		clip:					RgnHandle;
		graphicsMode:			SInt16;
		opColor:				RGBColor;
	end;

	QTSAudioParamsPtr = ^QTSAudioParams;
	QTSAudioParams = record
		leftVolume:				SInt16;
		rightVolume:			SInt16;
		bassLevel:				SInt16;
		trebleLevel:			SInt16;
		frequencyBandsCount:	SInt16;
		frequencyBands:			Ptr;
		levelMeteringEnabled:	boolean;
	end;

	QTSMediaParamsPtr = ^QTSMediaParams;
	QTSMediaParams = record
		v:						QTSVideoParams;
		a:						QTSAudioParams;
	end;


const
	kQTSMustDraw				= $08;
	kQTSAtEnd					= $10;
	kQTSPreflightDraw			= $20;
	kQTSSyncDrawing				= $40;

	{	 media task result flags 	}
	kQTSDidDraw					= $01;
	kQTSNeedsToDraw				= $04;
	kQTSDrawAgain				= $08;
	kQTSPartialDraw				= $10;

	{	============================================================================
	        Notifications
	============================================================================	}
	{	 ------ notification types ------ 	}
	kQTSNullNotification		= $6E756C6C (* 'null' *);						{  NULL  }
	kQTSErrorNotification		= $65727220 (* 'err ' *);						{  QTSErrorParams*, optional  }
	kQTSNewPresDetectedNotification = $6E657770 (* 'newp' *);					{  QTSNewPresDetectedParams*  }
	kQTSPresBeginChangingNotification = $70726362 (* 'prcb' *);					{  NULL  }
	kQTSPresDoneChangingNotification = $70726364 (* 'prcd' *);					{  NULL  }
	kQTSPresentationChangedNotification = $70726368 (* 'prch' *);				{  NULL  }
	kQTSNewStreamNotification	= $73746E77 (* 'stnw' *);						{  QTSNewStreamParams*  }
	kQTSStreamBeginChangingNotification = $73746362 (* 'stcb' *);				{  QTSStream  }
	kQTSStreamDoneChangingNotification = $73746364 (* 'stcd' *);				{  QTSStream  }
	kQTSStreamChangedNotification = $73746368 (* 'stch' *);						{  QTSStreamChangedParams*  }
	kQTSStreamGoneNotification	= $7374676E (* 'stgn' *);						{  QTSStreamGoneParams*  }
	kQTSPreviewAckNotification	= $7076616B (* 'pvak' *);						{  QTSStream  }
	kQTSPrerollAckNotification	= $7061636B (* 'pack' *);						{  QTSStream  }
	kQTSStartAckNotification	= $7361636B (* 'sack' *);						{  QTSStream  }
	kQTSStopAckNotification		= $7861636B (* 'xack' *);						{  QTSStream  }
	kQTSStatusNotification		= $73746174 (* 'stat' *);						{  QTSStatusParams*  }
	kQTSURLNotification			= $75726C20 (* 'url ' *);						{  QTSURLParams*  }
	kQTSDurationNotification	= $64757261 (* 'dura' *);						{  QTSDurationAtom*  }
	kQTSNewPresentationNotification = $6E707273 (* 'nprs' *);					{  QTSPresentation  }
	kQTSPresentationGoneNotification = $78707273 (* 'xprs' *);					{  QTSPresentation  }
	kQTSPresentationDoneNotification = $70646F6E (* 'pdon' *);					{  NULL  }
	kQTSBandwidthAlertNotification = $6277616C (* 'bwal' *);					{  QTSBandwidthAlertParams*  }
	kQTSAnnotationsChangedNotification = $6D657461 (* 'meta' *);				{  NULL  }


	{	 flags for QTSErrorParams 	}
	kQTSFatalErrorFlag			= $00000001;


type
	QTSErrorParamsPtr = ^QTSErrorParams;
	QTSErrorParams = record
		errorString:			ConstCStringPtr;
		flags:					SInt32;
	end;

	QTSNewPresDetectedParamsPtr = ^QTSNewPresDetectedParams;
	QTSNewPresDetectedParams = record
		data:					Ptr;
	end;

	QTSNewStreamParamsPtr = ^QTSNewStreamParams;
	QTSNewStreamParams = record
		stream:					QTSStream;
	end;

	QTSStreamChangedParamsPtr = ^QTSStreamChangedParams;
	QTSStreamChangedParams = record
		stream:					QTSStream;
		mediaComponent:			ComponentInstance;						{  could be NULL  }
	end;

	QTSStreamGoneParamsPtr = ^QTSStreamGoneParams;
	QTSStreamGoneParams = record
		stream:					QTSStream;
	end;

	QTSStatusParamsPtr = ^QTSStatusParams;
	QTSStatusParams = record
		status:					UInt32;
		statusString:			ConstCStringPtr;
		detailedStatus:			UInt32;
		detailedStatusString:	ConstCStringPtr;
	end;

	QTSInfoParamsPtr = ^QTSInfoParams;
	QTSInfoParams = record
		infoType:				OSType;
		infoParams:				Ptr;
	end;

	QTSURLParamsPtr = ^QTSURLParams;
	QTSURLParams = record
		urlLength:				UInt32;
		url:					ConstCStringPtr;
	end;


const
	kQTSBandwidthAlertNeedToStop = $01;
	kQTSBandwidthAlertRestartAt	= $02;


type
	QTSBandwidthAlertParamsPtr = ^QTSBandwidthAlertParams;
	QTSBandwidthAlertParams = record
		flags:					SInt32;
		restartAt:				TimeValue;								{  new field in QT 4.1 }
		reserved:				Ptr;
	end;

	{	============================================================================
	        Presentation
	============================================================================	}
	{	-----------------------------------------
	     Flags
	-----------------------------------------	}
	{	 flags for NewPresentationFromData 	}

const
	kQTSAutoModeFlag			= $00000001;
	kQTSDontShowStatusFlag		= $00000008;
	kQTSSendMediaFlag			= $00010000;
	kQTSReceiveMediaFlag		= $00020000;


type
	QTSNewPresentationParamsPtr = ^QTSNewPresentationParams;
	QTSNewPresentationParams = record
		dataType:				OSType;
		data:					Ptr;
		dataLength:				UInt32;
		editList:				QTSEditListHandle;
		flags:					SInt32;
		timeScale:				TimeScale_fix;	{  set to 0 for default timescale  }
		mediaParams:			QTSMediaParamsPtr;
		notificationProc:		QTSNotificationUPP;
		notificationRefCon:		Ptr;
	end;

	QTSPresParamsPtr = ^QTSPresParams;
	QTSPresParams = record
		version:				UInt32;
		editList:				QTSEditListHandle;
		flags:					SInt32;
		timeScale:				TimeScale_fix;	{  set to 0 for default timescale  }
		mediaParams:			QTSMediaParamsPtr;
		notificationProc:		QTSNotificationUPP;
		notificationRefCon:		Ptr;
	end;


const
	kQTSPresParamsVersion1		= 1;


type
	QTSPresIdleParamsPtr = ^QTSPresIdleParams;
	QTSPresIdleParams = record
		stream:					QTSStream;
		movieTimeToDisplay:		TimeValue64;
		flagsIn:				SInt32;
		flagsOut:				SInt32;
	end;


const
	kQTSExportFlag_ShowDialog	= $00000001;

	kQTSExportParamsVersion1	= 1;


type
	QTSExportParamsPtr = ^QTSExportParams;
	QTSExportParams = record
		version:				SInt32;
		exportType:				OSType;
		exportExtraData:		Ptr;
		destinationContainerType: OSType;
		destinationContainerData: Ptr;
		destinationContainerExtras: Ptr;
		flagsIn:				SInt32;
		flagsOut:				SInt32;
		filterProc:				QTSModalFilterUPP;
		filterProcRefCon:		Ptr;
		exportComponent:		Component;								{  NULL unless you want to override  }
	end;

	{	-----------------------------------------
	    Toolbox Init/Close
	-----------------------------------------	}
	{	 all "apps" must call this 	}
	{
	 *  InitializeQTS()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
	 *    CarbonLib:        in CarbonLib 1.1 and later
	 *    Mac OS X:         in version 10.0 and later
	 *    Windows:          in QTSClient.lib 4.0 and later
	 	}
function InitializeQTS: OSErr; external name '_InitializeQTS';

{
 *  TerminateQTS()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function TerminateQTS: OSErr; external name '_TerminateQTS';

{-----------------------------------------
    Presentation Functions
-----------------------------------------}
{
 *  QTSNewPresentation()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function QTSNewPresentation(const (*var*) inParams: QTSNewPresentationParams; var outPresentation: QTSPresentation): OSErr; external name '_QTSNewPresentation';

{
 *  QTSNewPresentationFromData()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 5.0 and later
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 5.0 and later
 }
function QTSNewPresentationFromData(inDataType: OSType; inData: UnivPtr; (*const*) var inDataLength: SInt64; const (*var*) inPresParams: QTSPresParams; var outPresentation: QTSPresentation): OSErr; external name '_QTSNewPresentationFromData';

{
 *  QTSNewPresentationFromFile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 5.0 and later
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 5.0 and later
 }
function QTSNewPresentationFromFile(const (*var*) inFileSpec: FSSpec; const (*var*) inPresParams: QTSPresParams; var outPresentation: QTSPresentation): OSErr; external name '_QTSNewPresentationFromFile';

{
 *  QTSNewPresentationFromDataRef()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 5.0 and later
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 5.0 and later
 }
function QTSNewPresentationFromDataRef(inDataRef: Handle; inDataRefType: OSType; const (*var*) inPresParams: QTSPresParams; var outPresentation: QTSPresentation): OSErr; external name '_QTSNewPresentationFromDataRef';

{
 *  QTSDisposePresentation()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function QTSDisposePresentation(inPresentation: QTSPresentation; inFlags: SInt32): OSErr; external name '_QTSDisposePresentation';

{
 *  QTSPresExport()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 5.0 and later
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 5.0 and later
 }
function QTSPresExport(inPresentation: QTSPresentation; inStream: QTSStream; var inExportParams: QTSExportParams): OSErr; external name '_QTSPresExport';

{
 *  QTSPresIdle()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
procedure QTSPresIdle(inPresentation: QTSPresentation; var ioParams: QTSPresIdleParams); external name '_QTSPresIdle';

{
 *  QTSPresInvalidateRegion()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function QTSPresInvalidateRegion(inPresentation: QTSPresentation; inRegion: RgnHandle): OSErr; external name '_QTSPresInvalidateRegion';

{-----------------------------------------
    Presentation Configuration
-----------------------------------------}
{
 *  QTSPresSetFlags()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function QTSPresSetFlags(inPresentation: QTSPresentation; inFlags: SInt32; inFlagsMask: SInt32): OSErr; external name '_QTSPresSetFlags';

{
 *  QTSPresGetFlags()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function QTSPresGetFlags(inPresentation: QTSPresentation; var outFlags: SInt32): OSErr; external name '_QTSPresGetFlags';

{
 *  QTSPresGetTimeBase()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function QTSPresGetTimeBase(inPresentation: QTSPresentation; var outTimeBase: TimeBase): OSErr; external name '_QTSPresGetTimeBase';

{
 *  QTSPresGetTimeScale()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function QTSPresGetTimeScale(inPresentation: QTSPresentation; var outTimeScale: TimeScale): OSErr; external name '_QTSPresGetTimeScale';

{
 *  QTSPresSetInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function QTSPresSetInfo(inPresentation: QTSPresentation; inStream: QTSStream; inSelector: OSType; ioParam: UnivPtr): OSErr; external name '_QTSPresSetInfo';

{
 *  QTSPresGetInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function QTSPresGetInfo(inPresentation: QTSPresentation; inStream: QTSStream; inSelector: OSType; ioParam: UnivPtr): OSErr; external name '_QTSPresGetInfo';

{
 *  QTSPresHasCharacteristic()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function QTSPresHasCharacteristic(inPresentation: QTSPresentation; inStream: QTSStream; inCharacteristic: OSType; var outHasIt: boolean): OSErr; external name '_QTSPresHasCharacteristic';

{
 *  QTSPresSetNotificationProc()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function QTSPresSetNotificationProc(inPresentation: QTSPresentation; inNotificationProc: QTSNotificationUPP; inRefCon: UnivPtr): OSErr; external name '_QTSPresSetNotificationProc';

{
 *  QTSPresGetNotificationProc()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function QTSPresGetNotificationProc(inPresentation: QTSPresentation; var outNotificationProc: QTSNotificationUPP; var outRefCon: UnivPtr): OSErr; external name '_QTSPresGetNotificationProc';

{-----------------------------------------
    Presentation Control
-----------------------------------------}
{
 *  QTSPresPreview()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 5.0 and later
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 }
function QTSPresPreview(inPresentation: QTSPresentation; inStream: QTSStream; (*const*) var inTimeValue: TimeValue64; inRate: Fixed; inFlags: SInt32): OSErr; external name '_QTSPresPreview';

{
 *  QTSPresPreroll()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function QTSPresPreroll(inPresentation: QTSPresentation; inStream: QTSStream; inTimeValue: UInt32; inRate: Fixed; inFlags: SInt32): OSErr; external name '_QTSPresPreroll';

{
 *  QTSPresPreroll64()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.1 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.1 and later
 }
function QTSPresPreroll64(inPresentation: QTSPresentation; inStream: QTSStream; (*const*) var inPrerollTime: TimeValue64; inRate: Fixed; inFlags: SInt32): OSErr; external name '_QTSPresPreroll64';

{
 *  QTSPresStart()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function QTSPresStart(inPresentation: QTSPresentation; inStream: QTSStream; inFlags: SInt32): OSErr; external name '_QTSPresStart';

{
 *  QTSPresSkipTo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function QTSPresSkipTo(inPresentation: QTSPresentation; inTimeValue: UInt32): OSErr; external name '_QTSPresSkipTo';

{
 *  QTSPresSkipTo64()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.1 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.1 and later
 }
function QTSPresSkipTo64(inPresentation: QTSPresentation; (*const*) var inTimeValue: TimeValue64): OSErr; external name '_QTSPresSkipTo64';

{
 *  QTSPresStop()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function QTSPresStop(inPresentation: QTSPresentation; inStream: QTSStream; inFlags: SInt32): OSErr; external name '_QTSPresStop';

{============================================================================
        Streams
============================================================================}
{-----------------------------------------
    Stream Functions
-----------------------------------------}
{
 *  QTSPresNewStream()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function QTSPresNewStream(inPresentation: QTSPresentation; inDataType: OSType; inData: UnivPtr; inDataLength: UInt32; inFlags: SInt32; var outStream: QTSStream): OSErr; external name '_QTSPresNewStream';

{
 *  QTSDisposeStream()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function QTSDisposeStream(inStream: QTSStream; inFlags: SInt32): OSErr; external name '_QTSDisposeStream';

{
 *  QTSPresGetNumStreams()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function QTSPresGetNumStreams(inPresentation: QTSPresentation): UInt32; external name '_QTSPresGetNumStreams';

{
 *  QTSPresGetIndStream()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function QTSPresGetIndStream(inPresentation: QTSPresentation; inIndex: UInt32): QTSStream; external name '_QTSPresGetIndStream';

{
 *  QTSGetStreamPresentation()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function QTSGetStreamPresentation(inStream: QTSStream): QTSPresentation; external name '_QTSGetStreamPresentation';

{
 *  QTSPresSetPreferredRate()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function QTSPresSetPreferredRate(inPresentation: QTSPresentation; inRate: Fixed; inFlags: SInt32): OSErr; external name '_QTSPresSetPreferredRate';

{
 *  QTSPresGetPreferredRate()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function QTSPresGetPreferredRate(inPresentation: QTSPresentation; var outRate: Fixed): OSErr; external name '_QTSPresGetPreferredRate';

{
 *  QTSPresSetEnable()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function QTSPresSetEnable(inPresentation: QTSPresentation; inStream: QTSStream; inEnableMode: boolean): OSErr; external name '_QTSPresSetEnable';

{
 *  QTSPresGetEnable()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function QTSPresGetEnable(inPresentation: QTSPresentation; inStream: QTSStream; var outEnableMode: boolean): OSErr; external name '_QTSPresGetEnable';

{
 *  QTSPresSetPresenting()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function QTSPresSetPresenting(inPresentation: QTSPresentation; inStream: QTSStream; inPresentingMode: boolean): OSErr; external name '_QTSPresSetPresenting';

{
 *  QTSPresGetPresenting()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function QTSPresGetPresenting(inPresentation: QTSPresentation; inStream: QTSStream; var outPresentingMode: boolean): OSErr; external name '_QTSPresGetPresenting';

{
 *  QTSPresSetActiveSegment()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.1 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.1 and later
 }
function QTSPresSetActiveSegment(inPresentation: QTSPresentation; inStream: QTSStream; (*const*) var inStartTime: TimeValue64; (*const*) var inDuration: TimeValue64): OSErr; external name '_QTSPresSetActiveSegment';

{
 *  QTSPresGetActiveSegment()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.1 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.1 and later
 }
function QTSPresGetActiveSegment(inPresentation: QTSPresentation; inStream: QTSStream; var outStartTime: TimeValue64; var outDuration: TimeValue64): OSErr; external name '_QTSPresGetActiveSegment';

{
 *  QTSPresSetPlayHints()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function QTSPresSetPlayHints(inPresentation: QTSPresentation; inStream: QTSStream; inFlags: SInt32; inFlagsMask: SInt32): OSErr; external name '_QTSPresSetPlayHints';

{
 *  QTSPresGetPlayHints()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function QTSPresGetPlayHints(inPresentation: QTSPresentation; inStream: QTSStream; var outFlags: SInt32): OSErr; external name '_QTSPresGetPlayHints';

{-----------------------------------------
    Stream Spatial Functions
-----------------------------------------}
{
 *  QTSPresSetGWorld()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function QTSPresSetGWorld(inPresentation: QTSPresentation; inStream: QTSStream; inGWorld: CGrafPtr; inGDHandle: GDHandle): OSErr; external name '_QTSPresSetGWorld';

{
 *  QTSPresGetGWorld()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function QTSPresGetGWorld(inPresentation: QTSPresentation; inStream: QTSStream; var outGWorld: CGrafPtr; var outGDHandle: GDHandle): OSErr; external name '_QTSPresGetGWorld';

{
 *  QTSPresSetClip()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function QTSPresSetClip(inPresentation: QTSPresentation; inStream: QTSStream; inClip: RgnHandle): OSErr; external name '_QTSPresSetClip';

{
 *  QTSPresGetClip()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function QTSPresGetClip(inPresentation: QTSPresentation; inStream: QTSStream; var outClip: RgnHandle): OSErr; external name '_QTSPresGetClip';

{
 *  QTSPresSetMatrix()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function QTSPresSetMatrix(inPresentation: QTSPresentation; inStream: QTSStream; const (*var*) inMatrix: MatrixRecord): OSErr; external name '_QTSPresSetMatrix';

{
 *  QTSPresGetMatrix()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function QTSPresGetMatrix(inPresentation: QTSPresentation; inStream: QTSStream; var outMatrix: MatrixRecord): OSErr; external name '_QTSPresGetMatrix';

{
 *  QTSPresSetDimensions()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function QTSPresSetDimensions(inPresentation: QTSPresentation; inStream: QTSStream; inWidth: Fixed; inHeight: Fixed): OSErr; external name '_QTSPresSetDimensions';

{
 *  QTSPresGetDimensions()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function QTSPresGetDimensions(inPresentation: QTSPresentation; inStream: QTSStream; var outWidth: Fixed; var outHeight: Fixed): OSErr; external name '_QTSPresGetDimensions';

{
 *  QTSPresSetGraphicsMode()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function QTSPresSetGraphicsMode(inPresentation: QTSPresentation; inStream: QTSStream; inMode: SInt16; const (*var*) inOpColor: RGBColor): OSErr; external name '_QTSPresSetGraphicsMode';

{
 *  QTSPresGetGraphicsMode()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function QTSPresGetGraphicsMode(inPresentation: QTSPresentation; inStream: QTSStream; var outMode: SInt16; var outOpColor: RGBColor): OSErr; external name '_QTSPresGetGraphicsMode';

{
 *  QTSPresGetPicture()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function QTSPresGetPicture(inPresentation: QTSPresentation; inStream: QTSStream; var outPicture: PicHandle): OSErr; external name '_QTSPresGetPicture';

{-----------------------------------------
    Stream Sound Functions
-----------------------------------------}
{
 *  QTSPresSetVolumes()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function QTSPresSetVolumes(inPresentation: QTSPresentation; inStream: QTSStream; inLeftVolume: SInt16; inRightVolume: SInt16): OSErr; external name '_QTSPresSetVolumes';

{
 *  QTSPresGetVolumes()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function QTSPresGetVolumes(inPresentation: QTSPresentation; inStream: QTSStream; var outLeftVolume: SInt16; var outRightVolume: SInt16): OSErr; external name '_QTSPresGetVolumes';

{-----------------------------------------
    Sourcing
-----------------------------------------}
{
 *  QTSPresGetSettingsAsText()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 5.0.1 and later
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.1 and later
 }
function QTSPresGetSettingsAsText(inPresentation: QTSPresentation; inStream: QTSStream; inFlags: SInt32; inSettingsType: OSType; var outText: Handle; inPanelFilterProc: QTSPanelFilterUPP; inPanelFilterProcRefCon: UnivPtr): OSErr; external name '_QTSPresGetSettingsAsText';

{
 *  QTSPresSettingsDialog()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 5.0 and later
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 }
function QTSPresSettingsDialog(inPresentation: QTSPresentation; inStream: QTSStream; inFlags: SInt32; inFilterProc: QTSModalFilterUPP; inFilterProcRefCon: UnivPtr): OSErr; external name '_QTSPresSettingsDialog';

{
 *  QTSPresSettingsDialogWithFilters()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 5.0.1 and later
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.1 and later
 }
function QTSPresSettingsDialogWithFilters(inPresentation: QTSPresentation; inStream: QTSStream; inFlags: SInt32; inFilterProc: QTSModalFilterUPP; inFilterProcRefCon: UnivPtr; inPanelFilterProc: QTSPanelFilterUPP; inPanelFilterProcRefCon: UnivPtr): OSErr; external name '_QTSPresSettingsDialogWithFilters';

{
 *  QTSPresSetSettings()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 5.0 and later
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 }
function QTSPresSetSettings(inPresentation: QTSPresentation; inStream: QTSStream; inSettings: QTAtomSpecPtr; inFlags: SInt32): OSErr; external name '_QTSPresSetSettings';

{
 *  QTSPresGetSettings()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 5.0 and later
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 }
function QTSPresGetSettings(inPresentation: QTSPresentation; inStream: QTSStream; var outSettings: QTAtomContainer; inFlags: SInt32): OSErr; external name '_QTSPresGetSettings';

{
 *  QTSPresAddSourcer()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 5.0 and later
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 }
function QTSPresAddSourcer(inPresentation: QTSPresentation; inStream: QTSStream; inSourcer: ComponentInstance; inFlags: SInt32): OSErr; external name '_QTSPresAddSourcer';

{
 *  QTSPresRemoveSourcer()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 5.0 and later
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 }
function QTSPresRemoveSourcer(inPresentation: QTSPresentation; inStream: QTSStream; inSourcer: ComponentInstance; inFlags: SInt32): OSErr; external name '_QTSPresRemoveSourcer';

{
 *  QTSPresGetNumSourcers()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 5.0 and later
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 }
function QTSPresGetNumSourcers(inPresentation: QTSPresentation; inStream: QTSStream): UInt32; external name '_QTSPresGetNumSourcers';

{
 *  QTSPresGetIndSourcer()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 5.0 and later
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 }
function QTSPresGetIndSourcer(inPresentation: QTSPresentation; inStream: QTSStream; inIndex: UInt32; var outSourcer: ComponentInstance): OSErr; external name '_QTSPresGetIndSourcer';

{============================================================================
        Misc
============================================================================}
{ flags for Get/SetNetworkAppName }

const
	kQTSNetworkAppNameIsFullNameFlag = $00000001;

	{
	 *  QTSSetNetworkAppName()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in QTStreamLib 4.1 and later
	 *    CarbonLib:        in CarbonLib 1.1 and later
	 *    Mac OS X:         in version 10.0 and later
	 *    Windows:          in QTSClient.lib 4.1 and later
	 	}
function QTSSetNetworkAppName(inAppName: ConstCStringPtr; inFlags: SInt32): OSErr; external name '_QTSSetNetworkAppName';

{
 *  QTSGetNetworkAppName()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.1 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.1 and later
 }
function QTSGetNetworkAppName(inFlags: SInt32; var outCStringPtr: CStringPtr): OSErr; external name '_QTSGetNetworkAppName';

{-----------------------------------------
    Statistics Utilities
-----------------------------------------}

type
	QTSStatHelperRecordPtr = ^QTSStatHelperRecord;
	QTSStatHelperRecord = record
		data:					array [0..0] of SInt32;
	end;

	QTSStatHelper						= ^QTSStatHelperRecord;

const
	kQTSInvalidStatHelper		= 0;

	{	 flags for QTSStatHelperNextParams 	}
	kQTSStatHelperReturnPascalStringsFlag = $00000001;


type
	QTSStatHelperNextParamsPtr = ^QTSStatHelperNextParams;
	QTSStatHelperNextParams = record
		flags:					SInt32;
		returnedStatisticsType:	OSType;
		returnedStream:			QTSStream;
		maxStatNameLength:		UInt32;
		returnedStatName:		CStringPtr;								{  NULL if you don't want it }
		maxStatStringLength:	UInt32;
		returnedStatString:		CStringPtr;								{  NULL if you don't want it }
		maxStatUnitLength:		UInt32;
		returnedStatUnit:		CStringPtr;								{  NULL if you don't want it }
	end;

	QTSStatisticsParamsPtr = ^QTSStatisticsParams;
	QTSStatisticsParams = record
		statisticsType:			OSType;
		container:				QTAtomContainer;
		parentAtom:				QTAtom;
		flags:					SInt32;
	end;

	{	 general statistics types 	}

const
	kQTSAllStatisticsType		= $616C6C20 (* 'all ' *);
	kQTSShortStatisticsType		= $73687274 (* 'shrt' *);
	kQTSSummaryStatisticsType	= $73756D6D (* 'summ' *);

	{	 statistics flags 	}
	kQTSGetNameStatisticsFlag	= $00000001;
	kQTSDontGetDataStatisticsFlag = $00000002;
	kQTSUpdateAtomsStatisticsFlag = $00000004;
	kQTSGetUnitsStatisticsFlag	= $00000008;
	kQTSUpdateAllIfNecessaryStatisticsFlag = $00010000;

	{	 statistics atom types 	}
	kQTSStatisticsStreamAtomType = $7374726D (* 'strm' *);
	kQTSStatisticsNameAtomType	= $6E616D65 (* 'name' *);						{  chars only, no length or terminator  }
	kQTSStatisticsDataFormatAtomType = $66726D74 (* 'frmt' *);					{  OSType  }
	kQTSStatisticsDataAtomType	= $64617461 (* 'data' *);
	kQTSStatisticsUnitsAtomType	= $756E6974 (* 'unit' *);						{  OSType  }
	kQTSStatisticsUnitsNameAtomType = $756E696E (* 'unin' *);					{  chars only, no length or terminator  }

	{	 statistics data formats 	}
	kQTSStatisticsSInt32DataFormat = $73693332 (* 'si32' *);
	kQTSStatisticsUInt32DataFormat = $75693332 (* 'ui32' *);
	kQTSStatisticsSInt16DataFormat = $73693136 (* 'si16' *);
	kQTSStatisticsUInt16DataFormat = $75693136 (* 'ui16' *);
	kQTSStatisticsFixedDataFormat = $66697864 (* 'fixd' *);
	kQTSStatisticsUnsignedFixedDataFormat = $75666978 (* 'ufix' *);
	kQTSStatisticsStringDataFormat = $73747267 (* 'strg' *);
	kQTSStatisticsOSTypeDataFormat = $6F737470 (* 'ostp' *);
	kQTSStatisticsRectDataFormat = $72656374 (* 'rect' *);
	kQTSStatisticsPointDataFormat = $706F6E74 (* 'pont' *);

	{	 statistics units types 	}
	kQTSStatisticsNoUnitsType	= 0;
	kQTSStatisticsPercentUnitsType = $70636E74 (* 'pcnt' *);
	kQTSStatisticsBitsPerSecUnitsType = $62707320 (* 'bps ' *);
	kQTSStatisticsFramesPerSecUnitsType = $66707320 (* 'fps ' *);

	{	 specific statistics types 	}
	kQTSTotalDataRateStat		= $64727474 (* 'drtt' *);
	kQTSTotalDataRateInStat		= $64727469 (* 'drti' *);
	kQTSTotalDataRateOutStat	= $6472746F (* 'drto' *);
	kQTSNetworkIDStringStat		= $6E696473 (* 'nids' *);

	{
	 *  QTSNewStatHelper()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
	 *    CarbonLib:        in CarbonLib 1.1 and later
	 *    Mac OS X:         in version 10.0 and later
	 *    Windows:          in QTSClient.lib 4.0 and later
	 	}
function QTSNewStatHelper(inPresentation: QTSPresentation; inStream: QTSStream; inStatType: OSType; inFlags: SInt32; var outStatHelper: QTSStatHelper): OSErr; external name '_QTSNewStatHelper';

{
 *  QTSDisposeStatHelper()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function QTSDisposeStatHelper(inStatHelper: QTSStatHelper): OSErr; external name '_QTSDisposeStatHelper';

{
 *  QTSStatHelperGetStats()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function QTSStatHelperGetStats(inStatHelper: QTSStatHelper): OSErr; external name '_QTSStatHelperGetStats';

{
 *  QTSStatHelperResetIter()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function QTSStatHelperResetIter(inStatHelper: QTSStatHelper): OSErr; external name '_QTSStatHelperResetIter';

{
 *  QTSStatHelperNext()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function QTSStatHelperNext(inStatHelper: QTSStatHelper; var ioParams: QTSStatHelperNextParams): boolean; external name '_QTSStatHelperNext';

{
 *  QTSStatHelperGetNumStats()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function QTSStatHelperGetNumStats(inStatHelper: QTSStatHelper): UInt32; external name '_QTSStatHelperGetNumStats';

{ used by components to put statistics into the atom container }
{
 *  QTSGetOrMakeStatAtomForStream()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function QTSGetOrMakeStatAtomForStream(inContainer: QTAtomContainer; inStream: QTSStream; var outParentAtom: QTAtom): OSErr; external name '_QTSGetOrMakeStatAtomForStream';

{
 *  QTSInsertStatistic()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function QTSInsertStatistic(inContainer: QTAtomContainer; inParentAtom: QTAtom; inStatType: OSType; inStatData: UnivPtr; inStatDataLength: UInt32; inStatDataFormat: OSType; inFlags: SInt32): OSErr; external name '_QTSInsertStatistic';

{
 *  QTSInsertStatisticName()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function QTSInsertStatisticName(inContainer: QTAtomContainer; inParentAtom: QTAtom; inStatType: OSType; inStatName: ConstCStringPtr; inStatNameLength: UInt32): OSErr; external name '_QTSInsertStatisticName';

{
 *  QTSInsertStatisticUnits()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function QTSInsertStatisticUnits(inContainer: QTAtomContainer; inParentAtom: QTAtom; inStatType: OSType; inUnitsType: OSType; inUnitsName: ConstCStringPtr; inUnitsNameLength: UInt32): OSErr; external name '_QTSInsertStatisticUnits';

{============================================================================
        Data Formats
============================================================================}
{-----------------------------------------
    Data Types
-----------------------------------------}
{ universal data types }

const
	kQTSNullDataType			= $4E554C4C (* 'NULL' *);
	kQTSUnknownDataType			= $6875683F (* 'huh?' *);
	kQTSAtomContainerDataType	= $71746163 (* 'qtac' *);						{  QTAtomContainer  }
	kQTSAtomDataType			= $71746174 (* 'qtat' *);						{  QTSAtomContainerDataStruct*  }
	kQTSAliasDataType			= $616C6973 (* 'alis' *);
	kQTSFileDataType			= $66737063 (* 'fspc' *);						{  FSSpec*  }
	kQTSFileSpecDataType		= $66737063 (* 'fspc' *);						{  FSSpec*  }
	kQTSHandleDataType			= $686E646C (* 'hndl' *);						{  Handle*  }
	kQTSDataRefDataType			= $64726566 (* 'dref' *);						{  DataReferencePtr  }

	{	 these data types are specific to presentations 	}
	kQTSRTSPDataType			= $72747370 (* 'rtsp' *);
	kQTSSDPDataType				= $73647020 (* 'sdp ' *);

	{	-----------------------------------------
	    Atom IDs
	-----------------------------------------	}
	kQTSAtomType_Presentation	= $70726573 (* 'pres' *);
	kQTSAtomType_PresentationHeader = $70686472 (* 'phdr' *);					{  QTSPresentationHeaderAtom  }
	kQTSAtomType_MediaStream	= $6D737472 (* 'mstr' *);
	kQTSAtomType_MediaStreamHeader = $6D736864 (* 'mshd' *);					{  QTSMediaStreamHeaderAtom  }
	kQTSAtomType_MediaDescriptionText = $6D646573 (* 'mdes' *);					{  chars, no length  }
	kQTSAtomType_ClipRect		= $636C6970 (* 'clip' *);						{  QTSClipRectAtom  }
	kQTSAtomType_Duration		= $64757261 (* 'dura' *);						{  QTSDurationAtom  }
	kQTSAtomType_BufferTime		= $62756672 (* 'bufr' *);						{  QTSBufferTimeAtom  }


type
	QTSAtomContainerDataStructPtr = ^QTSAtomContainerDataStruct;
	QTSAtomContainerDataStruct = record
		container:				QTAtomContainer;
		parentAtom:				QTAtom;
	end;

	{	 flags for QTSPresentationHeaderAtom 	}

const
	kQTSPresHeaderTypeIsData	= $00000100;
	kQTSPresHeaderDataIsHandle	= $00000200;


type
	QTSPresentationHeaderAtomPtr = ^QTSPresentationHeaderAtom;
	QTSPresentationHeaderAtom = record
		versionAndFlags:		SInt32;
		conductorOrDataType:	OSType;
		dataAtomType:			OSType;									{  where the data really is }
	end;

	QTSMediaStreamHeaderAtomPtr = ^QTSMediaStreamHeaderAtom;
	QTSMediaStreamHeaderAtom = record
		versionAndFlags:		SInt32;
		mediaTransportType:		OSType;
		mediaTransportDataAID:	OSType;									{  where the data really is }
	end;

	QTSBufferTimeAtomPtr = ^QTSBufferTimeAtom;
	QTSBufferTimeAtom = record
		versionAndFlags:		SInt32;
		bufferTime:				Fixed;
	end;

	QTSDurationAtomPtr = ^QTSDurationAtom;
	QTSDurationAtom = record
		versionAndFlags:		SInt32;
		timeScale:				TimeScale_fix;
		duration:				TimeValue64;
	end;

	QTSClipRectAtomPtr = ^QTSClipRectAtom;
	QTSClipRectAtom = record
		versionAndFlags:		SInt32;
		clipRect:				Rect;
	end;


const
	kQTSEmptyEditStreamStartTime = -1;


type
	QTSStatus							= UInt32;

const
	kQTSNullStatus				= 0;
	kQTSUninitializedStatus		= 1;
	kQTSConnectingStatus		= 2;
	kQTSOpeningConnectionDetailedStatus = 3;
	kQTSMadeConnectionDetailedStatus = 4;
	kQTSNegotiatingStatus		= 5;
	kQTSGettingDescriptionDetailedStatus = 6;
	kQTSGotDescriptionDetailedStatus = 7;
	kQTSSentSetupCmdDetailedStatus = 8;
	kQTSReceivedSetupResponseDetailedStatus = 9;
	kQTSSentPlayCmdDetailedStatus = 10;
	kQTSReceivedPlayResponseDetailedStatus = 11;
	kQTSBufferingStatus			= 12;
	kQTSPlayingStatus			= 13;
	kQTSPausedStatus			= 14;
	kQTSAutoConfiguringStatus	= 15;
	kQTSDownloadingStatus		= 16;
	kQTSBufferingWithTimeStatus	= 17;
	kQTSWaitingDisconnectStatus	= 100;

	{	-----------------------------------------
	    QuickTime Preferences Types
	-----------------------------------------	}
	kQTSConnectionPrefsType		= $7374636D (* 'stcm' *);						{  root atom that all other atoms are contained in }
																{     kQTSNotUsedForProxyPrefsType = 'nopr',     //        comma-delimited list of URLs that are never used for proxies }
	kQTSConnectionMethodPrefsType = $6D746864 (* 'mthd' *);						{       connection method (OSType that matches one of the following three) }
	kQTSDirectConnectPrefsType	= $64726374 (* 'drct' *);						{        used if direct connect (QTSDirectConnectPrefsRecord) }
																{     kQTSRTSPProxyPrefsType =     'rtsp',   //   used if RTSP Proxy (QTSProxyPrefsRecord) }
	kQTSSOCKSPrefsType			= $736F636B (* 'sock' *);						{        used if SOCKS Proxy (QTSProxyPrefsRecord) }

	kQTSDirectConnectHTTPProtocol = $68747470 (* 'http' *);
	kQTSDirectConnectRTSPProtocol = $72747370 (* 'rtsp' *);


type
	QTSDirectConnectPrefsRecordPtr = ^QTSDirectConnectPrefsRecord;
	QTSDirectConnectPrefsRecord = record
		tcpPortID:				UInt32;
		protocol:				OSType;
	end;

	QTSProxyPrefsRecordPtr = ^QTSProxyPrefsRecord;
	QTSProxyPrefsRecord = record
		serverNameStr:			Str255;
		portID:					UInt32;
	end;


const
	kConnectionActive			= $00000001;
	kConnectionUseSystemPref	= $00000002;


type
	QTSTransportPrefPtr = ^QTSTransportPref;
	QTSTransportPref = record
		protocol:				OSType;									{  udp, http, tcp, etc }
		portID:					SInt32;									{  port to use for this connection type }
		flags:					UInt32;									{  connection flags }
		seed:					UInt32;									{  seed value last time this setting was read from system prefs }
	end;


const
	kProxyActive				= $00000001;
	kProxyUseSystemPref			= $00000002;


type
	QTSProxyPrefPtr = ^QTSProxyPref;
	QTSProxyPref = record
		flags:					UInt32;									{  proxy flags }
		portID:					SInt32;									{  port to use for this connection type }
		seed:					UInt32;									{  seed value last time this setting was read from system prefs }
		serverNameStr:			Str255;									{  proxy server url }
	end;


const
	kNoProxyUseSystemPref		= $00000001;


type
	QTSNoProxyPrefPtr = ^QTSNoProxyPref;
	QTSNoProxyPref = record
		flags:					UInt32;									{  no-proxy flags }
		seed:					UInt32;									{  seed value last time this setting was read from system prefs }
		urlList:				SInt8;									{  NULL terminated, comma delimited list of urls }
	end;


const
	kQTSInstantOnFlag_Enable	= $00000001;					{  instant on is enabled (read/write) }
	kQTSInstantOnFlag_Permitted	= $00000002;					{  instant on is possible (read only) }


type
	QTSInstantOnPrefPtr = ^QTSInstantOnPref;
	QTSInstantOnPref = record
		flags:					SInt32;									{  flags }
		factor:					SInt32;									{     0-100; default is 50 }
	end;


const
	kQTSTransAndProxyAtomType	= $73747270 (* 'strp' *);						{  transport/proxy prefs root atom }
	kQTSConnectionPrefsVersion	= $76657273 (* 'vers' *);						{    prefs format version }
	kQTSTransportPrefsAtomType	= $74726E73 (* 'trns' *);						{    tranport prefs root atom }
	kQTSConnectionAtomType		= $636F6E6E (* 'conn' *);						{      connection prefs atom type, one for each transport type }
	kQTSUDPTransportType		= $75647020 (* 'udp ' *);						{      udp transport prefs }
	kQTSHTTPTransportType		= $68747470 (* 'http' *);						{      http transport prefs }
	kQTSTCPTransportType		= $74637020 (* 'tcp ' *);						{      tcp transport prefs     }
	kQTSProxyPrefsAtomType		= $70727879 (* 'prxy' *);						{    proxy prefs root atom }
	kQTSHTTPProxyPrefsType		= $68747470 (* 'http' *);						{      http proxy settings }
	kQTSRTSPProxyPrefsType		= $72747370 (* 'rtsp' *);						{      rtsp proxy settings }
	kQTSSOCKSProxyPrefsType		= $736F636B (* 'sock' *);						{      socks proxy settings }
	kQTSProxyUserInfoPrefsType	= $75736572 (* 'user' *);						{    proxy username/password root atom }
	kQTSDontProxyPrefsAtomType	= $6E6F7072 (* 'nopr' *);						{    no-proxy prefs root atom }
	kQTSDontProxyDataType		= $64617461 (* 'data' *);						{      no proxy settings }
	kQTSInstantOnPrefsAtomType	= $696E6F6E (* 'inon' *);						{  instant on prefs }

	{
	 *  QTSPrefsAddProxySetting()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in QTStreamLib 4.1 and later
	 *    CarbonLib:        in CarbonLib 1.1 and later
	 *    Mac OS X:         in version 10.0 and later
	 *    Windows:          in QTSClient.lib 4.1 and later
	 	}
function QTSPrefsAddProxySetting(proxyType: OSType; portID: SInt32; flags: UInt32; seed: UInt32; var srvrURL: Str255): OSErr; external name '_QTSPrefsAddProxySetting';

{
 *  QTSPrefsFindProxyByType()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.1 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.1 and later
 }
function QTSPrefsFindProxyByType(proxyType: OSType; flags: UInt32; flagsMask: UInt32; var proxyHndl: UnivPtr; var count: SInt16): OSErr; external name '_QTSPrefsFindProxyByType';

{
 *  QTSPrefsAddConnectionSetting()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.1 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.1 and later
 }
function QTSPrefsAddConnectionSetting(protocol: OSType; portID: SInt32; flags: UInt32; seed: UInt32): OSErr; external name '_QTSPrefsAddConnectionSetting';

{
 *  QTSPrefsFindConnectionByType()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.1 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.1 and later
 }
function QTSPrefsFindConnectionByType(protocol: OSType; flags: UInt32; flagsMask: UInt32; var connectionHndl: UnivPtr; var count: SInt16): OSErr; external name '_QTSPrefsFindConnectionByType';

{
 *  QTSPrefsGetActiveConnection()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.1 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.1 and later
 }
function QTSPrefsGetActiveConnection(protocol: OSType; var connectInfo: QTSTransportPref): OSErr; external name '_QTSPrefsGetActiveConnection';

{
 *  QTSPrefsGetNoProxyURLs()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.1 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.1 and later
 }
function QTSPrefsGetNoProxyURLs(var noProxyHndl: UnivPtr): OSErr; external name '_QTSPrefsGetNoProxyURLs';

{
 *  QTSPrefsSetNoProxyURLs()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.1 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.1 and later
 }
function QTSPrefsSetNoProxyURLs(urls: CStringPtr; flags: UInt32; seed: UInt32): OSErr; external name '_QTSPrefsSetNoProxyURLs';

{
 *  QTSPrefsAddProxyUserInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 5.0.1 and later
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.1 and later
 *    Windows:          in QTSClient.lib 5.0.1 and later
 }
function QTSPrefsAddProxyUserInfo(proxyType: OSType; flags: SInt32; flagsMask: SInt32; username: StringPtr; password: StringPtr): OSErr; external name '_QTSPrefsAddProxyUserInfo';

{
 *  QTSPrefsFindProxyUserInfoByType()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 5.0.1 and later
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.1 and later
 *    Windows:          in QTSClient.lib 5.0.1 and later
 }
function QTSPrefsFindProxyUserInfoByType(proxyType: OSType; flags: SInt32; flagsMask: SInt32; username: StringPtr; password: StringPtr): OSErr; external name '_QTSPrefsFindProxyUserInfoByType';

{
 *  QTSPrefsGetInstantOnSettings()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 6.0 and later
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.2 and later
 *    Windows:          in QTSClient.lib 6.0 and later
 }
function QTSPrefsGetInstantOnSettings(var outPref: QTSInstantOnPref; inFlags: SInt32): OSErr; external name '_QTSPrefsGetInstantOnSettings';

{
 *  QTSPrefsSetInstantOnSettings()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 6.0 and later
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.2 and later
 *    Windows:          in QTSClient.lib 6.0 and later
 }
function QTSPrefsSetInstantOnSettings(var inPref: QTSInstantOnPref; inFlags: SInt32): OSErr; external name '_QTSPrefsSetInstantOnSettings';


{============================================================================
        Memory Management Services
============================================================================}
{
   These routines allocate normal pointers and handles,
   but do the correct checking, etc.
   Dispose using the normal DisposePtr and DisposeHandle
   Call these routines for one time memory allocations.
   You do not need to set any hints to use these calls.
}

{
 *  QTSNewPtr()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function QTSNewPtr(inByteCount: UInt32; inFlags: SInt32; var outFlags: SInt32): Ptr; external name '_QTSNewPtr';

{
 *  QTSNewHandle()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function QTSNewHandle(inByteCount: UInt32; inFlags: SInt32; var outFlags: SInt32): Handle; external name '_QTSNewHandle';

{  flags in }

const
	kQTSMemAllocClearMem		= $00000001;
	kQTSMemAllocDontUseTempMem	= $00000002;
	kQTSMemAllocTryTempMemFirst	= $00000004;
	kQTSMemAllocDontUseSystemMem = $00000008;
	kQTSMemAllocTrySystemMemFirst = $00000010;
	kQTSMemAllocHoldMemory		= $00001000;
	kQTSMemAllocIsInterruptTime	= $01010000;					{  currently not supported for alloc }

	{  flags out }
	kQTSMemAllocAllocatedInTempMem = $00000001;
	kQTSMemAllocAllocatedInSystemMem = $00000002;


type
	QTSMemPtr    = ^SInt32; { an opaque 32-bit type }
	QTSMemPtrPtr = ^QTSMemPtr;  { when a var xx:QTSMemPtr parameter can be nil, it is changed to xx: QTSMemPtrPtr }
	{
	   These routines are for buffers that will be recirculated
	   you must use QTReleaseMemPtr instead of DisposePtr
	   QTSReleaseMemPtr can be used at interrupt time
	   but QTSAllocMemPtr currently cannot 
	}
	{
	 *  QTSAllocMemPtr()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
	 *    CarbonLib:        in CarbonLib 1.1 and later
	 *    Mac OS X:         in version 10.0 and later
	 *    Windows:          in QTSClient.lib 4.0 and later
	 	}
function QTSAllocMemPtr(inByteCount: UInt32; inFlags: SInt32): QTSMemPtr; external name '_QTSAllocMemPtr';

{
 *  QTSReleaseMemPtr()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
procedure QTSReleaseMemPtr(inMemPtr: QTSMemPtr; inFlags: SInt32); external name '_QTSReleaseMemPtr';


{============================================================================
        Buffer Management Services
============================================================================}


const
	kQTSStreamBufferVersion1	= 1;


type
	QTSStreamBufferPtr = ^QTSStreamBuffer;
	QTSStreamBuffer = record
		reserved1:				QTSStreamBufferPtr;
		reserved2:				QTSStreamBufferPtr;
		next:					QTSStreamBufferPtr;						{  next message block in a message  }
		rptr:					Ptr;									{  first byte with real data in the DataBuffer  }
		wptr:					Ptr;									{  last+1 byte with real data in the DataBuffer  }
		version:				SInt32;
		metadata:				array [0..3] of UInt32;					{  usage defined by message sender  }
		flags:					SInt32;									{  reserved  }
		reserved3:				SInt32;
		reserved4:				SInt32;
		reserved5:				SInt32;
		moreMeta:				array [0..7] of UInt32;
	end;

	{  flags for QTSDuplicateMessage }

const
	kQTSDuplicateBufferFlag_CopyData = $00000001;
	kQTSDuplicateBufferFlag_FlattenMessage = $00000002;


	{
	 *  QTSNewStreamBuffer()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in QTStreamLib 5.0 and later
	 *    CarbonLib:        in CarbonLib 1.3 and later
	 *    Mac OS X:         in version 10.0 and later
	 *    Windows:          in QTSClient.lib 5.0 and later
	 	}
function QTSNewStreamBuffer(inDataSize: UInt32; inFlags: SInt32; var outStreamBuffer: UnivPtr): OSErr; external name '_QTSNewStreamBuffer';

{
 *  QTSFreeMessage()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
procedure QTSFreeMessage(var inMessage: QTSStreamBuffer); external name '_QTSFreeMessage';

{
    kQTSDuplicateBufferFlag_CopyData - forces a copy of the data itself
    kQTSCopyBufferFlag_FlattenMessage - copies the data if it needs to be flattened
    QTSDuplicateMessage never frees the old message
}
{
 *  QTSDuplicateMessage()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 5.0 and later
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 5.0 and later
 }
function QTSDuplicateMessage(var inMessage: QTSStreamBuffer; inFlags: SInt32; var outDuplicatedMessage: UnivPtr): OSErr; external name '_QTSDuplicateMessage';

{
 *  QTSMessageLength()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function QTSMessageLength(var inMessage: QTSStreamBuffer): UInt32; external name '_QTSMessageLength';

{
 *  QTSStreamBufferDataInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 5.0 and later
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 5.0 and later
 }
procedure QTSStreamBufferDataInfo(var inStreamBuffer: QTSStreamBuffer; var outDataStart: UnivPtr; var outDataMaxLength: UInt32); external name '_QTSStreamBufferDataInfo';

{  ---- old calls (don't use these) }

{
 *  QTSAllocBuffer()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function QTSAllocBuffer(inSize: SInt32): QTSStreamBufferPtr; external name '_QTSAllocBuffer';

{
 *  QTSDupMessage()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function QTSDupMessage(var inMessage: QTSStreamBuffer): QTSStreamBufferPtr; external name '_QTSDupMessage';

{
 *  QTSCopyMessage()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function QTSCopyMessage(var inMessage: QTSStreamBuffer): QTSStreamBufferPtr; external name '_QTSCopyMessage';

{
 *  QTSFlattenMessage()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function QTSFlattenMessage(var inMessage: QTSStreamBuffer): QTSStreamBufferPtr; external name '_QTSFlattenMessage';


{============================================================================
        Misc
============================================================================}
{
 *  QTSGetErrorString()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function QTSGetErrorString(inErrorCode: SInt32; inMaxErrorStringLength: UInt32; outErrorString: CStringPtr; inFlags: SInt32): boolean; external name '_QTSGetErrorString';

{
 *  QTSInitializeMediaParams()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 5.0.1 and later
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.1 and later
 *    Windows:          in QTSClient.lib 5.0.1 and later
 }
function QTSInitializeMediaParams(var inMediaParams: QTSMediaParams): OSErr; external name '_QTSInitializeMediaParams';


{ UPP call backs }

const
	uppQTSNotificationProcInfo = $00003FF0;
	uppQTSPanelFilterProcInfo = $000003D0;
	uppQTSModalFilterProcInfo = $00003FD0;
	{
	 *  NewQTSNotificationUPP()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   available as macro/inline
	 *    CarbonLib:        in CarbonLib 1.3 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
function NewQTSNotificationUPP(userRoutine: QTSNotificationProcPtr): QTSNotificationUPP; external name '_NewQTSNotificationUPP'; { old name was NewQTSNotificationProc }
{
 *  NewQTSPanelFilterUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.1 and later
 }
function NewQTSPanelFilterUPP(userRoutine: QTSPanelFilterProcPtr): QTSPanelFilterUPP; external name '_NewQTSPanelFilterUPP'; { old name was NewQTSPanelFilterProc }
{
 *  NewQTSModalFilterUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewQTSModalFilterUPP(userRoutine: QTSModalFilterProcPtr): QTSModalFilterUPP; external name '_NewQTSModalFilterUPP'; { old name was NewQTSModalFilterProc }
{
 *  DisposeQTSNotificationUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeQTSNotificationUPP(userUPP: QTSNotificationUPP); external name '_DisposeQTSNotificationUPP';
{
 *  DisposeQTSPanelFilterUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.1 and later
 }
procedure DisposeQTSPanelFilterUPP(userUPP: QTSPanelFilterUPP); external name '_DisposeQTSPanelFilterUPP';
{
 *  DisposeQTSModalFilterUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeQTSModalFilterUPP(userUPP: QTSModalFilterUPP); external name '_DisposeQTSModalFilterUPP';
{
 *  InvokeQTSNotificationUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 }
function InvokeQTSNotificationUPP(inErr: ComponentResult; inNotificationType: OSType; inNotificationParams: UnivPtr; inRefCon: UnivPtr; userRoutine: QTSNotificationUPP): ComponentResult; external name '_InvokeQTSNotificationUPP'; { old name was CallQTSNotificationProc }
{
 *  InvokeQTSPanelFilterUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.1 and later
 }
function InvokeQTSPanelFilterUPP(var inParams: QTSPanelFilterParams; inRefCon: UnivPtr; userRoutine: QTSPanelFilterUPP): boolean; external name '_InvokeQTSPanelFilterUPP'; { old name was CallQTSPanelFilterProc }
{
 *  InvokeQTSModalFilterUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 }
function InvokeQTSModalFilterUPP(inDialog: DialogPtr; const (*var*) inEvent: EventRecord; var ioItemHit: SInt16; inRefCon: UnivPtr; userRoutine: QTSModalFilterUPP): boolean; external name '_InvokeQTSModalFilterUPP'; { old name was CallQTSModalFilterProc }
{$ALIGN MAC68K}


end.
