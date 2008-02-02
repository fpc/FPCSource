{
     File:       QuickTimeComponents.p
 
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

unit QuickTimeComponents;
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
uses MacTypes,Files,Events,QDOffscreen,Menus,Dialogs,Aliases,MixedMode,Components,Quickdraw,Video,Sound,ImageCompression,Movies,QuickTimeMusic;


{$ALIGN MAC68K}


const
	clockComponentType			= $636C6F6B (* 'clok' *);
	systemTickClock				= $7469636B (* 'tick' *);						{  subtype: 60ths since boot    }
	systemSecondClock			= $7365636F (* 'seco' *);						{  subtype: seconds since 1904        }
	systemMillisecondClock		= $6D696C6C (* 'mill' *);						{  subtype: 1000ths since boot        }
	systemMicrosecondClock		= $6D696372 (* 'micr' *);						{  subtype: 1000000ths since boot  }

	kClockRateIsLinear			= 1;
	kClockImplementsCallBacks	= 2;
	kClockCanHandleIntermittentSound = 4;						{  sound clocks only  }

	{	* These are Clock procedures *	}
	{
	 *  ClockGetTime()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 *    Windows:          in qtmlClient.lib 3.0 and later
	 	}
function ClockGetTime(aClock: ComponentInstance; var out: TimeRecord): ComponentResult; external name '_ClockGetTime';
{
 *  ClockNewCallBack()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ClockNewCallBack(aClock: ComponentInstance; tb: TimeBase; callBackType: SInt16): QTCallBack; external name '_ClockNewCallBack';
{
 *  ClockDisposeCallBack()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ClockDisposeCallBack(aClock: ComponentInstance; cb: QTCallBack): ComponentResult; external name '_ClockDisposeCallBack';
{
 *  ClockCallMeWhen()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ClockCallMeWhen(aClock: ComponentInstance; cb: QTCallBack; param1: SInt32; param2: SInt32; param3: SInt32): ComponentResult; external name '_ClockCallMeWhen';
{
 *  ClockCancelCallBack()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ClockCancelCallBack(aClock: ComponentInstance; cb: QTCallBack): ComponentResult; external name '_ClockCancelCallBack';
{
 *  ClockRateChanged()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ClockRateChanged(aClock: ComponentInstance; cb: QTCallBack): ComponentResult; external name '_ClockRateChanged';
{
 *  ClockTimeChanged()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ClockTimeChanged(aClock: ComponentInstance; cb: QTCallBack): ComponentResult; external name '_ClockTimeChanged';
{
 *  ClockSetTimeBase()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ClockSetTimeBase(aClock: ComponentInstance; tb: TimeBase): ComponentResult; external name '_ClockSetTimeBase';
{
 *  ClockStartStopChanged()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ClockStartStopChanged(aClock: ComponentInstance; cb: QTCallBack; startChanged: boolean; stopChanged: boolean): ComponentResult; external name '_ClockStartStopChanged';
{
 *  ClockGetRate()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ClockGetRate(aClock: ComponentInstance; var rate: Fixed): ComponentResult; external name '_ClockGetRate';
const
	StandardCompressionType		= $73636469 (* 'scdi' *);
	StandardCompressionSubType	= $696D6167 (* 'imag' *);
	StandardCompressionSubTypeSound = $736F756E (* 'soun' *);


type
{$ifc TYPED_FUNCTION_POINTERS}
	SCModalFilterProcPtr = function(theDialog: DialogRef; var theEvent: EventRecord; var itemHit: SInt16; refcon: SInt32): boolean;
{$elsec}
	SCModalFilterProcPtr = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	SCModalHookProcPtr = function(theDialog: DialogRef; itemHit: SInt16; params: UnivPtr; refcon: SInt32): SInt16;
{$elsec}
	SCModalHookProcPtr = ProcPtr;
{$endc}

{$ifc OPAQUE_UPP_TYPES}
	SCModalFilterUPP = ^SInt32; { an opaque UPP }
{$elsec}
	SCModalFilterUPP = UniversalProcPtr;
{$endc}	
{$ifc OPAQUE_UPP_TYPES}
	SCModalHookUPP = ^SInt32; { an opaque UPP }
{$elsec}
	SCModalHookUPP = UniversalProcPtr;
{$endc}	
	{   Preference flags. }

const
	scListEveryCodec			= $00000002;
	scAllowZeroFrameRate		= $00000004;
	scAllowZeroKeyFrameRate		= $00000008;
	scShowBestDepth				= $00000010;
	scUseMovableModal			= $00000020;
	scDisableFrameRateItem		= $00000040;
	scShowDataRateAsKilobits	= $00000080;


	{   Possible test flags for setting test image. }
	scPreferCropping			= $01;
	scPreferScaling				= $02;
	scPreferScalingAndCropping	= $03;
	scDontDetermineSettingsFromTestImage = $04;


	{   Dimensions of the image preview box. }
	scTestImageWidth			= 80;
	scTestImageHeight			= 80;

	{   Possible items returned by hookProc. }
	scOKItem					= 1;
	scCancelItem				= 2;
	scCustomItem				= 3;

	{   Result returned when user cancelled. }
	scUserCancelled				= 1;


	{   Get/SetInfo structures. }


type
	SCSpatialSettingsPtr = ^SCSpatialSettings;
	SCSpatialSettings = record
		codecType:				CodecType_fix;
		codec:					CodecComponent;
		depth:					SInt16;
		spatialQuality:			CodecQ;
	end;

	SCTemporalSettingsPtr = ^SCTemporalSettings;
	SCTemporalSettings = record
		temporalQuality:		CodecQ;
		frameRate:				Fixed;
		keyFrameRate:			SInt32;
	end;

	SCDataRateSettingsPtr = ^SCDataRateSettings;
	SCDataRateSettings = record
		dataRate:				SInt32;
		frameDuration:			SInt32;
		minSpatialQuality:		CodecQ;
		minTemporalQuality:		CodecQ;
	end;

	SCExtendedProcsPtr = ^SCExtendedProcs;
	SCExtendedProcs = record
		filterProc:				SCModalFilterUPP;
		hookProc:				SCModalHookUPP;
		refcon:					SInt32;
		customName:				Str31;
	end;

	{   Get/SetInfo selectors }

const
	scSpatialSettingsType		= $7370746C (* 'sptl' *);						{  pointer to SCSpatialSettings struct }
	scTemporalSettingsType		= $7470726C (* 'tprl' *);						{  pointer to SCTemporalSettings struct }
	scDataRateSettingsType		= $64726174 (* 'drat' *);						{  pointer to SCDataRateSettings struct }
	scColorTableType			= $636C7574 (* 'clut' *);						{  pointer to CTabHandle }
	scProgressProcType			= $70726F67 (* 'prog' *);						{  pointer to ProgressRecord struct }
	scExtendedProcsType			= $78707263 (* 'xprc' *);						{  pointer to SCExtendedProcs struct }
	scPreferenceFlagsType		= $70726566 (* 'pref' *);						{  pointer to long }
	scSettingsStateType			= $73737461 (* 'ssta' *);						{  pointer to Handle }
	scSequenceIDType			= $73657175 (* 'sequ' *);						{  pointer to ImageSequence }
	scWindowPositionType		= $776E6477 (* 'wndw' *);						{  pointer to Point }
	scCodecFlagsType			= $63666C67 (* 'cflg' *);						{  pointer to CodecFlags }
	scCodecSettingsType			= $63646563 (* 'cdec' *);						{  pointer to Handle }
	scForceKeyValueType			= $6B73696D (* 'ksim' *);						{  pointer to long }
	scSoundSampleRateType		= $73737274 (* 'ssrt' *);						{  pointer to UnsignedFixed }
	scSoundSampleSizeType		= $73737373 (* 'ssss' *);						{  pointer to short }
	scSoundChannelCountType		= $73736363 (* 'sscc' *);						{  pointer to short }
	scSoundCompressionType		= $73736374 (* 'ssct' *);						{  pointer to OSType }
	scCompressionListType		= $6374796C (* 'ctyl' *);						{  pointer to OSType Handle }
	scCodecManufacturerType		= $636D6672 (* 'cmfr' *);						{  pointer to OSType }
	scSoundVBRCompressionOK		= $63766272 (* 'cvbr' *);						{  pointer to Boolean }
	scSoundInputSampleRateType	= $73736972 (* 'ssir' *);						{  pointer to UnsignedFixed }
	scSoundSampleRateChangeOK	= $72636F6B (* 'rcok' *);						{  pointer to Boolean }
	scAvailableCompressionListType = $61766169 (* 'avai' *);					{  pointer to OSType Handle }

	{   scTypeNotFoundErr returned by Get/SetInfo when type cannot be found. }


type
	SCParamsPtr = ^SCParams;
	SCParams = record
		flags:					SInt32;
		theCodecType:			CodecType;
		theCodec:				CodecComponent;
		spatialQuality:			CodecQ;
		temporalQuality:		CodecQ;
		depth:					SInt16;
		frameRate:				Fixed;
		keyFrameRate:			SInt32;
		reserved1:				SInt32;
		reserved2:				SInt32;
	end;


const
	scGetCompression			= 1;
	scShowMotionSettings		= $00000001;
	scSettingsChangedItem		= -1;

	scCompressFlagIgnoreIdenticalFrames = 1;

	{  QTAtomTypes for atoms found in settings atom containers }
	kQTSettingsVideo			= $76696465 (* 'vide' *);						{  Container for video/image compression related atoms (Get/SetInfo selectors) }
	kQTSettingsSound			= $736F756E (* 'soun' *);						{  Container for sound compression related atoms (Get/SetInfo selectors) }
	kQTSettingsComponentVersion	= $76657273 (* 'vers' *);						{  . Version of component that wrote settings (QTSettingsVersionAtomRecord) }

	{  Format of 'vers' atom found in settings atom containers }

type
	QTSettingsVersionAtomRecordPtr = ^QTSettingsVersionAtomRecord;
	QTSettingsVersionAtomRecord = record
		componentVersion:		SInt32;								{  standard compression component version }
		flags:					SInt16;								{  low bit is 1 if little endian platform, 0 if big endian platform }
		reserved:				SInt16;								{  should be 0 }
	end;

	{	* These are Progress procedures *	}
	{
	 *  SCGetCompressionExtended()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 *    Windows:          in qtmlClient.lib 3.0 and later
	 	}
function SCGetCompressionExtended(ci: ComponentInstance; var params: SCParams; where: Point; filterProc: SCModalFilterUPP; hookProc: SCModalHookUPP; refcon: SInt32; customName: StringPtr): ComponentResult; external name '_SCGetCompressionExtended';
{
 *  SCPositionRect()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SCPositionRect(ci: ComponentInstance; var rp: Rect; var where: Point): ComponentResult; external name '_SCPositionRect';
{
 *  SCPositionDialog()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SCPositionDialog(ci: ComponentInstance; id: SInt16; var where: Point): ComponentResult; external name '_SCPositionDialog';
{
 *  SCSetTestImagePictHandle()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SCSetTestImagePictHandle(ci: ComponentInstance; testPict: PicHandle; var testRect: Rect; testFlags: SInt16): ComponentResult; external name '_SCSetTestImagePictHandle';
{
 *  SCSetTestImagePictFile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SCSetTestImagePictFile(ci: ComponentInstance; testFileRef: SInt16; var testRect: Rect; testFlags: SInt16): ComponentResult; external name '_SCSetTestImagePictFile';
{
 *  SCSetTestImagePixMap()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SCSetTestImagePixMap(ci: ComponentInstance; testPixMap: PixMapHandle; var testRect: Rect; testFlags: SInt16): ComponentResult; external name '_SCSetTestImagePixMap';
{
 *  SCGetBestDeviceRect()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SCGetBestDeviceRect(ci: ComponentInstance; var r: Rect): ComponentResult; external name '_SCGetBestDeviceRect';
{
 *  SCRequestImageSettings()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SCRequestImageSettings(ci: ComponentInstance): ComponentResult; external name '_SCRequestImageSettings';
{
 *  SCCompressImage()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SCCompressImage(ci: ComponentInstance; src: PixMapHandle; const (*var*) srcRect: Rect; var desc: ImageDescriptionHandle; var data: Handle): ComponentResult; external name '_SCCompressImage';
{
 *  SCCompressPicture()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SCCompressPicture(ci: ComponentInstance; srcPicture: PicHandle; dstPicture: PicHandle): ComponentResult; external name '_SCCompressPicture';
{
 *  SCCompressPictureFile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SCCompressPictureFile(ci: ComponentInstance; srcRefNum: SInt16; dstRefNum: SInt16): ComponentResult; external name '_SCCompressPictureFile';
{
 *  SCRequestSequenceSettings()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SCRequestSequenceSettings(ci: ComponentInstance): ComponentResult; external name '_SCRequestSequenceSettings';
{
 *  SCCompressSequenceBegin()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SCCompressSequenceBegin(ci: ComponentInstance; src: PixMapHandle; const (*var*) srcRect: Rect; var desc: ImageDescriptionHandle): ComponentResult; external name '_SCCompressSequenceBegin';
{
 *  SCCompressSequenceFrame()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SCCompressSequenceFrame(ci: ComponentInstance; src: PixMapHandle; const (*var*) srcRect: Rect; var data: Handle; var dataSize: SInt32; var notSyncFlag: SInt16): ComponentResult; external name '_SCCompressSequenceFrame';
{
 *  SCCompressSequenceEnd()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SCCompressSequenceEnd(ci: ComponentInstance): ComponentResult; external name '_SCCompressSequenceEnd';
{
 *  SCDefaultPictHandleSettings()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SCDefaultPictHandleSettings(ci: ComponentInstance; srcPicture: PicHandle; motion: SInt16): ComponentResult; external name '_SCDefaultPictHandleSettings';
{
 *  SCDefaultPictFileSettings()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SCDefaultPictFileSettings(ci: ComponentInstance; srcRef: SInt16; motion: SInt16): ComponentResult; external name '_SCDefaultPictFileSettings';
{
 *  SCDefaultPixMapSettings()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SCDefaultPixMapSettings(ci: ComponentInstance; src: PixMapHandle; motion: SInt16): ComponentResult; external name '_SCDefaultPixMapSettings';
{
 *  SCGetInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SCGetInfo(ci: ComponentInstance; infoType: OSType; info: UnivPtr): ComponentResult; external name '_SCGetInfo';
{
 *  SCSetInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SCSetInfo(ci: ComponentInstance; infoType: OSType; info: UnivPtr): ComponentResult; external name '_SCSetInfo';
{
 *  SCNewGWorld()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SCNewGWorld(ci: ComponentInstance; var gwp: GWorldPtr; var rp: Rect; flags: GWorldFlags): ComponentResult; external name '_SCNewGWorld';
{
 *  SCSetCompressFlags()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SCSetCompressFlags(ci: ComponentInstance; flags: SInt32): ComponentResult; external name '_SCSetCompressFlags';
{
 *  SCGetCompressFlags()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SCGetCompressFlags(ci: ComponentInstance; var flags: SInt32): ComponentResult; external name '_SCGetCompressFlags';
{
 *  SCGetSettingsAsText()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SCGetSettingsAsText(ci: ComponentInstance; var text: Handle): ComponentResult; external name '_SCGetSettingsAsText';
{
 *  SCGetSettingsAsAtomContainer()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SCGetSettingsAsAtomContainer(ci: ComponentInstance; var settings: QTAtomContainer): ComponentResult; external name '_SCGetSettingsAsAtomContainer';
{
 *  SCSetSettingsFromAtomContainer()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SCSetSettingsFromAtomContainer(ci: ComponentInstance; settings: QTAtomContainer): ComponentResult; external name '_SCSetSettingsFromAtomContainer';
{ Note: if you're using SCCompressSequenceFrameAsync with a scForceKeyValue setting, you must call SCAsyncIdle occasionally at main task time. }
{
 *  SCCompressSequenceFrameAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 5.0 and later
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 5.0 and later
 }
function SCCompressSequenceFrameAsync(ci: ComponentInstance; src: PixMapHandle; const (*var*) srcRect: Rect; var data: Handle; var dataSize: SInt32; var notSyncFlag: SInt16; asyncCompletionProc: ICMCompletionProcRecordPtr): ComponentResult; external name '_SCCompressSequenceFrameAsync';
{
 *  SCAsyncIdle()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 5.0 and later
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 5.0 and later
 }
function SCAsyncIdle(ci: ComponentInstance): ComponentResult; external name '_SCAsyncIdle';
const
	TweenComponentType			= $7477656E (* 'twen' *);


type
	TweenerComponent					= ComponentInstance;
	{
	 *  TweenerInitialize()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 *    Windows:          in qtmlClient.lib 3.0 and later
	 	}
function TweenerInitialize(tc: TweenerComponent; container: QTAtomContainer; tweenAtom: QTAtom; dataAtom: QTAtom): ComponentResult; external name '_TweenerInitialize';
{
 *  TweenerDoTween()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function TweenerDoTween(tc: TweenerComponent; var tr: TweenRecord): ComponentResult; external name '_TweenerDoTween';
{
 *  TweenerReset()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function TweenerReset(tc: TweenerComponent): ComponentResult; external name '_TweenerReset';
const
	TCSourceRefNameType			= $6E616D65 (* 'name' *);

	tcDropFrame					= $01;
	tc24HourMax					= $02;
	tcNegTimesOK				= $04;
	tcCounter					= $08;


type
	TimeCodeDefPtr = ^TimeCodeDef;
	TimeCodeDef = record
		flags:					SInt32;								{  drop-frame, etc. }
		fTimeScale:				TimeScale;								{  time scale of frameDuration (eg. 2997) }
		frameDuration:			TimeValue;								{  duration of each frame (eg. 100) }
		numFrames:				SInt8;									{  frames/sec for timecode (eg. 30) OR frames/tick for counter mode }
		padding:				SInt8;									{  unused padding byte }
	end;
	TimeCodeDef_fix = TimeCodeDef; { used as field type when a record declaration contains a TimeCodeDef field identifier }


const
	tctNegFlag					= $80;							{  negative bit is in minutes }


type
	TimeCodeTimePtr = ^TimeCodeTime;
	TimeCodeTime = record
		hours:					SInt8;
		minutes:				SInt8;
		seconds:				SInt8;
		frames:					SInt8;
	end;

	TimeCodeCounterPtr = ^TimeCodeCounter;
	TimeCodeCounter = record
		counter:				SInt32;
	end;

	TimeCodeRecordPtr = ^TimeCodeRecord;
	TimeCodeRecord = record
		case SInt16 of
		0: (
			t:					TimeCodeTime;
			);
		1: (
			c:					TimeCodeCounter;
			);
	end;

	TimeCodeDescriptionPtr = ^TimeCodeDescription;
	TimeCodeDescription = record
		descSize:				SInt32;								{  standard sample description header }
		dataFormat:				SInt32;
		resvd1:					SInt32;
		resvd2:					SInt16;
		dataRefIndex:			SInt16;
		flags:					SInt32;								{  timecode specific stuff }
		timeCodeDef:			TimeCodeDef_fix;
		srcRef:					array [0..0] of SInt32;
	end;

	TimeCodeDescriptionHandle			= ^TimeCodeDescriptionPtr;

const
	tcdfShowTimeCode			= $01;


type
	TCTextOptionsPtr = ^TCTextOptions;
	TCTextOptions = record
		txFont:					SInt16;
		txFace:					SInt16;
		txSize:					SInt16;
		pad:					SInt16;								{  let's make it longword aligned - thanks..  }
		foreColor:				RGBColor;
		backColor:				RGBColor;
	end;

	{
	 *  TCGetCurrentTimeCode()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 *    Windows:          in qtmlClient.lib 3.0 and later
	 	}
function TCGetCurrentTimeCode(mh: MediaHandler; var frameNum: SInt32; var tcdef: TimeCodeDef; var tcrec: TimeCodeRecord; var srcRefH: UserData): HandlerError; external name '_TCGetCurrentTimeCode';
{
 *  TCGetTimeCodeAtTime()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function TCGetTimeCodeAtTime(mh: MediaHandler; mediaTime: TimeValue; var frameNum: SInt32; var tcdef: TimeCodeDef; var tcdata: TimeCodeRecord; var srcRefH: UserData): HandlerError; external name '_TCGetTimeCodeAtTime';
{
 *  TCTimeCodeToString()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function TCTimeCodeToString(mh: MediaHandler; var tcdef: TimeCodeDef; var tcrec: TimeCodeRecord; tcStr: StringPtr): HandlerError; external name '_TCTimeCodeToString';
{
 *  TCTimeCodeToFrameNumber()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function TCTimeCodeToFrameNumber(mh: MediaHandler; var tcdef: TimeCodeDef; var tcrec: TimeCodeRecord; var frameNumber: SInt32): HandlerError; external name '_TCTimeCodeToFrameNumber';
{
 *  TCFrameNumberToTimeCode()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function TCFrameNumberToTimeCode(mh: MediaHandler; frameNumber: SInt32; var tcdef: TimeCodeDef; var tcrec: TimeCodeRecord): HandlerError; external name '_TCFrameNumberToTimeCode';
{
 *  TCGetSourceRef()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function TCGetSourceRef(mh: MediaHandler; tcdH: TimeCodeDescriptionHandle; var srefH: UserData): HandlerError; external name '_TCGetSourceRef';
{
 *  TCSetSourceRef()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function TCSetSourceRef(mh: MediaHandler; tcdH: TimeCodeDescriptionHandle; srefH: UserData): HandlerError; external name '_TCSetSourceRef';
{
 *  TCSetTimeCodeFlags()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function TCSetTimeCodeFlags(mh: MediaHandler; flags: SInt32; flagsMask: SInt32): HandlerError; external name '_TCSetTimeCodeFlags';
{
 *  TCGetTimeCodeFlags()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function TCGetTimeCodeFlags(mh: MediaHandler; var flags: SInt32): HandlerError; external name '_TCGetTimeCodeFlags';
{
 *  TCSetDisplayOptions()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function TCSetDisplayOptions(mh: MediaHandler; textOptions: TCTextOptionsPtr): HandlerError; external name '_TCSetDisplayOptions';
{
 *  TCGetDisplayOptions()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function TCGetDisplayOptions(mh: MediaHandler; textOptions: TCTextOptionsPtr): HandlerError; external name '_TCGetDisplayOptions';
type
	MovieImportComponent				= ComponentInstance;
	MovieExportComponent				= ComponentInstance;

const
	MovieImportType				= $65617420 (* 'eat ' *);
	MovieExportType				= $73706974 (* 'spit' *);

	canMovieImportHandles		= $01;
	canMovieImportFiles			= $02;
	hasMovieImportUserInterface	= $04;
	canMovieExportHandles		= $08;
	canMovieExportFiles			= $10;
	hasMovieExportUserInterface	= $20;
	movieImporterIsXMLBased		= $20;
	dontAutoFileMovieImport		= $40;
	canMovieExportAuxDataHandle	= $80;
	canMovieImportValidateHandles = $0100;
	canMovieImportValidateFile	= $0200;
	dontRegisterWithEasyOpen	= $0400;
	canMovieImportInPlace		= $0800;
	movieImportSubTypeIsFileExtension = $1000;
	canMovieImportPartial		= $2000;
	hasMovieImportMIMEList		= $4000;
	canMovieImportAvoidBlocking	= $8000;
	canMovieExportFromProcedures = $8000;
	canMovieExportValidateMovie	= $00010000;
	movieImportMustGetDestinationMediaType = $00010000;
	movieExportNeedsResourceFork = $00020000;
	canMovieImportDataReferences = $00040000;
	movieExportMustGetSourceMediaType = $00080000;
	canMovieImportWithIdle		= $00100000;
	canMovieImportValidateDataReferences = $00200000;
	reservedForUseByGraphicsImporters = $00800000;

	movieImportCreateTrack		= 1;
	movieImportInParallel		= 2;
	movieImportMustUseTrack		= 4;
	movieImportWithIdle			= 16;

	movieImportResultUsedMultipleTracks = 8;
	movieImportResultNeedIdles	= 32;
	movieImportResultComplete	= 64;

	kMovieExportTextOnly		= 0;
	kMovieExportAbsoluteTime	= 1;
	kMovieExportRelativeTime	= 2;

	kMIDIImportSilenceBefore	= $01;
	kMIDIImportSilenceAfter		= $02;
	kMIDIImport20Playable		= $04;
	kMIDIImportWantLyrics		= $08;


	kQTMediaConfigResourceType	= $6D636667 (* 'mcfg' *);
	kQTMediaConfigResourceVersion = 2;
	kQTMediaGroupResourceType	= $6D677270 (* 'mgrp' *);
	kQTMediaGroupResourceVersion = 1;
	kQTBrowserInfoResourceType	= $62727773 (* 'brws' *);
	kQTBrowserInfoResourceVersion = 1;


	kQTMediaMIMEInfoHasChanged	= $00000002;					{  the MIME type(s) is(are) new or has changed since the last time }
																{   someone asked about it }
	kQTMediaFileInfoHasChanged	= $00000004;					{  the file extension(s) is(are) new or has changed since the last time }
																{   anyone asked about it }
	kQTMediaConfigCanUseApp		= $00040000;					{  this MIME type can be configured to use app }
	kQTMediaConfigCanUsePlugin	= $00080000;					{  this MIME type can be configured to use plug-in }
	kQTMediaConfigUNUSED		= $00100000;					{  currently unused }
	kQTMediaConfigBinaryFile	= $00800000;					{  file should be transfered in binary mode }
	kQTMediaConfigTextFile		= 0;							{  not a bit, defined for clarity }
	kQTMediaConfigMacintoshFile	= $01000000;					{  file's resource fork is significant }
	kQTMediaConfigAssociateByDefault = $08000000;				{  take this file association by default }
	kQTMediaConfigUseAppByDefault = $10000000;					{  use the app by default for this MIME type }
	kQTMediaConfigUsePluginByDefault = $20000000;				{  use the plug-in by default for this MIME type }
	kQTMediaConfigDefaultsMask	= $30000000;
	kQTMediaConfigDefaultsShift	= 12;							{  ((flags & kQTMediaConfigDefaultsMask) >> kQTMediaConfigDefaultsShift) to get default setting  }
	kQTMediaConfigHasFileHasQTAtoms = $40000000;				{  the file has a "QuickTime like" file format  }


	{  mime type group constants for groupID field of 'mcfg' resource }
	kQTMediaConfigStreamGroupID	= $7374726D (* 'strm' *);
	kQTMediaConfigInteractiveGroupID = $696E7472 (* 'intr' *);
	kQTMediaConfigVideoGroupID	= $65796573 (* 'eyes' *);
	kQTMediaConfigAudioGroupID	= $65617273 (* 'ears' *);
	kQTMediaConfigMPEGGroupID	= $6D706567 (* 'mpeg' *);
	kQTMediaConfigMP3GroupID	= $6D703320 (* 'mp3 ' *);
	kQTMediaConfigImageGroupID	= $6F676C65 (* 'ogle' *);
	kQTMediaConfigMiscGroupID	= $6D697363 (* 'misc' *);

	{  file type group constants for groupID field of 'mcfg' resource }
	kQTMediaInfoNetGroup		= $6E657420 (* 'net ' *);
	kQTMediaInfoWinGroup		= $77696E20 (* 'win ' *);
	kQTMediaInfoMacGroup		= $6D616320 (* 'mac ' *);
	kQTMediaInfoMiscGroup		= $3F3F3F3F;					{  '????' }


	kMimeInfoMimeTypeTag		= $6D696D65 (* 'mime' *);
	kMimeInfoFileExtensionTag	= $65787420 (* 'ext ' *);
	kMimeInfoDescriptionTag		= $64657363 (* 'desc' *);
	kMimeInfoGroupTag			= $67726F70 (* 'grop' *);
	kMimeInfoDoNotOverrideExistingFileTypeAssociation = $6E6F6661 (* 'nofa' *);

	kQTFileTypeAIFF				= $41494646 (* 'AIFF' *);
	kQTFileTypeAIFC				= $41494643 (* 'AIFC' *);
	kQTFileTypeDVC				= $64766321 (* 'dvc!' *);
	kQTFileTypeMIDI				= $4D696469 (* 'Midi' *);
	kQTFileTypePicture			= $50494354 (* 'PICT' *);
	kQTFileTypeMovie			= $4D6F6F56 (* 'MooV' *);
	kQTFileTypeText				= $54455854 (* 'TEXT' *);
	kQTFileTypeWave				= $57415645 (* 'WAVE' *);
	kQTFileTypeSystemSevenSound	= $7366696C (* 'sfil' *);
	kQTFileTypeMuLaw			= $554C4157 (* 'ULAW' *);
	kQTFileTypeAVI				= $56665720 (* 'VfW ' *);
	kQTFileTypeSoundDesignerII	= $53643266 (* 'Sd2f' *);
	kQTFileTypeAudioCDTrack		= $7472616B (* 'trak' *);
	kQTFileTypePICS				= $50494353 (* 'PICS' *);
	kQTFileTypeGIF				= $47494666 (* 'GIFf' *);
	kQTFileTypePNG				= $504E4766 (* 'PNGf' *);
	kQTFileTypeTIFF				= $54494646 (* 'TIFF' *);
	kQTFileTypePhotoShop		= $38425053 (* '8BPS' *);
	kQTFileTypeSGIImage			= $2E534749 (* '.SGI' *);
	kQTFileTypeBMP				= $424D5066 (* 'BMPf' *);
	kQTFileTypeJPEG				= $4A504547 (* 'JPEG' *);
	kQTFileTypeJFIF				= $4A504547 (* 'JPEG' *);
	kQTFileTypeMacPaint			= $504E5447 (* 'PNTG' *);
	kQTFileTypeTargaImage		= $54504943 (* 'TPIC' *);
	kQTFileTypeQuickDrawGXPicture = $71646778 (* 'qdgx' *);
	kQTFileTypeQuickTimeImage	= $71746966 (* 'qtif' *);
	kQTFileType3DMF				= $33444D46 (* '3DMF' *);
	kQTFileTypeFLC				= $464C4320 (* 'FLC ' *);
	kQTFileTypeFlash			= $5357464C (* 'SWFL' *);
	kQTFileTypeFlashPix			= $46506978 (* 'FPix' *);
	kQTFileTypeMP4				= $6D706734 (* 'mpg4' *);

	{  QTAtomTypes for atoms in import/export settings containers }
	kQTSettingsDVExportNTSC		= $64766376 (* 'dvcv' *);						{  True is export as NTSC, false is export as PAL. (Boolean) }
	kQTSettingsDVExportLockedAudio = $6C6F636B (* 'lock' *);					{  True if audio locked to video. (Boolean) }
	kQTSettingsEffect			= $65666665 (* 'effe' *);						{  Parent atom whose contents are atoms of an effects description }
	kQTSettingsGraphicsFileImportSequence = $73657175 (* 'sequ' *);				{  Parent atom of graphic file movie import component }
	kQTSettingsGraphicsFileImportSequenceEnabled = $656E6162 (* 'enab' *);		{  . If true, import numbered image sequence (Boolean) }
	kQTSettingsMovieExportEnableVideo = $656E7669 (* 'envi' *);					{  Enable exporting of video track (Boolean) }
	kQTSettingsMovieExportEnableSound = $656E736F (* 'enso' *);					{  Enable exporting of sound track (Boolean) }
	kQTSettingsMovieExportSaveOptions = $73617665 (* 'save' *);					{  Parent atom of save options }
	kQTSettingsMovieExportSaveForInternet = $66617374 (* 'fast' *);				{  . Save for Internet }
	kQTSettingsMovieExportSaveCompressedMovie = $636D706D (* 'cmpm' *);			{  . Save compressed movie resource }
	kQTSettingsMIDI				= $4D494449 (* 'MIDI' *);						{  MIDI import related container }
	kQTSettingsMIDISettingFlags	= $73747467 (* 'sttg' *);						{  . MIDI import settings (UInt32) }
	kQTSettingsText				= $74657874 (* 'text' *);						{  Text related container }
	kQTSettingsTextDescription	= $64657363 (* 'desc' *);						{  . Text import settings (TextDescription record) }
	kQTSettingsTextSize			= $73697A65 (* 'size' *);						{  . Width/height to create during import (FixedPoint) }
	kQTSettingsTextSettingFlags	= $73747467 (* 'sttg' *);						{  . Text export settings (UInt32) }
	kQTSettingsTextTimeFraction	= $74696D66 (* 'timf' *);						{  . Movie time fraction for export (UInt32) }
	kQTSettingsTime				= $74696D65 (* 'time' *);						{  Time related container }
	kQTSettingsTimeDuration		= $64757261 (* 'dura' *);						{  . Time related container }
	kQTSettingsAudioCDTrack		= $7472616B (* 'trak' *);						{  Audio CD track related container }
	kQTSettingsAudioCDTrackRateShift = $72736866 (* 'rshf' *);					{  . Rate shift to be performed (SInt16) }
	kQTSettingsDVExportDVFormat	= $64766366 (* 'dvcf' *);						{  Exported DV Format, DV('dv  ') or DVCPRO('dvp '). (OSType) }


type
	MovieExportGetDataParamsPtr = ^MovieExportGetDataParams;
	MovieExportGetDataParams = record
		recordSize:				SInt32;
		trackID:				SInt32;
		sourceTimeScale:		TimeScale;
		requestedTime:			TimeValue;
		actualTime:				TimeValue;
		dataPtr:				Ptr;
		dataSize:				SInt32;
		desc:					SampleDescriptionHandle;
		descType:				OSType;
		descSeed:				SInt32;
		requestedSampleCount:	SInt32;
		actualSampleCount:		SInt32;
		durationPerSample:		TimeValue;
		sampleFlags:			SInt32;
	end;

{$ifc TYPED_FUNCTION_POINTERS}
	MovieExportGetDataProcPtr = function(refCon: UnivPtr; var params: MovieExportGetDataParams): OSErr;
{$elsec}
	MovieExportGetDataProcPtr = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	MovieExportGetPropertyProcPtr = function(refcon: UnivPtr; trackID: SInt32; propertyType: OSType; propertyValue: UnivPtr): OSErr;
{$elsec}
	MovieExportGetPropertyProcPtr = ProcPtr;
{$endc}


const
	kQTPresetsListResourceType	= $73746723 (* 'stg#' *);
	kQTPresetsPlatformListResourceType = $73746770 (* 'stgp' *);

	kQTPresetInfoIsDivider		= 1;


type
	QTPresetInfoPtr = ^QTPresetInfo;
	QTPresetInfo = record
		presetKey:				OSType;									{  unique key for this preset in presetsArray  }
		presetFlags:			UInt32;									{  flags about this preset  }
		settingsResourceType:	OSType;									{  resource type of settings resource  }
		settingsResourceID:		SInt16;									{  resource id of settings resource  }
		padding1:				SInt16;
		nameStringListID:		SInt16;									{  name string list resource id  }
		nameStringIndex:		SInt16;									{  name string index  }
		infoStringListID:		SInt16;									{  info string list resource id  }
		infoStringIndex:		SInt16;									{  info string index  }
	end;

	QTPresetListRecordPtr = ^QTPresetListRecord;
	QTPresetListRecord = record
		flags:					UInt32;									{  flags for whole list  }
		count:					UInt32;									{  number of elements in presetsArray  }
		reserved:				UInt32;
		presetsArray:			array [0..0] of QTPresetInfo;			{  info about each preset  }
	end;


const
	kQTMovieExportSourceInfoResourceType = $73726323 (* 'src#' *);
	kQTMovieExportSourceInfoIsMediaType = $00000001;
	kQTMovieExportSourceInfoIsMediaCharacteristic = $00000002;
	kQTMovieExportSourceInfoIsSourceType = $00000004;


type
	QTMovieExportSourceInfoPtr = ^QTMovieExportSourceInfo;
	QTMovieExportSourceInfo = record
		mediaType:				OSType;									{  Media type of source  }
		minCount:				UInt16;									{  min number of sources of this kind required, zero if none required  }
		maxCount:				UInt16;									{  max number of sources of this kind allowed, -1 if unlimited allowed  }
		flags:					SInt32;								{  reserved for flags  }
	end;

	QTMovieExportSourceRecordPtr = ^QTMovieExportSourceRecord;
	QTMovieExportSourceRecord = record
		count:					SInt32;
		reserved:				SInt32;
		sourceArray:			array [0..0] of QTMovieExportSourceInfo;
	end;

{$ifc OPAQUE_UPP_TYPES}
	MovieExportGetDataUPP = ^SInt32; { an opaque UPP }
{$elsec}
	MovieExportGetDataUPP = UniversalProcPtr;
{$endc}	
{$ifc OPAQUE_UPP_TYPES}
	MovieExportGetPropertyUPP = ^SInt32; { an opaque UPP }
{$elsec}
	MovieExportGetPropertyUPP = UniversalProcPtr;
{$endc}	

const
	uppSCModalFilterProcInfo = $00003FD0;
	uppSCModalHookProcInfo = $00003EE0;
	uppMovieExportGetDataProcInfo = $000003E0;
	uppMovieExportGetPropertyProcInfo = $00003FE0;
	{
	 *  NewSCModalFilterUPP()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   available as macro/inline
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
function NewSCModalFilterUPP(userRoutine: SCModalFilterProcPtr): SCModalFilterUPP; external name '_NewSCModalFilterUPP'; { old name was NewSCModalFilterProc }
{
 *  NewSCModalHookUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewSCModalHookUPP(userRoutine: SCModalHookProcPtr): SCModalHookUPP; external name '_NewSCModalHookUPP'; { old name was NewSCModalHookProc }
{
 *  NewMovieExportGetDataUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewMovieExportGetDataUPP(userRoutine: MovieExportGetDataProcPtr): MovieExportGetDataUPP; external name '_NewMovieExportGetDataUPP'; { old name was NewMovieExportGetDataProc }
{
 *  NewMovieExportGetPropertyUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewMovieExportGetPropertyUPP(userRoutine: MovieExportGetPropertyProcPtr): MovieExportGetPropertyUPP; external name '_NewMovieExportGetPropertyUPP'; { old name was NewMovieExportGetPropertyProc }
{
 *  DisposeSCModalFilterUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeSCModalFilterUPP(userUPP: SCModalFilterUPP); external name '_DisposeSCModalFilterUPP';
{
 *  DisposeSCModalHookUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeSCModalHookUPP(userUPP: SCModalHookUPP); external name '_DisposeSCModalHookUPP';
{
 *  DisposeMovieExportGetDataUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeMovieExportGetDataUPP(userUPP: MovieExportGetDataUPP); external name '_DisposeMovieExportGetDataUPP';
{
 *  DisposeMovieExportGetPropertyUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeMovieExportGetPropertyUPP(userUPP: MovieExportGetPropertyUPP); external name '_DisposeMovieExportGetPropertyUPP';
{
 *  InvokeSCModalFilterUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function InvokeSCModalFilterUPP(theDialog: DialogRef; var theEvent: EventRecord; var itemHit: SInt16; refcon: SInt32; userRoutine: SCModalFilterUPP): boolean; external name '_InvokeSCModalFilterUPP'; { old name was CallSCModalFilterProc }
{
 *  InvokeSCModalHookUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function InvokeSCModalHookUPP(theDialog: DialogRef; itemHit: SInt16; params: UnivPtr; refcon: SInt32; userRoutine: SCModalHookUPP): SInt16; external name '_InvokeSCModalHookUPP'; { old name was CallSCModalHookProc }
{
 *  InvokeMovieExportGetDataUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function InvokeMovieExportGetDataUPP(refCon: UnivPtr; var params: MovieExportGetDataParams; userRoutine: MovieExportGetDataUPP): OSErr; external name '_InvokeMovieExportGetDataUPP'; { old name was CallMovieExportGetDataProc }
{
 *  InvokeMovieExportGetPropertyUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function InvokeMovieExportGetPropertyUPP(refcon: UnivPtr; trackID: SInt32; propertyType: OSType; propertyValue: UnivPtr; userRoutine: MovieExportGetPropertyUPP): OSErr; external name '_InvokeMovieExportGetPropertyUPP'; { old name was CallMovieExportGetPropertyProc }
{
 *  MovieImportHandle()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MovieImportHandle(ci: MovieImportComponent; dataH: Handle; theMovie: Movie; targetTrack: Track; var usedTrack: Track; atTime: TimeValue; var addedDuration: TimeValue; inFlags: SInt32; var outFlags: SInt32): ComponentResult; external name '_MovieImportHandle';
{
 *  MovieImportFile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MovieImportFile(ci: MovieImportComponent; const (*var*) theFile: FSSpec; theMovie: Movie; targetTrack: Track; var usedTrack: Track; atTime: TimeValue; var addedDuration: TimeValue; inFlags: SInt32; var outFlags: SInt32): ComponentResult; external name '_MovieImportFile';
{
 *  MovieImportSetSampleDuration()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MovieImportSetSampleDuration(ci: MovieImportComponent; duration: TimeValue; scale: TimeScale): ComponentResult; external name '_MovieImportSetSampleDuration';
{
 *  MovieImportSetSampleDescription()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MovieImportSetSampleDescription(ci: MovieImportComponent; desc: SampleDescriptionHandle; mediaType: OSType): ComponentResult; external name '_MovieImportSetSampleDescription';
{
 *  MovieImportSetMediaFile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MovieImportSetMediaFile(ci: MovieImportComponent; alias: AliasHandle): ComponentResult; external name '_MovieImportSetMediaFile';
{
 *  MovieImportSetDimensions()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MovieImportSetDimensions(ci: MovieImportComponent; width: Fixed; height: Fixed): ComponentResult; external name '_MovieImportSetDimensions';
{
 *  MovieImportSetChunkSize()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MovieImportSetChunkSize(ci: MovieImportComponent; chunkSize: SInt32): ComponentResult; external name '_MovieImportSetChunkSize';
{
 *  MovieImportSetProgressProc()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MovieImportSetProgressProc(ci: MovieImportComponent; proc: MovieProgressUPP; refcon: SInt32): ComponentResult; external name '_MovieImportSetProgressProc';
{
 *  MovieImportSetAuxiliaryData()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MovieImportSetAuxiliaryData(ci: MovieImportComponent; data: Handle; handleType: OSType): ComponentResult; external name '_MovieImportSetAuxiliaryData';
{
 *  MovieImportSetFromScrap()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MovieImportSetFromScrap(ci: MovieImportComponent; fromScrap: boolean): ComponentResult; external name '_MovieImportSetFromScrap';
{
 *  MovieImportDoUserDialog()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MovieImportDoUserDialog(ci: MovieImportComponent; const (*var*) theFile: FSSpec; theData: Handle; var canceled: boolean): ComponentResult; external name '_MovieImportDoUserDialog';
{
 *  MovieImportSetDuration()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MovieImportSetDuration(ci: MovieImportComponent; duration: TimeValue): ComponentResult; external name '_MovieImportSetDuration';
{
 *  MovieImportGetAuxiliaryDataType()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MovieImportGetAuxiliaryDataType(ci: MovieImportComponent; var auxType: OSType): ComponentResult; external name '_MovieImportGetAuxiliaryDataType';
{
 *  MovieImportValidate()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MovieImportValidate(ci: MovieImportComponent; const (*var*) theFile: FSSpec; theData: Handle; var valid: boolean): ComponentResult; external name '_MovieImportValidate';
{
 *  MovieImportGetFileType()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MovieImportGetFileType(ci: MovieImportComponent; var fileType: OSType): ComponentResult; external name '_MovieImportGetFileType';
{
 *  MovieImportDataRef()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MovieImportDataRef(ci: MovieImportComponent; dataRef: Handle; dataRefType: OSType; theMovie: Movie; targetTrack: Track; var usedTrack: Track; atTime: TimeValue; var addedDuration: TimeValue; inFlags: SInt32; var outFlags: SInt32): ComponentResult; external name '_MovieImportDataRef';
{
 *  MovieImportGetSampleDescription()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MovieImportGetSampleDescription(ci: MovieImportComponent; var desc: SampleDescriptionHandle; var mediaType: OSType): ComponentResult; external name '_MovieImportGetSampleDescription';
{
 *  MovieImportGetMIMETypeList()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MovieImportGetMIMETypeList(ci: MovieImportComponent; var mimeInfo: QTAtomContainer): ComponentResult; external name '_MovieImportGetMIMETypeList';
{
 *  MovieImportSetOffsetAndLimit()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MovieImportSetOffsetAndLimit(ci: MovieImportComponent; offset: UInt32; limit: UInt32): ComponentResult; external name '_MovieImportSetOffsetAndLimit';
{
 *  MovieImportGetSettingsAsAtomContainer()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MovieImportGetSettingsAsAtomContainer(ci: MovieImportComponent; var settings: QTAtomContainer): ComponentResult; external name '_MovieImportGetSettingsAsAtomContainer';
{
 *  MovieImportSetSettingsFromAtomContainer()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MovieImportSetSettingsFromAtomContainer(ci: MovieImportComponent; settings: QTAtomContainer): ComponentResult; external name '_MovieImportSetSettingsFromAtomContainer';
{
 *  MovieImportSetOffsetAndLimit64()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function MovieImportSetOffsetAndLimit64(ci: MovieImportComponent; const (*var*) offset: wide; const (*var*) limit: wide): ComponentResult; external name '_MovieImportSetOffsetAndLimit64';
{
 *  MovieImportIdle()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function MovieImportIdle(ci: MovieImportComponent; inFlags: SInt32; var outFlags: SInt32): ComponentResult; external name '_MovieImportIdle';
{
 *  MovieImportValidateDataRef()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function MovieImportValidateDataRef(ci: MovieImportComponent; dataRef: Handle; dataRefType: OSType; var valid: UInt8): ComponentResult; external name '_MovieImportValidateDataRef';
{
 *  MovieImportGetLoadState()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.1 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.1 and later
 }
function MovieImportGetLoadState(ci: MovieImportComponent; var importerLoadState: SInt32): ComponentResult; external name '_MovieImportGetLoadState';
{
 *  MovieImportGetMaxLoadedTime()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.1 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.1 and later
 }
function MovieImportGetMaxLoadedTime(ci: MovieImportComponent; var time: TimeValue): ComponentResult; external name '_MovieImportGetMaxLoadedTime';
{
 *  MovieImportEstimateCompletionTime()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 5.0 and later
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 5.0 and later
 }
function MovieImportEstimateCompletionTime(ci: MovieImportComponent; var time: TimeRecord): ComponentResult; external name '_MovieImportEstimateCompletionTime';
{
 *  MovieImportSetDontBlock()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 5.0 and later
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 5.0 and later
 }
function MovieImportSetDontBlock(ci: MovieImportComponent; dontBlock: boolean): ComponentResult; external name '_MovieImportSetDontBlock';
{
 *  MovieImportGetDontBlock()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 5.0 and later
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 5.0 and later
 }
function MovieImportGetDontBlock(ci: MovieImportComponent; var willBlock: boolean): ComponentResult; external name '_MovieImportGetDontBlock';
{
 *  MovieImportSetIdleManager()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 6.0 and later
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.2 and later
 *    Windows:          in qtmlClient.lib 6.0 and later
 }
function MovieImportSetIdleManager(ci: MovieImportComponent; im: IdleManager): ComponentResult; external name '_MovieImportSetIdleManager';
{
 *  MovieImportSetNewMovieFlags()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 6.0 and later
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.2 and later
 *    Windows:          in qtmlClient.lib 6.0 and later
 }
function MovieImportSetNewMovieFlags(ci: MovieImportComponent; newMovieFlags: SInt32): ComponentResult; external name '_MovieImportSetNewMovieFlags';
{
 *  MovieImportGetDestinationMediaType()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 6.0 and later
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.2 and later
 *    Windows:          in qtmlClient.lib 6.0 and later
 }
function MovieImportGetDestinationMediaType(ci: MovieImportComponent; var mediaType: OSType): ComponentResult; external name '_MovieImportGetDestinationMediaType';
{
 *  MovieExportToHandle()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MovieExportToHandle(ci: MovieExportComponent; dataH: Handle; theMovie: Movie; onlyThisTrack: Track; startTime: TimeValue; duration: TimeValue): ComponentResult; external name '_MovieExportToHandle';
{
 *  MovieExportToFile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MovieExportToFile(ci: MovieExportComponent; const (*var*) theFile: FSSpec; theMovie: Movie; onlyThisTrack: Track; startTime: TimeValue; duration: TimeValue): ComponentResult; external name '_MovieExportToFile';
{
 *  MovieExportGetAuxiliaryData()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MovieExportGetAuxiliaryData(ci: MovieExportComponent; dataH: Handle; var handleType: OSType): ComponentResult; external name '_MovieExportGetAuxiliaryData';
{
 *  MovieExportSetProgressProc()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MovieExportSetProgressProc(ci: MovieExportComponent; proc: MovieProgressUPP; refcon: SInt32): ComponentResult; external name '_MovieExportSetProgressProc';
{
 *  MovieExportSetSampleDescription()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MovieExportSetSampleDescription(ci: MovieExportComponent; desc: SampleDescriptionHandle; mediaType: OSType): ComponentResult; external name '_MovieExportSetSampleDescription';
{
 *  MovieExportDoUserDialog()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MovieExportDoUserDialog(ci: MovieExportComponent; theMovie: Movie; onlyThisTrack: Track; startTime: TimeValue; duration: TimeValue; var canceled: boolean): ComponentResult; external name '_MovieExportDoUserDialog';
{
 *  MovieExportGetCreatorType()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MovieExportGetCreatorType(ci: MovieExportComponent; var creator: OSType): ComponentResult; external name '_MovieExportGetCreatorType';
{
 *  MovieExportToDataRef()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MovieExportToDataRef(ci: MovieExportComponent; dataRef: Handle; dataRefType: OSType; theMovie: Movie; onlyThisTrack: Track; startTime: TimeValue; duration: TimeValue): ComponentResult; external name '_MovieExportToDataRef';
{
 *  MovieExportFromProceduresToDataRef()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MovieExportFromProceduresToDataRef(ci: MovieExportComponent; dataRef: Handle; dataRefType: OSType): ComponentResult; external name '_MovieExportFromProceduresToDataRef';
{
 *  MovieExportAddDataSource()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MovieExportAddDataSource(ci: MovieExportComponent; trackType: OSType; scale: TimeScale; var trackID: SInt32; getPropertyProc: MovieExportGetPropertyUPP; getDataProc: MovieExportGetDataUPP; refCon: UnivPtr): ComponentResult; external name '_MovieExportAddDataSource';
{
 *  MovieExportValidate()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MovieExportValidate(ci: MovieExportComponent; theMovie: Movie; onlyThisTrack: Track; var valid: boolean): ComponentResult; external name '_MovieExportValidate';
{
 *  MovieExportGetSettingsAsAtomContainer()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MovieExportGetSettingsAsAtomContainer(ci: MovieExportComponent; var settings: QTAtomContainer): ComponentResult; external name '_MovieExportGetSettingsAsAtomContainer';
{
 *  MovieExportSetSettingsFromAtomContainer()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MovieExportSetSettingsFromAtomContainer(ci: MovieExportComponent; settings: QTAtomContainer): ComponentResult; external name '_MovieExportSetSettingsFromAtomContainer';
{
 *  MovieExportGetFileNameExtension()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MovieExportGetFileNameExtension(ci: MovieExportComponent; var extension: OSType): ComponentResult; external name '_MovieExportGetFileNameExtension';
{
 *  MovieExportGetShortFileTypeString()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MovieExportGetShortFileTypeString(ci: MovieExportComponent; var typeString: Str255): ComponentResult; external name '_MovieExportGetShortFileTypeString';
{
 *  MovieExportGetSourceMediaType()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MovieExportGetSourceMediaType(ci: MovieExportComponent; var mediaType: OSType): ComponentResult; external name '_MovieExportGetSourceMediaType';
{
 *  MovieExportSetGetMoviePropertyProc()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function MovieExportSetGetMoviePropertyProc(ci: MovieExportComponent; getPropertyProc: MovieExportGetPropertyUPP; refCon: UnivPtr): ComponentResult; external name '_MovieExportSetGetMoviePropertyProc';
{  Text Export Display Info data structure }

type
	TextDisplayDataPtr = ^TextDisplayData;
	TextDisplayData = record
		displayFlags:			SInt32;
		textJustification:		SInt32;
		bgColor:				RGBColor;
		textBox:				Rect;
		beginHilite:			SInt16;
		endHilite:				SInt16;
		hiliteColor:			RGBColor;
		doHiliteColor:			boolean;
		filler:					SInt8;
		scrollDelayDur:			TimeValue;
		dropShadowOffset:		Point;
		dropShadowTransparency:	SInt16;
	end;

	TextExportComponent					= ComponentInstance;
	GraphicImageMovieImportComponent	= ComponentInstance;
	{
	 *  TextExportGetDisplayData()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 *    Windows:          in qtmlClient.lib 3.0 and later
	 	}
function TextExportGetDisplayData(ci: TextExportComponent; var textDisplay: TextDisplayData): ComponentResult; external name '_TextExportGetDisplayData';
{
 *  TextExportGetTimeFraction()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function TextExportGetTimeFraction(ci: TextExportComponent; var movieTimeFraction: SInt32): ComponentResult; external name '_TextExportGetTimeFraction';
{
 *  TextExportSetTimeFraction()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function TextExportSetTimeFraction(ci: TextExportComponent; movieTimeFraction: SInt32): ComponentResult; external name '_TextExportSetTimeFraction';
{
 *  TextExportGetSettings()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function TextExportGetSettings(ci: TextExportComponent; var setting: SInt32): ComponentResult; external name '_TextExportGetSettings';
{
 *  TextExportSetSettings()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function TextExportSetSettings(ci: TextExportComponent; setting: SInt32): ComponentResult; external name '_TextExportSetSettings';
{
 *  MIDIImportGetSettings()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MIDIImportGetSettings(ci: TextExportComponent; var setting: SInt32): ComponentResult; external name '_MIDIImportGetSettings';
{
 *  MIDIImportSetSettings()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MIDIImportSetSettings(ci: TextExportComponent; setting: SInt32): ComponentResult; external name '_MIDIImportSetSettings';
{
 *  MovieExportNewGetDataAndPropertiesProcs()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MovieExportNewGetDataAndPropertiesProcs(ci: MovieExportComponent; trackType: OSType; var scale: TimeScale; theMovie: Movie; theTrack: Track; startTime: TimeValue; duration: TimeValue; var getPropertyProc: MovieExportGetPropertyUPP; var getDataProc: MovieExportGetDataUPP; var refCon: UnivPtr): ComponentResult; external name '_MovieExportNewGetDataAndPropertiesProcs';
{
 *  MovieExportDisposeGetDataAndPropertiesProcs()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MovieExportDisposeGetDataAndPropertiesProcs(ci: MovieExportComponent; getPropertyProc: MovieExportGetPropertyUPP; getDataProc: MovieExportGetDataUPP; refCon: UnivPtr): ComponentResult; external name '_MovieExportDisposeGetDataAndPropertiesProcs';
const
	movieExportUseConfiguredSettings = $75636667 (* 'ucfg' *);					{  pointer to Boolean }
	movieExportWidth			= $77647468 (* 'wdth' *);						{  pointer to Fixed }
	movieExportHeight			= $68656774 (* 'hegt' *);						{  pointer to Fixed }
	movieExportDuration			= $64757261 (* 'dura' *);						{  pointer to TimeRecord }
	movieExportVideoFilter		= $69666C74 (* 'iflt' *);						{  pointer to QTAtomContainer }
	movieExportTimeScale		= $746D7363 (* 'tmsc' *);						{  pointer to TimeScale }

	{
	 *  GraphicsImageImportSetSequenceEnabled()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 *    Windows:          in qtmlClient.lib 3.0 and later
	 	}
function GraphicsImageImportSetSequenceEnabled(ci: GraphicImageMovieImportComponent; enable: boolean): ComponentResult; external name '_GraphicsImageImportSetSequenceEnabled';
{
 *  GraphicsImageImportGetSequenceEnabled()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GraphicsImageImportGetSequenceEnabled(ci: GraphicImageMovieImportComponent; var enable: boolean): ComponentResult; external name '_GraphicsImageImportGetSequenceEnabled';
{----------------------------'brws' • browser prefs configuration info ------------------------}

const
	kQTBrowserInfoCanUseSystemFolderPlugin = $00000001;			{  Mac browser can use plug-in from System "Internet Plug-ins" folder  }


	kQTPreFlightOpenComponent	= $00000002;					{  Open component as preflight check }


type
	ComponentPreflightFlagsPtr = ^ComponentPreflightFlags;
	ComponentPreflightFlags = record
		flags:					SInt32;
	end;


	{	**************
	
	    File Preview Components
	
	**************	}
	pnotComponent						= ComponentInstance;

const
	pnotComponentWantsEvents	= 1;
	pnotComponentNeedsNoCache	= 2;

	ShowFilePreviewComponentType = $706E6F74 (* 'pnot' *);
	CreateFilePreviewComponentType = $706D616B (* 'pmak' *);

	{
	 *  PreviewShowData()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 *    Windows:          in qtmlClient.lib 3.0 and later
	 	}
function PreviewShowData(p: pnotComponent; dataType: OSType; data: Handle; const (*var*) inHere: Rect): ComponentResult; external name '_PreviewShowData';
{
 *  PreviewMakePreview()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function PreviewMakePreview(p: pnotComponent; var previewType: OSType; var previewResult: Handle; const (*var*) sourceFile: FSSpec; progress: ICMProgressProcRecordPtr): ComponentResult; external name '_PreviewMakePreview';
{
 *  PreviewMakePreviewReference()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function PreviewMakePreviewReference(p: pnotComponent; var previewType: OSType; var resID: SInt16; const (*var*) sourceFile: FSSpec): ComponentResult; external name '_PreviewMakePreviewReference';
{
 *  PreviewEvent()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function PreviewEvent(p: pnotComponent; var e: EventRecord; var handledEvent: boolean): ComponentResult; external name '_PreviewEvent';
type
	DataCompressorComponent				= ComponentInstance;
	DataDecompressorComponent			= ComponentInstance;
	DataCodecComponent					= ComponentInstance;

const
	DataCompressorComponentType	= $64636F6D (* 'dcom' *);
	DataDecompressorComponentType = $64646563 (* 'ddec' *);
	AppleDataCompressorSubType	= $61646563 (* 'adec' *);
	zlibDataCompressorSubType	= $7A6C6962 (* 'zlib' *);


	{	* These are DataCodec procedures *	}
	{
	 *  DataCodecDecompress()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 *    Windows:          in qtmlClient.lib 3.0 and later
	 	}
function DataCodecDecompress(dc: DataCodecComponent; srcData: UnivPtr; srcSize: UInt32; dstData: UnivPtr; dstBufferSize: UInt32): ComponentResult; external name '_DataCodecDecompress';
{
 *  DataCodecGetCompressBufferSize()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function DataCodecGetCompressBufferSize(dc: DataCodecComponent; srcSize: UInt32; var dstSize: UInt32): ComponentResult; external name '_DataCodecGetCompressBufferSize';
{
 *  DataCodecCompress()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function DataCodecCompress(dc: DataCodecComponent; srcData: UnivPtr; srcSize: UInt32; dstData: UnivPtr; dstBufferSize: UInt32; var actualDstSize: UInt32; var decompressSlop: UInt32): ComponentResult; external name '_DataCodecCompress';
{
 *  DataCodecBeginInterruptSafe()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function DataCodecBeginInterruptSafe(dc: DataCodecComponent; maxSrcSize: UInt32): ComponentResult; external name '_DataCodecBeginInterruptSafe';
{
 *  DataCodecEndInterruptSafe()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function DataCodecEndInterruptSafe(dc: DataCodecComponent): ComponentResult; external name '_DataCodecEndInterruptSafe';
{
 *  DataCodecDecompressPartial()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function DataCodecDecompressPartial(dc: DataCodecComponent; var next_in: UnivPtr; var avail_in: UInt32; var total_in: UInt32; var next_out: UnivPtr; var avail_out: UInt32; var total_out: UInt32; var didFinish: boolean): ComponentResult; external name '_DataCodecDecompressPartial';
{
 *  DataCodecCompressPartial()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function DataCodecCompressPartial(dc: DataCodecComponent; var next_in: UnivPtr; var avail_in: UInt32; var total_in: UInt32; var next_out: UnivPtr; var avail_out: UInt32; var total_out: UInt32; tryToFinish: boolean; var didFinish: boolean): ComponentResult; external name '_DataCodecCompressPartial';
type
{$ifc TYPED_FUNCTION_POINTERS}
	DataHCompletionProcPtr = procedure(request: Ptr; refcon: SInt32; err: OSErr);
{$elsec}
	DataHCompletionProcPtr = ProcPtr;
{$endc}

{$ifc OPAQUE_UPP_TYPES}
	DataHCompletionUPP = ^SInt32; { an opaque UPP }
{$elsec}
	DataHCompletionUPP = UniversalProcPtr;
{$endc}	


const
	kDataHCanRead				= $00000001;
	kDataHSpecialRead			= $00000002;
	kDataHSpecialReadFile		= $00000004;
	kDataHCanWrite				= $00000008;
	kDataHSpecialWrite			= $10;
	kDataHSpecialWriteFile		= $20;
	kDataHCanStreamingWrite		= $40;
	kDataHMustCheckDataRef		= $80;

	{  Data reference records for specific data ref types }

type
	HandleDataRefRecordPtr = ^HandleDataRefRecord;
	HandleDataRefRecord = record
		dataHndl:				Handle;
	end;

	HandleDataRefPtr					= ^HandleDataRefRecord;
	HandleDataRef						= ^HandleDataRefPtr;
	PointerDataRefRecordPtr = ^PointerDataRefRecord;
	PointerDataRefRecord = record
		data:					Ptr;
		dataLength:				Size;
	end;

	PointerDataRefPtr					= ^PointerDataRefRecord;
	PointerDataRef						= ^PointerDataRefPtr;
	{  Data reference extensions }

const
	kDataRefExtensionChokeSpeed	= $63686F6B (* 'chok' *);
	kDataRefExtensionFileName	= $666E616D (* 'fnam' *);
	kDataRefExtensionMIMEType	= $6D696D65 (* 'mime' *);
	kDataRefExtensionMacOSFileType = $66747970 (* 'ftyp' *);
	kDataRefExtensionInitializationData = $64617461 (* 'data' *);
	kDataRefExtensionQuickTimeMediaType = $6D747970 (* 'mtyp' *);

	kDataHChokeToMovieDataRate	= $01;							{  param is 0 }
	kDataHChokeToParam			= $02;							{  param is bytes per second }


type
	DataHChokeAtomRecordPtr = ^DataHChokeAtomRecord;
	DataHChokeAtomRecord = record
		flags:					SInt32;								{  one of kDataHChokeTo constants }
		param:					SInt32;
	end;


	DataHVolumeListRecordPtr = ^DataHVolumeListRecord;
	DataHVolumeListRecord = record
		vRefNum:				SInt16;
		flags:					SInt32;
	end;

	DataHVolumeListPtr					= ^DataHVolumeListRecord;
	DataHVolumeList						= ^DataHVolumeListPtr;

const
	kDataHExtendedSchedule		= $78746E64 (* 'xtnd' *);


type
	DataHScheduleRecordPtr = ^DataHScheduleRecord;
	DataHScheduleRecord = record
		timeNeededBy:			TimeRecord;
		extendedID:				SInt32;								{  always is kDataHExtendedSchedule }
		extendedVers:			SInt32;								{  always set to 0 }
		priority:				Fixed;									{  100.0 or more means must have. lower numbers… }
	end;

	DataHSchedulePtr					= ^DataHScheduleRecord;
	{  Flags for DataHGetInfoFlags }

const
	kDataHInfoFlagNeverStreams	= $01;							{  set if this data handler doesn't stream }
	kDataHInfoFlagCanUpdateDataRefs = $02;						{  set if this data handler might update data reference }
	kDataHInfoFlagNeedsNetworkBandwidth = $04;					{  set if this data handler may need to occupy the network }


	{  Types for DataHGetFileTypeOrdering }
	kDataHFileTypeMacOSFileType	= $66747970 (* 'ftyp' *);
	kDataHFileTypeExtension		= $66657874 (* 'fext' *);
	kDataHFileTypeMIME			= $6D696D65 (* 'mime' *);


type
	DataHFileTypeOrderingPtr			= ^OSType;
	DataHFileTypeOrderingHandle			= ^DataHFileTypeOrderingPtr;

	{
	 *  DataHGetData()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 *    Windows:          in qtmlClient.lib 3.0 and later
	 	}
function DataHGetData(dh: DataHandler; h: Handle; hOffset: SInt32; offset: SInt32; size: SInt32): ComponentResult; external name '_DataHGetData';
{
 *  DataHPutData()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function DataHPutData(dh: DataHandler; h: Handle; hOffset: SInt32; var offset: SInt32; size: SInt32): ComponentResult; external name '_DataHPutData';
{
 *  DataHFlushData()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function DataHFlushData(dh: DataHandler): ComponentResult; external name '_DataHFlushData';
{
 *  DataHOpenForWrite()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function DataHOpenForWrite(dh: DataHandler): ComponentResult; external name '_DataHOpenForWrite';
{
 *  DataHCloseForWrite()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function DataHCloseForWrite(dh: DataHandler): ComponentResult; external name '_DataHCloseForWrite';
{
 *  DataHOpenForRead()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function DataHOpenForRead(dh: DataHandler): ComponentResult; external name '_DataHOpenForRead';
{
 *  DataHCloseForRead()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function DataHCloseForRead(dh: DataHandler): ComponentResult; external name '_DataHCloseForRead';
{
 *  DataHSetDataRef()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function DataHSetDataRef(dh: DataHandler; dataRef: Handle): ComponentResult; external name '_DataHSetDataRef';
{
 *  DataHGetDataRef()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function DataHGetDataRef(dh: DataHandler; var dataRef: Handle): ComponentResult; external name '_DataHGetDataRef';
{
 *  DataHCompareDataRef()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function DataHCompareDataRef(dh: DataHandler; dataRef: Handle; var equal: boolean): ComponentResult; external name '_DataHCompareDataRef';
{
 *  DataHTask()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function DataHTask(dh: DataHandler): ComponentResult; external name '_DataHTask';
{
 *  DataHScheduleData()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function DataHScheduleData(dh: DataHandler; PlaceToPutDataPtr: Ptr; FileOffset: SInt32; DataSize: SInt32; RefCon: SInt32; scheduleRec: DataHSchedulePtr; CompletionRtn: DataHCompletionUPP): ComponentResult; external name '_DataHScheduleData';
{
 *  DataHFinishData()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function DataHFinishData(dh: DataHandler; PlaceToPutDataPtr: Ptr; Cancel: boolean): ComponentResult; external name '_DataHFinishData';
{
 *  DataHFlushCache()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function DataHFlushCache(dh: DataHandler): ComponentResult; external name '_DataHFlushCache';
{
 *  DataHResolveDataRef()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function DataHResolveDataRef(dh: DataHandler; theDataRef: Handle; var wasChanged: boolean; userInterfaceAllowed: boolean): ComponentResult; external name '_DataHResolveDataRef';
{
 *  DataHGetFileSize()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function DataHGetFileSize(dh: DataHandler; var fileSize: SInt32): ComponentResult; external name '_DataHGetFileSize';
{
 *  DataHCanUseDataRef()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function DataHCanUseDataRef(dh: DataHandler; dataRef: Handle; var useFlags: SInt32): ComponentResult; external name '_DataHCanUseDataRef';
{
 *  DataHGetVolumeList()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function DataHGetVolumeList(dh: DataHandler; var volumeList: DataHVolumeList): ComponentResult; external name '_DataHGetVolumeList';
{
 *  DataHWrite()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function DataHWrite(dh: DataHandler; data: Ptr; offset: SInt32; size: SInt32; completion: DataHCompletionUPP; refCon: SInt32): ComponentResult; external name '_DataHWrite';
{
 *  DataHPreextend()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function DataHPreextend(dh: DataHandler; maxToAdd: UInt32; var spaceAdded: UInt32): ComponentResult; external name '_DataHPreextend';
{
 *  DataHSetFileSize()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function DataHSetFileSize(dh: DataHandler; fileSize: SInt32): ComponentResult; external name '_DataHSetFileSize';
{
 *  DataHGetFreeSpace()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function DataHGetFreeSpace(dh: DataHandler; var freeSize: UInt32): ComponentResult; external name '_DataHGetFreeSpace';
{
 *  DataHCreateFile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function DataHCreateFile(dh: DataHandler; creator: OSType; deleteExisting: boolean): ComponentResult; external name '_DataHCreateFile';
{
 *  DataHGetPreferredBlockSize()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function DataHGetPreferredBlockSize(dh: DataHandler; var blockSize: SInt32): ComponentResult; external name '_DataHGetPreferredBlockSize';
{
 *  DataHGetDeviceIndex()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function DataHGetDeviceIndex(dh: DataHandler; var deviceIndex: SInt32): ComponentResult; external name '_DataHGetDeviceIndex';
{
 *  DataHIsStreamingDataHandler()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function DataHIsStreamingDataHandler(dh: DataHandler; var yes: boolean): ComponentResult; external name '_DataHIsStreamingDataHandler';
{
 *  DataHGetDataInBuffer()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function DataHGetDataInBuffer(dh: DataHandler; startOffset: SInt32; var size: SInt32): ComponentResult; external name '_DataHGetDataInBuffer';
{
 *  DataHGetScheduleAheadTime()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function DataHGetScheduleAheadTime(dh: DataHandler; var millisecs: SInt32): ComponentResult; external name '_DataHGetScheduleAheadTime';
{
 *  DataHSetCacheSizeLimit()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function DataHSetCacheSizeLimit(dh: DataHandler; cacheSizeLimit: Size): ComponentResult; external name '_DataHSetCacheSizeLimit';
{
 *  DataHGetCacheSizeLimit()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function DataHGetCacheSizeLimit(dh: DataHandler; var cacheSizeLimit: Size): ComponentResult; external name '_DataHGetCacheSizeLimit';
{
 *  DataHGetMovie()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function DataHGetMovie(dh: DataHandler; var theMovie: Movie; var id: SInt16): ComponentResult; external name '_DataHGetMovie';
{
 *  DataHAddMovie()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function DataHAddMovie(dh: DataHandler; theMovie: Movie; var id: SInt16): ComponentResult; external name '_DataHAddMovie';
{
 *  DataHUpdateMovie()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function DataHUpdateMovie(dh: DataHandler; theMovie: Movie; id: SInt16): ComponentResult; external name '_DataHUpdateMovie';
{
 *  DataHDoesBuffer()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function DataHDoesBuffer(dh: DataHandler; var buffersReads: boolean; var buffersWrites: boolean): ComponentResult; external name '_DataHDoesBuffer';
{
 *  DataHGetFileName()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function DataHGetFileName(dh: DataHandler; var str: Str255): ComponentResult; external name '_DataHGetFileName';
{
 *  DataHGetAvailableFileSize()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function DataHGetAvailableFileSize(dh: DataHandler; var fileSize: SInt32): ComponentResult; external name '_DataHGetAvailableFileSize';
{
 *  DataHGetMacOSFileType()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function DataHGetMacOSFileType(dh: DataHandler; var fileType: OSType): ComponentResult; external name '_DataHGetMacOSFileType';
{
 *  DataHGetMIMEType()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function DataHGetMIMEType(dh: DataHandler; var mimeType: Str255): ComponentResult; external name '_DataHGetMIMEType';
{
 *  DataHSetDataRefWithAnchor()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function DataHSetDataRefWithAnchor(dh: DataHandler; anchorDataRef: Handle; dataRefType: OSType; dataRef: Handle): ComponentResult; external name '_DataHSetDataRefWithAnchor';
{
 *  DataHGetDataRefWithAnchor()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function DataHGetDataRefWithAnchor(dh: DataHandler; anchorDataRef: Handle; dataRefType: OSType; var dataRef: Handle): ComponentResult; external name '_DataHGetDataRefWithAnchor';
{
 *  DataHSetMacOSFileType()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function DataHSetMacOSFileType(dh: DataHandler; fileType: OSType): ComponentResult; external name '_DataHSetMacOSFileType';
{
 *  DataHSetTimeBase()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function DataHSetTimeBase(dh: DataHandler; tb: TimeBase): ComponentResult; external name '_DataHSetTimeBase';
{
 *  DataHGetInfoFlags()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function DataHGetInfoFlags(dh: DataHandler; var flags: UInt32): ComponentResult; external name '_DataHGetInfoFlags';
{
 *  DataHScheduleData64()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function DataHScheduleData64(dh: DataHandler; PlaceToPutDataPtr: Ptr; const (*var*) FileOffset: wide; DataSize: SInt32; RefCon: SInt32; scheduleRec: DataHSchedulePtr; CompletionRtn: DataHCompletionUPP): ComponentResult; external name '_DataHScheduleData64';
{
 *  DataHWrite64()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function DataHWrite64(dh: DataHandler; data: Ptr; const (*var*) offset: wide; size: SInt32; completion: DataHCompletionUPP; refCon: SInt32): ComponentResult; external name '_DataHWrite64';
{
 *  DataHGetFileSize64()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function DataHGetFileSize64(dh: DataHandler; var fileSize: wide): ComponentResult; external name '_DataHGetFileSize64';
{
 *  DataHPreextend64()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function DataHPreextend64(dh: DataHandler; const (*var*) maxToAdd: wide; var spaceAdded: wide): ComponentResult; external name '_DataHPreextend64';
{
 *  DataHSetFileSize64()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function DataHSetFileSize64(dh: DataHandler; const (*var*) fileSize: wide): ComponentResult; external name '_DataHSetFileSize64';
{
 *  DataHGetFreeSpace64()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function DataHGetFreeSpace64(dh: DataHandler; var freeSize: wide): ComponentResult; external name '_DataHGetFreeSpace64';
{
 *  DataHAppend64()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function DataHAppend64(dh: DataHandler; data: UnivPtr; var fileOffset: wide; size: UInt32): ComponentResult; external name '_DataHAppend64';
{
 *  DataHReadAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function DataHReadAsync(dh: DataHandler; dataPtr: UnivPtr; dataSize: UInt32; const (*var*) dataOffset: wide; completion: DataHCompletionUPP; refCon: SInt32): ComponentResult; external name '_DataHReadAsync';
{
 *  DataHPollRead()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function DataHPollRead(dh: DataHandler; dataPtr: UnivPtr; var dataSizeSoFar: UInt32): ComponentResult; external name '_DataHPollRead';
{
 *  DataHGetDataAvailability()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function DataHGetDataAvailability(dh: DataHandler; offset: SInt32; len: SInt32; var missing_offset: SInt32; var missing_len: SInt32): ComponentResult; external name '_DataHGetDataAvailability';
{
 *  DataHGetFileSizeAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function DataHGetFileSizeAsync(dh: DataHandler; var fileSize: wide; completionRtn: DataHCompletionUPP; refCon: SInt32): ComponentResult; external name '_DataHGetFileSizeAsync';
{
 *  DataHGetDataRefAsType()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.1 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.1 and later
 }
function DataHGetDataRefAsType(dh: DataHandler; requestedType: OSType; var dataRef: Handle): ComponentResult; external name '_DataHGetDataRefAsType';
{
 *  DataHSetDataRefExtension()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.1 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.1 and later
 }
function DataHSetDataRefExtension(dh: DataHandler; extension: Handle; idType: OSType): ComponentResult; external name '_DataHSetDataRefExtension';
{
 *  DataHGetDataRefExtension()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.1 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.1 and later
 }
function DataHGetDataRefExtension(dh: DataHandler; var extension: Handle; idType: OSType): ComponentResult; external name '_DataHGetDataRefExtension';
{
 *  DataHGetMovieWithFlags()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.1 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.1 and later
 }
function DataHGetMovieWithFlags(dh: DataHandler; var theMovie: Movie; var id: SInt16; flags: SInt16): ComponentResult; external name '_DataHGetMovieWithFlags';
{
 *  DataHGetFileTypeOrdering()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 5.0 and later
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 5.0 and later
 }
function DataHGetFileTypeOrdering(dh: DataHandler; var orderingListHandle: DataHFileTypeOrderingHandle): ComponentResult; external name '_DataHGetFileTypeOrdering';
{  flags for DataHCreateFileWithFlags }

const
	kDataHCreateFileButDontCreateResFile = $00000001;

	{
	 *  DataHCreateFileWithFlags()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in QuickTimeLib 5.0 and later
	 *    CarbonLib:        in CarbonLib 1.3 and later
	 *    Mac OS X:         in version 10.0 and later
	 *    Windows:          in qtmlClient.lib 5.0 and later
	 	}
function DataHCreateFileWithFlags(dh: DataHandler; creator: OSType; deleteExisting: boolean; flags: UInt32): ComponentResult; external name '_DataHCreateFileWithFlags';
{
 *  DataHGetMIMETypeAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 5.0 and later
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 5.0 and later
 }
function DataHGetMIMETypeAsync(dh: DataHandler; var mimeType: Str255; completionRtn: DataHCompletionUPP; refCon: SInt32): ComponentResult; external name '_DataHGetMIMETypeAsync';
{
 *  DataHGetInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 5.0.1 and later
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.1 and later
 *    Windows:          in qtmlClient.lib 5.0.1 and later
 }
function DataHGetInfo(dh: DataHandler; what: OSType; info: UnivPtr): ComponentResult; external name '_DataHGetInfo';
{
 *  DataHSetIdleManager()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 6.0 and later
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.2 and later
 *    Windows:          in qtmlClient.lib 6.0 and later
 }
function DataHSetIdleManager(dh: DataHandler; im: IdleManager): ComponentResult; external name '_DataHSetIdleManager';
{
 *  DataHDeleteFile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 6.0 and later
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.2 and later
 *    Windows:          in qtmlClient.lib 6.0 and later
 }
function DataHDeleteFile(dh: DataHandler): ComponentResult; external name '_DataHDeleteFile';
const
	kDataHMovieUsageDoAppendMDAT = $00000001;					{  if set, datahandler should append wide and mdat atoms in append call }

	{
	 *  DataHSetMovieUsageFlags()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in QuickTimeLib 6.0 and later
	 *    CarbonLib:        in CarbonLib 1.6 and later
	 *    Mac OS X:         in version 10.2 and later
	 *    Windows:          in qtmlClient.lib 6.0 and later
	 	}
function DataHSetMovieUsageFlags(dh: DataHandler; flags: SInt32): ComponentResult; external name '_DataHSetMovieUsageFlags';
const
	kDataHTempUseSameDirectory	= $00000001;					{  temp data ref should be in same directory as current data ref (vs. in temporary directory) }
	kDataHTempUseSameVolume		= $00000002;					{  temp data ref should be on same volume as current data ref (vs. find "best" volume) }
	kDataHTempCreateFile		= $00000004;					{  create the file }
	kDataHTempOpenFile			= $00000008;					{  open temporary file for write (kDataHTempCreateFile must be passed, too) }

	{
	 *  DataHUseTemporaryDataRef()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in QuickTimeLib 6.0 and later
	 *    CarbonLib:        in CarbonLib 1.6 and later
	 *    Mac OS X:         in version 10.2 and later
	 *    Windows:          in qtmlClient.lib 6.0 and later
	 	}
function DataHUseTemporaryDataRef(dh: DataHandler; inFlags: SInt32): ComponentResult; external name '_DataHUseTemporaryDataRef';
{
 *  DataHGetTemporaryDataRefCapabilities()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 6.0 and later
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.2 and later
 *    Windows:          in qtmlClient.lib 6.0 and later
 }
function DataHGetTemporaryDataRefCapabilities(dh: DataHandler; var outUnderstoodFlags: SInt32): ComponentResult; external name '_DataHGetTemporaryDataRefCapabilities';
{
 *  DataHRenameFile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 6.0 and later
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.2 and later
 *    Windows:          in qtmlClient.lib 6.0 and later
 }
function DataHRenameFile(dh: DataHandler; newDataRef: Handle): ComponentResult; external name '_DataHRenameFile';
{
 *  DataHPlaybackHints()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function DataHPlaybackHints(dh: DataHandler; flags: SInt32; minFileOffset: UInt32; maxFileOffset: UInt32; bytesPerSecond: SInt32): ComponentResult; external name '_DataHPlaybackHints';
{
 *  DataHPlaybackHints64()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.1 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.1 and later
 }
function DataHPlaybackHints64(dh: DataHandler; flags: SInt32; const (*var*) minFileOffset: wide; const (*var*) maxFileOffset: wide; bytesPerSecond: SInt32): ComponentResult; external name '_DataHPlaybackHints64';
{  Symbolic constants for DataHGetDataRate }

const
	kDataHGetDataRateInfiniteRate = $7FFFFFFF;					{  all the data arrived instantaneously }

	{
	 *  DataHGetDataRate()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in QuickTimeLib 5.0 and later
	 *    CarbonLib:        in CarbonLib 1.3 and later
	 *    Mac OS X:         in version 10.0 and later
	 *    Windows:          in qtmlClient.lib 5.0 and later
	 	}
function DataHGetDataRate(dh: DataHandler; flags: SInt32; var bytesPerSecond: SInt32): ComponentResult; external name '_DataHGetDataRate';
{  Flags for DataHSetTimeHints }

const
	kDataHSetTimeHintsSkipBandwidthRequest = $01;				{  set if this data handler should use the network without requesting bandwidth }

	{
	 *  DataHSetTimeHints()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in QuickTimeLib 5.0 and later
	 *    CarbonLib:        in CarbonLib 1.3 and later
	 *    Mac OS X:         in version 10.0 and later
	 *    Windows:          in qtmlClient.lib 5.0 and later
	 	}
function DataHSetTimeHints(dh: DataHandler; flags: SInt32; bandwidthPriority: SInt32; scale: TimeScale; minTime: TimeValue; maxTime: TimeValue): ComponentResult; external name '_DataHSetTimeHints';
{ Standard type for video digitizers }

const
	videoDigitizerComponentType	= $76646967 (* 'vdig' *);
	vdigInterfaceRev			= 2;

	{	 Input Format Standards 	}
	ntscIn						= 0;							{  current input format  }
	currentIn					= 0;							{  ntsc input format  }
	palIn						= 1;							{  pal input format  }
	secamIn						= 2;							{  secam input format  }
	ntscReallyIn				= 3;							{  ntsc input format  }

	{	 Input Formats 	}
	compositeIn					= 0;							{  input is composite format  }
	sVideoIn					= 1;							{  input is sVideo format  }
	rgbComponentIn				= 2;							{  input is rgb component format  }
	rgbComponentSyncIn			= 3;							{  input is rgb component format (sync on green?) }
	yuvComponentIn				= 4;							{  input is yuv component format  }
	yuvComponentSyncIn			= 5;							{  input is yuv component format (sync on green?)  }
	tvTunerIn					= 6;
	sdiIn						= 7;


	{	 Video Digitizer PlayThru States 	}
	vdPlayThruOff				= 0;
	vdPlayThruOn				= 1;

	{	 Input Color Space Modes 	}
	vdDigitizerBW				= 0;							{  black and white  }
	vdDigitizerRGB				= 1;							{  rgb color  }

	{	 Phase Lock Loop Modes 	}
	vdBroadcastMode				= 0;							{  Broadcast / Laser Disk video mode  }
	vdVTRMode					= 1;							{  VCR / Magnetic media mode  }

	{	 Field Select Options 	}
	vdUseAnyField				= 0;							{  Digitizers choice on field use  }
	vdUseOddField				= 1;							{  Use odd field for half size vert and smaller  }
	vdUseEvenField				= 2;							{  Use even field for half size vert and smaller  }

	{	 vdig types 	}
	vdTypeBasic					= 0;							{  basic, no clipping  }
	vdTypeAlpha					= 1;							{  supports clipping with alpha channel  }
	vdTypeMask					= 2;							{  supports clipping with mask plane  }
	vdTypeKey					= 3;							{  supports clipping with key color(s)  }


	{	 Digitizer Input Capability/Current Flags 	}
	digiInDoesNTSC				= $00000001;					{  digitizer supports NTSC input format  }
	digiInDoesPAL				= $00000002;					{  digitizer supports PAL input format  }
	digiInDoesSECAM				= $00000004;					{  digitizer supports SECAM input format  }
	digiInDoesGenLock			= $00000080;					{  digitizer does genlock  }
	digiInDoesComposite			= $00000100;					{  digitizer supports composite input type  }
	digiInDoesSVideo			= $00000200;					{  digitizer supports S-Video input type  }
	digiInDoesComponent			= $00000400;					{  digitizer supports component = rgb, input type  }
	digiInVTR_Broadcast			= $00000800;					{  digitizer can differentiate between the two  }
	digiInDoesColor				= $00001000;					{  digitizer supports color  }
	digiInDoesBW				= $00002000;					{  digitizer supports black & white  }
																{  Digitizer Input Current Flags = these are valid only during active operating conditions,    }
	digiInSignalLock			= $80000000;					{  digitizer detects input signal is locked, this bit = horiz lock || vertical lock  }


	{	 Digitizer Output Capability/Current Flags 	}
	digiOutDoes1				= $00000001;					{  digitizer supports 1 bit pixels  }
	digiOutDoes2				= $00000002;					{  digitizer supports 2 bit pixels  }
	digiOutDoes4				= $00000004;					{  digitizer supports 4 bit pixels  }
	digiOutDoes8				= $00000008;					{  digitizer supports 8 bit pixels  }
	digiOutDoes16				= $00000010;					{  digitizer supports 16 bit pixels  }
	digiOutDoes32				= $00000020;					{  digitizer supports 32 bit pixels  }
	digiOutDoesDither			= $00000040;					{  digitizer dithers in indexed modes  }
	digiOutDoesStretch			= $00000080;					{  digitizer can arbitrarily stretch  }
	digiOutDoesShrink			= $00000100;					{  digitizer can arbitrarily shrink  }
	digiOutDoesMask				= $00000200;					{  digitizer can mask to clipping regions  }
	digiOutDoesDouble			= $00000800;					{  digitizer can stretch to exactly double size  }
	digiOutDoesQuad				= $00001000;					{  digitizer can stretch exactly quadruple size  }
	digiOutDoesQuarter			= $00002000;					{  digitizer can shrink to exactly quarter size  }
	digiOutDoesSixteenth		= $00004000;					{  digitizer can shrink to exactly sixteenth size  }
	digiOutDoesRotate			= $00008000;					{  digitizer supports rotate transformations  }
	digiOutDoesHorizFlip		= $00010000;					{  digitizer supports horizontal flips Sx < 0  }
	digiOutDoesVertFlip			= $00020000;					{  digitizer supports vertical flips Sy < 0  }
	digiOutDoesSkew				= $00040000;					{  digitizer supports skew = shear,twist,  }
	digiOutDoesBlend			= $00080000;
	digiOutDoesWarp				= $00100000;
	digiOutDoesHW_DMA			= $00200000;					{  digitizer not constrained to local device  }
	digiOutDoesHWPlayThru		= $00400000;					{  digitizer doesn't need time to play thru  }
	digiOutDoesILUT				= $00800000;					{  digitizer does inverse LUT for index modes  }
	digiOutDoesKeyColor			= $01000000;					{  digitizer does key color functions too  }
	digiOutDoesAsyncGrabs		= $02000000;					{  digitizer supports async grabs  }
	digiOutDoesUnreadableScreenBits = $04000000;				{  playthru doesn't generate readable bits on screen }
	digiOutDoesCompress			= $08000000;					{  supports alternate output data types  }
	digiOutDoesCompressOnly		= $10000000;					{  can't provide raw frames anywhere  }
	digiOutDoesPlayThruDuringCompress = $20000000;				{  digi can do playthru while providing compressed data  }
	digiOutDoesCompressPartiallyVisible = $40000000;			{  digi doesn't need all bits visible on screen to do hardware compress  }
	digiOutDoesNotNeedCopyOfCompressData = $80000000;			{  digi doesn't need any bufferization when providing compressed data  }

	{	 Types 	}

type
	VideoDigitizerComponent				= ComponentInstance;
	VideoDigitizerError					= ComponentResult;
	DigitizerInfoPtr = ^DigitizerInfo;
	DigitizerInfo = record
		vdigType:				SInt16;
		inputCapabilityFlags:	SInt32;
		outputCapabilityFlags:	SInt32;
		inputCurrentFlags:		SInt32;
		outputCurrentFlags:		SInt32;
		slot:					SInt16;								{  temporary for connection purposes  }
		gdh:					GDHandle;								{  temporary for digitizers that have preferred screen  }
		maskgdh:				GDHandle;								{  temporary for digitizers that have mask planes  }
		minDestHeight:			SInt16;								{  Smallest resizable height  }
		minDestWidth:			SInt16;								{  Smallest resizable width  }
		maxDestHeight:			SInt16;								{  Largest resizable height  }
		maxDestWidth:			SInt16;								{  Largest resizable width  }
		blendLevels:			SInt16;								{  Number of blend levels supported (2 if 1 bit mask)  }
		reserved:				SInt32;								{  reserved  }
	end;

	VdigTypePtr = ^VdigType;
	VdigType = record
		digType:				SInt32;
		reserved:				SInt32;
	end;

	VdigTypeListPtr = ^VdigTypeList;
	VdigTypeList = record
		count:					SInt16;
		list:					array [0..0] of VdigType;
	end;

	VdigBufferRecPtr = ^VdigBufferRec;
	VdigBufferRec = record
		dest:					PixMapHandle;
		location:				Point;
		reserved:				SInt32;
	end;

	VdigBufferRecListPtr = ^VdigBufferRecList;
	VdigBufferRecList = record
		count:					SInt16;
		matrix:					MatrixRecordPtr;
		mask:					RgnHandle;
		list:					array [0..0] of VdigBufferRec;
	end;

	VdigBufferRecListHandle				= ^VdigBufferRecListPtr;
{$ifc TYPED_FUNCTION_POINTERS}
	VdigIntProcPtr = procedure(flags: SInt32; refcon: SInt32);
{$elsec}
	VdigIntProcPtr = ProcPtr;
{$endc}

{$ifc OPAQUE_UPP_TYPES}
	VdigIntUPP = ^SInt32; { an opaque UPP }
{$elsec}
	VdigIntUPP = UniversalProcPtr;
{$endc}	
	VDCompressionListPtr = ^VDCompressionList;
	VDCompressionList = record
		codec:					CodecComponent;
		cType:					CodecType;
		typeName:				Str63;
		name:					Str63;
		formatFlags:			SInt32;
		compressFlags:			SInt32;
		reserved:				SInt32;
	end;

	VDCompressionListHandle				= ^VDCompressionListPtr;

const
	dmaDepth1					= 1;
	dmaDepth2					= 2;
	dmaDepth4					= 4;
	dmaDepth8					= 8;
	dmaDepth16					= 16;
	dmaDepth32					= 32;
	dmaDepth2Gray				= 64;
	dmaDepth4Gray				= 128;
	dmaDepth8Gray				= 256;

	kVDIGControlledFrameRate	= -1;


	{
	 *  VDGetMaxSrcRect()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 *    Windows:          in qtmlClient.lib 3.0 and later
	 	}
function VDGetMaxSrcRect(ci: VideoDigitizerComponent; inputStd: SInt16; var maxSrcRect: Rect): VideoDigitizerError; external name '_VDGetMaxSrcRect';
{
 *  VDGetActiveSrcRect()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function VDGetActiveSrcRect(ci: VideoDigitizerComponent; inputStd: SInt16; var activeSrcRect: Rect): VideoDigitizerError; external name '_VDGetActiveSrcRect';
{
 *  VDSetDigitizerRect()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function VDSetDigitizerRect(ci: VideoDigitizerComponent; var digitizerRect: Rect): VideoDigitizerError; external name '_VDSetDigitizerRect';
{
 *  VDGetDigitizerRect()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function VDGetDigitizerRect(ci: VideoDigitizerComponent; var digitizerRect: Rect): VideoDigitizerError; external name '_VDGetDigitizerRect';
{
 *  VDGetVBlankRect()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function VDGetVBlankRect(ci: VideoDigitizerComponent; inputStd: SInt16; var vBlankRect: Rect): VideoDigitizerError; external name '_VDGetVBlankRect';
{
 *  VDGetMaskPixMap()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function VDGetMaskPixMap(ci: VideoDigitizerComponent; maskPixMap: PixMapHandle): VideoDigitizerError; external name '_VDGetMaskPixMap';
{
 *  VDGetPlayThruDestination()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function VDGetPlayThruDestination(ci: VideoDigitizerComponent; var dest: PixMapHandle; var destRect: Rect; var m: MatrixRecord; var mask: RgnHandle): VideoDigitizerError; external name '_VDGetPlayThruDestination';
{
 *  VDUseThisCLUT()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function VDUseThisCLUT(ci: VideoDigitizerComponent; colorTableHandle: CTabHandle): VideoDigitizerError; external name '_VDUseThisCLUT';
{
 *  VDSetInputGammaValue()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function VDSetInputGammaValue(ci: VideoDigitizerComponent; channel1: Fixed; channel2: Fixed; channel3: Fixed): VideoDigitizerError; external name '_VDSetInputGammaValue';
{
 *  VDGetInputGammaValue()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function VDGetInputGammaValue(ci: VideoDigitizerComponent; var channel1: Fixed; var channel2: Fixed; var channel3: Fixed): VideoDigitizerError; external name '_VDGetInputGammaValue';
{
 *  VDSetBrightness()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function VDSetBrightness(ci: VideoDigitizerComponent; var brightness: UInt16): VideoDigitizerError; external name '_VDSetBrightness';
{
 *  VDGetBrightness()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function VDGetBrightness(ci: VideoDigitizerComponent; var brightness: UInt16): VideoDigitizerError; external name '_VDGetBrightness';
{
 *  VDSetContrast()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function VDSetContrast(ci: VideoDigitizerComponent; var contrast: UInt16): VideoDigitizerError; external name '_VDSetContrast';
{
 *  VDSetHue()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function VDSetHue(ci: VideoDigitizerComponent; var hue: UInt16): VideoDigitizerError; external name '_VDSetHue';
{
 *  VDSetSharpness()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function VDSetSharpness(ci: VideoDigitizerComponent; var sharpness: UInt16): VideoDigitizerError; external name '_VDSetSharpness';
{
 *  VDSetSaturation()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function VDSetSaturation(ci: VideoDigitizerComponent; var saturation: UInt16): VideoDigitizerError; external name '_VDSetSaturation';
{
 *  VDGetContrast()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function VDGetContrast(ci: VideoDigitizerComponent; var contrast: UInt16): VideoDigitizerError; external name '_VDGetContrast';
{
 *  VDGetHue()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function VDGetHue(ci: VideoDigitizerComponent; var hue: UInt16): VideoDigitizerError; external name '_VDGetHue';
{
 *  VDGetSharpness()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function VDGetSharpness(ci: VideoDigitizerComponent; var sharpness: UInt16): VideoDigitizerError; external name '_VDGetSharpness';
{
 *  VDGetSaturation()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function VDGetSaturation(ci: VideoDigitizerComponent; var saturation: UInt16): VideoDigitizerError; external name '_VDGetSaturation';
{
 *  VDGrabOneFrame()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function VDGrabOneFrame(ci: VideoDigitizerComponent): VideoDigitizerError; external name '_VDGrabOneFrame';
{
 *  VDGetMaxAuxBuffer()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function VDGetMaxAuxBuffer(ci: VideoDigitizerComponent; var pm: PixMapHandle; var r: Rect): VideoDigitizerError; external name '_VDGetMaxAuxBuffer';
{
 *  VDGetDigitizerInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function VDGetDigitizerInfo(ci: VideoDigitizerComponent; var info: DigitizerInfo): VideoDigitizerError; external name '_VDGetDigitizerInfo';
{
 *  VDGetCurrentFlags()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function VDGetCurrentFlags(ci: VideoDigitizerComponent; var inputCurrentFlag: SInt32; var outputCurrentFlag: SInt32): VideoDigitizerError; external name '_VDGetCurrentFlags';
{
 *  VDSetKeyColor()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function VDSetKeyColor(ci: VideoDigitizerComponent; index: SInt32): VideoDigitizerError; external name '_VDSetKeyColor';
{
 *  VDGetKeyColor()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function VDGetKeyColor(ci: VideoDigitizerComponent; var index: SInt32): VideoDigitizerError; external name '_VDGetKeyColor';
{
 *  VDAddKeyColor()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function VDAddKeyColor(ci: VideoDigitizerComponent; var index: SInt32): VideoDigitizerError; external name '_VDAddKeyColor';
{
 *  VDGetNextKeyColor()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function VDGetNextKeyColor(ci: VideoDigitizerComponent; index: SInt32): VideoDigitizerError; external name '_VDGetNextKeyColor';
{
 *  VDSetKeyColorRange()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function VDSetKeyColorRange(ci: VideoDigitizerComponent; var minRGB: RGBColor; var maxRGB: RGBColor): VideoDigitizerError; external name '_VDSetKeyColorRange';
{
 *  VDGetKeyColorRange()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function VDGetKeyColorRange(ci: VideoDigitizerComponent; var minRGB: RGBColor; var maxRGB: RGBColor): VideoDigitizerError; external name '_VDGetKeyColorRange';
{
 *  VDSetDigitizerUserInterrupt()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function VDSetDigitizerUserInterrupt(ci: VideoDigitizerComponent; flags: SInt32; userInterruptProc: VdigIntUPP; refcon: SInt32): VideoDigitizerError; external name '_VDSetDigitizerUserInterrupt';
{
 *  VDSetInputColorSpaceMode()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function VDSetInputColorSpaceMode(ci: VideoDigitizerComponent; colorSpaceMode: SInt16): VideoDigitizerError; external name '_VDSetInputColorSpaceMode';
{
 *  VDGetInputColorSpaceMode()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function VDGetInputColorSpaceMode(ci: VideoDigitizerComponent; var colorSpaceMode: SInt16): VideoDigitizerError; external name '_VDGetInputColorSpaceMode';
{
 *  VDSetClipState()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function VDSetClipState(ci: VideoDigitizerComponent; clipEnable: SInt16): VideoDigitizerError; external name '_VDSetClipState';
{
 *  VDGetClipState()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function VDGetClipState(ci: VideoDigitizerComponent; var clipEnable: SInt16): VideoDigitizerError; external name '_VDGetClipState';
{
 *  VDSetClipRgn()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function VDSetClipRgn(ci: VideoDigitizerComponent; clipRegion: RgnHandle): VideoDigitizerError; external name '_VDSetClipRgn';
{
 *  VDClearClipRgn()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function VDClearClipRgn(ci: VideoDigitizerComponent; clipRegion: RgnHandle): VideoDigitizerError; external name '_VDClearClipRgn';
{
 *  VDGetCLUTInUse()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function VDGetCLUTInUse(ci: VideoDigitizerComponent; var colorTableHandle: CTabHandle): VideoDigitizerError; external name '_VDGetCLUTInUse';
{
 *  VDSetPLLFilterType()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function VDSetPLLFilterType(ci: VideoDigitizerComponent; pllType: SInt16): VideoDigitizerError; external name '_VDSetPLLFilterType';
{
 *  VDGetPLLFilterType()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function VDGetPLLFilterType(ci: VideoDigitizerComponent; var pllType: SInt16): VideoDigitizerError; external name '_VDGetPLLFilterType';
{
 *  VDGetMaskandValue()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function VDGetMaskandValue(ci: VideoDigitizerComponent; blendLevel: UInt16; var mask: SInt32; var value: SInt32): VideoDigitizerError; external name '_VDGetMaskandValue';
{
 *  VDSetMasterBlendLevel()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function VDSetMasterBlendLevel(ci: VideoDigitizerComponent; var blendLevel: UInt16): VideoDigitizerError; external name '_VDSetMasterBlendLevel';
{
 *  VDSetPlayThruDestination()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function VDSetPlayThruDestination(ci: VideoDigitizerComponent; dest: PixMapHandle; destRect: RectPtr; m: MatrixRecordPtr; mask: RgnHandle): VideoDigitizerError; external name '_VDSetPlayThruDestination';
{
 *  VDSetPlayThruOnOff()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function VDSetPlayThruOnOff(ci: VideoDigitizerComponent; state: SInt16): VideoDigitizerError; external name '_VDSetPlayThruOnOff';
{
 *  VDSetFieldPreference()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function VDSetFieldPreference(ci: VideoDigitizerComponent; fieldFlag: SInt16): VideoDigitizerError; external name '_VDSetFieldPreference';
{
 *  VDGetFieldPreference()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function VDGetFieldPreference(ci: VideoDigitizerComponent; var fieldFlag: SInt16): VideoDigitizerError; external name '_VDGetFieldPreference';
{
 *  VDPreflightDestination()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function VDPreflightDestination(ci: VideoDigitizerComponent; var digitizerRect: Rect; var dest: PixMapPtr; destRect: RectPtr; m: MatrixRecordPtr): VideoDigitizerError; external name '_VDPreflightDestination';
{
 *  VDPreflightGlobalRect()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function VDPreflightGlobalRect(ci: VideoDigitizerComponent; theWindow: GrafPtr; var globalRect: Rect): VideoDigitizerError; external name '_VDPreflightGlobalRect';
{
 *  VDSetPlayThruGlobalRect()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function VDSetPlayThruGlobalRect(ci: VideoDigitizerComponent; theWindow: GrafPtr; var globalRect: Rect): VideoDigitizerError; external name '_VDSetPlayThruGlobalRect';
{
 *  VDSetInputGammaRecord()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function VDSetInputGammaRecord(ci: VideoDigitizerComponent; inputGammaPtr: VDGamRecPtr): VideoDigitizerError; external name '_VDSetInputGammaRecord';
{
 *  VDGetInputGammaRecord()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function VDGetInputGammaRecord(ci: VideoDigitizerComponent; var inputGammaPtr: VDGamRecPtr): VideoDigitizerError; external name '_VDGetInputGammaRecord';
{
 *  VDSetBlackLevelValue()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function VDSetBlackLevelValue(ci: VideoDigitizerComponent; var blackLevel: UInt16): VideoDigitizerError; external name '_VDSetBlackLevelValue';
{
 *  VDGetBlackLevelValue()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function VDGetBlackLevelValue(ci: VideoDigitizerComponent; var blackLevel: UInt16): VideoDigitizerError; external name '_VDGetBlackLevelValue';
{
 *  VDSetWhiteLevelValue()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function VDSetWhiteLevelValue(ci: VideoDigitizerComponent; var whiteLevel: UInt16): VideoDigitizerError; external name '_VDSetWhiteLevelValue';
{
 *  VDGetWhiteLevelValue()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function VDGetWhiteLevelValue(ci: VideoDigitizerComponent; var whiteLevel: UInt16): VideoDigitizerError; external name '_VDGetWhiteLevelValue';
{
 *  VDGetVideoDefaults()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function VDGetVideoDefaults(ci: VideoDigitizerComponent; var blackLevel: UInt16; var whiteLevel: UInt16; var brightness: UInt16; var hue: UInt16; var saturation: UInt16; var contrast: UInt16; var sharpness: UInt16): VideoDigitizerError; external name '_VDGetVideoDefaults';
{
 *  VDGetNumberOfInputs()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function VDGetNumberOfInputs(ci: VideoDigitizerComponent; var inputs: SInt16): VideoDigitizerError; external name '_VDGetNumberOfInputs';
{
 *  VDGetInputFormat()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function VDGetInputFormat(ci: VideoDigitizerComponent; input: SInt16; var format: SInt16): VideoDigitizerError; external name '_VDGetInputFormat';
{
 *  VDSetInput()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function VDSetInput(ci: VideoDigitizerComponent; input: SInt16): VideoDigitizerError; external name '_VDSetInput';
{
 *  VDGetInput()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function VDGetInput(ci: VideoDigitizerComponent; var input: SInt16): VideoDigitizerError; external name '_VDGetInput';
{
 *  VDSetInputStandard()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function VDSetInputStandard(ci: VideoDigitizerComponent; inputStandard: SInt16): VideoDigitizerError; external name '_VDSetInputStandard';
{
 *  VDSetupBuffers()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function VDSetupBuffers(ci: VideoDigitizerComponent; bufferList: VdigBufferRecListHandle): VideoDigitizerError; external name '_VDSetupBuffers';
{
 *  VDGrabOneFrameAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function VDGrabOneFrameAsync(ci: VideoDigitizerComponent; buffer: SInt16): VideoDigitizerError; external name '_VDGrabOneFrameAsync';
{
 *  VDDone()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function VDDone(ci: VideoDigitizerComponent; buffer: SInt16): VideoDigitizerError; external name '_VDDone';
{
 *  VDSetCompression()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function VDSetCompression(ci: VideoDigitizerComponent; compressType: OSType; depth: SInt16; var bounds: Rect; spatialQuality: CodecQ; temporalQuality: CodecQ; keyFrameRate: SInt32): VideoDigitizerError; external name '_VDSetCompression';
{
 *  VDCompressOneFrameAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function VDCompressOneFrameAsync(ci: VideoDigitizerComponent): VideoDigitizerError; external name '_VDCompressOneFrameAsync';
{    Note that UInt8* queuedFrameCount replaces Boolean* done. 0(==false) still means no frames, and 1(==true) one, 
    but if more than one are available the number should be returned here. The value 2 previously meant more than one frame,
    so some VDIGs may return 2 even if more than 2 are available, and some will still return 1 as they are using the original definition }
{
 *  VDCompressDone()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function VDCompressDone(ci: VideoDigitizerComponent; var queuedFrameCount: UInt8; var theData: Ptr; var dataSize: SInt32; var similarity: UInt8; var t: TimeRecord): VideoDigitizerError; external name '_VDCompressDone';
{
 *  VDReleaseCompressBuffer()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function VDReleaseCompressBuffer(ci: VideoDigitizerComponent; bufferAddr: Ptr): VideoDigitizerError; external name '_VDReleaseCompressBuffer';
{
 *  VDGetImageDescription()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function VDGetImageDescription(ci: VideoDigitizerComponent; desc: ImageDescriptionHandle): VideoDigitizerError; external name '_VDGetImageDescription';
{
 *  VDResetCompressSequence()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function VDResetCompressSequence(ci: VideoDigitizerComponent): VideoDigitizerError; external name '_VDResetCompressSequence';
{
 *  VDSetCompressionOnOff()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function VDSetCompressionOnOff(ci: VideoDigitizerComponent; state: boolean): VideoDigitizerError; external name '_VDSetCompressionOnOff';
{
 *  VDGetCompressionTypes()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function VDGetCompressionTypes(ci: VideoDigitizerComponent; h: VDCompressionListHandle): VideoDigitizerError; external name '_VDGetCompressionTypes';
{
 *  VDSetTimeBase()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function VDSetTimeBase(ci: VideoDigitizerComponent; t: TimeBase): VideoDigitizerError; external name '_VDSetTimeBase';
{
 *  VDSetFrameRate()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function VDSetFrameRate(ci: VideoDigitizerComponent; framesPerSecond: Fixed): VideoDigitizerError; external name '_VDSetFrameRate';
{
 *  VDGetDataRate()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function VDGetDataRate(ci: VideoDigitizerComponent; var milliSecPerFrame: SInt32; var framesPerSecond: Fixed; var bytesPerSecond: SInt32): VideoDigitizerError; external name '_VDGetDataRate';
{
 *  VDGetSoundInputDriver()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function VDGetSoundInputDriver(ci: VideoDigitizerComponent; var soundDriverName: Str255): VideoDigitizerError; external name '_VDGetSoundInputDriver';
{
 *  VDGetDMADepths()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function VDGetDMADepths(ci: VideoDigitizerComponent; var depthArray: SInt32; var preferredDepth: SInt32): VideoDigitizerError; external name '_VDGetDMADepths';
{
 *  VDGetPreferredTimeScale()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function VDGetPreferredTimeScale(ci: VideoDigitizerComponent; var preferred: TimeScale): VideoDigitizerError; external name '_VDGetPreferredTimeScale';
{
 *  VDReleaseAsyncBuffers()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function VDReleaseAsyncBuffers(ci: VideoDigitizerComponent): VideoDigitizerError; external name '_VDReleaseAsyncBuffers';
{ 83 is reserved for compatibility reasons }
{
 *  VDSetDataRate()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function VDSetDataRate(ci: VideoDigitizerComponent; bytesPerSecond: SInt32): VideoDigitizerError; external name '_VDSetDataRate';
{
 *  VDGetTimeCode()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function VDGetTimeCode(ci: VideoDigitizerComponent; var atTime: TimeRecord; timeCodeFormat: UnivPtr; timeCodeTime: UnivPtr): VideoDigitizerError; external name '_VDGetTimeCode';
{
 *  VDUseSafeBuffers()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function VDUseSafeBuffers(ci: VideoDigitizerComponent; useSafeBuffers: boolean): VideoDigitizerError; external name '_VDUseSafeBuffers';
{
 *  VDGetSoundInputSource()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function VDGetSoundInputSource(ci: VideoDigitizerComponent; videoInput: SInt32; var soundInput: SInt32): VideoDigitizerError; external name '_VDGetSoundInputSource';
{
 *  VDGetCompressionTime()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function VDGetCompressionTime(ci: VideoDigitizerComponent; compressionType: OSType; depth: SInt16; var srcRect: Rect; var spatialQuality: CodecQ; var temporalQuality: CodecQ; var compressTime: UInt32): VideoDigitizerError; external name '_VDGetCompressionTime';
{
 *  VDSetPreferredPacketSize()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function VDSetPreferredPacketSize(ci: VideoDigitizerComponent; preferredPacketSizeInBytes: SInt32): VideoDigitizerError; external name '_VDSetPreferredPacketSize';
{
 *  VDSetPreferredImageDimensions()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function VDSetPreferredImageDimensions(ci: VideoDigitizerComponent; width: SInt32; height: SInt32): VideoDigitizerError; external name '_VDSetPreferredImageDimensions';
{
 *  VDGetPreferredImageDimensions()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function VDGetPreferredImageDimensions(ci: VideoDigitizerComponent; var width: SInt32; var height: SInt32): VideoDigitizerError; external name '_VDGetPreferredImageDimensions';
{
 *  VDGetInputName()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function VDGetInputName(ci: VideoDigitizerComponent; videoInput: SInt32; var name: Str255): VideoDigitizerError; external name '_VDGetInputName';
{
 *  VDSetDestinationPort()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function VDSetDestinationPort(ci: VideoDigitizerComponent; destPort: CGrafPtr): VideoDigitizerError; external name '_VDSetDestinationPort';
{
   The following call is designed to give the VDIG a little more control over how it is presented to the User, to clarify the 
   distinction between Devices and Inputs. Historically, the assumption has been that there is one component registered per device
   and the component name is displayed. This change lets a component choose its name after registration.
   vdDeviceFlagShowInputsAsDevices is meant for components that register once and support multiple devices 
   The UI is clearer if these are presented as device rather than inputs, 
   and this allows a VDIG to present itself this way without huge restructuring
   vdDeviceFlagHideDevice is for the kind of VDIG that registers itself, and then can register a further VDIG for each device. 
   If no hardware is available, returning this flag will omit it from the list. 
   This call being made is also a good time to check for hardware and register further VDIG components if needed, 
   allowing for lazy initialization when the Application needs to find a VDIG rather than on every launch or replug.
}


const
	vdDeviceFlagShowInputsAsDevices = $01;						{  Tell the Panel to promote Inputs to Devices }
	vdDeviceFlagHideDevice		= $02;							{  Omit this Device entirely from the list }

	{
	 *  VDGetDeviceNameAndFlags()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in QuickTimeLib 6.0 and later
	 *    CarbonLib:        in CarbonLib 1.6 and later
	 *    Mac OS X:         in version 10.2 and later
	 *    Windows:          in qtmlClient.lib 6.0 and later
	 	}
function VDGetDeviceNameAndFlags(ci: VideoDigitizerComponent; var outName: Str255; var outNameFlags: UInt32): VideoDigitizerError; external name '_VDGetDeviceNameAndFlags';
const
	vdFlagCaptureStarting		= $01;							{  Capture is about to start; allocate bandwidth  }
	vdFlagCaptureStopping		= $02;							{  Capture is about to stop; stop queuing frames }
	vdFlagCaptureIsForPreview	= $04;							{  Capture is just to screen for preview purposes }
	vdFlagCaptureIsForRecord	= $08;							{  Capture is going to be recorded }
	vdFlagCaptureLowLatency		= $10;							{  Fresh frames are more important than delivering every frame - don't queue too much }
	vdFlagCaptureAlwaysUseTimeBase = $20;						{  Use the timebase for every frame; don't worry about making durations uniform }
	vdFlagCaptureSetSettingsBegin = $40;						{  A series of calls are about to be made to restore settings. }
	vdFlagCaptureSetSettingsEnd	= $80;							{  Finished restoring settings; any set calls after this are from the app or UI }

	{
	 *  VDCaptureStateChanging()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in QuickTimeLib 6.0 and later
	 *    CarbonLib:        in CarbonLib 1.6 and later
	 *    Mac OS X:         in version 10.2 and later
	 *    Windows:          in qtmlClient.lib 6.0 and later
	 	}
function VDCaptureStateChanging(ci: VideoDigitizerComponent; inStateFlags: UInt32): VideoDigitizerError; external name '_VDCaptureStateChanging';
{
   These UniqueID calls are so that the VDIG can give the SG information enabling it to restore a particular
   configuration - choose a particular device and input from those available.
   For example, restoring the specific camera for a set of several hot-plugged FireWire cameras 
   the caller can pass nil if it is not interested in one of the IDs
   returning 0 in an ID means you don't have one
}

{
 *  VDGetUniqueIDs()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 6.0 and later
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.2 and later
 *    Windows:          in qtmlClient.lib 6.0 and later
 }
function VDGetUniqueIDs(ci: VideoDigitizerComponent; var outDeviceID: UInt64; var outInputID: UInt64): VideoDigitizerError; external name '_VDGetUniqueIDs';
{
   Note this is a 'Select' not a 'Set' - the assumption is that the Unique ID is a function of the hardware
   and not modifiable by the calling application. Either a nil pointer or 0 an the ID means don't care.
   return vdDontHaveThatUniqueIDErr if your device doesn't have a match.
}

{
 *  VDSelectUniqueIDs()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 6.0 and later
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.2 and later
 *    Windows:          in qtmlClient.lib 6.0 and later
 }
function VDSelectUniqueIDs(ci: VideoDigitizerComponent; (*const*) var inDeviceID: UInt64; (*const*) var inInputID: UInt64): VideoDigitizerError; external name '_VDSelectUniqueIDs';
const
	xmlParseComponentType		= $70617273 (* 'pars' *);
	xmlParseComponentSubType	= $786D6C20 (* 'xml ' *);

	xmlIdentifierInvalid		= 0;
	xmlIdentifierUnrecognized	= $FFFFFFFF;
	xmlContentTypeInvalid		= 0;
	xmlContentTypeElement		= 1;
	xmlContentTypeCharData		= 2;

	elementFlagAlwaysSelfContained = $00000001;					{     Element doesn't have contents or closing tag even if it doesn't end with />, as in the HTML <img> tag }
	elementFlagPreserveWhiteSpace = $00000002;					{   Preserve whitespace in content, default is to remove it  }
	xmlParseFlagAllowUppercase	= $00000001;					{     Entities and attributes do not have to be lowercase (strict XML), but can be upper or mixed case as in HTML }
	xmlParseFlagAllowUnquotedAttributeValues = $00000002;		{     Attributes values do not have to be enclosed in quotes (strict XML), but can be left unquoted if they contain no spaces }
	xmlParseFlagEventParseOnly	= $00000004;					{     Do event parsing only }

	attributeValueKindCharString = 0;
	attributeValueKindInteger	= $00000001;					{     Number }
	attributeValueKindPercent	= $00000002;					{     Number or percent }
	attributeValueKindBoolean	= $00000004;					{     "true" or "false" }
	attributeValueKindOnOff		= $00000008;					{     "on" or "off" }
	attributeValueKindColor		= $00000010;					{     Either "#rrggbb" or a color name }
	attributeValueKindEnum		= $00000020;					{     one of a number of strings; the enum strings are passed as a zero-separated, double-zero-terminated C string in the attributeKindValueInfo param }
	attributeValueKindCaseSensEnum = $00000040;					{     one of a number of strings; the enum strings are passed as for attributeValueKindEnum, but the values are case-sensitive }
	MAX_ATTRIBUTE_VALUE_KIND	= $00000040;

	nameSpaceIDNone				= 0;

	{   A Parsed XML attribute value, one of number/percent, boolean/on-off, color, or enumerated type }

type
	XMLAttributeValuePtr = ^XMLAttributeValue;
	XMLAttributeValue = record
		case SInt16 of
		0: (
			number:				SInt32;									{     The value when valueKind is attributeValueKindInteger or attributeValueKindPercent }
			);
		1: (
			boolean:			boolean_fix;		                    {     The value when valueKind is attributeValueKindBoolean or attributeValueKindOnOff }
			);
		2: (
			color:				RGBColor;								{     The value when valueKind is attributeValueKindColor }
			);
		3: (
			enumType:			UInt32;									{     The value when valueKind is attributeValueKindEnum }
			);
	end;

	{   An XML attribute-value pair }
	XMLAttributePtr = ^XMLAttribute;
	XMLAttribute = record
		identifier:				UInt32;									{     Tokenized identifier, if the attribute name was recognized by the parser }
		name:					CStringPtr;								{     Attribute name, Only present if identifier == xmlIdentifierUnrecognized }
		valueKind:				SInt32;								{     Type of parsed value, if the value was recognized and parsed; otherwise, attributeValueKindCharString }
		value:					XMLAttributeValue;						{     Parsed attribute value }
		valueStr:				CStringPtr;								{     Always present }
	end;

	{   Forward struct declarations for recursively-defined tree structure }
	XMLContentPtr = ^XMLContent;
	{
	    An XML Element, i.e.
	        <element attr="value" attr="value" ...> [contents] </element>
	    or
	        <element attr="value" attr="value" .../>
	}
	XMLElementPtr = ^XMLElement;
	XMLElement = record
		identifier:				UInt32;									{     Tokenized identifier, if the element name was recognized by the parser }
		name:					CStringPtr;								{     Element name, only present if identifier == xmlIdentifierUnrecognized }
		attributes:				XMLAttributePtr;						{     Array of attributes, terminated with an attribute with identifier == xmlIdentifierInvalid }
		contents:				XMLContentPtr;							{     Array of contents, terminated with a content with kind == xmlIdentifierInvalid }
	end;

	{
	    The content of an XML element is a series of parts, each of which may be either another element
	    or simply character data.
	}
	XMLElementContentPtr = ^XMLElementContent;
	XMLElementContent = record
		case SInt16 of
		0: (
			element:			XMLElement;								{     The contents when the content kind is xmlContentTypeElement }
			);
		1: (
			charData:			CStringPtr;								{     The contents when the content kind is xmlContentTypeCharData }
			);
	end;

	XMLContent = record
		kind:					UInt32;
		actualContent:			XMLElementContent;
	end;

	XMLDocRecordPtr = ^XMLDocRecord;
	XMLDocRecord = record
		xmlDataStorage:			Ptr;									{     opaque storage }
		rootElement:			XMLElement;
	end;

	XMLDoc								= ^XMLDocRecord;
	{ callback routines for event parsing }
{$ifc TYPED_FUNCTION_POINTERS}
	StartDocumentHandler = function(refcon: SInt32): ComponentResult;
{$elsec}
	StartDocumentHandler = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	EndDocumentHandler = function(refcon: SInt32): ComponentResult;
{$elsec}
	EndDocumentHandler = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	StartElementHandler = function(name: ConstCStringPtr; var atts: ConstCStringPtr; refcon: SInt32): ComponentResult;
{$elsec}
	StartElementHandler = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	EndElementHandler = function(name: ConstCStringPtr; refcon: SInt32): ComponentResult;
{$elsec}
	EndElementHandler = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	CharDataHandler = function(charData: ConstCStringPtr; refcon: SInt32): ComponentResult;
{$elsec}
	CharDataHandler = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	PreprocessInstructionHandler = function(name: ConstCStringPtr; atts: ConstCStringPtrPtr; refcon: SInt32): ComponentResult;
{$elsec}
	PreprocessInstructionHandler = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	CommentHandler = function(comment: ConstCStringPtr; refcon: SInt32): ComponentResult;
{$elsec}
	CommentHandler = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	CDataHandler = function(cdata: ConstCStringPtr; refcon: SInt32): ComponentResult;
{$elsec}
	CDataHandler = ProcPtr;
{$endc}

{$ifc OPAQUE_UPP_TYPES}
	StartDocumentHandlerUPP = ^SInt32; { an opaque UPP }
{$elsec}
	StartDocumentHandlerUPP = UniversalProcPtr;
{$endc}	
{$ifc OPAQUE_UPP_TYPES}
	EndDocumentHandlerUPP = ^SInt32; { an opaque UPP }
{$elsec}
	EndDocumentHandlerUPP = UniversalProcPtr;
{$endc}	
{$ifc OPAQUE_UPP_TYPES}
	StartElementHandlerUPP = ^SInt32; { an opaque UPP }
{$elsec}
	StartElementHandlerUPP = UniversalProcPtr;
{$endc}	
{$ifc OPAQUE_UPP_TYPES}
	EndElementHandlerUPP = ^SInt32; { an opaque UPP }
{$elsec}
	EndElementHandlerUPP = UniversalProcPtr;
{$endc}	
{$ifc OPAQUE_UPP_TYPES}
	CharDataHandlerUPP = ^SInt32; { an opaque UPP }
{$elsec}
	CharDataHandlerUPP = UniversalProcPtr;
{$endc}	
{$ifc OPAQUE_UPP_TYPES}
	PreprocessInstructionHandlerUPP = ^SInt32; { an opaque UPP }
{$elsec}
	PreprocessInstructionHandlerUPP = UniversalProcPtr;
{$endc}	
{$ifc OPAQUE_UPP_TYPES}
	CommentHandlerUPP = ^SInt32; { an opaque UPP }
{$elsec}
	CommentHandlerUPP = UniversalProcPtr;
{$endc}	
{$ifc OPAQUE_UPP_TYPES}
	CDataHandlerUPP = ^SInt32; { an opaque UPP }
{$elsec}
	CDataHandlerUPP = UniversalProcPtr;
{$endc}	
	{   Parses the XML file pointed to by dataRef, returning a XMLDoc parse tree }
	{
	 *  XMLParseDataRef()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in QuickTimeLib 5.0 and later
	 *    CarbonLib:        in CarbonLib 1.3 and later
	 *    Mac OS X:         in version 10.0 and later
	 *    Windows:          in qtmlClient.lib 5.0 and later
	 	}
function XMLParseDataRef(aParser: ComponentInstance; dataRef: Handle; dataRefType: OSType; parseFlags: SInt32; var document: XMLDoc): ComponentResult; external name '_XMLParseDataRef';
{   Parses the XML file pointed to by fileSpec, returning a XMLDoc parse tree }
{
 *  XMLParseFile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 5.0 and later
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 5.0 and later
 }
function XMLParseFile(aParser: ComponentInstance; fileSpec: ConstFSSpecPtr; parseFlags: SInt32; var document: XMLDoc): ComponentResult; external name '_XMLParseFile';
{   Disposes of a XMLDoc parse tree }
{
 *  XMLParseDisposeXMLDoc()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 5.0 and later
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 5.0 and later
 }
function XMLParseDisposeXMLDoc(aParser: ComponentInstance; document: XMLDoc): ComponentResult; external name '_XMLParseDisposeXMLDoc';
{
    Returns a more detailed description of the error and the line in which it occurred, if a
    file failed to parse properly.
}
{
 *  XMLParseGetDetailedParseError()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 5.0 and later
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 5.0 and later
 }
function XMLParseGetDetailedParseError(aParser: ComponentInstance; var errorLine: SInt32; errDesc: StringPtr): ComponentResult; external name '_XMLParseGetDetailedParseError';
{
    Tell the parser of an element to be recognized. The tokenized element unique identifier is
    passed in *elementID, unless *elementID is zero, whereupon a unique ID is generated and returned.
    Thus, a valid element identifier can never be zero.
}
{
 *  XMLParseAddElement()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 5.0 and later
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 5.0 and later
 }
function XMLParseAddElement(aParser: ComponentInstance; elementName: CStringPtr; nameSpaceID: UInt32; var elementID: UInt32; elementFlags: SInt32): ComponentResult; external name '_XMLParseAddElement';
{
    Tells the parser of an attribute for the specified element. The tokenized attribute unique
    ID is passed in *attributeID, unless *attributeID is zero, whereupon a unique ID is generated and
    returned. Thus, a valid attribute identifier can never be zero.
}
{
 *  XMLParseAddAttribute()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 5.0 and later
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 5.0 and later
 }
function XMLParseAddAttribute(aParser: ComponentInstance; elementID: UInt32; nameSpaceID: UInt32; attributeName: CStringPtr; var attributeID: UInt32): ComponentResult; external name '_XMLParseAddAttribute';
{
    Tells the parser of several attributes for the specified element. The attributes are passed
    as a zero-delimited, double-zero-terminated C string in attributeNames, and the attribute
    IDs are passed in on attributeIDs as an array; if any attributeIDs are zero, unique IDs
    are generated for those and returned
}
{
 *  XMLParseAddMultipleAttributes()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 5.0 and later
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 5.0 and later
 }
function XMLParseAddMultipleAttributes(aParser: ComponentInstance; elementID: UInt32; var nameSpaceIDs: UInt32; attributeNames: CStringPtr; var attributeIDs: UInt32): ComponentResult; external name '_XMLParseAddMultipleAttributes';
{
    Tells the parser of an attribute, which may have a particular type of value, for the
    specified element. Params are as in XMLParseAddAttribute, plus all the kinds of values
    the attribute may have are passed in attributeValueKind, and optional additional information
    required to tokenize the particular kind of attribute is passed in attributeValueKindInfo
}
{
 *  XMLParseAddAttributeAndValue()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 5.0 and later
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 5.0 and later
 }
function XMLParseAddAttributeAndValue(aParser: ComponentInstance; elementID: UInt32; nameSpaceID: UInt32; attributeName: CStringPtr; var attributeID: UInt32; attributeValueKind: UInt32; attributeValueKindInfo: UnivPtr): ComponentResult; external name '_XMLParseAddAttributeAndValue';
{
    Tells the parser of several attributes, which may have a particular type of value, for the
    specified element. Params are as in XMLParseAddMultipleAttributes, plus all the kinds of values
    the attributes may have are passed in attributeValueKinds, and optional additional information
    required to tokenize the particular kind of attributes is passed in attributeValueKindInfos
}
{
 *  XMLParseAddMultipleAttributesAndValues()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 5.0 and later
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 5.0 and later
 }
function XMLParseAddMultipleAttributesAndValues(aParser: ComponentInstance; elementID: UInt32; var nameSpaceIDs: UInt32; attributeNames: CStringPtr; var attributeIDs: UInt32; var attributeValueKinds: UInt32; var attributeValueKindInfos: UnivPtr): ComponentResult; external name '_XMLParseAddMultipleAttributesAndValues';
{
    Tells the parser that the particular attribute may have an additional kind of
    value, as specified by attributeValueKind and attributeValueKindInfo
}
{
 *  XMLParseAddAttributeValueKind()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 5.0 and later
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 5.0 and later
 }
function XMLParseAddAttributeValueKind(aParser: ComponentInstance; elementID: UInt32; attributeID: UInt32; attributeValueKind: UInt32; attributeValueKindInfo: UnivPtr): ComponentResult; external name '_XMLParseAddAttributeValueKind';
{
    Tell the parser of a namespace to be recognized. The tokenized namespace unique identifier is
    passed in *nameSpaceID, unless *nameSpaceID is zero, whereupon a unique ID is generated and returned.
    Thus, a valid nameSpaceID identifier can never be zero.
}
{
 *  XMLParseAddNameSpace()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 5.0 and later
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 5.0 and later
 }
function XMLParseAddNameSpace(aParser: ComponentInstance; nameSpaceURL: CStringPtr; var nameSpaceID: UInt32): ComponentResult; external name '_XMLParseAddNameSpace';
{   Specifies the offset and limit for reading from the dataref to be used when parsing }
{
 *  XMLParseSetOffsetAndLimit()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 5.0 and later
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 5.0 and later
 }
function XMLParseSetOffsetAndLimit(aParser: ComponentInstance; offset: UInt32; limit: UInt32): ComponentResult; external name '_XMLParseSetOffsetAndLimit';
{   Set the event parse refcon }
{
 *  XMLParseSetEventParseRefCon()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 5.0 and later
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 5.0 and later
 }
function XMLParseSetEventParseRefCon(aParser: ComponentInstance; refcon: SInt32): ComponentResult; external name '_XMLParseSetEventParseRefCon';
{   Set the start document handler UPP for event parsing }
{
 *  XMLParseSetStartDocumentHandler()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 5.0 and later
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 5.0 and later
 }
function XMLParseSetStartDocumentHandler(aParser: ComponentInstance; startDocument: StartDocumentHandlerUPP): ComponentResult; external name '_XMLParseSetStartDocumentHandler';
{   Set the end document handler UPP for event parsing }
{
 *  XMLParseSetEndDocumentHandler()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 5.0 and later
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 5.0 and later
 }
function XMLParseSetEndDocumentHandler(aParser: ComponentInstance; endDocument: EndDocumentHandlerUPP): ComponentResult; external name '_XMLParseSetEndDocumentHandler';
{   Set the start element handler UPP for event parsing }
{
 *  XMLParseSetStartElementHandler()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 5.0 and later
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 5.0 and later
 }
function XMLParseSetStartElementHandler(aParser: ComponentInstance; startElement: StartElementHandlerUPP): ComponentResult; external name '_XMLParseSetStartElementHandler';
{   Set the end element handler UPP for event parsing }
{
 *  XMLParseSetEndElementHandler()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 5.0 and later
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 5.0 and later
 }
function XMLParseSetEndElementHandler(aParser: ComponentInstance; endElement: EndElementHandlerUPP): ComponentResult; external name '_XMLParseSetEndElementHandler';
{   Set the character data handler UPP for event parsing }
{
 *  XMLParseSetCharDataHandler()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 5.0 and later
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 5.0 and later
 }
function XMLParseSetCharDataHandler(aParser: ComponentInstance; charData: CharDataHandlerUPP): ComponentResult; external name '_XMLParseSetCharDataHandler';
{   Set the preprocess instruction handler UPP for event parsing }
{
 *  XMLParseSetPreprocessInstructionHandler()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 5.0 and later
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 5.0 and later
 }
function XMLParseSetPreprocessInstructionHandler(aParser: ComponentInstance; preprocessInstruction: PreprocessInstructionHandlerUPP): ComponentResult; external name '_XMLParseSetPreprocessInstructionHandler';
{   Set the comment handler UPP for event parsing }
{
 *  XMLParseSetCommentHandler()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 5.0 and later
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 5.0 and later
 }
function XMLParseSetCommentHandler(aParser: ComponentInstance; comment: CommentHandlerUPP): ComponentResult; external name '_XMLParseSetCommentHandler';
{   Set the cdata handler UPP for event parsing }
{
 *  XMLParseSetCDataHandler()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 6.0 and later
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.2 and later
 *    Windows:          in qtmlClient.lib 6.0 and later
 }
function XMLParseSetCDataHandler(aParser: ComponentInstance; cdata: CDataHandlerUPP): ComponentResult; external name '_XMLParseSetCDataHandler';
{
    Helper Macros
    
        These macros allow you to easily add entities and attributes to the parser
        in an error free manner when the identifiers are defined in a particular manner.
        For these to work, you must define the identifiers as follows:
        
        For entities, they must be defined as element_elementName, as in:
        
            enum
            (
                element_xml =   1,      //  "xml"
                element_head,           //  "head"
                element_body            //  "body"
            );
            
        If the element name has characters that are illegal in an identifier,
        some of the macros support that, but the identifier must not contain
        the illegal characters:
        
            enum
            (
                element_rootlayout      //  "root-layout"
            )
            
        For attribute names, similar rules apply except that they must be defined
        as attr_attributeName, as in:
            
            enum
            (
                attr_src    =   1,      //  "src"
                attr_href,
                attr_width,
                attr_height
            )
            
        Finally, the existence of local variables elementID and attributeID is required.
}
{
    Adds the specified element to the parser, i.e. XML_ADD_ELEMENT(head) adds the element "head" with
    a unique identifier of element_head
}
{
    Adds the specified element to the parser, not using the same string to generate the identifier and
    the element name. Use for element names that contain characters which are illegal in identifiers,
    i.e XML_ADD_COMPLEX_ELEMENT("root-layout",rootlayout) adds the element "root-layout" with a unique
    identifier of element_rootlayout
}
{
    Adds the specified attribute to the current element in the parser, i.e. XML_ADD_ATTRIBUTE(src)
    adds the attribute "src" to the current element, and identifies it by attr_src
}
{
    Adds the specified attribute to the current element in the parser, i.e. XML_ADD_ATTRIBUTE(element_img, src)
    adds the attribute "src" to the element_img element, and identifies it by attr_src
    Adds the specified attribute to the current element in the parser, not using the same string to
    generate the identifier and the element name. Use for attribute names that contain characters which
    are illegal in identifiers, i.e XML_ADD_COMPLEX_ATTRIBUTE("http-equiv",httpequiv) adds the element
    "http-equiv" with a unique identifier of attr_httpequiv
}


{
    General Sequence Grab stuff
}

type
	SeqGrabComponent					= ComponentInstance;
	SGChannel							= ComponentInstance;

const
	SeqGrabComponentType		= $62617267 (* 'barg' *);
	SeqGrabChannelType			= $73676368 (* 'sgch' *);
	SeqGrabPanelType			= $7367706E (* 'sgpn' *);
	SeqGrabCompressionPanelType	= $636D7072 (* 'cmpr' *);
	SeqGrabSourcePanelType		= $736F7572 (* 'sour' *);

	seqGrabToDisk				= 1;
	seqGrabToMemory				= 2;
	seqGrabDontUseTempMemory	= 4;
	seqGrabAppendToFile			= 8;
	seqGrabDontAddMovieResource	= 16;
	seqGrabDontMakeMovie		= 32;
	seqGrabPreExtendFile		= 64;
	seqGrabDataProcIsInterruptSafe = 128;
	seqGrabDataProcDoesOverlappingReads = 256;


type
	SeqGrabDataOutputEnum				= UInt32;

const
	seqGrabRecord				= 1;
	seqGrabPreview				= 2;
	seqGrabPlayDuringRecord		= 4;
	seqGrabLowLatencyCapture	= 8;							{  return the freshest frame possible, for live work (videoconferencing, live broadcast, live image processing)  }
	seqGrabAlwaysUseTimeBase	= 16;							{  Tell VDIGs to use TimebaseTime always, rather than creating uniform frame durations, for more accurate live sync with audio  }


type
	SeqGrabUsageEnum					= UInt32;

const
	seqGrabHasBounds			= 1;
	seqGrabHasVolume			= 2;
	seqGrabHasDiscreteSamples	= 4;
	seqGrabDoNotBufferizeData	= 8;
	seqGrabCanMoveWindowWhileRecording = 16;


type
	SeqGrabChannelInfoEnum				= UInt32;
	SGOutputRecordPtr = ^SGOutputRecord;
	SGOutputRecord = record
		data:					array [0..0] of SInt32;
	end;

	SGOutput							= ^SGOutputRecord;
	SeqGrabFrameInfoPtr = ^SeqGrabFrameInfo;
	SeqGrabFrameInfo = record
		frameOffset:			SInt32;
		frameTime:				SInt32;
		frameSize:				SInt32;
		frameChannel:			SGChannel;
		frameRefCon:			SInt32;
	end;

	SeqGrabExtendedFrameInfoPtr = ^SeqGrabExtendedFrameInfo;
	SeqGrabExtendedFrameInfo = record
		frameOffset:			wide;
		frameTime:				SInt32;
		frameSize:				SInt32;
		frameChannel:			SGChannel;
		frameRefCon:			SInt32;
		frameOutput:			SGOutput;
	end;


const
	grabPictOffScreen			= 1;
	grabPictIgnoreClip			= 2;
	grabPictCurrentImage		= 4;

	sgFlagControlledGrab		= $01;
	sgFlagAllowNonRGBPixMaps	= $02;


type
{$ifc TYPED_FUNCTION_POINTERS}
	SGDataProcPtr = function(c: SGChannel; p: Ptr; len: SInt32; var offset: SInt32; chRefCon: SInt32; time: TimeValue; writeType: SInt16; refCon: SInt32): OSErr;
{$elsec}
	SGDataProcPtr = ProcPtr;
{$endc}

{$ifc OPAQUE_UPP_TYPES}
	SGDataUPP = ^SInt32; { an opaque UPP }
{$elsec}
	SGDataUPP = UniversalProcPtr;
{$endc}	
	SGDeviceInputNamePtr = ^SGDeviceInputName;
	SGDeviceInputName = record
		name:					Str63;
		icon:					Handle;
		flags:					SInt32;
		reserved:				SInt32;								{  zero }
	end;


const
	sgDeviceInputNameFlagInputUnavailable = $01;


type
	SGDeviceInputListRecordPtr = ^SGDeviceInputListRecord;
	SGDeviceInputListRecord = record
		count:					SInt16;
		selectedIndex:			SInt16;
		reserved:				SInt32;								{  zero }
		entry:					array [0..0] of SGDeviceInputName;
	end;

	SGDeviceInputListPtr				= ^SGDeviceInputListRecord;
	SGDeviceInputList					= ^SGDeviceInputListPtr;
	SGDeviceNamePtr = ^SGDeviceName;
	SGDeviceName = record
		name:					Str63;
		icon:					Handle;
		flags:					SInt32;
		refCon:					SInt32;
		inputs:					SGDeviceInputList;						{  list of inputs; formerly reserved to 0 }
	end;


const
	sgDeviceNameFlagDeviceUnavailable = $01;
	sgDeviceNameFlagShowInputsAsDevices = $02;


type
	SGDeviceListRecordPtr = ^SGDeviceListRecord;
	SGDeviceListRecord = record
		count:					SInt16;
		selectedIndex:			SInt16;
		reserved:				SInt32;								{  zero }
		entry:					array [0..0] of SGDeviceName;
	end;

	SGDeviceListPtr						= ^SGDeviceListRecord;
	SGDeviceList						= ^SGDeviceListPtr;

const
	sgDeviceListWithIcons		= $01;
	sgDeviceListDontCheckAvailability = $02;
	sgDeviceListIncludeInputs	= $04;

	seqGrabWriteAppend			= 0;
	seqGrabWriteReserve			= 1;
	seqGrabWriteFill			= 2;

	seqGrabUnpause				= 0;
	seqGrabPause				= 1;
	seqGrabPauseForMenu			= 3;

	channelFlagDontOpenResFile	= 2;
	channelFlagHasDependency	= 4;


type
{$ifc TYPED_FUNCTION_POINTERS}
	SGModalFilterProcPtr = function(theDialog: DialogRef; const (*var*) theEvent: EventRecord; var itemHit: SInt16; refCon: SInt32): boolean;
{$elsec}
	SGModalFilterProcPtr = ProcPtr;
{$endc}

{$ifc OPAQUE_UPP_TYPES}
	SGModalFilterUPP = ^SInt32; { an opaque UPP }
{$elsec}
	SGModalFilterUPP = UniversalProcPtr;
{$endc}	

const
	sgPanelFlagForPanel			= 1;

	seqGrabSettingsPreviewOnly	= 1;

	channelPlayNormal			= 0;
	channelPlayFast				= 1;
	channelPlayHighQuality		= 2;
	channelPlayAllData			= 4;


	{
	 *  SGInitialize()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 *    Windows:          in qtmlClient.lib 3.0 and later
	 	}
function SGInitialize(s: SeqGrabComponent): ComponentResult; external name '_SGInitialize';
{
 *  SGSetDataOutput()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGSetDataOutput(s: SeqGrabComponent; const (*var*) movieFile: FSSpec; whereFlags: SInt32): ComponentResult; external name '_SGSetDataOutput';
{
 *  SGGetDataOutput()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGGetDataOutput(s: SeqGrabComponent; var movieFile: FSSpec; var whereFlags: SInt32): ComponentResult; external name '_SGGetDataOutput';
{
 *  SGSetGWorld()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGSetGWorld(s: SeqGrabComponent; gp: CGrafPtr; gd: GDHandle): ComponentResult; external name '_SGSetGWorld';
{
 *  SGGetGWorld()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGGetGWorld(s: SeqGrabComponent; var gp: CGrafPtr; var gd: GDHandle): ComponentResult; external name '_SGGetGWorld';
{
 *  SGNewChannel()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGNewChannel(s: SeqGrabComponent; channelType: OSType; var ref: SGChannel): ComponentResult; external name '_SGNewChannel';
{
 *  SGDisposeChannel()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGDisposeChannel(s: SeqGrabComponent; c: SGChannel): ComponentResult; external name '_SGDisposeChannel';
{
 *  SGStartPreview()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGStartPreview(s: SeqGrabComponent): ComponentResult; external name '_SGStartPreview';
{
 *  SGStartRecord()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGStartRecord(s: SeqGrabComponent): ComponentResult; external name '_SGStartRecord';
{
 *  SGIdle()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGIdle(s: SeqGrabComponent): ComponentResult; external name '_SGIdle';
{
 *  SGStop()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGStop(s: SeqGrabComponent): ComponentResult; external name '_SGStop';
{
 *  SGPause()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGPause(s: SeqGrabComponent; pause: ByteParameter): ComponentResult; external name '_SGPause';
{
 *  SGPrepare()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGPrepare(s: SeqGrabComponent; prepareForPreview: boolean; prepareForRecord: boolean): ComponentResult; external name '_SGPrepare';
{
 *  SGRelease()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGRelease(s: SeqGrabComponent): ComponentResult; external name '_SGRelease';
{
 *  SGGetMovie()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGGetMovie(s: SeqGrabComponent): Movie; external name '_SGGetMovie';
{
 *  SGSetMaximumRecordTime()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGSetMaximumRecordTime(s: SeqGrabComponent; ticks: UInt32): ComponentResult; external name '_SGSetMaximumRecordTime';
{
 *  SGGetMaximumRecordTime()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGGetMaximumRecordTime(s: SeqGrabComponent; var ticks: UInt32): ComponentResult; external name '_SGGetMaximumRecordTime';
{
 *  SGGetStorageSpaceRemaining()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGGetStorageSpaceRemaining(s: SeqGrabComponent; var bytes: UInt32): ComponentResult; external name '_SGGetStorageSpaceRemaining';
{
 *  SGGetTimeRemaining()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGGetTimeRemaining(s: SeqGrabComponent; var ticksLeft: SInt32): ComponentResult; external name '_SGGetTimeRemaining';
{
 *  SGGrabPict()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGGrabPict(s: SeqGrabComponent; var p: PicHandle; const (*var*) bounds: Rect; offscreenDepth: SInt16; grabPictFlags: SInt32): ComponentResult; external name '_SGGrabPict';
{
 *  SGGetLastMovieResID()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGGetLastMovieResID(s: SeqGrabComponent; var resID: SInt16): ComponentResult; external name '_SGGetLastMovieResID';
{
 *  SGSetFlags()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGSetFlags(s: SeqGrabComponent; sgFlags: SInt32): ComponentResult; external name '_SGSetFlags';
{
 *  SGGetFlags()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGGetFlags(s: SeqGrabComponent; var sgFlags: SInt32): ComponentResult; external name '_SGGetFlags';
{
 *  SGSetDataProc()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGSetDataProc(s: SeqGrabComponent; proc: SGDataUPP; refCon: SInt32): ComponentResult; external name '_SGSetDataProc';
{
 *  SGNewChannelFromComponent()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGNewChannelFromComponent(s: SeqGrabComponent; var newChannel: SGChannel; sgChannelComponent: Component): ComponentResult; external name '_SGNewChannelFromComponent';
{
 *  SGDisposeDeviceList()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGDisposeDeviceList(s: SeqGrabComponent; list: SGDeviceList): ComponentResult; external name '_SGDisposeDeviceList';
{
 *  SGAppendDeviceListToMenu()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGAppendDeviceListToMenu(s: SeqGrabComponent; list: SGDeviceList; mh: MenuRef): ComponentResult; external name '_SGAppendDeviceListToMenu';
{
 *  SGSetSettings()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGSetSettings(s: SeqGrabComponent; ud: UserData; flags: SInt32): ComponentResult; external name '_SGSetSettings';
{
 *  SGGetSettings()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGGetSettings(s: SeqGrabComponent; var ud: UserData; flags: SInt32): ComponentResult; external name '_SGGetSettings';
{
 *  SGGetIndChannel()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGGetIndChannel(s: SeqGrabComponent; index: SInt16; var ref: SGChannel; var chanType: OSType): ComponentResult; external name '_SGGetIndChannel';
{
 *  SGUpdate()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGUpdate(s: SeqGrabComponent; updateRgn: RgnHandle): ComponentResult; external name '_SGUpdate';
{
 *  SGGetPause()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGGetPause(s: SeqGrabComponent; var paused: Byte): ComponentResult; external name '_SGGetPause';
type
	ConstComponentListPtr				= ^Component;
	{
	 *  SGSettingsDialog()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 *    Windows:          in qtmlClient.lib 3.0 and later
	 	}
function SGSettingsDialog(s: SeqGrabComponent; c: SGChannel; numPanels: SInt16; panelList: ConstComponentListPtr; flags: SInt32; proc: SGModalFilterUPP; procRefNum: SInt32): ComponentResult; external name '_SGSettingsDialog';
{
 *  SGGetAlignmentProc()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGGetAlignmentProc(s: SeqGrabComponent; alignmentProc: ICMAlignmentProcRecordPtr): ComponentResult; external name '_SGGetAlignmentProc';
{
 *  SGSetChannelSettings()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGSetChannelSettings(s: SeqGrabComponent; c: SGChannel; ud: UserData; flags: SInt32): ComponentResult; external name '_SGSetChannelSettings';
{
 *  SGGetChannelSettings()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGGetChannelSettings(s: SeqGrabComponent; c: SGChannel; var ud: UserData; flags: SInt32): ComponentResult; external name '_SGGetChannelSettings';
{
 *  SGGetMode()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGGetMode(s: SeqGrabComponent; var previewMode: boolean; var recordMode: boolean): ComponentResult; external name '_SGGetMode';
{
 *  SGSetDataRef()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGSetDataRef(s: SeqGrabComponent; dataRef: Handle; dataRefType: OSType; whereFlags: SInt32): ComponentResult; external name '_SGSetDataRef';
{
 *  SGGetDataRef()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGGetDataRef(s: SeqGrabComponent; var dataRef: Handle; var dataRefType: OSType; var whereFlags: SInt32): ComponentResult; external name '_SGGetDataRef';
{
 *  SGNewOutput()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGNewOutput(s: SeqGrabComponent; dataRef: Handle; dataRefType: OSType; whereFlags: SInt32; var sgOut: SGOutput): ComponentResult; external name '_SGNewOutput';
{
 *  SGDisposeOutput()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGDisposeOutput(s: SeqGrabComponent; sgOut: SGOutput): ComponentResult; external name '_SGDisposeOutput';
{
 *  SGSetOutputFlags()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGSetOutputFlags(s: SeqGrabComponent; sgOut: SGOutput; whereFlags: SInt32): ComponentResult; external name '_SGSetOutputFlags';
{
 *  SGSetChannelOutput()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGSetChannelOutput(s: SeqGrabComponent; c: SGChannel; sgOut: SGOutput): ComponentResult; external name '_SGSetChannelOutput';
{
 *  SGGetDataOutputStorageSpaceRemaining()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGGetDataOutputStorageSpaceRemaining(s: SeqGrabComponent; sgOut: SGOutput; var space: UInt32): ComponentResult; external name '_SGGetDataOutputStorageSpaceRemaining';
{
 *  SGHandleUpdateEvent()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGHandleUpdateEvent(s: SeqGrabComponent; const (*var*) event: EventRecord; var handled: boolean): ComponentResult; external name '_SGHandleUpdateEvent';
{
 *  SGSetOutputNextOutput()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGSetOutputNextOutput(s: SeqGrabComponent; sgOut: SGOutput; nextOut: SGOutput): ComponentResult; external name '_SGSetOutputNextOutput';
{
 *  SGGetOutputNextOutput()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGGetOutputNextOutput(s: SeqGrabComponent; sgOut: SGOutput; var nextOut: SGOutput): ComponentResult; external name '_SGGetOutputNextOutput';
{
 *  SGSetOutputMaximumOffset()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGSetOutputMaximumOffset(s: SeqGrabComponent; sgOut: SGOutput; const (*var*) maxOffset: wide): ComponentResult; external name '_SGSetOutputMaximumOffset';
{
 *  SGGetOutputMaximumOffset()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGGetOutputMaximumOffset(s: SeqGrabComponent; sgOut: SGOutput; var maxOffset: wide): ComponentResult; external name '_SGGetOutputMaximumOffset';
{
 *  SGGetOutputDataReference()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGGetOutputDataReference(s: SeqGrabComponent; sgOut: SGOutput; var dataRef: Handle; var dataRefType: OSType): ComponentResult; external name '_SGGetOutputDataReference';
{
 *  SGWriteExtendedMovieData()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGWriteExtendedMovieData(s: SeqGrabComponent; c: SGChannel; p: Ptr; len: SInt32; var offset: wide; var sgOut: SGOutput): ComponentResult; external name '_SGWriteExtendedMovieData';
{
 *  SGGetStorageSpaceRemaining64()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function SGGetStorageSpaceRemaining64(s: SeqGrabComponent; var bytes: wide): ComponentResult; external name '_SGGetStorageSpaceRemaining64';
{
 *  SGGetDataOutputStorageSpaceRemaining64()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 5.0 and later
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 5.0 and later
 }
function SGGetDataOutputStorageSpaceRemaining64(s: SeqGrabComponent; sgOut: SGOutput; var space: wide): ComponentResult; external name '_SGGetDataOutputStorageSpaceRemaining64';
{
    calls from Channel to seqGrab
}
{
 *  SGWriteMovieData()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGWriteMovieData(s: SeqGrabComponent; c: SGChannel; p: Ptr; len: SInt32; var offset: SInt32): ComponentResult; external name '_SGWriteMovieData';
{
 *  SGAddFrameReference()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGAddFrameReference(s: SeqGrabComponent; frameInfo: SeqGrabFrameInfoPtr): ComponentResult; external name '_SGAddFrameReference';
{
 *  SGGetNextFrameReference()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGGetNextFrameReference(s: SeqGrabComponent; frameInfo: SeqGrabFrameInfoPtr; var frameDuration: TimeValue; var frameNumber: SInt32): ComponentResult; external name '_SGGetNextFrameReference';
{
 *  SGGetTimeBase()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGGetTimeBase(s: SeqGrabComponent; var tb: TimeBase): ComponentResult; external name '_SGGetTimeBase';
{
 *  SGSortDeviceList()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGSortDeviceList(s: SeqGrabComponent; list: SGDeviceList): ComponentResult; external name '_SGSortDeviceList';
{
 *  SGAddMovieData()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGAddMovieData(s: SeqGrabComponent; c: SGChannel; p: Ptr; len: SInt32; var offset: SInt32; chRefCon: SInt32; time: TimeValue; writeType: SInt16): ComponentResult; external name '_SGAddMovieData';
{
 *  SGChangedSource()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGChangedSource(s: SeqGrabComponent; c: SGChannel): ComponentResult; external name '_SGChangedSource';
{
 *  SGAddExtendedFrameReference()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGAddExtendedFrameReference(s: SeqGrabComponent; frameInfo: SeqGrabExtendedFrameInfoPtr): ComponentResult; external name '_SGAddExtendedFrameReference';
{
 *  SGGetNextExtendedFrameReference()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGGetNextExtendedFrameReference(s: SeqGrabComponent; frameInfo: SeqGrabExtendedFrameInfoPtr; var frameDuration: TimeValue; var frameNumber: SInt32): ComponentResult; external name '_SGGetNextExtendedFrameReference';
{
 *  SGAddExtendedMovieData()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGAddExtendedMovieData(s: SeqGrabComponent; c: SGChannel; p: Ptr; len: SInt32; var offset: wide; chRefCon: SInt32; time: TimeValue; writeType: SInt16; var whichOutput: SGOutput): ComponentResult; external name '_SGAddExtendedMovieData';
{
 *  SGAddOutputDataRefToMedia()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGAddOutputDataRefToMedia(s: SeqGrabComponent; sgOut: SGOutput; theMedia: Media; desc: SampleDescriptionHandle): ComponentResult; external name '_SGAddOutputDataRefToMedia';
{
 *  SGSetSettingsSummary()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 6.0 and later
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.2 and later
 *    Windows:          in qtmlClient.lib 6.0 and later
 }
function SGSetSettingsSummary(s: SeqGrabComponent; summaryText: Handle): ComponentResult; external name '_SGSetSettingsSummary';
{** Sequence Grab CHANNEL Component Stuff **}

{
 *  SGSetChannelUsage()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGSetChannelUsage(c: SGChannel; usage: SInt32): ComponentResult; external name '_SGSetChannelUsage';
{
 *  SGGetChannelUsage()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGGetChannelUsage(c: SGChannel; var usage: SInt32): ComponentResult; external name '_SGGetChannelUsage';
{
 *  SGSetChannelBounds()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGSetChannelBounds(c: SGChannel; const (*var*) bounds: Rect): ComponentResult; external name '_SGSetChannelBounds';
{
 *  SGGetChannelBounds()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGGetChannelBounds(c: SGChannel; var bounds: Rect): ComponentResult; external name '_SGGetChannelBounds';
{
 *  SGSetChannelVolume()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGSetChannelVolume(c: SGChannel; volume: SInt16): ComponentResult; external name '_SGSetChannelVolume';
{
 *  SGGetChannelVolume()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGGetChannelVolume(c: SGChannel; var volume: SInt16): ComponentResult; external name '_SGGetChannelVolume';
{
 *  SGGetChannelInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGGetChannelInfo(c: SGChannel; var channelInfo: SInt32): ComponentResult; external name '_SGGetChannelInfo';
{
 *  SGSetChannelPlayFlags()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGSetChannelPlayFlags(c: SGChannel; playFlags: SInt32): ComponentResult; external name '_SGSetChannelPlayFlags';
{
 *  SGGetChannelPlayFlags()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGGetChannelPlayFlags(c: SGChannel; var playFlags: SInt32): ComponentResult; external name '_SGGetChannelPlayFlags';
{
 *  SGSetChannelMaxFrames()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGSetChannelMaxFrames(c: SGChannel; frameCount: SInt32): ComponentResult; external name '_SGSetChannelMaxFrames';
{
 *  SGGetChannelMaxFrames()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGGetChannelMaxFrames(c: SGChannel; var frameCount: SInt32): ComponentResult; external name '_SGGetChannelMaxFrames';
{
 *  SGSetChannelRefCon()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGSetChannelRefCon(c: SGChannel; refCon: SInt32): ComponentResult; external name '_SGSetChannelRefCon';
{
 *  SGSetChannelClip()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGSetChannelClip(c: SGChannel; theClip: RgnHandle): ComponentResult; external name '_SGSetChannelClip';
{
 *  SGGetChannelClip()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGGetChannelClip(c: SGChannel; var theClip: RgnHandle): ComponentResult; external name '_SGGetChannelClip';
{
 *  SGGetChannelSampleDescription()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGGetChannelSampleDescription(c: SGChannel; sampleDesc: Handle): ComponentResult; external name '_SGGetChannelSampleDescription';
{
 *  SGGetChannelDeviceList()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGGetChannelDeviceList(c: SGChannel; selectionFlags: SInt32; var list: SGDeviceList): ComponentResult; external name '_SGGetChannelDeviceList';
{
 *  SGSetChannelDevice()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGSetChannelDevice(c: SGChannel; name: StringPtr): ComponentResult; external name '_SGSetChannelDevice';
{
 *  SGSetChannelMatrix()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGSetChannelMatrix(c: SGChannel; const (*var*) m: MatrixRecord): ComponentResult; external name '_SGSetChannelMatrix';
{
 *  SGGetChannelMatrix()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGGetChannelMatrix(c: SGChannel; var m: MatrixRecord): ComponentResult; external name '_SGGetChannelMatrix';
{
 *  SGGetChannelTimeScale()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGGetChannelTimeScale(c: SGChannel; var scale: TimeScale): ComponentResult; external name '_SGGetChannelTimeScale';
{
 *  SGChannelPutPicture()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGChannelPutPicture(c: SGChannel): ComponentResult; external name '_SGChannelPutPicture';
{
 *  SGChannelSetRequestedDataRate()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGChannelSetRequestedDataRate(c: SGChannel; bytesPerSecond: SInt32): ComponentResult; external name '_SGChannelSetRequestedDataRate';
{
 *  SGChannelGetRequestedDataRate()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGChannelGetRequestedDataRate(c: SGChannel; var bytesPerSecond: SInt32): ComponentResult; external name '_SGChannelGetRequestedDataRate';
{
 *  SGChannelSetDataSourceName()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGChannelSetDataSourceName(c: SGChannel; const (*var*) name: Str255; scriptTag: ScriptCode): ComponentResult; external name '_SGChannelSetDataSourceName';
{
 *  SGChannelGetDataSourceName()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGChannelGetDataSourceName(c: SGChannel; var name: Str255; var scriptTag: ScriptCode): ComponentResult; external name '_SGChannelGetDataSourceName';
{
 *  SGChannelSetCodecSettings()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function SGChannelSetCodecSettings(c: SGChannel; settings: Handle): ComponentResult; external name '_SGChannelSetCodecSettings';
{
 *  SGChannelGetCodecSettings()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function SGChannelGetCodecSettings(c: SGChannel; var settings: Handle): ComponentResult; external name '_SGChannelGetCodecSettings';
{
 *  SGGetChannelTimeBase()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function SGGetChannelTimeBase(c: SGChannel; var tb: TimeBase): ComponentResult; external name '_SGGetChannelTimeBase';
{
 *  SGGetChannelRefCon()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 6.0 and later
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.2 and later
 *    Windows:          in qtmlClient.lib 6.0 and later
 }
function SGGetChannelRefCon(c: SGChannel; var refCon: SInt32): ComponentResult; external name '_SGGetChannelRefCon';
{ A utility call to find out the current device and input names, instead of having to call GetDeviceList and walk it yourself }
{
 *  SGGetChannelDeviceAndInputNames()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 6.0 and later
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.2 and later
 *    Windows:          in qtmlClient.lib 6.0 and later
 }
function SGGetChannelDeviceAndInputNames(c: SGChannel; var outDeviceName: Str255; var outInputName: Str255; var outInputNumber: SInt16): ComponentResult; external name '_SGGetChannelDeviceAndInputNames';
{ A media format independent call for this. Inputs start at 0 here (Sound starts at 1, VDIGs at 0 in direct calls) }
{
 *  SGSetChannelDeviceInput()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 6.0 and later
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.2 and later
 *    Windows:          in qtmlClient.lib 6.0 and later
 }
function SGSetChannelDeviceInput(c: SGChannel; inInputNumber: SInt16): ComponentResult; external name '_SGSetChannelDeviceInput';
{ A call to bracket SetSettings related calls, to give downstream components an opportunity to deal with the entire 
    settings change in one go }

const
	sgSetSettingsBegin			= $01;							{  SGSetSettings related set calls about to start }
	sgSetSettingsEnd			= $02;							{  Finished SGSetSettings calls. Get ready to use the new settings }

	{
	 *  SGSetChannelSettingsStateChanging()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in QuickTimeLib 6.0 and later
	 *    CarbonLib:        in CarbonLib 1.6 and later
	 *    Mac OS X:         in version 10.2 and later
	 *    Windows:          in qtmlClient.lib 6.0 and later
	 	}
function SGSetChannelSettingsStateChanging(c: SGChannel; inFlags: UInt32): ComponentResult; external name '_SGSetChannelSettingsStateChanging';
{
    calls from seqGrab to Channel
}
{
 *  SGInitChannel()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGInitChannel(c: SGChannel; owner: SeqGrabComponent): ComponentResult; external name '_SGInitChannel';
{
 *  SGWriteSamples()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGWriteSamples(c: SGChannel; m: Movie; theFile: AliasHandle): ComponentResult; external name '_SGWriteSamples';
{
 *  SGGetDataRate()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGGetDataRate(c: SGChannel; var bytesPerSecond: SInt32): ComponentResult; external name '_SGGetDataRate';
{
 *  SGAlignChannelRect()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGAlignChannelRect(c: SGChannel; var r: Rect): ComponentResult; external name '_SGAlignChannelRect';
{
    Dorky dialog panel calls
}
{
 *  SGPanelGetDitl()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGPanelGetDitl(s: SeqGrabComponent; var ditl: Handle): ComponentResult; external name '_SGPanelGetDitl';
{
 *  SGPanelGetTitle()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGPanelGetTitle(s: SeqGrabComponent; var title: Str255): ComponentResult; external name '_SGPanelGetTitle';
{
 *  SGPanelCanRun()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGPanelCanRun(s: SeqGrabComponent; c: SGChannel): ComponentResult; external name '_SGPanelCanRun';
{
 *  SGPanelInstall()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGPanelInstall(s: SeqGrabComponent; c: SGChannel; d: DialogRef; itemOffset: SInt16): ComponentResult; external name '_SGPanelInstall';
{
 *  SGPanelEvent()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGPanelEvent(s: SeqGrabComponent; c: SGChannel; d: DialogRef; itemOffset: SInt16; const (*var*) theEvent: EventRecord; var itemHit: SInt16; var handled: boolean): ComponentResult; external name '_SGPanelEvent';
{
 *  SGPanelItem()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGPanelItem(s: SeqGrabComponent; c: SGChannel; d: DialogRef; itemOffset: SInt16; itemNum: SInt16): ComponentResult; external name '_SGPanelItem';
{
 *  SGPanelRemove()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGPanelRemove(s: SeqGrabComponent; c: SGChannel; d: DialogRef; itemOffset: SInt16): ComponentResult; external name '_SGPanelRemove';
{
 *  SGPanelSetGrabber()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGPanelSetGrabber(s: SeqGrabComponent; sg: SeqGrabComponent): ComponentResult; external name '_SGPanelSetGrabber';
{
 *  SGPanelSetResFile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGPanelSetResFile(s: SeqGrabComponent; resRef: SInt16): ComponentResult; external name '_SGPanelSetResFile';
{
 *  SGPanelGetSettings()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGPanelGetSettings(s: SeqGrabComponent; c: SGChannel; var ud: UserData; flags: SInt32): ComponentResult; external name '_SGPanelGetSettings';
{
 *  SGPanelSetSettings()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGPanelSetSettings(s: SeqGrabComponent; c: SGChannel; ud: UserData; flags: SInt32): ComponentResult; external name '_SGPanelSetSettings';
{
 *  SGPanelValidateInput()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGPanelValidateInput(s: SeqGrabComponent; var ok: boolean): ComponentResult; external name '_SGPanelValidateInput';
{
 *  SGPanelSetEventFilter()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGPanelSetEventFilter(s: SeqGrabComponent; proc: SGModalFilterUPP; refCon: SInt32): ComponentResult; external name '_SGPanelSetEventFilter';
{
    SGPanelGetDITLForSize is used to retrieve user interface elements that fit within a specified size
    panel.  The component should return badComponentSelector for sizes it does not support.  The component
    is required to support kSGSmallestDITLSize, and it is recommended to support kSGLargestDITLSize.
    
    If SGPanelGetDITLForSize is unimplemented entirely, the panel is assumed to not have resizable UI elements.
}

const
	kSGSmallestDITLSize			= -1;							{  requestedSize h and v set to this to retrieve small size }
	kSGLargestDITLSize			= -2;							{  requestedSize h and v set to this to retrieve large size }

	{
	 *  SGPanelGetDITLForSize()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in QuickTimeLib 6.0 and later
	 *    CarbonLib:        in CarbonLib 1.6 and later
	 *    Mac OS X:         in version 10.2 and later
	 *    Windows:          in qtmlClient.lib 6.0 and later
	 	}
function SGPanelGetDITLForSize(s: SeqGrabComponent; var ditl: Handle; var requestedSize: Point): ComponentResult; external name '_SGPanelGetDITLForSize';
{** Sequence Grab VIDEO CHANNEL Component Stuff **}
{
    Video stuff
}

type
	SGCompressInfoPtr = ^SGCompressInfo;
	SGCompressInfo = record
		buffer:					Ptr;
		bufferSize:				UInt32;
		similarity:				SInt8;
		reserved:				SInt8;
	end;

{$ifc TYPED_FUNCTION_POINTERS}
	SGGrabBottleProcPtr = function(c: SGChannel; bufferNum: SInt16; refCon: SInt32): ComponentResult;
{$elsec}
	SGGrabBottleProcPtr = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	SGGrabCompleteBottleProcPtr = function(c: SGChannel; bufferNum: SInt16; var done: boolean; refCon: SInt32): ComponentResult;
{$elsec}
	SGGrabCompleteBottleProcPtr = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	SGDisplayBottleProcPtr = function(c: SGChannel; bufferNum: SInt16; var mp: MatrixRecord; clipRgn: RgnHandle; refCon: SInt32): ComponentResult;
{$elsec}
	SGDisplayBottleProcPtr = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	SGCompressBottleProcPtr = function(c: SGChannel; bufferNum: SInt16; refCon: SInt32): ComponentResult;
{$elsec}
	SGCompressBottleProcPtr = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	SGCompressCompleteBottleProcPtr = function(c: SGChannel; bufferNum: SInt16; var done: boolean; var ci: SGCompressInfo; refCon: SInt32): ComponentResult;
{$elsec}
	SGCompressCompleteBottleProcPtr = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	SGAddFrameBottleProcPtr = function(c: SGChannel; bufferNum: SInt16; atTime: TimeValue; scale: TimeScale; const (*var*) ci: SGCompressInfo; refCon: SInt32): ComponentResult;
{$elsec}
	SGAddFrameBottleProcPtr = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	SGTransferFrameBottleProcPtr = function(c: SGChannel; bufferNum: SInt16; var mp: MatrixRecord; clipRgn: RgnHandle; refCon: SInt32): ComponentResult;
{$elsec}
	SGTransferFrameBottleProcPtr = ProcPtr;
{$endc}

	{	    Note that UInt8 *queuedFrameCount replaces Boolean *done. 0(==false) still means no frames, and 1(==true) one, 
	    but if more than one are available the number should be returned here. The value 2 previously meant more than one frame,
	    so some VDIGs may return 2 even if more than 2 are available, and some will still return 1 as they are using the original definition. 	}
{$ifc TYPED_FUNCTION_POINTERS}
	SGGrabCompressCompleteBottleProcPtr = function(c: SGChannel; var queuedFrameCount: UInt8; var ci: SGCompressInfo; var t: TimeRecord; refCon: SInt32): ComponentResult;
{$elsec}
	SGGrabCompressCompleteBottleProcPtr = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	SGDisplayCompressBottleProcPtr = function(c: SGChannel; dataPtr: Ptr; desc: ImageDescriptionHandle; var mp: MatrixRecord; clipRgn: RgnHandle; refCon: SInt32): ComponentResult;
{$elsec}
	SGDisplayCompressBottleProcPtr = ProcPtr;
{$endc}

{$ifc OPAQUE_UPP_TYPES}
	SGGrabBottleUPP = ^SInt32; { an opaque UPP }
{$elsec}
	SGGrabBottleUPP = UniversalProcPtr;
{$endc}	
{$ifc OPAQUE_UPP_TYPES}
	SGGrabCompleteBottleUPP = ^SInt32; { an opaque UPP }
{$elsec}
	SGGrabCompleteBottleUPP = UniversalProcPtr;
{$endc}	
{$ifc OPAQUE_UPP_TYPES}
	SGDisplayBottleUPP = ^SInt32; { an opaque UPP }
{$elsec}
	SGDisplayBottleUPP = UniversalProcPtr;
{$endc}	
{$ifc OPAQUE_UPP_TYPES}
	SGCompressBottleUPP = ^SInt32; { an opaque UPP }
{$elsec}
	SGCompressBottleUPP = UniversalProcPtr;
{$endc}	
{$ifc OPAQUE_UPP_TYPES}
	SGCompressCompleteBottleUPP = ^SInt32; { an opaque UPP }
{$elsec}
	SGCompressCompleteBottleUPP = UniversalProcPtr;
{$endc}	
{$ifc OPAQUE_UPP_TYPES}
	SGAddFrameBottleUPP = ^SInt32; { an opaque UPP }
{$elsec}
	SGAddFrameBottleUPP = UniversalProcPtr;
{$endc}	
{$ifc OPAQUE_UPP_TYPES}
	SGTransferFrameBottleUPP = ^SInt32; { an opaque UPP }
{$elsec}
	SGTransferFrameBottleUPP = UniversalProcPtr;
{$endc}	
{$ifc OPAQUE_UPP_TYPES}
	SGGrabCompressCompleteBottleUPP = ^SInt32; { an opaque UPP }
{$elsec}
	SGGrabCompressCompleteBottleUPP = UniversalProcPtr;
{$endc}	
{$ifc OPAQUE_UPP_TYPES}
	SGDisplayCompressBottleUPP = ^SInt32; { an opaque UPP }
{$elsec}
	SGDisplayCompressBottleUPP = UniversalProcPtr;
{$endc}	
	VideoBottlesPtr = ^VideoBottles;
	VideoBottles = record
		procCount:				SInt16;
		grabProc:				SGGrabBottleUPP;
		grabCompleteProc:		SGGrabCompleteBottleUPP;
		displayProc:			SGDisplayBottleUPP;
		compressProc:			SGCompressBottleUPP;
		compressCompleteProc:	SGCompressCompleteBottleUPP;
		addFrameProc:			SGAddFrameBottleUPP;
		transferFrameProc:		SGTransferFrameBottleUPP;
		grabCompressCompleteProc: SGGrabCompressCompleteBottleUPP;
		displayCompressProc:	SGDisplayCompressBottleUPP;
	end;

	{
	 *  SGGetSrcVideoBounds()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 *    Windows:          in qtmlClient.lib 3.0 and later
	 	}
function SGGetSrcVideoBounds(c: SGChannel; var r: Rect): ComponentResult; external name '_SGGetSrcVideoBounds';
{
 *  SGSetVideoRect()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGSetVideoRect(c: SGChannel; const (*var*) r: Rect): ComponentResult; external name '_SGSetVideoRect';
{
 *  SGGetVideoRect()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGGetVideoRect(c: SGChannel; var r: Rect): ComponentResult; external name '_SGGetVideoRect';
{
 *  SGGetVideoCompressorType()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGGetVideoCompressorType(c: SGChannel; var compressorType: OSType): ComponentResult; external name '_SGGetVideoCompressorType';
{
 *  SGSetVideoCompressorType()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGSetVideoCompressorType(c: SGChannel; compressorType: OSType): ComponentResult; external name '_SGSetVideoCompressorType';
{
 *  SGSetVideoCompressor()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGSetVideoCompressor(c: SGChannel; depth: SInt16; compressor: CompressorComponent; spatialQuality: CodecQ; temporalQuality: CodecQ; keyFrameRate: SInt32): ComponentResult; external name '_SGSetVideoCompressor';
{
 *  SGGetVideoCompressor()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGGetVideoCompressor(c: SGChannel; var depth: SInt16; var compressor: CompressorComponent; var spatialQuality: CodecQ; var temporalQuality: CodecQ; var keyFrameRate: SInt32): ComponentResult; external name '_SGGetVideoCompressor';
{
 *  SGGetVideoDigitizerComponent()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGGetVideoDigitizerComponent(c: SGChannel): ComponentInstance; external name '_SGGetVideoDigitizerComponent';
{
 *  SGSetVideoDigitizerComponent()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGSetVideoDigitizerComponent(c: SGChannel; vdig: ComponentInstance): ComponentResult; external name '_SGSetVideoDigitizerComponent';
{
 *  SGVideoDigitizerChanged()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGVideoDigitizerChanged(c: SGChannel): ComponentResult; external name '_SGVideoDigitizerChanged';
{
 *  SGSetVideoBottlenecks()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGSetVideoBottlenecks(c: SGChannel; var vb: VideoBottles): ComponentResult; external name '_SGSetVideoBottlenecks';
{
 *  SGGetVideoBottlenecks()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGGetVideoBottlenecks(c: SGChannel; var vb: VideoBottles): ComponentResult; external name '_SGGetVideoBottlenecks';
{
 *  SGGrabFrame()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGGrabFrame(c: SGChannel; bufferNum: SInt16): ComponentResult; external name '_SGGrabFrame';
{
 *  SGGrabFrameComplete()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGGrabFrameComplete(c: SGChannel; bufferNum: SInt16; var done: boolean): ComponentResult; external name '_SGGrabFrameComplete';
{
 *  SGDisplayFrame()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGDisplayFrame(c: SGChannel; bufferNum: SInt16; const (*var*) mp: MatrixRecord; clipRgn: RgnHandle): ComponentResult; external name '_SGDisplayFrame';
{
 *  SGCompressFrame()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGCompressFrame(c: SGChannel; bufferNum: SInt16): ComponentResult; external name '_SGCompressFrame';
{
 *  SGCompressFrameComplete()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGCompressFrameComplete(c: SGChannel; bufferNum: SInt16; var done: boolean; var ci: SGCompressInfo): ComponentResult; external name '_SGCompressFrameComplete';
{
 *  SGAddFrame()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGAddFrame(c: SGChannel; bufferNum: SInt16; atTime: TimeValue; scale: TimeScale; const (*var*) ci: SGCompressInfo): ComponentResult; external name '_SGAddFrame';
{
 *  SGTransferFrameForCompress()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGTransferFrameForCompress(c: SGChannel; bufferNum: SInt16; const (*var*) mp: MatrixRecord; clipRgn: RgnHandle): ComponentResult; external name '_SGTransferFrameForCompress';
{
 *  SGSetCompressBuffer()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGSetCompressBuffer(c: SGChannel; depth: SInt16; const (*var*) compressSize: Rect): ComponentResult; external name '_SGSetCompressBuffer';
{
 *  SGGetCompressBuffer()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGGetCompressBuffer(c: SGChannel; var depth: SInt16; var compressSize: Rect): ComponentResult; external name '_SGGetCompressBuffer';
{
 *  SGGetBufferInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGGetBufferInfo(c: SGChannel; bufferNum: SInt16; var bufferPM: PixMapHandle; var bufferRect: Rect; var compressBuffer: GWorldPtr; var compressBufferRect: Rect): ComponentResult; external name '_SGGetBufferInfo';
{
 *  SGSetUseScreenBuffer()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGSetUseScreenBuffer(c: SGChannel; useScreenBuffer: boolean): ComponentResult; external name '_SGSetUseScreenBuffer';
{
 *  SGGetUseScreenBuffer()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGGetUseScreenBuffer(c: SGChannel; var useScreenBuffer: boolean): ComponentResult; external name '_SGGetUseScreenBuffer';
{    Note that UInt8 *queuedFrameCount replaces Boolean *done. 0(==false) still means no frames, and 1(==true) one, 
    but if more than one are available the number should be returned here. The value 2 previously meant more than one frame,
    so some VDIGs may return 2 even if more than 2 are available, and some will still return 1 as they are using the original definition. }
{
 *  SGGrabCompressComplete()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGGrabCompressComplete(c: SGChannel; var queuedFrameCount: UInt8; var ci: SGCompressInfo; var tr: TimeRecord): ComponentResult; external name '_SGGrabCompressComplete';
{
 *  SGDisplayCompress()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGDisplayCompress(c: SGChannel; dataPtr: Ptr; desc: ImageDescriptionHandle; var mp: MatrixRecord; clipRgn: RgnHandle): ComponentResult; external name '_SGDisplayCompress';
{
 *  SGSetFrameRate()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGSetFrameRate(c: SGChannel; frameRate: Fixed): ComponentResult; external name '_SGSetFrameRate';
{
 *  SGGetFrameRate()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGGetFrameRate(c: SGChannel; var frameRate: Fixed): ComponentResult; external name '_SGGetFrameRate';
{
 *  SGSetPreferredPacketSize()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGSetPreferredPacketSize(c: SGChannel; preferredPacketSizeInBytes: SInt32): ComponentResult; external name '_SGSetPreferredPacketSize';
{
 *  SGGetPreferredPacketSize()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGGetPreferredPacketSize(c: SGChannel; var preferredPacketSizeInBytes: SInt32): ComponentResult; external name '_SGGetPreferredPacketSize';
{
 *  SGSetUserVideoCompressorList()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGSetUserVideoCompressorList(c: SGChannel; compressorTypes: Handle): ComponentResult; external name '_SGSetUserVideoCompressorList';
{
 *  SGGetUserVideoCompressorList()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGGetUserVideoCompressorList(c: SGChannel; var compressorTypes: Handle): ComponentResult; external name '_SGGetUserVideoCompressorList';
{** Sequence Grab SOUND CHANNEL Component Stuff **}

{
    Sound stuff
}
{
 *  SGSetSoundInputDriver()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGSetSoundInputDriver(c: SGChannel; const (*var*) driverName: Str255): ComponentResult; external name '_SGSetSoundInputDriver';
{
 *  SGGetSoundInputDriver()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGGetSoundInputDriver(c: SGChannel): SInt32; external name '_SGGetSoundInputDriver';
{
 *  SGSoundInputDriverChanged()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGSoundInputDriverChanged(c: SGChannel): ComponentResult; external name '_SGSoundInputDriverChanged';
{
 *  SGSetSoundRecordChunkSize()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGSetSoundRecordChunkSize(c: SGChannel; seconds: SInt32): ComponentResult; external name '_SGSetSoundRecordChunkSize';
{
 *  SGGetSoundRecordChunkSize()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGGetSoundRecordChunkSize(c: SGChannel): SInt32; external name '_SGGetSoundRecordChunkSize';
{
 *  SGSetSoundInputRate()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGSetSoundInputRate(c: SGChannel; rate: Fixed): ComponentResult; external name '_SGSetSoundInputRate';
{
 *  SGGetSoundInputRate()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGGetSoundInputRate(c: SGChannel): Fixed; external name '_SGGetSoundInputRate';
{
 *  SGSetSoundInputParameters()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGSetSoundInputParameters(c: SGChannel; sampleSize: SInt16; numChannels: SInt16; compressionType: OSType): ComponentResult; external name '_SGSetSoundInputParameters';
{
 *  SGGetSoundInputParameters()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGGetSoundInputParameters(c: SGChannel; var sampleSize: SInt16; var numChannels: SInt16; var compressionType: OSType): ComponentResult; external name '_SGGetSoundInputParameters';
{
 *  SGSetAdditionalSoundRates()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGSetAdditionalSoundRates(c: SGChannel; rates: Handle): ComponentResult; external name '_SGSetAdditionalSoundRates';
{
 *  SGGetAdditionalSoundRates()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGGetAdditionalSoundRates(c: SGChannel; var rates: Handle): ComponentResult; external name '_SGGetAdditionalSoundRates';
{
    Text stuff
}
{
 *  SGSetFontName()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGSetFontName(c: SGChannel; pstr: StringPtr): ComponentResult; external name '_SGSetFontName';
{
 *  SGSetFontSize()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGSetFontSize(c: SGChannel; fontSize: SInt16): ComponentResult; external name '_SGSetFontSize';
{
 *  SGSetTextForeColor()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGSetTextForeColor(c: SGChannel; var theColor: RGBColor): ComponentResult; external name '_SGSetTextForeColor';
{
 *  SGSetTextBackColor()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGSetTextBackColor(c: SGChannel; var theColor: RGBColor): ComponentResult; external name '_SGSetTextBackColor';
{
 *  SGSetJustification()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGSetJustification(c: SGChannel; just: SInt16): ComponentResult; external name '_SGSetJustification';
{
 *  SGGetTextReturnToSpaceValue()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGGetTextReturnToSpaceValue(c: SGChannel; var rettospace: SInt16): ComponentResult; external name '_SGGetTextReturnToSpaceValue';
{
 *  SGSetTextReturnToSpaceValue()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGSetTextReturnToSpaceValue(c: SGChannel; rettospace: SInt16): ComponentResult; external name '_SGSetTextReturnToSpaceValue';
{
    Music stuff
}
{
 *  SGGetInstrument()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGGetInstrument(c: SGChannel; var td: ToneDescription): ComponentResult; external name '_SGGetInstrument';
{
 *  SGSetInstrument()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SGSetInstrument(c: SGChannel; var td: ToneDescription): ComponentResult; external name '_SGSetInstrument';
const
	sgChannelAtom				= $6368616E (* 'chan' *);
	sgChannelSettingsAtom		= $63746F6D (* 'ctom' *);
	sgChannelDescription		= $63647363 (* 'cdsc' *);
	sgChannelSettings			= $63736574 (* 'cset' *);

	sgDeviceNameType			= $6E616D65 (* 'name' *);
	sgDeviceDisplayNameType		= $646E616D (* 'dnam' *);
	sgDeviceUIDType				= $64756964 (* 'duid' *);
	sgInputUIDType				= $69756964 (* 'iuid' *);
	sgUsageType					= $75736520 (* 'use ' *);
	sgPlayFlagsType				= $706C7966 (* 'plyf' *);
	sgClipType					= $636C6970 (* 'clip' *);
	sgMatrixType				= $6D747278 (* 'mtrx' *);
	sgVolumeType				= $766F6C75 (* 'volu' *);

	sgPanelSettingsAtom			= $70746F6D (* 'ptom' *);
	sgPanelDescription			= $70647363 (* 'pdsc' *);
	sgPanelSettings				= $70736574 (* 'pset' *);

	sgcSoundCompressionType		= $73636D70 (* 'scmp' *);
	sgcSoundCodecSettingsType	= $63646563 (* 'cdec' *);
	sgcSoundSampleRateType		= $73726174 (* 'srat' *);
	sgcSoundChannelCountType	= $7363686E (* 'schn' *);
	sgcSoundSampleSizeType		= $7373697A (* 'ssiz' *);
	sgcSoundInputType			= $73696E70 (* 'sinp' *);
	sgcSoundGainType			= $6761696E (* 'gain' *);

	sgcVideoHueType				= $68756520 (* 'hue ' *);
	sgcVideoSaturationType		= $73617472 (* 'satr' *);
	sgcVideoContrastType		= $74727374 (* 'trst' *);
	sgcVideoSharpnessType		= $73687270 (* 'shrp' *);
	sgcVideoBrigtnessType		= $62726974 (* 'brit' *);
	sgcVideoBlackLevelType		= $626C6B6C (* 'blkl' *);
	sgcVideoWhiteLevelType		= $7768746C (* 'whtl' *);
	sgcVideoInputType			= $76696E70 (* 'vinp' *);
	sgcVideoFormatType			= $76737464 (* 'vstd' *);
	sgcVideoFilterType			= $76666C74 (* 'vflt' *);
	sgcVideoRectType			= $76726374 (* 'vrct' *);
	sgcVideoDigitizerType		= $76646967 (* 'vdig' *);


type
	QTVideoOutputComponent				= ComponentInstance;
	{  Component type and subtype enumerations }

const
	QTVideoOutputComponentType	= $766F7574 (* 'vout' *);
	QTVideoOutputComponentBaseSubType = $62617365 (* 'base' *);


	{  QTVideoOutput Component flags }

	kQTVideoOutputDontDisplayToUser = $00000001;

	{  Display mode atom types }

	kQTVODisplayModeItem		= $71646D69 (* 'qdmi' *);
	kQTVODimensions				= $64696D6E (* 'dimn' *);						{  atom contains two longs - pixel count - width, height }
	kQTVOResolution				= $7265736C (* 'resl' *);						{  atom contains two Fixed - hRes, vRes in dpi }
	kQTVORefreshRate			= $72656672 (* 'refr' *);						{  atom contains one Fixed - refresh rate in Hz }
	kQTVOPixelType				= $7069786C (* 'pixl' *);						{  atom contains one OSType - pixel format of mode }
	kQTVOName					= $6E616D65 (* 'name' *);						{  atom contains string (no length byte) - name of mode for display to user }
	kQTVODecompressors			= $6465636F (* 'deco' *);						{  atom contains other atoms indicating supported decompressors }
																{  kQTVODecompressors sub-atoms }
	kQTVODecompressorType		= $64657479 (* 'dety' *);						{  atom contains one OSType - decompressor type code }
	kQTVODecompressorContinuous	= $636F6E74 (* 'cont' *);						{  atom contains one Boolean - true if this type is displayed continuously }
	kQTVODecompressorComponent	= $636D7074 (* 'cmpt' *);						{  atom contains one Component - component id of decompressor }

	{	* These are QTVideoOutput procedures *	}
	{
	 *  QTVideoOutputGetDisplayModeList()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 *    Windows:          in qtmlClient.lib 3.0 and later
	 	}
function QTVideoOutputGetDisplayModeList(vo: QTVideoOutputComponent; var outputs: QTAtomContainer): ComponentResult; external name '_QTVideoOutputGetDisplayModeList';
{
 *  QTVideoOutputGetCurrentClientName()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function QTVideoOutputGetCurrentClientName(vo: QTVideoOutputComponent; var str: Str255): ComponentResult; external name '_QTVideoOutputGetCurrentClientName';
{
 *  QTVideoOutputSetClientName()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function QTVideoOutputSetClientName(vo: QTVideoOutputComponent; const (*var*) str: Str255): ComponentResult; external name '_QTVideoOutputSetClientName';
{
 *  QTVideoOutputGetClientName()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function QTVideoOutputGetClientName(vo: QTVideoOutputComponent; var str: Str255): ComponentResult; external name '_QTVideoOutputGetClientName';
{
 *  QTVideoOutputBegin()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function QTVideoOutputBegin(vo: QTVideoOutputComponent): ComponentResult; external name '_QTVideoOutputBegin';
{
 *  QTVideoOutputEnd()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function QTVideoOutputEnd(vo: QTVideoOutputComponent): ComponentResult; external name '_QTVideoOutputEnd';
{
 *  QTVideoOutputSetDisplayMode()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function QTVideoOutputSetDisplayMode(vo: QTVideoOutputComponent; displayModeID: SInt32): ComponentResult; external name '_QTVideoOutputSetDisplayMode';
{
 *  QTVideoOutputGetDisplayMode()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function QTVideoOutputGetDisplayMode(vo: QTVideoOutputComponent; var displayModeID: SInt32): ComponentResult; external name '_QTVideoOutputGetDisplayMode';
{
 *  QTVideoOutputCustomConfigureDisplay()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function QTVideoOutputCustomConfigureDisplay(vo: QTVideoOutputComponent; filter: ModalFilterUPP): ComponentResult; external name '_QTVideoOutputCustomConfigureDisplay';
{
 *  QTVideoOutputSaveState()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function QTVideoOutputSaveState(vo: QTVideoOutputComponent; var state: QTAtomContainer): ComponentResult; external name '_QTVideoOutputSaveState';
{
 *  QTVideoOutputRestoreState()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function QTVideoOutputRestoreState(vo: QTVideoOutputComponent; state: QTAtomContainer): ComponentResult; external name '_QTVideoOutputRestoreState';
{
 *  QTVideoOutputGetGWorld()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function QTVideoOutputGetGWorld(vo: QTVideoOutputComponent; var gw: GWorldPtr): ComponentResult; external name '_QTVideoOutputGetGWorld';
{
 *  QTVideoOutputGetGWorldParameters()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function QTVideoOutputGetGWorldParameters(vo: QTVideoOutputComponent; var baseAddr: Ptr; var rowBytes: SInt32; var colorTable: CTabHandle): ComponentResult; external name '_QTVideoOutputGetGWorldParameters';
{
 *  QTVideoOutputGetIndSoundOutput()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function QTVideoOutputGetIndSoundOutput(vo: QTVideoOutputComponent; index: SInt32; var outputComponent: Component): ComponentResult; external name '_QTVideoOutputGetIndSoundOutput';
{
 *  QTVideoOutputGetClock()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function QTVideoOutputGetClock(vo: QTVideoOutputComponent; var clock: ComponentInstance): ComponentResult; external name '_QTVideoOutputGetClock';
{
 *  QTVideoOutputSetEchoPort()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function QTVideoOutputSetEchoPort(vo: QTVideoOutputComponent; echoPort: CGrafPtr): ComponentResult; external name '_QTVideoOutputSetEchoPort';
{
 *  QTVideoOutputGetIndImageDecompressor()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 5.0 and later
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 5.0 and later
 }
function QTVideoOutputGetIndImageDecompressor(vo: QTVideoOutputComponent; index: SInt32; var codec: Component): ComponentResult; external name '_QTVideoOutputGetIndImageDecompressor';
{
 *  QTVideoOutputBaseSetEchoPort()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 6.0 and later
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.2 and later
 *    Windows:          in qtmlClient.lib 6.0 and later
 }
function QTVideoOutputBaseSetEchoPort(vo: QTVideoOutputComponent; echoPort: CGrafPtr): ComponentResult; external name '_QTVideoOutputBaseSetEchoPort';
{ UPP call backs }

const
	uppDataHCompletionProcInfo = $00000BC0;
	uppVdigIntProcInfo = $000003C0;
	uppStartDocumentHandlerProcInfo = $000000F0;
	uppEndDocumentHandlerProcInfo = $000000F0;
	uppStartElementHandlerProcInfo = $00000FF0;
	uppEndElementHandlerProcInfo = $000003F0;
	uppCharDataHandlerProcInfo = $000003F0;
	uppPreprocessInstructionHandlerProcInfo = $00000FF0;
	uppCommentHandlerProcInfo = $000003F0;
	uppCDataHandlerProcInfo = $000003F0;
	uppSGDataProcInfo = $003BFFE0;
	uppSGModalFilterProcInfo = $00003FD0;
	uppSGGrabBottleProcInfo = $00000EF0;
	uppSGGrabCompleteBottleProcInfo = $00003EF0;
	uppSGDisplayBottleProcInfo = $0000FEF0;
	uppSGCompressBottleProcInfo = $00000EF0;
	uppSGCompressCompleteBottleProcInfo = $0000FEF0;
	uppSGAddFrameBottleProcInfo = $0003FEF0;
	uppSGTransferFrameBottleProcInfo = $0000FEF0;
	uppSGGrabCompressCompleteBottleProcInfo = $0000FFF0;
	uppSGDisplayCompressBottleProcInfo = $0003FFF0;
	{
	 *  NewDataHCompletionUPP()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   available as macro/inline
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
function NewDataHCompletionUPP(userRoutine: DataHCompletionProcPtr): DataHCompletionUPP; external name '_NewDataHCompletionUPP'; { old name was NewDataHCompletionProc }
{
 *  NewVdigIntUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewVdigIntUPP(userRoutine: VdigIntProcPtr): VdigIntUPP; external name '_NewVdigIntUPP'; { old name was NewVdigIntProc }
{
 *  NewStartDocumentHandlerUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewStartDocumentHandlerUPP(userRoutine: StartDocumentHandler): StartDocumentHandlerUPP; external name '_NewStartDocumentHandlerUPP'; { old name was NewStartDocumentHandlerProc }
{
 *  NewEndDocumentHandlerUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewEndDocumentHandlerUPP(userRoutine: EndDocumentHandler): EndDocumentHandlerUPP; external name '_NewEndDocumentHandlerUPP'; { old name was NewEndDocumentHandlerProc }
{
 *  NewStartElementHandlerUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewStartElementHandlerUPP(userRoutine: StartElementHandler): StartElementHandlerUPP; external name '_NewStartElementHandlerUPP'; { old name was NewStartElementHandlerProc }
{
 *  NewEndElementHandlerUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewEndElementHandlerUPP(userRoutine: EndElementHandler): EndElementHandlerUPP; external name '_NewEndElementHandlerUPP'; { old name was NewEndElementHandlerProc }
{
 *  NewCharDataHandlerUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewCharDataHandlerUPP(userRoutine: CharDataHandler): CharDataHandlerUPP; external name '_NewCharDataHandlerUPP'; { old name was NewCharDataHandlerProc }
{
 *  NewPreprocessInstructionHandlerUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewPreprocessInstructionHandlerUPP(userRoutine: PreprocessInstructionHandler): PreprocessInstructionHandlerUPP; external name '_NewPreprocessInstructionHandlerUPP'; { old name was NewPreprocessInstructionHandlerProc }
{
 *  NewCommentHandlerUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewCommentHandlerUPP(userRoutine: CommentHandler): CommentHandlerUPP; external name '_NewCommentHandlerUPP'; { old name was NewCommentHandlerProc }
{
 *  NewCDataHandlerUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.2 and later
 }
function NewCDataHandlerUPP(userRoutine: CDataHandler): CDataHandlerUPP; external name '_NewCDataHandlerUPP'; { old name was NewCDataHandlerProc }
{
 *  NewSGDataUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewSGDataUPP(userRoutine: SGDataProcPtr): SGDataUPP; external name '_NewSGDataUPP'; { old name was NewSGDataProc }
{
 *  NewSGModalFilterUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewSGModalFilterUPP(userRoutine: SGModalFilterProcPtr): SGModalFilterUPP; external name '_NewSGModalFilterUPP'; { old name was NewSGModalFilterProc }
{
 *  NewSGGrabBottleUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewSGGrabBottleUPP(userRoutine: SGGrabBottleProcPtr): SGGrabBottleUPP; external name '_NewSGGrabBottleUPP'; { old name was NewSGGrabBottleProc }
{
 *  NewSGGrabCompleteBottleUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewSGGrabCompleteBottleUPP(userRoutine: SGGrabCompleteBottleProcPtr): SGGrabCompleteBottleUPP; external name '_NewSGGrabCompleteBottleUPP'; { old name was NewSGGrabCompleteBottleProc }
{
 *  NewSGDisplayBottleUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewSGDisplayBottleUPP(userRoutine: SGDisplayBottleProcPtr): SGDisplayBottleUPP; external name '_NewSGDisplayBottleUPP'; { old name was NewSGDisplayBottleProc }
{
 *  NewSGCompressBottleUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewSGCompressBottleUPP(userRoutine: SGCompressBottleProcPtr): SGCompressBottleUPP; external name '_NewSGCompressBottleUPP'; { old name was NewSGCompressBottleProc }
{
 *  NewSGCompressCompleteBottleUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewSGCompressCompleteBottleUPP(userRoutine: SGCompressCompleteBottleProcPtr): SGCompressCompleteBottleUPP; external name '_NewSGCompressCompleteBottleUPP'; { old name was NewSGCompressCompleteBottleProc }
{
 *  NewSGAddFrameBottleUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewSGAddFrameBottleUPP(userRoutine: SGAddFrameBottleProcPtr): SGAddFrameBottleUPP; external name '_NewSGAddFrameBottleUPP'; { old name was NewSGAddFrameBottleProc }
{
 *  NewSGTransferFrameBottleUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewSGTransferFrameBottleUPP(userRoutine: SGTransferFrameBottleProcPtr): SGTransferFrameBottleUPP; external name '_NewSGTransferFrameBottleUPP'; { old name was NewSGTransferFrameBottleProc }
{
 *  NewSGGrabCompressCompleteBottleUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewSGGrabCompressCompleteBottleUPP(userRoutine: SGGrabCompressCompleteBottleProcPtr): SGGrabCompressCompleteBottleUPP; external name '_NewSGGrabCompressCompleteBottleUPP'; { old name was NewSGGrabCompressCompleteBottleProc }
{
 *  NewSGDisplayCompressBottleUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewSGDisplayCompressBottleUPP(userRoutine: SGDisplayCompressBottleProcPtr): SGDisplayCompressBottleUPP; external name '_NewSGDisplayCompressBottleUPP'; { old name was NewSGDisplayCompressBottleProc }
{
 *  DisposeDataHCompletionUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeDataHCompletionUPP(userUPP: DataHCompletionUPP); external name '_DisposeDataHCompletionUPP';
{
 *  DisposeVdigIntUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeVdigIntUPP(userUPP: VdigIntUPP); external name '_DisposeVdigIntUPP';
{
 *  DisposeStartDocumentHandlerUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeStartDocumentHandlerUPP(userUPP: StartDocumentHandlerUPP); external name '_DisposeStartDocumentHandlerUPP';
{
 *  DisposeEndDocumentHandlerUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeEndDocumentHandlerUPP(userUPP: EndDocumentHandlerUPP); external name '_DisposeEndDocumentHandlerUPP';
{
 *  DisposeStartElementHandlerUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeStartElementHandlerUPP(userUPP: StartElementHandlerUPP); external name '_DisposeStartElementHandlerUPP';
{
 *  DisposeEndElementHandlerUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeEndElementHandlerUPP(userUPP: EndElementHandlerUPP); external name '_DisposeEndElementHandlerUPP';
{
 *  DisposeCharDataHandlerUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeCharDataHandlerUPP(userUPP: CharDataHandlerUPP); external name '_DisposeCharDataHandlerUPP';
{
 *  DisposePreprocessInstructionHandlerUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposePreprocessInstructionHandlerUPP(userUPP: PreprocessInstructionHandlerUPP); external name '_DisposePreprocessInstructionHandlerUPP';
{
 *  DisposeCommentHandlerUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeCommentHandlerUPP(userUPP: CommentHandlerUPP); external name '_DisposeCommentHandlerUPP';
{
 *  DisposeCDataHandlerUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.2 and later
 }
procedure DisposeCDataHandlerUPP(userUPP: CDataHandlerUPP); external name '_DisposeCDataHandlerUPP';
{
 *  DisposeSGDataUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeSGDataUPP(userUPP: SGDataUPP); external name '_DisposeSGDataUPP';
{
 *  DisposeSGModalFilterUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeSGModalFilterUPP(userUPP: SGModalFilterUPP); external name '_DisposeSGModalFilterUPP';
{
 *  DisposeSGGrabBottleUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeSGGrabBottleUPP(userUPP: SGGrabBottleUPP); external name '_DisposeSGGrabBottleUPP';
{
 *  DisposeSGGrabCompleteBottleUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeSGGrabCompleteBottleUPP(userUPP: SGGrabCompleteBottleUPP); external name '_DisposeSGGrabCompleteBottleUPP';
{
 *  DisposeSGDisplayBottleUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeSGDisplayBottleUPP(userUPP: SGDisplayBottleUPP); external name '_DisposeSGDisplayBottleUPP';
{
 *  DisposeSGCompressBottleUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeSGCompressBottleUPP(userUPP: SGCompressBottleUPP); external name '_DisposeSGCompressBottleUPP';
{
 *  DisposeSGCompressCompleteBottleUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeSGCompressCompleteBottleUPP(userUPP: SGCompressCompleteBottleUPP); external name '_DisposeSGCompressCompleteBottleUPP';
{
 *  DisposeSGAddFrameBottleUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeSGAddFrameBottleUPP(userUPP: SGAddFrameBottleUPP); external name '_DisposeSGAddFrameBottleUPP';
{
 *  DisposeSGTransferFrameBottleUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeSGTransferFrameBottleUPP(userUPP: SGTransferFrameBottleUPP); external name '_DisposeSGTransferFrameBottleUPP';
{
 *  DisposeSGGrabCompressCompleteBottleUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeSGGrabCompressCompleteBottleUPP(userUPP: SGGrabCompressCompleteBottleUPP); external name '_DisposeSGGrabCompressCompleteBottleUPP';
{
 *  DisposeSGDisplayCompressBottleUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeSGDisplayCompressBottleUPP(userUPP: SGDisplayCompressBottleUPP); external name '_DisposeSGDisplayCompressBottleUPP';
{
 *  InvokeDataHCompletionUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure InvokeDataHCompletionUPP(request: Ptr; refcon: SInt32; err: OSErr; userRoutine: DataHCompletionUPP); external name '_InvokeDataHCompletionUPP'; { old name was CallDataHCompletionProc }
{
 *  InvokeVdigIntUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure InvokeVdigIntUPP(flags: SInt32; refcon: SInt32; userRoutine: VdigIntUPP); external name '_InvokeVdigIntUPP'; { old name was CallVdigIntProc }
{
 *  InvokeStartDocumentHandlerUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 }
function InvokeStartDocumentHandlerUPP(refcon: SInt32; userRoutine: StartDocumentHandlerUPP): ComponentResult; external name '_InvokeStartDocumentHandlerUPP'; { old name was CallStartDocumentHandlerProc }
{
 *  InvokeEndDocumentHandlerUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 }
function InvokeEndDocumentHandlerUPP(refcon: SInt32; userRoutine: EndDocumentHandlerUPP): ComponentResult; external name '_InvokeEndDocumentHandlerUPP'; { old name was CallEndDocumentHandlerProc }
{
 *  InvokeStartElementHandlerUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 }
function InvokeStartElementHandlerUPP(name: ConstCStringPtr; var atts: ConstCStringPtr; refcon: SInt32; userRoutine: StartElementHandlerUPP): ComponentResult; external name '_InvokeStartElementHandlerUPP'; { old name was CallStartElementHandlerProc }
{
 *  InvokeEndElementHandlerUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 }
function InvokeEndElementHandlerUPP(name: ConstCStringPtr; refcon: SInt32; userRoutine: EndElementHandlerUPP): ComponentResult; external name '_InvokeEndElementHandlerUPP'; { old name was CallEndElementHandlerProc }
{
 *  InvokeCharDataHandlerUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 }
function InvokeCharDataHandlerUPP(charData: ConstCStringPtr; refcon: SInt32; userRoutine: CharDataHandlerUPP): ComponentResult; external name '_InvokeCharDataHandlerUPP'; { old name was CallCharDataHandlerProc }
{
 *  InvokePreprocessInstructionHandlerUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 }
function InvokePreprocessInstructionHandlerUPP(name: ConstCStringPtr; atts: ConstCStringPtrPtr; refcon: SInt32; userRoutine: PreprocessInstructionHandlerUPP): ComponentResult; external name '_InvokePreprocessInstructionHandlerUPP'; { old name was CallPreprocessInstructionHandlerProc }
{
 *  InvokeCommentHandlerUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 }
function InvokeCommentHandlerUPP(comment: ConstCStringPtr; refcon: SInt32; userRoutine: CommentHandlerUPP): ComponentResult; external name '_InvokeCommentHandlerUPP'; { old name was CallCommentHandlerProc }
{
 *  InvokeCDataHandlerUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.2 and later
 }
function InvokeCDataHandlerUPP(cdata: ConstCStringPtr; refcon: SInt32; userRoutine: CDataHandlerUPP): ComponentResult; external name '_InvokeCDataHandlerUPP'; { old name was CallCDataHandlerProc }
{
 *  InvokeSGDataUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function InvokeSGDataUPP(c: SGChannel; p: Ptr; len: SInt32; var offset: SInt32; chRefCon: SInt32; time: TimeValue; writeType: SInt16; refCon: SInt32; userRoutine: SGDataUPP): OSErr; external name '_InvokeSGDataUPP'; { old name was CallSGDataProc }
{
 *  InvokeSGModalFilterUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function InvokeSGModalFilterUPP(theDialog: DialogRef; const (*var*) theEvent: EventRecord; var itemHit: SInt16; refCon: SInt32; userRoutine: SGModalFilterUPP): boolean; external name '_InvokeSGModalFilterUPP'; { old name was CallSGModalFilterProc }
{
 *  InvokeSGGrabBottleUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function InvokeSGGrabBottleUPP(c: SGChannel; bufferNum: SInt16; refCon: SInt32; userRoutine: SGGrabBottleUPP): ComponentResult; external name '_InvokeSGGrabBottleUPP'; { old name was CallSGGrabBottleProc }
{
 *  InvokeSGGrabCompleteBottleUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function InvokeSGGrabCompleteBottleUPP(c: SGChannel; bufferNum: SInt16; var done: boolean; refCon: SInt32; userRoutine: SGGrabCompleteBottleUPP): ComponentResult; external name '_InvokeSGGrabCompleteBottleUPP'; { old name was CallSGGrabCompleteBottleProc }
{
 *  InvokeSGDisplayBottleUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function InvokeSGDisplayBottleUPP(c: SGChannel; bufferNum: SInt16; var mp: MatrixRecord; clipRgn: RgnHandle; refCon: SInt32; userRoutine: SGDisplayBottleUPP): ComponentResult; external name '_InvokeSGDisplayBottleUPP'; { old name was CallSGDisplayBottleProc }
{
 *  InvokeSGCompressBottleUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function InvokeSGCompressBottleUPP(c: SGChannel; bufferNum: SInt16; refCon: SInt32; userRoutine: SGCompressBottleUPP): ComponentResult; external name '_InvokeSGCompressBottleUPP'; { old name was CallSGCompressBottleProc }
{
 *  InvokeSGCompressCompleteBottleUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function InvokeSGCompressCompleteBottleUPP(c: SGChannel; bufferNum: SInt16; var done: boolean; var ci: SGCompressInfo; refCon: SInt32; userRoutine: SGCompressCompleteBottleUPP): ComponentResult; external name '_InvokeSGCompressCompleteBottleUPP'; { old name was CallSGCompressCompleteBottleProc }
{
 *  InvokeSGAddFrameBottleUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function InvokeSGAddFrameBottleUPP(c: SGChannel; bufferNum: SInt16; atTime: TimeValue; scale: TimeScale; const (*var*) ci: SGCompressInfo; refCon: SInt32; userRoutine: SGAddFrameBottleUPP): ComponentResult; external name '_InvokeSGAddFrameBottleUPP'; { old name was CallSGAddFrameBottleProc }
{
 *  InvokeSGTransferFrameBottleUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function InvokeSGTransferFrameBottleUPP(c: SGChannel; bufferNum: SInt16; var mp: MatrixRecord; clipRgn: RgnHandle; refCon: SInt32; userRoutine: SGTransferFrameBottleUPP): ComponentResult; external name '_InvokeSGTransferFrameBottleUPP'; { old name was CallSGTransferFrameBottleProc }
{
 *  InvokeSGGrabCompressCompleteBottleUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function InvokeSGGrabCompressCompleteBottleUPP(c: SGChannel; var queuedFrameCount: UInt8; var ci: SGCompressInfo; var t: TimeRecord; refCon: SInt32; userRoutine: SGGrabCompressCompleteBottleUPP): ComponentResult; external name '_InvokeSGGrabCompressCompleteBottleUPP'; { old name was CallSGGrabCompressCompleteBottleProc }
{
 *  InvokeSGDisplayCompressBottleUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function InvokeSGDisplayCompressBottleUPP(c: SGChannel; dataPtr: Ptr; desc: ImageDescriptionHandle; var mp: MatrixRecord; clipRgn: RgnHandle; refCon: SInt32; userRoutine: SGDisplayCompressBottleUPP): ComponentResult; external name '_InvokeSGDisplayCompressBottleUPP'; { old name was CallSGDisplayCompressBottleProc }
{$ALIGN MAC68K}


end.
