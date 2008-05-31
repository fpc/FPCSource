{
     File:       QuickTimeVRFormat.p
 
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
    Version 210
    Please report any bugs to <gpc@microbizz.nl>
}

{$mode macpas}
{$packenum 1}
{$macro on}
{$inline on}
{$calling mwpascal}

unit QuickTimeVRFormat;
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
uses MacTypes,Files,ConditionalMacros,Movies,QuickTimeVR;


{$ALIGN MAC68K}

{  User data type for the Movie Controller type specifier }

const
	kQTControllerType			= FourCharCode('ctyp');						{  Atom & ID of where our }
	kQTControllerID				= 1;							{  ...controller name is stored }

	{  VRWorld atom types }
	kQTVRWorldHeaderAtomType	= FourCharCode('vrsc');
	kQTVRImagingParentAtomType	= FourCharCode('imgp');
	kQTVRPanoImagingAtomType	= FourCharCode('impn');
	kQTVRObjectImagingAtomType	= FourCharCode('imob');
	kQTVRNodeParentAtomType		= FourCharCode('vrnp');
	kQTVRNodeIDAtomType			= FourCharCode('vrni');
	kQTVRNodeLocationAtomType	= FourCharCode('nloc');
	kQTVRCursorParentAtomType	= FourCharCode('vrcp');						{  New with 2.1 }
	kQTVRCursorAtomType			= FourCharCode('CURS');						{  New with 2.1 }
	kQTVRColorCursorAtomType	= FourCharCode('crsr');						{  New with 2.1 }

	{  NodeInfo atom types }
	kQTVRNodeHeaderAtomType		= FourCharCode('ndhd');
	kQTVRHotSpotParentAtomType	= FourCharCode('hspa');
	kQTVRHotSpotAtomType		= FourCharCode('hots');
	kQTVRHotSpotInfoAtomType	= FourCharCode('hsin');
	kQTVRLinkInfoAtomType		= FourCharCode('link');

	{  Miscellaneous atom types }
	kQTVRStringAtomType			= FourCharCode('vrsg');
	kQTVRStringEncodingAtomType	= FourCharCode('vrse');						{  New with 2.1 }
	kQTVRPanoSampleDataAtomType	= FourCharCode('pdat');
	kQTVRObjectInfoAtomType		= FourCharCode('obji');
	kQTVRImageTrackRefAtomType	= FourCharCode('imtr');						{  Parent is kQTVRObjectInfoAtomType. Required if track ref is not 1 as required by 2.0 format. }
	kQTVRHotSpotTrackRefAtomType = FourCharCode('hstr');						{  Parent is kQTVRObjectInfoAtomType. Required if track ref is not 1 as required by 2.0 format. }
	kQTVRAngleRangeAtomType		= FourCharCode('arng');
	kQTVRTrackRefArrayAtomType	= FourCharCode('tref');
	kQTVRPanConstraintAtomType	= FourCharCode('pcon');
	kQTVRTiltConstraintAtomType	= FourCharCode('tcon');
	kQTVRFOVConstraintAtomType	= FourCharCode('fcon');
	kQTVRCubicViewAtomType		= FourCharCode('cuvw');						{  New with 5.0 }
	kQTVRCubicFaceDataAtomType	= FourCharCode('cufa');						{  New with 5.0 }

	kQTVRObjectInfoAtomID		= 1;
	kQTVRObjectImageTrackRefAtomID = 1;							{  New with 2.1, it adds a track reference to select between multiple image tracks }
	kQTVRObjectHotSpotTrackRefAtomID = 1;						{  New with 2.1, it adds a track reference to select between multiple hotspot tracks }

	{  Track reference types }
	kQTVRImageTrackRefType		= FourCharCode('imgt');
	kQTVRHotSpotTrackRefType	= FourCharCode('hott');

	{  Old hot spot types }
	kQTVRHotSpotNavigableType	= FourCharCode('navg');

	{  Valid bits used in QTVRLinkHotSpotAtom }
	kQTVRValidPan				= $00000001;
	kQTVRValidTilt				= $00000002;
	kQTVRValidFOV				= $00000004;
	kQTVRValidViewCenter		= $00000008;


	{  Values for flags field in QTVRPanoSampleAtom }
	kQTVRPanoFlagHorizontal		= $00000001;
	kQTVRPanoFlagLast			= $80000000;


	{  Values for locationFlags field in QTVRNodeLocationAtom }
	kQTVRSameFile				= 0;


	{  Header for QTVR track's Sample Description record (vrWorld atom container is appended) }

type
	QTVRSampleDescriptionPtr = ^QTVRSampleDescription;
	QTVRSampleDescription = record
		descSize:				UInt32;									{  total size of the QTVRSampleDescription }
		descType:				UInt32;									{  must be 'qtvr' }
		reserved1:				UInt32;									{  must be zero }
		reserved2:				UInt16;									{  must be zero }
		dataRefIndex:			UInt16;									{  must be zero }
		data:					UInt32;									{  Will be extended to hold vrWorld QTAtomContainer }
	end;

	QTVRSampleDescriptionHandle			= ^QTVRSampleDescriptionPtr;
	{
	  =================================================================================================
	   Definitions and structures used in the VRWorld QTAtomContainer
	  -------------------------------------------------------------------------------------------------
	}

	QTVRStringAtomPtr = ^QTVRStringAtom;
	QTVRStringAtom = record
		stringUsage:			UInt16;
		stringLength:			UInt16;
		theString:				packed array [0..3] of UInt8;			{  field previously named "string" }
	end;


	QTVRWorldHeaderAtomPtr = ^QTVRWorldHeaderAtom;
	QTVRWorldHeaderAtom = record
		majorVersion:			UInt16;
		minorVersion:			UInt16;
		nameAtomID:				QTAtomID;
		defaultNodeID:			UInt32;
		vrWorldFlags:			UInt32;
		reserved1:				UInt32;
		reserved2:				UInt32;
	end;


	{  Valid bits used in QTVRPanoImagingAtom }

const
	kQTVRValidCorrection		= $00000001;
	kQTVRValidQuality			= $00000002;
	kQTVRValidDirectDraw		= $00000004;
	kQTVRValidFirstExtraProperty = $00000008;


type
	QTVRPanoImagingAtomPtr = ^QTVRPanoImagingAtom;
	QTVRPanoImagingAtom = record
		majorVersion:			UInt16;
		minorVersion:			UInt16;
		imagingMode:			UInt32;
		imagingValidFlags:		UInt32;
		correction:				UInt32;
		quality:				UInt32;
		directDraw:				UInt32;
		imagingProperties:		array [0..5] of UInt32;					{  for future properties }
		reserved1:				UInt32;
		reserved2:				UInt32;
	end;

	QTVRNodeLocationAtomPtr = ^QTVRNodeLocationAtom;
	QTVRNodeLocationAtom = record
		majorVersion:			UInt16;
		minorVersion:			UInt16;
		nodeType:				OSType;
		locationFlags:			UInt32;
		locationData:			UInt32;
		reserved1:				UInt32;
		reserved2:				UInt32;
	end;

	{
	  =================================================================================================
	   Definitions and structures used in the Nodeinfo QTAtomContainer
	  -------------------------------------------------------------------------------------------------
	}

	QTVRNodeHeaderAtomPtr = ^QTVRNodeHeaderAtom;
	QTVRNodeHeaderAtom = record
		majorVersion:			UInt16;
		minorVersion:			UInt16;
		nodeType:				OSType;
		nodeID:					QTAtomID;
		nameAtomID:				QTAtomID;
		commentAtomID:			QTAtomID;
		reserved1:				UInt32;
		reserved2:				UInt32;
	end;

	QTVRAngleRangeAtomPtr = ^QTVRAngleRangeAtom;
	QTVRAngleRangeAtom = record
		minimumAngle:			Float32;
		maximumAngle:			Float32;
	end;

	QTVRHotSpotInfoAtomPtr = ^QTVRHotSpotInfoAtom;
	QTVRHotSpotInfoAtom = record
		majorVersion:			UInt16;
		minorVersion:			UInt16;
		hotSpotType:			OSType;
		nameAtomID:				QTAtomID;
		commentAtomID:			QTAtomID;
		cursorID:				array [0..2] of SInt32;
																		{  canonical view for this hot spot }
		bestPan:				Float32;
		bestTilt:				Float32;
		bestFOV:				Float32;
		bestViewCenter:			QTVRFloatPoint;
																		{  Bounding box for this hot spot }
		hotSpotRect:			Rect;
		flags:					UInt32;
		reserved1:				UInt32;
		reserved2:				UInt32;
	end;

	QTVRLinkHotSpotAtomPtr = ^QTVRLinkHotSpotAtom;
	QTVRLinkHotSpotAtom = record
		majorVersion:			UInt16;
		minorVersion:			UInt16;
		toNodeID:				UInt32;
		fromValidFlags:			UInt32;
		fromPan:				Float32;
		fromTilt:				Float32;
		fromFOV:				Float32;
		fromViewCenter:			QTVRFloatPoint;
		toValidFlags:			UInt32;
		toPan:					Float32;
		toTilt:					Float32;
		toFOV:					Float32;
		toViewCenter:			QTVRFloatPoint;
		distance:				Float32;
		flags:					UInt32;
		reserved1:				UInt32;
		reserved2:				UInt32;
	end;

	{
	  =================================================================================================
	   Definitions and structures used in Panorama and Object tracks
	  -------------------------------------------------------------------------------------------------
	}

	QTVRPanoSampleAtomPtr = ^QTVRPanoSampleAtom;
	QTVRPanoSampleAtom = record
		majorVersion:			UInt16;
		minorVersion:			UInt16;
		imageRefTrackIndex:		UInt32;									{  track reference index of the full res image track }
		hotSpotRefTrackIndex:	UInt32;									{  track reference index of the full res hot spot track }
		minPan:					Float32;
		maxPan:					Float32;
		minTilt:				Float32;
		maxTilt:				Float32;
		minFieldOfView:			Float32;
		maxFieldOfView:			Float32;
		defaultPan:				Float32;
		defaultTilt:			Float32;
		defaultFieldOfView:		Float32;
																		{  Info for highest res version of image track }
		imageSizeX:				UInt32;									{  pixel width of the panorama (e.g. 768) }
		imageSizeY:				UInt32;									{  pixel height of the panorama (e.g. 2496) }
		imageNumFramesX:		UInt16;									{  diced frames wide (e.g. 1) }
		imageNumFramesY:		UInt16;									{  diced frames high (e.g. 24) }
																		{  Info for highest res version of hotSpot track }
		hotSpotSizeX:			UInt32;									{  pixel width of the hot spot panorama (e.g. 768) }
		hotSpotSizeY:			UInt32;									{  pixel height of the hot spot panorama (e.g. 2496) }
		hotSpotNumFramesX:		UInt16;									{  diced frames wide (e.g. 1) }
		hotSpotNumFramesY:		UInt16;									{  diced frames high (e.g. 24) }
		flags:					UInt32;
		panoType:				OSType;
		reserved2:				UInt32;
	end;

	{
	   View atom for cubes (since same fields in QTVRPanoSampleAtom are set to special
	   values for backwards compatibility and hence are ignored by the cubic engine)
	}
	QTVRCubicViewAtomPtr = ^QTVRCubicViewAtom;
	QTVRCubicViewAtom = record
		minPan:					Float32;
		maxPan:					Float32;
		minTilt:				Float32;
		maxTilt:				Float32;
		minFieldOfView:			Float32;
		maxFieldOfView:			Float32;
		defaultPan:				Float32;
		defaultTilt:			Float32;
		defaultFieldOfView:		Float32;
	end;

	QTVRCubicFaceDataPtr = ^QTVRCubicFaceData;
	QTVRCubicFaceData = record
		orientation:			array [0..3] of Float32;				{  WXYZ quaternion of absolute orientation }
		center:					array [0..1] of Float32;				{  Center of image relative to center of projection (default = (0,0)) in normalized units }
		aspect:					Float32;								{  aspect>1 => tall pixels; aspect <1 => squat pixels (default = 1) }
		skew:					Float32;								{  skew x by y (default = 0) }
	end;

	{  Special resolution values for the Image Track Reference Atoms. Use only one value per track reference. }

const
	kQTVRFullTrackRes			= $00000001;
	kQTVRHalfTrackRes			= $00000002;
	kQTVRQuarterTrackRes		= $00000004;
	kQTVRPreviewTrackRes		= $8000;


type
	QTVRTrackRefEntryPtr = ^QTVRTrackRefEntry;
	QTVRTrackRefEntry = record
		trackRefType:			UInt32;
		trackResolution:		UInt16;
		trackRefIndex:			UInt32;
	end;

	{
	  =================================================================================================
	   Object File format 2.0
	  -------------------------------------------------------------------------------------------------
	}

const
	kQTVRObjectAnimateViewFramesOn = $00000001;
	kQTVRObjectPalindromeViewFramesOn = $00000002;
	kQTVRObjectStartFirstViewFrameOn = $00000004;
	kQTVRObjectAnimateViewsOn	= $00000008;
	kQTVRObjectPalindromeViewsOn = $00000010;
	kQTVRObjectSyncViewToFrameRate = $00000020;
	kQTVRObjectDontLoopViewFramesOn = $00000040;
	kQTVRObjectPlayEveryViewFrameOn = $00000080;
	kQTVRObjectStreamingViewsOn	= $00000100;

	kQTVRObjectWrapPanOn		= $00000001;
	kQTVRObjectWrapTiltOn		= $00000002;
	kQTVRObjectCanZoomOn		= $00000004;
	kQTVRObjectReverseHControlOn = $00000008;
	kQTVRObjectReverseVControlOn = $00000010;
	kQTVRObjectSwapHVControlOn	= $00000020;
	kQTVRObjectTranslationOn	= $00000040;

	kGrabberScrollerUI			= 1;							{  "Object"  }
	kOldJoyStickUI				= 2;							{   "1.0 Object as Scene"      }
	kJoystickUI					= 3;							{  "Object In Scene" }
	kGrabberUI					= 4;							{  "Grabber only" }
	kAbsoluteUI					= 5;							{  "Absolute pointer" }


type
	QTVRObjectSampleAtomPtr = ^QTVRObjectSampleAtom;
	QTVRObjectSampleAtom = record
		majorVersion:			UInt16;									{  kQTVRMajorVersion }
		minorVersion:			UInt16;									{  kQTVRMinorVersion }
		movieType:				UInt16;									{  ObjectUITypes }
		viewStateCount:			UInt16;									{  The number of view states 1 based }
		defaultViewState:		UInt16;									{  The default view state number. The number must be 1 to viewStateCount }
		mouseDownViewState:		UInt16;									{  The mouse down view state.   The number must be 1 to viewStateCount }
		viewDuration:			UInt32;									{  The duration of each view including all animation frames in a view }
		columns:				UInt32;									{  Number of columns in movie }
		rows:					UInt32;									{  Number rows in movie }
		mouseMotionScale:		Float32;								{  180.0 for kStandardObject or kQTVRObjectInScene, actual degrees for kOldNavigableMovieScene. }
		minPan:					Float32;								{  Start   horizontal pan angle in degrees }
		maxPan:					Float32;								{  End     horizontal pan angle in degrees }
		defaultPan:				Float32;								{  Initial horizontal pan angle in degrees (poster view) }
		minTilt:				Float32;								{  Start   vertical   pan angle in degrees }
		maxTilt:				Float32;								{  End     vertical   pan angle in degrees }
		defaultTilt:			Float32;								{  Initial vertical   pan angle in degrees (poster view)   }
		minFieldOfView:			Float32;								{  minimum field of view setting (appears as the maximum zoom effect) must be >= 1 }
		fieldOfView:			Float32;								{  the field of view range must be >= 1 }
		defaultFieldOfView:		Float32;								{  must be in minFieldOfView and maxFieldOfView range inclusive }
		defaultViewCenterH:		Float32;
		defaultViewCenterV:		Float32;
		viewRate:				Float32;
		frameRate:				Float32;
		animationSettings:		UInt32;									{  32 reserved bit fields }
		controlSettings:		UInt32;									{  32 reserved bit fields }
	end;

	{
	  =================================================================================================
	   QuickTime VR Authoring Components
	  -------------------------------------------------------------------------------------------------
	}

	{
	   ComponentDescription constants for QTVR Export components   
	    (componentType = MovieExportType; componentSubType = MovieFileType)
	}

const
	kQTVRFlattenerManufacturer	= FourCharCode('vrwe');						{  aka QTVRFlattenerType }
	kQTVRSplitterManufacturer	= FourCharCode('vrsp');
	kQTVRObjExporterManufacturer = FourCharCode('vrob');

	{  QuickTime VR Flattener atom types }
	kQTVRFlattenerSettingsParentAtomType = FourCharCode('VRWe');				{  parent of settings atoms (other than compression) }
	kQTVRFlattenerPreviewResAtomType = FourCharCode('PRes');					{  preview resolution Int16 }
	kQTVRFlattenerImportSpecAtomType = FourCharCode('ISpe');					{  import file spec FSSpec }
	kQTVRFlattenerCreatePreviewAtomType = FourCharCode('Prev');				{  Boolean }
	kQTVRFlattenerImportPreviewAtomType = FourCharCode('IPre');				{  Boolean }
	kQTVRFlattenerBlurPreviewAtomType = FourCharCode('Blur');					{  Boolean }

	{  QuickTime VR Splitter atom types }
	kQTVRSplitterSettingsParentAtomType = FourCharCode('VRSp');				{  parent of settings atoms (other than compression) }
	kQTVRSplitterGenerateHTMLAtomType = FourCharCode('Ghtm');					{  Boolean }
	kQTVRSplitterOverwriteFilesAtomType = FourCharCode('Owfi');				{  Boolean }
	kQTVRSplitterUseFlattenerAtomType = FourCharCode('Usef');					{  Boolean }
	kQTVRSplitterShowControllerAtomType = FourCharCode('Shco');				{  Boolean }
	kQTVRSplitterTargetMyselfAtomType = FourCharCode('Tgtm');					{  Boolean }

	{  QuickTime VR Object Exporter atom types }
	kQTVRObjExporterSettingsBlockSize = FourCharCode('bsiz');					{  block size for compression }
	kQTVRObjExporterSettingsTargetSize = FourCharCode('tsiz');				{  target file size }


{$ifc OLDROUTINENAMES}

type
	VRStringAtom						= QTVRStringAtom;
	VRStringAtomPtr 					= ^VRStringAtom;
	VRWorldHeaderAtom					= QTVRWorldHeaderAtom;
	VRWorldHeaderAtomPtr 				= ^VRWorldHeaderAtom;
	VRPanoImagingAtom					= QTVRPanoImagingAtom;
	VRPanoImagingAtomPtr 				= ^VRPanoImagingAtom;
	VRNodeLocationAtom					= QTVRNodeLocationAtom;
	VRNodeLocationAtomPtr 				= ^VRNodeLocationAtom;
	VRNodeHeaderAtom					= QTVRNodeHeaderAtom;
	VRNodeHeaderAtomPtr 				= ^VRNodeHeaderAtom;
	VRAngleRangeAtom					= QTVRAngleRangeAtom;
	VRAngleRangeAtomPtr 				= ^VRAngleRangeAtom;
	VRHotSpotInfoAtom					= QTVRHotSpotInfoAtom;
	VRHotSpotInfoAtomPtr 				= ^VRHotSpotInfoAtom;
	VRLinkHotSpotAtom					= QTVRLinkHotSpotAtom;
	VRLinkHotSpotAtomPtr 				= ^VRLinkHotSpotAtom;
	VRPanoSampleAtom					= QTVRPanoSampleAtom;
	VRPanoSampleAtomPtr 				= ^VRPanoSampleAtom;
	VRTrackRefEntry						= QTVRTrackRefEntry;
	VRTrackRefEntryPtr 					= ^VRTrackRefEntry;
	VRObjectSampleAtom					= QTVRObjectSampleAtom;
	VRObjectSampleAtomPtr 				= ^VRObjectSampleAtom;
{$endc}  {OLDROUTINENAMES}


{$ALIGN MAC68K}


end.
