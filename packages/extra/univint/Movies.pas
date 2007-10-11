{
     File:       Movies.p
 
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

unit Movies;
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
uses MacTypes,Files,QDOffscreen,TextEdit,Controls,Dialogs,Quickdraw,Aliases,Events,Menus,Components,Sound,ImageCompression;


{$ALIGN MAC68K}


{  "kFix1" is defined in FixMath as "fixed1"  }
{ error codes are in Errors.[haa] }
{ gestalt codes are in Gestalt.[hpa] }

const
	MovieFileType				= $4D6F6F56 (* 'MooV' *);
	MovieScrapType				= $6D6F6F76 (* 'moov' *);

	MovieResourceType			= $6D6F6F76 (* 'moov' *);
	MovieForwardPointerResourceType = $666F7265 (* 'fore' *);
	MovieBackwardPointerResourceType = $6261636B (* 'back' *);

	MovieResourceAtomType		= $6D6F6F76 (* 'moov' *);
	MovieDataAtomType			= $6D646174 (* 'mdat' *);
	FreeAtomType				= $66726565 (* 'free' *);
	SkipAtomType				= $736B6970 (* 'skip' *);
	WideAtomPlaceholderType		= $77696465 (* 'wide' *);

	MediaHandlerType			= $6D686C72 (* 'mhlr' *);
	DataHandlerType				= $64686C72 (* 'dhlr' *);

	VideoMediaType				= $76696465 (* 'vide' *);
	SoundMediaType				= $736F756E (* 'soun' *);
	TextMediaType				= $74657874 (* 'text' *);
	BaseMediaType				= $676E7263 (* 'gnrc' *);
	MPEGMediaType				= $4D504547 (* 'MPEG' *);
	MusicMediaType				= $6D757369 (* 'musi' *);
	TimeCodeMediaType			= $746D6364 (* 'tmcd' *);
	SpriteMediaType				= $73707274 (* 'sprt' *);
	FlashMediaType				= $666C7368 (* 'flsh' *);
	MovieMediaType				= $6D6F6F76 (* 'moov' *);
	TweenMediaType				= $7477656E (* 'twen' *);
	ThreeDeeMediaType			= $71643364 (* 'qd3d' *);
	SkinMediaType				= $736B696E (* 'skin' *);
	HandleDataHandlerSubType	= $686E646C (* 'hndl' *);
	PointerDataHandlerSubType	= $70747220 (* 'ptr ' *);
	NullDataHandlerSubType		= $6E756C6C (* 'null' *);
	ResourceDataHandlerSubType	= $72737263 (* 'rsrc' *);
	URLDataHandlerSubType		= $75726C20 (* 'url ' *);
	WiredActionHandlerType		= $77697265 (* 'wire' *);

	VisualMediaCharacteristic	= $65796573 (* 'eyes' *);
	AudioMediaCharacteristic	= $65617273 (* 'ears' *);
	kCharacteristicCanSendVideo	= $76736E64 (* 'vsnd' *);
	kCharacteristicProvidesActions = $6163746E (* 'actn' *);
	kCharacteristicNonLinear	= $6E6F6E6C (* 'nonl' *);
	kCharacteristicCanStep		= $73746570 (* 'step' *);
	kCharacteristicHasNoDuration = $6E6F7469 (* 'noti' *);
	kCharacteristicHasSkinData	= $736B696E (* 'skin' *);
	kCharacteristicProvidesKeyFocus = $6B657966 (* 'keyf' *);

	kUserDataMovieControllerType = $63747970 (* 'ctyp' *);
	kUserDataName				= $6E616D65 (* 'name' *);
	kUserDataTextAlbum			= $C2A9616C (* '©alb' *);
	kUserDataTextArtist			= $C2A94152 (* '©ART' *);
	kUserDataTextAuthor			= $C2A96175 (* '©aut' *);
	kUserDataTextChapter		= $C2A96368 (* '©chp' *);
	kUserDataTextComment		= $C2A9636D (* '©cmt' *);
	kUserDataTextComposer		= $C2A9636F (* '©com' *);
	kUserDataTextCopyright		= $C2A96370 (* '©cpy' *);
	kUserDataTextCreationDate	= $C2A96461 (* '©day' *);
	kUserDataTextDescription	= $C2A96465 (* '©des' *);
	kUserDataTextDirector		= $C2A96469 (* '©dir' *);
	kUserDataTextDisclaimer		= $C2A96469 (* '©dis' *);
	kUserDataTextEncodedBy		= $C2A9656E (* '©enc' *);
	kUserDataTextFullName		= $C2A96E61 (* '©nam' *);
	kUserDataTextGenre			= $C2A96765 (* '©gen' *);
	kUserDataTextHostComputer	= $C2A96873 (* '©hst' *);
	kUserDataTextInformation	= $C2A9696E (* '©inf' *);
	kUserDataTextKeywords		= $C2A96B65 (* '©key' *);
	kUserDataTextMake			= $C2A96D61 (* '©mak' *);
	kUserDataTextModel			= $C2A96D6F (* '©mod' *);
	kUserDataTextOriginalArtist	= $C2A96F70 (* '©ope' *);
	kUserDataTextOriginalFormat	= $C2A9666D (* '©fmt' *);
	kUserDataTextOriginalSource	= $C2A97372 (* '©src' *);
	kUserDataTextPerformers		= $C2A97072 (* '©prf' *);
	kUserDataTextProducer		= $C2A97072 (* '©prd' *);
	kUserDataTextProduct		= $C2A95052 (* '©PRD' *);
	kUserDataTextSoftware		= $C2A97377 (* '©swr' *);
	kUserDataTextSpecialPlaybackRequirements = $C2A97265 (* '©req' *);
	kUserDataTextTrack			= $C2A97472 (* '©trk' *);
	kUserDataTextWarning		= $C2A97772 (* '©wrn' *);
	kUserDataTextWriter			= $C2A97772 (* '©wrt' *);
	kUserDataTextURLLink		= $C2A97572 (* '©url' *);
	kUserDataTextEditDate1		= $C2A96564 (* '©ed1' *);

	kUserDataUnicodeBit			= $00000080;

	DoTheRightThing				= 0;


type
	MovieRecordPtr = ^MovieRecord;
	MovieRecord = record
		data:					array [0..0] of SInt32;
	end;

	Movie								= ^MovieRecord;
	Movie_fix	                        = Movie; { used as field type when a record declaration contains a Movie field identifier }
	MoviePtr							= ^Movie;
	TrackRecordPtr = ^TrackRecord;
	TrackRecord = record
		data:					array [0..0] of SInt32;
	end;

	Track								= ^TrackRecord;
	Track_fix	                        = Track; { used as field type when a record declaration contains a Track field identifier }
	MediaRecordPtr = ^MediaRecord;
	MediaRecord = record
		data:					array [0..0] of SInt32;
	end;

	Media								= ^MediaRecord;
	UserDataRecordPtr = ^UserDataRecord;
	UserDataRecord = record
		data:					array [0..0] of SInt32;
	end;

	UserData							= ^UserDataRecord;
	TrackEditStateRecordPtr = ^TrackEditStateRecord;
	TrackEditStateRecord = record
		data:					array [0..0] of SInt32;
	end;

	TrackEditState						= ^TrackEditStateRecord;
	MovieEditStateRecordPtr = ^MovieEditStateRecord;
	MovieEditStateRecord = record
		data:					array [0..0] of SInt32;
	end;

	MovieEditState						= ^MovieEditStateRecord;
	QTRestrictionSetRecordPtr = ^QTRestrictionSetRecord;
	QTRestrictionSetRecord = record
		data:					array [0..0] of SInt32;
	end;

	QTRestrictionSet					= ^QTRestrictionSetRecord;
	SpriteWorldRecordPtr = ^SpriteWorldRecord;
	SpriteWorldRecord = record
		data:					array [0..0] of SInt32;
	end;

	SpriteWorld							= ^SpriteWorldRecord;
	SpriteRecordPtr = ^SpriteRecord;
	SpriteRecord = record
		data:					array [0..0] of SInt32;
	end;

	Sprite								= ^SpriteRecord;
	QTTweenerRecordPtr = ^QTTweenerRecord;
	QTTweenerRecord = record
		data:					array [0..0] of SInt32;
	end;

	QTTweener							= ^QTTweenerRecord;
	SampleDescriptionPtr = ^SampleDescription;
	SampleDescription = record
		descSize:				SInt32;
		dataFormat:				SInt32;
		resvd1:					SInt32;
		resvd2:					SInt16;
		dataRefIndex:			SInt16;
	end;

	SampleDescriptionHandle				= ^SampleDescriptionPtr;
	QTBandwidthReference    = ^SInt32; { an opaque 32-bit type }
	QTBandwidthReferencePtr = ^QTBandwidthReference;  { when a var xx:QTBandwidthReference parameter can be nil, it is changed to xx: QTBandwidthReferencePtr }
	QTScheduledBandwidthReference    = ^SInt32; { an opaque 32-bit type }
	QTScheduledBandwidthReferencePtr = ^QTScheduledBandwidthReference;  { when a var xx:QTScheduledBandwidthReference parameter can be nil, it is changed to xx: QTScheduledBandwidthReferencePtr }

const
	kQTNetworkStatusNoNetwork	= -2;
	kQTNetworkStatusUncertain	= -1;
	kQTNetworkStatusNotConnected = 0;
	kQTNetworkStatusConnected	= 1;


type
	QTAtomContainer						= Handle;
	QTAtom								= SInt32;
	QTAtomType							= SInt32;
	QTAtomID							= SInt32;
	{  QTFloatDouble is the 64-bit IEEE-754 standard }
	QTFloatDouble						= Float64;
	{  QTFloatSingle is the 32-bit IEEE-754 standard }
	QTFloatSingle						= Float32;


	SoundDescriptionPtr = ^SoundDescription;
	SoundDescription = record
		descSize:				SInt32;								{  total size of SoundDescription including extra data  }
		dataFormat:				SInt32;								{  sound format  }
		resvd1:					SInt32;								{  reserved for apple use. set to zero  }
		resvd2:					SInt16;								{  reserved for apple use. set to zero  }
		dataRefIndex:			SInt16;
		version:				SInt16;								{  which version is this data  }
		revlevel:				SInt16;								{  what version of that codec did this  }
		vendor:					SInt32;								{  whose  codec compressed this data  }
		numChannels:			SInt16;								{  number of channels of sound  }
		sampleSize:				SInt16;								{  number of bits per sample  }
		compressionID:			SInt16;								{  unused. set to zero.  }
		packetSize:				SInt16;								{  unused. set to zero.  }
		sampleRate:				UnsignedFixed;							{  sample rate sound is captured at  }
	end;

	SoundDescriptionHandle				= ^SoundDescriptionPtr;
	{  version 1 of the SoundDescription record }
	SoundDescriptionV1Ptr = ^SoundDescriptionV1;
	SoundDescriptionV1 = record
																		{  original fields }
		desc:					SoundDescription;
																		{  fixed compression ratio information }
		samplesPerPacket:		UInt32;
		bytesPerPacket:			UInt32;
		bytesPerFrame:			UInt32;
		bytesPerSample:			UInt32;
																		{  additional atom based fields ([long size, long type, some data], repeat) }
	end;

	SoundDescriptionV1Handle			= ^SoundDescriptionV1Ptr;
	TextDescriptionPtr = ^TextDescription;
	TextDescription = record
		descSize:				SInt32;								{  Total size of TextDescription }
		dataFormat:				SInt32;								{  'text' }
		resvd1:					SInt32;
		resvd2:					SInt16;
		dataRefIndex:			SInt16;
		displayFlags:			SInt32;								{  see enum below for flag values }
		textJustification:		SInt32;								{  Can be: teCenter,teFlush -Default,-Right,-Left }
		bgColor:				RGBColor;								{  Background color }
		defaultTextBox:			Rect;									{  Location to place the text within the track bounds }
		defaultStyle:			ScrpSTElement;							{  Default style (struct defined in TextEdit.h) }
		defaultFontName:		SInt8;									{  Font Name (pascal string - struct extended to fit)  }
	end;

	TextDescriptionHandle				= ^TextDescriptionPtr;
	SpriteDescriptionPtr = ^SpriteDescription;
	SpriteDescription = record
		descSize:				SInt32;								{  total size of SpriteDescription including extra data  }
		dataFormat:				SInt32;								{    }
		resvd1:					SInt32;								{  reserved for apple use  }
		resvd2:					SInt16;
		dataRefIndex:			SInt16;
		version:				SInt32;								{  which version is this data  }
		decompressorType:		OSType;									{  which decompressor to use, 0 for no decompression  }
		sampleFlags:			SInt32;								{  how to interpret samples  }
	end;

	SpriteDescriptionHandle				= ^SpriteDescriptionPtr;
	FlashDescriptionPtr = ^FlashDescription;
	FlashDescription = record
		descSize:				SInt32;
		dataFormat:				SInt32;
		resvd1:					SInt32;
		resvd2:					SInt16;
		dataRefIndex:			SInt16;
		version:				SInt32;								{  which version is this data  }
		decompressorType:		OSType;									{  which decompressor to use, 0 for no decompression  }
		flags:					SInt32;
	end;

	FlashDescriptionHandle				= ^FlashDescriptionPtr;
	ThreeDeeDescriptionPtr = ^ThreeDeeDescription;
	ThreeDeeDescription = record
		descSize:				SInt32;								{  total size of ThreeDeeDescription including extra data  }
		dataFormat:				SInt32;								{    }
		resvd1:					SInt32;								{  reserved for apple use  }
		resvd2:					SInt16;
		dataRefIndex:			SInt16;
		version:				SInt32;								{  which version is this data  }
		rendererType:			SInt32;								{  which renderer to use, 0 for default  }
		decompressorType:		SInt32;								{  which decompressor to use, 0 for default  }
	end;

	ThreeDeeDescriptionHandle			= ^ThreeDeeDescriptionPtr;
	DataReferenceRecordPtr = ^DataReferenceRecord;
	DataReferenceRecord = record
		dataRefType:			OSType;
		dataRef:				Handle;
	end;

	DataReferencePtr					= ^DataReferenceRecord;
	{	--------------------------
	  Music Sample Description
	--------------------------	}
	MusicDescriptionPtr = ^MusicDescription;
	MusicDescription = record
		descSize:				SInt32;
		dataFormat:				SInt32;								{  'musi'  }
		resvd1:					SInt32;
		resvd2:					SInt16;
		dataRefIndex:			SInt16;
		musicFlags:				SInt32;
		headerData:				array [0..0] of UInt32;					{  variable size!  }
	end;

	MusicDescriptionHandle				= ^MusicDescriptionPtr;

const
	kMusicFlagDontPlay2Soft		= $00000001;
	kMusicFlagDontSlaveToMovie	= $00000002;


	dfDontDisplay				= $01;							{  Don't display the text }
	dfDontAutoScale				= $02;							{  Don't scale text as track bounds grows or shrinks }
	dfClipToTextBox				= $04;							{  Clip update to the textbox }
	dfUseMovieBGColor			= $08;							{  Set text background to movie's background color }
	dfShrinkTextBoxToFit		= $10;							{  Compute minimum box to fit the sample }
	dfScrollIn					= $20;							{  Scroll text in until last of text is in view  }
	dfScrollOut					= $40;							{  Scroll text out until last of text is gone (if both set, scroll in then out) }
	dfHorizScroll				= $80;							{  Scroll text horizontally (otherwise it's vertical) }
	dfReverseScroll				= $0100;						{  vert: scroll down rather than up; horiz: scroll backwards (justfication dependent) }
	dfContinuousScroll			= $0200;						{  new samples cause previous samples to scroll out  }
	dfFlowHoriz					= $0400;						{  horiz scroll text flows in textbox rather than extend to right  }
	dfContinuousKaraoke			= $0800;						{  ignore begin offset, hilite everything up to the end offset(karaoke) }
	dfDropShadow				= $1000;						{  display text with a drop shadow  }
	dfAntiAlias					= $2000;						{  attempt to display text anti aliased }
	dfKeyedText					= $4000;						{  key the text over background }
	dfInverseHilite				= $8000;						{  Use inverse hiliting rather than using hilite color }
	dfTextColorHilite			= $00010000;					{  changes text color in place of hiliting.  }

	searchTextDontGoToFoundTime	= $00010000;
	searchTextDontHiliteFoundText = $00020000;
	searchTextOneTrackOnly		= $00040000;
	searchTextEnabledTracksOnly	= $00080000;

	{ use these with the text property routines }
																{  set property parameter / get property parameter }
	kTextTextHandle				= 1;							{  Handle / preallocated Handle }
	kTextTextPtr				= 2;							{  Pointer }
	kTextTEStyle				= 3;							{  TextStyle * / TextStyle * }
	kTextSelection				= 4;							{  long [2] / long [2] }
	kTextBackColor				= 5;							{  RGBColor * / RGBColor * }
	kTextForeColor				= 6;							{  RGBColor * / RGBColor * }
	kTextFace					= 7;							{  long / long * }
	kTextFont					= 8;							{  long / long * }
	kTextSize					= 9;							{  long / long * }
	kTextAlignment				= 10;							{  short * / short * }
	kTextHilite					= 11;							{  hiliteRecord * / hiliteRecord * }
	kTextDropShadow				= 12;							{  dropShadowRecord * / dropShadowRecord * }
	kTextDisplayFlags			= 13;							{  long / long * }
	kTextScroll					= 14;							{  TimeValue * / TimeValue * }
	kTextRelativeScroll			= 15;							{  Point * }
	kTextHyperTextFace			= 16;							{  hyperTextSetFace * / hyperTextSetFace * }
	kTextHyperTextColor			= 17;							{  hyperTextSetColor * / hyperTextSetColor * }
	kTextKeyEntry				= 18;							{  short }
	kTextMouseDown				= 19;							{  Point * }
	kTextTextBox				= 20;							{  Rect * / Rect * }
	kTextEditState				= 21;							{  short / short * }
	kTextLength					= 22;							{        / long * }

	k3DMediaRendererEntry		= $72656E64 (* 'rend' *);
	k3DMediaRendererName		= $6E616D65 (* 'name' *);
	k3DMediaRendererCode		= $72636F64 (* 'rcod' *);

	{	 progress messages 	}
	movieProgressOpen			= 0;
	movieProgressUpdatePercent	= 1;
	movieProgressClose			= 2;

	{	 progress operations 	}
	progressOpFlatten			= 1;
	progressOpInsertTrackSegment = 2;
	progressOpInsertMovieSegment = 3;
	progressOpPaste				= 4;
	progressOpAddMovieSelection	= 5;
	progressOpCopy				= 6;
	progressOpCut				= 7;
	progressOpLoadMovieIntoRam	= 8;
	progressOpLoadTrackIntoRam	= 9;
	progressOpLoadMediaIntoRam	= 10;
	progressOpImportMovie		= 11;
	progressOpExportMovie		= 12;

	mediaQualityDraft			= $0000;
	mediaQualityNormal			= $0040;
	mediaQualityBetter			= $0080;
	mediaQualityBest			= $00C0;

	{	****
	    Interactive Sprites Support
	****	}
	{  QTEventRecord flags }
	kQTEventPayloadIsQTList		= $00000001;


type
	QTEventRecordPtr = ^QTEventRecord;
	QTEventRecord = record
		version:				SInt32;
		eventType:				OSType;
		where:					Point;
		flags:					SInt32;
		payloadRefcon:			SInt32;								{  from here down only present if version >= 2 }
		param1:					SInt32;
		param2:					SInt32;
		param3:					SInt32;
	end;

	QTAtomSpecPtr = ^QTAtomSpec;
	QTAtomSpec = record
		container:				QTAtomContainer;
		atom:					QTAtom;
	end;

	ResolvedQTEventSpecPtr = ^ResolvedQTEventSpec;
	ResolvedQTEventSpec = record
		actionAtom:				QTAtomSpec;
		targetTrack:			Track;
		targetRefCon:			SInt32;
	end;


	{  action constants  }

const
	kActionMovieSetVolume		= 1024;							{  (short movieVolume)  }
	kActionMovieSetRate			= 1025;							{  (Fixed rate)  }
	kActionMovieSetLoopingFlags	= 1026;							{  (long loopingFlags)  }
	kActionMovieGoToTime		= 1027;							{  (TimeValue time)  }
	kActionMovieGoToTimeByName	= 1028;							{  (Str255 timeName)  }
	kActionMovieGoToBeginning	= 1029;							{  no params  }
	kActionMovieGoToEnd			= 1030;							{  no params  }
	kActionMovieStepForward		= 1031;							{  no params  }
	kActionMovieStepBackward	= 1032;							{  no params  }
	kActionMovieSetSelection	= 1033;							{  (TimeValue startTime, TimeValue endTime)  }
	kActionMovieSetSelectionByName = 1034;						{  (Str255 startTimeName, Str255 endTimeName)  }
	kActionMoviePlaySelection	= 1035;							{  (Boolean selectionOnly)  }
	kActionMovieSetLanguage		= 1036;							{  (long language)  }
	kActionMovieChanged			= 1037;							{  no params  }
	kActionMovieRestartAtTime	= 1038;							{  (TimeValue startTime, Fixed rate)  }
	kActionMovieGotoNextChapter	= 1039;							{  no params  }
	kActionMovieGotoPreviousChapter = 1040;						{  no params  }
	kActionMovieGotoFirstChapter = 1041;						{  no params  }
	kActionMovieGotoLastChapter	= 1042;							{  no params  }
	kActionMovieGotoChapterByIndex = 1043;						{  ( short index )  }
	kActionMovieSetScale		= 1044;							{  (Fixed xScale, Fixed yScale)  }
	kActionTrackSetVolume		= 2048;							{  (short volume)  }
	kActionTrackSetBalance		= 2049;							{  (short balance)  }
	kActionTrackSetEnabled		= 2050;							{  (Boolean enabled)  }
	kActionTrackSetMatrix		= 2051;							{  (MatrixRecord matrix)  }
	kActionTrackSetLayer		= 2052;							{  (short layer)  }
	kActionTrackSetClip			= 2053;							{  (RgnHandle clip)  }
	kActionTrackSetCursor		= 2054;							{  (QTATomID cursorID)  }
	kActionTrackSetGraphicsMode	= 2055;							{  (ModifierTrackGraphicsModeRecord graphicsMode)  }
	kActionTrackSetIdleFrequency = 2056;						{  (long frequency)  }
	kActionTrackSetBassTreble	= 2057;							{  (short base, short treble)  }
	kActionSpriteSetMatrix		= 3072;							{  (MatrixRecord matrix)  }
	kActionSpriteSetImageIndex	= 3073;							{  (short imageIndex)  }
	kActionSpriteSetVisible		= 3074;							{  (short visible)  }
	kActionSpriteSetLayer		= 3075;							{  (short layer)  }
	kActionSpriteSetGraphicsMode = 3076;						{  (ModifierTrackGraphicsModeRecord graphicsMode)  }
	kActionSpritePassMouseToCodec = 3078;						{  no params  }
	kActionSpriteClickOnCodec	= 3079;							{  Point localLoc  }
	kActionSpriteTranslate		= 3080;							{  (Fixed x, Fixed y, Boolean isAbsolute)  }
	kActionSpriteScale			= 3081;							{  (Fixed xScale, Fixed yScale)  }
	kActionSpriteRotate			= 3082;							{  (Fixed degrees)  }
	kActionSpriteStretch		= 3083;							{  (Fixed p1x, Fixed p1y, Fixed p2x, Fixed p2y, Fixed p3x, Fixed p3y, Fixed p4x, Fixed p4y)  }
	kActionSpriteSetCanBeHitTested = 3094;						{  (short canBeHitTested)  }
	kActionQTVRSetPanAngle		= 4096;							{  (float panAngle)  }
	kActionQTVRSetTiltAngle		= 4097;							{  (float tiltAngle)  }
	kActionQTVRSetFieldOfView	= 4098;							{  (float fieldOfView)  }
	kActionQTVRShowDefaultView	= 4099;							{  no params  }
	kActionQTVRGoToNodeID		= 4100;							{  (UInt32 nodeID)  }
	kActionQTVREnableHotSpot	= 4101;							{  long ID, Boolean enable  }
	kActionQTVRShowHotSpots		= 4102;							{  Boolean show  }
	kActionQTVRTranslateObject	= 4103;							{  float xMove, float yMove  }
	kActionQTVRSetViewState		= 4109;							{  long viewStateType, short state  }
	kActionMusicPlayNote		= 5120;							{  (long sampleDescIndex, long partNumber, long delay, long pitch, long velocity, long duration)  }
	kActionMusicSetController	= 5121;							{  (long sampleDescIndex, long partNumber, long delay, long controller, long value)  }
	kActionCase					= 6144;							{  [(CaseStatementActionAtoms)]  }
	kActionWhile				= 6145;							{  [(WhileStatementActionAtoms)]  }
	kActionGoToURL				= 6146;							{  (C string urlLink)  }
	kActionSendQTEventToSprite	= 6147;							{  ([(SpriteTargetAtoms)], QTEventRecord theEvent)  }
	kActionDebugStr				= 6148;							{  (Str255 theString)  }
	kActionPushCurrentTime		= 6149;							{  no params  }
	kActionPushCurrentTimeWithLabel = 6150;						{  (Str255 theLabel)  }
	kActionPopAndGotoTopTime	= 6151;							{  no params  }
	kActionPopAndGotoLabeledTime = 6152;						{  (Str255 theLabel)  }
	kActionStatusString			= 6153;							{  (C string theString, long stringTypeFlags)  }
	kActionSendQTEventToTrackObject = 6154;						{  ([(TrackObjectTargetAtoms)], QTEventRecord theEvent)  }
	kActionAddChannelSubscription = 6155;						{  (Str255 channelName, C string channelsURL, C string channelsPictureURL)  }
	kActionRemoveChannelSubscription = 6156;					{  (C string channelsURL)  }
	kActionOpenCustomActionHandler = 6157;						{  (long handlerID, ComponentDescription handlerDesc)  }
	kActionDoScript				= 6158;							{  (long scriptTypeFlags, CString command, CString arguments)  }
	kActionDoCompressedActions	= 6159;							{  (compressed QTAtomContainer prefixed with eight bytes: long compressorType, long decompressedSize)  }
	kActionSendAppMessage		= 6160;							{  (long appMessageID)  }
	kActionLoadComponent		= 6161;							{  (ComponentDescription handlerDesc)  }
	kActionSetFocus				= 6162;							{  [(TargetAtoms theObject)]  }
	kActionDontPassKeyEvent		= 6163;							{  no params  }
	kActionSetRandomSeed		= 6164;							{  long randomSeed  }
	kActionSpriteTrackSetVariable = 7168;						{  (QTAtomID variableID, float value)  }
	kActionSpriteTrackNewSprite	= 7169;							{  (QTAtomID spriteID, short imageIndex, MatrixRecord *matrix, short visible, short layer, ModifierTrackGraphicsModeRecord *graphicsMode, QTAtomID actionHandlingSpriteID)  }
	kActionSpriteTrackDisposeSprite = 7170;						{  (QTAtomID spriteID)  }
	kActionSpriteTrackSetVariableToString = 7171;				{  (QTAtomID variableID, C string value)  }
	kActionSpriteTrackConcatVariables = 7172;					{  (QTAtomID firstVariableID, QTAtomID secondVariableID, QTAtomID resultVariableID )  }
	kActionSpriteTrackSetVariableToMovieURL = 7173;				{  (QTAtomID variableID, < optional: [(MovieTargetAtoms)] > )  }
	kActionSpriteTrackSetVariableToMovieBaseURL = 7174;			{  (QTAtomID variableID, < optional: [(MovieTargetAtoms)] > )  }
	kActionSpriteTrackSetAllSpritesHitTestingMode = 7181;
	kActionSpriteTrackNewImage	= 7182;							{  (C string imageURL, QTAtomID desiredID)  }
	kActionSpriteTrackDisposeImage = 7183;						{  (short imageIndex)  }
	kActionApplicationNumberAndString = 8192;					{  (long aNumber, Str255 aString )  }
	kActionQD3DNamedObjectTranslateTo = 9216;					{  (Fixed x, Fixed y, Fixed z )  }
	kActionQD3DNamedObjectScaleTo = 9217;						{  (Fixed xScale, Fixed yScale, Fixed zScale )  }
	kActionQD3DNamedObjectRotateTo = 9218;						{  (Fixed xDegrees, Fixed yDegrees, Fixed zDegrees )  }
	kActionFlashTrackSetPan		= 10240;						{  (short xPercent, short yPercent )  }
	kActionFlashTrackSetZoom	= 10241;						{  (short zoomFactor )  }
	kActionFlashTrackSetZoomRect = 10242;						{  (long left, long top, long right, long bottom )  }
	kActionFlashTrackGotoFrameNumber = 10243;					{  (long frameNumber )  }
	kActionFlashTrackGotoFrameLabel = 10244;					{  (C string frameLabel )  }
	kActionFlashTrackSetFlashVariable = 10245;					{  (C string path, C string name, C string value, Boolean updateFocus)  }
	kActionFlashTrackDoButtonActions = 10246;					{  (C string path, long buttonID, long transition)  }
	kActionMovieTrackAddChildMovie = 11264;						{  (QTAtomID childMovieID, C string childMovieURL)  }
	kActionMovieTrackLoadChildMovie = 11265;					{  (QTAtomID childMovieID)  }
	kActionMovieTrackLoadChildMovieWithQTListParams = 11266;	{  (QTAtomID childMovieID, C string qtlistXML)  }
	kActionTextTrackPasteText	= 12288;						{  (C string theText, long startSelection, long endSelection )  }
	kActionTextTrackSetTextBox	= 12291;						{  (short left, short top, short right, short bottom)  }
	kActionTextTrackSetTextStyle = 12292;						{  (Handle textStyle)  }
	kActionTextTrackSetSelection = 12293;						{  (long startSelection, long endSelection )  }
	kActionTextTrackSetBackgroundColor = 12294;					{  (ModifierTrackGraphicsModeRecord backgroundColor )  }
	kActionTextTrackSetForegroundColor = 12295;					{  (ModifierTrackGraphicsModeRecord foregroundColor )  }
	kActionTextTrackSetFace		= 12296;						{  (long fontFace )  }
	kActionTextTrackSetFont		= 12297;						{  (long fontID )  }
	kActionTextTrackSetSize		= 12298;						{  (long fontSize )  }
	kActionTextTrackSetAlignment = 12299;						{  (short alignment )  }
	kActionTextTrackSetHilite	= 12300;						{  (long startHighlight, long endHighlight, ModifierTrackGraphicsModeRecord highlightColor )  }
	kActionTextTrackSetDropShadow = 12301;						{  (Point dropShadow, short transparency )  }
	kActionTextTrackSetDisplayFlags = 12302;					{  (long flags )  }
	kActionTextTrackSetScroll	= 12303;						{  (long delay )  }
	kActionTextTrackRelativeScroll = 12304;						{  (short deltaX, short deltaY )  }
	kActionTextTrackFindText	= 12305;						{  (long flags, Str255 theText, ModifierTrackGraphicsModeRecord highlightColor )  }
	kActionTextTrackSetHyperTextFace = 12306;					{  (short index, long fontFace )  }
	kActionTextTrackSetHyperTextColor = 12307;					{  (short index, ModifierTrackGraphicsModeRecord highlightColor )  }
	kActionTextTrackKeyEntry	= 12308;						{  (short character )  }
	kActionTextTrackMouseDown	= 12309;						{  no params  }
	kActionTextTrackSetEditable	= 12310;						{  (short editState)  }
	kActionListAddElement		= 13312;						{  (C string parentPath, long atIndex, C string newElementName)  }
	kActionListRemoveElements	= 13313;						{  (C string parentPath, long startIndex, long endIndex)  }
	kActionListSetElementValue	= 13314;						{  (C string elementPath, C string valueString)  }
	kActionListPasteFromXML		= 13315;						{  (C string xml, C string targetParentPath, long startIndex)  }
	kActionListSetMatchingFromXML = 13316;						{  (C string xml, C string targetParentPath)  }
	kActionListSetFromURL		= 13317;						{  (C string url, C string targetParentPath )  }
	kActionListExchangeLists	= 13318;						{  (C string url, C string parentPath)  }
	kActionListServerQuery		= 13319;						{  (C string url, C string keyValuePairs, long flags, C string parentPath)  }


	kOperandExpression			= 1;
	kOperandConstant			= 2;
	kOperandSubscribedToChannel	= 3;							{  C string channelsURL  }
	kOperandUniqueCustomActionHandlerID = 4;
	kOperandCustomActionHandlerIDIsOpen = 5;					{  long ID  }
	kOperandConnectionSpeed		= 6;
	kOperandGMTDay				= 7;
	kOperandGMTMonth			= 8;
	kOperandGMTYear				= 9;
	kOperandGMTHours			= 10;
	kOperandGMTMinutes			= 11;
	kOperandGMTSeconds			= 12;
	kOperandLocalDay			= 13;
	kOperandLocalMonth			= 14;
	kOperandLocalYear			= 15;
	kOperandLocalHours			= 16;
	kOperandLocalMinutes		= 17;
	kOperandLocalSeconds		= 18;
	kOperandRegisteredForQuickTimePro = 19;
	kOperandPlatformRunningOn	= 20;
	kOperandQuickTimeVersion	= 21;
	kOperandComponentVersion	= 22;							{  C string type, C string subType, C string manufacturer  }
	kOperandOriginalHandlerRefcon = 23;
	kOperandTicks				= 24;
	kOperandMaxLoadedTimeInMovie = 25;
	kOperandEventParameter		= 26;							{  short index  }
	kOperandFreeMemory			= 27;
	kOperandNetworkStatus		= 28;
	kOperandQuickTimeVersionRegistered = 29;					{  long version  }
	kOperandSystemVersion		= 30;
	kOperandMovieVolume			= 1024;
	kOperandMovieRate			= 1025;
	kOperandMovieIsLooping		= 1026;
	kOperandMovieLoopIsPalindrome = 1027;
	kOperandMovieTime			= 1028;
	kOperandMovieDuration		= 1029;
	kOperandMovieTimeScale		= 1030;
	kOperandMovieWidth			= 1031;
	kOperandMovieHeight			= 1032;
	kOperandMovieLoadState		= 1033;
	kOperandMovieTrackCount		= 1034;
	kOperandMovieIsActive		= 1035;
	kOperandMovieName			= 1036;
	kOperandMovieID				= 1037;
	kOperandMovieChapterCount	= 1038;
	kOperandMovieChapterIndex	= 1039;
	kOperandMovieChapterName	= 1040;
	kOperandMovieChapterNameByIndex = 1041;						{  ( short index )  }
	kOperandMovieChapterIndexByName = 1042;						{  (c string name)   }
	kOperandMovieAnnotation		= 1043;							{  (c string requested, long flags)  }
	kOperandMovieConnectionFlags = 1044;
	kOperandMovieConnectionString = 1045;
	kOperandTrackVolume			= 2048;
	kOperandTrackBalance		= 2049;
	kOperandTrackEnabled		= 2050;
	kOperandTrackLayer			= 2051;
	kOperandTrackWidth			= 2052;
	kOperandTrackHeight			= 2053;
	kOperandTrackDuration		= 2054;
	kOperandTrackName			= 2055;
	kOperandTrackID				= 2056;
	kOperandTrackIdleFrequency	= 2057;
	kOperandTrackBass			= 2058;
	kOperandTrackTreble			= 2059;
	kOperandSpriteBoundsLeft	= 3072;
	kOperandSpriteBoundsTop		= 3073;
	kOperandSpriteBoundsRight	= 3074;
	kOperandSpriteBoundsBottom	= 3075;
	kOperandSpriteImageIndex	= 3076;
	kOperandSpriteVisible		= 3077;
	kOperandSpriteLayer			= 3078;
	kOperandSpriteTrackVariable	= 3079;							{  [QTAtomID variableID]  }
	kOperandSpriteTrackNumSprites = 3080;
	kOperandSpriteTrackNumImages = 3081;
	kOperandSpriteID			= 3082;
	kOperandSpriteIndex			= 3083;
	kOperandSpriteFirstCornerX	= 3084;
	kOperandSpriteFirstCornerY	= 3085;
	kOperandSpriteSecondCornerX	= 3086;
	kOperandSpriteSecondCornerY	= 3087;
	kOperandSpriteThirdCornerX	= 3088;
	kOperandSpriteThirdCornerY	= 3089;
	kOperandSpriteFourthCornerX	= 3090;
	kOperandSpriteFourthCornerY	= 3091;
	kOperandSpriteImageRegistrationPointX = 3092;
	kOperandSpriteImageRegistrationPointY = 3093;
	kOperandSpriteTrackSpriteIDAtPoint = 3094;					{  short x, short y  }
	kOperandSpriteName			= 3095;
	kOperandSpriteCanBeHitTested = 3105;						{  short  }
	kOperandSpriteTrackAllSpritesHitTestingMode = 3106;
	kOperandSpriteTrackImageIDByIndex = 3107;					{  short imageIndex  }
	kOperandSpriteTrackImageIndexByID = 3108;					{  QTAtomID  }
	kOperandQTVRPanAngle		= 4096;
	kOperandQTVRTiltAngle		= 4097;
	kOperandQTVRFieldOfView		= 4098;
	kOperandQTVRNodeID			= 4099;
	kOperandQTVRHotSpotsVisible	= 4100;
	kOperandQTVRViewCenterH		= 4101;
	kOperandQTVRViewCenterV		= 4102;
	kOperandQTVRViewStateCount	= 4103;
	kOperandQTVRViewState		= 4104;							{  long viewStateType  }
	kOperandMouseLocalHLoc		= 5120;							{  [TargetAtoms aTrack]  }
	kOperandMouseLocalVLoc		= 5121;							{  [TargetAtoms aTrack]  }
	kOperandKeyIsDown			= 5122;							{  [short modKeys, char asciiValue]  }
	kOperandRandom				= 5123;							{  [short min, short max]  }
	kOperandCanHaveFocus		= 5124;							{  [(TargetAtoms theObject)]  }
	kOperandHasFocus			= 5125;							{  [(TargetAtoms theObject)]  }
	kOperandTextTrackEditable	= 6144;
	kOperandTextTrackCopyText	= 6145;							{  long startSelection, long endSelection  }
	kOperandTextTrackStartSelection = 6146;
	kOperandTextTrackEndSelection = 6147;
	kOperandTextTrackTextBoxLeft = 6148;
	kOperandTextTrackTextBoxTop	= 6149;
	kOperandTextTrackTextBoxRight = 6150;
	kOperandTextTrackTextBoxBottom = 6151;
	kOperandTextTrackTextLength	= 6152;
	kOperandListCountElements	= 7168;							{  (C string parentPath)  }
	kOperandListGetElementPathByIndex = 7169;					{  (C string parentPath, long index)  }
	kOperandListGetElementValue	= 7170;							{  (C string elementPath)  }
	kOperandListCopyToXML		= 7171;							{  (C string parentPath, long startIndex, long endIndex)  }
	kOperandSin					= 8192;							{  float x     }
	kOperandCos					= 8193;							{  float x     }
	kOperandTan					= 8194;							{  float x     }
	kOperandATan				= 8195;							{  float x     }
	kOperandATan2				= 8196;							{  float y, float x    }
	kOperandDegreesToRadians	= 8197;							{  float x  }
	kOperandRadiansToDegrees	= 8198;							{  float x  }
	kOperandSquareRoot			= 8199;							{  float x  }
	kOperandExponent			= 8200;							{  float x  }
	kOperandLog					= 8201;							{  float x  }
	kOperandFlashTrackVariable	= 9216;							{  [CString path, CString name]  }
	kOperandStringLength		= 10240;						{  (C string text)  }
	kOperandStringCompare		= 10241;						{  (C string aText, C string bText, Boolean caseSensitive, Boolan diacSensitive)  }
	kOperandStringSubString		= 10242;						{  (C string text, long offset, long length)  }
	kOperandStringConcat		= 10243;						{  (C string aText, C string bText)  }

	kFirstMovieAction			= 1024;
	kLastMovieAction			= 1044;
	kFirstTrackAction			= 2048;
	kLastTrackAction			= 2057;
	kFirstSpriteAction			= 3072;
	kLastSpriteAction			= 3094;
	kFirstQTVRAction			= 4096;
	kLastQTVRAction				= 4109;
	kFirstMusicAction			= 5120;
	kLastMusicAction			= 5121;
	kFirstSystemAction			= 6144;
	kLastSystemAction			= 6164;
	kFirstSpriteTrackAction		= 7168;
	kLastSpriteTrackAction		= 7183;
	kFirstApplicationAction		= 8192;
	kLastApplicationAction		= 8192;
	kFirstQD3DNamedObjectAction	= 9216;
	kLastQD3DNamedObjectAction	= 9218;
	kFirstFlashTrackAction		= 10240;
	kLastFlashTrackAction		= 10246;
	kFirstMovieTrackAction		= 11264;
	kLastMovieTrackAction		= 11266;
	kFirstTextTrackAction		= 12288;
	kLastTextTrackAction		= 12310;
	kFirstMultiTargetAction		= 13312;
	kLastMultiTargetAction		= 13319;
	kFirstAction				= 1024;
	kLastAction					= 13319;

	{  target atom types }
	kTargetMovie				= $6D6F6F76 (* 'moov' *);						{  no data  }
	kTargetMovieName			= $6D6F6E61 (* 'mona' *);						{  (PString movieName)  }
	kTargetMovieID				= $6D6F6964 (* 'moid' *);						{  (long movieID)  }
	kTargetRootMovie			= $6D6F726F (* 'moro' *);						{  no data  }
	kTargetParentMovie			= $6D6F7061 (* 'mopa' *);						{  no data  }
	kTargetChildMovieTrackName	= $6D6F746E (* 'motn' *);						{  (PString childMovieTrackName)  }
	kTargetChildMovieTrackID	= $6D6F7469 (* 'moti' *);						{  (long childMovieTrackID)  }
	kTargetChildMovieTrackIndex	= $6D6F7478 (* 'motx' *);						{  (long childMovieTrackIndex)  }
	kTargetChildMovieMovieName	= $6D6F6D6E (* 'momn' *);						{  (PString childMovieName)  }
	kTargetChildMovieMovieID	= $6D6F6D69 (* 'momi' *);						{  (long childMovieID)  }
	kTargetTrackName			= $74726E61 (* 'trna' *);						{  (PString trackName)  }
	kTargetTrackID				= $74726964 (* 'trid' *);						{  (long trackID)  }
	kTargetTrackType			= $74727479 (* 'trty' *);						{  (OSType trackType)  }
	kTargetTrackIndex			= $7472696E (* 'trin' *);						{  (long trackIndex)  }
	kTargetSpriteName			= $73706E61 (* 'spna' *);						{  (PString spriteName)  }
	kTargetSpriteID				= $73706964 (* 'spid' *);						{  (QTAtomID spriteID)  }
	kTargetSpriteIndex			= $7370696E (* 'spin' *);						{  (short spriteIndex)  }
	kTargetQD3DNamedObjectName	= $6E616E61 (* 'nana' *);						{  (CString objectName)  }
	kTargetCurrentQTEventParams	= $65767061 (* 'evpa' *);						{  no data  }

	{  action container atom types }
	kQTEventType				= $65766E74 (* 'evnt' *);
	kAction						= $6163746E (* 'actn' *);
	kWhichAction				= $77686963 (* 'whic' *);
	kActionParameter			= $7061726D (* 'parm' *);
	kActionTarget				= $74617267 (* 'targ' *);
	kActionFlags				= $666C6167 (* 'flag' *);
	kActionParameterMinValue	= $6D696E76 (* 'minv' *);
	kActionParameterMaxValue	= $6D617876 (* 'maxv' *);
	kActionListAtomType			= $6C697374 (* 'list' *);
	kExpressionContainerAtomType = $65787072 (* 'expr' *);
	kConditionalAtomType		= $74657374 (* 'test' *);
	kOperatorAtomType			= $6F706572 (* 'oper' *);
	kOperandAtomType			= $6F70726E (* 'oprn' *);
	kCommentAtomType			= $77687920 (* 'why ' *);
	kCustomActionHandler		= $63757374 (* 'cust' *);
	kCustomHandlerID			= $69642020 (* 'id  ' *);
	kCustomHandlerDesc			= $64657363 (* 'desc' *);
	kQTEventRecordAtomType		= $65726563 (* 'erec' *);

	{  QTEvent types  }
	kQTEventMouseClick			= $636C696B (* 'clik' *);
	kQTEventMouseClickEnd		= $63656E64 (* 'cend' *);
	kQTEventMouseClickEndTriggerButton = $74726967 (* 'trig' *);
	kQTEventMouseEnter			= $656E7472 (* 'entr' *);
	kQTEventMouseExit			= $65786974 (* 'exit' *);
	kQTEventMouseMoved			= $6D6F7665 (* 'move' *);
	kQTEventFrameLoaded			= $6672616D (* 'fram' *);
	kQTEventIdle				= $69646C65 (* 'idle' *);
	kQTEventKey					= $6B657920 (* 'key ' *);						{  qtevent.param1 = key, qtevent.param2 = modifiers, qtEvent.param3 = scanCode  }
	kQTEventMovieLoaded			= $6C6F6164 (* 'load' *);
	kQTEventRequestToModifyMovie = $7265716D (* 'reqm' *);
	kQTEventListReceived		= $6C697374 (* 'list' *);
	kQTEventKeyUp				= $6B657955 (* 'keyU' *);						{  qtevent.param1 = key, qtevent.param2 = modifiers, qtEvent.param3 = scanCode  }

	{  flags for the kActionFlags atom  }
	kActionFlagActionIsDelta	= $00000002;
	kActionFlagParameterWrapsAround = $00000004;
	kActionFlagActionIsToggle	= $00000008;

	{  flags for stringTypeFlags field of the QTStatusStringRecord  }
	kStatusStringIsURLLink		= $00000002;
	kStatusStringIsStreamingStatus = $00000004;
	kStatusHasCodeNumber		= $00000008;					{  high 16 bits of stringTypeFlags is error code number }
	kStatusIsError				= $00000010;

	{  flags for scriptTypeFlags field of the QTDoScriptRecord }
	kScriptIsUnknownType		= $00000001;
	kScriptIsJavaScript			= $00000002;
	kScriptIsLingoEvent			= $00000004;
	kScriptIsVBEvent			= $00000008;
	kScriptIsProjectorCommand	= $00000010;
	kScriptIsAppleScript		= $00000020;

	{  flags for CheckQuickTimeRegistration routine }
	kQTRegistrationDialogTimeOutFlag = $01;
	kQTRegistrationDialogShowDialog = $02;
	kQTRegistrationDialogForceDialog = $04;

	{  constants for kOperatorAtomType IDs (operator types) }
	kOperatorAdd				= $61646420 (* 'add ' *);
	kOperatorSubtract			= $73756220 (* 'sub ' *);
	kOperatorMultiply			= $6D756C74 (* 'mult' *);
	kOperatorDivide				= $64697620 (* 'div ' *);
	kOperatorOr					= $6F722020 (* 'or  ' *);
	kOperatorAnd				= $616E6420 (* 'and ' *);
	kOperatorNot				= $6E6F7420 (* 'not ' *);
	kOperatorLessThan			= $3C202020 (* '<   ' *);
	kOperatorLessThanEqualTo	= $3C3D2020 (* '<=  ' *);
	kOperatorEqualTo			= $3D202020 (* '=   ' *);
	kOperatorNotEqualTo			= $213D2020 (* '!=  ' *);
	kOperatorGreaterThan		= $3E202020 (* '>   ' *);
	kOperatorGreaterThanEqualTo	= $3E3D2020 (* '>=  ' *);
	kOperatorModulo				= $6D6F6420 (* 'mod ' *);
	kOperatorIntegerDivide		= $69646976 (* 'idiv' *);
	kOperatorAbsoluteValue		= $61627320 (* 'abs ' *);
	kOperatorNegate				= $6E656720 (* 'neg ' *);

	{  constants for kOperandPlatformRunningOn }
	kPlatformMacintosh			= 1;
	kPlatformWindows			= 2;

	{  flags for kOperandSystemVersion }
	kSystemIsWindows9x			= $00010000;
	kSystemIsWindowsNT			= $00020000;

	{  constants for MediaPropertiesAtom }
	kMediaPropertyNonLinearAtomType = $6E6F6E6C (* 'nonl' *);
	kMediaPropertyHasActions	= 105;

	{	 TimeBase and TimeRecord moved to MacTypes.h 	}

type
	TimeBaseFlags 				= UInt32;
const
	loopTimeBase				= 1;
	palindromeLoopTimeBase		= 2;
	maintainTimeBaseZero		= 4;


type
	CallBackRecordPtr = ^CallBackRecord;
	CallBackRecord = record
		data:					array [0..0] of SInt32;
	end;

	QTCallBack							= ^CallBackRecord;
	{	 CallBack equates 	}
	QTCallBackFlags 			= UInt16;
const
	triggerTimeFwd				= $0001;						{  when curTime exceeds triggerTime going forward  }
	triggerTimeBwd				= $0002;						{  when curTime exceeds triggerTime going backwards  }
	triggerTimeEither			= $0003;						{  when curTime exceeds triggerTime going either direction  }
	triggerRateLT				= $0004;						{  when rate changes to less than trigger value  }
	triggerRateGT				= $0008;						{  when rate changes to greater than trigger value  }
	triggerRateEqual			= $0010;						{  when rate changes to equal trigger value  }
	triggerRateLTE				= $0014;
	triggerRateGTE				= $0018;
	triggerRateNotEqual			= $001C;
	triggerRateChange			= 0;
	triggerAtStart				= $0001;
	triggerAtStop				= $0002;


type
	TimeBaseStatus 				= UInt32;
const
	timeBaseBeforeStartTime		= 1;
	timeBaseAfterStopTime		= 2;


type
	QTCallBackType 				= UInt16;
const
	callBackAtTime				= 1;
	callBackAtRate				= 2;
	callBackAtTimeJump			= 3;
	callBackAtExtremes			= 4;
	callBackAtTimeBaseDisposed	= 5;
	callBackAtInterrupt			= $8000;
	callBackAtDeferredTask		= $4000;


type
{$ifc TYPED_FUNCTION_POINTERS}
	QTCallBackProcPtr = procedure(cb: QTCallBack; refCon: SInt32);
{$elsec}
	QTCallBackProcPtr = ProcPtr;
{$endc}

{$ifc OPAQUE_UPP_TYPES}
	QTCallBackUPP = ^SInt32; { an opaque UPP }
{$elsec}
	QTCallBackUPP = UniversalProcPtr;
{$endc}	

const
	qtcbNeedsRateChanges		= 1;							{  wants to know about rate changes  }
	qtcbNeedsTimeChanges		= 2;							{  wants to know about time changes  }
	qtcbNeedsStartStopChanges	= 4;							{  wants to know when TimeBase start/stop is changed }


type
	QTCallBackHeaderPtr = ^QTCallBackHeader;
	QTCallBackHeader = record
		callBackFlags:			SInt32;
		reserved1:				SInt32;
		qtPrivate:				array [0..39] of SInt8;
	end;

{$ifc TYPED_FUNCTION_POINTERS}
	QTSyncTaskProcPtr = procedure(task: UnivPtr);
{$elsec}
	QTSyncTaskProcPtr = ProcPtr;
{$endc}

{$ifc OPAQUE_UPP_TYPES}
	QTSyncTaskUPP = ^SInt32; { an opaque UPP }
{$elsec}
	QTSyncTaskUPP = UniversalProcPtr;
{$endc}	
	QTSyncTaskRecordPtr = ^QTSyncTaskRecord;
	QTSyncTaskRecord = record
		qLink:					Ptr;
		proc:					QTSyncTaskUPP;
	end;

	QTSyncTaskPtr						= ^QTSyncTaskRecord;

{$ifc TYPED_FUNCTION_POINTERS}
	MovieRgnCoverProcPtr = function(theMovie: Movie; changedRgn: RgnHandle; refcon: SInt32): OSErr;
{$elsec}
	MovieRgnCoverProcPtr = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	MovieProgressProcPtr = function(theMovie: Movie; message: SInt16; whatOperation: SInt16; percentDone: Fixed; refcon: SInt32): OSErr;
{$elsec}
	MovieProgressProcPtr = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	MovieDrawingCompleteProcPtr = function(theMovie: Movie; refCon: SInt32): OSErr;
{$elsec}
	MovieDrawingCompleteProcPtr = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	TrackTransferProcPtr = function(t: Track; refCon: SInt32): OSErr;
{$elsec}
	TrackTransferProcPtr = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	GetMovieProcPtr = function(offset: SInt32; size: SInt32; dataPtr: UnivPtr; refCon: UnivPtr): OSErr;
{$elsec}
	GetMovieProcPtr = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	MoviePreviewCallOutProcPtr = function(refcon: SInt32): boolean;
{$elsec}
	MoviePreviewCallOutProcPtr = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	TextMediaProcPtr = function(theText: Handle; theMovie: Movie; var displayFlag: SInt16; refcon: SInt32): OSErr;
{$elsec}
	TextMediaProcPtr = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	ActionsProcPtr = function(refcon: UnivPtr; targetTrack: Track; targetRefCon: SInt32; theEvent: QTEventRecordPtr): OSErr;
{$elsec}
	ActionsProcPtr = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	DoMCActionProcPtr = function(refcon: UnivPtr; action: SInt16; params: UnivPtr; var handled: boolean): OSErr;
{$elsec}
	DoMCActionProcPtr = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	MovieExecuteWiredActionsProcPtr = function(theMovie: Movie; refcon: UnivPtr; flags: SInt32; wiredActions: QTAtomContainer): OSErr;
{$elsec}
	MovieExecuteWiredActionsProcPtr = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	MoviePrePrerollCompleteProcPtr = procedure(theMovie: Movie; prerollErr: OSErr; refcon: UnivPtr);
{$elsec}
	MoviePrePrerollCompleteProcPtr = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	QTNextTaskNeededSoonerCallbackProcPtr = procedure(duration: TimeValue; flags: UInt32; refcon: UnivPtr);
{$elsec}
	QTNextTaskNeededSoonerCallbackProcPtr = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	MoviesErrorProcPtr = procedure(theErr: OSErr; refcon: SInt32);
{$elsec}
	MoviesErrorProcPtr = ProcPtr;
{$endc}

{$ifc OPAQUE_UPP_TYPES}
	MovieRgnCoverUPP = ^SInt32; { an opaque UPP }
{$elsec}
	MovieRgnCoverUPP = UniversalProcPtr;
{$endc}	
{$ifc OPAQUE_UPP_TYPES}
	MovieProgressUPP = ^SInt32; { an opaque UPP }
{$elsec}
	MovieProgressUPP = UniversalProcPtr;
{$endc}	
{$ifc OPAQUE_UPP_TYPES}
	MovieDrawingCompleteUPP = ^SInt32; { an opaque UPP }
{$elsec}
	MovieDrawingCompleteUPP = UniversalProcPtr;
{$endc}	
{$ifc OPAQUE_UPP_TYPES}
	TrackTransferUPP = ^SInt32; { an opaque UPP }
{$elsec}
	TrackTransferUPP = UniversalProcPtr;
{$endc}	
{$ifc OPAQUE_UPP_TYPES}
	GetMovieUPP = ^SInt32; { an opaque UPP }
{$elsec}
	GetMovieUPP = UniversalProcPtr;
{$endc}	
{$ifc OPAQUE_UPP_TYPES}
	MoviePreviewCallOutUPP = ^SInt32; { an opaque UPP }
{$elsec}
	MoviePreviewCallOutUPP = UniversalProcPtr;
{$endc}	
{$ifc OPAQUE_UPP_TYPES}
	TextMediaUPP = ^SInt32; { an opaque UPP }
{$elsec}
	TextMediaUPP = UniversalProcPtr;
{$endc}	
{$ifc OPAQUE_UPP_TYPES}
	ActionsUPP = ^SInt32; { an opaque UPP }
{$elsec}
	ActionsUPP = UniversalProcPtr;
{$endc}	
{$ifc OPAQUE_UPP_TYPES}
	DoMCActionUPP = ^SInt32; { an opaque UPP }
{$elsec}
	DoMCActionUPP = UniversalProcPtr;
{$endc}	
{$ifc OPAQUE_UPP_TYPES}
	MovieExecuteWiredActionsUPP = ^SInt32; { an opaque UPP }
{$elsec}
	MovieExecuteWiredActionsUPP = UniversalProcPtr;
{$endc}	
{$ifc OPAQUE_UPP_TYPES}
	MoviePrePrerollCompleteUPP = ^SInt32; { an opaque UPP }
{$elsec}
	MoviePrePrerollCompleteUPP = UniversalProcPtr;
{$endc}	
{$ifc OPAQUE_UPP_TYPES}
	QTNextTaskNeededSoonerCallbackUPP = ^SInt32; { an opaque UPP }
{$elsec}
	QTNextTaskNeededSoonerCallbackUPP = UniversalProcPtr;
{$endc}	
{$ifc OPAQUE_UPP_TYPES}
	MoviesErrorUPP = ^SInt32; { an opaque UPP }
{$elsec}
	MoviesErrorUPP = UniversalProcPtr;
{$endc}	
	MediaHandler						= ComponentInstance;
	DataHandler							= ComponentInstance;
	MediaHandlerComponent				= Component;
	DataHandlerComponent				= Component;
	HandlerError						= ComponentResult;

const
	keepInRam					= $01;							{  load and make non-purgable }
	unkeepInRam					= $02;							{  mark as purgable }
	flushFromRam				= $04;							{  empty those handles }
	loadForwardTrackEdits		= $08;							{     load track edits into ram for playing forward }
	loadBackwardTrackEdits		= $10;							{     load track edits into ram for playing in reverse }

	newMovieActive				= $01;
	newMovieDontResolveDataRefs	= $02;
	newMovieDontAskUnresolvedDataRefs = $04;
	newMovieDontAutoAlternates	= $08;
	newMovieDontUpdateForeBackPointers = $10;
	newMovieDontAutoUpdateClock	= $20;
	newMovieAsyncOK				= $0100;
	newMovieIdleImportOK		= $0400;
	newMovieDontInteractWithUser = $0800;

	{	 track usage bits 	}
	trackUsageInMovie			= $02;
	trackUsageInPreview			= $04;
	trackUsageInPoster			= $08;

	{	 Add/GetMediaSample flags 	}
	mediaSampleNotSync			= $01;							{  sample is not a sync sample (eg. is frame differenced  }
	mediaSampleShadowSync		= $02;							{  sample is a shadow sync  }

	pasteInParallel				= $01;
	showUserSettingsDialog		= $02;
	movieToFileOnlyExport		= $04;
	movieFileSpecValid			= $08;

	nextTimeMediaSample			= $01;
	nextTimeMediaEdit			= $02;
	nextTimeTrackEdit			= $04;
	nextTimeSyncSample			= $08;
	nextTimeStep				= $10;
	nextTimeEdgeOK				= $4000;
	nextTimeIgnoreActiveSegment	= $8000;


type
	nextTimeFlagsEnum					= UInt16;

const
	createMovieFileDeleteCurFile = $80000000;
	createMovieFileDontCreateMovie = $40000000;
	createMovieFileDontOpenFile	= $20000000;
	createMovieFileDontCreateResFile = $10000000;


type
	createMovieFileFlagsEnum			= UInt32;

const
	flattenAddMovieToDataFork	= $00000001;
	flattenActiveTracksOnly		= $00000004;
	flattenDontInterleaveFlatten = $00000008;
	flattenFSSpecPtrIsDataRefRecordPtr = $00000010;
	flattenCompressMovieResource = $00000020;
	flattenForceMovieResourceBeforeMovieData = $00000040;


type
	movieFlattenFlagsEnum				= UInt32;

const
	movieInDataForkResID		= -1;							{  magic res ID  }

	mcTopLeftMovie				= $01;							{  usually centered  }
	mcScaleMovieToFit			= $02;							{  usually only scales down  }
	mcWithBadge					= $04;							{  give me a badge  }
	mcNotVisible				= $08;							{  don't show controller  }
	mcWithFrame					= $10;							{  gimme a frame  }

	movieScrapDontZeroScrap		= $01;
	movieScrapOnlyPutMovie		= $02;

	dataRefSelfReference		= $01;
	dataRefWasNotResolved		= $02;


type
	dataRefAttributesFlags				= UInt32;

const
	kMovieAnchorDataRefIsDefault = $01;							{  data ref returned is movie default data ref  }

	hintsScrubMode				= $01;							{  mask == && (if flags == scrub on, flags != scrub off)  }
	hintsLoop					= $02;
	hintsDontPurge				= $04;
	hintsUseScreenBuffer		= $20;
	hintsAllowInterlace			= $40;
	hintsUseSoundInterp			= $80;
	hintsHighQuality			= $0100;						{  slooooow  }
	hintsPalindrome				= $0200;
	hintsInactive				= $0800;
	hintsOffscreen				= $1000;
	hintsDontDraw				= $2000;
	hintsAllowBlacklining		= $4000;
	hintsDontUseVideoOverlaySurface = $00010000;
	hintsIgnoreBandwidthRestrictions = $00020000;
	hintsPlayingEveryFrame		= $00040000;
	hintsAllowDynamicResize		= $00080000;
	hintsSingleField			= $00100000;
	hintsNoRenderingTimeOut		= $00200000;
	hintsFlushVideoInsteadOfDirtying = $00400000;
	hintsEnableSubPixelPositioning = $00800000;


type
	playHintsEnum						= UInt32;

const
	mediaHandlerFlagBaseClient	= 1;


type
	mediaHandlerFlagsEnum				= UInt32;

const
	movieTrackMediaType			= $01;
	movieTrackCharacteristic	= $02;
	movieTrackEnabledOnly		= $04;


type
	SampleReferenceRecordPtr = ^SampleReferenceRecord;
	SampleReferenceRecord = record
		dataOffset:				SInt32;
		dataSize:				SInt32;
		durationPerSample:		TimeValue;
		numberOfSamples:		SInt32;
		sampleFlags:			SInt16;
	end;

	SampleReferencePtr					= ^SampleReferenceRecord;
	SampleReference64RecordPtr = ^SampleReference64Record;
	SampleReference64Record = record
		dataOffset:				wide;
		dataSize:				UInt32;
		durationPerSample:		TimeValue;
		numberOfSamples:		UInt32;
		sampleFlags:			SInt16;
	end;

	SampleReference64Ptr				= ^SampleReference64Record;

	{	************************
	* Initialization Routines 
	*************************	}
	{
	 *  CheckQuickTimeRegistration()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
	 *    CarbonLib:        in CarbonLib 1.1 and later
	 *    Mac OS X:         in version 10.0 and later
	 *    Windows:          in qtmlClient.lib 3.0 and later
	 	}
procedure CheckQuickTimeRegistration(registrationKey: UnivPtr; flags: SInt32); external name '_CheckQuickTimeRegistration';
{
 *  EnterMovies()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function EnterMovies: OSErr; external name '_EnterMovies';
{
 *  ExitMovies()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure ExitMovies; external name '_ExitMovies';
{************************
* Error Routines 
*************************}

{
 *  GetMoviesError()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetMoviesError: OSErr; external name '_GetMoviesError';
{
 *  ClearMoviesStickyError()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure ClearMoviesStickyError; external name '_ClearMoviesStickyError';
{
 *  GetMoviesStickyError()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetMoviesStickyError: OSErr; external name '_GetMoviesStickyError';
{
 *  SetMoviesErrorProc()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure SetMoviesErrorProc(errProc: MoviesErrorUPP; refcon: SInt32); external name '_SetMoviesErrorProc';
{************************
* Idle Routines 
*************************}
{
 *  MoviesTask()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure MoviesTask(theMovie: Movie; maxMilliSecToUse: SInt32); external name '_MoviesTask';
{
 *  PrerollMovie()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function PrerollMovie(theMovie: Movie; time: TimeValue; Rate: Fixed): OSErr; external name '_PrerollMovie';
{
 *  PrePrerollMovie()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function PrePrerollMovie(m: Movie; time: TimeValue; rate: Fixed; proc: MoviePrePrerollCompleteUPP; refcon: UnivPtr): OSErr; external name '_PrePrerollMovie';
{
 *  AbortPrePrerollMovie()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
procedure AbortPrePrerollMovie(m: Movie; err: OSErr); external name '_AbortPrePrerollMovie';
{
 *  LoadMovieIntoRam()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function LoadMovieIntoRam(theMovie: Movie; time: TimeValue; duration: TimeValue; flags: SInt32): OSErr; external name '_LoadMovieIntoRam';
{
 *  LoadTrackIntoRam()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function LoadTrackIntoRam(theTrack: Track; time: TimeValue; duration: TimeValue; flags: SInt32): OSErr; external name '_LoadTrackIntoRam';
{
 *  LoadMediaIntoRam()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function LoadMediaIntoRam(theMedia: Media; time: TimeValue; duration: TimeValue; flags: SInt32): OSErr; external name '_LoadMediaIntoRam';
{
 *  SetMovieActive()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure SetMovieActive(theMovie: Movie; active: boolean); external name '_SetMovieActive';
{
 *  GetMovieActive()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetMovieActive(theMovie: Movie): boolean; external name '_GetMovieActive';
{
 *  QTGetWallClockTimeBase()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 6.0 and later
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.2 and later
 *    Windows:          in qtmlClient.lib 6.0 and later
 }
function QTGetWallClockTimeBase(var wallClockTimeBase: TimeBase): OSErr; external name '_QTGetWallClockTimeBase';
{************************
* Idle Management
*************************}

type
	IdleManager    = ^SInt32; { an opaque 32-bit type }
	IdleManagerPtr = ^IdleManager;  { when a var xx:IdleManager parameter can be nil, it is changed to xx: IdleManagerPtr }
	{
	 *  QTIdleManagerOpen()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in QuickTimeLib 6.0 and later
	 *    CarbonLib:        in CarbonLib 1.6 and later
	 *    Mac OS X:         in version 10.2 and later
	 *    Windows:          in qtmlClient.lib 6.0 and later
	 	}
function QTIdleManagerOpen: IdleManager; external name '_QTIdleManagerOpen';
{
 *  QTIdleManagerClose()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 6.0 and later
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.2 and later
 *    Windows:          in qtmlClient.lib 6.0 and later
 }
function QTIdleManagerClose(im: IdleManager): OSErr; external name '_QTIdleManagerClose';
{
 *  QTIdleManagerGetNextIdleTime()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 6.0 and later
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.2 and later
 *    Windows:          in qtmlClient.lib 6.0 and later
 }
function QTIdleManagerGetNextIdleTime(im: IdleManager; var nextIdle: TimeRecord): OSErr; external name '_QTIdleManagerGetNextIdleTime';
{
 *  QTIdleManagerSetNextIdleTime()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 6.0 and later
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.2 and later
 *    Windows:          in qtmlClient.lib 6.0 and later
 }
function QTIdleManagerSetNextIdleTime(im: IdleManager; var nextIdle: TimeRecord): OSErr; external name '_QTIdleManagerSetNextIdleTime';
{
 *  QTIdleManagerSetNextIdleTimeNever()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 6.0 and later
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.2 and later
 *    Windows:          in qtmlClient.lib 6.0 and later
 }
function QTIdleManagerSetNextIdleTimeNever(im: IdleManager): OSErr; external name '_QTIdleManagerSetNextIdleTimeNever';
{
 *  QTIdleManagerSetNextIdleTimeNow()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 6.0 and later
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.2 and later
 *    Windows:          in qtmlClient.lib 6.0 and later
 }
function QTIdleManagerSetNextIdleTimeNow(im: IdleManager): OSErr; external name '_QTIdleManagerSetNextIdleTimeNow';
{
 *  QTIdleManagerSetNextIdleTimeDelta()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 6.0 and later
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.2 and later
 *    Windows:          in qtmlClient.lib 6.0 and later
 }
function QTIdleManagerSetNextIdleTimeDelta(im: IdleManager; duration: TimeValue; scale: TimeScale): OSErr; external name '_QTIdleManagerSetNextIdleTimeDelta';
{
 *  QTIdleManagerSetParent()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 6.0 and later
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.2 and later
 *    Windows:          in qtmlClient.lib 6.0 and later
 }
function QTIdleManagerSetParent(im: IdleManager; parent: IdleManager): OSErr; external name '_QTIdleManagerSetParent';
{
 *  QTIdleManagerNeedsAnIdle()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 6.0 and later
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.2 and later
 *    Windows:          in qtmlClient.lib 6.0 and later
 }
function QTIdleManagerNeedsAnIdle(im: IdleManager; var needsOne: boolean): OSErr; external name '_QTIdleManagerNeedsAnIdle';
{************************
* Carbon Movie Control
*************************}
{  Movie Control option bits }

const
	kMovieControlOptionHideController = $00000001;
	kMovieControlOptionLocateTopLeft = $00000002;
	kMovieControlOptionEnableEditing = $00000004;
	kMovieControlOptionHandleEditingHI = $00000008;
	kMovieControlOptionSetKeysEnabled = $00000010;
	kMovieControlOptionManuallyIdled = $00000020;

	{  Item tags for use in GetControlData() (some with SetControlData()) calls on Movie Controls }
	kMovieControlDataMovieController = $6D632020 (* 'mc  ' *);
	kMovieControlDataMovie		= $6D6F6F76 (* 'moov' *);
	kMovieControlDataManualIdling = $6D616E75 (* 'manu' *);

	{	
	** CreateMovieControl() -   This is the public API routine that creates a Movie Control. Given a window and location
	**                          plus a movie, it constructs a Movie Control with a Movie Controller in the window.
		}
	{
	 *  CreateMovieControl()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   not available
	 *    CarbonLib:        in CarbonLib 1.6 and later
	 *    Mac OS X:         in version 10.2 and later
	 	}
function CreateMovieControl(theWindow: WindowRef; var localRect: Rect; theMovie: Movie; options: UInt32; var returnedControl: ControlRef): OSErr; external name '_CreateMovieControl';
{************************
* calls for playing movies, previews, posters
*************************}
{
 *  StartMovie()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure StartMovie(theMovie: Movie); external name '_StartMovie';
{
 *  StopMovie()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure StopMovie(theMovie: Movie); external name '_StopMovie';
{
 *  GoToBeginningOfMovie()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure GoToBeginningOfMovie(theMovie: Movie); external name '_GoToBeginningOfMovie';
{
 *  GoToEndOfMovie()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure GoToEndOfMovie(theMovie: Movie); external name '_GoToEndOfMovie';
{
 *  IsMovieDone()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function IsMovieDone(theMovie: Movie): boolean; external name '_IsMovieDone';
{
 *  GetMoviePreviewMode()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetMoviePreviewMode(theMovie: Movie): boolean; external name '_GetMoviePreviewMode';
{
 *  SetMoviePreviewMode()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure SetMoviePreviewMode(theMovie: Movie; usePreview: boolean); external name '_SetMoviePreviewMode';
{
 *  ShowMoviePoster()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure ShowMoviePoster(theMovie: Movie); external name '_ShowMoviePoster';
{
 *  PlayMoviePreview()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure PlayMoviePreview(theMovie: Movie; callOutProc: MoviePreviewCallOutUPP; refcon: SInt32); external name '_PlayMoviePreview';
{************************
* calls for controlling movies & tracks which are playing
*************************}
{
 *  GetMovieTimeBase()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetMovieTimeBase(theMovie: Movie): TimeBase; external name '_GetMovieTimeBase';
{
 *  SetMovieMasterTimeBase()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure SetMovieMasterTimeBase(theMovie: Movie; tb: TimeBase; const (*var*) slaveZero: TimeRecord); external name '_SetMovieMasterTimeBase';
{
 *  SetMovieMasterClock()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure SetMovieMasterClock(theMovie: Movie; clockMeister: Component; const (*var*) slaveZero: TimeRecord); external name '_SetMovieMasterClock';
{
 *  ChooseMovieClock()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 6.0 and later
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.2 and later
 *    Windows:          in qtmlClient.lib 6.0 and later
 }
procedure ChooseMovieClock(m: Movie; flags: SInt32); external name '_ChooseMovieClock';
{
 *  GetMovieGWorld()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure GetMovieGWorld(theMovie: Movie; var port: CGrafPtr; var gdh: GDHandle); external name '_GetMovieGWorld';
{
 *  SetMovieGWorld()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure SetMovieGWorld(theMovie: Movie; port: CGrafPtr; gdh: GDHandle); external name '_SetMovieGWorld';
const
	movieDrawingCallWhenChanged	= 0;
	movieDrawingCallAlways		= 1;

	{
	 *  SetMovieDrawingCompleteProc()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 *    Windows:          in qtmlClient.lib 3.0 and later
	 	}
procedure SetMovieDrawingCompleteProc(theMovie: Movie; flags: SInt32; proc: MovieDrawingCompleteUPP; refCon: SInt32); external name '_SetMovieDrawingCompleteProc';
{
 *  GetMovieNaturalBoundsRect()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure GetMovieNaturalBoundsRect(theMovie: Movie; var naturalBounds: Rect); external name '_GetMovieNaturalBoundsRect';
{
 *  GetNextTrackForCompositing()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetNextTrackForCompositing(theMovie: Movie; theTrack: Track): Track; external name '_GetNextTrackForCompositing';
{
 *  GetPrevTrackForCompositing()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetPrevTrackForCompositing(theMovie: Movie; theTrack: Track): Track; external name '_GetPrevTrackForCompositing';
{
 *  SetTrackGWorld()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure SetTrackGWorld(theTrack: Track; port: CGrafPtr; gdh: GDHandle; proc: TrackTransferUPP; refCon: SInt32); external name '_SetTrackGWorld';
{
 *  GetMoviePict()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetMoviePict(theMovie: Movie; time: TimeValue): PicHandle; external name '_GetMoviePict';
{
 *  GetTrackPict()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetTrackPict(theTrack: Track; time: TimeValue): PicHandle; external name '_GetTrackPict';
{
 *  GetMoviePosterPict()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetMoviePosterPict(theMovie: Movie): PicHandle; external name '_GetMoviePosterPict';
{ called between Begin & EndUpdate }
{
 *  UpdateMovie()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function UpdateMovie(theMovie: Movie): OSErr; external name '_UpdateMovie';
{
 *  InvalidateMovieRegion()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function InvalidateMovieRegion(theMovie: Movie; invalidRgn: RgnHandle): OSErr; external name '_InvalidateMovieRegion';
{*** spatial movie routines ***}
{
 *  GetMovieBox()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure GetMovieBox(theMovie: Movie; var boxRect: Rect); external name '_GetMovieBox';
{
 *  SetMovieBox()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure SetMovieBox(theMovie: Movie; const (*var*) boxRect: Rect); external name '_SetMovieBox';
{* movie display clip }
{
 *  GetMovieDisplayClipRgn()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetMovieDisplayClipRgn(theMovie: Movie): RgnHandle; external name '_GetMovieDisplayClipRgn';
{
 *  SetMovieDisplayClipRgn()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure SetMovieDisplayClipRgn(theMovie: Movie; theClip: RgnHandle); external name '_SetMovieDisplayClipRgn';
{* movie src clip }
{
 *  GetMovieClipRgn()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetMovieClipRgn(theMovie: Movie): RgnHandle; external name '_GetMovieClipRgn';
{
 *  SetMovieClipRgn()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure SetMovieClipRgn(theMovie: Movie; theClip: RgnHandle); external name '_SetMovieClipRgn';
{* track src clip }
{
 *  GetTrackClipRgn()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetTrackClipRgn(theTrack: Track): RgnHandle; external name '_GetTrackClipRgn';
{
 *  SetTrackClipRgn()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure SetTrackClipRgn(theTrack: Track; theClip: RgnHandle); external name '_SetTrackClipRgn';
{* bounds in display space (not clipped by display clip) }
{
 *  GetMovieDisplayBoundsRgn()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetMovieDisplayBoundsRgn(theMovie: Movie): RgnHandle; external name '_GetMovieDisplayBoundsRgn';
{
 *  GetTrackDisplayBoundsRgn()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetTrackDisplayBoundsRgn(theTrack: Track): RgnHandle; external name '_GetTrackDisplayBoundsRgn';
{* bounds in movie space }
{
 *  GetMovieBoundsRgn()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetMovieBoundsRgn(theMovie: Movie): RgnHandle; external name '_GetMovieBoundsRgn';
{
 *  GetTrackMovieBoundsRgn()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetTrackMovieBoundsRgn(theTrack: Track): RgnHandle; external name '_GetTrackMovieBoundsRgn';
{* bounds in track space }
{
 *  GetTrackBoundsRgn()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetTrackBoundsRgn(theTrack: Track): RgnHandle; external name '_GetTrackBoundsRgn';
{* mattes - always in track space }
{
 *  GetTrackMatte()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetTrackMatte(theTrack: Track): PixMapHandle; external name '_GetTrackMatte';
{
 *  SetTrackMatte()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure SetTrackMatte(theTrack: Track; theMatte: PixMapHandle); external name '_SetTrackMatte';
{
 *  DisposeMatte()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure DisposeMatte(theMatte: PixMapHandle); external name '_DisposeMatte';
{* video out }
{
 *  SetMovieVideoOutput()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 5.0 and later
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 5.0 and later
 }
procedure SetMovieVideoOutput(theMovie: Movie; vout: ComponentInstance); external name '_SetMovieVideoOutput';
{************************
* calls for getting/saving movies
*************************}
{
 *  NewMovie()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function NewMovie(flags: SInt32): Movie; external name '_NewMovie';
{
 *  PutMovieIntoHandle()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function PutMovieIntoHandle(theMovie: Movie; publicMovie: Handle): OSErr; external name '_PutMovieIntoHandle';
{
 *  PutMovieIntoDataFork()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function PutMovieIntoDataFork(theMovie: Movie; fRefNum: SInt16; offset: SInt32; maxSize: SInt32): OSErr; external name '_PutMovieIntoDataFork';
{
 *  PutMovieIntoDataFork64()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function PutMovieIntoDataFork64(theMovie: Movie; fRefNum: SInt32; const (*var*) offset: wide; maxSize: UInt32): OSErr; external name '_PutMovieIntoDataFork64';
{
 *  PutMovieIntoStorage()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 6.0 and later
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.2 and later
 *    Windows:          in qtmlClient.lib 6.0 and later
 }
function PutMovieIntoStorage(theMovie: Movie; dh: DataHandler; const (*var*) offset: wide; maxSize: UInt32): OSErr; external name '_PutMovieIntoStorage';
{
 *  PutMovieForDataRefIntoHandle()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 6.0 and later
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.2 and later
 *    Windows:          in qtmlClient.lib 6.0 and later
 }
function PutMovieForDataRefIntoHandle(theMovie: Movie; dataRef: Handle; dataRefType: OSType; publicMovie: Handle): OSErr; external name '_PutMovieForDataRefIntoHandle';
{
 *  DisposeMovie()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure DisposeMovie(theMovie: Movie); external name '_DisposeMovie';
{************************
* Movie State Routines
*************************}
{
 *  GetMovieCreationTime()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetMovieCreationTime(theMovie: Movie): UInt32; external name '_GetMovieCreationTime';
{
 *  GetMovieModificationTime()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetMovieModificationTime(theMovie: Movie): UInt32; external name '_GetMovieModificationTime';
{
 *  GetMovieTimeScale()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetMovieTimeScale(theMovie: Movie): TimeScale; external name '_GetMovieTimeScale';
{
 *  SetMovieTimeScale()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure SetMovieTimeScale(theMovie: Movie; timeScale_: TimeScale); external name '_SetMovieTimeScale';
{
 *  GetMovieDuration()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetMovieDuration(theMovie: Movie): TimeValue; external name '_GetMovieDuration';
{
 *  GetMovieRate()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetMovieRate(theMovie: Movie): Fixed; external name '_GetMovieRate';
{
 *  SetMovieRate()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure SetMovieRate(theMovie: Movie; rate: Fixed); external name '_SetMovieRate';
{
 *  GetMoviePreferredRate()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetMoviePreferredRate(theMovie: Movie): Fixed; external name '_GetMoviePreferredRate';
{
 *  SetMoviePreferredRate()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure SetMoviePreferredRate(theMovie: Movie; rate: Fixed); external name '_SetMoviePreferredRate';
{
 *  GetMoviePreferredVolume()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetMoviePreferredVolume(theMovie: Movie): SInt16; external name '_GetMoviePreferredVolume';
{
 *  SetMoviePreferredVolume()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure SetMoviePreferredVolume(theMovie: Movie; volume: SInt16); external name '_SetMoviePreferredVolume';
{
 *  GetMovieVolume()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetMovieVolume(theMovie: Movie): SInt16; external name '_GetMovieVolume';
{
 *  SetMovieVolume()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure SetMovieVolume(theMovie: Movie; volume: SInt16); external name '_SetMovieVolume';
{
 *  GetMovieMatrix()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure GetMovieMatrix(theMovie: Movie; var matrix: MatrixRecord); external name '_GetMovieMatrix';
{
 *  SetMovieMatrix()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure SetMovieMatrix(theMovie: Movie; const (*var*) matrix: MatrixRecord); external name '_SetMovieMatrix';
{
 *  GetMoviePreviewTime()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure GetMoviePreviewTime(theMovie: Movie; var previewTime: TimeValue; var previewDuration: TimeValue); external name '_GetMoviePreviewTime';
{
 *  SetMoviePreviewTime()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure SetMoviePreviewTime(theMovie: Movie; previewTime: TimeValue; previewDuration: TimeValue); external name '_SetMoviePreviewTime';
{
 *  GetMoviePosterTime()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetMoviePosterTime(theMovie: Movie): TimeValue; external name '_GetMoviePosterTime';
{
 *  SetMoviePosterTime()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure SetMoviePosterTime(theMovie: Movie; posterTime: TimeValue); external name '_SetMoviePosterTime';
{
 *  GetMovieSelection()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure GetMovieSelection(theMovie: Movie; var selectionTime: TimeValue; var selectionDuration: TimeValue); external name '_GetMovieSelection';
{
 *  SetMovieSelection()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure SetMovieSelection(theMovie: Movie; selectionTime: TimeValue; selectionDuration: TimeValue); external name '_SetMovieSelection';
{
 *  SetMovieActiveSegment()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure SetMovieActiveSegment(theMovie: Movie; startTime: TimeValue; duration: TimeValue); external name '_SetMovieActiveSegment';
{
 *  GetMovieActiveSegment()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure GetMovieActiveSegment(theMovie: Movie; var startTime: TimeValue; var duration: TimeValue); external name '_GetMovieActiveSegment';
{
 *  GetMovieTime()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetMovieTime(theMovie: Movie; var currentTime: TimeRecord): TimeValue; external name '_GetMovieTime';
{
 *  SetMovieTime()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure SetMovieTime(theMovie: Movie; const (*var*) newtime: TimeRecord); external name '_SetMovieTime';
{
 *  SetMovieTimeValue()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure SetMovieTimeValue(theMovie: Movie; newtime: TimeValue); external name '_SetMovieTimeValue';
{
 *  GetMovieUserData()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetMovieUserData(theMovie: Movie): UserData; external name '_GetMovieUserData';
{
 *  QTGetTimeUntilNextTask()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 6.0 and later
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.2 and later
 *    Windows:          in qtmlClient.lib 6.0 and later
 }
function QTGetTimeUntilNextTask(var duration: SInt32; scale: SInt32): OSErr; external name '_QTGetTimeUntilNextTask';
{
 *  QTInstallNextTaskNeededSoonerCallback()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 6.0 and later
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.2 and later
 *    Windows:          in qtmlClient.lib 6.0 and later
 }
function QTInstallNextTaskNeededSoonerCallback(callbackProc: QTNextTaskNeededSoonerCallbackUPP; scale: TimeScale; flags: UInt32; refcon: UnivPtr): OSErr; external name '_QTInstallNextTaskNeededSoonerCallback';
{
 *  QTUninstallNextTaskNeededSoonerCallback()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 6.0 and later
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.2 and later
 *    Windows:          in qtmlClient.lib 6.0 and later
 }
function QTUninstallNextTaskNeededSoonerCallback(callbackProc: QTNextTaskNeededSoonerCallbackUPP; refcon: UnivPtr): OSErr; external name '_QTUninstallNextTaskNeededSoonerCallback';
{************************
* Track/Media finding routines
*************************}
{
 *  GetMovieTrackCount()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetMovieTrackCount(theMovie: Movie): SInt32; external name '_GetMovieTrackCount';
{
 *  GetMovieTrack()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetMovieTrack(theMovie: Movie; trackID: SInt32): Track; external name '_GetMovieTrack';
{
 *  GetMovieIndTrack()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetMovieIndTrack(theMovie: Movie; index: SInt32): Track; external name '_GetMovieIndTrack';
{
 *  GetMovieIndTrackType()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetMovieIndTrackType(theMovie: Movie; index: SInt32; trackType: OSType; flags: SInt32): Track; external name '_GetMovieIndTrackType';
{
 *  GetTrackID()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetTrackID(theTrack: Track): SInt32; external name '_GetTrackID';
{
 *  GetTrackMovie()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetTrackMovie(theTrack: Track): Movie; external name '_GetTrackMovie';
{************************
* Track creation routines
*************************}
{
 *  NewMovieTrack()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function NewMovieTrack(theMovie: Movie; width: Fixed; height: Fixed; trackVolume: SInt16): Track; external name '_NewMovieTrack';
{
 *  DisposeMovieTrack()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure DisposeMovieTrack(theTrack: Track); external name '_DisposeMovieTrack';
{************************
* Track State routines
*************************}
{
 *  GetTrackCreationTime()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetTrackCreationTime(theTrack: Track): UInt32; external name '_GetTrackCreationTime';
{
 *  GetTrackModificationTime()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetTrackModificationTime(theTrack: Track): UInt32; external name '_GetTrackModificationTime';
{
 *  GetTrackEnabled()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetTrackEnabled(theTrack: Track): boolean; external name '_GetTrackEnabled';
{
 *  SetTrackEnabled()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure SetTrackEnabled(theTrack: Track; isEnabled: boolean); external name '_SetTrackEnabled';
{
 *  GetTrackUsage()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetTrackUsage(theTrack: Track): SInt32; external name '_GetTrackUsage';
{
 *  SetTrackUsage()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure SetTrackUsage(theTrack: Track; usage: SInt32); external name '_SetTrackUsage';
{
 *  GetTrackDuration()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetTrackDuration(theTrack: Track): TimeValue; external name '_GetTrackDuration';
{
 *  GetTrackOffset()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetTrackOffset(theTrack: Track): TimeValue; external name '_GetTrackOffset';
{
 *  SetTrackOffset()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure SetTrackOffset(theTrack: Track; movieOffsetTime: TimeValue); external name '_SetTrackOffset';
{
 *  GetTrackLayer()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetTrackLayer(theTrack: Track): SInt16; external name '_GetTrackLayer';
{
 *  SetTrackLayer()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure SetTrackLayer(theTrack: Track; layer: SInt16); external name '_SetTrackLayer';
{
 *  GetTrackAlternate()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetTrackAlternate(theTrack: Track): Track; external name '_GetTrackAlternate';
{
 *  SetTrackAlternate()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure SetTrackAlternate(theTrack: Track; alternateT: Track); external name '_SetTrackAlternate';
{
 *  SetAutoTrackAlternatesEnabled()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure SetAutoTrackAlternatesEnabled(theMovie: Movie; enable: boolean); external name '_SetAutoTrackAlternatesEnabled';
{
 *  SelectMovieAlternates()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure SelectMovieAlternates(theMovie: Movie); external name '_SelectMovieAlternates';
{
 *  GetTrackVolume()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetTrackVolume(theTrack: Track): SInt16; external name '_GetTrackVolume';
{
 *  SetTrackVolume()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure SetTrackVolume(theTrack: Track; volume: SInt16); external name '_SetTrackVolume';
{
 *  GetTrackMatrix()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure GetTrackMatrix(theTrack: Track; var matrix: MatrixRecord); external name '_GetTrackMatrix';
{
 *  SetTrackMatrix()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure SetTrackMatrix(theTrack: Track; const (*var*) matrix: MatrixRecord); external name '_SetTrackMatrix';
{
 *  GetTrackDimensions()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure GetTrackDimensions(theTrack: Track; var width: Fixed; var height: Fixed); external name '_GetTrackDimensions';
{
 *  SetTrackDimensions()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure SetTrackDimensions(theTrack: Track; width: Fixed; height: Fixed); external name '_SetTrackDimensions';
{
 *  GetTrackUserData()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetTrackUserData(theTrack: Track): UserData; external name '_GetTrackUserData';
{
 *  GetTrackDisplayMatrix()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetTrackDisplayMatrix(theTrack: Track; var matrix: MatrixRecord): OSErr; external name '_GetTrackDisplayMatrix';
{
 *  GetTrackSoundLocalizationSettings()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetTrackSoundLocalizationSettings(theTrack: Track; var settings: Handle): OSErr; external name '_GetTrackSoundLocalizationSettings';
{
 *  SetTrackSoundLocalizationSettings()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SetTrackSoundLocalizationSettings(theTrack: Track; settings: Handle): OSErr; external name '_SetTrackSoundLocalizationSettings';
{************************
* get Media routines
*************************}
{
 *  NewTrackMedia()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function NewTrackMedia(theTrack: Track; mediaType: OSType; timeScale_: TimeScale; dataRef: Handle; dataRefType: OSType): Media; external name '_NewTrackMedia';
{
 *  DisposeTrackMedia()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure DisposeTrackMedia(theMedia: Media); external name '_DisposeTrackMedia';
{
 *  GetTrackMedia()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetTrackMedia(theTrack: Track): Media; external name '_GetTrackMedia';
{
 *  GetMediaTrack()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetMediaTrack(theMedia: Media): Track; external name '_GetMediaTrack';
{************************
* Media State routines
*************************}
{
 *  GetMediaCreationTime()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetMediaCreationTime(theMedia: Media): UInt32; external name '_GetMediaCreationTime';
{
 *  GetMediaModificationTime()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetMediaModificationTime(theMedia: Media): UInt32; external name '_GetMediaModificationTime';
{
 *  GetMediaTimeScale()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetMediaTimeScale(theMedia: Media): TimeScale; external name '_GetMediaTimeScale';
{
 *  SetMediaTimeScale()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure SetMediaTimeScale(theMedia: Media; timeScale_: TimeScale); external name '_SetMediaTimeScale';
{
 *  GetMediaDuration()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetMediaDuration(theMedia: Media): TimeValue; external name '_GetMediaDuration';
{
 *  GetMediaLanguage()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetMediaLanguage(theMedia: Media): SInt16; external name '_GetMediaLanguage';
{
 *  SetMediaLanguage()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure SetMediaLanguage(theMedia: Media; language: SInt16); external name '_SetMediaLanguage';
{
 *  GetMediaQuality()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetMediaQuality(theMedia: Media): SInt16; external name '_GetMediaQuality';
{
 *  SetMediaQuality()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure SetMediaQuality(theMedia: Media; quality: SInt16); external name '_SetMediaQuality';
{
 *  GetMediaHandlerDescription()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure GetMediaHandlerDescription(theMedia: Media; var mediaType: OSType; var creatorName: Str255; var creatorManufacturer: OSType); external name '_GetMediaHandlerDescription';
{
 *  GetMediaUserData()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetMediaUserData(theMedia: Media): UserData; external name '_GetMediaUserData';
{
 *  GetMediaInputMap()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetMediaInputMap(theMedia: Media; var inputMap: QTAtomContainer): OSErr; external name '_GetMediaInputMap';
{
 *  SetMediaInputMap()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SetMediaInputMap(theMedia: Media; inputMap: QTAtomContainer): OSErr; external name '_SetMediaInputMap';
{************************
* Media Handler routines
*************************}
{
 *  GetMediaHandler()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetMediaHandler(theMedia: Media): MediaHandler; external name '_GetMediaHandler';
{
 *  SetMediaHandler()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SetMediaHandler(theMedia: Media; mH: MediaHandlerComponent): OSErr; external name '_SetMediaHandler';
{************************
* Media's Data routines
*************************}
{
 *  BeginMediaEdits()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function BeginMediaEdits(theMedia: Media): OSErr; external name '_BeginMediaEdits';
{
 *  EndMediaEdits()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function EndMediaEdits(theMedia: Media): OSErr; external name '_EndMediaEdits';
{
 *  SetMediaDefaultDataRefIndex()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SetMediaDefaultDataRefIndex(theMedia: Media; index: SInt16): OSErr; external name '_SetMediaDefaultDataRefIndex';
{
 *  GetMediaDataHandlerDescription()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure GetMediaDataHandlerDescription(theMedia: Media; index: SInt16; var dhType: OSType; var creatorName: Str255; var creatorManufacturer: OSType); external name '_GetMediaDataHandlerDescription';
{
 *  GetMediaDataHandler()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetMediaDataHandler(theMedia: Media; index: SInt16): DataHandler; external name '_GetMediaDataHandler';
{
 *  SetMediaDataHandler()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SetMediaDataHandler(theMedia: Media; index: SInt16; dataHandler: DataHandlerComponent): OSErr; external name '_SetMediaDataHandler';
{
 *  GetDataHandler()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetDataHandler(dataRef: Handle; dataHandlerSubType: OSType; flags: SInt32): Component; external name '_GetDataHandler';
{
 *  OpenADataHandler()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.1 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.1 and later
 }
function OpenADataHandler(dataRef: Handle; dataHandlerSubType: OSType; anchorDataRef: Handle; anchorDataRefType: OSType; tb: TimeBase; flags: SInt32; var dh: ComponentInstance): OSErr; external name '_OpenADataHandler';
{************************
* Media Sample Table Routines
*************************}
{
 *  GetMediaSampleDescriptionCount()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetMediaSampleDescriptionCount(theMedia: Media): SInt32; external name '_GetMediaSampleDescriptionCount';
{
 *  GetMediaSampleDescription()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure GetMediaSampleDescription(theMedia: Media; index: SInt32; descH: SampleDescriptionHandle); external name '_GetMediaSampleDescription';
{
 *  SetMediaSampleDescription()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SetMediaSampleDescription(theMedia: Media; index: SInt32; descH: SampleDescriptionHandle): OSErr; external name '_SetMediaSampleDescription';
{
 *  GetMediaSampleCount()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetMediaSampleCount(theMedia: Media): SInt32; external name '_GetMediaSampleCount';
{
 *  GetMediaSyncSampleCount()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetMediaSyncSampleCount(theMedia: Media): SInt32; external name '_GetMediaSyncSampleCount';
{
 *  SampleNumToMediaTime()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure SampleNumToMediaTime(theMedia: Media; logicalSampleNum: SInt32; var sampleTime: TimeValue; var sampleDuration: TimeValue); external name '_SampleNumToMediaTime';
{
 *  MediaTimeToSampleNum()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure MediaTimeToSampleNum(theMedia: Media; time: TimeValue; var sampleNum: SInt32; var sampleTime: TimeValue; var sampleDuration: TimeValue); external name '_MediaTimeToSampleNum';
{
 *  AddMediaSample()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function AddMediaSample(theMedia: Media; dataIn: Handle; inOffset: SInt32; size: UInt32; durationPerSample: TimeValue; sampleDescriptionH: SampleDescriptionHandle; numberOfSamples: SInt32; sampleFlags: SInt16; var sampleTime: TimeValue): OSErr; external name '_AddMediaSample';
{
 *  AddMediaSampleReference()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function AddMediaSampleReference(theMedia: Media; dataOffset: SInt32; size: UInt32; durationPerSample: TimeValue; sampleDescriptionH: SampleDescriptionHandle; numberOfSamples: SInt32; sampleFlags: SInt16; var sampleTime: TimeValue): OSErr; external name '_AddMediaSampleReference';
{
 *  AddMediaSampleReferences()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function AddMediaSampleReferences(theMedia: Media; sampleDescriptionH: SampleDescriptionHandle; numberOfSamples: SInt32; sampleRefs: SampleReferencePtr; var sampleTime: TimeValue): OSErr; external name '_AddMediaSampleReferences';
{
 *  AddMediaSampleReferences64()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function AddMediaSampleReferences64(theMedia: Media; sampleDescriptionH: SampleDescriptionHandle; numberOfSamples: SInt32; sampleRefs: SampleReference64Ptr; var sampleTime: TimeValue): OSErr; external name '_AddMediaSampleReferences64';
{
 *  GetMediaSample()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetMediaSample(theMedia: Media; dataOut: Handle; maxSizeToGrow: SInt32; var size: SInt32; time: TimeValue; var sampleTime: TimeValue; var durationPerSample: TimeValue; sampleDescriptionH: SampleDescriptionHandle; var sampleDescriptionIndex: SInt32; maxNumberOfSamples: SInt32; var numberOfSamples: SInt32; var sampleFlags: SInt16): OSErr; external name '_GetMediaSample';
{
 *  GetMediaSampleReference()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetMediaSampleReference(theMedia: Media; var dataOffset: SInt32; var size: SInt32; time: TimeValue; var sampleTime: TimeValue; var durationPerSample: TimeValue; sampleDescriptionH: SampleDescriptionHandle; var sampleDescriptionIndex: SInt32; maxNumberOfSamples: SInt32; var numberOfSamples: SInt32; var sampleFlags: SInt16): OSErr; external name '_GetMediaSampleReference';
{
 *  GetMediaSampleReferences()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetMediaSampleReferences(theMedia: Media; time: TimeValue; var sampleTime: TimeValue; sampleDescriptionH: SampleDescriptionHandle; var sampleDescriptionIndex: SInt32; maxNumberOfEntries: SInt32; var actualNumberofEntries: SInt32; sampleRefs: SampleReferencePtr): OSErr; external name '_GetMediaSampleReferences';
{
 *  GetMediaSampleReferences64()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function GetMediaSampleReferences64(theMedia: Media; time: TimeValue; var sampleTime: TimeValue; sampleDescriptionH: SampleDescriptionHandle; var sampleDescriptionIndex: SInt32; maxNumberOfEntries: SInt32; var actualNumberofEntries: SInt32; sampleRefs: SampleReference64Ptr): OSErr; external name '_GetMediaSampleReferences64';
{
 *  SetMediaPreferredChunkSize()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SetMediaPreferredChunkSize(theMedia: Media; maxChunkSize: SInt32): OSErr; external name '_SetMediaPreferredChunkSize';
{
 *  GetMediaPreferredChunkSize()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetMediaPreferredChunkSize(theMedia: Media; var maxChunkSize: SInt32): OSErr; external name '_GetMediaPreferredChunkSize';
{
 *  SetMediaShadowSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SetMediaShadowSync(theMedia: Media; frameDiffSampleNum: SInt32; syncSampleNum: SInt32): OSErr; external name '_SetMediaShadowSync';
{
 *  GetMediaShadowSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetMediaShadowSync(theMedia: Media; frameDiffSampleNum: SInt32; var syncSampleNum: SInt32): OSErr; external name '_GetMediaShadowSync';
{************************
* Editing Routines
*************************}
{
 *  InsertMediaIntoTrack()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function InsertMediaIntoTrack(theTrack: Track; trackStart: TimeValue; mediaTime: TimeValue; mediaDuration: TimeValue; mediaRate: Fixed): OSErr; external name '_InsertMediaIntoTrack';
{
 *  InsertTrackSegment()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function InsertTrackSegment(srcTrack: Track; dstTrack: Track; srcIn: TimeValue; srcDuration: TimeValue; dstIn: TimeValue): OSErr; external name '_InsertTrackSegment';
{
 *  InsertMovieSegment()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function InsertMovieSegment(srcMovie: Movie; dstMovie: Movie; srcIn: TimeValue; srcDuration: TimeValue; dstIn: TimeValue): OSErr; external name '_InsertMovieSegment';
{
 *  InsertEmptyTrackSegment()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function InsertEmptyTrackSegment(dstTrack: Track; dstIn: TimeValue; dstDuration: TimeValue): OSErr; external name '_InsertEmptyTrackSegment';
{
 *  InsertEmptyMovieSegment()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function InsertEmptyMovieSegment(dstMovie: Movie; dstIn: TimeValue; dstDuration: TimeValue): OSErr; external name '_InsertEmptyMovieSegment';
{
 *  DeleteTrackSegment()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function DeleteTrackSegment(theTrack: Track; startTime: TimeValue; duration: TimeValue): OSErr; external name '_DeleteTrackSegment';
{
 *  DeleteMovieSegment()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function DeleteMovieSegment(theMovie: Movie; startTime: TimeValue; duration: TimeValue): OSErr; external name '_DeleteMovieSegment';
{
 *  ScaleTrackSegment()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ScaleTrackSegment(theTrack: Track; startTime: TimeValue; oldDuration: TimeValue; newDuration: TimeValue): OSErr; external name '_ScaleTrackSegment';
{
 *  ScaleMovieSegment()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ScaleMovieSegment(theMovie: Movie; startTime: TimeValue; oldDuration: TimeValue; newDuration: TimeValue): OSErr; external name '_ScaleMovieSegment';
{************************
* Hi-level Editing Routines
*************************}
{
 *  CutMovieSelection()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function CutMovieSelection(theMovie: Movie): Movie; external name '_CutMovieSelection';
{
 *  CopyMovieSelection()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function CopyMovieSelection(theMovie: Movie): Movie; external name '_CopyMovieSelection';
{
 *  PasteMovieSelection()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure PasteMovieSelection(theMovie: Movie; src: Movie); external name '_PasteMovieSelection';
{
 *  AddMovieSelection()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure AddMovieSelection(theMovie: Movie; src: Movie); external name '_AddMovieSelection';
{
 *  ClearMovieSelection()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure ClearMovieSelection(theMovie: Movie); external name '_ClearMovieSelection';
{
 *  PasteHandleIntoMovie()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function PasteHandleIntoMovie(h: Handle; handleType: OSType; theMovie: Movie; flags: SInt32; userComp: ComponentInstance): OSErr; external name '_PasteHandleIntoMovie';
{
 *  PutMovieIntoTypedHandle()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function PutMovieIntoTypedHandle(theMovie: Movie; targetTrack: Track; handleType: OSType; publicMovie: Handle; start: TimeValue; dur: TimeValue; flags: SInt32; userComp: ComponentInstance): OSErr; external name '_PutMovieIntoTypedHandle';
{
 *  IsScrapMovie()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function IsScrapMovie(targetTrack: Track): Component; external name '_IsScrapMovie';
{************************
* Middle-level Editing Routines
*************************}
{
 *  CopyTrackSettings()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function CopyTrackSettings(srcTrack: Track; dstTrack: Track): OSErr; external name '_CopyTrackSettings';
{
 *  CopyMovieSettings()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function CopyMovieSettings(srcMovie: Movie; dstMovie: Movie): OSErr; external name '_CopyMovieSettings';
{
 *  AddEmptyTrackToMovie()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function AddEmptyTrackToMovie(srcTrack: Track; dstMovie: Movie; dataRef: Handle; dataRefType: OSType; var dstTrack: Track): OSErr; external name '_AddEmptyTrackToMovie';
const
	kQTCloneShareSamples		= $01;
	kQTCloneDontCopyEdits		= $02;

	{
	 *  AddClonedTrackToMovie()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in QuickTimeLib 5.0 and later
	 *    CarbonLib:        in CarbonLib 1.3 and later
	 *    Mac OS X:         in version 10.0 and later
	 *    Windows:          in qtmlClient.lib 5.0 and later
	 	}
function AddClonedTrackToMovie(srcTrack: Track; dstMovie: Movie; flags: SInt32; var dstTrack: Track): OSErr; external name '_AddClonedTrackToMovie';
{************************
* movie & track edit state routines
*************************}
{
 *  NewMovieEditState()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function NewMovieEditState(theMovie: Movie): MovieEditState; external name '_NewMovieEditState';
{
 *  UseMovieEditState()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function UseMovieEditState(theMovie: Movie; toState: MovieEditState): OSErr; external name '_UseMovieEditState';
{
 *  DisposeMovieEditState()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function DisposeMovieEditState(state: MovieEditState): OSErr; external name '_DisposeMovieEditState';
{
 *  NewTrackEditState()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function NewTrackEditState(theTrack: Track): TrackEditState; external name '_NewTrackEditState';
{
 *  UseTrackEditState()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function UseTrackEditState(theTrack: Track; state: TrackEditState): OSErr; external name '_UseTrackEditState';
{
 *  DisposeTrackEditState()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function DisposeTrackEditState(state: TrackEditState): OSErr; external name '_DisposeTrackEditState';
{************************
* track reference routines
*************************}
{
 *  AddTrackReference()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function AddTrackReference(theTrack: Track; refTrack: Track; refType: OSType; var addedIndex: SInt32): OSErr; external name '_AddTrackReference';
{
 *  DeleteTrackReference()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function DeleteTrackReference(theTrack: Track; refType: OSType; index: SInt32): OSErr; external name '_DeleteTrackReference';
{
 *  SetTrackReference()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SetTrackReference(theTrack: Track; refTrack: Track; refType: OSType; index: SInt32): OSErr; external name '_SetTrackReference';
{
 *  GetTrackReference()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetTrackReference(theTrack: Track; refType: OSType; index: SInt32): Track; external name '_GetTrackReference';
{
 *  GetNextTrackReferenceType()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetNextTrackReferenceType(theTrack: Track; refType: OSType): SInt32; external name '_GetNextTrackReferenceType';
{
 *  GetTrackReferenceCount()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetTrackReferenceCount(theTrack: Track; refType: OSType): SInt32; external name '_GetTrackReferenceCount';
{************************
* high level file conversion routines
*************************}
{
 *  ConvertFileToMovieFile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ConvertFileToMovieFile(const (*var*) inputFile: FSSpec; const (*var*) outputFile: FSSpec; creator: OSType; scriptTag: ScriptCode; var resID: SInt16; flags: SInt32; userComp: ComponentInstance; proc: MovieProgressUPP; refCon: SInt32): OSErr; external name '_ConvertFileToMovieFile';
{
 *  ConvertMovieToFile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ConvertMovieToFile(theMovie: Movie; onlyTrack: Track; var outputFile: FSSpec; fileType: OSType; creator: OSType; scriptTag: ScriptCode; var resID: SInt16; flags: SInt32; userComp: ComponentInstance): OSErr; external name '_ConvertMovieToFile';
const
	kGetMovieImporterValidateToFind = $00000001;
	kGetMovieImporterAllowNewFile = $00000002;
	kGetMovieImporterDontConsiderGraphicsImporters = $00000004;
	kGetMovieImporterDontConsiderFileOnlyImporters = $00000040;
	kGetMovieImporterAutoImportOnly = $00000400;				{  reject aggressive movie importers which have dontAutoFileMovieImport set }

	{
	 *  GetMovieImporterForDataRef()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 *    Windows:          in qtmlClient.lib 3.0 and later
	 	}
function GetMovieImporterForDataRef(dataRefType: OSType; dataRef: Handle; flags: SInt32; var importer: Component): OSErr; external name '_GetMovieImporterForDataRef';
const
	kQTGetMIMETypeInfoIsQuickTimeMovieType = $6D6F6F76 (* 'moov' *);			{  info is a pointer to a Boolean }
	kQTGetMIMETypeInfoIsUnhelpfulType = $64756D62 (* 'dumb' *);					{  info is a pointer to a Boolean }

	{
	 *  QTGetMIMETypeInfo()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in QuickTimeLib 5.0 and later
	 *    CarbonLib:        in CarbonLib 1.3 and later
	 *    Mac OS X:         in version 10.0 and later
	 *    Windows:          in qtmlClient.lib 5.0 and later
	 	}
function QTGetMIMETypeInfo(mimeStringStart: ConstCStringPtr; mimeStringLength: SInt16; infoSelector: OSType; infoDataPtr: UnivPtr; var infoDataSize: SInt32): OSErr; external name '_QTGetMIMETypeInfo';
{************************
* Movie Timebase Conversion Routines
*************************}
{
 *  TrackTimeToMediaTime()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function TrackTimeToMediaTime(value: TimeValue; theTrack: Track): TimeValue; external name '_TrackTimeToMediaTime';
{
 *  GetTrackEditRate()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetTrackEditRate(theTrack: Track; atTime: TimeValue): Fixed; external name '_GetTrackEditRate';
{************************
* Miscellaneous Routines
*************************}

{
 *  GetMovieDataSize()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetMovieDataSize(theMovie: Movie; startTime: TimeValue; duration: TimeValue): SInt32; external name '_GetMovieDataSize';
{
 *  GetMovieDataSize64()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function GetMovieDataSize64(theMovie: Movie; startTime: TimeValue; duration: TimeValue; var dataSize: wide): OSErr; external name '_GetMovieDataSize64';
{
 *  GetTrackDataSize()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetTrackDataSize(theTrack: Track; startTime: TimeValue; duration: TimeValue): SInt32; external name '_GetTrackDataSize';
{
 *  GetTrackDataSize64()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function GetTrackDataSize64(theTrack: Track; startTime: TimeValue; duration: TimeValue; var dataSize: wide): OSErr; external name '_GetTrackDataSize64';
{
 *  GetMediaDataSize()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetMediaDataSize(theMedia: Media; startTime: TimeValue; duration: TimeValue): SInt32; external name '_GetMediaDataSize';
{
 *  GetMediaDataSize64()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function GetMediaDataSize64(theMedia: Media; startTime: TimeValue; duration: TimeValue; var dataSize: wide): OSErr; external name '_GetMediaDataSize64';
{
 *  PtInMovie()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function PtInMovie(theMovie: Movie; pt: Point): boolean; external name '_PtInMovie';
{
 *  PtInTrack()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function PtInTrack(theTrack: Track; pt: Point): boolean; external name '_PtInTrack';
{************************
* Group Selection Routines
*************************}

{
 *  SetMovieLanguage()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure SetMovieLanguage(theMovie: Movie; language: SInt32); external name '_SetMovieLanguage';
{************************
* User Data
*************************}

{
 *  GetUserData()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetUserData(theUserData: UserData; data: Handle; udType: OSType; index: SInt32): OSErr; external name '_GetUserData';
{
 *  AddUserData()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function AddUserData(theUserData: UserData; data: Handle; udType: OSType): OSErr; external name '_AddUserData';
{
 *  RemoveUserData()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function RemoveUserData(theUserData: UserData; udType: OSType; index: SInt32): OSErr; external name '_RemoveUserData';
{
 *  CountUserDataType()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function CountUserDataType(theUserData: UserData; udType: OSType): SInt16; external name '_CountUserDataType';
{
 *  GetNextUserDataType()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetNextUserDataType(theUserData: UserData; udType: OSType): SInt32; external name '_GetNextUserDataType';
{
 *  GetUserDataItem()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetUserDataItem(theUserData: UserData; data: UnivPtr; size: SInt32; udType: OSType; index: SInt32): OSErr; external name '_GetUserDataItem';
{
 *  SetUserDataItem()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SetUserDataItem(theUserData: UserData; data: UnivPtr; size: SInt32; udType: OSType; index: SInt32): OSErr; external name '_SetUserDataItem';
{
 *  AddUserDataText()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function AddUserDataText(theUserData: UserData; data: Handle; udType: OSType; index: SInt32; itlRegionTag: SInt16): OSErr; external name '_AddUserDataText';
{
 *  GetUserDataText()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetUserDataText(theUserData: UserData; data: Handle; udType: OSType; index: SInt32; itlRegionTag: SInt16): OSErr; external name '_GetUserDataText';
{
 *  RemoveUserDataText()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function RemoveUserDataText(theUserData: UserData; udType: OSType; index: SInt32; itlRegionTag: SInt16): OSErr; external name '_RemoveUserDataText';
{
 *  NewUserData()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function NewUserData(var theUserData: UserData): OSErr; external name '_NewUserData';
{
 *  DisposeUserData()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function DisposeUserData(theUserData: UserData): OSErr; external name '_DisposeUserData';
{
 *  NewUserDataFromHandle()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function NewUserDataFromHandle(h: Handle; var theUserData: UserData): OSErr; external name '_NewUserDataFromHandle';
{
 *  PutUserDataIntoHandle()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function PutUserDataIntoHandle(theUserData: UserData; h: Handle): OSErr; external name '_PutUserDataIntoHandle';
const
	kQTCopyUserDataReplace		= $72706C63 (* 'rplc' *);						{  Delete all destination user data items and then add source user data items  }
	kQTCopyUserDataMerge		= $6D657267 (* 'merg' *);						{  Add source user data items to destination user data  }

	{
	 *  CopyMovieUserData()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in QuickTimeLib 6.0 and later
	 *    CarbonLib:        in CarbonLib 1.6 and later
	 *    Mac OS X:         in version 10.2 and later
	 *    Windows:          in qtmlClient.lib 6.0 and later
	 	}
function CopyMovieUserData(srcMovie: Movie; dstMovie: Movie; copyRule: OSType): OSErr; external name '_CopyMovieUserData';
{
 *  CopyTrackUserData()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 6.0 and later
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.2 and later
 *    Windows:          in qtmlClient.lib 6.0 and later
 }
function CopyTrackUserData(srcTrack: Track; dstTrack: Track; copyRule: OSType): OSErr; external name '_CopyTrackUserData';
{
 *  CopyMediaUserData()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 6.0 and later
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.2 and later
 *    Windows:          in qtmlClient.lib 6.0 and later
 }
function CopyMediaUserData(srcMedia: Media; dstMedia: Media; copyRule: OSType): OSErr; external name '_CopyMediaUserData';
{
 *  CopyUserData()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 6.0 and later
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.2 and later
 *    Windows:          in qtmlClient.lib 6.0 and later
 }
function CopyUserData(srcUserData: UserData; dstUserData: UserData; copyRule: OSType): OSErr; external name '_CopyUserData';
{
 *  SetMoviePropertyAtom()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.1 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.1 and later
 }
function SetMoviePropertyAtom(theMovie: Movie; propertyAtom: QTAtomContainer): OSErr; external name '_SetMoviePropertyAtom';
{
 *  GetMoviePropertyAtom()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.1 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.1 and later
 }
function GetMoviePropertyAtom(theMovie: Movie; var propertyAtom: QTAtomContainer): OSErr; external name '_GetMoviePropertyAtom';
{
 *  GetMediaNextInterestingTime()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure GetMediaNextInterestingTime(theMedia: Media; interestingTimeFlags: SInt16; time: TimeValue; rate: Fixed; var interestingTime: TimeValue; var interestingDuration: TimeValue); external name '_GetMediaNextInterestingTime';
{
 *  GetTrackNextInterestingTime()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure GetTrackNextInterestingTime(theTrack: Track; interestingTimeFlags: SInt16; time: TimeValue; rate: Fixed; var interestingTime: TimeValue; var interestingDuration: TimeValue); external name '_GetTrackNextInterestingTime';
{
 *  GetMovieNextInterestingTime()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure GetMovieNextInterestingTime(theMovie: Movie; interestingTimeFlags: SInt16; numMediaTypes: SInt16; whichMediaTypes: OSTypePtr; time: TimeValue; rate: Fixed; var interestingTime: TimeValue; var interestingDuration: TimeValue); external name '_GetMovieNextInterestingTime';
{
 *  CreateMovieFile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function CreateMovieFile(const (*var*) fileSpec: FSSpec; creator: OSType; scriptTag: ScriptCode; createMovieFileFlags: UInt32; var resRefNum: SInt16; var newmovie: Movie): OSErr; external name '_CreateMovieFile';
{
 *  OpenMovieFile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function OpenMovieFile(const (*var*) fileSpec: FSSpec; var resRefNum: SInt16; permission: SInt8): OSErr; external name '_OpenMovieFile';
{
 *  CloseMovieFile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function CloseMovieFile(resRefNum: SInt16): OSErr; external name '_CloseMovieFile';
{
 *  DeleteMovieFile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function DeleteMovieFile(const (*var*) fileSpec: FSSpec): OSErr; external name '_DeleteMovieFile';
{
 *  NewMovieFromFile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function NewMovieFromFile(var theMovie: Movie; resRefNum: SInt16; resId: SInt16Ptr; resName: StringPtr; newMovieFlags: SInt16; dataRefWasChanged: BooleanPtr): OSErr; external name '_NewMovieFromFile';
{
 *  NewMovieFromHandle()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function NewMovieFromHandle(var theMovie: Movie; h: Handle; newMovieFlags: SInt16; var dataRefWasChanged: boolean): OSErr; external name '_NewMovieFromHandle';
{
 *  NewMovieFromDataFork()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function NewMovieFromDataFork(var theMovie: Movie; fRefNum: SInt16; fileOffset: SInt32; newMovieFlags: SInt16; var dataRefWasChanged: boolean): OSErr; external name '_NewMovieFromDataFork';
{
 *  NewMovieFromDataFork64()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function NewMovieFromDataFork64(var theMovie: Movie; fRefNum: SInt32; const (*var*) fileOffset: wide; newMovieFlags: SInt16; var dataRefWasChanged: boolean): OSErr; external name '_NewMovieFromDataFork64';
{
 *  NewMovieFromUserProc()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function NewMovieFromUserProc(var m: Movie; flags: SInt16; var dataRefWasChanged: boolean; getProc: GetMovieUPP; refCon: UnivPtr; defaultDataRef: Handle; dataRefType: OSType): OSErr; external name '_NewMovieFromUserProc';
{
 *  NewMovieFromDataRef()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function NewMovieFromDataRef(var m: Movie; flags: SInt16; var id: SInt16; dataRef: Handle; dtaRefType: OSType): OSErr; external name '_NewMovieFromDataRef';
{
 *  NewMovieFromStorageOffset()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 6.0 and later
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.2 and later
 *    Windows:          in qtmlClient.lib 6.0 and later
 }
function NewMovieFromStorageOffset(var theMovie: Movie; dh: DataHandler; const (*var*) fileOffset: wide; newMovieFlags: SInt16; var dataRefWasCataRefType: boolean): OSErr; external name '_NewMovieFromStorageOffset';
{
 *  NewMovieForDataRefFromHandle()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 6.0 and later
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.2 and later
 *    Windows:          in qtmlClient.lib 6.0 and later
 }
function NewMovieForDataRefFromHandle(var theMovie: Movie; h: Handle; newMovieFlags: SInt16; var dataRefWasChanged: boolean; dataRef: Handle; dataRefType: OSType): OSErr; external name '_NewMovieForDataRefFromHandle';
{
 *  AddMovieResource()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function AddMovieResource(theMovie: Movie; resRefNum: SInt16; var resId: SInt16; const (*var*) resName: Str255): OSErr; external name '_AddMovieResource';
{
 *  UpdateMovieResource()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function UpdateMovieResource(theMovie: Movie; resRefNum: SInt16; resId: SInt16; const (*var*) resName: Str255): OSErr; external name '_UpdateMovieResource';
{
 *  RemoveMovieResource()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function RemoveMovieResource(resRefNum: SInt16; resId: SInt16): OSErr; external name '_RemoveMovieResource';
{
 *  CreateMovieStorage()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 6.0 and later
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.2 and later
 *    Windows:          in qtmlClient.lib 6.0 and later
 }
function CreateMovieStorage(dataRef: Handle; dataRefType: OSType; creator: OSType; scriptTag: ScriptCode; createMovieFileFlags: SInt32; var outDataHandler: DataHandler; var newmovie: Movie): OSErr; external name '_CreateMovieStorage';
{
 *  OpenMovieStorage()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 6.0 and later
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.2 and later
 *    Windows:          in qtmlClient.lib 6.0 and later
 }
function OpenMovieStorage(dataRef: Handle; dataRefType: OSType; flags: SInt32; var outDataHandler: DataHandler): OSErr; external name '_OpenMovieStorage';
{
 *  CloseMovieStorage()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 6.0 and later
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.2 and later
 *    Windows:          in qtmlClient.lib 6.0 and later
 }
function CloseMovieStorage(dh: DataHandler): OSErr; external name '_CloseMovieStorage';
{
 *  DeleteMovieStorage()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 6.0 and later
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.2 and later
 *    Windows:          in qtmlClient.lib 6.0 and later
 }
function DeleteMovieStorage(dataRef: Handle; dataRefType: OSType): OSErr; external name '_DeleteMovieStorage';
{
 *  AddMovieToStorage()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 6.0 and later
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.2 and later
 *    Windows:          in qtmlClient.lib 6.0 and later
 }
function AddMovieToStorage(theMovie: Movie; dh: DataHandler): OSErr; external name '_AddMovieToStorage';
{
 *  UpdateMovieInStorage()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 6.0 and later
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.2 and later
 *    Windows:          in qtmlClient.lib 6.0 and later
 }
function UpdateMovieInStorage(theMovie: Movie; dh: DataHandler): OSErr; external name '_UpdateMovieInStorage';
{
 *  HasMovieChanged()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function HasMovieChanged(theMovie: Movie): boolean; external name '_HasMovieChanged';
{
 *  ClearMovieChanged()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure ClearMovieChanged(theMovie: Movie); external name '_ClearMovieChanged';
{
 *  SetMovieDefaultDataRef()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SetMovieDefaultDataRef(theMovie: Movie; dataRef: Handle; dataRefType: OSType): OSErr; external name '_SetMovieDefaultDataRef';
{
 *  GetMovieDefaultDataRef()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetMovieDefaultDataRef(theMovie: Movie; var dataRef: Handle; var dataRefType: OSType): OSErr; external name '_GetMovieDefaultDataRef';
{
 *  SetMovieAnchorDataRef()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.1 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.1 and later
 }
function SetMovieAnchorDataRef(theMovie: Movie; dataRef: Handle; dataRefType: OSType): OSErr; external name '_SetMovieAnchorDataRef';
{
 *  GetMovieAnchorDataRef()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.1 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.1 and later
 }
function GetMovieAnchorDataRef(theMovie: Movie; var dataRef: Handle; var dataRefType: OSType; var outFlags: SInt32): OSErr; external name '_GetMovieAnchorDataRef';
{
 *  SetMovieColorTable()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SetMovieColorTable(theMovie: Movie; ctab: CTabHandle): OSErr; external name '_SetMovieColorTable';
{
 *  GetMovieColorTable()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetMovieColorTable(theMovie: Movie; var ctab: CTabHandle): OSErr; external name '_GetMovieColorTable';
{
 *  FlattenMovie()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure FlattenMovie(theMovie: Movie; movieFlattenFlags: SInt32; const (*var*) theFile: FSSpec; creator: OSType; scriptTag: ScriptCode; createMovieFileFlags: SInt32; var resId: SInt16; const (*var*) resName: Str255); external name '_FlattenMovie';
{
 *  FlattenMovieData()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function FlattenMovieData(theMovie: Movie; movieFlattenFlags: SInt32; const (*var*) theFile: FSSpec; creator: OSType; scriptTag: ScriptCode; createMovieFileFlags: SInt32): Movie; external name '_FlattenMovieData';
{
 *  FlattenMovieDataToDataRef()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 6.0 and later
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.2 and later
 *    Windows:          in qtmlClient.lib 6.0 and later
 }
function FlattenMovieDataToDataRef(theMovie: Movie; movieFlattenFlags: SInt32; dataRef: Handle; dataRefType: OSType; creator: OSType; scriptTag: ScriptCode; createMovieFileFlags: SInt32): Movie; external name '_FlattenMovieDataToDataRef';
{
 *  SetMovieProgressProc()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure SetMovieProgressProc(theMovie: Movie; p: MovieProgressUPP; refcon: SInt32); external name '_SetMovieProgressProc';
{
 *  GetMovieProgressProc()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
procedure GetMovieProgressProc(theMovie: Movie; var p: MovieProgressUPP; var refcon: SInt32); external name '_GetMovieProgressProc';
{
 *  CreateShortcutMovieFile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function CreateShortcutMovieFile(const (*var*) fileSpec: FSSpec; creator: OSType; scriptTag: ScriptCode; createMovieFileFlags: SInt32; targetDataRef: Handle; targetDataRefType: OSType): OSErr; external name '_CreateShortcutMovieFile';
{
 *  MovieSearchText()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MovieSearchText(theMovie: Movie; text: Ptr; size: SInt32; searchFlags: SInt32; var searchTrack: Track; var searchTime: TimeValue; var searchOffset: SInt32): OSErr; external name '_MovieSearchText';
{
 *  GetPosterBox()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure GetPosterBox(theMovie: Movie; var boxRect: Rect); external name '_GetPosterBox';
{
 *  SetPosterBox()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure SetPosterBox(theMovie: Movie; const (*var*) boxRect: Rect); external name '_SetPosterBox';
{
 *  GetMovieSegmentDisplayBoundsRgn()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetMovieSegmentDisplayBoundsRgn(theMovie: Movie; time: TimeValue; duration: TimeValue): RgnHandle; external name '_GetMovieSegmentDisplayBoundsRgn';
{
 *  GetTrackSegmentDisplayBoundsRgn()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetTrackSegmentDisplayBoundsRgn(theTrack: Track; time: TimeValue; duration: TimeValue): RgnHandle; external name '_GetTrackSegmentDisplayBoundsRgn';
{
 *  SetMovieCoverProcs()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure SetMovieCoverProcs(theMovie: Movie; uncoverProc: MovieRgnCoverUPP; coverProc: MovieRgnCoverUPP; refcon: SInt32); external name '_SetMovieCoverProcs';
{
 *  GetMovieCoverProcs()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetMovieCoverProcs(theMovie: Movie; var uncoverProc: MovieRgnCoverUPP; var coverProc: MovieRgnCoverUPP; var refcon: SInt32): OSErr; external name '_GetMovieCoverProcs';
{
 *  GetTrackStatus()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetTrackStatus(theTrack: Track): ComponentResult; external name '_GetTrackStatus';
{
 *  GetMovieStatus()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetMovieStatus(theMovie: Movie; var firstProblemTrack: Track): ComponentResult; external name '_GetMovieStatus';
const
	kMovieLoadStateError		= -1;
	kMovieLoadStateLoading		= 1000;
	kMovieLoadStateLoaded		= 2000;
	kMovieLoadStatePlayable		= 10000;
	kMovieLoadStatePlaythroughOK = 20000;
	kMovieLoadStateComplete		= 100000;

	{
	 *  GetMovieLoadState()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in QuickTimeLib 4.1 and later
	 *    CarbonLib:        in CarbonLib 1.1 and later
	 *    Mac OS X:         in version 10.0 and later
	 *    Windows:          in qtmlClient.lib 4.1 and later
	 	}
function GetMovieLoadState(theMovie: Movie): SInt32; external name '_GetMovieLoadState';
{ Input flags for CanQuickTimeOpenFile/DataRef }

const
	kQTDontUseDataToFindImporter = $00000001;
	kQTDontLookForMovieImporterIfGraphicsImporterFound = $00000002;
	kQTAllowOpeningStillImagesAsMovies = $00000004;
	kQTAllowImportersThatWouldCreateNewFile = $00000008;
	kQTAllowAggressiveImporters	= $00000010;					{  eg, TEXT and PICT movie importers }

	{	 Determines whether the file could be opened using a graphics importer or opened in place as a movie. 	}
	{
	 *  CanQuickTimeOpenFile()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in QuickTimeLib 5.0 and later
	 *    CarbonLib:        in CarbonLib 1.3 and later
	 *    Mac OS X:         in version 10.0 and later
	 *    Windows:          in qtmlClient.lib 5.0 and later
	 	}
function CanQuickTimeOpenFile(fileSpec: FSSpecPtr; fileType: OSType; fileNameExtension: OSType; var outCanOpenWithGraphicsImporter: boolean; var outCanOpenAsMovie: boolean; var outPreferGraphicsImporter: boolean; inFlags: UInt32): OSErr; external name '_CanQuickTimeOpenFile';
{ Determines whether the file could be opened using a graphics importer or opened in place as a movie. }
{
 *  CanQuickTimeOpenDataRef()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 5.0 and later
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 5.0 and later
 }
function CanQuickTimeOpenDataRef(dataRef: Handle; dataRefType: OSType; var outCanOpenWithGraphicsImporter: boolean; var outCanOpenAsMovie: boolean; var outPreferGraphicsImporter: boolean; inFlags: UInt32): OSErr; external name '_CanQuickTimeOpenDataRef';
{***
    Movie Controller support routines
***}
{
 *  NewMovieController()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function NewMovieController(theMovie: Movie; const (*var*) movieRect: Rect; someFlags: SInt32): ComponentInstance; external name '_NewMovieController';
{
 *  DisposeMovieController()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure DisposeMovieController(mc: ComponentInstance); external name '_DisposeMovieController';
{
 *  ShowMovieInformation()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure ShowMovieInformation(theMovie: Movie; filterProc: ModalFilterUPP; refCon: SInt32); external name '_ShowMovieInformation';
{****
    Scrap routines
****}
{
 *  PutMovieOnScrap()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function PutMovieOnScrap(theMovie: Movie; movieScrapFlags: SInt32): OSErr; external name '_PutMovieOnScrap';
{
 *  NewMovieFromScrap()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function NewMovieFromScrap(newMovieFlags: SInt32): Movie; external name '_NewMovieFromScrap';
{****
    DataRef routines
****}

{
 *  GetMediaDataRef()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetMediaDataRef(theMedia: Media; index: SInt16; var dataRef: Handle; var dataRefType: OSType; var dataRefAttributes: SInt32): OSErr; external name '_GetMediaDataRef';
{
 *  SetMediaDataRef()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SetMediaDataRef(theMedia: Media; index: SInt16; dataRef: Handle; dataRefType: OSType): OSErr; external name '_SetMediaDataRef';
{
 *  SetMediaDataRefAttributes()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SetMediaDataRefAttributes(theMedia: Media; index: SInt16; dataRefAttributes: SInt32): OSErr; external name '_SetMediaDataRefAttributes';
{
 *  AddMediaDataRef()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function AddMediaDataRef(theMedia: Media; var index: SInt16; dataRef: Handle; dataRefType: OSType): OSErr; external name '_AddMediaDataRef';
{
 *  GetMediaDataRefCount()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetMediaDataRefCount(theMedia: Media; var count: SInt16): OSErr; external name '_GetMediaDataRefCount';
{
 *  QTNewAlias()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function QTNewAlias(const (*var*) fss: FSSpec; var alias: AliasHandle; minimal: boolean): OSErr; external name '_QTNewAlias';
{****
    Playback hint routines
****}
{
 *  SetMoviePlayHints()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure SetMoviePlayHints(theMovie: Movie; flags: SInt32; flagsMask: SInt32); external name '_SetMoviePlayHints';
{
 *  SetMediaPlayHints()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure SetMediaPlayHints(theMedia: Media; flags: SInt32; flagsMask: SInt32); external name '_SetMediaPlayHints';
{
 *  GetMediaPlayHints()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure GetMediaPlayHints(theMedia: Media; var flags: SInt32); external name '_GetMediaPlayHints';
{****
    Load time track hints
****}

const
	preloadAlways				= $00000001;
	preloadOnlyIfEnabled		= $00000002;

	{
	 *  SetTrackLoadSettings()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 *    Windows:          in qtmlClient.lib 3.0 and later
	 	}
procedure SetTrackLoadSettings(theTrack: Track; preloadTime: TimeValue; preloadDuration: TimeValue; preloadFlags: SInt32; defaultHints: SInt32); external name '_SetTrackLoadSettings';
{
 *  GetTrackLoadSettings()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure GetTrackLoadSettings(theTrack: Track; var preloadTime: TimeValue; var preloadDuration: TimeValue; var preloadFlags: SInt32; var defaultHints: SInt32); external name '_GetTrackLoadSettings';
{****
    Big screen TV
****}

const
	fullScreenHideCursor		= $00000001;
	fullScreenAllowEvents		= $00000002;
	fullScreenDontChangeMenuBar	= $00000004;
	fullScreenPreflightSize		= $00000008;

	{
	 *  BeginFullScreen()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 *    Windows:          in qtmlClient.lib 3.0 and later
	 	}
function BeginFullScreen(var restoreState: Ptr; whichGD: GDHandle; var desiredWidth: SInt16; var desiredHeight: SInt16; var newWindow: WindowRef; var eraseColor: RGBColor; flags: SInt32): OSErr; external name '_BeginFullScreen';
{
 *  EndFullScreen()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function EndFullScreen(fullState: Ptr; flags: SInt32): OSErr; external name '_EndFullScreen';
{****
    Wired Actions
****}
{  flags for MovieExecuteWiredActions }

const
	movieExecuteWiredActionDontExecute = $00000001;

	{
	 *  AddMovieExecuteWiredActionsProc()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
	 *    CarbonLib:        in CarbonLib 1.0.2 and later
	 *    Mac OS X:         in version 10.0 and later
	 *    Windows:          in qtmlClient.lib 4.0 and later
	 	}
function AddMovieExecuteWiredActionsProc(theMovie: Movie; proc: MovieExecuteWiredActionsUPP; refCon: UnivPtr): OSErr; external name '_AddMovieExecuteWiredActionsProc';
{
 *  RemoveMovieExecuteWiredActionsProc()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function RemoveMovieExecuteWiredActionsProc(theMovie: Movie; proc: MovieExecuteWiredActionsUPP; refCon: UnivPtr): OSErr; external name '_RemoveMovieExecuteWiredActionsProc';
{
 *  MovieExecuteWiredActions()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function MovieExecuteWiredActions(theMovie: Movie; flags: SInt32; actions: QTAtomContainer): OSErr; external name '_MovieExecuteWiredActions';
{****
    Keyboard Navigation/Editable Text Field Support
****}
{
   Navigation Direction Constants
        for MediaNavigateTargetRefCon
}

const
	kRefConNavigationNext		= 0;
	kRefConNavigationPrevious	= 1;

	{
	   Refcon Properties 
	        for MediaRefConGetProerty/MediaRefConSetProperty
	}
	kRefConPropertyCanHaveFocus	= 1;							{  Boolean  }
	kRefConPropertyHasFocus		= 2;							{  Boolean  }

	{  media properties }
	kTrackFocusCanEditFlag		= $6B656474 (* 'kedt' *);
	kTrackDefaultFocusFlags		= $6B666F63 (* 'kfoc' *);
	kTrackFocusDefaultRefcon	= $6B726566 (* 'kref' *);

	{  focus flags }
	kTrackFocusOn				= 1;
	kTrackHandlesTabs			= 2;							{  this is reserved for a future release }

	{	****
	    Flash track properties
	****	}
	kFlashTrackPropertyAcceptAllClicks = $636C696B (* 'clik' *);				{  type of media property atom; data is a Boolean  }

	{	****
	    Sprite Toolbox
	****	}
	kBackgroundSpriteLayerNum	= 32767;


	{   Sprite Properties }
	kSpritePropertyMatrix		= 1;
	kSpritePropertyImageDescription = 2;
	kSpritePropertyImageDataPtr	= 3;
	kSpritePropertyVisible		= 4;
	kSpritePropertyLayer		= 5;
	kSpritePropertyGraphicsMode	= 6;
	kSpritePropertyImageDataSize = 7;
	kSpritePropertyActionHandlingSpriteID = 8;
	kSpritePropertyCanBeHitTested = 9;
	kSpritePropertyImageIndex	= 100;
	kSpriteTrackPropertyBackgroundColor = 101;
	kSpriteTrackPropertyOffscreenBitDepth = 102;
	kSpriteTrackPropertySampleFormat = 103;
	kSpriteTrackPropertyScaleSpritesToScaleWorld = 104;
	kSpriteTrackPropertyHasActions = 105;
	kSpriteTrackPropertyVisible	= 106;
	kSpriteTrackPropertyQTIdleEventsFrequency = 107;
	kSpriteTrackPropertyAllSpritesHitTestingMode = 108;
	kSpriteTrackPropertyPreferredDepthInterpretationMode = 109;
	kSpriteImagePropertyRegistrationPoint = 1000;
	kSpriteImagePropertyGroupID	= 1001;

	{  values for kSpriteTrackPropertyPreferredDepthInterpretationMode }
	kSpriteTrackPreferredDepthCompatibilityMode = 0;
	kSpriteTrackPreferredDepthModernMode = 1;

	{  values for kSpriteTrackPropertyAllSpritesHitTestingMode }
	kSpriteHitTestUseSpritesOwnPropertiesMode = 0;
	kSpriteHitTestTreatAllSpritesAsHitTestableMode = 1;
	kSpriteHitTestTreatAllSpritesAsNotHitTestableMode = 2;

	{  special value for kSpriteTrackPropertyQTIdleEventsFrequency (the default) }
	kNoQTIdleEvents				= -1;

	{  GetSpriteProperties for accessing invalid SpriteWorldRegion }
	kGetSpriteWorldInvalidRegionAndLeaveIntact = -1;
	kGetSpriteWorldInvalidRegionAndThenSetEmpty = -2;

	{  flagsIn for SpriteWorldIdle }
	kOnlyDrawToSpriteWorld		= $00000001;
	kSpriteWorldPreflight		= $00000002;

	{  flagsOut for SpriteWorldIdle }
	kSpriteWorldDidDraw			= $00000001;
	kSpriteWorldNeedsToDraw		= $00000002;

	{  flags for sprite track sample format }
	kKeyFrameAndSingleOverride	= $00000002;
	kKeyFrameAndAllOverrides	= $00000004;

	{  sprite world flags }
	kScaleSpritesToScaleWorld	= $00000002;
	kSpriteWorldHighQuality		= $00000004;
	kSpriteWorldDontAutoInvalidate = $00000008;
	kSpriteWorldInvisible		= $00000010;
	kSpriteWorldDirtyInsteadOfFlush = $00000020;

	{
	 *  NewSpriteWorld()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 *    Windows:          in qtmlClient.lib 3.0 and later
	 	}
function NewSpriteWorld(var newSpriteWorld: SpriteWorld; destination: GWorldPtr; spriteLayer: GWorldPtr; var backgroundColor: RGBColor; background: GWorldPtr): OSErr; external name '_NewSpriteWorld';
{
 *  DisposeSpriteWorld()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure DisposeSpriteWorld(theSpriteWorld: SpriteWorld); external name '_DisposeSpriteWorld';
{
 *  SetSpriteWorldClip()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SetSpriteWorldClip(theSpriteWorld: SpriteWorld; clipRgn: RgnHandle): OSErr; external name '_SetSpriteWorldClip';
{
 *  SetSpriteWorldMatrix()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SetSpriteWorldMatrix(theSpriteWorld: SpriteWorld; const (*var*) matrix: MatrixRecord): OSErr; external name '_SetSpriteWorldMatrix';
{
 *  SetSpriteWorldGraphicsMode()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SetSpriteWorldGraphicsMode(theSpriteWorld: SpriteWorld; mode: SInt32; const (*var*) opColor: RGBColor): OSErr; external name '_SetSpriteWorldGraphicsMode';
{
 *  SpriteWorldIdle()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SpriteWorldIdle(theSpriteWorld: SpriteWorld; flagsIn: SInt32; var flagsOut: SInt32): OSErr; external name '_SpriteWorldIdle';
{
 *  InvalidateSpriteWorld()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function InvalidateSpriteWorld(theSpriteWorld: SpriteWorld; var invalidArea: Rect): OSErr; external name '_InvalidateSpriteWorld';
{
 *  SpriteWorldHitTest()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SpriteWorldHitTest(theSpriteWorld: SpriteWorld; flags: SInt32; loc: Point; var spriteHit: Sprite): OSErr; external name '_SpriteWorldHitTest';
{
 *  SpriteHitTest()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SpriteHitTest(theSprite: Sprite; flags: SInt32; loc: Point; var wasHit: boolean): OSErr; external name '_SpriteHitTest';
{
 *  DisposeAllSprites()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure DisposeAllSprites(theSpriteWorld: SpriteWorld); external name '_DisposeAllSprites';
{
 *  SetSpriteWorldFlags()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SetSpriteWorldFlags(spriteWorld_: SpriteWorld; flags: SInt32; flagsMask: SInt32): OSErr; external name '_SetSpriteWorldFlags';
{
 *  NewSprite()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function NewSprite(var newSprite: Sprite; itsSpriteWorld: SpriteWorld; idh: ImageDescriptionHandle; imageDataPtr: Ptr; var matrix: MatrixRecord; visible: boolean; layer: SInt16): OSErr; external name '_NewSprite';
{
 *  DisposeSprite()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure DisposeSprite(theSprite: Sprite); external name '_DisposeSprite';
{
 *  InvalidateSprite()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure InvalidateSprite(theSprite: Sprite); external name '_InvalidateSprite';
{
 *  SetSpriteProperty()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SetSpriteProperty(theSprite: Sprite; propertyType: SInt32; propertyValue: UnivPtr): OSErr; external name '_SetSpriteProperty';
{
 *  GetSpriteProperty()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetSpriteProperty(theSprite: Sprite; propertyType: SInt32; propertyValue: UnivPtr): OSErr; external name '_GetSpriteProperty';
{****
    QT Atom Data Support
****}

const
	kParentAtomIsContainer		= 0;

	{  create and dispose QTAtomContainer objects }

	{
	 *  QTNewAtomContainer()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 *    Windows:          in qtmlClient.lib 3.0 and later
	 	}
function QTNewAtomContainer(var atomData: QTAtomContainer): OSErr; external name '_QTNewAtomContainer';
{
 *  QTDisposeAtomContainer()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function QTDisposeAtomContainer(atomData: QTAtomContainer): OSErr; external name '_QTDisposeAtomContainer';
{  locating nested atoms within QTAtomContainer container }

{
 *  QTGetNextChildType()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function QTGetNextChildType(container: QTAtomContainer; parentAtom: QTAtom; currentChildType: QTAtomType): QTAtomType; external name '_QTGetNextChildType';
{
 *  QTCountChildrenOfType()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function QTCountChildrenOfType(container: QTAtomContainer; parentAtom: QTAtom; childType: QTAtomType): SInt16; external name '_QTCountChildrenOfType';
{
 *  QTFindChildByIndex()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function QTFindChildByIndex(container: QTAtomContainer; parentAtom: QTAtom; atomType: QTAtomType; index: SInt16; var id: QTAtomID): QTAtom; external name '_QTFindChildByIndex';
{
 *  QTFindChildByID()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function QTFindChildByID(container: QTAtomContainer; parentAtom: QTAtom; atomType: QTAtomType; id: QTAtomID; var index: SInt16): QTAtom; external name '_QTFindChildByID';
{
 *  QTNextChildAnyType()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function QTNextChildAnyType(container: QTAtomContainer; parentAtom: QTAtom; currentChild: QTAtom; var nextChild: QTAtom): OSErr; external name '_QTNextChildAnyType';
{  set a leaf atom's data }
{
 *  QTSetAtomData()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function QTSetAtomData(container: QTAtomContainer; atom: QTAtom; dataSize: SInt32; atomData: UnivPtr): OSErr; external name '_QTSetAtomData';
{  extracting data }
{
 *  QTCopyAtomDataToHandle()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function QTCopyAtomDataToHandle(container: QTAtomContainer; atom: QTAtom; targetHandle: Handle): OSErr; external name '_QTCopyAtomDataToHandle';
{
 *  QTCopyAtomDataToPtr()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function QTCopyAtomDataToPtr(container: QTAtomContainer; atom: QTAtom; sizeOrLessOK: boolean; size: SInt32; targetPtr: UnivPtr; var actualSize: SInt32): OSErr; external name '_QTCopyAtomDataToPtr';
{
 *  QTGetAtomTypeAndID()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function QTGetAtomTypeAndID(container: QTAtomContainer; atom: QTAtom; var atomType: QTAtomType; var id: QTAtomID): OSErr; external name '_QTGetAtomTypeAndID';
{  extract a copy of an atom and all of it's children, caller disposes }
{
 *  QTCopyAtom()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function QTCopyAtom(container: QTAtomContainer; atom: QTAtom; var targetContainer: QTAtomContainer): OSErr; external name '_QTCopyAtom';
{  obtaining direct reference to atom data }
{
 *  QTLockContainer()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function QTLockContainer(container: QTAtomContainer): OSErr; external name '_QTLockContainer';
{
 *  QTGetAtomDataPtr()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function QTGetAtomDataPtr(container: QTAtomContainer; atom: QTAtom; var dataSize: SInt32; var atomData: Ptr): OSErr; external name '_QTGetAtomDataPtr';
{
 *  QTUnlockContainer()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function QTUnlockContainer(container: QTAtomContainer): OSErr; external name '_QTUnlockContainer';
{
   building QTAtomContainer trees
   creates and inserts new atom at specified index, existing atoms at or after index are moved toward end of list
   used for Top-Down tree creation
}
{
 *  QTInsertChild()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function QTInsertChild(container: QTAtomContainer; parentAtom: QTAtom; atomType: QTAtomType; id: QTAtomID; index: SInt16; dataSize: SInt32; data: UnivPtr; var newAtom: QTAtom): OSErr; external name '_QTInsertChild';
{  inserts children from childrenContainer as children of parentAtom }
{
 *  QTInsertChildren()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function QTInsertChildren(container: QTAtomContainer; parentAtom: QTAtom; childrenContainer: QTAtomContainer): OSErr; external name '_QTInsertChildren';
{  destruction }
{
 *  QTRemoveAtom()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function QTRemoveAtom(container: QTAtomContainer; atom: QTAtom): OSErr; external name '_QTRemoveAtom';
{
 *  QTRemoveChildren()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function QTRemoveChildren(container: QTAtomContainer; atom: QTAtom): OSErr; external name '_QTRemoveChildren';
{  replacement must be same type as target }
{
 *  QTReplaceAtom()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function QTReplaceAtom(targetContainer: QTAtomContainer; targetAtom: QTAtom; replacementContainer: QTAtomContainer; replacementAtom: QTAtom): OSErr; external name '_QTReplaceAtom';
{
 *  QTSwapAtoms()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function QTSwapAtoms(container: QTAtomContainer; atom1: QTAtom; atom2: QTAtom): OSErr; external name '_QTSwapAtoms';
{
 *  QTSetAtomID()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function QTSetAtomID(container: QTAtomContainer; atom: QTAtom; newID: QTAtomID): OSErr; external name '_QTSetAtomID';
{
 *  QTGetAtomParent()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function QTGetAtomParent(container: QTAtomContainer; childAtom: QTAtom): QTAtom; external name '_QTGetAtomParent';
{
 *  SetMediaPropertyAtom()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SetMediaPropertyAtom(theMedia: Media; propertyAtom: QTAtomContainer): OSErr; external name '_SetMediaPropertyAtom';
{
 *  GetMediaPropertyAtom()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetMediaPropertyAtom(theMedia: Media; var propertyAtom: QTAtomContainer): OSErr; external name '_GetMediaPropertyAtom';
{****
    Tween Support
****}

type
	TweenRecordPtr = ^TweenRecord;
{$ifc TYPED_FUNCTION_POINTERS}
	TweenerDataProcPtr = function(tr: TweenRecordPtr; tweenData: UnivPtr; tweenDataSize: SInt32; dataDescriptionSeed: SInt32; dataDescription: Handle; asyncCompletionProc: ICMCompletionProcRecordPtr; transferProc: UniversalProcPtr; refCon: UnivPtr): ComponentResult;
{$elsec}
	TweenerDataProcPtr = ProcPtr;
{$endc}

{$ifc OPAQUE_UPP_TYPES}
	TweenerDataUPP = ^SInt32; { an opaque UPP }
{$elsec}
	TweenerDataUPP = UniversalProcPtr;
{$endc}	
	TweenRecord = record
		version:				SInt32;
		container:				QTAtomContainer;
		tweenAtom:				QTAtom;
		dataAtom:				QTAtom;
		percent:				Fixed;
		dataProc:				TweenerDataUPP;
		private1:				Ptr;
		private2:				Ptr;
	end;

	TweenV1RecordPtr = ^TweenV1Record;
	TweenV1Record = record
		version:				SInt32;
		container:				QTAtomContainer;
		tweenAtom:				QTAtom;
		dataAtom:				QTAtom;
		percent:				Fixed;
		dataProc:				TweenerDataUPP;
		private1:				Ptr;
		private2:				Ptr;
		fractPercent:			Fract;
	end;


const
	kTweenRecordNoFlags			= 0;
	kTweenRecordIsAtInterruptTime = $00000001;


type
	TweenV2RecordPtr = ^TweenV2Record;
	TweenV2Record = record
		version:				SInt32;
		container:				QTAtomContainer;
		tweenAtom:				QTAtom;
		dataAtom:				QTAtom;
		percent:				Fixed;
		dataProc:				TweenerDataUPP;
		private1:				Ptr;
		private2:				Ptr;
		fractPercent:			Fract;
		flags:					SInt32;
	end;

	{
	 *  QTNewTween()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 *    Windows:          in qtmlClient.lib 3.0 and later
	 	}
function QTNewTween(var tween: QTTweener; container: QTAtomContainer; tweenAtom: QTAtom; maxTime: TimeValue): OSErr; external name '_QTNewTween';
{
 *  QTDisposeTween()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function QTDisposeTween(tween: QTTweener): OSErr; external name '_QTDisposeTween';
{
 *  QTDoTween()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function QTDoTween(tween: QTTweener; atTime: TimeValue; result: Handle; var resultSize: SInt32; tweenDataProc: TweenerDataUPP; tweenDataRefCon: UnivPtr): OSErr; external name '_QTDoTween';
{
    QTDoTweenPtr is an interrupt-safe version of QTDoTween.  It has the following limitations:
     - not all tween types support this call (those which must allocated memory), in which case they return codecUnimpErr.
     - the QTAtomContainer used for the tween must be locked
     - the dataSize must be large enough to contain the result
     - this call is not supported for sequence tweens, use interpolation tweens instead
}
{
 *  QTDoTweenPtr()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 6.0 and later
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.2 and later
 *    Windows:          in qtmlClient.lib 6.0 and later
 }
function QTDoTweenPtr(tween: QTTweener; atTime: TimeValue; result: Ptr; resultSize: SInt32): OSErr; external name '_QTDoTweenPtr';
{****
    Sound Description Manipulations
****}
{
 *  AddSoundDescriptionExtension()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function AddSoundDescriptionExtension(desc: SoundDescriptionHandle; extension: Handle; idType: OSType): OSErr; external name '_AddSoundDescriptionExtension';
{
 *  GetSoundDescriptionExtension()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetSoundDescriptionExtension(desc: SoundDescriptionHandle; var extension: Handle; idType: OSType): OSErr; external name '_GetSoundDescriptionExtension';
{
 *  RemoveSoundDescriptionExtension()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function RemoveSoundDescriptionExtension(desc: SoundDescriptionHandle; idType: OSType): OSErr; external name '_RemoveSoundDescriptionExtension';
{****
    Preferences
****}
{
 *  GetQuickTimePreference()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetQuickTimePreference(preferenceType: OSType; var preferenceAtom: QTAtomContainer): OSErr; external name '_GetQuickTimePreference';
{
 *  SetQuickTimePreference()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SetQuickTimePreference(preferenceType: OSType; preferenceAtom: QTAtomContainer): OSErr; external name '_SetQuickTimePreference';
{****
    Effects and dialog Support
****}
{  atom types for entries in the effects list }

const
	kEffectNameAtom				= $6E616D65 (* 'name' *);						{  name of effect  }
	kEffectTypeAtom				= $74797065 (* 'type' *);						{  codec sub-type for effect  }
	kEffectManufacturerAtom		= $6D616E75 (* 'manu' *);						{  codec manufacturer for effect  }


type
	QTParamPreviewRecordPtr = ^QTParamPreviewRecord;
	QTParamPreviewRecord = record
		sourceID:				SInt32;								{  1 based source identifier }
		sourcePicture:			PicHandle;								{  picture for preview, must not dispose until dialog is disposed }
	end;

	QTParamPreviewPtr					= ^QTParamPreviewRecord;
	QTParamDialogEventRecordPtr = ^QTParamDialogEventRecord;
	QTParamDialogEventRecord = record
		theEvent:				EventRecordPtr;							{  Event received by the dialog  }
		whichDialog:			DialogRef;								{  dialog that event was directed towards  }
		itemHit:				SInt16;								{  dialog item which was hit  }
	end;

	QTParamDialogEventPtr				= ^QTParamDialogEventRecord;
	QTParamFetchPreviewRecordPtr = ^QTParamFetchPreviewRecord;
	QTParamFetchPreviewRecord = record
		theWorld:				GWorldPtr;								{  the world into which to draw the preview  }
		percentage:				Fixed;									{  frame percentage (from 0.0 - 1.0) to be drawn  }
	end;

	QTParamFetchPreviewPtr				= ^QTParamFetchPreviewRecord;

const
	pdActionConfirmDialog		= 1;							{  no param }
	pdActionSetAppleMenu		= 2;							{  param is MenuRef }
	pdActionSetEditMenu			= 3;							{  param is MenuRef }
	pdActionGetDialogValues		= 4;							{  param is QTAtomContainer }
	pdActionSetPreviewUserItem	= 5;							{  param is long }
	pdActionSetPreviewPicture	= 6;							{  param is QTParamPreviewPtr; }
	pdActionSetColorPickerEventProc = 7;						{  param is UserEventUPP }
	pdActionSetDialogTitle		= 8;							{  param is StringPtr  }
	pdActionGetSubPanelMenu		= 9;							{  param is MenuRef*  }
	pdActionActivateSubPanel	= 10;							{  param is long  }
	pdActionConductStopAlert	= 11;							{  param is StringPtr  }
	pdActionModelessCallback	= 12;							{  param is QTParamDialogEventPtr  }
	pdActionFetchPreview		= 13;							{  param is QTParamFetchPreviewPtr  }
	pdActionSetDialogSettings	= 14;							{  param is QTAtomContainer  }
	pdActionGetDialogSettings	= 15;							{  param is QTAtomContainer  }
	pdActionGetNextSample		= 16;							{  param is QTAtomContainer with effect sample to change - createdDialog may be NIL  }
	pdActionGetPreviousSample	= 17;							{  param is QTAtomContainer with effect sample to change - createdDialog may be NIL  }
	pdActionCompactSample		= 18;							{  param is QTAtomContainer with effect sample to compact, - createdDialog may be NIL  }
	pdActionSetEditCallout		= 19;							{  param is QTParamPreviewCalloutPtr, can be NIL  }
	pdActionSetSampleTime		= 20;							{  param is QTParamSampleTimePtr, can be NIL  }
	pdActionDoEditCommand		= 21;							{  param is long with menu command (ie, mcMenuCut etc)  }
	pdActionGetSubPanelMenuValue = 22;							{  param is long and returns current sub-panel value selected by the effect  }
																{  Action codes and typedefs used for custom controls within effects  }
	pdActionCustomNewControl	= 23;							{  param is QTCustomControlNewPtr  }
	pdActionCustomDisposeControl = 24;							{  param is QTCustomControlNewPtr  }
	pdActionCustomPositionControl = 25;							{  param is QTCustomControlPositionControlPtr  }
	pdActionCustomShowHideControl = 26;							{  param is QTCustomControlShowHideControlPtr  }
	pdActionCustomHandleEvent	= 27;							{  param is QTCustomControlHandleEventPtr  }
	pdActionCustomSetFocus		= 28;							{  param is QTCustomControlSetFocusPtr  }
	pdActionCustomSetEditMenu	= 29;							{  param is QTCustomControlSetEditMenuPtr  }
	pdActionCustomSetPreviewPicture = 30;						{  param is QTCustomControlSetPreviewPicturePtr  }
	pdActionCustomSetEditCallout = 31;							{  param is QTCustomControlSetEditCalloutPtr  }
	pdActionCustomGetEnableValue = 32;							{  param is QTCustomControlGetEnableValuePtr  }
	pdActionCustomSetSampleTime	= 33;							{  param is QTCustomControlSetSampleTimePtr  }
	pdActionCustomGetValue		= 34;							{  param is QTCustomControlGetValue  }
	pdActionCustomDoEditCommand	= 35;							{  param is QTCustomControlDoEditCommand  }

	{	 Sample Time information 	}
	pdSampleTimeDisplayOptionsNone = $00000000;


type
	QTParamSampleTimeRecordPtr = ^QTParamSampleTimeRecord;
	QTParamSampleTimeRecord = record
		displayOptions:			SInt32;
		sampleStartTime:		TimeRecord;
		sampleDuration:			TimeValue;
		framesPerSecond:		SInt32;								{  if 0, will cause revert to seconds display }
	end;

	QTParamSampleTimePtr				= ^QTParamSampleTimeRecord;
	{	 Preview change callout information 	}
	QTParamPreviewCalloutRecordPtr = ^QTParamPreviewCalloutRecord;
	QTParamPreviewCalloutRecord = record
		calloutProc:			MoviePreviewCallOutUPP;					{  called when user makes editing changes to dialog.  May be NIL.  You should return true from your function.  }
		refCon:					SInt32;								{  passed to the callout procedure  }
	end;

	QTParamPreviewCalloutPtr			= ^QTParamPreviewCalloutRecord;
	QTParameterDialogOptions			= SInt32;
	{  ------- CUSTOM EFFECT CONTROLS }
	{	
	    Effects may choose to implement custom controls to allow the user to more easily edit complex parameters
	    that are ill-served by simple sliders or type in boxes.   Effects may allow a custom control for either
	    a single parameter, or for a group of parameters.
	    
	    Parameter(s) for a custom control must still be data types defined by the standard set, or for
	    complex records of data, must be defined within a group as individual parameters made up from base
	    data types (for example, a point is a group containing two Fixed point numbers).  
	    This is to allow applications that do not wish to use the custom control for the effect to set values themselves.
	    
	    Effects should be aware that these custom controls may be deployed by the application in either a dialog or
	    a window, with application defined background colors or patterns, along with application defined font
	    characteristics for the window.
	    
	    It is recommended that effects implement custom controls only when needed, and that custom controls be used
	    for specific types of parameters (ie, point, rectangle, polygon, path) rather than the entire user interface
	    for the effect.  Effects may choose to implement multiple custom controls which combine with standard controls
	    to present the total user interface.  For effects which have very complex user interfaces not well suited for 
	    inclusion within a single window, it is recommended to use kParameterImageIsPreset -- which allows the effect to
	    have an external editing application for parameters which may then be set within the standard UI via the open file 
	    dialog or drag and drop.  The Lens Flare effect's "Flare Type" is an example of such a preset.
	
	    For parameters that use a custom control to control a single parameter value, a new behavior
	    flag has been added (kCustomControl), and the behavior for the parameter should be kParameterItemControl.
	    
	    For parameters that are groups, the same flag (kCustomControl) should be used, and the behavior
	    should be kParameterItemGroupDivider.  Groups with the kCustomControl bit set will be implemented
	    by calling the custom control for that group -- the parameters within that group will not be processed
	    in the normal manner.
	    
	    In both cases, the new customType and customID fields of the behavior must be filled in.  These are 
	    used in order to allow your custom control to determine which parameter is being edited in the case
	    where the custom control is used for the editing of multiple parameters.  These values are passed into
	    the pdActionCustomNewControl call.  Since the custom control mechanism is also used by QuickTime's
	    default effect dialogs, you should be prepared to pass onto the base effect any pdActionCustomNewControl
	    calls for type/id pairs that you do not handle yourself.  When  pdActionCustomNewControl is called
	    for controls of types handled by QuickTime, customType is kParameterAtomTypeAndID and customID is
	    the ID of the parameter atom. 
		}


	{	
	    pdActionCustomNewControlControl is called by application to create a new custom control or set of controls
	    for an effect parameter.  When pdActionCustomNewControl is called, the effect should perform any
	    basic allocation it needs for storage and return the result in storage. The options parameter tells
	    the control if the application wishes to support interpolated, optionally interpolated, or a single
	    value parameter.
	    
	    Since pdActionCustomNewControlControl may be called upon your effect for other items within the
	    dialog, it is recommended that your effect have an easy way to determine which controls it implements:
	     a) by having storage be a pointer with an OSType at the begining to mark controls
	        implemented by your code.
	     - or -
	     b) keeping track in your component globals those custom controls which you have created.
	    
	    When pdActionCustomDisposeControl is called any allocation done by the control should be disposed. In addition, 
	    pdActionCustomDisposeControl is the last chance the control has to commit any user changes into the sample.
	    Controls which implement type in fields typically need to commit any final user edits at this time.
		}
	QTCustomControlNewRecordPtr = ^QTCustomControlNewRecord;
	QTCustomControlNewRecord = record
		storage:				Ptr;									{  storage allocated/disposed by the control }
		options:				QTParameterDialogOptions;				{  options used to control interpolation/not }
		sample:					QTAtomContainer;						{  sample that holds the data to be edited }
		customType:				SInt32;								{  custom type and ID specified by effect for creation of this control }
		customID:				SInt32;
	end;

	QTCustomControlNewPtr				= ^QTCustomControlNewRecord;
	{	
	    pdActionCustomPositionControl is called by the application to position the control within a window or dialog.
	
	    The control should determine if it will fit in the alloted area and position itself there.  It should also
	    return the space taken up by the control.   Note you are free to implement controls which are variable in size depending upon
	    which parameter you are editing.  You need not scale your control to the requested size.  If the area presented to your
	    control is too small, set didFit to false.  You should still return in used the size you would have liked to use for
	    the control.   The application will then try again with a new size.  Note that all
	    controls must be able to fit within a minimum of 300 by 250 pixels.
	    
	    Custom controls that draw text should make note of the text font, size, and style at this time in order
	    to properly display within application windows.
	    
	    Note that the default state for the control is hidden.  You will receive a pdActionCustomShowHideControl
	    in order to enable your control.  You should not draw your control in response to pdActionCustomPositionControl.
		}
	QTCustomControlPositionControlRecordPtr = ^QTCustomControlPositionControlRecord;
	QTCustomControlPositionControlRecord = record
		storage:				Ptr;									{  storage for the control }
		window:					WindowPtr;								{  window to be used by the control }
		location:				Rect;									{  location within the window the control may use }
		used:					Rect;									{  returned by the control to indicate size it actually used }
		didFit:					boolean;								{  did the control fit in the specified area? }
		pad1,pad2,pad3:			SInt8;
	end;

	QTCustomControlPositionControlPtr	= ^QTCustomControlPositionControlRecord;
	{	
	    pdActionCustomShowHideControl is called when the application wishes to enable/disable your control, or 
	    completely disable drawing of the control
	    
	    Your control should make note of the new state (if different from the last) and perform an InvalRect()
	    on your drawing area, or you may draw your control's initial state in the case of show.  You should not
	    attempt to erase your control as the result of a hide -- instead call InvalRect() and allow the application
	    to process the resulting event as appropriate.
		}
	QTCustomControlShowHideControlRecordPtr = ^QTCustomControlShowHideControlRecord;
	QTCustomControlShowHideControlRecord = record
		storage:				Ptr;									{  storage for the control }
		show:					boolean;								{  display the control? }
		enable:					boolean;								{  enable the control (ie, black vs gray display) }
		pad:					array [0..1] of boolean;
	end;

	QTCustomControlShowHideControlPtr	= ^QTCustomControlShowHideControlRecord;
	{	
	    pdActionCustomHandleEvent is called to allow your custom control to process events.
	    
	    Typical controls handle the following events:
	        - activate - to draw your control in normal/gray mode
	        - update - to draw your control
	        - mouseDown - to handle clicks
	        - keyDown - to handle typing when you have focus
	        - idle - to perform idle drawing (if applicable)
	    If your control handles the entire event, set didProcess to true.  If
	    you handled the event, but other controls still need the event, set didProcess to false.
	    
	    If your control supports the concept of focus for the purposes of typing (such as by having
	    a type-in box for the parameter) then you set the tookFocus Boolean as part of your processing
	    of the event.  It is assumed that your control will draw the appropriate focus UI as a result, and
	    the calling application will disable any focus drawing within the remainder of the UI.
	
	    By default, custom controls are not given idle time.  If you need idle time, set needIdle to true
	    in response to the even that causes you to need idle (typically the taking of focus, or the first draw).
	    Your control will continue to be given idle events until you set needIdle to false in response to
	    a nullEvent.
		}
	QTCustomControlHandleEventRecordPtr = ^QTCustomControlHandleEventRecord;
	QTCustomControlHandleEventRecord = record
		storage:				Ptr;									{  storage for the control }
		pEvent:					EventRecordPtr;							{  event to process }
		didProcess:				boolean;								{  did we process entire event? }
		tookFocus:				boolean;								{  did we take focus as a result of this event (typically mouseDowns) }
		needIdle:				boolean;								{  does this control need idle events? }
		didEdit:				boolean;								{  did we edit the samples? }
	end;

	QTCustomControlHandleEventPtr		= ^QTCustomControlHandleEventRecord;
	{	
	    pdActionCustomSetFocus is called in order to set or advance the current focus of the user interface, typically
	    because the user has pressed the tab or shift-tab keys, or because the user clicked within the area defined by
	    your control.
	    
	    Your control will be called with pdActionFocusFirst,  pdActionFocusLast, or pdActionFocusOff to set or clear focus on your
	    control.  Your control will be called with pdActionFocusForward or pdActionFocusBackward to cycle
	    focus within your control (if your control has multiple focus).  If your control does not support focus,
	    or the focus request results in focus moving beyond your supported range, return pdActionFocusOff in
	    the focus parameter.  Otherwise, return the focus that you set.
	    
	    Controls which have no focus would always set focus to be pdActionFocusOff.
	    
	    Controls with a single focus would set pdActionFocusFirst when requsted to set either
	    pdActionFocusFirst or pdActionFocusLast, and would set pdActionFocusOff for either
	    pdActionFocusForward or pdActionFocusBackward.
		}

const
	pdActionFocusOff			= 0;							{  no focus  }
	pdActionFocusFirst			= 1;							{  focus on first element  }
	pdActionFocusLast			= 2;							{  focus on last element  }
	pdActionFocusForward		= 3;							{  focus on next element  }
	pdActionFocusBackward		= 4;							{  focus on previous element  }


type
	QTCustomControlSetFocusRecordPtr = ^QTCustomControlSetFocusRecord;
	QTCustomControlSetFocusRecord = record
		storage:				Ptr;									{  storage for the control }
		focus:					SInt32;								{  focus to set, return resulting focus }
	end;

	QTCustomControlSetFocusPtr			= ^QTCustomControlSetFocusRecord;
	{	 
	    pdActionCustomSetEditMenu will be called to inform your custom control of the location of the edit menu.
	    
	    If your control has editing boxes, this is useful in order to allow the user to perform cut/copy/paste operations
	    when focus is on one of these boxes.
		}
	QTCustomControlSetEditMenuRecordPtr = ^QTCustomControlSetEditMenuRecord;
	QTCustomControlSetEditMenuRecord = record
		storage:				Ptr;									{  storage for the control }
		editMenu:				MenuHandle;								{  edit menu, or NIL }
	end;

	QTCustomControlSetEditMenuPtr		= ^QTCustomControlSetEditMenuRecord;
	{	
	    pdActionCustomSetPreviewPicture will be called to inform your custom control of preview information that you
	    may wish to use in the drawing of your user interface.  
		}
	QTCustomControlSetPreviewPictureRecordPtr = ^QTCustomControlSetPreviewPictureRecord;
	QTCustomControlSetPreviewPictureRecord = record
		storage:				Ptr;									{  storage for the control }
		preview:				QTParamPreviewPtr;						{  preview to set }
	end;

	QTCustomControlSetPreviewPicturePtr	= ^QTCustomControlSetPreviewPictureRecord;
	{	
	    pdActionCustomSetEditCallout tells your control of the need by the application to be informed of
	    changes to the parameter values (typically for the purposes of updating previews).
	    
	    If a callout is available, your custom control should call it whenever a change has been
	    made to the parameter(s) that your control is editing (as a result of user actions, most typically).
	    If you choose not to implement this, live dragging or updating of values will not work.
		}
	QTCustomControlSetEditCalloutRecordPtr = ^QTCustomControlSetEditCalloutRecord;
	QTCustomControlSetEditCalloutRecord = record
		storage:				Ptr;									{  storage for the control }
		callout:				QTParamPreviewCalloutPtr;				{  requested callout, or NIL to disable }
	end;

	QTCustomControlSetEditCalloutPtr	= ^QTCustomControlSetEditCalloutRecord;
	{	
	    pdActionCustomGetEnableValue allows you to return a value for the purposes of enabling/disabling
	    other controls.
	    Most custom controls do not need to implement this call.
	    
	    If your control is able to control the enabling and disabling of other parameter controls (such as is done
	    by standard pop up or enumerated type controls), you need to supply a value that can be use for greater than/less than
	    types of comparisons.
		}
	QTCustomControlGetEnableValueRecordPtr = ^QTCustomControlGetEnableValueRecord;
	QTCustomControlGetEnableValueRecord = record
		storage:				Ptr;									{  storage for the control }
		currentValue:			SInt32;								{  value to compare against for enable/disable purposes }
	end;

	QTCustomControlGetEnableValuePtr	= ^QTCustomControlGetEnableValueRecord;
	{	
	    pdActionCustomSetSampleTime tells your control information from the application about the duration
	    and start time for the sample being edited.
	    
	    Most controls do not need this information, but some may choose to use it in the interface
	    they present the user.  However, this call need not be made by applications, so the custom
	    control should be prepared to run when the sample time information is not available.
		}
	QTCustomControlSetSampleTimeRecordPtr = ^QTCustomControlSetSampleTimeRecord;
	QTCustomControlSetSampleTimeRecord = record
		storage:				Ptr;									{  storage for the control }
		sampleTime:				QTParamSampleTimePtr;					{  sample time information or NIL }
	end;

	QTCustomControlSetSampleTimePtr		= ^QTCustomControlSetSampleTimeRecord;
	{	
	    pdActionCustomGetValue tells your control to store any value(s) into the specified atom container.
	    
	    All custom controls must implement this call
		}
	QTCustomControlGetValueRecordPtr = ^QTCustomControlGetValueRecord;
	QTCustomControlGetValueRecord = record
		storage:				Ptr;									{  storage for the control }
		sample:					QTAtomContainer;						{  sample to store into }
	end;

	QTCustomControlGetValuePtr			= ^QTCustomControlGetValueRecord;
	{	
	    pdActionCustomDoEditCommand tells your control to handle edit commands if it allow focus and type in boxes.
	    
	    All custom controls must implement this call if they support edit boxes
		}
	QTCustomControlDoEditCommandRecordPtr = ^QTCustomControlDoEditCommandRecord;
	QTCustomControlDoEditCommandRecord = record
		storage:				Ptr;									{  storage for the control }
		command:				SInt32;								{  command to execute, return 0 here if processed }
	end;

	QTCustomControlDoEditCommandPtr		= ^QTCustomControlDoEditCommandRecord;
	QTParameterDialog					= SInt32;

const
	elOptionsIncludeNoneInList	= $00000001;					{  "None" effect is included in list  }


type
	QTEffectListOptions					= SInt32;

const
	pdOptionsCollectOneValue	= $00000001;					{  should collect a single value only }
	pdOptionsAllowOptionalInterpolations = $00000002;			{  non-novice interpolation options are shown  }
	pdOptionsModalDialogBox		= $00000004;					{  dialog box should be modal  }
	pdOptionsEditCurrentEffectOnly = $00000008;					{  List of effects will not be shown  }
	pdOptionsHidePreview		= $00000010;					{  Preview item will not be shown  }

	effectIsRealtime			= 0;							{  effect can be rendered in real time  }

	{	
	    QTGetEffectsListExtended is a call that provides for 
	    more advanced filtering of effects to be placed into the
	    effect list.  Applications can filter on:
	     1) number of input sources
	     2) effect major or minor class
	     3) custom filtering through a callback
	    The callback will be called for each effect which passes
	    the other criteria for inclusion.  If the callback
	    returns a true result the effect will be included in the list.
	    
	    Note that your filter proc may receive multiple effects from various
	    manufacturers.  If you return true for multiple effects of a given type
	    only the one with the higher parameter version number will be included.
	    If you wish other filtering (such as effects from a given manufacturer, you
	    can do this by return false for the other effects and true for those
	    that you prefer.
		}

type
{$ifc TYPED_FUNCTION_POINTERS}
	QTEffectListFilterProcPtr = function(effect: Component; effectMinSource: SInt32; effectMaxSource: SInt32; majorClass: OSType; minorClass: OSType; refcon: UnivPtr): boolean;
{$elsec}
	QTEffectListFilterProcPtr = ProcPtr;
{$endc}

{$ifc OPAQUE_UPP_TYPES}
	QTEffectListFilterUPP = ^SInt32; { an opaque UPP }
{$elsec}
	QTEffectListFilterUPP = UniversalProcPtr;
{$endc}	
	{
	 *  QTGetEffectsList()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 *    Windows:          in qtmlClient.lib 3.0 and later
	 	}
function QTGetEffectsList(var returnedList: QTAtomContainer; minSources: SInt32; maxSources: SInt32; getOptions: QTEffectListOptions): OSErr; external name '_QTGetEffectsList';
{
 *  QTGetEffectsListExtended()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 6.0 and later
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.2 and later
 *    Windows:          in qtmlClient.lib 6.0 and later
 }
function QTGetEffectsListExtended(var returnedList: QTAtomContainer; minSources: SInt32; maxSources: SInt32; getOptions: QTEffectListOptions; majorClass: OSType; minorClass: OSType; filterProc: QTEffectListFilterUPP; filterRefCon: UnivPtr): OSErr; external name '_QTGetEffectsListExtended';
{
 *  QTCreateStandardParameterDialog()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function QTCreateStandardParameterDialog(effectList: QTAtomContainer; parameters: QTAtomContainer; dialogOptions: QTParameterDialogOptions; var createdDialog: QTParameterDialog): OSErr; external name '_QTCreateStandardParameterDialog';
{
 *  QTIsStandardParameterDialogEvent()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function QTIsStandardParameterDialogEvent(var pEvent: EventRecord; createdDialog: QTParameterDialog): OSErr; external name '_QTIsStandardParameterDialogEvent';
{
 *  QTDismissStandardParameterDialog()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function QTDismissStandardParameterDialog(createdDialog: QTParameterDialog): OSErr; external name '_QTDismissStandardParameterDialog';
{
 *  QTStandardParameterDialogDoAction()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function QTStandardParameterDialogDoAction(createdDialog: QTParameterDialog; action: SInt32; params: UnivPtr): OSErr; external name '_QTStandardParameterDialogDoAction';
{
 *  QTGetEffectSpeed()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function QTGetEffectSpeed(parameters: QTAtomContainer; var pFPS: Fixed): OSErr; external name '_QTGetEffectSpeed';
{****
    Error reporting
****}

type
	QTErrorReplacementRecordPtr = ^QTErrorReplacementRecord;
	QTErrorReplacementRecord = record
		numEntries:				SInt32;
		replacementString:		array [0..0] of StringPtr;				{  array of numEntries StringPtrs (each String is allocated separately). }
	end;

	QTErrorReplacementPtr				= ^QTErrorReplacementRecord;
	{	
	    QTAddMovieError is used to add orthogonal errors to a list of errors that will
	    later be reported (at the end of an import or playback, for example).  Errors are stored
	    in 'qter' resources within the component.
	    
	    QTAddMovieError(Movie       addTo,                          // in: movie to add error to
	                    Component   adder,                          // in: component which is adding the error
	                    long        errorCode,                      // in: error code being added
	                    QTErrorReplacementPtr   stringReplacements);// in: list of strings to subsitute (in order) for "^1", "^2", etc
		}
	{
	 *  QTAddMovieError()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in QuickTimeLib 6.0 and later
	 *    CarbonLib:        in CarbonLib 1.6 and later
	 *    Mac OS X:         in version 10.2 and later
	 *    Windows:          in qtmlClient.lib 6.0 and later
	 	}
function QTAddMovieError(movieH: Movie; c: Component; errorCode: SInt32; stringReplacements: QTErrorReplacementPtr): OSErr; external name '_QTAddMovieError';
{****
    Access Keys
****}

const
	kAccessKeyAtomType			= $61636B79 (* 'acky' *);

	kAccessKeySystemFlag		= $00000001;

	{
	 *  QTGetAccessKeys()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 *    Windows:          in qtmlClient.lib 3.0 and later
	 	}
function QTGetAccessKeys(var accessKeyType: Str255; flags: SInt32; var keys: QTAtomContainer): OSErr; external name '_QTGetAccessKeys';
{
 *  QTRegisterAccessKey()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function QTRegisterAccessKey(var accessKeyType: Str255; flags: SInt32; accessKey: Handle): OSErr; external name '_QTRegisterAccessKey';
{
 *  QTUnregisterAccessKey()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function QTUnregisterAccessKey(var accessKeyType: Str255; flags: SInt32; accessKey: Handle): OSErr; external name '_QTUnregisterAccessKey';
{****
    Content Restrictions
****}

{
 *  QTGetMovieRestrictions()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 6.0 and later
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.2 and later
 *    Windows:          in qtmlClient.lib 6.0 and later
 }
function QTGetMovieRestrictions(theMovie: Movie; var outRestrictionSet: QTRestrictionSet; var outSeed: UInt32): OSErr; external name '_QTGetMovieRestrictions';
{
 *  QTRestrictionsGetInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 6.0 and later
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.2 and later
 *    Windows:          in qtmlClient.lib 6.0 and later
 }
function QTRestrictionsGetInfo(inRestrictionSet: QTRestrictionSet; var outRestrictionClassCount: SInt32; var outSeed: SInt32): OSErr; external name '_QTRestrictionsGetInfo';
{
 *  QTRestrictionsGetIndClass()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 6.0 and later
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.2 and later
 *    Windows:          in qtmlClient.lib 6.0 and later
 }
function QTRestrictionsGetIndClass(inRestrictionSet: QTRestrictionSet; inIndex: SInt32; var outClass: OSType): OSErr; external name '_QTRestrictionsGetIndClass';
{
 *  QTRestrictionsGetItem()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 6.0 and later
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.2 and later
 *    Windows:          in qtmlClient.lib 6.0 and later
 }
function QTRestrictionsGetItem(inRestrictionSet: QTRestrictionSet; inRestrictionClass: OSType; var outRestrictions: UInt32): OSErr; external name '_QTRestrictionsGetItem';
{
 *  QTGetSupportedRestrictions()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 6.0 and later
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.2 and later
 *    Windows:          in qtmlClient.lib 6.0 and later
 }
function QTGetSupportedRestrictions(inRestrictionClass: OSType; var outRestrictionIDs: UInt32): OSErr; external name '_QTGetSupportedRestrictions';
{
 *  QTCreateUUID()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 6.0 and later
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.2 and later
 *    Windows:          in qtmlClient.lib 6.0 and later
 }
function QTCreateUUID(var outUUID: QTUUID; creationFlags: SInt32): OSErr; external name '_QTCreateUUID';
{
 *  QTEqualUUIDs()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 6.0 and later
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.2 and later
 *    Windows:          in qtmlClient.lib 6.0 and later
 }
function QTEqualUUIDs(const (*var*) uuid1: QTUUID; const (*var*) uuid2: QTUUID): boolean; external name '_QTEqualUUIDs';
{****
    Time table
****}
{
 *  MakeTrackTimeTable()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MakeTrackTimeTable(trackH: Track; var offsets: SInt32Ptr; startTime: TimeValue; endTime: TimeValue; timeIncrement: TimeValue; firstDataRefIndex: SInt16; lastDataRefIndex: SInt16; var retdataRefSkew: SInt32): OSErr; external name '_MakeTrackTimeTable';
{
 *  MakeMediaTimeTable()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MakeMediaTimeTable(theMedia: Media; var offsets: SInt32Ptr; startTime: TimeValue; endTime: TimeValue; timeIncrement: TimeValue; firstDataRefIndex: SInt16; lastDataRefIndex: SInt16; var retdataRefSkew: SInt32): OSErr; external name '_MakeMediaTimeTable';
{
 *  GetMaxLoadedTimeInMovie()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetMaxLoadedTimeInMovie(theMovie: Movie; var time: TimeValue): OSErr; external name '_GetMaxLoadedTimeInMovie';
{
 *  QTMovieNeedsTimeTable()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function QTMovieNeedsTimeTable(theMovie: Movie; var needsTimeTable: boolean): OSErr; external name '_QTMovieNeedsTimeTable';
{
 *  QTGetDataRefMaxFileOffset()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function QTGetDataRefMaxFileOffset(movieH: Movie; dataRefType: OSType; dataRef: Handle; var offset: SInt32): OSErr; external name '_QTGetDataRefMaxFileOffset';
{****
    Bandwidth management support
****}

const
	ConnectionSpeedPrefsType	= $63737064 (* 'cspd' *);
	BandwidthManagementPrefsType = $62776D67 (* 'bwmg' *);


type
	ConnectionSpeedPrefsRecordPtr = ^ConnectionSpeedPrefsRecord;
	ConnectionSpeedPrefsRecord = record
		connectionSpeed:		SInt32;
	end;

	ConnectionSpeedPrefsPtr				= ^ConnectionSpeedPrefsRecord;
	ConnectionSpeedPrefsHandle			= ^ConnectionSpeedPrefsPtr;
	BandwidthManagementPrefsRecordPtr = ^BandwidthManagementPrefsRecord;
	BandwidthManagementPrefsRecord = record
		overrideConnectionSpeedForBandwidth: boolean;
		pad: SInt8
	end;

	BandwidthManagementPrefsPtr			= ^BandwidthManagementPrefsRecord;
	BandwidthManagementPrefsHandle		= ^BandwidthManagementPrefsPtr;

const
	kQTIdlePriority				= 10;
	kQTNonRealTimePriority		= 20;
	kQTRealTimeSharedPriority	= 25;
	kQTRealTimePriority			= 30;

	kQTBandwidthNotifyNeedToStop = $00000001;
	kQTBandwidthNotifyGoodToGo	= $00000002;
	kQTBandwidthChangeRequest	= $00000004;
	kQTBandwidthQueueRequest	= $00000008;
	kQTBandwidthScheduledRequest = $00000010;
	kQTBandwidthVoluntaryRelease = $00000020;


type
{$ifc TYPED_FUNCTION_POINTERS}
	QTBandwidthNotificationProcPtr = function(flags: SInt32; reserved: UnivPtr; refcon: UnivPtr): OSErr;
{$elsec}
	QTBandwidthNotificationProcPtr = ProcPtr;
{$endc}

	QTScheduledBandwidthRecordPtr = ^QTScheduledBandwidthRecord;
	QTScheduledBandwidthRecord = record
		recordSize:				SInt32;								{  total number of bytes in QTScheduledBandwidthRecord }
		priority:				SInt32;
		dataRate:				SInt32;
		startTime:				CompTimeValue;							{  bandwidth usage start time }
		duration:				CompTimeValue;							{  duration of bandwidth usage (0 if unknown) }
		prerollDuration:		CompTimeValue;							{  time for negotiation before startTime (0 if unknown) }
		scale:					TimeScale;								{  timescale of value/duration/prerollDuration fields }
		base:					TimeBase;								{  timebase }
	end;

	QTScheduledBandwidthPtr				= ^QTScheduledBandwidthRecord;
	QTScheduledBandwidthHandle			= ^QTScheduledBandwidthPtr;
{$ifc OPAQUE_UPP_TYPES}
	QTBandwidthNotificationUPP = ^SInt32; { an opaque UPP }
{$elsec}
	QTBandwidthNotificationUPP = UniversalProcPtr;
{$endc}	
	{
	 *  QTBandwidthRequest()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
	 *    CarbonLib:        in CarbonLib 1.0.2 and later
	 *    Mac OS X:         in version 10.0 and later
	 *    Windows:          in qtmlClient.lib 4.0 and later
	 	}
function QTBandwidthRequest(priority: SInt32; callback: QTBandwidthNotificationUPP; refcon: UnivPtr; var bwRef: QTBandwidthReference; flags: SInt32): OSErr; external name '_QTBandwidthRequest';
{
 *  QTBandwidthRequestForTimeBase()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.1 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.1 and later
 }
function QTBandwidthRequestForTimeBase(tb: TimeBase; priority: SInt32; callback: QTBandwidthNotificationUPP; refcon: UnivPtr; var bwRef: QTBandwidthReference; flags: SInt32): OSErr; external name '_QTBandwidthRequestForTimeBase';
{
 *  QTBandwidthRelease()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function QTBandwidthRelease(bwRef: QTBandwidthReference; flags: SInt32): OSErr; external name '_QTBandwidthRelease';
{
 *  QTScheduledBandwidthRequest()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.1 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.1 and later
 }
function QTScheduledBandwidthRequest(scheduleRec: QTScheduledBandwidthPtr; notificationCallback: QTBandwidthNotificationUPP; refcon: UnivPtr; var sbwRef: QTScheduledBandwidthReference; flags: SInt32): OSErr; external name '_QTScheduledBandwidthRequest';
{
 *  QTScheduledBandwidthRelease()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.1 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.1 and later
 }
function QTScheduledBandwidthRelease(sbwRef: QTScheduledBandwidthReference; flags: SInt32): OSErr; external name '_QTScheduledBandwidthRelease';
const
	uppQTCallBackProcInfo = $000003C0;
	uppQTSyncTaskProcInfo = $000000C0;
	uppMovieRgnCoverProcInfo = $00000FE0;
	uppMovieProgressProcInfo = $0000FAE0;
	uppMovieDrawingCompleteProcInfo = $000003E0;
	uppTrackTransferProcInfo = $000003E0;
	uppGetMovieProcInfo = $00003FE0;
	uppMoviePreviewCallOutProcInfo = $000000D0;
	uppTextMediaProcInfo = $00003FE0;
	uppActionsProcInfo = $00003FE0;
	uppDoMCActionProcInfo = $00003EE0;
	uppMovieExecuteWiredActionsProcInfo = $00003FE0;
	uppMoviePrePrerollCompleteProcInfo = $00000EC0;
	uppQTNextTaskNeededSoonerCallbackProcInfo = $00000FC0;
	uppMoviesErrorProcInfo = $00000380;
	uppTweenerDataProcInfo = $003FFFF0;
	uppQTEffectListFilterProcInfo = $0003FFD0;
	uppQTBandwidthNotificationProcInfo = $00000FE0;
	{
	 *  NewQTCallBackUPP()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   available as macro/inline
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
function NewQTCallBackUPP(userRoutine: QTCallBackProcPtr): QTCallBackUPP; external name '_NewQTCallBackUPP'; { old name was NewQTCallBackProc }
{
 *  NewQTSyncTaskUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewQTSyncTaskUPP(userRoutine: QTSyncTaskProcPtr): QTSyncTaskUPP; external name '_NewQTSyncTaskUPP'; { old name was NewQTSyncTaskProc }
{
 *  NewMovieRgnCoverUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewMovieRgnCoverUPP(userRoutine: MovieRgnCoverProcPtr): MovieRgnCoverUPP; external name '_NewMovieRgnCoverUPP'; { old name was NewMovieRgnCoverProc }
{
 *  NewMovieProgressUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewMovieProgressUPP(userRoutine: MovieProgressProcPtr): MovieProgressUPP; external name '_NewMovieProgressUPP'; { old name was NewMovieProgressProc }
{
 *  NewMovieDrawingCompleteUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewMovieDrawingCompleteUPP(userRoutine: MovieDrawingCompleteProcPtr): MovieDrawingCompleteUPP; external name '_NewMovieDrawingCompleteUPP'; { old name was NewMovieDrawingCompleteProc }
{
 *  NewTrackTransferUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewTrackTransferUPP(userRoutine: TrackTransferProcPtr): TrackTransferUPP; external name '_NewTrackTransferUPP'; { old name was NewTrackTransferProc }
{
 *  NewGetMovieUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewGetMovieUPP(userRoutine: GetMovieProcPtr): GetMovieUPP; external name '_NewGetMovieUPP'; { old name was NewGetMovieProc }
{
 *  NewMoviePreviewCallOutUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewMoviePreviewCallOutUPP(userRoutine: MoviePreviewCallOutProcPtr): MoviePreviewCallOutUPP; external name '_NewMoviePreviewCallOutUPP'; { old name was NewMoviePreviewCallOutProc }
{
 *  NewTextMediaUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewTextMediaUPP(userRoutine: TextMediaProcPtr): TextMediaUPP; external name '_NewTextMediaUPP'; { old name was NewTextMediaProc }
{
 *  NewActionsUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewActionsUPP(userRoutine: ActionsProcPtr): ActionsUPP; external name '_NewActionsUPP'; { old name was NewActionsProc }
{
 *  NewDoMCActionUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewDoMCActionUPP(userRoutine: DoMCActionProcPtr): DoMCActionUPP; external name '_NewDoMCActionUPP'; { old name was NewDoMCActionProc }
{
 *  NewMovieExecuteWiredActionsUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewMovieExecuteWiredActionsUPP(userRoutine: MovieExecuteWiredActionsProcPtr): MovieExecuteWiredActionsUPP; external name '_NewMovieExecuteWiredActionsUPP'; { old name was NewMovieExecuteWiredActionsProc }
{
 *  NewMoviePrePrerollCompleteUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewMoviePrePrerollCompleteUPP(userRoutine: MoviePrePrerollCompleteProcPtr): MoviePrePrerollCompleteUPP; external name '_NewMoviePrePrerollCompleteUPP'; { old name was NewMoviePrePrerollCompleteProc }
{
 *  NewQTNextTaskNeededSoonerCallbackUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.2 and later
 }
function NewQTNextTaskNeededSoonerCallbackUPP(userRoutine: QTNextTaskNeededSoonerCallbackProcPtr): QTNextTaskNeededSoonerCallbackUPP; external name '_NewQTNextTaskNeededSoonerCallbackUPP'; { old name was NewQTNextTaskNeededSoonerCallbackProc }
{
 *  NewMoviesErrorUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewMoviesErrorUPP(userRoutine: MoviesErrorProcPtr): MoviesErrorUPP; external name '_NewMoviesErrorUPP'; { old name was NewMoviesErrorProc }
{
 *  NewTweenerDataUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewTweenerDataUPP(userRoutine: TweenerDataProcPtr): TweenerDataUPP; external name '_NewTweenerDataUPP'; { old name was NewTweenerDataProc }
{
 *  NewQTEffectListFilterUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.2 and later
 }
function NewQTEffectListFilterUPP(userRoutine: QTEffectListFilterProcPtr): QTEffectListFilterUPP; external name '_NewQTEffectListFilterUPP'; { old name was NewQTEffectListFilterProc }
{
 *  NewQTBandwidthNotificationUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewQTBandwidthNotificationUPP(userRoutine: QTBandwidthNotificationProcPtr): QTBandwidthNotificationUPP; external name '_NewQTBandwidthNotificationUPP'; { old name was NewQTBandwidthNotificationProc }
{
 *  DisposeQTCallBackUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeQTCallBackUPP(userUPP: QTCallBackUPP); external name '_DisposeQTCallBackUPP';
{
 *  DisposeQTSyncTaskUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeQTSyncTaskUPP(userUPP: QTSyncTaskUPP); external name '_DisposeQTSyncTaskUPP';
{
 *  DisposeMovieRgnCoverUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeMovieRgnCoverUPP(userUPP: MovieRgnCoverUPP); external name '_DisposeMovieRgnCoverUPP';
{
 *  DisposeMovieProgressUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeMovieProgressUPP(userUPP: MovieProgressUPP); external name '_DisposeMovieProgressUPP';
{
 *  DisposeMovieDrawingCompleteUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeMovieDrawingCompleteUPP(userUPP: MovieDrawingCompleteUPP); external name '_DisposeMovieDrawingCompleteUPP';
{
 *  DisposeTrackTransferUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeTrackTransferUPP(userUPP: TrackTransferUPP); external name '_DisposeTrackTransferUPP';
{
 *  DisposeGetMovieUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeGetMovieUPP(userUPP: GetMovieUPP); external name '_DisposeGetMovieUPP';
{
 *  DisposeMoviePreviewCallOutUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeMoviePreviewCallOutUPP(userUPP: MoviePreviewCallOutUPP); external name '_DisposeMoviePreviewCallOutUPP';
{
 *  DisposeTextMediaUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeTextMediaUPP(userUPP: TextMediaUPP); external name '_DisposeTextMediaUPP';
{
 *  DisposeActionsUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeActionsUPP(userUPP: ActionsUPP); external name '_DisposeActionsUPP';
{
 *  DisposeDoMCActionUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeDoMCActionUPP(userUPP: DoMCActionUPP); external name '_DisposeDoMCActionUPP';
{
 *  DisposeMovieExecuteWiredActionsUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeMovieExecuteWiredActionsUPP(userUPP: MovieExecuteWiredActionsUPP); external name '_DisposeMovieExecuteWiredActionsUPP';
{
 *  DisposeMoviePrePrerollCompleteUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeMoviePrePrerollCompleteUPP(userUPP: MoviePrePrerollCompleteUPP); external name '_DisposeMoviePrePrerollCompleteUPP';
{
 *  DisposeQTNextTaskNeededSoonerCallbackUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.2 and later
 }
procedure DisposeQTNextTaskNeededSoonerCallbackUPP(userUPP: QTNextTaskNeededSoonerCallbackUPP); external name '_DisposeQTNextTaskNeededSoonerCallbackUPP';
{
 *  DisposeMoviesErrorUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeMoviesErrorUPP(userUPP: MoviesErrorUPP); external name '_DisposeMoviesErrorUPP';
{
 *  DisposeTweenerDataUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeTweenerDataUPP(userUPP: TweenerDataUPP); external name '_DisposeTweenerDataUPP';
{
 *  DisposeQTEffectListFilterUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.2 and later
 }
procedure DisposeQTEffectListFilterUPP(userUPP: QTEffectListFilterUPP); external name '_DisposeQTEffectListFilterUPP';
{
 *  DisposeQTBandwidthNotificationUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeQTBandwidthNotificationUPP(userUPP: QTBandwidthNotificationUPP); external name '_DisposeQTBandwidthNotificationUPP';
{
 *  InvokeQTCallBackUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure InvokeQTCallBackUPP(cb: QTCallBack; refCon: SInt32; userRoutine: QTCallBackUPP); external name '_InvokeQTCallBackUPP'; { old name was CallQTCallBackProc }
{
 *  InvokeQTSyncTaskUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure InvokeQTSyncTaskUPP(task: UnivPtr; userRoutine: QTSyncTaskUPP); external name '_InvokeQTSyncTaskUPP'; { old name was CallQTSyncTaskProc }
{
 *  InvokeMovieRgnCoverUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function InvokeMovieRgnCoverUPP(theMovie: Movie; changedRgn: RgnHandle; refcon: SInt32; userRoutine: MovieRgnCoverUPP): OSErr; external name '_InvokeMovieRgnCoverUPP'; { old name was CallMovieRgnCoverProc }
{
 *  InvokeMovieProgressUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function InvokeMovieProgressUPP(theMovie: Movie; message: SInt16; whatOperation: SInt16; percentDone: Fixed; refcon: SInt32; userRoutine: MovieProgressUPP): OSErr; external name '_InvokeMovieProgressUPP'; { old name was CallMovieProgressProc }
{
 *  InvokeMovieDrawingCompleteUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function InvokeMovieDrawingCompleteUPP(theMovie: Movie; refCon: SInt32; userRoutine: MovieDrawingCompleteUPP): OSErr; external name '_InvokeMovieDrawingCompleteUPP'; { old name was CallMovieDrawingCompleteProc }
{
 *  InvokeTrackTransferUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function InvokeTrackTransferUPP(t: Track; refCon: SInt32; userRoutine: TrackTransferUPP): OSErr; external name '_InvokeTrackTransferUPP'; { old name was CallTrackTransferProc }
{
 *  InvokeGetMovieUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function InvokeGetMovieUPP(offset: SInt32; size: SInt32; dataPtr: UnivPtr; refCon: UnivPtr; userRoutine: GetMovieUPP): OSErr; external name '_InvokeGetMovieUPP'; { old name was CallGetMovieProc }
{
 *  InvokeMoviePreviewCallOutUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function InvokeMoviePreviewCallOutUPP(refcon: SInt32; userRoutine: MoviePreviewCallOutUPP): boolean; external name '_InvokeMoviePreviewCallOutUPP'; { old name was CallMoviePreviewCallOutProc }
{
 *  InvokeTextMediaUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function InvokeTextMediaUPP(theText: Handle; theMovie: Movie; var displayFlag: SInt16; refcon: SInt32; userRoutine: TextMediaUPP): OSErr; external name '_InvokeTextMediaUPP'; { old name was CallTextMediaProc }
{
 *  InvokeActionsUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function InvokeActionsUPP(refcon: UnivPtr; targetTrack: Track; targetRefCon: SInt32; theEvent: QTEventRecordPtr; userRoutine: ActionsUPP): OSErr; external name '_InvokeActionsUPP'; { old name was CallActionsProc }
{
 *  InvokeDoMCActionUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 }
function InvokeDoMCActionUPP(refcon: UnivPtr; action: SInt16; params: UnivPtr; var handled: boolean; userRoutine: DoMCActionUPP): OSErr; external name '_InvokeDoMCActionUPP'; { old name was CallDoMCActionProc }
{
 *  InvokeMovieExecuteWiredActionsUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 }
function InvokeMovieExecuteWiredActionsUPP(theMovie: Movie; refcon: UnivPtr; flags: SInt32; wiredActions: QTAtomContainer; userRoutine: MovieExecuteWiredActionsUPP): OSErr; external name '_InvokeMovieExecuteWiredActionsUPP'; { old name was CallMovieExecuteWiredActionsProc }
{
 *  InvokeMoviePrePrerollCompleteUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure InvokeMoviePrePrerollCompleteUPP(theMovie: Movie; prerollErr: OSErr; refcon: UnivPtr; userRoutine: MoviePrePrerollCompleteUPP); external name '_InvokeMoviePrePrerollCompleteUPP'; { old name was CallMoviePrePrerollCompleteProc }
{
 *  InvokeQTNextTaskNeededSoonerCallbackUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.2 and later
 }
procedure InvokeQTNextTaskNeededSoonerCallbackUPP(duration: TimeValue; flags: UInt32; refcon: UnivPtr; userRoutine: QTNextTaskNeededSoonerCallbackUPP); external name '_InvokeQTNextTaskNeededSoonerCallbackUPP'; { old name was CallQTNextTaskNeededSoonerCallbackProc }
{
 *  InvokeMoviesErrorUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure InvokeMoviesErrorUPP(theErr: OSErr; refcon: SInt32; userRoutine: MoviesErrorUPP); external name '_InvokeMoviesErrorUPP'; { old name was CallMoviesErrorProc }
{
 *  InvokeTweenerDataUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function InvokeTweenerDataUPP(tr: TweenRecordPtr; tweenData: UnivPtr; tweenDataSize: SInt32; dataDescriptionSeed: SInt32; dataDescription: Handle; asyncCompletionProc: ICMCompletionProcRecordPtr; transferProc: UniversalProcPtr; refCon: UnivPtr; userRoutine: TweenerDataUPP): ComponentResult; external name '_InvokeTweenerDataUPP'; { old name was CallTweenerDataProc }
{
 *  InvokeQTEffectListFilterUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.2 and later
 }
function InvokeQTEffectListFilterUPP(effect: Component; effectMinSource: SInt32; effectMaxSource: SInt32; majorClass: OSType; minorClass: OSType; refcon: UnivPtr; userRoutine: QTEffectListFilterUPP): boolean; external name '_InvokeQTEffectListFilterUPP'; { old name was CallQTEffectListFilterProc }
{
 *  InvokeQTBandwidthNotificationUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 }
function InvokeQTBandwidthNotificationUPP(flags: SInt32; reserved: UnivPtr; refcon: UnivPtr; userRoutine: QTBandwidthNotificationUPP): OSErr; external name '_InvokeQTBandwidthNotificationUPP'; { old name was CallQTBandwidthNotificationProc }
{****
    QT International Text Atom Support
****}

const
	kITextRemoveEverythingBut	= $00;
	kITextRemoveLeaveSuggestedAlternate = $02;

	kITextAtomType				= $69747874 (* 'itxt' *);
	kITextStringAtomType		= $74657874 (* 'text' *);

	{
	 *  ITextAddString()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 *    Windows:          in qtmlClient.lib 3.0 and later
	 	}
function ITextAddString(container: QTAtomContainer; parentAtom: QTAtom; theRegionCode: RegionCode; const (*var*) theString: Str255): OSErr; external name '_ITextAddString';
{
 *  ITextRemoveString()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ITextRemoveString(container: QTAtomContainer; parentAtom: QTAtom; theRegionCode: RegionCode; flags: SInt32): OSErr; external name '_ITextRemoveString';
{
 *  ITextGetString()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ITextGetString(container: QTAtomContainer; parentAtom: QTAtom; requestedRegion: RegionCode; var foundRegion: RegionCode; theString: StringPtr): OSErr; external name '_ITextGetString';
{
 *  QTTextToNativeText()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function QTTextToNativeText(theText: Handle; encoding: SInt32; flags: SInt32): OSErr; external name '_QTTextToNativeText';
{  QTParseTextHREF inContainer atoms }

const
	kQTParseTextHREFText		= $74657874 (* 'text' *);						{  string }
	kQTParseTextHREFBaseURL		= $6275726C (* 'burl' *);						{  string }
	kQTParseTextHREFClickPoint	= $636C696B (* 'clik' *);						{  Point; if present, QTParseTextHREF will expand URLs to support server-side image maps }
	kQTParseTextHREFUseAltDelim	= $616C7464 (* 'altd' *);						{  boolean; if no kQTParseTextHREFDelimiter, delim is ':' }
	kQTParseTextHREFDelimiter	= $64656C6D (* 'delm' *);						{  character }
	kQTParseTextHREFRecomposeHREF = $72687266 (* 'rhrf' *);						{  Boolean; if true, QTParseTextHREF returns recomposed HREF with URL expanded as appropriate }

	{  QTParseTextHREF outContainer atoms }
	kQTParseTextHREFURL			= $75726C20 (* 'url ' *);						{  string }
	kQTParseTextHREFTarget		= $74617267 (* 'targ' *);						{  string }
	kQTParseTextHREFChapter		= $63686170 (* 'chap' *);						{  string }
	kQTParseTextHREFIsAutoHREF	= $6175746F (* 'auto' *);						{  Boolean }
	kQTParseTextHREFIsServerMap	= $736D6170 (* 'smap' *);						{  Boolean }
	kQTParseTextHREFHREF		= $68726566 (* 'href' *);						{  string; recomposed HREF with URL expanded as appropriate, suitable for mcActionLinkToURL }
	kQTParseTextHREFEMBEDArgs	= $6D626564 (* 'mbed' *);						{  string; text between 'E<' and '>' to be used as new movie's embed tags }

	{
	 *  QTParseTextHREF()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in QuickTimeLib 4.1 and later
	 *    CarbonLib:        in CarbonLib 1.1 and later
	 *    Mac OS X:         in version 10.0 and later
	 *    Windows:          in qtmlClient.lib 4.1 and later
	 	}
function QTParseTextHREF(href: CStringPtr; hrefLen: SInt32; inContainer: QTAtomContainer; var outContainer: QTAtomContainer): OSErr; external name '_QTParseTextHREF';
{************************
* track reference types
*************************}

const
	kTrackReferenceChapterList	= $63686170 (* 'chap' *);
	kTrackReferenceTimeCode		= $746D6364 (* 'tmcd' *);
	kTrackReferenceModifier		= $73737263 (* 'ssrc' *);

	{	************************
	* modifier track types
	*************************	}
	kTrackModifierInput			= $696E;						{  is really 'in' }
	kTrackModifierType			= $7479;						{  is really 'ty' }
	kTrackModifierReference		= $73737263 (* 'ssrc' *);
	kTrackModifierObjectID		= $6F626964 (* 'obid' *);
	kTrackModifierInputName		= $6E616D65 (* 'name' *);

	kInputMapSubInputID			= $73756269 (* 'subi' *);

	kTrackModifierTypeMatrix	= 1;
	kTrackModifierTypeClip		= 2;
	kTrackModifierTypeGraphicsMode = 5;
	kTrackModifierTypeVolume	= 3;
	kTrackModifierTypeBalance	= 4;
	kTrackModifierTypeImage		= $76696465 (* 'vide' *);						{  was kTrackModifierTypeSpriteImage }
	kTrackModifierObjectMatrix	= 6;
	kTrackModifierObjectGraphicsMode = 7;
	kTrackModifierType3d4x4Matrix = 8;
	kTrackModifierCameraData	= 9;
	kTrackModifierSoundLocalizationData = 10;
	kTrackModifierObjectImageIndex = 11;
	kTrackModifierObjectLayer	= 12;
	kTrackModifierObjectVisible	= 13;
	kTrackModifierAngleAspectCamera = 14;
	kTrackModifierPanAngle		= $70616E20 (* 'pan ' *);
	kTrackModifierTiltAngle		= $74696C74 (* 'tilt' *);
	kTrackModifierVerticalFieldOfViewAngle = $666F7620 (* 'fov ' *);
	kTrackModifierObjectQTEventSend = $65766E74 (* 'evnt' *);
	kTrackModifierObjectCanBeHitTested = 15;


type
	ModifierTrackGraphicsModeRecordPtr = ^ModifierTrackGraphicsModeRecord;
	ModifierTrackGraphicsModeRecord = record
		graphicsMode:			SInt32;
		opColor:				RGBColor;
	end;


	{	************************
	* tween track types
	*************************	}

const
	kTweenTypeShort				= 1;
	kTweenTypeLong				= 2;
	kTweenTypeFixed				= 3;
	kTweenTypePoint				= 4;
	kTweenTypeQDRect			= 5;
	kTweenTypeQDRegion			= 6;
	kTweenTypeMatrix			= 7;
	kTweenTypeRGBColor			= 8;
	kTweenTypeGraphicsModeWithRGBColor = 9;
	kTweenTypeQTFloatSingle		= 10;
	kTweenTypeQTFloatDouble		= 11;
	kTweenTypeFixedPoint		= 12;
	kTweenType3dScale			= $33736361 (* '3sca' *);
	kTweenType3dTranslate		= $33747261 (* '3tra' *);
	kTweenType3dRotate			= $33726F74 (* '3rot' *);
	kTweenType3dRotateAboutPoint = $33726170 (* '3rap' *);
	kTweenType3dRotateAboutAxis	= $33726178 (* '3rax' *);
	kTweenType3dRotateAboutVector = $33727663 (* '3rvc' *);
	kTweenType3dQuaternion		= $33717561 (* '3qua' *);
	kTweenType3dMatrix			= $336D6174 (* '3mat' *);
	kTweenType3dCameraData		= $3363616D (* '3cam' *);
	kTweenType3dAngleAspectCameraData = $33636161 (* '3caa' *);
	kTweenType3dSoundLocalizationData = $33736C63 (* '3slc' *);
	kTweenTypePathToMatrixTranslation = $67786D74 (* 'gxmt' *);
	kTweenTypePathToMatrixRotation = $67787072 (* 'gxpr' *);
	kTweenTypePathToMatrixTranslationAndRotation = $67786D72 (* 'gxmr' *);
	kTweenTypePathToFixedPoint	= $67786670 (* 'gxfp' *);
	kTweenTypePathXtoY			= $67787879 (* 'gxxy' *);
	kTweenTypePathYtoX			= $67787978 (* 'gxyx' *);
	kTweenTypeAtomList			= $61746F6D (* 'atom' *);
	kTweenTypePolygon			= $706F6C79 (* 'poly' *);
	kTweenTypeMultiMatrix		= $6D756C6D (* 'mulm' *);
	kTweenTypeSpin				= $7370696E (* 'spin' *);
	kTweenType3dMatrixNonLinear	= $336E6C72 (* '3nlr' *);
	kTweenType3dVRObject		= $3376726F (* '3vro' *);

	kTweenEntry					= $7477656E (* 'twen' *);
	kTweenData					= $64617461 (* 'data' *);
	kTweenType					= $74776E74 (* 'twnt' *);
	kTweenStartOffset			= $74777374 (* 'twst' *);
	kTweenDuration				= $74776475 (* 'twdu' *);
	kTweenFlags					= $666C6167 (* 'flag' *);
	kTweenOutputMin				= $6F6D696E (* 'omin' *);
	kTweenOutputMax				= $6F6D6178 (* 'omax' *);
	kTweenSequenceElement		= $73657165 (* 'seqe' *);
	kTween3dInitialCondition	= $69636E64 (* 'icnd' *);
	kTweenInterpolationID		= $696E7472 (* 'intr' *);
	kTweenRegionData			= $71647267 (* 'qdrg' *);
	kTweenPictureData			= $50494354 (* 'PICT' *);
	kListElementType			= $74797065 (* 'type' *);
	kListElementDataType		= $64617479 (* 'daty' *);
	kNameAtom					= $6E616D65 (* 'name' *);
	kInitialRotationAtom		= $696E726F (* 'inro' *);
	kNonLinearTweenHeader		= $6E6C7468 (* 'nlth' *);

	{  kTweenFlags }
	kTweenReturnDelta			= $00000001;


type
	TweenSequenceEntryRecordPtr = ^TweenSequenceEntryRecord;
	TweenSequenceEntryRecord = record
		endPercent:				Fixed;
		tweenAtomID:			QTAtomID;
		dataAtomID:				QTAtomID;
	end;


	{	****
	    Content Restrictions
	****	}

const
	kQTRestrictionClassSave		= $73617665 (* 'save' *);
	kQTRestrictionSaveDontAddMovieResource = $00000001;
	kQTRestrictionSaveDontFlatten = $00000002;
	kQTRestrictionSaveDontExport = $00000004;
	kQTRestrictionSaveDontExtract = $00000008;					{  don't allow any form of extraction of content }
	kQTRestrictionClassEdit		= $65646974 (* 'edit' *);
	kQTRestrictionEditDontCopy	= $00000001;					{  disable copy  }
	kQTRestrictionEditDontCut	= $00000002;					{  disable cut  }
	kQTRestrictionEditDontPaste	= $00000004;					{  disable paste  }
	kQTRestrictionEditDontClear	= $00000008;					{  disable clear }
	kQTRestrictionEditDontModify = $00000010;					{  don't allow modification of content }
	kQTRestrictionEditDontExtract = $00000020;					{  don't allow any form of extraction of content }


	{	************************
	* Video Media routines
	*************************	}


	videoFlagDontLeanAhead		= $00000001;


	{  use these five routines at your own peril }
	{
	 *  VideoMediaResetStatistics()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 *    Windows:          in qtmlClient.lib 3.0 and later
	 	}
function VideoMediaResetStatistics(mh: MediaHandler): ComponentResult; external name '_VideoMediaResetStatistics';
{
 *  VideoMediaGetStatistics()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function VideoMediaGetStatistics(mh: MediaHandler): ComponentResult; external name '_VideoMediaGetStatistics';
{
 *  VideoMediaGetStallCount()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function VideoMediaGetStallCount(mh: MediaHandler; var stalls: UInt32): ComponentResult; external name '_VideoMediaGetStallCount';
{
 *  VideoMediaSetCodecParameter()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function VideoMediaSetCodecParameter(mh: MediaHandler; cType: CodecType; parameterID: OSType; parameterChangeSeed: SInt32; dataPtr: UnivPtr; dataSize: SInt32): ComponentResult; external name '_VideoMediaSetCodecParameter';
{
 *  VideoMediaGetCodecParameter()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function VideoMediaGetCodecParameter(mh: MediaHandler; cType: CodecType; parameterID: OSType; outParameterData: Handle): ComponentResult; external name '_VideoMediaGetCodecParameter';
{************************
* Text Media routines
*************************}


{ Return displayFlags for TextProc }

const
	txtProcDefaultDisplay		= 0;							{     Use the media's default }
	txtProcDontDisplay			= 1;							{     Don't display the text }
	txtProcDoDisplay			= 2;							{     Do display the text }

	{
	 *  TextMediaSetTextProc()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 *    Windows:          in qtmlClient.lib 3.0 and later
	 	}
function TextMediaSetTextProc(mh: MediaHandler; TextProc: TextMediaUPP; refcon: SInt32): ComponentResult; external name '_TextMediaSetTextProc';
{
 *  TextMediaAddTextSample()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function TextMediaAddTextSample(mh: MediaHandler; text: Ptr; size: UInt32; fontNumber: SInt16; fontSize: SInt16; txtFace: ByteParameter; var textColor: RGBColor; var backColor: RGBColor; textJustification: SInt16; var textBox: Rect; displayFlags: SInt32; scrollDelay: TimeValue; hiliteStart: SInt16; hiliteEnd: SInt16; var rgbHiliteColor: RGBColor; duration: TimeValue; var sampleTime: TimeValue): ComponentResult; external name '_TextMediaAddTextSample';
{
 *  TextMediaAddTESample()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function TextMediaAddTESample(mh: MediaHandler; hTE: TEHandle; var backColor: RGBColor; textJustification: SInt16; var textBox: Rect; displayFlags: SInt32; scrollDelay: TimeValue; hiliteStart: SInt16; hiliteEnd: SInt16; var rgbHiliteColor: RGBColor; duration: TimeValue; var sampleTime: TimeValue): ComponentResult; external name '_TextMediaAddTESample';
{
 *  TextMediaAddHiliteSample()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function TextMediaAddHiliteSample(mh: MediaHandler; hiliteStart: SInt16; hiliteEnd: SInt16; var rgbHiliteColor: RGBColor; duration: TimeValue; var sampleTime: TimeValue): ComponentResult; external name '_TextMediaAddHiliteSample';
{
 *  TextMediaDrawRaw()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function TextMediaDrawRaw(mh: MediaHandler; gw: GWorldPtr; gd: GDHandle; data: UnivPtr; dataSize: SInt32; tdh: TextDescriptionHandle): ComponentResult; external name '_TextMediaDrawRaw';
{
 *  TextMediaSetTextProperty()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function TextMediaSetTextProperty(mh: MediaHandler; atMediaTime: TimeValue; propertyType: SInt32; data: UnivPtr; dataSize: SInt32): ComponentResult; external name '_TextMediaSetTextProperty';
{
 *  TextMediaRawSetup()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function TextMediaRawSetup(mh: MediaHandler; gw: GWorldPtr; gd: GDHandle; data: UnivPtr; dataSize: SInt32; tdh: TextDescriptionHandle; sampleDuration: TimeValue): ComponentResult; external name '_TextMediaRawSetup';
{
 *  TextMediaRawIdle()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function TextMediaRawIdle(mh: MediaHandler; gw: GWorldPtr; gd: GDHandle; sampleTime: TimeValue; flagsIn: SInt32; var flagsOut: SInt32): ComponentResult; external name '_TextMediaRawIdle';
{
 *  TextMediaGetTextProperty()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 5.0 and later
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 5.0 and later
 }
function TextMediaGetTextProperty(mh: MediaHandler; atMediaTime: TimeValue; propertyType: SInt32; data: UnivPtr; dataSize: SInt32): ComponentResult; external name '_TextMediaGetTextProperty';
const
	findTextEdgeOK				= $01;							{  Okay to find text at specified sample time }
	findTextCaseSensitive		= $02;							{  Case sensitive search }
	findTextReverseSearch		= $04;							{  Search from sampleTime backwards }
	findTextWrapAround			= $08;							{  Wrap search when beginning or end of movie is hit }
	findTextUseOffset			= $10;							{  Begin search at the given character offset into sample rather than edge }

	{
	 *  TextMediaFindNextText()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 *    Windows:          in qtmlClient.lib 3.0 and later
	 	}
function TextMediaFindNextText(mh: MediaHandler; text: Ptr; size: SInt32; findFlags: SInt16; startTime: TimeValue; var foundTime: TimeValue; var foundDuration: TimeValue; var offset: SInt32): ComponentResult; external name '_TextMediaFindNextText';
{
 *  TextMediaHiliteTextSample()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function TextMediaHiliteTextSample(mh: MediaHandler; sampleTime: TimeValue; hiliteStart: SInt16; hiliteEnd: SInt16; var rgbHiliteColor: RGBColor): ComponentResult; external name '_TextMediaHiliteTextSample';
const
	dropShadowOffsetType		= $6472706F (* 'drpo' *);
	dropShadowTranslucencyType	= $64727074 (* 'drpt' *);

	{
	 *  TextMediaSetTextSampleData()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 *    Windows:          in qtmlClient.lib 3.0 and later
	 	}
function TextMediaSetTextSampleData(mh: MediaHandler; data: UnivPtr; dataType: OSType): ComponentResult; external name '_TextMediaSetTextSampleData';
{************************
* Sprite Media routines
*************************}
{ flags for sprite hit test routines }

const
	spriteHitTestBounds			= $00000001;					{     point must only be within sprite's bounding box }
	spriteHitTestImage			= $00000002;					{   point must be within the shape of the sprite's image }
	spriteHitTestInvisibleSprites = $00000004;					{   invisible sprites may be hit tested }
	spriteHitTestIsClick		= $00000008;					{   for codecs that want mouse events }
	spriteHitTestLocInDisplayCoordinates = $00000010;			{     set if you want to pass a display coordiate point to SpriteHitTest }
	spriteHitTestTreatAllSpritesAsHitTestable = $00000020;		{  set if you want to override each sprites hittestable property as true }

	{	 atom types for sprite media 	}
	kSpriteAtomType				= $73707274 (* 'sprt' *);
	kSpriteImagesContainerAtomType = $696D6374 (* 'imct' *);
	kSpriteImageAtomType		= $696D6167 (* 'imag' *);
	kSpriteImageDataAtomType	= $696D6461 (* 'imda' *);
	kSpriteImageDataRefAtomType	= $696D7265 (* 'imre' *);
	kSpriteImageDataRefTypeAtomType = $696D7274 (* 'imrt' *);
	kSpriteImageGroupIDAtomType	= $696D6772 (* 'imgr' *);
	kSpriteImageRegistrationAtomType = $696D7267 (* 'imrg' *);
	kSpriteImageDefaultImageIndexAtomType = $64656669 (* 'defi' *);
	kSpriteSharedDataAtomType	= $64666C74 (* 'dflt' *);
	kSpriteNameAtomType			= $6E616D65 (* 'name' *);
	kSpriteImageNameAtomType	= $6E616D65 (* 'name' *);
	kSpriteUsesImageIDsAtomType	= $75736573 (* 'uses' *);						{  leaf data is an array of QTAtomID's, one per image used }
	kSpriteBehaviorsAtomType	= $62656861 (* 'beha' *);
	kSpriteImageBehaviorAtomType = $696D6167 (* 'imag' *);
	kSpriteCursorBehaviorAtomType = $63727372 (* 'crsr' *);
	kSpriteStatusStringsBehaviorAtomType = $73737472 (* 'sstr' *);
	kSpriteVariablesContainerAtomType = $76617273 (* 'vars' *);
	kSpriteStringVariableAtomType = $73747276 (* 'strv' *);
	kSpriteFloatingPointVariableAtomType = $666C6F76 (* 'flov' *);


type
	QTRuntimeSpriteDescStructPtr = ^QTRuntimeSpriteDescStruct;
	QTRuntimeSpriteDescStruct = record
		version:				SInt32;								{  set to zero }
		spriteID:				QTAtomID;
		imageIndex:				SInt16;
		matrix:					MatrixRecord;
		visible:				SInt16;
		layer:					SInt16;
		graphicsMode:			ModifierTrackGraphicsModeRecord;
		actionHandlingSpriteID:	QTAtomID;
	end;

	QTRuntimeSpriteDescPtr				= ^QTRuntimeSpriteDescStruct;
	{
	   when filling in QTSpriteButtonBehaviorStruct values -1 may be used to indicate that
	   the state transition does not change the property
	}
	QTSpriteButtonBehaviorStructPtr = ^QTSpriteButtonBehaviorStruct;
	QTSpriteButtonBehaviorStruct = record
		notOverNotPressedStateID: QTAtomID;
		overNotPressedStateID:	QTAtomID;
		overPressedStateID:		QTAtomID;
		notOverPressedStateID:	QTAtomID;
	end;

	QTSpriteButtonBehaviorPtr			= ^QTSpriteButtonBehaviorStruct;
	{
	 *  SpriteMediaSetProperty()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 *    Windows:          in qtmlClient.lib 3.0 and later
	 	}
function SpriteMediaSetProperty(mh: MediaHandler; spriteIndex: SInt16; propertyType: SInt32; propertyValue: UnivPtr): ComponentResult; external name '_SpriteMediaSetProperty';
{
 *  SpriteMediaGetProperty()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SpriteMediaGetProperty(mh: MediaHandler; spriteIndex: SInt16; propertyType: SInt32; propertyValue: UnivPtr): ComponentResult; external name '_SpriteMediaGetProperty';
{
 *  SpriteMediaHitTestSprites()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SpriteMediaHitTestSprites(mh: MediaHandler; flags: SInt32; loc: Point; var spriteHitIndex: SInt16): ComponentResult; external name '_SpriteMediaHitTestSprites';
{
 *  SpriteMediaCountSprites()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SpriteMediaCountSprites(mh: MediaHandler; var numSprites: SInt16): ComponentResult; external name '_SpriteMediaCountSprites';
{
 *  SpriteMediaCountImages()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SpriteMediaCountImages(mh: MediaHandler; var numImages: SInt16): ComponentResult; external name '_SpriteMediaCountImages';
{
 *  SpriteMediaGetIndImageDescription()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SpriteMediaGetIndImageDescription(mh: MediaHandler; imageIndex: SInt16; imageDescription: ImageDescriptionHandle): ComponentResult; external name '_SpriteMediaGetIndImageDescription';
{
 *  SpriteMediaGetDisplayedSampleNumber()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SpriteMediaGetDisplayedSampleNumber(mh: MediaHandler; var sampleNum: SInt32): ComponentResult; external name '_SpriteMediaGetDisplayedSampleNumber';
{
 *  SpriteMediaGetSpriteName()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SpriteMediaGetSpriteName(mh: MediaHandler; spriteID: QTAtomID; var spriteName: Str255): ComponentResult; external name '_SpriteMediaGetSpriteName';
{
 *  SpriteMediaGetImageName()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SpriteMediaGetImageName(mh: MediaHandler; imageIndex: SInt16; var imageName: Str255): ComponentResult; external name '_SpriteMediaGetImageName';
{
 *  SpriteMediaSetSpriteProperty()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SpriteMediaSetSpriteProperty(mh: MediaHandler; spriteID: QTAtomID; propertyType: SInt32; propertyValue: UnivPtr): ComponentResult; external name '_SpriteMediaSetSpriteProperty';
{
 *  SpriteMediaGetSpriteProperty()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SpriteMediaGetSpriteProperty(mh: MediaHandler; spriteID: QTAtomID; propertyType: SInt32; propertyValue: UnivPtr): ComponentResult; external name '_SpriteMediaGetSpriteProperty';
{
 *  SpriteMediaHitTestAllSprites()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SpriteMediaHitTestAllSprites(mh: MediaHandler; flags: SInt32; loc: Point; var spriteHitID: QTAtomID): ComponentResult; external name '_SpriteMediaHitTestAllSprites';
{
 *  SpriteMediaHitTestOneSprite()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SpriteMediaHitTestOneSprite(mh: MediaHandler; spriteID: QTAtomID; flags: SInt32; loc: Point; var wasHit: boolean): ComponentResult; external name '_SpriteMediaHitTestOneSprite';
{
 *  SpriteMediaSpriteIndexToID()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SpriteMediaSpriteIndexToID(mh: MediaHandler; spriteIndex: SInt16; var spriteID: QTAtomID): ComponentResult; external name '_SpriteMediaSpriteIndexToID';
{
 *  SpriteMediaSpriteIDToIndex()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SpriteMediaSpriteIDToIndex(mh: MediaHandler; spriteID: QTAtomID; var spriteIndex: SInt16): ComponentResult; external name '_SpriteMediaSpriteIDToIndex';
{
 *  SpriteMediaGetSpriteActionsForQTEvent()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SpriteMediaGetSpriteActionsForQTEvent(mh: MediaHandler; event: QTEventRecordPtr; spriteID: QTAtomID; var container: QTAtomContainer; var atom: QTAtom): ComponentResult; external name '_SpriteMediaGetSpriteActionsForQTEvent';
{
 *  SpriteMediaSetActionVariable()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SpriteMediaSetActionVariable(mh: MediaHandler; variableID: QTAtomID; value: Float32Ptr): ComponentResult; external name '_SpriteMediaSetActionVariable';
{
 *  SpriteMediaGetActionVariable()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SpriteMediaGetActionVariable(mh: MediaHandler; variableID: QTAtomID; var value: Float32): ComponentResult; external name '_SpriteMediaGetActionVariable';
{
 *  SpriteMediaGetIndImageProperty()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SpriteMediaGetIndImageProperty(mh: MediaHandler; imageIndex: SInt16; imagePropertyType: SInt32; imagePropertyValue: UnivPtr): ComponentResult; external name '_SpriteMediaGetIndImageProperty';
{
 *  SpriteMediaNewSprite()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function SpriteMediaNewSprite(mh: MediaHandler; newSpriteDesc: QTRuntimeSpriteDescPtr): ComponentResult; external name '_SpriteMediaNewSprite';
{
 *  SpriteMediaDisposeSprite()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function SpriteMediaDisposeSprite(mh: MediaHandler; spriteID: QTAtomID): ComponentResult; external name '_SpriteMediaDisposeSprite';
{
 *  SpriteMediaSetActionVariableToString()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function SpriteMediaSetActionVariableToString(mh: MediaHandler; variableID: QTAtomID; theCString: Ptr): ComponentResult; external name '_SpriteMediaSetActionVariableToString';
{
 *  SpriteMediaGetActionVariableAsString()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function SpriteMediaGetActionVariableAsString(mh: MediaHandler; variableID: QTAtomID; var theCString: Handle): ComponentResult; external name '_SpriteMediaGetActionVariableAsString';
{
 *  SpriteMediaNewImage()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 6.0 and later
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.2 and later
 *    Windows:          in qtmlClient.lib 6.0 and later
 }
function SpriteMediaNewImage(mh: MediaHandler; dataRef: Handle; dataRefType: OSType; desiredID: QTAtomID): ComponentResult; external name '_SpriteMediaNewImage';
{
 *  SpriteMediaDisposeImage()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 6.0 and later
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.2 and later
 *    Windows:          in qtmlClient.lib 6.0 and later
 }
function SpriteMediaDisposeImage(mh: MediaHandler; imageIndex: SInt16): ComponentResult; external name '_SpriteMediaDisposeImage';
{
 *  SpriteMediaImageIndexToID()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 6.0 and later
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.2 and later
 *    Windows:          in qtmlClient.lib 6.0 and later
 }
function SpriteMediaImageIndexToID(mh: MediaHandler; imageIndex: SInt16; var imageID: QTAtomID): ComponentResult; external name '_SpriteMediaImageIndexToID';
{
 *  SpriteMediaImageIDToIndex()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 6.0 and later
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.2 and later
 *    Windows:          in qtmlClient.lib 6.0 and later
 }
function SpriteMediaImageIDToIndex(mh: MediaHandler; imageID: QTAtomID; var imageIndex: SInt16): ComponentResult; external name '_SpriteMediaImageIDToIndex';
{************************
* Flash Media routines
*************************}

{
 *  FlashMediaSetPan()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function FlashMediaSetPan(mh: MediaHandler; xPercent: SInt16; yPercent: SInt16): ComponentResult; external name '_FlashMediaSetPan';
{
 *  FlashMediaSetZoom()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function FlashMediaSetZoom(mh: MediaHandler; factor: SInt16): ComponentResult; external name '_FlashMediaSetZoom';
{
 *  FlashMediaSetZoomRect()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function FlashMediaSetZoomRect(mh: MediaHandler; left: SInt32; top: SInt32; right: SInt32; bottom: SInt32): ComponentResult; external name '_FlashMediaSetZoomRect';
{
 *  FlashMediaGetRefConBounds()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function FlashMediaGetRefConBounds(mh: MediaHandler; refCon: SInt32; var left: SInt32; var top: SInt32; var right: SInt32; var bottom: SInt32): ComponentResult; external name '_FlashMediaGetRefConBounds';
{
 *  FlashMediaGetRefConID()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function FlashMediaGetRefConID(mh: MediaHandler; refCon: SInt32; var refConID: SInt32): ComponentResult; external name '_FlashMediaGetRefConID';
{
 *  FlashMediaIDToRefCon()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function FlashMediaIDToRefCon(mh: MediaHandler; refConID: SInt32; var refCon: SInt32): ComponentResult; external name '_FlashMediaIDToRefCon';
{
 *  FlashMediaGetDisplayedFrameNumber()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function FlashMediaGetDisplayedFrameNumber(mh: MediaHandler; var flashFrameNumber: SInt32): ComponentResult; external name '_FlashMediaGetDisplayedFrameNumber';
{
 *  FlashMediaFrameNumberToMovieTime()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function FlashMediaFrameNumberToMovieTime(mh: MediaHandler; flashFrameNumber: SInt32; var movieTime: TimeValue): ComponentResult; external name '_FlashMediaFrameNumberToMovieTime';
{
 *  FlashMediaFrameLabelToMovieTime()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function FlashMediaFrameLabelToMovieTime(mh: MediaHandler; theLabel: Ptr; var movieTime: TimeValue): ComponentResult; external name '_FlashMediaFrameLabelToMovieTime';
{
 *  FlashMediaGetFlashVariable()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 5.0 and later
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 5.0 and later
 }
function FlashMediaGetFlashVariable(mh: MediaHandler; path: CStringPtr; name: CStringPtr; var theVariableCStringOut: Handle): ComponentResult; external name '_FlashMediaGetFlashVariable';
{
 *  FlashMediaSetFlashVariable()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 5.0 and later
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 5.0 and later
 }
function FlashMediaSetFlashVariable(mh: MediaHandler; path: CStringPtr; name: CStringPtr; value: CStringPtr; updateFocus: boolean): ComponentResult; external name '_FlashMediaSetFlashVariable';
{
 *  FlashMediaDoButtonActions()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 5.0 and later
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 5.0 and later
 }
function FlashMediaDoButtonActions(mh: MediaHandler; path: CStringPtr; buttonID: SInt32; transition: SInt32): ComponentResult; external name '_FlashMediaDoButtonActions';
{
 *  FlashMediaGetSupportedSwfVersion()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 5.0 and later
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 5.0 and later
 }
function FlashMediaGetSupportedSwfVersion(mh: MediaHandler; var swfVersion: UInt8): ComponentResult; external name '_FlashMediaGetSupportedSwfVersion';
{  sample format atoms }

const
	kMovieMediaDataReference	= $6D6D6472 (* 'mmdr' *);						{  data reference }
	kMovieMediaDefaultDataReferenceID = $64647269 (* 'ddri' *);					{  atom id }
	kMovieMediaSlaveTime		= $736C7469 (* 'slti' *);						{  boolean }
	kMovieMediaSlaveAudio		= $736C6175 (* 'slau' *);						{  boolean }
	kMovieMediaSlaveGraphicsMode = $736C6772 (* 'slgr' *);						{  boolean }
	kMovieMediaAutoPlay			= $706C6179 (* 'play' *);						{  boolean }
	kMovieMediaLoop				= $6C6F6F70 (* 'loop' *);						{  UInt8 (0=no loop, 1=loop, 2=palindrome loop) }
	kMovieMediaUseMIMEType		= $6D696D65 (* 'mime' *);						{  string indicating the MIME type to use for the dataref (usually not required) }
	kMovieMediaTitle			= $7469746C (* 'titl' *);						{  string of the media's title (tooltips) }
	kMovieMediaAltText			= $616C7474 (* 'altt' *);						{  string of alternate text if media isn't loaded }
	kMovieMediaClipBegin		= $636C7062 (* 'clpb' *);						{  MovieMediaTimeRecord of start time of embedded media }
	kMovieMediaClipDuration		= $636C7064 (* 'clpd' *);						{  MovieMediaTimeRecord of duration of embedded media }
	kMovieMediaRegionAtom		= $72656769 (* 'regi' *);						{  contains subatoms that describe layout }
	kMovieMediaSlaveTrackDuration = $736C7472 (* 'sltr' *);						{  Boolean indicating that media handler should adjust track and media based on actual embedded movie duration }
	kMovieMediaEnableFrameStepping = $656E6673 (* 'enfs' *);					{  boolean. if true stepping on external movie steps frames within embedded movie. }
	kMovieMediaBackgroundColor	= $626B636C (* 'bkcl' *);						{  RGBColor. }
	kMovieMediaPrerollTime		= $70726572 (* 'prer' *);						{  SInt32 indicating preroll time }

	{  fit types }
	kMovieMediaFitNone			= 0;
	kMovieMediaFitScroll		= $7363726F (* 'scro' *);
	kMovieMediaFitClipIfNecessary = $68696464 (* 'hidd' *);
	kMovieMediaFitFill			= $66696C6C (* 'fill' *);
	kMovieMediaFitMeet			= $6D656574 (* 'meet' *);
	kMovieMediaFitSlice			= $736C6963 (* 'slic' *);

	{  sub atoms for region atom }
	kMovieMediaSpatialAdjustment = $66697420 (* 'fit ' *);						{  OSType from kMovieMediaFit* }
	kMovieMediaRectangleAtom	= $72656374 (* 'rect' *);
	kMovieMediaTop				= $746F7020 (* 'top ' *);
	kMovieMediaLeft				= $6C656674 (* 'left' *);
	kMovieMediaWidth			= $77642020 (* 'wd  ' *);
	kMovieMediaHeight			= $68742020 (* 'ht  ' *);

	{  contained movie properties }
	kMoviePropertyDuration		= $64757261 (* 'dura' *);						{  TimeValue * }
	kMoviePropertyTimeScale		= $74696D73 (* 'tims' *);						{  TimeValue * }
	kMoviePropertyTime			= $74696D76 (* 'timv' *);						{  TimeValue * }
	kMoviePropertyNaturalBounds	= $6E617462 (* 'natb' *);						{  Rect * }
	kMoviePropertyMatrix		= $6D747278 (* 'mtrx' *);						{  Matrix * }
	kMoviePropertyTrackList		= $746C7374 (* 'tlst' *);						{  long *** }


	kTrackPropertyMediaType		= $6D747970 (* 'mtyp' *);						{  OSType }
	kTrackPropertyInstantiation	= $696E7374 (* 'inst' *);						{  MovieMediaInstantiationInfoRecord }


type
	MovieMediaTimeRecordPtr = ^MovieMediaTimeRecord;
	MovieMediaTimeRecord = record
		time:					wide;
		scale:					TimeScale;
	end;

	MovieMediaInstantiationInfoRecordPtr = ^MovieMediaInstantiationInfoRecord;
	MovieMediaInstantiationInfoRecord = record
		immediately:			boolean;
		pad:					boolean;
		bitRate:				SInt32;
	end;

	{	************************
	* Movie Media routines
	*************************	}


	{
	 *  MovieMediaGetChildDoMCActionCallback()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in QuickTimeLib 4.1 and later
	 *    CarbonLib:        in CarbonLib 1.1 and later
	 *    Mac OS X:         in version 10.0 and later
	 *    Windows:          in qtmlClient.lib 4.1 and later
	 	}
function MovieMediaGetChildDoMCActionCallback(mh: MediaHandler; var doMCActionCallbackProc: DoMCActionUPP; var refcon: SInt32): ComponentResult; external name '_MovieMediaGetChildDoMCActionCallback';
{
 *  MovieMediaGetDoMCActionCallback()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.1 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.1 and later
 }
function MovieMediaGetDoMCActionCallback(mh: MediaHandler; var doMCActionCallbackProc: DoMCActionUPP; var refcon: SInt32): ComponentResult; external name '_MovieMediaGetDoMCActionCallback';
{
 *  MovieMediaGetCurrentMovieProperty()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.1 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.1 and later
 }
function MovieMediaGetCurrentMovieProperty(mh: MediaHandler; whichProperty: OSType; value: UnivPtr): ComponentResult; external name '_MovieMediaGetCurrentMovieProperty';
{
 *  MovieMediaGetCurrentTrackProperty()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.1 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.1 and later
 }
function MovieMediaGetCurrentTrackProperty(mh: MediaHandler; trackID: SInt32; whichProperty: OSType; value: UnivPtr): ComponentResult; external name '_MovieMediaGetCurrentTrackProperty';
{
 *  MovieMediaGetChildMovieDataReference()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.1 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.1 and later
 }
function MovieMediaGetChildMovieDataReference(mh: MediaHandler; dataRefID: QTAtomID; dataRefIndex: SInt16; var dataRefType: OSType; var dataRef: Handle; var dataRefIDOut: QTAtomID; var dataRefIndexOut: SInt16): ComponentResult; external name '_MovieMediaGetChildMovieDataReference';
{
 *  MovieMediaSetChildMovieDataReference()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.1 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.1 and later
 }
function MovieMediaSetChildMovieDataReference(mh: MediaHandler; dataRefID: QTAtomID; dataRefType: OSType; dataRef: Handle): ComponentResult; external name '_MovieMediaSetChildMovieDataReference';
{
 *  MovieMediaLoadChildMovieFromDataReference()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.1 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.1 and later
 }
function MovieMediaLoadChildMovieFromDataReference(mh: MediaHandler; dataRefID: QTAtomID): ComponentResult; external name '_MovieMediaLoadChildMovieFromDataReference';
{************************
* 3D Media routines
*************************}
{
 *  Media3DGetNamedObjectList()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function Media3DGetNamedObjectList(mh: MediaHandler; var objectList: QTAtomContainer): ComponentResult; external name '_Media3DGetNamedObjectList';
{
 *  Media3DGetRendererList()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function Media3DGetRendererList(mh: MediaHandler; var rendererList: QTAtomContainer): ComponentResult; external name '_Media3DGetRendererList';
{
 *  Media3DGetCurrentGroup()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function Media3DGetCurrentGroup(mh: MediaHandler; group: UnivPtr): ComponentResult; external name '_Media3DGetCurrentGroup';
{
 *  Media3DTranslateNamedObjectTo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function Media3DTranslateNamedObjectTo(mh: MediaHandler; objectName: CStringPtr; x: Fixed; y: Fixed; z: Fixed): ComponentResult; external name '_Media3DTranslateNamedObjectTo';
{
 *  Media3DScaleNamedObjectTo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function Media3DScaleNamedObjectTo(mh: MediaHandler; objectName: CStringPtr; xScale: Fixed; yScale: Fixed; zScale: Fixed): ComponentResult; external name '_Media3DScaleNamedObjectTo';
{
 *  Media3DRotateNamedObjectTo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function Media3DRotateNamedObjectTo(mh: MediaHandler; objectName: CStringPtr; xDegrees: Fixed; yDegrees: Fixed; zDegrees: Fixed): ComponentResult; external name '_Media3DRotateNamedObjectTo';
{
 *  Media3DSetCameraData()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function Media3DSetCameraData(mh: MediaHandler; cameraData: UnivPtr): ComponentResult; external name '_Media3DSetCameraData';
{
 *  Media3DGetCameraData()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function Media3DGetCameraData(mh: MediaHandler; cameraData: UnivPtr): ComponentResult; external name '_Media3DGetCameraData';
{
 *  Media3DSetCameraAngleAspect()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function Media3DSetCameraAngleAspect(mh: MediaHandler; fov: QTFloatSingle; aspectRatioXToY: QTFloatSingle): ComponentResult; external name '_Media3DSetCameraAngleAspect';
{
 *  Media3DGetCameraAngleAspect()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function Media3DGetCameraAngleAspect(mh: MediaHandler; var fov: QTFloatSingle; var aspectRatioXToY: QTFloatSingle): ComponentResult; external name '_Media3DGetCameraAngleAspect';
{
 *  Media3DSetCameraRange()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function Media3DSetCameraRange(mh: MediaHandler; tQ3CameraRange: UnivPtr): ComponentResult; external name '_Media3DSetCameraRange';
{
 *  Media3DGetCameraRange()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function Media3DGetCameraRange(mh: MediaHandler; tQ3CameraRange: UnivPtr): ComponentResult; external name '_Media3DGetCameraRange';
{
 *  Media3DGetViewObject()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.1 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.1 and later
 }
function Media3DGetViewObject(mh: MediaHandler; tq3viewObject: UnivPtr): ComponentResult; external name '_Media3DGetViewObject';
{***************************************
*                                       *
*   M O V I E   C O N T R O L L E R     *
*                                       *
***************************************}

const
	MovieControllerComponentType = $706C6179 (* 'play' *);


	kMovieControllerQTVRFlag	= $01;
	kMovieControllerDontDisplayToUser = $02;


type
	MovieController						= ComponentInstance;
	MovieControllerPtr					= ^MovieController;

const
	mcActionIdle				= 1;							{  no param }
	mcActionDraw				= 2;							{  param is WindowRef }
	mcActionActivate			= 3;							{  no param }
	mcActionDeactivate			= 4;							{  no param }
	mcActionMouseDown			= 5;							{  param is pointer to EventRecord }
	mcActionKey					= 6;							{  param is pointer to EventRecord }
	mcActionPlay				= 8;							{  param is Fixed, play rate }
	mcActionGoToTime			= 12;							{  param is TimeRecord }
	mcActionSetVolume			= 14;							{  param is a short }
	mcActionGetVolume			= 15;							{  param is pointer to a short }
	mcActionStep				= 18;							{  param is number of steps (short) }
	mcActionSetLooping			= 21;							{  param is Boolean }
	mcActionGetLooping			= 22;							{  param is pointer to a Boolean }
	mcActionSetLoopIsPalindrome	= 23;							{  param is Boolean }
	mcActionGetLoopIsPalindrome	= 24;							{  param is pointer to a Boolean }
	mcActionSetGrowBoxBounds	= 25;							{  param is a Rect }
	mcActionControllerSizeChanged = 26;							{  no param }
	mcActionSetSelectionBegin	= 29;							{  param is TimeRecord }
	mcActionSetSelectionDuration = 30;							{  param is TimeRecord, action only taken on set-duration }
	mcActionSetKeysEnabled		= 32;							{  param is Boolean }
	mcActionGetKeysEnabled		= 33;							{  param is pointer to Boolean }
	mcActionSetPlaySelection	= 34;							{  param is Boolean }
	mcActionGetPlaySelection	= 35;							{  param is pointer to Boolean }
	mcActionSetUseBadge			= 36;							{  param is Boolean }
	mcActionGetUseBadge			= 37;							{  param is pointer to Boolean }
	mcActionSetFlags			= 38;							{  param is long of flags }
	mcActionGetFlags			= 39;							{  param is pointer to a long of flags }
	mcActionSetPlayEveryFrame	= 40;							{  param is Boolean }
	mcActionGetPlayEveryFrame	= 41;							{  param is pointer to Boolean }
	mcActionGetPlayRate			= 42;							{  param is pointer to Fixed }
	mcActionShowBalloon			= 43;							{  param is a pointer to a boolean. set to false to stop balloon }
	mcActionBadgeClick			= 44;							{  param is pointer to Boolean. set to false to ignore click }
	mcActionMovieClick			= 45;							{  param is pointer to event record. change "what" to nullEvt to kill click }
	mcActionSuspend				= 46;							{  no param }
	mcActionResume				= 47;							{  no param }
	mcActionSetControllerKeysEnabled = 48;						{  param is Boolean }
	mcActionGetTimeSliderRect	= 49;							{  param is pointer to rect }
	mcActionMovieEdited			= 50;							{  no param }
	mcActionGetDragEnabled		= 51;							{  param is pointer to Boolean }
	mcActionSetDragEnabled		= 52;							{  param is Boolean }
	mcActionGetSelectionBegin	= 53;							{  param is TimeRecord }
	mcActionGetSelectionDuration = 54;							{  param is TimeRecord }
	mcActionPrerollAndPlay		= 55;							{  param is Fixed, play rate }
	mcActionGetCursorSettingEnabled = 56;						{  param is pointer to Boolean }
	mcActionSetCursorSettingEnabled = 57;						{  param is Boolean }
	mcActionSetColorTable		= 58;							{  param is CTabHandle }
	mcActionLinkToURL			= 59;							{  param is Handle to URL }
	mcActionCustomButtonClick	= 60;							{  param is pointer to EventRecord }
	mcActionForceTimeTableUpdate = 61;							{  no param }
	mcActionSetControllerTimeLimits = 62;						{  param is pointer to 2 time values min/max. do no send this message to controller. used internally only. }
	mcActionExecuteAllActionsForQTEvent = 63;					{  param is ResolvedQTEventSpecPtr }
	mcActionExecuteOneActionForQTEvent = 64;					{  param is ResolvedQTEventSpecPtr }
	mcActionAdjustCursor		= 65;							{  param is pointer to EventRecord (WindowRef is in message parameter) }
	mcActionUseTrackForTimeTable = 66;							{  param is pointer to (long trackID; Boolean useIt). do not send this message to controller.  }
	mcActionClickAndHoldPoint	= 67;							{  param is point (local coordinates). return true if point has click & hold action (e.g., VR object movie autorotate spot) }
	mcActionShowMessageString	= 68;							{  param is a StringPtr }
	mcActionShowStatusString	= 69;							{  param is a QTStatusStringPtr }
	mcActionGetExternalMovie	= 70;							{  param is a QTGetExternalMoviePtr }
	mcActionGetChapterTime		= 71;							{  param is a QTGetChapterTimePtr }
	mcActionPerformActionList	= 72;							{  param is a QTAtomSpecPtr }
	mcActionEvaluateExpression	= 73;							{  param is a QTEvaluateExpressionPtr }
	mcActionFetchParameterAs	= 74;							{  param is a QTFetchParameterAsPtr }
	mcActionGetCursorByID		= 75;							{  param is a QTGetCursorByIDPtr }
	mcActionGetNextURL			= 76;							{  param is a Handle to URL }
	mcActionMovieChanged		= 77;
	mcActionDoScript			= 78;							{  param is QTDoScriptPtr }
	mcActionRestartAtTime		= 79;							{  param is QTResartAtTimePtr }
	mcActionGetIndChapter		= 80;							{  param is QTChapterInfoPtr }
	mcActionLinkToURLExtended	= 81;							{  param is QTAtomContainer as used by QTParseHREF }
	mcActionSetVolumeStep		= 82;							{  param is short containing amount to step volume via arrow keys - default = 64 }
	mcActionAutoPlay			= 83;							{  param is Fixed, play rate }
	mcActionPauseToBuffer		= 84;							{  param is Fixed, play rate on restart }
	mcActionAppMessageReceived	= 85;							{  param is a long, application message }
	mcActionEvaluateExpressionWithType = 89;					{  param is a QTEvaluateExpressionWithTypePtr }
	mcActionGetMovieName		= 90;							{  param is a p String Handle }
	mcActionGetMovieID			= 91;							{  param is pointer to long }
	mcActionGetMovieActive		= 92;							{  param is pointer to Boolean }


type
	mcAction							= SInt16;

const
	mcFlagSuppressMovieFrame	= $01;
	mcFlagSuppressStepButtons	= $02;
	mcFlagSuppressSpeakerButton	= $04;
	mcFlagsUseWindowPalette		= $08;
	mcFlagsDontInvalidate		= $10;
	mcFlagsUseCustomButton		= $20;


	mcPositionDontInvalidate	= $20;


type
	mcFlags								= UInt32;

const
	kMCIEEnabledButtonPicture	= 1;
	kMCIEDisabledButtonPicture	= 2;
	kMCIEDepressedButtonPicture	= 3;
	kMCIEEnabledSizeBoxPicture	= 4;
	kMCIEDisabledSizeBoxPicture	= 5;
	kMCIEEnabledUnavailableButtonPicture = 6;
	kMCIEDisabledUnavailableButtonPicture = 7;
	kMCIESoundSlider			= 128;
	kMCIESoundThumb				= 129;
	kMCIEColorTable				= 256;
	kMCIEIsFlatAppearance		= 257;
	kMCIEDoButtonIconsDropOnDepress = 258;


type
	MCInterfaceElement					= UInt32;
{$ifc TYPED_FUNCTION_POINTERS}
	MCActionFilterProcPtr = function(mc: MovieController; var action: SInt16; params: UnivPtr): boolean;
{$elsec}
	MCActionFilterProcPtr = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	MCActionFilterWithRefConProcPtr = function(mc: MovieController; action: SInt16; params: UnivPtr; refCon: SInt32): boolean;
{$elsec}
	MCActionFilterWithRefConProcPtr = ProcPtr;
{$endc}

{$ifc OPAQUE_UPP_TYPES}
	MCActionFilterUPP = ^SInt32; { an opaque UPP }
{$elsec}
	MCActionFilterUPP = UniversalProcPtr;
{$endc}	
{$ifc OPAQUE_UPP_TYPES}
	MCActionFilterWithRefConUPP = ^SInt32; { an opaque UPP }
{$elsec}
	MCActionFilterWithRefConUPP = UniversalProcPtr;
{$endc}	
	{	
	    menu related stuff
		}

const
	mcInfoUndoAvailable			= $01;
	mcInfoCutAvailable			= $02;
	mcInfoCopyAvailable			= $04;
	mcInfoPasteAvailable		= $08;
	mcInfoClearAvailable		= $10;
	mcInfoHasSound				= $20;
	mcInfoIsPlaying				= $40;
	mcInfoIsLooping				= $80;
	mcInfoIsInPalindrome		= $0100;
	mcInfoEditingEnabled		= $0200;
	mcInfoMovieIsInteractive	= $0400;

	{  menu item codes }
	mcMenuUndo					= 1;
	mcMenuCut					= 3;
	mcMenuCopy					= 4;
	mcMenuPaste					= 5;
	mcMenuClear					= 6;

	{  messages to the application via mcActionAppMessageReceived }
	kQTAppMessageSoftwareChanged = 1;							{  notification to app that installed QuickTime software has been updated }
	kQTAppMessageWindowCloseRequested = 3;						{  request for app to close window containing movie controller }
	kQTAppMessageExitFullScreenRequested = 4;					{  request for app to turn off full screen mode if active }
	kQTAppMessageDisplayChannels = 5;							{  request for app to display the channel UI }
	kQTAppMessageEnterFullScreenRequested = 6;					{  request for app to turn on full screen mode }

	{  structures used as mcActionFilterProc params }

type
	QTStatusStringRecordPtr = ^QTStatusStringRecord;
	QTStatusStringRecord = record
		stringTypeFlags:		SInt32;
		statusString:			CStringPtr;
	end;

	QTStatusStringPtr					= ^QTStatusStringRecord;
	QTGetExternalMovieRecordPtr = ^QTGetExternalMovieRecord;
	QTGetExternalMovieRecord = record
		targetType:				SInt32;								{  set to kTargetMovieName or kTargetMovieID }
		movieName:				StringPtr;
		movieID:				SInt32;
		theMovie:				MoviePtr;
		theController:			MovieControllerPtr;
	end;

	QTGetExternalMoviePtr				= ^QTGetExternalMovieRecord;
	QTGetChapterTimeRecordPtr = ^QTGetChapterTimeRecord;
	QTGetChapterTimeRecord = record
		chapterName:			StringPtr;
		chapterTime:			TimeRecord;
	end;

	QTGetChapterTimePtr					= ^QTGetChapterTimeRecord;
	QTChapterInfoRecordPtr = ^QTChapterInfoRecord;
	QTChapterInfoRecord = record
		index:					SInt32;								{  first chapter has index of 1 }
		time:					TimeValue;								{  -1 if no more chapters available }
		name:					Str255;
	end;

	QTChapterInfoPtr					= ^QTChapterInfoRecord;
	QTEvaluateExpressionRecordPtr = ^QTEvaluateExpressionRecord;
	QTEvaluateExpressionRecord = record
		expressionSpec:			QTAtomSpec;
		expressionResult:		Float32Ptr;
	end;

	QTEvaluateExpressionPtr				= ^QTEvaluateExpressionRecord;
	QTEvaluateExpressionWithTypeRecordPtr = ^QTEvaluateExpressionWithTypeRecord;
	QTEvaluateExpressionWithTypeRecord = record
		recordSize:				SInt32;								{  Size of structure (fill in at allocation)  }
		expressionSpec:			QTAtomSpec;
		expressionResult:		Float32Ptr;
		fetchAsType:			SInt32;
		nonNumericResult:		Handle;
																		{  Current size is 24  }
	end;

	QTEvaluateExpressionWithTypePtr		= ^QTEvaluateExpressionWithTypeRecord;
	QTFetchParameterAsRecordPtr = ^QTFetchParameterAsRecord;
	QTFetchParameterAsRecord = record
		paramListSpec:			QTAtomSpec;
		paramIndex:				SInt32;
		paramType:				SInt32;
		allowedFlags:			SInt32;
		min:					Ptr;
		max:					Ptr;
		currentValue:			Ptr;
		newValue:				Ptr;
		isUnsignedValue:		boolean;
	end;

	QTFetchParameterAsPtr				= ^QTFetchParameterAsRecord;
	QTGetCursorByIDRecordPtr = ^QTGetCursorByIDRecord;
	QTGetCursorByIDRecord = record
		cursorID:				SInt16;
		colorCursorData:		Handle;
		reserved1:				SInt32;
	end;

	QTGetCursorByIDPtr					= ^QTGetCursorByIDRecord;
	QTDoScriptRecordPtr = ^QTDoScriptRecord;
	QTDoScriptRecord = record
		scriptTypeFlags:		SInt32;
		command:				CStringPtr;
		arguments:				CStringPtr;
	end;

	QTDoScriptPtr						= ^QTDoScriptRecord;
	QTRestartAtTimeRecordPtr = ^QTRestartAtTimeRecord;
	QTRestartAtTimeRecord = record
		startTime:				TimeValue;								{  time scale is the movie timescale }
		rate:					Fixed;									{  if rate is zero, the movie's current rate is maintained }
	end;

	QTRestartAtTimePtr					= ^QTRestartAtTimeRecord;
	{  values for paramType field of QTFetchParameterAsRecord }

const
	kFetchAsBooleanPtr			= 1;
	kFetchAsShortPtr			= 2;
	kFetchAsLongPtr				= 3;
	kFetchAsMatrixRecordPtr		= 4;
	kFetchAsModifierTrackGraphicsModeRecord = 5;
	kFetchAsHandle				= 6;
	kFetchAsStr255				= 7;
	kFetchAsFloatPtr			= 8;
	kFetchAsPointPtr			= 9;
	kFetchAsNewAtomContainer	= 10;
	kFetchAsQTEventRecordPtr	= 11;
	kFetchAsFixedPtr			= 12;
	kFetchAsSetControllerValuePtr = 13;
	kFetchAsRgnHandle			= 14;							{  flipped to native }
	kFetchAsComponentDescriptionPtr = 15;
	kFetchAsCString				= 16;

	kQTCursorOpenHand			= -19183;
	kQTCursorClosedHand			= -19182;
	kQTCursorPointingHand		= -19181;
	kQTCursorRightArrow			= -19180;
	kQTCursorLeftArrow			= -19179;
	kQTCursorDownArrow			= -19178;
	kQTCursorUpArrow			= -19177;
	kQTCursorIBeam				= -19176;


	{	 target management 	}
	{
	 *  MCSetMovie()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 *    Windows:          in qtmlClient.lib 3.0 and later
	 	}
function MCSetMovie(mc: MovieController; theMovie: Movie; movieWindow: WindowRef; where: Point): ComponentResult; external name '_MCSetMovie';
{
 *  MCGetIndMovie()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MCGetIndMovie(mc: MovieController; index: SInt16): Movie; external name '_MCGetIndMovie';
{
 *  MCRemoveAllMovies()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MCRemoveAllMovies(mc: MovieController): ComponentResult; external name '_MCRemoveAllMovies';
{
 *  MCRemoveAMovie()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MCRemoveAMovie(mc: MovieController; m: Movie): ComponentResult; external name '_MCRemoveAMovie';
{
 *  MCRemoveMovie()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MCRemoveMovie(mc: MovieController): ComponentResult; external name '_MCRemoveMovie';
{ event handling etc. }
{
 *  MCIsPlayerEvent()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MCIsPlayerEvent(mc: MovieController; const (*var*) e: EventRecord): ComponentResult; external name '_MCIsPlayerEvent';
{ obsolete. use MCSetActionFilterWithRefCon instead. }
{
 *  MCSetActionFilter()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MCSetActionFilter(mc: MovieController; blob: MCActionFilterUPP): ComponentResult; external name '_MCSetActionFilter';
{
    proc is of the form:
        Boolean userPlayerFilter(MovieController mc, short *action, void *params) =
    proc returns TRUE if it handles the action, FALSE if not
    action is passed as a var so that it could be changed by filter
    this is consistent with the current dialog manager stuff
    params is any potential parameters that go with the action
        such as set playback rate to xxx.
}
{
 *  MCDoAction()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MCDoAction(mc: MovieController; action: SInt16; params: UnivPtr): ComponentResult; external name '_MCDoAction';
{ state type things }
{
 *  MCSetControllerAttached()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MCSetControllerAttached(mc: MovieController; attach: boolean): ComponentResult; external name '_MCSetControllerAttached';
{
 *  MCIsControllerAttached()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MCIsControllerAttached(mc: MovieController): ComponentResult; external name '_MCIsControllerAttached';
{
 *  MCSetControllerPort()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MCSetControllerPort(mc: MovieController; gp: CGrafPtr): ComponentResult; external name '_MCSetControllerPort';
{
 *  MCGetControllerPort()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MCGetControllerPort(mc: MovieController): CGrafPtr; external name '_MCGetControllerPort';
{
 *  MCSetVisible()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MCSetVisible(mc: MovieController; visible: boolean): ComponentResult; external name '_MCSetVisible';
{
 *  MCGetVisible()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MCGetVisible(mc: MovieController): ComponentResult; external name '_MCGetVisible';
{
 *  MCGetControllerBoundsRect()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MCGetControllerBoundsRect(mc: MovieController; var bounds: Rect): ComponentResult; external name '_MCGetControllerBoundsRect';
{
 *  MCSetControllerBoundsRect()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MCSetControllerBoundsRect(mc: MovieController; const (*var*) bounds: Rect): ComponentResult; external name '_MCSetControllerBoundsRect';
{
 *  MCGetControllerBoundsRgn()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MCGetControllerBoundsRgn(mc: MovieController): RgnHandle; external name '_MCGetControllerBoundsRgn';
{
 *  MCGetWindowRgn()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MCGetWindowRgn(mc: MovieController; w: WindowRef): RgnHandle; external name '_MCGetWindowRgn';
{ other stuff }
{
 *  MCMovieChanged()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MCMovieChanged(mc: MovieController; m: Movie): ComponentResult; external name '_MCMovieChanged';
{
    called when the app has changed thing about the movie (like bounding rect) or rate. So that we
        can update our graphical (and internal) state accordingly.
}
{
 *  MCSetDuration()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MCSetDuration(mc: MovieController; duration: TimeValue): ComponentResult; external name '_MCSetDuration';
{
    duration to use for time slider -- will be reset next time MCMovieChanged is called
        or MCSetMovie is called
}
{
 *  MCGetCurrentTime()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MCGetCurrentTime(mc: MovieController; var scale: TimeScale): TimeValue; external name '_MCGetCurrentTime';
{
    returns the time value and the time scale it is on. if there are no movies, the
        time scale is passed back as 0. scale is an optional parameter

}
{
 *  MCNewAttachedController()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MCNewAttachedController(mc: MovieController; theMovie: Movie; w: WindowRef; where: Point): ComponentResult; external name '_MCNewAttachedController';
{
    makes theMovie the only movie attached to the controller. makes the controller visible.
    the window and where parameters are passed a long to MCSetMovie and behave as
    described there
}
{
 *  MCDraw()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MCDraw(mc: MovieController; w: WindowRef): ComponentResult; external name '_MCDraw';
{
 *  MCActivate()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MCActivate(mc: MovieController; w: WindowRef; activate: boolean): ComponentResult; external name '_MCActivate';
{
 *  MCIdle()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MCIdle(mc: MovieController): ComponentResult; external name '_MCIdle';
{
 *  MCKey()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MCKey(mc: MovieController; key: SInt8; modifiers: SInt32): ComponentResult; external name '_MCKey';
{
 *  MCClick()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MCClick(mc: MovieController; w: WindowRef; where: Point; when: SInt32; modifiers: SInt32): ComponentResult; external name '_MCClick';
{
    calls for editing
}
{
 *  MCEnableEditing()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MCEnableEditing(mc: MovieController; enabled: boolean): ComponentResult; external name '_MCEnableEditing';
{
 *  MCIsEditingEnabled()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MCIsEditingEnabled(mc: MovieController): SInt32; external name '_MCIsEditingEnabled';
{
 *  MCCopy()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MCCopy(mc: MovieController): Movie; external name '_MCCopy';
{
 *  MCCut()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MCCut(mc: MovieController): Movie; external name '_MCCut';
{
 *  MCPaste()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MCPaste(mc: MovieController; srcMovie: Movie): ComponentResult; external name '_MCPaste';
{
 *  MCClear()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MCClear(mc: MovieController): ComponentResult; external name '_MCClear';
{
 *  MCUndo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MCUndo(mc: MovieController): ComponentResult; external name '_MCUndo';
{
 *  somewhat special stuff
 }
{
 *  MCPositionController()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MCPositionController(mc: MovieController; const (*var*) movieRect: Rect; const (*var*) controllerRect: Rect; someFlags: SInt32): ComponentResult; external name '_MCPositionController';
{
 *  MCGetControllerInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MCGetControllerInfo(mc: MovieController; var someFlags: SInt32): ComponentResult; external name '_MCGetControllerInfo';
{
 *  MCSetClip()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MCSetClip(mc: MovieController; theClip: RgnHandle; movieClip: RgnHandle): ComponentResult; external name '_MCSetClip';
{
 *  MCGetClip()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MCGetClip(mc: MovieController; var theClip: RgnHandle; var movieClip: RgnHandle): ComponentResult; external name '_MCGetClip';
{
 *  MCDrawBadge()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MCDrawBadge(mc: MovieController; movieRgn: RgnHandle; var badgeRgn: RgnHandle): ComponentResult; external name '_MCDrawBadge';
{
 *  MCSetUpEditMenu()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MCSetUpEditMenu(mc: MovieController; modifiers: SInt32; mh: MenuRef): ComponentResult; external name '_MCSetUpEditMenu';
{
 *  MCGetMenuString()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MCGetMenuString(mc: MovieController; modifiers: SInt32; item: SInt16; var aString: Str255): ComponentResult; external name '_MCGetMenuString';
{
 *  MCSetActionFilterWithRefCon()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MCSetActionFilterWithRefCon(mc: MovieController; blob: MCActionFilterWithRefConUPP; refCon: SInt32): ComponentResult; external name '_MCSetActionFilterWithRefCon';
{
 *  MCPtInController()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MCPtInController(mc: MovieController; thePt: Point; var inController: boolean): ComponentResult; external name '_MCPtInController';
{
 *  MCInvalidate()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MCInvalidate(mc: MovieController; w: WindowRef; invalidRgn: RgnHandle): ComponentResult; external name '_MCInvalidate';
{
 *  MCAdjustCursor()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MCAdjustCursor(mc: MovieController; w: WindowRef; where: Point; modifiers: SInt32): ComponentResult; external name '_MCAdjustCursor';
{
 *  MCGetInterfaceElement()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MCGetInterfaceElement(mc: MovieController; whichElement: MCInterfaceElement; element: UnivPtr): ComponentResult; external name '_MCGetInterfaceElement';
{
 *  MCGetDoActionsProc()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function MCGetDoActionsProc(mc: MovieController; var doMCActionProc: DoMCActionUPP; var doMCActionRefCon: SInt32): ComponentResult; external name '_MCGetDoActionsProc';
{
 *  MCAddMovieSegment()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 5.0 and later
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 5.0 and later
 }
function MCAddMovieSegment(mc: MovieController; srcMovie: Movie; scaled: boolean): ComponentResult; external name '_MCAddMovieSegment';
{
 *  MCTrimMovieSegment()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 5.0 and later
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 5.0 and later
 }
function MCTrimMovieSegment(mc: MovieController): ComponentResult; external name '_MCTrimMovieSegment';
{
 *  MCSetIdleManager()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 6.0 and later
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.2 and later
 *    Windows:          in qtmlClient.lib 6.0 and later
 }
function MCSetIdleManager(mc: MovieController; im: IdleManager): ComponentResult; external name '_MCSetIdleManager';
{ Called (but not implemented) by controllers that derive from the standard movie controller.
   All controllers except standard movie controller must delegate this call. }

const
	kControllerUnderstandsIdleManagers = $01;

	{
	 *  MCSetControllerCapabilities()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in QuickTimeLib 6.0 and later
	 *    CarbonLib:        in CarbonLib 1.6 and later
	 *    Mac OS X:         in version 10.2 and later
	 *    Windows:          in qtmlClient.lib 6.0 and later
	 	}
function MCSetControllerCapabilities(mc: MovieController; flags: SInt32; flagsMask: SInt32): ComponentResult; external name '_MCSetControllerCapabilities';
{***************************************
*                                       *
*       T  I  M  E  B  A  S  E          *
*                                       *
***************************************}
{
 *  NewTimeBase()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function NewTimeBase: TimeBase; external name '_NewTimeBase';
{
 *  DisposeTimeBase()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure DisposeTimeBase(tb: TimeBase); external name '_DisposeTimeBase';
{
 *  GetTimeBaseTime()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetTimeBaseTime(tb: TimeBase; s: TimeScale; var tr: TimeRecord): TimeValue; external name '_GetTimeBaseTime';
{
 *  SetTimeBaseTime()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure SetTimeBaseTime(tb: TimeBase; const (*var*) tr: TimeRecord); external name '_SetTimeBaseTime';
{
 *  SetTimeBaseValue()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure SetTimeBaseValue(tb: TimeBase; t: TimeValue; s: TimeScale); external name '_SetTimeBaseValue';
{
 *  GetTimeBaseRate()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetTimeBaseRate(tb: TimeBase): Fixed; external name '_GetTimeBaseRate';
{
 *  SetTimeBaseRate()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure SetTimeBaseRate(tb: TimeBase; r: Fixed); external name '_SetTimeBaseRate';
{
 *  GetTimeBaseStartTime()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetTimeBaseStartTime(tb: TimeBase; s: TimeScale; var tr: TimeRecord): TimeValue; external name '_GetTimeBaseStartTime';
{
 *  SetTimeBaseStartTime()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure SetTimeBaseStartTime(tb: TimeBase; const (*var*) tr: TimeRecord); external name '_SetTimeBaseStartTime';
{
 *  GetTimeBaseStopTime()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetTimeBaseStopTime(tb: TimeBase; s: TimeScale; var tr: TimeRecord): TimeValue; external name '_GetTimeBaseStopTime';
{
 *  SetTimeBaseStopTime()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure SetTimeBaseStopTime(tb: TimeBase; const (*var*) tr: TimeRecord); external name '_SetTimeBaseStopTime';
{
 *  GetTimeBaseFlags()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetTimeBaseFlags(tb: TimeBase): SInt32; external name '_GetTimeBaseFlags';
{
 *  SetTimeBaseFlags()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure SetTimeBaseFlags(tb: TimeBase; timeBaseFlags: SInt32); external name '_SetTimeBaseFlags';
{
 *  SetTimeBaseMasterTimeBase()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure SetTimeBaseMasterTimeBase(slave: TimeBase; master: TimeBase; const (*var*) slaveZero: TimeRecord); external name '_SetTimeBaseMasterTimeBase';
{
 *  GetTimeBaseMasterTimeBase()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetTimeBaseMasterTimeBase(tb: TimeBase): TimeBase; external name '_GetTimeBaseMasterTimeBase';
{
 *  SetTimeBaseMasterClock()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure SetTimeBaseMasterClock(slave: TimeBase; clockMeister: Component; const (*var*) slaveZero: TimeRecord); external name '_SetTimeBaseMasterClock';
{
 *  GetTimeBaseMasterClock()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetTimeBaseMasterClock(tb: TimeBase): ComponentInstance; external name '_GetTimeBaseMasterClock';
{
 *  ConvertTime()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure ConvertTime(var theTime: TimeRecord; newBase: TimeBase); external name '_ConvertTime';
{
 *  ConvertTimeScale()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure ConvertTimeScale(var theTime: TimeRecord; newScale: TimeScale); external name '_ConvertTimeScale';
{
 *  AddTime()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure AddTime(var dst: TimeRecord; const (*var*) src: TimeRecord); external name '_AddTime';
{
 *  SubtractTime()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure SubtractTime(var dst: TimeRecord; const (*var*) src: TimeRecord); external name '_SubtractTime';
{
 *  GetTimeBaseStatus()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetTimeBaseStatus(tb: TimeBase; var unpinnedTime: TimeRecord): SInt32; external name '_GetTimeBaseStatus';
{
 *  SetTimeBaseZero()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure SetTimeBaseZero(tb: TimeBase; var zero: TimeRecord); external name '_SetTimeBaseZero';
{
 *  GetTimeBaseEffectiveRate()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetTimeBaseEffectiveRate(tb: TimeBase): Fixed; external name '_GetTimeBaseEffectiveRate';
{***************************************
*                                       *
*       C  A  L  L  B  A  C  K          *
*                                       *
***************************************}
{
 *  NewCallBack()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function NewCallBack(tb: TimeBase; cbType: SInt16): QTCallBack; external name '_NewCallBack';
{
 *  DisposeCallBack()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure DisposeCallBack(cb: QTCallBack); external name '_DisposeCallBack';
{
 *  GetCallBackType()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetCallBackType(cb: QTCallBack): SInt16; external name '_GetCallBackType';
{
 *  GetCallBackTimeBase()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetCallBackTimeBase(cb: QTCallBack): TimeBase; external name '_GetCallBackTimeBase';
{
 *  CallMeWhen()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function CallMeWhen(cb: QTCallBack; callBackProc: QTCallBackUPP; refCon: SInt32; param1: SInt32; param2: SInt32; param3: SInt32): OSErr; external name '_CallMeWhen';
{
 *  CancelCallBack()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure CancelCallBack(cb: QTCallBack); external name '_CancelCallBack';
{***************************************
*                                       *
*       C L O C K   C A L L B A C K     *
*             S U P P O R T             *
*                                       *
***************************************}
{
 *  AddCallBackToTimeBase()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function AddCallBackToTimeBase(cb: QTCallBack): OSErr; external name '_AddCallBackToTimeBase';
{
 *  RemoveCallBackFromTimeBase()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function RemoveCallBackFromTimeBase(cb: QTCallBack): OSErr; external name '_RemoveCallBackFromTimeBase';
{
 *  GetFirstCallBack()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetFirstCallBack(tb: TimeBase): QTCallBack; external name '_GetFirstCallBack';
{
 *  GetNextCallBack()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetNextCallBack(cb: QTCallBack): QTCallBack; external name '_GetNextCallBack';
{
 *  ExecuteCallBack()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure ExecuteCallBack(cb: QTCallBack); external name '_ExecuteCallBack';
{
 *  MusicMediaGetIndexedTunePlayer()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MusicMediaGetIndexedTunePlayer(ti: ComponentInstance; sampleDescIndex: SInt32; var tp: ComponentInstance): ComponentResult; external name '_MusicMediaGetIndexedTunePlayer';
{ UPP call backs }

const
	uppMCActionFilterProcInfo = $00000FD0;
	uppMCActionFilterWithRefConProcInfo = $00003ED0;
	{
	 *  NewMCActionFilterUPP()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   available as macro/inline
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
function NewMCActionFilterUPP(userRoutine: MCActionFilterProcPtr): MCActionFilterUPP; external name '_NewMCActionFilterUPP'; { old name was NewMCActionFilterProc }
{
 *  NewMCActionFilterWithRefConUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewMCActionFilterWithRefConUPP(userRoutine: MCActionFilterWithRefConProcPtr): MCActionFilterWithRefConUPP; external name '_NewMCActionFilterWithRefConUPP'; { old name was NewMCActionFilterWithRefConProc }
{
 *  DisposeMCActionFilterUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeMCActionFilterUPP(userUPP: MCActionFilterUPP); external name '_DisposeMCActionFilterUPP';
{
 *  DisposeMCActionFilterWithRefConUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeMCActionFilterWithRefConUPP(userUPP: MCActionFilterWithRefConUPP); external name '_DisposeMCActionFilterWithRefConUPP';
{
 *  InvokeMCActionFilterUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function InvokeMCActionFilterUPP(mc: MovieController; var action: SInt16; params: UnivPtr; userRoutine: MCActionFilterUPP): boolean; external name '_InvokeMCActionFilterUPP'; { old name was CallMCActionFilterProc }
{
 *  InvokeMCActionFilterWithRefConUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function InvokeMCActionFilterWithRefConUPP(mc: MovieController; action: SInt16; params: UnivPtr; refCon: SInt32; userRoutine: MCActionFilterWithRefConUPP): boolean; external name '_InvokeMCActionFilterWithRefConUPP'; { old name was CallMCActionFilterWithRefConProc }
{$ALIGN MAC68K}


end.
