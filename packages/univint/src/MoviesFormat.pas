{
     File:       MoviesFormat.p
 
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
    Version 210
    Please report any bugs to <gpc@microbizz.nl>
}

{$mode macpas}
{$packenum 1}
{$macro on}
{$inline on}
{$calling mwpascal}

unit MoviesFormat;
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
uses MacTypes,ImageCompression,Components,Movies;


{$ALIGN MAC68K}


const
	kMovieVersion				= 0;							{  version number of the format here described  }

	{	***************************************
	*
	*   General Types -
	*       These types are used in more than one of the
	*       directory types.
	*
	***************************************	}
	{	 MoviesUserData is the type used for user data in movie and track directories 	}

type
	MoviesUserDataPtr = ^MoviesUserData;
	MoviesUserData = record
		size:					SInt32;								{  size of this user data  }
		udType:					SInt32;								{  type of user data  }
		data:					SInt8;									{  the user data  }
	end;

	UserDataAtomPtr = ^UserDataAtom;
	UserDataAtom = record
		size:					SInt32;
		atomType:				SInt32;
		userData:				array [0..0] of MoviesUserData;
	end;

	{	 MoviesDataDescription tells us where the data for the movie or track lives.
	   The data can follow the directory, be in the datafork of the same file as the directory resource,
	   be in the resource fork of the same file as the directory resource, be in another file in the
	   data fork or resource fork, or require a specific bottleneck to fetch the data. 	}
	{	***************************************
	*
	*   MediaDirectory information -
	*       The MediaDirectory is tightly coupled to the data.
	*
	***************************************	}

	SampleDescriptionAtomPtr = ^SampleDescriptionAtom;
	SampleDescriptionAtom = record
		size:					SInt32;
		atomType:				SInt32;								{  = 'stsd'  }
		flags:					SInt32;								{  1 byte of version / 3 bytes of flags  }
		numEntries:				SInt32;
		sampleDescTable:		array [0..0] of SampleDescription;
	end;

	{	 TimeToSampleNum maps physical sample time to physical sample number. 	}
	TimeToSampleNumPtr = ^TimeToSampleNum;
	TimeToSampleNum = record
		sampleCount:			SInt32;
		sampleDuration:			TimeValue;
	end;

	TimeToSampleNumAtomPtr = ^TimeToSampleNumAtom;
	TimeToSampleNumAtom = record
		size:					SInt32;
		atomType:				SInt32;								{  = 'stts'  }
		flags:					SInt32;								{  1 byte of version / 3 bytes of flags  }
		numEntries:				SInt32;
		timeToSampleNumTable:	array [0..0] of TimeToSampleNum;
	end;

	{	 SyncSamples is a list of the physical samples which are self contained. 	}
	SyncSampleAtomPtr = ^SyncSampleAtom;
	SyncSampleAtom = record
		size:					SInt32;
		atomType:				SInt32;								{  = 'stss'  }
		flags:					SInt32;								{  1 byte of version / 3 bytes of flags  }
		numEntries:				SInt32;
		syncSampleTable:		array [0..0] of SInt32;
	end;

	{	 SampleToChunk maps physical sample number to chunk number. 	}
	{	 same as SampleToChunk, but redundant first sample is removed 	}
	SampleToChunkPtr = ^SampleToChunk;
	SampleToChunk = record
		firstChunk:				SInt32;
		samplesPerChunk:		SInt32;
		sampleDescriptionID:	SInt32;
	end;

	SampleToChunkAtomPtr = ^SampleToChunkAtom;
	SampleToChunkAtom = record
		size:					SInt32;
		atomType:				SInt32;								{  = 'stsc'  }
		flags:					SInt32;								{  1 byte of version / 3 bytes of flags  }
		numEntries:				SInt32;
		sampleToChunkTable:		array [0..0] of SampleToChunk;
	end;

	ChunkOffsetAtomPtr = ^ChunkOffsetAtom;
	ChunkOffsetAtom = record
		size:					SInt32;
		atomType:				SInt32;								{  = 'stco'  }
		flags:					SInt32;								{  1 byte of version / 3 bytes of flags  }
		numEntries:				SInt32;
		chunkOffsetTable:		array [0..0] of SInt32;
	end;

	SampleSizeAtomPtr = ^SampleSizeAtom;
	SampleSizeAtom = record
		size:					SInt32;
		atomType:				SInt32;								{  = 'stsz'  }
		flags:					SInt32;								{  1 byte of version / 3 bytes of flags  }
		sampleSize:				SInt32;
		numEntries:				SInt32;
		sampleSizeTable:		array [0..0] of SInt32;
	end;

	ShadowSyncPtr = ^ShadowSync;
	ShadowSync = record
		fdSampleNum:			SInt32;
		syncSampleNum:			SInt32;
	end;

	ShadowSyncAtomPtr = ^ShadowSyncAtom;
	ShadowSyncAtom = record
		size:					SInt32;
		atomType:				SInt32;								{  = 'stsz'  }
		flags:					SInt32;								{  1 byte of version / 3 bytes of flags  }
		numEntries:				SInt32;
		shadowSyncTable:		array [0..0] of ShadowSync;
	end;

	SampleTableAtomPtr = ^SampleTableAtom;
	SampleTableAtom = record
		size:					SInt32;
		atomType:				SInt32;								{  = 'stbl'  }
		sampleDescription:		SampleDescriptionAtom;
		timeToSampleNum:		TimeToSampleNumAtom;
		sampleToChunk:			SampleToChunkAtom;
		syncSample:				SyncSampleAtom;
		sampleSize:				SampleSizeAtom;
		chunkOffset:			ChunkOffsetAtom;
		shadowSync:				ShadowSyncAtom;
	end;

	PublicHandlerInfoPtr = ^PublicHandlerInfo;
	PublicHandlerInfo = record
		flags:					SInt32;								{  1 byte of version / 3 bytes of flags  }
		componentType:			SInt32;
		componentSubType:		SInt32;
		componentManufacturer:	SInt32;
		componentFlags:			SInt32;
		componentFlagsMask:		SInt32;
		componentName:			SInt8;
	end;

	HandlerAtomPtr = ^HandlerAtom;
	HandlerAtom = record
		size:					SInt32;
		atomType:				SInt32;								{  = 'hdlr'  }
		hInfo:					PublicHandlerInfo;
	end;

	{	 a data reference is a private structure 	}
	DataRefAtom							= SInt32;
	DataInfoAtomPtr = ^DataInfoAtom;
	DataInfoAtom = record
		size:					SInt32;
		atomType:				SInt32;								{  = 'dinf'  }
		dataRef:				DataRefAtom;
	end;

	RgnAtomPtr = ^RgnAtom;
	RgnAtom = record
		size:					SInt32;
		atomType:				SInt32;
		rgnSize:				SInt16;
		rgnBBox:				Rect;
		data:					SInt8;
	end;

	MatteCompressedAtomPtr = ^MatteCompressedAtom;
	MatteCompressedAtom = record
		size:					SInt32;
		atomType:				SInt32;
		flags:					SInt32;								{  1 byte of version / 3 bytes of flags  }
		matteImageDescription:	ImageDescription;
		matteData:				SInt8;
	end;

	MatteAtomPtr = ^MatteAtom;
	MatteAtom = record
		size:					SInt32;
		atomType:				SInt32;
		aCompressedMatte:		MatteCompressedAtom;
	end;

	ClippingAtomPtr = ^ClippingAtom;
	ClippingAtom = record
		size:					SInt32;
		atomType:				SInt32;
		aRgnClip:				RgnAtom;
	end;

	{	**********************
	* Media Info Example Structures
	**********************	}

	VideoMediaInfoHeaderPtr = ^VideoMediaInfoHeader;
	VideoMediaInfoHeader = record
		flags:					SInt32;								{  1 byte of version / 3 bytes of flags  }
		graphicsMode:			SInt16;								{  for QD - transfer mode  }
		opColorRed:				SInt16;								{  opcolor for transfer mode  }
		opColorGreen:			SInt16;
		opColorBlue:			SInt16;
	end;

	VideoMediaInfoHeaderAtomPtr = ^VideoMediaInfoHeaderAtom;
	VideoMediaInfoHeaderAtom = record
		size:					SInt32;								{  size of Media info  }
		atomType:				SInt32;								{  = 'vmhd'  }
		vmiHeader:				VideoMediaInfoHeader;
	end;

	VideoMediaInfoPtr = ^VideoMediaInfo;
	VideoMediaInfo = record
		size:					SInt32;								{  size of Media info  }
		atomType:				SInt32;								{  = 'minf'  }
		header:					VideoMediaInfoHeaderAtom;
		dataHandler:			HandlerAtom;
		dataInfo:				DataInfoAtom;
		sampleTable:			SampleTableAtom;
	end;

	SoundMediaInfoHeaderPtr = ^SoundMediaInfoHeader;
	SoundMediaInfoHeader = record
		flags:					SInt32;								{  1 byte of version / 3 bytes of flags  }
		balance:				SInt16;
		rsrvd:					SInt16;
	end;

	SoundMediaInfoHeaderAtomPtr = ^SoundMediaInfoHeaderAtom;
	SoundMediaInfoHeaderAtom = record
		size:					SInt32;								{  size of Media info  }
		atomType:				SInt32;								{  = 'vmhd'  }
		smiHeader:				SoundMediaInfoHeader;
	end;

	SoundMediaInfoPtr = ^SoundMediaInfo;
	SoundMediaInfo = record
		size:					SInt32;								{  size of Media info  }
		atomType:				SInt32;								{  = 'minf'  }
		header:					SoundMediaInfoHeaderAtom;
		dataHandler:			HandlerAtom;
		dataReference:			DataRefAtom;
		sampleTable:			SampleTableAtom;
	end;

	{	 whatever data the media handler needs goes after the atomType 	}
	MediaInfoPtr = ^MediaInfo;
	MediaInfo = record
		size:					SInt32;
		atomType:				SInt32;
	end;
	MediaInfo_fix = MediaInfo; { used as field type when a record declaration contains a MediaInfo field identifier }

	{	**********************
	* Media Directory Structures
	**********************	}
	MediaHeaderPtr = ^MediaHeader;
	MediaHeader = record
		flags:					SInt32;								{  1 byte of version / 3 bytes of flags  }
		creationTime:			SInt32;								{  seconds since Jan 1904 when directory was created  }
		modificationTime:		SInt32;								{  seconds since Jan 1904 when directory was appended  }
		timeScale:				TimeValue;								{  start time for Media (Media time)  }
		duration:				TimeValue;								{  length of Media (Media time)  }
		language:				SInt16;
		quality:				SInt16;
	end;

	MediaHeaderAtomPtr = ^MediaHeaderAtom;
	MediaHeaderAtom = record
		size:					SInt32;
		atomType:				SInt32;
		header:					MediaHeader;
	end;

	MediaDirectoryPtr = ^MediaDirectory;
	MediaDirectory = record
		size:					SInt32;
		atomType:				SInt32;								{  = 'mdia'  }
		mediaHeader:			MediaHeaderAtom;						{  standard Media information  }
		mediaHandler:			HandlerAtom;
		mediaInfo:				MediaInfo_fix;
	end;

	{	**********************
	* Track Structures
	**********************	}

const
	TrackEnable					= $01;
	TrackInMovie				= $02;
	TrackInPreview				= $04;
	TrackInPoster				= $08;


type
	TrackHeaderPtr = ^TrackHeader;
	TrackHeader = record
		flags:					SInt32;								{  1 byte of version / 3 bytes of flags  }
		creationTime:			SInt32;								{  seconds since Jan 1904 when directory was created  }
		modificationTime:		SInt32;								{  seconds since Jan 1904 when directory was appended  }
		trackID:				SInt32;
		reserved1:				SInt32;
		duration:				TimeValue;								{  length of track (track time)  }
		reserved2:				SInt32;
		reserved3:				SInt32;
		layer:					SInt16;
		alternateGroup:			SInt16;
		volume:					SInt16;
		reserved4:				SInt16;
		matrix:					MatrixRecord;
		trackWidth:				Fixed;
		trackHeight:			Fixed;
	end;

	TrackHeaderAtomPtr = ^TrackHeaderAtom;
	TrackHeaderAtom = record
		size:					SInt32;								{  size of track header  }
		atomType:				SInt32;								{  = 'tkhd'  }
		header:					TrackHeader;
	end;

	EditListTypePtr = ^EditListType;
	EditListType = record
		trackDuration:			TimeValue;
		mediaTime:				TimeValue;
		mediaRate:				Fixed;
	end;

	EditListAtomPtr = ^EditListAtom;
	EditListAtom = record
		size:					SInt32;
		atomType:				SInt32;								{  = elst  }
		flags:					SInt32;								{  1 byte of version / 3 bytes of flags  }
		numEntries:				SInt32;
		editListTable:			array [0..0] of EditListType;
	end;

	EditsAtomPtr = ^EditsAtom;
	EditsAtom = record
		size:					SInt32;
		atomType:				SInt32;								{  = edts  }
		editList:				EditListAtom;
	end;

	TrackLoadSettingsPtr = ^TrackLoadSettings;
	TrackLoadSettings = record
		preloadStartTime:		TimeValue;
		preloadDuration:		TimeValue;
		preloadFlags:			SInt32;
		defaultHints:			SInt32;
	end;

	TrackLoadSettingsAtomPtr = ^TrackLoadSettingsAtom;
	TrackLoadSettingsAtom = record
		size:					SInt32;
		atomType:				SInt32;								{  = load  }
		settings:				TrackLoadSettings;
	end;

	TrackDirectoryPtr = ^TrackDirectory;
	TrackDirectory = record
		size:					SInt32;
		atomType:				SInt32;								{  = 'trak'  }
		trackHeader:			TrackHeaderAtom;						{  standard track information  }
		trackClip:				ClippingAtom;
		edits:					EditsAtom;
		media:					MediaDirectory;
		userData:				UserDataAtom;							{  space for extending with new data types  }
	end;
	TrackDirectory_fix = TrackDirectory; { used as field type when a record declaration contains a TrackDirectory field identifier }

	{	***************************************
	*
	*   MovieDirectory -
	*       The MovieDirectory is the top level structure which
	*       holds the TrackInstance describing where the
	*       TrackDirectories are.
	*
	***************************************	}
	MovieHeaderPtr = ^MovieHeader;
	MovieHeader = record
		flags:					SInt32;								{  1 byte of version / 3 bytes of flags  }
		creationTime:			SInt32;								{  seconds since Jan 1904 when directory was created  }
		modificationTime:		SInt32;								{  seconds since Jan 1904 when directory was appended  }
		timeScale:				TimeValue;								{  Time specifications  }
		duration:				TimeValue;
		preferredRate:			Fixed;									{  rate at which to play this movie  }
		preferredVolume:		SInt16;								{  volume to play movie at  }
		reserved1:				SInt16;
		preferredLong1:			SInt32;
		preferredLong2:			SInt32;
		matrix:					MatrixRecord;
		previewTime:			TimeValue;								{  time in track the proxy begins (track time)  }
		previewDuration:		TimeValue;								{  how long the proxy lasts (track time)  }
		posterTime:				TimeValue;								{  time in track the proxy begins (track time)  }
		selectionTime:			TimeValue;								{  time in track the proxy begins (track time)  }
		selectionDuration:		TimeValue;								{  time in track the proxy begins (track time)  }
		currentTime:			TimeValue;								{  time in track the proxy begins (track time)  }
		nextTrackID:			SInt32;								{  next value to use for a TrackID  }
	end;

	MovieHeaderAtomPtr = ^MovieHeaderAtom;
	MovieHeaderAtom = record
		size:					SInt32;
		atomType:				SInt32;								{  = 'mvhd'  }
		header:					MovieHeader;
	end;

	TrackDirectoryEntryPtr = ^TrackDirectoryEntry;
	TrackDirectoryEntry = record
		trackDirectory:			TrackDirectory_fix;							{  Track directory information  }
	end;

	MovieDirectoryPtr = ^MovieDirectory;
	MovieDirectory = record
		size:					SInt32;
		atomType:				SInt32;								{  = 'moov'  }
		header:					MovieHeaderAtom;
		movieClip:				ClippingAtom;
																		{  Track Directories  }
		track:					array [0..0] of TrackDirectoryEntry;	{  Track directory information  }
																		{  User data for Movie  }
		userData:				UserDataAtom;							{  space for user extensions  }
	end;

	{	***************************************
	***************************************	}

	{	 Movie formats and tags 	}

const
																{  some system defined format IDs  }
	MOVIE_TYPE					= FourCharCode('moov');
	TRACK_TYPE					= FourCharCode('trak');
	MEDIA_TYPE					= FourCharCode('mdia');
	VIDEO_TYPE					= FourCharCode('vide');
	SOUND_TYPE					= FourCharCode('soun');

	{	 atom id's 	}
	MovieAID					= FourCharCode('moov');
	MovieHeaderAID				= FourCharCode('mvhd');
	ClipAID						= FourCharCode('clip');
	RgnClipAID					= FourCharCode('crgn');
	MatteAID					= FourCharCode('matt');
	MatteCompAID				= FourCharCode('kmat');
	TrackAID					= FourCharCode('trak');
	UserDataAID					= FourCharCode('udta');
	TrackHeaderAID				= FourCharCode('tkhd');
	EditsAID					= FourCharCode('edts');
	EditListAID					= FourCharCode('elst');
	MediaAID					= FourCharCode('mdia');
	MediaHeaderAID				= FourCharCode('mdhd');
	MediaInfoAID				= FourCharCode('minf');
	VideoMediaInfoHeaderAID		= FourCharCode('vmhd');
	SoundMediaInfoHeaderAID		= FourCharCode('smhd');
	GenericMediaInfoHeaderAID	= FourCharCode('gmhd');
	GenericMediaInfoAID			= FourCharCode('gmin');
	DataInfoAID					= FourCharCode('dinf');
	DataRefAID					= FourCharCode('dref');
	SampleTableAID				= FourCharCode('stbl');
	STSampleDescAID				= FourCharCode('stsd');
	STTimeToSampAID				= FourCharCode('stts');
	STSyncSampleAID				= FourCharCode('stss');
	STSampleToChunkAID			= FourCharCode('stsc');
	STShadowSyncAID				= FourCharCode('stsh');
	HandlerAID					= FourCharCode('hdlr');
	STSampleSizeAID				= FourCharCode('stsz');
	STChunkOffsetAID			= FourCharCode('stco');
	STChunkOffset64AID			= FourCharCode('co64');
	STSampleIDAID				= FourCharCode('stid');
	DataRefContainerAID			= FourCharCode('drfc');
	TrackReferenceAID			= FourCharCode('tref');
	ColorTableAID				= FourCharCode('ctab');
	LoadSettingsAID				= FourCharCode('load');
	PropertyAtomAID				= FourCharCode('code');
	InputMapAID					= FourCharCode('imap');
	MovieBufferHintsAID			= FourCharCode('mbfh');
	MovieDataRefAliasAID		= FourCharCode('mdra');
	SoundLocalizationAID		= FourCharCode('sloc');
	CompressedMovieAID			= FourCharCode('cmov');
	CompressedMovieDataAID		= FourCharCode('cmvd');
	DataCompressionAtomAID		= FourCharCode('dcom');
	ReferenceMovieRecordAID		= FourCharCode('rmra');
	ReferenceMovieDescriptorAID	= FourCharCode('rmda');
	ReferenceMovieDataRefAID	= FourCharCode('rdrf');
	ReferenceMovieVersionCheckAID = FourCharCode('rmvc');
	ReferenceMovieDataRateAID	= FourCharCode('rmdr');
	ReferenceMovieComponentCheckAID = FourCharCode('rmcd');
	ReferenceMovieQualityAID	= FourCharCode('rmqu');
	ReferenceMovieLanguageAID	= FourCharCode('rmla');
	ReferenceMovieCPURatingAID	= FourCharCode('rmcs');
	ReferenceMovieAlternateGroupAID = FourCharCode('rmag');
	ReferenceMovieNetworkStatusAID = FourCharCode('rnet');
	CloneMediaAID				= FourCharCode('clon');
	FileTypeAID					= FourCharCode('ftyp');
	SecureContentInfoAID		= FourCharCode('sinf');
	SecureContentSchemeTypeAID	= FourCharCode('schm');
	SecureContentSchemeInfoAID	= FourCharCode('schi');

	{  Text ATOM definitions }


type
	TextBoxAtomPtr = ^TextBoxAtom;
	TextBoxAtom = record
		size:					SInt32;
		atomType:				SInt32;								{  = 'tbox'  }
		textBox:				Rect;									{  New text box (overrides defaultTextBox) }
	end;

	HiliteAtomPtr = ^HiliteAtom;
	HiliteAtom = record
		size:					SInt32;
		atomType:				SInt32;								{  = 'hlit'  }
		selStart:				SInt32;								{  hilite selection start character }
		selEnd:					SInt32;								{  hilite selection end character }
	end;

	KaraokeRecPtr = ^KaraokeRec;
	KaraokeRec = record
		timeVal:				TimeValue;
		beginHilite:			SInt16;
		endHilite:				SInt16;
	end;

	KaraokeAtomPtr = ^KaraokeAtom;
	KaraokeAtom = record
		numEntries:				SInt32;
		karaokeEntries:			array [0..0] of KaraokeRec;
	end;

	{  for ReferenceMovieDataRefRecord.flags }

const
	kDataRefIsSelfContained		= $01;


type
	ReferenceMovieDataRefRecordPtr = ^ReferenceMovieDataRefRecord;
	ReferenceMovieDataRefRecord = record
		flags:					SInt32;
		dataRefType:			OSType;
		dataRefSize:			SInt32;
		dataRef:				SInt8;
	end;

	{  for VersionCheckRecord.checkType }

const
	kVersionCheckMin			= 0;							{  val1 is the min. version required }
	kVersionCheckMask			= 1;							{  (gestalt return value & val2) must == val1 }


type
	QTAltVersionCheckRecordPtr = ^QTAltVersionCheckRecord;
	QTAltVersionCheckRecord = record
		flags:					SInt32;								{  currently always 0 }
		gestaltTag:				OSType;
		val1:					UInt32;
		val2:					UInt32;
		checkType:				SInt16;
	end;

	{  some helpful constants for DataRateRecord.dataRate  }

const
	kDataRate144ModemRate		= 1400;
	kDataRate288ModemRate		= 2800;
	kDataRateISDNRate			= 5600;
	kDataRateDualISDNRate		= 11200;
	kDataRate256kbpsRate		= 25600;
	kDataRate384kbpsRate		= 38400;
	kDataRate512kbpsRate		= 51200;
	kDataRate768kbpsRate		= 76800;
	kDataRate1MbpsRate			= 100000;
	kDataRateT1Rate				= 150000;
	kDataRateInfiniteRate		= $7FFFFFFF;
	kDataRateDefaultIfNotSet	= 5600;


type
	QTAltDataRateRecordPtr = ^QTAltDataRateRecord;
	QTAltDataRateRecord = record
		flags:					SInt32;								{  currently always 0 }
		dataRate:				SInt32;
	end;

	QTAltComponentCheckRecordPtr = ^QTAltComponentCheckRecord;
	QTAltComponentCheckRecord = record
		flags:					SInt32;								{  currently always 0  }
		cd:						ComponentDescription;
		minVersion:				UInt32;
	end;

	QTAltLanguageRecordPtr = ^QTAltLanguageRecord;
	QTAltLanguageRecord = record
		flags:					SInt32;								{  currently always 0 }
		language:				SInt16;
	end;


const
	kQTCPUSpeed1Rating			= 100;							{  slowest }
	kQTCPUSpeed2Rating			= 200;
	kQTCPUSpeed3Rating			= 300;
	kQTCPUSpeed4Rating			= 400;
	kQTCPUSpeed5Rating			= 500;							{  fastest }


type
	QTAltCPURatingRecordPtr = ^QTAltCPURatingRecord;
	QTAltCPURatingRecord = record
		flags:					UInt32;									{  currently always 0 }
		speed:					UInt16;
	end;

	ReferenceMovieNetworkStatusRecordPtr = ^ReferenceMovieNetworkStatusRecord;
	ReferenceMovieNetworkStatusRecord = record
		flags:					UInt32;									{  currently always 0 }
		valueCount:				UInt32;									{  how many status values are in array }
		netStatusValues:		array [0..0] of SInt32;				{  a value from kQTNetworkStatus... constants }
	end;

	CloneRecordPtr = ^CloneRecord;
	CloneRecord = record
		flags:					SInt32;
		masterTrackID:			SInt32;								{  track ID of the track we're cloning  }
	end;

	CloneAtomPtr = ^CloneAtom;
	CloneAtom = record
		size:					SInt32;
		atomType:				SInt32;								{  = clon  }
		cloneInfo:				CloneRecord;
	end;

	FileTypeAtomPtr = ^FileTypeAtom;
	FileTypeAtom = record
		size:					SInt32;
		atomType:				SInt32;								{  = 'ftyp'  }
		majorBrand:				SInt32;								{  best use brand  }
		minorVersion:			SInt32;
		compatibleBrands:		array [0..3] of SInt32;				{  1 or greater  }
	end;


const
	kQTFileTypeBrandQuickTimeMovie = FourCharCode('qt  ');					{  QuickTime movie files }
	kQTFileTypeBrandISOFile		= FourCharCode('isom');						{  ISO Base Media files }
	kQTFileTypeBrandMPEG4v1		= FourCharCode('mp41');						{  MPEG-4 (ISO/IEC 14496-1) version 1 files }
	kQTFileTypeBrandMPEG4v2		= FourCharCode('mp42');						{  MPEG-4 (ISO/IEC 14496-1) version 2 files }


type
	SecureContentInfoAtomPtr = ^SecureContentInfoAtom;
	SecureContentInfoAtom = record
		size:					SInt32;
		atomType:				SInt32;								{  = 'sinf'  }
	end;

	SecureContentSchemeTypeAtomPtr = ^SecureContentSchemeTypeAtom;
	SecureContentSchemeTypeAtom = record
		size:					SInt32;
		atomType:				SInt32;								{  = 'schm'  }
		flags:					SInt32;								{  1 byte of version / 3 bytes of flags  }
		schemeType:				SInt32;
		schemeVersion:			UInt16;
																		{  if flags & 1, C string holding URL for security component server }
	end;

	SecureContentSchemeInfoAtomPtr = ^SecureContentSchemeInfoAtom;
	SecureContentSchemeInfoAtom = record
		size:					SInt32;
		atomType:				SInt32;								{  = 'schi'  }
	end;

{$ALIGN MAC68K}


end.
