{
     File:       QuickTime/MoviesFormat.h
 
     Contains:   QuickTime Interfaces.
 
     Version:    QuickTime 7.7.1
 
     Copyright:  © 1990-2012 by Apple Inc., all rights reserved
 
     Bugs?:      For bug reports, consult the following page on
                 the World Wide Web:
 
                     http://www.freepascal.org/bugs.html
 
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

unit MoviesFormat;
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
	{$setc TARGET_OS_EMBEDDED := FALSE}
{$elifc defined __ppc64__ and __ppc64__}
	{$setc TARGET_CPU_PPC := FALSE}
	{$setc TARGET_CPU_PPC64 := TRUE}
	{$setc TARGET_CPU_X86 := FALSE}
	{$setc TARGET_CPU_X86_64 := FALSE}
	{$setc TARGET_CPU_ARM := FALSE}
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
	{$setc TARGET_OS_MAC := TRUE}
	{$setc TARGET_OS_IPHONE := FALSE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
	{$setc TARGET_OS_EMBEDDED := FALSE}
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
	{$setc TARGET_OS_EMBEDDED := TRUE}
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
uses MacTypes,ImageCompression,Components,Movies;
{$endc} {not MACOSALLINCLUDE}


{$ifc TARGET_OS_MAC}

{$ALIGN MAC68K}


const
	kMovieVersion = 0;     { version number of the format here described }

{***************************************
*
*   General Types -
*       These types are used in more than one of the
*       directory types.
*
***************************************}
{ MoviesUserData is the type used for user data in movie and track directories }
type
	MoviesUserDataPtr = ^MoviesUserData;
	MoviesUserData = record
		size: SInt32;                   { size of this user data }
		udType: SInt32;                 { type of user data }
		data: array [0..0] of SInt8;                { the user data }
	end;
type
	UserDataAtomPtr = ^UserDataAtom;
	UserDataAtom = record
		size: SInt32;
		atomType: SInt32;
		userData: array [0..0] of MoviesUserData;
	end;
{ MoviesDataDescription tells us where the data for the movie or track lives.
   The data can follow the directory, be in the datafork of the same file as the directory resource,
   be in the resource fork of the same file as the directory resource, be in another file in the
   data fork or resource fork, or require a specific bottleneck to fetch the data. }
{***************************************
*
*   MediaDirectory information -
*       The MediaDirectory is tightly coupled to the data.
*
***************************************}
{ SampleDescription is in Movies.h }
type
	SampleDescriptionAtomPtr = ^SampleDescriptionAtom;
	SampleDescriptionAtom = record
		size: SInt32;
		atomType: SInt32;               { = 'stsd' }
		flags: SInt32;                  { 1 byte of version / 3 bytes of flags }
		numEntries: SInt32;
		sampleDescTable: array [0..0] of SampleDescription;
	end;
{ TimeToSampleNum maps physical sample time to physical sample number. }
type
	TimeToSampleNumPtr = ^TimeToSampleNum;
	TimeToSampleNum = record
		sampleCount: SInt32;
		sampleDuration: TimeValue;
	end;
type
	TimeToSampleNumAtomPtr = ^TimeToSampleNumAtom;
	TimeToSampleNumAtom = record
		size: SInt32;
		atomType: SInt32;               { = 'stts' }
		flags: SInt32;                  { 1 byte of version / 3 bytes of flags }
		numEntries: SInt32;
		timeToSampleNumTable: array [0..0] of TimeToSampleNum;
	end;
{ SyncSamples is a list of the physical samples which are self contained. }
type
	SyncSampleAtomPtr = ^SyncSampleAtom;
	SyncSampleAtom = record
		size: SInt32;
		atomType: SInt32;               { = 'stss' }
		flags: SInt32;                  { 1 byte of version / 3 bytes of flags }
		numEntries: SInt32;
		syncSampleTable: array [0..0] of SInt32;
	end;
{ SampleToChunk maps physical sample number to chunk number. }
{ same as SampleToChunk, but redundant first sample is removed }
type
	SampleToChunkPtr = ^SampleToChunk;
	SampleToChunk = record
		firstChunk: SInt32;
		samplesPerChunk: SInt32;
		sampleDescriptionID: SInt32;
	end;
type
	SampleToChunkAtomPtr = ^SampleToChunkAtom;
	SampleToChunkAtom = record
		size: SInt32;
		atomType: SInt32;               { = 'stsc' }
		flags: SInt32;                  { 1 byte of version / 3 bytes of flags }
		numEntries: SInt32;
		sampleToChunkTable: array [0..0] of SampleToChunk;
	end;
type
	ChunkOffsetAtomPtr = ^ChunkOffsetAtom;
	ChunkOffsetAtom = record
		size: SInt32;
		atomType: SInt32;               { = 'stco' }
		flags: SInt32;                  { 1 byte of version / 3 bytes of flags }
		numEntries: SInt32;
		chunkOffsetTable: array [0..0] of SInt32;
	end;
type
	SampleSizeAtomPtr = ^SampleSizeAtom;
	SampleSizeAtom = record
		size: SInt32;
		atomType: SInt32;               { = 'stsz' }
		flags: SInt32;                  { 1 byte of version / 3 bytes of flags }
		sampleSize: SInt32;
		numEntries: SInt32;
		sampleSizeTable: array [0..0] of SInt32;
	end;
type
	ShadowSyncPtr = ^ShadowSync;
	ShadowSync = record
		fdSampleNum: SInt32;
		syncSampleNum: SInt32;
	end;
type
	ShadowSyncAtomPtr = ^ShadowSyncAtom;
	ShadowSyncAtom = record
		size: SInt32;
		atomType: SInt32;               { = 'stsz' }
		flags: SInt32;                  { 1 byte of version / 3 bytes of flags }
		numEntries: SInt32;
		shadowSyncTable: array [0..0] of ShadowSync;
	end;
{ CompositionOffsetEntry maps sample numbers to composition offsets. }
type
	CompositionOffsetEntryPtr = ^CompositionOffsetEntry;
	CompositionOffsetEntry = record
		sampleCount: SInt32;
		displayOffset: TimeValue;
	end;
type
	CompositionOffsetAtomPtr = ^CompositionOffsetAtom;
	CompositionOffsetAtom = record
		size: SInt32;
		atomType: SInt32;               { = 'ctts' }
		flags: SInt32;                  { 1 byte of version / 3 bytes of flags }
		numEntries: SInt32;
		compositionOffsetTable: array [0..0] of CompositionOffsetEntry;
	end;
type
	SampleDependencyAtomPtr = ^SampleDependencyAtom;
	SampleDependencyAtom = record
		size: SInt32;
		atomType: SInt32;               { = 'sdtp' }
		flags: SInt32;                  { 1 byte of version / 3 bytes of flags }
		sampleDependencyTable: array [0..0] of UInt8;
	end;
{
NOTE: The values for these flags that shipped with QuickTime 7.0 were incorrect. 
They matched neither the specification nor the implementation -- the "Yes" and "No" bits
were reversed.  The flags have been corrected but renamed to ensure that code using
the incorrect flags is reviewed by developers.
enum (
    kQTSampleDependency_DependsOnOthers = 1<<5,         // INCORRECT VALUE
    kQTSampleDependency_DoesNotDependOnOthers = 1<<4,   // INCORRECT VALUE
    kQTSampleDependency_IsDependedOnByOthers = 1<<3,    // INCORRECT VALUE
    kQTSampleDependency_IsNotDependedOnByOthers = 1<<2, // INCORRECT VALUE
    kQTSampleDependency_HasRedundantCoding = 1<<1,      // INCORRECT VALUE
    kQTSampleDependency_HasNoRedundantCoding = 1<<0     // INCORRECT VALUE
);
}
{ Values for entries in SampleDependencyAtom.sampleDependencyTable[]}
const
{ bit 0x80 is reserved; bit combinations 0x30, 0xC0 and 0x03 are reserved}
	kQTSampleDependency_EarlierDisplayTimesAllowed = 1 shl 6; { corresponds to flag mediaSampleEarlierDisplayTimesAllowed except at different bit offset}
	kQTSampleDependency_DoesNotDependOnOthers_Corrected = 1 shl 5; { ie: an I picture}
	kQTSampleDependency_DependsOnOthers_Corrected = 1 shl 4; { ie: not an I picture}
	kQTSampleDependency_IsNotDependedOnByOthers_Corrected = 1 shl 3; { mediaSampleDroppable}
	kQTSampleDependency_IsDependedOnByOthers_Corrected = 1 shl 2;
	kQTSampleDependency_HasNoRedundantCoding_Corrected = 1 shl 1;
	kQTSampleDependency_HasRedundantCoding_Corrected = 1 shl 0;

type
	CompositionShiftLeastGreatestAtomPtr = ^CompositionShiftLeastGreatestAtom;
	CompositionShiftLeastGreatestAtom = record
		size: SInt32;
		atomType: SInt32;               { = 'cslg' }
		flags: SInt32;                  { 1 byte of version / 3 bytes of flags }
		compositionOffsetToDTDDeltaShift: SInt32;
		leastDecodeToDisplayDelta: SInt32;
		greatestDecodeToDisplayDelta: SInt32;
		displayStartTime: SInt32;
		displayEndTime: SInt32;
	end;
type
	PartialSyncSampleAtomPtr = ^PartialSyncSampleAtom;
	PartialSyncSampleAtom = record
		size: SInt32;
		atomType: SInt32;               { = 'stps' }
		flags: SInt32;                  { 1 byte of version / 3 bytes of flags }
		numEntries: SInt32;
		partialSyncSampleTable: array [0..0] of UInt32;
	end;
type
	SampleTableAtomPtr = ^SampleTableAtom;
	SampleTableAtom = record
		size: SInt32;
		atomType: SInt32;               { = 'stbl' }

		sampleDescription: SampleDescriptionAtom;
		timeToSampleNum: TimeToSampleNumAtom;
		sampleToChunk: SampleToChunkAtom;
		syncSample: SyncSampleAtom;
		sampleSize: SampleSizeAtom;
		chunkOffset: ChunkOffsetAtom;
		shadowSync: ShadowSyncAtom;
	end;
type
	PublicHandlerInfoPtr = ^PublicHandlerInfo;
	PublicHandlerInfo = record
		flags: SInt32;                  { 1 byte of version / 3 bytes of flags }

		componentType: SInt32;
		componentSubType: SInt32;
		componentManufacturer: SInt32;
		componentFlags: SInt32;
		componentFlagsMask: SInt32;
		componentName: array [0..0] of SInt8;
	end;
type
	HandlerAtomPtr = ^HandlerAtom;
	HandlerAtom = record
		size: SInt32;
		atomType: SInt32;               { = 'hdlr' }

		hInfo: PublicHandlerInfo;
	end;
{ a data reference is a private structure }

type
	DataRefAtom = SInt32;
	DataInfoAtom = record
		size: SInt32;
		atomType: SInt32;               { = 'dinf' }

		dataRef: DataRefAtom;
	end;
type
	RgnAtomPtr = ^RgnAtom;
	RgnAtom = record
		size: SInt32;
		atomType: SInt32;

		rgnSize: SInt16;
		rgnBBox: Rect;
		data: array [0..0] of SInt8;
	end;
type
	MatteCompressedAtomPtr = ^MatteCompressedAtom;
	MatteCompressedAtom = record
		size: SInt32;
		atomType: SInt32;

		flags: SInt32;                  { 1 byte of version / 3 bytes of flags }

		matteImageDescription: ImageDescription;
		matteData: array [0..0] of SInt8;
	end;
type
	MatteAtomPtr = ^MatteAtom;
	MatteAtom = record
		size: SInt32;
		atomType: SInt32;

		aCompressedMatte: MatteCompressedAtom;
	end;
type
	ClippingAtomPtr = ^ClippingAtom;
	ClippingAtom = record
		size: SInt32;
		atomType: SInt32;

		aRgnClip: RgnAtom;
	end;
{**********************
* Media Info Example Structures
**********************}

type
	VideoMediaInfoHeaderPtr = ^VideoMediaInfoHeader;
	VideoMediaInfoHeader = record
		flags: SInt32;                  { 1 byte of version / 3 bytes of flags }

		graphicsMode: SInt16;           { for QD - transfer mode }
		opColorRed: SInt16;             { opcolor for transfer mode }
		opColorGreen: SInt16;
		opColorBlue: SInt16;
	end;
type
	VideoMediaInfoHeaderAtomPtr = ^VideoMediaInfoHeaderAtom;
	VideoMediaInfoHeaderAtom = record
		size: SInt32;                   { size of Media info }
		atomType: SInt32;               { = 'vmhd' }
		vmiHeader: VideoMediaInfoHeader;
	end;
type
	VideoMediaInfoPtr = ^VideoMediaInfo;
	VideoMediaInfo = record
		size: SInt32;                   { size of Media info }
		atomType: SInt32;               { = 'minf' }

		header: VideoMediaInfoHeaderAtom;

		dataHandler: HandlerAtom;

		dataInfo: DataInfoAtom;

		sampleTable: SampleTableAtom;
	end;
type
	SoundMediaInfoHeaderPtr = ^SoundMediaInfoHeader;
	SoundMediaInfoHeader = record
		flags: SInt32;                  { 1 byte of version / 3 bytes of flags }

		balance: SInt16;
		rsrvd: SInt16;
	end;
type
	SoundMediaInfoHeaderAtomPtr = ^SoundMediaInfoHeaderAtom;
	SoundMediaInfoHeaderAtom = record
		size: SInt32;                   { size of Media info }
		atomType: SInt32;               { = 'vmhd' }

		smiHeader: SoundMediaInfoHeader;
	end;
type
	SoundMediaInfoPtr = ^SoundMediaInfo;
	SoundMediaInfo = record
		size: SInt32;                   { size of Media info }
		atomType: SInt32;               { = 'minf' }

		header: SoundMediaInfoHeaderAtom;

		dataHandler: HandlerAtom;

		dataReference: DataRefAtom;

		sampleTable: SampleTableAtom;
	end;
{ whatever data the media handler needs goes after the atomType }
type
	MediaInfoPtr = ^MediaInfo;
	MediaInfo = record
		size: SInt32;
		atomType: SInt32;
	end;
	MediaInfo_fix = MediaInfo;
{**********************
* Media Directory Structures
**********************}
type
	MediaHeaderPtr = ^MediaHeader;
	MediaHeader = record
		flags: SInt32;                  { 1 byte of version / 3 bytes of flags }

		creationTime: SInt32;           { seconds since Jan 1904 when directory was created }
		modificationTime: SInt32;       { seconds since Jan 1904 when directory was appended }

		timeScale: TimeValue;              { start time for Media (Media time) }
		duration: TimeValue;               { length of Media (Media time) }

		language: SInt16;
		quality: SInt16;
	end;
type
	MediaHeaderAtomPtr = ^MediaHeaderAtom;
	MediaHeaderAtom = record
		size: SInt32;
		atomType: SInt32;

		header: MediaHeader;
	end;
type
	MediaDirectoryPtr = ^MediaDirectory;
	MediaDirectory = record
		size: SInt32;
		atomType: SInt32;               { = 'mdia' }

		mediaHeader: MediaHeaderAtom;            { standard Media information }

		mediaHandler: HandlerAtom;

		mediaInfo: MediaInfo_fix;
	end;
{**********************
* Track Structures
**********************}
const
	TrackEnable = 1 shl 0;
	TrackInMovie = 1 shl 1;
	TrackInPreview = 1 shl 2;
	TrackInPoster = 1 shl 3;

type
	TrackHeaderPtr = ^TrackHeader;
	TrackHeader = record
		flags: SInt32;                  { 1 byte of version / 3 bytes of flags }

		creationTime: SInt32;           { seconds since Jan 1904 when directory was created }
		modificationTime: SInt32;       { seconds since Jan 1904 when directory was appended }

		trackID: SInt32;

		reserved1: SInt32;

		duration: TimeValue;               { length of track (track time) }

		reserved2: SInt32;
		reserved3: SInt32;

		layer: SInt16;
		alternateGroup: SInt16;

		volume: SInt16;
		reserved4: SInt16;

		matrix: MatrixRecord;
		trackWidth: Fixed;
		trackHeight: Fixed;
	end;
type
	TrackHeaderAtomPtr = ^TrackHeaderAtom;
	TrackHeaderAtom = record
		size: SInt32;                   { size of track header }
		atomType: SInt32;               { = 'tkhd' }

		header: TrackHeader;
	end;
type
	EditListTypePtr = ^EditListType;
	EditListType = record
		trackDuration: TimeValue;
		mediaTime: TimeValue;
		mediaRate: Fixed;
	end;
type
	EditListAtomPtr = ^EditListAtom;
	EditListAtom = record
		size: SInt32;
		atomType: SInt32;               { = elst }

		flags: SInt32;                  { 1 byte of version / 3 bytes of flags }

		numEntries: SInt32;
editListTable: array [0..0] of EditListType;
	end;
type
	EditsAtomPtr = ^EditsAtom;
	EditsAtom = record
		size: SInt32;
		atomType: SInt32;               { = edts }

		editList: EditListAtom;
	end;
type
	TrackLoadSettingsPtr = ^TrackLoadSettings;
	TrackLoadSettings = record
		preloadStartTime: TimeValue;
		preloadDuration: TimeValue;
		preloadFlags: SInt32;
		defaultHints: SInt32;
	end;
type
	TrackLoadSettingsAtomPtr = ^TrackLoadSettingsAtom;
	TrackLoadSettingsAtom = record
		size: SInt32;
		atomType: SInt32;               { = load }

		settings: TrackLoadSettings;
	end;
type
	TrackCleanApertureDimensionsPtr = ^TrackCleanApertureDimensions;
	TrackCleanApertureDimensions = record
		flags: SInt32;                  { 1 byte of version / 3 bytes of flags }
		cleanApertureDimensions: FixedPoint;
	end;
type
	TrackCleanApertureDimensionsAtomPtr = ^TrackCleanApertureDimensionsAtom;
	TrackCleanApertureDimensionsAtom = record
		size: SInt32;
		atomType: SInt32;               { = 'tapt' }

		cleanApertureDimensions: TrackCleanApertureDimensions;
	end;
type
	TrackProductionApertureDimensionsPtr = ^TrackProductionApertureDimensions;
	TrackProductionApertureDimensions = record
		flags: SInt32;                  { 1 byte of version / 3 bytes of flags }
		productionApertureDimensions: FixedPoint;
	end;
type
	TrackProductionApertureDimensionsAtomPtr = ^TrackProductionApertureDimensionsAtom;
	TrackProductionApertureDimensionsAtom = record
		size: SInt32;
		atomType: SInt32;               { = 'prof' }

		productionApertureDimensions: TrackProductionApertureDimensions;
	end;
type
	TrackEncodedPixelsDimensionsPtr = ^TrackEncodedPixelsDimensions;
	TrackEncodedPixelsDimensions = record
		flags: SInt32;                  { 1 byte of version / 3 bytes of flags }
		encodedPixelsDimensions: FixedPoint;
	end;
type
	TrackEncodedPixelsDimensionsAtomPtr = ^TrackEncodedPixelsDimensionsAtom;
	TrackEncodedPixelsDimensionsAtom = record
		size: SInt32;
		atomType: SInt32;               { = 'enof' }

		encodedPixelsDimensions: TrackEncodedPixelsDimensions;
	end;
type
	TrackDirectoryPtr = ^TrackDirectory;
	TrackDirectory = record
		size: SInt32;
		atomType: SInt32;               { = 'trak' }

		trackHeader: TrackHeaderAtom;            { standard track information }

		trackClip: ClippingAtom;

		edits: EditsAtom;

		media: MediaDirectory;

		userData: UserDataAtom;               { space for extending with new data types }
	end;
	TrackDirectory_fix = TrackDirectory;
{***************************************
*
*   MovieDirectory -
*       The MovieDirectory is the top level structure which
*       holds the TrackInstance describing where the
*       TrackDirectories are.
*
***************************************}
type
	MovieHeaderPtr = ^MovieHeader;
	MovieHeader = record
		flags: SInt32;                  { 1 byte of version / 3 bytes of flags }

		creationTime: SInt32;           { seconds since Jan 1904 when directory was created }
		modificationTime: SInt32;       { seconds since Jan 1904 when directory was appended }

		timeScale: TimeValue;              { Time specifications }
		duration: TimeValue;
		preferredRate: Fixed;          { rate at which to play this movie }

		preferredVolume: SInt16;        { volume to play movie at }
		reserved1: SInt16;

		preferredLong1: SInt32;
		preferredLong2: SInt32;

		matrix: MatrixRecord;

		previewTime: TimeValue;            { time in track the proxy begins (track time) }
		previewDuration: TimeValue;        { how long the proxy lasts (track time) }

		posterTime: TimeValue;             { time in track the proxy begins (track time) }

		selectionTime: TimeValue;          { time in track the proxy begins (track time) }
		selectionDuration: TimeValue;      { time in track the proxy begins (track time) }
		currentTime: TimeValue;            { time in track the proxy begins (track time) }

		nextTrackID: SInt32;            { next value to use for a TrackID }
	end;
type
	MovieHeaderAtomPtr = ^MovieHeaderAtom;
	MovieHeaderAtom = record
		size: SInt32;
		atomType: SInt32;               { = 'mvhd' }

		header: MovieHeader;
	end;
type
	TrackDirectoryEntryPtr = ^TrackDirectoryEntry;
	TrackDirectoryEntry = record
		trackDirectory: TrackDirectory_fix;         { Track directory information }
	end;
type
	MovieDirectoryPtr = ^MovieDirectory;
	MovieDirectory = record
		size: SInt32;
		atomType: SInt32;               { = 'moov' }

		header: MovieHeaderAtom;

		movieClip: ClippingAtom;

                                              { Track Directories }
		track: array [0..0] of TrackDirectoryEntry;              { Track directory information }

                                              { User data for Movie }
		userData: UserDataAtom;               { space for user extensions }
	end;
{***************************************
***************************************}

{ Movie formats and tags }
const
{ some system defined format IDs }
	QT_MOVIE_TYPE = FourCharCode('moov');
	QT_TRACK_TYPE = FourCharCode('trak');
	QT_MEDIA_TYPE = FourCharCode('mdia');
	QT_VIDEO_TYPE = FourCharCode('vide');
	QT_SOUND_TYPE = FourCharCode('soun');

const
	MOVIE_TYPE = FourCharCode('moov');
	TRACK_TYPE = FourCharCode('trak');
	VIDEO_TYPE = FourCharCode('vide');
	SOUND_TYPE = FourCharCode('soun');

{$ifc not TARGET_OS_WIN32}
{ The name "MEDIA_TYPE" has a name space collision on Win32.}
const
	MEDIA_TYPE = FourCharCode('mdia');

{$endc} {!TARGET_OS_WIN32}

{ atom id's }
const
	MovieAID = FourCharCode('moov');
	MovieHeaderAID = FourCharCode('mvhd');
	ClipAID = FourCharCode('clip');
	RgnClipAID = FourCharCode('crgn');
	MatteAID = FourCharCode('matt');
	MatteCompAID = FourCharCode('kmat');
	TrackAID = FourCharCode('trak');
	UserDataAID = FourCharCode('udta');
	TrackHeaderAID = FourCharCode('tkhd');
	EditsAID = FourCharCode('edts');
	EditListAID = FourCharCode('elst');
	MediaAID = FourCharCode('mdia');
	MediaHeaderAID = FourCharCode('mdhd');
	MediaInfoAID = FourCharCode('minf');
	VideoMediaInfoHeaderAID = FourCharCode('vmhd');
	SoundMediaInfoHeaderAID = FourCharCode('smhd');
	GenericMediaInfoHeaderAID = FourCharCode('gmhd');
	GenericMediaInfoAID = FourCharCode('gmin');
	DataInfoAID = FourCharCode('dinf');
	DataRefAID = FourCharCode('dref');
	SampleTableAID = FourCharCode('stbl');
	STSampleDescAID = FourCharCode('stsd');
	STTimeToSampAID = FourCharCode('stts');
	STSyncSampleAID = FourCharCode('stss');
	STSampleToChunkAID = FourCharCode('stsc');
	STShadowSyncAID = FourCharCode('stsh');
	HandlerAID = FourCharCode('hdlr');
	STSampleSizeAID = FourCharCode('stsz');
	STChunkOffsetAID = FourCharCode('stco');
	STChunkOffset64AID = FourCharCode('co64');
	STSampleIDAID = FourCharCode('stid');
	STCompositionOffsetAID = FourCharCode('ctts');
	STSampleDependencyAID = FourCharCode('sdtp');
	STCompositionShiftLeastGreatestAID = FourCharCode('cslg');
	STPartialSyncSampleAID = FourCharCode('stps');
	DataRefContainerAID = FourCharCode('drfc');
	TrackReferenceAID = FourCharCode('tref');
	ColorTableAID = FourCharCode('ctab');
	LoadSettingsAID = FourCharCode('load');
	PropertyAtomAID = FourCharCode('code');
	InputMapAID = FourCharCode('imap');
	MovieBufferHintsAID = FourCharCode('mbfh');
	MovieDataRefAliasAID = FourCharCode('mdra');
	SoundLocalizationAID = FourCharCode('sloc');
	CompressedMovieAID = FourCharCode('cmov');
	CompressedMovieDataAID = FourCharCode('cmvd');
	DataCompressionAtomAID = FourCharCode('dcom');
	ReferenceMovieRecordAID = FourCharCode('rmra');
	ReferenceMovieDescriptorAID = FourCharCode('rmda');
	ReferenceMovieDataRefAID = FourCharCode('rdrf');
	ReferenceMovieVersionCheckAID = FourCharCode('rmvc');
	ReferenceMovieDataRateAID = FourCharCode('rmdr');
	ReferenceMovieComponentCheckAID = FourCharCode('rmcd');
	ReferenceMovieQualityAID = FourCharCode('rmqu');
	ReferenceMovieLanguageAID = FourCharCode('rmla');
	ReferenceMovieCPURatingAID = FourCharCode('rmcs');
	ReferenceMovieAlternateGroupAID = FourCharCode('rmag');
	ReferenceMovieNetworkStatusAID = FourCharCode('rnet');
	CloneMediaAID = FourCharCode('clon');
	FileTypeAID = FourCharCode('ftyp');
	SecureContentInfoAID = FourCharCode('sinf');
	SecureContentSchemeTypeAID = FourCharCode('schm');
	SecureContentSchemeInfoAID = FourCharCode('schi');
	TrackApertureModeDimensionsAID = FourCharCode('tapt'); { container atom including TrackCleanApertureDimensionsAID, TrackProductionApertureDimensionsAID and TrackEncodedPixelsDimensionsAID }
	TrackCleanApertureDimensionsAID = FourCharCode('clef');
	TrackProductionApertureDimensionsAID = FourCharCode('prof');
	TrackEncodedPixelsDimensionsAID = FourCharCode('enof');

{ Text ATOM definitions}

type
	TextBoxAtomPtr = ^TextBoxAtom;
	TextBoxAtom = record
		size: SInt32;
		atomType: SInt32;               { = 'tbox' }
		textBox: Rect;                { New text box (overrides defaultTextBox)}
	end;
type
	HiliteAtomPtr = ^HiliteAtom;
	HiliteAtom = record
		size: SInt32;
		atomType: SInt32;               { = 'hlit' }
		selStart: SInt32;               { hilite selection start character}
		selEnd: SInt32;                 { hilite selection end character}
	end;
type
	KaraokeRecPtr = ^KaraokeRec;
	KaraokeRec = record
		timeVal: TimeValue;
		beginHilite: SInt16;
		endHilite: SInt16;
	end;
type
	KaraokeAtomPtr = ^KaraokeAtom;
	KaraokeAtom = record
		numEntries: SInt32;
		karaokeEntries: array [0..0] of KaraokeRec;
	end;
{ for ReferenceMovieDataRefRecord.flags}
const
	kDataRefIsSelfContained = 1 shl 0;

type
	ReferenceMovieDataRefRecordPtr = ^ReferenceMovieDataRefRecord;
	ReferenceMovieDataRefRecord = record
		flags: SInt32;
		dataRefType: OSType;
		dataRefSize: SInt32;
		dataRef: array [0..0] of SInt8;
	end;
{ for VersionCheckRecord.checkType}
const
	kVersionCheckMin = 0;    { val1 is the min. version required}
	kVersionCheckMask = 1;     { (gestalt return value & val2) must == val1}

type
	QTAltVersionCheckRecordPtr = ^QTAltVersionCheckRecord;
	QTAltVersionCheckRecord = record
		flags: SInt32;                  { currently always 0}
		gestaltTag: OSType;
		val1: UInt32;
		val2: UInt32;
		checkType: SInt16;
	end;
{ some helpful constants for DataRateRecord.dataRate }
const
	kDataRate144ModemRate = 1400;
	kDataRate288ModemRate = 2800;
	kDataRateISDNRate = 5600;
	kDataRateDualISDNRate = 11200;
	kDataRate256kbpsRate = 25600;
	kDataRate384kbpsRate = 38400;
	kDataRate512kbpsRate = 51200;
	kDataRate768kbpsRate = 76800;
	kDataRate1MbpsRate = 100000;
	kDataRateT1Rate = 150000;
	kDataRateInfiniteRate = $7FFFFFFF;
	kDataRateDefaultIfNotSet = kDataRate512kbpsRate;

type
	QTAltDataRateRecordPtr = ^QTAltDataRateRecord;
	QTAltDataRateRecord = record
		flags: SInt32;                  { currently always 0}
		dataRate: SInt32;
	end;
type
	QTAltComponentCheckRecordPtr = ^QTAltComponentCheckRecord;
	QTAltComponentCheckRecord = record
		flags: SInt32;                  { currently always 0 }
		cd: ComponentDescription;
		minVersion: UInt32;
	end;
type
	QTAltLanguageRecordPtr = ^QTAltLanguageRecord;
	QTAltLanguageRecord = record
		flags: SInt32;                  { currently always 0}
		language: SInt16;
	end;

const
	kQTCPUSpeed1Rating = 100;  { slowest}
	kQTCPUSpeed2Rating = 200;
	kQTCPUSpeed3Rating = 300;
	kQTCPUSpeed4Rating = 400;
	kQTCPUSpeed5Rating = 500;   { fastest}

type
	QTAltCPURatingRecordPtr = ^QTAltCPURatingRecord;
	QTAltCPURatingRecord = record
		flags: UInt32;                  { currently always 0}
		speed: UInt16;
	end;
type
	ReferenceMovieNetworkStatusRecordPtr = ^ReferenceMovieNetworkStatusRecord;
	ReferenceMovieNetworkStatusRecord = record
		flags: UInt32;                  { currently always 0}
		valueCount: UInt32;             { how many status values are in array}
		netStatusValues: array [0..0] of SInt32;     { a value from kQTNetworkStatus... constants}
	end;
type
	CloneRecordPtr = ^CloneRecord;
	CloneRecord = record
		flags: SInt32;
		masterTrackID: SInt32;          { track ID of the track we're cloning }
	end;
type
	CloneAtomPtr = ^CloneAtom;
	CloneAtom = record
		size: SInt32;
		atomType: SInt32;               { = clon }

		cloneInfo: CloneRecord;
	end;
type
	FileTypeAtomPtr = ^FileTypeAtom;
	FileTypeAtom = record
		size: SInt32;
		atomType: SInt32;               { = 'ftyp' }
		majorBrand: SInt32;             { best use brand }
		minorVersion: SInt32;
		compatibleBrands: array [0..3] of SInt32;    { 1 or greater }
	end;
const
	kQTFileTypeBrandQuickTimeMovie = FourCharCode('qt  '); { QuickTime movie files}
	kQTFileTypeBrandISOFile = FourCharCode('isom'); { ISO Base Media files}
	kQTFileTypeBrandMPEG4v1 = FourCharCode('mp41'); { MPEG-4 (ISO/IEC 14496-1) version 1 files}
	kQTFileTypeBrandMPEG4v2 = FourCharCode('mp42'); { MPEG-4 (ISO/IEC 14496-1) version 2 files}

type
	SecureContentInfoAtomPtr = ^SecureContentInfoAtom;
	SecureContentInfoAtom = record
		size: SInt32;
		atomType: SInt32;               { = 'sinf' }
	end;
type
	SecureContentSchemeTypeAtomPtr = ^SecureContentSchemeTypeAtom;
	SecureContentSchemeTypeAtom = record
		size: SInt32;
		atomType: SInt32;               { = 'schm' }

		flags: SInt32;                  { 1 byte of version / 3 bytes of flags }

		schemeType: SInt32;
		schemeVersion: UInt32;
                                              { if flags & 1, C string holding URL for security component server}
	end;
type
	SecureContentSchemeInfoAtomPtr = ^SecureContentSchemeInfoAtom;
	SecureContentSchemeInfoAtom = record
		size: SInt32;
		atomType: SInt32;               { = 'schi' }
	end;

{$endc} {TARGET_OS_MAC}

{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}

end.
{$endc} {not MACOSALLINCLUDE}
