{!
	@file		AudioFile.h
	@framework	AudioToolbox.framework
	@copyright	(c) 1985-2015 by Apple, Inc., all rights reserved.
	@abstract	API's to read and write audio files in the filesystem or in memory.
}
{  Pascal Translation: Jonas Maebe <jonas@freepascal.org>, July 2019 }
{
    Modified for use with Free Pascal
    Version 308
    Please report any bugs to <gpc@microbizz.nl>
}

{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}
{$mode macpas}
{$modeswitch cblocks}
{$packenum 1}
{$macro on}
{$inline on}
{$calling mwpascal}

unit AudioFile;
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
{$ifc defined iphonesim}
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
{$ifc defined iphonesim}
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
{$ifc defined ios}
	{$setc TARGET_OS_MAC := FALSE}
	{$setc TARGET_OS_IPHONE := TRUE}
	{$setc TARGET_OS_EMBEDDED := TRUE}
{$elsec}
	{$setc TARGET_OS_MAC := TRUE}
	{$setc TARGET_OS_IPHONE := FALSE}
	{$setc TARGET_OS_EMBEDDED := FALSE}
{$endc}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
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
uses MacTypes,CFBase,CoreAudioTypes,CoreFoundation;
{$endc} {not MACOSALLINCLUDE}

{$ALIGN POWER}


//=============================================================================
//	Includes
//=============================================================================


//CF_ASSUME_NONNULL_BEGIN


{!
	@typedef	AudioFileTypeID
	@abstract	Identifier for an audio file type.
}
type
	AudioFileTypeID = UInt32;

{!
    @enum Audio File Types
    @abstract   Constants for the built-in audio file types.
    @discussion These constants are used to indicate the type of file to be written, or as a hint to
					what type of file to expect from data provided.
    @constant   kAudioFileAIFFType
					Audio Interchange File Format (AIFF)
    @constant   kAudioFileAIFCType
					Audio Interchange File Format Compressed (AIFF-C)
    @constant   kAudioFileWAVEType
					Microsoft WAVE
    @constant   kAudioFileRF64Type
                    File Format specified in EBU Tech 3306
    @constant   kAudioFileSoundDesigner2Type
					Sound Designer II
    @constant   kAudioFileNextType
					NeXT / Sun
    @constant   kAudioFileMP3Type
					MPEG Audio Layer 3 (.mp3)
    @constant   kAudioFileMP2Type
					MPEG Audio Layer 2 (.mp2)
    @constant   kAudioFileMP1Type
					MPEG Audio Layer 1 (.mp1)
    @constant   kAudioFileAC3Type
					AC-3
    @constant   kAudioFileAAC_ADTSType
					Advanced Audio Coding (AAC) Audio Data Transport Stream (ADTS)
    @constant   kAudioFileMPEG4Type
    @constant   kAudioFileM4AType
    @constant   kAudioFileM4BType
    @constant   kAudioFileCAFType
                    CoreAudio File Format
    @constant   kAudioFile3GPType
    @constant   kAudioFile3GP2Type
    @constant   kAudioFileAMRType
    @constant   kAudioFileFLACType
                    Free Lossless Audio Codec
}
const
	kAudioFileAIFFType = FourCharCode('AIFF');
	kAudioFileAIFCType = FourCharCode('AIFC');
	kAudioFileWAVEType = FourCharCode('WAVE');
	kAudioFileRF64Type = FourCharCode('RF64');
	kAudioFileSoundDesigner2Type = FourCharCode('Sd2f');
	kAudioFileNextType = FourCharCode('NeXT');
	kAudioFileMP3Type = FourCharCode('MPG3'); 	// mpeg layer 3
	kAudioFileMP2Type = FourCharCode('MPG2'); 	// mpeg layer 2
	kAudioFileMP1Type = FourCharCode('MPG1'); 	// mpeg layer 1
	kAudioFileAC3Type = FourCharCode('ac-3');
	kAudioFileAAC_ADTSType = FourCharCode('adts');
	kAudioFileMPEG4Type = FourCharCode('mp4f');
	kAudioFileM4AType = FourCharCode('m4af');
	kAudioFileM4BType = FourCharCode('m4bf');
	kAudioFileCAFType = FourCharCode('caff');
	kAudioFile3GPType = FourCharCode('3gpp');
	kAudioFile3GP2Type = FourCharCode('3gp2');
	kAudioFileAMRType = FourCharCode('amrf');
	kAudioFileFLACType = FourCharCode('flac'); 

{!
    @enum AudioFile error codes
    @abstract   These are the error codes returned from the AudioFile API.
    @constant   kAudioFileUnspecifiedError 
		An unspecified error has occurred.
    @constant   kAudioFileUnsupportedFileTypeError 
		The file type is not supported.
    @constant   kAudioFileUnsupportedDataFormatError 
		The data format is not supported by this file type.
    @constant   kAudioFileUnsupportedPropertyError 
		The property is not supported.
    @constant   kAudioFileBadPropertySizeError 
		The size of the property data was not correct.
    @constant   kAudioFilePermissionsError 
		The operation violated the file permissions. For example, trying to write to a file opened with kAudioFileReadPermission.
    @constant   kAudioFileNotOptimizedError 
		There are chunks following the audio data chunk that prevent extending the audio data chunk. 
		The file must be optimized in order to write more audio data.
    @constant   kAudioFileInvalidChunkError 
		The chunk does not exist in the file or is not supported by the file. 
    @constant   kAudioFileDoesNotAllow64BitDataSizeError 
		The a file offset was too large for the file type. AIFF and WAVE have a 32 bit file size limit. 
    @constant   kAudioFileInvalidPacketOffsetError 
		A packet offset was past the end of the file, or not at the end of the file when writing a VBR format, 
		or a corrupt packet size was read when building the packet table. 
    @constant   kAudioFileInvalidFileError 
		The file is malformed, or otherwise not a valid instance of an audio file of its type. 
    @constant   kAudioFileOperationNotSupportedError 
		The operation cannot be performed. For example, setting kAudioFilePropertyAudioDataByteCount to increase 
		the size of the audio data in a file is not a supported operation. Write the data instead.
    @constant   kAudioFileNotOpenError 
		The file is closed.
	@constant   kAudioFileEndOfFileError 
		End of file.
	@constant   kAudioFilePositionError 
		Invalid file position.
	@constant   kAudioFileFileNotFoundError 
		File not found.
 }
const
	kAudioFileUnspecifiedError = FourCharCode('wht?'); 		// 0x7768743F, 2003334207
	kAudioFileUnsupportedFileTypeError = FourCharCode('typ?'); 		// 0x7479703F, 1954115647
	kAudioFileUnsupportedDataFormatError = FourCharCode('fmt?'); 		// 0x666D743F, 1718449215
	kAudioFileUnsupportedPropertyError = FourCharCode('pty?'); 		// 0x7074793F, 1886681407
	kAudioFileBadPropertySizeError = FourCharCode('!siz'); 		// 0x2173697A,  561211770
	kAudioFilePermissionsError = FourCharCode('prm?'); 		// 0x70726D3F, 1886547263
	kAudioFileNotOptimizedError = FourCharCode('optm'); 		// 0x6F70746D, 1869640813
        // file format specific error codes
	kAudioFileInvalidChunkError = FourCharCode('chk?'); 		// 0x63686B3F, 1667787583
	kAudioFileDoesNotAllow64BitDataSizeError = FourCharCode('off?'); 		// 0x6F66663F, 1868981823
	kAudioFileInvalidPacketOffsetError = FourCharCode('pck?'); 		// 0x70636B3F, 1885563711
	kAudioFileInvalidFileError = FourCharCode('dta?'); 		// 0x6474613F, 1685348671
	kAudioFileOperationNotSupportedError = $6F703F3F;  	// 'op??', integer used because of trigraph
		// general file error codes
	kAudioFileNotOpenError = -38;
	kAudioFileEndOfFileError = -39;
	kAudioFilePositionError = -40;
	kAudioFileFileNotFoundError = -43; 

{!
    @enum AudioFileFlags
    @abstract   These are flags that can be used with the CreateURL API call
    @constant   kAudioFileFlags_EraseFile 
		If set, then the CreateURL call will erase the contents of an existing file
		If not set, then the CreateURL call will fail if the file already exists
    @constant   kAudioFileFlags_DontPageAlignAudioData 
		Normally, newly created and optimized files will have padding added in order to page align 
		the data to 4KB boundaries. This makes reading the data more efficient. 
		When disk space is a concern, this flag can be set so that the padding will not be added.
}
type
	AudioFileFlags = UInt32;
const
	kAudioFileFlags_EraseFile = 1;
	kAudioFileFlags_DontPageAlignAudioData = 2; 

type
	AudioFilePermissions = SInt8;
const
	kAudioFileReadPermission = $01;
	kAudioFileWritePermission = $02;
	kAudioFileReadWritePermission = $03; 

//=============================================================================
//	Types specific to the Audio File API
//=============================================================================

{!
    @typedef	AudioFileID
    @abstract   An opaque reference to an AudioFile object.
}
type
	OpaqueAudioFileID = record end;
	AudioFileID = ^OpaqueAudioFileID;
{!
    @typedef	AudioFilePropertyID
    @abstract   A constant for an AudioFile property.
}
type
	AudioFilePropertyID = UInt32;

{!
    @enum		AudioFileLoopDirection
    @abstract   These constants describe the playback direction of a looped segment of a file.
    @constant   kAudioFileLoopDirection_NoLooping
					The segment is not looped.
    @constant   kAudioFileLoopDirection_Forward
					play segment forward.
    @constant   kAudioFileLoopDirection_Backward
					play segment backward.
    @constant   kAudioFileLoopDirection_ForwardAndBackward
					play segment forward and backward.
}
const
	kAudioFileLoopDirection_NoLooping = 0;
	kAudioFileLoopDirection_Forward = 1;
	kAudioFileLoopDirection_ForwardAndBackward = 2;
	kAudioFileLoopDirection_Backward = 3; 

//=============================================================================
//	Markers, Regions
//=============================================================================


{!
    @struct		AudioFile_SMPTE_Time
    @abstract   A struct for describing a SMPTE time.
    @field      mHours						The hours.
    @field      mMinutes					The minutes.
    @field      mSeconds					The seconds.
    @field      mFrames						The frames.
    @field      mSubFrameSampleOffset		The sample offset within a frame.
}
type
	AudioFile_SMPTE_Time = record
		mHours: SInt8;
		mMinutes: UInt8;
		mSeconds: UInt8;
		mFrames: UInt8;
		mSubFrameSampleOffset: UInt32;
	end;


{!
    @enum		AudioFileMarkerType
    @abstract   constants for types of markers within a file. Used in the mType field of AudioFileMarker.
    @constant   kAudioFileMarkerType_Generic		A generic marker. See CAFFile.h for marker types specific to CAF files.
}
const
	kAudioFileMarkerType_Generic = 0; 


{!
    @struct		AudioFileMarker
    @abstract   A marker annotates a position in an audio file with additional information.
    @discussion (description)
    @field      mFramePosition	The frame in the file counting from the start of the audio data.
    @field      mName			The name of this marker.
    @field      mMarkerID		A unique ID for this marker.
    @field      mSMPTETime		The SMPTE time for this marker.
    @field      mType			The marker type.
    @field      mReserved		A reserved field. Set to zero.
    @field      mChannel		The channel number that the marker refers to. Set to zero if marker applies to all channels.
}
type
	AudioFileMarker = record
		mFramePosition: Float64;
	
		mName: CFStringRef	{__nullable};
		mMarkerID: SInt32;

		mSMPTETime: AudioFile_SMPTE_Time;
		mType: UInt32;
		mReserved: UInt16;
		mChannel: UInt16;
	end;
	AudioFileMarkerPtr = ^AudioFileMarker;

{!
    @struct		AudioFileMarkerList
    @abstract   A list of AudioFileMarker.
    @field      mSMPTE_TimeType
					This defines the SMPTE timing scheme used in the marker list. See CAFFile.h for the values used here.
    @field      mNumberMarkers
					The number of markers in the mMarkers list.
    @field      mMarkers
					A list of AudioFileMarker.
}
type
	AudioFileMarkerList = record
		mSMPTE_TimeType: UInt32;
		mNumberMarkers: UInt32;
		mMarkers: array [0..1-1] of AudioFileMarker; // this is a variable length array of mNumberMarkers elements
	end;
	AudioFileMarkerListPtr = ^AudioFileMarkerList;

{!
    @function	NumBytesToNumAudioFileMarkers
    @abstract   Converts a size in bytes to the number of AudioFileMarkers that can be contained in that number of bytes.
    @discussion This can be used for the kAudioFilePropertyMarkerList property when calculating the number of 
				markers that will be returned. 
    @param      inNumBytes 
					a number of bytes.
    @result     the number of AudioFileMarkers that can be contained in that number of bytes.
}

function NumBytesToNumAudioFileMarkers(inNumBytes: size_t): size_t;
{FPC-ONLY-START}
inline;
{FPC-ONLY-END}

{!
    @function	NumAudioFileMarkersToNumBytes
    @abstract   Converts a number of AudioFileMarkers to a size in bytes.
    @discussion This can be used for the kAudioFilePropertyMarkerList property when calculating the size required to 
				contain a number of AudioFileMarkers. 
    @param      inNumMarkers 
					a number of AudioFileMarkers.
    @result     the size in bytes required to contain that number of AudioFileMarkers.
}

function NumAudioFileMarkersToNumBytes(inNumMarkers: size_t): size_t;
{FPC-ONLY-START}
inline;
{FPC-ONLY-END}

{!
    @enum		AudioFileRegionFlags
    @abstract   These are flags for an AudioFileRegion that specify a playback direction.
    @discussion One or multiple of these flags can be set. For example, if both kAudioFileRegionFlag_LoopEnable and 
				kAudioFileRegionFlag_PlayForward are set, then the region will play as a forward loop. If only 
				kAudioFileRegionFlag_PlayForward is set, then the region will be played forward once.
    @constant   kAudioFileRegionFlag_LoopEnable
					If this flag is set, the region will be looped. One or both of the following must also be set.
    @constant   kAudioFileRegionFlag_PlayForward
					If this flag is set, the region will be played forward.
    @constant   kAudioFileRegionFlag_PlayBackward
					If this flag is set, the region will be played backward.
}
type
	AudioFileRegionFlags = UInt32;
const
	kAudioFileRegionFlag_LoopEnable = 1;
	kAudioFileRegionFlag_PlayForward = 2;
	kAudioFileRegionFlag_PlayBackward = 4; 

{!
    @struct		AudioFileRegion
    @abstract   An AudioFileRegion specifies a segment of audio data.
    @discussion Generally a region consists of at least two markers marking the beginning and end of the segment.
				There may also be other markers defining other meta information such as sync point.
    @field      mRegionID 
					each region must have a unique ID.
    @field      mName 
					The name of the region.
    @field      mFlags 
					AudioFileRegionFlags.
    @field      mNumberMarkers 
					The number of markers in the mMarkers array.
    @field      mMarkers 
					A variable length array of AudioFileMarkers.
}
type
	AudioFileRegion = record
		mRegionID: UInt32;
		mName: CFStringRef;
		mFlags: AudioFileRegionFlags;
		mNumberMarkers: UInt32;
		mMarkers: array [0..1-1] of AudioFileMarker; // this is a variable length array of mNumberMarkers elements
	end;
	AudioFileRegionPtr = ^AudioFileRegion;


{!
    @struct		AudioFileRegionList
    @abstract   A list of the AudioFileRegions in a file.
    @discussion This is the struct used by the kAudioFilePropertyRegionList property.
    @field      mSMPTE_TimeType
					This defines the SMPTE timing scheme used in the file. See CAFFile.h for the values used here.
    @field      mNumberRegions
					The number of regions in the mRegions list.
    @field      mRegions
					A list of AudioFileRegions. Note that AudioFileMarkers are variable length, so this list cannot 
					be accessed as an array. Use the NextAudioFileRegion macro for traversing the list instead.
}
type
	AudioFileRegionList = record
		mSMPTE_TimeType: UInt32;
		mNumberRegions: UInt32;
		mRegions: array [0..1-1] of AudioFileRegion; // this is a variable length array of mNumberRegions elements
	end;
	AudioFileRegionListPtr = ^AudioFileRegionList;

{!
    @function	NextAudioFileRegion
    @abstract   convenience macro for traversing the region list.
    @discussion because AudioFileRegions are variable length, you cannot access them as an array. Use NextAudioFileRegion 
				to walk the list.
    @param      inAFRegionPtr 
					a pointer to the current region.
    @result     a pointer to the region after the current region. This does not protect you from walking off the end of the list, 
				so obey mNumberRegions.
}

function NextAudioFileRegion(const {var} inAFRegionPtr: AudioFileRegion): AudioFileRegionPtr;
{FPC-ONLY-START}
inline;
{FPC-ONLY-END}

{!
    @struct		AudioFramePacketTranslation
    @abstract   used for properties kAudioFilePropertyPacketToFrame and kAudioFilePropertyFrameToPacket
    @discussion See description of kAudioFilePropertyPacketToFrame and kAudioFilePropertyFrameToPacket
    @field      mFrame		a frame number.
    @field      mPacket		a packet number.
    @field      mFrameOffsetInPacket		a frame offset in a packet.
}
type
	AudioFramePacketTranslation = record
		mFrame: SInt64;
		mPacket: SInt64;
		mFrameOffsetInPacket: UInt32;
	end;
	AudioFramePacketTranslationPtr = ^AudioFramePacketTranslation;


{!
    @enum		AudioBytePacketTranslation Flags
	
    @abstract   flags for the AudioBytePacketTranslation mFlags field
    @discussion		There is currently only one flag.
					
    @constant   kBytePacketTranslationFlag_IsEstimate
					If the set then the result value is an estimate.
}
type
	AudioBytePacketTranslationFlags = UInt32;
const
	kBytePacketTranslationFlag_IsEstimate = 1; 

{!
    @struct		AudioBytePacketTranslation
    @abstract   used for properties kAudioFileByteToPacket and kAudioFilePacketToByte
    @discussion See description of kAudioFileByteToPacket and kAudioFilePacketToByte
    @field      mByte		a byte number.
    @field      mPacket		a packet number.
    @field      mByteOffsetInPacket		a byte offset in a packet.
    @field      mFlags		if kBytePacketTranslationFlag_IsEstimate is set, then the value is an estimate.
}
type
	AudioBytePacketTranslation = record
		mByte: SInt64;
		mPacket: SInt64;
		mByteOffsetInPacket: UInt32;
		mFlags: AudioBytePacketTranslationFlags;
	end;
	AudioBytePacketTranslationPtr = ^AudioBytePacketTranslation;


{!
    @struct		AudioFilePacketTableInfo
    @abstract   This contains information about the number of valid frames in a file and where they begin and end.
    @discussion	Some data formats may have packets whose contents are not completely valid, but represent priming or remainder 
				frames that are not meant to be played. For example a file with 100 packets of AAC is nominally 1024 * 100 = 102400 frames
				of data. However the first 2112 frames of that may be priming frames and there may be some 
				number of remainder frames added to pad out to a full packet of 1024 frames. The priming and remainder frames should be 
				discarded. The total number of packets in the file times the frames per packet (or counting each packet's frames 
				individually for a variable frames per packet format) minus mPrimingFrames, minus mRemainderFrames, should 
				equal mNumberValidFrames.
    @field      mNumberValidFrames the number of valid frames in the file.
    @field      mPrimingFrames the number of invalid frames at the beginning of the file.
    @field      mRemainderFrames the number of invalid frames at the end of the file.
}
type
	AudioFilePacketTableInfo = record
		mNumberValidFrames: SInt64;
		mPrimingFrames: SInt32;
		mRemainderFrames: SInt32;
	end;
	AudioFilePacketTableInfoPtr = ^AudioFilePacketTableInfo;

//=============================================================================
//	Info String Keys
//=============================================================================

// Get key values from the InfoDictionary by making CFStrings from the following constants

const
	kAFInfoDictionary_Album = 'album';
const
	kAFInfoDictionary_ApproximateDurationInSeconds = 'approximate duration in seconds';
const
	kAFInfoDictionary_Artist = 'artist';
const
	kAFInfoDictionary_ChannelLayout = 'channel layout';
const
	kAFInfoDictionary_Comments = 'comments';
const
	kAFInfoDictionary_Composer = 'composer';
const
	kAFInfoDictionary_Copyright = 'copyright';
const
	kAFInfoDictionary_EncodingApplication = 'encoding application';
const
	kAFInfoDictionary_Genre = 'genre';
const
	kAFInfoDictionary_ISRC = 'ISRC';  // International Standard Recording Code
const
	kAFInfoDictionary_KeySignature = 'key signature';
const
	kAFInfoDictionary_Lyricist = 'lyricist';
const
	kAFInfoDictionary_NominalBitRate = 'nominal bit rate';
const
	kAFInfoDictionary_RecordedDate = 'recorded date';
const
	kAFInfoDictionary_SourceBitDepth = 'source bit depth';
const
	kAFInfoDictionary_SourceEncoder = 'source encoder';
const
	kAFInfoDictionary_SubTitle = 'subtitle';
const
	kAFInfoDictionary_Tempo = 'tempo';
const
	kAFInfoDictionary_TimeSignature = 'time signature';
const
	kAFInfoDictionary_Title = 'title';
const
	kAFInfoDictionary_TrackNumber = 'track number';
const
	kAFInfoDictionary_Year = 'year';

//=============================================================================
//	Routines
//=============================================================================

{!
    @function	AudioFileCreateWithURL
    @abstract   creates a new audio file (or initialises an existing file)
    @discussion	creates a new (or initialises an existing) audio file specified by the URL.
					Upon success, an AudioFileID is returned which can be used for subsequent calls 
					to the AudioFile APIs.
    @param inFileRef		an CFURLRef fully specifying the path of the file to create/initialise
    @param inFileType		an AudioFileTypeID indicating the type of audio file to create.
    @param inFormat			an AudioStreamBasicDescription describing the data format that will be
							added to the audio file.
    @param inFlags			relevant flags for creating/opening the file. 
								if kAudioFileFlags_EraseFile is set, it will erase an existing file
								 if not set, then the Create call will fail if the URL is an existing file
    @param outAudioFile		if successful, an AudioFileID that can be used for subsequent AudioFile calls.
    @result					returns noErr if successful.
}
function AudioFileCreateWithURL( inFileRef: CFURLRef; inFileType: AudioFileTypeID; const (*var*) inFormat: AudioStreamBasicDescription; inFlags: AudioFileFlags; var outAudioFile: AudioFileID {__nullable * __nonnull} ): OSStatus; external name '_AudioFileCreateWithURL';
(* API_AVAILABLE(macos(10.5), ios(2.0), watchos(2.0), tvos(9.0)) *)

{!
    @function				AudioFileOpenURL
    @abstract				Open an existing audio file.
    @discussion				Open an existing audio file for reading or reading and writing.
    @param inFileRef		the CFURLRef of an existing audio file.
    @param inPermissions	use the permission constants
    @param inFileTypeHint	For files which have no filename extension and whose type cannot be easily or
							uniquely determined from the data (ADTS,AC3), this hint can be used to indicate the file type. 
							Otherwise you can pass zero for this. The hint is only used on OS versions 10.3.1 or greater.
							For OS versions prior to that, opening files of the above description will fail.
    @param outAudioFile		upon success, an AudioFileID that can be used for subsequent
							AudioFile calls.
    @result					returns noErr if successful.
}
function AudioFileOpenURL( inFileRef: CFURLRef; inPermissions: AudioFilePermissions; inFileTypeHint: AudioFileTypeID; var outAudioFile: AudioFileID {__nullable * __nonnull} ): OSStatus; external name '_AudioFileOpenURL';
(* API_AVAILABLE(macos(10.5), ios(2.0), watchos(2.0), tvos(9.0)) *)

{!
    @typedef	AudioFile_ReadProc
    @abstract   A callback for reading data. used with AudioFileOpenWithCallbacks or AudioFileInitializeWithCallbacks.
    @discussion a function that will be called when AudioFile needs to read data.
    @param      inClientData	A pointer to the client data as set in the inClientData parameter to AudioFileXXXWithCallbacks.
    @param      inPosition		An offset into the data from which to read.
    @param      requestCount	The number of bytes to read.
    @param      buffer			The buffer in which to put the data read.
    @param      actualCount		The callback should set this to the number of bytes successfully read.
    @result						The callback should return noErr on success, or an appropriate error code on failure.
}
type
	AudioFile_ReadProc = function( inClientData: UnivPtr; inPosition: SInt64; requestCount: UInt32; buffer: UnivPtr; var actualCount: UInt32 ): OSStatus;

{!
    @typedef	AudioFile_WriteProc
    @abstract   A callback for writing data. used with AudioFileOpenWithCallbacks or AudioFileInitializeWithCallbacks.
    @discussion a function that will be called when AudioFile needs to write data.
    @param      inClientData	A pointer to the client data as set in the inClientData parameter to AudioFileXXXWithCallbacks.
    @param      inPosition		An offset into the data from which to read.
    @param      requestCount	The number of bytes to write.
    @param      buffer			The buffer containing the data to write.
    @param      actualCount		The callback should set this to the number of bytes successfully written.
    @result						The callback should return noErr on success, or an appropriate error code on failure.
}
type
	AudioFile_WriteProc = function( inClientData: UnivPtr; inPosition: SInt64; requestCount: UInt32; buffer: {const} UnivPtr; var actualCount: UInt32 ): OSStatus;
								
{!
    @typedef	AudioFile_GetSizeProc
    @abstract   A callback for getting the size of the file data. used with AudioFileOpenWithCallbacks or AudioFileInitializeWithCallbacks.
    @discussion a function that will be called when AudioFile needs to determine the size of the file data. This size is for all of the 
				data in the file, not just the audio data.
    @param      inClientData	A pointer to the client data as set in the inClientData parameter to AudioFileXXXWithCallbacks.
    @result						The callback should return the size of the data.
}
type
	AudioFile_GetSizeProc = function( inClientData: UnivPtr ): SInt64;

{!
    @typedef	AudioFile_SetSizeProc
    @abstract   A callback for setting the size of the file data. used with AudioFileOpenWithCallbacks or AudioFileInitializeWithCallbacks.
    @discussion a function that will be called when AudioFile needs to set the size of the file data. This size is for all of the 
				data in the file, not just the audio data. This will only be called if the file is written to.
    @param      inClientData	A pointer to the client data as set in the inClientData parameter to AudioFileXXXWithCallbacks.
    @result						The callback should return the size of the data.
}
type
	AudioFile_SetSizeProc = function( inClientData: UnivPtr; inSize: SInt64 ): OSStatus;

{!
    @function	AudioFileInitializeWithCallbacks
    @abstract   Wipe clean an existing file. You provide callbacks that the AudioFile API
				will use to get the data.
    @param inClientData		a constant that will be passed to your callbacks.
	@param inReadFunc		a function that will be called when AudioFile needs to read data.
	@param inWriteFunc		a function that will be called when AudioFile needs to write data.
	@param inGetSizeFunc	a function that will be called when AudioFile needs to know the file size.
	@param inSetSizeFunc	a function that will be called when AudioFile needs to set the file size.
	
    @param inFileType 		an AudioFileTypeID indicating the type of audio file to which to initialize the file. 
    @param inFormat 		an AudioStreamBasicDescription describing the data format that will be
							added to the audio file.
    @param inFlags			flags for creating/opening the file. Currently zero.
    @param outAudioFile		upon success, an AudioFileID that can be used for subsequent
							AudioFile calls.
    @result					returns noErr if successful.
}
function AudioFileInitializeWithCallbacks( inClientData: UnivPtr; inReadFunc: AudioFile_ReadProc; inWriteFunc: AudioFile_WriteProc; inGetSizeFunc: AudioFile_GetSizeProc; inSetSizeFunc: AudioFile_SetSizeProc; inFileType: AudioFileTypeID; const (*var*) inFormat: AudioStreamBasicDescription; inFlags: AudioFileFlags; var outAudioFile: AudioFileID {__nullable * __nonnull} ): OSStatus; external name '_AudioFileInitializeWithCallbacks';
(* API_AVAILABLE(macos(10.3), ios(2.0), watchos(2.0), tvos(9.0)) *)


{!
    @function	AudioFileOpenWithCallbacks
    @abstract   Open an existing file. You provide callbacks that the AudioFile API
				will use to get the data.
    @param inClientData					a constant that will be passed to your callbacks.
	@param inReadFunc					a function that will be called when AudioFile needs to read data.
	@param inWriteFunc					a function that will be called when AudioFile needs to write data.
	@param inGetSizeFunc				a function that will be called when AudioFile needs to know the total file size.
	@param inSetSizeFunc				a function that will be called when AudioFile needs to set the file size.
	
    @param inFileTypeHint	For files which have no filename extension and whose type cannot be easily or
							uniquely determined from the data (ADTS,AC3), this hint can be used to indicate the file type. 
							Otherwise you can pass zero for this. The hint is only used on OS versions 10.3.1 or greater.
							For OS versions prior to that, opening files of the above description will fail.
    @param outAudioFile		upon success, an AudioFileID that can be used for subsequent
							AudioFile calls.
    @result					returns noErr if successful.
}
function AudioFileOpenWithCallbacks( inClientData: UnivPtr; inReadFunc: AudioFile_ReadProc; inWriteFunc: AudioFile_WriteProc {__nullable}; inGetSizeFunc: AudioFile_GetSizeProc; inSetSizeFunc: AudioFile_SetSizeProc {__nullable}; inFileTypeHint: AudioFileTypeID; var outAudioFile: AudioFileID {__nullable * __nonnull} ): OSStatus; external name '_AudioFileOpenWithCallbacks';
(* API_AVAILABLE(macos(10.3), ios(2.0), watchos(2.0), tvos(9.0)) *)
				

{!
    @function	AudioFileClose
    @abstract   Close an existing audio file.
    @param      inAudioFile		an AudioFileID.
    @result						returns noErr if successful.
}
function AudioFileClose( inAudioFile: AudioFileID ): OSStatus; external name '_AudioFileClose';
(* API_AVAILABLE(macos(10.2), ios(2.0), watchos(2.0), tvos(9.0)) *)

{!
    @function	AudioFileOptimize
    @abstract   Move the audio data to the end of the file and other internal optimizations of the file structure.
	@discussion			Optimize the file so additional audio data can be appended to 
                        the existing data. Generally, this will place the audio data at 
                        the end of the file so additional writes can be placed to the 
                        file end. This can be a potentially expensive and time-consuming operation 
                        and should not be used during time critical operations. There is 
                        a kAudioFilePropertyIsOptimized property for checking on the optimized state 
                        of the file.
    @param      inAudioFile		an AudioFileID.
    @result						returns noErr if successful.
}
function AudioFileOptimize( inAudioFile: AudioFileID ): OSStatus; external name '_AudioFileOptimize';
(* API_AVAILABLE(macos(10.2), ios(2.0), watchos(2.0), tvos(9.0)) *)

{!
    @function	AudioFileReadBytes
    @abstract   Read bytes of audio data from the audio file. 
				
    @discussion				Returns kAudioFileEndOfFileError when read encounters end of file.
    @param inAudioFile		an AudioFileID.
    @param inUseCache 		true if it is desired to cache the data upon read, else false
    @param inStartingByte	the byte offset of the audio data desired to be returned
    @param ioNumBytes 		on input, the number of bytes to read, on output, the number of
							bytes actually read.
    @param outBuffer 		outBuffer should be a void * to user allocated memory large enough for the requested bytes. 
    @result					returns noErr if successful.
}
function AudioFileReadBytes( inAudioFile: AudioFileID; inUseCache: Boolean; inStartingByte: SInt64; var ioNumBytes: UInt32; outBuffer: UnivPtr ): OSStatus; external name '_AudioFileReadBytes';
(* API_AVAILABLE(macos(10.2), ios(2.0), watchos(2.0), tvos(9.0)) *)

{!
    @function				AudioFileWriteBytes
    @abstract				Write bytes of audio data to the audio file.
    @param inAudioFile		an AudioFileID.
    @param inUseCache 		true if it is desired to cache the data upon write, else false
    @param inStartingByte	the byte offset where the audio data should be written
    @param ioNumBytes 		on input, the number of bytes to write, on output, the number of
							bytes actually written.
    @param inBuffer 		inBuffer should be a void * containing the bytes to be written 
    @result					returns noErr if successful.
}
function AudioFileWriteBytes( inAudioFile: AudioFileID; inUseCache: Boolean; inStartingByte: SInt64; var ioNumBytes: UInt32; inBuffer: {const} UnivPtr ): OSStatus; external name '_AudioFileWriteBytes';
(* API_AVAILABLE(macos(10.2), ios(2.0), watchos(2.0), tvos(9.0)) *)

{!
    @function	AudioFileReadPacketData
    @abstract   Read packets of audio data from the audio file.
    @discussion AudioFileReadPacketData reads as many of the requested number of packets
				as will fit in the buffer size given by ioNumPackets.
				Unlike the deprecated AudioFileReadPackets, ioNumPackets must be initialized.
				If the byte size of the number packets requested is 
				less than the buffer size, ioNumBytes will be reduced.
				If the buffer is too small for the number of packets 
				requested, ioNumPackets and ioNumBytes will be reduced 
				to the number of packets that can be accommodated and their byte size.
				Returns kAudioFileEndOfFileError when read encounters end of file.
				For all uncompressed formats, packets == frames.

    @param inAudioFile				an AudioFileID.
    @param inUseCache 				true if it is desired to cache the data upon read, else false
    @param ioNumBytes				on input the size of outBuffer in bytes. 
									on output, the number of bytes actually returned.
    @param outPacketDescriptions 	An array of packet descriptions describing the packets being returned. 
									The size of the array must be greater or equal to the number of packets requested. 
									On return the packet description will be filled out with the packet offsets and sizes.
									Packet descriptions are ignored for CBR data.   
    @param inStartingPacket 		The packet index of the first packet desired to be returned
    @param ioNumPackets 			on input, the number of packets to read, on output, the number of
									packets actually read.
    @param outBuffer 				outBuffer should be a pointer to user allocated memory.
    @result							returns noErr if successful.
}
function AudioFileReadPacketData( inAudioFile: AudioFileID; inUseCache: Boolean; var ioNumBytes: UInt32; outPacketDescriptions: AudioStreamPacketDescriptionPtr {* __nullable}; inStartingPacket: SInt64; var ioNumPackets: UInt32; outBuffer: UnivPtr {__nullable} ): OSStatus; external name '_AudioFileReadPacketData';
(* API_AVAILABLE(macos(10.6), ios(2.2), watchos(2.0), tvos(9.0)) *)

{!
    @function	AudioFileReadPackets
    @abstract   Read packets of audio data from the audio file.
    @discussion AudioFileReadPackets is DEPRECATED. Use AudioFileReadPacketData instead.
				READ THE HEADER DOC FOR AudioFileReadPacketData. It is not a drop-in replacement.
				In particular, for AudioFileReadPacketData ioNumBytes must be initialized to the buffer size.
				AudioFileReadPackets assumes you have allocated your buffer to ioNumPackets times the maximum packet size.
				For many compressed formats this will only use a portion of the buffer since the ratio of the maximum 
				packet size to the typical packet size can be large. Use AudioFileReadPacketData instead.
	
    @param inAudioFile				an AudioFileID.
    @param inUseCache 				true if it is desired to cache the data upon read, else false
    @param outNumBytes				on output, the number of bytes actually returned
    @param outPacketDescriptions 	on output, an array of packet descriptions describing
									the packets being returned. NULL may be passed for this
									parameter. Nothing will be returned for linear pcm data.   
    @param inStartingPacket 		the packet index of the first packet desired to be returned
    @param ioNumPackets 			on input, the number of packets to read, on output, the number of
									packets actually read.
    @param outBuffer 				outBuffer should be a pointer to user allocated memory of size: 
									number of packets requested times file's maximum (or upper bound on)
									packet size.
    @result							returns noErr if successful.
}
function AudioFileReadPackets (	inAudioFile: AudioFileID; inUseCache: Boolean; var outNumBytes: UInt32; outPacketDescriptions: AudioStreamPacketDescriptionPtr {__nullable}; inStartingPacket: SInt64; var ioNumPackets: UInt32; outBuffer: UnivPtr{__nullable}): OSStatus; external name '_AudioFileReadPackets';
(* API_DEPRECATED("no longer supported", macos(10.2, 10.10), ios(2.0, 8.0)) __WATCHOS_PROHIBITED __TVOS_PROHIBITED; *)


{!
    @function	AudioFileWritePackets
    @abstract   Write packets of audio data to the audio file.
    @discussion For all uncompressed formats, packets == frames.
    @param inAudioFile				an AudioFileID.
    @param inUseCache 				true if it is desired to cache the data upon write, else false
    @param inNumBytes				the number of bytes being provided for write
    @param inPacketDescriptions 	an array of packet descriptions describing the packets being 
									provided. Not all formats require packet descriptions to be 
									provided. NULL may be passed if no descriptions are required.   
    @param inStartingPacket 		the packet index of where the first packet provided should be placed.
    @param ioNumPackets 			on input, the number of packets to write, on output, the number of
									packets actually written.
    @param inBuffer 				a void * to user allocated memory containing the packets to write.
    @result							returns noErr if successful.
}
function AudioFileWritePackets( inAudioFile: AudioFileID; inUseCache: Boolean; inNumBytes: UInt32; {const} inPacketDescriptions: AudioStreamPacketDescriptionPtr {__nullable}; inStartingPacket: SInt64; var ioNumPackets: UInt32; inBuffer: {const} UnivPtr ): OSStatus; external name '_AudioFileWritePackets';
(* API_AVAILABLE(macos(10.2), ios(2.0), watchos(2.0), tvos(9.0)) *)


{!
    @function	AudioFileCountUserData
    @abstract   Get the number of user data items with a certain ID in the file
    @discussion		"User Data" refers to chunks in AIFF, CAF and WAVE files, or resources 
					in Sound Designer II files, and possibly other things in other files.
					For simplicity, referred to below as "chunks".
    @param      inAudioFile			an AudioFileID.
    @param      inUserDataID		the four char code of the chunk.
    @param      outNumberItems		on output, if successful, number of chunks of this type in the file.
    @result							returns noErr if successful.
}
function AudioFileCountUserData( inAudioFile: AudioFileID; inUserDataID: UInt32; var outNumberItems: UInt32 ): OSStatus; external name '_AudioFileCountUserData';
(* API_AVAILABLE(macos(10.4), ios(2.0), watchos(2.0), tvos(9.0)) *)

{!
    @function	AudioFileGetUserDataSize
    @abstract   Get the size of user data in a file
    @param      inAudioFile			an AudioFileID.
    @param      inUserDataID		the four char code of the chunk.
    @param      inIndex				an index specifying which chunk if there are more than one.
    @param      outUserDataSize		on output, if successful, the size of the user data chunk.
    @result							returns noErr if successful.
}
function AudioFileGetUserDataSize( inAudioFile: AudioFileID; inUserDataID: UInt32; inIndex: UInt32; var outUserDataSize: UInt32 ): OSStatus; external name '_AudioFileGetUserDataSize';
(* API_AVAILABLE(macos(10.4), ios(2.0), watchos(2.0), tvos(9.0)) *)

{!
    @function	AudioFileGetUserData
    @abstract   Get the data of a chunk in a file.
    @param      inAudioFile			an AudioFileID.
    @param      inUserDataID		the four char code of the chunk.
    @param      inIndex				an index specifying which chunk if there are more than one.
	@param		ioUserDataSize		the size of the buffer on input, size of bytes copied to buffer on output 
    @param      outUserData			a pointer to a buffer in which to copy the chunk data.
    @result							returns noErr if successful.
}
function AudioFileGetUserData( inAudioFile: AudioFileID; inUserDataID: UInt32; inIndex: UInt32; var ioUserDataSize: UInt32; outUserData: UnivPtr ): OSStatus; external name '_AudioFileGetUserData';
(* API_AVAILABLE(macos(10.4), ios(2.0), watchos(2.0), tvos(9.0)) *)

{!
    @function	AudioFileSetUserData
    @abstract   Set the data of a chunk in a file.
    @param      inAudioFile			an AudioFileID.
    @param      inUserDataID		the four char code of the chunk.
    @param      inIndex				an index specifying which chunk if there are more than one.
	@param		inUserDataSize		on input the size of the data to copy, on output, size of bytes copied from the buffer  
    @param      inUserData			a pointer to a buffer from which to copy the chunk data 
									(only the contents of the chunk, not including the chunk header).
    @result							returns noErr if successful.
}
function AudioFileSetUserData( inAudioFile: AudioFileID; inUserDataID: UInt32; inIndex: UInt32; inUserDataSize: UInt32; inUserData: {const} UnivPtr ): OSStatus; external name '_AudioFileSetUserData';
(* API_AVAILABLE(macos(10.4), ios(2.0), watchos(2.0), tvos(9.0)) *)


{!
    @function	AudioFileRemoveUserData
    @abstract   Remove a user chunk in a file.
    @param      inAudioFile			an AudioFileID.
    @param      inUserDataID		the four char code of the chunk.
    @param      inIndex				an index specifying which chunk if there are more than one.
    @result							returns noErr if successful.
}

function AudioFileRemoveUserData( inAudioFile: AudioFileID; inUserDataID: UInt32; inIndex: UInt32 ): OSStatus; external name '_AudioFileRemoveUserData';
(* API_AVAILABLE(macos(10.5), ios(2.0), watchos(2.0), tvos(9.0)) *)


//=============================================================================
//	Audio File Properties
//=============================================================================

{!
    @enum		Audio File Properties
    @abstract   constants for AudioFile get/set property calls
    @constant   kAudioFilePropertyFileFormat 
					An AudioFileTypeID that identifies the format of the file
    @constant   kAudioFilePropertyDataFormat 
					An AudioStreamBasicDescription describing the format of the audio data
    @constant   kAudioFilePropertyFormatList 
					In order to support formats such as AAC SBR where an encoded data stream can be decoded to 
					multiple destination formats, this property returns an array of AudioFormatListItems (see AudioFormat.h) of those formats.
					The default behavior is to return the an AudioFormatListItem that has the same AudioStreamBasicDescription 
					that kAudioFilePropertyDataFormat returns.
    @constant   kAudioFilePropertyIsOptimized 
					A UInt32 indicating whether an Audio File has been optimized.
					Optimized means it is ready to start having sound data written to it. 
					A value of 0 indicates the file needs to be optimized.
					A value of 1 indicates the file is currently optimized.
    @constant   kAudioFilePropertyMagicCookieData 
					A void * pointing to memory set up by the caller.
					Some file types require that a magic cookie be provided before packets can be written
					to the file, so this property should be set before calling 
					AudioFileWriteBytes()/AudioFileWritePackets() if a magic cookie exists.
    @constant   kAudioFilePropertyAudioDataByteCount 
					a UInt64 that indicates the number of bytes of audio data contained in the file
    @constant   kAudioFilePropertyAudioDataPacketCount 
					a UInt64 that indicates the number of packets of audio data contained in the file
    @constant   kAudioFilePropertyMaximumPacketSize 
					a UInt32 that indicates the maximum size of a packet for the data contained in the file
    @constant   kAudioFilePropertyDataOffset 
					a SInt64 that indicates the byte offset in the file of the audio data.
    @constant   kAudioFilePropertyChannelLayout 
					An AudioChannelLayout struct.
    @constant   kAudioFilePropertyDeferSizeUpdates 
					A UInt32. If 1, then updating the files sizes in the header is not done for every write, 
					but deferred until the file is read, optimized or closed. This is more efficient, but less safe
					since, if the application crashes before the size is updated, the file may not be readable.
					The default value is one, it doesn't update the header.
    @constant   kAudioFilePropertyDataFormatName 
					This is deprecated. Use kAudioFormatProperty_FormatName in AudioFormat.h instead.
    @constant   kAudioFilePropertyMarkerList 
					access the list of markers defined in the file. returns an AudioFileMarkerList.
					The CFStringRefs in the returned structs must be released by the client.
					(available in 10.2.4 and later)
    @constant   kAudioFilePropertyRegionList 
					access the list of regions defined in the file. returns an Array of AudioFileRegions.
					The CFStringRefs in the returned structs must be released by the client.
					(available in 10.2.4 and later)
    @constant   kAudioFilePropertyPacketToFrame 
					pass a AudioFramePacketTranslation with mPacket filled out and get mFrame back. mFrameOffsetInPacket is ignored.
    @constant   kAudioFilePropertyFrameToPacket 
					pass a AudioFramePacketTranslation with mFrame filled out and get mPacket and mFrameOffsetInPacket back.
					
	@constant	kAudioFilePropertyPacketToByte
					pass an AudioBytePacketTranslation struct with mPacket filled out and get mByte back.
					mByteOffsetInPacket is ignored. If the mByte value is an estimate then 
					kBytePacketTranslationFlag_IsEstimate will be set in the mFlags field.
	@constant	kAudioFilePropertyByteToPacket
					pass an AudioBytePacketTranslation struct with mByte filled out and get mPacket and
					mByteOffsetInPacket back. If the mPacket value is an estimate then 
					kBytePacketTranslationFlag_IsEstimate will be set in the mFlags field.
					
    @constant   kAudioFilePropertyChunkIDs 
					returns an array of OSType four char codes for each kind of chunk in the file.
    @constant   kAudioFilePropertyInfoDictionary 
					returns a CFDictionary filled with information about the data contained in the file. 
					See dictionary key constants already defined for info string types. 
					AudioFileComponents are free to add keys to the dictionaries that they return for this property...
					caller is responsible for releasing the CFObject
    @constant   kAudioFilePropertyPacketTableInfo 
					Gets or sets an AudioFilePacketTableInfo struct for the file types that support it.
					When setting, the sum of mNumberValidFrames, mPrimingFrames and mRemainderFrames must be the same as the total
					number of frames in all packets. If not you will get a kAudio_ParamError. The best way to ensure this is to get the value of
					the property and make sure the sum of the three values you set has the same sum as the three values you got.
	@constant	kAudioFilePropertyPacketSizeUpperBound
					a UInt32 for the theoretical maximum packet size in the file (without actually scanning
					the whole file to find the largest packet, as may happen with kAudioFilePropertyMaximumPacketSize).
	@constant	kAudioFilePropertyReserveDuration
					The value is a Float64 of the duration in seconds of data that is expected to be written.
					Setting this property before any data has been written reserves space in the file header for a packet table 
					and/or other information so that it can appear before the audio data. Otherwise the packet table may get written at the 
					end of the file, preventing the file from being streamable.
	@constant	kAudioFilePropertyEstimatedDuration
					The value is a Float64 representing an estimated duration in seconds. If duration can be calculated without scanning the entire file,
					or all the audio data packets have been scanned, the value will accurately reflect the duration of the audio data. 
	@constant	kAudioFilePropertyBitRate
					Returns the bit rate for the audio data as a UInt32. For some formats this will be approximate.
	@constant	kAudioFilePropertyID3Tag
					A void * pointing to memory set up by the caller to contain a fully formatted ID3 tag (get/set v2.2, v2.3, or v2.4, v1 get only).
					The ID3 tag is not manipulated in anyway either for read or write. 
					When setting, this property must be called before calling AudioFileWritePackets.
	@constant	kAudioFilePropertySourceBitDepth
					For encoded data this property returns the bit depth of the source as an SInt32, if known.
					The bit depth is expressed as a negative number if the source was floating point, e.g. -32 for float, -64 for double.
	@constant	kAudioFilePropertyAlbumArtwork
					returns a CFDataRef filled with the Album Art or NULL. 
					The caller is responsible for releasing a non-NULL CFDataRef.
					In order to parse the contents of the data, CGImageSourceCreateWithData may be used.
    @constant	kAudioFilePropertyAudioTrackCount
                    a UInt32 that indicates the number of audio tracks contained in the file. (get property only)
    @constant	kAudioFilePropertyUseAudioTrack
                    a UInt32 that indicates the number of audio tracks contained in the file. (set property only)
 }
const
	kAudioFilePropertyFileFormat = FourCharCode('ffmt');
	kAudioFilePropertyDataFormat = FourCharCode('dfmt');
	kAudioFilePropertyIsOptimized = FourCharCode('optm');
	kAudioFilePropertyMagicCookieData = FourCharCode('mgic');
	kAudioFilePropertyAudioDataByteCount = FourCharCode('bcnt');
	kAudioFilePropertyAudioDataPacketCount = FourCharCode('pcnt');
	kAudioFilePropertyMaximumPacketSize = FourCharCode('psze');
	kAudioFilePropertyDataOffset = FourCharCode('doff');
	kAudioFilePropertyChannelLayout = FourCharCode('cmap');
	kAudioFilePropertyDeferSizeUpdates = FourCharCode('dszu');
	kAudioFilePropertyDataFormatName = FourCharCode('fnme');
	kAudioFilePropertyMarkerList = FourCharCode('mkls');
	kAudioFilePropertyRegionList = FourCharCode('rgls');
	kAudioFilePropertyPacketToFrame = FourCharCode('pkfr');
	kAudioFilePropertyFrameToPacket = FourCharCode('frpk');
	kAudioFilePropertyPacketToByte = FourCharCode('pkby');
	kAudioFilePropertyByteToPacket = FourCharCode('bypk');
	kAudioFilePropertyChunkIDs = FourCharCode('chid');
	kAudioFilePropertyInfoDictionary = FourCharCode('info');
	kAudioFilePropertyPacketTableInfo = FourCharCode('pnfo');
	kAudioFilePropertyFormatList = FourCharCode('flst');
	kAudioFilePropertyPacketSizeUpperBound = FourCharCode('pkub');
	kAudioFilePropertyReserveDuration = FourCharCode('rsrv');
	kAudioFilePropertyEstimatedDuration = FourCharCode('edur');
	kAudioFilePropertyBitRate = FourCharCode('brat');
	kAudioFilePropertyID3Tag = FourCharCode('id3t');
	kAudioFilePropertySourceBitDepth = FourCharCode('sbtd');
	kAudioFilePropertyAlbumArtwork = FourCharCode('aart');
	kAudioFilePropertyAudioTrackCount = FourCharCode('atct');
	kAudioFilePropertyUseAudioTrack = FourCharCode('uatk'); 


{!
    @function	AudioFileGetPropertyInfo
    @abstract   Get information about the size of a property of an AudioFile  and whether it can be set.
    @param      inAudioFile			an AudioFileID.
    @param      inPropertyID		an AudioFileProperty constant.
    @param      outDataSize			the size in bytes of the current value of the property. In order to get the property value, 
									you will need a buffer of this size.
    @param      isWritable			will be set to 1 if writable, or 0 if read only.
    @result							returns noErr if successful.
}
function AudioFileGetPropertyInfo( inAudioFile: AudioFileID; inPropertyID: AudioFilePropertyID; outDataSize: UInt32Ptr {__nullable}; isWritable: UInt32Ptr {__nullable} ): OSStatus; external name '_AudioFileGetPropertyInfo';
(* API_AVAILABLE(macos(10.2), ios(2.0), watchos(2.0), tvos(9.0)) *)
                                
{!
    @function	AudioFileGetProperty
    @abstract   Copies the value for a property of an AudioFile into a buffer.
    @param      inAudioFile			an AudioFileID.
    @param      inPropertyID		an AudioFileProperty constant.
    @param      ioDataSize			on input the size of the outPropertyData buffer. On output the number of bytes written to the buffer.
    @param      outPropertyData		the buffer in which to write the property data.
    @result							returns noErr if successful.
}
function AudioFileGetProperty( inAudioFile: AudioFileID; inPropertyID: AudioFilePropertyID; var ioDataSize: UInt32; outPropertyData: UnivPtr ): OSStatus; external name '_AudioFileGetProperty';
(* API_AVAILABLE(macos(10.2), ios(2.0), watchos(2.0), tvos(9.0)) *)
                        
{!
    @function	AudioFileSetProperty
    @abstract   Sets the value for a property of an AudioFile .
    @param      inAudioFile			an AudioFileID.
    @param      inPropertyID		an AudioFileProperty constant.
    @param      inDataSize			the size of the property data.
    @param      inPropertyData		the buffer containing the property data.
    @result							returns noErr if successful.
}
function AudioFileSetProperty( inAudioFile: AudioFileID; inPropertyID: AudioFilePropertyID; inDataSize: UInt32; inPropertyData: {const} UnivPtr ): OSStatus; external name '_AudioFileSetProperty';
(* API_AVAILABLE(macos(10.2), ios(2.0), watchos(2.0), tvos(9.0)) *)


//=============================================================================
//	Audio File Global Info Properties
//=============================================================================

{!
    @enum		Audio File Global Info Properties
    @abstract   constants for AudioFileGetGlobalInfo properties
    @constant   kAudioFileGlobalInfo_ReadableTypes
					No specifier needed. Must be set to NULL.
					Returns an array of UInt32 containing the file types 
					(i.e. AIFF, WAVE, etc) that can be opened for reading.
    @constant   kAudioFileGlobalInfo_WritableTypes
					No specifier needed. Must be set to NULL.
					Returns an array of UInt32 containing the file types 
					(i.e. AIFF, WAVE, etc) that can be opened for writing.
    @constant   kAudioFileGlobalInfo_FileTypeName
					Specifier is a pointer to a AudioFileTypeID containing a file type.
					Returns a CFString containing the name for the file type. 
    @constant   kAudioFileGlobalInfo_AvailableFormatIDs
					Specifier is a pointer to a AudioFileTypeID containing a file type.
					Returns a array of format IDs for formats that can be read. 
    @constant   kAudioFileGlobalInfo_AvailableStreamDescriptionsForFormat
					Specifier is a pointer to a AudioFileTypeAndFormatID struct defined below.
					Returns an array of AudioStreamBasicDescriptions which have all of the 
					formats for a particular file type and format ID. The AudioStreamBasicDescriptions
					have the following fields filled in: mFormatID, mFormatFlags, mBitsPerChannel
					writing new files.
					
					
    @constant   kAudioFileGlobalInfo_AllExtensions
					No specifier needed. Must be set to NULL.
					Returns a CFArray of CFStrings containing all file extensions 
					that are recognized. The array be used when creating an NSOpenPanel.

    @constant   kAudioFileGlobalInfo_AllHFSTypeCodes
					No specifier needed. Must be set to NULL.
					Returns an array of HFSTypeCode's containing all HFSTypeCodes
					that are recognized.

    @constant   kAudioFileGlobalInfo_AllUTIs
					No specifier needed. Must be set to NULL.
					Returns a CFArray of CFString of all Universal Type Identifiers
					that are recognized by AudioFile. 
					The caller is responsible for releasing the CFArray.

    @constant   kAudioFileGlobalInfo_AllMIMETypes
					No specifier needed. Must be set to NULL.
					Returns a CFArray of CFString of all MIME types
					that are recognized by AudioFile. 
					The caller is responsible for releasing the CFArray.


    @constant   kAudioFileGlobalInfo_ExtensionsForType
					Specifier is a pointer to a AudioFileTypeID containing a file type.
					Returns a CFArray of CFStrings containing the file extensions 
					that are recognized for this file type. 

    @constant   kAudioFileGlobalInfo_HFSTypeCodesForType
					Specifier is a pointer to an AudioFileTypeID.
					Returns an array of HFSTypeCodes corresponding to that file type.
					The first type in the array is the preferred one for use when

    @constant   kAudioFileGlobalInfo_UTIsForType
					Specifier is a pointer to an AudioFileTypeID.
					Returns a CFArray of CFString of all Universal Type Identifiers
					that are recognized by the file type. 
					The caller is responsible for releasing the CFArray.

    @constant   kAudioFileGlobalInfo_MIMETypesForType
					Specifier is a pointer to an AudioFileTypeID.
					Returns a CFArray of CFString of all MIME types
					that are recognized by the file type. 
					The caller is responsible for releasing the CFArray.

	these are inverses of the above:

    @constant   kAudioFileGlobalInfo_TypesForExtension
					Specifier is a CFStringRef containing a file extension.
					Returns an array of all AudioFileTypeIDs that support the extension. 
	
    @constant   kAudioFileGlobalInfo_TypesForHFSTypeCode
					Specifier is an HFSTypeCode.
					Returns an array of all AudioFileTypeIDs that support the HFSTypeCode. 

    @constant   kAudioFileGlobalInfo_TypesForUTI
					Specifier is a CFStringRef containing a Universal Type Identifier.
					Returns an array of all AudioFileTypeIDs that support the UTI. 

    @constant   kAudioFileGlobalInfo_TypesForMIMEType
					Specifier is a CFStringRef containing a MIME Type.
					Returns an array of all AudioFileTypeIDs that support the MIME type. 

}
const
	kAudioFileGlobalInfo_ReadableTypes = FourCharCode('afrf');
	kAudioFileGlobalInfo_WritableTypes = FourCharCode('afwf');
	kAudioFileGlobalInfo_FileTypeName = FourCharCode('ftnm');
	kAudioFileGlobalInfo_AvailableStreamDescriptionsForFormat = FourCharCode('sdid');
	kAudioFileGlobalInfo_AvailableFormatIDs = FourCharCode('fmid');
	kAudioFileGlobalInfo_AllExtensions = FourCharCode('alxt');
	kAudioFileGlobalInfo_AllHFSTypeCodes = FourCharCode('ahfs');
	kAudioFileGlobalInfo_AllUTIs = FourCharCode('auti');
	kAudioFileGlobalInfo_AllMIMETypes = FourCharCode('amim');
	kAudioFileGlobalInfo_ExtensionsForType = FourCharCode('fext');
	kAudioFileGlobalInfo_HFSTypeCodesForType = FourCharCode('fhfs');
	kAudioFileGlobalInfo_UTIsForType = FourCharCode('futi');
	kAudioFileGlobalInfo_MIMETypesForType = FourCharCode('fmim');
	kAudioFileGlobalInfo_TypesForMIMEType = FourCharCode('tmim');
	kAudioFileGlobalInfo_TypesForUTI = FourCharCode('tuti');
	kAudioFileGlobalInfo_TypesForHFSTypeCode = FourCharCode('thfs');
	kAudioFileGlobalInfo_TypesForExtension = FourCharCode('text'); 


{!
    @struct		AudioFileTypeAndFormatID
    @abstract   This is used as a specifier for kAudioFileGlobalInfo_AvailableStreamDescriptions
    @discussion This struct is used to specify a desired audio file type and data format ID  so
				that a list of stream descriptions of available formats can be obtained.
    @field      mFileType
					a four char code for the file type such as kAudioFileAIFFType, kAudioFileCAFType, etc.
    @field      mFormatID
					a four char code for the format ID such as kAudioFormatLinearPCM, kAudioFormatMPEG4AAC, etc.
}
type
	AudioFileTypeAndFormatID = record
		mFileType: AudioFileTypeID;
		mFormatID: UInt32;
	end;


{!
    @function	AudioFileGetGlobalInfoSize
    @abstract   Get the size of a global property.
    @param      inPropertyID		an AudioFileGlobalInfo property constant.
    @param      inSpecifierSize		The size of the specifier data.
    @param      inSpecifier			A specifier is a buffer of data used as an input argument to some of the global info properties.
    @param      outDataSize			the size in bytes of the current value of the property. In order to get the property value, 
									you will need a buffer of this size.
    @result							returns noErr if successful.
}
function AudioFileGetGlobalInfoSize( inPropertyID: AudioFilePropertyID; inSpecifierSize: UInt32; inSpecifier: UnivPtr {__nullable}; var outDataSize: UInt32 ): OSStatus; external name '_AudioFileGetGlobalInfoSize';
(* API_AVAILABLE(macos(10.3), ios(2.0), watchos(2.0), tvos(9.0)) *)
                                
{!
    @function	AudioFileGetGlobalInfo
    @abstract   Copies the value for a global property into a buffer.
    @param      inPropertyID		an AudioFileGlobalInfo property constant.
    @param      inSpecifierSize		The size of the specifier data.
    @param      inSpecifier			A specifier is a buffer of data used as an input argument to some of the global info properties.
    @param      ioDataSize			on input the size of the outPropertyData buffer. On output the number of bytes written to the buffer.
    @param      outPropertyData		the buffer in which to write the property data.
    @result							returns noErr if successful.
}
function AudioFileGetGlobalInfo( inPropertyID: AudioFilePropertyID; inSpecifierSize: UInt32; inSpecifier: UnivPtr {__nullable}; var ioDataSize: UInt32; outPropertyData: UnivPtr ): OSStatus; external name '_AudioFileGetGlobalInfo';
(* API_AVAILABLE(macos(10.3), ios(2.0), watchos(2.0), tvos(9.0)) *)

//#pragma mark - Deprecated

(*	
struct FSRef;
{!
    @function	AudioFileCreate
    @abstract   creates a new audio file
    @discussion	creates a new audio file located in the parent directory 
                      provided. Upon success, an AudioFileID is returned which can
                      be used for subsequent calls to the AudioFile APIs.
    @param inParentRef		an FSRef to the directory where  the new file should be created.
    @param inFileName		a CFStringRef containing the name of the file to be created.
    @param inFileType		an AudioFileTypeID indicating the type of audio file to create.
    @param inFormat			an AudioStreamBasicDescription describing the data format that will be
							added to the audio file.
    @param inFlags			relevant flags for creating/opening the file. 
    @param outNewFileRef	if successful, the FSRef of the newly created file.
    @param outAudioFile		if successful, an AudioFileID that can be used for subsequent AudioFile calls.
    @result					returns noErr if successful.
	@deprecated				in Mac OS X 10.6, see AudioFileCreateWithURL
}
extern OSStatus	
AudioFileCreate (	const struct FSRef					*inParentRef, 
                    CFStringRef							inFileName,
                    AudioFileTypeID						inFileType,
                    const AudioStreamBasicDescription	*inFormat,
                    AudioFileFlags						inFlags,
                    struct FSRef						*outNewFileRef,
                    AudioFileID	__nullable * __nonnull	outAudioFile)		API_DEPRECATED("no longer supported", macos(10.2, 10.6)) API_UNAVAILABLE(ios, watchos, tvos);

{!
    @function				AudioFileInitialize
    @abstract				Write over an existing audio file.
    @discussion				Use AudioFileInitialize to wipe clean an existing audio file
							and prepare it to be populated with new data.
    @param inFileRef		the FSRef of an existing audio file.
    @param inFileType		an AudioFileTypeID indicating the type of audio file to initialize the file to. 
    @param inFormat			an AudioStreamBasicDescription describing the data format that will be
							added to the audio file.
    @param inFlags			flags for creating/opening the file. Currently zero.
    @param outAudioFile		upon success, an AudioFileID that can be used for subsequent
							AudioFile calls.
    @result					returns noErr if successful.
	@deprecated				in Mac OS X 10.6, see AudioFileCreateWithURL
}
extern OSStatus	
AudioFileInitialize (	const struct FSRef					*inFileRef,
                        AudioFileTypeID						inFileType,
                        const AudioStreamBasicDescription	*inFormat,
                        AudioFileFlags						inFlags,
                        AudioFileID	__nullable * __nonnull	outAudioFile)	API_DEPRECATED("no longer supported", macos(10.2, 10.6)) API_UNAVAILABLE(ios, watchos, tvos);

{!
    @function				AudioFileOpen
    @abstract				Open an existing audio file.
    @discussion				Open an existing audio file for reading or reading and writing.
    @param inFileRef		the FSRef of an existing audio file.
    @param inPermissions	use the permission constants
    @param inFileTypeHint	For files which have no filename extension and whose type cannot be easily or
							uniquely determined from the data (ADTS,AC3), this hint can be used to indicate the file type. 
							Otherwise you can pass zero for this. The hint is only used on OS versions 10.3.1 or greater.
							For OS versions prior to that, opening files of the above description will fail.
    @param outAudioFile		upon success, an AudioFileID that can be used for subsequent
							AudioFile calls.
    @result					returns noErr if successful.
	@deprecated				in Mac OS X 10.6, see AudioFileOpenURL
}
extern OSStatus	
AudioFileOpen (	const struct FSRef		*inFileRef,
                AudioFilePermissions	inPermissions,
                AudioFileTypeID			inFileTypeHint,
                AudioFileID	__nullable * __nonnull	outAudioFile)			API_DEPRECATED("no longer supported", macos(10.2, 10.6)) API_UNAVAILABLE(ios, watchos, tvos);

	
*)
//CF_ASSUME_NONNULL_END

{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}
implementation

function NumBytesToNumAudioFileMarkers(inNumBytes: size_t): size_t;
begin
  if inNumBytes < size_t(@AudioFileMarkerListPtr(nil)^.mMarkers[0]) then
    NumBytesToNumAudioFileMarkers := 0
  else
    NumBytesToNumAudioFileMarkers := inNumBytes - size_t(@AudioFileMarkerListPtr(nil)^.mMarkers[0])
end;


function NumAudioFileMarkersToNumBytes(inNumMarkers: size_t): size_t;
begin
	NumAudioFileMarkersToNumBytes := size_t(@AudioFileMarkerListPtr(nil)^.mMarkers[0]) + (inNumMarkers) * sizeof(AudioFileMarker);
end;


function NextAudioFileRegion(const {var} inAFRegionPtr: AudioFileRegion): AudioFileRegionPtr;
begin
  NextAudioFileRegion := AudioFileRegionPtr(
    Pointer(@inAFRegionPtr) +
    MacPtrUInt(@AudioFileRegionPtr(nil)^.mMarkers) +
    (inAFRegionPtr.mNumberMarkers*sizeof(AudioFileMarker))
    );
  end;

end.

{$endc} {not MACOSALLINCLUDE}
