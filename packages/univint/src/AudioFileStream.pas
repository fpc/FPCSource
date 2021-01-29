{!
	@file		AudioFileStream.h
	@framework	AudioToolbox.framework
	@copyright	(c) 1985-2015 by Apple, Inc., all rights reserved.

	@brief		API's to parse streamed audio files into packets of audio data.

	@discussion

	AudioFileStream addresses situations where, in a stream of audio data, only a limited window
	of data may be available at any time.

	This case differs significantly enough from the random access file case to warrant a separate
	API rather than overload the AudioFile API with additional semantics. With a random access file,
	one can always assume that a read request for contiguous data that doesn't include EOF will
	always supply all of the data. This makes parsing straightforward and inexpensive. In the
	streaming case such an assumption cannot be made. A request by the parser for data from the
	stream may only be partially satisfied. Any partially satisfied requests must be remembered and
	retried before any other requests are satisfied, otherwise the streamed data might be lost
	forever in the past. So the parser must be able to suspend work at any point and resume parsing
	where it left off.
	
	The client provides data to the parser using AudioFileStreamParseBytes and the parser calls back
	to the client with properties or packets using the AudioFileStream_PropertyListenerProc and
	AudioFileStream_PacketsProc function pointers.
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

unit AudioFileStream;
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
uses MacTypes,CoreAudioTypes,AudioFile;
{$endc} {not MACOSALLINCLUDE}

{$ALIGN POWER}


//=============================================================================
//	Includes
//=============================================================================


//CF_ASSUME_NONNULL_BEGIN


//=============================================================================
//	AudioFileStream flags
//=============================================================================
//#pragma mark flags

{!
    @enum AudioFileStreamPropertyFlags
    @constant   kAudioFileStreamPropertyFlag_PropertyIsCached 
		This flag is set in a call to AudioFileStream_PropertyListenerProc when the value of the property
		can be obtained at any later time. If this flag is not set, then you should either get the value of 
		the property from within this callback or set the flag kAudioFileStreamPropertyFlag_CacheProperty in order to signal
		to the parser to begin caching the property data. Otherwise the value may not be available in the future.
		
    @constant   kAudioFileStreamPropertyFlag_CacheProperty 
		This flag can be set by a property listener in order to signal to the parser that the client is
		interested in the value of the property and that it should be cached until the full value of the property is available.
}
type
	AudioFileStreamPropertyFlags = UInt32;
	AudioFileStreamPropertyFlagsPtr = ^AudioFileStreamPropertyFlags;
const
	kAudioFileStreamPropertyFlag_PropertyIsCached = 1;
	kAudioFileStreamPropertyFlag_CacheProperty = 2; 

{!	@enum	AudioFileStreamParseFlags
    @constant   kAudioFileStreamParseFlag_Discontinuity 
		This flag is passed in to AudioFileStreamParseBytes to signal a discontinuity. Any partial packet straddling a buffer
		boundary will be discarded. This is necessary to avoid being called with a corrupt packet. After a discontinuity occurs
		seeking may be approximate in some data formats.
}
type
	AudioFileStreamParseFlags = UInt32;
	AudioFileStreamParseFlagsPtr = ^AudioFileStreamParseFlags;
const
	kAudioFileStreamParseFlag_Discontinuity = 1; 

{!	@enum	AudioFileStreamParseFlags
    @constant   kAudioFileStreamSeekFlag_OffsetIsEstimated 
		This flag may be returned from AudioFileStreamSeek if the byte offset is only an estimate, not exact.
}
type
	AudioFileStreamSeekFlags = UInt32;
	AudioFileStreamSeekFlagsPtr = ^AudioFileStreamSeekFlags;
const
	kAudioFileStreamSeekFlag_OffsetIsEstimated = 1; 

//=============================================================================
//	AudioFileStream Types
//=============================================================================
//#pragma mark types

type
	AudioFileStreamPropertyID = UInt32;
	AudioFileStreamPropertyIDPtr = ^AudioFileStreamPropertyID;

	AudioFileStreamID = record end;
	OpaqueAudioFileStreamID = ^AudioFileStreamID;

type
	AudioFileStream_PropertyListenerProc = procedure( inClientData: UnivPtr; inAudioFileStream: AudioFileStreamID; inPropertyID: AudioFileStreamPropertyID; var ioFlags: AudioFileStreamPropertyFlags );

type
	AudioFileStream_PacketsProc = procedure( inClientData: UnivPtr; inNumberBytes: UInt32; inNumberPackets: UInt32; inInputData: {const} UnivPtr; var inPacketDescriptions: AudioStreamPacketDescription );

//=============================================================================
//	AudioFileStream error codes
//=============================================================================
//#pragma mark Error codes
{!
    @enum AudioFileStream error codes

    @abstract   These are the error codes returned from the AudioFile API.

    @constant   kAudioFileStreamError_UnsupportedFileType 
		The file type is not supported.
    @constant   kAudioFileStreamError_UnsupportedDataFormat 
		The data format is not supported by this file type.
    @constant   kAudioFileStreamError_UnsupportedProperty 
		The property is not supported.
    @constant   kAudioFileStreamError_BadPropertySize 
		The size of the property data was not correct.
    @constant   kAudioFileStreamError_NotOptimized 
		It is not possible to produce output packets because the file's packet table or other defining 
		info is either not present or is after the audio data.
    @constant   kAudioFileStreamError_InvalidPacketOffset 
		A packet offset was less than zero, or past the end of the file,
		or a corrupt packet size was read when building the packet table. 
    @constant   kAudioFileStreamError_InvalidFile 
		The file is malformed, or otherwise not a valid instance of an audio file of its type, or 
		is not recognized as an audio file. 
    @constant   kAudioFileStreamError_ValueUnknown 
		The property value is not present in this file before the audio data.
	@constant	kAudioFileStreamError_DataUnavailable
		The amount of data provided to the parser was insufficient to produce any result.
	@constant	kAudioFileStreamError_IllegalOperation
		An illegal operation was attempted.
    @constant   kAudioFileStreamError_UnspecifiedError 
		An unspecified error has occurred.
		
}
const
	kAudioFileStreamError_UnsupportedFileType = FourCharCode('typ?');
	kAudioFileStreamError_UnsupportedDataFormat = FourCharCode('fmt?');
	kAudioFileStreamError_UnsupportedProperty = FourCharCode('pty?');
	kAudioFileStreamError_BadPropertySize = FourCharCode('!siz');
	kAudioFileStreamError_NotOptimized = FourCharCode('optm');
	kAudioFileStreamError_InvalidPacketOffset = FourCharCode('pck?');
	kAudioFileStreamError_InvalidFile = FourCharCode('dta?');
	kAudioFileStreamError_ValueUnknown = FourCharCode('unk?');
	kAudioFileStreamError_DataUnavailable = FourCharCode('more');
	kAudioFileStreamError_IllegalOperation = FourCharCode('nope');
	kAudioFileStreamError_UnspecifiedError = FourCharCode('wht?');
	kAudioFileStreamError_DiscontinuityCantRecover = FourCharCode('dsc!'); 

//=============================================================================
//	AudioFileStream Properties
//=============================================================================
//#pragma mark Properties

{!
    @enum		AudioFileStream Properties
	
    @abstract   constants for AudioFileStream get property calls
    @discussion		There are currently no settable properties.

					
    @constant   kAudioFileStreamProperty_ReadyToProducePackets
					An UInt32 which is zero until the parser has parsed up to the beginning of the audio data. 
					Once it has reached the audio data, the value of this property becomes one. 
					When this value has become one, all properties that can be known about the stream are known.
					
    @constant   kAudioFileStreamProperty_FileFormat 
					An UInt32 four char code that identifies the format of the file
    @constant   kAudioFileStreamProperty_DataFormat 
					An AudioStreamBasicDescription describing the format of the audio data
    @constant   kAudioFileStreamProperty_FormatList 
					In order to support formats such as AAC SBR where an encoded data stream can be decoded to 
					multiple destination formats, this property returns an array of AudioFormatListItems 
					(see AudioFormat.h) of those formats.
					The default behavior is to return the an AudioFormatListItem that has the same 
					AudioStreamBasicDescription that kAudioFileStreamProperty_DataFormat returns.
    @constant   kAudioFileStreamProperty_MagicCookieData 
					A void * pointing to memory set up by the caller.
					Some file types require that a magic cookie be provided before packets can be written
					to the file, so this property should be set before calling 
					AudioFileWriteBytes()/AudioFileWritePackets() if a magic cookie exists.
    @constant   kAudioFileStreamProperty_AudioDataByteCount 
					a UInt64 that indicates the number of bytes of audio data contained in the file
    @constant   kAudioFileStreamProperty_AudioDataPacketCount 
					a UInt64 that indicates the number of packets of audio data contained in the file
    @constant   kAudioFileStreamProperty_MaximumPacketSize 
					a UInt32 that indicates the maximum size of a packet for the data contained in the file
    @constant   kAudioFileStreamProperty_DataOffset 
					a SInt64 that indicates the byte offset in the file of the audio data.
    @constant   kAudioFileStreamProperty_ChannelLayout 
					An AudioChannelLayout struct.
    @constant   kAudioFileStreamProperty_PacketToFrame 
					pass a AudioFramePacketTranslation with mPacket filled out and get mFrame back. 
					mFrameOffsetInPacket is ignored.
    @constant   kAudioFileStreamProperty_FrameToPacket 
					pass a AudioFramePacketTranslation with mFrame filled out and get mPacket and 
					mFrameOffsetInPacket back.
	@constant	kAudioFileStreamProperty_PacketToByte
					pass an AudioBytePacketTranslation struct with mPacket filled out and get mByte back.
					mByteOffsetInPacket is ignored. If the mByte value is an estimate then 
					kBytePacketTranslationFlag_IsEstimate will be set in the mFlags field.
	@constant	kAudioFileStreamProperty_ByteToPacket
					pass an AudioBytePacketTranslation struct with mByte filled out and get mPacket and
					mByteOffsetInPacket back. If the mPacket value is an estimate then 
					kBytePacketTranslationFlag_IsEstimate will be set in the mFlags field.
    @constant   kAudioFileStreamProperty_PacketTableInfo 
					Gets the AudioFilePacketTableInfo struct for the file types that support it.
	@constant	kAudioFileStreamProperty_PacketSizeUpperBound
					a UInt32 for the theoretical maximum packet size in the file.
	@constant	kAudioFileStreamProperty_AverageBytesPerPacket
					a Float64 of giving the average bytes per packet seen. 
					For CBR and files with packet tables, this number will be exact. Otherwise, it is a
					running average of packets parsed.
	@constant	kAudioFileStreamProperty_BitRate
					a UInt32 of the bit rate in bits per second.
    @constant	kAudioFileStreamProperty_InfoDictionary
                    a CFDictionary filled with information about the data contained in the stream.
                    See AudioFile.h for InfoDictionary key strings. Caller is responsible for releasing the CFObject.
}
const
	kAudioFileStreamProperty_ReadyToProducePackets = FourCharCode('redy');
	kAudioFileStreamProperty_FileFormat = FourCharCode('ffmt');
	kAudioFileStreamProperty_DataFormat = FourCharCode('dfmt');
	kAudioFileStreamProperty_FormatList = FourCharCode('flst');
	kAudioFileStreamProperty_MagicCookieData = FourCharCode('mgic');
	kAudioFileStreamProperty_AudioDataByteCount = FourCharCode('bcnt');
	kAudioFileStreamProperty_AudioDataPacketCount = FourCharCode('pcnt');
	kAudioFileStreamProperty_MaximumPacketSize = FourCharCode('psze');
	kAudioFileStreamProperty_DataOffset = FourCharCode('doff');
	kAudioFileStreamProperty_ChannelLayout = FourCharCode('cmap');
	kAudioFileStreamProperty_PacketToFrame = FourCharCode('pkfr');
	kAudioFileStreamProperty_FrameToPacket = FourCharCode('frpk');
	kAudioFileStreamProperty_PacketToByte = FourCharCode('pkby');
	kAudioFileStreamProperty_ByteToPacket = FourCharCode('bypk');
	kAudioFileStreamProperty_PacketTableInfo = FourCharCode('pnfo');
	kAudioFileStreamProperty_PacketSizeUpperBound = FourCharCode('pkub');
	kAudioFileStreamProperty_AverageBytesPerPacket = FourCharCode('abpp');
	kAudioFileStreamProperty_BitRate = FourCharCode('brat');
	kAudioFileStreamProperty_InfoDictionary = FourCharCode('info'); 

//=============================================================================
//	AudioFileStream Functions
//=============================================================================
//#pragma mark Functions


{!
	@function		AudioFileStreamOpen

	@discussion		Create a new audio file stream parser.
					The client provides the parser with data and the parser calls
					callbacks when interesting things are found in the data, such as properties and 
					audio packets.

    @param			inClientData					
						a constant that will be passed to your callbacks.
	@param			inPropertyListenerProc
						Whenever the value of a property is parsed in the data, this function will be called.
						You can then get the value of the property from in the callback. In some cases, due to 
						boundaries in the input data, the property may return kAudioFileStreamError_DataUnavailable.
						When unavailable data is requested from within the property listener, the parser will begin 
						caching the property value and will call the property listener again when the property is
						available. For property values for which kAudioFileStreamPropertyFlag_PropertyIsCached is unset, this 
						will be the only opportunity to get the value of the property, since the data will be 
						disposed upon return of the property listener callback. 
	@param			inPacketsProc
						Whenever packets are parsed in the data, a pointer to the packets is passed to the client 
						using this callback. At times only a single packet may be passed due to boundaries in the 
						input data.
    @param 			inFileTypeHint	
						For files whose type cannot be easily or uniquely determined from the data (ADTS,AC3), 
						this hint can be used to indicate the file type. 
						Otherwise if you do not know the file type, you can pass zero. 
	@param			outAudioFileStream 
						A new file stream ID for use in other AudioFileStream API calls.
} 
function AudioFileStreamOpen( inClientData: UnivPtr {__nullable}; inPropertyListenerProc: AudioFileStream_PropertyListenerProc; inPacketsProc: AudioFileStream_PacketsProc; inFileTypeHint: AudioFileTypeID; var outAudioFileStream: AudioFileStreamID {__nullable * __nonnull} ): OSStatus; external name '_AudioFileStreamOpen';
(* API_AVAILABLE(macos(10.5), ios(2.0), watchos(2.0), tvos(9.0)) *)


{!
	@function		AudioFileStreamParseBytes

	@discussion		This call is the means for streams to supply data to the parser. 
					Data is expected to be passed in sequentially from the beginning of the file, without gaps.
					In the course of parsing, the client's property and/or packets callbacks may be called.
					At the end of the stream, this function must be called once with null data pointer and zero
					data byte size to flush any remaining packets out of the parser.

	@param			inAudioFileStream 
						The file stream ID
	@param			inDataByteSize 
						The number of bytes passed in for parsing. Must be zero when flushing the parser.
	@param			inData 
						The data passed in to be parsed. Must be null when flushing the parser.
	@param			inFlags 
						If there is a data discontinuity, then kAudioFileStreamParseFlag_Discontinuity should be set true. 
}
function AudioFileStreamParseBytes( inAudioFileStream: AudioFileStreamID; inDataByteSize: UInt32; {const} inData: UnivPtr {__nullable}; inFlags: AudioFileStreamParseFlags ): OSStatus; external name '_AudioFileStreamParseBytes';
(* API_AVAILABLE(macos(10.5), ios(2.0), watchos(2.0), tvos(9.0)) *)

{!
	@function		AudioFileStreamSeek

	@discussion		This call is used to seek in the data stream. The client passes in a packet 
					offset to seek to and the parser passes back a byte offset from which to
					get the data to satisfy that request. The data passed to the next call to 
					AudioFileParseBytes will be assumed to be from that byte offset.
					For file formats which do not contain packet tables the byte offset may 
					be an estimate. If so, the flag kAudioFileStreamSeekFlag_OffsetIsEstimated will be true.

	@param			inAudioFileStream 
						The file stream ID
	@param			inPacketOffset 
						The offset from the beginning of the file of the packet to which to seek.
	@param			outDataByteOffset 
						The byte offset of the data from the file's data offset returned. 
						You need to add the value of kAudioFileStreamProperty_DataOffset to get an absolute byte offset in the file.
	@param			ioFlags
						If outDataByteOffset is an estimate, then kAudioFileStreamSeekFlag_OffsetIsEstimated will be set on output.
						There are currently no flags defined for passing into this call.
}
function AudioFileStreamSeek( inAudioFileStream: AudioFileStreamID; inPacketOffset: SInt64; var outDataByteOffset: SInt64; var ioFlags: AudioFileStreamSeekFlags ): OSStatus; external name '_AudioFileStreamSeek';
(* API_AVAILABLE(macos(10.5), ios(2.0), watchos(2.0), tvos(9.0)) *)

{!
	@function		AudioFileStreamGetPropertyInfo
 
	@discussion		Retrieve the info about the given property. The outSize argument
					will return the size in bytes of the current value of the property.
 
	@param			inAudioFileStream 
						The file stream ID
	@param			inPropertyID
						Property ID whose value should be read
	@param			outPropertyDataSize
						Size in bytes of the property
	@param			outWritable
						whether the property is writable
 
	@result			an OSStatus return code
}
function AudioFileStreamGetPropertyInfo( inAudioFileStream: AudioFileStreamID; inPropertyID: AudioFileStreamPropertyID; outPropertyDataSize: UInt32Ptr {__nullable}; outWritable: BooleanPtr {__nullable} ): OSStatus; external name '_AudioFileStreamGetPropertyInfo';
(* API_AVAILABLE(macos(10.5), ios(2.0), watchos(2.0), tvos(9.0)) *)


{!
	@function		AudioFileStreamGetProperty
 
	@discussion		Retrieve the indicated property data. 
 
	@param			inAudioFileStream 
						The file stream ID
	@param			inPropertyID
						Property ID whose value should be read
	@param			ioPropertyDataSize
						On input, the size of the buffer pointed to by outPropertyData. On output, 
						the number of bytes written.
	@param			outPropertyData
						Pointer to the property data buffer

	@result			an OSStatus return code
}
function AudioFileStreamGetProperty( inAudioFileStream: AudioFileStreamID; inPropertyID: AudioFileStreamPropertyID; var ioPropertyDataSize: UInt32; outPropertyData: UnivPtr ): OSStatus; external name '_AudioFileStreamGetProperty';
(* API_AVAILABLE(macos(10.5), ios(2.0), watchos(2.0), tvos(9.0)) *)

{!
	@function		AudioFileStreamSetProperty
 
	@discussion		Set the value of the property. There are currently no settable properties.
 
	@param			inAudioFileStream 
						The file stream ID
	@param			inPropertyID
						Property ID whose value should be set
	@param			inPropertyDataSize
						Size in bytes of the property data
	@param			inPropertyData
						Pointer to the property data buffer

	@result			an OSStatus return code
}
function AudioFileStreamSetProperty( inAudioFileStream: AudioFileStreamID; inPropertyID: AudioFileStreamPropertyID; inPropertyDataSize: UInt32; inPropertyData: {const} UnivPtr ): OSStatus; external name '_AudioFileStreamSetProperty';
(* API_AVAILABLE(macos(10.5), ios(2.0), watchos(2.0), tvos(9.0)) *)

{!
	@function		AudioFileStreamClose
 
	@discussion		Close and deallocate the file stream object.

	@param			inAudioFileStream 
						The file stream ID
}
function AudioFileStreamClose( inAudioFileStream: AudioFileStreamID ): OSStatus; external name '_AudioFileStreamClose';
(* API_AVAILABLE(macos(10.5), ios(2.0), watchos(2.0), tvos(9.0)) *)


//CF_ASSUME_NONNULL_END

{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}

end.
{$endc} {not MACOSALLINCLUDE}
