{!
	@file		AudioFileComponent.h
	@framework	AudioToolbox.framework
	@copyright	(c) 2004-2015 by Apple, Inc., all rights reserved.

	@abstract	Interfaces for components which implement knowledge of audio file formats.
    @discussion
    	Audio file components are not for the use of clients. Rather, they are called by the
    	implementation of the AudioFile API to implement the various semantics of that API.
		Most of these calls match a call in the AudioFile API which calls through to the component.

		A component may be used in two ways, either associated with a file or not. If a component is
		not associated with a file, it may be used to answer questions about the file type in
		general and whether some data is recognized by that file type. A component is associated
		with a file by calling one of the AudioFile Create, Open or Initialize calls. If a component
		is associated with a file, then it can also be asked to perform any of the calls that
		implement the AudioFile API.
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

unit AudioFileComponents;
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
uses MacTypes,CFBase,CoreAudioTypes,Files,AudioFile,AudioComponents;
{$endc} {not MACOSALLINCLUDE}

{$ALIGN POWER}


//==================================================================================================
//	Includes
//==================================================================================================


//CF_ASSUME_NONNULL_BEGIN


{!
    @typedef	AudioFileComponent
    @abstract		represents an instance of an AudioFileComponent.
}
type
	AudioFileComponent = AudioComponentInstance;
	AudioFileComponentPtr = ^AudioFileComponent;
{!
    @typedef	AudioFileComponentPropertyID
    @abstract		a four char code for a property ID.
}
type
	AudioFileComponentPropertyID = UInt32;
	AudioFileComponentPropertyIDPtr = ^AudioFileComponentPropertyID;

{$ifc TARGET_OS_MAC}
{!
    @function	AudioFileComponentCreateURL
    @abstract   creates a new (or initialises an existing) audio file specified by the URL.
    @discussion	creates a new (or initialises an existing) audio file specified by the URL.
    @param inComponent		an AudioFileComponent
    @param inFileRef		an CFURLRef fully specifying the path of the file to create/initialise
    @param inFormat			an AudioStreamBasicDescription describing the data format that will be
							added to the audio file.
    @param inFlags			relevant flags for creating/opening the file. 
								if kAudioFileFlags_EraseFile is set, it will erase an existing file
								 if not set, then the Create call will fail if the URL is an existing file
    @result					returns noErr if successful.
}
function AudioFileComponentCreateURL( inComponent: AudioFileComponent; inFileRef: CFURLRef; const (*var*) inFormat: AudioStreamBasicDescription; inFlags: UInt32 ): OSStatus; external name '_AudioFileComponentCreateURL';
(* API_AVAILABLE(macos(10.5)) API_UNAVAILABLE(ios, watchos, tvos) *)

{!
    @function				AudioFileComponentOpenURL
    @abstract				Open an existing audio file.
    @discussion				Open an existing audio file for reading or reading and writing.
    @param inComponent		an AudioFileComponent.
    @param inFileRef		the CFURLRef of an existing audio file.
    @param inPermissions	use the permission constants.
    @param inFileDescriptor	an open file descriptor.
    @result					returns noErr if successful.
}
function AudioFileComponentOpenURL( inComponent: AudioFileComponent; inFileRef: CFURLRef; inPermissions: SInt8; inFileDescriptor: SInt32 ): OSStatus; external name '_AudioFileComponentOpenURL';
(* API_AVAILABLE(macos(10.5)) API_UNAVAILABLE(ios, watchos, tvos) *)

{!
    @function	AudioFileComponentOpenWithCallbacks
    @abstract   implements AudioFileOpenWithCallbacks
    @param inComponent		an AudioFileComponent
    @param inClientData 	a constant that will be passed to your callbacks.
	@param inReadFunc		a function that will be called when AudioFile needs to read data.
	@param inWriteFunc		a function that will be called when AudioFile needs to write data.
	@param inGetSizeFunc	a function that will be called when AudioFile needs to know the file size.
	@param inSetSizeFunc	a function that will be called when AudioFile needs to set the file size.
    @result					returns noErr if successful.
}
function AudioFileComponentOpenWithCallbacks( inComponent: AudioFileComponent; inClientData: UnivPtr; inReadFunc: AudioFile_ReadProc; inWriteFunc: AudioFile_WriteProc; inGetSizeFunc: AudioFile_GetSizeProc; inSetSizeFunc: AudioFile_SetSizeProc ): OSStatus; external name '_AudioFileComponentOpenWithCallbacks';
(* API_AVAILABLE(macos(10.4)) API_UNAVAILABLE(ios, watchos, tvos) *)

{!
    @function	AudioFileComponentInitializeWithCallbacks
    @abstract   implements AudioFileInitializeWithCallbacks
    @param inComponent		an AudioFileComponent
    @param inClientData 	a constant that will be passed to your callbacks.
	@param inReadFunc		a function that will be called when AudioFile needs to read data.
	@param inWriteFunc		a function that will be called when AudioFile needs to write data.
	@param inGetSizeFunc	a function that will be called when AudioFile needs to know the file size.
	@param inSetSizeFunc	a function that will be called when AudioFile needs to set the file size.
    @param inFileType 		an AudioFileTypeID indicating the type of audio file to which to initialize the file. 
    @param inFormat			an AudioStreamBasicDescription describing the data format that will be
							added to the audio file.
    @param inFlags			relevant flags for creating/opening the file. Currently zero.
    @result					returns noErr if successful.
}
function AudioFileComponentInitializeWithCallbacks( inComponent: AudioFileComponent; inClientData: UnivPtr; inReadFunc: AudioFile_ReadProc; inWriteFunc: AudioFile_WriteProc; inGetSizeFunc: AudioFile_GetSizeProc; inSetSizeFunc: AudioFile_SetSizeProc; inFileType: UInt32; const (*var*) inFormat: AudioStreamBasicDescription; inFlags: UInt32 ): OSStatus; external name '_AudioFileComponentInitializeWithCallbacks';
(* API_AVAILABLE(macos(10.4)) API_UNAVAILABLE(ios, watchos, tvos) *)


{!
    @function	AudioFileComponentCloseFile
    @abstract   implements AudioFileClose.
    @param inComponent		an AudioFileComponent
    @result					returns noErr if successful.
}
function AudioFileComponentCloseFile( inComponent: AudioFileComponent ): OSStatus; external name '_AudioFileComponentCloseFile';
(* API_AVAILABLE(macos(10.4)) API_UNAVAILABLE(ios, watchos, tvos) *)
				
{!
    @function	AudioFileComponentOptimize
    @abstract   implements AudioFileOptimize.
    @param inComponent		an AudioFileComponent
    @result					returns noErr if successful.
}
function AudioFileComponentOptimize( inComponent: AudioFileComponent ): OSStatus; external name '_AudioFileComponentOptimize';
(* API_AVAILABLE(macos(10.4)) API_UNAVAILABLE(ios, watchos, tvos) *)
				
{!
    @function	AudioFileComponentReadBytes
    @abstract   implements AudioFileReadBytes. 
				
    @discussion				Returns kAudioFileEndOfFileError when read encounters end of file.
    @param inComponent		an AudioFileComponent
    @param inUseCache 		true if it is desired to cache the data upon read, else false
    @param inStartingByte	the byte offset of the audio data desired to be returned
    @param ioNumBytes 		on input, the number of bytes to read, on output, the number of
							bytes actually read.
    @param outBuffer 		outBuffer should be a void * to user allocated memory large enough for the requested bytes. 
    @result					returns noErr if successful.
}
function AudioFileComponentReadBytes( inComponent: AudioFileComponent; inUseCache: Boolean; inStartingByte: SInt64; var ioNumBytes: UInt32; outBuffer: UnivPtr ): OSStatus; external name '_AudioFileComponentReadBytes';
(* API_AVAILABLE(macos(10.4)) API_UNAVAILABLE(ios, watchos, tvos) *)
						
{!
    @function				AudioFileComponentWriteBytes
    @abstract				implements AudioFileWriteBytes.
    @param inComponent		an AudioFileComponent
    @param inUseCache 		true if it is desired to cache the data upon write, else false
    @param inStartingByte	the byte offset where the audio data should be written
    @param ioNumBytes 		on input, the number of bytes to write, on output, the number of
							bytes actually written.
    @param inBuffer 		inBuffer should be a void * containing the bytes to be written 
    @result					returns noErr if successful.
}
function AudioFileComponentWriteBytes( inComponent: AudioFileComponent; inUseCache: Boolean; inStartingByte: SInt64; var ioNumBytes: UInt32; inBuffer: {const} UnivPtr ): OSStatus; external name '_AudioFileComponentWriteBytes';
(* API_AVAILABLE(macos(10.4)) API_UNAVAILABLE(ios, watchos, tvos) *)
						
{!
    @function	AudioFileComponentReadPackets
    @abstract   implements AudioFileReadPackets.
    @discussion For all uncompressed formats, packets == frames.
				ioNumPackets less than requested indicates end of file.

    @param inComponent				an AudioFileComponent
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
function AudioFileComponentReadPackets( inComponent: AudioFileComponent; inUseCache: Boolean; var outNumBytes: UInt32; outPacketDescriptions: AudioStreamPacketDescriptionPtr {* __nullable}; inStartingPacket: SInt64; var ioNumPackets: UInt32; outBuffer: UnivPtr ): OSStatus; external name '_AudioFileComponentReadPackets';
(* API_AVAILABLE(macos(10.4)) API_UNAVAILABLE(ios, watchos, tvos) *)
									

{!
    @function	AudioFileComponentReadPacketData
    @abstract   implements AudioFileReadPacketData.
    @discussion For all uncompressed formats, packets == frames.
				If the byte size of the number packets requested is 
				less than the buffer size, ioNumBytes will be reduced.
				If the buffer is too small for the number of packets 
				requested, ioNumPackets and ioNumBytes will be reduced 
				to the number of packets that can be accommodated and their byte size.
				Returns kAudioFileEndOfFileError when read encounters end of file.

    @param inComponent				an AudioFileComponent
    @param inUseCache 				true if it is desired to cache the data upon read, else false
    @param ioNumBytes				on input the size of outBuffer in bytes. 
									on output, the number of bytes actually returned.
    @param outPacketDescriptions 	on output, an array of packet descriptions describing
									the packets being returned. NULL may be passed for this
									parameter. Nothing will be returned for linear pcm data.   
    @param inStartingPacket 		the packet index of the first packet desired to be returned
    @param ioNumPackets 			on input, the number of packets to read, on output, the number of
									packets actually read.
    @param outBuffer 				outBuffer should be a pointer to user allocated memory.
    @result							returns noErr if successful.
}
function AudioFileComponentReadPacketData( inComponent: AudioFileComponent; inUseCache: Boolean; var ioNumBytes: UInt32; outPacketDescriptions: AudioStreamPacketDescriptionPtr {* __nullable}; inStartingPacket: SInt64; var ioNumPackets: UInt32; outBuffer: UnivPtr ): OSStatus; external name '_AudioFileComponentReadPacketData';
(* API_AVAILABLE(macos(10.4)) API_UNAVAILABLE(ios, watchos, tvos) *)
									
{!
    @function	AudioFileComponentWritePackets
    @abstract   implements AudioFileWritePackets.
    @discussion For all uncompressed formats, packets == frames.
    @param inComponent				an AudioFileComponent
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
function AudioFileComponentWritePackets( inComponent: AudioFileComponent; inUseCache: Boolean; inNumBytes: UInt32; {const} inPacketDescriptions: AudioStreamPacketDescriptionPtr {* __nullable}; inStartingPacket: SInt64; var ioNumPackets: UInt32; inBuffer: {const} UnivPtr ): OSStatus; external name '_AudioFileComponentWritePackets';
(* API_AVAILABLE(macos(10.4)) API_UNAVAILABLE(ios, watchos, tvos) *)


{!
    @function	AudioFileComponentGetPropertyInfo
    @abstract   implements AudioFileGetPropertyInfo.
    @param		inComponent			an AudioFileComponent
    @param      inPropertyID		an AudioFileProperty constant.
    @param      outPropertySize		the size in bytes of the current value of the property. In order to get the property value, 
									you will need a buffer of this size.
    @param      outWritable			will be set to 1 if writable, or 0 if read only.
    @result							returns noErr if successful.
}
function AudioFileComponentGetPropertyInfo( inComponent: AudioFileComponent; inPropertyID: AudioFileComponentPropertyID; outPropertySize: UInt32Ptr {* __nullable}; outWritable: UInt32Ptr {* __nullable} ): OSStatus; external name '_AudioFileComponentGetPropertyInfo';
(* API_AVAILABLE(macos(10.4)) API_UNAVAILABLE(ios, watchos, tvos) *)


{!
    @function	AudioFileComponentGetProperty
    @abstract   implements AudioFileGetProperty.
    @param		inComponent			an AudioFileComponent
    @param      inPropertyID		an AudioFileProperty constant.
    @param      ioPropertyDataSize	on input the size of the outPropertyData buffer. On output the number of bytes written to the buffer.
    @param      outPropertyData		the buffer in which to write the property data.
    @result							returns noErr if successful.
}
function AudioFileComponentGetProperty( inComponent: AudioFileComponent; inPropertyID: AudioFileComponentPropertyID; var ioPropertyDataSize: UInt32; outPropertyData: UnivPtr ): OSStatus; external name '_AudioFileComponentGetProperty';
(* API_AVAILABLE(macos(10.4)) API_UNAVAILABLE(ios, watchos, tvos) *)

{!
    @function	AudioFileComponentSetProperty
    @abstract   implements AudioFileSetProperty.
    @param		inComponent			an AudioFileComponent
    @param      inPropertyID		an AudioFileProperty constant.
    @param      inPropertyDataSize	the size of the property data.
    @param      inPropertyData		the buffer containing the property data.
    @result							returns noErr if successful.
}
function AudioFileComponentSetProperty( inComponent: AudioFileComponent; inPropertyID: AudioFileComponentPropertyID; inPropertyDataSize: UInt32; inPropertyData: {const} UnivPtr ): OSStatus; external name '_AudioFileComponentSetProperty';
(* API_AVAILABLE(macos(10.4)) API_UNAVAILABLE(ios, watchos, tvos) *)


{!
    @function	AudioFileComponentCountUserData
    @abstract   implements AudioFileCountUserData
    @discussion		"User Data" refers to chunks in AIFF, CAF and WAVE files, or resources 
					in Sound Designer II files, and possibly other things in other files.
					For simplicity, referred to below as "chunks".
    @param inComponent				an AudioFileComponent
    @param inUserDataID				the four char code of the chunk.
    @param outNumberItems			on output, if successful, number of chunks of this type in the file.
    @result							returns noErr if successful.
}
function AudioFileComponentCountUserData( inComponent: AudioFileComponent; inUserDataID: UInt32; var outNumberItems: UInt32 ): OSStatus; external name '_AudioFileComponentCountUserData';
(* API_AVAILABLE(macos(10.4)) API_UNAVAILABLE(ios, watchos, tvos) *)

{!
    @function	AudioFileComponentGetUserDataSize
    @abstract   implements AudioFileGetUserDataSize
    @param inComponent				an AudioFileComponent
    @param inUserDataID				the four char code of the chunk.
    @param inIndex					an index specifying which chunk if there are more than one.
    @param outUserDataSize			on output, if successful, the size of the user data chunk.
    @result							returns noErr if successful.
}
function AudioFileComponentGetUserDataSize( inComponent: AudioFileComponent; inUserDataID: UInt32; inIndex: UInt32; var outUserDataSize: UInt32 ): OSStatus; external name '_AudioFileComponentGetUserDataSize';
(* API_AVAILABLE(macos(10.4)) API_UNAVAILABLE(ios, watchos, tvos) *)

{!
    @function	AudioFileGetUserData
    @abstract   implements AudioFileGetUserData.
    @param		inComponent			an AudioFileComponent
    @param      inUserDataID		the four char code of the chunk.
    @param      inIndex				an index specifying which chunk if there are more than one.
	@param		ioUserDataSize		the size of the buffer on input, size of bytes copied to buffer on output 
    @param      outUserData			a pointer to a buffer in which to copy the chunk data.
    @result							returns noErr if successful.
}
function AudioFileComponentGetUserData( inComponent: AudioFileComponent; inUserDataID: UInt32; inIndex: UInt32; var ioUserDataSize: UInt32; outUserData: UnivPtr ): OSStatus; external name '_AudioFileComponentGetUserData';
(* API_AVAILABLE(macos(10.4)) API_UNAVAILABLE(ios, watchos, tvos) *)

{!
    @function	AudioFileComponentSetUserData
    @abstract   implements AudioFileSetUserData.
    @param		inComponent			an AudioFileComponent
    @param      inUserDataID		the four char code of the chunk.
    @param      inIndex				an index specifying which chunk if there are more than one.
	@param		inUserDataSize		on input the size of the data to copy, on output, size of bytes copied from the buffer  
    @param      inUserData			a pointer to a buffer from which to copy the chunk data 
									(only the contents of the chunk, not including the chunk header).
    @result							returns noErr if successful.
}
function AudioFileComponentSetUserData( inComponent: AudioFileComponent; inUserDataID: UInt32; inIndex: UInt32; inUserDataSize: UInt32; inUserData: {const} UnivPtr ): OSStatus; external name '_AudioFileComponentSetUserData';
(* API_AVAILABLE(macos(10.4)) API_UNAVAILABLE(ios, watchos, tvos) *)


{!
    @function	AudioFileComponentRemoveUserData
    @abstract   implements AudioFileRemoveUserData.
    @param		inComponent			an AudioFileComponent
    @param      inUserDataID		the four char code of the chunk.
    @param      inIndex				an index specifying which chunk if there are more than one.
    @result							returns noErr if successful.
}
function AudioFileComponentRemoveUserData( inComponent: AudioFileComponent; inUserDataID: UInt32; inIndex: UInt32 ): OSStatus; external name '_AudioFileComponentRemoveUserData';
(* API_AVAILABLE(macos(10.5)) API_UNAVAILABLE(ios, watchos, tvos) *)

//=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
//	The following calls are not made on AudioFile instances.
//  These calls are used to determine the audio file type of some data. 
//=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
{!
    @function	AudioFileComponentExtensionIsThisFormat
    @abstract   used by the AudioFile API to determine if this component is appropriate for handling a file.
    @param		inComponent			an AudioFileComponent
    @param      inExtension			a CFString containing a file name extension.
    @param      outResult			on output, is set to 1 if the extension is recognized by this component, 0 if not.
    @result							returns noErr if successful.
}
function AudioFileComponentExtensionIsThisFormat( inComponent: AudioFileComponent; inExtension: CFStringRef; var outResult: UInt32 ): OSStatus; external name '_AudioFileComponentExtensionIsThisFormat';
(* API_AVAILABLE(macos(10.4)) API_UNAVAILABLE(ios, watchos, tvos) *)


{!
    @function	AudioFileComponentFileDataIsThisFormat
    @abstract   used by the AudioFile API to determine if this component is appropriate for handling a file.
    @param		inComponent			an AudioFileComponent
    @param      inDataByteSize		the size of inData in bytes.
    @param      inData				a pointer to a buffer of audio file data.
    @param      outResult			on output, is set to 1 if the file is recognized by this component, 0 if not.
    @result							returns noErr if successful.
}
function AudioFileComponentFileDataIsThisFormat( inComponent: AudioFileComponent; inDataByteSize: UInt32; inData: {const} UnivPtr; var outResult: UInt32 ): OSStatus; external name '_AudioFileComponentFileDataIsThisFormat';
(* API_AVAILABLE(macos(10.4)) API_UNAVAILABLE(ios, watchos, tvos) *)

{$ifc false}
//=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
//	The following two calls are deprecated. 
//  Please implement AudioFileComponentFileDataIsThisFormat instead.
//=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
{!
    @function	AudioFileComponentFileIsThisFormat
    @abstract   deprecated. use AudioFileComponentFileDataIsThisFormat instead.
    @param		inComponent			an AudioFileComponent
    @param      inFileRefNum		a refNum of a file.
    @param      outResult			on output, is set to 1 if the file is recognized by this component, 0 if not.
    @result							returns noErr if successful.
}
function AudioFileComponentFileIsThisFormat( inComponent: AudioFileComponent; inFileRefNum: SInt16; var outResult: UInt32 ): OSStatus; external name '_AudioFileComponentFileIsThisFormat';
(* API_DEPRECATED("no longer supported", macos(10.4, 10.5)) API_UNAVAILABLE(ios, watchos, tvos) *) 
	
{!
    @function	AudioFileComponentDataIsThisFormat
    @abstract   deprecated. use AudioFileComponentFileDataIsThisFormat instead.
    @param		inComponent			an AudioFileComponent
    @param		inClientData		a constant that will be passed to your callbacks.
	@param		inReadFunc			a function that will be called when AudioFile needs to read data.
	@param		inWriteFunc			a function that will be called when AudioFile needs to write data.
	@param		inGetSizeFunc		a function that will be called when AudioFile needs to know the file size.
	@param		inSetSizeFunc		a function that will be called when AudioFile needs to set the file size.
    @param      outResult			on output, is set to 1 if the file data is recognized by this component, 0 if not.
    @result							returns noErr if successful.
}
function AudioFileComponentDataIsThisFormat( inComponent: AudioFileComponent; inClientData: UnivPtr {__nullable}; inReadFunc: AudioFile_ReadProc {__nullable}; inWriteFunc: AudioFile_WriteProc {__nullable}; inGetSizeFunc: AudioFile_GetSizeProc {__nullable}; inSetSizeFunc: AudioFile_SetSizeProc {__nullable}; var outResult: UInt32 ): OSStatus; external name '_AudioFileComponentDataIsThisFormat';
(* API_DEPRECATED("no longer supported", macos(10.4, 10.5)) API_UNAVAILABLE(ios, watchos, tvos) *) 
{$endc}

//=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
//	The following calls are not made on AudioFile instances.
//  They implement the AudioFileGetGlobalInfo calls.
//=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

{!
    @function	AudioFileComponentGetGlobalInfoSize
    @abstract   implements AudioFileGetGlobalInfoSize.
    @param		inComponent			an AudioFileComponent
    @param      inPropertyID		an AudioFileGlobalInfo property constant.
    @param      inSpecifierSize		The size of the specifier data.
    @param      inSpecifier			A specifier is a buffer of data used as an input argument to some of the global info properties.
    @param      outPropertySize		the size in bytes of the current value of the property. In order to get the property value, 
									you will need a buffer of this size.
    @result							returns noErr if successful.
}
function AudioFileComponentGetGlobalInfoSize( inComponent: AudioFileComponent; inPropertyID: AudioFileComponentPropertyID; inSpecifierSize: UInt32; {const} inSpecifier: UnivPtr {__nullable}; var outPropertySize: UInt32 ): OSStatus; external name '_AudioFileComponentGetGlobalInfoSize';
(* API_AVAILABLE(macos(10.4)) API_UNAVAILABLE(ios, watchos, tvos) *)

{!
    @function	AudioFileComponentGetGlobalInfo
    @abstract   implements AudioFileGetGlobalInfo.
    @param		inComponent			an AudioFileComponent
    @param      inPropertyID		an AudioFileGlobalInfo property constant.
    @param      inSpecifierSize		The size of the specifier data.
    @param      inSpecifier			A specifier is a buffer of data used as an input argument to some of the global info properties.
    @param      ioPropertyDataSize	on input the size of the outPropertyData buffer. On output the number of bytes written to the buffer.
    @param      outPropertyData		the buffer in which to write the property data.
    @result							returns noErr if successful.
}
function AudioFileComponentGetGlobalInfo( inComponent: AudioFileComponent; inPropertyID: AudioFileComponentPropertyID; inSpecifierSize: UInt32; {const} inSpecifier: UnivPtr {__nullable}; var ioPropertyDataSize: UInt32; outPropertyData: UnivPtr ): OSStatus; external name '_AudioFileComponentGetGlobalInfo';
(* API_AVAILABLE(macos(10.4)) API_UNAVAILABLE(ios, watchos, tvos) *)

//==================================================================================================
//	Properties for AudioFileComponentGetGlobalInfo. 
//==================================================================================================

{!
    @enum AudioFileComponent specific properties
    @constant   kAudioFileComponent_CanRead
					Is file type readable? Returns a UInt32 1 or 0. 
    @constant   kAudioFileComponent_CanWrite
					Is file type writeable? Returns a UInt32 1 or 0. 
    @constant   kAudioFileComponent_FileTypeName
					Returns a CFString containing the name for the file type. 
    @constant   kAudioFileComponent_ExtensionsForType
					Returns a CFArray of CFStrings containing the file extensions 
					that are recognized for this file type. 
    @constant   kAudioFileComponent_UTIsForType
					Returns a CFArray of CFStrings containing the universal type identifiers 
					for this file type. 
    @constant   kAudioFileComponent_MIMETypesForType
					Returns a CFArray of CFStrings containing the MIME types 
					for this file type. 
    @constant   kAudioFileComponent_AvailableFormatIDs
					Returns a array of format IDs for formats that can be read. 
    @constant   kAudioFileComponent_AvailableStreamDescriptionsForFormat
					The specifier is the format ID for the requested format.
					Returns an array of AudioStreamBasicDescriptions which have all of the 
					formats for a particular file type and format ID. The AudioStreamBasicDescriptions
					have the following fields filled in: mFormatID, mFormatFlags, mBitsPerChannel
    @constant   kAudioFileComponent_FastDispatchTable
					Deprecated. This selector is no longer called by the implementation. 
    @constant   kAudioFileComponent_HFSTypeCodesForType
					Returns an array of HFSTypeCodes corresponding to this file type.
					The first type in the array is the preferred one for use when
					writing new files.
}
const
	kAudioFileComponent_CanRead = FourCharCode('cnrd');
	kAudioFileComponent_CanWrite = FourCharCode('cnwr');
	kAudioFileComponent_FileTypeName = FourCharCode('ftnm');
	kAudioFileComponent_UTIsForType = FourCharCode('futi');
	kAudioFileComponent_MIMETypesForType = FourCharCode('fmim');
	kAudioFileComponent_ExtensionsForType = FourCharCode('fext');
	kAudioFileComponent_AvailableFormatIDs = FourCharCode('fmid');
	kAudioFileComponent_AvailableStreamDescriptionsForFormat = FourCharCode('sdid');
	kAudioFileComponent_FastDispatchTable = FourCharCode('fdft');
	kAudioFileComponent_HFSTypeCodesForType = FourCharCode('fhfs'); 

//==================================================================================================
//	Selectors for the component routines 
//==================================================================================================

const
	kAudioFileCreateSelect = $0001;
	kAudioFileOpenSelect = $0002;
	kAudioFileInitializeSelect = $0003;
	kAudioFileOpenWithCallbacksSelect = $0004;
	kAudioFileInitializeWithCallbacksSelect = $0005;
	kAudioFileCloseSelect = $0006;
	kAudioFileOptimizeSelect = $0007;
	kAudioFileReadBytesSelect = $0008;
	kAudioFileWriteBytesSelect = $0009;
	kAudioFileReadPacketsSelect = $000A;
	kAudioFileWritePacketsSelect = $000B;
	kAudioFileGetPropertyInfoSelect = $000C;
	kAudioFileGetPropertySelect = $000D;
	kAudioFileSetPropertySelect = $000E;
	kAudioFileExtensionIsThisFormatSelect = $000F;
	kAudioFileFileIsThisFormatSelect = $0010;
	kAudioFileDataIsThisFormatSelect = $0011;
	kAudioFileGetGlobalInfoSizeSelect = $0012;
	kAudioFileGetGlobalInfoSelect = $0013;
	kAudioFileCountUserDataSelect = $0014;
	kAudioFileGetUserDataSizeSelect = $0015;
	kAudioFileGetUserDataSelect = $0016;
	kAudioFileSetUserDataSelect = $0017;
	kAudioFileRemoveUserDataSelect = $0018;
	kAudioFileCreateURLSelect = $0019;
	kAudioFileOpenURLSelect = $001A;
	kAudioFileFileDataIsThisFormatSelect = $001B;
	kAudioFileReadPacketDataSelect = $001C; 


//#pragma mark -
//#pragma mark Deprecated

//==================================================================================================
// Fast Dispatch Function typedefs. Deprecated. These are no longer used by the implementation.
//==================================================================================================

type
	ReadBytesFDF = function( inComponentStorage: UnivPtr; inUseCache: Boolean; inStartingByte: SInt64; var ioNumBytes: UInt32; outBuffer: UnivPtr ): OSStatus;
								
type
	WriteBytesFDF = function( inComponentStorage: UnivPtr; inUseCache: Boolean; inStartingByte: SInt64; var ioNumBytes: UInt32; inBuffer: {const} UnivPtr ): OSStatus;
							
type
	ReadPacketsFDF = function( inComponentStorage: UnivPtr; inUseCache: Boolean; var outNumBytes: UInt32; outPacketDescriptions: AudioStreamPacketDescriptionPtr {* __nullable}; inStartingPacket: SInt64; var ioNumPackets: UInt32; outBuffer: UnivPtr ): OSStatus;

type
	ReadPacketDataFDF = function( inComponentStorage: UnivPtr; inUseCache: Boolean; var ioNumBytes: UInt32; outPacketDescriptions: AudioStreamPacketDescriptionPtr {* __nullable}; inStartingPacket: SInt64; var ioNumPackets: UInt32; outBuffer: UnivPtr ): OSStatus;

type
	WritePacketsFDF = function( inComponentStorage: UnivPtr; inUseCache: Boolean; inNumBytes: UInt32; {const} inPacketDescriptions: AudioStreamPacketDescriptionPtr {* __nullable}; inStartingPacket: SInt64; var ioNumPackets: UInt32; inBuffer: {const} UnivPtr ): OSStatus;
								
type
	GetPropertyInfoFDF = function( inComponentStorage: UnivPtr; inPropertyID: AudioFilePropertyID; outDataSize: UInt32Ptr {* __nullable}; isWritable: UInt32Ptr {* __nullable} ): OSStatus;
								
type
	GetPropertyFDF = function( inComponentStorage: UnivPtr; inPropertyID: AudioFilePropertyID; var ioDataSize: UInt32; ioPropertyData: UnivPtr ): OSStatus;
							
type
	SetPropertyFDF = function( inComponentStorage: UnivPtr; inPropertyID: AudioFilePropertyID; inDataSize: UInt32; inPropertyData: {const} UnivPtr ): OSStatus;

type
	CountUserDataFDF = function( inComponentStorage: UnivPtr; inUserDataID: UInt32; var outNumberItems: UInt32 ): OSStatus;

type
	GetUserDataSizeFDF = function( inComponentStorage: UnivPtr; inUserDataID: UInt32; inIndex: UInt32; var outDataSize: UInt32 ): OSStatus;

type
	GetUserDataFDF = function( inComponentStorage: UnivPtr; inUserDataID: UInt32; inIndex: UInt32; var ioUserDataSize: UInt32; outUserData: UnivPtr ): OSStatus;

type
	SetUserDataFDF = function( inComponentStorage: UnivPtr; inUserDataID: UInt32; inIndex: UInt32; inUserDataSize: UInt32; inUserData: {const} UnivPtr ): OSStatus;
										
{ no fast dispatch for kAudioFileRemoveUserDataSelect }

//#pragma /mark -
//#pragma /mark Deprecated

//==================================================================================================
// Fast Dispatch Function tables. Deprecated. These are no longer used by the implementation.
//==================================================================================================

type
	AudioFileFDFTable = record
		mComponentStorage: UnivPtr;
	 	mReadBytesFDF: ReadBytesFDF;
		mWriteBytesFDF: WriteBytesFDF;
		mReadPacketsFDF: ReadPacketsFDF;
		mWritePacketsFDF: WritePacketsFDF;

		mGetPropertyInfoFDF: GetPropertyInfoFDF;
		mGetPropertyFDF: GetPropertyFDF;
		mSetPropertyFDF: SetPropertyFDF;
	
		mCountUserDataFDF: CountUserDataFDF;
		mGetUserDataSizeFDF: GetUserDataSizeFDF;
		mGetUserDataFDF: GetUserDataFDF;
		mSetUserDataFDF: SetUserDataFDF;
	end;
	AudioFileFDFTablePtr = ^AudioFileFDFTable;
	(* API_DEPRECATED("no longer supported", macos(10.4, 10.7)) API_UNAVAILABLE(ios, watchos, tvos); *)

type
	AudioFileFDFTableExtended = record
		mComponentStorage: UnivPtr;
	 	mReadBytesFDF: ReadBytesFDF;
		mWriteBytesFDF: WriteBytesFDF;
		mReadPacketsFDF: ReadPacketsFDF;
		mWritePacketsFDF: WritePacketsFDF;

		mGetPropertyInfoFDF: GetPropertyInfoFDF;
		mGetPropertyFDF: GetPropertyFDF;
		mSetPropertyFDF: SetPropertyFDF;
	
		mCountUserDataFDF: CountUserDataFDF;
		mGetUserDataSizeFDF: GetUserDataSizeFDF;
		mGetUserDataFDF: GetUserDataFDF;
		mSetUserDataFDF: SetUserDataFDF;

		mReadPacketDataFDF: ReadPacketDataFDF;
	end;
	AudioFileFDFTableExtendedPtr = ^AudioFileFDFTableExtended;
	(* API_DEPRECATED("no longer supported", macos(10.4, 10.7)) API_UNAVAILABLE(ios, watchos, tvos);*)

{!
	@functiongroup Deprecated AFComponent
	@discussion		These API calls are no longer called on Snow Leopard, instead the URL versions are used.
					They can be provided by the file component for compatibility with Leopard and Tiger systems
}

{!
    @function	AudioFileComponentCreate
    @abstract   implements AudioFileCreate
    @param inComponent		an AudioFileComponent
    @param inParentRef		an FSRef to the directory where  the new file should be created.
    @param inFileName		a CFStringRef containing the name of the file to be created.
    @param inFormat			an AudioStreamBasicDescription describing the data format that will be
							added to the audio file.
    @param inFlags			relevant flags for creating/opening the file. Currently zero.
    @param outNewFileRef	if successful, the FSRef of the newly created file.
    @result					returns noErr if successful.
}
function AudioFileComponentCreate( inComponent: AudioFileComponent; {const} inParentRef: FSRefPtr; inFileName: CFStringRef; const (*var*) inFormat: AudioStreamBasicDescription; inFlags: UInt32; outNewFileRef: FSRefPtr ): OSStatus; external name '_AudioFileComponentCreate';
(* API_DEPRECATED("no longer supported", macos(10.4, 10.6)) API_UNAVAILABLE(ios, watchos, tvos) *)
                                

{!
    @function	AudioFileComponentInitialize
    @abstract   implements AudioFileInitialize
    @param inComponent		an AudioFileComponent
    @param inFileRef		the FSRef of an existing audio file.
    @param inFormat			an AudioStreamBasicDescription describing the data format that will be
							added to the audio file.
    @param inFlags			flags for creating/opening the file. Currently zero.
    @result					returns noErr if successful.
}
function AudioFileComponentInitialize( inComponent: AudioFileComponent; {const} inFileRef: FSRefPtr; const (*var*) inFormat: AudioStreamBasicDescription; inFlags: UInt32 ): OSStatus; external name '_AudioFileComponentInitialize';
(* API_DEPRECATED("no longer supported", macos(10.4, 10.6)) API_UNAVAILABLE(ios, watchos, tvos) *)
							
{!
    @function	AudioFileComponentOpenFile
    @abstract   implements AudioFileOpen
    @param inComponent		an AudioFileComponent
    @param inFileRef		the FSRef of an existing audio file.
    @param inPermissions	use the permission constants
    @param inRefNum			the file refNum for the opened file. This avoids opening the file twice
							- once to determine which file type to which to delegate and once to parse it.
    @result					returns noErr if successful.
}
function AudioFileComponentOpenFile( inComponent: AudioFileComponent; {const} inFileRef: FSRefPtr; inPermissions: SInt8; inRefNum: SInt16 ): OSStatus; external name '_AudioFileComponentOpenFile';
(* API_DEPRECATED("no longer supported", macos(10.4, 10.6)) API_UNAVAILABLE(ios, watchos, tvos) *)


//=====================================================================================================================

type
	AudioFileComponentCreateURLProc = function( self: UnivPtr; inFileRef: CFURLRef; const (*var*) inFormat: AudioStreamBasicDescription; inFlags: UInt32 ): OSStatus;
	AudioFileComponentOpenURLProc = function( self: UnivPtr; inFileRef: CFURLRef; inPermissions: SInt8; inFileDescriptor: SInt32 ): OSStatus;
								
type
	AudioFileComponentOpenWithCallbacksProc = function( self: UnivPtr; inClientData: UnivPtr; inReadFunc: AudioFile_ReadProc; inWriteFunc: AudioFile_WriteProc; inGetSizeFunc: AudioFile_GetSizeProc; inSetSizeFunc: AudioFile_SetSizeProc ): OSStatus;

type
	AudioFileComponentInitializeWithCallbacksProc = function( self: UnivPtr; inClientData: UnivPtr; inReadFunc: AudioFile_ReadProc; inWriteFunc: AudioFile_WriteProc; inGetSizeFunc: AudioFile_GetSizeProc; inSetSizeFunc: AudioFile_SetSizeProc; inFileType: UInt32; const (*var*) inFormat: AudioStreamBasicDescription; inFlags: UInt32 ): OSStatus;

type
	AudioFileComponentCloseProc = function( self: UnivPtr ): OSStatus;
				
type
	AudioFileComponentOptimizeProc = function( self: UnivPtr ): OSStatus;
				
type
	AudioFileComponentReadBytesProc = function( self: UnivPtr; inUseCache: Boolean; inStartingByte: SInt64; var ioNumBytes: UInt32; outBuffer: UnivPtr ): OSStatus;	
						
type
	AudioFileComponentWriteBytesProc = function( self: UnivPtr; inUseCache: Boolean; inStartingByte: SInt64; var ioNumBytes: UInt32; inBuffer: {const} UnivPtr ): OSStatus;	
						
type
	AudioFileComponentReadPacketsProc = function( self: UnivPtr; inUseCache: Boolean; var outNumBytes: UInt32; outPacketDescriptions: AudioStreamPacketDescriptionPtr {* __nullable}; inStartingPacket: SInt64; var ioNumPackets: UInt32; outBuffer: UnivPtr ): OSStatus;	
									

type
	AudioFileComponentReadPacketDataProc = function( self: UnivPtr; inUseCache: Boolean; var ioNumBytes: UInt32; outPacketDescriptions: AudioStreamPacketDescription {* __nullable}; inStartingPacket: SInt64; var ioNumPackets: UInt32; outBuffer: UnivPtr ): OSStatus;	
									
type
	AudioFileComponentWritePacketsProc = function( self: UnivPtr; inUseCache: Boolean; inNumBytes: UInt32; {const} inPacketDescriptions: AudioStreamPacketDescription {* __nullable}; inStartingPacket: SInt64; var ioNumPackets: UInt32; inBuffer: {const} UnivPtr ): OSStatus;


type
	AudioFileComponentGetPropertyInfoProc = function( self: UnivPtr; inPropertyID: AudioFileComponentPropertyID; outPropertySize: UInt32Ptr {* __nullable}; outWritable: UInt32Ptr {* __nullable} ): OSStatus;


type
	AudioFileComponentGetPropertyProc = function( self: UnivPtr; inPropertyID: AudioFileComponentPropertyID; var ioPropertyDataSize: UInt32; outPropertyData: UnivPtr ): OSStatus;

type
	AudioFileComponentSetPropertyProc = function( self: UnivPtr; inPropertyID: AudioFileComponentPropertyID; inPropertyDataSize: UInt32; inPropertyData: {const} UnivPtr ): OSStatus;


type
	AudioFileComponentCountUserDataProc = function( self: UnivPtr; inUserDataID: UInt32; var outNumberItems: UInt32 ): OSStatus;

type
	AudioFileComponentGetUserDataSizeProc = function( self: UnivPtr; inUserDataID: UInt32; inIndex: UInt32; var outUserDataSize: UInt32 ): OSStatus;

type
	AudioFileComponentGetUserDataProc = function( self: UnivPtr; inUserDataID: UInt32; inIndex: UInt32; var ioUserDataSize: UInt32; outUserData: UnivPtr ): OSStatus;

type
	AudioFileComponentSetUserDataProc = function( self: UnivPtr; inUserDataID: UInt32; inIndex: UInt32; inUserDataSize: UInt32; inUserData: {const} UnivPtr ): OSStatus;


type
	AudioFileComponentRemoveUserDataProc = function( self: UnivPtr; inUserDataID: UInt32; inIndex: UInt32 ): OSStatus;

type
	AudioFileComponentExtensionIsThisFormatProc = function( self: UnivPtr; inExtension: CFStringRef; var outResult: UInt32 ): OSStatus;	


type
	AudioFileComponentFileDataIsThisFormatProc = function( self: UnivPtr; inDataByteSize: UInt32; inData: {const} UnivPtr; var outResult: UInt32 ): OSStatus;	


type
	AudioFileComponentGetGlobalInfoSizeProc = function( self: UnivPtr; inPropertyID: AudioFileComponentPropertyID; inSpecifierSize: UInt32; {const} inSpecifier: UnivPtr {__nullable}; var outPropertySize: UInt32 ): OSStatus;

type
	AudioFileComponentGetGlobalInfoProc = function( self: UnivPtr; inPropertyID: AudioFileComponentPropertyID; inSpecifierSize: UInt32; {const} inSpecifier: UnivPtr {__nullable}; var ioPropertyDataSize: UInt32; outPropertyData: UnivPtr ): OSStatus;

{$endc} {TARGET_OS_MAC}
//CF_ASSUME_NONNULL_END

{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}

end.
{$endc} {not MACOSALLINCLUDE}
