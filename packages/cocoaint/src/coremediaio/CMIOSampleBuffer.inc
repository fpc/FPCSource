{ Parsed from CoreMediaIO.framework CMIOSampleBuffer.h }

{$if  not (defined(__CMIOSampleBuffer_h__))}

{$ifdef TYPES}
const
  kCMIOSampleBufferNoDiscontinuities = 0;
  kCMIOSampleBufferDiscontinuityFlag_UnknownDiscontinuity = 1 shl 0;
  kCMIOSampleBufferDiscontinuityFlag_TimecodeDiscontinuity = 1 shl 1;
  kCMIOSampleBufferDiscontinuityFlag_PacketError = 1 shl 2;
  kCMIOSampleBufferDiscontinuityFlag_StreamDiscontinuity = 1 shl 3;
  kCMIOSampleBufferDiscontinuityFlag_MalformedData = 1 shl 4;
  kCMIOSampleBufferDiscontinuityFlag_DataWasFlushed = 1 shl 5;
  kCMIOSampleBufferDiscontinuityFlag_DataWasDropped = 1 shl 6;
  kCMIOSampleBufferDiscontinuityFlag_BufferOverrun = 1 shl 7;
  kCMIOSampleBufferDiscontinuityFlag_DiscontinuityInDTS = 1 shl 8;
  kCMIOSampleBufferDiscontinuityFlag_RelatedToDiscontinuity = 1 shl 9;
  kCMIOSampleBufferDiscontinuityFlag_ClientSyncDiscontinuity = 1 shl 10;
  kCMIOSampleBufferDiscontinuityFlag_TrickPlay = 1 shl 11;
  kCMIOSampleBufferDiscontinuityFlag_NoDataMarker = 1 shl 12;
  kCMIOSampleBufferDiscontinuityFlag_DataFormatChanged = 1 shl 13;
  kCMIOSampleBufferDiscontinuityFlag_TimingReferenceJumped = 1 shl 14;
  kCMIOSampleBufferDiscontinuityFlag_DurationWasExtended = 1 shl 15;
  kCMIOSampleBufferDiscontinuityFlag_SleepWakeCycle = 1 shl 16;
  kCMIOSampleBufferDiscontinuityFlag_CodecSettingsChanged = 1 shl 17;
const
  kCMIOSampleBufferNoDataEvent_Unknown = 0;
  kCMIOSampleBufferNoDataEvent_NoMedia = 1;
  kCMIOSampleBufferNoDataEvent_DeviceDidNotSync = 2;
  kCMIOSampleBufferNoDataEvent_DeviceInWrongMode = 3;
  kCMIOSampleBufferNoDataEvent_ProcessingError = 4;
  kCMIOSampleBufferNoDataEvent_SleepWakeCycle = 5;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  kCMIOSampleBufferAttachmentKey_DiscontinuityFlags: CFStringRef; cvar; external;
  kCMIOSampleBufferAttachmentKey_SequenceNumber: CFStringRef; cvar; external;
  kCMIOSampleBufferAttachmentKey_HDV1_PackData: CFStringRef; cvar; external;
  kCMIOSampleBufferAttachmentKey_HDV2_VAUX: CFStringRef; cvar; external;
  kCMIOSampleBufferAttachmentKey_CAAudioTimeStamp: CFStringRef; cvar; external;
  kCMIOSampleBufferAttachmentKey_SMPTETime: CFStringRef; cvar; external;
  kCMIOSampleBufferAttachmentKey_NativeSMPTEFrameCount: CFStringRef; cvar; external;
  kCMIOSampleBufferAttachmentKey_NumberOfVideoFramesInBuffer: CFStringRef; cvar; external;
  kCMIOSampleBufferAttachmentKey_NumberOfVideoFramesInGOP: CFStringRef; cvar; external;
  kCMIOSampleBufferAttachmentKey_MuxedSourcePresentationTimeStamp: CFStringRef; cvar; external;
  kCMIOSampleBufferAttachmentKey_HostTime: CFStringRef; cvar; external;
  kCMIOSampleBufferAttachmentKey_RepeatedBufferContents: CFStringRef; cvar; external;
  kCMIOSampleBufferAttachmentKey_SourceAudioFormatDescription: CFStringRef; cvar; external;
  kCMIOSampleBufferAttachmentKey_PulldownCadenceInfo: CFStringRef; cvar; external;
  kCMIOSampleBufferAttachmentKey_ClosedCaptionSampleBuffer: CFStringRef; cvar; external;
  kCMIOSampleBufferAttachmentKey_ClientSequenceID: CFStringRef; cvar; external;
  kCMIOSampleBufferAttachmentKey_MouseAndKeyboardModifiers: CFStringRef; cvar; external;
  kCMIOSampleBufferAttachment_MouseAndKeyboardModifiersKey_CursorPositionX: CFStringRef; cvar; external;
  kCMIOSampleBufferAttachment_MouseAndKeyboardModifiersKey_CursorPositionY: CFStringRef; cvar; external;
  kCMIOSampleBufferAttachment_MouseAndKeyboardModifiersKey_MouseButtonState: CFStringRef; cvar; external;
  kCMIOSampleBufferAttachment_MouseAndKeyboardModifiersKey_KeyboardModifiers: CFStringRef; cvar; external;
  kCMIOSampleBufferAttachmentKey_NoDataMarker: CFStringRef; cvar; external;
  kCMIOBlockBufferAttachmentKey_CVPixelBufferReference: CFStringRef; cvar; external;
{$endif}

{$ifdef FUNCTIONS}
function CMIOSampleBufferCreate(allocator: CFAllocatorRef; dataBuffer: CMBlockBufferRef; formatDescription: CMFormatDescriptionRef; numSamples: UInt32; numSampleTimingEntries: UInt32; sampleTimingArray: CMSampleTimingInfoPtr; numSampleSizeEntries: UInt32; sampleSizeArray: size_tPtr; sequenceNumber: UInt64; discontinuityFlags: UInt32; sBufOut: CMSampleBufferRef): OSStatus; cdecl; external;
function CMIOSampleBufferCreateNoDataMarker(allocator: CFAllocatorRef; noDataEvent: UInt32; formatDescription: CMFormatDescriptionRef; sequenceNumber: UInt64; discontinuityFlags: UInt32; sBufOut: CMSampleBufferRef): OSStatus; cdecl; external;
procedure CMIOSampleBufferSetSequenceNumber(allocator: CFAllocatorRef; sbuf: CMSampleBufferRef; sequenceNumber: UInt64); cdecl; external;
function CMIOSampleBufferGetSequenceNumber(sbuf: CMSampleBufferRef): UInt64; cdecl; external;
procedure CMIOSampleBufferSetDiscontinuityFlags(allocator: CFAllocatorRef; sbuf: CMSampleBufferRef; discontinuityFlags: UInt32); cdecl; external;
function CMIOSampleBufferGetDiscontinuityFlags(sbuf: CMSampleBufferRef): UInt32; cdecl; external;
function CMIOSampleBufferCopyNonRequiredAttachments(sourceSBuf: CMSampleBufferRef; destSBuf: CMSampleBufferRef; attachmentMode: CMAttachmentMode): OSStatus; cdecl; external;
function CMIOSampleBufferCopySampleAttachments(sourceSBuf: CMSampleBufferRef; destSBuf: CMSampleBufferRef): OSStatus; cdecl; external;
{$endif}

{$endif}
