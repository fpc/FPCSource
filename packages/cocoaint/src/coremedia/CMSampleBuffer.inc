{ Parsed from CoreMedia.framework CMSampleBuffer.h }

{$ifndef CMSAMPLEBUFFER_H}

{$ifdef TYPES}
const
  kCMSampleBufferError_AllocationFailed = -12730;
  kCMSampleBufferError_RequiredParameterMissing = -12731;
  kCMSampleBufferError_AlreadyHasDataBuffer = -12732;
  kCMSampleBufferError_BufferNotReady = -12733;
  kCMSampleBufferError_SampleIndexOutOfRange = -12734;
  kCMSampleBufferError_BufferHasNoSampleSizes = -12735;
  kCMSampleBufferError_BufferHasNoSampleTimingInfo = -12736;
  kCMSampleBufferError_ArrayTooSmall = -12737;
  kCMSampleBufferError_InvalidEntryCount = -12738;
  kCMSampleBufferError_CannotSubdivide = -12739;
  kCMSampleBufferError_SampleTimingInfoInvalid = -12740;
  kCMSampleBufferError_InvalidMediaTypeForOperation = -12741;
  kCMSampleBufferError_InvalidSampleData = -12742;
  kCMSampleBufferError_InvalidMediaFormat = -12743;
  kCMSampleBufferError_Invalidated = -12744;
  kCMSampleBufferError_DataFailed = -16750;
  kCMSampleBufferError_DataCanceled = -16751;
const
  kCMSampleBufferFlag_AudioBufferList_Assure16ByteAlignment = 1 shl 0;

type
  CMSampleBufferRef = OpaqueType;

type
  CMSampleTimingInfo = record
    duration: CMTime;
    presentationTimeStamp: CMTime;
    decodeTimeStamp: CMTime;
  end;
type
  CMSampleTimingInfoPtr = ^CMSampleTimingInfo;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  kCMTimingInfoInvalid: CMSampleTimingInfo; cvar; external;
{$endif}

{$ifdef TYPES}
type
  CMSampleBufferMakeDataReadyCallback = function (sbuf: CMSampleBufferRef; makeDataReadyRefcon: pointer): OSStatus; cdecl;
{$endif}

{$ifdef FUNCTIONS}
function CMSampleBufferCreate(allocator: CFAllocatorRef; dataBuffer: CMBlockBufferRef; dataReady: Boolean; makeDataReadyCallback: CMSampleBufferMakeDataReadyCallback; makeDataReadyRefcon: pointer; formatDescription: CMFormatDescriptionRef; numSamples: CMItemCount; numSampleTimingEntries: CMItemCount; sampleTimingArray: CMSampleTimingInfoPtr; numSampleSizeEntries: CMItemCount; sampleSizeArray: size_tPtr; sBufOut: CMSampleBufferRef): OSStatus; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMSampleBufferCreateReady(allocator: CFAllocatorRef; dataBuffer: CMBlockBufferRef; formatDescription: CMFormatDescriptionRef; numSamples: CMItemCount; numSampleTimingEntries: CMItemCount; sampleTimingArray: CMSampleTimingInfoPtr; numSampleSizeEntries: CMItemCount; sampleSizeArray: size_tPtr; sBufOut: CMSampleBufferRef): OSStatus; cdecl; external; { available starting in __MAC_10_10,__IPHONE_8_0 }
function CMAudioSampleBufferCreateWithPacketDescriptions(allocator: CFAllocatorRef; dataBuffer: CMBlockBufferRef; dataReady: Boolean; makeDataReadyCallback: CMSampleBufferMakeDataReadyCallback; makeDataReadyRefcon: pointer; formatDescription: CMFormatDescriptionRef; numSamples: CMItemCount; sbufPTS: CMTime; packetDescriptions: AudioStreamPacketDescriptionPtr; sBufOut: CMSampleBufferRef): OSStatus; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMAudioSampleBufferCreateReadyWithPacketDescriptions(allocator: CFAllocatorRef; dataBuffer: CMBlockBufferRef; formatDescription: CMFormatDescriptionRef; numSamples: CMItemCount; sbufPTS: CMTime; packetDescriptions: AudioStreamPacketDescriptionPtr; sBufOut: CMSampleBufferRef): OSStatus; cdecl; external; { available starting in __MAC_10_10,__IPHONE_8_0 }
function CMSampleBufferCreateForImageBuffer(allocator: CFAllocatorRef; imageBuffer: CVImageBufferRef; dataReady: Boolean; makeDataReadyCallback: CMSampleBufferMakeDataReadyCallback; makeDataReadyRefcon: pointer; formatDescription: CMVideoFormatDescriptionRef; sampleTiming: CMSampleTimingInfoPtr; sBufOut: CMSampleBufferRef): OSStatus; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMSampleBufferCreateReadyWithImageBuffer(allocator: CFAllocatorRef; imageBuffer: CVImageBufferRef; formatDescription: CMVideoFormatDescriptionRef; sampleTiming: CMSampleTimingInfoPtr; sBufOut: CMSampleBufferRef): OSStatus; cdecl; external; { available starting in __MAC_10_10,__IPHONE_8_0 }
function CMSampleBufferCreateCopy(allocator: CFAllocatorRef; sbuf: CMSampleBufferRef; sbufCopyOut: CMSampleBufferRef): OSStatus; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMSampleBufferCreateCopyWithNewTiming(allocator: CFAllocatorRef; originalSBuf: CMSampleBufferRef; numSampleTimingEntries: CMItemCount; sampleTimingArray: CMSampleTimingInfoPtr; sBufCopyOut: CMSampleBufferRef): OSStatus; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMSampleBufferCopySampleBufferForRange(allocator: CFAllocatorRef; sbuf: CMSampleBufferRef; sampleRange: CFRange; sBufOut: CMSampleBufferRef): OSStatus; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMSampleBufferGetTypeID: CFTypeID; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMSampleBufferSetDataBuffer(sbuf: CMSampleBufferRef; dataBuffer: CMBlockBufferRef): OSStatus; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMSampleBufferGetDataBuffer(sbuf: CMSampleBufferRef): CMBlockBufferRef; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMSampleBufferGetImageBuffer(sbuf: CMSampleBufferRef): CVImageBufferRef; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMSampleBufferSetDataBufferFromAudioBufferList(sbuf: CMSampleBufferRef; bbufStructAllocator: CFAllocatorRef; bbufMemoryAllocator: CFAllocatorRef; flags: cuint32; bufferList: AudioBufferListPtr): OSStatus; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMSampleBufferGetAudioBufferListWithRetainedBlockBuffer(sbuf: CMSampleBufferRef; bufferListSizeNeededOut: size_tPtr; bufferListOut: AudioBufferListPtr; bufferListSize: size_t; bbufStructAllocator: CFAllocatorRef; bbufMemoryAllocator: CFAllocatorRef; flags: cuint32; blockBufferOut: CMBlockBufferRef): OSStatus; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMSampleBufferGetAudioStreamPacketDescriptions(sbuf: CMSampleBufferRef; packetDescriptionsSize: size_t; packetDescriptionsOut: AudioStreamPacketDescriptionPtr; packetDescriptionsSizeNeededOut: size_tPtr): OSStatus; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMSampleBufferGetAudioStreamPacketDescriptionsPtr(sbuf: CMSampleBufferRef; packetDescriptionsPtrOut: AudioStreamPacketDescriptionPtr; packetDescriptionsSizeOut: size_tPtr): OSStatus; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMSampleBufferCopyPCMDataIntoAudioBufferList(sbuf: CMSampleBufferRef; frameOffset: cint32; numFrames: cint32; bufferList: AudioBufferListPtr): OSStatus; cdecl; external; { available starting in __MAC_10_9,__IPHONE_7_0 }
function CMSampleBufferSetDataReady(sbuf: CMSampleBufferRef): OSStatus; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMSampleBufferDataIsReady(sbuf: CMSampleBufferRef): Boolean; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMSampleBufferSetDataFailed(sbuf: CMSampleBufferRef; status: OSStatus): OSStatus; cdecl; external; { available starting in __MAC_10_10,__IPHONE_8_0 }
function CMSampleBufferHasDataFailed(sbuf: CMSampleBufferRef; statusOut: pointer {OSStatusPtr}): Boolean; cdecl; external; { available starting in __MAC_10_10,__IPHONE_8_0 }
function CMSampleBufferMakeDataReady(sbuf: CMSampleBufferRef): OSStatus; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMSampleBufferTrackDataReadiness(sbuf: CMSampleBufferRef; sbufToTrack: CMSampleBufferRef): OSStatus; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMSampleBufferInvalidate(sbuf: CMSampleBufferRef): OSStatus; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
{$endif}

{$ifdef TYPES}
type
  CMSampleBufferInvalidateCallback = procedure (sbuf: CMSampleBufferRef; invalidateRefCon: cuint64); cdecl;
{$endif}

{$ifdef FUNCTIONS}
function CMSampleBufferSetInvalidateCallback(sbuf: CMSampleBufferRef; invalidateCallback: CMSampleBufferInvalidateCallback; invalidateRefCon: cuint64): OSStatus; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
{$endif}

{$if defined(__BLOCKS__)}
{$ifdef TYPES}
type
  CMSampleBufferInvalidateHandler = OpaqueCBlock;
{$endif}

{$ifdef FUNCTIONS}
function CMSampleBufferSetInvalidateHandler(sbuf: CMSampleBufferRef; invalidateHandler: CMSampleBufferInvalidateHandler): OSStatus; cdecl; external; { available starting in __MAC_10_10,__IPHONE_8_0 }
{$endif}

{$endif}
{$ifdef FUNCTIONS}
function CMSampleBufferIsValid(sbuf: CMSampleBufferRef): Boolean; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  kCMSampleBufferNotification_DataBecameReady: CFStringRef { available starting in __MAC_10_7,__IPHONE_4_0 }; cvar; external;
  kCMSampleBufferNotification_DataFailed: CFStringRef { available starting in __MAC_10_10,__IPHONE_8_0 }; cvar; external;
  kCMSampleBufferNotificationParameter_OSStatus: CFStringRef { available starting in __MAC_10_10,__IPHONE_8_0 }; cvar; external;
  kCMSampleBufferConduitNotification_InhibitOutputUntil: CFStringRef; cvar; external;
  kCMSampleBufferConduitNotificationParameter_ResumeTag: CFStringRef; cvar; external;
  kCMSampleBufferConduitNotification_ResetOutput: CFStringRef { available starting in __MAC_10_7,__IPHONE_4_0 }; cvar; external;
  kCMSampleBufferConduitNotification_UpcomingOutputPTSRangeChanged: CFStringRef; cvar; external;
  kCMSampleBufferConduitNotificationParameter_UpcomingOutputPTSRangeMayOverlapQueuedOutputPTSRange: CFStringRef; cvar; external;
  kCMSampleBufferConduitNotificationParameter_MinUpcomingOutputPTS: CFStringRef; cvar; external;
  kCMSampleBufferConduitNotificationParameter_MaxUpcomingOutputPTS: CFStringRef; cvar; external;
  kCMSampleBufferConsumerNotification_BufferConsumed: CFStringRef; cvar; external;
{$endif}

{$ifdef FUNCTIONS}
function CMSampleBufferGetNumSamples(sbuf: CMSampleBufferRef): CMItemCount; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMSampleBufferGetDuration(sbuf: CMSampleBufferRef): CMTime; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMSampleBufferGetPresentationTimeStamp(sbuf: CMSampleBufferRef): CMTime; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMSampleBufferGetDecodeTimeStamp(sbuf: CMSampleBufferRef): CMTime; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMSampleBufferGetOutputDuration(sbuf: CMSampleBufferRef): CMTime; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMSampleBufferGetOutputPresentationTimeStamp(sbuf: CMSampleBufferRef): CMTime; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMSampleBufferSetOutputPresentationTimeStamp(sbuf: CMSampleBufferRef; outputPresentationTimeStamp: CMTime): OSStatus; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMSampleBufferGetOutputDecodeTimeStamp(sbuf: CMSampleBufferRef): CMTime; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMSampleBufferGetSampleTimingInfoArray(sbuf: CMSampleBufferRef; timingArrayEntries: CMItemCount; timingArrayOut: CMSampleTimingInfoPtr; timingArrayEntriesNeededOut: CMItemCountPtr): OSStatus; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMSampleBufferGetOutputSampleTimingInfoArray(sbuf: CMSampleBufferRef; timingArrayEntries: CMItemCount; timingArrayOut: CMSampleTimingInfoPtr; timingArrayEntriesNeededOut: CMItemCountPtr): OSStatus; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMSampleBufferGetSampleTimingInfo(sbuf: CMSampleBufferRef; sampleIndex: CMItemIndex; timingInfoOut: CMSampleTimingInfoPtr): OSStatus; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMSampleBufferGetSampleSizeArray(sbuf: CMSampleBufferRef; sizeArrayEntries: CMItemCount; sizeArrayOut: size_tPtr; sizeArrayEntriesNeededOut: CMItemCountPtr): OSStatus; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMSampleBufferGetSampleSize(sbuf: CMSampleBufferRef; sampleIndex: CMItemIndex): size_t; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMSampleBufferGetTotalSampleSize(sbuf: CMSampleBufferRef): size_t; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMSampleBufferGetFormatDescription(sbuf: CMSampleBufferRef): CMFormatDescriptionRef; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMSampleBufferGetSampleAttachmentsArray(sbuf: CMSampleBufferRef; createIfNecessary: Boolean): CFArrayRef; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  kCMSampleAttachmentKey_NotSync: CFStringRef; cvar; external;
  kCMSampleAttachmentKey_PartialSync: CFStringRef; cvar; external;
  kCMSampleAttachmentKey_HasRedundantCoding: CFStringRef; cvar; external;
  kCMSampleAttachmentKey_IsDependedOnByOthers: CFStringRef; cvar; external;
  kCMSampleAttachmentKey_DependsOnOthers: CFStringRef; cvar; external;
  kCMSampleAttachmentKey_EarlierDisplayTimesAllowed: CFStringRef; cvar; external;
  kCMSampleAttachmentKey_DisplayImmediately: CFStringRef; cvar; external;
  kCMSampleAttachmentKey_DoNotDisplay: CFStringRef; cvar; external;
  kCMSampleBufferAttachmentKey_ResetDecoderBeforeDecoding: CFStringRef; cvar; external;
  kCMSampleBufferAttachmentKey_DrainAfterDecoding: CFStringRef; cvar; external;
  kCMSampleBufferAttachmentKey_PostNotificationWhenConsumed: CFStringRef; cvar; external;
  kCMSampleBufferAttachmentKey_ResumeOutput: CFStringRef; cvar; external;
  kCMSampleBufferAttachmentKey_TransitionID: CFStringRef { available starting in __MAC_10_7,__IPHONE_4_0 }; cvar; external;
  kCMSampleBufferAttachmentKey_TrimDurationAtStart: CFStringRef; cvar; external;
  kCMSampleBufferAttachmentKey_TrimDurationAtEnd: CFStringRef; cvar; external;
  kCMSampleBufferAttachmentKey_SpeedMultiplier: CFStringRef; cvar; external;
  kCMSampleBufferAttachmentKey_Reverse: CFStringRef; cvar; external;
  kCMSampleBufferAttachmentKey_FillDiscontinuitiesWithSilence: CFStringRef; cvar; external;
  kCMSampleBufferAttachmentKey_EmptyMedia: CFStringRef; cvar; external;
  kCMSampleBufferAttachmentKey_PermanentEmptyMedia: CFStringRef; cvar; external;
  kCMSampleBufferAttachmentKey_DisplayEmptyMediaImmediately: CFStringRef { available starting in __MAC_10_7,__IPHONE_4_0 }; cvar; external;
  kCMSampleBufferAttachmentKey_EndsPreviousSampleDuration: CFStringRef; cvar; external;
  kCMSampleBufferAttachmentKey_SampleReferenceURL: CFStringRef; cvar; external;
  kCMSampleBufferAttachmentKey_SampleReferenceByteOffset: CFStringRef; cvar; external;
  kCMSampleBufferAttachmentKey_GradualDecoderRefresh: CFStringRef; cvar; external;
  kCMSampleBufferAttachmentKey_DroppedFrameReason: CFStringRef; cvar; external;
  kCMSampleBufferDroppedFrameReason_FrameWasLate: CFStringRef { available starting in __MAC_NA,__IPHONE_6_0 }; cvar; external;
  kCMSampleBufferDroppedFrameReason_OutOfBuffers: CFStringRef { available starting in __MAC_NA,__IPHONE_6_0 }; cvar; external;
  kCMSampleBufferDroppedFrameReason_Discontinuity: CFStringRef { available starting in __MAC_NA,__IPHONE_6_0 }; cvar; external;
  kCMSampleBufferAttachmentKey_DroppedFrameReasonInfo: CFStringRef; cvar; external;
  kCMSampleBufferDroppedFrameReasonInfo_CameraModeSwitch: CFStringRef { available starting in __MAC_NA,__IPHONE_7_0 }; cvar; external;
  kCMSampleBufferAttachmentKey_ForceKeyFrame: CFStringRef { available starting in __MAC_10_10,__IPHONE_8_0 }; cvar; external;
{$endif}

{$endif}
