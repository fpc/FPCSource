{ Parsed from CoreMedia.framework CMFormatDescriptionBridge.h }

{$ifndef CMFORMATDESCRIPTIONBRIDGE_H}

{$ifdef TYPES}
const
  kCMFormatDescriptionBridgeError_InvalidParameter = -12712;
  kCMFormatDescriptionBridgeError_AllocationFailed = -12713;
  kCMFormatDescriptionBridgeError_InvalidSerializedSampleDescription = -12714;
  kCMFormatDescriptionBridgeError_InvalidFormatDescription = -12715;
  kCMFormatDescriptionBridgeError_IncompatibleFormatDescription = -12716;
  kCMFormatDescriptionBridgeError_UnsupportedSampleDescriptionFlavor = -12717;
  kCMFormatDescriptionBridgeError_InvalidSlice = -12719;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  kCMImageDescriptionFlavor_QuickTimeMovie: CFStringRef { available starting in __MAC_10_10,__IPHONE_8_0 }; cvar; external;
  kCMImageDescriptionFlavor_ISOFamily: CFStringRef { available starting in __MAC_10_10,__IPHONE_8_0 }; cvar; external;
  kCMImageDescriptionFlavor_3GPFamily: CFStringRef { available starting in __MAC_10_10,__IPHONE_8_0 }; cvar; external;
{$endif}

{$ifdef FUNCTIONS}
function CMVideoFormatDescriptionCreateFromBigEndianImageDescriptionData(allocator: CFAllocatorRef; imageDescriptionData: pbyte; imageDescriptionSize: size_t; imageDescriptionStringEncoding: CFStringEncoding; imageDescriptionFlavor: CFStringRef; videoFormatDescriptionOut: CMVideoFormatDescriptionRefPtr): OSStatus; cdecl; external; { available starting in __MAC_10_10,__IPHONE_8_0 }
function CMVideoFormatDescriptionCreateFromBigEndianImageDescriptionBlockBuffer(allocator: CFAllocatorRef; imageDescriptionBlockBuffer: CMBlockBufferRef; imageDescriptionStringEncoding: CFStringEncoding; imageDescriptionFlavor: CFStringRef; videoFormatDescriptionOut: CMVideoFormatDescriptionRefPtr): OSStatus; cdecl; external; { available starting in __MAC_10_10,__IPHONE_8_0 }
function CMVideoFormatDescriptionCopyAsBigEndianImageDescriptionBlockBuffer(allocator: CFAllocatorRef; videoFormatDescription: CMVideoFormatDescriptionRef; imageDescriptionStringEncoding: CFStringEncoding; imageDescriptionFlavor: CFStringRef; imageDescriptionBlockBufferOut: CMBlockBufferRef): OSStatus; cdecl; external; { available starting in __MAC_10_10,__IPHONE_8_0 }
{$endif}

{$if defined(TARGET_RT_LITTLE_ENDIAN)}
{$ifdef FUNCTIONS}
function CMSwapBigEndianImageDescriptionToHost(imageDescriptionData: pbyte; imageDescriptionSize: size_t): OSStatus; cdecl; external; { available starting in __MAC_10_10,__IPHONE_8_0 }
function CMSwapHostEndianImageDescriptionToBig(imageDescriptionData: pbyte; imageDescriptionSize: size_t): OSStatus; cdecl; external; { available starting in __MAC_10_10,__IPHONE_8_0 }
{$endif}

{$else}
{$endif}
{$ifdef EXTERNAL_SYMBOLS}
var
  kCMSoundDescriptionFlavor_QuickTimeMovie: CFStringRef { available starting in __MAC_10_10,__IPHONE_8_0 }; cvar; external;
  kCMSoundDescriptionFlavor_QuickTimeMovieV2: CFStringRef { available starting in __MAC_10_10,__IPHONE_8_0 }; cvar; external;
  kCMSoundDescriptionFlavor_ISOFamily: CFStringRef { available starting in __MAC_10_10,__IPHONE_8_0 }; cvar; external;
  kCMSoundDescriptionFlavor_3GPFamily: CFStringRef { available starting in __MAC_10_10,__IPHONE_8_0 }; cvar; external;
{$endif}

{$ifdef FUNCTIONS}
function CMAudioFormatDescriptionCreateFromBigEndianSoundDescriptionData(allocator: CFAllocatorRef; soundDescriptionData: pbyte; soundDescriptionSize: size_t; soundDescriptionFlavor: CFStringRef; audioFormatDescriptionOut: CMAudioFormatDescriptionRefPtr): OSStatus; cdecl; external; { available starting in __MAC_10_10,__IPHONE_8_0 }
function CMAudioFormatDescriptionCreateFromBigEndianSoundDescriptionBlockBuffer(allocator: CFAllocatorRef; soundDescriptionBlockBuffer: CMBlockBufferRef; soundDescriptionFlavor: CFStringRef; audioFormatDescriptionOut: CMAudioFormatDescriptionRefPtr): OSStatus; cdecl; external; { available starting in __MAC_10_10,__IPHONE_8_0 }
function CMAudioFormatDescriptionCopyAsBigEndianSoundDescriptionBlockBuffer(allocator: CFAllocatorRef; audioFormatDescription: CMAudioFormatDescriptionRef; soundDescriptionFlavor: CFStringRef; soundDescriptionBlockBufferOut: CMBlockBufferRef): OSStatus; cdecl; external; { available starting in __MAC_10_10,__IPHONE_8_0 }
function CMDoesBigEndianSoundDescriptionRequireLegacyCBRSampleTableLayout(soundDescriptionBlockBuffer: CMBlockBufferRef; soundDescriptionFlavor: CFStringRef): Boolean; cdecl; external; { available starting in __MAC_10_10,__IPHONE_8_0 }
{$endif}

{$if defined(TARGET_RT_LITTLE_ENDIAN)}
{$ifdef FUNCTIONS}
function CMSwapBigEndianSoundDescriptionToHost(soundDescriptionData: pbyte; soundDescriptionSize: size_t): OSStatus; cdecl; external; { available starting in __MAC_10_10,__IPHONE_8_0 }
function CMSwapHostEndianSoundDescriptionToBig(soundDescriptionData: pbyte; soundDescriptionSize: size_t): OSStatus; cdecl; external; { available starting in __MAC_10_10,__IPHONE_8_0 }
{$endif}

{$else}
{$endif}
{$ifdef FUNCTIONS}
function CMTextFormatDescriptionCreateFromBigEndianTextDescriptionData(allocator: CFAllocatorRef; textDescriptionData: pbyte; textDescriptionSize: size_t; textDescriptionFlavor: CFStringRef; mediaType: CMMediaType; textFormatDescriptionOut: CMTextFormatDescriptionRefPtr): OSStatus; cdecl; external; { available starting in __MAC_10_10,__IPHONE_8_0 }
function CMTextFormatDescriptionCreateFromBigEndianTextDescriptionBlockBuffer(allocator: CFAllocatorRef; textDescriptionBlockBuffer: CMBlockBufferRef; textDescriptionFlavor: CFStringRef; mediaType: CMMediaType; textFormatDescriptionOut: CMTextFormatDescriptionRefPtr): OSStatus; cdecl; external; { available starting in __MAC_10_10,__IPHONE_8_0 }
function CMTextFormatDescriptionCopyAsBigEndianTextDescriptionBlockBuffer(allocator: CFAllocatorRef; textFormatDescription: CMTextFormatDescriptionRef; textDescriptionFlavor: CFStringRef; textDescriptionBlockBufferOut: CMBlockBufferRef): OSStatus; cdecl; external; { available starting in __MAC_10_10,__IPHONE_8_0 }
{$endif}

{$if defined(TARGET_RT_LITTLE_ENDIAN)}
{$ifdef FUNCTIONS}
function CMSwapBigEndianTextDescriptionToHost(textDescriptionData: pbyte; textDescriptionSize: size_t): OSStatus; cdecl; external; { available starting in __MAC_10_10,__IPHONE_8_0 }
function CMSwapHostEndianTextDescriptionToBig(textDescriptionData: pbyte; textDescriptionSize: size_t): OSStatus; cdecl; external; { available starting in __MAC_10_10,__IPHONE_8_0 }
{$endif}

{$else}
{$endif}
{$ifdef FUNCTIONS}
function CMClosedCaptionFormatDescriptionCreateFromBigEndianClosedCaptionDescriptionData(allocator: CFAllocatorRef; closedCaptionDescriptionData: pbyte; closedCaptionDescriptionSize: size_t; closedCaptionDescriptionFlavor: CFStringRef; closedCaptionFormatDescriptionOut: CMClosedCaptionFormatDescriptionRefPtr): OSStatus; cdecl; external; { available starting in __MAC_10_10,__IPHONE_8_0 }
function CMClosedCaptionFormatDescriptionCreateFromBigEndianClosedCaptionDescriptionBlockBuffer(allocator: CFAllocatorRef; closedCaptionDescriptionBlockBuffer: CMBlockBufferRef; closedCaptionDescriptionFlavor: CFStringRef; closedCaptionFormatDescriptionOut: CMClosedCaptionFormatDescriptionRefPtr): OSStatus; cdecl; external; { available starting in __MAC_10_10,__IPHONE_8_0 }
function CMClosedCaptionFormatDescriptionCopyAsBigEndianClosedCaptionDescriptionBlockBuffer(allocator: CFAllocatorRef; closedCaptionFormatDescription: CMClosedCaptionFormatDescriptionRef; closedCaptionDescriptionFlavor: CFStringRef; closedCaptionDescriptionBlockBufferOut: CMBlockBufferRef): OSStatus; cdecl; external; { available starting in __MAC_10_10,__IPHONE_8_0 }
{$endif}

{$if defined(TARGET_RT_LITTLE_ENDIAN)}
{$ifdef FUNCTIONS}
function CMSwapBigEndianClosedCaptionDescriptionToHost(closedCaptionDescriptionData: pbyte; closedCaptionDescriptionSize: size_t): OSStatus; cdecl; external; { available starting in __MAC_10_10,__IPHONE_8_0 }
function CMSwapHostEndianClosedCaptionDescriptionToBig(closedCaptionDescriptionData: pbyte; closedCaptionDescriptionSize: size_t): OSStatus; cdecl; external; { available starting in __MAC_10_10,__IPHONE_8_0 }
{$endif}

{$else}
{$endif}
{$ifdef FUNCTIONS}
function CMTimeCodeFormatDescriptionCreateFromBigEndianTimeCodeDescriptionData(allocator: CFAllocatorRef; timeCodeDescriptionData: pbyte; timeCodeDescriptionSize: size_t; timeCodeDescriptionFlavor: CFStringRef; timeCodeFormatDescriptionOut: CMTimeCodeFormatDescriptionRefPtr): OSStatus; cdecl; external; { available starting in __MAC_10_10,__IPHONE_8_0 }
function CMTimeCodeFormatDescriptionCreateFromBigEndianTimeCodeDescriptionBlockBuffer(allocator: CFAllocatorRef; timeCodeDescriptionBlockBuffer: CMBlockBufferRef; timeCodeDescriptionFlavor: CFStringRef; timeCodeFormatDescriptionOut: CMTimeCodeFormatDescriptionRefPtr): OSStatus; cdecl; external; { available starting in __MAC_10_10,__IPHONE_8_0 }
function CMTimeCodeFormatDescriptionCopyAsBigEndianTimeCodeDescriptionBlockBuffer(allocator: CFAllocatorRef; timeCodeFormatDescription: CMTimeCodeFormatDescriptionRef; timeCodeDescriptionFlavor: CFStringRef; timeCodeDescriptionBlockBufferOut: CMBlockBufferRef): OSStatus; cdecl; external; { available starting in __MAC_10_10,__IPHONE_8_0 }
{$endif}

{$if defined(TARGET_RT_LITTLE_ENDIAN)}
{$ifdef FUNCTIONS}
function CMSwapBigEndianTimeCodeDescriptionToHost(timeCodeDescriptionData: pbyte; timeCodeDescriptionSize: size_t): OSStatus; cdecl; external; { available starting in __MAC_10_10,__IPHONE_8_0 }
function CMSwapHostEndianTimeCodeDescriptionToBig(timeCodeDescriptionData: pbyte; timeCodeDescriptionSize: size_t): OSStatus; cdecl; external; { available starting in __MAC_10_10,__IPHONE_8_0 }
{$endif}

{$else}
{$endif}
{$ifdef FUNCTIONS}
function CMMetadataFormatDescriptionCreateFromBigEndianMetadataDescriptionData(allocator: CFAllocatorRef; metadataDescriptionData: pbyte; metadataDescriptionSize: size_t; metadataDescriptionFlavor: CFStringRef; metadataFormatDescriptionOut: CMMetadataFormatDescriptionRefPtr): OSStatus; cdecl; external; { available starting in __MAC_10_10,__IPHONE_8_0 }
function CMMetadataFormatDescriptionCreateFromBigEndianMetadataDescriptionBlockBuffer(allocator: CFAllocatorRef; metadataDescriptionBlockBuffer: CMBlockBufferRef; metadataDescriptionFlavor: CFStringRef; metadataFormatDescriptionOut: CMMetadataFormatDescriptionRefPtr): OSStatus; cdecl; external; { available starting in __MAC_10_10,__IPHONE_8_0 }
function CMMetadataFormatDescriptionCopyAsBigEndianMetadataDescriptionBlockBuffer(allocator: CFAllocatorRef; metadataFormatDescription: CMMetadataFormatDescriptionRef; metadataDescriptionFlavor: CFStringRef; metadataDescriptionBlockBufferOut: CMBlockBufferRef): OSStatus; cdecl; external; { available starting in __MAC_10_10,__IPHONE_8_0 }
{$endif}

{$if defined(TARGET_RT_LITTLE_ENDIAN)}
{$ifdef FUNCTIONS}
function CMSwapBigEndianMetadataDescriptionToHost(metadataDescriptionData: pbyte; metadataDescriptionSize: size_t): OSStatus; cdecl; external; { available starting in __MAC_10_10,__IPHONE_8_0 }
function CMSwapHostEndianMetadataDescriptionToBig(metadataDescriptionData: pbyte; metadataDescriptionSize: size_t): OSStatus; cdecl; external; { available starting in __MAC_10_10,__IPHONE_8_0 }
{$endif}

{$else}
{$endif}

{$endif}
