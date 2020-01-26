{ Parsed from CoreMedia.framework CMFormatDescription.h }

{$ifndef CMFORMATDESCRIPTION_H}

{$ifdef TYPES}
const
  kCMFormatDescriptionError_InvalidParameter = -12710;
  kCMFormatDescriptionError_AllocationFailed = -12711;
  kCMFormatDescriptionError_ValueNotAvailable = -12718;

type
  CMFormatDescriptionRef = OpaqueType;

const
  kCMMediaType_Video = 'vide';
  kCMMediaType_Audio = 'soun';
  kCMMediaType_Muxed = 'muxx';
  kCMMediaType_Text = 'text';
  kCMMediaType_ClosedCaption = 'clcp';
  kCMMediaType_Subtitle = 'sbtl';
  kCMMediaType_TimeCode = 'tmcd';
  kCMMediaType_Metadata = 'meta';

type
  CMMediaType = FourCharCode;
  CMMediaTypePtr = ^CMMediaType;
{$endif}

{$ifdef FUNCTIONS}
function CMFormatDescriptionCreate(allocator: CFAllocatorRef; mediaType: CMMediaType; mediaSubtype: FourCharCode; extensions: CFDictionaryRef; descOut: CMFormatDescriptionRef): OSStatus; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMFormatDescriptionGetTypeID: CFTypeID; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMFormatDescriptionEqual(desc1: CMFormatDescriptionRef; desc2: CMFormatDescriptionRef): Boolean; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMFormatDescriptionEqualIgnoringExtensionKeys(desc1: CMFormatDescriptionRef; desc2: CMFormatDescriptionRef; formatDescriptionExtensionKeysToIgnore: CFTypeRef; sampleDescriptionExtensionAtomKeysToIgnore: CFTypeRef): Boolean; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_3 }
function CMFormatDescriptionGetMediaType(desc: CMFormatDescriptionRef): CMMediaType; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMFormatDescriptionGetMediaSubType(desc: CMFormatDescriptionRef): FourCharCode; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMFormatDescriptionGetExtensions(desc: CMFormatDescriptionRef): CFDictionaryRef; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  kCMFormatDescriptionExtension_OriginalCompressionSettings: CFStringRef; cvar; external;
  kCMFormatDescriptionExtension_SampleDescriptionExtensionAtoms: CFStringRef; cvar; external;
  kCMFormatDescriptionExtension_VerbatimSampleDescription: CFStringRef; cvar; external;
  kCMFormatDescriptionExtension_VerbatimISOSampleEntry: CFStringRef; cvar; external;
{$endif}

{$ifdef FUNCTIONS}
function CMFormatDescriptionGetExtension(desc: CMFormatDescriptionRef; extensionKey: CFStringRef): CFPropertyListRef; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
{$endif}

{$ifdef TYPES}
const
  kCMAudioCodecType_AAC_LCProtected = 'paac';
  kCMAudioCodecType_AAC_AudibleProtected = 'aaac';

type
  CMAudioCodecType = FourCharCode;
  CMAudioCodecTypePtr = ^CMAudioCodecType;
  CMAudioFormatDescriptionRef = CMFormatDescriptionRef;
  CMAudioFormatDescriptionRefPtr = ^CMAudioFormatDescriptionRef;
{$endif}

{$ifdef FUNCTIONS}
function CMAudioFormatDescriptionCreate(allocator: CFAllocatorRef; asbd: AudioStreamBasicDescriptionPtr; layoutSize: size_t; layout: AudioChannelLayoutPtr; magicCookieSize: size_t; magicCookie: pointer; extensions: CFDictionaryRef; outDesc: CMAudioFormatDescriptionRefPtr): OSStatus; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMAudioFormatDescriptionGetStreamBasicDescription(desc: CMAudioFormatDescriptionRef): AudioStreamBasicDescriptionPtr; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMAudioFormatDescriptionGetMagicCookie(desc: CMAudioFormatDescriptionRef; cookieSizeOut: size_tPtr): pointer; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMAudioFormatDescriptionGetChannelLayout(desc: CMAudioFormatDescriptionRef; layoutSize: size_tPtr): AudioChannelLayoutPtr; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMAudioFormatDescriptionGetFormatList(desc: CMAudioFormatDescriptionRef; formatListSize: size_tPtr): AudioFormatListItemPtr; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMAudioFormatDescriptionGetRichestDecodableFormat(desc: CMAudioFormatDescriptionRef): AudioFormatListItemPtr; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMAudioFormatDescriptionGetMostCompatibleFormat(desc: CMAudioFormatDescriptionRef): AudioFormatListItemPtr; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMAudioFormatDescriptionCreateSummary(allocator: CFAllocatorRef; formatDescriptionArray: CFArrayRef; flags: cuint32; summaryFormatDescriptionOut: CMAudioFormatDescriptionRefPtr): OSStatus; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
{$endif}

{$ifdef TYPES}
const
  kCMAudioFormatDescriptionMask_StreamBasicDescription = 1 shl 0;
  kCMAudioFormatDescriptionMask_MagicCookie = 1 shl 1;
  kCMAudioFormatDescriptionMask_ChannelLayout = 1 shl 2;
  kCMAudioFormatDescriptionMask_Extensions = 1 shl 3;
  kCMAudioFormatDescriptionMask_All = kCMAudioFormatDescriptionMask_StreamBasicDescription + kCMAudioFormatDescriptionMask_MagicCookie + kCMAudioFormatDescriptionMask_ChannelLayout + kCMAudioFormatDescriptionMask_Extensions;

type
  CMAudioFormatDescriptionMask = cuint32;
  CMAudioFormatDescriptionMaskPtr = ^CMAudioFormatDescriptionMask;
{$endif}

{$ifdef FUNCTIONS}
function CMAudioFormatDescriptionEqual(desc1: CMAudioFormatDescriptionRef; desc2: CMAudioFormatDescriptionRef; equalityMask: CMAudioFormatDescriptionMask; equalityMaskOut: CMAudioFormatDescriptionMaskPtr): Boolean; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
{$endif}

{$ifdef TYPES}
type
  CMVideoFormatDescriptionRef = CMFormatDescriptionRef;
  CMVideoFormatDescriptionRefPtr = ^CMVideoFormatDescriptionRef;

const
  kCMPixelFormat_32ARGB = 32;
  kCMPixelFormat_32BGRA = 'BGRA';
  kCMPixelFormat_24RGB = 24;
  kCMPixelFormat_16BE555 = 16;
  kCMPixelFormat_16BE565 = 'B565';
  kCMPixelFormat_16LE555 = 'L555';
  kCMPixelFormat_16LE565 = 'L565';
  kCMPixelFormat_16LE5551 = '5551';
  kCMPixelFormat_422YpCbCr8 = '2vuy';
  kCMPixelFormat_422YpCbCr8_yuvs = 'yuvs';
  kCMPixelFormat_444YpCbCr8 = 'v308';
  kCMPixelFormat_4444YpCbCrA8 = 'v408';
  kCMPixelFormat_422YpCbCr16 = 'v216';
  kCMPixelFormat_422YpCbCr10 = 'v210';
  kCMPixelFormat_444YpCbCr10 = 'v410';
  kCMPixelFormat_8IndexedGray_WhiteIsZero = $00000028;

type
  CMPixelFormatType = FourCharCode;
  CMPixelFormatTypePtr = ^CMPixelFormatType;

const
  kCMVideoCodecType_422YpCbCr8 = kCMPixelFormat_422YpCbCr8;
  kCMVideoCodecType_Animation = 'rle ';
  kCMVideoCodecType_Cinepak = 'cvid';
  kCMVideoCodecType_JPEG = 'jpeg';
  kCMVideoCodecType_JPEG_OpenDML = 'dmb1';
  kCMVideoCodecType_SorensonVideo = 'SVQ1';
  kCMVideoCodecType_SorensonVideo3 = 'SVQ3';
  kCMVideoCodecType_H263 = 'h263';
  kCMVideoCodecType_H264 = 'avc1';
  kCMVideoCodecType_MPEG4Video = 'mp4v';
  kCMVideoCodecType_MPEG2Video = 'mp2v';
  kCMVideoCodecType_MPEG1Video = 'mp1v';
  kCMVideoCodecType_DVCNTSC = 'dvc ';
  kCMVideoCodecType_DVCPAL = 'dvcp';
  kCMVideoCodecType_DVCProPAL = 'dvpp';
  kCMVideoCodecType_DVCPro50NTSC = 'dv5n';
  kCMVideoCodecType_DVCPro50PAL = 'dv5p';
  kCMVideoCodecType_DVCPROHD720p60 = 'dvhp';
  kCMVideoCodecType_DVCPROHD720p50 = 'dvhq';
  kCMVideoCodecType_DVCPROHD1080i60 = 'dvh6';
  kCMVideoCodecType_DVCPROHD1080i50 = 'dvh5';
  kCMVideoCodecType_DVCPROHD1080p30 = 'dvh3';
  kCMVideoCodecType_DVCPROHD1080p25 = 'dvh2';
  kCMVideoCodecType_AppleProRes4444 = 'ap4h';
  kCMVideoCodecType_AppleProRes422HQ = 'apch';
  kCMVideoCodecType_AppleProRes422 = 'apcn';
  kCMVideoCodecType_AppleProRes422LT = 'apcs';
  kCMVideoCodecType_AppleProRes422Proxy = 'apco';

type
  CMVideoCodecType = FourCharCode;
  CMVideoCodecTypePtr = ^CMVideoCodecType;

type
  CMVideoDimensions = record
    width: cint32;
    height: cint32;
  end;
type
  CMVideoDimensionsPtr = ^CMVideoDimensions;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  kCMFormatDescriptionExtension_FormatName: CFStringRef; cvar; external;
  kCMFormatDescriptionExtension_Depth: CFStringRef; cvar; external;
  kCMFormatDescriptionKey_CleanApertureWidthRational: CFStringRef; cvar; external;
  kCMFormatDescriptionKey_CleanApertureHeightRational: CFStringRef; cvar; external;
  kCMFormatDescriptionKey_CleanApertureHorizontalOffsetRational: CFStringRef; cvar; external;
  kCMFormatDescriptionKey_CleanApertureVerticalOffsetRational: CFStringRef; cvar; external;
  kCMFormatDescriptionColorPrimaries_P22: CFStringRef; cvar; external;
  kCMFormatDescriptionExtension_FullRangeVideo: CFStringRef; cvar; external;
  kCMFormatDescriptionExtension_ICCProfile: CFStringRef; cvar; external;
  kCMFormatDescriptionExtension_BytesPerRow: CFStringRef; cvar; external;
  kCMFormatDescriptionConformsToMPEG2VideoProfile: CFStringRef; cvar; external;
{$endif}

{$ifdef TYPES}
const
  kCMMPEG2VideoProfile_HDV_720p30 = 'hdv1';
  kCMMPEG2VideoProfile_HDV_1080i60 = 'hdv2';
  kCMMPEG2VideoProfile_HDV_1080i50 = 'hdv3';
  kCMMPEG2VideoProfile_HDV_720p24 = 'hdv4';
  kCMMPEG2VideoProfile_HDV_720p25 = 'hdv5';
  kCMMPEG2VideoProfile_HDV_1080p24 = 'hdv6';
  kCMMPEG2VideoProfile_HDV_1080p25 = 'hdv7';
  kCMMPEG2VideoProfile_HDV_1080p30 = 'hdv8';
  kCMMPEG2VideoProfile_HDV_720p60 = 'hdv9';
  kCMMPEG2VideoProfile_HDV_720p50 = 'hdva';
  kCMMPEG2VideoProfile_XDCAM_HD_1080i60_VBR35 = 'xdv2';
  kCMMPEG2VideoProfile_XDCAM_HD_1080i50_VBR35 = 'xdv3';
  kCMMPEG2VideoProfile_XDCAM_HD_1080p24_VBR35 = 'xdv6';
  kCMMPEG2VideoProfile_XDCAM_HD_1080p25_VBR35 = 'xdv7';
  kCMMPEG2VideoProfile_XDCAM_HD_1080p30_VBR35 = 'xdv8';
  kCMMPEG2VideoProfile_XDCAM_EX_720p24_VBR35 = 'xdv4';
  kCMMPEG2VideoProfile_XDCAM_EX_720p25_VBR35 = 'xdv5';
  kCMMPEG2VideoProfile_XDCAM_EX_720p30_VBR35 = 'xdv1';
  kCMMPEG2VideoProfile_XDCAM_EX_720p50_VBR35 = 'xdva';
  kCMMPEG2VideoProfile_XDCAM_EX_720p60_VBR35 = 'xdv9';
  kCMMPEG2VideoProfile_XDCAM_EX_1080i60_VBR35 = 'xdvb';
  kCMMPEG2VideoProfile_XDCAM_EX_1080i50_VBR35 = 'xdvc';
  kCMMPEG2VideoProfile_XDCAM_EX_1080p24_VBR35 = 'xdvd';
  kCMMPEG2VideoProfile_XDCAM_EX_1080p25_VBR35 = 'xdve';
  kCMMPEG2VideoProfile_XDCAM_EX_1080p30_VBR35 = 'xdvf';
  kCMMPEG2VideoProfile_XDCAM_HD422_720p50_CBR50 = 'xd5a';
  kCMMPEG2VideoProfile_XDCAM_HD422_720p60_CBR50 = 'xd59';
  kCMMPEG2VideoProfile_XDCAM_HD422_1080i60_CBR50 = 'xd5b';
  kCMMPEG2VideoProfile_XDCAM_HD422_1080i50_CBR50 = 'xd5c';
  kCMMPEG2VideoProfile_XDCAM_HD422_1080p24_CBR50 = 'xd5d';
  kCMMPEG2VideoProfile_XDCAM_HD422_1080p25_CBR50 = 'xd5e';
  kCMMPEG2VideoProfile_XDCAM_HD422_1080p30_CBR50 = 'xd5f';
  kCMMPEG2VideoProfile_XDCAM_HD_540p = 'xdhd';
  kCMMPEG2VideoProfile_XDCAM_HD422_540p = 'xdh2';
  kCMMPEG2VideoProfile_XDCAM_HD422_720p24_CBR50 = 'xd54';
  kCMMPEG2VideoProfile_XDCAM_HD422_720p25_CBR50 = 'xd55';
  kCMMPEG2VideoProfile_XDCAM_HD422_720p30_CBR50 = 'xd51';
  kCMMPEG2VideoProfile_XF = 'xfz1';
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  kCMFormatDescriptionExtension_TemporalQuality: CFStringRef; cvar; external;
  kCMFormatDescriptionExtension_SpatialQuality: CFStringRef; cvar; external;
  kCMFormatDescriptionExtension_Version: CFStringRef; cvar; external;
  kCMFormatDescriptionExtension_RevisionLevel: CFStringRef; cvar; external;
  kCMFormatDescriptionExtension_Vendor: CFStringRef; cvar; external;
  kCMFormatDescriptionVendor_Apple: CFStringRef { available starting in __MAC_10_7,__IPHONE_4_0 }; cvar; external;
{$endif}

{$ifdef FUNCTIONS}
function CMVideoFormatDescriptionCreate(allocator: CFAllocatorRef; codecType: CMVideoCodecType; width: cint32; height: cint32; extensions: CFDictionaryRef; outDesc: CMVideoFormatDescriptionRefPtr): OSStatus; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMVideoFormatDescriptionCreateForImageBuffer(allocator: CFAllocatorRef; imageBuffer: CVImageBufferRef; outDesc: CMVideoFormatDescriptionRefPtr): OSStatus; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMVideoFormatDescriptionCreateFromH264ParameterSets(allocator: CFAllocatorRef; parameterSetCount: size_t; parameterSetPointers: pbyte; parameterSetSizes: size_tPtr; NALUnitHeaderLength: cint; formatDescriptionOut: CMFormatDescriptionRef): OSStatus; cdecl; external; { available starting in __MAC_10_9,__IPHONE_7_0 }
function CMVideoFormatDescriptionGetH264ParameterSetAtIndex(videoDesc: CMFormatDescriptionRef; parameterSetIndex: size_t; parameterSetPointerOut: pbyte; parameterSetSizeOut: size_tPtr; parameterSetCountOut: size_tPtr; NALUnitHeaderLengthOut: pcint): OSStatus; cdecl; external; { available starting in __MAC_10_9,__IPHONE_7_0 }
function CMVideoFormatDescriptionGetDimensions(videoDesc: CMVideoFormatDescriptionRef): CMVideoDimensions; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMVideoFormatDescriptionGetPresentationDimensions(videoDesc: CMVideoFormatDescriptionRef; usePixelAspectRatio: Boolean; useCleanAperture: Boolean): CGSize; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMVideoFormatDescriptionGetCleanAperture(videoDesc: CMVideoFormatDescriptionRef; originIsAtTopLeft: Boolean): CGRect; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMVideoFormatDescriptionGetExtensionKeysCommonWithImageBuffers: CFArrayRef; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMVideoFormatDescriptionMatchesImageBuffer(desc: CMVideoFormatDescriptionRef; imageBuffer: CVImageBufferRef): Boolean; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
{$endif}

{$ifdef TYPES}
type
  CMMuxedFormatDescriptionRef = CMFormatDescriptionRef;
  CMMuxedFormatDescriptionRefPtr = ^CMMuxedFormatDescriptionRef;

const
  kCMMuxedStreamType_MPEG1System = 'mp1s';
  kCMMuxedStreamType_MPEG2Transport = 'mp2t';
  kCMMuxedStreamType_MPEG2Program = 'mp2p';
  kCMMuxedStreamType_DV = 'dv  ';

type
  CMMuxedStreamType = FourCharCode;
  CMMuxedStreamTypePtr = ^CMMuxedStreamType;
{$endif}

{$ifdef FUNCTIONS}
function CMMuxedFormatDescriptionCreate(allocator: CFAllocatorRef; muxType: CMMuxedStreamType; extensions: CFDictionaryRef; outDesc: CMMuxedFormatDescriptionRefPtr): OSStatus; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
{$endif}

{$ifdef TYPES}
type
  CMClosedCaptionFormatDescriptionRef = CMFormatDescriptionRef;
  CMClosedCaptionFormatDescriptionRefPtr = ^CMClosedCaptionFormatDescriptionRef;

const
  kCMClosedCaptionFormatType_CEA608 = 'c608';
  kCMClosedCaptionFormatType_CEA708 = 'c708';
  kCMClosedCaptionFormatType_ATSC = 'atcc';

type
  CMClosedCaptionFormatType = FourCharCode;
  CMClosedCaptionFormatTypePtr = ^CMClosedCaptionFormatType;
  CMTextFormatDescriptionRef = CMFormatDescriptionRef;
  CMTextFormatDescriptionRefPtr = ^CMTextFormatDescriptionRef;

const
  kCMTextFormatType_QTText = 'text';
  kCMTextFormatType_3GText = 'tx3g';

type
  CMTextFormatType = FourCharCode;
  CMTextFormatTypePtr = ^CMTextFormatType;

const
  kCMTextDisplayFlag_scrollIn = $00000020;
  kCMTextDisplayFlag_scrollOut = $00000040;
  kCMTextDisplayFlag_scrollDirectionMask = $00000180;
  kCMTextDisplayFlag_scrollDirection_bottomToTop = $00000000;
  kCMTextDisplayFlag_scrollDirection_rightToLeft = $00000080;
  kCMTextDisplayFlag_scrollDirection_topToBottom = $00000100;
  kCMTextDisplayFlag_scrollDirection_leftToRight = $00000180;
  kCMTextDisplayFlag_continuousKaraoke = $00000800;
  kCMTextDisplayFlag_writeTextVertically = $00020000;
  kCMTextDisplayFlag_fillTextRegion = $00040000;
  kCMTextDisplayFlag_obeySubtitleFormatting = $20000000;
  kCMTextDisplayFlag_forcedSubtitlesPresent = $40000000;
  kCMTextDisplayFlag_allSubtitlesForced = $80000000;

type
  CMTextDisplayFlags = cuint32;
  CMTextDisplayFlagsPtr = ^CMTextDisplayFlags;

const
  kCMTextJustification_left_top = 0;
  kCMTextJustification_centered = 1;
  kCMTextJustification_bottom_right = -1;

type
  CMTextJustificationValue = cint8;
  CMTextJustificationValuePtr = ^CMTextJustificationValue;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  kCMTextFormatDescriptionExtension_DisplayFlags: CFStringRef; cvar; external;
  kCMTextFormatDescriptionExtension_BackgroundColor: CFStringRef; cvar; external;
  kCMTextFormatDescriptionColor_Red: CFStringRef; cvar; external;
  kCMTextFormatDescriptionColor_Green: CFStringRef; cvar; external;
  kCMTextFormatDescriptionColor_Blue: CFStringRef; cvar; external;
  kCMTextFormatDescriptionColor_Alpha: CFStringRef; cvar; external;
  kCMTextFormatDescriptionExtension_DefaultTextBox: CFStringRef; cvar; external;
  kCMTextFormatDescriptionRect_Top: CFStringRef; cvar; external;
  kCMTextFormatDescriptionRect_Left: CFStringRef; cvar; external;
  kCMTextFormatDescriptionRect_Bottom: CFStringRef; cvar; external;
  kCMTextFormatDescriptionRect_Right: CFStringRef; cvar; external;
  kCMTextFormatDescriptionExtension_DefaultStyle: CFStringRef; cvar; external;
  kCMTextFormatDescriptionStyle_StartChar: CFStringRef; cvar; external;
  kCMTextFormatDescriptionStyle_Font: CFStringRef; cvar; external;
  kCMTextFormatDescriptionStyle_FontFace: CFStringRef; cvar; external;
  kCMTextFormatDescriptionStyle_ForegroundColor: CFStringRef; cvar; external;
  kCMTextFormatDescriptionStyle_FontSize: CFStringRef; cvar; external;
  kCMTextFormatDescriptionExtension_HorizontalJustification: CFStringRef; cvar; external;
  kCMTextFormatDescriptionExtension_VerticalJustification: CFStringRef; cvar; external;
  kCMTextFormatDescriptionStyle_EndChar: CFStringRef; cvar; external;
  kCMTextFormatDescriptionExtension_FontTable: CFStringRef; cvar; external;
  kCMTextFormatDescriptionExtension_TextJustification: CFStringRef; cvar; external;
  kCMTextFormatDescriptionStyle_Height: CFStringRef; cvar; external;
  kCMTextFormatDescriptionStyle_Ascent: CFStringRef; cvar; external;
  kCMTextFormatDescriptionExtension_DefaultFontName: CFStringRef; cvar; external;
{$endif}

{$ifdef FUNCTIONS}
function CMTextFormatDescriptionGetDisplayFlags(desc: CMFormatDescriptionRef; outDisplayFlags: CMTextDisplayFlagsPtr): OSStatus; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMTextFormatDescriptionGetJustification(desc: CMFormatDescriptionRef; outHorizontalJust: CMTextJustificationValuePtr; outVerticalJust: CMTextJustificationValuePtr): OSStatus; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMTextFormatDescriptionGetDefaultTextBox(desc: CMFormatDescriptionRef; originIsAtTopLeft: Boolean; heightOfTextTrack: CGFloat; outDefaultTextBox: CGRectPtr): OSStatus; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMTextFormatDescriptionGetDefaultStyle(desc: CMFormatDescriptionRef; outLocalFontID: pcuint16; outBold: pObjCBOOL; outItalic: pObjCBOOL; outUnderline: pObjCBOOL; outFontSize: CGFloatPtr; outColorComponents: CGFloatPtr { 4 element array of CGFloat }): OSStatus; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMTextFormatDescriptionGetFontName(desc: CMFormatDescriptionRef; localFontID: cuint16; outFontName: CFStringRefPtr): OSStatus; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
{$endif}

{$ifdef TYPES}
const
  kCMSubtitleFormatType_3GText = 'tx3g';
  kCMSubtitleFormatType_WebVTT = 'wvtt';

type
  CMSubtitleFormatType = FourCharCode;
  CMSubtitleFormatTypePtr = ^CMSubtitleFormatType;
  CMTimeCodeFormatDescriptionRef = CMFormatDescriptionRef;
  CMTimeCodeFormatDescriptionRefPtr = ^CMTimeCodeFormatDescriptionRef;

const
  kCMTimeCodeFormatType_TimeCode32 = 'tmcd';
  kCMTimeCodeFormatType_TimeCode64 = 'tc64';
  kCMTimeCodeFormatType_Counter32 = 'cn32';
  kCMTimeCodeFormatType_Counter64 = 'cn64';

type
  CMTimeCodeFormatType = FourCharCode;
  CMTimeCodeFormatTypePtr = ^CMTimeCodeFormatType;

const
  kCMTimeCodeFlag_DropFrame = 1 shl 0;
  kCMTimeCodeFlag_24HourMax = 1 shl 1;
  kCMTimeCodeFlag_NegTimesOK = 1 shl 2;
{$endif}

{$ifdef FUNCTIONS}
function CMTimeCodeFormatDescriptionCreate(allocator: CFAllocatorRef; timeCodeFormatType: CMTimeCodeFormatType; frameDuration: CMTime; frameQuanta: cuint32; tcFlags: cuint32; extensions: CFDictionaryRef; descOut: CMTimeCodeFormatDescriptionRefPtr): OSStatus; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMTimeCodeFormatDescriptionGetFrameDuration(timeCodeFormatDescription: CMTimeCodeFormatDescriptionRef): CMTime; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMTimeCodeFormatDescriptionGetFrameQuanta(timeCodeFormatDescription: CMTimeCodeFormatDescriptionRef): cuint32; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMTimeCodeFormatDescriptionGetTimeCodeFlags(desc: CMTimeCodeFormatDescriptionRef): cuint32; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  kCMTimeCodeFormatDescriptionExtension_SourceReferenceName: CFStringRef; cvar; external;
  kCMTimeCodeFormatDescriptionKey_Value: CFStringRef; cvar; external;
  kCMTimeCodeFormatDescriptionKey_LangCode: CFStringRef; cvar; external;
{$endif}

{$ifdef TYPES}
type
  CMMetadataFormatDescriptionRef = CMFormatDescriptionRef;
  CMMetadataFormatDescriptionRefPtr = ^CMMetadataFormatDescriptionRef;

const
  kCMMetadataFormatType_ICY = 'icy ';
  kCMMetadataFormatType_ID3 = 'id3 ';
  kCMMetadataFormatType_Boxed = 'mebx';

type
  CMMetadataFormatType = FourCharCode;
  CMMetadataFormatTypePtr = ^CMMetadataFormatType;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  kCMFormatDescriptionExtensionKey_MetadataKeyTable: CFStringRef { available starting in __MAC_10_7,__IPHONE_4_0 }; cvar; external;
  kCMMetadataFormatDescriptionKey_Namespace: CFStringRef; cvar; external;
  kCMMetadataFormatDescriptionKey_Value: CFStringRef; cvar; external;
  kCMMetadataFormatDescriptionKey_LocalID: CFStringRef; cvar; external;
  kCMMetadataFormatDescriptionKey_DataType: CFStringRef; cvar; external;
  kCMMetadataFormatDescriptionKey_DataTypeNamespace: CFStringRef; cvar; external;
  kCMMetadataFormatDescriptionKey_ConformingDataTypes: CFStringRef; cvar; external;
  kCMMetadataFormatDescriptionKey_LanguageTag: CFStringRef; cvar; external;
  kCMMetadataFormatDescriptionMetadataSpecificationKey_Identifier: CFStringRef; cvar; external;
  kCMMetadataFormatDescriptionMetadataSpecificationKey_DataType: CFStringRef; cvar; external;
  kCMMetadataFormatDescriptionMetadataSpecificationKey_ExtendedLanguageTag: CFStringRef; cvar; external;
{$endif}

{$ifdef FUNCTIONS}
function CMMetadataFormatDescriptionCreateWithKeys(allocator: CFAllocatorRef; metadataType: CMMetadataFormatType; keys: CFArrayRef; outDesc: CMMetadataFormatDescriptionRefPtr): OSStatus; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMMetadataFormatDescriptionCreateWithMetadataSpecifications(allocator: CFAllocatorRef; metadataType: CMMetadataFormatType; metadataSpecifications: CFArrayRef; outDesc: CMMetadataFormatDescriptionRefPtr): OSStatus; cdecl; external; { available starting in __MAC_10_10,__IPHONE_8_0 }
function CMMetadataFormatDescriptionCreateWithMetadataFormatDescriptionAndMetadataSpecifications(allocator: CFAllocatorRef; srcDesc: CMMetadataFormatDescriptionRef; metadataSpecifications: CFArrayRef; outDesc: CMMetadataFormatDescriptionRefPtr): OSStatus; cdecl; external; { available starting in __MAC_10_10,__IPHONE_8_0 }
function CMMetadataFormatDescriptionCreateByMergingMetadataFormatDescriptions(allocator: CFAllocatorRef; srcDesc1: CMMetadataFormatDescriptionRef; srcDesc2: CMMetadataFormatDescriptionRef; outDesc: CMMetadataFormatDescriptionRefPtr): OSStatus; cdecl; external; { available starting in __MAC_10_10,__IPHONE_8_0 }
function CMMetadataFormatDescriptionGetKeyWithLocalID(desc: CMMetadataFormatDescriptionRef; localKeyID: OSType): CFDictionaryRef; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMMetadataFormatDescriptionGetIdentifiers(desc: CMMetadataFormatDescriptionRef): CFArrayRef; cdecl; external; { available starting in __MAC_10_10,__IPHONE_8_0 }
{$endif}

{$endif}
