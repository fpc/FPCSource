{ Parsed from CoreVideo.framework CVPixelBuffer.h }

{$if  not (defined(__COREVIDEO_CVPIXELBUFFER_H__))}
{$ifdef TYPES}
const
  __COREVIDEO_CVPIXELBUFFER_H__ = 1;
{$endif}

{$ifdef TYPES}
const
  kCVPixelFormatType_1Monochrome = $00000001;
  kCVPixelFormatType_2Indexed = $00000002;
  kCVPixelFormatType_4Indexed = $00000004;
  kCVPixelFormatType_8Indexed = $00000008;
  kCVPixelFormatType_1IndexedGray_WhiteIsZero = $00000021;
  kCVPixelFormatType_2IndexedGray_WhiteIsZero = $00000022;
  kCVPixelFormatType_4IndexedGray_WhiteIsZero = $00000024;
  kCVPixelFormatType_8IndexedGray_WhiteIsZero = $00000028;
  kCVPixelFormatType_16BE555 = $00000010;
  kCVPixelFormatType_16LE555 = 'L555';
  kCVPixelFormatType_16LE5551 = '5551';
  kCVPixelFormatType_16BE565 = 'B565';
  kCVPixelFormatType_16LE565 = 'L565';
  kCVPixelFormatType_24RGB = $00000018;
  kCVPixelFormatType_24BGR = '24BG';
  kCVPixelFormatType_32ARGB = $00000020;
  kCVPixelFormatType_32BGRA = 'BGRA';
  kCVPixelFormatType_32ABGR = 'ABGR';
  kCVPixelFormatType_32RGBA = 'RGBA';
  kCVPixelFormatType_64ARGB = 'b64a';
  kCVPixelFormatType_48RGB = 'b48r';
  kCVPixelFormatType_32AlphaGray = 'b32a';
  kCVPixelFormatType_16Gray = 'b16g';
  kCVPixelFormatType_30RGB = 'R10k';
  kCVPixelFormatType_422YpCbCr8 = '2vuy';
  kCVPixelFormatType_4444YpCbCrA8 = 'v408';
  kCVPixelFormatType_4444YpCbCrA8R = 'r408';
  kCVPixelFormatType_4444AYpCbCr8 = 'y408';
  kCVPixelFormatType_4444AYpCbCr16 = 'y416';
  kCVPixelFormatType_444YpCbCr8 = 'v308';
  kCVPixelFormatType_422YpCbCr16 = 'v216';
  kCVPixelFormatType_422YpCbCr10 = 'v210';
  kCVPixelFormatType_444YpCbCr10 = 'v410';
  kCVPixelFormatType_420YpCbCr8Planar = 'y420';
  kCVPixelFormatType_420YpCbCr8PlanarFullRange = 'f420';
  kCVPixelFormatType_422YpCbCr_4A_8BiPlanar = 'a2vy';
  kCVPixelFormatType_420YpCbCr8BiPlanarVideoRange = '420v';
  kCVPixelFormatType_420YpCbCr8BiPlanarFullRange = '420f';
  kCVPixelFormatType_422YpCbCr8_yuvs = 'yuvs';
  kCVPixelFormatType_422YpCbCr8FullRange = 'yuvf';
  kCVPixelFormatType_OneComponent8 = 'L008';
  kCVPixelFormatType_TwoComponent8 = '2C08';
  kCVPixelFormatType_OneComponent16Half = 'L00h';
  kCVPixelFormatType_OneComponent32Float = 'L00f';
  kCVPixelFormatType_TwoComponent16Half = '2C0h';
  kCVPixelFormatType_TwoComponent32Float = '2C0f';
  kCVPixelFormatType_64RGBAHalf = 'RGhA';
  kCVPixelFormatType_128RGBAFloat = 'RGfA';
const
  kCVPixelBufferLock_ReadOnly = $00000001;

type
  CVPlanarComponentInfo = record
    offset: cint32;
    rowBytes: cuint32;
  end;
type
  CVPlanarComponentInfoPtr = ^CVPlanarComponentInfo;
type
  CVPlanarPixelBufferInfo = record
    componentInfo:array[0..0] of CVPlanarComponentInfo;
  end;
type
  CVPlanarPixelBufferInfoPtr = ^CVPlanarPixelBufferInfo;
type
  CVPlanarPixelBufferInfo_YCbCrPlanar = record
    componentInfoY: CVPlanarComponentInfo;
    componentInfoCb: CVPlanarComponentInfo;
    componentInfoCr: CVPlanarComponentInfo;
  end;
type
  CVPlanarPixelBufferInfo_YCbCrPlanarPtr = ^CVPlanarPixelBufferInfo_YCbCrPlanar;
type
  CVPlanarPixelBufferInfo_YCbCrBiPlanar = record
    componentInfoY: CVPlanarComponentInfo;
    componentInfoCbCr: CVPlanarComponentInfo;
  end;
type
  CVPlanarPixelBufferInfo_YCbCrBiPlanarPtr = ^CVPlanarPixelBufferInfo_YCbCrBiPlanar;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  kCVPixelBufferPixelFormatTypeKey: CFStringRef { available starting in __MAC_10_4,__IPHONE_4_0 }; cvar; external;
  kCVPixelBufferMemoryAllocatorKey: CFStringRef { available starting in __MAC_10_4,__IPHONE_4_0 }; cvar; external;
  kCVPixelBufferWidthKey: CFStringRef { available starting in __MAC_10_4,__IPHONE_4_0 }; cvar; external;
  kCVPixelBufferHeightKey: CFStringRef { available starting in __MAC_10_4,__IPHONE_4_0 }; cvar; external;
  kCVPixelBufferExtendedPixelsLeftKey: CFStringRef { available starting in __MAC_10_4,__IPHONE_4_0 }; cvar; external;
  kCVPixelBufferExtendedPixelsTopKey: CFStringRef { available starting in __MAC_10_4,__IPHONE_4_0 }; cvar; external;
  kCVPixelBufferExtendedPixelsRightKey: CFStringRef { available starting in __MAC_10_4,__IPHONE_4_0 }; cvar; external;
  kCVPixelBufferExtendedPixelsBottomKey: CFStringRef { available starting in __MAC_10_4,__IPHONE_4_0 }; cvar; external;
  kCVPixelBufferBytesPerRowAlignmentKey: CFStringRef { available starting in __MAC_10_4,__IPHONE_4_0 }; cvar; external;
  kCVPixelBufferCGBitmapContextCompatibilityKey: CFStringRef { available starting in __MAC_10_4,__IPHONE_4_0 }; cvar; external;
  kCVPixelBufferCGImageCompatibilityKey: CFStringRef { available starting in __MAC_10_4,__IPHONE_4_0 }; cvar; external;
  kCVPixelBufferOpenGLCompatibilityKey: CFStringRef { available starting in __MAC_10_4,__IPHONE_4_0 }; cvar; external;
  kCVPixelBufferPlaneAlignmentKey: CFStringRef { available starting in __MAC_10_6,__IPHONE_4_0 }; cvar; external;
  kCVPixelBufferIOSurfacePropertiesKey: CFStringRef { available starting in __MAC_10_6,__IPHONE_4_0 }; cvar; external;
  kCVPixelBufferOpenGLESCompatibilityKey: CFStringRef { available starting in __MAC_NA,__IPHONE_6_0 }; cvar; external;
  kCVPixelBufferMetalCompatibilityKey: CFStringRef { available starting in __MAC_NA,__IPHONE_8_0 }; cvar; external;
{$endif}

{$ifdef TYPES}
type
  CVPixelBufferRef = CVImageBufferRef;
  CVPixelBufferRefPtr = ^CVPixelBufferRef;
{$endif}

{$ifdef FUNCTIONS}
function CVPixelBufferGetTypeID: CFTypeID; cdecl; external; { available starting in __MAC_10_4,__IPHONE_4_0 }
function CVPixelBufferRetain(texture: CVPixelBufferRef): CVPixelBufferRef; cdecl; external; { available starting in __MAC_10_4,__IPHONE_4_0 }
procedure CVPixelBufferRelease(texture: CVPixelBufferRef); cdecl; external; { available starting in __MAC_10_4,__IPHONE_4_0 }
function CVPixelBufferCreateResolvedAttributesDictionary(allocator: CFAllocatorRef; attributes: CFArrayRef; var resolvedDictionaryOut: CFDictionaryRef): CVReturn; cdecl; external; { available starting in __MAC_10_4,__IPHONE_4_0 }
function CVPixelBufferCreate(allocator: CFAllocatorRef; width: size_t; height: size_t; pixelFormatType: OSType; pixelBufferAttributes: CFDictionaryRef; var pixelBufferOut: CVPixelBufferRef): CVReturn; cdecl; external; { available starting in __MAC_10_4,__IPHONE_4_0 }
{$endif}

{$ifdef TYPES}
type
  CVPixelBufferReleaseBytesCallback = procedure (releaseRefCon: pointer; baseAddress: pointer); cdecl;
{$endif}

{$ifdef FUNCTIONS}
function CVPixelBufferCreateWithBytes(allocator: CFAllocatorRef; width: size_t; height: size_t; pixelFormatType: OSType; baseAddress: pointer; bytesPerRow: size_t; releaseCallback: CVPixelBufferReleaseBytesCallback; releaseRefCon: pointer; pixelBufferAttributes: CFDictionaryRef; var pixelBufferOut: CVPixelBufferRef): CVReturn; cdecl; external; { available starting in __MAC_10_4,__IPHONE_4_0 }
{$endif}

{$ifdef TYPES}
type
  CVPixelBufferReleasePlanarBytesCallback = procedure (releaseRefCon: pointer; dataPtr: pointer; dataSize: size_t; numberOfPlanes: size_t; planeAddresses: pointer { variable size array of pointer }); cdecl;
{$endif}

{$ifdef FUNCTIONS}
function CVPixelBufferCreateWithPlanarBytes(allocator: CFAllocatorRef; width: size_t; height: size_t; pixelFormatType: OSType; dataPtr: pointer; dataSize: size_t; numberOfPlanes: size_t; planeBaseAddress: pointer { variable size array of pointer }; var planeWidth: size_tPtr { variable size array of size_t }; planeHeight: size_tPtr { variable size array of size_t }; planeBytesPerRow: size_tPtr { variable size array of size_t }; releaseCallback: CVPixelBufferReleasePlanarBytesCallback; releaseRefCon: pointer; pixelBufferAttributes: CFDictionaryRef; pixelBufferOut: CVPixelBufferRefPtr): CVReturn; cdecl; external; { available starting in __MAC_10_4,__IPHONE_4_0 }
function CVPixelBufferLockBaseAddress(pixelBuffer: CVPixelBufferRef; lockFlags: CVOptionFlags): CVReturn; cdecl; external; { available starting in __MAC_10_4,__IPHONE_4_0 }
function CVPixelBufferUnlockBaseAddress(pixelBuffer: CVPixelBufferRef; unlockFlags: CVOptionFlags): CVReturn; cdecl; external; { available starting in __MAC_10_4,__IPHONE_4_0 }
function CVPixelBufferGetWidth(pixelBuffer: CVPixelBufferRef): size_t; cdecl; external; { available starting in __MAC_10_4,__IPHONE_4_0 }
function CVPixelBufferGetHeight(pixelBuffer: CVPixelBufferRef): size_t; cdecl; external; { available starting in __MAC_10_4,__IPHONE_4_0 }
function CVPixelBufferGetPixelFormatType(pixelBuffer: CVPixelBufferRef): OSType; cdecl; external; { available starting in __MAC_10_4,__IPHONE_4_0 }
function CVPixelBufferGetBaseAddress(pixelBuffer: CVPixelBufferRef): pointer; cdecl; external; { available starting in __MAC_10_4,__IPHONE_4_0 }
function CVPixelBufferGetBytesPerRow(pixelBuffer: CVPixelBufferRef): size_t; cdecl; external; { available starting in __MAC_10_4,__IPHONE_4_0 }
function CVPixelBufferGetDataSize(pixelBuffer: CVPixelBufferRef): size_t; cdecl; external; { available starting in __MAC_10_4,__IPHONE_4_0 }
function CVPixelBufferIsPlanar(pixelBuffer: CVPixelBufferRef): Boolean; cdecl; external; { available starting in __MAC_10_4,__IPHONE_4_0 }
function CVPixelBufferGetPlaneCount(pixelBuffer: CVPixelBufferRef): size_t; cdecl; external; { available starting in __MAC_10_4,__IPHONE_4_0 }
function CVPixelBufferGetWidthOfPlane(pixelBuffer: CVPixelBufferRef; planeIndex: size_t): size_t; cdecl; external; { available starting in __MAC_10_4,__IPHONE_4_0 }
function CVPixelBufferGetHeightOfPlane(pixelBuffer: CVPixelBufferRef; planeIndex: size_t): size_t; cdecl; external; { available starting in __MAC_10_4,__IPHONE_4_0 }
function CVPixelBufferGetBaseAddressOfPlane(pixelBuffer: CVPixelBufferRef; planeIndex: size_t): pointer; cdecl; external; { available starting in __MAC_10_4,__IPHONE_4_0 }
function CVPixelBufferGetBytesPerRowOfPlane(pixelBuffer: CVPixelBufferRef; planeIndex: size_t): size_t; cdecl; external; { available starting in __MAC_10_4,__IPHONE_4_0 }
procedure CVPixelBufferGetExtendedPixels(pixelBuffer: CVPixelBufferRef; var extraColumnsOnLeft: size_t; var extraColumnsOnRight: size_t; var extraRowsOnTop: size_t; var extraRowsOnBottom: size_t); cdecl; external; { available starting in __MAC_10_4,__IPHONE_4_0 }
function CVPixelBufferFillExtendedPixels(pixelBuffer: CVPixelBufferRef): CVReturn; cdecl; external; { available starting in __MAC_10_4,__IPHONE_4_0 }
{$endif}

{$endif}
