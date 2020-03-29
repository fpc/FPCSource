{ Parsed from CoreVideo.framework CVPixelFormatDescription.h }

{$if  not (defined(__COREVIDEO_CVPIXELFORMATDESCRIPTION_H__))}

{$ifdef EXTERNAL_SYMBOLS}
var
  kCVPixelFormatName: CFStringRef { available starting in __MAC_10_4,__IPHONE_4_0 }; cvar; external;
  kCVPixelFormatConstant: CFStringRef { available starting in __MAC_10_4,__IPHONE_4_0 }; cvar; external;
  kCVPixelFormatCodecType: CFStringRef { available starting in __MAC_10_4,__IPHONE_4_0 }; cvar; external;
  kCVPixelFormatFourCC: CFStringRef { available starting in __MAC_10_4,__IPHONE_4_0 }; cvar; external;
  kCVPixelFormatContainsAlpha: CFStringRef { available starting in __MAC_10_7,__IPHONE_4_3 }; cvar; external;
  kCVPixelFormatContainsYCbCr: CFStringRef { available starting in __MAC_10_10,__IPHONE_8_0 }; cvar; external;
  kCVPixelFormatContainsRGB: CFStringRef { available starting in __MAC_10_10,__IPHONE_8_0 }; cvar; external;
  kCVPixelFormatPlanes: CFStringRef { available starting in __MAC_10_4,__IPHONE_4_0 }; cvar; external;
  kCVPixelFormatBlockWidth: CFStringRef { available starting in __MAC_10_4,__IPHONE_4_0 }; cvar; external;
  kCVPixelFormatBlockHeight: CFStringRef { available starting in __MAC_10_4,__IPHONE_4_0 }; cvar; external;
  kCVPixelFormatBitsPerBlock: CFStringRef { available starting in __MAC_10_4,__IPHONE_4_0 }; cvar; external;
  kCVPixelFormatBlockHorizontalAlignment: CFStringRef { available starting in __MAC_10_4,__IPHONE_4_0 }; cvar; external;
  kCVPixelFormatBlockVerticalAlignment: CFStringRef { available starting in __MAC_10_4,__IPHONE_4_0 }; cvar; external;
  kCVPixelFormatBlackBlock: CFStringRef { available starting in __MAC_10_6,__IPHONE_4_0 }; cvar; external;
  kCVPixelFormatHorizontalSubsampling: CFStringRef { available starting in __MAC_10_4,__IPHONE_4_0 }; cvar; external;
  kCVPixelFormatVerticalSubsampling: CFStringRef { available starting in __MAC_10_4,__IPHONE_4_0 }; cvar; external;
  kCVPixelFormatOpenGLFormat: CFStringRef { available starting in __MAC_10_4,__IPHONE_4_0 }; cvar; external;
  kCVPixelFormatOpenGLType: CFStringRef { available starting in __MAC_10_4,__IPHONE_4_0 }; cvar; external;
  kCVPixelFormatOpenGLInternalFormat: CFStringRef { available starting in __MAC_10_4,__IPHONE_4_0 }; cvar; external;
  kCVPixelFormatCGBitmapInfo: CFStringRef { available starting in __MAC_10_4,__IPHONE_4_0 }; cvar; external;
  kCVPixelFormatQDCompatibility: CFStringRef { available starting in __MAC_10_4,__IPHONE_4_0 }; cvar; external;
  kCVPixelFormatCGBitmapContextCompatibility: CFStringRef { available starting in __MAC_10_4,__IPHONE_4_0 }; cvar; external;
  kCVPixelFormatCGImageCompatibility: CFStringRef { available starting in __MAC_10_4,__IPHONE_4_0 }; cvar; external;
  kCVPixelFormatOpenGLCompatibility: CFStringRef { available starting in __MAC_10_4,__IPHONE_4_0 }; cvar; external;
  kCVPixelFormatOpenGLESCompatibility: CFStringRef { available starting in __MAC_NA,__IPHONE_5_0 }; cvar; external;
{$endif}

{$ifdef TYPES}
type
  CVFillExtendedPixelsCallBack = function (pixelBuffer: CVPixelBufferRef; refCon: pointer): Boolean; cdecl;

type
  CVFillExtendedPixelsCallBackData = record
    version: CFIndex;
    fillCallBack: CVFillExtendedPixelsCallBack;
    refCon: pointer;
  end;
type
  CVFillExtendedPixelsCallBackDataPtr = ^CVFillExtendedPixelsCallBackData;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  kCVPixelFormatFillExtendedPixelsCallback: CFStringRef { available starting in __MAC_10_4,__IPHONE_4_0 }; cvar; external;
{$endif}

{$ifdef FUNCTIONS}
function CVPixelFormatDescriptionCreateWithPixelFormatType(allocator: CFAllocatorRef; pixelFormat: OSType): CFDictionaryRef; cdecl; external; { available starting in __MAC_10_4,__IPHONE_4_0 }
function CVPixelFormatDescriptionArrayCreateWithAllPixelFormatTypes(allocator: CFAllocatorRef): CFArrayRef; cdecl; external; { available starting in __MAC_10_4,__IPHONE_4_0 }
procedure CVPixelFormatDescriptionRegisterDescriptionWithPixelFormatType(description: CFDictionaryRef; pixelFormat: OSType); cdecl; external; { available starting in __MAC_10_4,__IPHONE_4_0 }
{$endif}

{$if defined(COREVIDEO_SUPPORTS_DIRECT3D)}
{$ifdef EXTERNAL_SYMBOLS}
var
  kCVPixelFormatDirect3DFormat: CFStringRef; cvar; external;
  kCVPixelFormatDirect3DType: CFStringRef; cvar; external;
  kCVPixelFormatDirect3DInternalFormat: CFStringRef; cvar; external;
  kCVPixelFormatDirect3DCompatibility: CFStringRef; cvar; external;
{$endif}

{$endif}

{$endif}
