{ Parsed from CoreVideo.framework CVPixelBufferIOSurface.h }

{$if  not (defined(__COREVIDEO_CVPIXELBUFFERIOSURFACE_H__))}
{$ifdef TYPES}
const
  __COREVIDEO_CVPIXELBUFFERIOSURFACE_H__ = 1;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  kCVPixelBufferIOSurfaceOpenGLTextureCompatibilityKey: CFStringRef; cvar; external;
  kCVPixelBufferIOSurfaceOpenGLFBOCompatibilityKey: CFStringRef; cvar; external;
  kCVPixelBufferIOSurfaceCoreAnimationCompatibilityKey: CFStringRef; cvar; external;
  kCVPixelBufferIOSurfaceOpenGLESTextureCompatibilityKey: CFStringRef { available starting in __MAC_NA,__IPHONE_5_0 }; cvar; external;
  kCVPixelBufferIOSurfaceOpenGLESFBOCompatibilityKey: CFStringRef { available starting in __MAC_NA,__IPHONE_5_0 }; cvar; external;
{$endif}

{$if defined(COREVIDEO_SUPPORTS_IOSURFACE)}
{$ifdef FUNCTIONS}
function CVPixelBufferGetIOSurface(pixelBuffer: CVPixelBufferRef): IOSurfaceRef; cdecl; external; { available starting in __MAC_10_6,__IPHONE_4_0 }
function CVPixelBufferCreateWithIOSurface(allocator: CFAllocatorRef; surface: IOSurfaceRef; pixelBufferAttributes: CFDictionaryRef; var pixelBufferOut: CVPixelBufferRef): CVReturn; cdecl; external; { available starting in __MAC_10_6,__IPHONE_4_0 }
{$endif}

{$endif}

{$endif}
