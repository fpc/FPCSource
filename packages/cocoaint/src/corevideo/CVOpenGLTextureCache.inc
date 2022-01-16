{ Parsed from CoreVideo.framework CVOpenGLTextureCache.h }

{$if  not (defined(__COREVIDEO__CVOPENGLTEXTURECACHE_H__))}
{$ifdef TYPES}
const
  __COREVIDEO__CVOPENGLTEXTURECACHE_H__ = 1;
{$endif}

{$ifdef TYPES}
type
  CVOpenGLTextureCacheRef = OpaqueType;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  kCVOpenGLTextureCacheChromaSamplingModeKey: CFStringRef; cvar; external;
  kCVOpenGLTextureCacheChromaSamplingModeAutomatic: CFStringRef; cvar; external;
  kCVOpenGLTextureCacheChromaSamplingModeHighestQuality: CFStringRef; cvar; external;
  kCVOpenGLTextureCacheChromaSamplingModeBestPerformance: CFStringRef; cvar; external;
{$endif}

{$ifdef FUNCTIONS}
function CVOpenGLTextureCacheGetTypeID: CFTypeID; cdecl; external;
function CVOpenGLTextureCacheRetain(textureCache: CVOpenGLTextureCacheRef): CVOpenGLTextureCacheRef; cdecl; external;
procedure CVOpenGLTextureCacheRelease(textureCache: CVOpenGLTextureCacheRef); cdecl; external;
function CVOpenGLTextureCacheCreate(allocator: CFAllocatorRef; cacheAttributes: CFDictionaryRef; cglContext: CGLContextObj; cglPixelFormat: CGLPixelFormatObj; textureAttributes: CFDictionaryRef; var cacheOut: CVOpenGLTextureCacheRef): CVReturn; cdecl; external;
function CVOpenGLTextureCacheCreateTextureFromImage(allocator: CFAllocatorRef; textureCache: CVOpenGLTextureCacheRef; sourceImage: CVImageBufferRef; attributes: CFDictionaryRef; var textureOut: CVOpenGLTextureRef): CVReturn; cdecl; external;
procedure CVOpenGLTextureCacheFlush(textureCache: CVOpenGLTextureCacheRef; options: CVOptionFlags); cdecl; external;
{$endif}

{$endif}
