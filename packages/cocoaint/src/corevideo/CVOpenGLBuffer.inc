{ Parsed from CoreVideo.framework CVOpenGLBuffer.h }

{$if  not (defined(__COREVIDEO_CVOPENGLBUFFER_H__))}
{$ifdef TYPES}
const
  __COREVIDEO_CVOPENGLBUFFER_H__ = 1;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  kCVOpenGLBufferWidth: CFStringRef; cvar; external;
  kCVOpenGLBufferHeight: CFStringRef; cvar; external;
  kCVOpenGLBufferTarget: CFStringRef; cvar; external;
  kCVOpenGLBufferInternalFormat: CFStringRef; cvar; external;
  kCVOpenGLBufferMaximumMipmapLevel: CFStringRef; cvar; external;
{$endif}

{$ifdef TYPES}
type
  CVOpenGLBufferRef = CVImageBufferRef;
  CVOpenGLBufferRefPtr = ^CVOpenGLBufferRef;
{$endif}

{$ifdef FUNCTIONS}
function CVOpenGLBufferGetTypeID: CFTypeID; cdecl; external;
function CVOpenGLBufferRetain(buffer: CVOpenGLBufferRef): CVOpenGLBufferRef; cdecl; external;
procedure CVOpenGLBufferRelease(buffer: CVOpenGLBufferRef); cdecl; external;
function CVOpenGLBufferCreate(allocator: CFAllocatorRef; width: size_t; height: size_t; attributes: CFDictionaryRef; var bufferOut: CVOpenGLBufferRef): CVReturn; cdecl; external;
function CVOpenGLBufferGetAttributes(openGLBuffer: CVOpenGLBufferRef): CFDictionaryRef; cdecl; external;
function CVOpenGLBufferAttach(openGLBuffer: CVOpenGLBufferRef; cglContext: CGLContextObj; face: GLenum; level: GLint; screen: GLint): CVReturn; cdecl; external;
{$endif}

{$endif}
