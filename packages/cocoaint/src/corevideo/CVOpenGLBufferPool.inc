{ Parsed from CoreVideo.framework CVOpenGLBufferPool.h }

{$if  not (defined(__COREVIDEO__CVOPENGLBUFFERPOOL_H__))}
{$ifdef TYPES}
const
  __COREVIDEO__CVOPENGLBUFFERPOOL_H__ = 1;
{$endif}

{$ifdef TYPES}
type
  CVOpenGLBufferPoolRef = OpaqueType;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  kCVOpenGLBufferPoolMinimumBufferCountKey: CFStringRef; cvar; external;
  kCVOpenGLBufferPoolMaximumBufferAgeKey: CFStringRef; cvar; external;
{$endif}

{$ifdef FUNCTIONS}
function CVOpenGLBufferPoolGetTypeID: CFTypeID; cdecl; external;
function CVOpenGLBufferPoolRetain(openGLBufferPool: CVOpenGLBufferPoolRef): CVOpenGLBufferPoolRef; cdecl; external;
procedure CVOpenGLBufferPoolRelease(openGLBufferPool: CVOpenGLBufferPoolRef); cdecl; external;
function CVOpenGLBufferPoolCreate(allocator: CFAllocatorRef; poolAttributes: CFDictionaryRef; openGLBufferAttributes: CFDictionaryRef; var poolOut: CVOpenGLBufferPoolRef): CVReturn; cdecl; external;
function CVOpenGLBufferPoolGetAttributes(pool: CVOpenGLBufferPoolRef): CFDictionaryRef; cdecl; external;
function CVOpenGLBufferPoolGetOpenGLBufferAttributes(pool: CVOpenGLBufferPoolRef): CFDictionaryRef; cdecl; external;
function CVOpenGLBufferPoolCreateOpenGLBuffer(allocator: CFAllocatorRef; openGLBufferPool: CVOpenGLBufferPoolRef; var openGLBufferOut: CVOpenGLBufferRef): CVReturn; cdecl; external;
{$endif}

{$endif}
