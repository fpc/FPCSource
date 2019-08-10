{ Parsed from CoreVideo.framework CVOpenGLTexture.h }

{$if  not (defined(__COREVIDEO_CVOPENGLTEXTURE_H__))}
{$ifdef TYPES}
const
  __COREVIDEO_CVOPENGLTEXTURE_H__ = 1;
{$endif}

{$ifdef TYPES}
type
  CVOpenGLTextureRef = CVImageBufferRef;
  CVOpenGLTextureRefPtr = ^CVOpenGLTextureRef;
{$endif}

{$ifdef FUNCTIONS}
function CVOpenGLTextureGetTypeID: CFTypeID; cdecl; external;
function CVOpenGLTextureRetain(texture: CVOpenGLTextureRef): CVOpenGLTextureRef; cdecl; external;
procedure CVOpenGLTextureRelease(texture: CVOpenGLTextureRef); cdecl; external;
function CVOpenGLTextureGetTarget(image: CVOpenGLTextureRef): GLenum; cdecl; external;
function CVOpenGLTextureGetName(image: CVOpenGLTextureRef): GLuint; cdecl; external;
function CVOpenGLTextureIsFlipped(image: CVOpenGLTextureRef): Boolean; cdecl; external;
procedure CVOpenGLTextureGetCleanTexCoords(image: CVOpenGLTextureRef; lowerLeft: GLfloatPtr { 2 element array of GLfloat }; lowerRight: GLfloatPtr { 2 element array of GLfloat }; upperRight: GLfloatPtr { 2 element array of GLfloat }; upperLeft: GLfloatPtr { 2 element array of GLfloat }); cdecl; external;
{$endif}

{$endif}
