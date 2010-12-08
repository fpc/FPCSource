{ Parsed from Appkit.framework NSOpenGL.h }
{ Version: 2.1.2 - Wed Dec 8 10:06:06 CET 2010 }


{$ifdef TYPES}
{$ifndef NSOPENGL_PAS_T}
{$define NSOPENGL_PAS_T}

{ Defines }
const
  NSOPENGL_CURRENT_VERSION = 1;

{ Types }
type
  
  NSOpenGLGlobalOption = culong;
  
  NSOpenGLContextParameter = culong;

{ Constants }

const
  NSOpenGLGOFormatCacheSize = 501;
  NSOpenGLGOClearFormatCache = 502;
  NSOpenGLGORetainRenderers = 503;
  NSOpenGLGOResetLibrary = 504;

const
  NSOpenGLPFAAllRenderers = 1;
  NSOpenGLPFADoubleBuffer = 5;
  NSOpenGLPFAStereo = 6;
  NSOpenGLPFAAuxBuffers = 7;
  NSOpenGLPFAColorSize = 8;
  NSOpenGLPFAAlphaSize = 11;
  NSOpenGLPFADepthSize = 12;
  NSOpenGLPFAStencilSize = 13;
  NSOpenGLPFAAccumSize = 14;
  NSOpenGLPFAMinimumPolicy = 51;
  NSOpenGLPFAMaximumPolicy = 52;
  NSOpenGLPFAOffScreen = 53;
  NSOpenGLPFAFullScreen = 54;
  NSOpenGLPFASampleBuffers = 55;
  NSOpenGLPFASamples = 56;
  NSOpenGLPFAAuxDepthStencil = 57;
  NSOpenGLPFAColorFloat = 58;
  NSOpenGLPFAMultisample = 59;
  NSOpenGLPFASupersample = 60;
  NSOpenGLPFASampleAlpha = 61;
  NSOpenGLPFARendererID = 70;
  NSOpenGLPFASingleRenderer = 71;
  NSOpenGLPFANoRecovery = 72;
  NSOpenGLPFAAccelerated = 73;
  NSOpenGLPFAClosestPolicy = 74;
  NSOpenGLPFARobust = 75;
  NSOpenGLPFABackingStore = 76;
  NSOpenGLPFAMPSafe = 78;
  NSOpenGLPFAWindow = 80;
  NSOpenGLPFAMultiScreen = 81;
  NSOpenGLPFACompliant = 83;
  NSOpenGLPFAScreenMask = 84;
  NSOpenGLPFAPixelBuffer = 90;
  NSOpenGLPFARemotePixelBuffer = 91;
  NSOpenGLPFAAllowOfflineRenderers = 96;
  NSOpenGLPFAAcceleratedCompute = 97;
  NSOpenGLPFAVirtualScreenCount = 128;

const
  NSOpenGLCPSwapRectangle = 200;
  NSOpenGLCPSwapRectangleEnable = 201;
  NSOpenGLCPRasterizationEnable = 221;
  NSOpenGLCPSwapInterval = 222;
  NSOpenGLCPSurfaceOrder = 235;
  NSOpenGLCPSurfaceOpacity = 236;
  NSOpenGLCPStateValidation = 301;

{ Types }
type
  NSOpenGLPixelFormatAttribute = cuint32;
  NSOpenGLPixelFormatAttributePtr = ^NSOpenGLPixelFormatAttribute;
  _CGLPixelFormatObject = record end;
  NSOpenGLPixelFormatAuxiliary = _CGLPixelFormatObject;
  NSOpenGLPixelFormatAuxiliaryPtr = ^NSOpenGLPixelFormatAuxiliary;
  _CGLContextObject = record end;
  NSOpenGLContextAuxiliary = _CGLContextObject;
  NSOpenGLContextAuxiliaryPtr = ^NSOpenGLContextAuxiliary;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSOPENGL_PAS_R}
{$define NSOPENGL_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSOPENGL_PAS_F}
{$define NSOPENGL_PAS_F}

{ Functions }
procedure NSOpenGLSetOption(pname: NSOpenGLGlobalOption; param: GLint); cdecl; external;
procedure NSOpenGLGetOption(pname: NSOpenGLGlobalOption; param: GLintPtr); cdecl; external;
procedure NSOpenGLGetVersion(major: GLintPtr; minor: GLintPtr); cdecl; external;

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSOPENGL_PAS_S}
{$define NSOPENGL_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSOpenGLPixelFormat = objcclass;
  NSOpenGLPixelFormatPointer = ^NSOpenGLPixelFormat;
  NSOpenGLPixelFormatPtr = NSOpenGLPixelFormatPointer;
  NSOpenGLPixelBuffer = objcclass;
  NSOpenGLPixelBufferPointer = ^NSOpenGLPixelBuffer;
  NSOpenGLPixelBufferPtr = NSOpenGLPixelBufferPointer;
  NSOpenGLContext = objcclass;
  NSOpenGLContextPointer = ^NSOpenGLContext;
  NSOpenGLContextPtr = NSOpenGLContextPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSOPENGL_PAS_C}
{$define NSOPENGL_PAS_C}

{ NSOpenGLPixelFormat }
  NSOpenGLPixelFormat = objcclass(NSObject, NSCodingProtocol)
  private
    _pixelFormatAuxiliary: NSOpenGLPixelFormatAuxiliaryPtr;
    _pixelAttributes: NSData;
    _reserved1: NSInteger;
    _reserved2: NSInteger;
    _reserved3: NSInteger;
    
  public
    class function alloc: NSOpenGLPixelFormat; message 'alloc';

    function initWithAttributes(attribs: NSOpenGLPixelFormatAttributePtr): id; message 'initWithAttributes:';
    function initWithData(attribs: NSData): id; message 'initWithData:';
    function initWithCGLPixelFormatObj(format: Pointer): id; message 'initWithCGLPixelFormatObj:';
    function attributes: NSData; message 'attributes';
    procedure setAttributes(attribs: NSData); message 'setAttributes:';
    procedure getValues_forAttribute_forVirtualScreen(vals: GLintPtr; attrib: NSOpenGLPixelFormatAttribute; screen: GLint); message 'getValues:forAttribute:forVirtualScreen:';
    function numberOfVirtualScreens: GLint; message 'numberOfVirtualScreens';
    function CGLPixelFormatObj: Pointer; message 'CGLPixelFormatObj';

    { Adopted Protocols }
    procedure encodeWithCoder(aCoder: NSCoder);
    function initWithCoder(aDecoder: NSCoder): id;
  end; external;

{ NSOpenGLPixelBuffer }
  NSOpenGLPixelBuffer = objcclass(NSObject)
  private
    __pixelBufferAuxiliary: Pointer;
    _reserved1: Pointer;
    _reserved2: Pointer;
    
  public
    class function alloc: NSOpenGLPixelBuffer; message 'alloc';

    function initWithTextureTarget_textureInternalFormat_textureMaxMipMapLevel_pixelsWide_pixelsHigh(target: GLenum; format: GLenum; maxLevel: GLint; pixelsWide_: GLsizei; pixelsHigh_: GLsizei): id; message 'initWithTextureTarget:textureInternalFormat:textureMaxMipMapLevel:pixelsWide:pixelsHigh:';
    function initWithCGLPBufferObj(pbuffer: Pointer): id; message 'initWithCGLPBufferObj:';
    function CGLPBufferObj: Pointer; message 'CGLPBufferObj';
    function pixelsWide: GLsizei; message 'pixelsWide';
    function pixelsHigh: GLsizei; message 'pixelsHigh';
    function textureTarget: GLenum; message 'textureTarget';
    function textureInternalFormat: GLenum; message 'textureInternalFormat';
    function textureMaxMipMapLevel: GLint; message 'textureMaxMipMapLevel';
  end; external;

{ NSOpenGLContext }
  NSOpenGLContext = objcclass(NSObject)
  private
    _view: NSView;
    _contextAuxiliary: NSOpenGLContextAuxiliaryPtr;
    
  public
    class function alloc: NSOpenGLContext; message 'alloc';

    function initWithFormat_shareContext(format: NSOpenGLPixelFormat; share: NSOpenGLContext): id; message 'initWithFormat:shareContext:';
    function initWithCGLContextObj(context: Pointer): id; message 'initWithCGLContextObj:';
    procedure setView(view_: NSView); message 'setView:';
    function view: NSView; message 'view';
    procedure setFullScreen; message 'setFullScreen';
    procedure setOffScreen_width_height_rowbytes(baseaddr: Pointer; width: GLsizei; height: GLsizei; rowbytes: GLint); message 'setOffScreen:width:height:rowbytes:';
    procedure clearDrawable; message 'clearDrawable';
    procedure update; message 'update';
    procedure flushBuffer; message 'flushBuffer';
    procedure makeCurrentContext; message 'makeCurrentContext';
    class procedure clearCurrentContext; message 'clearCurrentContext';
    class function currentContext: NSOpenGLContext; message 'currentContext';
    procedure copyAttributesFromContext_withMask(context: NSOpenGLContext; mask: GLbitfield); message 'copyAttributesFromContext:withMask:';
    procedure setValues_forParameter(vals: GLintPtr; param: NSOpenGLContextParameter); message 'setValues:forParameter:';
    procedure getValues_forParameter(vals: GLintPtr; param: NSOpenGLContextParameter); message 'getValues:forParameter:';
    procedure setCurrentVirtualScreen(screen: GLint); message 'setCurrentVirtualScreen:';
    function currentVirtualScreen: GLint; message 'currentVirtualScreen';
    procedure createTexture_fromView_internalFormat(target: GLenum; view_: NSView; format: GLenum); message 'createTexture:fromView:internalFormat:';
    function CGLContextObj: Pointer; message 'CGLContextObj';
    procedure setPixelBuffer_cubeMapFace_mipMapLevel_currentVirtualScreen(pixelBuffer_: NSOpenGLPixelBuffer; face: GLenum; level: GLint; screen: GLint); message 'setPixelBuffer:cubeMapFace:mipMapLevel:currentVirtualScreen:';
    function pixelBuffer: NSOpenGLPixelBuffer; message 'pixelBuffer';
    function pixelBufferCubeMapFace: GLenum; message 'pixelBufferCubeMapFace';
    function pixelBufferMipMapLevel: GLint; message 'pixelBufferMipMapLevel';
    procedure setTextureImageToPixelBuffer_colorBuffer(pixelBuffer_: NSOpenGLPixelBuffer; source: GLenum); message 'setTextureImageToPixelBuffer:colorBuffer:';
  end; external;

{$endif}
{$endif}
