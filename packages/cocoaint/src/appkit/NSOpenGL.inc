{ Parsed from Appkit.framework NSOpenGL.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Tue Sep 8 15:31:02 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSOPENGL_PAS_H}
{$define NSOPENGL_PAS_H}
type
  NSOpenGLPixelFormatPointer = Pointer;
  NSOpenGLPixelBufferPointer = Pointer;
  NSOpenGLContextPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSOPENGL_PAS_T}
{$define NSOPENGL_PAS_T}

{ Defines }
const
  NSOPENGL_CURRENT_VERSION = 1;

{ Sets }

type
  NSOpenGLGlobalOption = (NSOpenGLGOFormatCacheSize = 501, NSOpenGLGOClearFormatCache = 502, NSOpenGLGORetainRenderers = 503, NSOpenGLGOResetLibrary = 504);

type
  NSOpenGLContextParameter = (NSOpenGLCPSwapRectangle = 200, NSOpenGLCPSwapRectangleEnable = 201, NSOpenGLCPRasterizationEnable = 221, NSOpenGLCPSwapInterval = 222, NSOpenGLCPSurfaceOrder = 235, NSOpenGLCPSurfaceOpacity = 236, NSOpenGLCPStateValidation = 301);

{ Constants }

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
  NSOpenGLPFAAllowOfflineRenderers = 96;
  NSOpenGLPFAVirtualScreenCount = 128;

{ Types }
type
  NSOpenGLPixelFormatAttribute = cardinal;
  _CGLPixelFormatObject = Pointer;
  NSOpenGLPixelFormatAuxiliary = _CGLPixelFormatObject;
  _CGLContextObject = Pointer;
  NSOpenGLContextAuxiliary = _CGLContextObject;

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

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSOPENGL_PAS_T}
{$define NSOPENGL_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSOpenGLPixelFormat = objcclass;
  NSOpenGLPixelBuffer = objcclass;
  NSOpenGLContext = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSOPENGL_PAS_C}
{$define NSOPENGL_PAS_C}

{ NSOpenGLPixelFormat }
  NSOpenGLPixelFormat = objcclass(NSObject, NSCodingProtocol)
  private
    __pixelFormatAuxiliary: NSOpenGLPixelFormatAuxiliary;
    __pixelAttributes: NSData;
    __reserved1: clong;
    __reserved2: clong;
    __reserved3: clong;
    
  public
    class function alloc: NSOpenGLPixelFormat; message 'alloc';

    function initWithAttributes(var attribs: NSOpenGLPixelFormatAttribute): id; message 'initWithAttributes:';
    function initWithData(attribs: NSData): id; message 'initWithData:';
    function attributes: NSData; message 'attributes';
    procedure setAttributes(attribs: NSData); message 'setAttributes:';
    procedure getValues_forAttribute_forVirtualScreen(var vals: GLint; attrib: NSOpenGLPixelFormatAttribute; screen: GLint); message 'getValues:forAttribute:forVirtualScreen:';
    function numberOfVirtualScreens: GLint; message 'numberOfVirtualScreens';
    function CGLPixelFormatObj: Pointer; message 'CGLPixelFormatObj';
  end; external;

{ NSOpenGLPixelBuffer }
  NSOpenGLPixelBuffer = objcclass(NSObject)
  private
    __pixelBufferAuxiliary: _CGLPBufferObject;
    __reserved1: Pointer;
    __reserved2: Pointer;
    
  public
    class function alloc: NSOpenGLPixelBuffer; message 'alloc';

    function initWithTextureTarget_textureInternalFormat_textureMaxMipMapLevel_pixelsWide_pixelsHigh(target: GLenum; format: GLenum; maxLevel: GLint; pixelsWide_: GLsizei; pixelsHigh_: GLsizei): id; message 'initWithTextureTarget:textureInternalFormat:textureMaxMipMapLevel:pixelsWide:pixelsHigh:';
    function pixelsWide: GLsizei; message 'pixelsWide';
    function pixelsHigh: GLsizei; message 'pixelsHigh';
    function textureTarget: GLenum; message 'textureTarget';
    function textureInternalFormat: GLenum; message 'textureInternalFormat';
    function textureMaxMipMapLevel: GLint; message 'textureMaxMipMapLevel';
  end; external;

{ NSOpenGLContext }
  NSOpenGLContext = objcclass(NSObject)
  private
    __view: NSView;
    __contextAuxiliary: NSOpenGLContextAuxiliary;
    
  public
    class function alloc: NSOpenGLContext; message 'alloc';

    function initWithFormat_shareContext(format: NSOpenGLPixelFormat; share: NSOpenGLContext): id; message 'initWithFormat:shareContext:';
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
    procedure setValues_forParameter(var vals: GLint; param: NSOpenGLContextParameter); message 'setValues:forParameter:';
    procedure getValues_forParameter(var vals: GLint; param: NSOpenGLContextParameter); message 'getValues:forParameter:';
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
