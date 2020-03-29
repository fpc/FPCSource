{ Parsed from AppKit.framework NSOpenGL.h }


{$ifdef TYPES}
type
  NSOpenGLPixelFormatPtr = ^NSOpenGLPixelFormat;
  NSOpenGLPixelBufferPtr = ^NSOpenGLPixelBuffer;
  NSOpenGLContextPtr = ^NSOpenGLContext;
{$endif}

{$ifdef TYPES}
const
  NSOPENGL_CURRENT_VERSION = 1;

const
  NSOpenGLGOFormatCacheSize = 501;
  NSOpenGLGOClearFormatCache = 502;
  NSOpenGLGORetainRenderers = 503;
  NSOpenGLGOUseBuildCache = 506;
  NSOpenGLGOResetLibrary = 504 deprecated 'in 10_0, 10_4';
type
  NSOpenGLGlobalOption = clong;
{$endif}

{$ifdef FUNCTIONS}
procedure NSOpenGLSetOption(pname: NSOpenGLGlobalOption; param: GLint); cdecl; external;
procedure NSOpenGLGetOption(pname: NSOpenGLGlobalOption; param: GLintPtr); cdecl; external;
procedure NSOpenGLGetVersion(major: GLintPtr; minor: GLintPtr); cdecl; external;
{$endif}

{$ifdef TYPES}
const
  NSOpenGLPFAAllRenderers = 1;
  NSOpenGLPFATripleBuffer = 3;
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
  NSOpenGLPFASampleBuffers = 55;
  NSOpenGLPFASamples = 56;
  NSOpenGLPFAAuxDepthStencil = 57;
  NSOpenGLPFAColorFloat = 58;
  NSOpenGLPFAMultisample = 59;
  NSOpenGLPFASupersample = 60;
  NSOpenGLPFASampleAlpha = 61;
  NSOpenGLPFARendererID = 70;
  NSOpenGLPFANoRecovery = 72;
  NSOpenGLPFAAccelerated = 73;
  NSOpenGLPFAClosestPolicy = 74;
  NSOpenGLPFABackingStore = 76;
  NSOpenGLPFAScreenMask = 84;
  NSOpenGLPFAAllowOfflineRenderers = 96 { available in 10_5 };
  NSOpenGLPFAAcceleratedCompute = 97;
  NSOpenGLPFAVirtualScreenCount = 128;
  NSOpenGLPFAOpenGLProfile = 99 { available in 10_7 };
  NSOpenGLPFAOffScreen = 53 deprecated 'in 10_0, 10_7';
  NSOpenGLPFAFullScreen = 54 deprecated 'in 10_0, 10_6';
  NSOpenGLPFASingleRenderer = 71 deprecated 'in 10_0, 10_9';
  NSOpenGLPFAWindow = 80 deprecated 'in 10_0, 10_9';
  NSOpenGLPFACompliant = 83 deprecated 'in 10_0, 10_9';
  NSOpenGLPFAPixelBuffer = 90 deprecated 'in 10_3, 10_7';
  NSOpenGLPFARemotePixelBuffer = 91 deprecated 'in 10_3, 10_7';
  NSOpenGLPFARobust = 75 deprecated 'in 10_0, 10_5';
  NSOpenGLPFAMPSafe = 78 deprecated 'in 10_0, 10_5';
  NSOpenGLPFAMultiScreen = 81 deprecated 'in 10_0, 10_5';

type
  NSOpenGLPixelFormatAttribute = cuint32;
  NSOpenGLPixelFormatAttributePtr = ^NSOpenGLPixelFormatAttribute;

const
  NSOpenGLProfileVersionLegacy = $1000 { available in 10_7 };
  NSOpenGLProfileVersion3_2Core = $3200 { available in 10_7 };
  NSOpenGLProfileVersion4_1Core = $4100 { available in 10_10 };
{$endif}

{$ifdef CLASSES}

type
  NSOpenGLPixelFormat = objcclass external (NSObject, NSCodingProtocol)
  private
    _CGLPixelFormat: _CGLPixelFormatObjectPtr;
    _reserved1: pointer;
    _reserved2: pointer;
    _reserved3: pointer;
    _reserved4: pointer;
  public
    function initWithAttributes (attribs: NSOpenGLPixelFormatAttributePtr): instancetype; message 'initWithAttributes:';
    function initWithData (attribs: NSData): id; message 'initWithData:'; deprecated 'in 10_0, 10_6, "Use -initWithAttributes: instead"';
    function initWithCGLPixelFormatObj (format: _CGLPixelFormatObjectPtr): NSOpenGLPixelFormat; message 'initWithCGLPixelFormatObj:'; { available in 10_6 }
    function attributes: NSData; message 'attributes'; deprecated 'in 10_0, 10_6';
    procedure setAttributes (attribs: NSData); message 'setAttributes:'; deprecated 'in 10_0, 10_6';
    procedure getValues_forAttribute_forVirtualScreen (vals: GLintPtr; attrib: NSOpenGLPixelFormatAttribute; screen: GLint); message 'getValues:forAttribute:forVirtualScreen:';
    function numberOfVirtualScreens: GLint; message 'numberOfVirtualScreens';
    function CGLPixelFormatObj: _CGLPixelFormatObjectPtr; message 'CGLPixelFormatObj';

    { Adopted protocols }
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
  end;

type
  NSOpenGLPixelBuffer = objcclass external (NSObject)
  private
    _CGLPBuffer: _CGLPBufferObjectPtr;
    _reserved1: pointer;
    _reserved2: pointer;
  public
    function initWithTextureTarget_textureInternalFormat_textureMaxMipMapLevel_pixelsWide_pixelsHigh (target: GLenum; format: GLenum; maxLevel: GLint; pixelsWide: GLsizei; pixelsHigh: GLsizei): instancetype; message 'initWithTextureTarget:textureInternalFormat:textureMaxMipMapLevel:pixelsWide:pixelsHigh:'; deprecated 'in 10_2, 10_7';
    function initWithCGLPBufferObj (pbuffer: _CGLPBufferObjectPtr): NSOpenGLPixelBuffer; message 'initWithCGLPBufferObj:'; deprecated 'in 10_6, 10_7';
    function CGLPBufferObj: _CGLPBufferObjectPtr; message 'CGLPBufferObj'; { available in 10_6 }
    function pixelsWide: GLsizei; message 'pixelsWide'; deprecated 'in 10_2, 10_7';
    function pixelsHigh: GLsizei; message 'pixelsHigh'; deprecated 'in 10_2, 10_7';
    function textureTarget: GLenum; message 'textureTarget'; deprecated 'in 10_2, 10_7';
    function textureInternalFormat: GLenum; message 'textureInternalFormat'; deprecated 'in 10_2, 10_7';
    function textureMaxMipMapLevel: GLint; message 'textureMaxMipMapLevel'; deprecated 'in 10_2, 10_7';
  end;
{$endif}

{$ifdef TYPES}
type
  NSOpenGLContextParameter = NSInteger;
  NSOpenGLContextParameterPtr = ^NSOpenGLContextParameter;

const
  NSOpenGLCPSwapInterval = 222;
  NSOpenGLCPSurfaceOrder = 235;
  NSOpenGLCPSurfaceOpacity = 236;
  NSOpenGLCPSurfaceBackingSize = 304;
  NSOpenGLCPReclaimResources = 308;
  NSOpenGLCPCurrentRendererID = 309;
  NSOpenGLCPGPUVertexProcessing = 310;
  NSOpenGLCPGPUFragmentProcessing = 311;
  NSOpenGLCPHasDrawable = 314;
  NSOpenGLCPMPSwapsInFlight = 315;
  NSOpenGLCPSwapRectangle = 200;
  NSOpenGLCPSwapRectangleEnable = 201;
  NSOpenGLCPRasterizationEnable = 221;
  NSOpenGLCPStateValidation = 301;
  NSOpenGLCPSurfaceSurfaceVolatile = 306;

type
  NSOpenGLContextAuxiliary = OpaqueType;
  NSOpenGLContextAuxiliaryPtr = ^NSOpenGLContextAuxiliary;
{$endif}

{$ifdef CLASSES}

type
  NSOpenGLContext = objcclass external (NSObject, NSLockingProtocol)
  private
    _view: NSView;
    _CGLContext: _CGLContextObjectPtr;
  public
    function initWithFormat_shareContext (format: NSOpenGLPixelFormat; share: NSOpenGLContext): instancetype; message 'initWithFormat:shareContext:';
    function initWithCGLContextObj (context: _CGLContextObjectPtr): NSOpenGLContext; message 'initWithCGLContextObj:'; { available in 10_6 }
    function pixelFormat: NSOpenGLPixelFormat; message 'pixelFormat';
    procedure setView(newValue: NSView); message 'setView:';
    function view: NSView; message 'view';
    procedure setFullScreen; message 'setFullScreen'; deprecated 'in 10_0, 10_7, "Use a fullscreen NSOpenGLView instead"';
    procedure setOffScreen_width_height_rowbytes (baseaddr: pointer; width: GLsizei; height: GLsizei; rowbytes: GLint); message 'setOffScreen:width:height:rowbytes:'; deprecated 'in 10_0, 10_7';
    procedure clearDrawable; message 'clearDrawable';
    procedure update; message 'update';
    procedure flushBuffer; message 'flushBuffer';
    procedure makeCurrentContext; message 'makeCurrentContext';
    class procedure clearCurrentContext; message 'clearCurrentContext';
    class function currentContext: NSOpenGLContext; message 'currentContext';
    procedure copyAttributesFromContext_withMask (context: NSOpenGLContext; mask: GLbitfield); message 'copyAttributesFromContext:withMask:'; deprecated 'in 10_0, 10_8';
    procedure setValues_forParameter (vals: GLintPtr; param: NSOpenGLContextParameter); message 'setValues:forParameter:';
    procedure getValues_forParameter (vals: GLintPtr; param: NSOpenGLContextParameter); message 'getValues:forParameter:';
    procedure setCurrentVirtualScreen(newValue: GLint); message 'setCurrentVirtualScreen:';
    function currentVirtualScreen: GLint; message 'currentVirtualScreen';
    procedure createTexture_fromView_internalFormat (target: GLenum; view_: NSView; format: GLenum); message 'createTexture:fromView:internalFormat:'; deprecated 'in 10_2, 10_8';
    function CGLContextObj: _CGLContextObjectPtr; message 'CGLContextObj';

    { Adopted protocols }
    procedure lock; message 'lock';
    procedure unlock; message 'unlock';
  end;


type
  NSOpenGLPixelBufer = objccategory external (NSOpenGLContext)
    procedure setPixelBuffer_cubeMapFace_mipMapLevel_currentVirtualScreen (pixelBuffer: NSOpenGLPixelBuffer; face: GLenum; level: GLint; screen: GLint); message 'setPixelBuffer:cubeMapFace:mipMapLevel:currentVirtualScreen:'; deprecated 'in 10_3, 10_7';
    function pixelBuffer: NSOpenGLPixelBuffer; message 'pixelBuffer'; deprecated 'in 10_3, 10_7';
    function pixelBufferCubeMapFace: GLenum; message 'pixelBufferCubeMapFace'; deprecated 'in 10_3, 10_7';
    function pixelBufferMipMapLevel: GLint; message 'pixelBufferMipMapLevel'; deprecated 'in 10_3, 10_7';
    procedure setTextureImageToPixelBuffer_colorBuffer (pixelBuffer_: NSOpenGLPixelBuffer; source: GLenum); message 'setTextureImageToPixelBuffer:colorBuffer:'; deprecated 'in 10_3, 10_7';
  end;
{$endif}

