{ Parsed from GLKit.framework GLKTextureLoader.h }


{ Types from GLKTextureLoader }
{$ifdef TYPES}

{$endif}


{$ifdef TYPES}
type
  GLKTextureInfoPtr = ^GLKTextureInfo;
  GLKTextureLoaderPtr = ^GLKTextureLoader;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  GLKTextureLoaderApplyPremultiplication: NSString { available in 10_8, 5_0 }; cvar; external;
  GLKTextureLoaderGenerateMipmaps: NSString { available in 10_8, 5_0 }; cvar; external;
  GLKTextureLoaderOriginBottomLeft: NSString { available in 10_8, 5_0 }; cvar; external;
  GLKTextureLoaderGrayscaleAsAlpha: NSString { available in 5_0 }; cvar; external;
{$endif}

{$if defined(TARGET_OS_IPHONE) or (defined(MAC_OS_X_VERSION_10_9))}
{$ifdef EXTERNAL_SYMBOLS}
var
  GLKTextureLoaderSRGB: NSString { available in 10_9, 7_0 }; cvar; external;
{$endif}

{$else}
{$ifdef EXTERNAL_SYMBOLS}
var
  GLKTextureLoaderSRGB: NSString; cvar; external;
{$endif}

{$endif}
{$ifdef EXTERNAL_SYMBOLS}
var
  GLKTextureLoaderErrorDomain: NSString { available in 10_8, 5_0 }; cvar; external;
  GLKTextureLoaderErrorKey: NSString { available in 10_8, 5_0 }; cvar; external;
  GLKTextureLoaderGLErrorKey: NSString { available in 10_8, 5_0 }; cvar; external;
{$endif}

{$ifdef TYPES}
type
  GLKTextureLoaderError = GLuint;
  GLKTextureLoaderErrorPtr = ^GLKTextureLoaderError;

const
  GLKTextureLoaderErrorFileOrURLNotFound = 0;
  GLKTextureLoaderErrorInvalidNSData = 1;
  GLKTextureLoaderErrorInvalidCGImage = 2;
  GLKTextureLoaderErrorUnknownPathType = 3;
  GLKTextureLoaderErrorUnknownFileType = 4;
  GLKTextureLoaderErrorPVRAtlasUnsupported = 5;
  GLKTextureLoaderErrorCubeMapInvalidNumFiles = 6;
  GLKTextureLoaderErrorCompressedTextureUpload = 7;
  GLKTextureLoaderErrorUncompressedTextureUpload = 8;
  GLKTextureLoaderErrorUnsupportedCubeMapDimensions = 9;
  GLKTextureLoaderErrorUnsupportedBitDepth = 10;
  GLKTextureLoaderErrorUnsupportedPVRFormat = 11;
  GLKTextureLoaderErrorDataPreprocessingFailure = 12;
  GLKTextureLoaderErrorMipmapUnsupported = 13;
  GLKTextureLoaderErrorUnsupportedOrientation = 14;
  GLKTextureLoaderErrorReorientationFailure = 15;
  GLKTextureLoaderErrorAlphaPremultiplicationFailure = 16;
  GLKTextureLoaderErrorInvalidEAGLContext = 17;
  GLKTextureLoaderErrorIncompatibleFormatSRGB = 18;

type
  GLKTextureInfoAlphaState = GLint;
  GLKTextureInfoAlphaStatePtr = ^GLKTextureInfoAlphaState;

const
  GLKTextureInfoAlphaStateNone = 0;
  GLKTextureInfoAlphaStateNonPremultiplied = 1;
  GLKTextureInfoAlphaStatePremultiplied = 2;

type
  GLKTextureInfoOrigin = GLint;
  GLKTextureInfoOriginPtr = ^GLKTextureInfoOrigin;

const
  GLKTextureInfoOriginUnknown = 0;
  GLKTextureInfoOriginTopLeft = 1;
  GLKTextureInfoOriginBottomLeft = 2;
{$endif}

{$ifdef CLASSES}

type
  GLKTextureInfo = objcclass external (NSObject, NSCopyingProtocol)
  private
    name: GLuint;
    target: GLenum;
    width: GLuint;
    height: GLuint;
    alphaState: GLKTextureInfoAlphaState;
    textureOrigin: GLKTextureInfoOrigin;
    containsMipmaps: ObjCBOOL;
  public
    function name_: GLuint; message 'name';
    function target_: GLenum; message 'target';
    function width_: GLuint; message 'width';
    function height_: GLuint; message 'height';
    function alphaState_: GLKTextureInfoAlphaState; message 'alphaState';
    function textureOrigin_: GLKTextureInfoOrigin; message 'textureOrigin';
    function containsMipmaps_: ObjCBOOL; message 'containsMipmaps';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
  end;
{$endif}

{$ifdef TYPES}
type
  GLKTextureLoaderCallback = OpaqueCBlock;
{$endif}

{$ifdef CLASSES}

type
  GLKTextureLoader = objcclass external (NSObject)
  private
  public
    class function textureWithContentsOfFile_options_error (path: NSString; options: NSDictionary; outError: NSErrorPtr): GLKTextureInfo; message 'textureWithContentsOfFile:options:error:';
    class function textureWithContentsOfURL_options_error (url: NSURL; options: NSDictionary; outError: NSErrorPtr): GLKTextureInfo; message 'textureWithContentsOfURL:options:error:';
    class function textureWithContentsOfData_options_error (data: NSData; options: NSDictionary; outError: NSErrorPtr): GLKTextureInfo; message 'textureWithContentsOfData:options:error:';
    class function textureWithCGImage_options_error (cgImage: CGImageRef; options: NSDictionary; outError: NSErrorPtr): GLKTextureInfo; message 'textureWithCGImage:options:error:';
    class function cubeMapWithContentsOfFiles_options_error (paths: NSArray; options: NSDictionary; outError: NSErrorPtr): GLKTextureInfo; message 'cubeMapWithContentsOfFiles:options:error:';
    class function cubeMapWithContentsOfFile_options_error (path: NSString; options: NSDictionary; outError: NSErrorPtr): GLKTextureInfo; message 'cubeMapWithContentsOfFile:options:error:';
    class function cubeMapWithContentsOfURL_options_error (url: NSURL; options: NSDictionary; outError: NSErrorPtr): GLKTextureInfo; message 'cubeMapWithContentsOfURL:options:error:';
    {$if defined(TARGET_OS_IPHONE)}
    function initWithSharegroup (sharegroup: EAGLSharegroupPtr): id; message 'initWithSharegroup:';
    {$else}
    function initWithShareContext (context: NSOpenGLContext): id; message 'initWithShareContext:';
    {$endif}
    procedure textureWithContentsOfFile_options_queue_completionHandler (path: NSString; options: NSDictionary; queue: dispatch_queue_t; block: GLKTextureLoaderCallback); message 'textureWithContentsOfFile:options:queue:completionHandler:';
    procedure textureWithContentsOfURL_options_queue_completionHandler (url: NSURL; options: NSDictionary; queue: dispatch_queue_t; block: GLKTextureLoaderCallback); message 'textureWithContentsOfURL:options:queue:completionHandler:';
    procedure textureWithContentsOfData_options_queue_completionHandler (data: NSData; options: NSDictionary; queue: dispatch_queue_t; block: GLKTextureLoaderCallback); message 'textureWithContentsOfData:options:queue:completionHandler:';
    procedure textureWithCGImage_options_queue_completionHandler (cgImage: CGImageRef; options: NSDictionary; queue: dispatch_queue_t; block: GLKTextureLoaderCallback); message 'textureWithCGImage:options:queue:completionHandler:';
    procedure cubeMapWithContentsOfFiles_options_queue_completionHandler (paths: NSArray; options: NSDictionary; queue: dispatch_queue_t; block: GLKTextureLoaderCallback); message 'cubeMapWithContentsOfFiles:options:queue:completionHandler:';
    procedure cubeMapWithContentsOfFile_options_queue_completionHandler (path: NSString; options: NSDictionary; queue: dispatch_queue_t; block: GLKTextureLoaderCallback); message 'cubeMapWithContentsOfFile:options:queue:completionHandler:';
    procedure cubeMapWithContentsOfURL_options_queue_completionHandler (url: NSURL; options: NSDictionary; queue: dispatch_queue_t; block: GLKTextureLoaderCallback); message 'cubeMapWithContentsOfURL:options:queue:completionHandler:';
  end;
{$endif}

