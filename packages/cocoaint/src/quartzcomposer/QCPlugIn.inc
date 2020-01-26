{ Parsed from QuartzComposer.framework QCPlugIn.h }


{$ifdef TYPES}
type
  QCPlugInPtr = ^QCPlugIn;
  QCPlugInContextProtocolPtr = ^QCPlugInContextProtocol;
  QCPlugInInputImageSourceProtocolPtr = ^QCPlugInInputImageSourceProtocol;
  QCPlugInOutputImageProviderProtocolPtr = ^QCPlugInOutputImageProviderProtocol;
{$endif}

{$if (defined(MAC_OS_X_VERSION_10_5)) and ((MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_5))}
{$ifdef EXTERNAL_SYMBOLS}
var
  QCPlugInAttributeNameKey: NSString; cvar; external;
  QCPlugInAttributeDescriptionKey: NSString; cvar; external;
  QCPlugInAttributeCopyrightKey: NSString; cvar; external;
{$endif}

{$endif}

{$if (defined(MAC_OS_X_VERSION_10_7)) and ((MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_7))}
{$ifdef EXTERNAL_SYMBOLS}
var
  QCPlugInAttributeCategoriesKey: NSString; cvar; external;
  QCPlugInAttributeExamplesKey: NSString; cvar; external;
{$endif}

{$endif}
{$ifdef EXTERNAL_SYMBOLS}
var
  QCPortAttributeTypeKey: NSString; cvar; external;
  QCPortAttributeNameKey: NSString; cvar; external;
  QCPortAttributeMinimumValueKey: NSString; cvar; external;
  QCPortAttributeMaximumValueKey: NSString; cvar; external;
{$endif}

{$if (defined(MAC_OS_X_VERSION_10_5)) and ((MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_5))}
{$ifdef EXTERNAL_SYMBOLS}
var
  QCPortAttributeDefaultValueKey: NSString; cvar; external;
  QCPortAttributeMenuItemsKey: NSString; cvar; external;
{$endif}

{$endif}
{$ifdef EXTERNAL_SYMBOLS}
var
  QCPortTypeBoolean: NSString; cvar; external;
  QCPortTypeIndex: NSString; cvar; external;
  QCPortTypeNumber: NSString; cvar; external;
  QCPortTypeString: NSString; cvar; external;
  QCPortTypeColor: NSString; cvar; external;
  QCPortTypeImage: NSString; cvar; external;
  QCPortTypeStructure: NSString; cvar; external;
{$endif}

{$if (defined(MAC_OS_X_VERSION_10_5)) and ((MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_5))}
{$ifdef EXTERNAL_SYMBOLS}
var
  QCPlugInPixelFormatARGB8: NSString; cvar; external;
  QCPlugInPixelFormatBGRA8: NSString; cvar; external;
  QCPlugInPixelFormatRGBAf: NSString; cvar; external;
  QCPlugInPixelFormatI8: NSString; cvar; external;
  QCPlugInPixelFormatIf: NSString; cvar; external;
  QCPlugInExecutionArgumentEventKey: NSString; cvar; external;
  QCPlugInExecutionArgumentMouseLocationKey: NSString; cvar; external;
{$endif}

{$ifdef TYPES}
const
  kQCPlugInExecutionModeProvider = 1;
  kQCPlugInExecutionModeProcessor = 2;
  kQCPlugInExecutionModeConsumer = 3;
type
  QCPlugInExecutionMode = clong;
const
  kQCPlugInTimeModeNone = 0;
  kQCPlugInTimeModeIdle = 1;
  kQCPlugInTimeModeTimeBase = 2;
type
  QCPlugInTimeMode = clong;

type
  QCPlugInBufferReleaseCallback = procedure (address: pointer; context: pointer); cdecl;
  QCPlugInTextureReleaseCallback = procedure (cgl_ctx: CGLContextObj; name: GLuint; context: pointer); cdecl;
{$endif}

{$ifdef PROTOCOLS}

type
  QCPlugInContextProtocol = objcprotocol external name 'QCPlugInContext'
    function compositionURL: NSURL; message 'compositionURL';
    procedure logMessage (format: NSString); varargs; message 'logMessage:';
    function userInfo: NSMutableDictionary; message 'userInfo';
    function colorSpace: CGColorSpaceRef; message 'colorSpace';
    function bounds: NSRect; message 'bounds';
    function CGLContextObj: CGLContextObj; message 'CGLContextObj';
    function outputImageProviderFromBufferWithPixelFormat_pixelsWide_pixelsHigh_baseAddress_bytesPerRow_releaseCallback_releaseContext_colorSpace_shouldColorMatch (format: NSString; width: NSUInteger; height: NSUInteger; baseAddress: pointer; rowBytes: NSUInteger; callback: QCPlugInBufferReleaseCallback; context: pointer; colorSpace_: CGColorSpaceRef; colorMatch: ObjCBOOL): id; message 'outputImageProviderFromBufferWithPixelFormat:pixelsWide:pixelsHigh:baseAddress:bytesPerRow:releaseCallback:releaseContext:colorSpace:shouldColorMatch:';
    function outputImageProviderFromTextureWithPixelFormat_pixelsWide_pixelsHigh_name_flipped_releaseCallback_releaseContext_colorSpace_shouldColorMatch (format: NSString; width: NSUInteger; height: NSUInteger; name: GLuint; flipped: ObjCBOOL; callback: QCPlugInTextureReleaseCallback; context: pointer; colorSpace_: CGColorSpaceRef; colorMatch: ObjCBOOL): id; message 'outputImageProviderFromTextureWithPixelFormat:pixelsWide:pixelsHigh:name:flipped:releaseCallback:releaseContext:colorSpace:shouldColorMatch:';
  end;

type
  QCPlugInInputImageSourceProtocol = objcprotocol external name 'QCPlugInInputImageSource'
    function imageBounds: NSRect; message 'imageBounds';
    function imageColorSpace: CGColorSpaceRef; message 'imageColorSpace';
    function shouldColorMatch: ObjCBOOL; message 'shouldColorMatch';
    function lockBufferRepresentationWithPixelFormat_colorSpace_forBounds (format: NSString; colorSpace: CGColorSpaceRef; bounds: NSRect): ObjCBOOL; message 'lockBufferRepresentationWithPixelFormat:colorSpace:forBounds:';
    function bufferPixelsWide: NSUInteger; message 'bufferPixelsWide';
    function bufferPixelsHigh: NSUInteger; message 'bufferPixelsHigh';
    function bufferPixelFormat: NSString; message 'bufferPixelFormat';
    function bufferColorSpace: CGColorSpaceRef; message 'bufferColorSpace';
    function bufferBaseAddress: pointer; message 'bufferBaseAddress';
    function bufferBytesPerRow: NSUInteger; message 'bufferBytesPerRow';
    procedure unlockBufferRepresentation; message 'unlockBufferRepresentation';
    function lockTextureRepresentationWithColorSpace_forBounds (colorSpace: CGColorSpaceRef; bounds: NSRect): ObjCBOOL; message 'lockTextureRepresentationWithColorSpace:forBounds:';
    function texturePixelsWide: NSUInteger; message 'texturePixelsWide';
    function texturePixelsHigh: NSUInteger; message 'texturePixelsHigh';
    function textureTarget: GLenum; message 'textureTarget';
    function textureName: GLuint; message 'textureName';
    function textureColorSpace: CGColorSpaceRef; message 'textureColorSpace';
    function textureFlipped: ObjCBOOL; message 'textureFlipped';
    function textureMatrix: GLfloatPtr; message 'textureMatrix';
    procedure bindTextureRepresentationToCGLContext_textureUnit_normalizeCoordinates (cgl_ctx: CGLContextObj; unit_: GLenum; flag: ObjCBOOL); message 'bindTextureRepresentationToCGLContext:textureUnit:normalizeCoordinates:';
    procedure unbindTextureRepresentationFromCGLContext_textureUnit (cgl_ctx: CGLContextObj; unit_: GLenum); message 'unbindTextureRepresentationFromCGLContext:textureUnit:';
    procedure unlockTextureRepresentation; message 'unlockTextureRepresentation';
  end;

type
  QCPlugInOutputImageProviderProtocol = objcprotocol external name 'QCPlugInOutputImageProvider'
    function imageBounds: NSRect; message 'imageBounds';
    function imageColorSpace: CGColorSpaceRef; message 'imageColorSpace';
  optional
    function shouldColorMatch: ObjCBOOL; message 'shouldColorMatch';
    function supportedBufferPixelFormats: NSArray; message 'supportedBufferPixelFormats';
    function renderToBuffer_withBytesPerRow_pixelFormat_forBounds (baseAddress: pointer; rowBytes: NSUInteger; format: NSString; bounds: NSRect): ObjCBOOL; message 'renderToBuffer:withBytesPerRow:pixelFormat:forBounds:';
    function supportedRenderedTexturePixelFormats: NSArray; message 'supportedRenderedTexturePixelFormats';
    function copyRenderedTextureForCGLContext_pixelFormat_bounds_isFlipped (cgl_ctx: CGLContextObj; format: NSString; bounds: NSRect; flipped: pObjCBOOL): GLuint; message 'copyRenderedTextureForCGLContext:pixelFormat:bounds:isFlipped:';
    procedure releaseRenderedTexture_forCGLContext (name: GLuint; cgl_ctx: CGLContextObj); message 'releaseRenderedTexture:forCGLContext:';
    function canRenderWithCGLContext (cgl_ctx: CGLContextObj): ObjCBOOL; message 'canRenderWithCGLContext:';
    function renderWithCGLContext_forBounds (cgl_ctx: CGLContextObj; bounds: NSRect): ObjCBOOL; message 'renderWithCGLContext:forBounds:';
  end;
{$endif}

{$ifdef CLASSES}

type
  QCPlugIn = objcclass external (NSObject)
  private
    _private: pointer;
  public
    class function attributes: NSDictionary; message 'attributes';
    class function attributesForPropertyPortWithKey (key: NSString): NSDictionary; message 'attributesForPropertyPortWithKey:';
    class function sortedPropertyPortKeys: NSArray; message 'sortedPropertyPortKeys';
    class function executionMode: QCPlugInExecutionMode; message 'executionMode';
    class function timeMode: QCPlugInTimeMode; message 'timeMode';
    class function plugInKeys: NSArray; message 'plugInKeys';
    function startExecution (context: QCPlugInContextProtocol): ObjCBOOL; message 'startExecution:';
    procedure enableExecution (context: QCPlugInContextProtocol); message 'enableExecution:';
    function executionTimeForContext_atTime_withArguments (context: QCPlugInContextProtocol; time: NSTimeInterval; arguments: NSDictionary): NSTimeInterval; message 'executionTimeForContext:atTime:withArguments:';
    function execute_atTime_withArguments (context: QCPlugInContextProtocol; time: NSTimeInterval; arguments: NSDictionary): ObjCBOOL; message 'execute:atTime:withArguments:';
    procedure disableExecution (context: QCPlugInContextProtocol); message 'disableExecution:';
    procedure stopExecution (context: QCPlugInContextProtocol); message 'stopExecution:';
    function serializedValueForKey (key: NSString): id; message 'serializedValueForKey:';
    procedure setSerializedValue_forKey (serializedValue: id; key: NSString); message 'setSerializedValue:forKey:';
  end;


type
  Ports = objccategory external (QCPlugIn)
    function didValueForInputKeyChange (key: NSString): ObjCBOOL; message 'didValueForInputKeyChange:';
    function valueForInputKey (key: NSString): id; message 'valueForInputKey:';
    function setValue_forOutputKey (value: id; key: NSString): ObjCBOOL; message 'setValue:forOutputKey:';
    procedure addInputPortWithType_forKey_withAttributes (type_: NSString; key: NSString; attributes_: NSDictionary); message 'addInputPortWithType:forKey:withAttributes:';
    procedure removeInputPortForKey (key: NSString); message 'removeInputPortForKey:';
    procedure addOutputPortWithType_forKey_withAttributes (type_: NSString; key: NSString; attributes_: NSDictionary); message 'addOutputPortWithType:forKey:withAttributes:';
    procedure removeOutputPortForKey (key: NSString); message 'removeOutputPortForKey:';
  end;

type
  Registry = objccategory external (QCPlugIn)
    class function loadPlugInAtPath (path: NSString): ObjCBOOL; message 'loadPlugInAtPath:';
    class procedure registerPlugInClass (aClass: pobjc_class); message 'registerPlugInClass:';
  end;
{$endif}

{$endif}
