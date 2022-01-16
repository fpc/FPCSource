{ Parsed from QuartzComposer.framework QCRenderer.h }


{ Types from QCRenderer }
{$ifdef TYPES}


{$endif}


{ Types from QCCompositionRendererProtocol }
{$ifdef TYPES}

{$endif}


{$ifdef TYPES}
type
  QCRendererPtr = ^QCRenderer;
  QCCompositionRendererProtocolPtr = ^QCCompositionRendererProtocol;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  QCRendererEventKey: NSString; cvar; external;
  QCRendererMouseLocationKey: NSString; cvar; external;
{$endif}

{$ifdef PROTOCOLS}

type
  QCCompositionRendererProtocol = objcprotocol external name 'QCCompositionRenderer'
    function attributes: NSDictionary; message 'attributes';
    function inputKeys: NSArray; message 'inputKeys';
    function outputKeys: NSArray; message 'outputKeys';
    function setValue_forInputKey (value: id; key: NSString): ObjCBOOL; message 'setValue:forInputKey:';
    function valueForInputKey (key: NSString): id; message 'valueForInputKey:';
    function valueForOutputKey (key: NSString): id; message 'valueForOutputKey:';
    {$if (defined(MAC_OS_X_VERSION_10_5)) and ((MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_5))}
    function valueForOutputKey_ofType (key: NSString; type_: NSString): id; message 'valueForOutputKey:ofType:';
    function propertyListFromInputValues: id; message 'propertyListFromInputValues';
    procedure setInputValuesWithPropertyList (plist: id); message 'setInputValuesWithPropertyList:';
    function userInfo: NSMutableDictionary; message 'userInfo';
    {$endif}
  end;
{$endif}

{$ifdef CLASSES}

type
  QCRenderer = objcclass external (NSObject, QCCompositionRendererProtocol)
  private
    _QCRendererPrivate: pointer;
  public
    {$if (defined(MAC_OS_X_VERSION_10_5)) and ((MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_5))}
    function initWithComposition_colorSpace (composition: QCComposition; colorSpace: CGColorSpaceRef): id; message 'initWithComposition:colorSpace:';
    function initWithCGLContext_pixelFormat_colorSpace_composition (context: CGLContextObj; format: CGLPixelFormatObj; colorSpace: CGColorSpaceRef; composition: QCComposition): id; message 'initWithCGLContext:pixelFormat:colorSpace:composition:';
    function initOffScreenWithSize_colorSpace_composition (size: NSSize; colorSpace: CGColorSpaceRef; composition: QCComposition): id; message 'initOffScreenWithSize:colorSpace:composition:';
    {$endif}
    function initWithOpenGLContext_pixelFormat_file (context: NSOpenGLContext; format: NSOpenGLPixelFormat; path: NSString): id; message 'initWithOpenGLContext:pixelFormat:file:';
    function renderAtTime_arguments (time: NSTimeInterval; arguments: NSDictionary): ObjCBOOL; message 'renderAtTime:arguments:';
    function renderingTimeForTime_arguments (time: NSTimeInterval; arguments: NSDictionary): NSTimeInterval; message 'renderingTimeForTime:arguments:';
    {$if (defined(MAC_OS_X_VERSION_10_5)) and ((MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_5))}
    function composition: QCComposition; message 'composition';
    function snapshotImage: NSImage; message 'snapshotImage';
    function createSnapshotImageOfType (type_: NSString): id; message 'createSnapshotImageOfType:';
    {$endif}

    { Adopted protocols }
    function attributes: NSDictionary; message 'attributes';
    function inputKeys: NSArray; message 'inputKeys';
    function outputKeys: NSArray; message 'outputKeys';
    function propertyListFromInputValues: id; message 'propertyListFromInputValues';
    procedure setInputValuesWithPropertyList (plist: id); message 'setInputValuesWithPropertyList:';
    function setValue_forInputKey (value: id; key: NSString): ObjCBOOL; message 'setValue:forInputKey:';
    function userInfo: NSMutableDictionary; message 'userInfo';
    function valueForInputKey (key: NSString): id; message 'valueForInputKey:';
    function valueForOutputKey (key: NSString): id; message 'valueForOutputKey:';
    function valueForOutputKey_ofType (key: NSString; type_: NSString): id; message 'valueForOutputKey:ofType:';
  end;
{$endif}

