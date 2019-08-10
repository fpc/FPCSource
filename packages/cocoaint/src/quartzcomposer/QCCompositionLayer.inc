{ Parsed from QuartzComposer.framework QCCompositionLayer.h }


{$ifdef TYPES}
type
  QCCompositionLayerPtr = ^QCCompositionLayer;
{$endif}

{$if (defined(MAC_OS_X_VERSION_10_5)) and ((MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_5))}
{$ifdef CLASSES}

type
  QCCompositionLayer = objcclass external (CAOpenGLLayer, QCCompositionRendererProtocol)
  private
    _QCCompositionLayerPrivate: pointer;
  public
    class function compositionLayerWithFile (path: NSString): QCCompositionLayer; message 'compositionLayerWithFile:';
    class function compositionLayerWithComposition (composition: QCComposition): QCCompositionLayer; message 'compositionLayerWithComposition:';
    function initWithFile (path: NSString): id; message 'initWithFile:';
    function initWithComposition (composition: QCComposition): id; message 'initWithComposition:';
    function composition: QCComposition; message 'composition';

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

{$endif}
