{ Parsed from QuartzComposer.framework QCCompositionParameterView.h }


{$ifdef TYPES}
type
  QCCompositionParameterViewPtr = ^QCCompositionParameterView;
{$endif}

{$if (defined(MAC_OS_X_VERSION_10_5)) and ((MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_5))}
{$ifdef CLASSES}

type
  QCCompositionParameterView = objcclass external (NSView)
  private
    _QCCompositionParameterViewPrivate: pointer;
  public
    procedure setCompositionRenderer (renderer: QCCompositionRendererProtocol); message 'setCompositionRenderer:';
    function compositionRenderer: QCCompositionRendererProtocol; message 'compositionRenderer';
    function hasParameters: ObjCBOOL; message 'hasParameters';
    procedure setBackgroundColor (color: NSColor); message 'setBackgroundColor:';
    function backgroundColor: NSColor; message 'backgroundColor';
    procedure setDrawsBackground (flag: ObjCBOOL); message 'setDrawsBackground:';
    function drawsBackground: ObjCBOOL; message 'drawsBackground';
    procedure setDelegate (delegate: id); message 'setDelegate:';
    function delegate: id; message 'delegate';
  end;


type
  QCCompositionParameterViewDelegate = objccategory external (NSObject)
    function compositionParameterView_shouldDisplayParameterWithKey_attributes (parameterView: QCCompositionParameterView; portKey: NSString; portAttributes: NSDictionary): ObjCBOOL; message 'compositionParameterView:shouldDisplayParameterWithKey:attributes:';
    procedure compositionParameterView_didChangeParameterWithKey (parameterView: QCCompositionParameterView; portKey: NSString); message 'compositionParameterView:didChangeParameterWithKey:';
  end;
{$endif}

{$endif}
