{ Parsed from ImageKit.framework IKImageBrowserCell.h }


{$ifdef TYPES}
type
  IKImageBrowserCellPtr = ^IKImageBrowserCell;
{$endif}

{$if (MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_6)}
{$ifdef TYPES}
const
  IKImageStateNoImage = 0;
  IKImageStateInvalid = 1;
  IKImageStateReady = 2;
type
  IKImageBrowserCellState = clong;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  IKImageBrowserCellBackgroundLayer: NSString; cvar; external;
  IKImageBrowserCellForegroundLayer: NSString; cvar; external;
  IKImageBrowserCellSelectionLayer: NSString; cvar; external;
  IKImageBrowserCellPlaceHolderLayer: NSString; cvar; external;
{$endif}

{$ifdef CLASSES}

type
  IKImageBrowserCell = objcclass external (NSObject)
  private
    _parent: id;
    _ibCellReserved: pointer;
  public
    function imageBrowserView: IKImageBrowserView; message 'imageBrowserView';
    function representedItem: id; message 'representedItem';
    function indexOfRepresentedItem: NSUInteger; message 'indexOfRepresentedItem';
    function frame: NSRect; message 'frame';
    function imageContainerFrame: NSRect; message 'imageContainerFrame';
    function imageFrame: NSRect; message 'imageFrame';
    function selectionFrame: NSRect; message 'selectionFrame';
    function titleFrame: NSRect; message 'titleFrame';
    function subtitleFrame: NSRect; message 'subtitleFrame';
    function imageAlignment: NSImageAlignment; message 'imageAlignment';
    function isSelected: ObjCBOOL; message 'isSelected';
    function cellState: IKImageBrowserCellState; message 'cellState';
    function opacity: CGFloat; message 'opacity';
    function layerForType (type_: NSString): CALayer; message 'layerForType:';
  end;
{$endif}

{$endif}
