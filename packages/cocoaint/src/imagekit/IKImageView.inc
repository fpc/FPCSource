{ Parsed from ImageKit.framework IKImageView.h }


{$ifdef TYPES}
type
  IKImageViewPtr = ^IKImageView;
{$endif}

{$if (MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_5)}
{$ifdef EXTERNAL_SYMBOLS}
var
  IKToolModeNone: NSString; cvar; external;
  IKToolModeMove: NSString; cvar; external;
  IKToolModeSelect: NSString; cvar; external;
  IKToolModeSelectRect: NSString; cvar; external;
  IKToolModeSelectEllipse: NSString; cvar; external;
  IKToolModeSelectLasso: NSString; cvar; external;
  IKToolModeCrop: NSString; cvar; external;
  IKToolModeRotate: NSString; cvar; external;
  IKToolModeAnnotate: NSString; cvar; external;
  IKOverlayTypeBackground: NSString; cvar; external;
  IKOverlayTypeImage: NSString; cvar; external;
{$endif}

{$ifdef CLASSES}

type
  IKImageView = objcclass external (NSView)
  private
    _privateData: pointer;
  public
    procedure setDelegate(newValue: id); message 'setDelegate:';
    function delegate: id; message 'delegate';
    procedure setZoomFactor(newValue: CGFloat); message 'setZoomFactor:';
    function zoomFactor: CGFloat; message 'zoomFactor';
    procedure setRotationAngle(newValue: CGFloat); message 'setRotationAngle:';
    function rotationAngle: CGFloat; message 'rotationAngle';
    procedure setCurrentToolMode(newValue: NSString); message 'setCurrentToolMode:';
    function currentToolMode: NSString; message 'currentToolMode';
    procedure setAutoresizes(newValue: ObjCBOOL); message 'setAutoresizes:';
    function autoresizes: ObjCBOOL; message 'autoresizes';
    procedure setHasHorizontalScroller(newValue: ObjCBOOL); message 'setHasHorizontalScroller:';
    function hasHorizontalScroller: ObjCBOOL; message 'hasHorizontalScroller';
    procedure setHasVerticalScroller(newValue: ObjCBOOL); message 'setHasVerticalScroller:';
    function hasVerticalScroller: ObjCBOOL; message 'hasVerticalScroller';
    procedure setAutohidesScrollers(newValue: ObjCBOOL); message 'setAutohidesScrollers:';
    function autohidesScrollers: ObjCBOOL; message 'autohidesScrollers';
    procedure setSupportsDragAndDrop(newValue: ObjCBOOL); message 'setSupportsDragAndDrop:';
    function supportsDragAndDrop: ObjCBOOL; message 'supportsDragAndDrop';
    procedure setEditable(newValue: ObjCBOOL); message 'setEditable:';
    function editable: ObjCBOOL; message 'editable';
    procedure setDoubleClickOpensImageEditPanel(newValue: ObjCBOOL); message 'setDoubleClickOpensImageEditPanel:';
    function doubleClickOpensImageEditPanel: ObjCBOOL; message 'doubleClickOpensImageEditPanel';
    procedure setImageCorrection(newValue: CIFilter); message 'setImageCorrection:';
    function imageCorrection: CIFilter; message 'imageCorrection';
    procedure setBackgroundColor(newValue: NSColor); message 'setBackgroundColor:';
    function backgroundColor: NSColor; message 'backgroundColor';
    procedure setImage_imageProperties (image: CGImageRef; metaData: NSDictionary); message 'setImage:imageProperties:';
    procedure setImageWithURL (url: NSURL); message 'setImageWithURL:';
    function image: CGImageRef; message 'image';
    function imageSize: NSSize; message 'imageSize';
    function imageProperties: NSDictionary; message 'imageProperties';
    procedure setRotationAngle_centerPoint (rotationAngle_: CGFloat; centerPoint: NSPoint); message 'setRotationAngle:centerPoint:';
    procedure rotateImageLeft (sender: id); message 'rotateImageLeft:';
    procedure rotateImageRight (sender: id); message 'rotateImageRight:';
    procedure setImageZoomFactor_centerPoint (zoomFactor_: CGFloat; centerPoint: NSPoint); message 'setImageZoomFactor:centerPoint:';
    procedure zoomImageToRect (rect: NSRect); message 'zoomImageToRect:';
    procedure zoomImageToFit (sender: id); message 'zoomImageToFit:';
    procedure zoomImageToActualSize (sender: id); message 'zoomImageToActualSize:';
    procedure zoomIn (sender: id); message 'zoomIn:';
    procedure zoomOut (sender: id); message 'zoomOut:';
    procedure flipImageHorizontal (sender: id); message 'flipImageHorizontal:';
    procedure flipImageVertical (sender: id); message 'flipImageVertical:';
    procedure crop (sender: id); message 'crop:';
    procedure setOverlay_forType (layer: CALayer; layerType: NSString); message 'setOverlay:forType:';
    function overlayForType (layerType: NSString): CALayer; message 'overlayForType:';
    procedure scrollToPoint (point: NSPoint); message 'scrollToPoint:';
    procedure scrollToRect (rect: NSRect); message 'scrollToRect:';
    function convertViewPointToImagePoint (viewPoint: NSPoint): NSPoint; message 'convertViewPointToImagePoint:';
    function convertViewRectToImageRect (viewRect: NSRect): NSRect; message 'convertViewRectToImageRect:';
    function convertImagePointToViewPoint (imagePoint: NSPoint): NSPoint; message 'convertImagePointToViewPoint:';
    function convertImageRectToViewRect (imageRect: NSRect): NSRect; message 'convertImageRectToViewRect:';
  end;
{$endif}

{$endif}
