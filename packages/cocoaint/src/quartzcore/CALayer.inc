{ Parsed from QuartzCore.framework CALayer.h }


{$ifdef TYPES}
type
  CALayerPtr = ^CALayer;
  CAActionProtocolPtr = ^CAActionProtocol;
{$endif}

{$ifdef TYPES}
type
  CAAutoresizingMask = cuint;
  CAAutoresizingMaskPtr = ^CAAutoresizingMask;

const
  kCALayerNotSizable = 0;
  kCALayerMinXMargin = 1 shl 0;
  kCALayerWidthSizable = 1 shl 1;
  kCALayerMaxXMargin = 1 shl 2;
  kCALayerMinYMargin = 1 shl 3;
  kCALayerHeightSizable = 1 shl 4;
  kCALayerMaxYMargin = 1 shl 5;

type
  CAEdgeAntialiasingMask = cuint;
  CAEdgeAntialiasingMaskPtr = ^CAEdgeAntialiasingMask;

const
  kCALayerLeftEdge = 1 shl 0;
  kCALayerRightEdge = 1 shl 1;
  kCALayerBottomEdge = 1 shl 2;
  kCALayerTopEdge = 1 shl 3;
{$endif}

{$ifdef CLASSES}

type
  CALayer = objcclass external (NSObject, NSCodingProtocol, CAMediaTimingProtocol)
  private
    _CALayerIvars: record
      refcount: cint32;
      magic: cuint32;
      layer_: pointer;
      {$if defined(TARGET_OS_MAC) and not defined(TARGET_RT_64_BIT)}
      unused1:array[0..7] of pointer;
      {$endif}
    end;
  public
    class function layer: instancetype; message 'layer';
    function init: instancetype; message 'init';
    function initWithLayer (layer_: id): instancetype; message 'initWithLayer:';
    function presentationLayer: id; message 'presentationLayer';
    function modelLayer: id; message 'modelLayer';
    class function defaultValueForKey (key: NSString): id; message 'defaultValueForKey:';
    class function needsDisplayForKey (key: NSString): ObjCBOOL; message 'needsDisplayForKey:';
    function shouldArchiveValueForKey (key: NSString): ObjCBOOL; message 'shouldArchiveValueForKey:';
    procedure setBounds(newValue: CGRect); message 'setBounds:';
    function bounds: CGRect; message 'bounds';
    procedure setPosition(newValue: CGPoint); message 'setPosition:';
    function position: CGPoint; message 'position';
    procedure setZPosition(newValue: CGFloat); message 'setZPosition:';
    function zPosition: CGFloat; message 'zPosition';
    procedure setAnchorPoint(newValue: CGPoint); message 'setAnchorPoint:';
    function anchorPoint: CGPoint; message 'anchorPoint';
    procedure setAnchorPointZ(newValue: CGFloat); message 'setAnchorPointZ:';
    function anchorPointZ: CGFloat; message 'anchorPointZ';
    procedure setTransform(newValue: CATransform3D); message 'setTransform:';
    function transform: CATransform3D; message 'transform';
    function affineTransform: CGAffineTransform; message 'affineTransform';
    procedure setAffineTransform (m: CGAffineTransform); message 'setAffineTransform:';
    procedure setFrame(newValue: CGRect); message 'setFrame:';
    function frame: CGRect; message 'frame';
    procedure setHidden(newValue: ObjCBOOL); message 'setHidden:';
    function isHidden: ObjCBOOL; message 'isHidden';
    procedure setDoubleSided(newValue: ObjCBOOL); message 'setDoubleSided:';
    function isDoubleSided: ObjCBOOL; message 'isDoubleSided';
    procedure setGeometryFlipped(newValue: ObjCBOOL); message 'setGeometryFlipped:';
    function isGeometryFlipped: ObjCBOOL; message 'isGeometryFlipped';
    function contentsAreFlipped: ObjCBOOL; message 'contentsAreFlipped';
    function superlayer: CALayer; message 'superlayer';
    procedure removeFromSuperlayer; message 'removeFromSuperlayer';
    procedure setSublayers(newValue: NSArray); message 'setSublayers:';
    function sublayers: NSArray; message 'sublayers';
    procedure addSublayer (layer_: CALayer); message 'addSublayer:';
    procedure insertSublayer_atIndex (layer_: CALayer; idx: cuint); message 'insertSublayer:atIndex:';
    procedure insertSublayer_below (layer_: CALayer; sibling: CALayer); message 'insertSublayer:below:';
    procedure insertSublayer_above (layer_: CALayer; sibling: CALayer); message 'insertSublayer:above:';
    procedure replaceSublayer_with (layer_: CALayer; layer2: CALayer); message 'replaceSublayer:with:';
    procedure setSublayerTransform(newValue: CATransform3D); message 'setSublayerTransform:';
    function sublayerTransform: CATransform3D; message 'sublayerTransform';
    procedure setMask(newValue: CALayer); message 'setMask:';
    function mask: CALayer; message 'mask';
    procedure setMasksToBounds(newValue: ObjCBOOL); message 'setMasksToBounds:';
    function masksToBounds: ObjCBOOL; message 'masksToBounds';
    function convertPoint_fromLayer (p: CGPoint; l: CALayer): CGPoint; message 'convertPoint:fromLayer:';
    function convertPoint_toLayer (p: CGPoint; l: CALayer): CGPoint; message 'convertPoint:toLayer:';
    function convertRect_fromLayer (r: CGRect; l: CALayer): CGRect; message 'convertRect:fromLayer:';
    function convertRect_toLayer (r: CGRect; l: CALayer): CGRect; message 'convertRect:toLayer:';
    function convertTime_fromLayer (t: CFTimeInterval; l: CALayer): CFTimeInterval; message 'convertTime:fromLayer:';
    function convertTime_toLayer (t: CFTimeInterval; l: CALayer): CFTimeInterval; message 'convertTime:toLayer:';
    function hitTest (p: CGPoint): CALayer; message 'hitTest:';
    function containsPoint (p: CGPoint): ObjCBOOL; message 'containsPoint:';
    procedure setContents(newValue: id); message 'setContents:';
    function contents: id; message 'contents';
    procedure setContentsRect(newValue: CGRect); message 'setContentsRect:';
    function contentsRect: CGRect; message 'contentsRect';
    procedure setContentsGravity(newValue: NSString); message 'setContentsGravity:';
    function contentsGravity: NSString; message 'contentsGravity';
    procedure setContentsScale(newValue: CGFloat); message 'setContentsScale:';
    function contentsScale: CGFloat; message 'contentsScale';
    procedure setContentsCenter(newValue: CGRect); message 'setContentsCenter:';
    function contentsCenter: CGRect; message 'contentsCenter';
    procedure setMinificationFilter(newValue: NSString); message 'setMinificationFilter:';
    function minificationFilter: NSString; message 'minificationFilter';
    procedure setMagnificationFilter(newValue: NSString); message 'setMagnificationFilter:';
    function magnificationFilter: NSString; message 'magnificationFilter';
    procedure setMinificationFilterBias(newValue: single); message 'setMinificationFilterBias:';
    function minificationFilterBias: single; message 'minificationFilterBias';
    procedure setOpaque(newValue: ObjCBOOL); message 'setOpaque:';
    function isOpaque: ObjCBOOL; message 'isOpaque';
    procedure display; message 'display';
    procedure setNeedsDisplay; message 'setNeedsDisplay';
    procedure setNeedsDisplayInRect (r: CGRect); message 'setNeedsDisplayInRect:';
    function needsDisplay: ObjCBOOL; message 'needsDisplay';
    procedure displayIfNeeded; message 'displayIfNeeded';
    procedure setNeedsDisplayOnBoundsChange(newValue: ObjCBOOL); message 'setNeedsDisplayOnBoundsChange:';
    function needsDisplayOnBoundsChange: ObjCBOOL; message 'needsDisplayOnBoundsChange';
    procedure setDrawsAsynchronously(newValue: ObjCBOOL); message 'setDrawsAsynchronously:';
    function drawsAsynchronously: ObjCBOOL; message 'drawsAsynchronously';
    procedure drawInContext (ctx: CGContextRef); message 'drawInContext:';
    procedure renderInContext (ctx: CGContextRef); message 'renderInContext:';
    procedure setEdgeAntialiasingMask(newValue: CAEdgeAntialiasingMask); message 'setEdgeAntialiasingMask:';
    function edgeAntialiasingMask: CAEdgeAntialiasingMask; message 'edgeAntialiasingMask';
    procedure setBackgroundColor(newValue: CGColorRef); message 'setBackgroundColor:';
    function backgroundColor: CGColorRef; message 'backgroundColor';
    procedure setCornerRadius(newValue: CGFloat); message 'setCornerRadius:';
    function cornerRadius: CGFloat; message 'cornerRadius';
    procedure setBorderWidth(newValue: CGFloat); message 'setBorderWidth:';
    function borderWidth: CGFloat; message 'borderWidth';
    procedure setBorderColor(newValue: CGColorRef); message 'setBorderColor:';
    function borderColor: CGColorRef; message 'borderColor';
    procedure setOpacity(newValue: single); message 'setOpacity:';
    function opacity: single; message 'opacity';
    procedure setCompositingFilter(newValue: id); message 'setCompositingFilter:';
    function compositingFilter: id; message 'compositingFilter';
    procedure setFilters(newValue: NSArray); message 'setFilters:';
    function filters: NSArray; message 'filters';
    procedure setBackgroundFilters(newValue: NSArray); message 'setBackgroundFilters:';
    function backgroundFilters: NSArray; message 'backgroundFilters';
    procedure setShouldRasterize(newValue: ObjCBOOL); message 'setShouldRasterize:';
    function shouldRasterize: ObjCBOOL; message 'shouldRasterize';
    procedure setRasterizationScale(newValue: CGFloat); message 'setRasterizationScale:';
    function rasterizationScale: CGFloat; message 'rasterizationScale';
    procedure setShadowColor(newValue: CGColorRef); message 'setShadowColor:';
    function shadowColor: CGColorRef; message 'shadowColor';
    procedure setShadowOpacity(newValue: single); message 'setShadowOpacity:';
    function shadowOpacity: single; message 'shadowOpacity';
    procedure setShadowOffset(newValue: CGSize); message 'setShadowOffset:';
    function shadowOffset: CGSize; message 'shadowOffset';
    procedure setShadowRadius(newValue: CGFloat); message 'setShadowRadius:';
    function shadowRadius: CGFloat; message 'shadowRadius';
    procedure setShadowPath(newValue: CGPathRef); message 'setShadowPath:';
    function shadowPath: CGPathRef; message 'shadowPath';
    procedure setAutoresizingMask(newValue: CAAutoresizingMask); message 'setAutoresizingMask:';
    function autoresizingMask: CAAutoresizingMask; message 'autoresizingMask';
    procedure setLayoutManager(newValue: id); message 'setLayoutManager:';
    function layoutManager: id; message 'layoutManager';
    function preferredFrameSize: CGSize; message 'preferredFrameSize';
    procedure setNeedsLayout; message 'setNeedsLayout';
    function needsLayout: ObjCBOOL; message 'needsLayout';
    procedure layoutIfNeeded; message 'layoutIfNeeded';
    procedure layoutSublayers; message 'layoutSublayers';
    procedure resizeSublayersWithOldSize (size: CGSize); message 'resizeSublayersWithOldSize:';
    procedure resizeWithOldSuperlayerSize (size: CGSize); message 'resizeWithOldSuperlayerSize:';
    class function defaultActionForKey (event: NSString): CAActionProtocol; message 'defaultActionForKey:';
    function actionForKey (event: NSString): CAActionProtocol; message 'actionForKey:';
    procedure setActions(newValue: NSDictionary); message 'setActions:';
    function actions: NSDictionary; message 'actions';
    procedure addAnimation_forKey (anim: CAAnimation; key: NSString); message 'addAnimation:forKey:';
    procedure removeAllAnimations; message 'removeAllAnimations';
    procedure removeAnimationForKey (key: NSString); message 'removeAnimationForKey:';
    function animationKeys: NSArray; message 'animationKeys';
    function animationForKey (key: NSString): CAAnimation; message 'animationForKey:';
    procedure setName(newValue: NSString); message 'setName:';
    function name: NSString; message 'name';
    procedure setDelegate(newValue: id); message 'setDelegate:';
    function delegate: id; message 'delegate';
    procedure setStyle(newValue: NSDictionary); message 'setStyle:';
    function style: NSDictionary; message 'style';

    { Adopted protocols }
    procedure setAutoreverses(newValue: ObjCBOOL); message 'setAutoreverses:';
    function autoreverses: ObjCBOOL; message 'autoreverses';
    procedure setBeginTime(newValue: CFTimeInterval); message 'setBeginTime:';
    function beginTime: CFTimeInterval; message 'beginTime';
    procedure setDuration(newValue: CFTimeInterval); message 'setDuration:';
    function duration: CFTimeInterval; message 'duration';
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    procedure setFillMode(newValue: NSString); message 'setFillMode:';
    function fillMode: NSString; message 'fillMode';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
    procedure setRepeatCount(newValue: single); message 'setRepeatCount:';
    function repeatCount: single; message 'repeatCount';
    procedure setRepeatDuration(newValue: CFTimeInterval); message 'setRepeatDuration:';
    function repeatDuration: CFTimeInterval; message 'repeatDuration';
    procedure setSpeed(newValue: single); message 'setSpeed:';
    function speed: single; message 'speed';
    procedure setTimeOffset(newValue: CFTimeInterval); message 'setTimeOffset:';
    function timeOffset: CFTimeInterval; message 'timeOffset';
  end;


type
  CALayoutManager = objccategory external (NSObject)
    function preferredSizeOfLayer (layer: CALayer): CGSize; message 'preferredSizeOfLayer:';
    procedure invalidateLayoutOfLayer (layer: CALayer); message 'invalidateLayoutOfLayer:';
    procedure layoutSublayersOfLayer (layer: CALayer); message 'layoutSublayersOfLayer:';
  end;
{$endif}

{$ifdef PROTOCOLS}

type
  CAActionProtocol = objcprotocol external name 'CAAction'
    procedure runActionForKey_object_arguments (event: NSString; anObject: id; dict: NSDictionary); message 'runActionForKey:object:arguments:';
  end;
{$endif}

{$ifdef CLASSES}

type
  CALayerDelegate = objccategory external (NSObject)
    procedure displayLayer (layer: CALayer); message 'displayLayer:';
    procedure drawLayer_inContext (layer: CALayer; ctx: CGContextRef); message 'drawLayer:inContext:';
    procedure layoutSublayersOfLayer (layer: CALayer); message 'layoutSublayersOfLayer:';
    function actionForLayer_forKey (layer: CALayer; event: NSString): CAActionProtocol; message 'actionForLayer:forKey:';
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  kCAGravityCenter: NSString { available starting in __MAC_10_5, __IPHONE_2_0 }; cvar; external;
  kCAGravityTop: NSString { available starting in __MAC_10_5, __IPHONE_2_0 }; cvar; external;
  kCAGravityBottom: NSString { available starting in __MAC_10_5, __IPHONE_2_0 }; cvar; external;
  kCAGravityLeft: NSString { available starting in __MAC_10_5, __IPHONE_2_0 }; cvar; external;
  kCAGravityRight: NSString { available starting in __MAC_10_5, __IPHONE_2_0 }; cvar; external;
  kCAGravityTopLeft: NSString { available starting in __MAC_10_5, __IPHONE_2_0 }; cvar; external;
  kCAGravityTopRight: NSString { available starting in __MAC_10_5, __IPHONE_2_0 }; cvar; external;
  kCAGravityBottomLeft: NSString { available starting in __MAC_10_5, __IPHONE_2_0 }; cvar; external;
  kCAGravityBottomRight: NSString { available starting in __MAC_10_5, __IPHONE_2_0 }; cvar; external;
  kCAGravityResize: NSString { available starting in __MAC_10_5, __IPHONE_2_0 }; cvar; external;
  kCAGravityResizeAspect: NSString { available starting in __MAC_10_5, __IPHONE_2_0 }; cvar; external;
  kCAGravityResizeAspectFill: NSString { available starting in __MAC_10_5, __IPHONE_2_0 }; cvar; external;
  kCAFilterNearest: NSString { available starting in __MAC_10_5, __IPHONE_2_0 }; cvar; external;
  kCAFilterLinear: NSString { available starting in __MAC_10_5, __IPHONE_2_0 }; cvar; external;
  kCAFilterTrilinear: NSString { available starting in __MAC_10_6, __IPHONE_3_0 }; cvar; external;
  kCAOnOrderIn: NSString { available starting in __MAC_10_5, __IPHONE_2_0 }; cvar; external;
  kCAOnOrderOut: NSString { available starting in __MAC_10_5, __IPHONE_2_0 }; cvar; external;
  kCATransition: NSString { available starting in __MAC_10_5, __IPHONE_2_0 }; cvar; external;
{$endif}

