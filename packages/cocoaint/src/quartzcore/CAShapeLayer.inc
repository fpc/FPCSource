{ Parsed from QuartzCore.framework CAShapeLayer.h }


{$ifdef TYPES}
type
  CAShapeLayerPtr = ^CAShapeLayer;
{$endif}

{$ifdef CLASSES}

type
  CAShapeLayer = objcclass external (CALayer)
  public
    procedure setPath(newValue: CGPathRef); message 'setPath:';
    function path: CGPathRef; message 'path';
    procedure setFillColor(newValue: CGColorRef); message 'setFillColor:';
    function fillColor: CGColorRef; message 'fillColor';
    procedure setFillRule(newValue: NSString); message 'setFillRule:';
    function fillRule: NSString; message 'fillRule';
    procedure setStrokeColor(newValue: CGColorRef); message 'setStrokeColor:';
    function strokeColor: CGColorRef; message 'strokeColor';
    procedure setStrokeStart(newValue: CGFloat); message 'setStrokeStart:';
    function strokeStart: CGFloat; message 'strokeStart';
    procedure setStrokeEnd(newValue: CGFloat); message 'setStrokeEnd:';
    function strokeEnd: CGFloat; message 'strokeEnd';
    procedure setLineWidth(newValue: CGFloat); message 'setLineWidth:';
    function lineWidth: CGFloat; message 'lineWidth';
    procedure setMiterLimit(newValue: CGFloat); message 'setMiterLimit:';
    function miterLimit: CGFloat; message 'miterLimit';
    procedure setLineCap(newValue: NSString); message 'setLineCap:';
    function lineCap: NSString; message 'lineCap';
    procedure setLineJoin(newValue: NSString); message 'setLineJoin:';
    function lineJoin: NSString; message 'lineJoin';
    procedure setLineDashPhase(newValue: CGFloat); message 'setLineDashPhase:';
    function lineDashPhase: CGFloat; message 'lineDashPhase';
    procedure setLineDashPattern(newValue: NSArray); message 'setLineDashPattern:';
    function lineDashPattern: NSArray; message 'lineDashPattern';
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  kCAFillRuleNonZero: NSString { available starting in __MAC_10_6, __IPHONE_3_0 }; cvar; external;
  kCAFillRuleEvenOdd: NSString { available starting in __MAC_10_6, __IPHONE_3_0 }; cvar; external;
  kCALineJoinMiter: NSString { available starting in __MAC_10_6, __IPHONE_3_0 }; cvar; external;
  kCALineJoinRound: NSString { available starting in __MAC_10_6, __IPHONE_3_0 }; cvar; external;
  kCALineJoinBevel: NSString { available starting in __MAC_10_6, __IPHONE_3_0 }; cvar; external;
  kCALineCapButt: NSString { available starting in __MAC_10_6, __IPHONE_3_0 }; cvar; external;
  kCALineCapRound: NSString { available starting in __MAC_10_6, __IPHONE_3_0 }; cvar; external;
  kCALineCapSquare: NSString { available starting in __MAC_10_6, __IPHONE_3_0 }; cvar; external;
{$endif}

