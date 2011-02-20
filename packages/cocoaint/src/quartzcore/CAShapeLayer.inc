{ Parsed from Quartzcore.framework CAShapeLayer.h }

{$ifdef TYPES}
{$ifndef CASHAPELAYER_PAS_T}
{$define CASHAPELAYER_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef CASHAPELAYER_PAS_R}
{$define CASHAPELAYER_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef CASHAPELAYER_PAS_F}
{$define CASHAPELAYER_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef CASHAPELAYER_PAS_S}
{$define CASHAPELAYER_PAS_S}

{ External string constants }
var
  kCAFillRuleNonZero: NSString; cvar; external;
  kCAFillRuleEvenOdd: NSString; cvar; external;
  kCALineJoinMiter: NSString; cvar; external;
  kCALineJoinRound: NSString; cvar; external;
  kCALineJoinBevel: NSString; cvar; external;
  kCALineCapButt: NSString; cvar; external;
  kCALineCapRound: NSString; cvar; external;
  kCALineCapSquare: NSString; cvar; external;

{$endif}
{$endif}

{$ifdef FORWARD}
  CAShapeLayer = objcclass;
  CAShapeLayerPointer = ^CAShapeLayer;
  CAShapeLayerPtr = CAShapeLayerPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef CASHAPELAYER_PAS_C}
{$define CASHAPELAYER_PAS_C}

{ CAShapeLayer }
  CAShapeLayer = objcclass external (CALayer)
    
  public
    procedure setPath (newValue: CGPathRef); message 'setPath:';
    function path: CGPathRef; message 'path';
    procedure setFillColor (newValue: CGColorRef); message 'setFillColor:';
    function fillColor: CGColorRef; message 'fillColor';
    procedure setFillRule (newValue: NSString); message 'setFillRule:';
    function fillRule: NSString; message 'fillRule';
    procedure setStrokeColor (newValue: CGColorRef); message 'setStrokeColor:';
    function strokeColor: CGColorRef; message 'strokeColor';
    procedure setLineWidth (newValue: CGFloat); message 'setLineWidth:';
    function lineWidth: CGFloat; message 'lineWidth';
    procedure setMiterLimit (newValue: CGFloat); message 'setMiterLimit:';
    function miterLimit: CGFloat; message 'miterLimit';
    procedure setLineCap (newValue: NSString); message 'setLineCap:';
    function lineCap: NSString; message 'lineCap';
    procedure setLineJoin (newValue: NSString); message 'setLineJoin:';
    function lineJoin: NSString; message 'lineJoin';
    procedure setLineDashPhase (newValue: CGFloat); message 'setLineDashPhase:';
    function lineDashPhase: CGFloat; message 'lineDashPhase';
    procedure setLineDashPattern (newValue: NSArray); message 'setLineDashPattern:';
    function lineDashPattern: NSArray; message 'lineDashPattern';
  end;

{$endif}
{$endif}
