{ Parsed from AppKit.framework NSBezierPath.h }


{$ifdef TYPES}
type
  NSBezierPathPtr = ^NSBezierPath;
{$endif}

{$ifdef TYPES}
type
  NSLineCapStyle = NSUInteger;
  NSLineCapStylePtr = ^NSLineCapStyle;

const
  NSButtLineCapStyle = 0;
  NSRoundLineCapStyle = 1;
  NSSquareLineCapStyle = 2;

type
  NSLineJoinStyle = NSUInteger;
  NSLineJoinStylePtr = ^NSLineJoinStyle;

const
  NSMiterLineJoinStyle = 0;
  NSRoundLineJoinStyle = 1;
  NSBevelLineJoinStyle = 2;

type
  NSWindingRule = NSUInteger;
  NSWindingRulePtr = ^NSWindingRule;

const
  NSNonZeroWindingRule = 0;
  NSEvenOddWindingRule = 1;

type
  NSBezierPathElement = NSUInteger;
  NSBezierPathElementPtr = ^NSBezierPathElement;

const
  NSMoveToBezierPathElement = 0;
  NSLineToBezierPathElement = 1;
  NSCurveToBezierPathElement = 2;
  NSClosePathBezierPathElement = 3;
{$endif}

{$ifdef CLASSES}

type
  NSBezierPath = objcclass external (NSObject, NSCopyingProtocol, NSCodingProtocol)
  private
    _elementCount: NSInteger;
    _elementMax: NSInteger;
    _head: PATHSEGMENTPtr;
    _lastSubpathIndex: NSInteger;
    _extraSegments: pointer;
    _lineWidth: CGFloat;
    _controlPointBounds: NSRect;
    _miterLimit: CGFloat;
    _flatness: CGFloat;
    _dashedLinePattern: CGFloatPtr;
    _dashedLineCount: NSUInteger;
    _dashedLinePhase: CGFloat;
    _path: pointer;
    _extraSegmentCount: NSInteger;
    _extraSegmentMax: NSInteger;
    {$if  not defined(__LP64__)}
    _private:array[0..1] of id;
    {$endif}
    _bpFlags: bitpacked record
      case byte of
        0: (_anonBitField__bpFlags0: cuint);
        1: (
          _flags: 0..((1 shl 8)-1);
          _pathState: 0..((1 shl 2)-1);
          _unused: 0..((1 shl 22)-1);
        );
      end;
  public
    class function bezierPath: NSBezierPath; message 'bezierPath';
    class function bezierPathWithRect (rect: NSRect): NSBezierPath; message 'bezierPathWithRect:';
    class function bezierPathWithOvalInRect (rect: NSRect): NSBezierPath; message 'bezierPathWithOvalInRect:';
    class function bezierPathWithRoundedRect_xRadius_yRadius (rect: NSRect; xRadius: CGFloat; yRadius: CGFloat): NSBezierPath; message 'bezierPathWithRoundedRect:xRadius:yRadius:'; { available in 10_5 }
    class procedure fillRect (rect: NSRect); message 'fillRect:';
    class procedure strokeRect (rect: NSRect); message 'strokeRect:';
    class procedure clipRect (rect: NSRect); message 'clipRect:';
    class procedure strokeLineFromPoint_toPoint (point1: NSPoint; point2: NSPoint); message 'strokeLineFromPoint:toPoint:';
    class procedure drawPackedGlyphs_atPoint (packedGlyphs: PChar; point: NSPoint); message 'drawPackedGlyphs:atPoint:';
    class procedure setDefaultMiterLimit (limit: CGFloat); message 'setDefaultMiterLimit:';
    class function defaultMiterLimit: CGFloat; message 'defaultMiterLimit';
    class procedure setDefaultFlatness (flatness: CGFloat); message 'setDefaultFlatness:';
    class function defaultFlatness: CGFloat; message 'defaultFlatness';
    class procedure setDefaultWindingRule (windingRule: NSWindingRule); message 'setDefaultWindingRule:';
    class function defaultWindingRule: NSWindingRule; message 'defaultWindingRule';
    class procedure setDefaultLineCapStyle (lineCapStyle: NSLineCapStyle); message 'setDefaultLineCapStyle:';
    class function defaultLineCapStyle: NSLineCapStyle; message 'defaultLineCapStyle';
    class procedure setDefaultLineJoinStyle (lineJoinStyle: NSLineJoinStyle); message 'setDefaultLineJoinStyle:';
    class function defaultLineJoinStyle: NSLineJoinStyle; message 'defaultLineJoinStyle';
    class procedure setDefaultLineWidth (lineWidth: CGFloat); message 'setDefaultLineWidth:';
    class function defaultLineWidth: CGFloat; message 'defaultLineWidth';
    procedure moveToPoint (point: NSPoint); message 'moveToPoint:';
    procedure lineToPoint (point: NSPoint); message 'lineToPoint:';
    procedure curveToPoint_controlPoint1_controlPoint2 (endPoint: NSPoint; controlPoint1: NSPoint; controlPoint2: NSPoint); message 'curveToPoint:controlPoint1:controlPoint2:';
    procedure closePath; message 'closePath';
    procedure removeAllPoints; message 'removeAllPoints';
    procedure relativeMoveToPoint (point: NSPoint); message 'relativeMoveToPoint:';
    procedure relativeLineToPoint (point: NSPoint); message 'relativeLineToPoint:';
    procedure relativeCurveToPoint_controlPoint1_controlPoint2 (endPoint: NSPoint; controlPoint1: NSPoint; controlPoint2: NSPoint); message 'relativeCurveToPoint:controlPoint1:controlPoint2:';
    procedure setLineWidth(newValue: CGFloat); message 'setLineWidth:';
    function lineWidth: CGFloat; message 'lineWidth';
    procedure setLineCapStyle(newValue: NSLineCapStyle); message 'setLineCapStyle:';
    function lineCapStyle: NSLineCapStyle; message 'lineCapStyle';
    procedure setLineJoinStyle(newValue: NSLineJoinStyle); message 'setLineJoinStyle:';
    function lineJoinStyle: NSLineJoinStyle; message 'lineJoinStyle';
    procedure setWindingRule(newValue: NSWindingRule); message 'setWindingRule:';
    function windingRule: NSWindingRule; message 'windingRule';
    procedure setMiterLimit(newValue: CGFloat); message 'setMiterLimit:';
    function miterLimit: CGFloat; message 'miterLimit';
    procedure setFlatness(newValue: CGFloat); message 'setFlatness:';
    function flatness: CGFloat; message 'flatness';
    procedure getLineDash_count_phase (pattern: CGFloatPtr; count: NSIntegerPtr; phase: CGFloatPtr); message 'getLineDash:count:phase:';
    procedure setLineDash_count_phase (pattern: CGFloatPtr; count: NSInteger; phase: CGFloat); message 'setLineDash:count:phase:';
    procedure stroke; message 'stroke';
    procedure fill; message 'fill';
    procedure addClip; message 'addClip';
    procedure setClip; message 'setClip';
    function bezierPathByFlatteningPath: NSBezierPath; message 'bezierPathByFlatteningPath';
    function bezierPathByReversingPath: NSBezierPath; message 'bezierPathByReversingPath';
    procedure transformUsingAffineTransform (transform: NSAffineTransform); message 'transformUsingAffineTransform:';
    function isEmpty: ObjCBOOL; message 'isEmpty';
    function currentPoint: NSPoint; message 'currentPoint';
    function controlPointBounds: NSRect; message 'controlPointBounds';
    function bounds: NSRect; message 'bounds';
    function elementCount: NSInteger; message 'elementCount';
    function elementAtIndex_associatedPoints (index: NSInteger; points: NSPointArray): NSBezierPathElement; message 'elementAtIndex:associatedPoints:';
    function elementAtIndex (index: NSInteger): NSBezierPathElement; message 'elementAtIndex:';
    procedure setAssociatedPoints_atIndex (points: NSPointArray; index: NSInteger); message 'setAssociatedPoints:atIndex:';
    procedure appendBezierPath (path: NSBezierPath); message 'appendBezierPath:';
    procedure appendBezierPathWithRect (rect: NSRect); message 'appendBezierPathWithRect:';
    procedure appendBezierPathWithPoints_count (points: NSPointArray; count: NSInteger); message 'appendBezierPathWithPoints:count:';
    procedure appendBezierPathWithOvalInRect (rect: NSRect); message 'appendBezierPathWithOvalInRect:';
    procedure appendBezierPathWithArcWithCenter_radius_startAngle_endAngle_clockwise (center: NSPoint; radius: CGFloat; startAngle: CGFloat; endAngle: CGFloat; clockwise: ObjCBOOL); message 'appendBezierPathWithArcWithCenter:radius:startAngle:endAngle:clockwise:';
    procedure appendBezierPathWithArcWithCenter_radius_startAngle_endAngle (center: NSPoint; radius: CGFloat; startAngle: CGFloat; endAngle: CGFloat); message 'appendBezierPathWithArcWithCenter:radius:startAngle:endAngle:';
    procedure appendBezierPathWithArcFromPoint_toPoint_radius (point1: NSPoint; point2: NSPoint; radius: CGFloat); message 'appendBezierPathWithArcFromPoint:toPoint:radius:';
    procedure appendBezierPathWithGlyph_inFont (glyph: NSGlyph; font: NSFont); message 'appendBezierPathWithGlyph:inFont:';
    procedure appendBezierPathWithGlyphs_count_inFont (glyphs: NSGlyphPtr; count: NSInteger; font: NSFont); message 'appendBezierPathWithGlyphs:count:inFont:';
    procedure appendBezierPathWithPackedGlyphs (packedGlyphs: PChar); message 'appendBezierPathWithPackedGlyphs:';
    procedure appendBezierPathWithRoundedRect_xRadius_yRadius (rect: NSRect; xRadius: CGFloat; yRadius: CGFloat); message 'appendBezierPathWithRoundedRect:xRadius:yRadius:'; { available in 10_5 }
    function containsPoint (point: NSPoint): ObjCBOOL; message 'containsPoint:';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
  end;


type
  NSBezierPathDeprecated = objccategory external (NSBezierPath)
    function cachesBezierPath: ObjCBOOL; message 'cachesBezierPath'; deprecated 'in 10_0, 10_0';
    procedure setCachesBezierPath (flag: ObjCBOOL); message 'setCachesBezierPath:'; deprecated 'in 10_0, 10_0';
  end;
{$endif}

