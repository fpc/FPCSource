{ Parsed from Appkit.framework NSBezierPath.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Thu Oct 1 18:54:07 ICT 2009 }


{$ifdef TYPES}
{$ifndef NSBEZIERPATH_PAS_T}
{$define NSBEZIERPATH_PAS_T}

{ Constants }

const
  NSButtLineCapStyle = 0;
  NSRoundLineCapStyle = 1;
  NSSquareLineCapStyle = 2;

const
  NSMiterLineJoinStyle = 0;
  NSRoundLineJoinStyle = 1;
  NSBevelLineJoinStyle = 2;

const
  NSNonZeroWindingRule = 0;
  NSEvenOddWindingRule = 1;

const
  NSMoveToBezierPathElement = 0;
  NSLineToBezierPathElement = 1;
  NSCurveToBezierPathElement = 2;
  NSClosePathBezierPathElement = 3;

{ Types }
type
  NSLineCapStyle = NSUInteger;
  NSLineJoinStyle = NSUInteger;
  NSWindingRule = NSUInteger;
  NSBezierPathElement = NSUInteger;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSBEZIERPATH_PAS_R}
{$define NSBEZIERPATH_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSBEZIERPATH_PAS_F}
{$define NSBEZIERPATH_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSBEZIERPATH_PAS_S}
{$define NSBEZIERPATH_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSBezierPath = objcclass;
  NSBezierPathPointer = ^NSBezierPath;

{$endif}

{$ifdef CLASSES}
{$ifndef NSBEZIERPATH_PAS_C}
{$define NSBEZIERPATH_PAS_C}

{ NSBezierPath }
  NSBezierPath = objcclass(NSObject, NSCopyingProtocol, NSCodingProtocol)
  private
    __segmentCount: NSInteger;
    __segmentMax: NSInteger;
    __head: PATHSEGMENT;
    __lastSubpathIndex: NSInteger;
    __elementCount: NSInteger;
    __lineWidth: CGFloat;
    __controlPointBounds: NSRect;
    __miterLimit: CGFloat;
    __flatness: CGFloat;
    __dashedLinePattern: CGFloat;
    __dashedLineCount: NSUInteger;
    __dashedLinePhase: CGFloat;
    __path: Pointer;
        _private: array[0..3] of id;
    __bpFlags: bitpacked record
        _flags: 0..((1 shl 8)-1);
        _pathState: 0..((1 shl 2)-1);
        
        _unused: 0..((1 shl 22)-1);
      end;
    
  public
    class function alloc: NSBezierPath; message 'alloc';

    class function bezierPath: NSBezierPath; message 'bezierPath';
    class function bezierPathWithRect(rect: NSRect): NSBezierPath; message 'bezierPathWithRect:';
    class function bezierPathWithOvalInRect(rect: NSRect): NSBezierPath; message 'bezierPathWithOvalInRect:';
    class function bezierPathWithRoundedRect_xRadius_yRadius(rect: NSRect; xRadius: CGFloat; yRadius: CGFloat): NSBezierPath; message 'bezierPathWithRoundedRect:xRadius:yRadius:';
    class procedure fillRect(rect: NSRect); message 'fillRect:';
    class procedure strokeRect(rect: NSRect); message 'strokeRect:';
    class procedure clipRect(rect: NSRect); message 'clipRect:';
    class procedure strokeLineFromPoint_toPoint(point: NSPoint; point1: NSPoint); message 'strokeLineFromPoint:toPoint:';
    class procedure drawPackedGlyphs_atPoint(packedGlyphs: PChar; point: NSPoint); message 'drawPackedGlyphs:atPoint:';
    class procedure setDefaultMiterLimit(limit: CGFloat); message 'setDefaultMiterLimit:';
    class function defaultMiterLimit: CGFloat; message 'defaultMiterLimit';
    class procedure setDefaultFlatness(flatness_: CGFloat); message 'setDefaultFlatness:';
    class function defaultFlatness: CGFloat; message 'defaultFlatness';
    class procedure setDefaultWindingRule(windingRule_: NSWindingRule); message 'setDefaultWindingRule:';
    class function defaultWindingRule: NSWindingRule; message 'defaultWindingRule';
    class procedure setDefaultLineCapStyle(lineCapStyle_: NSLineCapStyle); message 'setDefaultLineCapStyle:';
    class function defaultLineCapStyle: NSLineCapStyle; message 'defaultLineCapStyle';
    class procedure setDefaultLineJoinStyle(lineJoinStyle_: NSLineJoinStyle); message 'setDefaultLineJoinStyle:';
    class function defaultLineJoinStyle: NSLineJoinStyle; message 'defaultLineJoinStyle';
    class procedure setDefaultLineWidth(lineWidth_: CGFloat); message 'setDefaultLineWidth:';
    class function defaultLineWidth: CGFloat; message 'defaultLineWidth';
    procedure moveToPoint(point: NSPoint); message 'moveToPoint:';
    procedure lineToPoint(point: NSPoint); message 'lineToPoint:';
    procedure closePath; message 'closePath';
    procedure removeAllPoints; message 'removeAllPoints';
    procedure relativeMoveToPoint(point: NSPoint); message 'relativeMoveToPoint:';
    procedure relativeLineToPoint(point: NSPoint); message 'relativeLineToPoint:';
    function lineWidth: CGFloat; message 'lineWidth';
    procedure setLineWidth(lineWidth_: CGFloat); message 'setLineWidth:';
    function lineCapStyle: NSLineCapStyle; message 'lineCapStyle';
    procedure setLineCapStyle(lineCapStyle_: NSLineCapStyle); message 'setLineCapStyle:';
    function lineJoinStyle: NSLineJoinStyle; message 'lineJoinStyle';
    procedure setLineJoinStyle(lineJoinStyle_: NSLineJoinStyle); message 'setLineJoinStyle:';
    function windingRule: NSWindingRule; message 'windingRule';
    procedure setWindingRule(windingRule_: NSWindingRule); message 'setWindingRule:';
    function miterLimit: CGFloat; message 'miterLimit';
    procedure setMiterLimit(miterLimit_: CGFloat); message 'setMiterLimit:';
    function flatness: CGFloat; message 'flatness';
    procedure setFlatness(flatness_: CGFloat); message 'setFlatness:';
    procedure getLineDash_count_phase(pattern: psingle; count: NSIntegerPointer; phase: psingle); message 'getLineDash:count:phase:';
    procedure setLineDash_count_phase(pattern: psingle; count: NSInteger; phase: CGFloat); message 'setLineDash:count:phase:';
    procedure stroke; message 'stroke';
    procedure fill; message 'fill';
    procedure addClip; message 'addClip';
    procedure setClip; message 'setClip';
    function bezierPathByFlatteningPath: NSBezierPath; message 'bezierPathByFlatteningPath';
    function bezierPathByReversingPath: NSBezierPath; message 'bezierPathByReversingPath';
    procedure transformUsingAffineTransform(transform: NSAffineTransform); message 'transformUsingAffineTransform:';
    function isEmpty: Boolean; message 'isEmpty';
    function currentPoint: NSPoint; message 'currentPoint';
    function controlPointBounds: NSRect; message 'controlPointBounds';
    function bounds: NSRect; message 'bounds';
    function elementCount: NSInteger; message 'elementCount';
    function elementAtIndex(index: NSInteger): NSBezierPathElement; message 'elementAtIndex:';
    procedure setAssociatedPoints_atIndex(points: NSPointArray; index: NSInteger); message 'setAssociatedPoints:atIndex:';
    procedure appendBezierPath(path: NSBezierPath); message 'appendBezierPath:';
    procedure appendBezierPathWithRect(rect: NSRect); message 'appendBezierPathWithRect:';
    procedure appendBezierPathWithPoints_count(points: NSPointArray; count: NSInteger); message 'appendBezierPathWithPoints:count:';
    procedure appendBezierPathWithOvalInRect(rect: NSRect); message 'appendBezierPathWithOvalInRect:';
    procedure appendBezierPathWithGlyph_inFont(glyph: NSGlyph; font: NSFont); message 'appendBezierPathWithGlyph:inFont:';
    procedure appendBezierPathWithPackedGlyphs(packedGlyphs: PChar); message 'appendBezierPathWithPackedGlyphs:';
    procedure appendBezierPathWithRoundedRect_xRadius_yRadius(rect: NSRect; xRadius: CGFloat; yRadius: CGFloat); message 'appendBezierPathWithRoundedRect:xRadius:yRadius:';
    function containsPoint(point: NSPoint): Boolean; message 'containsPoint:';
    function cachesBezierPath: Boolean; message 'cachesBezierPath';
    procedure setCachesBezierPath(flag: Boolean); message 'setCachesBezierPath:';
  end; external;

{$endif}
{$endif}
