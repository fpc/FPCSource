{ Parsed from SpriteKit.framework SKShapeNode.h }


{$ifdef TYPES}
type
  SKShapeNodePtr = ^SKShapeNode;
{$endif}

{$ifdef CLASSES}

type
  SKShapeNode = objcclass external (SKNode)
  public
    class function shapeNodeWithPath (path: CGPathRef): instancetype; message 'shapeNodeWithPath:'; { available in 10_10, 8_0 }
    class function shapeNodeWithPath_centered (path: CGPathRef; centered: ObjCBOOL): instancetype; message 'shapeNodeWithPath:centered:'; { available in 10_10, 8_0 }
    class function shapeNodeWithRect (rect: CGRect): instancetype; message 'shapeNodeWithRect:'; { available in 10_10, 8_0 }
    class function shapeNodeWithRectOfSize (size: CGSize): instancetype; message 'shapeNodeWithRectOfSize:'; { available in 10_10, 8_0 }
    class function shapeNodeWithRect_cornerRadius (rect: CGRect; cornerRadius: CGFloat): instancetype; message 'shapeNodeWithRect:cornerRadius:'; { available in 10_10, 8_0 }
    class function shapeNodeWithRectOfSize_cornerRadius (size: CGSize; cornerRadius: CGFloat): instancetype; message 'shapeNodeWithRectOfSize:cornerRadius:'; { available in 10_10, 8_0 }
    class function shapeNodeWithCircleOfRadius (radius: CGFloat): instancetype; message 'shapeNodeWithCircleOfRadius:'; { available in 10_10, 8_0 }
    class function shapeNodeWithEllipseInRect (rect: CGRect): instancetype; message 'shapeNodeWithEllipseInRect:'; { available in 10_10, 8_0 }
    class function shapeNodeWithEllipseOfSize (size: CGSize): instancetype; message 'shapeNodeWithEllipseOfSize:'; { available in 10_10, 8_0 }
    class function shapeNodeWithPoints_count (points: CGPointPtr; numPoints: size_t): instancetype; message 'shapeNodeWithPoints:count:'; { available in 10_10, 8_0 }
    class function shapeNodeWithSplinePoints_count (points: CGPointPtr; numPoints: size_t): instancetype; message 'shapeNodeWithSplinePoints:count:'; { available in 10_10, 8_0 }
    procedure setPath(newValue: CGPathRef); message 'setPath:';
    function path: CGPathRef; message 'path';
    procedure setStrokeColor(newValue: SKColorPtr); message 'setStrokeColor:';
    function strokeColor: SKColorPtr; message 'strokeColor';
    procedure setFillColor(newValue: SKColorPtr); message 'setFillColor:';
    function fillColor: SKColorPtr; message 'fillColor';
    procedure setBlendMode(newValue: SKBlendMode); message 'setBlendMode:';
    function blendMode: SKBlendMode; message 'blendMode';
    procedure setAntialiased(newValue: ObjCBOOL); message 'setAntialiased:';
    function isAntialiased: ObjCBOOL; message 'isAntialiased';
    procedure setLineWidth(newValue: CGFloat); message 'setLineWidth:';
    function lineWidth: CGFloat; message 'lineWidth';
    procedure setGlowWidth(newValue: CGFloat); message 'setGlowWidth:';
    function glowWidth: CGFloat; message 'glowWidth';
    procedure setLineCap(newValue: CGLineCap); message 'setLineCap:';
    function lineCap: CGLineCap; message 'lineCap';
    procedure setLineJoin(newValue: CGLineJoin); message 'setLineJoin:';
    function lineJoin: CGLineJoin; message 'lineJoin';
    procedure setMiterLimit(newValue: CGFloat); message 'setMiterLimit:';
    function miterLimit: CGFloat; message 'miterLimit';
    function lineLength: CGFloat; message 'lineLength';
    procedure setFillTexture(newValue: SKTexture); message 'setFillTexture:';
    function fillTexture: SKTexture; message 'fillTexture';
    procedure setFillShader(newValue: SKShader); message 'setFillShader:';
    function fillShader: SKShader; message 'fillShader';
    procedure setStrokeTexture(newValue: SKTexture); message 'setStrokeTexture:';
    function strokeTexture: SKTexture; message 'strokeTexture';
    procedure setStrokeShader(newValue: SKShader); message 'setStrokeShader:';
    function strokeShader: SKShader; message 'strokeShader';
  end;
{$endif}

