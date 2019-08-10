{ Parsed from SpriteKit.framework SKConstraint.h }


{$ifdef TYPES}
type
  SKRangePtr = ^SKRange;
  SKConstraintPtr = ^SKConstraint;
{$endif}

{$ifdef CLASSES}

type
  SKRange = objcclass external (NSObject, NSCodingProtocol, NSCopyingProtocol)
  public
    function initWithLowerLimit_upperLimit (lower: CGFloat; upper: CGFloat): instancetype; message 'initWithLowerLimit:upperLimit:'; { NS_DESIGNATED_INITIALIZER }
    class function rangeWithLowerLimit_upperLimit (lower: CGFloat; upper: CGFloat): instancetype; message 'rangeWithLowerLimit:upperLimit:';
    class function rangeWithLowerLimit (lower: CGFloat): instancetype; message 'rangeWithLowerLimit:';
    class function rangeWithUpperLimit (upper: CGFloat): instancetype; message 'rangeWithUpperLimit:';
    class function rangeWithConstantValue (value: CGFloat): instancetype; message 'rangeWithConstantValue:';
    class function rangeWithValue_variance (value: CGFloat; variance: CGFloat): instancetype; message 'rangeWithValue:variance:';
    class function rangeWithNoLimits: instancetype; message 'rangeWithNoLimits';
    procedure setLowerLimit(newValue: CGFloat); message 'setLowerLimit:';
    function lowerLimit: CGFloat; message 'lowerLimit';
    procedure setUpperLimit(newValue: CGFloat); message 'setUpperLimit:';
    function upperLimit: CGFloat; message 'upperLimit';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
  end;

type
  SKConstraint = objcclass external (NSObject, NSCodingProtocol, NSCopyingProtocol)
  public
    procedure setEnabled(newValue: ObjCBOOL); message 'setEnabled:';
    function enabled: ObjCBOOL; message 'enabled';
    procedure setReferenceNode(newValue: SKNode); message 'setReferenceNode:';
    function referenceNode: SKNode; message 'referenceNode';
    class function positionX (range: SKRange): instancetype; message 'positionX:';
    class function positionY (range: SKRange): instancetype; message 'positionY:';
    class function positionX_Y (xRange: SKRange; yRange: SKRange): instancetype; message 'positionX:Y:';
    class function distance_toNode (range: SKRange; node: SKNode): instancetype; message 'distance:toNode:';
    class function distance_toPoint (range: SKRange; point: CGPoint): instancetype; message 'distance:toPoint:';
    class function distance_toPoint_inNode (range: SKRange; point: CGPoint; node: SKNode): instancetype; message 'distance:toPoint:inNode:';
    class function zRotation (zRange: SKRange): instancetype; message 'zRotation:';
    class function orientToNode_offset (node: SKNode; radians: SKRange): instancetype; message 'orientToNode:offset:';
    class function orientToPoint_offset (point: CGPoint; radians: SKRange): instancetype; message 'orientToPoint:offset:';
    class function orientToPoint_inNode_offset (point: CGPoint; node: SKNode; radians: SKRange): instancetype; message 'orientToPoint:inNode:offset:';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
  end;
{$endif}

