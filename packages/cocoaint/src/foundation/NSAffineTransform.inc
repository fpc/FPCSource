{ Parsed from Foundation.framework NSAffineTransform.h }


{$ifdef TYPES}
type
  NSAffineTransformPtr = ^NSAffineTransform;
{$endif}

{$ifdef TYPES}
type
  NSAffineTransformStruct = record
    m11, m12, m21, m22: CGFloat;
    tX, tY: CGFloat;
  end;
type
  NSAffineTransformStructPtr = ^NSAffineTransformStruct;
{$endif}

{$ifdef CLASSES}

type
  NSAffineTransform = objcclass external (NSObject, NSCopyingProtocol, NSCodingProtocol)
  private
    _transformStruct: NSAffineTransformStruct;
  public
    class function transform: NSAffineTransform; message 'transform';
    function initWithTransform (transform_: NSAffineTransform): instancetype; message 'initWithTransform:';
    function init: instancetype; message 'init'; { NS_DESIGNATED_INITIALIZER }
    procedure translateXBy_yBy (deltaX: CGFloat; deltaY: CGFloat); message 'translateXBy:yBy:';
    procedure rotateByDegrees (angle: CGFloat); message 'rotateByDegrees:';
    procedure rotateByRadians (angle: CGFloat); message 'rotateByRadians:';
    procedure scaleBy (scale: CGFloat); message 'scaleBy:';
    procedure scaleXBy_yBy (scaleX: CGFloat; scaleY: CGFloat); message 'scaleXBy:yBy:';
    procedure invert; message 'invert';
    procedure appendTransform (transform_: NSAffineTransform); message 'appendTransform:';
    procedure prependTransform (transform_: NSAffineTransform); message 'prependTransform:';
    function transformPoint (aPoint: NSPoint): NSPoint; message 'transformPoint:';
    function transformSize (aSize: NSSize): NSSize; message 'transformSize:';
    procedure setTransformStruct(newValue: NSAffineTransformStruct); message 'setTransformStruct:';
    function transformStruct: NSAffineTransformStruct; message 'transformStruct';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
  end;
{$endif}

