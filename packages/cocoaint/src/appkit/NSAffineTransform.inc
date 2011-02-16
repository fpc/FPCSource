{ Parsed from Appkit.framework NSAffineTransform.h }

{$ifdef TYPES}
{$ifndef NSAFFINETRANSFORM_PAS_T}
{$define NSAFFINETRANSFORM_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSAFFINETRANSFORM_PAS_R}
{$define NSAFFINETRANSFORM_PAS_R}

{ Records }
type
  NSAffineTransformStruct = record
    m11, m12, m21, m22: CGFloat;
    tX, tY: CGFloat;
  end;
NSAffineTransformStructPtr = ^NSAffineTransformStruct;


{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSAFFINETRANSFORM_PAS_F}
{$define NSAFFINETRANSFORM_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSAFFINETRANSFORM_PAS_S}
{$define NSAFFINETRANSFORM_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSAffineTransform = objcclass;
  NSAffineTransformPointer = ^NSAffineTransform;
  NSAffineTransformPtr = NSAffineTransformPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSAFFINETRANSFORM_PAS_C}
{$define NSAFFINETRANSFORM_PAS_C}

{ NSAffineTransform }
  NSAffineTransform = objcclass external (NSObject, NSCopyingProtocol, NSCodingProtocol)
  private
    _transformStruct: NSAffineTransformStruct;
    
  public
    class function transform: NSAffineTransform; message 'transform';
    function initWithTransform(transform_: NSAffineTransform): id; message 'initWithTransform:';
    procedure translateXBy_yBy(deltaX: CGFloat; deltaY: CGFloat); message 'translateXBy:yBy:';
    procedure rotateByDegrees(angle: CGFloat); message 'rotateByDegrees:';
    procedure rotateByRadians(angle: CGFloat); message 'rotateByRadians:';
    procedure scaleBy(scale: CGFloat); message 'scaleBy:';
    procedure scaleXBy_yBy(scaleX: CGFloat; scaleY: CGFloat); message 'scaleXBy:yBy:';
    procedure invert; message 'invert';
    procedure appendTransform(transform_: NSAffineTransform); message 'appendTransform:';
    procedure prependTransform(transform_: NSAffineTransform); message 'prependTransform:';
    function transformPoint(aPoint: NSPoint): NSPoint; message 'transformPoint:';
    function transformSize(aSize: NSSize): NSSize; message 'transformSize:';
    function transformStruct: NSAffineTransformStruct; message 'transformStruct';
    procedure setTransformStruct(transformStruct_: NSAffineTransformStruct); message 'setTransformStruct:';

    { Adopted Protocols }
    function copyWithZone(zone_: NSZonePtr): id;
    procedure encodeWithCoder(aCoder: NSCoder);
    function initWithCoder(aDecoder: NSCoder): id;
  end;

{ NSAppKitAdditonsCategory }
  NSAppKitAdditonsCategory = objccategory external (NSAffineTransform)
    function transformBezierPath(aPath: NSBezierPath): NSBezierPath; message 'transformBezierPath:';
    procedure set_; message 'set';
    procedure concat; message 'concat';
  end;

{$endif}
{$endif}
