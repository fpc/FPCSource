{ Parsed from Appkit.framework NSAffineTransform.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Thu Sep 17 14:52:45 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSAFFINETRANSFORM_PAS_H}
{$define NSAFFINETRANSFORM_PAS_H}
type
  NSAffineTransformPointer = Pointer;

{$endif}
{$endif}

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
    m11: CGFloat;
    tX: CGFloat;
  end;


{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSAFFINETRANSFORM_PAS_F}
{$define NSAFFINETRANSFORM_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSAFFINETRANSFORM_PAS_T}
{$define NSAFFINETRANSFORM_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSAffineTransform = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSAFFINETRANSFORM_PAS_C}
{$define NSAFFINETRANSFORM_PAS_C}

{ NSAffineTransform }
  NSAffineTransform = objcclass(NSObject, NSCopyingProtocol, NSCodingProtocol)
  private
    __transformStruct: NSAffineTransformStruct;
    
  public
    class function alloc: NSAffineTransform; message 'alloc';

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

    { Category: NSAppKitAdditons }
    function transformBezierPath(aPath: NSBezierPath): NSBezierPath; message 'transformBezierPath:';
    procedure set_; message 'set';
    procedure concat; message 'concat';
  end; external;

{$endif}
{$endif}
