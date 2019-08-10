{ Parsed from AppKit.framework NSRotationGestureRecognizer.h }


{$ifdef TYPES}
type
  NSRotationGestureRecognizerPtr = ^NSRotationGestureRecognizer;
{$endif}

{$ifdef CLASSES}

type
  NSRotationGestureRecognizer = objcclass external (NSGestureRecognizer)
  private
    _location: NSPoint;
    _reserved1: NSPoint;
    _rflags: NSInteger;
    _rotation: CGFloat;
    {$ifndef __OBJC2__}
    _reserved2: NSInteger;
    _reserved3: NSInteger;
    {$endif}
  public
    procedure setRotation(newValue: CGFloat); message 'setRotation:';
    function rotation: CGFloat; message 'rotation';
    procedure setRotationInDegrees(newValue: CGFloat); message 'setRotationInDegrees:';
    function rotationInDegrees: CGFloat; message 'rotationInDegrees';
  end;
{$endif}

