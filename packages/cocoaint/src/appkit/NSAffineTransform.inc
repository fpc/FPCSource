{ Parsed from AppKit.framework NSAffineTransform.h }

{$ifdef CLASSES}

type
  NSAppKitAdditons = objccategory external (NSAffineTransform)
    function transformBezierPath (aPath: NSBezierPath): NSBezierPath; message 'transformBezierPath:';
    procedure set_; message 'set';
    procedure concat; message 'concat';
  end;
{$endif}

