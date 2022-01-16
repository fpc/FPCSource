{ Parsed from AppKit.framework NSStringDrawing.h }

{$ifdef CLASSES}

type
  NSString_NSStringDrawing = objccategory external name 'NSStringDrawing' (NSString)
    function sizeWithAttributes (attrs: NSDictionary): NSSize; message 'sizeWithAttributes:';
    procedure drawAtPoint_withAttributes (point: NSPoint; attrs: NSDictionary); message 'drawAtPoint:withAttributes:';
    procedure drawInRect_withAttributes (rect: NSRect; attrs: NSDictionary); message 'drawInRect:withAttributes:';
  end;

type
  NSStringDrawing = objccategory external (NSAttributedString)
    function size: NSSize; message 'size';
    procedure drawAtPoint (point: NSPoint); message 'drawAtPoint:';
    procedure drawInRect (rect: NSRect); message 'drawInRect:';
  end;
{$endif}

{$ifdef TYPES}
type
  NSStringDrawingOptions = NSUInteger;
  NSStringDrawingOptionsPtr = ^NSStringDrawingOptions;

const
  NSStringDrawingTruncatesLastVisibleLine = 1 shl 5 { available in 10_5 };
  NSStringDrawingUsesLineFragmentOrigin = 1 shl 0;
  NSStringDrawingUsesFontLeading = 1 shl 1;
  NSStringDrawingDisableScreenFontSubstitution = 1 shl 2;
  NSStringDrawingUsesDeviceMetrics = 1 shl 3;
  NSStringDrawingOneShot = 1 shl 4;
{$endif}

{$ifdef CLASSES}

type
  NSString_NSExtendedStringDrawing = objccategory external name 'NSExtendedStringDrawing' (NSString)
    procedure drawWithRect_options_attributes (rect: NSRect; options: NSStringDrawingOptions; attributes: NSDictionary); message 'drawWithRect:options:attributes:';
    function boundingRectWithSize_options_attributes (size: NSSize; options: NSStringDrawingOptions; attributes: NSDictionary): NSRect; message 'boundingRectWithSize:options:attributes:';
  end;

type
  NSExtendedStringDrawing = objccategory external (NSAttributedString)
    procedure drawWithRect_options (rect: NSRect; options: NSStringDrawingOptions); message 'drawWithRect:options:';
    function boundingRectWithSize_options (size: NSSize; options: NSStringDrawingOptions): NSRect; message 'boundingRectWithSize:options:';
  end;
{$endif}

