{ Parsed from Appkit.framework NSStringDrawing.h }
{ Version: 2.1.2 - Wed Dec 8 10:06:06 CET 2010 }


{$ifdef TYPES}
{$ifndef NSSTRINGDRAWING_PAS_T}
{$define NSSTRINGDRAWING_PAS_T}

{ Constants }

const
  NSStringDrawingTruncatesLastVisibleLine = 1 shl 5;
  NSStringDrawingUsesLineFragmentOrigin = 1 shl 0;
  NSStringDrawingUsesFontLeading = 1 shl 1;
  NSStringDrawingDisableScreenFontSubstitution = 1 shl 2;
  NSStringDrawingUsesDeviceMetrics = 1 shl 3;
  NSStringDrawingOneShot = 1 shl 4;

{ Types }
type
  NSStringDrawingOptions = NSInteger;
  NSStringDrawingOptionsPtr = ^NSStringDrawingOptions;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSSTRINGDRAWING_PAS_R}
{$define NSSTRINGDRAWING_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSSTRINGDRAWING_PAS_F}
{$define NSSTRINGDRAWING_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSSTRINGDRAWING_PAS_S}
{$define NSSTRINGDRAWING_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}

{$endif}

{$ifdef CLASSES}
{$ifndef NSSTRINGDRAWING_PAS_C}
{$define NSSTRINGDRAWING_PAS_C}

{ NSStringDrawingCategory }
  NSStringDrawingCategory = objccategory(NSString)
    function sizeWithAttributes(attrs: NSDictionary): NSSize; message 'sizeWithAttributes:';
    procedure drawAtPoint_withAttributes(point: NSPoint; attrs: NSDictionary); message 'drawAtPoint:withAttributes:';
    procedure drawInRect_withAttributes(rect: NSRect; attrs: NSDictionary); message 'drawInRect:withAttributes:';
  end; external;

{ NSStringDrawing_NSAttributedStringCategory }
  NSStringDrawing_NSAttributedStringCategory = objccategory(NSAttributedString)
    function size: NSSize; message 'size';
    procedure drawAtPoint(point: NSPoint); message 'drawAtPoint:';
    procedure drawInRect(rect: NSRect); message 'drawInRect:';
  end; external name 'NSStringDrawing';

{ NSExtendedStringDrawingCategory }
  NSExtendedStringDrawingCategory = objccategory(NSString)
    procedure drawWithRect_options_attributes(rect: NSRect; options: NSStringDrawingOptions; attributes: NSDictionary); message 'drawWithRect:options:attributes:';
    function boundingRectWithSize_options_attributes(size: NSSize; options: NSStringDrawingOptions; attributes: NSDictionary): NSRect; message 'boundingRectWithSize:options:attributes:';
  end; external;

{ NSExtendedStringDrawing_NSAttributedStringCategory }
  NSExtendedStringDrawing_NSAttributedStringCategory = objccategory(NSAttributedString)
    procedure drawWithRect_options(rect: NSRect; options: NSStringDrawingOptions); message 'drawWithRect:options:';
    function boundingRectWithSize_options(size: NSSize; options: NSStringDrawingOptions): NSRect; message 'boundingRectWithSize:options:';
  end; external name 'NSExtendedStringDrawing';

{$endif}
{$endif}
