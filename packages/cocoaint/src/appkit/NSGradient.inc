{ Parsed from AppKit.framework NSGradient.h }


{$ifdef TYPES}
type
  NSGradientPtr = ^NSGradient;
{$endif}

{$ifdef TYPES}
type
  NSGradientDrawingOptions = NSUInteger;
  NSGradientDrawingOptionsPtr = ^NSGradientDrawingOptions;

const
  NSGradientDrawsBeforeStartingLocation = 1 shl 0;
  NSGradientDrawsAfterEndingLocation = 1 shl 1;
{$endif}

{$ifdef CLASSES}

type
  NSGradient = objcclass external (NSObject, NSCopyingProtocol, NSCodingProtocol)
  private
    _colorArray: NSArray;
    _colorSpace: NSColorSpace;
    _functionRef: pointer;
    _componentArray: pointer;
    _reserved1: pointer;
    _reserved2: pointer;
    _reserved3: pointer;
  public
    function initWithStartingColor_endingColor (startingColor: NSColor; endingColor: NSColor): instancetype; message 'initWithStartingColor:endingColor:';
    function initWithColors (colorArray: NSArray): instancetype; message 'initWithColors:';
    function initWithColorsAndLocations (firstColor: NSColor): instancetype; varargs; message 'initWithColorsAndLocations:';
    function initWithColors_atLocations_colorSpace (colorArray: NSArray; locations: CGFloatPtr; colorSpace: NSColorSpace): instancetype; message 'initWithColors:atLocations:colorSpace:';
    procedure drawFromPoint_toPoint_options (startingPoint: NSPoint; endingPoint: NSPoint; options: NSGradientDrawingOptions); message 'drawFromPoint:toPoint:options:';
    procedure drawInRect_angle (rect: NSRect; angle: CGFloat); message 'drawInRect:angle:';
    procedure drawInBezierPath_angle (path: NSBezierPath; angle: CGFloat); message 'drawInBezierPath:angle:';
    procedure drawFromCenter_radius_toCenter_radius_options (startCenter: NSPoint; startRadius: CGFloat; endCenter: NSPoint; endRadius: CGFloat; options: NSGradientDrawingOptions); message 'drawFromCenter:radius:toCenter:radius:options:';
    procedure drawInRect_relativeCenterPosition (rect: NSRect; relativeCenterPosition: NSPoint); message 'drawInRect:relativeCenterPosition:';
    procedure drawInBezierPath_relativeCenterPosition (path: NSBezierPath; relativeCenterPosition: NSPoint); message 'drawInBezierPath:relativeCenterPosition:';
    function colorSpace: NSColorSpace; message 'colorSpace';
    function numberOfColorStops: NSInteger; message 'numberOfColorStops';
    procedure getColor_location_atIndex (color: NSColorPtr; location: CGFloatPtr; index: NSInteger); message 'getColor:location:atIndex:';
    function interpolatedColorAtLocation (location: CGFloat): NSColor; message 'interpolatedColorAtLocation:';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
  end;
{$endif}

