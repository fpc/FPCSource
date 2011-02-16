{ Parsed from Appkit.framework NSGradient.h }

{$ifdef TYPES}
{$ifndef NSGRADIENT_PAS_T}
{$define NSGRADIENT_PAS_T}

{ Types }
type
  NSGradientDrawingOptions = NSUInteger;
  NSGradientDrawingOptionsPtr = ^NSGradientDrawingOptions;

{ Constants }

const
  NSGradientDrawsBeforeStartingLocation = 1 shl 0;
  NSGradientDrawsAfterEndingLocation = 1 shl 1;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSGRADIENT_PAS_R}
{$define NSGRADIENT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSGRADIENT_PAS_F}
{$define NSGRADIENT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSGRADIENT_PAS_S}
{$define NSGRADIENT_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSGradient = objcclass;
  NSGradientPointer = ^NSGradient;
  NSGradientPtr = NSGradientPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSGRADIENT_PAS_C}
{$define NSGRADIENT_PAS_C}

{ NSGradient }
  NSGradient = objcclass external (NSObject, NSCopyingProtocol, NSCodingProtocol)
  private
    _colorArray: NSArray;
    _colorSpace: NSColorSpace;
    _functionRef: Pointer; {garbage collector: __strong }
    _componentArray: Pointer;
    _reserved1: Pointer;
    _reserved2: Pointer;
    _reserved3: Pointer;
    
  public
    function initWithStartingColor_endingColor(startingColor: NSColor; endingColor: NSColor): id; message 'initWithStartingColor:endingColor:';
    function initWithColors(colorArray: NSArray): id; message 'initWithColors:';
    function initWithColorsAndLocations(firstKey: id): id; varargs; message 'initWithColorsAndLocations:';
    function initWithColors_atLocations_colorSpace(colorArray: NSArray; locations: CGFloatPtr; colorSpace_: NSColorSpace): id; message 'initWithColors:atLocations:colorSpace:';
    procedure drawFromPoint_toPoint_options(startingPoint: NSPoint; endingPoint: NSPoint; options: NSGradientDrawingOptions); message 'drawFromPoint:toPoint:options:';
    procedure drawInRect_angle(rect: NSRect; angle: CGFloat); message 'drawInRect:angle:';
    procedure drawInBezierPath_angle(path: NSBezierPath; angle: CGFloat); message 'drawInBezierPath:angle:';
    procedure drawFromCenter_radius_toCenter_radius_options(startCenter: NSPoint; startRadius: CGFloat; endCenter: NSPoint; endRadius: CGFloat; options: NSGradientDrawingOptions); message 'drawFromCenter:radius:toCenter:radius:options:';
    procedure drawInRect_relativeCenterPosition(rect: NSRect; relativeCenterPosition: NSPoint); message 'drawInRect:relativeCenterPosition:';
    procedure drawInBezierPath_relativeCenterPosition(path: NSBezierPath; relativeCenterPosition: NSPoint); message 'drawInBezierPath:relativeCenterPosition:';
    function colorSpace: NSColorSpace; message 'colorSpace';
    function numberOfColorStops: NSInteger; message 'numberOfColorStops';
    procedure getColor_location_atIndex(color: NSColorPointer; location: CGFloatPtr; index: NSInteger); message 'getColor:location:atIndex:';
    function interpolatedColorAtLocation(location: CGFloat): NSColor; message 'interpolatedColorAtLocation:';

    { Adopted Protocols }
    function copyWithZone(zone_: NSZonePtr): id;
    procedure encodeWithCoder(aCoder: NSCoder);
    function initWithCoder(aDecoder: NSCoder): id;
  end;

{$endif}
{$endif}
