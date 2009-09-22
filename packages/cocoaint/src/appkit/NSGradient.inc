{ Parsed from Appkit.framework NSGradient.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Thu Sep 17 14:52:47 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSGRADIENT_PAS_H}
{$define NSGRADIENT_PAS_H}
type
  NSGradientPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSGRADIENT_PAS_T}
{$define NSGRADIENT_PAS_T}

{ Types }
type
  NSGradientDrawingOptions = NSUInteger;

{ Constants }

const
  NSGradientDrawsBeforeStartingLocation = 1  shl  0;
  NSGradientDrawsAfterEndingLocation = 1  shl  1;

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
{$ifndef NSGRADIENT_PAS_T}
{$define NSGRADIENT_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSGradient = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSGRADIENT_PAS_C}
{$define NSGRADIENT_PAS_C}

{ NSGradient }
  NSGradient = objcclass(NSObject, NSCopyingProtocol, NSCodingProtocol)
  private
    __colorArray: NSArray;
    __colorSpace: NSColorSpace;
    __functionRef: Pointer; {garbage collector: __strong }
    __componentArray: Pointer;
    __reserved1: Pointer;
    __reserved2: Pointer;
    __reserved3: Pointer;
    
  public
    class function alloc: NSGradient; message 'alloc';

    function initWithStartingColor_endingColor(startingColor: NSColor; endingColor: NSColor): id; message 'initWithStartingColor:endingColor:';
    function initWithColors(colorArray: NSArray): id; message 'initWithColors:';
    function initWithColorsAndLocations(firstColor: NSColor): id; varargs; message 'initWithColorsAndLocations:';
    function initWithColors_atLocations_colorSpace(colorArray: NSArray; locations: psingle; colorSpace_: NSColorSpace): id; message 'initWithColors:atLocations:colorSpace:';
    procedure drawFromPoint_toPoint_options(startingPoint: NSPoint; endingPoint: NSPoint; options: NSGradientDrawingOptions); message 'drawFromPoint:toPoint:options:';
    procedure drawInRect_angle(rect: NSRect; angle: CGFloat); message 'drawInRect:angle:';
    procedure drawInBezierPath_angle(path: NSBezierPath; angle: CGFloat); message 'drawInBezierPath:angle:';
    procedure drawFromCenter_radius_toCenter_radius_options(startCenter: NSPoint; startRadius: CGFloat; endCenter: NSPoint; endRadius: CGFloat; options: NSGradientDrawingOptions); message 'drawFromCenter:radius:toCenter:radius:options:';
    procedure drawInRect_relativeCenterPosition(rect: NSRect; relativeCenterPosition: NSPoint); message 'drawInRect:relativeCenterPosition:';
    procedure drawInBezierPath_relativeCenterPosition(path: NSBezierPath; relativeCenterPosition: NSPoint); message 'drawInBezierPath:relativeCenterPosition:';
    function colorSpace: NSColorSpace; message 'colorSpace';
    function numberOfColorStops: NSInteger; message 'numberOfColorStops';
    procedure getColor_location_atIndex(color: NSColorPointer; location: psingle; index: NSInteger); message 'getColor:location:atIndex:';
    function interpolatedColorAtLocation(location: CGFloat): NSColor; message 'interpolatedColorAtLocation:';
  end; external;

{$endif}
{$endif}
