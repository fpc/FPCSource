{ Parsed from Appkit.framework NSGraphicsContext.h }

{$ifdef TYPES}
{$ifndef NSGRAPHICSCONTEXT_PAS_T}
{$define NSGRAPHICSCONTEXT_PAS_T}

{ Constants }

const
  NSImageInterpolationDefault = 0;
  NSImageInterpolationNone = 1;
  NSImageInterpolationLow = 2;
  NSImageInterpolationMedium = 4;
  NSImageInterpolationHigh = 3;

{ Types }
type
  NSImageInterpolation = NSUInteger;
  NSImageInterpolationPtr = ^NSImageInterpolation;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSGRAPHICSCONTEXT_PAS_R}
{$define NSGRAPHICSCONTEXT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSGRAPHICSCONTEXT_PAS_F}
{$define NSGRAPHICSCONTEXT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSGRAPHICSCONTEXT_PAS_S}
{$define NSGRAPHICSCONTEXT_PAS_S}

{ External string constants }
var
  NSGraphicsContextDestinationAttributeName: NSString; cvar; external;
  NSGraphicsContextRepresentationFormatAttributeName: NSString; cvar; external;
  NSGraphicsContextPSFormat: NSString; cvar; external;
  NSGraphicsContextPDFFormat: NSString; cvar; external;

{$endif}
{$endif}

{$ifdef FORWARD}
  NSGraphicsContext = objcclass;
  NSGraphicsContextPointer = ^NSGraphicsContext;
  NSGraphicsContextPtr = NSGraphicsContextPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSGRAPHICSCONTEXT_PAS_C}
{$define NSGRAPHICSCONTEXT_PAS_C}

{ NSGraphicsContext }
  NSGraphicsContext = objcclass external (NSObject)
    
  public
    class function graphicsContextWithAttributes(attributes_: NSDictionary): NSGraphicsContext; message 'graphicsContextWithAttributes:';
    class function graphicsContextWithWindow(window: NSWindow): NSGraphicsContext; message 'graphicsContextWithWindow:';
    class function graphicsContextWithBitmapImageRep(bitmapRep: NSBitmapImageRep): NSGraphicsContext; message 'graphicsContextWithBitmapImageRep:';
    class function graphicsContextWithGraphicsPort_flipped(graphicsPort_: Pointer; initialFlippedState: Boolean): NSGraphicsContext; message 'graphicsContextWithGraphicsPort:flipped:';
    class function currentContext: NSGraphicsContext; message 'currentContext';
    class procedure setCurrentContext(context: NSGraphicsContext); message 'setCurrentContext:';
    class function currentContextDrawingToScreen: Boolean; message 'currentContextDrawingToScreen';
    class procedure saveGraphicsState; message 'saveGraphicsState';
    class procedure restoreGraphicsState; message 'restoreGraphicsState';
    class procedure setGraphicsState(gState: NSInteger); message 'setGraphicsState:';
    function attributes: NSDictionary; message 'attributes';
    function isDrawingToScreen: Boolean; message 'isDrawingToScreen';
    procedure flushGraphics; message 'flushGraphics';
    function graphicsPort: Pointer; message 'graphicsPort';
    function isFlipped: Boolean; message 'isFlipped';
  end;

{ NSQuartzCoreAdditionsCategory }
  NSQuartzCoreAdditionsCategory = objccategory external (NSGraphicsContext)
    function CIContext: CIContext; message 'CIContext';
  end;

{ NSGraphicsContextDeprecatedCategory }
  NSGraphicsContextDeprecatedCategory = objccategory external (NSGraphicsContext)
    function focusStack: id; message 'focusStack';
    procedure setFocusStack(stack: id); message 'setFocusStack:';
  end;

{$endif}
{$endif}
