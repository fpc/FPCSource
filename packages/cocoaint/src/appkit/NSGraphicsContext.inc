{ Parsed from AppKit.framework NSGraphicsContext.h }


{$ifdef TYPES}
type
  NSGraphicsContextPtr = ^NSGraphicsContext;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSGraphicsContextDestinationAttributeName: NSString; cvar; external;
  NSGraphicsContextRepresentationFormatAttributeName: NSString; cvar; external;
  NSGraphicsContextPSFormat: NSString; cvar; external;
  NSGraphicsContextPDFFormat: NSString; cvar; external;
{$endif}

{$ifdef TYPES}
type
  NSImageInterpolation = NSUInteger;
  NSImageInterpolationPtr = ^NSImageInterpolation;

const
  NSImageInterpolationDefault = 0;
  NSImageInterpolationNone = 1;
  NSImageInterpolationLow = 2;
  NSImageInterpolationMedium = 4 { available in 10_6 };
  NSImageInterpolationHigh = 3;
{$endif}

{$ifdef CLASSES}

type
  NSGraphicsContext = objcclass external (NSObject)
  private
  public
    class function graphicsContextWithAttributes (attributes: NSDictionary): NSGraphicsContext; message 'graphicsContextWithAttributes:';
    class function graphicsContextWithWindow (window: NSWindow): NSGraphicsContext; message 'graphicsContextWithWindow:';
    class function graphicsContextWithBitmapImageRep (bitmapRep: NSBitmapImageRep): NSGraphicsContext; message 'graphicsContextWithBitmapImageRep:';
    class function graphicsContextWithCGContext_flipped (graphicsPort: CGContextRef; initialFlippedState: ObjCBOOL): NSGraphicsContext; message 'graphicsContextWithCGContext:flipped:'; { available in 10_10 }
    class function currentContext: NSGraphicsContext; message 'currentContext';
    class procedure setCurrentContext (context: NSGraphicsContext); message 'setCurrentContext:';
    class function currentContextDrawingToScreen: ObjCBOOL; message 'currentContextDrawingToScreen';
    class procedure classSaveGraphicsState; message 'saveGraphicsState';
    class procedure classRestoreGraphicsState; message 'restoreGraphicsState';
    function attributes: NSDictionary; message 'attributes';
    function isDrawingToScreen: ObjCBOOL; message 'isDrawingToScreen';
    procedure saveGraphicsState; message 'saveGraphicsState';
    procedure restoreGraphicsState; message 'restoreGraphicsState';
    procedure flushGraphics; message 'flushGraphics';
    function CGContext: CGContextRef; message 'CGContext';
    function isFlipped: ObjCBOOL; message 'isFlipped';
  end;


type
  NSGraphicsContext_RenderingOptions = objccategory external (NSGraphicsContext)
    procedure setShouldAntialias(newValue: ObjCBOOL); message 'setShouldAntialias:';
    function shouldAntialias: ObjCBOOL; message 'shouldAntialias';
    procedure setImageInterpolation(newValue: NSImageInterpolation); message 'setImageInterpolation:';
    function imageInterpolation: NSImageInterpolation; message 'imageInterpolation';
    procedure setPatternPhase(newValue: NSPoint); message 'setPatternPhase:';
    function patternPhase: NSPoint; message 'patternPhase';
    procedure setCompositingOperation(newValue: NSCompositingOperation); message 'setCompositingOperation:';
    function compositingOperation: NSCompositingOperation; message 'compositingOperation';
    procedure setColorRenderingIntent(newValue: NSColorRenderingIntent); message 'setColorRenderingIntent:';
    function colorRenderingIntent: NSColorRenderingIntent; message 'colorRenderingIntent';
  end;

type
  NSGraphicsContext_NSQuartzCoreAdditions = objccategory external name 'NSQuartzCoreAdditions' (NSGraphicsContext)
    function CIContext_: CIContext; message 'CIContext';
  end;

type
  NSGraphicsContextDeprecated = objccategory external (NSGraphicsContext)
    class procedure setGraphicsState (gState: NSInteger); message 'setGraphicsState:'; deprecated 'in 10_0, 10_10';
    function focusStack: id; message 'focusStack'; deprecated 'in 10_0, 10_6';
    procedure setFocusStack (stack: id); message 'setFocusStack:'; deprecated 'in 10_0, 10_6';
    class function graphicsContextWithGraphicsPort_flipped (graphicsPort: pointer; initialFlippedState: ObjCBOOL): NSGraphicsContext; message 'graphicsContextWithGraphicsPort:flipped:';
    function graphicsPort: pointer; message 'graphicsPort';
  end;
{$endif}

