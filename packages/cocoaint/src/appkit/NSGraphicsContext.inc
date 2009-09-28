{ Parsed from Appkit.framework NSGraphicsContext.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Mon Sep 28 15:49:52 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSGRAPHICSCONTEXT_PAS_H}
{$define NSGRAPHICSCONTEXT_PAS_H}
type
  NSGraphicsContextPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSGRAPHICSCONTEXT_PAS_T}
{$define NSGRAPHICSCONTEXT_PAS_T}

{ Constants }

const
  NSImageInterpolationDefault = 0;
  NSImageInterpolationNone = 1;
  NSImageInterpolationLow = 2;
  NSImageInterpolationHigh = 3;

const
  NSColorRenderingIntentDefault = 0;
  NSColorRenderingIntentAbsoluteColorimetric = 1;
  NSColorRenderingIntentRelativeColorimetric = 2;
  NSColorRenderingIntentPerceptual = 3;
  NSColorRenderingIntentSaturation = 4;

{ Types }
type
  NSImageInterpolation = NSUInteger;
  NSColorRenderingIntent = NSInteger;

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

{$endif}
{$endif}

{$ifdef FORWARD}
  NSGraphicsContext = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSGRAPHICSCONTEXT_PAS_C}
{$define NSGRAPHICSCONTEXT_PAS_C}

{ NSGraphicsContext }
  NSGraphicsContext = objcclass(NSObject)
    
  public
    class function alloc: NSGraphicsContext; message 'alloc';

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
    function focusStack: id; message 'focusStack';
    procedure setFocusStack(stack: id); message 'setFocusStack:';
    function graphicsPort: Pointer; message 'graphicsPort';
    function isFlipped: Boolean; message 'isFlipped';

    { Category: NSQuartzCoreAdditions }
    function CIContext: CIContext; message 'CIContext';
  end; external;

{$endif}
{$endif}
