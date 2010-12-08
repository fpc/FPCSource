{ Parsed from Quartzcore.framework CAOpenGLLayer.h }
{ Version: 2.1.2 - Wed Dec 8 10:06:07 CET 2010 }


{$ifdef TYPES}
{$ifndef CAOPENGLLAYER_PAS_T}
{$define CAOPENGLLAYER_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef CAOPENGLLAYER_PAS_R}
{$define CAOPENGLLAYER_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef CAOPENGLLAYER_PAS_F}
{$define CAOPENGLLAYER_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef CAOPENGLLAYER_PAS_S}
{$define CAOPENGLLAYER_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  CAOpenGLLayer = objcclass;
  CAOpenGLLayerPointer = ^CAOpenGLLayer;
  CAOpenGLLayerPtr = CAOpenGLLayerPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef CAOPENGLLAYER_PAS_C}
{$define CAOPENGLLAYER_PAS_C}

{ CAOpenGLLayer }
  CAOpenGLLayer = objcclass(CALayer)
  private
    __glPriv: Pointer;
    
  public
    class function alloc: CAOpenGLLayer; message 'alloc';

    procedure setAsynchronous (newValue: Boolean); message 'setAsynchronous:';
    function isAsynchronous: Boolean; message 'isAsynchronous';
    function canDrawInCGLContext_pixelFormat_forLayerTime_displayTime(ctx: CGLContextObj; pf: CGLPixelFormatObj; t: CFTimeInterval; ts: CVTimeStampPtr): Boolean; message 'canDrawInCGLContext:pixelFormat:forLayerTime:displayTime:';
    procedure drawInCGLContext_pixelFormat_forLayerTime_displayTime(ctx: CGLContextObj; pf: CGLPixelFormatObj; t: CFTimeInterval; ts: CVTimeStampPtr); message 'drawInCGLContext:pixelFormat:forLayerTime:displayTime:';
    function copyCGLPixelFormatForDisplayMask(mask_: cuint32): CGLPixelFormatObj; message 'copyCGLPixelFormatForDisplayMask:';
    procedure releaseCGLPixelFormat(pf: CGLPixelFormatObj); message 'releaseCGLPixelFormat:';
    function copyCGLContextForPixelFormat(pf: CGLPixelFormatObj): CGLContextObj; message 'copyCGLContextForPixelFormat:';
    procedure releaseCGLContext(ctx: CGLContextObj); message 'releaseCGLContext:';
  end; external;

{$endif}
{$endif}
