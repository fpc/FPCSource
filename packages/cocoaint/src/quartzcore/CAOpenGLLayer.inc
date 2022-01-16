{ Parsed from QuartzCore.framework CAOpenGLLayer.h }


{$ifdef TYPES}
type
  CAOpenGLLayerPtr = ^CAOpenGLLayer;
{$endif}

{$ifdef CLASSES}

type
  CAOpenGLLayer = objcclass external (CALayer)
  private
    _glPriv: CAOpenGLLayerPrivatePtr;
  public
    procedure setAsynchronous(newValue: ObjCBOOL); message 'setAsynchronous:';
    function isAsynchronous: ObjCBOOL; message 'isAsynchronous';
    function canDrawInCGLContext_pixelFormat_forLayerTime_displayTime (ctx: CGLContextObj; pf: CGLPixelFormatObj; t: CFTimeInterval; ts: CVTimeStampPtr): ObjCBOOL; message 'canDrawInCGLContext:pixelFormat:forLayerTime:displayTime:';
    procedure drawInCGLContext_pixelFormat_forLayerTime_displayTime (ctx: CGLContextObj; pf: CGLPixelFormatObj; t: CFTimeInterval; ts: CVTimeStampPtr); message 'drawInCGLContext:pixelFormat:forLayerTime:displayTime:';
    function copyCGLPixelFormatForDisplayMask (mask_: cuint32): CGLPixelFormatObj; message 'copyCGLPixelFormatForDisplayMask:';
    procedure releaseCGLPixelFormat (pf: CGLPixelFormatObj); message 'releaseCGLPixelFormat:';
    function copyCGLContextForPixelFormat (pf: CGLPixelFormatObj): CGLContextObj; message 'copyCGLContextForPixelFormat:';
    procedure releaseCGLContext (ctx: CGLContextObj); message 'releaseCGLContext:';
  end;
{$endif}

