{ Parsed from AppKit.framework NSOpenGLLayer.h }


{$ifdef TYPES}
type
  NSOpenGLLayerPtr = ^NSOpenGLLayer;
{$endif}

{$ifdef CLASSES}

type
  NSOpenGLLayer = objcclass external (CAOpenGLLayer)
  private
    _openGLPixelFormat: NSOpenGLPixelFormat;
    _openGLContext: NSOpenGLContext;
    _reserved:array[0..4] of pointer;
  public
    procedure setView(newValue: NSView); message 'setView:';
    function view: NSView; message 'view';
    procedure setOpenGLPixelFormat(newValue: NSOpenGLPixelFormat); message 'setOpenGLPixelFormat:';
    function openGLPixelFormat: NSOpenGLPixelFormat; message 'openGLPixelFormat';
    procedure setOpenGLContext(newValue: NSOpenGLContext); message 'setOpenGLContext:';
    function openGLContext: NSOpenGLContext; message 'openGLContext';
    function openGLPixelFormatForDisplayMask (mask: cuint32): NSOpenGLPixelFormat; message 'openGLPixelFormatForDisplayMask:';
    function openGLContextForPixelFormat (pixelFormat: NSOpenGLPixelFormat): NSOpenGLContext; message 'openGLContextForPixelFormat:';
    function canDrawInOpenGLContext_pixelFormat_forLayerTime_displayTime (context: NSOpenGLContext; pixelFormat: NSOpenGLPixelFormat; t: CFTimeInterval; ts: CVTimeStampPtr): ObjCBOOL; message 'canDrawInOpenGLContext:pixelFormat:forLayerTime:displayTime:';
    procedure drawInOpenGLContext_pixelFormat_forLayerTime_displayTime (context: NSOpenGLContext; pixelFormat: NSOpenGLPixelFormat; t: CFTimeInterval; ts: CVTimeStampPtr); message 'drawInOpenGLContext:pixelFormat:forLayerTime:displayTime:';
  end;
{$endif}

