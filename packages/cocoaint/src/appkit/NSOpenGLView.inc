{ Parsed from AppKit.framework NSOpenGLView.h }


{$ifdef TYPES}
type
  NSOpenGLViewPtr = ^NSOpenGLView;
{$endif}

{$ifdef CLASSES}

type
  NSOpenGLView = objcclass external (NSView)
  private
    _openGLContext: NSOpenGLContext;
    _pixelFormat: NSOpenGLPixelFormat;
    _reserved1: NSInteger;
    _reserved2: NSInteger;
    _reserved3: NSInteger;
  public
    class function defaultPixelFormat: NSOpenGLPixelFormat; message 'defaultPixelFormat';
    function initWithFrame_pixelFormat (frameRect: NSRect; format: NSOpenGLPixelFormat): instancetype; message 'initWithFrame:pixelFormat:';
    procedure setOpenGLContext(newValue: NSOpenGLContext); message 'setOpenGLContext:';
    function openGLContext: NSOpenGLContext; message 'openGLContext';
    procedure clearGLContext; message 'clearGLContext';
    procedure update; message 'update';
    procedure reshape; message 'reshape';
    procedure setPixelFormat(newValue: NSOpenGLPixelFormat); message 'setPixelFormat:';
    function pixelFormat: NSOpenGLPixelFormat; message 'pixelFormat';
    procedure prepareOpenGL; message 'prepareOpenGL';
  end;


type
  NSOpenGLSurfaceResolution = objccategory external (NSView)
    procedure setWantsBestResolutionOpenGLSurface(newValue: ObjCBOOL); message 'setWantsBestResolutionOpenGLSurface:';
    function wantsBestResolutionOpenGLSurface: ObjCBOOL; message 'wantsBestResolutionOpenGLSurface';
  end;
{$endif}

