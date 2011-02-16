{ Parsed from Appkit.framework NSOpenGLView.h }

{$ifdef TYPES}
{$ifndef NSOPENGLVIEW_PAS_T}
{$define NSOPENGLVIEW_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSOPENGLVIEW_PAS_R}
{$define NSOPENGLVIEW_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSOPENGLVIEW_PAS_F}
{$define NSOPENGLVIEW_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSOPENGLVIEW_PAS_S}
{$define NSOPENGLVIEW_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSOpenGLView = objcclass;
  NSOpenGLViewPointer = ^NSOpenGLView;
  NSOpenGLViewPtr = NSOpenGLViewPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSOPENGLVIEW_PAS_C}
{$define NSOPENGLVIEW_PAS_C}

{ NSOpenGLView }
  NSOpenGLView = objcclass external (NSView)
  private
    _openGLContext: NSOpenGLContext;
    _pixelFormat: NSOpenGLPixelFormat;
    _reserved1: NSInteger;
    _reserved2: NSInteger;
    _reserved3: NSInteger;
    
  public
    class function defaultPixelFormat: NSOpenGLPixelFormat; message 'defaultPixelFormat';
    function initWithFrame_pixelFormat(frameRect: NSRect; format: NSOpenGLPixelFormat): id; message 'initWithFrame:pixelFormat:';
    procedure setOpenGLContext(context: NSOpenGLContext); message 'setOpenGLContext:';
    function openGLContext: NSOpenGLContext; message 'openGLContext';
    procedure clearGLContext; message 'clearGLContext';
    procedure update; message 'update';
    procedure reshape; message 'reshape';
    procedure setPixelFormat(pixelFormat_: NSOpenGLPixelFormat); message 'setPixelFormat:';
    function pixelFormat: NSOpenGLPixelFormat; message 'pixelFormat';
    procedure prepareOpenGL; message 'prepareOpenGL';
  end;

{$endif}
{$endif}
