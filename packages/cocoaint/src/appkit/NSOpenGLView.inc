{ Parsed from Appkit.framework NSOpenGLView.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Mon Sep 28 15:49:53 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSOPENGLVIEW_PAS_H}
{$define NSOPENGLVIEW_PAS_H}
type
  NSOpenGLViewPointer = Pointer;

{$endif}
{$endif}

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

{$endif}

{$ifdef CLASSES}
{$ifndef NSOPENGLVIEW_PAS_C}
{$define NSOPENGLVIEW_PAS_C}

{ NSOpenGLView }
  NSOpenGLView = objcclass(NSView)
  private
    __openGLContext: NSOpenGLContext;
    __pixelFormat: NSOpenGLPixelFormat;
    __reserved1: NSInteger;
    __reserved2: NSInteger;
    __reserved3: NSInteger;
    
  public
    class function alloc: NSOpenGLView; message 'alloc';

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
  end; external;

{$endif}
{$endif}
