{ Parsed from Quartzcore.framework CIContext.h }

{$ifdef TYPES}
{$ifndef CICONTEXT_PAS_T}
{$define CICONTEXT_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef CICONTEXT_PAS_R}
{$define CICONTEXT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef CICONTEXT_PAS_F}
{$define CICONTEXT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef CICONTEXT_PAS_S}
{$define CICONTEXT_PAS_S}

{ External string constants }
var
  kCIContextOutputColorSpace: NSString; cvar; external;
  kCIContextWorkingColorSpace: NSString; cvar; external;
  kCIContextUseSoftwareRenderer: NSString; cvar; external;

{$endif}
{$endif}

{$ifdef FORWARD}
  CIContext = objcclass;
  CIContextPointer = ^CIContext;
  CIContextPtr = CIContextPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef CICONTEXT_PAS_C}
{$define CICONTEXT_PAS_C}

{ CIContext }
  CIContext = objcclass external (NSObject)
  private
    _priv: id;
    
  public
    class function contextWithCGLContext_pixelFormat_colorSpace_options(ctx: CGLContextObj; pf: CGLPixelFormatObj; cs: CGColorSpaceRef; dict: NSDictionary): CIContext; message 'contextWithCGLContext:pixelFormat:colorSpace:options:';
    class function contextWithCGLContext_pixelFormat_options(ctx: CGLContextObj; pf: CGLPixelFormatObj; dict: NSDictionary): CIContext; message 'contextWithCGLContext:pixelFormat:options:'; deprecated 'in Mac OS X 10.6 and later';
    class function contextWithCGContext_options(ctx: CGContextRef; dict: NSDictionary): CIContext; message 'contextWithCGContext:options:';
    procedure drawImage_atPoint_fromRect(im: CIImage; p: CGPoint; src: CGRect); message 'drawImage:atPoint:fromRect:';
    procedure drawImage_inRect_fromRect(im: CIImage; dest: CGRect; src: CGRect); message 'drawImage:inRect:fromRect:';
    function createCGImage_fromRect(im: CIImage; r: CGRect): CGImageRef; message 'createCGImage:fromRect:';
    function createCGImage_fromRect_format_colorSpace(im: CIImage; r: CGRect; f: CIFormat; cs: CGColorSpaceRef): CGImageRef; message 'createCGImage:fromRect:format:colorSpace:';
    function createCGLayerWithSize_info(size: CGSize; d: CFDictionaryRef): CGLayerRef; message 'createCGLayerWithSize:info:';
    procedure render_toBitmap_rowBytes_bounds_format_colorSpace(im: CIImage; data: Pointer; rb: ptrdiff_t; r: CGRect; f: CIFormat; cs: CGColorSpaceRef); message 'render:toBitmap:rowBytes:bounds:format:colorSpace:';
    procedure render_toIOSurface_bounds_colorSpace(im: CIImage; surface: IOSurfaceRef; r: CGRect; cs: CGColorSpaceRef); message 'render:toIOSurface:bounds:colorSpace:';
    procedure reclaimResources; message 'reclaimResources';
    procedure clearCaches; message 'clearCaches';
  end;

{$endif}
{$endif}
