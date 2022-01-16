{ Parsed from CoreImage.framework CIContext.h }


{$ifdef TYPES}
type
  CIContextPtr = ^CIContext;
{$endif}

{$ifdef CLASSES}

type
  CIContext = objcclass external (NSObject)
  private
    _priv: id;
  public
    class function contextWithCGLContext_pixelFormat_colorSpace_options (ctx: CGLContextObj; pf: CGLPixelFormatObj; cs: CGColorSpaceRef; dict: NSDictionary): CIContext; message 'contextWithCGLContext:pixelFormat:colorSpace:options:';
    class function contextWithCGLContext_pixelFormat_options (ctx: CGLContextObj; pf: CGLPixelFormatObj; dict: NSDictionary): CIContext; message 'contextWithCGLContext:pixelFormat:options:'; deprecated 'available in 10.4 and later but deprecated in 10.6';
    class function contextWithCGContext_options (ctx: CGContextRef; dict: NSDictionary): CIContext; message 'contextWithCGContext:options:';
    class function offlineGPUCount: cuint; message 'offlineGPUCount';
    class function contextForOfflineGPUAtIndex (index: cuint): CIContext; message 'contextForOfflineGPUAtIndex:';
    class function contextForOfflineGPUAtIndex_colorSpace_options_sharedContext (index: cuint; colorSpace: CGColorSpaceRef; options: NSDictionary; sharedContext: CGLContextObj): CIContext; message 'contextForOfflineGPUAtIndex:colorSpace:options:sharedContext:';
    procedure drawImage_atPoint_fromRect (im: CIImage; p: CGPoint; src: CGRect); message 'drawImage:atPoint:fromRect:'; deprecated 'available in 10.4 and later but deprecated in 10.8';
    procedure drawImage_inRect_fromRect (im: CIImage; dest: CGRect; src: CGRect); message 'drawImage:inRect:fromRect:';
    function createCGImage_fromRect (im: CIImage; r: CGRect): CGImageRef; message 'createCGImage:fromRect:';
    function createCGImage_fromRect_format_colorSpace (im: CIImage; r: CGRect; f: CIFormat; cs: CGColorSpaceRef): CGImageRef; message 'createCGImage:fromRect:format:colorSpace:';
    function createCGLayerWithSize_info (size: CGSize; d: CFDictionaryRef): CGLayerRef; message 'createCGLayerWithSize:info:';
    procedure render_toBitmap_rowBytes_bounds_format_colorSpace (im: CIImage; data: pointer; rb: ptrdiff_t; r: CGRect; f: CIFormat; cs: CGColorSpaceRef); message 'render:toBitmap:rowBytes:bounds:format:colorSpace:';
    procedure render_toIOSurface_bounds_colorSpace (im: CIImage; surface: IOSurfaceRef; r: CGRect; cs: CGColorSpaceRef); message 'render:toIOSurface:bounds:colorSpace:';
    procedure reclaimResources; message 'reclaimResources';
    procedure clearCaches; message 'clearCaches';
  end;
{$endif}

