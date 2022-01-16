{ Parsed from CoreImage.framework CIImage.h }


{ Types from CIImage }
{$ifdef TYPES}

type
  CIFormat = cint;
type
  CIFormatPtr = ^CIFormat;
{$endif}


{$ifdef TYPES}
type
  CIImagePtr = ^CIImage;
{$endif}

{$ifdef CLASSES}

type
  CIImage = objcclass external (NSObject, NSCodingProtocol, NSCopyingProtocol)
  private
    _state: pointer;
    _priv: pointer;
  public
    class function imageWithCGImage (image: CGImageRef): CIImage; message 'imageWithCGImage:';
    class function imageWithCGImage_options (image: CGImageRef; d: NSDictionary): CIImage; message 'imageWithCGImage:options:';
    class function imageWithCGLayer (layer: CGLayerRef): CIImage; message 'imageWithCGLayer:'; { available starting in __MAC_10_4, __IPHONE_NA }
    class function imageWithCGLayer_options (layer: CGLayerRef; d: NSDictionary): CIImage; message 'imageWithCGLayer:options:'; { available starting in __MAC_10_4, __IPHONE_NA }
    class function imageWithBitmapData_bytesPerRow_size_format_colorSpace (d: NSData; bpr: size_t; size: CGSize; f: CIFormat; cs: CGColorSpaceRef): CIImage; message 'imageWithBitmapData:bytesPerRow:size:format:colorSpace:';
    class function imageWithTexture_size_flipped_colorSpace (name: cuint; size: CGSize; flag: ObjCBOOL; cs: CGColorSpaceRef): CIImage; message 'imageWithTexture:size:flipped:colorSpace:'; { available starting in __MAC_10_4, __IPHONE_6_0 }
    class function imageWithTexture_size_flipped_options (name: cuint; size: CGSize; flag: ObjCBOOL; options: NSDictionary): CIImage; message 'imageWithTexture:size:flipped:options:'; { available starting in __MAC_10_9,__IPHONE_NA }
    class function imageWithContentsOfURL (url: NSURL): CIImage; message 'imageWithContentsOfURL:';
    class function imageWithContentsOfURL_options (url: NSURL; d: NSDictionary): CIImage; message 'imageWithContentsOfURL:options:';
    class function imageWithData (data: NSData): CIImage; message 'imageWithData:';
    class function imageWithData_options (data: NSData; d: NSDictionary): CIImage; message 'imageWithData:options:';
    class function imageWithCVImageBuffer (imageBuffer: CVImageBufferRef): CIImage; message 'imageWithCVImageBuffer:'; { available starting in __MAC_10_4, __IPHONE_NA }
    class function imageWithCVImageBuffer_options (imageBuffer: CVImageBufferRef; dict: NSDictionary): CIImage; message 'imageWithCVImageBuffer:options:'; { available starting in __MAC_10_4, __IPHONE_NA }
    class function imageWithCVPixelBuffer (buffer: CVPixelBufferRef): CIImage; message 'imageWithCVPixelBuffer:'; { available starting in __MAC_NA, __IPHONE_5_0 }
    class function imageWithCVPixelBuffer_options (buffer: CVPixelBufferRef; dict: NSDictionary): CIImage; message 'imageWithCVPixelBuffer:options:'; { available starting in __MAC_NA, __IPHONE_5_0 }
    class function imageWithIOSurface (surface: IOSurfaceRef): CIImage; message 'imageWithIOSurface:';
    class function imageWithIOSurface_options (surface: IOSurfaceRef; d: NSDictionary): CIImage; message 'imageWithIOSurface:options:';
    class function imageWithColor (color: CIColor): CIImage; message 'imageWithColor:';
    class function emptyImage: CIImage; message 'emptyImage';
    function initWithCGImage (image: CGImageRef): id; message 'initWithCGImage:';
    function initWithCGImage_options (image: CGImageRef; d: NSDictionary): id; message 'initWithCGImage:options:';
    function initWithCGLayer (layer: CGLayerRef): id; message 'initWithCGLayer:'; { available starting in __MAC_10_4, __IPHONE_NA }
    function initWithCGLayer_options (layer: CGLayerRef; d: NSDictionary): id; message 'initWithCGLayer:options:'; { available starting in __MAC_10_4, __IPHONE_NA }
    function initWithData (data: NSData): id; message 'initWithData:';
    function initWithData_options (data: NSData; d: NSDictionary): id; message 'initWithData:options:';
    function initWithBitmapData_bytesPerRow_size_format_colorSpace (d: NSData; bpr: size_t; size: CGSize; f: CIFormat; c: CGColorSpaceRef): id; message 'initWithBitmapData:bytesPerRow:size:format:colorSpace:';
    function initWithTexture_size_flipped_colorSpace (name: cuint; size: CGSize; flag: ObjCBOOL; cs: CGColorSpaceRef): id; message 'initWithTexture:size:flipped:colorSpace:'; { available starting in __MAC_10_4, __IPHONE_6_0 }
    function initWithTexture_size_flipped_options (name: cuint; size: CGSize; flag: ObjCBOOL; options: NSDictionary): id; message 'initWithTexture:size:flipped:options:'; { available starting in __MAC_10_9, __IPHONE_NA }
    function initWithContentsOfURL (url: NSURL): id; message 'initWithContentsOfURL:';
    function initWithContentsOfURL_options (url: NSURL; d: NSDictionary): id; message 'initWithContentsOfURL:options:';
    function initWithIOSurface (surface: IOSurfaceRef): id; message 'initWithIOSurface:'; { available starting in __MAC_10_6, __IPHONE_NA }
    function initWithIOSurface_options (surface: IOSurfaceRef; d: NSDictionary): id; message 'initWithIOSurface:options:'; { available starting in __MAC_10_6, __IPHONE_NA }
    function initWithIOSurface_plane_format_options (surface: IOSurfaceRef; plane: size_t; format: CIFormat; d: NSDictionary): id; message 'initWithIOSurface:plane:format:options:';
    function initWithCVImageBuffer (imageBuffer: CVImageBufferRef): id; message 'initWithCVImageBuffer:'; { available starting in __MAC_10_4, __IPHONE_NA }
    function initWithCVImageBuffer_options (imageBuffer: CVImageBufferRef; dict: NSDictionary): id; message 'initWithCVImageBuffer:options:'; { available starting in __MAC_10_4, __IPHONE_NA }
    function initWithCVPixelBuffer (buffer: CVPixelBufferRef): id; message 'initWithCVPixelBuffer:'; { available starting in __MAC_NA, __IPHONE_5_0 }
    function initWithCVPixelBuffer_options (buffer: CVPixelBufferRef; dict: NSDictionary): id; message 'initWithCVPixelBuffer:options:'; { available starting in __MAC_NA, __IPHONE_5_0 }
    function initWithColor (color: CIColor): id; message 'initWithColor:';
    function imageByApplyingTransform (matrix: CGAffineTransform): CIImage; message 'imageByApplyingTransform:';
    function imageByApplyingOrientation (orientation: cint): CIImage; message 'imageByApplyingOrientation:'; { available starting in __MAC_10_10, __IPHONE_8_0 }
    function imageTransformForOrientation (orientation: cint): CGAffineTransform; message 'imageTransformForOrientation:'; { available starting in __MAC_10_10, __IPHONE_8_0 }
    function imageByCompositingOverImage (dest: CIImage): CIImage; message 'imageByCompositingOverImage:'; { available starting in __MAC_10_4, __IPHONE_8_0 }
    function imageByCroppingToRect (r: CGRect): CIImage; message 'imageByCroppingToRect:';
    function imageByClampingToExtent: CIImage; message 'imageByClampingToExtent'; { available starting in __MAC_10_10, __IPHONE_8_0 }
    function extent: CGRect; message 'extent';
    function imageByApplyingFilter_withInputParameters (filterName: NSString; params: NSDictionary): CIImage; message 'imageByApplyingFilter:withInputParameters:'; { available starting in __MAC_10_10, __IPHONE_8_0 }
    function properties: NSDictionary; message 'properties'; { available starting in __MAC_10_8, __IPHONE_5_0 }
    function definition: CIFilterShape; message 'definition'; { available starting in __MAC_10_4, __IPHONE_NA }
    function url: NSURL; message 'url'; { available starting in __MAC_10_4, __IPHONE_NA }
    function colorSpace: CGColorSpaceRef; message 'colorSpace'; { available starting in __MAC_10_4, __IPHONE_NA }

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
  end;


type
  AutoAdjustment = objccategory external (CIImage)
    function autoAdjustmentFilters: NSArray; message 'autoAdjustmentFilters'; { available starting in __MAC_10_8, __IPHONE_5_0 }
    function autoAdjustmentFiltersWithOptions (dict: NSDictionary): NSArray; message 'autoAdjustmentFiltersWithOptions:'; { available starting in __MAC_10_8, __IPHONE_5_0 }
  end;
{$endif}

