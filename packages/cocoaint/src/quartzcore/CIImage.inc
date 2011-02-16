{ Parsed from Quartzcore.framework CIImage.h }

{$ifdef TYPES}
{$ifndef CIIMAGE_PAS_T}
{$define CIIMAGE_PAS_T}

{ Types }
type
  CIFormat = cint;
  CIFormatPtr = ^CIFormat;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef CIIMAGE_PAS_R}
{$define CIIMAGE_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef CIIMAGE_PAS_F}
{$define CIIMAGE_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef CIIMAGE_PAS_S}
{$define CIIMAGE_PAS_S}

{ External symbols }
var
  kCIFormatARGB8: CIFormat; cvar; external;
  kCIFormatRGBA16: CIFormat; cvar; external;
  kCIFormatRGBAf: CIFormat; cvar; external;

{ External string constants }
var
  kCIImageColorSpace: NSString; cvar; external;

{$endif}
{$endif}

{$ifdef FORWARD}
  CIImage = objcclass;
  CIImagePointer = ^CIImage;
  CIImagePtr = CIImagePointer;

{$endif}

{$ifdef CLASSES}
{$ifndef CIIMAGE_PAS_C}
{$define CIIMAGE_PAS_C}

{ CIImage }
  CIImage = objcclass external (NSObject, NSCodingProtocol, NSCopyingProtocol)
  private
    _state: Pointer;
    _priv: Pointer;
    
  public
    class function imageWithCGImage(image: CGImageRef): CIImage; message 'imageWithCGImage:';
    class function imageWithCGImage_options(image: CGImageRef; d: NSDictionary): CIImage; message 'imageWithCGImage:options:';
    class function imageWithCGLayer(layer: CGLayerRef): CIImage; message 'imageWithCGLayer:';
    class function imageWithCGLayer_options(layer: CGLayerRef; d: NSDictionary): CIImage; message 'imageWithCGLayer:options:';
    class function imageWithBitmapData_bytesPerRow_size_format_colorSpace(d: NSData; bpr: size_t; size: CGSize; f: CIFormat; cs: CGColorSpaceRef): CIImage; message 'imageWithBitmapData:bytesPerRow:size:format:colorSpace:';
    class function imageWithTexture_size_flipped_colorSpace(name: cuint; size: CGSize; flag: Boolean; cs: CGColorSpaceRef): CIImage; message 'imageWithTexture:size:flipped:colorSpace:';
    class function imageWithContentsOfURL(url_: NSURL): CIImage; message 'imageWithContentsOfURL:';
    class function imageWithContentsOfURL_options(url_: NSURL; d: NSDictionary): CIImage; message 'imageWithContentsOfURL:options:';
    class function imageWithData(data: NSData): CIImage; message 'imageWithData:';
    class function imageWithData_options(data: NSData; d: NSDictionary): CIImage; message 'imageWithData:options:';
    class function imageWithCVImageBuffer(imageBuffer: CVImageBufferRef): CIImage; message 'imageWithCVImageBuffer:';
    class function imageWithCVImageBuffer_options(imageBuffer: CVImageBufferRef; dict: NSDictionary): CIImage; message 'imageWithCVImageBuffer:options:';
    class function imageWithIOSurface(surface: IOSurfaceRef): CIImage; message 'imageWithIOSurface:';
    class function imageWithIOSurface_options(surface: IOSurfaceRef; d: NSDictionary): CIImage; message 'imageWithIOSurface:options:';
    class function imageWithColor(color: CIColor): CIImage; message 'imageWithColor:';
    class function emptyImage: CIImage; message 'emptyImage';
    function initWithCGImage(image: CGImageRef): id; message 'initWithCGImage:';
    function initWithCGImage_options(image: CGImageRef; d: NSDictionary): id; message 'initWithCGImage:options:';
    function initWithCGLayer(layer: CGLayerRef): id; message 'initWithCGLayer:';
    function initWithCGLayer_options(layer: CGLayerRef; d: NSDictionary): id; message 'initWithCGLayer:options:';
    function initWithData(data: NSData): id; message 'initWithData:';
    function initWithData_options(data: NSData; d: NSDictionary): id; message 'initWithData:options:';
    function initWithBitmapData_bytesPerRow_size_format_colorSpace(d: NSData; bpr: size_t; size: CGSize; f: CIFormat; c: CGColorSpaceRef): id; message 'initWithBitmapData:bytesPerRow:size:format:colorSpace:';
    function initWithTexture_size_flipped_colorSpace(name: cuint; size: CGSize; flag: Boolean; cs: CGColorSpaceRef): id; message 'initWithTexture:size:flipped:colorSpace:';
    function initWithContentsOfURL(url_: NSURL): id; message 'initWithContentsOfURL:';
    function initWithContentsOfURL_options(url_: NSURL; d: NSDictionary): id; message 'initWithContentsOfURL:options:';
    function initWithIOSurface(surface: IOSurfaceRef): id; message 'initWithIOSurface:';
    function initWithIOSurface_options(surface: IOSurfaceRef; d: NSDictionary): id; message 'initWithIOSurface:options:';
    function initWithCVImageBuffer(imageBuffer: CVImageBufferRef): id; message 'initWithCVImageBuffer:';
    function initWithCVImageBuffer_options(imageBuffer: CVImageBufferRef; dict: NSDictionary): id; message 'initWithCVImageBuffer:options:';
    function initWithColor(color: CIColor): id; message 'initWithColor:';
    function imageByApplyingTransform(matrix: CGAffineTransform): CIImage; message 'imageByApplyingTransform:';
    function imageByCroppingToRect(r: CGRect): CIImage; message 'imageByCroppingToRect:';
    function extent: CGRect; message 'extent';
    function definition: CIFilterShape; message 'definition';
    function url: NSURL; message 'url';
    function colorSpace: CGColorSpaceRef; message 'colorSpace';

    { Adopted Protocols }
    procedure encodeWithCoder(aCoder: NSCoder);
    function initWithCoder(aDecoder: NSCoder): id;
    function copyWithZone(zone_: NSZonePtr): id;
  end;

{$endif}
{$endif}
