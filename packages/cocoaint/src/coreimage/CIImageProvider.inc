{ Parsed from CoreImage.framework CIImageProvider.h }

{$ifdef CLASSES}

type
  CIImage_CIImageProvider = objccategory external name 'CIImageProvider' (CIImage)
    class function imageWithImageProvider_size_format_colorSpace_options (p: id; width: size_t; f: CIFormat; cs: CGColorSpaceRef; dict: NSDictionary): CIImage; message 'imageWithImageProvider:size:format:colorSpace:options:';
    function initWithImageProvider_size_format_colorSpace_options (p: id; width: size_t; f: CIFormat; cs: CGColorSpaceRef; dict: NSDictionary): id; message 'initWithImageProvider:size:format:colorSpace:options:';
  end;

type
  CIImageProvider = objccategory external (NSObject)
    procedure provideImageData_bytesPerRow_origin_size_userInfo (data: pointer; rowbytes: size_t; x: size_t; width: size_t; info: id); message 'provideImageData:bytesPerRow:origin:size:userInfo:';
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  kCIImageProviderTileSize: NSString; cvar; external;
  kCIImageProviderUserInfo: NSString; cvar; external;
{$endif}

