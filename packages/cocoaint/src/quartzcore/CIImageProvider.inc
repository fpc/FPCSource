{ Parsed from Quartzcore.framework CIImageProvider.h }

{$ifdef TYPES}
{$ifndef CIIMAGEPROVIDER_PAS_T}
{$define CIIMAGEPROVIDER_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef CIIMAGEPROVIDER_PAS_R}
{$define CIIMAGEPROVIDER_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef CIIMAGEPROVIDER_PAS_F}
{$define CIIMAGEPROVIDER_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef CIIMAGEPROVIDER_PAS_S}
{$define CIIMAGEPROVIDER_PAS_S}

{ External string constants }
var
  kCIImageProviderTileSize: NSString; cvar; external;
  kCIImageProviderUserInfo: NSString; cvar; external;

{$endif}
{$endif}

{$ifdef FORWARD}

{$endif}

{$ifdef CLASSES}
{$ifndef CIIMAGEPROVIDER_PAS_C}
{$define CIIMAGEPROVIDER_PAS_C}

{ CIImageProviderCategory }
  CIImageProviderCategory = objccategory external (CIImage)
    class function imageWithImageProvider_size_width_format_colorSpace_options(p: id; width: size_t; height: size_t; f: CIFormat; cs: CGColorSpaceRef; dict: NSDictionary): CIImage; message 'imageWithImageProvider:size:width:format:colorSpace:options:';
    function initWithImageProvider_size_width_format_colorSpace_options(p: id; width: size_t; height: size_t; f: CIFormat; cs: CGColorSpaceRef; dict: NSDictionary): id; message 'initWithImageProvider:size:width:format:colorSpace:options:';
  end;

{ CIImageProvider_NSObjectCategory }
  CIImageProvider_NSObjectCategory = objccategory external name 'CIImageProvider' (NSObject)
    procedure provideImageData_bytesPerRow_origin_x_size_width_userInfo(data: Pointer; rowbytes: size_t; x: size_t; y: size_t; width: size_t; height: size_t; info: id); message 'provideImageData:bytesPerRow:origin:x:size:width:userInfo:';
  end;

{$endif}
{$endif}
