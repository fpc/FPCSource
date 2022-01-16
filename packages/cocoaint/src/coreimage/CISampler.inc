{ Parsed from CoreImage.framework CISampler.h }


{$ifdef TYPES}
type
  CISamplerPtr = ^CISampler;
{$endif}

{$ifdef CLASSES}

type
  CISampler = objcclass external (NSObject, NSCopyingProtocol)
  private
    _priv: pointer;
  public
    class function samplerWithImage (im: CIImage): CISampler; message 'samplerWithImage:';
    class function samplerWithImage_keysAndValues (im: CIImage; key0: id): CISampler; varargs; message 'samplerWithImage:keysAndValues:';
    class function samplerWithImage_options (im: CIImage; dict: NSDictionary): CISampler; message 'samplerWithImage:options:';
    function initWithImage (im: CIImage): id; message 'initWithImage:';
    function initWithImage_keysAndValues (im: CIImage; key0: id): id; varargs; message 'initWithImage:keysAndValues:';
    function initWithImage_options (im: CIImage; dict: NSDictionary): id; message 'initWithImage:options:';
    function definition: CIFilterShape; message 'definition';
    function extent: CGRect; message 'extent';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  kCISamplerAffineMatrix: NSString; cvar; external;
  kCISamplerWrapMode: NSString; cvar; external;
  kCISamplerFilterMode: NSString; cvar; external;
  kCISamplerWrapBlack: NSString; cvar; external;
  kCISamplerWrapClamp: NSString; cvar; external;
  kCISamplerFilterNearest: NSString; cvar; external;
  kCISamplerFilterLinear: NSString; cvar; external;
  kCISamplerColorSpace: NSString; cvar; external;
{$endif}

