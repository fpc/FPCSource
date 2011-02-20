{ Parsed from Quartzcore.framework CISampler.h }

{$ifdef TYPES}
{$ifndef CISAMPLER_PAS_T}
{$define CISAMPLER_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef CISAMPLER_PAS_R}
{$define CISAMPLER_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef CISAMPLER_PAS_F}
{$define CISAMPLER_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef CISAMPLER_PAS_S}
{$define CISAMPLER_PAS_S}

{ External string constants }
var
  kCISamplerAffineMatrix: NSString; cvar; external;
  kCISamplerWrapMode: NSString; cvar; external;
  kCISamplerFilterMode: NSString; cvar; external;
  kCISamplerWrapBlack: NSString; cvar; external;
  kCISamplerFilterNearest: NSString; cvar; external;

{$endif}
{$endif}

{$ifdef FORWARD}
  CISampler = objcclass;
  CISamplerPointer = ^CISampler;
  CISamplerPtr = CISamplerPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef CISAMPLER_PAS_C}
{$define CISAMPLER_PAS_C}

{ CISampler }
  CISampler = objcclass external (NSObject, NSCopyingProtocol)
  private
    _priv: Pointer;
    
  public
    class function samplerWithImage(im: CIImage): CISampler; message 'samplerWithImage:';
    class function samplerWithImage_keysAndValues(im: CIImage; firstKey: id): CISampler; varargs; message 'samplerWithImage:keysAndValues:';
    class function samplerWithImage_options(im: CIImage; dict: NSDictionary): CISampler; message 'samplerWithImage:options:';
    function initWithImage(im: CIImage): id; message 'initWithImage:';
    function initWithImage_keysAndValues(im: CIImage; firstKey: id): id; varargs; message 'initWithImage:keysAndValues:';
    function initWithImage_options(im: CIImage; dict: NSDictionary): id; message 'initWithImage:options:';
    function definition: CIFilterShape; message 'definition';
    function extent: CGRect; message 'extent';

    { Adopted Protocols }
    function copyWithZone(zone_: NSZonePtr): id;
  end;

{$endif}
{$endif}
