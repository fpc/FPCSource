{ Parsed from Quartzcore.framework CIRAWFilter.h }

{$ifdef TYPES}
{$ifndef CIRAWFILTER_PAS_T}
{$define CIRAWFILTER_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef CIRAWFILTER_PAS_R}
{$define CIRAWFILTER_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef CIRAWFILTER_PAS_F}
{$define CIRAWFILTER_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef CIRAWFILTER_PAS_S}
{$define CIRAWFILTER_PAS_S}

{ External string constants }
var
  kCIInputDecoderVersionKey: NSString; cvar; external;
  kCISupportedDecoderVersionsKey: NSString; cvar; external;
  kCIInputBoostKey: NSString; cvar; external;
  kCIInputNeutralChromaticityXKey: NSString; cvar; external;
  kCIInputNeutralChromaticityYKey: NSString; cvar; external;
  kCIInputNeutralTemperatureKey: NSString; cvar; external;
  kCIInputNeutralTintKey: NSString; cvar; external;
  kCIInputNeutralLocationKey: NSString; cvar; external;
  kCIInputScaleFactorKey: NSString; cvar; external;
  kCIInputAllowDraftModeKey: NSString; cvar; external;
  kCIInputIgnoreImageOrientationKey: NSString; cvar; external;
  kCIInputImageOrientationKey: NSString; cvar; external;
  kCIInputEnableSharpeningKey: NSString; cvar; external;
  kCIInputEnableChromaticNoiseTrackingKey: NSString; cvar; external;
  kCIInputBoostShadowAmountKey: NSString; cvar; external;
  kCIInputBiasKey: NSString; cvar; external;
  kCIOutputNativeSizeKey: NSString; cvar; external;

{$endif}
{$endif}

{$ifdef FORWARD}

{$endif}

{$ifdef CLASSES}
{$ifndef CIRAWFILTER_PAS_C}
{$define CIRAWFILTER_PAS_C}

{ CIRAWFilterCategory }
  CIRAWFilterCategory = objccategory external (CIFilter)
    class function filterWithImageURL_options(url: NSURL; options: NSDictionary): CIFilter; message 'filterWithImageURL:options:';
    class function filterWithImageData_options(data: NSData; options: NSDictionary): CIFilter; message 'filterWithImageData:options:';
  end;

{$endif}
{$endif}
