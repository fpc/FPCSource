{ Parsed from CoreImage.framework CIFilter.h }


{$ifdef TYPES}
type
  CIFilterPtr = ^CIFilter;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  kCIAttributeFilterName: NSString; cvar; external;
  kCIAttributeFilterDisplayName: NSString; cvar; external;
  kCIAttributeDescription: NSString; cvar; external;
  kCIAttributeReferenceDocumentation: NSString; cvar; external;
  kCIAttributeFilterCategories: NSString; cvar; external;
  kCIAttributeClass: NSString; cvar; external;
  kCIAttributeType: NSString; cvar; external;
  kCIAttributeMin: NSString; cvar; external;
  kCIAttributeMax: NSString; cvar; external;
  kCIAttributeSliderMin: NSString; cvar; external;
  kCIAttributeSliderMax: NSString; cvar; external;
  kCIAttributeDefault: NSString; cvar; external;
  kCIAttributeIdentity: NSString; cvar; external;
  kCIAttributeName: NSString; cvar; external;
  kCIAttributeDisplayName: NSString; cvar; external;
  kCIUIParameterSet: NSString; cvar; external;
  kCIUISetBasic: NSString; cvar; external;
  kCIUISetIntermediate: NSString; cvar; external;
  kCIUISetAdvanced: NSString; cvar; external;
  kCIUISetDevelopment: NSString; cvar; external;
  kCIAttributeTypeTime: NSString; cvar; external;
  kCIAttributeTypeScalar: NSString; cvar; external;
  kCIAttributeTypeDistance: NSString; cvar; external;
  kCIAttributeTypeAngle: NSString; cvar; external;
  kCIAttributeTypeBoolean: NSString; cvar; external;
  kCIAttributeTypeInteger: NSString; cvar; external;
  kCIAttributeTypeCount: NSString; cvar; external;
  kCIAttributeTypePosition: NSString; cvar; external;
  kCIAttributeTypeOffset: NSString; cvar; external;
  kCIAttributeTypePosition3: NSString; cvar; external;
  kCIAttributeTypeRectangle: NSString; cvar; external;
  kCIAttributeTypeOpaqueColor: NSString; cvar; external;
  kCIAttributeTypeGradient: NSString; cvar; external;
  kCICategoryDistortionEffect: NSString; cvar; external;
  kCICategoryGeometryAdjustment: NSString; cvar; external;
  kCICategoryCompositeOperation: NSString; cvar; external;
  kCICategoryHalftoneEffect: NSString; cvar; external;
  kCICategoryColorAdjustment: NSString; cvar; external;
  kCICategoryColorEffect: NSString; cvar; external;
  kCICategoryTransition: NSString; cvar; external;
  kCICategoryTileEffect: NSString; cvar; external;
  kCICategoryGenerator: NSString; cvar; external;
  kCICategoryReduction: NSString; cvar; external;
  kCICategoryGradient: NSString; cvar; external;
  kCICategoryStylize: NSString; cvar; external;
  kCICategorySharpen: NSString; cvar; external;
  kCICategoryBlur: NSString; cvar; external;
  kCICategoryVideo: NSString; cvar; external;
  kCICategoryStillImage: NSString; cvar; external;
  kCICategoryInterlaced: NSString; cvar; external;
  kCICategoryNonSquarePixels: NSString; cvar; external;
  kCICategoryHighDynamicRange: NSString; cvar; external;
  kCICategoryBuiltIn: NSString; cvar; external;
  kCICategoryFilterGenerator: NSString; cvar; external;
  kCIApplyOptionExtent: NSString; cvar; external;
  kCIApplyOptionDefinition: NSString; cvar; external;
  kCIApplyOptionUserInfo: NSString; cvar; external;
  kCIApplyOptionColorSpace: NSString; cvar; external;
  kCIOutputImageKey: NSString; cvar; external;
  kCIInputBackgroundImageKey: NSString; cvar; external;
  kCIInputImageKey: NSString; cvar; external;
  kCIInputTimeKey: NSString; cvar; external;
  kCIInputTransformKey: NSString; cvar; external;
  kCIInputScaleKey: NSString; cvar; external;
  kCIInputAspectRatioKey: NSString; cvar; external;
  kCIInputCenterKey: NSString; cvar; external;
  kCIInputRadiusKey: NSString; cvar; external;
  kCIInputAngleKey: NSString; cvar; external;
  kCIInputRefractionKey: NSString; cvar; external;
  kCIInputWidthKey: NSString; cvar; external;
  kCIInputSharpnessKey: NSString; cvar; external;
  kCIInputIntensityKey: NSString; cvar; external;
  kCIInputEVKey: NSString; cvar; external;
  kCIInputSaturationKey: NSString; cvar; external;
  kCIInputColorKey: NSString; cvar; external;
  kCIInputBrightnessKey: NSString; cvar; external;
  kCIInputContrastKey: NSString; cvar; external;
  kCIInputGradientImageKey: NSString; cvar; external;
  kCIInputMaskImageKey: NSString; cvar; external;
  kCIInputShadingImageKey: NSString; cvar; external;
  kCIInputTargetImageKey: NSString; cvar; external;
  kCIInputExtentKey: NSString; cvar; external;
{$endif}

{$ifdef CLASSES}

type
  CIFilter = objcclass external (NSObject, NSCodingProtocol, NSCopyingProtocol)
  private
    _priv:array[0..7] of pointer;
  public
    function outputImage: CIImage; message 'outputImage';
    function inputKeys: NSArray; message 'inputKeys';
    function outputKeys: NSArray; message 'outputKeys';
    procedure setDefaults; message 'setDefaults';
    function attributes: NSDictionary; message 'attributes';
    function apply_arguments_options (k: CIKernel; args: NSArray; dict: NSDictionary): CIImage; message 'apply:arguments:options:';
    function apply (k: CIKernel): CIImage; varargs; message 'apply:';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
  end;


type
  CIFilterRegistry = objccategory external (CIFilter)
    class function filterWithName (name: NSString): CIFilter; message 'filterWithName:';
    class function filterWithName_keysAndValues (name: NSString; key0: id): CIFilter; varargs; message 'filterWithName:keysAndValues:';
    class function filterWithName_withInputParameters (name: NSString; params: NSDictionary): CIFilter; message 'filterWithName:withInputParameters:'; { available starting in __MAC_10_10, __IPHONE_8_0 }
    class function filterNamesInCategory (category: NSString): NSArray; message 'filterNamesInCategory:';
    class function filterNamesInCategories (categories: NSArray): NSArray; message 'filterNamesInCategories:';
    class procedure registerFilterName_constructor_classAttributes (name: NSString; anObject: CIFilterConstructorProtocol; attributes_: NSDictionary); message 'registerFilterName:constructor:classAttributes:';
    class function localizedNameForFilterName (filterName: NSString): NSString; message 'localizedNameForFilterName:';
    class function localizedNameForCategory (category: NSString): NSString; message 'localizedNameForCategory:';
    class function localizedDescriptionForFilterName (filterName: NSString): NSString; message 'localizedDescriptionForFilterName:';
    class function localizedReferenceDocumentationForFilterName (filterName: NSString): NSURL; message 'localizedReferenceDocumentationForFilterName:';
  end;

type
  CIFilterXMPSerialization = objccategory external (CIFilter)
    class function serializedXMPFromFilters_inputImageExtent (filters: NSArray; extent: CGRect): NSData; message 'serializedXMPFromFilters:inputImageExtent:'; { available starting in __MAC_10_9, __IPHONE_6_0 }
    class function filterArrayFromSerializedXMP_inputImageExtent_error (xmpData: NSData; extent: CGRect; outError: NSErrorPtr): NSArray; message 'filterArrayFromSerializedXMP:inputImageExtent:error:'; { available starting in __MAC_10_9, __IPHONE_6_0 }
  end;
{$endif}

