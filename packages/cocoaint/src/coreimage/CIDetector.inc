{ Parsed from CoreImage.framework CIDetector.h }


{$ifdef TYPES}
type
  CIDetectorPtr = ^CIDetector;
{$endif}

{$ifdef CLASSES}

type
  CIDetector = objcclass external (NSObject)
  private
  public
    class function detectorOfType_context_options (type_: NSString; context: CIContext; options: NSDictionary): CIDetector; message 'detectorOfType:context:options:';
    function featuresInImage (image: CIImage): NSArray; message 'featuresInImage:';
    function featuresInImage_options (image: CIImage; options: NSDictionary): NSArray; message 'featuresInImage:options:'; { available starting in __MAC_10_8, __IPHONE_5_0 }
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  CIDetectorTypeFace: NSString { available starting in __MAC_10_7, __IPHONE_5_0 }; cvar; external;
  CIDetectorTypeRectangle: NSString { available starting in __MAC_10_10, __IPHONE_8_0 }; cvar; external;
  CIDetectorTypeQRCode: NSString { available starting in __MAC_10_10, __IPHONE_8_0 }; cvar; external;
  CIDetectorAccuracy: NSString { available starting in __MAC_10_7, __IPHONE_5_0 }; cvar; external;
  CIDetectorAccuracyLow: NSString { available starting in __MAC_10_7, __IPHONE_5_0 }; cvar; external;
  CIDetectorAccuracyHigh: NSString { available starting in __MAC_10_7, __IPHONE_5_0 }; cvar; external;
  CIDetectorTracking: NSString { available starting in __MAC_10_8, __IPHONE_NA }; cvar; external;
  CIDetectorMinFeatureSize: NSString { available starting in __MAC_10_8, __IPHONE_NA }; cvar; external;
  CIDetectorImageOrientation: NSString { available starting in __MAC_10_8, __IPHONE_5_0 }; cvar; external;
  CIDetectorEyeBlink: NSString { available starting in __MAC_10_9, __IPHONE_7_0 }; cvar; external;
  CIDetectorSmile: NSString { available starting in __MAC_10_9, __IPHONE_7_0 }; cvar; external;
  CIDetectorFocalLength: NSString { available starting in __MAC_10_10, __IPHONE_8_0 }; cvar; external;
  CIDetectorAspectRatio: NSString { available starting in __MAC_10_10, __IPHONE_8_0 }; cvar; external;
{$endif}

