{$mode delphi}
{$modeswitch objectivec1}
{$modeswitch cvar}
{$packrecords c}

unit DefinedClassesCoreImage;
interface

type
  CIColor = objcclass external;
  CIContext = objcclass external;
  CIDetector = objcclass external;
  CIFaceFeature = objcclass external;
  CIFeature = objcclass external;
  CIFilter = objcclass external;
  CIFilterGenerator = objcclass external;
  CIFilterShape = objcclass external;
  CIImage = objcclass external;
  CIImageAccumulator = objcclass external;
  CIKernel = objcclass external;
  CIPlugIn = objcclass external;
  CIQRCodeFeature = objcclass external;
  CIRectangleFeature = objcclass external;
  CISampler = objcclass external;
  CIVector = objcclass external;
  CIFilterConstructorProtocol = objcprotocol external name 'CIFilterConstructor';
  CIPlugInRegistrationProtocol = objcprotocol external name 'CIPlugInRegistration';

type
  NSArray = objcclass external;
  NSData = objcclass external;
  NSDictionary = objcclass external;
  NSString = objcclass external;
  NSURL = objcclass external;

implementation
end.
