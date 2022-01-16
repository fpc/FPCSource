{$mode delphi}
{$modeswitch objectivec1}
{$modeswitch cvar}
{$packrecords c}

unit DefinedClassesImageCaptureCore;
interface

type
  ICCameraDevice = objcclass external;
  ICCameraFile = objcclass external;
  ICCameraFolder = objcclass external;
  ICCameraItem = objcclass external;
  ICDevice = objcclass external;
  ICDeviceBrowser = objcclass external;
  ICScannerBandData = objcclass external;
  ICScannerDevice = objcclass external;
  ICScannerFeature = objcclass external;
  ICScannerFeatureBoolean = objcclass external;
  ICScannerFeatureEnumeration = objcclass external;
  ICScannerFeatureRange = objcclass external;
  ICScannerFeatureTemplate = objcclass external;
  ICScannerFunctionalUnit = objcclass external;
  ICScannerFunctionalUnitDocumentFeeder = objcclass external;
  ICScannerFunctionalUnitFlatbed = objcclass external;
  ICScannerFunctionalUnitNegativeTransparency = objcclass external;
  ICScannerFunctionalUnitPositiveTransparency = objcclass external;
  ICCameraDeviceDelegateProtocol = objcprotocol external name 'ICCameraDeviceDelegate';
  ICCameraDeviceDownloadDelegateProtocol = objcprotocol external name 'ICCameraDeviceDownloadDelegate';
  ICDeviceBrowserDelegateProtocol = objcprotocol external name 'ICDeviceBrowserDelegate';
  ICDeviceDelegateProtocol = objcprotocol external name 'ICDeviceDelegate';
  ICScannerDeviceDelegateProtocol = objcprotocol external name 'ICScannerDeviceDelegate';

implementation
end.
