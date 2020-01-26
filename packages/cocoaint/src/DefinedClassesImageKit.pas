{$mode delphi}
{$modeswitch objectivec1}
{$modeswitch cvar}

unit DefinedClassesImageKit;
interface

type
  IKCameraDeviceView = objcclass external;
  IKDeviceBrowserView = objcclass external;
  IKFilterBrowserPanel = objcclass external;
  IKFilterBrowserView = objcclass external;
  IKFilterUIView = objcclass external;
  IKImageBrowserCell = objcclass external;
  IKImageBrowserView = objcclass external;
  IKImageEditPanel = objcclass external;
  IKImageView = objcclass external;
  IKPictureTaker = objcclass external;
  IKSaveOptions = objcclass external;
  IKScannerDeviceView = objcclass external;
  IKSlideshow = objcclass external;
  IKCameraDeviceViewDelegateProtocol = objcprotocol external name 'IKCameraDeviceViewDelegate';
  IKDeviceBrowserViewDelegateProtocol = objcprotocol external name 'IKDeviceBrowserViewDelegate';
  IKFilterCustomUIProviderProtocol = objcprotocol external name 'IKFilterCustomUIProvider';
  IKImageEditPanelDataSourceProtocol = objcprotocol external name 'IKImageEditPanelDataSource';
  IKScannerDeviceViewDelegateProtocol = objcprotocol external name 'IKScannerDeviceViewDelegate';
  IKSlideshowDataSourceProtocol = objcprotocol external name 'IKSlideshowDataSource';

implementation
end.
