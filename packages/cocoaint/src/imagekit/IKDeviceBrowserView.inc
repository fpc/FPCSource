{ Parsed from ImageKit.framework IKDeviceBrowserView.h }


{$ifdef TYPES}
type
  IKDeviceBrowserViewPtr = ^IKDeviceBrowserView;
  IKDeviceBrowserViewDelegateProtocolPtr = ^IKDeviceBrowserViewDelegateProtocol;
{$endif}

{$if (MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_6)}
{$ifdef PROTOCOLS}

type
  IKDeviceBrowserViewDelegateProtocol = objcprotocol external name 'IKDeviceBrowserViewDelegate'
  required
    procedure deviceBrowserView_selectionDidChange (deviceBrowserView: IKDeviceBrowserView; device: ICDevice); message 'deviceBrowserView:selectionDidChange:';
  optional
    procedure deviceBrowserView_didEncounterError (deviceBrowserView: IKDeviceBrowserView; error: NSError); message 'deviceBrowserView:didEncounterError:';
  end;
{$endif}

{$ifdef TYPES}
type
  IKDeviceBrowserViewDisplayMode = NSInteger;
  IKDeviceBrowserViewDisplayModePtr = ^IKDeviceBrowserViewDisplayMode;

const
  IKDeviceBrowserViewDisplayModeTable = 0;
  IKDeviceBrowserViewDisplayModeOutline = 1;
  IKDeviceBrowserViewDisplayModeIcon = 2;
{$endif}

{$ifdef CLASSES}

type
  IKDeviceBrowserView = objcclass external (NSView)
  private
    _privateData: id;
  public
    procedure setDelegate(newValue: IKDeviceBrowserViewDelegateProtocol); message 'setDelegate:';
    function delegate: IKDeviceBrowserViewDelegateProtocol; message 'delegate';
    procedure setDisplaysLocalCameras(newValue: ObjCBOOL); message 'setDisplaysLocalCameras:';
    function displaysLocalCameras: ObjCBOOL; message 'displaysLocalCameras';
    procedure setDisplaysNetworkCameras(newValue: ObjCBOOL); message 'setDisplaysNetworkCameras:';
    function displaysNetworkCameras: ObjCBOOL; message 'displaysNetworkCameras';
    procedure setDisplaysLocalScanners(newValue: ObjCBOOL); message 'setDisplaysLocalScanners:';
    function displaysLocalScanners: ObjCBOOL; message 'displaysLocalScanners';
    procedure setDisplaysNetworkScanners(newValue: ObjCBOOL); message 'setDisplaysNetworkScanners:';
    function displaysNetworkScanners: ObjCBOOL; message 'displaysNetworkScanners';
    procedure setMode(newValue: IKDeviceBrowserViewDisplayMode); message 'setMode:';
    function mode: IKDeviceBrowserViewDisplayMode; message 'mode';
    function selectedDevice: ICDevice; message 'selectedDevice';
  end;
{$endif}

{$endif}
