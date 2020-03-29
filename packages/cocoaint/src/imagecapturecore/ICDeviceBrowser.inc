{ Parsed from ImageCaptureCore.framework ICDeviceBrowser.h }


{$ifdef TYPES}
type
  ICDeviceBrowserPtr = ^ICDeviceBrowser;
  ICDeviceBrowserDelegateProtocolPtr = ^ICDeviceBrowserDelegateProtocol;
{$endif}

{$ifdef PROTOCOLS}

type
  ICDeviceBrowserDelegateProtocol = objcprotocol external name 'ICDeviceBrowserDelegate' (NSObjectProtocol)
  required
    procedure deviceBrowser_didAddDevice_moreComing (browser: ICDeviceBrowser; device: ICDevice; moreComing: ObjCBOOL); message 'deviceBrowser:didAddDevice:moreComing:';
    procedure deviceBrowser_didRemoveDevice_moreGoing (browser: ICDeviceBrowser; device: ICDevice; moreGoing: ObjCBOOL); message 'deviceBrowser:didRemoveDevice:moreGoing:';
  optional
    procedure deviceBrowser_deviceDidChangeName (browser: ICDeviceBrowser; device: ICDevice); message 'deviceBrowser:deviceDidChangeName:';
    procedure deviceBrowser_deviceDidChangeSharingState (browser: ICDeviceBrowser; device: ICDevice); message 'deviceBrowser:deviceDidChangeSharingState:';
    procedure deviceBrowser_requestsSelectDevice (browser: ICDeviceBrowser; device: ICDevice); message 'deviceBrowser:requestsSelectDevice:';
  optional
    procedure deviceBrowserDidEnumerateLocalDevices (browser: ICDeviceBrowser); message 'deviceBrowserDidEnumerateLocalDevices:';
  end;
{$endif}

{$ifdef CLASSES}

type
  ICDeviceBrowser = objcclass external (NSObject)
  private
    _privateData: id;
  public
    procedure setDelegate(newValue: ICDeviceBrowserDelegateProtocol); message 'setDelegate:';
    function delegate: ICDeviceBrowserDelegateProtocol; message 'delegate';
    function isBrowsing: ObjCBOOL; message 'isBrowsing';
    procedure setBrowsedDeviceTypeMask(newValue: ICDeviceTypeMask); message 'setBrowsedDeviceTypeMask:';
    function browsedDeviceTypeMask: ICDeviceTypeMask; message 'browsedDeviceTypeMask';
    function devices: NSArray; message 'devices';
    function preferredDevice: ICDevice; message 'preferredDevice';
    function init: id; message 'init';
    procedure start; message 'start';
    procedure stop; message 'stop';
  end;
{$endif}

