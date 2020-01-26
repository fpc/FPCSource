{ Parsed from ImageCaptureCore.framework ICDevice.h }


{$ifdef TYPES}
type
  ICDevicePtr = ^ICDevice;
  ICDeviceDelegateProtocolPtr = ^ICDeviceDelegateProtocol;
{$endif}

{$ifdef TYPES}
const
  ICDeviceTypeCamera = $00000001;
  ICDeviceTypeScanner = $00000002;

type
  ICDeviceType = NSUInteger;
  ICDeviceTypePtr = ^ICDeviceType;

const
  ICDeviceLocationTypeLocal = $00000100;
  ICDeviceLocationTypeShared = $00000200;
  ICDeviceLocationTypeBonjour = $00000400;
  ICDeviceLocationTypeBluetooth = $00000800;

type
  ICDeviceLocationType = NSUInteger;
  ICDeviceLocationTypePtr = ^ICDeviceLocationType;

const
  ICDeviceTypeMaskCamera = $00000001;
  ICDeviceTypeMaskScanner = $00000002;

type
  ICDeviceTypeMask = NSUInteger;
  ICDeviceTypeMaskPtr = ^ICDeviceTypeMask;

const
  ICDeviceLocationTypeMaskLocal = $00000100;
  ICDeviceLocationTypeMaskShared = $00000200;
  ICDeviceLocationTypeMaskBonjour = $00000400;
  ICDeviceLocationTypeMaskBluetooth = $00000800;
  ICDeviceLocationTypeMaskRemote = $0000FE00;

type
  ICDeviceLocationTypeMask = NSUInteger;
  ICDeviceLocationTypeMaskPtr = ^ICDeviceLocationTypeMask;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  ICTransportTypeUSB: NSString; cvar; external;
  ICTransportTypeFireWire: NSString; cvar; external;
  ICTransportTypeBluetooth: NSString; cvar; external;
  ICTransportTypeTCPIP: NSString; cvar; external;
  ICTransportTypeMassStorage: NSString; cvar; external;
  ICDeviceLocationDescriptionUSB: NSString; cvar; external;
  ICDeviceLocationDescriptionFireWire: NSString; cvar; external;
  ICDeviceLocationDescriptionBluetooth: NSString; cvar; external;
  ICDeviceLocationDescriptionMassStorage: NSString; cvar; external;
  ICButtonTypeScan: NSString; cvar; external;
  ICButtonTypeMail: NSString; cvar; external;
  ICButtonTypeCopy: NSString; cvar; external;
  ICButtonTypeWeb: NSString; cvar; external;
  ICButtonTypePrint: NSString; cvar; external;
  ICButtonTypeTransfer: NSString; cvar; external;
  ICStatusNotificationKey: NSString; cvar; external;
  ICStatusCodeKey: NSString; cvar; external;
  ICLocalizedStatusNotificationKey: NSString; cvar; external;
  ICDeviceCanEjectOrDisconnect: NSString; cvar; external;
{$endif}

{$ifdef PROTOCOLS}

type
  ICDeviceDelegateProtocol = objcprotocol external name 'ICDeviceDelegate' (NSObjectProtocol)
  required
    procedure didRemoveDevice (device: ICDevice); message 'didRemoveDevice:';
  optional
    procedure device_didOpenSessionWithError (device: ICDevice; error: NSError); message 'device:didOpenSessionWithError:';
    procedure deviceDidBecomeReady (device: ICDevice); message 'deviceDidBecomeReady:';
    procedure device_didCloseSessionWithError (device: ICDevice; error: NSError); message 'device:didCloseSessionWithError:';
    procedure deviceDidChangeName (device: ICDevice); message 'deviceDidChangeName:';
    procedure deviceDidChangeSharingState (device: ICDevice); message 'deviceDidChangeSharingState:';
    procedure device_didReceiveStatusInformation (device: ICDevice; status: NSDictionary); message 'device:didReceiveStatusInformation:';
    procedure device_didEncounterError (device: ICDevice; error: NSError); message 'device:didEncounterError:';
    procedure device_didReceiveButtonPress (device: ICDevice; buttonType: NSString); message 'device:didReceiveButtonPress:';
    procedure device_didReceiveCustomNotification_data (device: ICDevice; notification: NSDictionary; data: NSData); message 'device:didReceiveCustomNotification:data:';
  end;
{$endif}

{$ifdef CLASSES}

type
  ICDevice = objcclass external (NSObject)
  private
    _deviceProperties: id;
  public
    procedure setDelegate(newValue: ICDeviceDelegateProtocol); message 'setDelegate:';
    function delegate: ICDeviceDelegateProtocol; message 'delegate';
    function type_: ICDeviceType; message 'type';
    function name: NSString; message 'name';
    function icon: CGImageRef; message 'icon';
    function capabilities: NSArray; message 'capabilities';
    function modulePath: NSString; message 'modulePath';
    function moduleVersion: NSString; message 'moduleVersion';
    function moduleExecutableArchitecture: cint; message 'moduleExecutableArchitecture';
    function isRemote: ObjCBOOL; message 'isRemote';
    function isShared: ObjCBOOL; message 'isShared';
    function hasConfigurableWiFiInterface: ObjCBOOL; message 'hasConfigurableWiFiInterface';
    function transportType: NSString; message 'transportType';
    function usbLocationID: cint; message 'usbLocationID';
    function usbProductID: cint; message 'usbProductID';
    function usbVendorID: cint; message 'usbVendorID';
    function fwGUID: clonglong; message 'fwGUID';
    function serialNumberString: NSString; message 'serialNumberString';
    function locationDescription: NSString; message 'locationDescription';
    function hasOpenSession: ObjCBOOL; message 'hasOpenSession';
    function UUIDString: NSString; message 'UUIDString';
    function persistentIDString: NSString; message 'persistentIDString';
    function buttonPressed: NSString; message 'buttonPressed';
    procedure setAutolaunchApplicationPath(newValue: NSString); message 'setAutolaunchApplicationPath:';
    function autolaunchApplicationPath: NSString; message 'autolaunchApplicationPath';
    function userData: NSMutableDictionary; message 'userData';
    procedure requestOpenSession; message 'requestOpenSession';
    procedure requestCloseSession; message 'requestCloseSession';
    procedure requestYield; message 'requestYield';
    procedure requestSendMessage_outData_maxReturnedDataSize_sendMessageDelegate_didSendMessageSelector_contextInfo (messageCode: cuint; data: NSData; maxReturnedDataSize: cuint; sendMessageDelegate: id; selector: SEL; contextInfo: pointer); message 'requestSendMessage:outData:maxReturnedDataSize:sendMessageDelegate:didSendMessageSelector:contextInfo:';
    procedure requestEjectOrDisconnect; message 'requestEjectOrDisconnect';
  end;
{$endif}

