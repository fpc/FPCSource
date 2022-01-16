{ Parsed from QTKit.framework QTCaptureDevice.h }


{$ifdef TYPES}
type
  QTCaptureDevicePtr = ^QTCaptureDevice;
{$endif}

{$if ((QTKIT_VERSION_MAX_ALLOWED >= QTKIT_VERSION_7_2)) and ((MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4))}

{$ifdef TYPES}
const
  QTCaptureDeviceAVCTransportControlsNotPlayingMode = 0;
  QTCaptureDeviceAVCTransportControlsPlayingMode = 1;

type
  QTCaptureDeviceAVCTransportControlsPlaybackMode = NSUInteger;
  QTCaptureDeviceAVCTransportControlsPlaybackModePtr = ^QTCaptureDeviceAVCTransportControlsPlaybackMode;

const
  QTCaptureDeviceAVCTransportControlsFastestReverseSpeed = -19000;
  QTCaptureDeviceAVCTransportControlsVeryFastReverseSpeed = -16000;
  QTCaptureDeviceAVCTransportControlsFastReverseSpeed = -13000;
  QTCaptureDeviceAVCTransportControlsNormalReverseSpeed = -10000;
  QTCaptureDeviceAVCTransportControlsSlowReverseSpeed = -7000;
  QTCaptureDeviceAVCTransportControlsVerySlowReverseSpeed = -4000;
  QTCaptureDeviceAVCTransportControlsSlowestReverseSpeed = -1000;
  QTCaptureDeviceAVCTransportControlsStoppedSpeed = 0;
  QTCaptureDeviceAVCTransportControlsSlowestForwardSpeed = 1000;
  QTCaptureDeviceAVCTransportControlsVerySlowForwardSpeed = 4000;
  QTCaptureDeviceAVCTransportControlsSlowForwardSpeed = 7000;
  QTCaptureDeviceAVCTransportControlsNormalForwardSpeed = 10000;
  QTCaptureDeviceAVCTransportControlsFastForwardSpeed = 13000;
  QTCaptureDeviceAVCTransportControlsVeryFastForwardSpeed = 16000;
  QTCaptureDeviceAVCTransportControlsFastestForwardSpeed = 19000;

type
  QTCaptureDeviceAVCTransportControlsSpeed = NSInteger;
  QTCaptureDeviceAVCTransportControlsSpeedPtr = ^QTCaptureDeviceAVCTransportControlsSpeed;
{$endif}

{$ifdef CLASSES}

type
  QTCaptureDevice = objcclass external (NSObject, NSCodingProtocol)
  private
    _internal: QTCaptureDeviceInternal;
    _reserved1: clong;
    _reserved2: clong;
    _reserved3: clong;
  public
    class function inputDevices: NSArray; message 'inputDevices';
    class function inputDevicesWithMediaType (mediaType: NSString): NSArray; message 'inputDevicesWithMediaType:';
    class function defaultInputDeviceWithMediaType (mediaType: NSString): QTCaptureDevice; message 'defaultInputDeviceWithMediaType:';
    class function deviceWithUniqueID (deviceUniqueID: NSString): QTCaptureDevice; message 'deviceWithUniqueID:';
    function uniqueID: NSString; message 'uniqueID';
    function modelUniqueID: NSString; message 'modelUniqueID';
    function localizedDisplayName: NSString; message 'localizedDisplayName';
    function formatDescriptions: NSArray; message 'formatDescriptions';
    function hasMediaType (mediaType: NSString): ObjCBOOL; message 'hasMediaType:';
    function deviceAttributes: NSDictionary; message 'deviceAttributes';
    procedure setDeviceAttributes (deviceAttributes: NSDictionary); message 'setDeviceAttributes:';
    function attributeIsReadOnly (attributeKey: NSString): ObjCBOOL; message 'attributeIsReadOnly:';
    function attributeForKey (attributeKey: NSString): id; message 'attributeForKey:';
    procedure setAttribute_forKey (attribute: id; attributeKey: NSString); message 'setAttribute:forKey:';
    function isConnected: ObjCBOOL; message 'isConnected';
    function isInUseByAnotherApplication: ObjCBOOL; message 'isInUseByAnotherApplication';
    function isOpen: ObjCBOOL; message 'isOpen';
    function open (errorPtr: NSErrorPtr): ObjCBOOL; message 'open:';
    procedure close; message 'close';

    { Adopted protocols }
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
  end;
{$endif}

{$endif}
