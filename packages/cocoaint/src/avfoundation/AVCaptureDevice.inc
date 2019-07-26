{ Parsed from AVFoundation.framework AVCaptureDevice.h }


{ Types from AVCaptureDevice }
{$ifdef TYPES}


{$endif}


{$ifdef TYPES}
type
  AVCaptureDevicePtr = ^AVCaptureDevice;
  AVFrameRateRangePtr = ^AVFrameRateRange;
  AVCaptureDeviceFormatPtr = ^AVCaptureDeviceFormat;
  AVCaptureDeviceInputSourcePtr = ^AVCaptureDeviceInputSource;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  AVCaptureDeviceWasConnectedNotification: NSString { available in 10_7, 4_0 }; cvar; external;
  AVCaptureDeviceWasDisconnectedNotification: NSString { available in 10_7, 4_0 }; cvar; external;
  AVCaptureDeviceSubjectAreaDidChangeNotification: NSString { available in 5_0 }; cvar; external;
{$endif}

{$ifdef CLASSES}

type
  AVCaptureDevice = objcclass external (NSObject)
  private
    _internal: AVCaptureDeviceInternal;
  public
    class function devices: NSArray; message 'devices';
    class function devicesWithMediaType (mediaType: NSString): NSArray; message 'devicesWithMediaType:';
    class function defaultDeviceWithMediaType (mediaType: NSString): AVCaptureDevice; message 'defaultDeviceWithMediaType:';
    class function deviceWithUniqueID (deviceUniqueID: NSString): AVCaptureDevice; message 'deviceWithUniqueID:';
    function uniqueID: NSString; message 'uniqueID';
    function modelID: NSString; message 'modelID';
    function localizedName: NSString; message 'localizedName';
    {$if (defined(TARGET_OS_MAC) and not (defined(TARGET_OS_EMBEDDED) or defined(TARGET_OS_IPHONE)))}
    function manufacturer: NSString; message 'manufacturer';
    function transportType: cint32; message 'transportType';
    {$endif}
    function hasMediaType (mediaType: NSString): ObjCBOOL; message 'hasMediaType:';
    function lockForConfiguration (outError: NSErrorPtr): ObjCBOOL; message 'lockForConfiguration:';
    procedure unlockForConfiguration; message 'unlockForConfiguration';
    function supportsAVCaptureSessionPreset (preset: NSString): ObjCBOOL; message 'supportsAVCaptureSessionPreset:';
    function isConnected: ObjCBOOL; message 'isConnected';
    {$if (defined(TARGET_OS_MAC) and not (defined(TARGET_OS_EMBEDDED) or defined(TARGET_OS_IPHONE)))}
    function isInUseByAnotherApplication: ObjCBOOL; message 'isInUseByAnotherApplication';
    function isSuspended: ObjCBOOL; message 'isSuspended';
    function linkedDevices: NSArray; message 'linkedDevices';
    function formats: NSArray; message 'formats';
    procedure setActiveFormat(newValue: AVCaptureDeviceFormat); message 'setActiveFormat:';
    function activeFormat: AVCaptureDeviceFormat; message 'activeFormat';
    procedure setActiveVideoMinFrameDuration(newValue: CMTime); message 'setActiveVideoMinFrameDuration:';
    function activeVideoMinFrameDuration: CMTime; message 'activeVideoMinFrameDuration';
    procedure setActiveVideoMaxFrameDuration(newValue: CMTime); message 'setActiveVideoMaxFrameDuration:';
    function activeVideoMaxFrameDuration: CMTime; message 'activeVideoMaxFrameDuration';
    function inputSources: NSArray; message 'inputSources';
    procedure setActiveInputSource(newValue: AVCaptureDeviceInputSource); message 'setActiveInputSource:';
    function activeInputSource: AVCaptureDeviceInputSource; message 'activeInputSource';
    {$endif}
  end;
{$endif}

{$ifdef TYPES}
type
  AVCaptureDevicePosition = NSInteger;
  AVCaptureDevicePositionPtr = ^AVCaptureDevicePosition;

const
  AVCaptureDevicePositionUnspecified = 0;
  AVCaptureDevicePositionBack = 1;
  AVCaptureDevicePositionFront = 2;
{$endif}

{$ifdef CLASSES}

type
  AVCaptureDevice_AVCaptureDevicePosition = objccategory external name 'AVCaptureDevicePosition' (AVCaptureDevice)
    function position: AVCaptureDevicePosition; message 'position';
  end;
{$endif}

{$ifdef TYPES}
type
  AVCaptureFlashMode = NSInteger;
  AVCaptureFlashModePtr = ^AVCaptureFlashMode;

const
  AVCaptureFlashModeOff = 0;
  AVCaptureFlashModeOn = 1;
  AVCaptureFlashModeAuto = 2;
{$endif}

{$ifdef CLASSES}

type
  AVCaptureDeviceFlash = objccategory external (AVCaptureDevice)
    function hasFlash: ObjCBOOL; message 'hasFlash';
    function isFlashAvailable: ObjCBOOL; message 'isFlashAvailable';
    function isFlashActive: ObjCBOOL; message 'isFlashActive';
    function isFlashModeSupported (flashMode: AVCaptureFlashMode): ObjCBOOL; message 'isFlashModeSupported:';
    procedure setFlashMode(newValue: AVCaptureFlashMode); message 'setFlashMode:';
    function flashMode: AVCaptureFlashMode; message 'flashMode';
  end;
{$endif}

{$ifdef TYPES}
type
  AVCaptureTorchMode = NSInteger;
  AVCaptureTorchModePtr = ^AVCaptureTorchMode;

const
  AVCaptureTorchModeOff = 0;
  AVCaptureTorchModeOn = 1;
  AVCaptureTorchModeAuto = 2;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  AVCaptureMaxAvailableTorchLevel: single; cvar; external;
{$endif}

{$ifdef CLASSES}

type
  AVCaptureDeviceTorch = objccategory external (AVCaptureDevice)
    function hasTorch: ObjCBOOL; message 'hasTorch';
    function isTorchAvailable: ObjCBOOL; message 'isTorchAvailable';
    function isTorchActive: ObjCBOOL; message 'isTorchActive';
    function torchLevel: single; message 'torchLevel';
    function isTorchModeSupported (torchMode: AVCaptureTorchMode): ObjCBOOL; message 'isTorchModeSupported:';
    procedure setTorchMode(newValue: AVCaptureTorchMode); message 'setTorchMode:';
    function torchMode: AVCaptureTorchMode; message 'torchMode';
    function setTorchModeOnWithLevel_error (torchLevel_: single; outError: NSErrorPtr): ObjCBOOL; message 'setTorchModeOnWithLevel:error:'; { available in 6_0 }
  end;
{$endif}

{$ifdef TYPES}
type
  AVCaptureFocusMode = NSInteger;
  AVCaptureFocusModePtr = ^AVCaptureFocusMode;

const
  AVCaptureFocusModeLocked = 0;
  AVCaptureFocusModeAutoFocus = 1;
  AVCaptureFocusModeContinuousAutoFocus = 2;
{$endif}

{$ifdef CLASSES}

type
  AVCaptureDeviceFocus = objccategory external (AVCaptureDevice)
    function isFocusModeSupported (focusMode: AVCaptureFocusMode): ObjCBOOL; message 'isFocusModeSupported:';
    procedure setFocusMode(newValue: AVCaptureFocusMode); message 'setFocusMode:';
    function focusMode: AVCaptureFocusMode; message 'focusMode';
    function isFocusPointOfInterestSupported: ObjCBOOL; message 'isFocusPointOfInterestSupported';
    procedure setFocusPointOfInterest(newValue: CGPoint); message 'setFocusPointOfInterest:';
    function focusPointOfInterest: CGPoint; message 'focusPointOfInterest';
    function isAdjustingFocus: ObjCBOOL; message 'isAdjustingFocus';
  end;
{$endif}

{$ifdef TYPES}
type
  AVCaptureExposureMode = NSInteger;
  AVCaptureExposureModePtr = ^AVCaptureExposureMode;

const
  AVCaptureExposureModeLocked = 0;
  AVCaptureExposureModeAutoExpose = 1;
  AVCaptureExposureModeContinuousAutoExposure = 2;
{$endif}

{$ifdef CLASSES}

type
  AVCaptureDeviceExposure = objccategory external (AVCaptureDevice)
    function isExposureModeSupported (exposureMode: AVCaptureExposureMode): ObjCBOOL; message 'isExposureModeSupported:';
    procedure setExposureMode(newValue: AVCaptureExposureMode); message 'setExposureMode:';
    function exposureMode: AVCaptureExposureMode; message 'exposureMode';
    function isExposurePointOfInterestSupported: ObjCBOOL; message 'isExposurePointOfInterestSupported';
    procedure setExposurePointOfInterest(newValue: CGPoint); message 'setExposurePointOfInterest:';
    function exposurePointOfInterest: CGPoint; message 'exposurePointOfInterest';
    function isAdjustingExposure: ObjCBOOL; message 'isAdjustingExposure';
  end;
{$endif}

{$ifdef TYPES}
type
  AVCaptureWhiteBalanceMode = NSInteger;
  AVCaptureWhiteBalanceModePtr = ^AVCaptureWhiteBalanceMode;

const
  AVCaptureWhiteBalanceModeLocked = 0;
  AVCaptureWhiteBalanceModeAutoWhiteBalance = 1;
  AVCaptureWhiteBalanceModeContinuousAutoWhiteBalance = 2;
{$endif}

{$ifdef CLASSES}

type
  AVCaptureDeviceWhiteBalance = objccategory external (AVCaptureDevice)
    function isWhiteBalanceModeSupported (whiteBalanceMode: AVCaptureWhiteBalanceMode): ObjCBOOL; message 'isWhiteBalanceModeSupported:';
    procedure setWhiteBalanceMode(newValue: AVCaptureWhiteBalanceMode); message 'setWhiteBalanceMode:';
    function whiteBalanceMode: AVCaptureWhiteBalanceMode; message 'whiteBalanceMode';
    function isAdjustingWhiteBalance: ObjCBOOL; message 'isAdjustingWhiteBalance';
  end;

type
  AVCaptureDeviceSubjectAreaChangeMonitoring = objccategory external (AVCaptureDevice)
    procedure setSubjectAreaChangeMonitoringEnabled(newValue: ObjCBOOL); message 'setSubjectAreaChangeMonitoringEnabled:';
    function isSubjectAreaChangeMonitoringEnabled: ObjCBOOL; message 'isSubjectAreaChangeMonitoringEnabled';
  end;

type
  AVCaptureDeviceLowLightBoost = objccategory external (AVCaptureDevice)
    function isLowLightBoostSupported: ObjCBOOL; message 'isLowLightBoostSupported';
    function isLowLightBoostEnabled: ObjCBOOL; message 'isLowLightBoostEnabled';
    procedure setAutomaticallyEnablesLowLightBoostWhenAvailable(newValue: ObjCBOOL); message 'setAutomaticallyEnablesLowLightBoostWhenAvailable:';
    function automaticallyEnablesLowLightBoostWhenAvailable: ObjCBOOL; message 'automaticallyEnablesLowLightBoostWhenAvailable';
  end;
{$endif}

{$if (defined(TARGET_OS_MAC) and not (defined(TARGET_OS_EMBEDDED) or defined(TARGET_OS_IPHONE)))}
{$ifdef TYPES}
type
  AVCaptureDeviceTransportControlsSpeed = single;
  AVCaptureDeviceTransportControlsSpeedPtr = ^AVCaptureDeviceTransportControlsSpeed;
  AVCaptureDeviceTransportControlsPlaybackMode = NSInteger;
  AVCaptureDeviceTransportControlsPlaybackModePtr = ^AVCaptureDeviceTransportControlsPlaybackMode;

const
  AVCaptureDeviceTransportControlsNotPlayingMode = 0;
  AVCaptureDeviceTransportControlsPlayingMode = 1;
{$endif}

{$ifdef CLASSES}

type
  AVCaptureDeviceTransportControls = objccategory external (AVCaptureDevice)
    function transportControlsSupported: ObjCBOOL; message 'transportControlsSupported';
    function transportControlsPlaybackMode: AVCaptureDeviceTransportControlsPlaybackMode; message 'transportControlsPlaybackMode';
    function transportControlsSpeed: AVCaptureDeviceTransportControlsSpeed; message 'transportControlsSpeed';
    procedure setTransportControlsPlaybackMode_speed (mode: AVCaptureDeviceTransportControlsPlaybackMode; speed: AVCaptureDeviceTransportControlsSpeed); message 'setTransportControlsPlaybackMode:speed:'; { available in 10_7, NA }
  end;
{$endif}

{$endif}

{$if (defined(TARGET_OS_MAC) and not (defined(TARGET_OS_EMBEDDED) or defined(TARGET_OS_IPHONE)))}
{$ifdef CLASSES}

type
  AVFrameRateRange = objcclass external (NSObject)
  private
    _internal: AVFrameRateRangeInternal;
  public
    function minFrameRate: Float64; message 'minFrameRate';
    function maxFrameRate: Float64; message 'maxFrameRate';
    function maxFrameDuration: CMTime; message 'maxFrameDuration';
    function minFrameDuration: CMTime; message 'minFrameDuration';
  end;
{$endif}

{$endif}

{$if (defined(TARGET_OS_MAC) and not (defined(TARGET_OS_EMBEDDED) or defined(TARGET_OS_IPHONE)))}
{$ifdef CLASSES}

type
  AVCaptureDeviceFormat = objcclass external (NSObject)
  private
    _internal: AVCaptureDeviceFormatInternal;
  public
    function mediaType: NSString; message 'mediaType';
    function formatDescription: CMFormatDescriptionRef; message 'formatDescription';
    function videoSupportedFrameRateRanges: NSArray; message 'videoSupportedFrameRateRanges';
  end;
{$endif}

{$endif}

{$if (defined(TARGET_OS_MAC) and not (defined(TARGET_OS_EMBEDDED) or defined(TARGET_OS_IPHONE)))}
{$ifdef CLASSES}

type
  AVCaptureDeviceInputSource = objcclass external (NSObject)
  private
    _internal: AVCaptureDeviceInputSourceInternal;
  public
    function inputSourceID: NSString; message 'inputSourceID';
    function localizedName: NSString; message 'localizedName';
  end;
{$endif}

{$endif}
