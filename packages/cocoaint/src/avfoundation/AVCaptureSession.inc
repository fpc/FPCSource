{ Parsed from AVFoundation.framework AVCaptureSession.h }


{ Types from AVCaptureSession }
{$ifdef TYPES}


{$endif}


{ Types from AVCaptureConnection }
{$ifdef TYPES}


{$endif}


{ Types from AVCaptureAudioChannel }
{$ifdef TYPES}

{$endif}


{$ifdef TYPES}
type
  AVCaptureSessionPtr = ^AVCaptureSession;
  AVCaptureConnectionPtr = ^AVCaptureConnection;
  AVCaptureAudioChannelPtr = ^AVCaptureAudioChannel;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  AVCaptureSessionRuntimeErrorNotification: NSString { available in 10_7, 4_0 }; cvar; external;
  AVCaptureSessionErrorKey: NSString { available in 10_7, 4_0 }; cvar; external;
  AVCaptureSessionDidStartRunningNotification: NSString { available in 10_7, 4_0 }; cvar; external;
  AVCaptureSessionDidStopRunningNotification: NSString { available in 10_7, 4_0 }; cvar; external;
{$endif}

{$if defined(TARGET_OS_IPHONE)}
{$ifdef EXTERNAL_SYMBOLS}
var
  AVCaptureSessionWasInterruptedNotification: NSString { available in 4_0 }; cvar; external;
  AVCaptureSessionInterruptionEndedNotification: NSString { available in 4_0 }; cvar; external;
{$endif}

{$endif}
{$ifdef TYPES}
type
  AVCaptureVideoOrientation = NSInteger;
  AVCaptureVideoOrientationPtr = ^AVCaptureVideoOrientation;

const
  AVCaptureVideoOrientationPortrait = 1;
  AVCaptureVideoOrientationPortraitUpsideDown = 2;
  AVCaptureVideoOrientationLandscapeRight = 3;
  AVCaptureVideoOrientationLandscapeLeft = 4;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  AVCaptureSessionPresetPhoto: NSString { available in 10_7, 4_0 }; cvar; external;
  AVCaptureSessionPresetHigh: NSString { available in 10_7, 4_0 }; cvar; external;
  AVCaptureSessionPresetMedium: NSString { available in 10_7, 4_0 }; cvar; external;
  AVCaptureSessionPresetLow: NSString { available in 10_7, 4_0 }; cvar; external;
{$endif}

{$if (defined(TARGET_OS_MAC) and not (defined(TARGET_OS_EMBEDDED) or defined(TARGET_OS_IPHONE)))}
{$ifdef EXTERNAL_SYMBOLS}
var
  AVCaptureSessionPreset320x240: NSString { available in 10_7, NA }; cvar; external;
{$endif}

{$endif}
{$ifdef EXTERNAL_SYMBOLS}
var
  AVCaptureSessionPreset352x288: NSString { available in 10_7, 5_0 }; cvar; external;
  AVCaptureSessionPreset640x480: NSString { available in 10_7, 4_0 }; cvar; external;
{$endif}

{$if (defined(TARGET_OS_MAC) and not (defined(TARGET_OS_EMBEDDED) or defined(TARGET_OS_IPHONE)))}
{$ifdef EXTERNAL_SYMBOLS}
var
  AVCaptureSessionPreset960x540: NSString { available in 10_7, NA }; cvar; external;
{$endif}

{$endif}
{$ifdef EXTERNAL_SYMBOLS}
var
  AVCaptureSessionPreset1280x720: NSString { available in 10_7, 4_0 }; cvar; external;
{$endif}

{$if defined(TARGET_OS_IPHONE)}
{$ifdef EXTERNAL_SYMBOLS}
var
  AVCaptureSessionPreset1920x1080: NSString { available in NA, 5_0 }; cvar; external;
{$endif}

{$endif}
{$ifdef EXTERNAL_SYMBOLS}
var
  AVCaptureSessionPresetiFrame960x540: NSString { available in 10_9, 5_0 }; cvar; external;
  AVCaptureSessionPresetiFrame1280x720: NSString { available in 10_9, 5_0 }; cvar; external;
{$endif}

{$ifdef CLASSES}

type
  AVCaptureSession = objcclass external (NSObject)
  private
    _internal: AVCaptureSessionInternal;
  public
    function canSetSessionPreset (preset: NSString): ObjCBOOL; message 'canSetSessionPreset:';
    procedure setSessionPreset(newValue: NSString); message 'setSessionPreset:';
    function sessionPreset: NSString; message 'sessionPreset';
    function inputs: NSArray; message 'inputs';
    function canAddInput (input: AVCaptureInput): ObjCBOOL; message 'canAddInput:';
    procedure addInput (input: AVCaptureInput); message 'addInput:';
    procedure removeInput (input: AVCaptureInput); message 'removeInput:';
    function outputs: NSArray; message 'outputs';
    function canAddOutput (output: AVCaptureOutput): ObjCBOOL; message 'canAddOutput:';
    procedure addOutput (output: AVCaptureOutput); message 'addOutput:';
    procedure removeOutput (output: AVCaptureOutput); message 'removeOutput:';
    {$if (defined(TARGET_OS_MAC) and not (defined(TARGET_OS_EMBEDDED) or defined(TARGET_OS_IPHONE)))}
    procedure addInputWithNoConnections (input: AVCaptureInput); message 'addInputWithNoConnections:'; { available in 10_7, NA }
    procedure addOutputWithNoConnections (output: AVCaptureOutput); message 'addOutputWithNoConnections:'; { available in 10_7, NA }
    function canAddConnection (connection: AVCaptureConnection): ObjCBOOL; message 'canAddConnection:'; { available in 10_7, NA }
    procedure addConnection (connection: AVCaptureConnection); message 'addConnection:'; { available in 10_7, NA }
    procedure removeConnection (connection: AVCaptureConnection); message 'removeConnection:'; { available in 10_7, NA }
    {$endif}
    procedure beginConfiguration; message 'beginConfiguration';
    procedure commitConfiguration; message 'commitConfiguration';
    function isRunning: ObjCBOOL; message 'isRunning';
    {$if defined(TARGET_OS_IPHONE)}
    function isInterrupted: ObjCBOOL; message 'isInterrupted';
    {$endif}
    procedure startRunning; message 'startRunning';
    procedure stopRunning; message 'stopRunning';
    function masterClock: CMClockRef; message 'masterClock';
  end;
{$endif}

{$ifdef TYPES}
type
  AVVideoFieldMode = NSInteger;
  AVVideoFieldModePtr = ^AVVideoFieldMode;

const
  AVVideoFieldModeBoth = 0;
  AVVideoFieldModeTopOnly = 1;
  AVVideoFieldModeBottomOnly = 2;
  AVVideoFieldModeDeinterlace = 3;
{$endif}

{$ifdef CLASSES}

type
  AVCaptureConnection = objcclass external (NSObject)
  private
    _internal: AVCaptureConnectionInternal;
  public
    {$if (defined(TARGET_OS_MAC) and not (defined(TARGET_OS_EMBEDDED) or defined(TARGET_OS_IPHONE)))}
    class function connectionWithInputPorts_output (ports: NSArray; output: AVCaptureOutput): AVCaptureConnection; message 'connectionWithInputPorts:output:'; { available in 10_7, NA }
    class function connectionWithInputPort_videoPreviewLayer (port: AVCaptureInputPort; layer: AVCaptureVideoPreviewLayer): AVCaptureConnection; message 'connectionWithInputPort:videoPreviewLayer:'; { available in 10_7, NA }
    function initWithInputPorts_output (ports: NSArray; output: AVCaptureOutput): id; message 'initWithInputPorts:output:'; { available in 10_7, NA }
    function initWithInputPort_videoPreviewLayer (port: AVCaptureInputPort; layer: AVCaptureVideoPreviewLayer): id; message 'initWithInputPort:videoPreviewLayer:'; { available in 10_7, NA }
    {$endif}
    function inputPorts: NSArray; message 'inputPorts';
    function output: AVCaptureOutput; message 'output';
    function videoPreviewLayer: AVCaptureVideoPreviewLayer; message 'videoPreviewLayer';
    procedure setEnabled(newValue: ObjCBOOL); message 'setEnabled:';
    function isEnabled: ObjCBOOL; message 'isEnabled';
    function isActive: ObjCBOOL; message 'isActive';
    function audioChannels: NSArray; message 'audioChannels';
    function isVideoMirroringSupported: ObjCBOOL; message 'isVideoMirroringSupported';
    procedure setVideoMirrored(newValue: ObjCBOOL); message 'setVideoMirrored:';
    function isVideoMirrored: ObjCBOOL; message 'isVideoMirrored';
    procedure setAutomaticallyAdjustsVideoMirroring(newValue: ObjCBOOL); message 'setAutomaticallyAdjustsVideoMirroring:';
    function automaticallyAdjustsVideoMirroring: ObjCBOOL; message 'automaticallyAdjustsVideoMirroring';
    function isVideoOrientationSupported: ObjCBOOL; message 'isVideoOrientationSupported';
    procedure setVideoOrientation(newValue: AVCaptureVideoOrientation); message 'setVideoOrientation:';
    function videoOrientation: AVCaptureVideoOrientation; message 'videoOrientation';
    {$if (defined(TARGET_OS_MAC) and not (defined(TARGET_OS_EMBEDDED) or defined(TARGET_OS_IPHONE)))}
    function isVideoFieldModeSupported: ObjCBOOL; message 'isVideoFieldModeSupported';
    procedure setVideoFieldMode(newValue: AVVideoFieldMode); message 'setVideoFieldMode:';
    function videoFieldMode: AVVideoFieldMode; message 'videoFieldMode';
    {$endif}
    function isVideoMinFrameDurationSupported: ObjCBOOL; message 'isVideoMinFrameDurationSupported';
    procedure setVideoMinFrameDuration(newValue: CMTime); message 'setVideoMinFrameDuration:';
    function videoMinFrameDuration: CMTime; message 'videoMinFrameDuration';
    function isVideoMaxFrameDurationSupported: ObjCBOOL; message 'isVideoMaxFrameDurationSupported';
    procedure setVideoMaxFrameDuration(newValue: CMTime); message 'setVideoMaxFrameDuration:';
    function videoMaxFrameDuration: CMTime; message 'videoMaxFrameDuration';
    function videoMaxScaleAndCropFactor: CGFloat; message 'videoMaxScaleAndCropFactor';
    procedure setVideoScaleAndCropFactor(newValue: CGFloat); message 'setVideoScaleAndCropFactor:';
    function videoScaleAndCropFactor: CGFloat; message 'videoScaleAndCropFactor';
    function isVideoStabilizationSupported: ObjCBOOL; message 'isVideoStabilizationSupported';
    function isVideoStabilizationEnabled: ObjCBOOL; message 'isVideoStabilizationEnabled';
    procedure setEnablesVideoStabilizationWhenAvailable(newValue: ObjCBOOL); message 'setEnablesVideoStabilizationWhenAvailable:';
    function enablesVideoStabilizationWhenAvailable: ObjCBOOL; message 'enablesVideoStabilizationWhenAvailable';
  end;

type
  AVCaptureAudioChannel = objcclass external (NSObject)
  private
    _internal: AVCaptureAudioChannelInternal;
  public
    function averagePowerLevel: single; message 'averagePowerLevel';
    function peakHoldLevel: single; message 'peakHoldLevel';
    {$if (defined(TARGET_OS_MAC) and not (defined(TARGET_OS_EMBEDDED) or defined(TARGET_OS_IPHONE)))}
    procedure setVolume(newValue: single); message 'setVolume:';
    function volume: single; message 'volume';
    procedure setEnabled(newValue: ObjCBOOL); message 'setEnabled:';
    function isEnabled: ObjCBOOL; message 'isEnabled';
    {$endif}
  end;
{$endif}

