{ Parsed from AVFoundation.framework AVCaptureVideoPreviewLayer.h }


{ Types from AVCaptureVideoPreviewLayer }
{$ifdef TYPES}



{$endif}


{$ifdef TYPES}
type
  AVCaptureVideoPreviewLayerPtr = ^AVCaptureVideoPreviewLayer;
{$endif}

{$ifdef CLASSES}

type
  AVCaptureVideoPreviewLayer = objcclass external (CALayer)
  private
    _internal: AVCaptureVideoPreviewLayerInternal;
  public
    class function layerWithSession (session: AVCaptureSession): id; message 'layerWithSession:';
    function initWithSession (session: AVCaptureSession): id; message 'initWithSession:';
    {$if (defined(TARGET_OS_MAC) and not (defined(TARGET_OS_EMBEDDED) or defined(TARGET_OS_IPHONE)))}
    class function layerWithSessionWithNoConnection (session: AVCaptureSession): id; message 'layerWithSessionWithNoConnection:'; { available in 10_7, NA }
    function initWithSessionWithNoConnection (session: AVCaptureSession): id; message 'initWithSessionWithNoConnection:'; { available in 10_7, NA }
    {$endif}
    procedure setSession(newValue: AVCaptureSession); message 'setSession:';
    function session: AVCaptureSession; message 'session';
    {$if (defined(TARGET_OS_MAC) and not (defined(TARGET_OS_EMBEDDED) or defined(TARGET_OS_IPHONE)))}
    procedure setSessionWithNoConnection (session_: AVCaptureSession); message 'setSessionWithNoConnection:'; { available in 10_7, NA }
    {$endif}
    function connection: AVCaptureConnection; message 'connection';
    procedure setVideoGravity(newValue: NSString); message 'setVideoGravity:';
    function videoGravity: NSString; message 'videoGravity';
    function captureDevicePointOfInterestForPoint (pointInLayer: CGPoint): CGPoint; message 'captureDevicePointOfInterestForPoint:'; { available in 6_0 }
    function pointForCaptureDevicePointOfInterest (captureDevicePointOfInterest: CGPoint): CGPoint; message 'pointForCaptureDevicePointOfInterest:'; { available in 6_0 }
    function transformedMetadataObjectForMetadataObject (metadataObject: AVMetadataObject): AVMetadataObject; message 'transformedMetadataObjectForMetadataObject:'; { available in 6_0 }
    {$if defined(TARGET_OS_IPHONE)}
    function isOrientationSupported: ObjCBOOL; message 'isOrientationSupported';
    procedure setOrientation(newValue: AVCaptureVideoOrientation); message 'setOrientation:';
    function orientation: AVCaptureVideoOrientation; message 'orientation';
    function isMirroringSupported: ObjCBOOL; message 'isMirroringSupported';
    procedure setAutomaticallyAdjustsMirroring(newValue: ObjCBOOL); message 'setAutomaticallyAdjustsMirroring:';
    function automaticallyAdjustsMirroring: ObjCBOOL; message 'automaticallyAdjustsMirroring';
    procedure setMirrored(newValue: ObjCBOOL); message 'setMirrored:';
    function isMirrored: ObjCBOOL; message 'isMirrored';
    {$endif}
  end;
{$endif}

