{ Parsed from AVFoundation.framework AVCaptureOutput.h }


{ Types from AVCaptureAudioDataOutput }
{$ifdef TYPES}

{$endif}


{ Types from AVCaptureFileOutput }
{$ifdef TYPES}


{$endif}


{ Types from AVCaptureMovieFileOutput }
{$ifdef TYPES}

{$endif}


{ Types from AVCaptureFileOutputRecordingDelegateProtocol }
{$ifdef TYPES}

{$endif}


{$ifdef TYPES}
type
  AVCaptureOutputPtr = ^AVCaptureOutput;
  AVCaptureVideoDataOutputPtr = ^AVCaptureVideoDataOutput;
  AVCaptureAudioDataOutputPtr = ^AVCaptureAudioDataOutput;
  AVCaptureFileOutputPtr = ^AVCaptureFileOutput;
  AVCaptureMovieFileOutputPtr = ^AVCaptureMovieFileOutput;
  AVCaptureAudioFileOutputPtr = ^AVCaptureAudioFileOutput;
  AVCaptureStillImageOutputPtr = ^AVCaptureStillImageOutput;
  AVCaptureAudioPreviewOutputPtr = ^AVCaptureAudioPreviewOutput;
  AVCaptureMetadataOutputPtr = ^AVCaptureMetadataOutput;
  AVCaptureVideoDataOutputSampleBufferDelegateProtocolPtr = ^AVCaptureVideoDataOutputSampleBufferDelegateProtocol;
  AVCaptureAudioDataOutputSampleBufferDelegateProtocolPtr = ^AVCaptureAudioDataOutputSampleBufferDelegateProtocol;
  AVCaptureFileOutputRecordingDelegateProtocolPtr = ^AVCaptureFileOutputRecordingDelegateProtocol;
  AVCaptureFileOutputDelegateProtocolPtr = ^AVCaptureFileOutputDelegateProtocol;
  AVCaptureMetadataOutputObjectsDelegateProtocolPtr = ^AVCaptureMetadataOutputObjectsDelegateProtocol;
{$endif}

{$ifdef CLASSES}

type
  AVCaptureOutput = objcclass external (NSObject)
  private
    _outputInternal: AVCaptureOutputInternal;
  public
    function connections: NSArray; message 'connections';
    function connectionWithMediaType (mediaType: NSString): AVCaptureConnection; message 'connectionWithMediaType:'; { available in 10_7, 5_0 }
    function transformedMetadataObjectForMetadataObject_connection (metadataObject: AVMetadataObject; connection: AVCaptureConnection): AVMetadataObject; message 'transformedMetadataObjectForMetadataObject:connection:'; { available in 6_0 }
  end;

type
  AVCaptureVideoDataOutput = objcclass external (AVCaptureOutput)
  private
    _internal: AVCaptureVideoDataOutputInternal;
  public
    procedure setSampleBufferDelegate_queue (sampleBufferDelegate: AVCaptureVideoDataOutputSampleBufferDelegateProtocol; sampleBufferCallbackQueue: dispatch_queue_t); message 'setSampleBufferDelegate:queue:';
    function sampleBufferDelegate: AVCaptureVideoDataOutputSampleBufferDelegateProtocol; message 'sampleBufferDelegate';
    function sampleBufferCallbackQueue: dispatch_queue_t; message 'sampleBufferCallbackQueue';
    procedure setVideoSettings(newValue: NSDictionary); message 'setVideoSettings:';
    function videoSettings: NSDictionary; message 'videoSettings';
    function availableVideoCVPixelFormatTypes: NSArray; message 'availableVideoCVPixelFormatTypes';
    function availableVideoCodecTypes: NSArray; message 'availableVideoCodecTypes';
    procedure setMinFrameDuration(newValue: CMTime); message 'setMinFrameDuration:';
    function minFrameDuration: CMTime; message 'minFrameDuration';
    procedure setAlwaysDiscardsLateVideoFrames(newValue: ObjCBOOL); message 'setAlwaysDiscardsLateVideoFrames:';
    function alwaysDiscardsLateVideoFrames: ObjCBOOL; message 'alwaysDiscardsLateVideoFrames';
  end;
{$endif}

{$ifdef PROTOCOLS}

type
  AVCaptureVideoDataOutputSampleBufferDelegateProtocol = objcprotocol external name 'AVCaptureVideoDataOutputSampleBufferDelegate' (NSObjectProtocol)
  optional
    procedure captureOutput_didOutputSampleBuffer_fromConnection (captureOutput: AVCaptureOutput; sampleBuffer: CMSampleBufferRef; connection: AVCaptureConnection); message 'captureOutput:didOutputSampleBuffer:fromConnection:';
    procedure captureOutput_didDropSampleBuffer_fromConnection (captureOutput: AVCaptureOutput; sampleBuffer: CMSampleBufferRef; connection: AVCaptureConnection); message 'captureOutput:didDropSampleBuffer:fromConnection:'; { available in 10_7, 6_0 }
  end;
{$endif}

{$ifdef CLASSES}

type
  AVCaptureAudioDataOutput = objcclass external (AVCaptureOutput)
  private
    _internal: AVCaptureAudioDataOutputInternal;
  public
    procedure setSampleBufferDelegate_queue (sampleBufferDelegate: AVCaptureAudioDataOutputSampleBufferDelegateProtocol; sampleBufferCallbackQueue: dispatch_queue_t); message 'setSampleBufferDelegate:queue:';
    function sampleBufferDelegate: AVCaptureAudioDataOutputSampleBufferDelegateProtocol; message 'sampleBufferDelegate';
    function sampleBufferCallbackQueue: dispatch_queue_t; message 'sampleBufferCallbackQueue';
    {$if (defined(TARGET_OS_MAC) and not (defined(TARGET_OS_EMBEDDED) or defined(TARGET_OS_IPHONE)))}
    procedure setAudioSettings(newValue: NSDictionary); message 'setAudioSettings:';
    function audioSettings: NSDictionary; message 'audioSettings';
    {$endif}
  end;
{$endif}

{$ifdef PROTOCOLS}

type
  AVCaptureAudioDataOutputSampleBufferDelegateProtocol = objcprotocol external name 'AVCaptureAudioDataOutputSampleBufferDelegate' (NSObjectProtocol)
  optional
    procedure captureOutput_didOutputSampleBuffer_fromConnection (captureOutput: AVCaptureOutput; sampleBuffer: CMSampleBufferRef; connection: AVCaptureConnection); message 'captureOutput:didOutputSampleBuffer:fromConnection:';
  end;
{$endif}

{$ifdef CLASSES}

type
  AVCaptureFileOutput = objcclass external (AVCaptureOutput)
  private
    _fileOutputInternal: AVCaptureFileOutputInternal;
  public
    {$if (defined(TARGET_OS_MAC) and not (defined(TARGET_OS_EMBEDDED) or defined(TARGET_OS_IPHONE)))}
    procedure setDelegate(newValue: AVCaptureFileOutputDelegateProtocol); message 'setDelegate:';
    function delegate: AVCaptureFileOutputDelegateProtocol; message 'delegate';
    {$endif}
    function outputFileURL: NSURL; message 'outputFileURL';
    procedure startRecordingToOutputFileURL_recordingDelegate (outputFileURL_: NSURL; delegate_: AVCaptureFileOutputRecordingDelegateProtocol); message 'startRecordingToOutputFileURL:recordingDelegate:';
    procedure stopRecording; message 'stopRecording';
    function isRecording: ObjCBOOL; message 'isRecording';
    {$if (defined(TARGET_OS_MAC) and not (defined(TARGET_OS_EMBEDDED) or defined(TARGET_OS_IPHONE)))}
    function isRecordingPaused: ObjCBOOL; message 'isRecordingPaused';
    procedure pauseRecording; message 'pauseRecording'; { available in 10_7, NA }
    procedure resumeRecording; message 'resumeRecording'; { available in 10_7, NA }
    {$endif}
    function recordedDuration: CMTime; message 'recordedDuration';
    function recordedFileSize: cint64; message 'recordedFileSize';
    procedure setMaxRecordedDuration(newValue: CMTime); message 'setMaxRecordedDuration:';
    function maxRecordedDuration: CMTime; message 'maxRecordedDuration';
    procedure setMaxRecordedFileSize(newValue: cint64); message 'setMaxRecordedFileSize:';
    function maxRecordedFileSize: cint64; message 'maxRecordedFileSize';
    procedure setMinFreeDiskSpaceLimit(newValue: cint64); message 'setMinFreeDiskSpaceLimit:';
    function minFreeDiskSpaceLimit: cint64; message 'minFreeDiskSpaceLimit';
  end;
{$endif}

{$ifdef PROTOCOLS}

type
  AVCaptureFileOutputRecordingDelegateProtocol = objcprotocol external name 'AVCaptureFileOutputRecordingDelegate' (NSObjectProtocol)
  optional
    procedure captureOutput_didStartRecordingToOutputFileAtURL_fromConnections (captureOutput: AVCaptureFileOutput; fileURL: NSURL; connections: NSArray); message 'captureOutput:didStartRecordingToOutputFileAtURL:fromConnections:';
    {$if (defined(TARGET_OS_MAC) and not (defined(TARGET_OS_EMBEDDED) or defined(TARGET_OS_IPHONE)))}
    procedure captureOutput_didPauseRecordingToOutputFileAtURL_fromConnections (captureOutput: AVCaptureFileOutput; fileURL: NSURL; connections: NSArray); message 'captureOutput:didPauseRecordingToOutputFileAtURL:fromConnections:'; { available in 10_7, NA }
    procedure captureOutput_didResumeRecordingToOutputFileAtURL_fromConnections (captureOutput: AVCaptureFileOutput; fileURL: NSURL; connections: NSArray); message 'captureOutput:didResumeRecordingToOutputFileAtURL:fromConnections:'; { available in 10_7, NA }
    procedure captureOutput_willFinishRecordingToOutputFileAtURL_fromConnections_error (captureOutput: AVCaptureFileOutput; fileURL: NSURL; connections: NSArray; error: NSError); message 'captureOutput:willFinishRecordingToOutputFileAtURL:fromConnections:error:'; { available in 10_7, NA }
    {$endif}
  required
    procedure captureOutput_didFinishRecordingToOutputFileAtURL_fromConnections_error (captureOutput: AVCaptureFileOutput; outputFileURL: NSURL; connections: NSArray; error: NSError); message 'captureOutput:didFinishRecordingToOutputFileAtURL:fromConnections:error:';
  end;
{$endif}

{$if (defined(TARGET_OS_MAC) and not (defined(TARGET_OS_EMBEDDED) or defined(TARGET_OS_IPHONE)))}
{$ifdef PROTOCOLS}

type
  AVCaptureFileOutputDelegateProtocol = objcprotocol external name 'AVCaptureFileOutputDelegate' (NSObjectProtocol)
  required
    function captureOutputShouldProvideSampleAccurateRecordingStart (captureOutput: AVCaptureOutput): ObjCBOOL; message 'captureOutputShouldProvideSampleAccurateRecordingStart:'; { available in 10_8, NA }
  optional
    procedure captureOutput_didOutputSampleBuffer_fromConnection (captureOutput: AVCaptureFileOutput; sampleBuffer: CMSampleBufferRef; connection: AVCaptureConnection); message 'captureOutput:didOutputSampleBuffer:fromConnection:'; { available in 10_7, NA }
  end;
{$endif}

{$endif}
{$ifdef CLASSES}

type
  AVCaptureMovieFileOutput = objcclass external (AVCaptureFileOutput)
  private
    _internal: AVCaptureMovieFileOutputInternal;
  public
    procedure setMovieFragmentInterval(newValue: CMTime); message 'setMovieFragmentInterval:';
    function movieFragmentInterval: CMTime; message 'movieFragmentInterval';
    procedure setMetadata(newValue: NSArray); message 'setMetadata:';
    function metadata: NSArray; message 'metadata';
    {$if (defined(TARGET_OS_MAC) and not (defined(TARGET_OS_EMBEDDED) or defined(TARGET_OS_IPHONE)))}
    function outputSettingsForConnection (connection: AVCaptureConnection): NSDictionary; message 'outputSettingsForConnection:'; { available in 10_7, NA }
    procedure setOutputSettings_forConnection (outputSettings: NSDictionary; connection: AVCaptureConnection); message 'setOutputSettings:forConnection:'; { available in 10_7, NA }
    {$endif}
  end;
{$endif}

{$if (defined(TARGET_OS_MAC) and not (defined(TARGET_OS_EMBEDDED) or defined(TARGET_OS_IPHONE)))}
{$ifdef CLASSES}

type
  AVCaptureAudioFileOutput = objcclass external (AVCaptureFileOutput)
  private
    _internal: AVCaptureAudioFileOutputInternal;
  public
    class function availableOutputFileTypes: NSArray; message 'availableOutputFileTypes';
    procedure startRecordingToOutputFileURL_outputFileType_recordingDelegate (outputFileURL_: NSURL; fileType: NSString; delegate_: AVCaptureFileOutputRecordingDelegateProtocol); message 'startRecordingToOutputFileURL:outputFileType:recordingDelegate:';
    procedure setMetadata(newValue: NSArray); message 'setMetadata:';
    function metadata: NSArray; message 'metadata';
    procedure setAudioSettings(newValue: NSDictionary); message 'setAudioSettings:';
    function audioSettings: NSDictionary; message 'audioSettings';
  end;
{$endif}

{$endif}
{$ifdef CLASSES}

type
  AVCaptureStillImageOutput = objcclass external (AVCaptureOutput)
  private
    _internal: AVCaptureStillImageOutputInternal;
  public
    procedure setOutputSettings(newValue: NSDictionary); message 'setOutputSettings:';
    function outputSettings: NSDictionary; message 'outputSettings';
    function availableImageDataCVPixelFormatTypes: NSArray; message 'availableImageDataCVPixelFormatTypes';
    function availableImageDataCodecTypes: NSArray; message 'availableImageDataCodecTypes';
    function isCapturingStillImage: ObjCBOOL; message 'isCapturingStillImage';
    procedure captureStillImageAsynchronouslyFromConnection_completionHandler (connection: AVCaptureConnection; handler: OpaqueCBlock); message 'captureStillImageAsynchronouslyFromConnection:completionHandler:';
    class function jpegStillImageNSDataRepresentation (jpegSampleBuffer: CMSampleBufferRef): NSData; message 'jpegStillImageNSDataRepresentation:';
  end;
{$endif}

{$if (defined(TARGET_OS_MAC) and not (defined(TARGET_OS_EMBEDDED) or defined(TARGET_OS_IPHONE)))}
{$ifdef CLASSES}

type
  AVCaptureAudioPreviewOutput = objcclass external (AVCaptureOutput)
  private
    _internal: AVCaptureAudioPreviewOutputInternal;
  public
    procedure setOutputDeviceUniqueID(newValue: NSString); message 'setOutputDeviceUniqueID:';
    function outputDeviceUniqueID: NSString; message 'outputDeviceUniqueID';
    procedure setVolume(newValue: single); message 'setVolume:';
    function volume: single; message 'volume';
  end;
{$endif}

{$endif}
{$ifdef CLASSES}

type
  AVCaptureMetadataOutput = objcclass external (AVCaptureOutput)
  private
    _internal: AVCaptureMetadataOutputInternal;
  public
    procedure setMetadataObjectsDelegate_queue (objectsDelegate: AVCaptureMetadataOutputObjectsDelegateProtocol; objectsCallbackQueue: dispatch_queue_t); message 'setMetadataObjectsDelegate:queue:';
    function metadataObjectsDelegate: AVCaptureMetadataOutputObjectsDelegateProtocol; message 'metadataObjectsDelegate';
    function metadataObjectsCallbackQueue: dispatch_queue_t; message 'metadataObjectsCallbackQueue';
    function availableMetadataObjectTypes: NSArray; message 'availableMetadataObjectTypes';
    procedure setMetadataObjectTypes(newValue: NSArray); message 'setMetadataObjectTypes:';
    function metadataObjectTypes: NSArray; message 'metadataObjectTypes';
  end;
{$endif}

{$ifdef PROTOCOLS}

type
  AVCaptureMetadataOutputObjectsDelegateProtocol = objcprotocol external name 'AVCaptureMetadataOutputObjectsDelegate' (NSObjectProtocol)
  optional
    procedure captureOutput_didOutputMetadataObjects_fromConnection (captureOutput: AVCaptureOutput; metadataObjects: NSArray; connection: AVCaptureConnection); message 'captureOutput:didOutputMetadataObjects:fromConnection:';
  end;
{$endif}

