{ Parsed from QTKit.framework QTCaptureFileOutput.h }


{ Types from QTCaptureFileOutput }
{$ifdef TYPES}

{$endif}


{$ifdef TYPES}
type
  QTCaptureFileOutputPtr = ^QTCaptureFileOutput;
{$endif}

{$if ((QTKIT_VERSION_MAX_ALLOWED >= QTKIT_VERSION_7_2)) and ((MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4))}
{$ifdef TYPES}
const
  QTCaptureFileOutputBufferDestinationNewFile = 1;
  QTCaptureFileOutputBufferDestinationOldFile = 2;

type
  QTCaptureFileOutputBufferDestination = NSUInteger;
  QTCaptureFileOutputBufferDestinationPtr = ^QTCaptureFileOutputBufferDestination;
{$endif}

{$ifdef CLASSES}

type
  QTCaptureFileOutput = objcclass external (QTCaptureOutput)
  private
    _fileOutputInternal: QTCaptureFileOutputInternal;
    _delegate: id;
    _reserved4: clong;
    _reserved5: clong;
    _reserved6: clong;
  public
    function outputFileURL: NSURL; message 'outputFileURL';
    procedure recordToOutputFileURL (url: NSURL); message 'recordToOutputFileURL:';
    procedure recordToOutputFileURL_bufferDestination (url: NSURL; bufferDestination: QTCaptureFileOutputBufferDestination); message 'recordToOutputFileURL:bufferDestination:';
    {$if (QTKIT_VERSION_MAX_ALLOWED >= QTKIT_VERSION_7_6_3)}
    function isRecordingPaused: ObjCBOOL; message 'isRecordingPaused';
    procedure pauseRecording; message 'pauseRecording';
    procedure resumeRecording; message 'resumeRecording';
    {$endif}
    function compressionOptionsForConnection (connection: QTCaptureConnection): QTCompressionOptions; message 'compressionOptionsForConnection:';
    procedure setCompressionOptions_forConnection (compressionOptions: QTCompressionOptions; connection: QTCaptureConnection); message 'setCompressionOptions:forConnection:';
    function maximumVideoSize: NSSize; message 'maximumVideoSize';
    procedure setMaximumVideoSize (maximumVideoSize: NSSize); message 'setMaximumVideoSize:';
    function minimumVideoFrameInterval: NSTimeInterval; message 'minimumVideoFrameInterval';
    procedure setMinimumVideoFrameInterval (minimumVideoFrameInterval: NSTimeInterval); message 'setMinimumVideoFrameInterval:';
    function recordedDuration: QTTime; message 'recordedDuration';
    function recordedFileSize: UInt64; message 'recordedFileSize';
    function maximumRecordedDuration: QTTime; message 'maximumRecordedDuration';
    procedure setMaximumRecordedDuration (maximumRecordedDuration: QTTime); message 'setMaximumRecordedDuration:';
    function maximumRecordedFileSize: UInt64; message 'maximumRecordedFileSize';
    procedure setMaximumRecordedFileSize (maximumRecordedFileSize: UInt64); message 'setMaximumRecordedFileSize:';
    function delegate: id; message 'delegate';
    procedure setDelegate (delegate: id); message 'setDelegate:';
  end;


type
  QTCaptureFileOutput_Delegate = objccategory external (NSObject)
    procedure captureOutput_didOutputSampleBuffer_fromConnection (captureOutput: QTCaptureFileOutput; sampleBuffer: QTSampleBuffer; connection: QTCaptureConnection); message 'captureOutput:didOutputSampleBuffer:fromConnection:';
    procedure captureOutput_willStartRecordingToOutputFileAtURL_forConnections (captureOutput: QTCaptureFileOutput; fileURL: NSURL; connections: NSArray); message 'captureOutput:willStartRecordingToOutputFileAtURL:forConnections:';
    procedure captureOutput_didStartRecordingToOutputFileAtURL_forConnections (captureOutput: QTCaptureFileOutput; fileURL: NSURL; connections: NSArray); message 'captureOutput:didStartRecordingToOutputFileAtURL:forConnections:';
    function captureOutput_shouldChangeOutputFileAtURL_forConnections_dueToError (captureOutput: QTCaptureFileOutput; outputFileURL: NSURL; connections: NSArray; error: NSError): ObjCBOOL; message 'captureOutput:shouldChangeOutputFileAtURL:forConnections:dueToError:';
    procedure captureOutput_mustChangeOutputFileAtURL_forConnections_dueToError (captureOutput: QTCaptureFileOutput; outputFileURL: NSURL; connections: NSArray; error: NSError); message 'captureOutput:mustChangeOutputFileAtURL:forConnections:dueToError:';
    procedure captureOutput_willFinishRecordingToOutputFileAtURL_forConnections_dueToError (captureOutput: QTCaptureFileOutput; outputFileURL: NSURL; connections: NSArray; error: NSError); message 'captureOutput:willFinishRecordingToOutputFileAtURL:forConnections:dueToError:';
    procedure captureOutput_didFinishRecordingToOutputFileAtURL_forConnections_dueToError (captureOutput: QTCaptureFileOutput; outputFileURL: NSURL; connections: NSArray; error: NSError); message 'captureOutput:didFinishRecordingToOutputFileAtURL:forConnections:dueToError:';
    procedure captureOutput_didPauseRecordingToOutputFileAtURL_forConnections (captureOutput: QTCaptureFileOutput; fileURL: NSURL; connections: NSArray); message 'captureOutput:didPauseRecordingToOutputFileAtURL:forConnections:';
    procedure captureOutput_didResumeRecordingToOutputFileAtURL_forConnections (captureOutput: QTCaptureFileOutput; fileURL: NSURL; connections: NSArray); message 'captureOutput:didResumeRecordingToOutputFileAtURL:forConnections:';
  end;
{$endif}

{$endif}
