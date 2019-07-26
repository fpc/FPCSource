{ Parsed from QTKit.framework QTCaptureDecompressedAudioOutput.h }


{$ifdef TYPES}
type
  QTCaptureDecompressedAudioOutputPtr = ^QTCaptureDecompressedAudioOutput;
{$endif}

{$if ((QTKIT_VERSION_MAX_ALLOWED >= QTKIT_VERSION_7_6_3)) and ((MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4))}
{$ifdef CLASSES}

type
  QTCaptureDecompressedAudioOutput = objcclass external (QTCaptureOutput)
  private
    _internal: QTCaptureDecompressedAudioOutputInternal;
    _delegate: id;
    _reserved4: clong;
    _reserved5: clong;
    _reserved6: clong;
  public
    function delegate: id; message 'delegate';
    procedure setDelegate (delegate: id); message 'setDelegate:';
    procedure outputAudioSampleBuffer_fromConnection (sampleBuffer: QTSampleBuffer; connection: QTCaptureConnection); message 'outputAudioSampleBuffer:fromConnection:';
  end;


type
  QTCaptureDecompressedAudioOutput_Delegate = objccategory external (NSObject)
    procedure captureOutput_didOutputAudioSampleBuffer_fromConnection (captureOutput: QTCaptureOutput; sampleBuffer: QTSampleBuffer; connection: QTCaptureConnection); message 'captureOutput:didOutputAudioSampleBuffer:fromConnection:';
  end;
{$endif}

{$endif}
