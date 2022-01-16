{ Parsed from QTKit.framework QTCaptureVideoPreviewOutput.h }


{ Types from QTCaptureVideoPreviewOutput }
{$ifdef TYPES}

{$endif}


{$ifdef TYPES}
type
  QTCaptureVideoPreviewOutputPtr = ^QTCaptureVideoPreviewOutput;
{$endif}

{$if ((QTKIT_VERSION_MAX_ALLOWED >= QTKIT_VERSION_7_2)) and ((MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4))}

{$ifdef CLASSES}

type
  QTCaptureVideoPreviewOutput = objcclass external (QTCaptureOutput)
  private
    _internal: QTCaptureVideoPreviewOutputInternal;
    _delegate: id;
    _reserved4: clong;
  public
    function pixelBufferAttributes: NSDictionary; message 'pixelBufferAttributes';
    procedure setPixelBufferAttributes (pixelBufferAttributes: NSDictionary); message 'setPixelBufferAttributes:';
    {$if  not defined(__LP64__)}
    function visualContextForConnection (connection: QTCaptureConnection): QTVisualContextRef; message 'visualContextForConnection:';
    procedure setVisualContext_forConnection (visualContext: QTVisualContextRef; connection: QTCaptureConnection); message 'setVisualContext:forConnection:';
    {$endif}
    function delegate: id; message 'delegate';
    procedure setDelegate (delegate: id); message 'setDelegate:';
    procedure outputVideoFrame_withSampleBuffer_fromConnection (videoFrame: CVImageBufferRef; sampleBuffer: QTSampleBuffer; connection: QTCaptureConnection); message 'outputVideoFrame:withSampleBuffer:fromConnection:';
  end;


type
  QTCaptureVideoPreviewOutput_Delegate = objccategory external (NSObject)
    procedure captureOutput_didOutputVideoFrame_withSampleBuffer_fromConnection (captureOutput: QTCaptureOutput; videoFrame: CVImageBufferRef; sampleBuffer: QTSampleBuffer; connection: QTCaptureConnection); message 'captureOutput:didOutputVideoFrame:withSampleBuffer:fromConnection:';
  end;
{$endif}

{$endif}
