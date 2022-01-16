{ Parsed from AVFoundation.framework AVSampleBufferGenerator.h }


{$ifdef TYPES}
type
  AVSampleBufferGeneratorPtr = ^AVSampleBufferGenerator;
  AVSampleBufferRequestPtr = ^AVSampleBufferRequest;
{$endif}

{$if  not defined(TARGET_OS_IPHONE)}
{$ifdef CLASSES}

type
  AVSampleBufferGenerator = objcclass external (NSObject)
  private
    _generator: AVSampleBufferGeneratorInternal;
  public
    function initWithAsset_timebase (asset: AVAsset; timebase: CMTimebaseRef): instancetype; message 'initWithAsset:timebase:';
    function createSampleBufferForRequest (request: AVSampleBufferRequest): CMSampleBufferRef; message 'createSampleBufferForRequest:';
    class procedure notifyOfDataReadyForSampleBuffer_completionHandler (sbuf: CMSampleBufferRef; completionHandler: OpaqueCBlock); message 'notifyOfDataReadyForSampleBuffer:completionHandler:';
  end;
{$endif}

{$ifdef TYPES}
type
  AVSampleBufferRequestDirection = NSInteger;
  AVSampleBufferRequestDirectionPtr = ^AVSampleBufferRequestDirection;

const
  AVSampleBufferRequestDirectionForward = +1;
  AVSampleBufferRequestDirectionNone = 0;
  AVSampleBufferRequestDirectionReverse = -1;

type
  AVSampleBufferRequestMode = NSInteger;
  AVSampleBufferRequestModePtr = ^AVSampleBufferRequestMode;

const
  AVSampleBufferRequestModeImmediate = 0;
  AVSampleBufferRequestModeScheduled = 1;
{$endif}

{$ifdef CLASSES}

type
  AVSampleBufferRequest = objcclass external (NSObject)
  private
    _request: AVSampleBufferRequestInternal;
  public
    function initWithStartCursor (startCursor: AVSampleCursor): instancetype; message 'initWithStartCursor:';
    function startCursor: AVSampleCursor; message 'startCursor';
    procedure setDirection(newValue: AVSampleBufferRequestDirection); message 'setDirection:';
    function direction: AVSampleBufferRequestDirection; message 'direction';
    procedure setLimitCursor(newValue: AVSampleCursor); message 'setLimitCursor:';
    function limitCursor: AVSampleCursor; message 'limitCursor';
    procedure setPreferredMinSampleCount(newValue: NSInteger); message 'setPreferredMinSampleCount:';
    function preferredMinSampleCount: NSInteger; message 'preferredMinSampleCount';
    procedure setMaxSampleCount(newValue: NSInteger); message 'setMaxSampleCount:';
    function maxSampleCount: NSInteger; message 'maxSampleCount';
    procedure setMode(newValue: AVSampleBufferRequestMode); message 'setMode:';
    function mode: AVSampleBufferRequestMode; message 'mode';
    procedure setOverrideTime(newValue: CMTime); message 'setOverrideTime:';
    function overrideTime: CMTime; message 'overrideTime';
  end;
{$endif}

{$endif}
