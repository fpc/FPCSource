{ Parsed from AVFoundation.framework AVSampleBufferDisplayLayer.h }


{$ifdef TYPES}
type
  AVSampleBufferDisplayLayerPtr = ^AVSampleBufferDisplayLayer;
{$endif}

{$ifdef TYPES}
type
  AVQueuedSampleBufferRenderingStatus = NSInteger;
  AVQueuedSampleBufferRenderingStatusPtr = ^AVQueuedSampleBufferRenderingStatus;

const
  AVQueuedSampleBufferRenderingStatusUnknown = 0;
  AVQueuedSampleBufferRenderingStatusRendering = 1;
  AVQueuedSampleBufferRenderingStatusFailed = 2;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  AVSampleBufferDisplayLayerFailedToDecodeNotification: NSString { available in 10_10, 8_0 }; cvar; external;
  AVSampleBufferDisplayLayerFailedToDecodeNotificationErrorKey: NSString { available in 10_10, 8_0 }; cvar; external;
{$endif}

{$ifdef CLASSES}

type
  AVSampleBufferDisplayLayer = objcclass external (CALayer)
  private
    _sampleBufferDisplayLayerInternal: AVSampleBufferDisplayLayerInternal;
  public
    procedure setControlTimebase(newValue: CMTimebaseRef); message 'setControlTimebase:';
    function controlTimebase: CMTimebaseRef; message 'controlTimebase';
    procedure setVideoGravity(newValue: NSString); message 'setVideoGravity:';
    function videoGravity: NSString; message 'videoGravity';
    function status: AVQueuedSampleBufferRenderingStatus; message 'status';
    function error: NSError; message 'error';
  end;


type
  AVSampleBufferDisplayLayerQueueManagement = objccategory external (AVSampleBufferDisplayLayer)
    procedure enqueueSampleBuffer (sampleBuffer: CMSampleBufferRef); message 'enqueueSampleBuffer:';
    procedure flush; message 'flush';
    procedure flushAndRemoveImage; message 'flushAndRemoveImage';
    function isReadyForMoreMediaData: ObjCBOOL; message 'isReadyForMoreMediaData';
    procedure requestMediaDataWhenReadyOnQueue_usingBlock (queue: dispatch_queue_t; block: OpaqueCBlock); message 'requestMediaDataWhenReadyOnQueue:usingBlock:';
    procedure stopRequestingMediaData; message 'stopRequestingMediaData';
  end;
{$endif}

