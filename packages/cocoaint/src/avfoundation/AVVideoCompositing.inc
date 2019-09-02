{ Parsed from AVFoundation.framework AVVideoCompositing.h }


{$ifdef TYPES}
type
  AVVideoCompositionRenderContextPtr = ^AVVideoCompositionRenderContext;
  AVAsynchronousVideoCompositionRequestPtr = ^AVAsynchronousVideoCompositionRequest;
  AVVideoCompositingProtocolPtr = ^AVVideoCompositingProtocol;
  AVVideoCompositionInstructionProtocolPtr = ^AVVideoCompositionInstructionProtocol;
{$endif}

{$ifdef TYPES}
type
  AVPixelAspectRatio = record
    horizontalSpacing: NSInteger;
    verticalSpacing: NSInteger;
  end;
type
  AVPixelAspectRatioPtr = ^AVPixelAspectRatio;
type
  AVEdgeWidths = record
    left: CGFloat;
    top: CGFloat;
    right: CGFloat;
    bottom: CGFloat;
  end;
type
  AVEdgeWidthsPtr = ^AVEdgeWidths;
{$endif}

{$ifdef CLASSES}

type
  AVVideoCompositionRenderContext = objcclass external (NSObject)
  private
    _internal: AVVideoCompositionRenderContextInternal;
  public
    function size: CGSize; message 'size';
    function renderTransform: CGAffineTransform; message 'renderTransform';
    function renderScale: single; message 'renderScale';
    function pixelAspectRatio: AVPixelAspectRatio; message 'pixelAspectRatio';
    function edgeWidths: AVEdgeWidths; message 'edgeWidths';
    function highQualityRendering: ObjCBOOL; message 'highQualityRendering';
    function videoComposition: AVVideoComposition; message 'videoComposition';
    function newPixelBuffer: CVPixelBufferRef; message 'newPixelBuffer';
  end;
{$endif}

{$ifdef PROTOCOLS}

type
  AVVideoCompositingProtocol = objcprotocol external name 'AVVideoCompositing' (NSObjectProtocol)
  required
    function sourcePixelBufferAttributes: NSDictionary; message 'sourcePixelBufferAttributes';
    function requiredPixelBufferAttributesForRenderContext: NSDictionary; message 'requiredPixelBufferAttributesForRenderContext';
    procedure renderContextChanged (newRenderContext: AVVideoCompositionRenderContext); message 'renderContextChanged:';
    procedure startVideoCompositionRequest (asyncVideoCompositionRequest: AVAsynchronousVideoCompositionRequest); message 'startVideoCompositionRequest:';
  optional
    procedure cancelAllPendingVideoCompositionRequests; message 'cancelAllPendingVideoCompositionRequests';
  end;
{$endif}

{$ifdef CLASSES}

type
  AVAsynchronousVideoCompositionRequest = objcclass external (NSObject, NSCopyingProtocol)
  private
    _internal: AVAsynchronousVideoCompositionRequestInternal;
  public
    function renderContext: AVVideoCompositionRenderContext; message 'renderContext';
    function compositionTime: CMTime; message 'compositionTime';
    function sourceTrackIDs: NSArray; message 'sourceTrackIDs';
    function videoCompositionInstruction: AVVideoCompositionInstructionProtocol; message 'videoCompositionInstruction';
    function sourceFrameByTrackID (trackID: CMPersistentTrackID): CVPixelBufferRef; message 'sourceFrameByTrackID:';
    procedure finishWithComposedVideoFrame (composedVideoFrame: CVPixelBufferRef); message 'finishWithComposedVideoFrame:';
    procedure finishWithError (error: NSError); message 'finishWithError:';
    procedure finishCancelledRequest; message 'finishCancelledRequest';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
  end;
{$endif}

{$ifdef PROTOCOLS}

type
  AVVideoCompositionInstructionProtocol = objcprotocol external name 'AVVideoCompositionInstruction' (NSObjectProtocol)
  required
    function timeRange: CMTimeRange; message 'timeRange';
    function enablePostProcessing: ObjCBOOL; message 'enablePostProcessing';
    function containsTweening: ObjCBOOL; message 'containsTweening';
    function requiredSourceTrackIDs: NSArray; message 'requiredSourceTrackIDs';
    function passthroughTrackID: CMPersistentTrackID; message 'passthroughTrackID';
  end;
{$endif}

