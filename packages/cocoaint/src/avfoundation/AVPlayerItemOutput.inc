{ Parsed from AVFoundation.framework AVPlayerItemOutput.h }


{ Types from AVPlayerItemOutput }
{$ifdef TYPES}

{$endif}


{$ifdef TYPES}
type
  AVPlayerItemOutputPtr = ^AVPlayerItemOutput;
  AVPlayerItemVideoOutputPtr = ^AVPlayerItemVideoOutput;
  AVPlayerItemLegibleOutputPtr = ^AVPlayerItemLegibleOutput;
  AVPlayerItemMetadataOutputPtr = ^AVPlayerItemMetadataOutput;
  AVPlayerItemOutputPullDelegateProtocolPtr = ^AVPlayerItemOutputPullDelegateProtocol;
  AVPlayerItemLegibleOutputPushDelegateProtocolPtr = ^AVPlayerItemLegibleOutputPushDelegateProtocol;
  AVPlayerItemOutputPushDelegateProtocolPtr = ^AVPlayerItemOutputPushDelegateProtocol;
  AVPlayerItemMetadataOutputPushDelegateProtocolPtr = ^AVPlayerItemMetadataOutputPushDelegateProtocol;
{$endif}

{$ifdef CLASSES}

type
  AVPlayerItemOutput = objcclass external (NSObject)
  private
    _outputInternal: AVPlayerItemOutputInternal;
  public
    function itemTimeForHostTime (hostTimeInSeconds: CFTimeInterval): CMTime; message 'itemTimeForHostTime:';
    function itemTimeForMachAbsoluteTime (machAbsoluteTime: cint64): CMTime; message 'itemTimeForMachAbsoluteTime:';
    {$if  not defined(TARGET_OS_IPHONE)}
    function itemTimeForCVTimeStamp (timestamp: CVTimeStamp): CMTime; message 'itemTimeForCVTimeStamp:'; { available in 10_8, NA }
    {$endif}
    procedure setSuppressesPlayerRendering(newValue: ObjCBOOL); message 'setSuppressesPlayerRendering:';
    function suppressesPlayerRendering: ObjCBOOL; message 'suppressesPlayerRendering';
  end;

type
  AVPlayerItemVideoOutput = objcclass external (AVPlayerItemOutput)
  private
    _videoOutputInternal: AVPlayerItemVideoOutputInternal;
  public
    function initWithPixelBufferAttributes (pixelBufferAttributes: NSDictionary): instancetype; message 'initWithPixelBufferAttributes:';
    function hasNewPixelBufferForItemTime (itemTime: CMTime): ObjCBOOL; message 'hasNewPixelBufferForItemTime:';
    function copyPixelBufferForItemTime_itemTimeForDisplay (itemTime: CMTime; outItemTimeForDisplay: CMTimePtr): CVPixelBufferRef; message 'copyPixelBufferForItemTime:itemTimeForDisplay:';
    procedure setDelegate_queue (delegate: AVPlayerItemOutputPullDelegateProtocol; delegateQueue: dispatch_queue_t); message 'setDelegate:queue:';
    procedure requestNotificationOfMediaDataChangeWithAdvanceInterval (interval: NSTimeInterval); message 'requestNotificationOfMediaDataChangeWithAdvanceInterval:';
    function delegate: AVPlayerItemOutputPullDelegateProtocol; message 'delegate';
    function delegateQueue: dispatch_queue_t; message 'delegateQueue';
  end;
{$endif}

{$ifdef PROTOCOLS}

type
  AVPlayerItemOutputPullDelegateProtocol = objcprotocol external name 'AVPlayerItemOutputPullDelegate' (NSObjectProtocol)
  optional
    procedure outputMediaDataWillChange (sender: AVPlayerItemOutput); message 'outputMediaDataWillChange:'; { available in 10_8, 6_0 }
    procedure outputSequenceWasFlushed (output: AVPlayerItemOutput); message 'outputSequenceWasFlushed:'; { available in 10_8, 6_0 }
  end;
{$endif}

{$ifdef CLASSES}

type
  AVPlayerItemLegibleOutput = objcclass external (AVPlayerItemOutput)
  private
    _legibleOutputInternal: AVPlayerItemLegibleOutputInternal;
  public
    procedure setDelegate_queue (delegate: AVPlayerItemLegibleOutputPushDelegateProtocol; delegateQueue: dispatch_queue_t); message 'setDelegate:queue:';
    function delegate: AVPlayerItemLegibleOutputPushDelegateProtocol; message 'delegate';
    function delegateQueue: dispatch_queue_t; message 'delegateQueue';
    procedure setAdvanceIntervalForDelegateInvocation(newValue: NSTimeInterval); message 'setAdvanceIntervalForDelegateInvocation:';
    function advanceIntervalForDelegateInvocation: NSTimeInterval; message 'advanceIntervalForDelegateInvocation';
  end;


type
  AVPlayerItemLegibleOutput_NativeRepresentation = objccategory external (AVPlayerItemLegibleOutput)
    function initWithMediaSubtypesForNativeRepresentation (subtypes: NSArray): instancetype; message 'initWithMediaSubtypesForNativeRepresentation:';
  end;

type
  AVPlayerItemLegibleOutput_TextStylingResolution = objccategory external (AVPlayerItemLegibleOutput)
    procedure setTextStylingResolution(newValue: NSString); message 'setTextStylingResolution:';
    function textStylingResolution: NSString; message 'textStylingResolution';
  end;
{$endif}

{$ifdef PROTOCOLS}
type
  AVPlayerItemOutputPushDelegateProtocol = objcprotocol external name 'AVPlayerItemOutputPushDelegate' (NSObjectProtocol)
  optional
    procedure outputSequenceWasFlushed (output: AVPlayerItemOutput); message 'outputSequenceWasFlushed:';
  end;

type
  AVPlayerItemLegibleOutputPushDelegateProtocol = objcprotocol external name 'AVPlayerItemLegibleOutputPushDelegate' (AVPlayerItemOutputPushDelegateProtocol)
  optional
    procedure legibleOutput_didOutputAttributedStrings_nativeSampleBuffers_forItemTime (output: AVPlayerItemLegibleOutput; strings: NSArray; nativeSamples: NSArray; itemTime: CMTime); message 'legibleOutput:didOutputAttributedStrings:nativeSampleBuffers:forItemTime:'; { available in 10_9, 7_0 }
  end;
{$endif}

{$ifdef CLASSES}

type
  AVPlayerItemMetadataOutput = objcclass external (AVPlayerItemOutput)
  private
    _metadataOutputInternal: AVPlayerItemMetadataOutputInternal;
  public
    function initWithIdentifiers (identifiers: NSArray): instancetype; message 'initWithIdentifiers:';
    procedure setDelegate_queue (delegate: AVPlayerItemMetadataOutputPushDelegateProtocol; delegateQueue: dispatch_queue_t); message 'setDelegate:queue:';
    function delegate: AVPlayerItemMetadataOutputPushDelegateProtocol; message 'delegate';
    function delegateQueue: dispatch_queue_t; message 'delegateQueue';
    procedure setAdvanceIntervalForDelegateInvocation(newValue: NSTimeInterval); message 'setAdvanceIntervalForDelegateInvocation:';
    function advanceIntervalForDelegateInvocation: NSTimeInterval; message 'advanceIntervalForDelegateInvocation';
  end;
{$endif}

{$ifdef PROTOCOLS}

type
  AVPlayerItemMetadataOutputPushDelegateProtocol = objcprotocol external name 'AVPlayerItemMetadataOutputPushDelegate' (AVPlayerItemOutputPushDelegateProtocol)
  optional
    procedure metadataOutput_didOutputTimedMetadataGroups_fromPlayerItemTrack (output: AVPlayerItemMetadataOutput; groups: NSArray; track: AVPlayerItemTrack); message 'metadataOutput:didOutputTimedMetadataGroups:fromPlayerItemTrack:'; { available in 10_10, 8_0 }
  end;
{$endif}

