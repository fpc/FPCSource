{ Parsed from AVFoundation.framework AVAssetWriterInput.h }


{$ifdef TYPES}
type
  AVAssetWriterInputPtr = ^AVAssetWriterInput;
  AVAssetWriterInputPassDescriptionPtr = ^AVAssetWriterInputPassDescription;
  AVAssetWriterInputPixelBufferAdaptorPtr = ^AVAssetWriterInputPixelBufferAdaptor;
  AVAssetWriterInputMetadataAdaptorPtr = ^AVAssetWriterInputMetadataAdaptor;
{$endif}

{$ifdef CLASSES}

type
  AVAssetWriterInput = objcclass external (NSObject)
  private
    _internal: AVAssetWriterInputInternal;
  public
    class function assetWriterInputWithMediaType_outputSettings (mediaType: NSString; outputSettings: NSDictionary): AVAssetWriterInput; message 'assetWriterInputWithMediaType:outputSettings:';
    class function assetWriterInputWithMediaType_outputSettings_sourceFormatHint (mediaType: NSString; outputSettings: NSDictionary; sourceFormatHint: CMFormatDescriptionRef): AVAssetWriterInput; message 'assetWriterInputWithMediaType:outputSettings:sourceFormatHint:'; { available in 10_8, 6_0 }
    function initWithMediaType_outputSettings (mediaType: NSString; outputSettings: NSDictionary): instancetype; message 'initWithMediaType:outputSettings:';
    function initWithMediaType_outputSettings_sourceFormatHint (mediaType: NSString; outputSettings: NSDictionary; sourceFormatHint: CMFormatDescriptionRef): instancetype; message 'initWithMediaType:outputSettings:sourceFormatHint:'; { available in 10_8, 6_0 }
    function mediaType: NSString; message 'mediaType';
    function outputSettings: NSDictionary; message 'outputSettings';
    function sourceFormatHint: CMFormatDescriptionRef; message 'sourceFormatHint';
    procedure setMetadata(newValue: NSArray); message 'setMetadata:';
    function metadata: NSArray; message 'metadata';
    function isReadyForMoreMediaData: ObjCBOOL; message 'isReadyForMoreMediaData';
    procedure setExpectsMediaDataInRealTime(newValue: ObjCBOOL); message 'setExpectsMediaDataInRealTime:';
    function expectsMediaDataInRealTime: ObjCBOOL; message 'expectsMediaDataInRealTime';
    procedure requestMediaDataWhenReadyOnQueue_usingBlock (queue: dispatch_queue_t; block: OpaqueCBlock); message 'requestMediaDataWhenReadyOnQueue:usingBlock:';
    function appendSampleBuffer (sampleBuffer: CMSampleBufferRef): ObjCBOOL; message 'appendSampleBuffer:';
    procedure markAsFinished; message 'markAsFinished';
  end;


type
  AVAssetWriterInputLanguageProperties = objccategory external (AVAssetWriterInput)
    procedure setLanguageCode(newValue: NSString); message 'setLanguageCode:';
    function languageCode: NSString; message 'languageCode';
    procedure setExtendedLanguageTag(newValue: NSString); message 'setExtendedLanguageTag:';
    function extendedLanguageTag: NSString; message 'extendedLanguageTag';
  end;

type
  AVAssetWriterInputPropertiesForVisualCharacteristic = objccategory external (AVAssetWriterInput)
    procedure setNaturalSize(newValue: CGSize); message 'setNaturalSize:';
    function naturalSize: CGSize; message 'naturalSize';
    procedure setTransform(newValue: CGAffineTransform); message 'setTransform:';
    function transform: CGAffineTransform; message 'transform';
  end;

type
  AVAssetWriterInputPropertiesForAudibleCharacteristic = objccategory external (AVAssetWriterInput)
    procedure setPreferredVolume(newValue: single); message 'setPreferredVolume:';
    function preferredVolume: single; message 'preferredVolume';
  end;

type
  AVAssetWriterInputFileTypeSpecificProperties = objccategory external (AVAssetWriterInput)
    procedure setMarksOutputTrackAsEnabled(newValue: ObjCBOOL); message 'setMarksOutputTrackAsEnabled:';
    function marksOutputTrackAsEnabled: ObjCBOOL; message 'marksOutputTrackAsEnabled';
    procedure setMediaTimeScale(newValue: CMTimeScale); message 'setMediaTimeScale:';
    function mediaTimeScale: CMTimeScale; message 'mediaTimeScale';
    procedure setPreferredMediaChunkDuration(newValue: CMTime); message 'setPreferredMediaChunkDuration:';
    function preferredMediaChunkDuration: CMTime; message 'preferredMediaChunkDuration';
    procedure setPreferredMediaChunkAlignment(newValue: NSInteger); message 'setPreferredMediaChunkAlignment:';
    function preferredMediaChunkAlignment: NSInteger; message 'preferredMediaChunkAlignment';
    procedure setSampleReferenceBaseURL(newValue: NSURL); message 'setSampleReferenceBaseURL:';
    function sampleReferenceBaseURL: NSURL; message 'sampleReferenceBaseURL';
  end;

type
  AVAssetWriterInputTrackAssociations = objccategory external (AVAssetWriterInput)
    function canAddTrackAssociationWithTrackOfInput_type (input: AVAssetWriterInput; trackAssociationType: NSString): ObjCBOOL; message 'canAddTrackAssociationWithTrackOfInput:type:'; { available in 10_9, 7_0 }
    procedure addTrackAssociationWithTrackOfInput_type (input: AVAssetWriterInput; trackAssociationType: NSString); message 'addTrackAssociationWithTrackOfInput:type:'; { available in 10_9, 7_0 }
  end;

type
  AVAssetWriterInputMultiPass = objccategory external (AVAssetWriterInput)
    procedure setPerformsMultiPassEncodingIfSupported(newValue: ObjCBOOL); message 'setPerformsMultiPassEncodingIfSupported:';
    function performsMultiPassEncodingIfSupported: ObjCBOOL; message 'performsMultiPassEncodingIfSupported';
    function canPerformMultiplePasses: ObjCBOOL; message 'canPerformMultiplePasses';
    function currentPassDescription: AVAssetWriterInputPassDescription; message 'currentPassDescription';
    procedure respondToEachPassDescriptionOnQueue_usingBlock (queue: dispatch_queue_t; block: dispatch_block_t); message 'respondToEachPassDescriptionOnQueue:usingBlock:'; { available in 10_10, 8_0 }
    procedure markCurrentPassAsFinished; message 'markCurrentPassAsFinished'; { available in 10_10, 8_0 }
  end;


type
  AVAssetWriterInputPassDescription = objcclass external (NSObject)
  private
    _internal: AVAssetWriterInputPassDescriptionInternal;
  public
    function sourceTimeRanges: NSArray; message 'sourceTimeRanges';
  end;

type
  AVAssetWriterInputPixelBufferAdaptor = objcclass external (NSObject)
  private
    _internal: AVAssetWriterInputPixelBufferAdaptorInternal;
  public
    class function assetWriterInputPixelBufferAdaptorWithAssetWriterInput_sourcePixelBufferAttributes (input: AVAssetWriterInput; sourcePixelBufferAttributes: NSDictionary): instancetype; message 'assetWriterInputPixelBufferAdaptorWithAssetWriterInput:sourcePixelBufferAttributes:';
    function initWithAssetWriterInput_sourcePixelBufferAttributes (input: AVAssetWriterInput; sourcePixelBufferAttributes: NSDictionary): instancetype; message 'initWithAssetWriterInput:sourcePixelBufferAttributes:';
    function assetWriterInput: AVAssetWriterInput; message 'assetWriterInput';
    function sourcePixelBufferAttributes: NSDictionary; message 'sourcePixelBufferAttributes';
    function pixelBufferPool: CVPixelBufferPoolRef; message 'pixelBufferPool';
    function appendPixelBuffer_withPresentationTime (pixelBuffer: CVPixelBufferRef; presentationTime: CMTime): ObjCBOOL; message 'appendPixelBuffer:withPresentationTime:';
  end;

type
  AVAssetWriterInputMetadataAdaptor = objcclass external (NSObject)
  private
    _internal: AVAssetWriterInputMetadataAdaptorInternal;
  public
    class function assetWriterInputMetadataAdaptorWithAssetWriterInput (input: AVAssetWriterInput): instancetype; message 'assetWriterInputMetadataAdaptorWithAssetWriterInput:';
    function initWithAssetWriterInput (input: AVAssetWriterInput): instancetype; message 'initWithAssetWriterInput:';
    function assetWriterInput: AVAssetWriterInput; message 'assetWriterInput';
    function appendTimedMetadataGroup (timedMetadataGroup: AVTimedMetadataGroup): ObjCBOOL; message 'appendTimedMetadataGroup:';
  end;
{$endif}

