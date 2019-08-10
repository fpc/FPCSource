{ Parsed from AVFoundation.framework AVVideoComposition.h }


{ Types from AVVideoComposition }
{$ifdef TYPES}

{$endif}


{ Types from AVMutableVideoComposition }
{$ifdef TYPES}

{$endif}


{$ifdef TYPES}
type
  AVVideoCompositionPtr = ^AVVideoComposition;
  AVMutableVideoCompositionPtr = ^AVMutableVideoComposition;
  AVVideoCompositionInstructionPtr = ^AVVideoCompositionInstruction;
  AVMutableVideoCompositionInstructionPtr = ^AVMutableVideoCompositionInstruction;
  AVVideoCompositionLayerInstructionPtr = ^AVVideoCompositionLayerInstruction;
  AVMutableVideoCompositionLayerInstructionPtr = ^AVMutableVideoCompositionLayerInstruction;
  AVVideoCompositionCoreAnimationToolPtr = ^AVVideoCompositionCoreAnimationTool;
  AVVideoCompositionValidationHandlingProtocolPtr = ^AVVideoCompositionValidationHandlingProtocol;
{$endif}

{$ifdef CLASSES}

type
  AVVideoComposition = objcclass external (NSObject, NSCopyingProtocol, NSMutableCopyingProtocol)
  private
    _videoComposition: AVVideoCompositionInternal;
  public
    class function videoCompositionWithPropertiesOfAsset (asset: AVAsset): AVVideoComposition; message 'videoCompositionWithPropertiesOfAsset:'; { available in 10_9, 6_0 }
    function customVideoCompositorClass: AVVideoCompositingProtocol; message 'customVideoCompositorClass';
    function frameDuration: CMTime; message 'frameDuration';
    function renderSize: CGSize; message 'renderSize';
    {$if defined(TARGET_OS_IPHONE)}
    function renderScale: single; message 'renderScale';
    {$endif}
    function instructions: NSArray; message 'instructions';
    function animationTool: AVVideoCompositionCoreAnimationTool; message 'animationTool';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    function mutableCopyWithZone (zone: NSZonePtr): id; message 'mutableCopyWithZone:';
  end;

type
  AVMutableVideoComposition = objcclass external (AVVideoComposition)
  private
    _mutableVideoComposition: AVMutableVideoCompositionInternal;
  public
    class function videoComposition: AVMutableVideoComposition; message 'videoComposition';
    class function videoCompositionWithPropertiesOfAsset (asset: AVAsset): AVMutableVideoComposition; message 'videoCompositionWithPropertiesOfAsset:'; { available in 10_9, 6_0 }
    procedure setCustomVideoCompositorClass(newValue: AVVideoCompositingProtocol); message 'setCustomVideoCompositorClass:';
    function customVideoCompositorClass: AVVideoCompositingProtocol; message 'customVideoCompositorClass';
    procedure setFrameDuration(newValue: CMTime); message 'setFrameDuration:';
    function frameDuration: CMTime; message 'frameDuration';
    procedure setRenderSize(newValue: CGSize); message 'setRenderSize:';
    function renderSize: CGSize; message 'renderSize';
    {$if defined(TARGET_OS_IPHONE)}
    procedure setRenderScale(newValue: single); message 'setRenderScale:';
    function renderScale: single; message 'renderScale';
    {$endif}
    procedure setInstructions(newValue: NSArray); message 'setInstructions:';
    function instructions: NSArray; message 'instructions';
    procedure setAnimationTool(newValue: AVVideoCompositionCoreAnimationTool); message 'setAnimationTool:';
    function animationTool: AVVideoCompositionCoreAnimationTool; message 'animationTool';
  end;

type
  AVVideoCompositionInstruction = objcclass external (NSObject, NSSecureCodingProtocol, NSCopyingProtocol, NSMutableCopyingProtocol, AVVideoCompositionInstructionProtocol)
  private
    _instruction: AVVideoCompositionInstructionInternal;
  public
    function timeRange: CMTimeRange; message 'timeRange';
    procedure setBackgroundColor(newValue: CGColorRef); message 'setBackgroundColor:';
    function backgroundColor: CGColorRef; message 'backgroundColor';
    function layerInstructions: NSArray; message 'layerInstructions';
    function enablePostProcessing: ObjCBOOL; message 'enablePostProcessing';
    function requiredSourceTrackIDs: NSArray; message 'requiredSourceTrackIDs';
    function passthroughTrackID: CMPersistentTrackID; message 'passthroughTrackID';

    { Adopted protocols }
    function containsTweening: ObjCBOOL; message 'containsTweening';
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
    function mutableCopyWithZone (zone: NSZonePtr): id; message 'mutableCopyWithZone:';
    class function supportsSecureCoding: ObjCBOOL; message 'supportsSecureCoding';
  end;

type
  AVMutableVideoCompositionInstruction = objcclass external (AVVideoCompositionInstruction)
  private
    _mutableInstruction: AVMutableVideoCompositionInstructionInternal;
  public
    class function videoCompositionInstruction: AVMutableVideoCompositionInstruction; message 'videoCompositionInstruction';
    procedure setTimeRange(newValue: CMTimeRange); message 'setTimeRange:';
    function timeRange: CMTimeRange; message 'timeRange';
    procedure setBackgroundColor(newValue: CGColorRef); message 'setBackgroundColor:';
    function backgroundColor: CGColorRef; message 'backgroundColor';
    procedure setLayerInstructions(newValue: NSArray); message 'setLayerInstructions:';
    function layerInstructions: NSArray; message 'layerInstructions';
    procedure setEnablePostProcessing(newValue: ObjCBOOL); message 'setEnablePostProcessing:';
    function enablePostProcessing: ObjCBOOL; message 'enablePostProcessing';
  end;

type
  AVVideoCompositionLayerInstruction = objcclass external (NSObject, NSSecureCodingProtocol, NSCopyingProtocol, NSMutableCopyingProtocol)
  private
    _layerInstruction: AVVideoCompositionLayerInstructionInternal;
  public
    function trackID: CMPersistentTrackID; message 'trackID';
    function getTransformRampForTime_startTransform_endTransform_timeRange (time: CMTime; startTransform: CGAffineTransformPtr; endTransform: CGAffineTransformPtr; timeRange: CMTimeRangePtr): ObjCBOOL; message 'getTransformRampForTime:startTransform:endTransform:timeRange:';
    function getOpacityRampForTime_startOpacity_endOpacity_timeRange (time: CMTime; startOpacity: psingle; endOpacity: psingle; timeRange: CMTimeRangePtr): ObjCBOOL; message 'getOpacityRampForTime:startOpacity:endOpacity:timeRange:';
    function getCropRectangleRampForTime_startCropRectangle_endCropRectangle_timeRange (time: CMTime; startCropRectangle: CGRectPtr; endCropRectangle: CGRectPtr; timeRange: CMTimeRangePtr): ObjCBOOL; message 'getCropRectangleRampForTime:startCropRectangle:endCropRectangle:timeRange:'; { available in 10_9, 7_0 }

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
    function mutableCopyWithZone (zone: NSZonePtr): id; message 'mutableCopyWithZone:';
    class function supportsSecureCoding: ObjCBOOL; message 'supportsSecureCoding';
  end;

type
  AVMutableVideoCompositionLayerInstruction = objcclass external (AVVideoCompositionLayerInstruction)
  private
    _mutableLayerInstruction: AVMutableVideoCompositionLayerInstructionInternal;
  public
    class function videoCompositionLayerInstructionWithAssetTrack (track: AVAssetTrack): AVMutableVideoCompositionLayerInstruction; message 'videoCompositionLayerInstructionWithAssetTrack:';
    class function videoCompositionLayerInstruction: AVMutableVideoCompositionLayerInstruction; message 'videoCompositionLayerInstruction';
    procedure setTrackID(newValue: CMPersistentTrackID); message 'setTrackID:';
    function trackID: CMPersistentTrackID; message 'trackID';
    procedure setTransformRampFromStartTransform_toEndTransform_timeRange (startTransform: CGAffineTransform; endTransform: CGAffineTransform; timeRange: CMTimeRange); message 'setTransformRampFromStartTransform:toEndTransform:timeRange:';
    procedure setTransform_atTime (transform: CGAffineTransform; time: CMTime); message 'setTransform:atTime:';
    procedure setOpacityRampFromStartOpacity_toEndOpacity_timeRange (startOpacity: single; endOpacity: single; timeRange: CMTimeRange); message 'setOpacityRampFromStartOpacity:toEndOpacity:timeRange:';
    procedure setOpacity_atTime (opacity: single; time: CMTime); message 'setOpacity:atTime:';
    procedure setCropRectangleRampFromStartCropRectangle_toEndCropRectangle_timeRange (startCropRectangle: CGRect; endCropRectangle: CGRect; timeRange: CMTimeRange); message 'setCropRectangleRampFromStartCropRectangle:toEndCropRectangle:timeRange:'; { available in 10_9, 7_0 }
    procedure setCropRectangle_atTime (cropRectangle: CGRect; time: CMTime); message 'setCropRectangle:atTime:'; { available in 10_9, 7_0 }
  end;

type
  AVVideoCompositionCoreAnimationTool = objcclass external (NSObject)
  private
    _videoCompositionTool: AVVideoCompositionCoreAnimationToolInternal;
  public
    class function videoCompositionCoreAnimationToolWithAdditionalLayer_asTrackID (layer: CALayer; trackID: CMPersistentTrackID): AVVideoCompositionCoreAnimationTool; message 'videoCompositionCoreAnimationToolWithAdditionalLayer:asTrackID:';
    class function videoCompositionCoreAnimationToolWithPostProcessingAsVideoLayer_inLayer (videoLayer: CALayer; animationLayer: CALayer): AVVideoCompositionCoreAnimationTool; message 'videoCompositionCoreAnimationToolWithPostProcessingAsVideoLayer:inLayer:';
    class function videoCompositionCoreAnimationToolWithPostProcessingAsVideoLayers_inLayer (videoLayers: NSArray; animationLayer: CALayer): AVVideoCompositionCoreAnimationTool; message 'videoCompositionCoreAnimationToolWithPostProcessingAsVideoLayers:inLayer:'; { available in 10_9, 7_0 }
  end;


type
  AVAssetVideoCompositionUtility = objccategory external (AVAsset)
    function unusedTrackID: CMPersistentTrackID; message 'unusedTrackID';
  end;

type
  AVVideoCompositionValidation = objccategory external (AVVideoComposition)
    function isValidForAsset_timeRange_validationDelegate (asset: AVAsset; timeRange: CMTimeRange; validationDelegate: AVVideoCompositionValidationHandlingProtocol): ObjCBOOL; message 'isValidForAsset:timeRange:validationDelegate:'; { available in 10_8, 5_0 }
  end;
{$endif}

{$ifdef PROTOCOLS}

type
  AVVideoCompositionValidationHandlingProtocol = objcprotocol external name 'AVVideoCompositionValidationHandling' (NSObjectProtocol)
  optional
    function videoComposition_shouldContinueValidatingAfterFindingInvalidValueForKey (videoComposition: AVVideoComposition; key: NSString): ObjCBOOL; message 'videoComposition:shouldContinueValidatingAfterFindingInvalidValueForKey:'; { available in 10_8, 5_0 }
    function videoComposition_shouldContinueValidatingAfterFindingEmptyTimeRange (videoComposition: AVVideoComposition; timeRange: CMTimeRange): ObjCBOOL; message 'videoComposition:shouldContinueValidatingAfterFindingEmptyTimeRange:'; { available in 10_8, 5_0 }
    function videoComposition_shouldContinueValidatingAfterFindingInvalidTimeRangeInInstruction (videoComposition: AVVideoComposition; videoCompositionInstruction: AVVideoCompositionInstructionProtocol): ObjCBOOL; message 'videoComposition:shouldContinueValidatingAfterFindingInvalidTimeRangeInInstruction:'; { available in 10_8, 5_0 }
    function videoComposition_shouldContinueValidatingAfterFindingInvalidTrackIDInInstruction_layerInstruction_asset (videoComposition: AVVideoComposition; videoCompositionInstruction: AVVideoCompositionInstructionProtocol; layerInstruction: AVVideoCompositionLayerInstruction; asset: AVAsset): ObjCBOOL; message 'videoComposition:shouldContinueValidatingAfterFindingInvalidTrackIDInInstruction:layerInstruction:asset:'; { available in 10_8, 5_0 }
  end;
{$endif}

