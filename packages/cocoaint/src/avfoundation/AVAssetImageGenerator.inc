{ Parsed from AVFoundation.framework AVAssetImageGenerator.h }


{ Types from AVAssetImageGenerator }
{$ifdef TYPES}

type
  AVAssetImageGeneratorCompletionHandler = OpaqueCBlock;
{$endif}


{$ifdef TYPES}
type
  AVAssetImageGeneratorPtr = ^AVAssetImageGenerator;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  AVAssetImageGeneratorApertureModeCleanAperture: NSString { available in 10_7, 4_0 }; cvar; external;
  AVAssetImageGeneratorApertureModeProductionAperture: NSString { available in 10_7, 4_0 }; cvar; external;
  AVAssetImageGeneratorApertureModeEncodedPixels: NSString { available in 10_7, 4_0 }; cvar; external;
{$endif}

{$ifdef TYPES}
type
  AVAssetImageGeneratorResult = NSInteger;
  AVAssetImageGeneratorResultPtr = ^AVAssetImageGeneratorResult;

const
  AVAssetImageGeneratorSucceeded = 0;
  AVAssetImageGeneratorFailed = 1;
  AVAssetImageGeneratorCancelled = 2;
{$endif}

{$ifdef CLASSES}

type
  AVAssetImageGenerator = objcclass external (NSObject)
  private
    _priv: AVAssetImageGeneratorInternal;
  public
    function asset: AVAsset; message 'asset';
    procedure setAppliesPreferredTrackTransform(newValue: ObjCBOOL); message 'setAppliesPreferredTrackTransform:';
    function appliesPreferredTrackTransform: ObjCBOOL; message 'appliesPreferredTrackTransform';
    procedure setMaximumSize(newValue: CGSize); message 'setMaximumSize:';
    function maximumSize: CGSize; message 'maximumSize';
    procedure setApertureMode(newValue: NSString); message 'setApertureMode:';
    function apertureMode: NSString; message 'apertureMode';
    procedure setVideoComposition(newValue: AVVideoComposition); message 'setVideoComposition:';
    function videoComposition: AVVideoComposition; message 'videoComposition';
    function customVideoCompositor: AVVideoCompositingProtocol; message 'customVideoCompositor';
    procedure setRequestedTimeToleranceBefore(newValue: CMTime); message 'setRequestedTimeToleranceBefore:';
    function requestedTimeToleranceBefore: CMTime; message 'requestedTimeToleranceBefore';
    procedure setRequestedTimeToleranceAfter(newValue: CMTime); message 'setRequestedTimeToleranceAfter:';
    function requestedTimeToleranceAfter: CMTime; message 'requestedTimeToleranceAfter';
    class function assetImageGeneratorWithAsset (asset_: AVAsset): AVAssetImageGenerator; message 'assetImageGeneratorWithAsset:';
    function initWithAsset (asset_: AVAsset): instancetype; message 'initWithAsset:';
    function copyCGImageAtTime_actualTime_error (requestedTime: CMTime; actualTime: CMTimePtr; outError: NSErrorPtr): CGImageRef; message 'copyCGImageAtTime:actualTime:error:';
    procedure generateCGImagesAsynchronouslyForTimes_completionHandler (requestedTimes: NSArray; handler: AVAssetImageGeneratorCompletionHandler); message 'generateCGImagesAsynchronouslyForTimes:completionHandler:';
    procedure cancelAllCGImageGeneration; message 'cancelAllCGImageGeneration';
  end;
{$endif}

