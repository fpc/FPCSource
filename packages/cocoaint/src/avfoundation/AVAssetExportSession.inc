{ Parsed from AVFoundation.framework AVAssetExportSession.h }


{ Types from AVAssetExportSession }
{$ifdef TYPES}


{$endif}


{$ifdef TYPES}
type
  AVAssetExportSessionPtr = ^AVAssetExportSession;
{$endif}

{$if defined(TARGET_OS_IPHONE)}
{$ifdef EXTERNAL_SYMBOLS}
var
  AVAssetExportPresetLowQuality: NSString { available in 4_0 }; cvar; external;
  AVAssetExportPresetMediumQuality: NSString { available in 4_0 }; cvar; external;
  AVAssetExportPresetHighestQuality: NSString { available in 4_0 }; cvar; external;
{$endif}

{$endif}
{$ifdef EXTERNAL_SYMBOLS}
var
  AVAssetExportPreset640x480: NSString { available in 10_7, 4_0 }; cvar; external;
  AVAssetExportPreset960x540: NSString { available in 10_7, 4_0 }; cvar; external;
  AVAssetExportPreset1280x720: NSString { available in 10_7, 4_0 }; cvar; external;
  AVAssetExportPreset1920x1080: NSString { available in 10_7, 5_0 }; cvar; external;
  AVAssetExportPreset3840x2160: NSString { available in 10_10, NA }; cvar; external;
  AVAssetExportPresetAppleM4A: NSString { available in 10_7, 4_0 }; cvar; external;
  AVAssetExportPresetPassthrough: NSString { available in 10_7, 4_0 }; cvar; external;
{$endif}

{$if (defined(TARGET_OS_MAC) and not (defined(TARGET_OS_EMBEDDED) or defined(TARGET_OS_IPHONE)))}
{$ifdef EXTERNAL_SYMBOLS}
var
  AVAssetExportPresetAppleM4VCellular: NSString { available in 10_7, NA }; cvar; external;
  AVAssetExportPresetAppleM4ViPod: NSString { available in 10_7, NA }; cvar; external;
  AVAssetExportPresetAppleM4V480pSD: NSString { available in 10_7, NA }; cvar; external;
  AVAssetExportPresetAppleM4VAppleTV: NSString { available in 10_7, NA }; cvar; external;
  AVAssetExportPresetAppleM4VWiFi: NSString { available in 10_7, NA }; cvar; external;
  AVAssetExportPresetAppleM4V720pHD: NSString { available in 10_7, NA }; cvar; external;
  AVAssetExportPresetAppleM4V1080pHD: NSString { available in 10_8, NA }; cvar; external;
  AVAssetExportPresetAppleProRes422LPCM: NSString { available in 10_7, NA }; cvar; external;
{$endif}

{$endif}
{$ifdef TYPES}
type
  AVAssetExportSessionStatus = NSInteger;
  AVAssetExportSessionStatusPtr = ^AVAssetExportSessionStatus;

const
  AVAssetExportSessionStatusUnknown = 0;
  AVAssetExportSessionStatusWaiting = 1;
  AVAssetExportSessionStatusExporting = 2;
  AVAssetExportSessionStatusCompleted = 3;
  AVAssetExportSessionStatusFailed = 4;
  AVAssetExportSessionStatusCancelled = 5;
{$endif}

{$ifdef CLASSES}

type
  AVAssetExportSession = objcclass external (NSObject)
  private
    _exportSession: AVAssetExportSessionInternal;
  public
    class function allExportPresets: NSArray; message 'allExportPresets';
    class function exportPresetsCompatibleWithAsset (asset: AVAsset): NSArray; message 'exportPresetsCompatibleWithAsset:';
    class procedure determineCompatibilityOfExportPreset_withAsset_outputFileType_completionHandler (presetName: NSString; asset: AVAsset; outputFileType: NSString; handler: OpaqueCBlock); message 'determineCompatibilityOfExportPreset:withAsset:outputFileType:completionHandler:'; { available in 10_9, 6_0 }
    class function exportSessionWithAsset_presetName (asset: AVAsset; presetName: NSString): AVAssetExportSession; message 'exportSessionWithAsset:presetName:'; { available in 10_7, 4_1 }
    function initWithAsset_presetName (asset: AVAsset; presetName: NSString): instancetype; message 'initWithAsset:presetName:';
    function presetName: NSString; message 'presetName';
    function asset: AVAsset; message 'asset';
    function supportedFileTypes: NSArray; message 'supportedFileTypes';
    procedure setOutputFileType(newValue: NSString); message 'setOutputFileType:';
    function outputFileType: NSString; message 'outputFileType';
    procedure setOutputURL(newValue: NSURL); message 'setOutputURL:';
    function outputURL: NSURL; message 'outputURL';
    function status: AVAssetExportSessionStatus; message 'status';
    function error: NSError; message 'error';
    function progress: single; message 'progress';
    {$if defined(TARGET_OS_IPHONE)}
    function maxDuration: CMTime; message 'maxDuration';
    {$endif}
    function estimatedOutputFileLength: clonglong; message 'estimatedOutputFileLength';
    procedure setTimeRange(newValue: CMTimeRange); message 'setTimeRange:';
    function timeRange: CMTimeRange; message 'timeRange';
    procedure setMetadata(newValue: NSArray); message 'setMetadata:';
    function metadata: NSArray; message 'metadata';
    procedure setMetadataItemFilter(newValue: AVMetadataItemFilter); message 'setMetadataItemFilter:';
    function metadataItemFilter: AVMetadataItemFilter; message 'metadataItemFilter';
    {$if defined(TARGET_OS_IPHONE)}
    procedure setFileLengthLimit(newValue: clonglong); message 'setFileLengthLimit:';
    function fileLengthLimit: clonglong; message 'fileLengthLimit';
    {$endif}
    procedure setAudioTimePitchAlgorithm(newValue: NSString); message 'setAudioTimePitchAlgorithm:';
    function audioTimePitchAlgorithm: NSString; message 'audioTimePitchAlgorithm';
    procedure setAudioMix(newValue: AVAudioMix); message 'setAudioMix:';
    function audioMix: AVAudioMix; message 'audioMix';
    procedure setVideoComposition(newValue: AVVideoComposition); message 'setVideoComposition:';
    function videoComposition: AVVideoComposition; message 'videoComposition';
    function customVideoCompositor: AVVideoCompositingProtocol; message 'customVideoCompositor';
    procedure setShouldOptimizeForNetworkUse(newValue: ObjCBOOL); message 'setShouldOptimizeForNetworkUse:';
    function shouldOptimizeForNetworkUse: ObjCBOOL; message 'shouldOptimizeForNetworkUse';
    procedure setCanPerformMultiplePassesOverSourceMediaData(newValue: ObjCBOOL); message 'setCanPerformMultiplePassesOverSourceMediaData:';
    function canPerformMultiplePassesOverSourceMediaData: ObjCBOOL; message 'canPerformMultiplePassesOverSourceMediaData';
    procedure setDirectoryForTemporaryFiles(newValue: NSURL); message 'setDirectoryForTemporaryFiles:';
    function directoryForTemporaryFiles: NSURL; message 'directoryForTemporaryFiles';
    procedure determineCompatibleFileTypesWithCompletionHandler (handler: OpaqueCBlock); message 'determineCompatibleFileTypesWithCompletionHandler:'; { available in 10_9, 6_0 }
    procedure exportAsynchronouslyWithCompletionHandler (handler: OpaqueCBlock); message 'exportAsynchronouslyWithCompletionHandler:';
    procedure cancelExport; message 'cancelExport';
  end;
{$endif}

