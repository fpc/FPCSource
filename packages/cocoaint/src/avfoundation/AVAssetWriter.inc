{ Parsed from AVFoundation.framework AVAssetWriter.h }


{$ifdef TYPES}
type
  AVAssetWriterPtr = ^AVAssetWriter;
  AVAssetWriterInputGroupPtr = ^AVAssetWriterInputGroup;
{$endif}

{$ifdef TYPES}
type
  AVAssetWriterStatus = NSInteger;
  AVAssetWriterStatusPtr = ^AVAssetWriterStatus;

const
  AVAssetWriterStatusUnknown = 0;
  AVAssetWriterStatusWriting = 1;
  AVAssetWriterStatusCompleted = 2;
  AVAssetWriterStatusFailed = 3;
  AVAssetWriterStatusCancelled = 4;
{$endif}

{$ifdef CLASSES}

type
  AVAssetWriter = objcclass external (NSObject)
  private
    _internal: AVAssetWriterInternal;
  public
    class function assetWriterWithURL_fileType_error (outputURL: NSURL; outputFileType: NSString; outError: NSErrorPtr): instancetype; message 'assetWriterWithURL:fileType:error:';
    function initWithURL_fileType_error (outputURL: NSURL; outputFileType: NSString; outError: NSErrorPtr): instancetype; message 'initWithURL:fileType:error:';
    function outputURL: NSURL; message 'outputURL';
    function outputFileType: NSString; message 'outputFileType';
    function availableMediaTypes: NSArray; message 'availableMediaTypes';
    function status: AVAssetWriterStatus; message 'status';
    function error: NSError; message 'error';
    procedure setMetadata(newValue: NSArray); message 'setMetadata:';
    function metadata: NSArray; message 'metadata';
    procedure setShouldOptimizeForNetworkUse(newValue: ObjCBOOL); message 'setShouldOptimizeForNetworkUse:';
    function shouldOptimizeForNetworkUse: ObjCBOOL; message 'shouldOptimizeForNetworkUse';
    procedure setDirectoryForTemporaryFiles(newValue: NSURL); message 'setDirectoryForTemporaryFiles:';
    function directoryForTemporaryFiles: NSURL; message 'directoryForTemporaryFiles';
    function inputs: NSArray; message 'inputs';
    function canApplyOutputSettings_forMediaType (outputSettings: NSDictionary; mediaType: NSString): ObjCBOOL; message 'canApplyOutputSettings:forMediaType:';
    function canAddInput (input: AVAssetWriterInput): ObjCBOOL; message 'canAddInput:';
    procedure addInput (input: AVAssetWriterInput); message 'addInput:';
    function startWriting: ObjCBOOL; message 'startWriting';
    procedure startSessionAtSourceTime (startTime: CMTime); message 'startSessionAtSourceTime:';
    procedure endSessionAtSourceTime (endTime: CMTime); message 'endSessionAtSourceTime:';
    procedure cancelWriting; message 'cancelWriting';
    function finishWriting: ObjCBOOL; message 'finishWriting'; deprecated 'in 10_7, 10_9, 4_1, 6_0';
    procedure finishWritingWithCompletionHandler (handler: OpaqueCBlock); message 'finishWritingWithCompletionHandler:'; { available in 10_9, 6_0 }
  end;


type
  AVAssetWriterFileTypeSpecificProperties = objccategory external (AVAssetWriter)
    procedure setMovieFragmentInterval(newValue: CMTime); message 'setMovieFragmentInterval:';
    function movieFragmentInterval: CMTime; message 'movieFragmentInterval';
    procedure setMovieTimeScale(newValue: CMTimeScale); message 'setMovieTimeScale:';
    function movieTimeScale: CMTimeScale; message 'movieTimeScale';
  end;

type
  AVAssetWriterInputGroups = objccategory external (AVAssetWriter)
    function canAddInputGroup (inputGroup: AVAssetWriterInputGroup): ObjCBOOL; message 'canAddInputGroup:'; { available in 10_9, 7_0 }
    procedure addInputGroup (inputGroup: AVAssetWriterInputGroup); message 'addInputGroup:'; { available in 10_9, 7_0 }
    function inputGroups: NSArray; message 'inputGroups';
  end;


type
  AVAssetWriterInputGroup = objcclass external (AVMediaSelectionGroup)
  private
    _internal: AVAssetWriterInputGroupInternal;
  public
    class function assetWriterInputGroupWithInputs_defaultInput (inputs: NSArray; defaultInput: AVAssetWriterInput): AVAssetWriterInputGroup; message 'assetWriterInputGroupWithInputs:defaultInput:';
    function initWithInputs_defaultInput (inputs: NSArray; defaultInput: AVAssetWriterInput): instancetype; message 'initWithInputs:defaultInput:';
    function inputs: NSArray; message 'inputs';
    function defaultInput: AVAssetWriterInput; message 'defaultInput';
  end;
{$endif}

