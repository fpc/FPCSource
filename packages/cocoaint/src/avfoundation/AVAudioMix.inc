{ Parsed from AVFoundation.framework AVAudioMix.h }


{$ifdef TYPES}
type
  AVAudioMixPtr = ^AVAudioMix;
  AVMutableAudioMixPtr = ^AVMutableAudioMix;
  AVAudioMixInputParametersPtr = ^AVAudioMixInputParameters;
  AVMutableAudioMixInputParametersPtr = ^AVMutableAudioMixInputParameters;
{$endif}

{$ifdef CLASSES}

type
  AVAudioMix = objcclass external (NSObject, NSCopyingProtocol, NSMutableCopyingProtocol)
  private
    _audioMix: AVAudioMixInternal;
  public
    function inputParameters: NSArray; message 'inputParameters';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    function mutableCopyWithZone (zone: NSZonePtr): id; message 'mutableCopyWithZone:';
  end;

type
  AVMutableAudioMix = objcclass external (AVAudioMix)
  private
    _mutableAudioMix: AVMutableAudioMixInternal;
  public
    class function audioMix: AVMutableAudioMix; message 'audioMix';
    procedure setInputParameters(newValue: NSArray); message 'setInputParameters:';
    function inputParameters: NSArray; message 'inputParameters';
  end;

type
  AVAudioMixInputParameters = objcclass external (NSObject, NSCopyingProtocol, NSMutableCopyingProtocol)
  private
    _inputParameters: AVAudioMixInputParametersInternal;
  public
    function trackID: CMPersistentTrackID; message 'trackID';
    function audioTimePitchAlgorithm: NSString; message 'audioTimePitchAlgorithm';
    function audioTapProcessor: MTAudioProcessingTapRef; message 'audioTapProcessor';
    function getVolumeRampForTime_startVolume_endVolume_timeRange (time: CMTime; startVolume: psingle; endVolume: psingle; timeRange: CMTimeRangePtr): ObjCBOOL; message 'getVolumeRampForTime:startVolume:endVolume:timeRange:';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    function mutableCopyWithZone (zone: NSZonePtr): id; message 'mutableCopyWithZone:';
  end;

type
  AVMutableAudioMixInputParameters = objcclass external (AVAudioMixInputParameters)
  private
    _mutableInputParameters: AVMutableAudioMixInputParametersInternal;
  public
    class function audioMixInputParametersWithTrack (track: AVAssetTrack): AVMutableAudioMixInputParameters; message 'audioMixInputParametersWithTrack:';
    class function audioMixInputParameters: AVMutableAudioMixInputParameters; message 'audioMixInputParameters';
    procedure setTrackID(newValue: CMPersistentTrackID); message 'setTrackID:';
    function trackID: CMPersistentTrackID; message 'trackID';
    procedure setAudioTimePitchAlgorithm(newValue: NSString); message 'setAudioTimePitchAlgorithm:';
    function audioTimePitchAlgorithm: NSString; message 'audioTimePitchAlgorithm';
    procedure setAudioTapProcessor(newValue: MTAudioProcessingTapRef); message 'setAudioTapProcessor:';
    function audioTapProcessor: MTAudioProcessingTapRef; message 'audioTapProcessor';
    procedure setVolumeRampFromStartVolume_toEndVolume_timeRange (startVolume: single; endVolume: single; timeRange: CMTimeRange); message 'setVolumeRampFromStartVolume:toEndVolume:timeRange:';
    procedure setVolume_atTime (volume: single; time: CMTime); message 'setVolume:atTime:';
  end;
{$endif}

