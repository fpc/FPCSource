{ Parsed from AVFoundation.framework AVAudioFormat.h }


{$ifdef TYPES}
type
  AVAudioFormatPtr = ^AVAudioFormat;
{$endif}

{$ifdef TYPES}
type
  AVAudioCommonFormat = NSUInteger;
  AVAudioCommonFormatPtr = ^AVAudioCommonFormat;

const
  AVAudioOtherFormat = 0;
  AVAudioPCMFormatFloat32 = 1;
  AVAudioPCMFormatFloat64 = 2;
  AVAudioPCMFormatInt16 = 3;
  AVAudioPCMFormatInt32 = 4;
{$endif}

{$ifdef CLASSES}

type
  AVAudioFormat = objcclass external (NSObject)
  private
    _asbd: AudioStreamBasicDescription;
    _layout: AVAudioChannelLayout;
    _commonFormat: AVAudioCommonFormat;
    _reserved: pointer;
  public
    function initWithStreamDescription (asbd: AudioStreamBasicDescriptionPtr): instancetype; message 'initWithStreamDescription:';
    function initWithStreamDescription_channelLayout (asbd: AudioStreamBasicDescriptionPtr; layout: AVAudioChannelLayout): instancetype; message 'initWithStreamDescription:channelLayout:';
    function initStandardFormatWithSampleRate_channels (sampleRate: double; channels: AVAudioChannelCount): instancetype; message 'initStandardFormatWithSampleRate:channels:';
    function initStandardFormatWithSampleRate_channelLayout (sampleRate: double; layout: AVAudioChannelLayout): instancetype; message 'initStandardFormatWithSampleRate:channelLayout:';
    function initWithCommonFormat_sampleRate_channels_interleaved (format: AVAudioCommonFormat; sampleRate: double; channels: AVAudioChannelCount; interleaved: ObjCBOOL): instancetype; message 'initWithCommonFormat:sampleRate:channels:interleaved:';
    function initWithCommonFormat_sampleRate_interleaved_channelLayout (format: AVAudioCommonFormat; sampleRate: double; interleaved: ObjCBOOL; layout: AVAudioChannelLayout): instancetype; message 'initWithCommonFormat:sampleRate:interleaved:channelLayout:';
    function initWithSettings (settings: NSDictionary): instancetype; message 'initWithSettings:';
    function isEqual (object_: id): ObjCBOOL; message 'isEqual:';
    function isStandard: ObjCBOOL; message 'isStandard';
    function commonFormat: AVAudioCommonFormat; message 'commonFormat';
    function channelCount: AVAudioChannelCount; message 'channelCount';
    function sampleRate: double; message 'sampleRate';
    function isInterleaved: ObjCBOOL; message 'isInterleaved';
    function streamDescription: AudioStreamBasicDescriptionPtr; message 'streamDescription';
    function channelLayout: AVAudioChannelLayout; message 'channelLayout';
    function settings: NSDictionary; message 'settings';
  end;
{$endif}

