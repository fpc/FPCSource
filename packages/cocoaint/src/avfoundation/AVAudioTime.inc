{ Parsed from AVFoundation.framework AVAudioTime.h }


{$ifdef TYPES}
type
  AVAudioTimePtr = ^AVAudioTime;
{$endif}

{$ifdef CLASSES}

type
  AVAudioTime = objcclass external (NSObject)
  private
    _ats: AudioTimeStamp;
    _sampleRate: double;
    _reserved: pointer;
  public
    function initWithAudioTimeStamp_sampleRate (ts: AudioTimeStampPtr; sampleRate: double): instancetype; message 'initWithAudioTimeStamp:sampleRate:';
    function initWithHostTime (hostTime: cuint64): instancetype; message 'initWithHostTime:';
    function initWithSampleTime_atRate (sampleTime: AVAudioFramePosition; sampleRate: double): instancetype; message 'initWithSampleTime:atRate:';
    function initWithHostTime_sampleTime_atRate (hostTime: cuint64; sampleTime: AVAudioFramePosition; sampleRate: double): instancetype; message 'initWithHostTime:sampleTime:atRate:';
    class function timeWithAudioTimeStamp_sampleRate (ts: AudioTimeStampPtr; sampleRate: double): instancetype; message 'timeWithAudioTimeStamp:sampleRate:';
    class function timeWithHostTime (hostTime: cuint64): instancetype; message 'timeWithHostTime:';
    class function timeWithSampleTime_atRate (sampleTime: AVAudioFramePosition; sampleRate: double): instancetype; message 'timeWithSampleTime:atRate:';
    class function timeWithHostTime_sampleTime_atRate (hostTime: cuint64; sampleTime: AVAudioFramePosition; sampleRate: double): instancetype; message 'timeWithHostTime:sampleTime:atRate:';
    class function hostTimeForSeconds (seconds: NSTimeInterval): cuint64; message 'hostTimeForSeconds:';
    class function secondsForHostTime (hostTime: cuint64): NSTimeInterval; message 'secondsForHostTime:';
    function extrapolateTimeFromAnchor (anchorTime: AVAudioTime): AVAudioTime; message 'extrapolateTimeFromAnchor:';
    function isHostTimeValid: ObjCBOOL; message 'isHostTimeValid';
    function hostTime: cuint64; message 'hostTime';
    function isSampleTimeValid: ObjCBOOL; message 'isSampleTimeValid';
    function sampleTime: AVAudioFramePosition; message 'sampleTime';
    function sampleRate: double; message 'sampleRate';
    function audioTimeStamp: AudioTimeStamp; message 'audioTimeStamp';
  end;
{$endif}

