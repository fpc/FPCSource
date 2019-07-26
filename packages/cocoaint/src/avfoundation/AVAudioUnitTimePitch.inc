{ Parsed from AVFoundation.framework AVAudioUnitTimePitch.h }


{$ifdef TYPES}
type
  AVAudioUnitTimePitchPtr = ^AVAudioUnitTimePitch;
{$endif}

{$ifdef CLASSES}

type
  AVAudioUnitTimePitch = objcclass external (AVAudioUnitTimeEffect)
  public
    procedure setRate(newValue: single); message 'setRate:';
    function rate: single; message 'rate';
    procedure setPitch(newValue: single); message 'setPitch:';
    function pitch: single; message 'pitch';
    procedure setOverlap(newValue: single); message 'setOverlap:';
    function overlap: single; message 'overlap';
  end;
{$endif}

