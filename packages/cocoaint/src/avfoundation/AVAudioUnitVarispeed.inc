{ Parsed from AVFoundation.framework AVAudioUnitVarispeed.h }


{$ifdef TYPES}
type
  AVAudioUnitVarispeedPtr = ^AVAudioUnitVarispeed;
{$endif}

{$ifdef CLASSES}

type
  AVAudioUnitVarispeed = objcclass external (AVAudioUnitTimeEffect)
  public
    procedure setRate(newValue: single); message 'setRate:';
    function rate: single; message 'rate';
  end;
{$endif}

