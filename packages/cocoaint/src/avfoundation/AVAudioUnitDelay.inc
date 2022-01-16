{ Parsed from AVFoundation.framework AVAudioUnitDelay.h }


{$ifdef TYPES}
type
  AVAudioUnitDelayPtr = ^AVAudioUnitDelay;
{$endif}

{$ifdef CLASSES}

type
  AVAudioUnitDelay = objcclass external (AVAudioUnitEffect)
  public
    procedure setDelayTime(newValue: NSTimeInterval); message 'setDelayTime:';
    function delayTime: NSTimeInterval; message 'delayTime';
    procedure setFeedback(newValue: single); message 'setFeedback:';
    function feedback: single; message 'feedback';
    procedure setLowPassCutoff(newValue: single); message 'setLowPassCutoff:';
    function lowPassCutoff: single; message 'lowPassCutoff';
    procedure setWetDryMix(newValue: single); message 'setWetDryMix:';
    function wetDryMix: single; message 'wetDryMix';
  end;
{$endif}

