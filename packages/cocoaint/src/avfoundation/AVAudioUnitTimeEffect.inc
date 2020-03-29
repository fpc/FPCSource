{ Parsed from AVFoundation.framework AVAudioUnitTimeEffect.h }


{$ifdef TYPES}
type
  AVAudioUnitTimeEffectPtr = ^AVAudioUnitTimeEffect;
{$endif}

{$ifdef CLASSES}

type
  AVAudioUnitTimeEffect = objcclass external (AVAudioUnit)
  public
    function initWithAudioComponentDescription (audioComponentDescription_: AudioComponentDescription): instancetype; message 'initWithAudioComponentDescription:';
    procedure setBypass(newValue: ObjCBOOL); message 'setBypass:';
    function bypass: ObjCBOOL; message 'bypass';
  end;
{$endif}

