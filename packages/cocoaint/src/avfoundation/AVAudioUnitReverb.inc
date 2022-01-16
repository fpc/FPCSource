{ Parsed from AVFoundation.framework AVAudioUnitReverb.h }


{$ifdef TYPES}
type
  AVAudioUnitReverbPtr = ^AVAudioUnitReverb;
{$endif}

{$ifdef TYPES}
type
  AVAudioUnitReverbPreset = NSInteger;
  AVAudioUnitReverbPresetPtr = ^AVAudioUnitReverbPreset;

const
  AVAudioUnitReverbPresetSmallRoom = 0;
  AVAudioUnitReverbPresetMediumRoom = 1;
  AVAudioUnitReverbPresetLargeRoom = 2;
  AVAudioUnitReverbPresetMediumHall = 3;
  AVAudioUnitReverbPresetLargeHall = 4;
  AVAudioUnitReverbPresetPlate = 5;
  AVAudioUnitReverbPresetMediumChamber = 6;
  AVAudioUnitReverbPresetLargeChamber = 7;
  AVAudioUnitReverbPresetCathedral = 8;
  AVAudioUnitReverbPresetLargeRoom2 = 9;
  AVAudioUnitReverbPresetMediumHall2 = 10;
  AVAudioUnitReverbPresetMediumHall3 = 11;
  AVAudioUnitReverbPresetLargeHall2 = 12;
{$endif}

{$ifdef CLASSES}

type
  AVAudioUnitReverb = objcclass external (AVAudioUnitEffect)
  public
    procedure loadFactoryPreset (preset: AVAudioUnitReverbPreset); message 'loadFactoryPreset:';
    procedure setWetDryMix(newValue: single); message 'setWetDryMix:';
    function wetDryMix: single; message 'wetDryMix';
  end;
{$endif}

