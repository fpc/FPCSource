{ Parsed from AVFoundation.framework AVAudioUnitDistortion.h }


{$ifdef TYPES}
type
  AVAudioUnitDistortionPtr = ^AVAudioUnitDistortion;
{$endif}

{$ifdef TYPES}
type
  AVAudioUnitDistortionPreset = NSInteger;
  AVAudioUnitDistortionPresetPtr = ^AVAudioUnitDistortionPreset;

const
  AVAudioUnitDistortionPresetDrumsBitBrush = 0;
  AVAudioUnitDistortionPresetDrumsBufferBeats = 1;
  AVAudioUnitDistortionPresetDrumsLoFi = 2;
  AVAudioUnitDistortionPresetMultiBrokenSpeaker = 3;
  AVAudioUnitDistortionPresetMultiCellphoneConcert = 4;
  AVAudioUnitDistortionPresetMultiDecimated1 = 5;
  AVAudioUnitDistortionPresetMultiDecimated2 = 6;
  AVAudioUnitDistortionPresetMultiDecimated3 = 7;
  AVAudioUnitDistortionPresetMultiDecimated4 = 8;
  AVAudioUnitDistortionPresetMultiDistortedFunk = 9;
  AVAudioUnitDistortionPresetMultiDistortedCubed = 10;
  AVAudioUnitDistortionPresetMultiDistortedSquared = 11;
  AVAudioUnitDistortionPresetMultiEcho1 = 12;
  AVAudioUnitDistortionPresetMultiEcho2 = 13;
  AVAudioUnitDistortionPresetMultiEchoTight1 = 14;
  AVAudioUnitDistortionPresetMultiEchoTight2 = 15;
  AVAudioUnitDistortionPresetMultiEverythingIsBroken = 16;
  AVAudioUnitDistortionPresetSpeechAlienChatter = 17;
  AVAudioUnitDistortionPresetSpeechCosmicInterference = 18;
  AVAudioUnitDistortionPresetSpeechGoldenPi = 19;
  AVAudioUnitDistortionPresetSpeechRadioTower = 20;
  AVAudioUnitDistortionPresetSpeechWaves = 21;
{$endif}

{$ifdef CLASSES}

type
  AVAudioUnitDistortion = objcclass external (AVAudioUnitEffect)
  public
    procedure loadFactoryPreset (preset: AVAudioUnitDistortionPreset); message 'loadFactoryPreset:';
    procedure setPreGain(newValue: single); message 'setPreGain:';
    function preGain: single; message 'preGain';
    procedure setWetDryMix(newValue: single); message 'setWetDryMix:';
    function wetDryMix: single; message 'wetDryMix';
  end;
{$endif}

