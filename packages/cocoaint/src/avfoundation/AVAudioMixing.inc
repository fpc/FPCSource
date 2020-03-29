{ Parsed from AVFoundation.framework AVAudioMixing.h }


{$ifdef TYPES}
type
  AVAudioMixingProtocolPtr = ^AVAudioMixingProtocol;
  AVAudioStereoMixingProtocolPtr = ^AVAudioStereoMixingProtocol;
  AVAudio3DMixingProtocolPtr = ^AVAudio3DMixingProtocol;
{$endif}

{$ifdef TYPES}
type
  AVAudio3DMixingRenderingAlgorithm = NSInteger;
  AVAudio3DMixingRenderingAlgorithmPtr = ^AVAudio3DMixingRenderingAlgorithm;

const
  AVAudio3DMixingRenderingAlgorithmEqualPowerPanning = 0;
  AVAudio3DMixingRenderingAlgorithmSphericalHead = 1;
  AVAudio3DMixingRenderingAlgorithmHRTF = 2;
  AVAudio3DMixingRenderingAlgorithmSoundField = 3;
  AVAudio3DMixingRenderingAlgorithmStereoPassThrough = 5;
{$endif}

{$ifdef PROTOCOLS}
type
  AVAudioStereoMixingProtocol = objcprotocol external name 'AVAudioStereoMixing' (NSObjectProtocol)
    procedure setPan(newValue: single); message 'setPan:';
    function pan: single; message 'pan';
  end;

type
  AVAudio3DMixingProtocol = objcprotocol external name 'AVAudio3DMixing' (NSObjectProtocol)
    procedure setRenderingAlgorithm(newValue: AVAudio3DMixingRenderingAlgorithm); message 'setRenderingAlgorithm:';
    function renderingAlgorithm: AVAudio3DMixingRenderingAlgorithm; message 'renderingAlgorithm';
    procedure setRate(newValue: single); message 'setRate:';
    function rate: single; message 'rate';
    procedure setReverbBlend(newValue: single); message 'setReverbBlend:';
    function reverbBlend: single; message 'reverbBlend';
    procedure setObstruction(newValue: single); message 'setObstruction:';
    function obstruction: single; message 'obstruction';
    procedure setOcclusion(newValue: single); message 'setOcclusion:';
    function occlusion: single; message 'occlusion';
    procedure setPosition(newValue: AVAudio3DPoint); message 'setPosition:';
    function position: AVAudio3DPoint; message 'position';
  end;

type
  AVAudioMixingProtocol = objcprotocol external name 'AVAudioMixing' (AVAudioStereoMixingProtocol, AVAudio3DMixingProtocol)
    procedure setVolume(newValue: single); message 'setVolume:';
    function volume: single; message 'volume';
  end;

{$endif}

