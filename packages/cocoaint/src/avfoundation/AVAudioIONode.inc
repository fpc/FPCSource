{ Parsed from AVFoundation.framework AVAudioIONode.h }


{$ifdef TYPES}
type
  AVAudioIONodePtr = ^AVAudioIONode;
  AVAudioInputNodePtr = ^AVAudioInputNode;
  AVAudioOutputNodePtr = ^AVAudioOutputNode;
{$endif}

{$ifdef CLASSES}

type
  AVAudioIONode = objcclass external (AVAudioNode)
  public
    function presentationLatency: NSTimeInterval; message 'presentationLatency';
    function audioUnit: AudioUnit; message 'audioUnit';
  end;

type
  AVAudioInputNode = objcclass external (AVAudioIONode, AVAudioMixingProtocol)

    { Adopted protocols }
    procedure setObstruction(newValue: single); message 'setObstruction:';
    function obstruction: single; message 'obstruction';
    procedure setOcclusion(newValue: single); message 'setOcclusion:';
    function occlusion: single; message 'occlusion';
    procedure setPan(newValue: single); message 'setPan:';
    function pan: single; message 'pan';
    procedure setPosition(newValue: AVAudio3DPoint); message 'setPosition:';
    function position: AVAudio3DPoint; message 'position';
    procedure setRate(newValue: single); message 'setRate:';
    function rate: single; message 'rate';
    procedure setRenderingAlgorithm(newValue: AVAudio3DMixingRenderingAlgorithm); message 'setRenderingAlgorithm:';
    function renderingAlgorithm: AVAudio3DMixingRenderingAlgorithm; message 'renderingAlgorithm';
    procedure setReverbBlend(newValue: single); message 'setReverbBlend:';
    function reverbBlend: single; message 'reverbBlend';
    procedure setVolume(newValue: single); message 'setVolume:';
    function volume: single; message 'volume';
  end;

type
  AVAudioOutputNode = objcclass external (AVAudioIONode)
  end;
{$endif}

