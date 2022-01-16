{ Parsed from AVFoundation.framework AVAudioEnvironmentNode.h }


{$ifdef TYPES}
type
  AVAudioEnvironmentDistanceAttenuationParametersPtr = ^AVAudioEnvironmentDistanceAttenuationParameters;
  AVAudioEnvironmentReverbParametersPtr = ^AVAudioEnvironmentReverbParameters;
  AVAudioEnvironmentNodePtr = ^AVAudioEnvironmentNode;
{$endif}

{$ifdef TYPES}
type
  AVAudioEnvironmentDistanceAttenuationModel = NSInteger;
  AVAudioEnvironmentDistanceAttenuationModelPtr = ^AVAudioEnvironmentDistanceAttenuationModel;

const
  AVAudioEnvironmentDistanceAttenuationModelExponential = 1;
  AVAudioEnvironmentDistanceAttenuationModelInverse = 2;
  AVAudioEnvironmentDistanceAttenuationModelLinear = 3;
{$endif}

{$ifdef CLASSES}

type
  AVAudioEnvironmentDistanceAttenuationParameters = objcclass external (NSObject)
  private
    _impl: pointer;
  public
    procedure setDistanceAttenuationModel(newValue: AVAudioEnvironmentDistanceAttenuationModel); message 'setDistanceAttenuationModel:';
    function distanceAttenuationModel: AVAudioEnvironmentDistanceAttenuationModel; message 'distanceAttenuationModel';
    procedure setReferenceDistance(newValue: single); message 'setReferenceDistance:';
    function referenceDistance: single; message 'referenceDistance';
    procedure setMaximumDistance(newValue: single); message 'setMaximumDistance:';
    function maximumDistance: single; message 'maximumDistance';
    procedure setRolloffFactor(newValue: single); message 'setRolloffFactor:';
    function rolloffFactor: single; message 'rolloffFactor';
  end;

type
  AVAudioEnvironmentReverbParameters = objcclass external (NSObject)
  private
    _impl: pointer;
  public
    procedure setEnable(newValue: ObjCBOOL); message 'setEnable:';
    function enable: ObjCBOOL; message 'enable';
    procedure setLevel(newValue: single); message 'setLevel:';
    function level: single; message 'level';
    function filterParameters: AVAudioUnitEQFilterParameters; message 'filterParameters';
    procedure loadFactoryReverbPreset (preset: AVAudioUnitReverbPreset); message 'loadFactoryReverbPreset:';
  end;

type
  AVAudioEnvironmentNode = objcclass external (AVAudioNode, AVAudioMixingProtocol)
  public
    procedure setOutputVolume(newValue: single); message 'setOutputVolume:';
    function outputVolume: single; message 'outputVolume';
    function nextAvailableInputBus: AVAudioNodeBus; message 'nextAvailableInputBus';
    procedure setListenerPosition(newValue: AVAudio3DPoint); message 'setListenerPosition:';
    function listenerPosition: AVAudio3DPoint; message 'listenerPosition';
    procedure setListenerVectorOrientation(newValue: AVAudio3DVectorOrientation); message 'setListenerVectorOrientation:';
    function listenerVectorOrientation: AVAudio3DVectorOrientation; message 'listenerVectorOrientation';
    procedure setListenerAngularOrientation(newValue: AVAudio3DAngularOrientation); message 'setListenerAngularOrientation:';
    function listenerAngularOrientation: AVAudio3DAngularOrientation; message 'listenerAngularOrientation';
    function distanceAttenuationParameters: AVAudioEnvironmentDistanceAttenuationParameters; message 'distanceAttenuationParameters';
    function reverbParameters: AVAudioEnvironmentReverbParameters; message 'reverbParameters';
    function applicableRenderingAlgorithms: NSArray; message 'applicableRenderingAlgorithms';

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
{$endif}

