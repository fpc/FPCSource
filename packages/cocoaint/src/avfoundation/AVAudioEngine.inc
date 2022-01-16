{ Parsed from AVFoundation.framework AVAudioEngine.h }


{$ifdef TYPES}
type
  AVAudioEnginePtr = ^AVAudioEngine;
{$endif}

{$ifdef CLASSES}

type
  AVAudioEngine = objcclass external (NSObject)
  private
    _impl: pointer;
  public
    function init: instancetype; message 'init';
    procedure attachNode (node: AVAudioNode); message 'attachNode:';
    procedure detachNode (node: AVAudioNode); message 'detachNode:';
    procedure connect_to_fromBus_toBus_format (node1: AVAudioNode; node2: AVAudioNode; bus1: AVAudioNodeBus; bus2: AVAudioNodeBus; format: AVAudioFormat); message 'connect:to:fromBus:toBus:format:';
    procedure connect_to_format (node1: AVAudioNode; node2: AVAudioNode; format: AVAudioFormat); message 'connect:to:format:';
    procedure disconnectNodeInput_bus (node: AVAudioNode; bus: AVAudioNodeBus); message 'disconnectNodeInput:bus:';
    procedure disconnectNodeInput (node: AVAudioNode); message 'disconnectNodeInput:';
    procedure disconnectNodeOutput_bus (node: AVAudioNode; bus: AVAudioNodeBus); message 'disconnectNodeOutput:bus:';
    procedure disconnectNodeOutput (node: AVAudioNode); message 'disconnectNodeOutput:';
    procedure prepare; message 'prepare';
    function startAndReturnError (outError: NSErrorPtr): ObjCBOOL; message 'startAndReturnError:';
    procedure pause; message 'pause';
    procedure reset; message 'reset';
    procedure stop; message 'stop';
    procedure setMusicSequence(newValue: MusicSequence); message 'setMusicSequence:';
    function musicSequence: MusicSequence; message 'musicSequence';
    function outputNode: AVAudioOutputNode; message 'outputNode';
    function inputNode: AVAudioInputNode; message 'inputNode';
    function mainMixerNode: AVAudioMixerNode; message 'mainMixerNode';
    function isRunning: ObjCBOOL; message 'isRunning';
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  AVAudioEngineConfigurationChangeNotification: NSString { available in 10_10, 8_0 }; cvar; external;
{$endif}

