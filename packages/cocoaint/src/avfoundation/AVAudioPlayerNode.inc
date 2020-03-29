{ Parsed from AVFoundation.framework AVAudioPlayerNode.h }


{$ifdef TYPES}
type
  AVAudioPlayerNodePtr = ^AVAudioPlayerNode;
{$endif}

{$ifdef TYPES}
type
  AVAudioPlayerNodeBufferOptions = NSUInteger;
  AVAudioPlayerNodeBufferOptionsPtr = ^AVAudioPlayerNodeBufferOptions;

const
  AVAudioPlayerNodeBufferLoops = 1 shl 0;
  AVAudioPlayerNodeBufferInterrupts = 1 shl 1;
  AVAudioPlayerNodeBufferInterruptsAtLoop = 1 shl 2;
{$endif}

{$ifdef CLASSES}

type
  AVAudioPlayerNode = objcclass external (AVAudioNode, AVAudioMixingProtocol)
  public
    procedure scheduleBuffer_completionHandler (buffer: AVAudioPCMBuffer; completionHandler: AVAudioNodeCompletionHandler); message 'scheduleBuffer:completionHandler:';
    procedure scheduleBuffer_atTime_options_completionHandler (buffer: AVAudioPCMBuffer; when: AVAudioTime; options: AVAudioPlayerNodeBufferOptions; completionHandler: AVAudioNodeCompletionHandler); message 'scheduleBuffer:atTime:options:completionHandler:';
    procedure scheduleFile_atTime_completionHandler (file_: AVAudioFile; when: AVAudioTime; completionHandler: AVAudioNodeCompletionHandler); message 'scheduleFile:atTime:completionHandler:';
    procedure scheduleSegment_startingFrame_frameCount_atTime_completionHandler (file_: AVAudioFile; startFrame: AVAudioFramePosition; numberFrames: AVAudioFrameCount; when: AVAudioTime; completionHandler: AVAudioNodeCompletionHandler); message 'scheduleSegment:startingFrame:frameCount:atTime:completionHandler:';
    procedure stop; message 'stop';
    procedure prepareWithFrameCount (frameCount: AVAudioFrameCount); message 'prepareWithFrameCount:';
    procedure play; message 'play';
    procedure playAtTime (when: AVAudioTime); message 'playAtTime:';
    procedure pause; message 'pause';
    function nodeTimeForPlayerTime (playerTime: AVAudioTime): AVAudioTime; message 'nodeTimeForPlayerTime:';
    function playerTimeForNodeTime (nodeTime: AVAudioTime): AVAudioTime; message 'playerTimeForNodeTime:';
    function isPlaying: ObjCBOOL; message 'isPlaying';

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

