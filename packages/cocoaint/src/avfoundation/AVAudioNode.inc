{ Parsed from AVFoundation.framework AVAudioNode.h }


{$ifdef TYPES}
type
  AVAudioNodePtr = ^AVAudioNode;
{$endif}

{$ifdef TYPES}
type
  AVAudioNodeTapBlock = OpaqueCBlock;
{$endif}

{$ifdef CLASSES}

type
  AVAudioNode = objcclass external (NSObject)
  private
    _impl: pointer;
  public
    procedure reset; message 'reset';
    function inputFormatForBus (bus: AVAudioNodeBus): AVAudioFormat; message 'inputFormatForBus:';
    function outputFormatForBus (bus: AVAudioNodeBus): AVAudioFormat; message 'outputFormatForBus:';
    function nameForInputBus (bus: AVAudioNodeBus): NSString; message 'nameForInputBus:';
    function nameForOutputBus (bus: AVAudioNodeBus): NSString; message 'nameForOutputBus:';
    procedure installTapOnBus_bufferSize_format_block (bus: AVAudioNodeBus; bufferSize: AVAudioFrameCount; format: AVAudioFormat; tapBlock: AVAudioNodeTapBlock); message 'installTapOnBus:bufferSize:format:block:';
    procedure removeTapOnBus (bus: AVAudioNodeBus); message 'removeTapOnBus:';
    function engine: AVAudioEngine; message 'engine';
    function numberOfInputs: NSUInteger; message 'numberOfInputs';
    function numberOfOutputs: NSUInteger; message 'numberOfOutputs';
    function lastRenderTime: AVAudioTime; message 'lastRenderTime';
  end;
{$endif}

