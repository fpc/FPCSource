{ Parsed from AVFoundation.framework AVAudioBuffer.h }


{$ifdef TYPES}
type
  AVAudioBufferPtr = ^AVAudioBuffer;
  AVAudioPCMBufferPtr = ^AVAudioPCMBuffer;
{$endif}

{$ifdef CLASSES}

type
  AVAudioBuffer = objcclass external (NSObject, NSCopyingProtocol, NSMutableCopyingProtocol)
  private
    _impl: pointer;
  public
    function format: AVAudioFormat; message 'format';
    function audioBufferList: AudioBufferListPtr; message 'audioBufferList';
    function mutableAudioBufferList: AudioBufferListPtr; message 'mutableAudioBufferList';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    function mutableCopyWithZone (zone: NSZonePtr): id; message 'mutableCopyWithZone:';
  end;

type
  AVAudioPCMBuffer = objcclass external (AVAudioBuffer)
  public
    function initWithPCMFormat_frameCapacity (format_: AVAudioFormat; frameCapacity: AVAudioFrameCount): instancetype; message 'initWithPCMFormat:frameCapacity:';
    function frameCapacity: AVAudioFrameCount; message 'frameCapacity';
    procedure setFrameLength(newValue: AVAudioFrameCount); message 'setFrameLength:';
    function frameLength: AVAudioFrameCount; message 'frameLength';
    function stride: NSUInteger; message 'stride';
    function floatChannelData: psingle; message 'floatChannelData';
    function int16ChannelData: pcint16; message 'int16ChannelData';
    function int32ChannelData: pcint32; message 'int32ChannelData';
  end;
{$endif}

