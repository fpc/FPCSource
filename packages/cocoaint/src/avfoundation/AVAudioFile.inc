{ Parsed from AVFoundation.framework AVAudioFile.h }


{$ifdef TYPES}
type
  AVAudioFilePtr = ^AVAudioFile;
{$endif}

{$ifdef CLASSES}

type
  AVAudioFile = objcclass external (NSObject)
  private
    _impl: pointer;
  public
    function initForReading_error (fileURL: NSURL; outError: NSErrorPtr): instancetype; message 'initForReading:error:';
    function initForReading_commonFormat_interleaved_error (fileURL: NSURL; format: AVAudioCommonFormat; interleaved: ObjCBOOL; outError: NSErrorPtr): instancetype; message 'initForReading:commonFormat:interleaved:error:';
    function initForWriting_settings_error (fileURL: NSURL; settings: NSDictionary; outError: NSErrorPtr): instancetype; message 'initForWriting:settings:error:';
    function initForWriting_settings_commonFormat_interleaved_error (fileURL: NSURL; settings: NSDictionary; format: AVAudioCommonFormat; interleaved: ObjCBOOL; outError: NSErrorPtr): instancetype; message 'initForWriting:settings:commonFormat:interleaved:error:';
    function readIntoBuffer_error (buffer: AVAudioPCMBuffer; outError: NSErrorPtr): ObjCBOOL; message 'readIntoBuffer:error:';
    function readIntoBuffer_frameCount_error (buffer: AVAudioPCMBuffer; frames: AVAudioFrameCount; outError: NSErrorPtr): ObjCBOOL; message 'readIntoBuffer:frameCount:error:';
    function writeFromBuffer_error (buffer: AVAudioPCMBuffer; outError: NSErrorPtr): ObjCBOOL; message 'writeFromBuffer:error:';
    function url: NSURL; message 'url';
    function fileFormat: AVAudioFormat; message 'fileFormat';
    function processingFormat: AVAudioFormat; message 'processingFormat';
    function length: AVAudioFramePosition; message 'length';
    procedure setFramePosition(newValue: AVAudioFramePosition); message 'setFramePosition:';
    function framePosition: AVAudioFramePosition; message 'framePosition';
  end;
{$endif}

