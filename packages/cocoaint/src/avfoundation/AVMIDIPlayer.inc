{ Parsed from AVFoundation.framework AVMIDIPlayer.h }


{$ifdef TYPES}
type
  AVMIDIPlayerPtr = ^AVMIDIPlayer;
{$endif}

{$ifdef TYPES}
type
  AVMIDIPlayerCompletionHandler = OpaqueCBlock;
{$endif}

{$ifdef CLASSES}

type
  AVMIDIPlayer = objcclass external (NSObject)
  private
    _impl: pointer;
  public
    function initWithContentsOfURL_soundBankURL_error (inURL: NSURL; bankURL: NSURL; outError: NSErrorPtr): instancetype; message 'initWithContentsOfURL:soundBankURL:error:';
    function initWithData_soundBankURL_error (data: NSData; bankURL: NSURL; outError: NSErrorPtr): instancetype; message 'initWithData:soundBankURL:error:';
    procedure prepareToPlay; message 'prepareToPlay';
    procedure play (completionHandler: AVMIDIPlayerCompletionHandler); message 'play:';
    procedure stop; message 'stop';
    function duration: NSTimeInterval; message 'duration';
    function isPlaying: ObjCBOOL; message 'isPlaying';
    procedure setRate(newValue: single); message 'setRate:';
    function rate: single; message 'rate';
    procedure setCurrentPosition(newValue: NSTimeInterval); message 'setCurrentPosition:';
    function currentPosition: NSTimeInterval; message 'currentPosition';
  end;
{$endif}

