{ Parsed from AVFoundation.framework AVAudioUnit.h }


{$ifdef TYPES}
type
  AVAudioUnitPtr = ^AVAudioUnit;
{$endif}

{$ifdef CLASSES}

type
  AVAudioUnit = objcclass external (AVAudioNode)
  public
    function loadAudioUnitPresetAtURL_error (url: NSURL; error: NSErrorPtr): ObjCBOOL; message 'loadAudioUnitPresetAtURL:error:';
    function audioComponentDescription: AudioComponentDescription; message 'audioComponentDescription';
    function audioUnit: AudioUnit; message 'audioUnit';
    function name: NSString; message 'name';
    function manufacturerName: NSString; message 'manufacturerName';
    function version: NSUInteger; message 'version';
  end;
{$endif}

