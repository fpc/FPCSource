{ Parsed from AVFoundation.framework AVOutputSettingsAssistant.h }


{$ifdef TYPES}
type
  AVOutputSettingsAssistantPtr = ^AVOutputSettingsAssistant;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  AVOutputSettingsPreset640x480: NSString { available in 10_9, 7_0 }; cvar; external;
  AVOutputSettingsPreset960x540: NSString { available in 10_9, 7_0 }; cvar; external;
  AVOutputSettingsPreset1280x720: NSString { available in 10_9, 7_0 }; cvar; external;
  AVOutputSettingsPreset1920x1080: NSString { available in 10_9, 7_0 }; cvar; external;
  AVOutputSettingsPreset3840x2160: NSString { available in 10_10, NA }; cvar; external;
{$endif}

{$ifdef CLASSES}

type
  AVOutputSettingsAssistant = objcclass external (NSObject)
  private
    _internal: AVOutputSettingsAssistantInternal;
  public
    class function availableOutputSettingsPresets: NSArray; message 'availableOutputSettingsPresets'; { available in 10_10, 7_0 }
    class function outputSettingsAssistantWithPreset (presetIdentifier: NSString): instancetype; message 'outputSettingsAssistantWithPreset:';
    function audioSettings: NSDictionary; message 'audioSettings';
    function videoSettings: NSDictionary; message 'videoSettings';
    function outputFileType: NSString; message 'outputFileType';
  end;


type
  AVOutputSettingsAssistant_SourceInformation = objccategory external (AVOutputSettingsAssistant)
    procedure setSourceAudioFormat(newValue: CMAudioFormatDescriptionRef); message 'setSourceAudioFormat:';
    function sourceAudioFormat: CMAudioFormatDescriptionRef; message 'sourceAudioFormat';
    procedure setSourceVideoFormat(newValue: CMVideoFormatDescriptionRef); message 'setSourceVideoFormat:';
    function sourceVideoFormat: CMVideoFormatDescriptionRef; message 'sourceVideoFormat';
    procedure setSourceVideoAverageFrameDuration(newValue: CMTime); message 'setSourceVideoAverageFrameDuration:';
    function sourceVideoAverageFrameDuration: CMTime; message 'sourceVideoAverageFrameDuration';
    procedure setSourceVideoMinFrameDuration(newValue: CMTime); message 'setSourceVideoMinFrameDuration:';
    function sourceVideoMinFrameDuration: CMTime; message 'sourceVideoMinFrameDuration';
  end;
{$endif}

