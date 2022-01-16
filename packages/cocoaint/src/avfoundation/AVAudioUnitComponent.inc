{ Parsed from AVFoundation.framework AVAudioUnitComponent.h }


{$ifdef TYPES}
type
  AVAudioUnitComponentPtr = ^AVAudioUnitComponent;
  AVAudioUnitComponentManagerPtr = ^AVAudioUnitComponentManager;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  AVAudioUnitTypeOutput: NSString { available in 10_10, NA }; cvar; external;
  AVAudioUnitTypeMusicDevice: NSString { available in 10_10, NA }; cvar; external;
  AVAudioUnitTypeMusicEffect: NSString { available in 10_10, NA }; cvar; external;
  AVAudioUnitTypeFormatConverter: NSString { available in 10_10, NA }; cvar; external;
  AVAudioUnitTypeEffect: NSString { available in 10_10, NA }; cvar; external;
  AVAudioUnitTypeMixer: NSString { available in 10_10, NA }; cvar; external;
  AVAudioUnitTypePanner: NSString { available in 10_10, NA }; cvar; external;
  AVAudioUnitTypeGenerator: NSString { available in 10_10, NA }; cvar; external;
  AVAudioUnitTypeOfflineEffect: NSString { available in 10_10, NA }; cvar; external;
  AVAudioUnitTypeMIDIProcessor: NSString { available in 10_10, NA }; cvar; external;
  AVAudioUnitManufacturerNameApple: NSString { available in 10_10, NA }; cvar; external;
{$endif}

{$ifdef CLASSES}

type
  AVAudioUnitComponent = objcclass external (NSObject)
  private
    impl_: pointer;
  public
    function name: NSString; message 'name';
    function typeName: NSString; message 'typeName';
    function localizedTypeName: NSString; message 'localizedTypeName';
    function manufacturerName: NSString; message 'manufacturerName';
    function version: NSUInteger; message 'version';
    function versionString: NSString; message 'versionString';
    function iconURL: NSURL; message 'iconURL';
    function componentURL: NSURL; message 'componentURL';
    function availableArchitectures: NSArray; message 'availableArchitectures';
    function hasCustomView: ObjCBOOL; message 'hasCustomView';
    function passesAUVal: ObjCBOOL; message 'passesAUVal';
    function isSandboxSafe: ObjCBOOL; message 'isSandboxSafe';
    function hasMIDIInput: ObjCBOOL; message 'hasMIDIInput';
    function hasMIDIOutput: ObjCBOOL; message 'hasMIDIOutput';
    function audioComponent: AudioComponent; message 'audioComponent';
    function configurationDictionary: NSDictionary; message 'configurationDictionary';
    procedure setUserTagNames(newValue: NSArray); message 'setUserTagNames:';
    function userTagNames: NSArray; message 'userTagNames';
    function allTagNames: NSArray; message 'allTagNames';
    function audioComponentDescription: AudioComponentDescription; message 'audioComponentDescription';
    function supportsNumberInputChannels_outputChannels (numInputChannels: NSInteger; numOutputChannels: NSInteger): ObjCBOOL; message 'supportsNumberInputChannels:outputChannels:';
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  AVAudioUnitComponentTagsDidChangeNotification: NSString { available in 10_10, NA }; cvar; external;
{$endif}

{$ifdef CLASSES}

type
  AVAudioUnitComponentManager = objcclass external (NSObject)
  private
    impl_: pointer;
  public
    function tagNames: NSArray; message 'tagNames';
    function standardLocalizedTagNames: NSArray; message 'standardLocalizedTagNames';
    class function sharedAudioUnitComponentManager: instancetype; message 'sharedAudioUnitComponentManager';
    function componentsMatchingPredicate (predicate: NSPredicate): NSArray; message 'componentsMatchingPredicate:';
    function componentsPassingTest (testHandler: OpaqueCBlock): NSArray; message 'componentsPassingTest:';
    function componentsMatchingDescription (desc: AudioComponentDescription): NSArray; message 'componentsMatchingDescription:';
  end;
{$endif}

