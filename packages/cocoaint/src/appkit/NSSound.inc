{ Parsed from AppKit.framework NSSound.h }


{$ifdef TYPES}
type
  NSSoundPtr = ^NSSound;
  NSSoundDelegateProtocolPtr = ^NSSoundDelegateProtocol;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSSoundPboardType: NSString; cvar; external;
{$endif}

{$ifdef CLASSES}

type
  NSSound = objcclass external (NSObject, NSCopyingProtocol, NSCodingProtocol, NSPasteboardReadingProtocol, NSPasteboardWritingProtocol)
  private
    _delegate: id;
    _info: id;
    _reserved:array[0..5] of id;
    _sFlags: cuint32;
  public
    class function soundNamed (name: NSString): NSSound; message 'soundNamed:';
    function initWithContentsOfURL_byReference (url: NSURL; byRef: ObjCBOOL): instancetype; message 'initWithContentsOfURL:byReference:';
    function initWithContentsOfFile_byReference (path: NSString; byRef: ObjCBOOL): instancetype; message 'initWithContentsOfFile:byReference:';
    function initWithData (data: NSData): instancetype; message 'initWithData:';
    function setName (string_: NSString): ObjCBOOL; message 'setName:';
    function name: NSString; message 'name';
    class function canInitWithPasteboard (pasteboard: NSPasteboard): ObjCBOOL; message 'canInitWithPasteboard:';
    class function soundUnfilteredTypes: NSArray; message 'soundUnfilteredTypes'; { available in 10_5 }
    function initWithPasteboard (pasteboard: NSPasteboard): instancetype; message 'initWithPasteboard:';
    procedure writeToPasteboard (pasteboard: NSPasteboard); message 'writeToPasteboard:';
    function play: ObjCBOOL; message 'play';
    function pause: ObjCBOOL; message 'pause';
    function resume: ObjCBOOL; message 'resume';
    function stop: ObjCBOOL; message 'stop';
    function isPlaying: ObjCBOOL; message 'isPlaying';
    procedure setDelegate(newValue: NSSoundDelegateProtocol); message 'setDelegate:';
    function delegate: NSSoundDelegateProtocol; message 'delegate';
    function duration: NSTimeInterval; message 'duration';
    procedure setVolume(newValue: single); message 'setVolume:';
    function volume: single; message 'volume';
    procedure setCurrentTime(newValue: NSTimeInterval); message 'setCurrentTime:';
    function currentTime: NSTimeInterval; message 'currentTime';
    procedure setLoops(newValue: ObjCBOOL); message 'setLoops:';
    function loops: ObjCBOOL; message 'loops';
    procedure setPlaybackDeviceIdentifier(newValue: NSString); message 'setPlaybackDeviceIdentifier:';
    function playbackDeviceIdentifier: NSString; message 'playbackDeviceIdentifier';
    procedure setChannelMapping (channelMapping: NSArray); message 'setChannelMapping:'; deprecated 'in 10_5, 10_9';
    function channelMapping: NSArray; message 'channelMapping'; deprecated 'in 10_5, 10_9';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
    function initWithPasteboardPropertyList_ofType (propertyList: id; type_: NSString): id; message 'initWithPasteboardPropertyList:ofType:';
    function pasteboardPropertyListForType (type_: NSString): id; message 'pasteboardPropertyListForType:';
    class function readableTypesForPasteboard (pasteboard: NSPasteboard): NSArray; message 'readableTypesForPasteboard:';
    class function readingOptionsForType_pasteboard (type_: NSString; pasteboard: NSPasteboard): NSPasteboardReadingOptions; message 'readingOptionsForType:pasteboard:';
    function writableTypesForPasteboard (pasteboard: NSPasteboard): NSArray; message 'writableTypesForPasteboard:';
    function writingOptionsForType_pasteboard (type_: NSString; pasteboard: NSPasteboard): NSPasteboardWritingOptions; message 'writingOptionsForType:pasteboard:';
  end;


type
  NSSound_NSDeprecated = objccategory external name 'NSDeprecated' (NSSound)
    class function soundUnfilteredFileTypes: NSArray; message 'soundUnfilteredFileTypes'; deprecated 'in 10_0, 10_5';
    class function soundUnfilteredPasteboardTypes: NSArray; message 'soundUnfilteredPasteboardTypes'; deprecated 'in 10_0, 10_5';
  end;
{$endif}

{$ifdef PROTOCOLS}

type
  NSSoundDelegateProtocol = objcprotocol external name 'NSSoundDelegate' (NSObjectProtocol)
  optional
    procedure sound_didFinishPlaying (sound: NSSound; aBool: ObjCBOOL); message 'sound:didFinishPlaying:';
  end;
{$endif}

{$ifdef CLASSES}

type
  NSBundleSoundExtensions = objccategory external (NSBundle)
    function pathForSoundResource (name: NSString): NSString; message 'pathForSoundResource:';
  end;
{$endif}

