{ Parsed from Appkit.framework NSSound.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Tue Sep 8 15:31:01 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSSOUND_PAS_H}
{$define NSSOUND_PAS_H}
type
  NSSoundPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSSOUND_PAS_T}
{$define NSSOUND_PAS_T}

{ CFString constants }
var
  NSSoundPboardType: CFStringRef; external name '_NSSoundPboardType';

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSSOUND_PAS_R}
{$define NSSOUND_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSSOUND_PAS_F}
{$define NSSOUND_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSSOUND_PAS_T}
{$define NSSOUND_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSSound = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSSOUND_PAS_C}
{$define NSSOUND_PAS_C}

{ NSSound }
  NSSound = objcclass(NSObject, NSCopyingProtocol, NSCodingProtocol)
  private
    __delegate: id;
    __info: id;
    __reserved: id;
    __sFlags: culong;
    
  public
    class function alloc: NSSound; message 'alloc';

    class function soundNamed(name_: NSString): id; message 'soundNamed:';
    function initWithContentsOfURL_byReference(url: NSURL; byRef: Boolean): id; message 'initWithContentsOfURL:byReference:';
    function initWithContentsOfFile_byReference(path: NSString; byRef: Boolean): id; message 'initWithContentsOfFile:byReference:';
    function initWithData(data: NSData): id; message 'initWithData:';
    function setName(string_: NSString): Boolean; message 'setName:';
    function name: NSString; message 'name';
    class function canInitWithPasteboard(pasteboard: NSPasteboard): Boolean; message 'canInitWithPasteboard:';
    class function soundUnfilteredTypes: NSArray; message 'soundUnfilteredTypes';
    function initWithPasteboard(pasteboard: NSPasteboard): id; message 'initWithPasteboard:';
    procedure writeToPasteboard(pasteboard: NSPasteboard); message 'writeToPasteboard:';
    function play: Boolean; message 'play';
    function pause: Boolean; message 'pause';
    function resume: Boolean; message 'resume';
    function stop: Boolean; message 'stop';
    function isPlaying: Boolean; message 'isPlaying';
    function delegate: id; message 'delegate';
    procedure setDelegate(aDelegate: id); message 'setDelegate:';
    function duration: NSTimeInterval; message 'duration';
    procedure setVolume(volume_: single); message 'setVolume:';
    function volume: single; message 'volume';
    function currentTime: NSTimeInterval; message 'currentTime';
    procedure setCurrentTime(seconds: NSTimeInterval); message 'setCurrentTime:';
    procedure setLoops(val: Boolean); message 'setLoops:';
    function loops: Boolean; message 'loops';
    procedure setPlaybackDeviceIdentifier(deviceUID: NSString); message 'setPlaybackDeviceIdentifier:';
    function playbackDeviceIdentifier: NSString; message 'playbackDeviceIdentifier';
    procedure setChannelMapping(channelMapping_: NSArray); message 'setChannelMapping:';
    function channelMapping: NSArray; message 'channelMapping';

    { Category: NSDeprecated }
    class function soundUnfilteredFileTypes: NSArray; message 'soundUnfilteredFileTypes';
    class function soundUnfilteredPasteboardTypes: NSArray; message 'soundUnfilteredPasteboardTypes';
  end; external;

{$endif}
{$endif}
