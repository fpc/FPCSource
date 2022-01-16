{ Parsed from AppKit.framework NSSpeechRecognizer.h }


{$ifdef TYPES}
type
  NSSpeechRecognizerPtr = ^NSSpeechRecognizer;
  NSSpeechRecognizerDelegateProtocolPtr = ^NSSpeechRecognizerDelegateProtocol;
{$endif}

{$ifdef CLASSES}

type
  NSSpeechRecognizer = objcclass external (NSObject)
  private
    _privateNSSpeechRecognizerVars: id;
  public
    function init: instancetype; message 'init';
    procedure startListening; message 'startListening';
    procedure stopListening; message 'stopListening';
    procedure setDelegate(newValue: NSSpeechRecognizerDelegateProtocol); message 'setDelegate:';
    function delegate: NSSpeechRecognizerDelegateProtocol; message 'delegate';
    procedure setCommands(newValue: NSArray); message 'setCommands:';
    function commands: NSArray; message 'commands';
    procedure setDisplayedCommandsTitle(newValue: NSString); message 'setDisplayedCommandsTitle:';
    function displayedCommandsTitle: NSString; message 'displayedCommandsTitle';
    procedure setListensInForegroundOnly(newValue: ObjCBOOL); message 'setListensInForegroundOnly:';
    function listensInForegroundOnly: ObjCBOOL; message 'listensInForegroundOnly';
    procedure setBlocksOtherRecognizers(newValue: ObjCBOOL); message 'setBlocksOtherRecognizers:';
    function blocksOtherRecognizers: ObjCBOOL; message 'blocksOtherRecognizers';
  end;
{$endif}

{$ifdef PROTOCOLS}

type
  NSSpeechRecognizerDelegateProtocol = objcprotocol external name 'NSSpeechRecognizerDelegate' (NSObjectProtocol)
  optional
    procedure speechRecognizer_didRecognizeCommand (sender: NSSpeechRecognizer; command: id); message 'speechRecognizer:didRecognizeCommand:';
  end;
{$endif}

