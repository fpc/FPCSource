{ Parsed from Appkit.framework NSSpeechRecognizer.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Thu Oct 1 18:54:07 ICT 2009 }


{$ifdef TYPES}
{$ifndef NSSPEECHRECOGNIZER_PAS_T}
{$define NSSPEECHRECOGNIZER_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSSPEECHRECOGNIZER_PAS_R}
{$define NSSPEECHRECOGNIZER_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSSPEECHRECOGNIZER_PAS_F}
{$define NSSPEECHRECOGNIZER_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSSPEECHRECOGNIZER_PAS_S}
{$define NSSPEECHRECOGNIZER_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSSpeechRecognizer = objcclass;
  NSSpeechRecognizerPointer = ^NSSpeechRecognizer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSSPEECHRECOGNIZER_PAS_C}
{$define NSSPEECHRECOGNIZER_PAS_C}

{ NSSpeechRecognizer }
  NSSpeechRecognizer = objcclass(NSObject)
  private
    __privateNSSpeechRecognizerVars: id;
    
  public
    class function alloc: NSSpeechRecognizer; message 'alloc';

    function init: id; message 'init';
    procedure startListening; message 'startListening';
    procedure stopListening; message 'stopListening';
    function delegate: id; message 'delegate';
    procedure setDelegate(anObject: id); message 'setDelegate:';
    function commands: NSArray; message 'commands';
    procedure setCommands(commands_: NSArray); message 'setCommands:';
    function displayedCommandsTitle: NSString; message 'displayedCommandsTitle';
    procedure setDisplayedCommandsTitle(title: NSString); message 'setDisplayedCommandsTitle:';
    function listensInForegroundOnly: Boolean; message 'listensInForegroundOnly';
    procedure setListensInForegroundOnly(flag: Boolean); message 'setListensInForegroundOnly:';
    function blocksOtherRecognizers: Boolean; message 'blocksOtherRecognizers';
    procedure setBlocksOtherRecognizers(flag: Boolean); message 'setBlocksOtherRecognizers:';
  end; external;

{$endif}
{$endif}
