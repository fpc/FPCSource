{ Parsed from Appkit.framework NSInputManager.h }

{$ifdef TYPES}
{$ifndef NSINPUTMANAGER_PAS_T}
{$define NSINPUTMANAGER_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSINPUTMANAGER_PAS_R}
{$define NSINPUTMANAGER_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSINPUTMANAGER_PAS_F}
{$define NSINPUTMANAGER_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSINPUTMANAGER_PAS_S}
{$define NSINPUTMANAGER_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSTextInputProtocol = objcprotocol;
  NSInputManager = objcclass;
  NSInputManagerPointer = ^NSInputManager;
  NSInputManagerPtr = NSInputManagerPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSINPUTMANAGER_PAS_C}
{$define NSINPUTMANAGER_PAS_C}

{ NSInputManager }
  NSInputManager = objcclass external (NSObject, NSTextInputProtocol)
  private
    _currentClient: id;
    _server: id;
    _bundleObj: id;
    _keybindings: id;
    _trueName: NSString;
    _connectionName: NSString;
    _hostName: NSString;
    _procToExec: NSString;
    _visibleName: NSString;
    _bundleName: NSString;
    _language: NSString;
    _image: NSImage;
    _flags: cuint;
    _keyBindingsName: NSString;
    _reservedInputManager2: cint;
    
  public
    class function currentInputManager: NSInputManager; message 'currentInputManager';
    class procedure cycleToNextInputLanguage(sender: id); message 'cycleToNextInputLanguage:';
    class procedure cycleToNextInputServerInLanguage(sender: id); message 'cycleToNextInputServerInLanguage:';
    function initWithName_host(inputServerName: NSString; hostName: NSString): NSInputManager; message 'initWithName:host:'; deprecated 'in Mac OS X 10.6 and later';
    function localizedInputManagerName: NSString; message 'localizedInputManagerName'; deprecated 'in Mac OS X 10.6 and later';
    procedure markedTextAbandoned(cli: id); message 'markedTextAbandoned:';
    procedure markedTextSelectionChanged_client(newSel: NSRange; cli: id); message 'markedTextSelectionChanged:client:';
    function wantsToInterpretAllKeystrokes: Boolean; message 'wantsToInterpretAllKeystrokes'; deprecated 'in Mac OS X 10.6 and later';
    function language: NSString; message 'language'; deprecated 'in Mac OS X 10.6 and later';
    function image: NSImage; message 'image'; deprecated 'in Mac OS X 10.6 and later';
    function server: NSInputServer; message 'server'; deprecated 'in Mac OS X 10.6 and later';
    function wantsToHandleMouseEvents: Boolean; message 'wantsToHandleMouseEvents';
    function handleMouseEvent(theMouseEvent: NSEvent): Boolean; message 'handleMouseEvent:';
    function wantsToDelayTextChangeNotifications: Boolean; message 'wantsToDelayTextChangeNotifications'; deprecated 'in Mac OS X 10.6 and later';

    { Adopted Protocols }
    procedure insertText(aString: id);
    procedure doCommandBySelector(aSelector: SEL);
    procedure setMarkedText_selectedRange(aString: id; selRange: NSRange);
    procedure unmarkText;
    function hasMarkedText: Boolean;
    function conversationIdentifier: NSInteger;
    function attributedSubstringFromRange(theRange: NSRange): NSAttributedString;
    function markedRange: NSRange;
    function selectedRange: NSRange;
    function firstRectForCharacterRange(theRange: NSRange): NSRect;
    function characterIndexForPoint(thePoint: NSPoint): NSUInteger;
    function validAttributesForMarkedText: NSArray;
  end;

{$endif}
{$endif}
{$ifdef PROTOCOLS}
{$ifndef NSINPUTMANAGER_PAS_P}
{$define NSINPUTMANAGER_PAS_P}
  
{ NSTextInput Protocol }
  NSTextInputProtocol = objcprotocol external name 'NSTextInput'
    procedure insertText(aString: id); message 'insertText:';
    procedure doCommandBySelector(aSelector: SEL); message 'doCommandBySelector:';
    procedure setMarkedText_selectedRange(aString: id; selRange: NSRange); message 'setMarkedText:selectedRange:';
    procedure unmarkText; message 'unmarkText';
    function hasMarkedText: Boolean; message 'hasMarkedText';
    function conversationIdentifier: NSInteger; message 'conversationIdentifier';
    function attributedSubstringFromRange(theRange: NSRange): NSAttributedString; message 'attributedSubstringFromRange:';
    function markedRange: NSRange; message 'markedRange';
    function selectedRange: NSRange; message 'selectedRange';
    function firstRectForCharacterRange(theRange: NSRange): NSRect; message 'firstRectForCharacterRange:';
    function characterIndexForPoint(thePoint: NSPoint): NSUInteger; message 'characterIndexForPoint:';
    function validAttributesForMarkedText: NSArray; message 'validAttributesForMarkedText';
  end;
{$endif}
{$endif}
