{ Parsed from AppKit.framework NSInputManager.h }


{$ifdef TYPES}
type
  NSInputManagerPtr = ^NSInputManager;
  NSTextInputProtocolPtr = ^NSTextInputProtocol;
{$endif}

{$ifdef PROTOCOLS}

type
  NSTextInputProtocol = objcprotocol external name 'NSTextInput'
    procedure insertText (aString: id); message 'insertText:'; deprecated 'in 10_0, 10_6';
    procedure doCommandBySelector (aSelector: SEL); message 'doCommandBySelector:'; deprecated 'in 10_0, 10_6';
    procedure setMarkedText_selectedRange (aString: id; selRange: NSRange); message 'setMarkedText:selectedRange:'; deprecated 'in 10_0, 10_6';
    procedure unmarkText; message 'unmarkText'; deprecated 'in 10_0, 10_6';
    function hasMarkedText: ObjCBOOL; message 'hasMarkedText'; deprecated 'in 10_0, 10_6';
    function conversationIdentifier: NSInteger; message 'conversationIdentifier'; deprecated 'in 10_0, 10_6';
    function attributedSubstringFromRange (theRange: NSRange): NSAttributedString; message 'attributedSubstringFromRange:'; deprecated 'in 10_0, 10_6';
    function markedRange: NSRange; message 'markedRange'; deprecated 'in 10_0, 10_6';
    function selectedRange: NSRange; message 'selectedRange'; deprecated 'in 10_0, 10_6';
    function firstRectForCharacterRange (theRange: NSRange): NSRect; message 'firstRectForCharacterRange:'; deprecated 'in 10_0, 10_6';
    function characterIndexForPoint (thePoint: NSPoint): NSUInteger; message 'characterIndexForPoint:'; deprecated 'in 10_0, 10_6';
    function validAttributesForMarkedText: NSArray; message 'validAttributesForMarkedText'; deprecated 'in 10_0, 10_6';
  end;
{$endif}

{$ifdef CLASSES}

type
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
    class function currentInputManager: NSInputManager; message 'currentInputManager'; deprecated 'in 10_0, 10_6';
    class procedure cycleToNextInputLanguage (sender: id); message 'cycleToNextInputLanguage:'; deprecated 'in 10_0, 10_6';
    class procedure cycleToNextInputServerInLanguage (sender: id); message 'cycleToNextInputServerInLanguage:'; deprecated 'in 10_0, 10_6';
    function initWithName_host (inputServerName: NSString; hostName: NSString): NSInputManager; message 'initWithName:host:'; deprecated 'in 10_0, 10_6';
    function localizedInputManagerName: NSString; message 'localizedInputManagerName'; deprecated 'in 10_0, 10_6';
    procedure markedTextAbandoned (cli: id); message 'markedTextAbandoned:'; deprecated 'in 10_0, 10_6';
    procedure markedTextSelectionChanged_client (newSel: NSRange; cli: id); message 'markedTextSelectionChanged:client:'; deprecated 'in 10_0, 10_6';
    function wantsToInterpretAllKeystrokes: ObjCBOOL; message 'wantsToInterpretAllKeystrokes'; deprecated 'in 10_0, 10_6';
    function language: NSString; message 'language'; deprecated 'in 10_0, 10_6';
    function image: NSImage; message 'image'; deprecated 'in 10_0, 10_6';
    function server: NSInputServer; message 'server'; deprecated 'in 10_0, 10_6';
    function wantsToHandleMouseEvents: ObjCBOOL; message 'wantsToHandleMouseEvents'; deprecated 'in 10_0, 10_6';
    function handleMouseEvent (theMouseEvent: NSEvent): ObjCBOOL; message 'handleMouseEvent:'; deprecated 'in 10_0, 10_6';
    function wantsToDelayTextChangeNotifications: ObjCBOOL; message 'wantsToDelayTextChangeNotifications'; deprecated 'in 10_0, 10_6';

    { Adopted protocols }
    function attributedSubstringFromRange (theRange: NSRange): NSAttributedString; message 'attributedSubstringFromRange:'; deprecated 'in 10_0, 10_6';
    function characterIndexForPoint (thePoint: NSPoint): NSUInteger; message 'characterIndexForPoint:'; deprecated 'in 10_0, 10_6';
    function conversationIdentifier: NSInteger; message 'conversationIdentifier'; deprecated 'in 10_0, 10_6';
    procedure doCommandBySelector (aSelector: SEL); message 'doCommandBySelector:'; deprecated 'in 10_0, 10_6';
    function firstRectForCharacterRange (theRange: NSRange): NSRect; message 'firstRectForCharacterRange:'; deprecated 'in 10_0, 10_6';
    function hasMarkedText: ObjCBOOL; message 'hasMarkedText'; deprecated 'in 10_0, 10_6';
    procedure insertText (aString: id); message 'insertText:'; deprecated 'in 10_0, 10_6';
    function markedRange: NSRange; message 'markedRange'; deprecated 'in 10_0, 10_6';
    function selectedRange: NSRange; message 'selectedRange'; deprecated 'in 10_0, 10_6';
    procedure setMarkedText_selectedRange (aString: id; selRange: NSRange); message 'setMarkedText:selectedRange:'; deprecated 'in 10_0, 10_6';
    procedure unmarkText; message 'unmarkText'; deprecated 'in 10_0, 10_6';
    function validAttributesForMarkedText: NSArray; message 'validAttributesForMarkedText'; deprecated 'in 10_0, 10_6';
  end;
{$endif}

