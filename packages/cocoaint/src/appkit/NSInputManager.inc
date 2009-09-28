{ Parsed from Appkit.framework NSInputManager.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Mon Sep 28 15:49:53 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSINPUTMANAGER_PAS_H}
{$define NSINPUTMANAGER_PAS_H}
type
  NSInputManagerPointer = Pointer;

{$endif}
{$endif}

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

{$endif}

{$ifdef CLASSES}
{$ifndef NSINPUTMANAGER_PAS_C}
{$define NSINPUTMANAGER_PAS_C}

{ NSInputManager }
  NSInputManager = objcclass(NSObject, NSTextInputProtocol)
  private
    __currentClient: id;
    __server: id;
    __bundleObj: id;
    __keybindings: id;
    __trueName: NSString;
    __connectionName: NSString;
    __hostName: NSString;
    __procToExec: NSString;
    __visibleName: NSString;
    __bundleName: NSString;
    __language: NSString;
    __image: NSImage;
    __flags: cuint;
    __keyBindingsName: NSString;
    __reservedInputManager2: cint;
    
  public
    class function alloc: NSInputManager; message 'alloc';

    class function currentInputManager: NSInputManager; message 'currentInputManager';
    class procedure cycleToNextInputLanguage(sender: id); message 'cycleToNextInputLanguage:';
    class procedure cycleToNextInputServerInLanguage(sender: id); message 'cycleToNextInputServerInLanguage:';
    function initWithName_host(inputServerName: NSString; hostName: NSString): NSInputManager; message 'initWithName:host:';
    function localizedInputManagerName: NSString; message 'localizedInputManagerName';
    procedure markedTextAbandoned(cli: id); message 'markedTextAbandoned:';
    procedure markedTextSelectionChanged_client(newSel: NSRange; cli: id); message 'markedTextSelectionChanged:client:';
    function wantsToInterpretAllKeystrokes: Boolean; message 'wantsToInterpretAllKeystrokes';
    function language: NSString; message 'language';
    function image: NSImage; message 'image';
    function server: NSInputServer; message 'server';
    function wantsToHandleMouseEvents: Boolean; message 'wantsToHandleMouseEvents';
    function handleMouseEvent(theMouseEvent: NSEvent): Boolean; message 'handleMouseEvent:';
    function wantsToDelayTextChangeNotifications: Boolean; message 'wantsToDelayTextChangeNotifications';
  end; external;

{$endif}
{$endif}
{$ifdef PROTOCOLS}
{$ifndef NSINPUTMANAGER_PAS_P}
{$define NSINPUTMANAGER_PAS_P}
  
{ NSTextInput Protocol }
  NSTextInputProtocol = objcprotocol
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
  end; external name 'NSTextInput';
{$endif}
{$endif}
