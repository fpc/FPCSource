{ Parsed from Appkit.framework NSInputServer.h }

{$ifdef TYPES}
{$ifndef NSINPUTSERVER_PAS_T}
{$define NSINPUTSERVER_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSINPUTSERVER_PAS_R}
{$define NSINPUTSERVER_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSINPUTSERVER_PAS_F}
{$define NSINPUTSERVER_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSINPUTSERVER_PAS_S}
{$define NSINPUTSERVER_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSInputServiceProviderProtocol = objcprotocol;
  NSInputServerMouseTrackerProtocol = objcprotocol;
  NSInputServer = objcclass;
  NSInputServerPointer = ^NSInputServer;
  NSInputServerPtr = NSInputServerPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSINPUTSERVER_PAS_C}
{$define NSINPUTSERVER_PAS_C}

{ NSInputServer }
  NSInputServer = objcclass external (NSObject, NSInputServiceProviderProtocol, NSInputServerMouseTrackerProtocol)
  private
    _name: NSString;
    _delegate: id;
    
  public

    { Adopted Protocols }
    procedure insertText_client(aString: id; sender: id);
    procedure doCommandBySelector_client(aSelector: SEL; sender: id);
    procedure markedTextAbandoned(sender: id);
    procedure markedTextSelectionChanged_client(newSel: NSRange; sender: id);
    procedure terminate(sender: id);
    function canBeDisabled: Boolean;
    function wantsToInterpretAllKeystrokes: Boolean;
    function wantsToHandleMouseEvents: Boolean;
    function wantsToDelayTextChangeNotifications: Boolean;
    procedure inputClientBecomeActive(sender: id);
    procedure inputClientResignActive(sender: id);
    procedure inputClientEnabled(sender: id);
    procedure inputClientDisabled(sender: id);
    procedure activeConversationWillChange_fromOldConversation(sender: id; oldConversation: NSInteger);
    procedure activeConversationChanged_toNewConversation(sender: id; newConversation: NSInteger);
    function mouseDownOnCharacterIndex_atCoordinate_withModifier_client(theIndex: NSUInteger; thePoint: NSPoint; theFlags: NSUInteger; sender: id): Boolean;
    function mouseDraggedOnCharacterIndex_atCoordinate_withModifier_client(theIndex: NSUInteger; thePoint: NSPoint; theFlags: NSUInteger; sender: id): Boolean;
    procedure mouseUpOnCharacterIndex_atCoordinate_withModifier_client(theIndex: NSUInteger; thePoint: NSPoint; theFlags: NSUInteger; sender: id);
  end;

{$endif}
{$endif}
{$ifdef PROTOCOLS}
{$ifndef NSINPUTSERVER_PAS_P}
{$define NSINPUTSERVER_PAS_P}
  
{ NSInputServiceProvider Protocol }
  NSInputServiceProviderProtocol = objcprotocol external name 'NSInputServiceProvider'
    procedure insertText_client(aString: id; sender: id); message 'insertText:client:'; deprecated 'in Mac OS X 10.6 and later';
    procedure doCommandBySelector_client(aSelector: SEL; sender: id); message 'doCommandBySelector:client:'; deprecated 'in Mac OS X 10.6 and later';
    procedure markedTextAbandoned(sender: id); message 'markedTextAbandoned:'; deprecated 'in Mac OS X 10.6 and later';
    procedure markedTextSelectionChanged_client(newSel: NSRange; sender: id); message 'markedTextSelectionChanged:client:'; deprecated 'in Mac OS X 10.6 and later';
    procedure terminate(sender: id); message 'terminate:'; deprecated 'in Mac OS X 10.6 and later';
    function canBeDisabled: Boolean; message 'canBeDisabled'; deprecated 'in Mac OS X 10.6 and later';
    function wantsToInterpretAllKeystrokes: Boolean; message 'wantsToInterpretAllKeystrokes'; deprecated 'in Mac OS X 10.6 and later';
    function wantsToHandleMouseEvents: Boolean; message 'wantsToHandleMouseEvents'; deprecated 'in Mac OS X 10.6 and later';
    function wantsToDelayTextChangeNotifications: Boolean; message 'wantsToDelayTextChangeNotifications'; deprecated 'in Mac OS X 10.6 and later';
    procedure inputClientBecomeActive(sender: id); message 'inputClientBecomeActive:'; deprecated 'in Mac OS X 10.6 and later';
    procedure inputClientResignActive(sender: id); message 'inputClientResignActive:'; deprecated 'in Mac OS X 10.6 and later';
    procedure inputClientEnabled(sender: id); message 'inputClientEnabled:'; deprecated 'in Mac OS X 10.6 and later';
    procedure inputClientDisabled(sender: id); message 'inputClientDisabled:'; deprecated 'in Mac OS X 10.6 and later';
    procedure activeConversationWillChange_fromOldConversation(sender: id; oldConversation: NSInteger); message 'activeConversationWillChange:fromOldConversation:'; deprecated 'in Mac OS X 10.6 and later';
    procedure activeConversationChanged_toNewConversation(sender: id; newConversation: NSInteger); message 'activeConversationChanged:toNewConversation:'; deprecated 'in Mac OS X 10.6 and later';
  end;
  
{ NSInputServerMouseTracker Protocol }
  NSInputServerMouseTrackerProtocol = objcprotocol external name 'NSInputServerMouseTracker'
    function mouseDownOnCharacterIndex_atCoordinate_withModifier_client(theIndex: NSUInteger; thePoint: NSPoint; theFlags: NSUInteger; sender: id): Boolean; message 'mouseDownOnCharacterIndex:atCoordinate:withModifier:client:'; deprecated 'in Mac OS X 10.6 and later';
    function mouseDraggedOnCharacterIndex_atCoordinate_withModifier_client(theIndex: NSUInteger; thePoint: NSPoint; theFlags: NSUInteger; sender: id): Boolean; message 'mouseDraggedOnCharacterIndex:atCoordinate:withModifier:client:'; deprecated 'in Mac OS X 10.6 and later';
    procedure mouseUpOnCharacterIndex_atCoordinate_withModifier_client(theIndex: NSUInteger; thePoint: NSPoint; theFlags: NSUInteger; sender: id); message 'mouseUpOnCharacterIndex:atCoordinate:withModifier:client:'; deprecated 'in Mac OS X 10.6 and later';
  end;
{$endif}
{$endif}
