{ Parsed from AppKit.framework NSInputServer.h }


{$ifdef TYPES}
type
  NSInputServerPtr = ^NSInputServer;
  NSInputServiceProviderProtocolPtr = ^NSInputServiceProviderProtocol;
  NSInputServerMouseTrackerProtocolPtr = ^NSInputServerMouseTrackerProtocol;
{$endif}

{$ifdef PROTOCOLS}

type
  NSInputServiceProviderProtocol = objcprotocol external name 'NSInputServiceProvider'
    procedure insertText_client (aString: id; sender: id); message 'insertText:client:'; deprecated 'in 10_0, 10_6';
    procedure doCommandBySelector_client (aSelector: SEL; sender: id); message 'doCommandBySelector:client:'; deprecated 'in 10_0, 10_6';
    procedure markedTextAbandoned (sender: id); message 'markedTextAbandoned:'; deprecated 'in 10_0, 10_6';
    procedure markedTextSelectionChanged_client (newSel: NSRange; sender: id); message 'markedTextSelectionChanged:client:'; deprecated 'in 10_0, 10_6';
    procedure terminate (sender: id); message 'terminate:'; deprecated 'in 10_0, 10_6';
    function canBeDisabled: ObjCBOOL; message 'canBeDisabled'; deprecated 'in 10_0, 10_6';
    function wantsToInterpretAllKeystrokes: ObjCBOOL; message 'wantsToInterpretAllKeystrokes'; deprecated 'in 10_0, 10_6';
    function wantsToHandleMouseEvents: ObjCBOOL; message 'wantsToHandleMouseEvents'; deprecated 'in 10_0, 10_6';
    function wantsToDelayTextChangeNotifications: ObjCBOOL; message 'wantsToDelayTextChangeNotifications'; deprecated 'in 10_0, 10_6';
    procedure inputClientBecomeActive (sender: id); message 'inputClientBecomeActive:'; deprecated 'in 10_0, 10_6';
    procedure inputClientResignActive (sender: id); message 'inputClientResignActive:'; deprecated 'in 10_0, 10_6';
    procedure inputClientEnabled (sender: id); message 'inputClientEnabled:'; deprecated 'in 10_0, 10_6';
    procedure inputClientDisabled (sender: id); message 'inputClientDisabled:'; deprecated 'in 10_0, 10_6';
    procedure activeConversationWillChange_fromOldConversation (sender: id; oldConversation: NSInteger); message 'activeConversationWillChange:fromOldConversation:'; deprecated 'in 10_0, 10_6';
    procedure activeConversationChanged_toNewConversation (sender: id; newConversation: NSInteger); message 'activeConversationChanged:toNewConversation:'; deprecated 'in 10_0, 10_6';
  end;

type
  NSInputServerMouseTrackerProtocol = objcprotocol external name 'NSInputServerMouseTracker'
    function mouseDownOnCharacterIndex_atCoordinate_withModifier_client (theIndex: NSUInteger; thePoint: NSPoint; theFlags: NSUInteger; sender: id): ObjCBOOL; message 'mouseDownOnCharacterIndex:atCoordinate:withModifier:client:'; deprecated 'in 10_0, 10_6';
    function mouseDraggedOnCharacterIndex_atCoordinate_withModifier_client (theIndex: NSUInteger; thePoint: NSPoint; theFlags: NSUInteger; sender: id): ObjCBOOL; message 'mouseDraggedOnCharacterIndex:atCoordinate:withModifier:client:'; deprecated 'in 10_0, 10_6';
    procedure mouseUpOnCharacterIndex_atCoordinate_withModifier_client (theIndex: NSUInteger; thePoint: NSPoint; theFlags: NSUInteger; sender: id); message 'mouseUpOnCharacterIndex:atCoordinate:withModifier:client:'; deprecated 'in 10_0, 10_6';
  end;
{$endif}

{$ifdef CLASSES}

type
  NSInputServer = objcclass external (NSObject, NSInputServiceProviderProtocol, NSInputServerMouseTrackerProtocol)
  private
    _name: NSString;
    _delegate: id;

    { Adopted protocols }
    procedure activeConversationChanged_toNewConversation (sender: id; newConversation: NSInteger); message 'activeConversationChanged:toNewConversation:'; deprecated 'in 10_0, 10_6';
    procedure activeConversationWillChange_fromOldConversation (sender: id; oldConversation: NSInteger); message 'activeConversationWillChange:fromOldConversation:'; deprecated 'in 10_0, 10_6';
    function canBeDisabled: ObjCBOOL; message 'canBeDisabled'; deprecated 'in 10_0, 10_6';
    procedure doCommandBySelector_client (aSelector: SEL; sender: id); message 'doCommandBySelector:client:'; deprecated 'in 10_0, 10_6';
    procedure inputClientBecomeActive (sender: id); message 'inputClientBecomeActive:'; deprecated 'in 10_0, 10_6';
    procedure inputClientDisabled (sender: id); message 'inputClientDisabled:'; deprecated 'in 10_0, 10_6';
    procedure inputClientEnabled (sender: id); message 'inputClientEnabled:'; deprecated 'in 10_0, 10_6';
    procedure inputClientResignActive (sender: id); message 'inputClientResignActive:'; deprecated 'in 10_0, 10_6';
    procedure insertText_client (aString: id; sender: id); message 'insertText:client:'; deprecated 'in 10_0, 10_6';
    procedure markedTextAbandoned (sender: id); message 'markedTextAbandoned:'; deprecated 'in 10_0, 10_6';
    procedure markedTextSelectionChanged_client (newSel: NSRange; sender: id); message 'markedTextSelectionChanged:client:'; deprecated 'in 10_0, 10_6';
    function mouseDownOnCharacterIndex_atCoordinate_withModifier_client (theIndex: NSUInteger; thePoint: NSPoint; theFlags: NSUInteger; sender: id): ObjCBOOL; message 'mouseDownOnCharacterIndex:atCoordinate:withModifier:client:'; deprecated 'in 10_0, 10_6';
    function mouseDraggedOnCharacterIndex_atCoordinate_withModifier_client (theIndex: NSUInteger; thePoint: NSPoint; theFlags: NSUInteger; sender: id): ObjCBOOL; message 'mouseDraggedOnCharacterIndex:atCoordinate:withModifier:client:'; deprecated 'in 10_0, 10_6';
    procedure mouseUpOnCharacterIndex_atCoordinate_withModifier_client (theIndex: NSUInteger; thePoint: NSPoint; theFlags: NSUInteger; sender: id); message 'mouseUpOnCharacterIndex:atCoordinate:withModifier:client:'; deprecated 'in 10_0, 10_6';
    procedure terminate (sender: id); message 'terminate:'; deprecated 'in 10_0, 10_6';
    function wantsToDelayTextChangeNotifications: ObjCBOOL; message 'wantsToDelayTextChangeNotifications'; deprecated 'in 10_0, 10_6';
    function wantsToHandleMouseEvents: ObjCBOOL; message 'wantsToHandleMouseEvents'; deprecated 'in 10_0, 10_6';
    function wantsToInterpretAllKeystrokes: ObjCBOOL; message 'wantsToInterpretAllKeystrokes'; deprecated 'in 10_0, 10_6';
  end;
{$endif}

