{ Parsed from Appkit.framework NSInputServer.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Mon Sep 28 15:49:53 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSINPUTSERVER_PAS_H}
{$define NSINPUTSERVER_PAS_H}
type
  NSInputServerPointer = Pointer;

{$endif}
{$endif}

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

{$endif}

{$ifdef CLASSES}
{$ifndef NSINPUTSERVER_PAS_C}
{$define NSINPUTSERVER_PAS_C}

{ NSInputServer }
  NSInputServer = objcclass(NSObject, NSInputServiceProviderProtocol, NSInputServerMouseTrackerProtocol)
  private
    __name: NSString;
    __delegate: id;
    
  public
    class function alloc: NSInputServer; message 'alloc';
  end; external;

{$endif}
{$endif}
{$ifdef PROTOCOLS}
{$ifndef NSINPUTSERVER_PAS_P}
{$define NSINPUTSERVER_PAS_P}
  
{ NSInputServiceProvider Protocol }
  NSInputServiceProviderProtocol = objcprotocol
    procedure insertText_client(aString: id; sender: id); message 'insertText:client:';
    procedure doCommandBySelector_client(aSelector: SEL; sender: id); message 'doCommandBySelector:client:';
    procedure markedTextAbandoned(sender: id); message 'markedTextAbandoned:';
    procedure markedTextSelectionChanged_client(newSel: NSRange; sender: id); message 'markedTextSelectionChanged:client:';
    procedure terminate(sender: id); message 'terminate:';
    function canBeDisabled: Boolean; message 'canBeDisabled';
    function wantsToInterpretAllKeystrokes: Boolean; message 'wantsToInterpretAllKeystrokes';
    function wantsToHandleMouseEvents: Boolean; message 'wantsToHandleMouseEvents';
    function wantsToDelayTextChangeNotifications: Boolean; message 'wantsToDelayTextChangeNotifications';
    procedure inputClientBecomeActive(sender: id); message 'inputClientBecomeActive:';
    procedure inputClientResignActive(sender: id); message 'inputClientResignActive:';
    procedure inputClientEnabled(sender: id); message 'inputClientEnabled:';
    procedure inputClientDisabled(sender: id); message 'inputClientDisabled:';
    procedure activeConversationWillChange_fromOldConversation(sender: id; oldConversation: NSInteger); message 'activeConversationWillChange:fromOldConversation:';
    procedure activeConversationChanged_toNewConversation(sender: id; newConversation: NSInteger); message 'activeConversationChanged:toNewConversation:';
  end; external name 'NSInputServiceProvider';
  
{ NSInputServerMouseTracker Protocol }
  NSInputServerMouseTrackerProtocol = objcprotocol
    function mouseDownOnCharacterIndex_atCoordinate_withModifier_client(theIndex: NSUInteger; thePoint: NSPoint; theFlags: NSUInteger; sender: id): Boolean; message 'mouseDownOnCharacterIndex:atCoordinate:withModifier:client:';
    function mouseDraggedOnCharacterIndex_atCoordinate_withModifier_client(theIndex: NSUInteger; thePoint: NSPoint; theFlags: NSUInteger; sender: id): Boolean; message 'mouseDraggedOnCharacterIndex:atCoordinate:withModifier:client:';
    procedure mouseUpOnCharacterIndex_atCoordinate_withModifier_client(theIndex: NSUInteger; thePoint: NSPoint; theFlags: NSUInteger; sender: id); message 'mouseUpOnCharacterIndex:atCoordinate:withModifier:client:';
  end; external name 'NSInputServerMouseTracker';
{$endif}
{$endif}
