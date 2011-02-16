{ Parsed from Appkit.framework NSDrawer.h }

{$ifdef TYPES}
{$ifndef NSDRAWER_PAS_T}
{$define NSDRAWER_PAS_T}

{ Constants }

const
  NSDrawerClosedState = 0;
  NSDrawerOpeningState = 1;
  NSDrawerOpenState = 2;
  NSDrawerClosingState = 3;

{ Types }
type
  NSDrawerState = NSUInteger;
  NSDrawerStatePtr = ^NSDrawerState;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSDRAWER_PAS_R}
{$define NSDRAWER_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSDRAWER_PAS_F}
{$define NSDRAWER_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSDRAWER_PAS_S}
{$define NSDRAWER_PAS_S}

{ External string constants }
var
  NSDrawerWillOpenNotification: NSString; cvar; external;
  NSDrawerDidOpenNotification: NSString; cvar; external;
  NSDrawerWillCloseNotification: NSString; cvar; external;
  NSDrawerDidCloseNotification: NSString; cvar; external;

{$endif}
{$endif}

{$ifdef FORWARD}
  NSDrawerDelegateProtocol = objcprotocol;
  NSDrawer = objcclass;
  NSDrawerPointer = ^NSDrawer;
  NSDrawerPtr = NSDrawerPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSDRAWER_PAS_C}
{$define NSDRAWER_PAS_C}

{ NSDrawer }
  NSDrawer = objcclass external (NSResponder)
  private
    _drawerState: NSDrawerState;
    _drawerNextState: NSDrawerState;
    _drawerEdge: NSRectEdge;
    _drawerNextEdge: NSRectEdge;
    _drawerPreferredEdge: NSRectEdge;
    _drawerPercent: single;
    _drawerPercentSaved: single;
    _drawerLeadingOffset: CGFloat;
    _drawerTrailingOffset: CGFloat;
    _drawerLock: NSLock;
    _drawerWindow: NSWindow;
    _drawerParentWindow: NSWindow;
    _drawerNextParentWindow: NSWindow;
    _drawerSaveName: NSString;
    _drawerStartTime: CFAbsoluteTime;
    _drawerTotalTime: CFTimeInterval;
    _drawerLoop: CFRunLoopRef;
    _drawerTimer: CFRunLoopTimerRef; {garbage collector: __strong }
    _drawerDelegate: id;
    _drawerFlags: cuint;
    _drawerObserver: CFRunLoopObserverRef; {garbage collector: __strong }
    
  public
    function initWithContentSize_preferredEdge(contentSize_: NSSize; edge_: NSRectEdge): id; message 'initWithContentSize:preferredEdge:';
    procedure setParentWindow(parent: NSWindow); message 'setParentWindow:';
    function parentWindow: NSWindow; message 'parentWindow';
    procedure setContentView(aView: NSView); message 'setContentView:';
    function contentView: NSView; message 'contentView';
    procedure setPreferredEdge(edge_: NSRectEdge); message 'setPreferredEdge:';
    function preferredEdge: NSRectEdge; message 'preferredEdge';
    procedure setDelegate(anObject: NSDrawerDelegateProtocol); message 'setDelegate:';
    function delegate: NSDrawerDelegateProtocol; message 'delegate';
    procedure open; message 'open';
    procedure openOnEdge(edge_: NSRectEdge); message 'openOnEdge:';
    procedure close; message 'close';
    procedure toggle(sender: id); message 'toggle:';
    function state: NSInteger; message 'state';
    function edge: NSRectEdge; message 'edge';
    procedure setContentSize(size: NSSize); message 'setContentSize:';
    function contentSize: NSSize; message 'contentSize';
    procedure setMinContentSize(size: NSSize); message 'setMinContentSize:';
    function minContentSize: NSSize; message 'minContentSize';
    procedure setMaxContentSize(size: NSSize); message 'setMaxContentSize:';
    function maxContentSize: NSSize; message 'maxContentSize';
    procedure setLeadingOffset(offset: CGFloat); message 'setLeadingOffset:';
    function leadingOffset: CGFloat; message 'leadingOffset';
    procedure setTrailingOffset(offset: CGFloat); message 'setTrailingOffset:';
    function trailingOffset: CGFloat; message 'trailingOffset';
  end;

{ DrawersCategory }
  DrawersCategory = objccategory external (NSWindow)
    function drawers: NSArray; message 'drawers';
  end;

{$endif}
{$endif}
{$ifdef PROTOCOLS}
{$ifndef NSDRAWER_PAS_P}
{$define NSDRAWER_PAS_P}
  
{ NSDrawerDelegate Protocol }
  NSDrawerDelegateProtocol = objcprotocol external name 'NSDrawerDelegate'
  optional
    function drawerShouldOpen(sender: NSDrawer): Boolean; message 'drawerShouldOpen:';
    function drawerShouldClose(sender: NSDrawer): Boolean; message 'drawerShouldClose:';
    function drawerWillResizeContents_toSize(sender: NSDrawer; contentSize: NSSize): NSSize; message 'drawerWillResizeContents:toSize:';
    procedure drawerWillOpen(notification: NSNotification); message 'drawerWillOpen:';
    procedure drawerDidOpen(notification: NSNotification); message 'drawerDidOpen:';
    procedure drawerWillClose(notification: NSNotification); message 'drawerWillClose:';
    procedure drawerDidClose(notification: NSNotification); message 'drawerDidClose:';
  end;
{$endif}
{$endif}
