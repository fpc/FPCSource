{ Parsed from Appkit.framework NSDrawer.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Tue Sep 8 15:31:02 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSDRAWER_PAS_H}
{$define NSDRAWER_PAS_H}
type
  NSDrawerPointer = Pointer;

{$endif}
{$endif}

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
  NSDrawerState = culong;

{ CFString constants }
var
  NSDrawerWillOpenNotification: CFStringRef; external name '_NSDrawerWillOpenNotification';
  NSDrawerDidOpenNotification: CFStringRef; external name '_NSDrawerDidOpenNotification';
  NSDrawerWillCloseNotification: CFStringRef; external name '_NSDrawerWillCloseNotification';
  NSDrawerDidCloseNotification: CFStringRef; external name '_NSDrawerDidCloseNotification';

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
{$ifndef NSDRAWER_PAS_T}
{$define NSDRAWER_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSDrawer = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSDRAWER_PAS_C}
{$define NSDRAWER_PAS_C}

{ NSDrawer }
  NSDrawer = objcclass(NSResponder)
  private
    __drawerState: NSDrawerState;
    __drawerNextState: NSDrawerState;
    __drawerEdge: NSRectEdge;
    __drawerNextEdge: NSRectEdge;
    __drawerPreferredEdge: NSRectEdge;
    __drawerPercent: single;
    __drawerPercentSaved: single;
    __drawerLeadingOffset: CGFloat;
    __drawerTrailingOffset: CGFloat;
    __drawerLock: NSLock;
    __drawerWindow: NSWindow;
    __drawerParentWindow: NSWindow;
    __drawerNextParentWindow: NSWindow;
    __drawerSaveName: NSString;
    __drawerStartTime: CFAbsoluteTime;
    __drawerTotalTime: CFTimeInterval;
    __drawerLoop: CFRunLoopRef;
    __drawerTimer: CFRunLoopTimerRef; {garbage collector: __strong }
    __drawerDelegate: id;
    __drawerFlags: cuint;
    __drawerObserver: CFRunLoopObserverRef; {garbage collector: __strong }
    
  public
    class function alloc: NSDrawer; message 'alloc';

    function initWithContentSize_preferredEdge(contentSize_: NSSize; edge_: NSRectEdge): id; message 'initWithContentSize:preferredEdge:';
    procedure setParentWindow(parent: NSWindow); message 'setParentWindow:';
    function parentWindow: NSWindow; message 'parentWindow';
    procedure setContentView(aView: NSView); message 'setContentView:';
    function contentView: NSView; message 'contentView';
    procedure setPreferredEdge(edge_: NSRectEdge); message 'setPreferredEdge:';
    function preferredEdge: NSRectEdge; message 'preferredEdge';
    procedure setDelegate(anObject: id); message 'setDelegate:';
    function delegate: id; message 'delegate';
    procedure open; message 'open';
    procedure openOnEdge(edge_: NSRectEdge); message 'openOnEdge:';
    procedure close; message 'close';
    procedure toggle(sender: id); message 'toggle:';
    function state: clong; message 'state';
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
  end; external;

{$endif}
{$endif}
