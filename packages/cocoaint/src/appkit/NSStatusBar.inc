{ Parsed from Appkit.framework NSStatusBar.h }

{$ifdef TYPES}
{$ifndef NSSTATUSBAR_PAS_T}
{$define NSSTATUSBAR_PAS_T}

{ Defines }
const
  NSVariableStatusItemLength = -1;
  NSSquareStatusItemLength = -2;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSSTATUSBAR_PAS_R}
{$define NSSTATUSBAR_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSSTATUSBAR_PAS_F}
{$define NSSTATUSBAR_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSSTATUSBAR_PAS_S}
{$define NSSTATUSBAR_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSStatusBar = objcclass;
  NSStatusBarPointer = ^NSStatusBar;
  NSStatusBarPtr = NSStatusBarPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSSTATUSBAR_PAS_C}
{$define NSSTATUSBAR_PAS_C}

{ NSStatusBar }
  NSStatusBar = objcclass external (NSObject)
  private
    _items: id;
    _fReserved1: Pointer;
    _fReserved2: Pointer;
    _registeredForNote: NSInteger;
    
  public
    class function systemStatusBar: NSStatusBar; message 'systemStatusBar';
    function statusItemWithLength(length: CGFloat): NSStatusItem; message 'statusItemWithLength:';
    procedure removeStatusItem(item: NSStatusItem); message 'removeStatusItem:';
    function isVertical: Boolean; message 'isVertical';
    function thickness: CGFloat; message 'thickness';
  end;

{$endif}
{$endif}
