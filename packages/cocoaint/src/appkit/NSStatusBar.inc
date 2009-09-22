{ Parsed from Appkit.framework NSStatusBar.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Thu Sep 17 14:52:47 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSSTATUSBAR_PAS_H}
{$define NSSTATUSBAR_PAS_H}
type
  NSStatusBarPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSSTATUSBAR_PAS_T}
{$define NSSTATUSBAR_PAS_T}

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
{$ifndef NSSTATUSBAR_PAS_T}
{$define NSSTATUSBAR_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSStatusBar = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSSTATUSBAR_PAS_C}
{$define NSSTATUSBAR_PAS_C}

{ NSStatusBar }
  NSStatusBar = objcclass(NSObject)
  private
    __actions: NSMutableArray;
    __fReserved1: Pointer;
    __fReserved2: Pointer;
    __fReserved3: Pointer;
    
  public
    class function alloc: NSStatusBar; message 'alloc';

    class function systemStatusBar: NSStatusBar; message 'systemStatusBar';
    function statusItemWithLength(length: CGFloat): NSStatusItem; message 'statusItemWithLength:';
    procedure removeStatusItem(item: NSStatusItem); message 'removeStatusItem:';
    function isVertical: Boolean; message 'isVertical';
    function thickness: CGFloat; message 'thickness';
  end; external;

{$endif}
{$endif}
