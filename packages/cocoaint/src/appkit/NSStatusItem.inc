{ Parsed from Appkit.framework NSStatusItem.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Thu Sep 17 14:52:47 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSSTATUSITEM_PAS_H}
{$define NSSTATUSITEM_PAS_H}
type
  NSStatusItemPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSSTATUSITEM_PAS_T}
{$define NSSTATUSITEM_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSSTATUSITEM_PAS_R}
{$define NSSTATUSITEM_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSSTATUSITEM_PAS_F}
{$define NSSTATUSITEM_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSSTATUSITEM_PAS_T}
{$define NSSTATUSITEM_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSStatusItem = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSSTATUSITEM_PAS_C}
{$define NSSTATUSITEM_PAS_C}

{ NSStatusItem }
  NSStatusItem = objcclass(NSObject)
  private
    __fStatusBar: NSStatusBar;
    __fLength: CGFloat;
    __fWindow: NSWindow;
    __fView: NSView;
    __fPriority: cint;
    __fFlags: bitpacked record
        customView: 0..1;
        highlightMode: 0..1;
        hasAlternateImage: 0..1;
        hidden: 0..1;
        backgroundStyle: 0..((1 shl 4)-1);
        reserved: 0..((1 shl 24)-1);
      end;
    __fReserved1: id;
    __fReserved2: id;
    __fReserved3: id;
    __fReserved4: id;
    
  public
    class function alloc: NSStatusItem; message 'alloc';

    function statusBar: NSStatusBar; message 'statusBar';
    function length: CGFloat; message 'length';
    procedure setLength(length_: CGFloat); message 'setLength:';

    { Category: NSStatusItemCommon }
    function action: SEL; message 'action';
    procedure setAction(action_: SEL); message 'setAction:';
    function doubleAction: SEL; message 'doubleAction';
    procedure setDoubleAction(action_: SEL); message 'setDoubleAction:';
    function target: id; message 'target';
    procedure setTarget(target_: id); message 'setTarget:';
    function title: NSString; message 'title';
    procedure setTitle(title_: NSString); message 'setTitle:';
    function attributedTitle: NSAttributedString; message 'attributedTitle';
    procedure setAttributedTitle(title_: NSAttributedString); message 'setAttributedTitle:';
    function image: NSImage; message 'image';
    procedure setImage(image_: NSImage); message 'setImage:';
    function alternateImage: NSImage; message 'alternateImage';
    procedure setAlternateImage(image_: NSImage); message 'setAlternateImage:';
    function menu: NSMenu; message 'menu';
    procedure setMenu(menu_: NSMenu); message 'setMenu:';
    function isEnabled: Boolean; message 'isEnabled';
    procedure setEnabled(enabled: Boolean); message 'setEnabled:';
    function toolTip: NSString; message 'toolTip';
    procedure setToolTip(toolTip_: NSString); message 'setToolTip:';
    procedure setHighlightMode(highlightMode_: Boolean); message 'setHighlightMode:';
    function highlightMode: Boolean; message 'highlightMode';
    function sendActionOn(mask: NSInteger): NSInteger; message 'sendActionOn:';
    procedure popUpStatusItemMenu(menu_: NSMenu); message 'popUpStatusItemMenu:';
    procedure drawStatusBarBackgroundInRect_withHighlight(rect: NSRect; highlight: Boolean); message 'drawStatusBarBackgroundInRect:withHighlight:';

    { Category: NSStatusItemView }
    function view: NSView; message 'view';
    procedure setView(view_: NSView); message 'setView:';
  end; external;

{$endif}
{$endif}
