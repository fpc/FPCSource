{ Parsed from Appkit.framework NSTabView.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Thu Sep 17 14:52:47 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSTABVIEW_PAS_H}
{$define NSTABVIEW_PAS_H}
type
  NSTabViewPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSTABVIEW_PAS_T}
{$define NSTABVIEW_PAS_T}

{ Defines }
const
  NSAppKitVersionNumberWithDirectionalTabs = 631.0;

{ Constants }

const
  NSLeftTabsBezelBorder = 1;
  NSBottomTabsBezelBorder = 2;
  NSRightTabsBezelBorder = 3;
  NSNoTabsBezelBorder = 4;
  NSNoTabsLineBorder = 5;
  NSNoTabsNoBorder = 6;

{ Types }
type
  NSTabViewType = NSUInteger;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSTABVIEW_PAS_R}
{$define NSTABVIEW_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSTABVIEW_PAS_F}
{$define NSTABVIEW_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSTABVIEW_PAS_T}
{$define NSTABVIEW_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSTabView = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSTABVIEW_PAS_C}
{$define NSTABVIEW_PAS_C}

{ NSTabView }
  NSTabView = objcclass(NSView)
  private
    __tabViewItems: id;
    __selectedTabViewItem: NSTabViewItem;
    __font: NSFont;
    __tabViewType: NSTabViewType;
    __allowTruncatedLabels: Boolean;
    __delegate: id;
    __tabViewUnusedBOOL1: Boolean;
    __drawsBackground: Boolean;
    __pressedTabViewItem: NSTabViewItem;
    __endTabWidth: NSInteger;
    __maxOverlap: NSInteger;
    __tabHeight: CGFloat;
    __tabViewItemWithKeyView: NSTabViewItem;
    __originalNextKeyView: NSView;
    __delegateRespondTo: bitpacked record
        shouldSelectTabViewItem: 0..1;
        willSelectTabViewItem: 0..1;
        didSelectTabViewItem: 0..1;
        didChangeNumberOfTabViewItems: 0..1;
        reserved: 0..((1 shl 28)-1);
      end;
    __flags: bitpacked record
        needsLayout: 0..1;
        controlTint: 0..((1 shl 3)-1);
        controlSize: 0..((1 shl 2)-1);
        wiringNibConnections: 0..1;
        wiringInteriorLastKeyView: 0..1;
        originalNextKeyViewChanged: 0..1;
        liveResizeSkippedResetToolTips: 0..1;
        reserved: 0..((1 shl 22)-1);
      end;
    __focusedTabViewItem: NSTabViewItem;
    __tabViewUnused2: Pointer;
    
  public
    class function alloc: NSTabView; message 'alloc';

    procedure selectTabViewItem(tabViewItem: NSTabViewItem); message 'selectTabViewItem:';
    procedure selectTabViewItemAtIndex(index: NSInteger); message 'selectTabViewItemAtIndex:';
    procedure selectTabViewItemWithIdentifier(identifier: id); message 'selectTabViewItemWithIdentifier:';
    procedure takeSelectedTabViewItemFromSender(sender: id); message 'takeSelectedTabViewItemFromSender:';
    procedure selectFirstTabViewItem(sender: id); message 'selectFirstTabViewItem:';
    procedure selectLastTabViewItem(sender: id); message 'selectLastTabViewItem:';
    procedure selectNextTabViewItem(sender: id); message 'selectNextTabViewItem:';
    procedure selectPreviousTabViewItem(sender: id); message 'selectPreviousTabViewItem:';
    function selectedTabViewItem: NSTabViewItem; message 'selectedTabViewItem';
    function font: NSFont; message 'font';
    function tabViewType: NSTabViewType; message 'tabViewType';
    function tabViewItems: NSArray; message 'tabViewItems';
    function allowsTruncatedLabels: Boolean; message 'allowsTruncatedLabels';
    function minimumSize: NSSize; message 'minimumSize';
    function drawsBackground: Boolean; message 'drawsBackground';
    function controlTint: NSControlTint; message 'controlTint';
    function controlSize: NSControlSize; message 'controlSize';
    procedure setFont(font_: NSFont); message 'setFont:';
    procedure setTabViewType(tabViewType_: NSTabViewType); message 'setTabViewType:';
    procedure setAllowsTruncatedLabels(allowTruncatedLabels: Boolean); message 'setAllowsTruncatedLabels:';
    procedure setDrawsBackground(flag: Boolean); message 'setDrawsBackground:';
    procedure setControlTint(controlTint_: NSControlTint); message 'setControlTint:';
    procedure setControlSize(controlSize_: NSControlSize); message 'setControlSize:';
    procedure addTabViewItem(tabViewItem: NSTabViewItem); message 'addTabViewItem:';
    procedure insertTabViewItem_atIndex(tabViewItem: NSTabViewItem; index: NSInteger); message 'insertTabViewItem:atIndex:';
    procedure removeTabViewItem(tabViewItem: NSTabViewItem); message 'removeTabViewItem:';
    procedure setDelegate(anObject: id); message 'setDelegate:';
    function delegate: id; message 'delegate';
    function tabViewItemAtPoint(point: NSPoint): NSTabViewItem; message 'tabViewItemAtPoint:';
    function contentRect: NSRect; message 'contentRect';
    function numberOfTabViewItems: NSInteger; message 'numberOfTabViewItems';
    function indexOfTabViewItem(tabViewItem: NSTabViewItem): NSInteger; message 'indexOfTabViewItem:';
    function tabViewItemAtIndex(index: NSInteger): NSTabViewItem; message 'tabViewItemAtIndex:';
    function indexOfTabViewItemWithIdentifier(identifier: id): NSInteger; message 'indexOfTabViewItemWithIdentifier:';
  end; external;

{$endif}
{$endif}
