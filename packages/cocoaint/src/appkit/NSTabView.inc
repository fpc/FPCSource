{ Parsed from AppKit.framework NSTabView.h }


{$ifdef TYPES}
type
  NSTabViewPtr = ^NSTabView;
  NSTabViewDelegateProtocolPtr = ^NSTabViewDelegateProtocol;
{$endif}

{$ifdef TYPES}
const
  NSAppKitVersionNumberWithDirectionalTabs = 631.0;

type
  NSTabViewType = NSUInteger;
  NSTabViewTypePtr = ^NSTabViewType;

const
  NSTopTabsBezelBorder = 0;
  NSLeftTabsBezelBorder = 1;
  NSBottomTabsBezelBorder = 2;
  NSRightTabsBezelBorder = 3;
  NSNoTabsBezelBorder = 4;
  NSNoTabsLineBorder = 5;
  NSNoTabsNoBorder = 6;
{$endif}

{$ifdef CLASSES}

type
  NSTabView = objcclass external (NSView)
  private
    _tabViewItems: id;
    _selectedTabViewItem: NSTabViewItem;
    _font: NSFont;
    _tabViewType: NSTabViewType;
    _allowTruncatedLabels: ObjCBOOL;
    _delegate: id;
    _tabViewUnusedBOOL1: ObjCBOOL;
    _drawsBackground: ObjCBOOL;
    _pressedTabViewItem: NSTabViewItem;
    _endTabWidth: NSInteger;
    _maxOverlap: NSInteger;
    _tabHeight: CGFloat;
    _tabViewItemWithKeyView: NSTabViewItem;
    _originalNextKeyView: NSView;
    {$if defined(__LP64__)}
    _tabViewMinimumSizeConstraint: NSLayoutConstraint;
    {$endif}
    __NSTabViewDelegateRespondTo: bitpacked record
      case byte of
        0: (_anonBitField___NSTabViewDelegateRespondTo0: cuint);
        1: (
          shouldSelectTabViewItem: 0..1;
          willSelectTabViewItem: 0..1;
          didSelectTabViewItem: 0..1;
          didChangeNumberOfTabViewItems: 0..1;
          reserved: 0..((1 shl 28)-1);
        );
      end;
    __NSTabViewFlags: bitpacked record
      case byte of
        0: (_anonBitField___NSTabViewFlags0: cuint);
        1: (
          needsLayout: 0..1;
          controlTint: 0..((1 shl 3)-1);
          controlSize: 0..((1 shl 2)-1);
          wiringNibConnections: 0..1;
          wiringInteriorLastKeyView: 0..1;
          originalNextKeyViewChanged: 0..1;
          liveResizeSkippedResetToolTips: 0..1;
          subviewsAddedForTabs: 0..1;
          allowsPropertyChange: 0..1;
          ownedByTabViewController: 0..1;
          reserved: 0..((1 shl 19)-1);
        );
      end;
    _focusedTabViewItem: NSTabViewItem;
    _tabViewUnused2: pointer;
  public
    procedure selectTabViewItem (tabViewItem: NSTabViewItem); message 'selectTabViewItem:';
    procedure selectTabViewItemAtIndex (index: NSInteger); message 'selectTabViewItemAtIndex:';
    procedure selectTabViewItemWithIdentifier (identifier: id); message 'selectTabViewItemWithIdentifier:';
    procedure takeSelectedTabViewItemFromSender (sender: id); message 'takeSelectedTabViewItemFromSender:';
    procedure selectFirstTabViewItem (sender: id); message 'selectFirstTabViewItem:';
    procedure selectLastTabViewItem (sender: id); message 'selectLastTabViewItem:';
    procedure selectNextTabViewItem (sender: id); message 'selectNextTabViewItem:';
    procedure selectPreviousTabViewItem (sender: id); message 'selectPreviousTabViewItem:';
    function selectedTabViewItem: NSTabViewItem; message 'selectedTabViewItem';
    procedure setFont(newValue: NSFont); message 'setFont:';
    function font: NSFont; message 'font';
    procedure setTabViewType(newValue: NSTabViewType); message 'setTabViewType:';
    function tabViewType: NSTabViewType; message 'tabViewType';
    function tabViewItems: NSArray; message 'tabViewItems';
    procedure setAllowsTruncatedLabels(newValue: ObjCBOOL); message 'setAllowsTruncatedLabels:';
    function allowsTruncatedLabels: ObjCBOOL; message 'allowsTruncatedLabels';
    function minimumSize: NSSize; message 'minimumSize';
    procedure setDrawsBackground(newValue: ObjCBOOL); message 'setDrawsBackground:';
    function drawsBackground: ObjCBOOL; message 'drawsBackground';
    procedure setControlTint(newValue: NSControlTint); message 'setControlTint:';
    function controlTint: NSControlTint; message 'controlTint';
    procedure setControlSize(newValue: NSControlSize); message 'setControlSize:';
    function controlSize: NSControlSize; message 'controlSize';
    procedure addTabViewItem (tabViewItem: NSTabViewItem); message 'addTabViewItem:';
    procedure insertTabViewItem_atIndex (tabViewItem: NSTabViewItem; index: NSInteger); message 'insertTabViewItem:atIndex:';
    procedure removeTabViewItem (tabViewItem: NSTabViewItem); message 'removeTabViewItem:';
    procedure setDelegate(newValue: NSTabViewDelegateProtocol); message 'setDelegate:';
    function delegate: NSTabViewDelegateProtocol; message 'delegate';
    function tabViewItemAtPoint (point: NSPoint): NSTabViewItem; message 'tabViewItemAtPoint:';
    function contentRect: NSRect; message 'contentRect';
    function numberOfTabViewItems: NSInteger; message 'numberOfTabViewItems';
    function indexOfTabViewItem (tabViewItem: NSTabViewItem): NSInteger; message 'indexOfTabViewItem:';
    function tabViewItemAtIndex (index: NSInteger): NSTabViewItem; message 'tabViewItemAtIndex:';
    function indexOfTabViewItemWithIdentifier (identifier: id): NSInteger; message 'indexOfTabViewItemWithIdentifier:';
  end;
{$endif}

{$ifdef PROTOCOLS}

type
  NSTabViewDelegateProtocol = objcprotocol external name 'NSTabViewDelegate' (NSObjectProtocol)
  optional
    function tabView_shouldSelectTabViewItem (tabView: NSTabView; tabViewItem: NSTabViewItem): ObjCBOOL; message 'tabView:shouldSelectTabViewItem:';
    procedure tabView_willSelectTabViewItem (tabView: NSTabView; tabViewItem: NSTabViewItem); message 'tabView:willSelectTabViewItem:';
    procedure tabView_didSelectTabViewItem (tabView: NSTabView; tabViewItem: NSTabViewItem); message 'tabView:didSelectTabViewItem:';
    procedure tabViewDidChangeNumberOfTabViewItems (tabView: NSTabView); message 'tabViewDidChangeNumberOfTabViewItems:';
  end;
{$endif}

