{ Parsed from Appkit.framework NSMenuView.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Tue Sep 8 15:31:01 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSMENUVIEW_PAS_H}
{$define NSMENUVIEW_PAS_H}
type
  NSMenuViewPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSMENUVIEW_PAS_T}
{$define NSMENUVIEW_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSMENUVIEW_PAS_R}
{$define NSMENUVIEW_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSMENUVIEW_PAS_F}
{$define NSMENUVIEW_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSMENUVIEW_PAS_T}
{$define NSMENUVIEW_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSMenuView = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSMENUVIEW_PAS_C}
{$define NSMENUVIEW_PAS_C}

{ NSMenuView }
  NSMenuView = objcclass(NSView)
  private
    __menu: NSMenu;
    __cells: NSMutableArray;
    __stateImageWidth: CGFloat;
    __imageAndTitleWidth: CGFloat;
    __keyEquivalentWidth: CGFloat;
    __extents: CGFloat;
    __extentsCapacity: cuint;
    __highlightedItemIndex: cint;
    __submenuPopupTimer: Pointer;
    __attachedMenuView: NSMenuView;
    __scrollArrowHeight: CGFloat;
    __maxWinHeight: CGFloat;
    __font: NSFont;
    __minSize: NSSize;
    __horizontalEdgePad: CGFloat;
    _trackingState: Pointer;
    __scrollingView: id;
    __mvFlags: bitpacked record
        needsSizing: 0..1;
        releasingWindow: 0..1;
        isHorizontal: 0..1;
        disableSizing: 0..1;
        attachedSubmenuWhileMouseWasUp: 0..1;
        needsToCreateCells: 0..1;
        allowsTearOffs: 0..1;
        isTearOff: 0..1;
        keyEquivGlyphWidth: 0..((1 shl 16)-1);
        disableResize: 0..1;
        savedHODState: 0..1;
        drawCenter: 0..1;
        RESERVED: 0..((1 shl 5)-1);
      end;
    
  public
    class function alloc: NSMenuView; message 'alloc';

    class function menuBarHeight: CGFloat; message 'menuBarHeight';
    function initWithFrame(frame_: NSRect): id; message 'initWithFrame:';
    function initAsTearOff: id; message 'initAsTearOff';
    procedure setMenu(menu_: NSMenu); message 'setMenu:';
    function menu: NSMenu; message 'menu';
    procedure itemChanged(notification: NSNotification); message 'itemChanged:';
    procedure itemAdded(notification: NSNotification); message 'itemAdded:';
    procedure itemRemoved(notification: NSNotification); message 'itemRemoved:';
    procedure update; message 'update';
    procedure setHorizontal(flag: Boolean); message 'setHorizontal:';
    function isHorizontal: Boolean; message 'isHorizontal';
    procedure setFont(font_: NSFont); message 'setFont:';
    function font: NSFont; message 'font';
    function innerRect: NSRect; message 'innerRect';
    function rectOfItemAtIndex(index: clong): NSRect; message 'rectOfItemAtIndex:';
    function indexOfItemAtPoint(point: NSPoint): clong; message 'indexOfItemAtPoint:';
    procedure setNeedsDisplayForItemAtIndex(index: clong); message 'setNeedsDisplayForItemAtIndex:';
    procedure setHighlightedItemIndex(index: clong); message 'setHighlightedItemIndex:';
    function highlightedItemIndex: clong; message 'highlightedItemIndex';
    function stateImageOffset: CGFloat; message 'stateImageOffset';
    function stateImageWidth: CGFloat; message 'stateImageWidth';
    function imageAndTitleOffset: CGFloat; message 'imageAndTitleOffset';
    function imageAndTitleWidth: CGFloat; message 'imageAndTitleWidth';
    function keyEquivalentOffset: CGFloat; message 'keyEquivalentOffset';
    function keyEquivalentWidth: CGFloat; message 'keyEquivalentWidth';
    procedure setMenuItemCell_forItemAtIndex(cell: NSMenuItemCell; index: clong); message 'setMenuItemCell:forItemAtIndex:';
    function menuItemCellForItemAtIndex(index: clong): NSMenuItemCell; message 'menuItemCellForItemAtIndex:';
    function attachedMenuView: NSMenuView; message 'attachedMenuView';
    procedure setNeedsSizing(flag: Boolean); message 'setNeedsSizing:';
    function needsSizing: Boolean; message 'needsSizing';
    procedure sizeToFit; message 'sizeToFit';
    function attachedMenu: NSMenu; message 'attachedMenu';
    function isAttached: Boolean; message 'isAttached';
    function isTornOff: Boolean; message 'isTornOff';
    function locationForSubmenu(aSubmenu: NSMenu): NSPoint; message 'locationForSubmenu:';
    procedure setWindowFrameForAttachingToRect_onScreen_preferredEdge_popUpSelectedItem(screenRect: NSRect; screen: NSScreen; edge: NSRectEdge; selectedItemIndex: clong); message 'setWindowFrameForAttachingToRect:onScreen:preferredEdge:popUpSelectedItem:';
    procedure detachSubmenu; message 'detachSubmenu';
    procedure attachSubmenuForItemAtIndex(index: clong); message 'attachSubmenuForItemAtIndex:';
    procedure performActionWithHighlightingForItemAtIndex(index: clong); message 'performActionWithHighlightingForItemAtIndex:';
    function trackWithEvent(event: NSEvent): Boolean; message 'trackWithEvent:';
    function horizontalEdgePadding: CGFloat; message 'horizontalEdgePadding';
    procedure setHorizontalEdgePadding(pad: CGFloat); message 'setHorizontalEdgePadding:';
  end; external;

{$endif}
{$endif}
