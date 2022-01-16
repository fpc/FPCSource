{ Parsed from AppKit.framework NSPopUpButtonCell.h }


{$ifdef TYPES}
type
  NSPopUpButtonCellPtr = ^NSPopUpButtonCell;
{$endif}

{$ifdef TYPES}
type
  NSPopUpArrowPosition = NSUInteger;
  NSPopUpArrowPositionPtr = ^NSPopUpArrowPosition;

const
  NSPopUpNoArrow = 0;
  NSPopUpArrowAtCenter = 1;
  NSPopUpArrowAtBottom = 2;
{$endif}

{$ifdef CLASSES}

type
  NSPopUpButtonCell = objcclass external (NSMenuItemCell)
  private
    _menu: NSMenu;
    _selectedIndex: cint;
    __pbcFlags: bitpacked record
      case byte of
        0: (_anonBitField___pbcFlags0: cuint);
        1: (
          pullsDown: 0..1;
          preferredEdge: 0..((1 shl 3)-1);
          menuIsAttached: 0..1;
          usesItemFromMenu: 0..1;
          altersStateOfSelectedItem: 0..1;
          decoding: 0..1;
          arrowPosition: 0..((1 shl 2)-1);
          ignoreMenuLayout: 0..1;
          drawing: 0..1;
          menuShouldBeUniquedAgainstMain: 0..1;
          RESERVED: 0..((1 shl 19)-1);
        );
      end;
    {$if defined(__LP64__)}
    _popupReserved: id;
    {$endif}
  public
    function initTextCell_pullsDown (stringValue_: NSString; pullDown: ObjCBOOL): instancetype; message 'initTextCell:pullsDown:';
    procedure setMenu(newValue: NSMenu); message 'setMenu:';
    function menu: NSMenu; message 'menu';
    procedure setPullsDown(newValue: ObjCBOOL); message 'setPullsDown:';
    function pullsDown: ObjCBOOL; message 'pullsDown';
    procedure setAutoenablesItems(newValue: ObjCBOOL); message 'setAutoenablesItems:';
    function autoenablesItems: ObjCBOOL; message 'autoenablesItems';
    procedure setPreferredEdge(newValue: NSRectEdge); message 'setPreferredEdge:';
    function preferredEdge: NSRectEdge; message 'preferredEdge';
    procedure setUsesItemFromMenu(newValue: ObjCBOOL); message 'setUsesItemFromMenu:';
    function usesItemFromMenu: ObjCBOOL; message 'usesItemFromMenu';
    procedure setAltersStateOfSelectedItem(newValue: ObjCBOOL); message 'setAltersStateOfSelectedItem:';
    function altersStateOfSelectedItem: ObjCBOOL; message 'altersStateOfSelectedItem';
    procedure addItemWithTitle (title_: NSString); message 'addItemWithTitle:';
    procedure addItemsWithTitles (itemTitles: NSArray); message 'addItemsWithTitles:';
    procedure insertItemWithTitle_atIndex (title_: NSString; index: NSInteger); message 'insertItemWithTitle:atIndex:';
    procedure removeItemWithTitle (title_: NSString); message 'removeItemWithTitle:';
    procedure removeItemAtIndex (index: NSInteger); message 'removeItemAtIndex:';
    procedure removeAllItems; message 'removeAllItems';
    function itemArray: NSArray; message 'itemArray';
    function numberOfItems: NSInteger; message 'numberOfItems';
    function indexOfItem (item: NSMenuItem): NSInteger; message 'indexOfItem:';
    function indexOfItemWithTitle (title_: NSString): NSInteger; message 'indexOfItemWithTitle:';
    function indexOfItemWithTag (tag_: NSInteger): NSInteger; message 'indexOfItemWithTag:';
    function indexOfItemWithRepresentedObject (obj: id): NSInteger; message 'indexOfItemWithRepresentedObject:';
    function indexOfItemWithTarget_andAction (target_: id; actionSelector: SEL): NSInteger; message 'indexOfItemWithTarget:andAction:';
    function itemAtIndex (index: NSInteger): NSMenuItem; message 'itemAtIndex:';
    function itemWithTitle (title_: NSString): NSMenuItem; message 'itemWithTitle:';
    function lastItem: NSMenuItem; message 'lastItem';
    procedure selectItem (item: NSMenuItem); message 'selectItem:';
    procedure selectItemAtIndex (index: NSInteger); message 'selectItemAtIndex:';
    procedure selectItemWithTitle (title_: NSString); message 'selectItemWithTitle:';
    function selectItemWithTag (tag_: NSInteger): ObjCBOOL; message 'selectItemWithTag:';
    procedure setTitle (aString: NSString); message 'setTitle:';
    function selectedItem: NSMenuItem; message 'selectedItem';
    function indexOfSelectedItem: NSInteger; message 'indexOfSelectedItem';
    procedure synchronizeTitleAndSelectedItem; message 'synchronizeTitleAndSelectedItem';
    function itemTitleAtIndex (index: NSInteger): NSString; message 'itemTitleAtIndex:';
    function itemTitles: NSArray; message 'itemTitles';
    function titleOfSelectedItem: NSString; message 'titleOfSelectedItem';
    procedure attachPopUpWithFrame_inView (cellFrame: NSRect; controlView_: NSView); message 'attachPopUpWithFrame:inView:';
    procedure dismissPopUp; message 'dismissPopUp';
    procedure performClickWithFrame_inView (frame: NSRect; controlView_: NSView); message 'performClickWithFrame:inView:';
    procedure setArrowPosition(newValue: NSPopUpArrowPosition); message 'setArrowPosition:';
    function arrowPosition: NSPopUpArrowPosition; message 'arrowPosition';
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSPopUpButtonCellWillPopUpNotification: NSString; cvar; external;
{$endif}

