{ Parsed from AppKit.framework NSPopUpButton.h }


{$ifdef TYPES}
type
  NSPopUpButtonPtr = ^NSPopUpButton;
{$endif}

{$ifdef CLASSES}

type
  NSPopUpButton = objcclass external (NSButton)
  private
    _pbFlags: bitpacked record
      case byte of
        0: (_anonBitField__pbFlags0: cuint);
        1: (
          needsPullsDownFromTemplate: 0..1;
          RESERVED: 0..((1 shl 31)-1);
        );
      end;
    {$if defined(__LP64__)}
    _popupReserved: id;
    {$endif}
  public
    function initWithFrame_pullsDown (buttonFrame: NSRect; flag: ObjCBOOL): instancetype; message 'initWithFrame:pullsDown:';
    procedure setMenu(newValue: NSMenu); message 'setMenu:';
    function menu: NSMenu; message 'menu';
    procedure setPullsDown(newValue: ObjCBOOL); message 'setPullsDown:';
    function pullsDown: ObjCBOOL; message 'pullsDown';
    procedure setAutoenablesItems(newValue: ObjCBOOL); message 'setAutoenablesItems:';
    function autoenablesItems: ObjCBOOL; message 'autoenablesItems';
    procedure setPreferredEdge(newValue: NSRectEdge); message 'setPreferredEdge:';
    function preferredEdge: NSRectEdge; message 'preferredEdge';
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
    function selectedTag: NSInteger; message 'selectedTag';
    procedure synchronizeTitleAndSelectedItem; message 'synchronizeTitleAndSelectedItem';
    function itemTitleAtIndex (index: NSInteger): NSString; message 'itemTitleAtIndex:';
    function itemTitles: NSArray; message 'itemTitles';
    function titleOfSelectedItem: NSString; message 'titleOfSelectedItem';
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSPopUpButtonWillPopUpNotification: NSString; cvar; external;
{$endif}

