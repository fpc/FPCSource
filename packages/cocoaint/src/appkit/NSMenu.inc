{ Parsed from Appkit.framework NSMenu.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Tue Sep 8 15:31:00 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSMENU_PAS_H}
{$define NSMENU_PAS_H}
type
  NSMenuPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSMENU_PAS_T}
{$define NSMENU_PAS_T}

{ CFString constants }
var
  NSMenuWillSendActionNotification: CFStringRef; external name '_NSMenuWillSendActionNotification';
  NSMenuDidSendActionNotification: CFStringRef; external name '_NSMenuDidSendActionNotification';
  NSMenuDidAddItemNotification: CFStringRef; external name '_NSMenuDidAddItemNotification';
  NSMenuDidRemoveItemNotification: CFStringRef; external name '_NSMenuDidRemoveItemNotification';
  NSMenuDidChangeItemNotification: CFStringRef; external name '_NSMenuDidChangeItemNotification';

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSMENU_PAS_R}
{$define NSMENU_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSMENU_PAS_F}
{$define NSMENU_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSMENU_PAS_T}
{$define NSMENU_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSMenu = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSMENU_PAS_C}
{$define NSMENU_PAS_C}

{ NSMenu }
  NSMenu = objcclass(NSObject, NSCopyingProtocol, NSCodingProtocol)
  private
    __supermenu: NSMenu;
    __title: NSString;
    __itemArray: id;
    __menuImpl: id;
    __mFlags: bitpacked record
        noAutoenable: 0..1;
        menuChangedMessagesDisabled: 0..1;
        needsMenuChangedMessage: 0..1;
        suppressAutoenable: 0..1;
        disabled: 0..1;
        ownedByPopUp: 0..1;
        delegateNeedsUpdate: 0..1;
        delegateUpdateItem: 0..1;
        delegateHasKeyEquiv: 0..1;
        delegateHasAltKeyEquiv: 0..1;
        keyEquivalentMapIsDirty: 0..1;
        excludeMarkColumn: 0..1;
        isContextualMenu: 0..1;
        RESERVED: 0..((1 shl 19)-1);
        
      end;
    __name: NSString;
    
  public
    class function alloc: NSMenu; message 'alloc';

    class procedure setMenuZone(var aZone: NSZone); message 'setMenuZone:';
    class function menuZone: NSZone; message 'menuZone';
    class procedure popUpContextMenu_withEvent_forView(menu: NSMenu; event: NSEvent; view: NSView); message 'popUpContextMenu:withEvent:forView:';
    class procedure popUpContextMenu_withEvent_forView_withFont(menu: NSMenu; event: NSEvent; view: NSView; font: NSFont); message 'popUpContextMenu:withEvent:forView:withFont:';
    class procedure setMenuBarVisible(visible: Boolean); message 'setMenuBarVisible:';
    class function menuBarVisible: Boolean; message 'menuBarVisible';
    function initWithTitle(aTitle: NSString): id; message 'initWithTitle:';
    procedure setTitle(aString: NSString); message 'setTitle:';
    function title: NSString; message 'title';
    procedure setSupermenu(supermenu_: NSMenu); message 'setSupermenu:';
    function supermenu: NSMenu; message 'supermenu';
    procedure insertItem_atIndex(newItem: NSMenuItem; index: clong); message 'insertItem:atIndex:';
    procedure addItem(newItem: NSMenuItem); message 'addItem:';
    function insertItemWithTitle_action_keyEquivalent_atIndex(aString: NSString; aSelector: SEL; charCode: NSString; index: clong): NSMenuItem; message 'insertItemWithTitle:action:keyEquivalent:atIndex:';
    function addItemWithTitle_action_keyEquivalent(aString: NSString; aSelector: SEL; charCode: NSString): NSMenuItem; message 'addItemWithTitle:action:keyEquivalent:';
    procedure removeItemAtIndex(index: clong); message 'removeItemAtIndex:';
    procedure removeItem(item: NSMenuItem); message 'removeItem:';
    procedure setSubmenu_forItem(aMenu: NSMenu; anItem: NSMenuItem); message 'setSubmenu:forItem:';
    function itemArray: NSArray; message 'itemArray';
    function numberOfItems: clong; message 'numberOfItems';
    function indexOfItem(index: NSMenuItem): clong; message 'indexOfItem:';
    function indexOfItemWithTitle(aTitle: NSString): clong; message 'indexOfItemWithTitle:';
    function indexOfItemWithTag(aTag: clong): clong; message 'indexOfItemWithTag:';
    function indexOfItemWithRepresentedObject(object_: id): clong; message 'indexOfItemWithRepresentedObject:';
    function indexOfItemWithSubmenu(submenu: NSMenu): clong; message 'indexOfItemWithSubmenu:';
    function indexOfItemWithTarget_andAction(target: id; actionSelector: SEL): clong; message 'indexOfItemWithTarget:andAction:';
    function itemAtIndex(index: clong): NSMenuItem; message 'itemAtIndex:';
    function itemWithTitle(aTitle: NSString): NSMenuItem; message 'itemWithTitle:';
    function itemWithTag(tag: clong): NSMenuItem; message 'itemWithTag:';
    procedure setAutoenablesItems(flag: Boolean); message 'setAutoenablesItems:';
    function autoenablesItems: Boolean; message 'autoenablesItems';
    function performKeyEquivalent(theEvent: NSEvent): Boolean; message 'performKeyEquivalent:';
    procedure update; message 'update';
    procedure setMenuChangedMessagesEnabled(flag: Boolean); message 'setMenuChangedMessagesEnabled:';
    function menuChangedMessagesEnabled: Boolean; message 'menuChangedMessagesEnabled';
    procedure itemChanged(item: NSMenuItem); message 'itemChanged:';
    procedure helpRequested(eventPtr: NSEventPointer); message 'helpRequested:';
    procedure setMenuRepresentation(menuRep: id); message 'setMenuRepresentation:';
    function menuRepresentation: id; message 'menuRepresentation';
    procedure setContextMenuRepresentation(menuRep: id); message 'setContextMenuRepresentation:';
    function contextMenuRepresentation: id; message 'contextMenuRepresentation';
    procedure setTearOffMenuRepresentation(menuRep: id); message 'setTearOffMenuRepresentation:';
    function tearOffMenuRepresentation: id; message 'tearOffMenuRepresentation';
    function isTornOff: Boolean; message 'isTornOff';
    function attachedMenu: NSMenu; message 'attachedMenu';
    function isAttached: Boolean; message 'isAttached';
    procedure sizeToFit; message 'sizeToFit';
    function locationForSubmenu(aSubmenu: NSMenu): NSPoint; message 'locationForSubmenu:';
    procedure performActionForItemAtIndex(index: clong); message 'performActionForItemAtIndex:';
    procedure setDelegate(anObject: id); message 'setDelegate:';
    function delegate: id; message 'delegate';
    function menuBarHeight: CGFloat; message 'menuBarHeight';
    procedure cancelTracking; message 'cancelTracking';
    function highlightedItem: NSMenuItem; message 'highlightedItem';
    procedure setShowsStateColumn(showsState: Boolean); message 'setShowsStateColumn:';
    function showsStateColumn: Boolean; message 'showsStateColumn';

    { Category: NSSubmenuAction }
    procedure submenuAction(sender: id); message 'submenuAction:';
  end; external;

{$endif}
{$endif}
