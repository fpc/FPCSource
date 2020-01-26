{ Parsed from AppKit.framework NSMenu.h }


{$ifdef TYPES}
type
  NSMenuPtr = ^NSMenu;
  NSMenuDelegateProtocolPtr = ^NSMenuDelegateProtocol;
{$endif}

{$ifdef CLASSES}

type
  NSMenu = objcclass external (NSObject, NSCopyingProtocol, NSCodingProtocol)
  private
    _supermenu: NSMenu;
    _title: NSString;
    _itemArray: id;
    _extra: id;
    __mFlags: bitpacked record
      case byte of
        0: (_anonBitField___mFlags0: cuint);
        1: (
          noAutoenable: 0..1;
          inMain: 0..1;
          internalPerformAction: 0..1;
          condenseSeparators: 0..1;
          disabled: 0..1;
          ownedByPopUp: 0..1;
          delegateNeedsUpdate: 0..1;
          delegateUpdateItem: 0..1;
          delegateHasKeyEquiv: 0..1;
          delegateHasAltKeyEquiv: 0..1;
          excludeMarkColumn: 0..1;
          isContextualMenu: 0..1;
          cmPluginMode: 0..((1 shl 2)-1);
          invertedCMPluginTypes: 0..((1 shl 2)-1);
          allowsDifferentSelection: 0..1;
          noTopPadding: 0..1;
          noBottomPadding: 0..1;
          hasNCStyle: 0..1;
          RESERVED: 0..((1 shl 12)-1);
        );
      end;
    _uiid: NSString;
  public
    function initWithTitle (aTitle: NSString): instancetype; message 'initWithTitle:';
    procedure setTitle(newValue: NSString); message 'setTitle:';
    function title: NSString; message 'title';
    class procedure popUpContextMenu_withEvent_forView (menu: NSMenu; event: NSEvent; view: NSView); message 'popUpContextMenu:withEvent:forView:';
    class procedure popUpContextMenu_withEvent_forView_withFont (menu: NSMenu; event: NSEvent; view: NSView; font: NSFont); message 'popUpContextMenu:withEvent:forView:withFont:';
    function popUpMenuPositioningItem_atLocation_inView (item: NSMenuItem; location: NSPoint; view: NSView): ObjCBOOL; message 'popUpMenuPositioningItem:atLocation:inView:'; { available in 10_6 }
    class procedure setMenuBarVisible (visible: ObjCBOOL); message 'setMenuBarVisible:';
    class function menuBarVisible: ObjCBOOL; message 'menuBarVisible';
    procedure setSupermenu(newValue: NSMenu); message 'setSupermenu:';
    function supermenu: NSMenu; message 'supermenu';
    procedure insertItem_atIndex (newItem: NSMenuItem; index: NSInteger); message 'insertItem:atIndex:';
    procedure addItem (newItem: NSMenuItem); message 'addItem:';
    function insertItemWithTitle_action_keyEquivalent_atIndex (aString: NSString; aSelector: SEL; charCode: NSString; index: NSInteger): NSMenuItem; message 'insertItemWithTitle:action:keyEquivalent:atIndex:';
    function addItemWithTitle_action_keyEquivalent (aString: NSString; aSelector: SEL; charCode: NSString): NSMenuItem; message 'addItemWithTitle:action:keyEquivalent:';
    procedure removeItemAtIndex (index: NSInteger); message 'removeItemAtIndex:';
    procedure removeItem (item: NSMenuItem); message 'removeItem:';
    procedure setSubmenu_forItem (aMenu: NSMenu; anItem: NSMenuItem); message 'setSubmenu:forItem:';
    procedure removeAllItems; message 'removeAllItems'; { available in 10_6 }
    function itemArray: NSArray; message 'itemArray';
    function numberOfItems: NSInteger; message 'numberOfItems';
    function itemAtIndex (index: NSInteger): NSMenuItem; message 'itemAtIndex:';
    function indexOfItem (index: NSMenuItem): NSInteger; message 'indexOfItem:';
    function indexOfItemWithTitle (aTitle: NSString): NSInteger; message 'indexOfItemWithTitle:';
    function indexOfItemWithTag (aTag: NSInteger): NSInteger; message 'indexOfItemWithTag:';
    function indexOfItemWithRepresentedObject (object_: id): NSInteger; message 'indexOfItemWithRepresentedObject:';
    function indexOfItemWithSubmenu (submenu: NSMenu): NSInteger; message 'indexOfItemWithSubmenu:';
    function indexOfItemWithTarget_andAction (target: id; actionSelector: SEL): NSInteger; message 'indexOfItemWithTarget:andAction:';
    function itemWithTitle (aTitle: NSString): NSMenuItem; message 'itemWithTitle:';
    function itemWithTag (tag: NSInteger): NSMenuItem; message 'itemWithTag:';
    procedure setAutoenablesItems(newValue: ObjCBOOL); message 'setAutoenablesItems:';
    function autoenablesItems: ObjCBOOL; message 'autoenablesItems';
    procedure update; message 'update';
    function performKeyEquivalent (theEvent: NSEvent): ObjCBOOL; message 'performKeyEquivalent:';
    procedure itemChanged (item: NSMenuItem); message 'itemChanged:';
    procedure performActionForItemAtIndex (index: NSInteger); message 'performActionForItemAtIndex:';
    procedure setDelegate(newValue: NSMenuDelegateProtocol); message 'setDelegate:';
    function delegate: NSMenuDelegateProtocol; message 'delegate';
    function menuBarHeight: CGFloat; message 'menuBarHeight';
    procedure cancelTracking; message 'cancelTracking'; { available in 10_5 }
    procedure cancelTrackingWithoutAnimation; message 'cancelTrackingWithoutAnimation'; { available in 10_6 }
    function highlightedItem: NSMenuItem; message 'highlightedItem';
    procedure setMinimumWidth(newValue: CGFloat); message 'setMinimumWidth:';
    function minimumWidth: CGFloat; message 'minimumWidth';
    function size: NSSize; message 'size';
    procedure setFont(newValue: NSFont); message 'setFont:';
    function font: NSFont; message 'font';
    procedure setAllowsContextMenuPlugIns(newValue: ObjCBOOL); message 'setAllowsContextMenuPlugIns:';
    function allowsContextMenuPlugIns: ObjCBOOL; message 'allowsContextMenuPlugIns';
    procedure setShowsStateColumn(newValue: ObjCBOOL); message 'setShowsStateColumn:';
    function showsStateColumn: ObjCBOOL; message 'showsStateColumn';
    class function menuZone: NSZonePtr; message 'menuZone';
    procedure setMenuChangedMessagesEnabled(newValue: ObjCBOOL); message 'setMenuChangedMessagesEnabled:';
    function menuChangedMessagesEnabled: ObjCBOOL; message 'menuChangedMessagesEnabled';
    procedure helpRequested (eventPtr: NSEvent); message 'helpRequested:';
    function isTornOff: ObjCBOOL; message 'isTornOff';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
  end;


type
  NSSubmenuAction = objccategory external (NSMenu)
    procedure submenuAction (sender: id); message 'submenuAction:';
  end;

type
  NSMenuValidation = objccategory external (NSObject)
    function validateMenuItem (menuItem: NSMenuItem): ObjCBOOL; message 'validateMenuItem:';
  end;
{$endif}

{$ifdef PROTOCOLS}

type
  NSMenuDelegateProtocol = objcprotocol external name 'NSMenuDelegate' (NSObjectProtocol)
  optional
    procedure menuNeedsUpdate (menu: NSMenu); message 'menuNeedsUpdate:';
    function numberOfItemsInMenu (menu: NSMenu): NSInteger; message 'numberOfItemsInMenu:';
    function menu_updateItem_atIndex_shouldCancel (menu: NSMenu; item: NSMenuItem; index: NSInteger; shouldCancel: ObjCBOOL): ObjCBOOL; message 'menu:updateItem:atIndex:shouldCancel:';
    function menuHasKeyEquivalent_forEvent_target_action (menu: NSMenu; event: NSEvent; target: idPtr; action: SELPtr): ObjCBOOL; message 'menuHasKeyEquivalent:forEvent:target:action:';
    procedure menuWillOpen (menu: NSMenu); message 'menuWillOpen:'; { available in 10_5 }
    procedure menuDidClose (menu: NSMenu); message 'menuDidClose:'; { available in 10_5 }
    procedure menu_willHighlightItem (menu: NSMenu; item: NSMenuItem); message 'menu:willHighlightItem:'; { available in 10_5 }
    function confinementRectForMenu_onScreen (menu: NSMenu; screen: NSScreen): NSRect; message 'confinementRectForMenu:onScreen:'; { available in 10_6 }
  end;
{$endif}

{$ifdef TYPES}
type
  NSMenuProperties = NSUInteger;
  NSMenuPropertiesPtr = ^NSMenuProperties;

const
  NSMenuPropertyItemTitle = 1 shl 0;
  NSMenuPropertyItemAttributedTitle = 1 shl 1;
  NSMenuPropertyItemKeyEquivalent = 1 shl 2;
  NSMenuPropertyItemImage = 1 shl 3;
  NSMenuPropertyItemEnabled = 1 shl 4;
  NSMenuPropertyItemAccessibilityDescription = 1 shl 5;
{$endif}

{$ifdef CLASSES}

type
  NSMenuPropertiesToUpdate = objccategory external (NSMenu)
    function propertiesToUpdate: NSMenuProperties; message 'propertiesToUpdate';
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSMenuWillSendActionNotification: NSString; cvar; external;
  NSMenuDidSendActionNotification: NSString; cvar; external;
  NSMenuDidAddItemNotification: NSString; cvar; external;
  NSMenuDidRemoveItemNotification: NSString; cvar; external;
  NSMenuDidChangeItemNotification: NSString; cvar; external;
  NSMenuDidBeginTrackingNotification: NSString; cvar; external;
  NSMenuDidEndTrackingNotification: NSString; cvar; external;
{$endif}

{$ifdef CLASSES}

type
  NSMenu_NSDeprecated = objccategory external name 'NSDeprecated' (NSMenu)
    procedure setMenuRepresentation (menuRep: id); message 'setMenuRepresentation:'; deprecated 'in 10_0, 10_2';
    function menuRepresentation: id; message 'menuRepresentation'; deprecated 'in 10_0, 10_2';
    procedure setContextMenuRepresentation (menuRep: id); message 'setContextMenuRepresentation:'; deprecated 'in 10_0, 10_2';
    function contextMenuRepresentation: id; message 'contextMenuRepresentation'; deprecated 'in 10_0, 10_2';
    procedure setTearOffMenuRepresentation (menuRep: id); message 'setTearOffMenuRepresentation:'; deprecated 'in 10_0, 10_2';
    function tearOffMenuRepresentation: id; message 'tearOffMenuRepresentation'; deprecated 'in 10_0, 10_2';
    class procedure setMenuZone (aZone: NSZonePtr); message 'setMenuZone:'; deprecated 'in 10_0, 10_2';
    function attachedMenu: NSMenu; message 'attachedMenu'; deprecated 'in 10_0, 10_2';
    function isAttached: ObjCBOOL; message 'isAttached'; deprecated 'in 10_0, 10_2';
    procedure sizeToFit; message 'sizeToFit'; deprecated 'in 10_0, 10_2';
    function locationForSubmenu (aSubmenu: NSMenu): NSPoint; message 'locationForSubmenu:'; deprecated 'in 10_0, 10_2';
  end;
{$endif}

