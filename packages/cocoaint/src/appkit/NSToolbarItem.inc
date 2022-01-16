{ Parsed from AppKit.framework NSToolbarItem.h }


{ Types from NSToolbarItem }
{$ifdef TYPES}

const
  NSToolbarItemVisibilityPriorityStandard = 0;
  NSToolbarItemVisibilityPriorityLow = -1000;
  NSToolbarItemVisibilityPriorityHigh = 1000;
  NSToolbarItemVisibilityPriorityUser = 2000;
{$endif}


{$ifdef TYPES}
type
  NSToolbarItemPtr = ^NSToolbarItem;
{$endif}

{$ifdef CLASSES}

type
  NSToolbarItem = objcclass external (NSObject, NSCopyingProtocol, NSValidatedUserInterfaceItemProtocol)
  private
    _toolbar: NSToolbar;
    _image: NSImage;
    _itemIdentifier: NSString;
    _label: NSString;
    _labelAlignment: NSTextAlignment;
    _paletteLabel: NSString;
    _toolTip: NSString;
    _menuItemRep: NSMenuItem;
    _tag: NSInteger;
    _tbiFlags: bitpacked record
      case byte of
        0: (_anonBitField__tbiFlags0: cuint);
        1: (
          viewRespondsToIsEnabled: 0..1;
          viewRespondsToSetEnabled: 0..1;
          viewRespondsToTag: 0..1;
          viewRespondsToSetTag: 0..1;
          viewRespondsToAction: 0..1;
          viewRespondsToSetAction: 0..1;
          viewRespondsToTarget: 0..1;
          viewRespondsToSetTarget: 0..1;
          viewRespondsToImage: 0..1;
          viewRespondsToSetImage: 0..1;
          isEnabled: 0..1;
          isUserRemovable: 0..1;
          menuHasBeenSet: 0..1;
          menuRepIsDefault: 0..1;
          viewHasBeenLoaded: 0..1;
          drawingForDragImage: 0..1;
          isCustomItemType: 0..1;
          hasValidatedAutoModeConfiguration: 0..1;
          useAutoModeConfiguration: 0..1;
          fromBaseLocalizedNib: 0..1;
          autovalidationDisabled: 0..1;
          tagHasBeenSet: 0..1;
          sizeHasBeenSet: 0..1;
          stateWasDisabledBeforeSheet: 0..1;
          wantsToBeCentered: 0..1;
          RESERVED: 0..((1 shl 7)-1);
        );
      end;
    _tbiReserved: id;
    _itemViewer: id;
    _view: NSView;
    _minSize: NSSize;
    _maxSize: NSSize;
    {$if defined(__LP64__)}
    _toolbarItemReserved: id;
    {$endif}
  public
    function initWithItemIdentifier (itemIdentifier: NSString): instancetype; message 'initWithItemIdentifier:'; { NS_DESIGNATED_INITIALIZER }
    function itemIdentifier: NSString; message 'itemIdentifier';
    function toolbar: NSToolbar; message 'toolbar';
    procedure setLabel(newValue: NSString); message 'setLabel:';
    function label_: NSString; message 'label';
    procedure setPaletteLabel(newValue: NSString); message 'setPaletteLabel:';
    function paletteLabel: NSString; message 'paletteLabel';
    procedure setToolTip(newValue: NSString); message 'setToolTip:';
    function toolTip: NSString; message 'toolTip';
    procedure setMenuFormRepresentation(newValue: NSMenuItem); message 'setMenuFormRepresentation:';
    function menuFormRepresentation: NSMenuItem; message 'menuFormRepresentation';
    procedure setTag(newValue: NSInteger); message 'setTag:';
    function tag: NSInteger; message 'tag';
    procedure setTarget(newValue: id); message 'setTarget:';
    function target: id; message 'target';
    procedure setAction(newValue: SEL); message 'setAction:';
    function action: SEL; message 'action';
    procedure setEnabled(newValue: ObjCBOOL); message 'setEnabled:';
    function isEnabled: ObjCBOOL; message 'isEnabled';
    procedure setImage(newValue: NSImage); message 'setImage:';
    function image: NSImage; message 'image';
    procedure setView(newValue: NSView); message 'setView:';
    function view: NSView; message 'view';
    procedure setMinSize(newValue: NSSize); message 'setMinSize:';
    function minSize: NSSize; message 'minSize';
    procedure setMaxSize(newValue: NSSize); message 'setMaxSize:';
    function maxSize: NSSize; message 'maxSize';
    procedure setVisibilityPriority(newValue: NSInteger); message 'setVisibilityPriority:';
    function visibilityPriority: NSInteger; message 'visibilityPriority';
    procedure validate; message 'validate';
    procedure setAutovalidates(newValue: ObjCBOOL); message 'setAutovalidates:';
    function autovalidates: ObjCBOOL; message 'autovalidates';
    function allowsDuplicatesInToolbar: ObjCBOOL; message 'allowsDuplicatesInToolbar';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
  end;


type
  NSToolbarItemValidation = objccategory external (NSObject)
    function validateToolbarItem (theItem: NSToolbarItem): ObjCBOOL; message 'validateToolbarItem:';
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSToolbarSeparatorItemIdentifier: NSString; cvar; external;
  NSToolbarSpaceItemIdentifier: NSString; cvar; external;
  NSToolbarFlexibleSpaceItemIdentifier: NSString; cvar; external;
  NSToolbarShowColorsItemIdentifier: NSString; cvar; external;
  NSToolbarShowFontsItemIdentifier: NSString; cvar; external;
  NSToolbarCustomizeToolbarItemIdentifier: NSString; cvar; external;
  NSToolbarPrintItemIdentifier: NSString; cvar; external;
{$endif}

