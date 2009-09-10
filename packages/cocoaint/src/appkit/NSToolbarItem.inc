{ Parsed from Appkit.framework NSToolbarItem.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Tue Sep 8 15:31:02 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSTOOLBARITEM_PAS_H}
{$define NSTOOLBARITEM_PAS_H}
type
  NSToolbarItemPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSTOOLBARITEM_PAS_T}
{$define NSTOOLBARITEM_PAS_T}

{ CFString constants }
var
  NSToolbarSeparatorItemIdentifier: CFStringRef; external name '_NSToolbarSeparatorItemIdentifier';
  NSToolbarSpaceItemIdentifier: CFStringRef; external name '_NSToolbarSpaceItemIdentifier';
  NSToolbarFlexibleSpaceItemIdentifier: CFStringRef; external name '_NSToolbarFlexibleSpaceItemIdentifier';
  NSToolbarShowColorsItemIdentifier: CFStringRef; external name '_NSToolbarShowColorsItemIdentifier';
  NSToolbarShowFontsItemIdentifier: CFStringRef; external name '_NSToolbarShowFontsItemIdentifier';
  NSToolbarCustomizeToolbarItemIdentifier: CFStringRef; external name '_NSToolbarCustomizeToolbarItemIdentifier';
  NSToolbarPrintItemIdentifier: CFStringRef; external name '_NSToolbarPrintItemIdentifier';

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSTOOLBARITEM_PAS_R}
{$define NSTOOLBARITEM_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSTOOLBARITEM_PAS_F}
{$define NSTOOLBARITEM_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSTOOLBARITEM_PAS_T}
{$define NSTOOLBARITEM_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSToolbarItem = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSTOOLBARITEM_PAS_C}
{$define NSTOOLBARITEM_PAS_C}

{ NSToolbarItem }
  NSToolbarItem = objcclass(NSObject, NSCopyingProtocol, NSValidatedUserInterfaceItemProtocol)
  private
    __toolbar: NSToolbar;
    __image: NSImage;
    __itemIdentifier: NSString;
    __label: NSString;
    __labelAlignment: NSTextAlignment;
    __paletteLabel: NSString;
    __toolTip: NSString;
    __menuItemRep: NSMenuItem;
    __tag: clong;
    __tbiFlags: bitpacked record
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
        hasNonDefaultPrioritySetting: 0..1;
        autovalidationDisabled: 0..1;
        tagHasBeenSet: 0..1;
        sizeHasBeenSet: 0..1;
        stateWasDisabledBeforeSheet: 0..1;
        RESERVED: 0..((1 shl 8)-1);
      end;
    __allPossibleLabelsToFit: NSArray;
    __itemViewer: id;
    __view: NSView;
    __minSize: NSSize;
    __maxSize: NSSize;
    {$ifdef cpu64}
    __toolbarItemReserved: id;
    {$endif}
    
  public
    class function alloc: NSToolbarItem; message 'alloc';

    function initWithItemIdentifier(itemIdentifier_: NSString): id; message 'initWithItemIdentifier:';
    function itemIdentifier: NSString; message 'itemIdentifier';
    function toolbar: NSToolbar; message 'toolbar';
    procedure setLabel(label__: NSString); message 'setLabel:';
    function label_: NSString; message 'label';
    procedure setPaletteLabel(paletteLabel_: NSString); message 'setPaletteLabel:';
    function paletteLabel: NSString; message 'paletteLabel';
    procedure setToolTip(toolTip_: NSString); message 'setToolTip:';
    function toolTip: NSString; message 'toolTip';
    procedure setMenuFormRepresentation(menuItem: NSMenuItem); message 'setMenuFormRepresentation:';
    function menuFormRepresentation: NSMenuItem; message 'menuFormRepresentation';
    procedure setTag(tag_: clong); message 'setTag:';
    function tag: clong; message 'tag';
    procedure setTarget(target_: id); message 'setTarget:';
    function target: id; message 'target';
    procedure setAction(action_: SEL); message 'setAction:';
    function action: SEL; message 'action';
    procedure setEnabled(enabled: Boolean); message 'setEnabled:';
    function isEnabled: Boolean; message 'isEnabled';
    procedure setImage(image_: NSImage); message 'setImage:';
    function image: NSImage; message 'image';
    procedure setView(view_: NSView); message 'setView:';
    function view: NSView; message 'view';
    procedure setMinSize(size: NSSize); message 'setMinSize:';
    function minSize: NSSize; message 'minSize';
    procedure setMaxSize(size: NSSize); message 'setMaxSize:';
    function maxSize: NSSize; message 'maxSize';
    procedure setVisibilityPriority(visibilityPriority_: clong); message 'setVisibilityPriority:';
    function visibilityPriority: clong; message 'visibilityPriority';
    procedure validate; message 'validate';
    procedure setAutovalidates(resistance: Boolean); message 'setAutovalidates:';
    function autovalidates: Boolean; message 'autovalidates';
    function allowsDuplicatesInToolbar: Boolean; message 'allowsDuplicatesInToolbar';
  end; external;

{$endif}
{$endif}
