{ Parsed from Appkit.framework NSToolbar.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Thu Oct 1 18:54:07 ICT 2009 }


{$ifdef TYPES}
{$ifndef NSTOOLBAR_PAS_T}
{$define NSTOOLBAR_PAS_T}

{ Types }
type
  NSToolbarDisplayMode = NSUInteger;
  NSToolbarSizeMode = NSUInteger;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSTOOLBAR_PAS_R}
{$define NSTOOLBAR_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSTOOLBAR_PAS_F}
{$define NSTOOLBAR_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSTOOLBAR_PAS_S}
{$define NSTOOLBAR_PAS_S}

{ External string constants }
var
  NSToolbarWillAddItemNotification: NSString; external name '_NSToolbarWillAddItemNotification';
  NSToolbarDidRemoveItemNotification: NSString; external name '_NSToolbarDidRemoveItemNotification';

{$endif}
{$endif}

{$ifdef FORWARD}
  NSToolbar = objcclass;
  NSToolbarPointer = ^NSToolbar;

{$endif}

{$ifdef CLASSES}
{$ifndef NSTOOLBAR_PAS_C}
{$define NSTOOLBAR_PAS_C}

{ NSToolbar }
  NSToolbar = objcclass(NSObject)
  private
    __toolbarIdentifier: NSString;
    __currentItems: NSMutableArray;
    __currentItemIdentifiers: NSMutableArray;
    __initPListDatabase: NSDictionary;
    __initPListTarget: id;
    __selectedItemIdentifier: NSString;
    __metrics: Pointer;
    __delegate: id;
    __window: NSWindow;
    __configPalette: id;
    __toolbarView: id;
    __syncPostEnabledCount: NSInteger;
    __tbFlags: bitpacked record
        allowsUserCustomization: 0..1;
        autosavesUsingIdentifier: 0..1;
        initialConfigurationDone: 0..1;
        shouldHideAfterCustomization: 0..1;
        delegateDefaultItemIdentifiers: 0..1;
        delegateAllowedItemIdentifiers: 0..1;
        delegateItemWithItemIdentifier: 0..1;
        delegateNotificationsEnabled: 0..1;
        prefersToBeShown: 0..1;
        loadItemsImmediately: 0..1;
        currentItemsContainsPlaceholder: 0..1;
        customizationPanelIsRunning: 0..1;
        usesCustomSheetWidth: 0..1;
        clickAndDragPerformsCustomization: 0..1;
        showsNoContextMenu: 0..1;
        firstMoveableItemIndex: 0..((1 shl 6)-1);
        keyboardLoopNeedsUpdating: 0..1;
        showHideDuringConfigurationChangeDisabled: 0..1;
        displayMode: 0..((1 shl 2)-1);
        sizeMode: 0..((1 shl 2)-1);
        doNotShowBaselineSeparator: 0..1;
        hideWithoutResizingWindowHint: 0..1;
        autovalidatesItemsDisabled: 0..1;
        inAutovalidation: 0..1;
        loadedMetrics: 0..1;
      end;
    __customizationSheetWidth: NSInteger;
    __tbReserved: id;
    
  public
    class function alloc: NSToolbar; message 'alloc';

    function initWithIdentifier(identifier_: NSString): id; message 'initWithIdentifier:';
    procedure insertItemWithItemIdentifier_atIndex(itemIdentifier: NSString; index: NSInteger); message 'insertItemWithItemIdentifier:atIndex:';
    procedure removeItemAtIndex(index: NSInteger); message 'removeItemAtIndex:';
    procedure setDelegate(delegate_: id); message 'setDelegate:';
    function delegate: id; message 'delegate';
    procedure setVisible(shown: Boolean); message 'setVisible:';
    function isVisible: Boolean; message 'isVisible';
    procedure runCustomizationPalette(sender: id); message 'runCustomizationPalette:';
    function customizationPaletteIsRunning: Boolean; message 'customizationPaletteIsRunning';
    procedure setDisplayMode(displayMode_: NSToolbarDisplayMode); message 'setDisplayMode:';
    function displayMode: NSToolbarDisplayMode; message 'displayMode';
    procedure setSelectedItemIdentifier(itemIdentifier: NSString); message 'setSelectedItemIdentifier:';
    function selectedItemIdentifier: NSString; message 'selectedItemIdentifier';
    procedure setSizeMode(sizeMode_: NSToolbarSizeMode); message 'setSizeMode:';
    function sizeMode: NSToolbarSizeMode; message 'sizeMode';
    procedure setShowsBaselineSeparator(flag: Boolean); message 'setShowsBaselineSeparator:';
    function showsBaselineSeparator: Boolean; message 'showsBaselineSeparator';
    procedure setAllowsUserCustomization(allowCustomization: Boolean); message 'setAllowsUserCustomization:';
    function allowsUserCustomization: Boolean; message 'allowsUserCustomization';
    function identifier: NSString; message 'identifier';
    function items: NSArray; message 'items';
    function visibleItems: NSArray; message 'visibleItems';
    procedure setAutosavesConfiguration(flag: Boolean); message 'setAutosavesConfiguration:';
    function autosavesConfiguration: Boolean; message 'autosavesConfiguration';
    procedure setConfigurationFromDictionary(configDict: NSDictionary); message 'setConfigurationFromDictionary:';
    function configurationDictionary: NSDictionary; message 'configurationDictionary';
    procedure validateVisibleItems; message 'validateVisibleItems';
  end; external;

{$endif}
{$endif}
