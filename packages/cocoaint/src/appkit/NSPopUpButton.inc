{ Parsed from Appkit.framework NSPopUpButton.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Tue Sep 8 15:31:01 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSPOPUPBUTTON_PAS_H}
{$define NSPOPUPBUTTON_PAS_H}
type
  NSPopUpButtonPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSPOPUPBUTTON_PAS_T}
{$define NSPOPUPBUTTON_PAS_T}

{ CFString constants }
var
  NSPopUpButtonWillPopUpNotification: CFStringRef; external name '_NSPopUpButtonWillPopUpNotification';

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSPOPUPBUTTON_PAS_R}
{$define NSPOPUPBUTTON_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSPOPUPBUTTON_PAS_F}
{$define NSPOPUPBUTTON_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSPOPUPBUTTON_PAS_T}
{$define NSPOPUPBUTTON_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSPopUpButton = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSPOPUPBUTTON_PAS_C}
{$define NSPOPUPBUTTON_PAS_C}

{ NSPopUpButton }
  NSPopUpButton = objcclass(NSButton)
  private
    __pbFlags: bitpacked record
        needsPullsDownFromTemplate: 0..1;
        RESERVED: 0..((1 shl 31)-1);
      end;
    {$ifdef cpu64}
    __popupReserved: id;
    {$endif}
    
  public
    class function alloc: NSPopUpButton; message 'alloc';

    function initWithFrame_pullsDown(buttonFrame: NSRect; flag: Boolean): id; message 'initWithFrame:pullsDown:';
    procedure setMenu(menu_: NSMenu); message 'setMenu:';
    function menu: NSMenu; message 'menu';
    procedure setPullsDown(flag: Boolean); message 'setPullsDown:';
    function pullsDown: Boolean; message 'pullsDown';
    procedure setAutoenablesItems(flag: Boolean); message 'setAutoenablesItems:';
    function autoenablesItems: Boolean; message 'autoenablesItems';
    procedure setPreferredEdge(edge: NSRectEdge); message 'setPreferredEdge:';
    function preferredEdge: NSRectEdge; message 'preferredEdge';
    procedure addItemWithTitle(title_: NSString); message 'addItemWithTitle:';
    procedure addItemsWithTitles(itemTitles_: NSArray); message 'addItemsWithTitles:';
    procedure insertItemWithTitle_atIndex(title_: NSString; index: clong); message 'insertItemWithTitle:atIndex:';
    procedure removeItemWithTitle(title_: NSString); message 'removeItemWithTitle:';
    procedure removeItemAtIndex(index: clong); message 'removeItemAtIndex:';
    procedure removeAllItems; message 'removeAllItems';
    function itemArray: NSArray; message 'itemArray';
    function numberOfItems: clong; message 'numberOfItems';
    function indexOfItem(item: NSMenuItem): clong; message 'indexOfItem:';
    function indexOfItemWithTitle(title_: NSString): clong; message 'indexOfItemWithTitle:';
    function indexOfItemWithTag(tag_: clong): clong; message 'indexOfItemWithTag:';
    function indexOfItemWithRepresentedObject(obj: id): clong; message 'indexOfItemWithRepresentedObject:';
    function indexOfItemWithTarget_andAction(target_: id; actionSelector: SEL): clong; message 'indexOfItemWithTarget:andAction:';
    function itemAtIndex(index: clong): NSMenuItem; message 'itemAtIndex:';
    function itemWithTitle(title_: NSString): NSMenuItem; message 'itemWithTitle:';
    function lastItem: NSMenuItem; message 'lastItem';
    procedure selectItem(item: NSMenuItem); message 'selectItem:';
    procedure selectItemAtIndex(index: clong); message 'selectItemAtIndex:';
    procedure selectItemWithTitle(title_: NSString); message 'selectItemWithTitle:';
    function selectItemWithTag(tag_: clong): Boolean; message 'selectItemWithTag:';
    procedure setTitle(aString: NSString); message 'setTitle:';
    function selectedItem: NSMenuItem; message 'selectedItem';
    function indexOfSelectedItem: clong; message 'indexOfSelectedItem';
    procedure synchronizeTitleAndSelectedItem; message 'synchronizeTitleAndSelectedItem';
    function itemTitleAtIndex(index: clong): NSString; message 'itemTitleAtIndex:';
    function itemTitles: NSArray; message 'itemTitles';
    function titleOfSelectedItem: NSString; message 'titleOfSelectedItem';
  end; external;

{$endif}
{$endif}
