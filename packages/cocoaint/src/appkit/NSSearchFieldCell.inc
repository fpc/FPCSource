{ Parsed from Appkit.framework NSSearchFieldCell.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Tue Sep 8 15:31:02 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSSEARCHFIELDCELL_PAS_H}
{$define NSSEARCHFIELDCELL_PAS_H}
type
  NSSearchFieldCellPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSSEARCHFIELDCELL_PAS_T}
{$define NSSEARCHFIELDCELL_PAS_T}

{ Defines }
const
  NSSearchFieldRecentsTitleMenuItemTag = 1000;
  NSSearchFieldRecentsMenuItemTag = 1001;
  NSSearchFieldClearRecentsMenuItemTag = 1002;
  NSSearchFieldNoRecentsMenuItemTag = 1003;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSSEARCHFIELDCELL_PAS_R}
{$define NSSEARCHFIELDCELL_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSSEARCHFIELDCELL_PAS_F}
{$define NSSEARCHFIELDCELL_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSSEARCHFIELDCELL_PAS_T}
{$define NSSEARCHFIELDCELL_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSSearchFieldCell = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSSEARCHFIELDCELL_PAS_C}
{$define NSSEARCHFIELDCELL_PAS_C}

{ NSSearchFieldCell }
  NSSearchFieldCell = objcclass(NSTextFieldCell)
  private
    __sfFlags: bitpacked record
        sendsWholeSearchString: 0..1;
        maximumRecents: 0..((1 shl 8)-1);
        cancelVisible: 0..1;
        drawSize: 0..((1 shl 2)-1);
        disableText: 0..1;
        menuTracking: 0..1;
        deferredUpdate: 0..1;
        sendsImmediately: 0..1;
        activeTimer: 0..1;
        
        reserved: 0..((1 shl 15)-1);
      end;
    __searchButtonCell: NSButtonCell;
    __cancelButtonCell: NSButtonCell;
    __searchMenuTemplate: NSMenu;
    __recentsAutosaveName: NSString;
    __recentSearches: NSMutableArray;
    __searchMenu: NSMenu;
    __partialStringTimer: NSTimer;
    __reserved1: cuint;
    __reserved2: cuint;
    __reserved3: cuint;
    __reserved4: cuint;
    
  public
    class function alloc: NSSearchFieldCell; message 'alloc';

    function searchButtonCell: NSButtonCell; message 'searchButtonCell';
    procedure setSearchButtonCell(cell: NSButtonCell); message 'setSearchButtonCell:';
    function cancelButtonCell: NSButtonCell; message 'cancelButtonCell';
    procedure setCancelButtonCell(cell: NSButtonCell); message 'setCancelButtonCell:';
    procedure resetSearchButtonCell; message 'resetSearchButtonCell';
    procedure resetCancelButtonCell; message 'resetCancelButtonCell';
    function searchTextRectForBounds(rect: NSRect): NSRect; message 'searchTextRectForBounds:';
    function searchButtonRectForBounds(rect: NSRect): NSRect; message 'searchButtonRectForBounds:';
    function cancelButtonRectForBounds(rect: NSRect): NSRect; message 'cancelButtonRectForBounds:';
    procedure setSearchMenuTemplate(menu_: NSMenu); message 'setSearchMenuTemplate:';
    function searchMenuTemplate: NSMenu; message 'searchMenuTemplate';
    procedure setSendsWholeSearchString(flag: Boolean); message 'setSendsWholeSearchString:';
    function sendsWholeSearchString: Boolean; message 'sendsWholeSearchString';
    procedure setMaximumRecents(maxRecents: clong); message 'setMaximumRecents:';
    function maximumRecents: clong; message 'maximumRecents';
    procedure setRecentSearches(searches: NSArray); message 'setRecentSearches:';
    function recentSearches: NSArray; message 'recentSearches';
    procedure setRecentsAutosaveName(string_: NSString); message 'setRecentsAutosaveName:';
    function recentsAutosaveName: NSString; message 'recentsAutosaveName';
    function sendsSearchStringImmediately: Boolean; message 'sendsSearchStringImmediately';
    procedure setSendsSearchStringImmediately(flag: Boolean); message 'setSendsSearchStringImmediately:';
  end; external;

{$endif}
{$endif}
