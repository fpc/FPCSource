{ Parsed from Appkit.framework NSOutlineView.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Tue Sep 8 15:31:01 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSOUTLINEVIEW_PAS_H}
{$define NSOUTLINEVIEW_PAS_H}
type
  NSOutlineViewPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSOUTLINEVIEW_PAS_T}
{$define NSOUTLINEVIEW_PAS_T}

{ CFString constants }
var
  NSOutlineViewSelectionDidChangeNotification: CFStringRef; external name '_NSOutlineViewSelectionDidChangeNotification';
  NSOutlineViewColumnDidMoveNotification: CFStringRef; external name '_NSOutlineViewColumnDidMoveNotification';
  NSOutlineViewColumnDidResizeNotification: CFStringRef; external name '_NSOutlineViewColumnDidResizeNotification';
  NSOutlineViewSelectionIsChangingNotification: CFStringRef; external name '_NSOutlineViewSelectionIsChangingNotification';
  NSOutlineViewItemWillExpandNotification: CFStringRef; external name '_NSOutlineViewItemWillExpandNotification';
  NSOutlineViewItemDidExpandNotification: CFStringRef; external name '_NSOutlineViewItemDidExpandNotification';
  NSOutlineViewItemWillCollapseNotification: CFStringRef; external name '_NSOutlineViewItemWillCollapseNotification';
  NSOutlineViewItemDidCollapseNotification: CFStringRef; external name '_NSOutlineViewItemDidCollapseNotification';

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSOUTLINEVIEW_PAS_R}
{$define NSOUTLINEVIEW_PAS_R}

{ Records }
type
  __OvFlags = record
{$ifdef fpc_big_endian}
    delegateWillDisplayCell: cuint;
    delegateShouldEditTableColumn: cuint;
    delegateShouldSelectItem: cuint;
    delegateShouldSelectTableColumn: cuint;
    delegateSelectionShouldChangeInOutlineView: cuint;
    delegateShouldCollapseItem: cuint;
    delegateShouldExpandItem: cuint;
    autoresizesOutlineColumn: cuint;
    autoSaveExpandItems: cuint;
    enableExpandNotifications: cuint;
    delegateWillDisplayOutlineCell: cuint;
    removeChildInProgress: cuint;
    selectionAdjustmentDisabled: cuint;
    autoExpandFlashState: cuint;
    compatCollapseForceClearsExpandState: cuint;
    delegateHeightOfRowByItem: cuint;
    delayRowEntryFreeDisabled: cuint;
    numberOfRowsDataExpandEntered: cuint;
    validDataSourceMethods: cuint;
    reloadingData: cuint;
    _reserved: cuint;
{$else}
    _reserved: cuint;
    reloadingData: cuint;
    validDataSourceMethods: cuint;
    numberOfRowsDataExpandEntered: cuint;
    delayRowEntryFreeDisabled: cuint;
    delegateHeightOfRowByItem: cuint;
    compatCollapseForceClearsExpandState: cuint;
    autoExpandFlashState: cuint;
    selectionAdjustmentDisabled: cuint;
    removeChildInProgress: cuint;
    delegateWillDisplayOutlineCell: cuint;
    enableExpandNotifications: cuint;
    autoSaveExpandItems: cuint;
    autoresizesOutlineColumn: cuint;
    delegateShouldExpandItem: cuint;
    delegateShouldCollapseItem: cuint;
    delegateSelectionShouldChangeInOutlineView: cuint;
    delegateShouldSelectTableColumn: cuint;
    delegateShouldSelectItem: cuint;
    delegateShouldEditTableColumn: cuint;
    delegateWillDisplayCell: cuint;
{$endif}
  end;
_OVFlags = __OvFlags;


{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSOUTLINEVIEW_PAS_F}
{$define NSOUTLINEVIEW_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSOUTLINEVIEW_PAS_T}
{$define NSOUTLINEVIEW_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSOutlineView = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSOUTLINEVIEW_PAS_C}
{$define NSOUTLINEVIEW_PAS_C}

{ NSOutlineView }
  NSOutlineView = objcclass(NSTableView)
  private
    __numberOfRows: clong;
    __rowEntryTree: __NSOVRowEntry;
    __itemToEntryMap: CFMutableDictionaryRef;
    __unused2: clong;
    __unused3: clong;
    __unused1: clong;
    __outlineTableColumn: NSTableColumn;
    __initedRows: Boolean;
    __indentationMarkerInCell: Boolean;
    __indentationPerLevel: clong;
    __outlineCell: NSButtonCell;
    __trackingOutlineFrame: NSRect;
    __tracker: NSMouseTracker;
    __unused4: id;
    __ovFlags: _OVFlags;
    __ovLock: id;
    __indentArray: clong;
    __originalWidth: clong;
    __expandSet: id;
    __expandSetToExpandItemsInto: id;
    __indentArraySize: clong;
    __trackingOutlineCell: NSButtonCell;
    __trackingRow: clong;
    __ovReserved: id;
    
  public
    class function alloc: NSOutlineView; message 'alloc';

    procedure setOutlineTableColumn(outlineTableColumn_: NSTableColumn); message 'setOutlineTableColumn:';
    function outlineTableColumn: NSTableColumn; message 'outlineTableColumn';
    function isExpandable(item: id): Boolean; message 'isExpandable:';
    procedure expandItem_expandChildren(item: id; expandChildren: Boolean); message 'expandItem:expandChildren:';
    procedure expandItem(item: id); message 'expandItem:';
    procedure collapseItem_collapseChildren(item: id; collapseChildren: Boolean); message 'collapseItem:collapseChildren:';
    procedure collapseItem(item: id); message 'collapseItem:';
    procedure reloadItem_reloadChildren(item: id; reloadChildren: Boolean); message 'reloadItem:reloadChildren:';
    procedure reloadItem(item: id); message 'reloadItem:';
    function parentForItem(item: id): id; message 'parentForItem:';
    function itemAtRow(row: clong): id; message 'itemAtRow:';
    function rowForItem(item: id): clong; message 'rowForItem:';
    function levelForItem(item: id): clong; message 'levelForItem:';
    function levelForRow(row: clong): clong; message 'levelForRow:';
    function isItemExpanded(item: id): Boolean; message 'isItemExpanded:';
    procedure setIndentationPerLevel(indentationPerLevel_: CGFloat); message 'setIndentationPerLevel:';
    function indentationPerLevel: CGFloat; message 'indentationPerLevel';
    procedure setIndentationMarkerFollowsCell(drawInCell: Boolean); message 'setIndentationMarkerFollowsCell:';
    function indentationMarkerFollowsCell: Boolean; message 'indentationMarkerFollowsCell';
    procedure setAutoresizesOutlineColumn(resize: Boolean); message 'setAutoresizesOutlineColumn:';
    function autoresizesOutlineColumn: Boolean; message 'autoresizesOutlineColumn';
    function frameOfOutlineCellAtRow(row: clong): NSRect; message 'frameOfOutlineCellAtRow:';
    procedure setDropItem_dropChildIndex(item: id; index: clong); message 'setDropItem:dropChildIndex:';
    function shouldCollapseAutoExpandedItemsForDeposited(deposited: Boolean): Boolean; message 'shouldCollapseAutoExpandedItemsForDeposited:';
    function autosaveExpandedItems: Boolean; message 'autosaveExpandedItems';
    procedure setAutosaveExpandedItems(save: Boolean); message 'setAutosaveExpandedItems:';
  end; external;

{$endif}
{$endif}
