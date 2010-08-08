{ Parsed from Appkit.framework NSOutlineView.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Thu Oct 1 18:54:07 ICT 2009 }


{$ifdef TYPES}
{$ifndef NSOUTLINEVIEW_PAS_T}
{$define NSOUTLINEVIEW_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSOUTLINEVIEW_PAS_R}
{$define NSOUTLINEVIEW_PAS_R}

{ Records }
type
  __OvFlags = bitpacked record
{$ifdef fpc_big_endian}
    delegateWillDisplayCell: 0..1;
    delegateShouldEditTableColumn: 0..1;
    delegateShouldSelectItem: 0..1;
    delegateShouldSelectTableColumn: 0..1;
    delegateSelectionShouldChangeInOutlineView: 0..1;
    delegateShouldCollapseItem: 0..1;
    delegateShouldExpandItem: 0..1;
    autoresizesOutlineColumn: 0..1;
    autoSaveExpandItems: 0..1;
    enableExpandNotifications: 0..1;
    delegateWillDisplayOutlineCell: 0..1;
    removeChildInProgress: 0..1;
    selectionAdjustmentDisabled: 0..1;
    autoExpandFlashState: 0..1;
    compatCollapseForceClearsExpandState: 0..1;
    delegateHeightOfRowByItem: 0..1;
    delayRowEntryFreeDisabled: 0..1;
    numberOfRowsDataExpandEntered: 0..1;
    validDataSourceMethods: 0..1;
    reloadingData: 0..1;
    _reserved: 0..((1 shl 12)-1);
{$else}
    _reserved: 0..((1 shl 12)-1);
    reloadingData: 0..1;
    validDataSourceMethods: 0..1;
    numberOfRowsDataExpandEntered: 0..1;
    delayRowEntryFreeDisabled: 0..1;
    delegateHeightOfRowByItem: 0..1;
    compatCollapseForceClearsExpandState: 0..1;
    autoExpandFlashState: 0..1;
    selectionAdjustmentDisabled: 0..1;
    removeChildInProgress: 0..1;
    delegateWillDisplayOutlineCell: 0..1;
    enableExpandNotifications: 0..1;
    autoSaveExpandItems: 0..1;
    autoresizesOutlineColumn: 0..1;
    delegateShouldExpandItem: 0..1;
    delegateShouldCollapseItem: 0..1;
    delegateSelectionShouldChangeInOutlineView: 0..1;
    delegateShouldSelectTableColumn: 0..1;
    delegateShouldSelectItem: 0..1;
    delegateShouldEditTableColumn: 0..1;
    delegateWillDisplayCell: 0..1;
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
{$ifndef NSOUTLINEVIEW_PAS_S}
{$define NSOUTLINEVIEW_PAS_S}

{ External string constants }
var
  NSOutlineViewSelectionDidChangeNotification: NSString; external name '_NSOutlineViewSelectionDidChangeNotification';
  NSOutlineViewColumnDidMoveNotification: NSString; external name '_NSOutlineViewColumnDidMoveNotification';
  NSOutlineViewColumnDidResizeNotification: NSString; external name '_NSOutlineViewColumnDidResizeNotification';
  NSOutlineViewSelectionIsChangingNotification: NSString; external name '_NSOutlineViewSelectionIsChangingNotification';
  NSOutlineViewItemWillExpandNotification: NSString; external name '_NSOutlineViewItemWillExpandNotification';
  NSOutlineViewItemDidExpandNotification: NSString; external name '_NSOutlineViewItemDidExpandNotification';
  NSOutlineViewItemWillCollapseNotification: NSString; external name '_NSOutlineViewItemWillCollapseNotification';
  NSOutlineViewItemDidCollapseNotification: NSString; external name '_NSOutlineViewItemDidCollapseNotification';

{$endif}
{$endif}

{$ifdef FORWARD}
  NSOutlineView = objcclass;
  NSOutlineViewPointer = ^NSOutlineView;

{$endif}

{$ifdef CLASSES}
{$ifndef NSOUTLINEVIEW_PAS_C}
{$define NSOUTLINEVIEW_PAS_C}

{ NSOutlineView }
  NSOutlineView = objcclass(NSTableView)
  private
    __numberOfRows: NSInteger;
    __rowEntryTree: __NSOVRowEntry;
    __itemToEntryMap: CFMutableDictionaryRef;
    __unused2: NSInteger;
    __unused3: NSInteger;
    __unused1: NSInteger;
    __outlineTableColumn: NSTableColumn;
    __initedRows: Boolean;
    __indentationMarkerInCell: Boolean;
    __indentationPerLevel: NSInteger;
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
    __trackingRow: NSInteger;
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
    function itemAtRow(row: NSInteger): id; message 'itemAtRow:';
    function rowForItem(item: id): NSInteger; message 'rowForItem:';
    function levelForItem(item: id): NSInteger; message 'levelForItem:';
    function levelForRow(row: NSInteger): NSInteger; message 'levelForRow:';
    function isItemExpanded(item: id): Boolean; message 'isItemExpanded:';
    procedure setIndentationPerLevel(indentationPerLevel_: CGFloat); message 'setIndentationPerLevel:';
    function indentationPerLevel: CGFloat; message 'indentationPerLevel';
    procedure setIndentationMarkerFollowsCell(drawInCell: Boolean); message 'setIndentationMarkerFollowsCell:';
    function indentationMarkerFollowsCell: Boolean; message 'indentationMarkerFollowsCell';
    procedure setAutoresizesOutlineColumn(resize: Boolean); message 'setAutoresizesOutlineColumn:';
    function autoresizesOutlineColumn: Boolean; message 'autoresizesOutlineColumn';
    function frameOfOutlineCellAtRow(row: NSInteger): NSRect; message 'frameOfOutlineCellAtRow:';
    procedure setDropItem_dropChildIndex(item: id; index: NSInteger); message 'setDropItem:dropChildIndex:';
    function shouldCollapseAutoExpandedItemsForDeposited(deposited: Boolean): Boolean; message 'shouldCollapseAutoExpandedItemsForDeposited:';
    function autosaveExpandedItems: Boolean; message 'autosaveExpandedItems';
    procedure setAutosaveExpandedItems(save: Boolean); message 'setAutosaveExpandedItems:';
  end; external;

{$endif}
{$endif}
