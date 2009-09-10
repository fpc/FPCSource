{ Parsed from Appkit.framework NSTableView.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Tue Sep 8 15:31:01 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSTABLEVIEW_PAS_H}
{$define NSTABLEVIEW_PAS_H}
type
  NSTableViewPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSTABLEVIEW_PAS_T}
{$define NSTABLEVIEW_PAS_T}

{ Types }
type
  NSTableViewDropOperation = culong;
  NSTableViewColumnAutoresizingStyle = culong;
  NSTableViewSelectionHighlightStyle = clong;

{ Constants }

const
  NSTableViewNoColumnAutoresizing = 0;
  NSTableViewUniformColumnAutoresizingStyle = 0;
  NSTableViewLastColumnOnlyAutoresizingStyle = 1;
  NSTableViewFirstColumnOnlyAutoresizingStyle = 2;

const
  NSTableViewGridNone = 0;
  NSTableViewSolidVerticalGridLineMask = 1  shl  0;
  NSTableViewSolidHorizontalGridLineMask = 1  shl  1;

const
  NSTableViewSelectionHighlightStyleRegular = 0;
  NSTableViewSelectionHighlightStyleSourceList = 1;

{ CFString constants }
var
  NSTableViewSelectionDidChangeNotification: CFStringRef; external name '_NSTableViewSelectionDidChangeNotification';
  NSTableViewColumnDidMoveNotification: CFStringRef; external name '_NSTableViewColumnDidMoveNotification';
  NSTableViewColumnDidResizeNotification: CFStringRef; external name '_NSTableViewColumnDidResizeNotification';
  NSTableViewSelectionIsChangingNotification: CFStringRef; external name '_NSTableViewSelectionIsChangingNotification';

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSTABLEVIEW_PAS_R}
{$define NSTABLEVIEW_PAS_R}

{ Records }
type
  __TvFlags = record
{$ifdef fpc_big_endian}
    allowsColumnReordering: cuint;
    allowsColumnResizing: cuint;
    oldDrawsGridFlag: cuint;
    allowsEmptySelection: cuint;
    allowsMultipleSelection: cuint;
    allowsColumnSelection: cuint;
    selectionType: cuint;
    changingLayout: cuint;
    compareWidthWithSuperview: cuint;
    delegateWillDisplayCell: cuint;
    delegateShouldEditTableColumn: cuint;
    delegateShouldSelectRow: cuint;
    delegateShouldSelectTableColumn: cuint;
    delegateSelectionShouldChangeInTableView: cuint;
    oldAutoresizesAllColumnsToFit: cuint;
    dataSourceSetObjectValue: cuint;
    selectionPostingDisableCount: cuint;
    movedPostingDisableCount: cuint;
    refusesFirstResponder: cuint;
{$else}
    refusesFirstResponder: cuint;
    movedPostingDisableCount: cuint;
    selectionPostingDisableCount: cuint;
    dataSourceSetObjectValue: cuint;
    oldAutoresizesAllColumnsToFit: cuint;
    delegateSelectionShouldChangeInTableView: cuint;
    delegateShouldSelectTableColumn: cuint;
    delegateShouldSelectRow: cuint;
    delegateShouldEditTableColumn: cuint;
    delegateWillDisplayCell: cuint;
    compareWidthWithSuperview: cuint;
    changingLayout: cuint;
    selectionType: cuint;
    allowsColumnSelection: cuint;
    allowsMultipleSelection: cuint;
    allowsEmptySelection: cuint;
    oldDrawsGridFlag: cuint;
    allowsColumnResizing: cuint;
    allowsColumnReordering: cuint;
{$endif}
  end;
_TvFlags = __TvFlags;


{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSTABLEVIEW_PAS_F}
{$define NSTABLEVIEW_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSTABLEVIEW_PAS_T}
{$define NSTABLEVIEW_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSTableView = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSTABLEVIEW_PAS_C}
{$define NSTABLEVIEW_PAS_C}

{ NSTableView }
  NSTableView = objcclass(NSControl, NSUserInterfaceValidationsProtocol)
  private
    __headerView: NSTableHeaderView;
    __cornerView: NSView;
    __tableColumns: NSMutableArray;
    __editingCell: NSCell;
    __delegate: id;
    __dataSource: id;
    __intercellSpacing: NSSize;
    __rowHeight: CGFloat;
    __lastSelectedColumn: clong;
    __lastSelectedRow: clong;
    __editingRow: clong;
    __editingColumn: clong;
    __selectedColumns: NSMutableIndexSet;
    __selectedRows: NSMutableIndexSet;
    __bodyDragImage: NSImage;
    __backgroundColor: NSColor;
    __gridColor: NSColor;
    __dragYPos: CGFloat;
    __target: id;
    __action: SEL;
    __doubleAction: SEL;
    __rectOfLastColumn: NSRect;
    __lastCachedRectColumn: clong;
    __rectOfLastRow: NSRect;
    __lastCachedRectRow: clong;
    __tvFlags: _TvFlags;
    __reserved: id;
    
  public
    class function alloc: NSTableView; message 'alloc';

    procedure setDataSource(aSource: id); message 'setDataSource:';
    function dataSource: id; message 'dataSource';
    procedure setDelegate(delegate_: id); message 'setDelegate:';
    function delegate: id; message 'delegate';
    procedure setHeaderView(headerView_: NSTableHeaderView); message 'setHeaderView:';
    function headerView: NSTableHeaderView; message 'headerView';
    procedure setCornerView(cornerView_: NSView); message 'setCornerView:';
    function cornerView: NSView; message 'cornerView';
    procedure setAllowsColumnReordering(flag: Boolean); message 'setAllowsColumnReordering:';
    function allowsColumnReordering: Boolean; message 'allowsColumnReordering';
    procedure setAllowsColumnResizing(flag: Boolean); message 'setAllowsColumnResizing:';
    function allowsColumnResizing: Boolean; message 'allowsColumnResizing';
    procedure setColumnAutoresizingStyle(style: NSTableViewColumnAutoresizingStyle); message 'setColumnAutoresizingStyle:';
    function columnAutoresizingStyle: NSTableViewColumnAutoresizingStyle; message 'columnAutoresizingStyle';
    procedure setGridStyleMask(gridType: culong); message 'setGridStyleMask:';
    function gridStyleMask: culong; message 'gridStyleMask';
    procedure setIntercellSpacing(aSize: NSSize); message 'setIntercellSpacing:';
    function intercellSpacing: NSSize; message 'intercellSpacing';
    procedure setUsesAlternatingRowBackgroundColors(useAlternatingRowColors: Boolean); message 'setUsesAlternatingRowBackgroundColors:';
    function usesAlternatingRowBackgroundColors: Boolean; message 'usesAlternatingRowBackgroundColors';
    procedure setBackgroundColor(color: NSColor); message 'setBackgroundColor:';
    function backgroundColor: NSColor; message 'backgroundColor';
    procedure setGridColor(color: NSColor); message 'setGridColor:';
    function gridColor: NSColor; message 'gridColor';
    procedure setRowHeight(rowHeight_: CGFloat); message 'setRowHeight:';
    function rowHeight: CGFloat; message 'rowHeight';
    procedure noteHeightOfRowsWithIndexesChanged(indexSet: NSIndexSet); message 'noteHeightOfRowsWithIndexesChanged:';
    function tableColumns: NSArray; message 'tableColumns';
    function numberOfColumns: clong; message 'numberOfColumns';
    function numberOfRows: clong; message 'numberOfRows';
    procedure addTableColumn(column: NSTableColumn); message 'addTableColumn:';
    procedure removeTableColumn(column: NSTableColumn); message 'removeTableColumn:';
    function columnWithIdentifier(identifier: id): clong; message 'columnWithIdentifier:';
    function tableColumnWithIdentifier(identifier: id): NSTableColumn; message 'tableColumnWithIdentifier:';
    procedure tile; message 'tile';
    procedure sizeToFit; message 'sizeToFit';
    procedure sizeLastColumnToFit; message 'sizeLastColumnToFit';
    procedure scrollRowToVisible(row: clong); message 'scrollRowToVisible:';
    procedure scrollColumnToVisible(column: clong); message 'scrollColumnToVisible:';
    procedure moveColumn_toColumn(column: clong; newIndex: clong); message 'moveColumn:toColumn:';
    procedure reloadData; message 'reloadData';
    procedure noteNumberOfRowsChanged; message 'noteNumberOfRowsChanged';
    function editedColumn: clong; message 'editedColumn';
    function editedRow: clong; message 'editedRow';
    function clickedColumn: clong; message 'clickedColumn';
    function clickedRow: clong; message 'clickedRow';
    procedure setDoubleAction(aSelector: SEL); message 'setDoubleAction:';
    function doubleAction: SEL; message 'doubleAction';
    procedure setSortDescriptors(array_: NSArray); message 'setSortDescriptors:';
    function sortDescriptors: NSArray; message 'sortDescriptors';
    procedure setIndicatorImage_inTableColumn(anImage: NSImage; tc: NSTableColumn); message 'setIndicatorImage:inTableColumn:';
    function indicatorImageInTableColumn(tc: NSTableColumn): NSImage; message 'indicatorImageInTableColumn:';
    procedure setHighlightedTableColumn(tc: NSTableColumn); message 'setHighlightedTableColumn:';
    function highlightedTableColumn: NSTableColumn; message 'highlightedTableColumn';
    procedure setVerticalMotionCanBeginDrag(flag: Boolean); message 'setVerticalMotionCanBeginDrag:';
    function verticalMotionCanBeginDrag: Boolean; message 'verticalMotionCanBeginDrag';
    function canDragRowsWithIndexes_atPoint(rowIndexes: NSIndexSet; mouseDownPoint: NSPoint): Boolean; message 'canDragRowsWithIndexes:atPoint:';
    function dragImageForRowsWithIndexes_tableColumns_event_offset(dragRows: NSIndexSet; tableColumns_: NSArray; dragEvent: NSEvent; dragImageOffset: NSPointPointer): NSImage; message 'dragImageForRowsWithIndexes:tableColumns:event:offset:';
    procedure setDraggingSourceOperationMask_forLocal(mask: NSDragOperation; isLocal: Boolean); message 'setDraggingSourceOperationMask:forLocal:';
    procedure setDropRow_dropOperation(row: clong; op: NSTableViewDropOperation); message 'setDropRow:dropOperation:';
    procedure setAllowsMultipleSelection(flag: Boolean); message 'setAllowsMultipleSelection:';
    function allowsMultipleSelection: Boolean; message 'allowsMultipleSelection';
    procedure setAllowsEmptySelection(flag: Boolean); message 'setAllowsEmptySelection:';
    function allowsEmptySelection: Boolean; message 'allowsEmptySelection';
    procedure setAllowsColumnSelection(flag: Boolean); message 'setAllowsColumnSelection:';
    function allowsColumnSelection: Boolean; message 'allowsColumnSelection';
    procedure selectAll(sender: id); message 'selectAll:';
    procedure deselectAll(sender: id); message 'deselectAll:';
    procedure selectColumnIndexes_byExtendingSelection(indexes: NSIndexSet; extend: Boolean); message 'selectColumnIndexes:byExtendingSelection:';
    procedure selectRowIndexes_byExtendingSelection(indexes: NSIndexSet; extend: Boolean); message 'selectRowIndexes:byExtendingSelection:';
    function selectedColumnIndexes: NSIndexSet; message 'selectedColumnIndexes';
    function selectedRowIndexes: NSIndexSet; message 'selectedRowIndexes';
    procedure deselectColumn(column: clong); message 'deselectColumn:';
    procedure deselectRow(row: clong); message 'deselectRow:';
    function selectedColumn: clong; message 'selectedColumn';
    function selectedRow: clong; message 'selectedRow';
    function isColumnSelected(column: clong): Boolean; message 'isColumnSelected:';
    function isRowSelected(row: clong): Boolean; message 'isRowSelected:';
    function numberOfSelectedColumns: clong; message 'numberOfSelectedColumns';
    function numberOfSelectedRows: clong; message 'numberOfSelectedRows';
    function allowsTypeSelect: Boolean; message 'allowsTypeSelect';
    procedure setAllowsTypeSelect(value: Boolean); message 'setAllowsTypeSelect:';
    function selectionHighlightStyle: NSTableViewSelectionHighlightStyle; message 'selectionHighlightStyle';
    procedure setSelectionHighlightStyle(selectionHighlightStyle_: NSTableViewSelectionHighlightStyle); message 'setSelectionHighlightStyle:';
    function rectOfColumn(column: clong): NSRect; message 'rectOfColumn:';
    function rectOfRow(row: clong): NSRect; message 'rectOfRow:';
    function columnIndexesInRect(rect: NSRect): NSIndexSet; message 'columnIndexesInRect:';
    function rowsInRect(rect: NSRect): NSRange; message 'rowsInRect:';
    function columnAtPoint(point: NSPoint): clong; message 'columnAtPoint:';
    function rowAtPoint(point: NSPoint): clong; message 'rowAtPoint:';
    function frameOfCellAtColumn_row(column: clong; row: clong): NSRect; message 'frameOfCellAtColumn:row:';
    function preparedCellAtColumn_row(column: clong; row: clong): NSCell; message 'preparedCellAtColumn:row:';
    function textShouldBeginEditing(textObject: NSText): Boolean; message 'textShouldBeginEditing:';
    function textShouldEndEditing(textObject: NSText): Boolean; message 'textShouldEndEditing:';
    procedure textDidBeginEditing(notification: NSNotification); message 'textDidBeginEditing:';
    procedure textDidEndEditing(notification: NSNotification); message 'textDidEndEditing:';
    procedure textDidChange(notification: NSNotification); message 'textDidChange:';
    procedure setAutosaveName(name: NSString); message 'setAutosaveName:';
    function autosaveName: NSString; message 'autosaveName';
    procedure setAutosaveTableColumns(save: Boolean); message 'setAutosaveTableColumns:';
    function autosaveTableColumns: Boolean; message 'autosaveTableColumns';
    procedure editColumn_row_withEvent_select(column: clong; row: clong; theEvent: NSEvent; select: Boolean); message 'editColumn:row:withEvent:select:';
    procedure drawRow_clipRect(row: clong; clipRect: NSRect); message 'drawRow:clipRect:';
    procedure highlightSelectionInClipRect(clipRect: NSRect); message 'highlightSelectionInClipRect:';
    procedure drawGridInClipRect(clipRect: NSRect); message 'drawGridInClipRect:';
    procedure drawBackgroundInClipRect(clipRect: NSRect); message 'drawBackgroundInClipRect:';

    { Category: NSDeprecated }
    procedure setDrawsGrid(flag: Boolean); message 'setDrawsGrid:';
    function drawsGrid: Boolean; message 'drawsGrid';
    procedure selectColumn_byExtendingSelection(column: clong; extend: Boolean); message 'selectColumn:byExtendingSelection:';
    procedure selectRow_byExtendingSelection(row: clong; extend: Boolean); message 'selectRow:byExtendingSelection:';
    function selectedColumnEnumerator: NSEnumerator; message 'selectedColumnEnumerator';
    function selectedRowEnumerator: NSEnumerator; message 'selectedRowEnumerator';
    function dragImageForRows_event_dragImageOffset(dragRows: NSArray; dragEvent: NSEvent; dragImageOffset: NSPointPointer): NSImage; message 'dragImageForRows:event:dragImageOffset:';
    procedure setAutoresizesAllColumnsToFit(flag: Boolean); message 'setAutoresizesAllColumnsToFit:';
    function autoresizesAllColumnsToFit: Boolean; message 'autoresizesAllColumnsToFit';
    function columnsInRect(rect: NSRect): NSRange; message 'columnsInRect:';
  end; external;

{$endif}
{$endif}
