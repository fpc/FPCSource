{ Parsed from Appkit.framework NSBrowser.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Mon Sep 28 15:49:53 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSBROWSER_PAS_H}
{$define NSBROWSER_PAS_H}
type
  NSBrowserPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSBROWSER_PAS_T}
{$define NSBROWSER_PAS_T}

{ Defines }
const
  NSAppKitVersionNumberWithContinuousScrollingBrowser = 680.0;
  NSAppKitVersionNumberWithColumnResizingBrowser = 685.0;

{ Constants }

const
  NSBrowserNoColumnResizing = 0;
  NSBrowserAutoColumnResizing = 1;
  NSBrowserUserColumnResizing = 2;

{ Types }
type
  NSBrowserColumnResizingType = NSUInteger;
  NSBrowserDropOperation = NSUInteger;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSBROWSER_PAS_R}
{$define NSBROWSER_PAS_R}

{ Records }
type
  __Brflags = bitpacked record
{$ifdef fpc_big_endian}
    allowsMultipleSelection: 0..1;
    allowsBranchSelection: 0..1;
    reuseColumns: 0..1;
    isTitled: 0..1;
    titleFromPrevious: 0..1;
    separateColumns: 0..1;
    delegateImplementsWillDisplayCell: 0..1;
    delegateSetsTitles: 0..1;
    delegateSelectsCellsByString: 0..1;
    delegateDoesNotCreateRowsInMatrix: 0..1;
    delegateValidatesColumns: 0..1;
    acceptArrowKeys: 0..1;
    dontDrawTitles: 0..1;
    sendActionOnArrowKeys: 0..1;
    prohibitEmptySel: 0..1;
    hasHorizontalScroller: 0..1;
    time: 0..1;
    allowsIncrementalSearching: 0..1;
    delegateSelectsCellsByRow: 0..1;
    disableCompositing: 0..((1 shl 6)-1);
    refusesFirstResponder: 0..1;
    acceptsFirstMouse: 0..1;
    actionNeedsToBeSent: 0..1;
    usesSmallSizeTitleFont: 0..1;
    usesSmallScrollers: 0..1;
    prefersAllColumnUserResizing: 0..1;
    firstVisibleCalculationDisabled: 0..1;
{$else}
    firstVisibleCalculationDisabled: 0..1;
    prefersAllColumnUserResizing: 0..1;
    usesSmallScrollers: 0..1;
    usesSmallSizeTitleFont: 0..1;
    actionNeedsToBeSent: 0..1;
    acceptsFirstMouse: 0..1;
    refusesFirstResponder: 0..1;
    disableCompositing: 0..((1 shl 6)-1);
    delegateSelectsCellsByRow: 0..1;
    allowsIncrementalSearching: 0..1;
    time: 0..1;
    hasHorizontalScroller: 0..1;
    prohibitEmptySel: 0..1;
    sendActionOnArrowKeys: 0..1;
    dontDrawTitles: 0..1;
    acceptArrowKeys: 0..1;
    delegateValidatesColumns: 0..1;
    delegateDoesNotCreateRowsInMatrix: 0..1;
    delegateSelectsCellsByString: 0..1;
    delegateSetsTitles: 0..1;
    delegateImplementsWillDisplayCell: 0..1;
    separateColumns: 0..1;
    titleFromPrevious: 0..1;
    isTitled: 0..1;
    reuseColumns: 0..1;
    allowsBranchSelection: 0..1;
    allowsMultipleSelection: 0..1;
{$endif}
  end;
_Brflags = __Brflags;


{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSBROWSER_PAS_F}
{$define NSBROWSER_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSBROWSER_PAS_S}
{$define NSBROWSER_PAS_S}

{ External string constants }
var
  NSBrowserColumnConfigurationDidChangeNotification: NSString; external name '_NSBrowserColumnConfigurationDidChangeNotification';

{$endif}
{$endif}

{$ifdef FORWARD}
  NSBrowser = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSBROWSER_PAS_C}
{$define NSBROWSER_PAS_C}

{ NSBrowser }
  NSBrowser = objcclass(NSControl)
  private
    __target: id;
    __action: SEL;
    __delegate: id;
    __doubleAction: SEL;
    __matrixClass: Pobjc_class;
    __cellPrototype: id;
    __columnSize: NSSize;
    __numberOfVisibleColumns: cshort;
    __minColumnWidth: cshort;
    __firstVisibleColumn: cshort;
    __maxVisibleColumns: cshort;
    __titles: NSMutableArray;
    __pathSeparator: NSString;
    __columns: NSMutableArray;
    __brAuxiliaryStorage: id;
    __firstColumnTitle: NSString;
    __scroller: NSScroller;
    __brflags: _Brflags;
    
  public
    class function alloc: NSBrowser; message 'alloc';

    class function cellClass: Pobjc_class; message 'cellClass';
    procedure loadColumnZero; message 'loadColumnZero';
    function isLoaded: Boolean; message 'isLoaded';
    procedure setDoubleAction(aSelector: SEL); message 'setDoubleAction:';
    function doubleAction: SEL; message 'doubleAction';
    procedure setMatrixClass(factoryId: Pobjc_class); message 'setMatrixClass:';
    function matrixClass: Pobjc_class; message 'matrixClass';
    procedure setCellClass(factoryId: Pobjc_class); message 'setCellClass:';
    procedure setCellPrototype(aCell: NSCell); message 'setCellPrototype:';
    function cellPrototype: id; message 'cellPrototype';
    procedure setDelegate(anObject: id); message 'setDelegate:';
    function delegate: id; message 'delegate';
    procedure setReusesColumns(flag: Boolean); message 'setReusesColumns:';
    function reusesColumns: Boolean; message 'reusesColumns';
    procedure setHasHorizontalScroller(flag: Boolean); message 'setHasHorizontalScroller:';
    function hasHorizontalScroller: Boolean; message 'hasHorizontalScroller';
    procedure setSeparatesColumns(flag: Boolean); message 'setSeparatesColumns:';
    function separatesColumns: Boolean; message 'separatesColumns';
    procedure setTitled(flag: Boolean); message 'setTitled:';
    function isTitled: Boolean; message 'isTitled';
    procedure setMinColumnWidth(columnWidth: CGFloat); message 'setMinColumnWidth:';
    function minColumnWidth: CGFloat; message 'minColumnWidth';
    procedure setMaxVisibleColumns(columnCount: NSInteger); message 'setMaxVisibleColumns:';
    function maxVisibleColumns: NSInteger; message 'maxVisibleColumns';
    procedure setAllowsMultipleSelection(flag: Boolean); message 'setAllowsMultipleSelection:';
    function allowsMultipleSelection: Boolean; message 'allowsMultipleSelection';
    procedure setAllowsBranchSelection(flag: Boolean); message 'setAllowsBranchSelection:';
    function allowsBranchSelection: Boolean; message 'allowsBranchSelection';
    procedure setAllowsEmptySelection(flag: Boolean); message 'setAllowsEmptySelection:';
    function allowsEmptySelection: Boolean; message 'allowsEmptySelection';
    procedure setTakesTitleFromPreviousColumn(flag: Boolean); message 'setTakesTitleFromPreviousColumn:';
    function takesTitleFromPreviousColumn: Boolean; message 'takesTitleFromPreviousColumn';
    procedure setAcceptsArrowKeys(flag: Boolean); message 'setAcceptsArrowKeys:';
    function acceptsArrowKeys: Boolean; message 'acceptsArrowKeys';
    procedure setSendsActionOnArrowKeys(flag: Boolean); message 'setSendsActionOnArrowKeys:';
    function sendsActionOnArrowKeys: Boolean; message 'sendsActionOnArrowKeys';
    procedure setTitle_ofColumn(aString: NSString; column: NSInteger); message 'setTitle:ofColumn:';
    function titleOfColumn(column: NSInteger): NSString; message 'titleOfColumn:';
    procedure setPathSeparator(newString: NSString); message 'setPathSeparator:';
    function pathSeparator: NSString; message 'pathSeparator';
    function setPath(path_: NSString): Boolean; message 'setPath:';
    function path: NSString; message 'path';
    function pathToColumn(column: NSInteger): NSString; message 'pathToColumn:';
    function selectedColumn: NSInteger; message 'selectedColumn';
    function selectedCell: id; message 'selectedCell';
    function selectedCellInColumn(column: NSInteger): id; message 'selectedCellInColumn:';
    function selectedCells: NSArray; message 'selectedCells';
    procedure selectRow_inColumn(row: NSInteger; column: NSInteger); message 'selectRow:inColumn:';
    function selectedRowInColumn(column: NSInteger): NSInteger; message 'selectedRowInColumn:';
    procedure selectRowIndexes_inColumn(indexes: NSIndexSet; column: NSInteger); message 'selectRowIndexes:inColumn:';
    function selectedRowIndexesInColumn(column: NSInteger): NSIndexSet; message 'selectedRowIndexesInColumn:';
    procedure reloadColumn(column: NSInteger); message 'reloadColumn:';
    procedure validateVisibleColumns; message 'validateVisibleColumns';
    procedure scrollColumnsRightBy(shiftAmount: NSInteger); message 'scrollColumnsRightBy:';
    procedure scrollColumnsLeftBy(shiftAmount: NSInteger); message 'scrollColumnsLeftBy:';
    procedure scrollColumnToVisible(column: NSInteger); message 'scrollColumnToVisible:';
    procedure setLastColumn(column: NSInteger); message 'setLastColumn:';
    function lastColumn: NSInteger; message 'lastColumn';
    procedure addColumn; message 'addColumn';
    function numberOfVisibleColumns: NSInteger; message 'numberOfVisibleColumns';
    function firstVisibleColumn: NSInteger; message 'firstVisibleColumn';
    function lastVisibleColumn: NSInteger; message 'lastVisibleColumn';
    function columnOfMatrix(matrix: NSMatrix): NSInteger; message 'columnOfMatrix:';
    function matrixInColumn(column: NSInteger): NSMatrix; message 'matrixInColumn:';
    function loadedCellAtRow_column(row: NSInteger; col: NSInteger): id; message 'loadedCellAtRow:column:';
    procedure selectAll(sender: id); message 'selectAll:';
    procedure tile; message 'tile';
    procedure doClick(sender: id); message 'doClick:';
    procedure doDoubleClick(sender: id); message 'doDoubleClick:';
    function sendAction: Boolean; message 'sendAction';
    function titleFrameOfColumn(column: NSInteger): NSRect; message 'titleFrameOfColumn:';
    procedure drawTitleOfColumn_inRect(column: NSInteger; aRect: NSRect); message 'drawTitleOfColumn:inRect:';
    function titleHeight: CGFloat; message 'titleHeight';
    function frameOfColumn(column: NSInteger): NSRect; message 'frameOfColumn:';
    function frameOfInsideOfColumn(column: NSInteger): NSRect; message 'frameOfInsideOfColumn:';
    function columnWidthForColumnContentWidth(columnContentWidth: CGFloat): CGFloat; message 'columnWidthForColumnContentWidth:';
    function columnContentWidthForColumnWidth(columnWidth: CGFloat): CGFloat; message 'columnContentWidthForColumnWidth:';
    procedure setColumnResizingType(columnResizingType_: NSBrowserColumnResizingType); message 'setColumnResizingType:';
    function columnResizingType: NSBrowserColumnResizingType; message 'columnResizingType';
    procedure setPrefersAllColumnUserResizing(prefersAllColumnResizing: Boolean); message 'setPrefersAllColumnUserResizing:';
    function prefersAllColumnUserResizing: Boolean; message 'prefersAllColumnUserResizing';
    procedure setWidth_ofColumn(columnWidth: CGFloat; columnIndex: NSInteger); message 'setWidth:ofColumn:';
    function widthOfColumn(column: NSInteger): CGFloat; message 'widthOfColumn:';
    procedure setColumnsAutosaveName(name: NSString); message 'setColumnsAutosaveName:';
    function columnsAutosaveName: NSString; message 'columnsAutosaveName';
    class procedure removeSavedColumnsWithAutosaveName(name: NSString); message 'removeSavedColumnsWithAutosaveName:';
    function canDragRowsWithIndexes_inColumn_withEvent(rowIndexes: NSIndexSet; column: NSInteger; event: NSEvent): Boolean; message 'canDragRowsWithIndexes:inColumn:withEvent:';
    function draggingImageForRowsWithIndexes_inColumn_withEvent_offset(rowIndexes: NSIndexSet; column: NSInteger; event: NSEvent; dragImageOffset: NSPointPointer): NSImage; message 'draggingImageForRowsWithIndexes:inColumn:withEvent:offset:';
    procedure setDraggingSourceOperationMask_forLocal(mask: NSDragOperation; isLocal: Boolean); message 'setDraggingSourceOperationMask:forLocal:';
    function allowsTypeSelect: Boolean; message 'allowsTypeSelect';
    procedure setAllowsTypeSelect(value: Boolean); message 'setAllowsTypeSelect:';
    procedure setBackgroundColor(color: NSColor); message 'setBackgroundColor:';
    function backgroundColor: NSColor; message 'backgroundColor';
    procedure displayColumn(column: NSInteger); message 'displayColumn:';
    procedure displayAllColumns; message 'displayAllColumns';
    procedure scrollViaScroller(sender: NSScroller); message 'scrollViaScroller:';
    procedure updateScroller; message 'updateScroller';
  end; external;

{$endif}
{$endif}
