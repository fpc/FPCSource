{ Parsed from Appkit.framework NSBrowser.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Tue Sep 8 15:31:01 ICT 2009 }

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
  NSBrowserColumnResizingType = culong;
  NSBrowserDropOperation = culong;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSBROWSER_PAS_R}
{$define NSBROWSER_PAS_R}

{ Records }
type
  __Brflags = record
{$ifdef fpc_big_endian}
    allowsMultipleSelection: cuint;
    allowsBranchSelection: cuint;
    reuseColumns: cuint;
    isTitled: cuint;
    titleFromPrevious: cuint;
    separateColumns: cuint;
    delegateImplementsWillDisplayCell: cuint;
    delegateSetsTitles: cuint;
    delegateSelectsCellsByString: cuint;
    delegateDoesNotCreateRowsInMatrix: cuint;
    delegateValidatesColumns: cuint;
    acceptArrowKeys: cuint;
    dontDrawTitles: cuint;
    sendActionOnArrowKeys: cuint;
    prohibitEmptySel: cuint;
    hasHorizontalScroller: cuint;
    time: cuint;
    allowsIncrementalSearching: cuint;
    delegateSelectsCellsByRow: cuint;
    disableCompositing: cuint;
    refusesFirstResponder: cuint;
    acceptsFirstMouse: cuint;
    actionNeedsToBeSent: cuint;
    usesSmallSizeTitleFont: cuint;
    usesSmallScrollers: cuint;
    prefersAllColumnUserResizing: cuint;
    firstVisibleCalculationDisabled: cuint;
{$else}
    firstVisibleCalculationDisabled: cuint;
    prefersAllColumnUserResizing: cuint;
    usesSmallScrollers: cuint;
    usesSmallSizeTitleFont: cuint;
    actionNeedsToBeSent: cuint;
    acceptsFirstMouse: cuint;
    refusesFirstResponder: cuint;
    disableCompositing: cuint;
    delegateSelectsCellsByRow: cuint;
    allowsIncrementalSearching: cuint;
    time: cuint;
    hasHorizontalScroller: cuint;
    prohibitEmptySel: cuint;
    sendActionOnArrowKeys: cuint;
    dontDrawTitles: cuint;
    acceptArrowKeys: cuint;
    delegateValidatesColumns: cuint;
    delegateDoesNotCreateRowsInMatrix: cuint;
    delegateSelectsCellsByString: cuint;
    delegateSetsTitles: cuint;
    delegateImplementsWillDisplayCell: cuint;
    separateColumns: cuint;
    titleFromPrevious: cuint;
    isTitled: cuint;
    reuseColumns: cuint;
    allowsBranchSelection: cuint;
    allowsMultipleSelection: cuint;
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
{$ifndef NSBROWSER_PAS_T}
{$define NSBROWSER_PAS_T}

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
    procedure setMaxVisibleColumns(columnCount: clong); message 'setMaxVisibleColumns:';
    function maxVisibleColumns: clong; message 'maxVisibleColumns';
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
    procedure setTitle_ofColumn(aString: NSString; column: clong); message 'setTitle:ofColumn:';
    function titleOfColumn(column: clong): NSString; message 'titleOfColumn:';
    procedure setPathSeparator(newString: NSString); message 'setPathSeparator:';
    function pathSeparator: NSString; message 'pathSeparator';
    function setPath(path_: NSString): Boolean; message 'setPath:';
    function path: NSString; message 'path';
    function pathToColumn(column: clong): NSString; message 'pathToColumn:';
    function selectedColumn: clong; message 'selectedColumn';
    function selectedCell: id; message 'selectedCell';
    function selectedCellInColumn(column: clong): id; message 'selectedCellInColumn:';
    function selectedCells: NSArray; message 'selectedCells';
    procedure selectRow_inColumn(row: clong; column: clong); message 'selectRow:inColumn:';
    function selectedRowInColumn(column: clong): clong; message 'selectedRowInColumn:';
    procedure selectRowIndexes_inColumn(indexes: NSIndexSet; column: clong); message 'selectRowIndexes:inColumn:';
    function selectedRowIndexesInColumn(column: clong): NSIndexSet; message 'selectedRowIndexesInColumn:';
    procedure reloadColumn(column: clong); message 'reloadColumn:';
    procedure validateVisibleColumns; message 'validateVisibleColumns';
    procedure scrollColumnsRightBy(shiftAmount: clong); message 'scrollColumnsRightBy:';
    procedure scrollColumnsLeftBy(shiftAmount: clong); message 'scrollColumnsLeftBy:';
    procedure scrollColumnToVisible(column: clong); message 'scrollColumnToVisible:';
    procedure setLastColumn(column: clong); message 'setLastColumn:';
    function lastColumn: clong; message 'lastColumn';
    procedure addColumn; message 'addColumn';
    function numberOfVisibleColumns: clong; message 'numberOfVisibleColumns';
    function firstVisibleColumn: clong; message 'firstVisibleColumn';
    function lastVisibleColumn: clong; message 'lastVisibleColumn';
    function columnOfMatrix(matrix: NSMatrix): clong; message 'columnOfMatrix:';
    function matrixInColumn(column: clong): NSMatrix; message 'matrixInColumn:';
    function loadedCellAtRow_column(row: clong; col: clong): id; message 'loadedCellAtRow:column:';
    procedure selectAll(sender: id); message 'selectAll:';
    procedure tile; message 'tile';
    procedure doClick(sender: id); message 'doClick:';
    procedure doDoubleClick(sender: id); message 'doDoubleClick:';
    function sendAction: Boolean; message 'sendAction';
    function titleFrameOfColumn(column: clong): NSRect; message 'titleFrameOfColumn:';
    procedure drawTitleOfColumn_inRect(column: clong; aRect: NSRect); message 'drawTitleOfColumn:inRect:';
    function titleHeight: CGFloat; message 'titleHeight';
    function frameOfColumn(column: clong): NSRect; message 'frameOfColumn:';
    function frameOfInsideOfColumn(column: clong): NSRect; message 'frameOfInsideOfColumn:';
    function columnWidthForColumnContentWidth(columnContentWidth: CGFloat): CGFloat; message 'columnWidthForColumnContentWidth:';
    function columnContentWidthForColumnWidth(columnWidth: CGFloat): CGFloat; message 'columnContentWidthForColumnWidth:';
    procedure setColumnResizingType(columnResizingType_: NSBrowserColumnResizingType); message 'setColumnResizingType:';
    function columnResizingType: NSBrowserColumnResizingType; message 'columnResizingType';
    procedure setPrefersAllColumnUserResizing(prefersAllColumnResizing: Boolean); message 'setPrefersAllColumnUserResizing:';
    function prefersAllColumnUserResizing: Boolean; message 'prefersAllColumnUserResizing';
    procedure setWidth_ofColumn(columnWidth: CGFloat; columnIndex: clong); message 'setWidth:ofColumn:';
    function widthOfColumn(column: clong): CGFloat; message 'widthOfColumn:';
    procedure setColumnsAutosaveName(name: NSString); message 'setColumnsAutosaveName:';
    function columnsAutosaveName: NSString; message 'columnsAutosaveName';
    class procedure removeSavedColumnsWithAutosaveName(name: NSString); message 'removeSavedColumnsWithAutosaveName:';
    function canDragRowsWithIndexes_inColumn_withEvent(rowIndexes: NSIndexSet; column: clong; event: NSEvent): Boolean; message 'canDragRowsWithIndexes:inColumn:withEvent:';
    function draggingImageForRowsWithIndexes_inColumn_withEvent_offset(rowIndexes: NSIndexSet; column: clong; event: NSEvent; dragImageOffset: NSPointPointer): NSImage; message 'draggingImageForRowsWithIndexes:inColumn:withEvent:offset:';
    procedure setDraggingSourceOperationMask_forLocal(mask: NSDragOperation; isLocal: Boolean); message 'setDraggingSourceOperationMask:forLocal:';
    function allowsTypeSelect: Boolean; message 'allowsTypeSelect';
    procedure setAllowsTypeSelect(value: Boolean); message 'setAllowsTypeSelect:';
    procedure setBackgroundColor(color: NSColor); message 'setBackgroundColor:';
    function backgroundColor: NSColor; message 'backgroundColor';
    procedure displayColumn(column: clong); message 'displayColumn:';
    procedure displayAllColumns; message 'displayAllColumns';
    procedure scrollViaScroller(sender: NSScroller); message 'scrollViaScroller:';
    procedure updateScroller; message 'updateScroller';
  end; external;

{$endif}
{$endif}
