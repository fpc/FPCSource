{ Parsed from Appkit.framework NSMatrix.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Tue Sep 8 15:31:01 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSMATRIX_PAS_H}
{$define NSMATRIX_PAS_H}
type
  NSMatrixPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSMATRIX_PAS_T}
{$define NSMATRIX_PAS_T}

{ Callbacks }
type
  NSMatrixCompare = function (param1: id; param2: id; param3: Pointer): NSInteger; cdecl;

{ Constants }

const
  NSRadioModeMatrix = 0;
  NSHighlightModeMatrix = 1;
  NSListModeMatrix = 2;
  NSTrackModeMatrix = 3;

{ Types }
type
  NSMatrixMode = culong;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSMATRIX_PAS_R}
{$define NSMATRIX_PAS_R}

{ Records }
type
  __MFlags = record
{$ifdef fpc_big_endian}
    highlightMode: cuint;
    radioMode: cuint;
    listMode: cuint;
    allowEmptySel: cuint;
    autoscroll: cuint;
    selectionByRect: cuint;
    drawsCellBackground: cuint;
    drawsBackground: cuint;
    autosizeCells: cuint;
    drawingAncestor: cuint;
    tabKeyTraversesCells: cuint;
    tabKeyTraversesCellsExplicitlySet: cuint;
    allowsIncrementalSearching: cuint;
    currentlySelectingCell: cuint;
    onlySetKeyCell: cuint;
    changingSelectionWithKeyboard: cuint;
    dontScroll: cuint;
    refusesFirstResponder: cuint;
    useSimpleTrackingMode: cuint;
    checkForSimpleTrackingMode: cuint;
    liveResizeImageCacheingEnabled: cuint;
    hasCachedSubclassIsSafeForLiveResize: cuint;
    subclassIsSafeForLiveResize: cuint;
    tmpAllowNonVisibleCellsToBecomeFirstResponder: cuint;
    needsRedrawBeforeFirstLiveResizeCache: cuint;
    browserOptimizationsEnabled: cuint;
    reservedMatrix: cuint;
{$else}
    reservedMatrix: cuint;
    browserOptimizationsEnabled: cuint;
    needsRedrawBeforeFirstLiveResizeCache: cuint;
    tmpAllowNonVisibleCellsToBecomeFirstResponder: cuint;
    subclassIsSafeForLiveResize: cuint;
    hasCachedSubclassIsSafeForLiveResize: cuint;
    liveResizeImageCacheingEnabled: cuint;
    checkForSimpleTrackingMode: cuint;
    useSimpleTrackingMode: cuint;
    refusesFirstResponder: cuint;
    dontScroll: cuint;
    changingSelectionWithKeyboard: cuint;
    onlySetKeyCell: cuint;
    currentlySelectingCell: cuint;
    allowsIncrementalSearching: cuint;
    tabKeyTraversesCellsExplicitlySet: cuint;
    tabKeyTraversesCells: cuint;
    drawingAncestor: cuint;
    autosizeCells: cuint;
    drawsBackground: cuint;
    drawsCellBackground: cuint;
    selectionByRect: cuint;
    autoscroll: cuint;
    allowEmptySel: cuint;
    listMode: cuint;
    radioMode: cuint;
    highlightMode: cuint;
{$endif}
  end;
_MFlags = __MFlags;


{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSMATRIX_PAS_F}
{$define NSMATRIX_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSMATRIX_PAS_T}
{$define NSMATRIX_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSMatrix = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSMATRIX_PAS_C}
{$define NSMATRIX_PAS_C}

{ NSMatrix }
  NSMatrix = objcclass(NSControl, NSUserInterfaceValidationsProtocol)
  private
    __target: id;
    __action: SEL;
    __doubleAction: SEL;
    __errorAction: SEL;
    __delegate: id;
    __selectedCell: id;
    __selectedRow: clong;
    __selectedCol: clong;
    __numRows: clong;
    __numCols: clong;
    __cellSize: NSSize;
    __intercell: NSSize;
    __font: id;
    __protoCell: id;
    __cellClass: id;
    __backgroundColor: NSColor;
    __private: id;
    __cells: NSMutableArray;
    __mFlags: _MFlags;
    
  public
    class function alloc: NSMatrix; message 'alloc';

    function initWithFrame(frameRect: NSRect): id; message 'initWithFrame:';
    function initWithFrame_mode_prototype_numberOfRows_numberOfColumns(frameRect: NSRect; aMode: NSMatrixMode; aCell: NSCell; rowsHigh: clong; colsWide: clong): id; message 'initWithFrame:mode:prototype:numberOfRows:numberOfColumns:';
    function initWithFrame_mode_cellClass_numberOfRows_numberOfColumns(frameRect: NSRect; aMode: NSMatrixMode; factoryId: Pobjc_class; rowsHigh: clong; colsWide: clong): id; message 'initWithFrame:mode:cellClass:numberOfRows:numberOfColumns:';
    procedure setCellClass(factoryId: Pobjc_class); message 'setCellClass:';
    function cellClass: Pobjc_class; message 'cellClass';
    function prototype: id; message 'prototype';
    procedure setPrototype(aCell: NSCell); message 'setPrototype:';
    function makeCellAtRow_column(row: clong; col: clong): NSCell; message 'makeCellAtRow:column:';
    function mode: NSMatrixMode; message 'mode';
    procedure setMode(aMode: NSMatrixMode); message 'setMode:';
    procedure setAllowsEmptySelection(flag: Boolean); message 'setAllowsEmptySelection:';
    function allowsEmptySelection: Boolean; message 'allowsEmptySelection';
    procedure sendAction_to_forAllCells(aSelector: SEL; anObject: id; flag: Boolean); message 'sendAction:to:forAllCells:';
    function cells: NSArray; message 'cells';
    procedure sortUsingSelector(comparator: SEL); message 'sortUsingSelector:';
    procedure sortUsingFunction_context(compare: NSMatrixCompare; context: Pointer); message 'sortUsingFunction:context:';
    function selectedCell: id; message 'selectedCell';
    function selectedCells: NSArray; message 'selectedCells';
    function selectedRow: clong; message 'selectedRow';
    function selectedColumn: clong; message 'selectedColumn';
    procedure setSelectionByRect(flag: Boolean); message 'setSelectionByRect:';
    function isSelectionByRect: Boolean; message 'isSelectionByRect';
    procedure setSelectionFrom_to_anchor_highlight(startPos: clong; endPos: clong; anchorPos: clong; lit: Boolean); message 'setSelectionFrom:to:anchor:highlight:';
    procedure deselectSelectedCell; message 'deselectSelectedCell';
    procedure deselectAllCells; message 'deselectAllCells';
    procedure selectCellAtRow_column(row: clong; col: clong); message 'selectCellAtRow:column:';
    procedure selectAll(sender: id); message 'selectAll:';
    function selectCellWithTag(anInt: clong): Boolean; message 'selectCellWithTag:';
    function cellSize: NSSize; message 'cellSize';
    procedure setCellSize(aSize: NSSize); message 'setCellSize:';
    function intercellSpacing: NSSize; message 'intercellSpacing';
    procedure setIntercellSpacing(aSize: NSSize); message 'setIntercellSpacing:';
    procedure setScrollable(flag: Boolean); message 'setScrollable:';
    procedure setBackgroundColor(color: NSColor); message 'setBackgroundColor:';
    function backgroundColor: NSColor; message 'backgroundColor';
    procedure setCellBackgroundColor(color: NSColor); message 'setCellBackgroundColor:';
    function cellBackgroundColor: NSColor; message 'cellBackgroundColor';
    procedure setDrawsCellBackground(flag: Boolean); message 'setDrawsCellBackground:';
    function drawsCellBackground: Boolean; message 'drawsCellBackground';
    procedure setDrawsBackground(flag: Boolean); message 'setDrawsBackground:';
    function drawsBackground: Boolean; message 'drawsBackground';
    procedure setState_atRow_column(value: clong; row: clong; col: clong); message 'setState:atRow:column:';
    procedure getNumberOfRows_columns(var rowCount: clong; var colCount: clong); message 'getNumberOfRows:columns:';
    function numberOfRows: clong; message 'numberOfRows';
    function numberOfColumns: clong; message 'numberOfColumns';
    function cellAtRow_column(row: clong; col: clong): id; message 'cellAtRow:column:';
    function cellFrameAtRow_column(row: clong; col: clong): NSRect; message 'cellFrameAtRow:column:';
    function getRow_column_ofCell(var row: clong; var col: clong; aCell: NSCell): Boolean; message 'getRow:column:ofCell:';
    function getRow_column_forPoint(var row: clong; var col: clong; aPoint: NSPoint): Boolean; message 'getRow:column:forPoint:';
    procedure renewRows_columns(newRows: clong; newCols: clong); message 'renewRows:columns:';
    procedure putCell_atRow_column(newCell: NSCell; row: clong; col: clong); message 'putCell:atRow:column:';
    procedure addRow; message 'addRow';
    procedure addRowWithCells(newCells: NSArray); message 'addRowWithCells:';
    procedure insertRow(row: clong); message 'insertRow:';
    procedure insertRow_withCells(row: clong; newCells: NSArray); message 'insertRow:withCells:';
    procedure removeRow(row: clong); message 'removeRow:';
    procedure addColumn; message 'addColumn';
    procedure addColumnWithCells(newCells: NSArray); message 'addColumnWithCells:';
    procedure insertColumn(column: clong); message 'insertColumn:';
    procedure insertColumn_withCells(column: clong; newCells: NSArray); message 'insertColumn:withCells:';
    procedure removeColumn(col: clong); message 'removeColumn:';
    function cellWithTag(anInt: clong): id; message 'cellWithTag:';
    function doubleAction: SEL; message 'doubleAction';
    procedure setDoubleAction(aSelector: SEL); message 'setDoubleAction:';
    procedure setAutosizesCells(flag: Boolean); message 'setAutosizesCells:';
    function autosizesCells: Boolean; message 'autosizesCells';
    procedure sizeToCells; message 'sizeToCells';
    procedure setValidateSize(flag: Boolean); message 'setValidateSize:';
    procedure drawCellAtRow_column(row: clong; col: clong); message 'drawCellAtRow:column:';
    procedure highlightCell_atRow_column(flag: Boolean; row: clong; col: clong); message 'highlightCell:atRow:column:';
    procedure setAutoscroll(flag: Boolean); message 'setAutoscroll:';
    function isAutoscroll: Boolean; message 'isAutoscroll';
    procedure scrollCellToVisibleAtRow_column(row: clong; col: clong); message 'scrollCellToVisibleAtRow:column:';
    function mouseDownFlags: clong; message 'mouseDownFlags';
    procedure mouseDown(theEvent: NSEvent); message 'mouseDown:';
    function performKeyEquivalent(theEvent: NSEvent): Boolean; message 'performKeyEquivalent:';
    function sendAction: Boolean; message 'sendAction';
    procedure sendDoubleAction; message 'sendDoubleAction';
    function delegate: id; message 'delegate';
    procedure setDelegate(anObject: id); message 'setDelegate:';
    function textShouldBeginEditing(textObject: NSText): Boolean; message 'textShouldBeginEditing:';
    function textShouldEndEditing(textObject: NSText): Boolean; message 'textShouldEndEditing:';
    procedure textDidBeginEditing(notification: NSNotification); message 'textDidBeginEditing:';
    procedure textDidEndEditing(notification: NSNotification); message 'textDidEndEditing:';
    procedure textDidChange(notification: NSNotification); message 'textDidChange:';
    procedure selectText(sender: id); message 'selectText:';
    function selectTextAtRow_column(row: clong; col: clong): id; message 'selectTextAtRow:column:';
    function acceptsFirstMouse(theEvent: NSEvent): Boolean; message 'acceptsFirstMouse:';
    procedure resetCursorRects; message 'resetCursorRects';
    procedure setToolTip_forCell(toolTipString: NSString; cell_: NSCell); message 'setToolTip:forCell:';
    function toolTipForCell(cell_: NSCell): NSString; message 'toolTipForCell:';

    { Category: NSKeyboardUI }
    procedure setTabKeyTraversesCells(flag: Boolean); message 'setTabKeyTraversesCells:';
    function tabKeyTraversesCells: Boolean; message 'tabKeyTraversesCells';
    procedure setKeyCell(keyCell_: NSCell); message 'setKeyCell:';
    function keyCell: id; message 'keyCell';
  end; external;

{$endif}
{$endif}
