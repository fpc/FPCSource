{ Parsed from AppKit.framework NSMatrix.h }


{ Callbacks }
{$ifdef TYPES}
type
  NSMatrixSortUsingFunctionCompareCallback = function (param1: id; param2: id; param3: pointer): NSInteger; cdecl;
{$endif}


{$ifdef TYPES}
type
  NSMatrixPtr = ^NSMatrix;
  NSMatrixDelegateProtocolPtr = ^NSMatrixDelegateProtocol;
{$endif}

{$ifdef TYPES}
type
  NSMatrixMode = NSUInteger;
  NSMatrixModePtr = ^NSMatrixMode;

const
  NSRadioModeMatrix = 0;
  NSHighlightModeMatrix = 1;
  NSListModeMatrix = 2;
  NSTrackModeMatrix = 3;

type
  __MFlags = bitpacked record
    case byte of
      0: (_anonBitField___MFlags0: cuint);
      1: (
        {$ifdef __BIG_ENDIAN__}
        highlightMode: 0..1;
        radioMode: 0..1;
        listMode: 0..1;
        allowEmptySel: 0..1;
        autoscroll: 0..1;
        selectionByRect: 0..1;
        drawsCellBackground: 0..1;
        drawsBackground: 0..1;
        autosizeCells: 0..1;
        drawingAncestor: 0..1;
        tabKeyTraversesCells: 0..1;
        tabKeyTraversesCellsExplicitlySet: 0..1;
        allowsIncrementalSearching: 0..1;
        currentlySelectingCell: 0..1;
        onlySetKeyCell: 0..1;
        changingSelectionWithKeyboard: 0..1;
        dontScroll: 0..1;
        refusesFirstResponder: 0..1;
        useSimpleTrackingMode: 0..1;
        checkForSimpleTrackingMode: 0..1;
        liveResizeImageCacheingEnabled: 0..1;
        hasCachedSubclassIsSafeForLiveResize: 0..1;
        subclassIsSafeForLiveResize: 0..1;
        tmpAllowNonVisibleCellsToBecomeFirstResponder: 0..1;
        needsRedrawBeforeFirstLiveResizeCache: 0..1;
        browserOptimizationsEnabled: 0..1;
        drawingContextMenuHighlight: 0..1;
        drawingContextMenuHighlightOnAllSelectedRows: 0..1;
        autorecalculatesCellSize: 0..1;
        cellSizeNeedsAutorecalc: 0..1;
        reservedMatrix: 0..((1 shl 2)-1);
        {$else}
        reservedMatrix_: 0..((1 shl 2)-1);
        cellSizeNeedsAutorecalc_: 0..1;
        autorecalculatesCellSize_: 0..1;
        drawingContextMenuHighlightOnAllSelectedRows_: 0..1;
        drawingContextMenuHighlight_: 0..1;
        browserOptimizationsEnabled_: 0..1;
        needsRedrawBeforeFirstLiveResizeCache_: 0..1;
        tmpAllowNonVisibleCellsToBecomeFirstResponder_: 0..1;
        subclassIsSafeForLiveResize_: 0..1;
        hasCachedSubclassIsSafeForLiveResize_: 0..1;
        liveResizeImageCacheingEnabled_: 0..1;
        checkForSimpleTrackingMode_: 0..1;
        useSimpleTrackingMode_: 0..1;
        refusesFirstResponder_: 0..1;
        dontScroll_: 0..1;
        changingSelectionWithKeyboard_: 0..1;
        onlySetKeyCell_: 0..1;
        currentlySelectingCell_: 0..1;
        allowsIncrementalSearching_: 0..1;
        tabKeyTraversesCellsExplicitlySet_: 0..1;
        tabKeyTraversesCells_: 0..1;
        drawingAncestor_: 0..1;
        autosizeCells_: 0..1;
        drawsBackground_: 0..1;
        drawsCellBackground_: 0..1;
        selectionByRect_: 0..1;
        autoscroll_: 0..1;
        allowEmptySel_: 0..1;
        listMode_: 0..1;
        radioMode_: 0..1;
        highlightMode_: 0..1;
        {$endif}
      );
    end;
type
  _MFlags = __MFlags;
type
  _MFlagsPtr = ^__MFlags;
type
  __MFlagsPtr = ^__MFlags;
{$endif}

{$ifdef CLASSES}

type
  NSMatrix = objcclass external (NSControl, NSUserInterfaceValidationsProtocol)
  private
    _reserved2: id;
    _reserved3: SEL;
    _doubleAction: SEL;
    _errorAction: SEL;
    _delegate: id;
    _selectedCell: id;
    _selectedRow: NSInteger;
    _selectedCol: NSInteger;
    _numRows: NSInteger;
    _numCols: NSInteger;
    _cellSize: NSSize;
    _intercell: NSSize;
    _font: id;
    _protoCell: id;
    _cellClass: id;
    _backgroundColor: NSColor;
    _private: id;
    _cells: NSMutableArray;
    _mFlags: _MFlags;
  public
    function initWithFrame (frameRect: NSRect): instancetype; message 'initWithFrame:';
    function initWithFrame_mode_prototype_numberOfRows_numberOfColumns (frameRect: NSRect; aMode: NSMatrixMode; aCell: NSCell; rowsHigh: NSInteger; colsWide: NSInteger): instancetype; message 'initWithFrame:mode:prototype:numberOfRows:numberOfColumns:';
    function initWithFrame_mode_cellClass_numberOfRows_numberOfColumns (frameRect: NSRect; aMode: NSMatrixMode; factoryId: pobjc_class; rowsHigh: NSInteger; colsWide: NSInteger): instancetype; message 'initWithFrame:mode:cellClass:numberOfRows:numberOfColumns:';
    procedure setCellClass(newValue: pobjc_class); message 'setCellClass:';
    function cellClass: pobjc_class; message 'cellClass';
    procedure setPrototype(newValue: id); message 'setPrototype:';
    function prototype: id; message 'prototype';
    function makeCellAtRow_column (row: NSInteger; col: NSInteger): NSCell; message 'makeCellAtRow:column:';
    procedure setMode(newValue: NSMatrixMode); message 'setMode:';
    function mode: NSMatrixMode; message 'mode';
    procedure setAllowsEmptySelection(newValue: ObjCBOOL); message 'setAllowsEmptySelection:';
    function allowsEmptySelection: ObjCBOOL; message 'allowsEmptySelection';
    procedure sendAction_to_forAllCells (aSelector: SEL; anObject: id; flag: ObjCBOOL); message 'sendAction:to:forAllCells:';
    function cells: NSArray; message 'cells';
    procedure sortUsingSelector (comparator: SEL); message 'sortUsingSelector:';
    procedure sortUsingFunction_context (compare: NSMatrixSortUsingFunctionCompareCallback; context: pointer); message 'sortUsingFunction:context:';
    function selectedCell: id; message 'selectedCell';
    function selectedCells: NSArray; message 'selectedCells';
    function selectedRow: NSInteger; message 'selectedRow';
    function selectedColumn: NSInteger; message 'selectedColumn';
    procedure setSelectionByRect(newValue: ObjCBOOL); message 'setSelectionByRect:';
    function isSelectionByRect: ObjCBOOL; message 'isSelectionByRect';
    procedure setSelectionFrom_to_anchor_highlight (startPos: NSInteger; endPos: NSInteger; anchorPos: NSInteger; lit: ObjCBOOL); message 'setSelectionFrom:to:anchor:highlight:';
    procedure deselectSelectedCell; message 'deselectSelectedCell';
    procedure deselectAllCells; message 'deselectAllCells';
    procedure selectCellAtRow_column (row: NSInteger; col: NSInteger); message 'selectCellAtRow:column:';
    procedure selectAll (sender: id); message 'selectAll:';
    function selectCellWithTag (anInt: NSInteger): ObjCBOOL; message 'selectCellWithTag:';
    procedure setCellSize(newValue: NSSize); message 'setCellSize:';
    function cellSize: NSSize; message 'cellSize';
    procedure setIntercellSpacing(newValue: NSSize); message 'setIntercellSpacing:';
    function intercellSpacing: NSSize; message 'intercellSpacing';
    procedure setScrollable (flag: ObjCBOOL); message 'setScrollable:';
    procedure setBackgroundColor(newValue: NSColor); message 'setBackgroundColor:';
    function backgroundColor: NSColor; message 'backgroundColor';
    procedure setCellBackgroundColor(newValue: NSColor); message 'setCellBackgroundColor:';
    function cellBackgroundColor: NSColor; message 'cellBackgroundColor';
    procedure setDrawsCellBackground(newValue: ObjCBOOL); message 'setDrawsCellBackground:';
    function drawsCellBackground: ObjCBOOL; message 'drawsCellBackground';
    procedure setDrawsBackground(newValue: ObjCBOOL); message 'setDrawsBackground:';
    function drawsBackground: ObjCBOOL; message 'drawsBackground';
    procedure setState_atRow_column (value: NSInteger; row: NSInteger; col: NSInteger); message 'setState:atRow:column:';
    procedure getNumberOfRows_columns (rowCount: NSIntegerPtr; colCount: NSIntegerPtr); message 'getNumberOfRows:columns:';
    function numberOfRows: NSInteger; message 'numberOfRows';
    function numberOfColumns: NSInteger; message 'numberOfColumns';
    function cellAtRow_column (row: NSInteger; col: NSInteger): id; message 'cellAtRow:column:';
    function cellFrameAtRow_column (row: NSInteger; col: NSInteger): NSRect; message 'cellFrameAtRow:column:';
    function getRow_column_ofCell (row: NSIntegerPtr; col: NSIntegerPtr; aCell: NSCell): ObjCBOOL; message 'getRow:column:ofCell:';
    function getRow_column_forPoint (row: NSIntegerPtr; col: NSIntegerPtr; aPoint: NSPoint): ObjCBOOL; message 'getRow:column:forPoint:';
    procedure renewRows_columns (newRows: NSInteger; newCols: NSInteger); message 'renewRows:columns:';
    procedure putCell_atRow_column (newCell: NSCell; row: NSInteger; col: NSInteger); message 'putCell:atRow:column:';
    procedure addRow; message 'addRow';
    procedure addRowWithCells (newCells: NSArray); message 'addRowWithCells:';
    procedure insertRow (row: NSInteger); message 'insertRow:';
    procedure insertRow_withCells (row: NSInteger; newCells: NSArray); message 'insertRow:withCells:';
    procedure removeRow (row: NSInteger); message 'removeRow:';
    procedure addColumn; message 'addColumn';
    procedure addColumnWithCells (newCells: NSArray); message 'addColumnWithCells:';
    procedure insertColumn (column: NSInteger); message 'insertColumn:';
    procedure insertColumn_withCells (column: NSInteger; newCells: NSArray); message 'insertColumn:withCells:';
    procedure removeColumn (col: NSInteger); message 'removeColumn:';
    function cellWithTag (anInt: NSInteger): id; message 'cellWithTag:';
    procedure setDoubleAction(newValue: SEL); message 'setDoubleAction:';
    function doubleAction: SEL; message 'doubleAction';
    procedure setAutosizesCells(newValue: ObjCBOOL); message 'setAutosizesCells:';
    function autosizesCells: ObjCBOOL; message 'autosizesCells';
    procedure sizeToCells; message 'sizeToCells';
    procedure setValidateSize (flag: ObjCBOOL); message 'setValidateSize:';
    procedure drawCellAtRow_column (row: NSInteger; col: NSInteger); message 'drawCellAtRow:column:';
    procedure highlightCell_atRow_column (flag: ObjCBOOL; row: NSInteger; col: NSInteger); message 'highlightCell:atRow:column:';
    procedure setAutoscroll(newValue: ObjCBOOL); message 'setAutoscroll:';
    function isAutoscroll: ObjCBOOL; message 'isAutoscroll';
    procedure scrollCellToVisibleAtRow_column (row: NSInteger; col: NSInteger); message 'scrollCellToVisibleAtRow:column:';
    function mouseDownFlags: NSInteger; message 'mouseDownFlags';
    procedure mouseDown (theEvent: NSEvent); message 'mouseDown:';
    function performKeyEquivalent (theEvent: NSEvent): ObjCBOOL; message 'performKeyEquivalent:';
    function sendAction: ObjCBOOL; message 'sendAction';
    procedure sendDoubleAction; message 'sendDoubleAction';
    procedure setDelegate(newValue: NSMatrixDelegateProtocol); message 'setDelegate:';
    function delegate: NSMatrixDelegateProtocol; message 'delegate';
    function textShouldBeginEditing (textObject: NSText): ObjCBOOL; message 'textShouldBeginEditing:';
    function textShouldEndEditing (textObject: NSText): ObjCBOOL; message 'textShouldEndEditing:';
    procedure textDidBeginEditing (notification: NSNotification); message 'textDidBeginEditing:';
    procedure textDidEndEditing (notification: NSNotification); message 'textDidEndEditing:';
    procedure textDidChange (notification: NSNotification); message 'textDidChange:';
    procedure selectText (sender: id); message 'selectText:';
    function selectTextAtRow_column (row: NSInteger; col: NSInteger): id; message 'selectTextAtRow:column:';
    function acceptsFirstMouse (theEvent: NSEvent): ObjCBOOL; message 'acceptsFirstMouse:';
    procedure resetCursorRects; message 'resetCursorRects';
    procedure setToolTip_forCell (toolTipString: NSString; cell: NSCell); message 'setToolTip:forCell:';
    function toolTipForCell (cell: NSCell): NSString; message 'toolTipForCell:';
    procedure setAutorecalculatesCellSize(newValue: ObjCBOOL); message 'setAutorecalculatesCellSize:';
    function autorecalculatesCellSize: ObjCBOOL; message 'autorecalculatesCellSize';

    { Adopted protocols }
    function validateUserInterfaceItem (anItem: NSValidatedUserInterfaceItemProtocol): ObjCBOOL; message 'validateUserInterfaceItem:';
  end;


type
  NSMatrix_NSKeyboardUI = objccategory external name 'NSKeyboardUI' (NSMatrix)
    procedure setTabKeyTraversesCells(newValue: ObjCBOOL); message 'setTabKeyTraversesCells:';
    function tabKeyTraversesCells: ObjCBOOL; message 'tabKeyTraversesCells';
    procedure setKeyCell(newValue: id); message 'setKeyCell:';
    function keyCell: id; message 'keyCell';
  end;
{$endif}

{$ifdef PROTOCOLS}

type
  NSMatrixDelegateProtocol = objcprotocol external name 'NSMatrixDelegate' (NSControlTextEditingDelegateProtocol)
  end;
{$endif}

