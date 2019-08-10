{ Parsed from AppKit.framework NSBrowser.h }


{$ifdef TYPES}
type
  NSBrowserPtr = ^NSBrowser;
  NSBrowserDelegateProtocolPtr = ^NSBrowserDelegateProtocol;
{$endif}

{$ifdef TYPES}
const
  NSAppKitVersionNumberWithContinuousScrollingBrowser = 680.0;
  NSAppKitVersionNumberWithColumnResizingBrowser = 685.0;

type
  __Brflags = bitpacked record
    case byte of
      0: (_anonBitField___Brflags0: cuint);
      1: (
        {$ifdef __BIG_ENDIAN__}
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
        firstVisibleCalculationDisabled_: 0..1;
        prefersAllColumnUserResizing_: 0..1;
        usesSmallScrollers_: 0..1;
        usesSmallSizeTitleFont_: 0..1;
        actionNeedsToBeSent_: 0..1;
        acceptsFirstMouse_: 0..1;
        refusesFirstResponder_: 0..1;
        disableCompositing_: 0..((1 shl 6)-1);
        delegateSelectsCellsByRow_: 0..1;
        allowsIncrementalSearching_: 0..1;
        time_: 0..1;
        hasHorizontalScroller_: 0..1;
        prohibitEmptySel_: 0..1;
        sendActionOnArrowKeys_: 0..1;
        dontDrawTitles_: 0..1;
        acceptArrowKeys_: 0..1;
        delegateValidatesColumns_: 0..1;
        delegateDoesNotCreateRowsInMatrix_: 0..1;
        delegateSelectsCellsByString_: 0..1;
        delegateSetsTitles_: 0..1;
        delegateImplementsWillDisplayCell_: 0..1;
        separateColumns_: 0..1;
        titleFromPrevious_: 0..1;
        isTitled_: 0..1;
        reuseColumns_: 0..1;
        allowsBranchSelection_: 0..1;
        allowsMultipleSelection_: 0..1;
        {$endif}
      );
    end;
type
  _Brflags = __Brflags;
type
  _BrflagsPtr = ^__Brflags;
type
  __BrflagsPtr = ^__Brflags;

type
  NSBrowserColumnResizingType = NSUInteger;
  NSBrowserColumnResizingTypePtr = ^NSBrowserColumnResizingType;

const
  NSBrowserNoColumnResizing = 0;
  NSBrowserAutoColumnResizing = 1;
  NSBrowserUserColumnResizing = 2;

type
  NSBrowserDropOperation = NSUInteger;
  NSBrowserDropOperationPtr = ^NSBrowserDropOperation;

const
  NSBrowserDropOn = 0;
  NSBrowserDropAbove = 1;
{$endif}

{$ifdef CLASSES}

type
  NSBrowser = objcclass external (NSControl)
  private
    _nsreserved2: id;
    _nsreserved3: SEL;
    _delegate: id;
    _doubleAction: SEL;
    _matrixClass: pobjc_class;
    _cellPrototype: id;
    _columnSize: NSSize;
    _numberOfVisibleColumns: cshort;
    _minColumnWidth: cshort;
    _firstVisibleColumn: cshort;
    _maxVisibleColumns: cshort;
    _titles: NSMutableArray;
    _pathSeparator: NSString;
    _columns: NSMutableArray;
    _brAuxiliaryStorage: id;
    _firstColumnTitle: NSString;
    _scroller: NSScroller;
    _brflags: _Brflags;
  public
    class function cellClass: pobjc_class; message 'cellClass';
    procedure loadColumnZero; message 'loadColumnZero';
    function isLoaded: ObjCBOOL; message 'isLoaded';
    procedure setDoubleAction(newValue: SEL); message 'setDoubleAction:';
    function doubleAction: SEL; message 'doubleAction';
    procedure setCellClass (factoryId: pobjc_class); message 'setCellClass:';
    procedure setCellPrototype(newValue: id); message 'setCellPrototype:';
    function cellPrototype: id; message 'cellPrototype';
    procedure setDelegate(newValue: NSBrowserDelegateProtocol); message 'setDelegate:';
    function delegate: NSBrowserDelegateProtocol; message 'delegate';
    procedure setReusesColumns(newValue: ObjCBOOL); message 'setReusesColumns:';
    function reusesColumns: ObjCBOOL; message 'reusesColumns';
    procedure setHasHorizontalScroller(newValue: ObjCBOOL); message 'setHasHorizontalScroller:';
    function hasHorizontalScroller: ObjCBOOL; message 'hasHorizontalScroller';
    procedure setAutohidesScroller(newValue: ObjCBOOL); message 'setAutohidesScroller:';
    function autohidesScroller: ObjCBOOL; message 'autohidesScroller';
    procedure setSeparatesColumns(newValue: ObjCBOOL); message 'setSeparatesColumns:';
    function separatesColumns: ObjCBOOL; message 'separatesColumns';
    procedure setTitled(newValue: ObjCBOOL); message 'setTitled:';
    function isTitled: ObjCBOOL; message 'isTitled';
    procedure setMinColumnWidth(newValue: CGFloat); message 'setMinColumnWidth:';
    function minColumnWidth: CGFloat; message 'minColumnWidth';
    procedure setMaxVisibleColumns(newValue: NSInteger); message 'setMaxVisibleColumns:';
    function maxVisibleColumns: NSInteger; message 'maxVisibleColumns';
    procedure setAllowsMultipleSelection(newValue: ObjCBOOL); message 'setAllowsMultipleSelection:';
    function allowsMultipleSelection: ObjCBOOL; message 'allowsMultipleSelection';
    procedure setAllowsBranchSelection(newValue: ObjCBOOL); message 'setAllowsBranchSelection:';
    function allowsBranchSelection: ObjCBOOL; message 'allowsBranchSelection';
    procedure setAllowsEmptySelection(newValue: ObjCBOOL); message 'setAllowsEmptySelection:';
    function allowsEmptySelection: ObjCBOOL; message 'allowsEmptySelection';
    procedure setTakesTitleFromPreviousColumn(newValue: ObjCBOOL); message 'setTakesTitleFromPreviousColumn:';
    function takesTitleFromPreviousColumn: ObjCBOOL; message 'takesTitleFromPreviousColumn';
    procedure setSendsActionOnArrowKeys(newValue: ObjCBOOL); message 'setSendsActionOnArrowKeys:';
    function sendsActionOnArrowKeys: ObjCBOOL; message 'sendsActionOnArrowKeys';
    function itemAtIndexPath (indexPath: NSIndexPath): id; message 'itemAtIndexPath:'; { available in 10_6 }
    function itemAtRow_inColumn (row: NSInteger; column: NSInteger): id; message 'itemAtRow:inColumn:'; { available in 10_6 }
    function indexPathForColumn (column: NSInteger): NSIndexPath; message 'indexPathForColumn:'; { available in 10_6 }
    function isLeafItem (item: id): ObjCBOOL; message 'isLeafItem:'; { available in 10_6 }
    procedure reloadDataForRowIndexes_inColumn (rowIndexes: NSIndexSet; column: NSInteger); message 'reloadDataForRowIndexes:inColumn:'; { available in 10_6 }
    function parentForItemsInColumn (column: NSInteger): id; message 'parentForItemsInColumn:'; { available in 10_6 }
    procedure scrollRowToVisible_inColumn (row: NSInteger; column: NSInteger); message 'scrollRowToVisible:inColumn:'; { available in 10_6 }
    procedure setTitle_ofColumn (aString: NSString; column: NSInteger); message 'setTitle:ofColumn:';
    function titleOfColumn (column: NSInteger): NSString; message 'titleOfColumn:';
    procedure setPathSeparator(newValue: NSString); message 'setPathSeparator:';
    function pathSeparator: NSString; message 'pathSeparator';
    function setPath (path: NSString): ObjCBOOL; message 'setPath:';
    function path: NSString; message 'path';
    function pathToColumn (column: NSInteger): NSString; message 'pathToColumn:';
    function clickedColumn: NSInteger; message 'clickedColumn';
    function clickedRow: NSInteger; message 'clickedRow';
    function selectedColumn: NSInteger; message 'selectedColumn';
    function selectedCell: id; message 'selectedCell';
    function selectedCellInColumn (column: NSInteger): id; message 'selectedCellInColumn:';
    function selectedCells: NSArray; message 'selectedCells';
    procedure selectRow_inColumn (row: NSInteger; column: NSInteger); message 'selectRow:inColumn:';
    function selectedRowInColumn (column: NSInteger): NSInteger; message 'selectedRowInColumn:';
    procedure setSelectionIndexPath(newValue: NSIndexPath); message 'setSelectionIndexPath:';
    function selectionIndexPath: NSIndexPath; message 'selectionIndexPath';
    procedure setSelectionIndexPaths(newValue: NSArray); message 'setSelectionIndexPaths:';
    function selectionIndexPaths: NSArray; message 'selectionIndexPaths';
    procedure selectRowIndexes_inColumn (indexes: NSIndexSet; column: NSInteger); message 'selectRowIndexes:inColumn:'; { available in 10_5 }
    function selectedRowIndexesInColumn (column: NSInteger): NSIndexSet; message 'selectedRowIndexesInColumn:'; { available in 10_5 }
    procedure reloadColumn (column: NSInteger); message 'reloadColumn:';
    procedure validateVisibleColumns; message 'validateVisibleColumns';
    procedure scrollColumnsRightBy (shiftAmount: NSInteger); message 'scrollColumnsRightBy:';
    procedure scrollColumnsLeftBy (shiftAmount: NSInteger); message 'scrollColumnsLeftBy:';
    procedure scrollColumnToVisible (column: NSInteger); message 'scrollColumnToVisible:';
    procedure setLastColumn(newValue: NSInteger); message 'setLastColumn:';
    function lastColumn: NSInteger; message 'lastColumn';
    procedure addColumn; message 'addColumn';
    function numberOfVisibleColumns: NSInteger; message 'numberOfVisibleColumns';
    function firstVisibleColumn: NSInteger; message 'firstVisibleColumn';
    function lastVisibleColumn: NSInteger; message 'lastVisibleColumn';
    function loadedCellAtRow_column (row: NSInteger; col: NSInteger): id; message 'loadedCellAtRow:column:';
    procedure selectAll (sender: id); message 'selectAll:';
    procedure tile; message 'tile';
    procedure doClick (sender: id); message 'doClick:';
    procedure doDoubleClick (sender: id); message 'doDoubleClick:';
    function sendAction: ObjCBOOL; message 'sendAction';
    function titleFrameOfColumn (column: NSInteger): NSRect; message 'titleFrameOfColumn:';
    procedure drawTitleOfColumn_inRect (column: NSInteger; aRect: NSRect); message 'drawTitleOfColumn:inRect:';
    function titleHeight: CGFloat; message 'titleHeight';
    function frameOfColumn (column: NSInteger): NSRect; message 'frameOfColumn:';
    function frameOfInsideOfColumn (column: NSInteger): NSRect; message 'frameOfInsideOfColumn:';
    function frameOfRow_inColumn (row: NSInteger; column: NSInteger): NSRect; message 'frameOfRow:inColumn:'; { available in 10_6 }
    function getRow_column_forPoint (row: NSIntegerPtr; column: NSIntegerPtr; point: NSPoint): ObjCBOOL; message 'getRow:column:forPoint:'; { available in 10_6 }
    function columnWidthForColumnContentWidth (columnContentWidth: CGFloat): CGFloat; message 'columnWidthForColumnContentWidth:';
    function columnContentWidthForColumnWidth (columnWidth: CGFloat): CGFloat; message 'columnContentWidthForColumnWidth:';
    procedure setColumnResizingType(newValue: NSBrowserColumnResizingType); message 'setColumnResizingType:';
    function columnResizingType: NSBrowserColumnResizingType; message 'columnResizingType';
    procedure setPrefersAllColumnUserResizing(newValue: ObjCBOOL); message 'setPrefersAllColumnUserResizing:';
    function prefersAllColumnUserResizing: ObjCBOOL; message 'prefersAllColumnUserResizing';
    procedure setWidth_ofColumn (columnWidth: CGFloat; columnIndex: NSInteger); message 'setWidth:ofColumn:';
    function widthOfColumn (column: NSInteger): CGFloat; message 'widthOfColumn:';
    procedure setRowHeight(newValue: CGFloat); message 'setRowHeight:';
    function rowHeight: CGFloat; message 'rowHeight';
    procedure noteHeightOfRowsWithIndexesChanged_inColumn (indexSet: NSIndexSet; columnIndex: NSInteger); message 'noteHeightOfRowsWithIndexesChanged:inColumn:'; { available in 10_6 }
    procedure setDefaultColumnWidth (columnWidth: CGFloat); message 'setDefaultColumnWidth:'; { available in 10_6 }
    function defaultColumnWidth: CGFloat; message 'defaultColumnWidth'; { available in 10_6 }
    procedure setColumnsAutosaveName(newValue: NSString); message 'setColumnsAutosaveName:';
    function columnsAutosaveName: NSString; message 'columnsAutosaveName';
    class procedure removeSavedColumnsWithAutosaveName (name: NSString); message 'removeSavedColumnsWithAutosaveName:';
    function canDragRowsWithIndexes_inColumn_withEvent (rowIndexes: NSIndexSet; column: NSInteger; event: NSEvent): ObjCBOOL; message 'canDragRowsWithIndexes:inColumn:withEvent:'; { available in 10_5 }
    function draggingImageForRowsWithIndexes_inColumn_withEvent_offset (rowIndexes: NSIndexSet; column: NSInteger; event: NSEvent; dragImageOffset: NSPointPointer): NSImage; message 'draggingImageForRowsWithIndexes:inColumn:withEvent:offset:'; { available in 10_5 }
    procedure setDraggingSourceOperationMask_forLocal (mask: NSDragOperation; isLocal: ObjCBOOL); message 'setDraggingSourceOperationMask:forLocal:'; { available in 10_5 }
    procedure setAllowsTypeSelect(newValue: ObjCBOOL); message 'setAllowsTypeSelect:';
    function allowsTypeSelect: ObjCBOOL; message 'allowsTypeSelect';
    procedure setBackgroundColor(newValue: NSColor); message 'setBackgroundColor:';
    function backgroundColor: NSColor; message 'backgroundColor';
    procedure editItemAtIndexPath_withEvent_select (indexPath: NSIndexPath; theEvent: NSEvent; select: ObjCBOOL); message 'editItemAtIndexPath:withEvent:select:'; { available in 10_6 }
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSBrowserColumnConfigurationDidChangeNotification: NSString; cvar; external;
{$endif}

{$ifdef PROTOCOLS}

type
  NSBrowserDelegateProtocol = objcprotocol external name 'NSBrowserDelegate' (NSObjectProtocol)
  optional
    function browser_numberOfRowsInColumn (sender: NSBrowser; column: NSInteger): NSInteger; message 'browser:numberOfRowsInColumn:';
    procedure browser_createRowsForColumn_inMatrix (sender: NSBrowser; column: NSInteger; matrix: NSMatrix); message 'browser:createRowsForColumn:inMatrix:';
    function browser_numberOfChildrenOfItem (browser: NSBrowser; item: id): NSInteger; message 'browser:numberOfChildrenOfItem:'; { available in 10_6 }
    function browser_child_ofItem (browser: NSBrowser; index: NSInteger; item: id): id; message 'browser:child:ofItem:'; { available in 10_6 }
    function browser_isLeafItem (browser: NSBrowser; item: id): ObjCBOOL; message 'browser:isLeafItem:'; { available in 10_6 }
    function browser_objectValueForItem (browser: NSBrowser; item: id): id; message 'browser:objectValueForItem:'; { available in 10_6 }
    function browser_heightOfRow_inColumn (browser: NSBrowser; row: NSInteger; columnIndex: NSInteger): CGFloat; message 'browser:heightOfRow:inColumn:'; { available in 10_6 }
    function rootItemForBrowser (browser: NSBrowser): id; message 'rootItemForBrowser:'; { available in 10_6 }
    procedure browser_setObjectValue_forItem (browser: NSBrowser; object_: id; item: id); message 'browser:setObjectValue:forItem:'; { available in 10_6 }
    function browser_shouldEditItem (browser: NSBrowser; item: id): ObjCBOOL; message 'browser:shouldEditItem:'; { available in 10_6 }
    procedure browser_willDisplayCell_atRow_column (sender: NSBrowser; cell: id; row: NSInteger; column: NSInteger); message 'browser:willDisplayCell:atRow:column:';
    function browser_titleOfColumn (sender: NSBrowser; column: NSInteger): NSString; message 'browser:titleOfColumn:';
    function browser_selectCellWithString_inColumn (sender: NSBrowser; title: NSString; column: NSInteger): ObjCBOOL; message 'browser:selectCellWithString:inColumn:';
    function browser_selectRow_inColumn (sender: NSBrowser; row: NSInteger; column: NSInteger): ObjCBOOL; message 'browser:selectRow:inColumn:';
    function browser_isColumnValid (sender: NSBrowser; column: NSInteger): ObjCBOOL; message 'browser:isColumnValid:';
    procedure browserWillScroll (sender: NSBrowser); message 'browserWillScroll:';
    procedure browserDidScroll (sender: NSBrowser); message 'browserDidScroll:';
    function browser_shouldSizeColumn_forUserResize_toWidth (browser: NSBrowser; columnIndex: NSInteger; forUserResize: ObjCBOOL; suggestedWidth: CGFloat): CGFloat; message 'browser:shouldSizeColumn:forUserResize:toWidth:';
    function browser_sizeToFitWidthOfColumn (browser: NSBrowser; columnIndex: NSInteger): CGFloat; message 'browser:sizeToFitWidthOfColumn:';
    procedure browserColumnConfigurationDidChange (notification: NSNotification); message 'browserColumnConfigurationDidChange:';
    function browser_shouldShowCellExpansionForRow_column (browser: NSBrowser; row: NSInteger; column: NSInteger): ObjCBOOL; message 'browser:shouldShowCellExpansionForRow:column:'; { available in 10_5 }
    function browser_writeRowsWithIndexes_inColumn_toPasteboard (browser: NSBrowser; rowIndexes: NSIndexSet; column: NSInteger; pasteboard: NSPasteboard): ObjCBOOL; message 'browser:writeRowsWithIndexes:inColumn:toPasteboard:'; { available in 10_5 }
    function browser_namesOfPromisedFilesDroppedAtDestination_forDraggedRowsWithIndexes_inColumn (browser: NSBrowser; dropDestination: NSURL; rowIndexes: NSIndexSet; column: NSInteger): NSArray; message 'browser:namesOfPromisedFilesDroppedAtDestination:forDraggedRowsWithIndexes:inColumn:'; { available in 10_5 }
    function browser_canDragRowsWithIndexes_inColumn_withEvent (browser: NSBrowser; rowIndexes: NSIndexSet; column: NSInteger; event: NSEvent): ObjCBOOL; message 'browser:canDragRowsWithIndexes:inColumn:withEvent:'; { available in 10_5 }
    function browser_draggingImageForRowsWithIndexes_inColumn_withEvent_offset (browser: NSBrowser; rowIndexes: NSIndexSet; column: NSInteger; event: NSEvent; dragImageOffset: NSPointPointer): NSImage; message 'browser:draggingImageForRowsWithIndexes:inColumn:withEvent:offset:'; { available in 10_5 }
    function browser_validateDrop_proposedRow_column_dropOperation (browser: NSBrowser; info: NSDraggingInfoProtocol; row: NSIntegerPtr; column: NSIntegerPtr; dropOperation: NSBrowserDropOperationPtr): NSDragOperation; message 'browser:validateDrop:proposedRow:column:dropOperation:'; { available in 10_5 }
    function browser_acceptDrop_atRow_column_dropOperation (browser: NSBrowser; info: NSDraggingInfoProtocol; row: NSInteger; column: NSInteger; dropOperation: NSBrowserDropOperation): ObjCBOOL; message 'browser:acceptDrop:atRow:column:dropOperation:'; { available in 10_5 }
    function browser_typeSelectStringForRow_inColumn (browser: NSBrowser; row: NSInteger; column: NSInteger): NSString; message 'browser:typeSelectStringForRow:inColumn:'; { available in 10_5 }
    function browser_shouldTypeSelectForEvent_withCurrentSearchString (browser: NSBrowser; event: NSEvent; searchString: NSString): ObjCBOOL; message 'browser:shouldTypeSelectForEvent:withCurrentSearchString:'; { available in 10_5 }
    function browser_nextTypeSelectMatchFromRow_toRow_inColumn_forString (browser: NSBrowser; startRow: NSInteger; endRow: NSInteger; column: NSInteger; searchString: NSString): NSInteger; message 'browser:nextTypeSelectMatchFromRow:toRow:inColumn:forString:'; { available in 10_5 }
    function browser_previewViewControllerForLeafItem (browser: NSBrowser; item: id): NSViewController; message 'browser:previewViewControllerForLeafItem:'; { available in 10_6 }
    function browser_headerViewControllerForItem (browser: NSBrowser; item: id): NSViewController; message 'browser:headerViewControllerForItem:'; { available in 10_6 }
    procedure browser_didChangeLastColumn_toColumn (browser: NSBrowser; oldLastColumn: NSInteger; column: NSInteger); message 'browser:didChangeLastColumn:toColumn:';
    function browser_selectionIndexesForProposedSelection_inColumn (browser: NSBrowser; proposedSelectionIndexes: NSIndexSet; column: NSInteger): NSIndexSet; message 'browser:selectionIndexesForProposedSelection:inColumn:'; { available in 10_6 }
  end;
{$endif}

{$ifdef CLASSES}

type
  NSBrowser_NSDeprecated = objccategory external name 'NSDeprecated' (NSBrowser)
    procedure setAcceptsArrowKeys (flag: ObjCBOOL); message 'setAcceptsArrowKeys:'; deprecated 'in 10_0, 10_6';
    function acceptsArrowKeys: ObjCBOOL; message 'acceptsArrowKeys'; deprecated 'in 10_0, 10_6';
    procedure displayColumn (column: NSInteger); message 'displayColumn:'; deprecated 'in 10_0, 10_3';
    procedure displayAllColumns; message 'displayAllColumns'; deprecated 'in 10_0, 10_3';
    procedure scrollViaScroller (sender: NSScroller); message 'scrollViaScroller:'; deprecated 'in 10_0, 10_3';
    procedure updateScroller; message 'updateScroller'; deprecated 'in 10_0, 10_3';
    procedure setMatrixClass (factoryId: pobjc_class); message 'setMatrixClass:'; deprecated 'in 10_0, 10_10, "Use the item based NSBrowser instead"';
    function matrixClass: pobjc_class; message 'matrixClass'; deprecated 'in 10_0, 10_10, "Use the item based NSBrowser instead"';
    function columnOfMatrix (matrix: NSMatrix): NSInteger; message 'columnOfMatrix:'; deprecated 'in 10_0, 10_10, "Use the item based NSBrowser instead"';
    function matrixInColumn (column: NSInteger): NSMatrix; message 'matrixInColumn:'; deprecated 'in 10_0, 10_10, "Use the item based NSBrowser instead"';
  end;
{$endif}

