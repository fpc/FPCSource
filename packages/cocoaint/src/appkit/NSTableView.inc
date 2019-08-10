{ Parsed from AppKit.framework NSTableView.h }


{ Types from NSTableView }
{$ifdef TYPES}

{$if (MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_7)}
const
  NSTableViewAnimationEffectNone = $0;
const
  NSTableViewAnimationEffectFade = $1;
  NSTableViewAnimationEffectGap = $2;
const
  NSTableViewAnimationSlideUp = $10;
  NSTableViewAnimationSlideDown = $20;
  NSTableViewAnimationSlideLeft = $30;
  NSTableViewAnimationSlideRight = $40;
{$endif}

type
  NSTableViewAnimationOptions = NSUInteger;
type
  NSTableViewAnimationOptionsPtr = ^NSTableViewAnimationOptions;
{$endif}


{$ifdef TYPES}
type
  NSTableViewPtr = ^NSTableView;
  NSTableViewDelegateProtocolPtr = ^NSTableViewDelegateProtocol;
  NSTableViewDataSourceProtocolPtr = ^NSTableViewDataSourceProtocol;
{$endif}

{$ifdef TYPES}
type
  __TvFlags = bitpacked record
    case byte of
      0: (_anonBitField___TvFlags0: cuint);
      1: (
        {$ifdef __BIG_ENDIAN__}
        allowsColumnReordering: 0..1;
        allowsColumnResizing: 0..1;
        hasBlurBackgroundViews: 0..1;
        allowsEmptySelection: 0..1;
        allowsMultipleSelection: 0..1;
        allowsColumnSelection: 0..1;
        selectionType: 0..((1 shl 2)-1);
        allowGapRow: 0..1;
        compareWidthWithSuperview: 0..((1 shl 2)-1);
        delegateWillDisplayCell: 0..1;
        delegateShouldEditTableColumn: 0..1;
        delegateShouldSelectRow: 0..1;
        delegateShouldSelectTableColumn: 0..1;
        delegateSelectionShouldChangeInTableView: 0..1;
        oldAutoresizesAllColumnsToFit: 0..1;
        dataSourceSetObjectValue: 0..1;
        selectionPostingDisableCount: 0..((1 shl 7)-1);
        movedPostingDisableCount: 0..((1 shl 6)-1);
        refusesFirstResponder: 0..1;
        {$else}
        refusesFirstResponder_: 0..1;
        movedPostingDisableCount_: 0..((1 shl 6)-1);
        selectionPostingDisableCount_: 0..((1 shl 7)-1);
        dataSourceSetObjectValue_: 0..1;
        oldAutoresizesAllColumnsToFit_: 0..1;
        delegateSelectionShouldChangeInTableView_: 0..1;
        delegateShouldSelectTableColumn_: 0..1;
        delegateShouldSelectRow_: 0..1;
        delegateShouldEditTableColumn_: 0..1;
        delegateWillDisplayCell_: 0..1;
        compareWidthWithSuperview_: 0..((1 shl 2)-1);
        allowGapRow_: 0..1;
        selectionType_: 0..((1 shl 2)-1);
        allowsColumnSelection_: 0..1;
        allowsMultipleSelection_: 0..1;
        allowsEmptySelection_: 0..1;
        hasBlurBackgroundViews_: 0..1;
        allowsColumnResizing_: 0..1;
        allowsColumnReordering_: 0..1;
        {$endif}
      );
    end;
type
  _TvFlags = __TvFlags;
type
  _TvFlagsPtr = ^__TvFlags;
type
  __TvFlagsPtr = ^__TvFlags;

type
  NSTableViewDropOperation = NSUInteger;
  NSTableViewDropOperationPtr = ^NSTableViewDropOperation;

const
  NSTableViewDropOn = 0;
  NSTableViewDropAbove = 1;

type
  NSTableViewColumnAutoresizingStyle = NSUInteger;
  NSTableViewColumnAutoresizingStylePtr = ^NSTableViewColumnAutoresizingStyle;

const
  NSTableViewNoColumnAutoresizing = 0;
  NSTableViewUniformColumnAutoresizingStyle = 1;
  NSTableViewSequentialColumnAutoresizingStyle = 2;
  NSTableViewReverseSequentialColumnAutoresizingStyle = 3;
  NSTableViewLastColumnOnlyAutoresizingStyle = 4;
  NSTableViewFirstColumnOnlyAutoresizingStyle = 5;

type
  NSTableViewGridLineStyle = NSUInteger;
  NSTableViewGridLineStylePtr = ^NSTableViewGridLineStyle;

const
  NSTableViewGridNone = 0;
  NSTableViewSolidVerticalGridLineMask = 1 shl 0;
  NSTableViewSolidHorizontalGridLineMask = 1 shl 1;
  NSTableViewDashedHorizontalGridLineMask = 1 shl 3 { available in 10_7 };

type
  NSTableViewRowSizeStyle = NSInteger;
  NSTableViewRowSizeStylePtr = ^NSTableViewRowSizeStyle;

const
  NSTableViewRowSizeStyleDefault = -1;
  NSTableViewRowSizeStyleCustom = 0;
  NSTableViewRowSizeStyleSmall = 1;
  NSTableViewRowSizeStyleMedium = 2;
  NSTableViewRowSizeStyleLarge = 3;

type
  NSTableViewSelectionHighlightStyle = NSInteger;
  NSTableViewSelectionHighlightStylePtr = ^NSTableViewSelectionHighlightStyle;

const
  NSTableViewSelectionHighlightStyleNone = -1 { available in 10_6 };
  NSTableViewSelectionHighlightStyleRegular = 0;
  NSTableViewSelectionHighlightStyleSourceList = 1;

type
  NSTableViewDraggingDestinationFeedbackStyle = NSInteger;
  NSTableViewDraggingDestinationFeedbackStylePtr = ^NSTableViewDraggingDestinationFeedbackStyle;

const
  NSTableViewDraggingDestinationFeedbackStyleNone = -1;
  NSTableViewDraggingDestinationFeedbackStyleRegular = 0;
  NSTableViewDraggingDestinationFeedbackStyleSourceList = 1;
  NSTableViewDraggingDestinationFeedbackStyleGap = 2 { available in 10_9 };
{$endif}

{$ifdef CLASSES}

type
  NSTableView = objcclass external (NSControl, NSUserInterfaceValidationsProtocol, NSTextViewDelegateProtocol, NSDraggingSourceProtocol, NSAccessibilityTableProtocol)
  private
    _headerView: NSTableHeaderView;
    _cornerView: NSView;
    _tableColumns: NSMutableArray;
    _editingCell: NSCell;
    _delegate: id;
    _dataSource: id;
    _intercellSpacing: NSSize;
    _rowHeight: CGFloat;
    _lastSelectedColumn: NSInteger;
    _lastSelectedRow: NSInteger;
    _editingRow: NSInteger;
    _editingColumn: NSInteger;
    _selectedColumns: NSMutableIndexSet;
    _selectedRows: NSMutableIndexSet;
    _columnDragInfoX: id;
    _backgroundColor: NSColor;
    _gridColor: NSColor;
    _rowDataX: id;
    _reserved3: id;
    _reserved4: SEL;
    _doubleAction: SEL;
    _rectOfLastColumn: NSRect;
    _lastCachedRectColumn: NSInteger;
    _rectOfLastRow: NSRect deprecated 'in 10_0, 10_7';
    _lastCachedRectRow: NSInteger deprecated 'in 10_0, 10_7';
    _tvFlags: _TvFlags;
    _reserved: id;
  public
    function initWithFrame (frameRect: NSRect): instancetype; message 'initWithFrame:'; { NS_DESIGNATED_INITIALIZER }
    function initWithCoder (coder: NSCoder): instancetype; message 'initWithCoder:'; { NS_DESIGNATED_INITIALIZER }
    procedure setDataSource (aSource: NSTableViewDataSourceProtocol); message 'setDataSource:';
    function dataSource: NSTableViewDataSourceProtocol; message 'dataSource';
    procedure setDelegate (delegate: NSTableViewDelegateProtocol); message 'setDelegate:';
    function delegate: NSTableViewDelegateProtocol; message 'delegate';
    procedure setHeaderView(newValue: NSTableHeaderView); message 'setHeaderView:';
    function headerView: NSTableHeaderView; message 'headerView';
    procedure setCornerView(newValue: NSView); message 'setCornerView:';
    function cornerView: NSView; message 'cornerView';
    procedure setAllowsColumnReordering(newValue: ObjCBOOL); message 'setAllowsColumnReordering:';
    function allowsColumnReordering: ObjCBOOL; message 'allowsColumnReordering';
    procedure setAllowsColumnResizing(newValue: ObjCBOOL); message 'setAllowsColumnResizing:';
    function allowsColumnResizing: ObjCBOOL; message 'allowsColumnResizing';
    procedure setColumnAutoresizingStyle(newValue: NSTableViewColumnAutoresizingStyle); message 'setColumnAutoresizingStyle:';
    function columnAutoresizingStyle: NSTableViewColumnAutoresizingStyle; message 'columnAutoresizingStyle';
    procedure setGridStyleMask(newValue: NSTableViewGridLineStyle); message 'setGridStyleMask:';
    function gridStyleMask: NSTableViewGridLineStyle; message 'gridStyleMask';
    procedure setIntercellSpacing(newValue: NSSize); message 'setIntercellSpacing:';
    function intercellSpacing: NSSize; message 'intercellSpacing';
    procedure setUsesAlternatingRowBackgroundColors(newValue: ObjCBOOL); message 'setUsesAlternatingRowBackgroundColors:';
    function usesAlternatingRowBackgroundColors: ObjCBOOL; message 'usesAlternatingRowBackgroundColors';
    procedure setBackgroundColor(newValue: NSColor); message 'setBackgroundColor:';
    function backgroundColor: NSColor; message 'backgroundColor';
    procedure setGridColor(newValue: NSColor); message 'setGridColor:';
    function gridColor: NSColor; message 'gridColor';
    procedure setRowSizeStyle(newValue: NSTableViewRowSizeStyle); message 'setRowSizeStyle:';
    function rowSizeStyle: NSTableViewRowSizeStyle; message 'rowSizeStyle';
    function effectiveRowSizeStyle: NSTableViewRowSizeStyle; message 'effectiveRowSizeStyle';
    procedure setRowHeight(newValue: CGFloat); message 'setRowHeight:';
    function rowHeight: CGFloat; message 'rowHeight';
    procedure noteHeightOfRowsWithIndexesChanged (indexSet: NSIndexSet); message 'noteHeightOfRowsWithIndexesChanged:';
    function tableColumns: NSArray; message 'tableColumns';
    function numberOfColumns: NSInteger; message 'numberOfColumns';
    function numberOfRows: NSInteger; message 'numberOfRows';
    procedure addTableColumn (tableColumn: NSTableColumn); message 'addTableColumn:';
    procedure removeTableColumn (tableColumn: NSTableColumn); message 'removeTableColumn:';
    procedure moveColumn_toColumn (oldIndex: NSInteger; newIndex: NSInteger); message 'moveColumn:toColumn:';
    function columnWithIdentifier (identifier: NSString): NSInteger; message 'columnWithIdentifier:';
    function tableColumnWithIdentifier (identifier: NSString): NSTableColumn; message 'tableColumnWithIdentifier:';
    procedure tile; message 'tile';
    procedure sizeToFit; message 'sizeToFit';
    procedure sizeLastColumnToFit; message 'sizeLastColumnToFit';
    procedure scrollRowToVisible (row: NSInteger); message 'scrollRowToVisible:';
    procedure scrollColumnToVisible (column: NSInteger); message 'scrollColumnToVisible:';
    procedure reloadData; message 'reloadData';
    procedure noteNumberOfRowsChanged; message 'noteNumberOfRowsChanged';
    procedure reloadDataForRowIndexes_columnIndexes (rowIndexes: NSIndexSet; columnIndexes: NSIndexSet); message 'reloadDataForRowIndexes:columnIndexes:'; { available in 10_6 }
    function editedColumn: NSInteger; message 'editedColumn';
    function editedRow: NSInteger; message 'editedRow';
    function clickedColumn: NSInteger; message 'clickedColumn';
    function clickedRow: NSInteger; message 'clickedRow';
    procedure setDoubleAction(newValue: SEL); message 'setDoubleAction:';
    function doubleAction: SEL; message 'doubleAction';
    procedure setSortDescriptors(newValue: NSArray); message 'setSortDescriptors:';
    function sortDescriptors: NSArray; message 'sortDescriptors';
    procedure setIndicatorImage_inTableColumn (anImage: NSImage; tableColumn: NSTableColumn); message 'setIndicatorImage:inTableColumn:';
    function indicatorImageInTableColumn (tableColumn: NSTableColumn): NSImage; message 'indicatorImageInTableColumn:';
    procedure setHighlightedTableColumn(newValue: NSTableColumn); message 'setHighlightedTableColumn:';
    function highlightedTableColumn: NSTableColumn; message 'highlightedTableColumn';
    procedure setVerticalMotionCanBeginDrag(newValue: ObjCBOOL); message 'setVerticalMotionCanBeginDrag:';
    function verticalMotionCanBeginDrag: ObjCBOOL; message 'verticalMotionCanBeginDrag';
    function canDragRowsWithIndexes_atPoint (rowIndexes: NSIndexSet; mouseDownPoint: NSPoint): ObjCBOOL; message 'canDragRowsWithIndexes:atPoint:';
    function dragImageForRowsWithIndexes_tableColumns_event_offset (dragRows: NSIndexSet; tableColumns_: NSArray; dragEvent: NSEvent; dragImageOffset: NSPointPointer): NSImage; message 'dragImageForRowsWithIndexes:tableColumns:event:offset:';
    procedure setDraggingSourceOperationMask_forLocal (mask: NSDragOperation; isLocal: ObjCBOOL); message 'setDraggingSourceOperationMask:forLocal:';
    procedure setDropRow_dropOperation (row: NSInteger; dropOperation: NSTableViewDropOperation); message 'setDropRow:dropOperation:';
    procedure setAllowsMultipleSelection(newValue: ObjCBOOL); message 'setAllowsMultipleSelection:';
    function allowsMultipleSelection: ObjCBOOL; message 'allowsMultipleSelection';
    procedure setAllowsEmptySelection(newValue: ObjCBOOL); message 'setAllowsEmptySelection:';
    function allowsEmptySelection: ObjCBOOL; message 'allowsEmptySelection';
    procedure setAllowsColumnSelection(newValue: ObjCBOOL); message 'setAllowsColumnSelection:';
    function allowsColumnSelection: ObjCBOOL; message 'allowsColumnSelection';
    procedure selectAll (sender: id); message 'selectAll:';
    procedure deselectAll (sender: id); message 'deselectAll:';
    procedure selectColumnIndexes_byExtendingSelection (indexes: NSIndexSet; extend: ObjCBOOL); message 'selectColumnIndexes:byExtendingSelection:';
    procedure selectRowIndexes_byExtendingSelection (indexes: NSIndexSet; extend: ObjCBOOL); message 'selectRowIndexes:byExtendingSelection:';
    function selectedColumnIndexes: NSIndexSet; message 'selectedColumnIndexes';
    function selectedRowIndexes: NSIndexSet; message 'selectedRowIndexes';
    procedure deselectColumn (column: NSInteger); message 'deselectColumn:';
    procedure deselectRow (row: NSInteger); message 'deselectRow:';
    function selectedColumn: NSInteger; message 'selectedColumn';
    function selectedRow: NSInteger; message 'selectedRow';
    function isColumnSelected (column: NSInteger): ObjCBOOL; message 'isColumnSelected:';
    function isRowSelected (row: NSInteger): ObjCBOOL; message 'isRowSelected:';
    function numberOfSelectedColumns: NSInteger; message 'numberOfSelectedColumns';
    function numberOfSelectedRows: NSInteger; message 'numberOfSelectedRows';
    procedure setAllowsTypeSelect(newValue: ObjCBOOL); message 'setAllowsTypeSelect:';
    function allowsTypeSelect: ObjCBOOL; message 'allowsTypeSelect';
    procedure setSelectionHighlightStyle(newValue: NSTableViewSelectionHighlightStyle); message 'setSelectionHighlightStyle:';
    function selectionHighlightStyle: NSTableViewSelectionHighlightStyle; message 'selectionHighlightStyle';
    procedure setDraggingDestinationFeedbackStyle(newValue: NSTableViewDraggingDestinationFeedbackStyle); message 'setDraggingDestinationFeedbackStyle:';
    function draggingDestinationFeedbackStyle: NSTableViewDraggingDestinationFeedbackStyle; message 'draggingDestinationFeedbackStyle';
    function rectOfColumn (column: NSInteger): NSRect; message 'rectOfColumn:';
    function rectOfRow (row: NSInteger): NSRect; message 'rectOfRow:';
    function columnIndexesInRect (rect: NSRect): NSIndexSet; message 'columnIndexesInRect:'; { available in 10_5 }
    function rowsInRect (rect: NSRect): NSRange; message 'rowsInRect:';
    function columnAtPoint (point: NSPoint): NSInteger; message 'columnAtPoint:';
    function rowAtPoint (point: NSPoint): NSInteger; message 'rowAtPoint:';
    function frameOfCellAtColumn_row (column: NSInteger; row: NSInteger): NSRect; message 'frameOfCellAtColumn:row:';
    procedure setAutosaveName(newValue: NSString); message 'setAutosaveName:';
    function autosaveName: NSString; message 'autosaveName';
    procedure setAutosaveTableColumns(newValue: ObjCBOOL); message 'setAutosaveTableColumns:';
    function autosaveTableColumns: ObjCBOOL; message 'autosaveTableColumns';
    procedure editColumn_row_withEvent_select (column: NSInteger; row: NSInteger; theEvent: NSEvent; select: ObjCBOOL); message 'editColumn:row:withEvent:select:';
    procedure drawRow_clipRect (row: NSInteger; clipRect: NSRect); message 'drawRow:clipRect:';
    procedure highlightSelectionInClipRect (clipRect: NSRect); message 'highlightSelectionInClipRect:';
    procedure drawGridInClipRect (clipRect: NSRect); message 'drawGridInClipRect:';
    procedure drawBackgroundInClipRect (clipRect: NSRect); message 'drawBackgroundInClipRect:';
    function viewAtColumn_row_makeIfNecessary (column: NSInteger; row: NSInteger; makeIfNecessary: ObjCBOOL): id; message 'viewAtColumn:row:makeIfNecessary:'; { available in 10_7 }
    function rowViewAtRow_makeIfNecessary (row: NSInteger; makeIfNecessary: ObjCBOOL): id; message 'rowViewAtRow:makeIfNecessary:'; { available in 10_7 }
    function rowForView (view: NSView): NSInteger; message 'rowForView:'; { available in 10_7 }
    function columnForView (view: NSView): NSInteger; message 'columnForView:'; { available in 10_7 }
    function makeViewWithIdentifier_owner (identifier: NSString; owner: id): id; message 'makeViewWithIdentifier:owner:'; { available in 10_7 }
    procedure enumerateAvailableRowViewsUsingBlock (handler: OpaqueCBlock); message 'enumerateAvailableRowViewsUsingBlock:'; { available in 10_7 }
    procedure setFloatsGroupRows(newValue: ObjCBOOL); message 'setFloatsGroupRows:';
    function floatsGroupRows: ObjCBOOL; message 'floatsGroupRows';
    procedure beginUpdates; message 'beginUpdates'; { available in 10_7 }
    procedure endUpdates; message 'endUpdates'; { available in 10_7 }
    procedure insertRowsAtIndexes_withAnimation (indexes: NSIndexSet; animationOptions: NSTableViewAnimationOptions); message 'insertRowsAtIndexes:withAnimation:'; { available in 10_7 }
    procedure removeRowsAtIndexes_withAnimation (indexes: NSIndexSet; animationOptions: NSTableViewAnimationOptions); message 'removeRowsAtIndexes:withAnimation:'; { available in 10_7 }
    procedure moveRowAtIndex_toIndex (oldIndex: NSInteger; newIndex: NSInteger); message 'moveRowAtIndex:toIndex:'; { available in 10_7 }
    procedure registerNib_forIdentifier (nib: NSNib; identifier: NSString); message 'registerNib:forIdentifier:'; { available in 10_8 }
    function registeredNibsByIdentifier: NSDictionary; message 'registeredNibsByIdentifier';
    procedure didAddRowView_forRow (rowView: NSTableRowView; row: NSInteger); message 'didAddRowView:forRow:'; { available in 10_7 }
    procedure didRemoveRowView_forRow (rowView: NSTableRowView; row: NSInteger); message 'didRemoveRowView:forRow:'; { available in 10_7 }
    procedure setUsesStaticContents(newValue: ObjCBOOL); message 'setUsesStaticContents:';
    function usesStaticContents: ObjCBOOL; message 'usesStaticContents';

    { Adopted protocols }
    function accessibilityHeaderGroup: NSString; message 'accessibilityHeaderGroup';
    procedure draggingSession_endedAtPoint_operation (session: NSDraggingSession; screenPoint: NSPoint; operation: NSDragOperation); message 'draggingSession:endedAtPoint:operation:';
    procedure draggingSession_movedToPoint (session: NSDraggingSession; screenPoint: NSPoint); message 'draggingSession:movedToPoint:';
    function draggingSession_sourceOperationMaskForDraggingContext (session: NSDraggingSession; context: NSDraggingContext): NSDragOperation; message 'draggingSession:sourceOperationMaskForDraggingContext:';
    procedure draggingSession_willBeginAtPoint (session: NSDraggingSession; screenPoint: NSPoint); message 'draggingSession:willBeginAtPoint:';
    function ignoreModifierKeysForDraggingSession (session: NSDraggingSession): ObjCBOOL; message 'ignoreModifierKeysForDraggingSession:';
    procedure setAccessibilitySelectedRows (selectedRows: NSArray); message 'setAccessibilitySelectedRows:';
    procedure textDidBeginEditing (notification: NSNotification); message 'textDidBeginEditing:';
    procedure textDidChange (notification: NSNotification); message 'textDidChange:';
    procedure textDidEndEditing (notification: NSNotification); message 'textDidEndEditing:';
    function textShouldBeginEditing (textObject: NSText): ObjCBOOL; message 'textShouldBeginEditing:';
    function textShouldEndEditing (textObject: NSText): ObjCBOOL; message 'textShouldEndEditing:';
    procedure textView_clickedOnCell_inRect (textView: NSTextView; cell: NSTextAttachmentCellProtocol; cellFrame: NSRect); message 'textView:clickedOnCell:inRect:'; deprecated 'in 10_0, 10_6, "Use -textView:clickedOnCell:inRect:atIndex: instead"';
    procedure textView_clickedOnCell_inRect_atIndex (textView: NSTextView; cell: NSTextAttachmentCellProtocol; cellFrame: NSRect; charIndex: NSUInteger); message 'textView:clickedOnCell:inRect:atIndex:';
    function textView_clickedOnLink (textView: NSTextView; link: id): ObjCBOOL; message 'textView:clickedOnLink:'; deprecated 'in 10_0, 10_6, "Use -textView:clickedOnLink:atIndex: instead"';
    function textView_clickedOnLink_atIndex (textView: NSTextView; link: id; charIndex: NSUInteger): ObjCBOOL; message 'textView:clickedOnLink:atIndex:';
    function textView_completions_forPartialWordRange_indexOfSelectedItem (textView: NSTextView; words: NSArray; charRange: NSRange; index: NSIntegerPtr): NSArray; message 'textView:completions:forPartialWordRange:indexOfSelectedItem:';
    function textView_didCheckTextInRange_types_options_results_orthography_wordCount (view: NSTextView; range: NSRange; checkingTypes: NSTextCheckingTypes; options: NSDictionary; results: NSArray; orthography: NSOrthography; wordCount: NSInteger): NSArray; message 'textView:didCheckTextInRange:types:options:results:orthography:wordCount:'; { available in 10_6 }
    function textView_doCommandBySelector (textView: NSTextView; commandSelector: SEL): ObjCBOOL; message 'textView:doCommandBySelector:';
    procedure textView_doubleClickedOnCell_inRect (textView: NSTextView; cell: NSTextAttachmentCellProtocol; cellFrame: NSRect); message 'textView:doubleClickedOnCell:inRect:'; deprecated 'in 10_0, 10_6, "Use -textView:doubleClickedOnCell:inRect:atIndex: instead"';
    procedure textView_doubleClickedOnCell_inRect_atIndex (textView: NSTextView; cell: NSTextAttachmentCellProtocol; cellFrame: NSRect; charIndex: NSUInteger); message 'textView:doubleClickedOnCell:inRect:atIndex:';
    procedure textView_draggedCell_inRect_event (view: NSTextView; cell: NSTextAttachmentCellProtocol; rect: NSRect; event: NSEvent); message 'textView:draggedCell:inRect:event:'; deprecated 'in 10_0, 10_6, "Use -textView:draggedCell:inRect:event:atIndex: instead"';
    procedure textView_draggedCell_inRect_event_atIndex (view: NSTextView; cell: NSTextAttachmentCellProtocol; rect: NSRect; event: NSEvent; charIndex: NSUInteger); message 'textView:draggedCell:inRect:event:atIndex:';
    function textView_menu_forEvent_atIndex (view: NSTextView; menu: NSMenu; event: NSEvent; charIndex: NSUInteger): NSMenu; message 'textView:menu:forEvent:atIndex:'; { available in 10_5 }
    function textView_shouldChangeTextInRange_replacementString (textView: NSTextView; affectedCharRange: NSRange; replacementString: NSString): ObjCBOOL; message 'textView:shouldChangeTextInRange:replacementString:';
    function textView_shouldChangeTextInRanges_replacementStrings (textView: NSTextView; affectedRanges: NSArray; replacementStrings: NSArray): ObjCBOOL; message 'textView:shouldChangeTextInRanges:replacementStrings:';
    function textView_shouldChangeTypingAttributes_toAttributes (textView: NSTextView; oldTypingAttributes: NSDictionary; newTypingAttributes: NSDictionary): NSDictionary; message 'textView:shouldChangeTypingAttributes:toAttributes:';
    function textView_shouldSetSpellingState_range (textView: NSTextView; value: NSInteger; affectedCharRange: NSRange): NSInteger; message 'textView:shouldSetSpellingState:range:'; { available in 10_5 }
    function textView_URLForContentsOfTextAttachment_atIndex (textView: NSTextView; textAttachment: NSTextAttachment; charIndex: NSUInteger): NSURL; message 'textView:URLForContentsOfTextAttachment:atIndex:'; { available in 10_7 }
    function textView_willChangeSelectionFromCharacterRange_toCharacterRange (textView: NSTextView; oldSelectedCharRange: NSRange; newSelectedCharRange: NSRange): NSRange; message 'textView:willChangeSelectionFromCharacterRange:toCharacterRange:';
    function textView_willChangeSelectionFromCharacterRanges_toCharacterRanges (textView: NSTextView; oldSelectedCharRanges: NSArray; newSelectedCharRanges: NSArray): NSArray; message 'textView:willChangeSelectionFromCharacterRanges:toCharacterRanges:';
    function textView_willCheckTextInRange_options_types (view: NSTextView; range: NSRange; options: NSDictionary; checkingTypes: NSTextCheckingTypesPtr): NSDictionary; message 'textView:willCheckTextInRange:options:types:'; { available in 10_6 }
    function textView_willDisplayToolTip_forCharacterAtIndex (textView: NSTextView; tooltip: NSString; characterIndex: NSUInteger): NSString; message 'textView:willDisplayToolTip:forCharacterAtIndex:';
    function textView_willShowSharingServicePicker_forItems (textView: NSTextView; servicePicker: NSSharingServicePicker; items: NSArray): NSSharingServicePicker; message 'textView:willShowSharingServicePicker:forItems:'; { available in 10_8 }
    function textView_writablePasteboardTypesForCell_atIndex (view: NSTextView; cell: NSTextAttachmentCellProtocol; charIndex: NSUInteger): NSArray; message 'textView:writablePasteboardTypesForCell:atIndex:';
    function textView_writeCell_atIndex_toPasteboard_type (view: NSTextView; cell: NSTextAttachmentCellProtocol; charIndex: NSUInteger; pboard: NSPasteboard; type_: NSString): ObjCBOOL; message 'textView:writeCell:atIndex:toPasteboard:type:';
    procedure textViewDidChangeSelection (notification: NSNotification); message 'textViewDidChangeSelection:';
    procedure textViewDidChangeTypingAttributes (notification: NSNotification); message 'textViewDidChangeTypingAttributes:';
    function undoManagerForTextView (view: NSTextView): NSUndoManager; message 'undoManagerForTextView:';
    function validateUserInterfaceItem (anItem: NSValidatedUserInterfaceItemProtocol): ObjCBOOL; message 'validateUserInterfaceItem:';
  end;
{$endif}

{$ifdef PROTOCOLS}

type
  NSTableViewDelegateProtocol = objcprotocol external name 'NSTableViewDelegate' (NSControlTextEditingDelegateProtocol)
  optional
    function tableView_viewForTableColumn_row (tableView: NSTableView; tableColumn: NSTableColumn; row: NSInteger): NSView; message 'tableView:viewForTableColumn:row:'; { available in 10_7 }
    function tableView_rowViewForRow (tableView: NSTableView; row: NSInteger): NSTableRowView; message 'tableView:rowViewForRow:'; { available in 10_7 }
    procedure tableView_didAddRowView_forRow (tableView: NSTableView; rowView: NSTableRowView; row: NSInteger); message 'tableView:didAddRowView:forRow:'; { available in 10_7 }
    procedure tableView_didRemoveRowView_forRow (tableView: NSTableView; rowView: NSTableRowView; row: NSInteger); message 'tableView:didRemoveRowView:forRow:'; { available in 10_7 }
    procedure tableView_willDisplayCell_forTableColumn_row (tableView: NSTableView; cell: id; tableColumn: NSTableColumn; row: NSInteger); message 'tableView:willDisplayCell:forTableColumn:row:';
    function tableView_shouldEditTableColumn_row (tableView: NSTableView; tableColumn: NSTableColumn; row: NSInteger): ObjCBOOL; message 'tableView:shouldEditTableColumn:row:';
    function tableView_toolTipForCell_rect_tableColumn_row_mouseLocation (tableView: NSTableView; cell: NSCell; rect: NSRectPointer; tableColumn: NSTableColumn; row: NSInteger; mouseLocation: NSPoint): NSString; message 'tableView:toolTipForCell:rect:tableColumn:row:mouseLocation:';
    function tableView_shouldShowCellExpansionForTableColumn_row (tableView: NSTableView; tableColumn: NSTableColumn; row: NSInteger): ObjCBOOL; message 'tableView:shouldShowCellExpansionForTableColumn:row:'; { available in 10_5 }
    function tableView_shouldTrackCell_forTableColumn_row (tableView: NSTableView; cell: NSCell; tableColumn: NSTableColumn; row: NSInteger): ObjCBOOL; message 'tableView:shouldTrackCell:forTableColumn:row:'; { available in 10_5 }
    function tableView_dataCellForTableColumn_row (tableView: NSTableView; tableColumn: NSTableColumn; row: NSInteger): NSCell; message 'tableView:dataCellForTableColumn:row:'; { available in 10_5 }
    function selectionShouldChangeInTableView (tableView: NSTableView): ObjCBOOL; message 'selectionShouldChangeInTableView:';
    function tableView_shouldSelectRow (tableView: NSTableView; row: NSInteger): ObjCBOOL; message 'tableView:shouldSelectRow:';
    function tableView_selectionIndexesForProposedSelection (tableView: NSTableView; proposedSelectionIndexes: NSIndexSet): NSIndexSet; message 'tableView:selectionIndexesForProposedSelection:'; { available in 10_5 }
    function tableView_shouldSelectTableColumn (tableView: NSTableView; tableColumn: NSTableColumn): ObjCBOOL; message 'tableView:shouldSelectTableColumn:';
    procedure tableView_mouseDownInHeaderOfTableColumn (tableView: NSTableView; tableColumn: NSTableColumn); message 'tableView:mouseDownInHeaderOfTableColumn:';
    procedure tableView_didClickTableColumn (tableView: NSTableView; tableColumn: NSTableColumn); message 'tableView:didClickTableColumn:';
    procedure tableView_didDragTableColumn (tableView: NSTableView; tableColumn: NSTableColumn); message 'tableView:didDragTableColumn:';
    function tableView_heightOfRow (tableView: NSTableView; row: NSInteger): CGFloat; message 'tableView:heightOfRow:';
    function tableView_typeSelectStringForTableColumn_row (tableView: NSTableView; tableColumn: NSTableColumn; row: NSInteger): NSString; message 'tableView:typeSelectStringForTableColumn:row:'; { available in 10_5 }
    function tableView_nextTypeSelectMatchFromRow_toRow_forString (tableView: NSTableView; startRow: NSInteger; endRow: NSInteger; searchString: NSString): NSInteger; message 'tableView:nextTypeSelectMatchFromRow:toRow:forString:'; { available in 10_5 }
    function tableView_shouldTypeSelectForEvent_withCurrentSearchString (tableView: NSTableView; event: NSEvent; searchString: NSString): ObjCBOOL; message 'tableView:shouldTypeSelectForEvent:withCurrentSearchString:'; { available in 10_5 }
    function tableView_isGroupRow (tableView: NSTableView; row: NSInteger): ObjCBOOL; message 'tableView:isGroupRow:'; { available in 10_5 }
    function tableView_sizeToFitWidthOfColumn (tableView: NSTableView; column: NSInteger): CGFloat; message 'tableView:sizeToFitWidthOfColumn:'; { available in 10_6 }
    function tableView_shouldReorderColumn_toColumn (tableView: NSTableView; columnIndex: NSInteger; newColumnIndex: NSInteger): ObjCBOOL; message 'tableView:shouldReorderColumn:toColumn:'; { available in 10_6 }
    procedure tableViewSelectionDidChange (notification: NSNotification); message 'tableViewSelectionDidChange:';
    procedure tableViewColumnDidMove (notification: NSNotification); message 'tableViewColumnDidMove:';
    procedure tableViewColumnDidResize (notification: NSNotification); message 'tableViewColumnDidResize:';
    procedure tableViewSelectionIsChanging (notification: NSNotification); message 'tableViewSelectionIsChanging:';
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSTableViewSelectionDidChangeNotification: NSString; cvar; external;
  NSTableViewColumnDidMoveNotification: NSString; cvar; external;
  NSTableViewColumnDidResizeNotification: NSString; cvar; external;
  NSTableViewSelectionIsChangingNotification: NSString; cvar; external;
  NSTableViewRowViewKey: NSString { available in 10_7 }; cvar; external;
{$endif}

{$ifdef PROTOCOLS}

type
  NSTableViewDataSourceProtocol = objcprotocol external name 'NSTableViewDataSource' (NSObjectProtocol)
  optional
    function numberOfRowsInTableView (tableView: NSTableView): NSInteger; message 'numberOfRowsInTableView:';
    function tableView_objectValueForTableColumn_row (tableView: NSTableView; tableColumn: NSTableColumn; row: NSInteger): id; message 'tableView:objectValueForTableColumn:row:';
    procedure tableView_setObjectValue_forTableColumn_row (tableView: NSTableView; object_: id; tableColumn: NSTableColumn; row: NSInteger); message 'tableView:setObjectValue:forTableColumn:row:';
    procedure tableView_sortDescriptorsDidChange (tableView: NSTableView; oldDescriptors: NSArray); message 'tableView:sortDescriptorsDidChange:';
    function tableView_pasteboardWriterForRow (tableView: NSTableView; row: NSInteger): NSPasteboardWritingProtocol; message 'tableView:pasteboardWriterForRow:'; { available in 10_7 }
    procedure tableView_draggingSession_willBeginAtPoint_forRowIndexes (tableView: NSTableView; session: NSDraggingSession; screenPoint: NSPoint; rowIndexes: NSIndexSet); message 'tableView:draggingSession:willBeginAtPoint:forRowIndexes:'; { available in 10_7 }
    procedure tableView_draggingSession_endedAtPoint_operation (tableView: NSTableView; session: NSDraggingSession; screenPoint: NSPoint; operation: NSDragOperation); message 'tableView:draggingSession:endedAtPoint:operation:'; { available in 10_7 }
    procedure tableView_updateDraggingItemsForDrag (tableView: NSTableView; draggingInfo: NSDraggingInfoProtocol); message 'tableView:updateDraggingItemsForDrag:'; { available in 10_7 }
    function tableView_writeRowsWithIndexes_toPasteboard (tableView: NSTableView; rowIndexes: NSIndexSet; pboard: NSPasteboard): ObjCBOOL; message 'tableView:writeRowsWithIndexes:toPasteboard:';
    function tableView_validateDrop_proposedRow_proposedDropOperation (tableView: NSTableView; info: NSDraggingInfoProtocol; row: NSInteger; dropOperation: NSTableViewDropOperation): NSDragOperation; message 'tableView:validateDrop:proposedRow:proposedDropOperation:';
    function tableView_acceptDrop_row_dropOperation (tableView: NSTableView; info: NSDraggingInfoProtocol; row: NSInteger; dropOperation: NSTableViewDropOperation): ObjCBOOL; message 'tableView:acceptDrop:row:dropOperation:';
    function tableView_namesOfPromisedFilesDroppedAtDestination_forDraggedRowsWithIndexes (tableView: NSTableView; dropDestination: NSURL; indexSet: NSIndexSet): NSArray; message 'tableView:namesOfPromisedFilesDroppedAtDestination:forDraggedRowsWithIndexes:';
  end;
{$endif}

{$ifdef CLASSES}

type
  NSTableViewDataSourceDeprecated = objccategory external (NSObject)
    function tableView_writeRows_toPasteboard (tableView: NSTableView; rows: NSArray; pboard: NSPasteboard): ObjCBOOL; message 'tableView:writeRows:toPasteboard:'; deprecated 'in 10_0, 10_4';
  end;

type
  NSTableView_NSDeprecated = objccategory external name 'NSDeprecated' (NSTableView)
    procedure setDrawsGrid (flag: ObjCBOOL); message 'setDrawsGrid:'; deprecated 'in 10_0, 10_3';
    function drawsGrid: ObjCBOOL; message 'drawsGrid'; deprecated 'in 10_0, 10_3';
    procedure selectColumn_byExtendingSelection (column: NSInteger; extend: ObjCBOOL); message 'selectColumn:byExtendingSelection:'; deprecated 'in 10_0, 10_3';
    procedure selectRow_byExtendingSelection (row: NSInteger; extend: ObjCBOOL); message 'selectRow:byExtendingSelection:'; deprecated 'in 10_0, 10_3';
    function selectedColumnEnumerator: NSEnumerator; message 'selectedColumnEnumerator'; deprecated 'in 10_0, 10_3';
    function selectedRowEnumerator: NSEnumerator; message 'selectedRowEnumerator'; deprecated 'in 10_0, 10_3';
    function dragImageForRows_event_dragImageOffset (dragRows: NSArray; dragEvent: NSEvent; dragImageOffset: NSPointPointer): NSImage; message 'dragImageForRows:event:dragImageOffset:'; deprecated 'in 10_0, 10_4';
    procedure setAutoresizesAllColumnsToFit (flag: ObjCBOOL); message 'setAutoresizesAllColumnsToFit:'; deprecated 'in 10_0, 10_4';
    function autoresizesAllColumnsToFit: ObjCBOOL; message 'autoresizesAllColumnsToFit'; deprecated 'in 10_0, 10_4';
    function columnsInRect (rect: NSRect): NSRange; message 'columnsInRect:'; deprecated 'in 10_0, 10_5';
    function preparedCellAtColumn_row (column: NSInteger; row: NSInteger): NSCell; message 'preparedCellAtColumn:row:'; deprecated 'in 10_5, 10_10, "Use View Based TableView and -viewAtColumn:row:"';
    function textShouldBeginEditing (textObject: NSText): ObjCBOOL; message 'textShouldBeginEditing:'; deprecated 'in 10_0, 10_10, "Use a View Based TableView with an NSTextField"';
    function textShouldEndEditing (textObject: NSText): ObjCBOOL; message 'textShouldEndEditing:'; deprecated 'in 10_0, 10_10, "Use a View Based TableView with an NSTextField"';
    procedure textDidBeginEditing (notification: NSNotification); message 'textDidBeginEditing:'; deprecated 'in 10_0, 10_10, "Use a View Based TableView with an NSTextField"';
    procedure textDidEndEditing (notification: NSNotification); message 'textDidEndEditing:'; deprecated 'in 10_0, 10_10, "Use a View Based TableView with an NSTextField"';
    procedure textDidChange (notification: NSNotification); message 'textDidChange:'; deprecated 'in 10_0, 10_10, "Use a View Based TableView with an NSTextField"';
    function shouldFocusCell_atColumn_row (cell: NSCell; column: NSInteger; row: NSInteger): ObjCBOOL; message 'shouldFocusCell:atColumn:row:'; deprecated 'in 10_6, 10_10, "Use a View Based TableView; observe the window’s firstResponder for focus change notifications"';
    function focusedColumn: NSInteger; message 'focusedColumn'; deprecated 'in 10_5, 10_10, "Use a View Based TableView and observe the window.firstResponder"';
    procedure setFocusedColumn (focusedColumn_: NSInteger); message 'setFocusedColumn:'; deprecated 'in 10_6, 10_10, "Use a View Based TableView; make a particular view the first responder with [window makeFirstResponder:view] to focus it."';
    procedure performClickOnCellAtColumn_row (column: NSInteger; row: NSInteger); message 'performClickOnCellAtColumn:row:'; deprecated 'in 10_6, 10_10, "Use a View Based TableView; directly interact with a particular view as required and call -performClick: on it, if necessary"';
  end;
{$endif}

