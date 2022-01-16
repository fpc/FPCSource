{ Parsed from AppKit.framework NSOutlineView.h }


{$ifdef TYPES}
type
  NSOutlineViewPtr = ^NSOutlineView;
  NSOutlineViewDataSourceProtocolPtr = ^NSOutlineViewDataSourceProtocol;
  NSOutlineViewDelegateProtocolPtr = ^NSOutlineViewDelegateProtocol;
{$endif}

{$ifdef TYPES}
type
  __OvFlags = bitpacked record
    case byte of
      0: (_anonBitField___OvFlags0: cuint);
      1: (
        {$ifdef __BIG_ENDIAN__}
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
        subclassRowForItem: 0..1;
        selectionAdjustmentDisabled: 0..1;
        autoExpandFlashState: 0..1;
        animateExpandAndCollapse: 0..1;
        delegateHeightOfRowByItem: 0..1;
        delayRowEntryFreeDisabled: 0..1;
        numberOfRowsDataExpandEntered: 0..1;
        validDataSourceMethods: 0..1;
        reloadingData: 0..1;
        dataSourceDraggingEndedAt: 0..1;
        dataSourceDraggedImageMovedTo: 0..1;
        delegateShouldShowOutlineCellForItem: 0..1;
        delegateAutoExpandItem: 0..1;
        delegateAutoCollapseItem: 0..1;
        delegateShouldAutoExpandItem: 0..1;
        flashingEntireRow: 0..1;
        flashingOutlineCell: 0..1;
        dontRedisplayOnFrameChange: 0..1;
        allowAutomaticAnimations: 0..1;
        dataSourceObjectValueByItem: 0..1;
        allItemsLoaded: 0..1;
        {$else}
        allItemsLoaded_: 0..1;
        dataSourceObjectValueByItem_: 0..1;
        allowAutomaticAnimations_: 0..1;
        dontRedisplayOnFrameChange_: 0..1;
        flashingOutlineCell_: 0..1;
        flashingEntireRow_: 0..1;
        delegateShouldAutoExpandItem_: 0..1;
        delegateAutoCollapseItem_: 0..1;
        delegateAutoExpandItem_: 0..1;
        delegateShouldShowOutlineCellForItem_: 0..1;
        dataSourceDraggedImageMovedTo_: 0..1;
        dataSourceDraggingEndedAt_: 0..1;
        reloadingData_: 0..1;
        validDataSourceMethods_: 0..1;
        numberOfRowsDataExpandEntered_: 0..1;
        delayRowEntryFreeDisabled_: 0..1;
        delegateHeightOfRowByItem_: 0..1;
        animateExpandAndCollapse_: 0..1;
        autoExpandFlashState_: 0..1;
        selectionAdjustmentDisabled_: 0..1;
        subclassRowForItem_: 0..1;
        delegateWillDisplayOutlineCell_: 0..1;
        enableExpandNotifications_: 0..1;
        autoSaveExpandItems_: 0..1;
        autoresizesOutlineColumn_: 0..1;
        delegateShouldExpandItem_: 0..1;
        delegateShouldCollapseItem_: 0..1;
        delegateSelectionShouldChangeInOutlineView_: 0..1;
        delegateShouldSelectTableColumn_: 0..1;
        delegateShouldSelectItem_: 0..1;
        delegateShouldEditTableColumn_: 0..1;
        delegateWillDisplayCell_: 0..1;
        {$endif}
      );
    end;
type
  _OVFlags = __OvFlags;
type
  _OVFlagsPtr = ^__OvFlags;
type
  __OvFlagsPtr = ^__OvFlags;

const
  NSOutlineViewDropOnItemIndex = -1;
{$endif}

{$ifdef CLASSES}

type
  NSOutlineView = objcclass external (NSTableView, NSAccessibilityOutlineProtocol)
  private
    _numberOfRows: NSInteger;
    _rowEntryTree: _NSOVRowEntry;
    _itemToEntryMap: NSMapTable;
    _rowEntryArray: CFMutableArrayRef;
    _firstRowIndexDrawn: NSInteger;
    _autoExpandTimerItem: id;
    _outlineTableColumn: NSTableColumn;
    _initedRows: ObjCBOOL;
    _indentationMarkerInCell: ObjCBOOL;
    _indentationPerLevel: NSInteger;
    _outlineCell: NSButtonCell;
    _trackingOutlineFrame: NSRect;
    _expandingItem: id;
    _numberOfChildrenLimit: NSInteger;
    _ovFlags: _OVFlags;
    _ovLock: id;
    _indentArray: pclong;
    _originalWidth: clong;
    _expandSet: id;
    _expandSetToExpandItemsInto: id;
    _indentArraySize: clong;
    _trackingOutlineCell: NSButtonCell;
    _trackingRow: NSInteger;
    _ovReserved: id;
  public
    procedure setDelegate (anObject: NSOutlineViewDelegateProtocol); message 'setDelegate:';
    function delegate: NSOutlineViewDelegateProtocol; message 'delegate';
    procedure setDataSource (aSource: NSOutlineViewDataSourceProtocol); message 'setDataSource:';
    function dataSource: NSOutlineViewDataSourceProtocol; message 'dataSource';
    procedure setOutlineTableColumn(newValue: NSTableColumn); message 'setOutlineTableColumn:';
    function outlineTableColumn: NSTableColumn; message 'outlineTableColumn';
    function isExpandable (item: id): ObjCBOOL; message 'isExpandable:';
    function numberOfChildrenOfItem (item: id): NSInteger; message 'numberOfChildrenOfItem:'; { available in 10_10 }
    function child_ofItem (index: NSInteger; item: id): id; message 'child:ofItem:'; { available in 10_10 }
    procedure expandItem_expandChildren (item: id; expandChildren: ObjCBOOL); message 'expandItem:expandChildren:';
    procedure expandItem (item: id); message 'expandItem:';
    procedure collapseItem_collapseChildren (item: id; collapseChildren: ObjCBOOL); message 'collapseItem:collapseChildren:';
    procedure collapseItem (item: id); message 'collapseItem:';
    procedure reloadItem_reloadChildren (item: id; reloadChildren: ObjCBOOL); message 'reloadItem:reloadChildren:';
    procedure reloadItem (item: id); message 'reloadItem:';
    function parentForItem (item: id): id; message 'parentForItem:';
    function itemAtRow (row: NSInteger): id; message 'itemAtRow:';
    function rowForItem (item: id): NSInteger; message 'rowForItem:';
    function levelForItem (item: id): NSInteger; message 'levelForItem:';
    function levelForRow (row: NSInteger): NSInteger; message 'levelForRow:';
    function isItemExpanded (item: id): ObjCBOOL; message 'isItemExpanded:';
    procedure setIndentationPerLevel(newValue: CGFloat); message 'setIndentationPerLevel:';
    function indentationPerLevel: CGFloat; message 'indentationPerLevel';
    procedure setIndentationMarkerFollowsCell(newValue: ObjCBOOL); message 'setIndentationMarkerFollowsCell:';
    function indentationMarkerFollowsCell: ObjCBOOL; message 'indentationMarkerFollowsCell';
    procedure setAutoresizesOutlineColumn(newValue: ObjCBOOL); message 'setAutoresizesOutlineColumn:';
    function autoresizesOutlineColumn: ObjCBOOL; message 'autoresizesOutlineColumn';
    function frameOfOutlineCellAtRow (row: NSInteger): NSRect; message 'frameOfOutlineCellAtRow:'; { available in 10_5 }
    procedure setDropItem_dropChildIndex (item: id; index: NSInteger); message 'setDropItem:dropChildIndex:';
    function shouldCollapseAutoExpandedItemsForDeposited (deposited: ObjCBOOL): ObjCBOOL; message 'shouldCollapseAutoExpandedItemsForDeposited:';
    procedure setAutosaveExpandedItems(newValue: ObjCBOOL); message 'setAutosaveExpandedItems:';
    function autosaveExpandedItems: ObjCBOOL; message 'autosaveExpandedItems';
    procedure insertItemsAtIndexes_inParent_withAnimation (indexes: NSIndexSet; parent: id; animationOptions: NSTableViewAnimationOptions); message 'insertItemsAtIndexes:inParent:withAnimation:'; { available in 10_7 }
    procedure removeItemsAtIndexes_inParent_withAnimation (indexes: NSIndexSet; parent: id; animationOptions: NSTableViewAnimationOptions); message 'removeItemsAtIndexes:inParent:withAnimation:'; { available in 10_7 }
    procedure moveItemAtIndex_inParent_toIndex_inParent (fromIndex: NSInteger; oldParent: id; toIndex: NSInteger; newParent: id); message 'moveItemAtIndex:inParent:toIndex:inParent:'; { available in 10_7 }
    procedure insertRowsAtIndexes_withAnimation (indexes: NSIndexSet; animationOptions: NSTableViewAnimationOptions); message 'insertRowsAtIndexes:withAnimation:';
    procedure removeRowsAtIndexes_withAnimation (indexes: NSIndexSet; animationOptions: NSTableViewAnimationOptions); message 'removeRowsAtIndexes:withAnimation:';
    procedure moveRowAtIndex_toIndex (oldIndex: NSInteger; newIndex: NSInteger); message 'moveRowAtIndex:toIndex:';
    procedure setUserInterfaceLayoutDirection(newValue: NSUserInterfaceLayoutDirection); message 'setUserInterfaceLayoutDirection:';
    function userInterfaceLayoutDirection: NSUserInterfaceLayoutDirection; message 'userInterfaceLayoutDirection';
  end;
{$endif}

{$ifdef PROTOCOLS}

type
  NSOutlineViewDataSourceProtocol = objcprotocol external name 'NSOutlineViewDataSource' (NSObjectProtocol)
  optional
    function outlineView_numberOfChildrenOfItem (outlineView: NSOutlineView; item: id): NSInteger; message 'outlineView:numberOfChildrenOfItem:';
    function outlineView_child_ofItem (outlineView: NSOutlineView; index: NSInteger; item: id): id; message 'outlineView:child:ofItem:';
    function outlineView_isItemExpandable (outlineView: NSOutlineView; item: id): ObjCBOOL; message 'outlineView:isItemExpandable:';
    function outlineView_objectValueForTableColumn_byItem (outlineView: NSOutlineView; tableColumn: NSTableColumn; item: id): id; message 'outlineView:objectValueForTableColumn:byItem:';
    procedure outlineView_setObjectValue_forTableColumn_byItem (outlineView: NSOutlineView; object_: id; tableColumn: NSTableColumn; item: id); message 'outlineView:setObjectValue:forTableColumn:byItem:';
    function outlineView_itemForPersistentObject (outlineView: NSOutlineView; object_: id): id; message 'outlineView:itemForPersistentObject:';
    function outlineView_persistentObjectForItem (outlineView: NSOutlineView; item: id): id; message 'outlineView:persistentObjectForItem:';
    procedure outlineView_sortDescriptorsDidChange (outlineView: NSOutlineView; oldDescriptors: NSArray); message 'outlineView:sortDescriptorsDidChange:';
    function outlineView_pasteboardWriterForItem (outlineView: NSOutlineView; item: id): NSPasteboardWritingProtocol; message 'outlineView:pasteboardWriterForItem:'; { available in 10_7 }
    procedure outlineView_draggingSession_willBeginAtPoint_forItems (outlineView: NSOutlineView; session: NSDraggingSession; screenPoint: NSPoint; draggedItems: NSArray); message 'outlineView:draggingSession:willBeginAtPoint:forItems:'; { available in 10_7 }
    procedure outlineView_draggingSession_endedAtPoint_operation (outlineView: NSOutlineView; session: NSDraggingSession; screenPoint: NSPoint; operation: NSDragOperation); message 'outlineView:draggingSession:endedAtPoint:operation:'; { available in 10_7 }
    function outlineView_writeItems_toPasteboard (outlineView: NSOutlineView; items: NSArray; pasteboard: NSPasteboard): ObjCBOOL; message 'outlineView:writeItems:toPasteboard:';
    procedure outlineView_updateDraggingItemsForDrag (outlineView: NSOutlineView; draggingInfo: NSDraggingInfoProtocol); message 'outlineView:updateDraggingItemsForDrag:'; { available in 10_7 }
    function outlineView_validateDrop_proposedItem_proposedChildIndex (outlineView: NSOutlineView; info: NSDraggingInfoProtocol; item: id; index: NSInteger): NSDragOperation; message 'outlineView:validateDrop:proposedItem:proposedChildIndex:';
    function outlineView_acceptDrop_item_childIndex (outlineView: NSOutlineView; info: NSDraggingInfoProtocol; item: id; index: NSInteger): ObjCBOOL; message 'outlineView:acceptDrop:item:childIndex:';
    function outlineView_namesOfPromisedFilesDroppedAtDestination_forDraggedItems (outlineView: NSOutlineView; dropDestination: NSURL; items: NSArray): NSArray; message 'outlineView:namesOfPromisedFilesDroppedAtDestination:forDraggedItems:';
  end;

type
  NSOutlineViewDelegateProtocol = objcprotocol external name 'NSOutlineViewDelegate' (NSControlTextEditingDelegateProtocol)
  optional
    function outlineView_viewForTableColumn_item (outlineView: NSOutlineView; tableColumn: NSTableColumn; item: id): NSView; message 'outlineView:viewForTableColumn:item:'; { available in 10_7 }
    function outlineView_rowViewForItem (outlineView: NSOutlineView; item: id): NSTableRowView; message 'outlineView:rowViewForItem:'; { available in 10_7 }
    procedure outlineView_didAddRowView_forRow (outlineView: NSOutlineView; rowView: NSTableRowView; row: NSInteger); message 'outlineView:didAddRowView:forRow:'; { available in 10_7 }
    procedure outlineView_didRemoveRowView_forRow (outlineView: NSOutlineView; rowView: NSTableRowView; row: NSInteger); message 'outlineView:didRemoveRowView:forRow:'; { available in 10_7 }
    procedure outlineView_willDisplayCell_forTableColumn_item (outlineView: NSOutlineView; cell: id; tableColumn: NSTableColumn; item: id); message 'outlineView:willDisplayCell:forTableColumn:item:';
    function outlineView_shouldEditTableColumn_item (outlineView: NSOutlineView; tableColumn: NSTableColumn; item: id): ObjCBOOL; message 'outlineView:shouldEditTableColumn:item:';
    function selectionShouldChangeInOutlineView (outlineView: NSOutlineView): ObjCBOOL; message 'selectionShouldChangeInOutlineView:';
    function outlineView_shouldSelectItem (outlineView: NSOutlineView; item: id): ObjCBOOL; message 'outlineView:shouldSelectItem:';
    function outlineView_selectionIndexesForProposedSelection (outlineView: NSOutlineView; proposedSelectionIndexes: NSIndexSet): NSIndexSet; message 'outlineView:selectionIndexesForProposedSelection:'; { available in 10_5 }
    function outlineView_shouldSelectTableColumn (outlineView: NSOutlineView; tableColumn: NSTableColumn): ObjCBOOL; message 'outlineView:shouldSelectTableColumn:';
    procedure outlineView_mouseDownInHeaderOfTableColumn (outlineView: NSOutlineView; tableColumn: NSTableColumn); message 'outlineView:mouseDownInHeaderOfTableColumn:';
    procedure outlineView_didClickTableColumn (outlineView: NSOutlineView; tableColumn: NSTableColumn); message 'outlineView:didClickTableColumn:';
    procedure outlineView_didDragTableColumn (outlineView: NSOutlineView; tableColumn: NSTableColumn); message 'outlineView:didDragTableColumn:';
    function outlineView_toolTipForCell_rect_tableColumn_item_mouseLocation (outlineView: NSOutlineView; cell: NSCell; rect: NSRectPointer; tableColumn: NSTableColumn; item: id; mouseLocation: NSPoint): NSString; message 'outlineView:toolTipForCell:rect:tableColumn:item:mouseLocation:';
    function outlineView_heightOfRowByItem (outlineView: NSOutlineView; item: id): CGFloat; message 'outlineView:heightOfRowByItem:';
    function outlineView_typeSelectStringForTableColumn_item (outlineView: NSOutlineView; tableColumn: NSTableColumn; item: id): NSString; message 'outlineView:typeSelectStringForTableColumn:item:'; { available in 10_5 }
    function outlineView_nextTypeSelectMatchFromItem_toItem_forString (outlineView: NSOutlineView; startItem: id; endItem: id; searchString: NSString): id; message 'outlineView:nextTypeSelectMatchFromItem:toItem:forString:'; { available in 10_5 }
    function outlineView_shouldTypeSelectForEvent_withCurrentSearchString (outlineView: NSOutlineView; event: NSEvent; searchString: NSString): ObjCBOOL; message 'outlineView:shouldTypeSelectForEvent:withCurrentSearchString:'; { available in 10_5 }
    function outlineView_shouldShowCellExpansionForTableColumn_item (outlineView: NSOutlineView; tableColumn: NSTableColumn; item: id): ObjCBOOL; message 'outlineView:shouldShowCellExpansionForTableColumn:item:'; { available in 10_5 }
    function outlineView_shouldTrackCell_forTableColumn_item (outlineView: NSOutlineView; cell: NSCell; tableColumn: NSTableColumn; item: id): ObjCBOOL; message 'outlineView:shouldTrackCell:forTableColumn:item:'; { available in 10_5 }
    function outlineView_dataCellForTableColumn_item (outlineView: NSOutlineView; tableColumn: NSTableColumn; item: id): NSCell; message 'outlineView:dataCellForTableColumn:item:'; { available in 10_5 }
    function outlineView_isGroupItem (outlineView: NSOutlineView; item: id): ObjCBOOL; message 'outlineView:isGroupItem:'; { available in 10_5 }
    function outlineView_shouldExpandItem (outlineView: NSOutlineView; item: id): ObjCBOOL; message 'outlineView:shouldExpandItem:';
    function outlineView_shouldCollapseItem (outlineView: NSOutlineView; item: id): ObjCBOOL; message 'outlineView:shouldCollapseItem:';
    procedure outlineView_willDisplayOutlineCell_forTableColumn_item (outlineView: NSOutlineView; cell: id; tableColumn: NSTableColumn; item: id); message 'outlineView:willDisplayOutlineCell:forTableColumn:item:';
    function outlineView_sizeToFitWidthOfColumn (outlineView: NSOutlineView; column: NSInteger): CGFloat; message 'outlineView:sizeToFitWidthOfColumn:'; { available in 10_6 }
    function outlineView_shouldReorderColumn_toColumn (outlineView: NSOutlineView; columnIndex: NSInteger; newColumnIndex: NSInteger): ObjCBOOL; message 'outlineView:shouldReorderColumn:toColumn:'; { available in 10_6 }
    function outlineView_shouldShowOutlineCellForItem (outlineView: NSOutlineView; item: id): ObjCBOOL; message 'outlineView:shouldShowOutlineCellForItem:'; { available in 10_6 }
    procedure outlineViewSelectionDidChange (notification: NSNotification); message 'outlineViewSelectionDidChange:';
    procedure outlineViewColumnDidMove (notification: NSNotification); message 'outlineViewColumnDidMove:';
    procedure outlineViewColumnDidResize (notification: NSNotification); message 'outlineViewColumnDidResize:';
    procedure outlineViewSelectionIsChanging (notification: NSNotification); message 'outlineViewSelectionIsChanging:';
    procedure outlineViewItemWillExpand (notification: NSNotification); message 'outlineViewItemWillExpand:';
    procedure outlineViewItemDidExpand (notification: NSNotification); message 'outlineViewItemDidExpand:';
    procedure outlineViewItemWillCollapse (notification: NSNotification); message 'outlineViewItemWillCollapse:';
    procedure outlineViewItemDidCollapse (notification: NSNotification); message 'outlineViewItemDidCollapse:';
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSOutlineViewDisclosureButtonKey: NSString { available in 10_9 }; cvar; external;
  NSOutlineViewShowHideButtonKey: NSString { available in 10_9 }; cvar; external;
  NSOutlineViewSelectionDidChangeNotification: NSString; cvar; external;
  NSOutlineViewColumnDidMoveNotification: NSString; cvar; external;
  NSOutlineViewColumnDidResizeNotification: NSString; cvar; external;
  NSOutlineViewSelectionIsChangingNotification: NSString; cvar; external;
  NSOutlineViewItemWillExpandNotification: NSString; cvar; external;
  NSOutlineViewItemDidExpandNotification: NSString; cvar; external;
  NSOutlineViewItemWillCollapseNotification: NSString; cvar; external;
  NSOutlineViewItemDidCollapseNotification: NSString; cvar; external;
{$endif}

