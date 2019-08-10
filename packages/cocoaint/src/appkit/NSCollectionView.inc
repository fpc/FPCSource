{ Parsed from AppKit.framework NSCollectionView.h }


{$ifdef TYPES}
type
  NSCollectionViewItemPtr = ^NSCollectionViewItem;
  NSCollectionViewPtr = ^NSCollectionView;
  NSCollectionViewDelegateProtocolPtr = ^NSCollectionViewDelegateProtocol;
{$endif}

{$ifdef CLASSES}

type
  NSCollectionViewItem = objcclass external (NSViewController, NSCopyingProtocol)
  private
    _collectionView: NSCollectionView;
    _cviFlags: bitpacked record
      case byte of
        0: (_anonBitField__cviFlags0: cuint);
        1: (
          isSelected: 0..1;
          suppressSelectionChangedNotification: 0..1;
          connectionsCopied: 0..1;
          reserved: 0..((1 shl 29)-1);
        );
      end;
    _cachedArchive: NSData;
    _reserved2: pointer;
  public
    function collectionView: NSCollectionView; message 'collectionView';
    procedure setSelected(newValue: ObjCBOOL); message 'setSelected:';
    function isSelected: ObjCBOOL; message 'isSelected';
    procedure setImageView(newValue: NSImageView); message 'setImageView:';
    function imageView: NSImageView; message 'imageView';
    procedure setTextField(newValue: NSTextField); message 'setTextField:';
    function textField: NSTextField; message 'textField';
    function draggingImageComponents: NSArray; message 'draggingImageComponents';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
  end;
{$endif}

{$ifdef TYPES}
type
  NSCollectionViewDropOperation = NSInteger;
  NSCollectionViewDropOperationPtr = ^NSCollectionViewDropOperation;

const
  NSCollectionViewDropOn = 0;
  NSCollectionViewDropBefore = 1;
{$endif}

{$ifdef CLASSES}

type
  NSCollectionView = objcclass external (NSView, NSDraggingSourceProtocol, NSDraggingDestinationProtocol)
  private
    _content: NSArray;
    _selectionIndexes: NSMutableIndexSet;
    _itemPrototype: NSCollectionViewItem;
    _minItemSize: NSSize;
    _maxItemSize: NSSize;
    _maxGridRows: NSUInteger;
    _maxGridColumns: NSUInteger;
    _backgroundColors: NSArray;
    _displayedItems: NSMutableArray;
    _animationDuration: NSTimeInterval;
    _cvFlags: bitpacked record
      case byte of
        0: (_anonBitField__cvFlags0: cuint);
        1: (
          isFirstResponder: 0..1;
          invalidateItemViews: 0..1;
          selectable: 0..1;
          allowsMultipleSelection: 0..1;
          avoidsEmptySelection: 0..1;
          superviewIsClipView: 0..1;
          needsUpdateGrid: 0..1;
          needsUpdateBackground: 0..1;
          gridSettingsNeedUpdate: 0..1;
          guardSetFrameSize: 0..1;
          canDisplayItems: 0..1;
          animateForDrag: 0..1;
          unarchiving: 0..1;
          observingScroll: 0..1;
          reserved: 0..((1 shl 18)-1);
        );
      end;
    _delegate: id;
    _backgroundLayers: NSMutableArray;
    _storedFrameSize: NSSize;
    _cachedNib: NSNib;
    _animTimerCancel: pObjCBOOL;
    _removedItems: NSArray;
    _addedItems: NSArray;
    _addedItemIndexes: NSIndexSet;
    _resizeTimerCancel: pObjCBOOL;
    _draggedIndexes: NSIndexSet;
    _draggingSourceOperationMaskForLocal: NSDragOperation;
    _draggingSourceOperationMaskForNonLocal: NSDragOperation;
    _currentDropIndex: NSInteger;
    _currentDropSpaceIndex: NSInteger;
    _shiftRow: NSInteger;
    _currentDragOperation: NSDragOperation;
    _currentDropOperation: NSCollectionViewDropOperation;
    _draggingInfo: id;
    _dragTimerCancel: pObjCBOOL;
    _private: id;
    _reserved:array[0..12] of pointer;
  public
    procedure setDelegate(newValue: NSCollectionViewDelegateProtocol); message 'setDelegate:';
    function delegate: NSCollectionViewDelegateProtocol; message 'delegate';
    function isFirstResponder: ObjCBOOL; message 'isFirstResponder';
    procedure setContent(newValue: NSArray); message 'setContent:';
    function content: NSArray; message 'content';
    procedure setSelectable(newValue: ObjCBOOL); message 'setSelectable:';
    function isSelectable: ObjCBOOL; message 'isSelectable';
    procedure setAllowsMultipleSelection(newValue: ObjCBOOL); message 'setAllowsMultipleSelection:';
    function allowsMultipleSelection: ObjCBOOL; message 'allowsMultipleSelection';
    procedure setSelectionIndexes(newValue: NSIndexSet); message 'setSelectionIndexes:';
    function selectionIndexes: NSIndexSet; message 'selectionIndexes';
    function newItemForRepresentedObject (object_: id): NSCollectionViewItem; message 'newItemForRepresentedObject:';
    procedure setItemPrototype(newValue: NSCollectionViewItem); message 'setItemPrototype:';
    function itemPrototype: NSCollectionViewItem; message 'itemPrototype';
    procedure setMaxNumberOfRows(newValue: NSUInteger); message 'setMaxNumberOfRows:';
    function maxNumberOfRows: NSUInteger; message 'maxNumberOfRows';
    procedure setMaxNumberOfColumns(newValue: NSUInteger); message 'setMaxNumberOfColumns:';
    function maxNumberOfColumns: NSUInteger; message 'maxNumberOfColumns';
    procedure setMinItemSize(newValue: NSSize); message 'setMinItemSize:';
    function minItemSize: NSSize; message 'minItemSize';
    procedure setMaxItemSize(newValue: NSSize); message 'setMaxItemSize:';
    function maxItemSize: NSSize; message 'maxItemSize';
    procedure setBackgroundColors(newValue: NSArray); message 'setBackgroundColors:';
    function backgroundColors: NSArray; message 'backgroundColors';
    function itemAtIndex (index: NSUInteger): NSCollectionViewItem; message 'itemAtIndex:'; { available in 10_6 }
    function frameForItemAtIndex (index: NSUInteger): NSRect; message 'frameForItemAtIndex:'; { available in 10_6 }
    function frameForItemAtIndex_withNumberOfItems (index: NSUInteger; numberOfItems: NSUInteger): NSRect; message 'frameForItemAtIndex:withNumberOfItems:'; { available in 10_7 }
    procedure setDraggingSourceOperationMask_forLocal (dragOperationMask: NSDragOperation; localDestination: ObjCBOOL); message 'setDraggingSourceOperationMask:forLocal:'; { available in 10_6 }
    function draggingImageForItemsAtIndexes_withEvent_offset (indexes: NSIndexSet; event: NSEvent; dragImageOffset: NSPointPointer): NSImage; message 'draggingImageForItemsAtIndexes:withEvent:offset:'; { available in 10_6 }

    { Adopted protocols }
    procedure draggingSession_endedAtPoint_operation (session: NSDraggingSession; screenPoint: NSPoint; operation: NSDragOperation); message 'draggingSession:endedAtPoint:operation:';
    procedure draggingSession_movedToPoint (session: NSDraggingSession; screenPoint: NSPoint); message 'draggingSession:movedToPoint:';
    function draggingSession_sourceOperationMaskForDraggingContext (session: NSDraggingSession; context: NSDraggingContext): NSDragOperation; message 'draggingSession:sourceOperationMaskForDraggingContext:';
    procedure draggingSession_willBeginAtPoint (session: NSDraggingSession; screenPoint: NSPoint); message 'draggingSession:willBeginAtPoint:';
    function ignoreModifierKeysForDraggingSession (session: NSDraggingSession): ObjCBOOL; message 'ignoreModifierKeysForDraggingSession:';
  end;
{$endif}

{$ifdef PROTOCOLS}

type
  NSCollectionViewDelegateProtocol = objcprotocol external name 'NSCollectionViewDelegate' (NSObjectProtocol)
  optional
    function collectionView_canDragItemsAtIndexes_withEvent (collectionView: NSCollectionView; indexes: NSIndexSet; event: NSEvent): ObjCBOOL; message 'collectionView:canDragItemsAtIndexes:withEvent:'; { available in 10_6 }
    function collectionView_writeItemsAtIndexes_toPasteboard (collectionView: NSCollectionView; indexes: NSIndexSet; pasteboard: NSPasteboard): ObjCBOOL; message 'collectionView:writeItemsAtIndexes:toPasteboard:'; { available in 10_6 }
    function collectionView_namesOfPromisedFilesDroppedAtDestination_forDraggedItemsAtIndexes (collectionView: NSCollectionView; dropURL: NSURL; indexes: NSIndexSet): NSArray; message 'collectionView:namesOfPromisedFilesDroppedAtDestination:forDraggedItemsAtIndexes:'; { available in 10_6 }
    function collectionView_draggingImageForItemsAtIndexes_withEvent_offset (collectionView: NSCollectionView; indexes: NSIndexSet; event: NSEvent; dragImageOffset: NSPointPointer): NSImage; message 'collectionView:draggingImageForItemsAtIndexes:withEvent:offset:'; { available in 10_6 }
    function collectionView_validateDrop_proposedIndex_dropOperation (collectionView: NSCollectionView; draggingInfo: NSDraggingInfoProtocol; proposedDropIndex: NSIntegerPtr; proposedDropOperation: NSCollectionViewDropOperationPtr): NSDragOperation; message 'collectionView:validateDrop:proposedIndex:dropOperation:'; { available in 10_6 }
    function collectionView_acceptDrop_index_dropOperation (collectionView: NSCollectionView; draggingInfo: NSDraggingInfoProtocol; index: NSInteger; dropOperation: NSCollectionViewDropOperation): ObjCBOOL; message 'collectionView:acceptDrop:index:dropOperation:'; { available in 10_6 }
    function collectionView_pasteboardWriterForItemAtIndex (collectionView: NSCollectionView; index: NSUInteger): NSPasteboardWritingProtocol; message 'collectionView:pasteboardWriterForItemAtIndex:';
    procedure collectionView_draggingSession_willBeginAtPoint_forItemsAtIndexes (collectionView: NSCollectionView; session: NSDraggingSession; screenPoint: NSPoint; indexes: NSIndexSet); message 'collectionView:draggingSession:willBeginAtPoint:forItemsAtIndexes:';
    procedure collectionView_draggingSession_endedAtPoint_dragOperation (collectionView: NSCollectionView; session: NSDraggingSession; screenPoint: NSPoint; operation: NSDragOperation); message 'collectionView:draggingSession:endedAtPoint:dragOperation:';
    procedure collectionView_updateDraggingItemsForDrag (collectionView: NSCollectionView; draggingInfo: NSDraggingInfoProtocol); message 'collectionView:updateDraggingItemsForDrag:';
  end;
{$endif}

