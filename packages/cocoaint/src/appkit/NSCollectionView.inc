{ Parsed from Appkit.framework NSCollectionView.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Mon Sep 28 15:49:53 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSCOLLECTIONVIEW_PAS_H}
{$define NSCOLLECTIONVIEW_PAS_H}
type
  NSCollectionViewItemPointer = Pointer;
  NSCollectionViewPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSCOLLECTIONVIEW_PAS_T}
{$define NSCOLLECTIONVIEW_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSCOLLECTIONVIEW_PAS_R}
{$define NSCOLLECTIONVIEW_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSCOLLECTIONVIEW_PAS_F}
{$define NSCOLLECTIONVIEW_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSCOLLECTIONVIEW_PAS_S}
{$define NSCOLLECTIONVIEW_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSCollectionViewItem = objcclass;
  NSCollectionView = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSCOLLECTIONVIEW_PAS_C}
{$define NSCOLLECTIONVIEW_PAS_C}

{ NSCollectionViewItem }
  NSCollectionViewItem = objcclass(NSObject, NSCopyingProtocol, NSCodingProtocol)
  private
        _reserved: array[0..1] of Pointer;
    __archive: NSMutableData;
    __ownerView: NSCollectionView;
    __representedObject: id;
    __view: NSView;
    __itemFlags: bitpacked record
        _selected: 0..1;
        _removalNeeded: 0..1;
        _suppressSelectionChangeNotification: 0..1;
        _reservedAnimationContainer: 0..((1 shl 29)-1);
      end;
    __targetViewFrameRect: NSRect;
    __appliedViewFrameRect: NSRect;
    __containerReferenceCounter: NSUInteger;
    
  public
    class function alloc: NSCollectionViewItem; message 'alloc';

    function collectionView: NSCollectionView; message 'collectionView';
    procedure setRepresentedObject(object_: id); message 'setRepresentedObject:';
    function representedObject: id; message 'representedObject';
    procedure setView(view_: NSView); message 'setView:';
    function view: NSView; message 'view';
    procedure setSelected(flag: Boolean); message 'setSelected:';
    function isSelected: Boolean; message 'isSelected';
  end; external;

{ NSCollectionView }
  NSCollectionView = objcclass(NSView)
  private
        _reserved: array[0..8] of Pointer;
    __backgroundLayers: NSMutableArray;
    __content: NSArray;
    __selectionIndexes: NSMutableIndexSet;
    __itemPrototype: NSCollectionViewItem;
    __minGridSize: NSSize;
    __maxGridSize: NSSize;
    __minGridRows: NSUInteger;
    __maxGridRows: NSUInteger;
    __minGridColumns: NSUInteger;
    __maxGridColumns: NSUInteger;
    __backgroundColors: NSArray;
    __animationContainerFlags: bitpacked record
        _ignoreFrameSizeChanges: 0..1;
        _selectable: 0..1;
        _allowsMultipleSelection: 0..1;
        _avoidsEmptySelection: 0..1;
        _superviewIsClipView: 0..1;
        _gridParametersReadFromPrototype: 0..1;
        _isFirstResponder: 0..1;
        _reservedAnimationContainer: 0..((1 shl 25)-1);
      end;
    __targetFrameSize: NSSize;
    __targetGridSize: NSSize;
    __targetUnfilledViewSpace: NSSize;
    __targetNumberOfGridRows: NSUInteger;
    __targetNumberOfGridColumns: NSUInteger;
    __targetItems: NSMutableArray;
    __appliedFrameSize: NSSize;
    __appliedGridSize: NSSize;
    __appliedUnfilledViewSpace: NSSize;
    __appliedNumberOfGridRows: NSUInteger;
    __appliedNumberOfGridColumns: NSUInteger;
    __appliedItems: NSMutableArray;
    __appliedItemsAsSet: NSMutableSet;
    __appliedRemovedItemsAsSet: NSMutableSet;
    __animationDuration: NSTimeInterval;
    __animation: NSViewAnimation;
    __hideItems: NSMutableArray;
    __showItems: NSMutableArray;
    
  public
    class function alloc: NSCollectionView; message 'alloc';

    function isFirstResponder: Boolean; message 'isFirstResponder';
    procedure setContent(content_: NSArray); message 'setContent:';
    function content: NSArray; message 'content';
    procedure setSelectable(flag: Boolean); message 'setSelectable:';
    function isSelectable: Boolean; message 'isSelectable';
    procedure setAllowsMultipleSelection(flag: Boolean); message 'setAllowsMultipleSelection:';
    function allowsMultipleSelection: Boolean; message 'allowsMultipleSelection';
    procedure setSelectionIndexes(indexes: NSIndexSet); message 'setSelectionIndexes:';
    function selectionIndexes: NSIndexSet; message 'selectionIndexes';
    function newItemForRepresentedObject(object_: id): NSCollectionViewItem; message 'newItemForRepresentedObject:';
    procedure setItemPrototype(prototype: NSCollectionViewItem); message 'setItemPrototype:';
    function itemPrototype: NSCollectionViewItem; message 'itemPrototype';
    procedure setMaxNumberOfRows(number: NSUInteger); message 'setMaxNumberOfRows:';
    function maxNumberOfRows: NSUInteger; message 'maxNumberOfRows';
    procedure setMaxNumberOfColumns(number: NSUInteger); message 'setMaxNumberOfColumns:';
    function maxNumberOfColumns: NSUInteger; message 'maxNumberOfColumns';
    procedure setMinItemSize(size: NSSize); message 'setMinItemSize:';
    function minItemSize: NSSize; message 'minItemSize';
    procedure setMaxItemSize(size: NSSize); message 'setMaxItemSize:';
    function maxItemSize: NSSize; message 'maxItemSize';
    procedure setBackgroundColors(colors: NSArray); message 'setBackgroundColors:';
    function backgroundColors: NSArray; message 'backgroundColors';
  end; external;

{$endif}
{$endif}
