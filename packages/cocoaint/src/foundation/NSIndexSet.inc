{ Parsed from Foundation.framework NSIndexSet.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Thu Sep 17 14:52:45 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSINDEXSET_PAS_H}
{$define NSINDEXSET_PAS_H}
type
  NSIndexSetPointer = Pointer;
  NSMutableIndexSetPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSINDEXSET_PAS_T}
{$define NSINDEXSET_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSINDEXSET_PAS_R}
{$define NSINDEXSET_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSINDEXSET_PAS_F}
{$define NSINDEXSET_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSINDEXSET_PAS_T}
{$define NSINDEXSET_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSIndexSet = objcclass;
  NSMutableIndexSet = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSINDEXSET_PAS_C}
{$define NSINDEXSET_PAS_C}

{ NSIndexSet }
  NSIndexSet = objcclass(NSObject, NSCopyingProtocol, NSMutableCopyingProtocol, NSCodingProtocol)
  private
    __indexSetFlags: bitpacked record
        _isEmpty: 0..1;
        _hasSingleRange: 0..1;
        _cacheValid: 0..1;
        _reservedArrayBinderController: 0..((1 shl 29)-1);
      end;
    __singleRange: record
        _range: NSRange;
      end;
    __multipleRanges: record
        _data: Pointer; {garbage collector: __strong }
        _reserved: Pointer;
      end;
    __internal: record
      end;
    
  public
    class function alloc: NSIndexSet; message 'alloc';

    class function indexSet: id; message 'indexSet';
    class function indexSetWithIndex(value: NSUInteger): id; message 'indexSetWithIndex:';
    class function indexSetWithIndexesInRange(range: NSRange): id; message 'indexSetWithIndexesInRange:';
    function init: id; message 'init';
    function initWithIndex(value: NSUInteger): id; message 'initWithIndex:';
    function initWithIndexesInRange(range: NSRange): id; message 'initWithIndexesInRange:';
    function initWithIndexSet(indexSet_: NSIndexSet): id; message 'initWithIndexSet:';
    function isEqualToIndexSet(indexSet_: NSIndexSet): Boolean; message 'isEqualToIndexSet:';
    function count: NSUInteger; message 'count';
    function firstIndex: NSUInteger; message 'firstIndex';
    function lastIndex: NSUInteger; message 'lastIndex';
    function indexGreaterThanIndex(value: NSUInteger): NSUInteger; message 'indexGreaterThanIndex:';
    function indexLessThanIndex(value: NSUInteger): NSUInteger; message 'indexLessThanIndex:';
    function indexGreaterThanOrEqualToIndex(value: NSUInteger): NSUInteger; message 'indexGreaterThanOrEqualToIndex:';
    function indexLessThanOrEqualToIndex(value: NSUInteger): NSUInteger; message 'indexLessThanOrEqualToIndex:';
    function getIndexes_maxCount_inIndexRange(indexBuffer: NSUIntegerPointer; bufferSize: NSUInteger; range: NSRangePointer): NSUInteger; message 'getIndexes:maxCount:inIndexRange:';
    function countOfIndexesInRange(range: NSRange): NSUInteger; message 'countOfIndexesInRange:';
    function containsIndex(value: NSUInteger): Boolean; message 'containsIndex:';
    function containsIndexesInRange(range: NSRange): Boolean; message 'containsIndexesInRange:';
    function containsIndexes(indexSet_: NSIndexSet): Boolean; message 'containsIndexes:';
    function intersectsIndexesInRange(range: NSRange): Boolean; message 'intersectsIndexesInRange:';
  end; external;

{ NSMutableIndexSet }
  NSMutableIndexSet = objcclass(NSIndexSet)
  private
    __reserved: Pointer;
    
  public
    class function alloc: NSMutableIndexSet; message 'alloc';

    procedure addIndexes(indexSet_: NSIndexSet); message 'addIndexes:';
    procedure removeIndexes(indexSet_: NSIndexSet); message 'removeIndexes:';
    procedure removeAllIndexes; message 'removeAllIndexes';
    procedure addIndex(value: NSUInteger); message 'addIndex:';
    procedure removeIndex(value: NSUInteger); message 'removeIndex:';
    procedure addIndexesInRange(range: NSRange); message 'addIndexesInRange:';
    procedure removeIndexesInRange(range: NSRange); message 'removeIndexesInRange:';
    procedure shiftIndexesStartingAtIndex_by(index: NSUInteger; delta: NSInteger); message 'shiftIndexesStartingAtIndex:by:';
  end; external;

{$endif}
{$endif}
