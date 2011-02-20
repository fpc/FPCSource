{ Parsed from Foundation.framework NSArray.h }

{$ifdef TYPES}
{$ifndef NSARRAY_PAS_T}
{$define NSARRAY_PAS_T}

{ Callbacks }
type
  NSArrayComparator = function (param1: id; param2: id; param3: Pointer): NSInteger; cdecl;


{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSARRAY_PAS_R}
{$define NSARRAY_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSARRAY_PAS_F}
{$define NSARRAY_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSARRAY_PAS_S}
{$define NSARRAY_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSArray = objcclass;
  NSArrayPointer = ^NSArray;
  NSArrayPtr = NSArrayPointer;
  NSMutableArray = objcclass;
  NSMutableArrayPointer = ^NSMutableArray;
  NSMutableArrayPtr = NSMutableArrayPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSARRAY_PAS_C}
{$define NSARRAY_PAS_C}

{ NSArray }
  NSArray = objcclass external (NSObject, NSCopyingProtocol, NSMutableCopyingProtocol, NSCodingProtocol, NSFastEnumerationProtocol)
    
  public
    function count: NSUInteger; message 'count';
    function objectAtIndex(index: NSUInteger): id; message 'objectAtIndex:';

    { Adopted Protocols }
    function copyWithZone(zone_: NSZonePtr): id;
    function mutableCopyWithZone(zone_: NSZonePtr): id;
    procedure encodeWithCoder(aCoder: NSCoder);
    function initWithCoder(aDecoder: NSCoder): id;
    function countByEnumeratingWithState_objects_count(state: NSFastEnumerationStatePtr; stackbuf: idPtr; len: NSUInteger): NSUInteger;
  end;

{ NSMutableArray }
  NSMutableArray = objcclass external (NSArray)
    
  public
    procedure addObject(anObject: id); message 'addObject:';
    procedure insertObject_atIndex(anObject: id; index: NSUInteger); message 'insertObject:atIndex:';
    procedure removeLastObject; message 'removeLastObject';
    procedure removeObjectAtIndex(index: NSUInteger); message 'removeObjectAtIndex:';
    procedure replaceObjectAtIndex_withObject(index: NSUInteger; anObject: id); message 'replaceObjectAtIndex:withObject:';
  end;

{ NSExtendedArrayCategory }
  NSExtendedArrayCategory = objccategory external (NSArray)
    function arrayByAddingObject(anObject: id): NSArray; message 'arrayByAddingObject:';
    function arrayByAddingObjectsFromArray(otherArray: NSArray): NSArray; message 'arrayByAddingObjectsFromArray:';
    function componentsJoinedByString(separator: NSString): NSString; message 'componentsJoinedByString:';
    function containsObject(anObject: id): Boolean; message 'containsObject:';
    function description: NSString; message 'description';
    function descriptionWithLocale(locale: id): NSString; message 'descriptionWithLocale:';
    function descriptionWithLocale_indent(locale: id; level: NSUInteger): NSString; message 'descriptionWithLocale:indent:';
    function firstObjectCommonWithArray(otherArray: NSArray): id; message 'firstObjectCommonWithArray:';
    procedure getObjects_range(objects: idPtr; range: NSRange); message 'getObjects:range:';
    function indexOfObject(anObject: id): NSUInteger; message 'indexOfObject:';
    function indexOfObject_inRange(anObject: id; range: NSRange): NSUInteger; message 'indexOfObject:inRange:';
    function indexOfObjectIdenticalTo(anObject: id): NSUInteger; message 'indexOfObjectIdenticalTo:';
    function indexOfObjectIdenticalTo_inRange(anObject: id; range: NSRange): NSUInteger; message 'indexOfObjectIdenticalTo:inRange:';
    function isEqualToArray(otherArray: NSArray): Boolean; message 'isEqualToArray:';
    function lastObject: id; message 'lastObject';
    function objectEnumerator: NSEnumerator; message 'objectEnumerator';
    function reverseObjectEnumerator: NSEnumerator; message 'reverseObjectEnumerator';
    function sortedArrayHint: NSData; message 'sortedArrayHint';
    function sortedArrayUsingFunction_context(comparator: NSArrayComparator; context: Pointer): NSArray; message 'sortedArrayUsingFunction:context:';
    function sortedArrayUsingFunction_context_hint(comparator: NSArrayComparator; context: Pointer; hint: NSData): NSArray; message 'sortedArrayUsingFunction:context:hint:';
    function sortedArrayUsingSelector(comparator: SEL): NSArray; message 'sortedArrayUsingSelector:';
    function subarrayWithRange(range: NSRange): NSArray; message 'subarrayWithRange:';
    function writeToFile_atomically(path: NSString; useAuxiliaryFile: Boolean): Boolean; message 'writeToFile:atomically:';
    function writeToURL_atomically(url: NSURL; atomically: Boolean): Boolean; message 'writeToURL:atomically:';
    procedure makeObjectsPerformSelector(aSelector: SEL); message 'makeObjectsPerformSelector:';
    procedure makeObjectsPerformSelector_withObject(aSelector: SEL; argument: id); message 'makeObjectsPerformSelector:withObject:';
    function objectsAtIndexes(indexes: NSIndexSet): NSArray; message 'objectsAtIndexes:';
  end;

{ NSArrayCreationCategory }
  NSArrayCreationCategory = objccategory external (NSArray)
    class function array_: id; message 'array';
    class function arrayWithObject(anObject: id): id; message 'arrayWithObject:';
    class function arrayWithObjects_count(objects: NSObjectArrayOfObjectsPtr; cnt: NSUInteger): id; message 'arrayWithObjects:count:';
    class function arrayWithObjects(firstKey: id): id; varargs; message 'arrayWithObjects:';
    class function arrayWithArray(array__: NSArray): id; message 'arrayWithArray:';
    function initWithObjects_count(objects: NSObjectArrayOfObjectsPtr; cnt: NSUInteger): id; message 'initWithObjects:count:';
    function initWithObjects(firstKey: id): id; varargs; message 'initWithObjects:';
    function initWithArray(array__: NSArray): id; message 'initWithArray:';
    function initWithArray_copyItems(array__: NSArray; flag: Boolean): id; message 'initWithArray:copyItems:';
    class function arrayWithContentsOfFile(path: NSString): id; message 'arrayWithContentsOfFile:';
    class function arrayWithContentsOfURL(url: NSURL): id; message 'arrayWithContentsOfURL:';
    function initWithContentsOfFile(path: NSString): id; message 'initWithContentsOfFile:';
    function initWithContentsOfURL(url: NSURL): id; message 'initWithContentsOfURL:';
  end;

{ NSDeprecatedCategory }
  NSDeprecatedCategory = objccategory external (NSArray)
    procedure getObjects(objects: idPtr); message 'getObjects:';
  end;

{ NSExtendedMutableArrayCategory }
  NSExtendedMutableArrayCategory = objccategory external (NSMutableArray)
    procedure addObjectsFromArray(otherArray: NSArray); message 'addObjectsFromArray:';
    procedure exchangeObjectAtIndex_withObjectAtIndex(idx: NSUInteger; idx1: NSUInteger); message 'exchangeObjectAtIndex:withObjectAtIndex:';
    procedure removeAllObjects; message 'removeAllObjects';
    procedure removeObject_inRange(anObject: id; range: NSRange); message 'removeObject:inRange:';
    procedure removeObject(anObject: id); message 'removeObject:';
    procedure removeObjectIdenticalTo_inRange(anObject: id; range: NSRange); message 'removeObjectIdenticalTo:inRange:';
    procedure removeObjectIdenticalTo(anObject: id); message 'removeObjectIdenticalTo:';
    procedure removeObjectsFromIndices_numIndices(indices: NSUIntegerPtr; cnt: NSUInteger); message 'removeObjectsFromIndices:numIndices:'; deprecated 'in Mac OS X 10.6 and later';
    procedure removeObjectsInArray(otherArray: NSArray); message 'removeObjectsInArray:';
    procedure removeObjectsInRange(range: NSRange); message 'removeObjectsInRange:';
    procedure replaceObjectsInRange_withObjectsFromArray_range(range: NSRange; otherArray: NSArray; otherRange: NSRange); message 'replaceObjectsInRange:withObjectsFromArray:range:';
    procedure replaceObjectsInRange_withObjectsFromArray(range: NSRange; otherArray: NSArray); message 'replaceObjectsInRange:withObjectsFromArray:';
    procedure setArray(otherArray: NSArray); message 'setArray:';
    procedure sortUsingFunction_context(compare: NSArrayComparator; context: Pointer); message 'sortUsingFunction:context:';
    procedure sortUsingSelector(comparator: SEL); message 'sortUsingSelector:';
    procedure insertObjects_atIndexes(objects: NSArray; indexes: NSIndexSet); message 'insertObjects:atIndexes:';
    procedure removeObjectsAtIndexes(indexes: NSIndexSet); message 'removeObjectsAtIndexes:';
    procedure replaceObjectsAtIndexes_withObjects(indexes: NSIndexSet; objects: NSArray); message 'replaceObjectsAtIndexes:withObjects:';
  end;

{ NSMutableArrayCreationCategory }
  NSMutableArrayCreationCategory = objccategory external (NSMutableArray)
    class function arrayWithCapacity(numItems: NSUInteger): id; message 'arrayWithCapacity:';
    function initWithCapacity(numItems: NSUInteger): id; message 'initWithCapacity:';
  end;

{$endif}
{$endif}
