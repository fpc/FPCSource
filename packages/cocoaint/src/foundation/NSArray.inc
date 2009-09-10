{ Parsed from Foundation.framework NSArray.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Tue Sep 8 15:30:59 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSARRAY_PAS_H}
{$define NSARRAY_PAS_H}
type
  NSArrayPointer = Pointer;
  NSMutableArrayPointer = Pointer;

{$endif}
{$endif}

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
{$ifndef NSARRAY_PAS_T}
{$define NSARRAY_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSArray = objcclass;
  NSMutableArray = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSARRAY_PAS_C}
{$define NSARRAY_PAS_C}

{ NSArray }
  NSArray = objcclass(NSObject, NSCopyingProtocol, NSMutableCopyingProtocol, NSCodingProtocol, NSFastEnumerationProtocol)
    
  public
    class function alloc: NSArray; message 'alloc';

    function count: culong; message 'count';
    function objectAtIndex(index: culong): id; message 'objectAtIndex:';

    { Category: NSExtendedArray }
    function arrayByAddingObject(anObject: id): NSArray; message 'arrayByAddingObject:';
    function arrayByAddingObjectsFromArray(otherArray: NSArray): NSArray; message 'arrayByAddingObjectsFromArray:';
    function componentsJoinedByString(separator: NSString): NSString; message 'componentsJoinedByString:';
    function containsObject(anObject: id): Boolean; message 'containsObject:';
    function description: NSString; message 'description';
    function descriptionWithLocale(locale: id): NSString; message 'descriptionWithLocale:';
    function descriptionWithLocale_indent(locale: id; level: culong): NSString; message 'descriptionWithLocale:indent:';
    function firstObjectCommonWithArray(otherArray: NSArray): id; message 'firstObjectCommonWithArray:';
    procedure getObjects(objects: id); message 'getObjects:';
    procedure getObjects_range(objects: id; range: NSRange); message 'getObjects:range:';
    function indexOfObject(anObject: id): culong; message 'indexOfObject:';
    function indexOfObject_inRange(anObject: id; range: NSRange): culong; message 'indexOfObject:inRange:';
    function indexOfObjectIdenticalTo(anObject: id): culong; message 'indexOfObjectIdenticalTo:';
    function indexOfObjectIdenticalTo_inRange(anObject: id; range: NSRange): culong; message 'indexOfObjectIdenticalTo:inRange:';
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

    { Category: NSArrayCreation }
    class function array_: id; message 'array';
    class function arrayWithObject(anObject: id): id; message 'arrayWithObject:';
    class function arrayWithObjects_count(objects: NSObjectArrayOfObjectsPtr; cnt: culong): id; message 'arrayWithObjects:count:';
    class function arrayWithObjects(firstObj: id; objParams: array of const): id; message 'arrayWithObjects:';
    class function arrayWithArray(array__: NSArray): id; message 'arrayWithArray:';
    function initWithObjects_count(objects: NSObjectArrayOfObjectsPtr; cnt: culong): id; message 'initWithObjects:count:';
    function initWithObjects(firstObj: id; objParams: array of const): id; message 'initWithObjects:';
    function initWithArray(array__: NSArray): id; message 'initWithArray:';
    function initWithArray_copyItems(array__: NSArray; flag: Boolean): id; message 'initWithArray:copyItems:';
    class function arrayWithContentsOfFile(path: NSString): id; message 'arrayWithContentsOfFile:';
    class function arrayWithContentsOfURL(url: NSURL): id; message 'arrayWithContentsOfURL:';
    function initWithContentsOfFile(path: NSString): id; message 'initWithContentsOfFile:';
    function initWithContentsOfURL(url: NSURL): id; message 'initWithContentsOfURL:';

    { Category: NSKeyValueCoding }
    function valueForKey(key: NSString): id; message 'valueForKey:';
    procedure setValue_forKey(value: id; key: NSString); message 'setValue:forKey:';

    { Category: NSKeyValueObserverRegistration }
    procedure addObserver_toObjectsAtIndexes_forKeyPath_options_context(observer: NSObject; indexes: NSIndexSet; keyPath: NSString; options: NSKeyValueObservingOptions; context: Pointer); message 'addObserver:toObjectsAtIndexes:forKeyPath:options:context:';
    procedure removeObserver_fromObjectsAtIndexes_forKeyPath(observer: NSObject; indexes: NSIndexSet; keyPath: NSString); message 'removeObserver:fromObjectsAtIndexes:forKeyPath:';
    procedure addObserver_forKeyPath_options_context(observer: NSObject; keyPath: NSString; options: NSKeyValueObservingOptions; context: Pointer); message 'addObserver:forKeyPath:options:context:';
    procedure removeObserver_forKeyPath(observer: NSObject; keyPath: NSString); message 'removeObserver:forKeyPath:';

    { Category: NSSortDescriptorSorting }
    function sortedArrayUsingDescriptors(sortDescriptors: NSArray): NSArray; message 'sortedArrayUsingDescriptors:';

    { Category: NSArrayPathExtensions }
    function pathsMatchingExtensions(filterTypes: NSArray): NSArray; message 'pathsMatchingExtensions:';

    { Category: NSPredicateSupport }
    function filteredArrayUsingPredicate(predicate: NSPredicate): NSArray; message 'filteredArrayUsingPredicate:';
  end; external;

{ NSMutableArray }
  NSMutableArray = objcclass(NSArray)
    
  public
    class function alloc: NSMutableArray; message 'alloc';

    procedure addObject(anObject: id); message 'addObject:';
    procedure insertObject_atIndex(anObject: id; index: culong); message 'insertObject:atIndex:';
    procedure removeLastObject; message 'removeLastObject';
    procedure removeObjectAtIndex(index: culong); message 'removeObjectAtIndex:';
    procedure replaceObjectAtIndex_withObject(index: culong; anObject: id); message 'replaceObjectAtIndex:withObject:';

    { Category: NSExtendedMutableArray }
    procedure addObjectsFromArray(otherArray: NSArray); message 'addObjectsFromArray:';
    procedure exchangeObjectAtIndex_withObjectAtIndex(idx: culong; idx1: culong); message 'exchangeObjectAtIndex:withObjectAtIndex:';
    procedure removeAllObjects; message 'removeAllObjects';
    procedure removeObject_inRange(anObject: id; range: NSRange); message 'removeObject:inRange:';
    procedure removeObject(anObject: id); message 'removeObject:';
    procedure removeObjectIdenticalTo_inRange(anObject: id; range: NSRange); message 'removeObjectIdenticalTo:inRange:';
    procedure removeObjectIdenticalTo(anObject: id); message 'removeObjectIdenticalTo:';
    procedure removeObjectsFromIndices_numIndices(var indices: culong; cnt: culong); message 'removeObjectsFromIndices:numIndices:';
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

    { Category: NSMutableArrayCreation }
    class function arrayWithCapacity(numItems: culong): id; message 'arrayWithCapacity:';
    function initWithCapacity(numItems: culong): id; message 'initWithCapacity:';

    { Category: NSSortDescriptorSorting }
    procedure sortUsingDescriptors(sortDescriptors: NSArray); message 'sortUsingDescriptors:';

    { Category: NSPredicateSupport }
    procedure filterUsingPredicate(predicate: NSPredicate); message 'filterUsingPredicate:';
  end; external;

{$endif}
{$endif}
