{ Parsed from Foundation.framework NSArray.h }


{ Types from NSExtendedArray }
{$ifdef TYPES}

type
  NSBinarySearchingOptions = NSUInteger;
type
  NSBinarySearchingOptionsPtr = ^NSBinarySearchingOptions;

const
  NSBinarySearchingFirstEqual = 1 shl 8;
  NSBinarySearchingLastEqual = 1 shl 9;
  NSBinarySearchingInsertionIndex = 1 shl 10;
{$endif}


{ Callbacks }
{$ifdef TYPES}
type
  NSArraySortedArrayUsingFunctionComparatorCallback = function (param1: id; param2: id; param3: pointer): NSInteger; cdecl;
  NSArraySortUsingFunctionCompareCallback = function (param1: id; param2: id; param3: pointer): NSInteger; cdecl;
{$endif}


{$ifdef TYPES}
type
  NSArrayPtr = ^NSArray;
  NSMutableArrayPtr = ^NSMutableArray;
{$endif}

{$ifdef CLASSES}

type
  NSArray = objcclass external (NSObject, NSCopyingProtocol, NSMutableCopyingProtocol, NSSecureCodingProtocol, NSFastEnumerationProtocol)
  public
    function count: NSUInteger; message 'count';
    function objectAtIndex (index: NSUInteger): id; message 'objectAtIndex:';
    function init: instancetype; message 'init'; { NS_DESIGNATED_INITIALIZER }
    function initWithObjects_count (objects: idPtr { variable size array of id }; cnt: NSUInteger): instancetype; message 'initWithObjects:count:'; { NS_DESIGNATED_INITIALIZER }
    function initWithCoder (aDecoder: NSCoder): instancetype; message 'initWithCoder:'; { NS_DESIGNATED_INITIALIZER }

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    function countByEnumeratingWithState_objects_count (state: NSFastEnumerationStatePtr; buffer: idPtr { variable size array of id }; len: NSUInteger): NSUInteger; message 'countByEnumeratingWithState:objects:count:';
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function mutableCopyWithZone (zone: NSZonePtr): id; message 'mutableCopyWithZone:';
    class function supportsSecureCoding: ObjCBOOL; message 'supportsSecureCoding';
  end;


type
  NSExtendedArray = objccategory external (NSArray)
    function arrayByAddingObject (anObject: id): NSArray; message 'arrayByAddingObject:';
    function arrayByAddingObjectsFromArray (otherArray: NSArray): NSArray; message 'arrayByAddingObjectsFromArray:';
    function componentsJoinedByString (separator: NSString): NSString; message 'componentsJoinedByString:';
    function containsObject (anObject: id): ObjCBOOL; message 'containsObject:';
    function description: NSString; message 'description';
    function descriptionWithLocale (locale: id): NSString; message 'descriptionWithLocale:';
    function descriptionWithLocale_indent (locale: id; level: NSUInteger): NSString; message 'descriptionWithLocale:indent:';
    function firstObjectCommonWithArray (otherArray: NSArray): id; message 'firstObjectCommonWithArray:';
    procedure getObjects_range (objects: idPtr { variable size array of id }; range: NSRange); message 'getObjects:range:';
    function indexOfObject (anObject: id): NSUInteger; message 'indexOfObject:';
    function indexOfObject_inRange (anObject: id; range: NSRange): NSUInteger; message 'indexOfObject:inRange:';
    function indexOfObjectIdenticalTo (anObject: id): NSUInteger; message 'indexOfObjectIdenticalTo:';
    function indexOfObjectIdenticalTo_inRange (anObject: id; range: NSRange): NSUInteger; message 'indexOfObjectIdenticalTo:inRange:';
    function isEqualToArray (otherArray: NSArray): ObjCBOOL; message 'isEqualToArray:';
    function firstObject: id; message 'firstObject';
    function lastObject: id; message 'lastObject';
    function objectEnumerator: NSEnumerator; message 'objectEnumerator';
    function reverseObjectEnumerator: NSEnumerator; message 'reverseObjectEnumerator';
    function sortedArrayHint: NSData; message 'sortedArrayHint';
    function sortedArrayUsingFunction_context (comparator: NSArraySortedArrayUsingFunctionComparatorCallback; context: pointer): NSArray; message 'sortedArrayUsingFunction:context:';
    function sortedArrayUsingFunction_context_hint (comparator: NSArraySortedArrayUsingFunctionComparatorCallback; context: pointer; hint: NSData): NSArray; message 'sortedArrayUsingFunction:context:hint:';
    function sortedArrayUsingSelector (comparator: SEL): NSArray; message 'sortedArrayUsingSelector:';
    function subarrayWithRange (range: NSRange): NSArray; message 'subarrayWithRange:';
    function writeToFile_atomically (path: NSString; useAuxiliaryFile: ObjCBOOL): ObjCBOOL; message 'writeToFile:atomically:';
    function writeToURL_atomically (url: NSURL; atomically: ObjCBOOL): ObjCBOOL; message 'writeToURL:atomically:';
    procedure makeObjectsPerformSelector (aSelector: SEL); message 'makeObjectsPerformSelector:';
    procedure makeObjectsPerformSelector_withObject (aSelector: SEL; argument: id); message 'makeObjectsPerformSelector:withObject:';
    function objectsAtIndexes (indexes: NSIndexSet): NSArray; message 'objectsAtIndexes:';
    function objectAtIndexedSubscript (idx: NSUInteger): id; message 'objectAtIndexedSubscript:'; { available in 10_8, 6_0 }
    procedure enumerateObjectsUsingBlock (block: OpaqueCBlock); message 'enumerateObjectsUsingBlock:'; { available in 10_6, 4_0 }
    procedure enumerateObjectsWithOptions_usingBlock (opts: NSEnumerationOptions; block: OpaqueCBlock); message 'enumerateObjectsWithOptions:usingBlock:'; { available in 10_6, 4_0 }
    procedure enumerateObjectsAtIndexes_options_usingBlock (s: NSIndexSet; opts: NSEnumerationOptions; block: OpaqueCBlock); message 'enumerateObjectsAtIndexes:options:usingBlock:'; { available in 10_6, 4_0 }
    function indexOfObjectPassingTest (predicate: OpaqueCBlock): NSUInteger; message 'indexOfObjectPassingTest:'; { available in 10_6, 4_0 }
    function indexOfObjectWithOptions_passingTest (opts: NSEnumerationOptions; predicate: OpaqueCBlock): NSUInteger; message 'indexOfObjectWithOptions:passingTest:'; { available in 10_6, 4_0 }
    function indexOfObjectAtIndexes_options_passingTest (s: NSIndexSet; opts: NSEnumerationOptions; predicate: OpaqueCBlock): NSUInteger; message 'indexOfObjectAtIndexes:options:passingTest:'; { available in 10_6, 4_0 }
    function indexesOfObjectsPassingTest (predicate: OpaqueCBlock): NSIndexSet; message 'indexesOfObjectsPassingTest:'; { available in 10_6, 4_0 }
    function indexesOfObjectsWithOptions_passingTest (opts: NSEnumerationOptions; predicate: OpaqueCBlock): NSIndexSet; message 'indexesOfObjectsWithOptions:passingTest:'; { available in 10_6, 4_0 }
    function indexesOfObjectsAtIndexes_options_passingTest (s: NSIndexSet; opts: NSEnumerationOptions; predicate: OpaqueCBlock): NSIndexSet; message 'indexesOfObjectsAtIndexes:options:passingTest:'; { available in 10_6, 4_0 }
    function sortedArrayUsingComparator (cmptr: NSComparator): NSArray; message 'sortedArrayUsingComparator:'; { available in 10_6, 4_0 }
    function sortedArrayWithOptions_usingComparator (opts: NSSortOptions; cmptr: NSComparator): NSArray; message 'sortedArrayWithOptions:usingComparator:'; { available in 10_6, 4_0 }
    function indexOfObject_inSortedRange_options_usingComparator (obj: id; r: NSRange; opts: NSBinarySearchingOptions; cmp: NSComparator): NSUInteger; message 'indexOfObject:inSortedRange:options:usingComparator:'; { available in 10_6, 4_0 }
  end;

type
  NSArrayCreation = objccategory external (NSArray)
    class function array_: instancetype; message 'array';
    class function arrayWithObject (anObject: id): instancetype; message 'arrayWithObject:';
    class function arrayWithObjects_count (objects: idPtr { variable size array of id }; cnt: NSUInteger): instancetype; message 'arrayWithObjects:count:';
    class function arrayWithObjects (firstObj: id): instancetype; varargs; message 'arrayWithObjects:';
    class function arrayWithArray (array__: NSArray): instancetype; message 'arrayWithArray:';
    function initWithObjects (firstObj: id): instancetype; varargs; message 'initWithObjects:';
    function initWithArray (array__: NSArray): instancetype; message 'initWithArray:';
    function initWithArray_copyItems (array__: NSArray; flag: ObjCBOOL): instancetype; message 'initWithArray:copyItems:';
    class function arrayWithContentsOfFile (path: NSString): NSArray; message 'arrayWithContentsOfFile:';
    class function arrayWithContentsOfURL (url: NSURL): NSArray; message 'arrayWithContentsOfURL:';
    function initWithContentsOfFile (path: NSString): NSArray; message 'initWithContentsOfFile:';
    function initWithContentsOfURL (url: NSURL): NSArray; message 'initWithContentsOfURL:';
  end;

type
  NSArray_NSDeprecated = objccategory external name 'NSDeprecated' (NSArray)
    procedure getObjects (objects: idPtr { variable size array of id }); message 'getObjects:';
  end;


type
  NSMutableArray = objcclass external (NSArray)
  public
    procedure addObject (anObject: id); message 'addObject:';
    procedure insertObject_atIndex (anObject: id; index: NSUInteger); message 'insertObject:atIndex:';
    procedure removeLastObject; message 'removeLastObject';
    procedure removeObjectAtIndex (index: NSUInteger); message 'removeObjectAtIndex:';
    procedure replaceObjectAtIndex_withObject (index: NSUInteger; anObject: id); message 'replaceObjectAtIndex:withObject:';
    function init: instancetype; message 'init'; { NS_DESIGNATED_INITIALIZER }
    function initWithCapacity (numItems: NSUInteger): instancetype; message 'initWithCapacity:'; { NS_DESIGNATED_INITIALIZER }
    function initWithCoder (aDecoder: NSCoder): instancetype; message 'initWithCoder:'; { NS_DESIGNATED_INITIALIZER }
  end;


type
  NSExtendedMutableArray = objccategory external (NSMutableArray)
    procedure addObjectsFromArray (otherArray: NSArray); message 'addObjectsFromArray:';
    procedure exchangeObjectAtIndex_withObjectAtIndex (idx1: NSUInteger; idx2: NSUInteger); message 'exchangeObjectAtIndex:withObjectAtIndex:';
    procedure removeAllObjects; message 'removeAllObjects';
    procedure removeObject_inRange (anObject: id; range: NSRange); message 'removeObject:inRange:';
    procedure removeObject (anObject: id); message 'removeObject:';
    procedure removeObjectIdenticalTo_inRange (anObject: id; range: NSRange); message 'removeObjectIdenticalTo:inRange:';
    procedure removeObjectIdenticalTo (anObject: id); message 'removeObjectIdenticalTo:';
    procedure removeObjectsFromIndices_numIndices (indices: NSUIntegerPtr; cnt: NSUInteger); message 'removeObjectsFromIndices:numIndices:'; deprecated 'in 10_0, 10_6, 2_0, 4_0';
    procedure removeObjectsInArray (otherArray: NSArray); message 'removeObjectsInArray:';
    procedure removeObjectsInRange (range: NSRange); message 'removeObjectsInRange:';
    procedure replaceObjectsInRange_withObjectsFromArray_range (range: NSRange; otherArray: NSArray; otherRange: NSRange); message 'replaceObjectsInRange:withObjectsFromArray:range:';
    procedure replaceObjectsInRange_withObjectsFromArray (range: NSRange; otherArray: NSArray); message 'replaceObjectsInRange:withObjectsFromArray:';
    procedure setArray (otherArray: NSArray); message 'setArray:';
    procedure sortUsingFunction_context (compare: NSArraySortUsingFunctionCompareCallback; context: pointer); message 'sortUsingFunction:context:';
    procedure sortUsingSelector (comparator: SEL); message 'sortUsingSelector:';
    procedure insertObjects_atIndexes (objects: NSArray; indexes: NSIndexSet); message 'insertObjects:atIndexes:';
    procedure removeObjectsAtIndexes (indexes: NSIndexSet); message 'removeObjectsAtIndexes:';
    procedure replaceObjectsAtIndexes_withObjects (indexes: NSIndexSet; objects: NSArray); message 'replaceObjectsAtIndexes:withObjects:';
    procedure setObject_atIndexedSubscript (obj: id; idx: NSUInteger); message 'setObject:atIndexedSubscript:'; { available in 10_8, 6_0 }
    procedure sortUsingComparator (cmptr: NSComparator); message 'sortUsingComparator:'; { available in 10_6, 4_0 }
    procedure sortWithOptions_usingComparator (opts: NSSortOptions; cmptr: NSComparator); message 'sortWithOptions:usingComparator:'; { available in 10_6, 4_0 }
  end;

type
  NSMutableArrayCreation = objccategory external (NSMutableArray)
    class function arrayWithCapacity (numItems: NSUInteger): instancetype; message 'arrayWithCapacity:';
    class function arrayWithContentsOfFile (path: NSString): NSMutableArray; message 'arrayWithContentsOfFile:';
    class function arrayWithContentsOfURL (url: NSURL): NSMutableArray; message 'arrayWithContentsOfURL:';
    function initWithContentsOfFile (path: NSString): NSMutableArray; message 'initWithContentsOfFile:';
    function initWithContentsOfURL (url: NSURL): NSMutableArray; message 'initWithContentsOfURL:';
  end;
{$endif}

