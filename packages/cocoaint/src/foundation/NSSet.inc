{ Parsed from Foundation.framework NSSet.h }

{$ifdef TYPES}
{$ifndef NSSET_PAS_T}
{$define NSSET_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSSET_PAS_R}
{$define NSSET_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSSET_PAS_F}
{$define NSSET_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSSET_PAS_S}
{$define NSSET_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSSet = objcclass;
  NSSetPointer = ^NSSet;
  NSSetPtr = NSSetPointer;
  NSMutableSet = objcclass;
  NSMutableSetPointer = ^NSMutableSet;
  NSMutableSetPtr = NSMutableSetPointer;
  NSCountedSet = objcclass;
  NSCountedSetPointer = ^NSCountedSet;
  NSCountedSetPtr = NSCountedSetPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSSET_PAS_C}
{$define NSSET_PAS_C}

{ NSSet }
  NSSet = objcclass external (NSObject, NSCopyingProtocol, NSMutableCopyingProtocol, NSCodingProtocol, NSFastEnumerationProtocol)
    
  public
    function count: NSUInteger; message 'count';
    function member(object_: id): id; message 'member:';
    function objectEnumerator: NSEnumerator; message 'objectEnumerator';

    { Adopted Protocols }
    function copyWithZone(zone_: NSZonePtr): id;
    function mutableCopyWithZone(zone_: NSZonePtr): id;
    procedure encodeWithCoder(aCoder: NSCoder);
    function initWithCoder(aDecoder: NSCoder): id;
    function countByEnumeratingWithState_objects_count(state: NSFastEnumerationStatePtr; stackbuf: idPtr; len: NSUInteger): NSUInteger;
  end;

{ NSMutableSet }
  NSMutableSet = objcclass external (NSSet)
    
  public
    procedure addObject(object_: id); message 'addObject:';
    procedure removeObject(object_: id); message 'removeObject:';
  end;

{ NSCountedSet }
  NSCountedSet = objcclass external (NSMutableSet)
  private
    _table: id;
    _reserved: Pointer;
    
  public
    function initWithCapacity(numItems: NSUInteger): id; message 'initWithCapacity:';
    function initWithArray(array_: NSArray): id; message 'initWithArray:';
    function initWithSet(set__: NSSet): id; message 'initWithSet:';
    function countForObject(object_: id): NSUInteger; message 'countForObject:';
    function objectEnumerator: NSEnumerator; message 'objectEnumerator';
    procedure addObject(object_: id); message 'addObject:';
    procedure removeObject(object_: id); message 'removeObject:';
  end;

{ NSExtendedSetCategory }
  NSExtendedSetCategory = objccategory external (NSSet)
    function allObjects: NSArray; message 'allObjects';
    function anyObject: id; message 'anyObject';
    function containsObject(anObject: id): Boolean; message 'containsObject:';
    function description: NSString; message 'description';
    function descriptionWithLocale(locale: id): NSString; message 'descriptionWithLocale:';
    function intersectsSet(otherSet: NSSet): Boolean; message 'intersectsSet:';
    function isEqualToSet(otherSet: NSSet): Boolean; message 'isEqualToSet:';
    function isSubsetOfSet(otherSet: NSSet): Boolean; message 'isSubsetOfSet:';
    procedure makeObjectsPerformSelector(aSelector: SEL); message 'makeObjectsPerformSelector:';
    procedure makeObjectsPerformSelector_withObject(aSelector: SEL; argument: id); message 'makeObjectsPerformSelector:withObject:';
    function setByAddingObject(anObject: id): NSSet; message 'setByAddingObject:';
    function setByAddingObjectsFromSet(other: NSSet): NSSet; message 'setByAddingObjectsFromSet:';
    function setByAddingObjectsFromArray(other: NSArray): NSSet; message 'setByAddingObjectsFromArray:';
  end;

{ NSSetCreationCategory }
  NSSetCreationCategory = objccategory external (NSSet)
    class function set_: id; message 'set';
    class function setWithObject(object_: id): id; message 'setWithObject:';
    class function setWithObjects_count(objects: idPtr; cnt: NSUInteger): id; message 'setWithObjects:count:';
    class function setWithObjects(firstKey: id): id; varargs; message 'setWithObjects:';
    class function setWithSet(set__: NSSet): id; message 'setWithSet:';
    class function setWithArray(array_: NSArray): id; message 'setWithArray:';
    function initWithObjects_count(objects: idPtr; cnt: NSUInteger): id; message 'initWithObjects:count:';
    function initWithObjects(firstKey: id): id; varargs; message 'initWithObjects:';
    function initWithSet(set__: NSSet): id; message 'initWithSet:';
    function initWithSet_copyItems(set__: NSSet; flag: Boolean): id; message 'initWithSet:copyItems:';
    function initWithArray(array_: NSArray): id; message 'initWithArray:';
  end;

{ NSExtendedMutableSetCategory }
  NSExtendedMutableSetCategory = objccategory external (NSMutableSet)
    procedure addObjectsFromArray(array_: NSArray); message 'addObjectsFromArray:';
    procedure intersectSet(otherSet: NSSet); message 'intersectSet:';
    procedure minusSet(otherSet: NSSet); message 'minusSet:';
    procedure removeAllObjects; message 'removeAllObjects';
    procedure unionSet(otherSet: NSSet); message 'unionSet:';
    procedure setSet(otherSet: NSSet); message 'setSet:';
  end;

{ NSMutableSetCreationCategory }
  NSMutableSetCreationCategory = objccategory external (NSMutableSet)
    class function setWithCapacity(numItems: NSUInteger): id; message 'setWithCapacity:';
    function initWithCapacity(numItems: NSUInteger): id; message 'initWithCapacity:';
  end;

{$endif}
{$endif}
