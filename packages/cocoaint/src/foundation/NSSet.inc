{ Parsed from Foundation.framework NSSet.h }


{$ifdef TYPES}
type
  NSSetPtr = ^NSSet;
  NSMutableSetPtr = ^NSMutableSet;
  NSCountedSetPtr = ^NSCountedSet;
{$endif}

{$ifdef CLASSES}

type
  NSSet = objcclass external (NSObject, NSCopyingProtocol, NSMutableCopyingProtocol, NSSecureCodingProtocol, NSFastEnumerationProtocol)
  public
    function count: NSUInteger; message 'count';
    function member (object_: id): id; message 'member:';
    function objectEnumerator: NSEnumerator; message 'objectEnumerator';
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
  NSExtendedSet = objccategory external (NSSet)
    function allObjects: NSArray; message 'allObjects';
    function anyObject: id; message 'anyObject';
    function containsObject (anObject: id): ObjCBOOL; message 'containsObject:';
    function description: NSString; message 'description';
    function descriptionWithLocale (locale: id): NSString; message 'descriptionWithLocale:';
    function intersectsSet (otherSet: NSSet): ObjCBOOL; message 'intersectsSet:';
    function isEqualToSet (otherSet: NSSet): ObjCBOOL; message 'isEqualToSet:';
    function isSubsetOfSet (otherSet: NSSet): ObjCBOOL; message 'isSubsetOfSet:';
    procedure makeObjectsPerformSelector (aSelector: SEL); message 'makeObjectsPerformSelector:';
    procedure makeObjectsPerformSelector_withObject (aSelector: SEL; argument: id); message 'makeObjectsPerformSelector:withObject:';
    function setByAddingObject (anObject: id): NSSet; message 'setByAddingObject:'; { available in 10_5, 2_0 }
    function setByAddingObjectsFromSet (other: NSSet): NSSet; message 'setByAddingObjectsFromSet:'; { available in 10_5, 2_0 }
    function setByAddingObjectsFromArray (other: NSArray): NSSet; message 'setByAddingObjectsFromArray:'; { available in 10_5, 2_0 }
    procedure enumerateObjectsUsingBlock (block: OpaqueCBlock); message 'enumerateObjectsUsingBlock:'; { available in 10_6, 4_0 }
    procedure enumerateObjectsWithOptions_usingBlock (opts: NSEnumerationOptions; block: OpaqueCBlock); message 'enumerateObjectsWithOptions:usingBlock:'; { available in 10_6, 4_0 }
    function objectsPassingTest (predicate: OpaqueCBlock): NSSet; message 'objectsPassingTest:'; { available in 10_6, 4_0 }
    function objectsWithOptions_passingTest (opts: NSEnumerationOptions; predicate: OpaqueCBlock): NSSet; message 'objectsWithOptions:passingTest:'; { available in 10_6, 4_0 }
  end;

type
  NSSetCreation = objccategory external (NSSet)
    class function set_: instancetype; message 'set';
    class function setWithObject (object_: id): instancetype; message 'setWithObject:';
    class function setWithObjects_count (objects: idPtr { variable size array of id }; cnt: NSUInteger): instancetype; message 'setWithObjects:count:';
    class function setWithObjects (firstObj: id): instancetype; varargs; message 'setWithObjects:';
    class function setWithSet (set__: NSSet): instancetype; message 'setWithSet:';
    class function setWithArray (array_: NSArray): instancetype; message 'setWithArray:';
    function initWithObjects (firstObj: id): instancetype; varargs; message 'initWithObjects:';
    function initWithSet (set__: NSSet): instancetype; message 'initWithSet:';
    function initWithSet_copyItems (set__: NSSet; flag: ObjCBOOL): instancetype; message 'initWithSet:copyItems:';
    function initWithArray (array_: NSArray): instancetype; message 'initWithArray:';
  end;


type
  NSMutableSet = objcclass external (NSSet)
  public
    procedure addObject (object_: id); message 'addObject:';
    procedure removeObject (object_: id); message 'removeObject:';
    function initWithCoder (aDecoder: NSCoder): instancetype; message 'initWithCoder:'; { NS_DESIGNATED_INITIALIZER }
    function init: instancetype; message 'init'; { NS_DESIGNATED_INITIALIZER }
    function initWithCapacity (numItems: NSUInteger): instancetype; message 'initWithCapacity:'; { NS_DESIGNATED_INITIALIZER }
  end;


type
  NSExtendedMutableSet = objccategory external (NSMutableSet)
    procedure addObjectsFromArray (array_: NSArray); message 'addObjectsFromArray:';
    procedure intersectSet (otherSet: NSSet); message 'intersectSet:';
    procedure minusSet (otherSet: NSSet); message 'minusSet:';
    procedure removeAllObjects; message 'removeAllObjects';
    procedure unionSet (otherSet: NSSet); message 'unionSet:';
    procedure setSet (otherSet: NSSet); message 'setSet:';
  end;

type
  NSMutableSetCreation = objccategory external (NSMutableSet)
    class function setWithCapacity (numItems: NSUInteger): instancetype; message 'setWithCapacity:';
  end;


type
  NSCountedSet = objcclass external (NSMutableSet)
  private
    _table: id;
    _reserved: pointer;
  public
    function initWithCapacity (numItems: NSUInteger): instancetype; message 'initWithCapacity:';
    function initWithArray (array_: NSArray): instancetype; message 'initWithArray:';
    function initWithSet (set_: NSSet): instancetype; message 'initWithSet:';
    function countForObject (object_: id): NSUInteger; message 'countForObject:';
    function objectEnumerator: NSEnumerator; message 'objectEnumerator';
    procedure addObject (object_: id); message 'addObject:';
    procedure removeObject (object_: id); message 'removeObject:';
  end;
{$endif}

