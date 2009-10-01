{ Parsed from Foundation.framework NSSet.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Thu Oct 1 18:54:06 ICT 2009 }


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
  NSMutableSet = objcclass;
  NSMutableSetPointer = ^NSMutableSet;
  NSCountedSet = objcclass;
  NSCountedSetPointer = ^NSCountedSet;

{$endif}

{$ifdef CLASSES}
{$ifndef NSSET_PAS_C}
{$define NSSET_PAS_C}

{ NSSet }
  NSSet = objcclass(NSObject, NSCopyingProtocol, NSMutableCopyingProtocol, NSCodingProtocol, NSFastEnumerationProtocol)
    
  public
    class function alloc: NSSet; message 'alloc';

    function count: NSUInteger; message 'count';
    function member(object_: id): id; message 'member:';
    function objectEnumerator: NSEnumerator; message 'objectEnumerator';

    { Category: NSExtendedSet }
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

    { Category: NSSetCreation }
    class function set_: id; message 'set';
    class function setWithObject(object_: id): id; message 'setWithObject:';
    class function setWithObjects_count(objects: id; cnt: NSUInteger): id; message 'setWithObjects:count:';
    class function setWithObjects(firstObj: id): id; varargs; message 'setWithObjects:';
    class function setWithSet(set__: NSSet): id; message 'setWithSet:';
    class function setWithArray(array_: NSArray): id; message 'setWithArray:';
    function initWithObjects_count(objects: id; cnt: NSUInteger): id; message 'initWithObjects:count:';
    function initWithObjects(firstObj: id): id; varargs; message 'initWithObjects:';
    function initWithSet(set__: NSSet): id; message 'initWithSet:';
    function initWithSet_copyItems(set__: NSSet; flag: Boolean): id; message 'initWithSet:copyItems:';
    function initWithArray(array_: NSArray): id; message 'initWithArray:';

    { Category: NSKeyValueCoding }
    function valueForKey(key: NSString): id; message 'valueForKey:';
    procedure setValue_forKey(value: id; key: NSString); message 'setValue:forKey:';

    { Category: NSKeyValueObserverRegistration }
    procedure addObserver_forKeyPath_options_context(observer: NSObject; keyPath: NSString; options: NSKeyValueObservingOptions; context: Pointer); message 'addObserver:forKeyPath:options:context:';
    procedure removeObserver_forKeyPath(observer: NSObject; keyPath: NSString); message 'removeObserver:forKeyPath:';

    { Category: NSPredicateSupport }
    function filteredSetUsingPredicate(predicate: NSPredicate): NSSet; message 'filteredSetUsingPredicate:';
  end; external;

{ NSMutableSet }
  NSMutableSet = objcclass(NSSet)
    
  public
    class function alloc: NSMutableSet; message 'alloc';

    procedure addObject(object_: id); message 'addObject:';
    procedure removeObject(object_: id); message 'removeObject:';

    { Category: NSExtendedMutableSet }
    procedure addObjectsFromArray(array_: NSArray); message 'addObjectsFromArray:';
    procedure intersectSet(otherSet: NSSet); message 'intersectSet:';
    procedure minusSet(otherSet: NSSet); message 'minusSet:';
    procedure removeAllObjects; message 'removeAllObjects';
    procedure unionSet(otherSet: NSSet); message 'unionSet:';
    procedure setSet(otherSet: NSSet); message 'setSet:';

    { Category: NSMutableSetCreation }
    class function setWithCapacity(numItems: NSUInteger): id; message 'setWithCapacity:';
    function initWithCapacity(numItems: NSUInteger): id; message 'initWithCapacity:';

    { Category: NSPredicateSupport }
    procedure filterUsingPredicate(predicate: NSPredicate); message 'filterUsingPredicate:';
  end; external;

{ NSCountedSet }
  NSCountedSet = objcclass(NSMutableSet)
  private
    __table: id;
    __reserved: Pointer;
    
  public
    class function alloc: NSCountedSet; message 'alloc';

    function initWithCapacity(numItems: NSUInteger): id; message 'initWithCapacity:';
    function initWithArray(array_: NSArray): id; message 'initWithArray:';
    function initWithSet(set__: NSSet): id; message 'initWithSet:';
    function countForObject(object_: id): NSUInteger; message 'countForObject:';
    function objectEnumerator: NSEnumerator; message 'objectEnumerator';
    procedure addObject(object_: id); message 'addObject:';
    procedure removeObject(object_: id); message 'removeObject:';
  end; external;

{$endif}
{$endif}
