{ Parsed from Appkit.framework NSArrayController.h }

{$ifdef TYPES}
{$ifndef NSARRAYCONTROLLER_PAS_T}
{$define NSARRAYCONTROLLER_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSARRAYCONTROLLER_PAS_R}
{$define NSARRAYCONTROLLER_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSARRAYCONTROLLER_PAS_F}
{$define NSARRAYCONTROLLER_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSARRAYCONTROLLER_PAS_S}
{$define NSARRAYCONTROLLER_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSArrayController = objcclass;
  NSArrayControllerPointer = ^NSArrayController;
  NSArrayControllerPtr = NSArrayControllerPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSARRAYCONTROLLER_PAS_C}
{$define NSARRAYCONTROLLER_PAS_C}

{ NSArrayController }
  NSArrayController = objcclass external (NSObjectController)
  private
    _reserved4: Pointer;
    _rearrangementExtensions: id;
    _temporaryWorkObjects: NSMutableArray;
    __arrayControllerFlags: record
      case byte of
       0: (_anonbitfield_NSArrayController0: cuint);
       1: (data: bitpacked record
        _avoidsEmptySelection: 0..1;
        _preservesSelection: 0..1;
        _selectsInsertedObjects: 0..1;
        _alwaysUsesMultipleValuesMarker: 0..1;
        _refreshesAllModelObjects: 0..1;
        _filterRestrictsInsertion: 0..1;
        _overridesArrangeObjects: 0..1;
        _overridesDidChangeArrangementCriteria: 0..1;
        _explicitlyCannotInsert: 0..1;
        _generatedEmptyArray: 0..1;
        _isObservingKeyPathsThroughArrangedObjects: 0..1;
        _arrangedObjectsIsMutable: 0..1;
        _clearsFilterPredicateOnInsertion: 0..1;
        _skipSortingAfterFetch: 0..1;
        _automaticallyRearrangesObjects: 0..1;
        _reservedArrayController: 0..((1 shl 17)-1);
       end;
      );
      end;
    _observedIndexHint: NSUInteger;
    _selectionIndexes: NSMutableIndexSet;
    _objects: NSMutableArray;
    _cachedSelectedIndexes: NSIndexSet;
    _cachedSelectedObjects: NSArray;
    _arrangedObjects: NSArray;
    
  public
    procedure rearrangeObjects; message 'rearrangeObjects';
    procedure setAutomaticallyRearrangesObjects(flag: BOOL); message 'setAutomaticallyRearrangesObjects:';
    function automaticallyRearrangesObjects: BOOL; message 'automaticallyRearrangesObjects';
    function automaticRearrangementKeyPaths: NSArray; message 'automaticRearrangementKeyPaths';
    procedure didChangeArrangementCriteria; message 'didChangeArrangementCriteria';
    procedure setSortDescriptors(sortDescriptors_: NSArray); message 'setSortDescriptors:';
    function sortDescriptors: NSArray; message 'sortDescriptors';
    procedure setFilterPredicate(filterPredicate_: NSPredicate); message 'setFilterPredicate:';
    function filterPredicate: NSPredicate; message 'filterPredicate';
    procedure setClearsFilterPredicateOnInsertion(flag: BOOL); message 'setClearsFilterPredicateOnInsertion:';
    function clearsFilterPredicateOnInsertion: BOOL; message 'clearsFilterPredicateOnInsertion';
    function arrangeObjects(objects: NSArray): NSArray; message 'arrangeObjects:';
    function arrangedObjects: id; message 'arrangedObjects';
    procedure setAvoidsEmptySelection(flag: BOOL); message 'setAvoidsEmptySelection:';
    function avoidsEmptySelection: BOOL; message 'avoidsEmptySelection';
    procedure setPreservesSelection(flag: BOOL); message 'setPreservesSelection:';
    function preservesSelection: BOOL; message 'preservesSelection';
    procedure setSelectsInsertedObjects(flag: BOOL); message 'setSelectsInsertedObjects:';
    function selectsInsertedObjects: BOOL; message 'selectsInsertedObjects';
    procedure setAlwaysUsesMultipleValuesMarker(flag: BOOL); message 'setAlwaysUsesMultipleValuesMarker:';
    function alwaysUsesMultipleValuesMarker: BOOL; message 'alwaysUsesMultipleValuesMarker';
    function setSelectionIndexes(indexes: NSIndexSet): BOOL; message 'setSelectionIndexes:';
    function selectionIndexes: NSIndexSet; message 'selectionIndexes';
    function setSelectionIndex(index: NSUInteger): BOOL; message 'setSelectionIndex:';
    function selectionIndex: NSUInteger; message 'selectionIndex';
    function addSelectionIndexes(indexes: NSIndexSet): BOOL; message 'addSelectionIndexes:';
    function removeSelectionIndexes(indexes: NSIndexSet): BOOL; message 'removeSelectionIndexes:';
    function setSelectedObjects(objects: NSArray): BOOL; message 'setSelectedObjects:';
    function selectedObjects: NSArray; message 'selectedObjects';
    function addSelectedObjects(objects: NSArray): BOOL; message 'addSelectedObjects:';
    function removeSelectedObjects(objects: NSArray): BOOL; message 'removeSelectedObjects:';
    procedure add(sender: id); message 'add:';
    procedure remove(sender: id); message 'remove:';
    procedure insert(sender: id); message 'insert:';
    function canInsert: BOOL; message 'canInsert';
    procedure selectNext(sender: id); message 'selectNext:';
    procedure selectPrevious(sender: id); message 'selectPrevious:';
    function canSelectNext: BOOL; message 'canSelectNext';
    function canSelectPrevious: BOOL; message 'canSelectPrevious';
    procedure addObject(object_: id); message 'addObject:';
    procedure addObjects(objects: NSArray); message 'addObjects:';
    procedure insertObject_atArrangedObjectIndex(object_: id; index: NSUInteger); message 'insertObject:atArrangedObjectIndex:';
    procedure insertObjects_atArrangedObjectIndexes(objects: NSArray; indexes: NSIndexSet); message 'insertObjects:atArrangedObjectIndexes:';
    procedure removeObjectAtArrangedObjectIndex(index: NSUInteger); message 'removeObjectAtArrangedObjectIndex:';
    procedure removeObjectsAtArrangedObjectIndexes(indexes: NSIndexSet); message 'removeObjectsAtArrangedObjectIndexes:';
    procedure removeObject(object_: id); message 'removeObject:';
    procedure removeObjects(objects: NSArray); message 'removeObjects:';
  end;

{$endif}
{$endif}
