{ Parsed from AppKit.framework NSArrayController.h }


{$ifdef TYPES}
type
  NSArrayControllerPtr = ^NSArrayController;
{$endif}

{$ifdef CLASSES}

type
  NSArrayController = objcclass external (NSObjectController)
  private
    _reserved4: pointer;
    _rearrangementExtensions: id;
    _temporaryWorkObjects: NSMutableArray;
    _arrayControllerFlags: bitpacked record
      case byte of
        0: (_anonBitField__arrayControllerFlags0: cuint);
        1: (
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
    procedure setAutomaticallyRearrangesObjects(newValue: ObjCBOOL); message 'setAutomaticallyRearrangesObjects:';
    function automaticallyRearrangesObjects: ObjCBOOL; message 'automaticallyRearrangesObjects';
    function automaticRearrangementKeyPaths: NSArray; message 'automaticRearrangementKeyPaths';
    procedure didChangeArrangementCriteria; message 'didChangeArrangementCriteria'; { available in 10_5 }
    procedure setSortDescriptors(newValue: NSArray); message 'setSortDescriptors:';
    function sortDescriptors: NSArray; message 'sortDescriptors';
    procedure setFilterPredicate(newValue: NSPredicate); message 'setFilterPredicate:';
    function filterPredicate: NSPredicate; message 'filterPredicate';
    procedure setClearsFilterPredicateOnInsertion(newValue: ObjCBOOL); message 'setClearsFilterPredicateOnInsertion:';
    function clearsFilterPredicateOnInsertion: ObjCBOOL; message 'clearsFilterPredicateOnInsertion';
    function arrangeObjects (objects: NSArray): NSArray; message 'arrangeObjects:';
    function arrangedObjects: id; message 'arrangedObjects';
    procedure setAvoidsEmptySelection(newValue: ObjCBOOL); message 'setAvoidsEmptySelection:';
    function avoidsEmptySelection: ObjCBOOL; message 'avoidsEmptySelection';
    procedure setPreservesSelection(newValue: ObjCBOOL); message 'setPreservesSelection:';
    function preservesSelection: ObjCBOOL; message 'preservesSelection';
    procedure setSelectsInsertedObjects(newValue: ObjCBOOL); message 'setSelectsInsertedObjects:';
    function selectsInsertedObjects: ObjCBOOL; message 'selectsInsertedObjects';
    procedure setAlwaysUsesMultipleValuesMarker(newValue: ObjCBOOL); message 'setAlwaysUsesMultipleValuesMarker:';
    function alwaysUsesMultipleValuesMarker: ObjCBOOL; message 'alwaysUsesMultipleValuesMarker';
    function setSelectionIndexes (indexes: NSIndexSet): ObjCBOOL; message 'setSelectionIndexes:';
    function selectionIndexes: NSIndexSet; message 'selectionIndexes';
    function setSelectionIndex (index: NSUInteger): ObjCBOOL; message 'setSelectionIndex:';
    function selectionIndex: NSUInteger; message 'selectionIndex';
    function addSelectionIndexes (indexes: NSIndexSet): ObjCBOOL; message 'addSelectionIndexes:';
    function removeSelectionIndexes (indexes: NSIndexSet): ObjCBOOL; message 'removeSelectionIndexes:';
    function setSelectedObjects (objects: NSArray): ObjCBOOL; message 'setSelectedObjects:';
    function selectedObjects: NSArray; message 'selectedObjects';
    function addSelectedObjects (objects: NSArray): ObjCBOOL; message 'addSelectedObjects:';
    function removeSelectedObjects (objects: NSArray): ObjCBOOL; message 'removeSelectedObjects:';
    procedure add (sender: id); message 'add:';
    procedure remove (sender: id); message 'remove:';
    procedure insert (sender: id); message 'insert:';
    function canInsert: ObjCBOOL; message 'canInsert';
    procedure selectNext (sender: id); message 'selectNext:';
    procedure selectPrevious (sender: id); message 'selectPrevious:';
    function canSelectNext: ObjCBOOL; message 'canSelectNext';
    function canSelectPrevious: ObjCBOOL; message 'canSelectPrevious';
    procedure addObject (object_: id); message 'addObject:';
    procedure addObjects (objects: NSArray); message 'addObjects:';
    procedure insertObject_atArrangedObjectIndex (object_: id; index: NSUInteger); message 'insertObject:atArrangedObjectIndex:';
    procedure insertObjects_atArrangedObjectIndexes (objects: NSArray; indexes: NSIndexSet); message 'insertObjects:atArrangedObjectIndexes:';
    procedure removeObjectAtArrangedObjectIndex (index: NSUInteger); message 'removeObjectAtArrangedObjectIndex:';
    procedure removeObjectsAtArrangedObjectIndexes (indexes: NSIndexSet); message 'removeObjectsAtArrangedObjectIndexes:';
    procedure removeObject (object_: id); message 'removeObject:';
    procedure removeObjects (objects: NSArray); message 'removeObjects:';
  end;
{$endif}

