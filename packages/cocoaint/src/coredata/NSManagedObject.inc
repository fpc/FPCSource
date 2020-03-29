{ Parsed from CoreData.framework NSManagedObject.h }


{$ifdef TYPES}
type
  NSManagedObjectPtr = ^NSManagedObject;
{$endif}

{$ifdef TYPES}
type
  NSSnapshotEventType = NSUInteger;
  NSSnapshotEventTypePtr = ^NSSnapshotEventType;

const
  NSSnapshotEventUndoInsertion = 1 shl 1;
  NSSnapshotEventUndoDeletion = 1 shl 2;
  NSSnapshotEventUndoUpdate = 1 shl 3;
  NSSnapshotEventRollback = 1 shl 4;
  NSSnapshotEventRefresh = 1 shl 5;
  NSSnapshotEventMergePolicy = 1 shl 6;
{$endif}

{$ifdef CLASSES}

type
  NSManagedObject = objcclass external (NSObject)
  private
    _cd_rc: cint32;
    _cd_stateFlags: culong;
    _cd_rawData: id;
    _cd_entity: id;
    _cd_managedObjectContext: NSManagedObjectContext;
    _cd_objectID: NSManagedObjectID;
    _cd_extraFlags: culong;
    _cd_observationInfo: id;
    _cd_snapshots: idPtr;
    _cd_lockingInfo: culong;
    _cd_queueReference: id;
  public
    class function contextShouldIgnoreUnmodeledPropertyChanges: ObjCBOOL; message 'contextShouldIgnoreUnmodeledPropertyChanges'; { available in 10_6,3_0 }
    function initWithEntity_insertIntoManagedObjectContext (entity: NSEntityDescription; context: NSManagedObjectContext): instancetype; message 'initWithEntity:insertIntoManagedObjectContext:'; { NS_DESIGNATED_INITIALIZER }
    function managedObjectContext: NSManagedObjectContext; message 'managedObjectContext';
    function entity: NSEntityDescription; message 'entity';
    function objectID: NSManagedObjectID; message 'objectID';
    function isInserted: ObjCBOOL; message 'isInserted';
    function isUpdated: ObjCBOOL; message 'isUpdated';
    function isDeleted: ObjCBOOL; message 'isDeleted';
    function hasChanges: ObjCBOOL; message 'hasChanges';
    function isFault: ObjCBOOL; message 'isFault';
    function hasFaultForRelationshipNamed (key: NSString): ObjCBOOL; message 'hasFaultForRelationshipNamed:'; { available in 10_5,3_0 }
    function faultingState: NSUInteger; message 'faultingState';
    procedure willAccessValueForKey (key: NSString); message 'willAccessValueForKey:';
    procedure didAccessValueForKey (key: NSString); message 'didAccessValueForKey:';
    procedure willChangeValueForKey (key: NSString); message 'willChangeValueForKey:';
    procedure didChangeValueForKey (key: NSString); message 'didChangeValueForKey:';
    procedure willChangeValueForKey_withSetMutation_usingObjects (inKey: NSString; inMutationKind: NSKeyValueSetMutationKind; inObjects: NSSet); message 'willChangeValueForKey:withSetMutation:usingObjects:';
    procedure didChangeValueForKey_withSetMutation_usingObjects (inKey: NSString; inMutationKind: NSKeyValueSetMutationKind; inObjects: NSSet); message 'didChangeValueForKey:withSetMutation:usingObjects:';
    procedure awakeFromFetch; message 'awakeFromFetch';
    procedure awakeFromInsert; message 'awakeFromInsert';
    procedure awakeFromSnapshotEvents (flags: NSSnapshotEventType); message 'awakeFromSnapshotEvents:'; { available in 10_6,3_0 }
    procedure prepareForDeletion; message 'prepareForDeletion'; { available in 10_6,3_0 }
    procedure willSave; message 'willSave';
    procedure didSave; message 'didSave';
    procedure willTurnIntoFault; message 'willTurnIntoFault'; { available in 10_5,3_0 }
    procedure didTurnIntoFault; message 'didTurnIntoFault';
    function valueForKey (key: NSString): id; message 'valueForKey:';
    procedure setValue_forKey (value: id; key: NSString); message 'setValue:forKey:';
    function primitiveValueForKey (key: NSString): id; message 'primitiveValueForKey:';
    procedure setPrimitiveValue_forKey (value: id; key: NSString); message 'setPrimitiveValue:forKey:';
    function committedValuesForKeys (keys: NSArray): NSDictionary; message 'committedValuesForKeys:';
    function changedValues: NSDictionary; message 'changedValues';
    function changedValuesForCurrentEvent: NSDictionary; message 'changedValuesForCurrentEvent'; { available in 10_7, 5_0 }
    function validateValue_forKey_error (value: idPtr; key: NSString; error: NSErrorPtr): ObjCBOOL; message 'validateValue:forKey:error:';
    function validateForDelete (error: NSErrorPtr): ObjCBOOL; message 'validateForDelete:';
    function validateForInsert (error: NSErrorPtr): ObjCBOOL; message 'validateForInsert:';
    function validateForUpdate (error: NSErrorPtr): ObjCBOOL; message 'validateForUpdate:';
    procedure setObservationInfo (inObservationInfo: id); message 'setObservationInfo:';
    function observationInfo: id; message 'observationInfo';
  end;
{$endif}

