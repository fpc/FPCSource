{ Parsed from Coredata.framework NSManagedObject.h }

{$ifdef TYPES}
{$ifndef NSMANAGEDOBJECT_PAS_T}
{$define NSMANAGEDOBJECT_PAS_T}

{ Constants }

const
  NSSnapshotEventUndoInsertion = 1 shl 1;
  NSSnapshotEventUndoDeletion = 1 shl 2;
  NSSnapshotEventUndoUpdate = 1 shl 3;
  NSSnapshotEventRollback = 1 shl 4;
  NSSnapshotEventRefresh = 1 shl 5;
  NSSnapshotEventMergePolicy = 1 shl 6;

{ Types }
type
  NSSnapshotEventType = NSUInteger;
  NSSnapshotEventTypePtr = ^NSSnapshotEventType;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSMANAGEDOBJECT_PAS_R}
{$define NSMANAGEDOBJECT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSMANAGEDOBJECT_PAS_F}
{$define NSMANAGEDOBJECT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSMANAGEDOBJECT_PAS_S}
{$define NSMANAGEDOBJECT_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSManagedObject = objcclass;
  NSManagedObjectPointer = ^NSManagedObject;
  NSManagedObjectPtr = NSManagedObjectPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSMANAGEDOBJECT_PAS_C}
{$define NSMANAGEDOBJECT_PAS_C}

{ NSManagedObject }
  NSManagedObject = objcclass external (NSObject)
  private
    _cd_rc: cint32;
    _cd_stateFlags: culong;
    _cd_rawData: id;
    _cd_entity: id;
    _cd_managedObjectContext: NSManagedObjectContext;
    _cd_objectID: NSManagedObjectID;
    _cd_faultHandler: id;
    _cd_observationInfo: id;
    _cd_snapshots: idPtr;
    _cd_lockingInfo: culong;
    _cd_queueReference: id;
    
  public
    class function contextShouldIgnoreUnmodeledPropertyChanges: Boolean; message 'contextShouldIgnoreUnmodeledPropertyChanges';
    function initWithEntity_insertIntoManagedObjectContext(entity_: NSEntityDescription; context: NSManagedObjectContext): id; message 'initWithEntity:insertIntoManagedObjectContext:';
    function managedObjectContext: NSManagedObjectContext; message 'managedObjectContext';
    function entity: NSEntityDescription; message 'entity';
    function objectID: NSManagedObjectID; message 'objectID';
    function isInserted: Boolean; message 'isInserted';
    function isUpdated: Boolean; message 'isUpdated';
    function isDeleted: Boolean; message 'isDeleted';
    function isFault: Boolean; message 'isFault';
    function hasFaultForRelationshipNamed(key: NSString): Boolean; message 'hasFaultForRelationshipNamed:';
    function faultingState: NSUInteger; message 'faultingState';
    procedure willAccessValueForKey(key: NSString); message 'willAccessValueForKey:';
    procedure didAccessValueForKey(key: NSString); message 'didAccessValueForKey:';
    procedure willChangeValueForKey(key: NSString); message 'willChangeValueForKey:';
    procedure didChangeValueForKey(key: NSString); message 'didChangeValueForKey:';
    procedure willChangeValueForKey_withSetMutation_usingObjects(inKey: NSString; inMutationKind: NSKeyValueSetMutationKind; inObjects: NSSet); message 'willChangeValueForKey:withSetMutation:usingObjects:';
    procedure didChangeValueForKey_withSetMutation_usingObjects(inKey: NSString; inMutationKind: NSKeyValueSetMutationKind; inObjects: NSSet); message 'didChangeValueForKey:withSetMutation:usingObjects:';
    procedure setObservationInfo(inObservationInfo: id); message 'setObservationInfo:';
    function observationInfo: id; message 'observationInfo';
    procedure awakeFromFetch; message 'awakeFromFetch';
    procedure awakeFromInsert; message 'awakeFromInsert';
    procedure awakeFromSnapshotEvents(flags: NSSnapshotEventType); message 'awakeFromSnapshotEvents:';
    procedure prepareForDeletion; message 'prepareForDeletion';
    procedure willSave; message 'willSave';
    procedure didSave; message 'didSave';
    procedure willTurnIntoFault; message 'willTurnIntoFault';
    procedure didTurnIntoFault; message 'didTurnIntoFault';
    function valueForKey(key: NSString): id; message 'valueForKey:';
    procedure setValue_forKey(value: id; key: NSString); message 'setValue:forKey:';
    function primitiveValueForKey(key: NSString): id; message 'primitiveValueForKey:';
    procedure setPrimitiveValue_forKey(value: id; key: NSString); message 'setPrimitiveValue:forKey:';
    function committedValuesForKeys(keys: NSArray): NSDictionary; message 'committedValuesForKeys:';
    function changedValues: NSDictionary; message 'changedValues';
    function validateValue_forKey_error(value: idPtr; key: NSString; error: NSErrorPointer): Boolean; message 'validateValue:forKey:error:';
    function validateForDelete(error: NSErrorPointer): Boolean; message 'validateForDelete:';
    function validateForInsert(error: NSErrorPointer): Boolean; message 'validateForInsert:';
    function validateForUpdate(error: NSErrorPointer): Boolean; message 'validateForUpdate:';
  end;

{$endif}
{$endif}
