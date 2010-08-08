{ Parsed from Coredata.framework NSManagedObject.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Fri Oct 2 11:46:33 EDT 2009 }


{$ifdef TYPES}
{$ifndef NSMANAGEDOBJECT_PAS_T}
{$define NSMANAGEDOBJECT_PAS_T}

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

{$endif}

{$ifdef CLASSES}
{$ifndef NSMANAGEDOBJECT_PAS_C}
{$define NSMANAGEDOBJECT_PAS_C}

{ NSManagedObject }
  NSManagedObject = objcclass(NSObject)
  private
    __cd_rc: longint;
    __cd_stateFlags: uintptr_t;
    __cd_rawData: id;
    __cd_entity: id;
    __cd_managedObjectContext: NSManagedObjectContext; {garbage collector: __weak }
    __cd_objectID: NSManagedObjectID;
    __cd_faultHandler: id;
    __cd_observationInfo: id;
    __cd_snapshots: id;
    __cd_lockingInfo: uintptr_t;
    __cd_queueReference: id;
    
  public
    class function alloc: NSManagedObject; message 'alloc';

    function initWithEntity_insertIntoManagedObjectContext(entity_: NSEntityDescription; context: NSManagedObjectContext): id; message 'initWithEntity:insertIntoManagedObjectContext:';
    function managedObjectContext: NSManagedObjectContext; message 'managedObjectContext';
    function entity: NSEntityDescription; message 'entity';
    function objectID: NSManagedObjectID; message 'objectID';
    function isInserted: Boolean; message 'isInserted';
    function isUpdated: Boolean; message 'isUpdated';
    function isDeleted: Boolean; message 'isDeleted';
    function isFault: Boolean; message 'isFault';
    function hasFaultForRelationshipNamed(key: NSString): Boolean; message 'hasFaultForRelationshipNamed:';
    procedure willAccessValueForKey(key: NSString); message 'willAccessValueForKey:';
    procedure didAccessValueForKey(key: NSString); message 'didAccessValueForKey:';
    procedure willChangeValueForKey(key: NSString); message 'willChangeValueForKey:';
    procedure didChangeValueForKey(key: NSString); message 'didChangeValueForKey:';
    procedure willChangeValueForKey_withSetMutation_usingObjects(inKey: NSString; inMutationKind: NSKeyValueSetMutationKind; inObjects: NSSet); message 'willChangeValueForKey:withSetMutation:usingObjects:';
    procedure didChangeValueForKey_withSetMutation_usingObjects(inKey: NSString; inMutationKind: NSKeyValueSetMutationKind; inObjects: NSSet); message 'didChangeValueForKey:withSetMutation:usingObjects:';
    procedure setObservationInfo(inObservationInfo: Pointer); message 'setObservationInfo:';
    procedure awakeFromFetch; message 'awakeFromFetch';
    procedure awakeFromInsert; message 'awakeFromInsert';
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
    function validateValue_forKey_error(value: id; key: NSString; error: NSErrorPointer): Boolean; message 'validateValue:forKey:error:';
    function validateForDelete(error: NSErrorPointer): Boolean; message 'validateForDelete:';
    function validateForInsert(error: NSErrorPointer): Boolean; message 'validateForInsert:';
    function validateForUpdate(error: NSErrorPointer): Boolean; message 'validateForUpdate:';
  end; external;

{$endif}
{$endif}
