{ Parsed from CoreData.framework NSIncrementalStore.h }


{$ifdef TYPES}
type
  NSIncrementalStorePtr = ^NSIncrementalStore;
{$endif}

{$ifdef CLASSES}

type
  NSIncrementalStore = objcclass external (NSPersistentStore)
  private
    _storeMetadata: NSDictionary;
    _lastIdentifier: cuint64;
    _reserveda: pointer;
    _reservedb: pointer;
  public
    function loadMetadata (error: NSErrorPtr): ObjCBOOL; message 'loadMetadata:';
    function executeRequest_withContext_error (request: NSPersistentStoreRequest; context: NSManagedObjectContext; error: NSErrorPtr): id; message 'executeRequest:withContext:error:';
    function newValuesForObjectWithID_withContext_error (objectID: NSManagedObjectID; context: NSManagedObjectContext; error: NSErrorPtr): NSIncrementalStoreNode; message 'newValuesForObjectWithID:withContext:error:';
    function newValueForRelationship_forObjectWithID_withContext_error (relationship: NSRelationshipDescription; objectID: NSManagedObjectID; context: NSManagedObjectContext; error: NSErrorPtr): id; message 'newValueForRelationship:forObjectWithID:withContext:error:';
    class function identifierForNewStoreAtURL (storeURL: NSURL): id; message 'identifierForNewStoreAtURL:';
    function obtainPermanentIDsForObjects_error (array_: NSArray; error: NSErrorPtr): NSArray; message 'obtainPermanentIDsForObjects:error:';
    procedure managedObjectContextDidRegisterObjectsWithIDs (objectIDs: NSArray); message 'managedObjectContextDidRegisterObjectsWithIDs:';
    procedure managedObjectContextDidUnregisterObjectsWithIDs (objectIDs: NSArray); message 'managedObjectContextDidUnregisterObjectsWithIDs:';
    function newObjectIDForEntity_referenceObject (entity: NSEntityDescription; data: id): NSManagedObjectID; message 'newObjectIDForEntity:referenceObject:';
    function referenceObjectForObjectID (objectID: NSManagedObjectID): id; message 'referenceObjectForObjectID:';
  end;
{$endif}

