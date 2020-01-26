{ Parsed from SyncServices.framework ISyncCoreData.h }


{$ifdef TYPES}
type
  NSPersistentStoreCoordinatorSyncingProtocolPtr = ^NSPersistentStoreCoordinatorSyncingProtocol;
{$endif}

{$ifdef CLASSES}

type
  CoreDataSync = objccategory external (NSPersistentStoreCoordinator)
    function syncWithClient_inBackground_handler_error (client: ISyncClient; flag: ObjCBOOL; syncHandler: NSPersistentStoreCoordinatorSyncingProtocol; rError: NSErrorPtr): ObjCBOOL; message 'syncWithClient:inBackground:handler:error:'; deprecated 'in 10_4, 10_7';
    procedure setStoresFastSyncDetailsAtURL_forPersistentStore (url: NSURL; store: NSPersistentStore); message 'setStoresFastSyncDetailsAtURL:forPersistentStore:'; deprecated 'in 10_4, 10_7';
  end;
{$endif}

{$ifdef PROTOCOLS}

type
  NSPersistentStoreCoordinatorSyncingProtocol = objcprotocol external name 'NSPersistentStoreCoordinatorSyncing' (NSObjectProtocol)
  optional
    function managedObjectContextsToMonitorWhenSyncingPersistentStoreCoordinator (coordinator: NSPersistentStoreCoordinator): NSArray; message 'managedObjectContextsToMonitorWhenSyncingPersistentStoreCoordinator:'; deprecated 'in 10_4, 10_7';
    function managedObjectContextsToReloadAfterSyncingPersistentStoreCoordinator (coordinator: NSPersistentStoreCoordinator): NSArray; message 'managedObjectContextsToReloadAfterSyncingPersistentStoreCoordinator:'; deprecated 'in 10_4, 10_7';
    function persistentStoreCoordinatorShouldStartSyncing (coordinator: NSPersistentStoreCoordinator): ObjCBOOL; message 'persistentStoreCoordinatorShouldStartSyncing:'; deprecated 'in 10_4, 10_7';
    procedure persistentStoreCoordinator_willPushChangesInSyncSession (coordinator: NSPersistentStoreCoordinator; session: ISyncSession); message 'persistentStoreCoordinator:willPushChangesInSyncSession:'; deprecated 'in 10_4, 10_7';
    procedure persistentStoreCoordinator_didPushChangesInSyncSession (coordinator: NSPersistentStoreCoordinator; session: ISyncSession); message 'persistentStoreCoordinator:didPushChangesInSyncSession:'; deprecated 'in 10_4, 10_7';
    procedure persistentStoreCoordinator_willPullChangesInSyncSession (coordinator: NSPersistentStoreCoordinator; session: ISyncSession); message 'persistentStoreCoordinator:willPullChangesInSyncSession:'; deprecated 'in 10_4, 10_7';
    procedure persistentStoreCoordinator_didPullChangesInSyncSession (coordinator: NSPersistentStoreCoordinator; session: ISyncSession); message 'persistentStoreCoordinator:didPullChangesInSyncSession:'; deprecated 'in 10_4, 10_7';
    procedure persistentStoreCoordinator_didFinishSyncSession (coordinator: NSPersistentStoreCoordinator; session: ISyncSession); message 'persistentStoreCoordinator:didFinishSyncSession:'; deprecated 'in 10_4, 10_7';
    procedure persistentStoreCoordinator_didCancelSyncSession_error (coordinator: NSPersistentStoreCoordinator; session: ISyncSession; error: NSError); message 'persistentStoreCoordinator:didCancelSyncSession:error:'; deprecated 'in 10_4, 10_7';
    function persistentStoreCoordinator_willPushRecord_forManagedObject_inSyncSession (coordinator: NSPersistentStoreCoordinator; record_: NSDictionary; managedObject: NSManagedObject; session: ISyncSession): NSDictionary; message 'persistentStoreCoordinator:willPushRecord:forManagedObject:inSyncSession:'; deprecated 'in 10_4, 10_7';
    function persistentStoreCoordinator_willDeleteRecordWithIdentifier_inSyncSession (coordinator: NSPersistentStoreCoordinator; identifier: NSString; session: ISyncSession): ObjCBOOL; message 'persistentStoreCoordinator:willDeleteRecordWithIdentifier:inSyncSession:'; deprecated 'in 10_4, 10_7';
    function persistentStoreCoordinator_willApplyChange_toManagedObject_inSyncSession (coordinator: NSPersistentStoreCoordinator; change: ISyncChange; managedObject: NSManagedObject; session: ISyncSession): ISyncChange; message 'persistentStoreCoordinator:willApplyChange:toManagedObject:inSyncSession:'; deprecated 'in 10_4, 10_7';
    procedure persistentStoreCoordinator_didApplyChange_toManagedObject_inSyncSession (coordinator: NSPersistentStoreCoordinator; change: ISyncChange; managedObject: NSManagedObject; session: ISyncSession); message 'persistentStoreCoordinator:didApplyChange:toManagedObject:inSyncSession:'; deprecated 'in 10_4, 10_7';
    procedure persistentStoreCoordinator_didCommitChanges_inSyncSession (coordinator: NSPersistentStoreCoordinator; changes: NSDictionary; session: ISyncSession); message 'persistentStoreCoordinator:didCommitChanges:inSyncSession:'; deprecated 'in 10_4, 10_7';
  end;
{$endif}

