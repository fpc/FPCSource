{$mode delphi}
{$modeswitch objectivec1}
{$modeswitch cvar}
{$packrecords c}

unit DefinedClassesSyncServices;
interface

type
  ISyncChange = objcclass external;
  ISyncClient = objcclass external;
  ISyncFilter = objcclass external;
  ISyncManager = objcclass external;
  ISyncRecordReference = objcclass external;
  ISyncRecordSnapshot = objcclass external;
  ISyncSession = objcclass external;
  ISyncSessionDriver = objcclass external;
  ISyncFilteringProtocol = objcprotocol external name 'ISyncFiltering';
  ISyncSessionDriverDataSourceProtocol = objcprotocol external name 'ISyncSessionDriverDataSource';
  NSPersistentStoreCoordinatorSyncingProtocol = objcprotocol external name 'NSPersistentStoreCoordinatorSyncing';

implementation
end.
