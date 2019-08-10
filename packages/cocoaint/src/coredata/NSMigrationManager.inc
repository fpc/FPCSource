{ Parsed from CoreData.framework NSMigrationManager.h }


{$ifdef TYPES}
type
  NSMigrationManagerPtr = ^NSMigrationManager;
{$endif}

{$ifdef CLASSES}

type
  NSMigrationManager = objcclass external (NSObject)
  private
    _sourceModel: NSManagedObjectModel;
    _sourceEntitiesByVersionHash: NSDictionary;
    _destinationModel: NSManagedObjectModel;
    _destinationEntitiesByVersionHash: NSDictionary;
    _mappingModel: NSMappingModel;
    _sourceManagedObjectContext: NSManagedObjectContext;
    _destinationManagedObjectContext: NSManagedObjectContext;
    _migrationContext: NSMigrationContext;
    _userInfo: NSDictionary;
    _migrationManagerFlags: bitpacked record
      case byte of
        0: (_anonBitField__migrationManagerFlags0: cuint);
        1: (
          _migrationWasCancelled: 0..1;
          _usesStoreSpecificMigrationManager: 0..1;
          _reservedMigrationManager: 0..((1 shl 30)-1);
        );
      end;
    _migrationCancellationError: NSError;
    _reserved1: id;
    _reserved2: id;
    _reserved3: id;
    _reserved4: id;
  public
    function initWithSourceModel_destinationModel (sourceModel: NSManagedObjectModel; destinationModel: NSManagedObjectModel): instancetype; message 'initWithSourceModel:destinationModel:';
    function migrateStoreFromURL_type_options_withMappingModel_toDestinationURL_destinationType_destinationOptions_error (sourceURL: NSURL; sStoreType: NSString; sOptions: NSDictionary; mappings: NSMappingModel; dURL: NSURL; dStoreType: NSString; dOptions: NSDictionary; error: NSErrorPtr): ObjCBOOL; message 'migrateStoreFromURL:type:options:withMappingModel:toDestinationURL:destinationType:destinationOptions:error:';
    procedure setUsesStoreSpecificMigrationManager(newValue: ObjCBOOL); message 'setUsesStoreSpecificMigrationManager:';
    function usesStoreSpecificMigrationManager: ObjCBOOL; message 'usesStoreSpecificMigrationManager';
    procedure reset; message 'reset';
    function mappingModel: NSMappingModel; message 'mappingModel';
    function sourceModel: NSManagedObjectModel; message 'sourceModel';
    function destinationModel: NSManagedObjectModel; message 'destinationModel';
    function sourceContext: NSManagedObjectContext; message 'sourceContext';
    function destinationContext: NSManagedObjectContext; message 'destinationContext';
    function sourceEntityForEntityMapping (mEntity: NSEntityMapping): NSEntityDescription; message 'sourceEntityForEntityMapping:';
    function destinationEntityForEntityMapping (mEntity: NSEntityMapping): NSEntityDescription; message 'destinationEntityForEntityMapping:';
    procedure associateSourceInstance_withDestinationInstance_forEntityMapping (sourceInstance: NSManagedObject; destinationInstance: NSManagedObject; entityMapping: NSEntityMapping); message 'associateSourceInstance:withDestinationInstance:forEntityMapping:';
    function destinationInstancesForEntityMappingNamed_sourceInstances (mappingName: NSString; sourceInstances: NSArray): NSArray; message 'destinationInstancesForEntityMappingNamed:sourceInstances:';
    function sourceInstancesForEntityMappingNamed_destinationInstances (mappingName: NSString; destinationInstances: NSArray): NSArray; message 'sourceInstancesForEntityMappingNamed:destinationInstances:';
    function currentEntityMapping: NSEntityMapping; message 'currentEntityMapping';
    function migrationProgress: single; message 'migrationProgress';
    procedure setUserInfo(newValue: NSDictionary); message 'setUserInfo:';
    function userInfo: NSDictionary; message 'userInfo';
    procedure cancelMigrationWithError (error: NSError); message 'cancelMigrationWithError:';
  end;
{$endif}

