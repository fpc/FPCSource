{ Parsed from Coredata.framework NSMigrationManager.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Fri Oct 2 11:46:33 EDT 2009 }


{$ifdef TYPES}
{$ifndef NSMIGRATIONMANAGER_PAS_T}
{$define NSMIGRATIONMANAGER_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSMIGRATIONMANAGER_PAS_R}
{$define NSMIGRATIONMANAGER_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSMIGRATIONMANAGER_PAS_F}
{$define NSMIGRATIONMANAGER_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSMIGRATIONMANAGER_PAS_S}
{$define NSMIGRATIONMANAGER_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSMigrationManager = objcclass;
  NSMigrationManagerPointer = ^NSMigrationManager;

{$endif}

{$ifdef CLASSES}
{$ifndef NSMIGRATIONMANAGER_PAS_C}
{$define NSMIGRATIONMANAGER_PAS_C}

{ NSMigrationManager }
  NSMigrationManager = objcclass(NSObject)
  private
    __sourceModel: NSManagedObjectModel;
    __sourceEntitiesByVersionHash: NSDictionary;
    __destinationModel: NSManagedObjectModel;
    __destinationEntitiesByVersionHash: NSDictionary;
    __mappingModel: NSMappingModel;
    __sourceManagedObjectContext: NSManagedObjectContext;
    __destinationManagedObjectContext: NSManagedObjectContext;
    __migrationContext: NSMigrationContext;
    __userInfo: NSDictionary;
    __migrationManagerFlags: bitpacked record
        _migrationWasCancelled: 0..1;
        _reservedMigrationManager: 0..((1 shl 31)-1);
      end;
    __migrationCancellationError: NSError;
    __reserved1: id;
    __reserved2: id;
    __reserved3: id;
    __reserved4: id;
    
  public
    class function alloc: NSMigrationManager; message 'alloc';

    function initWithSourceModel_destinationModel(sourceModel_: NSManagedObjectModel; destinationModel_: NSManagedObjectModel): id; message 'initWithSourceModel:destinationModel:';
    function migrateStoreFromURL_type_options_withMappingModel_toDestinationURL_destinationType_destinationOptions_error(sourceURL: NSURL; sStoreType: NSString; sOptions: NSDictionary; mappings: NSMappingModel; dURL: NSURL; dStoreType: NSString; dOptions: NSDictionary; error: NSErrorPointer): Boolean; message 'migrateStoreFromURL:type:options:withMappingModel:toDestinationURL:destinationType:destinationOptions:error:';
    procedure reset; message 'reset';
    function mappingModel: NSMappingModel; message 'mappingModel';
    function sourceModel: NSManagedObjectModel; message 'sourceModel';
    function destinationModel: NSManagedObjectModel; message 'destinationModel';
    function sourceContext: NSManagedObjectContext; message 'sourceContext';
    function destinationContext: NSManagedObjectContext; message 'destinationContext';
    function sourceEntityForEntityMapping(mEntity: NSEntityMapping): NSEntityDescription; message 'sourceEntityForEntityMapping:';
    function destinationEntityForEntityMapping(mEntity: NSEntityMapping): NSEntityDescription; message 'destinationEntityForEntityMapping:';
    procedure associateSourceInstance_withDestinationInstance_forEntityMapping(sourceInstance: NSManagedObject; destinationInstance: NSManagedObject; entityMapping: NSEntityMapping); message 'associateSourceInstance:withDestinationInstance:forEntityMapping:';
    function sourceInstancesForEntityMappingNamed_destinationInstances(mappingName: NSString; destinationInstances: NSArray): NSArray; message 'sourceInstancesForEntityMappingNamed:destinationInstances:';
    function currentEntityMapping: NSEntityMapping; message 'currentEntityMapping';
    function migrationProgress: single; message 'migrationProgress';
    function userInfo: NSDictionary; message 'userInfo';
    procedure setUserInfo(dict: NSDictionary); message 'setUserInfo:';
    procedure cancelMigrationWithError(error: NSError); message 'cancelMigrationWithError:';
  end; external;

{$endif}
{$endif}
