{ Parsed from CoreData.framework NSManagedObjectModel.h }


{$ifdef TYPES}
type
  NSManagedObjectModelPtr = ^NSManagedObjectModel;
{$endif}

{$ifdef CLASSES}

type
  NSManagedObjectModel = objcclass external (NSObject, NSCodingProtocol, NSCopyingProtocol, NSFastEnumerationProtocol)
  private
    _dataForOptimization: id;
    _optimizationHints: idPtr;
    _localizationPolicy: id;
    _entities: NSMutableDictionary;
    _configurations: NSMutableDictionary;
    _fetchRequestTemplates: NSMutableDictionary;
    _versionIdentifiers: NSSet;
    __managedObjectModelFlags: bitpacked record
      case byte of
        0: (_anonBitField___managedObjectModelFlags0: cuint);
        1: (
          _isInUse: 0..1;
          _isImmutable: 0..1;
          _isOptimizedForEncoding: 0..1;
          _reservedEntityDescription: 0..((1 shl 29)-1);
        );
      end;
  public
    class function mergedModelFromBundles (bundles: NSArray): NSManagedObjectModel; message 'mergedModelFromBundles:';
    class function modelByMergingModels (models: NSArray): NSManagedObjectModel; message 'modelByMergingModels:';
    function init: instancetype; message 'init'; { NS_DESIGNATED_INITIALIZER }
    function initWithContentsOfURL (url: NSURL): instancetype; message 'initWithContentsOfURL:';
    function entitiesByName: NSDictionary; message 'entitiesByName';
    procedure setEntities(newValue: NSArray); message 'setEntities:';
    function entities: NSArray; message 'entities';
    function configurations: NSArray; message 'configurations';
    function entitiesForConfiguration (configuration: NSString): NSArray; message 'entitiesForConfiguration:';
    procedure setEntities_forConfiguration (entities_: NSArray; configuration: NSString); message 'setEntities:forConfiguration:';
    procedure setFetchRequestTemplate_forName (fetchRequestTemplate: NSFetchRequest; name: NSString); message 'setFetchRequestTemplate:forName:';
    function fetchRequestTemplateForName (name: NSString): NSFetchRequest; message 'fetchRequestTemplateForName:';
    function fetchRequestFromTemplateWithName_substitutionVariables (name: NSString; variables: NSDictionary): NSFetchRequest; message 'fetchRequestFromTemplateWithName:substitutionVariables:';
    procedure setLocalizationDictionary(newValue: NSDictionary); message 'setLocalizationDictionary:';
    function localizationDictionary: NSDictionary; message 'localizationDictionary';
    class function mergedModelFromBundles_forStoreMetadata (bundles: NSArray; metadata: NSDictionary): NSManagedObjectModel; message 'mergedModelFromBundles:forStoreMetadata:'; { available in 10_5,3_0 }
    class function modelByMergingModels_forStoreMetadata (models: NSArray; metadata: NSDictionary): NSManagedObjectModel; message 'modelByMergingModels:forStoreMetadata:'; { available in 10_5,3_0 }
    function fetchRequestTemplatesByName: NSDictionary; message 'fetchRequestTemplatesByName';
    procedure setVersionIdentifiers(newValue: NSSet); message 'setVersionIdentifiers:';
    function versionIdentifiers: NSSet; message 'versionIdentifiers';
    function isConfiguration_compatibleWithStoreMetadata (configuration: NSString; metadata: NSDictionary): ObjCBOOL; message 'isConfiguration:compatibleWithStoreMetadata:'; { available in 10_5,3_0 }
    function entityVersionHashesByName: NSDictionary; message 'entityVersionHashesByName';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    function countByEnumeratingWithState_objects_count (state: NSFastEnumerationStatePtr; buffer: idPtr { variable size array of id }; len: NSUInteger): NSUInteger; message 'countByEnumeratingWithState:objects:count:';
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
  end;
{$endif}

