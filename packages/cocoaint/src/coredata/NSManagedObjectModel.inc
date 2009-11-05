{ Parsed from Coredata.framework NSManagedObjectModel.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Fri Oct 2 11:46:33 EDT 2009 }


{$ifdef TYPES}
{$ifndef NSMANAGEDOBJECTMODEL_PAS_T}
{$define NSMANAGEDOBJECTMODEL_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSMANAGEDOBJECTMODEL_PAS_R}
{$define NSMANAGEDOBJECTMODEL_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSMANAGEDOBJECTMODEL_PAS_F}
{$define NSMANAGEDOBJECTMODEL_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSMANAGEDOBJECTMODEL_PAS_S}
{$define NSMANAGEDOBJECTMODEL_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSManagedObjectModel = objcclass;
  NSManagedObjectModelPointer = ^NSManagedObjectModel;

{$endif}

{$ifdef CLASSES}
{$ifndef NSMANAGEDOBJECTMODEL_PAS_C}
{$define NSMANAGEDOBJECTMODEL_PAS_C}

{ NSManagedObjectModel }
  NSManagedObjectModel = objcclass(NSObject, NSCodingProtocol, NSCopyingProtocol{, NSFastEnumerationProtocol})
  private
    __reserved: Pointer;
    __reserved2: Pointer;
    __localizationPolicy: id;
    __entities: NSMutableDictionary;
    __configurations: NSMutableDictionary;
    __fetchRequestTemplates: NSMutableDictionary;
    __versionIdentifiers: NSSet;
    __managedObjectModelFlags: bitpacked record
        _isInUse: 0..1;
        _isImmutable: 0..1;
        _reservedEntityDescription: 0..((1 shl 30)-1);
      end;
    
  public
    class function alloc: NSManagedObjectModel; message 'alloc';

    class function mergedModelFromBundles(bundles: NSArray): NSManagedObjectModel; message 'mergedModelFromBundles:';
    class function modelByMergingModels(models: NSArray): NSManagedObjectModel; message 'modelByMergingModels:';
    function init: id; message 'init';
    function initWithContentsOfURL(url: NSURL): id; message 'initWithContentsOfURL:';
    function entitiesByName: NSDictionary; message 'entitiesByName';
    function entities: NSArray; message 'entities';
    procedure setEntities(entities_: NSArray); message 'setEntities:';
    function configurations: NSArray; message 'configurations';
    function entitiesForConfiguration(configuration: NSString): NSArray; message 'entitiesForConfiguration:';
    procedure setEntities_forConfiguration(entities_: NSArray; configuration: NSString); message 'setEntities:forConfiguration:';
    procedure setFetchRequestTemplate_forName(fetchRequestTemplate: NSFetchRequest; name: NSString); message 'setFetchRequestTemplate:forName:';
    function fetchRequestTemplateForName(name: NSString): NSFetchRequest; message 'fetchRequestTemplateForName:';
    function fetchRequestFromTemplateWithName_substitutionVariables(name: NSString; variables: NSDictionary): NSFetchRequest; message 'fetchRequestFromTemplateWithName:substitutionVariables:';
    function localizationDictionary: NSDictionary; message 'localizationDictionary';
    procedure setLocalizationDictionary(localizationDictionary_: NSDictionary); message 'setLocalizationDictionary:';
    class function mergedModelFromBundles_forStoreMetadata(bundles: NSArray; metadata: NSDictionary): NSManagedObjectModel; message 'mergedModelFromBundles:forStoreMetadata:';
    class function modelByMergingModels_forStoreMetadata(models: NSArray; metadata: NSDictionary): NSManagedObjectModel; message 'modelByMergingModels:forStoreMetadata:';
    function fetchRequestTemplatesByName: NSDictionary; message 'fetchRequestTemplatesByName';
    function versionIdentifiers: NSSet; message 'versionIdentifiers';
    procedure setVersionIdentifiers(identifiers: NSSet); message 'setVersionIdentifiers:';
    function isConfiguration_compatibleWithStoreMetadata(configuration: NSString; metadata: NSDictionary): Boolean; message 'isConfiguration:compatibleWithStoreMetadata:';
    function entityVersionHashesByName: NSDictionary; message 'entityVersionHashesByName';
  end; external;

{$endif}
{$endif}
