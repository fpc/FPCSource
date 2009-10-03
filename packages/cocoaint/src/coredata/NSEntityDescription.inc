{ Parsed from Coredata.framework NSEntityDescription.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Fri Oct 2 11:46:33 EDT 2009 }


{$ifdef TYPES}
{$ifndef NSENTITYDESCRIPTION_PAS_T}
{$define NSENTITYDESCRIPTION_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSENTITYDESCRIPTION_PAS_R}
{$define NSENTITYDESCRIPTION_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSENTITYDESCRIPTION_PAS_F}
{$define NSENTITYDESCRIPTION_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSENTITYDESCRIPTION_PAS_S}
{$define NSENTITYDESCRIPTION_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSEntityDescription = objcclass;
  NSEntityDescriptionPointer = ^NSEntityDescription;

{$endif}

{$ifdef CLASSES}
{$ifndef NSENTITYDESCRIPTION_PAS_C}
{$define NSENTITYDESCRIPTION_PAS_C}

{ NSEntityDescription }
  NSEntityDescription = objcclass(NSObject, NSCodingProtocol, NSCopyingProtocol{, NSFastEnumerationProtocol})
  private
    __cd_rc: longint;
    __reserved1: id;
    __versionHashModifier: NSString;
    __versionHash: NSData;
    __model: NSManagedObjectModel; {garbage collector: __weak }
    __classNameForEntity: NSString;
    __instanceClass: Pobjc_class;
    __name: NSString;
    __rootentity: NSEntityDescription; {garbage collector: __weak }
    __superentity: NSEntityDescription; {garbage collector: __weak }
    __subentities: NSMutableDictionary;
    __properties: NSMutableDictionary;
    __propertyMapping: id;
    __propertyRanges: NSRange; {garbage collector: __strong }
    __entityDescriptionFlags: bitpacked record
        _isAbstract: 0..1;
        _shouldValidateOnSave: 0..1;
        _isImmutable: 0..1;
        _isFlattened: 0..1;
        _skipValidation: 0..1;
        _reservedEntityDescription: 0..((1 shl 27)-1);
      end;
    __mappings: NSMutableDictionary;
    __userInfo: NSMutableDictionary;
    __flattenedSubentities: id;
    __kvcPropertyAccessors: id;
    __modelsReferenceIDForEntity: clong;
    
  public
    class function alloc: NSEntityDescription; message 'alloc';

    class function entityForName_inManagedObjectContext(entityName: NSString; context: NSManagedObjectContext): NSEntityDescription; message 'entityForName:inManagedObjectContext:';
    class function insertNewObjectForEntityForName_inManagedObjectContext(entityName: NSString; context: NSManagedObjectContext): id; message 'insertNewObjectForEntityForName:inManagedObjectContext:';
    function managedObjectModel: NSManagedObjectModel; message 'managedObjectModel';
    function managedObjectClassName: NSString; message 'managedObjectClassName';
    procedure setManagedObjectClassName(name_: NSString); message 'setManagedObjectClassName:';
    function name: NSString; message 'name';
    procedure setName(name_: NSString); message 'setName:';
    function isAbstract: Boolean; message 'isAbstract';
    procedure setAbstract(flag: Boolean); message 'setAbstract:';
    function subentitiesByName: NSDictionary; message 'subentitiesByName';
    function subentities: NSArray; message 'subentities';
    procedure setSubentities(array_: NSArray); message 'setSubentities:';
    function superentity: NSEntityDescription; message 'superentity';
    function propertiesByName: NSDictionary; message 'propertiesByName';
    function properties: NSArray; message 'properties';
    procedure setProperties(properties_: NSArray); message 'setProperties:';
    function userInfo: NSDictionary; message 'userInfo';
    procedure setUserInfo(dictionary: NSDictionary); message 'setUserInfo:';
    function attributesByName: NSDictionary; message 'attributesByName';
    function relationshipsByName: NSDictionary; message 'relationshipsByName';
    function relationshipsWithDestinationEntity(entity: NSEntityDescription): NSArray; message 'relationshipsWithDestinationEntity:';
    function isKindOfEntity(entity: NSEntityDescription): Boolean; message 'isKindOfEntity:';
    function versionHash: NSData; message 'versionHash';
    function versionHashModifier: NSString; message 'versionHashModifier';
    procedure setVersionHashModifier(modifierString: NSString); message 'setVersionHashModifier:';
  end; external;

{$endif}
{$endif}
