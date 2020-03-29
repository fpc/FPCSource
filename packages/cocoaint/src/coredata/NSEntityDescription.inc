{ Parsed from CoreData.framework NSEntityDescription.h }


{$ifdef TYPES}
type
  NSEntityDescriptionPtr = ^NSEntityDescription;
{$endif}

{$ifdef CLASSES}

type
  NSEntityDescription = objcclass external (NSObject, NSCodingProtocol, NSCopyingProtocol, NSFastEnumerationProtocol)
  private
    _cd_rc: cint32;
    _snapshotClass: id;
    _versionHashModifier: NSString;
    _versionHash: NSData;
    _model: NSManagedObjectModel;
    _classNameForEntity: NSString;
    _instanceClass: pobjc_class;
    _name: NSString;
    _rootentity: NSEntityDescription;
    _superentity: NSEntityDescription;
    _subentities: NSMutableDictionary;
    _properties: NSMutableDictionary;
    _propertyMapping: id;
    _propertyRanges: NSRangePtr;
    _entityDescriptionFlags: bitpacked record
      case byte of
        0: (_anonBitField__entityDescriptionFlags0: cuint);
        1: (
          _isAbstract: 0..1;
          _shouldValidateOnSave: 0..1;
          _isImmutable: 0..1;
          _isFlattened: 0..1;
          _skipValidation: 0..1;
          _hasPropertiesIndexedBySpotlight: 0..1;
          _hasPropertiesStoredInTruthFile: 0..1;
          _rangesAreInDataBlob: 0..1;
          _hasAttributesWithExternalDataReferences: 0..1;
          _hasNonstandardPrimitiveProperties: 0..((1 shl 2)-1);
          _reservedEntityDescription: 0..((1 shl 21)-1);
        );
      end;
    _extraIvars: pointer;
    _userInfo: NSMutableDictionary;
    _flattenedSubentities: id;
    _kvcPropertyAccessors: idPtr;
    _modelsReferenceIDForEntity: clong;
  public
    class function entityForName_inManagedObjectContext (entityName: NSString; context: NSManagedObjectContext): NSEntityDescription; message 'entityForName:inManagedObjectContext:';
    class function insertNewObjectForEntityForName_inManagedObjectContext (entityName: NSString; context: NSManagedObjectContext): id; message 'insertNewObjectForEntityForName:inManagedObjectContext:';
    function managedObjectModel: NSManagedObjectModel; message 'managedObjectModel';
    procedure setManagedObjectClassName(newValue: NSString); message 'setManagedObjectClassName:';
    function managedObjectClassName: NSString; message 'managedObjectClassName';
    procedure setName(newValue: NSString); message 'setName:';
    function name: NSString; message 'name';
    procedure setAbstract(newValue: ObjCBOOL); message 'setAbstract:';
    function isAbstract: ObjCBOOL; message 'isAbstract';
    function subentitiesByName: NSDictionary; message 'subentitiesByName';
    procedure setSubentities(newValue: NSArray); message 'setSubentities:';
    function subentities: NSArray; message 'subentities';
    function superentity: NSEntityDescription; message 'superentity';
    function propertiesByName: NSDictionary; message 'propertiesByName';
    procedure setProperties(newValue: NSArray); message 'setProperties:';
    function properties: NSArray; message 'properties';
    procedure setUserInfo(newValue: NSDictionary); message 'setUserInfo:';
    function userInfo: NSDictionary; message 'userInfo';
    function attributesByName: NSDictionary; message 'attributesByName';
    function relationshipsByName: NSDictionary; message 'relationshipsByName';
    function relationshipsWithDestinationEntity (entity: NSEntityDescription): NSArray; message 'relationshipsWithDestinationEntity:';
    function isKindOfEntity (entity: NSEntityDescription): ObjCBOOL; message 'isKindOfEntity:'; { available in 10_5,3_0 }
    function versionHash: NSData; message 'versionHash';
    procedure setVersionHashModifier(newValue: NSString); message 'setVersionHashModifier:';
    function versionHashModifier: NSString; message 'versionHashModifier';
    procedure setRenamingIdentifier(newValue: NSString); message 'setRenamingIdentifier:';
    function renamingIdentifier: NSString; message 'renamingIdentifier';
    procedure setCompoundIndexes(newValue: NSArray); message 'setCompoundIndexes:';
    function compoundIndexes: NSArray; message 'compoundIndexes';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    function countByEnumeratingWithState_objects_count (state: NSFastEnumerationStatePtr; buffer: idPtr { variable size array of id }; len: NSUInteger): NSUInteger; message 'countByEnumeratingWithState:objects:count:';
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
  end;
{$endif}

