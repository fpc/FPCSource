{ Parsed from Coredata.framework NSPropertyDescription.h }

{$ifdef TYPES}
{$ifndef NSPROPERTYDESCRIPTION_PAS_T}
{$define NSPROPERTYDESCRIPTION_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSPROPERTYDESCRIPTION_PAS_R}
{$define NSPROPERTYDESCRIPTION_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSPROPERTYDESCRIPTION_PAS_F}
{$define NSPROPERTYDESCRIPTION_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSPROPERTYDESCRIPTION_PAS_S}
{$define NSPROPERTYDESCRIPTION_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSPropertyDescription = objcclass;
  NSPropertyDescriptionPointer = ^NSPropertyDescription;
  NSPropertyDescriptionPtr = NSPropertyDescriptionPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSPROPERTYDESCRIPTION_PAS_C}
{$define NSPROPERTYDESCRIPTION_PAS_C}

{ NSPropertyDescription }
  NSPropertyDescription = objcclass external (NSObject, NSCodingProtocol, NSCopyingProtocol)
  private
    _reserved: Pointer;
    _versionHashModifier: NSString;
    _versionHash: NSData;
    _entity: NSEntityDescription; {garbage collector: __weak }
    _name: NSString;
    _validationPredicates: NSArray;
    _validationWarnings: NSArray;
    __propertyDescriptionFlags: record
      case byte of
       0: (_anonbitfield_NSPropertyDescription0: cuint);
       1: (data: bitpacked record
        _isReadOnly: 0..1;
        _isTransient: 0..1;
        _isOptional: 0..1;
        _isIndexed: 0..1;
        _skipValidation: 0..1;
        _isIndexedBySpotlight: 0..1;
        _isStoredInExternalRecord: 0..1;
        _reservedPropertyDescription: 0..((1 shl 25)-1);
       end;
      );
      end;
    _extraIvars: Pointer; {garbage collector: __strong }
    _userInfo: NSMutableDictionary;
    _entitysReferenceIDForProperty: clong;
    
  public
    function entity: NSEntityDescription; message 'entity';
    function name: NSString; message 'name';
    procedure setName(name_: NSString); message 'setName:';
    function isOptional: ObjCBOOL; message 'isOptional';
    procedure setOptional(flag: ObjCBOOL); message 'setOptional:';
    function isTransient: ObjCBOOL; message 'isTransient';
    procedure setTransient(flag: ObjCBOOL); message 'setTransient:';
    function validationPredicates: NSArray; message 'validationPredicates';
    function validationWarnings: NSArray; message 'validationWarnings';
    procedure setValidationPredicates_withValidationWarnings(validationPredicates_: NSArray; validationWarnings_: NSArray); message 'setValidationPredicates:withValidationWarnings:';
    function userInfo: NSDictionary; message 'userInfo';
    procedure setUserInfo(dictionary: NSDictionary); message 'setUserInfo:';
    function isIndexed: ObjCBOOL; message 'isIndexed';
    procedure setIndexed(flag: ObjCBOOL); message 'setIndexed:';
    function versionHash: NSData; message 'versionHash';
    function versionHashModifier: NSString; message 'versionHashModifier';
    procedure setVersionHashModifier(modifierString: NSString); message 'setVersionHashModifier:';
    function isIndexedBySpotlight: ObjCBOOL; message 'isIndexedBySpotlight';
    procedure setIndexedBySpotlight(flag: ObjCBOOL); message 'setIndexedBySpotlight:';
    function isStoredInExternalRecord: ObjCBOOL; message 'isStoredInExternalRecord';
    procedure setStoredInExternalRecord(flag: ObjCBOOL); message 'setStoredInExternalRecord:';
    function renamingIdentifier: NSString; message 'renamingIdentifier';
    procedure setRenamingIdentifier(value: NSString); message 'setRenamingIdentifier:';

    { Adopted Protocols }
    procedure encodeWithCoder(aCoder: NSCoder);
    function initWithCoder(aDecoder: NSCoder): id;
    function copyWithZone(zone_: NSZonePtr): id;
  end;

{$endif}
{$endif}
