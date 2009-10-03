{ Parsed from Coredata.framework NSPropertyDescription.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Fri Oct 2 11:46:33 EDT 2009 }


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

{$endif}

{$ifdef CLASSES}
{$ifndef NSPROPERTYDESCRIPTION_PAS_C}
{$define NSPROPERTYDESCRIPTION_PAS_C}

{ NSPropertyDescription }
  NSPropertyDescription = objcclass(NSObject, NSCodingProtocol, NSCopyingProtocol)
  private
    __reserved: Pointer;
    __versionHashModifier: NSString;
    __versionHash: NSData;
    __entity: NSEntityDescription; {garbage collector: __weak }
    __name: NSString;
    __validationPredicates: NSArray;
    __validationWarnings: NSArray;
    __propertyDescriptionFlags: bitpacked record
        _isReadOnly: 0..1;
        _isTransient: 0..1;
        _isOptional: 0..1;
        _isIndexed: 0..1;
        _skipValidation: 0..1;
        _reservedPropertyDescription: 0..((1 shl 27)-1);
      end;
    __mappings: NSMutableDictionary;
    __userInfo: NSMutableDictionary;
    __entitysReferenceIDForProperty: clong;
    
  public
    class function alloc: NSPropertyDescription; message 'alloc';

    function entity: NSEntityDescription; message 'entity';
    function name: NSString; message 'name';
    procedure setName(name_: NSString); message 'setName:';
    function isOptional: Boolean; message 'isOptional';
    procedure setOptional(flag: Boolean); message 'setOptional:';
    function isTransient: Boolean; message 'isTransient';
    procedure setTransient(flag: Boolean); message 'setTransient:';
    function validationPredicates: NSArray; message 'validationPredicates';
    function validationWarnings: NSArray; message 'validationWarnings';
    procedure setValidationPredicates_withValidationWarnings(validationPredicates_: NSArray; validationWarnings_: NSArray); message 'setValidationPredicates:withValidationWarnings:';
    function userInfo: NSDictionary; message 'userInfo';
    procedure setUserInfo(dictionary: NSDictionary); message 'setUserInfo:';
    function isIndexed: Boolean; message 'isIndexed';
    procedure setIndexed(flag: Boolean); message 'setIndexed:';
    function versionHash: NSData; message 'versionHash';
    function versionHashModifier: NSString; message 'versionHashModifier';
    procedure setVersionHashModifier(modifierString: NSString); message 'setVersionHashModifier:';
  end; external;

{$endif}
{$endif}
