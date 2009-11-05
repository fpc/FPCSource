{ Parsed from Coredata.framework NSEntityMapping.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Fri Oct 2 11:46:33 EDT 2009 }


{$ifdef TYPES}
{$ifndef NSENTITYMAPPING_PAS_T}
{$define NSENTITYMAPPING_PAS_T}

{ Constants }

const
  NSUndefinedEntityMappingType = $00;
  NSCustomEntityMappingType = $01;
  NSAddEntityMappingType = $02;
  NSRemoveEntityMappingType = $03;
  NSCopyEntityMappingType = $04;
  NSTransformEntityMappingType = $05;

{ Types }
type
  NSEntityMappingType = NSUInteger;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSENTITYMAPPING_PAS_R}
{$define NSENTITYMAPPING_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSENTITYMAPPING_PAS_F}
{$define NSENTITYMAPPING_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSENTITYMAPPING_PAS_S}
{$define NSENTITYMAPPING_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSEntityMapping = objcclass;
  NSEntityMappingPointer = ^NSEntityMapping;

{$endif}

{$ifdef CLASSES}
{$ifndef NSENTITYMAPPING_PAS_C}
{$define NSENTITYMAPPING_PAS_C}

{ NSEntityMapping }
  NSEntityMapping = objcclass(NSObject)
  private
    __reserved: Pointer;
    __reserved1: Pointer;
    __reserved2: Pointer;
    __name: NSString;
    __mappingType: NSEntityMappingType;
    __sourceEntityName: NSString;
    __sourceEntityVersionHash: NSData;
    __destinationEntityName: NSString;
    __destinationEntityVersionHash: NSData;
    __sourceExpression: NSExpression;
    __userInfo: NSDictionary;
    __entityMigrationPolicyClassName: NSString;
    __attributeMappings: NSMutableArray;
    __relationshipMappings: NSMutableArray;
    __entityMappingFlags: bitpacked record
        _isInUse: 0..1;
        _reservedEntityMapping: 0..((1 shl 31)-1);
      end;
    
  public
    class function alloc: NSEntityMapping; message 'alloc';

    function name: NSString; message 'name';
    procedure setName(name_: NSString); message 'setName:';
    function mappingType: NSEntityMappingType; message 'mappingType';
    procedure setMappingType(type_: NSEntityMappingType); message 'setMappingType:';
    function sourceEntityName: NSString; message 'sourceEntityName';
    procedure setSourceEntityName(name_: NSString); message 'setSourceEntityName:';
    function sourceEntityVersionHash: NSData; message 'sourceEntityVersionHash';
    procedure setSourceEntityVersionHash(vhash: NSData); message 'setSourceEntityVersionHash:';
    function destinationEntityName: NSString; message 'destinationEntityName';
    procedure setDestinationEntityName(name_: NSString); message 'setDestinationEntityName:';
    function destinationEntityVersionHash: NSData; message 'destinationEntityVersionHash';
    procedure setDestinationEntityVersionHash(vhash: NSData); message 'setDestinationEntityVersionHash:';
    function attributeMappings: NSArray; message 'attributeMappings';
    procedure setAttributeMappings(mappings: NSArray); message 'setAttributeMappings:';
    function relationshipMappings: NSArray; message 'relationshipMappings';
    procedure setRelationshipMappings(mappings: NSArray); message 'setRelationshipMappings:';
    function sourceExpression: NSExpression; message 'sourceExpression';
    procedure setSourceExpression(source: NSExpression); message 'setSourceExpression:';
    function userInfo: NSDictionary; message 'userInfo';
    procedure setUserInfo(dict: NSDictionary); message 'setUserInfo:';
    function entityMigrationPolicyClassName: NSString; message 'entityMigrationPolicyClassName';
    procedure setEntityMigrationPolicyClassName(name_: NSString); message 'setEntityMigrationPolicyClassName:';
  end; external;

{$endif}
{$endif}
