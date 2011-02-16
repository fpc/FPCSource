{ Parsed from Coredata.framework NSRelationshipDescription.h }

{$ifdef TYPES}
{$ifndef NSRELATIONSHIPDESCRIPTION_PAS_T}
{$define NSRELATIONSHIPDESCRIPTION_PAS_T}

{ Constants }

const
  NSNoActionDeleteRule = 0;
  NSNullifyDeleteRule = 1;
  NSCascadeDeleteRule = 2;
  NSDenyDeleteRule = 3;

{ Types }
type
  NSDeleteRule = NSUInteger;
  NSDeleteRulePtr = ^NSDeleteRule;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSRELATIONSHIPDESCRIPTION_PAS_R}
{$define NSRELATIONSHIPDESCRIPTION_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSRELATIONSHIPDESCRIPTION_PAS_F}
{$define NSRELATIONSHIPDESCRIPTION_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSRELATIONSHIPDESCRIPTION_PAS_S}
{$define NSRELATIONSHIPDESCRIPTION_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSRelationshipDescription = objcclass;
  NSRelationshipDescriptionPointer = ^NSRelationshipDescription;
  NSRelationshipDescriptionPtr = NSRelationshipDescriptionPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSRELATIONSHIPDESCRIPTION_PAS_C}
{$define NSRELATIONSHIPDESCRIPTION_PAS_C}

{ NSRelationshipDescription }
  NSRelationshipDescription = objcclass external (NSPropertyDescription)
  private
    _reserved5: Pointer;
    _reserved6: Pointer;
    _destinationEntity: NSEntityDescription; {garbage collector: __weak }
    _lazyDestinationEntityName: NSString;
    _inverseRelationship: NSRelationshipDescription;
    _lazyInverseRelationshipName: NSString;
    _maxCount: culong;
    _minCount: culong;
    _deleteRule: NSDeleteRule;
    
  public
    function destinationEntity: NSEntityDescription; message 'destinationEntity';
    procedure setDestinationEntity(entity_: NSEntityDescription); message 'setDestinationEntity:';
    function inverseRelationship: NSRelationshipDescription; message 'inverseRelationship';
    procedure setInverseRelationship(relationship: NSRelationshipDescription); message 'setInverseRelationship:';
    function maxCount: NSUInteger; message 'maxCount';
    procedure setMaxCount(maxCount_: NSUInteger); message 'setMaxCount:';
    function minCount: NSUInteger; message 'minCount';
    procedure setMinCount(minCount_: NSUInteger); message 'setMinCount:';
    function deleteRule: NSDeleteRule; message 'deleteRule';
    procedure setDeleteRule(rule: NSDeleteRule); message 'setDeleteRule:';
    function isToMany: Boolean; message 'isToMany';
    function versionHash: NSData; message 'versionHash';
  end;

{$endif}
{$endif}
