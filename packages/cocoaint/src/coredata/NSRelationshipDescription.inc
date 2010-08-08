{ Parsed from Coredata.framework NSRelationshipDescription.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Fri Oct 2 11:46:33 EDT 2009 }


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

{$endif}

{$ifdef CLASSES}
{$ifndef NSRELATIONSHIPDESCRIPTION_PAS_C}
{$define NSRELATIONSHIPDESCRIPTION_PAS_C}

{ NSRelationshipDescription }
  NSRelationshipDescription = objcclass(NSPropertyDescription)
  private
    __reserved5: Pointer;
    __reserved6: Pointer;
    __destinationEntity: NSEntityDescription; {garbage collector: __weak }
    __lazyDestinationEntityName: NSString;
    __inverseRelationship: NSRelationshipDescription;
    __lazyInverseRelationshipName: NSString;
    __maxCount: culong;
    __minCount: culong;
    __deleteRule: NSDeleteRule;
    
  public
    class function alloc: NSRelationshipDescription; message 'alloc';

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
  end; external;

{$endif}
{$endif}
