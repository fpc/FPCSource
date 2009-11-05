{ Parsed from Coredata.framework NSFetchRequest.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Fri Oct 2 11:46:33 EDT 2009 }


{$ifdef TYPES}
{$ifndef NSFETCHREQUEST_PAS_T}
{$define NSFETCHREQUEST_PAS_T}

{ Constants }

const
  NSManagedObjectResultType = $00;
  NSManagedObjectIDResultType = $01;

{ Types }
type
  NSFetchRequestResultType = NSUInteger;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSFETCHREQUEST_PAS_R}
{$define NSFETCHREQUEST_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSFETCHREQUEST_PAS_F}
{$define NSFETCHREQUEST_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSFETCHREQUEST_PAS_S}
{$define NSFETCHREQUEST_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSFetchRequest = objcclass;
  NSFetchRequestPointer = ^NSFetchRequest;

{$endif}

{$ifdef CLASSES}
{$ifndef NSFETCHREQUEST_PAS_C}
{$define NSFETCHREQUEST_PAS_C}

{ NSFetchRequest }
  NSFetchRequest = objcclass(NSObject, NSCodingProtocol, NSCopyingProtocol)
  private
    __reserved: Pointer;
    __reserved2: Pointer;
    __reserved3: Pointer;
    __reserved4: Pointer;
    __entity: NSEntityDescription; {garbage collector: __weak }
    __predicate: NSPredicate;
    __sortDescriptors: NSArray;
    __inUseCounter: culong;
    __fetchLimit: culong;
    __affectedStores: NSArray;
    __relationshipKeyPathsForPrefetching: NSArray;
    __flags: bitpacked record
        autoDistinct: 0..1;
        includesSubentities: 0..1;
        includesPropertyValues: 0..1;
        resultType: 0..((1 shl 3)-1);
        returnsObjectsAsFaults: 0..1;
        excludePendingChanges: 0..1;
        _RESERVED: 0..((1 shl 24)-1);
      end;
    
  public
    class function alloc: NSFetchRequest; message 'alloc';

    function entity: NSEntityDescription; message 'entity';
    procedure setEntity(entity_: NSEntityDescription); message 'setEntity:';
    function predicate: NSPredicate; message 'predicate';
    procedure setPredicate(predicate_: NSPredicate); message 'setPredicate:';
    function sortDescriptors: NSArray; message 'sortDescriptors';
    procedure setSortDescriptors(sortDescriptors_: NSArray); message 'setSortDescriptors:';
    function fetchLimit: NSUInteger; message 'fetchLimit';
    procedure setFetchLimit(limit: NSUInteger); message 'setFetchLimit:';
    function affectedStores: NSArray; message 'affectedStores';
    procedure setAffectedStores(stores: NSArray); message 'setAffectedStores:';
    function resultType: NSFetchRequestResultType; message 'resultType';
    procedure setResultType(type_: NSFetchRequestResultType); message 'setResultType:';
    function includesSubentities: Boolean; message 'includesSubentities';
    procedure setIncludesSubentities(yesNo: Boolean); message 'setIncludesSubentities:';
    function includesPropertyValues: Boolean; message 'includesPropertyValues';
    procedure setIncludesPropertyValues(yesNo: Boolean); message 'setIncludesPropertyValues:';
    function returnsObjectsAsFaults: Boolean; message 'returnsObjectsAsFaults';
    procedure setReturnsObjectsAsFaults(yesNo: Boolean); message 'setReturnsObjectsAsFaults:';
    function relationshipKeyPathsForPrefetching: NSArray; message 'relationshipKeyPathsForPrefetching';
    procedure setRelationshipKeyPathsForPrefetching(keys: NSArray); message 'setRelationshipKeyPathsForPrefetching:';
  end; external;

{$endif}
{$endif}
