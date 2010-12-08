{ Parsed from Foundation.framework NSMetadata.h }
{ Version: 2.1.2 - Wed Dec 8 10:06:43 CET 2010 }


{$ifdef TYPES}
{$ifndef NSMETADATA_PAS_T}
{$define NSMETADATA_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSMETADATA_PAS_R}
{$define NSMETADATA_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSMETADATA_PAS_F}
{$define NSMETADATA_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSMETADATA_PAS_S}
{$define NSMETADATA_PAS_S}

{ External string constants }
var
  NSMetadataQueryDidStartGatheringNotification: NSString; cvar; external;
  NSMetadataQueryGatheringProgressNotification: NSString; cvar; external;
  NSMetadataQueryDidFinishGatheringNotification: NSString; cvar; external;
  NSMetadataQueryDidUpdateNotification: NSString; cvar; external;
  NSMetadataQueryResultContentRelevanceAttribute: NSString; cvar; external;
  NSMetadataQueryUserHomeScope: NSString; cvar; external;
  NSMetadataQueryLocalComputerScope: NSString; cvar; external;
  NSMetadataQueryNetworkScope: NSString; cvar; external;

{$endif}
{$endif}

{$ifdef FORWARD}
  NSMetadataQueryDelegateProtocol = objcprotocol;
  NSMetadataQuery = objcclass;
  NSMetadataQueryPointer = ^NSMetadataQuery;
  NSMetadataQueryPtr = NSMetadataQueryPointer;
  NSMetadataItem = objcclass;
  NSMetadataItemPointer = ^NSMetadataItem;
  NSMetadataItemPtr = NSMetadataItemPointer;
  NSMetadataQueryAttributeValueTuple = objcclass;
  NSMetadataQueryAttributeValueTuplePointer = ^NSMetadataQueryAttributeValueTuple;
  NSMetadataQueryAttributeValueTuplePtr = NSMetadataQueryAttributeValueTuplePointer;
  NSMetadataQueryResultGroup = objcclass;
  NSMetadataQueryResultGroupPointer = ^NSMetadataQueryResultGroup;
  NSMetadataQueryResultGroupPtr = NSMetadataQueryResultGroupPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSMETADATA_PAS_C}
{$define NSMETADATA_PAS_C}

{ NSMetadataQuery }
  NSMetadataQuery = objcclass(NSObject)
  private
    _flags: NSUInteger;
    _interval: NSTimeInterval;
        _private: array[0..10] of id;
    _reserved: Pointer; {garbage collector: __strong }
    
  public
    class function alloc: NSMetadataQuery; message 'alloc';

    function init: id; message 'init';
    function delegate: id; message 'delegate';
    procedure setDelegate(delegate_: id); message 'setDelegate:';
    function predicate: NSPredicate; message 'predicate';
    procedure setPredicate(predicate_: NSPredicate); message 'setPredicate:';
    function sortDescriptors: NSArray; message 'sortDescriptors';
    procedure setSortDescriptors(descriptors: NSArray); message 'setSortDescriptors:';
    function valueListAttributes: NSArray; message 'valueListAttributes';
    procedure setValueListAttributes(attrs: NSArray); message 'setValueListAttributes:';
    function groupingAttributes: NSArray; message 'groupingAttributes';
    procedure setGroupingAttributes(attrs: NSArray); message 'setGroupingAttributes:';
    function notificationBatchingInterval: NSTimeInterval; message 'notificationBatchingInterval';
    procedure setNotificationBatchingInterval(ti: NSTimeInterval); message 'setNotificationBatchingInterval:';
    function searchScopes: NSArray; message 'searchScopes';
    procedure setSearchScopes(scopes: NSArray); message 'setSearchScopes:';
    function startQuery: Boolean; message 'startQuery';
    procedure stopQuery; message 'stopQuery';
    function isStarted: Boolean; message 'isStarted';
    function isGathering: Boolean; message 'isGathering';
    function isStopped: Boolean; message 'isStopped';
    procedure disableUpdates; message 'disableUpdates';
    procedure enableUpdates; message 'enableUpdates';
    function resultCount: NSUInteger; message 'resultCount';
    function resultAtIndex(idx: NSUInteger): id; message 'resultAtIndex:';
    function results: NSArray; message 'results';
    function indexOfResult(result_: id): NSUInteger; message 'indexOfResult:';
    function valueLists: NSDictionary; message 'valueLists';
    function groupedResults: NSArray; message 'groupedResults';
    function valueOfAttribute_forResultAtIndex(attrName: NSString; idx: NSUInteger): id; message 'valueOfAttribute:forResultAtIndex:';
  end; external;

{ NSMetadataItem }
  NSMetadataItem = objcclass(NSObject)
  private
    _item: id;
    _reserved: Pointer; {garbage collector: __strong }
    
  public
    class function alloc: NSMetadataItem; message 'alloc';

    function valueForAttribute(key: NSString): id; message 'valueForAttribute:';
    function valuesForAttributes(keys: NSArray): NSDictionary; message 'valuesForAttributes:';
    function attributes: NSArray; message 'attributes';
  end; external;

{ NSMetadataQueryAttributeValueTuple }
  NSMetadataQueryAttributeValueTuple = objcclass(NSObject)
  private
    _attr: id;
    _value: id;
    _count: NSUInteger;
    _reserved: Pointer;
    
  public
    class function alloc: NSMetadataQueryAttributeValueTuple; message 'alloc';

    function attribute: NSString; message 'attribute';
    function value: id; message 'value';
    function count: NSUInteger; message 'count';
  end; external;

{ NSMetadataQueryResultGroup }
  NSMetadataQueryResultGroup = objcclass(NSObject)
  private
        _private: array[0..8] of id;
    _private2: NSUInteger;
    _reserved: Pointer;
    
  public
    class function alloc: NSMetadataQueryResultGroup; message 'alloc';

    function attribute: NSString; message 'attribute';
    function value: id; message 'value';
    function subgroups: NSArray; message 'subgroups';
    function resultCount: NSUInteger; message 'resultCount';
    function resultAtIndex(idx: NSUInteger): id; message 'resultAtIndex:';
    function results: NSArray; message 'results';
  end; external;

{$endif}
{$endif}
{$ifdef PROTOCOLS}
{$ifndef NSMETADATA_PAS_P}
{$define NSMETADATA_PAS_P}
  
{ NSMetadataQueryDelegate Protocol }
  NSMetadataQueryDelegateProtocol = objcprotocol
    function metadataQuery_replacementObjectForResultObject(query: NSMetadataQuery; result_: NSMetadataItem): id; message 'metadataQuery:replacementObjectForResultObject:';
    function metadataQuery_replacementValueForAttribute_value(query: NSMetadataQuery; attrName: NSString; attrValue: id): id; message 'metadataQuery:replacementValueForAttribute:value:';
  end; external name 'NSMetadataQueryDelegate';
{$endif}
{$endif}
