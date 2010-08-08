{ Parsed from Foundation.framework NSMetadata.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Thu Oct 1 18:54:06 ICT 2009 }


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
  NSMetadataQueryDidStartGatheringNotification: NSString; external name '_NSMetadataQueryDidStartGatheringNotification';
  NSMetadataQueryGatheringProgressNotification: NSString; external name '_NSMetadataQueryGatheringProgressNotification';
  NSMetadataQueryDidFinishGatheringNotification: NSString; external name '_NSMetadataQueryDidFinishGatheringNotification';
  NSMetadataQueryDidUpdateNotification: NSString; external name '_NSMetadataQueryDidUpdateNotification';
  NSMetadataQueryResultContentRelevanceAttribute: NSString; external name '_NSMetadataQueryResultContentRelevanceAttribute';
  NSMetadataQueryUserHomeScope: NSString; external name '_NSMetadataQueryUserHomeScope';
  NSMetadataQueryLocalComputerScope: NSString; external name '_NSMetadataQueryLocalComputerScope';
  NSMetadataQueryNetworkScope: NSString; external name '_NSMetadataQueryNetworkScope';

{$endif}
{$endif}

{$ifdef FORWARD}
  NSMetadataQuery = objcclass;
  NSMetadataQueryPointer = ^NSMetadataQuery;
  NSMetadataItem = objcclass;
  NSMetadataItemPointer = ^NSMetadataItem;
  NSMetadataQueryAttributeValueTuple = objcclass;
  NSMetadataQueryAttributeValueTuplePointer = ^NSMetadataQueryAttributeValueTuple;
  NSMetadataQueryResultGroup = objcclass;
  NSMetadataQueryResultGroupPointer = ^NSMetadataQueryResultGroup;

{$endif}

{$ifdef CLASSES}
{$ifndef NSMETADATA_PAS_C}
{$define NSMETADATA_PAS_C}

{ NSMetadataQuery }
  NSMetadataQuery = objcclass(NSObject)
  private
    __flags: NSUInteger;
    __interval: NSTimeInterval;
        _private: array[0..10] of id;
    __reserved: Pointer;
    
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
    __item: id;
    __reserved: Pointer;
    
  public
    class function alloc: NSMetadataItem; message 'alloc';

    function valueForAttribute(key: NSString): id; message 'valueForAttribute:';
    function valuesForAttributes(keys: NSArray): NSDictionary; message 'valuesForAttributes:';
    function attributes: NSArray; message 'attributes';
  end; external;

{ NSMetadataQueryAttributeValueTuple }
  NSMetadataQueryAttributeValueTuple = objcclass(NSObject)
  private
    __attr: id;
    __value: id;
    __count: NSUInteger;
    __reserved: Pointer;
    
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
    __private2: NSUInteger;
    __reserved: Pointer;
    
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
