{ Parsed from Foundation.framework NSMetadata.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Tue Sep 8 15:31:00 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSMETADATA_PAS_H}
{$define NSMETADATA_PAS_H}
type
  NSMetadataQueryPointer = Pointer;
  NSMetadataItemPointer = Pointer;
  NSMetadataQueryAttributeValueTuplePointer = Pointer;
  NSMetadataQueryResultGroupPointer = Pointer;

{$endif}
{$endif}

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
{$ifndef NSMETADATA_PAS_T}
{$define NSMETADATA_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSMetadataQuery = objcclass;
  NSMetadataItem = objcclass;
  NSMetadataQueryAttributeValueTuple = objcclass;
  NSMetadataQueryResultGroup = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSMETADATA_PAS_C}
{$define NSMETADATA_PAS_C}

{ NSMetadataQuery }
  NSMetadataQuery = objcclass(NSObject)
  private
    __flags: culong;
    __interval: NSTimeInterval;
    __private: id;
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
    function resultCount: culong; message 'resultCount';
    function resultAtIndex(idx: culong): id; message 'resultAtIndex:';
    function results: NSArray; message 'results';
    function indexOfResult(result_: id): culong; message 'indexOfResult:';
    function valueLists: NSDictionary; message 'valueLists';
    function groupedResults: NSArray; message 'groupedResults';
    function valueOfAttribute_forResultAtIndex(attrName: NSString; idx: culong): id; message 'valueOfAttribute:forResultAtIndex:';
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
    __count: culong;
    __reserved: Pointer;
    
  public
    class function alloc: NSMetadataQueryAttributeValueTuple; message 'alloc';

    function attribute: NSString; message 'attribute';
    function value: id; message 'value';
    function count: culong; message 'count';
  end; external;

{ NSMetadataQueryResultGroup }
  NSMetadataQueryResultGroup = objcclass(NSObject)
  private
    __private: id;
    __private2: culong;
    __reserved: Pointer;
    
  public
    class function alloc: NSMetadataQueryResultGroup; message 'alloc';

    function attribute: NSString; message 'attribute';
    function value: id; message 'value';
    function subgroups: NSArray; message 'subgroups';
    function resultCount: culong; message 'resultCount';
    function resultAtIndex(idx: culong): id; message 'resultAtIndex:';
    function results: NSArray; message 'results';
  end; external;

{$endif}
{$endif}
