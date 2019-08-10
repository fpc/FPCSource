{ Parsed from CoreData.framework NSPersistentStoreResult.h }


{$ifdef TYPES}
type
  NSPersistentStoreResultPtr = ^NSPersistentStoreResult;
  NSPersistentStoreAsynchronousResultPtr = ^NSPersistentStoreAsynchronousResult;
  NSBatchUpdateResultPtr = ^NSBatchUpdateResult;
  NSAsynchronousFetchResultPtr = ^NSAsynchronousFetchResult;
{$endif}

{$ifdef TYPES}
type
  NSBatchUpdateRequestResultType = NSUInteger;
  NSBatchUpdateRequestResultTypePtr = ^NSBatchUpdateRequestResultType;

const
  NSStatusOnlyResultType = $0;
  NSUpdatedObjectIDsResultType = $1;
  NSUpdatedObjectsCountResultType = $2;
{$endif}

{$ifdef CLASSES}

type
  NSPersistentStoreResult = objcclass external (NSObject)
  end;

type
  NSPersistentStoreAsynchronousResult = objcclass external (NSPersistentStoreResult)
  private
    _requestProgress: NSProgress;
    _requestError: NSError;
    _requestContext: NSManagedObjectContext;
    _requestCompletionBlock: id;
    _flags: cint32;
  public
    function managedObjectContext: NSManagedObjectContext; message 'managedObjectContext';
    function operationError: NSError; message 'operationError';
    function progress: NSProgress; message 'progress';
    procedure cancel; message 'cancel';
  end;

type
  NSBatchUpdateResult = objcclass external (NSPersistentStoreResult)
  private
    _aggregatedResult: id;
    _resultType: NSBatchUpdateRequestResultType;
  public
    function result_: id; message 'result';
    function resultType: NSBatchUpdateRequestResultType; message 'resultType';
  end;

type
  NSAsynchronousFetchResult = objcclass external (NSPersistentStoreAsynchronousResult)
  private
    _fetchRequest: NSAsynchronousFetchRequest;
    _finalResult: NSArray;
    _intermediateResultCallback: id;
  public
    function fetchRequest: NSAsynchronousFetchRequest; message 'fetchRequest';
    function finalResult: NSArray; message 'finalResult';
  end;
{$endif}

