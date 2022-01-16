{ Parsed from CoreData.framework NSPersistentStoreRequest.h }


{$ifdef TYPES}
type
  NSPersistentStoreRequestPtr = ^NSPersistentStoreRequest;
  NSAsynchronousFetchRequestPtr = ^NSAsynchronousFetchRequest;
{$endif}

{$ifdef TYPES}
type
  NSPersistentStoreRequestType = NSUInteger;
  NSPersistentStoreRequestTypePtr = ^NSPersistentStoreRequestType;

const
  NSFetchRequestType = 1;
  NSSaveRequestType = 2;
  NSBatchUpdateRequestType = 6;
{$endif}

{$ifdef CLASSES}

type
  NSPersistentStoreRequest = objcclass external (NSObject, NSCopyingProtocol)
  private
    _affectedStores: NSArray;
  public
    procedure setAffectedStores(newValue: NSArray); message 'setAffectedStores:';
    function affectedStores: NSArray; message 'affectedStores';
    function requestType: NSPersistentStoreRequestType; message 'requestType';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
  end;
{$endif}

{$ifdef TYPES}
type
  NSPersistentStoreAsynchronousFetchResultCompletionBlock = OpaqueCBlock;
{$endif}

{$ifdef CLASSES}

type
  NSAsynchronousFetchRequest = objcclass external (NSPersistentStoreRequest)
  private
    _fetchRequest: NSFetchRequest;
    _requestCompletionBlock: id;
    _estimatedResultCount: NSInteger;
  public
    function fetchRequest: NSFetchRequest; message 'fetchRequest';
    function completionBlock: NSPersistentStoreAsynchronousFetchResultCompletionBlock; message 'completionBlock';
    procedure setEstimatedResultCount(newValue: NSInteger); message 'setEstimatedResultCount:';
    function estimatedResultCount: NSInteger; message 'estimatedResultCount';
    function initWithFetchRequest_completionBlock (request: NSFetchRequest; blk: NSPersistentStoreAsynchronousFetchResultCompletionBlock): instancetype; message 'initWithFetchRequest:completionBlock:';
  end;
{$endif}

