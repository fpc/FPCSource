{ Parsed from Foundation.framework NSURLCache.h }


{$ifdef TYPES}
type
  NSCachedURLResponsePtr = ^NSCachedURLResponse;
  NSURLCachePtr = ^NSURLCache;
{$endif}

{$ifdef TYPES}
type
  NSURLCacheStoragePolicy = NSUInteger;
  NSURLCacheStoragePolicyPtr = ^NSURLCacheStoragePolicy;

const
  NSURLCacheStorageAllowed = 0;
  NSURLCacheStorageAllowedInMemoryOnly = 1;
  NSURLCacheStorageNotAllowed = 2;
{$endif}

{$ifdef CLASSES}

type
  NSCachedURLResponse = objcclass external (NSObject, NSSecureCodingProtocol, NSCopyingProtocol)
  private
    _internal: NSCachedURLResponseInternal;
  public
    function initWithResponse_data (response: NSURLResponse; data: NSData): instancetype; message 'initWithResponse:data:';
    function initWithResponse_data_userInfo_storagePolicy (response: NSURLResponse; data: NSData; userInfo: NSDictionary; storagePolicy: NSURLCacheStoragePolicy): instancetype; message 'initWithResponse:data:userInfo:storagePolicy:';
    function response: NSURLResponse; message 'response';
    function data: NSData; message 'data';
    function userInfo: NSDictionary; message 'userInfo';
    function storagePolicy: NSURLCacheStoragePolicy; message 'storagePolicy';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
    class function supportsSecureCoding: ObjCBOOL; message 'supportsSecureCoding';
  end;

type
  NSURLCache = objcclass external (NSObject)
  private
    _internal: NSURLCacheInternal;
  public
    class function sharedURLCache: NSURLCache; message 'sharedURLCache';
    class procedure setSharedURLCache (cache: NSURLCache); message 'setSharedURLCache:';
    function initWithMemoryCapacity_diskCapacity_diskPath (memoryCapacity: NSUInteger; diskCapacity: NSUInteger; path: NSString): instancetype; message 'initWithMemoryCapacity:diskCapacity:diskPath:';
    function cachedResponseForRequest (request: NSURLRequest): NSCachedURLResponse; message 'cachedResponseForRequest:';
    procedure storeCachedResponse_forRequest (cachedResponse: NSCachedURLResponse; request: NSURLRequest); message 'storeCachedResponse:forRequest:';
    procedure removeCachedResponseForRequest (request: NSURLRequest); message 'removeCachedResponseForRequest:';
    procedure removeAllCachedResponses; message 'removeAllCachedResponses';
    procedure removeCachedResponsesSinceDate (date: NSDate); message 'removeCachedResponsesSinceDate:'; { available in 10_10, 8_0 }
    procedure setMemoryCapacity(newValue: NSUInteger); message 'setMemoryCapacity:';
    function memoryCapacity: NSUInteger; message 'memoryCapacity';
    procedure setDiskCapacity(newValue: NSUInteger); message 'setDiskCapacity:';
    function diskCapacity: NSUInteger; message 'diskCapacity';
    function currentMemoryUsage: NSUInteger; message 'currentMemoryUsage';
    function currentDiskUsage: NSUInteger; message 'currentDiskUsage';
  end;


type
  NSURLCache_NSURLSessionTaskAdditions = objccategory external name 'NSURLSessionTaskAdditions' (NSURLCache)
    procedure storeCachedResponse_forDataTask (cachedResponse: NSCachedURLResponse; dataTask: NSURLSessionDataTask); message 'storeCachedResponse:forDataTask:'; { available in 10_10, 8_0 }
    procedure getCachedResponseForDataTask_completionHandler (dataTask: NSURLSessionDataTask; completionHandler: OpaqueCBlock); message 'getCachedResponseForDataTask:completionHandler:'; { available in 10_10, 8_0 }
    procedure removeCachedResponseForDataTask (dataTask: NSURLSessionDataTask); message 'removeCachedResponseForDataTask:'; { available in 10_10, 8_0 }
  end;
{$endif}

