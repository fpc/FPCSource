{ Parsed from Foundation.framework NSURLCache.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Thu Oct 1 18:54:07 ICT 2009 }


{$ifdef TYPES}
{$ifndef NSURLCACHE_PAS_T}
{$define NSURLCACHE_PAS_T}

{ Types }
type
  NSURLCacheStoragePolicy = NSUInteger;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSURLCACHE_PAS_R}
{$define NSURLCACHE_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSURLCACHE_PAS_F}
{$define NSURLCACHE_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSURLCACHE_PAS_S}
{$define NSURLCACHE_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSCachedURLResponse = objcclass;
  NSCachedURLResponsePointer = ^NSCachedURLResponse;
  NSURLCache = objcclass;
  NSURLCachePointer = ^NSURLCache;

{$endif}

{$ifdef CLASSES}
{$ifndef NSURLCACHE_PAS_C}
{$define NSURLCACHE_PAS_C}

{ NSCachedURLResponse }
  NSCachedURLResponse = objcclass(NSObject, NSCodingProtocol, NSCopyingProtocol)
  private
    __internal: NSCachedURLResponseInternal;
    
  public
    class function alloc: NSCachedURLResponse; message 'alloc';

    function initWithResponse_data(response_: NSURLResponse; data_: NSData): id; message 'initWithResponse:data:';
    function initWithResponse_data_userInfo_storagePolicy(response_: NSURLResponse; data_: NSData; userInfo_: NSDictionary; storagePolicy_: NSURLCacheStoragePolicy): id; message 'initWithResponse:data:userInfo:storagePolicy:';
    function response: NSURLResponse; message 'response';
    function data: NSData; message 'data';
    function userInfo: NSDictionary; message 'userInfo';
    function storagePolicy: NSURLCacheStoragePolicy; message 'storagePolicy';
  end; external;

{ NSURLCache }
  NSURLCache = objcclass(NSObject)
  private
    __internal: NSURLCacheInternal;
    
  public
    class function alloc: NSURLCache; message 'alloc';

    class function sharedURLCache: NSURLCache; message 'sharedURLCache';
    class procedure setSharedURLCache(cache: NSURLCache); message 'setSharedURLCache:';
    function initWithMemoryCapacity_diskCapacity_diskPath(memoryCapacity_: NSUInteger; diskCapacity_: NSUInteger; path: NSString): id; message 'initWithMemoryCapacity:diskCapacity:diskPath:';
    function cachedResponseForRequest(request: NSURLRequest): NSCachedURLResponse; message 'cachedResponseForRequest:';
    procedure storeCachedResponse_forRequest(cachedResponse: NSCachedURLResponse; request: NSURLRequest); message 'storeCachedResponse:forRequest:';
    procedure removeCachedResponseForRequest(request: NSURLRequest); message 'removeCachedResponseForRequest:';
    procedure removeAllCachedResponses; message 'removeAllCachedResponses';
    function memoryCapacity: NSUInteger; message 'memoryCapacity';
    function diskCapacity: NSUInteger; message 'diskCapacity';
    procedure setMemoryCapacity(memoryCapacity_: NSUInteger); message 'setMemoryCapacity:';
    procedure setDiskCapacity(diskCapacity_: NSUInteger); message 'setDiskCapacity:';
    function currentMemoryUsage: NSUInteger; message 'currentMemoryUsage';
    function currentDiskUsage: NSUInteger; message 'currentDiskUsage';
  end; external;

{$endif}
{$endif}
