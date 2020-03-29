{ Parsed from Foundation.framework NSCache.h }


{$ifdef TYPES}
type
  NSCachePtr = ^NSCache;
  NSCacheDelegateProtocolPtr = ^NSCacheDelegateProtocol;
{$endif}

{$ifdef CLASSES}

type
  NSCache = objcclass external (NSObject)
  private
    _delegate: id;
    _private:array[0..4] of pointer;
    _reserved: pointer;
  public
    procedure setName(newValue: NSString); message 'setName:';
    function name: NSString; message 'name';
    procedure setDelegate(newValue: NSCacheDelegateProtocol); message 'setDelegate:';
    function delegate: NSCacheDelegateProtocol; message 'delegate';
    function objectForKey (key: id): id; message 'objectForKey:';
    procedure setObject_forKey (obj: id; key: id); message 'setObject:forKey:';
    procedure setObject_forKey_cost (obj: id; key: id; g: NSUInteger); message 'setObject:forKey:cost:';
    procedure removeObjectForKey (key: id); message 'removeObjectForKey:';
    procedure removeAllObjects; message 'removeAllObjects';
    procedure setTotalCostLimit(newValue: NSUInteger); message 'setTotalCostLimit:';
    function totalCostLimit: NSUInteger; message 'totalCostLimit';
    procedure setCountLimit(newValue: NSUInteger); message 'setCountLimit:';
    function countLimit: NSUInteger; message 'countLimit';
    procedure setEvictsObjectsWithDiscardedContent(newValue: ObjCBOOL); message 'setEvictsObjectsWithDiscardedContent:';
    function evictsObjectsWithDiscardedContent: ObjCBOOL; message 'evictsObjectsWithDiscardedContent';
  end;
{$endif}

{$ifdef PROTOCOLS}

type
  NSCacheDelegateProtocol = objcprotocol external name 'NSCacheDelegate' (NSObjectProtocol)
  optional
    procedure cache_willEvictObject (cache: NSCache; obj: id); message 'cache:willEvictObject:';
  end;
{$endif}

