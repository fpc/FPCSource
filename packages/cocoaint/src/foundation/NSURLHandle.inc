{ Parsed from Foundation.framework NSURLHandle.h }


{$ifdef TYPES}
type
  NSURLHandlePtr = ^NSURLHandle;
  NSURLHandleClientProtocolPtr = ^NSURLHandleClientProtocol;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSHTTPPropertyStatusCodeKey: NSString deprecated 'in 10_0, 10_4, NA, NA'; cvar; external;
  NSHTTPPropertyStatusReasonKey: NSString deprecated 'in 10_0, 10_4, NA, NA'; cvar; external;
  NSHTTPPropertyServerHTTPVersionKey: NSString deprecated 'in 10_0, 10_4, NA, NA'; cvar; external;
  NSHTTPPropertyRedirectionHeadersKey: NSString deprecated 'in 10_0, 10_4, NA, NA'; cvar; external;
  NSHTTPPropertyErrorPageDataKey: NSString deprecated 'in 10_0, 10_4, NA, NA'; cvar; external;
  NSHTTPPropertyHTTPProxy: NSString deprecated 'in 10_2, 10_4, NA, NA'; cvar; external;
  NSFTPPropertyUserLoginKey: NSString deprecated 'in 10_2, 10_4, NA, NA'; cvar; external;
  NSFTPPropertyUserPasswordKey: NSString deprecated 'in 10_2, 10_4, NA, NA'; cvar; external;
  NSFTPPropertyActiveTransferModeKey: NSString deprecated 'in 10_2, 10_4, NA, NA'; cvar; external;
  NSFTPPropertyFileOffsetKey: NSString deprecated 'in 10_2, 10_4, NA, NA'; cvar; external;
  NSFTPPropertyFTPProxy: NSString deprecated 'in 10_3, 10_4, NA, NA'; cvar; external;
{$endif}

{$ifdef TYPES}
type
  NSURLHandleStatus = NSUInteger;
  NSURLHandleStatusPtr = ^NSURLHandleStatus;

const
  NSURLHandleNotLoaded = 0;
  NSURLHandleLoadSucceeded = 1;
  NSURLHandleLoadInProgress = 2;
  NSURLHandleLoadFailed = 3;
{$endif}

{$ifdef PROTOCOLS}

type
  NSURLHandleClientProtocol = objcprotocol external name 'NSURLHandleClient'
    procedure URLHandle_resourceDataDidBecomeAvailable (sender: NSURLHandle; newBytes: NSData); message 'URLHandle:resourceDataDidBecomeAvailable:'; deprecated 'in 10_0, 10_4, NA, NA';
    procedure URLHandleResourceDidBeginLoading (sender: NSURLHandle); message 'URLHandleResourceDidBeginLoading:'; deprecated 'in 10_0, 10_4, NA, NA';
    procedure URLHandleResourceDidFinishLoading (sender: NSURLHandle); message 'URLHandleResourceDidFinishLoading:'; deprecated 'in 10_0, 10_4, NA, NA';
    procedure URLHandleResourceDidCancelLoading (sender: NSURLHandle); message 'URLHandleResourceDidCancelLoading:'; deprecated 'in 10_0, 10_4, NA, NA';
    procedure URLHandle_resourceDidFailLoadingWithReason (sender: NSURLHandle; reason: NSString); message 'URLHandle:resourceDidFailLoadingWithReason:'; deprecated 'in 10_0, 10_4, NA, NA';
  end;
{$endif}

{$ifdef CLASSES}

type
  NSURLHandle = objcclass external (NSObject)
  private
    _clients: NSMutableArray;
    _data: id;
    _status: NSURLHandleStatus;
    _reserved: NSInteger;
  public
    class procedure registerURLHandleClass (anURLHandleSubclass: pobjc_class); message 'registerURLHandleClass:'; deprecated 'in 10_0, 10_4, NA, NA';
    class function URLHandleClassForURL (anURL: NSURL): pobjc_class; message 'URLHandleClassForURL:'; deprecated 'in 10_0, 10_4, NA, NA';
    function status: NSURLHandleStatus; message 'status'; deprecated 'in 10_0, 10_4, NA, NA';
    function failureReason: NSString; message 'failureReason'; deprecated 'in 10_0, 10_4, NA, NA';
    procedure addClient (client: NSURLHandleClientProtocol); message 'addClient:'; deprecated 'in 10_0, 10_4, NA, NA';
    procedure removeClient (client: NSURLHandleClientProtocol); message 'removeClient:'; deprecated 'in 10_0, 10_4, NA, NA';
    procedure loadInBackground; message 'loadInBackground'; deprecated 'in 10_0, 10_4, NA, NA';
    procedure cancelLoadInBackground; message 'cancelLoadInBackground'; deprecated 'in 10_0, 10_4, NA, NA';
    function resourceData: NSData; message 'resourceData'; deprecated 'in 10_0, 10_4, NA, NA';
    function availableResourceData: NSData; message 'availableResourceData'; deprecated 'in 10_0, 10_4, NA, NA';
    function expectedResourceDataSize: clonglong; message 'expectedResourceDataSize'; deprecated 'in 10_3, 10_4, NA, NA';
    procedure flushCachedData; message 'flushCachedData'; deprecated 'in 10_0, 10_4, NA, NA';
    procedure backgroundLoadDidFailWithReason (reason: NSString); message 'backgroundLoadDidFailWithReason:'; deprecated 'in 10_0, 10_4, NA, NA';
    procedure didLoadBytes_loadComplete (newBytes: NSData; yorn: ObjCBOOL); message 'didLoadBytes:loadComplete:'; deprecated 'in 10_0, 10_4, NA, NA';
    class function canInitWithURL (anURL: NSURL): ObjCBOOL; message 'canInitWithURL:'; deprecated 'in 10_0, 10_4, NA, NA';
    class function cachedHandleForURL (anURL: NSURL): NSURLHandle; message 'cachedHandleForURL:'; deprecated 'in 10_0, 10_4, NA, NA';
    function propertyForKey (propertyKey: NSString): id; message 'propertyForKey:'; deprecated 'in 10_0, 10_4, NA, NA';
    function propertyForKeyIfAvailable (propertyKey: NSString): id; message 'propertyForKeyIfAvailable:'; deprecated 'in 10_0, 10_4, NA, NA';
    function writeProperty_forKey (propertyValue: id; propertyKey: NSString): ObjCBOOL; message 'writeProperty:forKey:'; deprecated 'in 10_0, 10_4, NA, NA';
    function writeData (data: NSData): ObjCBOOL; message 'writeData:'; deprecated 'in 10_0, 10_4, NA, NA';
    function loadInForeground: NSData; message 'loadInForeground'; deprecated 'in 10_0, 10_4, NA, NA';
    procedure beginLoadInBackground; message 'beginLoadInBackground'; deprecated 'in 10_0, 10_4, NA, NA';
    procedure endLoadInBackground; message 'endLoadInBackground'; deprecated 'in 10_0, 10_4, NA, NA';
  end;
{$endif}

