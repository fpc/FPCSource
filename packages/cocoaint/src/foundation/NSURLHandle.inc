{ Parsed from Foundation.framework NSURLHandle.h }

{$ifdef TYPES}
{$ifndef NSURLHANDLE_PAS_T}
{$define NSURLHANDLE_PAS_T}

{ Constants }

const
  NSURLHandleNotLoaded = 0;
  NSURLHandleLoadSucceeded = 1;
  NSURLHandleLoadInProgress = 2;
  NSURLHandleLoadFailed = 3;

{ Types }
type
  NSURLHandleStatus = NSUInteger;
  NSURLHandleStatusPtr = ^NSURLHandleStatus;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSURLHANDLE_PAS_R}
{$define NSURLHANDLE_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSURLHANDLE_PAS_F}
{$define NSURLHANDLE_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSURLHANDLE_PAS_S}
{$define NSURLHANDLE_PAS_S}

{ External string constants }
var
  NSHTTPPropertyStatusCodeKey: NSString deprecated 'in Mac OS X 10.4 and later'; cvar; external;
  NSHTTPPropertyStatusReasonKey: NSString deprecated 'in Mac OS X 10.4 and later'; cvar; external;
  NSHTTPPropertyServerHTTPVersionKey: NSString deprecated 'in Mac OS X 10.4 and later'; cvar; external;
  NSHTTPPropertyRedirectionHeadersKey: NSString deprecated 'in Mac OS X 10.4 and later'; cvar; external;
  NSHTTPPropertyErrorPageDataKey: NSString deprecated 'in Mac OS X 10.4 and later'; cvar; external;
  NSHTTPPropertyHTTPProxy: NSString deprecated 'in Mac OS X 10.4 and later'; cvar; external;
  NSFTPPropertyUserLoginKey: NSString deprecated 'in Mac OS X 10.4 and later'; cvar; external;
  NSFTPPropertyUserPasswordKey: NSString deprecated 'in Mac OS X 10.4 and later'; cvar; external;
  NSFTPPropertyActiveTransferModeKey: NSString deprecated 'in Mac OS X 10.4 and later'; cvar; external;
  NSFTPPropertyFileOffsetKey: NSString deprecated 'in Mac OS X 10.4 and later'; cvar; external;
  NSFTPPropertyFTPProxy: NSString deprecated 'in Mac OS X 10.4 and later'; cvar; external;

{$endif}
{$endif}

{$ifdef FORWARD}
  NSURLHandleClientProtocol = objcprotocol;
  NSURLHandle = objcclass;
  NSURLHandlePointer = ^NSURLHandle;
  NSURLHandlePtr = NSURLHandlePointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSURLHANDLE_PAS_C}
{$define NSURLHANDLE_PAS_C}

{ NSURLHandle }
  NSURLHandle = objcclass external (NSObject)
  private
    _clients: NSMutableArray;
    _data: id;
    _status: NSURLHandleStatus;
    _reserved: NSInteger;
    
  public
    class procedure registerURLHandleClass(anURLHandleSubclass: Pobjc_class); message 'registerURLHandleClass:'; deprecated 'in Mac OS X 10.4 and later';
    class function URLHandleClassForURL(anURL: NSURL): Pobjc_class; message 'URLHandleClassForURL:'; deprecated 'in Mac OS X 10.4 and later';
    function status: NSURLHandleStatus; message 'status'; deprecated 'in Mac OS X 10.4 and later';
    function failureReason: NSString; message 'failureReason'; deprecated 'in Mac OS X 10.4 and later';
    procedure addClient(client: NSURLHandleClientProtocol); message 'addClient:'; deprecated 'in Mac OS X 10.4 and later';
    procedure removeClient(client: NSURLHandleClientProtocol); message 'removeClient:'; deprecated 'in Mac OS X 10.4 and later';
    procedure loadInBackground; message 'loadInBackground'; deprecated 'in Mac OS X 10.4 and later';
    procedure cancelLoadInBackground; message 'cancelLoadInBackground'; deprecated 'in Mac OS X 10.4 and later';
    function resourceData: NSData; message 'resourceData'; deprecated 'in Mac OS X 10.4 and later';
    function availableResourceData: NSData; message 'availableResourceData'; deprecated 'in Mac OS X 10.4 and later';
    function expectedResourceDataSize: clonglong; message 'expectedResourceDataSize'; deprecated 'in Mac OS X 10.4 and later';
    procedure flushCachedData; message 'flushCachedData'; deprecated 'in Mac OS X 10.4 and later';
    procedure backgroundLoadDidFailWithReason(reason: NSString); message 'backgroundLoadDidFailWithReason:'; deprecated 'in Mac OS X 10.4 and later';
    procedure didLoadBytes_loadComplete(newBytes: NSData; yorn: Boolean); message 'didLoadBytes:loadComplete:'; deprecated 'in Mac OS X 10.4 and later';
    class function canInitWithURL(anURL: NSURL): Boolean; message 'canInitWithURL:'; deprecated 'in Mac OS X 10.4 and later';
    class function cachedHandleForURL(anURL: NSURL): NSURLHandle; message 'cachedHandleForURL:'; deprecated 'in Mac OS X 10.4 and later';
    function propertyForKey(propertyKey: NSString): id; message 'propertyForKey:'; deprecated 'in Mac OS X 10.4 and later';
    function propertyForKeyIfAvailable(propertyKey: NSString): id; message 'propertyForKeyIfAvailable:'; deprecated 'in Mac OS X 10.4 and later';
    function writeProperty_forKey(propertyValue: id; propertyKey: NSString): Boolean; message 'writeProperty:forKey:'; deprecated 'in Mac OS X 10.4 and later';
    function writeData(data: NSData): Boolean; message 'writeData:'; deprecated 'in Mac OS X 10.4 and later';
    function loadInForeground: NSData; message 'loadInForeground'; deprecated 'in Mac OS X 10.4 and later';
    procedure beginLoadInBackground; message 'beginLoadInBackground'; deprecated 'in Mac OS X 10.4 and later';
    procedure endLoadInBackground; message 'endLoadInBackground'; deprecated 'in Mac OS X 10.4 and later';
  end;

{$endif}
{$endif}
{$ifdef PROTOCOLS}
{$ifndef NSURLHANDLE_PAS_P}
{$define NSURLHANDLE_PAS_P}
  
{ NSURLHandleClient Protocol }
  NSURLHandleClientProtocol = objcprotocol external name 'NSURLHandleClient'
    procedure URLHandle_resourceDataDidBecomeAvailable(sender: NSURLHandle; newBytes: NSData); message 'URLHandle:resourceDataDidBecomeAvailable:'; deprecated 'in Mac OS X 10.4 and later';
    procedure URLHandleResourceDidBeginLoading(sender: NSURLHandle); message 'URLHandleResourceDidBeginLoading:'; deprecated 'in Mac OS X 10.4 and later';
    procedure URLHandleResourceDidFinishLoading(sender: NSURLHandle); message 'URLHandleResourceDidFinishLoading:'; deprecated 'in Mac OS X 10.4 and later';
    procedure URLHandleResourceDidCancelLoading(sender: NSURLHandle); message 'URLHandleResourceDidCancelLoading:'; deprecated 'in Mac OS X 10.4 and later';
    procedure URLHandle_resourceDidFailLoadingWithReason(sender: NSURLHandle; reason: NSString); message 'URLHandle:resourceDidFailLoadingWithReason:'; deprecated 'in Mac OS X 10.4 and later';
  end;
{$endif}
{$endif}
