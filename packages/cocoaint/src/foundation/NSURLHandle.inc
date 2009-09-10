{ Parsed from Foundation.framework NSURLHandle.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Tue Sep 8 15:31:00 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSURLHANDLE_PAS_H}
{$define NSURLHANDLE_PAS_H}
type
  NSURLHandlePointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSURLHANDLE_PAS_T}
{$define NSURLHANDLE_PAS_T}

{ Constants }

const
  NSURLHandleNotLoaded = 0;
  NSURLHandleLoadSucceeded = 0;
  NSURLHandleLoadInProgress = 1;
  NSURLHandleLoadFailed = 2;

{ Types }
type
  NSURLHandleStatus = culong;

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
{$ifndef NSURLHANDLE_PAS_T}
{$define NSURLHANDLE_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSURLHandleClientProtocol = objcprotocol;
  NSURLHandle = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSURLHANDLE_PAS_C}
{$define NSURLHANDLE_PAS_C}

{ NSURLHandle }
  NSURLHandle = objcclass(NSObject)
  private
    __clients: NSMutableArray;
    __data: id;
    __status: NSURLHandleStatus;
    __reserved: clong;
    
  public
    class function alloc: NSURLHandle; message 'alloc';

    class procedure registerURLHandleClass(anURLHandleSubclass: Pobjc_class); message 'registerURLHandleClass:';
    class function URLHandleClassForURL(anURL: NSURL): Pobjc_class; message 'URLHandleClassForURL:';
    procedure addClient(client: id); message 'addClient:';
    procedure removeClient(client: id); message 'removeClient:';
    procedure backgroundLoadDidFailWithReason(reason: NSString); message 'backgroundLoadDidFailWithReason:';
    procedure didLoadBytes_loadComplete(newBytes: NSData; yorn: Boolean); message 'didLoadBytes:loadComplete:';
    class function canInitWithURL(anURL: NSURL): Boolean; message 'canInitWithURL:';
    class function cachedHandleForURL(anURL: NSURL): NSURLHandle; message 'cachedHandleForURL:';
    function propertyForKey(propertyKey: NSString): id; message 'propertyForKey:';
    function propertyForKeyIfAvailable(propertyKey: NSString): id; message 'propertyForKeyIfAvailable:';
    function writeProperty_forKey(propertyValue: id; propertyKey: NSString): Boolean; message 'writeProperty:forKey:';
    function writeData(data: NSData): Boolean; message 'writeData:';
  end; external;

{$endif}
{$endif}
{$ifdef PROTOCOLS}
{$ifndef NSURLHANDLE_PAS_P}
{$define NSURLHANDLE_PAS_P}
  
{ NSURLHandleClient Protocol }
  NSURLHandleClientProtocol = objcprotocol
    procedure URLHandle_resourceDataDidBecomeAvailable(sender: NSURLHandle; newBytes: NSData); message 'URLHandle:resourceDataDidBecomeAvailable:';
    procedure URLHandleResourceDidBeginLoading(sender: NSURLHandle); message 'URLHandleResourceDidBeginLoading:';
    procedure URLHandleResourceDidFinishLoading(sender: NSURLHandle); message 'URLHandleResourceDidFinishLoading:';
    procedure URLHandleResourceDidCancelLoading(sender: NSURLHandle); message 'URLHandleResourceDidCancelLoading:';
    procedure URLHandle_resourceDidFailLoadingWithReason(sender: NSURLHandle; reason: NSString); message 'URLHandle:resourceDidFailLoadingWithReason:';
  end; external name 'NSURLHandleClient';
{$endif}
{$endif}
