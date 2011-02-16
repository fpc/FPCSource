{ Parsed from Foundation.framework NSURLProtocol.h }

{$ifdef TYPES}
{$ifndef NSURLPROTOCOL_PAS_T}
{$define NSURLPROTOCOL_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSURLPROTOCOL_PAS_R}
{$define NSURLPROTOCOL_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSURLPROTOCOL_PAS_F}
{$define NSURLPROTOCOL_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSURLPROTOCOL_PAS_S}
{$define NSURLPROTOCOL_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSURLProtocolClientProtocol = objcprotocol;
  NSURLProtocol = objcclass;
  NSURLProtocolPointer = ^NSURLProtocol;
  NSURLProtocolPtr = NSURLProtocolPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSURLPROTOCOL_PAS_C}
{$define NSURLPROTOCOL_PAS_C}

{ NSURLProtocol }
  NSURLProtocol = objcclass external (NSObject)
  private
    _internal: NSURLProtocolInternal;
    
  public
    function initWithRequest_cachedResponse_client(request_: NSURLRequest; cachedResponse_: NSCachedURLResponse; client_: NSURLProtocolClientProtocol): id; message 'initWithRequest:cachedResponse:client:';
    function client: NSURLProtocolClientProtocol; message 'client';
    function request: NSURLRequest; message 'request';
    function cachedResponse: NSCachedURLResponse; message 'cachedResponse';
    class function canInitWithRequest(request_: NSURLRequest): Boolean; message 'canInitWithRequest:';
    class function canonicalRequestForRequest(request_: NSURLRequest): NSURLRequest; message 'canonicalRequestForRequest:';
    class function requestIsCacheEquivalent_toRequest(a: NSURLRequest; b: NSURLRequest): Boolean; message 'requestIsCacheEquivalent:toRequest:';
    procedure startLoading; message 'startLoading';
    procedure stopLoading; message 'stopLoading';
    class function propertyForKey_inRequest(key: NSString; request_: NSURLRequest): id; message 'propertyForKey:inRequest:';
    class procedure setProperty_forKey_inRequest(value: id; key: NSString; request_: NSMutableURLRequest); message 'setProperty:forKey:inRequest:';
    class procedure removePropertyForKey_inRequest(key: NSString; request_: NSMutableURLRequest); message 'removePropertyForKey:inRequest:';
    class function registerClass(protocolClass: Pobjc_class): Boolean; message 'registerClass:';
    class procedure unregisterClass(protocolClass: Pobjc_class); message 'unregisterClass:';
  end;

{$endif}
{$endif}
{$ifdef PROTOCOLS}
{$ifndef NSURLPROTOCOL_PAS_P}
{$define NSURLPROTOCOL_PAS_P}
  
{ NSURLProtocolClient Protocol }
  NSURLProtocolClientProtocol = objcprotocol external name 'NSURLProtocolClient'
    procedure URLProtocol_wasRedirectedToRequest_redirectResponse(protocol: NSURLProtocol; request: NSURLRequest; redirectResponse: NSURLResponse); message 'URLProtocol:wasRedirectedToRequest:redirectResponse:';
    procedure URLProtocol_cachedResponseIsValid(protocol: NSURLProtocol; cachedResponse: NSCachedURLResponse); message 'URLProtocol:cachedResponseIsValid:';
    procedure URLProtocol_didReceiveResponse_cacheStoragePolicy(protocol: NSURLProtocol; response: NSURLResponse; policy: NSURLCacheStoragePolicy); message 'URLProtocol:didReceiveResponse:cacheStoragePolicy:';
    procedure URLProtocol_didLoadData(protocol: NSURLProtocol; data: NSData); message 'URLProtocol:didLoadData:';
    procedure URLProtocolDidFinishLoading(protocol: NSURLProtocol); message 'URLProtocolDidFinishLoading:';
    procedure URLProtocol_didFailWithError(protocol: NSURLProtocol; error: NSError); message 'URLProtocol:didFailWithError:';
    procedure URLProtocol_didReceiveAuthenticationChallenge(protocol: NSURLProtocol; challenge: NSURLAuthenticationChallenge); message 'URLProtocol:didReceiveAuthenticationChallenge:';
    procedure URLProtocol_didCancelAuthenticationChallenge(protocol: NSURLProtocol; challenge: NSURLAuthenticationChallenge); message 'URLProtocol:didCancelAuthenticationChallenge:';
  end;
{$endif}
{$endif}
