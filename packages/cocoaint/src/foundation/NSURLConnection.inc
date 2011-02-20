{ Parsed from Foundation.framework NSURLConnection.h }

{$ifdef TYPES}
{$ifndef NSURLCONNECTION_PAS_T}
{$define NSURLCONNECTION_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSURLCONNECTION_PAS_R}
{$define NSURLCONNECTION_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSURLCONNECTION_PAS_F}
{$define NSURLCONNECTION_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSURLCONNECTION_PAS_S}
{$define NSURLCONNECTION_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSURLConnection = objcclass;
  NSURLConnectionPointer = ^NSURLConnection;
  NSURLConnectionPtr = NSURLConnectionPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSURLCONNECTION_PAS_C}
{$define NSURLCONNECTION_PAS_C}

{ NSURLConnection }
  NSURLConnection = objcclass external (NSObject)
  private
    _internal: NSURLConnectionInternal;
    
  public
    class function canHandleRequest(request: NSURLRequest): Boolean; message 'canHandleRequest:';
    class function connectionWithRequest_delegate(request: NSURLRequest; delegate: id): NSURLConnection; message 'connectionWithRequest:delegate:';
    function initWithRequest_delegate(request: NSURLRequest; delegate: id): id; message 'initWithRequest:delegate:';
    function initWithRequest_delegate_startImmediately(request: NSURLRequest; delegate: id; startImmediately: Boolean): id; message 'initWithRequest:delegate:startImmediately:';
    procedure start; message 'start';
    procedure cancel; message 'cancel';
    procedure scheduleInRunLoop_forMode(aRunLoop: NSRunLoop; mode: NSString); message 'scheduleInRunLoop:forMode:';
    procedure unscheduleFromRunLoop_forMode(aRunLoop: NSRunLoop; mode: NSString); message 'unscheduleFromRunLoop:forMode:';
  end;

{ NSURLConnectionDelegateCategory }
  NSURLConnectionDelegateCategory = objccategory external (NSObject)
    function connection_willSendRequest_redirectResponse(connection: NSURLConnection; request: NSURLRequest; response: NSURLResponse): NSURLRequest; message 'connection:willSendRequest:redirectResponse:';
    function connection_needNewBodyStream(connection: NSURLConnection; request: NSURLRequest): NSInputStream; message 'connection:needNewBodyStream:';
    function connection_canAuthenticateAgainstProtectionSpace(connection: NSURLConnection; protectionSpace: NSURLProtectionSpace): Boolean; message 'connection:canAuthenticateAgainstProtectionSpace:';
    procedure connection_didReceiveAuthenticationChallenge(connection: NSURLConnection; challenge: NSURLAuthenticationChallenge); message 'connection:didReceiveAuthenticationChallenge:';
    procedure connection_didCancelAuthenticationChallenge(connection: NSURLConnection; challenge: NSURLAuthenticationChallenge); message 'connection:didCancelAuthenticationChallenge:';
    function connectionShouldUseCredentialStorage(connection: NSURLConnection): Boolean; message 'connectionShouldUseCredentialStorage:';
    procedure connection_didReceiveResponse(connection: NSURLConnection; response: NSURLResponse); message 'connection:didReceiveResponse:';
    procedure connection_didReceiveData(connection: NSURLConnection; data: NSData); message 'connection:didReceiveData:';
    procedure connection_didSendBodyData_totalBytesWritten_totalBytesExpectedToWrite(connection: NSURLConnection; bytesWritten: NSInteger; totalBytesWritten: NSInteger; totalBytesExpectedToWrite: NSInteger); message 'connection:didSendBodyData:totalBytesWritten:totalBytesExpectedToWrite:';
    procedure connectionDidFinishLoading(connection: NSURLConnection); message 'connectionDidFinishLoading:';
    procedure connection_didFailWithError(connection: NSURLConnection; error: NSError); message 'connection:didFailWithError:';
    function connection_willCacheResponse(connection: NSURLConnection; cachedResponse: NSCachedURLResponse): NSCachedURLResponse; message 'connection:willCacheResponse:';
  end;

{ NSURLConnectionSynchronousLoadingCategory }
  NSURLConnectionSynchronousLoadingCategory = objccategory external (NSURLConnection)
    class function sendSynchronousRequest_returningResponse_error(request: NSURLRequest; response: NSURLResponsePointer; error: NSErrorPointer): NSData; message 'sendSynchronousRequest:returningResponse:error:';
  end;

{$endif}
{$endif}
