{ Parsed from Foundation.framework NSURLConnection.h }


{$ifdef TYPES}
type
  NSURLConnectionPtr = ^NSURLConnection;
  NSURLConnectionDelegateProtocolPtr = ^NSURLConnectionDelegateProtocol;
  NSURLConnectionDataDelegateProtocolPtr = ^NSURLConnectionDataDelegateProtocol;
  NSURLConnectionDownloadDelegateProtocolPtr = ^NSURLConnectionDownloadDelegateProtocol;
{$endif}

{$ifdef CLASSES}

type
  NSURLConnection = objcclass external (NSObject)
  private
    _internal: NSURLConnectionInternal;
  public
    function initWithRequest_delegate_startImmediately (request: NSURLRequest; delegate: id; startImmediately: ObjCBOOL): instancetype; message 'initWithRequest:delegate:startImmediately:'; { available in 10_5, 2_0 }
    function initWithRequest_delegate (request: NSURLRequest; delegate: id): instancetype; message 'initWithRequest:delegate:';
    class function connectionWithRequest_delegate (request: NSURLRequest; delegate: id): NSURLConnection; message 'connectionWithRequest:delegate:';
    function originalRequest: NSURLRequest; message 'originalRequest';
    function currentRequest: NSURLRequest; message 'currentRequest';
    procedure start; message 'start'; { available in 10_5, 2_0 }
    procedure cancel; message 'cancel';
    procedure scheduleInRunLoop_forMode (aRunLoop: NSRunLoop; mode: NSString); message 'scheduleInRunLoop:forMode:'; { available in 10_5, 2_0 }
    procedure unscheduleFromRunLoop_forMode (aRunLoop: NSRunLoop; mode: NSString); message 'unscheduleFromRunLoop:forMode:'; { available in 10_5, 2_0 }
    procedure setDelegateQueue (queue: NSOperationQueue); message 'setDelegateQueue:'; { available in 10_7, 5_0 }
    class function canHandleRequest (request: NSURLRequest): ObjCBOOL; message 'canHandleRequest:';
  end;
{$endif}

{$ifdef PROTOCOLS}

type
  NSURLConnectionDelegateProtocol = objcprotocol external name 'NSURLConnectionDelegate' (NSObjectProtocol)
  optional
    procedure connection_didFailWithError (connection: NSURLConnection; error: NSError); message 'connection:didFailWithError:';
    function connectionShouldUseCredentialStorage (connection: NSURLConnection): ObjCBOOL; message 'connectionShouldUseCredentialStorage:';
    procedure connection_willSendRequestForAuthenticationChallenge (connection: NSURLConnection; challenge: NSURLAuthenticationChallenge); message 'connection:willSendRequestForAuthenticationChallenge:';
    function connection_canAuthenticateAgainstProtectionSpace (connection: NSURLConnection; protectionSpace: NSURLProtectionSpace): ObjCBOOL; message 'connection:canAuthenticateAgainstProtectionSpace:'; deprecated 'in 10_6, 10_10, 3_0, 8_0, "Use -connection:willSendRequestForAuthenticationChallenge: instead."';
    procedure connection_didReceiveAuthenticationChallenge (connection: NSURLConnection; challenge: NSURLAuthenticationChallenge); message 'connection:didReceiveAuthenticationChallenge:'; deprecated 'in 10_2, 10_10, 2_0, 8_0, "Use -connection:willSendRequestForAuthenticationChallenge: instead."';
    procedure connection_didCancelAuthenticationChallenge (connection: NSURLConnection; challenge: NSURLAuthenticationChallenge); message 'connection:didCancelAuthenticationChallenge:'; deprecated 'in 10_2, 10_10, 2_0, 8_0, "Use -connection:willSendRequestForAuthenticationChallenge: instead."';
  end;

type
  NSURLConnectionDataDelegateProtocol = objcprotocol external name 'NSURLConnectionDataDelegate' (NSURLConnectionDelegateProtocol)
  optional
    function connection_willSendRequest_redirectResponse (connection: NSURLConnection; request: NSURLRequest; response: NSURLResponse): NSURLRequest; message 'connection:willSendRequest:redirectResponse:';
    procedure connection_didReceiveResponse (connection: NSURLConnection; response: NSURLResponse); message 'connection:didReceiveResponse:';
    procedure connection_didReceiveData (connection: NSURLConnection; data: NSData); message 'connection:didReceiveData:';
    function connection_needNewBodyStream (connection: NSURLConnection; request: NSURLRequest): NSInputStream; message 'connection:needNewBodyStream:';
    procedure connection_didSendBodyData_totalBytesWritten_totalBytesExpectedToWrite (connection: NSURLConnection; bytesWritten: NSInteger; totalBytesWritten: NSInteger; totalBytesExpectedToWrite: NSInteger); message 'connection:didSendBodyData:totalBytesWritten:totalBytesExpectedToWrite:';
    function connection_willCacheResponse (connection: NSURLConnection; cachedResponse: NSCachedURLResponse): NSCachedURLResponse; message 'connection:willCacheResponse:';
    procedure connectionDidFinishLoading (connection: NSURLConnection); message 'connectionDidFinishLoading:';
  end;

type
  NSURLConnectionDownloadDelegateProtocol = objcprotocol external name 'NSURLConnectionDownloadDelegate' (NSURLConnectionDelegateProtocol)
  optional
    procedure connection_didWriteData_totalBytesWritten_expectedTotalBytes (connection: NSURLConnection; bytesWritten: clonglong; totalBytesWritten: clonglong; expectedTotalBytes: clonglong); message 'connection:didWriteData:totalBytesWritten:expectedTotalBytes:';
    procedure connectionDidResumeDownloading_totalBytesWritten_expectedTotalBytes (connection: NSURLConnection; totalBytesWritten: clonglong; expectedTotalBytes: clonglong); message 'connectionDidResumeDownloading:totalBytesWritten:expectedTotalBytes:';
  required
    procedure connectionDidFinishDownloading_destinationURL (connection: NSURLConnection; destinationURL: NSURL); message 'connectionDidFinishDownloading:destinationURL:';
  end;
{$endif}

{$ifdef CLASSES}

type
  NSURLConnectionSynchronousLoading = objccategory external (NSURLConnection)
    class function sendSynchronousRequest_returningResponse_error (request: NSURLRequest; response: NSURLResponsePtr; error: NSErrorPtr): NSData; message 'sendSynchronousRequest:returningResponse:error:';
  end;

type
  NSURLConnectionQueuedLoading = objccategory external (NSURLConnection)
    class procedure sendAsynchronousRequest_queue_completionHandler (request: NSURLRequest; queue: NSOperationQueue; handler: OpaqueCBlock); message 'sendAsynchronousRequest:queue:completionHandler:'; { available in 10_7, 5_0 }
  end;
{$endif}

