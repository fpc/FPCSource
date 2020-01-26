{ Parsed from Foundation.framework NSURLSession.h }


{$ifdef TYPES}
type
  NSURLSessionPtr = ^NSURLSession;
  NSURLSessionTaskPtr = ^NSURLSessionTask;
  NSURLSessionDataTaskPtr = ^NSURLSessionDataTask;
  NSURLSessionUploadTaskPtr = ^NSURLSessionUploadTask;
  NSURLSessionDownloadTaskPtr = ^NSURLSessionDownloadTask;
  NSURLSessionConfigurationPtr = ^NSURLSessionConfiguration;
  NSURLSessionDelegateProtocolPtr = ^NSURLSessionDelegateProtocol;
  NSURLSessionTaskDelegateProtocolPtr = ^NSURLSessionTaskDelegateProtocol;
  NSURLSessionDataDelegateProtocolPtr = ^NSURLSessionDataDelegateProtocol;
  NSURLSessionDownloadDelegateProtocolPtr = ^NSURLSessionDownloadDelegateProtocol;
{$endif}

{$if defined(__OBJC2__)}
{$ifdef TYPES}
//const
//  NSURLSESSION_AVAILABLE = 10_9;
{$endif}

{$else}
{$ifdef TYPES}
//const
//  NSURLSESSION_AVAILABLE = 10_10;
{$endif}

{$endif}
{$ifdef EXTERNAL_SYMBOLS}
var
  NSURLSessionTransferSizeUnknown: cint64 { available in NSURLSESSION_AVAILABLE, 7_0 }; cvar; external;
{$endif}

{$ifdef CLASSES}

type
  NSURLSession = objcclass external (NSObject)
  public
    class function sharedSession: NSURLSession; message 'sharedSession';
    class function sessionWithConfiguration (configuration: NSURLSessionConfiguration): NSURLSession; message 'sessionWithConfiguration:';
    class function sessionWithConfiguration_delegate_delegateQueue (configuration: NSURLSessionConfiguration; delegate: NSURLSessionDelegateProtocol; queue: NSOperationQueue): NSURLSession; message 'sessionWithConfiguration:delegate:delegateQueue:';
    function delegateQueue: NSOperationQueue; message 'delegateQueue';
    function delegate: NSURLSessionDelegateProtocol; message 'delegate';
    function configuration: NSURLSessionConfiguration; message 'configuration';
    procedure setSessionDescription(newValue: NSString); message 'setSessionDescription:';
    function sessionDescription: NSString; message 'sessionDescription';
    procedure finishTasksAndInvalidate; message 'finishTasksAndInvalidate';
    procedure invalidateAndCancel; message 'invalidateAndCancel';
    procedure resetWithCompletionHandler (completionHandler: OpaqueCBlock); message 'resetWithCompletionHandler:';
    procedure flushWithCompletionHandler (completionHandler: OpaqueCBlock); message 'flushWithCompletionHandler:';
    procedure getTasksWithCompletionHandler (completionHandler: OpaqueCBlock); message 'getTasksWithCompletionHandler:';
    function dataTaskWithRequest (request: NSURLRequest): NSURLSessionDataTask; message 'dataTaskWithRequest:';
    function dataTaskWithURL (url: NSURL): NSURLSessionDataTask; message 'dataTaskWithURL:';
    function uploadTaskWithRequest_fromFile (request: NSURLRequest; fileURL: NSURL): NSURLSessionUploadTask; message 'uploadTaskWithRequest:fromFile:';
    function uploadTaskWithRequest_fromData (request: NSURLRequest; bodyData: NSData): NSURLSessionUploadTask; message 'uploadTaskWithRequest:fromData:';
    function uploadTaskWithStreamedRequest (request: NSURLRequest): NSURLSessionUploadTask; message 'uploadTaskWithStreamedRequest:';
    function downloadTaskWithRequest (request: NSURLRequest): NSURLSessionDownloadTask; message 'downloadTaskWithRequest:';
    function downloadTaskWithURL (url: NSURL): NSURLSessionDownloadTask; message 'downloadTaskWithURL:';
    function downloadTaskWithResumeData (resumeData: NSData): NSURLSessionDownloadTask; message 'downloadTaskWithResumeData:';
  end;


type
  NSURLSessionAsynchronousConvenience = objccategory external (NSURLSession)
    function dataTaskWithRequest_completionHandler (request: NSURLRequest; completionHandler: OpaqueCBlock): NSURLSessionDataTask; message 'dataTaskWithRequest:completionHandler:';
    function dataTaskWithURL_completionHandler (url: NSURL; completionHandler: OpaqueCBlock): NSURLSessionDataTask; message 'dataTaskWithURL:completionHandler:';
    function uploadTaskWithRequest_fromFile_completionHandler (request: NSURLRequest; fileURL: NSURL; completionHandler: OpaqueCBlock): NSURLSessionUploadTask; message 'uploadTaskWithRequest:fromFile:completionHandler:';
    function uploadTaskWithRequest_fromData_completionHandler (request: NSURLRequest; bodyData: NSData; completionHandler: OpaqueCBlock): NSURLSessionUploadTask; message 'uploadTaskWithRequest:fromData:completionHandler:';
    function downloadTaskWithRequest_completionHandler (request: NSURLRequest; completionHandler: OpaqueCBlock): NSURLSessionDownloadTask; message 'downloadTaskWithRequest:completionHandler:';
    function downloadTaskWithURL_completionHandler (url: NSURL; completionHandler: OpaqueCBlock): NSURLSessionDownloadTask; message 'downloadTaskWithURL:completionHandler:';
    function downloadTaskWithResumeData_completionHandler (resumeData: NSData; completionHandler: OpaqueCBlock): NSURLSessionDownloadTask; message 'downloadTaskWithResumeData:completionHandler:';
  end;
{$endif}

{$ifdef TYPES}
type
  NSURLSessionTaskState = NSInteger;
  NSURLSessionTaskStatePtr = ^NSURLSessionTaskState;

const
  NSURLSessionTaskStateRunning = 0;
  NSURLSessionTaskStateSuspended = 1;
  NSURLSessionTaskStateCanceling = 2;
  NSURLSessionTaskStateCompleted = 3;
{$endif}

{$ifdef CLASSES}

type
  NSURLSessionTask = objcclass external (NSObject, NSCopyingProtocol)
  public
    function taskIdentifier: NSUInteger; message 'taskIdentifier';
    function originalRequest: NSURLRequest; message 'originalRequest';
    function currentRequest: NSURLRequest; message 'currentRequest';
    function response: NSURLResponse; message 'response';
    function countOfBytesReceived: cint64; message 'countOfBytesReceived';
    function countOfBytesSent: cint64; message 'countOfBytesSent';
    function countOfBytesExpectedToSend: cint64; message 'countOfBytesExpectedToSend';
    function countOfBytesExpectedToReceive: cint64; message 'countOfBytesExpectedToReceive';
    procedure setTaskDescription(newValue: NSString); message 'setTaskDescription:';
    function taskDescription: NSString; message 'taskDescription';
    procedure cancel; message 'cancel';
    function state: NSURLSessionTaskState; message 'state';
    function error: NSError; message 'error';
    procedure suspend; message 'suspend';
    procedure resume; message 'resume';
    procedure setPriority(newValue: single); message 'setPriority:';
    function priority: single; message 'priority';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSURLSessionTaskPriorityDefault: single { available in 10_10, 8_0 }; cvar; external;
  NSURLSessionTaskPriorityLow: single { available in 10_10, 8_0 }; cvar; external;
  NSURLSessionTaskPriorityHigh: single { available in 10_10, 8_0 }; cvar; external;
{$endif}

{$ifdef CLASSES}

type
  NSURLSessionDataTask = objcclass external (NSURLSessionTask)
  end;

type
  NSURLSessionUploadTask = objcclass external (NSURLSessionDataTask)
  end;

type
  NSURLSessionDownloadTask = objcclass external (NSURLSessionTask)
  public
    procedure cancelByProducingResumeData (completionHandler: OpaqueCBlock); message 'cancelByProducingResumeData:';
  end;

type
  NSURLSessionConfiguration = objcclass external (NSObject, NSCopyingProtocol)
  public
    class function defaultSessionConfiguration: NSURLSessionConfiguration; message 'defaultSessionConfiguration';
    class function ephemeralSessionConfiguration: NSURLSessionConfiguration; message 'ephemeralSessionConfiguration';
    class function backgroundSessionConfigurationWithIdentifier (identifier: NSString): NSURLSessionConfiguration; message 'backgroundSessionConfigurationWithIdentifier:'; { available in 10_10, 8_0 }
    function identifier: NSString; message 'identifier';
    procedure setRequestCachePolicy(newValue: NSURLRequestCachePolicy); message 'setRequestCachePolicy:';
    function requestCachePolicy: NSURLRequestCachePolicy; message 'requestCachePolicy';
    procedure setTimeoutIntervalForRequest(newValue: NSTimeInterval); message 'setTimeoutIntervalForRequest:';
    function timeoutIntervalForRequest: NSTimeInterval; message 'timeoutIntervalForRequest';
    procedure setTimeoutIntervalForResource(newValue: NSTimeInterval); message 'setTimeoutIntervalForResource:';
    function timeoutIntervalForResource: NSTimeInterval; message 'timeoutIntervalForResource';
    procedure setNetworkServiceType(newValue: NSURLRequestNetworkServiceType); message 'setNetworkServiceType:';
    function networkServiceType: NSURLRequestNetworkServiceType; message 'networkServiceType';
    procedure setAllowsCellularAccess(newValue: ObjCBOOL); message 'setAllowsCellularAccess:';
    function allowsCellularAccess: ObjCBOOL; message 'allowsCellularAccess';
    procedure setDiscretionary(newValue: ObjCBOOL); message 'setDiscretionary:';
    function isDiscretionary: ObjCBOOL; message 'isDiscretionary';
    procedure setSharedContainerIdentifier(newValue: NSString); message 'setSharedContainerIdentifier:';
    function sharedContainerIdentifier: NSString; message 'sharedContainerIdentifier';
    procedure setSessionSendsLaunchEvents(newValue: ObjCBOOL); message 'setSessionSendsLaunchEvents:';
    function sessionSendsLaunchEvents: ObjCBOOL; message 'sessionSendsLaunchEvents';
    procedure setConnectionProxyDictionary(newValue: NSDictionary); message 'setConnectionProxyDictionary:';
    function connectionProxyDictionary: NSDictionary; message 'connectionProxyDictionary';
    procedure setTLSMinimumSupportedProtocol(newValue: SSLProtocol); message 'setTLSMinimumSupportedProtocol:';
    function TLSMinimumSupportedProtocol: SSLProtocol; message 'TLSMinimumSupportedProtocol';
    procedure setTLSMaximumSupportedProtocol(newValue: SSLProtocol); message 'setTLSMaximumSupportedProtocol:';
    function TLSMaximumSupportedProtocol: SSLProtocol; message 'TLSMaximumSupportedProtocol';
    procedure setHTTPShouldUsePipelining(newValue: ObjCBOOL); message 'setHTTPShouldUsePipelining:';
    function HTTPShouldUsePipelining: ObjCBOOL; message 'HTTPShouldUsePipelining';
    procedure setHTTPShouldSetCookies(newValue: ObjCBOOL); message 'setHTTPShouldSetCookies:';
    function HTTPShouldSetCookies: ObjCBOOL; message 'HTTPShouldSetCookies';
    procedure setHTTPCookieAcceptPolicy(newValue: NSHTTPCookieAcceptPolicy); message 'setHTTPCookieAcceptPolicy:';
    function HTTPCookieAcceptPolicy: NSHTTPCookieAcceptPolicy; message 'HTTPCookieAcceptPolicy';
    procedure setHTTPAdditionalHeaders(newValue: NSDictionary); message 'setHTTPAdditionalHeaders:';
    function HTTPAdditionalHeaders: NSDictionary; message 'HTTPAdditionalHeaders';
    procedure setHTTPMaximumConnectionsPerHost(newValue: NSInteger); message 'setHTTPMaximumConnectionsPerHost:';
    function HTTPMaximumConnectionsPerHost: NSInteger; message 'HTTPMaximumConnectionsPerHost';
    procedure setHTTPCookieStorage(newValue: NSHTTPCookieStorage); message 'setHTTPCookieStorage:';
    function HTTPCookieStorage: NSHTTPCookieStorage; message 'HTTPCookieStorage';
    procedure setURLCredentialStorage(newValue: NSURLCredentialStorage); message 'setURLCredentialStorage:';
    function URLCredentialStorage: NSURLCredentialStorage; message 'URLCredentialStorage';
    procedure setURLCache(newValue: NSURLCache); message 'setURLCache:';
    function URLCache: NSURLCache; message 'URLCache';
    procedure setProtocolClasses(newValue: NSArray); message 'setProtocolClasses:';
    function protocolClasses: NSArray; message 'protocolClasses';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
  end;
{$endif}

{$ifdef TYPES}
type
  NSURLSessionAuthChallengeDisposition = NSInteger;
  NSURLSessionAuthChallengeDispositionPtr = ^NSURLSessionAuthChallengeDisposition;

const
  NSURLSessionAuthChallengeUseCredential = 0;
  NSURLSessionAuthChallengePerformDefaultHandling = 1;
  NSURLSessionAuthChallengeCancelAuthenticationChallenge = 2;
  NSURLSessionAuthChallengeRejectProtectionSpace = 3;

type
  NSURLSessionResponseDisposition = NSInteger;
  NSURLSessionResponseDispositionPtr = ^NSURLSessionResponseDisposition;

const
  NSURLSessionResponseCancel = 0;
  NSURLSessionResponseAllow = 1;
  NSURLSessionResponseBecomeDownload = 2;
{$endif}

{$ifdef PROTOCOLS}

type
  NSURLSessionDelegateProtocol = objcprotocol external name 'NSURLSessionDelegate' (NSObjectProtocol)
  optional
    procedure URLSession_didBecomeInvalidWithError (session: NSURLSession; error: NSError); message 'URLSession:didBecomeInvalidWithError:';
    procedure URLSession_didReceiveChallenge_completionHandler (session: NSURLSession; challenge: NSURLAuthenticationChallenge; completionHandler: OpaqueCBlock); message 'URLSession:didReceiveChallenge:completionHandler:';
    procedure URLSessionDidFinishEventsForBackgroundURLSession (session: NSURLSession); message 'URLSessionDidFinishEventsForBackgroundURLSession:'; { available in 7_0 }
  end;

type
  NSURLSessionTaskDelegateProtocol = objcprotocol external name 'NSURLSessionTaskDelegate' (NSURLSessionDelegateProtocol)
  optional
    procedure URLSession_task_willPerformHTTPRedirection_newRequest_completionHandler (session: NSURLSession; task: NSURLSessionTask; response: NSHTTPURLResponse; request: NSURLRequest; completionHandler: OpaqueCBlock); message 'URLSession:task:willPerformHTTPRedirection:newRequest:completionHandler:';
    procedure URLSession_task_didReceiveChallenge_completionHandler (session: NSURLSession; task: NSURLSessionTask; challenge: NSURLAuthenticationChallenge; completionHandler: OpaqueCBlock); message 'URLSession:task:didReceiveChallenge:completionHandler:';
    procedure URLSession_task_needNewBodyStream (session: NSURLSession; task: NSURLSessionTask; completionHandler: OpaqueCBlock); message 'URLSession:task:needNewBodyStream:';
    procedure URLSession_task_didSendBodyData_totalBytesSent_totalBytesExpectedToSend (session: NSURLSession; task: NSURLSessionTask; bytesSent: cint64; totalBytesSent: cint64; totalBytesExpectedToSend: cint64); message 'URLSession:task:didSendBodyData:totalBytesSent:totalBytesExpectedToSend:';
    procedure URLSession_task_didCompleteWithError (session: NSURLSession; task: NSURLSessionTask; error: NSError); message 'URLSession:task:didCompleteWithError:';
  end;

type
  NSURLSessionDataDelegateProtocol = objcprotocol external name 'NSURLSessionDataDelegate' (NSURLSessionTaskDelegateProtocol)
  optional
    procedure URLSession_dataTask_didReceiveResponse_completionHandler (session: NSURLSession; dataTask: NSURLSessionDataTask; response: NSURLResponse; completionHandler: OpaqueCBlock); message 'URLSession:dataTask:didReceiveResponse:completionHandler:';
    procedure URLSession_dataTask_didBecomeDownloadTask (session: NSURLSession; dataTask: NSURLSessionDataTask; downloadTask: NSURLSessionDownloadTask); message 'URLSession:dataTask:didBecomeDownloadTask:';
    procedure URLSession_dataTask_didReceiveData (session: NSURLSession; dataTask: NSURLSessionDataTask; data: NSData); message 'URLSession:dataTask:didReceiveData:';
    procedure URLSession_dataTask_willCacheResponse_completionHandler (session: NSURLSession; dataTask: NSURLSessionDataTask; proposedResponse: NSCachedURLResponse; completionHandler: OpaqueCBlock); message 'URLSession:dataTask:willCacheResponse:completionHandler:';
  end;

type
  NSURLSessionDownloadDelegateProtocol = objcprotocol external name 'NSURLSessionDownloadDelegate' (NSURLSessionTaskDelegateProtocol)
    procedure URLSession_downloadTask_didFinishDownloadingToURL (session: NSURLSession; downloadTask: NSURLSessionDownloadTask; location: NSURL); message 'URLSession:downloadTask:didFinishDownloadingToURL:';
  optional
    procedure URLSession_downloadTask_didWriteData_totalBytesWritten_totalBytesExpectedToWrite (session: NSURLSession; downloadTask: NSURLSessionDownloadTask; bytesWritten: cint64; totalBytesWritten: cint64; totalBytesExpectedToWrite: cint64); message 'URLSession:downloadTask:didWriteData:totalBytesWritten:totalBytesExpectedToWrite:';
    procedure URLSession_downloadTask_didResumeAtOffset_expectedTotalBytes (session: NSURLSession; downloadTask: NSURLSessionDownloadTask; fileOffset: cint64; expectedTotalBytes: cint64); message 'URLSession:downloadTask:didResumeAtOffset:expectedTotalBytes:';
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSURLSessionDownloadTaskResumeData: NSString { available in NSURLSESSION_AVAILABLE, 7_0 }; cvar; external;
{$endif}

{$ifdef CLASSES}

type
  NSURLSession_NSURLSessionDeprecated = objccategory external name 'NSURLSessionDeprecated' (NSURLSession)
    function dataTaskWithHTTPGetRequest (url: NSURL): NSURLSessionDataTask; message 'dataTaskWithHTTPGetRequest:'; deprecated 'in NSURLSESSION_AVAILABLE, NSURLSESSION_AVAILABLE, 7_0, 7_0';
    function dataTaskWithHTTPGetRequest_completionHandler (url: NSURL; completionHandler: OpaqueCBlock): NSURLSessionDataTask; message 'dataTaskWithHTTPGetRequest:completionHandler:'; deprecated 'in NSURLSESSION_AVAILABLE, NSURLSESSION_AVAILABLE, 7_0, 7_0';
  end;

type
  NSURLSessionDeprecated = objccategory external (NSURLSessionConfiguration)
    class function backgroundSessionConfiguration (identifier_: NSString): NSURLSessionConfiguration; message 'backgroundSessionConfiguration:'; deprecated 'in NSURLSESSION_AVAILABLE, 10_10, 7_0, 8_0, "Please use backgroundSessionConfigurationWithIdentifier: instead"';
  end;
{$endif}

