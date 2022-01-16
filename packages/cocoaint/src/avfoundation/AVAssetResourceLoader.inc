{ Parsed from AVFoundation.framework AVAssetResourceLoader.h }


{$ifdef TYPES}
type
  AVAssetResourceLoaderPtr = ^AVAssetResourceLoader;
  AVAssetResourceLoadingRequestPtr = ^AVAssetResourceLoadingRequest;
  AVAssetResourceRenewalRequestPtr = ^AVAssetResourceRenewalRequest;
  AVAssetResourceLoadingContentInformationRequestPtr = ^AVAssetResourceLoadingContentInformationRequest;
  AVAssetResourceLoadingDataRequestPtr = ^AVAssetResourceLoadingDataRequest;
  AVAssetResourceLoaderDelegateProtocolPtr = ^AVAssetResourceLoaderDelegateProtocol;
{$endif}

{$ifdef CLASSES}

type
  AVAssetResourceLoader = objcclass external (NSObject)
  private
    _resourceLoader: AVAssetResourceLoaderInternal;
  public
    procedure setDelegate_queue (delegate: AVAssetResourceLoaderDelegateProtocol; delegateQueue: dispatch_queue_t); message 'setDelegate:queue:';
    function delegate: AVAssetResourceLoaderDelegateProtocol; message 'delegate';
    function delegateQueue: dispatch_queue_t; message 'delegateQueue';
  end;
{$endif}

{$ifdef PROTOCOLS}

type
  AVAssetResourceLoaderDelegateProtocol = objcprotocol external name 'AVAssetResourceLoaderDelegate' (NSObjectProtocol)
  optional
    function resourceLoader_shouldWaitForLoadingOfRequestedResource (resourceLoader: AVAssetResourceLoader; loadingRequest: AVAssetResourceLoadingRequest): ObjCBOOL; message 'resourceLoader:shouldWaitForLoadingOfRequestedResource:'; { available in 10_9, 6_0 }
    function resourceLoader_shouldWaitForRenewalOfRequestedResource (resourceLoader: AVAssetResourceLoader; renewalRequest: AVAssetResourceRenewalRequest): ObjCBOOL; message 'resourceLoader:shouldWaitForRenewalOfRequestedResource:'; { available in 10_10, 8_0 }
    procedure resourceLoader_didCancelLoadingRequest (resourceLoader: AVAssetResourceLoader; loadingRequest: AVAssetResourceLoadingRequest); message 'resourceLoader:didCancelLoadingRequest:'; { available in 10_9, 7_0 }
    function resourceLoader_shouldWaitForResponseToAuthenticationChallenge (resourceLoader: AVAssetResourceLoader; authenticationChallenge: NSURLAuthenticationChallenge): ObjCBOOL; message 'resourceLoader:shouldWaitForResponseToAuthenticationChallenge:'; { available in 10_10, 8_0 }
    procedure resourceLoader_didCancelAuthenticationChallenge (resourceLoader: AVAssetResourceLoader; authenticationChallenge: NSURLAuthenticationChallenge); message 'resourceLoader:didCancelAuthenticationChallenge:'; { available in 10_10, 8_0 }
  end;
{$endif}

{$ifdef CLASSES}

type
  AVAssetResourceLoadingRequest = objcclass external (NSObject)
  private
    _loadingRequest: AVAssetResourceLoadingRequestInternal;
  public
    function request: NSURLRequest; message 'request';
    function isFinished: ObjCBOOL; message 'isFinished';
    function isCancelled: ObjCBOOL; message 'isCancelled';
    function contentInformationRequest: AVAssetResourceLoadingContentInformationRequest; message 'contentInformationRequest';
    function dataRequest: AVAssetResourceLoadingDataRequest; message 'dataRequest';
    procedure setResponse(newValue: NSURLResponse); message 'setResponse:';
    function response: NSURLResponse; message 'response';
    procedure setRedirect(newValue: NSURLRequest); message 'setRedirect:';
    function redirect: NSURLRequest; message 'redirect';
    procedure finishLoading; message 'finishLoading'; { available in 10_9, 7_0 }
    procedure finishLoadingWithError (error: NSError); message 'finishLoadingWithError:';
  end;

type
  AVAssetResourceRenewalRequest = objcclass external (AVAssetResourceLoadingRequest)
  end;

type
  AVAssetResourceLoadingContentInformationRequest = objcclass external (NSObject)
  private
    _contentInformationRequest: AVAssetResourceLoadingContentInformationRequestInternal;
  public
    procedure setContentType(newValue: NSString); message 'setContentType:';
    function contentType: NSString; message 'contentType';
    procedure setContentLength(newValue: clonglong); message 'setContentLength:';
    function contentLength: clonglong; message 'contentLength';
    procedure setByteRangeAccessSupported(newValue: ObjCBOOL); message 'setByteRangeAccessSupported:';
    function isByteRangeAccessSupported: ObjCBOOL; message 'isByteRangeAccessSupported';
    procedure setRenewalDate(newValue: NSDate); message 'setRenewalDate:';
    function renewalDate: NSDate; message 'renewalDate';
  end;

type
  AVAssetResourceLoadingDataRequest = objcclass external (NSObject)
  private
    _dataRequest: AVAssetResourceLoadingDataRequestInternal;
  public
    function requestedOffset: clonglong; message 'requestedOffset';
    function requestedLength: NSInteger; message 'requestedLength';
    function currentOffset: clonglong; message 'currentOffset';
    procedure respondWithData (data: NSData); message 'respondWithData:';
  end;


type
  AVAssetResourceLoader_ContentKeyRequestSupport = objccategory external (AVAssetResourceLoadingRequest)
    function streamingContentKeyRequestDataForApp_contentIdentifier_options_error (appIdentifier: NSData; contentIdentifier: NSData; options: NSDictionary; outError: NSErrorPtr): NSData; message 'streamingContentKeyRequestDataForApp:contentIdentifier:options:error:';
  end;

type
  AVAssetResourceLoadingRequestDeprecated = objccategory external (AVAssetResourceLoadingRequest)
    procedure finishLoadingWithResponse_data_redirect (response_: NSURLResponse; data: NSData; redirect_: NSURLRequest); message 'finishLoadingWithResponse:data:redirect:'; deprecated 'in 6_0, 7_0';
  end;
{$endif}

