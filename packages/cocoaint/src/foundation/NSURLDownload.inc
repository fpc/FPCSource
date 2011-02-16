{ Parsed from Foundation.framework NSURLDownload.h }

{$ifdef TYPES}
{$ifndef NSURLDOWNLOAD_PAS_T}
{$define NSURLDOWNLOAD_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSURLDOWNLOAD_PAS_R}
{$define NSURLDOWNLOAD_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSURLDOWNLOAD_PAS_F}
{$define NSURLDOWNLOAD_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSURLDOWNLOAD_PAS_S}
{$define NSURLDOWNLOAD_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSURLDownload = objcclass;
  NSURLDownloadPointer = ^NSURLDownload;
  NSURLDownloadPtr = NSURLDownloadPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSURLDOWNLOAD_PAS_C}
{$define NSURLDOWNLOAD_PAS_C}

{ NSURLDownload }
  NSURLDownload = objcclass external (NSObject)
  private
    _internal: NSURLDownloadInternal;
    
  public
    class function canResumeDownloadDecodedWithEncodingMIMEType(MIMEType: NSString): Boolean; message 'canResumeDownloadDecodedWithEncodingMIMEType:';
    function initWithRequest_delegate(request_: NSURLRequest; delegate: id): id; message 'initWithRequest:delegate:';
    function initWithResumeData_delegate_path(resumeData_: NSData; delegate: id; path: NSString): id; message 'initWithResumeData:delegate:path:';
    procedure cancel; message 'cancel';
    procedure setDestination_allowOverwrite(path: NSString; allowOverwrite: Boolean); message 'setDestination:allowOverwrite:';
    function request: NSURLRequest; message 'request';
    function resumeData: NSData; message 'resumeData';
    procedure setDeletesFileUponFailure(deletesFileUponFailure_: Boolean); message 'setDeletesFileUponFailure:';
    function deletesFileUponFailure: Boolean; message 'deletesFileUponFailure';
  end;

{ NSURLDownloadDelegateCategory }
  NSURLDownloadDelegateCategory = objccategory external (NSObject)
    procedure downloadDidBegin(download: NSURLDownload); message 'downloadDidBegin:';
    function download_willSendRequest_redirectResponse(download: NSURLDownload; request: NSURLRequest; redirectResponse: NSURLResponse): NSURLRequest; message 'download:willSendRequest:redirectResponse:';
    function download_canAuthenticateAgainstProtectionSpace(connection: NSURLDownload; protectionSpace: NSURLProtectionSpace): Boolean; message 'download:canAuthenticateAgainstProtectionSpace:';
    procedure download_didReceiveAuthenticationChallenge(download: NSURLDownload; challenge: NSURLAuthenticationChallenge); message 'download:didReceiveAuthenticationChallenge:';
    procedure download_didCancelAuthenticationChallenge(download: NSURLDownload; challenge: NSURLAuthenticationChallenge); message 'download:didCancelAuthenticationChallenge:';
    function downloadShouldUseCredentialStorage(download: NSURLDownload): Boolean; message 'downloadShouldUseCredentialStorage:';
    procedure download_didReceiveResponse(download: NSURLDownload; response: NSURLResponse); message 'download:didReceiveResponse:';
    procedure download_willResumeWithResponse_fromByte(download: NSURLDownload; response: NSURLResponse; startingByte: clonglong); message 'download:willResumeWithResponse:fromByte:';
    procedure download_didReceiveDataOfLength(download: NSURLDownload; length: NSUInteger); message 'download:didReceiveDataOfLength:';
    function download_shouldDecodeSourceDataOfMIMEType(download: NSURLDownload; encodingType: NSString): Boolean; message 'download:shouldDecodeSourceDataOfMIMEType:';
    procedure download_decideDestinationWithSuggestedFilename(download: NSURLDownload; filename: NSString); message 'download:decideDestinationWithSuggestedFilename:';
    procedure download_didCreateDestination(download: NSURLDownload; path: NSString); message 'download:didCreateDestination:';
    procedure downloadDidFinish(download: NSURLDownload); message 'downloadDidFinish:';
    procedure download_didFailWithError(download: NSURLDownload; error: NSError); message 'download:didFailWithError:';
  end;

{$endif}
{$endif}
