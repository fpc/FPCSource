{ Parsed from Foundation.framework NSURLDownload.h }


{$ifdef TYPES}
type
  NSURLDownloadPtr = ^NSURLDownload;
  NSURLDownloadDelegateProtocolPtr = ^NSURLDownloadDelegateProtocol;
{$endif}

{$ifdef CLASSES}

type
  NSURLDownload = objcclass external (NSObject)
  private
    _internal: NSURLDownloadInternal;
  public
    class function canResumeDownloadDecodedWithEncodingMIMEType (MIMEType: NSString): ObjCBOOL; message 'canResumeDownloadDecodedWithEncodingMIMEType:';
    function initWithRequest_delegate (request: NSURLRequest; delegate: NSURLDownloadDelegateProtocol): instancetype; message 'initWithRequest:delegate:';
    function initWithResumeData_delegate_path (resumeData: NSData; delegate: NSURLDownloadDelegateProtocol; path: NSString): instancetype; message 'initWithResumeData:delegate:path:';
    procedure cancel; message 'cancel';
    procedure setDestination_allowOverwrite (path: NSString; allowOverwrite: ObjCBOOL); message 'setDestination:allowOverwrite:';
    function request: NSURLRequest; message 'request';
    function resumeData: NSData; message 'resumeData';
    procedure setDeletesFileUponFailure(newValue: ObjCBOOL); message 'setDeletesFileUponFailure:';
    function deletesFileUponFailure: ObjCBOOL; message 'deletesFileUponFailure';
  end;
{$endif}

{$ifdef PROTOCOLS}

type
  NSURLDownloadDelegateProtocol = objcprotocol external name 'NSURLDownloadDelegate' (NSObjectProtocol)
  optional
    procedure downloadDidBegin (download: NSURLDownload); message 'downloadDidBegin:';
    function download_willSendRequest_redirectResponse (download: NSURLDownload; request: NSURLRequest; redirectResponse: NSURLResponse): NSURLRequest; message 'download:willSendRequest:redirectResponse:';
    function download_canAuthenticateAgainstProtectionSpace (connection: NSURLDownload; protectionSpace: NSURLProtectionSpace): ObjCBOOL; message 'download:canAuthenticateAgainstProtectionSpace:';
    procedure download_didReceiveAuthenticationChallenge (download: NSURLDownload; challenge: NSURLAuthenticationChallenge); message 'download:didReceiveAuthenticationChallenge:';
    procedure download_didCancelAuthenticationChallenge (download: NSURLDownload; challenge: NSURLAuthenticationChallenge); message 'download:didCancelAuthenticationChallenge:';
    function downloadShouldUseCredentialStorage (download: NSURLDownload): ObjCBOOL; message 'downloadShouldUseCredentialStorage:';
    procedure download_didReceiveResponse (download: NSURLDownload; response: NSURLResponse); message 'download:didReceiveResponse:';
    procedure download_willResumeWithResponse_fromByte (download: NSURLDownload; response: NSURLResponse; startingByte: clonglong); message 'download:willResumeWithResponse:fromByte:';
    procedure download_didReceiveDataOfLength (download: NSURLDownload; length: NSUInteger); message 'download:didReceiveDataOfLength:';
    function download_shouldDecodeSourceDataOfMIMEType (download: NSURLDownload; encodingType: NSString): ObjCBOOL; message 'download:shouldDecodeSourceDataOfMIMEType:';
    procedure download_decideDestinationWithSuggestedFilename (download: NSURLDownload; filename: NSString); message 'download:decideDestinationWithSuggestedFilename:';
    procedure download_didCreateDestination (download: NSURLDownload; path: NSString); message 'download:didCreateDestination:';
    procedure downloadDidFinish (download: NSURLDownload); message 'downloadDidFinish:';
    procedure download_didFailWithError (download: NSURLDownload; error: NSError); message 'download:didFailWithError:';
  end;
{$endif}

