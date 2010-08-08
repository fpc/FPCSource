{ Parsed from Foundation.framework NSURLRequest.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Thu Oct 1 18:54:07 ICT 2009 }


{$ifdef TYPES}
{$ifndef NSURLREQUEST_PAS_T}
{$define NSURLREQUEST_PAS_T}

{ Types }
type
  NSURLRequestCachePolicy = NSUInteger;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSURLREQUEST_PAS_R}
{$define NSURLREQUEST_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSURLREQUEST_PAS_F}
{$define NSURLREQUEST_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSURLREQUEST_PAS_S}
{$define NSURLREQUEST_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSURLRequest = objcclass;
  NSURLRequestPointer = ^NSURLRequest;
  NSMutableURLRequest = objcclass;
  NSMutableURLRequestPointer = ^NSMutableURLRequest;

{$endif}

{$ifdef CLASSES}
{$ifndef NSURLREQUEST_PAS_C}
{$define NSURLREQUEST_PAS_C}

{ NSURLRequest }
  NSURLRequest = objcclass(NSObject, NSCodingProtocol, NSCopyingProtocol, NSMutableCopyingProtocol)
  private
    __internal: NSURLRequestInternal;
    
  public
    class function alloc: NSURLRequest; message 'alloc';

    class function requestWithURL(URL_: NSURL): id; message 'requestWithURL:';
    class function requestWithURL_cachePolicy_timeoutInterval(URL_: NSURL; cachePolicy_: NSURLRequestCachePolicy; timeoutInterval_: NSTimeInterval): id; message 'requestWithURL:cachePolicy:timeoutInterval:';
    function initWithURL(URL_: NSURL): id; message 'initWithURL:';
    function initWithURL_cachePolicy_timeoutInterval(URL_: NSURL; cachePolicy_: NSURLRequestCachePolicy; timeoutInterval_: NSTimeInterval): id; message 'initWithURL:cachePolicy:timeoutInterval:';
    function URL: NSURL; message 'URL';
    function cachePolicy: NSURLRequestCachePolicy; message 'cachePolicy';
    function timeoutInterval: NSTimeInterval; message 'timeoutInterval';
    function mainDocumentURL: NSURL; message 'mainDocumentURL';

    { Category: NSHTTPURLRequest }
    function HTTPMethod: NSString; message 'HTTPMethod';
    function allHTTPHeaderFields: NSDictionary; message 'allHTTPHeaderFields';
    function valueForHTTPHeaderField(field: NSString): NSString; message 'valueForHTTPHeaderField:';
    function HTTPBody: NSData; message 'HTTPBody';
    function HTTPBodyStream: NSInputStream; message 'HTTPBodyStream';
    function HTTPShouldHandleCookies: Boolean; message 'HTTPShouldHandleCookies';
  end; external;

{ NSMutableURLRequest }
  NSMutableURLRequest = objcclass(NSURLRequest)
    
  public
    class function alloc: NSMutableURLRequest; message 'alloc';

    procedure setURL(URL_: NSURL); message 'setURL:';
    procedure setCachePolicy(policy: NSURLRequestCachePolicy); message 'setCachePolicy:';
    procedure setTimeoutInterval(seconds: NSTimeInterval); message 'setTimeoutInterval:';
    procedure setMainDocumentURL(URL_: NSURL); message 'setMainDocumentURL:';

    { Category: NSMutableHTTPURLRequest }
    procedure setHTTPMethod(method: NSString); message 'setHTTPMethod:';
    procedure setAllHTTPHeaderFields(headerFields: NSDictionary); message 'setAllHTTPHeaderFields:';
    procedure setValue_forHTTPHeaderField(value: NSString; field: NSString); message 'setValue:forHTTPHeaderField:';
    procedure addValue_forHTTPHeaderField(value: NSString; field: NSString); message 'addValue:forHTTPHeaderField:';
    procedure setHTTPBody(data: NSData); message 'setHTTPBody:';
    procedure setHTTPBodyStream(inputStream: NSInputStream); message 'setHTTPBodyStream:';
    procedure setHTTPShouldHandleCookies(should: Boolean); message 'setHTTPShouldHandleCookies:';
  end; external;

{$endif}
{$endif}
