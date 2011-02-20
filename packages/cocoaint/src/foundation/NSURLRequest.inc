{ Parsed from Foundation.framework NSURLRequest.h }

{$ifdef TYPES}
{$ifndef NSURLREQUEST_PAS_T}
{$define NSURLREQUEST_PAS_T}

{ Constants }

const
  NSURLRequestUseProtocolCachePolicy = 0;
  NSURLRequestReloadIgnoringLocalCacheData = 1;
  NSURLRequestReloadIgnoringLocalAndRemoteCacheData = 4;
  NSURLRequestReloadIgnoringCacheData = NSURLRequestReloadIgnoringLocalCacheData;
  NSURLRequestReturnCacheDataElseLoad = 2;
  NSURLRequestReturnCacheDataDontLoad = 3;
  NSURLRequestReloadRevalidatingCacheData = 5;

{ Types }
type
  NSURLRequestCachePolicy = NSUInteger;
  NSURLRequestCachePolicyPtr = ^NSURLRequestCachePolicy;

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
  NSURLRequestPtr = NSURLRequestPointer;
  NSMutableURLRequest = objcclass;
  NSMutableURLRequestPointer = ^NSMutableURLRequest;
  NSMutableURLRequestPtr = NSMutableURLRequestPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSURLREQUEST_PAS_C}
{$define NSURLREQUEST_PAS_C}

{ NSURLRequest }
  NSURLRequest = objcclass external (NSObject, NSCodingProtocol, NSCopyingProtocol, NSMutableCopyingProtocol)
  private
    _internal: NSURLRequestInternal;
    
  public
    class function requestWithURL(URL_: NSURL): id; message 'requestWithURL:';
    class function requestWithURL_cachePolicy_timeoutInterval(URL_: NSURL; cachePolicy_: NSURLRequestCachePolicy; timeoutInterval_: NSTimeInterval): id; message 'requestWithURL:cachePolicy:timeoutInterval:';
    function initWithURL(URL_: NSURL): id; message 'initWithURL:';
    function initWithURL_cachePolicy_timeoutInterval(URL_: NSURL; cachePolicy_: NSURLRequestCachePolicy; timeoutInterval_: NSTimeInterval): id; message 'initWithURL:cachePolicy:timeoutInterval:';
    function URL: NSURL; message 'URL';
    function cachePolicy: NSURLRequestCachePolicy; message 'cachePolicy';
    function timeoutInterval: NSTimeInterval; message 'timeoutInterval';
    function mainDocumentURL: NSURL; message 'mainDocumentURL';

    { Adopted Protocols }
    procedure encodeWithCoder(aCoder: NSCoder);
    function initWithCoder(aDecoder: NSCoder): id;
    function copyWithZone(zone_: NSZonePtr): id;
    function mutableCopyWithZone(zone_: NSZonePtr): id;
  end;

{ NSMutableURLRequest }
  NSMutableURLRequest = objcclass external (NSURLRequest)
    
  public
    procedure setURL(URL_: NSURL); message 'setURL:';
    procedure setCachePolicy(policy: NSURLRequestCachePolicy); message 'setCachePolicy:';
    procedure setTimeoutInterval(seconds: NSTimeInterval); message 'setTimeoutInterval:';
    procedure setMainDocumentURL(URL_: NSURL); message 'setMainDocumentURL:';
  end;

{ NSHTTPURLRequestCategory }
  NSHTTPURLRequestCategory = objccategory external (NSURLRequest)
    function HTTPMethod: NSString; message 'HTTPMethod';
    function allHTTPHeaderFields: NSDictionary; message 'allHTTPHeaderFields';
    function valueForHTTPHeaderField(field: NSString): NSString; message 'valueForHTTPHeaderField:';
    function HTTPBody: NSData; message 'HTTPBody';
    function HTTPBodyStream: NSInputStream; message 'HTTPBodyStream';
    function HTTPShouldHandleCookies: Boolean; message 'HTTPShouldHandleCookies';
  end;

{ NSMutableHTTPURLRequestCategory }
  NSMutableHTTPURLRequestCategory = objccategory external (NSMutableURLRequest)
    procedure setHTTPMethod(method: NSString); message 'setHTTPMethod:';
    procedure setAllHTTPHeaderFields(headerFields: NSDictionary); message 'setAllHTTPHeaderFields:';
    procedure setValue_forHTTPHeaderField(value: NSString; field: NSString); message 'setValue:forHTTPHeaderField:';
    procedure addValue_forHTTPHeaderField(value: NSString; field: NSString); message 'addValue:forHTTPHeaderField:';
    procedure setHTTPBody(data: NSData); message 'setHTTPBody:';
    procedure setHTTPBodyStream(inputStream: NSInputStream); message 'setHTTPBodyStream:';
    procedure setHTTPShouldHandleCookies(should: Boolean); message 'setHTTPShouldHandleCookies:';
  end;

{$endif}
{$endif}
