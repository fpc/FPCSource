{ Parsed from Foundation.framework NSURLRequest.h }


{$ifdef TYPES}
type
  NSURLRequestPtr = ^NSURLRequest;
  NSMutableURLRequestPtr = ^NSMutableURLRequest;
{$endif}

{$ifdef TYPES}
type
  NSURLRequestCachePolicy = NSUInteger;
  NSURLRequestCachePolicyPtr = ^NSURLRequestCachePolicy;

const
  NSURLRequestUseProtocolCachePolicy = 0;
  NSURLRequestReloadIgnoringLocalCacheData = 1;
  NSURLRequestReloadIgnoringLocalAndRemoteCacheData = 4;
  NSURLRequestReloadIgnoringCacheData = NSURLRequestReloadIgnoringLocalCacheData;
  NSURLRequestReturnCacheDataElseLoad = 2;
  NSURLRequestReturnCacheDataDontLoad = 3;
  NSURLRequestReloadRevalidatingCacheData = 5;

type
  NSURLRequestNetworkServiceType = NSUInteger;
  NSURLRequestNetworkServiceTypePtr = ^NSURLRequestNetworkServiceType;

const
  NSURLNetworkServiceTypeDefault = 0;
  NSURLNetworkServiceTypeVoIP = 1;
  NSURLNetworkServiceTypeVideo = 2;
  NSURLNetworkServiceTypeBackground = 3;
  NSURLNetworkServiceTypeVoice = 4;
{$endif}

{$ifdef CLASSES}

type
  NSURLRequest = objcclass external (NSObject, NSSecureCodingProtocol, NSCopyingProtocol, NSMutableCopyingProtocol)
  private
    _internal: NSURLRequestInternal;
  public
    class function requestWithURL (URL: NSURL): instancetype; message 'requestWithURL:';
    class function supportsSecureCoding: ObjCBOOL; message 'supportsSecureCoding';
    class function requestWithURL_cachePolicy_timeoutInterval (URL: NSURL; cachePolicy: NSURLRequestCachePolicy; timeoutInterval: NSTimeInterval): instancetype; message 'requestWithURL:cachePolicy:timeoutInterval:';
    function initWithURL (URL: NSURL): instancetype; message 'initWithURL:';
    function initWithURL_cachePolicy_timeoutInterval (URL: NSURL; cachePolicy: NSURLRequestCachePolicy; timeoutInterval: NSTimeInterval): instancetype; message 'initWithURL:cachePolicy:timeoutInterval:';
    function URL: NSURL; message 'URL';
    function cachePolicy: NSURLRequestCachePolicy; message 'cachePolicy';
    function timeoutInterval: NSTimeInterval; message 'timeoutInterval';
    function mainDocumentURL: NSURL; message 'mainDocumentURL';
    function networkServiceType: NSURLRequestNetworkServiceType; message 'networkServiceType';
    function allowsCellularAccess: ObjCBOOL; message 'allowsCellularAccess';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
    function mutableCopyWithZone (zone: NSZonePtr): id; message 'mutableCopyWithZone:';
  end;

type
  NSMutableURLRequest = objcclass external (NSURLRequest)
  public
    procedure setURL(newValue: NSURL); message 'setURL:';
    function URL: NSURL; message 'URL';
    procedure setCachePolicy(newValue: NSURLRequestCachePolicy); message 'setCachePolicy:';
    function cachePolicy: NSURLRequestCachePolicy; message 'cachePolicy';
    procedure setTimeoutInterval(newValue: NSTimeInterval); message 'setTimeoutInterval:';
    function timeoutInterval: NSTimeInterval; message 'timeoutInterval';
    procedure setMainDocumentURL(newValue: NSURL); message 'setMainDocumentURL:';
    function mainDocumentURL: NSURL; message 'mainDocumentURL';
    procedure setNetworkServiceType(newValue: NSURLRequestNetworkServiceType); message 'setNetworkServiceType:';
    function networkServiceType: NSURLRequestNetworkServiceType; message 'networkServiceType';
    procedure setAllowsCellularAccess(newValue: ObjCBOOL); message 'setAllowsCellularAccess:';
    function allowsCellularAccess: ObjCBOOL; message 'allowsCellularAccess';
  end;


type
  NSHTTPURLRequest = objccategory external (NSURLRequest)
    function HTTPMethod: NSString; message 'HTTPMethod';
    function allHTTPHeaderFields: NSDictionary; message 'allHTTPHeaderFields';
    function valueForHTTPHeaderField (field: NSString): NSString; message 'valueForHTTPHeaderField:';
    function HTTPBody: NSData; message 'HTTPBody';
    function HTTPBodyStream: NSInputStream; message 'HTTPBodyStream';
    function HTTPShouldHandleCookies: ObjCBOOL; message 'HTTPShouldHandleCookies';
    function HTTPShouldUsePipelining: ObjCBOOL; message 'HTTPShouldUsePipelining';
  end;

type
  NSMutableHTTPURLRequest = objccategory external (NSMutableURLRequest)
    procedure setHTTPMethod(newValue: NSString); message 'setHTTPMethod:';
    function HTTPMethod: NSString; message 'HTTPMethod';
    procedure setAllHTTPHeaderFields(newValue: NSDictionary); message 'setAllHTTPHeaderFields:';
    function allHTTPHeaderFields: NSDictionary; message 'allHTTPHeaderFields';
    procedure setValue_forHTTPHeaderField (value: NSString; field: NSString); message 'setValue:forHTTPHeaderField:';
    procedure addValue_forHTTPHeaderField (value: NSString; field: NSString); message 'addValue:forHTTPHeaderField:';
    procedure setHTTPBody(newValue: NSData); message 'setHTTPBody:';
    function HTTPBody: NSData; message 'HTTPBody';
    procedure setHTTPBodyStream(newValue: NSInputStream); message 'setHTTPBodyStream:';
    function HTTPBodyStream: NSInputStream; message 'HTTPBodyStream';
    procedure setHTTPShouldHandleCookies(newValue: ObjCBOOL); message 'setHTTPShouldHandleCookies:';
    function HTTPShouldHandleCookies: ObjCBOOL; message 'HTTPShouldHandleCookies';
    procedure setHTTPShouldUsePipelining(newValue: ObjCBOOL); message 'setHTTPShouldUsePipelining:';
    function HTTPShouldUsePipelining: ObjCBOOL; message 'HTTPShouldUsePipelining';
  end;
{$endif}

