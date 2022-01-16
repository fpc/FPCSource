{ Parsed from Foundation.framework NSHTTPCookieStorage.h }


{$ifdef TYPES}
type
  NSHTTPCookieStoragePtr = ^NSHTTPCookieStorage;
{$endif}

{$ifdef TYPES}
type
  NSHTTPCookieAcceptPolicy = NSUInteger;
  NSHTTPCookieAcceptPolicyPtr = ^NSHTTPCookieAcceptPolicy;

const
  NSHTTPCookieAcceptPolicyAlways = 0;
  NSHTTPCookieAcceptPolicyNever = 1;
  NSHTTPCookieAcceptPolicyOnlyFromMainDocumentDomain = 2;
{$endif}

{$ifdef CLASSES}

type
  NSHTTPCookieStorage = objcclass external (NSObject)
  private
    _internal: NSHTTPCookieStorageInternal;
  public
    class function sharedHTTPCookieStorage: NSHTTPCookieStorage; message 'sharedHTTPCookieStorage';
    function cookies: NSArray; message 'cookies';
    procedure setCookie (cookie: NSHTTPCookie); message 'setCookie:';
    procedure deleteCookie (cookie: NSHTTPCookie); message 'deleteCookie:';
    procedure removeCookiesSinceDate (date: NSDate); message 'removeCookiesSinceDate:'; { available in 10_10, 8_0 }
    function cookiesForURL (URL: NSURL): NSArray; message 'cookiesForURL:';
    procedure setCookies_forURL_mainDocumentURL (cookies_: NSArray; URL: NSURL; mainDocumentURL: NSURL); message 'setCookies:forURL:mainDocumentURL:';
    procedure setCookieAcceptPolicy(newValue: NSHTTPCookieAcceptPolicy); message 'setCookieAcceptPolicy:';
    function cookieAcceptPolicy: NSHTTPCookieAcceptPolicy; message 'cookieAcceptPolicy';
    function sortedCookiesUsingDescriptors (sortOrder: NSArray): NSArray; message 'sortedCookiesUsingDescriptors:'; { available in 10_7, 5_0 }
  end;


type
  NSHTTPCookieStorage_NSURLSessionTaskAdditions = objccategory external name 'NSURLSessionTaskAdditions' (NSHTTPCookieStorage)
    procedure storeCookies_forTask (cookies_: NSArray; task: NSURLSessionTask); message 'storeCookies:forTask:'; { available in 10_10, 8_0 }
    procedure getCookiesForTask_completionHandler (task: NSURLSessionTask; completionHandler: OpaqueCBlock); message 'getCookiesForTask:completionHandler:'; { available in 10_10, 8_0 }
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSHTTPCookieManagerAcceptPolicyChangedNotification: NSString; cvar; external;
  NSHTTPCookieManagerCookiesChangedNotification: NSString; cvar; external;
{$endif}

