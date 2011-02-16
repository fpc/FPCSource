{ Parsed from Foundation.framework NSHTTPCookieStorage.h }

{$ifdef TYPES}
{$ifndef NSHTTPCOOKIESTORAGE_PAS_T}
{$define NSHTTPCOOKIESTORAGE_PAS_T}

{ Constants }

const
  NSHTTPCookieAcceptPolicyAlways = 0;
  NSHTTPCookieAcceptPolicyNever = 1;
  NSHTTPCookieAcceptPolicyOnlyFromMainDocumentDomain = 2;

{ Types }
type
  NSHTTPCookieAcceptPolicy = NSUInteger;
  NSHTTPCookieAcceptPolicyPtr = ^NSHTTPCookieAcceptPolicy;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSHTTPCOOKIESTORAGE_PAS_R}
{$define NSHTTPCOOKIESTORAGE_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSHTTPCOOKIESTORAGE_PAS_F}
{$define NSHTTPCOOKIESTORAGE_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSHTTPCOOKIESTORAGE_PAS_S}
{$define NSHTTPCOOKIESTORAGE_PAS_S}

{ External string constants }
var
  NSHTTPCookieManagerAcceptPolicyChangedNotification: NSString; cvar; external;
  NSHTTPCookieManagerCookiesChangedNotification: NSString; cvar; external;

{$endif}
{$endif}

{$ifdef FORWARD}
  NSHTTPCookieStorage = objcclass;
  NSHTTPCookieStoragePointer = ^NSHTTPCookieStorage;
  NSHTTPCookieStoragePtr = NSHTTPCookieStoragePointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSHTTPCOOKIESTORAGE_PAS_C}
{$define NSHTTPCOOKIESTORAGE_PAS_C}

{ NSHTTPCookieStorage }
  NSHTTPCookieStorage = objcclass external (NSObject)
  private
    _internal: NSHTTPCookieStorageInternal;
    
  public
    class function sharedHTTPCookieStorage: NSHTTPCookieStorage; message 'sharedHTTPCookieStorage';
    function initWithStorageLocation(storageFileURL: NSURL): id; message 'initWithStorageLocation:';
    function cookies: NSArray; message 'cookies';
    procedure setCookie(cookie: NSHTTPCookie); message 'setCookie:';
    procedure deleteCookie(cookie: NSHTTPCookie); message 'deleteCookie:';
    function cookiesForURL(URL: NSURL): NSArray; message 'cookiesForURL:';
    procedure setCookies_forURL_mainDocumentURL(cookies_: NSArray; URL: NSURL; mainDocumentURL: NSURL); message 'setCookies:forURL:mainDocumentURL:';
    function cookieAcceptPolicy: NSHTTPCookieAcceptPolicy; message 'cookieAcceptPolicy';
    procedure setCookieAcceptPolicy(cookieAcceptPolicy_: NSHTTPCookieAcceptPolicy); message 'setCookieAcceptPolicy:';
  end;

{$endif}
{$endif}
