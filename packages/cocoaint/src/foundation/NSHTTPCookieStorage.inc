{ Parsed from Foundation.framework NSHTTPCookieStorage.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Thu Sep 17 14:52:46 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSHTTPCOOKIESTORAGE_PAS_H}
{$define NSHTTPCOOKIESTORAGE_PAS_H}
type
  NSHTTPCookieStoragePointer = Pointer;

{$endif}
{$endif}

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
{$ifndef NSHTTPCOOKIESTORAGE_PAS_T}
{$define NSHTTPCOOKIESTORAGE_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSHTTPCookieStorage = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSHTTPCOOKIESTORAGE_PAS_C}
{$define NSHTTPCOOKIESTORAGE_PAS_C}

{ NSHTTPCookieStorage }
  NSHTTPCookieStorage = objcclass(NSObject)
  private
    __internal: NSHTTPCookieStorageInternal;
    
  public
    class function alloc: NSHTTPCookieStorage; message 'alloc';

    class function sharedHTTPCookieStorage: NSHTTPCookieStorage; message 'sharedHTTPCookieStorage';
    function cookies: NSArray; message 'cookies';
    procedure setCookie(cookie: NSHTTPCookie); message 'setCookie:';
    procedure deleteCookie(cookie: NSHTTPCookie); message 'deleteCookie:';
    function cookiesForURL(URL: NSURL): NSArray; message 'cookiesForURL:';
    procedure setCookies_forURL_mainDocumentURL(cookies_: NSArray; URL: NSURL; mainDocumentURL: NSURL); message 'setCookies:forURL:mainDocumentURL:';
    function cookieAcceptPolicy: NSHTTPCookieAcceptPolicy; message 'cookieAcceptPolicy';
    procedure setCookieAcceptPolicy(cookieAcceptPolicy_: NSHTTPCookieAcceptPolicy); message 'setCookieAcceptPolicy:';
  end; external;

{$endif}
{$endif}
