{ Parsed from Foundation.framework NSHTTPCookie.h }

{$ifdef TYPES}
{$ifndef NSHTTPCOOKIE_PAS_T}
{$define NSHTTPCOOKIE_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSHTTPCOOKIE_PAS_R}
{$define NSHTTPCOOKIE_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSHTTPCOOKIE_PAS_F}
{$define NSHTTPCOOKIE_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSHTTPCOOKIE_PAS_S}
{$define NSHTTPCOOKIE_PAS_S}

{ External string constants }
var
  NSHTTPCookieName: NSString; cvar; external;
  NSHTTPCookieValue: NSString; cvar; external;
  NSHTTPCookieOriginURL: NSString; cvar; external;
  NSHTTPCookieVersion: NSString; cvar; external;
  NSHTTPCookieDomain: NSString; cvar; external;
  NSHTTPCookiePath: NSString; cvar; external;
  NSHTTPCookieSecure: NSString; cvar; external;
  NSHTTPCookieExpires: NSString; cvar; external;
  NSHTTPCookieComment: NSString; cvar; external;
  NSHTTPCookieCommentURL: NSString; cvar; external;
  NSHTTPCookieDiscard: NSString; cvar; external;
  NSHTTPCookieMaximumAge: NSString; cvar; external;
  NSHTTPCookiePort: NSString; cvar; external;

{$endif}
{$endif}

{$ifdef FORWARD}
  NSHTTPCookie = objcclass;
  NSHTTPCookiePointer = ^NSHTTPCookie;
  NSHTTPCookiePtr = NSHTTPCookiePointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSHTTPCOOKIE_PAS_C}
{$define NSHTTPCOOKIE_PAS_C}

{ NSHTTPCookie }
  NSHTTPCookie = objcclass external (NSObject)
  private
    _cookiePrivate: NSHTTPCookieInternal;
    
  public
    function initWithProperties(properties_: NSDictionary): id; message 'initWithProperties:';
    class function cookieWithProperties(properties_: NSDictionary): id; message 'cookieWithProperties:';
    class function requestHeaderFieldsWithCookies(cookies: NSArray): NSDictionary; message 'requestHeaderFieldsWithCookies:';
    class function cookiesWithResponseHeaderFields_forURL(headerFields: NSDictionary; URL: NSURL): NSArray; message 'cookiesWithResponseHeaderFields:forURL:';
    function properties: NSDictionary; message 'properties';
    function version: NSUInteger; message 'version';
    function name: NSString; message 'name';
    function value: NSString; message 'value';
    function expiresDate: NSDate; message 'expiresDate';
    function isSessionOnly: Boolean; message 'isSessionOnly';
    function domain: NSString; message 'domain';
    function path: NSString; message 'path';
    function isSecure: Boolean; message 'isSecure';
    function isHTTPOnly: Boolean; message 'isHTTPOnly';
    function comment: NSString; message 'comment';
    function commentURL: NSURL; message 'commentURL';
    function portList: NSArray; message 'portList';
  end;

{$endif}
{$endif}
