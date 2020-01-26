{ Parsed from Foundation.framework NSHTTPCookie.h }


{$ifdef TYPES}
type
  NSHTTPCookiePtr = ^NSHTTPCookie;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
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

{$ifdef CLASSES}

type
  NSHTTPCookie = objcclass external (NSObject)
  private
    _cookiePrivate: NSHTTPCookieInternal;
  public
    function initWithProperties (properties: NSDictionary): instancetype; message 'initWithProperties:';
    class function cookieWithProperties (properties: NSDictionary): NSHTTPCookie; message 'cookieWithProperties:';
    class function requestHeaderFieldsWithCookies (cookies: NSArray): NSDictionary; message 'requestHeaderFieldsWithCookies:';
    class function cookiesWithResponseHeaderFields_forURL (headerFields: NSDictionary; URL: NSURL): NSArray; message 'cookiesWithResponseHeaderFields:forURL:';
    function properties: NSDictionary; message 'properties';
    function version: NSUInteger; message 'version';
    function name: NSString; message 'name';
    function value: NSString; message 'value';
    function expiresDate: NSDate; message 'expiresDate';
    function isSessionOnly: ObjCBOOL; message 'isSessionOnly';
    function domain: NSString; message 'domain';
    function path: NSString; message 'path';
    function isSecure: ObjCBOOL; message 'isSecure';
    function isHTTPOnly: ObjCBOOL; message 'isHTTPOnly';
    function comment: NSString; message 'comment';
    function commentURL: NSURL; message 'commentURL';
    function portList: NSArray; message 'portList';
  end;
{$endif}

