{ Parsed from Foundation.framework NSHTTPCookie.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Tue Sep 8 15:31:00 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSHTTPCOOKIE_PAS_H}
{$define NSHTTPCOOKIE_PAS_H}
type
  NSHTTPCookiePointer = Pointer;

{$endif}
{$endif}

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
{$ifndef NSHTTPCOOKIE_PAS_T}
{$define NSHTTPCOOKIE_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSHTTPCookie = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSHTTPCOOKIE_PAS_C}
{$define NSHTTPCOOKIE_PAS_C}

{ NSHTTPCookie }
  NSHTTPCookie = objcclass(NSObject)
  private
    __cookiePrivate: NSHTTPCookieInternal;
    
  public
    class function alloc: NSHTTPCookie; message 'alloc';

    function initWithProperties(properties_: NSDictionary): id; message 'initWithProperties:';
    class function cookieWithProperties(properties_: NSDictionary): id; message 'cookieWithProperties:';
    class function requestHeaderFieldsWithCookies(cookies: NSArray): NSDictionary; message 'requestHeaderFieldsWithCookies:';
    class function cookiesWithResponseHeaderFields_forURL(headerFields: NSDictionary; URL: NSURL): NSArray; message 'cookiesWithResponseHeaderFields:forURL:';
    function properties: NSDictionary; message 'properties';
    function version: culong; message 'version';
    function name: NSString; message 'name';
    function value: NSString; message 'value';
    function expiresDate: NSDate; message 'expiresDate';
    function isSessionOnly: Boolean; message 'isSessionOnly';
    function domain: NSString; message 'domain';
    function path: NSString; message 'path';
    function isSecure: Boolean; message 'isSecure';
    function comment: NSString; message 'comment';
    function commentURL: NSURL; message 'commentURL';
    function portList: NSArray; message 'portList';
  end; external;

{$endif}
{$endif}
