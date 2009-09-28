{ Parsed from Foundation.framework NSURL.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Mon Sep 28 15:49:52 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSURL_PAS_H}
{$define NSURL_PAS_H}
type
  NSURLPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSURL_PAS_T}
{$define NSURL_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSURL_PAS_R}
{$define NSURL_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSURL_PAS_F}
{$define NSURL_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSURL_PAS_S}
{$define NSURL_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSURL = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSURL_PAS_C}
{$define NSURL_PAS_C}

{ NSURL }
  NSURL = objcclass(NSObject, NSCodingProtocol, NSCopyingProtocol, NSURLHandleClientProtocol)
  private
    __urlString: NSString;
    __baseURL: NSURL;
    __clients: Pointer;
    __reserved: Pointer; {garbage collector: __strong }
    
  public
    class function alloc: NSURL; message 'alloc';

    class function fileURLWithPath_isDirectory(path_: NSString; isDir: Boolean): id; message 'fileURLWithPath:isDirectory:';
    class function fileURLWithPath(path_: NSString): id; message 'fileURLWithPath:';
    class function URLWithString(URLString: NSString): id; message 'URLWithString:';
    class function URLWithString_relativeToURL(URLString: NSString; baseURL_: NSURL): id; message 'URLWithString:relativeToURL:';
    function absoluteString: NSString; message 'absoluteString';
    function relativeString: NSString; message 'relativeString';
    function baseURL: NSURL; message 'baseURL';
    function absoluteURL: NSURL; message 'absoluteURL';
    function scheme: NSString; message 'scheme';
    function resourceSpecifier: NSString; message 'resourceSpecifier';
    function host: NSString; message 'host';
    function port: NSNumber; message 'port';
    function user: NSString; message 'user';
    function password: NSString; message 'password';
    function path: NSString; message 'path';
    function fragment: NSString; message 'fragment';
    function parameterString: NSString; message 'parameterString';
    function query: NSString; message 'query';
    function relativePath: NSString; message 'relativePath';
    function isFileURL: Boolean; message 'isFileURL';
    function standardizedURL: NSURL; message 'standardizedURL';

    { Category: NSURLLoading }
    function resourceDataUsingCache(shouldUseCache: Boolean): NSData; message 'resourceDataUsingCache:';
    procedure loadResourceDataNotifyingClient_usingCache(client: id; shouldUseCache: Boolean); message 'loadResourceDataNotifyingClient:usingCache:';
    function propertyForKey(propertyKey: NSString): id; message 'propertyForKey:';
    function setResourceData(data: NSData): Boolean; message 'setResourceData:';
    function setProperty_forKey(property_: id; propertyKey: NSString): Boolean; message 'setProperty:forKey:';
    function URLHandleUsingCache(shouldUseCache: Boolean): NSURLHandle; message 'URLHandleUsingCache:';

    { Category: NSPasteboardSupport }
    class function URLFromPasteboard(pasteBoard: NSPasteboard): NSURL; message 'URLFromPasteboard:';
    procedure writeToPasteboard(pasteBoard: NSPasteboard); message 'writeToPasteboard:';
  end; external;

{$endif}
{$endif}
