{ Parsed from Foundation.framework NSURLResponse.h }
{ Version: 2.1.2 - Wed Dec 8 10:06:43 CET 2010 }


{$ifdef TYPES}
{$ifndef NSURLRESPONSE_PAS_T}
{$define NSURLRESPONSE_PAS_T}

{ Defines }
const
  NSURLResponseUnknownLength = -1;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSURLRESPONSE_PAS_R}
{$define NSURLRESPONSE_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSURLRESPONSE_PAS_F}
{$define NSURLRESPONSE_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSURLRESPONSE_PAS_S}
{$define NSURLRESPONSE_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSURLResponse = objcclass;
  NSURLResponsePointer = ^NSURLResponse;
  NSURLResponsePtr = NSURLResponsePointer;
  NSHTTPURLResponse = objcclass;
  NSHTTPURLResponsePointer = ^NSHTTPURLResponse;
  NSHTTPURLResponsePtr = NSHTTPURLResponsePointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSURLRESPONSE_PAS_C}
{$define NSURLRESPONSE_PAS_C}

{ NSURLResponse }
  NSURLResponse = objcclass(NSObject, NSCodingProtocol, NSCopyingProtocol)
  private
    _internal: NSURLResponseInternal;
    
  public
    class function alloc: NSURLResponse; message 'alloc';

    function initWithURL_MIMEType_expectedContentLength_textEncodingName(URL_: NSURL; MIMEType_: NSString; length: NSInteger; name: NSString): id; message 'initWithURL:MIMEType:expectedContentLength:textEncodingName:';
    function URL: NSURL; message 'URL';
    function MIMEType: NSString; message 'MIMEType';
    function expectedContentLength: clonglong; message 'expectedContentLength';
    function textEncodingName: NSString; message 'textEncodingName';
    function suggestedFilename: NSString; message 'suggestedFilename';

    { Adopted Protocols }
    procedure encodeWithCoder(aCoder: NSCoder);
    function initWithCoder(aDecoder: NSCoder): id;
    function copyWithZone(zone_: NSZonePtr): id;
  end; external;

{ NSHTTPURLResponse }
  NSHTTPURLResponse = objcclass(NSURLResponse)
  private
    _httpInternal: NSHTTPURLResponseInternal;
    
  public
    class function alloc: NSHTTPURLResponse; message 'alloc';

    function statusCode: NSInteger; message 'statusCode';
    function allHeaderFields: NSDictionary; message 'allHeaderFields';
    class function localizedStringForStatusCode(statusCode_: NSInteger): NSString; message 'localizedStringForStatusCode:';
  end; external;

{$endif}
{$endif}
