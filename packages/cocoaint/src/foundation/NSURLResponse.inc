{ Parsed from Foundation.framework NSURLResponse.h }

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
  NSURLResponse = objcclass external (NSObject, NSCodingProtocol, NSCopyingProtocol)
  private
    _internal: NSURLResponseInternal;
    
  public
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
  end;

{ NSHTTPURLResponse }
  NSHTTPURLResponse = objcclass external (NSURLResponse)
  private
    _httpInternal: NSHTTPURLResponseInternal;
    
  public
    function statusCode: NSInteger; message 'statusCode';
    function allHeaderFields: NSDictionary; message 'allHeaderFields';
    class function localizedStringForStatusCode(statusCode_: NSInteger): NSString; message 'localizedStringForStatusCode:';
  end;

{$endif}
{$endif}
