{ Parsed from Foundation.framework NSURLResponse.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Thu Oct 1 18:54:07 ICT 2009 }


{$ifdef TYPES}
{$ifndef NSURLRESPONSE_PAS_T}
{$define NSURLRESPONSE_PAS_T}

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
  NSHTTPURLResponse = objcclass;
  NSHTTPURLResponsePointer = ^NSHTTPURLResponse;

{$endif}

{$ifdef CLASSES}
{$ifndef NSURLRESPONSE_PAS_C}
{$define NSURLRESPONSE_PAS_C}

{ NSURLResponse }
  NSURLResponse = objcclass(NSObject, NSCodingProtocol, NSCopyingProtocol)
  private
    __internal: NSURLResponseInternal;
    
  public
    class function alloc: NSURLResponse; message 'alloc';

    function initWithURL_MIMEType_expectedContentLength_textEncodingName(URL_: NSURL; MIMEType_: NSString; length: NSInteger; name: NSString): id; message 'initWithURL:MIMEType:expectedContentLength:textEncodingName:';
    function URL: NSURL; message 'URL';
    function MIMEType: NSString; message 'MIMEType';
    function expectedContentLength: clonglong; message 'expectedContentLength';
    function textEncodingName: NSString; message 'textEncodingName';
    function suggestedFilename: NSString; message 'suggestedFilename';
  end; external;

{ NSHTTPURLResponse }
  NSHTTPURLResponse = objcclass(NSURLResponse)
  private
    __httpInternal: NSHTTPURLResponseInternal;
    
  public
    class function alloc: NSHTTPURLResponse; message 'alloc';

    function statusCode: NSInteger; message 'statusCode';
    function allHeaderFields: NSDictionary; message 'allHeaderFields';
    class function localizedStringForStatusCode(statusCode_: NSInteger): NSString; message 'localizedStringForStatusCode:';
  end; external;

{$endif}
{$endif}
