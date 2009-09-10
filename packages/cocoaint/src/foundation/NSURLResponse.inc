{ Parsed from Foundation.framework NSURLResponse.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Tue Sep 8 15:31:00 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSURLRESPONSE_PAS_H}
{$define NSURLRESPONSE_PAS_H}
type
  NSURLResponsePointer = Pointer;
  NSHTTPURLResponsePointer = Pointer;

{$endif}
{$endif}

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
{$ifndef NSURLRESPONSE_PAS_T}
{$define NSURLRESPONSE_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSURLResponse = objcclass;
  NSHTTPURLResponse = objcclass;

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

    function initWithURL_MIMEType_expectedContentLength_textEncodingName(URL_: NSURL; MIMEType_: NSString; length: clong; name: NSString): id; message 'initWithURL:MIMEType:expectedContentLength:textEncodingName:';
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

    function statusCode: clong; message 'statusCode';
    function allHeaderFields: NSDictionary; message 'allHeaderFields';
    class function localizedStringForStatusCode(statusCode_: clong): NSString; message 'localizedStringForStatusCode:';
  end; external;

{$endif}
{$endif}
