{ Parsed from Webkit.framework WebDataSource.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 10 8:18:59 ICT 2009 }


{$ifdef TYPES}
{$ifndef WEBDATASOURCE_PAS_T}
{$define WEBDATASOURCE_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef WEBDATASOURCE_PAS_R}
{$define WEBDATASOURCE_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef WEBDATASOURCE_PAS_F}
{$define WEBDATASOURCE_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef WEBDATASOURCE_PAS_S}
{$define WEBDATASOURCE_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  WebDataSource = objcclass;
  WebDataSourcePointer = ^WebDataSource;

{$endif}

{$ifdef CLASSES}
{$ifndef WEBDATASOURCE_PAS_C}
{$define WEBDATASOURCE_PAS_C}

{ WebDataSource }
  WebDataSource = objcclass(NSObject)
  private
    __private: WebDataSourcePrivate;
    
  public
    class function alloc: WebDataSource; message 'alloc';

    function initWithRequest(request_: NSURLRequest): id; message 'initWithRequest:';
    function data: NSData; message 'data';
    function representation: id; message 'representation';
    function webFrame: WebFrame; message 'webFrame';
    function initialRequest: NSURLRequest; message 'initialRequest';
    function request: NSMutableURLRequest; message 'request';
    function response: NSURLResponse; message 'response';
    function textEncodingName: NSString; message 'textEncodingName';
    function isLoading: Boolean; message 'isLoading';
    function pageTitle: NSString; message 'pageTitle';
    function unreachableURL: NSURL; message 'unreachableURL';
    function webArchive: WebArchive; message 'webArchive';
    function mainResource: WebResource; message 'mainResource';
    function subresources: NSArray; message 'subresources';
    function subresourceForURL(URL: NSURL): WebResource; message 'subresourceForURL:';
    procedure addSubresource(subresource: WebResource); message 'addSubresource:';
  end; external;

{$endif}
{$endif}
