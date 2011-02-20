{ Parsed from Webkit.framework WebDataSource.h }

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
  WebDataSourcePtr = WebDataSourcePointer;

{$endif}

{$ifdef CLASSES}
{$ifndef WEBDATASOURCE_PAS_C}
{$define WEBDATASOURCE_PAS_C}

{ WebDataSource }
  WebDataSource = objcclass external (NSObject)
  private
    _private: WebDataSourcePrivate;
    
  public
    function initWithRequest(request_: NSURLRequest): id; message 'initWithRequest:';
    function data: NSData; message 'data';
    function representation: WebDocumentRepresentationProtocol; message 'representation';
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
  end;

{$endif}
{$endif}
