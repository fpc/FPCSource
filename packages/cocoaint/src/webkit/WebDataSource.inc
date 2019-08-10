{ Parsed from WebKit.framework WebDataSource.h }


{$ifdef TYPES}
type
  WebDataSourcePtr = ^WebDataSource;
{$endif}

{$ifdef CLASSES}

type
  WebDataSource = objcclass external (NSObject)
  private
    _private: pointer;
  public
    function initWithRequest (request: NSURLRequest): instancetype; message 'initWithRequest:';
    function data: NSData; message 'data';
    function representation: WebDocumentRepresentationProtocol; message 'representation';
    function webFrame: WebFrame; message 'webFrame';
    function initialRequest: NSURLRequest; message 'initialRequest';
    function request: NSMutableURLRequest; message 'request';
    function response: NSURLResponse; message 'response';
    function textEncodingName: NSString; message 'textEncodingName';
    function isLoading: ObjCBOOL; message 'isLoading';
    function pageTitle: NSString; message 'pageTitle';
    function unreachableURL: NSURL; message 'unreachableURL';
    function webArchive: WebArchive; message 'webArchive';
    function mainResource: WebResource; message 'mainResource';
    function subresources: NSArray; message 'subresources';
    function subresourceForURL (URL: NSURL): WebResource; message 'subresourceForURL:';
    procedure addSubresource (subresource: WebResource); message 'addSubresource:';
  end;
{$endif}

