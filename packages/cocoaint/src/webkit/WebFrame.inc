{ Parsed from WebKit.framework WebFrame.h }


{ Types from WebFrame }
{$ifdef TYPES}

{$endif}


{$ifdef TYPES}
type
  WebFramePtr = ^WebFrame;
{$endif}

{$ifdef CLASSES}

type
  WebFrame = objcclass external (NSObject)
  private
    _private: WebFramePrivate;
  public
    function initWithName_webFrameView_webView (name: NSString; view: WebFrameView; webView_: WebView): instancetype; message 'initWithName:webFrameView:webView:';
    function name: NSString; message 'name';
    function webView: WebView; message 'webView';
    function frameView: WebFrameView; message 'frameView';
    function DOMDocument: DOMDocument; message 'DOMDocument';
    function frameElement: DOMHTMLElement; message 'frameElement';
    procedure loadRequest (request: NSURLRequest); message 'loadRequest:';
    procedure loadData_MIMEType_textEncodingName_baseURL (data: NSData; MIMEType: NSString; encodingName: NSString; URL: NSURL); message 'loadData:MIMEType:textEncodingName:baseURL:';
    procedure loadHTMLString_baseURL (string_: NSString; URL: NSURL); message 'loadHTMLString:baseURL:';
    procedure loadAlternateHTMLString_baseURL_forUnreachableURL (string_: NSString; baseURL: NSURL; unreachableURL: NSURL); message 'loadAlternateHTMLString:baseURL:forUnreachableURL:';
    procedure loadArchive (archive: WebArchive); message 'loadArchive:';
    function dataSource: WebDataSource; message 'dataSource';
    function provisionalDataSource: WebDataSource; message 'provisionalDataSource';
    procedure stopLoading; message 'stopLoading';
    procedure reload; message 'reload';
    procedure reloadFromOrigin; message 'reloadFromOrigin';
    function findFrameNamed (name_: NSString): WebFrame; message 'findFrameNamed:';
    function parentFrame: WebFrame; message 'parentFrame';
    function childFrames: NSArray; message 'childFrames';
    function windowObject: WebScriptObject; message 'windowObject';
    function globalContext: JSGlobalContextRef; message 'globalContext';
    {$if defined(JSC_OBJC_API_ENABLED)}
    function javaScriptContext: JSContext; message 'javaScriptContext';
    {$endif}
  end;
{$endif}

