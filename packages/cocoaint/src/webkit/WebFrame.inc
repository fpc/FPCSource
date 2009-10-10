{ Parsed from Webkit.framework WebFrame.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 10 8:18:59 ICT 2009 }


{$ifdef TYPES}
{$ifndef WEBFRAME_PAS_T}
{$define WEBFRAME_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef WEBFRAME_PAS_R}
{$define WEBFRAME_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef WEBFRAME_PAS_F}
{$define WEBFRAME_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef WEBFRAME_PAS_S}
{$define WEBFRAME_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  WebFrame = objcclass;
  WebFramePointer = ^WebFrame;

{$endif}

{$ifdef CLASSES}
{$ifndef WEBFRAME_PAS_C}
{$define WEBFRAME_PAS_C}

{ WebFrame }
  WebFrame = objcclass(NSObject)
  private
    __private: WebFramePrivate;
    
  public
    class function alloc: WebFrame; message 'alloc';

    function initWithName_webFrameView_webView(name_: NSString; view: WebFrameView; webView_: webView_Pointer): id; message 'initWithName:webFrameView:webView:';
    function name: NSString; message 'name';
    function webView: WebView; message 'webView';
    function frameView: WebFrameView; message 'frameView';
    function DOMDocument: DOMDocument; message 'DOMDocument';
    function frameElement: DOMHTMLElement; message 'frameElement';
    procedure loadRequest(request: NSURLRequest); message 'loadRequest:';
    procedure loadData_MIMEType_textEncodingName_baseURL(data: NSData; MIMEType: NSString; encodingName: NSString; URL: NSURL); message 'loadData:MIMEType:textEncodingName:baseURL:';
    procedure loadHTMLString_baseURL(string_: NSString; URL: NSURL); message 'loadHTMLString:baseURL:';
    procedure loadAlternateHTMLString_baseURL_forUnreachableURL(string_: NSString; baseURL: NSURL; unreachableURL: NSURL); message 'loadAlternateHTMLString:baseURL:forUnreachableURL:';
    procedure loadArchive(archive: WebArchive); message 'loadArchive:';
    function dataSource: WebDataSource; message 'dataSource';
    function provisionalDataSource: WebDataSource; message 'provisionalDataSource';
    procedure stopLoading; message 'stopLoading';
    procedure reload; message 'reload';
    function findFrameNamed(name_: NSString): WebFrame; message 'findFrameNamed:';
    function parentFrame: WebFrame; message 'parentFrame';
    function childFrames: NSArray; message 'childFrames';
    function windowObject: WebScriptObject; message 'windowObject';
    function globalContext: JSGlobalContextRef; message 'globalContext';
  end; external;

{$endif}
{$endif}
