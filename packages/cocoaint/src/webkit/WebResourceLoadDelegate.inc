{ Parsed from Webkit.framework WebResourceLoadDelegate.h }

{$ifdef TYPES}
{$ifndef WEBRESOURCELOADDELEGATE_PAS_T}
{$define WEBRESOURCELOADDELEGATE_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef WEBRESOURCELOADDELEGATE_PAS_R}
{$define WEBRESOURCELOADDELEGATE_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef WEBRESOURCELOADDELEGATE_PAS_F}
{$define WEBRESOURCELOADDELEGATE_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef WEBRESOURCELOADDELEGATE_PAS_S}
{$define WEBRESOURCELOADDELEGATE_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}

{$endif}

{$ifdef CLASSES}
{$ifndef WEBRESOURCELOADDELEGATE_PAS_C}
{$define WEBRESOURCELOADDELEGATE_PAS_C}

{ WebResourceLoadDelegateCategory }
  WebResourceLoadDelegateCategory = objccategory external (NSObject)
    function webView_identifierForInitialRequest_fromDataSource(sender: WebView; request: NSURLRequest; dataSource: WebDataSource): id; message 'webView:identifierForInitialRequest:fromDataSource:';
    function webView_resource_willSendRequest_redirectResponse_fromDataSource(sender: WebView; identifier: id; request: NSURLRequest; redirectResponse: NSURLResponse; dataSource: WebDataSource): NSURLRequest; message 'webView:resource:willSendRequest:redirectResponse:fromDataSource:';
    procedure webView_resource_didReceiveAuthenticationChallenge_fromDataSource(sender: WebView; identifier: id; challenge: NSURLAuthenticationChallenge; dataSource: WebDataSource); message 'webView:resource:didReceiveAuthenticationChallenge:fromDataSource:';
    procedure webView_resource_didCancelAuthenticationChallenge_fromDataSource(sender: WebView; identifier: id; challenge: NSURLAuthenticationChallenge; dataSource: WebDataSource); message 'webView:resource:didCancelAuthenticationChallenge:fromDataSource:';
    procedure webView_resource_didReceiveResponse_fromDataSource(sender: WebView; identifier: id; response: NSURLResponse; dataSource: WebDataSource); message 'webView:resource:didReceiveResponse:fromDataSource:';
    procedure webView_resource_didReceiveContentLength_fromDataSource(sender: WebView; identifier: id; length: WebNSInteger; dataSource: WebDataSource); message 'webView:resource:didReceiveContentLength:fromDataSource:';
    procedure webView_resource_didFinishLoadingFromDataSource(sender: WebView; identifier: id; dataSource: WebDataSource); message 'webView:resource:didFinishLoadingFromDataSource:';
    procedure webView_resource_didFailLoadingWithError_fromDataSource(sender: WebView; identifier: id; error: NSError; dataSource: WebDataSource); message 'webView:resource:didFailLoadingWithError:fromDataSource:';
    procedure webView_plugInFailedWithError_dataSource(sender: WebView; error: NSError; dataSource: WebDataSource); message 'webView:plugInFailedWithError:dataSource:';
  end;

{$endif}
{$endif}
