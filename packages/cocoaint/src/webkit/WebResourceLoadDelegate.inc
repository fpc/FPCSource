{ Parsed from WebKit.framework WebResourceLoadDelegate.h }

{$ifdef CLASSES}

type
  WebResourceLoadDelegate = objccategory external (NSObject)
    function webView_identifierForInitialRequest_fromDataSource (sender: WebView; request: NSURLRequest; dataSource: WebDataSource): id; message 'webView:identifierForInitialRequest:fromDataSource:';
    function webView_resource_willSendRequest_redirectResponse_fromDataSource (sender: WebView; identifier: id; request: NSURLRequest; redirectResponse: NSURLResponse; dataSource: WebDataSource): NSURLRequest; message 'webView:resource:willSendRequest:redirectResponse:fromDataSource:';
    procedure webView_resource_didReceiveAuthenticationChallenge_fromDataSource (sender: WebView; identifier: id; challenge: NSURLAuthenticationChallenge; dataSource: WebDataSource); message 'webView:resource:didReceiveAuthenticationChallenge:fromDataSource:';
    procedure webView_resource_didCancelAuthenticationChallenge_fromDataSource (sender: WebView; identifier: id; challenge: NSURLAuthenticationChallenge; dataSource: WebDataSource); message 'webView:resource:didCancelAuthenticationChallenge:fromDataSource:';
    procedure webView_resource_didReceiveResponse_fromDataSource (sender: WebView; identifier: id; response: NSURLResponse; dataSource: WebDataSource); message 'webView:resource:didReceiveResponse:fromDataSource:';
    procedure webView_resource_didReceiveContentLength_fromDataSource (sender: WebView; identifier: id; length: NSInteger; dataSource: WebDataSource); message 'webView:resource:didReceiveContentLength:fromDataSource:';
    procedure webView_resource_didFinishLoadingFromDataSource (sender: WebView; identifier: id; dataSource: WebDataSource); message 'webView:resource:didFinishLoadingFromDataSource:';
    procedure webView_resource_didFailLoadingWithError_fromDataSource (sender: WebView; identifier: id; error: NSError; dataSource: WebDataSource); message 'webView:resource:didFailLoadingWithError:fromDataSource:';
    procedure webView_plugInFailedWithError_dataSource (sender: WebView; error: NSError; dataSource: WebDataSource); message 'webView:plugInFailedWithError:dataSource:';
  end;
{$endif}

