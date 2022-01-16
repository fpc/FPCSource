{ Parsed from WebKit.framework WKNavigationDelegate.h }


{$ifdef TYPES}
type
  WKNavigationDelegateProtocolPtr = ^WKNavigationDelegateProtocol;
{$endif}

{$if defined(WK_API_ENABLED)}
{$ifdef TYPES}
type
  WKNavigationActionPolicy = NSInteger;
  WKNavigationActionPolicyPtr = ^WKNavigationActionPolicy;

const
  WKNavigationActionPolicyCancel = 0;
  WKNavigationActionPolicyAllow = 1;

type
  WKNavigationResponsePolicy = NSInteger;
  WKNavigationResponsePolicyPtr = ^WKNavigationResponsePolicy;

const
  WKNavigationResponsePolicyCancel = 0;
  WKNavigationResponsePolicyAllow = 1;
{$endif}

{$ifdef PROTOCOLS}

type
  WKNavigationDelegateProtocol = objcprotocol external name 'WKNavigationDelegate' (NSObjectProtocol)
  optional
    procedure webView_decidePolicyForNavigationAction_decisionHandler (webView: WKWebView; navigationAction: WKNavigationAction; decisionHandler: OpaqueCBlock); message 'webView:decidePolicyForNavigationAction:decisionHandler:';
    procedure webView_decidePolicyForNavigationResponse_decisionHandler (webView: WKWebView; navigationResponse: WKNavigationResponse; decisionHandler: OpaqueCBlock); message 'webView:decidePolicyForNavigationResponse:decisionHandler:';
    procedure webView_didStartProvisionalNavigation (webView: WKWebView; navigation: WKNavigation); message 'webView:didStartProvisionalNavigation:';
    procedure webView_didReceiveServerRedirectForProvisionalNavigation (webView: WKWebView; navigation: WKNavigation); message 'webView:didReceiveServerRedirectForProvisionalNavigation:';
    procedure webView_didFailProvisionalNavigation_withError (webView: WKWebView; navigation: WKNavigation; error: NSError); message 'webView:didFailProvisionalNavigation:withError:';
    procedure webView_didCommitNavigation (webView: WKWebView; navigation: WKNavigation); message 'webView:didCommitNavigation:';
    procedure webView_didFinishNavigation (webView: WKWebView; navigation: WKNavigation); message 'webView:didFinishNavigation:';
    procedure webView_didFailNavigation_withError (webView: WKWebView; navigation: WKNavigation; error: NSError); message 'webView:didFailNavigation:withError:';
    procedure webView_didReceiveAuthenticationChallenge_completionHandler (webView: WKWebView; challenge: NSURLAuthenticationChallenge; completionHandler: OpaqueCBlock); message 'webView:didReceiveAuthenticationChallenge:completionHandler:';
  end;
{$endif}

{$endif}
