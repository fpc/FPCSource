{ Parsed from WebKit.framework WKUIDelegate.h }


{$ifdef TYPES}
type
  WKUIDelegateProtocolPtr = ^WKUIDelegateProtocol;
{$endif}

{$if defined(WK_API_ENABLED)}
{$ifdef PROTOCOLS}

type
  WKUIDelegateProtocol = objcprotocol external name 'WKUIDelegate' (NSObjectProtocol)
  optional
    function webView_createWebViewWithConfiguration_forNavigationAction_windowFeatures (webView: WKWebView; configuration: WKWebViewConfiguration; navigationAction: WKNavigationAction; windowFeatures: WKWindowFeatures): WKWebView; message 'webView:createWebViewWithConfiguration:forNavigationAction:windowFeatures:';
    procedure webView_runJavaScriptAlertPanelWithMessage_initiatedByFrame_completionHandler (webView: WKWebView; message_: NSString; frame: WKFrameInfo; completionHandler: OpaqueCBlock); message 'webView:runJavaScriptAlertPanelWithMessage:initiatedByFrame:completionHandler:';
    procedure webView_runJavaScriptConfirmPanelWithMessage_initiatedByFrame_completionHandler (webView: WKWebView; message_: NSString; frame: WKFrameInfo; completionHandler: OpaqueCBlock); message 'webView:runJavaScriptConfirmPanelWithMessage:initiatedByFrame:completionHandler:';
    procedure webView_runJavaScriptTextInputPanelWithPrompt_defaultText_initiatedByFrame_completionHandler (webView: WKWebView; prompt: NSString; defaultText: NSString; frame: WKFrameInfo; completionHandler: OpaqueCBlock); message 'webView:runJavaScriptTextInputPanelWithPrompt:defaultText:initiatedByFrame:completionHandler:';
  end;
{$endif}

{$endif}
