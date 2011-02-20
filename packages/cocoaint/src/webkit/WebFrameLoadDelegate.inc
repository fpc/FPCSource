{ Parsed from Webkit.framework WebFrameLoadDelegate.h }

{$ifdef TYPES}
{$ifndef WEBFRAMELOADDELEGATE_PAS_T}
{$define WEBFRAMELOADDELEGATE_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef WEBFRAMELOADDELEGATE_PAS_R}
{$define WEBFRAMELOADDELEGATE_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef WEBFRAMELOADDELEGATE_PAS_F}
{$define WEBFRAMELOADDELEGATE_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef WEBFRAMELOADDELEGATE_PAS_S}
{$define WEBFRAMELOADDELEGATE_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}

{$endif}

{$ifdef CLASSES}
{$ifndef WEBFRAMELOADDELEGATE_PAS_C}
{$define WEBFRAMELOADDELEGATE_PAS_C}

{ WebFrameLoadDelegateCategory }
  WebFrameLoadDelegateCategory = objccategory external (NSObject)
    procedure webView_didStartProvisionalLoadForFrame(sender: WebView; frame: WebFrame); message 'webView:didStartProvisionalLoadForFrame:';
    procedure webView_didReceiveServerRedirectForProvisionalLoadForFrame(sender: WebView; frame: WebFrame); message 'webView:didReceiveServerRedirectForProvisionalLoadForFrame:';
    procedure webView_didFailProvisionalLoadWithError_forFrame(sender: WebView; error: NSError; frame: WebFrame); message 'webView:didFailProvisionalLoadWithError:forFrame:';
    procedure webView_didCommitLoadForFrame(sender: WebView; frame: WebFrame); message 'webView:didCommitLoadForFrame:';
    procedure webView_didReceiveTitle_forFrame(sender: WebView; title: NSString; frame: WebFrame); message 'webView:didReceiveTitle:forFrame:';
    procedure webView_didReceiveIcon_forFrame(sender: WebView; image: NSImage; frame: WebFrame); message 'webView:didReceiveIcon:forFrame:';
    procedure webView_didFinishLoadForFrame(sender: WebView; frame: WebFrame); message 'webView:didFinishLoadForFrame:';
    procedure webView_didFailLoadWithError_forFrame(sender: WebView; error: NSError; frame: WebFrame); message 'webView:didFailLoadWithError:forFrame:';
    procedure webView_didChangeLocationWithinPageForFrame(sender: WebView; frame: WebFrame); message 'webView:didChangeLocationWithinPageForFrame:';
    procedure webView_willPerformClientRedirectToURL_delay_fireDate_forFrame(sender: WebView; URL: NSURL; seconds: NSTimeInterval; date: NSDate; frame: WebFrame); message 'webView:willPerformClientRedirectToURL:delay:fireDate:forFrame:';
    procedure webView_didCancelClientRedirectForFrame(sender: WebView; frame: WebFrame); message 'webView:didCancelClientRedirectForFrame:';
    procedure webView_willCloseFrame(sender: WebView; frame: WebFrame); message 'webView:willCloseFrame:';
    procedure webView_didClearWindowObject_forFrame(webView_: WebView; windowObject: WebScriptObject; frame: WebFrame); message 'webView:didClearWindowObject:forFrame:';
    procedure webView_windowScriptObjectAvailable(webView_: WebView; windowScriptObject: WebScriptObject); message 'webView:windowScriptObjectAvailable:'; deprecated 'in_webkit_version_3 and later';
  end;

{$endif}
{$endif}
