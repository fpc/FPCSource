{ Parsed from WebKit.framework WKScriptMessage.h }


{$ifdef TYPES}
type
  WKScriptMessagePtr = ^WKScriptMessage;
{$endif}

{$if defined(WK_API_ENABLED)}
{$ifdef CLASSES}

type
  WKScriptMessage = objcclass external (NSObject)
  public
    function body: id; message 'body';
    function webView: WKWebView; message 'webView';
    function frameInfo: WKFrameInfo; message 'frameInfo';
    function name: NSString; message 'name';
  end;
{$endif}

{$endif}
