{ Parsed from WebKit.framework WKNavigationResponse.h }


{$ifdef TYPES}
type
  WKNavigationResponsePtr = ^WKNavigationResponse;
{$endif}

{$if defined(WK_API_ENABLED)}
{$ifdef CLASSES}

type
  WKNavigationResponse = objcclass external (NSObject)
  public
    function isForMainFrame: ObjCBOOL; message 'isForMainFrame';
    function response: NSURLResponse; message 'response';
    function canShowMIMEType: ObjCBOOL; message 'canShowMIMEType';
  end;
{$endif}

{$endif}
