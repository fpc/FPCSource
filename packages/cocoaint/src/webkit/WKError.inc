{ Parsed from WebKit.framework WKError.h }

{$if defined(WK_API_ENABLED)}
{$ifdef EXTERNAL_SYMBOLS}
var
  WKErrorDomain: NSString { available in 10_10, 8_0 }; cvar; external;
{$endif}

{$ifdef TYPES}
type
  WKErrorCode = NSInteger;
  WKErrorCodePtr = ^WKErrorCode;

const
  WKErrorUnknown = 1;
  WKErrorWebContentProcessTerminated = 2;
  WKErrorWebViewInvalidated = 3;
  WKErrorJavaScriptExceptionOccurred = 4;
{$endif}

{$endif}
