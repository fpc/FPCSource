{ Parsed from WebKit.framework WebKitErrors.h }

{$ifdef EXTERNAL_SYMBOLS}
var
  WebKitErrorDomain: NSString; cvar; external;
  WebKitErrorMIMETypeKey: NSString; cvar; external;
  WebKitErrorPlugInNameKey: NSString; cvar; external;
  WebKitErrorPlugInPageURLStringKey: NSString; cvar; external;
{$endif}

{$ifdef TYPES}
const
  WebKitErrorCannotShowMIMEType = 100;
  WebKitErrorCannotShowURL = 101;
  WebKitErrorFrameLoadInterruptedByPolicyChange = 102;
const
  WebKitErrorCannotFindPlugIn = 200;
  WebKitErrorCannotLoadPlugIn = 201;
  WebKitErrorJavaUnavailable = 202;
  WebKitErrorBlockedPlugInVersion = 203;
{$endif}

