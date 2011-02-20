{ Parsed from Webkit.framework WebKitErrors.h }

{$ifdef TYPES}
{$ifndef WEBKITERRORS_PAS_T}
{$define WEBKITERRORS_PAS_T}

{ Constants }

const
  WebKitErrorCannotShowMIMEType = 100;
  WebKitErrorCannotShowURL = 101;
  WebKitErrorFrameLoadInterruptedByPolicyChange = 102;

const
  WebKitErrorCannotFindPlugIn = 200;
  WebKitErrorCannotLoadPlugIn = 201;
  WebKitErrorJavaUnavailable = 202;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef WEBKITERRORS_PAS_R}
{$define WEBKITERRORS_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef WEBKITERRORS_PAS_F}
{$define WEBKITERRORS_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef WEBKITERRORS_PAS_S}
{$define WEBKITERRORS_PAS_S}

{ External string constants }
var
  WebKitErrorDomain: NSString; cvar; external;
  WebKitErrorMIMETypeKey: NSString; cvar; external;
  WebKitErrorPlugInNameKey: NSString; cvar; external;
  WebKitErrorPlugInPageURLStringKey: NSString; cvar; external;

{$endif}
{$endif}
