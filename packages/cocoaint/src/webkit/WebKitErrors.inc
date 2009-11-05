{ Parsed from Webkit.framework WebKitErrors.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 10 8:18:59 ICT 2009 }


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
  WebKitErrorDomain: NSString; external name '_WebKitErrorDomain';
  WebKitErrorMIMETypeKey: NSString; external name '_WebKitErrorMIMETypeKey';
  WebKitErrorPlugInNameKey: NSString; external name '_WebKitErrorPlugInNameKey';
  WebKitErrorPlugInPageURLStringKey: NSString; external name '_WebKitErrorPlugInPageURLStringKey';

{$endif}
{$endif}
