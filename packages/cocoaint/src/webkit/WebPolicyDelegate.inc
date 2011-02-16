{ Parsed from Webkit.framework WebPolicyDelegate.h }

{$ifdef TYPES}
{$ifndef WEBPOLICYDELEGATE_PAS_T}
{$define WEBPOLICYDELEGATE_PAS_T}

{ Types }
type
  
  WebNavigationType = culong;

{ Constants }

const
  WebNavigationTypeLinkClicked = 0;
  WebNavigationTypeFormSubmitted = 1;
  WebNavigationTypeBackForward = 2;
  WebNavigationTypeReload = 3;
  WebNavigationTypeFormResubmitted = 4;
  WebNavigationTypeOther = 5;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef WEBPOLICYDELEGATE_PAS_R}
{$define WEBPOLICYDELEGATE_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef WEBPOLICYDELEGATE_PAS_F}
{$define WEBPOLICYDELEGATE_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef WEBPOLICYDELEGATE_PAS_S}
{$define WEBPOLICYDELEGATE_PAS_S}

{ External string constants }
var
  WebActionNavigationTypeKey: NSString; cvar; external;
  WebActionElementKey: NSString; cvar; external;
  WebActionButtonKey: NSString; cvar; external;
  WebActionModifierFlagsKey: NSString; cvar; external;
  WebActionOriginalURLKey: NSString; cvar; external;

{$endif}
{$endif}

{$ifdef FORWARD}
  WebPolicyDecisionListenerProtocol = objcprotocol;

{$endif}

{$ifdef CLASSES}
{$ifndef WEBPOLICYDELEGATE_PAS_C}
{$define WEBPOLICYDELEGATE_PAS_C}

{ WebPolicyDelegateCategory }
  WebPolicyDelegateCategory = objccategory external (NSObject)
    procedure webView_decidePolicyForNavigationAction_request_frame_decisionListener(webView_: WebView; actionInformation: NSDictionary; request: NSURLRequest; frame: WebFrame; listener: WebPolicyDecisionListenerProtocol); message 'webView:decidePolicyForNavigationAction:request:frame:decisionListener:';
    procedure webView_decidePolicyForNewWindowAction_request_newFrameName_decisionListener(webView_: WebView; actionInformation: NSDictionary; request: NSURLRequest; frameName: NSString; listener: WebPolicyDecisionListenerProtocol); message 'webView:decidePolicyForNewWindowAction:request:newFrameName:decisionListener:';
    procedure webView_decidePolicyForMIMEType_request_frame_decisionListener(webView_: WebView; type_: NSString; request: NSURLRequest; frame: WebFrame; listener: WebPolicyDecisionListenerProtocol); message 'webView:decidePolicyForMIMEType:request:frame:decisionListener:';
    procedure webView_unableToImplementPolicyWithError_frame(webView_: WebView; error: NSError; frame: WebFrame); message 'webView:unableToImplementPolicyWithError:frame:';
  end;

{$endif}
{$endif}
{$ifdef PROTOCOLS}
{$ifndef WEBPOLICYDELEGATE_PAS_P}
{$define WEBPOLICYDELEGATE_PAS_P}
  
{ WebPolicyDecisionListener Protocol }
  WebPolicyDecisionListenerProtocol = objcprotocol external name 'WebPolicyDecisionListener'
    procedure use; message 'use';
    procedure download; message 'download';
    procedure ignore; message 'ignore';
  end;
{$endif}
{$endif}
