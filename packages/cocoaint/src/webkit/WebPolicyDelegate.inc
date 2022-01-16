{ Parsed from WebKit.framework WebPolicyDelegate.h }


{$ifdef TYPES}
type
  WebPolicyDecisionListenerProtocolPtr = ^WebPolicyDecisionListenerProtocol;
{$endif}

{$ifdef TYPES}
type
  WebNavigationType = NSInteger;
  WebNavigationTypePtr = ^WebNavigationType;

const
  WebNavigationTypeLinkClicked = 0;
  WebNavigationTypeFormSubmitted = 1;
  WebNavigationTypeBackForward = 2;
  WebNavigationTypeReload = 3;
  WebNavigationTypeFormResubmitted = 4;
  WebNavigationTypeOther = 5;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  WebActionNavigationTypeKey: NSString; cvar; external;
  WebActionElementKey: NSString; cvar; external;
  WebActionButtonKey: NSString; cvar; external;
  WebActionModifierFlagsKey: NSString; cvar; external;
  WebActionOriginalURLKey: NSString; cvar; external;
{$endif}

{$ifdef PROTOCOLS}

type
  WebPolicyDecisionListenerProtocol = objcprotocol external name 'WebPolicyDecisionListener' (NSObjectProtocol)
    procedure use; message 'use';
    procedure download; message 'download';
    procedure ignore; message 'ignore';
  end;
{$endif}

{$ifdef CLASSES}

type
  WebPolicyDelegate = objccategory external (NSObject)
    procedure webView_decidePolicyForNavigationAction_request_frame_decisionListener (webView_: WebView; actionInformation: NSDictionary; request: NSURLRequest; frame: WebFrame; listener: WebPolicyDecisionListenerProtocol); message 'webView:decidePolicyForNavigationAction:request:frame:decisionListener:';
    procedure webView_decidePolicyForNewWindowAction_request_newFrameName_decisionListener (webView_: WebView; actionInformation: NSDictionary; request: NSURLRequest; frameName: NSString; listener: WebPolicyDecisionListenerProtocol); message 'webView:decidePolicyForNewWindowAction:request:newFrameName:decisionListener:';
    procedure webView_decidePolicyForMIMEType_request_frame_decisionListener (webView_: WebView; type_: NSString; request: NSURLRequest; frame: WebFrame; listener: WebPolicyDecisionListenerProtocol); message 'webView:decidePolicyForMIMEType:request:frame:decisionListener:';
    procedure webView_unableToImplementPolicyWithError_frame (webView_: WebView; error: NSError; frame: WebFrame); message 'webView:unableToImplementPolicyWithError:frame:';
  end;
{$endif}

