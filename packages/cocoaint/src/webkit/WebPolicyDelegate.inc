{ Parsed from Webkit.framework WebPolicyDelegate.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 10 8:18:59 ICT 2009 }


{$ifdef TYPES}
{$ifndef WEBPOLICYDELEGATE_PAS_T}
{$define WEBPOLICYDELEGATE_PAS_T}

{ Constants }

const
  WebNavigationTypeLinkClicked = 0;
  WebNavigationTypeFormSubmitted = 1;
  WebNavigationTypeBackForward = 2;
  WebNavigationTypeReload = 3;
  WebNavigationTypeFormResubmitted = 4;
  WebNavigationTypeOther = 5;

{ Types }
type
  WebNavigationType = culong;

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
  WebActionNavigationTypeKey: NSString; external name '_WebActionNavigationTypeKey';
  WebActionElementKey: NSString; external name '_WebActionElementKey';
  WebActionButtonKey: NSString; external name '_WebActionButtonKey';
  WebActionModifierFlagsKey: NSString; external name '_WebActionModifierFlagsKey';
  WebActionOriginalURLKey: NSString; external name '_WebActionOriginalURLKey';

{$endif}
{$endif}

{$ifdef FORWARD}
  WebPolicyDecisionListenerProtocol = objcprotocol;

{$endif}
{$ifdef PROTOCOLS}
{$ifndef WEBPOLICYDELEGATE_PAS_P}
{$define WEBPOLICYDELEGATE_PAS_P}
  
{ WebPolicyDecisionListener Protocol }
  WebPolicyDecisionListenerProtocol = objcprotocol
    procedure use; message 'use';
    procedure download; message 'download';
    procedure ignore; message 'ignore';
  end; external name 'WebPolicyDecisionListener';
{$endif}
{$endif}
