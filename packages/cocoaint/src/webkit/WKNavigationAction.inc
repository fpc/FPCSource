{ Parsed from WebKit.framework WKNavigationAction.h }


{ Types from WKNavigationAction }
{$ifdef TYPES}

{$endif}


{$ifdef TYPES}
type
  WKNavigationActionPtr = ^WKNavigationAction;
{$endif}

{$if defined(WK_API_ENABLED)}

{$ifdef TYPES}
type
  WKNavigationType = NSInteger;
  WKNavigationTypePtr = ^WKNavigationType;

const
  WKNavigationTypeLinkActivated = 0;
  WKNavigationTypeFormSubmitted = 1;
  WKNavigationTypeBackForward = 2;
  WKNavigationTypeReload = 3;
  WKNavigationTypeFormResubmitted = 4;
  WKNavigationTypeOther = -1;
{$endif}

{$ifdef CLASSES}

type
  WKNavigationAction = objcclass external (NSObject)
  public
    function sourceFrame: WKFrameInfo; message 'sourceFrame';
    function targetFrame: WKFrameInfo; message 'targetFrame';
    function navigationType: WKNavigationType; message 'navigationType';
    function request: NSURLRequest; message 'request';
    {$if  not defined(TARGET_OS_IPHONE)}
    function modifierFlags: NSEventModifierFlags; message 'modifierFlags';
    function buttonNumber: NSInteger; message 'buttonNumber';
    {$endif}
  end;
{$endif}

{$endif}
