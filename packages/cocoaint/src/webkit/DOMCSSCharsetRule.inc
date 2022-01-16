{ Parsed from WebKit.framework DOMCSSCharsetRule.h }


{$ifdef TYPES}
type
  DOMCSSCharsetRulePtr = ^DOMCSSCharsetRule;
{$endif}

{$ifdef CLASSES}

type
  DOMCSSCharsetRule = objcclass external (DOMCSSRule)
  public
    function encoding: NSString; message 'encoding';
  end;
{$endif}

