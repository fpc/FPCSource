{ Parsed from WebKit.framework DOMCSSStyleRule.h }


{$ifdef TYPES}
type
  DOMCSSStyleRulePtr = ^DOMCSSStyleRule;
{$endif}

{$ifdef CLASSES}

type
  DOMCSSStyleRule = objcclass external (DOMCSSRule)
  public
    procedure setSelectorText(newValue: NSString); message 'setSelectorText:';
    function selectorText: NSString; message 'selectorText';
    function style: DOMCSSStyleDeclaration; message 'style';
  end;
{$endif}

