{ Parsed from WebKit.framework DOMCSSPageRule.h }


{$ifdef TYPES}
type
  DOMCSSPageRulePtr = ^DOMCSSPageRule;
{$endif}

{$ifdef CLASSES}

type
  DOMCSSPageRule = objcclass external (DOMCSSRule)
  public
    procedure setSelectorText(newValue: NSString); message 'setSelectorText:';
    function selectorText: NSString; message 'selectorText';
    function style: DOMCSSStyleDeclaration; message 'style';
  end;
{$endif}

