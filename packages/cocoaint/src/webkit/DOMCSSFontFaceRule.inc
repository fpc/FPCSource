{ Parsed from WebKit.framework DOMCSSFontFaceRule.h }


{$ifdef TYPES}
type
  DOMCSSFontFaceRulePtr = ^DOMCSSFontFaceRule;
{$endif}

{$ifdef CLASSES}

type
  DOMCSSFontFaceRule = objcclass external (DOMCSSRule)
  public
    function style: DOMCSSStyleDeclaration; message 'style';
  end;
{$endif}

