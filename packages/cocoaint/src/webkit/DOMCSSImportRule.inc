{ Parsed from WebKit.framework DOMCSSImportRule.h }


{$ifdef TYPES}
type
  DOMCSSImportRulePtr = ^DOMCSSImportRule;
{$endif}

{$ifdef CLASSES}

type
  DOMCSSImportRule = objcclass external (DOMCSSRule)
  public
    function href: NSString; message 'href';
    function media: DOMMediaList; message 'media';
    function styleSheet: DOMCSSStyleSheet; message 'styleSheet';
  end;
{$endif}

